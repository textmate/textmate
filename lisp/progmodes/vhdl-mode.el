;;; vhdl-mode.el --- major mode for editing VHDL code

;; Copyright (C) 1992-2012 Free Software Foundation, Inc.

;; Authors:     Reto Zimmermann <reto@gnu.org>
;;              Rodney J. Whitby <software.vhdl-mode@rwhitby.net>
;; Maintainer:  Reto Zimmermann <reto@gnu.org>
;; Keywords:    languages vhdl
;; WWW:         http://www.iis.ee.ethz.ch/~zimmi/emacs/vhdl-mode.html

;; Yoni Rabkin <yoni@rabkins.net> contacted the maintainer of this
;; file on 18/3/2008, and the maintainer agreed that when a bug is
;; filed in the Emacs bug reporting system against this file, a copy
;; of the bug report be sent to the maintainer's email address.

(defconst vhdl-version "3.33.6"
  "VHDL Mode version number.")

(defconst vhdl-time-stamp "2005-08-30"
  "VHDL Mode time stamp for last update.")

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

;; This package provides an Emacs major mode for editing VHDL code.
;; It includes the following features:

;;   - Syntax highlighting
;;   - Indentation
;;   - Template insertion (electrification)
;;   - Insertion of file headers
;;   - Insertion of user-specified models
;;   - Port translation / testbench generation
;;   - Structural composition
;;   - Configuration generation
;;   - Sensitivity list updating
;;   - File browser
;;   - Design hierarchy browser
;;   - Source file compilation (syntax analysis)
;;   - Makefile generation
;;   - Code hiding
;;   - Word/keyword completion
;;   - Block commenting
;;   - Code fixing/alignment/beautification
;;   - PostScript printing
;;   - VHDL'87/'93 and VHDL-AMS supported
;;   - Comprehensive menu
;;   - Fully customizable
;;   - Works under GNU Emacs (recommended) and XEmacs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Documentation

;; See comment string of function `vhdl-mode' or type `C-c C-h' in Emacs.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs Versions

;; supported: GNU Emacs 20.X/21.X/22.X, XEmacs 20.X/21.X
;; tested on: GNU Emacs 20.4, XEmacs 21.1 (marginally)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Installation

;; Prerequisites:  GNU Emacs 20.X/21.X/22.X, XEmacs 20.X/21.X.

;; Put `vhdl-mode.el' into the `site-lisp' directory of your Emacs installation
;; or into an arbitrary directory that is added to the load path by the
;; following line in your Emacs start-up file `.emacs':

;;   (setq load-path (cons (expand-file-name "<directory-name>") load-path))

;; If you already have the compiled `vhdl-mode.elc' file, put it in the same
;; directory.  Otherwise, byte-compile the source file:
;;   Emacs:  M-x byte-compile-file RET vhdl-mode.el RET
;;   Unix:   emacs -batch -q -no-site-file -f batch-byte-compile vhdl-mode.el

;; Add the following lines to the `site-start.el' file in the `site-lisp'
;; directory of your Emacs installation or to your Emacs start-up file `.emacs'
;; (not required in Emacs 20.X):

;;   (autoload 'vhdl-mode "vhdl-mode" "VHDL Mode" t)
;;   (setq auto-mode-alist (cons '("\\.vhdl?\\'" . vhdl-mode) auto-mode-alist))

;; More detailed installation instructions are included in the official
;; VHDL Mode distribution.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Acknowledgements

;; Electrification ideas by Bob Pack <rlpst@cislabs.pitt.edu>
;; and Steve Grout.

;; Fontification approach suggested by Ken Wood <ken@eda.com.au>.
;; Ideas about alignment from John Wiegley <johnw@gnu.org>.

;; Many thanks to all the users who sent me bug reports and enhancement
;; requests.
;; Thanks to Colin Marquardt for his serious beta testing, his innumerable
;; enhancement suggestions and the fruitful discussions.
;; Thanks to Dan Nicolaescu for reviewing the code and for his valuable hints.
;; Thanks to Ulf Klaperski for the indentation speedup hint.

;; Special thanks go to Wolfgang Fichtner and the crew from the Integrated
;; Systems Laboratory, Swiss Federal Institute of Technology Zurich, for
;; giving me the opportunity to develop this code.
;; This work has been funded in part by MICROSWISS, a Microelectronics Program
;; of the Swiss Government.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

;; Emacs 21+ handling
(defconst vhdl-emacs-21 (and (<= 21 emacs-major-version) (not (featurep 'xemacs)))
  "Non-nil if GNU Emacs 21, 22, ... is used.")
(defconst vhdl-emacs-22 (and (<= 22 emacs-major-version) (not (featurep 'xemacs)))
  "Non-nil if GNU Emacs 22, ... is used.")

(defvar compilation-file-regexp-alist)
(defvar conf-alist)
(defvar conf-entry)
(defvar conf-key)
(defvar ent-alist)
(defvar itimer-version)
(defvar lazy-lock-defer-contextually)
(defvar lazy-lock-defer-on-scrolling)
(defvar lazy-lock-defer-on-the-fly)
(defvar speedbar-attached-frame)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; help function for user options
(defun vhdl-custom-set (variable value &rest functions)
  "Set variables as in `custom-set-default' and call FUNCTIONS afterwards."
  (if (fboundp 'custom-set-default)
      (custom-set-default variable value)
    (set-default variable value))
  (while functions
    (when (fboundp (car functions)) (funcall (car functions)))
    (setq functions (cdr functions))))

(defun vhdl-widget-directory-validate (widget)
  "Check that the value of WIDGET is a valid directory entry (i.e. ends with
'/' or is empty)."
  (let ((val (widget-value widget)))
    (unless (string-match "^\\(\\|.*/\\)$" val)
      (widget-put widget :error "Invalid directory entry: must end with '/'")
      widget)))

;; help string for user options
(defconst vhdl-name-doc-string "

FROM REGEXP is a regular expression matching the original name:
  \".*\"       matches the entire string
  \"\\(...\\)\"  matches a substring
TO STRING specifies the string to be inserted as new name:
  \"\\&\"  means substitute entire matched text
  \"\\N\"  means substitute what matched the Nth \"\\(...\\)\"
Examples:
  \".*\"           \"\\&\"    inserts original string
  \".*\"           \"\\&_i\"  attaches \"_i\" to original string
  \"\\(.*\\)_[io]$\" \"\\1\"    strips off \"_i\" or \"_o\" from original string
  \".*\"           \"foo\"   inserts constant string \"foo\"
  \".*\"           \"\"      inserts empty string")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User variables

(defgroup vhdl nil
  "Customizations for VHDL Mode."
  :prefix "vhdl-"
  :group 'languages
;  :version "21.2"  ; comment out for XEmacs
  )

(defgroup vhdl-mode nil
  "Customizations for modes."
  :group 'vhdl)

(defcustom vhdl-indent-tabs-mode nil
  "*Non-nil means indentation can insert tabs.
Overrides local variable `indent-tabs-mode'."
  :type 'boolean
  :group 'vhdl-mode)


(defgroup vhdl-compile nil
  "Customizations for compilation."
  :group 'vhdl)

(defcustom vhdl-compiler-alist
  '(
    ;; Cadence Leapfrog: cv -file test.vhd
    ;; duluth: *E,430 (test.vhd,13): identifier (POSITIV) is not declared
    ("Cadence Leapfrog" "cv" "-work \\1 -file" "make" "-f \\1"
     nil "mkdir \\1" "./" "work/" "Makefile" "leapfrog"
     ("duluth: \\*E,[0-9]+ (\\(.+\\),\\([0-9]+\\)):" 1 2 0) ("" 0)
     ("\\1/entity" "\\2/\\1" "\\1/configuration"
      "\\1/package" "\\1/body" downcase))
    ;; Cadence Affirma NC vhdl: ncvhdl test.vhd
    ;; ncvhdl_p: *E,IDENTU (test.vhd,13|25): identifier
    ;; (PLL_400X_TOP) is not declared [10.3].
    ("Cadence NC" "ncvhdl" "-work \\1" "make" "-f \\1"
     nil "mkdir \\1" "./" "work/" "Makefile" "ncvhdl"
     ("ncvhdl_p: \\*E,\\w+ (\\(.+\\),\\([0-9]+\\)|\\([0-9]+\\)):" 1 2 3) ("" 0)
     ("\\1/entity/pc.db" "\\2/\\1/pc.db" "\\1/configuration/pc.db"
      "\\1/package/pc.db" "\\1/body/pc.db" downcase))
    ;; Ikos Voyager: analyze test.vhd
    ;; analyze test.vhd
    ;; E L4/C5:        this library unit is inaccessible
    ("Ikos" "analyze" "-l \\1" "make" "-f \\1"
     nil "mkdir \\1" "./" "work/" "Makefile" "ikos"
     ("E L\\([0-9]+\\)/C\\([0-9]+\\):" 0 1 2)
     ("^analyze +\\(.+ +\\)*\\(.+\\)$" 2)
     nil)
    ;; ModelSim, Model Technology: vcom test.vhd
    ;; ERROR: test.vhd(14): Unknown identifier: positiv
    ;; WARNING[2]: test.vhd(85): Possible infinite loop
    ;; ** Error: adder.vhd(190): Unknown identifier: ctl_numb
    ("ModelSim" "vcom" "-93 -work \\1" "make" "-f \\1"
     nil "vlib \\1; vmap \\2 \\1" "./" "work/" "Makefile" "modelsim"
     ("\\(ERROR\\|WARNING\\|\\*\\* Error\\|\\*\\* Warning\\)[^:]*: \\(.+\\)(\\([0-9]+\\)):" 2 3 0) ("" 0)
     ("\\1/_primary.dat" "\\2/\\1.dat" "\\1/_primary.dat"
      "\\1/_primary.dat" "\\1/body.dat" downcase))
    ;; ProVHDL, Synopsys LEDA: provhdl -w work -f test.vhd
    ;; test.vhd:34: error message
    ("LEDA ProVHDL" "provhdl" "-w \\1 -f" "make" "-f \\1"
     nil "mkdir \\1" "./" "work/" "Makefile" "provhdl"
     ("\\([^ \t\n]+\\):\\([0-9]+\\): " 1 2 0) ("" 0)
     ("ENTI/\\1.vif" "ARCH/\\1-\\2.vif" "CONF/\\1.vif"
      "PACK/\\1.vif" "BODY/BODY-\\1.vif" upcase))
    ;; QuickHDL, Mentor Graphics: qvhcom test.vhd
    ;; ERROR: test.vhd(24): near "dnd": expecting: END
    ;; WARNING[4]: test.vhd(30): A space is required between ...
    ("QuickHDL" "qvhcom" "-work \\1" "make" "-f \\1"
     nil "mkdir \\1" "./" "work/" "Makefile" "quickhdl"
     ("\\(ERROR\\|WARNING\\)[^:]*: \\(.+\\)(\\([0-9]+\\)):" 2 3 0) ("" 0)
     ("\\1/_primary.dat" "\\2/\\1.dat" "\\1/_primary.dat"
      "\\1/_primary.dat" "\\1/body.dat" downcase))
    ;; Savant: scram -publish-cc test.vhd
    ;; test.vhd:87: _set_passed_through_out_port(IIR_Boolean) not defined for
    ("Savant" "scram" "-publish-cc -design-library-name \\1" "make" "-f \\1"
     nil "mkdir \\1" "./" "work._savant_lib/" "Makefile" "savant"
     ("\\([^ \t\n]+\\):\\([0-9]+\\): " 1 2 0) ("" 0)
     ("\\1_entity.vhdl" "\\2_secondary_units._savant_lib/\\2_\\1.vhdl"
      "\\1_config.vhdl" "\\1_package.vhdl"
      "\\1_secondary_units._savant_lib/\\1_package_body.vhdl" downcase))
    ;; Simili: vhdlp -work test.vhd
    ;; Error: CSVHDL0002: test.vhd: (line 97): Invalid prefix
    ("Simili" "vhdlp" "-work \\1" "make" "-f \\1"
     nil "mkdir \\1" "./" "work/" "Makefile" "simili"
     ("\\(Error\\|Warning\\): \\w+: \\(.+\\): (line \\([0-9]+\\)): " 2 3 0) ("" 0)
     ("\\1/prim.var" "\\2/_\\1.var" "\\1/prim.var"
      "\\1/prim.var" "\\1/_body.var" downcase))
    ;; Speedwave (Innoveda): analyze -libfile vsslib.ini -src test.vhd
    ;;     ERROR[11]::File test.vhd Line 100: Use of undeclared identifier
    ("Speedwave" "analyze" "-libfile vsslib.ini -src" "make" "-f \\1"
     nil "mkdir \\1" "./" "work/" "Makefile" "speedwave"
     ("^ *ERROR\[[0-9]+\]::File \\(.+\\) Line \\([0-9]+\\):" 1 2 0) ("" 0)
     nil)
    ;; Synopsys, VHDL Analyzer (sim): vhdlan -nc test.vhd
    ;; **Error: vhdlan,703 test.vhd(22): OTHERS is not legal in this context.
    ("Synopsys" "vhdlan" "-nc -work \\1" "make" "-f \\1"
     nil "mkdir \\1" "./" "work/" "Makefile" "synopsys"
     ("\\*\\*Error: vhdlan,[0-9]+ \\(.+\\)(\\([0-9]+\\)):" 1 2 0) ("" 0)
     ("\\1.sim" "\\2__\\1.sim" "\\1.sim" "\\1.sim" "\\1__.sim" upcase))
    ;; Synopsys, VHDL Analyzer (syn): vhdlan -nc -spc test.vhd
    ;; **Error: vhdlan,703 test.vhd(22): OTHERS is not legal in this context.
    ("Synopsys Design Compiler" "vhdlan" "-nc -spc -work \\1" "make" "-f \\1"
     nil "mkdir \\1" "./" "work/" "Makefile" "synopsys_dc"
     ("\\*\\*Error: vhdlan,[0-9]+ \\(.+\\)(\\([0-9]+\\)):" 1 2 0) ("" 0)
     ("\\1.syn" "\\2__\\1.syn" "\\1.syn" "\\1.syn" "\\1__.syn" upcase))
    ;; Synplify:
    ;; @W:"test.vhd":57:8:57:9|Optimizing register bit count_x(5) to a constant 0
    ("Synplify" "n/a" "n/a" "make" "-f \\1"
     nil "mkdir \\1" "./" "work/" "Makefile" "synplify"
     ("@[EWN]:\"\\(.+\\)\":\\([0-9]+\\):\\([0-9]+\\):" 1 2 3) ("" 0)
     nil)
    ;; Vantage: analyze -libfile vsslib.ini -src test.vhd
    ;;     Compiling "test.vhd" line 1...
    ;; **Error: LINE 49 *** No aggregate value is valid in this context.
    ("Vantage" "analyze" "-libfile vsslib.ini -src" "make" "-f \\1"
     nil "mkdir \\1" "./" "work/" "Makefile" "vantage"
     ("\\*\\*Error: LINE \\([0-9]+\\) \\*\\*\\*" 0 1 0)
     ("^ *Compiling \"\\(.+\\)\" " 1)
     nil)
    ;; VeriBest: vc vhdl test.vhd
    ;; (no file name printed out!)
    ;;     32:   Z <=  A and BitA ;
    ;;                       ^^^^
    ;; [Error] Name BITA is unknown
    ("VeriBest" "vc" "vhdl" "make" "-f \\1"
     nil "mkdir \\1" "./" "work/" "Makefile" "veribest"
     ("^ +\\([0-9]+\\): +[^ ]" 0 1 0) ("" 0)
     nil)
    ;; Viewlogic: analyze -libfile vsslib.ini -src test.vhd
    ;;     Compiling "test.vhd" line 1...
    ;; **Error: LINE 49 *** No aggregate value is valid in this context.
    ("Viewlogic" "analyze" "-libfile vsslib.ini -src" "make" "-f \\1"
     nil "mkdir \\1" "./" "work/" "Makefile" "viewlogic"
     ("\\*\\*Error: LINE \\([0-9]+\\) \\*\\*\\*" 0 1 0)
     ("^ *Compiling \"\\(.+\\)\" " 1)
     nil)
    )
  "*List of available VHDL compilers and their properties.
Each list entry specifies the following items for a compiler:
Compiler:
  Compiler name    : name used in option `vhdl-compiler' to choose compiler
  Compile command  : command used for source file compilation
  Compile options  : compile options (\"\\1\" inserts library name)
  Make command     : command used for compilation using a Makefile
  Make options     : make options (\"\\1\" inserts Makefile name)
  Generate Makefile: use built-in function or command to generate a Makefile
                     \(\"\\1\" inserts Makefile name, \"\\2\" inserts library name)
  Library command  : command to create library directory \(\"\\1\" inserts
                     library directory, \"\\2\" inserts library name)
  Compile directory: where compilation is run and the Makefile is placed
  Library directory: directory of default library
  Makefile name    : name of Makefile (default is \"Makefile\")
  ID string        : compiler identification string (see `vhdl-project-alist')
Error message:
  Regexp           : regular expression to match error messages (*)
  File subexp index: index of subexpression that matches the file name
  Line subexp index: index of subexpression that matches the line number
  Column subexp idx: index of subexpression that matches the column number
File message:
  Regexp           : regular expression to match a file name message
  File subexp index: index of subexpression that matches the file name
Unit-to-file name mapping: mapping of library unit names to names of files
                     generated by the compiler (used for Makefile generation)
  To string        : string a name is mapped to (\"\\1\" inserts the unit name,
                     \"\\2\" inserts the entity name for architectures)
  Case adjustment  : adjust case of inserted unit names

\(*) The regular expression must match the error message starting from the
    beginning of the line (but not necessarily to the end of the line).

Compile options allows insertion of the library name (see `vhdl-project-alist')
in order to set the compilers library option (e.g. \"vcom -work my_lib\").

For Makefile generation, the built-in function can be used (requires
specification of the unit-to-file name mapping).  Alternatively, an
external command can be specified.  Work directory allows specification of
an alternative \"work\" library path (e.g. \"WORK/\" instead of \"work/\",
used for Makefile generation).  To use another library name than \"work\",
customize `vhdl-project-alist'.  The library command is inserted in Makefiles
to automatically create the library directory if not existent.

Compile options, compile directory, library directory, and Makefile name are
overwritten by the project settings if a project is defined (see
`vhdl-project-alist').  Directory paths are relative to the source file
directory.

Some compilers do not include the file name in the error message, but print
out a file name message in advance.  In this case, set \"File Subexp Index\"
under \"Error Message\" to 0 and fill out the \"File Message\" entries.
If no file name at all is printed out, set both \"File Message\" entries to 0
\(a default file name message will be printed out instead, does not work in
XEmacs).

A compiler is selected for syntax analysis (`\\[vhdl-compile]') by
assigning its name to option `vhdl-compiler'.

Please send any missing or erroneous compiler properties to the maintainer for
updating.

NOTE: Activate new error and file message regexps and reflect the new setting
      in the choice list of option `vhdl-compiler' by restarting Emacs."
  :type '(repeat
	  (list :tag "Compiler" :indent 2
		(string :tag "Compiler name      ")
		(string :tag "Compile command    ")
		(string :tag "Compile options    " "-work \\1")
		(string :tag "Make command       " "make")
		(string :tag "Make options       " "-f \\1")
		(choice :tag "Generate Makefile  "
			(const :tag "Built-in function" nil)
			(string :tag "Command" "vmake \\2 > \\1"))
		(string :tag "Library command    " "mkdir \\1")
		(directory :tag "Compile directory  "
			   :validate vhdl-widget-directory-validate "./")
		(directory :tag "Library directory  "
			   :validate vhdl-widget-directory-validate "work/")
		(file :tag "Makefile name      " "Makefile")
		(string :tag "ID string          ")
		(list :tag "Error message" :indent 4
		      (regexp  :tag "Regexp           ")
		      (integer :tag "File subexp index")
		      (integer :tag "Line subexp index")
		      (integer :tag "Column subexp idx"))
		(list :tag "File message" :indent 4
		      (regexp  :tag "Regexp           ")
		      (integer :tag "File subexp index"))
		(choice :tag "Unit-to-file name mapping"
			:format "%t: %[Value Menu%] %v\n"
			(const :tag "Not defined" nil)
			(list :tag "To string" :indent 4
			      (string :tag "Entity           " "\\1.vhd")
			      (string :tag "Architecture     " "\\2_\\1.vhd")
			      (string :tag "Configuration    " "\\1.vhd")
			      (string :tag "Package          " "\\1.vhd")
			      (string :tag "Package Body     " "\\1_body.vhd")
			      (choice :tag "Case adjustment  "
				      (const :tag "None" identity)
				      (const :tag "Upcase" upcase)
				      (const :tag "Downcase" downcase))))))
  :set (lambda (variable value)
	 (vhdl-custom-set variable value 'vhdl-update-mode-menu))
  :group 'vhdl-compile)

(defcustom vhdl-compiler "ModelSim"
  "*Specifies the VHDL compiler to be used for syntax analysis.
Select a compiler name from the ones defined in option `vhdl-compiler-alist'."
  :type (let ((alist vhdl-compiler-alist) list)
	  (while alist
	    (setq list (cons (list 'const (caar alist)) list))
	    (setq alist (cdr alist)))
	  (append '(choice) (nreverse list)))
  :group 'vhdl-compile)

(defcustom vhdl-compile-use-local-error-regexp t
  "*Non-nil means use buffer-local `compilation-error-regexp-alist'.
In this case, only error message regexps for VHDL compilers are active if
compilation is started from a VHDL buffer.  Otherwise, the error message
regexps are appended to the predefined global regexps, and all regexps are
active all the time.  Note that by doing that, the predefined global regexps
might result in erroneous parsing of error messages for some VHDL compilers.

NOTE: Activate the new setting by restarting Emacs."
  :type 'boolean
  :group 'vhdl-compile)

(defcustom vhdl-makefile-generation-hook nil
  "*Functions to run at the end of Makefile generation.
Allows to insert user specific parts into a Makefile.

Example:
  \(lambda nil
    \(re-search-backward \"^# Rule for compiling entire design\")
    \(insert \"# My target\\n\\n.MY_TARGET :\\n\\n\\n\"))"
  :type 'hook
  :group 'vhdl-compile)

(defcustom vhdl-default-library "work"
  "*Name of default library.
Is overwritten by project settings if a project is active."
  :type 'string
  :group 'vhdl-compile)


(defgroup vhdl-project nil
  "Customizations for projects."
  :group 'vhdl)

(defcustom vhdl-project-alist
  '(("Example 1" "Source files in two directories, custom library name, VHDL'87"
     "~/example1/" ("src/system/" "src/components/") ""
     (("ModelSim" "-87 \\2" "-f \\1 top_level" nil)
      ("Synopsys" "-vhdl87 \\2" "-f \\1 top_level" ((".*/datapath/.*" . "-optimize \\3") (".*_tb\\.vhd" . nil))))
     "lib/" "example3_lib" "lib/example3/" "Makefile_\\2" "")
    ("Example 2" "Individual source files, multiple compilers in different directories"
     "$EXAMPLE2/" ("vhdl/system.vhd" "vhdl/component_*.vhd") ""
     nil "\\1/" "work" "\\1/work/" "Makefile" "")
    ("Example 3" "Source files in a directory tree, multiple compilers in same directory"
     "/home/me/example3/" ("-r ./*/vhdl/") "/CVS/"
     nil "./" "work" "work-\\1/" "Makefile-\\1" "\
-------------------------------------------------------------------------------
-- This is a multi-line project description
-- that can be used as a project dependent part of the file header.
"))
  "*List of projects and their properties.
  Name             : name used in option `vhdl-project' to choose project
  Title            : title of project (single-line string)
  Default directory: default project directory (absolute path)
  Sources          : a) source files  : path + \"/\" + file name
                     b) directory     : path + \"/\"
                     c) directory tree: \"-r \" + path + \"/\"
  Exclude regexp   : matches file/directory names to be excluded as sources
  Compile options  : project-specific options for each compiler
    Compiler name  : name of compiler for which these options are valid
    Compile options: project-specific compiler options
                     (\"\\1\" inserts library name, \"\\2\" default options)
    Make options:    project-specific make options
                     (\"\\1\" inserts Makefile name, \"\\2\" default options)
    Exceptions     : file-specific exceptions
      File name regexp: matches file names for which exceptions are valid
      - Options       : file-specific compiler options string
                        (\"\\1\" inserts library name, \"\\2\" default options,
                        \"\\3\" project-specific options)
      - Do not compile: do not compile this file (in Makefile)
  Compile directory: where compilation is run and the Makefile is placed
                     \(\"\\1\" inserts compiler ID string)
  Library name     : name of library (default is \"work\")
  Library directory: path to library (\"\\1\" inserts compiler ID string)
  Makefile name    : name of Makefile
                     (\"\\1\" inserts compiler ID string, \"\\2\" library name)
  Description      : description of project (multi-line string)

Project title and description are used to insert into the file header (see
option `vhdl-file-header').

The default directory must have an absolute path (use `M-TAB' for completion).
All other paths can be absolute or relative to the default directory.  All
paths must end with '/'.

The design units found in the sources (files and directories) are shown in the
hierarchy browser.  Path and file name can contain wildcards `*' and `?' as
well as \"./\" and \"../\" (\"sh\" syntax).  Paths can also be absolute.
Environment variables (e.g. \"$EXAMPLE2\") are resolved.  If no sources are
specified, the default directory is taken as source directory.  Otherwise,
the default directory is only taken as source directory if there is a sources
entry with the empty string or \"./\".  Exclude regexp allows to filter out
specific file and directory names from the list of sources (e.g. CVS
directories).

Files are compiled in the compile directory.  Makefiles are also placed into
the compile directory.  Library directory specifies which directory the
compiler compiles into (used to generate the Makefile).

Since different compile/library directories and Makefiles may exist for
different compilers within one project, these paths and names allow the
insertion of a compiler-dependent ID string (defined in `vhdl-compiler-alist').
Compile options, compile directory, library directory, and Makefile name
overwrite the settings of the current compiler.

File-specific compiler options (highest priority) overwrite project-specific
options which overwrite default options (lowest priority).  Lower priority
options can be inserted in higher priority options.  This allows to reuse
default options (e.g. \"-file\") in project- or file-specific options (e.g.
\"-93 -file\").

NOTE: Reflect the new setting in the choice list of option `vhdl-project'
      by restarting Emacs."
  :type `(repeat
	  (list :tag "Project" :indent 2
		(string :tag "Name             ")
		(string :tag "Title            ")
		(directory :tag "Default directory"
			   :validate vhdl-widget-directory-validate
			   ,(abbreviate-file-name default-directory))
		(repeat :tag "Sources          " :indent 4
			(directory :format "     %v" "./"))
		(regexp :tag "Exclude regexp   ")
		(repeat
		 :tag "Compile options  " :indent 4
		 (list :tag "Compiler" :indent 6
		       ,(let ((alist vhdl-compiler-alist) list)
			  (while alist
			    (setq list (cons (list 'const (caar alist)) list))
			    (setq alist (cdr alist)))
			  (append '(choice :tag "Compiler name")
				  (nreverse list)))
		       (string :tag "Compile options" "\\2")
		       (string :tag "Make options   " "\\2")
		       (repeat
			:tag "Exceptions   " :indent 8
			(cons :format "%v"
			      (regexp :tag "File name regexp    ")
			      (choice :format "%[Value Menu%] %v"
				      (string :tag "Options" "\\3")
				      (const :tag "Do not compile" nil))))))
		(directory :tag "Compile directory"
			   :validate vhdl-widget-directory-validate "./")
		(string :tag "Library name     " "work")
		(directory :tag "Library directory"
			   :validate vhdl-widget-directory-validate "work/")
		(file :tag "Makefile name    " "Makefile")
		(string :tag "Description: (type `C-j' for newline)"
			:format "%t\n%v\n")))
  :set (lambda (variable value)
	 (vhdl-custom-set variable value
			  'vhdl-update-mode-menu
			  'vhdl-speedbar-refresh))
  :group 'vhdl-project)

(defcustom vhdl-project nil
  "*Specifies the default for the current project.
Select a project name from the ones defined in option `vhdl-project-alist'.
Is used to determine the project title and description to be inserted in file
headers and the source files/directories to be scanned in the hierarchy
browser.  The current project can also be changed temporarily in the menu."
  :type (let ((alist vhdl-project-alist) list)
	  (while alist
	    (setq list (cons (list 'const (caar alist)) list))
	    (setq alist (cdr alist)))
	  (append '(choice (const :tag "None" nil) (const :tag "--"))
		  (nreverse list)))
  :group 'vhdl-project)

(defcustom vhdl-project-file-name '("\\1.prj")
  "*List of file names/paths for importing/exporting project setups.
\"\\1\" is replaced by the project name (SPC is replaced by `_'), \"\\2\" is
replaced by the user name (allows to have user-specific project setups).
The first entry is used as file name to import/export individual project
setups.  All entries are used to automatically import project setups at
startup (see option `vhdl-project-auto-load').  Projects loaded from the
first entry are automatically made current.  Hint: specify local project
setups in first entry, global setups in following entries; loading a local
project setup will make it current, while loading the global setups
is done without changing the current project.
Names can also have an absolute path (i.e. project setups can be stored
in global directories)."
  :type '(repeat (string :tag "File name" "\\1.prj"))
  :group 'vhdl-project)

(defcustom vhdl-project-auto-load '(startup)
  "*Automatically load project setups from files.
All project setup files that match the file names specified in option
`vhdl-project-file-name' are automatically loaded.  The project of the
\(alphabetically) last loaded setup of the first `vhdl-project-file-name'
entry is activated.
A project setup file can be obtained by exporting a project (see menu).
  At startup: project setup file is loaded at Emacs startup"
  :type '(set (const :tag "At startup" startup))
  :group 'vhdl-project)

(defcustom vhdl-project-sort t
  "*Non-nil means projects are displayed in alphabetical order."
  :type 'boolean
  :group 'vhdl-project)


(defgroup vhdl-style nil
  "Customizations for coding styles."
  :group 'vhdl
  :group 'vhdl-template
  :group 'vhdl-port
  :group 'vhdl-compose)

(defcustom vhdl-standard '(87 nil)
  "*VHDL standards used.
Basic standard:
  VHDL'87      : IEEE Std 1076-1987
  VHDL'93      : IEEE Std 1076-1993
Additional standards:
  VHDL-AMS     : IEEE Std 1076.1 (analog-mixed-signal)
  Math packages: IEEE Std 1076.2 (`math_real', `math_complex')

NOTE: Activate the new setting in a VHDL buffer by using the menu entry
      \"Activate Options\"."
  :type '(list (choice :tag "Basic standard"
		       (const :tag "VHDL'87" 87)
		       (const :tag "VHDL'93" 93))
	       (set :tag "Additional standards" :indent 2
		    (const :tag "VHDL-AMS" ams)
		    (const :tag "Math packages" math)))
  :set (lambda (variable value)
	 (vhdl-custom-set variable value
			  'vhdl-template-map-init
			  'vhdl-mode-abbrev-table-init
			  'vhdl-template-construct-alist-init
			  'vhdl-template-package-alist-init
			  'vhdl-update-mode-menu
			  'vhdl-words-init 'vhdl-font-lock-init))
  :group 'vhdl-style)

(defcustom vhdl-basic-offset 2
  "*Amount of basic offset used for indentation.
This value is used by + and - symbols in `vhdl-offsets-alist'."
  :type 'integer
  :group 'vhdl-style)

(defcustom vhdl-upper-case-keywords nil
  "*Non-nil means convert keywords to upper case.
This is done when typed or expanded or by the fix case functions."
  :type 'boolean
  :set (lambda (variable value)
	 (vhdl-custom-set variable value 'vhdl-abbrev-list-init))
  :group 'vhdl-style)

(defcustom vhdl-upper-case-types nil
  "*Non-nil means convert standardized types to upper case.
This is done when expanded or by the fix case functions."
  :type 'boolean
  :set (lambda (variable value)
	 (vhdl-custom-set variable value 'vhdl-abbrev-list-init))
  :group 'vhdl-style)

(defcustom vhdl-upper-case-attributes nil
  "*Non-nil means convert standardized attributes to upper case.
This is done when expanded or by the fix case functions."
  :type 'boolean
  :set (lambda (variable value)
	 (vhdl-custom-set variable value 'vhdl-abbrev-list-init))
  :group 'vhdl-style)

(defcustom vhdl-upper-case-enum-values nil
  "*Non-nil means convert standardized enumeration values to upper case.
This is done when expanded or by the fix case functions."
  :type 'boolean
  :set (lambda (variable value)
	 (vhdl-custom-set variable value 'vhdl-abbrev-list-init))
  :group 'vhdl-style)

(defcustom vhdl-upper-case-constants t
  "*Non-nil means convert standardized constants to upper case.
This is done when expanded."
  :type 'boolean
  :set (lambda (variable value)
	 (vhdl-custom-set variable value 'vhdl-abbrev-list-init))
  :group 'vhdl-style)

(defcustom vhdl-use-direct-instantiation 'standard
  "*Non-nil means use VHDL'93 direct component instantiation.
  Never   : never
  Standard: only in VHDL standards that allow it (VHDL'93 and higher)
  Always  : always"
  :type '(choice (const :tag "Never" never)
		 (const :tag "Standard" standard)
		 (const :tag "Always" always))
  :group 'vhdl-style)


(defgroup vhdl-naming nil
  "Customizations for naming conventions."
  :group 'vhdl)

(defcustom vhdl-entity-file-name '(".*" . "\\&")
  (concat
   "*Specifies how the entity file name is obtained.
The entity file name can be obtained by modifying the entity name (e.g.
attaching or stripping off a substring).  The file extension is automatically
taken from the file name of the current buffer."
   vhdl-name-doc-string)
  :type '(cons (regexp :tag "From regexp")
	       (string :tag "To string  "))
  :group 'vhdl-naming
  :group 'vhdl-compose)

(defcustom vhdl-architecture-file-name '("\\(.*\\) \\(.*\\)" . "\\1_\\2")
  (concat
   "*Specifies how the architecture file name is obtained.
The architecture file name can be obtained by modifying the entity
and/or architecture name (e.g. attaching or stripping off a substring).  The
file extension is automatically taken from the file name of the current
buffer.  The string that is matched against the regexp is the concatenation
of the entity and the architecture name separated by a space.  This gives
access to both names (see default setting as example)."
   vhdl-name-doc-string)
  :type '(cons (regexp :tag "From regexp")
	       (string :tag "To string  "))
  :group 'vhdl-naming
  :group 'vhdl-compose)

(defcustom vhdl-configuration-file-name '(".*" . "\\&")
  (concat
   "*Specifies how the configuration file name is obtained.
The configuration file name can be obtained by modifying the configuration
name (e.g. attaching or stripping off a substring).  The file extension is
automatically taken from the file name of the current buffer."
   vhdl-name-doc-string)
  :type '(cons (regexp :tag "From regexp")
	       (string :tag "To string  "))
  :group 'vhdl-naming
  :group 'vhdl-compose)

(defcustom vhdl-package-file-name '(".*" . "\\&")
  (concat
   "*Specifies how the package file name is obtained.
The package file name can be obtained by modifying the package name (e.g.
attaching or stripping off a substring).  The file extension is automatically
taken from the file name of the current buffer.  Package files can be created
in a different directory by prepending a relative or absolute path to the
file name."
   vhdl-name-doc-string)
  :type '(cons (regexp :tag "From regexp")
	       (string :tag "To string  "))
  :group 'vhdl-naming
  :group 'vhdl-compose)

(defcustom vhdl-file-name-case 'identity
  "*Specifies how to change case for obtaining file names.
When deriving a file name from a VHDL unit name, case can be changed as
follows:
  As Is:      case is not changed (taken as is)
  Lower Case: whole name is changed to lower case
  Upper Case: whole name is changed to upper case
  Capitalize: first letter of each word in name is capitalized"
  :type '(choice (const :tag "As Is" identity)
		 (const :tag "Lower Case" downcase)
		 (const :tag "Upper Case" upcase)
		 (const :tag "Capitalize" capitalize))
  :group 'vhdl-naming
  :group 'vhdl-compose)


(defgroup vhdl-template nil
  "Customizations for electrification."
  :group 'vhdl)

(defcustom vhdl-electric-keywords '(vhdl user)
  "*Type of keywords for which electrification is enabled.
  VHDL keywords: invoke built-in templates
  User keywords: invoke user models (see option `vhdl-model-alist')"
  :type '(set (const :tag "VHDL keywords" vhdl)
	      (const :tag "User model keywords" user))
  :set (lambda (variable value)
	 (vhdl-custom-set variable value 'vhdl-mode-abbrev-table-init))
  :group 'vhdl-template)

(defcustom vhdl-optional-labels 'process
  "*Constructs for which labels are to be queried.
Template generators prompt for optional labels for:
  None          : no constructs
  Processes only: processes only (also procedurals in VHDL-AMS)
  All constructs: all constructs with optional labels and keyword END"
  :type '(choice (const :tag "None" none)
		 (const :tag "Processes only" process)
		 (const :tag "All constructs" all))
  :group 'vhdl-template)

(defcustom vhdl-insert-empty-lines 'unit
  "*Specifies whether to insert empty lines in some templates.
This improves readability of code.  Empty lines are inserted in:
  None             : no constructs
  Design units only: entities, architectures, configurations, packages only
  All constructs   : also all constructs with BEGIN...END parts

Replaces option `vhdl-additional-empty-lines'."
  :type '(choice (const :tag "None" none)
		 (const :tag "Design units only" unit)
		 (const :tag "All constructs" all))
  :group 'vhdl-template
  :group 'vhdl-port
  :group 'vhdl-compose)

(defcustom vhdl-argument-list-indent nil
  "*Non-nil means indent argument lists relative to opening parenthesis.
That is, argument, association, and port lists start on the same line as the
opening parenthesis and subsequent lines are indented accordingly.
Otherwise, lists start on a new line and are indented as normal code."
  :type 'boolean
  :group 'vhdl-template
  :group 'vhdl-port
  :group 'vhdl-compose)

(defcustom vhdl-association-list-with-formals t
  "*Non-nil means write association lists with formal parameters.
Templates prompt for formal and actual parameters (ports/generics).
When pasting component instantiations, formals are included.
If nil, only a list of actual parameters is entered."
  :type 'boolean
  :group 'vhdl-template
  :group 'vhdl-port
  :group 'vhdl-compose)

(defcustom vhdl-conditions-in-parenthesis nil
  "*Non-nil means place parenthesis around condition expressions."
  :type 'boolean
  :group 'vhdl-template)

(defcustom vhdl-zero-string "'0'"
  "*String to use for a logic zero."
  :type 'string
  :group 'vhdl-template)

(defcustom vhdl-one-string "'1'"
  "*String to use for a logic one."
  :type 'string
  :group 'vhdl-template)


(defgroup vhdl-header nil
  "Customizations for file header."
  :group 'vhdl-template
  :group 'vhdl-compose)

(defcustom vhdl-file-header "\
-------------------------------------------------------------------------------
-- Title      : <title string>
-- Project    : <project>
-------------------------------------------------------------------------------
-- File       : <filename>
-- Author     : <author>
-- Company    : <company>
-- Created    : <date>
-- Last update: <date>
-- Platform   : <platform>
-- Standard   : <standard>
<projectdesc>-------------------------------------------------------------------------------
-- Description: <cursor>
<copyright>-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- <date>  1.0      <login>\tCreated
-------------------------------------------------------------------------------

"
  "*String or file to insert as file header.
If the string specifies an existing file name, the contents of the file is
inserted, otherwise the string itself is inserted as file header.
Type `C-j' for newlines.
If the header contains RCS keywords, they may be written as <RCS>Keyword<RCS>
if the header needs to be version controlled.

The following keywords for template generation are supported:
  <filename>    : replaced by the name of the buffer
  <author>      : replaced by the user name and email address
                  \(`user-full-name', `mail-host-address', `user-mail-address')
  <login>       : replaced by user login name (`user-login-name')
  <company>     : replaced by contents of option `vhdl-company-name'
  <date>        : replaced by the current date
  <year>        : replaced by the current year
  <project>     : replaced by title of current project (`vhdl-project')
  <projectdesc> : replaced by description of current project (`vhdl-project')
  <copyright>   : replaced by copyright string (`vhdl-copyright-string')
  <platform>    : replaced by contents of option `vhdl-platform-spec'
  <standard>    : replaced by the VHDL language standard(s) used
  <... string>  : replaced by a queried string (\"...\" is the prompt word)
  <title string>: replaced by file title in automatically generated files
  <cursor>      : final cursor position

The (multi-line) project description <projectdesc> can be used as a project
dependent part of the file header and can also contain the above keywords."
  :type 'string
  :group 'vhdl-header)

(defcustom vhdl-file-footer ""
  "*String or file to insert as file footer.
If the string specifies an existing file name, the contents of the file is
inserted, otherwise the string itself is inserted as file footer (i.e. at
the end of the file).
Type `C-j' for newlines.
The same keywords as in option `vhdl-file-header' can be used."
  :type 'string
  :group 'vhdl-header)

(defcustom vhdl-company-name ""
  "*Name of company to insert in file header.
See option `vhdl-file-header'."
  :type 'string
  :group 'vhdl-header)

(defcustom vhdl-copyright-string "\
-------------------------------------------------------------------------------
-- Copyright (c) <year> <company>
"
  "*Copyright string to insert in file header.
Can be multi-line string (type `C-j' for newline) and contain other file
header keywords (see option `vhdl-file-header')."
  :type 'string
  :group 'vhdl-header)

(defcustom vhdl-platform-spec ""
  "*Specification of VHDL platform to insert in file header.
The platform specification should contain names and versions of the
simulation and synthesis tools used.
See option `vhdl-file-header'."
  :type 'string
  :group 'vhdl-header)

(defcustom vhdl-date-format "%Y-%m-%d"
  "*Specifies the date format to use in the header.
This string is passed as argument to the command `format-time-string'.
For more information on format strings, see the documentation for the
`format-time-string' command (C-h f `format-time-string')."
  :type 'string
  :group 'vhdl-header)

(defcustom vhdl-modify-date-prefix-string "-- Last update: "
  "*Prefix string of modification date in VHDL file header.
If actualization of the modification date is called (menu,
`\\[vhdl-template-modify]'), this string is searched and the rest
of the line replaced by the current date."
  :type 'string
  :group 'vhdl-header)

(defcustom vhdl-modify-date-on-saving t
  "*Non-nil means update the modification date when the buffer is saved.
Calls function `\\[vhdl-template-modify]').

NOTE: Activate the new setting in a VHDL buffer by using the menu entry
      \"Activate Options\"."
  :type 'boolean
  :group 'vhdl-header)


(defgroup vhdl-sequential-process nil
  "Customizations for sequential processes."
  :group 'vhdl-template)

(defcustom vhdl-reset-kind 'async
  "*Specifies which kind of reset to use in sequential processes."
  :type '(choice (const :tag "None" none)
		 (const :tag "Synchronous" sync)
		 (const :tag "Asynchronous" async))
  :group 'vhdl-sequential-process)

(defcustom vhdl-reset-active-high nil
  "*Non-nil means reset in sequential processes is active high.
Otherwise, reset is active low."
  :type 'boolean
  :group 'vhdl-sequential-process)

(defcustom vhdl-clock-rising-edge t
  "*Non-nil means rising edge of clock triggers sequential processes.
Otherwise, falling edge triggers."
  :type 'boolean
  :group 'vhdl-sequential-process)

(defcustom vhdl-clock-edge-condition 'standard
  "*Syntax of the clock edge condition.
  Standard: \"clk'event and clk = '1'\"
  Function: \"rising_edge(clk)\""
  :type '(choice (const :tag "Standard" standard)
		 (const :tag "Function" function))
  :group 'vhdl-sequential-process)

(defcustom vhdl-clock-name ""
  "*Name of clock signal to use in templates."
  :type 'string
  :group 'vhdl-sequential-process)

(defcustom vhdl-reset-name ""
  "*Name of reset signal to use in templates."
  :type 'string
  :group 'vhdl-sequential-process)


(defgroup vhdl-model nil
  "Customizations for user models."
  :group 'vhdl)

(defcustom vhdl-model-alist
  '(("Example Model"
     "<label> : process (<clock>, <reset>)
begin  -- process <label>
  if <reset> = '0' then  -- asynchronous reset (active low)
    <cursor>
  elsif <clock>'event and <clock> = '1' then  -- rising clock edge
    if <enable> = '1' then  -- synchronous load

    end if;
  end if;
end process <label>;"
     "e" ""))
  "*List of user models.
VHDL models (templates) can be specified by the user in this list.  They can be
invoked from the menu, through key bindings (`C-c C-m ...'), or by keyword
electrification (i.e. overriding existing or creating new keywords, see
option `vhdl-electric-keywords').
  Name       : name of model (string of words and spaces)
  String     : string or name of file to be inserted as model (newline: `C-j')
  Key Binding: key binding to invoke model, added to prefix `C-c C-m'
                (must be in double-quotes, examples: \"i\", \"\\C-p\", \"\\M-s\")
  Keyword    : keyword to invoke model

The models can contain prompts to be queried.  A prompt is of the form \"<...>\".
A prompt that appears several times is queried once and replaced throughout
the model.  Special prompts are:
  <clock> : name specified in `vhdl-clock-name' (if not empty)
  <reset> : name specified in `vhdl-reset-name' (if not empty)
  <cursor>: final cursor position
File header prompts (see variable `vhdl-file-header') are automatically
replaced, so that user models can also be used to insert different types of
headers.

If the string specifies an existing file name, the contents of the file is
inserted, otherwise the string itself is inserted.
The code within the models should be correctly indented.
Type `C-j' for newlines.

NOTE: Activate the new setting in a VHDL buffer by using the menu entry
      \"Activate Options\"."
  :type '(repeat (list :tag "Model" :indent 2
		       (string :tag "Name       ")
		       (string :tag "String     : (type `C-j' for newline)"
			       :format "%t\n%v")
		       (sexp   :tag "Key binding" x)
		       (string :tag "Keyword    " :format "%t: %v\n")))
  :set (lambda (variable value)
	 (vhdl-custom-set variable value
			  'vhdl-model-map-init
			  'vhdl-model-defun
			  'vhdl-mode-abbrev-table-init
			  'vhdl-update-mode-menu))
  :group 'vhdl-model)


(defgroup vhdl-compose nil
  "Customizations for structural composition."
  :group 'vhdl)

(defcustom vhdl-compose-architecture-name '(".*" . "str")
  (concat
   "*Specifies how the component architecture name is obtained.
The component architecture name can be obtained by modifying the entity name
\(e.g. attaching or stripping off a substring).
If TO STRING is empty, the architecture name is queried."
   vhdl-name-doc-string)
  :type '(cons (regexp :tag "From regexp")
	       (string :tag "To string  "))
  :group 'vhdl-compose)

(defcustom vhdl-compose-configuration-name
  '("\\(.*\\) \\(.*\\)" . "\\1_\\2_cfg")
  (concat
   "*Specifies how the configuration name is obtained.
The configuration name can be obtained by modifying the entity and/or
architecture name (e.g. attaching or stripping off a substring).  The string
that is matched against the regexp is the concatenation of the entity and the
architecture name separated by a space.  This gives access to both names (see
default setting as example)."
   vhdl-name-doc-string)
  :type '(cons (regexp :tag "From regexp")
	       (string :tag "To string  "))
  :group 'vhdl-compose)

(defcustom vhdl-components-package-name
  '((".*" . "\\&_components") . "components")
  (concat
   "*Specifies how the name for the components package is obtained.
The components package is a package containing all component declarations for
the current design.  Its name can be obtained by modifying the project name
\(e.g. attaching or stripping off a substring).  If no project is defined, the
DIRECTORY entry is chosen."
   vhdl-name-doc-string)
  :type '(cons (cons :tag "Project" :indent 2
		     (regexp :tag "From regexp")
		     (string :tag "To string  "))
	       (string :tag "Directory:\n  String     "))
  :group 'vhdl-compose)

(defcustom vhdl-use-components-package nil
  "*Non-nil means use a separate components package for component declarations.
Otherwise, component declarations are inserted and searched for in the
architecture declarative parts."
  :type 'boolean
  :group 'vhdl-compose)

(defcustom vhdl-compose-include-header t
  "*Non-nil means include a header in automatically generated files."
  :type 'boolean
  :group 'vhdl-compose)

(defcustom vhdl-compose-create-files 'single
  "*Specifies whether new files should be created for the new component.
The component's entity and architecture are inserted:
  None          : in current buffer
  Single file   : in new single file
  Separate files: in two separate files
The file names are obtained from variables `vhdl-entity-file-name' and
`vhdl-architecture-file-name'."
  :type '(choice (const :tag "None" none)
		 (const :tag "Single file" single)
		 (const :tag "Separate files" separate))
  :group 'vhdl-compose)

(defcustom vhdl-compose-configuration-create-file nil
  "*Specifies whether a new file should be created for the configuration.
If non-nil, a new file is created for the configuration.
The file name is obtained from variable `vhdl-configuration-file-name'."
  :type 'boolean
  :group 'vhdl-compose)

(defcustom vhdl-compose-configuration-hierarchical t
  "*Specifies whether hierarchical configurations should be created.
If non-nil, automatically created configurations are hierarchical and include
the whole hierarchy of subcomponents.  Otherwise the configuration only
includes one level of subcomponents."
  :type 'boolean
  :group 'vhdl-compose)

(defcustom vhdl-compose-configuration-use-subconfiguration t
  "*Specifies whether subconfigurations should be used inside configurations.
If non-nil, automatically created configurations use configurations in binding
indications for subcomponents, if such configurations exist.  Otherwise,
entities are used in binding indications for subcomponents."
  :type 'boolean
  :group 'vhdl-compose)


(defgroup vhdl-port nil
  "Customizations for port translation functions."
  :group 'vhdl
  :group 'vhdl-compose)

(defcustom vhdl-include-port-comments nil
  "*Non-nil means include port comments when a port is pasted."
  :type 'boolean
  :group 'vhdl-port)

(defcustom vhdl-include-direction-comments nil
  "*Non-nil means include port direction in instantiations as comments."
  :type 'boolean
  :group 'vhdl-port)

(defcustom vhdl-include-type-comments nil
  "*Non-nil means include generic/port type in instantiations as comments."
  :type 'boolean
  :group 'vhdl-port)

(defcustom vhdl-include-group-comments 'never
  "*Specifies whether to include group comments and spacings.
The comments and empty lines between groups of ports are pasted:
  Never       : never
  Declarations: in entity/component/constant/signal declarations only
  Always      : also in generic/port maps"
  :type '(choice (const :tag "Never" never)
		 (const :tag "Declarations" decl)
		 (const :tag "Always" always))
  :group 'vhdl-port)

(defcustom vhdl-actual-port-name '(".*" . "\\&")
  (concat
   "*Specifies how actual port names are obtained from formal port names.
In a component instantiation, an actual port name can be obtained by
modifying the formal port name (e.g. attaching or stripping off a substring)."
   vhdl-name-doc-string)
  :type '(cons (regexp :tag "From regexp")
	       (string :tag "To string  "))
  :group 'vhdl-port)

(defcustom vhdl-instance-name '(".*" . "\\&_%d")
  (concat
   "*Specifies how an instance name is obtained.
The instance name can be obtained by modifying the name of the component to be
instantiated (e.g. attaching or stripping off a substring).  \"%d\" is replaced
by a unique number (starting with 1).
If TO STRING is empty, the instance name is queried."
   vhdl-name-doc-string)
  :type '(cons (regexp :tag "From regexp")
	       (string :tag "To string  "))
  :group 'vhdl-port)


(defgroup vhdl-testbench nil
  "Customizations for testbench generation."
  :group 'vhdl-port)

(defcustom vhdl-testbench-entity-name '(".*" . "\\&_tb")
  (concat
   "*Specifies how the testbench entity name is obtained.
The entity name of a testbench can be obtained by modifying the name of
the component to be tested (e.g. attaching or stripping off a substring)."
   vhdl-name-doc-string)
  :type '(cons (regexp :tag "From regexp")
	       (string :tag "To string  "))
  :group 'vhdl-testbench)

(defcustom vhdl-testbench-architecture-name '(".*" . "")
  (concat
   "*Specifies how the testbench architecture name is obtained.
The testbench architecture name can be obtained by modifying the name of
the component to be tested (e.g. attaching or stripping off a substring).
If TO STRING is empty, the architecture name is queried."
   vhdl-name-doc-string)
  :type '(cons (regexp :tag "From regexp")
	       (string :tag "To string  "))
  :group 'vhdl-testbench)

(defcustom vhdl-testbench-configuration-name vhdl-compose-configuration-name
  (concat
   "*Specifies how the testbench configuration name is obtained.
The configuration name of a testbench can be obtained by modifying the entity
and/or architecture name (e.g. attaching or stripping off a substring).  The
string that is matched against the regexp is the concatenation of the entity
and the architecture name separated by a space.  This gives access to both
names (see default setting as example)."
   vhdl-name-doc-string)
  :type '(cons (regexp :tag "From regexp")
	       (string :tag "To string  "))
  :group 'vhdl-testbench)

(defcustom vhdl-testbench-dut-name '(".*" . "DUT")
  (concat
   "*Specifies how a DUT instance name is obtained.
The design-under-test instance name (i.e. the component instantiated in the
testbench) can be obtained by modifying the component name (e.g. attaching
or stripping off a substring)."
   vhdl-name-doc-string)
  :type '(cons (regexp :tag "From regexp")
	       (string :tag "To string  "))
  :group 'vhdl-testbench)

(defcustom vhdl-testbench-include-header t
  "*Non-nil means include a header in automatically generated files."
  :type 'boolean
  :group 'vhdl-testbench)

(defcustom vhdl-testbench-declarations "\
  -- clock
  signal Clk : std_logic := '1';
"
  "*String or file to be inserted in the testbench declarative part.
If the string specifies an existing file name, the contents of the file is
inserted, otherwise the string itself is inserted in the testbench
architecture before the BEGIN keyword.
Type `C-j' for newlines."
  :type 'string
  :group 'vhdl-testbench)

(defcustom vhdl-testbench-statements "\
  -- clock generation
  Clk <= not Clk after 10 ns;

  -- waveform generation
  WaveGen_Proc: process
  begin
    -- insert signal assignments here

    wait until Clk = '1';
  end process WaveGen_Proc;
"
  "*String or file to be inserted in the testbench statement part.
If the string specifies an existing file name, the contents of the file is
inserted, otherwise the string itself is inserted in the testbench
architecture before the END keyword.
Type `C-j' for newlines."
  :type 'string
  :group 'vhdl-testbench)

(defcustom vhdl-testbench-initialize-signals nil
  "*Non-nil means initialize signals with `0' when declared in testbench."
  :type 'boolean
  :group 'vhdl-testbench)

(defcustom vhdl-testbench-include-library t
  "*Non-nil means a library/use clause for std_logic_1164 is included."
  :type 'boolean
  :group 'vhdl-testbench)

(defcustom vhdl-testbench-include-configuration t
  "*Non-nil means a testbench configuration is attached at the end."
  :type 'boolean
  :group 'vhdl-testbench)

(defcustom vhdl-testbench-create-files 'single
  "*Specifies whether new files should be created for the testbench.
testbench entity and architecture are inserted:
  None          : in current buffer
  Single file   : in new single file
  Separate files: in two separate files
The file names are obtained from variables `vhdl-testbench-entity-file-name'
and `vhdl-testbench-architecture-file-name'."
  :type '(choice (const :tag "None" none)
		 (const :tag "Single file" single)
		 (const :tag "Separate files" separate))
  :group 'vhdl-testbench)

(defcustom vhdl-testbench-entity-file-name vhdl-entity-file-name
  (concat
   "*Specifies how the testbench entity file name is obtained.
The entity file name can be obtained by modifying the testbench entity name
\(e.g. attaching or stripping off a substring).  The file extension is
automatically taken from the file name of the current buffer.  Testbench
files can be created in a different directory by prepending a relative or
absolute path to the file name."
   vhdl-name-doc-string)
  :type '(cons (regexp :tag "From regexp")
	       (string :tag "To string  "))
  :group 'vhdl-testbench)

(defcustom vhdl-testbench-architecture-file-name vhdl-architecture-file-name
  (concat
   "*Specifies how the testbench architecture file name is obtained.
The architecture file name can be obtained by modifying the testbench entity
and/or architecture name (e.g. attaching or stripping off a substring).  The
string that is matched against the regexp is the concatenation of the entity
and the architecture name separated by a space.  This gives access to both
names (see default setting as example).  Testbench files can be created in
a different directory by prepending a relative or absolute path to the file
name."
   vhdl-name-doc-string)
  :type '(cons (regexp :tag "From regexp")
	       (string :tag "To string  "))
  :group 'vhdl-testbench)


(defgroup vhdl-comment nil
  "Customizations for comments."
  :group 'vhdl)

(defcustom vhdl-self-insert-comments t
  "*Non-nil means various templates automatically insert help comments."
  :type 'boolean
  :group 'vhdl-comment)

(defcustom vhdl-prompt-for-comments t
  "*Non-nil means various templates prompt for user definable comments."
  :type 'boolean
  :group 'vhdl-comment)

(defcustom vhdl-inline-comment-column 40
  "*Column to indent and align inline comments to.
Overrides local option `comment-column'.

NOTE: Activate the new setting in a VHDL buffer by using the menu entry
      \"Activate Options\"."
  :type 'integer
  :group 'vhdl-comment)

(defcustom vhdl-end-comment-column 79
  "*End of comment column.
Comments that exceed this column number are wrapped.

NOTE: Activate the new setting in a VHDL buffer by using the menu entry
      \"Activate Options\"."
  :type 'integer
  :group 'vhdl-comment)

(defvar end-comment-column)


(defgroup vhdl-align nil
  "Customizations for alignment."
  :group 'vhdl)

(defcustom vhdl-auto-align t
  "*Non-nil means align some templates automatically after generation."
  :type 'boolean
  :group 'vhdl-align)

(defcustom vhdl-align-groups t
  "*Non-nil means align groups of code lines separately.
A group of code lines is a region of consecutive lines between two lines that
match the regexp in option `vhdl-align-group-separate'."
  :type 'boolean
  :group 'vhdl-align)

(defcustom vhdl-align-group-separate "^\\s-*$"
  "*Regexp for matching a line that separates groups of lines for alignment.
Examples:
  \"^\\s-*$\":          matches an empty line
  \"^\\s-*\\(--.*\\)?$\": matches an empty line or a comment-only line"
  :type 'regexp
  :group 'vhdl-align)

(defcustom vhdl-align-same-indent t
  "*Non-nil means align blocks with same indent separately.
When a region or the entire buffer is aligned, the code is divided into
blocks of same indent which are aligned separately (except for argument/port
lists).  This gives nicer alignment in most cases.
Option `vhdl-align-groups' still applies within these blocks."
  :type 'boolean
  :group 'vhdl-align)


(defgroup vhdl-highlight nil
  "Customizations for highlighting."
  :group 'vhdl)

(defcustom vhdl-highlight-keywords t
  "*Non-nil means highlight VHDL keywords and other standardized words.
The following faces are used:
  `font-lock-keyword-face'       : keywords
  `font-lock-type-face'          : standardized types
  `vhdl-font-lock-attribute-face': standardized attributes
  `vhdl-font-lock-enumvalue-face': standardized enumeration values
  `vhdl-font-lock-function-face' : standardized function and package names

NOTE: Activate the new setting in a VHDL buffer by re-fontifying it (menu
      entry \"Fontify Buffer\")."
  :type 'boolean
  :set (lambda (variable value)
	 (vhdl-custom-set variable value 'vhdl-font-lock-init))
  :group 'vhdl-highlight)

(defcustom vhdl-highlight-names t
  "*Non-nil means highlight declaration names and construct labels.
The following faces are used:
  `font-lock-function-name-face' : names in declarations of units,
     subprograms, components, as well as labels of VHDL constructs
  `font-lock-type-face'          : names in type/nature declarations
  `vhdl-font-lock-attribute-face': names in attribute declarations
  `font-lock-variable-name-face' : names in declarations of signals,
     variables, constants, subprogram parameters, generics, and ports

NOTE: Activate the new setting in a VHDL buffer by re-fontifying it (menu
      entry \"Fontify Buffer\")."
  :type 'boolean
  :set (lambda (variable value)
	 (vhdl-custom-set variable value 'vhdl-font-lock-init))
  :group 'vhdl-highlight)

(defcustom vhdl-highlight-special-words nil
  "*Non-nil means highlight words with special syntax.
The words with syntax and color specified in option `vhdl-special-syntax-alist'
are highlighted accordingly.
Can be used for visual support of naming conventions.

NOTE: Activate the new setting in a VHDL buffer by re-fontifying it (menu
      entry \"Fontify Buffer\")."
  :type 'boolean
  :set (lambda (variable value)
	 (vhdl-custom-set variable value 'vhdl-font-lock-init))
  :group 'vhdl-highlight)

(defcustom vhdl-highlight-forbidden-words nil
  "*Non-nil means highlight forbidden words.
The reserved words specified in option `vhdl-forbidden-words' or having the
syntax specified in option `vhdl-forbidden-syntax' are highlighted in a
warning color (face `vhdl-font-lock-reserved-words-face') to indicate not to
use them.

NOTE: Activate the new setting in a VHDL buffer by re-fontifying it (menu
      entry \"Fontify Buffer\")."
  :type 'boolean
  :set (lambda (variable value)
	 (vhdl-custom-set variable value
			  'vhdl-words-init 'vhdl-font-lock-init))
  :group 'vhdl-highlight)

(defcustom vhdl-highlight-verilog-keywords nil
  "*Non-nil means highlight Verilog keywords as reserved words.
Verilog keywords are highlighted in a warning color (face
`vhdl-font-lock-reserved-words-face') to indicate not to use them.

NOTE: Activate the new setting in a VHDL buffer by re-fontifying it (menu
      entry \"Fontify Buffer\")."
  :type 'boolean
  :set (lambda (variable value)
	 (vhdl-custom-set variable value
			  'vhdl-words-init 'vhdl-font-lock-init))
  :group 'vhdl-highlight)

(defcustom vhdl-highlight-translate-off nil
  "*Non-nil means background-highlight code excluded from translation.
That is, all code between \"-- pragma translate_off\" and
\"-- pragma translate_on\" is highlighted using a different background color
\(face `vhdl-font-lock-translate-off-face').
Note: this might slow down on-the-fly fontification (and thus editing).

NOTE: Activate the new setting in a VHDL buffer by re-fontifying it (menu
      entry \"Fontify Buffer\")."
  :type 'boolean
  :set (lambda (variable value)
	 (vhdl-custom-set variable value 'vhdl-font-lock-init))
  :group 'vhdl-highlight)

(defcustom vhdl-highlight-case-sensitive nil
  "*Non-nil means consider case for highlighting.
Possible trade-off:
  non-nil  also upper-case VHDL words are highlighted, but case of words with
           special syntax is not considered
  nil      only lower-case VHDL words are highlighted, but case of words with
           special syntax is considered
Overrides local option `font-lock-keywords-case-fold-search'.

NOTE: Activate the new setting in a VHDL buffer by re-fontifying it (menu
      entry \"Fontify Buffer\")."
  :type 'boolean
  :group 'vhdl-highlight)

(defcustom vhdl-special-syntax-alist
  '(("generic/constant" "\\w+_[cg]" "Gold3" "BurlyWood1")
    ("type" "\\w+_t" "ForestGreen" "PaleGreen")
    ("variable" "\\w+_v" "Grey50" "Grey80"))
  "*List of special syntax to be highlighted.
If option `vhdl-highlight-special-words' is non-nil, words with the specified
syntax (as regular expression) are highlighted in the corresponding color.

  Name         : string of words and spaces
  Regexp       : regular expression describing word syntax
                  (e.g. \"\\\w+_c\" matches word with suffix \"_c\")
  Color (light): foreground color for light background
                 (matching color examples: Gold3, Grey50, LimeGreen, Tomato,
                 LightSeaGreen, DodgerBlue, Gold, PaleVioletRed)
  Color (dark) : foreground color for dark background
                 (matching color examples: BurlyWood1, Grey80, Green, Coral,
                 AquaMarine2, LightSkyBlue1, Yellow, PaleVioletRed1)

Can be used for visual support of naming conventions, such as highlighting
different kinds of signals (e.g. \"Clk50\", \"Rst_n\") or objects (e.g.
\"Signal_s\", \"Variable_v\", \"Constant_c\") by distinguishing them using
common substrings or name suffices.
For each entry, a new face is generated with the specified colors and name
\"vhdl-font-lock-\" + name + \"-face\".

NOTE: Activate a changed regexp in a VHDL buffer by re-fontifying it (menu
      entry \"Fontify Buffer\").  All other changes require restarting Emacs."
  :type '(repeat (list :tag "Face" :indent 2
		       (string :tag "Name         ")
		       (regexp :tag "Regexp       " "\\w+_")
		       (string :tag "Color (light)")
		       (string :tag "Color (dark) ")))
  :set (lambda (variable value)
	 (vhdl-custom-set variable value 'vhdl-font-lock-init))
  :group 'vhdl-highlight)

(defcustom vhdl-forbidden-words '()
  "*List of forbidden words to be highlighted.
If option `vhdl-highlight-forbidden-words' is non-nil, these reserved
words are highlighted in a warning color to indicate not to use them.

NOTE: Activate the new setting in a VHDL buffer by re-fontifying it (menu
      entry \"Fontify Buffer\")."
  :type '(repeat (string :format "%v"))
  :set (lambda (variable value)
	 (vhdl-custom-set variable value
			  'vhdl-words-init 'vhdl-font-lock-init))
  :group 'vhdl-highlight)

(defcustom vhdl-forbidden-syntax ""
  "*Syntax of forbidden words to be highlighted.
If option `vhdl-highlight-forbidden-words' is non-nil, words with this
syntax are highlighted in a warning color to indicate not to use them.
Can be used to highlight too long identifiers (e.g. \"\\w\\w\\w\\w\\w\\w\\w\\w\\w\\w+\"
highlights identifiers with 10 or more characters).

NOTE: Activate the new setting in a VHDL buffer by re-fontifying it (menu
      entry \"Fontify Buffer\")."
  :type 'regexp
  :set (lambda (variable value)
	 (vhdl-custom-set variable value
			  'vhdl-words-init 'vhdl-font-lock-init))
  :group 'vhdl-highlight)

(defcustom vhdl-directive-keywords '("pragma" "synopsys")
  "*List of compiler directive keywords recognized for highlighting.

NOTE: Activate the new setting in a VHDL buffer by re-fontifying it (menu
      entry \"Fontify Buffer\")."
  :type '(repeat (string :format "%v"))
  :set (lambda (variable value)
	 (vhdl-custom-set variable value
			  'vhdl-words-init 'vhdl-font-lock-init))
  :group 'vhdl-highlight)


(defgroup vhdl-speedbar nil
  "Customizations for speedbar."
  :group 'vhdl)

(defcustom vhdl-speedbar-auto-open nil
  "*Non-nil means automatically open speedbar at startup.
Alternatively, the speedbar can be opened from the VHDL menu."
  :type 'boolean
  :group 'vhdl-speedbar)

(defcustom vhdl-speedbar-display-mode 'files
  "*Specifies the default displaying mode when opening speedbar.
Alternatively, the displaying mode can be selected from the speedbar menu or
by typing `f' (files), `h' (directory hierarchy) or `H' (project hierarchy)."
  :type '(choice (const :tag "Files" files)
		 (const :tag "Directory hierarchy" directory)
		 (const :tag "Project hierarchy" project))
  :group 'vhdl-speedbar)

(defcustom vhdl-speedbar-scan-limit '(10000000 (1000000 50))
  "*Limits scanning of large files and netlists.
Design units: maximum file size to scan for design units
Hierarchy (instances of subcomponents):
  File size: maximum file size to scan for instances (in bytes)
  Instances per arch: maximum number of instances to scan per architecture

\"None\" always means that there is no limit.
In case of files not or incompletely scanned, a warning message and the file
names are printed out.
Background: scanning for instances is considerably slower than scanning for
design units, especially when there are many instances.  These limits should
prevent the scanning of large netlists."
  :type '(list (choice :tag "Design units"
		       :format "%t        : %[Value Menu%] %v"
		       (const :tag "None" nil)
		       (integer :tag "File size"))
	       (list :tag "Hierarchy" :indent 2
		     (choice :tag "File size"
			     :format "%t         : %[Value Menu%] %v"
			     (const :tag "None" nil)
			     (integer :tag "Size     "))
		     (choice :tag "Instances per arch"
			     (const :tag "None" nil)
			     (integer :tag "Number   "))))
  :group 'vhdl-speedbar)

(defcustom vhdl-speedbar-jump-to-unit t
  "*Non-nil means jump to the design unit code when opened in a buffer.
The buffer cursor position is left unchanged otherwise."
  :type 'boolean
  :group 'vhdl-speedbar)

(defcustom vhdl-speedbar-update-on-saving t
  "*Automatically update design hierarchy when buffer is saved."
  :type 'boolean
  :group 'vhdl-speedbar)

(defcustom vhdl-speedbar-save-cache '(hierarchy display)
  "*Automatically save modified hierarchy caches when exiting Emacs.
  Hierarchy: design hierarchy information
  Display:   displaying information (which design units to expand)"
  :type '(set (const :tag "Hierarchy" hierarchy)
	      (const :tag "Display"   display))
  :group 'vhdl-speedbar)

(defcustom vhdl-speedbar-cache-file-name ".emacs-vhdl-cache-\\1-\\2"
  "*Name of file for saving hierarchy cache.
\"\\1\" is replaced by the project name if a project is specified,
\"directory\" otherwise.  \"\\2\" is replaced by the user name (allows for
different users to have cache files in the same directory). Can also have
an absolute path (i.e. all caches can be stored in one global directory)."
  :type 'string
  :group 'vhdl-speedbar)


(defgroup vhdl-menu nil
  "Customizations for menus."
  :group 'vhdl)

(defcustom vhdl-index-menu nil
  "*Non-nil means add an index menu for a source file when loading.
Alternatively, the speedbar can be used.  Note that the index menu scans a file
when it is opened, while speedbar only scans the file upon request."
  :type 'boolean
  :group 'vhdl-menu)

(defcustom vhdl-source-file-menu nil
  "*Non-nil means add a menu of all source files in current directory.
Alternatively, the speedbar can be used."
  :type 'boolean
  :group 'vhdl-menu)

(defcustom vhdl-hideshow-menu nil
  "*Non-nil means add hideshow menu and functionality at startup.
Hideshow can also be enabled from the VHDL Mode menu.
Hideshow allows hiding code of various VHDL constructs.

NOTE: Activate the new setting in a VHDL buffer by using the menu entry
      \"Activate Options\"."
  :type 'boolean
  :group 'vhdl-menu)

(defcustom vhdl-hide-all-init nil
  "*Non-nil means hide all design units initially after a file is loaded."
  :type 'boolean
  :group 'vhdl-menu)


(defgroup vhdl-print nil
  "Customizations for printing."
  :group 'vhdl)

(defcustom vhdl-print-two-column t
  "*Non-nil means print code in two columns and landscape format.
Adjusts settings in a way that PostScript printing (\"File\" menu, `ps-print')
prints VHDL files in a nice two-column landscape style.

NOTE: Activate the new setting by restarting Emacs.
      Overrides `ps-print' settings locally."
  :type 'boolean
  :group 'vhdl-print)

(defcustom vhdl-print-customize-faces t
  "*Non-nil means use an optimized set of faces for PostScript printing.

NOTE: Activate the new setting by restarting Emacs.
      Overrides `ps-print' settings locally."
  :type 'boolean
  :group 'vhdl-print)


(defgroup vhdl-misc nil
  "Miscellaneous customizations."
  :group 'vhdl)

(defcustom vhdl-intelligent-tab t
  "*Non-nil means `TAB' does indentation, word completion and tab insertion.
That is, if preceding character is part of a word then complete word,
else if not at beginning of line then insert tab,
else if last command was a `TAB' or `RET' then dedent one step,
else indent current line (i.e. `TAB' is bound to `vhdl-electric-tab').
If nil, TAB always indents current line (i.e. `TAB' is bound to
`indent-according-to-mode').

NOTE: Activate the new setting in a VHDL buffer by using the menu entry
      \"Activate Options\"."
  :type 'boolean
  :group 'vhdl-misc)

(defcustom vhdl-indent-syntax-based t
  "*Non-nil means indent lines of code based on their syntactic context.
Otherwise, a line is indented like the previous nonblank line.  This can be
useful in large files where syntax-based indentation gets very slow."
  :type 'boolean
  :group 'vhdl-misc)

(defcustom vhdl-word-completion-case-sensitive nil
  "*Non-nil means word completion using `TAB' is case sensitive.
That is, `TAB' completes words that start with the same letters and case.
Otherwise, case is ignored."
  :type 'boolean
  :group 'vhdl-misc)

(defcustom vhdl-word-completion-in-minibuffer t
  "*Non-nil enables word completion in minibuffer (for template prompts).

NOTE: Activate the new setting by restarting Emacs."
  :type 'boolean
  :group 'vhdl-misc)

(defcustom vhdl-underscore-is-part-of-word nil
  "*Non-nil means consider the underscore character `_' as part of word.
An identifier containing underscores is then treated as a single word in
select and move operations.  All parts of an identifier separated by underscore
are treated as single words otherwise.

NOTE: Activate the new setting in a VHDL buffer by using the menu entry
      \"Activate Options\"."
  :type 'boolean
  :set (lambda (variable value)
	 (vhdl-custom-set variable value 'vhdl-mode-syntax-table-init))
  :group 'vhdl-misc)


(defgroup vhdl-related nil
  "Related general customizations."
  :group 'vhdl)

;; add related general customizations
(custom-add-to-group 'vhdl-related 'hideshow 'custom-group)
(if (featurep 'xemacs)
    (custom-add-to-group 'vhdl-related 'paren-mode 'custom-variable)
  (custom-add-to-group 'vhdl-related 'paren-showing 'custom-group))
(custom-add-to-group 'vhdl-related 'ps-print 'custom-group)
(custom-add-to-group 'vhdl-related 'speedbar 'custom-group)
(custom-add-to-group 'vhdl-related 'line-number-mode 'custom-variable)
(unless (featurep 'xemacs)
  (custom-add-to-group 'vhdl-related 'transient-mark-mode 'custom-variable))
(custom-add-to-group 'vhdl-related 'user-full-name 'custom-variable)
(custom-add-to-group 'vhdl-related 'mail-host-address 'custom-variable)
(custom-add-to-group 'vhdl-related 'user-mail-address 'custom-variable)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal variables

(defvar vhdl-menu-max-size 20
  "*Specifies the maximum size of a menu before splitting it into submenus.")

(defvar vhdl-progress-interval 1
  "*Interval used to update progress status during long operations.
If a number, percentage complete gets updated after each interval of
that many seconds.  To inhibit all messages, set this option to nil.")

(defvar vhdl-inhibit-startup-warnings-p nil
  "*If non-nil, inhibits start up compatibility warnings.")

(defvar vhdl-strict-syntax-p nil
  "*If non-nil, all syntactic symbols must be found in `vhdl-offsets-alist'.
If the syntactic symbol for a particular line does not match a symbol
in the offsets alist, an error is generated, otherwise no error is
reported and the syntactic symbol is ignored.")

(defvar vhdl-echo-syntactic-information-p nil
  "*If non-nil, syntactic info is echoed when the line is indented.")

(defconst vhdl-offsets-alist-default
  '((string		   . -1000)
    (cpp-macro		   . -1000)
    (block-open		   . 0)
    (block-close	   . 0)
    (statement		   . 0)
    (statement-cont	   . vhdl-lineup-statement-cont)
    (statement-block-intro . +)
    (statement-case-intro  . +)
    (case-alternative	   . +)
    (comment		   . vhdl-lineup-comment)
    (arglist-intro	   . +)
    (arglist-cont	   . 0)
    (arglist-cont-nonempty . vhdl-lineup-arglist)
    (arglist-close	   . vhdl-lineup-arglist)
    (entity		   . 0)
    (configuration	   . 0)
    (package		   . 0)
    (architecture	   . 0)
    (package-body	   . 0)
    )
  "Default settings for offsets of syntactic elements.
Do not change this constant!  See the variable `vhdl-offsets-alist' for
more information.")

(defvar vhdl-offsets-alist (copy-alist vhdl-offsets-alist-default)
  "*Association list of syntactic element symbols and indentation offsets.
As described below, each cons cell in this list has the form:

    (SYNTACTIC-SYMBOL . OFFSET)

When a line is indented, `vhdl-mode' first determines the syntactic
context of the line by generating a list of symbols called syntactic
elements.  This list can contain more than one syntactic element and
the global variable `vhdl-syntactic-context' contains the context list
for the line being indented.  Each element in this list is actually a
cons cell of the syntactic symbol and a buffer position.  This buffer
position is call the relative indent point for the line.  Some
syntactic symbols may not have a relative indent point associated with
them.

After the syntactic context list for a line is generated, `vhdl-mode'
calculates the absolute indentation for the line by looking at each
syntactic element in the list.  First, it compares the syntactic
element against the SYNTACTIC-SYMBOL's in `vhdl-offsets-alist'.  When it
finds a match, it adds the OFFSET to the column of the relative indent
point.  The sum of this calculation for each element in the syntactic
list is the absolute offset for line being indented.

If the syntactic element does not match any in the `vhdl-offsets-alist',
an error is generated if `vhdl-strict-syntax-p' is non-nil, otherwise
the element is ignored.

Actually, OFFSET can be an integer, a function, a variable, or one of
the following symbols: `+', `-', `++', or `--'.  These latter
designate positive or negative multiples of `vhdl-basic-offset',
respectively: *1, *-1, *2, and *-2.  If OFFSET is a function, it is
called with a single argument containing the cons of the syntactic
element symbol and the relative indent point.  The function should
return an integer offset.

Here is the current list of valid syntactic element symbols:

 string                 -- inside multi-line string
 block-open             -- statement block open
 block-close            -- statement block close
 statement              -- a VHDL statement
 statement-cont         -- a continuation of a VHDL statement
 statement-block-intro  -- the first line in a new statement block
 statement-case-intro   -- the first line in a case alternative block
 case-alternative       -- a case statement alternative clause
 comment                -- a line containing only a comment
 arglist-intro          -- the first line in an argument list
 arglist-cont           -- subsequent argument list lines when no
                           arguments follow on the same line as
                           the arglist opening paren
 arglist-cont-nonempty  -- subsequent argument list lines when at
                           least one argument follows on the same
                           line as the arglist opening paren
 arglist-close          -- the solo close paren of an argument list
 entity                 -- inside an entity declaration
 configuration          -- inside a configuration declaration
 package                -- inside a package declaration
 architecture           -- inside an architecture body
 package-body           -- inside a package body")

(defvar vhdl-comment-only-line-offset 0
  "*Extra offset for line which contains only the start of a comment.
Can contain an integer or a cons cell of the form:

 (NON-ANCHORED-OFFSET . ANCHORED-OFFSET)

Where NON-ANCHORED-OFFSET is the amount of offset given to
non-column-zero anchored comment-only lines, and ANCHORED-OFFSET is
the amount of offset to give column-zero anchored comment-only lines.
Just an integer as value is equivalent to (<val> . 0)")

(defvar vhdl-special-indent-hook nil
  "*Hook for user defined special indentation adjustments.
This hook gets called after a line is indented by the mode.")

(defvar vhdl-style-alist
  '(("IEEE"
     (vhdl-basic-offset . 4)
     (vhdl-offsets-alist . ())))
  "Styles of Indentation.
Elements of this alist are of the form:

  (STYLE-STRING (VARIABLE . VALUE) [(VARIABLE . VALUE) ...])

where STYLE-STRING is a short descriptive string used to select a
style, VARIABLE is any `vhdl-mode' variable, and VALUE is the intended
value for that variable when using the selected style.

There is one special case when VARIABLE is `vhdl-offsets-alist'.  In this
case, the VALUE is a list containing elements of the form:

  (SYNTACTIC-SYMBOL . VALUE)

as described in `vhdl-offsets-alist'.  These are passed directly to
`vhdl-set-offset' so there is no need to set every syntactic symbol in
your style, only those that are different from the default.")

;; dynamically append the default value of most variables
(or (assoc "Default" vhdl-style-alist)
    (let* ((varlist '(vhdl-inhibit-startup-warnings-p
		      vhdl-strict-syntax-p
		      vhdl-echo-syntactic-information-p
		      vhdl-basic-offset
		      vhdl-offsets-alist
		      vhdl-comment-only-line-offset))
	   (default (cons "Default"
			  (mapcar
			   (function
			    (lambda (var)
			      (cons var (symbol-value var))))
			   varlist))))
      (setq vhdl-style-alist (cons default vhdl-style-alist))))

(defvar vhdl-mode-hook nil
  "*Hook called by `vhdl-mode'.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Required packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; mandatory
(require 'assoc)
(require 'compile)			; XEmacs
(require 'easymenu)
(require 'hippie-exp)

;; optional (minimize warning messages during compile)
(eval-when-compile
  (require 'font-lock)
  (require 'ps-print)
  (require 'speedbar))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Compatibility
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; XEmacs compatibility

;; active regions
(defun vhdl-keep-region-active ()
  "Do whatever is necessary to keep the region active in XEmacs.
Ignore byte-compiler warnings you might see."
  (and (featurep 'xemacs)
       (setq zmacs-region-stays t)))

;; `wildcard-to-regexp' is included only in XEmacs 21
(unless (fboundp 'wildcard-to-regexp)
  (defun wildcard-to-regexp (wildcard)
    "Simplified version of `wildcard-to-regexp' from Emacs's `files.el'."
    (let* ((i (string-match "[*?]" wildcard))
	   (result (substring wildcard 0 i))
	   (len (length wildcard)))
      (when i
	(while (< i len)
	  (let ((ch (aref wildcard i)))
	    (setq result (concat result
				 (cond ((eq ch ?*)  "[^\000]*")
				       ((eq ch ??)  "[^\000]")
				       (t (char-to-string ch)))))
	    (setq i (1+ i)))))
      (concat "\\`" result "\\'"))))

;; `regexp-opt' undefined (`xemacs-devel' not installed)
;; `regexp-opt' accelerates fontification by 10-20%
(unless (fboundp 'regexp-opt)
;  (vhdl-warning-when-idle "Please install `xemacs-devel' package.")
  (defun regexp-opt (strings &optional paren)
    (let ((open (if paren "\\(" "")) (close (if paren "\\)" "")))
      (concat open (mapconcat 'regexp-quote strings "\\|") close))))

;; `match-string-no-properties' undefined (XEmacs, what else?)
(unless (fboundp 'match-string-no-properties)
  (defalias 'match-string-no-properties 'match-string))

;; `subst-char-in-string' undefined (XEmacs)
(unless (fboundp 'subst-char-in-string)
  (defun subst-char-in-string (fromchar tochar string &optional inplace)
    (let ((i (length string))
	  (newstr (if inplace string (copy-sequence string))))
      (while (> i 0)
	(setq i (1- i))
	(if (eq (aref newstr i) fromchar) (aset newstr i tochar)))
      newstr)))

;; `itimer.el': idle timer bug fix in version 1.09 (XEmacs 21.1.9)
(when (and (featurep 'xemacs) (string< itimer-version "1.09")
	   (not noninteractive))
  (load "itimer")
  (when (string< itimer-version "1.09")
    (message "WARNING:  Install included `itimer.el' patch first (see INSTALL file)")
    (beep) (sit-for 5)))

;; `file-expand-wildcards' undefined (XEmacs)
(unless (fboundp 'file-expand-wildcards)
  (defun file-expand-wildcards (pattern &optional full)
    "Taken from Emacs's `files.el'."
    (let* ((nondir (file-name-nondirectory pattern))
    	   (dirpart (file-name-directory pattern))
    	   (dirs (if (and dirpart (string-match "[[*?]" dirpart))
		     (mapcar 'file-name-as-directory
			     (file-expand-wildcards (directory-file-name dirpart)))
		   (list dirpart)))
	   contents)
      (while dirs
	(when (or (null (car dirs))	; Possible if DIRPART is not wild.
		  (file-directory-p (directory-file-name (car dirs))))
	  (let ((this-dir-contents
		 (delq nil
		       (mapcar #'(lambda (name)
				   (unless (string-match "\\`\\.\\.?\\'"
							 (file-name-nondirectory name))
				     name))
			       (directory-files (or (car dirs) ".") full
						(wildcard-to-regexp nondir))))))
	    (setq contents
		  (nconc
		   (if (and (car dirs) (not full))
		       (mapcar (function (lambda (name) (concat (car dirs) name)))
			       this-dir-contents)
		     this-dir-contents)
		   contents))))
	(setq dirs (cdr dirs)))
      contents)))

;; `member-ignore-case' undefined (XEmacs)
(unless (fboundp 'member-ignore-case)
  (defalias 'member-ignore-case 'member))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compatibility with older VHDL Mode versions

(defvar vhdl-warnings nil
  "Warnings to tell the user during start up.")

(defun vhdl-run-when-idle (secs repeat function)
  "Wait until idle, then run FUNCTION."
  (if (fboundp 'start-itimer)
      (start-itimer "vhdl-mode" function secs repeat t)
;    (run-with-idle-timer secs repeat function)))
    ;; explicitly activate timer (necessary when Emacs is already idle)
    (aset (run-with-idle-timer secs repeat function) 0 nil)))

(defun vhdl-warning-when-idle (&rest args)
  "Wait until idle, then print out warning STRING and beep."
  (if noninteractive
      (vhdl-warning (apply 'format args) t)
    (unless vhdl-warnings
      (vhdl-run-when-idle .1 nil 'vhdl-print-warnings))
    (setq vhdl-warnings (cons (apply 'format args) vhdl-warnings))))

(defun vhdl-warning (string &optional nobeep)
  "Print out warning STRING and beep."
  (message "WARNING:  %s" string)
  (unless (or nobeep noninteractive) (beep)))

(defun vhdl-print-warnings ()
  "Print out messages in variable `vhdl-warnings'."
  (let ((no-warnings (length vhdl-warnings)))
    (setq vhdl-warnings (nreverse vhdl-warnings))
    (while vhdl-warnings
      (message "WARNING:  %s" (car vhdl-warnings))
      (setq vhdl-warnings (cdr vhdl-warnings)))
    (beep)
    (when (> no-warnings 1)
      (message "WARNING:  See warnings in message buffer (type `C-c M-m')."))))

;; Backward compatibility checks and fixes
;; option `vhdl-compiler' changed format
(unless (stringp vhdl-compiler)
  (setq vhdl-compiler "ModelSim")
  (vhdl-warning-when-idle "Option `vhdl-compiler' has changed format; customize again"))

;; option `vhdl-standard' changed format
(unless (listp vhdl-standard)
  (setq vhdl-standard '(87 nil))
  (vhdl-warning-when-idle "Option `vhdl-standard' has changed format; customize again"))

;; option `vhdl-model-alist' changed format
(when (= (length (car vhdl-model-alist)) 3)
  (let ((old-alist vhdl-model-alist)
	new-alist)
    (while old-alist
      (setq new-alist (cons (append (car old-alist) '("")) new-alist))
      (setq old-alist (cdr old-alist)))
    (setq vhdl-model-alist (nreverse new-alist)))
  (customize-save-variable 'vhdl-model-alist vhdl-model-alist))

;; option `vhdl-project-alist' changed format
(when (= (length (car vhdl-project-alist)) 3)
  (let ((old-alist vhdl-project-alist)
	new-alist)
    (while old-alist
      (setq new-alist (cons (append (car old-alist) '("")) new-alist))
      (setq old-alist (cdr old-alist)))
    (setq vhdl-project-alist (nreverse new-alist)))
  (customize-save-variable 'vhdl-project-alist vhdl-project-alist))

;; option `vhdl-project-alist' changed format (3.31.1)
(when (= (length (car vhdl-project-alist)) 4)
  (let ((old-alist vhdl-project-alist)
	new-alist elem)
    (while old-alist
      (setq elem (car old-alist))
      (setq new-alist
	    (cons (list (nth 0 elem) (nth 1 elem) "" (nth 2 elem)
			nil "./" "work" "work/" "Makefile" (nth 3 elem))
		  new-alist))
      (setq old-alist (cdr old-alist)))
    (setq vhdl-project-alist (nreverse new-alist)))
  (vhdl-warning-when-idle "Option `vhdl-project-alist' changed format; please re-customize"))

;; option `vhdl-project-alist' changed format (3.31.12)
(when (= (length (car vhdl-project-alist)) 10)
  (let ((tmp-alist vhdl-project-alist))
    (while tmp-alist
      (setcdr (nthcdr 3 (car tmp-alist))
	      (cons "" (nthcdr 4 (car tmp-alist))))
      (setq tmp-alist (cdr tmp-alist))))
  (customize-save-variable 'vhdl-project-alist vhdl-project-alist))

;; option `vhdl-compiler-alist' changed format (3.31.1)
(when (= (length (car vhdl-compiler-alist)) 7)
  (let ((old-alist vhdl-compiler-alist)
	new-alist elem)
    (while old-alist
      (setq elem (car old-alist))
      (setq new-alist
	    (cons (list (nth 0 elem) (nth 1 elem) "" "make -f \\1"
			(if (equal (nth 3 elem) "") nil (nth 3 elem))
			(nth 4 elem) "work/" "Makefile" (downcase (nth 0 elem))
			(nth 5 elem) (nth 6 elem) nil)
		  new-alist))
      (setq old-alist (cdr old-alist)))
    (setq vhdl-compiler-alist (nreverse new-alist)))
  (vhdl-warning-when-idle "Option `vhdl-compiler-alist' changed; please reset and re-customize"))

;; option `vhdl-compiler-alist' changed format (3.31.10)
(when (= (length (car vhdl-compiler-alist)) 12)
  (let ((tmp-alist vhdl-compiler-alist))
    (while tmp-alist
      (setcdr (nthcdr 4 (car tmp-alist))
	      (cons "mkdir \\1" (nthcdr 5 (car tmp-alist))))
      (setq tmp-alist (cdr tmp-alist))))
  (customize-save-variable 'vhdl-compiler-alist vhdl-compiler-alist))

;; option `vhdl-compiler-alist' changed format (3.31.11)
(when (= (length (car vhdl-compiler-alist)) 13)
  (let ((tmp-alist vhdl-compiler-alist))
    (while tmp-alist
      (setcdr (nthcdr 3 (car tmp-alist))
	      (cons "" (nthcdr 4 (car tmp-alist))))
      (setq tmp-alist (cdr tmp-alist))))
  (customize-save-variable 'vhdl-compiler-alist vhdl-compiler-alist))

;; option `vhdl-compiler-alist' changed format (3.32.7)
(when (= (length (nth 11 (car vhdl-compiler-alist))) 3)
  (let ((tmp-alist vhdl-compiler-alist))
    (while tmp-alist
      (setcdr (nthcdr 2 (nth 11 (car tmp-alist)))
	      '(0 . nil))
      (setq tmp-alist (cdr tmp-alist))))
  (customize-save-variable 'vhdl-compiler-alist vhdl-compiler-alist))

;; option `vhdl-project': empty value changed from "" to nil (3.31.1)
(when (equal vhdl-project "")
  (setq vhdl-project nil)
  (customize-save-variable 'vhdl-project vhdl-project))

;; option `vhdl-project-file-name': changed format (3.31.17 beta)
(when (stringp vhdl-project-file-name)
  (setq vhdl-project-file-name (list vhdl-project-file-name))
  (customize-save-variable 'vhdl-project-file-name vhdl-project-file-name))

;; option `speedbar-indentation-width': introduced in speedbar 0.10
(if (not (boundp 'speedbar-indentation-width))
    (defvar speedbar-indentation-width 2)
  ;; set default to 2 if not already customized
  (unless (get 'speedbar-indentation-width 'saved-value)
    (setq speedbar-indentation-width 2)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Help functions / inline substitutions / macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vhdl-standard-p (standard)
  "Check if STANDARD is specified as used standard."
  (or (eq standard (car vhdl-standard))
      (memq standard (cadr vhdl-standard))))

(defun vhdl-project-p (&optional warning)
  "Return non-nil if a project is displayed, i.e. directories or files are
specified."
  (if (assoc vhdl-project vhdl-project-alist)
      vhdl-project
    (when (and vhdl-project warning)
      (vhdl-warning-when-idle "Project does not exist: \"%s\"" vhdl-project))
    nil))

(defun vhdl-resolve-env-variable (string)
  "Resolve environment variables in STRING."
  (while (string-match "\\(.*\\)${?\\(\\(\\w\\|_\\)+\\)}?\\(.*\\)" string)
    (setq string (concat (match-string 1 string)
			 (getenv (match-string 2 string))
			 (match-string 4 string))))
  string)

(defun vhdl-default-directory ()
  "Return the default directory of the current project or the directory of the
current buffer if no project is defined."
  (if (vhdl-project-p)
      (expand-file-name (vhdl-resolve-env-variable
			 (nth 1 (aget vhdl-project-alist vhdl-project))))
    default-directory))

(defmacro vhdl-prepare-search-1 (&rest body)
  "Enable case insensitive search and switch to syntax table that includes '_',
then execute BODY, and finally restore the old environment.  Used for
consistent searching."
  `(let ((case-fold-search t))		; case insensitive search
     ;; use extended syntax table
     (with-syntax-table vhdl-mode-ext-syntax-table
       ,@body)))

(defmacro vhdl-prepare-search-2 (&rest body)
  "Enable case insensitive search, switch to syntax table that includes '_',
and remove `intangible' overlays, then execute BODY, and finally restore the
old environment.  Used for consistent searching."
  ;; FIXME: Why not just let-bind `inhibit-point-motion-hooks'?  --Stef
  `(let ((case-fold-search t)		; case insensitive search
	 (current-syntax-table (syntax-table))
	 overlay-all-list overlay-intangible-list overlay)
     ;; use extended syntax table
     (set-syntax-table vhdl-mode-ext-syntax-table)
     ;; remove `intangible' overlays
     (when (fboundp 'overlay-lists)
       (setq overlay-all-list (overlay-lists))
       (setq overlay-all-list
	     (append (car overlay-all-list) (cdr overlay-all-list)))
       (while overlay-all-list
	 (setq overlay (car overlay-all-list))
	 (when (memq 'intangible (overlay-properties overlay))
	   (setq overlay-intangible-list
		 (cons overlay overlay-intangible-list))
	   (overlay-put overlay 'intangible nil))
	 (setq overlay-all-list (cdr overlay-all-list))))
     ;; execute BODY safely
     (unwind-protect
         (progn ,@body)
       ;; restore syntax table
       (set-syntax-table current-syntax-table)
       ;; restore `intangible' overlays
       (when (fboundp 'overlay-lists)
         (while overlay-intangible-list
           (overlay-put (car overlay-intangible-list) 'intangible t)
           (setq overlay-intangible-list
                 (cdr overlay-intangible-list)))))))

(defmacro vhdl-visit-file (file-name issue-error &rest body)
  "Visit file FILE-NAME and execute BODY."
  `(if (null ,file-name)
       (progn ,@body)
     (unless (file-directory-p ,file-name)
       (let ((source-buffer (current-buffer))
	     (visiting-buffer (find-buffer-visiting ,file-name))
	     file-opened)
	 (when (or (and visiting-buffer (set-buffer visiting-buffer))
		   (condition-case ()
		       (progn (set-buffer (create-file-buffer ,file-name))
			      (setq file-opened t)
			      (vhdl-insert-file-contents ,file-name)
			      (modify-syntax-entry ?\- ". 12" (syntax-table))
			      (modify-syntax-entry ?\n ">" (syntax-table))
			      (modify-syntax-entry ?\^M ">" (syntax-table))
			      (modify-syntax-entry ?_ "w" (syntax-table))
			      t)
		     (error
		      (if ,issue-error
			  (progn
			    (when file-opened (kill-buffer (current-buffer)))
			    (set-buffer source-buffer)
			    (error "ERROR:  File cannot be opened: \"%s\"" ,file-name))
			(vhdl-warning (format "File cannot be opened: \"%s\"" ,file-name) t)
			nil))))
	   (condition-case info
	       (progn ,@body)
	     (error
	      (if ,issue-error
		  (progn
		    (when file-opened (kill-buffer (current-buffer)))
		    (set-buffer source-buffer)
		    (error (cadr info)))
		(vhdl-warning (cadr info))))))
	 (when file-opened (kill-buffer (current-buffer)))
	 (set-buffer source-buffer)))))

(defun vhdl-insert-file-contents (filename)
  "Nicked from `insert-file-contents-literally', but allow coding system
conversion."
  (let ((format-alist nil)
	(after-insert-file-functions nil)
	(jka-compr-compression-info-list nil))
    (insert-file-contents filename t)))

(defun vhdl-sort-alist (alist)
  "Sort ALIST."
  (sort alist (function (lambda (a b) (string< (car a) (car b))))))

(defun vhdl-get-subdirs (directory)
  "Recursively get subdirectories of DIRECTORY."
  (let ((dir-list (list (file-name-as-directory directory)))
	file-list)
    (setq file-list (vhdl-directory-files directory t "\\w.*"))
    (while file-list
      (when (file-directory-p (car file-list))
	(setq dir-list (append dir-list (vhdl-get-subdirs (car file-list)))))
      (setq file-list (cdr file-list)))
    dir-list))

(defun vhdl-aput (alist-symbol key &optional value)
  "As `aput', but delete key-value pair if VALUE is nil."
  (if value
      (aput alist-symbol key value)
    (adelete alist-symbol key)))

(defun vhdl-delete (elt list)
  "Delete by side effect the first occurrence of ELT as a member of LIST."
  (setq list (cons nil list))
  (let ((list1 list))
    (while (and (cdr list1) (not (equal elt (cadr list1))))
      (setq list1 (cdr list1)))
    (when list
      (setcdr list1 (cddr list1))))
  (cdr list))

(defun vhdl-speedbar-refresh (&optional key)
  "Refresh directory or project with name KEY."
  (when (and (boundp 'speedbar-frame)
	     (frame-live-p speedbar-frame))
    (let ((pos (point))
	  (last-frame (selected-frame)))
      (if (null key)
	  (speedbar-refresh)
	(select-frame speedbar-frame)
	(when (save-excursion
		(goto-char (point-min))
		(re-search-forward (concat "^\\([0-9]+:\\s-*<\\)->\\s-+" key "$") nil t))
	  (goto-char (match-end 1))
	  (speedbar-do-function-pointer)
	  (backward-char 2)
	  (speedbar-do-function-pointer)
	  (message "Refreshing speedbar...done"))
	(select-frame last-frame)))))

(defun vhdl-show-messages ()
  "Get *Messages* buffer to show recent messages."
  (interactive)
  (display-buffer (if (featurep 'xemacs) " *Message-Log*" "*Messages*")))

(defun vhdl-use-direct-instantiation ()
  "Return whether direct instantiation is used."
  (or (eq vhdl-use-direct-instantiation 'always)
      (and (eq vhdl-use-direct-instantiation 'standard)
	   (not (vhdl-standard-p '87)))))

(defun vhdl-max-marker (marker1 marker2)
  "Return larger marker."
  (if (> marker1 marker2) marker1 marker2))

(defun vhdl-goto-marker (marker)
  "Goto marker in appropriate buffer."
  (when (markerp marker)
    (set-buffer (marker-buffer marker)))
  (goto-char marker))

(defun vhdl-menu-split (list title)
  "Split menu LIST into several submenus, if number of
elements > `vhdl-menu-max-size'."
  (if (> (length list) vhdl-menu-max-size)
      (let ((remain list)
	    (result '())
	    (sublist '())
	    (menuno 1)
	    (i 0))
	(while remain
	  (setq sublist (cons (car remain) sublist))
	  (setq remain (cdr remain))
	  (setq i (+ i 1))
	  (if (= i vhdl-menu-max-size)
	      (progn
		(setq result (cons (cons (format "%s %s" title menuno)
					 (nreverse sublist)) result))
		(setq i 0)
		(setq menuno (+ menuno 1))
		(setq sublist '()))))
	(and sublist
	     (setq result (cons (cons (format "%s %s" title menuno)
				      (nreverse sublist)) result)))
	(nreverse result))
    list))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key bindings

(defvar vhdl-template-map nil
  "Keymap for VHDL templates.")

(defun vhdl-template-map-init ()
  "Initialize `vhdl-template-map'."
  (setq vhdl-template-map (make-sparse-keymap))
  ;; key bindings for VHDL templates
  (define-key vhdl-template-map "al"	'vhdl-template-alias)
  (define-key vhdl-template-map "ar"	'vhdl-template-architecture)
  (define-key vhdl-template-map "at"	'vhdl-template-assert)
  (define-key vhdl-template-map "ad"	'vhdl-template-attribute-decl)
  (define-key vhdl-template-map "as"	'vhdl-template-attribute-spec)
  (define-key vhdl-template-map "bl"	'vhdl-template-block)
  (define-key vhdl-template-map "ca"	'vhdl-template-case-is)
  (define-key vhdl-template-map "cd"	'vhdl-template-component-decl)
  (define-key vhdl-template-map "ci"	'vhdl-template-component-inst)
  (define-key vhdl-template-map "cs"	'vhdl-template-conditional-signal-asst)
  (define-key vhdl-template-map "Cb"	'vhdl-template-block-configuration)
  (define-key vhdl-template-map "Cc"	'vhdl-template-component-conf)
  (define-key vhdl-template-map "Cd"	'vhdl-template-configuration-decl)
  (define-key vhdl-template-map "Cs"	'vhdl-template-configuration-spec)
  (define-key vhdl-template-map "co"	'vhdl-template-constant)
  (define-key vhdl-template-map "di"	'vhdl-template-disconnect)
  (define-key vhdl-template-map "el"	'vhdl-template-else)
  (define-key vhdl-template-map "ei"	'vhdl-template-elsif)
  (define-key vhdl-template-map "en"	'vhdl-template-entity)
  (define-key vhdl-template-map "ex"	'vhdl-template-exit)
  (define-key vhdl-template-map "fi"	'vhdl-template-file)
  (define-key vhdl-template-map "fg"	'vhdl-template-for-generate)
  (define-key vhdl-template-map "fl"	'vhdl-template-for-loop)
  (define-key vhdl-template-map "\C-f"	'vhdl-template-footer)
  (define-key vhdl-template-map "fb"	'vhdl-template-function-body)
  (define-key vhdl-template-map "fd"	'vhdl-template-function-decl)
  (define-key vhdl-template-map "ge"	'vhdl-template-generic)
  (define-key vhdl-template-map "gd"	'vhdl-template-group-decl)
  (define-key vhdl-template-map "gt"	'vhdl-template-group-template)
  (define-key vhdl-template-map "\C-h"	'vhdl-template-header)
  (define-key vhdl-template-map "ig"	'vhdl-template-if-generate)
  (define-key vhdl-template-map "it"	'vhdl-template-if-then)
  (define-key vhdl-template-map "li"	'vhdl-template-library)
  (define-key vhdl-template-map "lo"	'vhdl-template-bare-loop)
  (define-key vhdl-template-map "\C-m"	'vhdl-template-modify)
  (define-key vhdl-template-map "\C-t"	'vhdl-template-insert-date)
  (define-key vhdl-template-map "ma"	'vhdl-template-map)
  (define-key vhdl-template-map "ne"	'vhdl-template-next)
  (define-key vhdl-template-map "ot"	'vhdl-template-others)
  (define-key vhdl-template-map "Pd"	'vhdl-template-package-decl)
  (define-key vhdl-template-map "Pb"	'vhdl-template-package-body)
  (define-key vhdl-template-map "("     'vhdl-template-paired-parens)
  (define-key vhdl-template-map "po"	'vhdl-template-port)
  (define-key vhdl-template-map "pb"	'vhdl-template-procedure-body)
  (define-key vhdl-template-map "pd"	'vhdl-template-procedure-decl)
  (define-key vhdl-template-map "pc"	'vhdl-template-process-comb)
  (define-key vhdl-template-map "ps"	'vhdl-template-process-seq)
  (define-key vhdl-template-map "rp"	'vhdl-template-report)
  (define-key vhdl-template-map "rt"	'vhdl-template-return)
  (define-key vhdl-template-map "ss"	'vhdl-template-selected-signal-asst)
  (define-key vhdl-template-map "si"	'vhdl-template-signal)
  (define-key vhdl-template-map "su"	'vhdl-template-subtype)
  (define-key vhdl-template-map "ty"	'vhdl-template-type)
  (define-key vhdl-template-map "us"	'vhdl-template-use)
  (define-key vhdl-template-map "va"	'vhdl-template-variable)
  (define-key vhdl-template-map "wa"	'vhdl-template-wait)
  (define-key vhdl-template-map "wl"	'vhdl-template-while-loop)
  (define-key vhdl-template-map "wi"	'vhdl-template-with)
  (define-key vhdl-template-map "wc"	'vhdl-template-clocked-wait)
  (define-key vhdl-template-map "\C-pb" 'vhdl-template-package-numeric-bit)
  (define-key vhdl-template-map "\C-pn" 'vhdl-template-package-numeric-std)
  (define-key vhdl-template-map "\C-ps" 'vhdl-template-package-std-logic-1164)
  (define-key vhdl-template-map "\C-pA" 'vhdl-template-package-std-logic-arith)
  (define-key vhdl-template-map "\C-pM" 'vhdl-template-package-std-logic-misc)
  (define-key vhdl-template-map "\C-pS" 'vhdl-template-package-std-logic-signed)
  (define-key vhdl-template-map "\C-pT" 'vhdl-template-package-std-logic-textio)
  (define-key vhdl-template-map "\C-pU" 'vhdl-template-package-std-logic-unsigned)
  (define-key vhdl-template-map "\C-pt" 'vhdl-template-package-textio)
  (define-key vhdl-template-map "\C-dn" 'vhdl-template-directive-translate-on)
  (define-key vhdl-template-map "\C-df" 'vhdl-template-directive-translate-off)
  (define-key vhdl-template-map "\C-dN" 'vhdl-template-directive-synthesis-on)
  (define-key vhdl-template-map "\C-dF" 'vhdl-template-directive-synthesis-off)
  (define-key vhdl-template-map "\C-q"  'vhdl-template-search-prompt)
  (when (vhdl-standard-p 'ams)
    (define-key vhdl-template-map "br"	'vhdl-template-break)
    (define-key vhdl-template-map "cu"	'vhdl-template-case-use)
    (define-key vhdl-template-map "iu"	'vhdl-template-if-use)
    (define-key vhdl-template-map "lm"	'vhdl-template-limit)
    (define-key vhdl-template-map "na"	'vhdl-template-nature)
    (define-key vhdl-template-map "pa"	'vhdl-template-procedural)
    (define-key vhdl-template-map "qf"	'vhdl-template-quantity-free)
    (define-key vhdl-template-map "qb"	'vhdl-template-quantity-branch)
    (define-key vhdl-template-map "qs"	'vhdl-template-quantity-source)
    (define-key vhdl-template-map "sn"	'vhdl-template-subnature)
    (define-key vhdl-template-map "te"	'vhdl-template-terminal)
    )
  (when (vhdl-standard-p 'math)
    (define-key vhdl-template-map "\C-pc" 'vhdl-template-package-math-complex)
    (define-key vhdl-template-map "\C-pr" 'vhdl-template-package-math-real)
    ))

;; initialize template map for VHDL Mode
(vhdl-template-map-init)

(defun vhdl-function-name (prefix string &optional postfix)
  "Generate a Lisp function name.
PREFIX, STRING and optional POSTFIX are concatenated by '-' and spaces in
STRING are replaced by `-' and substrings are converted to lower case."
  (let ((name prefix))
    (while (string-match "\\(\\w+\\)\\s-*\\(.*\\)" string)
      (setq name
	    (concat name "-" (downcase (substring string 0 (match-end 1)))))
      (setq string (substring string (match-beginning 2))))
    (when postfix (setq name (concat name "-" postfix)))
    (intern name)))

(defvar vhdl-model-map nil
  "Keymap for VHDL models.")

(defun vhdl-model-map-init ()
  "Initialize `vhdl-model-map'."
  (setq vhdl-model-map (make-sparse-keymap))
  ;; key bindings for VHDL models
  (let ((model-alist vhdl-model-alist) model)
    (while model-alist
      (setq model (car model-alist))
      (define-key vhdl-model-map (nth 2 model)
	(vhdl-function-name "vhdl-model" (nth 0 model)))
      (setq model-alist (cdr model-alist)))))

;; initialize user model map for VHDL Mode
(vhdl-model-map-init)

(defvar vhdl-mode-map nil
  "Keymap for VHDL Mode.")

(defun vhdl-mode-map-init ()
  "Initialize `vhdl-mode-map'."
  (setq vhdl-mode-map (make-sparse-keymap))
  ;; template key bindings
  (define-key vhdl-mode-map "\C-c\C-t"	   vhdl-template-map)
  ;; model key bindings
  (define-key vhdl-mode-map "\C-c\C-m"	   vhdl-model-map)
  ;; standard key bindings
  (define-key vhdl-mode-map "\M-a"	   'vhdl-beginning-of-statement)
  (define-key vhdl-mode-map "\M-e"	   'vhdl-end-of-statement)
  (define-key vhdl-mode-map "\M-\C-f"	   'vhdl-forward-sexp)
  (define-key vhdl-mode-map "\M-\C-b"	   'vhdl-backward-sexp)
  (define-key vhdl-mode-map "\M-\C-u"	   'vhdl-backward-up-list)
  (define-key vhdl-mode-map "\M-\C-a"	   'vhdl-backward-same-indent)
  (define-key vhdl-mode-map "\M-\C-e"	   'vhdl-forward-same-indent)
  (unless (featurep 'xemacs) ; would override `M-backspace' in XEmacs
    (define-key vhdl-mode-map "\M-\C-h"	   'vhdl-mark-defun))
  (define-key vhdl-mode-map "\M-\C-q"	   'vhdl-indent-sexp)
  (define-key vhdl-mode-map "\M-^"	   'vhdl-delete-indentation)
  ;; backspace/delete key bindings
  (define-key vhdl-mode-map [backspace]	   'backward-delete-char-untabify)
  (unless (boundp 'delete-key-deletes-forward) ; XEmacs variable
    (define-key vhdl-mode-map [delete]	     'delete-char)
    (define-key vhdl-mode-map [(meta delete)] 'kill-word))
  ;; mode specific key bindings
  (define-key vhdl-mode-map "\C-c\C-m\C-e" 'vhdl-electric-mode)
  (define-key vhdl-mode-map "\C-c\C-m\C-s" 'vhdl-stutter-mode)
  (define-key vhdl-mode-map "\C-c\C-s\C-p" 'vhdl-set-project)
  (define-key vhdl-mode-map "\C-c\C-p\C-d" 'vhdl-duplicate-project)
  (define-key vhdl-mode-map "\C-c\C-p\C-m" 'vhdl-import-project)
  (define-key vhdl-mode-map "\C-c\C-p\C-x" 'vhdl-export-project)
  (define-key vhdl-mode-map "\C-c\C-s\C-k" 'vhdl-set-compiler)
  (define-key vhdl-mode-map "\C-c\C-k"	   'vhdl-compile)
  (define-key vhdl-mode-map "\C-c\M-\C-k"  'vhdl-make)
  (define-key vhdl-mode-map "\C-c\M-k"	   'vhdl-generate-makefile)
  (define-key vhdl-mode-map "\C-c\C-p\C-w" 'vhdl-port-copy)
  (define-key vhdl-mode-map "\C-c\C-p\M-w" 'vhdl-port-copy)
  (define-key vhdl-mode-map "\C-c\C-p\C-e" 'vhdl-port-paste-entity)
  (define-key vhdl-mode-map "\C-c\C-p\C-c" 'vhdl-port-paste-component)
  (define-key vhdl-mode-map "\C-c\C-p\C-i" 'vhdl-port-paste-instance)
  (define-key vhdl-mode-map "\C-c\C-p\C-s" 'vhdl-port-paste-signals)
  (define-key vhdl-mode-map "\C-c\C-p\M-c" 'vhdl-port-paste-constants)
  (if (featurep 'xemacs) ; `... C-g' not allowed in XEmacs
      (define-key vhdl-mode-map "\C-c\C-p\M-g" 'vhdl-port-paste-generic-map)
    (define-key vhdl-mode-map "\C-c\C-p\C-g" 'vhdl-port-paste-generic-map))
  (define-key vhdl-mode-map "\C-c\C-p\C-z" 'vhdl-port-paste-initializations)
  (define-key vhdl-mode-map "\C-c\C-p\C-t" 'vhdl-port-paste-testbench)
  (define-key vhdl-mode-map "\C-c\C-p\C-f" 'vhdl-port-flatten)
  (define-key vhdl-mode-map "\C-c\C-p\C-r" 'vhdl-port-reverse-direction)
  (define-key vhdl-mode-map "\C-c\C-s\C-w" 'vhdl-subprog-copy)
  (define-key vhdl-mode-map "\C-c\C-s\M-w" 'vhdl-subprog-copy)
  (define-key vhdl-mode-map "\C-c\C-s\C-d" 'vhdl-subprog-paste-declaration)
  (define-key vhdl-mode-map "\C-c\C-s\C-b" 'vhdl-subprog-paste-body)
  (define-key vhdl-mode-map "\C-c\C-s\C-c" 'vhdl-subprog-paste-call)
  (define-key vhdl-mode-map "\C-c\C-s\C-f" 'vhdl-subprog-flatten)
  (define-key vhdl-mode-map "\C-c\C-m\C-n" 'vhdl-compose-new-component)
  (define-key vhdl-mode-map "\C-c\C-m\C-p" 'vhdl-compose-place-component)
  (define-key vhdl-mode-map "\C-c\C-m\C-w" 'vhdl-compose-wire-components)
  (define-key vhdl-mode-map "\C-c\C-m\C-f" 'vhdl-compose-configuration)
  (define-key vhdl-mode-map "\C-c\C-m\C-k" 'vhdl-compose-components-package)
  (define-key vhdl-mode-map "\C-c\C-c"	   'vhdl-comment-uncomment-region)
  (define-key vhdl-mode-map "\C-c-"	   'vhdl-comment-append-inline)
  (define-key vhdl-mode-map "\C-c\M--"	   'vhdl-comment-display-line)
  (define-key vhdl-mode-map "\C-c\C-i\C-l" 'indent-according-to-mode)
  (define-key vhdl-mode-map "\C-c\C-i\C-g" 'vhdl-indent-group)
  (define-key vhdl-mode-map "\M-\C-\\"	   'vhdl-indent-region)
  (define-key vhdl-mode-map "\C-c\C-i\C-b" 'vhdl-indent-buffer)
  (define-key vhdl-mode-map "\C-c\C-a\C-g" 'vhdl-align-group)
  (define-key vhdl-mode-map "\C-c\C-a\C-a" 'vhdl-align-group)
  (define-key vhdl-mode-map "\C-c\C-a\C-i" 'vhdl-align-same-indent)
  (define-key vhdl-mode-map "\C-c\C-a\C-l" 'vhdl-align-list)
  (define-key vhdl-mode-map "\C-c\C-a\C-d" 'vhdl-align-declarations)
  (define-key vhdl-mode-map "\C-c\C-a\M-a" 'vhdl-align-region)
  (define-key vhdl-mode-map "\C-c\C-a\C-b" 'vhdl-align-buffer)
  (define-key vhdl-mode-map "\C-c\C-a\C-c" 'vhdl-align-inline-comment-group)
  (define-key vhdl-mode-map "\C-c\C-a\M-c" 'vhdl-align-inline-comment-region)
  (define-key vhdl-mode-map "\C-c\C-f\C-l" 'vhdl-fill-list)
  (define-key vhdl-mode-map "\C-c\C-f\C-f" 'vhdl-fill-list)
  (define-key vhdl-mode-map "\C-c\C-f\C-g" 'vhdl-fill-group)
  (define-key vhdl-mode-map "\C-c\C-f\C-i" 'vhdl-fill-same-indent)
  (define-key vhdl-mode-map "\C-c\C-f\M-f" 'vhdl-fill-region)
  (define-key vhdl-mode-map "\C-c\C-l\C-w" 'vhdl-line-kill)
  (define-key vhdl-mode-map "\C-c\C-l\M-w" 'vhdl-line-copy)
  (define-key vhdl-mode-map "\C-c\C-l\C-y" 'vhdl-line-yank)
  (define-key vhdl-mode-map "\C-c\C-l\t"   'vhdl-line-expand)
  (define-key vhdl-mode-map "\C-c\C-l\C-n" 'vhdl-line-transpose-next)
  (define-key vhdl-mode-map "\C-c\C-l\C-p" 'vhdl-line-transpose-previous)
  (define-key vhdl-mode-map "\C-c\C-l\C-o" 'vhdl-line-open)
  (define-key vhdl-mode-map "\C-c\C-l\C-g" 'goto-line)
  (define-key vhdl-mode-map "\C-c\C-l\C-c" 'vhdl-comment-uncomment-line)
  (define-key vhdl-mode-map "\C-c\C-x\C-p" 'vhdl-fix-clause)
  (define-key vhdl-mode-map "\C-c\C-x\M-c" 'vhdl-fix-case-region)
  (define-key vhdl-mode-map "\C-c\C-x\C-c" 'vhdl-fix-case-buffer)
  (define-key vhdl-mode-map "\C-c\C-x\M-w" 'vhdl-fixup-whitespace-region)
  (define-key vhdl-mode-map "\C-c\C-x\C-w" 'vhdl-fixup-whitespace-buffer)
  (define-key vhdl-mode-map "\C-c\M-b"	   'vhdl-beautify-region)
  (define-key vhdl-mode-map "\C-c\C-b"	   'vhdl-beautify-buffer)
  (define-key vhdl-mode-map "\C-c\C-u\C-s" 'vhdl-update-sensitivity-list-process)
  (define-key vhdl-mode-map "\C-c\C-u\M-s" 'vhdl-update-sensitivity-list-buffer)
  (define-key vhdl-mode-map "\C-c\C-i\C-f" 'vhdl-fontify-buffer)
  (define-key vhdl-mode-map "\C-c\C-i\C-s" 'vhdl-statistics-buffer)
  (define-key vhdl-mode-map "\C-c\M-m"	   'vhdl-show-messages)
  (define-key vhdl-mode-map "\C-c\C-h"	   'vhdl-doc-mode)
  (define-key vhdl-mode-map "\C-c\C-v"	   'vhdl-version)
  (define-key vhdl-mode-map "\M-\t"	   'insert-tab)
  ;; insert commands bindings
  (define-key vhdl-mode-map "\C-c\C-i\C-t" 'vhdl-template-insert-construct)
  (define-key vhdl-mode-map "\C-c\C-i\C-p" 'vhdl-template-insert-package)
  (define-key vhdl-mode-map "\C-c\C-i\C-d" 'vhdl-template-insert-directive)
  (define-key vhdl-mode-map "\C-c\C-i\C-m" 'vhdl-model-insert)
  ;; electric key bindings
  (define-key vhdl-mode-map " "		   'vhdl-electric-space)
  (when vhdl-intelligent-tab
    (define-key vhdl-mode-map "\t"	   'vhdl-electric-tab))
  (define-key vhdl-mode-map "\r"	   'vhdl-electric-return)
  (define-key vhdl-mode-map "-"		   'vhdl-electric-dash)
  (define-key vhdl-mode-map "["		   'vhdl-electric-open-bracket)
  (define-key vhdl-mode-map "]"		   'vhdl-electric-close-bracket)
  (define-key vhdl-mode-map "'"		   'vhdl-electric-quote)
  (define-key vhdl-mode-map ";"		   'vhdl-electric-semicolon)
  (define-key vhdl-mode-map ","		   'vhdl-electric-comma)
  (define-key vhdl-mode-map "."		   'vhdl-electric-period)
  (when (vhdl-standard-p 'ams)
    (define-key vhdl-mode-map "="	   'vhdl-electric-equal)))

;; initialize mode map for VHDL Mode
(vhdl-mode-map-init)

;; define special minibuffer keymap for enabling word completion in minibuffer
;; (useful in template generator prompts)
(defvar vhdl-minibuffer-local-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (when vhdl-word-completion-in-minibuffer
      (define-key map "\t" 'vhdl-minibuffer-tab))
    map)
  "Keymap for minibuffer used in VHDL Mode.")

;; set up electric character functions to work with
;; `delete-selection-mode' (Emacs) and `pending-delete-mode' (XEmacs)
(mapc
 (function
  (lambda (sym)
    (put sym 'delete-selection t)	; for `delete-selection-mode' (Emacs)
    (put sym 'pending-delete t)))	; for `pending-delete-mode' (XEmacs)
 '(vhdl-electric-space
   vhdl-electric-tab
   vhdl-electric-return
   vhdl-electric-dash
   vhdl-electric-open-bracket
   vhdl-electric-close-bracket
   vhdl-electric-quote
   vhdl-electric-semicolon
   vhdl-electric-comma
   vhdl-electric-period
   vhdl-electric-equal))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax table

(defvar vhdl-mode-syntax-table nil
  "Syntax table used in `vhdl-mode' buffers.")

(defvar vhdl-mode-ext-syntax-table nil
  "Syntax table extended by `_' used in `vhdl-mode' buffers.")

(defun vhdl-mode-syntax-table-init ()
  "Initialize `vhdl-mode-syntax-table'."
  (setq vhdl-mode-syntax-table (make-syntax-table))
  ;; define punctuation
  (modify-syntax-entry ?\# "."    vhdl-mode-syntax-table)
  (modify-syntax-entry ?\$ "."    vhdl-mode-syntax-table)
  (modify-syntax-entry ?\% "."    vhdl-mode-syntax-table)
  (modify-syntax-entry ?\& "."    vhdl-mode-syntax-table)
  (modify-syntax-entry ?\' "."    vhdl-mode-syntax-table)
  (modify-syntax-entry ?\* "."    vhdl-mode-syntax-table)
  (modify-syntax-entry ?\+ "."    vhdl-mode-syntax-table)
  (modify-syntax-entry ?\. "."    vhdl-mode-syntax-table)
  (modify-syntax-entry ?\/ "."    vhdl-mode-syntax-table)
  (modify-syntax-entry ?\: "."    vhdl-mode-syntax-table)
  (modify-syntax-entry ?\; "."    vhdl-mode-syntax-table)
  (modify-syntax-entry ?\< "."    vhdl-mode-syntax-table)
  (modify-syntax-entry ?\= "."    vhdl-mode-syntax-table)
  (modify-syntax-entry ?\> "."    vhdl-mode-syntax-table)
  (modify-syntax-entry ?\\ "."    vhdl-mode-syntax-table)
  (modify-syntax-entry ?\| "."    vhdl-mode-syntax-table)
  ;; define string
  (modify-syntax-entry ?\" "\""   vhdl-mode-syntax-table)
  ;; define underscore
  (when vhdl-underscore-is-part-of-word
    (modify-syntax-entry ?\_ "w"   vhdl-mode-syntax-table))
  ;; a single hyphen is punctuation, but a double hyphen starts a comment
  (modify-syntax-entry ?\- ". 12" vhdl-mode-syntax-table)
  ;; and \n and \^M end a comment
  (modify-syntax-entry ?\n ">"    vhdl-mode-syntax-table)
  (modify-syntax-entry ?\^M ">"   vhdl-mode-syntax-table)
  ;; define parentheses to match
  (modify-syntax-entry ?\( "()"   vhdl-mode-syntax-table)
  (modify-syntax-entry ?\) ")("   vhdl-mode-syntax-table)
  (modify-syntax-entry ?\[ "(]"   vhdl-mode-syntax-table)
  (modify-syntax-entry ?\] ")["   vhdl-mode-syntax-table)
  (modify-syntax-entry ?\{ "(}"   vhdl-mode-syntax-table)
  (modify-syntax-entry ?\} "){"   vhdl-mode-syntax-table)
  ;; extended syntax table including '_' (for simpler search regexps)
  (setq vhdl-mode-ext-syntax-table (copy-syntax-table vhdl-mode-syntax-table))
  (modify-syntax-entry ?_ "w" vhdl-mode-ext-syntax-table))

;; initialize syntax table for VHDL Mode
(vhdl-mode-syntax-table-init)

(defvar vhdl-syntactic-context nil
  "Buffer local variable containing syntactic analysis list.")
(make-variable-buffer-local 'vhdl-syntactic-context)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Abbrev ook bindings

(defvar vhdl-mode-abbrev-table nil
  "Abbrev table to use in `vhdl-mode' buffers.")

(defun vhdl-mode-abbrev-table-init ()
  "Initialize `vhdl-mode-abbrev-table'."
  (define-abbrev-table 'vhdl-mode-abbrev-table
    (append
     (when (memq 'vhdl vhdl-electric-keywords)
       ;; VHDL'93 keywords
       (mapcar (lambda (x) (list (car x) "" (cdr x) 0 'system))
               '(
                 ("--"		  . vhdl-template-display-comment-hook)
                 ("abs"		  . vhdl-template-default-hook)
                 ("access"	  . vhdl-template-default-hook)
                 ("after"	  . vhdl-template-default-hook)
                 ("alias"	  . vhdl-template-alias-hook)
                 ("all"		  . vhdl-template-default-hook)
                 ("and"		  . vhdl-template-default-hook)
                 ("arch"	  . vhdl-template-architecture-hook)
                 ("architecture"  . vhdl-template-architecture-hook)
                 ("array"	  . vhdl-template-default-hook)
                 ("assert"	  . vhdl-template-assert-hook)
                 ("attr"	  . vhdl-template-attribute-hook)
                 ("attribute"	  . vhdl-template-attribute-hook)
                 ("begin"	  . vhdl-template-default-indent-hook)
                 ("block"	  . vhdl-template-block-hook)
                 ("body"	  . vhdl-template-default-hook)
                 ("buffer"	  . vhdl-template-default-hook)
                 ("bus"		  . vhdl-template-default-hook)
                 ("case"	  . vhdl-template-case-hook)
                 ("comp"	  . vhdl-template-component-hook)
                 ("component"	  . vhdl-template-component-hook)
                 ("cond"	  . vhdl-template-conditional-signal-asst-hook)
                 ("conditional"	  . vhdl-template-conditional-signal-asst-hook)
                 ("conf"	  . vhdl-template-configuration-hook)
                 ("configuration" . vhdl-template-configuration-hook)
                 ("cons"	  . vhdl-template-constant-hook)
                 ("constant"	  . vhdl-template-constant-hook)
                 ("disconnect"	  . vhdl-template-disconnect-hook)
                 ("downto"	  . vhdl-template-default-hook)
                 ("else"	  . vhdl-template-else-hook)
                 ("elseif"	  . vhdl-template-elsif-hook)
                 ("elsif"	  . vhdl-template-elsif-hook)
                 ("end"		  . vhdl-template-default-indent-hook)
                 ("entity"	  . vhdl-template-entity-hook)
                 ("exit"	  . vhdl-template-exit-hook)
                 ("file"	  . vhdl-template-file-hook)
                 ("for"		  . vhdl-template-for-hook)
                 ("func"	  . vhdl-template-function-hook)
                 ("function"	  . vhdl-template-function-hook)
                 ("generic"	  . vhdl-template-generic-hook)
                 ("group"	  . vhdl-template-group-hook)
                 ("guarded"	  . vhdl-template-default-hook)
                 ("if"		  . vhdl-template-if-hook)
                 ("impure"	  . vhdl-template-default-hook)
                 ("in"		  . vhdl-template-default-hook)
                 ("inertial"	  . vhdl-template-default-hook)
                 ("inout"	  . vhdl-template-default-hook)
                 ("inst"	  . vhdl-template-instance-hook)
                 ("instance"	  . vhdl-template-instance-hook)
                 ("is"		  . vhdl-template-default-hook)
                 ("label"	  . vhdl-template-default-hook)
                 ("library"	  . vhdl-template-library-hook)
                 ("linkage"	  . vhdl-template-default-hook)
                 ("literal"	  . vhdl-template-default-hook)
                 ("loop"	  . vhdl-template-bare-loop-hook)
                 ("map"		  . vhdl-template-map-hook)
                 ("mod"		  . vhdl-template-default-hook)
                 ("nand"	  . vhdl-template-default-hook)
                 ("new"		  . vhdl-template-default-hook)
                 ("next"	  . vhdl-template-next-hook)
                 ("nor"		  . vhdl-template-default-hook)
                 ("not"		  . vhdl-template-default-hook)
                 ("null"	  . vhdl-template-default-hook)
                 ("of"		  . vhdl-template-default-hook)
                 ("on"		  . vhdl-template-default-hook)
                 ("open"	  . vhdl-template-default-hook)
                 ("or"		  . vhdl-template-default-hook)
                 ("others"	  . vhdl-template-others-hook)
                 ("out"		  . vhdl-template-default-hook)
                 ("pack"	  . vhdl-template-package-hook)
                 ("package"	  . vhdl-template-package-hook)
                 ("port"	  . vhdl-template-port-hook)
                 ("postponed"	  . vhdl-template-default-hook)
                 ("procedure"	  . vhdl-template-procedure-hook)
                 ("process"	  . vhdl-template-process-hook)
                 ("pure"	  . vhdl-template-default-hook)
                 ("range"	  . vhdl-template-default-hook)
                 ("record"	  . vhdl-template-default-hook)
                 ("register"	  . vhdl-template-default-hook)
                 ("reject"	  . vhdl-template-default-hook)
                 ("rem"		  . vhdl-template-default-hook)
                 ("report"	  . vhdl-template-report-hook)
                 ("return"	  . vhdl-template-return-hook)
                 ("rol"		  . vhdl-template-default-hook)
                 ("ror"		  . vhdl-template-default-hook)
                 ("select"	  . vhdl-template-selected-signal-asst-hook)
                 ("severity"	  . vhdl-template-default-hook)
                 ("shared"	  . vhdl-template-default-hook)
                 ("sig"		  . vhdl-template-signal-hook)
                 ("signal"	  . vhdl-template-signal-hook)
                 ("sla"		  . vhdl-template-default-hook)
                 ("sll"		  . vhdl-template-default-hook)
                 ("sra"		  . vhdl-template-default-hook)
                 ("srl"		  . vhdl-template-default-hook)
                 ("subtype"	  . vhdl-template-subtype-hook)
                 ("then"	  . vhdl-template-default-hook)
                 ("to"		  . vhdl-template-default-hook)
                 ("transport"	  . vhdl-template-default-hook)
                 ("type"	  . vhdl-template-type-hook)
                 ("unaffected"	  . vhdl-template-default-hook)
                 ("units"	  . vhdl-template-default-hook)
                 ("until"	  . vhdl-template-default-hook)
                 ("use"		  . vhdl-template-use-hook)
                 ("var"		  . vhdl-template-variable-hook)
                 ("variable"	  . vhdl-template-variable-hook)
                 ("wait"	  . vhdl-template-wait-hook)
                 ("when"	  . vhdl-template-when-hook)
                 ("while"	  . vhdl-template-while-loop-hook)
                 ("with"	  . vhdl-template-with-hook)
                 ("xnor"	  . vhdl-template-default-hook)
                 ("xor"		  . vhdl-template-default-hook)
                 )))
     ;; VHDL-AMS keywords
     (when (and (memq 'vhdl vhdl-electric-keywords) (vhdl-standard-p 'ams))
       (mapcar (lambda (x) (list (car x) "" (cdr x) 0 'system))
               '(
                 ("across"     . vhdl-template-default-hook)
                 ("break"      . vhdl-template-break-hook)
                 ("limit"      . vhdl-template-limit-hook)
                 ("nature"     . vhdl-template-nature-hook)
                 ("noise"      . vhdl-template-default-hook)
                 ("procedural" . vhdl-template-procedural-hook)
                 ("quantity"   . vhdl-template-quantity-hook)
                 ("reference"  . vhdl-template-default-hook)
                 ("spectrum"   . vhdl-template-default-hook)
                 ("subnature"  . vhdl-template-subnature-hook)
                 ("terminal"   . vhdl-template-terminal-hook)
                 ("through"    . vhdl-template-default-hook)
                 ("tolerance"  . vhdl-template-default-hook)
                 )))
     ;; user model keywords
     (when (memq 'user vhdl-electric-keywords)
       (let (abbrev-list keyword)
         (dolist (elem vhdl-model-alist)
	   (setq keyword (nth 3 elem))
	   (unless (equal keyword "")
             (push (list keyword ""
                         (vhdl-function-name
                          "vhdl-model" (nth 0 elem) "hook") 0 'system)
                   abbrev-list)))
	 abbrev-list)))))

;; initialize abbrev table for VHDL Mode
(vhdl-mode-abbrev-table-init)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Template completion lists

(defvar vhdl-template-construct-alist nil
  "List of built-in construct templates.")

(defun vhdl-template-construct-alist-init ()
  "Initialize `vhdl-template-construct-alist'."
  (setq
   vhdl-template-construct-alist
   (append
    '(
      ("alias declaration"		   vhdl-template-alias)
      ("architecture body"		   vhdl-template-architecture)
      ("assertion"			   vhdl-template-assert)
      ("attribute declaration"		   vhdl-template-attribute-decl)
      ("attribute specification"	   vhdl-template-attribute-spec)
      ("block configuration"		   vhdl-template-block-configuration)
      ("block statement"		   vhdl-template-block)
      ("case statement"			   vhdl-template-case-is)
      ("component configuration"	   vhdl-template-component-conf)
      ("component declaration"		   vhdl-template-component-decl)
      ("component instantiation statement" vhdl-template-component-inst)
      ("conditional signal assignment"	   vhdl-template-conditional-signal-asst)
      ("configuration declaration"	   vhdl-template-configuration-decl)
      ("configuration specification" 	   vhdl-template-configuration-spec)
      ("constant declaration"		   vhdl-template-constant)
      ("disconnection specification"	   vhdl-template-disconnect)
      ("entity declaration"		   vhdl-template-entity)
      ("exit statement"			   vhdl-template-exit)
      ("file declaration"		   vhdl-template-file)
      ("generate statement"		   vhdl-template-generate)
      ("generic clause"			   vhdl-template-generic)
      ("group declaration"		   vhdl-template-group-decl)
      ("group template declaration" 	   vhdl-template-group-template)
      ("if statement"			   vhdl-template-if-then)
      ("library clause"			   vhdl-template-library)
      ("loop statement"			   vhdl-template-loop)
      ("next statement"			   vhdl-template-next)
      ("package declaration"		   vhdl-template-package-decl)
      ("package body"			   vhdl-template-package-body)
      ("port clause"			   vhdl-template-port)
      ("process statement"	 	   vhdl-template-process)
      ("report statement"		   vhdl-template-report)
      ("return statement"		   vhdl-template-return)
      ("selected signal assignment"	   vhdl-template-selected-signal-asst)
      ("signal declaration"		   vhdl-template-signal)
      ("subprogram declaration"		   vhdl-template-subprogram-decl)
      ("subprogram body"		   vhdl-template-subprogram-body)
      ("subtype declaration"		   vhdl-template-subtype)
      ("type declaration"		   vhdl-template-type)
      ("use clause"			   vhdl-template-use)
      ("variable declaration"	 	   vhdl-template-variable)
      ("wait statement"			   vhdl-template-wait)
      )
    (when (vhdl-standard-p 'ams)
      '(
	("break statement"		     vhdl-template-break)
	("nature declaration"		     vhdl-template-nature)
	("quantity declaration"		     vhdl-template-quantity)
	("simultaneous case statement"	     vhdl-template-case-use)
	("simultaneous if statement"	     vhdl-template-if-use)
	("simultaneous procedural statement" vhdl-template-procedural)
	("step limit specification"	     vhdl-template-limit)
	("subnature declaration"	     vhdl-template-subnature)
	("terminal declaration"		     vhdl-template-terminal)
	)))))

;; initialize for VHDL Mode
(vhdl-template-construct-alist-init)

(defvar vhdl-template-package-alist nil
  "List of built-in package templates.")

(defun vhdl-template-package-alist-init ()
  "Initialize `vhdl-template-package-alist'."
  (setq
   vhdl-template-package-alist
   (append
    '(
      ("numeric_bit"	    vhdl-template-package-numeric-bit)
      ("numeric_std"	    vhdl-template-package-numeric-std)
      ("std_logic_1164"	    vhdl-template-package-std-logic-1164)
      ("std_logic_arith"    vhdl-template-package-std-logic-arith)
      ("std_logic_misc"     vhdl-template-package-std-logic-misc)
      ("std_logic_signed"   vhdl-template-package-std-logic-signed)
      ("std_logic_textio"   vhdl-template-package-std-logic-textio)
      ("std_logic_unsigned" vhdl-template-package-std-logic-unsigned)
      ("textio"		    vhdl-template-package-textio)
      )
    (when (vhdl-standard-p 'math)
      '(
	("math_complex"	vhdl-template-package-math-complex)
	("math_real"	vhdl-template-package-math-real)
	)))))

;; initialize for VHDL Mode
(vhdl-template-package-alist-init)

(defvar vhdl-template-directive-alist
  '(
    ("translate_on"	vhdl-template-directive-translate-on)
    ("translate_off"	vhdl-template-directive-translate-off)
    ("synthesis_on"	vhdl-template-directive-synthesis-on)
    ("synthesis_off"	vhdl-template-directive-synthesis-off)
    )
  "List of built-in directive templates.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Menus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VHDL menu (using `easy-menu.el')

(defun vhdl-customize ()
  "Call the customize function with `vhdl' as argument."
  (interactive)
  (customize-browse 'vhdl))

(defun vhdl-create-mode-menu ()
  "Create VHDL Mode menu."
  `("VHDL"
    ,(append
      '("Project"
	["None"			(vhdl-set-project "")
				:style radio :selected (null vhdl-project)]
	"--")
      ;; add menu entries for defined projects
      (let ((project-alist vhdl-project-alist) menu-list name)
	(while project-alist
	  (setq name (caar project-alist))
	  (setq menu-list
		(cons `[,name (vhdl-set-project ,name)
			:style radio :selected (equal ,name vhdl-project)]
		      menu-list))
	  (setq project-alist (cdr project-alist)))
	(setq menu-list
	      (if vhdl-project-sort
		  (sort menu-list
			(function (lambda (a b) (string< (elt a 0) (elt b 0)))))
		(nreverse menu-list)))
	(vhdl-menu-split menu-list "Project"))
      '("--" "--"
	["Select Project..."	vhdl-set-project t]
	["Set As Default Project" vhdl-set-default-project t]
	"--"
	["Duplicate Project"	vhdl-duplicate-project vhdl-project]
	["Import Project..."	vhdl-import-project
				:keys "C-c C-p C-m" :active t]
	["Export Project"	vhdl-export-project vhdl-project]
	"--"
	["Customize Project..." (customize-option 'vhdl-project-alist) t]))
    "--"
    ("Compile"
     ["Compile Buffer"		vhdl-compile t]
     ["Stop Compilation"	kill-compilation t]
     "--"
     ["Make"			vhdl-make t]
     ["Generate Makefile"	vhdl-generate-makefile t]
     "--"
     ["Next Error"		next-error t]
     ["Previous Error"		previous-error t]
     ["First Error"		first-error t]
     "--"
     ,(append
       '("Compiler")
       ;; add menu entries for defined compilers
       (let ((comp-alist vhdl-compiler-alist) menu-list name)
	 (while comp-alist
	   (setq name (caar comp-alist))
	   (setq menu-list
		 (cons `[,name (setq vhdl-compiler ,name)
			 :style radio :selected (equal ,name vhdl-compiler)]
		       menu-list))
	   (setq comp-alist (cdr comp-alist)))
	 (setq menu-list (nreverse menu-list))
	 (vhdl-menu-split menu-list "Compiler"))
       '("--" "--"
	 ["Select Compiler..."	vhdl-set-compiler t]
	 "--"
	 ["Customize Compiler..."
	  (customize-option 'vhdl-compiler-alist) t])))
    "--"
    ,(append
      '("Template"
	("VHDL Construct 1"
	 ["Alias"		vhdl-template-alias t]
	 ["Architecture"	vhdl-template-architecture t]
	 ["Assert"		vhdl-template-assert t]
	 ["Attribute (Decl)"	vhdl-template-attribute-decl t]
	 ["Attribute (Spec)"	vhdl-template-attribute-spec t]
	 ["Block"		vhdl-template-block t]
	 ["Case"		vhdl-template-case-is t]
	 ["Component (Decl)"	vhdl-template-component-decl t]
	 ["(Component) Instance"	vhdl-template-component-inst t]
	 ["Conditional (Signal Asst)"	vhdl-template-conditional-signal-asst t]
	 ["Configuration (Block)"	vhdl-template-block-configuration t]
	 ["Configuration (Comp)"	vhdl-template-component-conf t]
	 ["Configuration (Decl)"	vhdl-template-configuration-decl t]
	 ["Configuration (Spec)"	vhdl-template-configuration-spec t]
	 ["Constant"		vhdl-template-constant t]
	 ["Disconnect"		vhdl-template-disconnect t]
	 ["Else"		vhdl-template-else t]
	 ["Elsif"		vhdl-template-elsif t]
	 ["Entity"		vhdl-template-entity t]
	 ["Exit"		vhdl-template-exit t]
	 ["File"		vhdl-template-file t]
	 ["For (Generate)"	vhdl-template-for-generate t]
	 ["For (Loop)"		vhdl-template-for-loop t]
	 ["Function (Body)"	vhdl-template-function-body t]
	 ["Function (Decl)"	vhdl-template-function-decl t]
	 ["Generic"		vhdl-template-generic t]
	 ["Group (Decl)"	vhdl-template-group-decl t]
	 ["Group (Template)"	vhdl-template-group-template t])
	("VHDL Construct 2"
	 ["If (Generate)"	vhdl-template-if-generate t]
	 ["If (Then)"		vhdl-template-if-then t]
	 ["Library"		vhdl-template-library t]
	 ["Loop"		vhdl-template-bare-loop t]
	 ["Map"			vhdl-template-map t]
	 ["Next"		vhdl-template-next t]
	 ["Others (Aggregate)"	vhdl-template-others t]
	 ["Package (Decl)"	vhdl-template-package-decl t]
	 ["Package (Body)"	vhdl-template-package-body t]
	 ["Port"		vhdl-template-port t]
	 ["Procedure (Body)"	vhdl-template-procedure-body t]
	 ["Procedure (Decl)"	vhdl-template-procedure-decl t]
	 ["Process (Comb)"	vhdl-template-process-comb t]
	 ["Process (Seq)"	vhdl-template-process-seq t]
	 ["Report"		vhdl-template-report t]
	 ["Return"		vhdl-template-return t]
	 ["Select"		vhdl-template-selected-signal-asst t]
	 ["Signal"		vhdl-template-signal t]
	 ["Subtype"		vhdl-template-subtype t]
	 ["Type"		vhdl-template-type t]
	 ["Use"			vhdl-template-use t]
	 ["Variable"		vhdl-template-variable t]
	 ["Wait"		vhdl-template-wait t]
	 ["(Clocked Wait)"	vhdl-template-clocked-wait t]
	 ["When"		vhdl-template-when t]
	 ["While (Loop)"	vhdl-template-while-loop t]
	 ["With"		vhdl-template-with t]))
      (when (vhdl-standard-p 'ams)
	'(("VHDL-AMS Construct"
	   ["Break"		vhdl-template-break t]
	   ["Case (Use)"	vhdl-template-case-use t]
	   ["If (Use)"		vhdl-template-if-use t]
	   ["Limit"		vhdl-template-limit t]
	   ["Nature"		vhdl-template-nature t]
	   ["Procedural"	vhdl-template-procedural t]
	   ["Quantity (Free)"	vhdl-template-quantity-free t]
	   ["Quantity (Branch)"	vhdl-template-quantity-branch t]
	   ["Quantity (Source)"	vhdl-template-quantity-source t]
	   ["Subnature"		vhdl-template-subnature t]
	   ["Terminal"		vhdl-template-terminal t])))
      '(["Insert Construct..."	vhdl-template-insert-construct
				:keys "C-c C-i C-t"]
	"--")
      (list
       (append
	'("Package")
	(when (vhdl-standard-p 'math)
	  '(["math_complex"	vhdl-template-package-math-complex t]
	    ["math_real"	vhdl-template-package-math-real t]))
	'(["numeric_bit"	vhdl-template-package-numeric-bit t]
	  ["numeric_std"	vhdl-template-package-numeric-std t]
	  ["std_logic_1164"	vhdl-template-package-std-logic-1164 t]
	  ["textio"		vhdl-template-package-textio t]
	  "--"
	  ["std_logic_arith"	vhdl-template-package-std-logic-arith t]
	  ["std_logic_signed"	vhdl-template-package-std-logic-signed t]
	  ["std_logic_unsigned"	vhdl-template-package-std-logic-unsigned t]
	  ["std_logic_misc"	vhdl-template-package-std-logic-misc t]
	  ["std_logic_textio"	vhdl-template-package-std-logic-textio t]
	  "--"
	  ["Insert Package..."	vhdl-template-insert-package
				:keys "C-c C-i C-p"])))
      '(("Directive"
	 ["translate_on"	vhdl-template-directive-translate-on t]
	 ["translate_off"	vhdl-template-directive-translate-off t]
	 ["synthesis_on"	vhdl-template-directive-synthesis-on t]
	 ["synthesis_off"	vhdl-template-directive-synthesis-off t]
	 "--"
	 ["Insert Directive..."	vhdl-template-insert-directive
				:keys "C-c C-i C-d"])
	"--"
	["Insert Header"	vhdl-template-header :keys "C-c C-t C-h"]
	["Insert Footer"	vhdl-template-footer t]
	["Insert Date"		vhdl-template-insert-date t]
	["Modify Date"		vhdl-template-modify :keys "C-c C-t C-m"]
	"--"
	["Query Next Prompt"	vhdl-template-search-prompt t]))
    ,(append
      '("Model")
      ;; add menu entries for defined models
      (let ((model-alist vhdl-model-alist) menu-list model)
	(while model-alist
	  (setq model (car model-alist))
	  (setq menu-list
		(cons
		 (vector
		  (nth 0 model)
		  (vhdl-function-name "vhdl-model" (nth 0 model))
		  :keys (concat "C-c C-m " (key-description (nth 2 model))))
		 menu-list))
	  (setq model-alist (cdr model-alist)))
	(setq menu-list (nreverse menu-list))
	(vhdl-menu-split menu-list "Model"))
      '("--" "--"
	["Insert Model..."	vhdl-model-insert :keys "C-c C-i C-m"]
	["Customize Model..."	(customize-option 'vhdl-model-alist) t]))
    ("Port"
     ["Copy"			vhdl-port-copy t]
     "--"
     ["Paste As Entity"		vhdl-port-paste-entity vhdl-port-list]
     ["Paste As Component"	vhdl-port-paste-component vhdl-port-list]
     ["Paste As Instance"	vhdl-port-paste-instance
				:keys "C-c C-p C-i" :active vhdl-port-list]
     ["Paste As Signals"	vhdl-port-paste-signals vhdl-port-list]
     ["Paste As Constants"	vhdl-port-paste-constants vhdl-port-list]
     ["Paste As Generic Map"	vhdl-port-paste-generic-map vhdl-port-list]
     ["Paste As Initializations" vhdl-port-paste-initializations vhdl-port-list]
     "--"
     ["Paste As Testbench"	vhdl-port-paste-testbench vhdl-port-list]
     "--"
     ["Flatten"			vhdl-port-flatten
      :style toggle :selected vhdl-port-flattened :active vhdl-port-list]
     ["Reverse Direction"	vhdl-port-reverse-direction
      :style toggle :selected vhdl-port-reversed-direction :active vhdl-port-list])
    ("Compose"
     ["New Component"		vhdl-compose-new-component t]
     ["Copy Component"		vhdl-port-copy t]
     ["Place Component"		vhdl-compose-place-component vhdl-port-list]
     ["Wire Components"		vhdl-compose-wire-components t]
     "--"
     ["Generate Configuration"	vhdl-compose-configuration t]
     ["Generate Components Package"	vhdl-compose-components-package t])
    ("Subprogram"
     ["Copy"			vhdl-subprog-copy t]
     "--"
     ["Paste As Declaration"	vhdl-subprog-paste-declaration vhdl-subprog-list]
     ["Paste As Body"		vhdl-subprog-paste-body vhdl-subprog-list]
     ["Paste As Call"		vhdl-subprog-paste-call vhdl-subprog-list]
     "--"
     ["Flatten"			vhdl-subprog-flatten
      :style toggle :selected vhdl-subprog-flattened :active vhdl-subprog-list])
    "--"
    ("Comment"
     ["(Un)Comment Out Region"	vhdl-comment-uncomment-region (mark)]
     "--"
     ["Insert Inline Comment"	vhdl-comment-append-inline t]
     ["Insert Horizontal Line"	vhdl-comment-display-line t]
     ["Insert Display Comment"	vhdl-comment-display t]
     "--"
     ["Fill Comment"		fill-paragraph t]
     ["Fill Comment Region"	fill-region (mark)]
     ["Kill Comment Region"	vhdl-comment-kill-region (mark)]
     ["Kill Inline Comment Region" vhdl-comment-kill-inline-region (mark)])
    ("Line"
     ["Kill"			vhdl-line-kill t]
     ["Copy"			vhdl-line-copy t]
     ["Yank"			vhdl-line-yank t]
     ["Expand"			vhdl-line-expand t]
     "--"
     ["Transpose Next"		vhdl-line-transpose-next t]
     ["Transpose Prev"		vhdl-line-transpose-previous t]
     ["Open"			vhdl-line-open t]
     ["Join"			vhdl-delete-indentation t]
     "--"
     ["Goto"			goto-line t]
     ["(Un)Comment Out"		vhdl-comment-uncomment-line t])
    ("Move"
     ["Forward Statement"	vhdl-end-of-statement t]
     ["Backward Statement"	vhdl-beginning-of-statement t]
     ["Forward Expression"	vhdl-forward-sexp t]
     ["Backward Expression"	vhdl-backward-sexp t]
     ["Forward Same Indent"	vhdl-forward-same-indent t]
     ["Backward Same Indent"	vhdl-backward-same-indent t]
     ["Forward Function"	vhdl-end-of-defun t]
     ["Backward Function"	vhdl-beginning-of-defun t]
     ["Mark Function"		vhdl-mark-defun t])
    "--"
    ("Indent"
     ["Line"			indent-according-to-mode :keys "C-c C-i C-l"]
     ["Group"			vhdl-indent-group :keys "C-c C-i C-g"]
     ["Region"			vhdl-indent-region (mark)]
     ["Buffer"			vhdl-indent-buffer :keys "C-c C-i C-b"])
    ("Align"
     ["Group"			vhdl-align-group t]
     ["Same Indent"		vhdl-align-same-indent :keys "C-c C-a C-i"]
     ["List"			vhdl-align-list t]
     ["Declarations"		vhdl-align-declarations t]
     ["Region"			vhdl-align-region (mark)]
     ["Buffer"			vhdl-align-buffer t]
     "--"
     ["Inline Comment Group"	vhdl-align-inline-comment-group t]
     ["Inline Comment Region"	vhdl-align-inline-comment-region (mark)]
     ["Inline Comment Buffer"	vhdl-align-inline-comment-buffer t])
    ("Fill"
     ["List"			vhdl-fill-list t]
     ["Group"			vhdl-fill-group t]
     ["Same Indent"		vhdl-fill-same-indent :keys "C-c C-f C-i"]
     ["Region"			vhdl-fill-region (mark)])
    ("Beautify"
     ["Region"			vhdl-beautify-region (mark)]
     ["Buffer"			vhdl-beautify-buffer t])
    ("Fix"
     ["Generic/Port Clause"	vhdl-fix-clause t]
     "--"
     ["Case Region"		vhdl-fix-case-region (mark)]
     ["Case Buffer"		vhdl-fix-case-buffer t]
     "--"
     ["Whitespace Region"	vhdl-fixup-whitespace-region (mark)]
     ["Whitespace Buffer"	vhdl-fixup-whitespace-buffer t]
     "--"
     ["Trailing Spaces Buffer"	vhdl-remove-trailing-spaces t])
    ("Update"
     ["Sensitivity List"	vhdl-update-sensitivity-list-process t]
     ["Sensitivity List Buffer"	vhdl-update-sensitivity-list-buffer t])
    "--"
    ["Fontify Buffer"		vhdl-fontify-buffer t]
    ["Statistics Buffer"	vhdl-statistics-buffer t]
    ["Show Messages"		vhdl-show-messages t]
    ["Syntactic Info"		vhdl-show-syntactic-information t]
    "--"
    ["Speedbar"			vhdl-speedbar t]
    ["Hide/Show"		vhdl-hs-minor-mode t]
    "--"
    ("Documentation"
     ["VHDL Mode"		vhdl-doc-mode :keys "C-c C-h"]
     ["Release Notes"		(vhdl-doc-variable 'vhdl-doc-release-notes) t]
     ["Reserved Words"		(vhdl-doc-variable 'vhdl-doc-keywords) t]
     ["Coding Style"		(vhdl-doc-variable 'vhdl-doc-coding-style) t])
    ["Version"			vhdl-version t]
    ["Bug Report..."		vhdl-submit-bug-report t]
    "--"
    ("Options"
     ("Mode"
      ["Electric Mode"
       (progn (customize-set-variable 'vhdl-electric-mode
				      (not vhdl-electric-mode)))
       :style toggle :selected vhdl-electric-mode :keys "C-c C-m C-e"]
      ["Stutter Mode"
       (progn (customize-set-variable 'vhdl-stutter-mode
				      (not vhdl-stutter-mode)))
       :style toggle :selected vhdl-stutter-mode :keys "C-c C-m C-s"]
      ["Indent Tabs Mode"
       (progn (customize-set-variable 'vhdl-indent-tabs-mode
				      (not vhdl-indent-tabs-mode))
	      (setq indent-tabs-mode vhdl-indent-tabs-mode))
       :style toggle :selected vhdl-indent-tabs-mode]
      "--"
      ["Customize Group..." (customize-group 'vhdl-mode) t])
     ("Project"
      ["Project Setup..." (customize-option 'vhdl-project-alist) t]
      ,(append
	'("Selected Project at Startup"
	  ["None" (progn (customize-set-variable 'vhdl-project nil)
			 (vhdl-set-project ""))
	   :style radio :selected (null vhdl-project)]
	  "--")
	;; add menu entries for defined projects
	(let ((project-alist vhdl-project-alist) menu-list name)
	  (while project-alist
	    (setq name (caar project-alist))
	    (setq menu-list
		  (cons `[,name (progn (customize-set-variable
					'vhdl-project ,name)
				       (vhdl-set-project ,name))
			  :style radio :selected (equal ,name vhdl-project)]
			menu-list))
	    (setq project-alist (cdr project-alist)))
	  (setq menu-list (nreverse menu-list))
	  (vhdl-menu-split menu-list "Project")))
      ["Setup File Name..." (customize-option 'vhdl-project-file-name) t]
      ("Auto Load Setup File"
       ["At Startup"
	(customize-set-variable 'vhdl-project-auto-load
				(if (memq 'startup vhdl-project-auto-load)
				   (delq 'startup vhdl-project-auto-load)
				 (cons 'startup vhdl-project-auto-load)))
       :style toggle :selected (memq 'startup vhdl-project-auto-load)])
      ["Sort Projects"
       (customize-set-variable 'vhdl-project-sort (not vhdl-project-sort))
       :style toggle :selected vhdl-project-sort]
      "--"
      ["Customize Group..." (customize-group 'vhdl-project) t])
     ("Compiler"
      ["Compiler Setup..." (customize-option 'vhdl-compiler-alist) t]
      ,(append
	'("Selected Compiler at Startup")
	;; add menu entries for defined compilers
	(let ((comp-alist vhdl-compiler-alist) menu-list name)
	  (while comp-alist
	    (setq name (caar comp-alist))
	    (setq menu-list
		  (cons `[,name (customize-set-variable 'vhdl-compiler ,name)
			  :style radio :selected (equal ,name vhdl-compiler)]
			menu-list))
	    (setq comp-alist (cdr comp-alist)))
	  (setq menu-list (nreverse menu-list))
	  (vhdl-menu-split menu-list "Compiler")))
      ["Use Local Error Regexp"
       (customize-set-variable 'vhdl-compile-use-local-error-regexp
			       (not vhdl-compile-use-local-error-regexp))
       :style toggle :selected vhdl-compile-use-local-error-regexp]
      ["Makefile Generation Hook..."
       (customize-option 'vhdl-makefile-generation-hook) t]
      ["Default Library Name" (customize-option 'vhdl-default-library) t]
      "--"
      ["Customize Group..." (customize-group 'vhdl-compiler) t])
     ("Style"
      ("VHDL Standard"
       ["VHDL'87"
	(progn (customize-set-variable 'vhdl-standard
				       (list '87 (cadr vhdl-standard)))
	       (vhdl-activate-customizations))
	:style radio :selected (eq '87 (car vhdl-standard))]
       ["VHDL'93"
	(progn (customize-set-variable 'vhdl-standard
				       (list '93 (cadr vhdl-standard)))
	       (vhdl-activate-customizations))
	:style radio :selected (eq '93 (car vhdl-standard))]
       "--"
       ["VHDL-AMS"
	(progn (customize-set-variable
		'vhdl-standard (list (car vhdl-standard)
				     (if (memq 'ams (cadr vhdl-standard))
					 (delq 'ams (cadr vhdl-standard))
				       (cons 'ams (cadr vhdl-standard)))))
	       (vhdl-activate-customizations))
	:style toggle :selected (memq 'ams (cadr vhdl-standard))]
       ["Math Packages"
	(progn (customize-set-variable
		'vhdl-standard (list (car vhdl-standard)
				     (if (memq 'math (cadr vhdl-standard))
					 (delq 'math (cadr vhdl-standard))
				       (cons 'math (cadr vhdl-standard)))))
	       (vhdl-activate-customizations))
	:style toggle :selected (memq 'math (cadr vhdl-standard))])
      ["Indentation Offset..." (customize-option 'vhdl-basic-offset) t]
      ["Upper Case Keywords"
       (customize-set-variable 'vhdl-upper-case-keywords
			       (not vhdl-upper-case-keywords))
       :style toggle :selected vhdl-upper-case-keywords]
      ["Upper Case Types"
       (customize-set-variable 'vhdl-upper-case-types
			       (not vhdl-upper-case-types))
       :style toggle :selected vhdl-upper-case-types]
      ["Upper Case Attributes"
       (customize-set-variable 'vhdl-upper-case-attributes
			       (not vhdl-upper-case-attributes))
       :style toggle :selected vhdl-upper-case-attributes]
      ["Upper Case Enumeration Values"
       (customize-set-variable 'vhdl-upper-case-enum-values
			       (not vhdl-upper-case-enum-values))
       :style toggle :selected vhdl-upper-case-enum-values]
      ["Upper Case Constants"
       (customize-set-variable 'vhdl-upper-case-constants
			       (not vhdl-upper-case-constants))
       :style toggle :selected vhdl-upper-case-constants]
      ("Use Direct Instantiation"
       ["Never"
	(customize-set-variable 'vhdl-use-direct-instantiation 'never)
	:style radio :selected (eq 'never vhdl-use-direct-instantiation)]
       ["Standard"
	(customize-set-variable 'vhdl-use-direct-instantiation 'standard)
	:style radio :selected (eq 'standard vhdl-use-direct-instantiation)]
       ["Always"
	(customize-set-variable 'vhdl-use-direct-instantiation 'always)
	:style radio :selected (eq 'always vhdl-use-direct-instantiation)])
      "--"
      ["Customize Group..." (customize-group 'vhdl-style) t])
     ("Naming"
      ["Entity File Name..." (customize-option 'vhdl-entity-file-name) t]
      ["Architecture File Name..."
       (customize-option 'vhdl-architecture-file-name) t]
      ["Configuration File Name..."
       (customize-option 'vhdl-configuration-file-name) t]
      ["Package File Name..." (customize-option 'vhdl-package-file-name) t]
      ("File Name Case"
       ["As Is"
	(customize-set-variable 'vhdl-file-name-case 'identity)
	:style radio :selected (eq 'identity vhdl-file-name-case)]
       ["Lower Case"
	(customize-set-variable 'vhdl-file-name-case 'downcase)
	:style radio :selected (eq 'downcase vhdl-file-name-case)]
       ["Upper Case"
	(customize-set-variable 'vhdl-file-name-case 'upcase)
	:style radio :selected (eq 'upcase vhdl-file-name-case)]
       ["Capitalize"
	(customize-set-variable 'vhdl-file-name-case 'capitalize)
	:style radio :selected (eq 'capitalize vhdl-file-name-case)])
      "--"
      ["Customize Group..." (customize-group 'vhdl-naming) t])
     ("Template"
      ("Electric Keywords"
       ["VHDL Keywords"
	(customize-set-variable 'vhdl-electric-keywords
				(if (memq 'vhdl vhdl-electric-keywords)
				    (delq 'vhdl vhdl-electric-keywords)
				  (cons 'vhdl vhdl-electric-keywords)))
	:style toggle :selected (memq 'vhdl vhdl-electric-keywords)]
       ["User Model Keywords"
	(customize-set-variable 'vhdl-electric-keywords
				(if (memq 'user vhdl-electric-keywords)
				     (delq 'user vhdl-electric-keywords)
				   (cons 'user vhdl-electric-keywords)))
	:style toggle :selected (memq 'user vhdl-electric-keywords)])
      ("Insert Optional Labels"
       ["None"
	(customize-set-variable 'vhdl-optional-labels 'none)
	:style radio :selected (eq 'none vhdl-optional-labels)]
       ["Processes Only"
	(customize-set-variable 'vhdl-optional-labels 'process)
	:style radio :selected (eq 'process vhdl-optional-labels)]
       ["All Constructs"
	(customize-set-variable 'vhdl-optional-labels 'all)
	:style radio :selected (eq 'all vhdl-optional-labels)])
      ("Insert Empty Lines"
       ["None"
	(customize-set-variable 'vhdl-insert-empty-lines 'none)
	:style radio :selected (eq 'none vhdl-insert-empty-lines)]
       ["Design Units Only"
	(customize-set-variable 'vhdl-insert-empty-lines 'unit)
	:style radio :selected (eq 'unit vhdl-insert-empty-lines)]
       ["All Constructs"
	(customize-set-variable 'vhdl-insert-empty-lines 'all)
	:style radio :selected (eq 'all vhdl-insert-empty-lines)])
      ["Argument List Indent"
       (customize-set-variable 'vhdl-argument-list-indent
			       (not vhdl-argument-list-indent))
       :style toggle :selected vhdl-argument-list-indent]
      ["Association List with Formals"
       (customize-set-variable 'vhdl-association-list-with-formals
			       (not vhdl-association-list-with-formals))
       :style toggle :selected vhdl-association-list-with-formals]
      ["Conditions in Parenthesis"
       (customize-set-variable 'vhdl-conditions-in-parenthesis
			       (not vhdl-conditions-in-parenthesis))
       :style toggle :selected vhdl-conditions-in-parenthesis]
      ["Zero String..." (customize-option 'vhdl-zero-string) t]
      ["One String..." (customize-option 'vhdl-one-string) t]
      ("File Header"
       ["Header String..." (customize-option 'vhdl-file-header) t]
       ["Footer String..." (customize-option 'vhdl-file-footer) t]
       ["Company Name..." (customize-option 'vhdl-company-name) t]
       ["Copyright String..." (customize-option 'vhdl-copyright-string) t]
       ["Platform Specification..." (customize-option 'vhdl-platform-spec) t]
       ["Date Format..." (customize-option 'vhdl-date-format) t]
       ["Modify Date Prefix String..."
	(customize-option 'vhdl-modify-date-prefix-string) t]
       ["Modify Date on Saving"
	(progn (customize-set-variable 'vhdl-modify-date-on-saving
				       (not vhdl-modify-date-on-saving))
	       (vhdl-activate-customizations))
	:style toggle :selected vhdl-modify-date-on-saving])
      ("Sequential Process"
       ("Kind of Reset"
	["None"
	 (customize-set-variable 'vhdl-reset-kind 'none)
	 :style radio :selected (eq 'none vhdl-reset-kind)]
	["Synchronous"
	 (customize-set-variable 'vhdl-reset-kind 'sync)
	 :style radio :selected (eq 'sync vhdl-reset-kind)]
	["Asynchronous"
	 (customize-set-variable 'vhdl-reset-kind 'async)
	 :style radio :selected (eq 'async vhdl-reset-kind)])
       ["Reset is Active High"
	(customize-set-variable 'vhdl-reset-active-high
				(not vhdl-reset-active-high))
	:style toggle :selected vhdl-reset-active-high]
       ["Use Rising Clock Edge"
	(customize-set-variable 'vhdl-clock-rising-edge
				(not vhdl-clock-rising-edge))
	:style toggle :selected vhdl-clock-rising-edge]
       ("Clock Edge Condition"
	["Standard"
	 (customize-set-variable 'vhdl-clock-edge-condition 'standard)
	 :style radio :selected (eq 'standard vhdl-clock-edge-condition)]
	["Function \"rising_edge\""
	 (customize-set-variable 'vhdl-clock-edge-condition 'function)
	 :style radio :selected (eq 'function vhdl-clock-edge-condition)])
       ["Clock Name..." (customize-option 'vhdl-clock-name) t]
       ["Reset Name..." (customize-option 'vhdl-reset-name) t])
      "--"
      ["Customize Group..." (customize-group 'vhdl-template) t])
     ("Model"
      ["Model Definition..." (customize-option 'vhdl-model-alist) t])
     ("Port"
      ["Include Port Comments"
       (customize-set-variable 'vhdl-include-port-comments
			       (not vhdl-include-port-comments))
       :style toggle :selected vhdl-include-port-comments]
      ["Include Direction Comments"
       (customize-set-variable 'vhdl-include-direction-comments
			       (not vhdl-include-direction-comments))
       :style toggle :selected vhdl-include-direction-comments]
      ["Include Type Comments"
       (customize-set-variable 'vhdl-include-type-comments
			       (not vhdl-include-type-comments))
       :style toggle :selected vhdl-include-type-comments]
      ("Include Group Comments"
       ["Never"
	(customize-set-variable 'vhdl-include-group-comments 'never)
	:style radio :selected (eq 'never vhdl-include-group-comments)]
       ["Declarations"
	(customize-set-variable 'vhdl-include-group-comments 'decl)
	:style radio :selected (eq 'decl vhdl-include-group-comments)]
       ["Always"
	(customize-set-variable 'vhdl-include-group-comments 'always)
	:style radio :selected (eq 'always vhdl-include-group-comments)])
      ["Actual Port Name..." (customize-option 'vhdl-actual-port-name) t]
      ["Instance Name..." (customize-option 'vhdl-instance-name) t]
      ("Testbench"
       ["Entity Name..." (customize-option 'vhdl-testbench-entity-name) t]
       ["Architecture Name..."
	(customize-option 'vhdl-testbench-architecture-name) t]
       ["Configuration Name..."
	(customize-option 'vhdl-testbench-configuration-name) t]
       ["DUT Name..." (customize-option 'vhdl-testbench-dut-name) t]
       ["Include Header"
	(customize-set-variable 'vhdl-testbench-include-header
				(not vhdl-testbench-include-header))
	:style toggle :selected vhdl-testbench-include-header]
       ["Declarations..." (customize-option 'vhdl-testbench-declarations) t]
       ["Statements..." (customize-option 'vhdl-testbench-statements) t]
       ["Initialize Signals"
	(customize-set-variable 'vhdl-testbench-initialize-signals
				(not vhdl-testbench-initialize-signals))
	:style toggle :selected vhdl-testbench-initialize-signals]
       ["Include Library Clause"
	(customize-set-variable 'vhdl-testbench-include-library
				(not vhdl-testbench-include-library))
	:style toggle :selected vhdl-testbench-include-library]
       ["Include Configuration"
	(customize-set-variable 'vhdl-testbench-include-configuration
				(not vhdl-testbench-include-configuration))
	:style toggle :selected vhdl-testbench-include-configuration]
       ("Create Files"
	["None"
	 (customize-set-variable 'vhdl-testbench-create-files 'none)
	 :style radio :selected (eq 'none vhdl-testbench-create-files)]
	["Single"
	 (customize-set-variable 'vhdl-testbench-create-files 'single)
	 :style radio :selected (eq 'single vhdl-testbench-create-files)]
	["Separate"
	 (customize-set-variable 'vhdl-testbench-create-files 'separate)
	 :style radio :selected (eq 'separate vhdl-testbench-create-files)])
       ["Testbench Entity File Name..."
	(customize-option 'vhdl-testbench-entity-file-name) t]
       ["Testbench Architecture File Name..."
	(customize-option 'vhdl-testbench-architecture-file-name) t])
      "--"
      ["Customize Group..." (customize-group 'vhdl-port) t])
     ("Compose"
      ["Architecture Name..."
       (customize-option 'vhdl-compose-architecture-name) t]
      ["Configuration Name..."
       (customize-option 'vhdl-compose-configuration-name) t]
      ["Components Package Name..."
       (customize-option 'vhdl-components-package-name) t]
      ["Use Components Package"
       (customize-set-variable 'vhdl-use-components-package
			       (not vhdl-use-components-package))
       :style toggle :selected vhdl-use-components-package]
      ["Include Header"
       (customize-set-variable 'vhdl-compose-include-header
			       (not vhdl-compose-include-header))
       :style toggle :selected vhdl-compose-include-header]
      ("Create Entity/Architecture Files"
       ["None"
	(customize-set-variable 'vhdl-compose-create-files 'none)
	:style radio :selected (eq 'none vhdl-compose-create-files)]
       ["Single"
	(customize-set-variable 'vhdl-compose-create-files 'single)
	:style radio :selected (eq 'single vhdl-compose-create-files)]
       ["Separate"
	(customize-set-variable 'vhdl-compose-create-files 'separate)
	:style radio :selected (eq 'separate vhdl-compose-create-files)])
      ["Create Configuration File"
       (customize-set-variable 'vhdl-compose-configuration-create-file
			       (not vhdl-compose-configuration-create-file))
       :style toggle :selected vhdl-compose-configuration-create-file]
      ["Hierarchical Configuration"
       (customize-set-variable 'vhdl-compose-configuration-hierarchical
			       (not vhdl-compose-configuration-hierarchical))
       :style toggle :selected vhdl-compose-configuration-hierarchical]
      ["Use Subconfiguration"
       (customize-set-variable 'vhdl-compose-configuration-use-subconfiguration
			       (not vhdl-compose-configuration-use-subconfiguration))
       :style toggle :selected vhdl-compose-configuration-use-subconfiguration]
      "--"
      ["Customize Group..." (customize-group 'vhdl-compose) t])
     ("Comment"
      ["Self Insert Comments"
       (customize-set-variable 'vhdl-self-insert-comments
			       (not vhdl-self-insert-comments))
       :style toggle :selected vhdl-self-insert-comments]
      ["Prompt for Comments"
       (customize-set-variable 'vhdl-prompt-for-comments
			       (not vhdl-prompt-for-comments))
       :style toggle :selected vhdl-prompt-for-comments]
      ["Inline Comment Column..."
       (customize-option 'vhdl-inline-comment-column) t]
      ["End Comment Column..." (customize-option 'vhdl-end-comment-column) t]
      "--"
      ["Customize Group..." (customize-group 'vhdl-comment) t])
     ("Align"
      ["Auto Align Templates"
       (customize-set-variable 'vhdl-auto-align (not vhdl-auto-align))
       :style toggle :selected vhdl-auto-align]
      ["Align Line Groups"
       (customize-set-variable 'vhdl-align-groups (not vhdl-align-groups))
       :style toggle :selected vhdl-align-groups]
      ["Group Separation String..."
       (customize-set-variable 'vhdl-align-group-separate) t]
      ["Align Lines with Same Indent"
       (customize-set-variable 'vhdl-align-same-indent
			       (not vhdl-align-same-indent))
       :style toggle :selected vhdl-align-same-indent]
      "--"
      ["Customize Group..." (customize-group 'vhdl-align) t])
     ("Highlight"
      ["Highlighting On/Off..."
       (customize-option
	(if (fboundp 'global-font-lock-mode)
	    'global-font-lock-mode 'font-lock-auto-fontify)) t]
      ["Highlight Keywords"
       (progn (customize-set-variable 'vhdl-highlight-keywords
				      (not vhdl-highlight-keywords))
	      (vhdl-fontify-buffer))
       :style toggle :selected vhdl-highlight-keywords]
      ["Highlight Names"
       (progn (customize-set-variable 'vhdl-highlight-names
				      (not vhdl-highlight-names))
	      (vhdl-fontify-buffer))
       :style toggle :selected vhdl-highlight-names]
      ["Highlight Special Words"
       (progn (customize-set-variable 'vhdl-highlight-special-words
				      (not vhdl-highlight-special-words))
	      (vhdl-fontify-buffer))
       :style toggle :selected vhdl-highlight-special-words]
      ["Highlight Forbidden Words"
       (progn (customize-set-variable 'vhdl-highlight-forbidden-words
				      (not vhdl-highlight-forbidden-words))
	      (vhdl-fontify-buffer))
       :style toggle :selected vhdl-highlight-forbidden-words]
      ["Highlight Verilog Keywords"
       (progn (customize-set-variable 'vhdl-highlight-verilog-keywords
				      (not vhdl-highlight-verilog-keywords))
	      (vhdl-fontify-buffer))
       :style toggle :selected vhdl-highlight-verilog-keywords]
      ["Highlight \"translate_off\""
       (progn (customize-set-variable 'vhdl-highlight-translate-off
				      (not vhdl-highlight-translate-off))
	      (vhdl-fontify-buffer))
       :style toggle :selected vhdl-highlight-translate-off]
      ["Case Sensitive Highlighting"
       (progn (customize-set-variable 'vhdl-highlight-case-sensitive
				      (not vhdl-highlight-case-sensitive))
	      (vhdl-fontify-buffer))
       :style toggle :selected vhdl-highlight-case-sensitive]
      ["Special Syntax Definition..."
       (customize-option 'vhdl-special-syntax-alist) t]
      ["Forbidden Words..." (customize-option 'vhdl-forbidden-words) t]
      ["Forbidden Syntax..." (customize-option 'vhdl-forbidden-syntax) t]
      ["Directive Keywords..." (customize-option 'vhdl-directive-keywords) t]
      ["Colors..." (customize-group 'vhdl-highlight-faces) t]
      "--"
      ["Customize Group..." (customize-group 'vhdl-highlight) t])
     ("Speedbar"
      ["Auto Open at Startup"
       (customize-set-variable 'vhdl-speedbar-auto-open
			       (not vhdl-speedbar-auto-open))
       :style toggle :selected vhdl-speedbar-auto-open]
      ("Default Displaying Mode"
       ["Files"
	(customize-set-variable 'vhdl-speedbar-display-mode 'files)
	:style radio :selected (eq 'files vhdl-speedbar-display-mode)]
       ["Directory Hierarchy"
	(customize-set-variable 'vhdl-speedbar-display-mode 'directory)
	:style radio :selected (eq 'directory vhdl-speedbar-display-mode)]
       ["Project Hierarchy"
	(customize-set-variable 'vhdl-speedbar-display-mode 'project)
	:style radio :selected (eq 'project vhdl-speedbar-display-mode)])
      ["Indentation Offset..."
       (customize-option 'speedbar-indentation-width) t]
      ["Scan Size Limits..." (customize-option 'vhdl-speedbar-scan-limit) t]
      ["Jump to Unit when Opening"
       (customize-set-variable 'vhdl-speedbar-jump-to-unit
			       (not vhdl-speedbar-jump-to-unit))
       :style toggle :selected vhdl-speedbar-jump-to-unit]
      ["Update Hierarchy on File Saving"
       (customize-set-variable 'vhdl-speedbar-update-on-saving
			       (not vhdl-speedbar-update-on-saving))
       :style toggle :selected vhdl-speedbar-update-on-saving]
      ("Save in Cache File"
       ["Hierarchy Information"
	(customize-set-variable 'vhdl-speedbar-save-cache
				(if (memq 'hierarchy vhdl-speedbar-save-cache)
				    (delq 'hierarchy vhdl-speedbar-save-cache)
				  (cons 'hierarchy vhdl-speedbar-save-cache)))
	:style toggle :selected (memq 'hierarchy vhdl-speedbar-save-cache)]
       ["Displaying Status"
	(customize-set-variable 'vhdl-speedbar-save-cache
				(if (memq 'display vhdl-speedbar-save-cache)
				    (delq 'display vhdl-speedbar-save-cache)
				  (cons 'display vhdl-speedbar-save-cache)))
	:style toggle :selected (memq 'display vhdl-speedbar-save-cache)])
      ["Cache File Name..."
       (customize-option 'vhdl-speedbar-cache-file-name) t]
      "--"
      ["Customize Group..." (customize-group 'vhdl-speedbar) t])
     ("Menu"
      ["Add Index Menu when Loading File"
       (progn (customize-set-variable 'vhdl-index-menu (not vhdl-index-menu))
	      (vhdl-index-menu-init))
       :style toggle :selected vhdl-index-menu]
      ["Add Source File Menu when Loading File"
       (progn (customize-set-variable 'vhdl-source-file-menu
				      (not vhdl-source-file-menu))
	      (vhdl-add-source-files-menu))
       :style toggle :selected vhdl-source-file-menu]
      ["Add Hideshow Menu at Startup"
       (progn (customize-set-variable 'vhdl-hideshow-menu
				      (not vhdl-hideshow-menu))
	      (vhdl-activate-customizations))
       :style toggle :selected vhdl-hideshow-menu]
      ["Hide Everything Initially"
       (customize-set-variable 'vhdl-hide-all-init (not vhdl-hide-all-init))
       :style toggle :selected vhdl-hide-all-init]
      "--"
      ["Customize Group..." (customize-group 'vhdl-menu) t])
     ("Print"
      ["In Two Column Format"
       (progn (customize-set-variable 'vhdl-print-two-column
				      (not vhdl-print-two-column))
	      (message "Activate new setting by saving options and restarting Emacs"))
       :style toggle :selected vhdl-print-two-column]
      ["Use Customized Faces"
       (progn (customize-set-variable 'vhdl-print-customize-faces
				      (not vhdl-print-customize-faces))
	      (message "Activate new setting by saving options and restarting Emacs"))
       :style toggle :selected vhdl-print-customize-faces]
      "--"
      ["Customize Group..." (customize-group 'vhdl-print) t])
     ("Miscellaneous"
      ["Use Intelligent Tab"
       (progn (customize-set-variable 'vhdl-intelligent-tab
				      (not vhdl-intelligent-tab))
	      (vhdl-activate-customizations))
       :style toggle :selected vhdl-intelligent-tab]
      ["Indent Syntax-Based"
       (customize-set-variable 'vhdl-indent-syntax-based
			       (not vhdl-indent-syntax-based))
       :style toggle :selected vhdl-indent-syntax-based]
      ["Word Completion is Case Sensitive"
       (customize-set-variable 'vhdl-word-completion-case-sensitive
			       (not vhdl-word-completion-case-sensitive))
       :style toggle :selected vhdl-word-completion-case-sensitive]
      ["Word Completion in Minibuffer"
       (progn (customize-set-variable 'vhdl-word-completion-in-minibuffer
				      (not vhdl-word-completion-in-minibuffer))
	      (message "Activate new setting by saving options and restarting Emacs"))
       :style toggle :selected vhdl-word-completion-in-minibuffer]
      ["Underscore is Part of Word"
       (progn (customize-set-variable 'vhdl-underscore-is-part-of-word
				      (not vhdl-underscore-is-part-of-word))
	      (vhdl-activate-customizations))
       :style toggle :selected vhdl-underscore-is-part-of-word]
      "--"
      ["Customize Group..." (customize-group 'vhdl-misc) t])
     ["Related..." (customize-browse 'vhdl-related) t]
     "--"
     ["Save Options" customize-save-customized t]
     ["Activate Options" vhdl-activate-customizations t]
     ["Browse Options..." vhdl-customize t])))

(defvar vhdl-mode-menu-list (vhdl-create-mode-menu)
  "VHDL Mode menu.")

(defun vhdl-update-mode-menu ()
  "Update VHDL Mode menu."
  (interactive)
  (easy-menu-remove vhdl-mode-menu-list) ; for XEmacs
  (setq vhdl-mode-menu-list (vhdl-create-mode-menu))
  (easy-menu-add vhdl-mode-menu-list)	; for XEmacs
  (easy-menu-define vhdl-mode-menu vhdl-mode-map
		    "Menu keymap for VHDL Mode." vhdl-mode-menu-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Index menu (using `imenu.el'), also used for speedbar (using `speedbar.el')

(defconst vhdl-imenu-generic-expression
  '(
    ("Subprogram"
     "^\\s-*\\(\\(\\(impure\\|pure\\)\\s-+\\|\\)function\\|procedure\\)\\s-+\\(\"?\\(\\w\\|\\s_\\)+\"?\\)"
     4)
    ("Instance"
     "^\\s-*\\(\\(\\w\\|\\s_\\)+\\s-*:\\(\\s-\\|\n\\)*\\(\\w\\|\\s_\\)+\\)\\(\\s-\\|\n\\)+\\(generic\\|port\\)\\s-+map\\>"
     1)
    ("Component"
     "^\\s-*\\(component\\)\\s-+\\(\\(\\w\\|\\s_\\)+\\)"
     2)
    ("Procedural"
     "^\\s-*\\(\\(\\w\\|\\s_\\)+\\)\\s-*:\\(\\s-\\|\n\\)*\\(procedural\\)"
     1)
    ("Process"
     "^\\s-*\\(\\(\\w\\|\\s_\\)+\\)\\s-*:\\(\\s-\\|\n\\)*\\(\\(postponed\\s-+\\|\\)process\\)"
     1)
    ("Block"
     "^\\s-*\\(\\(\\w\\|\\s_\\)+\\)\\s-*:\\(\\s-\\|\n\\)*\\(block\\)"
     1)
    ("Package"
     "^\\s-*\\(package\\( body\\|\\)\\)\\s-+\\(\\(\\w\\|\\s_\\)+\\)"
     3)
    ("Configuration"
     "^\\s-*\\(configuration\\)\\s-+\\(\\(\\w\\|\\s_\\)+\\s-+of\\s-+\\(\\w\\|\\s_\\)+\\)"
     2)
    ("Architecture"
     "^\\s-*\\(architecture\\)\\s-+\\(\\(\\w\\|\\s_\\)+\\s-+of\\s-+\\(\\w\\|\\s_\\)+\\)"
     2)
    ("Entity"
     "^\\s-*\\(entity\\)\\s-+\\(\\(\\w\\|\\s_\\)+\\)"
     2)
    )
  "Imenu generic expression for VHDL Mode.  See `imenu-generic-expression'.")

(defun vhdl-index-menu-init ()
  "Initialize index menu."
  (set (make-local-variable 'imenu-case-fold-search) t)
  (set (make-local-variable 'imenu-generic-expression)
       vhdl-imenu-generic-expression)
  (when (and vhdl-index-menu (fboundp 'imenu))
    (if (or (not (boundp 'font-lock-maximum-size))
	    (> font-lock-maximum-size (buffer-size)))
	(imenu-add-to-menubar "Index")
      (message "Scanning buffer for index...buffer too big"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Source file menu (using `easy-menu.el')

(defvar vhdl-sources-menu nil)

(defun vhdl-directory-files (directory &optional full match)
  "Call `directory-files' if DIRECTORY exists, otherwise generate error
message."
  (if (not (file-directory-p directory))
      (vhdl-warning-when-idle "No such directory: \"%s\"" directory)
    (let ((dir (directory-files directory full match)))
      (setq dir (delete "." dir))
      (setq dir (delete ".." dir))
      dir)))

(defun vhdl-get-source-files (&optional full directory)
  "Get list of VHDL source files in DIRECTORY or current directory."
  (let ((mode-alist auto-mode-alist)
	filename-regexp)
    ;; create regular expressions for matching file names
    (setq filename-regexp "\\`[^.].*\\(")
    (while mode-alist
      (when (eq (cdar mode-alist) 'vhdl-mode)
	(setq filename-regexp
	      (concat filename-regexp (caar mode-alist) "\\|")))
      (setq mode-alist (cdr mode-alist)))
    (setq filename-regexp
	  (concat (substring filename-regexp 0
			     (string-match "\\\\|$" filename-regexp)) "\\)"))
    ;; find files
    (vhdl-directory-files
     (or directory default-directory) full filename-regexp)))

(defun vhdl-add-source-files-menu ()
  "Scan directory for all VHDL source files and generate menu.
The directory of the current source file is scanned."
  (interactive)
  (message "Scanning directory for source files ...")
  (let ((newmap (current-local-map))
	(file-list (vhdl-get-source-files))
	menu-list found)
    ;; Create list for menu
    (setq found nil)
    (while file-list
      (setq found t)
      (setq menu-list (cons (vector (car file-list)
				   (list 'find-file (car file-list)) t)
			   menu-list))
      (setq file-list (cdr file-list)))
    (setq menu-list (vhdl-menu-split menu-list "Sources"))
    (when found (setq menu-list (cons "--" menu-list)))
    (setq menu-list (cons ["*Rescan*" vhdl-add-source-files-menu t] menu-list))
    (setq menu-list (cons "Sources" menu-list))
    ;; Create menu
    (easy-menu-add menu-list)
    (easy-menu-define vhdl-sources-menu newmap
		      "VHDL source files menu" menu-list))
  (message ""))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mode definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; performs all buffer local initializations

;;;###autoload
(define-derived-mode vhdl-mode prog-mode
  '("VHDL" (vhdl-electric-mode "/" (vhdl-stutter-mode "/"))
           (vhdl-electric-mode "e")
           (vhdl-stutter-mode "s"))
  "Major mode for editing VHDL code.

Usage:
------

  TEMPLATE INSERTION (electrification):
    After typing a VHDL keyword and entering `SPC', you are prompted for
    arguments while a template is generated for that VHDL construct.  Typing
    `RET' or `C-g' at the first \(mandatory) prompt aborts the current
    template generation.  Optional arguments are indicated by square
    brackets and removed if the queried string is left empty.  Prompts for
    mandatory arguments remain in the code if the queried string is left
    empty.  They can be queried again by `C-c C-t C-q'.  Enabled
    electrification is indicated by `/e' in the modeline.

      Typing `M-SPC' after a keyword inserts a space without calling the
    template generator.  Automatic template generation (i.e.
    electrification) can be disabled (enabled) by typing `C-c C-m C-e' or by
    setting option `vhdl-electric-mode' (see OPTIONS).

      Template generators can be invoked from the VHDL menu, by key
    bindings, by typing `C-c C-i C-c' and choosing a construct, or by typing
    the keyword (i.e. first word of menu entry not in parenthesis) and
    `SPC'.  The following abbreviations can also be used: arch, attr, cond,
    conf, comp, cons, func, inst, pack, sig, var.

      Template styles can be customized in customization group
    `vhdl-template' \(see OPTIONS).


  HEADER INSERTION:
    A file header can be inserted by `C-c C-t C-h'.  A file footer
    (template at the end of the file) can be inserted by `C-c C-t C-f'.
    See customization group `vhdl-header'.


  STUTTERING:
    Double striking of some keys inserts cumbersome VHDL syntax elements.
    Stuttering can be disabled (enabled) by typing `C-c C-m C-s' or by
    option `vhdl-stutter-mode'.  Enabled stuttering is indicated by `/s' in
    the modeline.  The stuttering keys and their effects are:

      ;;   -->  \" : \"       [   -->  (        --    -->  comment
      ;;;  -->  \" := \"      [[  -->  [        --CR  -->  comment-out code
      ..   -->  \" => \"      ]   -->  )        ---   -->  horizontal line
      ,,   -->  \" <= \"      ]]  -->  ]        ----  -->  display comment
      ==   -->  \" == \"      ''  -->  \\\"


  WORD COMPLETION:
    Typing `TAB' after a (not completed) word looks for a VHDL keyword or a
    word in the buffer that starts alike, inserts it and adjusts case.
    Re-typing `TAB' toggles through alternative word completions.  This also
    works in the minibuffer (i.e. in template generator prompts).

      Typing `TAB' after `(' looks for and inserts complete parenthesized
    expressions (e.g. for array index ranges).  All keywords as well as
    standard types and subprograms of VHDL have predefined abbreviations
    \(e.g. type \"std\" and `TAB' will toggle through all standard types
    beginning with \"std\").

      Typing `TAB' after a non-word character indents the line if at the
    beginning of a line (i.e. no preceding non-blank characters), and
    inserts a tabulator stop otherwise.  `M-TAB' always inserts a tabulator
    stop.


  COMMENTS:
        `--'       puts a single comment.
        `---'      draws a horizontal line for separating code segments.
        `----'     inserts a display comment, i.e. two horizontal lines
                   with a comment in between.
        `--CR'     comments out code on that line.  Re-hitting CR comments
                   out following lines.
        `C-c c'    comments out a region if not commented out,
                   uncomments a region if already commented out.

      You are prompted for comments after object definitions (i.e. signals,
    variables, constants, ports) and after subprogram and process
    specifications if option `vhdl-prompt-for-comments' is non-nil.
    Comments are automatically inserted as additional labels (e.g. after
    begin statements) and as help comments if `vhdl-self-insert-comments' is
    non-nil.

      Inline comments (i.e. comments after a piece of code on the same line)
    are indented at least to `vhdl-inline-comment-column'.  Comments go at
    maximum to `vhdl-end-comment-column'.  `RET' after a space in a comment
    will open a new comment line.  Typing beyond `vhdl-end-comment-column'
    in a comment automatically opens a new comment line.  `M-q' re-fills
    multi-line comments.


  INDENTATION:
    `TAB' indents a line if at the beginning of the line.  The amount of
    indentation is specified by option `vhdl-basic-offset'.  `C-c C-i C-l'
    always indents the current line (is bound to `TAB' if option
    `vhdl-intelligent-tab' is nil).

      Indentation can be done for a group of lines (`C-c C-i C-g'), a region
    \(`M-C-\\') or the entire buffer (menu).  Argument and port lists are
    indented normally (nil) or relative to the opening parenthesis (non-nil)
    according to option `vhdl-argument-list-indent'.

      If option `vhdl-indent-tabs-mode' is nil, spaces are used instead of
    tabs.  `M-x tabify' and `M-x untabify' allow to convert spaces to tabs
    and vice versa.

      Syntax-based indentation can be very slow in large files.  Option
    `vhdl-indent-syntax-based' allows to use faster but simpler indentation.


  ALIGNMENT:
    The alignment functions align operators, keywords, and inline comments
    to beautify the code.  `C-c C-a C-a' aligns a group of consecutive lines
    separated by blank lines, `C-c C-a C-i' a block of lines with same
    indent.  `C-c C-a C-l' aligns all lines belonging to a list enclosed by
    a pair of parentheses (e.g. port clause/map, argument list), and `C-c
    C-a C-d' all lines within the declarative part of a design unit.  `C-c
    C-a M-a' aligns an entire region.  `C-c C-a C-c' aligns inline comments
    for a group of lines, and `C-c C-a M-c' for a region.

      If option `vhdl-align-groups' is non-nil, groups of code lines
    separated by special lines (see option `vhdl-align-group-separate') are
    aligned individually.  If option `vhdl-align-same-indent' is non-nil,
    blocks of lines with same indent are aligned separately.  Some templates
    are automatically aligned after generation if option `vhdl-auto-align'
    is non-nil.

      Alignment tries to align inline comments at
    `vhdl-inline-comment-column' and tries inline comment not to exceed
    `vhdl-end-comment-column'.

      `C-c C-x M-w' fixes up whitespace in a region.  That is, operator
    symbols are surrounded by one space, and multiple spaces are eliminated.


  CODE FILLING:
    Code filling allows to condense code (e.g. sensitivity lists or port
    maps) by removing comments and newlines and re-wrapping so that all
    lines are maximally filled (block filling).  `C-c C-f C-f' fills a list
    enclosed by parenthesis, `C-c C-f C-g' a group of lines separated by
    blank lines, `C-c C-f C-i' a block of lines with same indent, and
    `C-c C-f M-f' an entire region.


  CODE BEAUTIFICATION:
    `C-c M-b' and `C-c C-b' beautify the code of a region or of the entire
    buffer respectively.  This includes indentation, alignment, and case
    fixing.  Code beautification can also be run non-interactively using the
    command:

      emacs -batch -l ~/.emacs filename.vhd -f vhdl-beautify-buffer


  PORT TRANSLATION:
    Generic and port clauses from entity or component declarations can be
    copied (`C-c C-p C-w') and pasted as entity and component declarations,
    as component instantiations and corresponding internal constants and
    signals, as a generic map with constants as actual generics, and as
    internal signal initializations (menu).

      To include formals in component instantiations, see option
    `vhdl-association-list-with-formals'.  To include comments in pasting,
    see options `vhdl-include-...-comments'.

      A clause with several generic/port names on the same line can be
    flattened (`C-c C-p C-f') so that only one name per line exists.  The
    direction of ports can be reversed (`C-c C-p C-r'), i.e., inputs become
    outputs and vice versa, which can be useful in testbenches.  (This
    reversion is done on the internal data structure and is only reflected
    in subsequent paste operations.)

      Names for actual ports, instances, testbenches, and
    design-under-test instances can be derived from existing names according
    to options `vhdl-...-name'.  See customization group `vhdl-port'.


  SUBPROGRAM TRANSLATION:
    Similar functionality exists for copying/pasting the interface of
    subprograms (function/procedure).  A subprogram interface can be copied
    and then pasted as a subprogram declaration, body or call (uses
    association list with formals).


  TESTBENCH GENERATION:
    A copied port can also be pasted as a testbench.  The generated
    testbench includes an entity, an architecture, and an optional
    configuration.  The architecture contains the component declaration and
    instantiation of the DUT as well as internal constant and signal
    declarations.  Additional user-defined templates can be inserted.  The
    names used for entity/architecture/configuration/DUT as well as the file
    structure to be generated can be customized. See customization group
   `vhdl-testbench'.


  KEY BINDINGS:
    Key bindings (`C-c ...') exist for most commands (see in menu).


  VHDL MENU:
    All commands can be found in the VHDL menu including their key bindings.


  FILE BROWSER:
    The speedbar allows browsing of directories and file contents.  It can
    be accessed from the VHDL menu and is automatically opened if option
    `vhdl-speedbar-auto-open' is non-nil.

      In speedbar, open files and directories with `mouse-2' on the name and
    browse/rescan their contents with `mouse-2'/`S-mouse-2' on the `+'.


  DESIGN HIERARCHY BROWSER:
    The speedbar can also be used for browsing the hierarchy of design units
    contained in the source files of the current directory or the specified
    projects (see option `vhdl-project-alist').

      The speedbar can be switched between file, directory hierarchy and
    project hierarchy browsing mode in the speedbar menu or by typing `f',
    `h' or `H' in speedbar.

      In speedbar, open design units with `mouse-2' on the name and browse
    their hierarchy with `mouse-2' on the `+'.  Ports can directly be copied
    from entities and components (in packages).  Individual design units and
    complete designs can directly be compiled (\"Make\" menu entry).

      The hierarchy is automatically updated upon saving a modified source
    file when option `vhdl-speedbar-update-on-saving' is non-nil.  The
    hierarchy is only updated for projects that have been opened once in the
    speedbar.  The hierarchy is cached between Emacs sessions in a file (see
    options in group `vhdl-speedbar').

      Simple design consistency checks are done during scanning, such as
    multiple declarations of the same unit or missing primary units that are
    required by secondary units.


  STRUCTURAL COMPOSITION:
    Enables simple structural composition.  `C-c C-c C-n' creates a skeleton
    for a new component.  Subcomponents (i.e. component declaration and
    instantiation) can be automatically placed from a previously read port
    \(`C-c C-c C-p') or directly from the hierarchy browser (`P').  Finally,
    all subcomponents can be automatically connected using internal signals
    and ports (`C-c C-c C-w') following these rules:
      - subcomponent actual ports with same name are considered to be
        connected by a signal (internal signal or port)
      - signals that are only inputs to subcomponents are considered as
        inputs to this component -> input port created
      - signals that are only outputs from subcomponents are considered as
        outputs from this component -> output port created
      - signals that are inputs to AND outputs from subcomponents are
        considered as internal connections -> internal signal created

      Purpose:  With appropriate naming conventions it is possible to
    create higher design levels with only a few mouse clicks or key
    strokes.  A new design level can be created by simply generating a new
    component, placing the required subcomponents from the hierarchy
    browser, and wiring everything automatically.

      Note: Automatic wiring only works reliably on templates of new
    components and component instantiations that were created by VHDL mode.

      Component declarations can be placed in a components package (option
    `vhdl-use-components-package') which can be automatically generated for
    an entire directory or project (`C-c C-c M-p').  The VHDL'93 direct
    component instantiation is also supported (option
    `vhdl-use-direct-instantiation').

|     Configuration declarations can automatically be generated either from
|   the menu (`C-c C-c C-f') (for the architecture the cursor is in) or from
|   the speedbar menu (for the architecture under the cursor).  The
|   configurations can optionally be hierarchical (i.e. include all
|   component levels of a hierarchical design, option
|   `vhdl-compose-configuration-hierarchical') or include subconfigurations
|   (option `vhdl-compose-configuration-use-subconfiguration').  For
|   subcomponents in hierarchical configurations, the most-recently-analyzed
|   (mra) architecture is selected.  If another architecture is desired, it
|   can be marked as most-recently-analyzed (speedbar menu) before
|   generating the configuration.
|
|     Note: Configurations of subcomponents (i.e. hierarchical configuration
|   declarations) are currently not considered when displaying
|   configurations in speedbar.

      See the options group `vhdl-compose' for all relevant user options.


  SOURCE FILE COMPILATION:
    The syntax of the current buffer can be analyzed by calling a VHDL
    compiler (menu, `C-c C-k').  The compiler to be used is specified by
    option `vhdl-compiler'.  The available compilers are listed in option
    `vhdl-compiler-alist' including all required compilation command,
    command options, compilation directory, and error message syntax
    information.  New compilers can be added.

      All the source files of an entire design can be compiled by the `make'
    command (menu, `C-c M-C-k') if an appropriate Makefile exists.


  MAKEFILE GENERATION:
    Makefiles can be generated automatically by an internal generation
    routine (`C-c M-k').  The library unit dependency information is
    obtained from the hierarchy browser.  Makefile generation can be
    customized for each compiler in option `vhdl-compiler-alist'.

      Makefile generation can also be run non-interactively using the
    command:

        emacs -batch -l ~/.emacs -l vhdl-mode
              [-compiler compilername] [-project projectname]
              -f vhdl-generate-makefile

      The Makefile's default target \"all\" compiles the entire design, the
    target \"clean\" removes it and the target \"library\" creates the
    library directory if not existent.  The Makefile also includes a target
    for each primary library unit which allows selective compilation of this
    unit, its secondary units and its subhierarchy (example: compilation of
    a design specified by a configuration).  User specific parts can be
    inserted into a Makefile with option `vhdl-makefile-generation-hook'.

    Limitations:
      - Only library units and dependencies within the current library are
        considered.  Makefiles for designs that span multiple libraries are
        not (yet) supported.
      - Only one-level configurations are supported (also hierarchical),
        but configurations that go down several levels are not.
      - The \"others\" keyword in configurations is not supported.


  PROJECTS:
    Projects can be defined in option `vhdl-project-alist' and a current
    project be selected using option `vhdl-project' (permanently) or from
    the menu or speedbar (temporarily).  For each project, title and
    description strings (for the file headers), source files/directories
    (for the hierarchy browser and Makefile generation), library name, and
    compiler-dependent options, exceptions and compilation directory can be
    specified.  Compilation settings overwrite the settings of option
    `vhdl-compiler-alist'.

      Project setups can be exported (i.e. written to a file) and imported.
    Imported setups are not automatically saved in `vhdl-project-alist' but
    can be saved afterwards in its customization buffer.  When starting
    Emacs with VHDL Mode (i.e. load a VHDL file or use \"emacs -l
    vhdl-mode\") in a directory with an existing project setup file, it is
    automatically loaded and its project activated if option
    `vhdl-project-auto-load' is non-nil.  Names/paths of the project setup
    files can be specified in option `vhdl-project-file-name'.  Multiple
    project setups can be automatically loaded from global directories.
    This is an alternative to specifying project setups with option
    `vhdl-project-alist'.


  SPECIAL MENUES:
    As an alternative to the speedbar, an index menu can be added (set
    option `vhdl-index-menu' to non-nil) or made accessible as a mouse menu
    (e.g. add \"(global-set-key '[S-down-mouse-3] 'imenu)\" to your start-up
    file) for browsing the file contents (is not populated if buffer is
    larger than `font-lock-maximum-size').  Also, a source file menu can be
    added (set option `vhdl-source-file-menu' to non-nil) for browsing the
    current directory for VHDL source files.


  VHDL STANDARDS:
    The VHDL standards to be used are specified in option `vhdl-standard'.
    Available standards are: VHDL'87/'93, VHDL-AMS, and Math Packages.


  KEYWORD CASE:
    Lower and upper case for keywords and standardized types, attributes,
    and enumeration values is supported.  If the option
    `vhdl-upper-case-keywords' is set to non-nil, keywords can be typed in
    lower case and are converted into upper case automatically (not for
    types, attributes, and enumeration values).  The case of keywords,
    types, attributes,and enumeration values can be fixed for an entire
    region (menu) or buffer (`C-c C-x C-c') according to the options
    `vhdl-upper-case-{keywords,types,attributes,enum-values}'.


  HIGHLIGHTING (fontification):
    Keywords and standardized types, attributes, enumeration values, and
    function names (controlled by option `vhdl-highlight-keywords'), as well
    as comments, strings, and template prompts are highlighted using
    different colors.  Unit, subprogram, signal, variable, constant,
    parameter and generic/port names in declarations as well as labels are
    highlighted if option `vhdl-highlight-names' is non-nil.

      Additional reserved words or words with a forbidden syntax (e.g. words
    that should be avoided) can be specified in option
    `vhdl-forbidden-words' or `vhdl-forbidden-syntax' and be highlighted in
    a warning color (option `vhdl-highlight-forbidden-words').  Verilog
    keywords are highlighted as forbidden words if option
    `vhdl-highlight-verilog-keywords' is non-nil.

      Words with special syntax can be highlighted by specifying their
    syntax and color in option `vhdl-special-syntax-alist' and by setting
    option `vhdl-highlight-special-words' to non-nil.  This allows to
    establish some naming conventions (e.g. to distinguish different kinds
    of signals or other objects by using name suffices) and to support them
    visually.

      Option `vhdl-highlight-case-sensitive' can be set to non-nil in order
    to support case-sensitive highlighting.  However, keywords are then only
    highlighted if written in lower case.

      Code between \"translate_off\" and \"translate_on\" pragmas is
    highlighted using a different background color if option
    `vhdl-highlight-translate-off' is non-nil.

      For documentation and customization of the used colors see
    customization group `vhdl-highlight-faces' (`M-x customize-group').  For
    highlighting of matching parenthesis, see customization group
    `paren-showing'.  Automatic buffer highlighting is turned on/off by
    option `global-font-lock-mode' (`font-lock-auto-fontify' in XEmacs).


  USER MODELS:
    VHDL models (templates) can be specified by the user and made accessible
    in the menu, through key bindings (`C-c C-m ...'), or by keyword
    electrification.  See option `vhdl-model-alist'.


  HIDE/SHOW:
    The code of blocks, processes, subprograms, component declarations and
    instantiations, generic/port clauses, and configuration declarations can
    be hidden using the `Hide/Show' menu or by pressing `S-mouse-2' within
    the code (see customization group `vhdl-menu').  XEmacs: limited
    functionality due to old `hideshow.el' package.


  CODE UPDATING:
    - Sensitivity List: `C-c C-u C-s' updates the sensitivity list of the
      current process, `C-c C-u M-s' of all processes in the current buffer.
      Limitations:
        - Only declared local signals (ports, signals declared in
          architecture and blocks) are automatically inserted.
        - Global signals declared in packages are not automatically inserted.
          Insert them once manually (will be kept afterwards).
        - Out parameters of procedures are considered to be read.
      Use option `vhdl-entity-file-name' to specify the entity file name
      \(used to obtain the port names).


  CODE FIXING:
    `C-c C-x C-p' fixes the closing parenthesis of a generic/port clause
    \(e.g. if the closing parenthesis is on the wrong line or is missing).


  PRINTING:
    PostScript printing with different faces (an optimized set of faces is
    used if `vhdl-print-customize-faces' is non-nil) or colors \(if
    `ps-print-color-p' is non-nil) is possible using the standard Emacs
    PostScript printing commands.  Option `vhdl-print-two-column' defines
    appropriate default settings for nice landscape two-column printing.
    The paper format can be set by option `ps-paper-type'.  Do not forget to
    switch `ps-print-color-p' to nil for printing on black-and-white
    printers.


  OPTIONS:
    User options allow customization of VHDL Mode.  All options are
    accessible from the \"Options\" menu entry.  Simple options (switches
    and choices) can directly be changed, while for complex options a
    customization buffer is opened.  Changed options can be saved for future
    sessions using the \"Save Options\" menu entry.

      Options and their detailed descriptions can also be accessed by using
    the \"Customize\" menu entry or the command `M-x customize-option' (`M-x
    customize-group' for groups).  Some customizations only take effect
    after some action (read the NOTE in the option documentation).
    Customization can also be done globally (i.e. site-wide, read the
    INSTALL file).

      Not all options are described in this documentation, so go and see
    what other useful user options there are (`M-x vhdl-customize' or menu)!


  FILE EXTENSIONS:
    As default, files with extensions \".vhd\" and \".vhdl\" are
    automatically recognized as VHDL source files.  To add an extension
    \".xxx\", add the following line to your Emacs start-up file (`.emacs'):

      \(setq auto-mode-alist (cons '(\"\\\\.xxx\\\\'\" . vhdl-mode) auto-mode-alist))


  HINTS:
    - To start Emacs with open VHDL hierarchy browser without having to load
      a VHDL file first, use the command:

        emacs -l vhdl-mode -f speedbar-frame-mode

    - Type `C-g C-g' to interrupt long operations or if Emacs hangs.

    - Some features only work on properly indented code.


  RELEASE NOTES:
    See also the release notes (menu) for added features in new releases.


Maintenance:
------------

To submit a bug report, enter `M-x vhdl-submit-bug-report' within VHDL Mode.
Add a description of the problem and include a reproducible test case.

Questions and enhancement requests can be sent to <reto@gnu.org>.

The `vhdl-mode-announce' mailing list informs about new VHDL Mode releases.
The `vhdl-mode-victims' mailing list informs about new VHDL Mode beta
releases.  You are kindly invited to participate in beta testing.  Subscribe
to above mailing lists by sending an email to <reto@gnu.org>.

VHDL Mode is officially distributed at
URL `http://opensource.ethz.ch/emacs/vhdl-mode.html'
where the latest version can be found.


Known problems:
---------------

- Indentation bug in simultaneous if- and case-statements (VHDL-AMS).
- XEmacs: Incorrect start-up when automatically opening speedbar.
- XEmacs: Indentation in XEmacs 21.4 (and higher).


                                                The VHDL Mode Authors
                                            Reto Zimmermann and Rod Whitby

Key bindings:
-------------

\\{vhdl-mode-map}"
  :abbrev-table vhdl-mode-abbrev-table

  ;; set local variables
  (set (make-local-variable 'paragraph-start)
       "\\s-*\\(--+\\s-*$\\|[^ -]\\|$\\)")
  (set (make-local-variable 'paragraph-separate) paragraph-start)
  (set (make-local-variable 'paragraph-ignore-fill-prefix) t)
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set (make-local-variable 'indent-line-function) 'vhdl-indent-line)
  (set (make-local-variable 'comment-start) "--")
  (set (make-local-variable 'comment-end) "")
  (when vhdl-emacs-21
    (set (make-local-variable 'comment-padding) ""))
  (set (make-local-variable 'comment-column) vhdl-inline-comment-column)
  (set (make-local-variable 'end-comment-column) vhdl-end-comment-column)
  (set (make-local-variable 'comment-start-skip) "--+\\s-*")
  (set (make-local-variable 'comment-multi-line) nil)
  (set (make-local-variable 'indent-tabs-mode) vhdl-indent-tabs-mode)
  (set (make-local-variable 'hippie-expand-verbose) nil)

  ;; setup the comment indent variable in a Emacs version portable way
  ;; ignore any byte compiler warnings you might get here
  (when (boundp 'comment-indent-function)
    (set (make-local-variable 'comment-indent-function) 'vhdl-comment-indent))

  ;; initialize font locking
  (set (make-local-variable 'font-lock-defaults)
       (list
	'(nil vhdl-font-lock-keywords) nil
	(not vhdl-highlight-case-sensitive) '((?\_ . "w")) 'beginning-of-line))
  (if (eval-when-compile (fboundp 'syntax-propertize-rules))
      (set (make-local-variable 'syntax-propertize-function)
           (syntax-propertize-rules
            ;; Mark single quotes as having string quote syntax in
            ;; 'c' instances.
            ("\\(\'\\).\\(\'\\)" (1 "\"'") (2 "\"'"))))
    (set (make-local-variable 'font-lock-syntactic-keywords)
         vhdl-font-lock-syntactic-keywords))
  (unless vhdl-emacs-21
    (set (make-local-variable 'font-lock-support-mode) 'lazy-lock-mode)
    (set (make-local-variable 'lazy-lock-defer-contextually) nil)
    (set (make-local-variable 'lazy-lock-defer-on-the-fly) t)
;    (set (make-local-variable 'lazy-lock-defer-time) 0.1)
    (set (make-local-variable 'lazy-lock-defer-on-scrolling) t))
;  (turn-on-font-lock)

  ;; variables for source file compilation
  (when vhdl-compile-use-local-error-regexp
    (set (make-local-variable 'compilation-error-regexp-alist) nil)
    (set (make-local-variable 'compilation-file-regexp-alist) nil))

  ;; add index menu
  (vhdl-index-menu-init)
  ;; add source file menu
  (if vhdl-source-file-menu (vhdl-add-source-files-menu))
  ;; add VHDL menu
  (easy-menu-add vhdl-mode-menu-list)	; for XEmacs
  (easy-menu-define vhdl-mode-menu vhdl-mode-map
		    "Menu keymap for VHDL Mode." vhdl-mode-menu-list)
  ;; initialize hideshow and add menu
  (vhdl-hideshow-init)
  (run-hooks 'menu-bar-update-hook)

  ;; miscellaneous
  (vhdl-ps-print-init)
  (vhdl-write-file-hooks-init)
  (message "VHDL Mode %s.%s" vhdl-version
	   (if noninteractive "" "  See menu for documentation and release notes.")))

(defun vhdl-activate-customizations ()
  "Activate all customizations on local variables."
  (interactive)
  (vhdl-mode-map-init)
  (use-local-map vhdl-mode-map)
  (set-syntax-table vhdl-mode-syntax-table)
  (setq comment-column vhdl-inline-comment-column)
  (setq end-comment-column vhdl-end-comment-column)
  (vhdl-write-file-hooks-init)
  (vhdl-update-mode-menu)
  (vhdl-hideshow-init)
  (run-hooks 'menu-bar-update-hook))

(defun vhdl-write-file-hooks-init ()
  "Add/remove hooks when buffer is saved."
  (if vhdl-modify-date-on-saving
      (add-hook 'local-write-file-hooks 'vhdl-template-modify-noerror nil t)
    (remove-hook 'local-write-file-hooks 'vhdl-template-modify-noerror t))
  (if (featurep 'xemacs) (make-local-hook 'after-save-hook))
  (add-hook 'after-save-hook 'vhdl-add-modified-file nil t))

(defun vhdl-process-command-line-option (option)
  "Process command line options for VHDL Mode."
  (cond
   ;; set compiler
   ((equal option "-compiler")
    (vhdl-set-compiler (car command-line-args-left))
    (setq command-line-args-left (cdr command-line-args-left)))
   ;; set project
   ((equal option "-project")
    (vhdl-set-project (car command-line-args-left))
    (setq command-line-args-left (cdr command-line-args-left)))))

;; make Emacs process VHDL Mode options
(setq command-switch-alist
      (append command-switch-alist
	      '(("-compiler" . vhdl-process-command-line-option)
		("-project" . vhdl-process-command-line-option))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keywords and standardized words
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst vhdl-93-keywords
  '(
    "abs" "access" "after" "alias" "all" "and" "architecture" "array"
    "assert" "attribute"
    "begin" "block" "body" "buffer" "bus"
    "case" "component" "configuration" "constant"
    "disconnect" "downto"
    "else" "elsif" "end" "entity" "exit"
    "file" "for" "function"
    "generate" "generic" "group" "guarded"
    "if" "impure" "in" "inertial" "inout" "is"
    "label" "library" "linkage" "literal" "loop"
    "map" "mod"
    "nand" "new" "next" "nor" "not" "null"
    "of" "on" "open" "or" "others" "out"
    "package" "port" "postponed" "procedure" "process" "pure"
    "range" "record" "register" "reject" "rem" "report" "return"
    "rol" "ror"
    "select" "severity" "shared" "signal" "sla" "sll" "sra" "srl" "subtype"
    "then" "to" "transport" "type"
    "unaffected" "units" "until" "use"
    "variable"
    "wait" "when" "while" "with"
    "xnor" "xor"
    )
  "List of VHDL'93 keywords.")

(defconst vhdl-ams-keywords
  '(
    "across" "break" "limit" "nature" "noise" "procedural" "quantity"
    "reference" "spectrum" "subnature" "terminal" "through"
    "tolerance"
    )
  "List of VHDL-AMS keywords.")

(defconst vhdl-verilog-keywords
  '(
    "`define" "`else" "`endif" "`ifdef" "`include" "`timescale" "`undef"
    "always" "and" "assign" "begin" "buf" "bufif0" "bufif1"
    "case" "casex" "casez" "cmos" "deassign" "default" "defparam" "disable"
    "edge" "else" "end" "endattribute" "endcase" "endfunction" "endmodule"
    "endprimitive" "endspecify" "endtable" "endtask" "event"
    "for" "force" "forever" "fork" "function"
    "highz0" "highz1" "if" "initial" "inout" "input" "integer" "join" "large"
    "macromodule" "makefile" "medium" "module"
    "nand" "negedge" "nmos" "nor" "not" "notif0" "notif1" "or" "output"
    "parameter" "pmos" "posedge" "primitive" "pull0" "pull1" "pulldown"
    "pullup"
    "rcmos" "real" "realtime" "reg" "release" "repeat" "rnmos" "rpmos" "rtran"
    "rtranif0" "rtranif1"
    "scalared" "signed" "small" "specify" "specparam" "strength" "strong0"
    "strong1" "supply" "supply0" "supply1"
    "table" "task" "time" "tran" "tranif0" "tranif1" "tri" "tri0" "tri1"
    "triand" "trior" "trireg"
    "vectored" "wait" "wand" "weak0" "weak1" "while" "wire" "wor" "xnor" "xor"
    )
  "List of Verilog keywords as candidate for additional reserved words.")

(defconst vhdl-93-types
  '(
    "boolean" "bit" "bit_vector" "character" "severity_level" "integer"
    "real" "time" "natural" "positive" "string" "line" "text" "side"
    "unsigned" "signed" "delay_length" "file_open_kind" "file_open_status"
    "std_logic" "std_logic_vector"
    "std_ulogic" "std_ulogic_vector"
    )
  "List of VHDL'93 standardized types.")

(defconst vhdl-ams-types
  '(
    "domain_type" "real_vector"
    ;; from `nature_pkg' package
    "voltage" "current" "electrical" "position" "velocity" "force"
    "mechanical_vf" "mechanical_pf" "rotvel" "torque" "rotational"
    "pressure" "flowrate" "fluid"
  )
  "List of VHDL-AMS standardized types.")

(defconst vhdl-math-types
  '(
    "complex" "complex_polar"
    )
  "List of Math Packages standardized types.")

(defconst vhdl-93-attributes
  '(
    "base" "left" "right" "high" "low" "pos" "val" "succ"
    "pred" "leftof" "rightof" "range" "reverse_range"
    "length" "delayed" "stable" "quiet" "transaction"
    "event" "active" "last_event" "last_active" "last_value"
    "driving" "driving_value" "ascending" "value" "image"
    "simple_name" "instance_name" "path_name"
    "foreign"
    )
  "List of VHDL'93 standardized attributes.")

(defconst vhdl-ams-attributes
  '(
    "across" "through"
    "reference" "contribution" "tolerance"
    "dot" "integ" "delayed" "above" "zoh" "ltf" "ztf"
    "ramp" "slew"
    )
  "List of VHDL-AMS standardized attributes.")

(defconst vhdl-93-enum-values
  '(
    "true" "false"
    "note" "warning" "error" "failure"
    "read_mode" "write_mode" "append_mode"
    "open_ok" "status_error" "name_error" "mode_error"
    "fs" "ps" "ns" "us" "ms" "sec" "min" "hr"
    "right" "left"
    )
  "List of VHDL'93 standardized enumeration values.")

(defconst vhdl-ams-enum-values
  '(
    "quiescent_domain" "time_domain" "frequency_domain"
    ;; from `nature_pkg' package
    "eps0" "mu0" "ground" "mecvf_gnd" "mecpf_gnd" "rot_gnd" "fld_gnd"
    )
  "List of VHDL-AMS standardized enumeration values.")

(defconst vhdl-math-constants
  '(
    "math_e" "math_1_over_e"
    "math_pi" "math_two_pi" "math_1_over_pi"
    "math_half_pi" "math_q_pi" "math_3_half_pi"
    "math_log_of_2" "math_log_of_10" "math_log2_of_e" "math_log10_of_e"
    "math_sqrt2" "math_sqrt1_2" "math_sqrt_pi"
    "math_deg_to_rad" "math_rad_to_deg"
    "cbase_1" "cbase_j" "czero"
    )
  "List of Math Packages standardized constants.")

(defconst vhdl-93-functions
  '(
    "now" "resolved" "rising_edge" "falling_edge"
    "read" "readline" "write" "writeline" "endfile"
    "resize" "is_X" "std_match"
    "shift_left" "shift_right" "rotate_left" "rotate_right"
    "to_unsigned" "to_signed" "to_integer"
    "to_stdLogicVector" "to_stdULogic" "to_stdULogicVector"
    "to_bit" "to_bitVector" "to_X01" "to_X01Z" "to_UX01" "to_01"
    "conv_unsigned" "conv_signed" "conv_integer" "conv_std_logic_vector"
    "shl" "shr" "ext" "sxt"
    "deallocate"
    )
  "List of VHDL'93 standardized functions.")

(defconst vhdl-ams-functions
  '(
    "frequency"
    )
  "List of VHDL-AMS standardized functions.")

(defconst vhdl-math-functions
  '(
    "sign" "ceil" "floor" "round" "trunc" "fmax" "fmin" "uniform"
    "sqrt" "cbrt" "exp" "log"
    "sin" "cos" "tan" "arcsin" "arccos" "arctan"
    "sinh" "cosh" "tanh" "arcsinh" "arccosh" "arctanh"
    "cmplx" "complex_to_polar" "polar_to_complex" "arg" "conj"
    )
  "List of Math Packages standardized functions.")

(defconst vhdl-93-packages
  '(
    "std_logic_1164" "numeric_std" "numeric_bit"
    "standard" "textio"
    "std_logic_arith" "std_logic_signed" "std_logic_unsigned"
    "std_logic_misc" "std_logic_textio"
    "ieee" "std" "work"
    )
  "List of VHDL'93 standardized packages and libraries.")

(defconst vhdl-ams-packages
  '(
    ;; from `nature_pkg' package
    "nature_pkg"
    )
  "List of VHDL-AMS standardized packages and libraries.")

(defconst vhdl-math-packages
  '(
    "math_real" "math_complex"
    )
  "List of Math Packages standardized packages and libraries.")

(defvar vhdl-keywords nil
  "List of VHDL keywords.")

(defvar vhdl-types nil
  "List of VHDL standardized types.")

(defvar vhdl-attributes nil
  "List of VHDL standardized attributes.")

(defvar vhdl-enum-values nil
  "List of VHDL standardized enumeration values.")

(defvar vhdl-constants nil
  "List of VHDL standardized constants.")

(defvar vhdl-functions nil
  "List of VHDL standardized functions.")

(defvar vhdl-packages nil
  "List of VHDL standardized packages and libraries.")

(defvar vhdl-reserved-words nil
  "List of additional reserved words.")

(defvar vhdl-keywords-regexp nil
  "Regexp for VHDL keywords.")

(defvar vhdl-types-regexp nil
  "Regexp for VHDL standardized types.")

(defvar vhdl-attributes-regexp nil
  "Regexp for VHDL standardized attributes.")

(defvar vhdl-enum-values-regexp nil
  "Regexp for VHDL standardized enumeration values.")

(defvar vhdl-functions-regexp nil
  "Regexp for VHDL standardized functions.")

(defvar vhdl-packages-regexp nil
  "Regexp for VHDL standardized packages and libraries.")

(defvar vhdl-reserved-words-regexp nil
  "Regexp for additional reserved words.")

(defvar vhdl-directive-keywords-regexp nil
  "Regexp for compiler directive keywords.")

(defun vhdl-words-init ()
  "Initialize reserved words."
  (setq vhdl-keywords
	(append vhdl-93-keywords
		(when (vhdl-standard-p 'ams) vhdl-ams-keywords)))
  (setq vhdl-types
	(append vhdl-93-types
		(when (vhdl-standard-p 'ams) vhdl-ams-types)
		(when (vhdl-standard-p 'math) vhdl-math-types)))
  (setq vhdl-attributes
	(append vhdl-93-attributes
		(when (vhdl-standard-p 'ams) vhdl-ams-attributes)))
  (setq vhdl-enum-values
	(append vhdl-93-enum-values
		(when (vhdl-standard-p 'ams) vhdl-ams-enum-values)))
  (setq vhdl-constants
	(append (when (vhdl-standard-p 'math) vhdl-math-constants)))
  (setq vhdl-functions
	(append vhdl-93-functions
		(when (vhdl-standard-p 'ams) vhdl-ams-functions)
		(when (vhdl-standard-p 'math) vhdl-math-functions)))
  (setq vhdl-packages
	(append vhdl-93-packages
		(when (vhdl-standard-p 'ams) vhdl-ams-packages)
		(when (vhdl-standard-p 'math) vhdl-math-packages)))
  (setq vhdl-reserved-words
	(append (when vhdl-highlight-forbidden-words vhdl-forbidden-words)
		(when vhdl-highlight-verilog-keywords vhdl-verilog-keywords)
		'("")))
  (setq vhdl-keywords-regexp
	(concat "\\<\\(" (regexp-opt vhdl-keywords) "\\)\\>"))
  (setq vhdl-types-regexp
	(concat "\\<\\(" (regexp-opt vhdl-types) "\\)\\>"))
  (setq vhdl-attributes-regexp
	(concat "\\<\\(" (regexp-opt vhdl-attributes) "\\)\\>"))
  (setq vhdl-enum-values-regexp
	(concat "\\<\\(" (regexp-opt vhdl-enum-values) "\\)\\>"))
  (setq vhdl-functions-regexp
	(concat "\\<\\(" (regexp-opt vhdl-functions) "\\)\\>"))
  (setq vhdl-packages-regexp
	(concat "\\<\\(" (regexp-opt vhdl-packages) "\\)\\>"))
  (setq vhdl-reserved-words-regexp
	(concat "\\<\\("
		(unless (equal vhdl-forbidden-syntax "")
		  (concat vhdl-forbidden-syntax "\\|"))
		(regexp-opt vhdl-reserved-words)
		"\\)\\>"))
  (setq vhdl-directive-keywords-regexp
	(concat "\\<\\(" (mapconcat 'regexp-quote
				    vhdl-directive-keywords "\\|") "\\)\\>"))
  (vhdl-abbrev-list-init))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Words to expand

(defvar vhdl-abbrev-list nil
  "Predefined abbreviations for VHDL.")

(defun vhdl-abbrev-list-init ()
  (setq vhdl-abbrev-list
	(append
	 (list vhdl-upper-case-keywords) vhdl-keywords
	 (list vhdl-upper-case-types) vhdl-types
	 (list vhdl-upper-case-attributes) vhdl-attributes
	 (list vhdl-upper-case-enum-values) vhdl-enum-values
	 (list vhdl-upper-case-constants) vhdl-constants
	 (list nil) vhdl-functions
	 (list nil) vhdl-packages)))

;; initialize reserved words for VHDL Mode
(vhdl-words-init)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Indentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax analysis

;; constant regular expressions for looking at various constructs

(defconst vhdl-symbol-key "\\(\\w\\|\\s_\\)+"
  "Regexp describing a VHDL symbol.
We cannot use just `word' syntax class since `_' cannot be in word
class.  Putting underscore in word class breaks forward word movement
behavior that users are familiar with.")

(defconst vhdl-case-header-key "case[( \t\n][^;=>]+[) \t\n]is"
  "Regexp describing a case statement header key.")

(defconst vhdl-label-key
  (concat "\\(" vhdl-symbol-key "\\s-*:\\)[^=]")
  "Regexp describing a VHDL label.")

;; Macro definitions:

(defmacro vhdl-point (position)
  "Return the value of point at certain commonly referenced POSITIONs.
POSITION can be one of the following symbols:

bol  -- beginning of line
eol  -- end of line
bod  -- beginning of defun
boi  -- back to indentation
eoi  -- last whitespace on line
ionl -- indentation of next line
iopl -- indentation of previous line
bonl -- beginning of next line
bopl -- beginning of previous line

This function does not modify point or mark."
  (or (and (eq 'quote (car-safe position))
	   (null (cddr position)))
      (error "ERROR:  Bad buffer position requested: %s" position))
  (setq position (nth 1 position))
  `(let ((here (point)))
     ,@(cond
	((eq position 'bol)  '((beginning-of-line)))
	((eq position 'eol)  '((end-of-line)))
	((eq position 'bod)  '((save-match-data
				 (vhdl-beginning-of-defun))))
	((eq position 'boi)  '((back-to-indentation)))
	((eq position 'eoi)  '((end-of-line) (skip-chars-backward " \t")))
	((eq position 'bonl) '((forward-line 1)))
	((eq position 'bopl) '((forward-line -1)))
	((eq position 'iopl)
	 '((forward-line -1)
	   (back-to-indentation)))
	((eq position 'ionl)
	 '((forward-line 1)
	   (back-to-indentation)))
	(t (error "ERROR:  Unknown buffer position requested: %s" position))
	)
     (prog1
	 (point)
       (goto-char here))
     ;; workaround for an Emacs18 bug -- blech! Well, at least it
     ;; doesn't hurt for v19
     ,@nil
     ))

(defmacro vhdl-safe (&rest body)
  "Safely execute BODY, return nil if an error occurred."
  `(condition-case nil
       (progn ,@body)
     (error nil)))

(defmacro vhdl-add-syntax (symbol &optional relpos)
  "A simple macro to append the syntax in SYMBOL to the syntax list.
Try to increase performance by using this macro."
  `(setq vhdl-syntactic-context
	 (cons (cons ,symbol ,relpos) vhdl-syntactic-context)))

(defmacro vhdl-has-syntax (symbol)
  "A simple macro to return check the syntax list.
Try to increase performance by using this macro."
  `(assoc ,symbol vhdl-syntactic-context))

;; Syntactic element offset manipulation:

(defun vhdl-read-offset (langelem)
  "Read new offset value for LANGELEM from minibuffer.
Return a valid value only."
  (let ((oldoff (format "%s" (cdr-safe (assq langelem vhdl-offsets-alist))))
	(errmsg "Offset must be int, func, var, or one of +, -, ++, --: ")
	(prompt "Offset: ")
	offset input interned)
    (while (not offset)
      (setq input (read-string prompt oldoff)
	    offset (cond ((string-equal "+" input) '+)
			 ((string-equal "-" input) '-)
			 ((string-equal "++" input) '++)
			 ((string-equal "--" input) '--)
			 ((string-match "^-?[0-9]+$" input)
			  (string-to-number input))
			 ((fboundp (setq interned (intern input)))
			  interned)
			 ((boundp interned) interned)
			 ;; error, but don't signal one, keep trying
			 ;; to read an input value
			 (t (ding)
			    (setq prompt errmsg)
			    nil))))
    offset))

(defun vhdl-set-offset (symbol offset &optional add-p)
  "Change the value of a syntactic element symbol in `vhdl-offsets-alist'.
SYMBOL is the syntactic element symbol to change and OFFSET is the new
offset for that syntactic element.  Optional ADD-P says to add SYMBOL to
`vhdl-offsets-alist' if it doesn't already appear there."
  (interactive
   (let* ((langelem
	   (intern (completing-read
		    (concat "Syntactic symbol to change"
			    (if current-prefix-arg " or add" "")
			    ": ")
		    (mapcar
		     (function
		      (lambda (langelem)
			(cons (format "%s" (car langelem)) nil)))
		     vhdl-offsets-alist)
		    nil (not current-prefix-arg)
		    ;; initial contents tries to be the last element
		    ;; on the syntactic analysis list for the current
		    ;; line
		    (let* ((syntax (vhdl-get-syntactic-context))
			   (len (length syntax))
			   (ic (format "%s" (car (nth (1- len) syntax)))))
		      ic)
		    )))
	  (offset (vhdl-read-offset langelem)))
     (list langelem offset current-prefix-arg)))
  ;; sanity check offset
  (or (eq offset '+)
      (eq offset '-)
      (eq offset '++)
      (eq offset '--)
      (integerp offset)
      (fboundp offset)
      (boundp offset)
      (error "ERROR:  Offset must be int, func, var, or one of +, -, ++, --: %s"
	     offset))
  (let ((entry (assq symbol vhdl-offsets-alist)))
    (if entry
	(setcdr entry offset)
      (if add-p
	  (setq vhdl-offsets-alist
		(cons (cons symbol offset) vhdl-offsets-alist))
	(error "ERROR:  %s is not a valid syntactic symbol" symbol))))
  (vhdl-keep-region-active))

(defun vhdl-set-style (style &optional local)
  "Set `vhdl-mode' variables to use one of several different indentation styles.
STYLE is a string representing the desired style and optional LOCAL is
a flag which, if non-nil, means to make the style variables being
changed buffer local, instead of the default, which is to set the
global variables.  Interactively, the flag comes from the prefix
argument.  The styles are chosen from the `vhdl-style-alist' variable."
  (interactive (list (completing-read "Use which VHDL indentation style? "
				      vhdl-style-alist nil t)
		     current-prefix-arg))
  (let ((vars (cdr (assoc style vhdl-style-alist))))
    (or vars
	(error "ERROR:  Invalid VHDL indentation style `%s'" style))
    ;; set all the variables
    (mapc
     (function
      (lambda (varentry)
	(let ((var (car varentry))
	      (val (cdr varentry)))
	  ;; special case for vhdl-offsets-alist
	  (if (not (eq var 'vhdl-offsets-alist))
	      (set (if local (make-local-variable var) var) val)
	    ;; reset vhdl-offsets-alist to the default value first
	    (set (if local (make-local-variable var) var)
                 (copy-alist vhdl-offsets-alist-default))
	    ;; now set the langelems that are different
	    (mapcar
	     (function
	      (lambda (langentry)
		(let ((langelem (car langentry))
		      (offset (cdr langentry)))
		  (vhdl-set-offset langelem offset)
		  )))
	     val))
	  )))
     vars))
  (vhdl-keep-region-active))

(defun vhdl-get-offset (langelem)
  "Get offset from LANGELEM which is a cons cell of the form:
\(SYMBOL . RELPOS).  The symbol is matched against
vhdl-offsets-alist and the offset found there is either returned,
or added to the indentation at RELPOS.  If RELPOS is nil, then
the offset is simply returned."
  (let* ((symbol (car langelem))
	 (relpos (cdr langelem))
	 (match  (assq symbol vhdl-offsets-alist))
	 (offset (cdr-safe match)))
    ;; offset can be a number, a function, a variable, or one of the
    ;; symbols + or -
    (cond
     ((not match)
      (if vhdl-strict-syntax-p
	  (error "ERROR:  Don't know how to indent a %s" symbol)
	(setq offset 0
	      relpos 0)))
     ((eq offset '+)  (setq offset vhdl-basic-offset))
     ((eq offset '-)  (setq offset (- vhdl-basic-offset)))
     ((eq offset '++) (setq offset (* 2 vhdl-basic-offset)))
     ((eq offset '--) (setq offset (* 2 (- vhdl-basic-offset))))
     ((and (not (numberp offset))
	   (fboundp offset))
      (setq offset (funcall offset langelem)))
     ((not (numberp offset))
      (setq offset (eval offset)))
     )
    (+ (if (and relpos
		(< relpos (vhdl-point 'bol)))
	   (save-excursion
	     (goto-char relpos)
	     (current-column))
	 0)
       offset)))

;; Syntactic support functions:

(defun vhdl-in-comment-p ()
  "Check if point is in a comment."
  (eq (vhdl-in-literal) 'comment))

(defun vhdl-in-string-p ()
  "Check if point is in a string."
  (eq (vhdl-in-literal) 'string))

(defun vhdl-in-literal ()
  "Determine if point is in a VHDL literal."
  (save-excursion
    (let ((state (parse-partial-sexp (vhdl-point 'bol) (point))))
      (cond
       ((nth 3 state) 'string)
       ((nth 4 state) 'comment)
       ((vhdl-beginning-of-macro) 'pound)
       (t nil)))))

(defun vhdl-forward-comment (&optional direction)
  "Skip all comments (including whitespace).  Skip backwards if DIRECTION is
negative, skip forward otherwise."
  (interactive "p")
  (if (and direction (< direction 0))
      ;; skip backwards
      (progn
	(skip-chars-backward " \t\n")
	(while (re-search-backward "^[^\"-]*\\(\\(-?\"[^\"]*\"\\|-[^\"-]\\)[^\"-]*\\)*\\(--\\)" (vhdl-point 'bol) t)
	  (goto-char (match-beginning 3))
	  (skip-chars-backward " \t\n")))
    ;; skip forwards
    (skip-chars-forward " \t\n")
    (while (looking-at "--.*")
      (goto-char (match-end 0))
      (skip-chars-forward " \t\n"))))

;; XEmacs hack: work around buggy `forward-comment' in XEmacs 21.4+
(unless (and (featurep 'xemacs) (string< "21.2" emacs-version))
  (defalias 'vhdl-forward-comment 'forward-comment))

;; This is the best we can do in Win-Emacs.
(defun vhdl-win-il (&optional lim)
  "Determine if point is in a VHDL literal."
  (save-excursion
    (let* ((here (point))
	   (state nil)
	   (match nil)
	   (lim  (or lim (vhdl-point 'bod))))
      (goto-char lim )
      (while (< (point) here)
	(setq match
	      (and (re-search-forward "--\\|[\"']"
				      here 'move)
		   (buffer-substring (match-beginning 0) (match-end 0))))
	(setq state
	      (cond
	       ;; no match
	       ((null match) nil)
	       ;; looking at the opening of a VHDL style comment
	       ((string= "--" match)
		(if (<= here (progn (end-of-line) (point))) 'comment))
	       ;; looking at the opening of a double quote string
	       ((string= "\"" match)
		(if (not (save-restriction
			   ;; this seems to be necessary since the
			   ;; re-search-forward will not work without it
			   (narrow-to-region (point) here)
			   (re-search-forward
			    ;; this regexp matches a double quote
			    ;; which is preceded by an even number
			    ;; of backslashes, including zero
			    "\\([^\\]\\|^\\)\\(\\\\\\\\\\)*\"" here 'move)))
		    'string))
	       ;; looking at the opening of a single quote string
	       ((string= "'" match)
		(if (not (save-restriction
			   ;; see comments from above
			   (narrow-to-region (point) here)
			   (re-search-forward
			    ;; this matches a single quote which is
			    ;; preceded by zero or two backslashes.
			    "\\([^\\]\\|^\\)\\(\\\\\\\\\\)?'"
			    here 'move)))
		    'string))
	       (t nil)))
	) ; end-while
      state)))

(and (string-match "Win-Emacs" emacs-version)
     (fset 'vhdl-in-literal 'vhdl-win-il))

;; Skipping of "syntactic whitespace".  Syntactic whitespace is
;; defined as lexical whitespace or comments.  Search no farther back
;; or forward than optional LIM.  If LIM is omitted, (point-min) is
;; used for backward skipping, (point-max) is used for forward
;; skipping.

(defun vhdl-forward-syntactic-ws (&optional lim)
  "Forward skip of syntactic whitespace."
  (let* ((here (point-max))
	 (hugenum (point-max)))
    (while (/= here (point))
      (setq here (point))
      (vhdl-forward-comment hugenum)
      ;; skip preprocessor directives
      (when (and (eq (char-after) ?#)
		 (= (vhdl-point 'boi) (point)))
	(while (and (eq (char-before (vhdl-point 'eol)) ?\\)
		    (= (forward-line 1) 0)))
	(end-of-line)))
    (if lim (goto-char (min (point) lim)))))


;; This is the best we can do in Win-Emacs.
(defun vhdl-win-fsws (&optional lim)
  "Forward skip syntactic whitespace for Win-Emacs."
  (let ((lim (or lim (point-max)))
	stop)
    (while (not stop)
      (skip-chars-forward " \t\n\r\f" lim)
      (cond
       ;; vhdl comment
       ((looking-at "--") (end-of-line))
       ;; none of the above
       (t (setq stop t))))))

(and (string-match "Win-Emacs" emacs-version)
     (fset 'vhdl-forward-syntactic-ws 'vhdl-win-fsws))

(defun vhdl-beginning-of-macro (&optional lim)
  "Go to the beginning of a cpp macro definition (nicked from `cc-engine')."
  (let ((here (point)))
    (beginning-of-line)
    (while (eq (char-before (1- (point))) ?\\)
      (forward-line -1))
    (back-to-indentation)
    (if (and (<= (point) here)
	     (eq (char-after) ?#))
	t
      (goto-char here)
      nil)))

(defun vhdl-backward-syntactic-ws (&optional lim)
  "Backward skip over syntactic whitespace."
  (let* ((here (point-min))
	 (hugenum (- (point-max))))
    (while (/= here (point))
      (setq here (point))
      (vhdl-forward-comment hugenum)
      (vhdl-beginning-of-macro))
    (if lim (goto-char (max (point) lim)))))

;; This is the best we can do in Win-Emacs.
(defun vhdl-win-bsws (&optional lim)
  "Backward skip syntactic whitespace for Win-Emacs."
  (let ((lim (or lim (vhdl-point 'bod)))
	stop)
    (while (not stop)
      (skip-chars-backward " \t\n\r\f" lim)
      (cond
       ;; vhdl comment
       ((eq (vhdl-in-literal) 'comment)
	(skip-chars-backward "^-" lim)
	(skip-chars-backward "-" lim)
	(while (not (or (and (= (following-char) ?-)
			     (= (char-after (1+ (point))) ?-))
			(<= (point) lim)))
	  (skip-chars-backward "^-" lim)
	  (skip-chars-backward "-" lim)))
       ;; none of the above
       (t (setq stop t))))))

(and (string-match "Win-Emacs" emacs-version)
    (fset 'vhdl-backward-syntactic-ws 'vhdl-win-bsws))

;; Functions to help finding the correct indentation column:

(defun vhdl-first-word (point)
  "If the keyword at POINT is at boi, then return (current-column) at
that point, else nil."
  (save-excursion
    (and (goto-char point)
	 (eq (point) (vhdl-point 'boi))
	 (current-column))))

(defun vhdl-last-word (point)
  "If the keyword at POINT is at eoi, then return (current-column) at
that point, else nil."
  (save-excursion
    (and (goto-char point)
	 (save-excursion (or (eq (progn (forward-sexp) (point))
				 (vhdl-point 'eoi))
			     (looking-at "\\s-*\\(--\\)?")))
	 (current-column))))

;; Core syntactic evaluation functions:

(defconst vhdl-libunit-re
  "\\b\\(architecture\\|configuration\\|entity\\|package\\)\\b[^_]")

(defun vhdl-libunit-p ()
  (and
   (save-excursion
     (forward-sexp)
     (skip-chars-forward " \t\n")
     (not (looking-at "is\\b[^_]")))
   (save-excursion
     (backward-sexp)
     (and (not (looking-at "use\\b[^_]"))
	  (progn
	    (forward-sexp)
	    (vhdl-forward-syntactic-ws)
	    (/= (following-char) ?:))))
   ))

(defconst vhdl-defun-re
  "\\b\\(architecture\\|block\\|configuration\\|entity\\|package\\|process\\|procedural\\|procedure\\|function\\)\\b[^_]")

(defun vhdl-defun-p ()
  (save-excursion
    (if (looking-at "block\\|process\\|procedural")
	;; "block", "process", "procedural":
	(save-excursion
	  (backward-sexp)
	  (not (looking-at "end\\s-+\\w")))
      ;; "architecture", "configuration", "entity",
      ;; "package", "procedure", "function":
      t)))

(defun vhdl-corresponding-defun ()
  "If the word at the current position corresponds to a \"defun\"
keyword, then return a string that can be used to find the
corresponding \"begin\" keyword, else return nil."
  (save-excursion
    (and (looking-at vhdl-defun-re)
	 (vhdl-defun-p)
	 (if (looking-at "block\\|process\\|procedural")
	     ;; "block", "process". "procedural:
	     (buffer-substring (match-beginning 0) (match-end 0))
	   ;; "architecture", "configuration", "entity", "package",
	   ;; "procedure", "function":
	   "is"))))

(defconst vhdl-begin-fwd-re
  "\\b\\(is\\|begin\\|block\\|component\\|generate\\|then\\|else\\|loop\\|process\\|procedural\\|units\\|record\\|for\\)\\b\\([^_]\\|\\'\\)"
  "A regular expression for searching forward that matches all known
\"begin\" keywords.")

(defconst vhdl-begin-bwd-re
  "\\b\\(is\\|begin\\|block\\|component\\|generate\\|then\\|else\\|loop\\|process\\|procedural\\|units\\|record\\|for\\)\\b[^_]"
  "A regular expression for searching backward that matches all known
\"begin\" keywords.")

(defun vhdl-begin-p (&optional lim)
  "Return t if we are looking at a real \"begin\" keyword.
Assumes that the caller will make sure that we are looking at
vhdl-begin-fwd-re, and are not inside a literal, and that we are not in
the middle of an identifier that just happens to contain a \"begin\"
keyword."
  (cond
   ;; "[architecture|case|configuration|entity|package|
   ;;   procedure|function] ... is":
   ((and (looking-at "i")
	 (save-excursion
	   ;; Skip backward over first sexp (needed to skip over a
	   ;; procedure interface list, and is harmless in other
	   ;; situations).  Note that we need "return" in the
	   ;; following search list so that we don't run into
	   ;; semicolons in the function interface list.
	   (backward-sexp)
	   (let (foundp)
	     (while (and (not foundp)
			 (re-search-backward
			  ";\\|\\b\\(architecture\\|case\\|configuration\\|entity\\|package\\|procedure\\|return\\|is\\|begin\\|process\\|procedural\\|block\\)\\b[^_]"
			  lim 'move))
	       (if (or (= (preceding-char) ?_)
		       (vhdl-in-literal))
		   (backward-char)
		 (setq foundp t))))
	   (and (/= (following-char) ?\;)
		(not (looking-at "is\\|begin\\|process\\|procedural\\|block")))))
    t)
   ;; "begin", "then":
   ((looking-at "be\\|t")
    t)
   ;; "else":
   ((and (looking-at "e")
	 ;; make sure that the "else" isn't inside a
	 ;; conditional signal assignment.
	 (save-excursion
	   (re-search-backward ";\\|\\bwhen\\b[^_]" lim 'move)
	   (or (eq (following-char) ?\;)
	       (eq (point) lim))))
    t)
   ;; "block", "generate", "loop", "process", "procedural",
   ;; "units", "record":
   ((and (looking-at "bl\\|[glpur]")
	 (save-excursion
	   (backward-sexp)
	   (not (looking-at "end\\s-+\\w"))))
    t)
   ;; "component":
   ((and (looking-at "c")
	 (save-excursion
	   (backward-sexp)
	   (not (looking-at "end\\s-+\\w")))
	 ;; look out for the dreaded entity class in an attribute
	 (save-excursion
	   (vhdl-backward-syntactic-ws lim)
	   (/= (preceding-char) ?:)))
    t)
   ;; "for" (inside configuration declaration):
   ((and (looking-at "f")
	 (save-excursion
	   (backward-sexp)
	   (not (looking-at "end\\s-+\\w")))
	 (vhdl-has-syntax 'configuration))
    t)
   ))

(defun vhdl-corresponding-mid (&optional lim)
  (cond
   ((looking-at "is\\|block\\|generate\\|process\\|procedural")
    "begin")
   ((looking-at "then")
    "<else>")
   (t
    "end")))

(defun vhdl-corresponding-end (&optional lim)
  "If the word at the current position corresponds to a \"begin\"
keyword, then return a vector containing enough information to find
the corresponding \"end\" keyword, else return nil.  The keyword to
search forward for is aref 0.  The column in which the keyword must
appear is aref 1 or nil if any column is suitable.
Assumes that the caller will make sure that we are not in the middle
of an identifier that just happens to contain a \"begin\" keyword."
  (save-excursion
    (and (looking-at vhdl-begin-fwd-re)
	 (/= (preceding-char) ?_)
	 (not (vhdl-in-literal))
	 (vhdl-begin-p lim)
	 (cond
	  ;; "is", "generate", "loop":
	  ((looking-at "[igl]")
	   (vector "end"
		   (and (vhdl-last-word (point))
			(or (vhdl-first-word (point))
			    (save-excursion
			      (vhdl-beginning-of-statement-1 lim)
			      (vhdl-backward-skip-label lim)
			      (vhdl-first-word (point)))))))
	  ;; "begin", "else", "for":
	  ((looking-at "be\\|[ef]")
	   (vector "end"
		   (and (vhdl-last-word (point))
			(or (vhdl-first-word (point))
			    (save-excursion
			      (vhdl-beginning-of-statement-1 lim)
			      (vhdl-backward-skip-label lim)
			      (vhdl-first-word (point)))))))
	  ;; "component", "units", "record":
	  ((looking-at "[cur]")
	   ;; The first end found will close the block
	   (vector "end" nil))
	  ;; "block", "process", "procedural":
	  ((looking-at "bl\\|p")
	   (vector "end"
		   (or (vhdl-first-word (point))
		       (save-excursion
			 (vhdl-beginning-of-statement-1 lim)
			 (vhdl-backward-skip-label lim)
			 (vhdl-first-word (point))))))
	  ;; "then":
	  ((looking-at "t")
	   (vector "elsif\\|else\\|end\\s-+if"
		   (and (vhdl-last-word (point))
			(or (vhdl-first-word (point))
			    (save-excursion
			      (vhdl-beginning-of-statement-1 lim)
			      (vhdl-backward-skip-label lim)
			      (vhdl-first-word (point)))))))
	  ))))

(defconst vhdl-end-fwd-re "\\b\\(end\\|else\\|elsif\\)\\b\\([^_]\\|\\'\\)")

(defconst vhdl-end-bwd-re "\\b\\(end\\|else\\|elsif\\)\\b[^_]")

(defun vhdl-end-p (&optional lim)
  "Return t if we are looking at a real \"end\" keyword.
Assumes that the caller will make sure that we are looking at
vhdl-end-fwd-re, and are not inside a literal, and that we are not in
the middle of an identifier that just happens to contain an \"end\"
keyword."
  (or (not (looking-at "else"))
      ;; make sure that the "else" isn't inside a conditional signal
      ;; assignment.
      (save-excursion
	(re-search-backward ";\\|\\bwhen\\b[^_]" lim 'move)
	(or (eq (following-char) ?\;)
	    (eq (point) lim)
	    (vhdl-in-literal)))))

(defun vhdl-corresponding-begin (&optional lim)
  "If the word at the current position corresponds to an \"end\"
keyword, then return a vector containing enough information to find
the corresponding \"begin\" keyword, else return nil.  The keyword to
search backward for is aref 0.  The column in which the keyword must
appear is aref 1 or nil if any column is suitable.  The supplementary
keyword to search forward for is aref 2 or nil if this is not
required.  If aref 3 is t, then the \"begin\" keyword may be found in
the middle of a statement.
Assumes that the caller will make sure that we are not in the middle
of an identifier that just happens to contain an \"end\" keyword."
  (save-excursion
    (let (pos)
      (if (and (looking-at vhdl-end-fwd-re)
	       (not (vhdl-in-literal))
	       (vhdl-end-p lim))
	  (if (looking-at "el")
	      ;; "else", "elsif":
	      (vector "if\\|elsif" (vhdl-first-word (point)) "then" nil)
	    ;; "end ...":
	    (setq pos (point))
	    (forward-sexp)
	    (skip-chars-forward " \t\n")
	    (cond
	     ;; "end if":
	     ((looking-at "if\\b[^_]")
	      (vector "else\\|elsif\\|if"
		      (vhdl-first-word pos)
		      "else\\|then" nil))
	     ;; "end component":
	     ((looking-at "component\\b[^_]")
	      (vector (buffer-substring (match-beginning 1)
					(match-end 1))
		      (vhdl-first-word pos)
		      nil nil))
	     ;; "end units", "end record":
	     ((looking-at "\\(units\\|record\\)\\b[^_]")
	      (vector (buffer-substring (match-beginning 1)
					(match-end 1))
		      (vhdl-first-word pos)
		      nil t))
	     ;; "end block", "end process", "end procedural":
	     ((looking-at "\\(block\\|process\\|procedural\\)\\b[^_]")
	      (vector "begin" (vhdl-first-word pos) nil nil))
	     ;; "end case":
	     ((looking-at "case\\b[^_]")
	      (vector "case" (vhdl-first-word pos) "is" nil))
	     ;; "end generate":
	     ((looking-at "generate\\b[^_]")
	      (vector "generate\\|for\\|if"
		      (vhdl-first-word pos)
		      "generate" nil))
	     ;; "end loop":
	     ((looking-at "loop\\b[^_]")
	      (vector "loop\\|while\\|for"
		      (vhdl-first-word pos)
		      "loop" nil))
	     ;; "end for" (inside configuration declaration):
	     ((looking-at "for\\b[^_]")
	      (vector "for" (vhdl-first-word pos) nil nil))
	     ;; "end [id]":
	     (t
	      (vector "begin\\|architecture\\|configuration\\|entity\\|package\\|procedure\\|function"
		      (vhdl-first-word pos)
		      ;; return an alist of (statement . keyword) mappings
		      '(
			;; "begin ... end [id]":
			("begin"	  . nil)
			;; "architecture ... is ... begin ... end [id]":
			("architecture"	  . "is")
			;; "configuration ... is ... end [id]":
			("configuration"  . "is")
			;; "entity ... is ... end [id]":
			("entity"	  . "is")
			;; "package ... is ... end [id]":
			("package"	  . "is")
			;; "procedure ... is ... begin ... end [id]":
			("procedure"      . "is")
			;; "function ... is ... begin ... end [id]":
			("function"       . "is")
			)
		      nil))
	     ))) ; "end ..."
      )))

(defconst vhdl-leader-re
  "\\b\\(block\\|component\\|process\\|procedural\\|for\\)\\b[^_]")

(defun vhdl-end-of-leader ()
  (save-excursion
    (cond ((looking-at "block\\|process\\|procedural")
	   (if (save-excursion
		 (forward-sexp)
		 (skip-chars-forward " \t\n")
		 (= (following-char) ?\())
	       (forward-sexp 2)
	     (forward-sexp))
	   (when (looking-at "[ \t\n]*is")
	     (goto-char (match-end 0)))
	   (point))
	  ((looking-at "component")
	   (forward-sexp 2)
	   (when (looking-at "[ \t\n]*is")
	     (goto-char (match-end 0)))
	   (point))
	  ((looking-at "for")
	   (forward-sexp 2)
	   (skip-chars-forward " \t\n")
	   (while (looking-at "[,:(]")
	     (forward-sexp)
	     (skip-chars-forward " \t\n"))
	   (point))
	  (t nil)
	  )))

(defconst vhdl-trailer-re
  "\\b\\(is\\|then\\|generate\\|loop\\|record\\)\\b[^_]")

(defconst vhdl-statement-fwd-re
  "\\b\\(if\\|for\\|while\\)\\b\\([^_]\\|\\'\\)"
  "A regular expression for searching forward that matches all known
\"statement\" keywords.")

(defconst vhdl-statement-bwd-re
  "\\b\\(if\\|for\\|while\\)\\b[^_]"
  "A regular expression for searching backward that matches all known
\"statement\" keywords.")

(defun vhdl-statement-p (&optional lim)
  "Return t if we are looking at a real \"statement\" keyword.
Assumes that the caller will make sure that we are looking at
vhdl-statement-fwd-re, and are not inside a literal, and that we are not
in the middle of an identifier that just happens to contain a
\"statement\" keyword."
  (cond
   ;; "for" ... "generate":
   ((and (looking-at "f")
	 ;; Make sure it's the start of a parameter specification.
	 (save-excursion
	   (forward-sexp 2)
	   (skip-chars-forward " \t\n")
	   (looking-at "in\\b[^_]"))
	 ;; Make sure it's not an "end for".
	 (save-excursion
	   (backward-sexp)
	   (not (looking-at "end\\s-+\\w"))))
    t)
   ;; "if" ... "then", "if" ... "generate", "if" ... "loop":
   ((and (looking-at "i")
	 ;; Make sure it's not an "end if".
	 (save-excursion
	   (backward-sexp)
	   (not (looking-at "end\\s-+\\w"))))
    t)
   ;; "while" ... "loop":
   ((looking-at "w")
    t)
   ))

(defconst vhdl-case-alternative-re "when[( \t\n][^;=>]+=>"
  "Regexp describing a case statement alternative key.")

(defun vhdl-case-alternative-p (&optional lim)
  "Return t if we are looking at a real case alternative.
Assumes that the caller will make sure that we are looking at
vhdl-case-alternative-re, and are not inside a literal, and that
we are not in the middle of an identifier that just happens to
contain a \"when\" keyword."
  (save-excursion
    (let (foundp)
      (while (and (not foundp)
		  (re-search-backward ";\\|<=" lim 'move))
	(if (or (= (preceding-char) ?_)
		(vhdl-in-literal))
	    (backward-char)
	  (setq foundp t)))
      (or (eq (following-char) ?\;)
	  (eq (point) lim)))
    ))

;; Core syntactic movement functions:

(defconst vhdl-b-t-b-re
  (concat vhdl-begin-bwd-re "\\|" vhdl-end-bwd-re))

(defun vhdl-backward-to-block (&optional lim)
  "Move backward to the previous \"begin\" or \"end\" keyword."
  (let (foundp)
    (while (and (not foundp)
		(re-search-backward vhdl-b-t-b-re lim 'move))
      (if (or (= (preceding-char) ?_)
	      (vhdl-in-literal))
	  (backward-char)
	(cond
	 ;; "begin" keyword:
	 ((and (looking-at vhdl-begin-fwd-re)
	       (/= (preceding-char) ?_)
	       (vhdl-begin-p lim))
	  (setq foundp 'begin))
	 ;; "end" keyword:
	 ((and (looking-at vhdl-end-fwd-re)
	       (/= (preceding-char) ?_)
	       (vhdl-end-p lim))
	  (setq foundp 'end))
	 ))
      )
    foundp
    ))

(defun vhdl-forward-sexp (&optional count lim)
  "Move forward across one balanced expression (sexp).
With COUNT, do it that many times."
  (interactive "p")
  (let ((count (or count 1))
	(case-fold-search t)
	end-vec target)
    (save-excursion
      (while (> count 0)
	;; skip whitespace
	(skip-chars-forward " \t\n")
	;; Check for an unbalanced "end" keyword
	(if (and (looking-at vhdl-end-fwd-re)
		 (/= (preceding-char) ?_)
		 (not (vhdl-in-literal))
		 (vhdl-end-p lim)
		 (not (looking-at "else")))
	    (error
	     "ERROR:  Containing expression ends prematurely in vhdl-forward-sexp"))
	;; If the current keyword is a "begin" keyword, then find the
	;; corresponding "end" keyword.
	(if (setq end-vec (vhdl-corresponding-end lim))
	    (let (
		  ;; end-re is the statement keyword to search for
		  (end-re
		   (concat "\\b\\(" (aref end-vec 0) "\\)\\b\\([^_]\\|\\'\\)"))
		  ;; column is either the statement keyword target column
		  ;; or nil
		  (column (aref end-vec 1))
		  (eol (vhdl-point 'eol))
		  foundp literal placeholder)
	      ;; Look for the statement keyword.
	      (while (and (not foundp)
			  (re-search-forward end-re nil t)
			  (setq placeholder (match-end 1))
			  (goto-char (match-beginning 0)))
		;; If we are in a literal, or not in the right target
		;; column and not on the same line as the begin, then
		;; try again.
		(if (or (and column
			     (/= (current-indentation) column)
			     (> (point) eol))
			(= (preceding-char) ?_)
			(setq literal (vhdl-in-literal)))
		    (if (eq literal 'comment)
			(end-of-line)
		      (forward-char))
		  ;; An "else" keyword corresponds to both the opening brace
		  ;; of the following sexp and the closing brace of the
		  ;; previous sexp.
		  (if (not (looking-at "else"))
		      (goto-char placeholder))
		  (setq foundp t))
		)
	      (if (not foundp)
		  (error "ERROR:  Unbalanced keywords in vhdl-forward-sexp"))
	      )
	  ;; If the current keyword is not a "begin" keyword, then just
	  ;; perform the normal forward-sexp.
	  (forward-sexp)
	  )
	(setq count (1- count))
	)
      (setq target (point)))
    (goto-char target)
    nil))

(defun vhdl-backward-sexp (&optional count lim)
  "Move backward across one balanced expression (sexp).
With COUNT, do it that many times.  LIM bounds any required backward
searches."
  (interactive "p")
  (let ((count (or count 1))
	(case-fold-search t)
	begin-vec target)
    (save-excursion
      (while (> count 0)
	;; Perform the normal backward-sexp, unless we are looking at
	;; "else" - an "else" keyword corresponds to both the opening brace
	;; of the following sexp and the closing brace of the previous sexp.
	(if (and (looking-at "else\\b\\([^_]\\|\\'\\)")
		 (/= (preceding-char) ?_)
		 (not (vhdl-in-literal)))
	    nil
	  (backward-sexp)
	  (if (and (looking-at vhdl-begin-fwd-re)
		   (/= (preceding-char) ?_)
		   (not (vhdl-in-literal))
		   (vhdl-begin-p lim))
	      (error "ERROR:  Containing expression ends prematurely in vhdl-backward-sexp")))
	;; If the current keyword is an "end" keyword, then find the
	;; corresponding "begin" keyword.
	(if (and (setq begin-vec (vhdl-corresponding-begin lim))
		 (/= (preceding-char) ?_))
	    (let (
		  ;; begin-re is the statement keyword to search for
		  (begin-re
		   (concat "\\b\\(" (aref begin-vec 0) "\\)\\b[^_]"))
		  ;; column is either the statement keyword target column
		  ;; or nil
		  (column (aref begin-vec 1))
		  ;; internal-p controls where the statement keyword can
		  ;; be found.
		  (internal-p (aref begin-vec 3))
		  (last-backward (point)) last-forward
		  foundp literal keyword)
	      ;; Look for the statement keyword.
	      (while (and (not foundp)
			  (re-search-backward begin-re lim t)
			  (setq keyword
				(buffer-substring (match-beginning 1)
						  (match-end 1))))
		;; If we are in a literal or in the wrong column,
		;; then try again.
		(if (or (and column
			     (and (/= (current-indentation) column)
				  ;; possibly accept current-column as
				  ;; well as current-indentation.
				  (or (not internal-p)
				      (/= (current-column) column))))
			(= (preceding-char) ?_)
			(vhdl-in-literal))
		    (backward-char)
		  ;; If there is a supplementary keyword, then
		  ;; search forward for it.
		  (if (and (setq begin-re (aref begin-vec 2))
			   (or (not (listp begin-re))
			       ;; If begin-re is an alist, then find the
			       ;; element corresponding to the actual
			       ;; keyword that we found.
			       (progn
				 (setq begin-re
				       (assoc keyword begin-re))
				 (and begin-re
				      (setq begin-re (cdr begin-re))))))
		      (and
		       (setq begin-re
			     (concat "\\b\\(" begin-re "\\)\\b[^_]"))
		       (save-excursion
			 (setq last-forward (point))
			 ;; Look for the supplementary keyword
			 ;; (bounded by the backward search start
			 ;; point).
			 (while (and (not foundp)
				     (re-search-forward begin-re
							last-backward t)
				     (goto-char (match-beginning 1)))
			   ;; If we are in a literal, then try again.
			   (if (or (= (preceding-char) ?_)
				   (setq literal
					 (vhdl-in-literal)))
			       (if (eq literal 'comment)
				   (goto-char
				    (min (vhdl-point 'eol) last-backward))
				 (forward-char))
			     ;; We have found the supplementary keyword.
			     ;; Save the position of the keyword in foundp.
			     (setq foundp (point)))
			   )
			 foundp)
		       ;; If the supplementary keyword was found, then
		       ;; move point to the supplementary keyword.
		       (goto-char foundp))
		    ;; If there was no supplementary keyword, then
		    ;; point is already at the statement keyword.
		    (setq foundp t)))
		) ; end of the search for the statement keyword
	      (if (not foundp)
		  (error "ERROR:  Unbalanced keywords in vhdl-backward-sexp"))
	      ))
	(setq count (1- count))
	)
      (setq target (point)))
    (goto-char target)
    nil))

(defun vhdl-backward-up-list (&optional count limit)
  "Move backward out of one level of blocks.
With argument, do this that many times."
  (interactive "p")
  (let ((count (or count 1))
	target)
    (save-excursion
      (while (> count 0)
	(if (looking-at vhdl-defun-re)
	    (error "ERROR:  Unbalanced blocks"))
	(vhdl-backward-to-block limit)
	(setq count (1- count)))
      (setq target (point)))
    (goto-char target)))

(defun vhdl-end-of-defun (&optional count)
  "Move forward to the end of a VHDL defun."
  (interactive)
  (let ((case-fold-search t))
    (vhdl-beginning-of-defun)
    (if (not (looking-at "block\\|process\\|procedural"))
	(re-search-forward "\\bis\\b"))
    (vhdl-forward-sexp)))

(defun vhdl-mark-defun ()
  "Put mark at end of this \"defun\", point at beginning."
  (interactive)
  (let ((case-fold-search t))
    (push-mark)
    (vhdl-beginning-of-defun)
    (push-mark)
    (if (not (looking-at "block\\|process\\|procedural"))
	(re-search-forward "\\bis\\b"))
    (vhdl-forward-sexp)
    (exchange-point-and-mark)))

(defun vhdl-beginning-of-libunit ()
  "Move backward to the beginning of a VHDL library unit.
Returns the location of the corresponding begin keyword, unless search
stops due to beginning or end of buffer.
Note that if point is between the \"libunit\" keyword and the
corresponding \"begin\" keyword, then that libunit will not be
recognized, and the search will continue backwards.  If point is
at the \"begin\" keyword, then the defun will be recognized.  The
returned point is at the first character of the \"libunit\" keyword."
  (let ((last-forward (point))
	(last-backward
	 ;; Just in case we are actually sitting on the "begin"
	 ;; keyword, allow for the keyword and an extra character,
	 ;; as this will be used when looking forward for the
	 ;; "begin" keyword.
	 (save-excursion (forward-word 1) (1+ (point))))
	foundp literal placeholder)
    ;; Find the "libunit" keyword.
    (while (and (not foundp)
		(re-search-backward vhdl-libunit-re nil 'move))
      ;; If we are in a literal, or not at a real libunit, then try again.
      (if (or (= (preceding-char) ?_)
	      (vhdl-in-literal)
	      (not (vhdl-libunit-p)))
	  (backward-char)
	;; Find the corresponding "begin" keyword.
	(setq last-forward (point))
	(while (and (not foundp)
		    (re-search-forward "\\bis\\b[^_]" last-backward t)
		    (setq placeholder (match-beginning 0)))
	  (if (or (= (preceding-char) ?_)
		  (setq literal (vhdl-in-literal)))
	      ;; It wasn't a real keyword, so keep searching.
	      (if (eq literal 'comment)
		  (goto-char
		   (min (vhdl-point 'eol) last-backward))
		(forward-char))
	    ;; We have found the begin keyword, loop will exit.
	    (setq foundp placeholder)))
	;; Go back to the libunit keyword
	(goto-char last-forward)))
    foundp))

(defun vhdl-beginning-of-defun (&optional count)
  "Move backward to the beginning of a VHDL defun.
With argument, do it that many times.
Returns the location of the corresponding begin keyword, unless search
stops due to beginning or end of buffer."
  ;; Note that if point is between the "defun" keyword and the
  ;; corresponding "begin" keyword, then that defun will not be
  ;; recognized, and the search will continue backwards.  If point is
  ;; at the "begin" keyword, then the defun will be recognized.  The
  ;; returned point is at the first character of the "defun" keyword.
  (interactive "p")
  (let ((count (or count 1))
	(case-fold-search t)
	(last-forward (point))
	foundp)
    (while (> count 0)
      (setq foundp nil)
      (goto-char last-forward)
      (let ((last-backward
	     ;; Just in case we are actually sitting on the "begin"
	     ;; keyword, allow for the keyword and an extra character,
	     ;; as this will be used when looking forward for the
	     ;; "begin" keyword.
	     (save-excursion (forward-word 1) (1+ (point))))
	    begin-string literal)
	(while (and (not foundp)
		    (re-search-backward vhdl-defun-re nil 'move))
	  ;; If we are in a literal, then try again.
	  (if (or (= (preceding-char) ?_)
		  (vhdl-in-literal))
	      (backward-char)
	    (if (setq begin-string (vhdl-corresponding-defun))
		;; This is a real defun keyword.
		;; Find the corresponding "begin" keyword.
		;; Look for the begin keyword.
		(progn
		  ;; Save the search start point.
		  (setq last-forward (point))
		  (while (and (not foundp)
			      (search-forward begin-string last-backward t))
		    (if (or (= (preceding-char) ?_)
			    (save-match-data
			      (setq literal (vhdl-in-literal))))
			;; It wasn't a real keyword, so keep searching.
			(if (eq literal 'comment)
			    (goto-char
			     (min (vhdl-point 'eol) last-backward))
			  (forward-char))
		      ;; We have found the begin keyword, loop will exit.
		      (setq foundp (match-beginning 0)))
		    )
		  ;; Go back to the defun keyword
		  (goto-char last-forward)) ; end search for begin keyword
	      ))
	  ) ; end of the search for the defun keyword
	)
      (setq count (1- count))
      )
    (vhdl-keep-region-active)
    foundp))

(defun vhdl-beginning-of-statement (&optional count lim interactive)
  "Go to the beginning of the innermost VHDL statement.
With prefix arg, go back N - 1 statements.  If already at the
beginning of a statement then go to the beginning of the preceding
one.  If within a string or comment, or next to a comment (only
whitespace between), move by sentences instead of statements.

When called from a program, this function takes 3 optional args: the
prefix arg, a buffer position limit which is the farthest back to
search, and an argument indicating an interactive call."
  (interactive "p\np")
  (let ((count (or count 1))
	(case-fold-search t)
	(lim (or lim (point-min)))
	(here (point))
	state)
    (save-excursion
      (goto-char lim)
      (setq state (parse-partial-sexp (point) here nil nil)))
    (if (and interactive
	     (or (nth 3 state)
		 (nth 4 state)
		 (looking-at (concat "[ \t]*" comment-start-skip))))
	(forward-sentence (- count))
      (while (> count 0)
	(vhdl-beginning-of-statement-1 lim)
	(setq count (1- count))))
    ;; its possible we've been left up-buf of lim
    (goto-char (max (point) lim))
    )
  (vhdl-keep-region-active))

(defconst vhdl-e-o-s-re
  (concat ";\\|" vhdl-begin-fwd-re "\\|" vhdl-statement-fwd-re))

(defun vhdl-end-of-statement ()
  "Very simple implementation."
  (interactive)
  (re-search-forward vhdl-e-o-s-re))

(defconst vhdl-b-o-s-re
  (concat ";\\|\(\\|\)\\|\\bwhen\\b[^_]\\|"
	  vhdl-begin-bwd-re "\\|" vhdl-statement-bwd-re))

(defun vhdl-beginning-of-statement-1 (&optional lim)
  "Move to the start of the current statement, or the previous
statement if already at the beginning of one."
  (let ((lim (or lim (point-min)))
	(here (point))
	(pos (point))
	donep)
    ;; go backwards one balanced expression, but be careful of
    ;; unbalanced paren being reached
    (if (not (vhdl-safe (progn (backward-sexp) t)))
	(progn
	  (backward-up-list 1)
	  (forward-char)
	  (vhdl-forward-syntactic-ws here)
	  (setq donep t)))
    (while (and (not donep)
		(not (bobp))
		;; look backwards for a statement boundary
		(re-search-backward vhdl-b-o-s-re lim 'move))
      (if (or (= (preceding-char) ?_)
	      (vhdl-in-literal))
	  (backward-char)
	(cond
	 ;; If we are looking at an open paren, then stop after it
	 ((eq (following-char) ?\()
	  (forward-char)
	  (vhdl-forward-syntactic-ws here)
	  (setq donep t))
	 ;; If we are looking at a close paren, then skip it
	 ((eq (following-char) ?\))
	  (forward-char)
	  (setq pos (point))
	  (backward-sexp)
	  (if (< (point) lim)
	      (progn (goto-char pos)
		     (vhdl-forward-syntactic-ws here)
		     (setq donep t))))
	 ;; If we are looking at a semicolon, then stop
	 ((eq (following-char) ?\;)
	  (progn
	    (forward-char)
	    (vhdl-forward-syntactic-ws here)
	    (setq donep t)))
	 ;; If we are looking at a "begin", then stop
	 ((and (looking-at vhdl-begin-fwd-re)
	       (/= (preceding-char) ?_)
	       (vhdl-begin-p nil))
	  ;; If it's a leader "begin", then find the
	  ;; right place
	  (if (looking-at vhdl-leader-re)
	      (save-excursion
		;; set a default stop point at the begin
		(setq pos (point))
		;; is the start point inside the leader area ?
		(goto-char (vhdl-end-of-leader))
		(vhdl-forward-syntactic-ws here)
		(if (< (point) here)
		    ;; start point was not inside leader area
		    ;; set stop point at word after leader
		    (setq pos (point))))
	    (forward-word 1)
	    (vhdl-forward-syntactic-ws here)
	    (setq pos (point)))
	  (goto-char pos)
	  (setq donep t))
	 ;; If we are looking at a "statement", then stop
	 ((and (looking-at vhdl-statement-fwd-re)
	       (/= (preceding-char) ?_)
	       (vhdl-statement-p nil))
	  (setq donep t))
	 ;; If we are looking at a case alternative key, then stop
	 ((and (looking-at vhdl-case-alternative-re)
	       (vhdl-case-alternative-p lim))
	  (save-excursion
	    ;; set a default stop point at the when
	    (setq pos (point))
	    ;; is the start point inside the case alternative key ?
	    (looking-at vhdl-case-alternative-re)
	    (goto-char (match-end 0))
	    (vhdl-forward-syntactic-ws here)
	    (if (< (point) here)
		;; start point was not inside the case alternative key
		;; set stop point at word after case alternative keyleader
		(setq pos (point))))
	  (goto-char pos)
	  (setq donep t))
	 ;; Bogus find, continue
	 (t
	  (backward-char)))))
    ))

;; Defuns for calculating the current syntactic state:

(defun vhdl-get-library-unit (bod placeholder)
  "If there is an enclosing library unit at BOD, with its \"begin\"
keyword at PLACEHOLDER, then return the library unit type."
  (let ((here (vhdl-point 'bol)))
    (if (save-excursion
	  (goto-char placeholder)
	  (vhdl-safe (vhdl-forward-sexp 1 bod))
	  (<= here (point)))
	(save-excursion
	  (goto-char bod)
	  (cond
	   ((looking-at "e") 'entity)
	   ((looking-at "a") 'architecture)
	   ((looking-at "c") 'configuration)
	   ((looking-at "p")
	    (save-excursion
	      (goto-char bod)
	      (forward-sexp)
	      (vhdl-forward-syntactic-ws here)
	      (if (looking-at "body\\b[^_]")
		  'package-body 'package))))))
    ))

(defun vhdl-get-block-state (&optional lim)
  "Finds and records all the closest opens.
LIM is the furthest back we need to search (it should be the
previous libunit keyword)."
  (let ((here (point))
	(lim (or lim (point-min)))
	keyword sexp-start sexp-mid sexp-end
	preceding-sexp containing-sexp
	containing-begin containing-mid containing-paren)
    (save-excursion
      ;; Find the containing-paren, and use that as the limit
      (if (setq containing-paren
		(save-restriction
		  (narrow-to-region lim (point))
		  (vhdl-safe (scan-lists (point) -1 1))))
	  (setq lim containing-paren))
      ;; Look backwards for "begin" and "end" keywords.
      (while (and (> (point) lim)
		  (not containing-sexp))
	(setq keyword (vhdl-backward-to-block lim))
	(cond
	 ((eq keyword 'begin)
	  ;; Found a "begin" keyword
	  (setq sexp-start (point))
	  (setq sexp-mid (vhdl-corresponding-mid lim))
	  (setq sexp-end (vhdl-safe
			  (save-excursion
			    (vhdl-forward-sexp 1 lim) (point))))
	  (if (and sexp-end (<= sexp-end here))
	      ;; we want to record this sexp, but we only want to
	      ;; record the last-most of any of them before here
	      (or preceding-sexp
		  (setq preceding-sexp sexp-start))
	    ;; we're contained in this sexp so put sexp-start on
	    ;; front of list
	    (setq containing-sexp sexp-start)
	    (setq containing-mid sexp-mid)
	    (setq containing-begin t)))
	 ((eq keyword 'end)
	  ;; Found an "end" keyword
	  (forward-sexp)
	  (setq sexp-end (point))
	  (setq sexp-mid nil)
	  (setq sexp-start
		(or (vhdl-safe (vhdl-backward-sexp 1 lim) (point))
		    (progn (backward-sexp) (point))))
	  ;; we want to record this sexp, but we only want to
	  ;; record the last-most of any of them before here
	  (or preceding-sexp
	      (setq preceding-sexp sexp-start)))
	 )))
    ;; Check if the containing-paren should be the containing-sexp
    (if (and containing-paren
	     (or (null containing-sexp)
		 (< containing-sexp containing-paren)))
	(setq containing-sexp containing-paren
	      preceding-sexp nil
	      containing-begin nil
	      containing-mid nil))
    (vector containing-sexp preceding-sexp containing-begin containing-mid)
    ))


(defconst vhdl-s-c-a-re
  (concat vhdl-case-alternative-re "\\|" vhdl-case-header-key))

(defun vhdl-skip-case-alternative (&optional lim)
  "Skip forward over case/when bodies, with optional maximal
limit.  If no next case alternative is found, nil is returned and
point is not moved."
  (let ((lim (or lim (point-max)))
	(here (point))
	donep foundp)
    (while (and (< (point) lim)
		(not donep))
      (if (and (re-search-forward vhdl-s-c-a-re lim 'move)
	       (save-match-data
		 (not (vhdl-in-literal)))
	       (/= (match-beginning 0) here))
	  (progn
	    (goto-char (match-beginning 0))
	    (cond
	     ((and (looking-at "case")
		   (re-search-forward "\\bis[^_]" lim t))
	      (backward-sexp)
	      (vhdl-forward-sexp))
	     (t
	      (setq donep t
		    foundp t))))))
    (if (not foundp)
	(goto-char here))
    foundp))

(defun vhdl-backward-skip-label (&optional lim)
  "Skip backward over a label, with optional maximal
limit.  If label is not found, nil is returned and point
is not moved."
  (let ((lim (or lim (point-min)))
	placeholder)
    (if (save-excursion
	  (vhdl-backward-syntactic-ws lim)
	  (and (eq (preceding-char) ?:)
	       (progn
		 (backward-sexp)
		 (setq placeholder (point))
		 (looking-at vhdl-label-key))))
	(goto-char placeholder))
    ))

(defun vhdl-forward-skip-label (&optional lim)
  "Skip forward over a label, with optional maximal
limit.  If label is not found, nil is returned and point
is not moved."
  (let ((lim (or lim (point-max))))
    (if (looking-at vhdl-label-key)
	(progn
	  (goto-char (match-end 0))
	  (vhdl-forward-syntactic-ws lim)))
    ))

(defun vhdl-get-syntactic-context ()
  "Guess the syntactic description of the current line of VHDL code."
  (save-excursion
    (save-restriction
      (beginning-of-line)
      (let* ((indent-point (point))
	     (case-fold-search t)
	     vec literal containing-sexp preceding-sexp
	     containing-begin containing-mid containing-leader
	     char-before-ip char-after-ip begin-after-ip end-after-ip
	     placeholder lim library-unit
	    )

	;; Reset the syntactic context
	(setq vhdl-syntactic-context nil)

	(save-excursion
	  ;; Move to the start of the previous library unit, and
	  ;; record the position of the "begin" keyword.
	  (setq placeholder (vhdl-beginning-of-libunit))
	  ;; The position of the "libunit" keyword gives us a gross
	  ;; limit point.
	  (setq lim (point))
	  )

	;; If there is a previous library unit, and we are enclosed by
	;; it, then set the syntax accordingly.
	(and placeholder
	     (setq library-unit (vhdl-get-library-unit lim placeholder))
	     (vhdl-add-syntax library-unit lim))

	;; Find the surrounding state.
	(if (setq vec (vhdl-get-block-state lim))
	    (progn
	      (setq containing-sexp (aref vec 0))
	      (setq preceding-sexp (aref vec 1))
	      (setq containing-begin (aref vec 2))
	      (setq containing-mid (aref vec 3))
	      ))

	;; set the limit on the farthest back we need to search
	(setq lim (if containing-sexp
		      (save-excursion
			(goto-char containing-sexp)
			;; set containing-leader if required
			(if (looking-at vhdl-leader-re)
			    (setq containing-leader (vhdl-end-of-leader)))
			(vhdl-point 'bol))
		    (point-min)))

	;; cache char before and after indent point, and move point to
	;; the most likely position to perform the majority of tests
	(goto-char indent-point)
	(skip-chars-forward " \t")
	(setq literal (vhdl-in-literal))
	(setq char-after-ip (following-char))
	(setq begin-after-ip (and
			      (not literal)
			      (looking-at vhdl-begin-fwd-re)
			      (vhdl-begin-p)))
	(setq end-after-ip (and
			    (not literal)
			    (looking-at vhdl-end-fwd-re)
			    (vhdl-end-p)))
	(vhdl-backward-syntactic-ws lim)
	(setq char-before-ip (preceding-char))
	(goto-char indent-point)
	(skip-chars-forward " \t")

	;; now figure out syntactic qualities of the current line
	(cond
	 ;; CASE 1: in a string or comment.
	 ((memq literal '(string comment))
	  (vhdl-add-syntax literal (vhdl-point 'bopl)))
	 ;; CASE 2: Line is at top level.
	 ((null containing-sexp)
	  ;; Find the point to which indentation will be relative
	  (save-excursion
	    (if (null preceding-sexp)
		;; CASE 2X.1
		;; no preceding-sexp -> use the preceding statement
		(vhdl-beginning-of-statement-1 lim)
	      ;; CASE 2X.2
	      ;; if there is a preceding-sexp then indent relative to it
	      (goto-char preceding-sexp)
	      ;; if not at boi, then the block-opening keyword is
	      ;; probably following a label, so we need a different
	      ;; relpos
	      (if (/= (point) (vhdl-point 'boi))
		  ;; CASE 2X.3
		  (vhdl-beginning-of-statement-1 lim)))
	    ;; v-b-o-s could have left us at point-min
	    (and (bobp)
		 ;; CASE 2X.4
		 (vhdl-forward-syntactic-ws indent-point))
	    (setq placeholder (point)))
	  (cond
	   ;; CASE 2A : we are looking at a block-open
	   (begin-after-ip
	    (vhdl-add-syntax 'block-open placeholder))
	   ;; CASE 2B: we are looking at a block-close
	   (end-after-ip
	    (vhdl-add-syntax 'block-close placeholder))
	   ;; CASE 2C: we are looking at a top-level statement
	   ((progn
	      (vhdl-backward-syntactic-ws lim)
	      (or (bobp)
		  (= (preceding-char) ?\;)))
	    (vhdl-add-syntax 'statement placeholder))
	   ;; CASE 2D: we are looking at a top-level statement-cont
	   (t
	    (vhdl-beginning-of-statement-1 lim)
	    ;; v-b-o-s could have left us at point-min
	    (and (bobp)
		 ;; CASE 2D.1
		 (vhdl-forward-syntactic-ws indent-point))
	    (vhdl-add-syntax 'statement-cont (point)))
	   )) ; end CASE 2
	 ;; CASE 3: line is inside parentheses.  Most likely we are
	 ;; either in a subprogram argument (interface) list, or a
	 ;; continued expression containing parentheses.
	 ((null containing-begin)
	  (vhdl-backward-syntactic-ws containing-sexp)
	  (cond
	   ;; CASE 3A: we are looking at the arglist closing paren
	   ((eq char-after-ip ?\))
	    (goto-char containing-sexp)
	    (vhdl-add-syntax 'arglist-close (vhdl-point 'boi)))
	   ;; CASE 3B: we are looking at the first argument in an empty
	   ;; argument list.
	   ((eq char-before-ip ?\()
	    (goto-char containing-sexp)
	    (vhdl-add-syntax 'arglist-intro (vhdl-point 'boi)))
	   ;; CASE 3C: we are looking at an arglist continuation line,
	   ;; but the preceding argument is on the same line as the
	   ;; opening paren.  This case includes multi-line
	   ;; expression paren groupings.
	   ((and (save-excursion
		   (goto-char (1+ containing-sexp))
		   (skip-chars-forward " \t")
		   (not (eolp))
		   (not (looking-at "--")))
		 (save-excursion
		   (vhdl-beginning-of-statement-1 containing-sexp)
		   (skip-chars-backward " \t(")
		   (<= (point) containing-sexp)))
	    (goto-char containing-sexp)
	    (vhdl-add-syntax 'arglist-cont-nonempty (vhdl-point 'boi)))
	   ;; CASE 3D: we are looking at just a normal arglist
	   ;; continuation line
	   (t (vhdl-beginning-of-statement-1 containing-sexp)
	      (vhdl-forward-syntactic-ws indent-point)
	      (vhdl-add-syntax 'arglist-cont (vhdl-point 'boi)))
	   ))
	 ;; CASE 4: A block mid open
	 ((and begin-after-ip
	       (looking-at containing-mid))
	  (goto-char containing-sexp)
	  ;; If the \"begin\" keyword is a trailer, then find v-b-o-s
	  (if (looking-at vhdl-trailer-re)
	      ;; CASE 4.1
	      (progn (forward-sexp) (vhdl-beginning-of-statement-1 nil)))
	  (vhdl-backward-skip-label (vhdl-point 'boi))
	  (vhdl-add-syntax 'block-open (point)))
	 ;; CASE 5: block close brace
	 (end-after-ip
	  (goto-char containing-sexp)
	  ;; If the \"begin\" keyword is a trailer, then find v-b-o-s
	  (if (looking-at vhdl-trailer-re)
	      ;; CASE 5.1
	      (progn (forward-sexp) (vhdl-beginning-of-statement-1 nil)))
	  (vhdl-backward-skip-label (vhdl-point 'boi))
	  (vhdl-add-syntax 'block-close (point)))
	 ;; CASE 6: A continued statement
	 ((and (/= char-before-ip ?\;)
	       ;; check it's not a trailer begin keyword, or a begin
	       ;; keyword immediately following a label.
	       (not (and begin-after-ip
			 (or (looking-at vhdl-trailer-re)
			     (save-excursion
			       (vhdl-backward-skip-label containing-sexp)))))
	       ;; check it's not a statement keyword
	       (not (and (looking-at vhdl-statement-fwd-re)
			 (vhdl-statement-p)))
	       ;; see if the b-o-s is before the indent point
	       (> indent-point
		  (save-excursion
		    (vhdl-beginning-of-statement-1 containing-sexp)
		    ;; If we ended up after a leader, then this will
		    ;; move us forward to the start of the first
		    ;; statement.  Note that a containing sexp here is
		    ;; always a keyword, not a paren, so this will
		    ;; have no effect if we hit the containing-sexp.
		    (vhdl-forward-syntactic-ws indent-point)
		    (setq placeholder (point))))
	       ;; check it's not a block-intro
	       (/= placeholder containing-sexp)
	       ;; check it's not a case block-intro
	       (save-excursion
		 (goto-char placeholder)
		 (or (not (looking-at vhdl-case-alternative-re))
		     (> (match-end 0) indent-point))))
	  ;; Make placeholder skip a label, but only if it puts us
	  ;; before the indent point at the start of a line.
	  (let ((new placeholder))
	    (if (and (> indent-point
			(save-excursion
			  (goto-char placeholder)
			  (vhdl-forward-skip-label indent-point)
			  (setq new (point))))
		     (save-excursion
		       (goto-char new)
		       (eq new (progn (back-to-indentation) (point)))))
		(setq placeholder new)))
	  (vhdl-add-syntax 'statement-cont placeholder)
	  (if begin-after-ip
	      (vhdl-add-syntax 'block-open)))
	 ;; Statement. But what kind?
	 ;; CASE 7: A case alternative key
	 ((and (looking-at vhdl-case-alternative-re)
	       (vhdl-case-alternative-p containing-sexp))
	  ;; for a case alternative key, we set relpos to the first
	  ;; non-whitespace char on the line containing the "case"
	  ;; keyword.
	  (goto-char containing-sexp)
	  ;; If the \"begin\" keyword is a trailer, then find v-b-o-s
	  (if (looking-at vhdl-trailer-re)
	      (progn (forward-sexp) (vhdl-beginning-of-statement-1 nil)))
	  (vhdl-add-syntax 'case-alternative (vhdl-point 'boi)))
	 ;; CASE 8: statement catchall
	 (t
	  ;; we know its a statement, but we need to find out if it is
	  ;; the first statement in a block
	  (if containing-leader
	      (goto-char containing-leader)
	    (goto-char containing-sexp)
	    ;; Note that a containing sexp here is always a keyword,
	    ;; not a paren, so skip over the keyword.
	    (forward-sexp))
	  ;; move to the start of the first statement
	  (vhdl-forward-syntactic-ws indent-point)
	  (setq placeholder (point))
	  ;; we want to ignore case alternatives keys when skipping forward
	  (let (incase-p)
	    (while (looking-at vhdl-case-alternative-re)
	      (setq incase-p (point))
	      ;; we also want to skip over the body of the
	      ;; case/when statement if that doesn't put us at
	      ;; after the indent-point
	      (while (vhdl-skip-case-alternative indent-point))
	      ;; set up the match end
	      (looking-at vhdl-case-alternative-re)
	      (goto-char (match-end 0))
	      ;; move to the start of the first case alternative statement
	      (vhdl-forward-syntactic-ws indent-point)
	      (setq placeholder (point)))
	    (cond
	     ;; CASE 8A: we saw a case/when statement so we must be
	     ;; in a switch statement.  find out if we are at the
	     ;; statement just after a case alternative key
	     ((and incase-p
		   (= (point) indent-point))
	      ;; relpos is the "when" keyword
	      (vhdl-add-syntax 'statement-case-intro incase-p))
	     ;; CASE 8B: any old statement
	     ((< (point) indent-point)
	      ;; relpos is the first statement of the block
	      (vhdl-add-syntax 'statement placeholder)
	      (if begin-after-ip
		  (vhdl-add-syntax 'block-open)))
	     ;; CASE 8C: first statement in a block
	     (t
	      (goto-char containing-sexp)
	      ;; If the \"begin\" keyword is a trailer, then find v-b-o-s
	      (if (looking-at vhdl-trailer-re)
		  (progn (forward-sexp) (vhdl-beginning-of-statement-1 nil)))
	      (vhdl-backward-skip-label (vhdl-point 'boi))
	      (vhdl-add-syntax 'statement-block-intro (point))
	      (if begin-after-ip
		  (vhdl-add-syntax 'block-open)))
	     )))
	 )

	;; now we need to look at any modifiers
	(goto-char indent-point)
	(skip-chars-forward " \t")
	(if (looking-at "--")
	    (vhdl-add-syntax 'comment))
	(if (eq literal 'pound)
	    (vhdl-add-syntax 'cpp-macro))
	;; return the syntax
	vhdl-syntactic-context))))

;; Standard indentation line-ups:

(defun vhdl-lineup-arglist (langelem)
  "Lineup the current arglist line with the arglist appearing just
after the containing paren which starts the arglist."
  (save-excursion
    (let* ((containing-sexp
	    (save-excursion
	      ;; arglist-cont-nonempty gives relpos ==
	      ;; to boi of containing-sexp paren. This
	      ;; is good when offset is +, but bad
	      ;; when it is vhdl-lineup-arglist, so we
	      ;; have to special case a kludge here.
	      (if (memq (car langelem) '(arglist-intro arglist-cont-nonempty))
		  (progn
		    (beginning-of-line)
		    (backward-up-list 1)
		    (skip-chars-forward " \t" (vhdl-point 'eol)))
		(goto-char (cdr langelem)))
	      (point)))
	   (cs-curcol (save-excursion
			(goto-char (cdr langelem))
			(current-column))))
      (if (save-excursion
	    (beginning-of-line)
	    (looking-at "[ \t]*)"))
	  (progn (goto-char (match-end 0))
		 (backward-sexp)
		 (forward-char)
		 (vhdl-forward-syntactic-ws)
		 (- (current-column) cs-curcol))
	(goto-char containing-sexp)
	(or (eolp)
	    (let ((eol (vhdl-point 'eol))
		  (here (progn
			  (forward-char)
			  (skip-chars-forward " \t")
			  (point))))
	      (vhdl-forward-syntactic-ws)
	      (if (< (point) eol)
		  (goto-char here))))
	(- (current-column) cs-curcol)
	))))

(defun vhdl-lineup-arglist-intro (langelem)
  "Lineup an arglist-intro line to just after the open paren."
  (save-excursion
    (let ((cs-curcol (save-excursion
		       (goto-char (cdr langelem))
		       (current-column)))
	  (ce-curcol (save-excursion
		       (beginning-of-line)
		       (backward-up-list 1)
		       (skip-chars-forward " \t" (vhdl-point 'eol))
		       (current-column))))
      (- ce-curcol cs-curcol -1))))

(defun vhdl-lineup-comment (langelem)
  "Support old behavior for comment indentation.  We look at
vhdl-comment-only-line-offset to decide how to indent comment
only-lines."
  (save-excursion
    (back-to-indentation)
    ;; at or to the right of comment-column
    (if (>= (current-column) comment-column)
	(vhdl-comment-indent)
      ;; otherwise, indent as specified by vhdl-comment-only-line-offset
      (if (not (bolp))
	  (or (car-safe vhdl-comment-only-line-offset)
	      vhdl-comment-only-line-offset)
	(or (cdr-safe vhdl-comment-only-line-offset)
	    (car-safe vhdl-comment-only-line-offset)
	    -1000			;jam it against the left side
	    )))))

(defun vhdl-lineup-statement-cont (langelem)
  "Line up statement-cont after the assignment operator."
  (save-excursion
    (let* ((relpos (cdr langelem))
	   (assignp (save-excursion
		     (goto-char (vhdl-point 'boi))
		     (and (re-search-forward "\\(<\\|:\\)="
					     (vhdl-point 'eol) t)
			  (- (point) (vhdl-point 'boi)))))
	   (curcol (progn
		     (goto-char relpos)
		     (current-column)))
	   foundp)
      (while (and (not foundp)
		  (< (point) (vhdl-point 'eol)))
	(re-search-forward "\\(<\\|:\\)=\\|(" (vhdl-point 'eol) 'move)
	(if (vhdl-in-literal)
	    (forward-char)
	  (if (= (preceding-char) ?\()
	      ;; skip over any parenthesized expressions
	      (goto-char (min (vhdl-point 'eol)
			      (scan-lists (point) 1 1)))
	    ;; found an assignment operator (not at eol)
	    (setq foundp (not (looking-at "\\s-*$"))))))
      (if (not foundp)
	  ;; there's no assignment operator on the line
	  vhdl-basic-offset
	;; calculate indentation column after assign and ws, unless
	;; our line contains an assignment operator
	(if (not assignp)
	    (progn
	      (forward-char)
	      (skip-chars-forward " \t")
	      (setq assignp 0)))
	(- (current-column) assignp curcol))
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Progress reporting

(defvar vhdl-progress-info nil
  "Array variable for progress information: 0 begin, 1 end, 2 time.")

(defun vhdl-update-progress-info (string pos)
  "Update progress information."
  (when (and vhdl-progress-info (not noninteractive)
	     (< vhdl-progress-interval
		(- (nth 1 (current-time)) (aref vhdl-progress-info 2))))
    (let ((delta (- (aref vhdl-progress-info 1)
                    (aref vhdl-progress-info 0))))
      (if (= 0 delta)
          (message (concat string "... (100%s)") "%")
        (message (concat string "... (%2d%s)")
                 (/ (* 100 (- pos (aref vhdl-progress-info 0)))
                    delta) "%")))
    (aset vhdl-progress-info 2 (nth 1 (current-time)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indentation commands

(defun vhdl-electric-tab (&optional prefix-arg)
  "If preceding character is part of a word or a paren then hippie-expand,
else if right of non whitespace on line then insert tab,
else if last command was a tab or return then dedent one step or if a comment
toggle between normal indent and inline comment indent,
else indent `correctly'."
  (interactive "*P")
  (vhdl-prepare-search-2
   (cond
    ;; indent region if region is active
    ((and (not (featurep 'xemacs)) (use-region-p))
     (vhdl-indent-region (region-beginning) (region-end) nil))
    ;; expand word
    ((= (char-syntax (preceding-char)) ?w)
     (let ((case-fold-search (not vhdl-word-completion-case-sensitive))
	   (case-replace nil)
	   (hippie-expand-only-buffers
	    (or (and (boundp 'hippie-expand-only-buffers)
		     hippie-expand-only-buffers)
		'(vhdl-mode))))
       (vhdl-expand-abbrev prefix-arg)))
    ;; expand parenthesis
    ((or (= (preceding-char) ?\() (= (preceding-char) ?\)))
     (let ((case-fold-search (not vhdl-word-completion-case-sensitive))
	   (case-replace nil))
       (vhdl-expand-paren prefix-arg)))
    ;; insert tab
    ((> (current-column) (current-indentation))
     (insert-tab))
    ;; toggle comment indent
    ((and (looking-at "--")
	  (or (eq last-command 'vhdl-electric-tab)
	      (eq last-command 'vhdl-electric-return)))
     (cond ((= (current-indentation) 0) ; no indent
	    (indent-to 1)
	    (indent-according-to-mode))
	   ((< (current-indentation) comment-column) ; normal indent
	    (indent-to comment-column)
	    (indent-according-to-mode))
	   (t				; inline comment indent
	    (delete-region (line-beginning-position) (point)))))
    ;; dedent
    ((and (>= (current-indentation) vhdl-basic-offset)
	  (or (eq last-command 'vhdl-electric-tab)
	      (eq last-command 'vhdl-electric-return)))
     (backward-delete-char-untabify vhdl-basic-offset nil))
    ;; indent line
    (t (indent-according-to-mode)))
   (setq this-command 'vhdl-electric-tab)))

(defun vhdl-electric-return ()
  "newline-and-indent or indent-new-comment-line if in comment and preceding
character is a space."
  (interactive)
  (if (and (= (preceding-char) ? ) (vhdl-in-comment-p))
      (indent-new-comment-line)
    (when (and (>= (preceding-char) ?a) (<= (preceding-char) ?z))
      (vhdl-fix-case-word -1))
    (newline-and-indent)))

(defun vhdl-indent-line ()
  "Indent the current line as VHDL code.  Returns the amount of
indentation change."
  (interactive)
  (let* ((syntax (and vhdl-indent-syntax-based (vhdl-get-syntactic-context)))
	 (pos (- (point-max) (point)))
	 (indent
	  (if syntax
	      ;; indent syntax-based
	      (if (and (eq (caar syntax) 'comment)
		       (>= (vhdl-get-offset (car syntax)) comment-column))
		  ;; special case: comments at or right of comment-column
		  (vhdl-get-offset (car syntax))
		(apply '+ (mapcar 'vhdl-get-offset syntax)))
	    ;; indent like previous nonblank line
	    (save-excursion (beginning-of-line)
			    (re-search-backward "^[^\n]" nil t)
			    (current-indentation))))
	 (shift-amt  (- indent (current-indentation))))
    (and vhdl-echo-syntactic-information-p
	 (message "syntax: %s, indent= %d" syntax indent))
    (unless (zerop shift-amt)
      (delete-region (vhdl-point 'bol) (vhdl-point 'boi))
      (beginning-of-line)
      (indent-to indent))
    (if (< (point) (vhdl-point 'boi))
	(back-to-indentation)
      ;; If initial point was within line's indentation, position after
      ;; the indentation.  Else stay at same point in text.
      (when (> (- (point-max) pos) (point))
	(goto-char (- (point-max) pos))))
    (run-hooks 'vhdl-special-indent-hook)
    (vhdl-update-progress-info "Indenting" (vhdl-current-line))
    shift-amt))

(defun vhdl-indent-region (beg end column)
  "Indent region as VHDL code.
Adds progress reporting to `indent-region'."
  (interactive "r\nP")
  (when vhdl-progress-interval
    (setq vhdl-progress-info (vector (count-lines (point-min) beg)
				     (count-lines (point-min) end) 0)))
  (indent-region beg end column)
  (when vhdl-progress-interval (message "Indenting...done"))
  (setq vhdl-progress-info nil))

(defun vhdl-indent-buffer ()
  "Indent whole buffer as VHDL code.
Calls `indent-region' for whole buffer and adds progress reporting."
  (interactive)
  (vhdl-indent-region (point-min) (point-max) nil))

(defun vhdl-indent-group ()
  "Indent group of lines between empty lines."
  (interactive)
  (let ((beg (save-excursion
	       (if (re-search-backward vhdl-align-group-separate nil t)
		   (point-marker)
		 (point-min-marker))))
	(end (save-excursion
	       (if (re-search-forward vhdl-align-group-separate nil t)
		   (point-marker)
		 (point-max-marker)))))
    (vhdl-indent-region beg end nil)))

(defun vhdl-indent-sexp (&optional endpos)
  "Indent each line of the list starting just after point.
If optional arg ENDPOS is given, indent each line, stopping when
ENDPOS is encountered."
  (interactive)
  (save-excursion
    (let ((beg (point))
	  (end (progn (vhdl-forward-sexp nil endpos) (point))))
      (indent-region beg end nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellaneous commands

(defun vhdl-show-syntactic-information ()
  "Show syntactic information for current line."
  (interactive)
  (message "Syntactic analysis: %s" (vhdl-get-syntactic-context))
  (vhdl-keep-region-active))

;; Verification and regression functions:

(defun vhdl-regress-line (&optional arg)
  "Check syntactic information for current line."
  (interactive "P")
  (let ((expected (save-excursion
		    (end-of-line)
		    (when (search-backward " -- ((" (vhdl-point 'bol) t)
		      (forward-char 4)
		      (read (current-buffer)))))
	(actual (vhdl-get-syntactic-context))
	(expurgated))
    ;; remove the library unit symbols
    (mapc
     (function
      (lambda (elt)
	(if (memq (car elt) '(entity configuration package
				     package-body architecture))
	    nil
	  (setq expurgated (append expurgated (list elt))))))
     actual)
    (if (and (not arg) expected (listp expected))
	(if (not (equal expected expurgated))
	    (error "ERROR:  Should be: %s, is: %s" expected expurgated))
      (save-excursion
	(beginning-of-line)
	(when (not (looking-at "^\\s-*\\(--.*\\)?$"))
	  (end-of-line)
	  (if (search-backward " -- ((" (vhdl-point 'bol) t)
	      (delete-region (point) (line-end-position)))
	  (insert " -- ")
	  (insert (format "%s" expurgated))))))
  (vhdl-keep-region-active))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Alignment, whitespace fixup, beautifying
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst vhdl-align-alist
  '(
    ;; after some keywords
    (vhdl-mode "^\\s-*\\(constant\\|quantity\\|signal\\|subtype\\|terminal\\|type\\|variable\\)[ \t]"
	       "^\\s-*\\(constant\\|quantity\\|signal\\|subtype\\|terminal\\|type\\|variable\\)\\([ \t]+\\)" 2)
    ;; before ':'
    (vhdl-mode ":[^=]" "\\([ \t]*\\):[^=]")
    ;; after direction specifications
    (vhdl-mode ":[ \t]*\\(in\\|out\\|inout\\|buffer\\|\\)\\>"
	       ":[ \t]*\\(in\\|out\\|inout\\|buffer\\|\\)\\([ \t]+\\)" 2)
    ;; before "==", ":=", "=>", and "<="
    (vhdl-mode "[<:=]=" "\\([ \t]*\\)[<:=]=" 1) ; since "<= ... =>" can occur
    (vhdl-mode "=>" "\\([ \t]*\\)=>" 1)
    (vhdl-mode "[<:=]=" "\\([ \t]*\\)[<:=]=" 1) ; since "=> ... <=" can occur
    ;; before some keywords
    (vhdl-mode "[ \t]after\\>" "[^ \t]\\([ \t]+\\)after\\>" 1)
    (vhdl-mode "[ \t]when\\>" "[^ \t]\\([ \t]+\\)when\\>" 1)
    (vhdl-mode "[ \t]else\\>" "[^ \t]\\([ \t]+\\)else\\>" 1)
    ;; before "=>" since "when/else ... =>" can occur
    (vhdl-mode "=>" "\\([ \t]*\\)=>" 1)
    )
  "The format of this alist is (MODES [or MODE] REGEXP ALIGN-PATTERN SUBEXP).
It is searched in order.  If REGEXP is found anywhere in the first
line of a region to be aligned, ALIGN-PATTERN will be used for that
region.  ALIGN-PATTERN must include the whitespace to be expanded or
contracted.  It may also provide regexps for the text surrounding the
whitespace.  SUBEXP specifies which sub-expression of
ALIGN-PATTERN matches the white space to be expanded/contracted.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Align code

(defvar vhdl-align-try-all-clauses t
  "If REGEXP is not found on the first line of the region that clause
is ignored.  If this variable is non-nil, then the clause is tried anyway.")

(defun vhdl-do-group (function &optional spacing)
  "Apply FUNCTION on group of lines between empty lines."
  (let
      ;; search for group beginning
      ((beg (save-excursion
	      (if (re-search-backward vhdl-align-group-separate nil t)
		  (progn (beginning-of-line 2) (back-to-indentation) (point))
		(point-min))))
       ;; search for group end
       (end (save-excursion
	      (if (re-search-forward vhdl-align-group-separate nil t)
		  (progn (beginning-of-line) (point))
		(point-max)))))
    ;; run FUNCTION
    (funcall function beg end spacing)))

(defun vhdl-do-list (function &optional spacing)
  "Apply FUNCTION to the lines of a list surrounded by a balanced group of
parentheses."
  (let (beg end)
    (save-excursion
      ;; search for beginning of balanced group of parentheses
      (setq beg (vhdl-re-search-backward "[()]" nil t))
      (while (looking-at ")")
	(forward-char) (backward-sexp)
	(setq beg (vhdl-re-search-backward "[()]" nil t)))
      ;; search for end of balanced group of parentheses
      (when beg
	(forward-list)
	(setq end (point))
	(goto-char (1+ beg))
	(skip-chars-forward " \t\n")
	(setq beg (point))))
    ;; run FUNCTION
    (if beg
	(funcall function beg end spacing)
      (error "ERROR:  Not within a list enclosed by a pair of parentheses"))))

(defun vhdl-do-same-indent (function &optional spacing)
  "Apply FUNCTION to block of lines with same indent."
  (let ((indent (current-indentation))
	beg end)
    ;; search for first line with same indent
    (save-excursion
      (while (and (not (bobp))
		  (or (looking-at "^\\s-*\\(--.*\\)?$")
		      (= (current-indentation) indent)))
	(unless (looking-at "^\\s-*$")
	  (back-to-indentation) (setq beg (point)))
	(beginning-of-line -0)))
    ;; search for last line with same indent
    (save-excursion
      (while (and (not (eobp))
		  (or (looking-at "^\\s-*\\(--.*\\)?$")
		      (= (current-indentation) indent)))
	(if (looking-at "^\\s-*$")
	    (beginning-of-line 2)
	  (beginning-of-line 2)
	  (setq end (point)))))
    ;; run FUNCTION
    (funcall function beg end spacing)))

(defun vhdl-align-region-1 (begin end &optional spacing alignment-list indent)
  "Attempt to align a range of lines based on the content of the
lines.  The definition of `alignment-list' determines the matching
order and the manner in which the lines are aligned.  If ALIGNMENT-LIST
is not specified `vhdl-align-alist' is used.  If INDENT is non-nil,
indentation is done before aligning."
  (interactive "r\np")
  (setq alignment-list (or alignment-list vhdl-align-alist))
  (setq spacing (or spacing 1))
  (save-excursion
    (let (bol indent)
      (goto-char end)
      (setq end (point-marker))
      (goto-char begin)
      (setq bol (setq begin (progn (beginning-of-line) (point))))
;      (untabify bol end)
      (when indent
	(indent-region bol end nil))))
  (let ((copy (copy-alist alignment-list)))
    (vhdl-prepare-search-2
     (while copy
       (save-excursion
	 (goto-char begin)
	 (let (element
	       (eol (point-at-eol)))
	   (setq element (nth 0 copy))
	   (when (and (or (and (listp (car element))
			       (memq major-mode (car element)))
			  (eq major-mode (car element)))
		      (or vhdl-align-try-all-clauses
			  (re-search-forward (car (cdr element)) eol t)))
	     (vhdl-align-region-2 begin end (car (cdr (cdr element)))
				  (car (cdr (cdr (cdr element)))) spacing))
	   (setq copy (cdr copy))))))))

(defun vhdl-align-region-2 (begin end match &optional substr spacing)
  "Align a range of lines from BEGIN to END.  The regular expression
MATCH must match exactly one field: the whitespace to be
contracted/expanded.  The alignment column will equal the
rightmost column of the widest whitespace block.  SPACING is
the amount of extra spaces to add to the calculated maximum required.
SPACING defaults to 1 so that at least one space is inserted after
the token in MATCH."
  (setq spacing (or spacing 1))
  (setq substr (or substr 1))
  (save-excursion
    (let (distance (max 0) (lines 0) bol eol width)
      ;; Determine the greatest whitespace distance to the alignment
      ;; character
      (goto-char begin)
      (setq eol (point-at-eol)
	    bol (setq begin (progn (beginning-of-line) (point))))
      (while (< bol end)
	(save-excursion
	  (when (and (re-search-forward match eol t)
		     (not (vhdl-in-literal)))
	    (setq distance (- (match-beginning substr) bol))
	    (when (> distance max)
	      (setq max distance))))
	(forward-line)
	(setq bol (point)
	      eol (point-at-eol))
	(setq lines (1+ lines)))
      ;; Now insert enough maxs to push each assignment operator to
      ;; the same column.  We need to use 'lines' as a counter, since
      ;; the location of the mark may change
      (goto-char (setq bol begin))
      (setq eol (point-at-eol))
      (while (> lines 0)
  	(when (and (re-search-forward match eol t)
		   (not (vhdl-in-literal)))
	  (setq width (- (match-end substr) (match-beginning substr)))
	  (setq distance (- (match-beginning substr) bol))
	  (goto-char (match-beginning substr))
	  (delete-char width)
	  (insert-char ?  (+ (- max distance) spacing)))
	(beginning-of-line)
	(forward-line)
	(setq bol (point)
	      eol (point-at-eol))
	(setq lines (1- lines))))))

(defun vhdl-align-region-groups (beg end &optional spacing
				     no-message no-comments)
  "Align region, treat groups of lines separately."
  (interactive "r\nP")
  (save-excursion
    (let (orig pos)
      (goto-char beg)
      (beginning-of-line)
      (setq orig (point-marker))
      (setq beg (point))
      (goto-char end)
      (setq end (point-marker))
      (untabify beg end)
      (unless no-message
	(when vhdl-progress-interval
	  (setq vhdl-progress-info (vector (count-lines (point-min) beg)
					   (count-lines (point-min) end) 0))))
      (vhdl-fixup-whitespace-region beg end t)
      (goto-char beg)
      (if (not vhdl-align-groups)
	  ;; align entire region
	  (progn (vhdl-align-region-1 beg end spacing)
		 (unless no-comments
		   (vhdl-align-inline-comment-region-1 beg end)))
	;; align groups
	(while (and (< beg end)
		    (re-search-forward vhdl-align-group-separate end t))
	  (setq pos (point-marker))
	  (vhdl-align-region-1 beg pos spacing)
	  (unless no-comments (vhdl-align-inline-comment-region-1 beg pos))
	  (vhdl-update-progress-info "Aligning" (vhdl-current-line))
	  (setq beg (1+ pos))
	  (goto-char beg))
	;; align last group
	(when (< beg end)
	  (vhdl-align-region-1 beg end spacing)
	  (unless no-comments (vhdl-align-inline-comment-region-1 beg end))
	  (vhdl-update-progress-info "Aligning" (vhdl-current-line))))
      (when vhdl-indent-tabs-mode
	(tabify orig end))
      (unless no-message
	(when vhdl-progress-interval (message "Aligning...done"))
	(setq vhdl-progress-info nil)))))

(defun vhdl-align-region (beg end &optional spacing)
  "Align region, treat blocks with same indent and argument lists separately."
  (interactive "r\nP")
  (if (not vhdl-align-same-indent)
      ;; align entire region
      (vhdl-align-region-groups beg end spacing)
    ;; align blocks with same indent and argument lists
    (save-excursion
      (let ((cur-beg beg)
	    indent cur-end)
	(when vhdl-progress-interval
	  (setq vhdl-progress-info (vector (count-lines (point-min) beg)
					   (count-lines (point-min) end) 0)))
	(goto-char end)
	(setq end (point-marker))
	(goto-char cur-beg)
	(while (< (point) end)
	  ;; is argument list opening?
	  (if (setq cur-beg (nth 1 (save-excursion (parse-partial-sexp
						    (point) (vhdl-point 'eol)))))
	      ;; determine region for argument list
	      (progn (goto-char cur-beg)
		     (forward-sexp)
		     (setq cur-end (point))
		     (beginning-of-line 2))
	    ;; determine region with same indent
	    (setq indent (current-indentation))
	    (setq cur-beg (point))
	    (setq cur-end (vhdl-point 'bonl))
	    (beginning-of-line 2)
	    (while (and (< (point) end)
			(or (looking-at "^\\s-*\\(--.*\\)?$")
			    (= (current-indentation) indent))
			(<= (save-excursion
			      (nth 0 (parse-partial-sexp
				      (point) (vhdl-point 'eol)))) 0))
	      (unless (looking-at "^\\s-*$")
		(setq cur-end (vhdl-point 'bonl)))
	      (beginning-of-line 2)))
	  ;; align region
	  (vhdl-align-region-groups cur-beg cur-end spacing t t))
	(vhdl-align-inline-comment-region beg end spacing noninteractive)
	(when vhdl-progress-interval (message "Aligning...done"))
	(setq vhdl-progress-info nil)))))

(defun vhdl-align-group (&optional spacing)
  "Align group of lines between empty lines."
  (interactive)
  (vhdl-do-group 'vhdl-align-region spacing))

(defun vhdl-align-list (&optional spacing)
  "Align the lines of a list surrounded by a balanced group of parentheses."
  (interactive)
  (vhdl-do-list 'vhdl-align-region-groups spacing))

(defun vhdl-align-same-indent (&optional spacing)
  "Align block of lines with same indent."
  (interactive)
  (vhdl-do-same-indent 'vhdl-align-region-groups spacing))

(defun vhdl-align-declarations (&optional spacing)
  "Align the lines within the declarative part of a design unit."
  (interactive)
  (let (beg end)
    (vhdl-prepare-search-2
     (save-excursion
       ;; search for declarative part
       (when (and (re-search-backward "^\\(architecture\\|begin\\|configuration\\|end\\|entity\\|package\\)\\>" nil t)
		  (not (member (upcase (match-string 1)) '("BEGIN" "END"))))
	 (setq beg (point))
	 (re-search-forward "^\\(begin\\|end\\)\\>" nil t)
	 (setq end (point)))))
    (if beg
	(vhdl-align-region-groups beg end spacing)
      (error "ERROR:  Not within the declarative part of a design unit"))))

(defun vhdl-align-buffer ()
  "Align buffer."
  (interactive)
  (vhdl-align-region (point-min) (point-max)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Align inline comments

(defun vhdl-align-inline-comment-region-1 (beg end &optional spacing)
  "Align inline comments in region."
  (save-excursion
    (let ((start-max comment-column)
	  (length-max 0)
	  comment-list start-list tmp-list start length
	  cur-start prev-start no-code)
      (setq spacing (or spacing 2))
      (vhdl-prepare-search-2
       (goto-char beg)
       ;; search for comment start positions and lengths
       (while (< (point) end)
	 (when (and (not (looking-at "^\\s-*\\(begin\\|end\\)\\>"))
		    (looking-at "^\\(.*[^ \t\n-]+\\)\\s-*\\(--.*\\)$")
		    (not (save-excursion (goto-char (match-beginning 2))
					 (vhdl-in-literal))))
	   (setq start (+ (- (match-end 1) (match-beginning 1)) spacing))
	   (setq length (- (match-end 2) (match-beginning 2)))
	   (setq start-max (max start start-max))
	   (setq length-max (max length length-max))
	   (setq comment-list (cons (cons start length) comment-list)))
	 (beginning-of-line 2))
       (setq comment-list
	     (sort comment-list (function (lambda (a b) (> (car a) (car b))))))
       ;; reduce start positions
       (setq start-list (list (caar comment-list)))
       (setq comment-list (cdr comment-list))
       (while comment-list
	 (unless (or (= (caar comment-list) (car start-list))
		     (<= (+ (car start-list) (cdar comment-list))
			    end-comment-column))
	   (setq start-list (cons (caar comment-list) start-list)))
	 (setq comment-list (cdr comment-list)))
       ;; align lines as nicely as possible
       (goto-char beg)
       (while (< (point) end)
	 (setq cur-start nil)
	 (when (and (not (looking-at "^\\s-*\\(begin\\|end\\)\\>"))
		    (or (and (looking-at "^\\(.*[^ \t\n-]+\\)\\(\\s-*\\)\\(--.*\\)$")
			     (not (save-excursion
				    (goto-char (match-beginning 3))
				    (vhdl-in-literal))))
			(and (looking-at "^\\(\\)\\(\\s-*\\)\\(--.*\\)$")
			     (>= (- (match-end 2) (match-beginning 2))
				 comment-column))))
	   (setq start (+ (- (match-end 1) (match-beginning 1)) spacing))
	   (setq length (- (match-end 3) (match-beginning 3)))
	   (setq no-code (= (match-beginning 1) (match-end 1)))
	   ;; insert minimum whitespace
	   (goto-char (match-end 2))
	   (delete-region (match-beginning 2) (match-end 2))
	   (insert-char ?\  spacing)
	   (setq tmp-list start-list)
	   ;; insert additional whitespace to align
	   (setq cur-start
		 (cond
		  ;; align comment-only line to inline comment of previous line
		  ((and no-code prev-start
			(<= length (- end-comment-column prev-start)))
		   prev-start)
		  ;; align all comments at `start-max' if this is possible
		  ((<= (+ start-max length-max) end-comment-column)
		   start-max)
		  ;; align at `comment-column' if possible
		  ((and (<= start comment-column)
			(<= length (- end-comment-column comment-column)))
		   comment-column)
		  ;; align at left-most possible start position otherwise
		  (t
		   (while (and tmp-list (< (car tmp-list) start))
		     (setq tmp-list (cdr tmp-list)))
		   (car tmp-list))))
	   (indent-to cur-start))
	 (setq prev-start cur-start)
	 (beginning-of-line 2))))))

(defun vhdl-align-inline-comment-region (beg end &optional spacing no-message)
  "Align inline comments within a region.  Groups of code lines separated by
empty lines are aligned individually, if `vhdl-align-groups' is non-nil."
  (interactive "r\nP")
  (save-excursion
    (let (orig pos)
      (goto-char beg)
      (beginning-of-line)
      (setq orig (point-marker))
      (setq beg (point))
      (goto-char end)
      (setq end (point-marker))
      (untabify beg end)
      (unless no-message (message "Aligning inline comments..."))
      (goto-char beg)
      (if (not vhdl-align-groups)
	  ;; align entire region
	  (vhdl-align-inline-comment-region-1 beg end spacing)
	;; align groups
	(while (and (< beg end)
		    (re-search-forward vhdl-align-group-separate end t))
	  (setq pos (point-marker))
	  (vhdl-align-inline-comment-region-1 beg pos spacing)
	  (setq beg (1+ pos))
	  (goto-char beg))
	;; align last group
	(when (< beg end)
	  (vhdl-align-inline-comment-region-1 beg end spacing)))
      (when vhdl-indent-tabs-mode
	(tabify orig end))
      (unless no-message (message "Aligning inline comments...done")))))

(defun vhdl-align-inline-comment-group (&optional spacing)
  "Align inline comments within a group of lines between empty lines."
  (interactive)
  (save-excursion
    (let ((start (point))
	  beg end)
      (setq end (if (re-search-forward vhdl-align-group-separate nil t)
		    (point-marker) (point-max)))
      (goto-char start)
      (setq beg (if (re-search-backward vhdl-align-group-separate nil t)
		    (point) (point-min)))
      (untabify beg end)
      (message "Aligning inline comments...")
      (vhdl-align-inline-comment-region-1 beg end)
      (when vhdl-indent-tabs-mode
	(tabify beg end))
      (message "Aligning inline comments...done"))))

(defun vhdl-align-inline-comment-buffer ()
  "Align inline comments within buffer.  Groups of code lines separated by
empty lines are aligned individually, if `vhdl-align-groups' is non-nil."
  (interactive)
  (vhdl-align-inline-comment-region (point-min) (point-max)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fixup whitespace

(defun vhdl-fixup-whitespace-region (beg end &optional no-message)
  "Fixup whitespace in region.  Surround operator symbols by one space,
eliminate multiple spaces (except at beginning of line), eliminate spaces at
end of line, do nothing in comments and strings."
  (interactive "r")
  (unless no-message (message "Fixing up whitespace..."))
  (save-excursion
    (goto-char end)
    (setq end (point-marker))
    ;; have no space before and one space after `,' and ';'
    (goto-char beg)
    (while (re-search-forward "\\(--.*\n\\|\"[^\"\n]*[\"\n]\\|\'.\'\\)\\|\\(\\s-*\\([,;]\\)\\)" end t)
      (if (match-string 1)
	  (goto-char (match-end 1))
	(replace-match "\\3 " nil nil nil 3)))
    ;; have no space after `('
    (goto-char beg)
    (while (re-search-forward "\\(--.*\n\\|\"[^\"\n]*[\"\n]\\|\'.\'\\)\\|\\((\\)\\s-+" end t)
      (if (match-string 1)
	  (goto-char (match-end 1))
	(replace-match "\\2")))
    ;; have no space before `)'
    (goto-char beg)
    (while (re-search-forward "\\(--.*\n\\|\"[^\"\n]*[\"\n]\\|\'.\'\\|^\\s-+\\)\\|\\s-+\\()\\)" end t)
      (if (match-string 1)
	  (goto-char (match-end 1))
	(replace-match "\\2")))
    ;; surround operator symbols by one space
    (goto-char beg)
    (while (re-search-forward "\\(--.*\n\\|\"[^\"\n]*[\"\n]\\|\'.\'\\)\\|\\(\\([^/:<>=]\\)\\(:\\|=\\|<\\|>\\|:=\\|<=\\|>=\\|=>\\|/=\\)\\([^=>]\\|$\\)\\)" end t)
      (if (match-string 1)
	  (goto-char (match-end 1))
	(replace-match "\\3 \\4 \\5")
	(goto-char (match-end 2))))
    ;; eliminate multiple spaces and spaces at end of line
    (goto-char beg)
    (while (or (and (looking-at "--.*\n") (re-search-forward "--.*\n" end t))
	       (and (looking-at "\"") (re-search-forward "\"[^\"\n]*[\"\n]" end t))
	       (and (looking-at "\\s-+$") (re-search-forward "\\s-+$" end t)
		    (progn (replace-match "" nil nil) t))
	       (and (looking-at "\\s-+;") (re-search-forward "\\s-+;" end t)
		    (progn (replace-match ";" nil nil) t))
	       (and (looking-at "^\\s-+") (re-search-forward "^\\s-+" end t))
	       (and (looking-at "\\s-+--") (re-search-forward "\\s-+" end t)
		    (progn (replace-match "  " nil nil) t))
	       (and (looking-at "\\s-+") (re-search-forward "\\s-+" end t)
		    (progn (replace-match " " nil nil) t))
;	       (re-search-forward "[^ \t-]+" end t))))
	       (re-search-forward "[^ \t\"-]+" end t))))
  (unless no-message (message "Fixing up whitespace...done")))

(defun vhdl-fixup-whitespace-buffer ()
  "Fixup whitespace in buffer.  Surround operator symbols by one space,
eliminate multiple spaces (except at beginning of line), eliminate spaces at
end of line, do nothing in comments."
  (interactive)
  (vhdl-fixup-whitespace-region (point-min) (point-max)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Beautify

(defun vhdl-beautify-region (beg end)
  "Beautify region by applying indentation, whitespace fixup, alignment, and
case fixing to a region.  Calls functions `vhdl-indent-buffer',
`vhdl-align-buffer' (option `vhdl-align-groups' set to non-nil), and
`vhdl-fix-case-buffer'."
  (interactive "r")
  (setq end (save-excursion (goto-char end) (point-marker)))
  (vhdl-indent-region beg end nil)
  (let ((vhdl-align-groups t))
    (vhdl-align-region beg end))
  (vhdl-fix-case-region beg end))

(defun vhdl-beautify-buffer ()
  "Beautify buffer by applying indentation, whitespace fixup, alignment, and
case fixing to entire buffer.  Calls `vhdl-beautify-region' for the entire
buffer."
  (interactive)
  (vhdl-beautify-region (point-min) (point-max))
  (when noninteractive (save-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code filling

(defun vhdl-fill-region (beg end &optional arg)
  "Fill lines for a region of code."
  (interactive "r\np")
  (save-excursion
    (goto-char beg)
    (let ((margin (if arg (current-indentation) (current-column))))
      (goto-char end)
      (setq end (point-marker))
      ;; remove inline comments, newlines and whitespace
      (vhdl-comment-kill-region beg end)
      (vhdl-comment-kill-inline-region beg end)
      (subst-char-in-region beg (1- end) ?\n ?\ )
      (vhdl-fixup-whitespace-region beg end)
      ;; wrap and end-comment-column
      (goto-char beg)
      (while (re-search-forward "\\s-" end t)
	(when(> (current-column) vhdl-end-comment-column)
	  (backward-char)
	  (when (re-search-backward "\\s-" beg t)
	    (replace-match "\n")
	    (indent-to margin)))))))

(defun vhdl-fill-group ()
  "Fill group of lines between empty lines."
  (interactive)
  (vhdl-do-group 'vhdl-fill-region))

(defun vhdl-fill-list ()
  "Fill the lines of a list surrounded by a balanced group of parentheses."
  (interactive)
  (vhdl-do-list 'vhdl-fill-region))

(defun vhdl-fill-same-indent ()
  "Fill the lines of block of lines with same indent."
  (interactive)
  (vhdl-do-same-indent 'vhdl-fill-region))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code updating/fixing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sensitivity list update

;; Strategy:
;; - no sensitivity list is generated for processes with wait statements
;; - otherwise, do the following:
;;   1. scan for all local signals (ports, signals declared in arch./blocks)
;;   2. scan for all signals already in the sensitivity list (in order to catch
;;      manually entered global signals)
;;   3. signals from 1. and 2. form the list of visible signals
;;   4. search for if/elsif conditions containing an event (sequential code)
;;   5. scan for strings that are within syntactical regions where signals are
;;      read but not within sequential code, and that correspond to visible
;;      signals
;;   6. replace sensitivity list by list of signals from 5.

(defun vhdl-update-sensitivity-list-process ()
  "Update sensitivity list of current process."
  (interactive)
  (save-excursion
    (vhdl-prepare-search-2
     (end-of-line)
     ;; look whether in process
     (if (not (and (re-search-backward "^\\s-*\\(\\w+[ \t\n]*:[ \t\n]*\\)?\\(process\\|end\\s-+process\\)\\>" nil t)
		   (equal (upcase (match-string 2)) "PROCESS")
		   (save-excursion (re-search-forward "^\\s-*end\\s-+process\\>" nil t))))
	 (error "ERROR:  Not within a process")
       (message "Updating sensitivity list...")
       (vhdl-update-sensitivity-list)
       (message "Updating sensitivity list...done")))))

(defun vhdl-update-sensitivity-list-buffer ()
  "Update sensitivity list of all processes in current buffer."
  (interactive)
  (save-excursion
    (vhdl-prepare-search-2
     (goto-char (point-min))
     (message "Updating sensitivity lists...")
     (while (re-search-forward "^\\s-*\\(\\w+[ \t\n]*:[ \t\n]*\\)?process\\>" nil t)
       (goto-char (match-beginning 0))
       (condition-case nil (vhdl-update-sensitivity-list) (error "")))
     (message "Updating sensitivity lists...done"))))

(defun vhdl-update-sensitivity-list ()
  "Update sensitivity list."
    (let ((proc-beg (point))
	  (proc-end (re-search-forward "^\\s-*end\\s-+process\\>" nil t))
	  (proc-mid (re-search-backward "^\\s-*begin\\>" nil t))
	  seq-region-list)
      (cond
       ;; search for wait statement (no sensitivity list allowed)
       ((progn (goto-char proc-mid)
	       (vhdl-re-search-forward "\\<wait\\>" proc-end t))
	(error "ERROR:  Process with wait statement, sensitivity list not generated"))
       ;; combinational process (update sensitivity list)
       (t
	(let
	    ;; scan for visible signals
	    ((visible-list (vhdl-get-visible-signals))
	     ;; define syntactic regions where signals are read
	     (scan-regions-list
	      '(;; right-hand side of signal/variable assignment
		;; (special case: "<=" is relational operator in a condition)
		((re-search-forward "[<:]=" proc-end t)
		 (re-search-forward ";\\|\\<\\(then\\|loop\\|report\\|severity\\|is\\)\\>" proc-end t))
		;; if condition
		((re-search-forward "^\\s-*if\\>" proc-end t)
		 (re-search-forward "\\<then\\>" proc-end t))
		;; elsif condition
		((re-search-forward "\\<elsif\\>" proc-end t)
		 (re-search-forward "\\<then\\>" proc-end t))
		;; while loop condition
		((re-search-forward "^\\s-*while\\>" proc-end t)
		 (re-search-forward "\\<loop\\>" proc-end t))
		;; exit/next condition
		((re-search-forward "\\<\\(exit\\|next\\)\\s-+\\w+\\s-+when\\>" proc-end t)
		 (re-search-forward ";" proc-end t))
		;; assert condition
		((re-search-forward "\\<assert\\>" proc-end t)
		 (re-search-forward "\\(\\<report\\>\\|\\<severity\\>\\|;\\)" proc-end t))
		;; case expression
		((re-search-forward "^\\s-*case\\>" proc-end t)
		 (re-search-forward "\\<is\\>" proc-end t))
		;; parameter list of procedure call
		((and (re-search-forward "^\\s-*\\w+[ \t\n]*(" proc-end t)
		      (1- (point)))
		 (progn (backward-char) (forward-sexp)
			(while (looking-at "(") (forward-sexp)) (point)))))
	     name read-list sens-list signal-list
	     sens-beg sens-end beg end margin)
	  ;; scan for signals in old sensitivity list
	  (goto-char proc-beg)
	  (re-search-forward "\\<process\\>" proc-mid t)
	  (if (not (looking-at "[ \t\n]*("))
	      (setq sens-beg (point))
	    (setq sens-beg (re-search-forward "\\([ \t\n]*\\)([ \t\n]*" nil t))
	    (goto-char (match-end 1))
	    (forward-sexp)
	    (setq sens-end (1- (point)))
	    (goto-char sens-beg)
	    (while (and (re-search-forward "\\(\\w+\\)" sens-end t)
			(setq sens-list
			      (cons (downcase (match-string 0)) sens-list))
			(re-search-forward "\\s-*,\\s-*" sens-end t))))
	  (setq signal-list (append visible-list sens-list))
	  ;; search for sequential parts
	  (goto-char proc-mid)
	  (while (setq beg (re-search-forward "^\\s-*\\(els\\)?if\\>" proc-end t))
	    (setq end (re-search-forward "\\<then\\>" proc-end t))
	    (when (re-search-backward "\\('event\\|\\<\\(falling\\|rising\\)_edge\\)\\>" beg t)
	      (goto-char end)
	      (backward-word 1)
	      (vhdl-forward-sexp)
	      (setq seq-region-list (cons (cons end (point)) seq-region-list))
	      (beginning-of-line)))
	  ;; scan for signals read in process
	  (while scan-regions-list
	    (goto-char proc-mid)
	    (while (and (setq beg (eval (nth 0 (car scan-regions-list))))
			(setq end (eval (nth 1 (car scan-regions-list)))))
	      (goto-char beg)
	      (unless (or (vhdl-in-literal)
			  (and seq-region-list
			       (let ((tmp-list seq-region-list))
				 (while (and tmp-list
					     (< (point) (caar tmp-list)))
				   (setq tmp-list (cdr tmp-list)))
				 (and tmp-list (< (point) (cdar tmp-list))))))
		(while (vhdl-re-search-forward "[^'\"]\\<\\([a-zA-Z]\\w*\\)\\>[ \t\n]*\\('\\(\\w+\\)\\|\\(=>\\)\\)?" end t)
		  (setq name (match-string 1))
		  (when (and (not (match-string 4)) ; not when formal parameter
			     (not (and (match-string 3) ; not event attribute
				       (not (member (downcase (match-string 3))
						    '("event" "last_event" "transaction")))))
			     (member (downcase name) signal-list))
		    (unless (member-ignore-case name read-list)
		      (setq read-list (cons name read-list))))
		  (goto-char (match-end 1)))))
	    (setq scan-regions-list (cdr scan-regions-list)))
	  ;; update sensitivity list
	  (goto-char sens-beg)
	  (if sens-end
	      (delete-region sens-beg sens-end)
	    (when read-list
	      (insert " ()") (backward-char)))
	  (setq read-list (sort read-list 'string<))
	  (when read-list
	    (setq margin (current-column))
	    (insert (car read-list))
	    (setq read-list (cdr read-list))
	    (while read-list
	      (insert ",")
	      (if (<= (+ (current-column) (length (car read-list)) 2)
		      end-comment-column)
		  (insert " ")
		(insert "\n") (indent-to margin))
	      (insert (car read-list))
	      (setq read-list (cdr read-list)))))))))

(defun vhdl-get-visible-signals ()
  "Get all signals visible in the current block."
  (let (beg end signal-list entity-name file-name)
    (vhdl-prepare-search-2
     ;; get entity name
     (save-excursion
       (unless (and (re-search-backward "^\\(architecture\\s-+\\w+\\s-+of\\s-+\\(\\w+\\)\\|end\\)\\>" nil t)
		    (not (equal "END" (upcase (match-string 1))))
		    (setq entity-name (match-string 2)))
	 (error "ERROR:  Not within an architecture")))
     ;; search for signals declared in entity port clause
     (save-excursion
       (goto-char (point-min))
       (unless (re-search-forward (concat "^entity\\s-+" entity-name "\\>") nil t)
	 (setq file-name
	       (concat (vhdl-replace-string vhdl-entity-file-name entity-name t)
		       "." (file-name-extension (buffer-file-name)))))
       (vhdl-visit-file
	file-name t
	(vhdl-prepare-search-2
	 (goto-char (point-min))
	 (if (not (re-search-forward (concat "^entity\\s-+" entity-name "\\>") nil t))
	     (error "ERROR:  Entity \"%s\" not found:\n  --> see option `vhdl-entity-file-name'" entity-name)
	   (when (setq beg (re-search-forward
			    "^\\s-*port[ \t\n]*("
			    (save-excursion
			      (re-search-forward "^end\\>" nil t)) t))
	     (setq end (save-excursion
			 (backward-char) (forward-sexp) (point)))
	     (vhdl-forward-syntactic-ws)
	     (while (< (point) end)
	       (when (looking-at "signal[ \t\n]+")
		 (goto-char (match-end 0)))
	       (while (looking-at "\\(\\w+\\)[ \t\n,]+")
		 (setq signal-list
		       (cons (downcase (match-string 1)) signal-list))
		 (goto-char (match-end 0))
		 (vhdl-forward-syntactic-ws))
	       (re-search-forward ";" end 1)
	       (vhdl-forward-syntactic-ws)))))))
     ;; search for signals declared in architecture declarative part
     (save-excursion
       (if (not (and (setq beg (re-search-backward "^\\(architecture\\s-+\\w+\\s-+of\\s-+\\(\\w+\\)\\|end\\)\\>" nil t))
		     (not (equal "END" (upcase (match-string 1))))
		     (setq end (re-search-forward "^begin\\>" nil t))))
	   (error "ERROR:  No architecture declarative part found")
	 ;; scan for all declared signal and alias names
	 (goto-char beg)
	 (while (re-search-forward "^\\s-*\\(\\(signal\\)\\|alias\\)\\>" end t)
	   (when (= 0 (nth 0 (parse-partial-sexp beg (point))))
	     (if (match-string 2)
		 ;; scan signal name
		 (while (looking-at "[ \t\n,]+\\(\\w+\\)")
		   (setq signal-list
			 (cons (downcase (match-string 1)) signal-list))
		   (goto-char (match-end 0)))
	       ;; scan alias name, check is alias of (declared) signal
	       (when (and (looking-at "[ \t\n]+\\(\\w+\\)[^;]*\\<is[ \t\n]+\\(\\w+\\)")
			  (member (downcase (match-string 2)) signal-list))
		 (setq signal-list
		       (cons (downcase (match-string 1)) signal-list))
		 (goto-char (match-end 0))))
	     (setq beg (point))))))
     ;; search for signals declared in surrounding block declarative parts
     (save-excursion
       (while (and (progn (while (and (setq beg (re-search-backward "^\\s-*\\(\\w+\\s-*:\\s-*block\\|\\(end\\)\\s-+block\\)\\>" nil t))
				      (match-string 2))
			    (goto-char (match-end 2))
			    (vhdl-backward-sexp)
			    (re-search-backward "^\\s-*\\w+\\s-*:\\s-*block\\>" nil t))
			  beg)
		   (setq end (re-search-forward "^\\s-*begin\\>" nil t)))
	 ;; scan for all declared signal names
	 (goto-char beg)
	 (while (re-search-forward "^\\s-*\\(\\(signal\\)\\|alias\\)\\>" end t)
	   (when (= 0 (nth 0 (parse-partial-sexp beg (point))))
	     (if (match-string 2)
		 ;; scan signal name
		 (while (looking-at "[ \t\n,]+\\(\\w+\\)")
		   (setq signal-list
			 (cons (downcase (match-string 1)) signal-list))
		   (goto-char (match-end 0)))
	       ;; scan alias name, check is alias of (declared) signal
	       (when (and (looking-at "[ \t\n]+\\(\\w+\\)[^;]*\\<is[ \t\n]+\\(\\w+\\)")
			  (member (downcase (match-string 2)) signal-list))
		 (setq signal-list
		       (cons (downcase (match-string 1)) signal-list))
		 (goto-char (match-end 0))))))
	 (goto-char beg)))
     signal-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic/port clause fixing

(defun vhdl-fix-clause ()
  "Fix closing parenthesis within generic/port clause."
  (interactive)
  (save-excursion
    (vhdl-prepare-search-2
     (let ((pos (point))
	   beg end)
       (if (not (re-search-backward "^\\s-*\\(generic\\|port\\)[ \t\n]*(" nil t))
	   (error "ERROR:  Not within a generic/port clause")
	 ;; search for end of clause
	 (goto-char (match-end 0))
	 (setq beg (1- (point)))
	 (vhdl-forward-syntactic-ws)
	 (while (looking-at "\\w+\\([ \t\n]*,[ \t\n]*\\w+\\)*[ \t\n]*:[ \t\n]*\\w+[^;]*;")
	   (goto-char (1- (match-end 0)))
	   (setq end (point-marker))
	   (forward-char)
	   (vhdl-forward-syntactic-ws))
	 (goto-char end)
	 (when (> pos (point-at-eol))
	   (error "ERROR:  Not within a generic/port clause"))
	 ;; delete closing parenthesis on separate line (not supported style)
	 (when (save-excursion (beginning-of-line) (looking-at "^\\s-*);"))
	   (vhdl-line-kill)
	   (vhdl-backward-syntactic-ws)
	   (setq end (point-marker))
	   (insert ";"))
	 ;; delete superfluous parentheses
	 (while (progn (goto-char beg)
		       (condition-case () (forward-sexp)
			 (error (goto-char (point-max))))
		       (< (point) end))
	   (delete-char -1))
	 ;; add closing parenthesis
	 (when (> (point) end)
	   (goto-char end)
	   (insert ")")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellaneous

(defun vhdl-remove-trailing-spaces ()
  "Remove trailing spaces in the whole buffer."
  (interactive)
  (save-match-data
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "[ \t]+$" (point-max) t)
	(unless (vhdl-in-literal)
	  (replace-match "" nil nil))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Electrification
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst vhdl-template-prompt-syntax "[^ =<>][^<>@.\n]*[^ =<>]"
  "Syntax of prompt inserted by template generators.")

(defvar vhdl-template-invoked-by-hook nil
  "Indicates whether a template has been invoked by a hook or by key or menu.
Used for undoing after template abortion.")

;; correct different behavior of function `unread-command-events' in XEmacs
(defun vhdl-character-to-event (arg))
(defalias 'vhdl-character-to-event
  (if (fboundp 'character-to-event) 'character-to-event 'identity))

(defun vhdl-work-library ()
  "Return the working library name of the current project or \"work\" if no
project is defined."
  (vhdl-resolve-env-variable
   (or (nth 6 (aget vhdl-project-alist vhdl-project)) vhdl-default-library)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Enabling/disabling

(define-minor-mode vhdl-electric-mode
  "Toggle VHDL electric mode.
With a prefix argument ARG, enable the mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable it if ARG
is omitted or nil."
  :global t)

(define-minor-mode vhdl-stutter-mode
  "Toggle VHDL stuttering mode.
With a prefix argument ARG, enable the mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable it if ARG
is omitted or nil."
  :global t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stuttering

(defun vhdl-electric-dash (count)
  "-- starts a comment, --- draws a horizontal line,
---- starts a display comment."
  (interactive "p")
  (if (and vhdl-stutter-mode (not (vhdl-in-literal)))
      (cond
       ((and abbrev-start-location (= abbrev-start-location (point)))
	(setq abbrev-start-location nil)
	(goto-char last-abbrev-location)
	(beginning-of-line nil)
	(vhdl-comment-display))
       ((/= (preceding-char) ?-)	; standard dash (minus)
	(self-insert-command count))
       (t (self-insert-command count)
	  (message "Enter '-' for horiz. line, 'CR' for commenting-out code, else enter comment")
	  (let ((next-input (read-char)))
	    (if (= next-input ?-)	; triple dash
		(progn
		  (vhdl-comment-display-line)
		  (message
		   "Enter '-' for display comment, else continue coding")
		  (let ((next-input (read-char)))
		    (if (= next-input ?-) ; four dashes
			(vhdl-comment-display t)
		      (setq unread-command-events ; pushback the char
			    (list (vhdl-character-to-event next-input))))))
	      (setq unread-command-events ; pushback the char
		    (list (vhdl-character-to-event next-input)))
	      (vhdl-comment-insert)))))
    (self-insert-command count)))

(defun vhdl-electric-open-bracket (count) "'[' --> '(', '([' --> '['"
  (interactive "p")
  (if (and vhdl-stutter-mode (= count 1) (not (vhdl-in-literal)))
      (if (= (preceding-char) ?\()
	  (progn (delete-char -1) (insert-char ?\[ 1))
	(insert-char ?\( 1))
    (self-insert-command count)))

(defun vhdl-electric-close-bracket (count) "']' --> ')', ')]' --> ']'"
  (interactive "p")
  (if (and vhdl-stutter-mode (= count 1) (not (vhdl-in-literal)))
      (progn
	(if (= (preceding-char) ?\))
	    (progn (delete-char -1) (insert-char ?\] 1))
	  (insert-char ?\) 1))
	(blink-matching-open))
    (self-insert-command count)))

(defun vhdl-electric-quote (count) "'' --> \""
  (interactive "p")
  (if (and vhdl-stutter-mode (= count 1) (not (vhdl-in-literal)))
      (if (= (preceding-char) last-input-event)
	  (progn (delete-char -1) (insert-char ?\" 1))
	(insert-char ?\' 1))
    (self-insert-command count)))

(defun vhdl-electric-semicolon (count) "';;' --> ' : ', ': ;' --> ' := '"
  (interactive "p")
  (if (and vhdl-stutter-mode (= count 1) (not (vhdl-in-literal)))
      (cond ((= (preceding-char) last-input-event)
	     (progn (delete-char -1)
		    (unless (eq (preceding-char) ? ) (insert " "))
		    (insert ": ")
		    (setq this-command 'vhdl-electric-colon)))
	    ((and
	      (eq last-command 'vhdl-electric-colon) (= (preceding-char) ? ))
	     (progn (delete-char -1) (insert "= ")))
	    (t (insert-char ?\; 1)))
    (self-insert-command count)))

(defun vhdl-electric-comma (count) "',,' --> ' <= '"
  (interactive "p")
  (if (and vhdl-stutter-mode (= count 1) (not (vhdl-in-literal)))
      (cond ((= (preceding-char) last-input-event)
	     (progn (delete-char -1)
		    (unless (eq (preceding-char) ? ) (insert " "))
		    (insert "<= ")))
	    (t (insert-char ?\, 1)))
    (self-insert-command count)))

(defun vhdl-electric-period (count) "'..' --> ' => '"
  (interactive "p")
  (if (and vhdl-stutter-mode (= count 1) (not (vhdl-in-literal)))
      (cond ((= (preceding-char) last-input-event)
	     (progn (delete-char -1)
		    (unless (eq (preceding-char) ? ) (insert " "))
		    (insert "=> ")))
	    (t (insert-char ?\. 1)))
    (self-insert-command count)))

(defun vhdl-electric-equal (count) "'==' --> ' == '"
  (interactive "p")
  (if (and vhdl-stutter-mode (= count 1) (not (vhdl-in-literal)))
      (cond ((= (preceding-char) last-input-event)
	     (progn (delete-char -1)
		    (unless (eq (preceding-char) ? ) (insert " "))
		    (insert "== ")))
	    (t (insert-char ?\= 1)))
    (self-insert-command count)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  VHDL templates

(defun vhdl-template-paired-parens ()
  "Insert a pair of round parentheses, placing point between them."
  (interactive)
  (insert "()")
  (backward-char))

(defun vhdl-template-alias ()
  "Insert alias declaration."
  (interactive)
  (let ((start (point)))
    (vhdl-insert-keyword "ALIAS ")
    (when (vhdl-template-field "name" nil t start (point))
      (insert " : ")
      (unless (vhdl-template-field
	       (concat "[type" (and (vhdl-standard-p 'ams) " or nature") "]")
	       nil t)
	(delete-char -3))
      (vhdl-insert-keyword " IS ")
      (vhdl-template-field "name" ";")
      (vhdl-comment-insert-inline))))

(defun vhdl-template-architecture ()
  "Insert architecture."
  (interactive)
  (let ((margin (current-indentation))
	(start (point))
	arch-name)
    (vhdl-insert-keyword "ARCHITECTURE ")
    (when (setq arch-name
		(vhdl-template-field "name" nil t start (point)))
      (vhdl-insert-keyword " OF ")
      (if (save-excursion
	    (vhdl-prepare-search-1
	     (vhdl-re-search-backward "\\<entity \\(\\w+\\) is\\>" nil t)))
	  (insert (match-string 1))
	(vhdl-template-field "entity name"))
      (vhdl-insert-keyword " IS\n")
      (vhdl-template-begin-end
       (unless (vhdl-standard-p '87) "ARCHITECTURE") arch-name margin
       (memq vhdl-insert-empty-lines '(unit all))))))

(defun vhdl-template-array (kind &optional secondary)
  "Insert array type definition."
  (interactive)
  (let ((start (point)))
    (vhdl-insert-keyword "ARRAY (")
    (when (or (vhdl-template-field "range" nil (not secondary) start (point))
	      secondary)
      (vhdl-insert-keyword ") OF ")
      (vhdl-template-field (if (eq kind 'type) "type" "nature"))
      (vhdl-insert-keyword ";"))))

(defun vhdl-template-assert ()
  "Insert an assertion statement."
  (interactive)
  (let ((start (point)))
    (vhdl-insert-keyword "ASSERT ")
    (when vhdl-conditions-in-parenthesis (insert "("))
    (when (vhdl-template-field "condition (negated)" nil t start (point))
      (when vhdl-conditions-in-parenthesis (insert ")"))
      (setq start (point))
      (vhdl-insert-keyword " REPORT ")
      (unless (vhdl-template-field "string expression" nil nil nil nil t)
	(delete-region start (point)))
      (setq start (point))
      (vhdl-insert-keyword " SEVERITY ")
      (unless (vhdl-template-field "[NOTE | WARNING | ERROR | FAILURE]" nil t)
	(delete-region start (point)))
      (insert ";"))))

(defun vhdl-template-attribute ()
  "Insert an attribute declaration or specification."
  (interactive)
  (if (eq (vhdl-decision-query
	   "attribute" "(d)eclaration or (s)pecification?" t) ?s)
      (vhdl-template-attribute-spec)
    (vhdl-template-attribute-decl)))

(defun vhdl-template-attribute-decl ()
  "Insert an attribute declaration."
  (interactive)
  (let ((start (point)))
    (vhdl-insert-keyword "ATTRIBUTE ")
    (when (vhdl-template-field "name" " : " t start (point))
      (vhdl-template-field "type" ";")
      (vhdl-comment-insert-inline))))

(defun vhdl-template-attribute-spec ()
  "Insert an attribute specification."
  (interactive)
  (let ((start (point)))
    (vhdl-insert-keyword "ATTRIBUTE ")
    (when (vhdl-template-field "name" nil t start (point))
      (vhdl-insert-keyword " OF ")
      (vhdl-template-field "entity names | OTHERS | ALL" " : ")
      (vhdl-template-field "entity class")
      (vhdl-insert-keyword " IS ")
      (vhdl-template-field "expression" ";"))))

(defun vhdl-template-block ()
  "Insert a block."
  (interactive)
  (let ((margin (current-indentation))
	(start (point))
	label)
    (vhdl-insert-keyword ": BLOCK ")
    (goto-char start)
    (when (setq label (vhdl-template-field "label" nil t start (+ (point) 8)))
      (forward-word 1)
      (forward-char 1)
      (insert "(")
      (if (vhdl-template-field "[guard expression]" nil t)
	  (insert ")")
	(delete-char -2))
      (unless (vhdl-standard-p '87) (vhdl-insert-keyword " IS"))
      (insert "\n")
      (vhdl-template-begin-end "BLOCK" label margin)
      (vhdl-comment-block))))

(defun vhdl-template-block-configuration ()
  "Insert a block configuration statement."
  (interactive)
  (let ((margin (current-indentation))
	(start (point)))
    (vhdl-insert-keyword "FOR ")
    (when (vhdl-template-field "block name" nil t start (point))
      (vhdl-insert-keyword "\n\n")
      (indent-to margin)
      (vhdl-insert-keyword "END FOR;")
      (end-of-line 0)
      (indent-to (+ margin vhdl-basic-offset)))))

(defun vhdl-template-break ()
  "Insert a break statement."
  (interactive)
  (let (position)
    (vhdl-insert-keyword "BREAK")
    (setq position (point))
    (insert " ")
    (while (or
	    (progn (vhdl-insert-keyword "FOR ")
		   (if (vhdl-template-field "[quantity name]" " USE " t)
		       (progn (vhdl-template-field "quantity name" " => ") t)
		     (delete-region (point)
				    (progn (forward-word -1) (point)))
		     nil))
	    (vhdl-template-field "[quantity name]" " => " t))
      (vhdl-template-field "expression")
      (setq position (point))
      (insert ", "))
    (delete-region position (point))
    (unless (vhdl-sequential-statement-p)
      (vhdl-insert-keyword " ON ")
      (if (vhdl-template-field "[sensitivity list]" nil t)
	  (setq position (point))
	(delete-region position (point))))
    (vhdl-insert-keyword " WHEN ")
    (when vhdl-conditions-in-parenthesis (insert "("))
    (if (vhdl-template-field "[condition]" nil t)
	(when vhdl-conditions-in-parenthesis (insert ")"))
      (delete-region position (point)))
    (insert ";")))

(defun vhdl-template-case (&optional kind)
  "Insert a case statement."
  (interactive)
  (let ((margin (current-indentation))
	(start (point))
	label)
    (unless kind (setq kind (if (vhdl-sequential-statement-p) 'is 'use)))
    (if (or (not (eq vhdl-optional-labels 'all)) (vhdl-standard-p '87))
	(vhdl-insert-keyword "CASE ")
      (vhdl-insert-keyword ": CASE ")
      (goto-char start)
      (setq label (vhdl-template-field "[label]" nil t))
      (unless label (delete-char 2))
      (forward-word 1)
      (forward-char 1))
    (when (vhdl-template-field "expression" nil t start (point))
      (vhdl-insert-keyword (concat " " (if (eq kind 'is) "IS" "USE") "\n\n"))
      (indent-to margin)
      (vhdl-insert-keyword "END CASE")
      (when label (insert " " label))
      (insert ";")
      (forward-line -1)
      (indent-to (+ margin vhdl-basic-offset))
      (vhdl-insert-keyword "WHEN ")
      (let ((position (point)))
	(insert " => ;\n")
	(indent-to (+ margin vhdl-basic-offset))
	(vhdl-insert-keyword "WHEN OTHERS => null;")
	(goto-char position)))))

(defun vhdl-template-case-is ()
  "Insert a sequential case statement."
  (interactive)
  (vhdl-template-case 'is))

(defun vhdl-template-case-use ()
  "Insert a simultaneous case statement."
  (interactive)
  (vhdl-template-case 'use))

(defun vhdl-template-component ()
  "Insert a component declaration."
  (interactive)
  (vhdl-template-component-decl))

(defun vhdl-template-component-conf ()
  "Insert a component configuration (uses `vhdl-template-configuration-spec'
since these are almost equivalent)."
  (interactive)
  (let ((margin (current-indentation))
	(result (vhdl-template-configuration-spec t)))
    (when result
      (insert "\n")
      (indent-to margin)
      (vhdl-insert-keyword "END FOR;")
      (when (eq result 'no-use)
	(end-of-line -0)))))

(defun vhdl-template-component-decl ()
  "Insert a component declaration."
  (interactive)
  (let ((margin (current-indentation))
	(start (point))
	name end-column)
    (vhdl-insert-keyword "COMPONENT ")
    (when (setq name (vhdl-template-field "name" nil t start (point)))
      (unless (vhdl-standard-p '87) (vhdl-insert-keyword " IS"))
      (insert "\n\n")
      (indent-to margin)
      (vhdl-insert-keyword "END COMPONENT")
      (unless (vhdl-standard-p '87) (insert " " name))
      (insert ";")
      (setq end-column (current-column))
      (end-of-line -0)
      (indent-to (+ margin vhdl-basic-offset))
      (vhdl-template-generic-list t t)
      (insert "\n")
      (indent-to (+ margin vhdl-basic-offset))
      (vhdl-template-port-list t)
      (beginning-of-line 2)
      (forward-char end-column))))

(defun vhdl-template-component-inst ()
  "Insert a component instantiation statement."
  (interactive)
  (let ((margin (current-indentation))
	(start (point))
	unit position)
    (when (vhdl-template-field "instance label" nil t start (point))
      (insert ": ")
      (if (not (vhdl-use-direct-instantiation))
	  (vhdl-template-field "component name")
	;; direct instantiation
	(setq unit (vhdl-template-field
		    "[COMPONENT | ENTITY | CONFIGURATION]" " " t))
	(setq unit (upcase (or unit "")))
	(cond ((equal unit "ENTITY")
	       (vhdl-template-field "library name" "." nil nil nil nil
				    (vhdl-work-library))
	       (vhdl-template-field "entity name" "(")
	       (if (vhdl-template-field "[architecture name]" nil t)
		   (insert ")")
		 (delete-char -1)))
	      ((equal unit "CONFIGURATION")
	       (vhdl-template-field "library name" "." nil nil nil nil
				    (vhdl-work-library))
	       (vhdl-template-field "configuration name"))
	      (t (vhdl-template-field "component name"))))
      (insert "\n")
      (indent-to (+ margin vhdl-basic-offset))
      (setq position (point))
      (vhdl-insert-keyword "GENERIC ")
      (when (vhdl-template-map position t t)
	(insert "\n")
	(indent-to (+ margin vhdl-basic-offset)))
      (setq position (point))
      (vhdl-insert-keyword "PORT ")
      (unless (vhdl-template-map position t t)
	(delete-region (line-beginning-position) (point))
	(delete-char -1))
      (insert ";"))))

(defun vhdl-template-conditional-signal-asst ()
  "Insert a conditional signal assignment."
  (interactive)
  (when (vhdl-template-field "target signal")
    (insert " <= ")
;    (if (not (equal (vhdl-template-field "[GUARDED] [TRANSPORT]") ""))
;       (insert " "))
    (let ((margin (current-column))
	  (start (point))
	  position)
      (vhdl-template-field "waveform")
      (setq position (point))
      (vhdl-insert-keyword " WHEN ")
      (when vhdl-conditions-in-parenthesis (insert "("))
      (while (and (vhdl-template-field "[condition]" nil t)
		  (progn
		    (when vhdl-conditions-in-parenthesis (insert ")"))
		    (setq position (point))
		    (vhdl-insert-keyword " ELSE")
		    (insert "\n")
		    (indent-to margin)
		    (vhdl-template-field "[waveform]" nil t)))
	(setq position (point))
	(vhdl-insert-keyword " WHEN ")
	(when vhdl-conditions-in-parenthesis (insert "(")))
      (delete-region position (point))
      (insert ";")
      (when vhdl-auto-align (vhdl-align-region-groups start (point) 1)))))

(defun vhdl-template-configuration ()
  "Insert a configuration specification if within an architecture,
a block or component configuration if within a configuration declaration,
a configuration declaration if not within a design unit."
  (interactive)
  (vhdl-prepare-search-1
   (cond
    ((and (save-excursion		; architecture body
	    (re-search-backward "^\\(architecture\\|end\\)\\>" nil t))
	  (equal "ARCHITECTURE" (upcase (match-string 1))))
     (vhdl-template-configuration-spec))
    ((and (save-excursion		; configuration declaration
	    (re-search-backward "^\\(configuration\\|end\\)\\>" nil t))
	  (equal "CONFIGURATION" (upcase (match-string 1))))
     (if (eq (vhdl-decision-query
	      "configuration" "(b)lock or (c)omponent configuration?" t) ?c)
	 (vhdl-template-component-conf)
       (vhdl-template-block-configuration)))
    (t (vhdl-template-configuration-decl))))) ; otherwise

(defun vhdl-template-configuration-spec (&optional optional-use)
  "Insert a configuration specification."
  (interactive)
  (let ((margin (current-indentation))
	(start (point))
	aspect position)
    (vhdl-insert-keyword "FOR ")
    (when (vhdl-template-field "instance names | OTHERS | ALL" " : "
			       t start (point))
      (vhdl-template-field "component name" "\n")
      (indent-to (+ margin vhdl-basic-offset))
      (setq start (point))
      (vhdl-insert-keyword "USE ")
      (if (and optional-use
	       (not (setq aspect (vhdl-template-field
				  "[ENTITY | CONFIGURATION | OPEN]" " " t))))
	  (progn (delete-region start (point)) 'no-use)
	(unless optional-use
	  (setq aspect (vhdl-template-field
			"ENTITY | CONFIGURATION | OPEN" " ")))
	(setq aspect (upcase (or aspect "")))
	(cond ((equal aspect "ENTITY")
	       (vhdl-template-field "library name" "." nil nil nil nil
				    (vhdl-work-library))
	       (vhdl-template-field "entity name" "(")
	       (if (vhdl-template-field "[architecture name]" nil t)
		   (insert ")")
		 (delete-char -1))
	       (insert "\n")
	       (indent-to (+ margin (* 2 vhdl-basic-offset)))
	       (setq position (point))
	       (vhdl-insert-keyword "GENERIC ")
	       (when (vhdl-template-map position t t)
		 (insert "\n")
		 (indent-to (+ margin (* 2 vhdl-basic-offset))))
	       (setq position (point))
	       (vhdl-insert-keyword "PORT ")
	       (unless (vhdl-template-map position t t)
		 (delete-region (line-beginning-position) (point))
		 (delete-char -1))
	       (insert ";")
	       t)
	      ((equal aspect "CONFIGURATION")
	       (vhdl-template-field "library name" "." nil nil nil nil
				    (vhdl-work-library))
	       (vhdl-template-field "configuration name" ";"))
	      (t (delete-char -1) (insert ";") t))))))


(defun vhdl-template-configuration-decl ()
  "Insert a configuration declaration."
  (interactive)
  (let ((margin (current-indentation))
	(start (point))
	entity-exists string name position)
    (vhdl-insert-keyword "CONFIGURATION ")
    (when (setq name (vhdl-template-field "name" nil t start (point)))
      (vhdl-insert-keyword " OF ")
      (save-excursion
	(vhdl-prepare-search-1
	 (setq entity-exists (vhdl-re-search-backward
			      "\\<entity \\(\\w*\\) is\\>" nil t))
	 (setq string (match-string 1))))
      (if (and entity-exists (not (equal string "")))
	  (insert string)
	(vhdl-template-field "entity name"))
      (vhdl-insert-keyword " IS\n")
      (when (memq vhdl-insert-empty-lines '(unit all)) (insert "\n"))
      (indent-to (+ margin vhdl-basic-offset))
      (setq position (point))
      (insert "\n")
      (when (memq vhdl-insert-empty-lines '(unit all)) (insert "\n"))
      (indent-to margin)
      (vhdl-insert-keyword "END ")
      (unless (vhdl-standard-p '87)
	(vhdl-insert-keyword "CONFIGURATION "))
      (insert name ";")
      (goto-char position))))

(defun vhdl-template-constant ()
  "Insert a constant declaration."
  (interactive)
  (let ((start (point))
	(in-arglist (vhdl-in-argument-list-p)))
    (vhdl-insert-keyword "CONSTANT ")
    (when (vhdl-template-field "name" nil t start (point))
      (insert " : ")
      (when in-arglist (vhdl-insert-keyword "IN "))
      (vhdl-template-field "type")
      (if in-arglist
	  (progn (insert ";")
		 (vhdl-comment-insert-inline))
	(let ((position (point)))
	  (insert " := ")
	  (unless (vhdl-template-field "[initialization]" nil t)
	    (delete-region position (point)))
	  (insert ";")
	  (vhdl-comment-insert-inline))))))

(defun vhdl-template-default ()
  "Insert nothing."
  (interactive)
  (insert " ")
  (unexpand-abbrev)
  (backward-word 1)
  (vhdl-case-word 1)
  (forward-char 1))

(defun vhdl-template-default-indent ()
  "Insert nothing and indent."
  (interactive)
  (insert " ")
  (unexpand-abbrev)
  (backward-word 1)
  (vhdl-case-word 1)
  (forward-char 1)
  (indent-according-to-mode))

(defun vhdl-template-disconnect ()
  "Insert a disconnect statement."
  (interactive)
  (let ((start (point)))
    (vhdl-insert-keyword "DISCONNECT ")
    (when (vhdl-template-field "signal names | OTHERS | ALL"
			       " : " t start (point))
      (vhdl-template-field "type")
      (vhdl-insert-keyword " AFTER ")
      (vhdl-template-field "time expression" ";"))))

(defun vhdl-template-else ()
  "Insert an else statement."
  (interactive)
  (let (margin)
    (vhdl-prepare-search-1
     (vhdl-insert-keyword "ELSE")
     (if (and (save-excursion (vhdl-re-search-backward "\\(\\<when\\>\\|;\\)" nil t))
	      (equal "WHEN" (upcase (match-string 1))))
	 (insert " ")
       (indent-according-to-mode)
       (setq margin (current-indentation))
       (insert "\n")
       (indent-to (+ margin vhdl-basic-offset))))))

(defun vhdl-template-elsif ()
  "Insert an elsif statement."
  (interactive)
  (let ((start (point))
	margin)
    (vhdl-insert-keyword "ELSIF ")
    (when (or (vhdl-sequential-statement-p) (vhdl-standard-p 'ams))
      (when vhdl-conditions-in-parenthesis (insert "("))
      (when (vhdl-template-field "condition" nil t start (point))
	(when vhdl-conditions-in-parenthesis (insert ")"))
	(indent-according-to-mode)
	(setq margin (current-indentation))
	(vhdl-insert-keyword
	 (concat " " (if (vhdl-sequential-statement-p) "THEN" "USE") "\n"))
	(indent-to (+ margin vhdl-basic-offset))))))

(defun vhdl-template-entity ()
  "Insert an entity."
  (interactive)
  (let ((margin (current-indentation))
	(start (point))
	name end-column)
    (vhdl-insert-keyword "ENTITY ")
    (when (setq name (vhdl-template-field "name" nil t start (point)))
      (vhdl-insert-keyword " IS\n\n")
      (indent-to margin)
      (vhdl-insert-keyword "END ")
      (unless (vhdl-standard-p '87) (vhdl-insert-keyword "ENTITY "))
      (insert name ";")
      (setq end-column (current-column))
      (end-of-line -0)
      (indent-to (+ margin vhdl-basic-offset))
      (when (memq vhdl-insert-empty-lines '(unit all)) (insert "\n"))
      (indent-to (+ margin vhdl-basic-offset))
      (when (vhdl-template-generic-list t)
	(when (memq vhdl-insert-empty-lines '(unit all)) (insert "\n")))
      (insert "\n")
      (indent-to (+ margin vhdl-basic-offset))
      (when (vhdl-template-port-list t)
	(when (memq vhdl-insert-empty-lines '(unit all)) (insert "\n")))
      (beginning-of-line 2)
      (forward-char end-column))))

(defun vhdl-template-exit ()
  "Insert an exit statement."
  (interactive)
  (let ((start (point)))
    (vhdl-insert-keyword "EXIT ")
    (if (vhdl-template-field "[loop label]" nil t start (point))
	(let ((position (point)))
	  (vhdl-insert-keyword " WHEN ")
	  (when vhdl-conditions-in-parenthesis (insert "("))
	  (if (vhdl-template-field "[condition]" nil t)
	      (when vhdl-conditions-in-parenthesis (insert ")"))
	    (delete-region position (point))))
      (delete-char -1))
    (insert ";")))

(defun vhdl-template-file ()
  "Insert a file declaration."
  (interactive)
  (let ((start (point)))
    (vhdl-insert-keyword "FILE ")
    (when (vhdl-template-field "name" nil t start (point))
      (insert " : ")
      (vhdl-template-field "type")
      (unless (vhdl-standard-p '87)
	(vhdl-insert-keyword " OPEN ")
	(unless (vhdl-template-field "[READ_MODE | WRITE_MODE | APPEND_MODE]"
				     nil t)
	  (delete-char -6)))
      (vhdl-insert-keyword " IS ")
      (when (vhdl-standard-p '87)
	(vhdl-template-field "[IN | OUT]" " " t))
      (vhdl-template-field "filename-string" nil nil nil nil t)
      (insert ";")
      (vhdl-comment-insert-inline))))

(defun vhdl-template-for ()
  "Insert a block or component configuration if within a configuration
declaration, a configuration specification if within an architecture
declarative part (and not within a subprogram), a for-loop if within a
sequential statement part (subprogram or process), and a for-generate
otherwise."
  (interactive)
  (vhdl-prepare-search-1
   (cond
    ((vhdl-sequential-statement-p)	; sequential statement
     (vhdl-template-for-loop))
    ((and (save-excursion		; configuration declaration
	    (re-search-backward "^\\(configuration\\|end\\)\\>" nil t))
	  (equal "CONFIGURATION" (upcase (match-string 1))))
     (if (eq (vhdl-decision-query
	      "for" "(b)lock or (c)omponent configuration?" t) ?c)
	 (vhdl-template-component-conf)
       (vhdl-template-block-configuration)))
    ((and (save-excursion
	    (re-search-backward		; architecture declarative part
	     "^\\(architecture\\|entity\\|begin\\|end\\)\\>" nil t))
	  (equal "ARCHITECTURE" (upcase (match-string 1))))
     (vhdl-template-configuration-spec))
    (t (vhdl-template-for-generate))))) ; concurrent statement

(defun vhdl-template-for-generate ()
  "Insert a for-generate."
  (interactive)
  (let ((margin (current-indentation))
	(start (point))
	label position)
    (vhdl-insert-keyword ": FOR ")
    (setq position (point-marker))
    (goto-char start)
    (when (setq label (vhdl-template-field "label" nil t start position))
      (goto-char position)
      (vhdl-template-field "loop variable")
      (vhdl-insert-keyword " IN ")
      (vhdl-template-field "range")
      (vhdl-template-generate-body margin label))))

(defun vhdl-template-for-loop ()
  "Insert a for loop."
  (interactive)
  (let ((margin (current-indentation))
	(start (point))
	label index)
    (if (not (eq vhdl-optional-labels 'all))
	(vhdl-insert-keyword "FOR ")
      (vhdl-insert-keyword ": FOR ")
      (goto-char start)
      (setq label (vhdl-template-field "[label]" nil t))
      (unless label (delete-char 2))
      (forward-word 1)
      (forward-char 1))
    (when (setq index (vhdl-template-field "loop variable"
					   nil t start (point)))
      (vhdl-insert-keyword " IN ")
      (vhdl-template-field "range")
      (vhdl-insert-keyword " LOOP\n\n")
      (indent-to margin)
      (vhdl-insert-keyword "END LOOP")
      (if label
	  (insert " " label ";")
	(insert ";")
	(when vhdl-self-insert-comments (insert "  -- " index)))
      (forward-line -1)
      (indent-to (+ margin vhdl-basic-offset)))))

(defun vhdl-template-function (&optional kind)
  "Insert a function declaration or body."
  (interactive)
  (let ((margin (current-indentation))
	(start (point))
	name)
    (vhdl-insert-keyword "FUNCTION ")
    (when (setq name (vhdl-template-field "name" nil t start (point)))
      (vhdl-template-argument-list t)
      (when vhdl-auto-align (vhdl-align-region-groups start (point) 1))
      (end-of-line)
      (insert "\n")
      (indent-to (+ margin vhdl-basic-offset))
      (vhdl-insert-keyword "RETURN ")
      (vhdl-template-field "type")
      (if (if kind (eq kind 'body)
	    (eq (vhdl-decision-query nil "(d)eclaration or (b)ody?") ?b))
	  (progn (vhdl-insert-keyword " IS\n")
		 (vhdl-template-begin-end
		  (unless (vhdl-standard-p '87) "FUNCTION") name margin)
		 (vhdl-comment-block))
	(insert ";")))))

(defun vhdl-template-function-decl ()
  "Insert a function declaration."
  (interactive)
  (vhdl-template-function 'decl))

(defun vhdl-template-function-body ()
  "Insert a function declaration."
  (interactive)
  (vhdl-template-function 'body))

(defun vhdl-template-generate ()
  "Insert a generation scheme."
  (interactive)
  (if (eq (vhdl-decision-query nil "(f)or or (i)f?" t) ?i)
      (vhdl-template-if-generate)
    (vhdl-template-for-generate)))

(defun vhdl-template-generic ()
  "Insert generic declaration, or generic map in instantiation statements."
  (interactive)
  (let ((start (point)))
    (vhdl-prepare-search-1
     (cond
      ((and (save-excursion		; entity declaration
	      (re-search-backward "^\\(entity\\|end\\)\\>" nil t))
	    (equal "ENTITY" (upcase (match-string 1))))
       (vhdl-template-generic-list nil))
      ((or (save-excursion
	     (or (beginning-of-line)
		 (looking-at "^\\s-*\\w+\\s-*:\\s-*\\w+")))
	   (equal 'statement-cont (caar (vhdl-get-syntactic-context))))
       (vhdl-insert-keyword "GENERIC ")
       (vhdl-template-map start))
      (t (vhdl-template-generic-list nil t))))))

(defun vhdl-template-group ()
  "Insert group or group template declaration."
  (interactive)
  (let ((start (point)))
    (if (eq (vhdl-decision-query
	     "group" "(d)eclaration or (t)emplate declaration?" t) ?t)
	(vhdl-template-group-template)
      (vhdl-template-group-decl))))

(defun vhdl-template-group-decl ()
  "Insert group declaration."
  (interactive)
  (let ((start (point)))
    (vhdl-insert-keyword "GROUP ")
    (when (vhdl-template-field "name" " : " t start (point))
      (vhdl-template-field "template name" " (")
      (vhdl-template-field "constituent list" ");")
      (vhdl-comment-insert-inline))))

(defun vhdl-template-group-template ()
  "Insert group template declaration."
  (interactive)
  (let ((start (point)))
    (vhdl-insert-keyword "GROUP ")
    (when (vhdl-template-field "template name" nil t start (point))
      (vhdl-insert-keyword " IS (")
      (vhdl-template-field "entity class list" ");")
      (vhdl-comment-insert-inline))))

(defun vhdl-template-if ()
  "Insert a sequential if statement or an if-generate statement."
  (interactive)
  (if (vhdl-sequential-statement-p)
      (vhdl-template-if-then)
    (if (and (vhdl-standard-p 'ams)
	     (eq (vhdl-decision-query "if" "(g)enerate or (u)se?" t) ?u))
	(vhdl-template-if-use)
      (vhdl-template-if-generate))))

(defun vhdl-template-if-generate ()
  "Insert an if-generate."
  (interactive)
  (let ((margin (current-indentation))
	(start (point))
	label position)
    (vhdl-insert-keyword ": IF ")
    (setq position (point-marker))
    (goto-char start)
    (when (setq label (vhdl-template-field "label" nil t start position))
      (goto-char position)
      (when vhdl-conditions-in-parenthesis (insert "("))
      (vhdl-template-field "condition")
      (when vhdl-conditions-in-parenthesis (insert ")"))
      (vhdl-template-generate-body margin label))))

(defun vhdl-template-if-then-use (kind)
  "Insert a sequential if statement."
  (interactive)
  (let ((margin (current-indentation))
	(start (point))
	label)
    (if (or (not (eq vhdl-optional-labels 'all)) (vhdl-standard-p '87))
	(vhdl-insert-keyword "IF ")
      (vhdl-insert-keyword ": IF ")
      (goto-char start)
      (setq label (vhdl-template-field "[label]" nil t))
      (unless label (delete-char 2))
      (forward-word 1)
      (forward-char 1))
    (when vhdl-conditions-in-parenthesis (insert "("))
    (when (vhdl-template-field "condition" nil t start (point))
      (when vhdl-conditions-in-parenthesis (insert ")"))
      (vhdl-insert-keyword
       (concat " " (if (eq kind 'then) "THEN" "USE") "\n\n"))
      (indent-to margin)
      (vhdl-insert-keyword "END IF")
      (when label (insert " " label))
      (insert ";")
      (forward-line -1)
      (indent-to (+ margin vhdl-basic-offset)))))

(defun vhdl-template-if-then ()
  "Insert a sequential if statement."
  (interactive)
  (vhdl-template-if-then-use 'then))

(defun vhdl-template-if-use ()
  "Insert a simultaneous if statement."
  (interactive)
  (vhdl-template-if-then-use 'use))

(defun vhdl-template-instance ()
  "Insert a component instantiation statement."
  (interactive)
  (vhdl-template-component-inst))

(defun vhdl-template-library ()
  "Insert a library specification."
  (interactive)
  (let ((margin (current-indentation))
	(start (point))
	name end-pos)
    (vhdl-insert-keyword "LIBRARY ")
    (when (setq name (vhdl-template-field "names" nil t start (point)))
      (insert ";")
      (unless (string-match "," name)
	(setq end-pos (point))
	(insert "\n")
	(indent-to margin)
	(vhdl-insert-keyword "USE ")
	(insert name)
	(vhdl-insert-keyword "..ALL;")
	(backward-char 5)
	(if (vhdl-template-field "package name")
	    (forward-char 5)
	  (delete-region end-pos (+ (point) 5)))))))

(defun vhdl-template-limit ()
  "Insert a limit."
  (interactive)
  (let ((start (point)))
    (vhdl-insert-keyword "LIMIT ")
    (when (vhdl-template-field "quantity names | OTHERS | ALL" " : "
			       t start (point))
      (vhdl-template-field "type")
      (vhdl-insert-keyword " WITH ")
      (vhdl-template-field "real expression" ";"))))

(defun vhdl-template-loop ()
  "Insert a loop."
  (interactive)
  (let ((char (vhdl-decision-query nil "(w)hile, (f)or, or (b)are?" t)))
    (cond ((eq char ?w)
	   (vhdl-template-while-loop))
	  ((eq char ?f)
	   (vhdl-template-for-loop))
	  (t (vhdl-template-bare-loop)))))

(defun vhdl-template-bare-loop ()
  "Insert a loop."
  (interactive)
  (let ((margin (current-indentation))
	(start (point))
	label)
    (if (not (eq vhdl-optional-labels 'all))
	(vhdl-insert-keyword "LOOP ")
      (vhdl-insert-keyword ": LOOP ")
      (goto-char start)
      (setq label (vhdl-template-field "[label]" nil t))
      (unless label (delete-char 2))
      (forward-word 1)
      (delete-char 1))
    (insert "\n\n")
    (indent-to margin)
    (vhdl-insert-keyword "END LOOP")
    (insert (if label (concat " " label ";") ";"))
    (forward-line -1)
    (indent-to (+ margin vhdl-basic-offset))))

(defun vhdl-template-map (&optional start optional secondary)
  "Insert a map specification with association list."
  (interactive)
  (let ((start (or start (point)))
	margin end-pos)
    (vhdl-insert-keyword "MAP (")
    (if (not vhdl-association-list-with-formals)
	(if (vhdl-template-field
	     (concat (and optional "[") "association list" (and optional "]"))
	     ")" (or (not secondary) optional)
	     (and (not secondary) start) (point))
	    t
	  (if (and optional secondary) (delete-region start (point)))
	  nil)
      (if vhdl-argument-list-indent
	  (setq margin (current-column))
	(setq margin (+ (current-indentation) vhdl-basic-offset))
	(insert "\n")
	(indent-to margin))
      (if (vhdl-template-field
	   (concat (and optional "[") "formal" (and optional "]"))
	   " => " (or (not secondary) optional)
	   (and (not secondary) start) (point))
	  (progn
	    (vhdl-template-field "actual" ",")
	    (setq end-pos (point))
	    (insert "\n")
	    (indent-to margin)
	    (while (vhdl-template-field "[formal]" " => " t)
	      (vhdl-template-field "actual" ",")
	      (setq end-pos (point))
	      (insert "\n")
	      (indent-to margin))
	    (delete-region end-pos (point))
	    (delete-char -1)
	    (insert ")")
	    (when vhdl-auto-align (vhdl-align-region-groups start (point) 1))
	    t)
	(when (and optional secondary) (delete-region start (point)))
	nil))))

(defun vhdl-template-modify (&optional noerror)
  "Actualize modification date."
  (interactive)
  (vhdl-prepare-search-2
   (save-excursion
     (goto-char (point-min))
     (if (re-search-forward vhdl-modify-date-prefix-string nil t)
	 (progn (delete-region (point) (progn (end-of-line) (point)))
		(vhdl-template-insert-date))
       (unless noerror
	 (error "ERROR:  Modification date prefix string \"%s\" not found"
			vhdl-modify-date-prefix-string))))))


(defun vhdl-template-modify-noerror ()
  "Call `vhdl-template-modify' with NOERROR non-nil."
  (vhdl-template-modify t))

(defun vhdl-template-nature ()
  "Insert a nature declaration."
  (interactive)
  (let ((start (point))
	name mid-pos end-pos)
    (vhdl-insert-keyword "NATURE ")
    (when (setq name (vhdl-template-field "name" nil t start (point)))
      (vhdl-insert-keyword " IS ")
      (let ((definition
	      (upcase
	       (or (vhdl-template-field
		    "across type | ARRAY | RECORD")
		   ""))))
	(cond ((equal definition "")
	       (insert ";"))
	      ((equal definition "ARRAY")
	       (delete-region (point) (progn (forward-word -1) (point)))
	       (vhdl-template-array 'nature t))
	      ((equal definition "RECORD")
	       (setq mid-pos (point-marker))
	       (delete-region (point) (progn (forward-word -1) (point)))
	       (vhdl-template-record 'nature name t))
	      (t
	       (vhdl-insert-keyword " ACROSS ")
	       (vhdl-template-field "through type")
	       (vhdl-insert-keyword " THROUGH ")
	       (vhdl-template-field "reference name")
	       (vhdl-insert-keyword " REFERENCE;")))
	(when mid-pos
	  (setq end-pos (point-marker))
	  (goto-char mid-pos)
	  (end-of-line))
	(vhdl-comment-insert-inline)
	(when end-pos (goto-char end-pos))))))

(defun vhdl-template-next ()
  "Insert a next statement."
  (interactive)
  (let ((start (point)))
    (vhdl-insert-keyword "NEXT ")
    (if (vhdl-template-field "[loop label]" nil t start (point))
	(let ((position (point)))
	  (vhdl-insert-keyword " WHEN ")
	  (when vhdl-conditions-in-parenthesis (insert "("))
	  (if (vhdl-template-field "[condition]" nil t)
	      (when vhdl-conditions-in-parenthesis (insert ")"))
	    (delete-region position (point))))
      (delete-char -1))
    (insert ";")))

(defun vhdl-template-others ()
  "Insert an others aggregate."
  (interactive)
  (let ((start (point)))
    (if (or (= (preceding-char) ?\() (not vhdl-template-invoked-by-hook))
	(progn (unless vhdl-template-invoked-by-hook (insert "("))
	       (vhdl-insert-keyword "OTHERS => '")
	       (when (vhdl-template-field "value" nil t start (point))
		 (insert "')")))
      (vhdl-insert-keyword "OTHERS "))))

(defun vhdl-template-package (&optional kind)
  "Insert a package specification or body."
  (interactive)
  (let ((margin (current-indentation))
	(start (point))
	name body position)
    (vhdl-insert-keyword "PACKAGE ")
    (setq body (if kind (eq kind 'body)
		 (eq (vhdl-decision-query nil "(d)eclaration or (b)ody?") ?b)))
    (when body
      (vhdl-insert-keyword "BODY ")
      (when (save-excursion
	      (vhdl-prepare-search-1
	       (vhdl-re-search-backward "\\<package \\(\\w+\\) is\\>" nil t)))
	(insert (setq name (match-string 1)))))
    (when (or name
	      (setq name (vhdl-template-field "name" nil t start (point))))
      (vhdl-insert-keyword " IS\n")
      (when (memq vhdl-insert-empty-lines '(unit all)) (insert "\n"))
      (indent-to (+ margin vhdl-basic-offset))
      (setq position (point))
      (insert "\n")
      (when (memq vhdl-insert-empty-lines '(unit all)) (insert "\n"))
      (indent-to margin)
      (vhdl-insert-keyword "END ")
      (unless (vhdl-standard-p '87)
	(vhdl-insert-keyword (concat "PACKAGE " (and body "BODY "))))
      (insert (or name "") ";")
      (goto-char position))))

(defun vhdl-template-package-decl ()
  "Insert a package specification."
  (interactive)
  (vhdl-template-package 'decl))

(defun vhdl-template-package-body ()
  "Insert a package body."
  (interactive)
  (vhdl-template-package 'body))

(defun vhdl-template-port ()
  "Insert a port declaration, or port map in instantiation statements."
  (interactive)
  (let ((start (point)))
    (vhdl-prepare-search-1
     (cond
      ((and (save-excursion		; entity declaration
	      (re-search-backward "^\\(entity\\|end\\)\\>" nil t))
	    (equal "ENTITY" (upcase (match-string 1))))
       (vhdl-template-port-list nil))
      ((or (save-excursion
	     (or (beginning-of-line)
		 (looking-at "^\\s-*\\w+\\s-*:\\s-*\\w+")))
	   (equal 'statement-cont (caar (vhdl-get-syntactic-context))))
       (vhdl-insert-keyword "PORT ")
       (vhdl-template-map start))
      (t (vhdl-template-port-list nil))))))

(defun vhdl-template-procedural ()
  "Insert a procedural."
  (interactive)
  (let ((margin (current-indentation))
	(start (point))
	(case-fold-search t)
	label)
    (vhdl-insert-keyword "PROCEDURAL ")
    (when (memq vhdl-optional-labels '(process all))
      (goto-char start)
      (insert ": ")
      (goto-char start)
      (setq label (vhdl-template-field "[label]" nil t))
      (unless label (delete-char 2))
      (forward-word 1)
      (forward-char 1))
    (unless (vhdl-standard-p '87) (vhdl-insert-keyword "IS"))
    (insert "\n")
    (vhdl-template-begin-end "PROCEDURAL" label margin)
    (vhdl-comment-block)))

(defun vhdl-template-procedure (&optional kind)
  "Insert a procedure declaration or body."
  (interactive)
  (let ((margin (current-indentation))
	(start (point))
	name)
    (vhdl-insert-keyword "PROCEDURE ")
    (when (setq name (vhdl-template-field "name" nil t start (point)))
      (vhdl-template-argument-list)
      (if (if kind (eq kind 'body)
	    (eq (vhdl-decision-query nil "(d)eclaration or (b)ody?") ?b))
	  (progn (vhdl-insert-keyword " IS")
		 (when vhdl-auto-align
		   (vhdl-align-region-groups start (point) 1))
		 (end-of-line) (insert "\n")
		 (vhdl-template-begin-end
		  (unless (vhdl-standard-p '87) "PROCEDURE")
		  name margin)
		 (vhdl-comment-block))
	(insert ";")
	(when vhdl-auto-align (vhdl-align-region-groups start (point) 1))
	(end-of-line)))))

(defun vhdl-template-procedure-decl ()
  "Insert a procedure declaration."
  (interactive)
  (vhdl-template-procedure 'decl))

(defun vhdl-template-procedure-body ()
  "Insert a procedure body."
  (interactive)
  (vhdl-template-procedure 'body))

(defun vhdl-template-process (&optional kind)
  "Insert a process."
  (interactive)
  (let ((margin (current-indentation))
	(start (point))
	label seq input-signals clock reset final-pos)
    (setq seq (if kind (eq kind 'seq)
		(eq (vhdl-decision-query
		     "process" "(c)ombinational or (s)equential?" t) ?s)))
    (vhdl-insert-keyword "PROCESS ")
    (when (memq vhdl-optional-labels '(process all))
      (goto-char start)
      (insert ": ")
      (goto-char start)
      (setq label (vhdl-template-field "[label]" nil t))
      (unless label (delete-char 2))
      (forward-word 1)
      (forward-char 1))
    (insert "(")
    (if (not seq)
	(unless (setq input-signals
		      (vhdl-template-field "[sensitivity list]" ")" t))
	  (setq input-signals "")
	  (delete-char -2))
      (setq clock (or (and (not (equal "" vhdl-clock-name))
			   (progn (insert vhdl-clock-name) vhdl-clock-name))
		      (vhdl-template-field "clock name") "<clock>"))
      (when (eq vhdl-reset-kind 'async)
	(insert ", ")
	(setq reset (or (and (not (equal "" vhdl-reset-name))
			     (progn (insert vhdl-reset-name) vhdl-reset-name))
			(vhdl-template-field "reset name") "<reset>")))
      (insert ")"))
    (unless (vhdl-standard-p '87) (vhdl-insert-keyword " IS"))
    (insert "\n")
    (vhdl-template-begin-end "PROCESS" label margin)
    (when seq (setq reset (vhdl-template-seq-process clock reset)))
    (when vhdl-prompt-for-comments
      (setq final-pos (point-marker))
      (vhdl-prepare-search-2
       (when (and (vhdl-re-search-backward "\\<begin\\>" nil t)
		  (vhdl-re-search-backward "\\<process\\>" nil t))
	 (end-of-line -0)
	 (if (bobp)
	     (progn (insert "\n") (forward-line -1))
	   (insert "\n"))
	 (indent-to margin)
	 (insert "-- purpose: ")
	 (if (not (vhdl-template-field "[description]" nil t))
	     (vhdl-line-kill-entire)
	   (insert "\n")
	   (indent-to margin)
	   (insert "-- type   : ")
	   (insert (if seq "sequential" "combinational") "\n")
	   (indent-to margin)
	   (insert "-- inputs : ")
	   (if (not seq)
	       (insert input-signals)
	     (insert clock ", ")
	     (when reset (insert reset ", "))
	     (unless (vhdl-template-field "[signal names]" nil t)
	       (delete-char -2)))
	   (insert "\n")
	   (indent-to margin)
	   (insert "-- outputs: ")
	   (vhdl-template-field "[signal names]" nil t))))
      (goto-char final-pos))))

(defun vhdl-template-process-comb ()
  "Insert a combinational process."
  (interactive)
  (vhdl-template-process 'comb))

(defun vhdl-template-process-seq ()
  "Insert a sequential process."
  (interactive)
  (vhdl-template-process 'seq))

(defun vhdl-template-quantity ()
  "Insert a quantity declaration."
  (interactive)
  (if (vhdl-in-argument-list-p)
      (let ((start (point)))
	(vhdl-insert-keyword "QUANTITY ")
	(when (vhdl-template-field "names" nil t start (point))
	  (insert " : ")
	  (vhdl-template-field "[IN | OUT]" " " t)
	  (vhdl-template-field "type")
	  (insert ";")
	  (vhdl-comment-insert-inline)))
    (let ((char (vhdl-decision-query
		 "quantity" "(f)ree, (b)ranch, or (s)ource quantity?" t)))
      (cond ((eq char ?f) (vhdl-template-quantity-free))
	    ((eq char ?b) (vhdl-template-quantity-branch))
	    ((eq char ?s) (vhdl-template-quantity-source))
	    (t (vhdl-template-undo (point) (point)))))))

(defun vhdl-template-quantity-free ()
  "Insert a free quantity declaration."
  (interactive)
  (vhdl-insert-keyword "QUANTITY ")
  (vhdl-template-field "names")
  (insert " : ")
  (vhdl-template-field "type")
  (let ((position (point)))
    (insert " := ")
    (unless (vhdl-template-field "[initialization]" nil t)
      (delete-region position (point)))
    (insert ";")
    (vhdl-comment-insert-inline)))

(defun vhdl-template-quantity-branch ()
  "Insert a branch quantity declaration."
  (interactive)
  (let (position)
    (vhdl-insert-keyword "QUANTITY ")
    (when (vhdl-template-field "[across names]" " " t)
      (vhdl-insert-keyword "ACROSS "))
    (when (vhdl-template-field "[through names]" " " t)
      (vhdl-insert-keyword "THROUGH "))
    (vhdl-template-field "plus terminal name")
    (setq position (point))
    (vhdl-insert-keyword " TO ")
    (unless (vhdl-template-field "[minus terminal name]" nil t)
      (delete-region position (point)))
    (insert ";")
    (vhdl-comment-insert-inline)))

(defun vhdl-template-quantity-source ()
  "Insert a source quantity declaration."
  (interactive)
  (vhdl-insert-keyword "QUANTITY ")
  (vhdl-template-field "names")
  (insert " : ")
  (vhdl-template-field "type" " ")
  (if (eq (vhdl-decision-query nil "(s)pectrum or (n)oise?") ?n)
      (progn (vhdl-insert-keyword "NOISE ")
	     (vhdl-template-field "power expression"))
    (vhdl-insert-keyword "SPECTRUM ")
    (vhdl-template-field "magnitude expression" ", ")
    (vhdl-template-field "phase expression"))
  (insert ";")
  (vhdl-comment-insert-inline))

(defun vhdl-template-record (kind &optional name secondary)
  "Insert a record type declaration."
  (interactive)
  (let ((margin (current-column))
	(start (point))
	(first t))
    (vhdl-insert-keyword "RECORD\n")
    (indent-to (+ margin vhdl-basic-offset))
    (when (or (vhdl-template-field "element names"
				   nil (not secondary) start (point))
	      secondary)
      (while (or first (vhdl-template-field "[element names]" nil t))
	(insert " : ")
	(vhdl-template-field (if (eq kind 'type) "type" "nature") ";")
	(vhdl-comment-insert-inline)
	(insert "\n")
	(indent-to (+ margin vhdl-basic-offset))
	(setq first nil))
      (delete-region (line-beginning-position) (point))
      (indent-to margin)
      (vhdl-insert-keyword "END RECORD")
      (unless (vhdl-standard-p '87) (and name (insert " " name)))
      (insert ";")
      (when vhdl-auto-align (vhdl-align-region-groups start (point) 1)))))

(defun vhdl-template-report ()
  "Insert a report statement."
  (interactive)
  (let ((start (point)))
    (vhdl-insert-keyword "REPORT ")
    (if (equal "\"\"" (vhdl-template-field
		       "string expression" nil t start (point) t))
	(delete-char -2)
      (setq start (point))
      (vhdl-insert-keyword " SEVERITY ")
      (unless (vhdl-template-field "[NOTE | WARNING | ERROR | FAILURE]" nil t)
	(delete-region start (point)))
      (insert ";"))))

(defun vhdl-template-return ()
  "Insert a return statement."
  (interactive)
  (let ((start (point)))
    (vhdl-insert-keyword "RETURN ")
    (unless (vhdl-template-field "[expression]" nil t start (point))
      (delete-char -1))
    (insert ";")))

(defun vhdl-template-selected-signal-asst ()
  "Insert a selected signal assignment."
  (interactive)
  (let ((margin (current-indentation))
	(start (point))
	(choices t))
    (let ((position (point)))
      (vhdl-insert-keyword " SELECT ")
      (goto-char position))
    (vhdl-insert-keyword "WITH ")
    (when (vhdl-template-field "selector expression"
			       nil t start (+ (point) 7))
      (forward-word 1)
      (delete-char 1)
      (insert "\n")
      (indent-to (+ margin vhdl-basic-offset))
      (vhdl-template-field "target signal" " <= ")
;      (vhdl-template-field "[GUARDED] [TRANSPORT]")
      (insert "\n")
      (indent-to (+ margin vhdl-basic-offset))
      (vhdl-template-field "waveform")
      (vhdl-insert-keyword " WHEN ")
      (vhdl-template-field "choices" ",")
      (insert "\n")
      (indent-to (+ margin vhdl-basic-offset))
      (while (and choices (vhdl-template-field "[waveform]" nil t))
	(vhdl-insert-keyword " WHEN ")
	(if (setq choices (vhdl-template-field "[choices]" "," t))
	    (progn (insert "\n") (indent-to (+ margin vhdl-basic-offset)))
	  (vhdl-insert-keyword "OTHERS")))
      (when choices
	(fixup-whitespace)
	(delete-char -2))
      (insert ";")
      (when vhdl-auto-align (vhdl-align-region-groups start (point) 1)))))

(defun vhdl-template-signal ()
  "Insert a signal declaration."
  (interactive)
  (let ((start (point))
	(in-arglist (vhdl-in-argument-list-p)))
    (vhdl-insert-keyword "SIGNAL ")
    (when (vhdl-template-field "names" nil t start (point))
      (insert " : ")
      (when in-arglist (vhdl-template-field "[IN | OUT | INOUT]" " " t))
      (vhdl-template-field "type")
      (if in-arglist
	  (progn (insert ";")
		 (vhdl-comment-insert-inline))
	(let ((position (point)))
	  (insert " := ")
	  (unless (vhdl-template-field "[initialization]" nil t)
	    (delete-region position (point)))
	  (insert ";")
	  (vhdl-comment-insert-inline))))))

(defun vhdl-template-subnature ()
  "Insert a subnature declaration."
  (interactive)
  (let ((start (point))
	position)
    (vhdl-insert-keyword "SUBNATURE ")
    (when (vhdl-template-field "name" nil t start (point))
      (vhdl-insert-keyword " IS ")
      (vhdl-template-field "nature" " (")
      (if (vhdl-template-field "[index range]" nil t)
	  (insert ")")
	(delete-char -2))
      (setq position (point))
      (vhdl-insert-keyword " TOLERANCE ")
      (if (equal "\"\"" (vhdl-template-field "[string expression]"
					     nil t nil nil t))
	  (delete-region position (point))
	(vhdl-insert-keyword " ACROSS ")
	(vhdl-template-field "string expression" nil nil nil nil t)
	(vhdl-insert-keyword " THROUGH"))
      (insert ";")
      (vhdl-comment-insert-inline))))

(defun vhdl-template-subprogram-body ()
  "Insert a subprogram body."
  (interactive)
  (if (eq (vhdl-decision-query nil "(p)rocedure or (f)unction?" t) ?f)
      (vhdl-template-function-body)
    (vhdl-template-procedure-body)))

(defun vhdl-template-subprogram-decl ()
  "Insert a subprogram declaration."
  (interactive)
  (if (eq (vhdl-decision-query nil "(p)rocedure or (f)unction?" t) ?f)
      (vhdl-template-function-decl)
    (vhdl-template-procedure-decl)))

(defun vhdl-template-subtype ()
  "Insert a subtype declaration."
  (interactive)
  (let ((start (point)))
    (vhdl-insert-keyword "SUBTYPE ")
    (when (vhdl-template-field "name" nil t start (point))
      (vhdl-insert-keyword " IS ")
      (vhdl-template-field "type" " ")
      (unless
	  (vhdl-template-field "[RANGE value range | ( index range )]" nil t)
	(delete-char -1))
      (insert ";")
      (vhdl-comment-insert-inline))))

(defun vhdl-template-terminal ()
  "Insert a terminal declaration."
  (interactive)
  (let ((start (point)))
    (vhdl-insert-keyword "TERMINAL ")
    (when (vhdl-template-field "names" nil t start (point))
      (insert " : ")
      (vhdl-template-field "nature")
      (insert ";")
      (vhdl-comment-insert-inline))))

(defun vhdl-template-type ()
  "Insert a type declaration."
  (interactive)
  (let ((start (point))
	name mid-pos end-pos)
    (vhdl-insert-keyword "TYPE ")
    (when (setq name (vhdl-template-field "name" nil t start (point)))
      (vhdl-insert-keyword " IS ")
      (let ((definition
	      (upcase
	       (or (vhdl-template-field
		    "[scalar type | ARRAY | RECORD | ACCESS | FILE]" nil t)
		   ""))))
	(cond ((equal definition "")
	       (delete-char -4)
	       (insert ";"))
	      ((equal definition "ARRAY")
	       (delete-region (point) (progn (forward-word -1) (point)))
	       (vhdl-template-array 'type t))
	      ((equal definition "RECORD")
	       (setq mid-pos (point-marker))
	       (delete-region (point) (progn (forward-word -1) (point)))
	       (vhdl-template-record 'type name t))
	      ((equal definition "ACCESS")
	       (insert " ")
	       (vhdl-template-field "type" ";"))
	      ((equal definition "FILE")
	       (vhdl-insert-keyword " OF ")
	       (vhdl-template-field "type" ";"))
	      (t (insert ";")))
	(when mid-pos
	  (setq end-pos (point-marker))
	  (goto-char mid-pos)
	  (end-of-line))
	(vhdl-comment-insert-inline)
	(when end-pos (goto-char end-pos))))))

(defun vhdl-template-use ()
  "Insert a use clause."
  (interactive)
  (let ((start (point)))
    (vhdl-prepare-search-1
     (vhdl-insert-keyword "USE ")
     (when (save-excursion (beginning-of-line) (looking-at "^\\s-*use\\>"))
       (vhdl-insert-keyword "..ALL;")
       (backward-char 6)
       (when (vhdl-template-field "library name" nil t start (+ (point) 6))
	 (forward-char 1)
	 (vhdl-template-field "package name")
	 (forward-char 5))))))

(defun vhdl-template-variable ()
  "Insert a variable declaration."
  (interactive)
  (let ((start (point))
	(in-arglist (vhdl-in-argument-list-p)))
    (vhdl-prepare-search-2
     (if (or (save-excursion
	       (and (vhdl-re-search-backward
		     "\\<function\\|procedure\\|process\\|procedural\\|end\\>"
		     nil t)
		    (not (progn (backward-word 1) (looking-at "\\<end\\>")))))
	     (save-excursion (backward-word 1) (looking-at "\\<shared\\>")))
	 (vhdl-insert-keyword "VARIABLE ")
       (vhdl-insert-keyword "SHARED VARIABLE ")))
    (when (vhdl-template-field "names" nil t start (point))
      (insert " : ")
      (when in-arglist (vhdl-template-field "[IN | OUT | INOUT]" " " t))
      (vhdl-template-field "type")
      (if in-arglist
	  (progn (insert ";")
		 (vhdl-comment-insert-inline))
	(let ((position (point)))
	  (insert " := ")
	  (unless (vhdl-template-field "[initialization]" nil t)
	    (delete-region position (point)))
	  (insert ";")
	  (vhdl-comment-insert-inline))))))

(defun vhdl-template-wait ()
  "Insert a wait statement."
  (interactive)
  (vhdl-insert-keyword "WAIT ")
  (unless (vhdl-template-field
	   "[ON sensitivity list] [UNTIL condition] [FOR time expression]"
	   nil t)
    (delete-char -1))
  (insert ";"))

(defun vhdl-template-when ()
  "Indent correctly if within a case statement."
  (interactive)
  (let ((position (point))
	margin)
    (vhdl-prepare-search-2
     (if (and (= (current-column) (current-indentation))
	      (vhdl-re-search-forward "\\<end\\>" nil t)
	      (looking-at "\\s-*\\<case\\>"))
	 (progn
	   (setq margin (current-indentation))
	   (goto-char position)
	   (delete-horizontal-space)
	   (indent-to (+ margin vhdl-basic-offset)))
       (goto-char position)))
    (vhdl-insert-keyword "WHEN ")))

(defun vhdl-template-while-loop ()
  "Insert a while loop."
  (interactive)
  (let* ((margin (current-indentation))
	 (start (point))
	 label)
    (if (not (eq vhdl-optional-labels 'all))
	(vhdl-insert-keyword "WHILE ")
      (vhdl-insert-keyword ": WHILE ")
      (goto-char start)
      (setq label (vhdl-template-field "[label]" nil t))
      (unless label (delete-char 2))
      (forward-word 1)
      (forward-char 1))
    (when vhdl-conditions-in-parenthesis (insert "("))
    (when (vhdl-template-field "condition" nil t start (point))
      (when vhdl-conditions-in-parenthesis (insert ")"))
      (vhdl-insert-keyword " LOOP\n\n")
      (indent-to margin)
      (vhdl-insert-keyword "END LOOP")
      (insert (if label (concat " " label ";") ";"))
      (forward-line -1)
      (indent-to (+ margin vhdl-basic-offset)))))

(defun vhdl-template-with ()
  "Insert a with statement (i.e. selected signal assignment)."
  (interactive)
  (vhdl-prepare-search-1
   (if (and (save-excursion (vhdl-re-search-backward "\\(\\<limit\\>\\|;\\)"))
	    (equal ";" (match-string 1)))
       (vhdl-template-selected-signal-asst)
     (vhdl-insert-keyword "WITH "))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special templates

(defun vhdl-template-clocked-wait ()
  "Insert a wait statement for rising/falling clock edge."
  (interactive)
  (let ((start (point))
	clock)
    (vhdl-insert-keyword "WAIT UNTIL ")
    (when (setq clock
		(or (and (not (equal "" vhdl-clock-name))
			 (progn (insert vhdl-clock-name) vhdl-clock-name))
		    (vhdl-template-field "clock name" nil t start (point))))
      (insert "'event")
      (vhdl-insert-keyword " AND ")
      (insert clock)
      (insert
       " = " (if vhdl-clock-rising-edge vhdl-one-string vhdl-zero-string) ";")
      (vhdl-comment-insert-inline
       (concat (if vhdl-clock-rising-edge "rising" "falling")
	       " clock edge")))))

(defun vhdl-template-seq-process (clock reset)
  "Insert a template for the body of a sequential process."
  (let ((margin (current-indentation))
	position)
    (vhdl-insert-keyword "IF ")
    (when (eq vhdl-reset-kind 'async)
      (insert reset " = "
	      (if vhdl-reset-active-high vhdl-one-string vhdl-zero-string))
      (vhdl-insert-keyword " THEN")
      (vhdl-comment-insert-inline
       (concat "asynchronous reset (active "
	       (if vhdl-reset-active-high "high" "low") ")"))
      (insert "\n") (indent-to (+ margin vhdl-basic-offset))
      (setq position (point))
      (insert "\n") (indent-to margin)
      (vhdl-insert-keyword "ELSIF "))
    (if (eq vhdl-clock-edge-condition 'function)
	(insert (if vhdl-clock-rising-edge "rising" "falling")
		"_edge(" clock ")")
      (insert clock "'event")
      (vhdl-insert-keyword " AND ")
      (insert clock " = "
	      (if vhdl-clock-rising-edge vhdl-one-string vhdl-zero-string)))
    (vhdl-insert-keyword " THEN")
    (vhdl-comment-insert-inline
     (concat (if vhdl-clock-rising-edge "rising" "falling") " clock edge"))
    (insert "\n") (indent-to (+ margin vhdl-basic-offset))
    (when (eq vhdl-reset-kind 'sync)
      (vhdl-insert-keyword "IF ")
      (setq reset (or (and (not (equal "" vhdl-reset-name))
			   (progn (insert vhdl-reset-name) vhdl-reset-name))
		      (vhdl-template-field "reset name") "<reset>"))
      (insert " = "
	      (if vhdl-reset-active-high vhdl-one-string vhdl-zero-string))
      (vhdl-insert-keyword " THEN")
      (vhdl-comment-insert-inline
       (concat "synchronous reset (active "
	       (if vhdl-reset-active-high "high" "low") ")"))
      (insert "\n") (indent-to (+ margin (* 2 vhdl-basic-offset)))
      (setq position (point))
      (insert "\n") (indent-to (+ margin vhdl-basic-offset))
      (vhdl-insert-keyword "ELSE")
      (insert "\n") (indent-to (+ margin (* 2 vhdl-basic-offset)))
      (insert "\n") (indent-to (+ margin vhdl-basic-offset))
      (vhdl-insert-keyword "END IF;"))
    (when (eq vhdl-reset-kind 'none)
      (setq position (point)))
    (insert "\n") (indent-to margin)
    (vhdl-insert-keyword "END IF;")
    (goto-char position)
    reset))

(defun vhdl-template-standard-package (library package)
  "Insert specification of a standard package.  Include a library
specification, if not already there."
  (let ((margin (current-indentation)))
    (unless (equal library "std")
      (unless (or (save-excursion
		    (vhdl-prepare-search-1
		     (and (not (bobp))
			  (re-search-backward
			   (concat "^\\s-*\\(\\(library\\)\\s-+\\(\\w+\\s-*,\\s-*\\)*"
				   library "\\|end\\)\\>") nil t)
			  (match-string 2))))
		  (equal (downcase library) "work"))
	(vhdl-insert-keyword "LIBRARY ")
	(insert library ";")
	(when package
	  (insert "\n")
	  (indent-to margin)))
      (when package
	(vhdl-insert-keyword "USE ")
	(insert library "." package)
	(vhdl-insert-keyword ".ALL;")))))

(defun vhdl-template-package-math-complex ()
  "Insert specification of `math_complex' package."
  (interactive)
  (vhdl-template-standard-package "ieee" "math_complex"))

(defun vhdl-template-package-math-real ()
  "Insert specification of `math_real' package."
  (interactive)
  (vhdl-template-standard-package "ieee" "math_real"))

(defun vhdl-template-package-numeric-bit ()
  "Insert specification of `numeric_bit' package."
  (interactive)
  (vhdl-template-standard-package "ieee" "numeric_bit"))

(defun vhdl-template-package-numeric-std ()
  "Insert specification of `numeric_std' package."
  (interactive)
  (vhdl-template-standard-package "ieee" "numeric_std"))

(defun vhdl-template-package-std-logic-1164 ()
  "Insert specification of `std_logic_1164' package."
  (interactive)
  (vhdl-template-standard-package "ieee" "std_logic_1164"))

(defun vhdl-template-package-std-logic-arith ()
  "Insert specification of `std_logic_arith' package."
  (interactive)
  (vhdl-template-standard-package "ieee" "std_logic_arith"))

(defun vhdl-template-package-std-logic-misc ()
  "Insert specification of `std_logic_misc' package."
  (interactive)
  (vhdl-template-standard-package "ieee" "std_logic_misc"))

(defun vhdl-template-package-std-logic-signed ()
  "Insert specification of `std_logic_signed' package."
  (interactive)
  (vhdl-template-standard-package "ieee" "std_logic_signed"))

(defun vhdl-template-package-std-logic-textio ()
  "Insert specification of `std_logic_textio' package."
  (interactive)
  (vhdl-template-standard-package "ieee" "std_logic_textio"))

(defun vhdl-template-package-std-logic-unsigned ()
  "Insert specification of `std_logic_unsigned' package."
  (interactive)
  (vhdl-template-standard-package "ieee" "std_logic_unsigned"))

(defun vhdl-template-package-textio ()
  "Insert specification of `textio' package."
  (interactive)
  (vhdl-template-standard-package "std" "textio"))

(defun vhdl-template-directive (directive)
  "Insert directive."
  (unless (= (current-indentation) (current-column))
    (delete-horizontal-space)
    (insert "  "))
  (insert "-- pragma " directive))

(defun vhdl-template-directive-translate-on ()
  "Insert directive 'translate_on'."
  (interactive)
  (vhdl-template-directive "translate_on"))

(defun vhdl-template-directive-translate-off ()
  "Insert directive 'translate_off'."
  (interactive)
  (vhdl-template-directive "translate_off"))

(defun vhdl-template-directive-synthesis-on ()
  "Insert directive 'synthesis_on'."
  (interactive)
  (vhdl-template-directive "synthesis_on"))

(defun vhdl-template-directive-synthesis-off ()
  "Insert directive 'synthesis_off'."
  (interactive)
  (vhdl-template-directive "synthesis_off"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Header and footer templates

(defun vhdl-template-header (&optional file-title)
  "Insert a VHDL file header."
  (interactive)
  (unless (equal vhdl-file-header "")
    (let (pos)
      (save-excursion
	(goto-char (point-min))
	(vhdl-insert-string-or-file vhdl-file-header)
	(setq pos (point-marker)))
      (vhdl-template-replace-header-keywords
       (point-min-marker) pos file-title))))

(defun vhdl-template-footer ()
  "Insert a VHDL file footer."
  (interactive)
  (unless (equal vhdl-file-footer "")
    (let (pos)
      (save-excursion
	(goto-char (point-max))
	(setq pos (point-marker))
	(vhdl-insert-string-or-file vhdl-file-footer)
	(unless (= (preceding-char) ?\n)
	  (insert "\n")))
      (vhdl-template-replace-header-keywords pos (point-max-marker)))))

(defun vhdl-template-replace-header-keywords (beg end &optional file-title
						  is-model)
  "Replace keywords in header and footer."
  (let ((project-title (or (nth 0 (aget vhdl-project-alist vhdl-project)) ""))
	(project-desc (or (nth 9 (aget vhdl-project-alist vhdl-project)) ""))
	pos)
    (vhdl-prepare-search-2
     (save-excursion
       (goto-char beg)
       (while (search-forward "<projectdesc>" end t)
	 (replace-match project-desc t t))
       (goto-char beg)
       (while (search-forward "<filename>" end t)
	 (replace-match (buffer-name) t t))
       (goto-char beg)
       (while (search-forward "<copyright>" end t)
	 (replace-match vhdl-copyright-string t t))
       (goto-char beg)
       (while (search-forward "<author>" end t)
	 (replace-match "" t t)
	 (insert (user-full-name))
	 (when user-mail-address (insert "  <" user-mail-address ">")))
       (goto-char beg)
       (while (search-forward "<login>" end t)
	 (replace-match (user-login-name) t t))
       (goto-char beg)
       (while (search-forward "<project>" end t)
	 (replace-match project-title t t))
       (goto-char beg)
       (while (search-forward "<company>" end t)
	 (replace-match vhdl-company-name t t))
       (goto-char beg)
       (while (search-forward "<platform>" end t)
	 (replace-match vhdl-platform-spec t t))
       (goto-char beg)
       (while (search-forward "<standard>" end t)
	 (replace-match
	  (concat "VHDL" (cond ((vhdl-standard-p '87) "'87")
			       ((vhdl-standard-p '93) "'93"))
		  (when (vhdl-standard-p 'ams) ", VHDL-AMS")
		  (when (vhdl-standard-p 'math) ", Math Packages")) t t))
       (goto-char beg)
       ;; Replace <RCS> with $, so that RCS for the source is
       ;; not over-enthusiastic with replacements
       (while (search-forward "<RCS>" end t)
	 (replace-match "$" nil t))
       (goto-char beg)
       (while (search-forward "<date>" end t)
	 (replace-match "" t t)
	 (vhdl-template-insert-date))
       (goto-char beg)
       (while (search-forward "<year>" end t)
	 (replace-match (format-time-string "%Y" nil) t t))
       (goto-char beg)
       (when file-title
	 (while (search-forward "<title string>" end t)
	   (replace-match file-title t t))
	 (goto-char beg))
       (let (string)
	 (while
	     (re-search-forward "<\\(\\(\\w\\|\\s_\\)*\\) string>" end t)
	   (setq string (read-string (concat (match-string 1) ": ")))
	   (replace-match string t t)))
       (goto-char beg)
       (when (and (not is-model) (search-forward "<cursor>" end t))
	 (replace-match "" t t)
	 (setq pos (point))))
     (when pos (goto-char pos))
     (unless is-model
       (when (or (not project-title) (equal project-title ""))
	 (message "You can specify a project title in user option `vhdl-project-alist'"))
       (when (or (not project-desc) (equal project-desc ""))
	 (message "You can specify a project description in user option `vhdl-project-alist'"))
       (when (equal vhdl-platform-spec "")
	 (message "You can specify a platform in user option `vhdl-platform-spec'"))
       (when (equal vhdl-company-name "")
	 (message "You can specify a company name in user option `vhdl-company-name'"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Comment templates and functions

(defun vhdl-comment-indent ()
  "Indent comments."
  (let* ((position (point))
	 (col
	  (progn
	    (forward-line -1)
	    (if (re-search-forward "--" position t)
		(- (current-column) 2)	; existing comment at bol stays there
	      (goto-char position)
	      (skip-chars-backward " \t")
	      (max comment-column	; else indent to comment column
		   (1+ (current-column))))))) ; except leave at least one space
    (goto-char position)
    col))

(defun vhdl-comment-insert ()
  "Start a comment at the end of the line.
If on line with code, indent at least `comment-column'.
If starting after end-comment-column, start a new line."
  (interactive)
  (when (> (current-column) end-comment-column) (newline-and-indent))
  (if (or (looking-at "\\s-*$") ; end of line
	  (and (not unread-command-events) ; called with key binding or menu
	       (not (end-of-line))))
      (let (margin)
	(while (= (preceding-char) ?-) (delete-char -1))
	(setq margin (current-column))
	(delete-horizontal-space)
	(if (bolp)
	    (progn (indent-to margin) (insert "--"))
	  (insert "  ")
	  (indent-to comment-column)
	  (insert "--"))
	(if (not unread-command-events) (insert " ")))
    ;; else code following current point implies commenting out code
    (let (next-input code)
      (while (= (preceding-char) ?-) (delete-char -2))
      (while (= (setq next-input (read-char)) 13) ; CR
	(insert "--") ; or have a space after it?
	(forward-char -2)
	(forward-line 1)
	(message "Enter CR if commenting out a line of code.")
	(setq code t))
      (unless code
	(insert "--")) ; hardwire to 1 space or use vhdl-basic-offset?
      (setq unread-command-events
	    (list (vhdl-character-to-event next-input)))))) ; pushback the char

(defun vhdl-comment-display (&optional line-exists)
  "Add 2 comment lines at the current indent, making a display comment."
  (interactive)
  (let ((margin (current-indentation)))
    (unless line-exists (vhdl-comment-display-line))
    (insert "\n") (indent-to margin)
    (insert "\n") (indent-to margin)
    (vhdl-comment-display-line)
    (end-of-line -0)
    (insert "-- ")))

(defun vhdl-comment-display-line ()
  "Displays one line of dashes."
  (interactive)
  (while (= (preceding-char) ?-) (delete-char -2))
  (let* ((col (current-column))
	 (len (- end-comment-column col)))
    (insert-char ?- len)))

(defun vhdl-comment-append-inline ()
  "Append empty inline comment to current line."
  (interactive)
  (end-of-line)
  (delete-horizontal-space)
  (insert "  ")
  (indent-to comment-column)
  (insert "-- "))

(defun vhdl-comment-insert-inline (&optional string always-insert)
  "Insert inline comment."
  (when (or (and string (or vhdl-self-insert-comments always-insert))
	    (and (not string) vhdl-prompt-for-comments))
    (let ((position (point)))
      (insert "  ")
      (indent-to comment-column)
      (insert "-- ")
      (if (not (or (and string (progn (insert string) t))
		   (vhdl-template-field "[comment]" nil t)))
	  (delete-region position (point))
	(while (= (preceding-char) ?\ ) (delete-char -1))
	;; (when (> (current-column) end-comment-column)
	;;   (setq position (point-marker))
	;;   (re-search-backward "-- ")
	;;   (insert "\n")
	;;   (indent-to comment-column)
	;;   (goto-char position))
	))))

(defun vhdl-comment-block ()
  "Insert comment for code block."
  (when vhdl-prompt-for-comments
    (let ((final-pos (point-marker)))
      (vhdl-prepare-search-2
       (when (and (re-search-backward "^\\s-*begin\\>" nil t)
		  (re-search-backward "\\<\\(architecture\\|block\\|function\\|procedure\\|process\\|procedural\\)\\>" nil t))
	 (let (margin)
	   (back-to-indentation)
	   (setq margin (current-column))
	   (end-of-line -0)
	   (if (bobp)
	       (progn (insert "\n") (forward-line -1))
	     (insert "\n"))
	   (indent-to margin)
	   (insert "-- purpose: ")
	   (unless (vhdl-template-field "[description]" nil t)
	     (vhdl-line-kill-entire)))))
      (goto-char final-pos))))

(defun vhdl-comment-uncomment-region (beg end &optional arg)
  "Comment out region if not commented out, uncomment otherwise."
  (interactive "r\nP")
  (save-excursion
    (goto-char (1- end))
    (end-of-line)
    (setq end (point-marker))
    (goto-char beg)
    (beginning-of-line)
    (setq beg (point))
    (if (looking-at comment-start)
	(comment-region beg end '(4))
      (comment-region beg end))))

(defun vhdl-comment-uncomment-line (&optional arg)
  "Comment out line if not commented out, uncomment otherwise."
  (interactive "p")
  (save-excursion
    (beginning-of-line)
    (let ((position (point)))
      (forward-line (or arg 1))
      (vhdl-comment-uncomment-region position (point)))))

(defun vhdl-comment-kill-region (beg end)
  "Kill comments in region."
  (interactive "r")
  (save-excursion
    (goto-char end)
    (setq end (point-marker))
    (goto-char beg)
    (beginning-of-line)
    (while (< (point) end)
      (if (looking-at "^\\(\\s-*--.*\n\\)")
	  (progn (delete-region (match-beginning 1) (match-end 1)))
	(beginning-of-line 2)))))

(defun vhdl-comment-kill-inline-region (beg end)
  "Kill inline comments in region."
  (interactive "r")
  (save-excursion
    (goto-char end)
    (setq end (point-marker))
    (goto-char beg)
    (beginning-of-line)
    (while (< (point) end)
      (when (looking-at "^.*[^ \t\n-]+\\(\\s-*--.*\\)$")
	(delete-region (match-beginning 1) (match-end 1)))
      (beginning-of-line 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subtemplates

(defun vhdl-template-begin-end (construct name margin &optional empty-lines)
  "Insert a begin ... end pair with optional name after the end.
Point is left between them."
  (let (position)
    (when (or empty-lines (eq vhdl-insert-empty-lines 'all)) (insert "\n"))
    (indent-to margin)
    (vhdl-insert-keyword "BEGIN")
    (when (and (or construct name) vhdl-self-insert-comments)
      (insert "  --")
      (when construct (insert " ") (vhdl-insert-keyword construct))
      (when name (insert " " name)))
    (insert "\n")
    (when (or empty-lines (eq vhdl-insert-empty-lines 'all)) (insert "\n"))
    (indent-to (+ margin vhdl-basic-offset))
    (setq position (point))
    (insert "\n")
    (when (or empty-lines (eq vhdl-insert-empty-lines 'all)) (insert "\n"))
    (indent-to margin)
    (vhdl-insert-keyword "END")
    (when construct (insert " ") (vhdl-insert-keyword construct))
    (insert (if name (concat " " name) "") ";")
    (goto-char position)))

(defun vhdl-template-argument-list (&optional is-function)
  "Read from user a procedure or function argument list."
  (insert " (")
  (let ((margin (current-column))
	(start (point))
	(end-pos (point))
	not-empty interface semicolon-pos)
    (unless vhdl-argument-list-indent
      (setq margin (+ (current-indentation) vhdl-basic-offset))
      (insert "\n")
      (indent-to margin))
    (setq interface (vhdl-template-field
		     (concat "[CONSTANT | SIGNAL"
			     (unless is-function " | VARIABLE") "]") " " t))
    (while (vhdl-template-field "[names]" nil t)
      (setq not-empty t)
      (insert " : ")
      (unless is-function
	(if (and interface (equal (upcase interface) "CONSTANT"))
	    (vhdl-insert-keyword "IN ")
	  (vhdl-template-field "[IN | OUT | INOUT]" " " t)))
      (vhdl-template-field "type")
      (setq semicolon-pos (point))
      (insert ";")
      (vhdl-comment-insert-inline)
      (setq end-pos (point))
      (insert "\n")
      (indent-to margin)
      (setq interface (vhdl-template-field
		       (concat "[CONSTANT | SIGNAL"
			       (unless is-function " | VARIABLE") "]") " " t)))
    (delete-region end-pos (point))
    (when semicolon-pos (goto-char semicolon-pos))
    (if not-empty
	(progn (delete-char 1) (insert ")"))
      (delete-char -2))))

(defun vhdl-template-generic-list (optional &optional no-value)
  "Read from user a generic spec argument list."
  (let (margin
	(start (point)))
    (vhdl-insert-keyword "GENERIC (")
    (setq margin (current-column))
    (unless vhdl-argument-list-indent
      (let ((position (point)))
	(back-to-indentation)
	(setq margin (+ (current-column) vhdl-basic-offset))
	(goto-char position)
	(insert "\n")
	(indent-to margin)))
    (let ((vhdl-generics (vhdl-template-field
			  (concat (and optional "[") "name"
				  (and no-value "s") (and optional "]"))
			  nil optional)))
      (if (not vhdl-generics)
	  (if optional
	      (progn (vhdl-line-kill-entire) (end-of-line -0)
		     (unless vhdl-argument-list-indent
		       (vhdl-line-kill-entire) (end-of-line -0)))
	    (vhdl-template-undo start (point))
	    nil )
	(insert " : ")
	(let (semicolon-pos end-pos)
	  (while vhdl-generics
	    (vhdl-template-field "type")
	    (if no-value
		(progn (setq semicolon-pos (point))
		       (insert ";"))
	      (insert " := ")
	      (unless (vhdl-template-field "[value]" nil t)
		(delete-char -4))
	      (setq semicolon-pos (point))
	      (insert ";"))
	    (vhdl-comment-insert-inline)
	    (setq end-pos (point))
 	    (insert "\n")
	    (indent-to margin)
	    (setq vhdl-generics (vhdl-template-field
				 (concat "[name" (and no-value "s") "]")
				 " : " t)))
	  (delete-region end-pos (point))
	  (goto-char semicolon-pos)
	  (insert ")")
	  (end-of-line)
	  (when vhdl-auto-align (vhdl-align-region-groups start (point) 1))
	  t)))))

(defun vhdl-template-port-list (optional)
  "Read from user a port spec argument list."
  (let ((start (point))
	margin vhdl-ports object)
    (vhdl-insert-keyword "PORT (")
    (setq margin (current-column))
    (unless vhdl-argument-list-indent
      (let ((position (point)))
	(back-to-indentation)
	(setq margin (+ (current-column) vhdl-basic-offset))
	(goto-char position)
	(insert "\n")
	(indent-to margin)))
    (when (vhdl-standard-p 'ams)
      (setq object (vhdl-template-field "[SIGNAL | TERMINAL | QUANTITY]"
					" " t)))
    (setq vhdl-ports (vhdl-template-field
		      (concat (and optional "[") "names" (and optional "]"))
		      nil optional))
    (if (not vhdl-ports)
	(if optional
	    (progn (vhdl-line-kill-entire) (end-of-line -0)
		   (unless vhdl-argument-list-indent
		     (vhdl-line-kill-entire) (end-of-line -0)))
	  (vhdl-template-undo start (point))
	  nil)
      (insert " : ")
      (let (semicolon-pos end-pos)
	(while vhdl-ports
	  (cond ((or (null object) (equal "SIGNAL" (upcase object)))
		 (vhdl-template-field "IN | OUT | INOUT" " "))
		((equal "QUANTITY" (upcase object))
		 (vhdl-template-field "[IN | OUT]" " " t)))
	  (vhdl-template-field
	   (if (and object (equal "TERMINAL" (upcase object)))
		    "nature" "type"))
	  (setq semicolon-pos (point))
	  (insert ";")
	  (vhdl-comment-insert-inline)
	  (setq end-pos (point))
	  (insert "\n")
	  (indent-to margin)
	  (when (vhdl-standard-p 'ams)
	    (setq object (vhdl-template-field "[SIGNAL | TERMINAL | QUANTITY]"
					      " " t)))
	  (setq vhdl-ports (vhdl-template-field "[names]" " : " t)))
	(delete-region end-pos (point))
	(goto-char semicolon-pos)
	(insert ")")
	(end-of-line)
	(when vhdl-auto-align (vhdl-align-region-groups start end-pos 1))
	t))))

(defun vhdl-template-generate-body (margin label)
  "Insert body for generate template."
  (vhdl-insert-keyword " GENERATE")
;   (if (not (vhdl-standard-p '87))
;       (vhdl-template-begin-end "GENERATE" label margin)
  (insert "\n\n")
  (indent-to margin)
  (vhdl-insert-keyword "END GENERATE ")
  (insert label ";")
  (end-of-line 0)
  (indent-to (+ margin vhdl-basic-offset)))

(defun vhdl-template-insert-date ()
  "Insert date in appropriate format."
  (interactive)
  (insert
   (cond
    ;; 'american, 'european, 'scientific kept for backward compatibility
    ((eq vhdl-date-format 'american) (format-time-string "%m/%d/%Y" nil))
    ((eq vhdl-date-format 'european) (format-time-string "%d.%m.%Y" nil))
    ((eq vhdl-date-format 'scientific) (format-time-string "%Y/%m/%d" nil))
    (t (format-time-string vhdl-date-format nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Help functions

(defun vhdl-electric-space (count)
  "Expand abbreviations and self-insert space(s), do indent-new-comment-line
if in comment and past end-comment-column."
  (interactive "p")
  (cond ((vhdl-in-comment-p)
	 (self-insert-command count)
	 (cond ((>= (current-column) (+ 2 end-comment-column))
		(backward-char 1)
		(skip-chars-backward "^ \t\n")
		(indent-new-comment-line)
		(skip-chars-forward "^ \t\n")
		(forward-char 1))
	       ((>= (current-column) end-comment-column)
		(indent-new-comment-line))
	       (t nil)))
	((or (and (>= (preceding-char) ?a) (<= (preceding-char) ?z))
	     (and (>= (preceding-char) ?A) (<= (preceding-char) ?Z)))
	 (vhdl-prepare-search-1
	  (or (expand-abbrev) (vhdl-fix-case-word -1)))
	 (self-insert-command count))
	(t (self-insert-command count))))

(defun vhdl-template-field (prompt &optional follow-string optional
				   begin end is-string default)
  "Prompt for string and insert it in buffer with optional FOLLOW-STRING.
If OPTIONAL is nil, the prompt is left if an empty string is inserted.  If
an empty string is inserted, return nil and call `vhdl-template-undo' for
the region between BEGIN and END.  IS-STRING indicates whether a string
with double-quotes is to be inserted.  DEFAULT specifies a default string."
  (let ((position (point))
	string)
    (insert "<" prompt ">")
    (setq string
	  (condition-case ()
	      (read-from-minibuffer (concat prompt ": ")
				    (or (and is-string '("\"\"" . 2)) default)
				    vhdl-minibuffer-local-map)
	    (quit (if (and optional begin end)
		      (progn (beep) "")
		    (keyboard-quit)))))
    (when (or (not (equal string "")) optional)
      (delete-region position (point)))
    (when (and (equal string "") optional begin end)
      (vhdl-template-undo begin end)
      (message "Template aborted"))
    (unless (equal string "")
      (insert string)
      (vhdl-fix-case-region-1 position (point) vhdl-upper-case-keywords
			      vhdl-keywords-regexp)
      (vhdl-fix-case-region-1 position (point) vhdl-upper-case-types
			      vhdl-types-regexp)
      (vhdl-fix-case-region-1 position (point) vhdl-upper-case-attributes
			      (concat "'" vhdl-attributes-regexp))
      (vhdl-fix-case-region-1 position (point) vhdl-upper-case-enum-values
			      vhdl-enum-values-regexp))
    (when (or (not (equal string "")) (not optional))
      (insert (or follow-string "")))
    (if (equal string "") nil string)))

(defun vhdl-decision-query (string prompt &optional optional)
  "Query a decision from the user."
  (let ((start (point)))
    (when string (vhdl-insert-keyword (concat string " ")))
    (message "%s" (or prompt ""))
    (let ((char (read-char)))
      (delete-region start (point))
      (if (and optional (eq char ?\r))
	  (progn (insert " ")
		 (unexpand-abbrev)
		 (throw 'abort "ERROR:  Template aborted"))
	char))))

(defun vhdl-insert-keyword (keyword)
  "Insert KEYWORD and adjust case."
  (insert (if vhdl-upper-case-keywords (upcase keyword) (downcase keyword))))

(defun vhdl-case-keyword (keyword)
  "Adjust case of KEYWORD."
  (if vhdl-upper-case-keywords (upcase keyword) (downcase keyword)))

(defun vhdl-case-word (num)
  "Adjust case of following NUM words."
  (if vhdl-upper-case-keywords (upcase-word num) (downcase-word num)))

(defun vhdl-minibuffer-tab (&optional prefix-arg)
  "If preceding character is part of a word or a paren then hippie-expand,
else insert tab (used for word completion in VHDL minibuffer)."
  (interactive "P")
  (cond
   ;; expand word
   ((= (char-syntax (preceding-char)) ?w)
    (let ((case-fold-search (not vhdl-word-completion-case-sensitive))
	  (case-replace nil)
	  (hippie-expand-only-buffers
	   (or (and (boundp 'hippie-expand-only-buffers)
		    hippie-expand-only-buffers)
	       '(vhdl-mode))))
      (vhdl-expand-abbrev prefix-arg)))
   ;; expand parenthesis
   ((or (= (preceding-char) ?\() (= (preceding-char) ?\)))
    (let ((case-fold-search (not vhdl-word-completion-case-sensitive))
	  (case-replace nil))
      (vhdl-expand-paren prefix-arg)))
   ;; insert tab
   (t (insert-tab))))

(defun vhdl-template-search-prompt ()
  "Search for left out template prompts and query again."
  (interactive)
  (vhdl-prepare-search-2
   (when (or (re-search-forward
	      (concat "<\\(" vhdl-template-prompt-syntax "\\)>") nil t)
	     (re-search-backward
	      (concat "<\\(" vhdl-template-prompt-syntax "\\)>") nil t))
     (let ((string (match-string 1)))
       (replace-match "")
       (vhdl-template-field string)))))

(defun vhdl-template-undo (begin end)
  "Undo aborted template by deleting region and unexpanding the keyword."
  (cond (vhdl-template-invoked-by-hook
	 (goto-char end)
	 (insert " ")
	 (delete-region begin end)
	 (unexpand-abbrev))
	(t (delete-region begin end))))

(defun vhdl-insert-string-or-file (string)
  "Insert STRING or file contents if STRING is an existing file name."
  (unless (equal string "")
    (let ((file-name
	   (progn (string-match "^\\([^\n]+\\)" string)
		  (vhdl-resolve-env-variable (match-string 1 string)))))
      (if (file-exists-p file-name)
	   (forward-char (cadr (insert-file-contents file-name)))
	(insert string)))))

(defun vhdl-beginning-of-block ()
  "Move cursor to the beginning of the enclosing block."
  (let (pos)
    (save-excursion
      (beginning-of-line)
      ;; search backward for block beginning or end
      (while (or (while (and (setq pos (re-search-backward "^\\s-*\\(\\(end\\)\\|\\(\\(impure\\|pure\\)[ \t\n]+\\)?\\(function\\|procedure\\)\\|\\(for\\)\\|\\(architecture\\|component\\|configuration\\|entity\\|package\\|record\\|units\\)\\|\\(\\w+[ \t\n]*:[ \t\n]*\\)?\\(postponed[ \t\n]+\\)?\\(block\\|case\\|for\\|if\\|procedural\\|process\\|while\\)\\)\\>" nil t))
			     ;; not consider subprogram declarations
			     (or (and (match-string 5)
				      (save-match-data
					(save-excursion
					  (goto-char (match-end 5))
					  (forward-word 1)
					  (vhdl-forward-syntactic-ws)
					  (when (looking-at "(")
					    (forward-sexp))
					  (re-search-forward "\\<is\\>\\|\\(;\\)" nil t))
					(match-string 1)))
				 ;; not consider configuration specifications
				 (and (match-string 6)
				      (save-match-data
					(save-excursion
					  (vhdl-end-of-block)
					  (beginning-of-line)
					  (not (looking-at "^\\s-*end\\s-+\\(for\\|generate\\|loop\\)\\>"))))))))
		 (match-string 2))
	;; skip subblock if block end found
	(vhdl-beginning-of-block)))
    (when pos (goto-char pos))))

(defun vhdl-end-of-block ()
  "Move cursor to the end of the enclosing block."
  (let (pos)
    (save-excursion
      (end-of-line)
      ;; search forward for block beginning or end
      (while (or (while (and (setq pos (re-search-forward "^\\s-*\\(\\(end\\)\\|\\(\\(impure\\|pure\\)[ \t\n]+\\)?\\(function\\|procedure\\)\\|\\(for\\)\\|\\(architecture\\|component\\|configuration\\|entity\\|package\\|record\\|units\\)\\|\\(\\w+[ \t\n]*:[ \t\n]*\\)?\\(postponed[ \t\n]+\\)?\\(block\\|case\\|for\\|if\\|procedural\\|process\\|while\\)\\)\\>" nil t))
			     ;; not consider subprogram declarations
			     (or (and (match-string 5)
				      (save-match-data
					(save-excursion (re-search-forward "\\<is\\>\\|\\(;\\)" nil t))
					(match-string 1)))
				 ;; not consider configuration specifications
				 (and (match-string 6)
				      (save-match-data
					(save-excursion
					  (vhdl-end-of-block)
					  (beginning-of-line)
					  (not (looking-at "^\\s-*end\\s-+\\(for\\|generate\\|loop\\)\\>"))))))))
		 (not (match-string 2)))
	;; skip subblock if block beginning found
	(vhdl-end-of-block)))
    (when pos (goto-char pos))))

(defun vhdl-sequential-statement-p ()
  "Check if point is within sequential statement part."
  (let ((start (point)))
    (save-excursion
      (vhdl-prepare-search-2
       ;; is sequential statement if ...
       (and (re-search-backward "^\\s-*begin\\>" nil t)
	    ;; ... point is between "begin" and "end" of ...
	    (progn (vhdl-end-of-block)
		   (< start (point)))
	    ;; ... a sequential block
	    (progn (vhdl-beginning-of-block)
		   (looking-at "^\\s-*\\(\\(\\w+[ \t\n]+\\)?\\(function\\|procedure\\)\\|\\(\\w+[ \t\n]*:[ \t\n]*\\)?\\(\\w+[ \t\n]+\\)?\\(procedural\\|process\\)\\)\\>")))))))

(defun vhdl-in-argument-list-p ()
  "Check if within an argument list."
  (save-excursion
    (vhdl-prepare-search-2
     (or (string-match "arglist"
		       (format "%s" (caar (vhdl-get-syntactic-context))))
	 (progn (beginning-of-line)
		(looking-at "^\\s-*\\(generic\\|port\\|\\(\\(impure\\|pure\\)\\s-+\\|\\)function\\|procedure\\)\\>\\s-*\\(\\w+\\s-*\\)?("))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Abbrev hooks

(defun vhdl-hooked-abbrev (func)
  "Do function, if syntax says abbrev is a keyword, invoked by hooked abbrev,
but not if inside a comment or quote."
  (if (or (vhdl-in-literal)
	  (save-excursion
	    (forward-word -1)
	    (and (looking-at "\\<end\\>") (not (looking-at "\\<end;")))))
      (progn
	(insert " ")
	(unexpand-abbrev)
	(delete-char -1))
    (if (not vhdl-electric-mode)
	(progn
	  (insert " ")
	  (unexpand-abbrev)
	  (backward-word 1)
	  (vhdl-case-word 1)
	  (delete-char 1))
      (let ((invoke-char last-command-event)
	    (abbrev-mode -1)
	    (vhdl-template-invoked-by-hook t))
	(let ((caught (catch 'abort
			(funcall func))))
	  (when (stringp caught) (message "%s" caught)))
	(when (= invoke-char ?-) (setq abbrev-start-location (point)))
	;; delete CR which is still in event queue
	(if (fboundp 'enqueue-eval-event)
	    (enqueue-eval-event 'delete-char -1)
	  (setq unread-command-events	; push back a delete char
		(list (vhdl-character-to-event ?\177))))))))

(defun vhdl-template-alias-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-alias))
(defun vhdl-template-architecture-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-architecture))
(defun vhdl-template-assert-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-assert))
(defun vhdl-template-attribute-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-attribute))
(defun vhdl-template-block-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-block))
(defun vhdl-template-break-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-break))
(defun vhdl-template-case-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-case))
(defun vhdl-template-component-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-component))
(defun vhdl-template-instance-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-instance))
(defun vhdl-template-conditional-signal-asst-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-conditional-signal-asst))
(defun vhdl-template-configuration-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-configuration))
(defun vhdl-template-constant-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-constant))
(defun vhdl-template-disconnect-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-disconnect))
(defun vhdl-template-display-comment-hook ()
  (vhdl-hooked-abbrev 'vhdl-comment-display))
(defun vhdl-template-else-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-else))
(defun vhdl-template-elsif-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-elsif))
(defun vhdl-template-entity-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-entity))
(defun vhdl-template-exit-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-exit))
(defun vhdl-template-file-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-file))
(defun vhdl-template-for-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-for))
(defun vhdl-template-function-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-function))
(defun vhdl-template-generic-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-generic))
(defun vhdl-template-group-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-group))
(defun vhdl-template-library-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-library))
(defun vhdl-template-limit-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-limit))
(defun vhdl-template-if-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-if))
(defun vhdl-template-bare-loop-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-bare-loop))
(defun vhdl-template-map-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-map))
(defun vhdl-template-nature-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-nature))
(defun vhdl-template-next-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-next))
(defun vhdl-template-others-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-others))
(defun vhdl-template-package-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-package))
(defun vhdl-template-port-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-port))
(defun vhdl-template-procedural-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-procedural))
(defun vhdl-template-procedure-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-procedure))
(defun vhdl-template-process-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-process))
(defun vhdl-template-quantity-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-quantity))
(defun vhdl-template-report-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-report))
(defun vhdl-template-return-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-return))
(defun vhdl-template-selected-signal-asst-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-selected-signal-asst))
(defun vhdl-template-signal-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-signal))
(defun vhdl-template-subnature-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-subnature))
(defun vhdl-template-subtype-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-subtype))
(defun vhdl-template-terminal-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-terminal))
(defun vhdl-template-type-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-type))
(defun vhdl-template-use-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-use))
(defun vhdl-template-variable-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-variable))
(defun vhdl-template-wait-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-wait))
(defun vhdl-template-when-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-when))
(defun vhdl-template-while-loop-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-while-loop))
(defun vhdl-template-with-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-with))
(defun vhdl-template-and-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-and))
(defun vhdl-template-or-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-or))
(defun vhdl-template-nand-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-nand))
(defun vhdl-template-nor-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-nor))
(defun vhdl-template-xor-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-xor))
(defun vhdl-template-xnor-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-xnor))
(defun vhdl-template-not-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-not))

(defun vhdl-template-default-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-default))
(defun vhdl-template-default-indent-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-default-indent))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Template insertion from completion list

(defun vhdl-template-insert-construct (name)
  "Insert the built-in construct template with NAME."
  (interactive
   (list (let ((completion-ignore-case t))
	   (completing-read "Construct name: "
			    vhdl-template-construct-alist nil t))))
  (vhdl-template-insert-fun
   (cadr (assoc name vhdl-template-construct-alist))))

(defun vhdl-template-insert-package (name)
  "Insert the built-in package template with NAME."
  (interactive
   (list (let ((completion-ignore-case t))
	   (completing-read "Package name: "
			    vhdl-template-package-alist nil t))))
  (vhdl-template-insert-fun
   (cadr (assoc name vhdl-template-package-alist))))

(defun vhdl-template-insert-directive (name)
  "Insert the built-in directive template with NAME."
  (interactive
   (list (let ((completion-ignore-case t))
	   (completing-read "Directive name: "
			    vhdl-template-directive-alist nil t))))
  (vhdl-template-insert-fun
   (cadr (assoc name vhdl-template-directive-alist))))

(defun vhdl-template-insert-fun (fun)
  "Call FUN to insert a built-in template."
  (let ((caught (catch 'abort (when fun (funcall fun)))))
    (when (stringp caught) (message "%s" caught))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Models
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vhdl-model-insert (model-name)
  "Insert the user model with name MODEL-NAME."
  (interactive
   (let ((completion-ignore-case t))
     (list (completing-read "Model name: " vhdl-model-alist))))
  (indent-according-to-mode)
  (let ((start (point-marker))
	(margin (current-indentation))
	model position prompt string end)
    (vhdl-prepare-search-2
     (when (setq model (assoc model-name vhdl-model-alist))
       ;; insert model
       (beginning-of-line)
       (delete-horizontal-space)
       (goto-char start)
       (vhdl-insert-string-or-file (nth 1 model))
       (setq end (point-marker))
       ;; indent code
       (goto-char start)
       (beginning-of-line)
       (while (< (point) end)
	 (unless (looking-at "^$")
	   (insert-char ?  margin))
	 (beginning-of-line 2))
       (goto-char start)
       ;; insert clock
       (unless (equal "" vhdl-clock-name)
	 (while (re-search-forward "<clock>" end t)
	   (replace-match vhdl-clock-name)))
       (goto-char start)
       ;; insert reset
       (unless (equal "" vhdl-reset-name)
	 (while (re-search-forward "<reset>" end t)
	   (replace-match vhdl-reset-name)))
       ;; replace header prompts
       (vhdl-template-replace-header-keywords start end nil t)
       (goto-char start)
       ;; query other prompts
       (while (re-search-forward
	       (concat "<\\(" vhdl-template-prompt-syntax "\\)>") end t)
	 (unless (equal "cursor" (match-string 1))
	   (setq position (match-beginning 1))
	   (setq prompt (match-string 1))
	   (replace-match "")
	   (setq string (vhdl-template-field prompt nil t))
	   ;; replace occurrences of same prompt
	   (while (re-search-forward (concat "<\\(" prompt "\\)>") end t)
	     (replace-match (or string "")))
	   (goto-char position)))
       (goto-char start)
       ;; goto final position
       (if (re-search-forward "<cursor>" end t)
	   (replace-match "")
	 (goto-char end))))))

(defun vhdl-model-defun ()
  "Define help and hook functions for user models."
  (let ((model-alist vhdl-model-alist)
	model-name model-keyword)
    (while model-alist
      ;; define functions for user models that can be invoked from menu and key
      ;; bindings and which themselves call `vhdl-model-insert' with the model
      ;; name as argument
      (setq model-name (nth 0 (car model-alist)))
      (eval `(defun ,(vhdl-function-name "vhdl-model" model-name) ()
	       ,(concat "Insert model for \"" model-name "\".")
	       (interactive)
	       (vhdl-model-insert ,model-name)))
      ;; define hooks for user models that are invoked from keyword abbrevs
      (setq model-keyword (nth 3 (car model-alist)))
      (unless (equal model-keyword "")
	(eval `(defun
		 ,(vhdl-function-name
		   "vhdl-model" model-name "hook") ()
		 (vhdl-hooked-abbrev
		  ',(vhdl-function-name "vhdl-model" model-name)))))
      (setq model-alist (cdr model-alist)))))

(vhdl-model-defun)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Port translation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar vhdl-port-list nil
  "Variable to hold last port map parsed.")
;; structure: (parenthesized expression means list of such entries)
;; (ent-name
;;  ((generic-names) generic-type generic-init generic-comment group-comment)
;;  ((port-names) port-object port-direct port-type port-comment group-comment)
;;  (lib-name pack-key))

(defun vhdl-parse-string (string &optional optional)
  "Check that the text following point matches the regexp in STRING."
  (if (looking-at string)
      (goto-char (match-end 0))
    (unless optional
      (throw 'parse (format "ERROR:  Syntax error near line %s, expecting \"%s\""
			    (vhdl-current-line) string)))
    nil))

(defun vhdl-replace-string (regexp-cons string &optional adjust-case)
  "Replace STRING from car of REGEXP-CONS to cdr of REGEXP-CONS."
  (vhdl-prepare-search-1
   (if (string-match (car regexp-cons) string)
       (if adjust-case
	   (funcall vhdl-file-name-case
		    (replace-match (cdr regexp-cons) t nil string))
	 (replace-match (cdr regexp-cons) t nil string))
     string)))

(defun vhdl-parse-group-comment ()
  "Parse comment and empty lines between groups of lines."
  (let ((start (point))
	string)
    (vhdl-forward-comment (point-max))
    (setq string (buffer-substring-no-properties start (point)))
    (vhdl-forward-syntactic-ws)
    ;; strip off leading blanks and first newline
    (while (string-match "^\\(\\s-+\\)" string)
      (setq string (concat (substring string 0 (match-beginning 1))
			   (substring string (match-end 1)))))
    (if (and (not (equal string "")) (equal (substring string 0 1) "\n"))
	(substring string 1)
      string)))

(defun vhdl-paste-group-comment (string indent)
  "Paste comment and empty lines from STRING between groups of lines
with INDENT."
  (let ((pos (point-marker)))
    (when (> indent 0)
      (while (string-match "^\\(--\\)" string)
	(setq string (concat (substring string 0 (match-beginning 1))
			     (make-string indent ? )
			     (substring string (match-beginning 1))))))
    (beginning-of-line)
    (insert string)
    (goto-char pos)))

(defvar vhdl-port-flattened nil
  "Indicates whether a port has been flattened.")

(defun vhdl-port-flatten (&optional as-alist)
  "Flatten port list so that only one generic/port exists per line.
This operation is performed on an internally stored port and is only
reflected in a subsequent paste operation."
  (interactive)
  (if (not vhdl-port-list)
      (error "ERROR:  No port has been read")
    (message "Flattening port for next paste...")
    (let ((new-vhdl-port-list (list (car vhdl-port-list)))
	  (old-vhdl-port-list (cdr vhdl-port-list))
	  old-port-list new-port-list old-port new-port names)
      ;; traverse port list and flatten entries
      (while (cdr old-vhdl-port-list)
	(setq old-port-list (car old-vhdl-port-list))
	(setq new-port-list nil)
	(while old-port-list
	  (setq old-port (car old-port-list))
	  (setq names (car old-port))
	  (while names
	    (setq new-port (cons (if as-alist (car names) (list (car names)))
				 (cdr old-port)))
	    (setq new-port-list (append new-port-list (list new-port)))
	    (setq names (cdr names)))
	  (setq old-port-list (cdr old-port-list)))
	(setq old-vhdl-port-list (cdr old-vhdl-port-list))
	(setq new-vhdl-port-list (append new-vhdl-port-list
					 (list new-port-list))))
      (setq vhdl-port-list
	    (append new-vhdl-port-list (list old-vhdl-port-list))
	    vhdl-port-flattened t)
      (message "Flattening port for next paste...done"))))

(defvar vhdl-port-reversed-direction nil
  "Indicates whether port directions are reversed.")

(defun vhdl-port-reverse-direction ()
  "Reverse direction for all ports (useful in testbenches).
This operation is performed on an internally stored port and is only
reflected in a subsequent paste operation."
  (interactive)
  (if (not vhdl-port-list)
      (error "ERROR:  No port has been read")
    (message "Reversing port directions for next paste...")
    (let ((port-list (nth 2 vhdl-port-list))
	  port-dir-car port-dir)
      ;; traverse port list and reverse directions
      (while port-list
	(setq port-dir-car (cddr (car port-list))
	      port-dir (car port-dir-car))
	(setcar port-dir-car
		(cond ((equal port-dir "in") "out")
		      ((equal port-dir "out") "in")
		      (t port-dir)))
	(setq port-list (cdr port-list)))
      (setq vhdl-port-reversed-direction (not vhdl-port-reversed-direction))
      (message "Reversing port directions for next paste...done"))))

(defun vhdl-port-copy ()
  "Get generic and port information from an entity or component declaration."
  (interactive)
  (save-excursion
    (let (parse-error end-of-list
	  decl-type name generic-list port-list context-clause
	  object names direct type init comment group-comment)
      (vhdl-prepare-search-2
       (setq
	parse-error
	(catch 'parse
	  ;; check if within entity or component declaration
	  (end-of-line)
	  (when (or (not (re-search-backward
			  "^\\s-*\\(component\\|entity\\|end\\)\\>" nil t))
		    (equal "END" (upcase (match-string 1))))
	    (throw 'parse "ERROR:  Not within an entity or component declaration"))
	  (setq decl-type (downcase (match-string-no-properties 1)))
	  (forward-word 1)
	  (vhdl-parse-string "\\s-+\\(\\w+\\)\\(\\s-+is\\>\\)?")
	  (setq name (match-string-no-properties 1))
	  (message "Reading port of %s \"%s\"..." decl-type name)
	  (vhdl-forward-syntactic-ws)
	  ;; parse generic clause
	  (when (vhdl-parse-string "generic[ \t\n]*(" t)
	    ;; parse group comment and spacing
	    (setq group-comment (vhdl-parse-group-comment))
	    (setq end-of-list (vhdl-parse-string ")[ \t\n]*;[ \t\n]*" t))
	    (while (not end-of-list)
	      ;; parse names (accept extended identifiers)
	      (vhdl-parse-string "\\(\\w+\\|\\\\[^\\]+\\\\\\)[ \t\n]*")
	      (setq names (list (match-string-no-properties 1)))
	      (while (vhdl-parse-string ",[ \t\n]*\\(\\w+\\)[ \t\n]*" t)
		(setq names
		      (append names (list (match-string-no-properties 1)))))
	      ;; parse type
	      (vhdl-parse-string ":[ \t\n]*\\([^():;\n]+\\)")
	      (setq type (match-string-no-properties 1))
	      (setq comment nil)
	      (while (looking-at "(")
		(setq type
		      (concat type
			      (buffer-substring-no-properties
			       (point) (progn (forward-sexp) (point)))
			      (and (vhdl-parse-string "\\([^():;\n]*\\)" t)
				   (match-string-no-properties 1)))))
	      ;; special case: closing parenthesis is on separate line
	      (when (and type (string-match "\\(\\s-*--\\s-*\\)\\(.*\\)" type))
		(setq comment (substring type (match-beginning 2)))
		(setq type (substring type 0 (match-beginning 1))))
	      ;; strip of trailing group-comment
	      (string-match "\\(\\(\\s-*\\S-+\\)+\\)\\s-*" type)
	      (setq type (substring type 0 (match-end 1)))
	      ;; parse initialization expression
	      (setq init nil)
	      (when (vhdl-parse-string ":=[ \t\n]*" t)
		(vhdl-parse-string "\\([^();\n]*\\)")
		(setq init (match-string-no-properties 1))
		(while (looking-at "(")
		  (setq init
			(concat init
				(buffer-substring-no-properties
				 (point) (progn (forward-sexp) (point)))
				(and (vhdl-parse-string "\\([^();\n]*\\)" t)
				     (match-string-no-properties 1))))))
	      ;; special case: closing parenthesis is on separate line
	      (when (and init (string-match "\\(\\s-*--\\s-*\\)\\(.*\\)" init))
		(setq comment (substring init (match-beginning 2)))
		(setq init (substring init 0 (match-beginning 1)))
		(vhdl-forward-syntactic-ws))
	      (skip-chars-forward " \t")
	      ;; parse inline comment, special case: as above, no initial.
	      (unless comment
		(setq comment (and (vhdl-parse-string "--\\s-*\\([^\n]*\\)" t)
				   (match-string-no-properties 1))))
	      (vhdl-forward-syntactic-ws)
	      (setq end-of-list (vhdl-parse-string ")" t))
	      (vhdl-parse-string "\\s-*;\\s-*")
	      ;; parse inline comment
	      (unless comment
		(setq comment (and (vhdl-parse-string "--\\s-*\\([^\n]*\\)" t)
				   (match-string-no-properties 1))))
	      ;; save everything in list
	      (setq generic-list (append generic-list
					 (list (list names type init
						     comment group-comment))))
	      ;; parse group comment and spacing
	      (setq group-comment (vhdl-parse-group-comment))))
	  ;; parse port clause
	  (when (vhdl-parse-string "port[ \t\n]*(" t)
	    ;; parse group comment and spacing
	    (setq group-comment (vhdl-parse-group-comment))
	    (setq end-of-list (vhdl-parse-string ")[ \t\n]*;[ \t\n]*" t))
	    (while (not end-of-list)
	      ;; parse object
	      (setq object
		    (and (vhdl-parse-string "\\<\\(signal\\|quantity\\|terminal\\)\\>[ \t\n]*" t)
			 (match-string-no-properties 1)))
	      ;; parse names (accept extended identifiers)
	      (vhdl-parse-string "\\(\\w+\\|\\\\[^\\]+\\\\\\)[ \t\n]*")
	      (setq names (list (match-string-no-properties 1)))
	      (while (vhdl-parse-string ",[ \t\n]*\\(\\w+\\|\\\\[^\\]+\\\\\\)[ \t\n]*" t)
		(setq names (append names (list (match-string-no-properties 1)))))
	      ;; parse direction
	      (vhdl-parse-string ":[ \t\n]*")
	      (setq direct
		    (and (vhdl-parse-string "\\<\\(in\\|out\\|inout\\|buffer\\|linkage\\)\\>[ \t\n]+" t)
			 (match-string-no-properties 1)))
	      ;; parse type
	      (vhdl-parse-string "\\([^();\n]+\\)")
	      (setq type (match-string-no-properties 1))
	      (setq comment nil)
	      (while (looking-at "(")
		(setq type (concat type
				   (buffer-substring-no-properties
				    (point) (progn (forward-sexp) (point)))
				   (and (vhdl-parse-string "\\([^();\n]*\\)" t)
					(match-string-no-properties 1)))))
	      ;; special case: closing parenthesis is on separate line
	      (when (and type (string-match "\\(\\s-*--\\s-*\\)\\(.*\\)" type))
		(setq comment (substring type (match-beginning 2)))
		(setq type (substring type 0 (match-beginning 1))))
	      ;; strip of trailing group-comment
	      (string-match "\\(\\(\\s-*\\S-+\\)+\\)\\s-*" type)
	      (setq type (substring type 0 (match-end 1)))
	      (vhdl-forward-syntactic-ws)
	      (setq end-of-list (vhdl-parse-string ")" t))
	      (vhdl-parse-string "\\s-*;\\s-*")
	      ;; parse inline comment
	      (unless comment
		(setq comment (and (vhdl-parse-string "--\\s-*\\([^\n]*\\)" t)
				   (match-string-no-properties 1))))
	      ;; save everything in list
	      (setq port-list (append port-list
				      (list (list names object direct type
						  comment group-comment))))
	      ;; parse group comment and spacing
	      (setq group-comment (vhdl-parse-group-comment))))
;	  (vhdl-parse-string "end\\>")
	  ;; parse context clause
	  (setq context-clause (vhdl-scan-context-clause))
;	  ;; add surrounding package to context clause
;	  (when (and (equal decl-type "component")
;		     (re-search-backward "^\\s-*package\\s-+\\(\\w+\\)" nil t))
;	    (setq context-clause
;		  (append context-clause
;			  (list (cons (vhdl-work-library)
;				      (match-string-no-properties 1))))))
	  (message "Reading port of %s \"%s\"...done" decl-type name)
	  nil)))
      ;; finish parsing
      (if parse-error
	  (error parse-error)
	(setq vhdl-port-list (list name generic-list port-list context-clause)
	      vhdl-port-reversed-direction nil
	      vhdl-port-flattened nil)))))

(defun vhdl-port-paste-context-clause (&optional exclude-pack-name)
  "Paste a context clause."
  (let ((margin (current-indentation))
	(clause-list (nth 3 vhdl-port-list))
	clause)
    (while clause-list
      (setq clause (car clause-list))
      (unless (or (and exclude-pack-name (equal (downcase (cdr clause))
						(downcase exclude-pack-name)))
		  (save-excursion
		    (re-search-backward
		     (concat "^\\s-*use\\s-+" (car clause)
			     "\." (cdr clause) "\\>") nil t)))
	(vhdl-template-standard-package (car clause) (cdr clause))
	(insert "\n"))
      (setq clause-list (cdr clause-list)))))

(defun vhdl-port-paste-generic (&optional no-init)
  "Paste a generic clause."
  (let ((margin (current-indentation))
	(generic-list (nth 1 vhdl-port-list))
	list-margin start names generic)
    ;; paste generic clause
    (when generic-list
      (setq start (point))
      (vhdl-insert-keyword "GENERIC (")
      (unless vhdl-argument-list-indent
	(insert "\n") (indent-to (+ margin vhdl-basic-offset)))
      (setq list-margin (current-column))
      (while generic-list
	(setq generic (car generic-list))
	;; paste group comment and spacing
	(when (memq vhdl-include-group-comments '(decl always))
	  (vhdl-paste-group-comment (nth 4 generic) list-margin))
	;; paste names
	(setq names (nth 0 generic))
	(while names
	  (insert (car names))
	  (setq names (cdr names))
	  (when names (insert ", ")))
	;; paste type
	(insert " : " (nth 1 generic))
	;; paste initialization
	(when (and (not no-init) (nth 2 generic))
	  (insert " := " (nth 2 generic)))
	(unless (cdr generic-list) (insert ")"))
	(insert ";")
	;; paste comment
	(when (and vhdl-include-port-comments (nth 3 generic))
	  (vhdl-comment-insert-inline (nth 3 generic) t))
	(setq generic-list (cdr generic-list))
	(when generic-list (insert "\n") (indent-to list-margin)))
      ;; align generic clause
      (when vhdl-auto-align (vhdl-align-region-groups start (point) 1 t)))))

(defun vhdl-port-paste-port ()
  "Paste a port clause."
  (let ((margin (current-indentation))
	(port-list (nth 2 vhdl-port-list))
	list-margin start names port)
    ;; paste port clause
    (when port-list
      (setq start (point))
      (vhdl-insert-keyword "PORT (")
      (unless vhdl-argument-list-indent
	(insert "\n") (indent-to (+ margin vhdl-basic-offset)))
      (setq list-margin (current-column))
      (while port-list
	(setq port (car port-list))
	;; paste group comment and spacing
	(when (memq vhdl-include-group-comments '(decl always))
	  (vhdl-paste-group-comment (nth 5 port) list-margin))
	;; paste object
	(when (nth 1 port) (insert (nth 1 port) " "))
	;; paste names
	(setq names (nth 0 port))
	(while names
	  (insert (car names))
	  (setq names (cdr names))
	  (when names (insert ", ")))
	;; paste direction
	(insert " : ")
	(when (nth 2 port) (insert (nth 2 port) " "))
	;; paste type
	(insert (nth 3 port))
	(unless (cdr port-list) (insert ")"))
	(insert ";")
	;; paste comment
	(when (and vhdl-include-port-comments (nth 4 port))
	  (vhdl-comment-insert-inline (nth 4 port) t))
	(setq port-list (cdr port-list))
	(when port-list (insert "\n") (indent-to list-margin)))
      ;; align port clause
      (when vhdl-auto-align (vhdl-align-region-groups start (point) 1)))))

(defun vhdl-port-paste-declaration (kind &optional no-indent)
  "Paste as an entity or component declaration."
  (unless no-indent (indent-according-to-mode))
  (let ((margin (current-indentation))
	(name (nth 0 vhdl-port-list)))
    (vhdl-insert-keyword (if (eq kind 'entity) "ENTITY " "COMPONENT "))
    (insert name)
    (when (or (eq kind 'entity) (not (vhdl-standard-p '87)))
	(vhdl-insert-keyword " IS"))
    ;; paste generic and port clause
    (when (nth 1 vhdl-port-list)
      (insert "\n")
      (when (and (memq vhdl-insert-empty-lines '(unit all)) (eq kind 'entity))
	(insert "\n"))
      (indent-to (+ margin vhdl-basic-offset))
      (vhdl-port-paste-generic (eq kind 'component)))
    (when (nth 2 vhdl-port-list)
      (insert "\n")
      (when (and (memq vhdl-insert-empty-lines '(unit all))
		 (eq kind 'entity))
	(insert "\n"))
      (indent-to (+ margin vhdl-basic-offset)))
    (vhdl-port-paste-port)
    (insert "\n")
    (when (and (memq vhdl-insert-empty-lines '(unit all)) (eq kind 'entity))
      (insert "\n"))
    (indent-to margin)
    (vhdl-insert-keyword "END")
    (if (eq kind 'entity)
	(progn
	  (unless (vhdl-standard-p '87) (vhdl-insert-keyword " ENTITY"))
	  (insert " " name))
      (vhdl-insert-keyword " COMPONENT")
      (unless (vhdl-standard-p '87) (insert " " name)))
    (insert ";")))

(defun vhdl-port-paste-entity (&optional no-indent)
  "Paste as an entity declaration."
  (interactive)
  (if (not vhdl-port-list)
      (error "ERROR:  No port read")
    (message "Pasting port as entity \"%s\"..." (car vhdl-port-list))
    (vhdl-port-paste-declaration 'entity no-indent)
    (message "Pasting port as entity \"%s\"...done" (car vhdl-port-list))))

(defun vhdl-port-paste-component (&optional no-indent)
  "Paste as a component declaration."
  (interactive)
  (if (not vhdl-port-list)
      (error "ERROR:  No port read")
    (message "Pasting port as component \"%s\"..." (car vhdl-port-list))
    (vhdl-port-paste-declaration 'component no-indent)
    (message "Pasting port as component \"%s\"...done" (car vhdl-port-list))))

(defun vhdl-port-paste-generic-map (&optional secondary no-constants)
  "Paste as a generic map."
  (interactive)
  (unless secondary (indent-according-to-mode))
  (let ((margin (current-indentation))
	list-margin start generic
	(generic-list (nth 1 vhdl-port-list)))
    (when generic-list
      (setq start (point))
      (vhdl-insert-keyword "GENERIC MAP (")
      (if (not vhdl-association-list-with-formals)
	  ;; paste list of actual generics
	  (while generic-list
	    (insert (if no-constants
		      (car (nth 0 (car generic-list)))
		    (or (nth 2 (car generic-list)) " ")))
	    (setq generic-list (cdr generic-list))
	    (insert (if generic-list ", " ")"))
	    (when (and (not generic-list) secondary
		       (null (nth 2 vhdl-port-list)))
	      (insert ";")))
	(unless vhdl-argument-list-indent
	  (insert "\n") (indent-to (+ margin vhdl-basic-offset)))
	(setq list-margin (current-column))
	(while generic-list
	  (setq generic (car generic-list))
	  ;; paste group comment and spacing
	  (when (eq vhdl-include-group-comments 'always)
	    (vhdl-paste-group-comment (nth 4 generic) list-margin))
	  ;; paste formal and actual generic
	  (insert (car (nth 0 generic)) " => "
		  (if no-constants
		      (car (nth 0 generic))
		    (or (nth 2 generic) "")))
	  (setq generic-list (cdr generic-list))
	  (insert (if generic-list "," ")"))
	  (when (and (not generic-list) secondary
		     (null (nth 2 vhdl-port-list)))
	    (insert ";"))
	  ;; paste comment
	  (when (or vhdl-include-type-comments
		    (and vhdl-include-port-comments (nth 3 generic)))
	    (vhdl-comment-insert-inline
	     (concat
	      (when vhdl-include-type-comments
		(concat "[" (nth 1 generic) "] "))
	      (when vhdl-include-port-comments (nth 3 generic))) t))
	  (when generic-list (insert "\n") (indent-to list-margin)))
	;; align generic map
	(when vhdl-auto-align
	  (vhdl-align-region-groups start (point) 1 t))))))

(defun vhdl-port-paste-port-map ()
  "Paste as a port map."
  (let ((margin (current-indentation))
	list-margin start port
	(port-list (nth 2 vhdl-port-list)))
    (when port-list
      (setq start (point))
      (vhdl-insert-keyword "PORT MAP (")
      (if (not vhdl-association-list-with-formals)
	  ;; paste list of actual ports
	  (while port-list
	    (insert (vhdl-replace-string vhdl-actual-port-name
					 (car (nth 0 (car port-list)))))
	    (setq port-list (cdr port-list))
	    (insert (if port-list ", " ")")))
	(unless vhdl-argument-list-indent
	  (insert "\n") (indent-to (+ margin vhdl-basic-offset)))
	(setq list-margin (current-column))
	(while port-list
	  (setq port (car port-list))
	  ;; paste group comment and spacing
	  (when (eq vhdl-include-group-comments 'always)
	    (vhdl-paste-group-comment (nth 5 port) list-margin))
	  ;; paste formal and actual port
	  (insert (car (nth 0 port)) " => ")
	  (insert (vhdl-replace-string vhdl-actual-port-name
				       (car (nth 0 port))))
	  (setq port-list (cdr port-list))
	  (insert (if port-list "," ");"))
	  ;; paste comment
	  (when (or vhdl-include-direction-comments
		    vhdl-include-type-comments
		    (and vhdl-include-port-comments (nth 4 port)))
	    (vhdl-comment-insert-inline
	     (concat
	      (cond ((and vhdl-include-direction-comments
			  vhdl-include-type-comments)
		     (concat "[" (format "%-4s" (concat (nth 2 port) " "))
			     (nth 3 port) "] "))
		    ((and vhdl-include-direction-comments (nth 2 port))
		     (format "%-6s" (concat "[" (nth 2 port) "] ")))
		    (vhdl-include-direction-comments "      ")
		    (vhdl-include-type-comments
		     (concat "[" (nth 3 port) "] ")))
	      (when vhdl-include-port-comments (nth 4 port))) t))
	  (when port-list (insert "\n") (indent-to list-margin)))
	;; align port clause
	(when vhdl-auto-align
	  (vhdl-align-region-groups start (point) 1))))))

(defun vhdl-port-paste-instance (&optional name no-indent title)
  "Paste as an instantiation."
  (interactive)
  (if (not vhdl-port-list)
      (error "ERROR:  No port read")
    (let ((orig-vhdl-port-list vhdl-port-list))
      ;; flatten local copy of port list (must be flat for port mapping)
      (vhdl-port-flatten)
      (unless no-indent (indent-according-to-mode))
      (let ((margin (current-indentation)))
	;; paste instantiation
	(cond (name
	       (insert name))
	      ((equal (cdr vhdl-instance-name) "")
	       (setq name (vhdl-template-field "instance name")))
	      ((string-match "\%d" (cdr vhdl-instance-name))
	       (let ((n 1))
		 (while (save-excursion
			  (setq name (format (vhdl-replace-string
					      vhdl-instance-name
					      (nth 0 vhdl-port-list)) n))
			  (goto-char (point-min))
			  (vhdl-re-search-forward name nil t))
		   (setq n (1+ n)))
		 (insert name)))
	      (t (insert (vhdl-replace-string vhdl-instance-name
					      (nth 0 vhdl-port-list)))))
	(message "Pasting port as instantiation \"%s\"..." name)
	(insert ": ")
	(when title
	  (save-excursion
	    (beginning-of-line)
	    (indent-to vhdl-basic-offset)
	    (insert "-- instance \"" name "\"\n")))
	(if (not (vhdl-use-direct-instantiation))
	    (insert (nth 0 vhdl-port-list))
	  (vhdl-insert-keyword "ENTITY ")
	  (insert (vhdl-work-library) "." (nth 0 vhdl-port-list)))
	(when (nth 1 vhdl-port-list)
	  (insert "\n") (indent-to (+ margin vhdl-basic-offset))
	  (vhdl-port-paste-generic-map t t))
	(when (nth 2 vhdl-port-list)
	  (insert "\n") (indent-to (+ margin vhdl-basic-offset))
	  (vhdl-port-paste-port-map))
	(unless (or (nth 1 vhdl-port-list) (nth 2 vhdl-port-list))
	  (insert ";"))
	(message "Pasting port as instantiation \"%s\"...done" name))
      (setq vhdl-port-list orig-vhdl-port-list))))

(defun vhdl-port-paste-constants (&optional no-indent)
  "Paste generics as constants."
  (interactive)
  (if (not vhdl-port-list)
      (error "ERROR:  No port read")
    (let ((orig-vhdl-port-list vhdl-port-list))
      (message "Pasting port as constants...")
      ;; flatten local copy of port list (must be flat for constant initial.)
      (vhdl-port-flatten)
      (unless no-indent (indent-according-to-mode))
      (let ((margin (current-indentation))
	    start generic name
	    (generic-list (nth 1 vhdl-port-list)))
	(when generic-list
	  (setq start (point))
	  (while generic-list
	    (setq generic (car generic-list))
	    ;; paste group comment and spacing
	    (when (memq vhdl-include-group-comments '(decl always))
	      (vhdl-paste-group-comment (nth 4 generic) margin))
	    (vhdl-insert-keyword "CONSTANT ")
	    ;; paste generic constants
	    (setq name (nth 0 generic))
	    (when name
	      (insert (car name))
	      ;; paste type
	      (insert " : " (nth 1 generic))
	      ;; paste initialization
	      (when (nth 2 generic)
		(insert " := " (nth 2 generic)))
	      (insert ";")
	      ;; paste comment
	      (when (and vhdl-include-port-comments (nth 3 generic))
		(vhdl-comment-insert-inline (nth 3 generic) t))
	      (setq generic-list (cdr generic-list))
	      (when generic-list (insert "\n") (indent-to margin))))
	    ;; align signal list
	  (when vhdl-auto-align
	    (vhdl-align-region-groups start (point) 1))))
      (message "Pasting port as constants...done")
      (setq vhdl-port-list orig-vhdl-port-list))))

(defun vhdl-port-paste-signals (&optional initialize no-indent)
  "Paste ports as internal signals."
  (interactive)
  (if (not vhdl-port-list)
      (error "ERROR:  No port read")
    (message "Pasting port as signals...")
    (unless no-indent (indent-according-to-mode))
    (let ((margin (current-indentation))
	  start port names
	  (port-list (nth 2 vhdl-port-list)))
      (when port-list
	(setq start (point))
	(while port-list
	  (setq port (car port-list))
	  ;; paste group comment and spacing
	  (when (memq vhdl-include-group-comments '(decl always))
	    (vhdl-paste-group-comment (nth 5 port) margin))
	  ;; paste object
	  (if (nth 1 port)
	      (insert (nth 1 port) " ")
	    (vhdl-insert-keyword "SIGNAL "))
	  ;; paste actual port signals
	  (setq names (nth 0 port))
	  (while names
	    (insert (vhdl-replace-string vhdl-actual-port-name (car names)))
	    (setq names (cdr names))
	    (when names (insert ", ")))
	  ;; paste type
	  (insert " : " (nth 3 port))
	  ;; paste initialization (inputs only)
	  (when (and initialize (equal "IN" (upcase (nth 2 port))))
	    (insert " := " (if (string-match "(.+)" (nth 3 port))
			       "(others => '0')" "'0'")))
	  (insert ";")
	  ;; paste comment
	  (when (or vhdl-include-direction-comments
		    (and vhdl-include-port-comments (nth 4 port)))
	    (vhdl-comment-insert-inline
	     (concat
	      (cond ((and vhdl-include-direction-comments (nth 2 port))
		     (format "%-6s" (concat "[" (nth 2 port) "] ")))
		    (vhdl-include-direction-comments "      "))
	      (when vhdl-include-port-comments (nth 4 port))) t))
	  (setq port-list (cdr port-list))
	  (when port-list (insert "\n") (indent-to margin)))
	;; align signal list
	(when vhdl-auto-align (vhdl-align-region-groups start (point) 1))))
    (message "Pasting port as signals...done")))

(defun vhdl-port-paste-initializations (&optional no-indent)
  "Paste ports as signal initializations."
  (interactive)
  (if (not vhdl-port-list)
      (error "ERROR:  No port read")
    (let ((orig-vhdl-port-list vhdl-port-list))
      (message "Pasting port as initializations...")
      ;; flatten local copy of port list (must be flat for signal initial.)
      (vhdl-port-flatten)
      (unless no-indent (indent-according-to-mode))
      (let ((margin (current-indentation))
	    start port name
	    (port-list (nth 2 vhdl-port-list)))
	(when port-list
	  (setq start (point))
	  (while port-list
	    (setq port (car port-list))
	    ;; paste actual port signal (inputs only)
	    (when (equal "IN" (upcase (nth 2 port)))
	      (setq name (car (nth 0 port)))
	      (insert (vhdl-replace-string vhdl-actual-port-name name))
	      ;; paste initialization
	      (insert " <= " (if (string-match "(.+)" (nth 3 port))
				 "(others => '0')" "'0'") ";"))
	    (setq port-list (cdr port-list))
	    (when (and port-list
		       (equal "IN" (upcase (nth 2 (car port-list)))))
	      (insert "\n") (indent-to margin)))
	  ;; align signal list
	  (when vhdl-auto-align (vhdl-align-region-groups start (point) 1))))
      (message "Pasting port as initializations...done")
      (setq vhdl-port-list orig-vhdl-port-list))))

(defun vhdl-port-paste-testbench ()
  "Paste as a bare-bones testbench."
  (interactive)
  (if (not vhdl-port-list)
      (error "ERROR:  No port read")
    (let ((case-fold-search t)
	  (ent-name (vhdl-replace-string vhdl-testbench-entity-name
					 (nth 0 vhdl-port-list)))
	  (source-buffer (current-buffer))
	  arch-name config-name ent-file-name arch-file-name
	  ent-buffer arch-buffer position)
      ;; open entity file
      (unless (eq vhdl-testbench-create-files 'none)
	(setq ent-file-name
	      (concat (vhdl-replace-string vhdl-testbench-entity-file-name
					   ent-name t)
		      "." (file-name-extension (buffer-file-name))))
	(if (file-exists-p ent-file-name)
	    (if (y-or-n-p
		 (concat "File \"" ent-file-name "\" exists; overwrite? "))
		(progn (find-file ent-file-name)
		       (erase-buffer)
		       (set-buffer-modified-p nil))
	      (if (eq vhdl-testbench-create-files 'separate)
		  (setq ent-file-name nil)
		(error "ERROR:  Pasting port as testbench...aborted")))
	  (find-file ent-file-name)))
      (unless (and (eq vhdl-testbench-create-files 'separate)
		   (null ent-file-name))
	;; paste entity header
	(if vhdl-testbench-include-header
	    (progn (vhdl-template-header
		    (concat "Testbench for design \""
			    (nth 0 vhdl-port-list) "\""))
		   (goto-char (point-max)))
	  (vhdl-comment-display-line) (insert "\n\n"))
	;; paste std_logic_1164 package
	(when vhdl-testbench-include-library
	  (vhdl-template-package-std-logic-1164)
	  (insert "\n\n") (vhdl-comment-display-line) (insert "\n\n"))
	;; paste entity declaration
	(vhdl-insert-keyword "ENTITY ")
	(insert ent-name)
	(vhdl-insert-keyword " IS")
	(when (memq vhdl-insert-empty-lines '(unit all)) (insert "\n"))
	(insert "\n")
	(vhdl-insert-keyword "END ")
	(unless (vhdl-standard-p '87) (vhdl-insert-keyword "ENTITY "))
	(insert ent-name ";")
	(insert "\n\n")
	(vhdl-comment-display-line) (insert "\n"))
      ;; get architecture name
      (setq arch-name (if (equal (cdr vhdl-testbench-architecture-name) "")
			  (read-from-minibuffer "architecture name: "
						nil vhdl-minibuffer-local-map)
			(vhdl-replace-string vhdl-testbench-architecture-name
					     (nth 0 vhdl-port-list))))
      (message "Pasting port as testbench \"%s(%s)\"..." ent-name arch-name)
      ;; open architecture file
      (if (not (eq vhdl-testbench-create-files 'separate))
	  (insert "\n")
	(setq ent-buffer (current-buffer))
	(setq arch-file-name
	      (concat (vhdl-replace-string vhdl-testbench-architecture-file-name
					   (concat ent-name " " arch-name) t)
		      "." (file-name-extension (buffer-file-name))))
	(when (and (file-exists-p arch-file-name)
		   (not (y-or-n-p (concat "File \"" arch-file-name
					  "\" exists; overwrite? "))))
	  (error "ERROR:  Pasting port as testbench...aborted"))
	(find-file arch-file-name)
	(erase-buffer)
	(set-buffer-modified-p nil)
	;; paste architecture header
	(if vhdl-testbench-include-header
	    (progn (vhdl-template-header
		    (concat "Testbench architecture for design \""
			    (nth 0 vhdl-port-list) "\""))
		   (goto-char (point-max)))
	  (vhdl-comment-display-line) (insert "\n\n")))
      ;; paste architecture body
      (vhdl-insert-keyword "ARCHITECTURE ")
      (insert arch-name)
      (vhdl-insert-keyword " OF ")
      (insert ent-name)
      (vhdl-insert-keyword " IS")
      (insert "\n\n") (indent-to vhdl-basic-offset)
      ;; paste component declaration
      (unless (vhdl-use-direct-instantiation)
	(vhdl-port-paste-component t)
	(insert "\n\n") (indent-to vhdl-basic-offset))
      ;; paste constants
      (when (nth 1 vhdl-port-list)
	(insert "-- component generics\n") (indent-to vhdl-basic-offset)
	(vhdl-port-paste-constants t)
	(insert "\n\n") (indent-to vhdl-basic-offset))
      ;; paste internal signals
      (insert "-- component ports\n") (indent-to vhdl-basic-offset)
      (vhdl-port-paste-signals vhdl-testbench-initialize-signals t)
      (insert "\n")
      ;; paste custom declarations
      (unless (equal "" vhdl-testbench-declarations)
	(insert "\n")
	(vhdl-insert-string-or-file vhdl-testbench-declarations))
      (setq position (point))
      (insert "\n\n")
      (vhdl-comment-display-line) (insert "\n")
      (when vhdl-testbench-include-configuration
	(setq config-name (vhdl-replace-string
			   vhdl-testbench-configuration-name
			   (concat ent-name " " arch-name)))
	(insert "\n")
	(vhdl-insert-keyword "CONFIGURATION ") (insert config-name)
	(vhdl-insert-keyword " OF ") (insert ent-name)
	(vhdl-insert-keyword " IS\n")
	(indent-to vhdl-basic-offset)
	(vhdl-insert-keyword "FOR ") (insert arch-name "\n")
	(indent-to vhdl-basic-offset)
	(vhdl-insert-keyword "END FOR;\n")
	(vhdl-insert-keyword "END ") (insert config-name ";\n\n")
	(vhdl-comment-display-line) (insert "\n"))
      (goto-char position)
      (vhdl-template-begin-end
       (unless (vhdl-standard-p '87) "ARCHITECTURE") arch-name 0 t)
      ;; paste instantiation
      (insert "-- component instantiation\n") (indent-to vhdl-basic-offset)
      (vhdl-port-paste-instance
       (vhdl-replace-string vhdl-testbench-dut-name (nth 0 vhdl-port-list)) t)
      (insert "\n")
      ;; paste custom statements
      (unless (equal "" vhdl-testbench-statements)
	(insert "\n")
	(vhdl-insert-string-or-file vhdl-testbench-statements))
      (insert "\n")
      (indent-to vhdl-basic-offset)
      (unless (eq vhdl-testbench-create-files 'none)
	(setq arch-buffer (current-buffer))
	(when ent-buffer (set-buffer ent-buffer) (save-buffer))
	(set-buffer arch-buffer) (save-buffer))
      (message "%s"
       (concat (format "Pasting port as testbench \"%s(%s)\"...done"
		       ent-name arch-name)
	       (and ent-file-name
		    (format "\n  File created: \"%s\"" ent-file-name))
	       (and arch-file-name
		    (format "\n  File created: \"%s\"" arch-file-name)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subprogram interface translation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar vhdl-subprog-list nil
  "Variable to hold last subprogram interface parsed.")
;; structure: (parenthesized expression means list of such entries)
;; (subprog-name kind
;;  ((names) object direct type init comment group-comment)
;;  return-type return-comment group-comment)

(defvar vhdl-subprog-flattened nil
  "Indicates whether an subprogram interface has been flattened.")

(defun vhdl-subprog-flatten ()
  "Flatten interface list so that only one parameter exists per line."
  (interactive)
  (if (not vhdl-subprog-list)
      (error "ERROR:  No subprogram interface has been read")
    (message "Flattening subprogram interface...")
    (let ((old-subprog-list (nth 2 vhdl-subprog-list))
	  new-subprog-list old-subprog new-subprog names)
      ;; traverse parameter list and flatten entries
      (while old-subprog-list
	(setq old-subprog (car old-subprog-list))
	(setq names (car old-subprog))
	(while names
	  (setq new-subprog (cons (list (car names)) (cdr old-subprog)))
	  (setq new-subprog-list (append new-subprog-list (list new-subprog)))
	  (setq names (cdr names)))
	(setq old-subprog-list (cdr old-subprog-list)))
      (setq vhdl-subprog-list
	    (list (nth 0 vhdl-subprog-list) (nth 1 vhdl-subprog-list)
		  new-subprog-list (nth 3 vhdl-subprog-list)
		  (nth 4 vhdl-subprog-list) (nth 5 vhdl-subprog-list))
	    vhdl-subprog-flattened t)
    (message "Flattening subprogram interface...done"))))

(defun vhdl-subprog-copy ()
  "Get interface information from a subprogram specification."
  (interactive)
  (save-excursion
    (let (parse-error pos end-of-list
	  name kind param-list object names direct type init
	  comment group-comment
	  return-type return-comment return-group-comment)
      (vhdl-prepare-search-2
       (setq
	parse-error
	(catch 'parse
	  ;; check if within function declaration
	  (setq pos (point))
	  (end-of-line)
	  (when (looking-at "[ \t\n]*\\((\\|;\\|is\\>\\)") (goto-char (match-end 0)))
	  (unless (and (re-search-backward "^\\s-*\\(\\(procedure\\)\\|\\(\\(pure\\|impure\\)\\s-+\\)?function\\)\\s-+\\(\"?\\w+\"?\\)[ \t\n]*\\(\\((\\)\\|;\\|is\\>\\)" nil t)
		       (goto-char (match-end 0))
		       (save-excursion (backward-char)
				    (forward-sexp)
				    (<= pos (point))))
	    (throw 'parse "ERROR:  Not within a subprogram specification"))
	  (setq name (match-string-no-properties 5))
	  (setq kind (if (match-string 2) 'procedure 'function))
	  (setq end-of-list (not (match-string 7)))
	  (message "Reading interface of subprogram \"%s\"..." name)
	  ;; parse parameter list
	  (setq group-comment (vhdl-parse-group-comment))
	  (setq end-of-list (or end-of-list
				(vhdl-parse-string ")[ \t\n]*\\(;\\|\\(is\\|return\\)\\>\\)" t)))
	  (while (not end-of-list)
	    ;; parse object
	    (setq object
		  (and (vhdl-parse-string "\\(constant\\|signal\\|variable\\|file\\|quantity\\|terminal\\)[ \t\n]*" t)
			 (match-string-no-properties 1)))
	    ;; parse names (accept extended identifiers)
	    (vhdl-parse-string "\\(\\w+\\|\\\\[^\\]+\\\\\\)[ \t\n]*")
	    (setq names (list (match-string-no-properties 1)))
	    (while (vhdl-parse-string ",[ \t\n]*\\(\\w+\\|\\\\[^\\]+\\\\\\)[ \t\n]*" t)
	      (setq names (append names (list (match-string-no-properties 1)))))
	    ;; parse direction
	    (vhdl-parse-string ":[ \t\n]*")
	    (setq direct
		  (and (vhdl-parse-string "\\(in\\|out\\|inout\\|buffer\\|linkage\\)[ \t\n]+" t)
		       (match-string-no-properties 1)))
	    ;; parse type
	    (vhdl-parse-string "\\([^():;\n]+\\)")
	    (setq type (match-string-no-properties 1))
	    (setq comment nil)
	    (while (looking-at "(")
	      (setq type
		    (concat type
			    (buffer-substring-no-properties
			     (point) (progn (forward-sexp) (point)))
			    (and (vhdl-parse-string "\\([^():;\n]*\\)" t)
				 (match-string-no-properties 1)))))
	    ;; special case: closing parenthesis is on separate line
	    (when (and type (string-match "\\(\\s-*--\\s-*\\)\\(.*\\)" type))
	      (setq comment (substring type (match-beginning 2)))
	      (setq type (substring type 0 (match-beginning 1))))
	    ;; strip off trailing group-comment
	    (string-match "\\(\\(\\s-*\\S-+\\)+\\)\\s-*" type)
	    (setq type (substring type 0 (match-end 1)))
	    ;; parse initialization expression
	    (setq init nil)
	    (when (vhdl-parse-string ":=[ \t\n]*" t)
	      (vhdl-parse-string "\\([^();\n]*\\)")
	      (setq init (match-string-no-properties 1))
	      (while (looking-at "(")
		(setq init
		      (concat init
			      (buffer-substring-no-properties
			       (point) (progn (forward-sexp) (point)))
			      (and (vhdl-parse-string "\\([^();\n]*\\)" t)
				   (match-string-no-properties 1))))))
	    ;; special case: closing parenthesis is on separate line
	    (when (and init (string-match "\\(\\s-*--\\s-*\\)\\(.*\\)" init))
	      (setq comment (substring init (match-beginning 2)))
	      (setq init (substring init 0 (match-beginning 1)))
	      (vhdl-forward-syntactic-ws))
	    (skip-chars-forward " \t")
	    ;; parse inline comment, special case: as above, no initial.
	    (unless comment
	      (setq comment (and (vhdl-parse-string "--\\s-*\\([^\n]*\\)" t)
				 (match-string-no-properties 1))))
	    (vhdl-forward-syntactic-ws)
	    (setq end-of-list (vhdl-parse-string ")\\s-*" t))
	    ;; parse inline comment
	    (unless comment
	      (setq comment (and (vhdl-parse-string "--\\s-*\\([^\n]*\\)" t)
				 (match-string-no-properties 1))))
	    (setq return-group-comment (vhdl-parse-group-comment))
	    (vhdl-parse-string "\\(;\\|\\(is\\|\\(return\\)\\)\\>\\)\\s-*")
	    ;; parse return type
	    (when (match-string 3)
	      (vhdl-parse-string "[ \t\n]*\\(.+\\)[ \t\n]*\\(;\\|is\\>\\)\\s-*")
	      (setq return-type (match-string-no-properties 1))
	      (when (and return-type
			 (string-match "\\(\\s-*--\\s-*\\)\\(.*\\)" return-type))
		(setq return-comment (substring return-type (match-beginning 2)))
		(setq return-type (substring return-type 0 (match-beginning 1))))
	      ;; strip of trailing group-comment
	      (string-match "\\(\\(\\s-*\\S-+\\)+\\)\\s-*" return-type)
	      (setq return-type (substring return-type 0 (match-end 1)))
	      ;; parse return comment
	      (unless return-comment
		(setq return-comment (and (vhdl-parse-string "--\\s-*\\([^\n]*\\)" t)
					  (match-string-no-properties 1)))))
	    ;; parse inline comment
	    (unless comment
	      (setq comment (and (vhdl-parse-string "--\\s-*\\([^\n]*\\)" t)
				 (match-string-no-properties 1))))
	    ;; save everything in list
	    (setq param-list (append param-list
				     (list (list names object direct type init
						 comment group-comment))))
	    ;; parse group comment and spacing
	    (setq group-comment (vhdl-parse-group-comment)))
	  (message "Reading interface of subprogram \"%s\"...done" name)
	  nil)))
      ;; finish parsing
      (if parse-error
	  (error parse-error)
	(setq vhdl-subprog-list
	      (list name kind param-list return-type return-comment
		    return-group-comment)
	      vhdl-subprog-flattened nil)))))

(defun vhdl-subprog-paste-specification (kind)
  "Paste as a subprogram specification."
  (indent-according-to-mode)
  (let ((margin (current-column))
	(param-list (nth 2 vhdl-subprog-list))
	list-margin start names param)
    ;; paste keyword and name
    (vhdl-insert-keyword
     (if (eq (nth 1 vhdl-subprog-list) 'procedure) "PROCEDURE " "FUNCTION "))
    (insert (nth 0 vhdl-subprog-list))
    (if (not param-list)
	(if (eq kind 'decl) (insert ";") (vhdl-insert-keyword " is"))
      (setq start (point))
      ;; paste parameter list
      (insert " (")
      (unless vhdl-argument-list-indent
	(insert "\n") (indent-to (+ margin vhdl-basic-offset)))
      (setq list-margin (current-column))
      (while param-list
	(setq param (car param-list))
	;; paste group comment and spacing
	(when (memq vhdl-include-group-comments (list kind 'always))
	  (vhdl-paste-group-comment (nth 6 param) list-margin))
	;; paste object
	(when (nth 1 param) (insert (nth 1 param) " "))
	;; paste names
	(setq names (nth 0 param))
	(while names
	  (insert (car names))
	  (setq names (cdr names))
	  (when names (insert ", ")))
	;; paste direction
	(insert " : ")
	(when (nth 2 param) (insert (nth 2 param) " "))
	;; paste type
	(insert (nth 3 param))
	;; paste initialization
	(when (nth 4 param) (insert " := " (nth 4 param)))
	;; terminate line
	(if (cdr param-list)
	    (insert ";")
	  (insert ")")
	  (when (null (nth 3 vhdl-subprog-list))
	    (if (eq kind 'decl) (insert ";") (vhdl-insert-keyword " is"))))
	;; paste comment
	(when (and vhdl-include-port-comments (nth 5 param))
	  (vhdl-comment-insert-inline (nth 5 param) t))
	(setq param-list (cdr param-list))
	(when param-list (insert "\n") (indent-to list-margin)))
      (when (nth 3 vhdl-subprog-list)
	(insert "\n") (indent-to list-margin)
	;; paste group comment and spacing
	(when (memq vhdl-include-group-comments (list kind 'always))
	  (vhdl-paste-group-comment (nth 5 vhdl-subprog-list) list-margin))
	;; paste return type
	(insert "return " (nth 3 vhdl-subprog-list))
	(if (eq kind 'decl) (insert ";") (vhdl-insert-keyword " is"))
	(when (and vhdl-include-port-comments (nth 4 vhdl-subprog-list))
	  (vhdl-comment-insert-inline (nth 4 vhdl-subprog-list) t)))
      ;; align parameter list
      (when vhdl-auto-align (vhdl-align-region-groups start (point) 1 t)))
    ;; paste body
    (when (eq kind 'body)
      (insert "\n")
      (vhdl-template-begin-end
       (unless (vhdl-standard-p '87)
	 (if (eq (nth 1 vhdl-subprog-list) 'procedure) "PROCEDURE" "FUNCTION"))
       (nth 0 vhdl-subprog-list) margin))))

(defun vhdl-subprog-paste-declaration ()
  "Paste as a subprogram declaration."
  (interactive)
  (if (not vhdl-subprog-list)
      (error "ERROR:  No subprogram interface read")
    (message "Pasting interface as subprogram declaration \"%s\"..."
	     (car vhdl-subprog-list))
    ;; paste specification
    (vhdl-subprog-paste-specification 'decl)
    (message "Pasting interface as subprogram declaration \"%s\"...done"
	     (car vhdl-subprog-list))))

(defun vhdl-subprog-paste-body ()
  "Paste as a subprogram body."
  (interactive)
  (if (not vhdl-subprog-list)
      (error "ERROR:  No subprogram interface read")
    (message "Pasting interface as subprogram body \"%s\"..."
	     (car vhdl-subprog-list))
    ;; paste specification and body
    (vhdl-subprog-paste-specification 'body)
    (message "Pasting interface as subprogram body \"%s\"...done"
	     (car vhdl-subprog-list))))

(defun vhdl-subprog-paste-call ()
  "Paste as a subprogram call."
  (interactive)
  (if (not vhdl-subprog-list)
      (error "ERROR:  No subprogram interface read")
    (let ((orig-vhdl-subprog-list vhdl-subprog-list)
	  param-list margin list-margin param start)
      ;; flatten local copy of interface list (must be flat for parameter mapping)
      (vhdl-subprog-flatten)
      (setq param-list (nth 2 vhdl-subprog-list))
      (indent-according-to-mode)
      (setq margin (current-indentation))
      (message "Pasting interface as subprogram call \"%s\"..."
	       (car vhdl-subprog-list))
      ;; paste name
      (insert (nth 0 vhdl-subprog-list))
      (if (not param-list)
	  (insert ";")
	(setq start (point))
	;; paste parameter list
	(insert " (")
	(unless vhdl-argument-list-indent
	  (insert "\n") (indent-to (+ margin vhdl-basic-offset)))
	(setq list-margin (current-column))
	(while param-list
	  (setq param (car param-list))
	  ;; paste group comment and spacing
	  (when (eq vhdl-include-group-comments 'always)
	    (vhdl-paste-group-comment (nth 6 param) list-margin))
	  ;; paste formal port
	  (insert (car (nth 0 param)) " => ")
	  (setq param-list (cdr param-list))
	  (insert (if param-list "," ");"))
	  ;; paste comment
	  (when (and vhdl-include-port-comments (nth 5 param))
	    (vhdl-comment-insert-inline (nth 5 param)))
	  (when param-list (insert "\n") (indent-to list-margin)))
	;; align parameter list
	(when vhdl-auto-align
	  (vhdl-align-region-groups start (point) 1)))
      (message "Pasting interface as subprogram call \"%s\"...done"
	       (car vhdl-subprog-list))
      (setq vhdl-subprog-list orig-vhdl-subprog-list))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellaneous
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hippie expand customization

(defvar vhdl-expand-upper-case nil)

(defun vhdl-try-expand-abbrev (old)
  "Try expanding abbreviations from `vhdl-abbrev-list'."
  (unless old
    (he-init-string (he-dabbrev-beg) (point))
    (setq he-expand-list
	  (let ((abbrev-list vhdl-abbrev-list)
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
;		  (equal (car he-expand-list) he-search-string)))
    (unless (stringp (car he-expand-list))
      (setq vhdl-expand-upper-case (car he-expand-list)))
    (setq he-expand-list (cdr he-expand-list)))
  (if (null he-expand-list)
      (progn (when old (he-reset-string))
	     nil)
    (he-substitute-string
     (if vhdl-expand-upper-case
	 (upcase (car he-expand-list))
       (car he-expand-list))
     t)
    (setq he-expand-list (cdr he-expand-list))
    t))

(defun vhdl-he-list-beg ()
  "Also looks at the word before `(' in order to better match parenthesized
expressions (e.g. for index ranges of types and signals)."
  (save-excursion
    (condition-case ()
	(progn (backward-up-list 1)
 	       (skip-syntax-backward "w_")) ; crashes in `viper-mode'
      (error ()))
    (point)))

;; override `he-list-beg' from `hippie-exp'
(unless (and (boundp 'viper-mode) viper-mode)
 (defalias 'he-list-beg 'vhdl-he-list-beg))

;; function for expanding abbrevs and dabbrevs
(defun vhdl-expand-abbrev (arg))
(fset 'vhdl-expand-abbrev (make-hippie-expand-function
			   '(try-expand-dabbrev
			     try-expand-dabbrev-all-buffers
			     vhdl-try-expand-abbrev)))

;; function for expanding parenthesis
(defun vhdl-expand-paren (arg))
(fset 'vhdl-expand-paren (make-hippie-expand-function
			  '(try-expand-list
			    try-expand-list-all-buffers)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Case fixing

(defun vhdl-fix-case-region-1 (beg end upper-case word-regexp &optional count)
  "Convert all words matching WORD-REGEXP in region to lower or upper case,
depending on parameter UPPER-CASE."
  (let ((case-replace nil)
	(last-update 0))
    (vhdl-prepare-search-2
     (save-excursion
       (goto-char end)
       (setq end (point-marker))
       (goto-char beg)
       (while (re-search-forward word-regexp end t)
	 (or (vhdl-in-literal)
	     (if upper-case
		 (upcase-word -1)
	       (downcase-word -1)))
	 (when (and count vhdl-progress-interval (not noninteractive)
		    (< vhdl-progress-interval
		       (- (nth 1 (current-time)) last-update)))
	   (message "Fixing case... (%2d%s)"
		    (+ (* count 25) (/ (* 25 (- (point) beg)) (- end beg)))
		    "%")
	   (setq last-update (nth 1 (current-time)))))
       (goto-char end)))))

(defun vhdl-fix-case-region (beg end &optional arg)
  "Convert all VHDL words in region to lower or upper case, depending on
options vhdl-upper-case-{keywords,types,attributes,enum-values}."
  (interactive "r\nP")
  (vhdl-fix-case-region-1
   beg end vhdl-upper-case-keywords vhdl-keywords-regexp 0)
  (vhdl-fix-case-region-1
   beg end vhdl-upper-case-types vhdl-types-regexp 1)
  (vhdl-fix-case-region-1
   beg end vhdl-upper-case-attributes (concat "'" vhdl-attributes-regexp) 2)
  (vhdl-fix-case-region-1
   beg end vhdl-upper-case-enum-values vhdl-enum-values-regexp 3)
  (when vhdl-progress-interval (message "Fixing case...done")))

(defun vhdl-fix-case-buffer ()
  "Convert all VHDL words in buffer to lower or upper case, depending on
options vhdl-upper-case-{keywords,types,attributes,enum-values}."
  (interactive)
  (vhdl-fix-case-region (point-min) (point-max)))

(defun vhdl-fix-case-word (&optional arg)
  "Convert word after cursor to upper case if necessary."
  (interactive "p")
  (save-excursion
    (when arg (backward-word 1))
    (vhdl-prepare-search-1
     (when (and vhdl-upper-case-keywords
		(looking-at vhdl-keywords-regexp))
       (upcase-word 1))
     (when (and vhdl-upper-case-types
		(looking-at vhdl-types-regexp))
       (upcase-word 1))
     (when (and vhdl-upper-case-attributes
		(looking-at vhdl-attributes-regexp))
       (upcase-word 1))
     (when (and vhdl-upper-case-enum-values
		(looking-at vhdl-enum-values-regexp))
       (upcase-word 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Line handling functions

(defun vhdl-current-line ()
  "Return the line number of the line containing point."
  (save-restriction
    (widen)
    (1+ (count-lines (point-min) (point-at-bol)))))

(defun vhdl-line-kill-entire (&optional arg)
  "Delete entire line."
  (interactive "p")
  (beginning-of-line)
  (kill-line (or arg 1)))

(defun vhdl-line-kill (&optional arg)
  "Kill current line."
  (interactive "p")
  (vhdl-line-kill-entire arg))

(defun vhdl-line-copy (&optional arg)
  "Copy current line."
  (interactive "p")
  (save-excursion
    (let ((position (point-at-bol)))
      (forward-line (or arg 1))
      (copy-region-as-kill position (point)))))

(defun vhdl-line-yank ()
  "Yank entire line."
  (interactive)
  (beginning-of-line)
  (yank))

(defun vhdl-line-expand (&optional prefix-arg)
  "Hippie-expand current line."
  (interactive "P")
  (let ((case-fold-search t) (case-replace nil)
	(hippie-expand-try-functions-list
	 '(try-expand-line try-expand-line-all-buffers)))
    (hippie-expand prefix-arg)))

(defun vhdl-line-transpose-next (&optional arg)
  "Interchange this line with next line."
  (interactive "p")
  (forward-line 1)
  (transpose-lines (or arg 1))
  (forward-line -1))

(defun vhdl-line-transpose-previous (&optional arg)
  "Interchange this line with previous line."
  (interactive "p")
  (forward-line 1)
  (transpose-lines (- 0 (or arg 0)))
  (forward-line -1))

(defun vhdl-line-open ()
  "Open a new line and indent."
  (interactive)
  (end-of-line -0)
  (newline-and-indent))

(defun vhdl-delete-indentation ()
  "Join lines.  That is, call `delete-indentation' with `fill-prefix' so that
it works within comments too."
  (interactive)
  (let ((fill-prefix "-- "))
    (delete-indentation)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Move functions

(defun vhdl-forward-same-indent ()
  "Move forward to next line with same indent."
  (interactive)
  (let ((pos (point))
	(indent (current-indentation)))
    (beginning-of-line 2)
    (while (and (not (eobp))
		(or (looking-at "^\\s-*\\(--.*\\)?$")
		    (> (current-indentation) indent)))
      (beginning-of-line 2))
    (if (= (current-indentation) indent)
	(back-to-indentation)
      (message "No following line with same indent found in this block")
      (goto-char pos)
      nil)))

(defun vhdl-backward-same-indent ()
  "Move backward to previous line with same indent."
  (interactive)
  (let ((pos (point))
	(indent (current-indentation)))
    (beginning-of-line -0)
    (while (and (not (bobp))
		(or (looking-at "^\\s-*\\(--.*\\)?$")
		    (> (current-indentation) indent)))
      (beginning-of-line -0))
    (if (= (current-indentation) indent)
	(back-to-indentation)
      (message "No preceding line with same indent found in this block")
      (goto-char pos)
      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Statistics

(defun vhdl-statistics-buffer ()
  "Get some file statistics."
  (interactive)
  (let ((no-stats 0)
	(no-code-lines 0)
	(no-lines (count-lines (point-min) (point-max))))
    (save-excursion
      ;; count statements
      (goto-char (point-min))
      (while (re-search-forward "\\(--.*\n\\|\"[^\"\n]*[\"\n]\\)\\|;" nil t)
	(if (match-string 1)
	    (goto-char (match-end 1))
	  (setq no-stats (1+ no-stats))))
      ;; count code lines
      (goto-char (point-min))
      (while (not (eobp))
	(unless (looking-at "^\\s-*\\(--.*\\)?$")
	  (setq no-code-lines (1+ no-code-lines)))
	(beginning-of-line 2)))
    ;; print results
    (message "\n\
File statistics: \"%s\"\n\
---------------------\n\
# statements  : %5d\n\
# code lines  : %5d\n\
# total lines : %5d\n\ "
	     (buffer-file-name) no-stats no-code-lines no-lines)
    (unless vhdl-emacs-21 (vhdl-show-messages))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Help functions

(defun vhdl-re-search-forward (regexp &optional bound noerror count)
  "Like `re-search-forward', but does not match within literals."
  (let (pos)
    (save-excursion
      (while (and (setq pos (re-search-forward regexp bound noerror count))
		  (vhdl-in-literal))))
    (when pos (goto-char pos))
    pos))

(defun vhdl-re-search-backward (regexp &optional bound noerror count)
  "Like `re-search-backward', but does not match within literals."
  (let (pos)
    (save-excursion
      (while (and (setq pos (re-search-backward regexp bound noerror count))
		  (vhdl-in-literal))))
    (when pos (goto-char pos))
    pos))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Project
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vhdl-set-project (name)
  "Set current project to NAME."
  (interactive
   (list (let ((completion-ignore-case t))
	   (completing-read "Project name: " vhdl-project-alist nil t))))
  (cond ((equal name "")
	 (setq vhdl-project nil)
	 (message "Current VHDL project: None"))
	((assoc name vhdl-project-alist)
	 (setq vhdl-project name)
	 (message "Current VHDL project: \"%s\"" name))
	(t
	 (vhdl-warning (format "Unknown VHDL project: \"%s\"" name))))
  (vhdl-speedbar-update-current-project))

(defun vhdl-set-default-project ()
  "Set current project as default on startup."
  (interactive)
  (customize-set-variable 'vhdl-project vhdl-project)
  (customize-save-customized))

(defun vhdl-toggle-project (name token indent)
  "Set current project to NAME or unset if NAME is current project."
  (vhdl-set-project (if (equal name vhdl-project) "" name)))

(defun vhdl-export-project (file-name)
  "Write project setup for current project."
  (interactive
   (let ((name (vhdl-resolve-env-variable
		(vhdl-replace-string
		 (cons "\\(.*\\) \\(.*\\)" (car vhdl-project-file-name))
		 (concat (subst-char-in-string
			  ?  ?_ (or (vhdl-project-p)
				    (error "ERROR:  No current project")))
			 " " (user-login-name))))))
     (list (read-file-name
	    "Write project file: "
	    (when (file-name-absolute-p name) "") nil nil name))))
  (setq file-name (abbreviate-file-name file-name))
  (let ((orig-buffer (current-buffer)))
    (unless (file-exists-p (file-name-directory file-name))
      (make-directory (file-name-directory file-name) t))
    (if (not (file-writable-p file-name))
	(error "ERROR:  File not writable: \"%s\"" file-name)
      (set-buffer (find-file-noselect file-name t t))
      (erase-buffer)
      (insert ";; -*- Emacs-Lisp -*-\n\n"
	      ";;; " (file-name-nondirectory file-name)
	      " - project setup file for Emacs VHDL Mode " vhdl-version "\n\n"
	      ";; Project : " vhdl-project "\n"
	      ";; Saved   : " (format-time-string "%Y-%m-%d %T ")
	      (user-login-name) "\n\n\n"
	      ";; project name\n"
	      "(setq vhdl-project \"" vhdl-project "\")\n\n"
	      ";; project setup\n"
	      "(aput 'vhdl-project-alist vhdl-project\n'")
      (pp (aget vhdl-project-alist vhdl-project) (current-buffer))
      (insert ")\n")
      (save-buffer)
      (kill-buffer (current-buffer))
      (set-buffer orig-buffer))))

(defun vhdl-import-project (file-name &optional auto not-make-current)
  "Read project setup and set current project."
  (interactive
   (let ((name (vhdl-resolve-env-variable
		(vhdl-replace-string
		 (cons "\\(.*\\) \\(.*\\)" (car vhdl-project-file-name))
		 (concat "" " " (user-login-name))))))
     (list (read-file-name
	    "Read project file: " (when (file-name-absolute-p name) "") nil t
	    (file-name-directory name)))))
  (when (file-exists-p file-name)
    (condition-case ()
	(let ((current-project vhdl-project))
	  (load-file file-name)
	  (when (/= (length (aget vhdl-project-alist vhdl-project t)) 10)
	    (adelete 'vhdl-project-alist vhdl-project)
	    (error ""))
	  (when not-make-current
	    (setq vhdl-project current-project))
	  (vhdl-update-mode-menu)
	  (vhdl-speedbar-refresh)
	  (unless not-make-current
	    (message "Current VHDL project: \"%s\"%s"
		     vhdl-project (if auto " (auto-loaded)" ""))))
      (error (vhdl-warning
	      (format "ERROR:  Invalid project setup file: \"%s\"" file-name))))))

(defun vhdl-duplicate-project ()
  "Duplicate setup of current project."
  (interactive)
  (let ((new-name (read-from-minibuffer "New project name: "))
	(project-entry (aget vhdl-project-alist vhdl-project t)))
    (setq vhdl-project-alist
	  (append vhdl-project-alist
		  (list (cons new-name project-entry))))
    (vhdl-update-mode-menu)))

(defun vhdl-auto-load-project ()
  "Automatically load project setup at startup."
  (let ((file-name-list vhdl-project-file-name)
	file-list list-length)
    (while file-name-list
      (setq file-list
	    (append file-list
		    (file-expand-wildcards
		     (vhdl-resolve-env-variable
		      (vhdl-replace-string
		       (cons "\\(.*\\) \\(.*\\)" (car file-name-list))
		       (concat "\*" " " (user-login-name)))))))
      (setq list-length (or list-length (length file-list)))
      (setq file-name-list (cdr file-name-list)))
    (while file-list
      (vhdl-import-project (expand-file-name (car file-list)) t
			   (not (> list-length 0)))
      (setq list-length (1- list-length))
      (setq file-list (cdr file-list)))))

;; automatically load project setup when idle after startup
(when (memq 'startup vhdl-project-auto-load)
  (if noninteractive
      (vhdl-auto-load-project)
    (vhdl-run-when-idle .1 nil 'vhdl-auto-load-project)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hideshow
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (using `hideshow.el')

(defconst vhdl-hs-start-regexp
  (concat
   "\\(^\\)\\s-*\\("
   ;; generic/port clause
   "\\(generic\\|port\\)[ \t\n]*(\\|"
   ;; component
   "component\\>\\|"
   ;; component instantiation
   "\\(\\w\\|\\s_\\)+[ \t\n]*:[ \t\n]*"
   "\\(\\(component\\|configuration\\|entity\\)[ \t\n]+\\)?"
   "\\(\\w\\|\\s_\\)+\\([ \t\n]*(\\(\\w\\|\\s_\\)+)\\)?[ \t\n]*"
   "\\(generic\\|port\\)[ \t\n]+map[ \t\n]*(\\|"
   ;; subprogram
   "\\(function\\|procedure\\)\\>\\|"
   ;; process, block
   "\\(\\(\\w\\|\\s_\\)+[ \t\n]*:[ \t\n]*\\)?\\(process\\|block\\)\\>\\|"
   ;; configuration declaration
   "configuration\\>"
   "\\)")
  "Regexp to match start of construct to hide.")

(defun vhdl-hs-forward-sexp-func (count)
  "Find end of construct to hide (for hideshow).  Only searches forward."
  (let ((pos (point)))
    (vhdl-prepare-search-2
     (beginning-of-line)
     (cond
      ;; generic/port clause
      ((looking-at "^\\s-*\\(generic\\|port\\)[ \t\n]*(")
       (goto-char (match-end 0))
       (backward-char)
       (forward-sexp))
      ;; component declaration
      ((looking-at "^\\s-*component\\>")
       (re-search-forward "^\\s-*end\\s-+component\\>" nil t))
      ;; component instantiation
      ((looking-at
	(concat
	 "^\\s-*\\w+\\s-*:[ \t\n]*"
	 "\\(\\(component\\|configuration\\|entity\\)[ \t\n]+\\)?"
	 "\\w+\\(\\s-*(\\w+)\\)?[ \t\n]*"
	 "\\(generic\\|port\\)\\s-+map[ \t\n]*("))
       (goto-char (match-end 0))
       (backward-char)
       (forward-sexp)
       (setq pos (point))
       (vhdl-forward-syntactic-ws)
       (when (looking-at "port\\s-+map[ \t\n]*(")
	 (goto-char (match-end 0))
	 (backward-char)
	 (forward-sexp)
	 (setq pos (point)))
       (goto-char pos))
      ;; subprogram declaration/body
      ((looking-at "^\\s-*\\(function\\|procedure\\)\\s-+\\(\\w+\\|\".+\"\\)")
       (goto-char (match-end 0))
       (vhdl-forward-syntactic-ws)
       (when (looking-at "(")
	 (forward-sexp))
       (while (and (re-search-forward "\\(;\\)\\|\\(\\<is\\>\\)" nil t)
		   (vhdl-in-literal)))
       ;; subprogram body
       (when (match-string 2)
	 (re-search-forward "^\\s-*\\<begin\\>" nil t)
	 (backward-word 1)
	 (vhdl-forward-sexp)))
      ;; block (recursive)
      ((looking-at "^\\s-*\\w+\\s-*:\\s-*block\\>")
       (goto-char (match-end 0))
       (while (and (re-search-forward "^\\s-*\\(\\(\\w+\\s-*:\\s-*block\\>\\)\\|\\(end\\s-+block\\>\\)\\)" nil t)
		   (match-beginning 2))
	 (vhdl-hs-forward-sexp-func count)))
      ;; process
      ((looking-at "^\\s-*\\(\\w+\\s-*:\\s-*\\)?process\\>")
       (re-search-forward "^\\s-*end\\s-+process\\>" nil t))
      ;; configuration declaration
      ((looking-at "^\\s-*configuration\\>")
       (forward-word 4)
       (vhdl-forward-sexp))
      (t (goto-char pos))))))

(defun vhdl-hideshow-init ()
  "Initialize `hideshow'."
  (when vhdl-hideshow-menu
    (vhdl-hs-minor-mode 1)))

(defun vhdl-hs-minor-mode (&optional arg)
  "Toggle hideshow minor mode and update menu bar."
  (interactive "P")
  (require 'hideshow)
  ;; check for hideshow version 5.x
  (if (not (boundp 'hs-block-start-mdata-select))
      (vhdl-warning-when-idle "Install included `hideshow.el' patch first (see INSTALL file)")
    ;; initialize hideshow
    (unless (assoc 'vhdl-mode hs-special-modes-alist)
      (setq hs-special-modes-alist
	    (cons (list 'vhdl-mode vhdl-hs-start-regexp nil "--\\( \\|$\\)"
			'vhdl-hs-forward-sexp-func nil)
		  hs-special-modes-alist)))
    (if (featurep 'xemacs) (make-local-hook 'hs-minor-mode-hook))
    (if vhdl-hide-all-init
	(add-hook 'hs-minor-mode-hook 'hs-hide-all nil t)
      (remove-hook 'hs-minor-mode-hook 'hs-hide-all t))
    (hs-minor-mode arg)
    (force-mode-line-update)))		; hack to update menu bar


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Font locking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (using `font-lock.el')

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Help functions

(defun vhdl-within-translate-off ()
  "Return point if within translate-off region, else nil."
  (and (save-excursion
	 (re-search-backward
	  "^\\s-*--\\s-*pragma\\s-*translate_\\(on\\|off\\)\\s-*\n" nil t))
       (equal "off" (match-string 1))
       (point)))

(defun vhdl-start-translate-off (limit)
  "Return point before translate-off pragma if before LIMIT, else nil."
  (when (re-search-forward
	 "^\\s-*--\\s-*pragma\\s-*translate_off\\s-*\n" limit t)
    (match-beginning 0)))

(defun vhdl-end-translate-off (limit)
  "Return point after translate-on pragma if before LIMIT, else nil."
  (re-search-forward "^\\s-*--\\s-*pragma\\s-*translate_on\\s-*\n" limit t))

(defun vhdl-match-translate-off (limit)
  "Match a translate-off block, setting match-data and returning t, else nil."
  (when (< (point) limit)
    (let ((start (or (vhdl-within-translate-off)
		     (vhdl-start-translate-off limit)))
	  (case-fold-search t))
      (when start
	(let ((end (or (vhdl-end-translate-off limit) limit)))
	  (set-match-data (list start end))
	  (goto-char end))))))

(defun vhdl-font-lock-match-item (limit)
  "Match, and move over, any declaration item after point.  Adapted from
`font-lock-match-c-style-declaration-item-and-skip-to-next'."
  (condition-case nil
      (save-restriction
	(narrow-to-region (point-min) limit)
	;; match item
	(when (looking-at "\\s-*\\([a-zA-Z]\\w*\\)")
	  (save-match-data
	    (goto-char (match-end 1))
	    ;; move to next item
	    (if (looking-at "\\(\\s-*,\\)")
		(goto-char (match-end 1))
	      (end-of-line) t))))
    (error t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax definitions

(defconst vhdl-font-lock-syntactic-keywords
  '(("\\(\'\\).\\(\'\\)" (1 (7 . ?\')) (2 (7 . ?\'))))
  "Mark single quotes as having string quote syntax in 'c' instances.")

(defvar vhdl-font-lock-keywords nil
  "Regular expressions to highlight in VHDL Mode.")

(defvar vhdl-font-lock-keywords-0
  ;; set in `vhdl-font-lock-init' because dependent on user options
  "For consideration as a value of `vhdl-font-lock-keywords'.
This does highlighting of template prompts and directives (pragmas).")

(defvar vhdl-font-lock-keywords-1 nil
  ;; set in `vhdl-font-lock-init' because dependent on user options
  "For consideration as a value of `vhdl-font-lock-keywords'.
This does highlighting of keywords and standard identifiers.")

(defconst vhdl-font-lock-keywords-2
  (list
   ;; highlight names of units, subprograms, and components when declared
   (list
    (concat
     "^\\s-*\\("
     "architecture\\|configuration\\|entity\\|package\\(\\s-+body\\)?\\|"
     "\\(\\(impure\\|pure\\)\\s-+\\)?function\\|procedure\\|component"
     "\\)\\s-+\\(\\w+\\)")
    5 'font-lock-function-name-face)

   ;; highlight entity names of architectures and configurations
   (list
    "^\\s-*\\(architecture\\|configuration\\)\\s-+\\w+\\s-+of\\s-+\\(\\w+\\)"
    2 'font-lock-function-name-face)

   ;; highlight labels of common constructs
   (list
    (concat
     "^\\s-*\\(\\w+\\)\\s-*:[ \t\n]*\\(\\("
     "assert\\|block\\|case\\|exit\\|for\\|if\\|loop\\|next\\|null\\|"
     "postponed\\|process\\|"
     (when (vhdl-standard-p 'ams) "procedural\\|")
     "with\\|while"
     "\\)\\>\\|\\w+\\s-*\\(([^\n]*)\\|\\.\\w+\\)*\\s-*<=\\)")
    1 'font-lock-function-name-face)

   ;; highlight label and component name of component instantiations
   (list
    (concat
     "^\\s-*\\(\\w+\\)\\s-*:[ \t\n]*\\(\\w+\\)"
     "\\(\\s-*\\(--[^\n]*\\)?$\\|\\s-+\\(generic\\|port\\)\\s-+map\\>\\)")
    '(1 font-lock-function-name-face) '(2 font-lock-function-name-face))

   ;; highlight label and instantiated unit of component instantiations
   (list
    (concat
     "^\\s-*\\(\\w+\\)\\s-*:[ \t\n]*"
     "\\(component\\|configuration\\|entity\\)\\s-+"
     "\\(\\w+\\)\\(\\.\\(\\w+\\)\\)?\\(\\s-*(\\(\\w+\\))\\)?")
    '(1 font-lock-function-name-face) '(3 font-lock-function-name-face)
    '(5 font-lock-function-name-face nil t)
    '(7 font-lock-function-name-face nil t))

   ;; highlight names and labels at end of constructs
   (list
    (concat
     "^\\s-*end\\s-+\\(\\("
     "architecture\\|block\\|case\\|component\\|configuration\\|entity\\|"
     "for\\|function\\|generate\\|if\\|loop\\|package\\(\\s-+body\\)?\\|"
     "procedure\\|\\(postponed\\s-+\\)?process\\|"
     (when (vhdl-standard-p 'ams) "procedural\\|")
     "units"
     "\\)\\s-+\\)?\\(\\w*\\)")
    5 'font-lock-function-name-face)

   ;; highlight labels in exit and next statements
   (list
    (concat
     "^\\s-*\\(\\w+\\s-*:\\s-*\\)?\\(exit\\|next\\)\\s-+\\(\\w*\\)")
    3 'font-lock-function-name-face)

   ;; highlight entity name in attribute specifications
   (list
    (concat
     "^\\s-*attribute\\s-+\\w+\\s-+of\\s-+\\(\\w+\\(,\\s-*\\w+\\)*\\)\\s-*:")
    1 'font-lock-function-name-face)

   ;; highlight labels in block and component specifications
   (list
    (concat
     "^\\s-*for\\s-+\\(\\w+\\(,\\s-*\\w+\\)*\\)\\>\\s-*"
     "\\(:[ \t\n]*\\(\\w+\\)\\|[^i \t]\\)")
    '(1 font-lock-function-name-face) '(4 font-lock-function-name-face nil t))

   ;; highlight names in library clauses
   (list "^\\s-*library\\>"
	 '(vhdl-font-lock-match-item nil nil (1 font-lock-function-name-face)))

   ;; highlight names in use clauses
   (list
    (concat
     "\\<use\\s-+\\(\\(entity\\|configuration\\)\\s-+\\)?"
     "\\(\\w+\\)\\(\\.\\(\\w+\\)\\)?\\((\\(\\w+\\))\\)?")
    '(3 font-lock-function-name-face) '(5 font-lock-function-name-face nil t)
    '(7 font-lock-function-name-face nil t))

   ;; highlight attribute name in attribute declarations/specifications
   (list
    (concat
     "^\\s-*attribute\\s-+\\(\\w+\\)")
    1 'vhdl-font-lock-attribute-face)

   ;; highlight type/nature name in (sub)type/(sub)nature declarations
   (list
    (concat
     "^\\s-*\\(sub\\)?\\(nature\\|type\\)\\s-+\\(\\w+\\)")
    3 'font-lock-type-face)

   ;; highlight signal/variable/constant declaration names
   (list "\\(:[^=]\\)"
	 '(vhdl-font-lock-match-item
	   (progn (goto-char (match-beginning 1))
		  (skip-syntax-backward " ")
		  (skip-syntax-backward "w_")
		  (skip-syntax-backward " ")
		  (while (= (preceding-char) ?,)
		    (backward-char 1)
		    (skip-syntax-backward " ")
		    (skip-syntax-backward "w_")
		    (skip-syntax-backward " ")))
;  		  (skip-chars-backward "^-(\n\";")
	   (goto-char (match-end 1)) (1 font-lock-variable-name-face)))

   ;; highlight formal parameters in component instantiations and subprogram
   ;; calls
   (list "\\(=>\\)"
	 '(vhdl-font-lock-match-item
	   (progn (goto-char (match-beginning 1))
		  (skip-syntax-backward " ")
		  (while (= (preceding-char) ?\)) (backward-sexp))
		  (skip-syntax-backward "w_")
		  (skip-syntax-backward " ")
		  (when (memq (preceding-char) '(?n ?N ?|))
		    (goto-char (point-max))))
	   (goto-char (match-end 1)) (1 font-lock-variable-name-face)))

   ;; highlight alias/group/quantity declaration names and for-loop/-generate
   ;; variables
   (list "\\<\\(alias\\|for\\|group\\|quantity\\)\\s-+\\w+\\s-+\\(across\\|in\\|is\\)\\>"
	 '(vhdl-font-lock-match-item
	   (progn (goto-char (match-end 1)) (match-beginning 2))
	   nil (1 font-lock-variable-name-face)))
   )
  "For consideration as a value of `vhdl-font-lock-keywords'.
This does context sensitive highlighting of names and labels.")

(defvar vhdl-font-lock-keywords-3 nil
  ;; set in `vhdl-font-lock-init' because dependent on user options
  "For consideration as a value of `vhdl-font-lock-keywords'.
This does highlighting of words with special syntax.")

(defvar vhdl-font-lock-keywords-4 nil
  ;; set in `vhdl-font-lock-init' because dependent on user options
  "For consideration as a value of `vhdl-font-lock-keywords'.
This does highlighting of additional reserved words.")

(defconst vhdl-font-lock-keywords-5
  ;; background highlight translate-off regions
  '((vhdl-match-translate-off (0 vhdl-font-lock-translate-off-face append)))
  "For consideration as a value of `vhdl-font-lock-keywords'.
This does background highlighting of translate-off regions.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Font and color definitions

(defvar vhdl-font-lock-prompt-face	   'vhdl-font-lock-prompt-face
  "Face name to use for prompts.")

(defvar vhdl-font-lock-attribute-face      'vhdl-font-lock-attribute-face
  "Face name to use for standardized attributes.")

(defvar vhdl-font-lock-enumvalue-face      'vhdl-font-lock-enumvalue-face
  "Face name to use for standardized enumeration values.")

(defvar vhdl-font-lock-function-face       'vhdl-font-lock-function-face
  "Face name to use for standardized functions and packages.")

(defvar vhdl-font-lock-directive-face      'vhdl-font-lock-directive-face
  "Face name to use for directives.")

(defvar vhdl-font-lock-reserved-words-face 'vhdl-font-lock-reserved-words-face
  "Face name to use for additional reserved words.")

(defvar vhdl-font-lock-translate-off-face  'vhdl-font-lock-translate-off-face
  "Face name to use for translate-off regions.")

;; face names to use for words with special syntax.
(let ((syntax-alist vhdl-special-syntax-alist)
      name)
  (while syntax-alist
    (setq name (vhdl-function-name
		"vhdl-font-lock" (nth 0 (car syntax-alist)) "face"))
    (eval `(defvar ,name ',name
	     ,(concat "Face name to use for "
		      (nth 0 (car syntax-alist)) ".")))
    (setq syntax-alist (cdr syntax-alist))))

(defgroup vhdl-highlight-faces nil
  "Faces for highlighting."
  :group 'vhdl-highlight)

;; add faces used from `font-lock'
(custom-add-to-group
 'vhdl-highlight-faces 'font-lock-comment-face 'custom-face)
(custom-add-to-group
 'vhdl-highlight-faces 'font-lock-string-face 'custom-face)
(custom-add-to-group
 'vhdl-highlight-faces 'font-lock-keyword-face 'custom-face)
(custom-add-to-group
 'vhdl-highlight-faces 'font-lock-type-face 'custom-face)
(custom-add-to-group
 'vhdl-highlight-faces 'font-lock-function-name-face 'custom-face)
(custom-add-to-group
 'vhdl-highlight-faces 'font-lock-variable-name-face 'custom-face)

(defface vhdl-font-lock-prompt-face
  '((((min-colors 88) (class color) (background light))
     (:foreground "Red1" :bold t))
    (((class color) (background light)) (:foreground "Red" :bold t))
    (((class color) (background dark)) (:foreground "Pink" :bold t))
    (t (:inverse-video t)))
  "Font lock mode face used to highlight prompts."
  :group 'vhdl-highlight-faces)

(defface vhdl-font-lock-attribute-face
  '((((class color) (background light)) (:foreground "Orchid"))
    (((class color) (background dark)) (:foreground "LightSteelBlue"))
    (t (:italic t :bold t)))
  "Font lock mode face used to highlight standardized attributes."
  :group 'vhdl-highlight-faces)

(defface vhdl-font-lock-enumvalue-face
  '((((class color) (background light)) (:foreground "SaddleBrown"))
    (((class color) (background dark)) (:foreground "BurlyWood"))
    (t (:italic t :bold t)))
  "Font lock mode face used to highlight standardized enumeration values."
  :group 'vhdl-highlight-faces)

(defface vhdl-font-lock-function-face
  '((((class color) (background light)) (:foreground "Cyan4"))
    (((class color) (background dark)) (:foreground "Orchid1"))
    (t (:italic t :bold t)))
  "Font lock mode face used to highlight standardized functions and packages."
  :group 'vhdl-highlight-faces)

(defface vhdl-font-lock-directive-face
  '((((class color) (background light)) (:foreground "CadetBlue"))
    (((class color) (background dark)) (:foreground "Aquamarine"))
    (t (:italic t :bold t)))
  "Font lock mode face used to highlight directives."
  :group 'vhdl-highlight-faces)

(defface vhdl-font-lock-reserved-words-face
  '((((class color) (background light)) (:foreground "Orange" :bold t))
    (((min-colors 88) (class color) (background dark))
     (:foreground "Yellow1" :bold t))
    (((class color) (background dark)) (:foreground "Yellow" :bold t))
    (t ()))
  "Font lock mode face used to highlight additional reserved words."
  :group 'vhdl-highlight-faces)

(defface vhdl-font-lock-translate-off-face
  '((((class color) (background light)) (:background "LightGray"))
    (((class color) (background dark)) (:background "DimGray"))
    (t ()))
  "Font lock mode face used to background highlight translate-off regions."
  :group 'vhdl-highlight-faces)

;; font lock mode faces used to highlight words with special syntax.
(let ((syntax-alist vhdl-special-syntax-alist))
  (while syntax-alist
    (eval `(defface ,(vhdl-function-name
		      "vhdl-font-lock" (caar syntax-alist) "face")
	     '((((class color) (background light))
		(:foreground ,(nth 2 (car syntax-alist))))
	       (((class color) (background dark))
		(:foreground ,(nth 3 (car syntax-alist))))
	       (t ()))
	     ,(concat "Font lock mode face used to highlight "
		      (nth 0 (car syntax-alist)) ".")
	     :group 'vhdl-highlight-faces))
    (setq syntax-alist (cdr syntax-alist))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Font lock initialization

(defun vhdl-font-lock-init ()
  "Initialize fontification."
  ;; highlight template prompts and directives
  (setq vhdl-font-lock-keywords-0
	(list (list (concat "\\(^\\|[ \t(.']\\)\\(<"
			    vhdl-template-prompt-syntax ">\\)")
		    2 'vhdl-font-lock-prompt-face t)
	      (list (concat "--\\s-*"
			    vhdl-directive-keywords-regexp "\\s-+\\(.*\\)$")
		    2 'vhdl-font-lock-directive-face t)
	      ;; highlight c-preprocessor directives
	      (list "^#[ \t]*\\(\\w+\\)\\([ \t]+\\(\\w+\\)\\)?"
		    '(1 font-lock-builtin-face)
		    '(3 font-lock-variable-name-face nil t))))
  ;; highlight keywords and standardized types, attributes, enumeration
  ;; values, and subprograms
  (setq vhdl-font-lock-keywords-1
	(list
	 (list (concat "'" vhdl-attributes-regexp)
	       1 'vhdl-font-lock-attribute-face)
	 (list vhdl-types-regexp       1 'font-lock-type-face)
	 (list vhdl-functions-regexp   1 'vhdl-font-lock-function-face)
	 (list vhdl-packages-regexp    1 'vhdl-font-lock-function-face)
	 (list vhdl-enum-values-regexp 1 'vhdl-font-lock-enumvalue-face)
	 (list vhdl-keywords-regexp    1 'font-lock-keyword-face)))
  ;; highlight words with special syntax.
  (setq vhdl-font-lock-keywords-3
	(let ((syntax-alist vhdl-special-syntax-alist)
	      keywords)
	  (while syntax-alist
	    (setq keywords
		  (cons
		   (cons (concat "\\<\\(" (nth 1 (car syntax-alist)) "\\)\\>")
			 (vhdl-function-name
			  "vhdl-font-lock" (nth 0 (car syntax-alist)) "face"))
		   keywords))
	    (setq syntax-alist (cdr syntax-alist)))
	  keywords))
  ;; highlight additional reserved words
  (setq vhdl-font-lock-keywords-4
	(list (list vhdl-reserved-words-regexp 1
		    'vhdl-font-lock-reserved-words-face)))
  ;; highlight everything together
  (setq vhdl-font-lock-keywords
	(append
	 vhdl-font-lock-keywords-0
	 (when vhdl-highlight-keywords vhdl-font-lock-keywords-1)
	 (when (or vhdl-highlight-forbidden-words
		   vhdl-highlight-verilog-keywords) vhdl-font-lock-keywords-4)
	 (when vhdl-highlight-special-words vhdl-font-lock-keywords-3)
	 (when vhdl-highlight-names vhdl-font-lock-keywords-2)
	 (when vhdl-highlight-translate-off vhdl-font-lock-keywords-5))))

;; initialize fontification for VHDL Mode
(vhdl-font-lock-init)

(defun vhdl-fontify-buffer ()
  "Re-initialize fontification and fontify buffer."
  (interactive)
  (setq font-lock-defaults
	`(vhdl-font-lock-keywords
          nil ,(not vhdl-highlight-case-sensitive) ((?\_ . "w"))
          beginning-of-line))
  (when (fboundp 'font-lock-unset-defaults)
    (font-lock-unset-defaults))		; not implemented in XEmacs
  (font-lock-set-defaults)
  (font-lock-mode nil)
  (font-lock-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization for PostScript printing

(defun vhdl-ps-print-settings ()
  "Initialize custom face and page settings for PostScript printing."
  ;; define custom face settings
  (unless (or (not vhdl-print-customize-faces)
	      ps-print-color-p)
    (set (make-local-variable 'ps-bold-faces)
	 '(font-lock-keyword-face
	   font-lock-type-face
	   vhdl-font-lock-attribute-face
	   vhdl-font-lock-enumvalue-face
	   vhdl-font-lock-directive-face))
    (set (make-local-variable 'ps-italic-faces)
	 '(font-lock-comment-face
	   font-lock-function-name-face
	   font-lock-type-face
	   vhdl-font-lock-attribute-face
	   vhdl-font-lock-enumvalue-face
	   vhdl-font-lock-directive-face))
    (set (make-local-variable 'ps-underlined-faces)
	 '(font-lock-string-face))
    (setq ps-always-build-face-reference t))
  ;; define page settings, so that a line containing 79 characters (default)
  ;; fits into one column
  (when vhdl-print-two-column
    (set (make-local-variable 'ps-landscape-mode) t)
    (set (make-local-variable 'ps-number-of-columns) 2)
    (set (make-local-variable 'ps-font-size) 7.0)
    (set (make-local-variable 'ps-header-title-font-size) 10.0)
    (set (make-local-variable 'ps-header-font-size) 9.0)
    (set (make-local-variable 'ps-header-offset) 12.0)
    (when (eq ps-paper-type 'letter)
      (set (make-local-variable 'ps-inter-column) 40.0)
      (set (make-local-variable 'ps-left-margin) 40.0)
      (set (make-local-variable 'ps-right-margin) 40.0))))

(defun vhdl-ps-print-init ()
  "Initialize PostScript printing."
  (if (featurep 'xemacs)
      (when (boundp 'ps-print-color-p)
	(vhdl-ps-print-settings))
    (if (featurep 'xemacs) (make-local-hook 'ps-print-hook))
    (add-hook 'ps-print-hook 'vhdl-ps-print-settings nil t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hierarchy browser (using `speedbar.el')
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Allows displaying the hierarchy of all VHDL design units contained in a
;; directory by using the speedbar.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables

(defvar vhdl-entity-alist nil
  "Cache with entities and corresponding architectures for each
project/directory.")
;; structure: (parenthesized expression means list of such entries)
;; (cache-key
;;   (ent-key ent-name ent-file ent-line
;;     (arch-key arch-name arch-file arch-line
;;       (inst-key inst-name inst-file inst-line inst-comp-name inst-ent-key
;;		   inst-arch-key inst-conf-key inst-lib-key inst-path)
;;	 (lib-name pack-key))
;;     mra-key (lib-name pack-key))

(defvar vhdl-config-alist nil
  "Cache with configurations for each project/directory.")
;; structure: (parenthesized expression means list of such entries)
;; (cache-key
;;   (conf-key conf-name conf-file conf-line ent-key arch-key
;;     (inst-key inst-comp-name inst-ent-key inst-arch-key
;;		 inst-conf-key inst-lib-key)
;;     (lib-name pack-key)))

(defvar vhdl-package-alist nil
  "Cache with packages for each project/directory.")
;; structure: (parenthesized expression means list of such entries)
;; (cache-key
;;   (pack-key pack-name pack-file pack-line
;;     (comp-key comp-name comp-file comp-line)
;;     (func-key func-name func-file func-line)
;;     (lib-name pack-key)
;;     pack-body-file pack-body-line
;;     (func-key func-name func-body-file func-body-line)
;;     (lib-name pack-key)))

(defvar vhdl-ent-inst-alist nil
  "Cache with instantiated entities for each project/directory.")
;; structure: (parenthesized expression means list of such entries)
;; (cache-key (inst-ent-key))

(defvar vhdl-file-alist nil
  "Cache with design units in each file for each project/directory.")
;; structure: (parenthesized expression means list of such entries)
;; (cache-key
;;   (file-name (ent-list) (arch-list) (arch-ent-list) (conf-list)
;;		(pack-list) (pack-body-list) (inst-list) (inst-ent-list))

(defvar vhdl-directory-alist nil
  "Cache with source directories for each project.")
;; structure: (parenthesized expression means list of such entries)
;; (cache-key (directory))

(defvar vhdl-speedbar-shown-unit-alist nil
  "Alist of design units simultaneously open in the current speedbar for each
directory and project.")

(defvar vhdl-speedbar-shown-project-list nil
  "List of projects simultaneously open in the current speedbar.")

(defvar vhdl-updated-project-list nil
  "List of projects and directories with updated files.")

(defvar vhdl-modified-file-list nil
  "List of modified files to be rescanned for hierarchy updating.")

(defvar vhdl-speedbar-hierarchy-depth 0
  "Depth of instantiation hierarchy to display.")

(defvar vhdl-speedbar-show-projects nil
  "Non-nil means project hierarchy is displayed in speedbar, directory
hierarchy otherwise.")

(defun vhdl-get-end-of-unit ()
  "Return position of end of current unit."
  (let ((pos (point)))
    (save-excursion
      (while (and (re-search-forward "^[ \t]*\\(architecture\\|configuration\\|entity\\|package\\)\\>" nil 1)
		  (save-excursion
		    (goto-char (match-beginning 0))
		    (vhdl-backward-syntactic-ws)
		    (and (/= (preceding-char) ?\;) (not (bobp))))))
      (re-search-backward "^[ \t]*end\\>" pos 1)
      (point))))

(defun vhdl-match-string-downcase (num &optional string)
  "Like `match-string-no-properties' with down-casing."
  (let ((match (match-string-no-properties num string)))
    (and match (downcase match))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scan functions

(defun vhdl-scan-context-clause ()
  "Scan the context clause that precedes a design unit."
  (let (lib-alist)
    (save-excursion
      (when (re-search-backward "^[ \t]*\\(architecture\\|configuration\\|entity\\|package\\)\\>" nil t)
	(while (and (re-search-backward "^[ \t]*\\(end\\|use\\)\\>" nil t)
		    (equal "USE" (upcase (match-string 1))))
	  (when (looking-at "^[ \t]*use[ \t\n]*\\(\\w+\\)\\.\\(\\w+\\)\\.\\w+")
	    (setq lib-alist (cons (cons (match-string-no-properties 1)
					(vhdl-match-string-downcase 2))
				  lib-alist))))))
    lib-alist))

(defun vhdl-scan-directory-contents (name &optional project update num-string
					  non-final)
  "Scan contents of VHDL files in directory or file pattern NAME."
  (string-match "\\(.*[/\\]\\)\\(.*\\)" name)
;   (unless (file-directory-p (match-string 1 name))
;     (message "No such directory: \"%s\"" (match-string 1 name)))
  (let* ((dir-name (match-string 1 name))
	 (file-pattern (match-string 2 name))
	 (is-directory (= 0 (length file-pattern)))
	 (file-list
	  (if update
	      (list name)
	    (if is-directory
		(vhdl-get-source-files t dir-name)
	      (vhdl-directory-files
	       dir-name t (wildcard-to-regexp file-pattern)))))
	 (key (or project dir-name))
	 (file-exclude-regexp
	  (or (nth 3 (aget vhdl-project-alist project)) ""))
	 (limit-design-file-size (nth 0 vhdl-speedbar-scan-limit))
	 (limit-hier-file-size (nth 0 (nth 1 vhdl-speedbar-scan-limit)))
	 (limit-hier-inst-no (nth 1 (nth 1 vhdl-speedbar-scan-limit)))
	 ent-alist conf-alist pack-alist ent-inst-list file-alist
	 tmp-list tmp-entry no-files files-exist big-files)
    (when (or project update)
      (setq ent-alist (aget vhdl-entity-alist key t)
	    conf-alist (aget vhdl-config-alist key t)
	    pack-alist (aget vhdl-package-alist key t)
	    ent-inst-list (car (aget vhdl-ent-inst-alist key t))
	    file-alist (aget vhdl-file-alist key t)))
    (when (and (not is-directory) (null file-list))
      (message "No such file: \"%s\"" name))
    (setq files-exist file-list)
    (when file-list
      (setq no-files (length file-list))
      (message "Scanning %s %s\"%s\"..."
	       (if is-directory "directory" "files") (or num-string "") name)
      ;; exclude files
      (unless (equal file-exclude-regexp "")
	(let ((case-fold-search nil)
	      file-tmp-list)
	  (while file-list
	    (unless (string-match file-exclude-regexp (car file-list))
	      (setq file-tmp-list (cons (car file-list) file-tmp-list)))
	    (setq file-list (cdr file-list)))
	  (setq file-list (nreverse file-tmp-list))))
      ;; do for all files
      (while file-list
	(unless noninteractive
	  (message "Scanning %s %s\"%s\"... (%2d%s)"
		   (if is-directory "directory" "files")
		   (or num-string "") name
		   (/ (* 100 (- no-files (length file-list))) no-files) "%"))
	(let ((file-name (abbreviate-file-name (car file-list)))
	      ent-list arch-list arch-ent-list conf-list
	      pack-list pack-body-list inst-list inst-ent-list)
	  ;; scan file
	  (vhdl-visit-file
	   file-name nil
	   (vhdl-prepare-search-2
	    (save-excursion
	      ;; scan for design units
	      (if (and limit-design-file-size
		       (< limit-design-file-size (buffer-size)))
		  (progn (message "WARNING:  Scan limit (design units: file size) reached in file:\n  \"%s\"" file-name)
			 (setq big-files t))
		;; scan for entities
		(goto-char (point-min))
		(while (re-search-forward "^[ \t]*entity[ \t\n]+\\(\\w+\\)[ \t\n]+is\\>" nil t)
		  (let* ((ent-name (match-string-no-properties 1))
			 (ent-key (downcase ent-name))
			 (ent-entry (aget ent-alist ent-key t))
			 (lib-alist (vhdl-scan-context-clause)))
		    (if (nth 1 ent-entry)
			(vhdl-warning-when-idle
			 "Entity declared twice (used 1.): \"%s\"\n  1. in \"%s\" (line %d)\n  2. in \"%s\" (line %d)"
			 ent-name  (nth 1 ent-entry) (nth 2 ent-entry)
			 file-name (vhdl-current-line))
		      (setq ent-list (cons ent-key ent-list))
		      (aput 'ent-alist ent-key
			    (list ent-name file-name (vhdl-current-line)
				  (nth 3 ent-entry) (nth 4 ent-entry)
				  lib-alist)))))
		;; scan for architectures
		(goto-char (point-min))
		(while (re-search-forward "^[ \t]*architecture[ \t\n]+\\(\\w+\\)[ \t\n]+of[ \t\n]+\\(\\w+\\)[ \t\n]+is\\>" nil t)
		  (let* ((arch-name (match-string-no-properties 1))
			 (arch-key (downcase arch-name))
			 (ent-name (match-string-no-properties 2))
			 (ent-key (downcase ent-name))
			 (ent-entry (aget ent-alist ent-key t))
			 (arch-alist (nth 3 ent-entry))
			 (arch-entry (aget arch-alist arch-key t))
			 (lib-arch-alist (vhdl-scan-context-clause)))
		    (if arch-entry
			(vhdl-warning-when-idle
			 "Architecture declared twice (used 1.): \"%s\" of \"%s\"\n  1. in \"%s\" (line %d)\n  2. in \"%s\" (line %d)"
			 arch-name ent-name (nth 1 arch-entry)
			 (nth 2 arch-entry) file-name (vhdl-current-line))
		      (setq arch-list (cons arch-key arch-list)
			    arch-ent-list (cons ent-key arch-ent-list))
		      (aput 'arch-alist arch-key
			    (list arch-name file-name (vhdl-current-line) nil
				  lib-arch-alist))
		      (aput 'ent-alist ent-key
			    (list (or (nth 0 ent-entry) ent-name)
				  (nth 1 ent-entry) (nth 2 ent-entry)
				  (vhdl-sort-alist arch-alist)
				  arch-key (nth 5 ent-entry))))))
		;; scan for configurations
		(goto-char (point-min))
		(while (re-search-forward "^[ \t]*configuration[ \t\n]+\\(\\w+\\)[ \t\n]+of[ \t\n]+\\(\\w+\\)[ \t\n]+is\\>" nil t)
		  (let* ((conf-name (match-string-no-properties 1))
			 (conf-key (downcase conf-name))
			 (conf-entry (aget conf-alist conf-key t))
			 (ent-name (match-string-no-properties 2))
			 (ent-key (downcase ent-name))
			 (lib-alist (vhdl-scan-context-clause))
			 (conf-line (vhdl-current-line))
			 (end-of-unit (vhdl-get-end-of-unit))
			 arch-key comp-conf-list inst-key-list
			 inst-comp-key inst-ent-key inst-arch-key
			 inst-conf-key inst-lib-key)
		    (when (vhdl-re-search-forward "\\<for[ \t\n]+\\(\\w+\\)")
		      (setq arch-key (vhdl-match-string-downcase 1)))
		    (if conf-entry
			(vhdl-warning-when-idle
			 "Configuration declared twice (used 1.): \"%s\" of \"%s\"\n  1. in \"%s\" (line %d)\n  2. in \"%s\" (line %d)"
			 conf-name ent-name (nth 1 conf-entry)
			 (nth 2 conf-entry) file-name conf-line)
		      (setq conf-list (cons conf-key conf-list))
		      ;; scan for subconfigurations and subentities
		      (while (re-search-forward "^[ \t]*for[ \t\n]+\\(\\w+\\([ \t\n]*,[ \t\n]*\\w+\\)*\\)[ \t\n]*:[ \t\n]*\\(\\w+\\)[ \t\n]+" end-of-unit t)
			(setq inst-comp-key (vhdl-match-string-downcase 3)
			      inst-key-list (split-string
					     (vhdl-match-string-downcase 1)
					     "[ \t\n]*,[ \t\n]*"))
			(vhdl-forward-syntactic-ws)
			(when (looking-at "use[ \t\n]+\\(\\(entity\\)\\|configuration\\)[ \t\n]+\\(\\w+\\)\\.\\(\\w+\\)[ \t\n]*\\((\\(\\w+\\))\\)?")
			  (setq
			   inst-lib-key (vhdl-match-string-downcase 3)
			   inst-ent-key (and (match-string 2)
					     (vhdl-match-string-downcase 4))
			   inst-arch-key (and (match-string 2)
					      (vhdl-match-string-downcase 6))
			   inst-conf-key (and (not (match-string 2))
					      (vhdl-match-string-downcase 4)))
			  (while inst-key-list
			    (setq comp-conf-list
				  (cons (list (car inst-key-list)
					      inst-comp-key inst-ent-key
					      inst-arch-key inst-conf-key
					      inst-lib-key)
					comp-conf-list))
			    (setq inst-key-list (cdr inst-key-list)))))
		      (aput 'conf-alist conf-key
			    (list conf-name file-name conf-line ent-key
				  arch-key comp-conf-list lib-alist)))))
		;; scan for packages
		(goto-char (point-min))
		(while (re-search-forward "^[ \t]*package[ \t\n]+\\(body[ \t\n]+\\)?\\(\\w+\\)[ \t\n]+is\\>" nil t)
		  (let* ((pack-name (match-string-no-properties 2))
			 (pack-key (downcase pack-name))
			 (is-body (match-string-no-properties 1))
			 (pack-entry (aget pack-alist pack-key t))
			 (pack-line (vhdl-current-line))
			 (end-of-unit (vhdl-get-end-of-unit))
			 comp-name func-name comp-alist func-alist lib-alist)
		    (if (if is-body (nth 6 pack-entry) (nth 1 pack-entry))
			(vhdl-warning-when-idle
			 "Package%s declared twice (used 1.): \"%s\"\n  1. in \"%s\" (line %d)\n  2. in \"%s\" (line %d)"
			 (if is-body " body" "") pack-name
			 (if is-body (nth 6 pack-entry) (nth 1 pack-entry))
			 (if is-body (nth 7 pack-entry) (nth 2 pack-entry))
			 file-name (vhdl-current-line))
		      ;; scan for context clauses
		      (setq lib-alist (vhdl-scan-context-clause))
		      ;; scan for component and subprogram declarations/bodies
		      (while (re-search-forward "^[ \t]*\\(component\\|function\\|procedure\\)[ \t\n]+\\(\\w+\\|\".*\"\\)" end-of-unit t)
			(if (equal (upcase (match-string 1)) "COMPONENT")
			    (setq comp-name (match-string-no-properties 2)
				  comp-alist
				  (cons (list (downcase comp-name) comp-name
					      file-name (vhdl-current-line))
					comp-alist))
			  (setq func-name (match-string-no-properties 2)
				func-alist
				(cons (list (downcase func-name) func-name
					    file-name (vhdl-current-line))
				      func-alist))))
		      (setq func-alist (nreverse func-alist))
		      (setq comp-alist (nreverse comp-alist))
		      (if is-body
			  (setq pack-body-list (cons pack-key pack-body-list))
			(setq pack-list (cons pack-key pack-list)))
		      (aput
		       'pack-alist pack-key
		       (if is-body
			   (list (or (nth 0 pack-entry) pack-name)
				 (nth 1 pack-entry) (nth 2 pack-entry)
				 (nth 3 pack-entry) (nth 4 pack-entry)
				 (nth 5 pack-entry)
				 file-name pack-line func-alist lib-alist)
			 (list pack-name file-name pack-line
			       comp-alist func-alist lib-alist
			       (nth 6 pack-entry) (nth 7 pack-entry)
			       (nth 8 pack-entry) (nth 9 pack-entry))))))))
	      ;; scan for hierarchy
	      (if (and limit-hier-file-size
		       (< limit-hier-file-size (buffer-size)))
		  (progn (message "WARNING:  Scan limit (hierarchy: file size) reached in file:\n  \"%s\"" file-name)
			 (setq big-files t))
		;; scan for architectures
		(goto-char (point-min))
		(while (re-search-forward "^[ \t]*architecture[ \t\n]+\\(\\w+\\)[ \t\n]+of[ \t\n]+\\(\\w+\\)[ \t\n]+is\\>" nil t)
		  (let* ((ent-name (match-string-no-properties 2))
			 (ent-key (downcase ent-name))
			 (arch-name (match-string-no-properties 1))
			 (arch-key (downcase arch-name))
			 (ent-entry (aget ent-alist ent-key t))
			 (arch-alist (nth 3 ent-entry))
			 (arch-entry (aget arch-alist arch-key t))
			 (beg-of-unit (point))
			 (end-of-unit (vhdl-get-end-of-unit))
			 (inst-no 0)
			 inst-alist inst-path)
		    ;; scan for contained instantiations
		    (while (and (re-search-forward
				 (concat "^[ \t]*\\(\\w+\\)[ \t\n]*:[ \t\n]*\\("
					 "\\(\\w+\\)[ \t\n]+\\(--[^\n]*\n[ \t\n]*\\)*\\(generic\\|port\\)[ \t\n]+map\\>\\|"
					 "component[ \t\n]+\\(\\w+\\)\\|"
					 "\\(\\(entity\\)\\|configuration\\)[ \t\n]+\\(\\(\\w+\\)\\.\\)?\\(\\w+\\)\\([ \t\n]*(\\(\\w+\\))\\)?\\|"
					 "\\(\\(for\\|if\\)\\>[^;:]+\\<generate\\>\\|block\\>\\)\\)\\|"
					 "\\(^[ \t]*end[ \t\n]+\\(generate\\|block\\)\\>\\)") end-of-unit t)
				(or (not limit-hier-inst-no)
				    (<= (setq inst-no (1+ inst-no))
					limit-hier-inst-no)))
		      (cond
		       ;; block/generate beginning found
		       ((match-string 14)
			(setq inst-path
			      (cons (match-string-no-properties 1) inst-path)))
		       ;; block/generate end found
		       ((match-string 16)
			(setq inst-path (cdr inst-path)))
		       ;; instantiation found
		       (t
			(let* ((inst-name (match-string-no-properties 1))
			       (inst-key (downcase inst-name))
			       (inst-comp-name
				(or (match-string-no-properties 3)
				    (match-string-no-properties 6)))
			       (inst-ent-key
				(or (and (match-string 8)
					 (vhdl-match-string-downcase 11))
				    (and inst-comp-name
					 (downcase inst-comp-name))))
			       (inst-arch-key (vhdl-match-string-downcase 13))
			       (inst-conf-key
				(and (not (match-string 8))
				     (vhdl-match-string-downcase 11)))
			       (inst-lib-key (vhdl-match-string-downcase 10)))
			  (goto-char (match-end 1))
			  (setq inst-list (cons inst-key inst-list)
				inst-ent-list
				(cons inst-ent-key inst-ent-list))
			  (setq inst-alist
				(append
				 inst-alist
				 (list (list inst-key inst-name file-name
					     (vhdl-current-line) inst-comp-name
					     inst-ent-key inst-arch-key
					     inst-conf-key inst-lib-key
					     (reverse inst-path)))))))))
		    ;; scan for contained configuration specifications
		    (goto-char beg-of-unit)
		    (while (re-search-forward
			    (concat "^[ \t]*for[ \t\n]+\\(\\w+\\([ \t\n]*,[ \t\n]*\\w+\\)*\\)[ \t\n]*:[ \t\n]*\\(\\w+\\)[ \t\n]+\\(--[^\n]*\n[ \t\n]*\\)*"
				    "use[ \t\n]+\\(\\(entity\\)\\|configuration\\)[ \t\n]+\\(\\(\\w+\\)\\.\\)?\\(\\w+\\)\\([ \t\n]*(\\(\\w+\\))\\)?") end-of-unit t)
		      (let* ((inst-comp-name (match-string-no-properties 3))
			     (inst-ent-key
			      (and (match-string 6)
				   (vhdl-match-string-downcase 9)))
			     (inst-arch-key (vhdl-match-string-downcase 11))
			     (inst-conf-key
			      (and (not (match-string 6))
				   (vhdl-match-string-downcase 9)))
			     (inst-lib-key (vhdl-match-string-downcase 8))
			     (inst-key-list
			      (split-string (vhdl-match-string-downcase 1)
					    "[ \t\n]*,[ \t\n]*"))
			     (tmp-inst-alist inst-alist)
			     inst-entry)
			(while tmp-inst-alist
			  (when (and (or (equal "all" (car inst-key-list))
					 (member (nth 0 (car tmp-inst-alist))
						 inst-key-list))
				     (equal
				      (downcase
				       (or (nth 4 (car tmp-inst-alist)) ""))
				      (downcase inst-comp-name)))
			    (setq inst-entry (car tmp-inst-alist))
			    (setq inst-ent-list
				  (cons (or inst-ent-key (nth 5 inst-entry))
					(vhdl-delete
					 (nth 5 inst-entry) inst-ent-list)))
			    (setq inst-entry
				  (list (nth 0 inst-entry) (nth 1 inst-entry)
					(nth 2 inst-entry) (nth 3 inst-entry)
					(nth 4 inst-entry)
					(or inst-ent-key (nth 5 inst-entry))
					(or inst-arch-key (nth 6 inst-entry))
					inst-conf-key inst-lib-key))
			    (setcar tmp-inst-alist inst-entry))
			  (setq tmp-inst-alist (cdr tmp-inst-alist)))))
		    ;; save in cache
		    (aput 'arch-alist arch-key
			  (list (nth 0 arch-entry) (nth 1 arch-entry)
				(nth 2 arch-entry) inst-alist
				(nth 4 arch-entry)))
		    (aput 'ent-alist ent-key
			  (list (nth 0 ent-entry) (nth 1 ent-entry)
				(nth 2 ent-entry) (vhdl-sort-alist arch-alist)
				(nth 4 ent-entry) (nth 5 ent-entry)))
		    (when (and limit-hier-inst-no
			       (> inst-no limit-hier-inst-no))
		      (message "WARNING:  Scan limit (hierarchy: instances per architecture) reached in file:\n  \"%s\"" file-name)
		      (setq big-files t))
		    (goto-char end-of-unit))))
	      ;; remember design units for this file
	      (aput 'file-alist file-name
		    (list ent-list arch-list arch-ent-list conf-list
			  pack-list pack-body-list inst-list inst-ent-list))
	      (setq ent-inst-list (append inst-ent-list ent-inst-list))))))
	(setq file-list (cdr file-list))))
    (when (or (and (not project) files-exist)
	      (and project (not non-final)))
      ;; consistency checks:
      ;; check whether each architecture has a corresponding entity
      (setq tmp-list ent-alist)
      (while tmp-list
	(when (null (nth 2 (car tmp-list)))
	  (setq tmp-entry (car (nth 4 (car tmp-list))))
	  (vhdl-warning-when-idle
	   "Architecture of non-existing entity: \"%s\" of \"%s\"\n  in \"%s\" (line %d)"
	   (nth 1 tmp-entry) (nth 1 (car tmp-list)) (nth 2 tmp-entry)
	   (nth 3 tmp-entry)))
	(setq tmp-list (cdr tmp-list)))
      ;; check whether configuration has a corresponding entity/architecture
      (setq tmp-list conf-alist)
      (while tmp-list
	(if (setq tmp-entry (aget ent-alist (nth 4 (car tmp-list)) t))
	    (unless (aget (nth 3 tmp-entry) (nth 5 (car tmp-list)) t)
	      (setq tmp-entry (car tmp-list))
	      (vhdl-warning-when-idle
	       "Configuration of non-existing architecture: \"%s\" of \"%s(%s)\"\n  in \"%s\" (line %d)"
	       (nth 1 tmp-entry) (nth 4 tmp-entry) (nth 5 tmp-entry)
	       (nth 2 tmp-entry) (nth 3 tmp-entry)))
	  (setq tmp-entry (car tmp-list))
	  (vhdl-warning-when-idle
	   "Configuration of non-existing entity: \"%s\" of \"%s\"\n  in \"%s\" (line %d)"
	   (nth 1 tmp-entry) (nth 4 tmp-entry)
	   (nth 2 tmp-entry) (nth 3 tmp-entry)))
	(setq tmp-list (cdr tmp-list)))
      ;; check whether each package body has a package declaration
      (setq tmp-list pack-alist)
      (while tmp-list
	(when (null (nth 2 (car tmp-list)))
	  (setq tmp-entry (car tmp-list))
	  (vhdl-warning-when-idle
	   "Package body of non-existing package: \"%s\"\n  in \"%s\" (line %d)"
	   (nth 1 tmp-entry) (nth 7 tmp-entry) (nth 8 tmp-entry)))
	(setq tmp-list (cdr tmp-list)))
      ;; sort lists
      (setq ent-alist (vhdl-sort-alist ent-alist))
      (setq conf-alist (vhdl-sort-alist conf-alist))
      (setq pack-alist (vhdl-sort-alist pack-alist))
      ;; remember updated directory/project
      (add-to-list 'vhdl-updated-project-list (or project dir-name)))
    ;; clear directory alists
    (unless project
      (adelete 'vhdl-entity-alist key)
      (adelete 'vhdl-config-alist key)
      (adelete 'vhdl-package-alist key)
      (adelete 'vhdl-ent-inst-alist key)
      (adelete 'vhdl-file-alist key))
    ;; put directory contents into cache
    (aput 'vhdl-entity-alist key ent-alist)
    (aput 'vhdl-config-alist key conf-alist)
    (aput 'vhdl-package-alist key pack-alist)
    (aput 'vhdl-ent-inst-alist key (list ent-inst-list))
    (aput 'vhdl-file-alist key file-alist)
    ;; final messages
    (message "Scanning %s %s\"%s\"...done"
	     (if is-directory "directory" "files") (or num-string "") name)
    (unless project (message "Scanning directory...done"))
    (when big-files
      (vhdl-warning-when-idle "Scanning is incomplete.\n  --> see user option `vhdl-speedbar-scan-limit'"))
    ;; save cache when scanned non-interactively
    (when (or (not project) (not non-final))
      (when (and noninteractive vhdl-speedbar-save-cache)
	(vhdl-save-cache key)))
    t))

(defun vhdl-scan-project-contents (project)
  "Scan the contents of all VHDL files found in the directories and files
of PROJECT."
  (let ((dir-list (or (nth 2 (aget vhdl-project-alist project)) '("")))
	(default-dir (vhdl-resolve-env-variable
		      (nth 1 (aget vhdl-project-alist project))))
	(file-exclude-regexp
	  (or (nth 3 (aget vhdl-project-alist project)) ""))
	dir-list-tmp dir dir-name num-dir act-dir recursive)
    ;; clear project alists
    (adelete 'vhdl-entity-alist project)
    (adelete 'vhdl-config-alist project)
    (adelete 'vhdl-package-alist project)
    (adelete 'vhdl-ent-inst-alist project)
    (adelete 'vhdl-file-alist project)
    ;; expand directory names by default-directory
    (message "Collecting source files...")
    (while dir-list
      (setq dir (vhdl-resolve-env-variable (car dir-list)))
      (string-match "\\(\\(-r \\)?\\)\\(.*\\)" dir)
      (setq recursive (match-string 1 dir)
	    dir-name (match-string 3 dir))
      (setq dir-list-tmp
	    (cons (concat recursive
			  (if (file-name-absolute-p dir-name) "" default-dir)
			  dir-name)
		  dir-list-tmp))
      (setq dir-list (cdr dir-list)))
    ;; resolve path wildcards
    (setq dir-list-tmp (vhdl-resolve-paths dir-list-tmp))
    ;; expand directories
    (while dir-list-tmp
      (setq dir (car dir-list-tmp))
      ;; get subdirectories
      (if (string-match "-r \\(.*[/\\]\\)" dir)
	  (setq dir-list (append dir-list (vhdl-get-subdirs
					   (match-string 1 dir))))
	(setq dir-list (append dir-list (list dir))))
      (setq dir-list-tmp (cdr dir-list-tmp)))
    ;; exclude files
    (unless (equal file-exclude-regexp "")
      (let ((case-fold-search nil))
	(while dir-list
	  (unless (string-match file-exclude-regexp (car dir-list))
	    (setq dir-list-tmp (cons (car dir-list) dir-list-tmp)))
	  (setq dir-list (cdr dir-list)))
	(setq dir-list (nreverse dir-list-tmp))))
    (message "Collecting source files...done")
    ;; scan for design units for each directory in DIR-LIST
    (setq dir-list-tmp nil
	  num-dir (length dir-list)
	  act-dir 1)
    (while dir-list
      (setq dir-name (abbreviate-file-name
		      (expand-file-name (car dir-list))))
      (vhdl-scan-directory-contents dir-name project nil
				    (format "(%s/%s) " act-dir num-dir)
				    (cdr dir-list))
      (add-to-list 'dir-list-tmp (file-name-directory dir-name))
      (setq dir-list (cdr dir-list)
	    act-dir (1+ act-dir)))
    (aput 'vhdl-directory-alist project (list (nreverse dir-list-tmp)))
    (message "Scanning project \"%s\"...done" project)))

(defun vhdl-update-file-contents (file-name)
  "Update hierarchy information by contents of current buffer."
  (setq file-name (abbreviate-file-name file-name))
  (let* ((dir-name (file-name-directory file-name))
	 (directory-alist vhdl-directory-alist)
	 updated)
    (while directory-alist
      (when (member dir-name (nth 1 (car directory-alist)))
	(let* ((vhdl-project (nth 0 (car directory-alist)))
	       (project (vhdl-project-p))
	       (ent-alist (aget vhdl-entity-alist (or project dir-name) t))
	       (conf-alist (aget vhdl-config-alist (or project dir-name) t))
	       (pack-alist (aget vhdl-package-alist (or project dir-name) t))
	       (ent-inst-list (car (aget vhdl-ent-inst-alist
					 (or project dir-name) t)))
	       (file-alist (aget vhdl-file-alist (or project dir-name) t))
	       (file-entry (aget file-alist file-name t))
	       (ent-list (nth 0 file-entry))
	       (arch-list (nth 1 file-entry))
	       (arch-ent-list (nth 2 file-entry))
	       (conf-list (nth 3 file-entry))
	       (pack-list (nth 4 file-entry))
	       (pack-body-list (nth 5 file-entry))
	       (inst-ent-list (nth 7 file-entry))
	       (cache-key (or project dir-name))
	       arch-alist key ent-key entry)
	  ;; delete design units previously contained in this file:
	  ;; entities
	  (while ent-list
	    (setq key (car ent-list)
		  entry (aget ent-alist key t))
	    (when (equal file-name (nth 1 entry))
	      (if (nth 3 entry)
		  (aput 'ent-alist key
			(list (nth 0 entry) nil nil (nth 3 entry) nil))
		(adelete 'ent-alist key)))
	    (setq ent-list (cdr ent-list)))
	  ;; architectures
	  (while arch-list
	    (setq key (car arch-list)
		  ent-key (car arch-ent-list)
		  entry (aget ent-alist ent-key t)
		  arch-alist (nth 3 entry))
	    (when (equal file-name (nth 1 (aget arch-alist key t)))
	      (adelete 'arch-alist key)
	      (if (or (nth 1 entry) arch-alist)
		  (aput 'ent-alist ent-key
			(list (nth 0 entry) (nth 1 entry) (nth 2 entry)
			      arch-alist (nth 4 entry) (nth 5 entry)))
		(adelete 'ent-alist ent-key)))
	    (setq arch-list (cdr arch-list)
		  arch-ent-list (cdr arch-ent-list)))
	  ;; configurations
	  (while conf-list
	    (setq key (car conf-list))
	    (when (equal file-name (nth 1 (aget conf-alist key t)))
	      (adelete 'conf-alist key))
	    (setq conf-list (cdr conf-list)))
	  ;; package declarations
	  (while pack-list
	    (setq key (car pack-list)
		  entry (aget pack-alist key t))
	    (when (equal file-name (nth 1 entry))
	      (if (nth 6 entry)
		  (aput 'pack-alist key
			(list (nth 0 entry) nil nil nil nil nil
			      (nth 6 entry) (nth 7 entry) (nth 8 entry)
			      (nth 9 entry)))
		(adelete 'pack-alist key)))
	    (setq pack-list (cdr pack-list)))
	  ;; package bodies
	  (while pack-body-list
	    (setq key (car pack-body-list)
		  entry (aget pack-alist key t))
	    (when (equal file-name (nth 6 entry))
	      (if (nth 1 entry)
		  (aput 'pack-alist key
			(list (nth 0 entry) (nth 1 entry) (nth 2 entry)
			      (nth 3 entry) (nth 4 entry) (nth 5 entry)
			      nil nil nil nil))
		(adelete 'pack-alist key)))
	    (setq pack-body-list (cdr pack-body-list)))
	  ;; instantiated entities
	  (while inst-ent-list
	    (setq ent-inst-list
		  (vhdl-delete (car inst-ent-list) ent-inst-list))
	    (setq inst-ent-list (cdr inst-ent-list)))
	  ;; update caches
	  (vhdl-aput 'vhdl-entity-alist cache-key ent-alist)
	  (vhdl-aput 'vhdl-config-alist cache-key conf-alist)
	  (vhdl-aput 'vhdl-package-alist cache-key pack-alist)
	  (vhdl-aput 'vhdl-ent-inst-alist cache-key (list ent-inst-list))
	  ;; scan file
	  (vhdl-scan-directory-contents file-name project t)
	  (when (or (and vhdl-speedbar-show-projects project)
		    (and (not vhdl-speedbar-show-projects) (not project)))
	    (vhdl-speedbar-refresh project))
	  (setq updated t)))
      (setq directory-alist (cdr directory-alist)))
    updated))

(defun vhdl-update-hierarchy ()
  "Update directory and hierarchy information in speedbar."
  (let ((file-list (reverse vhdl-modified-file-list))
	updated)
    (when (and vhdl-speedbar-update-on-saving file-list)
      (while file-list
	(setq updated
	      (or (vhdl-update-file-contents (car file-list))
		  updated))
	(setq file-list (cdr file-list)))
      (setq vhdl-modified-file-list nil)
      (vhdl-speedbar-update-current-unit)
      (when updated (message "Updating hierarchy...done")))))

;; structure (parenthesized expression means list of such entries)
;; (inst-key inst-file-marker comp-ent-key comp-ent-file-marker
;;  comp-arch-key comp-arch-file-marker comp-conf-key comp-conf-file-marker
;;  comp-lib-name level)
(defun vhdl-get-hierarchy (ent-alist conf-alist ent-key arch-key conf-key
				     conf-inst-alist level indent
				     &optional include-top ent-hier)
  "Get instantiation hierarchy beginning in architecture ARCH-KEY of
entity ENT-KEY."
  (let* ((ent-entry (aget ent-alist ent-key t))
	 (arch-entry (if arch-key (aget (nth 3 ent-entry) arch-key t)
		       (cdar (last (nth 3 ent-entry)))))
	 (inst-alist (nth 3 arch-entry))
	 inst-entry inst-ent-entry inst-arch-entry inst-conf-entry comp-entry
	 hier-list subcomp-list tmp-list inst-key inst-comp-name
	 inst-ent-key inst-arch-key inst-conf-key inst-lib-key)
    (when (= level 0) (message "Extract design hierarchy..."))
    (when include-top
      (setq level (1+ level)))
    (when (member ent-key ent-hier)
      (error "ERROR:  Instantiation loop detected, component instantiates itself: \"%s\"" ent-key))
    ;; check configured architecture (already checked during scanning)
;     (unless (or (null conf-inst-alist) (assoc arch-key (nth 3 ent-entry)))
;       (vhdl-warning-when-idle "Configuration for non-existing architecture used: \"%s\"" conf-key))
    ;; process all instances
    (while inst-alist
      (setq inst-entry (car inst-alist)
	    inst-key (nth 0 inst-entry)
	    inst-comp-name (nth 4 inst-entry)
	    inst-conf-key (nth 7 inst-entry))
      ;; search entry in configuration's instantiations list
      (setq tmp-list conf-inst-alist)
      (while (and tmp-list
		  (not (and (member (nth 0 (car tmp-list))
				    (list "all" inst-key))
			    (equal (nth 1 (car tmp-list))
				   (downcase (or inst-comp-name ""))))))
	(setq tmp-list (cdr tmp-list)))
      (setq inst-conf-key (or (nth 4 (car tmp-list)) inst-conf-key))
      (setq inst-conf-entry (aget conf-alist inst-conf-key t))
      (when (and inst-conf-key (not inst-conf-entry))
	(vhdl-warning-when-idle "Configuration not found: \"%s\"" inst-conf-key))
      ;; determine entity
      (setq inst-ent-key
	    (or (nth 2 (car tmp-list))	; from configuration
		(nth 3 inst-conf-entry) ; from subconfiguration
		(nth 3 (aget conf-alist (nth 7 inst-entry) t))
					; from configuration spec.
		(nth 5 inst-entry)))	; from direct instantiation
      (setq inst-ent-entry (aget ent-alist inst-ent-key t))
      ;; determine architecture
      (setq inst-arch-key
	    (or (nth 3 (car tmp-list))		; from configuration
		(nth 4 inst-conf-entry)		; from subconfiguration
		(nth 6 inst-entry)		; from direct instantiation
		(nth 4 (aget conf-alist (nth 7 inst-entry)))
						; from configuration spec.
		(nth 4 inst-ent-entry)		; MRA
		(caar (nth 3 inst-ent-entry))))	; first alphabetically
      (setq inst-arch-entry (aget (nth 3 inst-ent-entry) inst-arch-key t))
      ;; set library
      (setq inst-lib-key
	    (or (nth 5 (car tmp-list))		; from configuration
		(nth 8 inst-entry)))		; from direct instantiation
      ;; gather information for this instance
      (setq comp-entry
	    (list (nth 1 inst-entry)
		  (cons (nth 2 inst-entry) (nth 3 inst-entry))
		  (or (nth 0 inst-ent-entry) (nth 4 inst-entry))
		  (cons (nth 1 inst-ent-entry) (nth 2 inst-ent-entry))
		  (or (nth 0 inst-arch-entry) inst-arch-key)
		  (cons (nth 1 inst-arch-entry) (nth 2 inst-arch-entry))
		  (or (nth 0 inst-conf-entry) inst-conf-key)
		  (cons (nth 1 inst-conf-entry) (nth 2 inst-conf-entry))
		  inst-lib-key level))
      ;; get subcomponent hierarchy
      (setq subcomp-list (vhdl-get-hierarchy
			  ent-alist conf-alist
			  inst-ent-key inst-arch-key inst-conf-key
			  (nth 5 inst-conf-entry)
			  (1+ level) indent nil (cons ent-key ent-hier)))
      ;; add to list
      (setq hier-list (append hier-list (list comp-entry) subcomp-list))
      (setq inst-alist (cdr inst-alist)))
    (when include-top
      (setq hier-list
	    (cons (list nil nil (nth 0 ent-entry)
			(cons (nth 1 ent-entry) (nth 2 ent-entry))
			(nth 0 arch-entry)
			(cons (nth 1 arch-entry) (nth 2 arch-entry))
			nil nil
			nil (1- level))
		  hier-list)))
    (when (or (= level 0) (and include-top (= level 1))) (message ""))
    hier-list))

(defun vhdl-get-instantiations (ent-key indent)
  "Get all instantiations of entity ENT-KEY."
  (let ((ent-alist (aget vhdl-entity-alist (vhdl-speedbar-line-key indent) t))
	arch-alist inst-alist ent-inst-list
	ent-entry arch-entry inst-entry)
    (while ent-alist
      (setq ent-entry (car ent-alist))
      (setq arch-alist (nth 4 ent-entry))
      (while arch-alist
	(setq arch-entry (car arch-alist))
	(setq inst-alist (nth 4 arch-entry))
	(while inst-alist
	  (setq inst-entry (car inst-alist))
	  (when (equal ent-key (nth 5 inst-entry))
	    (setq ent-inst-list
		  (cons (list (nth 1 inst-entry)
			      (cons (nth 2 inst-entry) (nth 3 inst-entry))
			      (nth 1 ent-entry)
			      (cons (nth 2 ent-entry) (nth 3 ent-entry))
			      (nth 1 arch-entry)
			      (cons (nth 2 arch-entry) (nth 3 arch-entry)))
			ent-inst-list)))
	  (setq inst-alist (cdr inst-alist)))
	(setq arch-alist (cdr arch-alist)))
      (setq ent-alist (cdr ent-alist)))
    (nreverse ent-inst-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Caching in file

(defun vhdl-save-caches ()
  "Save all updated hierarchy caches to file."
  (interactive)
  (condition-case nil
      (when vhdl-speedbar-save-cache
	;; update hierarchy
	(vhdl-update-hierarchy)
	(let ((project-list vhdl-updated-project-list))
	  (message "Saving hierarchy caches...")
	  ;; write updated project caches
	  (while project-list
	    (vhdl-save-cache (car project-list))
	    (setq project-list (cdr project-list)))
	  (message "Saving hierarchy caches...done")))
    (error (progn (vhdl-warning "ERROR:  An error occurred while saving the hierarchy caches")
		  (sit-for 2)))))

(defun vhdl-save-cache (key)
  "Save current hierarchy cache to file."
  (let* ((orig-buffer (current-buffer))
	 (vhdl-project key)
	 (project (vhdl-project-p))
	 (default-directory key)
	 (directory (abbreviate-file-name (vhdl-default-directory)))
	 (file-name (vhdl-resolve-env-variable
		     (vhdl-replace-string
		      (cons "\\(.*\\) \\(.*\\)" vhdl-speedbar-cache-file-name)
		      (concat
		       (subst-char-in-string ?  ?_ (or project "dir"))
		       " " (user-login-name)))))
	 (file-dir-name (expand-file-name file-name directory))
	 (cache-key (or project directory))
	 (key (if project "project" "directory")))
    (unless (file-exists-p (file-name-directory file-dir-name))
      (make-directory (file-name-directory file-dir-name) t))
    (if (not (file-writable-p file-dir-name))
	(progn (vhdl-warning (format "File not writable: \"%s\""
				     (abbreviate-file-name file-dir-name)))
	       (sit-for 2))
      (message "Saving cache: \"%s\"" file-dir-name)
      (set-buffer (find-file-noselect file-dir-name t t))
      (erase-buffer)
      (insert ";; -*- Emacs-Lisp -*-\n\n"
	      ";;; " (file-name-nondirectory file-name)
	      " - design hierarchy cache file for Emacs VHDL Mode "
	      vhdl-version "\n")
      (insert "\n;; " (if project "Project  " "Directory") " : ")
      (if project (insert project) (prin1 directory (current-buffer)))
      (insert "\n;; Saved     : " (format-time-string "%Y-%m-%d %T ")
	      (user-login-name) "\n\n"
	      "\n;; version number\n"
	      "(setq vhdl-cache-version \"" vhdl-version "\")\n"
	      "\n;; " (if project "project" "directory") " name"
	      "\n(setq " key " ")
      (prin1 (or project directory) (current-buffer))
      (insert ")\n")
      (when (member 'hierarchy vhdl-speedbar-save-cache)
	(insert "\n;; entity and architecture cache\n"
		"(aput 'vhdl-entity-alist " key " '")
	(print (aget vhdl-entity-alist cache-key t) (current-buffer))
	(insert ")\n\n;; configuration cache\n"
		"(aput 'vhdl-config-alist " key " '")
	(print (aget vhdl-config-alist cache-key t) (current-buffer))
	(insert ")\n\n;; package cache\n"
		"(aput 'vhdl-package-alist " key " '")
	(print (aget vhdl-package-alist cache-key t) (current-buffer))
	(insert ")\n\n;; instantiated entities cache\n"
		"(aput 'vhdl-ent-inst-alist " key " '")
	(print (aget vhdl-ent-inst-alist cache-key t) (current-buffer))
	(insert ")\n\n;; design units per file cache\n"
		"(aput 'vhdl-file-alist " key " '")
	(print (aget vhdl-file-alist cache-key t) (current-buffer))
	(when project
	  (insert ")\n\n;; source directories in project cache\n"
		  "(aput 'vhdl-directory-alist " key " '")
	  (print (aget vhdl-directory-alist cache-key t) (current-buffer)))
	(insert ")\n"))
      (when (member 'display vhdl-speedbar-save-cache)
	(insert "\n;; shown design units cache\n"
		"(aput 'vhdl-speedbar-shown-unit-alist " key " '")
	(print (aget vhdl-speedbar-shown-unit-alist cache-key t)
	       (current-buffer))
	(insert ")\n"))
      (setq vhdl-updated-project-list
	    (delete cache-key vhdl-updated-project-list))
      (save-buffer)
      (kill-buffer (current-buffer))
      (set-buffer orig-buffer))))

(defun vhdl-load-cache (key)
  "Load hierarchy cache information from file."
  (let* ((vhdl-project key)
	 (default-directory key)
	 (directory (vhdl-default-directory))
	 (file-name (vhdl-resolve-env-variable
		     (vhdl-replace-string
		      (cons "\\(.*\\) \\(.*\\)" vhdl-speedbar-cache-file-name)
		      (concat
		       (subst-char-in-string ?  ?_ (or (vhdl-project-p) "dir"))
		       " " (user-login-name)))))
	 (file-dir-name (expand-file-name file-name directory))
	 vhdl-cache-version)
    (unless (memq 'vhdl-save-caches kill-emacs-hook)
      (add-hook 'kill-emacs-hook 'vhdl-save-caches))
    (when (file-exists-p file-dir-name)
      (condition-case ()
	  (progn (load-file file-dir-name)
		 (string< (mapconcat
			   (lambda (a) (format "%3d" (string-to-number a)))
			   (split-string "3.33" "\\.") "")
			  (mapconcat
			   (lambda (a) (format "%3d" (string-to-number a)))
			   (split-string vhdl-cache-version "\\.") "")))
	(error (progn (vhdl-warning (format "ERROR:  Corrupted cache file: \"%s\"" file-dir-name))
		      nil))))))

(defun vhdl-require-hierarchy-info ()
  "Make sure that hierarchy information is available.  Load cache or scan files
if required."
  (if (vhdl-project-p)
      (unless (or (assoc vhdl-project vhdl-file-alist)
		  (vhdl-load-cache vhdl-project))
	(vhdl-scan-project-contents vhdl-project))
    (let ((directory (abbreviate-file-name default-directory)))
      (unless (or (assoc directory vhdl-file-alist)
		  (vhdl-load-cache directory))
	(vhdl-scan-directory-contents directory)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add hierarchy browser functionality to speedbar

(defvar vhdl-speedbar-key-map nil
  "Keymap used when in the VHDL hierarchy browser mode.")

(defvar vhdl-speedbar-menu-items nil
  "Additional menu-items to add to speedbar frame.")

(defun vhdl-speedbar-initialize ()
  "Initialize speedbar."
  ;; general settings
;  (set (make-local-variable 'speedbar-tag-hierarchy-method) nil)
  ;; VHDL file extensions (extracted from `auto-mode-alist')
  (let ((mode-alist auto-mode-alist))
    (while mode-alist
      (when (eq (cdar mode-alist) 'vhdl-mode)
	(speedbar-add-supported-extension (caar mode-alist)))
      (setq mode-alist (cdr mode-alist))))
  ;; hierarchy browser settings
  (when (boundp 'speedbar-mode-functions-list)
    ;; special functions
    (speedbar-add-mode-functions-list
     '("vhdl directory"
       (speedbar-item-info . vhdl-speedbar-item-info)
       (speedbar-line-directory . speedbar-files-line-path)))
    (speedbar-add-mode-functions-list
     '("vhdl project"
       (speedbar-item-info . vhdl-speedbar-item-info)
       (speedbar-line-directory . vhdl-speedbar-line-project)))
    ;; keymap
    (unless vhdl-speedbar-key-map
      (setq vhdl-speedbar-key-map (speedbar-make-specialized-keymap))
      (define-key vhdl-speedbar-key-map "e" 'speedbar-edit-line)
      (define-key vhdl-speedbar-key-map "\C-m" 'speedbar-edit-line)
      (define-key vhdl-speedbar-key-map "+" 'speedbar-expand-line)
      (define-key vhdl-speedbar-key-map "=" 'speedbar-expand-line)
      (define-key vhdl-speedbar-key-map "-" 'vhdl-speedbar-contract-level)
      (define-key vhdl-speedbar-key-map "_" 'vhdl-speedbar-contract-all)
      (define-key vhdl-speedbar-key-map "C" 'vhdl-speedbar-port-copy)
      (define-key vhdl-speedbar-key-map "P" 'vhdl-speedbar-place-component)
      (define-key vhdl-speedbar-key-map "F" 'vhdl-speedbar-configuration)
      (define-key vhdl-speedbar-key-map "A" 'vhdl-speedbar-select-mra)
      (define-key vhdl-speedbar-key-map "K" 'vhdl-speedbar-make-design)
      (define-key vhdl-speedbar-key-map "R" 'vhdl-speedbar-rescan-hierarchy)
      (define-key vhdl-speedbar-key-map "S" 'vhdl-save-caches)
      (let ((key 0))
	(while (<= key 9)
	  (define-key vhdl-speedbar-key-map (int-to-string key)
	    `(lambda () (interactive) (vhdl-speedbar-set-depth ,key)))
	  (setq key (1+ key)))))
    (define-key speedbar-key-map "h"
      (lambda () (interactive)
	(speedbar-change-initial-expansion-list "vhdl directory")))
    (define-key speedbar-key-map "H"
      (lambda () (interactive)
	(speedbar-change-initial-expansion-list "vhdl project")))
    ;; menu
    (unless vhdl-speedbar-menu-items
      (setq
       vhdl-speedbar-menu-items
       `(["Edit" speedbar-edit-line t]
	 ["Expand" speedbar-expand-line
	  (save-excursion (beginning-of-line) (looking-at "[0-9]+: *.\\+. "))]
	 ["Contract" vhdl-speedbar-contract-level t]
	 ["Expand All" vhdl-speedbar-expand-all t]
	 ["Contract All" vhdl-speedbar-contract-all t]
	 ,(let ((key 0) (menu-list '("Hierarchy Depth")))
	    (while (<= key 9)
	      (setq menu-list
		    (cons `[,(if (= key 0) "All" (int-to-string key))
			    (vhdl-speedbar-set-depth ,key)
			    :style radio
			    :selected (= vhdl-speedbar-hierarchy-depth ,key)
			    :keys ,(int-to-string key)]
			  menu-list))
	      (setq key (1+ key)))
	    (nreverse menu-list))
	 "--"
	 ["Copy Port/Subprogram" vhdl-speedbar-port-copy
	  (or (vhdl-speedbar-check-unit 'entity)
	      (vhdl-speedbar-check-unit 'subprogram))]
	 ["Place Component" vhdl-speedbar-place-component
	  (vhdl-speedbar-check-unit 'entity)]
	 ["Generate Configuration" vhdl-speedbar-configuration
	  (vhdl-speedbar-check-unit 'architecture)]
	 ["Select as MRA" vhdl-speedbar-select-mra
	  (vhdl-speedbar-check-unit 'architecture)]
	 ["Make" vhdl-speedbar-make-design
	  (save-excursion (beginning-of-line) (looking-at "[0-9]+: *[[<]"))]
	 ["Generate Makefile" vhdl-speedbar-generate-makefile
	  (save-excursion (beginning-of-line) (looking-at "[0-9]+:"))]
	 ["Rescan Directory" vhdl-speedbar-rescan-hierarchy
	  :active (save-excursion (beginning-of-line) (looking-at "[0-9]+:"))
	  ,(if (featurep 'xemacs) :active :visible) (not vhdl-speedbar-show-projects)]
	 ["Rescan Project" vhdl-speedbar-rescan-hierarchy
	  :active (save-excursion (beginning-of-line) (looking-at "[0-9]+:"))
	  ,(if (featurep 'xemacs) :active :visible) vhdl-speedbar-show-projects]
	 ["Save Caches" vhdl-save-caches vhdl-updated-project-list])))
    ;; hook-ups
    (speedbar-add-expansion-list
     '("vhdl directory" vhdl-speedbar-menu-items vhdl-speedbar-key-map
       vhdl-speedbar-display-directory))
    (speedbar-add-expansion-list
     '("vhdl project" vhdl-speedbar-menu-items vhdl-speedbar-key-map
       vhdl-speedbar-display-projects))
    (setq speedbar-stealthy-function-list
	  (append
	   '(("vhdl directory" vhdl-speedbar-update-current-unit)
	     ("vhdl project" vhdl-speedbar-update-current-project
	      vhdl-speedbar-update-current-unit)
; 	     ("files" (lambda () (setq speedbar-ignored-path-regexp
; 				       (speedbar-extension-list-to-regex
; 					speedbar-ignored-path-expressions))))
	     )
	   speedbar-stealthy-function-list))
    (when (eq vhdl-speedbar-display-mode 'directory)
      (setq speedbar-initial-expansion-list-name "vhdl directory"))
    (when (eq vhdl-speedbar-display-mode 'project)
      (setq speedbar-initial-expansion-list-name "vhdl project"))
    (add-hook 'speedbar-timer-hook 'vhdl-update-hierarchy)))

(defun vhdl-speedbar (&optional arg)
  "Open/close speedbar."
  (interactive)
  (if (not (fboundp 'speedbar))
      (error "WARNING:  Speedbar is not available or not installed")
    (condition-case ()
	(speedbar-frame-mode arg)
      (error (error "WARNING:  An error occurred while opening speedbar")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display functions

(defvar vhdl-speedbar-last-selected-project nil
  "Name of last selected project.")

;; macros must be defined in the file they are used (copied from `speedbar.el')
(defmacro speedbar-with-writable (&rest forms)
  "Allow the buffer to be writable and evaluate FORMS."
  (list 'let '((inhibit-read-only t))
	(cons 'progn forms)))
(put 'speedbar-with-writable 'lisp-indent-function 0)

(defun vhdl-speedbar-display-directory (directory depth &optional rescan)
  "Display directory and hierarchy information in speedbar."
  (setq vhdl-speedbar-show-projects nil)
  (setq speedbar-ignored-directory-regexp
	(speedbar-extension-list-to-regex speedbar-ignored-directory-expressions))
  (setq directory (abbreviate-file-name (file-name-as-directory directory)))
  (setq speedbar-last-selected-file nil)
  (speedbar-with-writable
    (condition-case nil
	(progn
	  ;; insert directory path
	  (speedbar-directory-buttons directory depth)
	  ;; insert subdirectories
	  (vhdl-speedbar-insert-dirs (speedbar-file-lists directory) depth)
	  ;; scan and insert hierarchy of current directory
	  (vhdl-speedbar-insert-dir-hierarchy directory depth
					      speedbar-power-click)
	  ;; expand subdirectories
	  (when (= depth 0) (vhdl-speedbar-expand-dirs directory)))
      (error (vhdl-warning-when-idle "ERROR:  Invalid hierarchy information, unable to display correctly")))))

(defun vhdl-speedbar-display-projects (project depth &optional rescan)
  "Display projects and hierarchy information in speedbar."
  (setq vhdl-speedbar-show-projects t)
  (setq speedbar-ignored-directory-regexp ".")
  (setq speedbar-last-selected-file nil)
  (setq vhdl-speedbar-last-selected-project nil)
  (speedbar-with-writable
    (condition-case nil
	;; insert projects
	(vhdl-speedbar-insert-projects)
      (error (vhdl-warning-when-idle "ERROR:  Invalid hierarchy information, unable to display correctly"))))
  (setq speedbar-full-text-cache nil)) ; prevent caching

(defun vhdl-speedbar-insert-projects ()
  "Insert all projects in speedbar."
  (vhdl-speedbar-make-title-line "Projects:")
  (let ((project-alist (if vhdl-project-sort
			   (vhdl-sort-alist (copy-alist vhdl-project-alist))
			 vhdl-project-alist))
	(vhdl-speedbar-update-current-unit nil))
    ;; insert projects
    (while project-alist
      (speedbar-make-tag-line
       'angle ?+ 'vhdl-speedbar-expand-project
       (caar project-alist) (caar project-alist)
       'vhdl-toggle-project (caar project-alist) 'speedbar-directory-face 0)
      (setq project-alist (cdr project-alist)))
    (setq project-alist vhdl-project-alist)
    ;; expand projects
    (while project-alist
      (when (member (caar project-alist) vhdl-speedbar-shown-project-list)
	(goto-char (point-min))
	(when (re-search-forward
	       (concat "^\\([0-9]+:\\s-*<\\)[+]>\\s-+" (caar project-alist) "$") nil t)
	  (goto-char (match-end 1))
	  (speedbar-do-function-pointer)))
      (setq project-alist (cdr project-alist))))
;   (vhdl-speedbar-update-current-project)
;   (vhdl-speedbar-update-current-unit nil t)
  )

(defun vhdl-speedbar-insert-project-hierarchy (project indent &optional rescan)
  "Insert hierarchy of PROJECT.  Rescan directories if RESCAN is non-nil,
otherwise use cached data."
  (when (or rescan (and (not (assoc project vhdl-file-alist))
			(not (vhdl-load-cache project))))
    (vhdl-scan-project-contents project))
  ;; insert design hierarchy
  (vhdl-speedbar-insert-hierarchy
   (aget vhdl-entity-alist project t)
   (aget vhdl-config-alist project t)
   (aget vhdl-package-alist project t)
   (car (aget vhdl-ent-inst-alist project t)) indent)
  (insert (int-to-string indent) ":\n")
  (put-text-property (- (point) 3) (1- (point)) 'invisible t)
  (put-text-property (1- (point)) (point) 'invisible nil)
  ;; expand design units
  (vhdl-speedbar-expand-units project))

(defun vhdl-speedbar-insert-dir-hierarchy (directory depth &optional rescan)
  "Insert hierarchy of DIRECTORY.  Rescan directory if RESCAN is non-nil,
otherwise use cached data."
  (when (or rescan (and (not (assoc directory vhdl-file-alist))
			(not (vhdl-load-cache directory))))
    (vhdl-scan-directory-contents directory))
  ;; insert design hierarchy
  (vhdl-speedbar-insert-hierarchy
   (aget vhdl-entity-alist directory t)
   (aget vhdl-config-alist directory t)
   (aget vhdl-package-alist directory t)
   (car (aget vhdl-ent-inst-alist directory t)) depth)
  ;; expand design units
  (vhdl-speedbar-expand-units directory)
  (aput 'vhdl-directory-alist directory (list (list directory))))

(defun vhdl-speedbar-insert-hierarchy (ent-alist conf-alist pack-alist
						 ent-inst-list depth)
  "Insert hierarchy of ENT-ALIST, CONF-ALIST, and PACK-ALIST."
  (if (not (or ent-alist conf-alist pack-alist))
      (vhdl-speedbar-make-title-line "No VHDL design units!" depth)
    (let (ent-entry conf-entry pack-entry)
      ;; insert entities
      (when ent-alist (vhdl-speedbar-make-title-line "Entities:" depth))
      (while ent-alist
	(setq ent-entry (car ent-alist))
	(speedbar-make-tag-line
	 'bracket ?+ 'vhdl-speedbar-expand-entity (nth 0 ent-entry)
	 (nth 1 ent-entry) 'vhdl-speedbar-find-file
	 (cons (nth 2 ent-entry) (nth 3 ent-entry))
	 'vhdl-speedbar-entity-face depth)
	(unless (nth 2 ent-entry)
	  (end-of-line 0) (insert "!") (forward-char 1))
	(unless (member (nth 0 ent-entry) ent-inst-list)
	  (end-of-line 0) (insert " (top)") (forward-char 1))
	(setq ent-alist (cdr ent-alist)))
      ;; insert configurations
      (when conf-alist (vhdl-speedbar-make-title-line "Configurations:" depth))
      (while conf-alist
	(setq conf-entry (car conf-alist))
	(speedbar-make-tag-line
	 'bracket ?+ 'vhdl-speedbar-expand-config (nth 0 conf-entry)
	 (nth 1 conf-entry) 'vhdl-speedbar-find-file
	 (cons (nth 2 conf-entry) (nth 3 conf-entry))
	 'vhdl-speedbar-configuration-face depth)
	(setq conf-alist (cdr conf-alist)))
      ;; insert packages
      (when pack-alist (vhdl-speedbar-make-title-line "Packages:" depth))
      (while pack-alist
	(setq pack-entry (car pack-alist))
	(vhdl-speedbar-make-pack-line
	 (nth 0 pack-entry) (nth 1 pack-entry)
	 (cons (nth 2 pack-entry) (nth 3 pack-entry))
	 (cons (nth 7 pack-entry) (nth 8 pack-entry))
	 depth)
	(setq pack-alist (cdr pack-alist))))))

(defun vhdl-speedbar-rescan-hierarchy ()
  "Rescan hierarchy for the directory or project under the cursor."
  (interactive)
  (let (key path)
    (cond
     ;; current project
     (vhdl-speedbar-show-projects
      (setq key (vhdl-speedbar-line-project))
      (vhdl-scan-project-contents key))
     ;; top-level directory
     ((save-excursion (beginning-of-line) (looking-at "[^0-9]"))
      (re-search-forward "[0-9]+:" nil t)
      (vhdl-scan-directory-contents
       (abbreviate-file-name (speedbar-line-directory))))
     ;; current directory
     (t (setq path (speedbar-line-directory))
	(string-match "^\\(.+[/\\]\\)" path)
	(vhdl-scan-directory-contents
	 (abbreviate-file-name (match-string 1 path)))))
    (vhdl-speedbar-refresh key)))

(defun vhdl-speedbar-expand-dirs (directory)
  "Expand subdirectories in DIRECTORY according to
 `speedbar-shown-directories'."
  ;; (nicked from `speedbar-default-directory-list')
  (let ((sf (cdr (reverse speedbar-shown-directories)))
	(vhdl-speedbar-update-current-unit nil))
    (setq speedbar-shown-directories
	  (list (expand-file-name default-directory)))
    (while sf
      (when (speedbar-goto-this-file (car sf))
	(beginning-of-line)
	(when (looking-at "[0-9]+:\\s-*<")
	  (goto-char (match-end 0))
	  (speedbar-do-function-pointer)))
      (setq sf (cdr sf))))
  (vhdl-speedbar-update-current-unit nil t))

(defun vhdl-speedbar-expand-units (key)
  "Expand design units in directory/project KEY according to
`vhdl-speedbar-shown-unit-alist'."
  (let ((unit-alist (aget vhdl-speedbar-shown-unit-alist key t))
	(vhdl-speedbar-update-current-unit nil)
	vhdl-updated-project-list)
    (adelete 'vhdl-speedbar-shown-unit-alist key)
    (vhdl-prepare-search-1
     (while unit-alist			; expand units
       (vhdl-speedbar-goto-this-unit key (caar unit-alist))
       (beginning-of-line)
       (let ((arch-alist (nth 1 (car unit-alist)))
	     position)
	 (when (looking-at "^[0-9]+:\\s-*\\[")
	   (goto-char (match-end 0))
	   (setq position (point))
	   (speedbar-do-function-pointer)
	   (select-frame speedbar-frame)
	   (while arch-alist		; expand architectures
	     (goto-char position)
	     (when (re-search-forward
		    (concat "^[0-9]+:\\s-*\\(\\[\\|{.}\\s-+"
			    (car arch-alist) "\\>\\)") nil t)
	       (beginning-of-line)
	       (when (looking-at "^[0-9]+:\\s-*{")
		 (goto-char (match-end 0))
		 (speedbar-do-function-pointer)
		 (select-frame speedbar-frame)))
	     (setq arch-alist (cdr arch-alist))))
	 (setq unit-alist (cdr unit-alist))))))
  (vhdl-speedbar-update-current-unit nil t))

(defun vhdl-speedbar-contract-level ()
  "Contract current level in current directory/project."
  (interactive)
  (when (or (save-excursion
	      (beginning-of-line) (looking-at "^[0-9]:\\s-*[[{<]-"))
	    (and (save-excursion
		   (beginning-of-line) (looking-at "^\\([0-9]+\\):"))
		 (re-search-backward
		  (format "^[0-%d]:\\s-*[[{<]-"
			  (max (1- (string-to-number (match-string 1))) 0)) nil t)))
    (goto-char (match-end 0))
    (speedbar-do-function-pointer)
    (speedbar-center-buffer-smartly)))

(defun vhdl-speedbar-contract-all ()
  "Contract all expanded design units in current directory/project."
  (interactive)
  (if (and vhdl-speedbar-show-projects
	   (save-excursion (beginning-of-line) (looking-at "^0:")))
      (progn (setq vhdl-speedbar-shown-project-list nil)
	     (vhdl-speedbar-refresh))
    (let ((key (vhdl-speedbar-line-key)))
      (adelete 'vhdl-speedbar-shown-unit-alist key)
      (vhdl-speedbar-refresh (and vhdl-speedbar-show-projects key))
      (when (memq 'display vhdl-speedbar-save-cache)
	(add-to-list 'vhdl-updated-project-list key)))))

(defun vhdl-speedbar-expand-all ()
  "Expand all design units in current directory/project."
  (interactive)
  (let* ((key (vhdl-speedbar-line-key))
	 (ent-alist (aget vhdl-entity-alist key t))
	 (conf-alist (aget vhdl-config-alist key t))
	 (pack-alist (aget vhdl-package-alist key t))
	 arch-alist unit-alist subunit-alist)
    (add-to-list 'vhdl-speedbar-shown-project-list key)
    (while ent-alist
      (setq arch-alist (nth 4 (car ent-alist)))
      (setq subunit-alist nil)
      (while arch-alist
	(setq subunit-alist (cons (caar arch-alist) subunit-alist))
	(setq arch-alist (cdr arch-alist)))
      (setq unit-alist (cons (list (caar ent-alist) subunit-alist) unit-alist))
      (setq ent-alist (cdr ent-alist)))
    (while conf-alist
      (setq unit-alist (cons (list (caar conf-alist)) unit-alist))
      (setq conf-alist (cdr conf-alist)))
    (while pack-alist
      (setq unit-alist (cons (list (caar pack-alist)) unit-alist))
      (setq pack-alist (cdr pack-alist)))
    (aput 'vhdl-speedbar-shown-unit-alist key unit-alist)
    (vhdl-speedbar-refresh)
    (when (memq 'display vhdl-speedbar-save-cache)
      (add-to-list 'vhdl-updated-project-list key))))

(defun vhdl-speedbar-expand-project (text token indent)
  "Expand/contract the project under the cursor."
  (cond
   ((string-match "+" text)		; expand project
    (speedbar-change-expand-button-char ?-)
    (unless (member token vhdl-speedbar-shown-project-list)
      (setq vhdl-speedbar-shown-project-list
	    (cons token vhdl-speedbar-shown-project-list)))
    (speedbar-with-writable
      (save-excursion
	(end-of-line) (forward-char 1)
	(vhdl-speedbar-insert-project-hierarchy token (1+ indent)
						speedbar-power-click))))
   ((string-match "-" text)		; contract project
    (speedbar-change-expand-button-char ?+)
    (setq vhdl-speedbar-shown-project-list
	  (delete token vhdl-speedbar-shown-project-list))
    (speedbar-delete-subblock indent))
   (t (error "Nothing to display")))
  (when (equal (selected-frame) speedbar-frame)
    (speedbar-center-buffer-smartly)))

(defun vhdl-speedbar-expand-entity (text token indent)
  "Expand/contract the entity under the cursor."
  (cond
   ((string-match "+" text)		; expand entity
    (let* ((key (vhdl-speedbar-line-key indent))
	   (ent-alist (aget vhdl-entity-alist key t))
	   (ent-entry (aget ent-alist token t))
	   (arch-alist (nth 3 ent-entry))
	   (inst-alist (vhdl-get-instantiations token indent))
	   (subpack-alist (nth 5 ent-entry))
	   (multiple-arch (> (length arch-alist) 1))
	   arch-entry inst-entry)
      (if (not (or arch-alist inst-alist subpack-alist))
	  (speedbar-change-expand-button-char ??)
	(speedbar-change-expand-button-char ?-)
	;; add entity to `vhdl-speedbar-shown-unit-alist'
	(let* ((unit-alist (aget vhdl-speedbar-shown-unit-alist key t)))
	  (aput 'unit-alist token nil)
	  (aput 'vhdl-speedbar-shown-unit-alist key unit-alist))
	(speedbar-with-writable
	 (save-excursion
	   (end-of-line) (forward-char 1)
	   ;; insert architectures
	   (when arch-alist
	     (vhdl-speedbar-make-title-line "Architectures:" (1+ indent)))
	   (while arch-alist
	     (setq arch-entry (car arch-alist))
	     (speedbar-make-tag-line
	      'curly ?+ 'vhdl-speedbar-expand-architecture
	      (cons token (nth 0 arch-entry))
	      (nth 1 arch-entry) 'vhdl-speedbar-find-file
	      (cons (nth 2 arch-entry) (nth 3 arch-entry))
	      'vhdl-speedbar-architecture-face (1+ indent))
	     (when (and multiple-arch
			(equal (nth 0 arch-entry) (nth 4 ent-entry)))
	       (end-of-line 0) (insert " (mra)") (forward-char 1))
	     (setq arch-alist (cdr arch-alist)))
	   ;; insert instantiations
	   (when inst-alist
	     (vhdl-speedbar-make-title-line "Instantiated as:" (1+ indent)))
	   (while inst-alist
	     (setq inst-entry (car inst-alist))
	     (vhdl-speedbar-make-inst-line
	      (nth 0 inst-entry) (nth 1 inst-entry) (nth 2 inst-entry)
	      (nth 3 inst-entry) (nth 4 inst-entry) (nth 5 inst-entry)
	      nil nil nil (1+ indent) 0 " in ")
	     (setq inst-alist (cdr inst-alist)))
	   ;; insert required packages
	   (vhdl-speedbar-insert-subpackages
	    subpack-alist (1+ indent) indent)))
	(when (memq 'display vhdl-speedbar-save-cache)
	  (add-to-list 'vhdl-updated-project-list key))
	(vhdl-speedbar-update-current-unit t t))))
   ((string-match "-" text)		; contract entity
    (speedbar-change-expand-button-char ?+)
    ;; remove entity from `vhdl-speedbar-shown-unit-alist'
    (let* ((key (vhdl-speedbar-line-key indent))
	   (unit-alist (aget vhdl-speedbar-shown-unit-alist key t)))
      (adelete 'unit-alist token)
      (if unit-alist
	  (aput 'vhdl-speedbar-shown-unit-alist key unit-alist)
	(adelete 'vhdl-speedbar-shown-unit-alist key))
      (speedbar-delete-subblock indent)
      (when (memq 'display vhdl-speedbar-save-cache)
	(add-to-list 'vhdl-updated-project-list key))))
   (t (error "Nothing to display")))
  (when (equal (selected-frame) speedbar-frame)
    (speedbar-center-buffer-smartly)))

(defun vhdl-speedbar-expand-architecture (text token indent)
  "Expand/contract the architecture under the cursor."
  (cond
   ((string-match "+" text)		; expand architecture
    (let* ((key (vhdl-speedbar-line-key (1- indent)))
	   (ent-alist (aget vhdl-entity-alist key t))
	   (conf-alist (aget vhdl-config-alist key t))
	   (hier-alist (vhdl-get-hierarchy
			ent-alist conf-alist (car token) (cdr token) nil nil
			0 (1- indent)))
	   (ent-entry (aget ent-alist (car token) t))
	   (arch-entry (aget (nth 3 ent-entry) (cdr token) t))
	   (subpack-alist (nth 4 arch-entry))
	   entry)
      (if (not (or hier-alist subpack-alist))
	  (speedbar-change-expand-button-char ??)
	(speedbar-change-expand-button-char ?-)
	;; add architecture to `vhdl-speedbar-shown-unit-alist'
	(let* ((unit-alist (aget vhdl-speedbar-shown-unit-alist key t))
	       (arch-alist (nth 0 (aget unit-alist (car token) t))))
	  (aput 'unit-alist (car token) (list (cons (cdr token) arch-alist)))
	  (aput 'vhdl-speedbar-shown-unit-alist key unit-alist))
	(speedbar-with-writable
	  (save-excursion
	    (end-of-line) (forward-char 1)
	    ;; insert instance hierarchy
	    (when hier-alist
	      (vhdl-speedbar-make-title-line "Subcomponent hierarchy:"
					     (1+ indent)))
	    (while hier-alist
	      (setq entry (car hier-alist))
	      (when (or (= vhdl-speedbar-hierarchy-depth 0)
			(< (nth 9 entry) vhdl-speedbar-hierarchy-depth))
		(vhdl-speedbar-make-inst-line
		 (nth 0 entry) (nth 1 entry) (nth 2 entry) (nth 3 entry)
		 (nth 4 entry) (nth 5 entry) (nth 6 entry) (nth 7 entry)
		 (nth 8 entry) (1+ indent) (1+ (nth 9 entry)) ": "))
	      (setq hier-alist (cdr hier-alist)))
	    ;; insert required packages
	    (vhdl-speedbar-insert-subpackages
	     subpack-alist (1+ indent) (1- indent))))
	(when (memq 'display vhdl-speedbar-save-cache)
	  (add-to-list 'vhdl-updated-project-list key))
	(vhdl-speedbar-update-current-unit t t))))
   ((string-match "-" text)		; contract architecture
    (speedbar-change-expand-button-char ?+)
    ;; remove architecture from `vhdl-speedbar-shown-unit-alist'
    (let* ((key (vhdl-speedbar-line-key (1- indent)))
	   (unit-alist (aget vhdl-speedbar-shown-unit-alist key t))
	   (arch-alist (nth 0 (aget unit-alist (car token) t))))
      (aput 'unit-alist (car token) (list (delete (cdr token) arch-alist)))
      (aput 'vhdl-speedbar-shown-unit-alist key unit-alist)
      (speedbar-delete-subblock indent)
      (when (memq 'display vhdl-speedbar-save-cache)
	(add-to-list 'vhdl-updated-project-list key))))
   (t (error "Nothing to display")))
  (when (equal (selected-frame) speedbar-frame)
    (speedbar-center-buffer-smartly)))

(defun vhdl-speedbar-expand-config (text token indent)
  "Expand/contract the configuration under the cursor."
  (cond
   ((string-match "+" text)		; expand configuration
    (let* ((key (vhdl-speedbar-line-key indent))
	   (conf-alist (aget vhdl-config-alist key t))
	   (conf-entry (aget conf-alist token))
	   (ent-alist (aget vhdl-entity-alist key t))
	   (hier-alist (vhdl-get-hierarchy
			ent-alist conf-alist (nth 3 conf-entry)
			(nth 4 conf-entry) token (nth 5 conf-entry)
			0 indent t))
	   (subpack-alist (nth 6 conf-entry))
	   entry)
      (if (not (or hier-alist subpack-alist))
	  (speedbar-change-expand-button-char ??)
	(speedbar-change-expand-button-char ?-)
	;; add configuration to `vhdl-speedbar-shown-unit-alist'
	(let* ((unit-alist (aget vhdl-speedbar-shown-unit-alist key t)))
	  (aput 'unit-alist token nil)
	  (aput 'vhdl-speedbar-shown-unit-alist key unit-alist))
	(speedbar-with-writable
	 (save-excursion
	   (end-of-line) (forward-char 1)
	   ;; insert instance hierarchy
	   (when hier-alist
	     (vhdl-speedbar-make-title-line "Design hierarchy:" (1+ indent)))
	   (while hier-alist
	     (setq entry (car hier-alist))
	     (when (or (= vhdl-speedbar-hierarchy-depth 0)
		       (<= (nth 9 entry) vhdl-speedbar-hierarchy-depth))
	       (vhdl-speedbar-make-inst-line
		(nth 0 entry) (nth 1 entry) (nth 2 entry) (nth 3 entry)
		(nth 4 entry) (nth 5 entry) (nth 6 entry) (nth 7 entry)
		(nth 8 entry) (1+ indent) (nth 9 entry) ": "))
	     (setq hier-alist (cdr hier-alist)))
	   ;; insert required packages
	   (vhdl-speedbar-insert-subpackages
	    subpack-alist (1+ indent) indent)))
	(when (memq 'display vhdl-speedbar-save-cache)
	  (add-to-list 'vhdl-updated-project-list key))
	(vhdl-speedbar-update-current-unit t t))))
   ((string-match "-" text)		; contract configuration
    (speedbar-change-expand-button-char ?+)
    ;; remove configuration from `vhdl-speedbar-shown-unit-alist'
    (let* ((key (vhdl-speedbar-line-key indent))
	   (unit-alist (aget vhdl-speedbar-shown-unit-alist key t)))
      (adelete 'unit-alist token)
      (if unit-alist
	  (aput 'vhdl-speedbar-shown-unit-alist key unit-alist)
	(adelete 'vhdl-speedbar-shown-unit-alist key))
      (speedbar-delete-subblock indent)
      (when (memq 'display vhdl-speedbar-save-cache)
	(add-to-list 'vhdl-updated-project-list key))))
   (t (error "Nothing to display")))
  (when (equal (selected-frame) speedbar-frame)
    (speedbar-center-buffer-smartly)))

(defun vhdl-speedbar-expand-package (text token indent)
  "Expand/contract the package under the cursor."
  (cond
   ((string-match "+" text)		; expand package
    (let* ((key (vhdl-speedbar-line-key indent))
	   (pack-alist (aget vhdl-package-alist key t))
	   (pack-entry (aget pack-alist token t))
	   (comp-alist (nth 3 pack-entry))
	   (func-alist (nth 4 pack-entry))
	   (func-body-alist (nth 8 pack-entry))
	   (subpack-alist (append (nth 5 pack-entry) (nth 9 pack-entry)))
	   comp-entry func-entry func-body-entry)
      (if (not (or comp-alist func-alist subpack-alist))
	  (speedbar-change-expand-button-char ??)
	(speedbar-change-expand-button-char ?-)
	;; add package to `vhdl-speedbar-shown-unit-alist'
	(let* ((unit-alist (aget vhdl-speedbar-shown-unit-alist key t)))
	  (aput 'unit-alist token nil)
	  (aput 'vhdl-speedbar-shown-unit-alist key unit-alist))
	(speedbar-with-writable
	  (save-excursion
	    (end-of-line) (forward-char 1)
	    ;; insert components
	    (when comp-alist
	      (vhdl-speedbar-make-title-line "Components:" (1+ indent)))
	    (while comp-alist
	      (setq comp-entry (car comp-alist))
	      (speedbar-make-tag-line
	       nil nil nil
	       (cons token (nth 0 comp-entry))
	       (nth 1 comp-entry) 'vhdl-speedbar-find-file
	       (cons (nth 2 comp-entry) (nth 3 comp-entry))
	       'vhdl-speedbar-entity-face (1+ indent))
	      (setq comp-alist (cdr comp-alist)))
	    ;; insert subprograms
	    (when func-alist
	      (vhdl-speedbar-make-title-line "Subprograms:" (1+ indent)))
	    (while func-alist
	      (setq func-entry (car func-alist)
		    func-body-entry (aget func-body-alist (car func-entry) t))
	      (when (nth 2 func-entry)
		(vhdl-speedbar-make-subprogram-line
		 (nth 1 func-entry)
		 (cons (nth 2 func-entry) (nth 3 func-entry))
		 (cons (nth 1 func-body-entry) (nth 2 func-body-entry))
		 (1+ indent)))
	      (setq func-alist (cdr func-alist)))
	   ;; insert required packages
	   (vhdl-speedbar-insert-subpackages
	    subpack-alist (1+ indent) indent)))
	(when (memq 'display vhdl-speedbar-save-cache)
	  (add-to-list 'vhdl-updated-project-list key))
	(vhdl-speedbar-update-current-unit t t))))
   ((string-match "-" text)		; contract package
    (speedbar-change-expand-button-char ?+)
    ;; remove package from `vhdl-speedbar-shown-unit-alist'
    (let* ((key (vhdl-speedbar-line-key indent))
	   (unit-alist (aget vhdl-speedbar-shown-unit-alist key t)))
      (adelete 'unit-alist token)
      (if unit-alist
	  (aput 'vhdl-speedbar-shown-unit-alist key unit-alist)
	(adelete 'vhdl-speedbar-shown-unit-alist key))
      (speedbar-delete-subblock indent)
      (when (memq 'display vhdl-speedbar-save-cache)
	(add-to-list 'vhdl-updated-project-list key))))
   (t (error "Nothing to display")))
  (when (equal (selected-frame) speedbar-frame)
    (speedbar-center-buffer-smartly)))

(defun vhdl-speedbar-insert-subpackages (subpack-alist indent dir-indent)
  "Insert required packages."
  (let* ((pack-alist (aget vhdl-package-alist
			   (vhdl-speedbar-line-key dir-indent) t))
	 pack-key lib-name pack-entry)
    (when subpack-alist
      (vhdl-speedbar-make-title-line "Packages Used:" indent))
    (while subpack-alist
      (setq pack-key (cdar subpack-alist)
	    lib-name (caar subpack-alist))
      (setq pack-entry (aget pack-alist pack-key t))
      (vhdl-speedbar-make-subpack-line
       (or (nth 0 pack-entry) pack-key) lib-name
       (cons (nth 1 pack-entry) (nth 2 pack-entry))
       (cons (nth 6 pack-entry) (nth 7 pack-entry)) indent)
      (setq subpack-alist (cdr subpack-alist)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display help functions

(defvar vhdl-speedbar-update-current-unit t
  "Non-nil means to run `vhdl-speedbar-update-current-unit'.")

(defun vhdl-speedbar-update-current-project ()
  "Highlight project that is currently active."
  (when (and vhdl-speedbar-show-projects
	     (not (equal vhdl-speedbar-last-selected-project vhdl-project))
	     (and (boundp 'speedbar-frame)
		  (frame-live-p speedbar-frame)))
    (let ((last-frame (selected-frame))
	  (project-alist vhdl-project-alist)
	  pos)
      (select-frame speedbar-frame)
      (speedbar-with-writable
       (save-excursion
	 (while project-alist
	   (goto-char (point-min))
	   (when (re-search-forward
		  (concat "<.> \\(" (caar project-alist) "\\)$") nil t)
	     (put-text-property (match-beginning 1) (match-end 1) 'face
				(if (equal (caar project-alist) vhdl-project)
				    'speedbar-selected-face
				  'speedbar-directory-face))
	     (when (equal (caar project-alist) vhdl-project)
	       (setq pos (1- (match-beginning 1)))))
	   (setq project-alist (cdr project-alist))))
       (when pos (goto-char pos)))
      (select-frame last-frame)
      (setq vhdl-speedbar-last-selected-project vhdl-project)))
  t)

(defun vhdl-speedbar-update-current-unit (&optional no-position always)
  "Highlight all design units that are contained in the current file.
NO-POSITION non-nil means do not re-position cursor."
  (let ((last-frame (selected-frame))
	(project-list vhdl-speedbar-shown-project-list)
	file-alist pos file-name)
    ;; get current file name
    (if (fboundp 'speedbar-select-attached-frame)
	(speedbar-select-attached-frame)
      (select-frame speedbar-attached-frame))
    (setq file-name (abbreviate-file-name (or (buffer-file-name) "")))
    (when (and vhdl-speedbar-update-current-unit
	       (or always (not (equal file-name speedbar-last-selected-file))))
      (if vhdl-speedbar-show-projects
	  (while project-list
	    (setq file-alist (append file-alist (aget vhdl-file-alist
						      (car project-list) t)))
	    (setq project-list (cdr project-list)))
	(setq file-alist (aget vhdl-file-alist
			       (abbreviate-file-name default-directory) t)))
      (select-frame speedbar-frame)
      (set-buffer speedbar-buffer)
      (speedbar-with-writable
       (vhdl-prepare-search-1
	(save-excursion
	  ;; unhighlight last units
	  (let* ((file-entry (aget file-alist speedbar-last-selected-file t)))
	    (vhdl-speedbar-update-units
	     "\\[.\\] " (nth 0 file-entry)
	     speedbar-last-selected-file 'vhdl-speedbar-entity-face)
	    (vhdl-speedbar-update-units
	     "{.} " (nth 1 file-entry)
	     speedbar-last-selected-file 'vhdl-speedbar-architecture-face)
	    (vhdl-speedbar-update-units
	     "\\[.\\] " (nth 3 file-entry)
	     speedbar-last-selected-file 'vhdl-speedbar-configuration-face)
	    (vhdl-speedbar-update-units
	     "[]>] " (nth 4 file-entry)
	     speedbar-last-selected-file 'vhdl-speedbar-package-face)
	    (vhdl-speedbar-update-units
	     "\\[.\\].+(" '("body")
	     speedbar-last-selected-file 'vhdl-speedbar-package-face)
	    (vhdl-speedbar-update-units
	     "> " (nth 6 file-entry)
	     speedbar-last-selected-file 'vhdl-speedbar-instantiation-face))
	  ;; highlight current units
	  (let* ((file-entry (aget file-alist file-name t)))
	    (setq
	     pos (vhdl-speedbar-update-units
		  "\\[.\\] " (nth 0 file-entry)
		  file-name 'vhdl-speedbar-entity-selected-face pos)
	     pos (vhdl-speedbar-update-units
		  "{.} " (nth 1 file-entry)
		  file-name 'vhdl-speedbar-architecture-selected-face pos)
	     pos (vhdl-speedbar-update-units
		  "\\[.\\] " (nth 3 file-entry)
		  file-name 'vhdl-speedbar-configuration-selected-face pos)
	     pos (vhdl-speedbar-update-units
		  "[]>] " (nth 4 file-entry)
		  file-name 'vhdl-speedbar-package-selected-face pos)
	     pos (vhdl-speedbar-update-units
		  "\\[.\\].+(" '("body")
		  file-name 'vhdl-speedbar-package-selected-face pos)
	     pos (vhdl-speedbar-update-units
		  "> " (nth 6 file-entry)
		  file-name 'vhdl-speedbar-instantiation-selected-face pos))))))
      ;; move speedbar so the first highlighted unit is visible
      (when (and pos (not no-position))
	(goto-char pos)
 	(speedbar-center-buffer-smartly)
	(speedbar-position-cursor-on-line))
      (setq speedbar-last-selected-file file-name))
    (select-frame last-frame)
    t))

(defun vhdl-speedbar-update-units (text unit-list file-name face
					&optional pos)
  "Help function to highlight design units."
  (while unit-list
    (goto-char (point-min))
    (while (re-search-forward
	    (concat text "\\(" (car unit-list) "\\)\\>") nil t)
      (when (equal file-name (car (get-text-property
				   (match-beginning 1) 'speedbar-token)))
	(setq pos (or pos (point-marker)))
	(put-text-property (match-beginning 1) (match-end 1) 'face face)))
    (setq unit-list (cdr unit-list)))
  pos)

(defun vhdl-speedbar-make-inst-line (inst-name inst-file-marker
					       ent-name ent-file-marker
					       arch-name arch-file-marker
					       conf-name conf-file-marker
					       lib-name depth offset delimiter)
  "Insert instantiation entry."
  (let ((start (point))
	visible-start)
    (insert (int-to-string depth) ":")
    (put-text-property start (point) 'invisible t)
    (setq visible-start (point))
    (insert-char ?  (* depth speedbar-indentation-width))
    (while (> offset 0)
      (insert "|")
      (insert-char (if (= offset 1) ?- ? ) (1- speedbar-indentation-width))
      (setq offset (1- offset)))
    (put-text-property visible-start (point) 'invisible nil)
    (setq start (point))
    (insert ">")
    (speedbar-make-button start (point) nil nil nil)
    (setq visible-start (point))
    (insert " ")
    (setq start (point))
    (if (not inst-name)
	(insert "(top)")
      (insert inst-name)
      (speedbar-make-button
       start (point) 'vhdl-speedbar-instantiation-face 'speedbar-highlight-face
       'vhdl-speedbar-find-file inst-file-marker))
    (insert delimiter)
    (when ent-name
      (setq start (point))
      (insert ent-name)
      (speedbar-make-button
       start (point) 'vhdl-speedbar-entity-face 'speedbar-highlight-face
       'vhdl-speedbar-find-file ent-file-marker)
      (when arch-name
	(insert " (")
	(setq start (point))
	(insert arch-name)
	(speedbar-make-button
	 start (point) 'vhdl-speedbar-architecture-face 'speedbar-highlight-face
	 'vhdl-speedbar-find-file arch-file-marker)
	(insert ")"))
      (when conf-name
	(insert " (")
	(setq start (point))
	(insert conf-name)
	(speedbar-make-button
	 start (point) 'vhdl-speedbar-configuration-face 'speedbar-highlight-face
	 'vhdl-speedbar-find-file conf-file-marker)
	(insert ")")))
    (when (and lib-name (not (equal lib-name (downcase (vhdl-work-library)))))
      (setq start (point))
      (insert " (" lib-name ")")
      (put-text-property (+ 2 start) (1- (point)) 'face
			 'vhdl-speedbar-library-face))
    (insert-char ?\n 1)
    (put-text-property visible-start (point) 'invisible nil)))

(defun vhdl-speedbar-make-pack-line (pack-key pack-name pack-file-marker
					      body-file-marker depth)
  "Insert package entry."
  (let ((start (point))
	visible-start)
    (insert (int-to-string depth) ":")
    (put-text-property start (point) 'invisible t)
    (setq visible-start (point))
    (insert-char ?  (* depth speedbar-indentation-width))
    (put-text-property visible-start (point) 'invisible nil)
    (setq start (point))
    (insert "[+]")
    (speedbar-make-button
     start (point) 'speedbar-button-face 'speedbar-highlight-face
     'vhdl-speedbar-expand-package pack-key)
    (setq visible-start (point))
    (insert-char ?  1 nil)
    (setq start (point))
    (insert pack-name)
    (speedbar-make-button
     start (point) 'vhdl-speedbar-package-face 'speedbar-highlight-face
     'vhdl-speedbar-find-file pack-file-marker)
    (unless (car pack-file-marker)
      (insert "!"))
    (when (car body-file-marker)
      (insert " (")
      (setq start (point))
      (insert "body")
      (speedbar-make-button
       start (point) 'vhdl-speedbar-package-face 'speedbar-highlight-face
       'vhdl-speedbar-find-file body-file-marker)
      (insert ")"))
    (insert-char ?\n 1)
    (put-text-property visible-start (point) 'invisible nil)))

(defun vhdl-speedbar-make-subpack-line (pack-name lib-name pack-file-marker
						  pack-body-file-marker depth)
  "Insert used package entry."
  (let ((start (point))
	visible-start)
    (insert (int-to-string depth) ":")
    (put-text-property start (point) 'invisible t)
    (setq visible-start (point))
    (insert-char ?  (* depth speedbar-indentation-width))
    (put-text-property visible-start (point) 'invisible nil)
    (setq start (point))
    (insert ">")
    (speedbar-make-button start (point) nil nil nil)
    (setq visible-start (point))
    (insert " ")
    (setq start (point))
    (insert pack-name)
    (speedbar-make-button
     start (point) 'vhdl-speedbar-package-face 'speedbar-highlight-face
     'vhdl-speedbar-find-file pack-file-marker)
    (when (car pack-body-file-marker)
      (insert " (")
      (setq start (point))
      (insert "body")
      (speedbar-make-button
       start (point) 'vhdl-speedbar-package-face 'speedbar-highlight-face
       'vhdl-speedbar-find-file pack-body-file-marker)
      (insert ")"))
    (setq start (point))
    (insert " (" lib-name ")")
    (put-text-property (+ 2 start) (1- (point)) 'face
		       'vhdl-speedbar-library-face)
    (insert-char ?\n 1)
    (put-text-property visible-start (point) 'invisible nil)))

(defun vhdl-speedbar-make-subprogram-line (func-name func-file-marker
						     func-body-file-marker
						     depth)
  "Insert subprogram entry."
  (let ((start (point))
	visible-start)
    (insert (int-to-string depth) ":")
    (put-text-property start (point) 'invisible t)
    (setq visible-start (point))
    (insert-char ?  (* depth speedbar-indentation-width))
    (put-text-property visible-start (point) 'invisible nil)
    (setq start (point))
    (insert ">")
    (speedbar-make-button start (point) nil nil nil)
    (setq visible-start (point))
    (insert " ")
    (setq start (point))
    (insert func-name)
    (speedbar-make-button
     start (point) 'vhdl-speedbar-subprogram-face 'speedbar-highlight-face
     'vhdl-speedbar-find-file func-file-marker)
    (when (car func-body-file-marker)
      (insert " (")
      (setq start (point))
      (insert "body")
      (speedbar-make-button
       start (point) 'vhdl-speedbar-subprogram-face 'speedbar-highlight-face
       'vhdl-speedbar-find-file func-body-file-marker)
      (insert ")"))
    (insert-char ?\n 1)
    (put-text-property visible-start (point) 'invisible nil)))

(defun vhdl-speedbar-make-title-line (text &optional depth)
  "Insert design unit title entry."
  (let ((start (point))
	visible-start)
    (when depth
      (insert (int-to-string depth) ":")
      (put-text-property start (point) 'invisible t))
    (setq visible-start (point))
    (insert-char ?  (* (or depth 0) speedbar-indentation-width))
    (setq start (point))
    (insert text)
    (speedbar-make-button start (point) nil nil nil nil)
    (insert-char ?\n 1)
    (put-text-property visible-start (point) 'invisible nil)))

(defun vhdl-speedbar-insert-dirs (files level)
  "Insert subdirectories."
  (let ((dirs (car files)))
    (while dirs
      (speedbar-make-tag-line 'angle ?+ 'vhdl-speedbar-dired (car dirs)
			      (car dirs) 'speedbar-dir-follow nil
			      'speedbar-directory-face level)
      (setq dirs (cdr dirs)))))

(defun vhdl-speedbar-dired (text token indent)
  "Speedbar click handler for directory expand button in hierarchy mode."
  (cond ((string-match "+" text)	; we have to expand this dir
	 (setq speedbar-shown-directories
	       (cons (expand-file-name
		      (concat (speedbar-line-directory indent) token "/"))
		     speedbar-shown-directories))
  	 (speedbar-change-expand-button-char ?-)
	 (speedbar-reset-scanners)
	 (speedbar-with-writable
	   (save-excursion
	     (end-of-line) (forward-char 1)
	     (vhdl-speedbar-insert-dirs
	      (speedbar-file-lists
	       (concat (speedbar-line-directory indent) token "/"))
	      (1+ indent))
	     (speedbar-reset-scanners)
	     (vhdl-speedbar-insert-dir-hierarchy
	      (abbreviate-file-name
	       (concat (speedbar-line-directory indent) token "/"))
	      (1+ indent) speedbar-power-click)))
	 (vhdl-speedbar-update-current-unit t t))
	((string-match "-" text)	; we have to contract this node
	 (speedbar-reset-scanners)
	 (let ((oldl speedbar-shown-directories)
	       (newl nil)
	       (td (expand-file-name
		    (concat (speedbar-line-directory indent) token))))
	   (while oldl
	     (if (not (string-match (concat "^" (regexp-quote td)) (car oldl)))
		 (setq newl (cons (car oldl) newl)))
	     (setq oldl (cdr oldl)))
	   (setq speedbar-shown-directories (nreverse newl)))
	 (speedbar-change-expand-button-char ?+)
	 (speedbar-delete-subblock indent))
	(t (error "Nothing to display")))
  (when (equal (selected-frame) speedbar-frame)
    (speedbar-center-buffer-smartly)))

(defun vhdl-speedbar-item-info ()
  "Derive and display information about this line item."
  (save-excursion
    (beginning-of-line)
    ;; skip invisible number info
    (when (looking-at "^[0-9]+:") (goto-char (match-end 0)))
    (cond
     ;; project/directory entry
     ((looking-at "\\s-*<[-+?]>\\s-+\\([^\n]+\\)$")
      (if vhdl-speedbar-show-projects
	  (message "Project \"%s\"" (match-string-no-properties 1))
	(speedbar-files-item-info)))
     ;; design unit entry
     ((looking-at "\\(\\s-*\\([[{][-+?][]}]\\|[| -]*>\\) \\)\"?\\w")
      (goto-char (match-end 1))
      (let ((face (get-text-property (point) 'face)))
	(message
	 "%s \"%s\" in \"%s\""
	 ;; design unit kind
	 (cond ((or (eq face 'vhdl-speedbar-entity-face)
		    (eq face 'vhdl-speedbar-entity-selected-face))
		(if (equal (match-string 2) ">") "Component" "Entity"))
	       ((or (eq face 'vhdl-speedbar-architecture-face)
		    (eq face 'vhdl-speedbar-architecture-selected-face))
		"Architecture")
	       ((or (eq face 'vhdl-speedbar-configuration-face)
		    (eq face 'vhdl-speedbar-configuration-selected-face))
		"Configuration")
	       ((or (eq face 'vhdl-speedbar-package-face)
		    (eq face 'vhdl-speedbar-package-selected-face))
		"Package")
	       ((or (eq face 'vhdl-speedbar-instantiation-face)
		    (eq face 'vhdl-speedbar-instantiation-selected-face))
		"Instantiation")
	       ((eq face 'vhdl-speedbar-subprogram-face)
		"Subprogram")
	       (t ""))
	 ;; design unit name
	 (buffer-substring-no-properties
	  (progn (looking-at "\"?\\(\\(\\w\\|_\\)+\\)\"?") (match-beginning 1))
	  (match-end 1))
	 ;; file name
	 (file-relative-name
	  (or (car (get-text-property (point) 'speedbar-token))
	      "?")
	  (vhdl-default-directory)))))
     (t (message "")))))

(defun vhdl-speedbar-line-text ()
  "Calls `speedbar-line-text' and removes text properties."
  (let ((string (speedbar-line-text)))
    (set-text-properties 0 (length string) nil string)
    string))

(defun vhdl-speedbar-higher-text ()
  "Get speedbar-line-text of higher level."
  (let (depth string)
    (save-excursion
      (beginning-of-line)
      (looking-at "^\\([0-9]+\\):")
      (setq depth (string-to-number (match-string 1)))
      (when (re-search-backward (format "^%d: *[[<{][-+?][]>}] \\([^ \n]+\\)" (1- depth)) nil t)
	(setq string (match-string 1))
	(set-text-properties 0 (length string) nil string)
	string))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Help functions

(defun vhdl-speedbar-line-key (&optional indent)
  "Get currently displayed directory of project name."
  (if vhdl-speedbar-show-projects
      (vhdl-speedbar-line-project)
    (abbreviate-file-name
     (file-name-as-directory (speedbar-line-directory indent)))))

(defun vhdl-speedbar-line-project (&optional indent)
  "Get currently displayed project name."
  (and vhdl-speedbar-show-projects
       (save-excursion
	 (end-of-line)
	 (re-search-backward "^[0-9]+:\\s-*<[-+?]>\\s-+\\([^\n]+\\)$" nil t)
	 (match-string-no-properties 1))))

(defun vhdl-add-modified-file ()
  "Add file to `vhdl-modified-file-list'."
  (when vhdl-file-alist
    (add-to-list 'vhdl-modified-file-list (buffer-file-name)))
  nil)

(defun vhdl-resolve-paths (path-list)
  "Resolve path wildcards in PATH-LIST."
  (let (path-list-1 path-list-2 path-beg path-end dir)
    ;; eliminate non-existent directories
    (while path-list
      (setq dir (car path-list))
      (string-match "\\(-r \\)?\\(\\([^?*]*[/\\]\\)*\\)" dir)
      (if (file-directory-p (match-string 2 dir))
	  (setq path-list-1 (cons dir path-list-1))
	(vhdl-warning-when-idle "No such directory: \"%s\"" (match-string 2 dir)))
      (setq path-list (cdr path-list)))
    ;; resolve path wildcards
    (while path-list-1
      (setq dir (car path-list-1))
      (if (string-match "\\(-r \\)?\\(\\([^?*]*[/\\]\\)*\\)\\([^/\\]*[?*][^/\\]*\\)\\([/\\].*\\)" dir)
	  (progn
	    (setq path-beg (match-string 1 dir)
		  path-end (match-string 5 dir))
	    (setq path-list-1
		  (append
		   (mapcar
		    (function
		     (lambda (var) (concat path-beg var path-end)))
		    (let ((all-list (vhdl-directory-files
				     (match-string 2 dir) t
				     (concat "\\<" (wildcard-to-regexp
						    (match-string 4 dir)))))
			  dir-list)
		      (while all-list
			(when (file-directory-p (car all-list))
			  (setq dir-list (cons (car all-list) dir-list)))
			(setq all-list (cdr all-list)))
		      dir-list))
		   (cdr path-list-1))))
	(string-match "\\(-r \\)?\\(.*\\)[/\\].*" dir)
	(when (file-directory-p (match-string 2 dir))
	  (setq path-list-2 (cons dir path-list-2)))
	(setq path-list-1 (cdr path-list-1))))
    (nreverse path-list-2)))

(defun vhdl-speedbar-goto-this-unit (directory unit)
  "If UNIT is displayed in DIRECTORY, goto this line and return t, else nil."
  (let ((dest (point)))
    (if (and (if vhdl-speedbar-show-projects
		 (progn (goto-char (point-min)) t)
	       (speedbar-goto-this-file directory))
	     (re-search-forward (concat "[]}] " unit "\\>") nil t))
	(progn (speedbar-position-cursor-on-line)
	       t)
      (goto-char dest)
      nil)))

(defun vhdl-speedbar-find-file (text token indent)
  "When user clicks on TEXT, load file with name and position in TOKEN.
Jump to the design unit if `vhdl-speedbar-jump-to-unit' is t or if the file
is already shown in a buffer."
  (if (not (car token))
      (error "ERROR:  File cannot be found")
    (let ((buffer (get-file-buffer (car token))))
      (speedbar-find-file-in-frame (car token))
      (when (or vhdl-speedbar-jump-to-unit buffer)
	(goto-char (point-min))
	(forward-line (1- (cdr token)))
	(recenter))
      (vhdl-speedbar-update-current-unit t t)
      (speedbar-set-timer dframe-update-speed)
      (speedbar-maybee-jump-to-attached-frame))))

(defun vhdl-speedbar-port-copy ()
  "Copy the port of the entity/component or subprogram under the cursor."
  (interactive)
  (let ((is-entity (vhdl-speedbar-check-unit 'entity)))
    (if (not (or is-entity (vhdl-speedbar-check-unit 'subprogram)))
	(error "ERROR:  No entity/component or subprogram under cursor")
      (beginning-of-line)
      (if (looking-at "\\([0-9]\\)+:\\s-*\\(\\[[-+?]\\]\\|>\\) \\(\\(\\w\\|\\s_\\)+\\)")
	  (condition-case info
	      (let ((token (get-text-property
			    (match-beginning 3) 'speedbar-token)))
		(vhdl-visit-file (car token) t
				 (progn (goto-char (point-min))
					(forward-line (1- (cdr token)))
					(end-of-line)
					(if is-entity
					    (vhdl-port-copy)
					  (vhdl-subprog-copy)))))
	    (error (error "ERROR:  %s not scanned successfully\n  (%s)"
			  (if is-entity "Port" "Interface") (cadr info))))
	(error "ERROR:  No entity/component or subprogram on current line")))))

(defun vhdl-speedbar-place-component ()
  "Place the entity/component under the cursor as component."
  (interactive)
  (if (not (vhdl-speedbar-check-unit 'entity))
      (error "ERROR:  No entity/component under cursor")
    (vhdl-speedbar-port-copy)
    (if (fboundp 'speedbar-select-attached-frame)
	(speedbar-select-attached-frame)
      (select-frame speedbar-attached-frame))
    (vhdl-compose-place-component)
    (select-frame speedbar-frame)))

(defun vhdl-speedbar-configuration ()
  "Generate configuration for the architecture under the cursor."
  (interactive)
  (if (not (vhdl-speedbar-check-unit 'architecture))
      (error "ERROR:  No architecture under cursor")
    (let ((arch-name (vhdl-speedbar-line-text))
	  (ent-name (vhdl-speedbar-higher-text)))
      (if (fboundp 'speedbar-select-attached-frame)
	  (speedbar-select-attached-frame)
	(select-frame speedbar-attached-frame))
      (vhdl-compose-configuration ent-name arch-name))))

(defun vhdl-speedbar-select-mra ()
  "Select the architecture under the cursor as MRA."
  (interactive)
  (if (not (vhdl-speedbar-check-unit 'architecture))
      (error "ERROR:  No architecture under cursor")
    (let* ((arch-key (downcase (vhdl-speedbar-line-text)))
	   (ent-key (downcase (vhdl-speedbar-higher-text)))
	   (ent-alist (aget vhdl-entity-alist
			    (or (vhdl-project-p) default-directory) t))
	   (ent-entry (aget ent-alist ent-key t)))
      (setcar (cddr (cddr ent-entry)) arch-key) ; (nth 4 ent-entry)
      (speedbar-refresh))))

(defun vhdl-speedbar-make-design ()
  "Make (compile) design unit or directory/project under the cursor."
  (interactive)
  (if (not (save-excursion (beginning-of-line)
			   (looking-at "[0-9]+: *\\(\\(\\[\\)\\|<\\)")))
      (error "ERROR:  No primary design unit or directory/project under cursor")
    (let ((is-unit (match-string 2))
	  (unit-name (vhdl-speedbar-line-text))
	  (vhdl-project (vhdl-speedbar-line-project))
	  (directory (file-name-as-directory
		      (or (speedbar-line-file) (speedbar-line-directory)))))
      (if (fboundp 'speedbar-select-attached-frame)
	  (speedbar-select-attached-frame)
	(select-frame speedbar-attached-frame))
      (let ((default-directory directory))
	(vhdl-make (and is-unit unit-name))))))

(defun vhdl-speedbar-generate-makefile ()
  "Generate Makefile for directory/project under the cursor."
  (interactive)
  (let ((vhdl-project (vhdl-speedbar-line-project))
	(default-directory (file-name-as-directory
			    (or (speedbar-line-file) (speedbar-line-directory)))))
    (vhdl-generate-makefile)))

(defun vhdl-speedbar-check-unit (design-unit)
  "Check whether design unit under cursor corresponds to DESIGN-UNIT (or its
expansion function)."
    (save-excursion
      (speedbar-position-cursor-on-line)
      (cond ((eq design-unit 'entity)
	     (memq (get-text-property (match-end 0) 'face)
		   '(vhdl-speedbar-entity-face
		     vhdl-speedbar-entity-selected-face)))
	    ((eq design-unit 'architecture)
	     (memq (get-text-property (match-end 0) 'face)
		   '(vhdl-speedbar-architecture-face
		     vhdl-speedbar-architecture-selected-face)))
	    ((eq design-unit 'subprogram)
	     (eq (get-text-property (match-end 0) 'face)
		 'vhdl-speedbar-subprogram-face))
	    (t nil))))

(defun vhdl-speedbar-set-depth (depth)
  "Set hierarchy display depth to DEPTH and refresh speedbar."
  (setq vhdl-speedbar-hierarchy-depth depth)
  (speedbar-refresh))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fontification

(defface vhdl-speedbar-entity-face
  '((((class color) (background light)) (:foreground "ForestGreen"))
    (((class color) (background dark)) (:foreground "PaleGreen")))
  "Face used for displaying entity names."
  :group 'speedbar-faces)

(defface vhdl-speedbar-architecture-face
  '((((min-colors 88) (class color) (background light)) (:foreground "Blue1"))
    (((class color) (background light)) (:foreground "Blue"))

    (((class color) (background dark)) (:foreground "LightSkyBlue")))
  "Face used for displaying architecture names."
  :group 'speedbar-faces)

(defface vhdl-speedbar-configuration-face
  '((((class color) (background light)) (:foreground "DarkGoldenrod"))
    (((class color) (background dark)) (:foreground "Salmon")))
  "Face used for displaying configuration names."
  :group 'speedbar-faces)

(defface vhdl-speedbar-package-face
  '((((class color) (background light)) (:foreground "Grey50"))
    (((class color) (background dark)) (:foreground "Grey80")))
  "Face used for displaying package names."
  :group 'speedbar-faces)

(defface vhdl-speedbar-library-face
  '((((class color) (background light)) (:foreground "Purple"))
    (((class color) (background dark)) (:foreground "Orchid1")))
  "Face used for displaying library names."
  :group 'speedbar-faces)

(defface vhdl-speedbar-instantiation-face
  '((((class color) (background light)) (:foreground "Brown"))
    (((min-colors 88) (class color) (background dark)) (:foreground "Yellow1"))
    (((class color) (background dark)) (:foreground "Yellow")))
  "Face used for displaying instantiation names."
  :group 'speedbar-faces)

(defface vhdl-speedbar-subprogram-face
  '((((class color) (background light)) (:foreground "Orchid4"))
    (((class color) (background dark)) (:foreground "BurlyWood2")))
  "Face used for displaying subprogram names."
  :group 'speedbar-faces)

(defface vhdl-speedbar-entity-selected-face
  '((((class color) (background light)) (:foreground "ForestGreen" :underline t))
    (((class color) (background dark)) (:foreground "PaleGreen" :underline t)))
  "Face used for displaying entity names."
  :group 'speedbar-faces)

(defface vhdl-speedbar-architecture-selected-face
  '((((min-colors 88) (class color) (background light)) (:foreground
  "Blue1" :underline t))
    (((class color) (background light)) (:foreground "Blue" :underline t))
    (((class color) (background dark)) (:foreground "LightSkyBlue" :underline t)))
  "Face used for displaying architecture names."
  :group 'speedbar-faces)

(defface vhdl-speedbar-configuration-selected-face
  '((((class color) (background light)) (:foreground "DarkGoldenrod" :underline t))
    (((class color) (background dark)) (:foreground "Salmon" :underline t)))
  "Face used for displaying configuration names."
  :group 'speedbar-faces)

(defface vhdl-speedbar-package-selected-face
  '((((class color) (background light)) (:foreground "Grey50" :underline t))
    (((class color) (background dark)) (:foreground "Grey80" :underline t)))
  "Face used for displaying package names."
  :group 'speedbar-faces)

(defface vhdl-speedbar-instantiation-selected-face
  '((((class color) (background light)) (:foreground "Brown" :underline t))
    (((class color) (background dark)) (:foreground "Yellow" :underline t)))
  "Face used for displaying instantiation names."
  :group 'speedbar-faces)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization

;; add speedbar
(when (fboundp 'speedbar)
  (condition-case ()
      (when (and vhdl-speedbar-auto-open
		 (not (and (boundp 'speedbar-frame)
			   (frame-live-p speedbar-frame))))
	(speedbar-frame-mode 1)
	(if (fboundp 'speedbar-select-attached-frame)
	    (speedbar-select-attached-frame)
	  (select-frame speedbar-attached-frame)))
    (error (vhdl-warning-when-idle "ERROR:  An error occurred while opening speedbar"))))

;; initialize speedbar
(if (not (boundp 'speedbar-frame))
    (add-hook 'speedbar-load-hook 'vhdl-speedbar-initialize)
  (vhdl-speedbar-initialize)
  (when speedbar-frame (vhdl-speedbar-refresh)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Structural composition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vhdl-get-components-package-name ()
  "Return the name of the components package."
  (let ((project (vhdl-project-p)))
    (if project
	(vhdl-replace-string (car vhdl-components-package-name)
			     (subst-char-in-string ?  ?_ project))
      (cdr vhdl-components-package-name))))

(defun vhdl-compose-new-component ()
  "Create entity and architecture for new component."
  (interactive)
  (let* ((case-fold-search t)
	 (ent-name (read-from-minibuffer "entity name: "
					 nil vhdl-minibuffer-local-map))
	 (arch-name
	  (if (equal (cdr vhdl-compose-architecture-name) "")
	      (read-from-minibuffer "architecture name: "
				    nil vhdl-minibuffer-local-map)
	    (vhdl-replace-string vhdl-compose-architecture-name ent-name)))
	 ent-file-name arch-file-name ent-buffer arch-buffer project)
    (message "Creating component \"%s(%s)\"..." ent-name arch-name)
    ;; open entity file
    (unless (eq vhdl-compose-create-files 'none)
      (setq ent-file-name
	    (concat (vhdl-replace-string vhdl-entity-file-name ent-name t)
		    "." (file-name-extension (buffer-file-name))))
      (when (and (file-exists-p ent-file-name)
		 (not (y-or-n-p (concat "File \"" ent-file-name
					"\" exists; overwrite? "))))
	(error "ERROR:  Creating component...aborted"))
      (find-file ent-file-name)
      (erase-buffer)
      (set-buffer-modified-p nil))
    ;; insert header
    (if vhdl-compose-include-header
	(progn (vhdl-template-header)
	       (goto-char (point-max)))
      (vhdl-comment-display-line) (insert "\n\n"))
    ;; insert library clause
    (vhdl-template-package-std-logic-1164)
    (when vhdl-use-components-package
      (insert "\n")
      (vhdl-template-standard-package (vhdl-work-library)
				      (vhdl-get-components-package-name)))
    (insert "\n\n") (vhdl-comment-display-line) (insert "\n\n")
    ;; insert entity declaration
    (vhdl-insert-keyword "ENTITY ") (insert ent-name)
    (vhdl-insert-keyword " IS\n")
    (when (memq vhdl-insert-empty-lines '(unit all)) (insert "\n"))
    (indent-to vhdl-basic-offset) (vhdl-insert-keyword "GENERIC (\n")
    (indent-to (* 2 vhdl-basic-offset)) (insert ");\n")
    (when (memq vhdl-insert-empty-lines '(unit all)) (insert "\n"))
    (indent-to vhdl-basic-offset) (vhdl-insert-keyword "PORT (\n")
    (indent-to (* 2 vhdl-basic-offset)) (insert ");\n")
    (when (memq vhdl-insert-empty-lines '(unit all)) (insert "\n"))
    (vhdl-insert-keyword "END ")
    (unless (vhdl-standard-p '87) (vhdl-insert-keyword "ENTITY "))
    (insert ent-name ";\n\n")
    (vhdl-comment-display-line) (insert "\n")
    ;; open architecture file
    (if (not (eq vhdl-compose-create-files 'separate))
	(insert "\n")
      (setq ent-buffer (current-buffer))
      (setq arch-file-name
	    (concat (vhdl-replace-string vhdl-architecture-file-name
					 (concat ent-name " " arch-name) t)
		    "." (file-name-extension (buffer-file-name))))
      (when (and (file-exists-p arch-file-name)
		 (not (y-or-n-p (concat "File \"" arch-file-name
					"\" exists; overwrite? "))))
	(error "ERROR:  Creating component...aborted"))
      (find-file arch-file-name)
      (erase-buffer)
      (set-buffer-modified-p nil)
      ;; insert header
      (if vhdl-compose-include-header
	  (progn (vhdl-template-header)
		 (goto-char (point-max)))
	(vhdl-comment-display-line) (insert "\n\n")))
    ;; insert architecture body
    (vhdl-insert-keyword "ARCHITECTURE ") (insert arch-name)
    (vhdl-insert-keyword " OF ") (insert ent-name)
    (vhdl-insert-keyword " IS\n\n")
    (indent-to vhdl-basic-offset) (vhdl-comment-display-line) (insert "\n")
    (indent-to vhdl-basic-offset) (insert "-- Internal signal declarations\n")
    (indent-to vhdl-basic-offset) (vhdl-comment-display-line) (insert "\n\n")
    (unless (or vhdl-use-components-package (vhdl-use-direct-instantiation))
      (indent-to vhdl-basic-offset) (vhdl-comment-display-line) (insert "\n")
      (indent-to vhdl-basic-offset) (insert "-- Component declarations\n")
      (indent-to vhdl-basic-offset) (vhdl-comment-display-line) (insert "\n\n"))
    (vhdl-insert-keyword "BEGIN")
    (when vhdl-self-insert-comments
      (insert "  -- ")
      (unless (vhdl-standard-p '87) (vhdl-insert-keyword "ARCHITECTURE "))
      (insert arch-name))
    (insert "\n\n")
    (indent-to vhdl-basic-offset) (vhdl-comment-display-line) (insert "\n")
    (indent-to vhdl-basic-offset) (insert "-- Component instantiations\n")
    (indent-to vhdl-basic-offset) (vhdl-comment-display-line) (insert "\n\n")
    (vhdl-insert-keyword "END ")
    (unless (vhdl-standard-p '87) (vhdl-insert-keyword "ARCHITECTURE "))
    (insert arch-name ";\n\n")
    ;; insert footer and save
    (if (and vhdl-compose-include-header (not (equal vhdl-file-footer "")))
	(vhdl-template-footer)
      (vhdl-comment-display-line) (insert "\n"))
    (goto-char (point-min))
    (setq arch-buffer (current-buffer))
    (when ent-buffer (set-buffer ent-buffer) (save-buffer))
    (set-buffer arch-buffer) (save-buffer)
    (message "%s"
     (concat (format "Creating component \"%s(%s)\"...done" ent-name arch-name)
	     (and ent-file-name
		  (format "\n  File created: \"%s\"" ent-file-name))
	     (and arch-file-name
		  (format "\n  File created: \"%s\"" arch-file-name))))))

(defun vhdl-compose-place-component ()
  "Place new component by pasting current port as component declaration and
component instantiation."
  (interactive)
  (if (not vhdl-port-list)
      (error "ERROR:  No port has been read")
    (save-excursion
      (vhdl-prepare-search-2
       (unless (or (re-search-backward "^architecture[ \t\n]+\\w+[ \t\n]+of[ \t\n]+\\(\\w+\\)[ \t\n]+is\\>" nil t)
		   (re-search-forward "^architecture[ \t\n]+\\w+[ \t\n]+of[ \t\n]+\\(\\w+\\)[ \t\n]+is\\>" nil t))
	 (error "ERROR:  No architecture found"))
       (let* ((ent-name (match-string 1))
	      (ent-file-name
	       (concat (vhdl-replace-string vhdl-entity-file-name ent-name t)
		       "." (file-name-extension (buffer-file-name))))
	      (orig-buffer (current-buffer)))
	 (message "Placing component \"%s\"..." (nth 0 vhdl-port-list))
	 ;; place component declaration
	 (unless (or vhdl-use-components-package
		     (vhdl-use-direct-instantiation)
		     (save-excursion
		       (re-search-forward
			(concat "^\\s-*component\\s-+"
				(car vhdl-port-list) "\\>") nil t)))
	   (re-search-forward "^begin\\>" nil)
	   (beginning-of-line)
	   (skip-chars-backward " \t\n")
	   (insert "\n\n") (indent-to vhdl-basic-offset)
	   (vhdl-port-paste-component t))
	 ;; place component instantiation
	 (re-search-forward "^end\\>" nil)
	 (beginning-of-line)
	 (skip-chars-backward " \t\n")
	 (insert "\n\n") (indent-to vhdl-basic-offset)
	 (vhdl-port-paste-instance nil t t)
	 ;; place use clause for used packages
	 (when (nth 3 vhdl-port-list)
	   ;; open entity file
	   (when (file-exists-p ent-file-name)
	     (find-file ent-file-name))
	   (goto-char (point-min))
	   (unless (re-search-forward (concat "^entity[ \t\n]+" ent-name "[ \t\n]+is\\>") nil t)
	     (error "ERROR:  Entity not found: \"%s\"" ent-name))
	   (goto-char (match-beginning 0))
	   (if (and (save-excursion
		      (re-search-backward "^\\(library\\|use\\)\\|end\\>" nil t))
		    (match-string 1))
	       (progn (goto-char (match-end 0))
		      (beginning-of-line 2))
	     (insert "\n")
	     (backward-char))
	   (vhdl-port-paste-context-clause)
	   (switch-to-buffer orig-buffer))
	 (message "Placing component \"%s\"...done" (nth 0 vhdl-port-list)))))))

(defun vhdl-compose-wire-components ()
  "Connect components."
  (interactive)
  (save-excursion
    (vhdl-prepare-search-2
     (unless (or (re-search-backward "^architecture[ \t\n]+\\w+[ \t\n]+of[ \t\n]+\\(\\w+\\)[ \t\n]+is\\>" nil t)
		 (re-search-forward "^architecture[ \t\n]+\\w+[ \t\n]+of[ \t\n]+\\(\\w+\\)[ \t\n]+is\\>" nil t))
       (error "ERROR:  No architecture found"))
     (let* ((ent-name (match-string 1))
	    (ent-file-name
	     (concat (vhdl-replace-string vhdl-entity-file-name ent-name t)
		     "." (file-name-extension (buffer-file-name))))
	    (arch-decl-pos (point-marker))
	    (arch-stat-pos (re-search-forward "^begin\\>" nil))
	    (arch-end-pos (re-search-forward "^end\\>" nil))
	    (pack-name (vhdl-get-components-package-name))
	    (pack-file-name
	     (concat (vhdl-replace-string vhdl-package-file-name pack-name t)
		     "." (file-name-extension (buffer-file-name))))
	    inst-name comp-name comp-ent-name comp-ent-file-name has-generic
	    port-alist generic-alist inst-alist
	    signal-name signal-entry signal-alist local-list written-list
	    single-in-list multi-in-list single-out-list multi-out-list
	    constant-name constant-entry constant-alist single-list multi-list
	    port-beg-pos port-in-pos port-out-pos port-inst-pos port-end-pos
	    generic-beg-pos generic-pos generic-inst-pos generic-end-pos
	    signal-beg-pos signal-pos
	    constant-temp-pos port-temp-pos signal-temp-pos)
       (message "Wiring components...")
       ;; process all instances
       (goto-char arch-stat-pos)
       (while (re-search-forward
	       (concat "^[ \t]*\\(\\w+\\)[ \t\n]*:[ \t\n]*\\("
		       "\\(component[ \t\n]+\\)?\\(\\w+\\)"
		       "[ \t\n]+\\(--[^\n]*\n[ \t\n]*\\)*\\(\\(generic\\)\\|port\\)[ \t\n]+map\\|"
		       "\\(\\(entity\\)\\|configuration\\)[ \t\n]+\\(\\(\\w+\\)\\.\\)?\\(\\w+\\)\\([ \t\n]*(\\(\\w+\\))\\)?"
		       "[ \t\n]+\\(--[^\n]*\n[ \t\n]*\\)*\\(\\(generic\\)\\|port\\)[ \t\n]+map\\)[ \t\n]*(") arch-end-pos t)
	 (setq inst-name (match-string-no-properties 1)
	       comp-name (match-string-no-properties 4)
	       comp-ent-name (match-string-no-properties 12)
	       has-generic (or (match-string 7) (match-string 17)))
	 ;; get port ...
	 (if comp-name
	     ;; ... from component declaration
	     (vhdl-visit-file
	      (when vhdl-use-components-package pack-file-name) t
	      (save-excursion
		(goto-char (point-min))
		(unless (re-search-forward (concat "^\\s-*component[ \t\n]+" comp-name "\\>") nil t)
		  (error "ERROR:  Component declaration not found: \"%s\"" comp-name))
		(vhdl-port-copy)))
	   ;; ... from entity declaration (direct instantiation)
	   (setq comp-ent-file-name
		 (concat (vhdl-replace-string vhdl-entity-file-name comp-ent-name t)
			 "." (file-name-extension (buffer-file-name))))
	   (vhdl-visit-file
	    comp-ent-file-name t
	    (save-excursion
	      (goto-char (point-min))
	      (unless (re-search-forward (concat "^\\s-*entity[ \t\n]+" comp-ent-name "\\>") nil t)
		(error "ERROR:  Entity declaration not found: \"%s\"" comp-ent-name))
	      (vhdl-port-copy))))
	 (vhdl-port-flatten t)
	 (setq generic-alist (nth 1 vhdl-port-list)
	       port-alist (nth 2 vhdl-port-list)
	       vhdl-port-list nil)
	 (setq constant-alist nil
	       signal-alist nil)
	 (when has-generic
	   ;; process all constants in generic map
	   (vhdl-forward-syntactic-ws)
	   (while (vhdl-parse-string "\\(\\(\\w+\\)[ \t\n]*=>[ \t\n]*\\)?\\(\\w+\\),?" t)
	     (setq constant-name (match-string-no-properties 3))
	     (setq constant-entry
		   (cons constant-name
			 (if (match-string 1)
			     (or (aget generic-alist (match-string 2) t)
				 (error "ERROR:  Formal generic \"%s\" mismatch for instance \"%s\"" (match-string 2) inst-name))
			   (cdar generic-alist))))
	     (setq constant-alist (cons constant-entry constant-alist))
	     (setq constant-name (downcase constant-name))
	     (if (or (member constant-name single-list)
		     (member constant-name multi-list))
		 (progn (setq single-list (delete constant-name single-list))
			(add-to-list 'multi-list constant-name))
	       (add-to-list 'single-list constant-name))
	     (unless (match-string 1)
	       (setq generic-alist (cdr generic-alist)))
	     (vhdl-forward-syntactic-ws))
	   (vhdl-re-search-forward "\\<port\\s-+map[ \t\n]*(" nil t))
	 ;; process all signals in port map
	 (vhdl-forward-syntactic-ws)
	 (while (vhdl-parse-string "\\(\\(\\w+\\)[ \t\n]*=>[ \t\n]*\\)?\\(\\w+\\),?" t)
	   (setq signal-name (match-string-no-properties 3))
	   (setq signal-entry (cons signal-name
				    (if (match-string 1)
					(or (aget port-alist (match-string 2) t)
					    (error "ERROR:  Formal port \"%s\" mismatch for instance \"%s\"" (match-string 2) inst-name))
				      (cdar port-alist))))
	   (setq signal-alist (cons signal-entry signal-alist))
	   (setq signal-name (downcase signal-name))
	   (if (equal (upcase (nth 2 signal-entry)) "IN")
	       ;; input signal
	       (cond
		((member signal-name local-list)
		 nil)
		((or (member signal-name single-out-list)
		     (member signal-name multi-out-list))
		 (setq single-out-list (delete signal-name single-out-list))
		 (setq multi-out-list (delete signal-name multi-out-list))
		 (add-to-list 'local-list signal-name))
		((member signal-name single-in-list)
		 (setq single-in-list (delete signal-name single-in-list))
		 (add-to-list 'multi-in-list signal-name))
		((not (member signal-name multi-in-list))
		 (add-to-list 'single-in-list signal-name)))
	     ;; output signal
	     (cond
	      ((member signal-name local-list)
	       nil)
	      ((or (member signal-name single-in-list)
		   (member signal-name multi-in-list))
	       (setq single-in-list (delete signal-name single-in-list))
	       (setq multi-in-list (delete signal-name multi-in-list))
	       (add-to-list 'local-list signal-name))
	      ((member signal-name single-out-list)
	       (setq single-out-list (delete signal-name single-out-list))
	       (add-to-list 'multi-out-list signal-name))
	      ((not (member signal-name multi-out-list))
	       (add-to-list 'single-out-list signal-name))))
	   (unless (match-string 1)
	     (setq port-alist (cdr port-alist)))
	   (vhdl-forward-syntactic-ws))
	 (setq inst-alist (cons (list inst-name (nreverse constant-alist)
				      (nreverse signal-alist)) inst-alist)))
       ;; prepare signal insertion
       (vhdl-goto-marker arch-decl-pos)
       (forward-line 1)
       (re-search-forward "^\\s-*-- Internal signal declarations[ \t\n]*-*\n" arch-stat-pos t)
       (setq signal-pos (point-marker))
       (while (progn (vhdl-forward-syntactic-ws)
		     (looking-at "signal\\>"))
	 (beginning-of-line 2)
	 (delete-region signal-pos (point)))
       (setq signal-beg-pos signal-pos)
       ;; open entity file
       (when (file-exists-p ent-file-name)
	 (find-file ent-file-name))
       (goto-char (point-min))
       (unless (re-search-forward (concat "^entity[ \t\n]+" ent-name "[ \t\n]+is\\>") nil t)
	 (error "ERROR:  Entity not found: \"%s\"" ent-name))
       ;; prepare generic clause insertion
       (unless (and (re-search-forward "\\(^\\s-*generic[ \t\n]*(\\)\\|^end\\>" nil t)
		    (match-string 1))
	 (goto-char (match-beginning 0))
	 (indent-to vhdl-basic-offset)
	 (insert "generic ();\n\n")
	 (backward-char 4))
       (backward-char)
       (setq generic-pos (point-marker))
       (forward-sexp) (end-of-line)
       (delete-region generic-pos (point)) (delete-char 1)
       (insert "(\n")
       (when multi-list
	 (insert "\n")
	 (indent-to (* 2 vhdl-basic-offset))
	 (insert "-- global generics\n"))
       (setq generic-beg-pos (point-marker) generic-pos (point-marker)
	     generic-inst-pos (point-marker) generic-end-pos (point-marker))
       ;; prepare port clause insertion
       (unless (and (re-search-forward "\\(^\\s-*port[ \t\n]*(\\)\\|^end\\>" nil t)
		    (match-string 1))
	 (goto-char (match-beginning 0))
	 (indent-to vhdl-basic-offset)
	 (insert "port ();\n\n")
	 (backward-char 4))
       (backward-char)
       (setq port-in-pos (point-marker))
       (forward-sexp) (end-of-line)
       (delete-region port-in-pos (point)) (delete-char 1)
       (insert "(\n")
       (when (or multi-in-list multi-out-list)
	 (insert "\n")
	 (indent-to (* 2 vhdl-basic-offset))
	 (insert "-- global ports\n"))
       (setq port-beg-pos (point-marker) port-in-pos (point-marker)
	     port-out-pos (point-marker) port-inst-pos (point-marker)
	     port-end-pos (point-marker))
       ;; insert generics, ports and signals
       (setq inst-alist (nreverse inst-alist))
       (while inst-alist
	 (setq inst-name (nth 0 (car inst-alist))
	       constant-alist (nth 1 (car inst-alist))
	       signal-alist (nth 2 (car inst-alist))
	       constant-temp-pos generic-inst-pos
	       port-temp-pos port-inst-pos
	       signal-temp-pos signal-pos)
	 ;; generics
	 (while constant-alist
	   (setq constant-name (downcase (caar constant-alist))
		 constant-entry (car constant-alist))
	   (cond ((member constant-name written-list)
		  nil)
		 ((member constant-name multi-list)
		  (vhdl-goto-marker generic-pos)
		  (setq generic-end-pos
			(vhdl-max-marker
			 generic-end-pos
			 (vhdl-compose-insert-generic constant-entry)))
		  (setq generic-pos (point-marker))
		  (add-to-list 'written-list constant-name))
		 (t
		  (vhdl-goto-marker
		   (vhdl-max-marker generic-inst-pos generic-pos))
		  (setq generic-end-pos
			(vhdl-compose-insert-generic constant-entry))
		  (setq generic-inst-pos (point-marker))
		  (add-to-list 'written-list constant-name)))
	   (setq constant-alist (cdr constant-alist)))
	 (when (/= constant-temp-pos generic-inst-pos)
	   (vhdl-goto-marker (vhdl-max-marker constant-temp-pos generic-pos))
	   (insert "\n") (indent-to (* 2 vhdl-basic-offset))
	   (insert "-- generics for \"" inst-name "\"\n")
	   (vhdl-goto-marker generic-inst-pos))
	 ;; ports and signals
	 (while signal-alist
	   (setq signal-name (downcase (caar signal-alist))
		 signal-entry (car signal-alist))
	   (cond ((member signal-name written-list)
		  nil)
		 ((member signal-name multi-in-list)
		  (vhdl-goto-marker port-in-pos)
		  (setq port-end-pos
			(vhdl-max-marker
			 port-end-pos (vhdl-compose-insert-port signal-entry)))
		  (setq port-in-pos (point-marker))
		  (add-to-list 'written-list signal-name))
		 ((member signal-name multi-out-list)
		  (vhdl-goto-marker (vhdl-max-marker port-out-pos port-in-pos))
		  (setq port-end-pos
			(vhdl-max-marker
			 port-end-pos (vhdl-compose-insert-port signal-entry)))
		  (setq port-out-pos (point-marker))
		  (add-to-list 'written-list signal-name))
		 ((or (member signal-name single-in-list)
		      (member signal-name single-out-list))
		  (vhdl-goto-marker
		   (vhdl-max-marker
		    port-inst-pos
		    (vhdl-max-marker port-out-pos port-in-pos)))
		  (setq port-end-pos (vhdl-compose-insert-port signal-entry))
		  (setq port-inst-pos (point-marker))
		  (add-to-list 'written-list signal-name))
		 ((equal (upcase (nth 2 signal-entry)) "OUT")
		  (vhdl-goto-marker signal-pos)
		  (vhdl-compose-insert-signal signal-entry)
		  (setq signal-pos (point-marker))
		  (add-to-list 'written-list signal-name)))
	   (setq signal-alist (cdr signal-alist)))
	 (when (/= port-temp-pos port-inst-pos)
	   (vhdl-goto-marker
	    (vhdl-max-marker port-temp-pos
			     (vhdl-max-marker port-in-pos port-out-pos)))
	   (insert "\n") (indent-to (* 2 vhdl-basic-offset))
	   (insert "-- ports to \"" inst-name "\"\n")
	   (vhdl-goto-marker port-inst-pos))
	 (when (/= signal-temp-pos signal-pos)
	   (vhdl-goto-marker signal-temp-pos)
	   (insert "\n") (indent-to vhdl-basic-offset)
	   (insert "-- outputs of \"" inst-name "\"\n")
	   (vhdl-goto-marker signal-pos))
	 (setq inst-alist (cdr inst-alist)))
       ;; finalize generic/port clause
       (vhdl-goto-marker generic-end-pos) (backward-char)
       (when (= generic-beg-pos generic-end-pos)
	 (insert "\n") (indent-to (* 2 vhdl-basic-offset))
	 (insert ";") (backward-char))
       (insert ")")
       (vhdl-goto-marker port-end-pos) (backward-char)
       (when (= port-beg-pos port-end-pos)
	 (insert "\n") (indent-to (* 2 vhdl-basic-offset))
	 (insert ";") (backward-char))
       (insert ")")
       ;; align everything
       (when vhdl-auto-align
	 (vhdl-goto-marker generic-beg-pos)
	 (vhdl-align-region-groups generic-beg-pos generic-end-pos 1)
	 (vhdl-align-region-groups port-beg-pos port-end-pos 1)
	 (vhdl-goto-marker signal-beg-pos)
	 (vhdl-align-region-groups signal-beg-pos signal-pos))
       (switch-to-buffer (marker-buffer signal-beg-pos))
       (message "Wiring components...done")))))

(defun vhdl-compose-insert-generic (entry)
  "Insert ENTRY as generic declaration."
  (let (pos)
    (indent-to (* 2 vhdl-basic-offset))
    (insert (nth 0 entry) " : " (nth 1 entry))
    (when (nth 2 entry)
      (insert " := " (nth 2 entry)))
    (insert ";")
    (setq pos (point-marker))
    (when (and vhdl-include-port-comments (nth 3 entry))
      (vhdl-comment-insert-inline (nth 3 entry) t))
    (insert "\n")
    pos))

(defun vhdl-compose-insert-port (entry)
  "Insert ENTRY as port declaration."
  (let (pos)
    (indent-to (* 2 vhdl-basic-offset))
    (insert (nth 0 entry) " : " (nth 2 entry) " " (nth 3 entry) ";")
    (setq pos (point-marker))
    (when (and vhdl-include-port-comments (nth 4 entry))
      (vhdl-comment-insert-inline (nth 4 entry) t))
    (insert "\n")
    pos))

(defun vhdl-compose-insert-signal (entry)
  "Insert ENTRY as signal declaration."
  (indent-to vhdl-basic-offset)
  (insert "signal " (nth 0 entry) " : " (nth 3 entry) ";")
  (when (and vhdl-include-port-comments (nth 4 entry))
    (vhdl-comment-insert-inline (nth 4 entry) t))
  (insert "\n"))

(defun vhdl-compose-components-package ()
  "Generate a package containing component declarations for all entities in the
current project/directory."
  (interactive)
  (vhdl-require-hierarchy-info)
  (let* ((project (vhdl-project-p))
	 (pack-name (vhdl-get-components-package-name))
	 (pack-file-name
	  (concat (vhdl-replace-string vhdl-package-file-name pack-name t)
		  "." (file-name-extension (buffer-file-name))))
	 (ent-alist (aget vhdl-entity-alist
			  (or project default-directory) t))
	 (lazy-lock-minimum-size 0)
	 clause-pos component-pos)
    (message "Generating components package \"%s\"..." pack-name)
    ;; open package file
    (when (and (file-exists-p pack-file-name)
	       (not (y-or-n-p (concat "File \"" pack-file-name
				      "\" exists; overwrite? "))))
      (error "ERROR:  Generating components package...aborted"))
    (find-file pack-file-name)
    (erase-buffer)
    ;; insert header
    (if vhdl-compose-include-header
	(progn (vhdl-template-header
		(concat "Components package (generated by Emacs VHDL Mode "
			vhdl-version ")"))
	       (goto-char (point-max)))
      (vhdl-comment-display-line) (insert "\n\n"))
    ;; insert std_logic_1164 package
    (vhdl-template-package-std-logic-1164)
    (insert "\n") (setq clause-pos (point-marker))
    (insert "\n") (vhdl-comment-display-line) (insert "\n\n")
    ;; insert package declaration
    (vhdl-insert-keyword "PACKAGE ") (insert pack-name)
    (vhdl-insert-keyword " IS\n\n")
    (indent-to vhdl-basic-offset) (vhdl-comment-display-line) (insert "\n")
    (indent-to vhdl-basic-offset) (insert "-- Component declarations\n")
    (indent-to vhdl-basic-offset) (vhdl-comment-display-line) (insert "\n\n")
    (indent-to vhdl-basic-offset)
    (setq component-pos (point-marker))
    (insert "\n\n") (vhdl-insert-keyword "END ")
    (unless (vhdl-standard-p '87) (vhdl-insert-keyword "PACKAGE "))
    (insert pack-name ";\n\n")
    ;; insert footer
    (if (and vhdl-compose-include-header (not (equal vhdl-file-footer "")))
	(vhdl-template-footer)
      (vhdl-comment-display-line) (insert "\n"))
    ;; insert component declarations
    (while ent-alist
      (vhdl-visit-file (nth 2 (car ent-alist)) nil
		       (progn (goto-char (point-min))
			      (forward-line (1- (nth 3 (car ent-alist))))
			      (end-of-line)
			      (vhdl-port-copy)))
      (goto-char component-pos)
      (vhdl-port-paste-component t)
      (when (cdr ent-alist) (insert "\n\n") (indent-to vhdl-basic-offset))
      (setq component-pos (point-marker))
      (goto-char clause-pos)
      (vhdl-port-paste-context-clause pack-name)
      (setq clause-pos (point-marker))
      (setq ent-alist (cdr ent-alist)))
    (goto-char (point-min))
    (save-buffer)
    (message "Generating components package \"%s\"...done\n  File created: \"%s\""
	     pack-name pack-file-name)))

(defun vhdl-compose-configuration-architecture (ent-name arch-name inst-alist
							 &optional insert-conf)
  "Generate block configuration for architecture."
  (let ((margin (current-indentation))
	(beg (point-at-bol))
	ent-entry inst-entry inst-path inst-prev-path cons-key tmp-alist)
    ;; insert block configuration (for architecture)
    (vhdl-insert-keyword "FOR ") (insert arch-name "\n")
    (setq margin (+ margin vhdl-basic-offset))
    ;; process all instances
    (while inst-alist
      (setq inst-entry (car inst-alist))
      ;; is component?
      (when (nth 4 inst-entry)
	(setq insert-conf t)
	(setq inst-path (nth 9 inst-entry))
	;; skip common path with previous instance
	(while (and inst-path (equal (car inst-path) (car inst-prev-path)))
	  (setq inst-path (cdr inst-path)
		inst-prev-path (cdr inst-prev-path)))
	;; insert block configuration end (for previous block/generate)
	(while inst-prev-path
	  (setq margin (- margin vhdl-basic-offset))
	  (indent-to margin)
	  (vhdl-insert-keyword "END FOR;\n")
	  (setq inst-prev-path (cdr inst-prev-path)))
	;; insert block configuration beginning (for current block/generate)
	(indent-to margin)
	(while inst-path
	  (setq margin (+ margin vhdl-basic-offset))
	  (vhdl-insert-keyword "FOR ")
	  (insert (car inst-path) "\n")
	  (indent-to margin)
	  (setq inst-path (cdr inst-path)))
	;; insert component configuration beginning
	(vhdl-insert-keyword "FOR ")
	(insert (nth 1 inst-entry) " : " (nth 4 inst-entry) "\n")
	;; find subconfiguration
	(setq conf-key (nth 7 inst-entry))
	(setq tmp-alist conf-alist)
	;; use first configuration found for instance's entity
	(while (and tmp-alist (null conf-key))
	  (when (equal (nth 5 inst-entry) (nth 4 (car tmp-alist)))
	    (setq conf-key (nth 0 (car tmp-alist))))
	  (setq tmp-alist (cdr tmp-alist)))
	(setq conf-entry (aget conf-alist conf-key t))
	;; insert binding indication ...
	;; ... with subconfiguration (if exists)
	(if (and vhdl-compose-configuration-use-subconfiguration conf-entry)
	    (progn
	      (indent-to (+ margin vhdl-basic-offset))
	      (vhdl-insert-keyword "USE CONFIGURATION ")
	      (insert (vhdl-work-library) "." (nth 0 conf-entry))
	      (insert ";\n"))
	  ;; ... with entity (if exists)
	  (setq ent-entry (aget ent-alist (nth 5 inst-entry) t))
	  (when ent-entry
	    (indent-to (+ margin vhdl-basic-offset))
	    (vhdl-insert-keyword "USE ENTITY ")
	    (insert (vhdl-work-library) "." (nth 0 ent-entry))
	    ;; insert architecture name (if architecture exists)
	    (when (nth 3 ent-entry)
	      (setq arch-name
		    ;; choose architecture name a) from configuration,
		    ;; b) from mra, or c) from first architecture
		    (or (nth 0 (aget (nth 3 ent-entry)
				     (or (nth 6 inst-entry)
					 (nth 4 ent-entry)) t))
			(nth 1 (car (nth 3 ent-entry)))))
	      (insert "(" arch-name ")"))
	    (insert ";\n")
	    ;; insert block configuration (for architecture of subcomponent)
	    (when (and vhdl-compose-configuration-hierarchical
		       (nth 3 ent-entry))
	      (indent-to (+ margin vhdl-basic-offset))
	      (vhdl-compose-configuration-architecture
	       (nth 0 ent-entry) arch-name
	       (nth 3 (aget (nth 3 ent-entry) (downcase arch-name) t))))))
	;; insert component configuration end
	(indent-to margin)
	(vhdl-insert-keyword "END FOR;\n")
	(setq inst-prev-path (nth 9 inst-entry)))
      (setq inst-alist (cdr inst-alist)))
    ;; insert block configuration end (for block/generate)
    (while inst-prev-path
      (setq margin (- margin vhdl-basic-offset))
      (indent-to margin)
      (vhdl-insert-keyword "END FOR;\n")
      (setq inst-prev-path (cdr inst-prev-path)))
    (indent-to (- margin vhdl-basic-offset))
    ;; insert block configuration end or remove beginning (for architecture)
    (if insert-conf
	(vhdl-insert-keyword "END FOR;\n")
      (delete-region beg (point)))))

(defun vhdl-compose-configuration (&optional ent-name arch-name)
  "Generate configuration declaration."
  (interactive)
  (vhdl-require-hierarchy-info)
  (let ((ent-alist (aget vhdl-entity-alist
			 (or (vhdl-project-p) default-directory) t))
	(conf-alist (aget vhdl-config-alist
			  (or (vhdl-project-p) default-directory) t))
	(from-speedbar ent-name)
	inst-alist conf-name conf-file-name pos)
    (vhdl-prepare-search-2
     ;; get entity and architecture name
     (unless ent-name
       (save-excursion
	 (unless (and (re-search-backward "^\\(architecture\\s-+\\(\\w+\\)\\s-+of\\s-+\\(\\w+\\)\\|end\\)\\>" nil t)
		      (not (equal "END" (upcase (match-string 1))))
		      (setq ent-name (match-string-no-properties 3))
		      (setq arch-name (match-string-no-properties 2)))
	   (error "ERROR:  Not within an architecture"))))
     (setq conf-name (vhdl-replace-string
		      vhdl-compose-configuration-name
		      (concat ent-name " " arch-name)))
     (setq inst-alist
	   (nth 3 (aget (nth 3 (aget ent-alist (downcase ent-name) t))
			(downcase arch-name) t))))
     (message "Generating configuration \"%s\"..." conf-name)
     (if vhdl-compose-configuration-create-file
	 ;; open configuration file
	 (progn
	   (setq conf-file-name
		 (concat (vhdl-replace-string vhdl-configuration-file-name
					      conf-name t)
			 "." (file-name-extension (buffer-file-name))))
	   (when (and (file-exists-p conf-file-name)
		      (not (y-or-n-p (concat "File \"" conf-file-name
					     "\" exists; overwrite? "))))
	     (error "ERROR:  Creating configuration...aborted"))
	   (find-file conf-file-name)
	   (erase-buffer)
	   (set-buffer-modified-p nil)
	   ;; insert header
	   (if vhdl-compose-include-header
	       (progn (vhdl-template-header
		       (concat "Configuration declaration for design \""
			       ent-name "(" arch-name ")\""))
		      (goto-char (point-max)))
	     (vhdl-comment-display-line) (insert "\n\n")))
       ;; goto end of architecture
       (unless from-speedbar
	 (re-search-forward "^end\\>" nil)
	 (end-of-line) (insert "\n\n")
	 (vhdl-comment-display-line) (insert "\n\n")))
     ;; insert library clause
     (setq pos (point))
     (vhdl-template-standard-package (vhdl-work-library) nil)
     (when (/= pos (point))
       (insert "\n\n"))
     ;; insert configuration
     (vhdl-insert-keyword "CONFIGURATION ") (insert conf-name)
     (vhdl-insert-keyword " OF ") (insert ent-name)
     (vhdl-insert-keyword " IS\n")
     (indent-to vhdl-basic-offset)
     ;; insert block configuration (for architecture)
     (vhdl-compose-configuration-architecture ent-name arch-name inst-alist t)
     (vhdl-insert-keyword "END ") (insert conf-name ";")
     (when conf-file-name
       ;; insert footer and save
       (insert "\n\n")
       (if (and vhdl-compose-include-header (not (equal vhdl-file-footer "")))
	   (vhdl-template-footer)
	 (vhdl-comment-display-line) (insert "\n"))
       (save-buffer))
     (message "%s"
      (concat (format "Generating configuration \"%s\"...done" conf-name)
	      (and conf-file-name
		   (format "\n  File created: \"%s\"" conf-file-name))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Compilation / Makefile generation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (using `compile.el')

(defun vhdl-makefile-name ()
  "Return the Makefile name of the current project or the current compiler if
no project is defined."
  (let ((project-alist (aget vhdl-project-alist vhdl-project))
	(compiler-alist (aget vhdl-compiler-alist vhdl-compiler)))
    (vhdl-replace-string
     (cons "\\(.*\\)\n\\(.*\\)"
	   (or (nth 8 project-alist) (nth 8 compiler-alist)))
     (concat (nth 9 compiler-alist) "\n" (nth 6 project-alist)))))

(defun vhdl-compile-directory ()
  "Return the directory where compilation/make should be run."
  (let* ((project (aget vhdl-project-alist (vhdl-project-p t)))
	 (compiler (aget vhdl-compiler-alist vhdl-compiler))
	 (directory (vhdl-resolve-env-variable
		     (if project
			 (vhdl-replace-string
			  (cons "\\(.*\\)" (nth 5 project)) (nth 9 compiler))
		       (nth 6 compiler)))))
    (file-name-as-directory
     (if (file-name-absolute-p directory)
	 directory
       (expand-file-name directory (vhdl-default-directory))))))

(defun vhdl-uniquify (in-list)
  "Remove duplicate elements from IN-LIST."
  (let (out-list)
    (while in-list
      (add-to-list 'out-list (car in-list))
      (setq in-list (cdr in-list)))
    out-list))

(defun vhdl-set-compiler (name)
  "Set current compiler to NAME."
  (interactive
   (list (let ((completion-ignore-case t))
	   (completing-read "Compiler name: " vhdl-compiler-alist nil t))))
  (if (assoc name vhdl-compiler-alist)
      (progn (setq vhdl-compiler name)
	     (message "Current compiler: \"%s\"" vhdl-compiler))
    (vhdl-warning (format "Unknown compiler: \"%s\"" name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compilation

(defun vhdl-compile-init ()
  "Initialize for compilation."
  (when (or (null compilation-error-regexp-alist)
	    (not (assoc (car (nth 11 (car vhdl-compiler-alist)))
			compilation-error-regexp-alist)))
    ;; `compilation-error-regexp-alist'
    (let ((commands-alist vhdl-compiler-alist)
	  regexp-alist sublist)
      (while commands-alist
	(setq sublist (nth 11 (car commands-alist)))
	(unless (or (equal "" (car sublist))
		    (assoc (car sublist) regexp-alist))
	  (setq regexp-alist (cons (list (nth 0 sublist)
					 (if (= 0 (nth 1 sublist))
					     (if (featurep 'xemacs) 9 nil)
					   (nth 1 sublist))
					 (nth 2 sublist) (nth 3 sublist))
				   regexp-alist)))
	(setq commands-alist (cdr commands-alist)))
      (setq compilation-error-regexp-alist
	    (append compilation-error-regexp-alist (nreverse regexp-alist))))
    ;; `compilation-file-regexp-alist'
    (let ((commands-alist vhdl-compiler-alist)
	  regexp-alist sublist)
      ;; matches vhdl-mode file name output
      (setq regexp-alist '(("^Compiling \"\\(.+\\)\"" 1)))
      (while commands-alist
	(setq sublist (nth 12 (car commands-alist)))
	(unless (or (equal "" (car sublist))
		    (assoc (car sublist) regexp-alist))
	  (setq regexp-alist (cons sublist regexp-alist)))
	(setq commands-alist (cdr commands-alist)))
      (setq compilation-file-regexp-alist
	    (append compilation-file-regexp-alist (nreverse regexp-alist))))))

(defvar vhdl-compile-file-name nil
  "Name of file to be compiled.")

(defun vhdl-compile-print-file-name ()
  "Function called within `compile' to print out file name for compilers that
do not print any file names."
  (insert "Compiling \"" vhdl-compile-file-name "\"\n"))

(defun vhdl-get-compile-options (project compiler file-name
					 &optional file-options-only)
  "Get compiler options.  Returning nil means do not compile this file."
  (let* ((compiler-options (nth 1 compiler))
	 (project-entry (aget (nth 4 project) vhdl-compiler))
	 (project-options (nth 0 project-entry))
	 (exception-list (and file-name (nth 2 project-entry)))
	 (work-library (vhdl-work-library))
	 (case-fold-search nil)
	 file-options)
    (while (and exception-list
		(not (string-match (caar exception-list) file-name)))
      (setq exception-list (cdr exception-list)))
    (if (and exception-list (not (cdar exception-list)))
	nil
      (if (and file-options-only (not exception-list))
	  'default
	(setq file-options (cdar exception-list))
	;; insert library name in compiler-specific options
	(setq compiler-options
	      (vhdl-replace-string (cons "\\(.*\\)" compiler-options)
				   work-library))
	;; insert compiler-specific options in project-specific options
	(when project-options
	  (setq project-options
		(vhdl-replace-string
		 (cons "\\(.*\\)\n\\(.*\\)" project-options)
		 (concat work-library "\n" compiler-options))))
	;; insert project-specific options in file-specific options
	(when file-options
	  (setq file-options
		(vhdl-replace-string
		 (cons "\\(.*\\)\n\\(.*\\)\n\\(.*\\)" file-options)
		 (concat work-library "\n" compiler-options "\n"
			 project-options))))
	;; return options
	(or file-options project-options compiler-options)))))

(defun vhdl-get-make-options (project compiler)
  "Get make options."
  (let* ((compiler-options (nth 3 compiler))
	 (project-entry (aget (nth 4 project) vhdl-compiler))
	 (project-options (nth 1 project-entry))
	 (makefile-name (vhdl-makefile-name)))
    ;; insert Makefile name in compiler-specific options
    (setq compiler-options
	  (vhdl-replace-string (cons "\\(.*\\)" (nth 3 compiler))
			       makefile-name))
    ;; insert compiler-specific options in project-specific options
    (when project-options
      (setq project-options
	    (vhdl-replace-string
	     (cons "\\(.*\\)\n\\(.*\\)" project-options)
	     (concat makefile-name "\n" compiler-options))))
    ;; return options
    (or project-options compiler-options)))

(defun vhdl-compile ()
  "Compile current buffer using the VHDL compiler specified in
`vhdl-compiler'."
  (interactive)
  (vhdl-compile-init)
  (let* ((project (aget vhdl-project-alist vhdl-project))
	 (compiler (or (aget vhdl-compiler-alist vhdl-compiler nil)
		       (error "ERROR:  No such compiler: \"%s\"" vhdl-compiler)))
	 (command (nth 0 compiler))
	 (file-name (buffer-file-name))
	 (options (vhdl-get-compile-options project compiler file-name))
	 (default-directory (vhdl-compile-directory))
	 compilation-process-setup-function)
    (unless (file-directory-p default-directory)
      (error "ERROR:  Compile directory does not exist: \"%s\"" default-directory))
    ;; put file name into quotes if it contains spaces
    (when (string-match " " file-name)
      (setq file-name (concat "\"" file-name "\"")))
    ;; print out file name if compiler does not
    (setq vhdl-compile-file-name (buffer-file-name))
    (when (and (= 0 (nth 1 (nth 10 compiler)))
	       (= 0 (nth 1 (nth 11 compiler))))
      (setq compilation-process-setup-function 'vhdl-compile-print-file-name))
    ;; run compilation
    (if options
	(when command
	  (compile (concat command " " options " " file-name)))
      (vhdl-warning "Your project settings tell me not to compile this file"))))

(defvar vhdl-make-target "all"
  "Default target for `vhdl-make' command.")

(defun vhdl-make (&optional target)
  "Call make command for compilation of all updated source files (requires
`Makefile').  Optional argument TARGET allows to compile the design
specified by a target."
  (interactive)
  (setq vhdl-make-target
	(or target (read-from-minibuffer "Target: " vhdl-make-target
					 vhdl-minibuffer-local-map)))
  (vhdl-compile-init)
  (let* ((project (aget vhdl-project-alist vhdl-project))
	 (compiler (or (aget vhdl-compiler-alist vhdl-compiler)
		       (error "ERROR:  No such compiler: \"%s\"" vhdl-compiler)))
	 (command (nth 2 compiler))
	 (options (vhdl-get-make-options project compiler))
	 (default-directory (vhdl-compile-directory)))
    (unless (file-directory-p default-directory)
      (error "ERROR:  Compile directory does not exist: \"%s\"" default-directory))
    ;; run make
    (compile (concat (if (equal command "") "make" command)
		     " " options " " vhdl-make-target))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Makefile generation

(defun vhdl-generate-makefile ()
  "Generate `Makefile'."
  (interactive)
  (let* ((compiler (or (aget vhdl-compiler-alist vhdl-compiler)
		       (error "ERROR:  No such compiler: \"%s\"" vhdl-compiler)))
	 (command (nth 4 compiler)))
    ;; generate makefile
    (if command
	(let ((default-directory (vhdl-compile-directory)))
	  (compile (vhdl-replace-string
		    (cons "\\(.*\\) \\(.*\\)" command)
		    (concat (vhdl-makefile-name) " " (vhdl-work-library)))))
      (vhdl-generate-makefile-1))))

(defun vhdl-get-packages (lib-alist work-library)
  "Get packages from LIB-ALIST that belong to WORK-LIBRARY."
  (let (pack-list)
    (while lib-alist
      (when (equal (downcase (caar lib-alist)) (downcase work-library))
	(setq pack-list (cons (cdar lib-alist) pack-list)))
      (setq lib-alist (cdr lib-alist)))
    pack-list))

(defun vhdl-generate-makefile-1 ()
  "Generate Makefile for current project or directory."
  ;; scan hierarchy if required
  (if (vhdl-project-p)
      (unless (or (assoc vhdl-project vhdl-file-alist)
		  (vhdl-load-cache vhdl-project))
	(vhdl-scan-project-contents vhdl-project))
    (let ((directory (abbreviate-file-name default-directory)))
      (unless (or (assoc directory vhdl-file-alist)
		  (vhdl-load-cache directory))
	(vhdl-scan-directory-contents directory))))
  (let* ((directory (abbreviate-file-name (vhdl-default-directory)))
	 (project (vhdl-project-p))
	 (ent-alist (aget vhdl-entity-alist (or project directory) t))
	 (conf-alist (aget vhdl-config-alist (or project directory) t))
	 (pack-alist (aget vhdl-package-alist (or project directory) t))
	 (regexp-list (nth 12 (aget vhdl-compiler-alist vhdl-compiler)))
	 (ent-regexp (cons "\\(.*\\)" (nth 0 regexp-list)))
	 (arch-regexp (cons "\\(.*\\) \\(.*\\)" (nth 1 regexp-list)))
	 (conf-regexp (cons "\\(.*\\)" (nth 2 regexp-list)))
	 (pack-regexp (cons "\\(.*\\)" (nth 3 regexp-list)))
	 (pack-body-regexp (cons "\\(.*\\)" (nth 4 regexp-list)))
	 (adjust-case (nth 5 regexp-list))
	 (work-library (downcase (vhdl-work-library)))
	 (compile-directory (expand-file-name (vhdl-compile-directory)
					      default-directory))
	 (makefile-name (vhdl-makefile-name))
	 rule-alist arch-alist inst-alist
	 target-list depend-list unit-list prim-list second-list subcomp-list
	 lib-alist lib-body-alist pack-list all-pack-list
	 ent-key ent-file-name arch-key arch-file-name ent-arch-key
	 conf-key conf-file-name pack-key pack-file-name
	 ent-entry arch-entry conf-entry pack-entry inst-entry
	 pack-body-key pack-body-file-name inst-ent-key inst-conf-key
	 tmp-key tmp-list rule)
    ;; check prerequisites
    (unless (file-exists-p compile-directory)
      (make-directory compile-directory t))
    (unless regexp-list
      (error "Please contact the VHDL Mode maintainer for support of \"%s\""
	     vhdl-compiler))
    (message "Generating makefile \"%s\"..." makefile-name)
    ;; rules for all entities
    (setq tmp-list ent-alist)
    (while ent-alist
      (setq ent-entry (car ent-alist)
	    ent-key (nth 0 ent-entry))
      (when (nth 2 ent-entry)
	(setq ent-file-name (file-relative-name
			     (nth 2 ent-entry) compile-directory)
	      arch-alist (nth 4 ent-entry)
	      lib-alist (nth 6 ent-entry)
	      rule (aget rule-alist ent-file-name)
	      target-list (nth 0 rule)
	      depend-list (nth 1 rule)
	      second-list nil
	      subcomp-list nil)
	(setq tmp-key (vhdl-replace-string
		       ent-regexp (funcall adjust-case ent-key)))
	(setq unit-list (cons (cons ent-key tmp-key) unit-list))
	;; rule target for this entity
	(setq target-list (cons ent-key target-list))
	;; rule dependencies for all used packages
	(setq pack-list (vhdl-get-packages lib-alist work-library))
	(setq depend-list (append depend-list pack-list))
	(setq all-pack-list pack-list)
	;; add rule
	(aput 'rule-alist ent-file-name (list target-list depend-list))
	;; rules for all corresponding architectures
	(while arch-alist
	  (setq arch-entry (car arch-alist)
		arch-key (nth 0 arch-entry)
		ent-arch-key (concat ent-key "-" arch-key)
		arch-file-name (file-relative-name (nth 2 arch-entry)
						   compile-directory)
		inst-alist (nth 4 arch-entry)
		lib-alist (nth 5 arch-entry)
		rule (aget rule-alist arch-file-name)
		target-list (nth 0 rule)
		depend-list (nth 1 rule))
	  (setq tmp-key (vhdl-replace-string
			 arch-regexp
			 (funcall adjust-case (concat arch-key " " ent-key))))
	  (setq unit-list
		(cons (cons ent-arch-key tmp-key) unit-list))
	  (setq second-list (cons ent-arch-key second-list))
	  ;; rule target for this architecture
	  (setq target-list (cons ent-arch-key target-list))
	  ;; rule dependency for corresponding entity
	  (setq depend-list (cons ent-key depend-list))
	  ;; rule dependencies for contained component instantiations
	  (while inst-alist
	    (setq inst-entry (car inst-alist))
	    (when (or (null (nth 8 inst-entry))
		      (equal (downcase (nth 8 inst-entry)) work-library))
	      (setq inst-ent-key (or (nth 7 inst-entry)
				     (nth 5 inst-entry)))
	      (setq depend-list (cons inst-ent-key depend-list)
		    subcomp-list (cons inst-ent-key subcomp-list)))
	    (setq inst-alist (cdr inst-alist)))
	  ;; rule dependencies for all used packages
	  (setq pack-list (vhdl-get-packages lib-alist work-library))
	  (setq depend-list (append depend-list pack-list))
	  (setq all-pack-list (append all-pack-list pack-list))
	  ;; add rule
	  (aput 'rule-alist arch-file-name (list target-list depend-list))
	  (setq arch-alist (cdr arch-alist)))
	(setq prim-list (cons (list ent-key second-list
				    (append subcomp-list all-pack-list))
			      prim-list)))
      (setq ent-alist (cdr ent-alist)))
    (setq ent-alist tmp-list)
    ;; rules for all configurations
    (setq tmp-list conf-alist)
    (while conf-alist
      (setq conf-entry (car conf-alist)
	    conf-key (nth 0 conf-entry)
	    conf-file-name (file-relative-name
			    (nth 2 conf-entry) compile-directory)
	    ent-key (nth 4 conf-entry)
	    arch-key (nth 5 conf-entry)
	    inst-alist (nth 6 conf-entry)
	    lib-alist (nth 7 conf-entry)
	    rule (aget rule-alist conf-file-name)
	    target-list (nth 0 rule)
	    depend-list (nth 1 rule)
	    subcomp-list (list ent-key))
      (setq tmp-key (vhdl-replace-string
		     conf-regexp (funcall adjust-case conf-key)))
      (setq unit-list (cons (cons conf-key tmp-key) unit-list))
      ;; rule target for this configuration
      (setq target-list (cons conf-key target-list))
      ;; rule dependency for corresponding entity and architecture
      (setq depend-list
	    (cons ent-key (cons (concat ent-key "-" arch-key) depend-list)))
      ;; rule dependencies for used packages
      (setq pack-list (vhdl-get-packages lib-alist work-library))
      (setq depend-list (append depend-list pack-list))
      ;; rule dependencies for contained component configurations
      (while inst-alist
	(setq inst-entry (car inst-alist))
	(setq inst-ent-key (nth 2 inst-entry)
;	       comp-arch-key (nth 2 inst-entry))
	      inst-conf-key (nth 4 inst-entry))
	(when (equal (downcase (nth 5 inst-entry)) work-library)
	  (when inst-ent-key
	    (setq depend-list (cons inst-ent-key depend-list)
		  subcomp-list (cons inst-ent-key subcomp-list)))
; 	    (when comp-arch-key
; 	      (setq depend-list (cons (concat comp-ent-key "-" comp-arch-key)
; 				      depend-list)))
	  (when inst-conf-key
	    (setq depend-list (cons inst-conf-key depend-list)
		  subcomp-list (cons inst-conf-key subcomp-list))))
	(setq inst-alist (cdr inst-alist)))
      ;; add rule
      (aput 'rule-alist conf-file-name (list target-list depend-list))
      (setq prim-list (cons (list conf-key nil (append subcomp-list pack-list))
			    prim-list))
      (setq conf-alist (cdr conf-alist)))
    (setq conf-alist tmp-list)
    ;; rules for all packages
    (setq tmp-list pack-alist)
    (while pack-alist
      (setq pack-entry (car pack-alist)
	    pack-key (nth 0 pack-entry)
	    pack-body-key nil)
      (when (nth 2 pack-entry)
	(setq pack-file-name (file-relative-name (nth 2 pack-entry)
						 compile-directory)
	      lib-alist (nth 6 pack-entry) lib-body-alist (nth 10 pack-entry)
	      rule (aget rule-alist pack-file-name)
	      target-list (nth 0 rule) depend-list (nth 1 rule))
	(setq tmp-key (vhdl-replace-string
		       pack-regexp (funcall adjust-case pack-key)))
	(setq unit-list (cons (cons pack-key tmp-key) unit-list))
	;; rule target for this package
	(setq target-list (cons pack-key target-list))
	;; rule dependencies for all used packages
	(setq pack-list (vhdl-get-packages lib-alist work-library))
	(setq depend-list (append depend-list pack-list))
	(setq all-pack-list pack-list)
	;; add rule
	(aput 'rule-alist pack-file-name (list target-list depend-list))
	;; rules for this package's body
	(when (nth 7 pack-entry)
	  (setq pack-body-key (concat pack-key "-body")
		pack-body-file-name (file-relative-name (nth 7 pack-entry)
							compile-directory)
		rule (aget rule-alist pack-body-file-name)
		target-list (nth 0 rule)
		depend-list (nth 1 rule))
	  (setq tmp-key (vhdl-replace-string
			 pack-body-regexp (funcall adjust-case pack-key)))
	  (setq unit-list
		(cons (cons pack-body-key tmp-key) unit-list))
	  ;; rule target for this package's body
	  (setq target-list (cons pack-body-key target-list))
	  ;; rule dependency for corresponding package declaration
	  (setq depend-list (cons pack-key depend-list))
	  ;; rule dependencies for all used packages
	  (setq pack-list (vhdl-get-packages lib-body-alist work-library))
	  (setq depend-list (append depend-list pack-list))
	  (setq all-pack-list (append all-pack-list pack-list))
	  ;; add rule
	  (aput 'rule-alist pack-body-file-name
		(list target-list depend-list)))
	(setq prim-list
	      (cons (list pack-key (when pack-body-key (list pack-body-key))
			  all-pack-list)
		    prim-list)))
      (setq pack-alist (cdr pack-alist)))
    (setq pack-alist tmp-list)
    ;; generate Makefile
    (let* ((project (aget vhdl-project-alist project))
	   (compiler (aget vhdl-compiler-alist vhdl-compiler))
	   (compiler-id (nth 9 compiler))
	   (library-directory
	    (vhdl-resolve-env-variable
	     (vhdl-replace-string
	      (cons "\\(.*\\)" (or (nth 7 project) (nth 7 compiler)))
	      compiler-id)))
	   (makefile-path-name (expand-file-name
				makefile-name compile-directory))
	   (orig-buffer (current-buffer))
	   cell second-list subcomp-list options unit-key unit-name)
      ;; sort lists
      (setq unit-list (vhdl-sort-alist unit-list))
      (setq prim-list (vhdl-sort-alist prim-list))
      (setq tmp-list rule-alist)
      (while tmp-list			; pre-sort rule targets
	(setq cell (cdar tmp-list))
	(setcar cell (sort (car cell) 'string<))
	(setq tmp-list (cdr tmp-list)))
      (setq rule-alist			; sort by first rule target
	    (sort rule-alist
		  (function (lambda (a b)
			      (string< (car (cadr a)) (car (cadr b)))))))
      ;; open and clear Makefile
      (set-buffer (find-file-noselect makefile-path-name t t))
      (erase-buffer)
      (insert "# -*- Makefile -*-\n"
	      "### " (file-name-nondirectory makefile-name)
	      " - VHDL Makefile generated by Emacs VHDL Mode " vhdl-version
	      "\n")
      (if project
	  (insert "\n# Project   : " (nth 0 project))
	(insert "\n# Directory : \"" directory "\""))
      (insert "\n# Platform  : " vhdl-compiler
	      "\n# Generated : " (format-time-string "%Y-%m-%d %T ")
	      (user-login-name) "\n")
      ;; insert compile and option variable settings
      (insert "\n\n# Define compilation command and options\n"
	      "\nCOMPILE = " (nth 0 compiler)
	      "\nOPTIONS = " (vhdl-get-compile-options project compiler nil)
	      "\n")
      ;; insert library paths
      (setq library-directory
	    (directory-file-name
	     (if (file-name-absolute-p library-directory)
		 library-directory
	       (file-relative-name
		(expand-file-name library-directory directory)
		compile-directory))))
      (insert "\n\n# Define library paths\n"
	      "\nLIBRARY-" work-library " = " library-directory "\n")
      ;; insert variable definitions for all library unit files
      (insert "\n\n# Define library unit files\n")
      (setq tmp-list unit-list)
      (while unit-list
	(insert "\nUNIT-" work-library "-" (caar unit-list)
		" = \\\n\t$(LIBRARY-" work-library ")/" (cdar unit-list))
	(setq unit-list (cdr unit-list)))
      ;; insert variable definition for list of all library unit files
      (insert "\n\n\n# Define list of all library unit files\n"
	      "\nALL_UNITS =")
      (setq unit-list tmp-list)
      (while unit-list
	(insert " \\\n\t" "$(UNIT-" work-library "-" (caar unit-list) ")")
	(setq unit-list (cdr unit-list)))
      (insert "\n")
      (setq unit-list tmp-list)
      ;; insert `make all' rule
      (insert "\n\n\n# Rule for compiling entire design\n"
	      "\nall :"
	      " \\\n\t\tlibrary"
	      " \\\n\t\t$(ALL_UNITS)\n")
      ;; insert `make clean' rule
      (insert "\n\n# Rule for cleaning entire design\n"
	      "\nclean : "
	      "\n\t-rm -f $(ALL_UNITS)\n")
      ;; insert `make library' rule
      (insert "\n\n# Rule for creating library directory\n"
	      "\nlibrary :"
	      " \\\n\t\t$(LIBRARY-" work-library ")\n"
	      "\n$(LIBRARY-" work-library ") :"
	      "\n\t"
	      (vhdl-replace-string
	       (cons "\\(.*\\)\n\\(.*\\)" (nth 5 compiler))
	       (concat "$(LIBRARY-" work-library ")\n" (vhdl-work-library)))
	      "\n")
      ;; insert rule for each library unit
      (insert "\n\n# Rules for compiling single library units and their subhierarchy\n")
      (while prim-list
	(setq second-list (sort (nth 1 (car prim-list)) 'string<))
	(setq subcomp-list
	      (sort (vhdl-uniquify (nth 2 (car prim-list))) 'string<))
	(setq unit-key (caar prim-list)
	      unit-name (or (nth 0 (aget ent-alist unit-key t))
			    (nth 0 (aget conf-alist unit-key t))
			    (nth 0 (aget pack-alist unit-key t))))
	(insert "\n" unit-key)
	(unless (equal unit-key unit-name)
	  (insert " \\\n" unit-name))
	(insert " :"
		" \\\n\t\tlibrary"
		" \\\n\t\t$(UNIT-" work-library "-" unit-key ")")
	(while second-list
	  (insert " \\\n\t\t$(UNIT-" work-library "-" (car second-list) ")")
	  (setq second-list (cdr second-list)))
	(while subcomp-list
	  (when (and (assoc (car subcomp-list) unit-list)
		     (not (equal unit-key (car subcomp-list))))
	    (insert " \\\n\t\t" (car subcomp-list)))
	  (setq subcomp-list (cdr subcomp-list)))
	(insert "\n")
	(setq prim-list (cdr prim-list)))
      ;; insert rule for each library unit file
      (insert "\n\n# Rules for compiling single library unit files\n")
      (while rule-alist
	(setq rule (car rule-alist))
	;; get compiler options for this file
	(setq options
	      (vhdl-get-compile-options project compiler (nth 0 rule) t))
	;; insert rule if file is supposed to be compiled
	(setq target-list (nth 1 rule)
	      depend-list (sort (vhdl-uniquify (nth 2 rule)) 'string<))
	;; insert targets
	(setq tmp-list target-list)
	(while target-list
	  (insert "\n$(UNIT-" work-library "-" (car target-list) ")"
		  (if (cdr target-list) " \\" " :"))
	  (setq target-list (cdr target-list)))
	(setq target-list tmp-list)
	;; insert file name as first dependency
	(insert " \\\n\t\t" (nth 0 rule))
	;; insert dependencies (except if also target or unit does not exist)
	(while depend-list
	  (when (and (not (member (car depend-list) target-list))
		     (assoc (car depend-list) unit-list))
	    (insert " \\\n\t\t"
		    "$(UNIT-" work-library "-" (car depend-list) ")"))
	  (setq depend-list (cdr depend-list)))
	;; insert compile command
	(if options
	    (insert "\n\t$(COMPILE) "
		    (if (eq options 'default) "$(OPTIONS)" options) " "
		    (nth 0 rule) "\n")
	  (setq tmp-list target-list)
	  (while target-list
	    (insert "\n\t@touch $(UNIT-" work-library "-" (car target-list) ")"
		    (if (cdr target-list) " \\" "\n"))
	    (setq target-list (cdr target-list)))
	  (setq target-list tmp-list))
	(setq rule-alist (cdr rule-alist)))
      (insert "\n\n### " makefile-name " ends here\n")
      ;; run Makefile generation hook
      (run-hooks 'vhdl-makefile-generation-hook)
      (message "Generating makefile \"%s\"...done" makefile-name)
      ;; save and close file
      (if (file-writable-p makefile-path-name)
	  (progn (save-buffer)
		 (kill-buffer (current-buffer))
		 (set-buffer orig-buffer)
		 (add-to-history 'file-name-history makefile-path-name))
	(vhdl-warning-when-idle
	 (format "File not writable: \"%s\""
		 (abbreviate-file-name makefile-path-name)))
	(switch-to-buffer (current-buffer))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bug reports
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (using `reporter.el')

(defconst vhdl-mode-help-address
  "Reto Zimmermann <reto@gnu.org>"
  "Address for VHDL Mode bug reports.")

(defun vhdl-submit-bug-report ()
  "Submit via mail a bug report on VHDL Mode."
  (interactive)
  ;; load in reporter
  (and
   (y-or-n-p "Do you want to submit a report on VHDL Mode? ")
   (let ((reporter-prompt-for-summary-p t))
     (reporter-submit-bug-report
      vhdl-mode-help-address
      (concat "VHDL Mode " vhdl-version)
      (list
       ;; report all important user options
       'vhdl-offsets-alist
       'vhdl-comment-only-line-offset
       'tab-width
       'vhdl-electric-mode
       'vhdl-stutter-mode
       'vhdl-indent-tabs-mode
       'vhdl-project-alist
       'vhdl-project
       'vhdl-project-file-name
       'vhdl-project-auto-load
       'vhdl-project-sort
       'vhdl-compiler-alist
       'vhdl-compiler
       'vhdl-compile-use-local-error-regexp
       'vhdl-makefile-generation-hook
       'vhdl-default-library
       'vhdl-standard
       'vhdl-basic-offset
       'vhdl-upper-case-keywords
       'vhdl-upper-case-types
       'vhdl-upper-case-attributes
       'vhdl-upper-case-enum-values
       'vhdl-upper-case-constants
       'vhdl-use-direct-instantiation
       'vhdl-compose-configuration-name
       'vhdl-entity-file-name
       'vhdl-architecture-file-name
       'vhdl-configuration-file-name
       'vhdl-package-file-name
       'vhdl-file-name-case
       'vhdl-electric-keywords
       'vhdl-optional-labels
       'vhdl-insert-empty-lines
       'vhdl-argument-list-indent
       'vhdl-association-list-with-formals
       'vhdl-conditions-in-parenthesis
       'vhdl-zero-string
       'vhdl-one-string
       'vhdl-file-header
       'vhdl-file-footer
       'vhdl-company-name
       'vhdl-copyright-string
       'vhdl-platform-spec
       'vhdl-date-format
       'vhdl-modify-date-prefix-string
       'vhdl-modify-date-on-saving
       'vhdl-reset-kind
       'vhdl-reset-active-high
       'vhdl-clock-rising-edge
       'vhdl-clock-edge-condition
       'vhdl-clock-name
       'vhdl-reset-name
       'vhdl-model-alist
       'vhdl-include-port-comments
       'vhdl-include-direction-comments
       'vhdl-include-type-comments
       'vhdl-include-group-comments
       'vhdl-actual-port-name
       'vhdl-instance-name
       'vhdl-testbench-entity-name
       'vhdl-testbench-architecture-name
       'vhdl-testbench-configuration-name
       'vhdl-testbench-dut-name
       'vhdl-testbench-include-header
       'vhdl-testbench-declarations
       'vhdl-testbench-statements
       'vhdl-testbench-initialize-signals
       'vhdl-testbench-include-library
       'vhdl-testbench-include-configuration
       'vhdl-testbench-create-files
       'vhdl-testbench-entity-file-name
       'vhdl-testbench-architecture-file-name
       'vhdl-compose-create-files
       'vhdl-compose-configuration-create-file
       'vhdl-compose-configuration-hierarchical
       'vhdl-compose-configuration-use-subconfiguration
       'vhdl-compose-include-header
       'vhdl-compose-architecture-name
       'vhdl-components-package-name
       'vhdl-use-components-package
       'vhdl-self-insert-comments
       'vhdl-prompt-for-comments
       'vhdl-inline-comment-column
       'vhdl-end-comment-column
       'vhdl-auto-align
       'vhdl-align-groups
       'vhdl-align-group-separate
       'vhdl-align-same-indent
       'vhdl-highlight-keywords
       'vhdl-highlight-names
       'vhdl-highlight-special-words
       'vhdl-highlight-forbidden-words
       'vhdl-highlight-verilog-keywords
       'vhdl-highlight-translate-off
       'vhdl-highlight-case-sensitive
       'vhdl-special-syntax-alist
       'vhdl-forbidden-words
       'vhdl-forbidden-syntax
       'vhdl-directive-keywords
       'vhdl-speedbar-auto-open
       'vhdl-speedbar-display-mode
       'vhdl-speedbar-scan-limit
       'vhdl-speedbar-jump-to-unit
       'vhdl-speedbar-update-on-saving
       'vhdl-speedbar-save-cache
       'vhdl-speedbar-cache-file-name
       'vhdl-index-menu
       'vhdl-source-file-menu
       'vhdl-hideshow-menu
       'vhdl-hide-all-init
       'vhdl-print-two-column
       'vhdl-print-customize-faces
       'vhdl-intelligent-tab
       'vhdl-indent-syntax-based
       'vhdl-word-completion-case-sensitive
       'vhdl-word-completion-in-minibuffer
       'vhdl-underscore-is-part-of-word
       'vhdl-mode-hook)
      (function
       (lambda ()
	 (insert
	  (if vhdl-special-indent-hook
	      (concat "\n@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n"
		      "vhdl-special-indent-hook is set to '"
		      (format "%s" vhdl-special-indent-hook)
		      ".\nPerhaps this is your problem?\n"
		      "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n\n")
	    "\n"))))
      nil
      "Hi Reto,"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Documentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst vhdl-doc-release-notes nil
  "\
Release Notes for VHDL Mode 3.33
================================

  - New Features
  - User Options


New Features
------------

CONFIGURATION DECLARATION GENERATION:
  - Automatic generation of a configuration declaration for a design.
    (See documentation (`C-c C-h') in section on STRUCTURAL COMPOSITION.)


User Options
------------

`vhdl-configuration-file-name': (new)
  Specify how the configuration file name is obtained.
`vhdl-compose-configuration-name': (new)
  Specify how the configuration name is obtained.
`vhdl-compose-configuration-create-file': (new)
  Specify whether a new file should be created for a configuration.
`vhdl-compose-configuration-hierarchical': (new)
  Specify whether hierarchical configurations should be created.
`vhdl-compose-configuration-use-subconfiguration': (new)
  Specify whether subconfigurations should be used inside configurations.
")


(defconst vhdl-doc-keywords nil
  "\
Reserved words in VHDL
----------------------

VHDL'93 (IEEE Std 1076-1993):
  `vhdl-93-keywords'      : keywords
  `vhdl-93-types'         : standardized types
  `vhdl-93-attributes'    : standardized attributes
  `vhdl-93-enum-values'   : standardized enumeration values
  `vhdl-93-functions'     : standardized functions
  `vhdl-93-packages'      : standardized packages and libraries

VHDL-AMS (IEEE Std 1076.1):
  `vhdl-ams-keywords'     : keywords
  `vhdl-ams-types'        : standardized types
  `vhdl-ams-attributes'   : standardized attributes
  `vhdl-ams-enum-values'  : standardized enumeration values
  `vhdl-ams-functions'    : standardized functions

Math Packages (IEEE Std 1076.2):
  `vhdl-math-types'       : standardized types
  `vhdl-math-constants'   : standardized constants
  `vhdl-math-functions'   : standardized functions
  `vhdl-math-packages'    : standardized packages

Forbidden words:
  `vhdl-verilog-keywords' : Verilog reserved words

NOTE: click `mouse-2' on variable names above (not in XEmacs).")


(defconst vhdl-doc-coding-style nil
  "\
For VHDL coding style and naming convention guidelines, see the following
references:

\[1] Ben Cohen.
    \"VHDL Coding Styles and Methodologies\".
    Kluwer Academic Publishers, 1999.
    http://members.aol.com/vhdlcohen/vhdl/

\[2] Michael Keating and Pierre Bricaud.
    \"Reuse Methodology Manual, Second Edition\".
    Kluwer Academic Publishers, 1999.
    http://www.openmore.com/openmore/rmm2.html

\[3] European Space Agency.
    \"VHDL Modelling Guidelines\".
    ftp://ftp.estec.esa.nl/pub/vhdl/doc/ModelGuide.{pdf,ps}

Use user options `vhdl-highlight-special-words' and `vhdl-special-syntax-alist'
to visually support naming conventions.")


(defun vhdl-version ()
  "Echo the current version of VHDL Mode in the minibuffer."
  (interactive)
  (message "VHDL Mode %s (%s)" vhdl-version vhdl-time-stamp)
  (vhdl-keep-region-active))

(defun vhdl-doc-variable (variable)
  "Display VARIABLE's documentation in *Help* buffer."
  (interactive)
  (unless (featurep 'xemacs)
    (help-setup-xref (list #'vhdl-doc-variable variable) (interactive-p)))
  (with-output-to-temp-buffer
      (if (fboundp 'help-buffer) (help-buffer) "*Help*")
    (princ (documentation-property variable 'variable-documentation))
    (with-current-buffer standard-output
      (help-mode))
    (help-print-return-message)))

(defun vhdl-doc-mode ()
  "Display VHDL Mode documentation in *Help* buffer."
  (interactive)
  (unless (featurep 'xemacs)
    (help-setup-xref (list #'vhdl-doc-mode) (interactive-p)))
  (with-output-to-temp-buffer
      (if (fboundp 'help-buffer) (help-buffer) "*Help*")
    (princ mode-name)
    (princ " mode:\n")
    (princ (documentation 'vhdl-mode))
    (with-current-buffer standard-output
      (help-mode))
    (help-print-return-message)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'vhdl-mode)

;;; vhdl-mode.el ends here
