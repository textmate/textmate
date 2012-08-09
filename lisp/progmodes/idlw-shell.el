;; idlw-shell.el --- run IDL as an inferior process of Emacs.

;; Copyright (C) 1999-2012  Free Software Foundation, Inc.

;; Authors: J.D. Smith <jdsmith@as.arizona.edu>
;;          Carsten Dominik <dominik@astro.uva.nl>
;;          Chris Chase <chase@att.com>
;; Maintainer: J.D. Smith <jdsmith@as.arizona.edu>
;; Version: 6.1.22
;; Keywords: processes
;; Package: idlwave

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
;;
;; This mode is for IDL version 5 or later.  It should work on
;; Emacs>20.3 or XEmacs>20.4.
;;
;; Runs IDL as an inferior process of Emacs, much like the Emacs
;; `shell' or `telnet' commands.  Provides command history and
;; searching.  Provides debugging commands available in buffers
;; visiting IDL procedure files, e.g., breakpoint setting, stepping,
;; execution until a certain line, printing expressions under point,
;; visual line pointer for current execution line, etc.
;;
;; Documentation should be available online with `M-x idlwave-info'.
;;
;; New versions of IDLWAVE, documentation, and more information
;; available from:
;;                 http://idlwave.org
;;
;; INSTALLATION:
;; =============
;;
;; Follow the instructions in the INSTALL file of the distribution.
;; In short, put this file on your load path and add the following
;; lines to your .emacs file:
;;
;; (autoload 'idlwave-shell "idlw-shell" "IDLWAVE Shell" t)
;;
;;
;; SOURCE
;; ======
;;
;;   The newest version of this file can be found on the maintainers
;;   web site.
;;
;;     http://idlwave.org
;;
;; DOCUMENTATION
;; =============
;;
;; IDLWAVE is documented online in info format.
;; A printable version of the documentation is available from the
;; maintainers webpage (see under SOURCE)
;;
;;
;; KNOWN PROBLEMS
;; ==============
;;
;; Under XEmacs the Debug menu in the shell does not display the
;; keybindings in the prefix map.  There bindings are available anyway - so
;; it is a bug in XEmacs.
;; The Debug menu in source buffers *does* display the bindings correctly.
;;
;;
;; CUSTOMIZATION VARIABLES
;; =======================
;;
;; IDLWAVE has customize support - so if you want to learn about
;; the variables which control the behavior of the mode, use
;; `M-x idlwave-customize'.
;;
;;--------------------------------------------------------------------------
;;

;;; Code:

(require 'comint)
(require 'idlwave)

(eval-when-compile (require 'cl))

(defvar idlwave-shell-have-new-custom nil)

;;; Customizations: idlwave-shell group

;; General/Misc. customizations
(defgroup idlwave-shell-general-setup nil
  "General setup of the Shell interaction for IDLWAVE/Shell."
  :prefix "idlwave-shell"
  :group 'idlwave)

(defcustom idlwave-shell-prompt-pattern "^\r? ?IDL> "
  "*Regexp to match IDL prompt at beginning of a line.
For example, \"^\r?IDL> \" or \"^\r?WAVE> \".
The \"^\r?\" is needed, to indicate the beginning of the line, with
optional return character (which IDL seems to output randomly).
This variable is used to initialize `comint-prompt-regexp' in the
process buffer.

This is a fine thing to set in your `.emacs' file."
  :group 'idlwave-shell-general-setup
  :type 'regexp)

(defcustom idlwave-shell-process-name "idl"
  "*Name to be associated with the IDL process.  The buffer for the
process output is made by surrounding this name with `*'s."
  :group 'idlwave-shell-general-setup
  :type 'string)

;; (defcustom idlwave-shell-automatic-start...)  See idlwave.el

(defcustom idlwave-shell-use-dedicated-window nil
  "*Non-nil means, never replace the shell frame with another buffer."
  :group 'idlwave-shell-general-setup
  :type 'boolean)

(defcustom idlwave-shell-use-dedicated-frame nil
  "*Non-nil means, IDLWAVE should use a special frame to display shell buffer."
  :group 'idlwave-shell-general-setup
  :type 'boolean)

(defcustom idlwave-shell-frame-parameters
  '((height . 30) (unsplittable . nil))
  "The frame parameters for a dedicated idlwave-shell frame.
See also `idlwave-shell-use-dedicated-frame'.
The default makes the frame splittable, so that completion works correctly."
  :group 'idlwave-shell-general-setup
  :type '(repeat
	  (cons symbol sexp)))

(defcustom idlwave-shell-raise-frame t
  "*Non-nil means, `idlwave-shell' raises the frame showing the shell window."
  :group 'idlwave-shell-general-setup
  :type 'boolean)

(defcustom idlwave-shell-arrows-do-history t
  "*Non-nil means UP and DOWN arrows move through command history.
This variable can have 3 values:
nil        Arrows just move the cursor
t          Arrows force the cursor back to the current command line and
           walk the history
'cmdline   When the cursor is in the current command line, arrows walk the
           history.  Everywhere else in the buffer, arrows move the cursor."
  :group 'idlwave-shell-general-setup
  :type '(choice
	  (const :tag "never" nil)
	  (const :tag "everywhere" t)
	  (const :tag "in command line only" cmdline)))

;; FIXME: add comint-input-ring-size?

(defcustom idlwave-shell-use-toolbar t
  "*Non-nil means, use the debugging toolbar in all IDL related buffers.
Starting the shell will then add the toolbar to all idlwave-mode buffers.
Exiting the shell will removed everywhere.
Available on XEmacs and on Emacs 21.x or later.
At any time you can toggle the display of the toolbar with
`C-c C-d C-t' (`idlwave-shell-toggle-toolbar')."
  :group 'idlwave-shell-general-setup
  :type 'boolean)

(defcustom idlwave-shell-temp-pro-prefix "/tmp/idltemp"
  "*The prefix for temporary IDL files used when compiling regions.
It should be an absolute pathname.
The full temporary file name is obtained by using `make-temp-file'
so that the name will be unique among multiple Emacs processes."
  :group 'idlwave-shell-general-setup
  :type 'string)

(defvar idlwave-shell-fix-inserted-breaks nil
  "*OBSOLETE VARIABLE, is no longer used.

The documentation of this variable used to be:
If non-nil then run `idlwave-shell-remove-breaks' to clean up IDL messages.")

(defcustom idlwave-shell-prefix-key "\C-c\C-d"
  "*The prefix key for the debugging map `idlwave-shell-mode-prefix-map'.
This variable must already be set when idlwave-shell.el is loaded.
Setting it in the mode-hook is too late."
  :group 'idlwave-shell-general-setup
  :type 'string)

(defcustom idlwave-shell-activate-prefix-keybindings t
  "Non-nil means, the debug commands will be bound to the prefix key.
The prefix key itself is given in the option `idlwave-shell-prefix-key'.
So by default setting a breakpoint will be on C-c C-d C-b."
  :group 'idlwave-shell-general-setup
  :type 'boolean)

(defcustom idlwave-shell-automatic-electric-debug 'breakpoint
  "Enter the electric-debug minor mode automatically.
This occurs at a breakpoint or any other halt.  The mode is exited
upon return to the main level.  Can be set to 'breakpoint to enter
electric debug mode only when breakpoints are tripped."
  :group 'idlwave-shell-general-setup
  :type '(choice
	  (const :tag "never" nil)
	  (const :tag "always" t)
	  (const :tag "for breakpoints only" breakpoint)))

(defcustom idlwave-shell-electric-zap-to-file t
  "When entering electric debug mode, select the window displaying the
file at which point is stopped.  This takes point away from the shell
window, but is useful for stepping, etc."
  :group 'idlwave-shell-general-setup
  :type 'boolean)

;; (defcustom idlwave-shell-debug-modifiers... See idlwave.el

(defvar idlwave-shell-activate-alt-keybindings nil
  "Obsolete variable.  See `idlwave-shell-debug-modifiers'.")

(defcustom idlwave-shell-use-truename nil
  "*Non-nil means, use `file-truename' when looking for buffers.
If this variable is non-nil, Emacs will use the function `file-truename' to
resolve symbolic links in the file paths printed by e.g., STOP commands.
This means, unvisited files will be loaded under their truename.
However, when a file is already visited under a different name, IDLWAVE will
reuse that buffer.
This option was once introduced in order to avoid multiple buffers visiting
the same file.  However, IDLWAVE no longer makes this mistake, so it is safe
to set this option to nil."
  :group 'idlwave-shell-general-setup
  :type 'boolean)

(defcustom idlwave-shell-file-name-chars "~/A-Za-z0-9+:_.$#%={}\\- "
  "The characters allowed in file names, as a string.
Used for file name completion.  Must not contain `'', `,' and `\"'
because these are used as separators by IDL."
  :group 'idlwave-shell-general-setup
  :type 'string)

(defcustom idlwave-shell-mode-hook '()
  "*Hook for customizing `idlwave-shell-mode'."
  :group 'idlwave-shell-general-setup
  :type 'hook)

(defcustom idlwave-shell-graphics-window-size '(500 400)
  "Size of IDL graphics windows popped up by special IDLWAVE command.
The command is `C-c C-d C-f' and accepts as a prefix the window nr.
A command like `WINDOW,N,xsize=XX,ysize=YY' is sent to IDL."
  :group 'idlwave-shell-general-setup
  :type '(list
	  (integer :tag "x size")
	  (integer :tag "y size")))


;; Commands Sent to Shell... etc.
(defgroup idlwave-shell-command-setup nil
  "Setup for command parameters of the Shell interaction for IDLWAVE."
  :prefix "idlwave-shell"
  :group 'idlwave)

(defcustom idlwave-shell-initial-commands "!more=0 & defsysv,'!ERROR_STATE',EXISTS=__e & if __e then begin & !ERROR_STATE.MSG_PREFIX=\"% \" & delvar,__e & endif"
  "Initial commands, separated by newlines, to send to IDL.
This string is sent to the IDL process by `idlwave-shell-mode' which is
invoked by `idlwave-shell'."
  :group 'idlwave-shell-command-setup
  :type 'string)

(defcustom idlwave-shell-save-command-history t
  "Non-nil means preserve command history between sessions.
The file `idlwave-shell-command-history-file' is used to save and restore
the history."
  :group 'idlwave-shell-command-setup
  :type 'boolean)

(defcustom idlwave-shell-command-history-file "idlwhist"
  "The file in which the command history of the idlwave shell is saved.
In order to change the size of the history, see the variable
`comint-input-ring-size'.
The history is only saved if the variable `idlwave-shell-save-command-history'
is non-nil."
  :group 'idlwave-shell-command-setup
  :type 'file)

(defcustom idlwave-shell-show-commands
  '(run misc breakpoint)
  "*A list of command types to show output from in the shell.
Possibilities are 'run, 'debug, 'breakpoint, and 'misc.  Unselected
types are not displayed in the shell.  The type 'everything causes all
the copious shell traffic to be displayed."
  :group 'idlwave-shell-command-setup
  :type '(choice
	  (const everything)
	  (set :tag "Checklist" :greedy t
	       (const :tag "All .run and .compile commands"  	   run)
	       (const :tag "All breakpoint commands"         	   breakpoint)
	       (const :tag "All debug and stepping commands" 	   debug)
	       (const :tag "Close, window, retall, etc. commands"  misc))))

(defcustom idlwave-shell-max-print-length 200
  "Maximum number of array elements to print when examining."
  :group 'idlwave-shell-command-setup
  :type 'integer)

(defcustom idlwave-shell-examine-alist
  `(("Print"          	. ,(concat "idlwave_print_safe,___,"
				   (number-to-string
				    idlwave-shell-max-print-length)))
    ("Help"           	. "help,___")
    ("Structure Help"  	. "help,___,/STRUCTURE")
    ("Dimensions"     	. "print,size(___,/DIMENSIONS)")
    ("Type"           	. "print,size(___,/TNAME)")
    ("N_Elements"     	. "print,n_elements(___)")
    ("All Size Info"  	. "help,(__IWsz__=size(___,/STRUCTURE)),/STRUCTURE & print,__IWsz__.DIMENSIONS")
    ("Ptr Valid"      	. "print,ptr_valid(___)")
    ("Arg Present"      . "print,arg_present(___)")
    ("Widget Valid"     . "print,widget_info(___,/VALID)")
    ("Widget Geometry"  . "help,widget_info(___,/GEOMETRY)"))
  "Alist of special examine commands for popup selection.
The keys are used in the selection popup created by
`idlwave-shell-examine-select', and the corresponding value is sent as
a command to the shell, with special sequence `___' replaced by the
expression being examined."
  :group 'idlwave-shell-command-setup
  :type '(repeat
	  (cons
	   (string :tag "Label  ")
	   (string :tag "Command"))))

(defvar idlwave-shell-print-expression-function nil
  "*OBSOLETE VARIABLE, is no longer used.")

(defcustom idlwave-shell-separate-examine-output t
  "*Non-nil means, put output of examine commands in their own buffer."
  :group 'idlwave-shell-command-setup
  :type 'boolean)

(defcustom idlwave-shell-comint-settings
  '((comint-scroll-to-bottom-on-input . t)
    (comint-scroll-to-bottom-on-output . t)
    (comint-scroll-show-maximum-output . nil)
    (comint-prompt-read-only . t))

  "Alist of special settings for the comint variables in the IDLWAVE Shell.
Each entry is a cons cell with the name of a variable and a value.
The function `idlwave-shell-mode' will make local variables out of each entry.
Changes to this variable will only be active when the shell buffer is
newly created."
  :group 'idlwave-shell-command-setup
  :type '(repeat
	  (cons variable sexp)))

(defcustom idlwave-shell-query-for-class t
  "*Non-nil means query the shell for object class on object completions."
  :group 'idlwave-shell-command-setup
  :type 'boolean)

(defcustom idlwave-shell-use-input-mode-magic nil
  "*Non-nil means, IDLWAVE should check for input mode spells in output.
The spells are strings printed by your IDL program and matched
by the regular expressions in `idlwave-shell-input-mode-spells'.
When these expressions match, IDLWAVE switches to character input mode and
back, respectively.  See `idlwave-shell-input-mode-spells' for details."
  :group 'idlwave-shell-command-setup
  :type 'boolean)

(defcustom idlwave-shell-input-mode-spells
  '("^<onechar>$" "^<chars>$" "^</chars>$")
  "The three regular expressions which match the magic spells for input modes.

When the first regexp matches in the output stream of IDL, IDLWAVE
prompts for a single character and sends it immediately to IDL, similar
to the command \\[idlwave-shell-send-char].

When the second regexp matches, IDLWAVE switches to a blocking
single-character input mode.  This is the same mode which can be entered
manually with \\[idlwave-shell-char-mode-loop].
This input mode exits when the third regexp matches in the output,
or when the IDL prompt is encountered.

The variable `idlwave-shell-use-input-mode-magic' must be non-nil to enable
scanning for these expressions.  If the IDL program produces lots of
output, shell operation may be slowed down.

This mechanism is useful for correct interaction with the IDL function
GET_KBRD, because in normal operation IDLWAVE only sends \\n terminated
strings.  Here is some example code which makes use of the default spells.

  print,'<chars>'               ; Make IDLWAVE switch to character mode
  REPEAT BEGIN
      A = GET_KBRD(1)
      PRINT, BYTE(A)
  ENDREP UNTIL A EQ 'q'
  print,'</chars>'              ; Make IDLWAVE switch back to line mode

  print,'Quit the program, y or n?'
  print,'<onechar>'             ; Ask IDLWAVE to send one character
  answer = GET_KBRD(1)

Since the IDLWAVE shell defines the system variable `!IDLWAVE_VERSION',
you could actually check if you are running under Emacs before printing
the magic strings.  Here is a procedure which uses this.

Usage:
======
idlwave_char_input               ; Make IDLWAVE send one character
idlwave_char_input,/on           ; Start the loop to send characters
idlwave_char_input,/off          ; End the loop to send characters


pro idlwave_char_input,on=on,off=off
  ;; Test if we are running under Emacs
  defsysv,'!idlwave_version',exists=running_emacs
  if running_emacs then begin
      if keyword_set(on) then         print,'<chars>' $
        else if keyword_set(off) then print,'</chars>' $
        else                          print,'<onechar>'
  endif
end"
  :group 'idlwave-shell-command-setup
  :type '(list
	  (regexp :tag "One-char  regexp")
	  (regexp :tag "Char-mode regexp")
	  (regexp :tag "Line-mode regexp")))

(defcustom idlwave-shell-breakpoint-popup-menu t
  "*If non-nil, provide a menu on mouse-3 on breakpoint lines, and
popup help text on the line."
  :group 'idlwave-shell-command-setup
  :type 'boolean)

(defcustom idlwave-shell-reset-no-prompt nil
  "If non-nil, skip the yes/no prompt when resetting the IDL session."
  :group 'idlwave-shell-command-setup
  :type 'boolean)

;; Breakpoint Overlays etc
(defgroup idlwave-shell-highlighting-and-faces nil
  "Highlighting and faces used by the IDLWAVE Shell mode."
  :prefix "idlwave-shell"
  :group 'idlwave)

(defcustom idlwave-shell-mark-stop-line t
  "*Non-nil means, mark the source code line where IDL is currently stopped.
Value decides about the method which is used to mark the line.  Valid values
are:

nil       Do not mark the line
'arrow    Use the overlay arrow
'face     Use `idlwave-shell-stop-line-face' to highlight the line.
t         Use what IDLWAVE thinks is best.  Will be a face where possible,
          otherwise the overlay arrow.
The overlay-arrow has the disadvantage to hide the first chars of a line.
Since many people do not have the main block of IDL programs indented,
a face highlighting may be better.
In Emacs 21, the overlay arrow is displayed in a special area and never
hides any code, so setting this to 'arrow on Emacs 21 sounds like a good idea."
  :group 'idlwave-shell-highlighting-and-faces
  :type '(choice
	  (const :tag "No marking" nil)
	  (const :tag "Use overlay arrow" arrow)
	  (const :tag "Highlight with face" face)
	  (const :tag "Face or arrow." t)))

(defcustom idlwave-shell-overlay-arrow ">"
  "*The overlay arrow to display at source lines where execution halts.
We use a single character by default, since the main block of IDL procedures
often has no indentation.  Where possible, IDLWAVE will use overlays to
display the stop-lines.  The arrow is only used on character-based terminals.
See also `idlwave-shell-use-overlay-arrow'."
  :group 'idlwave-shell-highlighting-and-faces
  :type 'string)

(defcustom idlwave-shell-stop-line-face 'highlight
  "*The face for `idlwave-shell-stop-line-overlay'.
Allows you to choose the font, color and other properties for
line where IDL is stopped.  See also `idlwave-shell-mark-stop-line'."
  :group 'idlwave-shell-highlighting-and-faces
  :type 'symbol)

(defcustom idlwave-shell-electric-stop-color "Violet"
  "*The color for the default face or overlay arrow when stopped."
  :group 'idlwave-shell-highlighting-and-faces
  :type 'string)

(defcustom idlwave-shell-electric-stop-line-face
  (prog1
      (copy-face 'modeline 'idlwave-shell-electric-stop-line)
    (set-face-background 'idlwave-shell-electric-stop-line
			 idlwave-shell-electric-stop-color)
    (condition-case nil
	(set-face-foreground 'idlwave-shell-electric-stop-line nil)
      (error nil)))
  "*The face for `idlwave-shell-stop-line-overlay' when in electric debug mode.
Allows you to choose the font, color and other properties for the line
where IDL is stopped, when in Electric Debug Mode."
  :group 'idlwave-shell-highlighting-and-faces
  :type 'symbol)

(defcustom idlwave-shell-mark-breakpoints t
  "*Non-nil means, mark breakpoints in the source files.
Valid values are:
nil        Do not mark breakpoints.
'face      Highlight line with `idlwave-shell-breakpoint-face'.
'glyph     Red dot at the beginning of line.  If the display does not
           support glyphs, will use 'face instead.
t          Glyph when possible, otherwise face (same effect as 'glyph)."
  :group 'idlwave-shell-highlighting-and-faces
  :type '(choice
	  (const :tag "No marking" nil)
	  (const :tag "Highlight with face" face)
	  (const :tag "Display glyph (red dot)" glyph)
	  (const :tag "Glyph or face." t)))

(defvar idlwave-shell-use-breakpoint-glyph t
  "Obsolete variable.  See `idlwave-shell-mark-breakpoints'.")

(defcustom idlwave-shell-breakpoint-face 'idlwave-shell-bp
  "*The face for breakpoint lines in the source code.
Allows you to choose the font, color and other properties for
lines which have a breakpoint.  See also `idlwave-shell-mark-breakpoints'."
  :group 'idlwave-shell-highlighting-and-faces
  :type 'symbol)

(if (not idlwave-shell-have-new-custom)
    ;; Just copy the underline face to be on the safe side.
    (copy-face 'underline 'idlwave-shell-bp)
  ;; We have the new customize - use it to define a customizable face
  (defface idlwave-shell-bp
    '((((class color)) (:foreground "Black" :background "Pink"))
      (t (:underline t)))
    "Face for highlighting lines with breakpoints."
    :group 'idlwave-shell-highlighting-and-faces))

(defcustom idlwave-shell-disabled-breakpoint-face
  'idlwave-shell-disabled-bp
  "*The face for disabled breakpoint lines in the source code.
Allows you to choose the font, color and other properties for
lines which have a breakpoint.  See also `idlwave-shell-mark-breakpoints'."
  :group 'idlwave-shell-highlighting-and-faces
  :type 'symbol)

(if (not idlwave-shell-have-new-custom)
    ;; Just copy the underline face to be on the safe side.
    (copy-face 'underline 'idlwave-shell-disabled-bp)
  ;; We have the new customize - use it to define a customizable face
  (defface idlwave-shell-disabled-bp
    '((((class color)) (:foreground "Black" :background "gray"))
      (t (:underline t)))
    "Face for highlighting lines with breakpoints."
    :group 'idlwave-shell-highlighting-and-faces))


(defcustom idlwave-shell-expression-face 'secondary-selection
  "*The face for `idlwave-shell-expression-overlay'.
Allows you to choose the font, color and other properties for
the expression printed by IDL."
  :group 'idlwave-shell-highlighting-and-faces
  :type 'symbol)

(defcustom idlwave-shell-output-face 'secondary-selection
  "*The face for `idlwave-shell-output-overlay'.
Allows you to choose the font, color and other properties for
the expression output by IDL."
  :group 'idlwave-shell-highlighting-and-faces
  :type 'symbol)

;;; End user customization variables

;;; External variables
(defvar comint-last-input-start)
(defvar comint-last-input-end)

;; Other variables
(defvar idlwave-shell-temp-pro-file nil
  "Absolute pathname for temporary IDL file for compiling regions")

(defvar idlwave-shell-temp-rinfo-save-file nil
  "Absolute pathname for temporary IDL file save file for routine_info.
This is used to speed up the reloading of the routine info procedure
before use by the shell.")

(defun idlwave-shell-temp-file (type)
  "Return a temp file, creating it if necessary.

TYPE is either 'pro' or 'rinfo', and `idlwave-shell-temp-pro-file' or
`idlwave-shell-temp-rinfo-save-file' is set (respectively)."
  (cond
   ((eq type 'rinfo)
    (or idlwave-shell-temp-rinfo-save-file
	(setq idlwave-shell-temp-rinfo-save-file
	      (idlwave-shell-make-temp-file idlwave-shell-temp-pro-prefix))))
   ((eq type 'pro)
    (or idlwave-shell-temp-pro-file
	(setq idlwave-shell-temp-pro-file
	      (idlwave-shell-make-temp-file idlwave-shell-temp-pro-prefix))))
   (t (error "Wrong argument (idlwave-shell-temp-file): %s"
	     (symbol-name type)))))


(defun idlwave-shell-make-temp-file (prefix)
  "Create a temporary file."
  ; Hard coded make-temp-file for Emacs<21
  (if (fboundp 'make-temp-file)
      (make-temp-file prefix)
    (let (file
	  (temp-file-dir (if (boundp 'temporary-file-directory)
			     temporary-file-directory
			   "/tmp")))
      (while (condition-case ()
		 (progn
		   (setq file
			 (make-temp-name
			  (expand-file-name prefix temp-file-dir)))
                   (if (featurep 'xemacs)
		       (write-region "" nil file nil 'silent nil)
		     (write-region "" nil file nil 'silent nil 'excl))
		   nil)
	       (file-already-exists t))
	;; the file was somehow created by someone else between
	;; `make-temp-name' and `write-region', let's try again.
	nil)
      file)))


(defvar idlwave-shell-dirstack-query "cd,current=___cur & print,___cur"
  "Command used by `idlwave-shell-resync-dirs' to query IDL for
the directory stack.")

(defvar idlwave-shell-path-query "print,'PATH:<'+transpose(expand_path(!PATH,/ARRAY))+'>' & print,'SYSDIR:<'+!dir+'>'"

  "The command which gets !PATH and !DIR info from the shell.")

(defvar idlwave-shell-mode-line-info nil
  "Additional info displayed in the mode line.")

(defvar idlwave-shell-default-directory nil
  "The default directory in the idlwave-shell buffer, of outside use.")

(defvar idlwave-shell-last-save-and-action-file nil
  "The last file which was compiled with `idlwave-shell-save-and-...'.")

;; Highlighting uses overlays.  When necessary, require the emulation.
(if (not (fboundp 'make-overlay))
    (condition-case nil
	(require 'overlay)
      (error nil)))

(defvar idlwave-shell-stop-line-overlay nil
  "The overlay for where IDL is currently stopped.")
(defvar idlwave-shell-is-stopped nil)
(defvar idlwave-shell-expression-overlay nil
  "The overlay for the examined expression.")
(defvar idlwave-shell-output-overlay nil
  "The overlay for the last IDL output.")

;; If these were already overlays, delete them.  This probably means that we
;; are reloading this file.
(if (overlayp idlwave-shell-stop-line-overlay)
    (delete-overlay idlwave-shell-stop-line-overlay))
(if (overlayp idlwave-shell-expression-overlay)
    (delete-overlay idlwave-shell-expression-overlay))
(if (overlayp idlwave-shell-output-overlay)
    (delete-overlay idlwave-shell-output-overlay))

;; Set to nil initially
(setq idlwave-shell-stop-line-overlay nil
      idlwave-shell-expression-overlay nil
      idlwave-shell-output-overlay nil)

;; Define the shell stop overlay.  When left nil, the arrow will be used.
(cond
 ((or (null idlwave-shell-mark-stop-line)
      (eq idlwave-shell-mark-stop-line 'arrow))
  ;; Leave the overlay nil
  nil)

 ((eq idlwave-shell-mark-stop-line 'face)
  ;; Try to use a face.  If not possible, arrow will be used anyway
  ;; So who can display faces?
  (when (or (featurep 'xemacs)            ; XEmacs can do also ttys
	    (fboundp 'tty-defined-colors) ; Emacs 21 as well
	    window-system)                ; Window systems always
    (progn
      (setq idlwave-shell-stop-line-overlay (make-overlay 1 1))
      (overlay-put idlwave-shell-stop-line-overlay
		   'face idlwave-shell-stop-line-face))))

 (t
  ;; IDLWAVE may decide.  Will use a face on window systems, arrow elsewhere
  (if window-system
      (progn
	(setq idlwave-shell-stop-line-overlay (make-overlay 1 1))
	(overlay-put idlwave-shell-stop-line-overlay
		     'face idlwave-shell-stop-line-face)))))

;; Now the expression and output overlays
(setq idlwave-shell-expression-overlay (make-overlay 1 1))
(overlay-put idlwave-shell-expression-overlay
	     'face idlwave-shell-expression-face)
(overlay-put idlwave-shell-expression-overlay
	     'priority 1)
(setq idlwave-shell-output-overlay (make-overlay 1 1))
(overlay-put idlwave-shell-output-overlay
	     'face idlwave-shell-output-face)

(copy-face idlwave-shell-stop-line-face
	   'idlwave-shell-pending-stop)
(copy-face idlwave-shell-electric-stop-line-face
	   'idlwave-shell-pending-electric-stop)
(set-face-background 'idlwave-shell-pending-stop "gray70")
(set-face-background 'idlwave-shell-pending-electric-stop "gray70")



(defvar idlwave-shell-bp-query "help,/breakpoints"
  "Command to obtain list of breakpoints.")

(defvar idlwave-shell-command-output nil
  "String for accumulating current command output.")

(defvar idlwave-shell-post-command-hook nil
  "Lisp list expression or function to run when an IDL command is finished.
The current command is finished when the IDL prompt is displayed.
This is evaluated if it is a list or called with funcall.")

(defvar idlwave-shell-sentinel-hook nil
  "Hook run when the IDL process exits.")

(defvar idlwave-shell-hide-output nil
  "If non-nil the process output is not inserted into the output buffer.")

(defvar idlwave-shell-show-if-error nil
  "If non-nil the process output is inserted into the output buffer if
it contains an error message, even if hide-output is non-nil.")

(defvar idlwave-shell-accumulation nil
  "Accumulate last line of output.")

(defvar idlwave-shell-command-line-to-execute nil)
(defvar idlwave-shell-cleanup-hook nil
  "List of functions to do cleanup when the shell exits.")

(defvar idlwave-shell-pending-commands nil
  "List of commands to be sent to IDL.
Each element of the list is list of \(CMD PCMD HIDE\), where CMD is a
string to be sent to IDL and PCMD is a post-command to be placed on
`idlwave-shell-post-command-hook'.  If HIDE is non-nil, hide the output
from command CMD.  PCMD and HIDE are optional.")

(defun idlwave-shell-buffer ()
  "Name of buffer associated with IDL process.
The name of the buffer is made by surrounding `idlwave-shell-process-name'
with `*'s."
  (concat "*" idlwave-shell-process-name "*"))

(defvar idlwave-shell-ready nil
  "If non-nil can send next command to IDL process.")

;;; The following are the types of messages we attempt to catch to
;;; resync our idea of where IDL execution currently is.
;;;

(defvar idlwave-shell-halt-frame nil
  "The frame associated with halt/breakpoint messages.")

(defvar idlwave-shell-step-frame nil
  "The frame associated with step messages.")

(defvar idlwave-shell-trace-frame nil
  "The frame associated with trace messages.")

(defconst idlwave-shell-halt-messages
  '("^% Interrupted at:"
    "^% Stepped to:"
    "^% Skipped to:"
    "^% Stop encountered:"
    )
  "*A list of regular expressions matching IDL messages.
These are the messages containing file and line information where
IDL is currently stopped.")


(defconst idlwave-shell-halt-messages-re
  (mapconcat 'identity idlwave-shell-halt-messages "\\|")
  "The regular expression computed from `idlwave-shell-halt-messages'.")

(defconst idlwave-shell-trace-message-re
  "^% At "    ;; First line of a trace message
  "*A regular expression matching IDL trace messages.  These are the
messages containing file and line information of a current
traceback.")

(defconst idlwave-shell-step-messages
  '("^% Stepped to:"
    )
  "*A list of regular expressions matching stepped execution messages.
These are IDL messages containing file and line information where
IDL has currently stepped.")

(defvar idlwave-shell-break-message "^% Breakpoint at:"
  "*Regular expression matching an IDL breakpoint message line.")

(defconst idlwave-shell-electric-debug-help
  "   ==> IDLWAVE Electric Debug Mode Help <==

 Break Point Setting and Clearing:
  b   	     Set breakpoint ([C-u b] for conditional, [C-n b] nth hit, etc.).
  d   	     Clear nearby breakpoint.
  a          Clear all breakpoints.
  i   	     Set breakpoint in routine named here.
  j          Set breakpoint at beginning of containing routine.
  \\          Toggle breakpoint disable
  ]          Go to next breakpoint in file.
  [          Go to previous breakpoint in file.

 Stepping, Continuing, and the Stack:
  s or SPACE Step, into function calls.
  n   	     Step, over function calls.
  k   	     Skip one statement.
  m   	     Continue to end of function.
  o   	     Continue past end of function.
  u   	     Continue to end of block.
  h   	     Continue to line at cursor position.
  r   	     Continue execution to next breakpoint, if any.
  + or =     Show higher level in calling stack.
  - or _     Show lower level in calling stack.

 Examining Expressions (with prefix for examining the region):
  p   	     Print expression near point or in region ([C-u p]).
  ?          Help on expression near point or in region ([C-u ?]).
  x          Examine expression near point or in region ([C-u x]) with
             letter completion of the examine type.
  e          Prompt for an expression to print.

 Miscellaneous:
  q   	     Quit - end debugging session and return to the Shell's main level.
  v   	     Turn Electric Debugging Mode off (C-c C-d C-v to return).
  t   	     Print a calling-level traceback in the shell.
  z   	     Reset IDL.
  C-?        Show this help menu.")

(defvar idlwave-shell-bp-alist)
;(defvar idlwave-shell-post-command-output)
(defvar idlwave-shell-sources-alist)
(defvar idlwave-shell-menu-def)
(defvar idlwave-shell-mode-menu)
(defvar idlwave-shell-initial-commands)
(defvar idlwave-shell-syntax-error)
(defvar idlwave-shell-other-error)
(defvar idlwave-shell-error-buffer)
(defvar idlwave-shell-error-last)
(defvar idlwave-shell-bp-buffer)
(defvar idlwave-shell-sources-query)
(defvar idlwave-shell-mode-map)
(defvar idlwave-shell-calling-stack-index)
(defvar idlwave-shell-only-prompt-pattern nil)
(defvar tool-bar-map)

(define-derived-mode idlwave-shell-mode comint-mode "IDL-Shell"
  "Major mode for interacting with an inferior IDL process.

1. Shell Interaction
   -----------------
   RET after the end of the process' output sends the text from the
   end of process to the end of the current line.  RET before end of
   process output copies the current line (except for the prompt) to
   the end of the buffer.

   Command history, searching of previous commands, command line
   editing are available via the comint-mode key bindings, by default
   mostly on the key `C-c'.  Command history is also available with
   the arrow keys UP and DOWN.

2. Completion
   ----------
   TAB and M-TAB do completion of IDL routines, classes and keywords -
   similar to M-TAB in `idlwave-mode'.  In executive commands and
   strings, it completes file names.  Abbreviations are also expanded
   like in `idlwave-mode'.

3. Routine Info
   ------------
   `\\[idlwave-routine-info]' displays information about an IDL routine near point,
   just like in `idlwave-mode'.  The module used is the one at point or
   the one whose argument list is being edited.
   To update IDLWAVE's knowledge about compiled or edited modules, use
   \\[idlwave-update-routine-info].
   \\[idlwave-find-module] find the source of a module.
   \\[idlwave-resolve] tells IDL to compile an unresolved module.
   \\[idlwave-context-help] shows the online help on the item at
   point, if online help has been installed.


4. Debugging
   ---------
   A complete set of commands for compiling and debugging IDL programs
   is available from the menu.  Also keybindings starting with a
   `C-c C-d' prefix are available for most commands in the *idl* buffer
   and also in source buffers.  The best place to learn about the
   keybindings is again the menu.

   On Emacs versions where this is possible, a debugging toolbar is
   installed.

   When IDL is halted in the middle of a procedure, the corresponding
   line of that procedure file is displayed with an overlay in another
   window.  Breakpoints are also highlighted in the source.

   \\[idlwave-shell-resync-dirs] queries IDL in order to change Emacs current directory
   to correspond to the IDL process current directory.

5. Expression Examination
   ----------------------

   Expressions near point can be examined with print,
   \\[idlwave-shell-print] or \\[idlwave-shell-mouse-print] with the
   mouse, help, \\[idlwave-shell-help-expression] or
   \\[idlwave-shell-mouse-help] with the mouse, or with a
   configurable set of custom examine commands using
   \\[idlwave-shell-examine-select].  The mouse examine commands can
   also work by click and drag, to select an expression for
   examination.

6. Hooks
   -----
   Turning on `idlwave-shell-mode' runs `comint-mode-hook' and
   `idlwave-shell-mode-hook' (in that order).

7. Documentation and Customization
   -------------------------------
   Info documentation for this package is available.  Use \\[idlwave-info]
   to display (complain to your sysadmin if that does not work).
   For PostScript and HTML versions of the documentation, check IDLWAVE's
   homepage at URL `http://idlwave.org'.
   IDLWAVE has customize support - see the group `idlwave'.

8. Keybindings
   -----------
\\{idlwave-shell-mode-map}"
  :abbrev-table idlwave-mode-abbrev-table
  (idlwave-setup) ; Make sure config files and paths, etc. are available.
  (unless (file-name-absolute-p idlwave-shell-command-history-file)
    (setq idlwave-shell-command-history-file
	  (expand-file-name idlwave-shell-command-history-file
			    idlwave-config-directory)))

  (setq comint-prompt-regexp idlwave-shell-prompt-pattern)
  (setq comint-process-echoes t)

  ;; Can not use history expansion because "!" is used for system variables.
  (setq comint-input-autoexpand nil)
  ;; (setq comint-input-ring-size 64)

  (set (make-local-variable 'completion-ignore-case) t)
  (set (make-local-variable 'comint-completion-addsuffix) '("/" . ""))
  (setq comint-input-ignoredups t)
  (setq idlwave-shell-mode-line-info nil)
  (setq mode-line-format
	'(""
	  mode-line-modified
	  mode-line-buffer-identification
	  "   "
	  global-mode-string
	  "   %[("
	  mode-name
	  mode-line-process
	  minor-mode-alist
	  "%n"
	  ")%]-"
	  idlwave-shell-mode-line-info
	  "---"
	  (line-number-mode "L%l--")
	  (column-number-mode "C%c--")
	  (-3 . "%p")
	  "-%-"))
  ;; (make-local-variable 'idlwave-shell-bp-alist)
  (setq idlwave-shell-halt-frame nil
        idlwave-shell-trace-frame nil
        idlwave-shell-command-output nil
        idlwave-shell-step-frame nil)
  (idlwave-shell-display-line nil)
  (setq idlwave-shell-calling-stack-index 0)
  (setq idlwave-shell-only-prompt-pattern
	(concat "\\`[ \t\n]*"
		(substring idlwave-shell-prompt-pattern 1)
		"[ \t\n]*\\'"))

  (when idlwave-shell-query-for-class
      (add-to-list (make-local-variable 'idlwave-determine-class-special)
		   'idlwave-shell-get-object-class)
      (setq idlwave-store-inquired-class t))

  ;; Make sure comint-last-input-end does not go to beginning of
  ;; buffer (in case there were other processes already in this buffer).
  (set-marker comint-last-input-end (point))
  (setq idlwave-idlwave_routine_info-compiled nil)
  (setq idlwave-shell-ready nil)
  (setq idlwave-shell-bp-alist nil)
  (idlwave-shell-update-bp-overlays) ; Throw away old overlays
  (setq idlwave-shell-post-command-hook nil ;clean up any old stuff
	idlwave-shell-sources-alist nil)
  (setq idlwave-shell-default-directory default-directory)
  (setq idlwave-shell-hide-output nil)

  ;; NB: `make-local-hook' needed for older/alternative Emacs compatibility
  ;; (make-local-hook 'kill-buffer-hook)
  (add-hook 'kill-buffer-hook 'idlwave-shell-kill-shell-buffer-confirm
	    nil 'local)
  (add-hook 'kill-buffer-hook 'idlwave-shell-delete-temp-files nil 'local)
  (add-hook 'kill-emacs-hook 'idlwave-shell-delete-temp-files)
  (easy-menu-add idlwave-shell-mode-menu idlwave-shell-mode-map)

  ;; Set the optional comint variables
  (when idlwave-shell-comint-settings
    (let ((list idlwave-shell-comint-settings) entry)
      (while (setq entry (pop list))
	(set (make-local-variable (car entry)) (cdr entry)))))


  (unless (memq 'comint-carriage-motion
		(default-value 'comint-output-filter-functions))
    ;; Strip those pesky ctrl-m's.
    (add-hook 'comint-output-filter-functions
	      (lambda (string)
		(when (string-match "\r" string)
		  (let ((pmark (process-mark (get-buffer-process
					      (current-buffer)))))
		    (save-excursion
		      ;; bare CR -> delete preceding line
		      (goto-char comint-last-output-start)
		      (while (search-forward "\r" pmark t)
			(delete-region (point) (line-beginning-position)))))))
		'append 'local)
    (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m nil 'local))

  ;; Python-mode, bundled with many Emacs installs, quite cavalierly
  ;; adds this function to the global default hook.  It interferes
  ;; with overlay-arrows.
  (remove-hook 'comint-output-filter-functions 'py-pdbtrack-track-stack-file)

  ;; IDLWAVE syntax, and turn on abbreviations
  (set (make-local-variable 'comment-start) ";")
  (setq abbrev-mode t)

  ;; NB: `make-local-hook' needed for older/alternative Emacs compatibility
  ;; make-local-hook 'post-command-hook)
  (add-hook 'post-command-hook 'idlwave-command-hook nil t)

  ;; Read the command history?
  (when (and idlwave-shell-save-command-history
	     (stringp idlwave-shell-command-history-file))
    (set (make-local-variable 'comint-input-ring-file-name)
	 idlwave-shell-command-history-file)
    (if (file-regular-p idlwave-shell-command-history-file)
	(comint-read-input-ring)))

  ;; Turn off the non-debug toolbar buttons (open,save,etc.)
  (set (make-local-variable 'tool-bar-map) nil)

  (idlwave-shell-send-command idlwave-shell-initial-commands nil 'hide)
  ;; Turn off IDL's ^d interpreting, and define a system
  ;; variable which knows the version of IDLWAVE
  (idlwave-shell-send-command
   (format "defsysv,'!idlwave_version','%s',1" idlwave-mode-version)
   nil 'hide)
  ;; Read the paths, and save if they changed
  (idlwave-shell-send-command idlwave-shell-path-query
			      'idlwave-shell-get-path-info
			      'hide))

(defvar idlwave-system-directory)
(defun idlwave-shell-get-path-info (&optional no-write)
  "Get the path lists, writing to file unless NO-WRITE is set."
  (let* ((rpl (idlwave-shell-path-filter))
	 (sysdir (car rpl))
	 (dirs (cdr rpl))
	 (old-path-alist idlwave-path-alist)
	 (old-sys-dir idlwave-system-directory)
	 path-changed sysdir-changed)
    (when sysdir
      (setq idlwave-system-directory sysdir)
      (if (setq sysdir-changed
		(not (string= idlwave-system-directory old-sys-dir)))
	  (put 'idlwave-system-directory 'from-shell t)))
    ;; Preserve any existing flags
    (setq idlwave-path-alist
	  (mapcar (lambda (x)
		    (let ((old-entry (assoc x old-path-alist)))
		      (if old-entry
			  (cons x (cdr old-entry))
			(list x))))
		  dirs))
    (if (setq path-changed (not (equal idlwave-path-alist old-path-alist)))
	(put 'idlwave-path-alist 'from-shell t))
    (if idlwave-path-alist
	(if (and (not no-write)
		 idlwave-auto-write-paths
		 (or sysdir-changed path-changed)
		 (not idlwave-library-path))
	    (idlwave-write-paths))
      ;; Fall back
      (setq idlwave-path-alist old-path-alist))))

(if (not (fboundp 'idl-shell))
    (fset 'idl-shell 'idlwave-shell))

(defvar idlwave-shell-idl-wframe nil
  "Frame for displaying the IDL shell window.")
(defvar idlwave-shell-display-wframe nil
  "Frame for displaying the IDL source files.")

(defvar idlwave-shell-calling-stack-index 0)
(defvar idlwave-shell-calling-stack-routine nil)

(defun idlwave-shell-source-frame ()
  "Return the frame to be used for source display."
  (if idlwave-shell-use-dedicated-frame
      ;; We want separate frames for source and shell
      (if (frame-live-p idlwave-shell-display-wframe)
	  ;; The frame exists, so we use it.
	  idlwave-shell-display-wframe
	;; The frame does not exist.  We use the current frame.
	;; However, if the current is the shell frame, we make a new frame,
	;; or recycle the first existing visible frame
	(setq idlwave-shell-display-wframe
	      (if (eq (selected-frame) idlwave-shell-idl-wframe)
		  (or
		   (let ((flist (visible-frame-list))
			 (frame (selected-frame)))
		     (catch 'exit
		       (while flist
			 (if (not (eq (car flist)
				      idlwave-shell-idl-wframe))
			     (throw 'exit (car flist))
			   (setq flist (cdr flist))))))
		   (make-frame))
		(selected-frame))))))

(defun idlwave-shell-shell-frame ()
  "Return the frame to be used for the shell buffer."
  (if idlwave-shell-use-dedicated-frame
      ;; We want a dedicated frame
      (if (frame-live-p idlwave-shell-idl-wframe)
	  ;; It does exist, so we use it.
	  idlwave-shell-idl-wframe
	;; It does not exist.  Check if we have a source frame.
	(if (not (frame-live-p idlwave-shell-display-wframe))
	    ;; We do not have a source frame, so we use this one.
	    (setq idlwave-shell-display-wframe (selected-frame)))
	;; Return a new frame
	(setq idlwave-shell-idl-wframe
	      (make-frame idlwave-shell-frame-parameters)))))

;;;###autoload
(defun idlwave-shell (&optional arg quick)
  "Run an inferior IDL, with I/O through buffer `(idlwave-shell-buffer)'.
If buffer exists but shell process is not running, start new IDL.
If buffer exists and shell process is running, just switch to the buffer.

When called with a prefix ARG, or when `idlwave-shell-use-dedicated-frame'
is non-nil, the shell buffer and the source buffers will be in
separate frames.

The command to run comes from variable `idlwave-shell-explicit-file-name',
with options taken from `idlwave-shell-command-line-options'.

The buffer is put in `idlwave-shell-mode', providing commands for sending
input and controlling the IDL job.  See help on `idlwave-shell-mode'.
See also the variable `idlwave-shell-prompt-pattern'.

\(Type \\[describe-mode] in the shell buffer for a list of commands.)"
  (interactive "P")
  (if (eq arg 'quick)
      (progn
	(let ((idlwave-shell-use-dedicated-frame nil))
	  (idlwave-shell nil)
	  (delete-other-windows))
	(and idlwave-shell-use-dedicated-frame
	     (setq idlwave-shell-idl-wframe (selected-frame)))
	(add-hook 'idlwave-shell-sentinel-hook
		  'save-buffers-kill-emacs t))

    ;; A non-nil arg means, we want a dedicated frame.  This will last
    ;; for the current editing session.
    (if arg (setq idlwave-shell-use-dedicated-frame t))
    (if (equal arg '(16)) (setq idlwave-shell-use-dedicated-frame nil))

    ;; Check if the process still exists.  If not, create it.
    (unless (comint-check-proc (idlwave-shell-buffer))
      (let* ((prg (or idlwave-shell-explicit-file-name "idl"))
	     (buf (apply 'make-comint
			 idlwave-shell-process-name prg nil
			 (if (stringp idlwave-shell-command-line-options)
			     (idlwave-split-string
			      idlwave-shell-command-line-options)
			   idlwave-shell-command-line-options)))
	     (process (get-buffer-process buf)))
	(setq idlwave-idlwave_routine_info-compiled nil)
	(set-process-filter process 'idlwave-shell-filter)
	(set-process-sentinel process 'idlwave-shell-sentinel)
	(set-buffer buf)
	(idlwave-shell-mode)))
    (let ((window (idlwave-display-buffer (idlwave-shell-buffer) nil
					  (idlwave-shell-shell-frame)))
	  (current-window (selected-window)))
      (select-window window)
      (goto-char (point-max))
      (if idlwave-shell-use-dedicated-window
	  (set-window-dedicated-p window t))
      (select-window current-window)
      (if idlwave-shell-ready
	  (raise-frame (window-frame window)))
      (if (eq (selected-frame) (window-frame window))
	  (select-window window))))
  ;; Save the paths at the end, if they are from the Shell and new.
  (add-hook 'idlwave-shell-sentinel-hook
	    (lambda ()
	      (if (and
		   idlwave-auto-write-paths
		   idlwave-path-alist
		   (not idlwave-library-path)
		   (get 'idlwave-path-alist 'from-shell))
		  (idlwave-write-paths)))))

(defun idlwave-shell-recenter-shell-window (&optional arg)
  "Run `idlwave-shell', but make sure the current window stays selected."
  (interactive "P")
  (let ((window (selected-window)))
    (idlwave-shell arg)
    (select-window window)))

(defun idlwave-shell-hide-p (type &optional list)
  "Whether to hide this type of command.
Return either nil or 'hide."
  (let ((list (or list idlwave-shell-show-commands)))
    (if (listp list)
      (if (not (memq type list)) 'hide))))

(defun idlwave-shell-add-or-remove-show (type)
  "Add or remove a show command from the list."
  (if (listp idlwave-shell-show-commands)
      (setq idlwave-shell-show-commands
	    (if (memq type idlwave-shell-show-commands)
		(delq type idlwave-shell-show-commands)
	      (add-to-list'idlwave-shell-show-commands type)))
    (setq idlwave-shell-show-commands (list type))))


(defun idlwave-shell-send-command (&optional cmd pcmd hide preempt
					     show-if-error)
  "Send a command to IDL process.

\(CMD PCMD HIDE\) are placed at the end of `idlwave-shell-pending-commands'.
If IDL is ready the first command in `idlwave-shell-pending-commands',
CMD, is sent to the IDL process.

If optional second argument PCMD is non-nil it will be placed on
`idlwave-shell-post-command-hook' when CMD is executed.

If the optional third argument HIDE is non-nil, then hide output from
CMD, unless it is the symbol 'mostly, in which case only output
beginning with \"%\" is hidden, and all other output (i.e., the
results of a PRINT command), is shown.  This helps with, e.g.,
stepping through code with output.

If optional fourth argument PREEMPT is non-nil CMD is put at front of
`idlwave-shell-pending-commands'.  If PREEMPT is 'wait, wait for all
output to complete and the next prompt to arrive before returning
\(useful if you need an answer now\).  IDL is considered ready if the
prompt is present and if `idlwave-shell-ready' is non-nil.

If SHOW-IF-ERROR is non-nil, show the output if it contains an error
message, independent of what HIDE is set to."

;  (setq hide nil)  ;  FIXME: turn this on for debugging only
;  (if (null cmd)
;      (progn
;	(message "SENDING Pending commands: %s"
;		 (prin1-to-string idlwave-shell-pending-commands)))
;  (message "SENDING %s|||%s" cmd pcmd))
  (if (and (symbolp idlwave-shell-show-commands)
	   (eq idlwave-shell-show-commands 'everything))
      (setq hide nil))
  (let ((save-buffer (current-buffer))
	buf proc)
    ;; Get or make the buffer and its process
    (if (or (not (setq buf (get-buffer (idlwave-shell-buffer))))
	    (not (setq proc (get-buffer-process buf))))
	(if (not idlwave-shell-automatic-start)
	    (error "%s"
	     (substitute-command-keys
	      "You need to first start an IDL shell with \\[idlwave-shell]"))
	  (idlwave-shell-recenter-shell-window)
	  (setq buf (get-buffer (idlwave-shell-buffer)))
	  (if (or (not (setq buf (get-buffer (idlwave-shell-buffer))))
		  (not (setq proc (get-buffer-process buf))))
	      ;; Still nothing
	      (error "Problem with autostarting IDL shell"))))
    (when (or cmd idlwave-shell-pending-commands)
      (set-buffer buf)
      ;; To make this easy, always push CMD onto pending commands
      (if cmd
	  (setq idlwave-shell-pending-commands
		(if preempt
		    ;; Put at front.
		    (append (list (list cmd pcmd hide show-if-error))
			    idlwave-shell-pending-commands)
		  ;; Put at end.
		  (append idlwave-shell-pending-commands
			  (list (list cmd pcmd hide show-if-error))))))
      ;; Check if IDL ready
      (let ((save-point (point-marker)))
	(goto-char (process-mark proc))
	(if (and idlwave-shell-ready
		 ;; Check for IDL prompt
		 (prog2
		   (forward-line 0)
		   ;; (beginning-of-line) ; Changed for Emacs 21
		   (looking-at idlwave-shell-prompt-pattern)
		   (goto-char (process-mark proc))))
	    ;; IDL ready for command, execute it
	    (let* ((lcmd (car idlwave-shell-pending-commands))
		   (cmd (car lcmd))
		   (pcmd (nth 1 lcmd))
		   (hide (nth 2 lcmd))
		   (show-if-error (nth 3 lcmd)))
	      ;; If this is an executive command, reset the stack pointer
	      (if (eq (string-to-char cmd) ?.)
		  (setq idlwave-shell-calling-stack-index 0))
	      ;; Set post-command
	      (setq idlwave-shell-post-command-hook pcmd)
	      ;; Output hiding
	      (setq idlwave-shell-hide-output hide)
	      ;;Showing errors
	      (setq idlwave-shell-show-if-error show-if-error)
	      ;; Pop command
	      (setq idlwave-shell-pending-commands
		    (cdr idlwave-shell-pending-commands))
	      ;; Send command for execution
	      (set-marker comint-last-input-start (point))
	      (set-marker comint-last-input-end (point))
	      (comint-simple-send proc cmd)
	      (setq idlwave-shell-ready nil)
	      (if (equal preempt 'wait) ; Get all the output at once
		(while (not idlwave-shell-ready)
		  (when (not (accept-process-output proc 6)) ; long wait
		    (setq idlwave-shell-pending-commands nil)
		    (error "Process timed out"))))))
	(goto-char save-point))
      (set-buffer save-buffer))))

(defun idlwave-shell-send-char (c &optional error)
  "Send one character to the shell, without a newline."
  (interactive "cChar to send to IDL: \np")
  (let ((errf (if error 'error 'message))
	buf proc)
    (if (or (not (setq buf (get-buffer (idlwave-shell-buffer))))
	    (not (setq proc (get-buffer-process buf))))
	(funcall errf "Shell is not running"))
    (if (equal c ?\C-g)
	(funcall errf "Abort")
      (comint-send-string proc (char-to-string c)))))

(defvar idlwave-shell-char-mode-active)
(defun idlwave-shell-input-mode-magic (string)
  "Check STRING for magic words and toggle character input mode.
See also the variable `idlwave-shell-input-mode-spells'."
  (cond
   ((string-match (car idlwave-shell-input-mode-spells) string)
    (call-interactively 'idlwave-shell-send-char))
   ((and (boundp 'idlwave-shell-char-mode-active)
	 (string-match (nth 2 idlwave-shell-input-mode-spells) string))
    (setq idlwave-shell-char-mode-active 'exit))
   ((string-match (nth 1 idlwave-shell-input-mode-spells) string)
    ;; Set a timer which will soon start the character loop
    (if (fboundp 'start-itimer)
	(start-itimer "IDLWAVE Char Mode" 'idlwave-shell-char-mode-loop 0.5
		      nil nil t 'no-error)
      (run-at-time 0.5 nil 'idlwave-shell-char-mode-loop 'no-error)))))

(defvar keyboard-quit)
(defun idlwave-shell-char-mode-loop (&optional no-error)
  "Enter a loop which accepts single characters and sends them to IDL.
Characters are sent one by one, without newlines.  The loop is blocking
and intercepts all input events to Emacs.  You can use this command
to interact with the IDL command GET_KBRD.
The loop can be aborted by typing `C-g'.  The loop also exits automatically
when the IDL prompt gets displayed again after the current IDL command."
  (interactive)

  ;; First check if there is a shell waiting for input
  (let ((idlwave-shell-char-mode-active t)
	(errf (if no-error 'message 'error))
	buf proc c)
    (if (or (not (setq buf (get-buffer (idlwave-shell-buffer))))
	    (not (setq proc (get-buffer-process buf))))
	(funcall errf "Shell is not running"))
    (if idlwave-shell-ready
	(funcall errf "No IDL program seems to be waiting for input"))

    ;; OK, start the loop
    (message "Character mode on:  Sending single chars (`C-g' to exit)")
    (message
     (catch 'exit
       (while t
	 ;; Wait for input
	 ;; FIXME: Is it too dangerous to inhibit quit here?
	 (let ((inhibit-quit t))
	   ;; We wait and check frequently if we should abort
	   (while (sit-for 0.3)
	     (and idlwave-shell-ready
		  (throw 'exit "Character mode off (prompt displayed)"))
	     (and (eq idlwave-shell-char-mode-active 'exit)
		  (throw 'exit "Character mode off (closing spell incantation)")))
	   ;; Interpret input as a character - ignore non-char input
	   (condition-case nil
	       (setq c (read-char))
	     (error (ding) (throw 'exit "Character mode off")))
	   (cond
	    ((null c)               ; Non-char event: ignore
	     (ding))
	    ((equal c ?\C-g)        ; Abort the loop
	     (setq keyboard-quit nil)
	     (ding)
	     (throw 'exit "Character mode off (keyboard quit)"))
	    (t	                   ; Send the character and continue the loop
	     (comint-send-string proc (char-to-string c))))
	   (and (eq idlwave-shell-char-mode-active 'exit)
		(throw 'exit "Single char loop exited"))))))))

(defun idlwave-shell-move-or-history (up &optional arg)
  "When in last line of process buffer, do `comint-previous-input'.
Otherwise just move the line.  Move down unless UP is non-nil."
  (let* ((proc-pos (marker-position
		    (process-mark (get-buffer-process (current-buffer)))))
	 (arg (or arg 1))
	 (arg (if up arg (- arg))))
    (if (eq t idlwave-shell-arrows-do-history) (goto-char proc-pos))
    (if (and idlwave-shell-arrows-do-history
	     (>= (1+ (point-at-eol)) proc-pos))
	(comint-previous-input arg)
      (forward-line (- arg)))))

(defun idlwave-shell-up-or-history (&optional arg)
"When in last line of process buffer, move to previous input.
 Otherwise just go up one line."
  (interactive "p")
  (idlwave-shell-move-or-history t arg))

(defun idlwave-shell-down-or-history (&optional arg)
"When in last line of process buffer, move to next input.
 Otherwise just go down one line."
  (interactive "p")
  (idlwave-shell-move-or-history nil arg))

;; Newer versions of comint.el changed the name of comint-filter to
;; comint-output-filter.
(defun idlwave-shell-comint-filter (process string) nil)
(if (fboundp 'comint-output-filter)
    (fset 'idlwave-shell-comint-filter (symbol-function 'comint-output-filter))
  (fset 'idlwave-shell-comint-filter (symbol-function 'comint-filter)))

(defun idlwave-shell-is-running ()
  "Return t if the shell process is running."
  (eq (process-status idlwave-shell-process-name) 'run))

(defun idlwave-shell-filter-hidden-output (output)
  "Filter hidden output, leaving the good stuff.

Remove everything to the first newline, and all lines with % in front
of them, with optional follow-on lines starting with two spaces.  This
works well enough, since any print output typically arrives before
error messages, etc."
  (setq output (substring output (string-match "\n" output)))
  (while (string-match "\\(\n\\|\\`\\)%.*\\(\n  .*\\)*" output)
    (setq output (replace-match "" nil t output)))
  (unless
      (string-match idlwave-shell-only-prompt-pattern output)
    output))

(defvar idlwave-shell-hidden-output-buffer " *idlwave-shell-hidden-output*"
  "Buffer containing hidden output from IDL commands.")
(defvar idlwave-shell-current-state nil)

(defun idlwave-shell-filter (proc string)
  "Watch for IDL prompt and filter incoming text.
When the IDL prompt is received executes `idlwave-shell-post-command-hook'
and then calls `idlwave-shell-send-command' for any pending commands."
  ;; We no longer do the cleanup here - this is done by the process sentinel
  (if (eq (process-status idlwave-shell-process-name) 'run)
      ;; OK, process is still running, so we can use it.
      (let ((data (match-data)) p full-output)
	(unwind-protect
	    (progn
	      ;; Ring the bell if necessary
	      (while (setq p (string-match "\C-G" string))
		(ding)
		(aset string p ?\C-j ))
	      (if idlwave-shell-hide-output
		  (save-excursion
		    (while (setq p (string-match "\C-M" string))
		      (aset string p ?\  ))
		    (set-buffer
		     (get-buffer-create idlwave-shell-hidden-output-buffer))
		    (goto-char (point-max))
		    (insert string))
		(idlwave-shell-comint-filter proc string))
	      ;; Watch for magic - need to accumulate the current line
	      ;; since it may not be sent all at once.
	      (if (string-match "\n" string)
		  (progn
		    (if idlwave-shell-use-input-mode-magic
			(idlwave-shell-input-mode-magic
			 (concat idlwave-shell-accumulation string)))
		    (setq idlwave-shell-accumulation
			  (substring string
				     (progn (string-match "\\(.*[\n\r]+\\)*"
							  string)
					    (match-end 0)))))
		(setq idlwave-shell-accumulation
		      (concat idlwave-shell-accumulation string)))


              ;; ;; Test/Debug code
	      ;;(with-current-buffer
	      ;;    (get-buffer-create "*idlwave-shell-output*")
	      ;;  (goto-char (point-max))
	      ;;  (insert "\nReceived STRING\n===>\n" string "\n<====\n"))

	      ;; Check for prompt in current accumulating output
	      (when (setq idlwave-shell-ready
			  (string-match idlwave-shell-prompt-pattern
					idlwave-shell-accumulation))
		;; Gather the command output
		(if idlwave-shell-hide-output
		    (with-current-buffer idlwave-shell-hidden-output-buffer
		      (setq full-output (buffer-string))
		      (goto-char (point-max))
		      (re-search-backward idlwave-shell-prompt-pattern nil t)
		      (goto-char (match-end 0))
		      (setq idlwave-shell-command-output
			    (buffer-substring-no-properties
			     (point-min) (point)))
		      (delete-region (point-min) (point)))
		  (setq idlwave-shell-command-output
			(with-current-buffer (process-buffer proc)
			(buffer-substring-no-properties
			 (save-excursion
			   (goto-char (process-mark proc))
			   (forward-line 0) ; Emacs 21 (beginning-of-line nil)
			   (point))
			 comint-last-input-end))))

		;; Scan for state and do post commands - bracket
		;; them with idlwave-shell-ready=nil since they may
		;; call idlwave-shell-send-command themselves.
		(let ((idlwave-shell-ready nil))
		  (idlwave-shell-scan-for-state)
		  ;; Show the output in the shell if it contains an error
		  (if idlwave-shell-hide-output
		      (if (and idlwave-shell-show-if-error
			       (eq idlwave-shell-current-state 'error))
			  (idlwave-shell-comint-filter proc full-output)
			;; If it's only *mostly* hidden, filter % lines,
			;; and show anything that remains
			(if (eq idlwave-shell-hide-output 'mostly)
			    (let ((filtered
				   (idlwave-shell-filter-hidden-output
				    full-output)))
			      (if filtered
				  (idlwave-shell-comint-filter
				   proc filtered))))))

		  ;; Call the post-command hook
		  (if (listp idlwave-shell-post-command-hook)
		      (progn
			;;(message "Calling list")
			;;(prin1 idlwave-shell-post-command-hook)
			(eval idlwave-shell-post-command-hook))
		    ;;(message "Calling command function")
		    (funcall idlwave-shell-post-command-hook))

		  ;; Reset to default state for next command.
		  ;; Also we do not want to find this prompt again.
		  (setq idlwave-shell-accumulation nil
			idlwave-shell-command-output nil
			idlwave-shell-post-command-hook nil
			idlwave-shell-hide-output nil
			idlwave-shell-show-if-error nil))
		;; Done with post command.  Do pending command if
		;; any.
		(idlwave-shell-send-command)))
	  (store-match-data data)))))

(defun idlwave-shell-sentinel (process event)
  "The sentinel function for the IDLWAVE shell process."
  (let* ((buf (idlwave-shell-buffer))
	 (win (get-buffer-window buf)))
    (when (get-buffer buf)
      (with-current-buffer (idlwave-shell-buffer)
	(goto-char (point-max))
	(insert (format "\n\n  Process %s %s" process event))
	(if (and idlwave-shell-save-command-history
		 (stringp idlwave-shell-command-history-file))
	    (condition-case nil
		(comint-write-input-ring)
	      (error nil)))))

    (when (and (> (length (frame-list)) 1)
	       (frame-live-p idlwave-shell-idl-wframe))
      (delete-frame idlwave-shell-idl-wframe)
      (setq idlwave-shell-idl-wframe nil
	    idlwave-shell-display-wframe nil))
    (when (and (window-live-p win)
	       (not (one-window-p 'nomini)))
      (delete-window win))
    (idlwave-shell-cleanup)
    ;; Run the hook, if possible in the shell buffer.
    (if (get-buffer buf)
	(with-current-buffer buf
	  (run-hooks 'idlwave-shell-sentinel-hook))
      (run-hooks 'idlwave-shell-sentinel-hook))))

(defvar idlwave-shell-error-buffer " *idlwave-shell-errors*"
  "Buffer containing syntax errors from IDL compilations.")

;; FIXME: the following two variables do not currently allow line breaks
;; in module and file names.  I am not sure if it will be necessary to
;; change this.  Currently it seems to work the way it is.
(defvar idlwave-shell-syntax-error
  "^% Syntax error.\\s-*\n\\s-*At:\\s-*\\(.*\\),\\s-*Line\\s-*\\(.*\\)"
  "A regular expression to match an IDL syntax error.
The first pair matches the file name, the second pair matches the line
number.")

(defvar idlwave-shell-other-error
  "^% .*\n\\s-*At:\\s-*\\(.*\\),\\s-*Line\\s-*\\(.*\\)"
  "A regular expression to match any IDL error.")

(defvar idlwave-shell-halting-error
  "^% .*\n\\([^%].*\n\\)*% Execution halted at:\\(\\s-*\\S-+\\s-*[0-9]+\\s-*.*\\)\n"
  "A regular expression to match errors which halt execution.")

(defvar idlwave-shell-cant-continue-error
  "^% Can't continue from this point.\n"
  "A regular expression to match errors stepping errors.")

(defvar idlwave-shell-file-line-message
  (concat
   "\\("                                 ; program name group (1)
   "\\$MAIN\\$\\|"                   	 ; main level routine
   "\\<[a-zA-Z][a-zA-Z0-9_$:]*"          ; start with a letter followed by [..]
   "\\([ \t]*\n[ \t]*[a-zA-Z0-9_$:]+\\)*"; continuation lines program name (2)
   "\\)"                                 ; end program name group (1)
   "[ \t\n]+"                            ; white space
   "\\("                                 ; line number group (3)
   "[0-9]+"                              ; the line number (the fix point)
   "\\([ \t]*\n[ \t]*[0-9]+\\)*"         ; continuation lines number (4)
   "\\)"                                 ; end line number group (3)
   "[ \t\n]+"                            ; white space
   "\\("                                 ; file name group (5)
   "[^ \t\n]+"                           ; file names can contain any non-white
   "\\([ \t]*\n[ \t]*[^ \t\n]+\\)*"      ; continuation lines file name (6)
   "\\)"                                 ; end line number group (5)
   )
  "*A regular expression to parse out the file name and line number.
The 1st group should match the subroutine name.
The 3rd group is the line number.
The 5th group is the file name.
All parts may contain linebreaks surrounded by spaces.  This is important
in IDL5 which inserts random linebreaks in long module and file names.")

(defvar idlwave-shell-electric-debug-mode) ; defined by easy-mmode

(defun idlwave-shell-scan-for-state ()
  "Scan for state info.
Looks for messages in output from last IDL command indicating where
IDL has stopped.  The types of messages we are interested in are
execution halted, stepped, breakpoint, interrupted at and trace
messages.  For breakpoint messages process any attached count or
command parameters.  Update the stop line if a message is found.
The variable `idlwave-shell-current-state' is set to 'error, 'halt,
or 'breakpoint, which describes the status, or nil for none of
the above."
  (let (trace)
    (cond
     ;; Make sure we have output
     ((not idlwave-shell-command-output))

     ;; First Priority: Syntax and other errors
     ((or
       (string-match idlwave-shell-syntax-error
		     idlwave-shell-command-output)
       (string-match idlwave-shell-other-error
		     idlwave-shell-command-output))
      (with-current-buffer
	  (get-buffer-create idlwave-shell-error-buffer)
	(erase-buffer)
	(insert idlwave-shell-command-output)
	(goto-char (point-min))
	(setq idlwave-shell-error-last (point)))
      (setq idlwave-shell-current-state 'error)
      (idlwave-shell-goto-next-error))

     ;; Second Priority: Halting errors
     ((string-match idlwave-shell-halting-error
		    idlwave-shell-command-output)
      ;; Grab the file and line state info.
      (setq idlwave-shell-calling-stack-index 0)
      (setq idlwave-shell-halt-frame
	    (idlwave-shell-parse-line
	     (substring idlwave-shell-command-output
			(match-beginning 2)))
	    idlwave-shell-current-state 'error)
      (idlwave-shell-display-line (idlwave-shell-pc-frame)))

     ;; Third Priority: Various types of innocuous HALT and
     ;; TRACEBACK messages.
     ((or (setq trace (string-match idlwave-shell-trace-message-re
				    idlwave-shell-command-output))
	  (string-match idlwave-shell-halt-messages-re
			idlwave-shell-command-output))
      ;; Grab the file and line state info.
      (setq idlwave-shell-calling-stack-index 0)
      (setq idlwave-shell-halt-frame
	    (idlwave-shell-parse-line
	     (substring idlwave-shell-command-output (match-end 0))))
      (setq idlwave-shell-current-state 'halt)
      ;; Don't debug trace messages
      (idlwave-shell-display-line
       (idlwave-shell-pc-frame) nil
       (if trace 'disable
	 (if idlwave-shell-electric-debug-mode 'force))))

     ;; Fourth Priority: Breakpoints
     ((string-match idlwave-shell-break-message
		    idlwave-shell-command-output)
      (setq idlwave-shell-calling-stack-index 0)
      (setq idlwave-shell-halt-frame
	    (idlwave-shell-parse-line
	     (substring idlwave-shell-command-output (match-end 0))))
      ;; We used to count hits on breakpoints
      ;; this is no longer supported since IDL breakpoints
      ;; have learned counting.
      ;; Do breakpoint command processing
      (let ((bp (assoc
		 (list
		  (nth 0 idlwave-shell-halt-frame)
		  (nth 1 idlwave-shell-halt-frame))
		 idlwave-shell-bp-alist)))
	;(message "Scanning with %s" bp)
	(if bp
	    (let ((cmd (idlwave-shell-bp-get bp 'cmd)))
	      (if cmd ;; Execute any breakpoint command
		  (if (listp cmd) (eval cmd) (funcall cmd))))
	  ;; A breakpoint that we did not know about - perhaps it was
	  ;; set by the user...  Let's update our list.
	  (idlwave-shell-bp-query)))
      (setq idlwave-shell-current-state 'breakpoint)
      (idlwave-shell-display-line (idlwave-shell-pc-frame)))

     ;; Last Priority: Can't Step errors
     ((string-match idlwave-shell-cant-continue-error
		    idlwave-shell-command-output)
      (setq idlwave-shell-current-state 'breakpoint))

     ;; Otherwise, no particular state
     (t (setq idlwave-shell-current-state nil)))))


(defun idlwave-shell-parse-line (string &optional skip-main)
  "Parse IDL message for the subroutine, file name and line number."
;We need to work hard here to remove the stupid line breaks inserted by
;IDL5.  These line breaks can be right in the middle of procedure
;or file names.
;It is very difficult to come up with a robust solution.  This one seems
;to be pretty good though.
;
;Here is in what ways it improves over the previous solution:
;
;1. The procedure name can be split and will be restored.
;2. The number can be split.  I have never seen this, but who knows.
;3. We do not require the `.pro' extension for files.
;
;This function can still break when the file name ends on an end line
;and the message line contains an additional line with garbage.  Then
;the first part of that garbage will be added to the file name.
;However, the function checks the existence of the files with and
;without this last part - thus the function only breaks if file name
;plus garbage match an existing regular file.  This is hopefully very
;unlikely.
;
;If optional arg SKIP-MAIN is non-nil, don't parse $MAIN$ routine stop
;statements.

  (let (number procedure file)
    (when (and (not (if skip-main (string-match ":\\s-*\\$MAIN" string)))
	       (string-match idlwave-shell-file-line-message string))
      (setq procedure (match-string 1 string)
	    number (match-string 3 string)
	    file (match-string 5 string))

      ;; Repair the strings
      (setq procedure (idlwave-shell-repair-string procedure))
      (setq number (idlwave-shell-repair-string number))
      (setq file (idlwave-shell-repair-file-name file))

      ;; If we have a file, return the frame list
      (if file
	  (list (idlwave-shell-file-name file)
		(string-to-number number)
		procedure)
	;; No success finding a file
	nil))))

(defun idlwave-shell-repair-string (string)
  "Repair a string by taking out all linebreaks.  This is destructive!"
  (while (string-match "[ \t]*\n[ \t]*" string)
    (setq string (replace-match "" t t string)))
  string)

(defun idlwave-shell-repair-file-name (file)
  "Repair a file name string by taking out all linebreaks.
The last line of STRING may be garbage - we check which one makes a valid
file name."
  (let ((file1 "") (file2 "") (start 0))
    ;; We scan no further than to the next "^%" line
    (if (string-match "^%" file)
	(setq file (substring file 0 (match-beginning 0))))
    ;; Take out the line breaks
    (while (string-match "[ \t]*\n[ \t]*" file start)
      (setq file1 (concat file1 (substring file start (match-beginning 0)))
	    start (match-end 0)))
    (setq file2 (concat file1 (substring file start)))
    (cond
     ((file-regular-p file2) file2)
     ((file-regular-p file1) file1)
     ;; If we cannot verify the existence of the file, we return the shorter
     ;; name.  The idea behind this is that this may be a relative file name
     ;; and our idea about the current working directory may be wrong.
     ;; If it is a relative file name, it hopefully is short.
     ((not (string= "" file1)) file1)
     ((not (string= "" file2)) file2)
     (t nil))))

(defun idlwave-shell-cleanup ()
  "Do necessary cleanup for a terminated IDL process."
  (setq idlwave-shell-step-frame nil
        idlwave-shell-halt-frame nil
        idlwave-shell-pending-commands nil
	idlwave-shell-command-line-to-execute nil
	idlwave-shell-bp-alist nil
	idlwave-shell-calling-stack-index 0
	idlwave-idlwave_routine_info-compiled nil)
  (idlwave-shell-delete-temp-files)
  (idlwave-shell-display-line nil)
  (idlwave-shell-update-bp-overlays) ; kill old overlays
  (idlwave-shell-kill-buffer idlwave-shell-hidden-output-buffer)
  (idlwave-shell-kill-buffer idlwave-shell-bp-buffer)
  (idlwave-shell-kill-buffer idlwave-shell-error-buffer)
  ;; (idlwave-shell-kill-buffer (idlwave-shell-buffer))
  (and (get-buffer (idlwave-shell-buffer))
       (bury-buffer (get-buffer (idlwave-shell-buffer))))
  (run-hooks 'idlwave-shell-cleanup-hook))

(defun idlwave-shell-kill-buffer (buf)
  "Kill buffer BUF if it exists."
  (if (setq buf (get-buffer buf))
      (kill-buffer buf)))

(defun idlwave-shell-kill-shell-buffer-confirm ()
  (when (idlwave-shell-is-running)
    (ding)
    (unless (y-or-n-p "IDL shell is running.  Are you sure you want to kill the buffer? ")
      (error "Abort"))
    (message "Killing buffer *idl* and the associated process")))

(defun idlwave-shell-window (n)
  "Issue a `window,N' command to IDL, with special window size.
The size is given by `idlwave-shell-graphics-window-size'."
  (interactive "P")
  (let ((n (if n (prefix-numeric-value n) 0)))
    (idlwave-shell-send-command
     (apply 'format "window,%d,xs=%d,ys=%d"
	    n idlwave-shell-graphics-window-size)
     nil (idlwave-shell-hide-p 'misc) nil t)))

(defun idlwave-shell-resync-dirs ()
  "Resync the buffer's idea of the current directory.
This command queries IDL with the command bound to
`idlwave-shell-dirstack-query', reads the output for the new
directory."
  (interactive)
  (idlwave-shell-send-command idlwave-shell-dirstack-query
			      'idlwave-shell-filter-directory
			      'hide 'wait))

(defun idlwave-shell-retall (&optional arg)
  "Return from the entire calling stack.
Also get rid of widget events in the queue."
  (interactive "P")
  (save-selected-window
    ;;if (widget_info(/MANAGED))[0] gt 0 then for i=0,n_elements(widget_info(/MANAGED))-1 do widget_control,(widget_info(/MANAGED))[i],/clear_events &
    (idlwave-shell-send-command "retall" nil
				(if (idlwave-shell-hide-p 'misc) 'mostly)
				nil t)
    (idlwave-shell-display-line nil)))

(defun idlwave-shell-closeall (&optional arg)
  "Close all open files."
  (interactive "P")
  (idlwave-shell-send-command "close,/all" nil
			      (idlwave-shell-hide-p 'misc) nil t))

(defun idlwave-shell-quit (&optional arg)
  "Exit the IDL process after confirmation.
With prefix ARG, exit without confirmation."
  (interactive "P")
  (if (not (idlwave-shell-is-running))
      (error "Shell is not running")
    (if (or arg (y-or-n-p "Exit the IDLWAVE Shell? "))
	(condition-case nil
	    (idlwave-shell-send-command "exit")
	  (error nil)))))

(defun idlwave-shell-reset (&optional hidden)
  "Reset IDL.  Return to main level and destroy the leftover variables.
This issues the following commands:
RETALL
WIDGET_CONTROL,/RESET
CLOSE, /ALL
HEAP_GC, /VERBOSE"
  ;; OBJ_DESTROY, OBJ_VALID()  FIXME: should this be added?
  (interactive "P")
  (when (or idlwave-shell-reset-no-prompt
	    (yes-or-no-p "Really Reset IDL and discard current session? "))
    (message "Resetting IDL")
    (setq idlwave-shell-calling-stack-index 0)
    ;; Give widget exit handlers a chance
    (idlwave-shell-send-command "retall" nil hidden)
    (idlwave-shell-send-command "widget_control,/reset" nil hidden)
    (idlwave-shell-send-command "close,/all" nil hidden)
    ;; (idlwave-shell-send-command "obj_destroy, obj_valid()" nil hidden)
    (idlwave-shell-send-command "heap_gc,/verbose" nil hidden)
    (idlwave-shell-display-line nil)))

(defun idlwave-shell-path-filter ()
  ;; Convert the output of the path query into a list of directories
  (let ((path-string idlwave-shell-command-output)
	(case-fold-search t)
	(start 0)
	dirs sysdir)
    (while (string-match "^PATH:[ \t]*<\\(.*\\)>[ \t]*\n" path-string start)
      (push (match-string 1 path-string) dirs)
      (setq start (match-end 0)))
    (setq dirs (mapcar 'file-name-as-directory dirs))
    (if (string-match "^SYSDIR:[ \t]*<\\(.*\\)>[ \t]*\n" path-string)
	(setq sysdir (file-name-as-directory
		      (match-string 1 path-string))))
    (cons sysdir (nreverse dirs))))

(defun idlwave-shell-routine-info-filter ()
  "Function which parses the special output from idlwave_routine_info.pro."
  (let ((text idlwave-shell-command-output)
	(start 0)
	sep sep-re file type spec specs name cs key keys class entry)
    ;;    (message "GOT: %s" text) ;??????????????????????
    ;; Initialize variables
    (setq idlwave-compiled-routines nil
	  idlwave-unresolved-routines nil)
    ;; Cut out the correct part of the output.
    (if (string-match
	 "^>>>BEGIN OF IDLWAVE ROUTINE INFO (\"\\(.+\\)\" IS THE SEPARATOR.*"
	 text)
	(setq sep (match-string 1 text)
	      sep-re (concat (regexp-quote sep) " *")
	      text (substring text (match-end 0)))
      ;; Set dummy values and kill the text
      (setq sep "@" sep-re "@ *" text "")
      (if idlwave-idlwave_routine_info-compiled
	  (message
	   "Routine Info warning: No match for BEGIN line in \n>>>\n%s\n<<<\n"
	   idlwave-shell-command-output)))
    (if (string-match "^>>>END OF IDLWAVE ROUTINE INFO.*" text)
	(setq text (substring text 0 (match-beginning 0)))
      (if idlwave-idlwave_routine_info-compiled
	  (message
	   "Routine Info warning: No match for END line in \n>>>\n%s\n<<<\n"
	   idlwave-shell-command-output)))
    ;; Match the output lines
    (while (string-match "^IDLWAVE-\\(PRO\\|FUN\\): \\(.*\\)" text start)
      (setq start (match-end 0))
      (setq type (match-string 1 text)
	    spec (match-string 2 text)
	    specs (idlwave-split-string spec sep-re)
	    name (nth 0 specs)
	    class (if (equal (nth 1 specs) "") nil (nth 1 specs))
	    file (nth 2 specs)
	    cs (nth 3 specs)
	    key (nth 4 specs)
	    keys (if (and (stringp key)
			  (not (string-match "\\` *\\'" key)))
		     (mapcar 'list
			     (delete "" (idlwave-split-string key " +")))))
      (setq name (idlwave-sintern-routine-or-method name class t)
	    class (idlwave-sintern-class class t)
	    file (if (equal file "") nil file)
	    keys (mapcar (lambda (x)
			   (list (idlwave-sintern-keyword (car x) t))) keys))

      ;; In the following ignore routines already defined in buffers,
      ;; assuming that if the buffer stuff differs, it is a "new"
      ;; version, not yet compiled, and should take precedence.
      ;; We could do the same for the library to avoid duplicates -
      ;; but I think frequently a user might have several versions of
      ;; the same function in different programs, and in this case the
      ;; compiled one will be the best guess of all versions.
      ;; Therefore, we leave duplicates of library routines in.
      (cond ((string= name "$MAIN$"))    ; ignore this one
	    ((and (string= type "PRO")
		  ;; FIXME: is it OK to make the buffer routines dominate?
		  (or t (null file)
		      (not (idlwave-rinfo-assq name 'pro class
					       idlwave-buffer-routines)))
		  ;; FIXME: is it OK to make the library routines dominate?
		  ;;(not (idlwave-rinfo-assq name 'pro class
		  ;;			   idlwave-library-routines))
		  )
	     (setq entry (list name 'pro class
			       (cons 'compiled
				     (if file
					 (list
					  (file-name-nondirectory file)
					  (idlwave-sintern-dir
					   (file-name-directory file)))))
			       cs (cons nil keys)))
	     (if file
		 (push entry idlwave-compiled-routines)
	       (push entry idlwave-unresolved-routines)))

	    ((and (string= type "FUN")
		  ;; FIXME: is it OK to make the buffer routines dominate?
		  (or t (not file)
		      (not (idlwave-rinfo-assq name 'fun class
					       idlwave-buffer-routines)))
		  ;; FIXME: is it OK to make the library routines dominate?
		  ;; (not (idlwave-rinfo-assq name 'fun class
		  ;;			   idlwave-library-routines))
		  )
	     (setq entry (list name 'fun class
			       (cons 'compiled
				     (if file
					 (list
					  (file-name-nondirectory file)
					  (idlwave-sintern-dir
					   (file-name-directory file)))))
			       cs (cons nil keys)))
	     (if file
		 (push entry idlwave-compiled-routines)
	       (push entry idlwave-unresolved-routines))))))
  ;; Reverse the definitions so that they are alphabetically sorted.
  (setq idlwave-compiled-routines (nreverse idlwave-compiled-routines)
	idlwave-unresolved-routines (nreverse idlwave-unresolved-routines)))

(defun idlwave-shell-filter-directory ()
  "Get the current directory from `idlwave-shell-command-output'.
Change the default directory for the process buffer to concur."
  (with-current-buffer (idlwave-shell-buffer)
    (if (string-match ",___cur[\n\r ]+\\([^\n\r]+\\)[\n\r]"
		      idlwave-shell-command-output)
	(let ((dir (substring idlwave-shell-command-output
			      (match-beginning 1) (match-end 1))))
          ;; (message "Setting Emacs working dir to %s" dir)
	  (setq idlwave-shell-default-directory dir)
	  (setq default-directory (file-name-as-directory dir))))))

(defvar idlwave-shell-get-object-class nil)
(defun idlwave-shell-get-object-class (apos)
  "Query the shell for the class of the object before point."
  (let ((bos (save-excursion (idlwave-start-of-substatement 'pre) (point)))
	(bol (save-excursion (forward-line 0) (point)))
	expression)
    (save-excursion
      (goto-char apos)
      (setq expression (buffer-substring
			(catch 'exit
			  (while t
			    (if (not (re-search-backward
				      "[^][.A-Za-z0-9_() ]" bos t))
				(throw 'exit bos)) ;ran into bos
			    (if (not (idlwave-is-pointer-dereference bol))
				(throw 'exit (1+ (point))))))
			apos)))
    (when (not (string= expression ""))
      (setq idlwave-shell-get-object-class nil)
      (idlwave-shell-send-command
       (concat "if obj_valid(" expression ") then print,obj_class("
	       expression ")")
       'idlwave-shell-parse-object-class
       'hide 'wait)
      ;; If we don't know anything about the class, update shell routines
      (if (and idlwave-shell-get-object-class
	       (not (assoc-string idlwave-shell-get-object-class
				  (idlwave-class-alist) t)))
	  (idlwave-shell-maybe-update-routine-info))
      idlwave-shell-get-object-class)))

(defun idlwave-shell-parse-object-class ()
  "Parse the output of the obj_class command."
  (let ((match "obj_class([^\n\r]+[\n\r ]"))
    (if (string-match (concat match "\\([A-Za-z_0-9]+\\) *[\n\r]\\("
			      idlwave-shell-prompt-pattern "\\)")
		      idlwave-shell-command-output)
	(setq idlwave-shell-get-object-class
	      (match-string 1 idlwave-shell-command-output)))))

(defvar idlwave-sint-sysvars nil)
(idlwave-new-sintern-type 'execcomm)

(defun idlwave-shell-complete (&optional arg)
  "Do completion in the idlwave-shell buffer.
Calls `idlwave-shell-complete-filename' after some executive commands or
in strings.  Otherwise, calls `idlwave-complete' to complete modules and
keywords."
  (interactive "P")
  (let (exec-cmd)
    (cond
     ((and
       (setq exec-cmd (idlwave-shell-executive-command))
       (cdr exec-cmd)
       (member (upcase (cdr exec-cmd))
	       '(".R" ".RU" ".RUN" ".RN" ".RNE" ".RNEW"
		 ".COM" ".COMP" ".COMPI" ".COMPIL" ".COMPILE")))
      ;; We are in a command line with an executive command
      (idlwave-shell-complete-filename))

     ((car-safe exec-cmd)
      (setq idlwave-completion-help-info
	    '(idlwave-shell-complete-execcomm-help))
      (idlwave-complete-in-buffer 'execcomm 'execcomm
				  idlwave-executive-commands-alist nil
				  "Select an executive command"
				  "system variable"))

     ((idlwave-shell-batch-command)
      (idlwave-shell-complete-filename))

     ((idlwave-shell-shell-command)
      (idlwave-shell-complete-filename))

     ((and (idlwave-shell-filename-string)
	   (save-excursion
	     (beginning-of-line)
	     (let ((case-fold-search t))
	       (not (looking-at ".*obj_new")))))
      (idlwave-shell-complete-filename))

     (t
      ;; Default completion of modules and keywords
      (idlwave-complete arg)))))

;; Get rid of opaque dynamic variable passing of idlw-help-link?
(defvar idlw-help-link) ; dynamic variable from idlwave-do-mouse-completion-help
(defun idlwave-shell-complete-execcomm-help (mode word)
  (let ((word (or (nth 1 idlwave-completion-help-info) word))
	(entry (assoc-string word idlwave-executive-commands-alist t)))
    (cond
     ((eq mode 'test)
      (and (stringp word) entry (cdr entry)))
     ((eq mode 'set)
      (if entry (setq idlw-help-link (cdr entry)))) ; setting dynamic variable!
     (t (error "This should not happen")))))

(defun idlwave-shell-complete-filename (&optional arg)
  "Complete a file name at point if after a file name.
We assume that we are after a file name when completing one of the
args of an executive .run, .rnew or .compile."
  ;; CWD might have changed, resync, to set default directory
  (idlwave-shell-resync-dirs)
  (let ((comint-file-name-chars idlwave-shell-file-name-chars))
    (comint-filename-completion)))

(defun idlwave-shell-executive-command ()
  "Return the name of the current executive command, if any."
  (save-excursion
    (idlwave-beginning-of-statement)
    (cons (looking-at "[ \t]*\\.")
	  (if (looking-at "[ \t]*[.]\\([^ \t\n\r]+\\)[ \t]")
	      (match-string 1)))))

(defun idlwave-shell-filename-string ()
  "Return t if in a string and after what could be a file name."
  (let ((limit (point-at-bol)))
    (save-excursion
      ;; Skip backwards over file name chars
      (skip-chars-backward idlwave-shell-file-name-chars limit)
      ;; Check of the next char is a string delimiter
      (memq (preceding-char) '(?\' ?\")))))

(defun idlwave-shell-batch-command ()
  "Return t if we're in a batch command statement like @foo"
  (let ((limit (point-at-bol)))
    (save-excursion
      ;; Skip backwards over filename
      (skip-chars-backward idlwave-shell-file-name-chars limit)
      (skip-chars-backward " \t" limit)
      (and (eq (preceding-char) ?@) (not (idlwave-in-quote))))))

(defun idlwave-shell-shell-command ()
  "Return t if we're in a shell command statement like $ls"
  (save-excursion
    (idlwave-beginning-of-statement)
    (looking-at "\\$")))

;; Debugging Commands ------------------------------------------------------

(defun idlwave-shell-redisplay (&optional hide)
  "Try to resync the display with where execution has stopped.
Issues a \"help,/trace\" command followed by a call to
`idlwave-shell-display-line'.  Also updates the breakpoint
overlays."
  (interactive)
  (setq idlwave-shell-calling-stack-index 0)
  (idlwave-shell-send-command
   "help,/trace"
   '(idlwave-shell-display-line
     (idlwave-shell-pc-frame))
   hide)
  (idlwave-shell-bp-query))

(defun idlwave-shell-display-level-in-calling-stack (&optional hide)
  (idlwave-shell-send-command
   "help,/trace"
   `(progn
      ;; scanning for the state will reset the stack level - restore it
      (setq idlwave-shell-calling-stack-index
	    ,idlwave-shell-calling-stack-index)
      ;; parse the stack and visit the selected frame
      (idlwave-shell-parse-stack-and-display))
   hide))

(defun idlwave-shell-parse-stack-and-display ()
  (let* ((lines (delete "" (idlwave-split-string
			    idlwave-shell-command-output "^%")))
	 (stack (delq nil (mapcar 'idlwave-shell-parse-line lines)))
	 (nmax (1- (length stack)))
	 (nmin 0) message)
    (cond
     ((< nmax nmin)
      (setq idlwave-shell-calling-stack-index 0)
      (ding)
      (message "Problem with calling stack"))
     ((> idlwave-shell-calling-stack-index nmax)
      (ding)
      (setq idlwave-shell-calling-stack-index nmax
	    message (format "%d is the highest calling stack level - can't go further up"
			    (- nmax))))
     ((< idlwave-shell-calling-stack-index nmin)
      (ding)
      (setq idlwave-shell-calling-stack-index nmin
	    message (format "%d is the current calling stack level - can't go further down"
			    (- nmin)))))
    (setq idlwave-shell-calling-stack-routine
	  (nth 2 (nth idlwave-shell-calling-stack-index stack)))

    ;; force edebug for this frame if we're in that mode already
    (idlwave-shell-display-line
     (nth idlwave-shell-calling-stack-index stack) nil
     (if idlwave-shell-electric-debug-mode 'force))
    (message "%s" (or message
		      (format "In routine %s (stack level %d)"
			      idlwave-shell-calling-stack-routine
			      (- idlwave-shell-calling-stack-index))))))

(defun idlwave-shell-stack-up ()
  "Display the source code one step up the calling stack."
  (interactive)
  (incf idlwave-shell-calling-stack-index)
  (idlwave-shell-display-level-in-calling-stack 'hide))
(defun idlwave-shell-stack-down ()
  "Display the source code one step down the calling stack."
  (interactive)
  (decf idlwave-shell-calling-stack-index)
  (idlwave-shell-display-level-in-calling-stack 'hide))

(defun idlwave-shell-goto-frame (&optional frame)
  "Set buffer to FRAME with point at the frame line.
If the optional argument FRAME is nil then `idlwave-shell-pc-frame'
is used.  Does nothing if the resulting frame is nil."
  (if frame ()
    (setq frame (idlwave-shell-pc-frame)))
  (cond
   (frame
    (set-buffer (idlwave-find-file-noselect (car frame) 'shell))
    (widen)
    (goto-char (point-min))
    (forward-line (1- (nth 1 frame))))))

(defun idlwave-shell-pc-frame ()
  "Return the frame for IDL execution."
  (and idlwave-shell-halt-frame
       (list (nth 0 idlwave-shell-halt-frame)
	     (nth 1 idlwave-shell-halt-frame)
	     (nth 2 idlwave-shell-halt-frame))))

(defun idlwave-shell-valid-frame (frame)
  "Check that frame is for an existing file."
  (file-readable-p (car frame)))

(defun idlwave-shell-stop-line-pending ()
  ;; Temporarily change the color of the stop line overlay
  (if idlwave-shell-stop-line-overlay
      (overlay-put idlwave-shell-stop-line-overlay 'face
		   (if idlwave-shell-electric-debug-mode
		       'idlwave-shell-pending-electric-stop
		     'idlwave-shell-pending-stop))))

(defvar idlwave-shell-suppress-electric-debug nil)
(defun idlwave-shell-display-line (frame &optional col debug)
  "Display frame file in other window with overlay arrow.

FRAME is a list of file name, line number, and subroutine name.  If
FRAME is nil then remove overlay.  If COL is set, move point to that
column in the line.  If DEBUG is non-nil, enable the electric debug
mode.  If it is 'disable, do not enable no matter what the setting of
`idlwave-shell-automatic-electric-debug'.  If it is 'force, enable no
matter what the settings of that variable."
  (if (not frame)
      ;; remove stop-line overlay from old position
      (progn
        (setq overlay-arrow-string nil)
	(setq idlwave-shell-mode-line-info nil)
	(setq idlwave-shell-is-stopped nil)
        (if idlwave-shell-stop-line-overlay
            (delete-overlay idlwave-shell-stop-line-overlay))
	;; turn off electric debug everywhere, if it's on
	(idlwave-shell-electric-debug-all-off))
    (if (not (idlwave-shell-valid-frame frame))
	;; fixme: errors are dangerous in shell filters.  but i think i
	;; have never encountered this one.
        (error "invalid frame - unable to access file: %s" (car frame))
      ;;
      ;; buffer : the buffer to display a line in.
      ;; select-shell: current buffer is the shell.
      ;;
      (setq idlwave-shell-mode-line-info
	    (if (nth 2 frame)
		(format "[%d:%s]"
			(- idlwave-shell-calling-stack-index)
			(nth 2 frame))))
      (let* ((buffer (idlwave-find-file-noselect (car frame) 'shell))
             (select-shell (equal (buffer-name) (idlwave-shell-buffer)))
             window pos electric)

	;; first make sure the shell window is visible
	(idlwave-display-buffer (idlwave-shell-buffer)
				nil (idlwave-shell-shell-frame))

	;; now display the buffer and remember which window it is.
	(setq window (idlwave-display-buffer buffer
					     nil (idlwave-shell-source-frame)))

	;; enter the buffer and mark the line
        (with-current-buffer buffer
          (save-restriction
            (widen)
            (goto-char (point-min))
            (forward-line (1- (nth 1 frame)))
            (setq pos (point))
	    (setq idlwave-shell-is-stopped t)

            (if idlwave-shell-stop-line-overlay
                (progn
		  ;; restore face and move overlay
		  (overlay-put idlwave-shell-stop-line-overlay 'face
			       (if idlwave-shell-electric-debug-mode
                                   idlwave-shell-electric-stop-line-face
                                 idlwave-shell-stop-line-face))
		  (move-overlay idlwave-shell-stop-line-overlay
				(point) (point-at-eol)
				(current-buffer)))
	      ;; use the arrow instead, but only if marking is wanted.
	      (if idlwave-shell-mark-stop-line
		  (setq overlay-arrow-string idlwave-shell-overlay-arrow))
              (or overlay-arrow-position  ; create the marker if necessary
                  (setq overlay-arrow-position (make-marker)))
	      (set-marker overlay-arrow-position (point) buffer)))

	  ;; if the point is outside the restriction, widen the buffer.
          (if (or (< pos (point-min)) (> pos (point-max)))
	      (progn
		(widen)
		(goto-char pos)))

	  ;; if we have the column of the error, move the cursor there.
          (if col (move-to-column col))
          (setq pos (point))

	  ;; enter electric debug mode, if not prohibited and not in
	  ;; it already
	  (when  (and (not idlwave-shell-electric-debug-mode)
		      (or (eq debug 'force)
			  (and
			   (not (eq debug 'disable)) ;; explicitly disabled
			   (or
			    (eq idlwave-shell-automatic-electric-debug t)
			    (and
			     (eq idlwave-shell-automatic-electric-debug
				 'breakpoint)
			     (not (eq idlwave-shell-current-state 'error))))
			   (not idlwave-shell-suppress-electric-debug))))
	    (idlwave-shell-electric-debug-mode t))
	  (setq electric idlwave-shell-electric-debug-mode))

	;; Make sure pos is really displayed in the window.
	(set-window-point window pos)

	;; If we came from the shell, go back there.  Otherwise select
	;; the window where the error/halt is displayed.
        (if (or (and idlwave-shell-electric-zap-to-file electric)
		(and (equal (buffer-name) (idlwave-shell-buffer))
		     (not select-shell)))
            (select-window window))))))


(defun idlwave-shell-step (arg)
  "Step one source line.
If given prefix argument ARG, step ARG source lines."
  (interactive "p")
  (or (not arg) (< arg 1)
      (setq arg 1))
  (idlwave-shell-stop-line-pending)
  (idlwave-shell-send-command
   (concat ".s " (if (integerp arg) (int-to-string arg) arg))
   nil (if (idlwave-shell-hide-p 'debug) 'mostly) nil t))

(defun idlwave-shell-stepover (arg)
  "Stepover one source line.
If given prefix argument ARG, step ARG source lines.
Uses IDL's stepover executive command which does not enter called functions."
  (interactive "p")
  (or (not arg) (< arg 1)
      (setq arg 1))
  (idlwave-shell-stop-line-pending)
  (idlwave-shell-send-command
   (concat ".so " (if (integerp arg) (int-to-string arg) arg))
   nil (if (idlwave-shell-hide-p 'debug) 'mostly) nil t))

(defun idlwave-shell-break-here (&optional count cmd condition disabled
					   no-show)
  "Set breakpoint at current line.

If COUNT is nil then an ordinary breakpoint is set.  We treat a COUNT
of 1 as a temporary breakpoint using the ONCE keyword.  Counts greater
than 1 use the IDL AFTER=count keyword to break only after reaching
the statement COUNT times.

Optional argument CMD is a list or function to evaluate upon reaching
the breakpoint.  CONDITION is a break condition, and DISABLED, if
non-nil disables the breakpoint."
  (interactive "P")
  (when (listp count)
    (if (equal (car count) 4)
	(setq condition (read-string "Break Condition: ")))
    (setq count nil))
  (idlwave-shell-set-bp
   ;; Create breakpoint
   (idlwave-shell-bp (idlwave-shell-current-frame)
		     (list count cmd condition disabled)
		     (idlwave-shell-current-module))
   no-show))

(defun idlwave-shell-set-bp-check (bp)
  "Check for failure to set breakpoint.
This is run on `idlwave-shell-post-command-hook'.
Offers to recompile the procedure if we failed.  This usually fixes
the problem with not being able to set the breakpoint."
  ;; Scan for message
  (if idlwave-shell-command-output
      (cond
       ((string-match "% BREAKPOINT: *Unable to find code"
		      idlwave-shell-command-output)
	;; Offer to recompile
        (if (progn
              (beep)
              (y-or-n-p
               (concat "Okay to recompile file "
                       (idlwave-shell-bp-get bp 'file) "?")))
            ;; Recompile
            (progn
              ;; Clean up before retrying
              (idlwave-shell-command-failure)
              (idlwave-shell-send-command
               (concat ".run \"" (idlwave-shell-bp-get bp 'file) "\"") nil
	       (if (idlwave-shell-hide-p 'run) 'mostly) nil t)
              ;; Try setting breakpoint again
              (idlwave-shell-set-bp bp))
          (beep)
          (message "Unable to set breakpoint.")
          (idlwave-shell-command-failure))
	nil)

       ((string-match "% Syntax error" idlwave-shell-command-output)
	(message "Syntax error in condition.")
	(idlwave-shell-command-failure)
	nil)

       (t 'okay))))

(defun idlwave-shell-command-failure ()
  "Do any necessary clean up when an IDL command fails.
Call this from a function attached to `idlwave-shell-post-command-hook'
that detects the failure of a command.
For example, this is called from `idlwave-shell-set-bp-check' when a
breakpoint can not be set."
  ;; Clear pending commands
  (setq idlwave-shell-pending-commands nil))

(defun idlwave-shell-cont (&optional no-show)
  "Continue executing."
  (interactive)
  (idlwave-shell-stop-line-pending)
  (idlwave-shell-send-command ".c" (unless no-show
				     '(idlwave-shell-redisplay 'hide))
			      (if (idlwave-shell-hide-p 'debug) 'mostly)
			      nil t))

(defun idlwave-shell-go ()
  "Run .GO.  This starts the main program of the last compiled file."
  (interactive)
  (idlwave-shell-stop-line-pending)
  (idlwave-shell-send-command ".go" '(idlwave-shell-redisplay 'hide)
			      (if (idlwave-shell-hide-p 'debug) 'mostly)
			      nil t))

(defun idlwave-shell-return ()
  "Run .RETURN (continue to next return, but stay in subprogram)."
  (interactive)
  (idlwave-shell-stop-line-pending)
  (idlwave-shell-send-command ".return" '(idlwave-shell-redisplay 'hide)
			      (if (idlwave-shell-hide-p 'debug) 'mostly)
			      nil t))

(defun idlwave-shell-skip ()
  "Run .SKIP (skip one line, then step)."
  (interactive)
  (idlwave-shell-stop-line-pending)
  (idlwave-shell-send-command ".skip" '(idlwave-shell-redisplay 'hide)
			      (if (idlwave-shell-hide-p 'debug) 'mostly)
			      nil t))

(defun idlwave-shell-clear-bp (bp &optional no-query)
  "Clear breakpoint BP.
Clears in IDL and in `idlwave-shell-bp-alist'."
  (let ((index (idlwave-shell-bp-get bp)))
    (if index
        (progn
          (idlwave-shell-send-command
           (concat "breakpoint,/clear," (int-to-string index))
	   nil (idlwave-shell-hide-p 'breakpoint) nil t)
	  (unless no-query (idlwave-shell-bp-query))))))

(defun idlwave-shell-current-frame ()
  "Return a list containing the current file name and line point is in.
If in the IDL shell buffer, returns `idlwave-shell-pc-frame'."
  (if (eq (current-buffer) (get-buffer (idlwave-shell-buffer)))
      ;; In IDL shell
      (idlwave-shell-pc-frame)
    ;; In source
    (list (idlwave-shell-file-name (buffer-file-name))
          (save-restriction
            (widen)
	    (1+ (count-lines 1 (point-at-bol)))))))

(defun idlwave-shell-current-module ()
  "Return the name of the module for the current file.
Returns nil if unable to obtain a module name."
  (if (eq (current-buffer) (get-buffer (idlwave-shell-buffer)))
      ;; In IDL shell
      (nth 2 idlwave-shell-halt-frame)
    ;; In pro file
    (save-restriction
      (widen)
      (save-excursion
        (if (idlwave-prev-index-position)
	    (let* ((module (idlwave-what-module))
		   (name (idlwave-make-full-name (nth 2 module) (car module)))
		   (type (nth 1 module)))
	      (list (upcase name) type)))))))

(defun idlwave-shell-clear-current-bp ()
  "Remove breakpoint at current line.
This command can be called from the shell buffer if IDL is currently
stopped at a breakpoint."
  (interactive)
  (let ((bp (idlwave-shell-find-current-bp)))
    (if bp (idlwave-shell-clear-bp bp))))


(defun idlwave-shell-toggle-enable-current-bp (&optional bp force
							 no-update)
  "Disable or enable current breakpoint or a breakpoint passed in BP.
If FORCE is 'disable or 'enable, for that condition instead of
toggling.  If NO-UPDATE is non-nil, don't update the breakpoint
list after toggling."
  (interactive)
  (let* ((bp (or bp (idlwave-shell-find-current-bp)))
	 (disabled (idlwave-shell-bp-get bp 'disabled)))
    (cond ((eq force 'disable) (setq disabled nil))
	  ((eq force 'enable) (setq disabled t)))
    (when bp
      (setf (nth 3 (cdr (cdr bp))) (not disabled))
      (idlwave-shell-send-command
       (concat "breakpoint,"
	       (if disabled "/enable," "/disable,")
	       (int-to-string (idlwave-shell-bp-get bp)))
       nil (idlwave-shell-hide-p 'breakpoint) nil t)
      (unless no-update (idlwave-shell-bp-query)))))

(defun idlwave-shell-enable-all-bp (&optional enable no-update bpl)
  "Disable all breakpoints we know about which need disabling.
If ENABLE is non-nil, enable them instead."
  (let  ((bpl (or bpl idlwave-shell-bp-alist)) disabled modified)
    (while bpl
      (setq disabled (idlwave-shell-bp-get (car bpl) 'disabled))
      (when (idlwave-xor (not disabled) (eq enable 'enable))
	(idlwave-shell-toggle-enable-current-bp
	 (car bpl) (if (eq enable 'enable) 'enable 'disable) no-update)
	(push (car bpl) modified))
      (setq bpl (cdr bpl)))
    (unless no-update (idlwave-shell-bp-query))
    modified))

(defun idlwave-shell-to-here ()
  "Set a breakpoint with count 1 then continue."
  (interactive)
  ;; temporarily disable all other breakpoints
  (let ((disabled (idlwave-shell-enable-all-bp 'disable 'no-update)))
    (idlwave-shell-break-here 1 nil nil nil 'no-show)
    (idlwave-shell-cont 'no-show)
    (idlwave-shell-enable-all-bp 'enable 'no-update disabled))
  (idlwave-shell-redisplay)) ; sync up everything at the end

(defun idlwave-shell-break-this-module (&optional arg)
  (interactive "P")
  (save-excursion
    (idlwave-beginning-of-subprogram)
    (idlwave-shell-break-here arg)))

(defun idlwave-shell-break-in ()
  "Look for a module name near point and set a break point for it.
The command looks for an identifier near point and sets a breakpoint
for the first line of the corresponding module.  If MODULE is `t', set
in the current routine."
  (interactive)
  (let* ((module (idlwave-fix-module-if-obj_new (idlwave-what-module)))
	 (type (nth 1 module))
	 (name (car module))
	 (class (nth 2 module)))
    (if module
	(progn
	  (setq module (idlwave-make-full-name class name))
	  (idlwave-shell-module-source-query module type)
	  (idlwave-shell-set-bp-in-module name type class))
      (error "No identifier at point"))))


(defun idlwave-shell-set-bp-in-module (name type class)
  "Set breakpoint in module.
Assumes that `idlwave-shell-sources-alist' contains an entry for that module."
  (let* ((module (idlwave-make-full-name class name))
	 (source-file
	  (car-safe (cdr-safe
		     (or
		      (assoc (upcase module)
			     idlwave-shell-sources-alist)
		      (nth 3 (idlwave-best-rinfo-assoc name type class
						       (idlwave-routines)))))))
	 buf)
    (if (or (not source-file)
	    (not (file-regular-p source-file))
	    (not (setq buf
		       (or (idlwave-get-buffer-visiting source-file)
			   (find-file-noselect source-file)))))
	(progn
	  (message "The source file for module %s is probably not compiled"
		   module)
	  (beep))
      (with-current-buffer buf
	(save-excursion
	  (goto-char (point-min))
	  (let ((case-fold-search t))
	    (if (re-search-forward
		 (concat "^[ \t]*\\(pro\\|function\\)[ \t]+"
			 (downcase module)
			 "[ \t\n,]") nil t)
		(progn
		  (goto-char (match-beginning 1))
		  (message "Setting breakpoint for module %s" module)
		  (idlwave-shell-break-here))
	      (message "Cannot find module %s in file %s" module source-file)
	      (beep))))))))

(defun idlwave-shell-up ()
  "Run to end of current block.
Sets a breakpoint with count 1 at end of block, then continues."
  (interactive)
  (if (idlwave-shell-pc-frame)
      (save-excursion
        (idlwave-shell-goto-frame)
        ;; find end of subprogram
        (let ((eos (save-excursion
                     (idlwave-beginning-of-subprogram)
                     (idlwave-forward-block)
                     (point))))
          (idlwave-backward-up-block -1)
          ;; move beyond end block line - IDL will not break there.
          ;; That is, you can put a breakpoint there but when IDL does
          ;; break it will report that it is at the next line.
          (idlwave-next-statement)
          (idlwave-end-of-statement)
          ;; Make sure we are not beyond subprogram
          (if (< (point) eos)
              ;; okay
              ()
            ;; Move back inside subprogram
            (goto-char eos)
            (idlwave-previous-statement))
          (idlwave-shell-to-here)))))

(defun idlwave-shell-out ()
  "Attempt to run until this procedure exits.
Runs to the last statement and then steps 1 statement.  Use the .out command."
  (interactive)
  (idlwave-shell-send-command ".o" nil
			      (if (idlwave-shell-hide-p 'debug) 'mostly)
			      nil t))

(defun idlwave-shell-goto-previous-bp ()
  "Move to the previous breakpoint in the buffer."
  (interactive)
  (idlwave-shell-move-to-bp -1))
(defun idlwave-shell-goto-next-bp ()
  "Move to the next breakpoint in the buffer."
  (interactive)
  (idlwave-shell-move-to-bp 1))

(defun idlwave-shell-move-to-bp (dir)
  "Move to the next or previous breakpoint, depending on direction DIR."
  (let* ((frame (idlwave-shell-current-frame))
	 (file (car frame))
	 (orig-bp-line (nth 1 frame))
	 (bp-alist idlwave-shell-bp-alist)
	 (orig-func (if (> dir 0) '> '<))
	 (closer-func (if (> dir 0) '< '>))
	 bp got-bp bp-line cur-line)
    (while (setq bp (pop bp-alist))
      (when (string= file (car (car bp)))
	(setq got-bp 1)
	(setq cur-line (nth 1 (car bp)))
	(if (and
	     (funcall orig-func cur-line orig-bp-line)
	     (or (not bp-line) (funcall closer-func cur-line bp-line)))
	    (setq bp-line cur-line))))
    (unless bp-line (error "No further breakpoints"))
    (goto-char (point-min))
    (forward-line (1- bp-line))))

;; Examine Commands ------------------------------------------------------

(defun idlwave-shell-help-expression (arg)
  "Print help on current expression.  See `idlwave-shell-print'."
  (interactive "P")
  (idlwave-shell-print arg 'help))

(defmacro idlwave-shell-mouse-examine (help &optional ev)
  "Create a function for generic examination of expressions."
  `(lambda (event)
     "Expansion function for expression examination."
     (interactive "e")
     (let* ((drag-track (fboundp 'mouse-drag-track))
	    (transient-mark-mode t)
	    (zmacs-regions t)
	    (tracker (if (featurep 'xemacs)
			 (if (fboundp
			      'default-mouse-track-event-is-with-button)
			     'idlwave-xemacs-hack-mouse-track
			   'mouse-track)
		       ;; Emacs 22 no longer completes the drag with
		       ;; mouse-drag-region, without an additional
		       ;; event.  mouse-drag-track does so.
		       (if drag-track 'mouse-drag-track 'mouse-drag-region))))
       (funcall tracker event)
       (idlwave-shell-print (if (idlwave-region-active-p) '(4) nil)
			    ,help ,ev))))

;; Begin terrible hack section -- XEmacs tests for button2 explicitly
;; on drag events, calling drag-n-drop code if detected.  Ughhh...
(defun idlwave-default-mouse-track-event-is-with-button (event n)
  t)

(defun idlwave-xemacs-hack-mouse-track (event)
  (if (featurep 'xemacs)
      (let ((oldfunc (symbol-function
		      'default-mouse-track-event-is-with-button)))
	(unwind-protect
	    (progn
	      (fset 'default-mouse-track-event-is-with-button
		    'idlwave-default-mouse-track-event-is-with-button)
	      (mouse-track event))
	  (fset 'default-mouse-track-event-is-with-button oldfunc)))))
;;; End terrible hack section

(defun idlwave-shell-mouse-print (event)
  "Print value of variable at the mouse position, with `print'."
  (interactive "e")
  (funcall (idlwave-shell-mouse-examine nil) event))

(defun idlwave-shell-mouse-help (event)
  "Print value of variable at the mouse position, with `help'."
  (interactive "e")
  (funcall (idlwave-shell-mouse-examine 'help) event))

(defun idlwave-shell-examine-select (event)
  "Pop-up a list to select from for examining the expression."
  (interactive "e")
  (funcall (idlwave-shell-mouse-examine nil event) event))

(defmacro idlwave-shell-examine (help)
  "Create a function for key-driven expression examination."
  `(lambda ()
     (interactive)
     (idlwave-shell-print nil ,help)))

(defvar idlwave-shell-examine-label nil
  "Label to include with examine text if in a separate buffer.")
(defvar idlwave-shell-examine-completion-list nil)

(defun idlwave-shell-print (arg &optional help ev complete-help-type)
  "Print current expression.

With HELP non-nil, show help on expression.  If HELP is a string,
the expression will be put in place of ___, e.g.:

   print,size(___,/DIMENSIONS)

HELP can also be a cons cell ( NAME . STRING ) in which case NAME will
be used to label the help print-out.

Otherwise, print is called on the expression.

An expression is an identifier plus 1 pair of matched parentheses
directly following the identifier - an array or function call.
Alternatively, an expression is the contents of any matched
parentheses when the open parenthesis is not directly preceded by an
identifier.  If point is at the beginning or within an expression
return the inner-most containing expression, otherwise, return the
preceding expression.

With prefix arg, or if transient mode set and the region is defined,
use the current region as the expression.

With double prefix arg ARG prompt for an expression.

If EV is a valid event passed, pop-up a list from
`idlwave-shell-examine-alist' from which to select the help
command text.  If instead COMPLETE-HELP-TYPE is non-nil, choose
from `idlwave-shell-examine-alist' via mini-buffer shortcut key."
  (interactive "P")

  ;; For speed: assume the helper routine hasn't been lost, e.g. with
  ;; .FULL_RESET_SESSION.  We'll recover if necessary
  (unless idlwave-idlwave_routine_info-compiled
    (idlwave-shell-compile-helper-routines))
  (save-excursion
    (let* ((process (get-buffer-process (current-buffer)))
	   (process-mark (if process (process-mark process)))
	   (stack-label
	    (if (and (integerp idlwave-shell-calling-stack-index)
		     (> idlwave-shell-calling-stack-index 0))
		(format "  [-%d:%s]"
			idlwave-shell-calling-stack-index
			idlwave-shell-calling-stack-routine)))
	   expr beg end cmd)
      (cond
       ((equal arg '(16))
	(setq expr (read-string "Expression: ")))
       ((and (or arg (idlwave-region-active-p))
	     (< (- (region-end) (region-beginning)) 2000))
	(setq beg (region-beginning)
	      end (region-end)))
       (t
	(idlwave-with-special-syntax
	 ;; Move to beginning of current or previous expression
	 (if (looking-at "\\<\\|(")
	     ;; At beginning of expression, don't move backwards unless
	     ;; this is at the end of an identifier.
	     (if (looking-at "\\>")
		 (backward-sexp))
	   (backward-sexp))
	 (if (looking-at "\\>")
	     ;; Move to beginning of identifier - must be an array or
	     ;; function expression.
	     (backward-sexp))
	 ;; Move to end of expression
	 (setq beg (point))
	 (forward-sexp)
	 (while (looking-at "\\>[[(]\\|\\.")
	   ;; an array
	   (forward-sexp))
	 (setq end (point)))))

      ;; Get expression, but first move the begin mark if a
      ;; process-mark is inside the region, to keep the overlay from
      ;; wandering in the Shell.
      (when (and beg end)
	(if (and process-mark (> process-mark beg) (< process-mark end))
	    (setq beg (marker-position process-mark)))
	(setq expr (buffer-substring beg end)))

      ;; Show the overlay(s) and attach any necessary hooks and filters
      (when (and beg end idlwave-shell-expression-overlay)
	(move-overlay idlwave-shell-expression-overlay beg end
		      (current-buffer))
	(add-hook 'pre-command-hook
		  'idlwave-shell-delete-expression-overlay))
      (add-hook 'pre-command-hook
		'idlwave-shell-delete-output-overlay)

      ;; Remove empty or comment-only lines
      (while (string-match "\n[ \t]*\\(;.*\\)?\r*\n" expr)
	(setq expr (replace-match "\n" t t expr)))
      ;; Concatenate continuation lines
      (while (string-match "[ \t]*\\$[ \t]*\\(;.*\\)?\\(\n[ \t]*\\|$\\)" expr)
	(setq expr (replace-match "" t t expr)))
      ;; Remove final newline
      (if (string-match "\n[ \t\r]*\\'" expr)
	  (setq expr (replace-match "" t t expr)))

      (catch 'exit
	;; Pop-up or complete on the examine selection list, if appropriate
	(if (or
	     complete-help-type
	     (and ev idlwave-shell-examine-alist)
	     (consp help))
	    (let ((help-cons
		   (if (consp help) help
		     (assoc
		      ;; A cons from either a pop-up or mini-buffer completion
		      (if complete-help-type
			  (idlwave-one-key-select 'idlwave-shell-examine-alist
						  "Examine with: " 1.5)
;; 			  (idlwave-completing-read
;; 			   "Examine with: "
;; 			   idlwave-shell-examine-alist nil nil nil
;; 			   'idlwave-shell-examine-completion-list
;; 			   "Print")
			(idlwave-popup-select
			 ev
			 (mapcar 'car idlwave-shell-examine-alist)
			 "Examine with"))
		      idlwave-shell-examine-alist))))
	      (setq help (cdr help-cons))
	      (if (null help) (throw 'exit nil))
	      (if idlwave-shell-separate-examine-output
		  (setq idlwave-shell-examine-label
			(concat
			 (format "==>%s<==\n%s:" expr (car help-cons))
			 stack-label "\n"))))
	  ;; The regular help label (no popups, cons cells, etc.)
	  (setq idlwave-shell-examine-label
		(concat
		 (format "==>%s<==\n%s:" expr
			 (cond ((null help) "print")
			       ((stringp help) help)
			       (t (symbol-name help))))
		 stack-label "\n")))

	;; Send the command
	(if stack-label
	    (setq expr (idlwave-retrieve-expression-from-level
			expr
			idlwave-shell-calling-stack-index)))
	(setq cmd (idlwave-shell-help-statement help expr))
	;;(idlwave-shell-recenter-shell-window)
	(idlwave-shell-send-command
	 cmd
	 'idlwave-shell-check-compiled-and-display
	 (if idlwave-shell-separate-examine-output 'hide))))))

(defvar idlwave-shell-examine-window-alist nil
  "Variable to hold the win/height pairs for all *Examine* windows.")

(defvar idlwave-shell-examine-map (make-sparse-keymap))
(define-key idlwave-shell-examine-map "q" 'idlwave-shell-examine-display-quit)
(define-key idlwave-shell-examine-map "c" 'idlwave-shell-examine-display-clear)


(defun idlwave-shell-check-compiled-and-display ()
  "Check examine output for warning about undefined procedure/function."
  (if (string-match "% Attempt to call undefined" idlwave-shell-command-output)
      (idlwave-shell-compile-helper-routines))
  (if idlwave-shell-separate-examine-output
      (idlwave-shell-examine-display)
    (idlwave-shell-examine-highlight)))

(defun idlwave-shell-examine-display ()
  "View the examine command output in a separate buffer."
  (let (win cur-beg cur-end)
    (with-current-buffer (get-buffer-create "*Examine*")
      (use-local-map idlwave-shell-examine-map)
      (setq buffer-read-only nil)
      (goto-char (point-max))
      (save-restriction
	(narrow-to-region (point) (point))
	(if (string-match "^% Syntax error." idlwave-shell-command-output)
	    (insert "% Syntax error.\n")
	  (insert idlwave-shell-command-output)
	  ;; Just take the last bit between the prompts (if more than one).
	  (let* ((end (or
		       (re-search-backward idlwave-shell-prompt-pattern nil t)
		       (point-max)))
		 (beg (progn
			(goto-char
			 (or (progn (if (re-search-backward
					 idlwave-shell-prompt-pattern nil t)
					(match-end 0)))
			     (point-min)))
			(re-search-forward "\n")))
		 (str (buffer-substring beg end)))
	    (delete-region (point-min) (point-max))
	    (insert str)
	    (if idlwave-shell-examine-label
		(progn (goto-char (point-min))
		       (insert idlwave-shell-examine-label)
		       (setq idlwave-shell-examine-label nil)))))
	(setq cur-beg (point-min)
	      cur-end (point-max))
	(setq buffer-read-only t)
	(move-overlay idlwave-shell-output-overlay cur-beg cur-end
		      (current-buffer))

	;; Look for the examine buffer in all windows.  If one is
	;; found in a frame all by itself, use that, otherwise, switch
	;; to or create an examine window in this frame, and resize if
	;; it's a newly created window
	(let* ((winlist (get-buffer-window-list "*Examine*" nil 'visible)))
	  (setq win (idlwave-display-buffer
		     "*Examine*"
		     nil
		     (let ((list winlist) thiswin)
		       (catch 'exit
			 (save-selected-window
			   (while (setq thiswin (pop list))
			     (select-window thiswin)
			     (if (one-window-p)
				 (throw 'exit (window-frame thiswin)))))))))
	  (set-window-start win (point-min)) ; Ensure the point is visible.
	  (save-selected-window
	    (select-window win)
	    (let ((elt (assoc win idlwave-shell-examine-window-alist)))
	      (when (and (not (one-window-p))
			 (or (not (memq win winlist)) ;a newly created window
			     (eq (window-height) (cdr elt))))
		;; Autosize it.
		(enlarge-window (- (/ (frame-height) 2)
				   (window-height)))
		(shrink-window-if-larger-than-buffer)
		;; Clean the window list of dead windows
		(setq idlwave-shell-examine-window-alist
		      (delq nil
			    (mapcar (lambda (x) (if (window-live-p (car x)) x))
				    idlwave-shell-examine-window-alist)))
		;; And add the new value.
		(if (setq elt (assoc win idlwave-shell-examine-window-alist))
		    (setcdr elt (window-height))
		  (add-to-list 'idlwave-shell-examine-window-alist
			       (cons win (window-height)))))))))
      ;; Recenter for maximum output, after widened
      (save-selected-window
	(select-window win)
	(goto-char (point-max))
	(skip-chars-backward "\n")
	(recenter -1)))))

(defun idlwave-shell-examine-display-quit ()
  (interactive)
  (let ((win (selected-window)))
    (if (one-window-p)
	(delete-frame (window-frame win))
      (delete-window win))))

(defun idlwave-shell-examine-display-clear ()
  (interactive)
  (let ((buf (get-buffer "*Examine*")))
    (when (bufferp buf)
      (with-current-buffer buf
	(let ((inhibit-read-only t))
          (erase-buffer))))))

(defun idlwave-retrieve-expression-from-level (expr level)
  "Return IDL command to print the expression EXPR from stack level LEVEL.

It does not seem possible to evaluate an expression on a different
level than the current.  Therefore, this function retrieves variables
by reference from other levels, and then includes that variable in
place of the chosen one.

Since this function depends upon the undocumented IDL routine
routine_names, there is no guarantee that this will work with future
versions of IDL."
  (let ((fetch (- 0 level))
	(start 0)
        var fetch-start fetch-end pre post)

    ;; FIXME: In the following we try to find the variables in expression
    ;; This is quite empirical - I don't know in what situations this will
    ;; break.  We will look for identifiers and exclude cases where we
    ;; know it is not a variable.  To distinguish array references from
    ;; function calls, we require that arrays use [] instead of ()

    (while (string-match
	    "\\(\\`\\|[^a-zA-Z0-9$_][ \t]*\\)\\([a-zA-Z][a-zA-Z0-9$_]*\\)\\([ \t]*[^a-zA-Z0-9$_]\\|\\'\\)" expr start)
      (setq var (match-string 2 expr)
	    start (match-end 2)
	    pre (substring expr 0 (match-beginning 2))
	    post (substring expr (match-end 2)))
      (cond
       ((or
	;; Exclude identifiers which are not variables
	 (string-match ",[ \t$\n]*/\\'" pre)        ;; a `/' KEYWORD
	 (and (string-match "[,(][ \t\n]*\\'" pre)
	      (string-match "\\`[ \t]*=" post))  ;; a `=' KEYWORD
	 (string-match "\\`(" post)              ;; a function
	 (string-match "->[ \t]*\\'" pre)        ;; a method
	 (string-match "\\.\\'" pre)))             ;; structure member

       ;; Skip over strings
       ((and (string-match "\\([\"\']\\)[^\1]*$" pre)
	     (string-match (concat "^[^" (match-string 1 pre) "]*"
				   (match-string 1 pre)) post))
	(setq start (+ start (match-end 0))))


       ;; seems to be a variable - delimit its name
       (t
	(put-text-property start (- start (length var)) 'fetch t expr))))

    (setq start 0)
    (while (setq fetch-start
		 (next-single-property-change start 'fetch expr))
      (if (get-text-property start 'fetch expr) ; it's on in range
	  (setq fetch-end fetch-start ;it's off in range
		fetch-start start)
	(setq fetch-end (next-single-property-change fetch-start 'fetch expr)))
      (unless fetch-end (setq fetch-end (length expr)))
      (remove-text-properties fetch-start fetch-end '(fetch) expr)
      (setq expr (concat (substring expr 0 fetch-start)
			 (format "(routine_names('%s',fetch=%d))"
				 (substring expr fetch-start fetch-end)
				 fetch)
			 (substring expr fetch-end)))
      (setq start fetch-end))
    (if (get-text-property 0 'fetch expr) ; Full expression, left over
	(setq expr (format "(routine_names('%s',fetch=%d))" expr fetch)))
    expr))


(defun idlwave-shell-help-statement (help expr)
  "Construct a help statement for printing expression EXPR.

HELP can be non-nil for `help,', nil for 'print,' or any string into which
to insert expression in place of the marker ___, e.g.: print,
size(___,/DIMENSIONS)"
  (cond
   ((null help)
    (concat "idlwave_print_safe, " expr ","
	    (number-to-string idlwave-shell-max-print-length)))
   ((stringp help)
    (if (string-match "\\(^\\|[^_]\\)\\(___\\)\\([^_]\\|$\\)" help)
	(concat (substring help 0 (match-beginning 2))
		expr
		(substring help (match-end 2)))))
   (t
    (concat "help, " expr))))


(defun idlwave-shell-examine-highlight ()
  "Highlight the most recent IDL output."
  (let* ((buffer (get-buffer (idlwave-shell-buffer)))
	 (process (get-buffer-process buffer))
	 (process-mark (if process (process-mark process)))
	 output-begin output-end)
    (with-current-buffer buffer
      (goto-char process-mark)
      (beginning-of-line)
      (setq output-end (point))
      (re-search-backward idlwave-shell-prompt-pattern nil t)
      (beginning-of-line 2)
      (setq output-begin (point)))

    ;; First make sure the shell window is visible
    (idlwave-display-buffer (idlwave-shell-buffer)
			    nil (idlwave-shell-shell-frame))
    (if (and idlwave-shell-output-overlay process-mark)
	(move-overlay idlwave-shell-output-overlay
		      output-begin output-end buffer))))

(defun idlwave-shell-delete-output-overlay ()
  (unless (or (eq this-command 'idlwave-shell-mouse-nop)
	      (eq this-command 'handle-switch-frame))
    (condition-case nil
	(if idlwave-shell-output-overlay
	    (delete-overlay idlwave-shell-output-overlay))
      (error nil))
    (remove-hook 'pre-command-hook 'idlwave-shell-delete-output-overlay)))

(defun idlwave-shell-delete-expression-overlay ()
  (unless (or (eq this-command 'idlwave-shell-mouse-nop)
	      (eq this-command 'handle-switch-frame))
    (condition-case nil
	(if idlwave-shell-expression-overlay
	    (delete-overlay idlwave-shell-expression-overlay))
      (error nil))
    (remove-hook 'pre-command-hook 'idlwave-shell-delete-expression-overlay)))

(defvar idlwave-shell-bp-alist nil
  "Alist of breakpoints.
A breakpoint is a cons cell \(\(file line\) . \(\(index module\) data\)\)

The car is the `frame' for the breakpoint:
file - full path file name.
line - line number of breakpoint - integer.

The first element of the cdr is a list of internal IDL data:
index - the index number of the breakpoint internal to IDL.
module - the module for breakpoint internal to IDL.

Remaining elements of the cdr:
data - Data associated with the breakpoint by idlwave-shell currently
contains four items:

count - number of times to execute breakpoint.  When count reaches 0
  the breakpoint is cleared and removed from the alist.

command - command to execute when breakpoint is reached, either a
  lisp function to be called with `funcall' with no arguments or a
  list to be evaluated with `eval'.

condition - any condition to apply to the breakpoint.

disabled - whether the bp is disabled.")

(defun idlwave-shell-run-region (beg end &optional n)
  "Compile and run the region using the IDL process.
Copies the region to a temporary file `idlwave-shell-temp-pro-file'
and issues the IDL .run command for the file.  Because the region
is compiled and run as a main program there is no problem with
begin-end blocks extending over multiple lines - which would be
a problem if `idlwave-shell-evaluate-region' was used.
An END statement is appended to the region if necessary.

If there is a prefix argument, display IDL process."
  (interactive "r\nP")
  (let ((oldbuf (current-buffer)))
    (with-current-buffer (idlwave-find-file-noselect
                          (idlwave-shell-temp-file 'pro) 'tmp)
      (set (make-local-variable 'comment-start-skip) ";+[ \t]*")
      (set (make-local-variable 'comment-start) ";")
      (erase-buffer)
      (insert-buffer-substring oldbuf beg end)
      (if (not (save-excursion
                 (idlwave-previous-statement)
                 (idlwave-look-at "\\<end\\>")))
          (insert "\nend\n"))
      (save-buffer 0)))
  (idlwave-shell-send-command (concat ".run \""
				      idlwave-shell-temp-pro-file "\"")
			      nil
			      (if (idlwave-shell-hide-p 'run) 'mostly)
			      nil t)
  (if n
      (idlwave-display-buffer (idlwave-shell-buffer)
			      nil (idlwave-shell-shell-frame))))

(defun idlwave-shell-evaluate-region (beg end &optional n)
  "Send region to the IDL process.
If there is a prefix argument, display IDL process.
Does not work for a region with multiline blocks - use
`idlwave-shell-run-region' for this."
  (interactive "r\nP")
  (idlwave-shell-send-command (buffer-substring beg end))
  (if n
      (idlwave-display-buffer (idlwave-shell-buffer)
			      nil (idlwave-shell-shell-frame))))

(defun idlwave-shell-delete-temp-files ()
  "Delete the temporary files and kill associated buffers."
  (if (stringp idlwave-shell-temp-pro-file)
      (condition-case nil
	  (let ((buf (idlwave-get-buffer-visiting
		      idlwave-shell-temp-pro-file)))
	    (if (buffer-live-p buf)
		(kill-buffer buf))
	    (delete-file idlwave-shell-temp-pro-file))
	(error nil)))
  (if (stringp idlwave-shell-temp-rinfo-save-file)
      (condition-case nil
	  (delete-file idlwave-shell-temp-rinfo-save-file)
	(error nil))))

(defun idlwave-display-buffer (buf not-this-window-p &optional frame)
  (if (featurep 'xemacs)
      ;; The XEmacs version enforces the frame
      (display-buffer buf not-this-window-p frame)
    ;; For Emacs, we need to force the frame ourselves.
    (let ((this-frame (selected-frame)))
      (save-excursion ;; make sure we end up in the same buffer
	(if (frame-live-p frame)
	    (select-frame frame))
	(if (eq this-frame (selected-frame))
	    ;; same frame:  use display buffer, to make sure the current
	    ;; window stays.
	    (display-buffer buf)
	  ;; different frame
	  (if (one-window-p)
	      ;; only window:  switch
	      (progn
		(switch-to-buffer buf)
		(selected-window))   ; must return the window.
	    ;; several windows - use display-buffer
	    (display-buffer buf not-this-window-p)))))))
;  (if (not (frame-live-p frame)) (setq frame nil))
;  (display-buffer buf not-this-window-p frame))

(defvar idlwave-shell-bp-buffer " *idlwave-shell-bp*"
  "Scratch buffer for parsing IDL breakpoint lists and other stuff.")

(defun idlwave-shell-bp-query (&optional no-show)
  "Reconcile idlwave-shell's breakpoint list with IDL's.
Queries IDL using the string in `idlwave-shell-bp-query'."
  (interactive)
  (idlwave-shell-send-command idlwave-shell-bp-query
			      `(progn
				(idlwave-shell-filter-bp (quote ,no-show)))
			      'hide))

(defun idlwave-shell-bp-get (bp &optional item)
  "Get a value for a breakpoint.
BP has the form of elements in `idlwave-shell-bp-alist'.
Optional second arg ITEM is the particular value to retrieve.
ITEM can be 'file, 'line, 'index, 'module, 'count, 'cmd,
'condition, 'disabled, 'type, or 'data.  'data returns a list
of 'count, 'cmd and 'condition.  Defaults to 'index."
  (cond
   ;; Frame
   ((eq item 'line) (nth 1 (car bp)))
   ((eq item 'file) (nth 0 (car bp)))
   ;; idlwave-shell breakpoint data
   ((eq item 'data) (cdr (cdr bp)))
   ((eq item 'count) (nth 0 (cdr (cdr bp))))
   ((eq item 'cmd) (nth 1 (cdr (cdr bp))))
   ((eq item 'condition) (nth 2 (cdr (cdr bp))))
   ((eq item 'disabled) (nth 3 (cdr (cdr bp))))
   ;; IDL breakpoint info
   ((eq item 'module)
    (let ((module (nth 1 (car (cdr bp)))))
      (if (listp module) (car module) module)))
   ((eq item 'type)
    (let ((module (nth 1 (car (cdr bp)))))
      (if (listp module) (nth 1 module))))
   ;;    index - default
   (t (nth 0 (car (cdr bp))))))

(defun idlwave-shell-filter-bp (&optional no-show)
  "Get the breakpoints from `idlwave-shell-command-output'.
Create `idlwave-shell-bp-alist' updating breakpoint count and command
data from previous breakpoint list.  If NO-SHOW is set, don't update
the breakpoint overlays."
  (with-current-buffer (get-buffer-create idlwave-shell-bp-buffer)
    (erase-buffer)
    (insert idlwave-shell-command-output)
    (goto-char (point-min))
    (let ((old-bp-alist idlwave-shell-bp-alist)
	  ;; Searching the breakpoints
	  ;; In IDL 5.5, the breakpoint reporting format changed.
	  (bp-re54 "^[ \t]*\\([0-9]+\\)[ \t]+\\(\\S-+\\)?[ \t]+\\([0-9]+\\)[ \t]+\\(\\S-+\\)")
	  (bp-re55
	   (concat
	    "^\\s-*\\([0-9]+\\)"    ; 1 index
	    "\\s-+\\([0-9]+\\)"     ; 2 line number
	    "\\s-+\\(Uncompiled\\|" ; 3-6 either uncompiled or routine name
	    "\\(\\(Func=\\|Pro=\\)\\(\\$?[a-zA-Z][a-zA-Z0-9$_:]*\\$?\\)\\)\\)"
	    "\\(\\s-*,\\s-*After=[0-9]+/\\([0-9]+\\)?\\)?" ; 7-8 After part
	    "\\(\\s-*,\\s-*\\(BreakOnce\\)\\)?"            ; 9-10 BreakOnce
	    "\\(\\s-*,\\s-*\\(Condition='\\(.*\\)'\\)\n?\\)?" ; 11-13 Condition
	    "\\(\\s-*,\\s-*\\(Disabled\\)\n?\\)?"          ; 14-15 Disabled
	    "\\s-+\\(\\S-+\\)"))                           ; 16 File name
	  file line index module
	  count condition disabled
	  bp-re indmap)
      (setq idlwave-shell-bp-alist (list nil))
      ;; Search for either header type, and set the correct regexp
      (when (or
	     (if (re-search-forward "^\\s-*Index.*\n\\s-*-" nil t)
		 (setq bp-re bp-re54    ; versions <= 5.4
		       indmap '(1 2 3 4))) ;index module line file
	     (if (re-search-forward
		  "^\\s-*Index\\s-*Line\\s-*Attributes\\s-*File" nil t)
		 (setq bp-re bp-re55    ; versions >= 5.5
		       indmap '(1 6 2 16)))) ; index module line file
	;; There seems to be a breakpoint listing here, parse breakpoint lines.
	(while (re-search-forward bp-re nil t)
	  (setq index (string-to-number (match-string (nth 0 indmap)))
		module (match-string (nth 1 indmap))
		line (string-to-number (match-string (nth 2 indmap)))
		file (idlwave-shell-file-name (match-string (nth 3 indmap))))
	  (if (eq bp-re bp-re55)
	      (setq count (if (match-string 10) 1
			    (if (match-string 8)
				(string-to-number (match-string 8))))
		    condition (match-string 13)
		    disabled (not (null (match-string 15)))))

	  ;; Add the breakpoint info to the list
	  (nconc idlwave-shell-bp-alist
		 (list (cons (list file line)
			     (list
			      (list index module)
			      ;; bp data: count, command, condition, disabled
			      count nil condition disabled))))))
      (setq idlwave-shell-bp-alist (cdr idlwave-shell-bp-alist))
      ;; Update breakpoint data
      (if (eq bp-re bp-re54)
	  (mapc 'idlwave-shell-update-bp old-bp-alist)
	(mapc 'idlwave-shell-update-bp-command-only old-bp-alist))))
  ;; Update the breakpoint overlays
  (unless no-show (idlwave-shell-update-bp-overlays))
  ;; Return the new list
  idlwave-shell-bp-alist)

(defun idlwave-shell-update-bp-command-only (bp)
  (idlwave-shell-update-bp bp t))

(defun idlwave-shell-update-bp (bp &optional command-only)
  "Update BP data in breakpoint list.
If BP frame is in `idlwave-shell-bp-alist' updates the breakpoint data."
  (let ((match (assoc (car bp) idlwave-shell-bp-alist)))
    (if match
	(if command-only
	    (setf (nth 1 (cdr (cdr match))) (nth 1 (cdr (cdr match))))
	  (setcdr (cdr match) (cdr (cdr bp)))))))

(defun idlwave-shell-set-bp-data (bp data)
  "Set the data of BP to DATA."
  (setcdr (cdr bp) data))

(defun idlwave-shell-bp (frame &optional data module)
  "Create a breakpoint structure containing FRAME and DATA.
Second and third args, DATA and MODULE, are optional.  Returns
a breakpoint of the format used in `idlwave-shell-bp-alist'.
Can be used in commands attempting match a breakpoint in
`idlwave-shell-bp-alist'."
  (cons frame ;; (file line)
	(cons (list nil module) ;; (index_id (module type) | module)
	      data)))           ;; (count command condition disabled)

(defvar idlwave-shell-old-bp nil
  "List of breakpoints previous to setting a new breakpoint.")

(defun idlwave-shell-sources-bp (bp)
  "Check `idlwave-shell-sources-alist' for source of breakpoint using BP.
If an equivalency is found, return the IDL internal source name.
Otherwise return the filename in BP."
  (let*
      ((bp-file (idlwave-shell-bp-get bp 'file))
       (bp-module (idlwave-shell-bp-get bp 'module))
       (internal-file-list
	(if bp-module
	    (cdr (assoc bp-module idlwave-shell-sources-alist)))))
    (if (and internal-file-list
	     (equal bp-file (nth 0 internal-file-list)))
        (nth 1 internal-file-list)
      bp-file)))

(defun idlwave-shell-set-bp (bp &optional no-show)
  "Try to set a breakpoint BP.
The breakpoint will be placed at the beginning of the statement on the
line specified by BP or at the next IDL statement if that line is not
a statement.  Determines IDL's internal representation for the
breakpoint, which may have occurred at a different line than
specified.  If NO-SHOW is non-nil, don't do any updating."
  ;; Get and save the old breakpoints
  (idlwave-shell-send-command
   idlwave-shell-bp-query
   `(progn
     (idlwave-shell-filter-bp (quote ,no-show))
     (setq idlwave-shell-old-bp idlwave-shell-bp-alist))
   'hide)

  ;; Get sources for this routine in the sources list
  (idlwave-shell-module-source-query (idlwave-shell-bp-get bp 'module)
				     (idlwave-shell-bp-get bp 'type))
  (let*
      ((count (idlwave-shell-bp-get bp 'count))
       (condition (idlwave-shell-bp-get bp 'condition))
       (disabled (idlwave-shell-bp-get bp 'disabled))
       (key (concat (if (and count (numberp count))
			(cond
			 ((= count 1) ",/once")
			 ((> count 1) (format ",after=%d" count))))
		    (if condition (concat ",CONDITION=\"" condition "\""))
		    ;; IDL can't simultaneously set a condition/count
		    ;; and disable a breakpoint, but it does keep both
		    ;; of these when resetting the same BP.  We assume
		    ;; DISABLE and CONDITION/COUNT are not set
		    ;; together for a newly created breakpoint.
		    (if (and disabled (not condition) (not count))
			    ",/DISABLE")))
       (line (idlwave-shell-bp-get bp 'line)))
    (idlwave-shell-send-command
     (concat "breakpoint,'"
	     (idlwave-shell-sources-bp bp) "',"
	     (if (integerp line) (setq line (int-to-string line)))
	     key)
     ;; Check for failure and adjust breakpoint to match IDL's list
     `(progn
	(if (idlwave-shell-set-bp-check (quote ,bp))
	    (idlwave-shell-set-bp-adjust (quote ,bp) (quote ,no-show))))
     ;; hide output?
     (idlwave-shell-hide-p 'breakpoint)
     'preempt t)))

(defun idlwave-shell-set-bp-adjust (bp &optional no-show)
  "Find the breakpoint in IDL's internal list of breakpoints."
  (idlwave-shell-send-command
   idlwave-shell-bp-query
   `(progn
      (idlwave-shell-filter-bp 'no-show)
      (idlwave-shell-new-bp (quote ,bp))
      (unless (quote ,no-show)
	(idlwave-shell-update-bp-overlays)))
   'hide
   'preempt))

(defun idlwave-shell-find-bp (frame)
  "Return breakpoint from `idlwave-shell-bp-alist' for frame.
Returns nil if frame not found."
  (assoc frame idlwave-shell-bp-alist))

(defun idlwave-shell-find-current-bp ()
  "Find breakpoint here, or at halt location."
  (let ((bp (idlwave-shell-find-bp (idlwave-shell-current-frame))))
    (when (not bp)
      ;; Try moving to beginning of halted-at statement
      (save-excursion
	(idlwave-shell-goto-frame)
	(idlwave-beginning-of-statement)
	(setq bp (idlwave-shell-find-bp (idlwave-shell-current-frame))))
      (unless bp
	(beep)
	(message "Cannot identify breakpoint for this line")))
    bp))

(defun idlwave-shell-new-bp (bp)
  "Find the new breakpoint in IDL's list and update with DATA.
The actual line number for a breakpoint in IDL may be different than
the line number used with the IDL breakpoint command.
Looks for a new breakpoint index number in the list.  This is
considered the new breakpoint if the file name of frame matches."
  (let ((obp-index (mapcar 'idlwave-shell-bp-get idlwave-shell-old-bp))
        (bpl idlwave-shell-bp-alist))
    (while (and (member (idlwave-shell-bp-get (car bpl)) obp-index)
                (setq bpl (cdr bpl))))
    (if (and
         (not bpl)
         ;; No additional breakpoint.
         ;; Need to check if we are just replacing a breakpoint.
         (setq bpl (assoc (car bp) idlwave-shell-bp-alist)))
        (setq bpl (list bpl)))
    (if (and bpl
             (equal (idlwave-shell-bp-get (setq bpl (car bpl)) 'file)
                    (idlwave-shell-bp-get bp 'file)))
        ;; Got the breakpoint - add count, command to it.
        ;; This updates `idlwave-shell-bp-alist' because a deep copy was
        ;; not done for bpl.
        (idlwave-shell-set-bp-data bpl (idlwave-shell-bp-get bp 'data))
      (beep)
      (message "Failed to identify breakpoint in IDL"))))

(defvar idlwave-shell-bp-overlays nil
  "Alist of overlays marking breakpoints.")
(defvar idlwave-shell-bp-glyph)

(defvar idlwave-shell-debug-line-map (make-sparse-keymap))
(define-key idlwave-shell-debug-line-map
  (if (featurep 'xemacs) [button3] [mouse-3])
  'idlwave-shell-mouse-active-bp)

(defun idlwave-shell-update-bp-overlays ()
  "Update the overlays which mark breakpoints in the source code.
Existing overlays are recycled, in order to minimize consumption."
  (when idlwave-shell-mark-breakpoints
    (let ((ov-alist (copy-alist idlwave-shell-bp-overlays))
	  (bp-list idlwave-shell-bp-alist)
	  (use-glyph (and (memq idlwave-shell-mark-breakpoints '(t glyph))
			  idlwave-shell-bp-glyph))
	  ov ov-list bp buf old-buffers win)

      ;; Delete the old overlays from their buffers
      (if ov-alist
	  (while (setq ov-list (pop ov-alist))
	    (while (setq ov (pop (cdr ov-list)))
	      (add-to-list 'old-buffers (overlay-buffer ov))
	      (delete-overlay ov))))

      (setq ov-alist idlwave-shell-bp-overlays
	    idlwave-shell-bp-overlays
	    (if idlwave-shell-bp-glyph
		(mapcar 'list (mapcar 'car idlwave-shell-bp-glyph))
	      (list (list 'bp))))
      (while (setq bp (pop bp-list))
	(save-excursion
	  (idlwave-shell-goto-frame (car bp))
	  (let* ((end (point-at-eol))
		 (beg (progn (beginning-of-line 1) (point)))
		 (condition (idlwave-shell-bp-get bp 'condition))
		 (count (idlwave-shell-bp-get bp 'count))
		 (disabled (idlwave-shell-bp-get bp 'disabled))
		 (type (if idlwave-shell-bp-glyph
			   (cond
			    (condition 'bp-cond )
			    (count
			     (cond
			      ((<= count 0) 'bp)
			      ((<= count 4)
			       (intern
				(concat "bp-" (number-to-string count))))
			      (t 'bp-n)))
			    (t 'bp))
			 'bp))
		 (help-list
		  (delq nil
			(list
			 (if count
			     (concat "after:" (int-to-string count)))
			 (if condition
			     (concat "condition:" condition))
			 (if disabled "disabled"))))
		 (help-text (concat
			     "BP "
			     (int-to-string (idlwave-shell-bp-get bp))
			     (if help-list
				 (concat
				  " - "
				  (mapconcat 'identity help-list ", ")))
			     (if (and (not count) (not condition))
				 " (use mouse-3 for breakpoint actions)")))
		 (full-type (if disabled
				(intern (concat (symbol-name type)
						"-disabled"))
			      type))
		 (ov-existing (assq full-type ov-alist))
		 (ov (or (and (cdr ov-existing)
			      (pop (cdr ov-existing)))
			 (idlwave-shell-make-new-bp-overlay type disabled)))
		 match)
	    (if idlwave-shell-breakpoint-popup-menu
		(overlay-put ov 'help-echo help-text))
	    (move-overlay ov beg end)
	    (if (setq match (assq full-type idlwave-shell-bp-overlays))
		(push ov (cdr match))
	      (nconc idlwave-shell-bp-overlays
		     (list (list full-type ov)))))
	  ;; Take care of margins if using a glyph
	  (when use-glyph
	    (if old-buffers
		(setq old-buffers (delq (current-buffer) old-buffers)))
	    (if (fboundp 'set-specifier) ;; XEmacs
		(set-specifier left-margin-width (cons (current-buffer) 2))
	      (if (< left-margin-width 2)
		  (setq left-margin-width 2)))
	    (let ((window (get-buffer-window (current-buffer) 0)))
	      (if window
		  (set-window-margins
		   window left-margin-width right-margin-width))))))
      (if use-glyph
	  (while (setq buf (pop old-buffers))
	    (with-current-buffer buf
	      (if (fboundp 'set-specifier) ;; XEmacs
		  (set-specifier left-margin-width (cons (current-buffer) 0))
		(setq left-margin-width 0))
	      (let ((window (get-buffer-window buf 0)))
		(if window
		    (set-window-margins
		     window left-margin-width right-margin-width)))))))))

(defun idlwave-shell-make-new-bp-overlay (&optional type disabled)
  "Make a new overlay for highlighting breakpoints.

This stuff is strongly dependent upon the version of Emacs.  If TYPE
is passed, make an overlay of that type ('bp or 'bp-cond, currently
only for glyphs)."
  (let ((ov (make-overlay 1 1))
	(use-glyph (and (memq idlwave-shell-mark-breakpoints '(t glyph))
			idlwave-shell-bp-glyph))
	(type (or type 'bp))
	(face (if disabled
		  idlwave-shell-disabled-breakpoint-face
		idlwave-shell-breakpoint-face)))
    (if (featurep 'xemacs)
	;; This is XEmacs
	(progn
	  (when idlwave-shell-breakpoint-popup-menu
	    (set-extent-property ov 'mouse-face 'highlight)
	    (set-extent-property ov 'keymap idlwave-shell-debug-line-map))

	  (cond
	   ;; tty's cannot display glyphs
	   ((eq (console-type) 'tty)
	    (set-extent-property ov 'face face))

	   ;; use the glyph
	   (use-glyph
	    (let ((glyph (cdr (assq type idlwave-shell-bp-glyph))))
	      (if disabled (setq glyph (car glyph)) (setq glyph (nth 1 glyph)))
	      (set-extent-property ov 'begin-glyph glyph)
	      (set-extent-property ov 'begin-glyph-layout 'outside-margin)))

	   ;; use the face
	   (idlwave-shell-mark-breakpoints
	    (set-extent-property ov 'face face))

	   ;; no marking
	   (t nil))
	  (set-extent-priority ov -1))  ; make stop line face prevail
      ;; This is Emacs
      (when idlwave-shell-breakpoint-popup-menu
	(overlay-put ov 'mouse-face 'highlight)
	(overlay-put ov 'keymap idlwave-shell-debug-line-map))
      (cond
       (window-system
	(if use-glyph
	    (let ((image-props (cdr (assq type idlwave-shell-bp-glyph)))
		  string)

	      (if disabled (setq image-props
				 (append image-props
					 (list :conversion 'disabled))))
	      (setq string
		   (propertize "@"
			       'display
			       (list (list 'margin 'left-margin)
				     image-props)))
	      (overlay-put ov 'before-string string))
	  ;; just the face
	  (overlay-put ov 'face face)))

       ;; use a face
       (idlwave-shell-mark-breakpoints
	(overlay-put ov 'face face))

       ;; No marking
       (t nil)))
    ov))

(defun idlwave-shell-mouse-active-bp (ev)
  "Does right-click mouse action on breakpoint lines."
  (interactive "e")
  (if ev (mouse-set-point ev))
  (let ((bp (idlwave-shell-find-bp (idlwave-shell-current-frame)))
	index condition count select cmd disabled)
    (unless bp
      (error "Breakpoint not found"))
    (setq index (int-to-string (idlwave-shell-bp-get bp))
	  condition (idlwave-shell-bp-get bp 'condition)
	  cmd (idlwave-shell-bp-get bp 'cmd)
	  count (idlwave-shell-bp-get bp 'count)
	  disabled (idlwave-shell-bp-get bp 'disabled))
    (setq select (idlwave-popup-select
		  ev
		  (delq nil
			(list (if disabled "Enable" "Disable")
			      "Clear"
			      "Clear All"
			      (if condition "Remove Condition" "Add Condition")
			      (if condition "Change Condition")
			      (if count "Remove Repeat Count"
				"Add Repeat Count")
			      (if count "Change Repeat Count")))
		  (concat "BreakPoint " index)))
    (if select
	(cond
	 ((string-equal select "Clear All")
	  (idlwave-shell-clear-all-bp))
	 ((string-equal select "Clear")
	  (idlwave-shell-clear-current-bp))
	 ((string-match "Condition" select)
	  (idlwave-shell-break-here count cmd
				    (if (or (not condition)
					    (string-match "Change" select))
				      (read-string "Break Condition: "))
				    disabled))
	 ((string-match "Count" select)
	  (idlwave-shell-break-here (if (or (not count)
					    (string-match "Change" select))
					(string-to-number
					 (read-string "Break After Count: ")))
				    cmd condition disabled))
	 ((string-match "able$" select)
	  (idlwave-shell-toggle-enable-current-bp))
	 (t
	  (message "Unimplemented: %s" select))))))

(defun idlwave-shell-edit-default-command-line (arg)
  "Edit the current execute command."
  (interactive "P")
  (setq idlwave-shell-command-line-to-execute
	(read-string "IDL> " idlwave-shell-command-line-to-execute)))

(defun idlwave-shell-execute-default-command-line (arg)
  "Execute a command line.  On first use, ask for the command.
Also with prefix arg, ask for the command.  You can also use the command
`idlwave-shell-edit-default-command-line' to edit the line."
  (interactive "P")
  (cond
   ((equal arg '(16))
    (setq idlwave-shell-command-line-to-execute nil))
   ((equal arg '(4))
    (setq idlwave-shell-command-line-to-execute
	  (read-string "IDL> " idlwave-shell-command-line-to-execute))))
  (idlwave-shell-reset 'hidden)
  (idlwave-shell-send-command
   (or idlwave-shell-command-line-to-execute
       (with-current-buffer (idlwave-shell-buffer)
	 (ring-ref comint-input-ring 0)))
   '(idlwave-shell-redisplay 'hide)))

(defun idlwave-shell-save-and-run ()
  "Save file and run it in IDL.
Runs `save-buffer' and sends a '.RUN' command for the associated file to IDL.
When called from the shell buffer, re-run the file which was last handled by
one of the save-and-.. commands."
  (interactive)
  (idlwave-shell-save-and-action 'run))

(defun idlwave-shell-save-and-compile ()
  "Save file and run it in IDL.
Runs `save-buffer' and sends '.COMPILE' command for the associated file to IDL.
When called from the shell buffer, re-compile the file which was last handled by
one of the save-and-.. commands."
  (interactive)
  (idlwave-shell-save-and-action 'compile))

(defun idlwave-shell-save-and-batch ()
  "Save file and batch it in IDL.
Runs `save-buffer' and sends a '@file' command for the associated file to IDL.
When called from the shell buffer, re-batch the file which was last handled by
one of the save-and-.. commands."
  (interactive)
  (idlwave-shell-save-and-action 'batch))

(defun idlwave-shell-save-and-action (action)
  "Save file and compile it in IDL.
Runs `save-buffer' and sends a '.RUN' command for the associated file to IDL.
When called from the shell buffer, re-compile the file which was last
handled by this command."
  ;; Remove the stop overlay.
  (if idlwave-shell-stop-line-overlay
      (delete-overlay idlwave-shell-stop-line-overlay))
  (if idlwave-shell-is-stopped
      (idlwave-shell-electric-debug-all-off))
  (setq idlwave-shell-is-stopped nil)
  (setq overlay-arrow-string nil)
  (let (buf)
    (cond
     ((derived-mode-p 'idlwave-mode)
      (save-buffer)
      (setq idlwave-shell-last-save-and-action-file (buffer-file-name)))
     (idlwave-shell-last-save-and-action-file
      (if (setq buf (idlwave-get-buffer-visiting
		     idlwave-shell-last-save-and-action-file))
	  (with-current-buffer buf
	    (save-buffer))))
     (t (setq idlwave-shell-last-save-and-action-file
	      (read-file-name "File: ")))))
  (if (file-regular-p idlwave-shell-last-save-and-action-file)
      (progn
	(idlwave-shell-send-command
	 (concat (cond ((eq action 'run)     ".run ")
		       ((eq action 'compile) ".compile ")
		       ((eq action 'batch)   "@")
		       (t (error "Unknown action %s" action)))
		 "\""
		 idlwave-shell-last-save-and-action-file
		 "\"")
	 `(idlwave-shell-maybe-update-routine-info nil
	   ,idlwave-shell-last-save-and-action-file)
	 (if (idlwave-shell-hide-p 'run) 'mostly) nil t)
	(idlwave-shell-bp-query))
    (let ((msg (format "No such file %s"
		       idlwave-shell-last-save-and-action-file)))
      (setq idlwave-shell-last-save-and-action-file nil)
      (error msg))))

(defun idlwave-shell-maybe-update-routine-info (&optional wait file)
  "Update the routine info if the shell is not stopped at an error."
  (if (and (not idlwave-shell-is-stopped)
	   (or (eq t idlwave-auto-routine-info-updates)
	       (memq 'compile-buffer idlwave-auto-routine-info-updates))
	   idlwave-query-shell-for-routine-info
	   idlwave-routines)
      (idlwave-shell-update-routine-info t nil wait file)))

(defvar idlwave-shell-sources-query "help,/source,/full"
  "IDL command to obtain source files for compiled procedures.")

(defvar idlwave-shell-sources-alist nil
  "Alist of IDL procedure names and compiled source files.
Elements of the alist have the form:

  (module name . (source-file-truename idlwave-internal-filename))")

(defun idlwave-shell-module-source-query (module &optional type)
  "Determine the source file for a given module.
Query as a function if TYPE set to something beside 'pro."
  (if module
      (idlwave-shell-send-command
       (format "print,(routine_info('%s',/SOURCE%s)).PATH" module
	       (if (eq type 'pro) "" ",/FUNCTIONS"))
       `(idlwave-shell-module-source-filter ,module)
       'hide 'wait)))

(defun idlwave-shell-module-source-filter (module)
  "Get module source, and update `idlwave-shell-sources-alist'."
  (let ((old (assoc (upcase module) idlwave-shell-sources-alist))
	filename)
    (when (string-match "\.PATH *[\n\r]\\([^%][^\r\n]+\\)[\n\r]"
			idlwave-shell-command-output)
      (setq filename (substring idlwave-shell-command-output
				(match-beginning 1) (match-end 1)))
      (if old
	  (setcdr old (list (idlwave-shell-file-name filename) filename))
	(setq idlwave-shell-sources-alist
	      (append idlwave-shell-sources-alist
		      (list (cons (upcase module)
				  (list (idlwave-shell-file-name filename)
					filename)))))))))

(defun idlwave-shell-sources-query ()
  "Determine source files for all IDL compiled procedures.
Queries IDL using the string in `idlwave-shell-sources-query'."
  (interactive)
  (idlwave-shell-send-command idlwave-shell-sources-query
			      'idlwave-shell-sources-filter
			      'hide))

(defun idlwave-shell-sources-filter ()
  "Get source files from `idlwave-shell-sources-query' output.
Create `idlwave-shell-sources-alist' consisting of list elements
of the form:
 (module name . (source-file-truename idlwave-internal-filename))"
  (with-current-buffer (get-buffer-create idlwave-shell-bp-buffer)
    (erase-buffer)
    (insert idlwave-shell-command-output)
    (goto-char (point-min))
    (let (cpro cfun)
      (if (re-search-forward "Compiled Procedures:" nil t)
          (progn
            (forward-line) ; Skip $MAIN$
            (setq cpro (point))))
      (if (re-search-forward "Compiled Functions:" nil t)
          (progn
            (setq cfun (point))
            (setq idlwave-shell-sources-alist
                  (append
                   ;; compiled procedures
                   (progn
                     (narrow-to-region cpro (point-at-bol))
                     (goto-char (point-min))
                     (idlwave-shell-sources-grep))
                   ;; compiled functions
                   (progn
                     (widen)
                     (goto-char cfun)
                     (idlwave-shell-sources-grep)))))))))

(defun idlwave-shell-sources-grep ()
  (save-excursion
    (let ((al (list nil)))
      (while (and
              (not (progn (forward-line) (eobp)))
              (re-search-forward
               "\\s-*\\(\\S-+\\)\\s-+\\(\\S-+\\)" nil t))
        (nconc al
               (list
                (cons
                 (buffer-substring      ; name
                  (match-beginning 1) (match-end 1))
                 (let ((internal-filename
                        (buffer-substring       ; source
                         (match-beginning 2) (match-end 2))))
                   (list
                    (idlwave-shell-file-name internal-filename)
                    internal-filename))
		 ))))
      (cdr al))))

(defun idlwave-shell-clear-all-bp ()
  "Remove all breakpoints in IDL."
  (interactive)
  (idlwave-shell-send-command
   idlwave-shell-bp-query
   '(progn
      (idlwave-shell-filter-bp)
      (mapcar (lambda (x) (idlwave-shell-clear-bp x 'no-query))
	      idlwave-shell-bp-alist)
      (idlwave-shell-bp-query))
   'hide))

(defun idlwave-shell-list-all-bp ()
  "List all breakpoints in IDL."
  (interactive)
  (idlwave-shell-send-command
   idlwave-shell-bp-query))

(defvar idlwave-shell-error-last 0
  "Position of last syntax error in `idlwave-shell-error-buffer'.")

(defun idlwave-shell-goto-next-error ()
  "Move point to next IDL syntax error."
  (interactive)
  (let (frame col)
    (with-current-buffer idlwave-shell-error-buffer
      (goto-char idlwave-shell-error-last)
      (if (or
	   (re-search-forward idlwave-shell-syntax-error nil t)
	   (re-search-forward idlwave-shell-other-error nil t))
          (progn
            (setq frame
                  (list
                   (save-match-data
                     (idlwave-shell-file-name
                      (buffer-substring (match-beginning 1 )
					(match-end 1))))
                   (string-to-number
                    (buffer-substring (match-beginning 2)
                                      (match-end 2)))))
            ;; Try to find the column of the error
            (save-excursion
              (setq col
                    (if (re-search-backward "\\^" nil t)
                        (current-column)
                      0)))))
      (setq idlwave-shell-error-last (point)))
    (if frame
        (progn
          (idlwave-shell-display-line frame col 'disable))
      (beep)
      (message "No more errors."))))

(defun idlwave-shell-file-name (name)
  "If `idlwave-shell-use-truename' is non-nil, convert file name to true name.
Otherwise, just expand the file name."
  (let ((def-dir (if (derived-mode-p 'idlwave-shell-mode)
		     default-directory
		   idlwave-shell-default-directory)))
    (if idlwave-shell-use-truename
	(file-truename name def-dir)
      (expand-file-name name def-dir))))

;; Keybindings ------------------------------------------------------------

(defvar idlwave-shell-mode-map (copy-keymap comint-mode-map)
  "Keymap for `idlwave-mode'.")
(defvar idlwave-shell-electric-debug-mode-map (make-sparse-keymap))
(defvar idlwave-shell-mode-prefix-map (make-sparse-keymap))
(fset 'idlwave-shell-mode-prefix-map idlwave-shell-mode-prefix-map)
(defvar idlwave-mode-prefix-map (make-sparse-keymap))
(fset 'idlwave-mode-prefix-map idlwave-mode-prefix-map)

(defun idlwave-shell-define-key-both (key hook)
  "Define a key in both the shell and buffer mode maps."
  (define-key idlwave-mode-map key hook)
  (define-key idlwave-shell-mode-map key hook))

;(define-key idlwave-shell-mode-map "\M-?" 'comint-dynamic-list-completions)
;(define-key idlwave-shell-mode-map "\t" 'comint-dynamic-complete)

(define-key idlwave-shell-mode-map "\C-w"     'comint-kill-region)
(define-key idlwave-shell-mode-map "\t"       'idlwave-shell-complete)
(define-key idlwave-shell-mode-map "\M-\t"    'idlwave-shell-complete)
(define-key idlwave-shell-mode-map "\C-c\C-s" 'idlwave-shell)
(define-key idlwave-shell-mode-map "\C-c?"    'idlwave-routine-info)
(define-key idlwave-shell-mode-map "\C-g"     'idlwave-keyboard-quit)
(define-key idlwave-shell-mode-map "\M-?"     'idlwave-context-help)
(define-key idlwave-shell-mode-map [(control meta ?\?)]
  'idlwave-help-assistant-help-with-topic)
(define-key idlwave-shell-mode-map "\C-c\C-i" 'idlwave-update-routine-info)
(define-key idlwave-shell-mode-map "\C-c\C-y" 'idlwave-shell-char-mode-loop)
(define-key idlwave-shell-mode-map "\C-c\C-x" 'idlwave-shell-send-char)
(define-key idlwave-shell-mode-map "\C-c="    'idlwave-resolve)
(define-key idlwave-shell-mode-map "\C-c\C-v" 'idlwave-find-module)
(define-key idlwave-shell-mode-map "\C-c\C-k" 'idlwave-kill-autoloaded-buffers)
(define-key idlwave-shell-mode-map idlwave-shell-prefix-key
  'idlwave-shell-debug-map)
(define-key idlwave-shell-mode-map [(up)]  'idlwave-shell-up-or-history)
(define-key idlwave-shell-mode-map [(down)] 'idlwave-shell-down-or-history)
(define-key idlwave-mode-map "\C-c\C-y" 'idlwave-shell-char-mode-loop)
(define-key idlwave-mode-map "\C-c\C-x" 'idlwave-shell-send-char)

;; The mouse bindings for PRINT and HELP
(idlwave-shell-define-key-both
 (if (featurep 'xemacs)
     [(shift button2)]
   [(shift down-mouse-2)])
 'idlwave-shell-mouse-print)
(idlwave-shell-define-key-both
 (if (featurep 'xemacs)
     [(control meta button2)]
   [(control meta down-mouse-2)])
  'idlwave-shell-mouse-help)
(idlwave-shell-define-key-both
 (if (featurep 'xemacs)
     [(control shift button2)]
   [(control shift down-mouse-2)])
 'idlwave-shell-examine-select)
;; Add this one from the idlwave-mode-map
(define-key idlwave-shell-mode-map
  (if (featurep 'xemacs)
      [(shift button3)]
    [(shift mouse-3)])
  'idlwave-mouse-context-help)

;; For Emacs, we need to turn off the button release events.
(defun idlwave-shell-mouse-nop (event)
  (interactive "e"))
(unless (featurep 'xemacs)
  (idlwave-shell-define-key-both
   [(shift mouse-2)] 'idlwave-shell-mouse-nop)
  (idlwave-shell-define-key-both
   [(shift control mouse-2)] 'idlwave-shell-mouse-nop)
  (idlwave-shell-define-key-both
   [(control meta mouse-2)] 'idlwave-shell-mouse-nop))


;; The following set of bindings is used to bind the debugging keys.
;; If `idlwave-shell-activate-prefix-keybindings' is non-nil, the
;; first key in the list gets bound the C-c C-d prefix map.  If
;; `idlwave-shell-debug-modifiers' is non-nil, the second key in the
;; list gets bound with the specified modifiers in both
;; `idlwave-mode-map' and `idlwave-shell-mode-map'.  The next list
;; item, if non-nil, means to bind this as a single key in the
;; electric-debug-mode-map.
;;
;; [C-c C-d]-binding   debug-modifier-key command bind-electric-debug buf-only
;; Used keys:   abcdef hijklmnopqrstuvwxyz
;; Unused keys:       g
(let* ((specs
	'(([(control ?b)]   ?b   idlwave-shell-break-here t t)
	  ([(control ?i)]   ?i   idlwave-shell-break-in t t)
	  ([(control ?j)]   ?j   idlwave-shell-break-this-module t t)
	  ([(control ?d)]   ?d   idlwave-shell-clear-current-bp t)
	  ([(control ?a)]   ?a   idlwave-shell-clear-all-bp t)
	  ([(control ?\\)]  ?\\  idlwave-shell-toggle-enable-current-bp t)
	  ([(control ?s)]   ?s   idlwave-shell-step t)
	  ([(control ?n)]   ?n   idlwave-shell-stepover t)
	  ([(control ?k)]   ?k   idlwave-shell-skip t)
	  ([(control ?u)]   ?u   idlwave-shell-up t)
	  ([(control ?o)]   ?o   idlwave-shell-out t)
	  ([(control ?m)]   ?m   idlwave-shell-return t)
	  ([(control ?h)]   ?h   idlwave-shell-to-here t t)
	  ([(control ?r)]   ?r   idlwave-shell-cont t)
	  ([(control ?y)]   ?y   idlwave-shell-execute-default-command-line)
	  ([(control ?z)]   ?z   idlwave-shell-reset t)
	  ([(control ?q)]   ?q   idlwave-shell-quit)
	  ([(control ?p)]   ?p   idlwave-shell-print t)
	  ([(        ??)]   ??   idlwave-shell-help-expression t)
	  ([(control ?v)]   ?v   idlwave-shell-toggle-electric-debug-mode t t)
	  ([(control ?x)]   ?x   idlwave-shell-goto-next-error)
	  ([(control ?c)]   ?c   idlwave-shell-save-and-run t)
	  ([(        ?@)]   ?@   idlwave-shell-save-and-batch)
	  ([(control ?e)]   ?e   idlwave-shell-run-region)
	  ([(control ?w)]   ?w   idlwave-shell-resync-dirs)
	  ([(control ?l)]   ?l   idlwave-shell-redisplay t)
	  ([(control ?t)]   ?t   idlwave-shell-toggle-toolbar)
	  ([(control up)]   up   idlwave-shell-stack-up)
	  ([(control down)] down idlwave-shell-stack-down)
	  ([(        ?[)]   ?[   idlwave-shell-goto-previous-bp t t)
	  ([(        ?])]   ?]   idlwave-shell-goto-next-bp t t)
	  ([(control ?f)]   ?f   idlwave-shell-window)))
       (mod (cond ((and idlwave-shell-debug-modifiers
			(listp idlwave-shell-debug-modifiers)
			(not (equal '() idlwave-shell-debug-modifiers)))
		   idlwave-shell-debug-modifiers)
		  (idlwave-shell-activate-alt-keybindings
		   '(alt))))
       (shift (memq 'shift mod))
       (mod-noshift (delete 'shift (copy-sequence mod)))
       s k1 c2 k2 cmd electric only-buffer cannotshift)
  (while (setq s (pop specs))
    (setq k1  (nth 0 s)
	  c2  (nth 1 s)
	  cmd (nth 2 s)
	  electric (nth 3 s)
	  only-buffer (nth 4 s)
	  cannotshift (and shift (characterp c2) (eq c2 (upcase c2))))

    ;; The regular prefix keymap.
    (when (and idlwave-shell-activate-prefix-keybindings k1)
      (unless only-buffer
	(define-key idlwave-shell-mode-prefix-map k1 cmd))
      (define-key idlwave-mode-prefix-map k1 cmd))
    ;; The debug modifier map
    (when (and mod window-system)
      (if (char-or-string-p c2)
	  (setq k2 (vector (append mod-noshift
				   (list (if shift (upcase c2) c2)))))
	(setq k2 (vector (append mod (list c2)))))
      (unless cannotshift
	(define-key idlwave-mode-map k2 cmd)
	(unless only-buffer (define-key idlwave-shell-mode-map k2 cmd))))
    ;; The electric debug single-keystroke map
    (if (and electric (char-or-string-p c2))
	(define-key idlwave-shell-electric-debug-mode-map (char-to-string c2)
	  cmd))))

;; A few extras in the electric debug map
(define-key idlwave-shell-electric-debug-mode-map " " 'idlwave-shell-step)
(define-key idlwave-shell-electric-debug-mode-map "+" 'idlwave-shell-stack-up)
(define-key idlwave-shell-electric-debug-mode-map "=" 'idlwave-shell-stack-up)
(define-key idlwave-shell-electric-debug-mode-map "-"
  'idlwave-shell-stack-down)
(define-key idlwave-shell-electric-debug-mode-map "_"
  'idlwave-shell-stack-down)
(define-key idlwave-shell-electric-debug-mode-map "e"
  (lambda () (interactive) (idlwave-shell-print '(16))))
(define-key idlwave-shell-electric-debug-mode-map "q" 'idlwave-shell-retall)
(define-key idlwave-shell-electric-debug-mode-map "t"
  (lambda () (interactive) (idlwave-shell-send-command "help,/TRACE")))
(define-key idlwave-shell-electric-debug-mode-map [(control ??)]
  'idlwave-shell-electric-debug-help)
(define-key idlwave-shell-electric-debug-mode-map "x"
  (lambda (arg) (interactive "P")
    (idlwave-shell-print arg nil nil t)))


; Enter the prefix map in two places.
(fset 'idlwave-debug-map       idlwave-mode-prefix-map)
(fset 'idlwave-shell-debug-map idlwave-shell-mode-prefix-map)

;; The Electric Debug Minor Mode --------------------------------------------

(defun idlwave-shell-toggle-electric-debug-mode ()
  "Toggle electric-debug-mode, suppressing re-entry into mode if turned off."
  (interactive)
  ;; If turning it off, make sure it stays off throughout the debug
  ;; session until we return or hit $MAIN$.  Cancel this suppression
  ;; if it's explicitly turned on.
  (if idlwave-shell-electric-debug-mode
      (progn ;; Turn it off, and make sure it stays off.
	(setq idlwave-shell-suppress-electric-debug t)
	(idlwave-shell-electric-debug-mode 0))
    (setq idlwave-shell-suppress-electric-debug nil)
    (idlwave-shell-electric-debug-mode t)))

(defvar idlwave-shell-electric-debug-read-only)
(defvar idlwave-shell-electric-debug-buffers nil)

(define-minor-mode idlwave-shell-electric-debug-mode
  "Toggle Idlwave Shell Electric Debug mode.
With a prefix argument ARG, enable the mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable the mode
if ARG is omitted or nil.

When Idlwave Shell Electric Debug mode is enabled, the Idlwave
Shell debugging commands are available as single key sequences."
  nil " *Debugging*" idlwave-shell-electric-debug-mode-map)

(add-hook
 'idlwave-shell-electric-debug-mode-on-hook
 (lambda ()
   (set (make-local-variable 'idlwave-shell-electric-debug-read-only)
	buffer-read-only)
   (setq buffer-read-only t)
   (add-to-list 'idlwave-shell-electric-debug-buffers (current-buffer))
   (if idlwave-shell-stop-line-overlay
       (overlay-put idlwave-shell-stop-line-overlay 'face
		    idlwave-shell-electric-stop-line-face))
   (if (facep 'fringe)
       (set-face-foreground 'fringe idlwave-shell-electric-stop-color
			    (selected-frame)))))

(add-hook
 'idlwave-shell-electric-debug-mode-off-hook
 (lambda ()
   ;; Return to previous read-only state
   (setq buffer-read-only (if (boundp 'idlwave-shell-electric-debug-read-only)
			      idlwave-shell-electric-debug-read-only))
   (setq idlwave-shell-electric-debug-buffers
	 (delq (current-buffer) idlwave-shell-electric-debug-buffers))
   (if idlwave-shell-stop-line-overlay
       (overlay-put idlwave-shell-stop-line-overlay 'face
		    idlwave-shell-stop-line-face)
     (if (facep 'fringe)
	 (set-face-foreground 'fringe (face-foreground 'default))))))

;; easy-mmode defines electric-debug-mode for us, so we need to advise it.
(defadvice idlwave-shell-electric-debug-mode (after print-enter activate)
  "Print out an entrance message."
  (when idlwave-shell-electric-debug-mode
    (message
     "Electric Debugging mode entered.  Press [C-?] for help, [q] to quit"))
  (force-mode-line-update))

;; Turn it off in all relevant buffers
(defvar idlwave-shell-electric-debug-buffers nil)
(defun idlwave-shell-electric-debug-all-off ()
  (setq idlwave-shell-suppress-electric-debug nil)
  (let ((buffers idlwave-shell-electric-debug-buffers)
	buf)
    (save-excursion
      (while (setq buf (pop buffers))
	(when (buffer-live-p buf)
	  (set-buffer buf)
	  (when (and (derived-mode-p 'idlwave-mode)
		     buffer-file-name
		     idlwave-shell-electric-debug-mode)
	    (idlwave-shell-electric-debug-mode 0))))))
  (setq idlwave-shell-electric-debug-buffers nil))

;; Show the help text
(defun idlwave-shell-electric-debug-help ()
  (interactive)
  (with-output-to-temp-buffer "*IDLWAVE Electric Debug Help*"
    (princ idlwave-shell-electric-debug-help))
  (let* ((current-window (selected-window))
	 (window (get-buffer-window "*IDLWAVE Electric Debug Help*"))
	 (window-lines (window-height window)))
    (select-window window)
    (enlarge-window (1+ (- (count-lines 1 (point-max)) window-lines)))
    (select-window current-window)))


;; The Menus --------------------------------------------------------------
(defvar idlwave-shell-menu-def
  `("Debug"
    ["Electric Debug Mode"
     idlwave-shell-electric-debug-mode
     :style toggle :selected idlwave-shell-electric-debug-mode
     :included (derived-mode-p 'idlwave-mode) :keys "C-c C-d C-v"]
    "--"
    ("Compile & Run"
     ["Save and .RUN" idlwave-shell-save-and-run
      (or (derived-mode-p 'idlwave-mode)
	  idlwave-shell-last-save-and-action-file)]
     ["Save and .COMPILE" idlwave-shell-save-and-compile
      (or (derived-mode-p 'idlwave-mode)
	  idlwave-shell-last-save-and-action-file)]
     ["Save and @Batch" idlwave-shell-save-and-batch
      (or (derived-mode-p 'idlwave-mode)
	  idlwave-shell-last-save-and-action-file)]
     "--"
     ["Goto Next Error" idlwave-shell-goto-next-error t]
     "--"
     ["Compile and Run Region" idlwave-shell-run-region
      (derived-mode-p 'idlwave-mode)]
     ["Evaluate Region" idlwave-shell-evaluate-region
      (derived-mode-p 'idlwave-mode)]
     "--"
     ["Execute Default Cmd" idlwave-shell-execute-default-command-line t]
     ["Edit Default Cmd" idlwave-shell-edit-default-command-line t])
    ("Breakpoints"
     ["Set Breakpoint" idlwave-shell-break-here
      :keys "C-c C-d C-b" :active (derived-mode-p 'idlwave-mode)]
     ("Set Special Breakpoint"
      ["Set After Count Breakpoint"
       (progn
	(let ((count (string-to-number (read-string "Break after count: "))))
	      (if (integerp count) (idlwave-shell-break-here count))))
       :active (derived-mode-p 'idlwave-mode)]
      ["Set Condition Breakpoint"
       (idlwave-shell-break-here '(4))
       :active (derived-mode-p 'idlwave-mode)])
     ["Break in Module" idlwave-shell-break-in
      :keys "C-c C-d C-i" :active (derived-mode-p 'idlwave-mode)]
     ["Break in this Module" idlwave-shell-break-this-module
      :keys "C-c C-d C-j" :active (derived-mode-p 'idlwave-mode)]
     ["Clear Breakpoint" idlwave-shell-clear-current-bp t]
     ["Clear All Breakpoints" idlwave-shell-clear-all-bp t]
     ["Disable/Enable Breakpoint" idlwave-shell-toggle-enable-current-bp t]
     ["Goto Previous Breakpoint" idlwave-shell-goto-previous-bp
      :keys "C-c C-d [" :active (derived-mode-p 'idlwave-mode)]
     ["Goto Next Breakpoint" idlwave-shell-goto-next-bp
      :keys "C-c C-d ]" :active (derived-mode-p 'idlwave-mode)]
     ["List All Breakpoints" idlwave-shell-list-all-bp t]
     ["Resync Breakpoints" idlwave-shell-bp-query t])
    ("Continue/Step"
     ["Step (into)" idlwave-shell-step t]
     ["Step (over)" idlwave-shell-stepover t]
     ["Skip One Statement" idlwave-shell-skip t]
     ["Continue" idlwave-shell-cont t]
     ["... to End of Block" idlwave-shell-up t]
     ["... to End of Subprog" idlwave-shell-return t]
     ["... to End of Subprog+1" idlwave-shell-out t]
     ["... to Here (Cursor Line)" idlwave-shell-to-here
      :keys "C-c C-d C-h" :active (derived-mode-p 'idlwave-mode)])
    ("Examine Expressions"
     ["Print expression" idlwave-shell-print t]
     ["Help on expression" idlwave-shell-help-expression t]
     ("Examine nearby expression with"
      ,@(mapcar (lambda(x)
		  `[ ,(car x) (idlwave-shell-print nil ',x) t ])
		idlwave-shell-examine-alist))
     ("Examine region with"
      ,@(mapcar (lambda(x)
		  `[ ,(car x) (idlwave-shell-print '(4) ',x) t ])
		idlwave-shell-examine-alist)))
    ("Call Stack"
     ["Stack Up" idlwave-shell-stack-up t]
     ["Stack Down" idlwave-shell-stack-down t]
     "--"
     ["Redisplay and Sync" idlwave-shell-redisplay t])
    ("Show Commands"
     ["Everything" (if (eq idlwave-shell-show-commands 'everything)
		       (progn
			 (setq idlwave-shell-show-commands
			       (get 'idlwave-shell-show-commands 'last-val))
			 (put 'idlwave-shell-show-commands 'last-val nil))
		     (put 'idlwave-shell-show-commands 'last-val
			  idlwave-shell-show-commands)
		     (setq idlwave-shell-show-commands 'everything))
      :style toggle :selected (and (not (listp idlwave-shell-show-commands))
				   (eq idlwave-shell-show-commands
				       'everything))]
     "--"
     ["Compiling Commands" (idlwave-shell-add-or-remove-show 'run)
      :style toggle
      :selected (not (idlwave-shell-hide-p
		      'run
		      (get 'idlwave-shell-show-commands 'last-val)))
      :active (not (eq idlwave-shell-show-commands 'everything))]
     ["Breakpoint Commands" (idlwave-shell-add-or-remove-show 'breakpoint)
      :style toggle
      :selected (not (idlwave-shell-hide-p
		      'breakpoint
		      (get 'idlwave-shell-show-commands 'last-val)))
      :active (not (eq idlwave-shell-show-commands 'everything))]
     ["Debug Commands" (idlwave-shell-add-or-remove-show 'debug)
      :style toggle
      :selected (not (idlwave-shell-hide-p
		      'debug
		      (get 'idlwave-shell-show-commands 'last-val)))
      :active (not (eq idlwave-shell-show-commands 'everything))]
     ["Miscellaneous Commands" (idlwave-shell-add-or-remove-show 'misc)
      :style toggle
      :selected (not (idlwave-shell-hide-p
		      'misc
		      (get 'idlwave-shell-show-commands 'last-val)))
      :active (not (eq idlwave-shell-show-commands 'everything))])
    ("Input Mode"
     ["Send one char" idlwave-shell-send-char t]
     ["Temporary Character Mode" idlwave-shell-char-mode-loop t]
     "--"
     ["Use Input Mode Magic"
      (setq idlwave-shell-use-input-mode-magic
	    (not idlwave-shell-use-input-mode-magic))
      :style toggle :selected idlwave-shell-use-input-mode-magic])
    "--"
    ["Update Working Dir" idlwave-shell-resync-dirs t]
    ["Save Path Info"
     (idlwave-shell-send-command idlwave-shell-path-query
				 'idlwave-shell-get-path-info
				 'hide)
     t]
    ["Reset IDL" idlwave-shell-reset t]
    "--"
    ["Toggle Toolbar" idlwave-shell-toggle-toolbar t]
    ["Exit IDL" idlwave-shell-quit t]))

(if (or (featurep 'easymenu) (load "easymenu" t))
    (progn
      (easy-menu-define
       idlwave-mode-debug-menu idlwave-mode-map "IDL debugging menus"
       idlwave-shell-menu-def)
      (easy-menu-define
       idlwave-shell-mode-menu idlwave-shell-mode-map "IDL shell menus"
       idlwave-shell-menu-def)
      (save-current-buffer
	(dolist (buf (buffer-list))
          (set-buffer buf)
          (if (derived-mode-p 'idlwave-mode)
              (progn
                (easy-menu-remove idlwave-mode-debug-menu)
                (easy-menu-add idlwave-mode-debug-menu)))))))

;; The Breakpoint Glyph -------------------------------------------------------

(defvar idlwave-shell-bp-glyph nil
  "The glyphs to mark breakpoint lines in the source code.")

(let ((image-alist
       '((bp . "/* XPM */
static char * file[] = {
\"14 12 3 1\",
\" 	c None s backgroundColor\",
\".	c #4B4B4B4B4B4B\",
\"R	c #FFFF00000000\",
\"              \",
\"     ....     \",
\"    .RRRR.    \",
\"   .RRRRRR.   \",
\"  .RRRRRRRR.  \",
\"  .RRRRRRRR.  \",
\"  .RRRRRRRR.  \",
\"  .RRRRRRRR.  \",
\"   .RRRRRR.   \",
\"    .RRRR.    \",
\"     ....     \",
\"              \"};")
	 (bp-cond . "/* XPM */
static char * file[] = {
\"14 12 4 1\",
\" 	c None s backgroundColor\",
\".	c #4B4B4B4B4B4B\",
\"R	c #FFFF00000000\",
\"B     c #000000000000\",
\"              \",
\"     ....     \",
\"    .RRRR.    \",
\"   .RRRRRR.   \",
\"  .RRRRRRRR.  \",
\"  .RRBBBBRR.  \",
\"  .RRRRRRRR.  \",
\"  .RRBBBBRR.  \",
\"   .RRRRRR.   \",
\"    .RRRR.    \",
\"     ....     \",
\"              \"};")
	 (bp-1 . "/* XPM */
static char * file[] = {
\"14 12 4 1\",
\" 	c None s backgroundColor\",
\".     c #4B4B4B4B4B4B\",
\"X     c #FFFF00000000\",
\"o     c #000000000000\",
\"              \",
\"     ....     \",
\"    .XXXX.    \",
\"   .XXooXX.   \",
\"  .XXoooXXX.  \",
\"  .XXXooXXX.  \",
\"  .XXXooXXX.  \",
\"  .XXooooXX.  \",
\"   .XooooX.   \",
\"    .XXXX.    \",
\"     ....     \",
\"              \"};")
	 (bp-2 . "/* XPM */
static char * file[] = {
\"14 12 4 1\",
\" 	c None s backgroundColor\",
\".     c #4B4B4B4B4B4B\",
\"X     c #FFFF00000000\",
\"o     c #000000000000\",
\"              \",
\"     ....     \",
\"    .XXXX.    \",
\"   .XoooXX.   \",
\"  .XXoXooXX.  \",
\"  .XXXXooXX.  \",
\"  .XXXooXXX.  \",
\"  .XXooXXXX.  \",
\"   .XooooX.   \",
\"    .XXXX.    \",
\"     ....     \",
\"              \"};")
	 (bp-3 . "/* XPM */
static char * file[] = {
\"14 12 4 1\",
\" 	c None s backgroundColor\",
\".     c #4B4B4B4B4B4B\",
\"X     c #FFFF00000000\",
\"o     c #000000000000\",
\"              \",
\"     ....     \",
\"    .XXXX.    \",
\"   .XoooXX.   \",
\"  .XXXXooXX.  \",
\"  .XXXooXXX.  \",
\"  .XXXXooXX.  \",
\"  .XXoXooXX.  \",
\"   .XoooXX.   \",
\"    .XXXX.    \",
\"     ....     \",
\"              \"};")
	 (bp-4 . "/* XPM */
static char * file[] = {
\"14 12 4 1\",
\" 	c None s backgroundColor\",
\".     c #4B4B4B4B4B4B\",
\"X     c #FFFF00000000\",
\"o     c #000000000000\",
\"              \",
\"     ....     \",
\"    .XXXX.    \",
\"   .XoXXoX.   \",
\"  .XXoXXoXX.  \",
\"  .XXooooXX.  \",
\"  .XXXXooXX.  \",
\"  .XXXXooXX.  \",
\"   .XXXooX.   \",
\"    .XXXX.    \",
\"     ....     \",
\"              \"};")
	 (bp-n . "/* XPM */
static char * file[] = {
\"14 12 4 1\",
\" 	c None s backgroundColor\",
\".     c #4B4B4B4B4B4B\",
\"X     c #FFFF00000000\",
\"o     c #000000000000\",
\"              \",
\"     ....     \",
\"    .XXXX.    \",
\"   .XXXXXX.   \",
\"  .XXoXoXXX.  \",
\"  .XXooXoXX.  \",
\"  .XXoXXoXX.  \",
\"  .XXoXXoXX.  \",
\"   .XoXXoX.   \",
\"    .XXXX.    \",
\"     ....     \",
\"              \"};"))) im-cons im)

  (while (setq im-cons (pop image-alist))
    (setq im (cond ((and (featurep 'xemacs)
			 (featurep 'xpm))
		    (list
		     (let ((data (cdr im-cons)))
		       (string-match "#FFFF00000000" data)
		       (setq data (replace-match "#8F8F8F8F8F8F" t t data))
		       (make-glyph data))
		     (make-glyph (cdr im-cons))))
		   ((and (not (featurep 'xemacs))
			 (fboundp 'image-type-available-p)
			 (image-type-available-p 'xpm))
		    (list 'image :type 'xpm :data (cdr im-cons)
			  :ascent 'center))
		   (t nil)))
    (if im (push (cons (car im-cons) im) idlwave-shell-bp-glyph))))

(provide 'idlw-shell)
(provide 'idlwave-shell)

;; Load the toolbar when wanted by the user.

(autoload 'idlwave-toolbar-toggle "idlw-toolbar"
  "Toggle the IDLWAVE toolbar.")
(autoload 'idlwave-toolbar-add-everywhere "idlw-toolbar"
  "Add IDLWAVE toolbar.")
(defun idlwave-shell-toggle-toolbar ()
  "Toggle the display of the debugging toolbar."
  (interactive)
  (idlwave-toolbar-toggle))

(if idlwave-shell-use-toolbar
    (add-hook 'idlwave-shell-mode-hook 'idlwave-toolbar-add-everywhere))

;;; idlw-shell.el ends here
