;;; esh-groups.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "em-alias" "em-alias.el" "528ef07a01e2685381211d7f1257675b")
;;; Generated autoloads from em-alias.el

(eshell-defgroup eshell-alias nil "Command aliases allow for easy definition of alternate commands." :tag "Command aliases" :group 'eshell-module)

;;;***

;;;### (autoloads nil "em-banner" "em-banner.el" "e39b9295d7cc8f1e219e6eabb9f8b3f4")
;;; Generated autoloads from em-banner.el

(eshell-defgroup eshell-banner nil "This sample module displays a welcome banner at login.\nIt exists so that others wishing to create their own Eshell extension\nmodules may have a simple template to begin with." :tag "Login banner" :group 'eshell-module)

;;;***

;;;### (autoloads nil "em-basic" "em-basic.el" "5f294612104409dc80d0a1689d8eb353")
;;; Generated autoloads from em-basic.el

(eshell-defgroup eshell-basic nil "The \"basic\" code provides a set of convenience functions which\nare traditionally considered shell builtins.  Since all of the\nfunctionality provided by them is accessible through Lisp, they are\nnot really builtins at all, but offer a command-oriented way to do the\nsame thing." :tag "Basic shell commands" :group 'eshell-module)

;;;***

;;;### (autoloads nil "em-cmpl" "em-cmpl.el" "95a428650ff03f39029ef94b85b7f35e")
;;; Generated autoloads from em-cmpl.el

(eshell-defgroup eshell-cmpl nil "This module provides a programmable completion function bound to\nthe TAB key, which allows for completing command names, file names,\nvariable names, arguments, etc." :tag "Argument completion" :group 'eshell-module)

;;;***

;;;### (autoloads nil "em-dirs" "em-dirs.el" "2faa4ad5789c597d5e2ad2ad1071ffcc")
;;; Generated autoloads from em-dirs.el

(eshell-defgroup eshell-dirs nil "Directory navigation involves changing directories, examining the\ncurrent directory, maintaining a directory stack, and also keeping\ntrack of a history of the last directory locations the user was in.\nEmacs does provide standard Lisp definitions of `pwd' and `cd', but\nthey lack somewhat in feel from the typical shell equivalents." :tag "Directory navigation" :group 'eshell-module)

;;;***

;;;### (autoloads nil "em-glob" "em-glob.el" "f804156bdf5dd2f4ea5b917dee223e51")
;;; Generated autoloads from em-glob.el

(eshell-defgroup eshell-glob nil "This module provides extended globbing syntax, similar what is used\nby zsh for filename generation." :tag "Extended filename globbing" :group 'eshell-module)

;;;***

;;;### (autoloads nil "em-hist" "em-hist.el" "e4f11e8e6c419b28a611e7cbaba4ed85")
;;; Generated autoloads from em-hist.el

(eshell-defgroup eshell-hist nil "This module provides command history management." :tag "History list management" :group 'eshell-module)

;;;***

;;;### (autoloads nil "em-ls" "em-ls.el" "47e9d1160e3d679f37e4fbc85e2ee407")
;;; Generated autoloads from em-ls.el

(eshell-defgroup eshell-ls nil "This module implements the \"ls\" utility fully in Lisp.  If it is\npassed any unrecognized command switches, it will revert to the\noperating system's version.  This version of \"ls\" uses text\nproperties to colorize its output based on the setting of\n`eshell-ls-use-colors'." :tag "Implementation of `ls' in Lisp" :group 'eshell-module)

;;;***

;;;### (autoloads nil "em-pred" "em-pred.el" "8284bec4c30c320d33248cf2ff446583")
;;; Generated autoloads from em-pred.el

(eshell-defgroup eshell-pred nil "This module allows for predicates to be applied to globbing\npatterns (similar to zsh), in addition to string modifiers which can\nbe applied either to globbing results, variable references, or just\nordinary strings." :tag "Value modifiers and predicates" :group 'eshell-module)

;;;***

;;;### (autoloads nil "em-prompt" "em-prompt.el" "eb19a6879c2f5d599109f60fede64c43")
;;; Generated autoloads from em-prompt.el

(eshell-defgroup eshell-prompt nil "This module provides command prompts, and navigation between them,\nas is common with most shells." :tag "Command prompts" :group 'eshell-module)

;;;***

;;;### (autoloads nil "em-rebind" "em-rebind.el" "14311986e52a3d2885da4f599e3e88b9")
;;; Generated autoloads from em-rebind.el

(eshell-defgroup eshell-rebind nil "This module allows for special keybindings that only take effect\nwhile the point is in a region of input text.  By default, it binds\nC-a to move to the beginning of the input text (rather than just the\nbeginning of the line), and C-p and C-n to move through the input\nhistory, C-u kills the current input text, etc.  It also, if\n`eshell-confine-point-to-input' is non-nil, does not allow certain\ncommands to cause the point to leave the input area, such as\n`backward-word', `previous-line', etc.  This module intends to mimic\nthe behavior of normal shells while the user editing new input text." :tag "Rebind keys at input" :group 'eshell-module)

;;;***

;;;### (autoloads nil "em-script" "em-script.el" "e4fe8a03256c51e1cd760c47d4c0e6c1")
;;; Generated autoloads from em-script.el

(eshell-defgroup eshell-script nil "This module allows for the execution of files containing Eshell\ncommands, as a script file." :tag "Running script files." :group 'eshell-module)

;;;***

;;;### (autoloads nil "em-smart" "em-smart.el" "6f16d4f127f8a7b3686bf987d7cef2cf")
;;; Generated autoloads from em-smart.el

(eshell-defgroup eshell-smart nil "This module combines the facility of normal, modern shells with\nsome of the edit/review concepts inherent in the design of Plan 9's\n9term.  See the docs for more details.\n\nMost likely you will have to turn this option on and play around with\nit to get a real sense of how it works." :tag "Smart display of output" :group 'eshell-module)

;;;***

;;;### (autoloads nil "em-term" "em-term.el" "d4bb688e97f26c9091ec2ee05516da7f")
;;; Generated autoloads from em-term.el

(eshell-defgroup eshell-term nil "This module causes visual commands (e.g., 'vi') to be executed by\nthe `term' package, which comes with Emacs.  This package handles most\nof the ANSI control codes, allowing curses-based applications to run\nwithin an Emacs window.  The variable `eshell-visual-commands' defines\nwhich commands are considered visual in nature." :tag "Running visual commands" :group 'eshell-module)

;;;***

;;;### (autoloads nil "em-unix" "em-unix.el" "bcd545babfa601376b1d2bc000130521")
;;; Generated autoloads from em-unix.el

(eshell-defgroup eshell-unix nil "This module defines many of the more common UNIX utilities as\naliases implemented in Lisp.  These include mv, ln, cp, rm, etc.  If\nthe user passes arguments which are too complex, or are unrecognized\nby the Lisp variant, the external version will be called (if\navailable).  The only reason not to use them would be because they are\nusually much slower.  But in several cases their tight integration\nwith Eshell makes them more versatile than their traditional cousins\n(such as being able to use `kill' to kill Eshell background processes\nby name)." :tag "UNIX commands in Lisp" :group 'eshell-module)

;;;***

;;;### (autoloads nil "em-xtra" "em-xtra.el" "9834b70faac2a0d51172828cf48480eb")
;;; Generated autoloads from em-xtra.el

(eshell-defgroup eshell-xtra nil "This module defines some extra alias functions which are entirely\noptional.  They can be viewed as samples for how to write Eshell alias\nfunctions, or as aliases which make some of Emacs's behavior more\nnaturally accessible within Emacs." :tag "Extra alias functions" :group 'eshell-module)

;;;***

(provide 'esh-groups)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; esh-groups.el ends here
