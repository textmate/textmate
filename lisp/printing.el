;;; printing.el --- printing utilities

;; Copyright (C) 2000-2001, 2003-2012  Free Software Foundation, Inc.

;; Author: Vinicius Jose Latorre <viniciusjl@ig.com.br>
;; Maintainer: Vinicius Jose Latorre <viniciusjl@ig.com.br>
;; Keywords: wp, print, PostScript
;; Version: 6.9.3
;; X-URL: http://www.emacswiki.org/cgi-bin/wiki/ViniciusJoseLatorre

(defconst pr-version "6.9.3"
  "printing.el, v 6.9.3 <2007/12/09 vinicius>

Please send all bug fixes and enhancements to
	Vinicius Jose Latorre <viniciusjl@ig.com.br>
")

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

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Introduction
;; ------------
;;
;; With `printing' you can preview or print a PostScript file.  You can also
;; print a text file using PostScript, and preview or print buffers that use
;; certain special modes like mh-folder-mode, rmail-summary-mode,
;; gnus-summary-mode, etc.  This package also includes a PostScript/text
;; printer database.
;;
;; There are two user interfaces:
;;
;;    * Menu interface:
;;      The `printing' menu replaces the usual print options in the menu bar.
;;      This is the default user interface.
;;
;;    * Buffer interface:
;;      You can use a buffer interface instead of menus.  It looks like a
;;      customization buffer.  Basically, it has the same options found in the
;;      menu and some extra options, all this on a buffer.
;;
;; `printing' is prepared to run on GNU, Unix and NT systems.
;; On GNU or Unix system, `printing' depends on gs and gv utilities.
;; On NT system, `printing' depends on gstools (gswin32.exe and gsview32.exe).
;; To obtain ghostscript, ghostview and GSview see the URL
;; `http://www.gnu.org/software/ghostscript/ghostscript.html'.
;;
;; `printing' depends on ps-print package to generate PostScript files, to
;; spool and to despool PostScript buffer.  So, `printing' provides an
;; interface to ps-print package and it also provides some extra stuff.
;;
;; To download the latest ps-print package see
;; `http://www.emacswiki.org/cgi-bin/wiki/PsPrintPackage'.
;; Please, see README file for ps-print installation instructions.
;;
;; `printing' was inspired by:
;;
;;    print-nt.el	      Frederic Corne <frederic.corne@erli.fr>
;;	 Special printing functions for Windows NT
;;
;;    mh-e-init.el	      Tom Vogels <tov@ece.cmu.edu>
;;	 PS-print for mail messages
;;
;;    win32-ps-print.el	      Matthew O. Persico <mpersico@erols.com>
;;	 PostScript printing with ghostscript
;;
;;    ps-print-interface.el   Volker Franz <volker.franz@tuebingen.mpg.de>
;;	 Graphical front end for ps-print and previewing
;;
;;
;; Log Messages
;; ------------
;;
;; The buffer *Printing Command Output* is where the `printing' log messages
;; are inserted.  All program called by `printing' has a log entry in the
;; buffer *Printing Command Output*.  A log entry has the following form:
;;
;;    PROGRAM (ARG...)
;;    MESSAGE
;;    Exit status: CODE
;;
;; Where
;; PROGRAM is the program activated by `printing',
;; ARG is an argument passed to PROGRAM (it can have more than one argument),
;; MESSAGE is an error message returned by PROGRAM (it can have no message, if
;; PROGRAM is successful),
;; and CODE is a numeric exit status or a signal description string.
;;
;; For example, after previewing a PostScript file, *Printing Command Output*
;; will have the following entry:
;;
;;    /usr/X11R6/bin/gv ("/home/user/example/file.ps")
;;    Exit status: 0
;;
;; In the example above, the previewing was successful.  If during previewing,
;; you quit gv execution (by typing C-g during Emacs session), the log entry
;; would be:
;;
;;    /usr/X11R6/bin/gv ("/home/user/example/file.ps")
;;    Exit status: Quit
;;
;; So, if something goes wrong, a good place to take a look is the buffer
;; *Printing Command Output*.  Don't forget to see also the buffer *Messages*,
;; it can help.
;;
;;
;; Novices (First Users)
;; ---------------------
;;
;; First of all, see printing documentation only to get an idea of what
;; `printing' is capable.
;;
;; Then try to set the variables: `pr-ps-name', `pr-ps-printer-alist',
;; `pr-txt-name', `pr-txt-printer-alist' and `pr-path-alist'.  These variables
;; are the main variables for printing processing.
;;
;; Now, please, see these variables documentation deeper.  You can do this by
;; typing C-h v pr-ps-name RET (for example) if you already loaded printing
;; package, or by browsing printing.el source file.
;;
;; If the documentation isn't clear or if you find a way to improve the
;; documentation, please, send an email to maintainer.  All printing users
;; will thank you.
;;
;; One way to set variables is by calling `pr-customize', customize all
;; variables and save the customization by future sessions (see Options
;; section).  Other way is by coding your settings on Emacs init file (that is,
;; ~/.emacs file), see below for a first setting template that it should be
;; inserted on your ~/.emacs file (or c:/_emacs, if you're using Windows 9x/NT
;; or MS-DOS):
;;
;; * Example of setting for Windows system:
;;
;;    (require 'printing)		; load printing package
;;    (setq pr-path-alist
;;	    '((windows   "c:/applications/executables" PATH ghostview mpage)
;;	      (ghostview "c:/gs/gsview-dir")
;;	      (mpage     "c:/mpage-dir")
;;	      ))
;;    (setq pr-txt-name      'prt_06a)
;;    (setq pr-txt-printer-alist
;;          '((prt_06a  "print"     nil "/D:\\\\printers\\prt_06a")
;;            (prt_07c  nil         nil "/D:\\\\printers\\prt_07c")
;;            (PRN      ""          nil "PRN")
;;            (standard "redpr.exe" nil "")
;;            ))
;;    (setq pr-ps-name       'lps_06b)
;;    (setq pr-ps-printer-alist
;;          '((lps_06a  "print"     nil "/D:" "\\\\printers\\lps_06a")
;;            (lps_06b  "print"     nil nil   "\\\\printers\\lps_06b")
;;            (lps_07c  "print"     nil ""    "/D:\\\\printers\\lps_07c")
;;            (lps_08c  nil         nil nil   "\\\\printers\\lps_08c")
;;            (LPT1     ""          nil ""    "LPT1:")
;;            (PRN      ""          nil ""    "PRN")
;;            (standard "redpr.exe" nil ""    "")
;;            ))
;;    (pr-update-menus t)		; update now printer and utility menus
;;
;; * Example of setting for GNU or Unix system:
;;
;;    (require 'printing)		; load printing package
;;    (setq pr-path-alist
;;	    '((unix      "." "~/bin" ghostview mpage PATH)
;;	      (ghostview "$HOME/bin/gsview-dir")
;;	      (mpage     "$HOME/bin/mpage-dir")
;;	      ))
;;    (setq pr-txt-name      'prt_06a)
;;    (setq pr-txt-printer-alist
;;          '((prt_06a "lpr" nil "prt_06a")
;;            (prt_07c nil   nil "prt_07c")
;;            ))
;;    (setq pr-ps-name       'lps_06b)
;;    (setq pr-ps-printer-alist
;;          '((lps_06b "lpr" nil "-P" "lps_06b")
;;            (lps_07c "lpr" nil nil  "lps_07c")
;;            (lps_08c nil   nil nil  "lps_08c")
;;            ))
;;    (pr-update-menus t)		; update now printer and utility menus
;;
;;
;; NOTE 1: Don't forget to download and install ghostscript utilities (see
;;         Utilities section).
;;
;; NOTE 2: The `printer-name' and `ps-printer-name' variables don't need to be
;;         set, as they are implicit set by `pr-ps-printer-alist' and
;;         `pr-txt-printer-alist'.
;;
;; NOTE 3: The duplex feature will only work on PostScript printers that
;;         support this feature.
;;         You can check if your PostScript printer supports duplex feature
;;         by checking the printer manual.  Or you can try these steps:
;;         1. Open a buffer (or use the *scratch* buffer).
;;         2. Type:
;;            First line (on first page)
;;            ^L
;;            Second line (on second page)
;;         3. Print this buffer with duplex turned on.
;;         If it's printed 2 (two) sheets of paper, then your PostScript
;;         printer doesn't have duplex feature; otherwise, it's ok, your
;;         printer does have duplex feature.
;;
;; NOTE 4: See Tips section.
;;
;;
;; Tips
;; ----
;;
;; 1. If you have a local printer, that is, a printer which is connected
;;    directly to your computer, don't forget to connect the printer to your
;;    computer before printing.
;;
;; 2. If you try to print a file and it seems that the file was printed, but
;;    there is no paper in the printer, then try to set `pr-delete-temp-file'
;;    to nil.  Probably `printing' is deleting the temporary file before your
;;    local system can get it to send to the printer.
;;
;; 3. Don't try to print a dynamic buffer, that is, a buffer which is
;;    modifying while `printing' tries to print.  Eventually you got an error
;;    message.  Instead, save the dynamic buffer to a file or copy it in
;;    another buffer and, then, print the file or the new static buffer.
;;    An example of dynamic buffer is the *Messages* buffer.
;;
;; 4. When running Emacs on Windows (with or without cygwin), check if your
;;    printer is a text printer or not by typing in a DOS window:
;;
;;       print /D:\\host\printer somefile.txt
;;
;;    Where, `host' is the machine where the printer is directly connected,
;;    `printer' is the printer name and `somefile.txt' is a text file.
;;
;;    If the printer `\\host\printer' doesn't print the content of
;;    `somefile.txt' or, instead, it returns the following message:
;;
;;       PostScript Error Handler
;;       Offending Command = CCC
;;       Stack =
;;
;;    Where `CCC' is whatever is at the beginning of the text to be printed.
;;
;;    Therefore, the printer `\\host\printer' is not a text printer, but a
;;    PostScript printer.  So, please, don't include this printer in
;;    `pr-txt-printer-alist' (which see).
;;
;; 5. You can use gsprint instead of ghostscript to print monochrome PostScript
;;    files in Windows.  The gsprint utility documentation says that it is more
;;    efficient than ghostscript to print monochrome PostScript.
;;
;;    To print non-monochrome PostScript file, the efficiency of ghostscript
;;    is similar to gsprint.
;;
;;    Also the gsprint utility comes together with gsview distribution.
;;
;;    For more information about gsprint see
;;    `http://www.cs.wisc.edu/~ghost/gsview/gsprint.htm'.
;;
;;    As an example of gsprint declaration:
;;
;;    (setq pr-ps-printer-alist
;;          '((A "gsprint" ("-all" "-twoup") "-printer " "my-b/w-printer-name")
;;            (B "gsprint" ("-all" "-twoup") nil "-printer my-b/w-printer-name")
;;            ;; some other printer declaration
;;            ))
;;
;;    The example above declares that printer A prints all pages (-all) and two
;;    pages per sheet (-twoup).  The printer B declaration does the same as the
;;    printer A declaration, the only difference is the printer name selection.
;;
;;    There are other command line options like:
;;
;;    -mono	Render in monochrome as 1bit/pixel (only black and white).
;;    -grey	Render in greyscale as 8bits/pixel.
;;    -color	Render in color as 24bits/pixel.
;;
;;    The default is `-mono'.  So, printer A and B in the example above are
;;    using implicitly the `-mono' option.  Note that in `-mono' no gray tone
;;    or color is printed, this includes the zebra stripes, that is, in `-mono'
;;    the zebra stripes are not printed.
;;
;;    See also documentation for `pr-ps-printer-alist'.
;;
;;
;; Using `printing'
;; ----------------
;;
;; To use `printing' insert in your ~/.emacs file (or c:/_emacs, if you're
;; using Windows 9x/NT or MS-DOS):
;;
;;    (require 'printing)
;;    ;; ...some user settings...
;;    (pr-update-menus t)
;;
;; During `pr-update-menus' evaluation:
;;   * On Emacs 20:
;;      it replaces the Tools/Print menu by Tools/Printing menu.
;;   * On Emacs 21:
;;      it replaces the File/Print* menu entries by File/Print menu.
;; Please, see section Menu Layout below for menu explanation.
;;
;; To use `printing' utilities you can use the Printing menu options, type M-x
;; followed by one of the commands below, or type a key associated with the
;; command you want (if there is a key binding).
;;
;; `printing' has the following commands:
;;
;;    pr-interface
;;    pr-ps-directory-preview
;;    pr-ps-directory-using-ghostscript
;;    pr-ps-directory-print
;;    pr-ps-directory-ps-print
;;    pr-ps-buffer-preview
;;    pr-ps-buffer-using-ghostscript
;;    pr-ps-buffer-print
;;    pr-ps-buffer-ps-print
;;    pr-ps-region-preview
;;    pr-ps-region-using-ghostscript
;;    pr-ps-region-print
;;    pr-ps-region-ps-print
;;    pr-ps-mode-preview
;;    pr-ps-mode-using-ghostscript
;;    pr-ps-mode-print
;;    pr-ps-mode-ps-print
;;    pr-ps-file-preview
;;    pr-ps-file-up-preview
;;    pr-ps-file-using-ghostscript
;;    pr-ps-file-print
;;    pr-ps-file-ps-print
;;    pr-ps-file-up-ps-print
;;    pr-ps-fast-fire
;;    pr-despool-preview
;;    pr-despool-using-ghostscript
;;    pr-despool-print
;;    pr-despool-ps-print
;;    pr-printify-directory
;;    pr-printify-buffer
;;    pr-printify-region
;;    pr-txt-directory
;;    pr-txt-buffer
;;    pr-txt-region
;;    pr-txt-mode
;;    pr-txt-fast-fire
;;    pr-toggle-file-duplex
;;    pr-toggle-file-tumble
;;    pr-toggle-file-landscape
;;    pr-toggle-ghostscript
;;    pr-toggle-faces
;;    pr-toggle-spool
;;    pr-toggle-duplex
;;    pr-toggle-tumble
;;    pr-toggle-landscape
;;    pr-toggle-upside-down
;;    pr-toggle-line
;;    pr-toggle-zebra
;;    pr-toggle-header
;;    pr-toggle-lock
;;    pr-toggle-region
;;    pr-toggle-mode
;;    pr-customize
;;    lpr-customize
;;    pr-help
;;    pr-ps-name
;;    pr-txt-name
;;    pr-ps-utility
;;    pr-show-ps-setup
;;    pr-show-pr-setup
;;    pr-show-lpr-setup
;;
;; The general meanings of above commands are:
;;
;;    PREFIX:
;;    `pr-interface'	buffer interface for printing package.
;;    `pr-help'		help for printing package.
;;    `pr-ps-name'	interactively select a PostScript printer.
;;    `pr-txt-name'	interactively select a text printer.
;;    `pr-ps-utility'	interactively select a PostScript utility.
;;    `pr-show-*-setup'	show current settings.
;;    `pr-ps-*'		deal with PostScript code generation.
;;    `pr-txt-*'	deal with text generation.
;;    `pr-toggle-*'	toggle on/off some boolean variable.
;;    `pr-despool-*'	despool the PostScript spooling buffer.
;;    `pr-printify-*'	replace nonprintable ASCII by printable ASCII
;;			representation.
;;
;;    SUFFIX:
;;    `*-customize'		customization.
;;    `*-preview'		preview a PostScript file.
;;    `*-using-ghostscript'	use ghostscript to print.
;;    `*-fast-fire'		fast fire command (see it for documentation).
;;    `*-print'			send PostScript directly to printer.
;;    `*-ps-print'		send PostScript directly to printer or use
;;				ghostscript to print.  It depends on
;;				`pr-print-using-ghostscript' option.
;;
;;    INFIX/SUFFIX:
;;    `*-directory*'	process a directory.
;;    `*-buffer*'	process a buffer.
;;    `*-region*'	process a region.
;;    `*-mode*'		process a major mode (see explanation below).
;;    `*-file-*'	process a PostScript file.
;;    `*-file-up-*'	process a PostScript file using a filter utility.
;;
;; Here are some examples:
;;
;;    `pr-ps-buffer-using-ghostscript'
;;	 Use ghostscript to print a buffer.
;;
;;    `pr-ps-file-print'
;;	 Print a PostScript file.
;;
;;    `pr-toggle-spool'
;;	 Toggle spooling buffer.
;;
;; So you can preview through ghostview, use ghostscript to print (if you don't
;; have a PostScript printer) or send directly to printer a PostScript code
;; generated by `ps-print' package.
;;
;; Besides operating one buffer or region each time, you also can postpone
;; previewing or printing by saving the PostScript code generated in a
;; temporary Emacs buffer.  This way you can save banner pages between
;; successive printing.  You can toggle on/off spooling by invoking
;; `pr-toggle-spool' interactively or through menu bar.
;;
;; If you type, for example:
;;
;;    C-u M-x pr-ps-buffer-print RET
;;
;; The `pr-ps-buffer-print' command prompts you for a n-up printing number and
;; a file name, and save the PostScript code generated to the file name instead
;; of sending to printer.
;;
;; This behavior is similar with the commands that deal with PostScript code
;; generation, that is, with `pr-ps-*' and `pr-despool-*' commands.  If
;; spooling is on, only `pr-despool-*' commands prompt for a file name and save
;; the PostScript code spooled in this file.
;;
;; Besides the behavior described above, the `*-directory*' commands also
;; prompt for a directory and a file name regexp.  So, it's possible to process
;; all or certain files on a directory at once (see also documentation for
;; `pr-list-directory').
;;
;; `printing' has also a special way to handle some major mode through
;; `*-mode*' commands.  So it's possible to customize a major mode printing,
;; it's only needed to declare the customization in `pr-mode-alist' (see
;; section Options) and invoke some of `*-mode*' commands.  An example for
;; major mode usage is when you're using gnus (or mh, or rmail, etc.) and
;; you're in the *Summary* buffer, if you forget to switch to the *Article*
;; buffer before printing, you'll get a nicely formatted list of article
;; subjects shows up at the printer.  With major mode printing you don't need
;; to switch from gnus *Summary* buffer first.
;;
;; Current global keyboard mapping for GNU Emacs is:
;;
;;    (global-set-key [print]     'pr-ps-fast-fire)
;;    (global-set-key [M-print]   'pr-ps-mode-using-ghostscript)
;;    (global-set-key [S-print]   'pr-ps-mode-using-ghostscript)
;;    (global-set-key [C-print]   'pr-txt-fast-fire)
;;    (global-set-key [C-M-print] 'pr-txt-fast-fire)
;;
;; And for XEmacs is:
;;
;;    (global-set-key 'f22                'pr-ps-fast-fire)
;;    (global-set-key '(meta f22)         'pr-ps-mode-using-ghostscript)
;;    (global-set-key '(shift f22)        'pr-ps-mode-using-ghostscript)
;;    (global-set-key '(control f22)      'pr-txt-fast-fire)
;;    (global-set-key '(control meta f22) 'pr-txt-fast-fire)
;;
;; As a suggestion of global keyboard mapping for some `printing' commands:
;;
;;    (global-set-key "\C-ci"  'pr-interface)
;;    (global-set-key "\C-cbp" 'pr-ps-buffer-print)
;;    (global-set-key "\C-cbx" 'pr-ps-buffer-preview)
;;    (global-set-key "\C-cbb" 'pr-ps-buffer-using-ghostscript)
;;    (global-set-key "\C-crp" 'pr-ps-region-print)
;;    (global-set-key "\C-crx" 'pr-ps-region-preview)
;;    (global-set-key "\C-crr" 'pr-ps-region-using-ghostscript)
;;
;;
;; Options
;; -------
;;
;; Below it's shown a brief description of `printing' options, please, see the
;; options declaration in the code for a long documentation.
;;
;; `pr-path-style'		Specify which path style to use for external
;;				commands.
;;
;; `pr-path-alist'		Specify an alist for command paths.
;;
;; `pr-txt-name'		Specify a printer for printing a text file.
;;
;; `pr-txt-printer-alist'	Specify an alist of all text printers.
;;
;; `pr-ps-name'			Specify a printer for printing a PostScript
;;				file.
;;
;; `pr-ps-printer-alist'	Specify an alist for all PostScript printers.
;;
;; `pr-temp-dir'		Specify a directory for temporary files during
;;				printing.
;;
;; `pr-ps-temp-file'		Specify PostScript temporary file name prefix.
;;
;; `pr-file-modes'		Specify the file permission bits for newly
;;				created files.
;;
;; `pr-gv-command'		Specify path and name of the gsview/gv
;;				utility.
;;
;; `pr-gs-command'		Specify path and name of the ghostscript
;;				utility.
;;
;; `pr-gs-switches'		Specify ghostscript switches.
;;
;; `pr-gs-device'		Specify ghostscript device switch value.
;;
;; `pr-gs-resolution'		Specify ghostscript resolution switch value.
;;
;; `pr-print-using-ghostscript'	Non-nil means print using ghostscript.
;;
;; `pr-faces-p'			Non-nil means print with face attributes.
;;
;; `pr-spool-p'			Non-nil means spool printing in a buffer.
;;
;; `pr-file-landscape'		Non-nil means print PostScript file in
;;				landscape orientation.
;;
;; `pr-file-duplex'		Non-nil means print PostScript file in duplex
;;				mode.
;;
;; `pr-file-tumble'		Non-nil means print PostScript file in tumble
;;				mode.
;;
;; `pr-auto-region'		Non-nil means region is automagically detected.
;;
;; `pr-auto-mode'		Non-nil means major-mode specific printing is
;;				preferred over normal printing.
;;
;; `pr-mode-alist'		Specify an alist for a major-mode and printing
;;				function.
;;
;; `pr-ps-utility'		Specify PostScript utility processing.
;;
;; `pr-ps-utility-alist'	Specify an alist for PostScript utility
;;				processing.
;;
;; `pr-menu-lock'		Non-nil means menu is locked while selecting
;;				toggle options.
;;
;; `pr-menu-char-height'	Specify menu char height in pixels.
;;
;; `pr-menu-char-width'		Specify menu char width in pixels.
;;
;; `pr-setting-database'	Specify an alist for settings in general.
;;
;; `pr-visible-entry-list'	Specify a list of Printing menu visible
;;				entries.
;;
;; `pr-delete-temp-file'	Non-nil means delete temporary files.
;;
;; `pr-list-directory'		Non-nil means list directory when processing a
;;				directory.
;;
;; `pr-buffer-name'		Specify the name of the buffer interface for
;;				printing package.
;;
;; `pr-buffer-name-ignore'	Specify a regexp list for buffer names to be
;;				ignored in interface buffer.
;;
;; `pr-buffer-verbose'		Non-nil means to be verbose when editing a
;;				field in interface buffer.
;;
;; To set the above options you may:
;;
;; a) insert the code in your ~/.emacs, like:
;;
;;	 (setq pr-faces-p t)
;;
;;    This way always keep your default settings when you enter a new Emacs
;;    session.
;;
;; b) or use `set-variable' in your Emacs session, like:
;;
;;	 M-x set-variable RET pr-faces-p RET t RET
;;
;;    This way keep your settings only during the current Emacs session.
;;
;; c) or use customization, for example:
;;	 click on menu-bar *Help* option,
;;	 then click on *Customize*,
;;	 then click on *Browse Customization Groups*,
;;	 expand *PostScript* group,
;;	 expand *Printing* group
;;	 and then customize `printing' options.
;;    Through this way, you may choose if the settings are kept or not when
;;    you leave out the current Emacs session.
;;
;; d) or see the option value:
;;
;;	 C-h v pr-faces-p RET
;;
;;    and click the *customize* hypertext button.
;;    Through this way, you may choose if the settings are kept or not when
;;    you leave out the current Emacs session.
;;
;; e) or invoke:
;;
;;	 M-x pr-customize RET
;;
;;    and then customize `printing' options.
;;    Through this way, you may choose if the settings are kept or not when
;;    you leave out the current Emacs session.
;;
;; f) or use menu bar, for example:
;;	 click on menu-bar *File* option,
;;	 then click on *Printing*,
;;	 then click on *Customize*,
;;	 then click on *printing*
;;	 and then customize `printing' options.
;;    Through this way, you may choose if the settings are kept or not when
;;    you leave out the current Emacs session.
;;
;;
;; Menu Layout
;; -----------
;;
;; The `printing' menu (Tools/Printing or File/Print) has the following layout:
;;
;;        +-----------------------------+
;; A   0  |   Printing Interface        |
;;        +-----------------------------+       +-A---------+     +-B------+
;; I   1  |   PostScript Preview       >|-------|Directory >|-----|1-up    |
;;     2  |   PostScript Print         >|---- A |Buffer    >|-- B |2-up    |
;;     3  |   PostScript Printer: name >|---- C |Region    >|-- B |4-up    |
;;        +-----------------------------+       |Mode      >|-- B |Other...|
;; II  4  |   Printify                 >|-----\ |File      >|--\  +--------+
;;     5  |   Print                    >|---\ | |Despool... |  |
;;     6  |   Text Printer: name       >|-\ | | +-----------+  |
;;        +-----------------------------+ | | | +---------+   +------------+
;; III 7  |[ ]Landscape                 | | | \-|Directory|   | No Prep... | Ia
;;     8  |[ ]Print Header              | | |   |Buffer   |   +------------+ Ib
;;     9  |[ ]Print Header Frame        | | |   |Region   |   |   name    >|- C
;;     10 |[ ]Line Number               | | |   +---------+   +------------+
;;     11 |[ ]Zebra Stripes             | | |   +---------+   |   1-up...  | Ic
;;     12 |[ ]Duplex                    | | \---|Directory|   |   2-up...  |
;;     13 |[ ]Tumble                    | \--\  |Buffer   |   |   4-up...  |
;;     14 |[ ]Upside-Down               |    |  |Region   |   |   Other... |
;;     15 |   Print All Pages          >|--\ |  |Mode     |   +------------+
;;        +-----------------------------+  | |  +---------+   |[ ]Landscape| Id
;; IV  16 |[ ]Spool Buffer              |  | |  +-C-------+   |[ ]Duplex   | Ie
;;     17 |[ ]Print with faces          |  | \--|( )name A|   |[ ]Tumble   | If
;;     18 |[ ]Print via Ghostscript     |  |    |( )name B|   +------------+
;;        +-----------------------------+  |    |...      |
;; V   19 |[ ]Auto Region               |  |    |(*)name  |
;;     20 |[ ]Auto Mode                 |  |    |...      |
;;     21 |[ ]Menu Lock                 |  |    +---------+   +--------------+
;;        +-----------------------------+  \------------------|(*)All Pages  |
;; VI  22 |   Customize                >|--- D  +-D------+    |( )Even Pages |
;;     23 |   Show Settings            >|-------|printing|    |( )Odd Pages  |
;;     24 |   Help                      |       |ps-print|    |( )Even Sheets|
;;        +-----------------------------+       |lpr     |    |( )Odd Sheets |
;;                                              +--------+    +--------------+
;;
;; See `pr-visible-entry-list' for hiding some parts of the menu.
;;
;; The menu has the following sections:
;;
;; A. Interface:
;;
;;    0. You can use a buffer interface instead of menus.  It looks like the
;;	 customization buffer.  Basically, it has the same options found in the
;;	 menu and some extra options, all this on a buffer.
;;
;; I. PostScript printing:
;;
;;    1. You can generate a PostScript file (if you type C-u before activating
;;	 menu) or PostScript temporary file for a directory, a buffer, a region
;;	 or a major mode, choosing 1-up, 2-up, 4-up or any other n-up printing;
;;	 after file generation, ghostview is activated using the file generated
;;	 as argument.  This option is disabled if spooling is on (option 16).
;;	 Also, if you already have a PostScript file you can preview it.
;;	 Instead of previewing each buffer, region or major mode at once, you
;;	 can save temporarily the PostScript code generated in a buffer and
;;	 preview it later.  The option `Despool...' despools the PostScript
;;	 spooling buffer in a temporary file and uses ghostview to preview it.
;;	 If you type C-u before choosing this option, the PostScript code
;;	 generated is saved in a file instead of saving in a temporary file.
;;	 To spool the PostScript code generated you need to turn on the option
;;	 16.  The option `Despool...' is enabled if spooling is on (option
;;	 16).
;;
;;	 NOTE 1: It's possible to customize a major mode printing, just declare
;;		 the customization in `pr-mode-alist' and invoke some of
;;		 `*-mode*' commands or select Mode option in Printing menu.  An
;;		 example for major mode usage is when you're using gnus (or mh,
;;		 or rmail, etc.) and you're in the *Summary* buffer, if you
;;		 forget to switch to the *Article* buffer before printing,
;;		 you'll get a nicely formatted list of article subjects shows
;;		 up at the printer.  With major mode printing you don't need to
;;		 switch from gnus *Summary* buffer first.
;;
;;	 NOTE 2: There are the following options for PostScript file
;;		 processing:
;;		 Ia. Print the file *No Preprocessing*, that is, send it
;;		     directly to PostScript printer.
;;		 Ib. PostScript utility processing selection.
;;		     See `pr-ps-utility-alist' and `pr-setting-database' for
;;		     documentation.
;;		 Ic. Do n-up processing before printing.
;;		 Id. Toggle on/off landscape for PostScript file processing.
;;		 Ie. Toggle on/off duplex for PostScript file processing.
;;		 If. Toggle on/off tumble for PostScript file processing.
;;
;;	 NOTE 3: Don't forget to download and install the utilities declared on
;;		 `pr-ps-utility-alist'.
;;
;;    2. Operate the same way as option 1, but it sends directly the PostScript
;;	 code (or put in a file, if you've typed C-u) or it uses ghostscript to
;;	 print the PostScript file generated.  It depends on option 18, if it's
;;	 turned on, it uses ghostscript; otherwise, it sends directly to
;;	 printer.  If spooling is on (option 16), the PostScript code is saved
;;	 temporarily in a buffer instead of printing it or saving it in a file.
;;	 Also, if you already have a PostScript file you can print it.  Instead
;;	 of printing each buffer, region or major mode at once, you can save
;;	 temporarily the PostScript code generated in a buffer and print it
;;	 later.  The option `Despool...' despools the PostScript spooling
;;	 buffer directly on a printer.  If you type C-u before choosing this
;;	 option, the PostScript code generated is saved in a file instead of
;;	 sending to printer.  To spool the PostScript code generated you need
;;	 to turn on the option 16.  This option is enabled if spooling is on
;;	 (option 16).  See also the NOTE 1, NOTE 2 and NOTE 3 on option 1.
;;
;;    3. You can select a new PostScript printer to send PostScript code
;;	 generated.  For selection it's used all PostScript printers defined
;;	 in `pr-ps-printer-alist' variable (see it for documentation).
;;	 See also `pr-setting-database'.
;;
;; II. Text printing:
;;
;;    4. If you have control characters (character code from \000 to \037) in a
;;	 buffer and you want to print them in a text printer, select this
;;	 option.  All control characters in your buffer or region will be
;;	 replaced by a printable representation.  The printable representations
;;	 use ^ (for ASCII control characters) or hex.  The characters tab,
;;	 linefeed, space, return and formfeed are not affected.  You don't need
;;	 to select this option if you use any option of section I, the
;;	 PostScript engine treats control characters properly.
;;
;;    5. If you want to print a directory, buffer, region or major mode in a
;;	 text printer, select this option.  See also the NOTE 1 on option 1.
;;
;;    6. You can select a new text printer to send text generated.  For
;;	 selection it's used all text printers defined in
;;	 `pr-txt-printer-alist' variable (see it for documentation).
;;	 See also `pr-setting-database'.
;;
;; III. PostScript page toggle options:
;;
;;    7. If you want a PostScript landscape printing, turn on this option.
;;
;;    8. If you want to have a header in each page in your PostScript code,
;;	 turn on this option.
;;
;;    9. If you want to draw a gaudy frame around the header, turn on this
;;	 option.  This option is enabled if print header is on (option 8).
;;
;;    10. If you want that the line number is printed in your PostScript code,
;;	  turn on this option.
;;
;;    11. If you want background zebra stripes in your PostScript code, turn on
;;	  this option.
;;
;;    12. If you want a duplex printing and your PostScript printer has this
;;	  feature, turn on this option.
;;
;;    13. If you turned on duplex printing, you can choose if you want to have
;;	  a printing suitable for binding on the left or right (tumble off), or
;;	  to have a printing suitable for binding at top or bottom (tumble on).
;;	  This option is enabled if duplex is on (option 12).
;;
;;    14. If you want a PostScript upside-down printing, turn on this option.
;;
;;    15. With this option, you can choose if you want to print all pages, odd
;;	  pages, even pages, odd sheets or even sheets.
;;	  See also `ps-even-or-odd-pages'.
;;
;; IV. PostScript processing toggle options:
;;
;;    16. If you want to spool the PostScript code generated, turn on this
;;	  option.  To spool the PostScript code generated use option 2.  You
;;	  can despool later by choosing option 1 or 2, sub-option `Despool...'.
;;
;;    17. If you use colors in your buffers and want to see these colors on
;;	  your PostScript code generated, turn on this option.  If you have a
;;	  black/white PostScript printer, these colors are displayed in gray
;;	  scale by PostScript printer interpreter.
;;
;;    18. If you don't have a PostScript printer to send PostScript files, turn
;;	  on this option.  When this option is on, the ghostscript is used to
;;	  print PostScript files.  In GNU or Unix system, if ghostscript is set
;;	  as a PostScript filter, you don't need to turn on this option.
;;
;; V. Printing customization:
;;
;;    19. If you want that region is automagically detected, turn on this
;;	  option.  Note that this will only work if you're using transient mark
;;	  mode.  When this option is on, the `*-buffer*' commands will behave
;;	  like `*-region*' commands, that is, `*-buffer*' commands will print
;;	  only the region marked instead of all buffer.
;;
;;    20. Turn this option on if you want that when current major-mode is
;;	  declared in `pr-mode-alist', the `*-buffer*' and `*-region*' commands
;;	  behave like `*-mode*' commands.
;;
;;    21. If you want that Printing menu stays open while you are setting
;;	  toggle options, turn on this option.  The variables
;;	  `pr-menu-char-height' and `pr-menu-char-width' are used to guess the
;;	  menu position, so don't forget to adjust these variables if menu
;;	  position is not ok.
;;
;; VI. Customization:
;;
;;    22. Besides all options in section III, IV and V, you can customize much
;;	  more PostScript options in `ps-print' option.  Or you can customize
;;	  some `lpr' options for text printing.  Or customize `printing'
;;	  options.
;;
;;    23. Show current settings for `printing', `ps-print' or `lpr'.
;;
;;    24. Quick help for printing menu layout.
;;
;;
;; Option Settings
;; ---------------
;;
;; Below it's shown only the main options that affect all `printing' package.
;; Check all the settings below *BEFORE* running `printing' commands.
;;
;; * Example of setting for GNU or Unix system:
;;
;;    (require 'printing)
;;    (setq pr-path-alist
;;	    '((unix      "." "~/bin" ghostview mpage PATH)
;;	      (ghostview "$HOME/bin/gsview-dir")
;;	      (mpage     "$HOME/bin/mpage-dir")
;;	      ))
;;    (setq pr-txt-name      'prt_06a)
;;    (setq pr-txt-printer-alist
;;          '((prt_06a "lpr" nil "prt_06a")
;;            (prt_07c nil   nil "prt_07c")
;;            ))
;;    (setq pr-ps-name       'lps_06b)
;;    (setq pr-ps-printer-alist
;;          '((lps_06b "lpr" nil "-P" "lps_06b")
;;            (lps_07c "lpr" nil nil  "lps_07c")
;;            (lps_08c nil   nil nil  "lps_08c")
;;            ))
;;    (setq pr-temp-dir      "/tmp/")
;;    (setq pr-gv-command    "gv")
;;    (setq pr-gs-command    "gs")
;;    (setq pr-gs-switches '("-q -dNOPAUSE -I/usr/share/ghostscript/5.10"))
;;    (setq pr-gs-device     "uniprint")
;;    (setq pr-gs-resolution 300)
;;    (setq pr-ps-utility    'mpage)
;;    (setq pr-ps-utility-alist
;;	    '((mpage "mpage" nil    "-b%s" "-%d" "-l" "-t" "-T" ">" nil)
;;	      (psnup "psnup" ("-q") "-P%s" "-%d" "-l" nil  nil  " " nil
;;		     (inherits-from: . no-duplex))
;;	      ))
;;    (setq pr-setting-database
;;	    '((no-duplex
;;	       nil nil nil
;;	       (pr-file-duplex . nil)
;;	       (pr-file-tumble . nil))
;;	      ))
;;    (pr-update-menus t)		; update now printer and utility menus
;;
;; * Example of setting for Windows system:
;;
;;    (require 'printing)
;;    (setq pr-path-alist
;;	    '((windows   "c:/applications/executables" PATH ghostview mpage)
;;	      (ghostview "c:/gs/gsview-dir")
;;	      (mpage     "c:/mpage-dir")
;;	      ))
;;    (setq pr-txt-name      'prt_06a)
;;    (setq pr-txt-printer-alist
;;          '((prt_06a  "print"     nil "/D:\\\\printers\\prt_06a")
;;            (prt_07c  nil         nil "/D:\\\\printers\\prt_07c")
;;            (PRN      ""          nil "PRN")
;;            (standard "redpr.exe" nil "")
;;            ))
;;    (setq pr-ps-name       'lps_06b)
;;    (setq pr-ps-printer-alist
;;          '((lps_06a  "print"     nil "/D:" "\\\\printers\\lps_06a")
;;            (lps_06b  "print"     nil nil   "\\\\printers\\lps_06b")
;;            (lps_07c  "print"     nil ""    "/D:\\\\printers\\lps_07c")
;;            (lps_08c  nil         nil nil   "\\\\printers\\lps_08c")
;;            (b/w      "gsprint" ("-all" "-twoup") "-printer " "b/w-pr-name")
;;            (LPT1     ""          nil ""    "LPT1:")
;;            (PRN      ""          nil ""    "PRN")
;;            (standard "redpr.exe" nil ""    "")
;;            ))
;;    (setq pr-temp-dir      "C:/WINDOWS/TEMP/")
;;    (setq pr-gv-command    "c:/gs/gsview/gsview32.exe")
;;    (setq pr-gs-command    "c:/gs/gswin32.exe")
;;    (setq pr-gs-switches '("-q -dNOPAUSE -Ic:/gs/gs5.50;c:/gs/gs5.50/fonts"))
;;    (setq pr-gs-device     "mswinpr2")
;;    (setq pr-gs-resolution 300)
;;    (setq pr-ps-utility    'psnup)
;;    (setq pr-ps-utility-alist
;;	    '((psnup "c:/psutils/psnup" ("-q") "-P%s" "-%d" "-l" nil nil " "
;;		     nil (inherits-from: . no-duplex))
;;	      ))
;;    (setq pr-setting-database
;;	    '((no-duplex
;;	       nil nil nil
;;	       (pr-file-duplex . nil)
;;	       (pr-file-tumble . nil))
;;	      ))
;;    (pr-update-menus t)		; update now printer and utility menus
;;
;; NOTE: Don't forget to download and install the utilities declared on
;;	 `pr-ps-utility-alist'.
;;
;;
;; Utilities
;; ---------
;;
;; `printing' package has the following utilities:
;;
;;    `pr-setup'	Return the current `printing' setup.
;;
;;    `lpr-setup'	Return the current `lpr' setup.
;;
;;    `pr-update-menus'	Update utility, PostScript and text printer menus.
;;
;;    `pr-menu-bind'	Install `printing' menu in the menubar.
;;
;;
;; Below are some URL where you can find good utilities.
;;
;; * For `printing' package:
;;
;;    printing	`http://www.emacswiki.org/cgi-bin/emacs/download/printing.el'
;;    ps-print	`http://www.emacswiki.org/cgi-bin/wiki/PsPrintPackage'
;;
;; * For GNU or Unix system:
;;
;;    gs, gv         `http://www.gnu.org/software/ghostscript/ghostscript.html'
;;    enscript       `http://people.ssh.fi/mtr/genscript/'
;;    psnup          `http://www.knackered.org/angus/psutils/'
;;    mpage          `http://www.mesa.nl/pub/mpage/'
;;
;; * For Windows system:
;;
;;    gswin32, gsview32
;;                   `http://www.gnu.org/software/ghostscript/ghostscript.html'
;;    gsprint        `http://www.cs.wisc.edu/~ghost/gsview/gsprint.htm'.
;;    enscript       `http://people.ssh.fi/mtr/genscript/'
;;    psnup          `http://gnuwin32.sourceforge.net/packages/psutils.htm'
;;    redmon         `http://www.cs.wisc.edu/~ghost/redmon/'
;;
;;
;; Acknowledgments
;; ---------------
;;
;; Thanks to Stefan Monnier <monnier@iro.umontreal.ca> for GNU Emacs and XEmacs
;; printing menu (in `pr-menu-spec') merging suggestion.
;;
;; Thanks to Lennart Borgman <lennart.borgman.073@student.lu.se> for gsprint
;; suggestion (see tip 5 in section Tips).
;;
;; Thanks to Drew Adams <drew.adams@oracle.com> for suggestions:
;;    - directory processing.
;;    - `pr-path-alist' variable.
;;    - doc fix.
;;    - a lot of tests on Windows.
;;
;; Thanks to Fred Labrosse <f.labrosse@maths.bath.ac.uk> for XEmacs tests.
;;
;; Thanks to Klaus Berndl <klaus.berndl@sdm.de> for invaluable help/debugging
;; and for suggestions:
;;    - even/odd pages printing.
;;    - ghostscript parameters for `pr-ps-printer-alist'.
;;    - default printer name.
;;    - completion functions.
;;    - automagic region detection.
;;    - menu entry hiding.
;;    - fast fire PostScript printing command.
;;    - `pr-path-style' variable.
;;
;; Thanks to Kim F. Storm <storm@filanet.dk> for beta-test and for suggestions:
;;    - PostScript Print and PostScript Print Preview merge.
;;    - Tools/Printing menu.
;;    - replace *-using-preview by *-using-ghostscript.
;;    - printer selection.
;;    - extra parameters for `pr-ps-printer-alist'.
;;
;; Thanks to:
;;    Frederic Corne <frederic.corne@erli.fr>		print-nt.el
;;    Tom Vogels <tov@ece.cmu.edu>			mh-e-init.el
;;    Matthew O. Persico <mpersico@erols.com>		win32-ps-print.el
;;    Volker Franz <volker.franz@tuebingen.mpg.de>	ps-print-interface.el
;; And to all people who contributed with them.
;;
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:


(require 'lpr)
(require 'ps-print)


(and (string< ps-print-version "6.6.4")
     (error "`printing' requires `ps-print' package version 6.6.4 or later"))


(defconst pr-cygwin-system
  (and ps-windows-system (getenv "OSTYPE")
       (string-match "cygwin" (getenv "OSTYPE"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To avoid compilation gripes


(or (fboundp 'subst-char-in-string)	; hacked from subr.el
    (defun subst-char-in-string (fromchar tochar string &optional inplace)
      "Replace FROMCHAR with TOCHAR in STRING each time it occurs.
Unless optional argument INPLACE is non-nil, return a new string."
      (let ((i (length string))
	    (newstr (if inplace string (copy-sequence string))))
	(while (> (setq i (1- i)) 0)
	  (if (eq (aref newstr i) fromchar)
	      (aset newstr i tochar)))
	newstr)))


(or (fboundp 'make-temp-file)		; hacked from subr.el
    (defun make-temp-file (prefix &optional dir-flag suffix)
      "Create a temporary file.
The returned file name (created by appending some random characters at the end
of PREFIX, and expanding against `temporary-file-directory' if necessary),
is guaranteed to point to a newly created empty file.
You can then use `write-region' to write new data into the file.

If DIR-FLAG is non-nil, create a new empty directory instead of a file.

If SUFFIX is non-nil, add that at the end of the file name."
      (let ((umask (default-file-modes))
	    file)
	(unwind-protect
	    (progn
	      ;; Create temp files with strict access rights.  It's easy to
	      ;; loosen them later, whereas it's impossible to close the
	      ;; time-window of loose permissions otherwise.
	      (set-default-file-modes ?\700)
	      (while (condition-case ()
			 (progn
			   (setq file
				 (make-temp-name
				  (expand-file-name prefix temporary-file-directory)))
			   (if suffix
			       (setq file (concat file suffix)))
			   (if dir-flag
			       (make-directory file)
			     (write-region "" nil file nil 'silent nil 'excl))
			   nil)
		       (file-already-exists t))
		;; the file was somehow created by someone else between
		;; `make-temp-name' and `write-region', let's try again.
		nil)
	      file)
	  ;; Reset the umask.
	  (set-default-file-modes umask)))))


(eval-when-compile
  ;; User Interface --- declared here to avoid compiler warnings
  (defvar pr-path-style)
  (defvar pr-auto-region)
  (defvar pr-menu-char-height)
  (defvar pr-menu-char-width)
  (defvar pr-menu-lock)
  (defvar pr-ps-printer-alist)
  (defvar pr-txt-printer-alist)
  (defvar pr-ps-utility-alist)


  ;; Internal fun alias to avoid compilation gripes
  (defalias 'pr-menu-lookup            'ignore)
  (defalias 'pr-menu-lock              'ignore)
  (defalias 'pr-menu-alist             'ignore)
  (defalias 'pr-even-or-odd-pages      'ignore)
  (defalias 'pr-menu-get-item          'ignore)
  (defalias 'pr-menu-set-item-name     'ignore)
  (defalias 'pr-menu-set-utility-title 'ignore)
  (defalias 'pr-menu-set-ps-title      'ignore)
  (defalias 'pr-menu-set-txt-title     'ignore)
  (defalias 'pr-region-active-p        'ignore)
  (defalias 'pr-do-update-menus        'ignore)
  (defalias 'pr-update-mode-line       'ignore)
  (defalias 'pr-read-string            'ignore)
  (defalias 'pr-set-keymap-parents     'ignore)
  (defalias 'pr-keep-region-active     'ignore))


;; Internal Vars --- defined here to avoid compiler warnings
(defvar pr-menu-print-item "print"
  "Non-nil means that menu binding was not done.

Used by `pr-menu-bind' and `pr-update-menus'.")

(defvar pr-ps-printer-menu-modified  t
  "Non-nil means `pr-ps-printer-alist' was modified and we need to update menu.")

(defvar pr-txt-printer-menu-modified t
  "Non-nil means `pr-txt-printer-alist' was modified and we need to update menu.")

(defvar pr-ps-utility-menu-modified t
  "Non-nil means `pr-ps-utility-alist' was modified and we need to update menu.")

(defconst pr-even-or-odd-alist
  '((nil        . "Print All Pages")
    (even-page  . "Print Even Pages")
    (odd-page   . "Print Odd Pages")
    (even-sheet . "Print Even Sheets")
    (odd-sheet  . "Print Odd Sheets")))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; XEmacs Definitions


(cond
 ((featurep 'xemacs)			; XEmacs
  ;; XEmacs
  (defalias 'pr-set-keymap-parents 'set-keymap-parents)
  (defalias 'pr-set-keymap-name    'set-keymap-name)

  ;; XEmacs
  (defun pr-read-string (prompt initial history default)
    (let ((str (read-string prompt initial)))
      (if (and str (not (string= str "")))
	  str
	default)))

  ;; XEmacs
  (defvar zmacs-region-stays  nil)

  ;; XEmacs
  (defun pr-keep-region-active ()
    (setq zmacs-region-stays t))

  ;; XEmacs
  (defun pr-region-active-p ()
    (and pr-auto-region (not zmacs-region-stays) (ps-mark-active-p)))

  ;; XEmacs
  (defun pr-menu-char-height ()
    (font-height (face-font 'default)))

  ;; XEmacs
  (defun pr-menu-char-width ()
    (font-width (face-font 'default)))

  ;; XEmacs
  (defmacro pr-xemacs-global-menubar (&rest body)
    `(save-excursion
       (let ((temp (get-buffer-create (make-temp-name " *Temp"))))
	 ;; be sure to access global menubar
	 (set-buffer temp)
	 ,@body
	 (kill-buffer temp))))

  ;; XEmacs
  (defun pr-global-menubar (pr-menu-spec)
    ;; Menu binding
    (pr-xemacs-global-menubar
     (add-submenu nil (cons "Printing" pr-menu-spec) "Apps"))
    (setq pr-menu-print-item nil))

  ;; XEmacs
  (defvar current-mouse-event nil)
  (defun pr-menu-position (entry index horizontal)
    (make-event
     'button-release
     (list 'button 1
	   'x (- (event-x-pixel current-mouse-event) ; X
		 (* horizontal pr-menu-char-width))
	   'y (- (event-y-pixel current-mouse-event) ; Y
		 (* (pr-menu-index entry index) pr-menu-char-height)))))

  (defvar pr-menu-position nil)
  (defvar pr-menu-state nil)

  ;; XEmacs
  (defvar current-menubar nil)		; to avoid compilation gripes
  (defun pr-menu-lookup (path)
    (car (find-menu-item current-menubar (cons "Printing" path))))

  ;; XEmacs
  (defun pr-menu-lock (entry index horizontal state path)
    (when pr-menu-lock
      (or (and pr-menu-position (eq state pr-menu-state))
	  (setq pr-menu-position (pr-menu-position entry index horizontal)
		pr-menu-state    state))
      (let* ((menu   (pr-menu-lookup path))
	     (result (get-popup-menu-response menu pr-menu-position)))
	(and (misc-user-event-p result)
	     (funcall (event-function result)
		      (event-object result))))
      (setq pr-menu-position nil)))

  ;; XEmacs
  (defalias 'pr-update-mode-line 'set-menubar-dirty-flag)

  ;; XEmacs
  (defvar pr-ps-name-old     "PostScript Printers")
  (defvar pr-txt-name-old    "Text Printers")
  (defvar pr-ps-utility-old  "PostScript Utility")
  (defvar pr-even-or-odd-old "Print All Pages")

  ;; XEmacs
  (defun pr-do-update-menus (&optional force)
    (pr-menu-alist pr-ps-printer-alist
		   'pr-ps-name
		   'pr-menu-set-ps-title
		   '("Printing")
		   'pr-ps-printer-menu-modified
		   force
		   pr-ps-name-old
		   'postscript 2)
    (pr-menu-alist pr-txt-printer-alist
		   'pr-txt-name
		   'pr-menu-set-txt-title
		   '("Printing")
		   'pr-txt-printer-menu-modified
		   force
		   pr-txt-name-old
		   'text 2)
    (let ((save-var pr-ps-utility-menu-modified))
      (pr-menu-alist pr-ps-utility-alist
		     'pr-ps-utility
		     'pr-menu-set-utility-title
		     '("Printing" "PostScript Print" "File")
		     'save-var
		     force
		     pr-ps-utility-old
		     nil 1))
    (pr-menu-alist pr-ps-utility-alist
		   'pr-ps-utility
		   'pr-menu-set-utility-title
		   '("Printing" "PostScript Preview" "File")
		   'pr-ps-utility-menu-modified
		   force
		   pr-ps-utility-old
		   nil 1)
    (pr-even-or-odd-pages ps-even-or-odd-pages force))

  ;; XEmacs
  (defun pr-menu-alist (alist var-sym fun menu-path modified-sym force name
			      entry index)
    (when (and alist (or force (symbol-value modified-sym)))
      (pr-xemacs-global-menubar
       (add-submenu menu-path
		    (pr-menu-create name alist var-sym
				    fun entry index)))
      (funcall fun (symbol-value var-sym))
      (set modified-sym nil)))

  ;; XEmacs
  (defun pr-relabel-menu-item (newname var-sym)
    (pr-xemacs-global-menubar
     (relabel-menu-item
      (list "Printing" (symbol-value var-sym))
      newname)
     (set var-sym newname)))

  ;; XEmacs
  (defun pr-menu-set-ps-title (value &optional item entry index)
    (pr-relabel-menu-item (format "PostScript Printer: %s" value)
			  'pr-ps-name-old)
    (pr-ps-set-printer value)
    (and index
	 (pr-menu-lock entry index 12 'toggle nil)))

  ;; XEmacs
  (defun pr-menu-set-txt-title (value &optional item entry index)
    (pr-relabel-menu-item (format "Text Printer: %s" value)
			  'pr-txt-name-old)
    (pr-txt-set-printer value)
    (and index
	 (pr-menu-lock entry index 12 'toggle nil)))

  ;; XEmacs
  (defun pr-menu-set-utility-title (value &optional item entry index)
    (pr-xemacs-global-menubar
     (let ((newname (format "%s" value)))
       (relabel-menu-item
	(list "Printing" "PostScript Print" "File" pr-ps-utility-old)
	newname)
       (relabel-menu-item
	(list "Printing" "PostScript Preview" "File" pr-ps-utility-old)
	newname)
       (setq pr-ps-utility-old newname)))
    (pr-ps-set-utility value)
    (and index
	 (pr-menu-lock entry index 5 nil '("PostScript Print" "File"))))

  ;; XEmacs
  (defun pr-even-or-odd-pages (value &optional no-lock)
    (pr-relabel-menu-item (cdr (assq value pr-even-or-odd-alist))
			  'pr-even-or-odd-old)
    (setq ps-even-or-odd-pages value)
    (or no-lock
	(pr-menu-lock 'postscript-options 8 12 'toggle nil)))

  )
 (t					; GNU Emacs
  ;; Do nothing
  ))					; end cond featurep



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GNU Emacs Definitions


(cond
 ((featurep 'xemacs)			; XEmacs
  ;; Do nothing
  )
 (t					; GNU Emacs
  ;; GNU Emacs
  (defalias 'pr-set-keymap-parents 'set-keymap-parent)
  (defalias 'pr-set-keymap-name    'ignore)
  (defalias 'pr-read-string        'read-string)

  ;; GNU Emacs
  (defvar deactivate-mark)

  ;; GNU Emacs
  (defun pr-keep-region-active ()
    (setq deactivate-mark nil))

  ;; GNU Emacs
  (defun pr-region-active-p ()
    (and pr-auto-region transient-mark-mode mark-active))

  ;; GNU Emacs
  (defun pr-menu-char-height ()
    (frame-char-height))

  ;; GNU Emacs
  (defun pr-menu-char-width ()
    (frame-char-width))

  (defvar pr-menu-bar nil
    "Specify Printing menu-bar entry.")

  ;; GNU Emacs
  ;; Menu binding
  ;; Replace existing "print" item by "Printing" item.
  ;; If you're changing this file, you'll load it a second,
  ;; third... time, but "print" item exists only in the first load.
  (eval-when-compile
    (require 'easymenu))		; to avoid compilation gripes

  (eval-and-compile
      (defun pr-global-menubar (pr-menu-spec)
	(require 'easymenu)
	(let ((menu-file (if (= emacs-major-version 21)
			     '("menu-bar" "files") ; GNU Emacs 21
			   '("menu-bar" "file")))) ; GNU Emacs 22 or higher
	  (cond
	   (pr-menu-print-item
	    (easy-menu-add-item global-map menu-file
				(easy-menu-create-menu "Print" pr-menu-spec)
				"print-buffer")
	    (dolist (item '("print-buffer"          "print-region"
			    "ps-print-buffer-faces" "ps-print-region-faces"
			    "ps-print-buffer"       "ps-print-region"))
	      (easy-menu-remove-item global-map menu-file item))
	    (setq pr-menu-print-item nil
		  pr-menu-bar (vector 'menu-bar
				      (pr-get-symbol (nth 1 menu-file))
				      (pr-get-symbol "Print"))))
	   (t
	    (easy-menu-add-item global-map menu-file
				(easy-menu-create-menu "Print" pr-menu-spec)))
	   ))))

  (eval-and-compile
    (cond
     (ps-windows-system
      ;; GNU Emacs for Windows 9x/NT
      (defun pr-menu-position (entry index horizontal)
	(let ((pos (cdr (mouse-pixel-position))))
	  (list
	   (list (or (car pos) 0)	; X
		 (- (or (cdr pos) 0)	; Y
		    (* (pr-menu-index entry index) pr-menu-char-height)))
	   (selected-frame))))		; frame
      )
     (t
      ;; GNU Emacs
      (defun pr-menu-position (entry index horizontal)
	(let ((pos (cdr (mouse-pixel-position))))
	  (list
	   (list (- (or (car pos) 0)	; X
		    (* horizontal pr-menu-char-width))
		 (- (or (cdr pos) 0)	; Y
		    (* (pr-menu-index entry index) pr-menu-char-height)))
	   (selected-frame))))		; frame
      )))

  (defvar pr-menu-position nil)
  (defvar pr-menu-state    nil)

  ;; GNU Emacs
  (defun pr-menu-lookup (path)
    (lookup-key global-map
		(if path
		    (vconcat pr-menu-bar
			     (mapcar 'pr-get-symbol
				     (if (listp path)
					 path
				       (list path))))
		  pr-menu-bar)))

  ;; GNU Emacs
  (defun pr-menu-lock (entry index horizontal state path)
    (when pr-menu-lock
      (or (and pr-menu-position (eq state pr-menu-state))
	  (setq pr-menu-position (pr-menu-position entry index horizontal)
		pr-menu-state    state))
      (let* ((menu   (pr-menu-lookup path))
	     (result (x-popup-menu pr-menu-position menu)))
	(and result
	     (let ((command (lookup-key menu (vconcat result))))
	       (if (fboundp command)
		   (funcall command)
		 (eval command)))))
      (setq pr-menu-position nil)))

  ;; GNU Emacs
  (defalias 'pr-update-mode-line 'force-mode-line-update)

  ;; GNU Emacs
  (defun pr-do-update-menus (&optional force)
    (pr-menu-alist pr-ps-printer-alist
		   'pr-ps-name
		   'pr-menu-set-ps-title
		   "PostScript Printers"
		   'pr-ps-printer-menu-modified
		   force
		   "PostScript Printers"
		   'postscript 2)
    (pr-menu-alist pr-txt-printer-alist
		   'pr-txt-name
		   'pr-menu-set-txt-title
		   "Text Printers"
		   'pr-txt-printer-menu-modified
		   force
		   "Text Printers"
		   'text 2)
    (let ((save-var pr-ps-utility-menu-modified))
      (pr-menu-alist pr-ps-utility-alist
		     'pr-ps-utility
		     'pr-menu-set-utility-title
		     '("PostScript Print" "File" "PostScript Utility")
		     'save-var
		     force
		     "PostScript Utility"
		     nil 1))
    (pr-menu-alist pr-ps-utility-alist
		   'pr-ps-utility
		   'pr-menu-set-utility-title
		   '("PostScript Preview" "File" "PostScript Utility")
		   'pr-ps-utility-menu-modified
		   force
		   "PostScript Utility"
		   nil 1)
    (pr-even-or-odd-pages ps-even-or-odd-pages force))

  ;; GNU Emacs
  (defun pr-menu-get-item (name-list)
    ;; NAME-LIST is a string or a list of strings.
    (or (listp name-list)
	(setq name-list (list name-list)))
    (and name-list
	 (let* ((reversed (reverse name-list))
		(name (pr-get-symbol (car reversed)))
		(path (nreverse (cdr reversed)))
		(menu (lookup-key
		       global-map
		       (vconcat pr-menu-bar
				(mapcar 'pr-get-symbol path)))))
	   (assq name (nthcdr 2 menu)))))

  ;; GNU Emacs
  (defvar pr-temp-menu nil)

  ;; GNU Emacs
  (defun pr-menu-alist (alist var-sym fun menu-path modified-sym force name
			      entry index)
    (when (and alist (or force (symbol-value modified-sym)))
      (easy-menu-define pr-temp-menu nil ""
	(pr-menu-create name alist var-sym fun entry index))
      (let ((item (pr-menu-get-item menu-path)))
	(and item
	     (let* ((binding     (nthcdr 3 item))
		    (key-binding (cdr binding)))
	       (setcar binding pr-temp-menu)
	       (and key-binding (listp (car key-binding))
		    (setcdr binding (cdr key-binding)))	; skip KEY-BINDING
	       (funcall fun (symbol-value var-sym) item))))
      (set modified-sym nil)))

  ;; GNU Emacs
  (defun pr-menu-set-item-name (item name)
    (and item
	 (setcar (nthcdr 2 item) name))) ; ITEM-NAME

  ;; GNU Emacs
  (defun pr-menu-set-ps-title (value &optional item entry index)
    (pr-menu-set-item-name (or item
			       (pr-menu-get-item "PostScript Printers"))
			   (format "PostScript Printer: %s" value))
    (pr-ps-set-printer value)
    (and index
	 (pr-menu-lock entry index 12 'toggle nil)))

  ;; GNU Emacs
  (defun pr-menu-set-txt-title (value &optional item entry index)
    (pr-menu-set-item-name (or item
			       (pr-menu-get-item "Text Printers"))
			   (format "Text Printer: %s" value))
    (pr-txt-set-printer value)
    (and index
	 (pr-menu-lock entry index 12 'toggle nil)))

  ;; GNU Emacs
  (defun pr-menu-set-utility-title (value &optional item entry index)
    (let ((name (symbol-name value)))
      (if item
	  (pr-menu-set-item-name item name)
	(pr-menu-set-item-name
	 (pr-menu-get-item
	  '("PostScript Print"   "File" "PostScript Utility"))
	 name)
	(pr-menu-set-item-name
	 (pr-menu-get-item
	  '("PostScript Preview" "File" "PostScript Utility"))
	 name)))
    (pr-ps-set-utility value)
    (and index
	 (pr-menu-lock entry index 5 nil '("PostScript Print" "File"))))

  ;; GNU Emacs
  (defun pr-even-or-odd-pages (value &optional no-lock)
    (pr-menu-set-item-name (pr-menu-get-item "Print All Pages")
			   (cdr (assq value pr-even-or-odd-alist)))
    (setq ps-even-or-odd-pages value)
    (or no-lock
	(pr-menu-lock 'postscript-options 8 12 'toggle nil)))

  ))					; end cond featurep


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal Functions (I)


(defun pr-dosify-file-name (path)
  "Replace unix-style directory separator character with dos/windows one."
  (interactive "sPath: ")
  (if (eq pr-path-style 'windows)
      (subst-char-in-string ?/ ?\\ path)
    path))


(defun pr-unixify-file-name (path)
  "Replace dos/windows-style directory separator character with unix one."
  (interactive "sPath: ")
  (if (eq pr-path-style 'windows)
      (subst-char-in-string ?\\ ?/ path)
    path))


(defun pr-standard-file-name (path)
  "Ensure the proper directory separator depending on the OS.
That is, if Emacs is running on DOS/Windows, ensure dos/windows-style directory
separator; otherwise, ensure unix-style directory separator."
  (if (or pr-cygwin-system ps-windows-system)
      (subst-char-in-string ?/ ?\\ path)
    (subst-char-in-string ?\\ ?/ path)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization Functions


(defun pr-alist-custom-set (symbol value)
  "Set the value of custom variables for printer & utility selection."
  (set symbol value)
  (and (featurep 'printing)		; update only after printing is loaded
       (pr-update-menus t)))


(defun pr-ps-utility-custom-set (symbol value)
  "Update utility menu entry."
  (set symbol value)
  (and (featurep 'printing)		; update only after printing is loaded
       (pr-menu-set-utility-title value)))


(defun pr-ps-name-custom-set (symbol value)
  "Update `PostScript Printer:' menu entry."
  (set symbol value)
  (and (featurep 'printing)		; update only after printing is loaded
       (pr-menu-set-ps-title value)))


(defun pr-txt-name-custom-set (symbol value)
  "Update `Text Printer:' menu entry."
  (set symbol value)
  (and (featurep 'printing)		; update only after printing is loaded
       (pr-menu-set-txt-title value)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User Interface


(defgroup printing nil
  "Printing Utilities group."
  :tag "Printing Utilities"
  :link '(emacs-library-link :tag "Source Lisp File" "printing.el")
  :prefix "pr-"
  :version "22.1"
  :group 'wp
  :group 'postscript)


(defcustom pr-path-style
  (if (and (not pr-cygwin-system)
	   ps-windows-system)
      'windows
    'unix)
  "Specify which path style to use for external commands.

Valid values are:

   windows	Windows 9x/NT style (\\)

   unix		Unix style (/)"
  :type '(choice :tag "Path style"
		 (const :tag "Windows 9x/NT Style (\\)" :value windows)
		 (const :tag "Unix Style (/)" :value unix))
  :group 'printing)


(defcustom pr-path-alist
  '((unix    PATH)
    (cygwin  PATH)
    (windows PATH))
  "Specify an alist for command paths.

It's used to find commands used for printing package, like gv, gs, gsview.exe,
mpage, print.exe, etc.  See also `pr-command' function.

Each element has the form:

   (ENTRY DIRECTORY...)

Where:

ENTRY		It's a symbol, used to identify this entry.
		There must exist at least one of the following entries:

		unix	this entry is used when Emacs is running on GNU or
			Unix system.

		cygwin	this entry is used when Emacs is running on Windows
			95/98/NT/2000 with Cygwin.

		windows	this entry is used when Emacs is running on Windows
			95/98/NT/2000.

DIRECTORY	It should be a string or a symbol.  If it's a symbol, it should
		exist an equal entry in `pr-path-alist'.  If it's a string,
		it's considered a directory specification.

		The directory specification may contain:
		$var	environment variable expansion
		~/	tilde expansion
		./	current directory
		../	previous directory

		For example, let's say the home directory is /home/my and the
		current directory is /home/my/dir, so:

		THE ENTRY			IS EXPANDED TO
		~/entry				/home/my/entry
		./entry				/home/my/dir/entry
		../entry			/home/my/entry
		$HOME/entry			/home/my/entry
		$HOME/~/other/../my/entry	/home/my/entry

		SPECIAL SYMBOL: If the symbol `PATH' is used in the directory
		list and there isn't a `PATH' entry in `pr-path-alist' or the
		`PATH' entry has a null directory list, the PATH environment
		variable is used.

Examples:

* On GNU or Unix system:

   '((unix      \".\" \"~/bin\" ghostview mpage PATH)
     (ghostview \"$HOME/bin/gsview-dir\")
     (mpage     \"$HOME/bin/mpage-dir\")
     )

* On Windows system:

   '((windows   \"c:/applications/executables\" PATH ghostview mpage)
     (ghostview \"c:/gs/gsview-dir\")
     (mpage     \"c:/mpage-dir\")
     )"
  :type '(repeat
	  (cons :tag ""
		(symbol :tag "Identifier    ")
		(repeat :tag "Directory List"
			(choice :menu-tag "Directory"
				:tag "Directory"
				(string :value "")
				(symbol :value symbol)))))
  :group 'printing)


(defcustom pr-txt-name 'default
  "Specify a printer for printing a text file.

The printer name symbol should be defined on `pr-txt-printer-alist' (see it for
documentation).

This variable should be modified by customization engine.  If this variable is
modified by other means (for example, a lisp function), use `pr-update-menus'
function (see it for documentation) to update text printer menu."
  :type 'symbol
  :set 'pr-txt-name-custom-set
  :group 'printing)


(defcustom pr-txt-printer-alist
  (list (list 'default lpr-command nil
	      (cond ((boundp 'printer-name) printer-name)
		    (ps-windows-system "PRN")
		    (t nil)
		    )))
  ;; Examples:
  ;; * On GNU or Unix system:
  ;;    '((prt_06a "lpr" nil "prt_06a")
  ;;      (prt_07c nil   nil "prt_07c")
  ;;      )
  ;; * On Windows system:
  ;;    '((prt_06a  "print"     nil "/D:\\\\printers\\prt_06a")
  ;;      (prt_07c  nil         nil "/D:\\\\printers\\prt_07c")
  ;;      (PRN      ""          nil "PRN")
  ;;      (standard "redpr.exe" nil "")
  ;;      )
  "Specify an alist of all text printers (text printer database).

The alist element has the form:

   (SYMBOL COMMAND SWITCHES NAME)

Where:

SYMBOL		It's a symbol to identify a text printer.  It's for
		`pr-txt-name' variable setting and for menu selection.
		Examples:
			'prt_06a
			'my_printer

COMMAND		Name of the program for printing a text file.  On MS-DOS and
		MS-Windows systems, if the value is an empty string, then Emacs
		will write directly to the printer port given by NAME (see text
		below), that is, the NAME should be something like \"PRN\" or
		\"LPT1:\".
		If NAME is something like \"\\\\\\\\host\\\\share-name\" then
		COMMAND shouldn't be an empty string.
		The programs `print' and `nprint' (the standard print programs
		on Windows NT and Novell Netware respectively) are handled
		specially, using NAME as the destination for output; any other
		program is treated like `lpr' except that an explicit filename
		is given as the last argument.
		If COMMAND is nil, it's used the default printing program:
		`print' for Windows system, `lp' for lp system and `lpr' for
		all other systems.  See also `pr-path-alist'.
		Examples:
			\"print\"
			\"lpr\"
			\"lp\"

SWITCHES	List of sexp's to pass as extra options for text printer
		program.  It is recommended to set NAME (see text below)
		instead of including an explicit switch on this list.
		Example:
		   . for lpr
			'(\"-#3\" \"-l\")
			nil

NAME		A string that specifies a text printer name.
		On Unix-like systems, a string value should be a name
		understood by lpr's -P option (or lp's -d option).
		On MS-DOS and MS-Windows systems, it is the name of a printer
		device or port.  Typical non-default settings would be \"LPT1:\"
		to \"LPT3:\" for parallel printers, or \"COM1\" to \"COM4\" or
		\"AUX\" for serial printers, or \"\\\\\\\\hostname\\\\printer\"
		(or \"/D:\\\\\\\\hostname\\\\printer\") for a shared network
		printer.  You can also set it to a name of a file, in which
		case the output gets appended to that file.  If you want to
		discard the printed output, set this to \"NUL\".
		Examples:
		   . for print.exe
			\"/D:\\\\\\\\host\\\\share-name\"
			\"LPT1:\"
			\"PRN\"

		   . for lpr or lp
			\"share-name\"

This variable should be modified by customization engine.  If this variable is
modified by other means (for example, a lisp function), use `pr-update-menus'
function (see it for documentation) to update text printer menu.

Examples:

* On GNU or Unix system:

   '((prt_06a \"lpr\" nil \"prt_06a\")
     (prt_07c nil   nil \"prt_07c\")
     )

* On Windows system:

   '((prt_06a  \"print\"     nil \"/D:\\\\\\\\printers\\\\prt_06a\")
     (prt_07c  nil         nil \"/D:\\\\\\\\printers\\\\prt_07c\")
     (PRN      \"\"          nil \"PRN\")
     (standard \"redpr.exe\" nil \"\")
     )

Useful links:

* Information about the print command (print.exe)
  `http://www.computerhope.com/printhlp.htm'

* RedMon - Redirection Port Monitor (redpr.exe)
  `http://www.cs.wisc.edu/~ghost/redmon/index.htm'

* Redirection Port Monitor (redpr.exe on-line help)
  `http://www.cs.wisc.edu/~ghost/redmon/en/redmon.htm'

* UNIX man pages: lpr (or type `man lpr')
  `http://bama.ua.edu/cgi-bin/man-cgi?lpr'
  `http://www.mediacollege.com/cgi-bin/man/page.cgi?section=all&topic=lpr'

* UNIX man pages: lp (or type `man lp')
  `http://bama.ua.edu/cgi-bin/man-cgi?lp'
  `http://www.mediacollege.com/cgi-bin/man/page.cgi?section=all&topic=lp'
"
  :type '(repeat
	  (list :tag "Text Printer"
		(symbol :tag "Printer Symbol Name")
		(string :tag "Printer Command")
		(repeat :tag "Printer Switches"
			(sexp :tag "Switch" :value ""))
		(choice :menu-tag "Printer Name"
			:tag "Printer Name"
			(const :tag "None" nil)
			string)))
  :set 'pr-alist-custom-set
  :group 'printing)


(defcustom pr-ps-name 'default
  "Specify a printer for printing a PostScript file.

This printer name symbol should be defined on `pr-ps-printer-alist' (see it for
documentation).

This variable should be modified by customization engine.  If this variable is
modified by other means (for example, a lisp function), use `pr-update-menus'
function (see it for documentation) to update PostScript printer menu."
  :type 'symbol
  :set 'pr-ps-name-custom-set
  :group 'printing)


(defcustom pr-ps-printer-alist
  (list (list 'default lpr-command nil
	      (cond (ps-windows-system  nil)
		    (ps-lp-system       "-d")
		    (t                  "-P"))
	      (or (getenv "PRINTER") (getenv "LPDEST") ps-printer-name)))
  ;; Examples:
  ;; * On GNU or Unix system:
  ;;    '((lps_06b "lpr" nil "-P" "lps_06b")
  ;;      (lps_07c "lpr" nil nil  "lps_07c")
  ;;      (lps_08c nil   nil nil  "lps_08c")
  ;;      )
  ;; * On Windows system:
  ;;    '((lps_06a  "print"     nil "/D:" "\\\\printers\\lps_06a")
  ;;      (lps_06b  "print"     nil nil   "\\\\printers\\lps_06b")
  ;;      (lps_07c  "print"     nil ""    "/D:\\\\printers\\lps_07c")
  ;;      (lps_08c  nil         nil nil   "\\\\printers\\lps_08c")
  ;;      (b/w      "gsprint" ("-all" "-twoup") "-printer " "b/w-pr-name")
  ;;      (LPT1     ""          nil ""    "LPT1:")
  ;;      (PRN      ""          nil ""    "PRN")
  ;;      (standard "redpr.exe" nil ""    "")
  ;;      )
  "Specify an alist for all PostScript printers (PostScript printer database).

The alist element has the form:

   (SYMBOL COMMAND SWITCHES PRINTER-SWITCH NAME DEFAULT...)

Where:

SYMBOL		It's a symbol to identify a PostScript printer.  It's for
		`pr-ps-name' variable setting and for menu selection.
		Examples:
			'prt_06a
			'my_printer

COMMAND		Name of the program for printing a PostScript file.  On MS-DOS
		and MS-Windows systems, if the value is an empty string then
		Emacs will write directly to the printer port given by NAME
		(see text below), that is, the NAME should be something like
		\"PRN\" or \"LPT1:\".
		If NAME is something like \"\\\\\\\\host\\\\share-name\" then
		COMMAND shouldn't be an empty string.
		The programs `print' and `nprint' (the standard print programs
		on Windows NT and Novell Netware respectively) are handled
		specially, using NAME as the destination for output; any other
		program is treated like `lpr' except that an explicit filename
		is given as the last argument.
		If COMMAND is nil, it's used the default printing program:
		`print' for Windows system, `lp' for lp system and `lpr' for
		all other systems.  See also `pr-path-alist'.
		Examples:
			\"print\"
			\"lpr\"
			\"lp\"
			\"cp\"
			\"gsprint\"

SWITCHES	List of sexp's to pass as extra options for PostScript printer
		program.  It is recommended to set NAME (see text below)
		instead of including an explicit switch on this list.
		Example:
		   . for lpr
			'(\"-#3\" \"-l\")
			nil

		   . for gsprint.exe
			'(\"-all\" \"-twoup\")

PRINTER-SWITCH	A string that specifies PostScript printer name switch.  If
		it's necessary to have a space between PRINTER-SWITCH and NAME,
		it should be inserted at the end of PRINTER-SWITCH string.
		If PRINTER-SWITCH is nil, it's used the default printer name
		switch: `/D:' for Windows system, `-d' for lp system and `-P'
		for all other systems.
		Examples:
		   . for lpr
			\"-P \"

		   . for lp
			\"-d \"

		   . for print.exe
			\"/D:\"

		   . for gsprint.exe
			\"-printer \"

NAME		A string that specifies a PostScript printer name.
		On Unix-like systems, a string value should be a name
		understood by lpr's -P option (or lp's -d option).
		On MS-DOS and MS-Windows systems, it is the name of a printer
		device or port.  Typical non-default settings would be \"LPT1:\"
		to \"LPT3:\" for parallel printers, or \"COM1\" to \"COM4\" or
		\"AUX\" for serial printers, or \"\\\\\\\\hostname\\\\printer\"
		(or \"/D:\\\\\\\\hostname\\\\printer\") for a shared network
		printer.  You can also set it to a name of a file, in which
		case the output gets appended to that file.  If you want to
		discard the printed output, set this to \"NUL\".
		Examples:
		   . for cp.exe
			\"\\\\\\\\host\\\\share-name\"

		   . for print.exe or gsprint.exe
			\"/D:\\\\\\\\host\\\\share-name\"
			\"\\\\\\\\host\\\\share-name\"
			\"LPT1:\"
			\"PRN\"

		   . for lpr or lp
			\"share-name\"

DEFAULT		It's a way to set default values when this entry is selected.
		It's a cons like:

		   (VARIABLE . VALUE)

		Which associates VARIABLE with VALUE.  When this entry is
		selected, it's executed the following command:

		   (set VARIABLE (eval VALUE))

		Note that VALUE can be any valid lisp expression.  So, don't
		forget to quote symbols and constant lists.
		If VARIABLE is the special keyword `inherits-from:', VALUE must
		be a symbol name setting defined in `pr-setting-database' from
		which the current setting inherits the context.  Take care with
		circular inheritance.
		Examples:
			'(ps-landscape-mode . nil)
			'(ps-spool-duplex . t)
			'(pr-gs-device . (my-gs-device t))

This variable should be modified by customization engine.  If this variable is
modified by other means (for example, a lisp function), use `pr-update-menus'
function (see it for documentation) to update PostScript printer menu.

Examples:

* On GNU or Unix system:

   '((lps_06b \"lpr\" nil \"-P\" \"lps_06b\")
     (lps_07c \"lpr\" nil nil  \"lps_07c\")
     (lps_08c nil   nil nil  \"lps_08c\")
     )

* On Windows system:

   '((lps_06a  \"print\"     nil \"/D:\" \"\\\\\\\\printers\\\\lps_06a\")
     (lps_06b  \"print\"     nil nil   \"\\\\\\\\printers\\\\lps_06b\")
     (lps_07c  \"print\"     nil \"\"    \"/D:\\\\\\\\printers\\\\lps_07c\")
     (lps_08c  nil         nil nil   \"\\\\\\\\printers\\\\lps_08c\")
     (b/w1 \"gsprint\" (\"-all\" \"-twoup\") \"-printer \" \"b/w-pr-name\")
     (b/w2 \"gsprint\" (\"-all\" \"-twoup\") nil \"-printer \\\\\\\\printers\\\\lps_06a\")
     (LPT1     \"\"          nil \"\"    \"LPT1:\")
     (PRN      \"\"          nil \"\"    \"PRN\")
     (standard \"redpr.exe\" nil \"\"    \"\")
     )


gsprint:

You can use gsprint instead of ghostscript to print monochrome PostScript files
in Windows.  The gsprint utility documentation says that it is more efficient
than ghostscript to print monochrome PostScript.

To print non-monochrome PostScript file, the efficiency of ghostscript is
similar to gsprint.

Also the gsprint utility comes together with gsview distribution.

As an example of gsprint declaration:

   (setq pr-ps-printer-alist
	 '((A \"gsprint\" (\"-all\" \"-twoup\") \"-printer \" \"lps_015\")
	   (B \"gsprint\" (\"-all\" \"-twoup\") nil \"-printer lps_015\")
	   ;; some other printer declaration
	   ))

The example above declares that printer A prints all pages (-all) and two pages
per sheet (-twoup).  The printer B declaration does the same as the printer A
declaration, the only difference is the printer name selection.

There are other command line options like:

   -mono	Render in monochrome as 1bit/pixel (only black and white).
   -grey	Render in greyscale as 8bits/pixel.
   -color	Render in color as 24bits/pixel.

The default is `-mono'.  So, printer A and B in the example above are using
implicitly the `-mono' option.  Note that in `-mono' no gray tone or color is
printed, this includes the zebra stripes, that is, in `-mono' the zebra stripes
are not printed.


Useful links:

* GSPRINT - Ghostscript print to Windows printer
  `http://www.cs.wisc.edu/~ghost/gsview/gsprint.htm'

* Introduction to Ghostscript
  `http://www.cs.wisc.edu/~ghost/doc/intro.htm'

* How to use Ghostscript
  `http://www.cs.wisc.edu/~ghost/doc/cvs/Use.htm'

* Information about the print command (print.exe)
  `http://www.computerhope.com/printhlp.htm'

* RedMon - Redirection Port Monitor (redpr.exe)
  `http://www.cs.wisc.edu/~ghost/redmon/index.htm'

* Redirection Port Monitor (redpr.exe on-line help)
  `http://www.cs.wisc.edu/~ghost/redmon/en/redmon.htm'

* UNIX man pages: lpr (or type `man lpr')
  `http://bama.ua.edu/cgi-bin/man-cgi?lpr'
  `http://www.mediacollege.com/cgi-bin/man/page.cgi?section=all&topic=lpr'

* UNIX man pages: lp (or type `man lp')
  `http://bama.ua.edu/cgi-bin/man-cgi?lp'
  `http://www.mediacollege.com/cgi-bin/man/page.cgi?section=all&topic=lp'

* GNU utilities for Win32 (cp.exe)
  `http://unxutils.sourceforge.net/'
"
  :type '(repeat
	  (list
	   :tag "PostScript Printer"
	   (symbol :tag "Printer Symbol Name")
	   (string :tag "Printer Command")
	   (repeat :tag "Printer Switches"
		   (sexp :tag "Switch" :value ""))
	   (choice :menu-tag "Printer Name Switch"
		   :tag "Printer Name Switch"
		   (const :tag "None" nil)
		   string)
	   (choice :menu-tag "Printer Name"
		   :tag "Printer Name"
		   (const :tag "None" nil)
		   string)
	   (repeat
	    :tag "Default Value List"
	    :inline t
	    (cons
	     :tag ""
	     (choice
	      :menu-tag "Variable"
	      :tag "Variable"
	      (const :tag "Landscape"              ps-landscape-mode)
	      (const :tag "Print Header"           ps-print-header)
	      (const :tag "Print Header Frame"     ps-print-header-frame)
	      (const :tag "Line Number"            ps-line-number)
	      (const :tag "Zebra Stripes"          ps-zebra-stripes)
	      (const :tag "Duplex"                 ps-spool-duplex)
	      (const :tag "Tumble"                 ps-spool-tumble)
	      (const :tag "Upside-Down"            ps-print-upside-down)
	      (const :tag "PS File Landscape"      pr-file-landscape)
	      (const :tag "PS File Duplex"         pr-file-duplex)
	      (const :tag "PS File Tumble"         pr-file-tumble)
	      (const :tag "Auto Region"            pr-auto-region)
	      (const :tag "Auto Mode"              pr-auto-mode)
	      (const :tag "Ghostscript Device"     pr-gs-device)
	      (const :tag "Ghostscript Resolution" pr-gs-resolution)
	      (const :tag "inherits-from:"         inherits-from:)
	      (variable :tag "Other"))
	     (sexp :tag "Value")))
	   ))
  :set 'pr-alist-custom-set
  :group 'printing)


(defcustom pr-temp-dir
  (pr-dosify-file-name
   (if (boundp 'temporary-file-directory)
       (symbol-value 'temporary-file-directory)
     ;; hacked from `temporary-file-directory' variable in files.el
     (file-name-as-directory
      (or (getenv "TMPDIR") (getenv "TMP") (getenv "TEMP")
	  (cond (ps-windows-system "c:/temp")
		(t "/tmp")
		)))))
  "Specify a directory for temporary files during printing.

See also `pr-ps-temp-file' and `pr-file-modes'."
  :type '(directory :tag "Temporary Directory")
  :group 'printing)


(defcustom pr-ps-temp-file "prspool-"
  "Specify PostScript temporary file name prefix.

See also `pr-temp-dir' and `pr-file-modes'."
  :type '(file :tag "PostScript Temporary File Name")
  :group 'printing)


;; It uses 0600 as default instead of (default-file-modes).
;; So, by default, only the session owner have permission to deal with files
;; generated by `printing'.
(defcustom pr-file-modes ?\600
  "Specify the file permission bits for newly created files.

It should be an integer; only the low 9 bits are used.

See also `pr-temp-dir' and `pr-ps-temp-file'."
  :type '(integer :tag "File Permission Bits")
  :group 'printing)


(defcustom pr-gv-command
  (if ps-windows-system
      "gsview32.exe"
    "gv")
  "Specify path and name of the gsview/gv utility.

See also `pr-path-alist'.

Useful links:

* GNU gv manual
  `http://www.gnu.org/software/gv/manual/gv.html'

* GSview Help
  `http://www.cs.wisc.edu/~ghost/gsview/gsviewen.htm'

* GSview Help - Common Problems
  `http://www.cs.wisc.edu/~ghost/gsview/gsviewen.htm#Common_Problems'

* GSview Readme (compilation & installation)
  `http://www.cs.wisc.edu/~ghost/gsview/Readme.htm'

* GSview (main site)
  `http://www.cs.wisc.edu/~ghost/gsview/index.htm'

* Ghostscript, Ghostview and GSview
  `http://www.cs.wisc.edu/~ghost/'

* Ghostview
  `http://www.cs.wisc.edu/~ghost/gv/index.htm'

* gv 3.5, June 1997
  `http://www.cs.wisc.edu/~ghost/gv/gv_doc/gv.html'

* MacGSView (MacOS)
  `http://www.cs.wisc.edu/~ghost/macos/index.htm'
"
  :type '(string :tag "Ghostview Utility")
  :group 'printing)


(defcustom pr-gs-command
  (if ps-windows-system
      "gswin32.exe"
    "gs")
  "Specify path and name of the ghostscript utility.

See also `pr-path-alist'.

Useful links:

* Ghostscript, Ghostview and GSview
  `http://www.cs.wisc.edu/~ghost/'

* Introduction to Ghostscript
  `http://www.cs.wisc.edu/~ghost/doc/intro.htm'

* How to use Ghostscript
  `http://www.cs.wisc.edu/~ghost/doc/cvs/Use.htm'

* Printer compatibility
  `http://www.cs.wisc.edu/~ghost/doc/printer.htm'
"
  :type '(string :tag "Ghostscript Utility")
  :group 'printing)


(defcustom pr-gs-switches
  (if ps-windows-system
      '("-q -dNOPAUSE -Ic:/gs/gs5.50;c:/gs/gs5.50/fonts")
    '("-q -dNOPAUSE -I/usr/share/ghostscript/5.10"))
  "Specify ghostscript switches.  See the documentation on GS for more info.

It's a list of strings, where each string is one or more ghostscript switches.

A note on the gs switches:

-q					quiet
-dNOPAUSE				don't wait for user intervention
-Ic:/gs/gs5.50;c:/gs/gs5.50/fonts	the directories needed for gs
-c quit					it's added at the end to terminate gs

To see ghostscript documentation for more information:

* On GNU or Unix system:
   - for full documentation, type: man gs
   - for brief documentation, type: gs -h

* On Windows system:
   - for full documentation, see in a browser the file
     c:/gstools/gs5.50/index.html, that is, the file index.html which is
     located in the same directory as gswin32.exe.
   - for brief documentation, type: gswin32.exe -h

Useful links:

* Introduction to Ghostscript
  `http://www.cs.wisc.edu/~ghost/doc/intro.htm'

* How to use Ghostscript
  `http://www.cs.wisc.edu/~ghost/doc/cvs/Use.htm'

* Printer compatibility
  `http://www.cs.wisc.edu/~ghost/doc/printer.htm'
"
  :type '(repeat (string :tag "Ghostscript Switch"))
  :group 'printing)


(defcustom pr-gs-device
  (if ps-windows-system
      "mswinpr2"
    "uniprint")
  "Specify the ghostscript device switch value (-sDEVICE=).

A note on the gs switches:

-sDEVICE=djet500	the printer - works with HP DeskJet 540

See `pr-gs-switches' for documentation.
See also `pr-ps-printer-alist'."
  :type '(string :tag "Ghostscript Device")
  :group 'printing)


(defcustom pr-gs-resolution 300
  "Specify ghostscript resolution switch value (-r).

A note on the gs switches:

-r300	resolution 300x300

See `pr-gs-switches' for documentation.
See also `pr-ps-printer-alist'."
  :type '(integer :tag "Ghostscript Resolution")
  :group 'printing)


(defcustom pr-print-using-ghostscript nil
  "Non-nil means print using ghostscript.

This is useful if you don't have a PostScript printer, so you could use the
ghostscript to print a PostScript file.

In GNU or Unix system, if ghostscript is set as a PostScript filter, this
variable should be nil."
  :type 'boolean
  :group 'printing)


(defcustom pr-faces-p nil
  "Non-nil means print with face attributes."
  :type 'boolean
  :group 'printing)


(defcustom pr-spool-p nil
  "Non-nil means spool printing in a buffer."
  :type 'boolean
  :group 'printing)


(defcustom pr-file-landscape nil
  "Non-nil means print PostScript file in landscape orientation."
  :type 'boolean
  :group 'printing)


(defcustom pr-file-duplex nil
  "Non-nil means print PostScript file in duplex mode."
  :type 'boolean
  :group 'printing)


(defcustom pr-file-tumble nil
  "Non-nil means print PostScript file in tumble mode.

If tumble is off, produces a printing suitable for binding on the left or
right.
If tumble is on, produces a printing suitable for binding at the top or
bottom."
  :type 'boolean
  :group 'printing)


(defcustom pr-auto-region t
  "Non-nil means region is automagically detected.

Note that this will only work if you're using transient mark mode.

When this variable is non-nil, the `*-buffer*' commands will behave like
`*-region*' commands, that is, `*-buffer*' commands will print only the region
marked instead of all buffer."
  :type 'boolean
  :group 'printing)


(defcustom pr-auto-mode t
  "Non-nil means major-mode specific printing is preferred over normal printing.

That is, if current major-mode is declared in `pr-mode-alist', the `*-buffer*'
and `*-region*' commands will behave like `*-mode*' commands; otherwise,
`*-buffer*' commands will print the current buffer and `*-region*' commands
will print the current region."
  :type 'boolean
  :group 'printing)


(defcustom pr-mode-alist
  '((mh-folder-mode			; mh summary buffer
     pr-mh-lpr-1  pr-mh-print-1
     2
     (ps-article-author ps-article-subject)
     ("/pagenumberstring load" pr-article-date)
     nil
     )
    (mh-letter-mode			; mh letter buffer
     pr-mh-lpr-2  pr-mh-print-2
     2
     (ps-article-author ps-article-subject)
     ("/pagenumberstring load" pr-article-date)
     nil
     )
    (rmail-summary-mode			; rmail summary buffer
     pr-rmail-lpr pr-rmail-print
     3
     (ps-article-subject ps-article-author buffer-name)
     nil
     nil
     )
    (rmail-mode				; rmail buffer
     pr-rmail-lpr pr-rmail-print
     3
     (ps-article-subject ps-article-author buffer-name)
     nil
     nil
     )
    (gnus-summary-mode			; gnus summary buffer
     pr-gnus-lpr  pr-gnus-print
     3
     (ps-article-subject ps-article-author gnus-newsgroup-name)
     nil
     nil
     )
    (gnus-article-mode			; gnus article buffer
     pr-gnus-lpr  pr-gnus-print
     3
     (ps-article-subject ps-article-author gnus-newsgroup-name)
     nil
     nil
     )
    (Info-mode				; Info buffer
     pr-mode-lpr  pr-mode-print
     2
     (ps-info-node ps-info-file)
     nil
     nil
     )
    (vm-mode				; vm mode
     pr-vm-lpr  pr-vm-print
     3
     (ps-article-subject ps-article-author buffer-name)
     nil
     nil
     )
    )
  "Specify an alist for a major-mode and printing functions.

To customize a major mode printing, just declare the customization in
`pr-mode-alist' and invoke some of `*-mode*' commands.  An example for major
mode usage is when you're using gnus (or mh, or rmail, etc.) and you're in the
*Summary* buffer, if you forget to switch to the *Article* buffer before
printing, you'll get a nicely formatted list of article subjects shows up at
the printer.  With major mode printing you don't need to switch from gnus
*Summary* buffer first.

The elements have the following form:

   (MAJOR-MODE
    LPR-PRINT PS-PRINT
    HEADER-LINES
    LEFT-HEADER
    RIGHT-HEADER
    KILL-LOCAL-VARIABLE
    DEFAULT...)

Where:

MAJOR-MODE	It's the major mode symbol.

LPR-PRINT	It's a symbol function for text printing.  It's invoked with
		one argument:
		(HEADER-LINES  LEFT-HEADER  RIGHT-HEADER DEFAULT...).

		Usually LPR-PRINT function prepares the environment or buffer
		and then call the function `pr-mode-lpr' which it's used to
		process the buffer and send it to text printer.

		The `pr-mode-lpr' definition is:

		(pr-mode-lpr HEADER-LIST &optional FROM TO)

		Where HEADER-LIST is like the argument passed to LPR-PRINT.
		FROM and TO are the beginning and end markers, respectively,
		for a region.  If FROM is nil, it's used (point-min); if TO is
		nil, it's used (point-max).

PS-PRINT	It's a symbol function for PostScript printing.  It's invoked
		with three arguments: n-up printing, file name and the list:
		(HEADER-LINES  LEFT-HEADER  RIGHT-HEADER DEFAULT...).

		Usually PS-PRINT function prepares the environment or buffer
		and then call the function `pr-mode-print' which it's used to
		process the buffer and send it to PostScript printer.

		The `pr-mode-print' definition is:

		(pr-mode-print N-UP FILENAME HEADER-LIST &optional FROM TO)

		Where N-UP, FILENAME and HEADER-LIST are like the arguments
		passed to PS-PRINT.  FROM and TO are the beginning and end
		markers, respectively, for a region.  If TO is nil, it's used
		(point-max).

HEADER-LINES	It's the number of header lines; if is nil, it uses
		`ps-header-lines' value.

LEFT-HEADER	It's the left header part, it's a list of string, variable
		symbol or function symbol (with no argument); if is nil, it
		uses `ps-left-header' value.

RIGHT-HEADER	It's the right header part, it's a list of string, variable
		symbol or function symbol (with no argument); if is nil, it
		uses `ps-right-header' value.

KILL-LOCAL-VARIABLE
		Non-nil means to kill all buffer local variable declared in
		DEFAULT (see below).

DEFAULT		It's a way to set default values when this entry is selected.
		It's a cons like:

		   (VARIABLE-SYM . VALUE)

		Which associates VARIABLE-SYM with VALUE.  When this entry is
		selected, it's executed the following command:

		   (set (make-local-variable VARIABLE-SYM) (eval VALUE))

		Note that VALUE can be any valid lisp expression.  So, don't
		forget to quote symbols and constant lists.
		If VARIABLE is the special keyword `inherits-from:', VALUE must
		be a symbol name setting defined in `pr-setting-database' from
		which the current setting inherits the context.  Take care with
		circular inheritance.
		Examples:
			'(ps-landscape-mode . nil)
			'(ps-spool-duplex . t)
			'(pr-gs-device . (my-gs-device t))"
  :type '(repeat
	  (list
	   :tag ""
	   (symbol :tag "Major Mode")
	   (function :tag "Text Printing Function")
	   (function :tag "PS Printing Function")
	   (choice :menu-tag "Number of Header Lines"
		   :tag "Number of Header Lines"
		   (integer :tag "Number")
		   (const :tag "Default Number" nil))
	   (repeat :tag "Left Header List"
		   (choice :menu-tag "Left Header"
			   :tag "Left Header"
			   string symbol))
	   (repeat :tag "Right Header List"
		   (choice :menu-tag "Right Header"
			   :tag "Right Header"
			   string symbol))
	   (boolean :tag "Kill Local Variable At End")
	   (repeat
	    :tag "Default Value List"
	    :inline t
	    (cons
	     :tag ""
	     (choice
	      :menu-tag "Variable"
	      :tag "Variable"
	      (const :tag "Landscape"              ps-landscape-mode)
	      (const :tag "Print Header"           ps-print-header)
	      (const :tag "Print Header Frame"     ps-print-header-frame)
	      (const :tag "Line Number"            ps-line-number)
	      (const :tag "Zebra Stripes"          ps-zebra-stripes)
	      (const :tag "Duplex"                 ps-spool-duplex)
	      (const :tag "Tumble"                 ps-spool-tumble)
	      (const :tag "Upside-Down"            ps-print-upside-down)
	      (const :tag "PS File Landscape"      pr-file-landscape)
	      (const :tag "PS File Duplex"         pr-file-duplex)
	      (const :tag "PS File Tumble"         pr-file-tumble)
	      (const :tag "Auto Region"            pr-auto-region)
	      (const :tag "Auto Mode"              pr-auto-mode)
	      (const :tag "Ghostscript Device"     pr-gs-device)
	      (const :tag "Ghostscript Resolution" pr-gs-resolution)
	      (const :tag "inherits-from:"         inherits-from:)
	      (variable :tag "Other"))
	     (sexp :tag "Value")))
	   ))
  :group 'printing)


(defcustom pr-ps-utility 'mpage
  "Specify PostScript utility symbol.

This utility symbol should be defined on `pr-ps-utility-alist' (see it for
documentation).

This variable should be modified by customization engine.  If this variable is
modified by other means (for example, a lisp function), use `pr-update-menus'
function (see it for documentation) to update PostScript utility menu.

NOTE: Don't forget to download and install the utilities declared on
      `pr-ps-utility-alist'."
  :type '(symbol :tag "PS File Utility")
  :set 'pr-ps-utility-custom-set
  :group 'printing)


(defcustom pr-ps-utility-alist
  '((mpage "mpage" nil    "-b%s" "-%d" "-l" "-t" "-T" ">" nil)
    (psnup "psnup" ("-q") "-P%s" "-%d" "-l" nil  nil  " " nil
	   (inherits-from: . no-duplex))
    )
  ;; Examples:
  ;; * On GNU or Unix system:
  ;;    '((mpage "mpage" nil    "-b%s" "-%d" "-l" "-t" "-T" ">" nil)
  ;;      (psnup "psnup" ("-q") "-P%s" "-%d" "-l" nil  nil  " " nil
  ;;             (pr-file-duplex . nil) (pr-file-tumble . nil))
  ;;      )
  ;; * On Windows system:
  ;;    '((psnup "c:/psutils/psnup" ("-q") "-P%s" "-%d" "-l" nil  nil " " nil
  ;;             (pr-file-duplex . nil) (pr-file-tumble . nil))
  ;;      )
  "Specify an alist for PostScript utility processing (PS utility database).

The alist element has the form:

   (SYMBOL UTILITY MUST-SWITCHES PAPERSIZE N-UP LANDSCAPE DUPLEX TUMBLE OUTPUT
	   SWITCHES DEFAULT...)

Where:

SYMBOL		It's a symbol to identify a PostScript utility.  It's for
		`pr-ps-utility' variable setting and for menu selection.
		Examples:
			'mpage
			'psnup

UTILITY		Name of utility for processing a PostScript file.
		See also `pr-path-alist'.
		Examples:
		    . for GNU or Unix system:
			\"mpage\"
			\"psnup -q\"

		    . for Windows system:
			\"c:/psutils/psnup -q\"

MUST-SWITCHES	List of sexp's to pass as options to the PostScript utility
		program.  These options are necessary to process the utility
		program and must be placed before any other switches.
		Example:
		    . for psnup:
			'(\"-q\")

PAPERSIZE	It's a format string to specify paper size switch.
		Example:
		    . for mpage
			\"-b%s\"

N-UP		It's a format string to specify n-up switch.
		Example:
		    . for psnup
			\"-%d\"

LANDSCAPE	It's a string to specify landscape switch.  If the utility
		doesn't have landscape switch, set to nil.
		Example:
		    . for psnup
			\"-l\"

DUPLEX		It's a string to specify duplex switch.  If the utility doesn't
		have duplex switch, set to nil.
		Example:
		    . for psnup
			nil

TUMBLE		It's a string to specify tumble switch.  If the utility doesn't
		have tumble switch, set to nil.
		Example:
		    . for psnup
			nil

OUTPUT		It's a string to specify how to generate an output file.  Some
		utilities accept an output file option, but some others need
		output redirection or some other way to specify an output file.
		Example:
		    . for psnup
			\" \" ; psnup ... input output

		    . for mpage
			\">\" ; mpage ... input > output

SWITCHES	List of sexp's to pass as extra options to the PostScript utility
		program.
		Example:
		    . for psnup
			'(\"-q\")
			nil

DEFAULT		It's a way to set default values when this entry is selected.
		It's a cons like:

		   (VARIABLE . VALUE)

		Which associates VARIABLE with VALUE.  When this entry is
		selected, it's executed the following command:

		   (set VARIABLE (eval VALUE))

		Note that VALUE can be any valid lisp expression.  So, don't
		forget to quote symbols and constant lists.
		If VARIABLE is the special keyword `inherits-from:', VALUE must
		be a symbol name setting defined in `pr-setting-database' from
		which the current setting inherits the context.  Take care with
		circular inheritance.
		Examples:
			'(pr-file-landscape . nil)
			'(pr-file-duplex . t)
			'(pr-gs-device . (my-gs-device t))

This variable should be modified by customization engine.  If this variable is
modified by other means (for example, a lisp function), use `pr-update-menus'
function (see it for documentation) to update PostScript utility menu.

NOTE: Don't forget to download and install the utilities declared on
      `pr-ps-utility-alist'.

Examples:

* On GNU or Unix system:

   '((mpage \"mpage\" nil    \"-b%s\" \"-%d\" \"-l\" \"-t\" \"-T\" \">\" nil)
     (psnup \"psnup\" (\"-q\") \"-P%s\" \"-%d\" \"-l\" nil  nil  \" \" nil
	    (pr-file-duplex . nil) (pr-file-tumble . nil))
     )

* On Windows system:

   '((psnup \"c:/psutils/psnup\" (\"-q\") \"-P%s\" \"-%d\" \"-l\" nil nil \" \"
	    nil (pr-file-duplex . nil) (pr-file-tumble . nil))
     )

Useful links:

* mpage download (GNU or Unix)
  `http://www.mesa.nl/pub/mpage/'

* mpage documentation (GNU or Unix - or type `man mpage')
  `http://www.cs.umd.edu/faq/guides/manual_unix/node48.html'
  `http://www.rt.com/man/mpage.1.html'

* psnup (Windows, GNU or Unix)
  `http://www.knackered.org/angus/psutils/'
  `http://gershwin.ens.fr/vdaniel/Doc-Locale/Outils-Gnu-Linux/PsUtils/'

* psnup (PsUtils for Windows)
  `http://gnuwin32.sourceforge.net/packages/psutils.htm'

* psnup documentation (GNU or Unix - or type `man psnup')
  `http://linux.about.com/library/cmd/blcmdl1_psnup.htm'
  `http://amath.colorado.edu/computing/software/man/psnup.html'

* GNU Enscript (Windows, GNU or Unix)
  `http://people.ssh.com/mtr/genscript/'

* GNU Enscript documentation (Windows, GNU or Unix)
  `http://people.ssh.com/mtr/genscript/enscript.man.html'
  (on GNU or Unix, type `man enscript')
"
  :type '(repeat
	  (list :tag "PS File Utility"
		(symbol :tag "Utility Symbol")
		(string :tag "Utility Name")
		(repeat :tag "Must Utility Switches"
			(sexp :tag "Switch" :value ""))
		(choice :menu-tag "Paper Size"
			:tag "Paper Size"
			(const :tag "No Paper Size" nil)
			(string :tag "Paper Size Format"))
		(choice :menu-tag "N-Up"
			:tag "N-Up"
			(const :tag "No N-Up" nil)
			(string :tag "N-Up Format"))
		(choice :menu-tag "Landscape"
			:tag "Landscape"
			(const :tag "No Landscape" nil)
			(string :tag "Landscape Switch"))
		(choice :menu-tag "Duplex"
			:tag "Duplex"
			(const :tag "No Duplex" nil)
			(string :tag "Duplex Switch"))
		(choice :menu-tag "Tumble"
			:tag "Tumble"
			(const :tag "No Tumble" nil)
			(string :tag "Tumble Switch"))
		(string :tag "Output Separator")
		(repeat :tag "Utility Switches"
			(sexp :tag "Switch" :value ""))
		(repeat
		 :tag "Default Value List"
		 :inline t
		 (cons
		  :tag ""
		  (choice
		   :menu-tag "Variable"
		   :tag "Variable"
		   (const :tag "PS File Landscape"      pr-file-landscape)
		   (const :tag "PS File Duplex"         pr-file-duplex)
		   (const :tag "PS File Tumble"         pr-file-tumble)
		   (const :tag "Ghostscript Device"     pr-gs-device)
		   (const :tag "Ghostscript Resolution" pr-gs-resolution)
		   (const :tag "inherits-from:"         inherits-from:)
		   (variable :tag "Other"))
		  (sexp :tag "Value")))
		))
  :set 'pr-alist-custom-set
  :group 'printing)


(defcustom pr-menu-lock t
  "Non-nil means menu is locked while selecting toggle options.

See also `pr-menu-char-height' and `pr-menu-char-width'."
  :type 'boolean
  :group 'printing)


(defcustom pr-menu-char-height (pr-menu-char-height)
  "Specify menu char height in pixels.

This variable is used to guess which vertical position should be locked the
menu, so don't forget to adjust it if menu position is not ok.

See also `pr-menu-lock' and `pr-menu-char-width'."
  :type 'integer
  :group 'printing)


(defcustom pr-menu-char-width (pr-menu-char-width)
  "Specify menu char width in pixels.

This variable is used to guess which horizontal position should be locked the
menu, so don't forget to adjust it if menu position is not ok.

See also `pr-menu-lock' and `pr-menu-char-height'."
  :type 'integer
  :group 'printing)


(defcustom pr-setting-database
  '((no-duplex				; setting symbol name
     nil nil nil			; inherits  local  kill-local
     (pr-file-duplex . nil)		; settings
     (pr-file-tumble . nil))
    )
  "Specify an alist for settings in general.

The elements have the following form:

   (SYMBOL INHERITS LOCAL KILL-LOCAL SETTING...)

Where:

SYMBOL		It's a symbol to identify the setting group.

INHERITS	Specify the inheritance for SYMBOL group.  It's a symbol name
		setting from which the current setting inherits the context.
		If INHERITS is nil, means that there is no inheritance.
		This is a simple inheritance mechanism.

		Let's see an example to illustrate the inheritance mechanism:

		(setq pr-setting-database
		      '((no-duplex	; setting symbol name
			 nil		; inherits
			 nil nil	; local  kill-local
			 (pr-file-duplex . nil) ; settings
			 (pr-file-tumble . nil)
			 )
			(no-duplex-and-landscape ; setting symbol name
			 no-duplex	; inherits
			 nil nil	; local  kill-local
			 (pr-file-landscape . nil) ; settings
			 )))

		The example above has two setting groups: no-duplex and
		no-duplex-and-landscape.  When setting no-duplex is activated
		through `inherits-from:' (see `pr-ps-utility', `pr-mode-alist'
		and `pr-ps-printer-alist'), the variables pr-file-duplex and
		pr-file-tumble are both set to nil.

		Now when setting no-duplex-and-landscape is activated through
		`inherits-from:', the variable pr-file-landscape is set to nil
		and also the settings for no-duplex are done, because
		no-duplex-and-landscape inherits settings from no-duplex.

		Take care with circular inheritance.  It's an error if circular
		inheritance happens.

LOCAL		Non-nil means that all settings for SYMBOL group will be
		declared local buffer.

KILL-LOCAL	Non-nil means that all settings for SYMBOL group will be
		killed at end.  It has effect only when LOCAL is non-nil.

SETTING		It's a cons like:

		   (VARIABLE . VALUE)

		Which associates VARIABLE with VALUE.  When this entry is
		selected, it's executed the following command:

		  * If LOCAL is non-nil:
		   (set (make-local-variable VARIABLE) (eval VALUE))

		  * If LOCAL is nil:
		   (set VARIABLE (eval VALUE))

		Note that VALUE can be any valid lisp expression.  So, don't
		forget to quote symbols and constant lists.
		This setting is ignored if VARIABLE is equal to keyword
		`inherits-from:'.
		Examples:
			'(ps-landscape-mode . nil)
			'(ps-spool-duplex . t)
			'(pr-gs-device . (my-gs-device t))"
  :type '(repeat
	  (list
	   :tag ""
	   (symbol :tag "Setting Name")
	   (choice :menu-tag "Inheritance"
		   :tag "Inheritance"
		   (const :tag "No Inheritance" nil)
		   (symbol :tag "Inherits From"))
	   (boolean :tag "Local Buffer Setting")
	   (boolean :tag "Kill Local Variable At End")
	   (repeat
	    :tag "Setting List"
	    :inline t
	    (cons
	     :tag ""
	     (choice
	      :menu-tag "Variable"
	      :tag "Variable"
	      (const :tag "Landscape"              ps-landscape-mode)
	      (const :tag "Print Header"           ps-print-header)
	      (const :tag "Print Header Frame"     ps-print-header-frame)
	      (const :tag "Line Number"            ps-line-number)
	      (const :tag "Zebra Stripes"          ps-zebra-stripes)
	      (const :tag "Duplex"                 ps-spool-duplex)
	      (const :tag "Tumble"                 ps-spool-tumble)
	      (const :tag "Upside-Down"            ps-print-upside-down)
	      (const :tag "PS File Landscape"      pr-file-landscape)
	      (const :tag "PS File Duplex"         pr-file-duplex)
	      (const :tag "PS File Tumble"         pr-file-tumble)
	      (const :tag "Auto Region"            pr-auto-region)
	      (const :tag "Auto Mode"              pr-auto-mode)
	      (const :tag "Ghostscript Device"     pr-gs-device)
	      (const :tag "Ghostscript Resolution" pr-gs-resolution)
	      (variable :tag "Other"))
	     (sexp :tag "Value")))
	   ))
  :group 'printing)


(defcustom pr-visible-entry-list
  '(postscript text postscript-options postscript-process printing help)
  "Specify a list of Printing menu visible entries.

Valid values with the corresponding menu parts are:

			      +------------------------------+
			      |    Printing Interface        |
			      +------------------------------+
   `postscript'		      |    PostScript Preview       >|
			      |    PostScript Print         >|
			      |    PostScript Printer: name >|
			      +------------------------------+
   `text'		      |    Printify                 >|
			      |    Print                    >|
			      |    Text Printer: name       >|
			      +------------------------------+
   `postscript-options'	      |[ ] Landscape                 |
			      |[ ] Print Header              |
			      |[ ] Print Header Frame        |
			      |[ ] Line Number               |
			      |[ ] Zebra Stripes             |
			      |[ ] Duplex                    |
			      |[ ] Tumble                    |
			      |[ ] Upside-Down               |
			      |    Print All Pages          >|
			      +------------------------------+
   `postscript-process'	      |[ ] Spool Buffer              |
			      |[ ] Print with faces          |
			      |[ ] Print via Ghostscript     |
			      +------------------------------+
   `printing'		      |[ ] Auto Region               |
			      |[ ] Auto Mode                 |
			      |[ ] Menu Lock                 |
			      +------------------------------+
   `help'		      |    Customize                >|
			      |    Show Settings            >|
			      |    Help                      |
			      +------------------------------+

Any other value is ignored."
  :type '(repeat :tag "Menu Visible Part"
		 (choice :menu-tag "Menu Part"
			 :tag "Menu Part"
			 (const postscript)
			 (const text)
			 (const postscript-options)
			 (const postscript-process)
			 (const printing)
			 (const help)))
  :group 'printing)


(defcustom pr-delete-temp-file t
  "Non-nil means delete temporary files.

Set `pr-delete-temp-file' to nil, if the following message (or a similar)
happens when printing:

   Error: could not open \"c:\\temp\\prspool.ps\" for reading."
  :type 'boolean
  :group 'printing)


(defcustom pr-list-directory nil
  "Non-nil means list directory when processing a directory.

That is, any subdirectories (and the superdirectory) of the directory (given as
argument of functions below) are also printed (as dired-mode listings).

It's used by `pr-ps-directory-preview', `pr-ps-directory-using-ghostscript',
`pr-ps-directory-print', `pr-ps-directory-ps-print', `pr-printify-directory'
and `pr-txt-directory'."
  :type 'boolean
  :group 'printing)


(defcustom pr-buffer-name "*Printing Interface*"
  "Specify the name of the buffer interface for printing package.

It's used by `pr-interface'."
  :type 'string
  :group 'printing)


(defcustom pr-buffer-name-ignore
  (list (regexp-quote pr-buffer-name)	; ignore printing interface buffer
	"^ .*$")			; ignore invisible buffers
  "Specify a regexp list for buffer names to be ignored in interface buffer.

NOTE: Case is important for matching, that is, `case-fold-search' is always
      nil.

It's used by `pr-interface'."
  :type '(repeat (regexp :tag "Buffer Name Regexp"))
  :group 'printing)


(defcustom pr-buffer-verbose t
  "Non-nil means to be verbose when editing a field in interface buffer.

It's used by `pr-interface'."
  :type 'boolean
  :group 'printing)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal Variables


(defvar pr-txt-command nil
  "Name of program for printing a text file.
See `pr-txt-printer-alist'.")


(defvar pr-txt-switches nil
  "List of sexp's to pass as extra options to the text printer program.
See `pr-txt-printer-alist'.")


(defvar pr-txt-printer nil
  "Specify text printer name.
See `pr-txt-printer-alist'.")


(defvar pr-ps-command nil
  "Name of program for printing a PostScript file.
See `pr-ps-printer-alist'.")


(defvar pr-ps-switches nil
  "List of sexp's to pass as extra options to the PostScript printer program.
See `pr-ps-printer-alist'.")


(defvar pr-ps-printer-switch nil
  "Specify PostScript printer name switch.
See `pr-ps-printer-alist'.")


(defvar pr-ps-printer nil
  "Specify PostScript printer name.
See `pr-ps-printer-alist'.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macros


(defmacro pr-save-file-modes (&rest body)
  "Set temporally file modes to `pr-file-modes'."
  `(let ((pr--default-file-modes (default-file-modes)))	; save default
     (set-default-file-modes pr-file-modes)
     ,@body
     (set-default-file-modes pr--default-file-modes))) ; restore default


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keys & Menus


(defsubst pr-visible-p (key)
  (memq key pr-visible-entry-list))


(defsubst pr-mode-alist-p ()
  (cdr (assq major-mode pr-mode-alist)))


(defsubst pr-auto-mode-p ()
  (and pr-auto-mode (pr-mode-alist-p)))


(defsubst pr-using-ghostscript-p ()
  (and pr-print-using-ghostscript (not pr-spool-p)))


(defalias 'pr-get-symbol
  (if (fboundp 'easy-menu-intern)	; hacked from easymenu.el
      'easy-menu-intern
    (lambda (s) (if (stringp s) (intern s) s))))


(defconst pr-menu-spec
  ;; Menu mapping:
  ;;   unfortunately XEmacs doesn't support :active for submenus,
  ;;   only for items.
  ;;   So, it uses :included instead of :active.
  ;;   Also, XEmacs doesn't support :help tag.
  (let ((pr-:active  (if (featurep 'xemacs)
			 :included	; XEmacs
		       :active))	; GNU Emacs
	(pr-:help    (if (featurep 'xemacs)
			 'ignore				; XEmacs
		       #'(lambda (text) (list :help text)))))	; GNU Emacs
    `(
      ["Printing Interface" pr-interface
       ,@(funcall
	  pr-:help "Use buffer interface instead of menu interface")]
      "--"
      ("PostScript Preview" :included (pr-visible-p 'postscript)
       ,@(funcall
	  pr-:help "Preview PostScript instead of sending to printer")
       ("Directory" ,pr-:active (not pr-spool-p)
	["1-up"     (pr-ps-directory-preview 1   nil nil t) t]
	["2-up"     (pr-ps-directory-preview 2   nil nil t) t]
	["4-up"     (pr-ps-directory-preview 4   nil nil t) t]
	["Other..." (pr-ps-directory-preview nil nil nil t)
	 :keys "\\[pr-ps-buffer-preview]"])
       ("Buffer" ,pr-:active (not pr-spool-p)
	["1-up"     (pr-ps-buffer-preview 1   t) t]
	["2-up"     (pr-ps-buffer-preview 2   t) t]
	["4-up"     (pr-ps-buffer-preview 4   t) t]
	["Other..." (pr-ps-buffer-preview nil t)
	 :keys "\\[pr-ps-buffer-preview]"])
       ("Region" ,pr-:active (and (not pr-spool-p) (ps-mark-active-p))
	["1-up"     (pr-ps-region-preview 1   t) t]
	["2-up"     (pr-ps-region-preview 2   t) t]
	["4-up"     (pr-ps-region-preview 4   t) t]
	["Other..." (pr-ps-region-preview nil t)
	 :keys "\\[pr-ps-region-preview]"])
       ("Mode" ,pr-:active (and (not pr-spool-p) (pr-mode-alist-p))
	["1-up"     (pr-ps-mode-preview 1   t) t]
	["2-up"     (pr-ps-mode-preview 2   t) t]
	["4-up"     (pr-ps-mode-preview 4   t) t]
	["Other..." (pr-ps-mode-preview nil t)
	 :keys "\\[pr-ps-mode-preview]"])
       ("File"
	["No Preprocessing..." (call-interactively 'pr-ps-file-preview)
	 :keys "\\[pr-ps-file-preview]"
	 ,@(funcall
	    pr-:help "Preview PostScript file")]
	"--"
	["PostScript Utility" pr-update-menus :active pr-ps-utility-alist
	 ,@(funcall
	    pr-:help "Select PostScript utility")]
	"--"
	["1-up..."   (pr-ps-file-up-preview 1   t t) pr-ps-utility-alist]
	["2-up..."   (pr-ps-file-up-preview 2   t t) pr-ps-utility-alist]
	["4-up..."   (pr-ps-file-up-preview 4   t t) pr-ps-utility-alist]
	["Other..."  (pr-ps-file-up-preview nil t t)
	 :keys "\\[pr-ps-file-up-preview]" :active pr-ps-utility-alist]
	"--"
	["Landscape" pr-toggle-file-landscape-menu
	 :style toggle :selected pr-file-landscape
	 ,@(funcall
	    pr-:help "Toggle landscape for PostScript file")
	 :active pr-ps-utility-alist]
	["Duplex"    pr-toggle-file-duplex-menu
	 :style toggle :selected pr-file-duplex
	 ,@(funcall
	    pr-:help "Toggle duplex for PostScript file")
	 :active pr-ps-utility-alist]
	["Tumble"    pr-toggle-file-tumble-menu
	 :style toggle :selected pr-file-tumble
	 ,@(funcall
	    pr-:help "Toggle tumble for PostScript file")
	 :active (and pr-file-duplex pr-ps-utility-alist)])
       ["Despool..." (call-interactively 'pr-despool-preview)
	:active pr-spool-p :keys "\\[pr-despool-preview]"
	,@(funcall
	   pr-:help "Despool PostScript buffer to printer or file (C-u)")])
      ("PostScript Print" :included (pr-visible-p 'postscript)
       ,@(funcall
	  pr-:help "Send PostScript to printer or file (C-u)")
       ("Directory"
	["1-up"     (pr-ps-directory-ps-print 1   nil nil t) t]
	["2-up"     (pr-ps-directory-ps-print 2   nil nil t) t]
	["4-up"     (pr-ps-directory-ps-print 4   nil nil t) t]
	["Other..." (pr-ps-directory-ps-print nil nil nil t)
	 :keys "\\[pr-ps-buffer-ps-print]"])
       ("Buffer"
	["1-up"     (pr-ps-buffer-ps-print 1   t) t]
	["2-up"     (pr-ps-buffer-ps-print 2   t) t]
	["4-up"     (pr-ps-buffer-ps-print 4   t) t]
	["Other..." (pr-ps-buffer-ps-print nil t)
	 :keys "\\[pr-ps-buffer-ps-print]"])
       ("Region" ,pr-:active (ps-mark-active-p)
	["1-up"     (pr-ps-region-ps-print 1   t) t]
	["2-up"     (pr-ps-region-ps-print 2   t) t]
	["4-up"     (pr-ps-region-ps-print 4   t) t]
	["Other..." (pr-ps-region-ps-print nil t)
	 :keys "\\[pr-ps-region-ps-print]"])
       ("Mode" ,pr-:active (pr-mode-alist-p)
	["1-up"     (pr-ps-mode-ps-print 1   t) t]
	["2-up"     (pr-ps-mode-ps-print 2   t) t]
	["4-up"     (pr-ps-mode-ps-print 4   t) t]
	["Other..." (pr-ps-mode-ps-print nil t)
	 :keys "\\[pr-ps-mode-ps-print]"])
       ("File"
	["No Preprocessing..." (call-interactively 'pr-ps-file-ps-print)
	 :keys "\\[pr-ps-file-ps-print]"
	 ,@(funcall
	    pr-:help "Send PostScript file to printer")]
	"--"
	["PostScript Utility" pr-update-menus :active pr-ps-utility-alist
	 ,@(funcall
	    pr-:help "Select PostScript utility")]
	"--"
	["1-up..."   (pr-ps-file-up-ps-print 1   t t) pr-ps-utility-alist]
	["2-up..."   (pr-ps-file-up-ps-print 2   t t) pr-ps-utility-alist]
	["4-up..."   (pr-ps-file-up-ps-print 4   t t) pr-ps-utility-alist]
	["Other..."  (pr-ps-file-up-ps-print nil t t)
	 :keys "\\[pr-ps-file-up-ps-print]" :active pr-ps-utility-alist]
	"--"
	["Landscape" pr-toggle-file-landscape-menu
	 :style toggle :selected pr-file-landscape
	 ,@(funcall
	    pr-:help "Toggle landscape for PostScript file")
	 :active pr-ps-utility-alist]
	["Duplex"    pr-toggle-file-duplex-menu
	 :style toggle :selected pr-file-duplex
	 ,@(funcall
	    pr-:help "Toggle duplex for PostScript file")
	 :active pr-ps-utility-alist]
	["Tumble"    pr-toggle-file-tumble-menu
	 :style toggle :selected pr-file-tumble
	 ,@(funcall
	    pr-:help "Toggle tumble for PostScript file")
	 :active (and pr-file-duplex pr-ps-utility-alist)])
       ["Despool..." (call-interactively 'pr-despool-ps-print)
	:active pr-spool-p :keys "\\[pr-despool-ps-print]"
	,@(funcall
	   pr-:help "Despool PostScript buffer to printer or file (C-u)")])
      ["PostScript Printers" pr-update-menus
       :active pr-ps-printer-alist :included (pr-visible-p 'postscript)
       ,@(funcall
	  pr-:help "Select PostScript printer")]
      "--"
      ("Printify" :included (pr-visible-p 'text)
       ,@(funcall
	  pr-:help
	  "Replace non-printing chars with printable representations.")
       ["Directory" pr-printify-directory t]
       ["Buffer"    pr-printify-buffer    t]
       ["Region"    pr-printify-region    (ps-mark-active-p)])
      ("Print" :included (pr-visible-p 'text)
       ,@(funcall
	  pr-:help "Send text to printer")
       ["Directory" pr-txt-directory t]
       ["Buffer"    pr-txt-buffer    t]
       ["Region"    pr-txt-region    (ps-mark-active-p)]
       ["Mode"      pr-txt-mode      (pr-mode-alist-p)])
      ["Text Printers" pr-update-menus
       :active pr-txt-printer-alist :included (pr-visible-p 'text)
       ,@(funcall
	  pr-:help "Select text printer")]
      "--"
      ["Landscape"               pr-toggle-landscape-menu
       :style toggle :selected ps-landscape-mode
       :included (pr-visible-p 'postscript-options)]
      ["Print Header"            pr-toggle-header-menu
       :style toggle :selected ps-print-header
       :included (pr-visible-p 'postscript-options)]
      ["Print Header Frame"      pr-toggle-header-frame-menu
       :style toggle :selected ps-print-header-frame :active ps-print-header
       :included (pr-visible-p 'postscript-options)]
      ["Line Number"             pr-toggle-line-menu
       :style toggle :selected ps-line-number
       :included (pr-visible-p 'postscript-options)]
      ["Zebra Stripes"           pr-toggle-zebra-menu
       :style toggle :selected ps-zebra-stripes
       :included (pr-visible-p 'postscript-options)]
      ["Duplex"                  pr-toggle-duplex-menu
       :style toggle :selected ps-spool-duplex
       :included (pr-visible-p 'postscript-options)]
      ["Tumble"                  pr-toggle-tumble-menu
       :style toggle :selected ps-spool-tumble :active ps-spool-duplex
       :included (pr-visible-p 'postscript-options)]
      ["Upside-Down"             pr-toggle-upside-down-menu
       :style toggle :selected ps-print-upside-down
       :included (pr-visible-p 'postscript-options)]
      ("Print All Pages" :included (pr-visible-p 'postscript-options)
       ,@(funcall
	  pr-:help "Select odd/even pages/sheets to print")
       ["All Pages"   (pr-even-or-odd-pages nil)
	:style radio :selected (eq ps-even-or-odd-pages nil)]
       ["Even Pages"  (pr-even-or-odd-pages 'even-page)
	:style radio :selected (eq ps-even-or-odd-pages 'even-page)]
       ["Odd Pages"   (pr-even-or-odd-pages 'odd-page)
	:style radio :selected (eq ps-even-or-odd-pages 'odd-page)]
       ["Even Sheets" (pr-even-or-odd-pages 'even-sheet)
	:style radio :selected (eq ps-even-or-odd-pages 'even-sheet)]
       ["Odd Sheets"  (pr-even-or-odd-pages 'odd-sheet)
	:style radio :selected (eq ps-even-or-odd-pages 'odd-sheet)])
      "--"
      ["Spool Buffer"            pr-toggle-spool-menu
       :style toggle :selected pr-spool-p
       :included (pr-visible-p 'postscript-process)
       ,@(funcall
	  pr-:help "Toggle PostScript spooling")]
      ["Print with faces"        pr-toggle-faces-menu
       :style toggle :selected pr-faces-p
       :included (pr-visible-p 'postscript-process)
       ,@(funcall
	  pr-:help "Toggle PostScript printing with faces")]
      ["Print via Ghostscript" pr-toggle-ghostscript-menu
       :style toggle :selected pr-print-using-ghostscript
       :included (pr-visible-p 'postscript-process)
       ,@(funcall
	  pr-:help "Toggle PostScript generation using ghostscript")]
      "--"
      ["Auto Region" pr-toggle-region-menu
       :style toggle :selected pr-auto-region
       :included (pr-visible-p 'printing)]
      ["Auto Mode"   pr-toggle-mode-menu
       :style toggle :selected pr-auto-mode
       :included (pr-visible-p 'printing)]
      ["Menu Lock"   pr-toggle-lock-menu
       :style toggle :selected pr-menu-lock
       :included (pr-visible-p 'printing)]
      "--"
      ("Customize" :included (pr-visible-p 'help)
       ["printing" pr-customize       t]
       ["ps-print" ps-print-customize t]
       ["lpr"      lpr-customize      t])
      ("Show Settings" :included (pr-visible-p 'help)
       ["printing" pr-show-pr-setup  t]
       ["ps-print" pr-show-ps-setup  t]
       ["lpr"      pr-show-lpr-setup t])
      ["Help" pr-help :active t :included (pr-visible-p 'help)]
      )))


(defun pr-menu-bind ()
  "Install `printing' menu in the menubar.
This replaces the File/Print* menu entries with a File/Print sub-menu.
Calls `pr-update-menus' to adjust menus."
  (interactive)
  (pr-global-menubar pr-menu-spec)
  (pr-update-menus t))


;; Key binding
(let ((pr-print-key (if (featurep 'xemacs)
			'f22		; XEmacs
		      'print)))		; GNU Emacs
  (global-set-key `[,pr-print-key]                'pr-ps-fast-fire)
  ;; Well, M-print and S-print are used because in my keyboard S-print works
  ;; and M-print doesn't.  But M-print can work in other keyboard.
  (global-set-key `[(meta ,pr-print-key)]         'pr-ps-mode-using-ghostscript)
  (global-set-key `[(shift ,pr-print-key)]        'pr-ps-mode-using-ghostscript)
  ;; Well, C-print and C-M-print are used because in my keyboard C-M-print works
  ;; and C-print doesn't.  But C-print can work in other keyboard.
  (global-set-key `[(control ,pr-print-key)]      'pr-txt-fast-fire)
  (global-set-key `[(control meta ,pr-print-key)] 'pr-txt-fast-fire))


;;; You can also use something like:
;;;(global-set-key "\C-ci"  'pr-interface)
;;;(global-set-key "\C-cbp" 'pr-ps-buffer-print)
;;;(global-set-key "\C-cbx" 'pr-ps-buffer-preview)
;;;(global-set-key "\C-cbb" 'pr-ps-buffer-using-ghostscript)
;;;(global-set-key "\C-crp" 'pr-ps-region-print)
;;;(global-set-key "\C-crx" 'pr-ps-region-preview)
;;;(global-set-key "\C-crr" 'pr-ps-region-using-ghostscript)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Help Message


(defconst pr-help-message
  (concat "printing.el version " pr-version
	  "    ps-print.el version " ps-print-version
	  "\n\n
Menu Layout
-----------

The `printing' menu (Tools/Printing or File/Print) has the following layout:

       +-----------------------------+
A   0  |   Printing Interface        |
       +-----------------------------+       +-A---------+     +-B------+
I   1  |   PostScript Preview       >|-------|Directory >|-----|1-up    |
    2  |   PostScript Print         >|---- A |Buffer    >|-- B |2-up    |
    3  |   PostScript Printer: name >|---- C |Region    >|-- B |4-up    |
       +-----------------------------+       |Mode      >|-- B |Other...|
II  4  |   Printify                 >|-----\\ |File      >|--\\  +--------+
    5  |   Print                    >|---\\ | |Despool... |  |
    6  |   Text Printer: name       >|-\\ | | +-----------+  |
       +-----------------------------+ | | | +---------+   +------------+
III 7  |[ ]Landscape                 | | | \\-|Directory|   | No Prep... | Ia
    8  |[ ]Print Header              | | |   |Buffer   |   +------------+ Ib
    9  |[ ]Print Header Frame        | | |   |Region   |   |   name    >|- C
    10 |[ ]Line Number               | | |   +---------+   +------------+
    11 |[ ]Zebra Stripes             | | |   +---------+   |   1-up...  | Ic
    12 |[ ]Duplex                    | | \\---|Directory|   |   2-up...  |
    13 |[ ]Tumble                    | \\--\\  |Buffer   |   |   4-up...  |
    14 |[ ]Upside-Down               |    |  |Region   |   |   Other... |
    15 |   Print All Pages          >|--\\ |  |Mode     |   +------------+
       +-----------------------------+  | |  +---------+   |[ ]Landscape| Id
IV  16 |[ ]Spool Buffer              |  | |  +-C-------+   |[ ]Duplex   | Ie
    17 |[ ]Print with faces          |  | \\--|( )name A|   |[ ]Tumble   | If
    18 |[ ]Print via Ghostscript     |  |    |( )name B|   +------------+
       +-----------------------------+  |    |...      |
V   19 |[ ]Auto Region               |  |    |(*)name  |
    20 |[ ]Auto Mode                 |  |    |...      |
    21 |[ ]Menu Lock                 |  |    +---------+   +--------------+
       +-----------------------------+  \\------------------|(*)All Pages  |
VI  22 |   Customize                >|--- D  +-D------+    |( )Even Pages |
    23 |   Show Settings            >|-------|printing|    |( )Odd Pages  |
    24 |   Help                      |       |ps-print|    |( )Even Sheets|
       +-----------------------------+       |lpr     |    |( )Odd Sheets |
					     +--------+    +--------------+

See `pr-visible-entry-list' for hiding some parts of the menu.

The menu has the following sections:

A. Interface:

   0. You can use a buffer interface instead of menus.  It looks like the
      customization buffer.  Basically, it has the same options found in the
      menu and some extra options, all this on a buffer.

I. PostScript printing:

   1. You can generate a PostScript file (if you type C-u before activating
      menu) or PostScript temporary file for a directory, a buffer, a region
      or a major mode, choosing 1-up, 2-up, 4-up or any other n-up printing;
      after file generation, ghostview is activated using the file generated
      as argument.  This option is disabled if spooling is on (option 16).
      Also, if you already have a PostScript file you can preview it.
      Instead of previewing each buffer, region or major mode at once, you
      can save temporarily the PostScript code generated in a buffer and
      preview it later.  The option `Despool...' despools the PostScript
      spooling buffer in a temporary file and uses ghostview to preview it.
      If you type C-u before choosing this option, the PostScript code
      generated is saved in a file instead of saving in a temporary file.  To
      spool the PostScript code generated you need to turn on the option 16.
      The option `Despool...' is enabled if spooling is on (option 16).

      NOTE 1: It's possible to customize a major mode printing, just declare
	      the customization in `pr-mode-alist' and invoke some of
	      `*-mode*' commands or select Mode option in Printing menu.  An
	      example for major mode usage is when you're using gnus (or mh,
	      or rmail, etc.) and you're in the *Summary* buffer, if you
	      forget to switch to the *Article* buffer before printing,
	      you'll get a nicely formatted list of article subjects shows
	      up at the printer.  With major mode printing you don't need to
	      switch from gnus *Summary* buffer first.

      NOTE 2: There are the following options for PostScript file processing:
	      Ia. Print the file *No Preprocessing*, that is, send it
		  directly to PostScript printer.
	      Ib. PostScript utility processing selection.
		  See `pr-ps-utility-alist' and `pr-setting-database' for
		  documentation.
	      Ic. Do n-up processing before printing.
	      Id. Toggle on/off landscape for PostScript file processing.
	      Ie. Toggle on/off duplex for PostScript file processing.
	      If. Toggle on/off tumble for PostScript file processing.

      NOTE 3: Don't forget to download and install the utilities declared on
	      `pr-ps-utility-alist'.

   2. Operate the same way as option 1, but it sends directly the PostScript
      code (or put in a file, if you've typed C-u) or it uses ghostscript to
      print the PostScript file generated.  It depends on option 18, if it's
      turned on, it uses ghostscript; otherwise, it sends directly to
      printer.  If spooling is on (option 16), the PostScript code is saved
      temporarily in a buffer instead of printing it or saving it in a file.
      Also, if you already have a PostScript file you can print it.
      Instead of printing each buffer, region or major mode at once, you can
      save temporarily the PostScript code generated in a buffer and print it
      later.  The option `Despool...' despools the PostScript spooling buffer
      directly on a printer.  If you type C-u before choosing this option,
      the PostScript code generated is saved in a file instead of sending it to
      the printer.  To spool the PostScript code generated you need to turn on
      option 16.  This option is enabled if spooling is on (option 16).
      See also the NOTE 1, NOTE 2 and NOTE 3 on option 1.

   3. You can select a new PostScript printer to send PostScript code
      generated.  For selection it's used all PostScript printers defined
      in `pr-ps-printer-alist' variable (see it for documentation).
      See also `pr-setting-database'.

II. Text printing:

   4. If you have control characters (character code from \\000 to \\037) in a
      buffer and you want to print them in a text printer, select this
      option.  All control characters in your buffer or region will be
      replaced by a printable representation.  The printable representations
      use ^ (for ASCII control characters) or hex.  The characters tab,
      linefeed, space, return and formfeed are not affected.
      You don't need to select this option if you use any option of section
      I, the PostScript engine treats control characters properly.

   5. If you want to print a directory, buffer, region or major mode in a
      text printer, select this option.  See also the NOTE 1 on option 1.

   6. You can select a new text printer to send text generated.  For
      selection it's used all text printers defined in `pr-txt-printer-alist'
      variable (see it for documentation).
      See also `pr-setting-database'.

III. PostScript page toggle options:

   7. If you want a PostScript landscape printing, turn on this option.

   8. If you want to have a header in each page in your PostScript code,
      turn on this option.

   9. If you want to draw a gaudy frame around the header, turn on this
      option.  This option is enabled if print header is on (option 8).

   10. If you want that the line number is printed in your PostScript code,
       turn on this option.

   11. If you want background zebra stripes in your PostScript code, turn on
       this option.

   12. If you want a duplex printing and your PostScript printer has this
       feature, turn on this option.

   13. If you turned on duplex printing, you can choose if you want to have a
       printing suitable for binding on the left or right (tumble off), or to
       have a printing suitable for binding at top or bottom (tumble on).
       This option is enabled if duplex is on (option 12).

   14. If you want a PostScript upside-down printing, turn on this option.

   15. With this option, you can choose if you want to print all pages, odd
       pages, even pages, odd sheets or even sheets.
       See also `ps-even-or-odd-pages'.

IV. PostScript processing toggle options:

   16. If you want to spool the PostScript code generated, turn on this
       option.  To spool the PostScript code generated use option 2.  You can
       despool later by choosing option 1 or 2, sub-option `Despool...'.

   17. If you use colors in your buffers and want to see these colors on your
       PostScript code generated, turn on this option.  If you have a
       black/white PostScript printer, these colors are displayed in gray
       scale by PostScript printer interpreter.

   18. If you don't have a PostScript printer to send PostScript files, turn
       on this option.  When this option is on, the ghostscript is used to
       print PostScript files.  In GNU or Unix system, if ghostscript is set
       as a PostScript filter, you don't need to turn on this option.

V. Printing customization:

   19. If you want that region is automagically detected, turn on this
       option.  Note that this will only work if you're using transient mark
       mode.  When this option is on, the `*-buffer*' commands will behave
       like `*-region*' commands, that is, `*-buffer*' commands will print
       only the region marked instead of all buffer.

   20. Turn this option on if you want that when current major-mode is
       declared in `pr-mode-alist', the `*-buffer*' and `*-region*' commands
       behave like `*-mode*' commands.

   21. If you want that Printing menu stays open while you are setting
       toggle options, turn on this option.  The variables
       `pr-menu-char-height' and `pr-menu-char-width' are used to guess the
       menu position, so don't forget to adjust these variables if menu
       position is not ok.

VI. Customization:

   22. Besides all options in section III, IV and V, you can customize much
       more PostScript options in `ps-print' option.  Or you can customize
       some `lpr' options for text printing.  Or customize `printing'
       options.

   23. Show current settings for `printing', `ps-print' or `lpr'.

   24. Quick help for printing menu layout.
")
  "Printing help message.")


(defconst pr-interface-help-message
  (concat "printing.el version " pr-version
	  "    ps-print.el version " ps-print-version
	  "\n\n
The printing interface buffer has the same functionality as the printing menu.
The major difference is that the states (like sending PostScript generated to a
file, n-up printing, etc.) are set and saved between printing buffer
activation.  Also, the landscape, duplex and tumble values are the same for
PostScript file and directory/buffer/region/mode processing; using menu, there
are different value sets for PostScript file and directory/buffer/region/mode
processing.

The printing interface buffer has the following sections:

1. Print:

   Here you can choose to print/preview a buffer, a directory or a PostScript
   file:

   1a. Buffer:

      * Buffer List:
	 Select a buffer from the current buffer list.

      * Region:
	 If it's on, this means that the selected buffer has an active region,
	 so you can turn on/off, as you wish.
	 If it's off when a buffer is selected, this means that the selected
	 buffer has no active region, so it'll not be possible to turn it on.
	 If you want to process the region, let this option on.
	 If you want to process the whole buffer, let this option off.

      * Mode:
	 If it's on, this means that the selected buffer major mode is declared
	 for major mode processing, so you can turn on/off, as you wish.
	 If it's off when a buffer is selected, this means that the selected
	 buffer major mode isn't declared for major mode processing, so it'll
	 not be possible to turn it on.
	 If you want the major mode processing, let this option on.
	 If you don't want the major mode processing, let this option off.

      NOTE 1: It's possible to customize a major mode printing, just declare
	      the customization in `pr-mode-alist' and invoke some of
	      `*-mode*' commands or select Mode option in Printing menu.  An
	      example for major mode usage is when you're using gnus (or mh,
	      or rmail, etc.) and you're in the *Summary* buffer, if you
	      forget to switch to the *Article* buffer before printing,
	      you'll get a nicely formatted list of article subjects shows
	      up at the printer.  With major mode printing you don't need to
	      switch from gnus *Summary* buffer first.

   1b. Directory:

      * Directory:
	 Specify a valid directory path.

      * File Regexp:
	 Specify a file name regexp.  All file names in the directory that
	 match with regexp will be printed/previewed.  An empty file name
	 regexp means to print/preview all files in the directory.

      * List Directory Entry:
	 If it's turned on, list directory entries besides file entries.

   1c. PostScript file:

      * PostScript File:
	 Specify an existent PostScript file to print/preview.

      * PostScript Utility:
	 Select a PostScript utility.
	 See `pr-ps-utility-alist' and `pr-setting-database' for documentation.

      NOTE 2: Don't forget to download and install the utilities declared on
	      `pr-ps-utility-alist'.

      * No Preprocessing:
	 If it's turned on, don't use the PostScript utility to preprocess the
	 PostScript file before printing/previewing.

2. PostScript printer:

   * PostScript Printer:
      You can select a new PostScript printer to send PostScript code
      generated.  For selection it's used all PostScript printers defined
      in `pr-ps-printer-alist' variable (see it for documentation).
      See also `pr-setting-database'.

   * Despool:
      If spooling is on, you can turn it on/off, as you wish.
      If spooling is off, it'll not be possible to turn it on.
      If it's turned on, specify to despools the PostScript spooling buffer in
      a temporary file or in the selected PostScript file when
      printing/previewing.

   * Preview:
      Preview the PostScript generated.

   * Print:
      Print the PostScript generated.

   * Quit:
      Quit from printing interface buffer.

   * Send to Printer/Temporary File:
      If it's turned on, the PostScript generated is sent directly to
      PostScript printer or, for previewing, to a temporary file.

   * Send to File:
      Specify a file name to send the PostScript generated.

   * N-Up:
      Specify n-up printing.

3. Text printer:

   * Text Printer:
      Select a new text printer to send text generated.  For selection it's used
      all text printers defined in `pr-txt-printer-alist' variable (see it for
      documentation).  See also `pr-setting-database'.

   * Printify:
      If you have control characters (character code from \\000 to \\037) in a
      buffer and you want to print them in a text printer, select this
      option.  All control characters in your buffer or region will be
      replaced by a printable representation.  The printable representations
      use ^ (for ASCII control characters) or hex.  The characters tab,
      linefeed, space, return and formfeed are not affected.
      You don't need to select this option if you use any option of section
      I, the PostScript engine treats control characters properly.

   * Print:
      To print a directory, buffer, region or major mode in a
      text printer, select this option.  See also the NOTE 1 on section 1.

   * Quit:
      Quit from printing interface buffer.

4. Settings:

   There are 3 setting columns:

   4a. First column (left column):

      * Landscape:
	 PostScript landscape printing.

      * Print Header:
	 To have a header in each page in your PostScript code.

      * Print Header Frame:
	 To draw a gaudy frame around the header.

      * Line Number:
	 The line number is printed in your PostScript code.

      * Zebra Stripes:
	 Background zebra stripes in your PostScript code.

      * Duplex:
	 Duplex printing (if your PostScript printer has this feature).

      * Tumble:
	 If duplex printing is on, you can choose if you want to have a
	 printing suitable for binding on the left or right (tumble off), or to
	 have a printing suitable for binding at top or bottom (tumble on).

      * Upside-Down:
	 PostScript upside-down printing.

   4b. Second column (middle column):

      * Auto Region:
	 If you want that region is automagically detected, turn on this
	 option.  Note that this will only work if you're using transient mark
	 mode.  When this option is on, the `*-buffer*' commands will behave
	 like `*-region*' commands, that is, `*-buffer*' commands will print
	 only the region marked instead of all buffer.

      * Auto Mode:
	 Turn this option on if you want that when current major-mode is
	 declared in `pr-mode-alist', the `*-buffer*' and `*-region*' commands
	 behave like `*-mode*' commands.

      * Menu Lock:
	 If you want that Printing menu stays open while you are setting
	 toggle options, turn on this option.  The variables
	 `pr-menu-char-height' and `pr-menu-char-width' are used to guess the
	 menu position, so don't forget to adjust these variables if menu
	 position is not ok.

      * Spool Buffer:
	 To spool the PostScript code generated.  You can despool later by
	 setting Despool option on PostScript printer section.

      * Print with faces:
	 If you use colors in your buffers and want to see these colors on your
	 PostScript code generated, turn on this option.  If you have a
	 black/white PostScript printer, these colors are displayed in gray
	 scale by PostScript printer interpreter.

      * Print via Ghostscript:
	 If you don't have a PostScript printer to send PostScript files, turn
	 on this option.  When this option is on, the ghostscript is used to
	 print PostScript files.  In GNU or Unix system, if ghostscript is set
	 as a PostScript filter, you don't need to turn on this option.

      * Parity Page Menu:
	 To print all pages, odd pages, even pages, odd sheets or even sheets.
	 See also `ps-even-or-odd-pages'.

   4c. Third column (right column):

      * Verbose:
	 That is, to be verbose when editing a field in interface buffer.

5. Customize:

   Besides all options in section 4, you can customize much more PostScript
   options in `ps-print' option.  Or you can customize some `lpr' options for
   text printing.  Or customize `printing' options.

6. Show settings:

   Show current settings for `printing', `ps-print' or `lpr'.

7. Help:

   Quick help for printing interface buffer and printing menu layout.  You can
   also quit the printing interface buffer or kill all printing help buffer.
")
  "Printing buffer interface help message.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commands


;;;###autoload
(defun pr-interface (&optional buffer)
  "Activate the printing interface buffer.

If BUFFER is nil, the current buffer is used for printing.

For more information, type \\[pr-interface-help]."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (pr-create-interface)))


;;;###autoload
(defun pr-ps-directory-preview (n-up dir file-regexp &optional filename)
  "Preview directory using ghostview.

Interactively, the command prompts for N-UP printing number, a directory, a
file name regexp for matching and, when you use a prefix argument (C-u), the
command prompts the user for a file name, and saves the PostScript image in
that file instead of saving it in a temporary file.

Noninteractively, if N-UP is nil, prompts for N-UP printing number.  If DIR is
nil, prompts for DIRectory.  If FILE-REGEXP is nil, prompts for
FILE(name)-REGEXP.  The argument FILENAME is treated as follows: if it's nil,
save the image in a temporary file.  If FILENAME is a string, save the
PostScript image in a file with that name.  If FILENAME is t, prompts for a
file name.

See also documentation for `pr-list-directory'."
  (interactive (pr-interactive-ps-dir-args (pr-prompt "PS preview dir")))
  (pr-set-ps-dir-args 'n-up 'dir 'file-regexp 'filename
		      (pr-prompt "PS preview dir"))
  (setq filename (pr-ps-file filename))
  (pr-ps-file-list n-up dir file-regexp filename)
  (or pr-spool-p
      (pr-ps-file-preview filename)))


;;;###autoload
(defun pr-ps-directory-using-ghostscript (n-up dir file-regexp &optional filename)
  "Print directory using PostScript through ghostscript.

Interactively, the command prompts for N-UP printing number, a directory, a
file name regexp for matching and, when you use a prefix argument (C-u), the
command prompts the user for a file name, and saves the PostScript image in
that file instead of saving it in a temporary file.

Noninteractively, if N-UP is nil, prompts for N-UP printing number.  If DIR is
nil, prompts for DIRectory.  If FILE-REGEXP is nil, prompts for
FILE(name)-REGEXP.  The argument FILENAME is treated as follows: if it's nil,
save the image in a temporary file.  If FILENAME is a string, save the
PostScript image in a file with that name.  If FILENAME is t, prompts for a
file name.

See also documentation for `pr-list-directory'."
  (interactive (pr-interactive-ps-dir-args (pr-prompt "PS print dir GS")))
  (pr-set-ps-dir-args 'n-up 'dir 'file-regexp 'filename
		      (pr-prompt "PS print dir GS"))
  (let ((file (pr-ps-file filename)))
    (pr-ps-file-list n-up dir file-regexp file)
    (pr-ps-file-using-ghostscript file)
    (or filename (pr-delete-file file))))


;;;###autoload
(defun pr-ps-directory-print (n-up dir file-regexp &optional filename)
  "Print directory using PostScript printer.

Interactively, the command prompts for N-UP printing number, a directory, a
file name regexp for matching and, when you use a prefix argument (C-u), the
command prompts the user for a file name, and saves the PostScript image in
that file instead of saving it in a temporary file.

Noninteractively, if N-UP is nil, prompts for N-UP printing number.  If DIR is
nil, prompts for DIRectory.  If FILE-REGEXP is nil, prompts for
FILE(name)-REGEXP.  The argument FILENAME is treated as follows: if it's nil,
save the image in a temporary file.  If FILENAME is a string, save the
PostScript image in a file with that name.  If FILENAME is t, prompts for a
file name.

See also documentation for `pr-list-directory'."
  (interactive (pr-interactive-ps-dir-args (pr-prompt "PS print dir")))
  (pr-set-ps-dir-args 'n-up 'dir 'file-regexp 'filename
		      (pr-prompt "PS print dir"))
  (let ((file (pr-ps-file filename)))
    (pr-ps-file-list n-up dir file-regexp file)
    (pr-ps-file-print file)
    (or filename (pr-delete-file file))))


;;;###autoload
(defun pr-ps-directory-ps-print (n-up dir file-regexp &optional filename)
  "Print directory using PostScript printer or through ghostscript.

It depends on `pr-print-using-ghostscript'.

Interactively, the command prompts for N-UP printing number, a directory, a
file name regexp for matching and, when you use a prefix argument (C-u), the
command prompts the user for a file name, and saves the PostScript image in
that file instead of saving it in a temporary file.

Noninteractively, if N-UP is nil, prompts for N-UP printing number.  If DIR is
nil, prompts for DIRectory.  If FILE-REGEXP is nil, prompts for
FILE(name)-REGEXP.  The argument FILENAME is treated as follows: if it's nil,
save the image in a temporary file.  If FILENAME is a string, save the
PostScript image in a file with that name.  If FILENAME is t, prompts for a
file name.

See also documentation for `pr-list-directory'."
  (interactive (pr-interactive-ps-dir-args
		(pr-prompt (pr-prompt-gs "PS print dir"))))
  (pr-set-ps-dir-args 'n-up 'dir 'file-regexp 'filename
		      (pr-prompt (pr-prompt-gs "PS print dir")))
  (if (pr-using-ghostscript-p)
      (pr-ps-directory-using-ghostscript n-up dir file-regexp filename)
    (pr-ps-directory-print n-up dir file-regexp filename)))


;;;###autoload
(defun pr-ps-buffer-preview (n-up &optional filename)
  "Preview buffer using ghostview.

Interactively, the command prompts for N-UP printing number and, when you use a
prefix argument (C-u), the command prompts the user for a file name, and saves
the PostScript image in that file instead of saving it in a temporary file.

Noninteractively, if N-UP is nil, prompts for N-UP printing number.  The
argument FILENAME is treated as follows: if it's nil, save the image in a
temporary file.  If FILENAME is a string, save the PostScript image in a file
with that name.  If FILENAME is t, prompts for a file name."
  (interactive (pr-interactive-n-up-file (pr-prompt "PS preview")))
  (if (pr-auto-mode-p)
      (pr-ps-mode-preview n-up filename)
    (pr-ps-preview (pr-region-active-symbol) n-up filename
		   (pr-region-active-string "PS preview"))))


;;;###autoload
(defun pr-ps-buffer-using-ghostscript (n-up &optional filename)
  "Print buffer using PostScript through ghostscript.

Interactively, the command prompts for N-UP printing number and, when you use a
prefix argument (C-u), the command prompts the user for a file name, and saves
the PostScript image in that file instead of sending it to the printer.

Noninteractively, if N-UP is nil, prompts for N-UP printing number.  The
argument FILENAME is treated as follows: if it's nil, send the image to the
printer.  If FILENAME is a string, save the PostScript image in a file with
that name.  If FILENAME is t, prompts for a file name."
  (interactive (pr-interactive-n-up-file (pr-prompt "PS print GS")))
  (if (pr-auto-mode-p)
      (pr-ps-mode-using-ghostscript n-up filename)
    (pr-ps-using-ghostscript (pr-region-active-symbol) n-up filename
			     (pr-region-active-string "PS print GS"))))


;;;###autoload
(defun pr-ps-buffer-print (n-up &optional filename)
  "Print buffer using PostScript printer.

Interactively, the command prompts for N-UP printing number and, when you use a
prefix argument (C-u), the command prompts the user for a file name, and saves
the PostScript image in that file instead of sending it to the printer.

Noninteractively, if N-UP is nil, prompts for N-UP printing number.  The
argument FILENAME is treated as follows: if it's nil, send the image to the
printer.  If FILENAME is a string, save the PostScript image in a file with
that name.  If FILENAME is t, prompts for a file name."
  (interactive (pr-interactive-n-up-file (pr-prompt "PS print")))
  (if (pr-auto-mode-p)
      (pr-ps-mode-print n-up filename)
    (pr-ps-print (pr-region-active-symbol) n-up filename
		 (pr-region-active-string "PS print"))))


;;;###autoload
(defun pr-ps-buffer-ps-print (n-up &optional filename)
  "Print buffer using PostScript printer or through ghostscript.

It depends on `pr-print-using-ghostscript'.

Interactively, the command prompts for N-UP printing number and, when you use a
prefix argument (C-u), the command prompts the user for a file name, and saves
the PostScript image in that file instead of sending it to the printer.

Noninteractively, if N-UP is nil, prompts for N-UP printing number.  The
argument FILENAME is treated as follows: if it's nil, send the image to the
printer.  If FILENAME is a string, save the PostScript image in a file with
that name.  If FILENAME is t, prompts for a file name."
  (interactive (pr-interactive-n-up-file
		(pr-prompt (pr-prompt-gs "PS print"))))
  (cond ((pr-auto-mode-p)
	 (pr-ps-mode-ps-print n-up filename))
	((pr-using-ghostscript-p)
	 (pr-ps-using-ghostscript (pr-region-active-symbol) n-up filename
				  (pr-region-active-string "PS print GS")))
	(t
	 (pr-ps-print (pr-region-active-symbol) n-up filename
		      (pr-region-active-string "PS print")))))


;;;###autoload
(defun pr-ps-region-preview (n-up &optional filename)
  "Preview region using ghostview.

See also `pr-ps-buffer-preview'."
  (interactive (pr-interactive-n-up-file (pr-prompt-region "PS preview")))
  (if (pr-auto-mode-p)
      (let ((pr-auto-region t))
	(pr-ps-mode-preview n-up filename))
    (pr-ps-preview 'region n-up filename "PS preview region")))


;;;###autoload
(defun pr-ps-region-using-ghostscript (n-up &optional filename)
  "Print region using PostScript through ghostscript.

See also `pr-ps-buffer-using-ghostscript'."
  (interactive (pr-interactive-n-up-file (pr-prompt-region "PS print GS")))
  (if (pr-auto-mode-p)
      (let ((pr-auto-region t))
	(pr-ps-mode-using-ghostscript n-up filename))
    (pr-ps-using-ghostscript 'region n-up filename "PS print GS region")))


;;;###autoload
(defun pr-ps-region-print (n-up &optional filename)
  "Print region using PostScript printer.

See also `pr-ps-buffer-print'."
  (interactive (pr-interactive-n-up-file (pr-prompt-region "PS print")))
  (if (pr-auto-mode-p)
      (let ((pr-auto-region t))
	(pr-ps-mode-print n-up filename))
    (pr-ps-print 'region n-up filename "PS print region")))


;;;###autoload
(defun pr-ps-region-ps-print (n-up &optional filename)
  "Print region using PostScript printer or through ghostscript.

See also `pr-ps-buffer-ps-print'."
  (interactive (pr-interactive-n-up-file
		(pr-prompt-region (pr-prompt-gs "PS print"))))
  (cond ((pr-auto-mode-p)
	 (let ((pr-auto-region t))
	   (pr-ps-mode-ps-print n-up filename)))
	((pr-using-ghostscript-p)
	 (pr-ps-using-ghostscript 'region n-up filename "PS print GS region"))
	(t
	 (pr-ps-print 'region n-up filename "PS print region"))))


;;;###autoload
(defun pr-ps-mode-preview (n-up &optional filename)
  "Preview major mode using ghostview.

See also `pr-ps-buffer-preview'."
  (interactive (pr-interactive-n-up-file "PS preview mode"))
  (pr-set-n-up-and-filename 'n-up 'filename "PS preview mode")
  (let ((file (pr-ps-file filename)))
    (and (pr-ps-mode n-up file)
	 (not pr-spool-p)
	 (pr-ps-file-preview file))))


;;;###autoload
(defun pr-ps-mode-using-ghostscript (n-up &optional filename)
  "Print major mode using PostScript through ghostscript.

See also `pr-ps-buffer-using-ghostscript'."
  (interactive (pr-interactive-n-up-file "PS print GS mode"))
  (pr-set-n-up-and-filename 'n-up 'filename "PS print GS mode")
  (let ((file (pr-ps-file filename)))
    (when (and (pr-ps-mode n-up file)
	       (not pr-spool-p))
      (pr-ps-file-using-ghostscript file)
      (or filename (pr-delete-file file)))))


;;;###autoload
(defun pr-ps-mode-print (n-up &optional filename)
  "Print major mode using PostScript printer.

See also `pr-ps-buffer-print'."
  (interactive (pr-interactive-n-up-file "PS print mode"))
  (pr-set-n-up-and-filename 'n-up 'filename "PS print mode")
  (pr-ps-mode n-up filename))


;;;###autoload
(defun pr-ps-mode-ps-print (n-up &optional filename)
  "Print major mode using PostScript or through ghostscript.

See also `pr-ps-buffer-ps-print'."
  (interactive (pr-interactive-n-up-file (pr-prompt-gs "PS print mode")))
  (if (pr-using-ghostscript-p)
      (pr-ps-mode-using-ghostscript n-up filename)
    (pr-ps-mode-print n-up filename)))


;;;###autoload
(defun pr-printify-directory (&optional dir file-regexp)
  "Replace nonprinting characters in directory with printable representations.
The printable representations use ^ (for ASCII control characters) or hex.
The characters tab, linefeed, space, return and formfeed are not affected.

Interactively, the command prompts for a directory and a file name regexp for
matching.

Noninteractively, if DIR is nil, prompts for DIRectory.  If FILE-REGEXP is nil,
prompts for FILE(name)-REGEXP.

See also documentation for `pr-list-directory'."
  (interactive (pr-interactive-dir-args "Printify dir"))
  (pr-set-dir-args 'dir 'file-regexp "Printify dir")
  (pr-file-list dir file-regexp 'pr-printify-buffer))


;;;###autoload
(defun pr-printify-buffer ()
  "Replace nonprinting characters in buffer with printable representations.
The printable representations use ^ (for ASCII control characters) or hex.
The characters tab, linefeed, space, return and formfeed are not affected."
  (interactive "*")
  (if (pr-region-active-p)
      (pr-printify-region)
    (printify-region (point-min) (point-max))))


;;;###autoload
(defun pr-printify-region ()
  "Replace nonprinting characters in region with printable representations.
The printable representations use ^ (for ASCII control characters) or hex.
The characters tab, linefeed, space, return and formfeed are not affected."
  (interactive "*")
  (printify-region (point) (mark)))


;;;###autoload
(defun pr-txt-directory (&optional dir file-regexp)
  "Print directory using text printer.

Interactively, the command prompts for a directory and a file name regexp for
matching.

Noninteractively, if DIR is nil, prompts for DIRectory.  If FILE-REGEXP is nil,
prompts for FILE(name)-REGEXP.

See also documentation for `pr-list-directory'."
  (interactive (pr-interactive-dir-args "Print dir"))
  (pr-set-dir-args 'dir 'file-regexp "Print dir")
  (pr-file-list dir file-regexp 'pr-txt-buffer))


;;;###autoload
(defun pr-txt-buffer ()
  "Print buffer using text printer."
  (interactive)
  (cond ((pr-auto-mode-p)
	 (pr-txt-mode))
	((pr-region-active-p)
	 (pr-txt-region))
	(t
	 (pr-txt-print (point-min) (point-max)))))


;;;###autoload
(defun pr-txt-region ()
  "Print region using text printer."
  (interactive)
  (if (pr-auto-mode-p)
      (let ((pr-auto-region t))
	(pr-txt-mode))
    (pr-txt-print (point) (mark))))


;;;###autoload
(defun pr-txt-mode ()
  "Print major mode using text printer."
  (interactive)
  (let ((args (pr-mode-alist-p)))
    (if args
	(funcall (car args) (nthcdr 2 args))
      (ding)
      (message "`%s' major mode not declared." major-mode))))


;;;###autoload
(defun pr-despool-preview (&optional filename)
  "Preview spooled PostScript.

Interactively, when you use a prefix argument (C-u), the command prompts the
user for a file name, and saves the spooled PostScript image in that file
instead of saving it in a temporary file.

Noninteractively, the argument FILENAME is treated as follows: if it is nil,
save the image in a temporary file.  If FILENAME is a string, save the
PostScript image in a file with that name."
  (interactive (list (ps-print-preprint current-prefix-arg)))
  (let ((file (pr-ps-file filename)))
    (when (stringp file)
      (pr-despool-print file)
      (pr-ps-file-preview file))))


;;;###autoload
(defun pr-despool-using-ghostscript (&optional filename)
  "Print spooled PostScript using ghostscript.

Interactively, when you use a prefix argument (C-u), the command prompts the
user for a file name, and saves the spooled PostScript image in that file
instead of sending it to the printer.

Noninteractively, the argument FILENAME is treated as follows: if it is nil,
send the image to the printer.  If FILENAME is a string, save the PostScript
image in a file with that name."
  (interactive (list (ps-print-preprint current-prefix-arg)))
  (let ((file (pr-ps-file filename)))
    (when (stringp file)
      (pr-despool-print file)
      (pr-ps-file-using-ghostscript file)
      (or filename (pr-delete-file file)))))


;;;###autoload
(defun pr-despool-print (&optional filename)
  "Send the spooled PostScript to the printer.

Interactively, when you use a prefix argument (C-u), the command prompts the
user for a file name, and saves the spooled PostScript image in that file
instead of sending it to the printer.

Noninteractively, the argument FILENAME is treated as follows: if it is nil,
send the image to the printer.  If FILENAME is a string, save the PostScript
image in a file with that name."
  (interactive (list (ps-print-preprint current-prefix-arg)))
  (pr-save-file-modes
   (let ((ps-lpr-command         (pr-command pr-ps-command))
	 (ps-lpr-switches        pr-ps-switches)
	 (ps-printer-name-option pr-ps-printer-switch)
	 (ps-printer-name        pr-ps-printer))
     (ps-despool filename))))


;;;###autoload
(defun pr-despool-ps-print (&optional filename)
  "Send the spooled PostScript to the printer or use ghostscript to print it.

Interactively, when you use a prefix argument (C-u), the command prompts the
user for a file name, and saves the spooled PostScript image in that file
instead of sending it to the printer.

Noninteractively, the argument FILENAME is treated as follows: if it is nil,
send the image to the printer.  If FILENAME is a string, save the PostScript
image in a file with that name."
  (interactive (list (ps-print-preprint current-prefix-arg)))
  (if pr-print-using-ghostscript
      (pr-despool-using-ghostscript filename)
    (pr-despool-print filename)))


;;;###autoload
(defun pr-ps-file-preview (filename)
  "Preview PostScript file FILENAME."
  (interactive (list (pr-ps-infile-preprint "Preview ")))
  (and (stringp filename) (file-exists-p filename)
       (pr-call-process pr-gv-command filename)))


;;;###autoload
(defun pr-ps-file-up-preview (n-up ifilename &optional ofilename)
  "Preview PostScript file FILENAME."
  (interactive (pr-interactive-n-up-inout "PS preview"))
  (let ((outfile (pr-ps-utility-args 'n-up 'ifilename 'ofilename
				     "PS preview ")))
    (pr-ps-utility-process n-up ifilename outfile)
    (pr-ps-file-preview outfile)))


;;;###autoload
(defun pr-ps-file-using-ghostscript (filename)
  "Print PostScript file FILENAME using ghostscript."
  (interactive (list (pr-ps-infile-preprint "Print preview ")))
  (and (stringp filename) (file-exists-p filename)
       (let* ((file (pr-expand-file-name filename))
	      (tempfile (pr-dosify-file-name (make-temp-file file))))
	 ;; gs use
	 (pr-call-process pr-gs-command
			  (format "-sDEVICE=%s" pr-gs-device)
			  (format "-r%d" pr-gs-resolution)
			  (pr-switches-string pr-gs-switches "pr-gs-switches")
			  (format "-sOutputFile=\"%s\"" tempfile)
			  file
			  "-c quit")
	 ;; printing
	 (pr-ps-file-print tempfile)
	 ;; deleting
	 (pr-delete-file tempfile))))


;;;###autoload
(defun pr-ps-file-print (filename)
  "Print PostScript file FILENAME."
  (interactive (list (pr-ps-infile-preprint "Print ")))
  (and (stringp filename) (file-exists-p filename)
       ;; printing
       (let ((file (pr-expand-file-name filename)))
	 (if (string= pr-ps-command "")
	     ;; default action
	     (let ((ps-spool-buffer (get-buffer-create ps-spool-buffer-name)))
	       (with-current-buffer ps-spool-buffer
		 (erase-buffer)
		 (insert-file-contents-literally file))
	       (pr-despool-print))
	   ;; use `pr-ps-command' to print
	   (apply 'pr-call-process
		  pr-ps-command
		  (pr-switches-string pr-ps-switches "pr-ps-switches")
		  (if (string-match "cp" pr-ps-command)
		      ;; for "cp" (cmd in out)
		      (list file
			    (concat pr-ps-printer-switch pr-ps-printer))
		    ;; else, for others (cmd out in)
		    (list (concat pr-ps-printer-switch pr-ps-printer)
			  file)))))))


;;;###autoload
(defun pr-ps-file-ps-print (filename)
  "Send PostScript file FILENAME to printer or use ghostscript to print it."
  (interactive (list (pr-ps-infile-preprint
		      (if pr-print-using-ghostscript
			  "Print preview "
			"Print "))))
  (if pr-print-using-ghostscript
      (pr-ps-file-using-ghostscript filename)
    (pr-ps-file-print filename)))


;;;###autoload
(defun pr-ps-file-up-ps-print (n-up ifilename &optional ofilename)
  "Process a PostScript file IFILENAME and send it to printer.

Interactively, the command prompts for N-UP printing number, for an input
PostScript file IFILENAME and, when you use a prefix argument (C-u), the
command prompts the user for an output PostScript file name OFILENAME, and
saves the PostScript image in that file instead of sending it to the printer.

Noninteractively, if N-UP is nil, prompts for N-UP printing number.  The
argument IFILENAME is treated as follows: if it's t, prompts for an input
PostScript file name; otherwise, it *must* be a string that it's an input
PostScript file name.  The argument OFILENAME is treated as follows: if it's
nil, send the image to the printer.  If OFILENAME is a string, save the
PostScript image in a file with that name.  If OFILENAME is t, prompts for a
file name."
  (interactive (pr-interactive-n-up-inout
		(if pr-print-using-ghostscript
		    "PS print GS"
		  "PS print")))
  (let ((outfile (pr-ps-utility-args 'n-up 'ifilename 'ofilename
				     (if pr-print-using-ghostscript
					 "PS print GS "
				       "PS print "))))
    (pr-ps-utility-process n-up ifilename outfile)
    (unless ofilename
      (pr-ps-file-ps-print outfile)
      (pr-delete-file outfile))))


;;;###autoload
(defun pr-toggle-file-duplex ()
  "Toggle duplex for PostScript file."
  (interactive)
  (pr-toggle-file-duplex-menu t))


;;;###autoload
(defun pr-toggle-file-tumble ()
  "Toggle tumble for PostScript file.

If tumble is off, produces a printing suitable for binding on the left or
right.
If tumble is on, produces a printing suitable for binding at the top or
bottom."
  (interactive)
  (pr-toggle-file-tumble-menu t))


;;;###autoload
(defun pr-toggle-file-landscape ()
  "Toggle landscape for PostScript file."
  (interactive)
  (pr-toggle-file-landscape-menu t))


;;;###autoload
(defun pr-toggle-ghostscript ()
  "Toggle printing using ghostscript."
  (interactive)
  (pr-toggle-ghostscript-menu t))


;;;###autoload
(defun pr-toggle-faces ()
  "Toggle printing with faces."
  (interactive)
  (pr-toggle-faces-menu t))


;;;###autoload
(defun pr-toggle-spool ()
  "Toggle spooling."
  (interactive)
  (pr-toggle-spool-menu t))


;;;###autoload
(defun pr-toggle-duplex ()
  "Toggle duplex."
  (interactive)
  (pr-toggle-duplex-menu t))


;;;###autoload
(defun pr-toggle-tumble ()
  "Toggle tumble.

If tumble is off, produces a printing suitable for binding on the left or
right.
If tumble is on, produces a printing suitable for binding at the top or
bottom."
  (interactive)
  (pr-toggle-tumble-menu t))


;;;###autoload
(defun pr-toggle-landscape ()
  "Toggle landscape."
  (interactive)
  (pr-toggle-landscape-menu t))


;;;###autoload
(defun pr-toggle-upside-down ()
  "Toggle upside-down."
  (interactive)
  (pr-toggle-upside-down-menu t))


;;;###autoload
(defun pr-toggle-line ()
  "Toggle line number."
  (interactive)
  (pr-toggle-line-menu t))


;;;###autoload
(defun pr-toggle-zebra ()
  "Toggle zebra stripes."
  (interactive)
  (pr-toggle-zebra-menu t))


;;;###autoload
(defun pr-toggle-header ()
  "Toggle printing header."
  (interactive)
  (pr-toggle-header-menu t))


;;;###autoload
(defun pr-toggle-header-frame ()
  "Toggle printing header frame."
  (interactive)
  (pr-toggle-header-frame-menu t))


;;;###autoload
(defun pr-toggle-lock ()
  "Toggle menu lock."
  (interactive)
  (pr-toggle-lock-menu t))


;;;###autoload
(defun pr-toggle-region ()
  "Toggle whether the region is automagically detected."
  (interactive)
  (pr-toggle-region-menu t))


;;;###autoload
(defun pr-toggle-mode ()
  "Toggle auto mode."
  (interactive)
  (pr-toggle-mode-menu t))


;;;###autoload
(defun pr-customize (&rest ignore)
  "Customization of the `printing' group."
  (interactive)
  (customize-group 'printing))


;;;###autoload
(defun lpr-customize (&rest ignore)
  "Customization of the `lpr' group."
  (interactive)
  (customize-group 'lpr))


;;;###autoload
(defun pr-help (&rest ignore)
  "Help for the printing package."
  (interactive)
  (pr-show-setup pr-help-message "*Printing Help*"))


;;;###autoload
(defun pr-ps-name ()
  "Interactively select a PostScript printer."
  (interactive)
  (pr-menu-set-ps-title
   (pr-complete-alist "PostScript printer"
		      pr-ps-printer-alist pr-ps-name)))


;;;###autoload
(defun pr-txt-name ()
  "Interactively select a text printer."
  (interactive)
  (pr-menu-set-txt-title
   (pr-complete-alist "Text printer"
		      pr-txt-printer-alist pr-txt-name)))


;;;###autoload
(defun pr-ps-utility ()
  "Interactively select a PostScript utility."
  (interactive)
  (pr-menu-set-utility-title
   (pr-complete-alist "PostScript utility"
		      pr-ps-utility-alist pr-ps-utility)))


;;;###autoload
(defun pr-show-ps-setup (&rest ignore)
  "Show current ps-print settings."
  (interactive)
  (pr-show-setup (ps-setup) "*PS Setup*"))


;;;###autoload
(defun pr-show-pr-setup (&rest ignore)
  "Show current printing settings."
  (interactive)
  (pr-show-setup (pr-setup) "*PR Setup*"))


;;;###autoload
(defun pr-show-lpr-setup (&rest ignore)
  "Show current lpr settings."
  (interactive)
  (pr-show-setup (lpr-setup) "*LPR Setup*"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fast Commands


;;;###autoload
(defun pr-ps-fast-fire (n-up &optional select)
  "Fast fire function for PostScript printing.

If a region is active, the region will be printed instead of the whole buffer.
Also if the current major-mode is defined in `pr-mode-alist', the settings in
`pr-mode-alist' will be used, that is, the current buffer or region will be
printed using `pr-ps-mode-ps-print'.


Interactively, you have the following situations:

   M-x pr-ps-fast-fire RET
      The command prompts the user for a N-UP value and printing will
      immediately be done using the current active printer.

   C-u   M-x pr-ps-fast-fire RET
   C-u 0 M-x pr-ps-fast-fire RET
      The command prompts the user for a N-UP value and also for a current
      PostScript printer, then printing will immediately be done using the new
      current active printer.

   C-u 1 M-x pr-ps-fast-fire RET
      The command prompts the user for a N-UP value and also for a file name,
      and saves the PostScript image in that file instead of sending it to the
      printer.

   C-u 2 M-x pr-ps-fast-fire RET
      The command prompts the user for a N-UP value, then for a current
      PostScript printer and, finally, for a file name.  Then change the active
      printer to that chosen by user and saves the PostScript image in
      that file instead of sending it to the printer.


Noninteractively, the argument N-UP should be a positive integer greater than
zero and the argument SELECT is treated as follows:

   If it's nil, send the image to the printer.

   If it's a list or an integer lesser or equal to zero, the command prompts
   the user for a current PostScript printer, then printing will immediately
   be done using the new current active printer.

   If it's an integer equal to 1, the command prompts the user for a file name
   and saves the PostScript image in that file instead of sending it to the
   printer.

   If it's an integer greater or equal to 2, the command prompts the user for a
   current PostScript printer and for a file name.  Then change the active
   printer to that chosen by user and saves the PostScript image in that file
   instead of sending it to the printer.

   If it's a symbol which it's defined in `pr-ps-printer-alist', it's the new
   active printer and printing will immediately be done using the new active
   printer.

   Otherwise, send the image to the printer.


Note that this command always behaves as if `pr-auto-region' and `pr-auto-mode'
are both set to t."
  (interactive (list (pr-interactive-n-up (pr-prompt-gs "PS print fast"))
		     current-prefix-arg))
  (let ((pr-auto-region t)
	(pr-auto-mode   t)
	filename)
    (cond ((null select))
	  ((listp select)
	   (pr-ps-name))
	  ((and (symbolp select)
		(assq select pr-ps-printer-alist))
	   (pr-menu-set-ps-title select))
	  ((integerp select)
	   (and (/= select 1)
		(pr-ps-name))
	   (and (>= select 1) (not pr-spool-p)
		(setq filename (pr-ps-outfile-preprint
				(if pr-print-using-ghostscript
				    "Fast GS "
				  "Fast "))))))
    (pr-ps-buffer-ps-print
     (if (integerp n-up)
	 (min (max n-up 1) 100)
       (error "n-up must be an integer greater than zero"))
     filename)))


;;;###autoload
(defun pr-txt-fast-fire (&optional select-printer)
  "Fast fire function for text printing.

If a region is active, the region will be printed instead of the whole buffer.
Also if the current major-mode is defined in `pr-mode-alist', the settings in
`pr-mode-alist' will be used, that is, the current buffer or region will be
printed using `pr-txt-mode'.

Interactively, when you use a prefix argument (C-u), the command prompts the
user for a new active text printer.

Noninteractively, the argument SELECT-PRINTER is treated as follows:

   If it's nil, the printing is sent to the current active text printer.

   If it's a symbol which it's defined in `pr-txt-printer-alist', it's the new
   active printer and printing will immediately be done using the new active
   printer.

   If it's non-nil, the command prompts the user for a new active text printer.

Note that this command always behaves as if `pr-auto-region' and `pr-auto-mode'
are both set to t."
  (interactive (list current-prefix-arg))
  (cond ((null select-printer))
	((and (symbolp select-printer)
	      (assq select-printer pr-txt-printer-alist))
	 (pr-menu-set-txt-title select-printer))
	(t
	 (pr-txt-name)))
  (let ((pr-auto-region t)
	(pr-auto-mode   t))
    (pr-txt-buffer)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities


(defun pr-setup ()
  "Return the current `printing' setup.

This is *not* an interactive command.
One way to see `printing' setup is to switch to a *Scratch* buffer and type:

   M-: (insert (pr-setup)) RET

Or choose the menu option Printing/Show Settings/printing."
  (let (ps-prefix-quote)
    (mapconcat
     #'ps-print-quote
     (list
      (concat "\n;;; printing.el version " pr-version "\n")
      ";; internal vars"
      (ps-comment-string "emacs-version       " emacs-version)
      (ps-comment-string "pr-txt-command      " pr-txt-command)
      (ps-comment-string "pr-txt-switches     "
			 (pr-switches-string pr-txt-switches "pr-txt-switches"))
      (ps-comment-string "pr-txt-printer      " pr-txt-printer)
      (ps-comment-string "pr-ps-command       " pr-ps-command)
      (ps-comment-string "pr-ps-switches      "
			 (pr-switches-string pr-ps-switches "pr-ps-switches"))
      (ps-comment-string "pr-ps-printer-switch" pr-ps-printer-switch)
      (ps-comment-string "pr-ps-printer       " pr-ps-printer)
      (ps-comment-string "pr-cygwin-system    " pr-cygwin-system)
      (ps-comment-string "ps-windows-system   " ps-windows-system)
      (ps-comment-string "ps-lp-system        " ps-lp-system)
      nil
      '(14 . pr-path-style)
      '(14 . pr-path-alist)
      nil
      '(21 . pr-txt-name)
      '(21 . pr-txt-printer-alist)
      nil
      '(20 . pr-ps-name)
      '(20 . pr-ps-printer-alist)
      nil
      '(20 . pr-temp-dir)
      '(20 . pr-ps-temp-file)
      '(20 . pr-file-modes)
      '(20 . pr-delete-temp-file)
      '(20 . pr-list-directory)
      nil
      '(17 . pr-gv-command)
      '(17 . pr-gs-command)
      '(17 . pr-gs-switches)
      '(17 . pr-gs-device)
      '(17 . pr-gs-resolution)
      nil
      '(27 . pr-print-using-ghostscript)
      '(27 . pr-faces-p)
      '(27 . pr-spool-p)
      '(27 . pr-file-landscape)
      '(27 . pr-file-duplex)
      '(27 . pr-file-tumble)
      '(27 . pr-auto-region)
      '(27 . pr-auto-mode)
      nil
      '(20 . pr-ps-utility)
      '(20 . pr-ps-utility-alist)
      nil
      '(14 . pr-mode-alist)
      nil
      '(20 . pr-menu-lock)
      '(20 . pr-menu-char-height)
      '(20 . pr-menu-char-width)
      nil
      '(20 . pr-setting-database)
      nil
      '(22 . pr-visible-entry-list)
      nil
      '(22 . pr-buffer-verbose)
      '(22 . pr-buffer-name)
      '(22 . pr-buffer-name-ignore)
      ")\n\n;;; printing.el - end of settings\n")
     "\n")))


(defun lpr-setup ()
  "Return the current `lpr' setup.

This is *not* an interactive command.
One way to see `lpr' setup is to switch to a *Scratch* buffer and type:

   M-: (insert (lpr-setup)) RET

Or choose the menu option Printing/Show Settings/lpr."
  (let (ps-prefix-quote)
    (mapconcat
     #'ps-print-quote
     (list
      "\n;;; lpr.el settings\n"
      (ps-comment-string "emacs-version" emacs-version)
      nil
      '(25 . printer-name)
      '(25 . lpr-switches)
      '(25 . lpr-add-switches)
      '(25 . lpr-command)
      '(25 . lpr-headers-switches)
      '(25 . print-region-function)
      '(25 . lpr-page-header-program)
      '(25 . lpr-page-header-switches)
      ")\n\n;;; lpr.el - end of settings\n")
     "\n")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mh-e (adapted from mh-e-init.el -- Tom Vogels <tov@ece.cmu.edu>)

(declare-function mh-get-msg-num "mh-utils" (error-if-no-message))
(declare-function mh-show "mh-show" (&optional message redisplay-flag))
(declare-function mh-start-of-uncleaned-message "mh-show" ())
(defvar mh-show-buffer)


(defun pr-article-date ()
  "Find the date of an article or mail message in current buffer.
Return only the dayname, if present, weekday, month, and year."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward
	 "^Date:[ \t]+\\(\\([A-Za-z]+, \\)?[0-9]+ [A-Za-z]+ [0-9]+\\)" nil t)
	(buffer-substring (match-beginning 1) (match-end 1))
      (format-time-string "%Y/%m/%d"))))


(defun pr-mh-current-message ()
  "Go to mh-inbox current message."
  (let ((msg (or (mh-get-msg-num nil) 0)))
    (mh-show)
    (set-buffer mh-show-buffer)
    (goto-char (point-min))
    (mh-start-of-uncleaned-message)
    (message "Printing message %d" msg)))


(defun pr-mh-print-1 (n-up filename header-list)
  "Print mh-inbox current message in PostScript."
  (save-excursion
    (save-window-excursion
      (pr-mh-current-message)
      (pr-mode-print n-up filename header-list (point)))))


(defun pr-mh-lpr-1 (header-list)
  "Print mh-inbox current message in text printer."
  (save-excursion
    (save-window-excursion
      (pr-mh-current-message)
      (pr-mode-lpr header-list (point)))))


(defalias 'pr-mh-print-2 'pr-mode-print)


(defalias 'pr-mh-lpr-2 'pr-mode-lpr)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rmail (hacked from ps-print.el)


(defun pr-rmail-lpr (header-list)
  "Print RMAIL current message in text printer."
  (pr-lpr-message-from-summary header-list
			       'rmail-buffer 'rmail-summary-buffer))


(defun pr-rmail-print (n-up filename header-list)
  "Print RMAIL current message in PostScript."
  (pr-ps-message-from-summary n-up filename header-list
			      'rmail-buffer 'rmail-summary-buffer))


(defun pr-ps-message-from-summary (n-up filename header-list
					summary-buffer summary-default)
  "Print current message in PostScript."
  (let ((buf (or (and (boundp summary-buffer)
		      (symbol-value summary-buffer))
		 (symbol-value summary-default))))
    (and (get-buffer buf)
	 (with-current-buffer buf
	   (pr-mode-print n-up filename header-list)))))


(defun pr-lpr-message-from-summary (header-list summary-buffer summary-default)
  "Print current message in text printer."
  (let ((buf (or (and (boundp summary-buffer)
		      (symbol-value summary-buffer))
		 (symbol-value summary-default))))
    (and (get-buffer buf)
	 (with-current-buffer buf
	   (pr-mode-lpr header-list)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gnus (hacked from ps-print.el)


(defvar pr-gnus-article "*Article*")


(defun pr-gnus-print (n-up filename header-list)
  "Print *Article* current message in PostScript."
  (pr-ps-message-from-summary n-up filename header-list
			      'gnus-article-buffer 'pr-gnus-article))


(defun pr-gnus-lpr (header-list)
  "Print *Article* current message in text printer."
  (pr-lpr-message-from-summary header-list
			       'gnus-article-buffer 'pr-gnus-article))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; vm (hacked from ps-print.el)


(defvar pr-vm-summary "")


(defun pr-vm-print (n-up filename header-list)
  "Print current vm message in PostScript."
  (pr-ps-message-from-summary n-up filename header-list
			      'vm-mail-buffer 'pr-vm-summary))


(defun pr-vm-lpr (header-list)
  "Print current vm message in text printer."
  (pr-lpr-message-from-summary header-list
			       'vm-mail-buffer 'pr-vm-summary))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode Functions


(defun pr-ps-mode (n-up filename)
  "If current major mode is declared, print it in PostScript."
  (let ((args (pr-mode-alist-p)))
    (if args
	(let ((fun (cdr args)))
	  (funcall (car fun) n-up filename (cdr fun))
	  t)
      (ding)
      (message "`%s' major mode not declared." major-mode)
      nil)))


(defmacro pr-local-variable (header-list &rest body)
  `(save-excursion
     (let ((ps-header-lines (or (nth 0 ,header-list) ps-header-lines))
	   (ps-left-header  (or (nth 1 ,header-list) ps-left-header))
	   (ps-right-header (or (nth 2 ,header-list) ps-right-header))
	   ps-razzle-dazzle)
       (let ((local-var-list (pr-eval-local-alist (nthcdr 4 ,header-list))))
	 ,@body
	 (and (nth 3 ,header-list)
	      (pr-kill-local-variable local-var-list))))))


(defun pr-mode-print (n-up filename header-list &optional from to)
  "Print current major mode in PostScript."
  (pr-local-variable
   header-list
   (let ((file (pr-ps-file filename))
	 (start (cond (from)
		      ((pr-region-active-p) (region-beginning))
		      (t nil)
		      )))
     (pr-text2ps (pr-region-active-symbol start) n-up file start
		 (cond (to)
		       ((pr-region-active-p) (region-end))
		       (from (point-max))
		       ))
     (unless (or pr-spool-p filename)
       (pr-ps-file-print file)
       (pr-delete-file file)))))


(defun pr-mode-lpr (header-list &optional from to)
  "Print current major mode in text printer."
  (pr-local-variable
   header-list
   (pr-txt-print (cond (from)
		       ((pr-region-active-p) (region-beginning))
		       (t (point-min)))
		 (cond (to)
		       ((pr-region-active-p) (region-end))
		       (t (point-max))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menu Lock


(defconst pr-menu-entry-alist
  '((postscript         . 3)
    (text               . 3)
    (postscript-options . 9)
    (postscript-process . 3)
    (printing           . 3)
    (help               . 3)
    )
  "Alist that associates menu part with number of items per part.

It's used by `pr-menu-index'.

Each element has the form:

   (MENU-PART . NUMBER-OF-ITEMS)

See `pr-visible-entry-alist'.")


(defun pr-menu-index (entry index)
  (let ((base-list
	 (cond ((eq entry 'text)
		'(postscript))
	       ((eq entry 'postscript-options)
		'(postscript text))
	       ((eq entry 'postscript-process)
		'(postscript text postscript-options))
	       ((eq entry 'printing)
		'(postscript text postscript-options postscript-process))
	       (t
		nil)
	       ))
	key)
    (while base-list
      (setq key       (car base-list)
	    base-list (cdr base-list))
      (and (pr-visible-p key)
	   (setq index (+ index
			  (cdr (assq key pr-menu-entry-alist)))))))
  (+ index 2))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Printer & Utility Selection


(defun pr-update-var (var-sym alist)
  (or (assq (symbol-value var-sym) alist)
      (set var-sym (car (car alist)))))


(defun pr-update-menus (&optional force)
  "Update utility, PostScript and text printer menus.

If FORCE is non-nil, update menus doesn't matter if `pr-ps-printer-alist',
`pr-txt-printer-alist' or `pr-ps-utility-alist' were modified or not;
otherwise, update PostScript printer menu if `pr-ps-printer-menu-modified' is
non-nil, update text printer menu if `pr-txt-printer-menu-modified' is
non-nil, and update PostScript File menus if `pr-ps-utility-menu-modified' is
non-nil.

If menu binding was not done, calls `pr-menu-bind'."
  (interactive "P")
  (if pr-menu-print-item		; since v6.8.4
      ;; There was no menu binding yet, so do it now!
      ;; This is a hack to be compatible with old versions of printing.
      ;; So, user does not need to change printing calling in init files.
      (pr-menu-bind)
    ;; Here menu binding is ok.
    (pr-update-var 'pr-ps-name pr-ps-printer-alist)
    (pr-update-var 'pr-txt-name pr-txt-printer-alist)
    (pr-update-var 'pr-ps-utility pr-ps-utility-alist)
    (pr-do-update-menus force)))


(defun pr-menu-create (name alist var-sym fun entry index)
  (cons name
	(mapcar
	 #'(lambda (elt)
	     (let ((sym (car elt)))
	       (vector
		(symbol-name sym)
		(list fun (list 'quote sym) nil (list 'quote entry) index)
		:style 'radio
		:selected (list 'eq var-sym (list 'quote sym)))))
	 alist)))


(defun pr-ps-set-utility (value)
  (let ((item (cdr (assq value pr-ps-utility-alist))))
    (or item
	(error
	 "Invalid PostScript utility name `%s' for variable `pr-ps-utility'"
	 value))
    (setq pr-ps-utility value)
    (pr-eval-alist (nthcdr 9 item)))
  (pr-update-mode-line))


(defun pr-ps-set-printer (value)
  (let ((ps (cdr (assq value pr-ps-printer-alist))))
    (or ps
	(error
	 "Invalid PostScript printer name `%s' for variable `pr-ps-name'"
	 value))
    (setq pr-ps-name           value
	  pr-ps-command        (pr-dosify-file-name (nth 0 ps))
	  pr-ps-switches       (nth 1 ps)
	  pr-ps-printer-switch (nth 2 ps)
	  pr-ps-printer        (nth 3 ps))
    (or (stringp pr-ps-command)
	(setq pr-ps-command
	      (cond (ps-windows-system "print")
		    (ps-lp-system      "lp")
		    (t                 "lpr")
		    )))
    (or (stringp pr-ps-printer-switch)
	(setq pr-ps-printer-switch
	      (cond (ps-windows-system "/D:")
		    (ps-lp-system      "-d")
		    (t                 "-P")
		    )))
    (pr-eval-alist (nthcdr 4 ps)))
  (pr-update-mode-line))


(defun pr-txt-set-printer (value)
  (let ((txt (cdr (assq value pr-txt-printer-alist))))
    (or txt
	(error "Invalid text printer name `%s' for variable `pr-txt-name'"
	       value))
    (setq pr-txt-name     value
	  pr-txt-command  (pr-dosify-file-name (nth 0 txt))
	  pr-txt-switches (nth 1 txt)
	  pr-txt-printer  (nth 2 txt)))
  (or (stringp pr-txt-command)
      (setq pr-txt-command
	    (cond (ps-windows-system "print")
		  (ps-lp-system      "lp")
		  (t                 "lpr")
		  )))
  (pr-update-mode-line))


(defun pr-eval-alist (alist)
  (mapcar #'(lambda (option)
	      (let ((var-sym (car option))
		    (value   (cdr option)))
		(if (eq var-sym 'inherits-from:)
		    (pr-eval-setting-alist value 'global)
		  (set var-sym (eval value)))))
	  alist))


(defun pr-eval-local-alist (alist)
  (let (local-list)
    (mapc #'(lambda (option)
	      (let ((var-sym (car option))
		    (value   (cdr option)))
		(setq local-list
		      (if (eq var-sym 'inherits-from:)
			  (nconc (pr-eval-setting-alist value) local-list)
			(set (make-local-variable var-sym) (eval value))
			(cons var-sym local-list)))))
	  alist)
    local-list))


(defun pr-eval-setting-alist (key &optional global old)
  (let ((setting (cdr (assq key pr-setting-database))))
    (and setting
	 (let ((inherits (nth 0 setting))
	       (local    (nth 1 setting))
	       (kill     (nth 2 setting))
	       local-list)
	   (and local global
		(progn
		  (ding)
		  (message "There are local buffer settings for `%S'." key)
		  (setq global nil)))
	   (and inherits
		(if (memq inherits old)
		    (error "Circular inheritance for `%S'" inherits)
		  (setq local-list
			(pr-eval-setting-alist inherits global
					       (cons inherits old)))))
	   (mapc
	    (cond ((not local)		; global settings
		   #'(lambda (option)
		       (let ((var-sym (car option)))
			 (or (eq var-sym 'inherits-from:)
			     (set var-sym (eval (cdr option)))))))
		  (kill			; local settings with killing
		   #'(lambda (option)
		       (let ((var-sym (car option)))
			 (unless (eq var-sym 'inherits-from:)
			   (setq local-list (cons var-sym local-list))
			   (set (make-local-variable var-sym)
				(eval (cdr option)))))))
		  (t			; local settings without killing
		   #'(lambda (option)
		       (let ((var-sym (car option)))
			 (or (eq var-sym 'inherits-from:)
			     (set (make-local-variable var-sym)
				  (eval (cdr option))))))))
	    (nthcdr 3 setting))
	   local-list))))


(defun pr-kill-local-variable (local-var-list)
  (mapcar 'kill-local-variable local-var-list))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal Functions (II)


(defun pr-toggle (var-sym mess entry index horizontal state
			  &optional path no-menu)
  (set var-sym (not (symbol-value var-sym)))
  (message "%s is %s" mess (if (symbol-value var-sym) "on" "off"))
  (or no-menu
      (pr-menu-lock entry index horizontal state path)))


(defun pr-toggle-file-duplex-menu (&optional no-menu)
  "Toggle whether to print PostScript files in duplex mode."
  (interactive)
  (pr-toggle 'pr-file-duplex "PS file duplex" nil 7 5 nil
	     '("PostScript Print" "File") no-menu))


(defun pr-toggle-file-tumble-menu (&optional no-menu)
  "Toggle whether to print PostScript files in tumble mode."
  (interactive)
  (pr-toggle 'pr-file-tumble "PS file tumble" nil 8 5 nil
	     '("PostScript Print" "File") no-menu))


(defun pr-toggle-file-landscape-menu (&optional no-menu)
  "Toggle whether to print PostScript files in landscape orientation."
  (interactive)
  (pr-toggle 'pr-file-landscape "PS file landscape" nil 6 5 nil
	     '("PostScript Print" "File") no-menu))


(defun pr-toggle-ghostscript-menu (&optional no-menu)
  "Toggle whether to print using ghostscript."
  (interactive)
  (pr-toggle 'pr-print-using-ghostscript "Printing using ghostscript"
	     'postscript-process 2 12 'toggle nil no-menu))


(defun pr-toggle-faces-menu (&optional no-menu)
  "Toggle whether to print with face attributes."
  (interactive)
  (pr-toggle 'pr-faces-p "Printing with faces"
	     'postscript-process 1 12 'toggle nil no-menu))


(defun pr-toggle-spool-menu (&optional no-menu)
  "Toggle whether to spool printing in a buffer."
  (interactive)
  (pr-toggle 'pr-spool-p "Spooling printing"
	     'postscript-process 0 12 'toggle nil no-menu))


(defun pr-toggle-duplex-menu (&optional no-menu)
  "Toggle whether to generate PostScript for a two-sided printer."
  (interactive)
  (pr-toggle 'ps-spool-duplex "Printing duplex"
	     'postscript-options 5 12 'toggle nil no-menu))


(defun pr-toggle-tumble-menu (&optional no-menu)
  "Toggle how pages on opposite sides of a sheet are oriented."
  (interactive)
  (pr-toggle 'ps-spool-tumble "Tumble"
	     'postscript-options 6 12 'toggle nil no-menu))


(defun pr-toggle-landscape-menu (&optional no-menu)
  "Toggle whether to print in landscape mode."
  (interactive)
  (pr-toggle 'ps-landscape-mode "Landscape"
	     'postscript-options 0 12 'toggle nil no-menu))


(defun pr-toggle-upside-down-menu (&optional no-menu)
  "Toggle whether to print upside-down (that is, rotated by 180 degrees)."
  (interactive)
  (pr-toggle 'ps-print-upside-down "Upside-Down"
	     'postscript-options 7 12 'toggle nil no-menu))


(defun pr-toggle-line-menu (&optional no-menu)
  "Toggle whether to means print line numbers."
  (interactive)
  (pr-toggle 'ps-line-number "Line number"
	     'postscript-options 3 12 'toggle nil no-menu))


(defun pr-toggle-zebra-menu (&optional no-menu)
  "Toggle whether to print zebra stripes."
  (interactive)
  (pr-toggle 'ps-zebra-stripes "Zebra stripe"
	     'postscript-options 4 12 'toggle nil no-menu))


(defun pr-toggle-header-menu (&optional no-menu)
  "Toggle whether to print a header at the top of each page."
  (interactive)
  (pr-toggle 'ps-print-header "Print header"
	     'postscript-options 1 12 'toggle nil no-menu))


(defun pr-toggle-header-frame-menu (&optional no-menu)
  "Toggle whether to draw a gaudy frame around the header."
  (interactive)
  (pr-toggle 'ps-print-header-frame "Print header frame"
	     'postscript-options 2 12 'toggle nil no-menu))


(defun pr-toggle-lock-menu (&optional no-menu)
  "Toggle whether the menu is locked while selecting toggle options."
  (interactive)
  (pr-toggle 'pr-menu-lock "Menu lock"
	     'printing 2 12 'toggle nil no-menu))


(defun pr-toggle-region-menu (&optional no-menu)
  "Toggle whether the region is automagically detected."
  (interactive)
  (pr-toggle 'pr-auto-region "Auto region"
	     'printing 0 12 'toggle nil no-menu))


(defun pr-toggle-mode-menu (&optional no-menu)
  "Toggle whether major-mode specific printing is preferred over normal printing."
  (interactive)
  (pr-toggle 'pr-auto-mode "Auto mode"
	     'printing 1 12 'toggle nil no-menu))


(defun pr-prompt (str)
  (if (pr-auto-mode-p)
      (concat str " mode")
    (pr-region-active-string str)))


(defun pr-prompt-region (str)
  (concat str (if (pr-auto-mode-p)
		  " mode"
		" region")))


(defun pr-prompt-gs (str)
  (if (pr-using-ghostscript-p)
      (concat str " GS")
    str))


(defun pr-region-active-symbol (&optional region-p)
  (if (or region-p (pr-region-active-p))
      'region
    'buffer))


(defun pr-region-active-string (prefix)
  (concat prefix
	  (if (pr-region-active-p)
	      " region"
	    " buffer")))


(defun pr-show-setup (settings buffer-name)
  (with-output-to-temp-buffer buffer-name
    (princ settings)
    (help-print-return-message)))


(defun pr-complete-alist (prompt alist default)
  (let ((collection (mapcar #'(lambda (elt)
				(setq elt (car elt))
				(cons (symbol-name elt) elt))
			    alist)))
    (cdr (assoc (completing-read (concat prompt ": ")
				 collection nil t
				 (symbol-name default) nil
				 (symbol-name default))
		collection))))


(defun pr-delete-file (file)
  (and pr-delete-temp-file (file-exists-p file)
       (delete-file file)))


(defun pr-expand-file-name (filename)
  (pr-dosify-file-name (expand-file-name filename)))


(defun pr-ps-outfile-preprint (&optional mess)
  (let* ((prompt (format "%soutput PostScript file name: " (or mess "")))
	 (res    (read-file-name prompt default-directory "" nil)))
    (while (cond ((not (file-writable-p res))
		  (ding)
		  (setq prompt "is unwritable"))
		 ((file-directory-p res)
		  (ding)
		  (setq prompt "is a directory"))
		 ((file-exists-p res)
		  (ding)
		  (setq prompt "exists")
		  (not (y-or-n-p (format "File `%s' exists; overwrite? "
					 res))))
		 (t nil))
      (setq res (read-file-name
		 (format "File %s; PostScript file: " prompt)
		 (file-name-directory res) nil nil
		 (file-name-nondirectory res))))
    (pr-expand-file-name res)))


(defun pr-ps-infile-preprint (&optional mess)
  (let* ((prompt (format "%sinput PostScript file name: " (or mess "")))
	 (res    (read-file-name prompt default-directory "" nil)))
    (while (cond ((not (file-exists-p res))
		  (ding)
		  (setq prompt "doesn't exist"))
		 ((not (file-readable-p res))
		  (ding)
		  (setq prompt "is unreadable"))
		 ((file-directory-p res)
		  (ding)
		  (setq prompt "is a directory"))
		 (t nil))
      (setq res (read-file-name
		 (format "File %s; PostScript file: " prompt)
		 (file-name-directory res) nil nil
		 (file-name-nondirectory res))))
    (pr-expand-file-name res)))


(defun pr-ps-utility-args (n-up-sym infile-sym outfile-sym prompt)
  ;; check arguments for PostScript file processing.
  ;; n-up
  (or (symbol-value n-up-sym)
      (set n-up-sym (pr-interactive-n-up prompt)))
  ;; input file
  (and (eq (symbol-value infile-sym) t)
       (set infile-sym (pr-ps-infile-preprint prompt)))
  (or (symbol-value infile-sym)
      (error "%s: input PostScript file name is missing" prompt))
  (set infile-sym (pr-dosify-file-name (symbol-value infile-sym)))
  ;; output file
  (and (eq (symbol-value outfile-sym) t)
       (set outfile-sym (and current-prefix-arg
			     (pr-ps-outfile-preprint prompt))))
  (and (symbol-value outfile-sym)
       (set outfile-sym (pr-dosify-file-name (symbol-value outfile-sym))))
  (pr-ps-file (symbol-value outfile-sym)))


(defun pr-ps-utility-process (n-up infile outfile)
  ;; activate utility to process a PostScript file.
  (let (item)
    (and (stringp infile) (file-exists-p infile)
	 (setq item (cdr (assq pr-ps-utility pr-ps-utility-alist)))
	 (pr-call-process (nth 0 item)
			  (pr-switches-string (nth 1 item)
					      "pr-ps-utility-alist entry")
			  (pr-switches-string (nth 8 item)
					      "pr-ps-utility-alist entry")
			  (and (nth 2 item)
			       (format (nth 2 item) ps-paper-type))
			  (format (nth 3 item) n-up)
			  (and pr-file-landscape (nth 4 item))
			  (and pr-file-duplex    (nth 5 item))
			  (and pr-file-tumble    (nth 6 item))
			  (pr-expand-file-name infile)
			  (nth 7 item)
			  (pr-expand-file-name outfile)))))


(defun pr-remove-nil-from-list (lst)
  (while (and lst (null (car lst)))
    (setq lst (cdr lst)))
  (let ((b lst)
	(l (cdr lst)))
    (while l
      (if (car l)
	  (setq b l
		l (cdr l))
	(setq l (cdr l))
	(setcdr b l))))
  lst)


(defun pr-call-process (command &rest args)
  (let ((buffer (get-buffer-create "*Printing Command Output*"))
	(cmd    (pr-command command))
	status)
    (setq args (pr-remove-nil-from-list args))
    ;; *Printing Command Output* == show command & args
    (with-current-buffer buffer
      (goto-char (point-max))
      (insert (format "%s %S\n" cmd args)))
    ;; *Printing Command Output* == show any return message from command
    (pr-save-file-modes
     (setq status
	   (condition-case data
	       (apply 'call-process cmd nil buffer nil args)
	     ((quit error)
	      (error-message-string data)))))
    ;; *Printing Command Output* == show exit status
    (with-current-buffer buffer
      (goto-char (point-max))
      (insert (format "Exit status: %s\n\n" status)))
    ;; message if error status
    (if (or (stringp status)
	    (and (integerp status) (/= status 0)))
	(message
	 "Printing error status: %s (see *Printing Command Output* buffer)"
	 status))))


(defun pr-txt-print (from to)
  (let ((lpr-command  (pr-standard-file-name (pr-command pr-txt-command)))
	(lpr-switches (pr-switches pr-txt-switches "pr-txt-switches"))
	(printer-name pr-txt-printer))
    (lpr-region from to)))


(defun pr-switches-string (switches mess)
  ;; If SWITCHES is nil, return nil.
  ;; Otherwise, return the list of string in a string.
  (and switches
       (mapconcat 'identity (pr-switches switches mess) " ")))


(defun pr-switches (switches mess)
  (or (listp switches)
      (error "%S should have a list of strings" mess))
  (ps-flatten-list			; dynamic evaluation
   (mapcar 'ps-eval-switch switches)))


(defun pr-ps-preview (kind n-up filename mess)
  (pr-set-n-up-and-filename 'n-up 'filename mess)
  (let ((file (pr-ps-file filename)))
    (pr-text2ps kind n-up file)
    (or pr-spool-p (pr-ps-file-preview file))))


(defun pr-ps-using-ghostscript (kind n-up filename mess)
  (pr-set-n-up-and-filename 'n-up 'filename mess)
  (let ((file (pr-ps-file filename)))
    (pr-text2ps kind n-up file)
    (unless (or pr-spool-p filename)
      (pr-ps-file-using-ghostscript file)
      (pr-delete-file file))))


(defun pr-ps-print (kind n-up filename mess)
  (pr-set-n-up-and-filename 'n-up 'filename mess)
  (let ((file (pr-ps-file filename)))
    (pr-text2ps kind n-up file)
    (unless (or pr-spool-p filename)
      (pr-ps-file-print file)
      (pr-delete-file file))))


(defun pr-ps-file (&optional filename)
  (pr-dosify-file-name (or filename
			   (make-temp-file
			    (convert-standard-filename
			     (expand-file-name pr-ps-temp-file pr-temp-dir))
			    nil ".ps"))))


(defun pr-interactive-n-up (mess)
  (or (stringp mess) (setq mess "*"))
  (save-match-data
    (let* ((fmt-prompt "%s[%s] N-up printing (default 1): ")
	   (prompt "")
	   (str (pr-read-string (format fmt-prompt prompt mess) "1" nil "1"))
	   int)
      (while (if (string-match "^\\s *[0-9]+$" str)
		 (setq int (string-to-number str)
		       prompt (cond ((< int 1)   "Integer below 1; ")
				    ((> int 100) "Integer above 100; ")
				    (t           nil)))
	       (setq prompt "Invalid integer syntax; "))
	(ding)
	(setq str
	      (pr-read-string (format fmt-prompt prompt mess) str nil "1")))
      int)))


(defun pr-interactive-dir (mess)
  (let* ((dir-name   (file-name-directory (or (buffer-file-name)
					      default-directory)))
	 (fmt-prompt (concat "%s[" mess "] Directory to print: "))
	 (dir        (read-directory-name (format fmt-prompt "")
					  "" dir-name nil dir-name))
	 prompt)
    (while (cond ((not (file-directory-p dir))
		  (ding)
		  (setq prompt "It's not a directory! "))
		 ((not (file-readable-p dir))
		  (ding)
		  (setq prompt "Directory is unreadable! "))
		 (t nil))
      (setq dir-name (file-name-directory dir)
	    dir      (read-directory-name (format fmt-prompt prompt)
					  "" dir-name nil dir-name)))
    (file-name-as-directory dir)))


(defun pr-interactive-regexp (mess)
  (pr-read-string (format "[%s] File regexp to print: " mess) "" nil ""))


(defun pr-interactive-dir-args (mess)
  (list
   ;; get directory argument
   (pr-interactive-dir mess)
   ;; get file name regexp
   (pr-interactive-regexp mess)))


(defun pr-interactive-ps-dir-args (mess)
  (list
   ;; get n-up argument
   (pr-interactive-n-up mess)
   ;; get directory argument
   (pr-interactive-dir mess)
   ;; get file name regexp
   (pr-interactive-regexp mess)
   ;; get output file name
   (and (not pr-spool-p)
	(ps-print-preprint current-prefix-arg))))


(defun pr-interactive-n-up-file (mess)
  (list
   ;; get n-up argument
   (pr-interactive-n-up mess)
   ;; get output file name
   (and (not pr-spool-p)
	(ps-print-preprint current-prefix-arg))))


(defun pr-interactive-n-up-inout (mess)
  (list
   ;; get n-up argument
   (pr-interactive-n-up mess)
   ;; get input file name
   (pr-ps-infile-preprint (concat mess " "))
   ;; get output file name
   (ps-print-preprint current-prefix-arg)))


(defun pr-set-outfilename (filename-sym)
  (and (not pr-spool-p)
       (eq (symbol-value filename-sym) t)
       (set filename-sym (and current-prefix-arg
			      (ps-print-preprint current-prefix-arg))))
  (and (symbol-value filename-sym)
       (set filename-sym (pr-dosify-file-name (symbol-value filename-sym)))))


(defun pr-set-n-up-and-filename (n-up-sym filename-sym mess)
  ;; n-up
  (or (symbol-value n-up-sym)
      (set n-up-sym (pr-interactive-n-up mess)))
  ;; output file
  (pr-set-outfilename filename-sym))


(defun pr-set-dir-args (dir-sym regexp-sym mess)
  ;; directory
  (or (symbol-value dir-sym)
      (set dir-sym (pr-interactive-dir mess)))
  ;; file name regexp
  (or (symbol-value regexp-sym)
      (set regexp-sym (pr-interactive-regexp mess))))


(defun pr-set-ps-dir-args (n-up-sym dir-sym regexp-sym filename-sym mess)
  ;; n-up
  (or (symbol-value n-up-sym)
      (set n-up-sym (pr-interactive-n-up mess)))
  ;; directory & file name regexp
  (pr-set-dir-args dir-sym regexp-sym mess)
  ;; output file
  (pr-set-outfilename filename-sym))


(defun pr-find-buffer-visiting (file)
  (if (not (file-directory-p file))
      (find-buffer-visiting (if ps-windows-system
				(downcase file)
			      file))
    (let ((truename (file-truename file))
	  (blist (buffer-list))
	  found)
      (while (and (not found) blist)
	(with-current-buffer (car blist)
	  (and (eq major-mode 'dired-mode)
	       (save-excursion
		 (goto-char (point-min))
		 (string= (buffer-substring-no-properties
			   (+ (point-min) 2)
			   (progn
			     (end-of-line)
			     (1- (point))))
			  truename))
	       (setq found (car blist))))
	(setq blist (cdr blist)))
      found)))


(defun pr-file-list (dir file-regexp fun)
  (mapcar #'(lambda (file)
	      (and (or pr-list-directory
		       (not (file-directory-p file)))
		   (let ((buffer (pr-find-buffer-visiting file))
			 pop-up-windows
			 pop-up-frames)
		     (and (or buffer
			      (file-readable-p file))
			  (with-current-buffer (or buffer
                                                   (find-file-noselect file))
			    (funcall fun)
			    (or buffer
				(kill-buffer (current-buffer))))))))
	  (directory-files dir t file-regexp)))


(defun pr-delete-file-if-exists (filename)
  (and (not pr-spool-p) (stringp filename) (file-exists-p filename)
       (delete-file filename)))


(defun pr-ps-file-list (n-up dir file-regexp filename)
  (pr-delete-file-if-exists (setq filename (pr-expand-file-name filename)))
  (let ((pr-spool-p t))
    (pr-file-list dir file-regexp
		  #'(lambda ()
		      (if (pr-auto-mode-p)
			  (pr-ps-mode n-up filename)
			(pr-text2ps 'buffer n-up filename)))))
  (or pr-spool-p
      (pr-despool-print filename)))


(defun pr-text2ps (kind n-up filename &optional from to)
  (pr-save-file-modes
   (let ((ps-n-up-printing n-up)
	 (ps-spool-config (and (eq ps-spool-config 'setpagedevice)
			       'setpagedevice)))
     (pr-delete-file-if-exists filename)
     (cond (pr-faces-p
	    (cond (pr-spool-p
		   ;; pr-faces-p and pr-spool-p
		   ;; here FILENAME arg is ignored
		   (cond ((eq kind 'buffer)
			  (ps-spool-buffer-with-faces))
			 ((eq kind 'region)
			  (ps-spool-region-with-faces (or from (point))
						      (or to (mark))))
			 ))
		  ;; pr-faces-p and not pr-spool-p
		  ((eq kind 'buffer)
		   (ps-print-buffer-with-faces filename))
		  ((eq kind 'region)
		   (ps-print-region-with-faces (or from (point))
					       (or to (mark)) filename))
		  ))
	   (pr-spool-p
	    ;; not pr-faces-p and pr-spool-p
	    ;; here FILENAME arg is ignored
	    (cond ((eq kind 'buffer)
		   (ps-spool-buffer))
		  ((eq kind 'region)
		   (ps-spool-region (or from (point)) (or to (mark))))
		  ))
	   ;; not pr-faces-p and not pr-spool-p
	   ((eq kind 'buffer)
	    (ps-print-buffer filename))
	   ((eq kind 'region)
	    (ps-print-region (or from (point)) (or to (mark)) filename))
	   ))))


(defun pr-command (command)
  "Return absolute file name specification for COMMAND.

If COMMAND is an empty string, return it.

If COMMAND is already an absolute file name specification, return it.
Else it uses `pr-path-alist' to find COMMAND, if find it then return it;
otherwise, gives an error.

When using `pr-path-alist' to find COMMAND, the entries `cygwin', `windows' and
`unix' are used (see `pr-path-alist' for documentation).

If Emacs is running on Windows 95/98/NT/2000, tries to find COMMAND,
COMMAND.exe, COMMAND.bat and COMMAND.com in this order."
  (if (string= command "")
      command
    (pr-dosify-file-name
     (or (pr-find-command command)
	 (pr-path-command (cond (pr-cygwin-system  'cygwin)
				(ps-windows-system 'windows)
				(t                 'unix))
			  (file-name-nondirectory command)
			  nil)
	 (error "Command not found: %s"
		(file-name-nondirectory command))))))


(defun pr-path-command (symbol command sym-list)
  (let ((lpath (cdr (assq symbol pr-path-alist)))
	cmd)
    ;; PATH expansion
    (and (eq symbol 'PATH) (null lpath)
	 (setq lpath (parse-colon-path (getenv "PATH"))))
    (while (and lpath
		(not
		 (setq cmd
		       (let ((path (car lpath)))
			 (cond
			  ;; symbol expansion
			  ((symbolp path)
			   (and (not (memq path sym-list))
				(pr-path-command path command
						 (cons path sym-list))))
			  ;; normal path
			  ((stringp path)
			   (pr-find-command
			    (expand-file-name
			     (substitute-in-file-name
			      (concat (file-name-as-directory path)
				      command)))))
			  )))))
      (setq lpath (cdr lpath)))
    cmd))


(defun pr-find-command (cmd)
  (if ps-windows-system
      ;; windows system
      (let ((ext (cons (file-name-extension cmd t)
		       (list ".exe" ".bat" ".com")))
	    found)
	(setq cmd (file-name-sans-extension cmd))
	(while (and ext
		    (setq found (concat cmd (car ext)))
		    (not (and (file-regular-p found)
			      (file-executable-p found))))
	  (setq ext (cdr ext)
		found nil))
	found)
    ;; non-windows systems
    (and (file-regular-p cmd)
	 (file-executable-p cmd)
	 cmd)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Printing Interface (inspired by ps-print-interface.el)


(eval-when-compile
  (require 'cus-edit)
  (require 'wid-edit)
  (require 'widget))


(defvar pr-i-window-configuration nil)

(defvar pr-i-buffer     nil)
(defvar pr-i-region     nil)
(defvar pr-i-mode       nil)
(defvar pr-i-despool    nil)
(defvar pr-i-ps-as-is   t)
(defvar pr-i-n-up       1)
(defvar pr-i-directory  "./")
(defvar pr-i-regexp     "")
(defvar pr-i-ps-file    "")
(defvar pr-i-out-file   "")
(defvar pr-i-answer-yes nil)
(defvar pr-i-process    'buffer)
(defvar pr-i-ps-send    'printer)


(defvar pr-interface-map nil
  "Keymap for pr-interface.")

(unless pr-interface-map
  (let ((map (make-sparse-keymap)))
    (cond ((featurep 'xemacs)		; XEmacs
	   (pr-set-keymap-parents map (list widget-keymap))
	   (pr-set-keymap-name    map 'pr-interface-map))
	  (t				; GNU Emacs
	   (pr-set-keymap-parents map widget-keymap)))
    (define-key map "q" 'pr-interface-quit)
    (define-key map "?" 'pr-interface-help)
    (setq pr-interface-map map)))


(defmacro pr-interface-save (&rest body)
  `(with-current-buffer pr-i-buffer
     ,@body))


(defun pr-create-interface ()
  "Create the front end for printing package."
  (setq pr-i-buffer (buffer-name (current-buffer))
	pr-i-region (ps-mark-active-p)
	pr-i-mode   (pr-mode-alist-p)
	pr-i-window-configuration (current-window-configuration))

  (put 'pr-i-process 'pr-widget-list nil)
  (put 'pr-i-ps-send 'pr-widget-list nil)

  (delete-other-windows)
  (kill-buffer (get-buffer-create pr-buffer-name))
  (switch-to-buffer (get-buffer-create pr-buffer-name))

  ;; header
  (let ((versions (concat "printing v" pr-version
			  "    ps-print v" ps-print-version)))
    ;; to keep compatibility with Emacs 20 & 21:
    ;; DO NOT REPLACE `?\ ' BY `?\s'
    (widget-insert (make-string (- 79 (length versions)) ?\ ) versions))
  (pr-insert-italic "\nCurrent Directory : " 1)
  (pr-insert-italic default-directory)

  (pr-insert-section-1)			; 1. Print
  (pr-insert-section-2)			; 2. PostScript Printer
  (pr-insert-section-3)			; 3. Text Printer

  ;; separator
  (widget-insert "\n\n " (make-string 77 ?-))

  (pr-insert-section-4)			; 4. Settings
  (pr-insert-section-5)			; 5. Customize
  (pr-insert-section-6)			; 6. Show Settings
  (pr-insert-section-7)			; 7. Help

  (use-local-map pr-interface-map)
  (widget-setup)
  (goto-char (point-min))

  (and pr-i-region			; let region activated
       (pr-keep-region-active)))


(defun pr-insert-section-1 ()
  ;; 1. Print:
  (pr-insert-italic "\nPrint :" 1)

  ;;    1a. Buffer:
  ;;    1a. Buffer: Buffer List
  (pr-insert-radio-button 'pr-i-process 'buffer)
  (pr-insert-menu "Buffer List" 'pr-i-buffer
		  (let ((blist (buffer-list))
			case-fold-search choices)
		    (while blist
		      (let ((name (buffer-name (car blist)))
			    (ignore pr-buffer-name-ignore)
			    found)
			(setq blist (cdr blist))
			(while (and ignore (not found))
			  (setq found  (string-match (car ignore) name)
				ignore (cdr ignore)))
			(or found
			    (setq choices
				  (cons (list 'quote
					      (list 'choice-item
						    :format "%[%t%]"
						    name))
					choices)))))
		    (nreverse choices))
		  " Buffer : " nil
		  '(progn
		     (pr-interface-save
		      (setq pr-i-region (ps-mark-active-p)
			    pr-i-mode   (pr-mode-alist-p)))
		     (pr-update-checkbox 'pr-i-region)
		     (pr-update-checkbox 'pr-i-mode)))
  ;;    1a. Buffer: Region
  (put 'pr-i-region 'pr-widget
       (pr-insert-checkbox
	"\n               "
	'pr-i-region
	#'(lambda (widget &rest ignore)
	    (let ((region-p (pr-interface-save
			     (ps-mark-active-p))))
	      (cond ((null (widget-value widget)) ; widget is nil
		     (setq pr-i-region nil))
		    (region-p		; widget is true and there is a region
		     (setq pr-i-region t)
		     (widget-value-set widget t)
		     (widget-setup))	; MUST be called after widget-value-set
		    (t			; widget is true and there is no region
		     (ding)
		     (message "There is no region active")
		     (setq pr-i-region nil)
		     (widget-value-set widget nil)
		     (widget-setup)))))	; MUST be called after widget-value-set
	" Region"))
  ;;    1a. Buffer: Mode
  (put 'pr-i-mode 'pr-widget
       (pr-insert-checkbox
	"    "
	'pr-i-mode
	#'(lambda (widget &rest ignore)
	    (let ((mode-p (pr-interface-save
			   (pr-mode-alist-p))))
	      (cond
	       ((null (widget-value widget)) ; widget is nil
		(setq pr-i-mode nil))
	       (mode-p			; widget is true and there is a `mode'
		(setq pr-i-mode t)
		(widget-value-set widget t)
		(widget-setup))		; MUST be called after widget-value-set
	       (t			; widget is true and there is no `mode'
		(ding)
		(message
		 "This buffer isn't in a mode that printing treats specially.")
		(setq pr-i-mode nil)
		(widget-value-set widget nil)
		(widget-setup)))))	; MUST be called after widget-value-set
	" Mode\n"))

  ;;    1b. Directory:
  (pr-insert-radio-button 'pr-i-process 'directory)
  (widget-create
   'directory
   :size 58
   :format " Directory   : %v"
   :notify 'pr-interface-directory
   :action (lambda (widget &optional event)
	     (if (pr-interface-directory widget)
		 (pr-widget-field-action widget event)
	       (ding)
	       (message "Please specify a readable directory")))
   pr-i-directory)
  ;;    1b. Directory: File Regexp
  (widget-create 'regexp
		 :size 58
		 :format "\n      File Regexp : %v\n"
		 :notify (lambda (widget &rest ignore)
			   (setq pr-i-regexp (widget-value widget)))
		 pr-i-regexp)
  ;;    1b. Directory: List Directory Entry
  (widget-insert "                    ")
  (pr-insert-toggle 'pr-list-directory " List Directory Entry\n")

  ;;    1c. PostScript File:
  (pr-insert-radio-button 'pr-i-process 'file)
  (widget-create
   'file
   :size 51
   :format " PostScript File    : %v"
   :notify 'pr-interface-infile
   :action (lambda (widget &rest event)
	     (if (pr-interface-infile widget)
		 (pr-widget-field-action widget event)
	       (ding)
	       (message "Please specify a readable PostScript file")))
   pr-i-ps-file)
  ;;    1c. PostScript File: PostScript Utility
  (pr-insert-menu "PostScript Utility" 'pr-ps-utility
		  (pr-choice-alist pr-ps-utility-alist)
		  "\n      PostScript Utility : "
		  "    ")
  ;;    1c. PostScript File: No Preprocessing
  (pr-insert-toggle 'pr-i-ps-as-is " No Preprocessing"))


(defun pr-insert-section-2 ()
  ;; 2. PostScript Printer:
  ;; 2. PostScript Printer: PostScript Printer List
  (pr-insert-italic "\n\nPostScript Printer : " 2 20)
  (pr-insert-menu "PostScript Printer" 'pr-ps-name
		  (pr-choice-alist pr-ps-printer-alist))
  ;; 2. PostScript Printer: Despool
  (put 'pr-i-despool 'pr-widget
       (pr-insert-checkbox
	"    "
	'pr-i-despool
	#'(lambda (widget &rest ignore)
	    (if pr-spool-p
		(setq pr-i-despool (not pr-i-despool))
	      (ding)
	      (message "Can despool only when spooling is actually selected")
	      (setq pr-i-despool nil))
	    (widget-value-set widget pr-i-despool)
	    (widget-setup))		; MUST be called after widget-value-set
	" Despool   "))
  ;; 2. PostScript Printer: Preview    Print    Quit
  (pr-insert-button 'pr-interface-preview "Preview" "   ")
  (pr-insert-button 'pr-interface-ps-print "Print" "   ")
  (pr-insert-button 'pr-interface-quit "Quit")
  ;; 2. PostScript Printer: Send to Printer/Temporary File
  (pr-insert-radio-button 'pr-i-ps-send 'printer)
  (widget-insert " Send to Printer/Temporary File")
  ;; 2. PostScript Printer: Send to File
  (pr-insert-radio-button 'pr-i-ps-send 'file)
  (widget-create
   'file
   :size 57
   :format " Send to File : %v"
   :notify 'pr-interface-outfile
   :action (lambda (widget &rest event)
	     (if (and (pr-interface-outfile widget)
		      (or (not (file-exists-p pr-i-out-file))
			  (setq pr-i-answer-yes
				(y-or-n-p "File exists; overwrite? "))))
		 (pr-widget-field-action widget event)
	       (ding)
	       (message "Please specify a writable PostScript file")))
   pr-i-out-file)
  ;; 2. PostScript Printer: N-Up
  (widget-create
   'integer
   :size 3
   :format "\n  N-Up : %v"
   :notify (lambda (widget &rest ignore)
	     (let ((value (if (string= (widget-apply widget :value-get) "")
			      0
			    (widget-value widget))))
	       (if (and (integerp value)
			(<= 1 value) (<= value 100))
		   (progn
		     (message " ")
		     (setq pr-i-n-up value))
		 (ding)
		 (message "Please specify an integer between 1 and 100"))))
   pr-i-n-up))


(defun pr-insert-section-3 ()
  ;; 3. Text Printer:
  (pr-insert-italic "\n\nText Printer : " 2 14)
  (pr-insert-menu "Text Printer" 'pr-txt-name
		  (pr-choice-alist pr-txt-printer-alist)
		  nil "    ")
  (pr-insert-button 'pr-interface-printify "Printify" "   ")
  (pr-insert-button 'pr-interface-txt-print "Print" "   ")
  (pr-insert-button 'pr-interface-quit "Quit"))


(defun pr-insert-section-4 ()
  ;; 4. Settings:
  ;; 4. Settings: Landscape             Auto Region    Verbose
  (pr-insert-checkbox "\n\n  " 'ps-landscape-mode
		      #'(lambda (&rest ignore)
			  (setq ps-landscape-mode (not ps-landscape-mode)
				pr-file-landscape ps-landscape-mode))
		      " Landscape             ")
  (pr-insert-toggle 'pr-auto-region " Auto Region                ")
  (pr-insert-toggle 'pr-buffer-verbose " Verbose\n  ")

  ;; 4. Settings: Print Header          Auto Mode
  (pr-insert-toggle 'ps-print-header " Print Header          ")
  (pr-insert-toggle 'pr-auto-mode " Auto Mode\n  ")

  ;; 4. Settings: Print Header Frame    Menu Lock
  (pr-insert-toggle 'ps-print-header-frame " Print Header Frame    ")
  (pr-insert-toggle 'pr-menu-lock " Menu Lock\n  ")

  ;; 4. Settings: Line Number
  (pr-insert-toggle 'ps-line-number " Line Number\n  ")

  ;; 4. Settings: Zebra Stripes         Spool Buffer
  (pr-insert-toggle 'ps-zebra-stripes " Zebra Stripes")
  (pr-insert-checkbox "         "
		      'pr-spool-p
		      #'(lambda (&rest ignore)
			  (setq pr-spool-p (not pr-spool-p))
			  (unless pr-spool-p
			    (setq pr-i-despool nil)
			    (pr-update-checkbox 'pr-i-despool)))
		      " Spool Buffer")

  ;; 4. Settings: Duplex                Print with faces
  (pr-insert-checkbox "\n  "
		      'ps-spool-duplex
		      #'(lambda (&rest ignore)
			  (setq ps-spool-duplex (not ps-spool-duplex)
				pr-file-duplex  ps-spool-duplex))
		      " Duplex                ")
  (pr-insert-toggle 'pr-faces-p " Print with faces")

  ;; 4. Settings: Tumble                Print via Ghostscript
  (pr-insert-checkbox "\n  "
		      'ps-spool-tumble
		      #'(lambda (&rest ignore)
			  (setq ps-spool-tumble (not ps-spool-tumble)
				pr-file-tumble  ps-spool-tumble))
		      " Tumble                ")
  (pr-insert-toggle 'pr-print-using-ghostscript " Print via Ghostscript\n  ")

  ;; 4. Settings: Upside-Down           Page Parity
  (pr-insert-toggle 'ps-print-upside-down " Upside-Down")
  (pr-insert-italic "\n\nSelect Pages  :   " 2 14)
  (pr-insert-menu "Page Parity" 'ps-even-or-odd-pages
		  (mapcar #'(lambda (alist)
			      (list 'quote
				    (list 'choice-item
					  :format "%[%t%]"
					  :tag (cdr alist)
					  :value (car alist))))
			  pr-even-or-odd-alist)))


(defun pr-insert-section-5 ()
  ;; 5. Customize:
  (pr-insert-italic "\n\nCustomize     :   " 2 11)
  (pr-insert-button 'pr-customize "printing" "   ")
  (pr-insert-button #'(lambda (&rest ignore) (ps-print-customize))
		    "ps-print" "   ")
  (pr-insert-button 'lpr-customize "lpr"))


(defun pr-insert-section-6 ()
  ;; 6. Show Settings:
  (pr-insert-italic "\nShow Settings :   " 1 14)
  (pr-insert-button 'pr-show-pr-setup "printing" "   ")
  (pr-insert-button 'pr-show-ps-setup "ps-print" "   ")
  (pr-insert-button 'pr-show-lpr-setup "lpr"))


(defun pr-insert-section-7 ()
  ;; 7. Help:
  (pr-insert-italic "\nHelp          :   " 1 5)
  (pr-insert-button 'pr-interface-help "Interface Help" "    ")
  (pr-insert-button 'pr-help "Menu Help" "   ")
  (pr-insert-button 'pr-interface-quit "Quit" "\n                  ")
  (pr-insert-button 'pr-kill-help "Kill All Printing Help Buffer"))


(defun pr-kill-help (&rest ignore)
  "Kill all printing help buffer."
  (interactive)
  (let ((help '("*Printing Interface Help*" "*Printing Help*"
		"*LPR Setup*" "*PR Setup*" "*PS Setup*")))
    (while help
      (let ((buffer (get-buffer (car help))))
	(setq help (cdr help))
	(when buffer
	  (delete-windows-on buffer)
	  (kill-buffer buffer)))))
  (recenter (- (window-height) 2)))


(defun pr-interface-quit (&rest ignore)
  "Kill the printing buffer interface and quit."
  (interactive)
  (kill-buffer pr-buffer-name)
  (set-window-configuration pr-i-window-configuration))


(defun pr-interface-help (&rest ignore)
  "printing buffer interface help."
  (interactive)
  (pr-show-setup pr-interface-help-message "*Printing Interface Help*"))


(defun pr-interface-txt-print (&rest ignore)
  "Print using lpr package."
  (interactive)
  (condition-case data
      (cond
       ((eq pr-i-process 'directory)
	(pr-i-directory)
	(pr-interface-save
	 (pr-txt-directory pr-i-directory pr-i-regexp)))
       ((eq pr-i-process 'buffer)
	(pr-interface-save
	 (cond (pr-i-region
		(let ((pr-auto-mode pr-i-mode))
		  (pr-txt-region)))
	       (pr-i-mode
		(let (pr-auto-region)
		  (pr-txt-mode)))
	       (t
		(let (pr-auto-mode pr-auto-region)
		  (pr-txt-buffer)))
	       )))
       ((eq pr-i-process 'file)
	(error "Please specify a text file"))
       (t
	(error "Internal error: `pr-i-process' = %S" pr-i-process))
       )
    ;; handlers
    ((quit error)
     (ding)
     (message "%s" (error-message-string data)))))


(defun pr-interface-printify (&rest ignore)
  "Printify a buffer."
  (interactive)
  (condition-case data
      (cond
       ((eq pr-i-process 'directory)
	(pr-i-directory)
	(pr-interface-save
	 (pr-printify-directory pr-i-directory pr-i-regexp)))
       ((eq pr-i-process 'buffer)
	(pr-interface-save
	 (if pr-i-region
	     (pr-printify-region)
	   (pr-printify-buffer))))
       ((eq pr-i-process 'file)
	(error "Cannot printify a PostScript file"))
       (t
	(error "Internal error: `pr-i-process' = %S" pr-i-process))
       )
    ;; handlers
    ((quit error)
     (ding)
     (message "%s" (error-message-string data)))))


(defun pr-interface-ps-print (&rest ignore)
  "Print using ps-print package."
  (interactive)
  (pr-interface-ps 'pr-despool-ps-print 'pr-ps-directory-ps-print
		   'pr-ps-file-ps-print 'pr-ps-file-up-ps-print
		   'pr-ps-region-ps-print 'pr-ps-mode-ps-print
		   'pr-ps-buffer-ps-print))


(defun pr-interface-preview (&rest ignore)
  "Preview a PostScript file."
  (interactive)
  (pr-interface-ps 'pr-despool-preview 'pr-ps-directory-preview
		   'pr-ps-file-preview 'pr-ps-file-up-preview
		   'pr-ps-region-preview 'pr-ps-mode-preview
		   'pr-ps-buffer-preview))


(defun pr-interface-ps (ps-despool ps-directory ps-file ps-file-up ps-region
				   ps-mode ps-buffer)
  (condition-case data
      (let ((outfile (or (and (eq pr-i-process 'file) pr-i-ps-as-is)
			 (pr-i-ps-send))))
	(cond
	 ((and pr-i-despool pr-spool-p)
	  (pr-interface-save
	   (funcall ps-despool outfile))
	  (setq pr-i-despool nil)
	  (pr-update-checkbox 'pr-i-despool))
	 ((eq pr-i-process 'directory)
	  (pr-i-directory)
	  (pr-interface-save
	   (funcall ps-directory
		    pr-i-n-up pr-i-directory pr-i-regexp outfile)))
	 ((eq pr-i-process 'file)
	  (cond ((or (file-directory-p pr-i-ps-file)
		     (not (file-readable-p pr-i-ps-file)))
		 (error "Please specify a readable PostScript file"))
		(pr-i-ps-as-is
		 (pr-interface-save
		  (funcall ps-file pr-i-ps-file)))
		(t
		 (pr-interface-save
		  (funcall ps-file-up pr-i-n-up pr-i-ps-file outfile)))
		))
	 ((eq pr-i-process 'buffer)
	  (pr-interface-save
	   (cond (pr-i-region
		  (let ((pr-auto-mode pr-i-mode))
		    (funcall ps-region pr-i-n-up outfile)))
		 (pr-i-mode
		  (let (pr-auto-region)
		    (funcall ps-mode pr-i-n-up outfile)))
		 (t
		  (let (pr-auto-mode pr-auto-region)
		    (funcall ps-buffer pr-i-n-up outfile)))
		 )))
	 (t
	  (error "Internal error: `pr-i-process' = %S" pr-i-process))
	 ))
    ;; handlers
    ((quit error)
     (ding)
     (message "%s" (error-message-string data)))))


(defun pr-i-ps-send ()
  (cond ((eq pr-i-ps-send 'printer)
	 nil)
	((not (eq pr-i-ps-send 'file))
	 (error "Internal error: `pr-i-ps-send' = %S" pr-i-ps-send))
	((or (file-directory-p pr-i-out-file)
	     (not (file-writable-p pr-i-out-file)))
	 (error "Please specify a writable PostScript file"))
	((or (not (file-exists-p pr-i-out-file))
	     pr-i-answer-yes
	     (setq pr-i-answer-yes
		   (y-or-n-p (format "File `%s' exists; overwrite? "
				     pr-i-out-file))))
	 pr-i-out-file)
	(t
	 (error "File already exists"))))


(defun pr-i-directory ()
  (or (and (file-directory-p pr-i-directory)
	   (file-readable-p pr-i-directory))
      (error "Please specify be a readable directory")))


(defun pr-interface-directory (widget &rest ignore)
  (and pr-buffer-verbose
       (message "You can use M-TAB or ESC TAB for file completion"))
  (let ((dir (widget-value widget)))
    (and (file-directory-p dir)
	 (file-readable-p dir)
	 (setq pr-i-directory dir))))


(defun pr-interface-infile (widget &rest ignore)
  (and pr-buffer-verbose
       (message "You can use M-TAB or ESC TAB for file completion"))
  (let ((file (widget-value widget)))
    (and (not (file-directory-p file))
	 (file-readable-p file)
	 (setq pr-i-ps-file file))))


(defun pr-interface-outfile (widget &rest ignore)
  (setq pr-i-answer-yes nil)
  (and pr-buffer-verbose
       (message "You can use M-TAB or ESC TAB for file completion"))
  (let ((file (widget-value widget)))
    (and (not (file-directory-p file))
	 (file-writable-p file)
	 (setq pr-i-out-file file))))


(defun pr-widget-field-action (widget event)
  (and (get-buffer "*Completions*")	; clean frame window
       (delete-windows-on "*Completions*"))
  (message " ")				; clean echo area
  (widget-field-action widget event))


(defun pr-insert-italic (str &optional from to)
  (let ((len (length str)))
    (put-text-property (if from (max from 0)   0)
		       (if to   (max to   len) len)
		       'face 'italic str)
    (widget-insert str)))


(defun pr-insert-checkbox (before var-sym fun label)
  (widget-insert before)
  (prog1
      (widget-create 'checkbox
		     :notify fun
		     (symbol-value var-sym))
    (widget-insert label)))


(defun pr-insert-toggle (var-sym label)
  (widget-create 'checkbox
		 :notify `(lambda (&rest ignore)
			    (setq ,var-sym (not ,var-sym)))
		 (symbol-value var-sym))
  (widget-insert label))


(defun pr-insert-button (fun label &optional separator)
  (widget-create 'push-button
		 :notify fun
		 label)
  (and separator
       (widget-insert separator)))


(defun pr-insert-menu (tag var-sym choices &optional before after &rest body)
  (and before (widget-insert before))
  (eval `(widget-create 'menu-choice
			:tag ,tag
			:format "%v"
			:inline t
			:value ,var-sym
			:notify (lambda (widget &rest ignore)
				  (setq ,var-sym (widget-value widget))
				  ,@body)
			:void '(choice-item :format "%[%t%]"
					    :tag "Can not display value!")
			,@choices))
    (and after (widget-insert after)))


(defun pr-insert-radio-button (var-sym sym)
  (widget-insert "\n")
  (let ((wid-list (get var-sym 'pr-widget-list))
	(wid (eval `(widget-create
		     'radio-button
		     :format "  %[%v%]"
		     :value (eq ,var-sym (quote ,sym))
		     :notify (lambda (&rest ignore)
			       (setq ,var-sym (quote ,sym))
			       (pr-update-radio-button (quote ,var-sym)))))))
    (put var-sym 'pr-widget-list (cons (cons wid sym) wid-list))))


(defun pr-update-radio-button (var-sym)
  (let ((wid-list (get var-sym 'pr-widget-list)))
    (while wid-list
      (let ((wid   (car (car wid-list)))
	    (value (cdr (car wid-list))))
	(setq wid-list (cdr wid-list))
	(widget-value-set wid (eq (symbol-value var-sym) value))))
    (widget-setup)))


(defun pr-update-checkbox (var-sym)
  (let ((wid (get var-sym 'pr-widget)))
    (when wid
      (widget-value-set wid (symbol-value var-sym))
      (widget-setup))))


(defun pr-choice-alist (alist)
  (let ((max (apply 'max (mapcar #'(lambda (alist)
				     (length (symbol-name (car alist))))
				 alist))))
    (mapcar #'(lambda (alist)
		(let* ((sym  (car alist))
		       (name (symbol-name sym)))
		  (list
		   'quote
		   (list
		    'choice-item
		    :format "%[%t%]"
		    :tag (concat name
				 (make-string (- max (length name)) ?_))
		    :value sym))))
	    alist)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'printing)


;;; printing.el ends here
