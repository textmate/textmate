;;; ps-print.el --- print text from the buffer as PostScript

;; Copyright (C) 1993-2012  Free Software Foundation, Inc.

;; Author: Jim Thompson (was <thompson@wg2.waii.com>)
;;	Jacques Duthen (was <duthen@cegelec-red.fr>)
;;	Vinicius Jose Latorre <viniciusjl@ig.com.br>
;;	Kenichi Handa <handa@m17n.org> (multi-byte characters)
;; Maintainer: Kenichi Handa <handa@m17n.org> (multi-byte characters)
;;	Vinicius Jose Latorre <viniciusjl@ig.com.br>
;; Keywords: wp, print, PostScript
;; Version: 7.3.5
;; X-URL: http://www.emacswiki.org/cgi-bin/wiki/ViniciusJoseLatorre

(defconst ps-print-version "7.3.5"
  "ps-print.el, v 7.3.5 <2009/12/23 vinicius>

Vinicius's last change version -- this file may have been edited as part of
Emacs without changes to the version number.  When reporting bugs, please also
report the version of Emacs, if any, that ps-print was distributed with.

Please send all bug fixes and enhancements to
	Vinicius Jose Latorre <viniciusjl@ig.com.br>.")

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
;; About ps-print
;; --------------
;;
;; This package provides printing of Emacs buffers on PostScript printers; the
;; buffer's bold and italic text attributes are preserved in the printer
;; output.  ps-print is intended for use with Emacs or XEmacs, together with a
;; fontifying package such as font-lock or hilit.
;;
;; ps-print uses the same face attributes defined through font-lock or hilit to
;; print a PostScript file, but some faces are better seeing on the screen than
;; on paper, specially when you have a black/white PostScript printer.
;;
;; ps-print allows a remap of face to another one that it is better to print,
;; for example, the face font-lock-comment-face (if you are using font-lock)
;; could have bold or italic attribute when printing, besides foreground color.
;; This remap improves printing look (see How Ps-Print Maps Faces).
;;
;;
;; Using ps-print
;; --------------
;;
;; ps-print provides eight commands for generating PostScript images of Emacs
;; buffers:
;;
;;        ps-print-buffer
;;        ps-print-buffer-with-faces
;;        ps-print-region
;;        ps-print-region-with-faces
;;        ps-spool-buffer
;;        ps-spool-buffer-with-faces
;;        ps-spool-region
;;        ps-spool-region-with-faces
;;
;; These commands all perform essentially the same function: they generate
;; PostScript images suitable for printing on a PostScript printer or
;; displaying with GhostScript.  These commands are collectively referred to as
;; "ps-print- commands".
;;
;; The word "print" or "spool" in the command name determines when the
;; PostScript image is sent to the printer:
;;
;;        print      - The PostScript image is immediately sent to the printer;
;;
;;        spool      - The PostScript image is saved temporarily in an Emacs
;;                     buffer.  Many images may be spooled locally before
;;                     printing them.  To send the spooled images to the
;;                     printer, use the command `ps-despool'.
;;
;; The spooling mechanism was designed for printing lots of small files (mail
;; messages or netnews articles) to save paper that would otherwise be wasted
;; on banner pages, and to make it easier to find your output at the printer
;; (it's easier to pick up one 50-page printout than to find 50 single-page
;; printouts).
;;
;; ps-print has a hook in the `kill-emacs-hook' so that you won't accidentally
;; quit from Emacs while you have unprinted PostScript waiting in the spool
;; buffer.  If you do attempt to exit with spooled PostScript, you'll be asked
;; if you want to print it, and if you decline, you'll be asked to confirm the
;; exit; this is modeled on the confirmation that Emacs uses for modified
;; buffers.
;;
;; The word "buffer" or "region" in the command name determines how much of the
;; buffer is printed:
;;
;;        buffer     - Print the entire buffer.
;;
;;        region     - Print just the current region.
;;
;; The -with-faces suffix on the command name means that the command will
;; include font, color, and underline information in the PostScript image, so
;; the printed image can look as pretty as the buffer.  The ps-print- commands
;; without the -with-faces suffix don't include font, color, or underline
;; information; images printed with these commands aren't as pretty, but are
;; faster to generate.
;;
;; Two ps-print- command examples:
;;
;;        ps-print-buffer             - print the entire buffer, without font,
;;                                      color, or underline information, and
;;                                      send it immediately to the printer.
;;
;;        ps-spool-region-with-faces  - print just the current region; include
;;                                      font, color, and underline information,
;;                                      and spool the image in Emacs to send to
;;                                      the printer later.
;;
;;
;; Invoking Ps-Print
;; -----------------
;;
;; To print your buffer, type
;;
;;        M-x ps-print-buffer
;;
;; or substitute one of the other seven ps-print- commands.  The command will
;; generate the PostScript image and print or spool it as specified.  By giving
;; the command a prefix argument
;;
;;        C-u M-x ps-print-buffer
;;
;; it will save the PostScript image to a file instead of sending it to the
;; printer; you will be prompted for the name of the file to save the image to.
;; The prefix argument is ignored by the commands that spool their images, but
;; you may save the spooled images to a file by giving a prefix argument to
;; `ps-despool':
;;
;;        C-u M-x ps-despool
;;
;; When invoked this way, `ps-despool' will prompt you for the name of the file
;; to save to.
;;
;; Any of the `ps-print-' commands can be bound to keys; I recommend binding
;; `ps-spool-buffer-with-faces', `ps-spool-region-with-faces', and
;; `ps-despool'.  Here are the bindings I use on my Sun 4 keyboard:
;;
;;   (global-set-key 'f22 'ps-spool-buffer-with-faces) ;f22 is prsc
;;   (global-set-key '(shift f22) 'ps-spool-region-with-faces)
;;   (global-set-key '(control f22) 'ps-despool)
;;
;;
;; The Printer Interface
;; ---------------------
;;
;; The variables `ps-lpr-command' and `ps-lpr-switches' determine what command
;; is used to send the PostScript images to the printer, and what arguments to
;; give the command.  These are analogous to `lpr-command' and `lpr-switches'.
;;
;; Make sure that they contain appropriate values for your system;
;; see the usage notes below and the documentation of these variables.
;;
;; The variable `ps-printer-name' determines the name of a local printer for
;; printing PostScript files.
;;
;; The variable `ps-printer-name-option' determines the option used by some
;; utilities to indicate the printer name, it's used only when
;; `ps-printer-name' is a non-empty string.  If you're using lpr utility to
;; print, for example, `ps-printer-name-option' should be set to "-P".
;;
;; NOTE: `ps-lpr-command' and `ps-lpr-switches' take their initial values from
;;       the variables `lpr-command' and `lpr-switches'.  If you have
;;       `lpr-command' set to invoke a pretty-printer such as `enscript', then
;;       ps-print won't work properly.  `ps-lpr-command' must name a program
;;       that does not format the files it prints.
;;       `ps-printer-name' takes its initial value from the variable
;;       `printer-name'.  `ps-printer-name-option' tries to guess which system
;;       Emacs is running and takes its initial value in accordance with this
;;       guess.
;;
;; The variable `ps-print-region-function' specifies a function to print the
;; region on a PostScript printer.
;; See definition of `call-process-region' for calling conventions.  The fourth
;; and the sixth arguments are both nil.
;;
;; The variable `ps-manual-feed' indicates if the printer will manually feed
;; paper.  If it's nil, automatic feeding takes place.  If it's non-nil, manual
;; feeding takes place.  The default is nil (automatic feeding).
;;
;; The variable `ps-end-with-control-d' specifies whether C-d (\x04) should be
;; inserted at end of PostScript generated.  Non-nil means do so.  The default
;; is nil (don't insert).
;;
;; If you're using Emacs for Windows 95/98/NT or MS-DOS, don't forget to
;; customize the following variables: `ps-printer-name',
;; `ps-printer-name-option', `ps-lpr-command', `ps-lpr-switches' and
;; `ps-spool-config'.  See these variables documentation in the code or by
;; typing, for example, C-h v ps-printer-name RET.
;;
;;
;; The Page Layout
;; ---------------
;;
;; All dimensions are floats in PostScript points.
;; 1 inch  ==       2.54  cm    ==     72       points
;; 1 cm    ==  (/ 1 2.54) inch  ==  (/ 72 2.54) points
;;
;; The variable `ps-paper-type' determines the size of paper ps-print formats
;; for; it should contain one of the symbols: `a4' `a3' `letter' `legal'
;; `letter-small' `tabloid' `ledger' `statement' `executive' `a4small' `b4'
;; `b5'.
;;
;; If variable `ps-warn-paper-type' is nil, it's *not* given an error if
;; PostScript printer doesn't have a paper with the size indicated by
;; `ps-paper-type', instead it uses the default paper size.  If variable
;; `ps-warn-paper-type' is non-nil, it's given an error if PostScript printer
;; doesn't have a paper with the size indicated by `ps-paper-type'.  It's used
;; when `ps-spool-config' is set to `setpagedevice' (see section Duplex
;; Printers).  The default value is non-nil (it gives an error).
;;
;; The variable `ps-landscape-mode' determines the orientation of the printing
;; on the page: nil means `portrait' mode, non-nil means `landscape' mode.
;; There is no oblique mode yet, though this is easy to do in ps.
;;
;; In landscape mode, the text is NOT scaled: you may print 70 lines in
;; portrait mode and only 50 lines in landscape mode.  The margins represent
;; margins in the printed paper: the top margin is the margin between the top
;; of the page and the printed header, whatever the orientation is.
;;
;; The variable `ps-number-of-columns' determines the number of columns both in
;; landscape and portrait mode.
;; You can use:
;; - (the standard) one column portrait mode.
;; - (my favorite) two columns landscape mode (which spares trees).
;; but also:
;; - one column landscape mode for files with very long lines.
;; - multi-column portrait or landscape mode.
;;
;; The variable `ps-print-upside-down' determines other orientation for
;; printing page: nil means `normal' printing, non-nil means `upside-down'
;; printing (that is, the page is rotated by 180 grades).  The default value is
;; nil (`normal' printing).
;;
;; The `upside-down' orientation can be used in portrait or landscape mode.
;;
;; The variable `ps-selected-pages' specifies which pages to print.  If it's
;; nil, all pages are printed.  If it's a list, the list element may be an
;; integer or a cons cell (FROM . TO) designating FROM page to TO page; any
;; invalid element is ignored, that is, an integer lesser than one or if FROM
;; is greater than TO.  Otherwise, it's treated as nil.  The default value is
;; nil (print all pages).  After ps-print processing `ps-selected-pages' is set
;; to nil.  But the latest `ps-selected-pages' is saved in
;; `ps-last-selected-pages' (see it for documentation).  So you can restore the
;; latest selected pages by using `ps-last-selected-pages' or by calling
;; `ps-restore-selected-pages' command (see it for documentation).
;;
;; The variable `ps-even-or-odd-pages' specifies if it prints even/odd pages.
;;
;; Valid values are:
;;
;; nil		print all pages.
;;
;; even-page	print only even pages.
;;
;; odd-page	print only odd pages.
;;
;; even-sheet	print only even sheets.
;;
;; odd-sheet	print only odd sheets.
;;
;; Any other value is treated as nil.  The default value is nil.
;;
;; See `ps-even-or-odd-pages' for more detailed documentation.
;;
;;
;; Horizontal layout
;; -----------------
;;
;; The horizontal layout is determined by the variables
;; `ps-left-margin' `ps-inter-column' `ps-right-margin'
;; as follows:
;;
;;  ------------------------------------------
;;  |    |      |    |      |    |      |    |
;;  | lm | text | ic | text | ic | text | rm |
;;  |    |      |    |      |    |      |    |
;;  ------------------------------------------
;;
;; If `ps-number-of-columns' is 1, `ps-inter-column' is not relevant.
;; Usually, lm = rm > 0 and ic = lm
;; If (ic < 0), the text of adjacent columns can overlap.
;;
;;
;; Vertical layout
;; ---------------
;;
;; The vertical layout is determined by the variables
;; `ps-bottom-margin' `ps-top-margin' `ps-header-offset' `ps-footer-offset'
;; as follows:
;;
;; |--------|        |--------|        |--------|        |--------|
;; | tm     |        | tm     |        | tm     |        | tm     |
;; |--------|        |--------|        |--------|        |--------|
;; | header |        |        |        | header |        |        |
;; |--------|        |        |        |--------|        |        |
;; | ho     |        |        |        | ho     |        |        |
;; |--------|        |        |        |--------|        |        |
;; |        |        |        |        |        |        |        |
;; | text   |   or   | text   |   or   | text   |   or   | text   |
;; |        |        |        |        |        |        |        |
;; |        |        |--------|        |--------|        |        |
;; |        |        | fo     |        | fo     |        |        |
;; |        |        |--------|        |--------|        |        |
;; |        |        | footer |        | footer |        |        |
;; |--------|        |--------|        |--------|        |--------|
;; | bm     |        | bm     |        | bm     |        | bm     |
;; |--------|        |--------|        |--------|        |--------|
;;
;; If `ps-print-header' is nil, `ps-header-offset' is not relevant.
;; If `ps-print-footer' is nil, `ps-footer-offset' is not relevant.
;; The margins represent margins in the printed paper:
;; the top margin is the margin between the top of the page and the printed
;; header, whatever the orientation is;
;; the bottom margin is the margin between the bottom of the page and the
;; printed footer, whatever the orientation is.
;;
;;
;; Headers & Footers
;; -----------------
;;
;; ps-print can print headers at the top of each column or at the top of each
;; page; the default headers contain the following four items: on the left, the
;; name of the buffer and, if the buffer is visiting a file, the file's
;; directory; on the right, the page number and date of printing.  The default
;; headers look something like this:
;;
;;     ps-print.el                                         1/21
;;     /home/jct/emacs-lisp/ps/new                     94/12/31
;;
;; When printing on duplex printers, left and right are reversed so that the
;; page numbers are toward the outside (cf. `ps-spool-duplex').
;;
;; Headers are configurable:
;; To turn them off completely, set `ps-print-header' to nil.
;; To turn off the header's gaudy framing box,
;; set `ps-print-header-frame' to nil.
;;
;; The variable `ps-header-frame-alist' specifies header frame properties
;; alist.  Valid frame properties are:
;;
;;   fore-color		Specify the foreground frame color.
;;			It should be a float number between 0.0 (black color)
;;			and 1.0 (white color), a string which is a color name,
;;			or a list of 3 float numbers which corresponds to the
;;			Red Green Blue color scale, each float number between
;;			0.0 (dark color) and 1.0 (bright color).
;;			The default is 0 ("black").
;;
;;   back-color		Specify the background frame color (similar to
;;			fore-color).  The default is 0.9 ("gray90").
;;
;;   shadow-color	Specify the shadow color (similar to fore-color).
;;			The default is 0 ("black").
;;
;;   border-color	Specify the border color (similar to fore-color).
;;			The default is 0 ("black").
;;
;;   border-width	Specify the border width.
;;			The default is 0.4.
;;
;; Any other property is ignored.
;;
;; Don't change this alist directly, instead use customization, or `ps-value',
;; `ps-get', `ps-put' and `ps-del' functions (see them for documentation).
;;
;; To print only one header at the top of each page, set
;; `ps-print-only-one-header' to t.
;;
;; To switch headers, set `ps-switch-header' to:
;;
;;    nil	Never switch headers.
;;
;;    t		Always switch headers.
;;
;;    duplex	Switch headers only when duplexing is on, that is, when
;;		`ps-spool-duplex' is non-nil (see Duplex Printers).
;;
;; Any other value is treated as t.  The default value is `duplex'.
;;
;; The font family and size of text in the header are determined by the
;; variables `ps-header-font-family', `ps-header-font-size' and
;; `ps-header-title-font-size' (see below).
;;
;; The variable `ps-header-line-pad' determines the portion of a header title
;; line height to insert between the header frame and the text it contains,
;; both in the vertical and horizontal directions: .5 means half a line.
;;
;; Page numbers are printed in `n/m' format, indicating page n of m pages; to
;; omit the total page count and just print the page number, set
;; `ps-show-n-of-n' to nil.
;;
;; The amount of information in the header can be changed by changing the
;; number of lines.  To show less, set `ps-header-lines' to 1, and the header
;; will show only the buffer name and page number.  To show more, set
;; `ps-header-lines' to 3, and the header will show the time of printing below
;; the date.
;;
;; To change the content of the headers, change the variables `ps-left-header'
;; and `ps-right-header'.
;; These variables are lists, specifying top-to-bottom the text to display on
;; the left or right side of the header.  Each element of the list should be a
;; string or a symbol.  Strings are inserted directly into the PostScript
;; arrays, and should contain the PostScript string delimiters '(' and ')'.
;;
;; Symbols in the header format lists can either represent functions or
;; variables.  Functions are called, and should return a string to show in the
;; header.  Variables should contain strings to display in the header.  In
;; either case, function or variable, the PostScript string delimiters are
;; added by ps-print, and should not be part of the returned value.
;;
;; Here's an example: say we want the left header to display the text
;;
;;     Moe
;;     Larry
;;     Curly
;;
;; where we have a function to return "Moe"
;;
;;     (defun moe-func ()
;;       "Moe")
;;
;; a variable specifying "Larry"
;;
;;     (setq larry-var "Larry")
;;
;; and a literal for "Curly".  Here's how `ps-left-header' should be set:
;;
;;     (setq ps-left-header (list 'moe-func 'larry-var "(Curly)"))
;;
;; Note that Curly has the PostScript string delimiters inside his quotes --
;; those aren't misplaced lisp delimiters!
;;
;; Without them, PostScript would attempt to call the undefined function Curly,
;; which would result in a PostScript error.
;;
;; Since most printers don't report PostScript errors except by aborting the
;; print job, this kind of error can be hard to track down.
;;
;; Consider yourself warned!
;;
;; ps-print also print footers.  The footer variables are: `ps-print-footer',
;; `ps-footer-offset', `ps-print-footer-frame', `ps-footer-font-family',
;; `ps-footer-font-size', `ps-footer-line-pad', `ps-footer-lines',
;; `ps-left-footer', `ps-right-footer' and `ps-footer-frame-alist'.  These
;; variables are similar to those one that control headers.
;;
;; The variables `ps-print-only-one-header' and `ps-switch-header' also control
;; the footer (The same way that control header).
;;
;; As a footer example, if you want to have a centered page number in the
;; footer but without headers, set:
;;
;;    (setq ps-print-header nil
;;          ps-print-footer t
;;          ps-print-footer-frame nil
;;          ps-footer-lines 1
;;          ps-right-footer nil
;;          ps-left-footer
;;          (list (concat "{pagenumberstring dup stringwidth pop"
;;                        " 2 div PrintWidth 2 div exch sub 0 rmoveto}")))
;;
;;
;; PostScript Prologue Header
;; --------------------------
;;
;; It is possible to add PostScript prologue header comments besides that
;; ps-print generates by setting the variable `ps-print-prologue-header'.
;;
;; `ps-print-prologue-header' may be a string or a symbol function which
;; returns a string.  Note that this string is inserted on PostScript prologue
;; header section which is used to define some document characteristic through
;; PostScript special comments, like "%%Requirements: jog\n".
;;
;; By default `ps-print-prologue-header' is nil.
;;
;; ps-print always inserts the %%Requirements: comment, so if you need to
;; insert more requirements put them first in `ps-print-prologue-header' using
;; the "%%+" comment.  For example, if you need to set numcopies to 3 and jog
;; on requirements and set %%LanguageLevel: to 2, do:
;;
;; (setq ps-print-prologue-header
;;       "%%+ numcopies(3) jog\n%%LanguageLevel: 2\n")
;;
;; The duplex requirement is inserted by ps-print (see section Duplex
;; Printers).
;;
;; Do not forget to terminate the string with "\n".
;;
;; For more information about PostScript document comments, see:
;;    PostScript Language Reference Manual (2nd edition)
;;    Adobe Systems Incorporated
;;    Appendix G: Document Structuring Conventions -- Version 3.0
;;
;; It is also possible to add an user defined PostScript prologue code before
;; all generated prologue code by setting the variable
;; `ps-user-defined-prologue'.
;;
;; `ps-user-defined-prologue' may be a string or a symbol function which
;; returns a string.  Note that this string is inserted after `ps-adobe-tag'
;; and PostScript prologue comments, and before ps-print PostScript prologue
;; code section.  That is, this string is inserted after error handler
;; initialization and before ps-print settings.
;;
;; By default `ps-user-defined-prologue' is nil.
;;
;; It's strongly recommended only insert PostScript code and/or comments
;; specific for your printing system particularities.  For example, some
;; special initialization that only your printing system needs.
;;
;; Do not insert code for duplex printing, n-up printing or error handler,
;; ps-print handles this in a suitable way.
;;
;; For more information about PostScript, see:
;;    PostScript Language Reference Manual (2nd edition)
;;    Adobe Systems Incorporated
;;
;; As an example for `ps-user-defined-prologue' setting:
;;
;;   ;; Setting for HP PostScript printer
;;   (setq ps-user-defined-prologue
;;	   (concat "<</DeferredMediaSelection true /PageSize [612 792] "
;;		   "/MediaPosition 2 /MediaType (Plain)>> setpagedevice"))
;;
;;
;; PostScript Error Handler
;; ------------------------
;;
;; ps-print instruments generated PostScript code with an error handler.
;;
;; The variable `ps-error-handler-message' specifies where the error handler
;; message should be sent.
;;
;; Valid values are:
;;
;; none			catch the error and *DON'T* send any message.
;;
;; paper		catch the error and print on paper the error message.
;;			This is the default value.
;;
;; system		catch the error and send back the error message to
;;			printing system.  This is useful only if printing
;;			system send back an email reporting the error, or if
;;			there is some other alternative way to report back the
;;			error from the system to you.
;;
;; paper-and-system	catch the error, print on paper the error message and
;;			send back the error message to printing system.
;;
;; Any other value is treated as `paper'.
;;
;;
;; Duplex Printers
;; ---------------
;;
;; If you have a duplex-capable printer (one that prints both sides of the
;; paper), set `ps-spool-duplex' to t.
;; ps-print will insert blank pages to make sure each buffer starts on the
;; correct side of the paper.
;;
;; The variable `ps-spool-config' specifies who is the responsible for setting
;; duplex and page size.  Valid values are:
;;
;; lpr-switches    duplex and page size are configured by `ps-lpr-switches'.
;;                 Don't forget to set `ps-lpr-switches' to select duplex
;;                 printing for your printer.
;;
;; setpagedevice   duplex and page size are configured by ps-print using the
;;                 setpagedevice PostScript operator.
;;
;; nil             duplex and page size are configured by ps-print *not* using
;;                 the setpagedevice PostScript operator.
;;
;; Any other value is treated as nil.
;;
;; The default value is `lpr-switches'.
;;
;; WARNING: The setpagedevice PostScript operator affects ghostview utility
;;          when viewing file generated using landscape.  Also on some
;;          printers, setpagedevice affects zebra stripes; on other printers,
;;          setpagedevice affects the left margin.
;;          Besides all that, if your printer does not have the paper size
;;          specified by setpagedevice, your printing will be aborted.
;;          So, if you need to use setpagedevice, set `ps-spool-config' to
;;          `setpagedevice', generate a test file and send it to your printer;
;;          if the printed file isn't ok, set `ps-spool-config' to nil.
;;
;; The variable `ps-spool-tumble' specifies how the page images on opposite
;; sides of a sheet are oriented with respect to each other.  If
;; `ps-spool-tumble' is nil, produces output suitable for binding on the left
;; or right.  If `ps-spool-tumble' is non-nil, produces output suitable for
;; binding at the top or bottom.  It has effect only when `ps-spool-duplex' is
;; non-nil.  The default value is nil.
;;
;; Some printer system prints a header page and forces the first page be
;; printed on header page back, when using duplex.  If your printer system has
;; this behavior, set variable `ps-banner-page-when-duplexing' to t.
;;
;; When `ps-banner-page-when-duplexing' is non-nil, it prints a blank page as
;; the very first printed page.  So, it behaves as the very first character of
;; buffer (or region) is ^L (\014).
;;
;; The default for `ps-banner-page-when-duplexing' is nil (*don't* skip the
;; very first page).
;;
;;
;; N-up Printing
;; -------------
;;
;; The variable `ps-n-up-printing' specifies the number of pages per sheet of
;; paper.  The value specified must be between 1 and 100.  The default is 1.
;;
;; NOTE: some PostScript printer may crash printing if `ps-n-up-printing' is
;; set to a high value (for example, 23).  If this happens, set a lower value.
;;
;; The variable `ps-n-up-margin' specifies the margin in points between the
;; sheet border and the n-up printing.  The default is 1 cm (or 0.3937 inches,
;; or 28.35 points).
;;
;; If variable `ps-n-up-border-p' is non-nil a border is drawn around each
;; page.  The default is t.
;;
;; The variable `ps-n-up-filling' specifies how page matrix is filled on each
;; sheet of paper.  Following are the valid values for `ps-n-up-filling' with a
;; filling example using a 3x4 page matrix:
;;
;;  left-top   1  2  3  4         left-bottom    9  10 11 12
;;             5  6  7  8                        5  6  7  8
;;             9  10 11 12                       1  2  3  4
;;
;;  right-top  4  3  2  1         right-bottom   12 11 10 9
;;             8  7  6  5                        8  7  6  5
;;             12 11 10 9                        4  3  2  1
;;
;;  top-left   1  4  7  10        bottom-left    3  6  9  12
;;             2  5  8  11                       2  5  8  11
;;             3  6  9  12                       1  4  7  10
;;
;;  top-right  10 7  4  1         bottom-right   12 9  6  3
;;             11 8  5  2                        11 8  5  2
;;             12 9  6  3                        10 7  4  1
;;
;; Any other value is treated as `left-top'.
;;
;; The default value is left-top.
;;
;;
;; Control And 8-bit Characters
;; ----------------------------
;;
;; The variable `ps-print-control-characters' specifies whether you want to see
;; a printable form for control and 8-bit characters, that is, instead of
;; sending, for example, a ^D (\004) to printer, it is sent the string "^D".
;;
;; Valid values for `ps-print-control-characters' are:
;;
;;  8-bit           This is the value to use when you want an ASCII encoding of
;;                  any control or non-ASCII character. Control characters are
;;                  encoded as "^D", and non-ASCII characters have an
;;                  octal encoding.
;;
;;  control-8-bit   This is the value to use when you want an ASCII encoding of
;;                  any control character, whether it is 7 or 8-bit.
;;                  European 8-bits accented characters are printed according
;;                  the current font.
;;
;;  control         Only ASCII control characters have an ASCII encoding.
;;                  European 8-bits accented characters are printed according
;;                  the current font.
;;
;;  nil             No ASCII encoding. Any character is printed according the
;;                  current font.
;;
;; Any other value is treated as nil.
;;
;; The default is `control-8-bit'.
;;
;; Characters TAB, NEWLINE and FORMFEED are always treated by ps-print engine.
;;
;;
;; Printing Multi-byte Buffer
;; --------------------------
;;
;; See ps-mule.el for documentation.
;;
;;
;; Line Number
;; -----------
;;
;; The variable `ps-line-number' specifies whether to number each line;
;; non-nil means do so.  The default is nil (don't number each line).
;;
;; The variable `ps-line-number-color' specifies the color for line number.
;; See `ps-zebra-color' for documentation.  The default is "black" (or 0.0, or
;; '(0.0 0.0 0.0)).
;;
;; The variable `ps-line-number-font' specifies the font for line number.
;; The default is "Times-Italic".
;;
;; The variable `ps-line-number-font-size' specifies the font size in points
;; for line number.  See `ps-font-size' for documentation.  The default is 6.
;;
;; The variable `ps-line-number-step' specifies the interval that line number
;; is printed.  For example, if `ps-line-number-step' is set to 2, the printing
;; will look like:
;;
;;    1 one line
;;      one line
;;    3 one line
;;      one line
;;    5 one line
;;      one line
;;      ...
;;
;; Valid values are:
;;
;; integer	an integer that specifies the interval that line number is
;;		printed.  If it's lesser than or equal to zero, it's used the
;;		value 1.
;;
;; `zebra'	specifies that only the line number of the first line in a
;;		zebra stripe is to be printed.
;;
;; Any other value is treated as `zebra'.
;; The default value is 1, so each line number is printed.
;;
;; The variable `ps-line-number-start' specifies the starting point in the
;; interval given by `ps-line-number-step'.  For example, if
;; `ps-line-number-step' is set to 3 and `ps-line-number-start' is set to 3,
;; the printing will look like:
;;
;;      one line
;;      one line
;;    3 one line
;;      one line
;;      one line
;;    6 one line
;;      one line
;;      one line
;;    9 one line
;;      one line
;;      ...
;;
;; The values for `ps-line-number-start':
;;
;;    * If `ps-line-number-step' is an integer, must be between 1 and the value
;;	of `ps-line-number-step' inclusive.
;;
;;    * If `ps-line-number-step' is set to `zebra', must be between 1 and the
;;	value of `ps-zebra-stripe-height' inclusive.
;;
;; The default value is 1, so the line number of the first line of each
;; interval is printed.
;;
;;
;; Zebra Stripes
;; -------------
;;
;; Zebra stripes are a kind of background that appear "underneath" the text and
;; can make the text easier to read.  They look like this:
;;
;; XXXXXXXXXXXXXXXXXXXXXXXX
;; XXXXXXXXXXXXXXXXXXXXXXXX
;; XXXXXXXXXXXXXXXXXXXXXXXX
;;
;;
;;
;; XXXXXXXXXXXXXXXXXXXXXXXX
;; XXXXXXXXXXXXXXXXXXXXXXXX
;; XXXXXXXXXXXXXXXXXXXXXXXX
;;
;; The blocks of X's represent rectangles filled with a light gray color.
;; Each rectangle extends all the way across the page.
;;
;; The height, in lines, of each rectangle is controlled by the variable
;; `ps-zebra-stripe-height', which is 3 by default.  The distance between
;; stripes equals the height of a stripe.
;;
;; The variable `ps-zebra-stripes' controls whether to print zebra stripes.
;; Non-nil means yes, nil means no.  The default is nil.
;;
;; The variable `ps-zebra-color' controls the zebra stripes gray scale or RGB
;; color.  It should be a float number between 0.0 (black color) and 1.0 (white
;; color), a string which is a color name, or a list of 3 numbers which
;; corresponds to the Red Green Blue color scale.
;; The default is 0.95 (or "gray95", or '(0.95 0.95 0.95)).
;;
;; The variable `ps-zebra-stripe-follow' specifies how zebra stripes continue
;; on next page.  Visually, valid values are (the character `+' at right of
;; each column indicates that a line is printed):
;;
;;		   `nil'        `follow'        `full'        `full-follow'
;; Current Page --------     -----------     ---------     ----------------
;;		1  XXXXX +   1  XXXXXXXX +   1  XXXXXX +   1  XXXXXXXXXXXXX +
;;		2  XXXXX +   2  XXXXXXXX +   2  XXXXXX +   2  XXXXXXXXXXXXX +
;;		3  XXXXX +   3  XXXXXXXX +   3  XXXXXX +   3  XXXXXXXXXXXXX +
;;		4        +   4           +   4         +   4                +
;;		5        +   5           +   5         +   5                +
;;		6        +   6           +   6         +   6                +
;;		7  XXXXX +   7  XXXXXXXX +   7  XXXXXX +   7  XXXXXXXXXXXXX +
;;		8  XXXXX +   8  XXXXXXXX +   8  XXXXXX +   8  XXXXXXXXXXXXX +
;;		9  XXXXX +   9  XXXXXXXX +   9  XXXXXX +   9  XXXXXXXXXXXXX +
;;		10       +   10          +
;;		11       +   11          +
;;		--------     -----------     ---------     ----------------
;;    Next Page --------     -----------     ---------     ----------------
;;		12 XXXXX +   12          +   10 XXXXXX +   10               +
;;		13 XXXXX +   13 XXXXXXXX +   11 XXXXXX +   11               +
;;		14 XXXXX +   14 XXXXXXXX +   12 XXXXXX +   12               +
;;		15       +   15 XXXXXXXX +   13        +   13 XXXXXXXXXXXXX +
;;		16       +   16          +   14        +   14 XXXXXXXXXXXXX +
;;		17       +   17          +   15        +   15 XXXXXXXXXXXXX +
;;		18 XXXXX +   18          +   16 XXXXXX +   16               +
;;		19 XXXXX +   19 XXXXXXXX +   17 XXXXXX +   17               +
;;		20 XXXXX +   20 XXXXXXXX +   18 XXXXXX +   18               +
;;		21       +   21 XXXXXXXX +
;;		22       +   22          +
;;		--------     -----------     ---------     ----------------
;;
;; Any other value is treated as nil.
;;
;; See also section How Ps-Print Has A Text And/Or Image On Background.
;;
;;
;; Hooks
;; -----
;;
;; ps-print has the following hook variables:
;;
;; `ps-print-hook'
;;    It is evaluated once before any printing process.  This is the right
;;    place to initialize ps-print global data.
;;    For an example, see section Adding a New Font Family.
;;
;; `ps-print-begin-sheet-hook'
;;    It is evaluated on each beginning of sheet of paper.
;;    If `ps-n-up-printing' is equal to 1, `ps-print-begin-page-hook' is never
;;    evaluated.
;;
;; `ps-print-begin-page-hook'
;;    It is evaluated on each beginning of page, except in the beginning of
;;    page that `ps-print-begin-sheet-hook' is evaluated.
;;
;; `ps-print-begin-column-hook'
;;    It is evaluated on each beginning of column, except in the beginning of
;;    column that `ps-print-begin-page-hook' is evaluated or that
;;    `ps-print-begin-sheet-hook' is evaluated.
;;
;;
;; Font Managing
;; -------------
;;
;; ps-print now knows rather precisely some fonts: the variable
;; `ps-font-info-database' contains information for a list of font families
;; (currently mainly `Courier' `Helvetica' `Times' `Palatino'
;; `Helvetica-Narrow' `NewCenturySchlbk').  Each font family contains the font
;; names for standard, bold, italic and bold-italic characters, a reference
;; size (usually 10) and the corresponding line height, width of a space and
;; average character width.
;;
;; The variable `ps-font-family' determines which font family is to be used for
;; ordinary text.  If its value does not correspond to a known font family, an
;; error message is printed into the `*Messages*' buffer, which lists the
;; currently available font families.
;;
;; The variable `ps-font-size' determines the size (in points) of the font for
;; ordinary text, when generating PostScript.  Its value is a float or a cons
;; of floats which has the following form:
;;
;;    (LANDSCAPE-SIZE . PORTRAIT-SIZE)
;;
;; Similarly, the variable `ps-header-font-family' determines which font family
;; is to be used for text in the header.
;;
;; The variable `ps-header-font-size' determines the font size, in points, for
;; text in the header (similar to `ps-font-size').
;;
;; The variable `ps-header-title-font-size' determines the font size, in
;; points, for the top line of text in the header (similar to `ps-font-size').
;;
;; The variable `ps-line-spacing' determines the line spacing, in points, for
;; ordinary text, when generating PostScript (similar to `ps-font-size').  The
;; default value is 0 (zero = no line spacing).
;;
;; The variable `ps-paragraph-spacing' determines the paragraph spacing, in
;; points, for ordinary text, when generating PostScript (similar to
;; `ps-font-size').  The default value is 0 (zero = no paragraph spacing).
;;
;; To get all lines with some spacing set both `ps-line-spacing' and
;; `ps-paragraph-spacing' variables.
;;
;; The variable `ps-paragraph-regexp' specifies the paragraph delimiter.  It
;; should be a regexp or nil.  The default value is "[ \t]*$", that is, an
;; empty line or a line containing only spaces and tabs.
;;
;; The variable `ps-begin-cut-regexp' and `ps-end-cut-regexp' specify the start
;; and end of a region to cut out when printing.
;;
;; As an example, variables `ps-begin-cut-regexp' and `ps-end-cut-regexp' may
;; be set to "^Local Variables:" and "^End:", respectively, in order to leave
;; out some special printing instructions from the actual print.  Special
;; printing instructions may be appended to the end of the file just like any
;; other buffer-local variables.  See section "Local Variables in Files" on
;; Emacs manual for more information.
;;
;; Variables `ps-begin-cut-regexp' and `ps-end-cut-regexp' control together
;; what actually gets printed.  Both variables may be set to nil in which case
;; no cutting occurs.  By default, both variables are set to nil.
;;
;;
;; Adding a New Font Family
;; ------------------------
;;
;; To use a new font family, you MUST first teach ps-print this font, i.e., add
;; its information to `ps-font-info-database', otherwise ps-print cannot
;; correctly place line and page breaks.
;;
;; For example, assuming `Helvetica' is unknown, you first need to do the
;; following ONLY ONCE:
;;
;; - create a new buffer
;; - generate the PostScript image to a file (C-u M-x ps-print-buffer)
;; - open this file and find the line:
;;	`% 3 cm 20 cm moveto  10/Courier ReportFontInfo  showpage'
;; - delete the leading `%' (which is the PostScript comment character)
;; - replace in this line `Courier' by the new font (say `Helvetica') to get
;;   the line:
;;	`3 cm 20 cm moveto  10/Helvetica ReportFontInfo  showpage'
;; - send this file to the printer (or to ghostscript).
;;   You should read the following on the output page:
;;
;;     For Helvetica 10 point, the line height is 11.56, the space width is 2.78
;;     and a crude estimate of average character width is 5.09243
;;
;; - Add these values to the `ps-font-info-database':
;;   (setq ps-font-info-database
;;	   (append
;;	    '((Helvetica			; the family key
;;	       (fonts (normal      . "Helvetica")
;;		      (bold        . "Helvetica-Bold")
;;		      (italic      . "Helvetica-Oblique")
;;		      (bold-italic . "Helvetica-BoldOblique"))
;;	       (size . 10.0)
;;	       (line-height . 11.56)
;;	       (space-width . 2.78)
;;	       (avg-char-width . 5.09243)))
;;	    ps-font-info-database))
;; - Now you can use this font family with any size:
;;	(setq ps-font-family 'Helvetica)
;; - if you want to use this family in another emacs session, you must put into
;;   your `~/.emacs':
;;	(require 'ps-print)
;;	(setq ps-font-info-database (append ...)))
;;   if you don't want to load ps-print, you have to copy the whole value:
;;	(setq ps-font-info-database '(<your stuff> <the standard stuff>))
;;   or, use `ps-print-hook' (see section Hooks):
;;	(add-hook 'ps-print-hook
;;		  (lambda ()
;;		     (or (assq 'Helvetica ps-font-info-database)
;;			 (setq ps-font-info-database (append ...)))))
;;
;; You can create new `mixed' font families like:
;;      (my-mixed-family
;;       (fonts (normal               . "Courier-Bold")
;;              (bold                 . "Helvetica")
;;              (italic               . "ZapfChancery-MediumItalic")
;;              (bold-italic          . "NewCenturySchlbk-BoldItalic")
;;              (w3-table-hack-x-face . "LineDrawNormal"))
;;       (size . 10.0)
;;       (line-height . 10.55)
;;       (space-width . 6.0)
;;       (avg-char-width . 6.0))
;;
;; Now you can use your new font family with any size:
;;	(setq ps-font-family 'my-mixed-family)
;;
;; Note that on above example the `w3-table-hack-x-face' entry refers to a face
;; symbol, so when printing this face it'll be used the font `LineDrawNormal'.
;; If the face `w3-table-hack-x-face' is remapped to use bold and/or italic
;; attribute, the corresponding entry (bold, italic or bold-italic) will be
;; used instead of `w3-table-hack-x-face' entry.
;;
;; Note also that the font family entry order is irrelevant, so the above
;; example could also be written:
;;      (my-mixed-family
;;       (size . 10.0)
;;       (fonts (w3-table-hack-x-face . "LineDrawNormal")
;;              (bold                 . "Helvetica")
;;              (bold-italic          . "NewCenturySchlbk-BoldItalic")
;;              (italic               . "ZapfChancery-MediumItalic")
;;              (normal               . "Courier-Bold"))
;;       (avg-char-width . 6.0)
;;       (space-width . 6.0)
;;       (line-height . 10.55))
;;
;; Despite the note above, it is recommended that some convention about
;; entry order be used.
;;
;; You can get information on all the fonts resident in YOUR printer
;; by uncommenting the line:
;;	% 3 cm 20 cm moveto  ReportAllFontInfo           showpage
;;
;; The PostScript file should be sent to YOUR PostScript printer.
;; If you send it to ghostscript or to another PostScript printer, you may get
;; slightly different results.
;; Anyway, as ghostscript fonts are autoload, you won't get much font info.
;;
;; Note also that ps-print DOESN'T download any font to your printer, instead
;; it uses the fonts resident in your printer.
;;
;;
;; How Ps-Print Deals With Faces
;; -----------------------------
;;
;; The ps-print-*-with-faces commands attempt to determine which faces should
;; be printed in bold or italic, but their guesses aren't always right.  For
;; example, you might want to map colors into faces so that blue faces print in
;; bold, and red faces in italic.
;;
;; It is possible to force ps-print to consider specific faces bold, italic or
;; underline, no matter what font they are displayed in, by setting the
;; variables `ps-bold-faces', `ps-italic-faces' and `ps-underlined-faces'.
;; These variables contain lists of faces that ps-print should consider bold,
;; italic or underline; to set them, put code like the following into your
;; .emacs file:
;;
;;      (setq ps-bold-faces '(my-blue-face))
;;      (setq ps-italic-faces '(my-red-face))
;;      (setq ps-underlined-faces '(my-green-face))
;;
;; Faces like bold-italic that are both bold and italic should go in *both*
;; lists.
;;
;; ps-print keeps internal lists of which fonts are bold and which are italic;
;; these lists are built the first time you invoke ps-print.
;; For the sake of efficiency, the lists are built only once; the same lists
;; are referred in later invocations of ps-print.
;;
;; Because these lists are built only once, it's possible for them to get out
;; of sync, if a face changes, or if new faces are added.  To get the lists
;; back in sync, you can set the variable `ps-build-face-reference' to t, and
;; the lists will be rebuilt the next time ps-print is invoked.  If you need
;; that the lists always be rebuilt when ps-print is invoked, set the variable
;; `ps-always-build-face-reference' to t.
;;
;; If you need to print without worrying about face background color, set the
;; variable `ps-use-face-background' which specifies if face background should
;; be used.  Valid values are:
;;
;;    t		always use face background color.
;;    nil	never use face background color.
;;    (face...)	list of faces whose background color will be used.
;;
;; Any other value will be treated as t.
;; The default value is nil.
;;
;;
;; How Ps-Print Deals With Color
;; -----------------------------
;;
;; ps-print detects faces with foreground and background colors defined and
;; embeds color information in the PostScript image.
;; The default foreground and background colors are defined by the variables
;; `ps-default-fg' and `ps-default-bg'.
;; On black/white printers, colors are displayed in gray scale.
;; To turn off color output, set `ps-print-color-p' to nil.
;; You can also set `ps-print-color-p' to 'black-white to have a better looking
;; on black/white printers.  See also `ps-black-white-faces' for documentation.
;;
;; ps-print also detects if the text foreground and background colors are
;; equals when `ps-fg-validate-p' is non-nil.  In this case, if these colors
;; are used, no text will appear.  You can use `ps-fg-list' to give a list of
;; foreground colors to be used when text foreground and background colors are
;; equals.  It'll be used the first foreground color in `ps-fg-list' which is
;; different from the background color.  If `ps-fg-list' is nil, the default
;; foreground color is used.
;;
;;
;; How Ps-Print Maps Faces
;; -----------------------
;;
;; As ps-print uses PostScript to print buffers, it is possible to have other
;; attributes associated with faces. So the new attributes used by ps-print
;; are:
;;
;;   strikeout - like underline, but the line is in middle of text.
;;   overline  - like underline, but the line is over the text.
;;   shadow    - text will have a shadow.
;;   box       - text will be surrounded by a box.
;;   outline   - print characters as hollow outlines.
;;
;; See the documentation for `ps-extend-face'.
;;
;; Let's, for example, remap `font-lock-keyword-face' to another foreground
;; color and bold attribute:
;;
;;    (ps-extend-face '(font-lock-keyword-face "RoyalBlue" nil bold) 'MERGE)
;;
;; If you want to use a new face, define it first with `defface', and then call
;; `ps-extend-face' to specify how to print it.
;;
;;
;; How Ps-Print Has A Text And/Or Image On Background
;; --------------------------------------------------
;;
;; ps-print can print texts and/or EPS PostScript images on background; it is
;; possible to define the following text attributes: font name, font size,
;; initial position, angle, gray scale and pages to print.
;;
;; It has the following EPS PostScript images attributes: file name containing
;; the image, initial position, X and Y scales, angle and pages to print.
;;
;; See documentation for `ps-print-background-text' and
;; `ps-print-background-image'.
;;
;; For example, if we wish to print text "preliminary" on all pages and text
;; "special" on page 5 and from page 11 to page 17, we could specify:
;;
;; (setq ps-print-background-text
;;       '(("preliminary")
;;         ("special"
;;          "LeftMargin" "BottomMargin PrintHeight add" ; X and Y position
;;                                      ; (upper left corner)
;;          nil nil nil
;;          "PrintHeight neg PrintPageWidth atan" ; angle
;;          5 (11 . 17))                ; page list
;;         ))
;;
;; Similarly, we could print image "~/images/EPS-image1.ps" on all pages and
;; image "~/images/EPS-image2.ps" on page 5 and from page 11 to page 17, we
;; specify:
;;
;; (setq ps-print-background-image
;;       '(("~/images/EPS-image1.ps"
;;          "LeftMargin" "BottomMargin") ; X and Y position (lower left corner)
;;         ("~/images/EPS-image2.ps"
;;          "LeftMargin" "BottomMargin PrintHeight 2 div add" ; X and Y pos.
;;                                      ; (upper left corner)
;;          nil nil nil
;;          5 (11 . 17))                ; page list
;;         ))
;;
;; If it is not possible to read (or does not exist) an image file, that file
;; is ignored.
;;
;; The printing order is:
;;
;;    1. Print background color
;;    2. Print zebra stripes
;;    3. Print background texts that it should be on all pages
;;    4. Print background images that it should be on all pages
;;    5. Print background texts only for current page (if any)
;;    6. Print background images only for current page (if any)
;;    7. Print header
;;    8. Print buffer text (with faces, if specified) and line number
;;
;;
;; Utilities
;; ---------
;;
;; Some tools are provided to help you customize your font setup.
;;
;; `ps-setup' returns (some part of) the current setup.
;;
;; To avoid wrapping too many lines, you may want to adjust the left and right
;; margins and the font size.  On UN*X systems, do:
;; pr -t file | awk '{printf "%3d %s\n", length($0), $0}' | sort -r | head
;; to determine the longest lines of your file.
;; Then, the command `ps-line-lengths' will give you the correspondence between
;; a line length (number of characters) and the maximum font size which doesn't
;; wrap such a line with the current ps-print setup.
;;
;; The commands `ps-nb-pages-buffer' and `ps-nb-pages-region' display the
;; correspondence between a number of pages and the maximum font size which
;; allow the number of lines of the current buffer or of its current region to
;; fit in this number of pages.
;;
;; NOTE: line folding is not taken into account in this process and could
;;       change the results.
;;
;; The command `ps-print-customize' activates a customization buffer for
;; ps-print options.
;;
;;
;; New since version 1.5
;; ---------------------
;;
;; Color output capability.
;; Automatic detection of font attributes (bold, italic).
;; Configurable headers with page numbers.
;; Slightly faster.
;; Support for different paper sizes.
;; Better conformance to PostScript Document Structure Conventions.
;;
;;
;; New since version 2.8
;; ---------------------
;;
;; [vinicius] Vinicius Jose Latorre <viniciusjl@ig.com.br>
;;
;;    2007-10-27
;;	 `ps-fg-validate-p', `ps-fg-list'
;;
;;    2004-02-29
;;	 `ps-time-stamp-yyyy-mm-dd', `ps-time-stamp-iso8601'
;;
;;    2001-06-19
;;	 `ps-time-stamp-locale-default'
;;
;;    2001-05-30
;;	 Handle before-string and after-string overlay properties.
;;
;;    2001-04-07
;;	 `ps-line-number-color', `ps-print-footer', `ps-footer-offset',
;;	 `ps-print-footer-frame', `ps-footer-font-family',
;;	 `ps-footer-font-size', `ps-footer-line-pad', `ps-footer-lines',
;;	 `ps-left-footer', `ps-right-footer', `ps-footer-frame-alist' and
;;	 `ps-header-frame-alist'.
;;
;;    2001-03-28
;;	 `ps-line-spacing', `ps-paragraph-spacing', `ps-paragraph-regexp',
;;	 `ps-begin-cut-regexp' and `ps-end-cut-regexp'.
;;
;;    2000-11-22
;;	 `ps-line-number-font', `ps-line-number-font-size' and
;;	 `ps-end-with-control-d'.
;;
;;    2000-08-21
;;	 `ps-even-or-odd-pages'
;;
;;    2000-06-17
;;	 `ps-manual-feed', `ps-warn-paper-type', `ps-print-upside-down',
;;	 `ps-selected-pages', `ps-last-selected-pages',
;;	 `ps-restore-selected-pages', `ps-switch-header',
;;	 `ps-line-number-step', `ps-line-number-start',
;;	 `ps-zebra-stripe-follow' and `ps-use-face-background'.
;;
;;    2000-03-10
;;	 PostScript error handler.
;;	 `ps-user-defined-prologue' and `ps-error-handler-message'.
;;
;;    1999-12-11
;;	 `ps-print-customize'.
;;
;;    1999-07-03
;;	 Better customization.
;;	 `ps-banner-page-when-duplexing' and `ps-zebra-color'.
;;
;;    1999-05-13
;;	 N-up printing.
;;	 Hook: `ps-print-begin-sheet-hook'.
;;
;; [kenichi] 1999-05-09 Ken'ichi Handa <handa@m17n.org>
;;
;;    `ps-print-region-function'
;;
;; [vinicius] Vinicius Jose Latorre <viniciusjl@ig.com.br>
;;
;;    1999-03-01
;;	 PostScript tumble and setpagedevice.
;;
;;    1998-09-22
;;	 PostScript prologue header comment insertion.
;;	 Skip invisible text better.
;;
;; [kenichi] 1998-08-19 Ken'ichi Handa <handa@m17n.org>
;;
;;    Multi-byte buffer handling.
;;
;; [vinicius] Vinicius Jose Latorre <viniciusjl@ig.com.br>
;;
;;    1998-03-06
;;	 Skip invisible text.
;;
;;    1997-11-30
;;	 Hooks: `ps-print-hook', `ps-print-begin-page-hook' and
;;	 `ps-print-begin-column-hook'.
;;	 Put one header per page over the columns.
;;	 Better database font management.
;;	 Better control characters handling.
;;
;;    1997-11-21
;;	 Dynamic evaluation at print time of `ps-lpr-switches'.
;;	 Handle control characters.
;;	 Face remapping.
;;	 New face attributes.
;;	 Line number.
;;	 Zebra stripes.
;;	 Text and/or image on background.
;;
;; [jack] 1996-05-17 Jacques Duthen <duthen@cegelec-red.fr>
;;
;;    Font family and float size for text and header.
;;    Landscape mode.
;;    Multiple columns.
;;    Tools for page setup.
;;
;;
;; Known bugs and limitations of ps-print
;; --------------------------------------
;;
;; Although color printing will work in XEmacs 19.12, it doesn't work well; in
;; particular, bold or italic fonts don't print in the right background color.
;;
;; Invisible properties aren't correctly ignored in XEmacs 19.12.
;;
;; Automatic font-attribute detection doesn't work well, especially with
;; hilit19 and older versions of get-create-face.  Users having problems with
;; auto-font detection should use the lists `ps-italic-faces', `ps-bold-faces'
;; and `ps-underlined-faces' and/or turn off automatic detection by setting
;; `ps-auto-font-detect' to nil.
;;
;; Automatic font-attribute detection doesn't work with XEmacs 19.12 in tty
;; mode; use the lists `ps-italic-faces', `ps-bold-faces' and
;; `ps-underlined-faces' instead.
;;
;; Still too slow; could use some hand-optimization.
;;
;; Default background color isn't working.
;;
;; Faces are always treated as opaque.
;;
;; Epoch, Lucid and Emacs 22 not supported.  At all.
;;
;; Fixed-pitch fonts work better for line folding, but are not required.
;;
;; `ps-nb-pages-buffer' and `ps-nb-pages-region' don't take care of folding
;; lines.
;;
;;
;; Things to change
;; ----------------
;;
;; Avoid page break inside a paragraph.
;;
;; Add `ps-non-bold-faces' and `ps-non-italic-faces' (should be easy).
;;
;; Improve the memory management for big files (hard?).
;;
;; `ps-nb-pages-buffer' and `ps-nb-pages-region' should take care of folding
;; lines.
;;
;;
;; Acknowledgments
;; ---------------
;;
;; Thanks to Eduard Wiebe <usenet@pusto.de> for fixing face
;; background/foreground extraction.
;;
;; Thanks to Friedrich Delgado Friedrichs <friedel@nomaden.org> for new label
;; printer page sizes.
;;
;; Thanks to Michael Piotrowski <mxp@dynalabs.de> for improving the DSC
;; compliance of the generated PostScript.
;;
;; Thanks to Adam Doppelt <adoppelt@avogadro.com> for face mapping suggestion
;; for black/white PostScript printers.
;;
;; Thanks to Toni Ronkko <tronkko@hytti.uku.fi> for line and paragraph spacing,
;; region to cut out when printing and footer suggestions.
;;
;; Thanks to Pavel Janik ml <Pavel@Janik.cz> for documentation correction.
;;
;; Thanks to Corinne Ilvedson <cilvedson@draper.com> for line number font size
;; suggestion.
;;
;; Thanks to Gord Wait <Gord_Wait@spectrumsignal.com> for
;; `ps-user-defined-prologue' example setting for HP PostScript printer.
;;
;; Thanks to Paul Furnanz <pfurnanz@synopsys.com> for XEmacs compatibility
;; suggestion for `ps-postscript-code-directory' variable.
;;
;; Thanks to David X Callaway <dxc@xprt.net> for helping debugging PostScript
;; level 1 compatibility.
;;
;; Thanks to Colin Marquardt <colin.marquardt@usa.alcatel.com> for:
;;    - upside-down, line number step, line number start and zebra stripe
;;	follow suggestions.
;;    - `ps-time-stamp-yyyy-mm-dd' and `ps-time-stamp-iso8601' suggestion.
;;    - and for XEmacs beta-tests.
;;
;; Thanks to Klaus Berndl <klaus.berndl@sdm.de> for user defined PostScript
;; prologue code suggestion, for odd/even printing suggestion and for
;; `ps-prologue-file' enhancement.
;;
;; Thanks to Ken'ichi Handa <handa@m17n.org> for multi-byte buffer handling.
;;
;; Thanks to Matthew O Persico <Matthew.Persico@lazard.com> for line number on
;; empty columns.
;;
;; Thanks to Theodore Jump <tjump@cais.com> for adjust PostScript code order on
;; last page.
;;
;; Thanks to Roland Ducournau <ducour@lirmm.fr> for
;; `ps-print-control-characters' variable documentation.
;;
;; Thanks to Marcus G Daniels <marcus@cathcart.sysc.pdx.edu> for a better
;; database font management.
;;
;; Thanks to Martin Boyer <gamin@videotron.ca> for some ideas on putting one
;; header per page over the columns and correct line numbers when printing a
;; region.
;;
;; Thanks to Steven L Baur <steve@miranova.com> for dynamic evaluation at
;; print time of `ps-lpr-switches'.
;;
;; Thanks to Kevin Rodgers <kevinr@ihs.com> for handling control characters
;; (his code was severely modified, but the main idea was kept).
;;
;; Thanks to some suggestions on:
;;  * Face color map: Marco Melgazzi <marco@techie.com>
;;  * XEmacs compatibility: William J. Henney <will@astrosmo.unam.mx>
;;  * Check `ps-paper-type': Sudhakar Frederick <sfrederi@asc.corp.mot.com>
;;
;; Thanks to Jacques Duthen <duthen@cegelec-red.fr> (Jack) for version 3.4 I
;; started from. [vinicius]
;;
;; Thanks to Jim Thompson <?@?> for the 2.8 version I started from.  [jack]
;;
;; Thanks to Kevin Rodgers <kevinr@ihs.com> for adding support for color and
;; the invisible property.
;;
;; Thanks to Avishai Yacobi, avishaiy@mcil.comm.mot.com, for writing the
;; initial port to Emacs 19.  His code is no longer part of ps-print, but his
;; work is still appreciated.
;;
;; Thanks to Remi Houdaille and Michel Train <michel@metasoft.fdn.org> for
;; adding underline support.  Their code also is no longer part of ps-print,
;; but their efforts are not forgotten.
;;
;; Thanks also to all of you who mailed code to add features to ps-print;
;; although I didn't use your code, I still appreciate your sharing it with me.
;;
;; Thanks to all who mailed comments, encouragement, and criticism.
;; Thanks also to all who responded to my survey; I had too many responses to
;; reply to them all, but I greatly appreciate your interest.
;;
;; Jim
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:


(require 'lpr)


(if (featurep 'xemacs)
    (or (featurep 'lisp-float-type)
	(error "`ps-print' requires floating point support"))
  (unless (and (boundp 'emacs-major-version)
	       (>= emacs-major-version 23))
    (error "`ps-print' only supports Emacs 23 and higher")))


(defconst ps-windows-system
  (memq system-type '(ms-dos windows-nt)))
(defconst ps-lp-system
  (memq system-type '(usg-unix-v hpux irix)))


;; Load XEmacs/Emacs definitions
(require 'ps-def)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User Variables:


;;; Interface to the command system

(defgroup postscript nil
  "Support for printing and PostScript."
  :tag "PostScript"
  :version "20"
  :group 'external)

(defgroup ps-print nil
  "PostScript generator for Emacs."
  :link '(emacs-library-link :tag "Source Lisp File" "ps-print.el")
  :prefix "ps-"
  :version "20"
  :group 'wp
  :group 'postscript)

(defgroup ps-print-horizontal nil
  "Horizontal page layout."
  :prefix "ps-"
  :tag "Horizontal"
  :version "20"
  :group 'ps-print)

(defgroup ps-print-vertical nil
  "Vertical page layout."
  :prefix "ps-"
  :tag "Vertical"
  :version "20"
  :group 'ps-print)

(defgroup ps-print-headers nil
  "Headers & footers layout."
  :prefix "ps-"
  :tag "Header & Footer"
  :version "20"
  :group 'ps-print)

(defgroup ps-print-font nil
  "Fonts customization."
  :prefix "ps-"
  :tag "Font"
  :version "20"
  :group 'ps-print)

(defgroup ps-print-color nil
  "Color customization."
  :prefix "ps-"
  :tag "Color"
  :version "20"
  :group 'ps-print)

(defgroup ps-print-face nil
  "Faces customization."
  :prefix "ps-"
  :tag "PS Faces"
  :version "20"
  :group 'ps-print
  :group 'faces)

(defgroup ps-print-n-up nil
  "N-up customization."
  :prefix "ps-"
  :tag "N-Up"
  :version "20"
  :group 'ps-print)

(defgroup ps-print-zebra nil
  "Zebra customization."
  :prefix "ps-"
  :tag "Zebra"
  :version "20"
  :group 'ps-print)

(defgroup ps-print-background nil
  "Background customization."
  :prefix "ps-"
  :tag "Background"
  :version "20"
  :group 'ps-print)

(defgroup ps-print-printer '((lpr custom-group))
  "Printer customization."
  :prefix "ps-"
  :tag "Printer"
  :version "20"
  :group 'ps-print)

(defgroup ps-print-page nil
  "Page customization."
  :prefix "ps-"
  :tag "Page"
  :version "20"
  :group 'ps-print)

(defgroup ps-print-miscellany nil
  "Miscellany customization."
  :prefix "ps-"
  :tag "Miscellany"
  :version "20"
  :group 'ps-print)


(defcustom ps-error-handler-message 'paper
  "Specify where the error handler message should be sent.

Valid values are:

   `none'		catch the error and *DON'T* send any message.

   `paper'		catch the error and print on paper the error message.

   `system'		catch the error and send back the error message to
			printing system.  This is useful only if printing system
			send back an email reporting the error, or if there is
			some other alternative way to report back the error from
			the system to you.

   `paper-and-system'	catch the error, print on paper the error message and
			send back the error message to printing system.

Any other value is treated as `paper'."
  :type '(choice :menu-tag "Error Handler Message"
		 :tag "Error Handler Message"
		 (const none)   (const paper)
		 (const system) (const paper-and-system))
  :version "20"
  :group 'ps-print-miscellany)

(defcustom ps-user-defined-prologue nil
  "User defined PostScript prologue code inserted before all prologue code.

`ps-user-defined-prologue' may be a string or a symbol function which returns a
string.  Note that this string is inserted after `ps-adobe-tag' and PostScript
prologue comments, and before ps-print PostScript prologue code section.  That
is, this string is inserted after error handler initialization and before
ps-print settings.

It's strongly recommended only insert PostScript code and/or comments specific
for your printing system particularities.  For example, some special
initialization that only your printing system needs.

Do not insert code for duplex printing, n-up printing or error handler,
ps-print handles this in a suitable way.

For more information about PostScript, see:
   PostScript Language Reference Manual (2nd edition)
   Adobe Systems Incorporated

As an example for `ps-user-defined-prologue' setting:

   ;; Setting for HP PostScript printer
   (setq ps-user-defined-prologue
	 (concat \"<</DeferredMediaSelection true /PageSize [612 792] \"
		 \"/MediaPosition 2 /MediaType (Plain)>> setpagedevice\"))"
  :type '(choice :menu-tag "User Defined Prologue"
		 :tag "User Defined Prologue"
		 (const :tag "none" nil) string symbol)
  :version "20"
  :group 'ps-print-miscellany)

(defcustom ps-print-prologue-header nil
  "PostScript prologue header comments besides that ps-print generates.

`ps-print-prologue-header' may be a string or a symbol function which returns a
string.  Note that this string is inserted on PostScript prologue header
section which is used to define some document characteristic through PostScript
special comments, like \"%%Requirements: jog\\n\".

ps-print always inserts the %%Requirements: comment, so if you need to insert
more requirements put them first in `ps-print-prologue-header' using the
\"%%+\" comment.  For example, if you need to set numcopies to 3 and jog on
requirements and set %%LanguageLevel: to 2, do:

 (setq ps-print-prologue-header
       \"%%+ numcopies(3) jog\\n%%LanguageLevel: 2\\n\")

The duplex requirement is inserted by ps-print (see `ps-spool-duplex').

Do not forget to terminate the string with \"\\n\".

For more information about PostScript document comments, see:
   PostScript Language Reference Manual (2nd edition)
   Adobe Systems Incorporated
   Appendix G: Document Structuring Conventions -- Version 3.0"
  :type '(choice :menu-tag "Prologue Header"
		 :tag "Prologue Header"
		 (const :tag "none" nil) string symbol)
  :version "20"
  :group 'ps-print-miscellany)

(defcustom ps-printer-name (and (boundp 'printer-name)
				(symbol-value 'printer-name))
  "The name of a local printer for printing PostScript files.

On Unix-like systems, a string value should be a name understood by lpr's -P
option; a value of nil means use the value of `printer-name' instead.

On MS-DOS and MS-Windows systems, a string value is taken as the name of the
printer device or port to which PostScript files are written, provided
`ps-lpr-command' is \"\".  By default it is the same as `printer-name'; typical
non-default settings would be \"LPT1\" to \"LPT3\" for parallel printers, or
\"COM1\" to \"COM4\" or \"AUX\" for serial printers, or \"\\\\hostname\\printer\"
for a shared network printer.  You can also set it to a name of a file, in
which case the output gets appended to that file.  \(Note that `ps-print'
package already has facilities for printing to a file, so you might as well use
them instead of changing the setting of this variable.\)  If you want to
silently discard the printed output, set this to \"NUL\".

Set to t, if the utility given by `ps-lpr-command' needs an empty printer name.

Any other value is treated as t, that is, an empty printer name.

See also `ps-printer-name-option' for documentation."
  :type '(choice :menu-tag "Printer Name"
		 :tag "Printer Name"
		 (const :tag "Same as printer-name" nil)
		 (const :tag "No Printer Name" t)
		 (file :tag "Print to file")
		 (string :tag "Pipe to ps-lpr-command"))
  :version "20"
  :group 'ps-print-printer)

(defcustom ps-printer-name-option
  (cond (ps-windows-system
	 "/D:")
	(ps-lp-system
	 "-d")
	(t
	 "-P" ))
  "Option for `ps-printer-name' variable (see it).

On Unix-like systems, if `lpr' is in use, this should be the string
\"-P\"; if `lp' is in use, this should be the string \"-d\".

On MS-DOS and MS-Windows systems, if `print' is in use, this should be
the string \"/D:\".

For any other printing utility, see its documentation.

Set this to \"\" or nil, if the utility given by `ps-lpr-command'
needs an empty printer name option--that is, pass the printer name
with no special option preceding it.

Any value that is not a string is treated as nil.

This variable is used only when `ps-printer-name' is a non-empty string."
  :type '(choice :menu-tag "Printer Name Option"
		 :tag "Printer Name Option"
		 (const :tag "None" nil)
		 (string :tag "Option"))
  :version "21.1"
  :group 'ps-print-printer)

(defcustom ps-lpr-command lpr-command
  "Name of program for printing a PostScript file.

On MS-DOS and MS-Windows systems, if the value is an empty string then Emacs
will write directly to the printer port named by `ps-printer-name'.  The
programs `print' and `nprint' (the standard print programs on Windows NT and
Novell Netware respectively) are handled specially, using `ps-printer-name' as
the destination for output; any other program is treated like `lpr' except that
an explicit filename is given as the last argument."
  :type 'string
  :version "20"
  :group 'ps-print-printer)

(defcustom ps-lpr-switches lpr-switches
  "List of extra switches to pass to `ps-lpr-command'.

The list element can be:

   string	it should be an option for `ps-lpr-command' (which see).
		For example: \"-o Duplex=DuplexNoTumble\"

   symbol	it can be a function or variable symbol.  If it's a function
		symbol, it should be a function with no argument.  The result
		of the function or the variable value should be a string or a
		list of strings.

   list		the header should be a symbol function and the tail is the
		arguments for this function.  This function should return a
		string or a list of strings.

Any other value is silently ignored.

It is recommended to set `ps-printer-name' (which see) instead of including an
explicit switch on this list.

See `ps-lpr-command'."
  :type '(repeat :tag "PostScript lpr Switches"
		 (choice :menu-tag "PostScript lpr Switch"
			 :tag "PostScript lpr Switch"
			 string symbol (repeat sexp)))
  :version "20"
  :group 'ps-print-printer)

(defcustom ps-print-region-function nil
  "Specify a function to print the region on a PostScript printer.
See definition of `call-process-region' for calling conventions.  The fourth
and the sixth arguments are both nil."
  :type '(choice (const nil) function)
  :version "20"
  :group 'ps-print-printer)

(defcustom ps-manual-feed nil
  "Non-nil means the printer will manually feed paper.

If it's nil, automatic feeding takes place."
  :type 'boolean
  :version "20"
  :group 'ps-print-printer)

(defcustom ps-end-with-control-d (and ps-windows-system t)
  "Non-nil means insert C-d at end of PostScript file generated."
  :version "21.1"
  :type 'boolean
  :version "20"
  :group 'ps-print-printer)

;;; Page layout

;; All page dimensions are in PostScript points.
;; 1 inch  ==       2.54  cm    ==     72       points
;; 1 cm    ==  (/ 1 2.54) inch  ==  (/ 72 2.54) points

;; Letter      8.5   inch x 11.0   inch
;; Legal       8.5   inch x 14.0   inch
;; A4          8.26  inch x 11.69  inch = 21.0 cm x 29.7 cm

;; LetterSmall 7.68  inch x 10.16  inch
;; Tabloid    11.0   inch x 17.0   inch
;; Ledger     17.0   inch x 11.0   inch
;; Statement   5.5   inch x  8.5   inch
;; Executive   7.5   inch x 10.0   inch
;; A3         11.69  inch x 16.5   inch = 29.7 cm x 42.0 cm
;; A4Small     7.47  inch x 10.85  inch
;; B4         10.125 inch x 14.33  inch
;; B5          7.16  inch x 10.125 inch

;;;###autoload
(defcustom ps-page-dimensions-database
 (purecopy
  (list (list 'a4    (/ (* 72 21.0) 2.54)  (/ (* 72 29.7) 2.54) "A4")
	(list 'a3    (/ (* 72 29.7) 2.54)  (/ (* 72 42.0) 2.54) "A3")
	(list 'letter       (* 72  8.5)    (* 72 11.0)          "Letter")
	(list 'legal        (* 72  8.5)    (* 72 14.0)          "Legal")
	(list 'letter-small (* 72  7.68)   (* 72 10.16)         "LetterSmall")
	(list 'tabloid      (* 72 11.0)    (* 72 17.0)          "Tabloid")
	(list 'ledger       (* 72 17.0)    (* 72 11.0)          "Ledger")
	(list 'statement    (* 72  5.5)    (* 72  8.5)          "Statement")
	(list 'executive    (* 72  7.5)    (* 72 10.0)          "Executive")
	(list 'a4small      (* 72  7.47)   (* 72 10.85)         "A4Small")
	(list 'b4           (* 72 10.125)  (* 72 14.33)         "B4")
	(list 'b5           (* 72  7.16)   (* 72 10.125)        "B5")
	;; page sizes for label printer
	;; NOTE: the page sizes below don't have n-up > 1.
	'(addresslarge       236.0      99.0 "AddressLarge")
	'(addresssmall       236.0      68.0 "AddressSmall")
	'(cuthanging13        90.0     222.0 "CutHanging13")
	'(cuthanging15        90.0     114.0 "CutHanging15")
	'(diskette           181.0     136.0 "Diskette")
	'(eurofilefolder     139.0     112.0 "EuropeanFilefolder")
	'(eurofoldernarrow   526.0     107.0 "EuroFolderNarrow")
	'(eurofolderwide     526.0     136.0 "EuroFolderWide")
	'(euronamebadge      189.0     108.0 "EuroNameBadge")
	'(euronamebadgelarge 223.0     136.0 "EuroNameBadgeLarge")
	'(filefolder         230.0      37.0 "FileFolder")
	'(jewelry             76.0     136.0 "Jewelry")
	'(mediabadge         180.0     136.0 "MediaBadge")
	'(multipurpose       126.0      68.0 "MultiPurpose")
	'(retaillabel         90.0     104.0 "RetailLabel")
	'(shipping           271.0     136.0 "Shipping")
	'(slide35mm           26.0     104.0 "Slide35mm")
	'(spine8mm           187.0      26.0 "Spine8mm")
	'(topcoated          425.19685 136.0 "TopCoatedPaper")
	'(topcoatedpaper     396.0     136.0 "TopcoatedPaper150")
	'(vhsface            205.0     127.0 "VHSFace")
	'(vhsspine           400.0      50.0 "VHSSpine")
	'(zipdisk            156.0     136.0 "ZipDisk")))
  "List associating a symbolic paper type to its width, height and doc media.
See `ps-paper-type'."
  :type '(repeat (list :tag "Paper Type"
		       (symbol :tag "Symbol Name")
		       (number :tag "Width in points")
		       (number :tag "Height in points")
		       (string :tag "Media")))
  :version "20"
  :group 'ps-print-page)

;;;###autoload
(defcustom ps-paper-type 'letter
  "Specify the size of paper to format for.
Should be one of the paper types defined in `ps-page-dimensions-database', for
example `letter', `legal' or `a4'."
  :type '(symbol :validate (lambda (wid)
			     (if (assq (widget-value wid)
				       ps-page-dimensions-database)
				 nil
			       (widget-put wid :error "Unknown paper size")
			       wid)))
  :version "20"
  :group 'ps-print-page)

(defcustom ps-warn-paper-type t
  "Non-nil means give an error if paper size is not equal to `ps-paper-type'.

It's used when `ps-spool-config' is set to `setpagedevice'."
  :type 'boolean
  :version "20"
  :group 'ps-print-page)

(defcustom ps-landscape-mode nil
  "Non-nil means print in landscape mode."
  :type 'boolean
  :version "20"
  :group 'ps-print-page)

(defcustom ps-print-upside-down nil
  "Non-nil means print upside-down (that is, rotated by 180 degrees)."
  :type 'boolean
  :version "21.1"
  :group 'ps-print-page)

(defcustom ps-selected-pages nil
  "Specify which pages to print.

If nil, print all pages.

If a list, the lists element may be an integer or a cons cell (FROM . TO)
designating FROM page to TO page; any invalid element is ignored, that is, an
integer lesser than one or if FROM is greater than TO.

Otherwise, it's treated as nil.

After ps-print processing `ps-selected-pages' is set to nil.  But the
latest `ps-selected-pages' is saved in `ps-last-selected-pages' (which
see).  So you can restore the latest selected pages by using
`ps-last-selected-pages' or with the `ps-restore-selected-pages'
command (which see).

See also `ps-even-or-odd-pages'."
  :type '(repeat :tag "Selected Pages"
		 (radio :tag "Page"
			(integer :tag "Number")
			(cons :tag "Range"
			      (integer :tag "From")
			      (integer :tag "To"))))
  :version "20"
  :group 'ps-print-page)

(defcustom ps-even-or-odd-pages nil
  "Specify if it prints even/odd pages.

Valid values are:

   nil		print all pages.

   `even-page'	print only even pages.

   `odd-page'	print only odd pages.

   `even-sheet'	print only even sheets.
		That is, if `ps-n-up-printing' is 1, it behaves as `even-page';
		but for values greater than 1, it'll print only the even sheet
		of paper.

   `odd-sheet'	print only odd sheets.
		That is, if `ps-n-up-printing' is 1, it behaves as `odd-page';
		but for values greater than 1, it'll print only the odd sheet
		of paper.

Any other value is treated as nil.

If you set `ps-selected-pages' (see it for documentation), first the pages are
filtered by `ps-selected-pages' and then by `ps-even-or-odd-pages'.  For
example, if we have:

   (setq ps-selected-pages '(1 4 (6 . 10) (12 . 16) 20))

Combining with `ps-even-or-odd-pages' and `ps-n-up-printing', we have:

`ps-n-up-printing' = 1:
   `ps-even-or-odd-pages'	PAGES PRINTED
	nil			1, 4, 6, 7, 8, 9, 10, 12, 13, 14, 15, 16, 20
	even-page		4, 6, 8, 10, 12, 14, 16, 20
	odd-page		1, 7, 9, 13, 15
	even-sheet		4, 6, 8, 10, 12, 14, 16, 20
	odd-sheet		1, 7, 9, 13, 15

`ps-n-up-printing' = 2:
   `ps-even-or-odd-pages'	PAGES PRINTED
	nil			1/4, 6/7, 8/9, 10/12, 13/14, 15/16, 20
	even-page		4/6, 8/10, 12/14, 16/20
	odd-page		1/7, 9/13, 15
	even-sheet		6/7, 10/12, 15/16
	odd-sheet		1/4, 8/9, 13/14, 20

So even-page/odd-page are about page parity and even-sheet/odd-sheet are about
sheet parity."
  :type '(choice :menu-tag "Print Even/Odd Pages"
		 :tag "Print Even/Odd Pages"
		 (const :tag "All Pages" nil)
		 (const :tag "Only Even Pages" even-page)
		 (const :tag "Only Odd Pages" odd-page)
		 (const :tag "Only Even Sheets" even-sheet)
		 (const :tag "Only Odd Sheets" odd-sheet))
  :version "20"
  :group 'ps-print-page)

(defcustom ps-print-control-characters 'control-8-bit
  "Specify the printable form for control and 8-bit characters.
That is, instead of sending, for example, a ^D (\\004) to printer,
it is sent the string \"^D\".

Valid values are:

  `8-bit'         This is the value to use when you want an ASCII encoding of
		  any control or non-ASCII character.  Control characters are
		  encoded as \"^D\", and non-ASCII characters have an
		  octal encoding.

  `control-8-bit' This is the value to use when you want an ASCII encoding of
		  any control character, whether it is 7 or 8-bit.
		  European 8-bits accented characters are printed according
		  the current font.

  `control'       Only ASCII control characters have an ASCII encoding.
		  European 8-bits accented characters are printed according
		  the current font.

  nil             No ASCII encoding.  Any character is printed according the
		  current font.

Any other value is treated as nil."
  :type '(choice :menu-tag "Control Char"
		 :tag "Control Char"
		 (const 8-bit)   (const control-8-bit)
		 (const control) (const :tag "nil" nil))
  :version "20"
  :group 'ps-print-miscellany)

(defcustom ps-n-up-printing 1
  "Specify the number of pages per sheet paper."
  :type '(integer
	  :tag "N Up Printing"
	  :validate
	  (lambda (wid)
	    (if (and (< 0 (widget-value wid))
		     (<= (widget-value wid) 100))
		nil
	      (widget-put
	       wid :error
	       "Number of pages per sheet paper must be between 1 and 100.")
	      wid)))
  :version "20"
  :group 'ps-print-n-up)

(defcustom ps-n-up-margin (/ (* 72  1.0) 2.54) ; 1 cm
  "Specify the margin in points between the sheet border and n-up printing."
  :type 'number
  :version "20"
  :group 'ps-print-n-up)

(defcustom ps-n-up-border-p t
  "Non-nil means a border is drawn around each page."
  :type 'boolean
  :version "20"
  :group 'ps-print-n-up)

(defcustom ps-n-up-filling 'left-top
  "Specify how page matrix is filled on each sheet of paper.

Following are the valid values for `ps-n-up-filling' with a filling example
using a 3x4 page matrix:

   `left-top'   1  2  3  4         `left-bottom'    9  10 11 12
		5  6  7  8                          5  6  7  8
		9  10 11 12                         1  2  3  4

   `right-top'  4  3  2  1         `right-bottom'   12 11 10 9
		8  7  6  5                          8  7  6  5
		12 11 10 9                          4  3  2  1

   `top-left'   1  4  7  10        `bottom-left'    3  6  9  12
		2  5  8  11                         2  5  8  11
		3  6  9  12                         1  4  7  10

   `top-right'  10 7  4  1         `bottom-right'   12 9  6  3
		11 8  5  2                          11 8  5  2
		12 9  6  3                          10 7  4  1

Any other value is treated as `left-top'."
  :type '(choice :menu-tag "N-Up Filling"
		 :tag "N-Up Filling"
		 (const left-top)  (const left-bottom)
		 (const right-top) (const right-bottom)
		 (const top-left)  (const bottom-left)
		 (const top-right) (const bottom-right))
  :version "20"
  :group 'ps-print-n-up)

(defcustom ps-number-of-columns (if ps-landscape-mode 2 1)
  "Specify the number of columns."
  :type 'number
  :version "20"
  :group 'ps-print-miscellany)

(defcustom ps-zebra-stripes nil
  "Non-nil means print zebra stripes.
See also documentation for `ps-zebra-stripe-height' and `ps-zebra-color'."
  :type 'boolean
  :version "20"
  :group 'ps-print-zebra)

(defcustom ps-zebra-stripe-height 3
  "Number of zebra stripe lines.
See also documentation for `ps-zebra-stripes' and `ps-zebra-color'."
  :type 'number
  :version "20"
  :group 'ps-print-zebra)

(defcustom ps-zebra-color 0.95
  "Zebra stripe gray scale or RGB color.
See also documentation for `ps-zebra-stripes' and `ps-zebra-stripe-height'."
  :type '(choice :menu-tag "Zebra Gray/Color"
		 :tag "Zebra Gray/Color"
		 (number :tag "Gray Scale" :value 0.95)
		 (string :tag "Color Name" :value "gray95")
		 (list :tag "RGB Color" :value (0.95 0.95 0.95)
		       (number :tag "Red")
		       (number :tag "Green")
		       (number :tag "Blue")))
  :version "20"
  :group 'ps-print-zebra)

(defcustom ps-zebra-stripe-follow nil
  "Specify how zebra stripes continue on next page.

Visually, valid values are (the character `+' at right of each column indicates
that a line is printed):

		   `nil'        `follow'        `full'        `full-follow'
   Current Page --------     -----------     ---------     ----------------
		1  XXXXX +   1  XXXXXXXX +   1  XXXXXX +   1  XXXXXXXXXXXXX +
		2  XXXXX +   2  XXXXXXXX +   2  XXXXXX +   2  XXXXXXXXXXXXX +
		3  XXXXX +   3  XXXXXXXX +   3  XXXXXX +   3  XXXXXXXXXXXXX +
		4        +   4           +   4         +   4                +
		5        +   5           +   5         +   5                +
		6        +   6           +   6         +   6                +
		7  XXXXX +   7  XXXXXXXX +   7  XXXXXX +   7  XXXXXXXXXXXXX +
		8  XXXXX +   8  XXXXXXXX +   8  XXXXXX +   8  XXXXXXXXXXXXX +
		9  XXXXX +   9  XXXXXXXX +   9  XXXXXX +   9  XXXXXXXXXXXXX +
		10       +   10          +
		11       +   11          +
		--------     -----------     ---------     ----------------
      Next Page --------     -----------     ---------     ----------------
		12 XXXXX +   12          +   10 XXXXXX +   10               +
		13 XXXXX +   13 XXXXXXXX +   11 XXXXXX +   11               +
		14 XXXXX +   14 XXXXXXXX +   12 XXXXXX +   12               +
		15       +   15 XXXXXXXX +   13        +   13 XXXXXXXXXXXXX +
		16       +   16          +   14        +   14 XXXXXXXXXXXXX +
		17       +   17          +   15        +   15 XXXXXXXXXXXXX +
		18 XXXXX +   18          +   16 XXXXXX +   16               +
		19 XXXXX +   19 XXXXXXXX +   17 XXXXXX +   17               +
		20 XXXXX +   20 XXXXXXXX +   18 XXXXXX +   18               +
		21       +   21 XXXXXXXX +
		22       +   22          +
		--------     -----------     ---------     ----------------

Any other value is treated as nil."
  :type '(choice :menu-tag "Zebra Stripe Follow"
		 :tag "Zebra Stripe Follow"
		 (const :tag "Always Restart" nil)
		 (const :tag "Continue on Next Page" follow)
		 (const :tag "Print Only Full Stripe" full)
		 (const :tag "Continue on Full Stripe" full-follow))
  :version "20"
  :group 'ps-print-zebra)

(defcustom ps-line-number nil
  "Non-nil means print line number."
  :type 'boolean
  :version "20"
  :group 'ps-print-miscellany)

(defcustom ps-line-number-step 1
  "Specify the interval that line number is printed.

For example, `ps-line-number-step' is set to 2, the printing will look like:

   1 one line
     one line
   3 one line
     one line
   5 one line
     one line
     ...

Valid values are:

   integer	an integer that specifies the interval that line number is
		printed.  If it's lesser than or equal to zero, it's used the
		value 1.

   `zebra'	specifies that only the line number of the first line in a
		zebra stripe is to be printed.

Any other value is treated as `zebra'."
  :type '(choice :menu-tag "Line Number Step"
		 :tag "Line Number Step"
		 (integer :tag "Step Interval")
		 (const :tag "Synchronize Zebra" zebra))
  :version "20"
  :group 'ps-print-miscellany)

(defcustom ps-line-number-start 1
  "Specify the starting point in the interval given by `ps-line-number-step'.

For example, if `ps-line-number-step' is set to 3 and `ps-line-number-start' is
set to 3, the printing will look like:

      one line
      one line
    3 one line
      one line
      one line
    6 one line
      one line
      one line
    9 one line
      one line
      ...

The values for `ps-line-number-start':

   * If `ps-line-number-step' is an integer, must be between 1 and the value of
     `ps-line-number-step' inclusive.

   * If `ps-line-number-step' is set to `zebra', must be between 1 and the
     value of `ps-zebra-strip-height' inclusive.  Use this combination if you
     wish that line number be relative to zebra stripes."
  :type '(integer :tag "Start Step Interval")
  :version "20"
  :group 'ps-print-miscellany)

(defcustom ps-print-background-image nil
  "EPS image list to be printed on background.

The elements are:

   (FILENAME X Y XSCALE YSCALE ROTATION PAGES...)

FILENAME is a file name which contains an EPS image or some PostScript
programming like EPS.
FILENAME is ignored, if it doesn't exist or is read protected.

X and Y are relative positions on paper to put the image.
If X and Y are nil, the image is centered on paper.

XSCALE and YSCALE are scale factor to be applied to image before printing.
If XSCALE and YSCALE are nil, the original size is used.

ROTATION is the image rotation angle; if nil, the default is 0.

PAGES designates the page to print background image.
PAGES may be a number or a cons cell (FROM . TO) designating FROM page to TO
page.
If PAGES is nil, print background image on all pages.

X, Y, XSCALE, YSCALE and ROTATION may be a floating point number, an integer
number or a string.  If it is a string, the string should contain PostScript
programming that returns a float or integer value.

For example, if you wish to print an EPS image on all pages do:

   '((\"~/images/EPS-image.ps\"))"
  :type '(repeat
	  (list
	   (file   :tag "EPS File")
	   (choice :tag "X" (const :tag "default" nil) number string)
	   (choice :tag "Y" (const :tag "default" nil) number string)
	   (choice :tag "X Scale" (const :tag "default" nil) number string)
	   (choice :tag "Y Scale" (const :tag "default" nil) number string)
	   (choice :tag "Rotation" (const :tag "default" nil) number string)
	   (repeat :tag "Pages" :inline t
		   (radio (integer :tag "Page")
			  (cons :tag "Range"
				(integer :tag "From")
				(integer :tag "To"))))))
  :version "20"
  :group 'ps-print-background)

(defcustom ps-print-background-text nil
  "Text list to be printed on background.

The elements are:

   (STRING X Y FONT FONTSIZE GRAY ROTATION PAGES...)

STRING is the text to be printed on background.

X and Y are positions on paper to put the text.
If X and Y are nil, the text is positioned at lower left corner.

FONT is a font name to be used on printing the text.
If nil, \"Times-Roman\" is used.

FONTSIZE is font size to be used, if nil, 200 is used.

GRAY is the text gray factor (should be very light like 0.8).
If nil, the default is 0.85.

ROTATION is the text rotation angle; if nil, the angle is given by the diagonal
from lower left corner to upper right corner.

PAGES designates the page to print background text.
PAGES may be a number or a cons cell (FROM . TO) designating FROM page to TO
page.
If PAGES is nil, print background text on all pages.

X, Y, FONTSIZE, GRAY and ROTATION may be a floating point number, an integer
number or a string.  If it is a string, the string should contain PostScript
programming that returns a float or integer value.

For example, if you wish to print text \"Preliminary\" on all pages do:

   '((\"Preliminary\"))"
  :type '(repeat
	  (list
	   (string :tag "Text")
	   (choice :tag "X" (const :tag "default" nil) number string)
	   (choice :tag "Y" (const :tag "default" nil) number string)
	   (choice :tag "Font" (const :tag "default" nil) string)
	   (choice :tag "Fontsize" (const :tag "default" nil) number string)
	   (choice :tag "Gray" (const :tag "default" nil) number string)
	   (choice :tag "Rotation" (const :tag "default" nil) number string)
	   (repeat :tag "Pages" :inline t
		   (radio (integer :tag "Page")
			  (cons :tag "Range"
				(integer :tag "From")
				(integer :tag "To"))))))
  :version "20"
  :group 'ps-print-background)

;;; Horizontal layout

;;  ------------------------------------------
;;  |    |      |    |      |    |      |    |
;;  | lm | text | ic | text | ic | text | rm |
;;  |    |      |    |      |    |      |    |
;;  ------------------------------------------

(defcustom ps-left-margin   (/ (* 72  2.0) 2.54) ;   2 cm
  "Left margin in points (1/72 inch)."
  :type 'number
  :version "20"
  :group 'ps-print-horizontal)

(defcustom ps-right-margin  (/ (* 72  2.0) 2.54) ;   2 cm
  "Right margin in points (1/72 inch)."
  :type 'number
  :version "20"
  :group 'ps-print-horizontal)

(defcustom ps-inter-column  (/ (* 72  2.0) 2.54) ;   2 cm
  "Horizontal space between columns in points (1/72 inch)."
  :type 'number
  :version "20"
  :group 'ps-print-horizontal)

;;; Vertical layout

;; |--------|
;; | tm     |
;; |--------|
;; | header |
;; |--------|
;; | ho     |
;; |--------|
;; | text   |
;; |--------|
;; | bm     |
;; |--------|

(defcustom ps-bottom-margin (/ (* 72  1.5) 2.54) ; 1.5 cm
  "Bottom margin in points (1/72 inch)."
  :type 'number
  :version "20"
  :group 'ps-print-vertical)

(defcustom ps-top-margin    (/ (* 72  1.5) 2.54) ; 1.5 cm
  "Top margin in points (1/72 inch)."
  :type 'number
  :version "20"
  :group 'ps-print-vertical)

(defcustom ps-header-offset (/ (* 72  1.0) 2.54) ; 1.0 cm
  "Vertical space in points (1/72 inch) between the main text and the header."
  :type 'number
  :version "20"
  :group 'ps-print-vertical)

(defcustom ps-header-line-pad 0.15
  "Portion of a header title line height to insert.
The insertion is done between the header frame and the text it contains,
both in the vertical and horizontal directions."
  :type 'number
  :version "20"
  :group 'ps-print-vertical)

(defcustom ps-footer-offset (/ (* 72  1.0) 2.54) ; 1.0 cm
  "Vertical space in points (1/72 inch) between the main text and the footer."
  :type 'number
  :version "20"
  :group 'ps-print-vertical)

(defcustom ps-footer-line-pad 0.15
  "Portion of a footer title line height to insert.
The insertion is done between the footer frame and the text it contains,
both in the vertical and horizontal directions."
  :type 'number
  :version "20"
  :group 'ps-print-vertical)

;;; Header/Footer setup

(defcustom ps-print-header t
  "Non-nil means print a header at the top of each page.
By default, the header displays the buffer name, page number, and, if the
buffer is visiting a file, the file's directory.  Headers are customizable by
changing variables `ps-left-header' and `ps-right-header'."
  :type 'boolean
  :version "20"
  :group 'ps-print-headers)

(defcustom ps-print-header-frame t
  "Non-nil means draw a gaudy frame around the header."
  :type 'boolean
  :version "20"
  :group 'ps-print-headers)

(defcustom ps-header-frame-alist
  '((fore-color   . 0.0)
    (back-color   . 0.9)
    (border-width . 0.4)
    (border-color . 0.0)
    (shadow-color . 0.0))
  "Specify header frame properties alist.

Valid frame properties are:

   `fore-color'		Specify the foreground frame color.
			It should be a float number between 0.0 (black color)
			and 1.0 (white color), a string which is a color name,
			or a list of 3 float numbers which corresponds to the
			Red Green Blue color scale, each float number between
			0.0 (dark color) and 1.0 (bright color).

   `back-color'		Specify the background frame color (similar to
			`fore-color').

   `shadow-color'	Specify the shadow color (similar to `fore-color').

   `border-color'	Specify the border color (similar to `fore-color').

   `border-width'	Specify the border width.

Any other property is ignored.

Don't change this alist directly, instead use customization, or `ps-value',
`ps-get', `ps-put' and `ps-del' functions (see them for documentation)."
  :version "21.1"
  :type '(repeat
	  (choice :menu-tag "Header Frame Element"
		  :tag ""
		  (cons :tag "Foreground Color" :format "%v"
			(const :format "" fore-color)
			(choice :menu-tag "Foreground Color"
				:tag "Foreground Color"
				(number :tag "Gray Scale" :value 0.0)
				(string :tag "Color Name" :value "black")
				(list :tag "RGB Color" :value (0.0 0.0 0.0)
				      (number :tag "Red")
				      (number :tag "Green")
				      (number :tag "Blue"))))
		  (cons :tag "Background Color" :format "%v"
			(const :format "" back-color)
			(choice :menu-tag "Background Color"
				:tag "Background Color"
				(number :tag "Gray Scale" :value 0.9)
				(string :tag "Color Name" :value "gray90")
				(list :tag "RGB Color" :value (0.9 0.9 0.9)
				      (number :tag "Red")
				      (number :tag "Green")
				      (number :tag "Blue"))))
		  (cons :tag "Border Width" :format "%v"
			(const :format "" border-width)
			(number :tag "Border Width" :value 0.4))
		  (cons :tag "Border Color" :format "%v"
			(const :format "" border-color)
			(choice :menu-tag "Border Color"
				:tag "Border Color"
				(number :tag "Gray Scale" :value 0.0)
				(string :tag "Color Name" :value "black")
				(list :tag "RGB Color" :value (0.0 0.0 0.0)
				      (number :tag "Red")
				      (number :tag "Green")
				      (number :tag "Blue"))))
		  (cons :tag "Shadow Color" :format "%v"
			(const :format "" shadow-color)
			(choice :menu-tag "Shadow Color"
				:tag "Shadow Color"
				(number :tag "Gray Scale" :value 0.0)
				(string :tag "Color Name" :value "black")
				(list :tag "RGB Color" :value (0.0 0.0 0.0)
				      (number :tag "Red")
				      (number :tag "Green")
				      (number :tag "Blue"))))))
  :version "20"
  :group 'ps-print-headers)

(defcustom ps-header-lines 2
  "Number of lines to display in page header, when generating PostScript."
  :type 'integer
  :version "20"
  :group 'ps-print-headers)

(defcustom ps-print-footer nil
  "Non-nil means print a footer at the bottom of each page.
By default, the footer displays page number.
Footers are customizable by changing variables `ps-left-footer' and
`ps-right-footer'."
  :type 'boolean
  :version "21.1"
  :group 'ps-print-headers)

(defcustom ps-print-footer-frame t
  "Non-nil means draw a gaudy frame around the footer."
  :type 'boolean
  :version "21.1"
  :group 'ps-print-headers)

(defcustom ps-footer-frame-alist
  '((fore-color   . 0.0)
    (back-color   . 0.9)
    (border-width . 0.4)
    (border-color . 0.0)
    (shadow-color . 0.0))
  "Specify footer frame properties alist.

Don't change this alist directly, instead use customization, or `ps-value',
`ps-get', `ps-put' and `ps-del' functions (see them for documentation).

See also `ps-header-frame-alist' for documentation."
  :type '(repeat
	  (choice :menu-tag "Header Frame Element"
		  :tag ""
		  (cons :tag "Foreground Color" :format "%v"
			(const :format "" fore-color)
			(choice :menu-tag "Foreground Color"
				:tag "Foreground Color"
				(number :tag "Gray Scale" :value 0.0)
				(string :tag "Color Name" :value "black")
				(list :tag "RGB Color" :value (0.0 0.0 0.0)
				      (number :tag "Red")
				      (number :tag "Green")
				      (number :tag "Blue"))))
		  (cons :tag "Background Color" :format "%v"
			(const :format "" back-color)
			(choice :menu-tag "Background Color"
				:tag "Background Color"
				(number :tag "Gray Scale" :value 0.9)
				(string :tag "Color Name" :value "gray90")
				(list :tag "RGB Color" :value (0.9 0.9 0.9)
				      (number :tag "Red")
				      (number :tag "Green")
				      (number :tag "Blue"))))
		  (cons :tag "Border Width" :format "%v"
			(const :format "" border-width)
			(number :tag "Border Width" :value 0.4))
		  (cons :tag "Border Color" :format "%v"
			(const :format "" border-color)
			(choice :menu-tag "Border Color"
				:tag "Border Color"
				(number :tag "Gray Scale" :value 0.0)
				(string :tag "Color Name" :value "black")
				(list :tag "RGB Color" :value (0.0 0.0 0.0)
				      (number :tag "Red")
				      (number :tag "Green")
				      (number :tag "Blue"))))
		  (cons :tag "Shadow Color" :format "%v"
			(const :format "" shadow-color)
			(choice :menu-tag "Shadow Color"
				:tag "Shadow Color"
				(number :tag "Gray Scale" :value 0.0)
				(string :tag "Color Name" :value "black")
				(list :tag "RGB Color" :value (0.0 0.0 0.0)
				      (number :tag "Red")
				      (number :tag "Green")
				      (number :tag "Blue"))))))
  :version "21.1"
  :group 'ps-print-headers)

(defcustom ps-footer-lines 2
  "Number of lines to display in page footer, when generating PostScript."
  :type 'integer
  :version "21.1"
  :group 'ps-print-headers)

(defcustom ps-print-only-one-header nil
  "Non-nil means print only one header/footer at the top/bottom of each page.
This is useful when printing more than one column, so it is possible to have
only one header/footer over all columns or one header/footer per column.
See also `ps-print-header' and `ps-print-footer'."
  :type 'boolean
  :version "20"
  :group 'ps-print-headers)

(defcustom ps-switch-header 'duplex
  "Specify if headers/footers are switched or not.

Valid values are:

nil	Never switch headers/footers.

t	Always switch headers/footers.

duplex	Switch headers/footers only when duplexing is on, that is, when
	`ps-spool-duplex' is non-nil.

Any other value is treated as t.

See also `ps-print-header' and `ps-print-footer'."
  :type '(choice :menu-tag "Switch Header/Footer"
		 :tag "Switch Header/Footer"
		 (const :tag "Never Switch" nil)
		 (const :tag "Always Switch" t)
		 (const :tag "Switch When Duplexing" duplex))
  :version "20"
  :group 'ps-print-headers)

(defcustom ps-show-n-of-n t
  "Non-nil means show page numbers as N/M, meaning page N of M.
NOTE: page numbers are displayed as part of headers,
      see variable `ps-print-header'."
  :type 'boolean
  :version "20"
  :group 'ps-print-headers)

(defcustom ps-spool-config
  (if ps-windows-system
      nil
    'lpr-switches)
  "Specify who is responsible for setting duplex and page size.

Valid values are:

  `lpr-switches'    duplex and page size are configured by `ps-lpr-switches'.
		    Don't forget to set `ps-lpr-switches' to select duplex
		    printing for your printer.

  `setpagedevice'   duplex and page size are configured by ps-print using the
		    setpagedevice PostScript operator.

  nil               duplex and page size are configured by ps-print *not* using
		    the setpagedevice PostScript operator.

Any other value is treated as nil.

WARNING: The setpagedevice PostScript operator affects ghostview utility when
	 viewing file generated using landscape.  Also on some printers,
	 setpagedevice affects zebra stripes; on other printers, setpagedevice
	 affects the left margin.
	 Besides all that, if your printer does not have the paper size
	 specified by setpagedevice, your printing will be aborted.
	 So, if you need to use setpagedevice, set `ps-spool-config' to
	 `setpagedevice', generate a test file and send it to your printer; if
	 the printed file isn't OK, set `ps-spool-config' to nil."
  :type '(choice :menu-tag "Spool Config"
		 :tag "Spool Config"
		 (const lpr-switches) (const setpagedevice)
		 (const :tag "nil" nil))
  :version "20"
  :group 'ps-print-headers)

(defcustom ps-spool-duplex nil		; Not many people have duplex printers,
					; so default to nil.
  "Non-nil generates PostScript for a two-sided printer.
For a duplex printer, the `ps-spool-*' and `ps-print-*' commands will insert
blank pages as needed between print jobs so that the next buffer printed will
start on the right page.  Also, if headers are turned on, the headers will be
reversed on duplex printers so that the page numbers fall to the left on
even-numbered pages.

See also `ps-spool-tumble'."
  :type 'boolean
  :version "20"
  :group 'ps-print-headers)

(defcustom ps-spool-tumble nil
  "Specify how the page images on opposite sides of a sheet are oriented.
If `ps-spool-tumble' is nil, produces output suitable for binding on the left
or right.  If `ps-spool-tumble' is non-nil, produces output suitable for
binding at the top or bottom.

It has effect only when `ps-spool-duplex' is non-nil."
  :type 'boolean
  :version "20"
  :group 'ps-print-headers)

;;; Fonts

(defcustom ps-font-info-database
  '((Courier				; the family key
     (fonts (normal      . "Courier")
	    (bold        . "Courier-Bold")
	    (italic      . "Courier-Oblique")
	    (bold-italic . "Courier-BoldOblique"))
     (size . 10.0)
     (line-height . 10.55)
     (space-width . 6.0)
     (avg-char-width . 6.0))
    (Helvetica				; the family key
     (fonts (normal      . "Helvetica")
	    (bold        . "Helvetica-Bold")
	    (italic      . "Helvetica-Oblique")
	    (bold-italic . "Helvetica-BoldOblique"))
     (size . 10.0)
     (line-height . 11.56)
     (space-width . 2.78)
     (avg-char-width . 5.09243))
    (Times
     (fonts (normal      . "Times-Roman")
	    (bold        . "Times-Bold")
	    (italic      . "Times-Italic")
	    (bold-italic . "Times-BoldItalic"))
     (size . 10.0)
     (line-height . 11.0)
     (space-width . 2.5)
     (avg-char-width . 4.71432))
    (Palatino
     (fonts (normal      . "Palatino-Roman")
	    (bold        . "Palatino-Bold")
	    (italic      . "Palatino-Italic")
	    (bold-italic . "Palatino-BoldItalic"))
     (size . 10.0)
     (line-height . 12.1)
     (space-width . 2.5)
     (avg-char-width . 5.08676))
    (Helvetica-Narrow
     (fonts (normal      . "Helvetica-Narrow")
	    (bold        . "Helvetica-Narrow-Bold")
	    (italic      . "Helvetica-Narrow-Oblique")
	    (bold-italic . "Helvetica-Narrow-BoldOblique"))
     (size . 10.0)
     (line-height . 11.56)
     (space-width . 2.2796)
     (avg-char-width . 4.17579))
    (NewCenturySchlbk
     (fonts (normal      . "NewCenturySchlbk-Roman")
	    (bold        . "NewCenturySchlbk-Bold")
	    (italic      . "NewCenturySchlbk-Italic")
	    (bold-italic . "NewCenturySchlbk-BoldItalic"))
     (size . 10.0)
     (line-height . 12.15)
     (space-width . 2.78)
     (avg-char-width . 5.31162))
    ;; got no bold for the next ones
    (AvantGarde-Book
     (fonts (normal . "AvantGarde-Book")
	    (italic . "AvantGarde-BookOblique"))
     (size . 10.0)
     (line-height . 11.77)
     (space-width . 2.77)
     (avg-char-width . 5.45189))
    (AvantGarde-Demi
     (fonts (normal . "AvantGarde-Demi")
	    (italic . "AvantGarde-DemiOblique"))
     (size . 10.0)
     (line-height . 12.72)
     (space-width . 2.8)
     (avg-char-width . 5.51351))
    (Bookman-Demi
     (fonts (normal . "Bookman-Demi")
	    (italic . "Bookman-DemiItalic"))
     (size . 10.0)
     (line-height . 11.77)
     (space-width . 3.4)
     (avg-char-width . 6.05946))
    (Bookman-Light
     (fonts (normal . "Bookman-Light")
	    (italic . "Bookman-LightItalic"))
     (size . 10.0)
     (line-height . 11.79)
     (space-width . 3.2)
     (avg-char-width . 5.67027))
    ;; got no bold and no italic for the next ones
    (Symbol
     (fonts (normal . "Symbol"))
     (size . 10.0)
     (line-height . 13.03)
     (space-width . 2.5)
     (avg-char-width . 3.24324))
    (Zapf-Dingbats
     (fonts (normal . "Zapf-Dingbats"))
     (size . 10.0)
     (line-height . 9.63)
     (space-width . 2.78)
     (avg-char-width . 2.78))
    (ZapfChancery-MediumItalic
     (fonts (normal . "ZapfChancery-MediumItalic"))
     (size . 10.0)
     (line-height . 11.45)
     (space-width . 2.2)
     (avg-char-width . 4.10811))
    ;; We keep this wrong entry name (but with correct font name) for
    ;; backward compatibility.
    (Zapf-Chancery-MediumItalic
     (fonts (normal . "ZapfChancery-MediumItalic"))
     (size . 10.0)
     (line-height . 11.45)
     (space-width . 2.2)
     (avg-char-width . 4.10811))
    )
  "Font info database.
Each element comprises: font family (the key), name, bold, italic, bold-italic,
reference size, line height, space width, average character width.
To get the info for another specific font (say Helvetica), do the following:
- create a new buffer
- generate the PostScript image to a file (C-u M-x ps-print-buffer)
- open this file and delete the leading `%' (which is the PostScript comment
  character) from the line
	   `% 3 cm 20 cm moveto  10/Courier ReportFontInfo  showpage'
  to get the line
	   `3 cm 20 cm moveto  10/Helvetica ReportFontInfo  showpage'
- add the values to `ps-font-info-database'.
You can get all the fonts of YOUR printer using `ReportAllFontInfo'.

Note also that ps-print DOESN'T download any font to your printer, instead it
uses the fonts resident in your printer."
  :type '(repeat
	  (list :tag "Font Definition"
		(symbol :tag "Font Family")
		(cons :format "%v"
		      (const :format "" fonts)
		      (repeat :tag "Faces"
			      (cons (choice :menu-tag "Font Weight/Slant"
					    :tag "Font Weight/Slant"
					    (const normal)
					    (const bold)
					    (const italic)
					    (const bold-italic)
					    (symbol :tag "Face"))
				    (string :tag "Font Name"))))
		(cons :format "%v"
		      (const :format "" size)
		      (number :tag "Reference Size"))
		(cons :format "%v"
		      (const :format "" line-height)
		      (number :tag "Line Height"))
		(cons :format "%v"
		      (const :format "" space-width)
		      (number :tag "Space Width"))
		(cons :format "%v"
		      (const :format "" avg-char-width)
		      (number :tag "Average Character Width"))))
  :version "20"
  :group 'ps-print-font)

(defcustom ps-font-family 'Courier
  "Font family name for ordinary text, when generating PostScript."
  :type 'symbol
  :version "20"
  :group 'ps-print-font)

(defcustom ps-font-size   '(7 . 8.5)
  "Font size, in points, for ordinary text, when generating PostScript.
Either a float or a cons of floats (LANDSCAPE-SIZE . PORTRAIT-SIZE)."
  :type '(choice :menu-tag "Ordinary Text Font Size"
		 :tag "Ordinary Text Font Size"
		 (number :tag "Text Size")
		 (cons :tag "Landscape/Portrait"
		       (number :tag "Landscape Text Size")
		       (number :tag "Portrait Text Size")))
  :version "20"
  :group 'ps-print-font)

(defcustom ps-header-font-family      'Helvetica
  "Font family name for text in the header, when generating PostScript."
  :type 'symbol
  :version "20"
  :group 'ps-print-font)

(defcustom ps-header-font-size       '(10 . 12)
  "Font size, in points, for text in the header, when generating PostScript.
Either a float or a cons of floats (LANDSCAPE-SIZE . PORTRAIT-SIZE)."
  :type '(choice :menu-tag "Header Font Size"
		 :tag "Header Font Size"
		 (number :tag "Header Size")
		 (cons :tag "Landscape/Portrait"
		       (number :tag "Landscape Header Size")
		       (number :tag "Portrait Header Size")))
  :version "20"
  :group 'ps-print-font)

(defcustom ps-header-title-font-size '(12 . 14)
  "Font size, in points, for the top line of text in header, in PostScript.
Either a float or a cons of floats (LANDSCAPE-SIZE . PORTRAIT-SIZE)."
  :type '(choice :menu-tag "Header Title Font Size"
		 :tag "Header Title Font Size"
		 (number :tag "Header Title Size")
		 (cons :tag "Landscape/Portrait"
		       (number :tag "Landscape Header Title Size")
		       (number :tag "Portrait Header Title Size")))
  :version "20"
  :group 'ps-print-font)

(defcustom ps-footer-font-family      'Helvetica
  "Font family name for text in the footer, when generating PostScript."
  :type 'symbol
  :version "21.1"
  :group 'ps-print-font)

(defcustom ps-footer-font-size       '(10 . 12)
  "Font size, in points, for text in the footer, when generating PostScript.
Either a float or a cons of floats (LANDSCAPE-SIZE . PORTRAIT-SIZE)."
  :type '(choice :menu-tag "Footer Font Size"
		 :tag "Footer Font Size"
		 (number :tag "Footer Size")
		 (cons :tag "Landscape/Portrait"
		       (number :tag "Landscape Footer Size")
		       (number :tag "Portrait Footer Size")))
  :version "21.1"
  :group 'ps-print-font)

(defcustom ps-line-number-color      "black"
  "Specify color for line-number, when generating PostScript."
  :type '(choice :menu-tag "Line Number Color"
		 :tag "Line Number Color"
		 (number :tag "Gray Scale" :value 0)
		 (string :tag "Color Name" :value "black")
		 (list :tag "RGB Color" :value (0 0 0)
		       (number :tag "Red")
		       (number :tag "Green")
		       (number :tag "Blue")))
  :version "21.1"
  :group 'ps-print-font
  :group 'ps-print-miscellany)

(defcustom ps-line-number-font      "Times-Italic"
  "Font for line-number, when generating PostScript."
  :type 'string
  :version "20"
  :group 'ps-print-font
  :group 'ps-print-miscellany)

(defcustom ps-line-number-font-size 6
  "Font size, in points, for line number, when generating PostScript.
Either a float or a cons of floats (LANDSCAPE-SIZE . PORTRAIT-SIZE)."
  :type '(choice :menu-tag "Line Number Font Size"
		 :tag "Line Number Font Size"
		 (number :tag "Font Size")
		 (cons :tag "Landscape/Portrait"
		       (number :tag "Landscape Font Size")
		       (number :tag "Portrait Font Size")))
  :version "20"
  :group 'ps-print-font
  :group 'ps-print-miscellany)

;;; Colors

;; Printing color requires x-color-values.
;; XEmacs change: Need autoload for the "Options->Printing->Color Printing"
;;                widget to work.
;;;###autoload
(defcustom ps-print-color-p
  (or (fboundp 'x-color-values)		; Emacs
      (fboundp 'color-instance-rgb-components))
					; XEmacs
  "Specify how buffer's text color is printed.

Valid values are:

   nil		Do not print colors.

   t		Print colors.

   black-white	Print colors on black/white printer.
		See also `ps-black-white-faces'.

Any other value is treated as t."
  :type '(choice :menu-tag "Print Color"
		 :tag "Print Color"
		 (const :tag "Do NOT Print Color" nil)
		 (const :tag "Print Always Color" t)
		 (const :tag "Print Black/White Color" black-white))
  :version "20"
  :group 'ps-print-color)

(defcustom ps-default-fg nil
  "RGB values of the default foreground color.

The `ps-default-fg' variable contains the default foreground color used by
ps-print, that is, if there is a face in a text that doesn't have a foreground
color, the `ps-default-fg' color should be used.

Valid values are:

   t		The foreground color of Emacs session will be used.

   frame-parameter	The foreground-color frame parameter will be used.

   NUMBER	It's a real value between 0.0 (black) and 1.0 (white) that
		indicate the gray color.

   COLOR-NAME	It's a string which contains the color name.  For example:
		\"yellow\".

   LIST		It's a list of RGB values, that is a list of three real values
		of the form:

		  (RED GREEN BLUE)

		Where RED, GREEN and BLUE are reals between 0.0 (no color) and
		1.0 (full color).

Any other value is ignored and black color will be used.

This variable is used only when `ps-print-color-p' (which see) is neither nil
nor black-white."
  :type '(choice :menu-tag "Default Foreground Gray/Color"
		 :tag "Default Foreground Gray/Color"
		 (const :tag "Session Foreground" t)
		 (const :tag "Frame Foreground" frame-parameter)
		 (number :tag "Gray Scale" :value 0.0)
		 (string :tag "Color Name" :value "black")
		 (list :tag "RGB Color" :value (0.0 0.0 0.0)
		       (number :tag "Red")
		       (number :tag "Green")
		       (number :tag "Blue")))
  :version "20"
  :group 'ps-print-color)

(defcustom ps-default-bg nil
  "RGB values of the default background color.

The `ps-default-bg' variable contains the default background color used by
ps-print, that is, if there is a face in a text that doesn't have a background
color, the `ps-default-bg' color should be used.

Valid values are:

   t		The background color of Emacs session will be used.

   frame-parameter	The background-color frame parameter will be used.

   NUMBER	It's a real value between 0.0 (black) and 1.0 (white) that
		indicate the gray color.

   COLOR-NAME	It's a string which contains the color name.  For example:
		\"yellow\".

   LIST		It's a list of RGB values, that is a list of three real values
		of the form:

		  (RED GREEN BLUE)

		Where RED, GREEN and BLUE are reals between 0.0 (no color) and
		1.0 (full color).

Any other value is ignored and white color will be used.

This variable is used only when `ps-print-color-p' (which see) is neither nil
nor black-white.

See also `ps-use-face-background'."
  :type '(choice :menu-tag "Default Background Gray/Color"
		 :tag "Default Background Gray/Color"
		 (const :tag "Session Background" t)
		 (const :tag "Frame Background" frame-parameter)
		 (number :tag "Gray Scale" :value 1.0)
		 (string :tag "Color Name" :value "white")
		 (list :tag "RGB Color" :value (1.0 1.0 1.0)
		       (number :tag "Red")
		       (number :tag "Green")
		       (number :tag "Blue")))
  :version "20"
  :group 'ps-print-color)

(defcustom ps-fg-list nil
  "Specify foreground color list.

This list is used to chose a text foreground color which is different than the
background color.  It'll be used the first foreground color in `ps-fg-list'
which is different from the background color.

If this list is nil, the default foreground color is used.  See
`ps-default-fg'.

The list element valid values are:

   NUMBER	It's a real value between 0.0 (black) and 1.0 (white) that
		indicate the gray color.

   COLOR-NAME	It's a string which contains the color name.  For example:
		\"yellow\".

   LIST		It's a list of RGB values, that is a list of three real values
		of the form:

		  (RED GREEN BLUE)

		Where RED, GREEN and BLUE are reals between 0.0 (no color) and
		1.0 (full color).

Any other value is ignored and black color will be used.

This variable is used only when `ps-fg-validate-p' (which see) is non-nil and
when `ps-print-color-p' (which see) is neither nil nor black-white."
  :type '(repeat
	  (choice :menu-tag "Foreground Gray/Color"
		  :tag "Foreground Gray/Color"
		  (number :tag "Gray Scale" :value 0.0)
		  (string :tag "Color Name" :value "black")
		  (list :tag "RGB Color" :value (0.0 0.0 0.0)
			(number :tag "Red")
			(number :tag "Green")
			(number :tag "Blue"))))
  :version "22"
  :group 'ps-print-color)

(defcustom ps-fg-validate-p t
  "Non-nil means validate if foreground color is different than background.

If text foreground and background colors are equals, no text will appear.

See also `ps-fg-list'."
  :type 'boolean
  :version "22"
  :group 'ps-print-color)

(defcustom ps-auto-font-detect t
  "Non-nil means automatically detect bold/italic/underline face attributes.
If nil, we rely solely on the lists `ps-bold-faces', `ps-italic-faces', and
`ps-underlined-faces'."
  :type 'boolean
  :version "20"
  :group 'ps-print-font)

(defcustom ps-black-white-faces
  '((font-lock-builtin-face       "black"  nil bold       )
    (font-lock-comment-face       "gray20" nil      italic)
    (font-lock-constant-face      "black"  nil bold       )
    (font-lock-function-name-face "black"  nil bold       )
    (font-lock-keyword-face       "black"  nil bold       )
    (font-lock-string-face        "black"  nil      italic)
    (font-lock-type-face          "black"  nil      italic)
    (font-lock-variable-name-face "black"  nil bold italic)
    (font-lock-warning-face       "black"  nil bold italic))
  "Specify list of face attributes to print colors on black/white printers.

The list elements are the same as defined on `ps-extend-face' (which see).

This variable is used only when `ps-print-color-p' is set to `black-white'."
  :version "21.1"
  :type '(repeat
	  (list :tag "Face Specification"
		(face :tag "Face Symbol")
		(choice :menu-tag "Foreground Color"
			:tag "Foreground Color"
			(const :tag "Black" nil)
			(string :tag "Color Name"))
		(choice :menu-tag "Background Color"
			:tag "Background Color"
			(const :tag "None" nil)
			(string :tag "Color Name"))
		(repeat :inline t
			(choice :menu-tag "Attribute"
				(const bold)
				(const italic)
				(const underline)
				(const strikeout)
				(const overline)
				(const shadow)
				(const box)
				(const outline)))))
  :version "20"
  :group 'ps-print-face)

(defcustom ps-bold-faces
  (unless ps-print-color-p
    '(font-lock-function-name-face
      font-lock-builtin-face
      font-lock-variable-name-face
      font-lock-keyword-face
      font-lock-warning-face))
  "A list of the \(non-bold\) faces that should be printed in bold font.
This applies to generating PostScript."
  :type '(repeat face)
  :version "20"
  :group 'ps-print-face)

(defcustom ps-italic-faces
  (unless ps-print-color-p
    '(font-lock-variable-name-face
      font-lock-type-face
      font-lock-string-face
      font-lock-comment-face
      font-lock-warning-face))
  "A list of the \(non-italic\) faces that should be printed in italic font.
This applies to generating PostScript."
  :type '(repeat face)
  :version "20"
  :group 'ps-print-face)

(defcustom ps-underlined-faces
  (unless ps-print-color-p
    '(font-lock-function-name-face
      font-lock-constant-face
      font-lock-warning-face))
  "A list of the \(non-underlined\) faces that should be printed underlined.
This applies to generating PostScript."
  :type '(repeat face)
  :version "20"
  :group 'ps-print-face)

(defcustom ps-use-face-background nil
  "Specify if face background should be used.

Valid values are:

   t		always use face background color.
   nil		never use face background color.
   (face...)	list of faces whose background color will be used.

Any other value will be treated as t."
  :type '(choice :menu-tag "Use Face Background"
		 :tag "Use Face Background"
		 (const :tag "Always Use Face Background" t)
		 (const :tag "Never Use Face Background" nil)
		 (repeat :menu-tag "Face Background List"
			 :tag "Face Background List"
			 face))
  :version "20"
  :group 'ps-print-face)

(defcustom ps-left-header
  (list 'ps-get-buffer-name 'ps-header-dirpart)
  "The items to display (each on a line) on the left part of the page header.
This applies to generating PostScript.

The value should be a list of strings and symbols, each representing an entry
in the PostScript array HeaderLinesLeft.

Strings are inserted unchanged into the array; those representing
PostScript string literals should be delimited with PostScript string
delimiters '(' and ')'.

For symbols with bound functions, the function is called and should return a
string to be inserted into the array.  For symbols with bound values, the value
should be a string to be inserted into the array.  In either case, function or
variable, the string value has PostScript string delimiters added to it.

If symbols are unbounded, they are silently ignored."
  :type '(repeat (choice :menu-tag "Left Header"
			 :tag "Left Header"
			 string symbol))
  :version "20"
  :group 'ps-print-headers)

(defcustom ps-right-header
  (list "/pagenumberstring load"
	'ps-time-stamp-locale-default 'ps-time-stamp-hh:mm:ss)
  "The items to display (each on a line) on the right part of the page header.
This applies to generating PostScript.

See the variable `ps-left-header' for a description of the format of this
variable.

There are the following basic functions implemented:

   `ps-time-stamp-locale-default'	Return the locale's \"preferred\" date
					as, for example, \"06/18/01\".

   `ps-time-stamp-hh:mm:ss'		Return time as \"17:28:31\".

   `ps-time-stamp-mon-dd-yyyy'		Return date as \"Jun 18 2001\".

   `ps-time-stamp-yyyy-mm-dd'		Return date as \"2001-06-18\" (ISO
					date).

   `ps-time-stamp-iso8601'		Alias for `ps-time-stamp-yyyy-mm-dd'.

You can also create your own time stamp function by using `format-time-string'
\(which see)."
  :type '(repeat (choice :menu-tag "Right Header"
			 :tag "Right Header"
			 string symbol))
  :version "20"
  :group 'ps-print-headers)

(defcustom ps-left-footer
  (list 'ps-get-buffer-name 'ps-header-dirpart)
  "The items to display (each on a line) on the left part of the page footer.
This applies to generating PostScript.

The value should be a list of strings and symbols, each representing an entry
in the PostScript array FooterLinesLeft.

Strings are inserted unchanged into the array; those representing PostScript
string literals should be delimited with PostScript string delimiters '(' and
')'.

For symbols with bound functions, the function is called and should return a
string to be inserted into the array.  For symbols with bound values, the value
should be a string to be inserted into the array.  In either case, function or
variable, the string value has PostScript string delimiters added to it.

If symbols are unbounded, they are silently ignored."
  :type '(repeat (choice :menu-tag "Left Footer"
			 :tag "Left Footer"
			 string symbol))
  :version "21.1"
  :group 'ps-print-headers)

(defcustom ps-right-footer
  (list "/pagenumberstring load"
	'ps-time-stamp-locale-default 'ps-time-stamp-hh:mm:ss)
  "The items to display (each on a line) on the right part of the page footer.
This applies to generating PostScript.

See the variable `ps-left-footer' for a description of the format of this
variable.

There are the following basic functions implemented:

   `ps-time-stamp-locale-default'	Return the locale's \"preferred\" date
					as, for example, \"06/18/01\".

   `ps-time-stamp-hh:mm:ss'		Return time as \"17:28:31\".

   `ps-time-stamp-mon-dd-yyyy'		Return date as \"Jun 18 2001\".

   `ps-time-stamp-yyyy-mm-dd'		Return date as \"2001-06-18\" (ISO
					date).

   `ps-time-stamp-iso8601'		Alias for `ps-time-stamp-yyyy-mm-dd'.

You can also create your own time stamp function by using `format-time-string'
\(which see)."
  :type '(repeat (choice :menu-tag "Right Footer"
			 :tag "Right Footer"
			 string symbol))
  :version "21.1"
  :group 'ps-print-headers)

(defcustom ps-razzle-dazzle t
  "Non-nil means report progress while formatting buffer."
  :type 'boolean
  :version "20"
  :group 'ps-print-miscellany)

(defcustom ps-adobe-tag "%!PS-Adobe-3.0\n"
  "Contains the header line identifying the output as PostScript.
By default, `ps-adobe-tag' contains the standard identifier.  Some printers
require slightly different versions of this line."
  :type 'string
  :version "20"
  :group 'ps-print-miscellany)

(defcustom ps-build-face-reference t
  "Non-nil means build the reference face lists.

ps-print sets this value to nil after it builds its internal reference lists of
bold and italic faces.  By setting its value back to t, you can force ps-print
to rebuild the lists the next time you invoke one of the ...-with-faces
commands.

You should set this value back to t after you change the attributes of any
face, or create new faces.  Most users shouldn't have to worry about its
setting, though."
  :type 'boolean
  :version "20"
  :group 'ps-print-face)

(defcustom ps-always-build-face-reference nil
  "Non-nil means always rebuild the reference face lists.

If this variable is non-nil, ps-print will rebuild its internal reference lists
of bold and italic faces *every* time one of the ...-with-faces commands is
called.  Most users shouldn't need to set this variable."
  :type 'boolean
  :version "20"
  :group 'ps-print-face)

(defcustom ps-banner-page-when-duplexing nil
  "Non-nil means the very first page is skipped.
It's like the very first character of buffer (or region) is ^L (\\014)."
  :type 'boolean
  :version "20"
  :group 'ps-print-headers)

(defcustom ps-postscript-code-directory
  (or (if (featurep 'xemacs)
	  (cond ((fboundp 'locate-data-directory) ; XEmacs
		 (funcall 'locate-data-directory "ps-print"))
		((boundp 'data-directory) ; XEmacs
		 (symbol-value 'data-directory))
		(t			; don't know what to do
		 nil))
	data-directory)			; Emacs
      (error "`ps-postscript-code-directory' isn't set properly"))
  "Directory where it's located the PostScript prologue file used by ps-print.
By default, this directory is the same as in the variable `data-directory'."
  :type 'directory
  :version "20"
  :group 'ps-print-miscellany)

(defcustom ps-line-spacing 0
  "Specify line spacing, in points, for ordinary text.

Either a float or a cons of floats (LANDSCAPE-SIZE . PORTRAIT-SIZE).

See also `ps-paragraph-spacing' and `ps-paragraph-regexp'.

To get all lines with some spacing set both `ps-line-spacing' and
`ps-paragraph-spacing' variables."
  :type '(choice :menu-tag "Line Spacing For Ordinary Text"
		 :tag "Line Spacing For Ordinary Text"
		 (number :tag "Line Spacing")
		 (cons :tag "Landscape/Portrait"
		       (number :tag "Landscape Line Spacing")
		       (number :tag "Portrait Line Spacing")))
  :version "21.1"
  :group 'ps-print-miscellany)

(defcustom ps-paragraph-spacing 0
  "Specify paragraph spacing, in points, for ordinary text.

Either a float or a cons of floats (LANDSCAPE-SIZE . PORTRAIT-SIZE).

See also `ps-line-spacing' and `ps-paragraph-regexp'.

To get all lines with some spacing set both `ps-line-spacing' and
`ps-paragraph-spacing' variables."
  :type '(choice :menu-tag "Paragraph Spacing For Ordinary Text"
		 :tag "Paragraph Spacing For Ordinary Text"
		 (number :tag "Paragraph Spacing")
		 (cons :tag "Landscape/Portrait"
		       (number :tag "Landscape Paragraph Spacing")
		       (number :tag "Portrait Paragraph Spacing")))
  :version "21.1"
  :group 'ps-print-miscellany)

(defcustom ps-paragraph-regexp "[ \t]*$"
  "Specify paragraph delimiter.

It should be a regexp or nil.

See also `ps-paragraph-spacing'."
  :type '(choice :menu-tag "Paragraph Delimiter"
		 (const :tag "No Delimiter" nil)
		 (regexp :tag "Delimiter Regexp"))
  :version "21.1"
  :group 'ps-print-miscellany)

(defcustom ps-begin-cut-regexp nil
  "Specify regexp which is start of a region to cut out when printing.

As an example, variables `ps-begin-cut-regexp' and `ps-end-cut-regexp' may be
set to \"^Local Variables:\" and \"^End:\", respectively, in order to leave out
some special printing instructions from the actual print.  Special printing
instructions may be appended to the end of the file just like any other
buffer-local variables.  See section \"Local Variables in Files\" on Emacs
manual for more information.

Variables `ps-begin-cut-regexp' and `ps-end-cut-regexp' control together what
actually gets printed.  Both variables may be set to nil in which case no
cutting occurs."
  :type '(choice (const :tag "No Delimiter" nil)
		 (regexp :tag "Delimiter Regexp"))
  :version "21.1"
  :group 'ps-print-miscellany)

(defcustom ps-end-cut-regexp nil
  "Specify regexp which is end of the region to cut out when printing.

See `ps-begin-cut-regexp' for more information."
  :type '(choice (const :tag "No Delimiter" nil)
		 (regexp :tag "Delimiter Regexp"))
  :version "21.1"
  :group 'ps-print-miscellany)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Selected Pages


(defvar ps-last-selected-pages nil
  "Latest `ps-selected-pages' value.")


(defun ps-restore-selected-pages ()
  "Restore latest `ps-selected-pages' value."
  (interactive)
  (setq ps-selected-pages ps-last-selected-pages))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization


;;;###autoload
(defun ps-print-customize ()
  "Customization of ps-print group."
  (interactive)
  (customize-group 'ps-print))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User commands


;;;###autoload
(defun ps-print-buffer (&optional filename)
  "Generate and print a PostScript image of the buffer.

Interactively, when you use a prefix argument (\\[universal-argument]), the command prompts the
user for a file name, and saves the PostScript image in that file instead of
sending it to the printer.

Noninteractively, the argument FILENAME is treated as follows: if it is nil,
send the image to the printer.  If FILENAME is a string, save the PostScript
image in a file with that name."
  (interactive (list (ps-print-preprint current-prefix-arg)))
  (ps-print-without-faces (point-min) (point-max) filename))


;;;###autoload
(defun ps-print-buffer-with-faces (&optional filename)
  "Generate and print a PostScript image of the buffer.
Like `ps-print-buffer', but includes font, color, and underline information in
the generated image.  This command works only if you are using a window system,
so it has a way to determine color values."
  (interactive (list (ps-print-preprint current-prefix-arg)))
  (ps-print-with-faces (point-min) (point-max) filename))


;;;###autoload
(defun ps-print-region (from to &optional filename)
  "Generate and print a PostScript image of the region.
Like `ps-print-buffer', but prints just the current region."
  (interactive (ps-print-preprint-region current-prefix-arg))
  (ps-print-without-faces from to filename t))


;;;###autoload
(defun ps-print-region-with-faces (from to &optional filename)
  "Generate and print a PostScript image of the region.
Like `ps-print-region', but includes font, color, and underline information in
the generated image.  This command works only if you are using a window system,
so it has a way to determine color values."
  (interactive (ps-print-preprint-region current-prefix-arg))
  (ps-print-with-faces from to filename t))


;;;###autoload
(defun ps-spool-buffer ()
  "Generate and spool a PostScript image of the buffer.
Like `ps-print-buffer' except that the PostScript image is saved in a local
buffer to be sent to the printer later.

Use the command `ps-despool' to send the spooled images to the printer."
  (interactive)
  (ps-spool-without-faces (point-min) (point-max)))


;;;###autoload
(defun ps-spool-buffer-with-faces ()
  "Generate and spool a PostScript image of the buffer.
Like `ps-spool-buffer', but includes font, color, and underline information in
the generated image.  This command works only if you are using a window system,
so it has a way to determine color values.

Use the command `ps-despool' to send the spooled images to the printer."
  (interactive)
  (ps-spool-with-faces (point-min) (point-max)))


;;;###autoload
(defun ps-spool-region (from to)
  "Generate a PostScript image of the region and spool locally.
Like `ps-spool-buffer', but spools just the current region.

Use the command `ps-despool' to send the spooled images to the printer."
  (interactive "r")
  (ps-spool-without-faces from to t))


;;;###autoload
(defun ps-spool-region-with-faces (from to)
  "Generate a PostScript image of the region and spool locally.
Like `ps-spool-region', but includes font, color, and underline information in
the generated image.  This command works only if you are using a window system,
so it has a way to determine color values.

Use the command `ps-despool' to send the spooled images to the printer."
  (interactive "r")
  (ps-spool-with-faces from to t))

;;;###autoload
(defun ps-despool (&optional filename)
  "Send the spooled PostScript to the printer.

Interactively, when you use a prefix argument (\\[universal-argument]), the command prompts the
user for a file name, and saves the spooled PostScript image in that file
instead of sending it to the printer.

Noninteractively, the argument FILENAME is treated as follows: if it is nil,
send the image to the printer.  If FILENAME is a string, save the PostScript
image in a file with that name."
  (interactive (list (ps-print-preprint current-prefix-arg)))
  (ps-do-despool filename))

;;;###autoload
(defun ps-line-lengths ()
  "Display the correspondence between a line length and a font size.
Done using the current ps-print setup.
Try: pr -t file | awk '{printf \"%3d %s\n\", length($0), $0}' | sort -r | head"
  (interactive)
  (ps-line-lengths-internal))

;;;###autoload
(defun ps-nb-pages-buffer (nb-lines)
  "Display number of pages to print this buffer, for various font heights.
The table depends on the current ps-print setup."
  (interactive (ps-count-lines-preprint (point-min) (point-max)))
  (ps-nb-pages nb-lines))

;;;###autoload
(defun ps-nb-pages-region (nb-lines)
  "Display number of pages to print the region, for various font heights.
The table depends on the current ps-print setup."
  (interactive (ps-count-lines-preprint (mark) (point)))
  (ps-nb-pages nb-lines))

(defvar ps-prefix-quote nil
  "Used for `ps-print-quote' (which see).")

;;;###autoload
(defun ps-setup ()
  "Return the current PostScript-generation setup."
  (let (ps-prefix-quote)
    (mapconcat
     #'ps-print-quote
     (list
      (concat "\n;;; (" (if (featurep 'xemacs) "XEmacs" "Emacs")
	      ") ps-print version " ps-print-version "\n")
      ";; internal vars"
      (ps-comment-string "emacs-version     " emacs-version)
      (ps-comment-string "ps-windows-system " ps-windows-system)
      (ps-comment-string "ps-lp-system      " ps-lp-system)
      nil
      '(25 . ps-print-color-p)
      '(25 . ps-lpr-command)
      '(25 . ps-lpr-switches)
      '(25 . ps-printer-name)
      '(25 . ps-printer-name-option)
      '(25 . ps-print-region-function)
      '(25 . ps-manual-feed)
      '(25 . ps-end-with-control-d)
      nil
      '(23 . ps-paper-type)
      '(23 . ps-warn-paper-type)
      '(23 . ps-landscape-mode)
      '(23 . ps-print-upside-down)
      '(23 . ps-number-of-columns)
      nil
      '(23 . ps-zebra-stripes)
      '(23 . ps-zebra-stripe-height)
      '(23 . ps-zebra-stripe-follow)
      '(23 . ps-zebra-color)
      '(23 . ps-line-number)
      '(23 . ps-line-number-step)
      '(23 . ps-line-number-start)
      nil
      '(17 . ps-razzle-dazzle)
      '(17 . ps-default-bg)
      '(17 . ps-default-fg)
      '(17 . ps-fg-validate-p)
      '(17 . ps-fg-list)
      nil
      '(23 . ps-use-face-background)
      nil
      '(28 . ps-print-control-characters)
      nil
      '(26 . ps-print-background-image)
      nil
      '(25 . ps-print-background-text)
      nil
      '(29 . ps-error-handler-message)
      '(29 . ps-user-defined-prologue)
      '(29 . ps-print-prologue-header)
      '(29 . ps-postscript-code-directory)
      '(29 . ps-adobe-tag)
      nil
      '(30 . ps-left-margin)
      '(30 . ps-right-margin)
      '(30 . ps-inter-column)
      '(30 . ps-bottom-margin)
      '(30 . ps-top-margin)
      '(30 . ps-print-only-one-header)
      '(30 . ps-switch-header)
      '(30 . ps-print-header)
      '(30 . ps-header-lines)
      '(30 . ps-header-offset)
      '(30 . ps-header-line-pad)
      '(30 . ps-print-header-frame)
      '(30 . ps-header-frame-alist)
      '(30 . ps-print-footer)
      '(30 . ps-footer-lines)
      '(30 . ps-footer-offset)
      '(30 . ps-footer-line-pad)
      '(30 . ps-print-footer-frame)
      '(30 . ps-footer-frame-alist)
      '(30 . ps-show-n-of-n)
      '(30 . ps-spool-config)
      '(30 . ps-spool-duplex)
      '(30 . ps-spool-tumble)
      '(30 . ps-banner-page-when-duplexing)
      '(30 . ps-left-header)
      '(30 . ps-right-header)
      '(30 . ps-left-footer)
      '(30 . ps-right-footer)
      nil
      '(23 . ps-n-up-printing)
      '(23 . ps-n-up-margin)
      '(23 . ps-n-up-border-p)
      '(23 . ps-n-up-filling)
      nil
      '(26 . ps-multibyte-buffer)
      '(26 . ps-font-family)
      '(26 . ps-font-size)
      '(26 . ps-header-font-family)
      '(26 . ps-header-font-size)
      '(26 . ps-header-title-font-size)
      '(26 . ps-footer-font-family)
      '(26 . ps-footer-font-size)
      '(26 . ps-line-number-color)
      '(26 . ps-line-number-font)
      '(26 . ps-line-number-font-size)
      '(26 . ps-line-spacing)
      '(26 . ps-paragraph-spacing)
      '(26 . ps-paragraph-regexp)
      '(26 . ps-begin-cut-regexp)
      '(26 . ps-end-cut-regexp)
      nil
      '(23 . ps-even-or-odd-pages)
      '(23 . ps-selected-pages)
      '(23 . ps-last-selected-pages)
      nil
      '(31 . ps-build-face-reference)
      '(31 . ps-always-build-face-reference)
      nil
      '(20 . ps-auto-font-detect)
      '(20 . ps-bold-faces)
      '(20 . ps-italic-faces)
      '(20 . ps-underlined-faces)
      '(20 . ps-black-white-faces)
      "      )\n
\;; The following customized variables have long lists and are seldom modified:
\;;    ps-page-dimensions-database
\;;    ps-font-info-database

\;;; ps-print - end of settings\n")
     "\n")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions and variables:


(defun ps-print-quote (elt)
  "Quote ELT for printing (used for showing settings).

If ELT is nil, return an empty string.
If ELT is string, return it.
Otherwise, ELT should be a cons (LEN . SYM) where SYM is a variable symbol and
LEN is the field length where SYM name will be inserted.  The variable
`ps-prefix-quote' is used to form the string, if `ps-prefix-quote' is nil, it's
used \"(setq \" as prefix; otherwise, it's used \"      \".  So, the string
generated is:

   * If `ps-prefix-quote' is nil:
      \"(setq SYM-NAME   SYM-VALUE\"
	     |<------->|
		 LEN

   * If `ps-prefix-quote' is non-nil:
      \"      SYM-NAME   SYM-VALUE\"
	     |<------->|
		 LEN

If `ps-prefix-quote' is nil, it's set to t after generating string."
  (cond
   ((stringp elt) elt)
   ((and (consp elt) (integerp (car elt))
	 (symbolp (cdr elt)) (boundp (cdr elt)))
    (let* ((col (car elt))
	   (sym (cdr elt))
	   (key (symbol-name sym))
	   (len (length key))
	   (val (symbol-value sym)))
      (concat (if ps-prefix-quote
		  "      "
		(setq ps-prefix-quote t)
		"(setq ")
	      key
	      (if (> col len)
		  (make-string (- col len) ?\s)
		" ")
	      (ps-value-string val))))
   (t "")
   ))


(defun ps-value-string (val)
  "Return a string representation of VAL.  Used by `ps-print-quote'."
  (cond ((null val)
	 "nil")
	((eq val t)
	 "t")
	((or (symbolp val) (listp val))
	 (format "'%S" val))
	(t
	 (format "%S" val))))


(defun ps-comment-string (str value)
  "Return a comment string like \";; STR = VALUE\"."
  (format ";; %s = %s" str (ps-value-string value)))


(defun ps-value (alist-sym key)
  "Return value from association list ALIST-SYM which car is `eq' to KEY."
  (cdr (assq key (symbol-value alist-sym))))


(defun ps-get (alist-sym key)
  "Return element from association list ALIST-SYM which car is `eq' to KEY."
  (assq key (symbol-value alist-sym)))


(defun ps-put (alist-sym key value)
  "Store element (KEY . VALUE) into association list ALIST-SYM.
If KEY already exists in ALIST-SYM, modify cdr to VALUE.
It can be retrieved with `(ps-get ALIST-SYM KEY)'."
  (let ((elt: (assq key (symbol-value alist-sym)))) ; to avoid name conflict
    (if elt:
	(setcdr elt: value)
      (setq elt: (cons key value))
      (set alist-sym (cons elt: (symbol-value alist-sym))))
    elt:))


(defun ps-del (alist-sym key)
  "Delete by side effect element KEY from association list ALIST-SYM."
  (let ((a:list: (symbol-value alist-sym)) ; to avoid name conflict
	old)
    (while a:list:
      (if (eq key (car (car a:list:)))
	  (progn
	    (if old
		(setcdr old (cdr a:list:))
	      (set alist-sym (cdr a:list:)))
	    (setq a:list: nil))
	(setq old     a:list:
	      a:list: (cdr a:list:)))))
  (symbol-value alist-sym))


(defun ps-time-stamp-locale-default ()
  "Return the locale's \"preferred\" date as, for example, \"06/18/01\"."
  (format-time-string "%x"))


(defun ps-time-stamp-mon-dd-yyyy ()
  "Return date as \"Jun 18 2001\"."
  (format-time-string "%b %d %Y"))


(defun ps-time-stamp-yyyy-mm-dd ()
  "Return date as \"2001-06-18\" (ISO date)."
  (format-time-string "%Y-%m-%d"))


;; Alias for `ps-time-stamp-yyyy-mm-dd' (which see).
(defalias 'ps-time-stamp-iso8601 'ps-time-stamp-yyyy-mm-dd)


(defun ps-time-stamp-hh:mm:ss ()
  "Return time as \"17:28:31\"."
  (format-time-string "%T"))


(defvar ps-print-color-scale 1.0)

(defun ps-color-scale (color)
  ;; Scale 16-bit X-COLOR-VALUE to PostScript color value in [0, 1] interval.
  (mapcar #'(lambda (value) (/ value ps-print-color-scale))
	  (ps-color-values color)))


(defun ps-face-underlined-p (face)
  (or (face-underline-p face)
      (memq face ps-underlined-faces)))


(defun ps-prologue-file (filenumber)
  "If prologue FILENUMBER exists and is readable, return contents as string.

Note: No major/minor-mode is activated and no local variables are evaluated for
      FILENUMBER, but proper EOL-conversion and character interpretation is
      done!"
  (let ((filename (convert-standard-filename
		   (expand-file-name (format "ps-prin%d.ps" filenumber)
				     ps-postscript-code-directory))))
    (if (and (file-exists-p filename)
	     (file-readable-p filename))
	(with-temp-buffer
	  (insert-file-contents filename)
	  (buffer-string))
      (error "ps-print PostScript prologue `%s' file was not found"
	     filename))))


(defvar ps-mark-code-directory nil)

(defvar ps-print-prologue-0 ""
  "ps-print PostScript error handler.")

(defvar ps-print-prologue-1 ""
  "ps-print PostScript prologue.")

;; Start Editing Here:

(defvar ps-source-buffer nil)
(defvar ps-spool-buffer-name "*PostScript*")
(defvar ps-spool-buffer nil)

(defvar ps-output-head nil)
(defvar ps-output-tail nil)

(defvar ps-page-postscript 0)		; page number
(defvar ps-page-order 0)		; PostScript page counter
(defvar ps-page-sheet 0)		; sheet counter
(defvar ps-page-column 0)		; column counter
(defvar ps-page-printed 0)		; total pages printed
(defvar ps-page-n-up 0)			; n-up counter
(defvar ps-lines-printed 0)		; total lines printed
(defvar ps-showline-count 1)		; line number counter
(defvar ps-first-page nil)
(defvar ps-last-page nil)
(defvar ps-print-page-p t)

(defvar ps-control-or-escape-regexp nil)
(defvar ps-n-up-on nil)

(defvar ps-background-pages nil)
(defvar ps-background-all-pages nil)
(defvar ps-background-text-count 0)
(defvar ps-background-image-count 0)

(defvar ps-current-font 0)
(defvar ps-default-foreground nil)
(defvar ps-default-background nil)
(defvar ps-default-color nil)
(defvar ps-current-color nil)
(defvar ps-current-bg nil)
(defvar ps-foreground-list nil)

(defvar ps-zebra-stripe-full-p nil)
(defvar ps-razchunk 0)

(defvar ps-color-p nil)

;; These values determine how much print-height to deduct when headers/footers
;; are turned on.  This is a pretty clumsy way of handling it, but it'll do for
;; now.

(defvar ps-header-pad 0
  "Vertical and horizontal space between the header frame and the text.
This is in units of points (1/72 inch).")

(defvar ps-footer-pad 0
  "Vertical and horizontal space between the footer frame and the text.
This is in units of points (1/72 inch).")

;; Define accessors to the dimensions list.

(defmacro ps-page-dimensions-get-width  (dims) `(nth 0 ,dims))
(defmacro ps-page-dimensions-get-height (dims) `(nth 1 ,dims))
(defmacro ps-page-dimensions-get-media  (dims) `(nth 2 ,dims))

(defvar ps-landscape-page-height nil)

(defvar ps-print-width nil)
(defvar ps-print-height nil)

(defvar ps-height-remaining nil)
(defvar ps-width-remaining nil)

(defvar ps-font-size-internal nil)
(defvar ps-header-font-size-internal nil)
(defvar ps-header-title-font-size-internal nil)
(defvar ps-footer-font-size-internal nil)
(defvar ps-line-spacing-internal nil)
(defvar ps-paragraph-spacing-internal nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal Variables


(defvar ps-black-white-faces-alist nil
  "Alist of symbolic faces used for black/white PostScript printers.
An element of this list has the same form as `ps-print-face-extension-alist'
\(which see).

Don't change this list directly; instead,
use `ps-extend-face' and `ps-extend-face-list'.
See documentation for `ps-extend-face' for valid extension symbol.
See also documentation for `ps-print-color-p'.")


(defvar ps-print-face-extension-alist nil
  "Alist of symbolic faces *WITH* extension features (box, outline, etc).
An element of this list has the following form:

   (FACE . [BITS FG BG])

   FACE is a symbol denoting a face name
   BITS is a bit vector, where each bit correspond
      to a feature (bold, underline, etc)
      (see documentation for `ps-print-face-map-alist')
   FG foreground color (string or nil)
   BG background color (string or nil)

Don't change this list directly; instead,
use `ps-extend-face' and `ps-extend-face-list'.
See documentation for `ps-extend-face' for valid extension symbol.")


(defvar ps-print-face-alist nil
  "Alist of symbolic faces *WITHOUT* extension features (box, outline, etc).

An element of this list has the same form as an element of
`ps-print-face-extension-alist'.

Don't change this list directly; this list is used by `ps-face-attributes',
`ps-map-face' and `ps-build-reference-face-lists'.")


(defconst ps-print-face-map-alist
  '((bold        . 1)
    (italic      . 2)
    (underline   . 4)
    (strikeout   . 8)
    (overline    . 16)
    (shadow      . 32)
    (box         . 64)
    (outline     . 128))
  "Alist of all features and the corresponding bit mask.
Each symbol correspond to one bit in a bit vector.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remapping Faces


;;;###autoload
(defun ps-extend-face-list (face-extension-list &optional merge-p alist-sym)
  "Extend face in ALIST-SYM.

If optional MERGE-P is non-nil, extensions in FACE-EXTENSION-LIST are merged
with face extension in ALIST-SYM; otherwise, overrides.

If optional ALIST-SYM is nil, `ps-print-face-extension-alist' is used;
otherwise, it should be an alist symbol.

The elements in FACE-EXTENSION-LIST are like those for `ps-extend-face'.

See `ps-extend-face' for documentation."
  (while face-extension-list
    (ps-extend-face (car face-extension-list) merge-p alist-sym)
    (setq face-extension-list (cdr face-extension-list))))


;;;###autoload
(defun ps-extend-face (face-extension &optional merge-p alist-sym)
  "Extend face in ALIST-SYM.

If optional MERGE-P is non-nil, extensions in FACE-EXTENSION list are merged
with face extensions in ALIST-SYM; otherwise, overrides.

If optional ALIST-SYM is nil, `ps-print-face-extension-alist' is used;
otherwise, it should be an alist symbol.

The elements of FACE-EXTENSION list have the form:

   (FACE-NAME FOREGROUND BACKGROUND EXTENSION...)

FACE-NAME is a face name symbol.

FOREGROUND and BACKGROUND may be nil or a string that denotes the
foreground and background colors respectively.

EXTENSION is one of the following symbols:
   bold      - use bold font.
   italic    - use italic font.
   underline - put a line under text.
   strikeout - like underline, but the line is in middle of text.
   overline  - like underline, but the line is over the text.
   shadow    - text will have a shadow.
   box       - text will be surrounded by a box.
   outline   - print characters as hollow outlines.

If EXTENSION is any other symbol, it is ignored."
  (or alist-sym
      (setq alist-sym 'ps-print-face-extension-alist))
  (let* ((background  (nth 2 face-extension))
	 (foreground  (nth 1 face-extension))
	 (face-name   (nth 0 face-extension))
	 (ps-face     (cdr (assq face-name (symbol-value alist-sym))))
	 (face-vector (or ps-face (vector 0 nil nil)))
	 (face-bit    (ps-extension-bit face-extension)))
    ;; extend face
    (aset face-vector 0 (if merge-p
			    (logior (aref face-vector 0) face-bit)
			  face-bit))
    (and (or (not merge-p) (and foreground (stringp foreground)))
	 (aset face-vector 1 foreground))
    (and (or (not merge-p) (and background (stringp background)))
	 (aset face-vector 2 background))
    ;; if face does not exist, insert it
    (or ps-face
	(set alist-sym (cons (cons face-name face-vector)
			     (symbol-value alist-sym))))))


(defun ps-extension-bit (face-extension)
  (let ((face-bit 0))
    ;; map valid symbol extension to bit vector
    (setq face-extension (cdr (cdr face-extension)))
    (while (setq face-extension (cdr face-extension))
      (setq face-bit (logior face-bit
			     (or (cdr (assq (car face-extension)
					    ps-print-face-map-alist))
				 0))))
    face-bit))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adapted from font-lock: (obsolete stuff)
;; Originally face attributes were specified via `font-lock-face-attributes'.
;; Users then changed the default face attributes by setting that variable.
;; However, we try and be back-compatible and respect its value if set except
;; for faces where M-x customize has been used to save changes for the face.


(defun ps-font-lock-face-attributes ()
  (and (boundp 'font-lock-mode) (symbol-value 'font-lock-mode)
       (boundp 'font-lock-face-attributes)
       (let ((face-attributes (symbol-value 'font-lock-face-attributes)))
	 (while face-attributes
	   (let* ((face-attribute
		   (car (prog1 face-attributes
			  (setq face-attributes (cdr face-attributes)))))
		  (face (car face-attribute)))
	     ;; Rustle up a `defface' SPEC from a
	     ;; `font-lock-face-attributes' entry.
	     (unless (get face 'saved-face)
	       (let ((foreground (nth 1 face-attribute))
		     (background (nth 2 face-attribute))
		     (bold-p (nth 3 face-attribute))
		     (italic-p (nth 4 face-attribute))
		     (underline-p (nth 5 face-attribute))
		     face-spec)
		 (when foreground
		   (setq face-spec (cons ':foreground
					 (cons foreground face-spec))))
		 (when background
		   (setq face-spec (cons ':background
					 (cons background face-spec))))
		 (when bold-p
		   (setq face-spec (append '(:weight bold) face-spec)))
		 (when italic-p
		   (setq face-spec (append '(:slant italic) face-spec)))
		 (when underline-p
		   (setq face-spec (append '(:underline t) face-spec)))
		 (custom-declare-face face (list (list t face-spec)) nil)
		 )))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal functions and variables


(defun ps-message-log-max ()
  (and (not (string= (buffer-name) "*Messages*"))
       (boundp 'message-log-max)
       message-log-max))


(defvar ps-print-hook nil)
(defvar ps-print-begin-sheet-hook nil)
(defvar ps-print-begin-page-hook nil)
(defvar ps-print-begin-column-hook nil)


(defun ps-print-without-faces (from to &optional filename region-p)
  (ps-spool-without-faces from to region-p)
  (ps-do-despool filename))


(defun ps-spool-without-faces (from to &optional region-p)
  (let ((message-log-max (ps-message-log-max)))	; to print *Messages* buffer
    (run-hooks 'ps-print-hook)
    (ps-printing-region region-p from to)
    (ps-generate (current-buffer) from to 'ps-generate-postscript)))


(defun ps-print-with-faces (from to &optional filename region-p)
  (ps-spool-with-faces from to region-p)
  (ps-do-despool filename))


(defun ps-spool-with-faces (from to &optional region-p)
  (let ((message-log-max (ps-message-log-max)))	; to print *Messages* buffer
    (run-hooks 'ps-print-hook)
    (ps-printing-region region-p from to)
    (ps-generate (current-buffer) from to 'ps-generate-postscript-with-faces)))


(defun ps-count-lines-preprint (from to)
  (or (and from to)
      (error "The mark is not set now"))
  (let ((message-log-max (ps-message-log-max)))	; to count lines of *Messages*
    (list (count-lines from to))))


(defun ps-count-lines (from to)
  (+ (count-lines from to)
     (save-excursion
       (goto-char to)
       (if (= (current-column) 0) 1 0))))


(defvar ps-printing-region nil
  "Variable used to indicate the region that ps-print is printing.
It is a cons, the car of which is the line number where the region begins, and
its cdr is the total number of lines in the buffer.  Formatting functions can
use this information to print the original line number (and not the number of
lines printed), and to indicate in the header that the printout is of a partial
file.")


(defvar ps-printing-region-p nil
  "Non-nil means ps-print is printing a region.")


(defun ps-printing-region (region-p from to)
  (setq ps-printing-region-p region-p
	ps-printing-region
	(cons (if region-p
		  (ps-count-lines (point-min) (min from to))
		1)
	      (ps-count-lines (point-min) (point-max)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal functions


(defsubst ps-font-alist (font-sym)
  (get font-sym 'fonts))

(defun ps-font (font-sym font-type)
  "Font family name for text of `font-type', when generating PostScript."
  (let* ((font-list (ps-font-alist font-sym))
	 (normal-font (cdr (assq 'normal font-list))))
    (while (and font-list (not (eq font-type (car (car font-list)))))
      (setq font-list (cdr font-list)))
    (or (cdr (car font-list)) normal-font)))

(defsubst ps-fonts (font-sym)
  (mapcar 'cdr (ps-font-alist font-sym)))

(defsubst ps-font-number (font-sym font-type)
  (or (ps-alist-position font-type (ps-font-alist font-sym))
      0))

(defsubst ps-line-height (font-sym)
  "The height of a line, for generating PostScript.
This is the value that ps-print uses to determine the height,
y-dimension, of the lines of text it has printed, and thus affects the
point at which page-breaks are placed.
The line-height is *not* the same as the point size of the font."
  (get font-sym 'line-height))

(defsubst ps-title-line-height (font-sym)
  "The height of a `title' line, for generating PostScript.
This is the value that ps-print uses to determine the height,
y-dimension, of the lines of text it has printed, and thus affects the
point at which page-breaks are placed.
The title-line-height is *not* the same as the point size of the font."
  (get font-sym 'title-line-height))

(defsubst ps-space-width (font-sym)
  "The width of a space character, for generating PostScript.
This value is used in expanding tab characters."
  (get font-sym 'space-width))

(defsubst ps-avg-char-width (font-sym)
  "The average width, in points, of a character, for generating PostScript.
This is the value that ps-print uses to determine the length,
x-dimension, of the text it has printed, and thus affects the point at
which long lines wrap around."
  (get font-sym 'avg-char-width))

(defun ps-line-lengths-internal ()
  "Display the correspondence between a line length and a font size.
Done using the current ps-print setup.
Try: pr -t file | awk '{printf \"%3d %s\n\", length($0), $0}' | sort -r | head"
  (let* ((ps-font-size-internal
	  (or ps-font-size-internal
	      (ps-get-font-size 'ps-font-size)))
	 (ps-header-font-size-internal
	  (or ps-header-font-size-internal
	      (ps-get-font-size 'ps-header-font-size)))
	 (ps-footer-font-size-internal
	  (or ps-footer-font-size-internal
	      (ps-get-font-size 'ps-footer-font-size)))
	 (ps-header-title-font-size-internal
	  (or ps-header-title-font-size-internal
	      (ps-get-font-size 'ps-header-title-font-size)))
	 (buf (get-buffer-create "*Line-lengths*"))
	 (ifs ps-font-size-internal)	; initial font size
	 (print-width (progn (ps-get-page-dimensions)
			     ps-print-width))
	 (icw (ps-avg-char-width 'ps-font-for-text)) ; initial character width
	 (ps-setup (ps-setup))		; setup for the current buffer
	 (fs-min 5)			; minimum font size
	 cw-min				; minimum character width
	 nb-cpl-max			; maximum nb of characters per line
	 (fs-max 14)			; maximum font size
	 cw-max				; maximum character width
	 nb-cpl-min			; minimum nb of characters per line
	 fs				; current font size
	 cw				; current character width
	 nb-cpl				; current nb of characters per line
	 )
    (setq cw-min     (/ (* icw fs-min) ifs)
	  nb-cpl-max (floor (/ print-width cw-min))
	  cw-max     (/ (* icw fs-max) ifs)
	  nb-cpl-min (floor (/ print-width cw-max))
	  nb-cpl     nb-cpl-min)
    (set-buffer buf)
    (goto-char (point-max))
    (or (bobp) (insert "\n" (make-string 75 ?\;) "\n"))
    (insert ps-setup
	    "\nnb char per line / font size\n")
    (while (<= nb-cpl nb-cpl-max)
      (setq cw (/ print-width (float nb-cpl))
	    fs (/ (* ifs cw) icw))
      (insert (format "%16d   %s\n" nb-cpl fs))
      (setq nb-cpl (1+ nb-cpl)))
    (insert "\n")
    (display-buffer buf 'not-this-window)))

(defun ps-nb-pages (nb-lines)
  "Display correspondence between font size and the number of pages.
The correspondence is based on having NB-LINES lines of text,
and on the current ps-print setup."
  (let* ((ps-font-size-internal
	  (or ps-font-size-internal
	      (ps-get-font-size 'ps-font-size)))
	 (ps-header-font-size-internal
	  (or ps-header-font-size-internal
	      (ps-get-font-size 'ps-header-font-size)))
	 (ps-footer-font-size-internal
	  (or ps-footer-font-size-internal
	      (ps-get-font-size 'ps-footer-font-size)))
	 (ps-header-title-font-size-internal
	  (or ps-header-title-font-size-internal
	      (ps-get-font-size 'ps-header-title-font-size)))
	 (ps-line-spacing-internal
	  (or ps-line-spacing-internal
	      (ps-get-size ps-line-spacing "line spacing")))
	 (buf (get-buffer-create "*Nb-Pages*"))
	 (ils ps-line-spacing-internal) ; initial line spacing
	 (ifs ps-font-size-internal)	; initial font size
	 (page-height (progn (ps-get-page-dimensions)
			     ps-print-height))
	 (ilh (ps-line-height 'ps-font-for-text)) ; initial line height
	 (ps-setup (ps-setup))		; setup for the current buffer
	 (fs-min 4)			; minimum font size
	 lh-min				; minimum line height
	 nb-lpp-max			; maximum nb of lines per page
	 nb-page-min			; minimum nb of pages
	 (fs-max 14)			; maximum font size
	 lh-max				; maximum line height
	 nb-lpp-min			; minimum nb of lines per page
	 nb-page-max			; maximum nb of pages
	 fs				; current font size
	 lh				; current line height
	 nb-lpp				; current nb of lines per page
	 nb-page			; current nb of pages
	 )
    (setq lh-min      (/ (- (* (+ ilh ils) fs-min) ils) ifs)
	  nb-lpp-max  (floor (/ page-height lh-min))
	  nb-page-min (ceiling (/ (float nb-lines) nb-lpp-max))
	  lh-max      (/ (- (* (+ ilh ils) fs-max) ils) ifs)
	  nb-lpp-min  (floor (/ page-height lh-max))
	  nb-page-max (ceiling (/ (float nb-lines) nb-lpp-min))
	  nb-page     nb-page-min)
    (set-buffer buf)
    (goto-char (point-max))
    (or (bobp) (insert "\n" (make-string 75 ?\;) "\n"))
    (insert ps-setup
	    (format "\nThere are %d lines.\n\n" nb-lines)
	    "nb page / font size\n")
    (while (<= nb-page nb-page-max)
      (setq nb-lpp (ceiling (/ nb-lines (float nb-page)))
	    lh     (/ page-height nb-lpp)
	    fs     (/ (* ifs lh) ilh))
      (insert (format "%7d   %s\n" nb-page fs))
      (setq nb-page (1+ nb-page)))
    (insert "\n")
    (display-buffer buf 'not-this-window)))

;; macros used in `ps-select-font'
(defmacro ps-lookup (key) `(cdr (assq ,key font-entry)))
(defmacro ps-size-scale (key) `(/ (* (ps-lookup ,key) font-size) size))

(defun ps-select-font (font-family sym font-size title-font-size)
  (let ((font-entry (cdr (assq font-family ps-font-info-database))))
    (or font-entry
	(error "Don't have data to scale font %s.  Known fonts families are %s"
	       font-family
	       (mapcar 'car ps-font-info-database)))
    (let ((size (ps-lookup 'size)))
      (put sym 'fonts (ps-lookup 'fonts))
      (put sym 'space-width (ps-size-scale 'space-width))
      (put sym 'avg-char-width (ps-size-scale 'avg-char-width))
      (put sym 'line-height (ps-size-scale 'line-height))
      (put sym 'title-line-height
	   (/ (* (ps-lookup 'line-height) title-font-size) size)))))

(defun ps-get-page-dimensions ()
  (let ((page-dimensions (cdr (assq ps-paper-type ps-page-dimensions-database)))
	page-width page-height)
    (cond
     ((null page-dimensions)
      (error "`ps-paper-type' must be one of:\n%s"
	     (mapcar 'car ps-page-dimensions-database)))
     ((< ps-number-of-columns 1)
      (error "The number of columns %d should be positive"
	     ps-number-of-columns)))

    (ps-select-font ps-font-family 'ps-font-for-text
		    ps-font-size-internal ps-font-size-internal)
    (ps-select-font ps-header-font-family 'ps-font-for-header
		    ps-header-font-size-internal
		    ps-header-title-font-size-internal)
    (ps-select-font ps-footer-font-family 'ps-font-for-footer
		    ps-footer-font-size-internal ps-footer-font-size-internal)

    (setq page-width  (ps-page-dimensions-get-width  page-dimensions)
	  page-height (ps-page-dimensions-get-height page-dimensions))

    ;; Landscape mode
    (if ps-landscape-mode
	;; exchange width and height
	(setq page-width (prog1 page-height (setq page-height page-width))))

    ;; It is used to get the lower right corner (only in landscape mode)
    (setq ps-landscape-page-height page-height)

    ;; | lm | text | ic | text | ic | text | rm |
    ;; page-width == lm  +  n * pw  +  (n - 1) * ic  +  rm
    ;; => pw == (page-width - lm -rm - (n - 1) * ic) / n
    (setq ps-print-width (/ (- page-width
			       ps-left-margin ps-right-margin
			       (* (1- ps-number-of-columns) ps-inter-column))
			    ps-number-of-columns))
    (if (<= ps-print-width 0)
	(error "Bad horizontal layout:
page-width           == %s
ps-left-margin       == %s
ps-right-margin      == %s
ps-inter-column      == %s
ps-number-of-columns == %s
| lm | text | ic | text | ic | text | rm |
page-width == lm  +  n * print-width  +  (n - 1) * ic  +  rm
=> print-width == %d !"
	       page-width
	       ps-left-margin
	       ps-right-margin
	       ps-inter-column
	       ps-number-of-columns
	       ps-print-width))

    (setq ps-print-height
	  (- page-height ps-bottom-margin ps-top-margin))
    (if (<= ps-print-height 0)
	(error "Bad vertical layout:
ps-top-margin    == %s
ps-bottom-margin == %s
page-height == bm + print-height + tm
=> print-height == %d !"
	       ps-top-margin
	       ps-bottom-margin
	       ps-print-height))
    ;; If headers are turned on, deduct the height of the header from the print
    ;; height.
    (if ps-print-header
	(setq ps-header-pad   (* ps-header-line-pad
				 (ps-title-line-height 'ps-font-for-header))
	      ps-print-height (- ps-print-height
				 ps-header-offset
				 ps-header-pad
				 (ps-title-line-height 'ps-font-for-header)
				 (* (ps-line-height 'ps-font-for-header)
				    (1- ps-header-lines))
				 ps-header-pad)))
    (if (<= ps-print-height 0)
	(error "Bad vertical layout (header):
ps-top-margin    == %s
ps-bottom-margin == %s
ps-header-offset == %s
ps-header-pad    == %s
header-height    == %s
page-height == bm + print-height + tm - ho - hh
=> print-height == %d !"
	       ps-top-margin
	       ps-bottom-margin
	       ps-header-offset
	       ps-header-pad
	       (+ ps-header-pad
		  (ps-title-line-height 'ps-font-for-header)
		  (* (ps-line-height 'ps-font-for-header)
		     (1- ps-header-lines))
		  ps-header-pad)
	       ps-print-height))
    ;; If footers are turned on, deduct the height of the footer from the print
    ;; height.
    (if ps-print-footer
	(setq ps-footer-pad   (* ps-footer-line-pad
				 (ps-title-line-height 'ps-font-for-footer))
	      ps-print-height (- ps-print-height
				 ps-footer-offset
				 ps-footer-pad
				 (* (ps-line-height 'ps-font-for-footer)
				    (1- ps-footer-lines))
				 ps-footer-pad)))
    (if (<= ps-print-height 0)
	(error "Bad vertical layout (footer):
ps-top-margin    == %s
ps-bottom-margin == %s
ps-footer-offset == %s
ps-footer-pad    == %s
footer-height    == %s
page-height == bm + print-height + tm - fo - fh
=> print-height == %d !"
	       ps-top-margin
	       ps-bottom-margin
	       ps-footer-offset
	       ps-footer-pad
	       (+ ps-footer-pad
		  (* (ps-line-height 'ps-font-for-footer)
		     (1- ps-footer-lines))
		  ps-footer-pad)
	       ps-print-height))
    ;; ps-zebra-stripe-follow is `full' or `full-follow'
    (if ps-zebra-stripe-full-p
	(let* ((line-height (ps-line-height 'ps-font-for-text))
	       (zebra (* (+ line-height ps-line-spacing-internal)
			 ps-zebra-stripe-height)))
	  (setq ps-print-height (- (* (floor ps-print-height zebra) zebra)
				   line-height))
	  (if (<= ps-print-height 0)
	      (error "Bad vertical layout (full zebra stripe follow):
ps-zebra-stripe-follow == %s
ps-zebra-stripe-height == %s
font-text-height       == %s
line-spacing           == %s
page-height == ((floor print-height ((th + ls) * zh)) * ((th + ls) * zh)) - th
=> print-height == %d !"
		     ps-zebra-stripe-follow
		     ps-zebra-stripe-height
		     (ps-line-height 'ps-font-for-text)
		     ps-line-spacing-internal
		     ps-print-height))))))


(defun ps-print-preprint-region (prefix)
  (or (ps-mark-active-p)
      (error "The mark is not set now"))
  (list (point) (mark) (ps-print-preprint prefix)))


(defun ps-print-preprint (prefix)
  (and prefix
       (or (numberp prefix)
	   (listp prefix))
       (let* ((name   (concat (file-name-nondirectory (or (buffer-file-name)
							  (buffer-name)))
			      ".ps"))
	      (prompt (format "Save PostScript to file (default %s): " name))
	      (res    (read-file-name prompt default-directory name nil)))
	 (while (cond ((file-directory-p res)
		       (ding)
		       (setq prompt "It's a directory"))
		      ((not (file-writable-p res))
		       (ding)
		       (setq prompt "File is unwritable"))
		      ((file-exists-p res)
		       (setq prompt "File exists")
		       (not (y-or-n-p (format "File `%s' exists; overwrite? "
					      res))))
		      (t nil))
	   (setq res (read-file-name
		      (format "%s; save PostScript to file: " prompt)
		      (file-name-directory res) nil nil
		      (file-name-nondirectory res))))
	 (if (file-directory-p res)
	     (expand-file-name name (file-name-as-directory res))
	   res))))

;; The following functions implement a simple list-buffering scheme so
;; that ps-print doesn't have to repeatedly switch between buffers
;; while spooling.  The functions `ps-output' and `ps-output-string' build
;; up the lists; the function `ps-flush-output' takes the lists and
;; insert its contents into the spool buffer (*PostScript*).

(defvar ps-string-escape-codes
  (let ((table (make-vector 256 nil))
	(char ?\000))
    ;; control characters
    (while (<= char ?\037)
      (aset table char (format "\\%03o" char))
      (setq char (1+ char)))
    ;; printable characters
    (while (< char ?\177)
      (aset table char (format "%c" char))
      (setq char (1+ char)))
    ;; DEL and 8-bit characters
    (while (<= char ?\377)
      (aset table char (format "\\%o" char))
      (setq char (1+ char)))
    ;; Override ASCII formatting characters with named escape code:
    (aset table ?\n "\\n")		; [NL] linefeed
    (aset table ?\r "\\r")		; [CR] carriage return
    (aset table ?\t "\\t")		; [HT] horizontal tab
    (aset table ?\b "\\b")		; [BS] backspace
    (aset table ?\f "\\f")		; [NP] form feed
    ;; Escape PostScript escape and string delimiter characters:
    (aset table ?\\ "\\\\")
    (aset table ?\( "\\(")
    (aset table ?\) "\\)")
    table)
  "Vector used to map characters to PostScript string escape codes.")

(defsubst ps-output-string-prim (string)
  (insert "(")				;insert start-string delimiter
  (save-excursion			;insert string
    (insert (string-as-unibyte string)))
  ;; Find and quote special characters as necessary for PS
  ;; This skips everything except control chars, non-ASCII chars, (, ) and \.
  (while (progn (skip-chars-forward " -'*-[]-~") (not (eobp)))
    (let ((special (following-char)))
      (delete-char 1)
      (insert
       (if (and (<= 0 special) (<= special 255))
	   (aref ps-string-escape-codes special)
	 ;; insert hexadecimal representation if character code is out of range
	 (format "\\%04X" special)
	 ))))
  (goto-char (point-max))
  (insert ")"))				;insert end-string delimiter

(defsubst ps-init-output-queue ()
  (setq ps-output-head (list "")
	ps-output-tail ps-output-head))


(defun ps-selected-pages ()
  (while (progn
	   (setq ps-first-page     (car (car ps-selected-pages))
		 ps-last-page      (cdr (car ps-selected-pages))
		 ps-selected-pages (cdr ps-selected-pages))
	   (and ps-selected-pages
		(< ps-last-page ps-page-postscript)))))


(defsubst ps-print-page-p ()
  (setq ps-print-page-p
	(and (cond ((null ps-first-page))
		   ((<= ps-page-postscript ps-last-page)
		    (<= ps-first-page ps-page-postscript))
		   (ps-selected-pages
		    (ps-selected-pages)
		    (and (<= ps-first-page ps-page-postscript)
			 (<= ps-page-postscript ps-last-page)))
		   (t
		    nil))
	     (cond ((eq ps-even-or-odd-pages 'even-page)
		    (= (logand ps-page-postscript 1) 0))
		   ((eq ps-even-or-odd-pages 'odd-page)
		    (= (logand ps-page-postscript 1) 1))
		   (t)
		   ))))


(defsubst ps-print-sheet-p ()
  (setq ps-print-page-p
	(cond ((eq ps-even-or-odd-pages 'even-sheet)
	       (= (logand ps-page-sheet 1) 0))
	      ((eq ps-even-or-odd-pages 'odd-sheet)
	       (= (logand ps-page-sheet 1) 1))
	      (t)
	      )))


(defun ps-output (&rest args)
  (when ps-print-page-p
    (setcdr ps-output-tail args)
    (while (cdr ps-output-tail)
      (setq ps-output-tail (cdr ps-output-tail)))))

(defun ps-output-string (string)
  (ps-output t string))

;; Output strings in the list ARGS in the PostScript prologue part.
(defun ps-output-prologue (args)
  (ps-output 'prologue (if (stringp args) (list args) args)))

(defun ps-flush-output ()
  (with-current-buffer ps-spool-buffer
    (goto-char (point-max))
    (while ps-output-head
      (let ((it (car ps-output-head)))
	(cond
	 ((eq t it)
	  (setq ps-output-head (cdr ps-output-head))
	  (ps-output-string-prim (car ps-output-head)))
	 ((eq 'prologue it)
	  (setq ps-output-head (cdr ps-output-head))
	  (save-excursion
	    (search-backward "\nBeginDoc")
	    (forward-char 1)
	    (apply 'insert (car ps-output-head))))
	 (t
	  (insert it))))
      (setq ps-output-head (cdr ps-output-head))))
  (ps-init-output-queue))

(defun ps-insert-file (fname)
  (ps-flush-output)
  (with-current-buffer ps-spool-buffer
    (goto-char (point-max))
    (insert-file-contents fname)))

;; These functions insert the arrays that define the contents of the headers.

(defvar ps-encode-header-string-function nil)

(defun ps-generate-header-line (fonttag &optional content)
  (ps-output " [" fonttag " ")
  (cond
   ;; Literal strings should be output as is -- the string must contain its own
   ;; PS string delimiters, '(' and ')', if necessary.
   ((stringp content)
    (ps-output content))

   ;; Functions are called -- they should return strings; they will be inserted
   ;; as strings and the PS string delimiters added.
   ((functionp content)
    (if (functionp ps-encode-header-string-function)
	(dolist (l (funcall ps-encode-header-string-function
			    (funcall content) fonttag))
	  (ps-output-string l))
      (ps-output-string (funcall content))))

   ;; Variables will have their contents inserted.  They should contain
   ;; strings, and will be inserted as strings.
   ((and (symbolp content) (boundp content))
    (if (fboundp ps-encode-header-string-function)
	(dolist (l (funcall ps-encode-header-string-function
			     (symbol-value content) fonttag))
	  (ps-output-string l))
      (ps-output-string (symbol-value content))))

   ;; Anything else will get turned into an empty string.
   (t
    (ps-output-string "")))
  (ps-output "]\n"))

(defun ps-generate-header (name fonttag0 fonttag1 contents)
  (ps-output "/" name "[\n")
  (and contents (> ps-header-lines 0)
       (let ((count 1))
	 (ps-generate-header-line fonttag0 (car contents))
	 (while (and (< count ps-header-lines)
		     (setq contents (cdr contents)))
	   (ps-generate-header-line fonttag1 (car contents))
	   (setq count (1+ count)))))
  (ps-output "]def\n"))


(defun ps-output-boolean (name bool)
  (ps-output (format "/%s %s def\n" name (if bool "true" "false"))))


(defun ps-output-frame-properties (name alist)
  (ps-output "/" name " ["
	     (ps-format-color (cdr (assq 'fore-color alist)) 0.0)
	     (ps-format-color (cdr (assq 'back-color alist)) 0.9)
	     (ps-float-format (or (cdr (assq 'border-width alist)) 0.4))
	     (ps-format-color (cdr (assq 'border-color alist)) 0.0)
	     (ps-format-color (cdr (assq 'shadow-color alist)) 0.0)
	     "]def\n"))


(defun ps-background-pages (page-list func)
  (if page-list
      (mapcar
       #'(lambda (pages)
	   (let ((start (if (consp pages) (car pages) pages))
		 (end   (if (consp pages) (cdr pages) pages)))
	     (and (integerp start) (integerp end) (<= start end)
		  (add-to-list 'ps-background-pages (vector start end func)))))
       page-list)
    (setq ps-background-all-pages (cons func ps-background-all-pages))))


(defconst ps-boundingbox-re
  "^%%BoundingBox:\
\\s-+\\([0-9.]+\\)\\s-+\\([0-9.]+\\)\\s-+\\([0-9.]+\\)\\s-+\\([0-9.]+\\)")


(defun ps-get-boundingbox ()
  (with-current-buffer ps-spool-buffer
    (save-excursion
      (if (re-search-forward ps-boundingbox-re nil t)
	  (vector (string-to-number	; lower x
		   (buffer-substring (match-beginning 1) (match-end 1)))
		  (string-to-number	; lower y
		   (buffer-substring (match-beginning 2) (match-end 2)))
		  (string-to-number	; upper x
		   (buffer-substring (match-beginning 3) (match-end 3)))
		  (string-to-number	; upper y
		   (buffer-substring (match-beginning 4) (match-end 4))))
	(vector 0 0 0 0)))))


(defun ps-float-format (value &optional default)
  (let ((literal (or value default)))
    (cond ((null literal)
	   " ")
	  ((numberp literal)
	    (format ps-float-format (* literal 1.0))) ; force float number
	  (t
	   (format "%s " literal))
	  )))


(defun ps-background-text ()
  (mapcar
   #'(lambda (text)
       (setq ps-background-text-count (1+ ps-background-text-count))
       (ps-output (format "/ShowBackText-%d{\n" ps-background-text-count))
       (ps-output-string (nth 0 text))	; text
       (ps-output
	"\n"
	(ps-float-format (nth 4 text) 200.0) ; font size
	(format "/%s " (or (nth 3 text) "Times-Roman")) ; font name
	(ps-float-format (nth 6 text)
			 "PrintHeight PrintPageWidth atan") ; rotation
	(ps-float-format (nth 5 text) 0.85) ; gray
	(ps-float-format (nth 1 text) "0") ; x position
	(ps-float-format (nth 2 text) "0") ; y position
	"\nShowBackText}def\n")
       (ps-background-pages (nthcdr 7 text) ; page list
			    (format "ShowBackText-%d\n"
				    ps-background-text-count)))
   ps-print-background-text))


(defun ps-background-image ()
  (mapcar
   #'(lambda (image)
       (let ((image-file (expand-file-name (nth 0 image))))
	 (when (file-readable-p image-file)
	   (setq ps-background-image-count (1+ ps-background-image-count))
	   (ps-output
	    (format "/ShowBackImage-%d{\n--back-- "
		    ps-background-image-count)
	    (ps-float-format (nth 5 image) 0.0) ; rotation
	    (ps-float-format (nth 3 image) 1.0) ; x scale
	    (ps-float-format (nth 4 image) 1.0) ; y scale
	    (ps-float-format (nth 1 image) ; x position
			     "PrintPageWidth 2 div")
	    (ps-float-format (nth 2 image) ; y position
			     "PrintHeight 2 div BottomMargin add")
	    "\nBeginBackImage\n")
	   (ps-insert-file image-file)
	   ;; coordinate adjustment to center image
	   ;; around x and y position
	   (let ((box (ps-get-boundingbox)))
	     (with-current-buffer ps-spool-buffer
	       (save-excursion
		 (if (re-search-backward "^--back--" nil t)
		     (replace-match
		      (format "%s %s"
			      (ps-float-format
			       (- (+ (/ (- (aref box 2) (aref box 0)) 2.0)
				     (aref box 0))))
			      (ps-float-format
			       (- (+ (/ (- (aref box 3) (aref box 1)) 2.0)
				     (aref box 1)))))
		      t)))))
	   (ps-output "\nEndBackImage}def\n")
	   (ps-background-pages (nthcdr 6 image) ; page list
				(format "ShowBackImage-%d\n"
					ps-background-image-count)))))
   ps-print-background-image))


(defun ps-background (page-number)
  (let (has-local-background)
    (mapc #'(lambda (range)
	      (and (<= (aref range 0) page-number)
		   (<= page-number (aref range 1))
		   (if has-local-background
		       (ps-output (aref range 2))
		     (setq has-local-background t)
		     (ps-output "/printLocalBackground{\n"
				(aref range 2)))))
	  ps-background-pages)
    (and has-local-background (ps-output "}def\n"))))


;; Return a list of the distinct elements of LIST.
;; Elements are compared with `equal'.
(defun ps-remove-duplicates (list)
  (let (new (tail list))
    (while tail
      (or (member (car tail) new)
	  (setq new (cons (car tail) new)))
      (setq tail (cdr tail)))
    (nreverse new)))


;; Find the first occurrence of ITEM in LIST.
;; Return the index of the matching item, or nil if not found.
;; Elements are compared with `eq'.
(defun ps-alist-position (item list)
  (let ((tail list) (index 0) found)
    (while tail
      (if (setq found (eq (car (car tail)) item))
	  (setq tail nil)
	(setq index (1+ index)
	      tail (cdr tail))))
    (and found index)))


(defconst ps-n-up-database
  '((a4
     (1   nil 1  1  0)
     (2   t   1  2  0)
     (4   nil 2  2  0)
     (6   t   2  3  1)
     (8   t   2  4  0)
     (9   nil 3  3  0)
     (12  t   3  4  2)
     (16  nil 4  4  0)
     (18  t   3  6  0)
     (20  nil 5  4  1)
     (25  nil 5  5  0)
     (30  nil 6  5  1)
     (32  t   4  8  0)
     (36  nil 6  6  0)
     (42  nil 7  6  1)
     (49  nil 7  7  0)
     (50  t   5  10 0)
     (56  nil 8  7  1)
     (64  nil 8  8  0)
     (72  nil 9  8  1)
     (81  nil 9  9  0)
     (90  nil 10 9  1)
     (100 nil 10 10 0))
    (a3
     (1   nil 1  1  0)
     (2   t   1  2  0)
     (4   nil 2  2  0)
     (6   t   2  3  1)
     (8   t   2  4  0)
     (9   nil 3  3  0)
     (12  nil 4  3  1)
     (16  nil 4  4  0)
     (18  t   3  6  0)
     (20  nil 5  4  1)
     (25  nil 5  5  0)
     (30  nil 6  5  1)
     (32  t   4  8  0)
     (36  nil 6  6  0)
     (42  nil 7  6  1)
     (49  nil 7  7  0)
     (50  t   5  10 0)
     (56  nil 8  7  1)
     (64  nil 8  8  0)
     (72  nil 9  8  1)
     (81  nil 9  9  0)
     (90  nil 10 9  1)
     (100 nil 10 10 0))
    (letter
     (1   nil 1  1  0)
     (2   t   1  2  0)			; adjusted by PostScript code
     (4   nil 2  2  0)
     (6   t   2  3  0)
     (9   nil 3  3  0)
     (12  nil 4  3  1)
     (16  nil 4  4  0)
     (20  nil 5  4  1)
     (25  nil 5  5  0)
     (30  nil 6  5  1)
     (36  nil 6  6  0)
     (40  t   5  8  0)
     (42  nil 7  6  1)
     (49  nil 7  7  0)
     (56  nil 8  7  1)
     (64  nil 8  8  0)
     (72  nil 9  8  1)
     (81  nil 9  9  0)
     (90  nil 10 9  1)
     (100 nil 10 10 0))
    (legal
     (1   nil 1  1  0)
     (2   t   1  2  0)
     (4   nil 2  2  0)
     (6   nil 3  2  1)
     (9   nil 3  3  0)
     (10  t   2  5  0)
     (12  nil 4  3  1)
     (16  nil 4  4  0)
     (20  nil 5  4  1)
     (25  nil 5  5  0)
     (30  nil 6  5  1)
     (36  nil 6  6  0)
     (42  nil 7  6  1)
     (49  nil 7  7  0)
     (56  nil 8  7  1)
     (64  nil 8  8  0)
     (70  t   5  14 0)
     (72  nil 9  8  1)
     (81  nil 9  9  0)
     (90  nil 10 9  1)
     (100 nil 10 10 0))
    (letter-small
     (1   nil 1  1  0)
     (2   t   1  2  0)			; adjusted by PostScript code
     (4   nil 2  2  0)
     (6   t   2  3  0)
     (9   nil 3  3  0)
     (12  t   3  4  1)
     (15  t   3  5  0)
     (16  nil 4  4  0)
     (20  nil 5  4  1)
     (25  nil 5  5  0)
     (28  t   4  7  0)
     (30  nil 6  5  1)
     (36  nil 6  6  0)
     (40  t   5  8  0)
     (42  nil 7  6  1)
     (49  nil 7  7  0)
     (56  nil 8  7  1)
     (60  t   6  10 0)
     (64  nil 8  8  0)
     (72  ni  9  8  1)
     (81  nil 9  9  0)
     (84  t   7  12 0)
     (90  nil 10 9  1)
     (100 nil 10 10 0))
    (tabloid
     (1   nil 1  1  0)
     (2   t   1  2  0)
     (4   nil 2  2  0)
     (6   t   2  3  1)
     (8   t   2  4  0)
     (9   nil 3  3  0)
     (12  nil 4  3  1)
     (16  nil 4  4  0)
     (20  nil 5  4  1)
     (25  nil 5  5  0)
     (30  nil 6  5  1)
     (36  nil 6  6  0)
     (42  nil 7  6  1)
     (49  nil 7  7  0)
     (56  nil 8  7  1)
     (64  nil 8  8  0)
     (72  nil 9  8  1)
     (81  nil 9  9  0)
     (84  t   6  14 0)
     (90  nil 10 9  1)
     (100 nil 10 10 0))
    ;; Ledger paper size is a special case, it is the only paper size where the
    ;; normal size is landscaped, that is, the height is smaller than width.
    ;; So, we use the special value `pag' in the `landscape' field.
    (ledger
     (1   nil 1  1  0)
     (2   pag 1  2  0)
     (4   nil 2  2  0)
     (6   pag 2  3  1)
     (8   pag 2  4  0)
     (9   nil 3  3  0)
     (12  nil 4  3  1)
     (16  nil 4  4  0)
     (20  nil 5  4  1)
     (25  nil 5  5  0)
     (30  nil 6  5  1)
     (36  nil 6  6  0)
     (42  nil 7  6  1)
     (49  nil 7  7  0)
     (56  nil 8  7  1)
     (64  nil 8  8  0)
     (72  nil 9  8  1)
     (81  nil 9  9  0)
     (84  pag 6  14 0)
     (90  nil 10 9  1)
     (100 nil 10 10 0))
    (statement
     (1   nil 1  1  0)
     (2   t   1  2  0)
     (4   nil 2  2  0)
     (6   nil 3  2  1)
     (9   nil 3  3  0)
     (10  t   2  5  0)
     (12  nil 4  3  1)
     (16  nil 4  4  0)
     (20  nil 5  4  1)
     (21  t   3  7  0)
     (25  nil 5  5  0)
     (30  nil 6  5  1)
     (36  nil 6  6  0)
     (40  t   4  10 0)
     (42  nil 7  6  1)
     (49  nil 7  7  0)
     (56  nil 8  7  1)
     (60  t   5  12 0)
     (64  nil 8  8  0)
     (72  nil 9  8  1)
     (81  nil 9  9  0)
     (90  nil 10 9  1)
     (100 nil 10 10 0))
    (executive
     (1   nil 1  1  0)
     (2   t   1  2  0)			; adjusted by PostScript code
     (4   nil 2  2  0)
     (6   t   2  3  0)
     (9   nil 3  3  0)
     (12  nil 4  3  1)
     (16  nil 4  4  0)
     (20  nil 5  4  1)
     (25  nil 5  5  0)
     (28  t   4  7  0)
     (30  nil 6  5  1)
     (36  nil 6  6  0)
     (42  nil 7  6  1)
     (45  t   5  9  0)
     (49  nil 7  7  0)
     (56  nil 8  7  1)
     (60  t   6  10 0)
     (64  nil 8  8  0)
     (72  nil 9  8  1)
     (81  nil 9  9  0)
     (84  t   7  12 0)
     (90  nil 10 9  1)
     (100 nil 10 10 0))
    (a4small
     (1   nil 1  1  0)
     (2   t   1  2  0)
     (4   nil 2  2  0)
     (6   t   2  3  1)
     (8   t   2  4  0)
     (9   nil 3  3  0)
     (12  nil 4  3  1)
     (16  nil 4  4  0)
     (18  t   3  6  0)
     (20  nil 5  4  1)
     (25  nil 5  5  0)
     (30  nil 6  5  1)
     (32  t   4  8  0)
     (36  nil 6  6  0)
     (42  nil 7  6  1)
     (49  nil 7  7  0)
     (50  t   5  10 0)
     (56  nil 8  7  1)
     (64  nil 8  8  0)
     (72  nil 9  8  1)
     (78  t   6  13 0)
     (81  nil 9  9  0)
     (90  nil 10 9  1)
     (100 nil 10 10 0))
    (b4
     (1   nil 1  1  0)
     (2   t   1  2  0)
     (4   nil 2  2  0)
     (6   t   2  3  1)
     (8   t   2  4  0)
     (9   nil 3  3  0)
     (12  nil 4  3  1)
     (16  nil 4  4  0)
     (18  t   3  6  0)
     (20  nil 5  4  1)
     (25  nil 5  5  0)
     (30  nil 6  5  1)
     (32  t   4  8  0)
     (36  nil 6  6  0)
     (42  nil 7  6  1)
     (49  nil 7  7  0)
     (50  t   5  10 0)
     (56  nil 8  7  1)
     (64  nil 8  8  0)
     (72  nil 9  8  1)
     (81  nil 9  9  0)
     (90  nil 10 9  1)
     (100 nil 10 10 0))
    (b5
     (1   nil 1  1  0)
     (2   t   1  2  0)
     (4   nil 2  2  0)
     (6   t   2  3  1)
     (8   t   2  4  0)
     (9   nil 3  3  0)
     (12  nil 4  3  1)
     (16  nil 4  4  0)
     (18  t   3  6  0)
     (20  nil 5  4  1)
     (25  nil 5  5  0)
     (30  nil 6  5  1)
     (32  t   4  8  0)
     (36  nil 6  6  0)
     (42  nil 7  6  1)
     (49  nil 7  7  0)
     (50  t   5  10 0)
     (56  nil 8  7  1)
     (64  nil 8  8  0)
     (72  nil 9  8  0)
     (81  nil 9  9  0)
     (90  nil 10 9  1)
     (98  t   7  14 0)
     (100 nil 10 10 0)))
  "Alist which is the page matrix database used for N-up printing.

Each element has the following form:

   (PAGE
    (MAX LANDSCAPE LINES COLUMNS COL-MISSING)
    ...)

Where:
PAGE is the page size used (see `ps-paper-type').
MAX is the maximum elements of this page matrix.
LANDSCAPE specifies if page matrix is landscaped, has the following valid
	  values:
	    nil   the sheet is in portrait mode.
	    t     the sheet is in landscape mode.
	    pag   the sheet is in portrait mode and page is in landscape mode.
LINES is the number of lines of page matrix.
COLUMNS is the number of columns of page matrix.
COL-MISSING is the number of columns missing to fill the sheet.")


(defmacro ps-n-up-landscape (mat) `(nth 1 ,mat))
(defmacro ps-n-up-lines     (mat) `(nth 2 ,mat))
(defmacro ps-n-up-columns   (mat) `(nth 3 ,mat))
(defmacro ps-n-up-missing   (mat) `(nth 4 ,mat))


(defun ps-n-up-printing ()
  ;; force `ps-n-up-printing' be in range 1 to 100.
  (setq ps-n-up-printing (max (min ps-n-up-printing 100) 1))
  ;; find suitable page matrix for a given `ps-paper-type'.
  (let ((the-list (cdr (assq ps-paper-type ps-n-up-database))))
    (and the-list
	 (while (> ps-n-up-printing (caar the-list))
	   (setq the-list (cdr the-list))))
    (or (car the-list)
	'(1 nil 1 1 0))))


(defconst ps-n-up-filling-database
  '((left-top
     "PageWidth"			; N-Up-XColumn
     "0"				; N-Up-YColumn
     "N-Up-End 1 sub PageWidth mul neg" ; N-Up-XLine
     "LandscapePageHeight neg"		; N-Up-YLine
     "N-Up-Lines"			; N-Up-Repeat
     "N-Up-Columns"			; N-Up-End
     "0"				; N-Up-XStart
     "0")				; N-Up-YStart
    (left-bottom
     "PageWidth"			; N-Up-XColumn
     "0"				; N-Up-YColumn
     "N-Up-End 1 sub PageWidth mul neg" ; N-Up-XLine
     "LandscapePageHeight"		; N-Up-YLine
     "N-Up-Lines"			; N-Up-Repeat
     "N-Up-Columns"			; N-Up-End
     "0"				; N-Up-XStart
     "N-Up-Repeat 1 sub LandscapePageHeight mul neg") ; N-Up-YStart
    (right-top
     "PageWidth neg"			; N-Up-XColumn
     "0"				; N-Up-YColumn
     "N-Up-End 1 sub PageWidth mul"	; N-Up-XLine
     "LandscapePageHeight neg"		; N-Up-YLine
     "N-Up-Lines"			; N-Up-Repeat
     "N-Up-Columns"			; N-Up-End
     "N-Up-End 1 sub PageWidth mul"	; N-Up-XStart
     "0")				; N-Up-YStart
    (right-bottom
     "PageWidth neg"			; N-Up-XColumn
     "0"				; N-Up-YColumn
     "N-Up-End 1 sub PageWidth mul"	; N-Up-XLine
     "LandscapePageHeight"		; N-Up-YLine
     "N-Up-Lines"			; N-Up-Repeat
     "N-Up-Columns"			; N-Up-End
     "N-Up-End 1 sub PageWidth mul"	; N-Up-XStart
     "N-Up-Repeat 1 sub LandscapePageHeight mul neg") ; N-Up-YStart
    (top-left
     "0"				; N-Up-XColumn
     "LandscapePageHeight neg"		; N-Up-YColumn
     "PageWidth"			; N-Up-XLine
     "N-Up-End 1 sub LandscapePageHeight mul" ; N-Up-YLine
     "N-Up-Columns"			; N-Up-Repeat
     "N-Up-Lines"			; N-Up-End
     "0"				; N-Up-XStart
     "0")				; N-Up-YStart
    (bottom-left
     "0"				; N-Up-XColumn
     "LandscapePageHeight"		; N-Up-YColumn
     "PageWidth"			; N-Up-XLine
     "N-Up-End 1 sub LandscapePageHeight mul neg" ; N-Up-YLine
     "N-Up-Columns"			; N-Up-Repeat
     "N-Up-Lines"			; N-Up-End
     "0"				; N-Up-XStart
     "N-Up-End 1 sub LandscapePageHeight mul neg") ; N-Up-YStart
    (top-right
     "0"				; N-Up-XColumn
     "LandscapePageHeight neg"		; N-Up-YColumn
     "PageWidth neg"			; N-Up-XLine
     "N-Up-End 1 sub LandscapePageHeight mul" ; N-Up-YLine
     "N-Up-Columns"			; N-Up-Repeat
     "N-Up-Lines"			; N-Up-End
     "N-Up-Repeat 1 sub PageWidth mul"	; N-Up-XStart
     "0")				; N-Up-YStart
    (bottom-right
     "0"				; N-Up-XColumn
     "LandscapePageHeight"		; N-Up-YColumn
     "PageWidth neg"			; N-Up-XLine
     "N-Up-End 1 sub LandscapePageHeight mul neg" ; N-Up-YLine
     "N-Up-Columns"			; N-Up-Repeat
     "N-Up-Lines"			; N-Up-End
     "N-Up-Repeat 1 sub PageWidth mul"	; N-Up-XStart
     "N-Up-End 1 sub LandscapePageHeight mul neg")) ; N-Up-YStart
  "Alist for n-up printing initializations.

Each element has the following form:

   (KIND XCOL YCOL XLIN YLIN REPEAT END XSTART YSTART)

Where:
KIND is a valid value of `ps-n-up-filling'.
XCOL YCOL are the relative position for the next column.
XLIN YLIN are the relative position for the beginning of next line.
REPEAT is the number of repetitions for external loop.
END is the number of repetitions for internal loop and also the number
    of pages in a row.
XSTART YSTART are the relative position for the first page in a sheet.")


(defun ps-n-up-filling ()
  (cdr (or (assq ps-n-up-filling ps-n-up-filling-database)
	   (assq 'left-top ps-n-up-filling-database))))


(defmacro ps-n-up-xcolumn (init) `(nth 0 ,init))
(defmacro ps-n-up-ycolumn (init) `(nth 1 ,init))
(defmacro ps-n-up-xline	  (init) `(nth 2 ,init))
(defmacro ps-n-up-yline	  (init) `(nth 3 ,init))
(defmacro ps-n-up-repeat  (init) `(nth 4 ,init))
(defmacro ps-n-up-end	  (init) `(nth 5 ,init))
(defmacro ps-n-up-xstart  (init) `(nth 6 ,init))
(defmacro ps-n-up-ystart  (init) `(nth 7 ,init))


(defconst ps-error-handler-alist
  '((none             . 0)
    (paper            . 1)
    (system           . 2)
    (paper-and-system . 3))
  "Alist for error handler message.")


(defconst ps-zebra-stripe-alist
  '((follow      . 1)
    (full        . 2)
    (full-follow . 3))
  "Alist for zebra stripe continuation.")


(defun ps-begin-file ()
  (setq ps-page-order 0
	ps-page-printed 0
	ps-background-text-count 0
	ps-background-image-count 0
	ps-background-pages nil
	ps-background-all-pages nil)

  (let ((dimensions (cdr (assq ps-paper-type ps-page-dimensions-database)))
	(tumble (if ps-landscape-mode (not ps-spool-tumble) ps-spool-tumble))
	(n-up (ps-n-up-printing))
	(n-up-filling (ps-n-up-filling)))
    (and ps-n-up-on (setq tumble (not tumble)))
    (ps-output
     ps-adobe-tag
     "%%Title: " (buffer-name)		; Take job name from name of
					; first buffer printed
     "\n%%Creator: ps-print v" ps-print-version
     "\n%%For: " (user-full-name)
     "\n%%CreationDate: " (format-time-string "%T %b %d %Y")
     "\n%%Orientation: "
     (if ps-landscape-mode "Landscape" "Portrait")
     "\n%%DocumentNeededResources: font Times-Roman Times-Italic\n%%+ font "
     (mapconcat 'identity
		(ps-remove-duplicates
		 (append (ps-fonts 'ps-font-for-text)
			 (list (ps-font 'ps-font-for-header 'normal)
			       (ps-font 'ps-font-for-header 'bold)
			       (ps-font 'ps-font-for-footer 'normal)
			       (ps-font 'ps-font-for-footer 'bold))))
		"\n%%+ font ")
     "\n%%DocumentSuppliedResources: procset PSPrintUserDefinedPrologue-" (user-login-name) " 0 0"
     "\n%%DocumentMedia: " (ps-page-dimensions-get-media dimensions)
     (format " %d" (round (ps-page-dimensions-get-width dimensions)))
     (format " %d" (round (ps-page-dimensions-get-height dimensions)))
     " 0 () ()\n%%PageOrder: Ascend\n%%Pages: (atend)\n%%Requirements:"
     (if ps-spool-duplex
	 (if tumble " duplex(tumble)\n" " duplex\n")
       "\n"))

    (ps-insert-string ps-print-prologue-header)

    (ps-output "%%EndComments\n%%BeginDefaults\n%%PageMedia: "
	       (ps-page-dimensions-get-media dimensions)
	       "\n%%EndDefaults\n\n%%BeginProlog\n\n"
	       "/languagelevel where{pop}{/languagelevel 1 def}ifelse\n"
	       (format "/ErrorMessage  %s def\n\n"
		       (or (cdr (assoc ps-error-handler-message
				       ps-error-handler-alist))
			   1))		; send to paper
	       ps-print-prologue-0
	       "\n%%BeginResource: procset PSPrintUserDefinedPrologue-" (user-login-name) " 0 0\n\n")

    (ps-insert-string ps-user-defined-prologue)

    (ps-output "\n%%EndResource\n\n")

    (ps-output-boolean "LandscapeMode      "
		       (or ps-landscape-mode
			   (eq (ps-n-up-landscape n-up) 'pag)))
    (ps-output-boolean "UpsideDown         " ps-print-upside-down)
    (ps-output (format "/NumberOfColumns     %d def\n" ps-number-of-columns)

	       (format "/LandscapePageHeight %s def\n" ps-landscape-page-height)
	       (format "/PrintPageWidth      %s def\n"
		       (- (* (+ ps-print-width ps-inter-column)
			     ps-number-of-columns)
			  ps-inter-column))
	       (format "/PrintWidth   %s def\n" ps-print-width)
	       (format "/PrintHeight  %s def\n" ps-print-height)

	       (format "/LeftMargin   %s def\n" ps-left-margin)
	       (format "/RightMargin  %s def\n" ps-right-margin)
	       (format "/InterColumn  %s def\n" ps-inter-column)

	       (format "/BottomMargin %s def\n" ps-bottom-margin)
	       (format "/TopMargin    %s def\n" ps-top-margin) ; not used
	       (format "/HeaderOffset %s def\n" ps-header-offset)
	       (format "/HeaderPad    %s def\n" ps-header-pad)
	       (format "/FooterOffset %s def\n" ps-footer-offset)
	       (format "/FooterPad    %s def\n" ps-footer-pad)
	       (format "/FooterLines  %s def\n" ps-footer-lines))

    (ps-output-boolean "ShowNofN          " ps-show-n-of-n)
    (ps-output-boolean "SwitchHeader      " (if (eq ps-switch-header 'duplex)
						ps-spool-duplex
					      ps-switch-header))
    (ps-output-boolean "PrintOnlyOneHeader" ps-print-only-one-header)
    (ps-output-boolean "PrintHeader       " ps-print-header)
    (ps-output-boolean "PrintHeaderFrame  " ps-print-header-frame)
    (ps-output-frame-properties "HeaderFrameProperties" ps-header-frame-alist)
    (ps-output-boolean "PrintFooter       " ps-print-footer)
    (ps-output-boolean "PrintFooterFrame  " ps-print-footer-frame)
    (ps-output-frame-properties "FooterFrameProperties" ps-footer-frame-alist)

    (let ((line-height (ps-line-height 'ps-font-for-text)))
      (ps-output (format "/LineSpacing      %s def\n" ps-line-spacing-internal)
		 (format "/ParagraphSpacing %s def\n"
			 ps-paragraph-spacing-internal)
		 (format "/LineHeight       %s def\n" line-height)
		 (format "/LinesPerColumn   %d def\n"
			 (let ((height (+ line-height
					  ps-line-spacing-internal)))
			   (round (/ (+ ps-print-height
					(* height 0.45))
				     height))))))

    (ps-output-boolean "WarnPaperSize   " ps-warn-paper-type)
    (ps-output-boolean "Zebra           " ps-zebra-stripes)
    (ps-output-boolean "PrintLineNumber " ps-line-number)
    (ps-output-boolean "SyncLineZebra   " (not (integerp ps-line-number-step)))
    (ps-output (format "/ZebraFollow      %d def\n"
		       (or (cdr (assq ps-zebra-stripe-follow
				      ps-zebra-stripe-alist))
			   0))
	       (format "/PrintLineStep    %d def\n"
		       (if (integerp ps-line-number-step)
			   ps-line-number-step
			 ps-zebra-stripe-height))
	       (format "/PrintLineStart   %d def\n" ps-line-number-start)
	       "/LineNumberColor  "
	       (ps-format-color ps-line-number-color 0.0)
	       (format "def\n/ZebraHeight      %d def\n"
		       ps-zebra-stripe-height)
	       "/ZebraColor       "
	       (ps-format-color ps-zebra-color 0.95)
	       "def\n")
    (ps-output "/BackgroundColor  "
	       (ps-format-color ps-default-background 1.0)
	       "def\n")
    (ps-output "/UseSetpagedevice "
	       (if (eq ps-spool-config 'setpagedevice)
		   "/setpagedevice where{pop languagelevel 2 eq}{false}ifelse"
		 "false")
	       " def\n\n/PageWidth "
	       "PrintPageWidth LeftMargin add RightMargin add def\n\n"
	       (format "/N-Up           %d def\n" ps-n-up-printing))
    (ps-output-boolean "N-Up-Landscape" (eq (ps-n-up-landscape n-up) t))
    (ps-output-boolean "N-Up-Border   " ps-n-up-border-p)
    (ps-output (format "/N-Up-Lines     %d def\n" (ps-n-up-lines n-up))
	       (format "/N-Up-Columns   %d def\n" (ps-n-up-columns n-up))
	       (format "/N-Up-Missing   %d def\n" (ps-n-up-missing n-up))
	       (format "/N-Up-Margin    %s def\n" ps-n-up-margin)
	       "/N-Up-Repeat    "
	       (if ps-landscape-mode
		   (ps-n-up-end     n-up-filling)
		 (ps-n-up-repeat  n-up-filling))
	       " def\n/N-Up-End       "
	       (if ps-landscape-mode
		   (ps-n-up-repeat  n-up-filling)
		 (ps-n-up-end     n-up-filling))
	       " def\n/N-Up-XColumn   " (ps-n-up-xcolumn n-up-filling)
	       " def\n/N-Up-YColumn   " (ps-n-up-ycolumn n-up-filling)
	       " def\n/N-Up-XLine     " (ps-n-up-xline   n-up-filling)
	       " def\n/N-Up-YLine     " (ps-n-up-yline   n-up-filling)
	       " def\n/N-Up-XStart    " (ps-n-up-xstart  n-up-filling)
	       " def\n/N-Up-YStart    " (ps-n-up-ystart  n-up-filling) " def\n")

    (ps-background-text)
    (ps-background-image)
    (setq ps-background-all-pages (nreverse ps-background-all-pages)
	  ps-background-pages (nreverse ps-background-pages))

    (ps-output "\n" ps-print-prologue-1
	       "\n/printGlobalBackground{\n")
    (mapc 'ps-output ps-background-all-pages)
    (ps-output
     "}def\n/printLocalBackground{\n}def\n"
     "\n%%EndProlog\n\n%%BeginSetup\n"
     "\n%%IncludeResource: font Times-Roman"
     "\n%%IncludeResource: font Times-Italic"
     "\n%%IncludeResource: font "
     (mapconcat 'identity
		(ps-remove-duplicates
		 (append (ps-fonts 'ps-font-for-text)
			 (list (ps-font 'ps-font-for-header 'normal)
			       (ps-font 'ps-font-for-header 'bold)
			       (ps-font 'ps-font-for-footer 'normal)
			       (ps-font 'ps-font-for-footer 'bold))))
		"\n%%IncludeResource: font ")
     ;; Header/line number fonts
     (format "\n/h0 %s(%s)cvn DefFont\n" ; /h0 14/Helvetica-Bold DefFont
	     ps-header-title-font-size-internal
	     (ps-font 'ps-font-for-header 'bold))
     (format "/h1 %s(%s)cvn DefFont\n"	; /h1 12/Helvetica DefFont
	     ps-header-font-size-internal
	     (ps-font 'ps-font-for-header 'normal))
     (format "/L0 %s(%s)cvn DefFont\n"	; /L0 6/Times-Italic DefFont
	     (ps-get-font-size 'ps-line-number-font-size)
	     ps-line-number-font)
     (format "/H0 %s(%s)cvn DefFont\n"	; /H0 12/Helvetica DefFont
	     ps-footer-font-size-internal
	     (ps-font 'ps-font-for-footer 'normal))
     "\n\n% ---- These lines must be kept together because...

/h0 F
/HeaderTitleLineHeight FontHeight def

/h1 F
/HeaderLineHeight FontHeight def
/HeaderDescent    Descent def

/H0 F
/FooterLineHeight FontHeight def
/FooterDescent    Descent def

% ---- ...because `F' has a side-effect on `FontHeight' and `Descent'\n\n")

    ;; Text fonts
    (let ((font (ps-font-alist 'ps-font-for-text))
	  (i 0))
      (while font
	(ps-output (format "/f%d %s(%s)cvn DefFont\n"
			   i
			   ps-font-size-internal
			   (ps-font 'ps-font-for-text (car (car font)))))
	(setq font (cdr font)
	      i (1+ i))))

    (let ((font-entry (cdr (assq ps-font-family ps-font-info-database))))
      (ps-output (format "/SpaceWidthRatio %f def\n"
			 (/ (ps-lookup 'space-width) (ps-lookup 'size)))))

    (unless (eq ps-spool-config 'lpr-switches)
      (ps-output "\n%%BeginFeature: *Duplex "
		 (ps-boolean-capitalized ps-spool-duplex)
		 " *Tumble "
		 (ps-boolean-capitalized tumble)
		 "\nUseSetpagedevice\n{BMark/Duplex "
		 (ps-boolean-constant ps-spool-duplex)
		 "/Tumble "
		 (ps-boolean-constant tumble)
		 " EMark setpagedevice}\n{statusdict begin "
		 (ps-boolean-constant ps-spool-duplex)
		 " setduplexmode "
		 (ps-boolean-constant tumble)
		 " settumble end}ifelse\n%%EndFeature\n")))
  (ps-output "\n%%BeginFeature: *ManualFeed "
	     (ps-boolean-capitalized ps-manual-feed)
	     "\nBMark /ManualFeed "
	     (ps-boolean-constant ps-manual-feed)
	     " EMark setpagedevice\n%%EndFeature\n\nBeginDoc\n%%EndSetup\n")
  (and ps-banner-page-when-duplexing
       (ps-output "\n%%Page: banner 0\nsave showpage restore\n")))


(defun ps-format-color (color &optional default)
  (let ((the-color (if (stringp color)
		       (ps-color-scale color)
		     color)))
    (if (and the-color (listp the-color))
	(concat "["
		(format ps-color-format
			(* (nth 0 the-color) 1.0) ; force float number
			(* (nth 1 the-color) 1.0) ; force float number
			(* (nth 2 the-color) 1.0)) ; force float number
		"] ")
      (ps-float-format (if (numberp the-color) the-color default)))))


(defun ps-insert-string (prologue)
  (let ((str (if (functionp prologue)
		 (funcall prologue)
	       prologue)))
    (and (stringp str)
	 (ps-output str))))


(defun ps-boolean-capitalized (bool)
  (if bool "True" "False"))


(defun ps-boolean-constant (bool)
  (if bool "true" "false"))


(defun ps-header-dirpart ()
  (let ((fname (buffer-file-name)))
    (if fname
	(if (string-equal (buffer-name) (file-name-nondirectory fname))
	    (abbreviate-file-name (file-name-directory fname))
	  fname)
      "")))


(defun ps-get-buffer-name ()
  (cond
   ;; Indulge Jim this little easter egg:
   ((string= (buffer-name) "ps-print.el")
    "Hey, Cool!  It's ps-print.el!!!")
   ;; Indulge Jack this other little easter egg:
   ((string= (buffer-name) "sokoban.el")
    "Super! C'est sokoban.el!")
   (t (concat
       (and ps-printing-region-p "Subset of: ")
       (buffer-name)
       (and (buffer-modified-p) " (unsaved)")))))


(defun ps-get-size (size mess &optional arg)
  (let ((siz (cond ((numberp size)
		    size)
		   ((and (consp size)
			 (numberp (car size))
			 (numberp (cdr size)))
		    (if ps-landscape-mode
			(car size)
		      (cdr size)))
		   (t
		    -1))))
    (and (< siz 0)
	 (error "Invalid %s `%S'%s"
		mess size
		(if arg
		    (format " for `%S'" arg)
		  "")))
    siz))


(defun ps-get-font-size (font-sym)
  (ps-get-size (symbol-value font-sym) "font size" font-sym))


(defun ps-rgb-color (color unspecified default)
  (cond
   ;; (float float float) ==> (R G B)
   ((and color (listp color) (= (length color) 3)
	 (let ((cl color)
	       (ok t) e)
	   (while (and ok cl)
	     (setq e  (car cl)
		   cl (cdr cl)
		   ok (and (floatp e) (<= 0.0 e) (<= e 1.0))))
	   ok))
    color)
   ;; float ==> 0.0 = black .. 1.0 = white
   ((and (floatp color) (<= 0.0 color) (<= color 1.0))
    (list color color color))
   ;; "colorName" but different from "unspecified-[bf]g"
   ((and (stringp color) (not (string= color unspecified)))
    (ps-color-scale color))
   ;; ok, use the default
   (t
    (list default default default))))

(defvar ps-basic-plot-string-function 'ps-basic-plot-string)

(defun ps-begin-job (genfunc)
  ;; prologue files
  (or (equal ps-mark-code-directory ps-postscript-code-directory)
      (setq ps-print-prologue-0    (ps-prologue-file 0)
	    ps-print-prologue-1    (ps-prologue-file 1)
	    ps-mark-code-directory ps-postscript-code-directory))
  ;; selected pages
  (let (new page)
    (while ps-selected-pages
      (setq page              (car ps-selected-pages)
	    ps-selected-pages (cdr ps-selected-pages))
      (cond ((integerp page)
	     (and (> page 0)
		  (setq new (cons (cons page page) new))))
	    ((consp page)
	     (and (integerp (car page)) (integerp (cdr page))
		  (> (car page) 0)
		  (<= (car page) (cdr page))
		  (setq new (cons page new))))))
    (setq ps-selected-pages      (sort new #'(lambda (one other)
					       (< (car one) (car other))))
	  ps-last-selected-pages ps-selected-pages
	  ps-first-page          nil
	  ps-last-page           nil))
  ;; face background
  (or (listp ps-use-face-background)
      (setq ps-use-face-background t))
  ;; line number
  (and (integerp ps-line-number-step)
       (<= ps-line-number-step 0)
       (setq ps-line-number-step 1))
  (setq ps-n-up-on           (> ps-n-up-printing 1)
	ps-line-number-start (max 1 (min ps-line-number-start
					 (if (integerp ps-line-number-step)
					     ps-line-number-step
					   ps-zebra-stripe-height))))
  ;; spooling buffer
  (with-current-buffer ps-spool-buffer
    (goto-char (point-max))
    (and (re-search-backward "^%%Trailer$" nil t)
	 (delete-region (match-beginning 0) (point-max))))
  ;; miscellaneous
  (setq ps-zebra-stripe-full-p (memq ps-zebra-stripe-follow
				     '(full full-follow))
	ps-page-postscript 0
	ps-page-sheet 0
	ps-page-n-up 0
	ps-page-column 0
	ps-lines-printed 0
	ps-print-page-p t
	ps-showline-count (car ps-printing-region)
	ps-line-spacing-internal      (ps-get-size ps-line-spacing
						   "line spacing")
	ps-paragraph-spacing-internal (ps-get-size ps-paragraph-spacing
						   "paragraph spacing")
	ps-font-size-internal        (ps-get-font-size 'ps-font-size)
	ps-header-font-size-internal (ps-get-font-size 'ps-header-font-size)
	ps-header-title-font-size-internal
	(ps-get-font-size 'ps-header-title-font-size)
	ps-footer-font-size-internal (ps-get-font-size 'ps-footer-font-size)
	ps-control-or-escape-regexp
	(cond ((eq ps-print-control-characters '8-bit)
	       (string-as-unibyte "[\000-\037\177-\377]"))
	      ((eq ps-print-control-characters 'control-8-bit)
	       (string-as-unibyte "[\000-\037\177-\237]"))
	      ((eq ps-print-control-characters 'control)
	       "[\000-\037\177]")
	      (t "[\t\n\f]"))
	;; Set the color scale.  We do it here instead of in the defvar so
	;; that ps-print can be dumped into emacs.  This expression can't be
	;; evaluated at dump-time because X isn't initialized.
	ps-color-p            (and ps-print-color-p (ps-color-device))
	ps-print-color-scale  (if ps-color-p
				  (float (car (ps-color-values "white")))
				1.0)
	ps-default-background (ps-rgb-color
			       (cond
				((or (member ps-print-color-p
					     '(nil back-white))
				     (eq genfunc 'ps-generate-postscript))
				 nil)
				((eq ps-default-bg 'frame-parameter)
				 (ps-frame-parameter nil 'background-color))
				((eq ps-default-bg t)
				 (ps-face-background-name 'default))
				(t
				 ps-default-bg))
			       "unspecified-bg"
			       1.0)
	ps-default-foreground (ps-rgb-color
			       (cond
				((or (member ps-print-color-p
					     '(nil back-white))
				     (eq genfunc 'ps-generate-postscript))
				 nil)
				((eq ps-default-fg 'frame-parameter)
				 (ps-frame-parameter nil 'foreground-color))
				((eq ps-default-fg t)
				 (ps-face-foreground-name 'default))
				(t
				 ps-default-fg))
			       "unspecified-fg"
			       0.0)
	ps-foreground-list    (mapcar
			       #'(lambda (arg)
				   (ps-rgb-color arg "unspecified-fg" 0.0))
			       (append (and (not (member ps-print-color-p
							 '(nil back-white)))
					    ps-fg-list)
				       (list ps-default-foreground
					     "black")))
	ps-default-color      (and (not (member ps-print-color-p
						'(nil back-white)))
				   ps-default-foreground)
	ps-current-color      ps-default-color
	;; Set up default functions.
	;; They may be overridden by ps-mule-begin-job.
	ps-basic-plot-string-function    'ps-basic-plot-string
	ps-encode-header-string-function nil)
  ;; initialize page dimensions
  (ps-get-page-dimensions)
  ;; final check
  (unless (listp ps-lpr-switches)
    (error "`ps-lpr-switches' value should be a list"))
  (and ps-color-p
       (equal ps-default-background ps-default-foreground)
       (error
	(concat
	 "`ps-default-fg' and `ps-default-bg' have the same color.\n"
	 "Text won't appear on page.  Please, check these variables."))))


(defun ps-page-number ()
  (if ps-print-only-one-header
      (1+ (/ (1- ps-page-column) ps-number-of-columns))
    ps-page-column))


(defsubst ps-end-page ()
  (ps-output "EndPage\nEndDSCPage\n"))


(defsubst ps-next-page ()
  (ps-end-page)
  (ps-flush-output)
  (ps-begin-page))


(defun ps-end-sheet ()
  (and ps-print-page-p (> ps-page-sheet 0)
       (ps-output "EndSheet\n")))


(defun ps-header-sheet ()
  ;; Print only when a new sheet begins.
  (ps-end-sheet)
  (setq ps-page-sheet (1+ ps-page-sheet))
  (when (ps-print-sheet-p)
    (setq ps-page-order (1+ ps-page-order))
    (ps-output (if ps-n-up-on
		   (format "\n%%%%Page: (%d \\(%d\\)) %d\n"
			   ps-page-order ps-page-postscript ps-page-order)
		 (format "\n%%%%Page: %d %d\n"
			 ps-page-postscript ps-page-order))
	       ;; spooling needs to redefine Lines and PageCount on each page
	       "/Lines 0 def\n/PageCount 0 def\n"
	       (format "%d BeginSheet\nBeginDSCPage\n"
		       ps-n-up-printing))))


(defun ps-header-page ()
  ;; set total line and page number when printing has finished
  ;; (see `ps-generate')
  (if (zerop (mod ps-page-column ps-number-of-columns))
      (progn
	(setq ps-page-postscript (1+ ps-page-postscript))
	(when (ps-print-page-p)
	  (ps-print-sheet-p)
	  (if (zerop (mod ps-page-n-up ps-n-up-printing))
	      ;; Print only when a new sheet begins.
	      (progn
		(ps-header-sheet)
		(run-hooks 'ps-print-begin-sheet-hook))
	    ;; Print only when a new page begins.
	    (ps-output "BeginDSCPage\n")
	    (run-hooks 'ps-print-begin-page-hook))
	  (ps-background ps-page-postscript)
	  (setq ps-page-n-up (1+ ps-page-n-up))
	  (and ps-print-page-p
	       (setq ps-page-printed (1+ ps-page-printed)))))
    ;; Print only when a new column begins.
    (ps-output "BeginDSCPage\n")
    (run-hooks 'ps-print-begin-column-hook))
  (setq ps-page-column (1+ ps-page-column)))

(defun ps-begin-page ()
  (setq ps-width-remaining  ps-print-width
	ps-height-remaining ps-print-height)

  (ps-header-page)

  (ps-output (format "/LineNumber %d def\n" ps-showline-count)
	     (format "/PageNumber %d def\n" (ps-page-number)))

  (when ps-print-header
    (ps-generate-header "HeaderLinesLeft"  "/h0" "/h1" ps-left-header)
    (ps-generate-header "HeaderLinesRight" "/h0" "/h1" ps-right-header)
    (ps-output (format "%d SetHeaderLines\n" ps-header-lines)))

  (when ps-print-footer
    (ps-generate-header "FooterLinesLeft"  "/H0" "/H0" ps-left-footer)
    (ps-generate-header "FooterLinesRight" "/H0" "/H0" ps-right-footer)
    (ps-output (format "%d SetFooterLines\n" ps-footer-lines)))

  (ps-output (number-to-string ps-lines-printed) " BeginPage\n")
  (ps-set-font  ps-current-font)
  (ps-set-bg    ps-current-bg)
  (ps-set-color ps-current-color))

(defsubst ps-skip-newline (limit)
  (setq ps-showline-count (1+ ps-showline-count)
	ps-lines-printed  (1+ ps-lines-printed))
  (and (< (point) limit)
       (forward-char 1)))

(defsubst ps-next-line ()
  (setq ps-showline-count (1+ ps-showline-count)
	ps-lines-printed  (1+ ps-lines-printed))
  (let* ((paragraph-p (and ps-paragraph-regexp
			   (looking-at ps-paragraph-regexp)))
	 (lh (+ (ps-line-height 'ps-font-for-text)
		(if paragraph-p
		    ps-paragraph-spacing-internal
		  ps-line-spacing-internal))))
    (if (< ps-height-remaining lh)
	(ps-next-page)
      (setq ps-width-remaining  ps-print-width
	    ps-height-remaining (- ps-height-remaining lh))
      (ps-output (if paragraph-p "PHL\n" "LHL\n")))))

(defun ps-continue-line ()
  (setq ps-lines-printed (1+ ps-lines-printed))
  (let ((lh (+ (ps-line-height 'ps-font-for-text) ps-line-spacing-internal)))
    (if (< ps-height-remaining lh)
	(ps-next-page)
      (setq ps-width-remaining  ps-print-width
	    ps-height-remaining (- ps-height-remaining lh))
      (ps-output "SL\n"))))

(defun ps-find-wrappoint (from to char-width)
  (let ((avail (truncate (/ ps-width-remaining char-width)))
	(todo (- to from)))
    (if (< todo avail)
	(cons to (* todo char-width))
      (cons (+ from avail) ps-width-remaining))))

(defun ps-basic-plot-str (from to string)
  (let* ((wrappoint (ps-find-wrappoint from to
				       (ps-avg-char-width 'ps-font-for-text)))
	 (to (car wrappoint))
	 (str (substring string from to)))
    (ps-output-string str)
    (ps-output " S\n")
    wrappoint))

(defun ps-basic-plot-string (from to &optional _bg-color)
  (let* ((wrappoint (ps-find-wrappoint from to
				       (ps-avg-char-width 'ps-font-for-text)))
	 (to (car wrappoint))
	 (string (buffer-substring-no-properties from to)))
    (ps-output-string string)
    (ps-output " S\n")
    wrappoint))

(defun ps-basic-plot-whitespace (from to &optional _bg-color)
  (let* ((wrappoint (ps-find-wrappoint from to
				       (ps-space-width 'ps-font-for-text)))
	 (to (car wrappoint)))
    (ps-output (format "%d W\n" (- to from)))
    wrappoint))

(defun ps-plot (plotfunc from to &optional bg-color)
  (while (< from to)
    (let* ((wrappoint (funcall plotfunc from to bg-color))
	   (plotted-to (car wrappoint))
	   (plotted-width (cdr wrappoint)))
      (setq from plotted-to
	    ps-width-remaining (- ps-width-remaining plotted-width))
      (if (< from to)
	  (ps-continue-line))))
  (if ps-razzle-dazzle
      (let* ((q-todo (- (point-max) (point-min)))
	     (q-done (- (point) (point-min)))
	     (chunkfrac (/ q-todo 8))
	     (chunksize (min chunkfrac 1000)))
	(if (> (- q-done ps-razchunk) chunksize)
	    (progn
	      (setq ps-razchunk q-done)
	      (message "Formatting...%3d%%"
		       (if (< q-todo 100)
			   (/ (* 100 q-done) q-todo)
			 (/ q-done (/ q-todo 100)))
		       ))))))

(defvar ps-last-font nil)

(defun ps-set-font (font)
  (setq ps-last-font (format "f%d" (setq ps-current-font font)))
  (ps-output (format "/%s F\n" ps-last-font)))

(defun ps-set-bg (color)
  (if (setq ps-current-bg color)
      (ps-output (format ps-color-format
			 (nth 0 color) (nth 1 color) (nth 2 color))
		 " true BG\n")
    (ps-output "false BG\n")))

(defun ps-set-color (color)
  (setq ps-current-color (or color ps-default-foreground))
  (ps-output (format ps-color-format
		     (nth 0 ps-current-color)
		     (nth 1 ps-current-color) (nth 2 ps-current-color))
	     " FG\n"))


(defsubst ps-plot-string (string)
  (ps-plot 'ps-basic-plot-str 0 (length string) string))


(defvar ps-current-effect 0)

(defvar ps-print-translation-table
  (let ((tbl (make-char-table 'translation-table nil)))
    (if (and (boundp 'ucs-mule-8859-to-mule-unicode)
	   (char-table-p ucs-mule-8859-to-mule-unicode))
	(map-char-table
	 #'(lambda (k v)
	     (if (and v (eq (char-charset v) 'latin-iso8859-1) (/= k v))
		 (aset tbl k v)))
	 ucs-mule-8859-to-mule-unicode))
    tbl)
  "Translation table for PostScript printing.
The default value is a table that translates non-Latin-1 Latin characters
to the equivalent Latin-1 characters.")

(defun ps-plot-region (from to font &optional fg-color bg-color effects)
  (or (equal font ps-current-font)
      (ps-set-font font))

  ;; Specify a foreground color only if:
  ;;    one's specified,
  ;;    it's different than the background (if `ps-fg-validate-p' is non-nil)
  ;;    and it's different than the current.
  (let ((fg (or fg-color ps-default-foreground)))
    (if ps-fg-validate-p
	(let ((bg (or bg-color ps-default-background))
	      (el ps-foreground-list))
	  (while (and el (equal fg bg))
	    (setq fg (car el)
		  el (cdr el)))))
    (or (equal fg ps-current-color)
	(ps-set-color fg)))

  (or (equal bg-color ps-current-bg)
      (ps-set-bg bg-color))

  ;; Specify effects (underline, overline, box, etc.)
  (cond
   ((not (integerp effects))
    (ps-output "0 EF\n")
    (setq ps-current-effect 0))
   ((/= effects ps-current-effect)
    (ps-output (number-to-string effects) " EF\n")
    (setq ps-current-effect effects)))

  ;; Starting at the beginning of the specified region...
  (save-excursion
    (goto-char from)

    ;; ...break the region up into chunks separated by tabs, linefeeds,
    ;; pagefeeds, control characters, and plot each chunk.
    (while (< from to)
      ;; skip lines between cut markers
      (and ps-begin-cut-regexp ps-end-cut-regexp
	   (looking-at ps-begin-cut-regexp)
	   (progn
	     (goto-char (match-end 0))
	     (and (re-search-forward ps-end-cut-regexp to 'noerror)
		  (= (following-char) ?\n)
		  (forward-char 1))
	     (setq from (point))))
      (if (re-search-forward ps-control-or-escape-regexp to t)
	  ;; region with some control characters or some multi-byte characters
	  (let* ((match-point (match-beginning 0))
		 (match       (char-after match-point)))
	    (when (< from match-point)
	      (ps-plot ps-basic-plot-string-function
		       from match-point bg-color))
	    (cond
	     ((= match ?\t)		; tab
	      (let ((linestart (line-beginning-position)))
		(forward-char -1)
		(setq from (+ linestart (current-column)))
		(when (re-search-forward "[ \t]+" to t)
		  (ps-plot 'ps-basic-plot-whitespace
			   from (+ linestart (current-column))
			   bg-color))))

	     ((= match ?\n)		; newline
	      (if (looking-at "\f[^\n]")
		  ;; \n\ftext\n ==>> next page, but keep line counting!!
		  (progn
		    (ps-skip-newline to)
		    (ps-next-page))
		;; \n\f\n     ==>> it'll be handled by form feed
		;; \ntext\n   ==>> next line
		(ps-next-line)))

	     ((= match ?\f)		; form feed
	      ;; do not skip page if previous character is NEWLINE and
	      ;; it is a beginning of page.
	      (unless (and (equal (char-after (1- match-point)) ?\n)
			   (= ps-height-remaining ps-print-height))
		;; \f\n ==>> skip \n, but keep line counting!!
		(and (equal (following-char) ?\n)
		     (ps-skip-newline to))
		(ps-next-page)))

	     (t				; characters from 127 to 255
	      (ps-control-character match)))
	    (setq from (point)))
	;; region without control characters
	(ps-plot ps-basic-plot-string-function from to bg-color)
	(setq from to)))))

(defvar ps-string-control-codes
  (let ((table (make-vector 256 nil))
	(char ?\000))
    ;; control character
    (while (<= char ?\037)
      (aset table char (format "^%c" (+ char ?@)))
      (setq char (1+ char)))
    ;; printable character
    (while (< char ?\177)
      (aset table char (format "%c" char))
      (setq char (1+ char)))
    ;; DEL
    (aset table char "^?")
    ;; 8-bit character
    (while (<= (setq char (1+ char)) ?\377)
      (aset table char (format "\\%o" char)))
    table)
  "Vector used to map characters to a printable string.")

(defun ps-control-character (char)
  (let* ((str (aref ps-string-control-codes char))
	 (from (1- (point)))
	 (len (length str))
	 (to (+ from len))
	 (char-width (ps-avg-char-width 'ps-font-for-text))
	 (wrappoint (ps-find-wrappoint from to char-width)))
    (if (< (car wrappoint) to)
	(ps-continue-line))
    (setq ps-width-remaining (- ps-width-remaining (* len char-width)))
    (ps-output-string str)
    (ps-output " S\n")))


(defsubst ps-face-foreground-color-p (attr)
  (memq attr '(foreground-color :foreground)))


(defsubst ps-face-background-color-p (attr)
  (memq attr '(background-color :background)))


(defsubst ps-face-color-p (attr)
  (memq attr '(foreground-color :foreground background-color :background)))


(defun ps-face-extract-color (face-attrs)
  (let ((color (cdr face-attrs)))
    (if (listp color)
	(car color)
      color)))


(defun ps-face-attributes (face)
  "Return face attribute vector.

If FACE is not in `ps-print-face-extension-alist' or in
`ps-print-face-alist', insert it on `ps-print-face-alist' and
return the attribute vector.

If FACE is not a valid face name, use default face."
  (and (stringp face) (facep face) (setq face (intern face)))
  (cond
   (ps-black-white-faces-alist
    (or (and (symbolp face)
	     (cdr (assq face ps-black-white-faces-alist)))
	(vector 0 nil nil)))
   ((symbolp face)
    (cdr (or (assq face ps-print-face-extension-alist)
	     (assq face ps-print-face-alist)
	     (let* ((the-face (if (facep face) face 'default))
		    (new-face (ps-screen-to-bit-face the-face)))
	       (or (and (eq the-face 'default)
			(assq the-face ps-print-face-alist))
		   (setq ps-print-face-alist
			 (cons new-face ps-print-face-alist)))
	       new-face))))
   ((ps-face-foreground-color-p (car face))
    (vector 0 (ps-face-extract-color face) nil))
   ((ps-face-background-color-p (car face))
    (vector 0 nil (ps-face-extract-color face)))
   (t
    (vector 0 nil nil))))


(defun ps-face-background (face background)
  (and (cond ((eq ps-use-face-background t))	 ; always
	     ((null ps-use-face-background) nil) ; never
	     ;; ps-user-face-background is a symbol face list
	     ((symbolp face)
	      (memq face ps-use-face-background))
	     ((listp face)
	      (or (ps-face-color-p (car face))
		  (let (ok)
		    (while face
		      (if (or (memq (car face) ps-use-face-background)
			      (ps-face-color-p (car face)))
			  (setq face nil
				ok   t)
			(setq face (cdr face))))
		    ok)))
	     (t
	      nil)
	     )
       background))


(defun ps-face-attribute-list (face-or-list)
  (cond
   ;; simple face
   ((not (listp face-or-list))
    (ps-face-attributes face-or-list))
   ;; only foreground color, not a `real' face
   ((ps-face-foreground-color-p (car face-or-list))
    (vector 0 (ps-face-extract-color face-or-list) nil))
   ;; only background color, not a `real' face
   ((ps-face-background-color-p (car face-or-list))
    (vector 0 nil (ps-face-extract-color face-or-list)))
   ;; list of faces
   (t
    (let ((effects 0)
	  foreground background face-attr face)
      (while face-or-list
	(setq face         (car face-or-list)
	      face-or-list (cdr face-or-list)
	      face-attr    (ps-face-attributes face)
	      effects      (logior effects (aref face-attr 0)))
	(or foreground (setq foreground (aref face-attr 1)))
	(or background
	    (setq background (ps-face-background face (aref face-attr 2)))))
      (vector effects foreground background)))))


(defconst ps-font-type (vector nil 'bold 'italic 'bold-italic))


(defun ps-plot-with-face (from to face)
  (cond
   ((null face)				; print text with null face
    (ps-plot-region from to 0))
   ((eq face 'emacs--invisible--face))	; skip invisible text!!!
   (t					; otherwise, text has a valid face
    (let* ((face-bit   (ps-face-attribute-list face))
	   (effect     (aref face-bit 0))
	   (foreground (aref face-bit 1))
	   (background (ps-face-background face (aref face-bit 2)))
	   (fg-color (if (and ps-color-p foreground)
			 (ps-color-scale foreground)
		       ps-default-color))
	   (bg-color (and ps-color-p background
			  (ps-color-scale background))))
      (ps-plot-region
       from to
       (ps-font-number 'ps-font-for-text
		       (or (aref ps-font-type (logand effect 3))
			   face))
       fg-color bg-color (lsh effect -2)))))
  (goto-char to))


;; Ensure that face-list is fbound.
(or (fboundp 'face-list) (defalias 'face-list 'list-faces))


(defun ps-build-reference-face-lists ()
  ;; Ensure that face database is updated with faces on
  ;; `font-lock-face-attributes' (obsolete stuff)
  (ps-font-lock-face-attributes)
  ;; Now, rebuild reference face lists
  (setq ps-print-face-alist nil)
  (if ps-auto-font-detect
      (mapc 'ps-map-face (face-list))
    (mapc 'ps-set-face-bold ps-bold-faces)
    (mapc 'ps-set-face-italic ps-italic-faces)
    (mapc 'ps-set-face-underline ps-underlined-faces))
  (setq ps-build-face-reference nil))


(defun ps-set-face-bold (face)
  (ps-set-face-attribute face 1))

(defun ps-set-face-italic (face)
  (ps-set-face-attribute face 2))

(defun ps-set-face-underline (face)
  (ps-set-face-attribute face 4))


(defun ps-set-face-attribute (face effect)
  (let ((face-bit (cdr (ps-map-face face))))
    (aset face-bit 0 (logior (aref face-bit 0) effect))))


(defun ps-map-face (face)
  (let* ((face-map (ps-screen-to-bit-face face))
	 (ps-face-bit (cdr (assq (car face-map) ps-print-face-alist))))
    (if ps-face-bit
	;; if face exists, merge both
	(let ((face-bit (cdr face-map)))
	  (aset ps-face-bit 0 (logior (aref ps-face-bit 0) (aref face-bit 0)))
	  (or (aref ps-face-bit 1) (aset ps-face-bit 1 (aref face-bit 1)))
	  (or (aref ps-face-bit 2) (aset ps-face-bit 2 (aref face-bit 2))))
      ;; if face does not exist, insert it
      (setq ps-print-face-alist (cons face-map ps-print-face-alist)))
    face-map))


(defun ps-screen-to-bit-face (face)
  (cons face
	(vector (logior (if (ps-face-bold-p face)       1 0)  ; bold
			(if (ps-face-italic-p face)     2 0)  ; italic
			(if (ps-face-underlined-p face) 4 0)  ; underline
			(if (ps-face-strikeout-p face)  8 0)  ; strikeout
			(if (ps-face-overline-p face)  16 0)  ; overline
			(if (ps-face-box-p face)       64 0)) ; box
		(ps-face-foreground-name face)
		(ps-face-background-name face))))


(declare-function jit-lock-fontify-now "jit-lock" (&optional start end))
(declare-function lazy-lock-fontify-region "lazy-lock" (beg end))

;; to avoid compilation gripes
(defun ps-print-ensure-fontified (start end)
  (cond ((and (boundp 'jit-lock-mode) (symbol-value 'jit-lock-mode))
	 (jit-lock-fontify-now start end))
	((and (boundp 'lazy-lock-mode) (symbol-value 'lazy-lock-mode))
	 (lazy-lock-fontify-region start end))))


(defun ps-generate-postscript-with-faces (from to)
  ;; Some initialization...
  (setq ps-current-effect 0)

  ;; Build the reference lists of faces if necessary.
  (when (or ps-always-build-face-reference
	    ps-build-face-reference)
    (message "Collecting face information...")
    (ps-build-reference-face-lists))

  ;; Black/white printer.
  (setq ps-black-white-faces-alist nil)
  (and (eq ps-print-color-p 'black-white)
       (ps-extend-face-list ps-black-white-faces nil
			    'ps-black-white-faces-alist))

  ;; Generate some PostScript.
  (save-restriction
    (narrow-to-region from to)
    (ps-print-ensure-fontified from to)
    (ps-generate-postscript-with-faces1 from to)))

(defun ps-generate-postscript (from to)
  (ps-plot-region from to 0))

;; These are autoloaded, but ps-mule generates autoloads at the end of
;; this file, so they are unknown at this point when compiling.
(declare-function ps-mule-initialize "ps-mule" ())
(declare-function ps-mule-begin-job  "ps-mule" (from to))
(declare-function ps-mule-end-job    "ps-mule" ())

(defun ps-generate (buffer from to genfunc)
  (save-excursion
    (let ((from (min to from))
	  (to (max to from))
	  ;; This avoids trouble if chars with read-only properties
	  ;; are copied into ps-spool-buffer.
	  (inhibit-read-only t))
      (save-restriction
	(narrow-to-region from to)
	(and ps-razzle-dazzle
	     (message "Formatting...%3d%%" (setq ps-razchunk 0)))
	(setq ps-source-buffer buffer
	      ps-spool-buffer (get-buffer-create ps-spool-buffer-name))
	(ps-init-output-queue)
	(let (safe-marker completed-safely needs-begin-file)
	  (unwind-protect
	      (progn
		(set-buffer ps-spool-buffer)
		(set-buffer-multibyte nil)

		;; Get a marker and make it point to the current end of the
		;; buffer,  If an error occurs, we'll delete everything from
		;; the end of this marker onwards.
		(setq safe-marker (make-marker))
		(set-marker safe-marker (point-max))

		(goto-char (point-min))
		(or (looking-at (regexp-quote ps-adobe-tag))
		    (setq needs-begin-file t))

		(set-buffer ps-source-buffer)
		(save-excursion
		  (let ((ps-print-page-p t)
			ps-even-or-odd-pages)
		    (ps-begin-job genfunc)
		    (when needs-begin-file
		      (ps-begin-file)
		      (ps-mule-initialize))
		    (ps-mule-begin-job from to)
		    (ps-selected-pages)))
		(ps-begin-page)
		(funcall genfunc from to)
		(ps-end-page)
		(ps-mule-end-job)
		(ps-end-job needs-begin-file)

		;; Setting this variable tells the unwind form that the
		;; the PostScript was generated without error.
		(setq completed-safely t))

	    ;; Unwind form: If some bad mojo occurred while generating
	    ;; PostScript, delete all the PostScript that was generated.
	    ;; This protects the previously spooled files from getting
	    ;; corrupted.
	    (and (markerp safe-marker) (not completed-safely)
		 (progn
		   (set-buffer ps-spool-buffer)
		   (delete-region (marker-position safe-marker) (point-max))))))

	(and ps-razzle-dazzle (message "Formatting...done"))))))


(defun ps-end-job (needs-begin-file)
  (let ((ps-print-page-p t))
    (ps-flush-output)
    (save-excursion
      (let ((pages-per-sheet (mod ps-page-printed ps-n-up-printing))
	    (total-lines (cdr ps-printing-region))
	    (total-pages (ps-page-number)))
	(set-buffer ps-spool-buffer)
	(let (case-fold-search)
	  ;; Back to the PS output buffer to set the last page n-up printing
	  (goto-char (point-max))
	  (and (> pages-per-sheet 0)
	       (re-search-backward "^[0-9]+ BeginSheet$" nil t)
	       (replace-match (format "%d BeginSheet" pages-per-sheet) t))
	  ;; Back to the PS output buffer to set the page count
	  (goto-char (point-min))
	  (while (re-search-forward "^/Lines 0 def\n/PageCount 0 def$" nil t)
	    (replace-match (format "/Lines %d def\n/PageCount %d def"
				   total-lines total-pages) t)))))
    ;; Set dummy page
    (and ps-spool-duplex (= (mod ps-page-order 2) 1)
	 (let ((ps-n-up-printing 0))
	   (ps-header-sheet)
	   (ps-output "/PrintHeader false def\n/ColumnIndex 0 def\n"
		      "/PrintLineNumber false def\n"
		      (number-to-string ps-lines-printed) " BeginPage\n")
	   (ps-end-page)))
    ;; Set end of PostScript file
    (ps-end-sheet)
    (ps-output "\n%%Trailer\n%%Pages: "
	       (number-to-string
		(if (and needs-begin-file
			 ps-banner-page-when-duplexing)
		    (1+ ps-page-order)
		  ps-page-order))
	       "\n\nEndDoc\n\n%%EOF\n")
    (and ps-end-with-control-d
	 (ps-output "\C-d"))
    (ps-flush-output))
  ;; disable selected pages
  (setq ps-selected-pages nil))


;; Permit dynamic evaluation at print time of `ps-lpr-switches'.
(defun ps-do-despool (filename)
  (if (or (not (boundp 'ps-spool-buffer))
	  (not (symbol-value 'ps-spool-buffer)))
      (message "No spooled PostScript to print")
    (if filename
	(save-excursion
	  (and ps-razzle-dazzle (message "Saving..."))
	  (set-buffer ps-spool-buffer)
	  (setq filename (expand-file-name filename))
	  (let ((coding-system-for-write 'raw-text-unix))
	    (write-region (point-min) (point-max) filename))
	  (and ps-razzle-dazzle (message "Wrote %s" filename)))
      ;; Else, spool to the printer
      (and ps-razzle-dazzle (message "Printing..."))
      (with-current-buffer ps-spool-buffer
	(let* ((coding-system-for-write 'raw-text-unix)
	       (ps-printer-name (or ps-printer-name
				    (and (boundp 'printer-name)
					 (symbol-value 'printer-name))))
	       (ps-lpr-switches
		(append ps-lpr-switches
			(and (stringp ps-printer-name)
			     (string< "" ps-printer-name)
			     (list (concat
				    (and (stringp ps-printer-name-option)
					 ps-printer-name-option)
				    ps-printer-name))))))
	  (or (stringp ps-printer-name)
	      (setq ps-printer-name nil))
	  (apply (or ps-print-region-function 'call-process-region)
		 (point-min) (point-max) ps-lpr-command nil
		 (and (fboundp 'start-process) 0)
		 nil
		 (ps-flatten-list	; dynamic evaluation
		  (ps-string-list
		   (mapcar 'ps-eval-switch ps-lpr-switches))))))
      (and ps-razzle-dazzle (message "Printing...done")))
    (kill-buffer ps-spool-buffer)))

(defun ps-string-list (arg)
  (let (lstr)
    (dolist (elm arg)
      (cond ((stringp elm)
	     (setq lstr (cons elm lstr)))
	    ((listp elm)
	     (let ((s (ps-string-list elm)))
	       (when s
		 (setq lstr (cons s lstr)))))
	    (t )))			; ignore any other value
    (nreverse lstr)))

;; Dynamic evaluation
(defun ps-eval-switch (arg)
  (cond ((stringp arg) arg)
	((functionp arg) (apply arg nil))
	((symbolp arg) (symbol-value arg))
	((consp arg) (apply (car arg) (cdr arg)))
	(t nil)))

;; `ps-flatten-list' is defined here (copied from "message.el" and
;; enhanced to handle dotted pairs as well) until we can get some
;; sensible autoloads, or `flatten-list' gets put somewhere decent.

;; (ps-flatten-list '((a . b) c (d . e) (f g h) i . j))
;; => (a b c d e f g h i j)

(defun ps-flatten-list (&rest list)
  (ps-flatten-list-1 list))

(defun ps-flatten-list-1 (list)
  (cond ((null list) nil)
	((consp list) (append (ps-flatten-list-1 (car list))
			      (ps-flatten-list-1 (cdr list))))
	(t (list list))))

(defun ps-kill-emacs-check ()
  (let (ps-buffer)
    (and (setq ps-buffer (get-buffer ps-spool-buffer-name))
	 (buffer-name ps-buffer)	; check if it's not killed
	 (buffer-modified-p ps-buffer)
	 (y-or-n-p "Unprinted PostScript waiting; print now? ")
	 (ps-despool))
    (and (setq ps-buffer (get-buffer ps-spool-buffer-name))
	 (buffer-name ps-buffer)	; check if it's not killed
	 (buffer-modified-p ps-buffer)
	 (not (yes-or-no-p "Unprinted PostScript waiting; exit anyway? "))
	 (error "Unprinted PostScript"))))

(cond ((fboundp 'add-hook)
       (unless noninteractive
         (funcall 'add-hook 'kill-emacs-hook 'ps-kill-emacs-check)))
      (kill-emacs-hook
       (message "Won't override existing `kill-emacs-hook'"))
      (t
       (setq kill-emacs-hook 'ps-kill-emacs-check)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To make this file smaller, some commands go in a separate file.
;; But autoload them here to make the separation invisible.

;;;### (autoloads (ps-mule-end-job ps-mule-begin-job ps-mule-initialize
;;;;;;  ps-multibyte-buffer) "ps-mule" "ps-mule.el" "86bf8e46dac41afe73df5ab098038ab0")
;;; Generated autoloads from ps-mule.el

(defvar ps-multibyte-buffer nil "\
Specifies the multi-byte buffer handling.

Valid values are:

  nil			  This is the value to use the default settings;
			  by default, this only works to print buffers with
			  only ASCII and Latin characters.   But this default
			  setting can be changed by setting the variable
			  `ps-mule-font-info-database-default' differently.
			  The initial value of this variable is
			  `ps-mule-font-info-database-latin' (see
			  documentation).

  `non-latin-printer'	  This is the value to use when you have a Japanese
			  or Korean PostScript printer and want to print
			  buffer with ASCII, Latin-1, Japanese (JISX0208 and
			  JISX0201-Kana) and Korean characters.  At present,
			  it was not tested with the Korean characters
			  printing.  If you have a korean PostScript printer,
			  please, test it.

  `bdf-font'		  This is the value to use when you want to print
			  buffer with BDF fonts.  BDF fonts include both latin
			  and non-latin fonts.  BDF (Bitmap Distribution
			  Format) is a format used for distributing X's font
			  source file.  BDF fonts are included in
			  `intlfonts-1.2' which is a collection of X11 fonts
			  for all characters supported by Emacs.  In order to
			  use this value, be sure to have installed
			  `intlfonts-1.2' and set the variable
			  `bdf-directory-list' appropriately (see ps-bdf.el for
			  documentation of this variable).

  `bdf-font-except-latin' This is like `bdf-font' except that it uses
			  PostScript default fonts to print ASCII and Latin-1
			  characters.  This is convenient when you want or
			  need to use both latin and non-latin characters on
			  the same buffer.  See `ps-font-family',
			  `ps-header-font-family' and `ps-font-info-database'.

Any other value is treated as nil.")

(custom-autoload 'ps-multibyte-buffer "ps-mule" t)

(autoload 'ps-mule-initialize "ps-mule" "\
Initialize global data for printing multi-byte characters.

\(fn)" nil nil)

(autoload 'ps-mule-begin-job "ps-mule" "\
Start printing job for multi-byte chars between FROM and TO.
It checks if all multi-byte characters in the region are printable or not.

\(fn FROM TO)" nil nil)

(autoload 'ps-mule-end-job "ps-mule" "\
Finish printing job for multi-byte chars.

\(fn)" nil nil)

;;;***

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'ps-print)

;;; ps-print.el ends here
