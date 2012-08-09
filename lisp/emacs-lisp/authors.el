;;; authors.el --- utility for maintaining Emacs's AUTHORS file -*-coding: utf-8;-*-

;; Copyright (C) 2000-2012 Free Software Foundation, Inc.

;; Author: Gerd Moellmann <gerd@gnu.org>
;; Maintainer: Kim F. Storm <storm@cua.dk>
;; Keywords: maint
;; Package: emacs

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

;; Use M-x authors RET to create an *Authors* buffer that can used as
;; or merged with Emacs's AUTHORS file.

;;; Code:

(defvar authors-coding-system 'utf-8
  "Coding system used in the AUTHORS file.")

(defconst authors-many-files 20
  "Maximum number of files for which to print individual information.
If an author has modified more files, only the names of the most
frequently modified files are printed and a count of the additional
files.")

(defconst authors-aliases
  '(
    ("Aaron S. Hawley" "Aaron Hawley")
    ("Alexandru Harsanyi" "Alex Harsanyi")
    ("Andrew Csillag" "Drew Csillag")
    ("Anna M. Bigatti" "Anna Bigatti")
    ("Barry A. Warsaw" "Barry A. Warsaw, Century Computing, Inc."
     "Barry A. Warsaw, ITB" "Barry Warsaw")
    ("Bill Carpenter" "WJ Carpenter")
    ("Bill Mann" "William F. Mann")
    ("Bill Rozas" "Guillermo J. Rozas")
    ("Björn Torkelsson" "Bjorn Torkelsson")
    ("Brian Fox" "Brian J. Fox")
    ("Brian Sniffen" "Brian T. Sniffen")
    ("Christoph Wedler" "Christoph.Wedler@sap.com")
    ("Daniel Pfeiffer" "<Daniel.Pfeiffer@Informatik.START.db.de>"
     "<Daniel.Pfeiffer@Informatik.START.dbp.de>")
    ("David Abrahams" "Dave Abrahams")
    ("David De La Harpe Golden" "David Golden")
    ("David Gillespie" "Dave Gillespie")
    ("David Kågedal" "David K..edal")
    ("David M. Koppelman" "David M. Koppelman, Koppel@Ec?e.Lsu.Edu"
     "David Koppelman")
    ("David M. Smith" "David Smith" "David M Smith")
    ("David O'Toole" "David T. O'Toole")
    ("Deepak Goel" "D. Goel")
    ("Ed L. Cashin" "Ed L Cashin")
    ("Edward M. Reingold" "Ed Reingold" "Edward M Reingold"
     "Reingold Edward M")
    ("Eli Zaretskii" "eliz")
    ("Emilio C. Lopes" "Emilio Lopes")
    ("Era Eriksson" "Era@Iki.Fi")
    ("Eric M. Ludlam" "Eric Ludlam")
    ("Eric S. Raymond" "Eric Raymond")
    ("Eric Youngdale" "(Eric Youngdale at youngdale@v6550c.nrl.navy.mil)")
    ("Francis J. Wright" "Dr Francis J. Wright" "Francis Wright")
    ("François Pinard" "Francois Pinard")
    ("Francesco Potortì" "Francesco Potorti" "Francesco Potorti`")
    ("Frederic Pierresteguy" "Fred Pierresteguy")
    ("Geoff Voelker" "voelker")
    ("Gerd Möllmann" "Gerd Moellmann")
    ("Hallvard B. Furuseth" "Hallvard B Furuseth" "Hallvard Furuseth")
    ("Hrvoje Nikšić" "Hrvoje Niksic")
    ;; lisp/org/ChangeLog 2010-11-11.
    (nil "aaa bbb")
    ;; src/ChangeLog.4, 1994-01-11, since fixed.
;;;    (nil "(afs@hplb.hpl.hp.com)")
    ;; lisp/gnus/ChangeLog.1, 1998-01-15.
    ;; http://quimby.gnus.org/cgi-bin/cvsweb.cgi/gnus/lisp/gnus-art.el?rev=4.13
    (nil "<Use-Author-Address-Header@\\[127.1\\]>")
    (nil "Code Extracted") ; lisp/newcomment.el's "Author:" header
    (nil "\\`FSF")  ; FIXME what is this for - no effect?
    ;; lisp/gnus/ChangeLog.1, 1997-10-12, since fixed.
;;;    (nil "ISO-2022-JP")
    ("Jaeyoun Chung" "Jae-youn Chung" "Jae-you Chung" "Chung Jae-youn")
    ("Jan Djärv" "Jan D." "Jan Djarv")
    ("Jay K. Adams" "jka@ece.cmu.edu" "Jay Adams")
    ("Jérôme Marant" "Jérôme Marant" "Jerome Marant")
    ("Jens-Ulrik Holger Petersen" "Jens-Ulrik Petersen")
    ("Jeremy Bertram Maitin-Shepard" "Jeremy Maitin-Shepard")
    ("Johan Bockgård" "Johan Bockgard")
    ("John J Foerch" "John Foerch")
    ("John W. Eaton" "John Eaton")
    ("Jonathan I. Kamens" "Jonathan Kamens")
    ("Joseph Arceneaux" "Joe Arceneaux")
    ("Joseph M. Kelsey" "Joe Kelsey")	; FIXME ?
    ("Juan León Lahoz García" "Juan-Leon Lahoz Garcia")
    ("K. Shane Hartman" "Shane Hartman")
    ("Kai Großjohann" "Kai Grossjohann" "Kai Großjohann"
     "Kai.Grossjohann@Cs.Uni-Dortmund.De"
     "Kai.Grossjohann@Gmx.Net")
    ("Karl Berry" "K. Berry")
    ("Károly Lőrentey" "Károly Lőrentey" "Lőrentey Károly")
    ("Kazushi Marukawa" "Kazushi")
    ("Ken Manheimer" "Kenneth Manheimer")
    ("Kenichi Handa" "Ken'ichi Handa" "Kenichi HANDA")
    ("Kevin Greiner" "Kevin J. Greiner")
    ("Kim F. Storm" "Kim Storm")
    ("Kyle Jones" "Kyle E. Jones")
    ("Lars Magne Ingebrigtsen" "Lars Ingebrigtsen")
    ("Marcus G. Daniels" "Marcus Daniels")
    ("Mark D. Baushke" "Mark D Baushke")
    ("Marko Kohtala" "Kohtala Marko")
    ("Agustín Martín" "Agustin Martin" "Agustín Martín Domingo")
    ("Martin Lorentzon" "Martin Lorentzson")
    ("Matt Swift" "Matthew Swift")
    ("Maxime Edouard Robert Froumentin" "Max Froumentin")
    ("Michael R. Mauger" "Michael Mauger")
    ("Michael D. Ernst" "Michael Ernst")
    ("Michaël Cadilhac" "Michael Cadilhac")
    ("Michael I. Bushnell" "Michael I Bushnell" "Michael I. Bushnell, P/Bsg")
    ("Michael R. Cook" "Michael Cook")
    ("Michael Sperber" "Michael Sperber \\[Mr. Preprocessor\\]")
    ("Mikio Nakajima" "Nakajima Mikio")
    ("Nelson Jose dos Santos Ferreira" "Nelson Ferreira")
    ("Noorul Islam" "Noorul Islam K M")
    ("Paul Eggert" "eggert")
    ("Paul Reilly" "(pmr@legacy.pajato.com)")
    ("Pavel Janík" "Pavel Janík Ml." "Pavel Janik Ml." "Pavel Janik" "Pavel Janík" "Pavel@Janik.Cz")
    ("Pavel Kobiakov" "Pavel Kobyakov")
    ("Per Abrahamsen" "Per Abhiddenware")
    ("Per Starbäck" "Per Starback")
    ("Peter J. Weisberg" "PJ Weisberg")
    ("Peter S. Galbraith" "Peter Galbraith")
    ("Peter Runestig" "Peter 'luna' Runestig")
    ("Peter S. Galbraith" "Peter S Galbraith")
    ("Raja R. Harinath" "Raja R Harinath")
    ("Richard G. Bielawski" "Richard G Bielawski" "Richard Bielawski")
    ("Richard King" "Dick King")
    ("Richard M. Stallman" "Richard M. Stallman,,," "Richard Stallman"
     "rms" "rms@gnu.org")
    ("Robert J. Chassell" "Bob Chassell")
    ("Roland B. Roberts" "Roland B Roberts" "Roland Roberts")
    ("Rui-Tao Dong" "Rui-Tao Dong ~{6-Hpln~}")
    ("Sacha Chua" "Sandra Jean Chua")
    ("Sam Steingold" "Sam Shteingold")
    ("Satyaki Das" "Indexed search by Satyaki Das")
    ("Sébastien Vauban" "Sebastien Vauban")
    ;; There are other Stefans.
;;;    ("Stefan Monnier" "Stefan")
    ("Stephen A. Wood" "(saw@cebaf.gov)")
    ("Steven L. Baur" "SL Baur" "Steven L Baur")
    ("Stewart M. Clamen" "Stewart Clamen")
    ("Stuart D. Herring" "Stuart Herring" "Davis Herring")
    ("T.V. Raman" "T\\. V\\. Raman")
    ("Taichi Kawabata" "KAWABATA,? Taichi")
    ("Takaaki Ota" "Tak Ota")
    ("Takahashi Naoto" "Naoto Takahashi")
    ("Teodor Zlatanov" "Ted Zlatanov")
    ("Thomas Dye" "Tom Dye")
    ("Thomas Horsley" "Tom Horsley")	; FIXME ?
    ("Thomas Wurgler" "Tom Wurgler")
    ("Toby Cubitt" "Toby S\\. Cubitt")
    ("Tomohiko Morioka" "MORIOKA Tomohiko")
    ("Torbjörn Axelsson" "Torbjvrn Axelsson")
    ("Torbjörn Einarsson" "Torbj.*rn Einarsson")
    ("Toru Tomabechi" "Toru Tomabechi,")
    ("Tsugutomo Enami" "enami tsugutomo")
    ("Vincent Del Vecchio" "Vince Del Vecchio")
    ("William M. Perry" "Bill Perry")
    ("Wlodzimierz Bzyl" "W.*dek Bzyl")
    ("Yoni Rabkin" "Yoni Rabkin Katzenell")
    ("Yoshinori Koseki" "KOSEKI Yoshinori" "小関 吉則")
    ("Yutaka NIIBE" "NIIBE Yutaka")
    )
  "Alist of author aliases.

Each entry is of the form (REALNAME REGEXP...).  If an author's name
matches one of the REGEXPs, use REALNAME instead.
If REALNAME is nil, ignore that author.")

;; FIXME seems it would be less fragile to check for O', Mc, etc.
(defconst authors-fixed-case
  '("Bryan O'Sullivan"
    "Christian von Roques"
    "Christophe de Dinechin"
    "Craig McDaniel"
    "David J. MacKenzie"
    "David McCabe"
    "David O'Toole"
    "Devon Sean McCullough"
    "Dominique de Waleffe"
    "Edward O'Connor"
    "Exal de Jesus Garcia Carrillo"
    "Greg McGary"
    "Hans de Graaff"
    "James TD Smith"
    "Joel N. Weber II"
    "Michael McNamara"
    "Mike McEwan"
    "Nelson Jose dos Santos Ferreira"
    "Peter von der Ahe"
    "Peter O'Gorman"
    "Piet van Oostrum"
    "Roland McGrath"
    "Sean O'Halpin"
    "Sean O'Rourke"
    "Tijs van Bakel")
  "List of authors whose names cannot be simply capitalized.")

(defvar authors-public-domain-files
  '("emerge\\.el"
    "vi\\.el"
    "feedmail\\.el"
    "mailpost\\.el"
    "hanoi\\.el"
    "meese\\.el"
    "studly\\.el"
    "modula2\\.el"
    "nnmaildir\\.el"
    "nnil\\.el"
    "b2m\\.c"
    "unexhp9k800\\.c"
    "emacsclient\\.1"
    "check-doc-strings")
  "List of regexps matching files for which the FSF doesn't need papers.")


(defvar authors-obsolete-files-regexps
  '("vc-\\*\\.el$"
    "spec.txt$"
    ".*loaddefs.el$"			; not obsolete, but auto-generated
    "\\.\\(cvs\\|git\\)ignore$"		; obsolete or uninteresting
    "\\.arch-inventory$"
    ;; TODO lib/? Matches other things?
    "build-aux/" "m4/" "Emacs.xcodeproj" "charsets" "mapfiles"
    "preferences\\.\\(nib\\|gorm\\)"
    "vc-\\(rcs\\|cvs\\|sccs\\)-hooks\\.el$")
  "List of regexps matching obsolete files.
Changes to files matching one of the regexps in this list are not listed.")

(defconst authors-ignored-files
  '("external-lisp"
    "lock" "share-lib" "local-lisp"
    "noleim-Makefile.in"
    "NEWS" "ORDERS" "PROBLEMS" "FAQ" "AUTHORS" "FOR-RELEASE" "TODO" "todo"
    "MACHINES" "SERVICE"
    "README.unicode" "README.multi-tty" "TUTORIAL.translators"
    "NEWS.unicode" "COPYING.DJ" "Makefile.old" "Makefile.am"
    "NEWS.1" "OOOOONEWS...OONEWS" "OOOONEWS" "etc/NEWS"
    "NEWS.1-17" "NEWS.18" "NEWS.19" "NEWS.20" "NEWS.21" "NEWS.22"
    "MAINTAINERS" "MH-E-NEWS"
    "install-sh" "missing" "mkinstalldirs"
    "termcap.dat" "termcap.src" "termcap.ucb" "termcap"
    "ChangeLog.nextstep" "Emacs.clr" "spec.txt"
    "gfdl.1"
    "texi/Makefile.in"
    "Imakefile" "icons/sink.ico" "aixcc.lex"
    "nxml/char-name/unicode"
    "js2-mode.el"      ; only installed very briefly, replaced by js.el
    "cedet/tests/testtemplates.cpp"
    "cedet/tests/testusing.cpp"
    "cedet/tests/scopetest.cpp"
    "cedet/tests/scopetest.java"
    "cedet/tests/test.cpp"
    "cedet/tests/test.py"
    "cedet/tests/teststruct.cpp"
    "*.el"
    ;; Autogen:
    "cus-load.el" "finder-inf.el" "ldefs-boot.el"
    "compile" "config.guess" "config.sub" "depcomp"
    ;; Only existed briefly, then renamed:
    "images/icons/allout-widgets-dark-bg"
    "images/icons/allout-widgets-light-bg"
    ;; Never had any meaningful changes logged, now deleted:
    "unidata/bidimirror.awk" "unidata/biditype.awk"
    "split-man" "Xkeymap.txt" "ms-7bkermit" "ulimit.hack"
    "gnu-hp300" "refcard.bit" "ledit.l" "forms.README" "forms-d2.dat"
    "CXTERM-DIC/PY.tit" "CXTERM-DIC/ZIRANMA.tit"
    "CXTERM-DIC/CTLau.tit" "CXTERM-DIC/CTLauB.tit"
    "NICKLES.WORTH" "INTERVAL.IDEAS" "RCP"
    "3B-MAXMEM" "AIX.DUMP" "SUN-SUPPORT" "XENIX"
    "CODINGS" "CHARSETS"
    "calc/INSTALL" "calc/Makefile"
    "vms-pp.trans" "_emacs" "batcomp.com" "notes/cpp" ; admin/
    "emacsver.texi.in"
    ;; MH-E stuff not in Emacs:
    "import-emacs" "release-utils"
    ;; Erc stuff not in Emacs:
    "ChangeLog.2001" "ChangeLog.2002" "ChangeLog.2003" "ChangeLog.2004"
    "ChangeLog.2005"
    "README.extras" "dir-template" "mkChangeLog" "MkChangeLog" "erc-auto.in"
    "CREDITS" "HACKING"
    "debian/changelog"
    "debian/control"
    "debian/copyright"
    "debian/maint/conffiles"
    "debian/maint/conffiles.in"
    "debian/maint/postinst"
    "debian/maint/postinst.in"
    "debian/maint/prerm"
    "debian/maint/prerm.in"
    "debian/README.Debian"
    "debian/README.erc-speak"
    "debian/rules"
    "debian/scripts/install"
    "debian/scripts/install.in"
    "debian/scripts/remove"
    "debian/scripts/remove.in"
    "debian/scripts/startup"
    "debian/scripts/startup.erc"
    "debian/scripts/startup.erc-speak"
    )
  "List of files and directories to ignore.
Changes to files in this list are not listed.")

;; List via: find . -name '*.el' | sed 's/.*\///g' | sort | uniq -d
;; FIXME It would be better to discover these dynamically.
;; Note that traditionally "Makefile.in" etc have not been in this list.
;; Ditto for "abbrev.texi" etc.
(defconst authors-ambiguous-files
  '("chart.el"
    "compile.el"
    "complete.el"
    "cpp.el"
    "ctxt.el"
    "custom.el"
    "cyrillic.el"
    "czech.el"
    "debug.el"
    "dired.el"
    "el.el"
    "eshell.el"
    "ethiopic.el"
    "f90.el"
    "files.el"
    "find.el"
    "format.el"
    "generic.el"
    "georgian.el"
    "greek.el"
    "grep.el"
    "hebrew.el"
    "imenu.el"
    "indian.el"
    "japanese.el"
    "java.el"
    "lao.el"
    "linux.el"
    "locate.el"
    "make.el"
    "mode.el"
    "python.el"
    "rmailmm.el"
    "semantic.el"
    "shell.el"
    "simple.el"
    "slovak.el"
    "sort.el"
    "speedbar.el"
    "srecode.el"
    "table.el"
    "texi.el"
    "thai.el"
    "tibetan.el"
    "util.el"
    "vc-bzr.el"
    "wisent.el")
  "List of basenames occurring more than once in the source.")

;; FIXME :cowrote entries here can be overwritten by :wrote entries
;; derived from a file's Author: header (eg mh-e).  This really means
;; the Author: header is erroneous.
(defconst authors-fixed-entries
  '(("Richard M. Stallman" :wrote "[The original GNU Emacs and numerous files]")
    ("Joseph Arceneaux" :wrote "xrdb.c")
    ;; This refers to the obsolete Willisson (qv) version.
;;;    ("Blitz Product Development Corporation" :wrote "ispell.el")
    ("Frank Bresz" :wrote "diff.el")
    ("David M. Brown" :wrote "array.el")
    ;; No longer distributed.
;;;    ("Gary Byers" :changed "xenix.h")
    ("Shawn M. Carey" :wrote "freebsd.h")
    ;; hp800.h renamed from hp9000s800.h, hpux.h merged into hpux10-20.h.
    ;; FIXME overwritten by Author:.
    ("Satyaki Das" :cowrote "mh-search.el")
    ("Eric Decker" :changed "hp800.h" "hpux10-20.h" "sysdep.c")
    ("Lawrence R. Dodd" :cowrote "dired-x.el")
    ;; No longer distributed.
;;;    ("Viktor Dukhovni" :wrote "unexsunos4.c")
    ("Paul Eggert" :wrote "rcs2log" "vcdiff")
    ("Fred Fish" :changed "unexcoff.c")
    ;; No longer distributed.
;;;    ("Tim Fleehart" :wrote "makefile.nt")
    ("Keith Gabryelski" :wrote "hexl.c")
    ("Kevin Gallagher" :wrote "flow-ctrl.el")
    ;; Also wrote an earlier version of disp-table.el, since replaced
    ;; by Erik Naggum's version; also iso-syntax.el, later renamed to
    ;; latin-1.el, since deleted.
    ("Howard Gayle" :wrote "casetab.c")
    ;; :wrote mh-pick.el, since merged into mh-search.el.
    ;; Originally wrote mh-funcs.el, but it has been rewritten since.
    ("Stephen Gildea" :wrote "refcard.tex"
     :cowrote "mh-funcs.el" "mh-search.el")
    ;; cl.texinfo renamed to cl.texi.
    ("David Gillespie" :wrote "cl.texi")
    ;; No longer distributed: emacsserver.c.
    ("Hewlett-Packard" :changed "emacsclient.c" "server.el" "keyboard.c")
    ;; No longer distributed.
;;;    ("Thomas Horsley" :wrote "cxux.h" "cxux7.h")
    ("Indiana University Foundation" :changed "buffer.c" "buffer.h"
     "indent.c" "search.c" "xdisp.c" "region-cache.c" "region-cache.h")
    ;; ibmrt.h, ibmrt-aix.h no longer distributed.
    ("International Business Machines" :changed "emacs.c" "fileio.c"
     "process.c" "sysdep.c" "unexcoff.c")
    ;; No longer distributed.
;;;    ("Ishikawa Chiaki" :changed "aviion.h" "dgux.h")
    ;; ymakefile no longer distributed.
    ("Michael K. Johnson" :changed "configure.in" "emacs.c" "intel386.h"
     "mem-limits.h" "process.c" "template.h" "sysdep.c" "syssignal.h"
     "systty.h" "unexcoff.c" "linux.h")
    ;; No longer distributed.
;;;    ("Kyle Jones" :wrote "mldrag.el")
    ("Henry Kautz" :wrote "bib-mode.el")
    ;; No longer distributed: vms-pwd.h, vmsfns.c, uaf.h.
    ("Joseph M. Kelsey" :changed "fileio.c" "dir.h")
    ("Sam Kendall" :changed "etags.c" "etags.el")
    ;; ack.texi: "We're not using his backquote.el any more."
    ("Richard King" :wrote "userlock.el" "filelock.c")
    ("Sebastian Kremer" :changed "add-log.el")
    ("Mark Lambert" :changed "process.c" "process.h")
    ("Aaron Larson" :changed "bibtex.el")
    ;; It was :wrote, but it has been rewritten since.
    ("James R. Larus" :cowrote "mh-e.el")
    ("Lars Lindberg" :changed "dabbrev.el" :cowrote "imenu.el")
    ;; No longer distributed: lselect.el.
    ("Lucid, Inc." :changed "bytecode.c" "byte-opt.el" "byte-run.el"
     "bytecomp.el" "delsel.el" "disass.el" "faces.el" "font-lock.el"
     "lmenu.el" "mailabbrev.el" "select.el" "xfaces.c" "xselect.c")
    ;; MCC.  No longer distributed: emacsserver.c.
    ("Microelectronics and Computer Technology Corporation"
     :changed "etags.c" "emacsclient.c" "movemail.c"
     "rmail.el" "rmailedit.el" "rmailkwd.el"
     "rmailmsc.el" "rmailout.el" "rmailsum.el" "scribe.el"
     ;; It was :wrote for xmenu.c, but it has been rewritten since.
     "server.el" "lisp.h" "sysdep.c" "unexcoff.c" "xmenu.c")
    ("Niall Mansfield" :changed "etags.c")
    ("Brian Marick" :cowrote "hideif.el")
    ("Marko Kohtala" :changed "info.el")
    ("Sidney Markowitz" :changed "doctor.el")
    ;; No longer distributed: env.c.
    ("Richard Mlynarik" :wrote "ehelp.el")
    ("Mosur Mohan" :changed "etags.c")
    ("Jeff Morgenthaler" :changed "flow-ctrl.el" "vt200.el" "vt201.el"
     "vt220.el" "vt240.el")
    ("Motorola" :changed "buff-menu.el")
    ("Hiroshi Nakano" :changed "ralloc.c")
    ;; File removed in Emacs 24.1.
;;;    ("Sundar Narasimhan" :changed "rnewspost.el")
    ;; No longer distributed.
;;;    ("NeXT, Inc." :wrote "unexnext.c")
    ("Mark Neale" :changed "fortran.el")
    ;; Renamed from sc.el.
    ("Martin Neitzel" :changed "supercite.el")
    ("Andrew Oram" :changed "calendar.texi (and other files in man/)")
    ("Frederic Pierresteguy" :wrote "widget.c")
    ("Michael D. Prange" :changed "tex-mode.el")
    ;; No longer distributed (dgux5-4r3.h was renamed to dgux5-4-3.h).
;;;    ("Paul Reilly" :wrote "gux5-4r2.h" "dgux5-4-3.h")
    ("Roland B. Roberts" :changed "files.el" "sort.el"
     "buffer.h" "callproc.c" "dired.c" "process.c" "sysdep.c" "systty.h")
     ;; No longer distributed.
;;;     "vmspaths.h" "build.com" "compile.com" "kepteditor.com" "precomp.com"
;;;     "vmsproc.el" :wrote "logout.com" "mailemacs.com")
;;;    ("Guillermo J. Rozas" :wrote "fakemail.c")
    ("Wolfgang Rupprecht" :changed "lisp-mode.el" "loadup.el"
     "sort.el" "alloc.c" "callint.c"
     ;; config.in renamed from config.h.in; ecrt0.c from crt0.c.
     "config.in" "ecrt0.c" "data.c" "fns.c"
     "lisp.h" "lread.c" ; "sun3.h" "ymakefile" - no longer distributed
     "print.c" :wrote "float-sup.el" "floatfns.c")
    ("Schlumberger Technology Corporation" :changed "gud.el")
    ;; Replaced by tcl.el.
;;;    ("Gregor Schmid" :wrote "tcl-mode.el")
    ("Rainer Schoepf" :wrote "alpha.h" "unexalpha.c")
    ;; No longer distributed: emacsserver.c.
    ("William Sommerfeld" :wrote "emacsclient.c" "scribe.el")
    ;; No longer distributed: emacsserver.c.
    ("Leigh Stoller" :changed "emacsclient.c" "server.el")
    ("Steve Strassmann" :wrote "spook.el")
    ("Shinichirou Sugou" :changed "etags.c")
    ;; No longer distributed: emacsserver.c.
    ("Sun Microsystems, Inc" :changed "emacsclient.c" "server.el"
     :wrote "emacs.icon" "sun.el")
    ;; No longer distributed.
;;;     "emacstool.1" "emacstool.c" "sun-curs.el"
;;;     "sun-fns.el" "sun-mouse.el" "sunfns.c")
    ;; Renamed from sc.el.
    ("Kayvan Sylvan" :changed "supercite.el")
    ;; No longer distributed: emacsserver.c, tcp.c.
    ("Spencer Thomas" :changed "emacsclient.c" "server.el"
     "dabbrev.el" "unexcoff.c" "gnus.texi")
    ("Jonathan Vail" :changed "vc.el")
    ("James Van Artsdalen" :changed "usg5-4.h" "unexcoff.c")
    ;; No longer distributed: src/makefile.nt, lisp/makefile.nt
    ;; winnt.el renamed to w32-fns.el; nt.[ch] to w32.[ch];
    ;; ntheap.[ch] to w32heap.[ch]; ntinevt.c to w32inevt.c;
    ;; ntproc.c to w32proc.c; ntterm.c to w32term.c;
    ;; windowsnt.h to ms-w32.h.
    ("Geoff Voelker" :wrote "w32-fns.el" "w32.c" "w32.h" "w32heap.c"
     "w32heap.h" "w32inevt.c" "w32proc.c" "w32term.c" "ms-w32.h")
    ("Morten Welinder" :wrote "dosfns.c" "[many MS-DOS files]" "msdos.h")
    ("Eli Zaretskii" :wrote "bidi.c" "[bidirectional display in xdisp.c]")
    ;; Not using this version any more.
;;;    ("Pace Willisson" :wrote "ispell.el")
    ;; FIXME overwritten by Author:.
    ("Bill Wohler" :cowrote "mh-e.el")
    ("Garrett Wollman" :changed "sendmail.el")
    ("Dale R. Worley" :changed "mail-extr.el")
    ("Jamie Zawinski" :changed "bytecode.c" :wrote "tar-mode.el"
     :cowrote "disass.el"))
  "Actions taken from the original, manually (un)maintained AUTHORS file.")


(defconst authors-valid-file-names
  '("aclocal.m4"
    "build-ins.in"
    "Makefile.noleim"
    "makedist.bat"
    "makefile.def"
    "makefile.nt"
    "debug.bat.in" "emacs.bat.in"
    ".gdbinit-union"
    "alloca.s"
    "make-delta"
    "config.w95"
    "emacstool.1"
    "align.umax"
    "cxux-crt0.s"
    "gould-sigvec.s"
    "getdate.y"
    "ymakefile"
    "permute-index" "index.perm"
    "ibmrs6000.inp"
    "b2m.c" "b2m.1" "b2m.pl"
    "emacs.bash" "emacs.csh" "ms-kermit"
    "emacs.ico"
    "emacs21.ico"
    "BABYL" "LPF" "LEDIT" "OTHER.EMACSES"
    "emacs16_mac.png" "emacs24_mac.png"
    "emacs256_mac.png" "emacs32_mac.png"
    "emacs48_mac.png" "emacs512_mac.png"
    "revdiff"				; admin/
    "mainmake" "sed1.inp" "sed2.inp" "sed3.inp" ; msdos/
    "mac-fix-env.m"
    ;; Deleted vms stuff:
    "temacs.opt" "descrip.mms" "compile.com" "link.com"
    )
  "File names which are valid, but no longer exist (or cannot be found)
in the repository.")

(defconst authors-renamed-files-alist
  '(("nt.c" . "w32.c") ("nt.h" . "w32.h")
    ("ntheap.c" . "w32heap.c") ("ntheap.h" . "w32heap.h")
    ("ntinevt.c" . "w32inevt.c") ("ntinevt.h" . "w32inevt.h")
    ("ntproc.c" . "w32proc.c")
    ("w32console.c" . "w32term.c")
    ("unexnt.c" . "unexw32.c")
    ("s/windowsnt.h" . "s/ms-w32.h")
    ("winnt.el" . "w32-fns.el")
    ("config.emacs" . "configure")
    ("config.h.dist" . "config.in")
    ("config.h-dist" . "config.in")
    ("config.h.in" . "config.in")
    ("paths.h-dist" . "paths.h.in")
    ("patch1" . "sed1.inp")
    ("GETTING.GNU.SOFTWARE" . "FTP")
    ("etc/MACHINES" . "MACHINES")
    ("ONEWS" . "NEWS.19")
    ("ONEWS.1" . "NEWS.1-17")
    ("ONEWS.2" . "NEWS.1-17")
    ("ONEWS.3" . "NEWS.18")
    ("ONEWS.4" . "NEWS.18")
    ("ORDERS.USA" . "ORDERS")
    ("EUROPE" . "ORDERS")
    ("DIFF" . "OTHER.EMACSES")
    ("CCADIFF" . "OTHER.EMACSES")
    ("GOSDIFF" . "OTHER.EMACSES")
    ("Makefile.in.in" . "Makefile.in")
    ("leim-Makefile" . "leim/Makefile")
    ("leim-Makefile.in" . "leim/Makefile.in")
    ("emacs-lisp/testcover-ses.el" . "tcover-ses.el")
    ("emacs-lisp/testcover-unsafep.el" . "tcover-unsafep.el")
    ;; index and pick merged into search.
    ("mh-index.el" . "mh-search.el")
    ("mh-pick.el" . "mh-search.el")
    ("font-setting.el" . "dynamic-setting.el")
    ;; INSTALL-CVS -> .CVS -> .BZR
    ("INSTALL-CVS" . "INSTALL.BZR")
    ("INSTALL.CVS" . "INSTALL.BZR")
    ("refcards/fr-drdref.pdf" . "refcards/fr-dired-ref.pdf")
    ("gnus-logo.eps" . "refcards/gnus-logo.eps")
    ("build-install" . "build-ins.in")
    ("build-install.in" . "build-ins.in")
    ("unidata/Makefile" . "unidata/Makefile.in")
    ;; Not renamed, but we only have the latter in the Emacs repo.
    ("trampver.texi.in" . "trampver.texi")
    ("e/eterm" . "e/eterm-color")
    ("e/eterm.ti" . "e/eterm-color.ti")
    ("README.txt" . "README")
    ("emacs.names" . "JOKES")
    ("ED.WORSHIP" . "JOKES")
    ("GNU.JOKES" . "JOKES")
    ("CHARACTERS" . "TODO")
    ("schema/xhtml-basic-form.rnc" . "schema/xhtml-bform.rnc" )
    ("schema/xhtml-basic-table.rnc" . "schema/xhtml-btable.rnc")
    ("schema/xhtml-list.rnc" . "schema/xhtml-lst.rnc")
    ("schema/xhtml-target.rnc" . "schema/xhtml-tgt.rnc")
    ("schema/xhtml-style.rnc" . "schema/xhtml-xstyle.rnc")
    ("schema/docbook-dyntbl.rnc" . "schema/docbk-dyntbl.rnc")
    ("schema/docbook-soextbl.rnc" . "schema/docbk-soextbl.rn" )
    ("texi/url.txi" . "url.texi")
    ("edt-user.doc" . "edt.texi")
    ("DEV-NOTES" . "nextstep")
    ("org/COPYRIGHT-AND-LICENSE" . "org/README")
    ;; Moved to different directories.
    ("ctags.1" . "ctags.1")
    ("etags.1" . "etags.1")
    ("emacs.1" . "emacs.1")
    ("emacsclient.1" . "emacsclient.1")
    ("icons/emacs21.ico" . "emacs21.ico")
    ;; Moved from admin/nt/ to nt/.
    ("nt/README.W32" . "README.W32")
    )
  "Alist of files which have been renamed during their lifetime.
Elements are (OLDNAME . NEWNAME).")

(defconst authors-renamed-files-regexps
  '(("^m/m-\\(.*\\.h\\)$" . "m/\\1")
    ("^m-\\(.*\\.h\\)$" . "\\1")
    ("^s/s-\\(.*\\.h\\)$" . "s/\\1")
    ("^s-\\(.*\\.h\\)$" . "\\1")
    ("^s/[-.a-zA-Z0-9_]+\\.h$" . t)
    ("\\(.*\\)\\.cmd$" . "\\1.bat")
    ("\\.bat$" . t)
    ("\\.[ch]$" . t)
    ("\\.el$" . t)
    ("\\.ps$" . t)
    ("\\.texi?$" . t)
    ("\\.texinfo$" . t)
    ("\\.xml?$" . t)
    ("\\.x[pb]m$" . t)
    ("\\.[xp]bm$" . t)
    ("^paths\\." . t)
    ("^install\\." . t)
    ("^\\(TUTORIAL[^/]*\\)" . "tutorials/\\1")
    ("^\\(tree-widget/\\(?:default\\|folder\\)/[-a-z]+\\.png\\)$" .
     "images/\\1")
    ("^\\(images/icons/\\)mac\\(emacs\\)_\\([0-9]+\\)\\(\\.png\\)" .
     "\\1\\2\\3_mac\\4")
    ("\\(images/icons/\\)emacs_\\([0-9][0-9]\\)\\.png" .
     "\\1hicolor/\\2x\\2/apps/emacs.png")
    )
  "List regexps and rewriting rules for renamed files.
Elements are (REGEXP . REPLACE).  If REPLACE is a string, the file
name matching REGEXP is replaced by REPLACE using `replace-string'.
Otherwise, the file name is accepted as is.")

(defvar authors-checked-files-alist)
(defvar authors-invalid-file-names)

(defun authors-disambiguate-file-name (fullname)
  "Convert FULLNAME to an unambiguous relative-name."
  (let ((relname (file-name-nondirectory fullname))
	parent)
    (if (member relname authors-ambiguous-files)
	;; In case of ambiguity, just prepend the parent directory.
	;; FIXME obviously this is not a perfect solution.
	(if (string-equal "lisp"
			  (setq parent (file-name-nondirectory
					(directory-file-name
					 (file-name-directory fullname)))))
	    relname
	  (format "%s/%s" parent relname))
      relname)))

(defun authors-canonical-file-name (file log-file pos author)
  "Return canonical file name for FILE found in LOG-FILE.
Checks whether FILE is a valid (existing) file name, has been renamed,
or is on the list of removed files.  Returns the non-directory part of
the file name.  Only uses the LOG-FILE position POS and associated AUTHOR
to print a message if FILE is not found."
  ;; FILE should be re-checked in every different directory associated
  ;; with a LOG-FILE.  Eg configure.in from src/ChangeLog is not the
  ;; same as that from top-level/ChangeLog.
  (let* ((fullname (expand-file-name file (file-name-directory log-file)))
	 (entry (assoc fullname authors-checked-files-alist))
	 relname
	 valid)
    (if entry
	(cdr entry)
      (setq relname (file-name-nondirectory file))
      (if (or (member relname authors-valid-file-names)
	      (file-exists-p file)
	      (file-exists-p relname)
	      (file-exists-p (concat "etc/" relname)))
	  (setq valid (authors-disambiguate-file-name fullname))
	(setq valid (assoc file authors-renamed-files-alist))
	(if valid
	    (setq valid (cdr valid))
	  (let ((rules authors-renamed-files-regexps))
	    (while rules
	      (if (string-match (car (car rules)) file)
		  (setq valid (if (stringp (cdr (car rules)))
				  (file-name-nondirectory
				   (replace-match (cdr (car rules)) t nil file))
				relname)
			rules nil))
	      (setq rules (cdr rules))))))
      (setq authors-checked-files-alist
	    (cons (cons fullname valid) authors-checked-files-alist))
      (unless (or valid
		  (member file authors-ignored-files)
		  (authors-obsolete-file-p file)
		  (string-match "[*]" file)
		  (string-match "^[0-9.]+$" file))
	(setq authors-invalid-file-names
	      (cons (format "%s:%d: unrecognized `%s' for %s"
			    log-file
			    (1+ (count-lines (point-min) pos))
			    file author)
		    authors-invalid-file-names)))
      valid)))

(defun authors-add-fixed-entries (table)
  "Add actions from `authors-fixed-entries' to TABLE."
  (dolist (entry authors-fixed-entries)
    (let ((author (car entry))
	  action)
      (dolist (item (cdr entry))
	(if (symbolp item)
	    (setq action item)
	  (authors-add author item action table))))))


(defun authors-obsolete-file-p (file)
  "Return non-nil if FILE is obsolete.
FILE is considered obsolete if it matches one of the regular expressions
from `authors-obsolete-files-regexps'."
  (let (obsolete-p
	(regexps authors-obsolete-files-regexps))
    (while (and regexps (not obsolete-p))
      (setq obsolete-p (string-match (car regexps) file)
	    regexps (cdr regexps)))
    obsolete-p))


(defun authors-add (author file action table)
  "Record that AUTHOR worked on FILE.
ACTION is a keyword symbol describing what he did.  Record file,
author and what he did in hash table TABLE.  See the description of
`authors-scan-change-log' for the structure of the hash table."
  (unless (or (member file authors-ignored-files)
	      (authors-obsolete-file-p file)
	      (equal author ""))
    (let* ((value (gethash author table))
	   (entry (assoc file value))
	   slot)
      (if (null entry)
	  (puthash author (cons (list file (cons action 1)) value) table)
	(if (setq slot (assoc action (cdr entry)))
	    (setcdr slot (1+ (cdr slot)))
	  (nconc entry (list (cons action 1))))))))


(defun authors-canonical-author-name (author)
  "Return a canonicalized form of AUTHOR, an author name.
If AUTHOR has an entry in `authors-aliases', use that.  Remove
email addresses.  Capitalize words in the author's name, unless
it is found in `authors-fixed-case'."
  (let* ((aliases authors-aliases)
	 regexps realname)
    (while aliases
      (setq realname (car (car aliases))
	    regexps (cdr (car aliases))
	    aliases (cdr aliases))
      (while regexps
	(if (string-match (car regexps) author)
	    (setq author realname
		  regexps nil
		  aliases nil)
	  (setq regexps (cdr regexps))))))
  (when author
    (setq author (replace-regexp-in-string "[ \t]*[(<].*$" "" author))
    (setq author (replace-regexp-in-string "\`[ \t]+" "" author))
    (setq author (replace-regexp-in-string "[ \t]+$" "" author))
    (setq author (replace-regexp-in-string "[ \t]+" " " author))
    (unless (string-match "[-, \t]" author)
      (setq author ""))
    (or (car (member author authors-fixed-case))
	(capitalize author))))

(defun authors-scan-change-log (log-file table)
  "Scan change log LOG-FILE for author information.

For each change mentioned in the log, add an entry to hash table TABLE
under the author's canonical name.

Keys of TABLE are author names.  Values are alists of entries (FILE
\(ACTION . COUNT) ...).  FILE is one file the author worked on.  The
rest of the entry is a list of keyword symbols describing what he did
with the file and the number of each action:

:wrote		means the author wrote the file
:cowrote	means he wrote the file in collaboration with others
:changed	means he changed the file COUNT times."

  (let* ((enable-local-variables :safe)	; for find-file, hence let*
	 (enable-local-eval nil)
	 (existing-buffer (get-file-buffer log-file))
	 (buffer (find-file-noselect log-file))
	 authors file pos)
    (with-current-buffer buffer
      (save-restriction
	(widen)
	(goto-char (point-min))
	(while (re-search-forward "^[0-9]\\|^[ \t]+\\* " nil t)
	  (beginning-of-line)
	  (setq pos (point))
	  (cond ((looking-at "^[0-9]+-[0-9]+-[0-9]+")
		 ;; Handle joint authorship of changes.
		 ;; This can be a bit fragile, and is not too common.
		 (setq authors nil)
		 (while (progn
			  (skip-chars-forward " \t+:0-9-")
			  (not (looking-at "\\($\\|\\*\\|\
Suggested\\|Trivial\\|Version\\|Originally\\|From:\\|Patch[ \t]+[Bb]y\\)")))
		   (push (authors-canonical-author-name
			  (buffer-substring-no-properties
			   (point) (line-end-position))) authors)
		   (forward-line 1)))
		((looking-at "^[ \t]+\\*")
		 (let ((line (buffer-substring-no-properties
			      (match-end 0) (line-end-position))))
		   (while (and (not (string-match ":" line))
			       (forward-line 1)
			       (not (looking-at ":\\|^[ \t]*$")))
		     (setq line (concat line
					(buffer-substring-no-properties
					 (line-beginning-position)
					 (line-end-position)))))
		   (when (string-match ":" line)
		     (setq line (substring line 0 (match-beginning 0)))
		     (setq line (replace-regexp-in-string "[[(<{].*$" "" line))
		     (setq line (replace-regexp-in-string "," "" line))
		     (dolist (file (split-string line))
		       (when (setq file (authors-canonical-file-name file log-file pos (car authors)))
			 (dolist (author authors)
			   ;;(message "%s changed %s" author file)
			   (authors-add author file :changed table)))))
		   (forward-line 1)))))))
    (unless existing-buffer
      (kill-buffer buffer))))


(defun authors-scan-el (file table)
  "Scan Lisp file FILE for author information.
TABLE is a hash table to add author information to."
  (let* ((existing-buffer (get-file-buffer file))
	 (enable-local-variables :safe)	; for find-file, hence let*
	 (enable-local-eval nil)
	 (buffer (find-file-noselect file)))
    (setq file (authors-disambiguate-file-name (expand-file-name file)))
    (with-current-buffer buffer
      (save-restriction
	(widen)
	(goto-char (point-min))
	(while (and (re-search-forward
		     "^;+[ \t]*\\(Authors?\\|Commentary\\|Code\\):[ \t]*" nil t)
		    (not (member (match-string 1) '("Commentary" "Code"))))
	  (let ((continue t)
		(action :wrote)
		authors)
	    (while continue
	      ;; Some entries contain a year range in front of the
	      ;; author's name.
	      (skip-chars-forward "-0-9 \t")
	      (push (authors-canonical-author-name
		     (buffer-substring-no-properties
		      (point) (line-end-position))) authors)
	      ;; tips.texi says the continuation line should begin
	      ;; with a tab, but often spaces are used.
	      (setq continue
		    (and (zerop (forward-line 1))
			 (looking-at ";;;?\\(\t+ *\\|  +\\)[[:alnum:]]")
			 (goto-char (1- (match-end 0)))
			 (not (looking-at "[[:upper:]][-[:alpha:]]+:[ \t]")))))
	    (and (> (length authors) 1)
		 (setq action :cowrote))
	    (mapc (lambda (author)
		    (authors-add author file action table))
		  authors)))))
    (unless existing-buffer
      (kill-buffer buffer))))


(defun authors-public-domain-p (file)
  "Return t if FILE is a file that was put in public domain."
  (let ((public-domain-p nil)
	(list authors-public-domain-files))
    (while (and list (not public-domain-p))
      (when (string-match (car list) file)
	(setq public-domain-p t))
      (setq list (cdr list)))
    public-domain-p))

(defvar authors-author-list)

(defun authors-add-to-author-list (author changes)
  "Insert information about AUTHOR's work on Emacs into `authors-author-list'.
CHANGES is an alist of entries (FILE (ACTION . COUNT) ...), as produced by
`authors-scan-change-log'.
The element added to `authors-author-list' is (AUTHOR WROTE CO-WROTE CHANGED),
where WROTE, CO-WROTE, and CHANGED are lists of the files written, co-written
and changed by AUTHOR."
  (when author
    (let ((nchanged 0)
	  wrote-list
	  cowrote-list
	  changed-list)
      (dolist (change changes)
	(let* ((actions (cdr change))
	       (file (car change))
	       (filestat (if (authors-public-domain-p file)
			     (concat file " (public domain)")
			   file))
	       slot)
	  (cond ((assq :wrote actions)
		 (setq wrote-list (cons filestat wrote-list)))
		((assq :cowrote actions)
		 (setq cowrote-list (cons filestat cowrote-list)))
		(t
		 (setq changed-list
		       (cons (cons file (cdr (assq :changed actions)))
			     changed-list))))))
      (if wrote-list
	  (setq wrote-list (sort wrote-list 'string-lessp)))
      (if cowrote-list
	  (setq cowrote-list (sort cowrote-list 'string-lessp)))
      (when changed-list
	(setq changed-list (sort changed-list
				 (lambda (a b)
				   (if (= (cdr a) (cdr b))
				       (string-lessp (car a) (car b))
				     (> (cdr a) (cdr b))))))
	(setq nchanged (length changed-list))
	(setq changed-list (mapcar 'car changed-list)))
      (if (> (- nchanged authors-many-files) 2)
	  (setcdr (nthcdr authors-many-files changed-list)
		  (list (format "and %d other files" (- nchanged authors-many-files)))))
      (setq authors-author-list
	    (cons (list author wrote-list cowrote-list changed-list)
		  authors-author-list)))))

(defun authors (root)
  "Extract author information from change logs and Lisp source files.
ROOT is the root directory under which to find the files.  If called
interactively, ROOT is read from the minibuffer.
Result is a buffer *Authors* containing authorship information, and a
buffer *Authors Errors* containing references to unknown files."
  (interactive "DEmacs source directory: ")
  (setq root (expand-file-name root))
  (let ((logs (process-lines find-program root "-name" "ChangeLog*"))
	(table (make-hash-table :test 'equal))
	(buffer-name "*Authors*")
	authors-checked-files-alist
	authors-invalid-file-names)
    (authors-add-fixed-entries table)
    (unless (file-exists-p (expand-file-name "src/emacs.c" root))
      (unless (y-or-n-p
	       (format "Not the root directory of Emacs: %s, continue? " root))
	(error "Not the root directory")))
    (dolist (log logs)
      (when (string-match "ChangeLog\\(.[0-9]+\\)?$" log)
	(message "Scanning %s..." log)
	(authors-scan-change-log log table)))
    (let ((els (process-lines find-program root "-name" "*.el")))
      (dolist (file els)
	(message "Scanning %s..." file)
	(authors-scan-el file table)))
    (message "Generating buffer %s..." buffer-name)
    (set-buffer (get-buffer-create buffer-name))
    (erase-buffer)
    (set-buffer-file-coding-system authors-coding-system)
    (insert
"Many people have contributed code included in the Free Software
Foundation's distribution of GNU Emacs.  To show our appreciation for
their public spirit, we list here in alphabetical order a condensed
list of their contributions.\n")
    (let (authors-author-list a)
      (maphash #'authors-add-to-author-list table)
      (setq authors-author-list
	    (sort authors-author-list
		  (lambda (a b) (string-lessp (car a) (car b)))))
      (dolist (a authors-author-list)
	(let ((author (car a))
	      (wrote (nth 1 a))
	      (cowrote (nth 2 a))
	      (changed (nth 3 a))
	      file)
	(insert "\n" author ": ")
	(when wrote
	  (insert "wrote")
	  (dolist (file wrote)
	    (if (> (+ (current-column) (length file)) 72)
	      (insert "\n "))
	    (insert " " file))
	  (insert "\n"))
	(when cowrote
	  (if wrote
	      (insert "and "))
	  (insert "co-wrote")
	  (dolist (file cowrote)
	    (if (> (+ (current-column) (length file)) 72)
	      (insert "\n "))
	    (insert " " file))
	  (insert "\n"))
	(when changed
	  (if (or wrote cowrote)
	      (insert "and "))
	  (insert "changed")
	  (dolist (file changed)
	    (if (> (+ (current-column) (length file)) 72)
		(insert "\n "))
	    (insert " " file))
	  (insert "\n")))))
    (insert "\nLocal" " Variables:\ncoding: "
	    (symbol-name authors-coding-system) "\nEnd:\n")
    (message "Generating buffer %s... done" buffer-name)
    (unless noninteractive
      (when authors-invalid-file-names
	(with-current-buffer (get-buffer-create "*Authors Errors*")
	  (setq buffer-read-only nil)
	  (erase-buffer)
	  (set-buffer-file-coding-system authors-coding-system)
	  (insert "Unrecognized file entries found:\n\n")
	  (mapc (lambda (f) (if (not (string-match "^[A-Za-z]+$" f)) (insert f "\n")))
		(sort authors-invalid-file-names 'string-lessp))
	  (goto-char (point-min))
	  (compilation-mode)
	  (message "Errors were found.  See buffer %s" (buffer-name))))
      (pop-to-buffer buffer-name))))


(defun batch-update-authors ()
  "Produce an AUTHORS file.
Call this function in batch mode with two command line arguments FILE
and ROOT.  FILE is the file to write, ROOT is the root directory of
the Emacs source tree, from which to build the file."
  (unless noninteractive
    (error "`batch-update-authors' is to be used only with -batch"))
  (when (/= (length command-line-args-left) 2)
    (error "Call `batch-update-authors' with the name of the file to write"))
  (let* ((file (pop command-line-args-left))
	 (root (pop command-line-args-left)))
    (authors root)
    (write-file file)))

(provide 'authors)

;;; authors.el ends here
