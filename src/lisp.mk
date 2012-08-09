### lisp.mk --- src/Makefile fragment for GNU Emacs

## Copyright (C) 1985, 1987-1988, 1993-1995, 1999-2012
##   Free Software Foundation, Inc.

## This file is part of GNU Emacs.

## GNU Emacs is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.

## GNU Emacs is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

### Commentary:

## This is the list of all Lisp files that might be loaded into the
## dumped Emacs.  Some of them are not loaded on all platforms, but
## the DOC file on every platform uses them (because the DOC file is
## supposed to be platform-independent).
## It is arranged like this because it is easier to generate it
## semi-mechanically from loadup.el this way.
## Eg something like:
##   sed -e 's/"[ )].*//' -n -e '/(load "/ s/.*load "//p' loadup.el | \
##   grep -vE 'site-|ldefs-boot'
## minus any duplicates.
## Note that you cannot just add a ".elc" extension to every file,
## since some of them are no-byte-compile (eg some language/ ones).

## Confusingly, term/internal is not in loadup, but is unconditionally
## loaded by pc-win, which is.

## Note that this list should not include lisp files which might not
## be present, like site-load.el and site-init.el; this makefile
## expects them all to be either present or buildable.

## Place loaddefs.el first, so it gets generated first, since it is on
## the critical path (relevant in parallel compilations).

### Code:

## NB: This list is parsed by sed in the main src/Makefile.
## Do not change the formatting.
lisp = \
	$(lispsource)/loaddefs.el \
	$(lispsource)/loadup.el \
	$(lispsource)/emacs-lisp/byte-run.elc \
	$(lispsource)/emacs-lisp/backquote.elc \
	$(lispsource)/subr.elc \
	$(lispsource)/version.el \
	$(lispsource)/widget.elc \
	$(lispsource)/custom.elc \
	$(lispsource)/emacs-lisp/map-ynp.elc \
	$(lispsource)/cus-start.elc \
	$(lispsource)/international/mule.elc \
	$(lispsource)/international/mule-conf.elc \
	$(lispsource)/env.elc \
	$(lispsource)/format.elc \
	$(lispsource)/bindings.elc \
	$(lispsource)/files.elc \
	$(lispsource)/cus-face.elc \
	$(lispsource)/faces.elc \
	$(lispsource)/button.elc \
	$(lispsource)/startup.elc \
	$(lispsource)/minibuffer.elc \
	$(lispsource)/abbrev.elc \
	$(lispsource)/simple.elc \
	$(lispsource)/help.elc \
	$(lispsource)/jka-cmpr-hook.elc \
	$(lispsource)/epa-hook.elc \
	$(lispsource)/international/mule-cmds.elc \
	$(lispsource)/case-table.elc \
	$(lispsource)/international/characters.elc \
	$(lispsource)/composite.elc \
	$(lispsource)/international/charprop.el \
	$(lispsource)/language/chinese.elc \
	$(lispsource)/language/cyrillic.elc \
	$(lispsource)/language/indian.elc \
	$(lispsource)/language/sinhala.el \
	$(lispsource)/language/english.el \
	$(lispsource)/language/ethiopic.elc \
	$(lispsource)/language/european.elc \
	$(lispsource)/language/czech.el \
	$(lispsource)/language/slovak.el \
	$(lispsource)/language/romanian.el \
	$(lispsource)/language/greek.el \
	$(lispsource)/language/hebrew.elc \
	$(lispsource)/language/japanese.el \
	$(lispsource)/language/korean.el \
	$(lispsource)/language/lao.el \
	$(lispsource)/language/tai-viet.el \
	$(lispsource)/language/thai.el \
	$(lispsource)/language/tibetan.elc \
	$(lispsource)/language/vietnamese.elc \
	$(lispsource)/language/misc-lang.el \
	$(lispsource)/language/utf-8-lang.el \
	$(lispsource)/language/georgian.el \
	$(lispsource)/language/khmer.el \
	$(lispsource)/language/burmese.el \
	$(lispsource)/language/cham.el \
	$(lispsource)/indent.elc \
	$(lispsource)/window.elc \
	$(lispsource)/frame.elc \
	$(lispsource)/term/tty-colors.elc \
	$(lispsource)/font-core.elc \
	$(lispsource)/facemenu.elc \
	$(lispsource)/emacs-lisp/syntax.elc \
	$(lispsource)/font-lock.elc \
	$(lispsource)/jit-lock.elc \
	$(lispsource)/mouse.elc \
	$(lispsource)/scroll-bar.elc \
	$(lispsource)/select.elc \
	$(lispsource)/emacs-lisp/timer.elc \
	$(lispsource)/isearch.elc \
	$(lispsource)/rfn-eshadow.elc \
	$(lispsource)/menu-bar.elc \
	$(lispsource)/paths.el \
	$(lispsource)/emacs-lisp/lisp.elc \
	$(lispsource)/textmodes/page.elc \
	$(lispsource)/register.elc \
	$(lispsource)/textmodes/paragraphs.elc \
	$(lispsource)/emacs-lisp/lisp-mode.elc \
	$(lispsource)/textmodes/text-mode.elc \
	$(lispsource)/textmodes/fill.elc \
	$(lispsource)/replace.elc \
	$(lispsource)/buff-menu.elc \
	$(lispsource)/fringe.elc \
	$(lispsource)/image.elc \
	$(lispsource)/international/fontset.elc \
	$(lispsource)/dnd.elc \
	$(lispsource)/tool-bar.elc \
	$(lispsource)/dynamic-setting.elc \
	$(lispsource)/x-dnd.elc \
	$(lispsource)/term/common-win.elc \
	$(lispsource)/term/x-win.elc \
	$(lispsource)/w32-vars.elc \
	$(lispsource)/term/w32-win.elc \
	$(lispsource)/ls-lisp.elc \
	$(lispsource)/disp-table.elc \
	$(lispsource)/dos-w32.elc \
	$(lispsource)/w32-fns.elc \
	$(lispsource)/dos-fns.elc \
	$(lispsource)/dos-vars.elc \
	$(lispsource)/term/pc-win.elc \
	$(lispsource)/term/internal.elc \
	$(lispsource)/term/ns-win.elc \
	$(lispsource)/mwheel.elc \
	$(lispsource)/emacs-lisp/float-sup.elc \
	$(lispsource)/vc/vc-hooks.elc \
	$(lispsource)/vc/ediff-hook.elc \
	$(lispsource)/tooltip.elc


### lisp.mk ends here
