### autodeps.mk --- src/Makefile fragment for GNU Emacs

## Copyright (C) 2008-2012  Free Software Foundation, Inc.

## This file is part of GNU Emacs.

## GNU Emacs is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
## 
## GNU Emacs is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
## 
## You should have received a copy of the GNU General Public License
## along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

### Commentary:

## This is inserted in src/Makefile if HAVE_NS. 

## The only reason this is in a separate file is because $ns_appdir,
## which appears as a target, is empty on non-NS builds.  Some makes
## do not like empty targets, even if they are never used.

${ns_appdir}: ${ns_appsrc}
	rm -fr ${ns_appdir}
	mkdir -p ${ns_appdir}
	( cd ${ns_appsrc} ; tar cfh - . ) | ( cd ${ns_appdir} ; umask 022; tar xf - )

${ns_appbindir}Emacs: emacs${EXEEXT}
	mkdir -p ${ns_appbindir}
	cp -f emacs${EXEEXT} ${ns_appbindir}Emacs

ns-app: ${ns_appdir} ${ns_appbindir}Emacs

### ns.mk ends here
