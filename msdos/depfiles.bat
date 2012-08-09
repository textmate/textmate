@echo off
rem   ----------------------------------------------------------------------
rem   Auxiliary script for MSDOS, run by ../config.bat
rem   Copyright (C) 2011-2012  Free Software Foundation, Inc.

rem   This file is part of GNU Emacs.

rem   GNU Emacs is free software: you can redistribute it and/or modify
rem   it under the terms of the GNU General Public License as published by
rem   the Free Software Foundation, either version 3 of the License, or
rem   (at your option) any later version.

rem   GNU Emacs is distributed in the hope that it will be useful,
rem   but WITHOUT ANY WARRANTY; without even the implied warranty of
rem   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
rem   GNU General Public License for more details.

rem   You should have received a copy of the GNU General Public License
rem   along with GNU Emacs.  If not, see http://www.gnu.org/licenses/.

rem   ----------------------------------------------------------------------

echo %1 | sed -e "s,\(.*\)\.c,@if not exist deps\\\1.Po echo # dummy > deps\\\1.Po," > tdepfile.bat
call tdepfile
del tdepfile.bat
