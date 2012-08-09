@echo off

rem  Hack to run install-info with multiple info files on the command
rem  line on the Windows platform.

rem Copyright (C) 2003-2012  Free Software Foundation, Inc.

rem  This file is part of GNU Emacs.

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


rem  Usage:
rem   multi-install-info <switch passed to install-info> FILE1 FILE2 ...
rem
rem  By Peter 'Luna' Runestig <peter@runestig.com> 2003

set INSTALL_INFO=install-info
set II_SWITCH=%1=%2
rem Eat the install-info switch:
shift

:Loop
shift
if .%1% == . goto EndLoop
%INSTALL_INFO% %II_SWITCH% %1
goto Loop
:EndLoop

