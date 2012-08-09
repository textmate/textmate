@echo off
rem   ----------------------------------------------------------------------
rem   Configuration script for MS Windows operating systems
rem   Copyright (C) 1999-2012  Free Software Foundation, Inc.

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
rem   YOU'LL NEED THE FOLLOWING UTILITIES TO MAKE EMACS:
rem
rem   + MS Windows 95, NT or later
rem   + either MSVC 2.x or later, or gcc-2.95 or later (with GNU make 3.75
rem     or later) and the Mingw32 and W32 API headers and libraries.
rem   + Visual Studio 2005 is not supported at this time.
rem
rem For reference, here is a list of which builds of GNU make are known to
rem work or not, and whether they work in the presence and/or absence of
rem sh.exe.
rem
rem                                       sh exists     no sh
rem  cygwin b20.1 make (3.75):            fails[1,5]    fails[2,5]
rem  MSVC compiled gmake 3.77:            okay          okay
rem  MSVC compiled gmake 3.78.1:          okay          okay
rem  MSVC compiled gmake 3.79.1:          okay          okay
rem  mingw32/gcc-2.92.2 make (3.77):      okay          okay[4]
rem  cygwin compiled gmake 3.77:          fails[1,5]    fails[2,5]
rem  cygwin compiled gmake 3.78.1:        fails[5]      fails[2,5]
rem  cygwin compiled gmake 3.79.1:        fails[3,5]    fails[2?,5]
rem  cygwin compiled make 3.80:           okay[6]       fails?[7]
rem  cygwin compiled make 3.81:           fails         fails?[7]
rem  mingw32 compiled make 3.79.1:        okay          okay
rem  mingw32 compiled make 3.80:          okay          okay?[7]
rem  mingw32 compiled make 3.81:          okay          okay[8]
rem
rem [1] doesn't cope with makefiles with DOS line endings, so must mount
rem     emacs source with text!=binary.
rem [2] fails when needs to invoke shell commands; okay invoking gcc etc.
rem [3] requires LC_MESSAGES support to build; cannot build with early
rem     versions of cygwin.
rem [4] may fail on Windows 9X and Windows ME; if so, install Bash.
rem [5] fails when building leim due to the use of cygwin style paths.
rem     May work if building emacs without leim.
rem [6] need to uncomment 3 lines in nt/gmake.defs that invoke `cygpath';
rem    	look for "cygpath" near line 85 of gmake.defs.
rem [7] not recommended; please report if you try this combination.
rem [8] tested only on Windows XP.
rem

if exist config.log del config.log

rem ----------------------------------------------------------------------
rem   See if the environment is large enough.  We need 43 (?) bytes.
set $foo$=123456789_123456789_123456789_123456789_123
if not "%$foo$%" == "123456789_123456789_123456789_123456789_123" goto SmallEnv
set $foo$=

rem ----------------------------------------------------------------------
rem   Make sure we are running in the nt subdir
if exist configure.bat goto start
echo You must run configure from the nt subdirectory.
goto end

:start
rem ----------------------------------------------------------------------
rem   Attempt to enable command extensions.  Set use_extensions to 1 if
rem   they are available and 0 if they are not available.
set use_extensions=1
setlocal ENABLEEXTENSIONS
if "%CMDEXTVERSION%" == "" set use_extensions=0
if "%use_extensions%" == "1" goto afterext

echo. Command extensions are not available.  Using parameters that include the =
echo. character by enclosing them in quotes will not be supported.

:afterext

rem ----------------------------------------------------------------------
rem   Default settings.
set prefix=
set nodebug=N
set noopt=N
set enablechecking=N
set profile=N
set nocygwin=N
set COMPILER=
set usercflags=
set escusercflags=
set docflags=
set userldflags=
set escuserldflags=
set extrauserlibs=
set doldflags=
set doextralibs=
set sep1=
set sep2=
set sep3=
set sep4=
set distfiles=

rem ----------------------------------------------------------------------
rem   Handle arguments.
:again
if "%1" == "-h" goto usage
if "%1" == "--help" goto usage
if "%1" == "--prefix" goto setprefix
if "%1" == "--with-gcc" goto withgcc
if "%1" == "--with-msvc" goto withmsvc
if "%1" == "--no-debug" goto nodebug
if "%1" == "--no-opt" goto noopt
if "%1" == "--enable-checking" goto enablechecking
if "%1" == "--profile" goto profile
if "%1" == "--no-cygwin" goto nocygwin
if "%1" == "--cflags" goto usercflags
if "%1" == "--ldflags" goto userldflags
if "%1" == "--lib" goto extrauserlibs
if "%1" == "--without-png" goto withoutpng
if "%1" == "--without-jpeg" goto withoutjpeg
if "%1" == "--without-gif" goto withoutgif
if "%1" == "--without-tiff" goto withouttiff
if "%1" == "--without-gnutls" goto withoutgnutls
if "%1" == "--without-xpm" goto withoutxpm
if "%1" == "--with-svg" goto withsvg
if "%1" == "--distfiles" goto distfiles
if "%1" == "" goto checkutils

:usage
echo Usage: configure [options]
echo Options:
echo.   --prefix PREFIX         install Emacs in directory PREFIX
echo.   --with-gcc              use GCC to compile Emacs
echo.   --with-msvc             use MSVC to compile Emacs
echo.   --no-debug              exclude debug info from executables
echo.   --no-opt                disable optimization
echo.   --enable-checking       enable checks and assertions
echo.   --profile               enable profiling
echo.   --no-cygwin             use -mno-cygwin option with GCC
echo.   --cflags FLAG           pass FLAG to compiler
echo.   --ldflags FLAG          pass FLAG to compiler when linking
echo.   --lib LIB               link to extra library LIB
echo.   --without-png           do not use PNG library even if it is installed
echo.   --without-jpeg          do not use JPEG library even if it is installed
echo.   --without-gif           do not use GIF library even if it is installed
echo.   --without-tiff          do not use TIFF library even if it is installed
echo.   --without-xpm           do not use XPM library even if it is installed
echo.   --without-gnutls        do not use GnuTLS library even if it is installed
echo.   --with-svg              use the RSVG library (experimental)
echo.   --distfiles             path to files for make dist, e.g. libXpm.dll
if "%use_extensions%" == "0" goto end
echo.
echo. The cflags and ldflags arguments support parameters that include the =
echo. character.  However, since the = character is normally treated as a
echo. separator character you will need to enclose any parameter that includes
echo. the = character in quotes.  For example, to include
echo. -DSITELOAD_PURESIZE_EXTRA=100000 as one of the cflags you would run
echo. configure.bat as follows:
echo. configure.bat --cflags "-DSITELOAD_PURESIZE_EXTRA=100000"
echo.
echo. Note that this capability of processing parameters that include the =
echo. character depends on command extensions.  This batch file attempts to
echo. enable command extensions.  If command extensions cannot be enabled, a
echo. warning message will be displayed.
goto end

rem ----------------------------------------------------------------------

:setprefix
shift
set prefix=%1
shift
goto again

rem ----------------------------------------------------------------------

:withgcc
set COMPILER=gcc
shift
goto again

rem ----------------------------------------------------------------------

:withmsvc
set COMPILER=cl
shift
goto again

rem ----------------------------------------------------------------------

:nodebug
set nodebug=Y
shift
goto again

rem ----------------------------------------------------------------------

:noopt
set noopt=Y
shift
goto again

rem ----------------------------------------------------------------------

:enablechecking
set enablechecking=Y
shift
goto again

rem ----------------------------------------------------------------------

:profile
set profile=Y
shift
goto again

rem ----------------------------------------------------------------------

:nocygwin
set nocygwin=Y
shift
goto again

rem ----------------------------------------------------------------------

:usercflags
if "%use_extensions%" == "1" goto ucflagex
goto ucflagne

:ucflagex
shift
set usercflags=%usercflags%%sep1%%~1
set escusercflags=%usercflags:"=\"%
set sep1= %nothing%
shift
goto again

:ucflagne
shift
set usercflags=%usercflags%%sep1%%1
set escusercflags=%usercflags%
set sep1= %nothing%
shift
goto again

:extrauserlibs
shift
echo. extrauserlibs: %extrauserlibs%
set extrauserlibs=%extrauserlibs%%sep4%%1
set sep4= %nothing%
shift
goto again

rem ----------------------------------------------------------------------

:userldflags
if "%use_extensions%" == "1" goto ulflagex
goto ulflagne

:ulflagex
shift
set userldflags=%userldflags%%sep2%%~1
set escuserldflags=%userldflags:"=\"%
set sep2= %nothing%
shift
goto again

:ulflagne
shift
set userldflags=%userldflags%%sep2%%1
set escuserldflags=%userldflags%
set sep2= %nothing%
shift
goto again

rem ----------------------------------------------------------------------

:withoutpng
set pngsupport=N
set HAVE_PNG=
shift
goto again

rem ----------------------------------------------------------------------

:withoutjpeg
set jpegsupport=N
set HAVE_JPEG=
shift
goto again

rem ----------------------------------------------------------------------

:withoutgif
set gifsupport=N
set HAVE_GIF=
shift
goto again

rem ----------------------------------------------------------------------

:withoutgnutls
set tlssupport=N
set HAVE_GNUTLS=
shift
goto again

rem ----------------------------------------------------------------------

:withouttiff
set tiffsupport=N
set HAVE_TIFF=
shift
goto again

rem ----------------------------------------------------------------------

:withoutxpm
set xpmsupport=N
set HAVE_XPM=
shift
goto again

:withsvg
shift
set svgsupport=Y
goto again

rem ----------------------------------------------------------------------

:distfiles
set HAVE_DISTFILES=1
shift
set distfiles=%distfiles%%sep3%%1
set sep3= %nothing%
shift
goto again

rem ----------------------------------------------------------------------
rem    Check that necessary utilities (cp and rm) are present.

:checkutils
echo Checking for 'cp'...
cp configure.bat junk.bat
if not exist junk.bat goto needcp
echo Checking for 'rm'...
rm junk.bat
if exist junk.bat goto needrm
goto checkcompiler

:needcp
echo You need 'cp' (the Unix file copy program) to build Emacs.
goto end

:needrm
del junk.bat
echo You need 'rm' (the Unix file delete program) to build Emacs.
goto end

rem ----------------------------------------------------------------------
rem   Auto-detect compiler if not specified, and validate GCC if chosen.

:checkcompiler
if (%COMPILER%)==(cl) goto compilercheckdone
if (%COMPILER%)==(gcc) goto checkgcc

echo Checking whether 'gcc' is available...
echo main(){} >junk.c
gcc -c junk.c
if exist junk.o goto checkgcc

echo Checking whether 'cl' is available...
cl -nologo -c junk.c
if exist junk.obj goto clOK
goto nocompiler

:checkgcc
if exist junk.o del junk.o
Rem WARNING -- COMMAND.COM on some systems only looks at the first
Rem            8 characters of a label.  So do NOT be tempted to change
Rem            chkapi* into something fancier like checkw32api
Rem You HAVE been warned!
if (%nocygwin%) == (Y) goto chkapiN
echo Checking whether gcc requires '-mno-cygwin'...
echo #include "cygwin/version.h" >junk.c
echo main(){} >>junk.c
echo gcc -c junk.c >>config.log
gcc -c junk.c >>config.log 2>&1
if not exist junk.o goto chkapi
echo gcc -mno-cygwin -c junk.c >>config.log
gcc -mno-cygwin -c junk.c >>config.log 2>&1
if exist junk.o set nocygwin=Y

:chkapi
echo The failed program was: >>config.log
type junk.c >>config.log

:chkapiN
rm -f junk.c junk.o
rem ----------------------------------------------------------------------
rem   Older versions of the Windows API headers either don't have any of
rem   the IMAGE_xxx definitions (the headers that come with Cygwin b20.1
rem   are like this), or have a typo in the definition of
rem   IMAGE_FIRST_SECTION (the headers with gcc/mingw32 2.95 have this
rem   problem).  The gcc/mingw32 2.95.2 headers are okay, as are distros
rem   of w32api-xxx.zip from Anders Norlander since 1999-11-18 at least.
rem   Beginning with Emacs 23, we need usp10.h.
rem
echo Checking whether W32 API headers are too old...
echo #include "windows.h" >junk.c
echo #include "usp10.h" >>junk.c
echo test(PIMAGE_NT_HEADERS pHeader) >>junk.c
echo {PIMAGE_SECTION_HEADER pSection = IMAGE_FIRST_SECTION(pHeader);} >>junk.c
if (%nocygwin%) == (Y) goto chkapi1
set cf=%usercflags%
goto chkapi2

:chkapi1
set cf=%usercflags% -mno-cygwin

:chkapi2
echo on
gcc %cf% -c junk.c
@echo off
@echo gcc %cf% -c junk.c >>config.log
gcc %cf% -c junk.c >>config.log 2>&1
set cf=
if exist junk.o goto chkuser
echo The failed program was: >>config.log
type junk.c >>config.log
goto nocompiler

:chkuser
rm -f junk.o
echo int main (int argc, char *argv[]) {>junk.c
echo char *usercflags = "%escusercflags%";>>junk.c
echo }>>junk.c
echo gcc -Werror -c junk.c >>config.log
gcc -Werror -c junk.c >>config.log 2>&1
if exist junk.o goto gccOk
echo.
echo Error in --cflags argument: %usercflags%
echo Backslashes and quotes cannot be used with --cflags.  Please use forward
echo slashes for filenames and paths (e.g. when passing directories to -I).
rm -f junk.c
goto end

:nocompiler
echo.
echo Configure failed.
echo To configure Emacs for Windows, you need to have either
echo gcc-2.95 or later with Mingw32 and the W32 API headers,
echo or MSVC 2.x or later.
del junk.c
goto end

:gccOk
set COMPILER=gcc
echo Using 'gcc'
rm -f junk.c junk.o
Rem It is not clear what GCC version began supporting -mtune
Rem and pentium4 on x86, so check this explicitly.
echo main(){} >junk.c
echo gcc -c -O2 -mtune=pentium4 junk.c >>config.log
gcc -c -O2 -mtune=pentium4 junk.c >>config.log 2>&1
if not errorlevel 1 goto gccMtuneOk
echo The failed program was: >>config.log
type junk.c >>config.log
set mf=-mcpu=i686
rm -f junk.c junk.o
goto gccdebug

:gccMtuneOk
echo GCC supports -mtune=pentium4 >>config.log
set mf=-mtune=pentium4
rm -f junk.c junk.o

:gccdebug
rem Check for DWARF-2 debug info support, else default to stabs
echo main(){} >junk.c
echo gcc -c -gdwarf-2 -g3 junk.c >>config.log
gcc -c -gdwarf-2 -g3 junk.c >>config.log 2>&1
if not errorlevel 1 goto gccdwarf
echo The failed program was: >>config.log
type junk.c >>config.log
set dbginfo=-gstabs+
rm -f junk.c junk.o
goto compilercheckdone

:gccdwarf
echo GCC supports DWARF-2 >>config.log
set dbginfo=-gdwarf-2 -g3
rm -f junk.c junk.o
goto compilercheckdone

:clOk
set COMPILER=cl
rm -f junk.c junk.obj
echo Using 'MSVC'

:compilercheckdone

rem ----------------------------------------------------------------------
rem   Check for external image libraries. Since they are loaded
rem   dynamically, the libraries themselves do not need to be present
rem   at compile time, but the header files are required.

set mingwflag=

if (%nocygwin%) == (N) goto flagsOK
set mingwflag=-mno-cygwin

:flagsOK

if (%pngsupport%) == (N) goto pngDone

echo Checking for libpng...
echo #include "png.h" >junk.c
echo main (){} >>junk.c
rem   -o option is ignored with cl, but allows result to be consistent.
echo %COMPILER% %usercflags% %mingwflag% -c junk.c -o junk.obj >>config.log
%COMPILER% %usercflags% %mingwflag% -c junk.c -o junk.obj >junk.out 2>>config.log
if exist junk.obj goto havePng

echo ...png.h not found, building without PNG support.
echo The failed program was: >>config.log
type junk.c >>config.log
set HAVE_PNG=
goto :pngDone

:havePng
echo ...PNG header available, building with PNG support.
set HAVE_PNG=1

:pngDone
rm -f junk.c junk.obj

if (%tlssupport%) == (N) goto tlsDone

rem this is a copy of the PNG detection
echo Checking for libgnutls...
echo #include "gnutls/gnutls.h" >junk.c
echo main (){} >>junk.c
rem   -o option is ignored with cl, but allows result to be consistent.
echo %COMPILER% %usercflags% %mingwflag% -c junk.c -o junk.obj >>config.log
%COMPILER% %usercflags% %mingwflag% -c junk.c -o junk.obj >junk.out 2>>config.log
if exist junk.obj goto haveTls

echo ...gnutls.h not found, building without TLS support.
echo The failed program was: >>config.log
type junk.c >>config.log
set HAVE_GNUTLS=
goto :tlsDone

:haveTls
echo ...GnuTLS header available, building with GnuTLS support.
set HAVE_GNUTLS=1

:tlsDone
rm -f junk.c junk.obj

if (%jpegsupport%) == (N) goto jpegDone

echo Checking for jpeg-6b...
echo #include "jconfig.h" >junk.c
echo main (){} >>junk.c
rem   -o option is ignored with cl, but allows result to be consistent.
echo %COMPILER% %usercflags% %mingwflag% -c junk.c -o junk.obj >>config.log
%COMPILER% %usercflags% %mingwflag% -c junk.c -o junk.obj >junk.out 2>>config.log
if exist junk.obj goto haveJpeg

echo ...jconfig.h not found, building without JPEG support.
echo The failed program was: >>config.log
type junk.c >>config.log
set HAVE_JPEG=
goto :jpegDone

:haveJpeg
echo ...JPEG header available, building with JPEG support.
set HAVE_JPEG=1

:jpegDone
rm -f junk.c junk.obj

if (%gifsupport%) == (N) goto gifDone

echo Checking for libgif...
echo #include "gif_lib.h" >junk.c
echo main (){} >>junk.c
rem   -o option is ignored with cl, but allows result to be consistent.
echo %COMPILER% %usercflags% %mingwflag% -c junk.c -o junk.obj >>config.log
%COMPILER% %usercflags% %mingwflag% -c junk.c -o junk.obj >junk.out 2>>config.log
if exist junk.obj goto haveGif

echo ...gif_lib.h not found, building without GIF support.
echo The failed program was: >>config.log
type junk.c >>config.log
set HAVE_GIF=
goto :gifDone

:haveGif
echo ...GIF header available, building with GIF support.
set HAVE_GIF=1

:gifDone
rm -f junk.c junk.obj

if (%tiffsupport%) == (N) goto tiffDone

echo Checking for tiff...
echo #include "tiffio.h" >junk.c
echo main (){} >>junk.c
rem   -o option is ignored with cl, but allows result to be consistent.
echo %COMPILER% %usercflags% %mingwflag% -c junk.c -o junk.obj >>config.log
%COMPILER% %usercflags% %mingwflag% -c junk.c -o junk.obj >junk.out 2>>config.log
if exist junk.obj goto haveTiff

echo ...tiffio.h not found, building without TIFF support.
echo The failed program was: >>config.log
type junk.c >>config.log
set HAVE_TIFF=
goto :tiffDone

:haveTiff
echo ...TIFF header available, building with TIFF support.
set HAVE_TIFF=1

:tiffDone
rm -f junk.c junk.obj

if (%xpmsupport%) == (N) goto xpmDone

echo Checking for libXpm...
echo #define FOR_MSW 1 >junk.c
echo #include "X11/xpm.h" >>junk.c
echo main (){} >>junk.c
rem   -o option is ignored with cl, but allows result to be consistent.
echo %COMPILER% %usercflags% %mingwflag% -c junk.c -o junk.obj >>config.log
%COMPILER% %usercflags% %mingwflag% -c junk.c -o junk.obj >junk.out 2>>config.log
if exist junk.obj goto haveXpm

echo ...X11/xpm.h not found, building without XPM support.
echo The failed program was: >>config.log
type junk.c >>config.log
set HAVE_XPM=
goto :xpmDone

:haveXpm
echo ...XPM header available, building with XPM support.
set HAVE_XPM=1

:xpmDone
rm -f junk.c junk.obj

if not (%svgsupport%) == (Y) goto :svgDone
echo Checking for librsvg...
echo #include "librsvg/rsvg.h" >junk.c
echo main (){} >>junk.c
rem   -o option is ignored with cl, but allows result to be consistent.
echo %COMPILER% %usercflags% %mingwflag% -c junk.c -o junk.obj >>config.log
%COMPILER% %usercflags% %mingwflag% -c junk.c -o junk.obj >junk.out 2>>config.log
if exist junk.obj goto haveSvg

echo ...librsvg/rsvg.h or dependencies not found, building without SVG support.
echo The failed program was: >>config.log
type junk.c >>config.log
set HAVE_RSVG=
goto :svgDone

:haveSvg
echo ...librsvg header available, building with SVG support (EXPERIMENTAL).
set HAVE_RSVG=1

:svgDone
rm -f junk.c junk.obj junk.err junk.out

rem Any distfiles provided for building distribution? If no, we're done.
if "(%HAVE_DISTFILES%)"=="()" goto :distFilesDone

rem Any arguments to --distfiles specified? If no, we're done.
if not "%distfiles%"=="" goto :checkDistFiles
set distFilesOk=0
echo No arguments specified for option --distfiles!
goto distfilesDone

:checkDistFiles
echo Checking for distfiles...
rem Check if all specified distfiles exist
set fileNotFound=
for %%d in (%distfiles%) do if not exist %%d set fileNotFound=%%d
if not "%fileNotFound%"=="" goto distFilesNotFound

set distFilesOK=1
echo ...all distfiles found.
goto :distFilesDone

:distFilesNotFound
set distFilesOk=0
echo ...%fileNotFound% not found.
set distfiles=
goto :distfilesDone

:distFilesDone
set fileNotFound=

rem ----------------------------------------------------------------------

:genmakefiles
echo Generating makefiles
if %COMPILER% == gcc set MAKECMD=gmake
if %COMPILER% == cl set MAKECMD=nmake

rem   Pass on chosen settings to makefiles.
rem
rem   The weird place we put the redirection is to make sure no extra
rem   whitespace winds up at the end of the Make variables, since some
rem   variables, e.g. INSTALL_DIR, cannot stand that.  Yes, echo will
rem   write the blanks between the end of command arguments and the
rem   redirection symbol to the file.  OTOH, we cannot put the
rem   redirection immediately after the last character of the command,
rem   because environment variable expansion can yield a digit there,
rem   which will then be misinterpreted as the file descriptor to
rem   redirect...
echo # Start of settings from configure.bat >config.settings
>>config.settings echo COMPILER=%COMPILER%
if not "(%mf%)" == "()" >>config.settings echo MCPU_FLAG=%mf%
if not "(%dbginfo%)" == "()" >>config.settings echo DEBUG_INFO=%dbginfo%
if (%nodebug%) == (Y) >>config.settings echo NODEBUG=1
if (%noopt%) == (Y) >>config.settings echo NOOPT=1
if (%enablechecking%) == (Y) >>config.settings echo ENABLECHECKS=1
if (%profile%) == (Y) >>config.settings echo PROFILE=1
if (%nocygwin%) == (Y) >>config.settings echo NOCYGWIN=1
if not "(%prefix%)" == "()" >>config.settings echo INSTALL_DIR=%prefix%
if not "(%distfiles%)" == "()" >>config.settings echo DIST_FILES=%distfiles%
rem We go thru docflags because usercflags could be "-DFOO=bar" -something
rem and the if command cannot cope with this
for %%v in (%usercflags%) do if not (%%v)==() set docflags=Y
if (%docflags%)==(Y) >>config.settings echo USER_CFLAGS=%usercflags%
if (%docflags%)==(Y) >>config.settings echo ESC_USER_CFLAGS=%escusercflags%
for %%v in (%userldflags%) do if not (%%v)==() set doldflags=Y
if (%doldflags%)==(Y) >>config.settings echo USER_LDFLAGS=%userldflags%
for %%v in (%extrauserlibs%) do if not (%%v)==() set doextralibs=Y
if (%doextralibs%)==(Y) >>config.settings echo USER_LIBS=%extrauserlibs%
echo # End of settings from configure.bat>>config.settings
echo. >>config.settings

copy config.nt config.tmp
echo. >>config.tmp
echo /* Start of settings from configure.bat.  */ >>config.tmp
rem   We write USER_CFLAGS and USER_LDFLAGS starting with a space to simplify
rem   processing of compiler options in w32.c:get_emacs_configuration_options
if (%docflags%) == (Y) echo #define USER_CFLAGS " %escusercflags%" >>config.tmp
if (%doldflags%) == (Y) echo #define USER_LDFLAGS " %escuserldflags%" >>config.tmp
if (%profile%) == (Y) echo #define PROFILING 1 >>config.tmp
if not "(%HAVE_PNG%)" == "()" echo #define HAVE_PNG 1 >>config.tmp
if not "(%HAVE_GNUTLS%)" == "()" echo #define HAVE_GNUTLS 1 >>config.tmp
if not "(%HAVE_JPEG%)" == "()" echo #define HAVE_JPEG 1 >>config.tmp
if not "(%HAVE_GIF%)" == "()" echo #define HAVE_GIF 1 >>config.tmp
if not "(%HAVE_TIFF%)" == "()" echo #define HAVE_TIFF 1 >>config.tmp
if not "(%HAVE_XPM%)" == "()" echo #define HAVE_XPM 1 >>config.tmp
if "(%HAVE_RSVG%)" == "(1)" echo #define HAVE_RSVG 1 >>config.tmp

echo /* End of settings from configure.bat.  */ >>config.tmp

Rem See if fc.exe returns a meaningful exit status.  If it does, we
Rem might as well avoid unnecessary overwriting of config.h and epaths.h,
Rem since this forces recompilation of every source file.
if exist foo.bar del foo.bar
fc /b foo.bar foo.bar >nul 2>&1
if not errorlevel 2 goto doCopy
fc /b config.tmp ..\src\config.h >nul 2>&1
if errorlevel 1 goto doCopy
fc /b paths.h ..\src\epaths.h >nul 2>&1
if not errorlevel 1 goto dontCopy

:doCopy
copy config.tmp ..\src\config.h
copy paths.h ..\src\epaths.h

:dontCopy
if exist config.tmp del config.tmp
copy /b config.settings+%MAKECMD%.defs+..\nt\makefile.w32-in ..\nt\makefile
if exist ..\admin\unidata copy /b config.settings+%MAKECMD%.defs+..\admin\unidata\makefile.w32-in ..\admin\unidata\makefile
copy /b config.settings+%MAKECMD%.defs+..\lib-src\makefile.w32-in ..\lib-src\makefile
copy /b config.settings+%MAKECMD%.defs+..\lib\makefile.w32-in ..\lib\makefile
copy /b config.settings+%MAKECMD%.defs+..\src\makefile.w32-in ..\src\makefile
copy /b config.settings+%MAKECMD%.defs+..\doc\emacs\makefile.w32-in ..\doc\emacs\makefile
copy /b config.settings+%MAKECMD%.defs+..\doc\misc\makefile.w32-in ..\doc\misc\makefile
copy /b config.settings+%MAKECMD%.defs+..\doc\lispref\makefile.w32-in ..\doc\lispref\makefile
copy /b config.settings+%MAKECMD%.defs+..\doc\lispintro\makefile.w32-in ..\doc\lispintro\makefile
if exist ..\lisp\makefile rm -f ../lisp/[Mm]akefile
copy /b config.settings+%MAKECMD%.defs+..\lisp\makefile.w32-in ..\lisp\makefile
rem   Use the default (no-op) Makefile.in if the nt version is not present.
if exist ..\leim\makefile.w32-in copy /b config.settings+%MAKECMD%.defs+..\leim\makefile.w32-in ..\leim\makefile
if not exist ..\leim\makefile.w32-in copy /b config.settings+%MAKECMD%.defs+..\leim\Makefile.in ..\leim\makefile
del config.settings

Rem Some people use WinZip which doesn't create empty directories!
if not exist ..\site-lisp\nul mkdir ..\site-lisp\
Rem Update subdirs.el only if it is different or fc.exe doesn't work.
if exist foo.bar del foo.bar
fc /b foo.bar foo.bar >nul 2>&1
if not errorlevel 2 goto doUpdateSubdirs
fc /b subdirs.el ..\site-lisp\subdirs.el >nul 2>&1
if not errorlevel 1 goto dontUpdateSubdirs

:doUpdateSubdirs
if exist ..\site-lisp\subdirs.el del ..\site-lisp\subdirs.el
copy subdirs.el ..\site-lisp\subdirs.el

:dontUpdateSubdirs
echo.

rem check that we have all the libraries we need.
set libsOK=1

if not "(%HAVE_XPM%)" == "()" goto checkpng
if (%xpmsupport%) == (N) goto checkpng
 set libsOK=0
 echo XPM support is missing. It is required for color icons in the toolbar.
 echo   Install libXpm development files or use --without-xpm

:checkpng
if not "(%HAVE_PNG%)" == "()" goto checkjpeg
if (%pngsupport%) == (N) goto checkjpeg
 set libsOK=0
 echo PNG support is missing.
 echo   Install libpng development files or use --without-png

:checkjpeg
if not "(%HAVE_JPEG%)" == "()" goto checktiff
if (%jpegsupport%) == (N) goto checktiff
 set libsOK=0
 echo JPEG support is missing.
 echo   Install jpeg development files or use --without-jpeg

:checktiff
if not "(%HAVE_TIFF%)" == "()" goto checkgif
if (%tiffsupport%) == (N) goto checkgif
 set libsOK=0
 echo TIFF support is missing.
 echo   Install libtiff development files or use --without-tiff

:checkgif
if not "(%HAVE_GIF%)" == "()" goto checkdistfiles
if (%gifsupport%) == (N) goto checkdistfiles
 set libsOK=0
 echo GIF support is missing.
 echo   Install giflib or libungif development files or use --without-gif

:checkdistfiles
if "(%HAVE_DISTFILES%)" == "()" goto donelibchecks
if (%distFilesOk%) == (1) goto donelibchecks
echo.
echo Files specified with option --distfiles could not be found.
echo   Fix these issues before running make dist

:donelibchecks
if (%libsOK%) == (1) goto success
echo.
echo Important libraries are missing. Fix these issues before running make.
goto end

:success
echo Emacs successfully configured.
echo Emacs successfully configured. >>config.log
if (%MAKECMD%) == (gmake) set MAKECMD=make
echo Run `%MAKECMD%' to build, then run `%MAKECMD% install' to install.
goto end

:SmallEnv
echo Your environment size is too small.  Please enlarge it and rerun configure.
echo For example, type "command.com /e:2048" to have 2048 bytes available.
set $foo$=

:end
set prefix=
set nodebug=
set noopt=
set enablechecking=
set profile=
set nocygwin=
set COMPILER=
set MAKECMD=
set usercflags=
set docflags=
set userldflags=
set doldflags=
set mingwflag=
set mf=
set distfiles=
set HAVE_DISTFILES=
set distFilesOk=
set pngsupport=
set tlssupport=
set jpegsupport=
set gifsupport=
set tiffsupport=
set xpmsupport=
set svgsupport=
set libsOK=
set HAVE_GIF=
set HAVE_JPEG=
set HAVE_PNG=
set HAVE_TIFF=
set HAVE_XPM=
set dbginfo=

