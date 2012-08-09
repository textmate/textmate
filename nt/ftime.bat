@echo off
if (%1)==() echo Usage: %0 tracefile
if (%1)==() goto done
rem Need to fiddle with the dumped image so prep doesn't break it
obj\i386\preprep ..\src\obj\i386\emacs.exe ..\src\obj\i386\pemacs.exe
copy ..\src\obj\i386\temacs.map ..\src\obj\i386\pemacs.map
rem -----------------------------------------------------------------
rem    Use this version to profile explicit commands only.
prep /om /ft /sf _Fexecute_extended_command ..\src\obj\i386\pemacs
rem -----------------------------------------------------------------
rem    Use this version to ignore startup code
rem prep /om /ft /sf _command_loop_1 ..\src\obj\i386\pemacs
rem -----------------------------------------------------------------
rem    Use this version to include startup code
rem prep /om /ft ..\src\obj\i386\pemacs
rem -----------------------------------------------------------------
if errorlevel 1 goto done
profile ..\src\obj\i386\pemacs %2 %3 %4 %5 %6 %7 %8 %9
if errorlevel 1 goto done
prep /m  ..\src\obj\i386\pemacs
if errorlevel 1 goto done
plist ..\src\obj\i386\pemacs > %1
:done

