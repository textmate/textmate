@echo off
rem  Run temacs.exe to recreate the dumped emacs.exe.
rem  This is only likely to be useful on systems where the prebuilt
rem  emacs.exe crashes on startup.

rem First save original emacs.exe if present
if exist emacs.exe.orig goto dump
echo Saving original emacs.exe as emacs.exe.orig
ren emacs.exe emacs.exe.orig

:dump
rem Overwrites emacs.exe if still present
mkdir obj
mkdir obj\i386
mkdir obj\etc
copy ..\etc\DOC* obj\etc
copy temacs.exe obj\i386
obj\i386\temacs -batch -l loadup dump
copy obj\i386\emacs.exe .

