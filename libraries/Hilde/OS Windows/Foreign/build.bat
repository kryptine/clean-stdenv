@echo off
set path=C:\MinGW 1.1\bin;%path%
if not exist "..\Clean System Files\." md "..\Clean System Files"
if not errorlevel 1 for %%c in (*_library., *.s, *.c) do call compile.bat %%c
pause
