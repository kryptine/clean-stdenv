@echo off
if not exist "..\Clean System Files\." md "..\Clean System Files"
if not errorlevel 1 for %%c in (*_library., *.s, *.c) do call compile.bat %%c
pause
