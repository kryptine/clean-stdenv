@echo off
if (%2)==() goto error
..\bin\makedx %1
..\bin\map2icl %1 %2
copy %1.bmp ..\*.bmp
del %1.bmp
copy %1.dcl ..\*.dcl
del %1.dcl
copy %1.icl ..\*.icl
del %1.icl
if exist %1.lev del %1.lev
if exist %1.seq del %1.seq
goto end
:error
echo usage: conv levelfile identifier
:end
