@echo off
if (%2)==() goto error
..\bin\makedx %1
..\bin\map2icl %1 %2
copy %1.bmp ..
del %1.bmp
copy %1.dcl ..
del %1.dcl
copy %1.icl ..
del %1.icl
goto end
:error
echo usage: conv levelfile identifier
:end
