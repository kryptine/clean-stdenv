@echo off
if (%1) == () goto error
echo creating directory %1
mkdir %1
copy bin\default.pal %1
copy bin\*.bat %1
goto end
:error
echo usage: newdir NAME
:end
