echo off

set BUILDPATH=build
set DST=..\..\kotya-web\static\oo

xcopy %BUILDPATH% "%DST%" /y /s
goto :exit

:exit
  echo on
