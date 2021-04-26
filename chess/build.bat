echo off

set SRC=src/Main.elm
set OUT=main.js
set DST=../../kotya-web/static/elm
set BUILDPATH=build

if "%1" == "o" goto :optimize

:build
  elm make %SRC% --output="%BUILDPATH%/%OUT%"
  goto :copy

:optimize
  elm make %SRC% --output="%BUILDPATH%/%OUT%" --optimize
  uglifyjs "%BUILDPATH%/%OUT%" --compress "pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe" | uglifyjs --mangle --output "%BUILDPATH%/%OUT%"
  goto :copy

:copy
  xcopy %BUILDPATH% "%DST%" /y /s > nul
  goto :exit

:exit
  copy "..\..\kotya-web\handlers\WSChess.py" "server/WSChess.py" > nul
  copy "..\..\kotya-web\modules\chessdb\chessdb.py" "server/chessdb.py" > nul

  echo on
