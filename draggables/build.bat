set SRC=src/Main.elm
set OUT=main.js
set DST=../../kotya-web/static/elm

elm make %SRC% --output="%OUT%"

copy %OUT% "%DST%/%OUT%"
copy index.html "%DST%/index.html"
