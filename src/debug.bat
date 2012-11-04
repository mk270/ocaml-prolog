@echo off

rem Compilation with parser debugging information.
rem Should be executed only in command line, as it sets environment variable.

set OCAMLRUNPARAM='p'
echo Creating debugging information...
echo Compiling to destination ./opl.exe
echo.
ocamlc -c types.ml
ocamlyacc -v parser.mly 2> debug.info
ocamlc -c parser.mli
ocamlc -c parser.ml
ocamllex lexer.mll
ocamlc -c lexer.ml
ocamlc -o ./opl.exe lexer.cmo parser.cmo opl.ml
del *cmi 
del *cmo
del lexer.ml
del parser.mli
del parser.ml
echo Finished.