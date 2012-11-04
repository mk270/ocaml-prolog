@echo off

rem Simple compilation script.

echo Compiling to destination ../bin/opl.exe
echo.
ocamlc -c types.ml
ocamlyacc parser.mly
ocamlc -c parser.mli
ocamlc -c parser.ml
ocamllex lexer.mll
ocamlc -c lexer.ml
ocamlc -c unificator.mli
ocamlc -c unificator.ml
ocamlc -c evaluator.mli
ocamlc -c evaluator.ml
ocamlc -o ../bin/opl.exe lexer.cmo parser.cmo unificator.cmo evaluator.cmo opl.ml
del *cmi 
del *cmo
del lexer.ml
del parser.mli
del parser.ml
echo Finished.
PAUSE
