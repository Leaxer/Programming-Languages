INPUTFILE = test.txt

flex: gpp_lexer.l
	flex gpp_lexer.l
	yacc -d gpp_interpreter.y
	gcc -g lex.yy.c y.tab.c gpp_helper.c -ll -o a.out
	./a.out
input:
	./a.out ${INPUTFILE}