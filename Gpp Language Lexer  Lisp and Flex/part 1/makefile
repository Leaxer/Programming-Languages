FILE = gpp_lexer

INPUTFILE = test.txt

flex: ${FILE}.l
	flex -o ${FILE}.c $(FILE).l
	gcc ${FILE}.c -o ${FILE}
	./${FILE}

input:
	./${FILE} ${INPUTFILE}