%{

#include "stdio.h"

%}

%%
            /* Keywords */
[a][n][d]                   { printf("KW_AND\n");}
[o][r]                      { printf("KW_OR\n");}
[n][o][t]                   { printf("KW_NOT\n");}
[e][q][u][a][l]             { printf("KW_EQUAL\n");}
[l][e][s][s]                { printf("KW_LESS\n");}
[n][i][l]                   { printf("KW_NIL\n");}
[l][i][s][t]                { printf("KW_LIST\n");}
[a][p][p][e][n][d]	    	{ printf("KW_APPEND\n");}
[c][o][n][c][a][t]	    	{ printf("KW_CONCAT\n");}
[s][e][t]					{ printf("KW_SET\n");}
[d][e][f]	            	{ printf("KW_DEF\n");}
[f][o][r]					{ printf("KW_FOR\n");}
[i][f]						{ printf("KW_IF\n");}
[e][x][i][t]				{ printf("KW_EXIT\n");}
[l][o][a][d]				{ printf("KW_LOAD\n");}
[d][i][s][p][l][a][y]		{ printf("KW_DISPLAY\n");}
[t][r][u][e]				{ printf("KW_TRUE\n");}
[f][a][l][s][e]	            { printf("KW_FALSE\n");}

            /* Operators */

[+] 							{ printf("OP_PLUS\n");}
[-] 							{ printf("OP_MINUS\n");}
[/] 							{ printf("OP_DIV\n");}
[*] 							{ printf("OP_MULT\n");}
[(] 							{ printf("OP_OP\n");}
[)] 							{ printf("OP_CP\n");}
[,]                             { printf("OP_COMMA\n");}

            /* Comment */
[;][;].*                        { printf("COMMENT\n");}

			/* VALUEF */
[0-9]+[b][0-9]+                 { printf("VALUEF\n");}

            /* IDENTIFIER */
[a-zA-Z]+[a-zA-Z0-9]*           {
                                    // Any combination of alphabetical characters and digits with noleading digit.
                                    { printf("IDENTIFIER\n");}
                                }

[ \t\n]                         { ;} // ignore spaces, tabs and newlines
            /* Syntax Errors */
[0-9][0-9a-zA-Z]*               {
                                    // Identifiers can not have digit as a leading character
                                    { printf("%s cannot be tokenized\n",yytext);} 
                                }

.                               { ;} 
[\n]{2}		                    { return 0;}

%%


int yywrap(){}
int main(int argc , char** argv){
// yywrap() - wraps the above rule section
// yylex() - this is the main flex function which runs the Rule Section
if (argc == 1) {
        printf("Press enter to exit or enter an input\n");
        printf("Enter the input:");
    }
    else if (argc == 2)
    {
        FILE *fp;
        fp = fopen(argv[1], "r");
        yyin = fp;
    }
    else 
    {
        printf("Too many arguments\n");
        return 0;
    }

    yylex();
    return 0;
 
return 0;
}