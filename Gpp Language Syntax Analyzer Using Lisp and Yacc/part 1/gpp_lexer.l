%{
#include "y.tab.h"
#include "gpp_helper.h"
extern YYSTYPE yylval;
%}

%%

(\r\n|\r|\n)                 {return 0;}
[a][n][d]                   { concatenate(yytext);
                               return KW_AND; }
[o][r]                      { concatenate(yytext);
                               return KW_OR; }
[n][o][t]                   { concatenate(yytext);
                               return KW_NOT; }
[e][q][u][a][l]             { concatenate(yytext);
                               return KW_EQUAL; }
[l][e][s][s]                { concatenate(yytext);
                               return KW_LESS; }
[n][i][l]                   { concatenate(yytext);
                               return KW_NIL; }
[l][i][s][t]                { concatenate(yytext);
                               return KW_LIST; }
[a][p][p][e][n][d]           { concatenate(yytext);
                               return KW_APPEND; }
[c][o][n][c][a][t]           { concatenate(yytext);
                               return KW_CONCAT; }
[s][e][t]                   { concatenate(yytext);
                               return KW_SET; }
[d][e][f]                    { concatenate(yytext);
                               return KW_DEF; }
[f][o][r]                    { concatenate(yytext);
                               return KW_FOR; }
[i][f]                       { concatenate(yytext);
                               return KW_IF; }
[e][x][i][t]                 { concatenate(yytext);
                               return KW_EXIT; }
[l][o][a][d]                 { concatenate(yytext);
                               return KW_LOAD; }
[d][i][s][p][l][a][y]        { concatenate(yytext);
                               return KW_DISPLAY; }
[t][r][u][e]                 { concatenate(yytext);
                               return KW_TRUE; }
[f][a][l][s][e]               { concatenate(yytext);
                                return KW_FALSE; }

[+]                          { concatenate(yytext);
                               return OP_PLUS; }
[-]                          { concatenate(yytext);
                               return OP_MINUS; }
[/]                          { concatenate(yytext);
                               return OP_DIV; }
[*]                          { concatenate(yytext);
                               return OP_MULT; }
[(]                          { concatenate(yytext);
                               return OP_OP; }
[)]                          { concatenate(yytext);
                               return OP_CP; }
[,]                          { concatenate(yytext);
                               return OP_COMMA; }

[;][;].*                     { return COMMENT; }
[[:space:]]+ 		        /* ignore space */

[0-9]+[b][0-9]+              {  concatenate(yytext);
                                yylval.value = parseFraction(yytext);
                                return VALUEF;}

[a-zA-Z]+[a-zA-Z0-9]*        {  concatenate(yytext);
                                yylval.id = strdup(yytext);
                                return IDENTIFIER;
                            }

[0-9][0-9a-zA-Z]*            {
                                // Identifiers cannot have a digit as a leading character
                                { printf("%s cannot be tokenized\n", yytext);} 
                            }

[a-zA-Z]*[,+-/*]	 	  	  {printf("SYNTAX ERROR\n"); return 0;} 
[,][A-zA-Z0-9+-/*,]*	 	  {printf("SYNTAX ERROR\n"); return 0;}
[+][A-zA-Z0-9,-/*+]*	 	  {printf("SYNTAX ERROR\n"); return 0;}	
[-][A-zA-Z0-9+,/*-]*	 	  {printf("SYNTAX ERROR\n"); return 0;}	
[/][A-zA-Z0-9+-,*/]*	 	  {printf("SYNTAX ERROR\n"); return 0;}
[*][A-zA-Z0-9+-,/*]*	 	  {printf("SYNTAX ERROR\n"); return 0;}
.                            {return 0;}

%%

int yywrap() {}
