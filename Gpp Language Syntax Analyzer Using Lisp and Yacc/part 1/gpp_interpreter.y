%{
#include "gpp_helper.h"
%}

%union {
    int* value;
    char* id;
}

%start START
%token STRING COMMENT OP_PLUS OP_MINUS OP_DIV OP_MULT OP_OP OP_CP OP_DBLMULT OP_OC OP_CC OP_COMMA KW_AND KW_OR KW_NOT KW_EQUAL KW_LESS
KW_NIL KW_LIST KW_APPEND KW_CONCAT KW_SET KW_DEF KW_FOR KW_IF KW_EXIT
KW_LOAD KW_DISPLAY KW_TRUE KW_FALSE NEWLINE


%token <value> VALUEF
%token <id> IDENTIFIER

%type <id> EXP
%type FUNCTION
%type EXIT

%%

START: 
    EXP {printf("Result = %s\n", $1);}
    | FUNCTION {}
    | EXIT {exit(0);}
    ;

EXP:
    OP_OP OP_PLUS EXP EXP OP_CP  {
        int* num1 = parseFraction($3);
        int* num2 = parseFraction($4);
        int* res = sumValues(num1, num2);
        $$ = mergeNums(res);
    }
    |
    OP_OP OP_MINUS EXP EXP OP_CP {
        int* num1 = parseFraction($3);
        int* num2 = parseFraction($4);
        int* res = subValues(num1, num2);
        $$ = mergeNums(res);
    }
    |
    OP_OP OP_MULT EXP EXP OP_CP  {
        int* num1 = parseFraction($3);
        int* num2 = parseFraction($4);
        int* res = multValues(num1, num2);
        $$ = mergeNums(res);
    }
    |
    OP_OP OP_DIV EXP EXP OP_CP   {
        int* num1 = parseFraction($3);
        int* num2 = parseFraction($4);
        int* res = divValues(num1, num2);
        $$ = mergeNums(res);
    }
    | OP_OP IDENTIFIER OP_CP {
        int functionIndex = findFunctionIndex($2);
        if(functionIndex != -1) {
            $$ = mergeNums(functionControl(functions[functionIndex].expression, functionIndex));
        }
    }
    | OP_OP IDENTIFIER EXP OP_CP {
        int functionIndex = findFunctionIndex($2);
        if(functionIndex != -1) {
            setParams($3, NULL, functionIndex);
            $$ = mergeNums(functionControl(functions[functionIndex].expression, functionIndex));
        }
    }
    | OP_OP IDENTIFIER EXP EXP OP_CP {
        int functionIndex = findFunctionIndex($2);
        if(functionIndex != -1) {
            setParams($3, $4, functionIndex);
            $$ = mergeNums(functionControl(functions[functionIndex].expression, functionIndex));
        }
    }
    |
    IDENTIFIER {
        $$ = $1;}
    | 
    VALUEF {
        $$ = mergeNums($1);}
    ;
    
FUNCTION: OP_OP KW_DEF IDENTIFIER EXP OP_CP {
        strcpy(functions[functionCount].name, $3);
        char* expText = extractSubstring(text, 3);
        functions[functionCount].expression = strdup(expText);
        free(expText);
        functionCount++;
    }
    |OP_OP KW_DEF IDENTIFIER IDENTIFIER EXP OP_CP {
        strcpy(functions[functionCount].name, $3);
        char* expText = extractSubstring(text, 4);
        functions[functionCount].expression = strdup(expText);
        free(expText);
        strcpy(functions[functionCount].paramID[0], $4);
        functions[functionCount].paramCount = 1;
        functionCount++;
    }
    |OP_OP KW_DEF IDENTIFIER IDENTIFIER IDENTIFIER EXP OP_CP {
        strcpy(functions[functionCount].name, $3);
        char* expText = extractSubstring(text, 5);
        functions[functionCount].expression = strdup(expText);
        free(expText);
        strcpy(functions[functionCount].paramID[0], $4);
        strcpy(functions[functionCount].paramID[1], $5);
        functions[functionCount].paramCount = 2;
        functionCount++;
    }
    ;
EXIT: OP_OP KW_EXIT OP_CP {
    }
    ;
%%

/* For printing error messages */
int yyerror(char *s) {
    fprintf(stderr, "SYNTAX ERROR. \n");
    return 0;
}

int main(int argc, char *argv[])
{
    
    if (argc == 1)
    {
        printf("Type (exit) for exit\n");
        printf("Enter your input\n");
        while (1) {
            if (text != NULL) {
                free(text);
                text = NULL;
            }
            yyparse();
        }
    }
    else if (argc == 2)
    {
        yyin = fopen(argv[1], "r");
        if (yyin == NULL) {
            perror("Error opening file");
            return 1;
        }
        while (1) {
            if (text != NULL) {
                free(text);
                text = NULL;
            }
            yyparse();
        }
        fclose(yyin);
    }
    else 
    {
        printf("Too many arguments\n");
        return 0;
    }
}