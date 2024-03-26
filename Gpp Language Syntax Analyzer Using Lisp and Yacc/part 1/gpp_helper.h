#ifndef GPP_HELPER_H
#define GPP_HELPER_H

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

extern FILE *yyin;
extern char *text;

struct Function {
    char name[20];
	char paramID[2][20];
    int paramVal[2][2];
    char* expression;
	int paramCount;
};
struct Function functions[100]; // Assuming a maximum of 100 functions
extern int functionCount;

int* parseFraction(const char* fractionStr);
void printFraction(int *num);
int* sumValues(int* num1, int* num2);
int* subValues(int* num1, int* num2);
int* multValues(int* num1, int* num2);
int* divValues(int* num1, int* num2);
char* mergeNums(int* result);

int findFunctionIndex(char* name);
void concatenate(const char *newText);
char* extractSubstring(const char* input, int spaceCountEnd);
int* functionControl(const char* expText, int functionIndex);
char* extractUntilOPCP(const char* input, int* length);
char* extractBeforeSpace(const char* input, int* length);
void setParams(const char* param1, const char* param2, int functionIndex);

int yyerror (char *s);
int yylex();

#endif // GPP_HELPER_H