#include "gpp_helper.h"

char *text = NULL;
int functionCount = 0;

int* parseFraction(const char* fractionStr) {
    int* result = malloc(2 * sizeof(int));
    sscanf(fractionStr, "%db%d", &result[0], &result[1]);
    return result;
}
int* sumValues(int* num1, int* num2) {
    int* result = malloc(2 * sizeof(int));
    result[0] = (num1[0] * num2[1]) +  (num2[0] * num1[1]);
    result[1] = num1[1] * num2[1];
    return result;
}
int* subValues(int* num1, int* num2) {
    int* result = malloc(2 * sizeof(int));
    result[0] = (num1[0] * num2[1]) -  (num2[0] * num1[1]);
    result[1] = num1[1] * num2[1];
    return result;
}
int* multValues(int* num1, int* num2) {
    int* result = malloc(2 * sizeof(int));
    result[0] = num1[0] * num2[0];
    result[1] = num1[1] * num2[1];
    return result;
}
int* divValues(int* num1, int* num2) {
    int* result = malloc(2 * sizeof(int));
    result[0] = num1[0] * num2[1];
    result[1] = num1[1] * num2[0];
    return result;
}
char* mergeNums(int* result) {
    char* strResult = malloc(20 * sizeof(char));

    if (strResult != NULL) {
        sprintf(strResult, "%db%d", result[0], result[1]);
    } else {
        printf("Memory allocation failed in mergeNums.\n");
        exit(EXIT_FAILURE);
    }

    return strResult;
}
/* FUNCTION */
int findFunctionIndex(char* name) {
    for (int i = 0; i < functionCount; i++) {
        if (strcmp(functions[i].name, name) == 0) {
            return i;
        }
    }
    return -1; // Function not found
}

void concatenate(const char *newText) {
    if (text == NULL) {
        text = strdup(newText);
    } else {
        size_t len = strlen(text) + strlen(newText) + 2;
        text = realloc(text, len);

        if (text == NULL) {
            printf("Memory allocation failed\n");
            exit(EXIT_FAILURE); // or return an error code
        }
        strcat(text, " ");  // Add a space between the concatenated values
        strcat(text, newText);
    }
}

char* extractSubstring(const char* input, int spaceCountEnd) {
    int spaceCount = 0;
    const char* start = input;

    while (*start && spaceCount < spaceCountEnd) {
        if (*start == ' ') {
            spaceCount++;
        }
        start++;
    }
    size_t length = strlen(start) - 1;

    char* result = malloc(length + 1);

    if (result == NULL) {
        printf("Memory allocation failed\n");
        exit(EXIT_FAILURE);
    }

    strncpy(result, start, length);
    return result;
}

int* functionControl(const char* expText, int functionIndex) {
    int spaceCount = 0;
    int* num1Val;
    int* num2Val;
    int length;
    const char* start = expText;
    if(*start == '(') {
        start+=2;
        if(*start == '+') {
            start+=2;
            if(*start == '(') {
                char* extractText = extractUntilOPCP(start, &length);
                num1Val = functionControl(extractText, functionIndex);
                free(extractText);
            }else {
                char* num = extractBeforeSpace(start, &length);
                if(isdigit(*start)) {
                    num1Val = parseFraction(num);
                }else {
                    for(int i = 0;i < functions[functionIndex].paramCount;i++) {
                        if(strcmp(num, functions[functionIndex].paramID[i]) == 0) {
                            num1Val = functions[functionIndex].paramVal[i];
                        }
                    }
                }
                free(num);
            }
            start+= 1 + length;
            if(*start == '(') {
                char* extractText = extractUntilOPCP(start, &length);
                num2Val = functionControl(extractText, functionIndex);
                free(extractText);
            }else {
                char* num = extractBeforeSpace(start, &length);
                if(isdigit(*start)) {
                    num2Val = parseFraction(num);
                }else {
                    for(int i = 0;i < functions[functionIndex].paramCount;i++) {
                        if(strcmp(num, functions[functionIndex].paramID[i]) == 0) {
                            num2Val = functions[functionIndex].paramVal[i];
                        }
                    }
                }
                free(num);
            }
            return sumValues(num1Val,num2Val);
        }else if (*start == '-') {
            start+=2;
            if(*start == '(') {
                char* extractText = extractUntilOPCP(start, &length);
                num1Val = functionControl(extractText, functionIndex);
                free(extractText);
            }else {
                char* num = extractBeforeSpace(start, &length);
                if(isdigit(*start)) {
                    num1Val = parseFraction(num);
                }else {
                    for(int i = 0;i < functions[functionIndex].paramCount;i++) {
                        if(strcmp(num, functions[functionIndex].paramID[i]) == 0) {
                            num1Val = functions[functionIndex].paramVal[i];
                        }
                    }
                }
                free(num);
            }
            start+= 1 + length;
            if(*start == '(') {
                char* extractText = extractUntilOPCP(start, &length);
                num2Val = functionControl(extractText, functionIndex);
                free(extractText);
            }else {
                char* num = extractBeforeSpace(start, &length);
                if(isdigit(*start)) {
                    num2Val = parseFraction(num);
                }else {
                    for(int i = 0;i < functions[functionIndex].paramCount;i++) {
                        if(strcmp(num, functions[functionIndex].paramID[i]) == 0) {
                            num2Val = functions[functionIndex].paramVal[i];
                        }
                    }
                }
                free(num);
            }
            return subValues(num1Val,num2Val);
        }else if (*start == '*') {
            start+=2;
            if(*start == '(') {
                char* extractText = extractUntilOPCP(start, &length);
                num1Val = functionControl(extractText, functionIndex);
                free(extractText);
            }else {
                char* num = extractBeforeSpace(start, &length);
                if(isdigit(*start)) {
                    num1Val = parseFraction(num);
                }else {
                    for(int i = 0;i < functions[functionIndex].paramCount;i++) {
                        if(strcmp(num, functions[functionIndex].paramID[i]) == 0) {
                            num1Val = functions[functionIndex].paramVal[i];
                        }
                    }
                }
                free(num);
            }
            start+= 1 + length;
            if(*start == '(') {
                char* extractText = extractUntilOPCP(start, &length);
                num2Val = functionControl(extractText, functionIndex);
                free(extractText);
            }else {
                char* num = extractBeforeSpace(start, &length);
                if(isdigit(*start)) {
                    num2Val = parseFraction(num);
                }else {
                    for(int i = 0;i < functions[functionIndex].paramCount;i++) {
                        if(strcmp(num, functions[functionIndex].paramID[i]) == 0) {
                            num2Val = functions[functionIndex].paramVal[i];
                        }
                    }
                }
                free(num);
            }
            return multValues(num1Val,num2Val);
        }else if (*start == '/') {
            start+=2;
            if(*start == '(') {
                char* extractText = extractUntilOPCP(start, &length);
                num1Val = functionControl(extractText, functionIndex);
                free(extractText);
            }else {
                char* num = extractBeforeSpace(start, &length);
                if(isdigit(*start)) {
                    num1Val = parseFraction(num);
                }else {
                    for(int i = 0;i < functions[functionIndex].paramCount;i++) {
                        if(strcmp(num, functions[functionIndex].paramID[i]) == 0) {
                            num1Val = functions[functionIndex].paramVal[i];
                        }
                    }
                }
                free(num);
            }
            start+= 1 + length;
            if(*start == '(') {
                char* extractText = extractUntilOPCP(start, &length);
                num2Val = functionControl(extractText, functionIndex);
                free(extractText);
            }else {
                char* num = extractBeforeSpace(start, &length);
                if(isdigit(*start)) {
                    num2Val = parseFraction(num);
                }else {
                    for(int i = 0;i < functions[functionIndex].paramCount;i++) {
                        if(strcmp(num, functions[functionIndex].paramID[i]) == 0) {
                            num2Val = functions[functionIndex].paramVal[i];
                        }
                    }
                }
                free(num);
            }
            return divValues(num1Val,num2Val);
        }
    }
    // Find the start position after the third space
    while (*start && spaceCount < 3) {
        if(*start == '(') {

        } else {
            if (*start == ' ') {
                spaceCount++;
            }
        }
            start++;
    }
}
char* extractUntilOPCP(const char* input, int* len) {
    int isCP = 0;
    const char* end = input;

    while (*end && isCP != 1) {
        if (*end == ')') {
            isCP = 1;
        }
        else {
            end++;
        }
    }
    size_t length = end - input + 1;

    char* result = malloc(length + 1);

    if (result == NULL) {
        printf("Memory allocation failed\n");
        exit(EXIT_FAILURE);
    }
    strncpy(result, input, length);
    *len = length;
    return result;
}
char* extractBeforeSpace(const char* input, int* length) {
    // Find the index of the first space
    const char* spaceIndex = strchr(input, ' ');

    if (spaceIndex != NULL) {
        // Calculate the length before the space
        size_t lengthBeforeSpace = spaceIndex - input;

        // Allocate memory for the result and copy the substring
        char* result = (char*)malloc(lengthBeforeSpace + 1); // +1 for the null terminator
        strncpy(result, input, lengthBeforeSpace);
        result[lengthBeforeSpace] = '\0'; // Null-terminate the result
        *length = lengthBeforeSpace;
        return result;
    } else {
        // No space found, return a copy of the entire input
        return strdup(input);
    }
}
void setParams(const char* param1, const char* param2, int functionIndex) {
    if(param1 != NULL) {
        int* num = parseFraction(param1);
        functions[functionIndex].paramVal[0][0] = num[0];
        functions[functionIndex].paramVal[0][1] = num[1];
        free(num);
    }
    if(param2 != NULL) {
        int* num = parseFraction(param2);
        functions[functionIndex].paramVal[1][0] = num[0];
        functions[functionIndex].paramVal[1][1] = num[1];
        free(num);
    }
}