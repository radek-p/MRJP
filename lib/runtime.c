#include <stddef.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include "runtime.h"

char *liblatteConcat(const char *s1, const char *s2) {

    char *result;
    size_t new_length;

    new_length = strlen(s1) + strlen(s2) + 1;
    result = malloc(sizeof(char) * new_length);
    result[0] = '\0';
    strcat(result, s1);
    strcat(result, s2);

    return result;
}

void printInt(int i) {
    if (printf("%d\n", i) < 0)
        error();
}

void printString(char *string) {
    if (printf("%s\n", string) < 0)
        error();
}

void error() {
    printf("runtime error");

    exit(1);
}

int readInt() {
    int result;

    if (scanf("%d", &result) != 1)
        error();

    return result;
}

char *readString() {
    char * result;

    result = NULL;

    if (getline(&result, NULL, stdin) == -1)
        error();

    return result;
}
