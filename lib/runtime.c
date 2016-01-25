#include <stddef.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include "runtime.h"

const char *liblatteConcat(const char *s1, const char *s2) {

    char *result;
    size_t new_length;

    if (s1 == NULL) return s2;
    if (s2 == NULL) return s1;

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

void printString(const char *string) {
    if (string == NULL) {
        if (printf("\n") < 0)
            error();
    } else {
        if (printf("%s\n", string) < 0)
        error();
    }
}

void error() {
    printf("runtime error\n");

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
    size_t size;
    ssize_t read;

    result = NULL;
    size   = 0;

    do {
        read = getline(&result, &size, stdin);
        if (read == -1)
            error();
        
    } while (read == 1);

    if (read > 0)
        result[read - 1] = '\0';

    return result;
}
