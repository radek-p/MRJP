#ifndef RUNTIME_RUNTIME_H
#define RUNTIME_RUNTIME_H

#include <string.h>

// Concatenate two arrays of char
char *liblatteConcat(const char *s1, const char *s2);

void printInt(int);

void printString(char *);

void error();

int readInt();

char * readString();

#endif //RUNTIME_RUNTIME_H
