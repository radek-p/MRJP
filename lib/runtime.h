#ifndef RUNTIME_RUNTIME_H
#define RUNTIME_RUNTIME_H

#include <string.h>

// Concatenate two arrays of char
const char *liblatteConcat(const char *, const char *);

void printInt(int);

void printString(const char *);

void error();

int readInt();

char * readString();

#endif //RUNTIME_RUNTIME_H
