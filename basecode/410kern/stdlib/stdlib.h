#ifndef _STDLIB_H_
#define _STDLIB_H_

#include <aan.h>
#include <stddef.h>  /* For size_t, NULL */
#include <malloc.h>  /* "Listen to what the man says" */

WONT_SLEEP long atol(const char *__str);
#define atoi(str) ((int)atol(str))

WONT_SLEEP long strtol(const char *__p, char **__out_p, int __base);
WONT_SLEEP unsigned long strtoul(const char *__p, char **__out_p, int __base);

#define RAND_MAX 0x80000000
WONT_SLEEP int rand(void);
WONT_SLEEP void srand(unsigned new_seed);

WONT_SLEEP int abs(int val);

MAY_SLEEP void qsort(void *a, size_t n, size_t es, int (*cmp)() MAY_SLEEP);

WONT_SLEEP void panic(const char *, ...);

#endif
