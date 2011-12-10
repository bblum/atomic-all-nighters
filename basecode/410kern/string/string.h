/* 
 * Copyright (c) 1994 The University of Utah and
 * the Computer Systems Laboratory at the University of Utah (CSL).
 * All rights reserved.
 *
 * Permission to use, copy, modify and distribute this software is hereby
 * granted provided that (1) source code retains these copyright, permission,
 * and disclaimer notices, and (2) redistributions including binaries
 * reproduce the notices in supporting documentation, and (3) all advertising
 * materials mentioning features or use of this software display the following
 * acknowledgement: ``This product includes software developed by the
 * Computer Systems Laboratory at the University of Utah.''
 *
 * THE UNIVERSITY OF UTAH AND CSL ALLOW FREE USE OF THIS SOFTWARE IN ITS "AS
 * IS" CONDITION.  THE UNIVERSITY OF UTAH AND CSL DISCLAIM ANY LIABILITY OF
 * ANY KIND FOR ANY DAMAGES WHATSOEVER RESULTING FROM THE USE OF THIS SOFTWARE.
 *
 * CSL requests users of this software to return to csl-dist@cs.utah.edu any
 * improvements that they make and grant CSL redistribution rights.
 */
#ifndef _FLUX_MC_STRING_H_
#define _FLUX_MC_STRING_H_

#include <aan.h>
#include <types.h>

WONT_SLEEP size_t strlen(const char *__s);
WONT_SLEEP char *strcpy(char *__dest, const char *__src);
WONT_SLEEP char *strncpy(char *__dest, const char *__src, size_t __n);
WONT_SLEEP char *strdup(const char *__s);
WONT_SLEEP char *strcat(char *__dest, const char *__src);
WONT_SLEEP char *strncat(char *__dest, const char *__src, size_t __n);
WONT_SLEEP int strcmp(const char *__a, const char *__b);
WONT_SLEEP int strncmp(const char *__a, const char *__b, size_t __n);
WONT_SLEEP char *strchr(const char *__s, int __c);
WONT_SLEEP char *strrchr(const char *__s, int __c);
WONT_SLEEP char *strstr(const char *__haystack, const char *__needle);
WONT_SLEEP char *strtok(char *__s, const char *__delim);
WONT_SLEEP char *strpbrk(const char *__s1, const char *__s2);
WONT_SLEEP size_t strspn(const char *__s1, const char *__s2);
WONT_SLEEP size_t strcspn(const char *__s1, const char *__s2);

WONT_SLEEP void *memset(void *__to, int __ch, unsigned int __n);
WONT_SLEEP int memcmp(const void *s1v, const void *s2v, int size);

/* FIXME These are defined here only by tradition... we should move them. */
WONT_SLEEP void *memcpy(void *__to, const void *__from, unsigned int __n);
WONT_SLEEP void *memmove(void *__to, const void *__from, unsigned int __n);

/*** BSD compatibility functions; do not use in new code ***/

WONT_SLEEP char *index(const char *__s, int __c);
WONT_SLEEP char *rindex(const char *__s, int __c);

WONT_SLEEP void bcopy(const void *__from, void *__to, unsigned int __n);
WONT_SLEEP void bzero(void *__to, unsigned int __n);

#endif	/* _FLUX_MC_STRING_H_ */
