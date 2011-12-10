/*
 * Copyright (c) 1996-1994 The University of Utah and
 * the Computer Systems Laboratory (CSL).  All rights reserved.
 *
 * Permission to use, copy, modify and distribute this software and its
 * documentation is hereby granted, provided that both the copyright
 * notice and this permission notice appear in all copies of the
 * software, derivative works or modified versions, and any portions
 * thereof, and that both notices appear in supporting documentation.
 *
 * THE UNIVERSITY OF UTAH AND CSL ALLOW FREE USE OF THIS SOFTWARE IN ITS "AS
 * IS" CONDITION.  THE UNIVERSITY OF UTAH AND CSL DISCLAIM ANY LIABILITY OF
 * ANY KIND FOR ANY DAMAGES WHATSOEVER RESULTING FROM THE USE OF THIS SOFTWARE.
 *
 * CSL requests users of this software to return to csl-dist@cs.utah.edu any
 * improvements that they make and grant CSL redistribution rights.
 */
#ifndef _FLUX_MC_CTYPE_H_
#define _FLUX_MC_CTYPE_H_

#include <aan.h>

WONT_SLEEP int isascii(int c);
WONT_SLEEP int iscntrl(int c);
WONT_SLEEP int isdigit(int c);
WONT_SLEEP int isgraph(int c);
WONT_SLEEP int islower(int c);
WONT_SLEEP int isprint(int c);
WONT_SLEEP int isspace(int c);
WONT_SLEEP int isupper(int c);
WONT_SLEEP int isxdigit(int c);
WONT_SLEEP int isalpha(int c);
WONT_SLEEP int isalnum(int c);
WONT_SLEEP int ispunct(int c);
WONT_SLEEP int toupper(int c);
WONT_SLEEP int tolower(int c);


#endif	/* _FLUX_MC_CTYPE_H_ */
