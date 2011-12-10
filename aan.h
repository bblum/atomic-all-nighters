/**
 * @file aan.h
 * @brief Macro definitions for atomic all-nighters.
 * @author Ben Blum <bblum@andrew.cmu.edu>
 */

#ifndef __AAN_H
#define __AAN_H

#ifdef ATOMIC_ALL_NIGHTERS
/* function annotations */
#define MAY_SLEEP           __attribute__((atomic_all_nighters("might_sleep")))
#define WONT_SLEEP          __attribute__((atomic_all_nighters("wont_sleep")))
#define INT_HANDLER         __attribute__((atomic_all_nighters("int_handler","exit_nested")))

/* context-changing annotations */
#define ENTER_ATOMIC        __attribute__((atomic_all_nighters("wont_sleep","force_disable")))
#define EXIT_ATOMIC         __attribute__((atomic_all_nighters("wont_sleep","force_enable")))
#define ENTER_ATOMIC_NESTED __attribute__((atomic_all_nighters("wont_sleep","enter_nested")))
#define EXIT_ATOMIC_NESTED  __attribute__((atomic_all_nighters("wont_sleep","exit_nested")))

#else
#define MAY_SLEEP
#define WONT_SLEEP
#define INT_HANDLER
#define ENTER_ATOMIC
#define EXIT_ATOMIC
#define ENTER_ATOMIC_NESTED
#define EXIT_ATOMIC_NESTED
#endif

#endif
