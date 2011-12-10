/* The 15-410 kernel.
 * syscall.h
 *
 * Prototypes for the user land C library interface 
 * to system calls.
 *
 */

#ifndef _SYSCALL_H
#define _SYSCALL_H

#define NORETURN __attribute__((__noreturn__))

#define PAGE_SIZE 0x0001000 /* 4096 */

#include <aan.h>

/* Life cycle */
MAY_SLEEP int fork(void);
MAY_SLEEP int exec(char *execname, char *argvec[]);
MAY_SLEEP void set_status(int status);
MAY_SLEEP void vanish(void) NORETURN;
MAY_SLEEP int wait(int *status_ptr);
MAY_SLEEP void task_vanish(int status) NORETURN;

/* Thread management */
MAY_SLEEP int gettid(void);
MAY_SLEEP int yield(int pid);
MAY_SLEEP int deschedule(int *flag);
MAY_SLEEP int make_runnable(int pid);
MAY_SLEEP int get_ticks();
MAY_SLEEP int sleep(int ticks);

/* Memory management */
MAY_SLEEP int new_pages(void * addr, int len);
MAY_SLEEP int remove_pages(void * addr);

/* Console I/O */
MAY_SLEEP char getchar(void);
MAY_SLEEP int readline(int size, char *buf);
MAY_SLEEP int print(int size, char *buf);
MAY_SLEEP int set_term_color(int color);
MAY_SLEEP int set_cursor_pos(int row, int col);
MAY_SLEEP int get_cursor_pos(int *row, int *col);

/* Color values for set_term_color() */
#define FGND_BLACK 0x0
#define FGND_BLUE  0x1
#define FGND_GREEN 0x2
#define FGND_CYAN  0x3
#define FGND_RED   0x4
#define FGND_MAG   0x5
#define FGND_BRWN  0x6
#define FGND_LGRAY 0x7 /* Light gray. */
#define FGND_DGRAY 0x8 /* Dark gray. */
#define FGND_BBLUE 0x9 /* Bright blue. */
#define FGND_BGRN  0xA /* Bright green. */
#define FGND_BCYAN 0xB /* Bright cyan. */
#define FGND_PINK  0xC
#define FGND_BMAG  0xD /* Bright magenta. */
#define FGND_YLLW  0xE
#define FGND_WHITE 0xF

#define BGND_BLACK 0x00
#define BGND_BLUE  0x10
#define BGND_GREEN 0x20
#define BGND_CYAN  0x30
#define BGND_RED   0x40
#define BGND_MAG   0x50
#define BGND_BRWN  0x60
#define BGND_LGRAY 0x70 /* Light gray. */

/* Miscellaneous */
MAY_SLEEP void halt();
MAY_SLEEP int ls(int size, char *buf);

/* "Special" */
MAY_SLEEP void misbehave(int mode);

/* Project 4 F2010 */
#include <ureg.h> /* may be directly included by kernel guts */
typedef void (*swexn_handler_t)(void *arg, ureg_t *ureg) MAY_SLEEP;
MAY_SLEEP int swexn(void *esp3, swexn_handler_t eip, void *arg, ureg_t *newureg);

/* Previous API */
/*
void exit(int status) NORETURN;
void task_exit(int status) NORETURN;
int cas2i_runflag(int tid, int *oldp, int ev1, int nv1, int ev2, int nv2);
*/

#endif /* _SYSCALL_H */
