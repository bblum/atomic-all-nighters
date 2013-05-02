#ifdef ATOMIC_ALL_NIGHTERS
/* function annotations */
#define MAY_SLEEP           __attribute__((atomic_all_nighters("might_sleep")))

/* context-changing annotations */
#define ENTER_ATOMIC        __attribute__((atomic_all_nighters("wont_sleep","force_disable")))
#define EXIT_ATOMIC         __attribute__((atomic_all_nighters("wont_sleep","force_enable")))
#define ENTER_ATOMIC_NESTED __attribute__((atomic_all_nighters("wont_sleep","enter_nested")))
#define EXIT_ATOMIC_NESTED  __attribute__((atomic_all_nighters("wont_sleep","exit_nested")))

#else
#define MAY_SLEEP
#define ENTER_ATOMIC
#define EXIT_ATOMIC
#define ENTER_ATOMIC_NESTED
#define EXIT_ATOMIC_NESTED
#endif

struct mutex;
struct spinlock;

void mutex_lock(struct mutex *mp) MAY_SLEEP;
void mutex_unlock(struct mutex *mp) MAY_SLEEP;

void spin_lock(struct spinlock *sp) ENTER_ATOMIC_NESTED;
void spin_unlock(struct spinlock *sp) EXIT_ATOMIC_NESTED;

struct spinlock *a;
struct spinlock *b;
struct mutex *m;

int x;
int y;
int z;

ENTER_ATOMIC void disable_interrupts();
EXIT_ATOMIC void enable_interrupts();

int MAY_SLEEP main() {
	/* This should fail (exit context != advertised effect). */
	if (x == 0) {
		goto foo;
		return 1;
foo:
		spin_lock(a);
	} else {
		goto bar;
		return 2;
bar:
		spin_lock(b);
	}

	return 0;
}
