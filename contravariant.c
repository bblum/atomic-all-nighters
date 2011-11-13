#ifdef ATOMIC_ALL_NIGHTERS
/* function annotations */
#define MIGHT_SLEEP         __attribute__((might_sleep))
#define DOESNT_SLEEP        __attribute__((doesnt_sleep))

/* context-changing annotations */
#define ENTER_ATOMIC        __attribute__((enter_atomic))
#define EXIT_ATOMIC         __attribute__((exit_atomic))
#define ENTER_ATOMIC_NESTED __attribute__((enter_atomic_nested))
#define EXIT_ATOMIC_NESTED  __attribute__((exit_atomic_nested))


/* context types (really, function pointer types.) */
#define MUSTNT_SLEEP __attribute__((mustnt_sleep))
#define MAY_SLEEP __attribute__((may_sleep))
/* is MAY_SLEEP the default? what about function pointers that change the ctx? */

#else
#define MIGHT_SLEEP
#define DOESNT_SLEEP
#define ENTER_ATOMIC
#define EXIT_ATOMIC
#define ENTER_ATOMIC_NESTED
#define EXIT_ATOMIC_NESTED
#define MUSTNT_SLEEP
#define MAY_SLEEP
#endif

struct mutex;
struct spinlock;

void mutex_lock(struct mutex *mp) MIGHT_SLEEP;
void mutex_unlock(struct mutex *mp) MIGHT_SLEEP;
void mutex_assert_is_locked(struct mutex *mp) DOESNT_SLEEP;

void spin_lock(struct spinlock *sp) ENTER_ATOMIC_NESTED;
void spin_unlock(struct spinlock *sp) EXIT_ATOMIC_NESTED;

struct spinlock *a;
struct mutex *m;

void (*h)(struct mutex *) MAY_SLEEP;

void g(void (*x)(struct mutex *) MAY_SLEEP) DOESNT_SLEEP
{
	h = x;
}

int main() MIGHT_SLEEP {
	void (*f)(void (*)(struct mutex *) MUSTNT_SLEEP) MUSTNT_SLEEP = g;
	spin_lock(a);
	f(mutex_assert_is_locked);
	spin_unlock(a);
	h(m);
	return 0;
}
