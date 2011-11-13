#ifdef ATOMIC_ALL_NIGHTERS
/* function annotations */
#define MIGHT_SLEEP         __attribute__((might_sleep))

/* context-changing annotations */
#define ENTER_ATOMIC        __attribute__((enter_atomic))
#define EXIT_ATOMIC         __attribute__((exit_atomic))
#define ENTER_ATOMIC_NESTED __attribute__((enter_atomic_nested))
#define EXIT_ATOMIC_NESTED  __attribute__((exit_atomic_nested))


/* context types (really, function pointer types.) */
#define MUSTNT_SLEEP __attribute__((mustnt_sleep))
/* is MAY_SLEEP the default? what about function pointers that change the ctx? */

#else
#define MIGHT_SLEEP
#define ENTER_ATOMIC
#define EXIT_ATOMIC
#define ENTER_ATOMIC_NESTED
#define EXIT_ATOMIC_NESTED
#define MUSTNT_SLEEP
#endif

struct mutex;
struct spinlock;

void mutex_lock(struct mutex *mp) MIGHT_SLEEP;
void mutex_unlock(struct mutex *mp) MIGHT_SLEEP;

void spin_lock(struct spinlock *sp) ENTER_ATOMIC_NESTED;
void spin_unlock(struct spinlock *sp) EXIT_ATOMIC_NESTED;

struct spinlock *a;
struct spinlock *b;
struct mutex *m;

int x;
int y;
int z;

int main() {
	spin_lock(a);
	x++;
	spin_lock(b);
	y++;
	spin_unlock(*(&a));
	mutex_lock(m);
	spin_unlock(b);
	z++;
	mutex_unlock(m);
	return 0;
}
