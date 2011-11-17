#ifdef ATOMIC_ALL_NIGHTERS
/* function annotations */
#define MIGHT_SLEEP         __attribute__((atomic_all_nighters("might_sleep")))

/* context-changing annotations */
#define ENTER_ATOMIC        __attribute__((atomic_all_nighters("wont_sleep","force_disable")))
#define EXIT_ATOMIC         __attribute__((atomic_all_nighters("wont_sleep","force_enable")))
#define ENTER_ATOMIC_NESTED __attribute__((atomic_all_nighters("wont_sleep","enter_nested")))
#define EXIT_ATOMIC_NESTED  __attribute__((atomic_all_nighters("wont_sleep","exit_nested")))

#else
#define MIGHT_SLEEP
#define ENTER_ATOMIC
#define EXIT_ATOMIC
#define ENTER_ATOMIC_NESTED
#define EXIT_ATOMIC_NESTED
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
	// return 0;
}
