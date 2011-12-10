#ifdef ATOMIC_ALL_NIGHTERS
/* function annotations */
#define MAY_SLEEP         __attribute__((atomic_all_nighters("might_sleep")))
#define WONT_SLEEP        __attribute__((atomic_all_nighters("wont_sleep")))

/* context-changing annotations */
#define ENTER_ATOMIC        __attribute__((atomic_all_nighters("wont_sleep","force_enable")))
#define EXIT_ATOMIC         __attribute__((atomic_all_nighters("wont_sleep","force_disable")))
#define ENTER_ATOMIC_NESTED __attribute__((atomic_all_nighters("wont_sleep","enter_nested")))
#define EXIT_ATOMIC_NESTED  __attribute__((atomic_all_nighters("wont_sleep","exit_nested")))

#else
#define MAY_SLEEP
#define WONT_SLEEP
#define ENTER_ATOMIC
#define EXIT_ATOMIC
#define ENTER_ATOMIC_NESTED
#define EXIT_ATOMIC_NESTED
#endif

struct mutex;
struct spinlock;

void mutex_lock(struct mutex *mp) MAY_SLEEP;
void mutex_unlock(struct mutex *mp) MAY_SLEEP;
void mutex_assert_is_locked(struct mutex *mp) WONT_SLEEP;

void spin_lock(struct spinlock *sp) ENTER_ATOMIC_NESTED;
void spin_unlock(struct spinlock *sp) EXIT_ATOMIC_NESTED;

struct spinlock *a;
struct mutex *m;

/* h2 <: h1 */

void (*h1)(struct mutex *) MAY_SLEEP;
void (*h2)(struct mutex *) WONT_SLEEP;

/* g1 <: g2 */

void WONT_SLEEP g1(void (*x)(struct mutex *) MAY_SLEEP)
{
	h1 = x;
}

void MAY_SLEEP g2(void (*x)(struct mutex *) WONT_SLEEP)
{
	mutex_lock(m);
	h2 = x;
	mutex_unlock(m);
}


int MAY_SLEEP main()
{
	/* this should fail - have to use union on the return type */
	void (*f)(void (*)(struct mutex *) WONT_SLEEP) WONT_SLEEP = 0 ? g1 : g2;
	spin_lock(a);
	f(mutex_assert_is_locked);
	spin_unlock(a);
	h2(m);
	// return 0;
}
