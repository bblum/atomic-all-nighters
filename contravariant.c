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

void MAY_SLEEP mutex_lock(struct mutex *mp);
void MAY_SLEEP mutex_unlock(struct mutex *mp);
void WONT_SLEEP mutex_assert_is_locked(struct mutex *mp);

void ENTER_ATOMIC_NESTED spin_lock(struct spinlock *sp);
void EXIT_ATOMIC_NESTED spin_unlock(struct spinlock *sp);

struct spinlock *a;
struct mutex *m;

void (*h)(struct mutex *) MAY_SLEEP;

void WONT_SLEEP g(void (*x)(struct mutex *) MAY_SLEEP)
{
	h = x;
}

int MAY_SLEEP main()
{
	// This should succeed.
	// Argument types contravary, so (may_sleep -> ()) <: (wont_sleep -> ())
	void (*f)(void (*)(struct mutex *) WONT_SLEEP) WONT_SLEEP = g;
	spin_lock(a);
	f(mutex_assert_is_locked);
	spin_unlock(a);
	h(m);
}
