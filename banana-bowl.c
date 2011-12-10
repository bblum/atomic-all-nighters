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
#define WONT_SLEEP
#define MAY_SLEEP
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

int x;

void WONT_SLEEP banana()
{
	x++;
}

void MAY_SLEEP apple()
{
	mutex_lock(m);
	x++;
	mutex_unlock(m);
}

struct banana_bowl {
	void (*f)(void) WONT_SLEEP;
};

struct fruit_bowl {
	void (*f)(void) MAY_SLEEP;
};

int MAY_SLEEP main()
{
	// This should fail.
	// The struct pointer acts as a reference cell, assignment to which
	// must be neither covariant nor contravariant.
	struct banana_bowl bananas;
	struct fruit_bowl *fruit;

	bananas.f = banana;
	fruit = &bananas;
	fruit->f = apple;
	spin_lock(a);
	bananas.f();
	spin_unlock(a);
}
