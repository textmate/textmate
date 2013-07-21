#ifndef SETUP_H_5FEH3ZO0
#define SETUP_H_5FEH3ZO0

void onig_lock_mutex();
void onig_unlock_mutex();

#ifndef THREAD_ATOMIC_START
#define THREAD_ATOMIC_START  onig_lock_mutex()
#else
#error THREAD_ATOMIC_START already defined
#endif

#ifndef THREAD_ATOMIC_END
#define THREAD_ATOMIC_END    onig_unlock_mutex()
#else
#error THREAD_ATOMIC_END already defined
#endif

#endif /* end of include guard: SETUP_H_5FEH3ZO0 */
