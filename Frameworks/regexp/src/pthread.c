#include <stdio.h>
#include <pthread.h>

static pthread_mutex_t OnigMutex;

void lock_mutex ()	{ pthread_mutex_lock(&OnigMutex); }
void unlock_mutex ()	{ pthread_mutex_unlock(&OnigMutex); }

__attribute__((constructor)) static void initializer()
{
	pthread_mutexattr_t attr;
	pthread_mutexattr_init(&attr);
	pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE);
	pthread_mutex_init(&OnigMutex, &attr);
	pthread_mutexattr_destroy(&attr);
}

__attribute__((destructor)) static void finalizer()
{
	pthread_mutex_destroy(&OnigMutex);
}
