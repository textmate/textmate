#ifndef LOCK_H_SDS5VGYK
#define LOCK_H_SDS5VGYK

namespace oak
{
	struct lock_t
	{
		lock_t (pthread_mutex_t& mutex) : mutex(&mutex), is_locked(false) { lock(); }
		lock_t (lock_t const& rhs) : mutex(rhs.mutex)                     { lock(); }
		lock_t& operator= (lock_t const& rhs)                             { unlock(); mutex = rhs.mutex; is_locked = false; lock(); return *this; }
		~lock_t ()                                                        { unlock(); }

		void lock ()         { if(!is_locked)  pthread_mutex_lock(mutex);    is_locked = true; }
		void unlock ()       { if(is_locked)   pthread_mutex_unlock(mutex);  is_locked = false; }

	private:
		pthread_mutex_t* mutex;
		bool is_locked;
	};

	struct mutex_t
	{
		mutex_t ()
		{
			pthread_mutexattr_t attr;
			pthread_mutexattr_init(&attr);
			pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE);
			pthread_mutex_init(&mutex, &attr);
			pthread_mutexattr_destroy(&attr);
		}

		~mutex_t ()                            { pthread_mutex_destroy(&mutex); }
		lock_t const operator() () const       { return lock_t(mutex); }

	private:
		mutable pthread_mutex_t mutex;
	};

} /* oak */

#endif /* end of include guard: LOCK_H_SDS5VGYK */
