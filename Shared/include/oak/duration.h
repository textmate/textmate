#ifndef OAK_DURATION_H_NQISZ9T7
#define OAK_DURATION_H_NQISZ9T7

namespace oak
{
	struct duration_t
	{
		duration_t ()
		{
			reset();
		}

		void reset ()
		{
			struct timeval now;
			gettimeofday(&now, NULL);
			start_time = (double)now.tv_sec + (double)now.tv_usec / 1000000.0;
		}

		double duration () const
		{
			struct timeval now;
			gettimeofday(&now, NULL);
			double elapsed = (double)now.tv_sec + (double)now.tv_usec / 1000000.0;
			return elapsed - start_time;
		}

	private:
		double start_time;
	};

} /* oak */

#endif /* end of include guard: OAK_DURATION_H_NQISZ9T7 */
