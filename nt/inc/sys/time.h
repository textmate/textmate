#ifndef SYS_TIME_H_INCLUDED
#define SYS_TIME_H_INCLUDED

/*
 * sys/time.h doesn't exist on NT
 */

struct timeval
  {
    long tv_sec;	/* seconds */
    long tv_usec;	/* microseconds */
  };
struct timezone
  {
    int	tz_minuteswest;	/* minutes west of Greenwich */
    int	tz_dsttime;	/* type of dst correction */
  };

void gettimeofday (struct timeval *, struct timezone *);

#endif /* SYS_TIME_H_INCLUDED */

/* end of sys/time.h */

