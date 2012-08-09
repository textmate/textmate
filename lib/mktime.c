/* Convert a `struct tm' to a time_t value.
   Copyright (C) 1993-1999, 2002-2007, 2009-2011 Free Software Foundation, Inc.
   This file is part of the GNU C Library.
   Contributed by Paul Eggert <eggert@twinsun.com>.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License along
   with this program; if not, write to the Free Software Foundation,
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA. */

/* Define this to have a standalone program to test this implementation of
   mktime.  */
/* #define DEBUG 1 */

#ifndef _LIBC
# include <config.h>
#endif

/* Some of the code in this file assumes that signed integer overflow
   silently wraps around.  This assumption can't easily be programmed
   around, nor can it be checked for portably at compile-time or
   easily eliminated at run-time.

   Define WRAPV to 1 if the assumption is valid.  Otherwise, define it
   to 0; this forces the use of slower code that, while not guaranteed
   by the C Standard, works on all production platforms that we know
   about.  */
#ifndef WRAPV
# if (__GNUC__ == 4 && 4 <= __GNUC_MINOR__) || 4 < __GNUC__
#  pragma GCC optimize ("wrapv")
#  define WRAPV 1
# else
#  define WRAPV 0
# endif
#endif

/* Assume that leap seconds are possible, unless told otherwise.
   If the host has a `zic' command with a `-L leapsecondfilename' option,
   then it supports leap seconds; otherwise it probably doesn't.  */
#ifndef LEAP_SECONDS_POSSIBLE
# define LEAP_SECONDS_POSSIBLE 1
#endif

#include <time.h>

#include <limits.h>

#include <string.h>             /* For the real memcpy prototype.  */

#if DEBUG
# include <stdio.h>
# include <stdlib.h>
/* Make it work even if the system's libc has its own mktime routine.  */
# undef mktime
# define mktime my_mktime
#endif /* DEBUG */

/* Verify a requirement at compile-time (unlike assert, which is runtime).  */
#define verify(name, assertion) struct name { char a[(assertion) ? 1 : -1]; }

/* A signed type that is at least one bit wider than int.  */
#if INT_MAX <= LONG_MAX / 2
typedef long int long_int;
#else
typedef long long int long_int;
#endif
verify (long_int_is_wide_enough, INT_MAX == INT_MAX * (long_int) 2 / 2);

/* Shift A right by B bits portably, by dividing A by 2**B and
   truncating towards minus infinity.  A and B should be free of side
   effects, and B should be in the range 0 <= B <= INT_BITS - 2, where
   INT_BITS is the number of useful bits in an int.  GNU code can
   assume that INT_BITS is at least 32.

   ISO C99 says that A >> B is implementation-defined if A < 0.  Some
   implementations (e.g., UNICOS 9.0 on a Cray Y-MP EL) don't shift
   right in the usual way when A < 0, so SHR falls back on division if
   ordinary A >> B doesn't seem to be the usual signed shift.  */
#define SHR(a, b)                                               \
  ((-1 >> 1 == -1                                               \
    && (long_int) -1 >> 1 == -1                                 \
    && ((time_t) -1 >> 1 == -1 || ! TYPE_SIGNED (time_t)))      \
   ? (a) >> (b)                                                 \
   : (a) / (1 << (b)) - ((a) % (1 << (b)) < 0))

/* The extra casts in the following macros work around compiler bugs,
   e.g., in Cray C 5.0.3.0.  */

/* True if the arithmetic type T is an integer type.  bool counts as
   an integer.  */
#define TYPE_IS_INTEGER(t) ((t) 1.5 == 1)

/* True if negative values of the signed integer type T use two's
   complement, or if T is an unsigned integer type.  */
#define TYPE_TWOS_COMPLEMENT(t) ((t) ~ (t) 0 == (t) -1)

/* True if the arithmetic type T is signed.  */
#define TYPE_SIGNED(t) (! ((t) 0 < (t) -1))

/* The maximum and minimum values for the integer type T.  These
   macros have undefined behavior if T is signed and has padding bits.
   If this is a problem for you, please let us know how to fix it for
   your host.  */
#define TYPE_MINIMUM(t) \
  ((t) (! TYPE_SIGNED (t) \
        ? (t) 0 \
        : ~ TYPE_MAXIMUM (t)))
#define TYPE_MAXIMUM(t) \
  ((t) (! TYPE_SIGNED (t) \
        ? (t) -1 \
        : ((((t) 1 << (sizeof (t) * CHAR_BIT - 2)) - 1) * 2 + 1)))

#ifndef TIME_T_MIN
# define TIME_T_MIN TYPE_MINIMUM (time_t)
#endif
#ifndef TIME_T_MAX
# define TIME_T_MAX TYPE_MAXIMUM (time_t)
#endif
#define TIME_T_MIDPOINT (SHR (TIME_T_MIN + TIME_T_MAX, 1) + 1)

verify (time_t_is_integer, TYPE_IS_INTEGER (time_t));
verify (twos_complement_arithmetic,
        (TYPE_TWOS_COMPLEMENT (int)
         && TYPE_TWOS_COMPLEMENT (long_int)
         && TYPE_TWOS_COMPLEMENT (time_t)));

#define EPOCH_YEAR 1970
#define TM_YEAR_BASE 1900
verify (base_year_is_a_multiple_of_100, TM_YEAR_BASE % 100 == 0);

/* Return 1 if YEAR + TM_YEAR_BASE is a leap year.  */
static inline int
leapyear (long_int year)
{
  /* Don't add YEAR to TM_YEAR_BASE, as that might overflow.
     Also, work even if YEAR is negative.  */
  return
    ((year & 3) == 0
     && (year % 100 != 0
         || ((year / 100) & 3) == (- (TM_YEAR_BASE / 100) & 3)));
}

/* How many days come before each month (0-12).  */
#ifndef _LIBC
static
#endif
const unsigned short int __mon_yday[2][13] =
  {
    /* Normal years.  */
    { 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365 },
    /* Leap years.  */
    { 0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366 }
  };


#ifndef _LIBC
/* Portable standalone applications should supply a <time.h> that
   declares a POSIX-compliant localtime_r, for the benefit of older
   implementations that lack localtime_r or have a nonstandard one.
   See the gnulib time_r module for one way to implement this.  */
# undef __localtime_r
# define __localtime_r localtime_r
# define __mktime_internal mktime_internal
# include "mktime-internal.h"
#endif

/* Return 1 if the values A and B differ according to the rules for
   tm_isdst: A and B differ if one is zero and the other positive.  */
static int
isdst_differ (int a, int b)
{
  return (!a != !b) & (0 <= a) & (0 <= b);
}

/* Return an integer value measuring (YEAR1-YDAY1 HOUR1:MIN1:SEC1) -
   (YEAR0-YDAY0 HOUR0:MIN0:SEC0) in seconds, assuming that the clocks
   were not adjusted between the time stamps.

   The YEAR values uses the same numbering as TP->tm_year.  Values
   need not be in the usual range.  However, YEAR1 must not be less
   than 2 * INT_MIN or greater than 2 * INT_MAX.

   The result may overflow.  It is the caller's responsibility to
   detect overflow.  */

static inline time_t
ydhms_diff (long_int year1, long_int yday1, int hour1, int min1, int sec1,
            int year0, int yday0, int hour0, int min0, int sec0)
{
  verify (C99_integer_division, -1 / 2 == 0);

  /* Compute intervening leap days correctly even if year is negative.
     Take care to avoid integer overflow here.  */
  int a4 = SHR (year1, 2) + SHR (TM_YEAR_BASE, 2) - ! (year1 & 3);
  int b4 = SHR (year0, 2) + SHR (TM_YEAR_BASE, 2) - ! (year0 & 3);
  int a100 = a4 / 25 - (a4 % 25 < 0);
  int b100 = b4 / 25 - (b4 % 25 < 0);
  int a400 = SHR (a100, 2);
  int b400 = SHR (b100, 2);
  int intervening_leap_days = (a4 - b4) - (a100 - b100) + (a400 - b400);

  /* Compute the desired time in time_t precision.  Overflow might
     occur here.  */
  time_t tyear1 = year1;
  time_t years = tyear1 - year0;
  time_t days = 365 * years + yday1 - yday0 + intervening_leap_days;
  time_t hours = 24 * days + hour1 - hour0;
  time_t minutes = 60 * hours + min1 - min0;
  time_t seconds = 60 * minutes + sec1 - sec0;
  return seconds;
}

/* Return the average of A and B, even if A + B would overflow.  */
static time_t
time_t_avg (time_t a, time_t b)
{
  return SHR (a, 1) + SHR (b, 1) + (a & b & 1);
}

/* Return 1 if A + B does not overflow.  If time_t is unsigned and if
   B's top bit is set, assume that the sum represents A - -B, and
   return 1 if the subtraction does not wrap around.  */
static int
time_t_add_ok (time_t a, time_t b)
{
  if (! TYPE_SIGNED (time_t))
    {
      time_t sum = a + b;
      return (sum < a) == (TIME_T_MIDPOINT <= b);
    }
  else if (WRAPV)
    {
      time_t sum = a + b;
      return (sum < a) == (b < 0);
    }
  else
    {
      time_t avg = time_t_avg (a, b);
      return TIME_T_MIN / 2 <= avg && avg <= TIME_T_MAX / 2;
    }
}

/* Return 1 if A + B does not overflow.  */
static int
time_t_int_add_ok (time_t a, int b)
{
  verify (int_no_wider_than_time_t, INT_MAX <= TIME_T_MAX);
  if (WRAPV)
    {
      time_t sum = a + b;
      return (sum < a) == (b < 0);
    }
  else
    {
      int a_odd = a & 1;
      time_t avg = SHR (a, 1) + (SHR (b, 1) + (a_odd & b));
      return TIME_T_MIN / 2 <= avg && avg <= TIME_T_MAX / 2;
    }
}

/* Return a time_t value corresponding to (YEAR-YDAY HOUR:MIN:SEC),
   assuming that *T corresponds to *TP and that no clock adjustments
   occurred between *TP and the desired time.
   If TP is null, return a value not equal to *T; this avoids false matches.
   If overflow occurs, yield the minimal or maximal value, except do not
   yield a value equal to *T.  */
static time_t
guess_time_tm (long_int year, long_int yday, int hour, int min, int sec,
               const time_t *t, const struct tm *tp)
{
  if (tp)
    {
      time_t d = ydhms_diff (year, yday, hour, min, sec,
                             tp->tm_year, tp->tm_yday,
                             tp->tm_hour, tp->tm_min, tp->tm_sec);
      if (time_t_add_ok (*t, d))
        return *t + d;
    }

  /* Overflow occurred one way or another.  Return the nearest result
     that is actually in range, except don't report a zero difference
     if the actual difference is nonzero, as that would cause a false
     match; and don't oscillate between two values, as that would
     confuse the spring-forward gap detector.  */
  return (*t < TIME_T_MIDPOINT
          ? (*t <= TIME_T_MIN + 1 ? *t + 1 : TIME_T_MIN)
          : (TIME_T_MAX - 1 <= *t ? *t - 1 : TIME_T_MAX));
}

/* Use CONVERT to convert *T to a broken down time in *TP.
   If *T is out of range for conversion, adjust it so that
   it is the nearest in-range value and then convert that.  */
static struct tm *
ranged_convert (struct tm *(*convert) (const time_t *, struct tm *),
                time_t *t, struct tm *tp)
{
  struct tm *r = convert (t, tp);

  if (!r && *t)
    {
      time_t bad = *t;
      time_t ok = 0;

      /* BAD is a known unconvertible time_t, and OK is a known good one.
         Use binary search to narrow the range between BAD and OK until
         they differ by 1.  */
      while (bad != ok + (bad < 0 ? -1 : 1))
        {
          time_t mid = *t = time_t_avg (ok, bad);
          r = convert (t, tp);
          if (r)
            ok = mid;
          else
            bad = mid;
        }

      if (!r && ok)
        {
          /* The last conversion attempt failed;
             revert to the most recent successful attempt.  */
          *t = ok;
          r = convert (t, tp);
        }
    }

  return r;
}


/* Convert *TP to a time_t value, inverting
   the monotonic and mostly-unit-linear conversion function CONVERT.
   Use *OFFSET to keep track of a guess at the offset of the result,
   compared to what the result would be for UTC without leap seconds.
   If *OFFSET's guess is correct, only one CONVERT call is needed.
   This function is external because it is used also by timegm.c.  */
time_t
__mktime_internal (struct tm *tp,
                   struct tm *(*convert) (const time_t *, struct tm *),
                   time_t *offset)
{
  time_t t, gt, t0, t1, t2;
  struct tm tm;

  /* The maximum number of probes (calls to CONVERT) should be enough
     to handle any combinations of time zone rule changes, solar time,
     leap seconds, and oscillations around a spring-forward gap.
     POSIX.1 prohibits leap seconds, but some hosts have them anyway.  */
  int remaining_probes = 6;

  /* Time requested.  Copy it in case CONVERT modifies *TP; this can
     occur if TP is localtime's returned value and CONVERT is localtime.  */
  int sec = tp->tm_sec;
  int min = tp->tm_min;
  int hour = tp->tm_hour;
  int mday = tp->tm_mday;
  int mon = tp->tm_mon;
  int year_requested = tp->tm_year;
  int isdst = tp->tm_isdst;

  /* 1 if the previous probe was DST.  */
  int dst2;

  /* Ensure that mon is in range, and set year accordingly.  */
  int mon_remainder = mon % 12;
  int negative_mon_remainder = mon_remainder < 0;
  int mon_years = mon / 12 - negative_mon_remainder;
  long_int lyear_requested = year_requested;
  long_int year = lyear_requested + mon_years;

  /* The other values need not be in range:
     the remaining code handles minor overflows correctly,
     assuming int and time_t arithmetic wraps around.
     Major overflows are caught at the end.  */

  /* Calculate day of year from year, month, and day of month.
     The result need not be in range.  */
  int mon_yday = ((__mon_yday[leapyear (year)]
                   [mon_remainder + 12 * negative_mon_remainder])
                  - 1);
  long_int lmday = mday;
  long_int yday = mon_yday + lmday;

  time_t guessed_offset = *offset;

  int sec_requested = sec;

  if (LEAP_SECONDS_POSSIBLE)
    {
      /* Handle out-of-range seconds specially,
         since ydhms_tm_diff assumes every minute has 60 seconds.  */
      if (sec < 0)
        sec = 0;
      if (59 < sec)
        sec = 59;
    }

  /* Invert CONVERT by probing.  First assume the same offset as last
     time.  */

  t0 = ydhms_diff (year, yday, hour, min, sec,
                   EPOCH_YEAR - TM_YEAR_BASE, 0, 0, 0, - guessed_offset);

  if (TIME_T_MAX / INT_MAX / 366 / 24 / 60 / 60 < 3)
    {
      /* time_t isn't large enough to rule out overflows, so check
         for major overflows.  A gross check suffices, since if t0
         has overflowed, it is off by a multiple of TIME_T_MAX -
         TIME_T_MIN + 1.  So ignore any component of the difference
         that is bounded by a small value.  */

      /* Approximate log base 2 of the number of time units per
         biennium.  A biennium is 2 years; use this unit instead of
         years to avoid integer overflow.  For example, 2 average
         Gregorian years are 2 * 365.2425 * 24 * 60 * 60 seconds,
         which is 63113904 seconds, and rint (log2 (63113904)) is
         26.  */
      int ALOG2_SECONDS_PER_BIENNIUM = 26;
      int ALOG2_MINUTES_PER_BIENNIUM = 20;
      int ALOG2_HOURS_PER_BIENNIUM = 14;
      int ALOG2_DAYS_PER_BIENNIUM = 10;
      int LOG2_YEARS_PER_BIENNIUM = 1;

      int approx_requested_biennia =
        (SHR (year_requested, LOG2_YEARS_PER_BIENNIUM)
         - SHR (EPOCH_YEAR - TM_YEAR_BASE, LOG2_YEARS_PER_BIENNIUM)
         + SHR (mday, ALOG2_DAYS_PER_BIENNIUM)
         + SHR (hour, ALOG2_HOURS_PER_BIENNIUM)
         + SHR (min, ALOG2_MINUTES_PER_BIENNIUM)
         + (LEAP_SECONDS_POSSIBLE
            ? 0
            : SHR (sec, ALOG2_SECONDS_PER_BIENNIUM)));

      int approx_biennia = SHR (t0, ALOG2_SECONDS_PER_BIENNIUM);
      int diff = approx_biennia - approx_requested_biennia;
      int abs_diff = diff < 0 ? -1 - diff : diff;

      /* IRIX 4.0.5 cc miscalculates TIME_T_MIN / 3: it erroneously
         gives a positive value of 715827882.  Setting a variable
         first then doing math on it seems to work.
         (ghazi@caip.rutgers.edu) */
      time_t time_t_max = TIME_T_MAX;
      time_t time_t_min = TIME_T_MIN;
      time_t overflow_threshold =
        (time_t_max / 3 - time_t_min / 3) >> ALOG2_SECONDS_PER_BIENNIUM;

      if (overflow_threshold < abs_diff)
        {
          /* Overflow occurred.  Try repairing it; this might work if
             the time zone offset is enough to undo the overflow.  */
          time_t repaired_t0 = -1 - t0;
          approx_biennia = SHR (repaired_t0, ALOG2_SECONDS_PER_BIENNIUM);
          diff = approx_biennia - approx_requested_biennia;
          abs_diff = diff < 0 ? -1 - diff : diff;
          if (overflow_threshold < abs_diff)
            return -1;
          guessed_offset += repaired_t0 - t0;
          t0 = repaired_t0;
        }
    }

  /* Repeatedly use the error to improve the guess.  */

  for (t = t1 = t2 = t0, dst2 = 0;
       (gt = guess_time_tm (year, yday, hour, min, sec, &t,
                            ranged_convert (convert, &t, &tm)),
        t != gt);
       t1 = t2, t2 = t, t = gt, dst2 = tm.tm_isdst != 0)
    if (t == t1 && t != t2
        && (tm.tm_isdst < 0
            || (isdst < 0
                ? dst2 <= (tm.tm_isdst != 0)
                : (isdst != 0) != (tm.tm_isdst != 0))))
      /* We can't possibly find a match, as we are oscillating
         between two values.  The requested time probably falls
         within a spring-forward gap of size GT - T.  Follow the common
         practice in this case, which is to return a time that is GT - T
         away from the requested time, preferring a time whose
         tm_isdst differs from the requested value.  (If no tm_isdst
         was requested and only one of the two values has a nonzero
         tm_isdst, prefer that value.)  In practice, this is more
         useful than returning -1.  */
      goto offset_found;
    else if (--remaining_probes == 0)
      return -1;

  /* We have a match.  Check whether tm.tm_isdst has the requested
     value, if any.  */
  if (isdst_differ (isdst, tm.tm_isdst))
    {
      /* tm.tm_isdst has the wrong value.  Look for a neighboring
         time with the right value, and use its UTC offset.

         Heuristic: probe the adjacent timestamps in both directions,
         looking for the desired isdst.  This should work for all real
         time zone histories in the tz database.  */

      /* Distance between probes when looking for a DST boundary.  In
         tzdata2003a, the shortest period of DST is 601200 seconds
         (e.g., America/Recife starting 2000-10-08 01:00), and the
         shortest period of non-DST surrounded by DST is 694800
         seconds (Africa/Tunis starting 1943-04-17 01:00).  Use the
         minimum of these two values, so we don't miss these short
         periods when probing.  */
      int stride = 601200;

      /* The longest period of DST in tzdata2003a is 536454000 seconds
         (e.g., America/Jujuy starting 1946-10-01 01:00).  The longest
         period of non-DST is much longer, but it makes no real sense
         to search for more than a year of non-DST, so use the DST
         max.  */
      int duration_max = 536454000;

      /* Search in both directions, so the maximum distance is half
         the duration; add the stride to avoid off-by-1 problems.  */
      int delta_bound = duration_max / 2 + stride;

      int delta, direction;

      for (delta = stride; delta < delta_bound; delta += stride)
        for (direction = -1; direction <= 1; direction += 2)
          if (time_t_int_add_ok (t, delta * direction))
            {
              time_t ot = t + delta * direction;
              struct tm otm;
              ranged_convert (convert, &ot, &otm);
              if (! isdst_differ (isdst, otm.tm_isdst))
                {
                  /* We found the desired tm_isdst.
                     Extrapolate back to the desired time.  */
                  t = guess_time_tm (year, yday, hour, min, sec, &ot, &otm);
                  ranged_convert (convert, &t, &tm);
                  goto offset_found;
                }
            }
    }

 offset_found:
  *offset = guessed_offset + t - t0;

  if (LEAP_SECONDS_POSSIBLE && sec_requested != tm.tm_sec)
    {
      /* Adjust time to reflect the tm_sec requested, not the normalized value.
         Also, repair any damage from a false match due to a leap second.  */
      int sec_adjustment = (sec == 0 && tm.tm_sec == 60) - sec;
      if (! time_t_int_add_ok (t, sec_requested))
        return -1;
      t1 = t + sec_requested;
      if (! time_t_int_add_ok (t1, sec_adjustment))
        return -1;
      t2 = t1 + sec_adjustment;
      if (! convert (&t2, &tm))
        return -1;
      t = t2;
    }

  *tp = tm;
  return t;
}


/* FIXME: This should use a signed type wide enough to hold any UTC
   offset in seconds.  'int' should be good enough for GNU code.  We
   can't fix this unilaterally though, as other modules invoke
   __mktime_internal.  */
static time_t localtime_offset;

/* Convert *TP to a time_t value.  */
time_t
mktime (struct tm *tp)
{
#ifdef _LIBC
  /* POSIX.1 8.1.1 requires that whenever mktime() is called, the
     time zone names contained in the external variable `tzname' shall
     be set as if the tzset() function had been called.  */
  __tzset ();
#endif

  return __mktime_internal (tp, __localtime_r, &localtime_offset);
}

#ifdef weak_alias
weak_alias (mktime, timelocal)
#endif

#ifdef _LIBC
libc_hidden_def (mktime)
libc_hidden_weak (timelocal)
#endif

#if DEBUG

static int
not_equal_tm (const struct tm *a, const struct tm *b)
{
  return ((a->tm_sec ^ b->tm_sec)
          | (a->tm_min ^ b->tm_min)
          | (a->tm_hour ^ b->tm_hour)
          | (a->tm_mday ^ b->tm_mday)
          | (a->tm_mon ^ b->tm_mon)
          | (a->tm_year ^ b->tm_year)
          | (a->tm_yday ^ b->tm_yday)
          | isdst_differ (a->tm_isdst, b->tm_isdst));
}

static void
print_tm (const struct tm *tp)
{
  if (tp)
    printf ("%04d-%02d-%02d %02d:%02d:%02d yday %03d wday %d isdst %d",
            tp->tm_year + TM_YEAR_BASE, tp->tm_mon + 1, tp->tm_mday,
            tp->tm_hour, tp->tm_min, tp->tm_sec,
            tp->tm_yday, tp->tm_wday, tp->tm_isdst);
  else
    printf ("0");
}

static int
check_result (time_t tk, struct tm tmk, time_t tl, const struct tm *lt)
{
  if (tk != tl || !lt || not_equal_tm (&tmk, lt))
    {
      printf ("mktime (");
      print_tm (lt);
      printf (")\nyields (");
      print_tm (&tmk);
      printf (") == %ld, should be %ld\n", (long int) tk, (long int) tl);
      return 1;
    }

  return 0;
}

int
main (int argc, char **argv)
{
  int status = 0;
  struct tm tm, tmk, tml;
  struct tm *lt;
  time_t tk, tl, tl1;
  char trailer;

  if ((argc == 3 || argc == 4)
      && (sscanf (argv[1], "%d-%d-%d%c",
                  &tm.tm_year, &tm.tm_mon, &tm.tm_mday, &trailer)
          == 3)
      && (sscanf (argv[2], "%d:%d:%d%c",
                  &tm.tm_hour, &tm.tm_min, &tm.tm_sec, &trailer)
          == 3))
    {
      tm.tm_year -= TM_YEAR_BASE;
      tm.tm_mon--;
      tm.tm_isdst = argc == 3 ? -1 : atoi (argv[3]);
      tmk = tm;
      tl = mktime (&tmk);
      lt = localtime (&tl);
      if (lt)
        {
          tml = *lt;
          lt = &tml;
        }
      printf ("mktime returns %ld == ", (long int) tl);
      print_tm (&tmk);
      printf ("\n");
      status = check_result (tl, tmk, tl, lt);
    }
  else if (argc == 4 || (argc == 5 && strcmp (argv[4], "-") == 0))
    {
      time_t from = atol (argv[1]);
      time_t by = atol (argv[2]);
      time_t to = atol (argv[3]);

      if (argc == 4)
        for (tl = from; by < 0 ? to <= tl : tl <= to; tl = tl1)
          {
            lt = localtime (&tl);
            if (lt)
              {
                tmk = tml = *lt;
                tk = mktime (&tmk);
                status |= check_result (tk, tmk, tl, &tml);
              }
            else
              {
                printf ("localtime (%ld) yields 0\n", (long int) tl);
                status = 1;
              }
            tl1 = tl + by;
            if ((tl1 < tl) != (by < 0))
              break;
          }
      else
        for (tl = from; by < 0 ? to <= tl : tl <= to; tl = tl1)
          {
            /* Null benchmark.  */
            lt = localtime (&tl);
            if (lt)
              {
                tmk = tml = *lt;
                tk = tl;
                status |= check_result (tk, tmk, tl, &tml);
              }
            else
              {
                printf ("localtime (%ld) yields 0\n", (long int) tl);
                status = 1;
              }
            tl1 = tl + by;
            if ((tl1 < tl) != (by < 0))
              break;
          }
    }
  else
    printf ("Usage:\
\t%s YYYY-MM-DD HH:MM:SS [ISDST] # Test given time.\n\
\t%s FROM BY TO # Test values FROM, FROM+BY, ..., TO.\n\
\t%s FROM BY TO - # Do not test those values (for benchmark).\n",
            argv[0], argv[0], argv[0]);

  return status;
}

#endif /* DEBUG */

/*
Local Variables:
compile-command: "gcc -DDEBUG -I. -Wall -W -O2 -g mktime.c -o mktime"
End:
*/
