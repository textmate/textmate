/* uint64_t-like operations that work even on hosts lacking uint64_t

   Copyright (C) 2006, 2009-2011 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* Written by Paul Eggert.  */

#include <stdint.h>

/* Return X rotated left by N bits, where 0 < N < 64.  */
#define u64rol(x, n) u64or (u64shl (x, n), u64shr (x, 64 - n))

#ifdef UINT64_MAX

/* Native implementations are trivial.  See below for comments on what
   these operations do.  */
typedef uint64_t u64;
# define u64hilo(hi, lo) ((u64) (((u64) (hi) << 32) + (lo)))
# define u64init(hi, lo) u64hilo (hi, lo)
# define u64lo(x) ((u64) (x))
# define u64lt(x, y) ((x) < (y))
# define u64and(x, y) ((x) & (y))
# define u64or(x, y) ((x) | (y))
# define u64xor(x, y) ((x) ^ (y))
# define u64plus(x, y) ((x) + (y))
# define u64shl(x, n) ((x) << (n))
# define u64shr(x, n) ((x) >> (n))

#else

/* u64 is a 64-bit unsigned integer value.
   u64init (HI, LO), is like u64hilo (HI, LO), but for use in
   initializer contexts.  */
# ifdef WORDS_BIGENDIAN
typedef struct { uint32_t hi, lo; } u64;
#  define u64init(hi, lo) { hi, lo }
# else
typedef struct { uint32_t lo, hi; } u64;
#  define u64init(hi, lo) { lo, hi }
# endif

/* Given the high and low-order 32-bit quantities HI and LO, return a u64
   value representing (HI << 32) + LO.  */
static inline u64
u64hilo (uint32_t hi, uint32_t lo)
{
  u64 r;
  r.hi = hi;
  r.lo = lo;
  return r;
}

/* Return a u64 value representing LO.  */
static inline u64
u64lo (uint32_t lo)
{
  u64 r;
  r.hi = 0;
  r.lo = lo;
  return r;
}

/* Return X < Y.  */
static inline int
u64lt (u64 x, u64 y)
{
  return x.hi < y.hi || (x.hi == y.hi && x.lo < y.lo);
}

/* Return X & Y.  */
static inline u64
u64and (u64 x, u64 y)
{
  u64 r;
  r.hi = x.hi & y.hi;
  r.lo = x.lo & y.lo;
  return r;
}

/* Return X | Y.  */
static inline u64
u64or (u64 x, u64 y)
{
  u64 r;
  r.hi = x.hi | y.hi;
  r.lo = x.lo | y.lo;
  return r;
}

/* Return X ^ Y.  */
static inline u64
u64xor (u64 x, u64 y)
{
  u64 r;
  r.hi = x.hi ^ y.hi;
  r.lo = x.lo ^ y.lo;
  return r;
}

/* Return X + Y.  */
static inline u64
u64plus (u64 x, u64 y)
{
  u64 r;
  r.lo = x.lo + y.lo;
  r.hi = x.hi + y.hi + (r.lo < x.lo);
  return r;
}

/* Return X << N.  */
static inline u64
u64shl (u64 x, int n)
{
  u64 r;
  if (n < 32)
    {
      r.hi = (x.hi << n) | (x.lo >> (32 - n));
      r.lo = x.lo << n;
    }
  else
    {
      r.hi = x.lo << (n - 32);
      r.lo = 0;
    }
  return r;
}

/* Return X >> N.  */
static inline u64
u64shr (u64 x, int n)
{
  u64 r;
  if (n < 32)
    {
      r.hi = x.hi >> n;
      r.lo = (x.hi << (32 - n)) | (x.lo >> n);
    }
  else
    {
      r.hi = 0;
      r.lo = x.hi >> (n - 32);
    }
  return r;
}

#endif
