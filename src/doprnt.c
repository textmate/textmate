/* Output like sprintf to a buffer of specified size.
   Also takes args differently: pass one pointer to the end
   of the format string in addition to the format string itself.
   Copyright (C) 1985, 2001-2012  Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.  */

/* If you think about replacing this with some similar standard C function of
   the printf family (such as vsnprintf), please note that this function
   supports the following Emacs-specific features:

   . For %c conversions, it produces a string with the multibyte representation
     of the (`int') argument, suitable for display in an Emacs buffer.

   . For %s and %c, when field width is specified (e.g., %25s), it accounts for
     the display width of each character, according to char-width-table.  That
     is, it does not assume that each character takes one column on display.

   . If the size of the buffer is not enough to produce the formatted string in
     its entirety, it makes sure that truncation does not chop the last
     character in the middle of its multibyte sequence, producing an invalid
     sequence.

   . It accepts a pointer to the end of the format string, so the format string
     could include embedded null characters.

   . It signals an error if the length of the formatted string is about to
     overflow MOST_POSITIVE_FIXNUM, to avoid producing strings longer than what
     Emacs can handle.

   OTOH, this function supports only a small subset of the standard C formatted
   output facilities.  E.g., %u and %ll are not supported, and precision is
   ignored %s and %c conversions.  (See below for the detailed documentation of
   what is supported.)  However, this is okay, as this function is supposed to
   be called from `error' and similar functions, and thus does not need to
   support features beyond those in `Fformat', which is used by `error' on the
   Lisp level.  */

/* This function supports the following %-sequences in the `format'
   argument:

   %s means print a string argument.
   %S is silently treated as %s, for loose compatibility with `Fformat'.
   %d means print a `signed int' argument in decimal.
   %o means print an `unsigned int' argument in octal.
   %x means print an `unsigned int' argument in hex.
   %e means print a `double' argument in exponential notation.
   %f means print a `double' argument in decimal-point notation.
   %g means print a `double' argument in exponential notation
      or in decimal-point notation, whichever uses fewer characters.
   %c means print a `signed int' argument as a single character.
   %% means produce a literal % character.

   A %-sequence may contain optional flag, width, and precision specifiers, and
   a length modifier, as follows:

     %<flags><width><precision><length>character

   where flags is [+ -0], width is [0-9]+, precision is .[0-9]+, and length
   is empty or l or the value of the pD or pI or pMd (sans "d") macros.
   Also, %% in a format stands for a single % in the output.  A % that
   does not introduce a valid %-sequence causes undefined behavior.

   The + flag character inserts a + before any positive number, while a space
   inserts a space before any positive number; these flags only affect %d, %o,
   %x, %e, %f, and %g sequences.  The - and 0 flags affect the width specifier,
   as described below.  For signed numerical arguments only, the ` ' (space)
   flag causes the result to be prefixed with a space character if it does not
   start with a sign (+ or -).

   The l (lower-case letter ell) length modifier is a `long' data type
   modifier: it is supported for %d, %o, and %x conversions of integral
   arguments, must immediately precede the conversion specifier, and means that
   the respective argument is to be treated as `long int' or `unsigned long
   int'.  Similarly, the value of the pD macro means to use ptrdiff_t,
   the value of the pI macro means to use EMACS_INT or EMACS_UINT, the
   value of the pMd etc. macros means to use intmax_t or uintmax_t,
   and the empty length modifier means `int' or `unsigned int'.

   The width specifier supplies a lower limit for the length of the printed
   representation.  The padding, if any, normally goes on the left, but it goes
   on the right if the - flag is present.  The padding character is normally a
   space, but (for numerical arguments only) it is 0 if the 0 flag is present.
   The - flag takes precedence over the 0 flag.

   For %e, %f, and %g sequences, the number after the "." in the precision
   specifier says how many decimal places to show; if zero, the decimal point
   itself is omitted.  For %s and %S, the precision specifier is ignored.  */

#include <config.h>
#include <stdio.h>
#include <ctype.h>
#include <setjmp.h>
#include <float.h>
#include <unistd.h>
#include <limits.h>

#include "lisp.h"

/* Since we use the macro CHAR_HEAD_P, we have to include this, but
   don't have to include others because CHAR_HEAD_P does not contains
   another macro.  */
#include "character.h"

#ifndef DBL_MAX_10_EXP
#define DBL_MAX_10_EXP 308 /* IEEE double */
#endif

/* Generate output from a format-spec FORMAT,
   terminated at position FORMAT_END.
   (*FORMAT_END is not part of the format, but must exist and be readable.)
   Output goes in BUFFER, which has room for BUFSIZE chars.
   BUFSIZE must be positive.  If the output does not fit, truncate it
   to fit and return BUFSIZE - 1; if this truncates a multibyte
   sequence, store '\0' into the sequence's first byte.
   Returns the number of bytes stored into BUFFER, excluding
   the terminating null byte.  Output is always null-terminated.
   String arguments are passed as C strings.
   Integers are passed as C integers.  */

ptrdiff_t
doprnt (char *buffer, ptrdiff_t bufsize, const char *format,
	const char *format_end, va_list ap)
{
  const char *fmt = format;	/* Pointer into format string */
  register char *bufptr = buffer; /* Pointer into output buffer.. */

  /* Use this for sprintf unless we need something really big.  */
  char tembuf[DBL_MAX_10_EXP + 100];

  /* Size of sprintf_buffer.  */
  ptrdiff_t size_allocated = sizeof (tembuf);

  /* Buffer to use for sprintf.  Either tembuf or same as BIG_BUFFER.  */
  char *sprintf_buffer = tembuf;

  /* Buffer we have got with malloc.  */
  char *big_buffer = NULL;

  register size_t tem;
  char *string;
  char fixed_buffer[20];	/* Default buffer for small formatting. */
  char *fmtcpy;
  int minlen;
  char charbuf[MAX_MULTIBYTE_LENGTH + 1];	/* Used for %c.  */
  USE_SAFE_ALLOCA;

  if (format_end == 0)
    format_end = format + strlen (format);

  if (format_end - format < sizeof (fixed_buffer) - 1)
    fmtcpy = fixed_buffer;
  else
    SAFE_ALLOCA (fmtcpy, char *, format_end - format + 1);

  bufsize--;

  /* Loop until end of format string or buffer full. */
  while (fmt < format_end && bufsize > 0)
    {
      if (*fmt == '%')	/* Check for a '%' character */
	{
	  ptrdiff_t size_bound = 0;
	  EMACS_INT width;  /* Columns occupied by STRING on display.  */
	  enum {
	    pDlen = sizeof pD - 1,
	    pIlen = sizeof pI - 1,
	    pMlen = sizeof pMd - 2
	  };
	  enum {
	    no_modifier, long_modifier, pD_modifier, pI_modifier, pM_modifier
	  } length_modifier = no_modifier;
	  static char const modifier_len[] = { 0, 1, pDlen, pIlen, pMlen };
	  int maxmlen = max (max (1, pDlen), max (pIlen, pMlen));
	  int mlen;

	  fmt++;
	  /* Copy this one %-spec into fmtcpy.  */
	  string = fmtcpy;
	  *string++ = '%';
	  while (fmt < format_end)
	    {
	      *string++ = *fmt;
	      if ('0' <= *fmt && *fmt <= '9')
		{
		  /* Get an idea of how much space we might need.
		     This might be a field width or a precision; e.g.
		     %1.1000f and %1000.1f both might need 1000+ bytes.
		     Parse the width or precision, checking for overflow.  */
		  ptrdiff_t n = *fmt - '0';
		  while (fmt + 1 < format_end
			 && '0' <= fmt[1] && fmt[1] <= '9')
		    {
		      /* Avoid ptrdiff_t, size_t, and int overflow, as
			 many sprintfs mishandle widths greater than INT_MAX.
			 This test is simple but slightly conservative: e.g.,
			 (INT_MAX - INT_MAX % 10) is reported as an overflow
			 even when it's not.  */
		      if (n >= min (INT_MAX, min (PTRDIFF_MAX, SIZE_MAX)) / 10)
			error ("Format width or precision too large");
		      n = n * 10 + fmt[1] - '0';
		      *string++ = *++fmt;
		    }

		  if (size_bound < n)
		    size_bound = n;
		}
	      else if (! (*fmt == '-' || *fmt == ' ' || *fmt == '.'
			  || *fmt == '+'))
		break;
	      fmt++;
	    }

	  /* Check for the length modifiers in textual length order, so
	     that longer modifiers override shorter ones.  */
	  for (mlen = 1; mlen <= maxmlen; mlen++)
	    {
	      if (format_end - fmt < mlen)
		break;
	      if (mlen == 1 && *fmt == 'l')
		length_modifier = long_modifier;
	      if (mlen == pDlen && memcmp (fmt, pD, pDlen) == 0)
		length_modifier = pD_modifier;
	      if (mlen == pIlen && memcmp (fmt, pI, pIlen) == 0)
		length_modifier = pI_modifier;
	      if (mlen == pMlen && memcmp (fmt, pMd, pMlen) == 0)
		length_modifier = pM_modifier;
	    }

	  mlen = modifier_len[length_modifier];
	  memcpy (string, fmt + 1, mlen);
	  string += mlen;
	  fmt += mlen;
	  *string = 0;

	  /* Make the size bound large enough to handle floating point formats
	     with large numbers.  */
	  if (size_bound > min (PTRDIFF_MAX, SIZE_MAX) - DBL_MAX_10_EXP - 50)
	    error ("Format width or precision too large");
	  size_bound += DBL_MAX_10_EXP + 50;

	  /* Make sure we have that much.  */
	  if (size_bound > size_allocated)
	    {
	      if (big_buffer)
		xfree (big_buffer);
	      big_buffer = (char *) xmalloc (size_bound);
	      sprintf_buffer = big_buffer;
	      size_allocated = size_bound;
	    }
	  minlen = 0;
	  switch (*fmt++)
	    {
	    default:
	      error ("Invalid format operation %s", fmtcpy);

/*	    case 'b': */
	    case 'l':
	    case 'd':
	      switch (length_modifier)
		{
		case no_modifier:
		  {
		    int v = va_arg (ap, int);
		    sprintf (sprintf_buffer, fmtcpy, v);
		  }
		  break;
		case long_modifier:
		  {
		    long v = va_arg (ap, long);
		    sprintf (sprintf_buffer, fmtcpy, v);
		  }
		  break;
		case pD_modifier:
		signed_pD_modifier:
		  {
		    ptrdiff_t v = va_arg (ap, ptrdiff_t);
		    sprintf (sprintf_buffer, fmtcpy, v);
		  }
		  break;
		case pI_modifier:
		  {
		    EMACS_INT v = va_arg (ap, EMACS_INT);
		    sprintf (sprintf_buffer, fmtcpy, v);
		  }
		  break;
		case pM_modifier:
		  {
		    intmax_t v = va_arg (ap, intmax_t);
		    sprintf (sprintf_buffer, fmtcpy, v);
		  }
		  break;
		}
	      /* Now copy into final output, truncating as necessary.  */
	      string = sprintf_buffer;
	      goto doit;

	    case 'o':
	    case 'x':
	      switch (length_modifier)
		{
		case no_modifier:
		  {
		    unsigned v = va_arg (ap, unsigned);
		    sprintf (sprintf_buffer, fmtcpy, v);
		  }
		  break;
		case long_modifier:
		  {
		    unsigned long v = va_arg (ap, unsigned long);
		    sprintf (sprintf_buffer, fmtcpy, v);
		  }
		  break;
		case pD_modifier:
		  goto signed_pD_modifier;
		case pI_modifier:
		  {
		    EMACS_UINT v = va_arg (ap, EMACS_UINT);
		    sprintf (sprintf_buffer, fmtcpy, v);
		  }
		  break;
		case pM_modifier:
		  {
		    uintmax_t v = va_arg (ap, uintmax_t);
		    sprintf (sprintf_buffer, fmtcpy, v);
		  }
		  break;
		}
	      /* Now copy into final output, truncating as necessary.  */
	      string = sprintf_buffer;
	      goto doit;

	    case 'f':
	    case 'e':
	    case 'g':
	      {
		double d = va_arg (ap, double);
		sprintf (sprintf_buffer, fmtcpy, d);
		/* Now copy into final output, truncating as necessary.  */
		string = sprintf_buffer;
		goto doit;
	      }

	    case 'S':
	      string[-1] = 's';
	    case 's':
	      if (fmtcpy[1] != 's')
		minlen = atoi (&fmtcpy[1]);
	      string = va_arg (ap, char *);
	      tem = strlen (string);
	      if (STRING_BYTES_BOUND < tem)
		error ("String for %%s or %%S format is too long");
	      width = strwidth (string, tem);
	      goto doit1;

	      /* Copy string into final output, truncating if no room.  */
	    doit:
	      /* Coming here means STRING contains ASCII only.  */
	      tem = strlen (string);
	      if (STRING_BYTES_BOUND < tem)
		error ("Format width or precision too large");
	      width = tem;
	    doit1:
	      /* We have already calculated:
		 TEM -- length of STRING,
		 WIDTH -- columns occupied by STRING when displayed, and
		 MINLEN -- minimum columns of the output.  */
	      if (minlen > 0)
		{
		  while (minlen > width && bufsize > 0)
		    {
		      *bufptr++ = ' ';
		      bufsize--;
		      minlen--;
		    }
		  minlen = 0;
		}
	      if (tem > bufsize)
		{
		  /* Truncate the string at character boundary.  */
		  tem = bufsize;
		  while (!CHAR_HEAD_P (string[tem - 1])) tem--;
		  /* If the multibyte sequence of this character is
		     too long for the space we have left in the
		     buffer, truncate before it.  */
		  if (tem > 0
		      && BYTES_BY_CHAR_HEAD (string[tem - 1]) > bufsize)
		    tem--;
		  if (tem > 0)
		    memcpy (bufptr, string, tem);
		  bufptr[tem] = 0;
		  /* Trigger exit from the loop, but make sure we
		     return to the caller a value which will indicate
		     that the buffer was too small.  */
		  bufptr += bufsize;
		  bufsize = 0;
		  continue;
		}
	      else
		memcpy (bufptr, string, tem);
	      bufptr += tem;
	      bufsize -= tem;
	      if (minlen < 0)
		{
		  while (minlen < - width && bufsize > 0)
		    {
		      *bufptr++ = ' ';
		      bufsize--;
		      minlen++;
		    }
		  minlen = 0;
		}
	      continue;

	    case 'c':
	      {
		int chr = va_arg (ap, int);
		tem = CHAR_STRING (chr, (unsigned char *) charbuf);
		string = charbuf;
		string[tem] = 0;
		width = strwidth (string, tem);
		if (fmtcpy[1] != 'c')
		  minlen = atoi (&fmtcpy[1]);
		goto doit1;
	      }

	    case '%':
	      fmt--;    /* Drop thru and this % will be treated as normal */
	    }
	}

      {
	/* Just some character; Copy it if the whole multi-byte form
	   fit in the buffer.  */
	char *save_bufptr = bufptr;

	do { *bufptr++ = *fmt++; }
	while (fmt < format_end && --bufsize > 0 && !CHAR_HEAD_P (*fmt));
	if (!CHAR_HEAD_P (*fmt))
	  {
	    /* Truncate, but return value that will signal to caller
	       that the buffer was too small.  */
	    *save_bufptr = 0;
	    break;
	  }
      }
    };

  /* If we had to malloc something, free it.  */
  xfree (big_buffer);

  *bufptr = 0;		/* Make sure our string ends with a '\0' */

  SAFE_FREE ();
  return bufptr - buffer;
}

/* Format to an unbounded buffer BUF.  This is like sprintf, except it
   is not limited to returning an 'int' so it doesn't have a silly 2
   GiB limit on typical 64-bit hosts.  However, it is limited to the
   Emacs-style formats that doprnt supports.

   Return the number of bytes put into BUF, excluding the terminating
   '\0'.  */
ptrdiff_t
esprintf (char *buf, char const *format, ...)
{
  ptrdiff_t nbytes;
  va_list ap;
  va_start (ap, format);
  nbytes = doprnt (buf, TYPE_MAXIMUM (ptrdiff_t), format, 0, ap);
  va_end (ap);
  return nbytes;
}

#if defined HAVE_X_WINDOWS && defined USE_X_TOOLKIT

/* Format to buffer *BUF of positive size *BUFSIZE, reallocating *BUF
   and updating *BUFSIZE if the buffer is too small, and otherwise
   behaving line esprintf.  When reallocating, free *BUF unless it is
   equal to NONHEAPBUF, and if BUFSIZE_MAX is nonnegative then signal
   memory exhaustion instead of growing the buffer size past
   BUFSIZE_MAX.  */
ptrdiff_t
exprintf (char **buf, ptrdiff_t *bufsize,
	  char const *nonheapbuf, ptrdiff_t bufsize_max,
	  char const *format, ...)
{
  ptrdiff_t nbytes;
  va_list ap;
  va_start (ap, format);
  nbytes = evxprintf (buf, bufsize, nonheapbuf, bufsize_max, format, ap);
  va_end (ap);
  return nbytes;
}

#endif

/* Act like exprintf, except take a va_list.  */
ptrdiff_t
evxprintf (char **buf, ptrdiff_t *bufsize,
	   char const *nonheapbuf, ptrdiff_t bufsize_max,
	   char const *format, va_list ap)
{
  for (;;)
    {
      ptrdiff_t nbytes;
      va_list ap_copy;
      va_copy (ap_copy, ap);
      nbytes = doprnt (*buf, *bufsize, format, 0, ap_copy);
      va_end (ap_copy);
      if (nbytes < *bufsize - 1)
	return nbytes;
      if (*buf != nonheapbuf)
	xfree (*buf);
      *buf = xpalloc (NULL, bufsize, 1, bufsize_max, 1);
    }
}
