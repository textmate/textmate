/* Work-alike for termcap, plus extra features.
   Copyright (C) 1985, 1986, 1993, 1994, 1995, 2000, 2001, 2002, 2003,
                 2004, 2005, 2006, 2007, 2008, 2011 Free Software Foundation, Inc.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

/* Emacs config.h may rename various library functions such as malloc.  */
#include <config.h>
#include <setjmp.h>
#include <sys/file.h>
#include <fcntl.h>
#include <unistd.h>

#include "lisp.h"
#include "tparam.h"
#ifdef MSDOS
#include "msdos.h"
#endif

#ifndef NULL
#define NULL (char *) 0
#endif

/* BUFSIZE is the initial size allocated for the buffer
   for reading the termcap file.
   It is not a limit.
   Make it large normally for speed.
   Make it variable when debugging, so can exercise
   increasing the space dynamically.  */

#ifndef BUFSIZE
#ifdef DEBUG
#define BUFSIZE bufsize

int bufsize = 128;
#else
#define BUFSIZE 2048
#endif
#endif

#ifndef TERMCAP_FILE
#define TERMCAP_FILE "/etc/termcap"
#endif


/* Looking up capabilities in the entry already found.  */

/* The pointer to the data made by tgetent is left here
   for tgetnum, tgetflag and tgetstr to find.  */
static char *term_entry;

static char *tgetst1 (char *ptr, char **area);

/* Search entry BP for capability CAP.
   Return a pointer to the capability (in BP) if found,
   0 if not found.  */

static char *
find_capability (register char *bp, register const char *cap)
{
  for (; *bp; bp++)
    if (bp[0] == ':'
	&& bp[1] == cap[0]
	&& bp[2] == cap[1])
      return &bp[4];
  return NULL;
}

int
tgetnum (const char *cap)
{
  register char *ptr = find_capability (term_entry, cap);
  if (!ptr || ptr[-1] != '#')
    return -1;
  return atoi (ptr);
}

int
tgetflag (const char *cap)
{
  register char *ptr = find_capability (term_entry, cap);
  return ptr && ptr[-1] == ':';
}

/* Look up a string-valued capability CAP.
   If AREA is non-null, it points to a pointer to a block in which
   to store the string.  That pointer is advanced over the space used.
   If AREA is null, space is allocated with `malloc'.  */

char *
tgetstr (const char *cap, char **area)
{
  register char *ptr = find_capability (term_entry, cap);
  if (!ptr || (ptr[-1] != '=' && ptr[-1] != '~'))
    return NULL;
  return tgetst1 (ptr, area);
}

#ifdef IS_EBCDIC_HOST
/* Table, indexed by a character in range 0200 to 0300 with 0200 subtracted,
   gives meaning of character following \, or a space if no special meaning.
   Sixteen characters per line within the string.  */

static const char esctab[]
  = " \057\026  \047\014         \
     \025   \015      \
   \005 \013          \
                ";
#else
/* Table, indexed by a character in range 0100 to 0140 with 0100 subtracted,
   gives meaning of character following \, or a space if no special meaning.
   Eight characters per line within the string.  */

static const char esctab[]
  = " \007\010  \033\014 \
      \012 \
  \015 \011 \013 \
        ";
#endif

/* PTR points to a string value inside a termcap entry.
   Copy that value, processing \ and ^ abbreviations,
   into the block that *AREA points to,
   or to newly allocated storage if AREA is NULL.
   Return the address to which we copied the value,
   or NULL if PTR is NULL.  */

static char *
tgetst1 (char *ptr, char **area)
{
  register char *p, *r;
  register int c;
  register int size;
  char *ret;
  register int c1;

  if (!ptr)
    return NULL;

  /* `ret' gets address of where to store the string.  */
  if (!area)
    {
      /* Compute size of block needed (may overestimate).  */
      p = ptr;
      while ((c = *p++) && c != ':' && c != '\n')
	;
      ret = (char *) xmalloc (p - ptr + 1);
    }
  else
    ret = *area;

  /* Copy the string value, stopping at null or colon.
     Also process ^ and \ abbreviations.  */
  p = ptr;
  r = ret;
  while ((c = *p++) && c != ':' && c != '\n')
    {
      if (c == '^')
	{
	  c = *p++;
	  if (c == '?')
	    c = 0177;
	  else
	    c &= 037;
	}
      else if (c == '\\')
	{
	  c = *p++;
	  if (c >= '0' && c <= '7')
	    {
	      c -= '0';
	      size = 0;

	      while (++size < 3 && (c1 = *p) >= '0' && c1 <= '7')
		{
		  c *= 8;
		  c += c1 - '0';
		  p++;
		}
	    }
#ifdef IS_EBCDIC_HOST
	  else if (c >= 0200 && c < 0360)
	    {
	      c1 = esctab[(c & ~0100) - 0200];
	      if (c1 != ' ')
		c = c1;
	    }
#else
	  else if (c >= 0100 && c < 0200)
	    {
	      c1 = esctab[(c & ~040) - 0100];
	      if (c1 != ' ')
		c = c1;
	    }
#endif
	}
      *r++ = c;
    }

  /* Sometimes entries have "%pN" which means use parameter N in the
     next %-substitution.  If all such N are continuous in the range
     [1,9] we can remove each "%pN" because they are redundant, thus
     reducing bandwidth requirements.  True, Emacs is well beyond the
     days of 150baud teletypes, but some of its users aren't much so.

     This pass could probably be integrated into the one above but
     abbreviation expansion makes that effort a little more hairy than
     its worth; this is cleaner.  */
  {
    register int last_p_param = 0;
    int remove_p_params = 1;
    struct { char *beg; int len; } cut[11];

    for (cut[0].beg = p = ret; p < r - 3; p++)
      {
	if (!remove_p_params)
	  break;
	if (*p == '%' && *(p + 1) == 'p')
	  {
	    if (*(p + 2) - '0' == 1 + last_p_param)
	      {
		cut[last_p_param].len = p - cut[last_p_param].beg;
		last_p_param++;
		p += 3;
		cut[last_p_param].beg = p;
	      }
	    else				/* not continuous: bail */
	      remove_p_params = 0;
	    if (last_p_param > 10)		/* too many: bail */
	      remove_p_params = 0;
	  }
      }
    if (remove_p_params && last_p_param)
      {
	register int i;
	char *wp;

	cut[last_p_param].len = r - cut[last_p_param].beg;
	for (i = 0, wp = ret; i <= last_p_param; wp += cut[i++].len)
	  memcpy (wp, cut[i].beg, cut[i].len);
	r = wp;
      }
  }

  *r = '\0';
  /* Update *AREA.  */
  if (area)
    *area = r + 1;
  return ret;
}

/* Outputting a string with padding.  */

char PC;

void
tputs (register const char *str, int nlines, int (*outfun) (int))
{
  register int padcount = 0;
  register int speed;

  speed = baud_rate;
  /* For quite high speeds, convert to the smaller
     units to avoid overflow.  */
  if (speed > 10000)
    speed = - speed / 100;

  if (!str)
    return;

  while (*str >= '0' && *str <= '9')
    {
      padcount += *str++ - '0';
      padcount *= 10;
    }
  if (*str == '.')
    {
      str++;
      padcount += *str++ - '0';
    }
  if (*str == '*')
    {
      str++;
      padcount *= nlines;
    }
  while (*str)
    (*outfun) (*str++);

  /* PADCOUNT is now in units of tenths of msec.
     SPEED is measured in characters per 10 seconds
     or in characters per .1 seconds (if negative).
     We use the smaller units for larger speeds to avoid overflow.  */
  padcount *= speed;
  padcount += 500;
  padcount /= 1000;
  if (speed < 0)
    padcount = -padcount;
  else
    {
      padcount += 50;
      padcount /= 100;
    }

  while (padcount-- > 0)
    (*outfun) (PC);
}

/* Finding the termcap entry in the termcap data base.  */

struct termcap_buffer
  {
    char *beg;
    ptrdiff_t size;
    char *ptr;
    int ateof;
    ptrdiff_t full;
  };

/* Forward declarations of static functions.  */

static int scan_file (char *str, int fd, register struct termcap_buffer *bufp);
static char *gobble_line (int fd, register struct termcap_buffer *bufp, char *append_end);
static int compare_contin (register char *str1, register char *str2);
static int name_match (char *line, char *name);

#ifdef MSDOS /* MW, May 1993 */
static int
valid_filename_p (char *fn)
{
  return *fn == '/' || fn[1] == ':';
}
#else
#define valid_filename_p(fn) (*(fn) == '/')
#endif

/* Find the termcap entry data for terminal type NAME
   and store it in the block that BP points to.
   Record its address for future use.

   If BP is null, space is dynamically allocated.

   Return -1 if there is some difficulty accessing the data base
   of terminal types,
   0 if the data base is accessible but the type NAME is not defined
   in it, and some other value otherwise.  */

int
tgetent (char *bp, const char *name)
{
  register char *termcap_name;
  register int fd;
  struct termcap_buffer buf;
  register char *bp1;
  char *tc_search_point;
  char *term;
  ptrdiff_t malloc_size = 0;
  register int c;
  char *tcenv = NULL;		/* TERMCAP value, if it contains :tc=.  */
  char *indirect = NULL;	/* Terminal type in :tc= in TERMCAP value.  */
  int filep;

#ifdef INTERNAL_TERMINAL
  /* For the internal terminal we don't want to read any termcap file,
     so fake it.  */
  if (!strcmp (name, "internal"))
    {
      term = INTERNAL_TERMINAL;
      if (!bp)
	{
	  malloc_size = 1 + strlen (term);
	  bp = (char *) xmalloc (malloc_size);
	}
      strcpy (bp, term);
      goto ret;
    }
#endif /* INTERNAL_TERMINAL */

  /* For compatibility with programs like `less' that want to
     put data in the termcap buffer themselves as a fallback.  */
  if (bp)
    term_entry = bp;

  termcap_name = getenv ("TERMCAP");
  if (termcap_name && *termcap_name == '\0')
    termcap_name = NULL;
#if defined (MSDOS) && !defined (TEST)
  if (termcap_name && (*termcap_name == '\\'
		       || *termcap_name == '/'
		       || termcap_name[1] == ':'))
    dostounix_filename (termcap_name);
#endif

  filep = termcap_name && valid_filename_p (termcap_name);

  /* If termcap_name is non-null and starts with / (in the un*x case, that is),
     it is a file name to use instead of /etc/termcap.
     If it is non-null and does not start with /,
     it is the entry itself, but only if
     the name the caller requested matches the TERM variable.  */

  if (termcap_name && !filep && !strcmp (name, getenv ("TERM")))
    {
      indirect = tgetst1 (find_capability (termcap_name, "tc"), (char **) 0);
      if (!indirect)
	{
	  if (!bp)
	    bp = termcap_name;
	  else
	    strcpy (bp, termcap_name);
	  goto ret;
	}
      else
	{			/* It has tc=.  Need to read /etc/termcap.  */
	  tcenv = termcap_name;
 	  termcap_name = NULL;
	}
    }

  if (!termcap_name || !filep)
    termcap_name = TERMCAP_FILE;

  /* Here we know we must search a file and termcap_name has its name.  */

#ifdef MSDOS
  fd = open (termcap_name, O_RDONLY|O_TEXT, 0);
#else
  fd = open (termcap_name, O_RDONLY, 0);
#endif
  if (fd < 0)
    return -1;

  buf.size = BUFSIZE;
  /* Add 1 to size to ensure room for terminating null.  */
  buf.beg = (char *) xmalloc (buf.size + 1);
  term = indirect ? indirect : (char *)name;

  if (!bp)
    {
      malloc_size = indirect ? strlen (tcenv) + 1 : buf.size;
      bp = (char *) xmalloc (malloc_size);
    }
  tc_search_point = bp1 = bp;

  if (indirect)
    /* Copy the data from the environment variable.  */
    {
      strcpy (bp, tcenv);
      bp1 += strlen (tcenv);
    }

  while (term)
    {
      /* Scan the file, reading it via buf, till find start of main entry.  */
      if (scan_file (term, fd, &buf) == 0)
	{
	  close (fd);
	  xfree (buf.beg);
	  if (malloc_size)
	    xfree (bp);
	  return 0;
	}

      /* Free old `term' if appropriate.  */
      if (term != name)
	xfree (term);

      /* If BP is malloc'd by us, make sure it is big enough.  */
      if (malloc_size)
	{
	  ptrdiff_t offset1 = bp1 - bp, offset2 = tc_search_point - bp;
	  malloc_size = offset1 + buf.size;
	  bp = termcap_name = (char *) xrealloc (bp, malloc_size);
	  bp1 = termcap_name + offset1;
	  tc_search_point = termcap_name + offset2;
	}

      /* Copy the line of the entry from buf into bp.  */
      termcap_name = buf.ptr;
      while ((*bp1++ = c = *termcap_name++) && c != '\n')
	/* Drop out any \ newline sequence.  */
	if (c == '\\' && *termcap_name == '\n')
	  {
	    bp1--;
	    termcap_name++;
	  }
      *bp1 = '\0';

      /* Does this entry refer to another terminal type's entry?
	 If something is found, copy it into heap and null-terminate it.  */
      tc_search_point = find_capability (tc_search_point, "tc");
      term = tgetst1 (tc_search_point, (char **) 0);
    }

  close (fd);
  xfree (buf.beg);

  if (malloc_size)
    bp = (char *) xrealloc (bp, bp1 - bp + 1);

 ret:
  term_entry = bp;
  return 1;
}

/* Given file open on FD and buffer BUFP,
   scan the file from the beginning until a line is found
   that starts the entry for terminal type STR.
   Return 1 if successful, with that line in BUFP,
   or 0 if no entry is found in the file.  */

static int
scan_file (char *str, int fd, register struct termcap_buffer *bufp)
{
  register char *end;

  bufp->ptr = bufp->beg;
  bufp->full = 0;
  bufp->ateof = 0;
  *bufp->ptr = '\0';

  lseek (fd, 0L, 0);

  while (!bufp->ateof)
    {
      /* Read a line into the buffer.  */
      end = NULL;
      do
	{
	  /* if it is continued, append another line to it,
	     until a non-continued line ends.  */
	  end = gobble_line (fd, bufp, end);
	}
      while (!bufp->ateof && end[-2] == '\\');

      if (*bufp->ptr != '#'
	  && name_match (bufp->ptr, str))
	return 1;

      /* Discard the line just processed.  */
      bufp->ptr = end;
    }
  return 0;
}

/* Return nonzero if NAME is one of the names specified
   by termcap entry LINE.  */

static int
name_match (char *line, char *name)
{
  register char *tem;

  if (!compare_contin (line, name))
    return 1;
  /* This line starts an entry.  Is it the right one?  */
  for (tem = line; *tem && *tem != '\n' && *tem != ':'; tem++)
    if (*tem == '|' && !compare_contin (tem + 1, name))
      return 1;

  return 0;
}

static int
compare_contin (register char *str1, register char *str2)
{
  register int c1, c2;
  while (1)
    {
      c1 = *str1++;
      c2 = *str2++;
      while (c1 == '\\' && *str1 == '\n')
	{
	  str1++;
	  while ((c1 = *str1++) == ' ' || c1 == '\t');
	}
      if (c2 == '\0')
	{
	  /* End of type being looked up.  */
	  if (c1 == '|' || c1 == ':')
	    /* If end of name in data base, we win.  */
	    return 0;
	  else
	    return 1;
        }
      else if (c1 != c2)
	return 1;
    }
}

/* Make sure that the buffer <- BUFP contains a full line
   of the file open on FD, starting at the place BUFP->ptr
   points to.  Can read more of the file, discard stuff before
   BUFP->ptr, or make the buffer bigger.

   Return the pointer to after the newline ending the line,
   or to the end of the file, if there is no newline to end it.

   Can also merge on continuation lines.  If APPEND_END is
   non-null, it points past the newline of a line that is
   continued; we add another line onto it and regard the whole
   thing as one line.  The caller decides when a line is continued.  */

static char *
gobble_line (int fd, register struct termcap_buffer *bufp, char *append_end)
{
  register char *end;
  register int nread;
  register char *buf = bufp->beg;

  if (!append_end)
    append_end = bufp->ptr;

  while (1)
    {
      end = append_end;
      while (*end && *end != '\n') end++;
      if (*end)
        break;
      if (bufp->ateof)
	return buf + bufp->full;
      if (bufp->ptr == buf)
	{
	  if (bufp->full == bufp->size)
	    {
	      ptrdiff_t ptr_offset = bufp->ptr - buf;
	      ptrdiff_t append_end_offset = append_end - buf;
	      /* Add 1 to size to ensure room for terminating null.  */
	      ptrdiff_t size = bufp->size + 1;
	      bufp->beg = buf = xpalloc (buf, &size, 1, -1, 1);
	      bufp->size = size - 1;
	      bufp->ptr = buf + ptr_offset;
	      append_end = buf + append_end_offset;
	    }
	}
      else
	{
	  append_end -= bufp->ptr - buf;
	  memcpy (buf, bufp->ptr, bufp->full -= bufp->ptr - buf);
	  bufp->ptr = buf;
	}
      if (!(nread = read (fd, buf + bufp->full, bufp->size - bufp->full)))
	bufp->ateof = 1;
      bufp->full += nread;
      buf[bufp->full] = '\0';
    }
  return end + 1;
}

#ifdef TEST

#ifdef NULL
#undef NULL
#endif

#include <stdio.h>

static void
tprint (char *cap)
{
  char *x = tgetstr (cap, 0);
  register char *y;

  printf ("%s: ", cap);
  if (x)
    {
      for (y = x; *y; y++)
	if (*y <= ' ' || *y == 0177)
	  printf ("\\%0o", *y);
	else
	  putchar (*y);
      free (x);
    }
  else
    printf ("none");
  putchar ('\n');
}

int
main (int argc, char **argv)
{
  char *term;
  char *buf;

  term = argv[1];
  printf ("TERM: %s\n", term);

  buf = (char *) tgetent (0, term);
  if ((int) buf <= 0)
    {
      printf ("No entry.\n");
      return 0;
    }

  printf ("Entry: %s\n", buf);

  tprint ("cm");
  tprint ("AL");

  printf ("co: %d\n", tgetnum ("co"));
  printf ("am: %d\n", tgetflag ("am"));

  return 0;
}

#endif /* TEST */
