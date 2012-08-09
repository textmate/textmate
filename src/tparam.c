/* Merge parameters into a termcap entry string.
   Copyright (C) 1985, 1987, 1993, 1995, 2000, 2001, 2002, 2003, 2004,
                 2005, 2006, 2007, 2008  Free Software Foundation, Inc.

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
#include "lisp.h"		/* for xmalloc */
#include "tparam.h"

#ifndef NULL
#define NULL (char *) 0
#endif

/* Assuming STRING is the value of a termcap string entry
   containing `%' constructs to expand parameters,
   merge in parameter values and store result in block OUTSTRING points to.
   LEN is the length of OUTSTRING.  If more space is needed,
   a block is allocated with `malloc'.

   The value returned is the address of the resulting string.
   This may be OUTSTRING or may be the address of a block got with `malloc'.
   In the latter case, the caller must free the block.

   The fourth and following args to tparam serve as the parameter values.  */

static char *tparam1 (char const *string, char *outstring, int len,
		      char *up, char *left, int *argp);

char *
tparam (const char *string, char *outstring, int len,
	int arg0, int arg1, int arg2, int arg3)
{
  int arg[4];

  arg[0] = arg0;
  arg[1] = arg1;
  arg[2] = arg2;
  arg[3] = arg3;
  return tparam1 (string, outstring, len, NULL, NULL, arg);
}

char *BC;
char *UP;

static char tgoto_buf[50];

char *
tgoto (const char *cm, int hpos, int vpos)
{
  int args[2];
  if (!cm)
    return NULL;
  args[0] = vpos;
  args[1] = hpos;
  return tparam1 (cm, tgoto_buf, 50, UP, BC, args);
}

static char *
tparam1 (const char *string, char *outstring, int len,
	 char *up, char *left, register int *argp)
{
  register int c;
  register const char *p = string;
  register char *op = outstring;
  char *outend;
  char *new = 0;
  ptrdiff_t outlen = 0;

  register int tem;
  int *old_argp = argp;                 /* can move */
  int *fixed_argp = argp;               /* never moves */
  int explicit_param_p = 0;             /* set by %p */
  ptrdiff_t doleft = 0;
  ptrdiff_t doup = 0;
  ptrdiff_t append_len = 0;

  outend = outstring + len;

  while (1)
    {
      /* If the buffer might be too short, make it bigger.  */
      while (outend - op - append_len <= 5)
	{
	  ptrdiff_t offset = op - outstring;

	  if (outlen == 0)
	    {
	      outlen = len + 40;
	      new = (char *) xmalloc (outlen);
	      memcpy (new, outstring, offset);
	    }
	  else
	    {
	      new = xpalloc (outstring, &outlen, 1, -1, 1);
	    }

	  op = new + offset;
	  outend = new + outlen;
	  outstring = new;
	}
      c = *p++;
      if (!c)
	break;
      if (c == '%')
	{
	  c = *p++;
	  if (explicit_param_p)
	    explicit_param_p = 0;
	  else
	    tem = *argp;
	  switch (c)
	    {
	    case 'd':		/* %d means output in decimal.  */
	      if (tem < 10)
		goto onedigit;
	      if (tem < 100)
		goto twodigit;
	    case '3':		/* %3 means output in decimal, 3 digits.  */
	      if (tem > 999)
		{
		  *op++ = tem / 1000 + '0';
		  tem %= 1000;
		}
	      *op++ = tem / 100 + '0';
	    case '2':		/* %2 means output in decimal, 2 digits.  */
	    twodigit:
	      tem %= 100;
	      *op++ = tem / 10 + '0';
	    onedigit:
	      *op++ = tem % 10 + '0';
	      argp++;
	      break;
            case 'p':           /* %pN means use param N for next subst.  */
	      tem = fixed_argp[(*p++) - '1'];
	      explicit_param_p = 1;
	      break;
	    case 'C':
	      /* For c-100: print quotient of value by 96, if nonzero,
		 then do like %+.  */
	      if (tem >= 96)
		{
		  *op++ = tem / 96;
		  tem %= 96;
		}
	    case '+':		/* %+x means add character code of char x.  */
	      tem += *p++;
	    case '.':		/* %. means output as character.  */
	      if (left)
		{
		  /* If want to forbid output of 0 and \n and \t,
		     and this is one of them, increment it.  */
		  while (tem == 0 || tem == '\n' || tem == '\t')
		    {
		      ptrdiff_t append_len_incr;
		      tem++;
		      if (argp == old_argp)
			doup++, append_len_incr = strlen (up);
		      else
			doleft++, append_len_incr = strlen (left);
		      if (INT_ADD_OVERFLOW (append_len, append_len_incr))
			memory_full (SIZE_MAX);
		      append_len += append_len_incr;
		    }
		}
	      *op++ = tem ? tem : 0200;
	    case 'f':		/* %f means discard next arg.  */
	      argp++;
	      break;

	    case 'b':		/* %b means back up one arg (and re-use it).  */
	      argp--;
	      break;

	    case 'r':		/* %r means interchange following two args.  */
	      argp[0] = argp[1];
	      argp[1] = tem;
	      old_argp++;
	      break;

	    case '>':		/* %>xy means if arg is > char code of x, */
	      if (argp[0] > *p++) /* then add char code of y to the arg, */
		argp[0] += *p;	/* and in any case don't output.  */
	      p++;		/* Leave the arg to be output later.  */
	      break;

	    case 'a':		/* %a means arithmetic.  */
	      /* Next character says what operation.
		 Add or subtract either a constant or some other arg.  */
	      /* First following character is + to add or - to subtract
		 or = to assign.  */
	      /* Next following char is 'p' and an arg spec
		 (0100 plus position of that arg relative to this one)
		 or 'c' and a constant stored in a character.  */
	      tem = p[2] & 0177;
	      if (p[1] == 'p')
		tem = argp[tem - 0100];
	      if (p[0] == '-')
		argp[0] -= tem;
	      else if (p[0] == '+')
		argp[0] += tem;
	      else if (p[0] == '*')
		argp[0] *= tem;
	      else if (p[0] == '/')
		argp[0] /= tem;
	      else
		argp[0] = tem;

	      p += 3;
	      break;

	    case 'i':		/* %i means add one to arg, */
	      argp[0] ++;	/* and leave it to be output later.  */
	      argp[1] ++;	/* Increment the following arg, too!  */
	      break;

	    case '%':		/* %% means output %; no arg.  */
	      goto ordinary;

	    case 'n':		/* %n means xor each of next two args with 140.  */
	      argp[0] ^= 0140;
	      argp[1] ^= 0140;
	      break;

	    case 'm':		/* %m means xor each of next two args with 177.  */
	      argp[0] ^= 0177;
	      argp[1] ^= 0177;
	      break;

	    case 'B':		/* %B means express arg as BCD char code.  */
	      argp[0] += 6 * (tem / 10);
	      break;

	    case 'D':		/* %D means weird Delta Data transformation.  */
	      argp[0] -= 2 * (tem % 16);
	      break;

	    default:
	      abort ();
	    }
	}
      else
	/* Ordinary character in the argument string.  */
      ordinary:
	*op++ = c;
    }
  *op = 0;
  while (doup-- > 0)
    strcat (op, up);
  while (doleft-- > 0)
    strcat (op, left);
  return outstring;
}

#ifdef DEBUG

int
main (int argc, char **argv)
{
  char buf[50];
  int args[3];
  args[0] = atoi (argv[2]);
  args[1] = atoi (argv[3]);
  args[2] = atoi (argv[4]);
  tparam1 (argv[1], buf, 50, "LEFT", "UP", args);
  printf ("%s\n", buf);
  return 0;
}

#endif /* DEBUG */
