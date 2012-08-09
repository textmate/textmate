/* Basic character support.

Copyright (C) 2001-2012  Free Software Foundation, Inc.
Copyright (C) 1995, 1997, 1998, 2001 Electrotechnical Laboratory, JAPAN.
  Licensed to the Free Software Foundation.
Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011
  National Institute of Advanced Industrial Science and Technology (AIST)
  Registration Number H13PRO009

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

/* At first, see the document in `character.h' to understand the code
   in this file.  */

#ifdef emacs
#include <config.h>
#endif

#include <stdio.h>

#ifdef emacs

#include <sys/types.h>
#include <setjmp.h>
#include <intprops.h>
#include "lisp.h"
#include "character.h"
#include "buffer.h"
#include "charset.h"
#include "composite.h"
#include "disptab.h"

#else  /* not emacs */

#include "mulelib.h"

#endif /* emacs */

Lisp_Object Qcharacterp;

static Lisp_Object Qauto_fill_chars;

/* Char-table of information about which character to unify to which
   Unicode character.  Mainly used by the macro MAYBE_UNIFY_CHAR.  */
Lisp_Object Vchar_unify_table;

/* Variable used locally in the macro FETCH_MULTIBYTE_CHAR.  */
unsigned char *_fetch_multibyte_char_p;

static Lisp_Object Qchar_script_table;



/* If character code C has modifier masks, reflect them to the
   character code if possible.  Return the resulting code.  */

int
char_resolve_modifier_mask (int c)
{
  /* A non-ASCII character can't reflect modifier bits to the code.  */
  if (! ASCII_CHAR_P ((c & ~CHAR_MODIFIER_MASK)))
    return c;

  /* For Meta, Shift, and Control modifiers, we need special care.  */
  if (c & CHAR_SHIFT)
    {
      /* Shift modifier is valid only with [A-Za-z].  */
      if ((c & 0377) >= 'A' && (c & 0377) <= 'Z')
	c &= ~CHAR_SHIFT;
      else if ((c & 0377) >= 'a' && (c & 0377) <= 'z')
	c = (c & ~CHAR_SHIFT) - ('a' - 'A');
      /* Shift modifier for control characters and SPC is ignored.  */
      else if ((c & ~CHAR_MODIFIER_MASK) <= 0x20)
	c &= ~CHAR_SHIFT;
    }
  if (c & CHAR_CTL)
    {
      /* Simulate the code in lread.c.  */
      /* Allow `\C- ' and `\C-?'.  */
      if ((c & 0377) == ' ')
	c &= ~0177 & ~ CHAR_CTL;
      else if ((c & 0377) == '?')
	c = 0177 | (c & ~0177 & ~CHAR_CTL);
      /* ASCII control chars are made from letters (both cases),
	 as well as the non-letters within 0100...0137.  */
      else if ((c & 0137) >= 0101 && (c & 0137) <= 0132)
	c &= (037 | (~0177 & ~CHAR_CTL));
      else if ((c & 0177) >= 0100 && (c & 0177) <= 0137)
	c &= (037 | (~0177 & ~CHAR_CTL));
    }
#if 0	/* This is outside the scope of this function.  (bug#4751)  */
  if (c & CHAR_META)
    {
      /* Move the meta bit to the right place for a string.  */
      c = (c & ~CHAR_META) | 0x80;
    }
#endif

  return c;
}


/* Store multibyte form of character C at P.  If C has modifier bits,
   handle them appropriately.  */

int
char_string (unsigned int c, unsigned char *p)
{
  int bytes;

  if (c & CHAR_MODIFIER_MASK)
    {
      c = char_resolve_modifier_mask (c);
      /* If C still has any modifier bits, just ignore it.  */
      c &= ~CHAR_MODIFIER_MASK;
    }

  MAYBE_UNIFY_CHAR (c);

  if (c <= MAX_3_BYTE_CHAR)
    {
      bytes = CHAR_STRING (c, p);
    }
  else if (c <= MAX_4_BYTE_CHAR)
    {
      p[0] = (0xF0 | (c >> 18));
      p[1] = (0x80 | ((c >> 12) & 0x3F));
      p[2] = (0x80 | ((c >> 6) & 0x3F));
      p[3] = (0x80 | (c & 0x3F));
      bytes = 4;
    }
  else if (c <= MAX_5_BYTE_CHAR)
    {
      p[0] = 0xF8;
      p[1] = (0x80 | ((c >> 18) & 0x0F));
      p[2] = (0x80 | ((c >> 12) & 0x3F));
      p[3] = (0x80 | ((c >> 6) & 0x3F));
      p[4] = (0x80 | (c & 0x3F));
      bytes = 5;
    }
  else if (c <= MAX_CHAR)
    {
      c = CHAR_TO_BYTE8 (c);
      bytes = BYTE8_STRING (c, p);
    }
  else
    error ("Invalid character: %x", c);

  return bytes;
}


/* Return a character whose multibyte form is at P.  If LEN is not
   NULL, it must be a pointer to integer.  In that case, set *LEN to
   the byte length of the multibyte form.  If ADVANCED is not NULL, it
   must be a pointer to unsigned char.  In that case, set *ADVANCED to
   the ending address (i.e., the starting address of the next
   character) of the multibyte form.  */

int
string_char (const unsigned char *p, const unsigned char **advanced, int *len)
{
  int c;
  const unsigned char *saved_p = p;

  if (*p < 0x80 || ! (*p & 0x20) || ! (*p & 0x10))
    {
      c = STRING_CHAR_ADVANCE (p);
    }
  else if (! (*p & 0x08))
    {
      c = ((((p)[0] & 0xF) << 18)
	   | (((p)[1] & 0x3F) << 12)
	   | (((p)[2] & 0x3F) << 6)
	   | ((p)[3] & 0x3F));
      p += 4;
    }
  else
    {
      c = ((((p)[1] & 0x3F) << 18)
	   | (((p)[2] & 0x3F) << 12)
	   | (((p)[3] & 0x3F) << 6)
	   | ((p)[4] & 0x3F));
      p += 5;
    }

  MAYBE_UNIFY_CHAR (c);

  if (len)
    *len = p - saved_p;
  if (advanced)
    *advanced = p;
  return c;
}


/* Translate character C by translation table TABLE.  If no translation is
   found in TABLE, return the untranslated character.  If TABLE is a list,
   elements are char tables.  In that case, recursively translate C by all the
   tables in the list.  */

int
translate_char (Lisp_Object table, int c)
{
  if (CHAR_TABLE_P (table))
    {
      Lisp_Object ch;

      ch = CHAR_TABLE_REF (table, c);
      if (CHARACTERP (ch))
	c = XINT (ch);
    }
  else
    {
      for (; CONSP (table); table = XCDR (table))
	c = translate_char (XCAR (table), c);
    }
  return c;
}

/* Convert ASCII or 8-bit character C to unibyte.  If C is none of
   them, return (C & 0xFF).  */

int
multibyte_char_to_unibyte (int c)
{
  if (c < 0x80)
    return c;
  if (CHAR_BYTE8_P (c))
    return CHAR_TO_BYTE8 (c);
  return (c & 0xFF);
}

/* Like multibyte_char_to_unibyte, but return -1 if C is not supported
   by charset_unibyte.  */

int
multibyte_char_to_unibyte_safe (int c)
{
  if (c < 0x80)
    return c;
  if (CHAR_BYTE8_P (c))
    return CHAR_TO_BYTE8 (c);
  return -1;
}

DEFUN ("characterp", Fcharacterp, Scharacterp, 1, 2, 0,
       doc: /* Return non-nil if OBJECT is a character.
usage: (characterp OBJECT)  */)
  (Lisp_Object object, Lisp_Object ignore)
{
  return (CHARACTERP (object) ? Qt : Qnil);
}

DEFUN ("max-char", Fmax_char, Smax_char, 0, 0, 0,
       doc: /* Return the character of the maximum code.  */)
  (void)
{
  return make_number (MAX_CHAR);
}

DEFUN ("unibyte-char-to-multibyte", Funibyte_char_to_multibyte,
       Sunibyte_char_to_multibyte, 1, 1, 0,
       doc: /* Convert the byte CH to multibyte character.  */)
  (Lisp_Object ch)
{
  int c;

  CHECK_CHARACTER (ch);
  c = XFASTINT (ch);
  if (c >= 0x100)
    error ("Not a unibyte character: %d", c);
  MAKE_CHAR_MULTIBYTE (c);
  return make_number (c);
}

DEFUN ("multibyte-char-to-unibyte", Fmultibyte_char_to_unibyte,
       Smultibyte_char_to_unibyte, 1, 1, 0,
       doc: /* Convert the multibyte character CH to a byte.
If the multibyte character does not represent a byte, return -1.  */)
  (Lisp_Object ch)
{
  int cm;

  CHECK_CHARACTER (ch);
  cm = XFASTINT (ch);
  if (cm < 256)
    /* Can't distinguish a byte read from a unibyte buffer from
       a latin1 char, so let's let it slide.  */
    return ch;
  else
    {
      int cu = CHAR_TO_BYTE_SAFE (cm);
      return make_number (cu);
    }
}


/* Return width (columns) of C considering the buffer display table DP. */

static EMACS_INT
char_width (int c, struct Lisp_Char_Table *dp)
{
  EMACS_INT width = CHAR_WIDTH (c);

  if (dp)
    {
      Lisp_Object disp = DISP_CHAR_VECTOR (dp, c), ch;
      int i;

      if (VECTORP (disp))
	for (i = 0, width = 0; i < ASIZE (disp); i++)
	  {
	    ch = AREF (disp, i);
	    if (CHARACTERP (ch))
	      {
		int w = CHAR_WIDTH (XFASTINT (ch));
		if (INT_ADD_OVERFLOW (width, w))
		  string_overflow ();
		width += w;
	      }
	  }
    }
  return width;
}


DEFUN ("char-width", Fchar_width, Schar_width, 1, 1, 0,
       doc: /* Return width of CHAR when displayed in the current buffer.
The width is measured by how many columns it occupies on the screen.
Tab is taken to occupy `tab-width' columns.
usage: (char-width CHAR)  */)
  (Lisp_Object ch)
{
  int c;
  EMACS_INT width;

  CHECK_CHARACTER (ch);
  c = XINT (ch);
  width = char_width (c, buffer_display_table ());
  return make_number (width);
}

/* Return width of string STR of length LEN when displayed in the
   current buffer.  The width is measured by how many columns it
   occupies on the screen.  If PRECISION > 0, return the width of
   longest substring that doesn't exceed PRECISION, and set number of
   characters and bytes of the substring in *NCHARS and *NBYTES
   respectively.  */

EMACS_INT
c_string_width (const unsigned char *str, EMACS_INT len, int precision,
		EMACS_INT *nchars, EMACS_INT *nbytes)
{
  EMACS_INT i = 0, i_byte = 0;
  EMACS_INT width = 0;
  struct Lisp_Char_Table *dp = buffer_display_table ();

  while (i_byte < len)
    {
      int bytes;
      int c = STRING_CHAR_AND_LENGTH (str + i_byte, bytes);
      EMACS_INT thiswidth = char_width (c, dp);

      if (precision <= 0)
	{
	  if (INT_ADD_OVERFLOW (width, thiswidth))
	    string_overflow ();
	}
      else if (precision - width < thiswidth)
	{
	  *nchars = i;
	  *nbytes = i_byte;
	  return width;
	}
      i++;
      i_byte += bytes;
      width += thiswidth;
  }

  if (precision > 0)
    {
      *nchars = i;
      *nbytes = i_byte;
    }

  return width;
}

/* Return width of string STR of length LEN when displayed in the
   current buffer.  The width is measured by how many columns it
   occupies on the screen.  */

EMACS_INT
strwidth (const char *str, EMACS_INT len)
{
  return c_string_width ((const unsigned char *) str, len, -1, NULL, NULL);
}

/* Return width of Lisp string STRING when displayed in the current
   buffer.  The width is measured by how many columns it occupies on
   the screen while paying attention to compositions.  If PRECISION >
   0, return the width of longest substring that doesn't exceed
   PRECISION, and set number of characters and bytes of the substring
   in *NCHARS and *NBYTES respectively.  */

EMACS_INT
lisp_string_width (Lisp_Object string, EMACS_INT precision,
		   EMACS_INT *nchars, EMACS_INT *nbytes)
{
  EMACS_INT len = SCHARS (string);
  /* This set multibyte to 0 even if STRING is multibyte when it
     contains only ascii and eight-bit-graphic, but that's
     intentional.  */
  int multibyte = len < SBYTES (string);
  unsigned char *str = SDATA (string);
  EMACS_INT i = 0, i_byte = 0;
  EMACS_INT width = 0;
  struct Lisp_Char_Table *dp = buffer_display_table ();

  while (i < len)
    {
      EMACS_INT chars, bytes, thiswidth;
      Lisp_Object val;
      ptrdiff_t cmp_id;
      EMACS_INT ignore, end;

      if (find_composition (i, -1, &ignore, &end, &val, string)
	  && ((cmp_id = get_composition_id (i, i_byte, end - i, val, string))
	      >= 0))
	{
	  thiswidth = composition_table[cmp_id]->width;
	  chars = end - i;
	  bytes = string_char_to_byte (string, end) - i_byte;
	}
      else
	{
	  int c;

	  if (multibyte)
	    {
	      int cbytes;
	      c = STRING_CHAR_AND_LENGTH (str + i_byte, cbytes);
	      bytes = cbytes;
	    }
	  else
	    c = str[i_byte], bytes = 1;
	  chars = 1;
	  thiswidth = char_width (c, dp);
	}

      if (precision <= 0)
	{
#ifdef emacs
	  if (INT_ADD_OVERFLOW (width, thiswidth))
	    string_overflow ();
#endif
	}
      else if (precision - width < thiswidth)
	{
	  *nchars = i;
	  *nbytes = i_byte;
	  return width;
	}
      i += chars;
      i_byte += bytes;
      width += thiswidth;
    }

  if (precision > 0)
    {
      *nchars = i;
      *nbytes = i_byte;
    }

  return width;
}

DEFUN ("string-width", Fstring_width, Sstring_width, 1, 1, 0,
       doc: /* Return width of STRING when displayed in the current buffer.
Width is measured by how many columns it occupies on the screen.
When calculating width of a multibyte character in STRING,
only the base leading-code is considered; the validity of
the following bytes is not checked.  Tabs in STRING are always
taken to occupy `tab-width' columns.
usage: (string-width STRING)  */)
  (Lisp_Object str)
{
  Lisp_Object val;

  CHECK_STRING (str);
  XSETFASTINT (val, lisp_string_width (str, -1, NULL, NULL));
  return val;
}

/* Return the number of characters in the NBYTES bytes at PTR.
   This works by looking at the contents and checking for multibyte
   sequences while assuming that there's no invalid sequence.
   However, if the current buffer has enable-multibyte-characters =
   nil, we treat each byte as a character.  */

EMACS_INT
chars_in_text (const unsigned char *ptr, EMACS_INT nbytes)
{
  /* current_buffer is null at early stages of Emacs initialization.  */
  if (current_buffer == 0
      || NILP (BVAR (current_buffer, enable_multibyte_characters)))
    return nbytes;

  return multibyte_chars_in_text (ptr, nbytes);
}

/* Return the number of characters in the NBYTES bytes at PTR.
   This works by looking at the contents and checking for multibyte
   sequences while assuming that there's no invalid sequence.  It
   ignores enable-multibyte-characters.  */

EMACS_INT
multibyte_chars_in_text (const unsigned char *ptr, EMACS_INT nbytes)
{
  const unsigned char *endp = ptr + nbytes;
  EMACS_INT chars = 0;

  while (ptr < endp)
    {
      EMACS_INT len = MULTIBYTE_LENGTH (ptr, endp);

      if (len == 0)
	abort ();
      ptr += len;
      chars++;
    }

  return chars;
}

/* Parse unibyte text at STR of LEN bytes as a multibyte text, count
   characters and bytes in it, and store them in *NCHARS and *NBYTES
   respectively.  On counting bytes, pay attention to that 8-bit
   characters not constructing a valid multibyte sequence are
   represented by 2-byte in a multibyte text.  */

void
parse_str_as_multibyte (const unsigned char *str, EMACS_INT len,
			EMACS_INT *nchars, EMACS_INT *nbytes)
{
  const unsigned char *endp = str + len;
  EMACS_INT n, chars = 0, bytes = 0;

  if (len >= MAX_MULTIBYTE_LENGTH)
    {
      const unsigned char *adjusted_endp = endp - MAX_MULTIBYTE_LENGTH;
      while (str < adjusted_endp)
	{
	  if (! CHAR_BYTE8_HEAD_P (*str)
	      && (n = MULTIBYTE_LENGTH_NO_CHECK (str)) > 0)
	    str += n, bytes += n;
	  else
	    str++, bytes += 2;
	  chars++;
	}
    }
  while (str < endp)
    {
      if (! CHAR_BYTE8_HEAD_P (*str)
	  && (n = MULTIBYTE_LENGTH (str, endp)) > 0)
	str += n, bytes += n;
      else
	str++, bytes += 2;
      chars++;
    }

  *nchars = chars;
  *nbytes = bytes;
  return;
}

/* Arrange unibyte text at STR of NBYTES bytes as a multibyte text.
   It actually converts only such 8-bit characters that don't construct
   a multibyte sequence to multibyte forms of Latin-1 characters.  If
   NCHARS is nonzero, set *NCHARS to the number of characters in the
   text.  It is assured that we can use LEN bytes at STR as a work
   area and that is enough.  Return the number of bytes of the
   resulting text.  */

EMACS_INT
str_as_multibyte (unsigned char *str, EMACS_INT len, EMACS_INT nbytes,
		  EMACS_INT *nchars)
{
  unsigned char *p = str, *endp = str + nbytes;
  unsigned char *to;
  EMACS_INT chars = 0;
  int n;

  if (nbytes >= MAX_MULTIBYTE_LENGTH)
    {
      unsigned char *adjusted_endp = endp - MAX_MULTIBYTE_LENGTH;
      while (p < adjusted_endp
	     && ! CHAR_BYTE8_HEAD_P (*p)
	     && (n = MULTIBYTE_LENGTH_NO_CHECK (p)) > 0)
	p += n, chars++;
    }
  while (p < endp
	 && ! CHAR_BYTE8_HEAD_P (*p)
	 && (n = MULTIBYTE_LENGTH (p, endp)) > 0)
    p += n, chars++;
  if (nchars)
    *nchars = chars;
  if (p == endp)
    return nbytes;

  to = p;
  nbytes = endp - p;
  endp = str + len;
  memmove (endp - nbytes, p, nbytes);
  p = endp - nbytes;

  if (nbytes >= MAX_MULTIBYTE_LENGTH)
    {
      unsigned char *adjusted_endp = endp - MAX_MULTIBYTE_LENGTH;
      while (p < adjusted_endp)
	{
	  if (! CHAR_BYTE8_HEAD_P (*p)
	      && (n = MULTIBYTE_LENGTH_NO_CHECK (p)) > 0)
	    {
	      while (n--)
		*to++ = *p++;
	    }
	  else
	    {
	      int c = *p++;
	      c = BYTE8_TO_CHAR (c);
	      to += CHAR_STRING (c, to);
	    }
	}
      chars++;
    }
  while (p < endp)
    {
      if (! CHAR_BYTE8_HEAD_P (*p)
	  && (n = MULTIBYTE_LENGTH (p, endp)) > 0)
	{
	  while (n--)
	    *to++ = *p++;
	}
      else
	{
	  int c = *p++;
	  c = BYTE8_TO_CHAR (c);
	  to += CHAR_STRING (c, to);
	}
      chars++;
    }
  if (nchars)
    *nchars = chars;
  return (to - str);
}

/* Parse unibyte string at STR of LEN bytes, and return the number of
   bytes it may occupy when converted to multibyte string by
   `str_to_multibyte'.  */

EMACS_INT
count_size_as_multibyte (const unsigned char *str, EMACS_INT len)
{
  const unsigned char *endp = str + len;
  EMACS_INT bytes;

  for (bytes = 0; str < endp; str++)
    {
      int n = *str < 0x80 ? 1 : 2;
      if (INT_ADD_OVERFLOW (bytes, n))
        string_overflow ();
      bytes += n;
    }
  return bytes;
}


/* Convert unibyte text at STR of BYTES bytes to a multibyte text
   that contains the same single-byte characters.  It actually
   converts all 8-bit characters to multibyte forms.  It is assured
   that we can use LEN bytes at STR as a work area and that is
   enough.  */

EMACS_INT
str_to_multibyte (unsigned char *str, EMACS_INT len, EMACS_INT bytes)
{
  unsigned char *p = str, *endp = str + bytes;
  unsigned char *to;

  while (p < endp && *p < 0x80) p++;
  if (p == endp)
    return bytes;
  to = p;
  bytes = endp - p;
  endp = str + len;
  memmove (endp - bytes, p, bytes);
  p = endp - bytes;
  while (p < endp)
    {
      int c = *p++;

      if (c >= 0x80)
	c = BYTE8_TO_CHAR (c);
      to += CHAR_STRING (c, to);
    }
  return (to - str);
}

/* Arrange multibyte text at STR of LEN bytes as a unibyte text.  It
   actually converts characters in the range 0x80..0xFF to
   unibyte.  */

EMACS_INT
str_as_unibyte (unsigned char *str, EMACS_INT bytes)
{
  const unsigned char *p = str, *endp = str + bytes;
  unsigned char *to;
  int c, len;

  while (p < endp)
    {
      c = *p;
      len = BYTES_BY_CHAR_HEAD (c);
      if (CHAR_BYTE8_HEAD_P (c))
	break;
      p += len;
    }
  to = str + (p - str);
  while (p < endp)
    {
      c = *p;
      len = BYTES_BY_CHAR_HEAD (c);
      if (CHAR_BYTE8_HEAD_P (c))
	{
	  c = STRING_CHAR_ADVANCE (p);
	  *to++ = CHAR_TO_BYTE8 (c);
	}
      else
	{
	  while (len--) *to++ = *p++;
	}
    }
  return (to - str);
}

/* Convert eight-bit chars in SRC (in multibyte form) to the
   corresponding byte and store in DST.  CHARS is the number of
   characters in SRC.  The value is the number of bytes stored in DST.
   Usually, the value is the same as CHARS, but is less than it if SRC
   contains a non-ASCII, non-eight-bit character.  If ACCEPT_LATIN_1
   is nonzero, a Latin-1 character is accepted and converted to a byte
   of that character code.
   Note: Currently the arg ACCEPT_LATIN_1 is not used.  */

EMACS_INT
str_to_unibyte (const unsigned char *src, unsigned char *dst, EMACS_INT chars, int accept_latin_1)
{
  EMACS_INT i;

  for (i = 0; i < chars; i++)
    {
      int c = STRING_CHAR_ADVANCE (src);

      if (CHAR_BYTE8_P (c))
	c = CHAR_TO_BYTE8 (c);
      else if (! ASCII_CHAR_P (c)
	       && (! accept_latin_1 || c >= 0x100))
	return i;
      *dst++ = c;
    }
  return i;
}


static EMACS_INT
string_count_byte8 (Lisp_Object string)
{
  int multibyte = STRING_MULTIBYTE (string);
  EMACS_INT nbytes = SBYTES (string);
  unsigned char *p = SDATA (string);
  unsigned char *pend = p + nbytes;
  EMACS_INT count = 0;
  int c, len;

  if (multibyte)
    while (p < pend)
      {
	c = *p;
	len = BYTES_BY_CHAR_HEAD (c);

	if (CHAR_BYTE8_HEAD_P (c))
	  count++;
	p += len;
      }
  else
    while (p < pend)
      {
	if (*p++ >= 0x80)
	  count++;
      }
  return count;
}


Lisp_Object
string_escape_byte8 (Lisp_Object string)
{
  EMACS_INT nchars = SCHARS (string);
  EMACS_INT nbytes = SBYTES (string);
  int multibyte = STRING_MULTIBYTE (string);
  EMACS_INT byte8_count;
  const unsigned char *src, *src_end;
  unsigned char *dst;
  Lisp_Object val;
  int c, len;

  if (multibyte && nchars == nbytes)
    return string;

  byte8_count = string_count_byte8 (string);

  if (byte8_count == 0)
    return string;

  if (multibyte)
    {
      if ((MOST_POSITIVE_FIXNUM - nchars) / 3 < byte8_count
	  || (STRING_BYTES_BOUND - nbytes) / 2 < byte8_count)
	string_overflow ();

      /* Convert 2-byte sequence of byte8 chars to 4-byte octal.  */
      val = make_uninit_multibyte_string (nchars + byte8_count * 3,
					  nbytes + byte8_count * 2);
    }
  else
    {
      if ((STRING_BYTES_BOUND - nbytes) / 3 < byte8_count)
	string_overflow ();

      /* Convert 1-byte sequence of byte8 chars to 4-byte octal.  */
      val = make_uninit_string (nbytes + byte8_count * 3);
    }

  src = SDATA (string);
  src_end = src + nbytes;
  dst = SDATA (val);
  if (multibyte)
    while (src < src_end)
      {
	c = *src;
	len = BYTES_BY_CHAR_HEAD (c);

	if (CHAR_BYTE8_HEAD_P (c))
	  {
	    c = STRING_CHAR_ADVANCE (src);
	    c = CHAR_TO_BYTE8 (c);
	    sprintf ((char *) dst, "\\%03o", c);
	    dst += 4;
	  }
	else
	  while (len--) *dst++ = *src++;
      }
  else
    while (src < src_end)
      {
	c = *src++;
	if (c >= 0x80)
	  {
	    sprintf ((char *) dst, "\\%03o", c);
	    dst += 4;
	  }
	else
	  *dst++ = c;
      }
  return val;
}


DEFUN ("string", Fstring, Sstring, 0, MANY, 0,
       doc: /*
Concatenate all the argument characters and make the result a string.
usage: (string &rest CHARACTERS)  */)
  (ptrdiff_t n, Lisp_Object *args)
{
  ptrdiff_t i;
  int c;
  unsigned char *buf, *p;
  Lisp_Object str;
  USE_SAFE_ALLOCA;

  SAFE_NALLOCA (buf, MAX_MULTIBYTE_LENGTH, n);
  p = buf;

  for (i = 0; i < n; i++)
    {
      CHECK_CHARACTER (args[i]);
      c = XINT (args[i]);
      p += CHAR_STRING (c, p);
    }

  str = make_string_from_bytes ((char *) buf, n, p - buf);
  SAFE_FREE ();
  return str;
}

DEFUN ("unibyte-string", Funibyte_string, Sunibyte_string, 0, MANY, 0,
       doc: /* Concatenate all the argument bytes and make the result a unibyte string.
usage: (unibyte-string &rest BYTES)  */)
  (ptrdiff_t n, Lisp_Object *args)
{
  ptrdiff_t i;
  int c;
  unsigned char *buf, *p;
  Lisp_Object str;
  USE_SAFE_ALLOCA;

  SAFE_ALLOCA (buf, unsigned char *, n);
  p = buf;

  for (i = 0; i < n; i++)
    {
      CHECK_NATNUM (args[i]);
      c = XFASTINT (args[i]);
      if (c >= 256)
	args_out_of_range_3 (args[i], make_number (0), make_number (255));
      *p++ = c;
    }

  str = make_string_from_bytes ((char *) buf, n, p - buf);
  SAFE_FREE ();
  return str;
}

DEFUN ("char-resolve-modifiers", Fchar_resolve_modifiers,
       Schar_resolve_modifiers, 1, 1, 0,
       doc: /* Resolve modifiers in the character CHAR.
The value is a character with modifiers resolved into the character
code.  Unresolved modifiers are kept in the value.
usage: (char-resolve-modifiers CHAR)  */)
  (Lisp_Object character)
{
  int c;

  CHECK_NUMBER (character);
  c = XINT (character);
  return make_number (char_resolve_modifier_mask (c));
}

DEFUN ("get-byte", Fget_byte, Sget_byte, 0, 2, 0,
       doc: /* Return a byte value of a character at point.
Optional 1st arg POSITION, if non-nil, is a position of a character to get
a byte value.
Optional 2nd arg STRING, if non-nil, is a string of which first
character is a target to get a byte value.  In this case, POSITION, if
non-nil, is an index of a target character in the string.

If the current buffer (or STRING) is multibyte, and the target
character is not ASCII nor 8-bit character, an error is signaled.  */)
  (Lisp_Object position, Lisp_Object string)
{
  int c;
  EMACS_INT pos;
  unsigned char *p;

  if (NILP (string))
    {
      if (NILP (position))
	{
	  p = PT_ADDR;
	}
      else
	{
	  CHECK_NUMBER_COERCE_MARKER (position);
	  if (XINT (position) < BEGV || XINT (position) >= ZV)
	    args_out_of_range_3 (position, make_number (BEGV), make_number (ZV));
	  pos = XFASTINT (position);
	  p = CHAR_POS_ADDR (pos);
	}
      if (NILP (BVAR (current_buffer, enable_multibyte_characters)))
	return make_number (*p);
    }
  else
    {
      CHECK_STRING (string);
      if (NILP (position))
	{
	  p = SDATA (string);
	}
      else
	{
	  CHECK_NATNUM (position);
	  if (XINT (position) >= SCHARS (string))
	    args_out_of_range (string, position);
	  pos = XFASTINT (position);
	  p = SDATA (string) + string_char_to_byte (string, pos);
	}
      if (! STRING_MULTIBYTE (string))
	return make_number (*p);
    }
  c = STRING_CHAR (p);
  if (CHAR_BYTE8_P (c))
    c = CHAR_TO_BYTE8 (c);
  else if (! ASCII_CHAR_P (c))
    error ("Not an ASCII nor an 8-bit character: %d", c);
  return make_number (c);
}


void
init_character_once (void)
{
}

#ifdef emacs

void
syms_of_character (void)
{
  DEFSYM (Qcharacterp, "characterp");
  DEFSYM (Qauto_fill_chars, "auto-fill-chars");

  staticpro (&Vchar_unify_table);
  Vchar_unify_table = Qnil;

  defsubr (&Smax_char);
  defsubr (&Scharacterp);
  defsubr (&Sunibyte_char_to_multibyte);
  defsubr (&Smultibyte_char_to_unibyte);
  defsubr (&Schar_width);
  defsubr (&Sstring_width);
  defsubr (&Sstring);
  defsubr (&Sunibyte_string);
  defsubr (&Schar_resolve_modifiers);
  defsubr (&Sget_byte);

  DEFVAR_LISP ("translation-table-vector",  Vtranslation_table_vector,
	       doc: /*
Vector recording all translation tables ever defined.
Each element is a pair (SYMBOL . TABLE) relating the table to the
symbol naming it.  The ID of a translation table is an index into this vector.  */);
  Vtranslation_table_vector = Fmake_vector (make_number (16), Qnil);

  DEFVAR_LISP ("auto-fill-chars", Vauto_fill_chars,
	       doc: /*
A char-table for characters which invoke auto-filling.
Such characters have value t in this table.  */);
  Vauto_fill_chars = Fmake_char_table (Qauto_fill_chars, Qnil);
  CHAR_TABLE_SET (Vauto_fill_chars, ' ', Qt);
  CHAR_TABLE_SET (Vauto_fill_chars, '\n', Qt);

  DEFVAR_LISP ("char-width-table", Vchar_width_table,
	       doc: /*
A char-table for width (columns) of each character.  */);
  Vchar_width_table = Fmake_char_table (Qnil, make_number (1));
  char_table_set_range (Vchar_width_table, 0x80, 0x9F, make_number (4));
  char_table_set_range (Vchar_width_table, MAX_5_BYTE_CHAR + 1, MAX_CHAR,
			make_number (4));

  DEFVAR_LISP ("printable-chars", Vprintable_chars,
	       doc: /* A char-table for each printable character.  */);
  Vprintable_chars = Fmake_char_table (Qnil, Qnil);
  Fset_char_table_range (Vprintable_chars,
			 Fcons (make_number (32), make_number (126)), Qt);
  Fset_char_table_range (Vprintable_chars,
			 Fcons (make_number (160),
				make_number (MAX_5_BYTE_CHAR)), Qt);

  DEFVAR_LISP ("char-script-table", Vchar_script_table,
	       doc: /* Char table of script symbols.
It has one extra slot whose value is a list of script symbols.  */);

  /* Intern this now in case it isn't already done.
     Setting this variable twice is harmless.
     But don't staticpro it here--that is done in alloc.c.  */
  Qchar_table_extra_slots = intern_c_string ("char-table-extra-slots");
  DEFSYM (Qchar_script_table, "char-script-table");
  Fput (Qchar_script_table, Qchar_table_extra_slots, make_number (1));
  Vchar_script_table = Fmake_char_table (Qchar_script_table, Qnil);

  DEFVAR_LISP ("script-representative-chars", Vscript_representative_chars,
	       doc: /* Alist of scripts vs the representative characters.
Each element is a cons (SCRIPT . CHARS).
SCRIPT is a symbol representing a script or a subgroup of a script.
CHARS is a list or a vector of characters.
If it is a list, all characters in the list are necessary for supporting SCRIPT.
If it is a vector, one of the characters in the vector is necessary.
This variable is used to find a font for a specific script.  */);
  Vscript_representative_chars = Qnil;

  DEFVAR_LISP ("unicode-category-table", Vunicode_category_table,
	       doc: /* Char table of Unicode's "General Category".
All Unicode characters have one of the following values (symbol):
  Lu, Ll, Lt, Lm, Lo, Mn, Mc, Me, Nd, Nl, No, Pc, Pd, Ps, Pe, Pi, Pf, Po,
  Sm, Sc, Sk, So, Zs, Zl, Zp, Cc, Cf, Cs, Co, Cn
See The Unicode Standard for the meaning of those values.  */);
  /* The correct char-table is setup in characters.el.  */
  Vunicode_category_table = Qnil;
}

#endif /* emacs */
