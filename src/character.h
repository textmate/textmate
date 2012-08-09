/* Header for multibyte character handler.
   Copyright (C) 1995, 1997, 1998 Electrotechnical Laboratory, JAPAN.
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

#ifndef EMACS_CHARACTER_H
#define EMACS_CHARACTER_H

#include <verify.h>

/* character code	1st byte   byte sequence
   --------------	--------   -------------
        0-7F		00..7F	   0xxxxxxx
       80-7FF		C2..DF	   110xxxxx 10xxxxxx
      800-FFFF		E0..EF	   1110xxxx 10xxxxxx 10xxxxxx
    10000-1FFFFF	F0..F7	   11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
   200000-3FFF7F	F8	   11111000 1000xxxx 10xxxxxx 10xxxxxx 10xxxxxx
   3FFF80-3FFFFF	C0..C1	   1100000x 10xxxxxx (for eight-bit-char)
   400000-...		invalid

   invalid 1st byte	80..BF	   10xxxxxx
			F9..FF	   11111xxx (xxx != 000)
*/

/* Maximum character code ((1 << CHARACTERBITS) - 1).  */
#define MAX_CHAR  0x3FFFFF

/* Maximum Unicode character code.  */
#define MAX_UNICODE_CHAR 0x10FFFF

/* Maximum N-byte character codes.  */
#define MAX_1_BYTE_CHAR 0x7F
#define MAX_2_BYTE_CHAR 0x7FF
#define MAX_3_BYTE_CHAR 0xFFFF
#define MAX_4_BYTE_CHAR 0x1FFFFF
#define MAX_5_BYTE_CHAR 0x3FFF7F

/* Minimum leading code of multibyte characters.  */
#define MIN_MULTIBYTE_LEADING_CODE 0xC0
/* Maximum leading code of multibyte characters.  */
#define MAX_MULTIBYTE_LEADING_CODE 0xF8

/* Nonzero iff C is a character that corresponds to a raw 8-bit
   byte.  */
#define CHAR_BYTE8_P(c) ((c) > MAX_5_BYTE_CHAR)

/* Return the character code for raw 8-bit byte BYTE.  */
#define BYTE8_TO_CHAR(byte) ((byte) + 0x3FFF00)

#define UNIBYTE_TO_CHAR(byte) \
  (ASCII_BYTE_P (byte) ? (byte) : BYTE8_TO_CHAR (byte))

/* Return the raw 8-bit byte for character C.  */
#define CHAR_TO_BYTE8(c)	\
  (CHAR_BYTE8_P (c)		\
   ? (c) - 0x3FFF00		\
   : multibyte_char_to_unibyte (c))

/* Return the raw 8-bit byte for character C,
   or -1 if C doesn't correspond to a byte.  */
#define CHAR_TO_BYTE_SAFE(c)	\
  (CHAR_BYTE8_P (c)		\
   ? (c) - 0x3FFF00		\
   : multibyte_char_to_unibyte_safe (c))

/* Nonzero iff BYTE is the 1st byte of a multibyte form of a character
   that corresponds to a raw 8-bit byte.  */
#define CHAR_BYTE8_HEAD_P(byte) ((byte) == 0xC0 || (byte) == 0xC1)

/* If C is not ASCII, make it unibyte. */
#define MAKE_CHAR_UNIBYTE(c)	\
  do {				\
    if (! ASCII_CHAR_P (c))	\
      c = CHAR_TO_BYTE8 (c);	\
  } while (0)


/* If C is not ASCII, make it multibyte.  Assumes C < 256.  */
#define MAKE_CHAR_MULTIBYTE(c) \
  (eassert ((c) >= 0 && (c) < 256), (c) = UNIBYTE_TO_CHAR (c))

/* This is the maximum byte length of multibyte form.  */
#define MAX_MULTIBYTE_LENGTH 5

/* Return a Lisp character whose character code is C.  Assumes C is
   a valid character code.  */
#define make_char(c) make_number (c)

/* Nonzero iff C is an ASCII byte.  */
#define ASCII_BYTE_P(c) UNSIGNED_CMP (c, <, 0x80)

/* Nonzero iff X is a character.  */
#define CHARACTERP(x) (NATNUMP (x) && XFASTINT (x) <= MAX_CHAR)

/* Nonzero iff C is valid as a character code.  */
#define CHAR_VALID_P(c) UNSIGNED_CMP (c, <=, MAX_CHAR)

/* Check if Lisp object X is a character or not.  */
#define CHECK_CHARACTER(x) \
  CHECK_TYPE (CHARACTERP (x), Qcharacterp, x)

#define CHECK_CHARACTER_CAR(x) \
  do {					\
    Lisp_Object tmp = XCAR (x);		\
    CHECK_CHARACTER (tmp);		\
    XSETCAR ((x), tmp);			\
  } while (0)

#define CHECK_CHARACTER_CDR(x) \
  do {					\
    Lisp_Object tmp = XCDR (x);		\
    CHECK_CHARACTER (tmp);		\
    XSETCDR ((x), tmp);			\
  } while (0)

/* Nonzero iff C is a character of code less than 0x100.  */
#define SINGLE_BYTE_CHAR_P(c) UNSIGNED_CMP (c, <, 0x100)

/* Nonzero if character C has a printable glyph.  */
#define CHAR_PRINTABLE_P(c)	\
  (((c) >= 32 && (c) < 127)	\
   || ! NILP (CHAR_TABLE_REF (Vprintable_chars, (c))))

/* Return byte length of multibyte form for character C.  */
#define CHAR_BYTES(c)			\
  ( (c) <= MAX_1_BYTE_CHAR ? 1		\
    : (c) <= MAX_2_BYTE_CHAR ? 2	\
    : (c) <= MAX_3_BYTE_CHAR ? 3	\
    : (c) <= MAX_4_BYTE_CHAR ? 4	\
    : (c) <= MAX_5_BYTE_CHAR ? 5	\
    : 2)


/* Return the leading code of multibyte form of C.  */
#define CHAR_LEADING_CODE(c)				\
  ((c) <= MAX_1_BYTE_CHAR ? c				\
   : (c) <= MAX_2_BYTE_CHAR ? (0xC0 | ((c) >> 6))	\
   : (c) <= MAX_3_BYTE_CHAR ? (0xE0 | ((c) >> 12))	\
   : (c) <= MAX_4_BYTE_CHAR ? (0xF0 | ((c) >> 18))	\
   : (c) <= MAX_5_BYTE_CHAR ? 0xF8			\
   : (0xC0 | (((c) >> 6) & 0x01)))


/* Store multibyte form of the character C in P.  The caller should
   allocate at least MAX_MULTIBYTE_LENGTH bytes area at P in advance.
   Returns the length of the multibyte form.  */

#define CHAR_STRING(c, p)			\
  (UNSIGNED_CMP (c, <=, MAX_1_BYTE_CHAR)	\
   ? ((p)[0] = (c),				\
      1)					\
   : UNSIGNED_CMP (c, <=, MAX_2_BYTE_CHAR)	\
   ? ((p)[0] = (0xC0 | ((c) >> 6)),		\
      (p)[1] = (0x80 | ((c) & 0x3F)),		\
      2)					\
   : UNSIGNED_CMP (c, <=, MAX_3_BYTE_CHAR)	\
   ? ((p)[0] = (0xE0 | ((c) >> 12)),		\
      (p)[1] = (0x80 | (((c) >> 6) & 0x3F)),	\
      (p)[2] = (0x80 | ((c) & 0x3F)),		\
      3)					\
   : verify_expr (sizeof (c) <= sizeof (unsigned), char_string (c, p)))

/* Store multibyte form of byte B in P.  The caller should allocate at
   least MAX_MULTIBYTE_LENGTH bytes area at P in advance.  Returns the
   length of the multibyte form.  */

#define BYTE8_STRING(b, p)			\
  ((p)[0] = (0xC0 | (((b) >> 6) & 0x01)),	\
   (p)[1] = (0x80 | ((b) & 0x3F)),		\
   2)


/* Store multibyte form of the character C in P and advance P to the
   end of the multibyte form.  The caller should allocate at least
   MAX_MULTIBYTE_LENGTH bytes area at P in advance.  */

#define CHAR_STRING_ADVANCE(c, p)		\
  do {						\
    if ((c) <= MAX_1_BYTE_CHAR)			\
      *(p)++ = (c);				\
    else if ((c) <= MAX_2_BYTE_CHAR)		\
      *(p)++ = (0xC0 | ((c) >> 6)),		\
	*(p)++ = (0x80 | ((c) & 0x3F));		\
    else if ((c) <= MAX_3_BYTE_CHAR)		\
      *(p)++ = (0xE0 | ((c) >> 12)),		\
	*(p)++ = (0x80 | (((c) >> 6) & 0x3F)),	\
	*(p)++ = (0x80 | ((c) & 0x3F));		\
    else					\
      {						\
	verify (sizeof (c) <= sizeof (unsigned));	\
	(p) += char_string (c, p);		\
      }						\
  } while (0)


/* Nonzero iff BYTE starts a non-ASCII character in a multibyte
   form.  */
#define LEADING_CODE_P(byte) (((byte) & 0xC0) == 0xC0)

/* Nonzero iff BYTE is a trailing code of a non-ASCII character in a
   multibyte form.  */
#define TRAILING_CODE_P(byte) (((byte) & 0xC0) == 0x80)

/* Nonzero iff BYTE starts a character in a multibyte form.
   This is equivalent to:
	(ASCII_BYTE_P (byte) || LEADING_CODE_P (byte))  */
#define CHAR_HEAD_P(byte) (((byte) & 0xC0) != 0x80)

/* How many bytes a character that starts with BYTE occupies in a
   multibyte form.  */
#define BYTES_BY_CHAR_HEAD(byte)	\
  (!((byte) & 0x80) ? 1			\
   : !((byte) & 0x20) ? 2		\
   : !((byte) & 0x10) ? 3		\
   : !((byte) & 0x08) ? 4		\
   : 5)


/* The byte length of multibyte form at unibyte string P ending at
   PEND.  If STR doesn't point to a valid multibyte form, return 0.  */

#define MULTIBYTE_LENGTH(p, pend)				\
  (p >= pend ? 0						\
   : !((p)[0] & 0x80) ? 1					\
   : ((p + 1 >= pend) || (((p)[1] & 0xC0) != 0x80)) ? 0		\
   : ((p)[0] & 0xE0) == 0xC0 ? 2				\
   : ((p + 2 >= pend) || (((p)[2] & 0xC0) != 0x80)) ? 0		\
   : ((p)[0] & 0xF0) == 0xE0 ? 3				\
   : ((p + 3 >= pend) || (((p)[3] & 0xC0) != 0x80)) ? 0		\
   : ((p)[0] & 0xF8) == 0xF0 ? 4				\
   : ((p + 4 >= pend) || (((p)[4] & 0xC0) != 0x80)) ? 0		\
   : (p)[0] == 0xF8 && ((p)[1] & 0xF0) == 0x80 ? 5		\
   : 0)


/* Like MULTIBYTE_LENGTH, but don't check the ending address.  */

#define MULTIBYTE_LENGTH_NO_CHECK(p)			\
  (!((p)[0] & 0x80) ? 1					\
   : ((p)[1] & 0xC0) != 0x80 ? 0			\
   : ((p)[0] & 0xE0) == 0xC0 ? 2			\
   : ((p)[2] & 0xC0) != 0x80 ? 0			\
   : ((p)[0] & 0xF0) == 0xE0 ? 3			\
   : ((p)[3] & 0xC0) != 0x80 ? 0			\
   : ((p)[0] & 0xF8) == 0xF0 ? 4			\
   : ((p)[4] & 0xC0) != 0x80 ? 0			\
   : (p)[0] == 0xF8 && ((p)[1] & 0xF0) == 0x80 ? 5	\
   : 0)

/* If P is before LIMIT, advance P to the next character boundary.
   Assumes that P is already at a character boundary of the same
   multibyte form whose end address is LIMIT.  */

#define NEXT_CHAR_BOUNDARY(p, limit)	\
  do {					\
    if ((p) < (limit))			\
      (p) += BYTES_BY_CHAR_HEAD (*(p));	\
  } while (0)


/* If P is after LIMIT, advance P to the previous character boundary.
   Assumes that P is already at a character boundary of the same
   multibyte form whose beginning address is LIMIT.  */

#define PREV_CHAR_BOUNDARY(p, limit)					\
  do {									\
    if ((p) > (limit))							\
      {									\
	const unsigned char *chp = (p);					\
	do {								\
	  chp--;							\
	} while (chp >= limit && ! CHAR_HEAD_P (*chp));			\
	(p) = (BYTES_BY_CHAR_HEAD (*chp) == (p) - chp) ? chp : (p) - 1;	\
      }									\
  } while (0)

/* Return the character code of character whose multibyte form is at
   P.  Note that this macro unifies CJK characters whose codepoints
   are in the Private Use Areas (PUAs), so it might return a different
   codepoint from the one actually stored at P.  */

#define STRING_CHAR(p)						\
  (!((p)[0] & 0x80)						\
   ? (p)[0]							\
   : ! ((p)[0] & 0x20)						\
   ? (((((p)[0] & 0x1F) << 6)					\
       | ((p)[1] & 0x3F))					\
      + (((unsigned char) (p)[0]) < 0xC2 ? 0x3FFF80 : 0))	\
   : ! ((p)[0] & 0x10)						\
   ? ((((p)[0] & 0x0F) << 12)					\
      | (((p)[1] & 0x3F) << 6)					\
      | ((p)[2] & 0x3F))					\
   : string_char ((p), NULL, NULL))


/* Like STRING_CHAR, but set ACTUAL_LEN to the length of multibyte
   form.

   Note: This macro returns the actual length of the character's
   multibyte sequence as it is stored in a buffer or string.  The
   character it returns might have a different codepoint that has a
   different multibyte sequence of a different legth, due to possible
   unification of CJK characters inside string_char.  Therefore do NOT
   assume that the length returned by this macro is identical to the
   length of the multibyte sequence of the character it returns.  */

#define STRING_CHAR_AND_LENGTH(p, actual_len)			\
  (!((p)[0] & 0x80)						\
   ? ((actual_len) = 1, (p)[0])					\
   : ! ((p)[0] & 0x20)						\
   ? ((actual_len) = 2,						\
      (((((p)[0] & 0x1F) << 6)					\
	| ((p)[1] & 0x3F))					\
       + (((unsigned char) (p)[0]) < 0xC2 ? 0x3FFF80 : 0)))	\
   : ! ((p)[0] & 0x10)						\
   ? ((actual_len) = 3,						\
      ((((p)[0] & 0x0F) << 12)					\
       | (((p)[1] & 0x3F) << 6)					\
       | ((p)[2] & 0x3F)))					\
   : string_char ((p), NULL, &actual_len))


/* Like STRING_CHAR, but advance P to the end of multibyte form.  */

#define STRING_CHAR_ADVANCE(p)					\
  (!((p)[0] & 0x80)						\
   ? *(p)++							\
   : ! ((p)[0] & 0x20)						\
   ? ((p) += 2,							\
      ((((p)[-2] & 0x1F) << 6)					\
       | ((p)[-1] & 0x3F)					\
       | ((unsigned char) ((p)[-2]) < 0xC2 ? 0x3FFF80 : 0)))	\
   : ! ((p)[0] & 0x10)						\
   ? ((p) += 3,							\
      ((((p)[-3] & 0x0F) << 12)					\
       | (((p)[-2] & 0x3F) << 6)				\
       | ((p)[-1] & 0x3F)))					\
   : string_char ((p), &(p), NULL))


/* Fetch the "next" character from Lisp string STRING at byte position
   BYTEIDX, character position CHARIDX.  Store it into OUTPUT.

   All the args must be side-effect-free.
   BYTEIDX and CHARIDX must be lvalues;
   we increment them past the character fetched.  */

#define FETCH_STRING_CHAR_ADVANCE(OUTPUT, STRING, CHARIDX, BYTEIDX)	\
  do                                                                    \
    {									\
      CHARIDX++;							\
      if (STRING_MULTIBYTE (STRING))					\
	{								\
	  unsigned char *chp = &SDATA (STRING)[BYTEIDX];		\
	  int chlen;							\
									\
	  OUTPUT = STRING_CHAR_AND_LENGTH (chp, chlen);			\
	  BYTEIDX += chlen;						\
	}								\
      else								\
	{								\
	  OUTPUT = SREF (STRING, BYTEIDX);				\
	  BYTEIDX++;							\
	}								\
    }									\
  while (0)

/* Like FETCH_STRING_CHAR_ADVANCE, but return a multibyte character
   even if STRING is unibyte.  */

#define FETCH_STRING_CHAR_AS_MULTIBYTE_ADVANCE(OUTPUT, STRING, CHARIDX, BYTEIDX) \
  do                                                                          \
    {									      \
      CHARIDX++;							      \
      if (STRING_MULTIBYTE (STRING))					      \
	{								      \
	  unsigned char *chp = &SDATA (STRING)[BYTEIDX];		      \
	  int chlen;							      \
									      \
	  OUTPUT = STRING_CHAR_AND_LENGTH (chp, chlen);			      \
	  BYTEIDX += chlen;						      \
	}								      \
      else								      \
	{								      \
	  OUTPUT = SREF (STRING, BYTEIDX);				      \
	  BYTEIDX++;							      \
	  MAKE_CHAR_MULTIBYTE (OUTPUT);					      \
	}								      \
    }									      \
  while (0)


/* Like FETCH_STRING_CHAR_ADVANCE, but assumes STRING is multibyte.  */

#define FETCH_STRING_CHAR_ADVANCE_NO_CHECK(OUTPUT, STRING, CHARIDX, BYTEIDX) \
  do    								     \
    {									     \
      unsigned char *fetch_ptr = &SDATA (STRING)[BYTEIDX];		     \
      int fetch_len;							     \
									     \
      OUTPUT = STRING_CHAR_AND_LENGTH (fetch_ptr, fetch_len);		     \
      BYTEIDX += fetch_len;						     \
      CHARIDX++;							     \
    }									     \
  while (0)


/* Like FETCH_STRING_CHAR_ADVANCE, but fetch character from the current
   buffer.  */

#define FETCH_CHAR_ADVANCE(OUTPUT, CHARIDX, BYTEIDX)		\
  do    							\
    {								\
      CHARIDX++;						\
      if (!NILP (BVAR (current_buffer, enable_multibyte_characters)))	\
	{							\
	  unsigned char *chp = BYTE_POS_ADDR (BYTEIDX);		\
	  int chlen;						\
								\
	  OUTPUT= STRING_CHAR_AND_LENGTH (chp, chlen);		\
	  BYTEIDX += chlen;					\
	}							\
      else							\
	{							\
	  OUTPUT = *(BYTE_POS_ADDR (BYTEIDX));			\
	  BYTEIDX++;						\
	}							\
    }								\
  while (0)


/* Like FETCH_CHAR_ADVANCE, but assumes the current buffer is multibyte.  */

#define FETCH_CHAR_ADVANCE_NO_CHECK(OUTPUT, CHARIDX, BYTEIDX)	\
  do    							\
    {								\
      unsigned char *chp = BYTE_POS_ADDR (BYTEIDX);		\
      int chlen;							\
								\
      OUTPUT = STRING_CHAR_AND_LENGTH (chp, chlen);		\
      BYTEIDX += chlen;						\
      CHARIDX++;						\
    }								\
  while (0)


/* Increment the buffer byte position POS_BYTE of the current buffer to
   the next character boundary.  No range checking of POS.  */

#define INC_POS(pos_byte)				\
  do {							\
    unsigned char *chp = BYTE_POS_ADDR (pos_byte);	\
    pos_byte += BYTES_BY_CHAR_HEAD (*chp);		\
  } while (0)


/* Decrement the buffer byte position POS_BYTE of the current buffer to
   the previous character boundary.  No range checking of POS.  */

#define DEC_POS(pos_byte)			\
  do {						\
    unsigned char *chp;				\
    						\
    pos_byte--;					\
    if (pos_byte < GPT_BYTE)			\
      chp = BEG_ADDR + pos_byte - BEG_BYTE;	\
    else					\
      chp = BEG_ADDR + GAP_SIZE + pos_byte - BEG_BYTE; \
    while (!CHAR_HEAD_P (*chp))			\
      {						\
	chp--;					\
	pos_byte--;				\
      }						\
  } while (0)

/* Increment both CHARPOS and BYTEPOS, each in the appropriate way.  */

#define INC_BOTH(charpos, bytepos)				\
  do								\
    {								\
      (charpos)++;						\
      if (NILP (BVAR (current_buffer, enable_multibyte_characters)))	\
	(bytepos)++;						\
      else							\
	INC_POS ((bytepos));					\
    }								\
  while (0)


/* Decrement both CHARPOS and BYTEPOS, each in the appropriate way.  */

#define DEC_BOTH(charpos, bytepos)				\
  do								\
    {								\
      (charpos)--;						\
      if (NILP (BVAR (current_buffer, enable_multibyte_characters)))	\
	(bytepos)--;						\
      else							\
	DEC_POS ((bytepos));					\
    }								\
  while (0)


/* Increment the buffer byte position POS_BYTE of the current buffer to
   the next character boundary.  This macro relies on the fact that
   *GPT_ADDR and *Z_ADDR are always accessible and the values are
   '\0'.  No range checking of POS_BYTE.  */

#define BUF_INC_POS(buf, pos_byte)				\
  do {								\
    unsigned char *chp = BUF_BYTE_ADDRESS (buf, pos_byte);	\
    pos_byte += BYTES_BY_CHAR_HEAD (*chp);			\
  } while (0)


/* Decrement the buffer byte position POS_BYTE of the current buffer to
   the previous character boundary.  No range checking of POS_BYTE.  */

#define BUF_DEC_POS(buf, pos_byte)					\
  do {									\
    unsigned char *chp;							\
    pos_byte--;								\
    if (pos_byte < BUF_GPT_BYTE (buf))					\
      chp = BUF_BEG_ADDR (buf) + pos_byte - BEG_BYTE;			\
    else								\
      chp = BUF_BEG_ADDR (buf) + BUF_GAP_SIZE (buf) + pos_byte - BEG_BYTE;\
    while (!CHAR_HEAD_P (*chp))						\
      {									\
	chp--;								\
	pos_byte--;							\
      }									\
  } while (0)


/* If C is a character to be unified with a Unicode character, return
   the unified Unicode character.  */

#define MAYBE_UNIFY_CHAR(c)				\
  do {							\
    if (c > MAX_UNICODE_CHAR && c <= MAX_5_BYTE_CHAR)	\
      {							\
	Lisp_Object val;				\
	val = CHAR_TABLE_REF (Vchar_unify_table, c);	\
	if (INTEGERP (val))				\
	  c = XFASTINT (val);				\
	else if (! NILP (val))				\
	  c = maybe_unify_char (c, val);		\
      }							\
  } while (0)


/* Return a non-outlandish value for the tab width.  */

#define SANE_TAB_WIDTH(buf) \
  sanitize_tab_width (XFASTINT (BVAR (buf, tab_width)))
static inline int
sanitize_tab_width (EMACS_INT width)
{
  return 0 < width && width <= 1000 ? width : 8;
}

/* Return the width of ASCII character C.  The width is measured by
   how many columns C will occupy on the screen when displayed in the
   current buffer.  */

#define ASCII_CHAR_WIDTH(c)						\
  (c < 0x20								\
   ? (c == '\t'								\
      ? SANE_TAB_WIDTH (current_buffer)					\
      : (c == '\n' ? 0 : (NILP (BVAR (current_buffer, ctl_arrow)) ? 4 : 2)))	\
   : (c < 0x7f								\
      ? 1								\
      : ((NILP (BVAR (current_buffer, ctl_arrow)) ? 4 : 2))))

/* Return a non-outlandish value for a character width.  */

static inline int
sanitize_char_width (EMACS_INT width)
{
  return 0 <= width && width <= 1000 ? width : 1000;
}

/* Return the width of character C.  The width is measured by how many
   columns C will occupy on the screen when displayed in the current
   buffer.  */

#define CHAR_WIDTH(c)		\
  (ASCII_CHAR_P (c)		\
   ? ASCII_CHAR_WIDTH (c)	\
   : sanitize_char_width (XINT (CHAR_TABLE_REF (Vchar_width_table, c))))

/* If C is a variation selector, return the index of the
   variation selector (1..256).  Otherwise, return 0.  */

#define CHAR_VARIATION_SELECTOR_P(c)		\
  ((c) < 0xFE00 ? 0				\
   : (c) <= 0xFE0F ? (c) - 0xFE00 + 1		\
   : (c) < 0xE0100 ? 0				\
   : (c) <= 0xE01EF ? (c) - 0xE0100 + 17	\
   : 0)

/* If C is a high surrogate, return 1.  If C is a low surrogate,
   return 0.  Otherwise, return 0.  */

#define CHAR_SURROGATE_PAIR_P(c)	\
  ((c) < 0xD800 ? 0			\
   : (c) <= 0xDBFF ? 1			\
   : (c) <= 0xDFFF ? 2			\
   : 0)

/* Data type for Unicode general category.

   The order of members must be in sync with the 8th element of the
   member of unidata-prop-alist (in admin/unidata/unidata-getn.el) for
   Unicode character property `general-category'.  */

typedef enum {
  UNICODE_CATEGORY_UNKNOWN = 0,
  UNICODE_CATEGORY_Lu,
  UNICODE_CATEGORY_Ll,
  UNICODE_CATEGORY_Lt,
  UNICODE_CATEGORY_Lm,
  UNICODE_CATEGORY_Lo,
  UNICODE_CATEGORY_Mn,
  UNICODE_CATEGORY_Mc,
  UNICODE_CATEGORY_Me,
  UNICODE_CATEGORY_Nd,
  UNICODE_CATEGORY_Nl,
  UNICODE_CATEGORY_No,
  UNICODE_CATEGORY_Pc,
  UNICODE_CATEGORY_Pd,
  UNICODE_CATEGORY_Ps,
  UNICODE_CATEGORY_Pe,
  UNICODE_CATEGORY_Pi,
  UNICODE_CATEGORY_Pf,
  UNICODE_CATEGORY_Po,
  UNICODE_CATEGORY_Sm,
  UNICODE_CATEGORY_Sc,
  UNICODE_CATEGORY_Sk,
  UNICODE_CATEGORY_So,
  UNICODE_CATEGORY_Zs,
  UNICODE_CATEGORY_Zl,
  UNICODE_CATEGORY_Zp,
  UNICODE_CATEGORY_Cc,
  UNICODE_CATEGORY_Cf,
  UNICODE_CATEGORY_Cs,
  UNICODE_CATEGORY_Co,
  UNICODE_CATEGORY_Cn
} unicode_category_t;

extern int char_resolve_modifier_mask (int);
extern int char_string (unsigned, unsigned char *);
extern int string_char (const unsigned char *,
                        const unsigned char **, int *);

extern int translate_char (Lisp_Object, int c);
extern int char_printable_p (int c);
extern void parse_str_as_multibyte (const unsigned char *,
				    EMACS_INT, EMACS_INT *, EMACS_INT *);
extern EMACS_INT count_size_as_multibyte (const unsigned char *, EMACS_INT);
extern EMACS_INT str_as_multibyte (unsigned char *, EMACS_INT, EMACS_INT,
			     EMACS_INT *);
extern EMACS_INT str_to_multibyte (unsigned char *, EMACS_INT, EMACS_INT);
extern EMACS_INT str_as_unibyte (unsigned char *, EMACS_INT);
extern EMACS_INT str_to_unibyte (const unsigned char *, unsigned char *,
                                 EMACS_INT, int);
extern EMACS_INT strwidth (const char *, EMACS_INT);
extern EMACS_INT c_string_width (const unsigned char *, EMACS_INT, int,
				 EMACS_INT *, EMACS_INT *);
extern EMACS_INT lisp_string_width (Lisp_Object, EMACS_INT,
				    EMACS_INT *, EMACS_INT *);

extern Lisp_Object Qcharacterp;
extern Lisp_Object Vchar_unify_table;
extern Lisp_Object string_escape_byte8 (Lisp_Object);

/* Return a translation table of id number ID.  */
#define GET_TRANSLATION_TABLE(id) \
  (XCDR(XVECTOR(Vtranslation_table_vector)->contents[(id)]))

#endif /* EMACS_CHARACTER_H */
