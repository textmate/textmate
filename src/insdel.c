/* Buffer insertion/deletion and gap motion for GNU Emacs.
   Copyright (C) 1985-1986, 1993-1995, 1997-2012
                 Free Software Foundation, Inc.

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


#include <config.h>
#include <setjmp.h>

#include <intprops.h>

#include "lisp.h"
#include "intervals.h"
#include "buffer.h"
#include "character.h"
#include "window.h"
#include "blockinput.h"
#include "region-cache.h"

#ifndef NULL
#define NULL 0
#endif

static void insert_from_string_1 (Lisp_Object string,
				  EMACS_INT pos, EMACS_INT pos_byte,
				  EMACS_INT nchars, EMACS_INT nbytes,
				  int inherit, int before_markers);
static void insert_from_buffer_1 (struct buffer *buf,
				  EMACS_INT from, EMACS_INT nchars,
				  int inherit);
static void gap_left (EMACS_INT charpos, EMACS_INT bytepos, int newgap);
static void gap_right (EMACS_INT charpos, EMACS_INT bytepos);

static Lisp_Object Fcombine_after_change_execute (void);

/* List of elements of the form (BEG-UNCHANGED END-UNCHANGED CHANGE-AMOUNT)
   describing changes which happened while combine_after_change_calls
   was nonzero.  We use this to decide how to call them
   once the deferral ends.

   In each element.
   BEG-UNCHANGED is the number of chars before the changed range.
   END-UNCHANGED is the number of chars after the changed range,
   and CHANGE-AMOUNT is the number of characters inserted by the change
   (negative for a deletion).  */
static Lisp_Object combine_after_change_list;

/* Buffer which combine_after_change_list is about.  */
static Lisp_Object combine_after_change_buffer;

Lisp_Object Qinhibit_modification_hooks;

static void signal_before_change (EMACS_INT, EMACS_INT, EMACS_INT *);

#define CHECK_MARKERS()				\
  do						\
    {						\
      if (check_markers_debug_flag)		\
	check_markers ();			\
    }						\
  while (0)

static void
check_markers (void)
{
  register struct Lisp_Marker *tail;
  int multibyte = ! NILP (BVAR (current_buffer, enable_multibyte_characters));

  for (tail = BUF_MARKERS (current_buffer); tail; tail = tail->next)
    {
      if (tail->buffer->text != current_buffer->text)
	abort ();
      if (tail->charpos > Z)
	abort ();
      if (tail->bytepos > Z_BYTE)
	abort ();
      if (multibyte && ! CHAR_HEAD_P (FETCH_BYTE (tail->bytepos)))
	abort ();
    }
}

/* Move gap to position CHARPOS.
   Note that this can quit!  */

void
move_gap (EMACS_INT charpos)
{
  move_gap_both (charpos, charpos_to_bytepos (charpos));
}

/* Move gap to byte position BYTEPOS, which is also char position CHARPOS.
   Note that this can quit!  */

void
move_gap_both (EMACS_INT charpos, EMACS_INT bytepos)
{
  if (bytepos < GPT_BYTE)
    gap_left (charpos, bytepos, 0);
  else if (bytepos > GPT_BYTE)
    gap_right (charpos, bytepos);
}

/* Move the gap to a position less than the current GPT.
   BYTEPOS describes the new position as a byte position,
   and CHARPOS is the corresponding char position.
   If NEWGAP is nonzero, then don't update beg_unchanged and end_unchanged.  */

static void
gap_left (EMACS_INT charpos, EMACS_INT bytepos, int newgap)
{
  register unsigned char *to, *from;
  register EMACS_INT i;
  EMACS_INT new_s1;

  if (!newgap)
    BUF_COMPUTE_UNCHANGED (current_buffer, charpos, GPT);

  i = GPT_BYTE;
  to = GAP_END_ADDR;
  from = GPT_ADDR;
  new_s1 = GPT_BYTE;

  /* Now copy the characters.  To move the gap down,
     copy characters up.  */

  while (1)
    {
      /* I gets number of characters left to copy.  */
      i = new_s1 - bytepos;
      if (i == 0)
	break;
      /* If a quit is requested, stop copying now.
	 Change BYTEPOS to be where we have actually moved the gap to.  */
      if (QUITP)
	{
	  bytepos = new_s1;
	  charpos = BYTE_TO_CHAR (bytepos);
	  break;
	}
      /* Move at most 32000 chars before checking again for a quit.  */
      if (i > 32000)
	i = 32000;
      new_s1 -= i;
      from -= i, to -= i;
      memmove (to, from, i);
    }

  /* Adjust buffer data structure, to put the gap at BYTEPOS.
     BYTEPOS is where the loop above stopped, which may be what
     was specified or may be where a quit was detected.  */
  GPT_BYTE = bytepos;
  GPT = charpos;
  if (bytepos < charpos)
    abort ();
  if (GAP_SIZE > 0) *(GPT_ADDR) = 0; /* Put an anchor.  */
  QUIT;
}

/* Move the gap to a position greater than the current GPT.
   BYTEPOS describes the new position as a byte position,
   and CHARPOS is the corresponding char position.  */

static void
gap_right (EMACS_INT charpos, EMACS_INT bytepos)
{
  register unsigned char *to, *from;
  register EMACS_INT i;
  EMACS_INT new_s1;

  BUF_COMPUTE_UNCHANGED (current_buffer, charpos, GPT);

  i = GPT_BYTE;
  from = GAP_END_ADDR;
  to = GPT_ADDR;
  new_s1 = GPT_BYTE;

  /* Now copy the characters.  To move the gap up,
     copy characters down.  */

  while (1)
    {
      /* I gets number of characters left to copy.  */
      i = bytepos - new_s1;
      if (i == 0)
	break;
      /* If a quit is requested, stop copying now.
	 Change BYTEPOS to be where we have actually moved the gap to.  */
      if (QUITP)
	{
	  bytepos = new_s1;
	  charpos = BYTE_TO_CHAR (bytepos);
	  break;
	}
      /* Move at most 32000 chars before checking again for a quit.  */
      if (i > 32000)
	i = 32000;
      new_s1 += i;
      memmove (to, from, i);
      from += i, to += i;
    }

  GPT = charpos;
  GPT_BYTE = bytepos;
  if (bytepos < charpos)
    abort ();
  if (GAP_SIZE > 0) *(GPT_ADDR) = 0; /* Put an anchor.  */
  QUIT;
}

/* Adjust all markers for a deletion
   whose range in bytes is FROM_BYTE to TO_BYTE.
   The range in charpos is FROM to TO.

   This function assumes that the gap is adjacent to
   or inside of the range being deleted.  */

void
adjust_markers_for_delete (EMACS_INT from, EMACS_INT from_byte,
			   EMACS_INT to, EMACS_INT to_byte)
{
  Lisp_Object marker;
  register struct Lisp_Marker *m;
  register EMACS_INT charpos;

  for (m = BUF_MARKERS (current_buffer); m; m = m->next)
    {
      charpos = m->charpos;

      if (charpos > Z)
	abort ();

      /* If the marker is after the deletion,
	 relocate by number of chars / bytes deleted.  */
      if (charpos > to)
	{
	  m->charpos -= to - from;
	  m->bytepos -= to_byte - from_byte;
	}
      /* Here's the case where a marker is inside text being deleted.  */
      else if (charpos > from)
	{
	  if (! m->insertion_type)
	    { /* Normal markers will end up at the beginning of the
	       re-inserted text after undoing a deletion, and must be
	       adjusted to move them to the correct place.  */
	      XSETMISC (marker, m);
	      record_marker_adjustment (marker, from - charpos);
	    }
	  else if (charpos < to)
	    { /* Before-insertion markers will automatically move forward
	       upon re-inserting the deleted text, so we have to arrange
	       for them to move backward to the correct position.  */
	      XSETMISC (marker, m);
	      record_marker_adjustment (marker, to - charpos);
	    }
	  m->charpos = from;
	  m->bytepos = from_byte;
	}
      /* Here's the case where a before-insertion marker is immediately
	 before the deleted region.  */
      else if (charpos == from && m->insertion_type)
	{
	  /* Undoing the change uses normal insertion, which will
	     incorrectly make MARKER move forward, so we arrange for it
	     to then move backward to the correct place at the beginning
	     of the deleted region.  */
	  XSETMISC (marker, m);
	  record_marker_adjustment (marker, to - from);
	}
    }
}


/* Adjust markers for an insertion that stretches from FROM / FROM_BYTE
   to TO / TO_BYTE.  We have to relocate the charpos of every marker
   that points after the insertion (but not their bytepos).

   When a marker points at the insertion point,
   we advance it if either its insertion-type is t
   or BEFORE_MARKERS is true.  */

static void
adjust_markers_for_insert (EMACS_INT from, EMACS_INT from_byte,
			   EMACS_INT to, EMACS_INT to_byte, int before_markers)
{
  struct Lisp_Marker *m;
  int adjusted = 0;
  EMACS_INT nchars = to - from;
  EMACS_INT nbytes = to_byte - from_byte;

  for (m = BUF_MARKERS (current_buffer); m; m = m->next)
    {
      eassert (m->bytepos >= m->charpos
	       && m->bytepos - m->charpos <= Z_BYTE - Z);

      if (m->bytepos == from_byte)
	{
	  if (m->insertion_type || before_markers)
	    {
	      m->bytepos = to_byte;
	      m->charpos = to;
	      if (m->insertion_type)
		adjusted = 1;
	    }
	}
      else if (m->bytepos > from_byte)
	{
	  m->bytepos += nbytes;
	  m->charpos += nchars;
	}
    }

  /* Adjusting only markers whose insertion-type is t may result in
     - disordered start and end in overlays, and
     - disordered overlays in the slot `overlays_before' of current_buffer.  */
  if (adjusted)
    {
      fix_start_end_in_overlays (from, to);
      fix_overlays_before (current_buffer, from, to);
    }
}

/* Adjust point for an insertion of NBYTES bytes, which are NCHARS characters.

   This is used only when the value of point changes due to an insert
   or delete; it does not represent a conceptual change in point as a
   marker.  In particular, point is not crossing any interval
   boundaries, so there's no need to use the usual SET_PT macro.  In
   fact it would be incorrect to do so, because either the old or the
   new value of point is out of sync with the current set of
   intervals.  */

static void
adjust_point (EMACS_INT nchars, EMACS_INT nbytes)
{
  SET_BUF_PT_BOTH (current_buffer, PT + nchars, PT_BYTE + nbytes);
  /* In a single-byte buffer, the two positions must be equal.  */
  eassert (PT_BYTE >= PT && PT_BYTE - PT <= ZV_BYTE - ZV);
}

/* Adjust markers for a replacement of a text at FROM (FROM_BYTE) of
   length OLD_CHARS (OLD_BYTES) to a new text of length NEW_CHARS
   (NEW_BYTES).  It is assumed that OLD_CHARS > 0, i.e., this is not
   an insertion.  */

static void
adjust_markers_for_replace (EMACS_INT from, EMACS_INT from_byte,
			    EMACS_INT old_chars, EMACS_INT old_bytes,
			    EMACS_INT new_chars, EMACS_INT new_bytes)
{
  register struct Lisp_Marker *m;
  EMACS_INT prev_to_byte = from_byte + old_bytes;
  EMACS_INT diff_chars = new_chars - old_chars;
  EMACS_INT diff_bytes = new_bytes - old_bytes;

  for (m = BUF_MARKERS (current_buffer); m; m = m->next)
    {
      if (m->bytepos >= prev_to_byte)
	{
	  m->charpos += diff_chars;
	  m->bytepos += diff_bytes;
	}
      else if (m->bytepos > from_byte)
	{
	  m->charpos = from;
	  m->bytepos = from_byte;
	}
    }

  CHECK_MARKERS ();
}


void
buffer_overflow (void)
{
  error ("Maximum buffer size exceeded");
}

/* Make the gap NBYTES_ADDED bytes longer.  */

static void
make_gap_larger (EMACS_INT nbytes_added)
{
  Lisp_Object tem;
  EMACS_INT real_gap_loc;
  EMACS_INT real_gap_loc_byte;
  EMACS_INT old_gap_size;
  EMACS_INT current_size = Z_BYTE - BEG_BYTE + GAP_SIZE;
  enum { enough_for_a_while = 2000 };

  if (BUF_BYTES_MAX - current_size < nbytes_added)
    buffer_overflow ();

  /* If we have to get more space, get enough to last a while;
     but do not exceed the maximum buffer size.  */
  nbytes_added = min (nbytes_added + enough_for_a_while,
		      BUF_BYTES_MAX - current_size);

  enlarge_buffer_text (current_buffer, nbytes_added);

  /* Prevent quitting in move_gap.  */
  tem = Vinhibit_quit;
  Vinhibit_quit = Qt;

  real_gap_loc = GPT;
  real_gap_loc_byte = GPT_BYTE;
  old_gap_size = GAP_SIZE;

  /* Call the newly allocated space a gap at the end of the whole space.  */
  GPT = Z + GAP_SIZE;
  GPT_BYTE = Z_BYTE + GAP_SIZE;
  GAP_SIZE = nbytes_added;

  /* Move the new gap down to be consecutive with the end of the old one.
     This adjusts the markers properly too.  */
  gap_left (real_gap_loc + old_gap_size, real_gap_loc_byte + old_gap_size, 1);

  /* Now combine the two into one large gap.  */
  GAP_SIZE += old_gap_size;
  GPT = real_gap_loc;
  GPT_BYTE = real_gap_loc_byte;

  /* Put an anchor.  */
  *(Z_ADDR) = 0;

  Vinhibit_quit = tem;
}

#if defined USE_MMAP_FOR_BUFFERS || defined REL_ALLOC || defined DOUG_LEA_MALLOC

/* Make the gap NBYTES_REMOVED bytes shorter.  */

static void
make_gap_smaller (EMACS_INT nbytes_removed)
{
  Lisp_Object tem;
  EMACS_INT real_gap_loc;
  EMACS_INT real_gap_loc_byte;
  EMACS_INT real_Z;
  EMACS_INT real_Z_byte;
  EMACS_INT real_beg_unchanged;
  EMACS_INT new_gap_size;

  /* Make sure the gap is at least 20 bytes.  */
  if (GAP_SIZE - nbytes_removed < 20)
    nbytes_removed = GAP_SIZE - 20;

  /* Prevent quitting in move_gap.  */
  tem = Vinhibit_quit;
  Vinhibit_quit = Qt;

  real_gap_loc = GPT;
  real_gap_loc_byte = GPT_BYTE;
  new_gap_size = GAP_SIZE - nbytes_removed;
  real_Z = Z;
  real_Z_byte = Z_BYTE;
  real_beg_unchanged = BEG_UNCHANGED;

  /* Pretend that the last unwanted part of the gap is the entire gap,
     and that the first desired part of the gap is part of the buffer
     text.  */
  memset (GPT_ADDR, 0, new_gap_size);
  GPT += new_gap_size;
  GPT_BYTE += new_gap_size;
  Z += new_gap_size;
  Z_BYTE += new_gap_size;
  GAP_SIZE = nbytes_removed;

  /* Move the unwanted pretend gap to the end of the buffer.  This
     adjusts the markers properly too.  */
  gap_right (Z, Z_BYTE);

  enlarge_buffer_text (current_buffer, -nbytes_removed);

  /* Now restore the desired gap.  */
  GAP_SIZE = new_gap_size;
  GPT = real_gap_loc;
  GPT_BYTE = real_gap_loc_byte;
  Z = real_Z;
  Z_BYTE = real_Z_byte;
  BEG_UNCHANGED = real_beg_unchanged;

  /* Put an anchor.  */
  *(Z_ADDR) = 0;

  Vinhibit_quit = tem;
}

#endif /* USE_MMAP_FOR_BUFFERS || REL_ALLOC || DOUG_LEA_MALLOC */

void
make_gap (EMACS_INT nbytes_added)
{
  if (nbytes_added >= 0)
    make_gap_larger (nbytes_added);
#if defined USE_MMAP_FOR_BUFFERS || defined REL_ALLOC || defined DOUG_LEA_MALLOC
  else
    make_gap_smaller (-nbytes_added);
#endif
}

/* Copy NBYTES bytes of text from FROM_ADDR to TO_ADDR.
   FROM_MULTIBYTE says whether the incoming text is multibyte.
   TO_MULTIBYTE says whether to store the text as multibyte.
   If FROM_MULTIBYTE != TO_MULTIBYTE, we convert.

   Return the number of bytes stored at TO_ADDR.  */

EMACS_INT
copy_text (const unsigned char *from_addr, unsigned char *to_addr,
	   EMACS_INT nbytes, int from_multibyte, int to_multibyte)
{
  if (from_multibyte == to_multibyte)
    {
      memcpy (to_addr, from_addr, nbytes);
      return nbytes;
    }
  else if (from_multibyte)
    {
      EMACS_INT nchars = 0;
      EMACS_INT bytes_left = nbytes;

      while (bytes_left > 0)
	{
	  int thislen, c;
	  c = STRING_CHAR_AND_LENGTH (from_addr, thislen);
	  if (! ASCII_CHAR_P (c))
	    c &= 0xFF;
	  *to_addr++ = c;
	  from_addr += thislen;
	  bytes_left -= thislen;
	  nchars++;
	}
      return nchars;
    }
  else
    {
      unsigned char *initial_to_addr = to_addr;

      /* Convert single-byte to multibyte.  */
      while (nbytes > 0)
	{
	  int c = *from_addr++;

	  if (!ASCII_CHAR_P (c))
	    {
	      c = BYTE8_TO_CHAR (c);
	      to_addr += CHAR_STRING (c, to_addr);
	      nbytes--;
	    }
	  else
	    /* Special case for speed.  */
	    *to_addr++ = c, nbytes--;
	}
      return to_addr - initial_to_addr;
    }
}

/* Insert a string of specified length before point.
   This function judges multibyteness based on
   enable_multibyte_characters in the current buffer;
   it never converts between single-byte and multibyte.

   DO NOT use this for the contents of a Lisp string or a Lisp buffer!
   prepare_to_modify_buffer could relocate the text.  */

void
insert (const char *string, EMACS_INT nbytes)
{
  if (nbytes > 0)
    {
      EMACS_INT len = chars_in_text ((unsigned char *) string, nbytes), opoint;
      insert_1_both (string, len, nbytes, 0, 1, 0);
      opoint = PT - len;
      signal_after_change (opoint, 0, len);
      update_compositions (opoint, PT, CHECK_BORDER);
    }
}

/* Likewise, but inherit text properties from neighboring characters.  */

void
insert_and_inherit (const char *string, EMACS_INT nbytes)
{
  if (nbytes > 0)
    {
      EMACS_INT len = chars_in_text ((unsigned char *) string, nbytes), opoint;
      insert_1_both (string, len, nbytes, 1, 1, 0);
      opoint = PT - len;
      signal_after_change (opoint, 0, len);
      update_compositions (opoint, PT, CHECK_BORDER);
    }
}

/* Insert the character C before point.  Do not inherit text properties.  */

void
insert_char (int c)
{
  unsigned char str[MAX_MULTIBYTE_LENGTH];
  int len;

  if (! NILP (BVAR (current_buffer, enable_multibyte_characters)))
    len = CHAR_STRING (c, str);
  else
    {
      len = 1;
      str[0] = c;
    }

  insert ((char *) str, len);
}

/* Insert the null-terminated string S before point.  */

void
insert_string (const char *s)
{
  insert (s, strlen (s));
}

/* Like `insert' except that all markers pointing at the place where
   the insertion happens are adjusted to point after it.
   Don't use this function to insert part of a Lisp string,
   since gc could happen and relocate it.  */

void
insert_before_markers (const char *string, EMACS_INT nbytes)
{
  if (nbytes > 0)
    {
      EMACS_INT len = chars_in_text ((unsigned char *) string, nbytes), opoint;
      insert_1_both (string, len, nbytes, 0, 1, 1);
      opoint = PT - len;
      signal_after_change (opoint, 0, len);
      update_compositions (opoint, PT, CHECK_BORDER);
    }
}

/* Likewise, but inherit text properties from neighboring characters.  */

void
insert_before_markers_and_inherit (const char *string,
				   EMACS_INT nbytes)
{
  if (nbytes > 0)
    {
      EMACS_INT len = chars_in_text ((unsigned char *) string, nbytes), opoint;
      insert_1_both (string, len, nbytes, 1, 1, 1);
      opoint = PT - len;
      signal_after_change (opoint, 0, len);
      update_compositions (opoint, PT, CHECK_BORDER);
    }
}

/* Subroutine used by the insert functions above.  */

void
insert_1 (const char *string, EMACS_INT nbytes,
	  int inherit, int prepare, int before_markers)
{
  insert_1_both (string, chars_in_text ((unsigned char *) string, nbytes),
		 nbytes, inherit, prepare, before_markers);
}


#ifdef BYTE_COMBINING_DEBUG

/* See if the bytes before POS/POS_BYTE combine with bytes
   at the start of STRING to form a single character.
   If so, return the number of bytes at the start of STRING
   which combine in this way.  Otherwise, return 0.  */

int
count_combining_before (const unsigned char *string, EMACS_INT length,
			EMACS_INT pos, EMACS_INT pos_byte)
{
  int len, combining_bytes;
  const unsigned char *p;

  if (NILP (current_buffer->enable_multibyte_characters))
    return 0;

  /* At first, we can exclude the following cases:
	(1) STRING[0] can't be a following byte of multibyte sequence.
	(2) POS is the start of the current buffer.
	(3) A character before POS is not a multibyte character.  */
  if (length == 0 || CHAR_HEAD_P (*string)) /* case (1) */
    return 0;
  if (pos_byte == BEG_BYTE)	/* case (2) */
    return 0;
  len = 1;
  p = BYTE_POS_ADDR (pos_byte - 1);
  while (! CHAR_HEAD_P (*p)) p--, len++;
  if (! LEADING_CODE_P (*p)) /* case (3) */
    return 0;

  combining_bytes = BYTES_BY_CHAR_HEAD (*p) - len;
  if (combining_bytes <= 0)
    /* The character preceding POS is, complete and no room for
       combining bytes (combining_bytes == 0), or an independent 8-bit
       character (combining_bytes < 0).  */
    return 0;

  /* We have a combination situation.  Count the bytes at STRING that
     may combine.  */
  p = string + 1;
  while (!CHAR_HEAD_P (*p) && p < string + length)
    p++;

  return (combining_bytes < p - string ? combining_bytes : p - string);
}

/* See if the bytes after POS/POS_BYTE combine with bytes
   at the end of STRING to form a single character.
   If so, return the number of bytes after POS/POS_BYTE
   which combine in this way.  Otherwise, return 0.  */

int
count_combining_after (const unsigned char *string,
		       EMACS_INT length, EMACS_INT pos, EMACS_INT pos_byte)
{
  EMACS_INT opos_byte = pos_byte;
  EMACS_INT i;
  EMACS_INT bytes;
  unsigned char *bufp;

  if (NILP (current_buffer->enable_multibyte_characters))
    return 0;

  /* At first, we can exclude the following cases:
	(1) The last byte of STRING is an ASCII.
	(2) POS is the last of the current buffer.
	(3) A character at POS can't be a following byte of multibyte
	    character.  */
  if (length > 0 && ASCII_BYTE_P (string[length - 1])) /* case (1) */
    return 0;
  if (pos_byte == Z_BYTE)	/* case (2) */
    return 0;
  bufp = BYTE_POS_ADDR (pos_byte);
  if (CHAR_HEAD_P (*bufp))	/* case (3) */
    return 0;

  i = length - 1;
  while (i >= 0 && ! CHAR_HEAD_P (string[i]))
    {
      i--;
    }
  if (i < 0)
    {
      /* All characters in STRING are not character head.  We must
	 check also preceding bytes at POS.  We are sure that the gap
	 is at POS.  */
      unsigned char *p = BEG_ADDR;
      i = pos_byte - 2;
      while (i >= 0 && ! CHAR_HEAD_P (p[i]))
	i--;
      if (i < 0 || !LEADING_CODE_P (p[i]))
	return 0;

      bytes = BYTES_BY_CHAR_HEAD (p[i]);
      return (bytes <= pos_byte - 1 - i + length
	      ? 0
	      : bytes - (pos_byte - 1 - i + length));
    }
  if (!LEADING_CODE_P (string[i]))
    return 0;

  bytes = BYTES_BY_CHAR_HEAD (string[i]) - (length - i);
  bufp++, pos_byte++;
  while (!CHAR_HEAD_P (*bufp)) bufp++, pos_byte++;

  return (bytes <= pos_byte - opos_byte ? bytes : pos_byte - opos_byte);
}

#endif


/* Insert a sequence of NCHARS chars which occupy NBYTES bytes
   starting at STRING.  INHERIT, PREPARE and BEFORE_MARKERS
   are the same as in insert_1.  */

void
insert_1_both (const char *string,
	       EMACS_INT nchars, EMACS_INT nbytes,
	       int inherit, int prepare, int before_markers)
{
  if (nchars == 0)
    return;

  if (NILP (BVAR (current_buffer, enable_multibyte_characters)))
    nchars = nbytes;

  if (prepare)
    /* Do this before moving and increasing the gap,
       because the before-change hooks might move the gap
       or make it smaller.  */
    prepare_to_modify_buffer (PT, PT, NULL);

  if (PT != GPT)
    move_gap_both (PT, PT_BYTE);
  if (GAP_SIZE < nbytes)
    make_gap (nbytes - GAP_SIZE);

#ifdef BYTE_COMBINING_DEBUG
  if (count_combining_before (string, nbytes, PT, PT_BYTE)
      || count_combining_after (string, nbytes, PT, PT_BYTE))
    abort ();
#endif

  /* Record deletion of the surrounding text that combines with
     the insertion.  This, together with recording the insertion,
     will add up to the right stuff in the undo list.  */
  record_insert (PT, nchars);
  MODIFF++;
  CHARS_MODIFF = MODIFF;

  memcpy (GPT_ADDR, string, nbytes);

  GAP_SIZE -= nbytes;
  GPT += nchars;
  ZV += nchars;
  Z += nchars;
  GPT_BYTE += nbytes;
  ZV_BYTE += nbytes;
  Z_BYTE += nbytes;
  if (GAP_SIZE > 0) *(GPT_ADDR) = 0; /* Put an anchor.  */

  if (GPT_BYTE < GPT)
    abort ();

  /* The insert may have been in the unchanged region, so check again. */
  if (Z - GPT < END_UNCHANGED)
    END_UNCHANGED = Z - GPT;

  adjust_overlays_for_insert (PT, nchars);
  adjust_markers_for_insert (PT, PT_BYTE,
			     PT + nchars, PT_BYTE + nbytes,
			     before_markers);

  if (BUF_INTERVALS (current_buffer) != 0)
    offset_intervals (current_buffer, PT, nchars);

  if (!inherit && BUF_INTERVALS (current_buffer) != 0)
    set_text_properties (make_number (PT), make_number (PT + nchars),
			 Qnil, Qnil, Qnil);

  adjust_point (nchars, nbytes);

  CHECK_MARKERS ();
}

/* Insert the part of the text of STRING, a Lisp object assumed to be
   of type string, consisting of the LENGTH characters (LENGTH_BYTE bytes)
   starting at position POS / POS_BYTE.  If the text of STRING has properties,
   copy them into the buffer.

   It does not work to use `insert' for this, because a GC could happen
   before we copy the stuff into the buffer, and relocate the string
   without insert noticing.  */

void
insert_from_string (Lisp_Object string, EMACS_INT pos, EMACS_INT pos_byte,
		    EMACS_INT length, EMACS_INT length_byte, int inherit)
{
  EMACS_INT opoint = PT;

  if (SCHARS (string) == 0)
    return;

  insert_from_string_1 (string, pos, pos_byte, length, length_byte,
			inherit, 0);
  signal_after_change (opoint, 0, PT - opoint);
  update_compositions (opoint, PT, CHECK_BORDER);
}

/* Like `insert_from_string' except that all markers pointing
   at the place where the insertion happens are adjusted to point after it.  */

void
insert_from_string_before_markers (Lisp_Object string,
				   EMACS_INT pos, EMACS_INT pos_byte,
				   EMACS_INT length, EMACS_INT length_byte,
				   int inherit)
{
  EMACS_INT opoint = PT;

  if (SCHARS (string) == 0)
    return;

  insert_from_string_1 (string, pos, pos_byte, length, length_byte,
			inherit, 1);
  signal_after_change (opoint, 0, PT - opoint);
  update_compositions (opoint, PT, CHECK_BORDER);
}

/* Subroutine of the insertion functions above.  */

static void
insert_from_string_1 (Lisp_Object string, EMACS_INT pos, EMACS_INT pos_byte,
		      EMACS_INT nchars, EMACS_INT nbytes,
		      int inherit, int before_markers)
{
  struct gcpro gcpro1;
  EMACS_INT outgoing_nbytes = nbytes;
  INTERVAL intervals;

  /* Make OUTGOING_NBYTES describe the text
     as it will be inserted in this buffer.  */

  if (NILP (BVAR (current_buffer, enable_multibyte_characters)))
    outgoing_nbytes = nchars;
  else if (! STRING_MULTIBYTE (string))
    outgoing_nbytes
      = count_size_as_multibyte (SDATA (string) + pos_byte,
				 nbytes);

  GCPRO1 (string);
  /* Do this before moving and increasing the gap,
     because the before-change hooks might move the gap
     or make it smaller.  */
  prepare_to_modify_buffer (PT, PT, NULL);

  if (PT != GPT)
    move_gap_both (PT, PT_BYTE);
  if (GAP_SIZE < outgoing_nbytes)
    make_gap (outgoing_nbytes - GAP_SIZE);
  UNGCPRO;

  /* Copy the string text into the buffer, perhaps converting
     between single-byte and multibyte.  */
  copy_text (SDATA (string) + pos_byte, GPT_ADDR, nbytes,
	     STRING_MULTIBYTE (string),
	     ! NILP (BVAR (current_buffer, enable_multibyte_characters)));

#ifdef BYTE_COMBINING_DEBUG
  /* We have copied text into the gap, but we have not altered
     PT or PT_BYTE yet.  So we can pass PT and PT_BYTE
     to these functions and get the same results as we would
     have got earlier on.  Meanwhile, PT_ADDR does point to
     the text that has been stored by copy_text.  */
  if (count_combining_before (GPT_ADDR, outgoing_nbytes, PT, PT_BYTE)
      || count_combining_after (GPT_ADDR, outgoing_nbytes, PT, PT_BYTE))
    abort ();
#endif

  record_insert (PT, nchars);
  MODIFF++;
  CHARS_MODIFF = MODIFF;

  GAP_SIZE -= outgoing_nbytes;
  GPT += nchars;
  ZV += nchars;
  Z += nchars;
  GPT_BYTE += outgoing_nbytes;
  ZV_BYTE += outgoing_nbytes;
  Z_BYTE += outgoing_nbytes;
  if (GAP_SIZE > 0) *(GPT_ADDR) = 0; /* Put an anchor.  */

  if (GPT_BYTE < GPT)
    abort ();

  /* The insert may have been in the unchanged region, so check again. */
  if (Z - GPT < END_UNCHANGED)
    END_UNCHANGED = Z - GPT;

  adjust_overlays_for_insert (PT, nchars);
  adjust_markers_for_insert (PT, PT_BYTE, PT + nchars,
			     PT_BYTE + outgoing_nbytes,
			     before_markers);

  offset_intervals (current_buffer, PT, nchars);

  intervals = STRING_INTERVALS (string);
  /* Get the intervals for the part of the string we are inserting.  */
  if (nbytes < SBYTES (string))
    intervals = copy_intervals (intervals, pos, nchars);

  /* Insert those intervals.  */
  graft_intervals_into_buffer (intervals, PT, nchars,
			       current_buffer, inherit);

  adjust_point (nchars, outgoing_nbytes);

  CHECK_MARKERS ();
}

/* Insert a sequence of NCHARS chars which occupy NBYTES bytes
   starting at GPT_ADDR.  */

void
insert_from_gap (EMACS_INT nchars, EMACS_INT nbytes)
{
  if (NILP (BVAR (current_buffer, enable_multibyte_characters)))
    nchars = nbytes;

  record_insert (GPT, nchars);
  MODIFF++;

  GAP_SIZE -= nbytes;
  GPT += nchars;
  ZV += nchars;
  Z += nchars;
  GPT_BYTE += nbytes;
  ZV_BYTE += nbytes;
  Z_BYTE += nbytes;
  if (GAP_SIZE > 0) *(GPT_ADDR) = 0; /* Put an anchor.  */

  if (GPT_BYTE < GPT)
    abort ();

  adjust_overlays_for_insert (GPT - nchars, nchars);
  adjust_markers_for_insert (GPT - nchars, GPT_BYTE - nbytes,
			     GPT, GPT_BYTE, 0);

  if (BUF_INTERVALS (current_buffer) != 0)
    {
      offset_intervals (current_buffer, GPT - nchars, nchars);
      graft_intervals_into_buffer (NULL_INTERVAL, GPT - nchars, nchars,
				   current_buffer, 0);
    }

  if (GPT - nchars < PT)
    adjust_point (nchars, nbytes);

  CHECK_MARKERS ();
}

/* Insert text from BUF, NCHARS characters starting at CHARPOS, into the
   current buffer.  If the text in BUF has properties, they are absorbed
   into the current buffer.

   It does not work to use `insert' for this, because a malloc could happen
   and relocate BUF's text before the copy happens.  */

void
insert_from_buffer (struct buffer *buf,
		    EMACS_INT charpos, EMACS_INT nchars, int inherit)
{
  EMACS_INT opoint = PT;

  insert_from_buffer_1 (buf, charpos, nchars, inherit);
  signal_after_change (opoint, 0, PT - opoint);
  update_compositions (opoint, PT, CHECK_BORDER);
}

static void
insert_from_buffer_1 (struct buffer *buf,
		      EMACS_INT from, EMACS_INT nchars, int inherit)
{
  EMACS_INT chunk, chunk_expanded;
  EMACS_INT from_byte = buf_charpos_to_bytepos (buf, from);
  EMACS_INT to_byte = buf_charpos_to_bytepos (buf, from + nchars);
  EMACS_INT incoming_nbytes = to_byte - from_byte;
  EMACS_INT outgoing_nbytes = incoming_nbytes;
  INTERVAL intervals;

  /* Make OUTGOING_NBYTES describe the text
     as it will be inserted in this buffer.  */

  if (NILP (BVAR (current_buffer, enable_multibyte_characters)))
    outgoing_nbytes = nchars;
  else if (NILP (BVAR (buf, enable_multibyte_characters)))
    {
      EMACS_INT outgoing_before_gap = 0;
      EMACS_INT outgoing_after_gap = 0;

      if (from < BUF_GPT (buf))
	{
	  chunk =  BUF_GPT_BYTE (buf) - from_byte;
	  if (chunk > incoming_nbytes)
	    chunk = incoming_nbytes;
	  outgoing_before_gap
	    = count_size_as_multibyte (BUF_BYTE_ADDRESS (buf, from_byte),
				       chunk);
	}
      else
	chunk = 0;

      if (chunk < incoming_nbytes)
	outgoing_after_gap
	  = count_size_as_multibyte (BUF_BYTE_ADDRESS (buf,
						       from_byte + chunk),
				     incoming_nbytes - chunk);

      outgoing_nbytes = outgoing_before_gap + outgoing_after_gap;
    }

  /* Do this before moving and increasing the gap,
     because the before-change hooks might move the gap
     or make it smaller.  */
  prepare_to_modify_buffer (PT, PT, NULL);

  if (PT != GPT)
    move_gap_both (PT, PT_BYTE);
  if (GAP_SIZE < outgoing_nbytes)
    make_gap (outgoing_nbytes - GAP_SIZE);

  if (from < BUF_GPT (buf))
    {
      chunk = BUF_GPT_BYTE (buf) - from_byte;
      if (chunk > incoming_nbytes)
	chunk = incoming_nbytes;
      /* Record number of output bytes, so we know where
	 to put the output from the second copy_text.  */
      chunk_expanded
	= copy_text (BUF_BYTE_ADDRESS (buf, from_byte),
		     GPT_ADDR, chunk,
		     ! NILP (BVAR (buf, enable_multibyte_characters)),
		     ! NILP (BVAR (current_buffer, enable_multibyte_characters)));
    }
  else
    chunk_expanded = chunk = 0;

  if (chunk < incoming_nbytes)
    copy_text (BUF_BYTE_ADDRESS (buf, from_byte + chunk),
	       GPT_ADDR + chunk_expanded, incoming_nbytes - chunk,
	       ! NILP (BVAR (buf, enable_multibyte_characters)),
	       ! NILP (BVAR (current_buffer, enable_multibyte_characters)));

#ifdef BYTE_COMBINING_DEBUG
  /* We have copied text into the gap, but we have not altered
     PT or PT_BYTE yet.  So we can pass PT and PT_BYTE
     to these functions and get the same results as we would
     have got earlier on.  Meanwhile, GPT_ADDR does point to
     the text that has been stored by copy_text.  */
  if (count_combining_before (GPT_ADDR, outgoing_nbytes, PT, PT_BYTE)
      || count_combining_after (GPT_ADDR, outgoing_nbytes, PT, PT_BYTE))
    abort ();
#endif

  record_insert (PT, nchars);
  MODIFF++;
  CHARS_MODIFF = MODIFF;

  GAP_SIZE -= outgoing_nbytes;
  GPT += nchars;
  ZV += nchars;
  Z += nchars;
  GPT_BYTE += outgoing_nbytes;
  ZV_BYTE += outgoing_nbytes;
  Z_BYTE += outgoing_nbytes;
  if (GAP_SIZE > 0) *(GPT_ADDR) = 0; /* Put an anchor.  */

  if (GPT_BYTE < GPT)
    abort ();

  /* The insert may have been in the unchanged region, so check again. */
  if (Z - GPT < END_UNCHANGED)
    END_UNCHANGED = Z - GPT;

  adjust_overlays_for_insert (PT, nchars);
  adjust_markers_for_insert (PT, PT_BYTE, PT + nchars,
			     PT_BYTE + outgoing_nbytes,
			     0);

  if (BUF_INTERVALS (current_buffer) != 0)
    offset_intervals (current_buffer, PT, nchars);

  /* Get the intervals for the part of the string we are inserting.  */
  intervals = BUF_INTERVALS (buf);
  if (nchars < BUF_Z (buf) - BUF_BEG (buf))
    {
      if (buf == current_buffer && PT <= from)
	from += nchars;
      intervals = copy_intervals (intervals, from, nchars);
    }

  /* Insert those intervals.  */
  graft_intervals_into_buffer (intervals, PT, nchars, current_buffer, inherit);

  adjust_point (nchars, outgoing_nbytes);
}

/* Record undo information and adjust markers and position keepers for
   a replacement of a text PREV_TEXT at FROM to a new text of LEN
   chars (LEN_BYTE bytes) which resides in the gap just after
   GPT_ADDR.

   PREV_TEXT nil means the new text was just inserted.  */

static void
adjust_after_replace (EMACS_INT from, EMACS_INT from_byte,
		      Lisp_Object prev_text, EMACS_INT len, EMACS_INT len_byte)
{
  EMACS_INT nchars_del = 0, nbytes_del = 0;

#ifdef BYTE_COMBINING_DEBUG
  if (count_combining_before (GPT_ADDR, len_byte, from, from_byte)
      || count_combining_after (GPT_ADDR, len_byte, from, from_byte))
    abort ();
#endif

  if (STRINGP (prev_text))
    {
      nchars_del = SCHARS (prev_text);
      nbytes_del = SBYTES (prev_text);
    }

  /* Update various buffer positions for the new text.  */
  GAP_SIZE -= len_byte;
  ZV += len; Z+= len;
  ZV_BYTE += len_byte; Z_BYTE += len_byte;
  GPT += len; GPT_BYTE += len_byte;
  if (GAP_SIZE > 0) *(GPT_ADDR) = 0; /* Put an anchor. */

  if (nchars_del > 0)
    adjust_markers_for_replace (from, from_byte, nchars_del, nbytes_del,
				len, len_byte);
  else
    adjust_markers_for_insert (from, from_byte,
			       from + len, from_byte + len_byte, 0);

  if (! EQ (BVAR (current_buffer, undo_list), Qt))
    {
      if (nchars_del > 0)
	record_delete (from, prev_text);
      record_insert (from, len);
    }

  if (len > nchars_del)
    adjust_overlays_for_insert (from, len - nchars_del);
  else if (len < nchars_del)
    adjust_overlays_for_delete (from, nchars_del - len);
  if (BUF_INTERVALS (current_buffer) != 0)
    {
      offset_intervals (current_buffer, from, len - nchars_del);
    }

  if (from < PT)
    adjust_point (len - nchars_del, len_byte - nbytes_del);

  /* As byte combining will decrease Z, we must check this again. */
  if (Z - GPT < END_UNCHANGED)
    END_UNCHANGED = Z - GPT;

  CHECK_MARKERS ();

  if (len == 0)
    evaporate_overlays (from);
  MODIFF++;
  CHARS_MODIFF = MODIFF;
}

/* Record undo information, adjust markers and position keepers for an
   insertion of a text from FROM (FROM_BYTE) to TO (TO_BYTE).  The
   text already exists in the current buffer but character length (TO
   - FROM) may be incorrect, the correct length is NEWLEN.  */

void
adjust_after_insert (EMACS_INT from, EMACS_INT from_byte,
		     EMACS_INT to, EMACS_INT to_byte, EMACS_INT newlen)
{
  EMACS_INT len = to - from, len_byte = to_byte - from_byte;

  if (GPT != to)
    move_gap_both (to, to_byte);
  GAP_SIZE += len_byte;
  GPT -= len; GPT_BYTE -= len_byte;
  ZV -= len; ZV_BYTE -= len_byte;
  Z -= len; Z_BYTE -= len_byte;
  adjust_after_replace (from, from_byte, Qnil, newlen, len_byte);
}

/* Replace the text from character positions FROM to TO with NEW,
   If PREPARE is nonzero, call prepare_to_modify_buffer.
   If INHERIT, the newly inserted text should inherit text properties
   from the surrounding non-deleted text.  */

/* Note that this does not yet handle markers quite right.
   Also it needs to record a single undo-entry that does a replacement
   rather than a separate delete and insert.
   That way, undo will also handle markers properly.

   But if MARKERS is 0, don't relocate markers.  */

void
replace_range (EMACS_INT from, EMACS_INT to, Lisp_Object new,
	       int prepare, int inherit, int markers)
{
  EMACS_INT inschars = SCHARS (new);
  EMACS_INT insbytes = SBYTES (new);
  EMACS_INT from_byte, to_byte;
  EMACS_INT nbytes_del, nchars_del;
  struct gcpro gcpro1;
  INTERVAL intervals;
  EMACS_INT outgoing_insbytes = insbytes;
  Lisp_Object deletion;

  CHECK_MARKERS ();

  GCPRO1 (new);
  deletion = Qnil;

  if (prepare)
    {
      EMACS_INT range_length = to - from;
      prepare_to_modify_buffer (from, to, &from);
      to = from + range_length;
    }

  UNGCPRO;

  /* Make args be valid.  */
  if (from < BEGV)
    from = BEGV;
  if (to > ZV)
    to = ZV;

  from_byte = CHAR_TO_BYTE (from);
  to_byte = CHAR_TO_BYTE (to);

  nchars_del = to - from;
  nbytes_del = to_byte - from_byte;

  if (nbytes_del <= 0 && insbytes == 0)
    return;

  /* Make OUTGOING_INSBYTES describe the text
     as it will be inserted in this buffer.  */

  if (NILP (BVAR (current_buffer, enable_multibyte_characters)))
    outgoing_insbytes = inschars;
  else if (! STRING_MULTIBYTE (new))
    outgoing_insbytes
      = count_size_as_multibyte (SDATA (new), insbytes);

  GCPRO1 (new);

  /* Make sure the gap is somewhere in or next to what we are deleting.  */
  if (from > GPT)
    gap_right (from, from_byte);
  if (to < GPT)
    gap_left (to, to_byte, 0);

  /* Even if we don't record for undo, we must keep the original text
     because we may have to recover it because of inappropriate byte
     combining.  */
  if (! EQ (BVAR (current_buffer, undo_list), Qt))
    deletion = make_buffer_string_both (from, from_byte, to, to_byte, 1);

  GAP_SIZE += nbytes_del;
  ZV -= nchars_del;
  Z -= nchars_del;
  ZV_BYTE -= nbytes_del;
  Z_BYTE -= nbytes_del;
  GPT = from;
  GPT_BYTE = from_byte;
  if (GAP_SIZE > 0) *(GPT_ADDR) = 0; /* Put an anchor.  */

  if (GPT_BYTE < GPT)
    abort ();

  if (GPT - BEG < BEG_UNCHANGED)
    BEG_UNCHANGED = GPT - BEG;
  if (Z - GPT < END_UNCHANGED)
    END_UNCHANGED = Z - GPT;

  if (GAP_SIZE < outgoing_insbytes)
    make_gap (outgoing_insbytes - GAP_SIZE);

  /* Copy the string text into the buffer, perhaps converting
     between single-byte and multibyte.  */
  copy_text (SDATA (new), GPT_ADDR, insbytes,
	     STRING_MULTIBYTE (new),
	     ! NILP (BVAR (current_buffer, enable_multibyte_characters)));

#ifdef BYTE_COMBINING_DEBUG
  /* We have copied text into the gap, but we have not marked
     it as part of the buffer.  So we can use the old FROM and FROM_BYTE
     here, for both the previous text and the following text.
     Meanwhile, GPT_ADDR does point to
     the text that has been stored by copy_text.  */
  if (count_combining_before (GPT_ADDR, outgoing_insbytes, from, from_byte)
      || count_combining_after (GPT_ADDR, outgoing_insbytes, from, from_byte))
    abort ();
#endif

  if (! EQ (BVAR (current_buffer, undo_list), Qt))
    {
      /* Record the insertion first, so that when we undo,
	 the deletion will be undone first.  Thus, undo
	 will insert before deleting, and thus will keep
	 the markers before and after this text separate.  */
      record_insert (from + SCHARS (deletion), inschars);
      record_delete (from, deletion);
    }

  GAP_SIZE -= outgoing_insbytes;
  GPT += inschars;
  ZV += inschars;
  Z += inschars;
  GPT_BYTE += outgoing_insbytes;
  ZV_BYTE += outgoing_insbytes;
  Z_BYTE += outgoing_insbytes;
  if (GAP_SIZE > 0) *(GPT_ADDR) = 0; /* Put an anchor.  */

  if (GPT_BYTE < GPT)
    abort ();

  /* Adjust the overlay center as needed.  This must be done after
     adjusting the markers that bound the overlays.  */
  adjust_overlays_for_delete (from, nchars_del);
  adjust_overlays_for_insert (from, inschars);

  /* Adjust markers for the deletion and the insertion.  */
  if (markers)
    adjust_markers_for_replace (from, from_byte, nchars_del, nbytes_del,
				inschars, outgoing_insbytes);

  offset_intervals (current_buffer, from, inschars - nchars_del);

  /* Get the intervals for the part of the string we are inserting--
     not including the combined-before bytes.  */
  intervals = STRING_INTERVALS (new);
  /* Insert those intervals.  */
  graft_intervals_into_buffer (intervals, from, inschars,
			       current_buffer, inherit);

  /* Relocate point as if it were a marker.  */
  if (from < PT)
    adjust_point ((from + inschars - (PT < to ? PT : to)),
		  (from_byte + outgoing_insbytes
		   - (PT_BYTE < to_byte ? PT_BYTE : to_byte)));

  if (outgoing_insbytes == 0)
    evaporate_overlays (from);

  CHECK_MARKERS ();

  MODIFF++;
  CHARS_MODIFF = MODIFF;
  UNGCPRO;

  signal_after_change (from, nchars_del, GPT - from);
  update_compositions (from, GPT, CHECK_BORDER);
}

/* Replace the text from character positions FROM to TO with
   the text in INS of length INSCHARS.
   Keep the text properties that applied to the old characters
   (extending them to all the new chars if there are more new chars).

   Note that this does not yet handle markers quite right.

   If MARKERS is nonzero, relocate markers.

   Unlike most functions at this level, never call
   prepare_to_modify_buffer and never call signal_after_change.  */

void
replace_range_2 (EMACS_INT from, EMACS_INT from_byte,
		 EMACS_INT to, EMACS_INT to_byte,
		 const char *ins, EMACS_INT inschars, EMACS_INT insbytes,
		 int markers)
{
  EMACS_INT nbytes_del, nchars_del;

  CHECK_MARKERS ();

  nchars_del = to - from;
  nbytes_del = to_byte - from_byte;

  if (nbytes_del <= 0 && insbytes == 0)
    return;

  /* Make sure the gap is somewhere in or next to what we are deleting.  */
  if (from > GPT)
    gap_right (from, from_byte);
  if (to < GPT)
    gap_left (to, to_byte, 0);

  GAP_SIZE += nbytes_del;
  ZV -= nchars_del;
  Z -= nchars_del;
  ZV_BYTE -= nbytes_del;
  Z_BYTE -= nbytes_del;
  GPT = from;
  GPT_BYTE = from_byte;
  if (GAP_SIZE > 0) *(GPT_ADDR) = 0; /* Put an anchor.  */

  if (GPT_BYTE < GPT)
    abort ();

  if (GPT - BEG < BEG_UNCHANGED)
    BEG_UNCHANGED = GPT - BEG;
  if (Z - GPT < END_UNCHANGED)
    END_UNCHANGED = Z - GPT;

  if (GAP_SIZE < insbytes)
    make_gap (insbytes - GAP_SIZE);

  /* Copy the replacement text into the buffer.  */
  memcpy (GPT_ADDR, ins, insbytes);

#ifdef BYTE_COMBINING_DEBUG
  /* We have copied text into the gap, but we have not marked
     it as part of the buffer.  So we can use the old FROM and FROM_BYTE
     here, for both the previous text and the following text.
     Meanwhile, GPT_ADDR does point to
     the text that has been stored by copy_text.  */
  if (count_combining_before (GPT_ADDR, insbytes, from, from_byte)
      || count_combining_after (GPT_ADDR, insbytes, from, from_byte))
    abort ();
#endif

  GAP_SIZE -= insbytes;
  GPT += inschars;
  ZV += inschars;
  Z += inschars;
  GPT_BYTE += insbytes;
  ZV_BYTE += insbytes;
  Z_BYTE += insbytes;
  if (GAP_SIZE > 0) *(GPT_ADDR) = 0; /* Put an anchor.  */

  if (GPT_BYTE < GPT)
    abort ();

  /* Adjust the overlay center as needed.  This must be done after
     adjusting the markers that bound the overlays.  */
  if (nchars_del != inschars)
    {
      adjust_overlays_for_insert (from, inschars);
      adjust_overlays_for_delete (from + inschars, nchars_del);
    }

  /* Adjust markers for the deletion and the insertion.  */
  if (markers
      && ! (nchars_del == 1 && inschars == 1 && nbytes_del == insbytes))
    adjust_markers_for_replace (from, from_byte, nchars_del, nbytes_del,
				inschars, insbytes);

  offset_intervals (current_buffer, from, inschars - nchars_del);

  /* Relocate point as if it were a marker.  */
  if (from < PT && (nchars_del != inschars || nbytes_del != insbytes))
    {
      if (PT < to)
	/* PT was within the deleted text.  Move it to FROM.  */
	adjust_point (from - PT, from_byte - PT_BYTE);
      else
	adjust_point (inschars - nchars_del, insbytes - nbytes_del);
    }

  if (insbytes == 0)
    evaporate_overlays (from);

  CHECK_MARKERS ();

  MODIFF++;
  CHARS_MODIFF = MODIFF;
}

/* Delete characters in current buffer
   from FROM up to (but not including) TO.
   If TO comes before FROM, we delete nothing.  */

void
del_range (EMACS_INT from, EMACS_INT to)
{
  del_range_1 (from, to, 1, 0);
}

/* Like del_range; PREPARE says whether to call prepare_to_modify_buffer.
   RET_STRING says to return the deleted text. */

Lisp_Object
del_range_1 (EMACS_INT from, EMACS_INT to, int prepare, int ret_string)
{
  EMACS_INT from_byte, to_byte;
  Lisp_Object deletion;
  struct gcpro gcpro1;

  /* Make args be valid */
  if (from < BEGV)
    from = BEGV;
  if (to > ZV)
    to = ZV;

  if (to <= from)
    return Qnil;

  if (prepare)
    {
      EMACS_INT range_length = to - from;
      prepare_to_modify_buffer (from, to, &from);
      to = min (ZV, from + range_length);
    }

  from_byte = CHAR_TO_BYTE (from);
  to_byte = CHAR_TO_BYTE (to);

  deletion = del_range_2 (from, from_byte, to, to_byte, ret_string);
  GCPRO1 (deletion);
  signal_after_change (from, to - from, 0);
  update_compositions (from, from, CHECK_HEAD);
  UNGCPRO;
  return deletion;
}

/* Like del_range_1 but args are byte positions, not char positions.  */

void
del_range_byte (EMACS_INT from_byte, EMACS_INT to_byte, int prepare)
{
  EMACS_INT from, to;

  /* Make args be valid */
  if (from_byte < BEGV_BYTE)
    from_byte = BEGV_BYTE;
  if (to_byte > ZV_BYTE)
    to_byte = ZV_BYTE;

  if (to_byte <= from_byte)
    return;

  from = BYTE_TO_CHAR (from_byte);
  to = BYTE_TO_CHAR (to_byte);

  if (prepare)
    {
      EMACS_INT old_from = from, old_to = Z - to;
      EMACS_INT range_length = to - from;
      prepare_to_modify_buffer (from, to, &from);
      to = from + range_length;

      if (old_from != from)
	from_byte = CHAR_TO_BYTE (from);
      if (to > ZV)
	{
	  to = ZV;
	  to_byte = ZV_BYTE;
	}
      else if (old_to == Z - to)
	to_byte = CHAR_TO_BYTE (to);
    }

  del_range_2 (from, from_byte, to, to_byte, 0);
  signal_after_change (from, to - from, 0);
  update_compositions (from, from, CHECK_HEAD);
}

/* Like del_range_1, but positions are specified both as charpos
   and bytepos.  */

void
del_range_both (EMACS_INT from, EMACS_INT from_byte,
		EMACS_INT to, EMACS_INT to_byte, int prepare)
{
  /* Make args be valid */
  if (from_byte < BEGV_BYTE)
    from_byte = BEGV_BYTE;
  if (to_byte > ZV_BYTE)
    to_byte = ZV_BYTE;

  if (to_byte <= from_byte)
    return;

  if (from < BEGV)
    from = BEGV;
  if (to > ZV)
    to = ZV;

  if (prepare)
    {
      EMACS_INT old_from = from, old_to = Z - to;
      EMACS_INT range_length = to - from;
      prepare_to_modify_buffer (from, to, &from);
      to = from + range_length;

      if (old_from != from)
	from_byte = CHAR_TO_BYTE (from);
      if (to > ZV)
	{
	  to = ZV;
	  to_byte = ZV_BYTE;
	}
      else if (old_to == Z - to)
	to_byte = CHAR_TO_BYTE (to);
    }

  del_range_2 (from, from_byte, to, to_byte, 0);
  signal_after_change (from, to - from, 0);
  update_compositions (from, from, CHECK_HEAD);
}

/* Delete a range of text, specified both as character positions
   and byte positions.  FROM and TO are character positions,
   while FROM_BYTE and TO_BYTE are byte positions.
   If RET_STRING is true, the deleted area is returned as a string. */

Lisp_Object
del_range_2 (EMACS_INT from, EMACS_INT from_byte,
	     EMACS_INT to, EMACS_INT to_byte, int ret_string)
{
  register EMACS_INT nbytes_del, nchars_del;
  Lisp_Object deletion;

  CHECK_MARKERS ();

  nchars_del = to - from;
  nbytes_del = to_byte - from_byte;

  /* Make sure the gap is somewhere in or next to what we are deleting.  */
  if (from > GPT)
    gap_right (from, from_byte);
  if (to < GPT)
    gap_left (to, to_byte, 0);

#ifdef BYTE_COMBINING_DEBUG
  if (count_combining_before (BUF_BYTE_ADDRESS (current_buffer, to_byte),
			      Z_BYTE - to_byte, from, from_byte))
    abort ();
#endif

  if (ret_string || ! EQ (BVAR (current_buffer, undo_list), Qt))
    deletion = make_buffer_string_both (from, from_byte, to, to_byte, 1);
  else
    deletion = Qnil;

  /* Relocate all markers pointing into the new, larger gap
     to point at the end of the text before the gap.
     Do this before recording the deletion,
     so that undo handles this after reinserting the text.  */
  adjust_markers_for_delete (from, from_byte, to, to_byte);

  if (! EQ (BVAR (current_buffer, undo_list), Qt))
    record_delete (from, deletion);
  MODIFF++;
  CHARS_MODIFF = MODIFF;

  /* Relocate point as if it were a marker.  */
  if (from < PT)
    adjust_point (from - (PT < to ? PT : to),
		  from_byte - (PT_BYTE < to_byte ? PT_BYTE : to_byte));

  offset_intervals (current_buffer, from, - nchars_del);

  /* Adjust the overlay center as needed.  This must be done after
     adjusting the markers that bound the overlays.  */
  adjust_overlays_for_delete (from, nchars_del);

  GAP_SIZE += nbytes_del;
  ZV_BYTE -= nbytes_del;
  Z_BYTE -= nbytes_del;
  ZV -= nchars_del;
  Z -= nchars_del;
  GPT = from;
  GPT_BYTE = from_byte;
  if (GAP_SIZE > 0 && !current_buffer->text->inhibit_shrinking)
    /* Put an anchor, unless called from decode_coding_object which
       needs to access the previous gap contents.  */
    *(GPT_ADDR) = 0;

  if (GPT_BYTE < GPT)
    abort ();

  if (GPT - BEG < BEG_UNCHANGED)
    BEG_UNCHANGED = GPT - BEG;
  if (Z - GPT < END_UNCHANGED)
    END_UNCHANGED = Z - GPT;

  CHECK_MARKERS ();

  evaporate_overlays (from);

  return deletion;
}

/* Call this if you're about to change the region of BUFFER from
   character positions START to END.  This checks the read-only
   properties of the region, calls the necessary modification hooks,
   and warns the next redisplay that it should pay attention to that
   area.

   If PRESERVE_CHARS_MODIFF is non-zero, do not update CHARS_MODIFF.
   Otherwise set CHARS_MODIFF to the new value of MODIFF.  */

void
modify_region (struct buffer *buffer, EMACS_INT start, EMACS_INT end,
	       int preserve_chars_modiff)
{
  struct buffer *old_buffer = current_buffer;

  if (buffer != old_buffer)
    set_buffer_internal (buffer);

  prepare_to_modify_buffer (start, end, NULL);

  BUF_COMPUTE_UNCHANGED (buffer, start - 1, end);

  if (MODIFF <= SAVE_MODIFF)
    record_first_change ();
  MODIFF++;
  if (! preserve_chars_modiff)
    CHARS_MODIFF = MODIFF;

  BVAR (buffer, point_before_scroll) = Qnil;

  if (buffer != old_buffer)
    set_buffer_internal (old_buffer);
}

/* Check that it is okay to modify the buffer between START and END,
   which are char positions.

   Run the before-change-function, if any.  If intervals are in use,
   verify that the text to be modified is not read-only, and call
   any modification properties the text may have.

   If PRESERVE_PTR is nonzero, we relocate *PRESERVE_PTR
   by holding its value temporarily in a marker.  */

void
prepare_to_modify_buffer (EMACS_INT start, EMACS_INT end,
			  EMACS_INT *preserve_ptr)
{
  struct buffer *base_buffer;

  if (!NILP (BVAR (current_buffer, read_only)))
    Fbarf_if_buffer_read_only ();

  /* Let redisplay consider other windows than selected_window
     if modifying another buffer.  */
  if (XBUFFER (XWINDOW (selected_window)->buffer) != current_buffer)
    ++windows_or_buffers_changed;

  if (BUF_INTERVALS (current_buffer) != 0)
    {
      if (preserve_ptr)
	{
	  Lisp_Object preserve_marker;
	  struct gcpro gcpro1;
	  preserve_marker = Fcopy_marker (make_number (*preserve_ptr), Qnil);
	  GCPRO1 (preserve_marker);
	  verify_interval_modification (current_buffer, start, end);
	  *preserve_ptr = marker_position (preserve_marker);
	  unchain_marker (XMARKER (preserve_marker));
	  UNGCPRO;
	}
      else
	verify_interval_modification (current_buffer, start, end);
    }

  /* For indirect buffers, use the base buffer to check clashes.  */
  if (current_buffer->base_buffer != 0)
    base_buffer = current_buffer->base_buffer;
  else
    base_buffer = current_buffer;

#ifdef CLASH_DETECTION
  if (!NILP (BVAR (base_buffer, file_truename))
      /* Make binding buffer-file-name to nil effective.  */
      && !NILP (BVAR (base_buffer, filename))
      && SAVE_MODIFF >= MODIFF)
    lock_file (BVAR (base_buffer, file_truename));
#else
  /* At least warn if this file has changed on disk since it was visited.  */
  if (!NILP (BVAR (base_buffer, filename))
      && SAVE_MODIFF >= MODIFF
      && NILP (Fverify_visited_file_modtime (Fcurrent_buffer ()))
      && !NILP (Ffile_exists_p (BVAR (base_buffer, filename))))
    call1 (intern ("ask-user-about-supersession-threat"),
	   BVAR (base_buffer,filename));
#endif /* not CLASH_DETECTION */

  /* If `select-active-regions' is non-nil, save the region text.  */
  if (!NILP (BVAR (current_buffer, mark_active))
      && !inhibit_modification_hooks
      && XMARKER (BVAR (current_buffer, mark))->buffer
      && NILP (Vsaved_region_selection)
      && (EQ (Vselect_active_regions, Qonly)
	  ? EQ (CAR_SAFE (Vtransient_mark_mode), Qonly)
	  : (!NILP (Vselect_active_regions)
	     && !NILP (Vtransient_mark_mode))))
    {
      EMACS_INT b = XMARKER (BVAR (current_buffer, mark))->charpos;
      EMACS_INT e = PT;
      if (b < e)
	Vsaved_region_selection = make_buffer_string (b, e, 0);
      else if (b > e)
	Vsaved_region_selection = make_buffer_string (e, b, 0);
    }

  signal_before_change (start, end, preserve_ptr);

  if (current_buffer->newline_cache)
    invalidate_region_cache (current_buffer,
                             current_buffer->newline_cache,
                             start - BEG, Z - end);
  if (current_buffer->width_run_cache)
    invalidate_region_cache (current_buffer,
                             current_buffer->width_run_cache,
                             start - BEG, Z - end);

  Vdeactivate_mark = Qt;
}

/* These macros work with an argument named `preserve_ptr'
   and a local variable named `preserve_marker'.  */

#define PRESERVE_VALUE							\
  if (preserve_ptr && NILP (preserve_marker))				\
    preserve_marker = Fcopy_marker (make_number (*preserve_ptr), Qnil)

#define RESTORE_VALUE						\
  if (! NILP (preserve_marker))					\
    {								\
      *preserve_ptr = marker_position (preserve_marker);	\
      unchain_marker (XMARKER (preserve_marker));		\
    }

#define PRESERVE_START_END			\
  if (NILP (start_marker))			\
    start_marker = Fcopy_marker (start, Qnil);	\
  if (NILP (end_marker))			\
    end_marker = Fcopy_marker (end, Qnil);

#define FETCH_START				\
  (! NILP (start_marker) ? Fmarker_position (start_marker) : start)

#define FETCH_END				\
  (! NILP (end_marker) ? Fmarker_position (end_marker) : end)

/* Set a variable to nil if an error occurred.
   Don't change the variable if there was no error.
   VAL is a cons-cell (VARIABLE . NO-ERROR-FLAG).
   VARIABLE is the variable to maybe set to nil.
   NO-ERROR-FLAG is nil if there was an error,
   anything else meaning no error (so this function does nothing).  */
static Lisp_Object
reset_var_on_error (Lisp_Object val)
{
  if (NILP (XCDR (val)))
    Fset (XCAR (val), Qnil);
  return Qnil;
}

/* Signal a change to the buffer immediately before it happens.
   START_INT and END_INT are the bounds of the text to be changed.

   If PRESERVE_PTR is nonzero, we relocate *PRESERVE_PTR
   by holding its value temporarily in a marker.  */

static void
signal_before_change (EMACS_INT start_int, EMACS_INT end_int,
		      EMACS_INT *preserve_ptr)
{
  Lisp_Object start, end;
  Lisp_Object start_marker, end_marker;
  Lisp_Object preserve_marker;
  struct gcpro gcpro1, gcpro2, gcpro3;
  int count = SPECPDL_INDEX ();

  if (inhibit_modification_hooks)
    return;

  start = make_number (start_int);
  end = make_number (end_int);
  preserve_marker = Qnil;
  start_marker = Qnil;
  end_marker = Qnil;
  GCPRO3 (preserve_marker, start_marker, end_marker);

  specbind (Qinhibit_modification_hooks, Qt);

  /* If buffer is unmodified, run a special hook for that case.  The
   check for Vfirst_change_hook is just a minor optimization. */
  if (SAVE_MODIFF >= MODIFF
      && !NILP (Vfirst_change_hook))
    {
      PRESERVE_VALUE;
      PRESERVE_START_END;
      Frun_hooks (1, &Qfirst_change_hook);
    }

  /* Now run the before-change-functions if any.  */
  if (!NILP (Vbefore_change_functions))
    {
      Lisp_Object args[3];
      Lisp_Object rvoe_arg = Fcons (Qbefore_change_functions, Qnil);

      PRESERVE_VALUE;
      PRESERVE_START_END;

      /* Mark before-change-functions to be reset to nil in case of error.  */
      record_unwind_protect (reset_var_on_error, rvoe_arg);

      /* Actually run the hook functions.  */
      args[0] = Qbefore_change_functions;
      args[1] = FETCH_START;
      args[2] = FETCH_END;
      Frun_hook_with_args (3, args);

      /* There was no error: unarm the reset_on_error.  */
      XSETCDR (rvoe_arg, Qt);
    }

  if (current_buffer->overlays_before || current_buffer->overlays_after)
    {
      PRESERVE_VALUE;
      report_overlay_modification (FETCH_START, FETCH_END, 0,
				   FETCH_START, FETCH_END, Qnil);
    }

  if (! NILP (start_marker))
    free_marker (start_marker);
  if (! NILP (end_marker))
    free_marker (end_marker);
  RESTORE_VALUE;
  UNGCPRO;

  unbind_to (count, Qnil);
}

/* Signal a change immediately after it happens.
   CHARPOS is the character position of the start of the changed text.
   LENDEL is the number of characters of the text before the change.
   (Not the whole buffer; just the part that was changed.)
   LENINS is the number of characters in that part of the text
   after the change.  */

void
signal_after_change (EMACS_INT charpos, EMACS_INT lendel, EMACS_INT lenins)
{
  int count = SPECPDL_INDEX ();
  if (inhibit_modification_hooks)
    return;

  /* If we are deferring calls to the after-change functions
     and there are no before-change functions,
     just record the args that we were going to use.  */
  if (! NILP (Vcombine_after_change_calls)
      && NILP (Vbefore_change_functions)
      && !current_buffer->overlays_before
      && !current_buffer->overlays_after)
    {
      Lisp_Object elt;

      if (!NILP (combine_after_change_list)
	  && current_buffer != XBUFFER (combine_after_change_buffer))
	Fcombine_after_change_execute ();

      elt = Fcons (make_number (charpos - BEG),
		   Fcons (make_number (Z - (charpos - lendel + lenins)),
			  Fcons (make_number (lenins - lendel), Qnil)));
      combine_after_change_list
	= Fcons (elt, combine_after_change_list);
      combine_after_change_buffer = Fcurrent_buffer ();

      return;
    }

  if (!NILP (combine_after_change_list))
    Fcombine_after_change_execute ();

  specbind (Qinhibit_modification_hooks, Qt);

  if (!NILP (Vafter_change_functions))
    {
      Lisp_Object args[4];
      Lisp_Object rvoe_arg = Fcons (Qafter_change_functions, Qnil);

      /* Mark after-change-functions to be reset to nil in case of error.  */
      record_unwind_protect (reset_var_on_error, rvoe_arg);

      /* Actually run the hook functions.  */
      args[0] = Qafter_change_functions;
      XSETFASTINT (args[1], charpos);
      XSETFASTINT (args[2], charpos + lenins);
      XSETFASTINT (args[3], lendel);
      Frun_hook_with_args (4, args);

      /* There was no error: unarm the reset_on_error.  */
      XSETCDR (rvoe_arg, Qt);
    }

  if (current_buffer->overlays_before || current_buffer->overlays_after)
    report_overlay_modification (make_number (charpos),
				 make_number (charpos + lenins),
				 1,
				 make_number (charpos),
				 make_number (charpos + lenins),
				 make_number (lendel));

  /* After an insertion, call the text properties
     insert-behind-hooks or insert-in-front-hooks.  */
  if (lendel == 0)
    report_interval_modification (make_number (charpos),
				  make_number (charpos + lenins));

  unbind_to (count, Qnil);
}

static Lisp_Object
Fcombine_after_change_execute_1 (Lisp_Object val)
{
  Vcombine_after_change_calls = val;
  return val;
}

DEFUN ("combine-after-change-execute", Fcombine_after_change_execute,
       Scombine_after_change_execute, 0, 0, 0,
       doc: /* This function is for use internally in `combine-after-change-calls'.  */)
  (void)
{
  int count = SPECPDL_INDEX ();
  EMACS_INT beg, end, change;
  EMACS_INT begpos, endpos;
  Lisp_Object tail;

  if (NILP (combine_after_change_list))
    return Qnil;

  /* It is rare for combine_after_change_buffer to be invalid, but
     possible.  It can happen when combine-after-change-calls is
     non-nil, and insertion calls a file handler (e.g. through
     lock_file) which scribbles into a temp file -- cyd  */
  if (!BUFFERP (combine_after_change_buffer)
      || NILP (BVAR (XBUFFER (combine_after_change_buffer), name)))
    {
      combine_after_change_list = Qnil;
      return Qnil;
    }

  record_unwind_protect (Fset_buffer, Fcurrent_buffer ());

  Fset_buffer (combine_after_change_buffer);

  /* # chars unchanged at beginning of buffer.  */
  beg = Z - BEG;
  /* # chars unchanged at end of buffer.  */
  end = beg;
  /* Total amount of insertion (negative for deletion).  */
  change = 0;

  /* Scan the various individual changes,
     accumulating the range info in BEG, END and CHANGE.  */
  for (tail = combine_after_change_list; CONSP (tail);
       tail = XCDR (tail))
    {
      Lisp_Object elt;
      EMACS_INT thisbeg, thisend, thischange;

      /* Extract the info from the next element.  */
      elt = XCAR (tail);
      if (! CONSP (elt))
	continue;
      thisbeg = XINT (XCAR (elt));

      elt = XCDR (elt);
      if (! CONSP (elt))
	continue;
      thisend = XINT (XCAR (elt));

      elt = XCDR (elt);
      if (! CONSP (elt))
	continue;
      thischange = XINT (XCAR (elt));

      /* Merge this range into the accumulated range.  */
      change += thischange;
      if (thisbeg < beg)
	beg = thisbeg;
      if (thisend < end)
	end = thisend;
    }

  /* Get the current start and end positions of the range
     that was changed.  */
  begpos = BEG + beg;
  endpos = Z - end;

  /* We are about to handle these, so discard them.  */
  combine_after_change_list = Qnil;

  /* Now run the after-change functions for real.
     Turn off the flag that defers them.  */
  record_unwind_protect (Fcombine_after_change_execute_1,
			 Vcombine_after_change_calls);
  signal_after_change (begpos, endpos - begpos - change, endpos - begpos);
  update_compositions (begpos, endpos, CHECK_ALL);

  return unbind_to (count, Qnil);
}

void
syms_of_insdel (void)
{
  staticpro (&combine_after_change_list);
  staticpro (&combine_after_change_buffer);
  combine_after_change_list = Qnil;
  combine_after_change_buffer = Qnil;

  DEFVAR_BOOL ("check-markers-debug-flag", check_markers_debug_flag,
	       doc: /* Non-nil means enable debugging checks for invalid marker positions.  */);
  check_markers_debug_flag = 0;
  DEFVAR_LISP ("combine-after-change-calls", Vcombine_after_change_calls,
	       doc: /* Used internally by the `combine-after-change-calls' macro.  */);
  Vcombine_after_change_calls = Qnil;

  DEFVAR_BOOL ("inhibit-modification-hooks", inhibit_modification_hooks,
	       doc: /* Non-nil means don't run any of the hooks that respond to buffer changes.
This affects `before-change-functions' and `after-change-functions',
as well as hooks attached to text properties and overlays.  */);
  inhibit_modification_hooks = 0;
  DEFSYM (Qinhibit_modification_hooks, "inhibit-modification-hooks");

  defsubr (&Scombine_after_change_execute);
}
