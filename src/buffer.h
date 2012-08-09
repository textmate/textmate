/* Header file for the buffer manipulation primitives.

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

#include <sys/types.h> /* for off_t, time_t */

/* Accessing the parameters of the current buffer.  */

/* These macros come in pairs, one for the char position
   and one for the byte position.  */

/* Position of beginning of buffer.  */
#define BEG (1)
#define BEG_BYTE (BEG)

/* Position of beginning of accessible range of buffer.  */
#define BEGV (current_buffer->begv)
#define BEGV_BYTE (current_buffer->begv_byte)

/* Position of point in buffer.  The "+ 0" makes this
   not an l-value, so you can't assign to it.  Use SET_PT instead.  */
#define PT (current_buffer->pt + 0)
#define PT_BYTE (current_buffer->pt_byte + 0)

/* Position of gap in buffer.  */
#define GPT (current_buffer->text->gpt)
#define GPT_BYTE (current_buffer->text->gpt_byte)

/* Position of end of accessible range of buffer.  */
#define ZV (current_buffer->zv)
#define ZV_BYTE (current_buffer->zv_byte)

/* Position of end of buffer.  */
#define Z (current_buffer->text->z)
#define Z_BYTE (current_buffer->text->z_byte)

/* Macros for the addresses of places in the buffer.  */

/* Address of beginning of buffer.  */
#define BEG_ADDR (current_buffer->text->beg)

/* Address of beginning of accessible range of buffer.  */
#define BEGV_ADDR (BYTE_POS_ADDR (current_buffer->begv_byte))

/* Address of point in buffer.  */
#define PT_ADDR (BYTE_POS_ADDR (current_buffer->pt_byte))

/* Address of beginning of gap in buffer.  */
#define GPT_ADDR (current_buffer->text->beg + current_buffer->text->gpt_byte - BEG_BYTE)

/* Address of end of gap in buffer.  */
#define GAP_END_ADDR (current_buffer->text->beg + current_buffer->text->gpt_byte + current_buffer->text->gap_size - BEG_BYTE)

/* Address of end of accessible range of buffer.  */
#define ZV_ADDR (BYTE_POS_ADDR (current_buffer->zv_byte))

/* Address of end of buffer.  */
#define Z_ADDR (current_buffer->text->beg + current_buffer->text->gap_size + current_buffer->text->z_byte - BEG_BYTE)

/* Size of gap.  */
#define GAP_SIZE (current_buffer->text->gap_size)

/* Is the current buffer narrowed?  */
#define NARROWED	((BEGV != BEG) || (ZV != Z))

/* Modification count.  */
#define MODIFF (current_buffer->text->modiff)

/* Character modification count.  */
#define CHARS_MODIFF (current_buffer->text->chars_modiff)

/* Overlay modification count.  */
#define OVERLAY_MODIFF (current_buffer->text->overlay_modiff)

/* Modification count as of last visit or save.  */
#define SAVE_MODIFF (current_buffer->text->save_modiff)

/* BUFFER_CEILING_OF (resp. BUFFER_FLOOR_OF), when applied to n, return
   the max (resp. min) p such that

   BYTE_POS_ADDR (p) - BYTE_POS_ADDR (n) == p - n       */

#define BUFFER_CEILING_OF(BYTEPOS) \
  (((BYTEPOS) < GPT_BYTE && GPT < ZV ? GPT_BYTE : ZV_BYTE) - 1)
#define BUFFER_FLOOR_OF(BYTEPOS) \
  (BEGV <= GPT && GPT_BYTE <= (BYTEPOS) ? GPT_BYTE : BEGV_BYTE)

/* Similar macros to operate on a specified buffer.
   Note that many of these evaluate the buffer argument more than once.  */

/* Position of beginning of buffer.  */
#define BUF_BEG(buf) (BEG)
#define BUF_BEG_BYTE(buf) (BEG_BYTE)

/* The BUF_BEGV[_BYTE], BUF_ZV[_BYTE], and BUF_PT[_BYTE] macros cannot
   be used for assignment; use SET_BUF_* macros below for that.  */

/* Position of beginning of accessible range of buffer.  */
#define BUF_BEGV(buf)					\
   (buf == current_buffer ? BEGV			\
    : NILP (BVAR (buf, begv_marker)) ? buf->begv	\
    : marker_position (BVAR (buf, begv_marker)))

#define BUF_BEGV_BYTE(buf)				\
   (buf == current_buffer ? BEGV_BYTE			\
    : NILP (BVAR (buf, begv_marker)) ? buf->begv_byte	\
    : marker_byte_position (BVAR (buf, begv_marker)))

/* Position of point in buffer.  */
#define BUF_PT(buf)					\
   (buf == current_buffer ? PT				\
    : NILP (BVAR (buf, pt_marker)) ? buf->pt		\
    : marker_position (BVAR (buf, pt_marker)))

#define BUF_PT_BYTE(buf)				\
   (buf == current_buffer ? PT_BYTE			\
    : NILP (BVAR (buf, pt_marker)) ? buf->pt_byte	\
    : marker_byte_position (BVAR (buf, pt_marker)))

/* Position of end of accessible range of buffer.  */
#define BUF_ZV(buf)					\
   (buf == current_buffer ? ZV				\
    : NILP (BVAR (buf, zv_marker)) ? buf->zv		\
    : marker_position (BVAR (buf, zv_marker)))

#define BUF_ZV_BYTE(buf)				\
   (buf == current_buffer ? ZV_BYTE			\
    : NILP (BVAR (buf, zv_marker)) ? buf->zv_byte	\
    : marker_byte_position (BVAR (buf, zv_marker)))

/* Position of gap in buffer.  */
#define BUF_GPT(buf) ((buf)->text->gpt)
#define BUF_GPT_BYTE(buf) ((buf)->text->gpt_byte)

/* Position of end of buffer.  */
#define BUF_Z(buf) ((buf)->text->z)
#define BUF_Z_BYTE(buf) ((buf)->text->z_byte)

/* Address of beginning of buffer.  */
#define BUF_BEG_ADDR(buf) ((buf)->text->beg)

/* Address of beginning of gap of buffer.  */
#define BUF_GPT_ADDR(buf) ((buf)->text->beg + (buf)->text->gpt_byte - BEG_BYTE)

/* Address of end of buffer.  */
#define BUF_Z_ADDR(buf) ((buf)->text->beg + (buf)->text->gap_size + (buf)->text->z_byte - BEG_BYTE)

/* Address of end of gap in buffer.  */
#define BUF_GAP_END_ADDR(buf) ((buf)->text->beg + (buf)->text->gpt_byte + (buf)->text->gap_size - BEG_BYTE)

/* Size of gap.  */
#define BUF_GAP_SIZE(buf) ((buf)->text->gap_size)

/* Is this buffer narrowed?  */
#define BUF_NARROWED(buf) ((BUF_BEGV (buf) != BUF_BEG (buf)) \
			   || (BUF_ZV (buf) != BUF_Z (buf)))

/* Modification count.  */
#define BUF_MODIFF(buf) ((buf)->text->modiff)

/* Character modification count.  */
#define BUF_CHARS_MODIFF(buf) ((buf)->text->chars_modiff)

/* Modification count as of last visit or save.  */
#define BUF_SAVE_MODIFF(buf) ((buf)->text->save_modiff)

/* Overlay modification count.  */
#define BUF_OVERLAY_MODIFF(buf) ((buf)->text->overlay_modiff)

/* Modification count as of last auto-save.  */
/* FIXME: should we move this into ->text->auto_save_modiff?  */
#define BUF_AUTOSAVE_MODIFF(buf) ((buf)->auto_save_modified)

/* Interval tree of buffer.  */
#define BUF_INTERVALS(buf) ((buf)->text->intervals)

/* Marker chain of buffer.  */
#define BUF_MARKERS(buf) ((buf)->text->markers)

#define BUF_UNCHANGED_MODIFIED(buf) \
  ((buf)->text->unchanged_modified)

#define BUF_OVERLAY_UNCHANGED_MODIFIED(buf) \
  ((buf)->text->overlay_unchanged_modified)
#define BUF_BEG_UNCHANGED(buf) ((buf)->text->beg_unchanged)
#define BUF_END_UNCHANGED(buf) ((buf)->text->end_unchanged)

#define UNCHANGED_MODIFIED \
  BUF_UNCHANGED_MODIFIED (current_buffer)
#define OVERLAY_UNCHANGED_MODIFIED \
  BUF_OVERLAY_UNCHANGED_MODIFIED (current_buffer)
#define BEG_UNCHANGED BUF_BEG_UNCHANGED (current_buffer)
#define END_UNCHANGED BUF_END_UNCHANGED (current_buffer)

/* Compute how many characters at the top and bottom of BUF are
   unchanged when the range START..END is modified.  This computation
   must be done each time BUF is modified.  */

#define BUF_COMPUTE_UNCHANGED(buf, start, end)				\
  do									\
    {									\
      if (BUF_UNCHANGED_MODIFIED (buf) == BUF_MODIFF (buf)		\
	  && (BUF_OVERLAY_UNCHANGED_MODIFIED (buf)			\
	      == BUF_OVERLAY_MODIFF (buf)))				\
	{								\
	  BUF_BEG_UNCHANGED (buf) = (start) - BUF_BEG (buf);		\
	  BUF_END_UNCHANGED (buf) = BUF_Z (buf) - (end);		\
	}								\
      else								\
	{								\
	  if (BUF_Z (buf) - (end) < BUF_END_UNCHANGED (buf))		\
	    BUF_END_UNCHANGED (buf) = BUF_Z (buf) - (end);		\
	  if ((start) - BUF_BEG (buf) < BUF_BEG_UNCHANGED (buf))	\
	    BUF_BEG_UNCHANGED (buf) = (start) - BUF_BEG (buf);		\
	}								\
    }									\
  while (0)


/* Macros to set PT in the current buffer, or another buffer.  */

#define SET_PT(position) (set_point (position))
#define TEMP_SET_PT(position) (temp_set_point (current_buffer, (position)))

#define SET_PT_BOTH(position, byte) (set_point_both (position, byte))
#define TEMP_SET_PT_BOTH(position, byte) \
  (temp_set_point_both (current_buffer, (position), (byte)))

#define BUF_TEMP_SET_PT(buffer, position) \
  (temp_set_point ((buffer), (position)))

extern void set_point (EMACS_INT);
extern void temp_set_point (struct buffer *, EMACS_INT);
extern void set_point_both (EMACS_INT, EMACS_INT);
extern void temp_set_point_both (struct buffer *,
				 EMACS_INT, EMACS_INT);
extern void enlarge_buffer_text (struct buffer *, EMACS_INT);


/* Macros for setting the BEGV, ZV or PT of a given buffer.

   The ..._BOTH macros take both a charpos and a bytepos,
   which must correspond to each other.

   The macros without ..._BOTH take just a charpos,
   and compute the bytepos from it.  */

#define SET_BUF_BEGV(buf, charpos)				 \
  ((buf)->begv_byte = buf_charpos_to_bytepos ((buf), (charpos)), \
   (buf)->begv = (charpos))

#define SET_BUF_ZV(buf, charpos)				\
  ((buf)->zv_byte = buf_charpos_to_bytepos ((buf), (charpos)),	\
   (buf)->zv = (charpos))

#define SET_BUF_BEGV_BOTH(buf, charpos, byte)		\
  ((buf)->begv = (charpos),				\
   (buf)->begv_byte = (byte))

#define SET_BUF_ZV_BOTH(buf, charpos, byte)		\
  ((buf)->zv = (charpos),				\
   (buf)->zv_byte = (byte))

#define SET_BUF_PT_BOTH(buf, charpos, byte)		\
  ((buf)->pt = (charpos),				\
   (buf)->pt_byte = (byte))

/* Macros to access a character or byte in the current buffer,
   or convert between a byte position and an address.
   These macros do not check that the position is in range.  */

/* Access a Lisp position value in POS,
   and store the charpos in CHARPOS and the bytepos in BYTEPOS.  */

#define DECODE_POSITION(charpos, bytepos, pos)			\
do								\
  {								\
    Lisp_Object __pos = (pos);					\
    if (NUMBERP (__pos))					\
      {								\
	charpos = __pos;					\
	bytepos = buf_charpos_to_bytepos (current_buffer, __pos);  \
      }								\
    else if (MARKERP (__pos))					\
      {								\
	charpos = marker_position (__pos);			\
	bytepos = marker_byte_position (__pos);			\
      }								\
    else							\
      wrong_type_argument (Qinteger_or_marker_p, __pos);	\
  }								\
while (0)

/* Maximum number of bytes in a buffer.
   A buffer cannot contain more bytes than a 1-origin fixnum can represent,
   nor can it be so large that C pointer arithmetic stops working.
   The ptrdiff_t cast ensures that this is signed, not unsigned.  */
#define BUF_BYTES_MAX \
  (ptrdiff_t) min (MOST_POSITIVE_FIXNUM - 1, min (SIZE_MAX, PTRDIFF_MAX))

/* Return the address of byte position N in current buffer.  */

#define BYTE_POS_ADDR(n) \
  (((n) >= GPT_BYTE ? GAP_SIZE : 0) + (n) + BEG_ADDR - BEG_BYTE)

/* Return the address of char position N.  */

#define CHAR_POS_ADDR(n)			\
  (((n) >= GPT ? GAP_SIZE : 0)			\
   + buf_charpos_to_bytepos (current_buffer, n)	\
   + BEG_ADDR - BEG_BYTE)

/* Convert a character position to a byte position.  */

#define CHAR_TO_BYTE(charpos)			\
  (buf_charpos_to_bytepos (current_buffer, charpos))

/* Convert a byte position to a character position.  */

#define BYTE_TO_CHAR(bytepos)			\
  (buf_bytepos_to_charpos (current_buffer, bytepos))

/* Convert PTR, the address of a byte in the buffer, into a byte position.  */

#define PTR_BYTE_POS(ptr) \
((ptr) - (current_buffer)->text->beg					    \
 - (ptr - (current_buffer)->text->beg <= GPT_BYTE - BEG_BYTE ? 0 : GAP_SIZE) \
 + BEG_BYTE)

/* Return character at byte position POS.  See the caveat WARNING for
   FETCH_MULTIBYTE_CHAR below.  */

#define FETCH_CHAR(pos)				      	\
  (!NILP (BVAR (current_buffer, enable_multibyte_characters))	\
   ? FETCH_MULTIBYTE_CHAR ((pos))		      	\
   : FETCH_BYTE ((pos)))

/* Return the byte at byte position N.  */

#define FETCH_BYTE(n) *(BYTE_POS_ADDR ((n)))

/* Variables used locally in FETCH_MULTIBYTE_CHAR.  */
extern unsigned char *_fetch_multibyte_char_p;

/* Return character code of multi-byte form at byte position POS.  If POS
   doesn't point the head of valid multi-byte form, only the byte at
   POS is returned.  No range checking.

   WARNING: The character returned by this macro could be "unified"
   inside STRING_CHAR, if the original character in the buffer belongs
   to one of the Private Use Areas (PUAs) of codepoints that Emacs
   uses to support non-unified CJK characters.  If that happens,
   CHAR_BYTES will return a value that is different from the length of
   the original multibyte sequence stored in the buffer.  Therefore,
   do _not_ use FETCH_MULTIBYTE_CHAR if you need to advance through
   the buffer to the next character after fetching this one.  Instead,
   use either FETCH_CHAR_ADVANCE or STRING_CHAR_AND_LENGTH.  */

#define FETCH_MULTIBYTE_CHAR(pos)				 	\
  (_fetch_multibyte_char_p = (((pos) >= GPT_BYTE ? GAP_SIZE : 0) 	\
			       + (pos) + BEG_ADDR - BEG_BYTE),	 	\
   STRING_CHAR (_fetch_multibyte_char_p))

/* Return character at byte position POS.  If the current buffer is unibyte
   and the character is not ASCII, make the returning character
   multibyte.  */

#define FETCH_CHAR_AS_MULTIBYTE(pos)			\
  (!NILP (BVAR (current_buffer, enable_multibyte_characters))	\
   ? FETCH_MULTIBYTE_CHAR ((pos))			\
   : UNIBYTE_TO_CHAR (FETCH_BYTE ((pos))))


/* Macros for accessing a character or byte,
   or converting between byte positions and addresses,
   in a specified buffer.  */

/* Return the address of character at byte position POS in buffer BUF.
   Note that both arguments can be computed more than once.  */

#define BUF_BYTE_ADDRESS(buf, pos) \
((buf)->text->beg + (pos) - BEG_BYTE		\
 + ((pos) >= (buf)->text->gpt_byte ? (buf)->text->gap_size : 0))

/* Return the address of character at char position POS in buffer BUF.
   Note that both arguments can be computed more than once.  */

#define BUF_CHAR_ADDRESS(buf, pos) \
((buf)->text->beg + buf_charpos_to_bytepos ((buf), (pos)) - BEG_BYTE	\
 + ((pos) >= (buf)->text->gpt ? (buf)->text->gap_size : 0))

/* Convert PTR, the address of a char in buffer BUF,
   into a character position.  */

#define BUF_PTR_BYTE_POS(buf, ptr)				\
((ptr) - (buf)->text->beg					\
 - (ptr - (buf)->text->beg <= BUF_GPT_BYTE (buf) - BEG_BYTE	\
    ? 0 : BUF_GAP_SIZE ((buf)))					\
 + BEG_BYTE)

/* Return the character at byte position POS in buffer BUF.   */

#define BUF_FETCH_CHAR(buf, pos)	      	\
  (!NILP (buf->enable_multibyte_characters)	\
   ? BUF_FETCH_MULTIBYTE_CHAR ((buf), (pos))    \
   : BUF_FETCH_BYTE ((buf), (pos)))

/* Return the byte at byte position N in buffer BUF.   */

#define BUF_FETCH_BYTE(buf, n) \
  *(BUF_BYTE_ADDRESS ((buf), (n)))

/* Return character code of multi-byte form at byte position POS in BUF.
   If POS doesn't point the head of valid multi-byte form, only the byte at
   POS is returned.  No range checking.  */

#define BUF_FETCH_MULTIBYTE_CHAR(buf, pos)				\
  (_fetch_multibyte_char_p						\
     = (((pos) >= BUF_GPT_BYTE (buf) ? BUF_GAP_SIZE (buf) : 0)		\
        + (pos) + BUF_BEG_ADDR (buf) - BEG_BYTE),			\
   STRING_CHAR (_fetch_multibyte_char_p))

/* Define the actual buffer data structures.  */

/* This data structure describes the actual text contents of a buffer.
   It is shared between indirect buffers and their base buffer.  */

struct buffer_text
  {
    /* Actual address of buffer contents.  If REL_ALLOC is defined,
       this address might change when blocks are relocated which can
       e.g. happen when malloc is called.  So, don't pass a pointer
       into a buffer's text to functions that malloc.  */
    unsigned char *beg;

    EMACS_INT gpt;		/* Char pos of gap in buffer.  */
    EMACS_INT z;		/* Char pos of end of buffer.  */
    EMACS_INT gpt_byte;		/* Byte pos of gap in buffer.  */
    EMACS_INT z_byte;		/* Byte pos of end of buffer.  */
    EMACS_INT gap_size;		/* Size of buffer's gap.  */
    int modiff;			/* This counts buffer-modification events
				   for this buffer.  It is incremented for
				   each such event, and never otherwise
				   changed.  */
    int chars_modiff;           /* This is modified with character change
				   events for this buffer.  It is set to
				   modiff for each such event, and never
				   otherwise changed.  */
    int save_modiff;		/* Previous value of modiff, as of last
				   time buffer visited or saved a file.  */

    int overlay_modiff;		/* Counts modifications to overlays.  */

    /* Minimum value of GPT - BEG since last redisplay that finished.  */
    EMACS_INT beg_unchanged;

    /* Minimum value of Z - GPT since last redisplay that finished.  */
    EMACS_INT end_unchanged;

    /* MODIFF as of last redisplay that finished; if it matches MODIFF,
       beg_unchanged and end_unchanged contain no useful information.  */
    int unchanged_modified;

    /* BUF_OVERLAY_MODIFF of current buffer, as of last redisplay that
       finished; if it matches BUF_OVERLAY_MODIFF, beg_unchanged and
       end_unchanged contain no useful information.  */
    int overlay_unchanged_modified;

    /* Properties of this buffer's text.  */
    INTERVAL intervals;

    /* The markers that refer to this buffer.
       This is actually a single marker ---
       successive elements in its marker `chain'
       are the other markers referring to this buffer.
       This is a singly linked unordered list, which means that it's
       very cheap to add a marker to the list and it's also very cheap
       to move a marker within a buffer.  */
    struct Lisp_Marker *markers;

    /* Usually 0.  Temporarily set to 1 in decode_coding_gap to
       prevent Fgarbage_collect from shrinking the gap and losing
       not-yet-decoded bytes.  */
    int inhibit_shrinking;
  };

/* Lisp fields in struct buffer are hidden from most code and accessed
   via the BVAR macro, below.  Only select pieces of code, like the GC,
   are allowed to use BUFFER_INTERNAL_FIELD.  */
#define BUFFER_INTERNAL_FIELD(field) field ## _

/* Most code should use this macro to access Lisp fields in struct
   buffer.  */
#define BVAR(buf, field) ((buf)->BUFFER_INTERNAL_FIELD (field))

/* This is the structure that the buffer Lisp object points to.  */

struct buffer
{
  /* Everything before the `name' slot must be of a non-Lisp_Object type,
     and every slot after `name' must be a Lisp_Object.

     Check out mark_buffer (alloc.c) to see why.  */

  /* HEADER.NEXT is the next buffer, in chain of all buffers,
     including killed buffers.
     This chain is used only for garbage collection, in order to
     collect killed buffers properly.
     Note that vectors and most pseudovectors are all on one chain,
     but buffers are on a separate chain of their own.  */
  struct vectorlike_header header;

  /* This structure holds the coordinates of the buffer contents
     in ordinary buffers.  In indirect buffers, this is not used.  */
  struct buffer_text own_text;

  /* This points to the `struct buffer_text' that used for this buffer.
     In an ordinary buffer, this is the own_text field above.
     In an indirect buffer, this is the own_text field of another buffer.  */
  struct buffer_text *text;

  /* Char position of point in buffer.  */
  EMACS_INT pt;
  /* Byte position of point in buffer.  */
  EMACS_INT pt_byte;
  /* Char position of beginning of accessible range.  */
  EMACS_INT begv;
  /* Byte position of beginning of accessible range.  */
  EMACS_INT begv_byte;
  /* Char position of end of accessible range.  */
  EMACS_INT zv;
  /* Byte position of end of accessible range.  */
  EMACS_INT zv_byte;

  /* In an indirect buffer, this points to the base buffer.
     In an ordinary buffer, it is 0.  */
  struct buffer *base_buffer;

  /* A non-zero value in slot IDX means that per-buffer variable
     with index IDX has a local value in this buffer.  The index IDX
     for a buffer-local variable is stored in that variable's slot
     in buffer_local_flags as a Lisp integer.  If the index is -1,
     this means the variable is always local in all buffers.  */
#define MAX_PER_BUFFER_VARS 50
  char local_flags[MAX_PER_BUFFER_VARS];

  /* Set to the modtime of the visited file when read or written.
     -1 means visited file was nonexistent.
     0 means visited file modtime unknown; in no case complain
     about any mismatch on next save attempt.  */
  time_t modtime;
  /* Size of the file when modtime was set.  This is used to detect the
     case where the file grew while we were reading it, so the modtime
     is still the same (since it's rounded up to seconds) but we're actually
     not up-to-date.  -1 means the size is unknown.  Only meaningful if
     modtime is actually set.  */
  off_t modtime_size;
  /* The value of text->modiff at the last auto-save.  */
  int auto_save_modified;
  /* The value of text->modiff at the last display error.
     Redisplay of this buffer is inhibited until it changes again.  */
  int display_error_modiff;
  /* The time at which we detected a failure to auto-save,
     Or 0 if we didn't have a failure.  */
  time_t auto_save_failure_time;
  /* Position in buffer at which display started
     the last time this buffer was displayed.  */
  EMACS_INT last_window_start;

  /* Set nonzero whenever the narrowing is changed in this buffer.  */
  int clip_changed;

  /* If the long line scan cache is enabled (i.e. the buffer-local
     variable cache-long-line-scans is non-nil), newline_cache
     points to the newline cache, and width_run_cache points to the
     width run cache.

     The newline cache records which stretches of the buffer are
     known *not* to contain newlines, so that they can be skipped
     quickly when we search for newlines.

     The width run cache records which stretches of the buffer are
     known to contain characters whose widths are all the same.  If
     the width run cache maps a character to a value > 0, that value is
     the character's width; if it maps a character to zero, we don't
     know what its width is.  This allows compute_motion to process
     such regions very quickly, using algebra instead of inspecting
     each character.   See also width_table, below.  */
  struct region_cache *newline_cache;
  struct region_cache *width_run_cache;

  /* Non-zero means don't use redisplay optimizations for
     displaying this buffer.  */
  unsigned prevent_redisplay_optimizations_p : 1;

  /* List of overlays that end at or before the current center,
     in order of end-position.  */
  struct Lisp_Overlay *overlays_before;

  /* List of overlays that end after  the current center,
     in order of start-position.  */
  struct Lisp_Overlay *overlays_after;

  /* Position where the overlay lists are centered.  */
  EMACS_INT overlay_center;

  /* Everything from here down must be a Lisp_Object.  */
  /* buffer-local Lisp variables start at `undo_list',
     tho only the ones from `name' on are GC'd normally.  */
  #define FIRST_FIELD_PER_BUFFER undo_list

  /* Changes in the buffer are recorded here for undo.
     t means don't record anything.
     This information belongs to the base buffer of an indirect buffer,
     But we can't store it in the  struct buffer_text
     because local variables have to be right in the  struct buffer.
     So we copy it around in set_buffer_internal.
     This comes before `name' because it is marked in a special way.  */
  Lisp_Object BUFFER_INTERNAL_FIELD (undo_list);

  /* The name of this buffer.  */
  Lisp_Object BUFFER_INTERNAL_FIELD (name);

  /* The name of the file visited in this buffer, or nil.  */
  Lisp_Object BUFFER_INTERNAL_FIELD (filename);
  /* Dir for expanding relative file names.  */
  Lisp_Object BUFFER_INTERNAL_FIELD (directory);
  /* True if this buffer has been backed up (if you write to the
     visited file and it hasn't been backed up, then a backup will
     be made).  */
  /* This isn't really used by the C code, so could be deleted.  */
  Lisp_Object BUFFER_INTERNAL_FIELD (backed_up);
  /* Length of file when last read or saved.
     -1 means auto saving turned off because buffer shrank a lot.
     -2 means don't turn off auto saving if buffer shrinks.
       (That value is used with buffer-swap-text.)
     This is not in the  struct buffer_text
     because it's not used in indirect buffers at all.  */
  Lisp_Object BUFFER_INTERNAL_FIELD (save_length);
  /* File name used for auto-saving this buffer.
     This is not in the  struct buffer_text
     because it's not used in indirect buffers at all.  */
  Lisp_Object BUFFER_INTERNAL_FIELD (auto_save_file_name);

  /* Non-nil if buffer read-only.  */
  Lisp_Object BUFFER_INTERNAL_FIELD (read_only);
  /* "The mark".  This is a marker which may
     point into this buffer or may point nowhere.  */
  Lisp_Object BUFFER_INTERNAL_FIELD (mark);

  /* Alist of elements (SYMBOL . VALUE-IN-THIS-BUFFER) for all
     per-buffer variables of this buffer.  For locally unbound
     symbols, just the symbol appears as the element.  */
  Lisp_Object BUFFER_INTERNAL_FIELD (local_var_alist);

  /* Symbol naming major mode (eg, lisp-mode).  */
  Lisp_Object BUFFER_INTERNAL_FIELD (major_mode);
  /* Pretty name of major mode (eg, "Lisp"). */
  Lisp_Object BUFFER_INTERNAL_FIELD (mode_name);
  /* Mode line element that controls format of mode line.  */
  Lisp_Object BUFFER_INTERNAL_FIELD (mode_line_format);

  /* Analogous to mode_line_format for the line displayed at the top
     of windows.  Nil means don't display that line.  */
  Lisp_Object BUFFER_INTERNAL_FIELD (header_line_format);

  /* Keys that are bound local to this buffer.  */
  Lisp_Object BUFFER_INTERNAL_FIELD (keymap);
  /* This buffer's local abbrev table.  */
  Lisp_Object BUFFER_INTERNAL_FIELD (abbrev_table);
  /* This buffer's syntax table.  */
  Lisp_Object BUFFER_INTERNAL_FIELD (syntax_table);
  /* This buffer's category table.  */
  Lisp_Object BUFFER_INTERNAL_FIELD (category_table);

  /* Values of several buffer-local variables.  */
  /* tab-width is buffer-local so that redisplay can find it
     in buffers that are not current.  */
  Lisp_Object BUFFER_INTERNAL_FIELD (case_fold_search);
  Lisp_Object BUFFER_INTERNAL_FIELD (tab_width);
  Lisp_Object BUFFER_INTERNAL_FIELD (fill_column);
  Lisp_Object BUFFER_INTERNAL_FIELD (left_margin);
  /* Function to call when insert space past fill column.  */
  Lisp_Object BUFFER_INTERNAL_FIELD (auto_fill_function);

  /* Case table for case-conversion in this buffer.
     This char-table maps each char into its lower-case version.  */
  Lisp_Object BUFFER_INTERNAL_FIELD (downcase_table);
  /* Char-table mapping each char to its upper-case version.  */
  Lisp_Object BUFFER_INTERNAL_FIELD (upcase_table);
  /* Char-table for conversion for case-folding search.  */
  Lisp_Object BUFFER_INTERNAL_FIELD (case_canon_table);
  /* Char-table of equivalences for case-folding search.  */
  Lisp_Object BUFFER_INTERNAL_FIELD (case_eqv_table);

  /* Non-nil means do not display continuation lines.  */
  Lisp_Object BUFFER_INTERNAL_FIELD (truncate_lines);
  /* Non-nil means to use word wrapping when displaying continuation lines.  */
  Lisp_Object BUFFER_INTERNAL_FIELD (word_wrap);
  /* Non-nil means display ctl chars with uparrow.  */
  Lisp_Object BUFFER_INTERNAL_FIELD (ctl_arrow);
  /* Non-nil means reorder bidirectional text for display in the
     visual order.  */
  Lisp_Object BUFFER_INTERNAL_FIELD (bidi_display_reordering);
  /* If non-nil, specifies which direction of text to force in all the
     paragraphs of the buffer.  Nil means determine paragraph
     direction dynamically for each paragraph.  */
  Lisp_Object BUFFER_INTERNAL_FIELD (bidi_paragraph_direction);
  /* Non-nil means do selective display;
     see doc string in syms_of_buffer (buffer.c) for details.  */
  Lisp_Object BUFFER_INTERNAL_FIELD (selective_display);
#ifndef old
  /* Non-nil means show ... at end of line followed by invisible lines.  */
  Lisp_Object BUFFER_INTERNAL_FIELD (selective_display_ellipses);
#endif
  /* Alist of (FUNCTION . STRING) for each minor mode enabled in buffer.  */
  Lisp_Object BUFFER_INTERNAL_FIELD (minor_modes);
  /* t if "self-insertion" should overwrite; `binary' if it should also
     overwrite newlines and tabs - for editing executables and the like.  */
  Lisp_Object BUFFER_INTERNAL_FIELD (overwrite_mode);
  /* non-nil means abbrev mode is on.  Expand abbrevs automatically.  */
  Lisp_Object BUFFER_INTERNAL_FIELD (abbrev_mode);
  /* Display table to use for text in this buffer.  */
  Lisp_Object BUFFER_INTERNAL_FIELD (display_table);
  /* t means the mark and region are currently active.  */
  Lisp_Object BUFFER_INTERNAL_FIELD (mark_active);

  /* Non-nil means the buffer contents are regarded as multi-byte
     form of characters, not a binary code.  */
  Lisp_Object BUFFER_INTERNAL_FIELD (enable_multibyte_characters);

  /* Coding system to be used for encoding the buffer contents on
     saving.  */
  Lisp_Object BUFFER_INTERNAL_FIELD (buffer_file_coding_system);

  /* List of symbols naming the file format used for visited file.  */
  Lisp_Object BUFFER_INTERNAL_FIELD (file_format);

  /* List of symbols naming the file format used for auto-save file.  */
  Lisp_Object BUFFER_INTERNAL_FIELD (auto_save_file_format);

  /* True if the newline position cache and width run cache are
     enabled.  See search.c and indent.c.  */
  Lisp_Object BUFFER_INTERNAL_FIELD (cache_long_line_scans);

  /* If the width run cache is enabled, this table contains the
     character widths width_run_cache (see above) assumes.  When we
     do a thorough redisplay, we compare this against the buffer's
     current display table to see whether the display table has
     affected the widths of any characters.  If it has, we
     invalidate the width run cache, and re-initialize width_table.  */
  Lisp_Object BUFFER_INTERNAL_FIELD (width_table);

  /* In an indirect buffer, or a buffer that is the base of an
     indirect buffer, this holds a marker that records
     PT for this buffer when the buffer is not current.  */
  Lisp_Object BUFFER_INTERNAL_FIELD (pt_marker);

  /* In an indirect buffer, or a buffer that is the base of an
     indirect buffer, this holds a marker that records
     BEGV for this buffer when the buffer is not current.  */
  Lisp_Object BUFFER_INTERNAL_FIELD (begv_marker);

  /* In an indirect buffer, or a buffer that is the base of an
     indirect buffer, this holds a marker that records
     ZV for this buffer when the buffer is not current.  */
  Lisp_Object BUFFER_INTERNAL_FIELD (zv_marker);

  /* This holds the point value before the last scroll operation.
     Explicitly setting point sets this to nil.  */
  Lisp_Object BUFFER_INTERNAL_FIELD (point_before_scroll);

  /* Truename of the visited file, or nil.  */
  Lisp_Object BUFFER_INTERNAL_FIELD (file_truename);

  /* Invisibility spec of this buffer.
     t => any non-nil `invisible' property means invisible.
     A list => `invisible' property means invisible
     if it is memq in that list.  */
  Lisp_Object BUFFER_INTERNAL_FIELD (invisibility_spec);

  /* This is the last window that was selected with this buffer in it,
     or nil if that window no longer displays this buffer.  */
  Lisp_Object BUFFER_INTERNAL_FIELD (last_selected_window);

  /* Incremented each time the buffer is displayed in a window.  */
  Lisp_Object BUFFER_INTERNAL_FIELD (display_count);

  /* Widths of left and right marginal areas for windows displaying
     this buffer.  */
  Lisp_Object BUFFER_INTERNAL_FIELD (left_margin_cols), BUFFER_INTERNAL_FIELD (right_margin_cols);

  /* Widths of left and right fringe areas for windows displaying
     this buffer.  */
  Lisp_Object BUFFER_INTERNAL_FIELD (left_fringe_width), BUFFER_INTERNAL_FIELD (right_fringe_width);

  /* Non-nil means fringes are drawn outside display margins;
     othersize draw them between margin areas and text.  */
  Lisp_Object BUFFER_INTERNAL_FIELD (fringes_outside_margins);

  /* Width and type of scroll bar areas for windows displaying
     this buffer.  */
  Lisp_Object BUFFER_INTERNAL_FIELD (scroll_bar_width), BUFFER_INTERNAL_FIELD (vertical_scroll_bar_type);

  /* Non-nil means indicate lines not displaying text (in a style
     like vi).  */
  Lisp_Object BUFFER_INTERNAL_FIELD (indicate_empty_lines);

  /* Non-nil means indicate buffer boundaries and scrolling.  */
  Lisp_Object BUFFER_INTERNAL_FIELD (indicate_buffer_boundaries);

  /* Logical to physical fringe bitmap mappings.  */
  Lisp_Object BUFFER_INTERNAL_FIELD (fringe_indicator_alist);

  /* Logical to physical cursor bitmap mappings.  */
  Lisp_Object BUFFER_INTERNAL_FIELD (fringe_cursor_alist);

  /* Time stamp updated each time this buffer is displayed in a window.  */
  Lisp_Object BUFFER_INTERNAL_FIELD (display_time);

  /* If scrolling the display because point is below the bottom of a
     window showing this buffer, try to choose a window start so
     that point ends up this number of lines from the top of the
     window.  Nil means that scrolling method isn't used.  */
  Lisp_Object BUFFER_INTERNAL_FIELD (scroll_up_aggressively);

  /* If scrolling the display because point is above the top of a
     window showing this buffer, try to choose a window start so
     that point ends up this number of lines from the bottom of the
     window.  Nil means that scrolling method isn't used.  */
  Lisp_Object BUFFER_INTERNAL_FIELD (scroll_down_aggressively);

  /* Desired cursor type in this buffer.  See the doc string of
     per-buffer variable `cursor-type'.  */
  Lisp_Object BUFFER_INTERNAL_FIELD (cursor_type);

  /* An integer > 0 means put that number of pixels below text lines
     in the display of this buffer.  */
  Lisp_Object BUFFER_INTERNAL_FIELD (extra_line_spacing);

  /* *Cursor type to display in non-selected windows.
     t means to use hollow box cursor.
     See `cursor-type' for other values.  */
  Lisp_Object BUFFER_INTERNAL_FIELD (cursor_in_non_selected_windows);

  /* This must be the last field in the above list.  */
  #define LAST_FIELD_PER_BUFFER cursor_in_non_selected_windows
};


/* This points to the current buffer.  */

extern struct buffer *current_buffer;

/* This structure holds the default values of the buffer-local variables
   that have special slots in each buffer.
   The default value occupies the same slot in this structure
   as an individual buffer's value occupies in that buffer.
   Setting the default value also goes through the alist of buffers
   and stores into each buffer that does not say it has a local value.  */

extern struct buffer buffer_defaults;

/* This structure marks which slots in a buffer have corresponding
   default values in buffer_defaults.
   Each such slot has a nonzero value in this structure.
   The value has only one nonzero bit.

   When a buffer has its own local value for a slot,
   the entry for that slot (found in the same slot in this structure)
   is turned on in the buffer's local_flags array.

   If a slot in this structure is zero, then even though there may
   be a Lisp-level local variable for the slot, it has no default value,
   and the corresponding slot in buffer_defaults is not used.  */


extern struct buffer buffer_local_flags;

/* For each buffer slot, this points to the Lisp symbol name
   for that slot in the current buffer.  It is 0 for slots
   that don't have such names.  */

extern struct buffer buffer_local_symbols;

extern void delete_all_overlays (struct buffer *);
extern void reset_buffer (struct buffer *);
extern void evaporate_overlays (EMACS_INT);
extern ptrdiff_t overlays_at (EMACS_INT pos, int extend, Lisp_Object **vec_ptr,
			      ptrdiff_t *len_ptr, EMACS_INT *next_ptr,
			      EMACS_INT *prev_ptr, int change_req);
extern ptrdiff_t sort_overlays (Lisp_Object *, ptrdiff_t, struct window *);
extern void recenter_overlay_lists (struct buffer *, EMACS_INT);
extern EMACS_INT overlay_strings (EMACS_INT, struct window *, unsigned char **);
extern void validate_region (Lisp_Object *, Lisp_Object *);
extern void set_buffer_internal (struct buffer *);
extern void set_buffer_internal_1 (struct buffer *);
extern void set_buffer_temp (struct buffer *);
extern void record_buffer (Lisp_Object);
extern void buffer_slot_type_mismatch (Lisp_Object, int) NO_RETURN;
extern void fix_overlays_before (struct buffer *, EMACS_INT, EMACS_INT);
extern void mmap_set_vars (int);

/* Get overlays at POSN into array OVERLAYS with NOVERLAYS elements.
   If NEXTP is non-NULL, return next overlay there.
   See overlay_at arg CHANGE_REQ for meaning of CHRQ arg.  */

#define GET_OVERLAYS_AT(posn, overlays, noverlays, nextp, chrq)		\
  do {									\
    ptrdiff_t maxlen = 40;							\
    overlays = (Lisp_Object *) alloca (maxlen * sizeof (Lisp_Object));	\
    noverlays = overlays_at (posn, 0, &overlays, &maxlen,		\
			     nextp, NULL, chrq);				\
    if (noverlays > maxlen)						\
      {									\
	maxlen = noverlays;						\
	overlays = (Lisp_Object *) alloca (maxlen * sizeof (Lisp_Object)); \
	noverlays = overlays_at (posn, 0, &overlays, &maxlen,		\
				 nextp, NULL, chrq);			\
      }									\
  } while (0)

EXFUN (Fbuffer_live_p, 1);
EXFUN (Fbuffer_name, 1);
EXFUN (Fnext_overlay_change, 1);
EXFUN (Fbuffer_local_value, 2);

extern Lisp_Object Qbefore_change_functions;
extern Lisp_Object Qafter_change_functions;
extern Lisp_Object Qfirst_change_hook;


/* Overlays */

/* 1 if the OV is an overlay object.  */

#define OVERLAY_VALID(OV) (OVERLAYP (OV))

/* Return the marker that stands for where OV starts in the buffer.  */

#define OVERLAY_START(OV) (XOVERLAY (OV)->start)

/* Return the marker that stands for where OV ends in the buffer.  */

#define OVERLAY_END(OV) (XOVERLAY (OV)->end)

/* Return the plist of overlay OV.  */

#define OVERLAY_PLIST(OV) XOVERLAY ((OV))->plist

/* Return the actual buffer position for the marker P.
   We assume you know which buffer it's pointing into.  */

#define OVERLAY_POSITION(P) \
 (MARKERP (P) ? marker_position (P) : (abort (), 0))


/***********************************************************************
			Buffer-local Variables
 ***********************************************************************/

/* Number of per-buffer variables used.  */

extern int last_per_buffer_idx;

/* Return the offset in bytes of member VAR of struct buffer
   from the start of a buffer structure.  */

#define PER_BUFFER_VAR_OFFSET(VAR) \
  offsetof (struct buffer, BUFFER_INTERNAL_FIELD (VAR))

/* Return the index of buffer-local variable VAR.  Each per-buffer
   variable has an index > 0 associated with it, except when it always
   has buffer-local values, in which case the index is -1.  If this is
   0, this is a bug and means that the slot of VAR in
   buffer_local_flags wasn't initialized.  */

#define PER_BUFFER_VAR_IDX(VAR) \
    PER_BUFFER_IDX (PER_BUFFER_VAR_OFFSET (VAR))

/* Value is non-zero if the variable with index IDX has a local value
   in buffer B.  */

#define PER_BUFFER_VALUE_P(B, IDX)		\
    (((IDX) < 0 || IDX >= last_per_buffer_idx)	\
     ? (abort (), 0)				\
     : ((B)->local_flags[IDX] != 0))

/* Set whether per-buffer variable with index IDX has a buffer-local
   value in buffer B.  VAL zero means it hasn't.  */

#define SET_PER_BUFFER_VALUE_P(B, IDX, VAL)	\
     do {						\
       if ((IDX) < 0 || (IDX) >= last_per_buffer_idx)	\
	 abort ();					\
       (B)->local_flags[IDX] = (VAL);			\
     } while (0)

/* Return the index value of the per-buffer variable at offset OFFSET
   in the buffer structure.

   If the slot OFFSET has a corresponding default value in
   buffer_defaults, the index value is positive and has only one
   nonzero bit.  When a buffer has its own local value for a slot, the
   bit for that slot (found in the same slot in this structure) is
   turned on in the buffer's local_flags array.

   If the index value is -1, even though there may be a
   DEFVAR_PER_BUFFER for the slot, there is no default value for it;
   and the corresponding slot in buffer_defaults is not used.

   If the index value is -2, then there is no DEFVAR_PER_BUFFER for
   the slot, but there is a default value which is copied into each
   new buffer.

   If a slot in this structure corresponding to a DEFVAR_PER_BUFFER is
   zero, that is a bug */


#define PER_BUFFER_IDX(OFFSET) \
      XINT (*(Lisp_Object *)((OFFSET) + (char *) &buffer_local_flags))

/* Return the default value of the per-buffer variable at offset
   OFFSET in the buffer structure.  */

#define PER_BUFFER_DEFAULT(OFFSET) \
      (*(Lisp_Object *)((OFFSET) + (char *) &buffer_defaults))

/* Return the buffer-local value of the per-buffer variable at offset
   OFFSET in the buffer structure.  */

#define PER_BUFFER_VALUE(BUFFER, OFFSET) \
      (*(Lisp_Object *)((OFFSET) + (char *) (BUFFER)))

/* Downcase a character C, or make no change if that cannot be done.  */
static inline int
downcase (int c)
{
  Lisp_Object downcase_table = BVAR (current_buffer, downcase_table);
  Lisp_Object down = CHAR_TABLE_REF (downcase_table, c);
  return NATNUMP (down) ? XFASTINT (down) : c;
}

/* 1 if C is upper case.  */
static inline int uppercasep (int c) { return downcase (c) != c; }

/* Upcase a character C known to be not upper case.  */
static inline int
upcase1 (int c)
{
  Lisp_Object upcase_table = BVAR (current_buffer, upcase_table);
  Lisp_Object up = CHAR_TABLE_REF (upcase_table, c);
  return NATNUMP (up) ? XFASTINT (up) : c;
}

/* 1 if C is lower case.  */
static inline int lowercasep (int c)
{ return !uppercasep (c) && upcase1 (c) != c; }

/* Upcase a character C, or make no change if that cannot be done.  */
static inline int upcase (int c) { return uppercasep (c) ? c : upcase1 (c); }
