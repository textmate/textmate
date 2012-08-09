/* Lisp functions pertaining to editing.

Copyright (C) 1985-1987, 1989, 1993-2012 Free Software Foundation, Inc.

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
#include <sys/types.h>
#include <stdio.h>
#include <setjmp.h>

#ifdef HAVE_PWD_H
#include <pwd.h>
#endif

#include <unistd.h>

#ifdef HAVE_SYS_UTSNAME_H
#include <sys/utsname.h>
#endif

#include "lisp.h"

/* systime.h includes <sys/time.h> which, on some systems, is required
   for <sys/resource.h>; thus systime.h must be included before
   <sys/resource.h> */
#include "systime.h"

#if defined HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif

#include <ctype.h>
#include <float.h>
#include <limits.h>
#include <intprops.h>
#include <strftime.h>
#include <verify.h>

#include "intervals.h"
#include "buffer.h"
#include "character.h"
#include "coding.h"
#include "frame.h"
#include "window.h"
#include "blockinput.h"

#ifndef NULL
#define NULL 0
#endif

#ifndef USER_FULL_NAME
#define USER_FULL_NAME pw->pw_gecos
#endif

#ifndef USE_CRT_DLL
extern char **environ;
#endif

#define TM_YEAR_BASE 1900

/* Nonzero if TM_YEAR is a struct tm's tm_year value that causes
   asctime to have well-defined behavior.  */
#ifndef TM_YEAR_IN_ASCTIME_RANGE
# define TM_YEAR_IN_ASCTIME_RANGE(tm_year) \
    (1000 - TM_YEAR_BASE <= (tm_year) && (tm_year) <= 9999 - TM_YEAR_BASE)
#endif

#ifdef WINDOWSNT
extern Lisp_Object w32_get_internal_run_time (void);
#endif

static void time_overflow (void) NO_RETURN;
static Lisp_Object format_time_string (char const *, ptrdiff_t, Lisp_Object,
				       int, time_t *, struct tm **);
static int tm_diff (struct tm *, struct tm *);
static void update_buffer_properties (EMACS_INT, EMACS_INT);

static Lisp_Object Qbuffer_access_fontify_functions;
static Lisp_Object Fuser_full_name (Lisp_Object);

/* Symbol for the text property used to mark fields.  */

Lisp_Object Qfield;

/* A special value for Qfield properties.  */

static Lisp_Object Qboundary;


void
init_editfns (void)
{
  const char *user_name;
  register char *p;
  struct passwd *pw;	/* password entry for the current user */
  Lisp_Object tem;

  /* Set up system_name even when dumping.  */
  init_system_name ();

#ifndef CANNOT_DUMP
  /* Don't bother with this on initial start when just dumping out */
  if (!initialized)
    return;
#endif /* not CANNOT_DUMP */

  pw = getpwuid (getuid ());
#ifdef MSDOS
  /* We let the real user name default to "root" because that's quite
     accurate on MSDOG and because it lets Emacs find the init file.
     (The DVX libraries override the Djgpp libraries here.)  */
  Vuser_real_login_name = build_string (pw ? pw->pw_name : "root");
#else
  Vuser_real_login_name = build_string (pw ? pw->pw_name : "unknown");
#endif

  /* Get the effective user name, by consulting environment variables,
     or the effective uid if those are unset.  */
  user_name = getenv ("LOGNAME");
  if (!user_name)
#ifdef WINDOWSNT
    user_name = getenv ("USERNAME");	/* it's USERNAME on NT */
#else  /* WINDOWSNT */
    user_name = getenv ("USER");
#endif /* WINDOWSNT */
  if (!user_name)
    {
      pw = getpwuid (geteuid ());
      user_name = pw ? pw->pw_name : "unknown";
    }
  Vuser_login_name = build_string (user_name);

  /* If the user name claimed in the environment vars differs from
     the real uid, use the claimed name to find the full name.  */
  tem = Fstring_equal (Vuser_login_name, Vuser_real_login_name);
  Vuser_full_name = Fuser_full_name (NILP (tem)? make_number (geteuid ())
				     : Vuser_login_name);

  p = getenv ("NAME");
  if (p)
    Vuser_full_name = build_string (p);
  else if (NILP (Vuser_full_name))
    Vuser_full_name = build_string ("unknown");

#ifdef HAVE_SYS_UTSNAME_H
  {
    struct utsname uts;
    uname (&uts);
    Voperating_system_release = build_string (uts.release);
  }
#else
  Voperating_system_release = Qnil;
#endif
}

DEFUN ("char-to-string", Fchar_to_string, Schar_to_string, 1, 1, 0,
       doc: /* Convert arg CHAR to a string containing that character.
usage: (char-to-string CHAR)  */)
  (Lisp_Object character)
{
  int c, len;
  unsigned char str[MAX_MULTIBYTE_LENGTH];

  CHECK_CHARACTER (character);
  c = XFASTINT (character);

  len = CHAR_STRING (c, str);
  return make_string_from_bytes ((char *) str, 1, len);
}

DEFUN ("byte-to-string", Fbyte_to_string, Sbyte_to_string, 1, 1, 0,
       doc: /* Convert arg BYTE to a unibyte string containing that byte.  */)
  (Lisp_Object byte)
{
  unsigned char b;
  CHECK_NUMBER (byte);
  if (XINT (byte) < 0 || XINT (byte) > 255)
    error ("Invalid byte");
  b = XINT (byte);
  return make_string_from_bytes ((char *) &b, 1, 1);
}

DEFUN ("string-to-char", Fstring_to_char, Sstring_to_char, 1, 1, 0,
       doc: /* Return the first character in STRING.  */)
  (register Lisp_Object string)
{
  register Lisp_Object val;
  CHECK_STRING (string);
  if (SCHARS (string))
    {
      if (STRING_MULTIBYTE (string))
	XSETFASTINT (val, STRING_CHAR (SDATA (string)));
      else
	XSETFASTINT (val, SREF (string, 0));
    }
  else
    XSETFASTINT (val, 0);
  return val;
}

static Lisp_Object
buildmark (EMACS_INT charpos, EMACS_INT bytepos)
{
  register Lisp_Object mark;
  mark = Fmake_marker ();
  set_marker_both (mark, Qnil, charpos, bytepos);
  return mark;
}

DEFUN ("point", Fpoint, Spoint, 0, 0, 0,
       doc: /* Return value of point, as an integer.
Beginning of buffer is position (point-min).  */)
  (void)
{
  Lisp_Object temp;
  XSETFASTINT (temp, PT);
  return temp;
}

DEFUN ("point-marker", Fpoint_marker, Spoint_marker, 0, 0, 0,
       doc: /* Return value of point, as a marker object.  */)
  (void)
{
  return buildmark (PT, PT_BYTE);
}

EMACS_INT
clip_to_bounds (EMACS_INT lower, EMACS_INT num, EMACS_INT upper)
{
  if (num < lower)
    return lower;
  else if (num > upper)
    return upper;
  else
    return num;
}

DEFUN ("goto-char", Fgoto_char, Sgoto_char, 1, 1, "NGoto char: ",
       doc: /* Set point to POSITION, a number or marker.
Beginning of buffer is position (point-min), end is (point-max).

The return value is POSITION.  */)
  (register Lisp_Object position)
{
  EMACS_INT pos;

  if (MARKERP (position)
      && current_buffer == XMARKER (position)->buffer)
    {
      pos = marker_position (position);
      if (pos < BEGV)
	SET_PT_BOTH (BEGV, BEGV_BYTE);
      else if (pos > ZV)
	SET_PT_BOTH (ZV, ZV_BYTE);
      else
	SET_PT_BOTH (pos, marker_byte_position (position));

      return position;
    }

  CHECK_NUMBER_COERCE_MARKER (position);

  pos = clip_to_bounds (BEGV, XINT (position), ZV);
  SET_PT (pos);
  return position;
}


/* Return the start or end position of the region.
   BEGINNINGP non-zero means return the start.
   If there is no region active, signal an error. */

static Lisp_Object
region_limit (int beginningp)
{
  Lisp_Object m;

  if (!NILP (Vtransient_mark_mode)
      && NILP (Vmark_even_if_inactive)
      && NILP (BVAR (current_buffer, mark_active)))
    xsignal0 (Qmark_inactive);

  m = Fmarker_position (BVAR (current_buffer, mark));
  if (NILP (m))
    error ("The mark is not set now, so there is no region");

  if ((PT < XFASTINT (m)) == (beginningp != 0))
    m = make_number (PT);
  return m;
}

DEFUN ("region-beginning", Fregion_beginning, Sregion_beginning, 0, 0, 0,
       doc: /* Return the integer value of point or mark, whichever is smaller.  */)
  (void)
{
  return region_limit (1);
}

DEFUN ("region-end", Fregion_end, Sregion_end, 0, 0, 0,
       doc: /* Return the integer value of point or mark, whichever is larger.  */)
  (void)
{
  return region_limit (0);
}

DEFUN ("mark-marker", Fmark_marker, Smark_marker, 0, 0, 0,
       doc: /* Return this buffer's mark, as a marker object.
Watch out!  Moving this marker changes the mark position.
If you set the marker not to point anywhere, the buffer will have no mark.  */)
  (void)
{
  return BVAR (current_buffer, mark);
}


/* Find all the overlays in the current buffer that touch position POS.
   Return the number found, and store them in a vector in VEC
   of length LEN.  */

static ptrdiff_t
overlays_around (EMACS_INT pos, Lisp_Object *vec, ptrdiff_t len)
{
  Lisp_Object overlay, start, end;
  struct Lisp_Overlay *tail;
  EMACS_INT startpos, endpos;
  ptrdiff_t idx = 0;

  for (tail = current_buffer->overlays_before; tail; tail = tail->next)
    {
      XSETMISC (overlay, tail);

      end = OVERLAY_END (overlay);
      endpos = OVERLAY_POSITION (end);
      if (endpos < pos)
	  break;
      start = OVERLAY_START (overlay);
      startpos = OVERLAY_POSITION (start);
      if (startpos <= pos)
	{
	  if (idx < len)
	    vec[idx] = overlay;
	  /* Keep counting overlays even if we can't return them all.  */
	  idx++;
	}
    }

  for (tail = current_buffer->overlays_after; tail; tail = tail->next)
    {
      XSETMISC (overlay, tail);

      start = OVERLAY_START (overlay);
      startpos = OVERLAY_POSITION (start);
      if (pos < startpos)
	break;
      end = OVERLAY_END (overlay);
      endpos = OVERLAY_POSITION (end);
      if (pos <= endpos)
	{
	  if (idx < len)
	    vec[idx] = overlay;
	  idx++;
	}
    }

  return idx;
}

/* Return the value of property PROP, in OBJECT at POSITION.
   It's the value of PROP that a char inserted at POSITION would get.
   OBJECT is optional and defaults to the current buffer.
   If OBJECT is a buffer, then overlay properties are considered as well as
   text properties.
   If OBJECT is a window, then that window's buffer is used, but
   window-specific overlays are considered only if they are associated
   with OBJECT. */
Lisp_Object
get_pos_property (Lisp_Object position, register Lisp_Object prop, Lisp_Object object)
{
  CHECK_NUMBER_COERCE_MARKER (position);

  if (NILP (object))
    XSETBUFFER (object, current_buffer);
  else if (WINDOWP (object))
    object = XWINDOW (object)->buffer;

  if (!BUFFERP (object))
    /* pos-property only makes sense in buffers right now, since strings
       have no overlays and no notion of insertion for which stickiness
       could be obeyed.  */
    return Fget_text_property (position, prop, object);
  else
    {
      EMACS_INT posn = XINT (position);
      ptrdiff_t noverlays;
      Lisp_Object *overlay_vec, tem;
      struct buffer *obuf = current_buffer;

      set_buffer_temp (XBUFFER (object));

      /* First try with room for 40 overlays.  */
      noverlays = 40;
      overlay_vec = (Lisp_Object *) alloca (noverlays * sizeof (Lisp_Object));
      noverlays = overlays_around (posn, overlay_vec, noverlays);

      /* If there are more than 40,
	 make enough space for all, and try again.  */
      if (noverlays > 40)
	{
	  overlay_vec = (Lisp_Object *) alloca (noverlays * sizeof (Lisp_Object));
	  noverlays = overlays_around (posn, overlay_vec, noverlays);
	}
      noverlays = sort_overlays (overlay_vec, noverlays, NULL);

      set_buffer_temp (obuf);

      /* Now check the overlays in order of decreasing priority.  */
      while (--noverlays >= 0)
	{
	  Lisp_Object ol = overlay_vec[noverlays];
	  tem = Foverlay_get (ol, prop);
	  if (!NILP (tem))
	    {
	      /* Check the overlay is indeed active at point.  */
	      Lisp_Object start = OVERLAY_START (ol), finish = OVERLAY_END (ol);
	      if ((OVERLAY_POSITION (start) == posn
		   && XMARKER (start)->insertion_type == 1)
		  || (OVERLAY_POSITION (finish) == posn
		      && XMARKER (finish)->insertion_type == 0))
		; /* The overlay will not cover a char inserted at point.  */
	      else
		{
		  return tem;
		}
	    }
	}

      { /* Now check the text properties.  */
	int stickiness = text_property_stickiness (prop, position, object);
	if (stickiness > 0)
	  return Fget_text_property (position, prop, object);
	else if (stickiness < 0
		 && XINT (position) > BUF_BEGV (XBUFFER (object)))
	  return Fget_text_property (make_number (XINT (position) - 1),
				     prop, object);
	else
	  return Qnil;
      }
    }
}

/* Find the field surrounding POS in *BEG and *END.  If POS is nil,
   the value of point is used instead.  If BEG or END is null,
   means don't store the beginning or end of the field.

   BEG_LIMIT and END_LIMIT serve to limit the ranged of the returned
   results; they do not effect boundary behavior.

   If MERGE_AT_BOUNDARY is nonzero, then if POS is at the very first
   position of a field, then the beginning of the previous field is
   returned instead of the beginning of POS's field (since the end of a
   field is actually also the beginning of the next input field, this
   behavior is sometimes useful).  Additionally in the MERGE_AT_BOUNDARY
   true case, if two fields are separated by a field with the special
   value `boundary', and POS lies within it, then the two separated
   fields are considered to be adjacent, and POS between them, when
   finding the beginning and ending of the "merged" field.

   Either BEG or END may be 0, in which case the corresponding value
   is not stored.  */

static void
find_field (Lisp_Object pos, Lisp_Object merge_at_boundary,
	    Lisp_Object beg_limit,
	    EMACS_INT *beg, Lisp_Object end_limit, EMACS_INT *end)
{
  /* Fields right before and after the point.  */
  Lisp_Object before_field, after_field;
  /* 1 if POS counts as the start of a field.  */
  int at_field_start = 0;
  /* 1 if POS counts as the end of a field.  */
  int at_field_end = 0;

  if (NILP (pos))
    XSETFASTINT (pos, PT);
  else
    CHECK_NUMBER_COERCE_MARKER (pos);

  after_field
    = get_char_property_and_overlay (pos, Qfield, Qnil, NULL);
  before_field
    = (XFASTINT (pos) > BEGV
       ? get_char_property_and_overlay (make_number (XINT (pos) - 1),
					Qfield, Qnil, NULL)
       /* Using nil here would be a more obvious choice, but it would
          fail when the buffer starts with a non-sticky field.  */
       : after_field);

  /* See if we need to handle the case where MERGE_AT_BOUNDARY is nil
     and POS is at beginning of a field, which can also be interpreted
     as the end of the previous field.  Note that the case where if
     MERGE_AT_BOUNDARY is non-nil (see function comment) is actually the
     more natural one; then we avoid treating the beginning of a field
     specially.  */
  if (NILP (merge_at_boundary))
    {
      Lisp_Object field = get_pos_property (pos, Qfield, Qnil);
      if (!EQ (field, after_field))
	at_field_end = 1;
      if (!EQ (field, before_field))
	at_field_start = 1;
      if (NILP (field) && at_field_start && at_field_end)
	/* If an inserted char would have a nil field while the surrounding
	   text is non-nil, we're probably not looking at a
	   zero-length field, but instead at a non-nil field that's
	   not intended for editing (such as comint's prompts).  */
	at_field_end = at_field_start = 0;
    }

  /* Note about special `boundary' fields:

     Consider the case where the point (`.') is between the fields `x' and `y':

	xxxx.yyyy

     In this situation, if merge_at_boundary is true, we consider the
     `x' and `y' fields as forming one big merged field, and so the end
     of the field is the end of `y'.

     However, if `x' and `y' are separated by a special `boundary' field
     (a field with a `field' char-property of 'boundary), then we ignore
     this special field when merging adjacent fields.  Here's the same
     situation, but with a `boundary' field between the `x' and `y' fields:

	xxx.BBBByyyy

     Here, if point is at the end of `x', the beginning of `y', or
     anywhere in-between (within the `boundary' field), we merge all
     three fields and consider the beginning as being the beginning of
     the `x' field, and the end as being the end of the `y' field.  */

  if (beg)
    {
      if (at_field_start)
	/* POS is at the edge of a field, and we should consider it as
	   the beginning of the following field.  */
	*beg = XFASTINT (pos);
      else
	/* Find the previous field boundary.  */
	{
	  Lisp_Object p = pos;
	  if (!NILP (merge_at_boundary) && EQ (before_field, Qboundary))
	    /* Skip a `boundary' field.  */
	    p = Fprevious_single_char_property_change (p, Qfield, Qnil,
						       beg_limit);

	  p = Fprevious_single_char_property_change (p, Qfield, Qnil,
						     beg_limit);
	  *beg = NILP (p) ? BEGV : XFASTINT (p);
	}
    }

  if (end)
    {
      if (at_field_end)
	/* POS is at the edge of a field, and we should consider it as
	   the end of the previous field.  */
	*end = XFASTINT (pos);
      else
	/* Find the next field boundary.  */
	{
	  if (!NILP (merge_at_boundary) && EQ (after_field, Qboundary))
	    /* Skip a `boundary' field.  */
	    pos = Fnext_single_char_property_change (pos, Qfield, Qnil,
						     end_limit);

	  pos = Fnext_single_char_property_change (pos, Qfield, Qnil,
						   end_limit);
	  *end = NILP (pos) ? ZV : XFASTINT (pos);
	}
    }
}


DEFUN ("delete-field", Fdelete_field, Sdelete_field, 0, 1, 0,
       doc: /* Delete the field surrounding POS.
A field is a region of text with the same `field' property.
If POS is nil, the value of point is used for POS.  */)
  (Lisp_Object pos)
{
  EMACS_INT beg, end;
  find_field (pos, Qnil, Qnil, &beg, Qnil, &end);
  if (beg != end)
    del_range (beg, end);
  return Qnil;
}

DEFUN ("field-string", Ffield_string, Sfield_string, 0, 1, 0,
       doc: /* Return the contents of the field surrounding POS as a string.
A field is a region of text with the same `field' property.
If POS is nil, the value of point is used for POS.  */)
  (Lisp_Object pos)
{
  EMACS_INT beg, end;
  find_field (pos, Qnil, Qnil, &beg, Qnil, &end);
  return make_buffer_string (beg, end, 1);
}

DEFUN ("field-string-no-properties", Ffield_string_no_properties, Sfield_string_no_properties, 0, 1, 0,
       doc: /* Return the contents of the field around POS, without text properties.
A field is a region of text with the same `field' property.
If POS is nil, the value of point is used for POS.  */)
  (Lisp_Object pos)
{
  EMACS_INT beg, end;
  find_field (pos, Qnil, Qnil, &beg, Qnil, &end);
  return make_buffer_string (beg, end, 0);
}

DEFUN ("field-beginning", Ffield_beginning, Sfield_beginning, 0, 3, 0,
       doc: /* Return the beginning of the field surrounding POS.
A field is a region of text with the same `field' property.
If POS is nil, the value of point is used for POS.
If ESCAPE-FROM-EDGE is non-nil and POS is at the beginning of its
field, then the beginning of the *previous* field is returned.
If LIMIT is non-nil, it is a buffer position; if the beginning of the field
is before LIMIT, then LIMIT will be returned instead.  */)
  (Lisp_Object pos, Lisp_Object escape_from_edge, Lisp_Object limit)
{
  EMACS_INT beg;
  find_field (pos, escape_from_edge, limit, &beg, Qnil, 0);
  return make_number (beg);
}

DEFUN ("field-end", Ffield_end, Sfield_end, 0, 3, 0,
       doc: /* Return the end of the field surrounding POS.
A field is a region of text with the same `field' property.
If POS is nil, the value of point is used for POS.
If ESCAPE-FROM-EDGE is non-nil and POS is at the end of its field,
then the end of the *following* field is returned.
If LIMIT is non-nil, it is a buffer position; if the end of the field
is after LIMIT, then LIMIT will be returned instead.  */)
  (Lisp_Object pos, Lisp_Object escape_from_edge, Lisp_Object limit)
{
  EMACS_INT end;
  find_field (pos, escape_from_edge, Qnil, 0, limit, &end);
  return make_number (end);
}

DEFUN ("constrain-to-field", Fconstrain_to_field, Sconstrain_to_field, 2, 5, 0,
       doc: /* Return the position closest to NEW-POS that is in the same field as OLD-POS.
A field is a region of text with the same `field' property.

If NEW-POS is nil, then use the current point instead, and move point
to the resulting constrained position, in addition to returning that
position.

If OLD-POS is at the boundary of two fields, then the allowable
positions for NEW-POS depends on the value of the optional argument
ESCAPE-FROM-EDGE: If ESCAPE-FROM-EDGE is nil, then NEW-POS is
constrained to the field that has the same `field' char-property
as any new characters inserted at OLD-POS, whereas if ESCAPE-FROM-EDGE
is non-nil, NEW-POS is constrained to the union of the two adjacent
fields.  Additionally, if two fields are separated by another field with
the special value `boundary', then any point within this special field is
also considered to be `on the boundary'.

If the optional argument ONLY-IN-LINE is non-nil and constraining
NEW-POS would move it to a different line, NEW-POS is returned
unconstrained.  This useful for commands that move by line, like
\\[next-line] or \\[beginning-of-line], which should generally respect field boundaries
only in the case where they can still move to the right line.

If the optional argument INHIBIT-CAPTURE-PROPERTY is non-nil, and OLD-POS has
a non-nil property of that name, then any field boundaries are ignored.

Field boundaries are not noticed if `inhibit-field-text-motion' is non-nil.  */)
  (Lisp_Object new_pos, Lisp_Object old_pos, Lisp_Object escape_from_edge, Lisp_Object only_in_line, Lisp_Object inhibit_capture_property)
{
  /* If non-zero, then the original point, before re-positioning.  */
  EMACS_INT orig_point = 0;
  int fwd;
  Lisp_Object prev_old, prev_new;

  if (NILP (new_pos))
    /* Use the current point, and afterwards, set it.  */
    {
      orig_point = PT;
      XSETFASTINT (new_pos, PT);
    }

  CHECK_NUMBER_COERCE_MARKER (new_pos);
  CHECK_NUMBER_COERCE_MARKER (old_pos);

  fwd = (XFASTINT (new_pos) > XFASTINT (old_pos));

  prev_old = make_number (XFASTINT (old_pos) - 1);
  prev_new = make_number (XFASTINT (new_pos) - 1);

  if (NILP (Vinhibit_field_text_motion)
      && !EQ (new_pos, old_pos)
      && (!NILP (Fget_char_property (new_pos, Qfield, Qnil))
          || !NILP (Fget_char_property (old_pos, Qfield, Qnil))
          /* To recognize field boundaries, we must also look at the
             previous positions; we could use `get_pos_property'
             instead, but in itself that would fail inside non-sticky
             fields (like comint prompts).  */
          || (XFASTINT (new_pos) > BEGV
              && !NILP (Fget_char_property (prev_new, Qfield, Qnil)))
          || (XFASTINT (old_pos) > BEGV
              && !NILP (Fget_char_property (prev_old, Qfield, Qnil))))
      && (NILP (inhibit_capture_property)
          /* Field boundaries are again a problem; but now we must
             decide the case exactly, so we need to call
             `get_pos_property' as well.  */
          || (NILP (get_pos_property (old_pos, inhibit_capture_property, Qnil))
              && (XFASTINT (old_pos) <= BEGV
                  || NILP (Fget_char_property (old_pos, inhibit_capture_property, Qnil))
                  || NILP (Fget_char_property (prev_old, inhibit_capture_property, Qnil))))))
    /* It is possible that NEW_POS is not within the same field as
       OLD_POS; try to move NEW_POS so that it is.  */
    {
      EMACS_INT shortage;
      Lisp_Object field_bound;

      if (fwd)
	field_bound = Ffield_end (old_pos, escape_from_edge, new_pos);
      else
	field_bound = Ffield_beginning (old_pos, escape_from_edge, new_pos);

      if (/* See if ESCAPE_FROM_EDGE caused FIELD_BOUND to jump to the
             other side of NEW_POS, which would mean that NEW_POS is
             already acceptable, and it's not necessary to constrain it
             to FIELD_BOUND.  */
	  ((XFASTINT (field_bound) < XFASTINT (new_pos)) ? fwd : !fwd)
	  /* NEW_POS should be constrained, but only if either
	     ONLY_IN_LINE is nil (in which case any constraint is OK),
	     or NEW_POS and FIELD_BOUND are on the same line (in which
	     case the constraint is OK even if ONLY_IN_LINE is non-nil). */
	  && (NILP (only_in_line)
	      /* This is the ONLY_IN_LINE case, check that NEW_POS and
		 FIELD_BOUND are on the same line by seeing whether
		 there's an intervening newline or not.  */
	      || (scan_buffer ('\n',
			       XFASTINT (new_pos), XFASTINT (field_bound),
			       fwd ? -1 : 1, &shortage, 1),
		  shortage != 0)))
	/* Constrain NEW_POS to FIELD_BOUND.  */
	new_pos = field_bound;

      if (orig_point && XFASTINT (new_pos) != orig_point)
	/* The NEW_POS argument was originally nil, so automatically set PT. */
	SET_PT (XFASTINT (new_pos));
    }

  return new_pos;
}


DEFUN ("line-beginning-position",
       Fline_beginning_position, Sline_beginning_position, 0, 1, 0,
       doc: /* Return the character position of the first character on the current line.
With argument N not nil or 1, move forward N - 1 lines first.
If scan reaches end of buffer, return that position.

The returned position is of the first character in the logical order,
i.e. the one that has the smallest character position.

This function constrains the returned position to the current field
unless that would be on a different line than the original,
unconstrained result.  If N is nil or 1, and a front-sticky field
starts at point, the scan stops as soon as it starts.  To ignore field
boundaries bind `inhibit-field-text-motion' to t.

This function does not move point.  */)
  (Lisp_Object n)
{
  EMACS_INT orig, orig_byte, end;
  int count = SPECPDL_INDEX ();
  specbind (Qinhibit_point_motion_hooks, Qt);

  if (NILP (n))
    XSETFASTINT (n, 1);
  else
    CHECK_NUMBER (n);

  orig = PT;
  orig_byte = PT_BYTE;
  Fforward_line (make_number (XINT (n) - 1));
  end = PT;

  SET_PT_BOTH (orig, orig_byte);

  unbind_to (count, Qnil);

  /* Return END constrained to the current input field.  */
  return Fconstrain_to_field (make_number (end), make_number (orig),
			      XINT (n) != 1 ? Qt : Qnil,
			      Qt, Qnil);
}

DEFUN ("line-end-position", Fline_end_position, Sline_end_position, 0, 1, 0,
       doc: /* Return the character position of the last character on the current line.
With argument N not nil or 1, move forward N - 1 lines first.
If scan reaches end of buffer, return that position.

The returned position is of the last character in the logical order,
i.e. the character whose buffer position is the largest one.

This function constrains the returned position to the current field
unless that would be on a different line than the original,
unconstrained result.  If N is nil or 1, and a rear-sticky field ends
at point, the scan stops as soon as it starts.  To ignore field
boundaries bind `inhibit-field-text-motion' to t.

This function does not move point.  */)
  (Lisp_Object n)
{
  EMACS_INT end_pos;
  EMACS_INT orig = PT;

  if (NILP (n))
    XSETFASTINT (n, 1);
  else
    CHECK_NUMBER (n);

  end_pos = find_before_next_newline (orig, 0, XINT (n) - (XINT (n) <= 0));

  /* Return END_POS constrained to the current input field.  */
  return Fconstrain_to_field (make_number (end_pos), make_number (orig),
			      Qnil, Qt, Qnil);
}


Lisp_Object
save_excursion_save (void)
{
  int visible = (XBUFFER (XWINDOW (selected_window)->buffer)
		 == current_buffer);

  return Fcons (Fpoint_marker (),
		Fcons (Fcopy_marker (BVAR (current_buffer, mark), Qnil),
		       Fcons (visible ? Qt : Qnil,
			      Fcons (BVAR (current_buffer, mark_active),
				     selected_window))));
}

Lisp_Object
save_excursion_restore (Lisp_Object info)
{
  Lisp_Object tem, tem1, omark, nmark;
  struct gcpro gcpro1, gcpro2, gcpro3;
  int visible_p;

  tem = Fmarker_buffer (XCAR (info));
  /* If buffer being returned to is now deleted, avoid error */
  /* Otherwise could get error here while unwinding to top level
     and crash */
  /* In that case, Fmarker_buffer returns nil now.  */
  if (NILP (tem))
    return Qnil;

  omark = nmark = Qnil;
  GCPRO3 (info, omark, nmark);

  Fset_buffer (tem);

  /* Point marker.  */
  tem = XCAR (info);
  Fgoto_char (tem);
  unchain_marker (XMARKER (tem));

  /* Mark marker.  */
  info = XCDR (info);
  tem = XCAR (info);
  omark = Fmarker_position (BVAR (current_buffer, mark));
  Fset_marker (BVAR (current_buffer, mark), tem, Fcurrent_buffer ());
  nmark = Fmarker_position (tem);
  unchain_marker (XMARKER (tem));

  /* visible */
  info = XCDR (info);
  visible_p = !NILP (XCAR (info));

#if 0 /* We used to make the current buffer visible in the selected window
	 if that was true previously.  That avoids some anomalies.
	 But it creates others, and it wasn't documented, and it is simpler
	 and cleaner never to alter the window/buffer connections.  */
  tem1 = Fcar (tem);
  if (!NILP (tem1)
      && current_buffer != XBUFFER (XWINDOW (selected_window)->buffer))
    Fswitch_to_buffer (Fcurrent_buffer (), Qnil);
#endif /* 0 */

  /* Mark active */
  info = XCDR (info);
  tem = XCAR (info);
  tem1 = BVAR (current_buffer, mark_active);
  BVAR (current_buffer, mark_active) = tem;

  /* If mark is active now, and either was not active
     or was at a different place, run the activate hook.  */
  if (! NILP (tem))
    {
      if (! EQ (omark, nmark))
        {
          tem = intern ("activate-mark-hook");
          Frun_hooks (1, &tem);
        }
    }
  /* If mark has ceased to be active, run deactivate hook.  */
  else if (! NILP (tem1))
    {
      tem = intern ("deactivate-mark-hook");
      Frun_hooks (1, &tem);
    }

  /* If buffer was visible in a window, and a different window was
     selected, and the old selected window is still showing this
     buffer, restore point in that window.  */
  tem = XCDR (info);
  if (visible_p
      && !EQ (tem, selected_window)
      && (tem1 = XWINDOW (tem)->buffer,
	  (/* Window is live...  */
	   BUFFERP (tem1)
	   /* ...and it shows the current buffer.  */
	   && XBUFFER (tem1) == current_buffer)))
    Fset_window_point (tem, make_number (PT));

  UNGCPRO;
  return Qnil;
}

DEFUN ("save-excursion", Fsave_excursion, Ssave_excursion, 0, UNEVALLED, 0,
       doc: /* Save point, mark, and current buffer; execute BODY; restore those things.
Executes BODY just like `progn'.
The values of point, mark and the current buffer are restored
even in case of abnormal exit (throw or error).
The state of activation of the mark is also restored.

This construct does not save `deactivate-mark', and therefore
functions that change the buffer will still cause deactivation
of the mark at the end of the command.  To prevent that, bind
`deactivate-mark' with `let'.

If you only want to save the current buffer but not point nor mark,
then just use `save-current-buffer', or even `with-current-buffer'.

usage: (save-excursion &rest BODY)  */)
  (Lisp_Object args)
{
  register Lisp_Object val;
  int count = SPECPDL_INDEX ();

  record_unwind_protect (save_excursion_restore, save_excursion_save ());

  val = Fprogn (args);
  return unbind_to (count, val);
}

DEFUN ("save-current-buffer", Fsave_current_buffer, Ssave_current_buffer, 0, UNEVALLED, 0,
       doc: /* Save the current buffer; execute BODY; restore the current buffer.
Executes BODY just like `progn'.
usage: (save-current-buffer &rest BODY)  */)
  (Lisp_Object args)
{
  Lisp_Object val;
  int count = SPECPDL_INDEX ();

  record_unwind_protect (set_buffer_if_live, Fcurrent_buffer ());

  val = Fprogn (args);
  return unbind_to (count, val);
}

DEFUN ("buffer-size", Fbufsize, Sbufsize, 0, 1, 0,
       doc: /* Return the number of characters in the current buffer.
If BUFFER, return the number of characters in that buffer instead.  */)
  (Lisp_Object buffer)
{
  if (NILP (buffer))
    return make_number (Z - BEG);
  else
    {
      CHECK_BUFFER (buffer);
      return make_number (BUF_Z (XBUFFER (buffer))
			  - BUF_BEG (XBUFFER (buffer)));
    }
}

DEFUN ("point-min", Fpoint_min, Spoint_min, 0, 0, 0,
       doc: /* Return the minimum permissible value of point in the current buffer.
This is 1, unless narrowing (a buffer restriction) is in effect.  */)
  (void)
{
  Lisp_Object temp;
  XSETFASTINT (temp, BEGV);
  return temp;
}

DEFUN ("point-min-marker", Fpoint_min_marker, Spoint_min_marker, 0, 0, 0,
       doc: /* Return a marker to the minimum permissible value of point in this buffer.
This is the beginning, unless narrowing (a buffer restriction) is in effect.  */)
  (void)
{
  return buildmark (BEGV, BEGV_BYTE);
}

DEFUN ("point-max", Fpoint_max, Spoint_max, 0, 0, 0,
       doc: /* Return the maximum permissible value of point in the current buffer.
This is (1+ (buffer-size)), unless narrowing (a buffer restriction)
is in effect, in which case it is less.  */)
  (void)
{
  Lisp_Object temp;
  XSETFASTINT (temp, ZV);
  return temp;
}

DEFUN ("point-max-marker", Fpoint_max_marker, Spoint_max_marker, 0, 0, 0,
       doc: /* Return a marker to the maximum permissible value of point in this buffer.
This is (1+ (buffer-size)), unless narrowing (a buffer restriction)
is in effect, in which case it is less.  */)
  (void)
{
  return buildmark (ZV, ZV_BYTE);
}

DEFUN ("gap-position", Fgap_position, Sgap_position, 0, 0, 0,
       doc: /* Return the position of the gap, in the current buffer.
See also `gap-size'.  */)
  (void)
{
  Lisp_Object temp;
  XSETFASTINT (temp, GPT);
  return temp;
}

DEFUN ("gap-size", Fgap_size, Sgap_size, 0, 0, 0,
       doc: /* Return the size of the current buffer's gap.
See also `gap-position'.  */)
  (void)
{
  Lisp_Object temp;
  XSETFASTINT (temp, GAP_SIZE);
  return temp;
}

DEFUN ("position-bytes", Fposition_bytes, Sposition_bytes, 1, 1, 0,
       doc: /* Return the byte position for character position POSITION.
If POSITION is out of range, the value is nil.  */)
  (Lisp_Object position)
{
  CHECK_NUMBER_COERCE_MARKER (position);
  if (XINT (position) < BEG || XINT (position) > Z)
    return Qnil;
  return make_number (CHAR_TO_BYTE (XINT (position)));
}

DEFUN ("byte-to-position", Fbyte_to_position, Sbyte_to_position, 1, 1, 0,
       doc: /* Return the character position for byte position BYTEPOS.
If BYTEPOS is out of range, the value is nil.  */)
  (Lisp_Object bytepos)
{
  CHECK_NUMBER (bytepos);
  if (XINT (bytepos) < BEG_BYTE || XINT (bytepos) > Z_BYTE)
    return Qnil;
  return make_number (BYTE_TO_CHAR (XINT (bytepos)));
}

DEFUN ("following-char", Ffollowing_char, Sfollowing_char, 0, 0, 0,
       doc: /* Return the character following point, as a number.
At the end of the buffer or accessible region, return 0.  */)
  (void)
{
  Lisp_Object temp;
  if (PT >= ZV)
    XSETFASTINT (temp, 0);
  else
    XSETFASTINT (temp, FETCH_CHAR (PT_BYTE));
  return temp;
}

DEFUN ("preceding-char", Fprevious_char, Sprevious_char, 0, 0, 0,
       doc: /* Return the character preceding point, as a number.
At the beginning of the buffer or accessible region, return 0.  */)
  (void)
{
  Lisp_Object temp;
  if (PT <= BEGV)
    XSETFASTINT (temp, 0);
  else if (!NILP (BVAR (current_buffer, enable_multibyte_characters)))
    {
      EMACS_INT pos = PT_BYTE;
      DEC_POS (pos);
      XSETFASTINT (temp, FETCH_CHAR (pos));
    }
  else
    XSETFASTINT (temp, FETCH_BYTE (PT_BYTE - 1));
  return temp;
}

DEFUN ("bobp", Fbobp, Sbobp, 0, 0, 0,
       doc: /* Return t if point is at the beginning of the buffer.
If the buffer is narrowed, this means the beginning of the narrowed part.  */)
  (void)
{
  if (PT == BEGV)
    return Qt;
  return Qnil;
}

DEFUN ("eobp", Feobp, Seobp, 0, 0, 0,
       doc: /* Return t if point is at the end of the buffer.
If the buffer is narrowed, this means the end of the narrowed part.  */)
  (void)
{
  if (PT == ZV)
    return Qt;
  return Qnil;
}

DEFUN ("bolp", Fbolp, Sbolp, 0, 0, 0,
       doc: /* Return t if point is at the beginning of a line.  */)
  (void)
{
  if (PT == BEGV || FETCH_BYTE (PT_BYTE - 1) == '\n')
    return Qt;
  return Qnil;
}

DEFUN ("eolp", Feolp, Seolp, 0, 0, 0,
       doc: /* Return t if point is at the end of a line.
`End of a line' includes point being at the end of the buffer.  */)
  (void)
{
  if (PT == ZV || FETCH_BYTE (PT_BYTE) == '\n')
    return Qt;
  return Qnil;
}

DEFUN ("char-after", Fchar_after, Schar_after, 0, 1, 0,
       doc: /* Return character in current buffer at position POS.
POS is an integer or a marker and defaults to point.
If POS is out of range, the value is nil.  */)
  (Lisp_Object pos)
{
  register EMACS_INT pos_byte;

  if (NILP (pos))
    {
      pos_byte = PT_BYTE;
      XSETFASTINT (pos, PT);
    }

  if (MARKERP (pos))
    {
      pos_byte = marker_byte_position (pos);
      if (pos_byte < BEGV_BYTE || pos_byte >= ZV_BYTE)
	return Qnil;
    }
  else
    {
      CHECK_NUMBER_COERCE_MARKER (pos);
      if (XINT (pos) < BEGV || XINT (pos) >= ZV)
	return Qnil;

      pos_byte = CHAR_TO_BYTE (XINT (pos));
    }

  return make_number (FETCH_CHAR (pos_byte));
}

DEFUN ("char-before", Fchar_before, Schar_before, 0, 1, 0,
       doc: /* Return character in current buffer preceding position POS.
POS is an integer or a marker and defaults to point.
If POS is out of range, the value is nil.  */)
  (Lisp_Object pos)
{
  register Lisp_Object val;
  register EMACS_INT pos_byte;

  if (NILP (pos))
    {
      pos_byte = PT_BYTE;
      XSETFASTINT (pos, PT);
    }

  if (MARKERP (pos))
    {
      pos_byte = marker_byte_position (pos);

      if (pos_byte <= BEGV_BYTE || pos_byte > ZV_BYTE)
	return Qnil;
    }
  else
    {
      CHECK_NUMBER_COERCE_MARKER (pos);

      if (XINT (pos) <= BEGV || XINT (pos) > ZV)
	return Qnil;

      pos_byte = CHAR_TO_BYTE (XINT (pos));
    }

  if (!NILP (BVAR (current_buffer, enable_multibyte_characters)))
    {
      DEC_POS (pos_byte);
      XSETFASTINT (val, FETCH_CHAR (pos_byte));
    }
  else
    {
      pos_byte--;
      XSETFASTINT (val, FETCH_BYTE (pos_byte));
    }
   return val;
}

DEFUN ("user-login-name", Fuser_login_name, Suser_login_name, 0, 1, 0,
       doc: /* Return the name under which the user logged in, as a string.
This is based on the effective uid, not the real uid.
Also, if the environment variables LOGNAME or USER are set,
that determines the value of this function.

If optional argument UID is an integer or a float, return the login name
of the user with that uid, or nil if there is no such user.  */)
  (Lisp_Object uid)
{
  struct passwd *pw;
  uid_t id;

  /* Set up the user name info if we didn't do it before.
     (That can happen if Emacs is dumpable
     but you decide to run `temacs -l loadup' and not dump.  */
  if (INTEGERP (Vuser_login_name))
    init_editfns ();

  if (NILP (uid))
    return Vuser_login_name;

  id = XFLOATINT (uid);
  BLOCK_INPUT;
  pw = getpwuid (id);
  UNBLOCK_INPUT;
  return (pw ? build_string (pw->pw_name) : Qnil);
}

DEFUN ("user-real-login-name", Fuser_real_login_name, Suser_real_login_name,
       0, 0, 0,
       doc: /* Return the name of the user's real uid, as a string.
This ignores the environment variables LOGNAME and USER, so it differs from
`user-login-name' when running under `su'.  */)
  (void)
{
  /* Set up the user name info if we didn't do it before.
     (That can happen if Emacs is dumpable
     but you decide to run `temacs -l loadup' and not dump.  */
  if (INTEGERP (Vuser_login_name))
    init_editfns ();
  return Vuser_real_login_name;
}

DEFUN ("user-uid", Fuser_uid, Suser_uid, 0, 0, 0,
       doc: /* Return the effective uid of Emacs.
Value is an integer or a float, depending on the value.  */)
  (void)
{
  /* Assignment to EMACS_INT stops GCC whining about limited range of
     data type.  */
  EMACS_INT euid = geteuid ();

  /* Make sure we don't produce a negative UID due to signed integer
     overflow.  */
  if (euid < 0)
    return make_float (geteuid ());
  return make_fixnum_or_float (euid);
}

DEFUN ("user-real-uid", Fuser_real_uid, Suser_real_uid, 0, 0, 0,
       doc: /* Return the real uid of Emacs.
Value is an integer or a float, depending on the value.  */)
  (void)
{
  /* Assignment to EMACS_INT stops GCC whining about limited range of
     data type.  */
  EMACS_INT uid = getuid ();

  /* Make sure we don't produce a negative UID due to signed integer
     overflow.  */
  if (uid < 0)
    return make_float (getuid ());
  return make_fixnum_or_float (uid);
}

DEFUN ("user-full-name", Fuser_full_name, Suser_full_name, 0, 1, 0,
       doc: /* Return the full name of the user logged in, as a string.
If the full name corresponding to Emacs's userid is not known,
return "unknown".

If optional argument UID is an integer or float, return the full name
of the user with that uid, or nil if there is no such user.
If UID is a string, return the full name of the user with that login
name, or nil if there is no such user.  */)
  (Lisp_Object uid)
{
  struct passwd *pw;
  register char *p, *q;
  Lisp_Object full;

  if (NILP (uid))
    return Vuser_full_name;
  else if (NUMBERP (uid))
    {
      uid_t u = XFLOATINT (uid);
      BLOCK_INPUT;
      pw = getpwuid (u);
      UNBLOCK_INPUT;
    }
  else if (STRINGP (uid))
    {
      BLOCK_INPUT;
      pw = getpwnam (SSDATA (uid));
      UNBLOCK_INPUT;
    }
  else
    error ("Invalid UID specification");

  if (!pw)
    return Qnil;

  p = USER_FULL_NAME;
  /* Chop off everything after the first comma. */
  q = strchr (p, ',');
  full = make_string (p, q ? q - p : strlen (p));

#ifdef AMPERSAND_FULL_NAME
  p = SSDATA (full);
  q = strchr (p, '&');
  /* Substitute the login name for the &, upcasing the first character.  */
  if (q)
    {
      register char *r;
      Lisp_Object login;

      login = Fuser_login_name (make_number (pw->pw_uid));
      r = (char *) alloca (strlen (p) + SCHARS (login) + 1);
      memcpy (r, p, q - p);
      r[q - p] = 0;
      strcat (r, SSDATA (login));
      r[q - p] = upcase ((unsigned char) r[q - p]);
      strcat (r, q + 1);
      full = build_string (r);
    }
#endif /* AMPERSAND_FULL_NAME */

  return full;
}

DEFUN ("system-name", Fsystem_name, Ssystem_name, 0, 0, 0,
       doc: /* Return the host name of the machine you are running on, as a string.  */)
  (void)
{
  return Vsystem_name;
}

const char *
get_system_name (void)
{
  if (STRINGP (Vsystem_name))
    return SSDATA (Vsystem_name);
  else
    return "";
}

DEFUN ("emacs-pid", Femacs_pid, Semacs_pid, 0, 0, 0,
       doc: /* Return the process ID of Emacs, as an integer.  */)
  (void)
{
  return make_number (getpid ());
}



#ifndef TIME_T_MIN
# define TIME_T_MIN TYPE_MINIMUM (time_t)
#endif
#ifndef TIME_T_MAX
# define TIME_T_MAX TYPE_MAXIMUM (time_t)
#endif

/* Report that a time value is out of range for Emacs.  */
static void
time_overflow (void)
{
  error ("Specified time is not representable");
}

/* Return the upper part of the time T (everything but the bottom 16 bits),
   making sure that it is representable.  */
static EMACS_INT
hi_time (time_t t)
{
  time_t hi = t >> 16;

  /* Check for overflow, helping the compiler for common cases where
     no runtime check is needed, and taking care not to convert
     negative numbers to unsigned before comparing them.  */
  if (! ((! TYPE_SIGNED (time_t)
	  || MOST_NEGATIVE_FIXNUM <= TIME_T_MIN >> 16
	  || MOST_NEGATIVE_FIXNUM <= hi)
	 && (TIME_T_MAX >> 16 <= MOST_POSITIVE_FIXNUM
	     || hi <= MOST_POSITIVE_FIXNUM)))
    time_overflow ();

  return hi;
}

/* Return the bottom 16 bits of the time T.  */
static EMACS_INT
lo_time (time_t t)
{
  return t & ((1 << 16) - 1);
}

DEFUN ("current-time", Fcurrent_time, Scurrent_time, 0, 0, 0,
       doc: /* Return the current time, as the number of seconds since 1970-01-01 00:00:00.
The time is returned as a list of three integers.  The first has the
most significant 16 bits of the seconds, while the second has the
least significant 16 bits.  The third integer gives the microsecond
count.

The microsecond count is zero on systems that do not provide
resolution finer than a second.  */)
  (void)
{
  EMACS_TIME t;

  EMACS_GET_TIME (t);
  return list3 (make_number (hi_time (EMACS_SECS (t))),
		make_number (lo_time (EMACS_SECS (t))),
		make_number (EMACS_USECS (t)));
}

DEFUN ("get-internal-run-time", Fget_internal_run_time, Sget_internal_run_time,
       0, 0, 0,
       doc: /* Return the current run time used by Emacs.
The time is returned as a list of three integers.  The first has the
most significant 16 bits of the seconds, while the second has the
least significant 16 bits.  The third integer gives the microsecond
count.

On systems that can't determine the run time, `get-internal-run-time'
does the same thing as `current-time'.  The microsecond count is zero
on systems that do not provide resolution finer than a second.  */)
  (void)
{
#ifdef HAVE_GETRUSAGE
  struct rusage usage;
  time_t secs;
  int usecs;

  if (getrusage (RUSAGE_SELF, &usage) < 0)
    /* This shouldn't happen.  What action is appropriate?  */
    xsignal0 (Qerror);

  /* Sum up user time and system time.  */
  secs = usage.ru_utime.tv_sec + usage.ru_stime.tv_sec;
  usecs = usage.ru_utime.tv_usec + usage.ru_stime.tv_usec;
  if (usecs >= 1000000)
    {
      usecs -= 1000000;
      secs++;
    }

  return list3 (make_number (hi_time (secs)),
		make_number (lo_time (secs)),
		make_number (usecs));
#else /* ! HAVE_GETRUSAGE  */
#ifdef WINDOWSNT
  return w32_get_internal_run_time ();
#else /* ! WINDOWSNT  */
  return Fcurrent_time ();
#endif /* WINDOWSNT  */
#endif /* HAVE_GETRUSAGE  */
}


/* Make a Lisp list that represents the time T.  */
Lisp_Object
make_time (time_t t)
{
  return list2 (make_number (hi_time (t)),
		make_number (lo_time (t)));
}

/* Decode a Lisp list SPECIFIED_TIME that represents a time.
   If SPECIFIED_TIME is nil, use the current time.
   Set *RESULT to seconds since the Epoch.
   If USEC is not null, set *USEC to the microseconds component.
   Return nonzero if successful.  */
int
lisp_time_argument (Lisp_Object specified_time, time_t *result, int *usec)
{
  if (NILP (specified_time))
    {
      if (usec)
        {
          EMACS_TIME t;

          EMACS_GET_TIME (t);
          *usec = EMACS_USECS (t);
          *result = EMACS_SECS (t);
          return 1;
        }
      else
        return time (result) != -1;
    }
  else
    {
      Lisp_Object high, low;
      EMACS_INT hi;
      high = Fcar (specified_time);
      CHECK_NUMBER (high);
      low = Fcdr (specified_time);
      if (CONSP (low))
        {
          if (usec)
            {
              Lisp_Object usec_l = Fcdr (low);
              if (CONSP (usec_l))
                usec_l = Fcar (usec_l);
              if (NILP (usec_l))
                *usec = 0;
              else
                {
                  CHECK_NUMBER (usec_l);
                  *usec = XINT (usec_l);
                }
            }
          low = Fcar (low);
        }
      else if (usec)
        *usec = 0;
      CHECK_NUMBER (low);
      hi = XINT (high);

      /* Check for overflow, helping the compiler for common cases
	 where no runtime check is needed, and taking care not to
	 convert negative numbers to unsigned before comparing them.  */
      if (! ((TYPE_SIGNED (time_t)
	      ? (TIME_T_MIN >> 16 <= MOST_NEGATIVE_FIXNUM
		 || TIME_T_MIN >> 16 <= hi)
	      : 0 <= hi)
	     && (MOST_POSITIVE_FIXNUM <= TIME_T_MAX >> 16
		 || hi <= TIME_T_MAX >> 16)))
	return 0;

      *result = (hi << 16) + (XINT (low) & 0xffff);
      return 1;
    }
}

DEFUN ("float-time", Ffloat_time, Sfloat_time, 0, 1, 0,
       doc: /* Return the current time, as a float number of seconds since the epoch.
If SPECIFIED-TIME is given, it is the time to convert to float
instead of the current time.  The argument should have the form
(HIGH LOW) or (HIGH LOW USEC). Thus, you can use times obtained from
`current-time' and from `file-attributes'.  SPECIFIED-TIME can also
have the form (HIGH . LOW), but this is considered obsolete.

WARNING: Since the result is floating point, it may not be exact.
If precise time stamps are required, use either `current-time',
or (if you need time as a string) `format-time-string'.  */)
  (Lisp_Object specified_time)
{
  time_t sec;
  int usec;

  if (! lisp_time_argument (specified_time, &sec, &usec))
    error ("Invalid time specification");

  return make_float ((sec * 1e6 + usec) / 1e6);
}

/* Write information into buffer S of size MAXSIZE, according to the
   FORMAT of length FORMAT_LEN, using time information taken from *TP.
   Default to Universal Time if UT is nonzero, local time otherwise.
   Use NS as the number of nanoseconds in the %N directive.
   Return the number of bytes written, not including the terminating
   '\0'.  If S is NULL, nothing will be written anywhere; so to
   determine how many bytes would be written, use NULL for S and
   ((size_t) -1) for MAXSIZE.

   This function behaves like nstrftime, except it allows null
   bytes in FORMAT and it does not support nanoseconds.  */
static size_t
emacs_nmemftime (char *s, size_t maxsize, const char *format,
		 size_t format_len, const struct tm *tp, int ut, int ns)
{
  size_t total = 0;

  /* Loop through all the null-terminated strings in the format
     argument.  Normally there's just one null-terminated string, but
     there can be arbitrarily many, concatenated together, if the
     format contains '\0' bytes.  nstrftime stops at the first
     '\0' byte so we must invoke it separately for each such string.  */
  for (;;)
    {
      size_t len;
      size_t result;

      if (s)
	s[0] = '\1';

      result = nstrftime (s, maxsize, format, tp, ut, ns);

      if (s)
	{
	  if (result == 0 && s[0] != '\0')
	    return 0;
	  s += result + 1;
	}

      maxsize -= result + 1;
      total += result;
      len = strlen (format);
      if (len == format_len)
	return total;
      total++;
      format += len + 1;
      format_len -= len + 1;
    }
}

DEFUN ("format-time-string", Fformat_time_string, Sformat_time_string, 1, 3, 0,
       doc: /* Use FORMAT-STRING to format the time TIME, or now if omitted.
TIME is specified as (HIGH LOW . IGNORED), as returned by
`current-time' or `file-attributes'.  The obsolete form (HIGH . LOW)
is also still accepted.
The third, optional, argument UNIVERSAL, if non-nil, means describe TIME
as Universal Time; nil means describe TIME in the local time zone.
The value is a copy of FORMAT-STRING, but with certain constructs replaced
by text that describes the specified date and time in TIME:

%Y is the year, %y within the century, %C the century.
%G is the year corresponding to the ISO week, %g within the century.
%m is the numeric month.
%b and %h are the locale's abbreviated month name, %B the full name.
%d is the day of the month, zero-padded, %e is blank-padded.
%u is the numeric day of week from 1 (Monday) to 7, %w from 0 (Sunday) to 6.
%a is the locale's abbreviated name of the day of week, %A the full name.
%U is the week number starting on Sunday, %W starting on Monday,
 %V according to ISO 8601.
%j is the day of the year.

%H is the hour on a 24-hour clock, %I is on a 12-hour clock, %k is like %H
 only blank-padded, %l is like %I blank-padded.
%p is the locale's equivalent of either AM or PM.
%M is the minute.
%S is the second.
%N is the nanosecond, %6N the microsecond, %3N the millisecond, etc.
%Z is the time zone name, %z is the numeric form.
%s is the number of seconds since 1970-01-01 00:00:00 +0000.

%c is the locale's date and time format.
%x is the locale's "preferred" date format.
%D is like "%m/%d/%y".

%R is like "%H:%M", %T is like "%H:%M:%S", %r is like "%I:%M:%S %p".
%X is the locale's "preferred" time format.

Finally, %n is a newline, %t is a tab, %% is a literal %.

Certain flags and modifiers are available with some format controls.
The flags are `_', `-', `^' and `#'.  For certain characters X,
%_X is like %X, but padded with blanks; %-X is like %X,
but without padding.  %^X is like %X, but with all textual
characters up-cased; %#X is like %X, but with letter-case of
all textual characters reversed.
%NX (where N stands for an integer) is like %X,
but takes up at least N (a number) positions.
The modifiers are `E' and `O'.  For certain characters X,
%EX is a locale's alternative version of %X;
%OX is like %X, but uses the locale's number symbols.

For example, to produce full ISO 8601 format, use "%Y-%m-%dT%T%z".

usage: (format-time-string FORMAT-STRING &optional TIME UNIVERSAL)  */)
  (Lisp_Object format_string, Lisp_Object timeval, Lisp_Object universal)
{
  time_t t;
  struct tm *tm;

  CHECK_STRING (format_string);
  format_string = code_convert_string_norecord (format_string,
						Vlocale_coding_system, 1);
  return format_time_string (SSDATA (format_string), SBYTES (format_string),
			     timeval, ! NILP (universal), &t, &tm);
}

static Lisp_Object
format_time_string (char const *format, ptrdiff_t formatlen,
		    Lisp_Object timeval, int ut, time_t *tval, struct tm **tmp)
{
  ptrdiff_t size;
  int usec;
  int ns;
  struct tm *tm;

  if (! (lisp_time_argument (timeval, tval, &usec)
	 && 0 <= usec && usec < 1000000))
    error ("Invalid time specification");
  ns = usec * 1000;

  /* This is probably enough.  */
  size = formatlen;
  if (size <= (STRING_BYTES_BOUND - 50) / 6)
    size = size * 6 + 50;

  BLOCK_INPUT;
  tm = ut ? gmtime (tval) : localtime (tval);
  UNBLOCK_INPUT;
  if (! tm)
    time_overflow ();
  *tmp = tm;

  synchronize_system_time_locale ();

  while (1)
    {
      char *buf = (char *) alloca (size + 1);
      size_t result;

      buf[0] = '\1';
      BLOCK_INPUT;
      result = emacs_nmemftime (buf, size, format, formatlen, tm, ut, ns);
      UNBLOCK_INPUT;
      if ((result > 0 && result < size) || (result == 0 && buf[0] == '\0'))
	return code_convert_string_norecord (make_unibyte_string (buf, result),
					     Vlocale_coding_system, 0);

      /* If buffer was too small, make it bigger and try again.  */
      BLOCK_INPUT;
      result = emacs_nmemftime (NULL, (size_t) -1, format, formatlen,
				tm, ut, ns);
      UNBLOCK_INPUT;
      if (STRING_BYTES_BOUND <= result)
	string_overflow ();
      size = result + 1;
    }
}

DEFUN ("decode-time", Fdecode_time, Sdecode_time, 0, 1, 0,
       doc: /* Decode a time value as (SEC MINUTE HOUR DAY MONTH YEAR DOW DST ZONE).
The optional SPECIFIED-TIME should be a list of (HIGH LOW . IGNORED),
as from `current-time' and `file-attributes', or nil to use the
current time.  The obsolete form (HIGH . LOW) is also still accepted.
The list has the following nine members: SEC is an integer between 0
and 60; SEC is 60 for a leap second, which only some operating systems
support.  MINUTE is an integer between 0 and 59.  HOUR is an integer
between 0 and 23.  DAY is an integer between 1 and 31.  MONTH is an
integer between 1 and 12.  YEAR is an integer indicating the
four-digit year.  DOW is the day of week, an integer between 0 and 6,
where 0 is Sunday.  DST is t if daylight saving time is in effect,
otherwise nil.  ZONE is an integer indicating the number of seconds
east of Greenwich.  (Note that Common Lisp has different meanings for
DOW and ZONE.)  */)
  (Lisp_Object specified_time)
{
  time_t time_spec;
  struct tm save_tm;
  struct tm *decoded_time;
  Lisp_Object list_args[9];

  if (! lisp_time_argument (specified_time, &time_spec, NULL))
    error ("Invalid time specification");

  BLOCK_INPUT;
  decoded_time = localtime (&time_spec);
  UNBLOCK_INPUT;
  if (! (decoded_time
	 && MOST_NEGATIVE_FIXNUM - TM_YEAR_BASE <= decoded_time->tm_year
	 && decoded_time->tm_year <= MOST_POSITIVE_FIXNUM - TM_YEAR_BASE))
    time_overflow ();
  XSETFASTINT (list_args[0], decoded_time->tm_sec);
  XSETFASTINT (list_args[1], decoded_time->tm_min);
  XSETFASTINT (list_args[2], decoded_time->tm_hour);
  XSETFASTINT (list_args[3], decoded_time->tm_mday);
  XSETFASTINT (list_args[4], decoded_time->tm_mon + 1);
  /* On 64-bit machines an int is narrower than EMACS_INT, thus the
     cast below avoids overflow in int arithmetics.  */
  XSETINT (list_args[5], TM_YEAR_BASE + (EMACS_INT) decoded_time->tm_year);
  XSETFASTINT (list_args[6], decoded_time->tm_wday);
  list_args[7] = (decoded_time->tm_isdst)? Qt : Qnil;

  /* Make a copy, in case gmtime modifies the struct.  */
  save_tm = *decoded_time;
  BLOCK_INPUT;
  decoded_time = gmtime (&time_spec);
  UNBLOCK_INPUT;
  if (decoded_time == 0)
    list_args[8] = Qnil;
  else
    XSETINT (list_args[8], tm_diff (&save_tm, decoded_time));
  return Flist (9, list_args);
}

/* Return OBJ - OFFSET, checking that OBJ is a valid fixnum and that
   the result is representable as an int.  Assume OFFSET is small and
   nonnegative.  */
static int
check_tm_member (Lisp_Object obj, int offset)
{
  EMACS_INT n;
  CHECK_NUMBER (obj);
  n = XINT (obj);
  if (! (INT_MIN + offset <= n && n - offset <= INT_MAX))
    time_overflow ();
  return n - offset;
}

DEFUN ("encode-time", Fencode_time, Sencode_time, 6, MANY, 0,
       doc: /* Convert SECOND, MINUTE, HOUR, DAY, MONTH, YEAR and ZONE to internal time.
This is the reverse operation of `decode-time', which see.
ZONE defaults to the current time zone rule.  This can
be a string or t (as from `set-time-zone-rule'), or it can be a list
\(as from `current-time-zone') or an integer (as from `decode-time')
applied without consideration for daylight saving time.

You can pass more than 7 arguments; then the first six arguments
are used as SECOND through YEAR, and the *last* argument is used as ZONE.
The intervening arguments are ignored.
This feature lets (apply 'encode-time (decode-time ...)) work.

Out-of-range values for SECOND, MINUTE, HOUR, DAY, or MONTH are allowed;
for example, a DAY of 0 means the day preceding the given month.
Year numbers less than 100 are treated just like other year numbers.
If you want them to stand for years in this century, you must do that yourself.

Years before 1970 are not guaranteed to work.  On some systems,
year values as low as 1901 do work.

usage: (encode-time SECOND MINUTE HOUR DAY MONTH YEAR &optional ZONE)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  time_t value;
  struct tm tm;
  Lisp_Object zone = (nargs > 6 ? args[nargs - 1] : Qnil);

  tm.tm_sec  = check_tm_member (args[0], 0);
  tm.tm_min  = check_tm_member (args[1], 0);
  tm.tm_hour = check_tm_member (args[2], 0);
  tm.tm_mday = check_tm_member (args[3], 0);
  tm.tm_mon  = check_tm_member (args[4], 1);
  tm.tm_year = check_tm_member (args[5], TM_YEAR_BASE);
  tm.tm_isdst = -1;

  if (CONSP (zone))
    zone = Fcar (zone);
  if (NILP (zone))
    {
      BLOCK_INPUT;
      value = mktime (&tm);
      UNBLOCK_INPUT;
    }
  else
    {
      char tzbuf[100];
      const char *tzstring;
      char **oldenv = environ, **newenv;

      if (EQ (zone, Qt))
	tzstring = "UTC0";
      else if (STRINGP (zone))
	tzstring = SSDATA (zone);
      else if (INTEGERP (zone))
	{
	  int abszone = eabs (XINT (zone));
	  sprintf (tzbuf, "XXX%s%d:%02d:%02d", "-" + (XINT (zone) < 0),
		   abszone / (60*60), (abszone/60) % 60, abszone % 60);
	  tzstring = tzbuf;
	}
      else
	error ("Invalid time zone specification");

      /* Set TZ before calling mktime; merely adjusting mktime's returned
	 value doesn't suffice, since that would mishandle leap seconds.  */
      set_time_zone_rule (tzstring);

      BLOCK_INPUT;
      value = mktime (&tm);
      UNBLOCK_INPUT;

      /* Restore TZ to previous value.  */
      newenv = environ;
      environ = oldenv;
      xfree (newenv);
#ifdef LOCALTIME_CACHE
      tzset ();
#endif
    }

  if (value == (time_t) -1)
    time_overflow ();

  return make_time (value);
}

DEFUN ("current-time-string", Fcurrent_time_string, Scurrent_time_string, 0, 1, 0,
       doc: /* Return the current local time, as a human-readable string.
Programs can use this function to decode a time,
since the number of columns in each field is fixed
if the year is in the range 1000-9999.
The format is `Sun Sep 16 01:03:52 1973'.
However, see also the functions `decode-time' and `format-time-string'
which provide a much more powerful and general facility.

If SPECIFIED-TIME is given, it is a time to format instead of the
current time.  The argument should have the form (HIGH LOW . IGNORED).
Thus, you can use times obtained from `current-time' and from
`file-attributes'.  SPECIFIED-TIME can also have the form (HIGH . LOW),
but this is considered obsolete.  */)
  (Lisp_Object specified_time)
{
  time_t value;
  struct tm *tm;
  register char *tem;

  if (! lisp_time_argument (specified_time, &value, NULL))
    error ("Invalid time specification");

  /* Convert to a string, checking for out-of-range time stamps.
     Don't use 'ctime', as that might dump core if VALUE is out of
     range.  */
  BLOCK_INPUT;
  tm = localtime (&value);
  UNBLOCK_INPUT;
  if (! (tm && TM_YEAR_IN_ASCTIME_RANGE (tm->tm_year) && (tem = asctime (tm))))
    time_overflow ();

  /* Remove the trailing newline.  */
  tem[strlen (tem) - 1] = '\0';

  return build_string (tem);
}

/* Yield A - B, measured in seconds.
   This function is copied from the GNU C Library.  */
static int
tm_diff (struct tm *a, struct tm *b)
{
  /* Compute intervening leap days correctly even if year is negative.
     Take care to avoid int overflow in leap day calculations,
     but it's OK to assume that A and B are close to each other.  */
  int a4 = (a->tm_year >> 2) + (TM_YEAR_BASE >> 2) - ! (a->tm_year & 3);
  int b4 = (b->tm_year >> 2) + (TM_YEAR_BASE >> 2) - ! (b->tm_year & 3);
  int a100 = a4 / 25 - (a4 % 25 < 0);
  int b100 = b4 / 25 - (b4 % 25 < 0);
  int a400 = a100 >> 2;
  int b400 = b100 >> 2;
  int intervening_leap_days = (a4 - b4) - (a100 - b100) + (a400 - b400);
  int years = a->tm_year - b->tm_year;
  int days = (365 * years + intervening_leap_days
	      + (a->tm_yday - b->tm_yday));
  return (60 * (60 * (24 * days + (a->tm_hour - b->tm_hour))
		+ (a->tm_min - b->tm_min))
	  + (a->tm_sec - b->tm_sec));
}

DEFUN ("current-time-zone", Fcurrent_time_zone, Scurrent_time_zone, 0, 1, 0,
       doc: /* Return the offset and name for the local time zone.
This returns a list of the form (OFFSET NAME).
OFFSET is an integer number of seconds ahead of UTC (east of Greenwich).
    A negative value means west of Greenwich.
NAME is a string giving the name of the time zone.
If SPECIFIED-TIME is given, the time zone offset is determined from it
instead of using the current time.  The argument should have the form
(HIGH LOW . IGNORED).  Thus, you can use times obtained from
`current-time' and from `file-attributes'.  SPECIFIED-TIME can also
have the form (HIGH . LOW), but this is considered obsolete.

Some operating systems cannot provide all this information to Emacs;
in this case, `current-time-zone' returns a list containing nil for
the data it can't find.  */)
  (Lisp_Object specified_time)
{
  time_t value;
  struct tm *t;
  struct tm localtm;
  struct tm *localt;
  Lisp_Object zone_offset, zone_name;

  zone_offset = Qnil;
  zone_name = format_time_string ("%Z", sizeof "%Z" - 1, specified_time,
				  0, &value, &localt);
  localtm = *localt;
  BLOCK_INPUT;
  t = gmtime (&value);
  UNBLOCK_INPUT;

  if (t)
    {
      int offset = tm_diff (&localtm, t);
      zone_offset = make_number (offset);
      if (SCHARS (zone_name) == 0)
	{
	  /* No local time zone name is available; use "+-NNNN" instead.  */
	  int m = offset / 60;
	  int am = offset < 0 ? - m : m;
	  char buf[sizeof "+00" + INT_STRLEN_BOUND (int)];
	  sprintf (buf, "%c%02d%02d", (offset < 0 ? '-' : '+'), am/60, am%60);
	  zone_name = build_string (buf);
	}
    }

  return list2 (zone_offset, zone_name);
}

/* This holds the value of `environ' produced by the previous
   call to Fset_time_zone_rule, or 0 if Fset_time_zone_rule
   has never been called.  */
static char **environbuf;

/* This holds the startup value of the TZ environment variable so it
   can be restored if the user calls set-time-zone-rule with a nil
   argument.  */
static char *initial_tz;

DEFUN ("set-time-zone-rule", Fset_time_zone_rule, Sset_time_zone_rule, 1, 1, 0,
       doc: /* Set the local time zone using TZ, a string specifying a time zone rule.
If TZ is nil, use implementation-defined default time zone information.
If TZ is t, use Universal Time.

Instead of calling this function, you typically want (setenv "TZ" TZ).
That changes both the environment of the Emacs process and the
variable `process-environment', whereas `set-time-zone-rule' affects
only the former.  */)
  (Lisp_Object tz)
{
  const char *tzstring;

  /* When called for the first time, save the original TZ.  */
  if (!environbuf)
    initial_tz = (char *) getenv ("TZ");

  if (NILP (tz))
    tzstring = initial_tz;
  else if (EQ (tz, Qt))
    tzstring = "UTC0";
  else
    {
      CHECK_STRING (tz);
      tzstring = SSDATA (tz);
    }

  set_time_zone_rule (tzstring);
  xfree (environbuf);
  environbuf = environ;

  return Qnil;
}

#ifdef LOCALTIME_CACHE

/* These two values are known to load tz files in buggy implementations,
   i.e. Solaris 1 executables running under either Solaris 1 or Solaris 2.
   Their values shouldn't matter in non-buggy implementations.
   We don't use string literals for these strings,
   since if a string in the environment is in readonly
   storage, it runs afoul of bugs in SVR4 and Solaris 2.3.
   See Sun bugs 1113095 and 1114114, ``Timezone routines
   improperly modify environment''.  */

static char set_time_zone_rule_tz1[] = "TZ=GMT+0";
static char set_time_zone_rule_tz2[] = "TZ=GMT+1";

#endif

/* Set the local time zone rule to TZSTRING.
   This allocates memory into `environ', which it is the caller's
   responsibility to free.  */

void
set_time_zone_rule (const char *tzstring)
{
  ptrdiff_t envptrs;
  char **from, **to, **newenv;

  /* Make the ENVIRON vector longer with room for TZSTRING.  */
  for (from = environ; *from; from++)
    continue;
  envptrs = from - environ + 2;
  newenv = to = (char **) xmalloc (envptrs * sizeof (char *)
				   + (tzstring ? strlen (tzstring) + 4 : 0));

  /* Add TZSTRING to the end of environ, as a value for TZ.  */
  if (tzstring)
    {
      char *t = (char *) (to + envptrs);
      strcpy (t, "TZ=");
      strcat (t, tzstring);
      *to++ = t;
    }

  /* Copy the old environ vector elements into NEWENV,
     but don't copy the TZ variable.
     So we have only one definition of TZ, which came from TZSTRING.  */
  for (from = environ; *from; from++)
    if (strncmp (*from, "TZ=", 3) != 0)
      *to++ = *from;
  *to = 0;

  environ = newenv;

  /* If we do have a TZSTRING, NEWENV points to the vector slot where
     the TZ variable is stored.  If we do not have a TZSTRING,
     TO points to the vector slot which has the terminating null.  */

#ifdef LOCALTIME_CACHE
  {
    /* In SunOS 4.1.3_U1 and 4.1.4, if TZ has a value like
       "US/Pacific" that loads a tz file, then changes to a value like
       "XXX0" that does not load a tz file, and then changes back to
       its original value, the last change is (incorrectly) ignored.
       Also, if TZ changes twice in succession to values that do
       not load a tz file, tzset can dump core (see Sun bug#1225179).
       The following code works around these bugs.  */

    if (tzstring)
      {
	/* Temporarily set TZ to a value that loads a tz file
	   and that differs from tzstring.  */
	char *tz = *newenv;
	*newenv = (strcmp (tzstring, set_time_zone_rule_tz1 + 3) == 0
		   ? set_time_zone_rule_tz2 : set_time_zone_rule_tz1);
	tzset ();
	*newenv = tz;
      }
    else
      {
	/* The implied tzstring is unknown, so temporarily set TZ to
	   two different values that each load a tz file.  */
	*to = set_time_zone_rule_tz1;
	to[1] = 0;
	tzset ();
	*to = set_time_zone_rule_tz2;
	tzset ();
	*to = 0;
      }

    /* Now TZ has the desired value, and tzset can be invoked safely.  */
  }

  tzset ();
#endif
}

/* Insert NARGS Lisp objects in the array ARGS by calling INSERT_FUNC
   (if a type of object is Lisp_Int) or INSERT_FROM_STRING_FUNC (if a
   type of object is Lisp_String).  INHERIT is passed to
   INSERT_FROM_STRING_FUNC as the last argument.  */

static void
general_insert_function (void (*insert_func)
			      (const char *, EMACS_INT),
			 void (*insert_from_string_func)
			      (Lisp_Object, EMACS_INT, EMACS_INT,
			       EMACS_INT, EMACS_INT, int),
			 int inherit, ptrdiff_t nargs, Lisp_Object *args)
{
  ptrdiff_t argnum;
  register Lisp_Object val;

  for (argnum = 0; argnum < nargs; argnum++)
    {
      val = args[argnum];
      if (CHARACTERP (val))
	{
	  int c = XFASTINT (val);
	  unsigned char str[MAX_MULTIBYTE_LENGTH];
	  int len;

	  if (!NILP (BVAR (current_buffer, enable_multibyte_characters)))
	    len = CHAR_STRING (c, str);
	  else
	    {
	      str[0] = ASCII_CHAR_P (c) ? c : multibyte_char_to_unibyte (c);
	      len = 1;
	    }
	  (*insert_func) ((char *) str, len);
	}
      else if (STRINGP (val))
	{
	  (*insert_from_string_func) (val, 0, 0,
				      SCHARS (val),
				      SBYTES (val),
				      inherit);
	}
      else
	wrong_type_argument (Qchar_or_string_p, val);
    }
}

void
insert1 (Lisp_Object arg)
{
  Finsert (1, &arg);
}


/* Callers passing one argument to Finsert need not gcpro the
   argument "array", since the only element of the array will
   not be used after calling insert or insert_from_string, so
   we don't care if it gets trashed.  */

DEFUN ("insert", Finsert, Sinsert, 0, MANY, 0,
       doc: /* Insert the arguments, either strings or characters, at point.
Point and before-insertion markers move forward to end up
 after the inserted text.
Any other markers at the point of insertion remain before the text.

If the current buffer is multibyte, unibyte strings are converted
to multibyte for insertion (see `string-make-multibyte').
If the current buffer is unibyte, multibyte strings are converted
to unibyte for insertion (see `string-make-unibyte').

When operating on binary data, it may be necessary to preserve the
original bytes of a unibyte string when inserting it into a multibyte
buffer; to accomplish this, apply `string-as-multibyte' to the string
and insert the result.

usage: (insert &rest ARGS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  general_insert_function (insert, insert_from_string, 0, nargs, args);
  return Qnil;
}

DEFUN ("insert-and-inherit", Finsert_and_inherit, Sinsert_and_inherit,
   0, MANY, 0,
       doc: /* Insert the arguments at point, inheriting properties from adjoining text.
Point and before-insertion markers move forward to end up
 after the inserted text.
Any other markers at the point of insertion remain before the text.

If the current buffer is multibyte, unibyte strings are converted
to multibyte for insertion (see `unibyte-char-to-multibyte').
If the current buffer is unibyte, multibyte strings are converted
to unibyte for insertion.

usage: (insert-and-inherit &rest ARGS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  general_insert_function (insert_and_inherit, insert_from_string, 1,
			   nargs, args);
  return Qnil;
}

DEFUN ("insert-before-markers", Finsert_before_markers, Sinsert_before_markers, 0, MANY, 0,
       doc: /* Insert strings or characters at point, relocating markers after the text.
Point and markers move forward to end up after the inserted text.

If the current buffer is multibyte, unibyte strings are converted
to multibyte for insertion (see `unibyte-char-to-multibyte').
If the current buffer is unibyte, multibyte strings are converted
to unibyte for insertion.

usage: (insert-before-markers &rest ARGS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  general_insert_function (insert_before_markers,
			   insert_from_string_before_markers, 0,
			   nargs, args);
  return Qnil;
}

DEFUN ("insert-before-markers-and-inherit", Finsert_and_inherit_before_markers,
  Sinsert_and_inherit_before_markers, 0, MANY, 0,
       doc: /* Insert text at point, relocating markers and inheriting properties.
Point and markers move forward to end up after the inserted text.

If the current buffer is multibyte, unibyte strings are converted
to multibyte for insertion (see `unibyte-char-to-multibyte').
If the current buffer is unibyte, multibyte strings are converted
to unibyte for insertion.

usage: (insert-before-markers-and-inherit &rest ARGS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  general_insert_function (insert_before_markers_and_inherit,
			   insert_from_string_before_markers, 1,
			   nargs, args);
  return Qnil;
}

DEFUN ("insert-char", Finsert_char, Sinsert_char, 2, 3, 0,
       doc: /* Insert COUNT copies of CHARACTER.
Point, and before-insertion markers, are relocated as in the function `insert'.
The optional third arg INHERIT, if non-nil, says to inherit text properties
from adjoining text, if those properties are sticky.  */)
  (Lisp_Object character, Lisp_Object count, Lisp_Object inherit)
{
  int i, stringlen;
  register EMACS_INT n;
  int c, len;
  unsigned char str[MAX_MULTIBYTE_LENGTH];
  char string[4000];

  CHECK_CHARACTER (character);
  CHECK_NUMBER (count);
  c = XFASTINT (character);

  if (!NILP (BVAR (current_buffer, enable_multibyte_characters)))
    len = CHAR_STRING (c, str);
  else
    str[0] = c, len = 1;
  if (XINT (count) <= 0)
    return Qnil;
  if (BUF_BYTES_MAX / len < XINT (count))
    buffer_overflow ();
  n = XINT (count) * len;
  stringlen = min (n, sizeof string - sizeof string % len);
  for (i = 0; i < stringlen; i++)
    string[i] = str[i % len];
  while (n > stringlen)
    {
      QUIT;
      if (!NILP (inherit))
	insert_and_inherit (string, stringlen);
      else
	insert (string, stringlen);
      n -= stringlen;
    }
  if (!NILP (inherit))
    insert_and_inherit (string, n);
  else
    insert (string, n);
  return Qnil;
}

DEFUN ("insert-byte", Finsert_byte, Sinsert_byte, 2, 3, 0,
       doc: /* Insert COUNT (second arg) copies of BYTE (first arg).
Both arguments are required.
BYTE is a number of the range 0..255.

If BYTE is 128..255 and the current buffer is multibyte, the
corresponding eight-bit character is inserted.

Point, and before-insertion markers, are relocated as in the function `insert'.
The optional third arg INHERIT, if non-nil, says to inherit text properties
from adjoining text, if those properties are sticky.  */)
  (Lisp_Object byte, Lisp_Object count, Lisp_Object inherit)
{
  CHECK_NUMBER (byte);
  if (XINT (byte) < 0 || XINT (byte) > 255)
    args_out_of_range_3 (byte, make_number (0), make_number (255));
  if (XINT (byte) >= 128
      && ! NILP (BVAR (current_buffer, enable_multibyte_characters)))
    XSETFASTINT (byte, BYTE8_TO_CHAR (XINT (byte)));
  return Finsert_char (byte, count, inherit);
}


/* Making strings from buffer contents.  */

/* Return a Lisp_String containing the text of the current buffer from
   START to END.  If text properties are in use and the current buffer
   has properties in the range specified, the resulting string will also
   have them, if PROPS is nonzero.

   We don't want to use plain old make_string here, because it calls
   make_uninit_string, which can cause the buffer arena to be
   compacted.  make_string has no way of knowing that the data has
   been moved, and thus copies the wrong data into the string.  This
   doesn't effect most of the other users of make_string, so it should
   be left as is.  But we should use this function when conjuring
   buffer substrings.  */

Lisp_Object
make_buffer_string (EMACS_INT start, EMACS_INT end, int props)
{
  EMACS_INT start_byte = CHAR_TO_BYTE (start);
  EMACS_INT end_byte = CHAR_TO_BYTE (end);

  return make_buffer_string_both (start, start_byte, end, end_byte, props);
}

/* Return a Lisp_String containing the text of the current buffer from
   START / START_BYTE to END / END_BYTE.

   If text properties are in use and the current buffer
   has properties in the range specified, the resulting string will also
   have them, if PROPS is nonzero.

   We don't want to use plain old make_string here, because it calls
   make_uninit_string, which can cause the buffer arena to be
   compacted.  make_string has no way of knowing that the data has
   been moved, and thus copies the wrong data into the string.  This
   doesn't effect most of the other users of make_string, so it should
   be left as is.  But we should use this function when conjuring
   buffer substrings.  */

Lisp_Object
make_buffer_string_both (EMACS_INT start, EMACS_INT start_byte,
			 EMACS_INT end, EMACS_INT end_byte, int props)
{
  Lisp_Object result, tem, tem1;

  if (start < GPT && GPT < end)
    move_gap (start);

  if (! NILP (BVAR (current_buffer, enable_multibyte_characters)))
    result = make_uninit_multibyte_string (end - start, end_byte - start_byte);
  else
    result = make_uninit_string (end - start);
  memcpy (SDATA (result), BYTE_POS_ADDR (start_byte), end_byte - start_byte);

  /* If desired, update and copy the text properties.  */
  if (props)
    {
      update_buffer_properties (start, end);

      tem = Fnext_property_change (make_number (start), Qnil, make_number (end));
      tem1 = Ftext_properties_at (make_number (start), Qnil);

      if (XINT (tem) != end || !NILP (tem1))
	copy_intervals_to_string (result, current_buffer, start,
				  end - start);
    }

  return result;
}

/* Call Vbuffer_access_fontify_functions for the range START ... END
   in the current buffer, if necessary.  */

static void
update_buffer_properties (EMACS_INT start, EMACS_INT end)
{
  /* If this buffer has some access functions,
     call them, specifying the range of the buffer being accessed.  */
  if (!NILP (Vbuffer_access_fontify_functions))
    {
      Lisp_Object args[3];
      Lisp_Object tem;

      args[0] = Qbuffer_access_fontify_functions;
      XSETINT (args[1], start);
      XSETINT (args[2], end);

      /* But don't call them if we can tell that the work
	 has already been done.  */
      if (!NILP (Vbuffer_access_fontified_property))
	{
	  tem = Ftext_property_any (args[1], args[2],
				    Vbuffer_access_fontified_property,
				    Qnil, Qnil);
	  if (! NILP (tem))
	    Frun_hook_with_args (3, args);
	}
      else
	Frun_hook_with_args (3, args);
    }
}

DEFUN ("buffer-substring", Fbuffer_substring, Sbuffer_substring, 2, 2, 0,
       doc: /* Return the contents of part of the current buffer as a string.
The two arguments START and END are character positions;
they can be in either order.
The string returned is multibyte if the buffer is multibyte.

This function copies the text properties of that part of the buffer
into the result string; if you don't want the text properties,
use `buffer-substring-no-properties' instead.  */)
  (Lisp_Object start, Lisp_Object end)
{
  register EMACS_INT b, e;

  validate_region (&start, &end);
  b = XINT (start);
  e = XINT (end);

  return make_buffer_string (b, e, 1);
}

DEFUN ("buffer-substring-no-properties", Fbuffer_substring_no_properties,
       Sbuffer_substring_no_properties, 2, 2, 0,
       doc: /* Return the characters of part of the buffer, without the text properties.
The two arguments START and END are character positions;
they can be in either order.  */)
  (Lisp_Object start, Lisp_Object end)
{
  register EMACS_INT b, e;

  validate_region (&start, &end);
  b = XINT (start);
  e = XINT (end);

  return make_buffer_string (b, e, 0);
}

DEFUN ("buffer-string", Fbuffer_string, Sbuffer_string, 0, 0, 0,
       doc: /* Return the contents of the current buffer as a string.
If narrowing is in effect, this function returns only the visible part
of the buffer.  */)
  (void)
{
  return make_buffer_string (BEGV, ZV, 1);
}

DEFUN ("insert-buffer-substring", Finsert_buffer_substring, Sinsert_buffer_substring,
       1, 3, 0,
       doc: /* Insert before point a substring of the contents of BUFFER.
BUFFER may be a buffer or a buffer name.
Arguments START and END are character positions specifying the substring.
They default to the values of (point-min) and (point-max) in BUFFER.  */)
  (Lisp_Object buffer, Lisp_Object start, Lisp_Object end)
{
  register EMACS_INT b, e, temp;
  register struct buffer *bp, *obuf;
  Lisp_Object buf;

  buf = Fget_buffer (buffer);
  if (NILP (buf))
    nsberror (buffer);
  bp = XBUFFER (buf);
  if (NILP (BVAR (bp, name)))
    error ("Selecting deleted buffer");

  if (NILP (start))
    b = BUF_BEGV (bp);
  else
    {
      CHECK_NUMBER_COERCE_MARKER (start);
      b = XINT (start);
    }
  if (NILP (end))
    e = BUF_ZV (bp);
  else
    {
      CHECK_NUMBER_COERCE_MARKER (end);
      e = XINT (end);
    }

  if (b > e)
    temp = b, b = e, e = temp;

  if (!(BUF_BEGV (bp) <= b && e <= BUF_ZV (bp)))
    args_out_of_range (start, end);

  obuf = current_buffer;
  set_buffer_internal_1 (bp);
  update_buffer_properties (b, e);
  set_buffer_internal_1 (obuf);

  insert_from_buffer (bp, b, e - b, 0);
  return Qnil;
}

DEFUN ("compare-buffer-substrings", Fcompare_buffer_substrings, Scompare_buffer_substrings,
       6, 6, 0,
       doc: /* Compare two substrings of two buffers; return result as number.
the value is -N if first string is less after N-1 chars,
+N if first string is greater after N-1 chars, or 0 if strings match.
Each substring is represented as three arguments: BUFFER, START and END.
That makes six args in all, three for each substring.

The value of `case-fold-search' in the current buffer
determines whether case is significant or ignored.  */)
  (Lisp_Object buffer1, Lisp_Object start1, Lisp_Object end1, Lisp_Object buffer2, Lisp_Object start2, Lisp_Object end2)
{
  register EMACS_INT begp1, endp1, begp2, endp2, temp;
  register struct buffer *bp1, *bp2;
  register Lisp_Object trt
    = (!NILP (BVAR (current_buffer, case_fold_search))
       ? BVAR (current_buffer, case_canon_table) : Qnil);
  EMACS_INT chars = 0;
  EMACS_INT i1, i2, i1_byte, i2_byte;

  /* Find the first buffer and its substring.  */

  if (NILP (buffer1))
    bp1 = current_buffer;
  else
    {
      Lisp_Object buf1;
      buf1 = Fget_buffer (buffer1);
      if (NILP (buf1))
	nsberror (buffer1);
      bp1 = XBUFFER (buf1);
      if (NILP (BVAR (bp1, name)))
	error ("Selecting deleted buffer");
    }

  if (NILP (start1))
    begp1 = BUF_BEGV (bp1);
  else
    {
      CHECK_NUMBER_COERCE_MARKER (start1);
      begp1 = XINT (start1);
    }
  if (NILP (end1))
    endp1 = BUF_ZV (bp1);
  else
    {
      CHECK_NUMBER_COERCE_MARKER (end1);
      endp1 = XINT (end1);
    }

  if (begp1 > endp1)
    temp = begp1, begp1 = endp1, endp1 = temp;

  if (!(BUF_BEGV (bp1) <= begp1
	&& begp1 <= endp1
        && endp1 <= BUF_ZV (bp1)))
    args_out_of_range (start1, end1);

  /* Likewise for second substring.  */

  if (NILP (buffer2))
    bp2 = current_buffer;
  else
    {
      Lisp_Object buf2;
      buf2 = Fget_buffer (buffer2);
      if (NILP (buf2))
	nsberror (buffer2);
      bp2 = XBUFFER (buf2);
      if (NILP (BVAR (bp2, name)))
	error ("Selecting deleted buffer");
    }

  if (NILP (start2))
    begp2 = BUF_BEGV (bp2);
  else
    {
      CHECK_NUMBER_COERCE_MARKER (start2);
      begp2 = XINT (start2);
    }
  if (NILP (end2))
    endp2 = BUF_ZV (bp2);
  else
    {
      CHECK_NUMBER_COERCE_MARKER (end2);
      endp2 = XINT (end2);
    }

  if (begp2 > endp2)
    temp = begp2, begp2 = endp2, endp2 = temp;

  if (!(BUF_BEGV (bp2) <= begp2
	&& begp2 <= endp2
        && endp2 <= BUF_ZV (bp2)))
    args_out_of_range (start2, end2);

  i1 = begp1;
  i2 = begp2;
  i1_byte = buf_charpos_to_bytepos (bp1, i1);
  i2_byte = buf_charpos_to_bytepos (bp2, i2);

  while (i1 < endp1 && i2 < endp2)
    {
      /* When we find a mismatch, we must compare the
	 characters, not just the bytes.  */
      int c1, c2;

      QUIT;

      if (! NILP (BVAR (bp1, enable_multibyte_characters)))
	{
	  c1 = BUF_FETCH_MULTIBYTE_CHAR (bp1, i1_byte);
	  BUF_INC_POS (bp1, i1_byte);
	  i1++;
	}
      else
	{
	  c1 = BUF_FETCH_BYTE (bp1, i1);
	  MAKE_CHAR_MULTIBYTE (c1);
	  i1++;
	}

      if (! NILP (BVAR (bp2, enable_multibyte_characters)))
	{
	  c2 = BUF_FETCH_MULTIBYTE_CHAR (bp2, i2_byte);
	  BUF_INC_POS (bp2, i2_byte);
	  i2++;
	}
      else
	{
	  c2 = BUF_FETCH_BYTE (bp2, i2);
	  MAKE_CHAR_MULTIBYTE (c2);
	  i2++;
	}

      if (!NILP (trt))
	{
	  c1 = CHAR_TABLE_TRANSLATE (trt, c1);
	  c2 = CHAR_TABLE_TRANSLATE (trt, c2);
	}
      if (c1 < c2)
	return make_number (- 1 - chars);
      if (c1 > c2)
	return make_number (chars + 1);

      chars++;
    }

  /* The strings match as far as they go.
     If one is shorter, that one is less.  */
  if (chars < endp1 - begp1)
    return make_number (chars + 1);
  else if (chars < endp2 - begp2)
    return make_number (- chars - 1);

  /* Same length too => they are equal.  */
  return make_number (0);
}

static Lisp_Object
subst_char_in_region_unwind (Lisp_Object arg)
{
  return BVAR (current_buffer, undo_list) = arg;
}

static Lisp_Object
subst_char_in_region_unwind_1 (Lisp_Object arg)
{
  return BVAR (current_buffer, filename) = arg;
}

DEFUN ("subst-char-in-region", Fsubst_char_in_region,
       Ssubst_char_in_region, 4, 5, 0,
       doc: /* From START to END, replace FROMCHAR with TOCHAR each time it occurs.
If optional arg NOUNDO is non-nil, don't record this change for undo
and don't mark the buffer as really changed.
Both characters must have the same length of multi-byte form.  */)
  (Lisp_Object start, Lisp_Object end, Lisp_Object fromchar, Lisp_Object tochar, Lisp_Object noundo)
{
  register EMACS_INT pos, pos_byte, stop, i, len, end_byte;
  /* Keep track of the first change in the buffer:
     if 0 we haven't found it yet.
     if < 0 we've found it and we've run the before-change-function.
     if > 0 we've actually performed it and the value is its position.  */
  EMACS_INT changed = 0;
  unsigned char fromstr[MAX_MULTIBYTE_LENGTH], tostr[MAX_MULTIBYTE_LENGTH];
  unsigned char *p;
  int count = SPECPDL_INDEX ();
#define COMBINING_NO	 0
#define COMBINING_BEFORE 1
#define COMBINING_AFTER  2
#define COMBINING_BOTH (COMBINING_BEFORE | COMBINING_AFTER)
  int maybe_byte_combining = COMBINING_NO;
  EMACS_INT last_changed = 0;
  int multibyte_p = !NILP (BVAR (current_buffer, enable_multibyte_characters));
  int fromc, toc;

 restart:

  validate_region (&start, &end);
  CHECK_CHARACTER (fromchar);
  CHECK_CHARACTER (tochar);
  fromc = XFASTINT (fromchar);
  toc = XFASTINT (tochar);

  if (multibyte_p)
    {
      len = CHAR_STRING (fromc, fromstr);
      if (CHAR_STRING (toc, tostr) != len)
	error ("Characters in `subst-char-in-region' have different byte-lengths");
      if (!ASCII_BYTE_P (*tostr))
	{
	  /* If *TOSTR is in the range 0x80..0x9F and TOCHAR is not a
	     complete multibyte character, it may be combined with the
	     after bytes.  If it is in the range 0xA0..0xFF, it may be
	     combined with the before and after bytes.  */
	  if (!CHAR_HEAD_P (*tostr))
	    maybe_byte_combining = COMBINING_BOTH;
	  else if (BYTES_BY_CHAR_HEAD (*tostr) > len)
	    maybe_byte_combining = COMBINING_AFTER;
	}
    }
  else
    {
      len = 1;
      fromstr[0] = fromc;
      tostr[0] = toc;
    }

  pos = XINT (start);
  pos_byte = CHAR_TO_BYTE (pos);
  stop = CHAR_TO_BYTE (XINT (end));
  end_byte = stop;

  /* If we don't want undo, turn off putting stuff on the list.
     That's faster than getting rid of things,
     and it prevents even the entry for a first change.
     Also inhibit locking the file.  */
  if (!changed && !NILP (noundo))
    {
      record_unwind_protect (subst_char_in_region_unwind,
			     BVAR (current_buffer, undo_list));
      BVAR (current_buffer, undo_list) = Qt;
      /* Don't do file-locking.  */
      record_unwind_protect (subst_char_in_region_unwind_1,
			     BVAR (current_buffer, filename));
      BVAR (current_buffer, filename) = Qnil;
    }

  if (pos_byte < GPT_BYTE)
    stop = min (stop, GPT_BYTE);
  while (1)
    {
      EMACS_INT pos_byte_next = pos_byte;

      if (pos_byte >= stop)
	{
	  if (pos_byte >= end_byte) break;
	  stop = end_byte;
	}
      p = BYTE_POS_ADDR (pos_byte);
      if (multibyte_p)
	INC_POS (pos_byte_next);
      else
	++pos_byte_next;
      if (pos_byte_next - pos_byte == len
	  && p[0] == fromstr[0]
	  && (len == 1
	      || (p[1] == fromstr[1]
		  && (len == 2 || (p[2] == fromstr[2]
				 && (len == 3 || p[3] == fromstr[3]))))))
	{
	  if (changed < 0)
	    /* We've already seen this and run the before-change-function;
	       this time we only need to record the actual position. */
	    changed = pos;
	  else if (!changed)
	    {
	      changed = -1;
	      modify_region (current_buffer, pos, XINT (end), 0);

	      if (! NILP (noundo))
		{
		  if (MODIFF - 1 == SAVE_MODIFF)
		    SAVE_MODIFF++;
		  if (MODIFF - 1 == BUF_AUTOSAVE_MODIFF (current_buffer))
		    BUF_AUTOSAVE_MODIFF (current_buffer)++;
		}

	      /* The before-change-function may have moved the gap
		 or even modified the buffer so we should start over. */
	      goto restart;
	    }

	  /* Take care of the case where the new character
	     combines with neighboring bytes.  */
	  if (maybe_byte_combining
	      && (maybe_byte_combining == COMBINING_AFTER
		  ? (pos_byte_next < Z_BYTE
		     && ! CHAR_HEAD_P (FETCH_BYTE (pos_byte_next)))
		  : ((pos_byte_next < Z_BYTE
		      && ! CHAR_HEAD_P (FETCH_BYTE (pos_byte_next)))
		     || (pos_byte > BEG_BYTE
			 && ! ASCII_BYTE_P (FETCH_BYTE (pos_byte - 1))))))
	    {
	      Lisp_Object tem, string;

	      struct gcpro gcpro1;

	      tem = BVAR (current_buffer, undo_list);
	      GCPRO1 (tem);

	      /* Make a multibyte string containing this single character.  */
	      string = make_multibyte_string ((char *) tostr, 1, len);
	      /* replace_range is less efficient, because it moves the gap,
		 but it handles combining correctly.  */
	      replace_range (pos, pos + 1, string,
			     0, 0, 1);
	      pos_byte_next = CHAR_TO_BYTE (pos);
	      if (pos_byte_next > pos_byte)
		/* Before combining happened.  We should not increment
		   POS.  So, to cancel the later increment of POS,
		   decrease it now.  */
		pos--;
	      else
		INC_POS (pos_byte_next);

	      if (! NILP (noundo))
		BVAR (current_buffer, undo_list) = tem;

	      UNGCPRO;
	    }
	  else
	    {
	      if (NILP (noundo))
		record_change (pos, 1);
	      for (i = 0; i < len; i++) *p++ = tostr[i];
	    }
	  last_changed =  pos + 1;
	}
      pos_byte = pos_byte_next;
      pos++;
    }

  if (changed > 0)
    {
      signal_after_change (changed,
			   last_changed - changed, last_changed - changed);
      update_compositions (changed, last_changed, CHECK_ALL);
    }

  unbind_to (count, Qnil);
  return Qnil;
}


static Lisp_Object check_translation (EMACS_INT, EMACS_INT, EMACS_INT,
				      Lisp_Object);

/* Helper function for Ftranslate_region_internal.

   Check if a character sequence at POS (POS_BYTE) matches an element
   of VAL.  VAL is a list (([FROM-CHAR ...] . TO) ...).  If a matching
   element is found, return it.  Otherwise return Qnil.  */

static Lisp_Object
check_translation (EMACS_INT pos, EMACS_INT pos_byte, EMACS_INT end,
		   Lisp_Object val)
{
  int buf_size = 16, buf_used = 0;
  int *buf = alloca (sizeof (int) * buf_size);

  for (; CONSP (val); val = XCDR (val))
    {
      Lisp_Object elt;
      EMACS_INT len, i;

      elt = XCAR (val);
      if (! CONSP (elt))
	continue;
      elt = XCAR (elt);
      if (! VECTORP (elt))
	continue;
      len = ASIZE (elt);
      if (len <= end - pos)
	{
	  for (i = 0; i < len; i++)
	    {
	      if (buf_used <= i)
		{
		  unsigned char *p = BYTE_POS_ADDR (pos_byte);
		  int len1;

		  if (buf_used == buf_size)
		    {
		      int *newbuf;

		      buf_size += 16;
		      newbuf = alloca (sizeof (int) * buf_size);
		      memcpy (newbuf, buf, sizeof (int) * buf_used);
		      buf = newbuf;
		    }
		  buf[buf_used++] = STRING_CHAR_AND_LENGTH (p, len1);
		  pos_byte += len1;
		}
	      if (XINT (AREF (elt, i)) != buf[i])
		break;
	    }
	  if (i == len)
	    return XCAR (val);
	}
    }
  return Qnil;
}


DEFUN ("translate-region-internal", Ftranslate_region_internal,
       Stranslate_region_internal, 3, 3, 0,
       doc: /* Internal use only.
From START to END, translate characters according to TABLE.
TABLE is a string or a char-table; the Nth character in it is the
mapping for the character with code N.
It returns the number of characters changed.  */)
  (Lisp_Object start, Lisp_Object end, register Lisp_Object table)
{
  register unsigned char *tt;	/* Trans table. */
  register int nc;		/* New character. */
  int cnt;			/* Number of changes made. */
  EMACS_INT size;		/* Size of translate table. */
  EMACS_INT pos, pos_byte, end_pos;
  int multibyte = !NILP (BVAR (current_buffer, enable_multibyte_characters));
  int string_multibyte IF_LINT (= 0);

  validate_region (&start, &end);
  if (CHAR_TABLE_P (table))
    {
      if (! EQ (XCHAR_TABLE (table)->purpose, Qtranslation_table))
	error ("Not a translation table");
      size = MAX_CHAR;
      tt = NULL;
    }
  else
    {
      CHECK_STRING (table);

      if (! multibyte && (SCHARS (table) < SBYTES (table)))
	table = string_make_unibyte (table);
      string_multibyte = SCHARS (table) < SBYTES (table);
      size = SBYTES (table);
      tt = SDATA (table);
    }

  pos = XINT (start);
  pos_byte = CHAR_TO_BYTE (pos);
  end_pos = XINT (end);
  modify_region (current_buffer, pos, end_pos, 0);

  cnt = 0;
  for (; pos < end_pos; )
    {
      register unsigned char *p = BYTE_POS_ADDR (pos_byte);
      unsigned char *str, buf[MAX_MULTIBYTE_LENGTH];
      int len, str_len;
      int oc;
      Lisp_Object val;

      if (multibyte)
	oc = STRING_CHAR_AND_LENGTH (p, len);
      else
	oc = *p, len = 1;
      if (oc < size)
	{
	  if (tt)
	    {
	      /* Reload as signal_after_change in last iteration may GC.  */
	      tt = SDATA (table);
	      if (string_multibyte)
		{
		  str = tt + string_char_to_byte (table, oc);
		  nc = STRING_CHAR_AND_LENGTH (str, str_len);
		}
	      else
		{
		  nc = tt[oc];
		  if (! ASCII_BYTE_P (nc) && multibyte)
		    {
		      str_len = BYTE8_STRING (nc, buf);
		      str = buf;
		    }
		  else
		    {
		      str_len = 1;
		      str = tt + oc;
		    }
		}
	    }
	  else
	    {
	      nc = oc;
	      val = CHAR_TABLE_REF (table, oc);
	      if (CHARACTERP (val))
		{
		  nc = XFASTINT (val);
		  str_len = CHAR_STRING (nc, buf);
		  str = buf;
		}
	      else if (VECTORP (val) || (CONSP (val)))
		{
		  /* VAL is [TO_CHAR ...] or (([FROM-CHAR ...] .  TO) ...)
		     where TO is TO-CHAR or [TO-CHAR ...].  */
		  nc = -1;
		}
	    }

	  if (nc != oc && nc >= 0)
	    {
	      /* Simple one char to one char translation.  */
	      if (len != str_len)
		{
		  Lisp_Object string;

		  /* This is less efficient, because it moves the gap,
		     but it should handle multibyte characters correctly.  */
		  string = make_multibyte_string ((char *) str, 1, str_len);
		  replace_range (pos, pos + 1, string, 1, 0, 1);
		  len = str_len;
		}
	      else
		{
		  record_change (pos, 1);
		  while (str_len-- > 0)
		    *p++ = *str++;
		  signal_after_change (pos, 1, 1);
		  update_compositions (pos, pos + 1, CHECK_BORDER);
		}
	      ++cnt;
	    }
	  else if (nc < 0)
	    {
	      Lisp_Object string;

	      if (CONSP (val))
		{
		  val = check_translation (pos, pos_byte, end_pos, val);
		  if (NILP (val))
		    {
		      pos_byte += len;
		      pos++;
		      continue;
		    }
		  /* VAL is ([FROM-CHAR ...] . TO).  */
		  len = ASIZE (XCAR (val));
		  val = XCDR (val);
		}
	      else
		len = 1;

	      if (VECTORP (val))
		{
		  string = Fconcat (1, &val);
		}
	      else
		{
		  string = Fmake_string (make_number (1), val);
		}
	      replace_range (pos, pos + len, string, 1, 0, 1);
	      pos_byte += SBYTES (string);
	      pos += SCHARS (string);
	      cnt += SCHARS (string);
	      end_pos += SCHARS (string) - len;
	      continue;
	    }
	}
      pos_byte += len;
      pos++;
    }

  return make_number (cnt);
}

DEFUN ("delete-region", Fdelete_region, Sdelete_region, 2, 2, "r",
       doc: /* Delete the text between START and END.
If called interactively, delete the region between point and mark.
This command deletes buffer text without modifying the kill ring.  */)
  (Lisp_Object start, Lisp_Object end)
{
  validate_region (&start, &end);
  del_range (XINT (start), XINT (end));
  return Qnil;
}

DEFUN ("delete-and-extract-region", Fdelete_and_extract_region,
       Sdelete_and_extract_region, 2, 2, 0,
       doc: /* Delete the text between START and END and return it.  */)
  (Lisp_Object start, Lisp_Object end)
{
  validate_region (&start, &end);
  if (XINT (start) == XINT (end))
    return empty_unibyte_string;
  return del_range_1 (XINT (start), XINT (end), 1, 1);
}

DEFUN ("widen", Fwiden, Swiden, 0, 0, "",
       doc: /* Remove restrictions (narrowing) from current buffer.
This allows the buffer's full text to be seen and edited.  */)
  (void)
{
  if (BEG != BEGV || Z != ZV)
    current_buffer->clip_changed = 1;
  BEGV = BEG;
  BEGV_BYTE = BEG_BYTE;
  SET_BUF_ZV_BOTH (current_buffer, Z, Z_BYTE);
  /* Changing the buffer bounds invalidates any recorded current column.  */
  invalidate_current_column ();
  return Qnil;
}

DEFUN ("narrow-to-region", Fnarrow_to_region, Snarrow_to_region, 2, 2, "r",
       doc: /* Restrict editing in this buffer to the current region.
The rest of the text becomes temporarily invisible and untouchable
but is not deleted; if you save the buffer in a file, the invisible
text is included in the file.  \\[widen] makes all visible again.
See also `save-restriction'.

When calling from a program, pass two arguments; positions (integers
or markers) bounding the text that should remain visible.  */)
  (register Lisp_Object start, Lisp_Object end)
{
  CHECK_NUMBER_COERCE_MARKER (start);
  CHECK_NUMBER_COERCE_MARKER (end);

  if (XINT (start) > XINT (end))
    {
      Lisp_Object tem;
      tem = start; start = end; end = tem;
    }

  if (!(BEG <= XINT (start) && XINT (start) <= XINT (end) && XINT (end) <= Z))
    args_out_of_range (start, end);

  if (BEGV != XFASTINT (start) || ZV != XFASTINT (end))
    current_buffer->clip_changed = 1;

  SET_BUF_BEGV (current_buffer, XFASTINT (start));
  SET_BUF_ZV (current_buffer, XFASTINT (end));
  if (PT < XFASTINT (start))
    SET_PT (XFASTINT (start));
  if (PT > XFASTINT (end))
    SET_PT (XFASTINT (end));
  /* Changing the buffer bounds invalidates any recorded current column.  */
  invalidate_current_column ();
  return Qnil;
}

Lisp_Object
save_restriction_save (void)
{
  if (BEGV == BEG && ZV == Z)
    /* The common case that the buffer isn't narrowed.
       We return just the buffer object, which save_restriction_restore
       recognizes as meaning `no restriction'.  */
    return Fcurrent_buffer ();
  else
    /* We have to save a restriction, so return a pair of markers, one
       for the beginning and one for the end.  */
    {
      Lisp_Object beg, end;

      beg = buildmark (BEGV, BEGV_BYTE);
      end = buildmark (ZV, ZV_BYTE);

      /* END must move forward if text is inserted at its exact location.  */
      XMARKER (end)->insertion_type = 1;

      return Fcons (beg, end);
    }
}

Lisp_Object
save_restriction_restore (Lisp_Object data)
{
  struct buffer *cur = NULL;
  struct buffer *buf = (CONSP (data)
			? XMARKER (XCAR (data))->buffer
			: XBUFFER (data));

  if (buf && buf != current_buffer && !NILP (BVAR (buf, pt_marker)))
    { /* If `buf' uses markers to keep track of PT, BEGV, and ZV (as
	 is the case if it is or has an indirect buffer), then make
	 sure it is current before we update BEGV, so
	 set_buffer_internal takes care of managing those markers.  */
      cur = current_buffer;
      set_buffer_internal (buf);
    }

  if (CONSP (data))
    /* A pair of marks bounding a saved restriction.  */
    {
      struct Lisp_Marker *beg = XMARKER (XCAR (data));
      struct Lisp_Marker *end = XMARKER (XCDR (data));
      eassert (buf == end->buffer);

      if (buf /* Verify marker still points to a buffer.  */
	  && (beg->charpos != BUF_BEGV (buf) || end->charpos != BUF_ZV (buf)))
	/* The restriction has changed from the saved one, so restore
	   the saved restriction.  */
	{
	  EMACS_INT pt = BUF_PT (buf);

	  SET_BUF_BEGV_BOTH (buf, beg->charpos, beg->bytepos);
	  SET_BUF_ZV_BOTH (buf, end->charpos, end->bytepos);

	  if (pt < beg->charpos || pt > end->charpos)
	    /* The point is outside the new visible range, move it inside. */
	    SET_BUF_PT_BOTH (buf,
			     clip_to_bounds (beg->charpos, pt, end->charpos),
			     clip_to_bounds (beg->bytepos, BUF_PT_BYTE (buf),
					     end->bytepos));

	  buf->clip_changed = 1; /* Remember that the narrowing changed. */
	}
    }
  else
    /* A buffer, which means that there was no old restriction.  */
    {
      if (buf /* Verify marker still points to a buffer.  */
	  && (BUF_BEGV (buf) != BUF_BEG (buf) || BUF_ZV (buf) != BUF_Z (buf)))
	/* The buffer has been narrowed, get rid of the narrowing.  */
	{
	  SET_BUF_BEGV_BOTH (buf, BUF_BEG (buf), BUF_BEG_BYTE (buf));
	  SET_BUF_ZV_BOTH (buf, BUF_Z (buf), BUF_Z_BYTE (buf));

	  buf->clip_changed = 1; /* Remember that the narrowing changed. */
	}
    }

  /* Changing the buffer bounds invalidates any recorded current column.  */
  invalidate_current_column ();

  if (cur)
    set_buffer_internal (cur);

  return Qnil;
}

DEFUN ("save-restriction", Fsave_restriction, Ssave_restriction, 0, UNEVALLED, 0,
       doc: /* Execute BODY, saving and restoring current buffer's restrictions.
The buffer's restrictions make parts of the beginning and end invisible.
\(They are set up with `narrow-to-region' and eliminated with `widen'.)
This special form, `save-restriction', saves the current buffer's restrictions
when it is entered, and restores them when it is exited.
So any `narrow-to-region' within BODY lasts only until the end of the form.
The old restrictions settings are restored
even in case of abnormal exit (throw or error).

The value returned is the value of the last form in BODY.

Note: if you are using both `save-excursion' and `save-restriction',
use `save-excursion' outermost:
    (save-excursion (save-restriction ...))

usage: (save-restriction &rest BODY)  */)
  (Lisp_Object body)
{
  register Lisp_Object val;
  int count = SPECPDL_INDEX ();

  record_unwind_protect (save_restriction_restore, save_restriction_save ());
  val = Fprogn (body);
  return unbind_to (count, val);
}

/* Buffer for the most recent text displayed by Fmessage_box.  */
static char *message_text;

/* Allocated length of that buffer.  */
static ptrdiff_t message_length;

DEFUN ("message", Fmessage, Smessage, 1, MANY, 0,
       doc: /* Display a message at the bottom of the screen.
The message also goes into the `*Messages*' buffer.
\(In keyboard macros, that's all it does.)
Return the message.

The first argument is a format control string, and the rest are data
to be formatted under control of the string.  See `format' for details.

Note: Use (message "%s" VALUE) to print the value of expressions and
variables to avoid accidentally interpreting `%' as format specifiers.

If the first argument is nil or the empty string, the function clears
any existing message; this lets the minibuffer contents show.  See
also `current-message'.

usage: (message FORMAT-STRING &rest ARGS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  if (NILP (args[0])
      || (STRINGP (args[0])
	  && SBYTES (args[0]) == 0))
    {
      message (0);
      return args[0];
    }
  else
    {
      register Lisp_Object val;
      val = Fformat (nargs, args);
      message3 (val, SBYTES (val), STRING_MULTIBYTE (val));
      return val;
    }
}

DEFUN ("message-box", Fmessage_box, Smessage_box, 1, MANY, 0,
       doc: /* Display a message, in a dialog box if possible.
If a dialog box is not available, use the echo area.
The first argument is a format control string, and the rest are data
to be formatted under control of the string.  See `format' for details.

If the first argument is nil or the empty string, clear any existing
message; let the minibuffer contents show.

usage: (message-box FORMAT-STRING &rest ARGS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  if (NILP (args[0]))
    {
      message (0);
      return Qnil;
    }
  else
    {
      register Lisp_Object val;
      val = Fformat (nargs, args);
#ifdef HAVE_MENUS
      /* The MS-DOS frames support popup menus even though they are
	 not FRAME_WINDOW_P.  */
      if (FRAME_WINDOW_P (XFRAME (selected_frame))
	  || FRAME_MSDOS_P (XFRAME (selected_frame)))
      {
	Lisp_Object pane, menu;
	struct gcpro gcpro1;
	pane = Fcons (Fcons (build_string ("OK"), Qt), Qnil);
	GCPRO1 (pane);
	menu = Fcons (val, pane);
	Fx_popup_dialog (Qt, menu, Qt);
	UNGCPRO;
	return val;
      }
#endif /* HAVE_MENUS */
      /* Copy the data so that it won't move when we GC.  */
      if (! message_text)
	{
	  message_text = (char *)xmalloc (80);
	  message_length = 80;
	}
      if (SBYTES (val) > message_length)
	{
	  message_text = (char *) xrealloc (message_text, SBYTES (val));
	  message_length = SBYTES (val);
	}
      memcpy (message_text, SDATA (val), SBYTES (val));
      message2 (message_text, SBYTES (val),
		STRING_MULTIBYTE (val));
      return val;
    }
}

DEFUN ("message-or-box", Fmessage_or_box, Smessage_or_box, 1, MANY, 0,
       doc: /* Display a message in a dialog box or in the echo area.
If this command was invoked with the mouse, use a dialog box if
`use-dialog-box' is non-nil.
Otherwise, use the echo area.
The first argument is a format control string, and the rest are data
to be formatted under control of the string.  See `format' for details.

If the first argument is nil or the empty string, clear any existing
message; let the minibuffer contents show.

usage: (message-or-box FORMAT-STRING &rest ARGS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
#ifdef HAVE_MENUS
  if ((NILP (last_nonmenu_event) || CONSP (last_nonmenu_event))
      && use_dialog_box)
    return Fmessage_box (nargs, args);
#endif
  return Fmessage (nargs, args);
}

DEFUN ("current-message", Fcurrent_message, Scurrent_message, 0, 0, 0,
       doc: /* Return the string currently displayed in the echo area, or nil if none.  */)
  (void)
{
  return current_message ();
}


DEFUN ("propertize", Fpropertize, Spropertize, 1, MANY, 0,
       doc: /* Return a copy of STRING with text properties added.
First argument is the string to copy.
Remaining arguments form a sequence of PROPERTY VALUE pairs for text
properties to add to the result.
usage: (propertize STRING &rest PROPERTIES)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  Lisp_Object properties, string;
  struct gcpro gcpro1, gcpro2;
  ptrdiff_t i;

  /* Number of args must be odd.  */
  if ((nargs & 1) == 0)
    error ("Wrong number of arguments");

  properties = string = Qnil;
  GCPRO2 (properties, string);

  /* First argument must be a string.  */
  CHECK_STRING (args[0]);
  string = Fcopy_sequence (args[0]);

  for (i = 1; i < nargs; i += 2)
    properties = Fcons (args[i], Fcons (args[i + 1], properties));

  Fadd_text_properties (make_number (0),
			make_number (SCHARS (string)),
			properties, string);
  RETURN_UNGCPRO (string);
}

DEFUN ("format", Fformat, Sformat, 1, MANY, 0,
       doc: /* Format a string out of a format-string and arguments.
The first argument is a format control string.
The other arguments are substituted into it to make the result, a string.

The format control string may contain %-sequences meaning to substitute
the next available argument:

%s means print a string argument.  Actually, prints any object, with `princ'.
%d means print as number in decimal (%o octal, %x hex).
%X is like %x, but uses upper case.
%e means print a number in exponential notation.
%f means print a number in decimal-point notation.
%g means print a number in exponential notation
  or decimal-point notation, whichever uses fewer characters.
%c means print a number as a single character.
%S means print any object as an s-expression (using `prin1').

The argument used for %d, %o, %x, %e, %f, %g or %c must be a number.
Use %% to put a single % into the output.

A %-sequence may contain optional flag, width, and precision
specifiers, as follows:

  %<flags><width><precision>character

where flags is [+ #-0]+, width is [0-9]+, and precision is .[0-9]+

The + flag character inserts a + before any positive number, while a
space inserts a space before any positive number; these flags only
affect %d, %e, %f, and %g sequences, and the + flag takes precedence.
The # flag means to use an alternate display form for %o, %x, %X, %e,
%f, and %g sequences.  The - and 0 flags affect the width specifier,
as described below.

The width specifier supplies a lower limit for the length of the
printed representation.  The padding, if any, normally goes on the
left, but it goes on the right if the - flag is present.  The padding
character is normally a space, but it is 0 if the 0 flag is present.
The 0 flag is ignored if the - flag is present, or the format sequence
is something other than %d, %e, %f, and %g.

For %e, %f, and %g sequences, the number after the "." in the
precision specifier says how many decimal places to show; if zero, the
decimal point itself is omitted.  For %s and %S, the precision
specifier truncates the string to the given width.

usage: (format STRING &rest OBJECTS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  ptrdiff_t n;		/* The number of the next arg to substitute */
  char initial_buffer[4000];
  char *buf = initial_buffer;
  EMACS_INT bufsize = sizeof initial_buffer;
  EMACS_INT max_bufsize = STRING_BYTES_BOUND + 1;
  char *p;
  Lisp_Object buf_save_value IF_LINT (= {0});
  register char *format, *end, *format_start;
  EMACS_INT formatlen, nchars;
  /* Nonzero if the format is multibyte.  */
  int multibyte_format = 0;
  /* Nonzero if the output should be a multibyte string,
     which is true if any of the inputs is one.  */
  int multibyte = 0;
  /* When we make a multibyte string, we must pay attention to the
     byte combining problem, i.e., a byte may be combined with a
     multibyte character of the previous string.  This flag tells if we
     must consider such a situation or not.  */
  int maybe_combine_byte;
  Lisp_Object val;
  int arg_intervals = 0;
  USE_SAFE_ALLOCA;

  /* discarded[I] is 1 if byte I of the format
     string was not copied into the output.
     It is 2 if byte I was not the first byte of its character.  */
  char *discarded;

  /* Each element records, for one argument,
     the start and end bytepos in the output string,
     whether the argument has been converted to string (e.g., due to "%S"),
     and whether the argument is a string with intervals.
     info[0] is unused.  Unused elements have -1 for start.  */
  struct info
  {
    EMACS_INT start, end;
    int converted_to_string;
    int intervals;
  } *info = 0;

  /* It should not be necessary to GCPRO ARGS, because
     the caller in the interpreter should take care of that.  */

  CHECK_STRING (args[0]);
  format_start = SSDATA (args[0]);
  formatlen = SBYTES (args[0]);

  /* Allocate the info and discarded tables.  */
  {
    ptrdiff_t i;
    if ((SIZE_MAX - formatlen) / sizeof (struct info) <= nargs)
      memory_full (SIZE_MAX);
    SAFE_ALLOCA (info, struct info *, (nargs + 1) * sizeof *info + formatlen);
    discarded = (char *) &info[nargs + 1];
    for (i = 0; i < nargs + 1; i++)
      {
	info[i].start = -1;
	info[i].intervals = info[i].converted_to_string = 0;
      }
    memset (discarded, 0, formatlen);
  }

  /* Try to determine whether the result should be multibyte.
     This is not always right; sometimes the result needs to be multibyte
     because of an object that we will pass through prin1,
     and in that case, we won't know it here.  */
  multibyte_format = STRING_MULTIBYTE (args[0]);
  multibyte = multibyte_format;
  for (n = 1; !multibyte && n < nargs; n++)
    if (STRINGP (args[n]) && STRING_MULTIBYTE (args[n]))
      multibyte = 1;

  /* If we start out planning a unibyte result,
     then discover it has to be multibyte, we jump back to retry.  */
 retry:

  p = buf;
  nchars = 0;
  n = 0;

  /* Scan the format and store result in BUF.  */
  format = format_start;
  end = format + formatlen;
  maybe_combine_byte = 0;

  while (format != end)
    {
      /* The values of N and FORMAT when the loop body is entered.  */
      ptrdiff_t n0 = n;
      char *format0 = format;

      /* Bytes needed to represent the output of this conversion.  */
      EMACS_INT convbytes;

      if (*format == '%')
	{
	  /* General format specifications look like

	     '%' [flags] [field-width] [precision] format

	     where

	     flags ::= [-+0# ]+
	     field-width ::= [0-9]+
	     precision ::= '.' [0-9]*

	     If a field-width is specified, it specifies to which width
	     the output should be padded with blanks, if the output
	     string is shorter than field-width.

	     If precision is specified, it specifies the number of
	     digits to print after the '.' for floats, or the max.
	     number of chars to print from a string.  */

	  int minus_flag = 0;
	  int  plus_flag = 0;
	  int space_flag = 0;
	  int sharp_flag = 0;
	  int  zero_flag = 0;
	  EMACS_INT field_width;
	  int precision_given;
	  uintmax_t precision = UINTMAX_MAX;
	  char *num_end;
	  char conversion;

	  while (1)
	    {
	      switch (*++format)
		{
		case '-': minus_flag = 1; continue;
		case '+':  plus_flag = 1; continue;
		case ' ': space_flag = 1; continue;
		case '#': sharp_flag = 1; continue;
		case '0':  zero_flag = 1; continue;
		}
	      break;
	    }

	  /* Ignore flags when sprintf ignores them.  */
	  space_flag &= ~ plus_flag;
	  zero_flag &= ~ minus_flag;

	  {
	    uintmax_t w = strtoumax (format, &num_end, 10);
	    if (max_bufsize <= w)
	      string_overflow ();
	    field_width = w;
	  }
	  precision_given = *num_end == '.';
	  if (precision_given)
	    precision = strtoumax (num_end + 1, &num_end, 10);
	  format = num_end;

	  if (format == end)
	    error ("Format string ends in middle of format specifier");

	  memset (&discarded[format0 - format_start], 1, format - format0);
	  conversion = *format;
	  if (conversion == '%')
	    goto copy_char;
	  discarded[format - format_start] = 1;
	  format++;

	  ++n;
	  if (! (n < nargs))
	    error ("Not enough arguments for format string");

	  /* For 'S', prin1 the argument, and then treat like 's'.
	     For 's', princ any argument that is not a string or
	     symbol.  But don't do this conversion twice, which might
	     happen after retrying.  */
	  if ((conversion == 'S'
	       || (conversion == 's'
		   && ! STRINGP (args[n]) && ! SYMBOLP (args[n]))))
	    {
	      if (! info[n].converted_to_string)
		{
		  Lisp_Object noescape = conversion == 'S' ? Qnil : Qt;
		  args[n] = Fprin1_to_string (args[n], noescape);
		  info[n].converted_to_string = 1;
		  if (STRING_MULTIBYTE (args[n]) && ! multibyte)
		    {
		      multibyte = 1;
		      goto retry;
		    }
		}
	      conversion = 's';
	    }
	  else if (conversion == 'c')
	    {
	      if (FLOATP (args[n]))
		{
		  double d = XFLOAT_DATA (args[n]);
		  args[n] = make_number (FIXNUM_OVERFLOW_P (d) ? -1 : d);
		}

	      if (INTEGERP (args[n]) && ! ASCII_CHAR_P (XINT (args[n])))
		{
		  if (!multibyte)
		    {
		      multibyte = 1;
		      goto retry;
		    }
		  args[n] = Fchar_to_string (args[n]);
		  info[n].converted_to_string = 1;
		}

	      if (info[n].converted_to_string)
		conversion = 's';
	      zero_flag = 0;
	    }

	  if (SYMBOLP (args[n]))
	    {
	      args[n] = SYMBOL_NAME (args[n]);
	      if (STRING_MULTIBYTE (args[n]) && ! multibyte)
		{
		  multibyte = 1;
		  goto retry;
		}
	    }

	  if (conversion == 's')
	    {
	      /* handle case (precision[n] >= 0) */

	      EMACS_INT width, padding, nbytes;
	      EMACS_INT nchars_string;

	      EMACS_INT prec = -1;
	      if (precision_given && precision <= TYPE_MAXIMUM (EMACS_INT))
		prec = precision;

	      /* lisp_string_width ignores a precision of 0, but GNU
		 libc functions print 0 characters when the precision
		 is 0.  Imitate libc behavior here.  Changing
		 lisp_string_width is the right thing, and will be
		 done, but meanwhile we work with it. */

	      if (prec == 0)
		width = nchars_string = nbytes = 0;
	      else
		{
		  EMACS_INT nch, nby;
		  width = lisp_string_width (args[n], prec, &nch, &nby);
		  if (prec < 0)
		    {
		      nchars_string = SCHARS (args[n]);
		      nbytes = SBYTES (args[n]);
		    }
		  else
		    {
		      nchars_string = nch;
		      nbytes = nby;
		    }
		}

	      convbytes = nbytes;
	      if (convbytes && multibyte && ! STRING_MULTIBYTE (args[n]))
		convbytes = count_size_as_multibyte (SDATA (args[n]), nbytes);

	      padding = width < field_width ? field_width - width : 0;

	      if (max_bufsize - padding <= convbytes)
		string_overflow ();
	      convbytes += padding;
	      if (convbytes <= buf + bufsize - p)
		{
		  if (! minus_flag)
		    {
		      memset (p, ' ', padding);
		      p += padding;
		      nchars += padding;
		    }

		  if (p > buf
		      && multibyte
		      && !ASCII_BYTE_P (*((unsigned char *) p - 1))
		      && STRING_MULTIBYTE (args[n])
		      && !CHAR_HEAD_P (SREF (args[n], 0)))
		    maybe_combine_byte = 1;

		  p += copy_text (SDATA (args[n]), (unsigned char *) p,
				  nbytes,
				  STRING_MULTIBYTE (args[n]), multibyte);

                  info[n].start = nchars;
		  nchars += nchars_string;
		  info[n].end = nchars;

		  if (minus_flag)
		    {
		      memset (p, ' ', padding);
		      p += padding;
		      nchars += padding;
		    }

		  /* If this argument has text properties, record where
		     in the result string it appears.  */
		  if (STRING_INTERVALS (args[n]))
		    info[n].intervals = arg_intervals = 1;

		  continue;
		}
	    }
	  else if (! (conversion == 'c' || conversion == 'd'
		      || conversion == 'e' || conversion == 'f'
		      || conversion == 'g' || conversion == 'i'
		      || conversion == 'o' || conversion == 'x'
		      || conversion == 'X'))
	    error ("Invalid format operation %%%c",
		   STRING_CHAR ((unsigned char *) format - 1));
	  else if (! (INTEGERP (args[n]) || FLOATP (args[n])))
	    error ("Format specifier doesn't match argument type");
	  else
	    {
	      enum
	      {
		/* Maximum precision for a %f conversion such that the
		   trailing output digit might be nonzero.  Any precision
		   larger than this will not yield useful information.  */
		USEFUL_PRECISION_MAX =
		  ((1 - DBL_MIN_EXP)
		   * (FLT_RADIX == 2 || FLT_RADIX == 10 ? 1
		      : FLT_RADIX == 16 ? 4
		      : -1)),

		/* Maximum number of bytes generated by any format, if
		   precision is no more than USEFUL_PRECISION_MAX.
		   On all practical hosts, %f is the worst case.  */
		SPRINTF_BUFSIZE =
		  sizeof "-." + (DBL_MAX_10_EXP + 1) + USEFUL_PRECISION_MAX,

		/* Length of pM (that is, of pMd without the
		   trailing "d").  */
		pMlen = sizeof pMd - 2
	      };
	      verify (0 < USEFUL_PRECISION_MAX);

	      int prec;
	      EMACS_INT padding, sprintf_bytes;
	      uintmax_t excess_precision, numwidth;
	      uintmax_t leading_zeros = 0, trailing_zeros = 0;

	      char sprintf_buf[SPRINTF_BUFSIZE];

	      /* Copy of conversion specification, modified somewhat.
		 At most three flags F can be specified at once.  */
	      char convspec[sizeof "%FFF.*d" + pMlen];

	      /* Avoid undefined behavior in underlying sprintf.  */
	      if (conversion == 'd' || conversion == 'i')
		sharp_flag = 0;

	      /* Create the copy of the conversion specification, with
		 any width and precision removed, with ".*" inserted,
		 and with pM inserted for integer formats.  */
	      {
		char *f = convspec;
		*f++ = '%';
		*f = '-'; f += minus_flag;
		*f = '+'; f +=  plus_flag;
		*f = ' '; f += space_flag;
		*f = '#'; f += sharp_flag;
		*f = '0'; f +=  zero_flag;
                *f++ = '.';
                *f++ = '*';
		if (conversion == 'd' || conversion == 'i'
		    || conversion == 'o' || conversion == 'x'
		    || conversion == 'X')
		  {
		    memcpy (f, pMd, pMlen);
		    f += pMlen;
		    zero_flag &= ~ precision_given;
		  }
		*f++ = conversion;
		*f = '\0';
	      }

	      prec = -1;
	      if (precision_given)
		prec = min (precision, USEFUL_PRECISION_MAX);

	      /* Use sprintf to format this number into sprintf_buf.  Omit
		 padding and excess precision, though, because sprintf limits
		 output length to INT_MAX.

		 There are four types of conversion: double, unsigned
		 char (passed as int), wide signed int, and wide
		 unsigned int.  Treat them separately because the
		 sprintf ABI is sensitive to which type is passed.  Be
		 careful about integer overflow, NaNs, infinities, and
		 conversions; for example, the min and max macros are
		 not suitable here.  */
	      if (conversion == 'e' || conversion == 'f' || conversion == 'g')
		{
		  double x = (INTEGERP (args[n])
			      ? XINT (args[n])
			      : XFLOAT_DATA (args[n]));
		  sprintf_bytes = sprintf (sprintf_buf, convspec, prec, x);
		}
	      else if (conversion == 'c')
		{
		  /* Don't use sprintf here, as it might mishandle prec.  */
		  sprintf_buf[0] = XINT (args[n]);
		  sprintf_bytes = prec != 0;
		}
	      else if (conversion == 'd')
		{
		  /* For float, maybe we should use "%1.0f"
		     instead so it also works for values outside
		     the integer range.  */
		  printmax_t x;
		  if (INTEGERP (args[n]))
		    x = XINT (args[n]);
		  else
		    {
		      double d = XFLOAT_DATA (args[n]);
		      if (d < 0)
			{
			  x = TYPE_MINIMUM (printmax_t);
			  if (x < d)
			    x = d;
			}
		      else
			{
			  x = TYPE_MAXIMUM (printmax_t);
			  if (d < x)
			    x = d;
			}
		    }
		  sprintf_bytes = sprintf (sprintf_buf, convspec, prec, x);
		}
	      else
		{
		  /* Don't sign-extend for octal or hex printing.  */
		  uprintmax_t x;
		  if (INTEGERP (args[n]))
		    x = XUINT (args[n]);
		  else
		    {
		      double d = XFLOAT_DATA (args[n]);
		      if (d < 0)
			x = 0;
		      else
			{
			  x = TYPE_MAXIMUM (uprintmax_t);
			  if (d < x)
			    x = d;
			}
		    }
		  sprintf_bytes = sprintf (sprintf_buf, convspec, prec, x);
		}

	      /* Now the length of the formatted item is known, except it omits
		 padding and excess precision.  Deal with excess precision
		 first.  This happens only when the format specifies
		 ridiculously large precision.  */
	      excess_precision = precision - prec;
	      if (excess_precision)
		{
		  if (conversion == 'e' || conversion == 'f'
		      || conversion == 'g')
		    {
		      if ((conversion == 'g' && ! sharp_flag)
			  || ! ('0' <= sprintf_buf[sprintf_bytes - 1]
				&& sprintf_buf[sprintf_bytes - 1] <= '9'))
			excess_precision = 0;
		      else
			{
			  if (conversion == 'g')
			    {
			      char *dot = strchr (sprintf_buf, '.');
			      if (!dot)
				excess_precision = 0;
			    }
			}
		      trailing_zeros = excess_precision;
		    }
		  else
		    leading_zeros = excess_precision;
		}

	      /* Compute the total bytes needed for this item, including
		 excess precision and padding.  */
	      numwidth = sprintf_bytes + excess_precision;
	      padding = numwidth < field_width ? field_width - numwidth : 0;
	      if (max_bufsize - sprintf_bytes <= excess_precision
		  || max_bufsize - padding <= numwidth)
		string_overflow ();
	      convbytes = numwidth + padding;

	      if (convbytes <= buf + bufsize - p)
		{
		  /* Copy the formatted item from sprintf_buf into buf,
		     inserting padding and excess-precision zeros.  */

                  char *src = sprintf_buf;
		  char src0 = src[0];
		  int exponent_bytes = 0;
		  int signedp = src0 == '-' || src0 == '+' || src0 == ' ';
		  int significand_bytes;
		  if (zero_flag
		      && ((src[signedp] >= '0' && src[signedp] <= '9')
			  || (src[signedp] >= 'a' && src[signedp] <= 'f')
			  || (src[signedp] >= 'A' && src[signedp] <= 'F')))
		    {
		      leading_zeros += padding;
		      padding = 0;
		    }

		  if (excess_precision
		      && (conversion == 'e' || conversion == 'g'))
		    {
		      char *e = strchr (src, 'e');
		      if (e)
			exponent_bytes = src + sprintf_bytes - e;
		    }

		  if (! minus_flag)
		    {
		      memset (p, ' ', padding);
		      p += padding;
		      nchars += padding;
		    }

		  *p = src0;
		  src += signedp;
		  p += signedp;
		  memset (p, '0', leading_zeros);
		  p += leading_zeros;
		  significand_bytes = sprintf_bytes - signedp - exponent_bytes;
		  memcpy (p, src, significand_bytes);
                  p += significand_bytes;
		  src += significand_bytes;
		  memset (p, '0', trailing_zeros);
		  p += trailing_zeros;
		  memcpy (p, src, exponent_bytes);
		  p += exponent_bytes;

                  info[n].start = nchars;
		  nchars += leading_zeros + sprintf_bytes + trailing_zeros;
		  info[n].end = nchars;

		  if (minus_flag)
		    {
		      memset (p, ' ', padding);
		      p += padding;
		      nchars += padding;
		    }

		  continue;
		}
	    }
	}
      else
      copy_char:
	{
	  /* Copy a single character from format to buf.  */

	  char *src = format;
	  unsigned char str[MAX_MULTIBYTE_LENGTH];

	  if (multibyte_format)
	    {
	      /* Copy a whole multibyte character.  */
	      if (p > buf
		  && !ASCII_BYTE_P (*((unsigned char *) p - 1))
		  && !CHAR_HEAD_P (*format))
		maybe_combine_byte = 1;

	      do
		format++;
	      while (! CHAR_HEAD_P (*format));

	      convbytes = format - src;
	      memset (&discarded[src + 1 - format_start], 2, convbytes - 1);
	    }
	  else
	    {
	      unsigned char uc = *format++;
	      if (! multibyte || ASCII_BYTE_P (uc))
		convbytes = 1;
	      else
		{
		  int c = BYTE8_TO_CHAR (uc);
		  convbytes = CHAR_STRING (c, str);
		  src = (char *) str;
		}
	    }

	  if (convbytes <= buf + bufsize - p)
	    {
	      memcpy (p, src, convbytes);
	      p += convbytes;
	      nchars++;
	      continue;
	    }
	}

      /* There wasn't enough room to store this conversion or single
	 character.  CONVBYTES says how much room is needed.  Allocate
	 enough room (and then some) and do it again.  */
      {
	ptrdiff_t used = p - buf;

	if (max_bufsize - used < convbytes)
	  string_overflow ();
	bufsize = used + convbytes;
	bufsize = bufsize < max_bufsize / 2 ? bufsize * 2 : max_bufsize;

	if (buf == initial_buffer)
	  {
	    buf = xmalloc (bufsize);
	    sa_must_free = 1;
	    buf_save_value = make_save_value (buf, 0);
	    record_unwind_protect (safe_alloca_unwind, buf_save_value);
	    memcpy (buf, initial_buffer, used);
	  }
	else
	  XSAVE_VALUE (buf_save_value)->pointer = buf = xrealloc (buf, bufsize);

	p = buf + used;
      }

      format = format0;
      n = n0;
    }

  if (bufsize < p - buf)
    abort ();

  if (maybe_combine_byte)
    nchars = multibyte_chars_in_text ((unsigned char *) buf, p - buf);
  val = make_specified_string (buf, nchars, p - buf, multibyte);

  /* If we allocated BUF with malloc, free it too.  */
  SAFE_FREE ();

  /* If the format string has text properties, or any of the string
     arguments has text properties, set up text properties of the
     result string.  */

  if (STRING_INTERVALS (args[0]) || arg_intervals)
    {
      Lisp_Object len, new_len, props;
      struct gcpro gcpro1;

      /* Add text properties from the format string.  */
      len = make_number (SCHARS (args[0]));
      props = text_property_list (args[0], make_number (0), len, Qnil);
      GCPRO1 (props);

      if (CONSP (props))
	{
	  EMACS_INT bytepos = 0, position = 0, translated = 0;
	  EMACS_INT argn = 1;
	  Lisp_Object list;

	  /* Adjust the bounds of each text property
	     to the proper start and end in the output string.  */

	  /* Put the positions in PROPS in increasing order, so that
	     we can do (effectively) one scan through the position
	     space of the format string.  */
	  props = Fnreverse (props);

	  /* BYTEPOS is the byte position in the format string,
	     POSITION is the untranslated char position in it,
	     TRANSLATED is the translated char position in BUF,
	     and ARGN is the number of the next arg we will come to.  */
	  for (list = props; CONSP (list); list = XCDR (list))
	    {
	      Lisp_Object item;
	      EMACS_INT pos;

	      item = XCAR (list);

	      /* First adjust the property start position.  */
	      pos = XINT (XCAR (item));

	      /* Advance BYTEPOS, POSITION, TRANSLATED and ARGN
		 up to this position.  */
	      for (; position < pos; bytepos++)
		{
		  if (! discarded[bytepos])
		    position++, translated++;
		  else if (discarded[bytepos] == 1)
		    {
		      position++;
		      if (translated == info[argn].start)
			{
			  translated += info[argn].end - info[argn].start;
			  argn++;
			}
		    }
		}

	      XSETCAR (item, make_number (translated));

	      /* Likewise adjust the property end position.  */
	      pos = XINT (XCAR (XCDR (item)));

	      for (; position < pos; bytepos++)
		{
		  if (! discarded[bytepos])
		    position++, translated++;
		  else if (discarded[bytepos] == 1)
		    {
		      position++;
		      if (translated == info[argn].start)
			{
			  translated += info[argn].end - info[argn].start;
			  argn++;
			}
		    }
		}

	      XSETCAR (XCDR (item), make_number (translated));
	    }

	  add_text_properties_from_list (val, props, make_number (0));
	}

      /* Add text properties from arguments.  */
      if (arg_intervals)
	for (n = 1; n < nargs; ++n)
	  if (info[n].intervals)
	    {
	      len = make_number (SCHARS (args[n]));
	      new_len = make_number (info[n].end - info[n].start);
	      props = text_property_list (args[n], make_number (0), len, Qnil);
	      props = extend_property_ranges (props, new_len);
	      /* If successive arguments have properties, be sure that
		 the value of `composition' property be the copy.  */
	      if (n > 1 && info[n - 1].end)
		make_composition_value_copy (props);
	      add_text_properties_from_list (val, props,
					     make_number (info[n].start));
	    }

      UNGCPRO;
    }

  return val;
}

Lisp_Object
format2 (const char *string1, Lisp_Object arg0, Lisp_Object arg1)
{
  Lisp_Object args[3];
  args[0] = build_string (string1);
  args[1] = arg0;
  args[2] = arg1;
  return Fformat (3, args);
}

DEFUN ("char-equal", Fchar_equal, Schar_equal, 2, 2, 0,
       doc: /* Return t if two characters match, optionally ignoring case.
Both arguments must be characters (i.e. integers).
Case is ignored if `case-fold-search' is non-nil in the current buffer.  */)
  (register Lisp_Object c1, Lisp_Object c2)
{
  int i1, i2;
  /* Check they're chars, not just integers, otherwise we could get array
     bounds violations in downcase.  */
  CHECK_CHARACTER (c1);
  CHECK_CHARACTER (c2);

  if (XINT (c1) == XINT (c2))
    return Qt;
  if (NILP (BVAR (current_buffer, case_fold_search)))
    return Qnil;

  i1 = XFASTINT (c1);
  if (NILP (BVAR (current_buffer, enable_multibyte_characters))
      && ! ASCII_CHAR_P (i1))
    {
      MAKE_CHAR_MULTIBYTE (i1);
    }
  i2 = XFASTINT (c2);
  if (NILP (BVAR (current_buffer, enable_multibyte_characters))
      && ! ASCII_CHAR_P (i2))
    {
      MAKE_CHAR_MULTIBYTE (i2);
    }
  return (downcase (i1) == downcase (i2) ? Qt :  Qnil);
}

/* Transpose the markers in two regions of the current buffer, and
   adjust the ones between them if necessary (i.e.: if the regions
   differ in size).

   START1, END1 are the character positions of the first region.
   START1_BYTE, END1_BYTE are the byte positions.
   START2, END2 are the character positions of the second region.
   START2_BYTE, END2_BYTE are the byte positions.

   Traverses the entire marker list of the buffer to do so, adding an
   appropriate amount to some, subtracting from some, and leaving the
   rest untouched.  Most of this is copied from adjust_markers in insdel.c.

   It's the caller's job to ensure that START1 <= END1 <= START2 <= END2.  */

static void
transpose_markers (EMACS_INT start1, EMACS_INT end1,
		   EMACS_INT start2, EMACS_INT end2,
		   EMACS_INT start1_byte, EMACS_INT end1_byte,
		   EMACS_INT start2_byte, EMACS_INT end2_byte)
{
  register EMACS_INT amt1, amt1_byte, amt2, amt2_byte, diff, diff_byte, mpos;
  register struct Lisp_Marker *marker;

  /* Update point as if it were a marker.  */
  if (PT < start1)
    ;
  else if (PT < end1)
    TEMP_SET_PT_BOTH (PT + (end2 - end1),
		      PT_BYTE + (end2_byte - end1_byte));
  else if (PT < start2)
    TEMP_SET_PT_BOTH (PT + (end2 - start2) - (end1 - start1),
		      (PT_BYTE + (end2_byte - start2_byte)
		       - (end1_byte - start1_byte)));
  else if (PT < end2)
    TEMP_SET_PT_BOTH (PT - (start2 - start1),
		      PT_BYTE - (start2_byte - start1_byte));

  /* We used to adjust the endpoints here to account for the gap, but that
     isn't good enough.  Even if we assume the caller has tried to move the
     gap out of our way, it might still be at start1 exactly, for example;
     and that places it `inside' the interval, for our purposes.  The amount
     of adjustment is nontrivial if there's a `denormalized' marker whose
     position is between GPT and GPT + GAP_SIZE, so it's simpler to leave
     the dirty work to Fmarker_position, below.  */

  /* The difference between the region's lengths */
  diff = (end2 - start2) - (end1 - start1);
  diff_byte = (end2_byte - start2_byte) - (end1_byte - start1_byte);

  /* For shifting each marker in a region by the length of the other
     region plus the distance between the regions.  */
  amt1 = (end2 - start2) + (start2 - end1);
  amt2 = (end1 - start1) + (start2 - end1);
  amt1_byte = (end2_byte - start2_byte) + (start2_byte - end1_byte);
  amt2_byte = (end1_byte - start1_byte) + (start2_byte - end1_byte);

  for (marker = BUF_MARKERS (current_buffer); marker; marker = marker->next)
    {
      mpos = marker->bytepos;
      if (mpos >= start1_byte && mpos < end2_byte)
	{
	  if (mpos < end1_byte)
	    mpos += amt1_byte;
	  else if (mpos < start2_byte)
	    mpos += diff_byte;
	  else
	    mpos -= amt2_byte;
	  marker->bytepos = mpos;
	}
      mpos = marker->charpos;
      if (mpos >= start1 && mpos < end2)
	{
	  if (mpos < end1)
	    mpos += amt1;
	  else if (mpos < start2)
	    mpos += diff;
	  else
	    mpos -= amt2;
	}
      marker->charpos = mpos;
    }
}

DEFUN ("transpose-regions", Ftranspose_regions, Stranspose_regions, 4, 5, 0,
       doc: /* Transpose region STARTR1 to ENDR1 with STARTR2 to ENDR2.
The regions should not be overlapping, because the size of the buffer is
never changed in a transposition.

Optional fifth arg LEAVE-MARKERS, if non-nil, means don't update
any markers that happen to be located in the regions.

Transposing beyond buffer boundaries is an error.  */)
  (Lisp_Object startr1, Lisp_Object endr1, Lisp_Object startr2, Lisp_Object endr2, Lisp_Object leave_markers)
{
  register EMACS_INT start1, end1, start2, end2;
  EMACS_INT start1_byte, start2_byte, len1_byte, len2_byte;
  EMACS_INT gap, len1, len_mid, len2;
  unsigned char *start1_addr, *start2_addr, *temp;

  INTERVAL cur_intv, tmp_interval1, tmp_interval_mid, tmp_interval2, tmp_interval3;
  Lisp_Object buf;

  XSETBUFFER (buf, current_buffer);
  cur_intv = BUF_INTERVALS (current_buffer);

  validate_region (&startr1, &endr1);
  validate_region (&startr2, &endr2);

  start1 = XFASTINT (startr1);
  end1 = XFASTINT (endr1);
  start2 = XFASTINT (startr2);
  end2 = XFASTINT (endr2);
  gap = GPT;

  /* Swap the regions if they're reversed.  */
  if (start2 < end1)
    {
      register EMACS_INT glumph = start1;
      start1 = start2;
      start2 = glumph;
      glumph = end1;
      end1 = end2;
      end2 = glumph;
    }

  len1 = end1 - start1;
  len2 = end2 - start2;

  if (start2 < end1)
    error ("Transposed regions overlap");
  /* Nothing to change for adjacent regions with one being empty */
  else if ((start1 == end1 || start2 == end2) && end1 == start2)
    return Qnil;

  /* The possibilities are:
     1. Adjacent (contiguous) regions, or separate but equal regions
     (no, really equal, in this case!), or
     2. Separate regions of unequal size.

     The worst case is usually No. 2.  It means that (aside from
     potential need for getting the gap out of the way), there also
     needs to be a shifting of the text between the two regions.  So
     if they are spread far apart, we are that much slower... sigh.  */

  /* It must be pointed out that the really studly thing to do would
     be not to move the gap at all, but to leave it in place and work
     around it if necessary.  This would be extremely efficient,
     especially considering that people are likely to do
     transpositions near where they are working interactively, which
     is exactly where the gap would be found.  However, such code
     would be much harder to write and to read.  So, if you are
     reading this comment and are feeling squirrely, by all means have
     a go!  I just didn't feel like doing it, so I will simply move
     the gap the minimum distance to get it out of the way, and then
     deal with an unbroken array.  */

  /* Make sure the gap won't interfere, by moving it out of the text
     we will operate on.  */
  if (start1 < gap && gap < end2)
    {
      if (gap - start1 < end2 - gap)
	move_gap (start1);
      else
	move_gap (end2);
    }

  start1_byte = CHAR_TO_BYTE (start1);
  start2_byte = CHAR_TO_BYTE (start2);
  len1_byte = CHAR_TO_BYTE (end1) - start1_byte;
  len2_byte = CHAR_TO_BYTE (end2) - start2_byte;

#ifdef BYTE_COMBINING_DEBUG
  if (end1 == start2)
    {
      if (count_combining_before (BYTE_POS_ADDR (start2_byte),
				  len2_byte, start1, start1_byte)
	  || count_combining_before (BYTE_POS_ADDR (start1_byte),
				     len1_byte, end2, start2_byte + len2_byte)
	  || count_combining_after (BYTE_POS_ADDR (start1_byte),
				    len1_byte, end2, start2_byte + len2_byte))
	abort ();
    }
  else
    {
      if (count_combining_before (BYTE_POS_ADDR (start2_byte),
				  len2_byte, start1, start1_byte)
	  || count_combining_before (BYTE_POS_ADDR (start1_byte),
				     len1_byte, start2, start2_byte)
	  || count_combining_after (BYTE_POS_ADDR (start2_byte),
				    len2_byte, end1, start1_byte + len1_byte)
	  || count_combining_after (BYTE_POS_ADDR (start1_byte),
				    len1_byte, end2, start2_byte + len2_byte))
	abort ();
    }
#endif

  /* Hmmm... how about checking to see if the gap is large
     enough to use as the temporary storage?  That would avoid an
     allocation... interesting.  Later, don't fool with it now.  */

  /* Working without memmove, for portability (sigh), so must be
     careful of overlapping subsections of the array...  */

  if (end1 == start2)		/* adjacent regions */
    {
      modify_region (current_buffer, start1, end2, 0);
      record_change (start1, len1 + len2);

      tmp_interval1 = copy_intervals (cur_intv, start1, len1);
      tmp_interval2 = copy_intervals (cur_intv, start2, len2);
      /* Don't use Fset_text_properties: that can cause GC, which can
	 clobber objects stored in the tmp_intervals.  */
      tmp_interval3 = validate_interval_range (buf, &startr1, &endr2, 0);
      if (!NULL_INTERVAL_P (tmp_interval3))
	set_text_properties_1 (startr1, endr2, Qnil, buf, tmp_interval3);

      /* First region smaller than second.  */
      if (len1_byte < len2_byte)
        {
	  USE_SAFE_ALLOCA;

	  SAFE_ALLOCA (temp, unsigned char *, len2_byte);

	  /* Don't precompute these addresses.  We have to compute them
	     at the last minute, because the relocating allocator might
	     have moved the buffer around during the xmalloc.  */
	  start1_addr = BYTE_POS_ADDR (start1_byte);
	  start2_addr = BYTE_POS_ADDR (start2_byte);

          memcpy (temp, start2_addr, len2_byte);
          memcpy (start1_addr + len2_byte, start1_addr, len1_byte);
          memcpy (start1_addr, temp, len2_byte);
	  SAFE_FREE ();
        }
      else
	/* First region not smaller than second.  */
        {
	  USE_SAFE_ALLOCA;

	  SAFE_ALLOCA (temp, unsigned char *, len1_byte);
	  start1_addr = BYTE_POS_ADDR (start1_byte);
	  start2_addr = BYTE_POS_ADDR (start2_byte);
          memcpy (temp, start1_addr, len1_byte);
          memcpy (start1_addr, start2_addr, len2_byte);
          memcpy (start1_addr + len2_byte, temp, len1_byte);
	  SAFE_FREE ();
        }
      graft_intervals_into_buffer (tmp_interval1, start1 + len2,
                                   len1, current_buffer, 0);
      graft_intervals_into_buffer (tmp_interval2, start1,
                                   len2, current_buffer, 0);
      update_compositions (start1, start1 + len2, CHECK_BORDER);
      update_compositions (start1 + len2, end2, CHECK_TAIL);
    }
  /* Non-adjacent regions, because end1 != start2, bleagh...  */
  else
    {
      len_mid = start2_byte - (start1_byte + len1_byte);

      if (len1_byte == len2_byte)
	/* Regions are same size, though, how nice.  */
        {
	  USE_SAFE_ALLOCA;

          modify_region (current_buffer, start1, end1, 0);
          modify_region (current_buffer, start2, end2, 0);
          record_change (start1, len1);
          record_change (start2, len2);
          tmp_interval1 = copy_intervals (cur_intv, start1, len1);
          tmp_interval2 = copy_intervals (cur_intv, start2, len2);

	  tmp_interval3 = validate_interval_range (buf, &startr1, &endr1, 0);
	  if (!NULL_INTERVAL_P (tmp_interval3))
	    set_text_properties_1 (startr1, endr1, Qnil, buf, tmp_interval3);

	  tmp_interval3 = validate_interval_range (buf, &startr2, &endr2, 0);
	  if (!NULL_INTERVAL_P (tmp_interval3))
	    set_text_properties_1 (startr2, endr2, Qnil, buf, tmp_interval3);

	  SAFE_ALLOCA (temp, unsigned char *, len1_byte);
	  start1_addr = BYTE_POS_ADDR (start1_byte);
	  start2_addr = BYTE_POS_ADDR (start2_byte);
          memcpy (temp, start1_addr, len1_byte);
          memcpy (start1_addr, start2_addr, len2_byte);
          memcpy (start2_addr, temp, len1_byte);
	  SAFE_FREE ();

          graft_intervals_into_buffer (tmp_interval1, start2,
                                       len1, current_buffer, 0);
          graft_intervals_into_buffer (tmp_interval2, start1,
                                       len2, current_buffer, 0);
        }

      else if (len1_byte < len2_byte)	/* Second region larger than first */
        /* Non-adjacent & unequal size, area between must also be shifted.  */
        {
	  USE_SAFE_ALLOCA;

          modify_region (current_buffer, start1, end2, 0);
          record_change (start1, (end2 - start1));
          tmp_interval1 = copy_intervals (cur_intv, start1, len1);
          tmp_interval_mid = copy_intervals (cur_intv, end1, len_mid);
          tmp_interval2 = copy_intervals (cur_intv, start2, len2);

	  tmp_interval3 = validate_interval_range (buf, &startr1, &endr2, 0);
	  if (!NULL_INTERVAL_P (tmp_interval3))
	    set_text_properties_1 (startr1, endr2, Qnil, buf, tmp_interval3);

	  /* holds region 2 */
	  SAFE_ALLOCA (temp, unsigned char *, len2_byte);
	  start1_addr = BYTE_POS_ADDR (start1_byte);
	  start2_addr = BYTE_POS_ADDR (start2_byte);
          memcpy (temp, start2_addr, len2_byte);
          memcpy (start1_addr + len_mid + len2_byte, start1_addr, len1_byte);
          memmove (start1_addr + len2_byte, start1_addr + len1_byte, len_mid);
          memcpy (start1_addr, temp, len2_byte);
	  SAFE_FREE ();

          graft_intervals_into_buffer (tmp_interval1, end2 - len1,
                                       len1, current_buffer, 0);
          graft_intervals_into_buffer (tmp_interval_mid, start1 + len2,
                                       len_mid, current_buffer, 0);
          graft_intervals_into_buffer (tmp_interval2, start1,
                                       len2, current_buffer, 0);
        }
      else
	/* Second region smaller than first.  */
        {
	  USE_SAFE_ALLOCA;

          record_change (start1, (end2 - start1));
          modify_region (current_buffer, start1, end2, 0);

          tmp_interval1 = copy_intervals (cur_intv, start1, len1);
          tmp_interval_mid = copy_intervals (cur_intv, end1, len_mid);
          tmp_interval2 = copy_intervals (cur_intv, start2, len2);

	  tmp_interval3 = validate_interval_range (buf, &startr1, &endr2, 0);
	  if (!NULL_INTERVAL_P (tmp_interval3))
	    set_text_properties_1 (startr1, endr2, Qnil, buf, tmp_interval3);

	  /* holds region 1 */
	  SAFE_ALLOCA (temp, unsigned char *, len1_byte);
	  start1_addr = BYTE_POS_ADDR (start1_byte);
	  start2_addr = BYTE_POS_ADDR (start2_byte);
          memcpy (temp, start1_addr, len1_byte);
          memcpy (start1_addr, start2_addr, len2_byte);
          memcpy (start1_addr + len2_byte, start1_addr + len1_byte, len_mid);
          memcpy (start1_addr + len2_byte + len_mid, temp, len1_byte);
	  SAFE_FREE ();

          graft_intervals_into_buffer (tmp_interval1, end2 - len1,
                                       len1, current_buffer, 0);
          graft_intervals_into_buffer (tmp_interval_mid, start1 + len2,
                                       len_mid, current_buffer, 0);
          graft_intervals_into_buffer (tmp_interval2, start1,
                                       len2, current_buffer, 0);
        }

      update_compositions (start1, start1 + len2, CHECK_BORDER);
      update_compositions (end2 - len1, end2, CHECK_BORDER);
    }

  /* When doing multiple transpositions, it might be nice
     to optimize this.  Perhaps the markers in any one buffer
     should be organized in some sorted data tree.  */
  if (NILP (leave_markers))
    {
      transpose_markers (start1, end1, start2, end2,
			 start1_byte, start1_byte + len1_byte,
			 start2_byte, start2_byte + len2_byte);
      fix_start_end_in_overlays (start1, end2);
    }

  signal_after_change (start1, end2 - start1, end2 - start1);
  return Qnil;
}


void
syms_of_editfns (void)
{
  environbuf = 0;
  initial_tz = 0;

  DEFSYM (Qbuffer_access_fontify_functions, "buffer-access-fontify-functions");

  DEFVAR_LISP ("inhibit-field-text-motion", Vinhibit_field_text_motion,
	       doc: /* Non-nil means text motion commands don't notice fields.  */);
  Vinhibit_field_text_motion = Qnil;

  DEFVAR_LISP ("buffer-access-fontify-functions",
	       Vbuffer_access_fontify_functions,
	       doc: /* List of functions called by `buffer-substring' to fontify if necessary.
Each function is called with two arguments which specify the range
of the buffer being accessed.  */);
  Vbuffer_access_fontify_functions = Qnil;

  {
    Lisp_Object obuf;
    obuf = Fcurrent_buffer ();
    /* Do this here, because init_buffer_once is too early--it won't work.  */
    Fset_buffer (Vprin1_to_string_buffer);
    /* Make sure buffer-access-fontify-functions is nil in this buffer.  */
    Fset (Fmake_local_variable (intern_c_string ("buffer-access-fontify-functions")),
	  Qnil);
    Fset_buffer (obuf);
  }

  DEFVAR_LISP ("buffer-access-fontified-property",
	       Vbuffer_access_fontified_property,
	       doc: /* Property which (if non-nil) indicates text has been fontified.
`buffer-substring' need not call the `buffer-access-fontify-functions'
functions if all the text being accessed has this property.  */);
  Vbuffer_access_fontified_property = Qnil;

  DEFVAR_LISP ("system-name", Vsystem_name,
	       doc: /* The host name of the machine Emacs is running on.  */);

  DEFVAR_LISP ("user-full-name", Vuser_full_name,
	       doc: /* The full name of the user logged in.  */);

  DEFVAR_LISP ("user-login-name", Vuser_login_name,
	       doc: /* The user's name, taken from environment variables if possible.  */);

  DEFVAR_LISP ("user-real-login-name", Vuser_real_login_name,
	       doc: /* The user's name, based upon the real uid only.  */);

  DEFVAR_LISP ("operating-system-release", Voperating_system_release,
	       doc: /* The release of the operating system Emacs is running on.  */);

  defsubr (&Spropertize);
  defsubr (&Schar_equal);
  defsubr (&Sgoto_char);
  defsubr (&Sstring_to_char);
  defsubr (&Schar_to_string);
  defsubr (&Sbyte_to_string);
  defsubr (&Sbuffer_substring);
  defsubr (&Sbuffer_substring_no_properties);
  defsubr (&Sbuffer_string);

  defsubr (&Spoint_marker);
  defsubr (&Smark_marker);
  defsubr (&Spoint);
  defsubr (&Sregion_beginning);
  defsubr (&Sregion_end);

  DEFSYM (Qfield, "field");
  DEFSYM (Qboundary, "boundary");
  defsubr (&Sfield_beginning);
  defsubr (&Sfield_end);
  defsubr (&Sfield_string);
  defsubr (&Sfield_string_no_properties);
  defsubr (&Sdelete_field);
  defsubr (&Sconstrain_to_field);

  defsubr (&Sline_beginning_position);
  defsubr (&Sline_end_position);

/*  defsubr (&Smark); */
/*  defsubr (&Sset_mark); */
  defsubr (&Ssave_excursion);
  defsubr (&Ssave_current_buffer);

  defsubr (&Sbufsize);
  defsubr (&Spoint_max);
  defsubr (&Spoint_min);
  defsubr (&Spoint_min_marker);
  defsubr (&Spoint_max_marker);
  defsubr (&Sgap_position);
  defsubr (&Sgap_size);
  defsubr (&Sposition_bytes);
  defsubr (&Sbyte_to_position);

  defsubr (&Sbobp);
  defsubr (&Seobp);
  defsubr (&Sbolp);
  defsubr (&Seolp);
  defsubr (&Sfollowing_char);
  defsubr (&Sprevious_char);
  defsubr (&Schar_after);
  defsubr (&Schar_before);
  defsubr (&Sinsert);
  defsubr (&Sinsert_before_markers);
  defsubr (&Sinsert_and_inherit);
  defsubr (&Sinsert_and_inherit_before_markers);
  defsubr (&Sinsert_char);
  defsubr (&Sinsert_byte);

  defsubr (&Suser_login_name);
  defsubr (&Suser_real_login_name);
  defsubr (&Suser_uid);
  defsubr (&Suser_real_uid);
  defsubr (&Suser_full_name);
  defsubr (&Semacs_pid);
  defsubr (&Scurrent_time);
  defsubr (&Sget_internal_run_time);
  defsubr (&Sformat_time_string);
  defsubr (&Sfloat_time);
  defsubr (&Sdecode_time);
  defsubr (&Sencode_time);
  defsubr (&Scurrent_time_string);
  defsubr (&Scurrent_time_zone);
  defsubr (&Sset_time_zone_rule);
  defsubr (&Ssystem_name);
  defsubr (&Smessage);
  defsubr (&Smessage_box);
  defsubr (&Smessage_or_box);
  defsubr (&Scurrent_message);
  defsubr (&Sformat);

  defsubr (&Sinsert_buffer_substring);
  defsubr (&Scompare_buffer_substrings);
  defsubr (&Ssubst_char_in_region);
  defsubr (&Stranslate_region_internal);
  defsubr (&Sdelete_region);
  defsubr (&Sdelete_and_extract_region);
  defsubr (&Swiden);
  defsubr (&Snarrow_to_region);
  defsubr (&Ssave_restriction);
  defsubr (&Stranspose_regions);
}
