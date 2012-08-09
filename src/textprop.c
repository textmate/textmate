/* Interface code for dealing with text properties.
   Copyright (C) 1993-1995, 1997, 1999-2012 Free Software Foundation, Inc.

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
#include "lisp.h"
#include "intervals.h"
#include "buffer.h"
#include "window.h"

#ifndef NULL
#define NULL (void *)0
#endif

/* Test for membership, allowing for t (actually any non-cons) to mean the
   universal set.  */

#define TMEM(sym, set) (CONSP (set) ? ! NILP (Fmemq (sym, set)) : ! NILP (set))


/* NOTES:  previous- and next- property change will have to skip
  zero-length intervals if they are implemented.  This could be done
  inside next_interval and previous_interval.

  set_properties needs to deal with the interval property cache.

  It is assumed that for any interval plist, a property appears
  only once on the list.  Although some code i.e., remove_properties,
  handles the more general case, the uniqueness of properties is
  necessary for the system to remain consistent.  This requirement
  is enforced by the subrs installing properties onto the intervals.  */


/* Types of hooks.  */
static Lisp_Object Qmouse_left;
static Lisp_Object Qmouse_entered;
Lisp_Object Qpoint_left;
Lisp_Object Qpoint_entered;
Lisp_Object Qcategory;
Lisp_Object Qlocal_map;

/* Visual properties text (including strings) may have.  */
static Lisp_Object Qforeground, Qbackground, Qunderline;
Lisp_Object Qfont;
static Lisp_Object Qstipple;
Lisp_Object Qinvisible, Qintangible, Qmouse_face;
static Lisp_Object Qread_only;
Lisp_Object Qminibuffer_prompt;

/* Sticky properties */
Lisp_Object Qfront_sticky, Qrear_nonsticky;

/* If o1 is a cons whose cdr is a cons, return non-zero and set o2 to
   the o1's cdr.  Otherwise, return zero.  This is handy for
   traversing plists.  */
#define PLIST_ELT_P(o1, o2) (CONSP (o1) && ((o2)=XCDR (o1), CONSP (o2)))

/* verify_interval_modification saves insertion hooks here
   to be run later by report_interval_modification.  */
static Lisp_Object interval_insert_behind_hooks;
static Lisp_Object interval_insert_in_front_hooks;

static void text_read_only (Lisp_Object) NO_RETURN;
static Lisp_Object Fprevious_property_change (Lisp_Object, Lisp_Object,
					      Lisp_Object);


/* Signal a `text-read-only' error.  This function makes it easier
   to capture that error in GDB by putting a breakpoint on it.  */

static void
text_read_only (Lisp_Object propval)
{
  if (STRINGP (propval))
    xsignal1 (Qtext_read_only, propval);

  xsignal0 (Qtext_read_only);
}



/* Extract the interval at the position pointed to by BEGIN from
   OBJECT, a string or buffer.  Additionally, check that the positions
   pointed to by BEGIN and END are within the bounds of OBJECT, and
   reverse them if *BEGIN is greater than *END.  The objects pointed
   to by BEGIN and END may be integers or markers; if the latter, they
   are coerced to integers.

   When OBJECT is a string, we increment *BEGIN and *END
   to make them origin-one.

   Note that buffer points don't correspond to interval indices.
   For example, point-max is 1 greater than the index of the last
   character.  This difference is handled in the caller, which uses
   the validated points to determine a length, and operates on that.
   Exceptions are Ftext_properties_at, Fnext_property_change, and
   Fprevious_property_change which call this function with BEGIN == END.
   Handle this case specially.

   If FORCE is soft (0), it's OK to return NULL_INTERVAL.  Otherwise,
   create an interval tree for OBJECT if one doesn't exist, provided
   the object actually contains text.  In the current design, if there
   is no text, there can be no text properties.  */

#define soft 0
#define hard 1

INTERVAL
validate_interval_range (Lisp_Object object, Lisp_Object *begin, Lisp_Object *end, int force)
{
  register INTERVAL i;
  EMACS_INT searchpos;

  CHECK_STRING_OR_BUFFER (object);
  CHECK_NUMBER_COERCE_MARKER (*begin);
  CHECK_NUMBER_COERCE_MARKER (*end);

  /* If we are asked for a point, but from a subr which operates
     on a range, then return nothing.  */
  if (EQ (*begin, *end) && begin != end)
    return NULL_INTERVAL;

  if (XINT (*begin) > XINT (*end))
    {
      Lisp_Object n;
      n = *begin;
      *begin = *end;
      *end = n;
    }

  if (BUFFERP (object))
    {
      register struct buffer *b = XBUFFER (object);

      if (!(BUF_BEGV (b) <= XINT (*begin) && XINT (*begin) <= XINT (*end)
	    && XINT (*end) <= BUF_ZV (b)))
	args_out_of_range (*begin, *end);
      i = BUF_INTERVALS (b);

      /* If there's no text, there are no properties.  */
      if (BUF_BEGV (b) == BUF_ZV (b))
	return NULL_INTERVAL;

      searchpos = XINT (*begin);
    }
  else
    {
      EMACS_INT len = SCHARS (object);

      if (! (0 <= XINT (*begin) && XINT (*begin) <= XINT (*end)
	     && XINT (*end) <= len))
	args_out_of_range (*begin, *end);
      XSETFASTINT (*begin, XFASTINT (*begin));
      if (begin != end)
	XSETFASTINT (*end, XFASTINT (*end));
      i = STRING_INTERVALS (object);

      if (len == 0)
	return NULL_INTERVAL;

      searchpos = XINT (*begin);
    }

  if (NULL_INTERVAL_P (i))
    return (force ? create_root_interval (object) : i);

  return find_interval (i, searchpos);
}

/* Validate LIST as a property list.  If LIST is not a list, then
   make one consisting of (LIST nil).  Otherwise, verify that LIST
   is even numbered and thus suitable as a plist.  */

static Lisp_Object
validate_plist (Lisp_Object list)
{
  if (NILP (list))
    return Qnil;

  if (CONSP (list))
    {
      register int i;
      register Lisp_Object tail;
      for (i = 0, tail = list; CONSP (tail); i++)
	{
	  tail = XCDR (tail);
	  QUIT;
	}
      if (i & 1)
	error ("Odd length text property list");
      return list;
    }

  return Fcons (list, Fcons (Qnil, Qnil));
}

/* Return nonzero if interval I has all the properties,
   with the same values, of list PLIST.  */

static int
interval_has_all_properties (Lisp_Object plist, INTERVAL i)
{
  register Lisp_Object tail1, tail2, sym1;
  register int found;

  /* Go through each element of PLIST.  */
  for (tail1 = plist; CONSP (tail1); tail1 = Fcdr (XCDR (tail1)))
    {
      sym1 = XCAR (tail1);
      found = 0;

      /* Go through I's plist, looking for sym1 */
      for (tail2 = i->plist; CONSP (tail2); tail2 = Fcdr (XCDR (tail2)))
	if (EQ (sym1, XCAR (tail2)))
	  {
	    /* Found the same property on both lists.  If the
	       values are unequal, return zero.  */
	    if (! EQ (Fcar (XCDR (tail1)), Fcar (XCDR (tail2))))
	      return 0;

	    /* Property has same value on both lists; go to next one.  */
	    found = 1;
	    break;
	  }

      if (! found)
	return 0;
    }

  return 1;
}

/* Return nonzero if the plist of interval I has any of the
   properties of PLIST, regardless of their values.  */

static inline int
interval_has_some_properties (Lisp_Object plist, INTERVAL i)
{
  register Lisp_Object tail1, tail2, sym;

  /* Go through each element of PLIST.  */
  for (tail1 = plist; CONSP (tail1); tail1 = Fcdr (XCDR (tail1)))
    {
      sym = XCAR (tail1);

      /* Go through i's plist, looking for tail1 */
      for (tail2 = i->plist; CONSP (tail2); tail2 = Fcdr (XCDR (tail2)))
	if (EQ (sym, XCAR (tail2)))
	  return 1;
    }

  return 0;
}

/* Return nonzero if the plist of interval I has any of the
   property names in LIST, regardless of their values.  */

static inline int
interval_has_some_properties_list (Lisp_Object list, INTERVAL i)
{
  register Lisp_Object tail1, tail2, sym;

  /* Go through each element of LIST.  */
  for (tail1 = list; CONSP (tail1); tail1 = XCDR (tail1))
    {
      sym = Fcar (tail1);

      /* Go through i's plist, looking for tail1 */
      for (tail2 = i->plist; CONSP (tail2); tail2 = XCDR (XCDR (tail2)))
	if (EQ (sym, XCAR (tail2)))
	  return 1;
    }

  return 0;
}

/* Changing the plists of individual intervals.  */

/* Return the value of PROP in property-list PLIST, or Qunbound if it
   has none.  */
static Lisp_Object
property_value (Lisp_Object plist, Lisp_Object prop)
{
  Lisp_Object value;

  while (PLIST_ELT_P (plist, value))
    if (EQ (XCAR (plist), prop))
      return XCAR (value);
    else
      plist = XCDR (value);

  return Qunbound;
}

/* Set the properties of INTERVAL to PROPERTIES,
   and record undo info for the previous values.
   OBJECT is the string or buffer that INTERVAL belongs to.  */

static void
set_properties (Lisp_Object properties, INTERVAL interval, Lisp_Object object)
{
  Lisp_Object sym, value;

  if (BUFFERP (object))
    {
      /* For each property in the old plist which is missing from PROPERTIES,
	 or has a different value in PROPERTIES, make an undo record.  */
      for (sym = interval->plist;
	   PLIST_ELT_P (sym, value);
	   sym = XCDR (value))
	if (! EQ (property_value (properties, XCAR (sym)),
		  XCAR (value)))
	  {
	    record_property_change (interval->position, LENGTH (interval),
				    XCAR (sym), XCAR (value),
				    object);
	  }

      /* For each new property that has no value at all in the old plist,
	 make an undo record binding it to nil, so it will be removed.  */
      for (sym = properties;
	   PLIST_ELT_P (sym, value);
	   sym = XCDR (value))
	if (EQ (property_value (interval->plist, XCAR (sym)), Qunbound))
	  {
	    record_property_change (interval->position, LENGTH (interval),
				    XCAR (sym), Qnil,
				    object);
	  }
    }

  /* Store new properties.  */
  interval->plist = Fcopy_sequence (properties);
}

/* Add the properties of PLIST to the interval I, or set
   the value of I's property to the value of the property on PLIST
   if they are different.

   OBJECT should be the string or buffer the interval is in.

   Return nonzero if this changes I (i.e., if any members of PLIST
   are actually added to I's plist) */

static int
add_properties (Lisp_Object plist, INTERVAL i, Lisp_Object object)
{
  Lisp_Object tail1, tail2, sym1, val1;
  register int changed = 0;
  register int found;
  struct gcpro gcpro1, gcpro2, gcpro3;

  tail1 = plist;
  sym1 = Qnil;
  val1 = Qnil;
  /* No need to protect OBJECT, because we can GC only in the case
     where it is a buffer, and live buffers are always protected.
     I and its plist are also protected, via OBJECT.  */
  GCPRO3 (tail1, sym1, val1);

  /* Go through each element of PLIST.  */
  for (tail1 = plist; CONSP (tail1); tail1 = Fcdr (XCDR (tail1)))
    {
      sym1 = XCAR (tail1);
      val1 = Fcar (XCDR (tail1));
      found = 0;

      /* Go through I's plist, looking for sym1 */
      for (tail2 = i->plist; CONSP (tail2); tail2 = Fcdr (XCDR (tail2)))
	if (EQ (sym1, XCAR (tail2)))
	  {
	    /* No need to gcpro, because tail2 protects this
	       and it must be a cons cell (we get an error otherwise).  */
	    register Lisp_Object this_cdr;

	    this_cdr = XCDR (tail2);
	    /* Found the property.  Now check its value.  */
	    found = 1;

	    /* The properties have the same value on both lists.
	       Continue to the next property.  */
	    if (EQ (val1, Fcar (this_cdr)))
	      break;

	    /* Record this change in the buffer, for undo purposes.  */
	    if (BUFFERP (object))
	      {
		record_property_change (i->position, LENGTH (i),
					sym1, Fcar (this_cdr), object);
	      }

	    /* I's property has a different value -- change it */
	    Fsetcar (this_cdr, val1);
	    changed++;
	    break;
	  }

      if (! found)
	{
	  /* Record this change in the buffer, for undo purposes.  */
	  if (BUFFERP (object))
	    {
	      record_property_change (i->position, LENGTH (i),
				      sym1, Qnil, object);
	    }
	  i->plist = Fcons (sym1, Fcons (val1, i->plist));
	  changed++;
	}
    }

  UNGCPRO;

  return changed;
}

/* For any members of PLIST, or LIST,
   which are properties of I, remove them from I's plist.
   (If PLIST is non-nil, use that, otherwise use LIST.)
   OBJECT is the string or buffer containing I.  */

static int
remove_properties (Lisp_Object plist, Lisp_Object list, INTERVAL i, Lisp_Object object)
{
  register Lisp_Object tail1, tail2, sym, current_plist;
  register int changed = 0;

  /* Nonzero means tail1 is a plist, otherwise it is a list.  */
  int use_plist;

  current_plist = i->plist;

  if (! NILP (plist))
    tail1 = plist, use_plist = 1;
  else
    tail1 = list, use_plist = 0;

  /* Go through each element of LIST or PLIST.  */
  while (CONSP (tail1))
    {
      sym = XCAR (tail1);

      /* First, remove the symbol if it's at the head of the list */
      while (CONSP (current_plist) && EQ (sym, XCAR (current_plist)))
	{
	  if (BUFFERP (object))
	    record_property_change (i->position, LENGTH (i),
				    sym, XCAR (XCDR (current_plist)),
				    object);

	  current_plist = XCDR (XCDR (current_plist));
	  changed++;
	}

      /* Go through I's plist, looking for SYM.  */
      tail2 = current_plist;
      while (! NILP (tail2))
	{
	  register Lisp_Object this;
	  this = XCDR (XCDR (tail2));
	  if (CONSP (this) && EQ (sym, XCAR (this)))
	    {
	      if (BUFFERP (object))
		record_property_change (i->position, LENGTH (i),
					sym, XCAR (XCDR (this)), object);

	      Fsetcdr (XCDR (tail2), XCDR (XCDR (this)));
	      changed++;
	    }
	  tail2 = this;
	}

      /* Advance thru TAIL1 one way or the other.  */
      tail1 = XCDR (tail1);
      if (use_plist && CONSP (tail1))
	tail1 = XCDR (tail1);
    }

  if (changed)
    i->plist = current_plist;
  return changed;
}

#if 0
/* Remove all properties from interval I.  Return non-zero
   if this changes the interval.  */

static inline int
erase_properties (INTERVAL i)
{
  if (NILP (i->plist))
    return 0;

  i->plist = Qnil;
  return 1;
}
#endif

/* Returns the interval of POSITION in OBJECT.
   POSITION is BEG-based.  */

INTERVAL
interval_of (EMACS_INT position, Lisp_Object object)
{
  register INTERVAL i;
  EMACS_INT beg, end;

  if (NILP (object))
    XSETBUFFER (object, current_buffer);
  else if (EQ (object, Qt))
    return NULL_INTERVAL;

  CHECK_STRING_OR_BUFFER (object);

  if (BUFFERP (object))
    {
      register struct buffer *b = XBUFFER (object);

      beg = BUF_BEGV (b);
      end = BUF_ZV (b);
      i = BUF_INTERVALS (b);
    }
  else
    {
      beg = 0;
      end = SCHARS (object);
      i = STRING_INTERVALS (object);
    }

  if (!(beg <= position && position <= end))
    args_out_of_range (make_number (position), make_number (position));
  if (beg == end || NULL_INTERVAL_P (i))
    return NULL_INTERVAL;

  return find_interval (i, position);
}

DEFUN ("text-properties-at", Ftext_properties_at,
       Stext_properties_at, 1, 2, 0,
       doc: /* Return the list of properties of the character at POSITION in OBJECT.
If the optional second argument OBJECT is a buffer (or nil, which means
the current buffer), POSITION is a buffer position (integer or marker).
If OBJECT is a string, POSITION is a 0-based index into it.
If POSITION is at the end of OBJECT, the value is nil.  */)
  (Lisp_Object position, Lisp_Object object)
{
  register INTERVAL i;

  if (NILP (object))
    XSETBUFFER (object, current_buffer);

  i = validate_interval_range (object, &position, &position, soft);
  if (NULL_INTERVAL_P (i))
    return Qnil;
  /* If POSITION is at the end of the interval,
     it means it's the end of OBJECT.
     There are no properties at the very end,
     since no character follows.  */
  if (XINT (position) == LENGTH (i) + i->position)
    return Qnil;

  return i->plist;
}

DEFUN ("get-text-property", Fget_text_property, Sget_text_property, 2, 3, 0,
       doc: /* Return the value of POSITION's property PROP, in OBJECT.
OBJECT is optional and defaults to the current buffer.
If POSITION is at the end of OBJECT, the value is nil.  */)
  (Lisp_Object position, Lisp_Object prop, Lisp_Object object)
{
  return textget (Ftext_properties_at (position, object), prop);
}

/* Return the value of char's property PROP, in OBJECT at POSITION.
   OBJECT is optional and defaults to the current buffer.
   If OVERLAY is non-0, then in the case that the returned property is from
   an overlay, the overlay found is returned in *OVERLAY, otherwise nil is
   returned in *OVERLAY.
   If POSITION is at the end of OBJECT, the value is nil.
   If OBJECT is a buffer, then overlay properties are considered as well as
   text properties.
   If OBJECT is a window, then that window's buffer is used, but
   window-specific overlays are considered only if they are associated
   with OBJECT. */
Lisp_Object
get_char_property_and_overlay (Lisp_Object position, register Lisp_Object prop, Lisp_Object object, Lisp_Object *overlay)
{
  struct window *w = 0;

  CHECK_NUMBER_COERCE_MARKER (position);

  if (NILP (object))
    XSETBUFFER (object, current_buffer);

  if (WINDOWP (object))
    {
      w = XWINDOW (object);
      object = w->buffer;
    }
  if (BUFFERP (object))
    {
      ptrdiff_t noverlays;
      Lisp_Object *overlay_vec;
      struct buffer *obuf = current_buffer;

      if (XINT (position) < BUF_BEGV (XBUFFER (object))
	  || XINT (position) > BUF_ZV (XBUFFER (object)))
	xsignal1 (Qargs_out_of_range, position);

      set_buffer_temp (XBUFFER (object));

      GET_OVERLAYS_AT (XINT (position), overlay_vec, noverlays, NULL, 0);
      noverlays = sort_overlays (overlay_vec, noverlays, w);

      set_buffer_temp (obuf);

      /* Now check the overlays in order of decreasing priority.  */
      while (--noverlays >= 0)
	{
	  Lisp_Object tem = Foverlay_get (overlay_vec[noverlays], prop);
	  if (!NILP (tem))
	    {
	      if (overlay)
		/* Return the overlay we got the property from.  */
		*overlay = overlay_vec[noverlays];
	      return tem;
	    }
	}
    }

  if (overlay)
    /* Indicate that the return value is not from an overlay.  */
    *overlay = Qnil;

  /* Not a buffer, or no appropriate overlay, so fall through to the
     simpler case.  */
  return Fget_text_property (position, prop, object);
}

DEFUN ("get-char-property", Fget_char_property, Sget_char_property, 2, 3, 0,
       doc: /* Return the value of POSITION's property PROP, in OBJECT.
Both overlay properties and text properties are checked.
OBJECT is optional and defaults to the current buffer.
If POSITION is at the end of OBJECT, the value is nil.
If OBJECT is a buffer, then overlay properties are considered as well as
text properties.
If OBJECT is a window, then that window's buffer is used, but window-specific
overlays are considered only if they are associated with OBJECT.  */)
  (Lisp_Object position, Lisp_Object prop, Lisp_Object object)
{
  return get_char_property_and_overlay (position, prop, object, 0);
}

DEFUN ("get-char-property-and-overlay", Fget_char_property_and_overlay,
       Sget_char_property_and_overlay, 2, 3, 0,
       doc: /* Like `get-char-property', but with extra overlay information.
The value is a cons cell.  Its car is the return value of `get-char-property'
with the same arguments--that is, the value of POSITION's property
PROP in OBJECT.  Its cdr is the overlay in which the property was
found, or nil, if it was found as a text property or not found at all.

OBJECT is optional and defaults to the current buffer.  OBJECT may be
a string, a buffer or a window.  For strings, the cdr of the return
value is always nil, since strings do not have overlays.  If OBJECT is
a window, then that window's buffer is used, but window-specific
overlays are considered only if they are associated with OBJECT.  If
POSITION is at the end of OBJECT, both car and cdr are nil.  */)
  (Lisp_Object position, Lisp_Object prop, Lisp_Object object)
{
  Lisp_Object overlay;
  Lisp_Object val
    = get_char_property_and_overlay (position, prop, object, &overlay);
  return Fcons (val, overlay);
}


DEFUN ("next-char-property-change", Fnext_char_property_change,
       Snext_char_property_change, 1, 2, 0,
       doc: /* Return the position of next text property or overlay change.
This scans characters forward in the current buffer from POSITION till
it finds a change in some text property, or the beginning or end of an
overlay, and returns the position of that.
If none is found up to (point-max), the function returns (point-max).

If the optional second argument LIMIT is non-nil, don't search
past position LIMIT; return LIMIT if nothing is found before LIMIT.
LIMIT is a no-op if it is greater than (point-max).  */)
  (Lisp_Object position, Lisp_Object limit)
{
  Lisp_Object temp;

  temp = Fnext_overlay_change (position);
  if (! NILP (limit))
    {
      CHECK_NUMBER_COERCE_MARKER (limit);
      if (XINT (limit) < XINT (temp))
	temp = limit;
    }
  return Fnext_property_change (position, Qnil, temp);
}

DEFUN ("previous-char-property-change", Fprevious_char_property_change,
       Sprevious_char_property_change, 1, 2, 0,
       doc: /* Return the position of previous text property or overlay change.
Scans characters backward in the current buffer from POSITION till it
finds a change in some text property, or the beginning or end of an
overlay, and returns the position of that.
If none is found since (point-min), the function returns (point-min).

If the optional second argument LIMIT is non-nil, don't search
past position LIMIT; return LIMIT if nothing is found before LIMIT.
LIMIT is a no-op if it is less than (point-min).  */)
  (Lisp_Object position, Lisp_Object limit)
{
  Lisp_Object temp;

  temp = Fprevious_overlay_change (position);
  if (! NILP (limit))
    {
      CHECK_NUMBER_COERCE_MARKER (limit);
      if (XINT (limit) > XINT (temp))
	temp = limit;
    }
  return Fprevious_property_change (position, Qnil, temp);
}


DEFUN ("next-single-char-property-change", Fnext_single_char_property_change,
       Snext_single_char_property_change, 2, 4, 0,
       doc: /* Return the position of next text property or overlay change for a specific property.
Scans characters forward from POSITION till it finds
a change in the PROP property, then returns the position of the change.
If the optional third argument OBJECT is a buffer (or nil, which means
the current buffer), POSITION is a buffer position (integer or marker).
If OBJECT is a string, POSITION is a 0-based index into it.

In a string, scan runs to the end of the string.
In a buffer, it runs to (point-max), and the value cannot exceed that.

The property values are compared with `eq'.
If the property is constant all the way to the end of OBJECT, return the
last valid position in OBJECT.
If the optional fourth argument LIMIT is non-nil, don't search
past position LIMIT; return LIMIT if nothing is found before LIMIT.  */)
  (Lisp_Object position, Lisp_Object prop, Lisp_Object object, Lisp_Object limit)
{
  if (STRINGP (object))
    {
      position = Fnext_single_property_change (position, prop, object, limit);
      if (NILP (position))
	{
	  if (NILP (limit))
	    position = make_number (SCHARS (object));
	  else
	    {
	      CHECK_NUMBER (limit);
	      position = limit;
	    }
	}
    }
  else
    {
      Lisp_Object initial_value, value;
      int count = SPECPDL_INDEX ();

      if (! NILP (object))
	CHECK_BUFFER (object);

      if (BUFFERP (object) && current_buffer != XBUFFER (object))
	{
	  record_unwind_protect (Fset_buffer, Fcurrent_buffer ());
	  Fset_buffer (object);
	}

      CHECK_NUMBER_COERCE_MARKER (position);

      initial_value = Fget_char_property (position, prop, object);

      if (NILP (limit))
	XSETFASTINT (limit, ZV);
      else
	CHECK_NUMBER_COERCE_MARKER (limit);

      if (XFASTINT (position) >= XFASTINT (limit))
	{
	  position = limit;
	  if (XFASTINT (position) > ZV)
	    XSETFASTINT (position, ZV);
	}
      else
	while (1)
	  {
	    position = Fnext_char_property_change (position, limit);
	    if (XFASTINT (position) >= XFASTINT (limit))
	      {
		position = limit;
		break;
	      }

	    value = Fget_char_property (position, prop, object);
	    if (!EQ (value, initial_value))
	      break;
	  }

      unbind_to (count, Qnil);
    }

  return position;
}

DEFUN ("previous-single-char-property-change",
       Fprevious_single_char_property_change,
       Sprevious_single_char_property_change, 2, 4, 0,
       doc: /* Return the position of previous text property or overlay change for a specific property.
Scans characters backward from POSITION till it finds
a change in the PROP property, then returns the position of the change.
If the optional third argument OBJECT is a buffer (or nil, which means
the current buffer), POSITION is a buffer position (integer or marker).
If OBJECT is a string, POSITION is a 0-based index into it.

In a string, scan runs to the start of the string.
In a buffer, it runs to (point-min), and the value cannot be less than that.

The property values are compared with `eq'.
If the property is constant all the way to the start of OBJECT, return the
first valid position in OBJECT.
If the optional fourth argument LIMIT is non-nil, don't search back past
position LIMIT; return LIMIT if nothing is found before reaching LIMIT.  */)
  (Lisp_Object position, Lisp_Object prop, Lisp_Object object, Lisp_Object limit)
{
  if (STRINGP (object))
    {
      position = Fprevious_single_property_change (position, prop, object, limit);
      if (NILP (position))
	{
	  if (NILP (limit))
	    position = make_number (0);
	  else
	    {
	      CHECK_NUMBER (limit);
	      position = limit;
	    }
	}
    }
  else
    {
      int count = SPECPDL_INDEX ();

      if (! NILP (object))
	CHECK_BUFFER (object);

      if (BUFFERP (object) && current_buffer != XBUFFER (object))
	{
	  record_unwind_protect (Fset_buffer, Fcurrent_buffer ());
	  Fset_buffer (object);
	}

      CHECK_NUMBER_COERCE_MARKER (position);

      if (NILP (limit))
	XSETFASTINT (limit, BEGV);
      else
	CHECK_NUMBER_COERCE_MARKER (limit);

      if (XFASTINT (position) <= XFASTINT (limit))
	{
	  position = limit;
	  if (XFASTINT (position) < BEGV)
	    XSETFASTINT (position, BEGV);
	}
      else
	{
	  Lisp_Object initial_value
	    = Fget_char_property (make_number (XFASTINT (position) - 1),
				  prop, object);

	  while (1)
	    {
	      position = Fprevious_char_property_change (position, limit);

	      if (XFASTINT (position) <= XFASTINT (limit))
		{
		  position = limit;
		  break;
		}
	      else
		{
		  Lisp_Object value
		    = Fget_char_property (make_number (XFASTINT (position) - 1),
					  prop, object);

		  if (!EQ (value, initial_value))
		    break;
		}
	    }
	}

      unbind_to (count, Qnil);
    }

  return position;
}

DEFUN ("next-property-change", Fnext_property_change,
       Snext_property_change, 1, 3, 0,
       doc: /* Return the position of next property change.
Scans characters forward from POSITION in OBJECT till it finds
a change in some text property, then returns the position of the change.
If the optional second argument OBJECT is a buffer (or nil, which means
the current buffer), POSITION is a buffer position (integer or marker).
If OBJECT is a string, POSITION is a 0-based index into it.
Return nil if the property is constant all the way to the end of OBJECT.
If the value is non-nil, it is a position greater than POSITION, never equal.

If the optional third argument LIMIT is non-nil, don't search
past position LIMIT; return LIMIT if nothing is found before LIMIT.  */)
  (Lisp_Object position, Lisp_Object object, Lisp_Object limit)
{
  register INTERVAL i, next;

  if (NILP (object))
    XSETBUFFER (object, current_buffer);

  if (!NILP (limit) && !EQ (limit, Qt))
    CHECK_NUMBER_COERCE_MARKER (limit);

  i = validate_interval_range (object, &position, &position, soft);

  /* If LIMIT is t, return start of next interval--don't
     bother checking further intervals.  */
  if (EQ (limit, Qt))
    {
      if (NULL_INTERVAL_P (i))
	next = i;
      else
	next = next_interval (i);

      if (NULL_INTERVAL_P (next))
	XSETFASTINT (position, (STRINGP (object)
				? SCHARS (object)
				: BUF_ZV (XBUFFER (object))));
      else
	XSETFASTINT (position, next->position);
      return position;
    }

  if (NULL_INTERVAL_P (i))
    return limit;

  next = next_interval (i);

  while (!NULL_INTERVAL_P (next) && intervals_equal (i, next)
	 && (NILP (limit) || next->position < XFASTINT (limit)))
    next = next_interval (next);

  if (NULL_INTERVAL_P (next)
      || (next->position
	  >= (INTEGERP (limit)
	      ? XFASTINT (limit)
	      : (STRINGP (object)
		 ? SCHARS (object)
		 : BUF_ZV (XBUFFER (object))))))
    return limit;
  else
    return make_number (next->position);
}

DEFUN ("next-single-property-change", Fnext_single_property_change,
       Snext_single_property_change, 2, 4, 0,
       doc: /* Return the position of next property change for a specific property.
Scans characters forward from POSITION till it finds
a change in the PROP property, then returns the position of the change.
If the optional third argument OBJECT is a buffer (or nil, which means
the current buffer), POSITION is a buffer position (integer or marker).
If OBJECT is a string, POSITION is a 0-based index into it.
The property values are compared with `eq'.
Return nil if the property is constant all the way to the end of OBJECT.
If the value is non-nil, it is a position greater than POSITION, never equal.

If the optional fourth argument LIMIT is non-nil, don't search
past position LIMIT; return LIMIT if nothing is found before LIMIT.  */)
  (Lisp_Object position, Lisp_Object prop, Lisp_Object object, Lisp_Object limit)
{
  register INTERVAL i, next;
  register Lisp_Object here_val;

  if (NILP (object))
    XSETBUFFER (object, current_buffer);

  if (!NILP (limit))
    CHECK_NUMBER_COERCE_MARKER (limit);

  i = validate_interval_range (object, &position, &position, soft);
  if (NULL_INTERVAL_P (i))
    return limit;

  here_val = textget (i->plist, prop);
  next = next_interval (i);
  while (! NULL_INTERVAL_P (next)
	 && EQ (here_val, textget (next->plist, prop))
	 && (NILP (limit) || next->position < XFASTINT (limit)))
    next = next_interval (next);

  if (NULL_INTERVAL_P (next)
      || (next->position
	  >= (INTEGERP (limit)
	      ? XFASTINT (limit)
	      : (STRINGP (object)
		 ? SCHARS (object)
		 : BUF_ZV (XBUFFER (object))))))
    return limit;
  else
    return make_number (next->position);
}

DEFUN ("previous-property-change", Fprevious_property_change,
       Sprevious_property_change, 1, 3, 0,
       doc: /* Return the position of previous property change.
Scans characters backwards from POSITION in OBJECT till it finds
a change in some text property, then returns the position of the change.
If the optional second argument OBJECT is a buffer (or nil, which means
the current buffer), POSITION is a buffer position (integer or marker).
If OBJECT is a string, POSITION is a 0-based index into it.
Return nil if the property is constant all the way to the start of OBJECT.
If the value is non-nil, it is a position less than POSITION, never equal.

If the optional third argument LIMIT is non-nil, don't search
back past position LIMIT; return LIMIT if nothing is found until LIMIT.  */)
  (Lisp_Object position, Lisp_Object object, Lisp_Object limit)
{
  register INTERVAL i, previous;

  if (NILP (object))
    XSETBUFFER (object, current_buffer);

  if (!NILP (limit))
    CHECK_NUMBER_COERCE_MARKER (limit);

  i = validate_interval_range (object, &position, &position, soft);
  if (NULL_INTERVAL_P (i))
    return limit;

  /* Start with the interval containing the char before point.  */
  if (i->position == XFASTINT (position))
    i = previous_interval (i);

  previous = previous_interval (i);
  while (!NULL_INTERVAL_P (previous) && intervals_equal (previous, i)
	 && (NILP (limit)
	     || (previous->position + LENGTH (previous) > XFASTINT (limit))))
    previous = previous_interval (previous);

  if (NULL_INTERVAL_P (previous)
      || (previous->position + LENGTH (previous)
	  <= (INTEGERP (limit)
	      ? XFASTINT (limit)
	      : (STRINGP (object) ? 0 : BUF_BEGV (XBUFFER (object))))))
    return limit;
  else
    return make_number (previous->position + LENGTH (previous));
}

DEFUN ("previous-single-property-change", Fprevious_single_property_change,
       Sprevious_single_property_change, 2, 4, 0,
       doc: /* Return the position of previous property change for a specific property.
Scans characters backward from POSITION till it finds
a change in the PROP property, then returns the position of the change.
If the optional third argument OBJECT is a buffer (or nil, which means
the current buffer), POSITION is a buffer position (integer or marker).
If OBJECT is a string, POSITION is a 0-based index into it.
The property values are compared with `eq'.
Return nil if the property is constant all the way to the start of OBJECT.
If the value is non-nil, it is a position less than POSITION, never equal.

If the optional fourth argument LIMIT is non-nil, don't search
back past position LIMIT; return LIMIT if nothing is found until LIMIT.  */)
  (Lisp_Object position, Lisp_Object prop, Lisp_Object object, Lisp_Object limit)
{
  register INTERVAL i, previous;
  register Lisp_Object here_val;

  if (NILP (object))
    XSETBUFFER (object, current_buffer);

  if (!NILP (limit))
    CHECK_NUMBER_COERCE_MARKER (limit);

  i = validate_interval_range (object, &position, &position, soft);

  /* Start with the interval containing the char before point.  */
  if (!NULL_INTERVAL_P (i) && i->position == XFASTINT (position))
    i = previous_interval (i);

  if (NULL_INTERVAL_P (i))
    return limit;

  here_val = textget (i->plist, prop);
  previous = previous_interval (i);
  while (!NULL_INTERVAL_P (previous)
	 && EQ (here_val, textget (previous->plist, prop))
	 && (NILP (limit)
	     || (previous->position + LENGTH (previous) > XFASTINT (limit))))
    previous = previous_interval (previous);

  if (NULL_INTERVAL_P (previous)
      || (previous->position + LENGTH (previous)
	  <= (INTEGERP (limit)
	      ? XFASTINT (limit)
	      : (STRINGP (object) ? 0 : BUF_BEGV (XBUFFER (object))))))
    return limit;
  else
    return make_number (previous->position + LENGTH (previous));
}

/* Callers note, this can GC when OBJECT is a buffer (or nil).  */

DEFUN ("add-text-properties", Fadd_text_properties,
       Sadd_text_properties, 3, 4, 0,
       doc: /* Add properties to the text from START to END.
The third argument PROPERTIES is a property list
specifying the property values to add.  If the optional fourth argument
OBJECT is a buffer (or nil, which means the current buffer),
START and END are buffer positions (integers or markers).
If OBJECT is a string, START and END are 0-based indices into it.
Return t if any property value actually changed, nil otherwise.  */)
  (Lisp_Object start, Lisp_Object end, Lisp_Object properties, Lisp_Object object)
{
  register INTERVAL i, unchanged;
  register EMACS_INT s, len;
  register int modified = 0;
  struct gcpro gcpro1;

  properties = validate_plist (properties);
  if (NILP (properties))
    return Qnil;

  if (NILP (object))
    XSETBUFFER (object, current_buffer);

  i = validate_interval_range (object, &start, &end, hard);
  if (NULL_INTERVAL_P (i))
    return Qnil;

  s = XINT (start);
  len = XINT (end) - s;

  /* No need to protect OBJECT, because we GC only if it's a buffer,
     and live buffers are always protected.  */
  GCPRO1 (properties);

  /* If we're not starting on an interval boundary, we have to
    split this interval.  */
  if (i->position != s)
    {
      /* If this interval already has the properties, we can
         skip it.  */
      if (interval_has_all_properties (properties, i))
	{
	  EMACS_INT got = (LENGTH (i) - (s - i->position));
	  if (got >= len)
	    RETURN_UNGCPRO (Qnil);
	  len -= got;
	  i = next_interval (i);
	}
      else
	{
	  unchanged = i;
	  i = split_interval_right (unchanged, s - unchanged->position);
	  copy_properties (unchanged, i);
	}
    }

  if (BUFFERP (object))
    modify_region (XBUFFER (object), XINT (start), XINT (end), 1);

  /* We are at the beginning of interval I, with LEN chars to scan.  */
  for (;;)
    {
      if (i == 0)
	abort ();

      if (LENGTH (i) >= len)
	{
	  /* We can UNGCPRO safely here, because there will be just
	     one more chance to gc, in the next call to add_properties,
	     and after that we will not need PROPERTIES or OBJECT again.  */
	  UNGCPRO;

	  if (interval_has_all_properties (properties, i))
	    {
	      if (BUFFERP (object))
		signal_after_change (XINT (start), XINT (end) - XINT (start),
				     XINT (end) - XINT (start));

	      return modified ? Qt : Qnil;
	    }

	  if (LENGTH (i) == len)
	    {
	      add_properties (properties, i, object);
	      if (BUFFERP (object))
		signal_after_change (XINT (start), XINT (end) - XINT (start),
				     XINT (end) - XINT (start));
	      return Qt;
	    }

	  /* i doesn't have the properties, and goes past the change limit */
	  unchanged = i;
	  i = split_interval_left (unchanged, len);
	  copy_properties (unchanged, i);
	  add_properties (properties, i, object);
	  if (BUFFERP (object))
	    signal_after_change (XINT (start), XINT (end) - XINT (start),
				 XINT (end) - XINT (start));
	  return Qt;
	}

      len -= LENGTH (i);
      modified += add_properties (properties, i, object);
      i = next_interval (i);
    }
}

/* Callers note, this can GC when OBJECT is a buffer (or nil).  */

DEFUN ("put-text-property", Fput_text_property,
       Sput_text_property, 4, 5, 0,
       doc: /* Set one property of the text from START to END.
The third and fourth arguments PROPERTY and VALUE
specify the property to add.
If the optional fifth argument OBJECT is a buffer (or nil, which means
the current buffer), START and END are buffer positions (integers or
markers).  If OBJECT is a string, START and END are 0-based indices into it.  */)
  (Lisp_Object start, Lisp_Object end, Lisp_Object property, Lisp_Object value, Lisp_Object object)
{
  Fadd_text_properties (start, end,
			Fcons (property, Fcons (value, Qnil)),
			object);
  return Qnil;
}

DEFUN ("set-text-properties", Fset_text_properties,
       Sset_text_properties, 3, 4, 0,
       doc: /* Completely replace properties of text from START to END.
The third argument PROPERTIES is the new property list.
If the optional fourth argument OBJECT is a buffer (or nil, which means
the current buffer), START and END are buffer positions (integers or
markers).  If OBJECT is a string, START and END are 0-based indices into it.
If PROPERTIES is nil, the effect is to remove all properties from
the designated part of OBJECT.  */)
  (Lisp_Object start, Lisp_Object end, Lisp_Object properties, Lisp_Object object)
{
  return set_text_properties (start, end, properties, object, Qt);
}


/* Replace properties of text from START to END with new list of
   properties PROPERTIES.  OBJECT is the buffer or string containing
   the text.  OBJECT nil means use the current buffer.
   COHERENT_CHANGE_P nil means this is being called as an internal
   subroutine, rather than as a change primitive with checking of
   read-only, invoking change hooks, etc..  Value is nil if the
   function _detected_ that it did not replace any properties, non-nil
   otherwise.  */

Lisp_Object
set_text_properties (Lisp_Object start, Lisp_Object end, Lisp_Object properties, Lisp_Object object, Lisp_Object coherent_change_p)
{
  register INTERVAL i;
  Lisp_Object ostart, oend;

  ostart = start;
  oend = end;

  properties = validate_plist (properties);

  if (NILP (object))
    XSETBUFFER (object, current_buffer);

  /* If we want no properties for a whole string,
     get rid of its intervals.  */
  if (NILP (properties) && STRINGP (object)
      && XFASTINT (start) == 0
      && XFASTINT (end) == SCHARS (object))
    {
      if (! STRING_INTERVALS (object))
	return Qnil;

      STRING_SET_INTERVALS (object, NULL_INTERVAL);
      return Qt;
    }

  i = validate_interval_range (object, &start, &end, soft);

  if (NULL_INTERVAL_P (i))
    {
      /* If buffer has no properties, and we want none, return now.  */
      if (NILP (properties))
	return Qnil;

      /* Restore the original START and END values
	 because validate_interval_range increments them for strings.  */
      start = ostart;
      end = oend;

      i = validate_interval_range (object, &start, &end, hard);
      /* This can return if start == end.  */
      if (NULL_INTERVAL_P (i))
	return Qnil;
    }

  if (BUFFERP (object) && !NILP (coherent_change_p))
    modify_region (XBUFFER (object), XINT (start), XINT (end), 1);

  set_text_properties_1 (start, end, properties, object, i);

  if (BUFFERP (object) && !NILP (coherent_change_p))
    signal_after_change (XINT (start), XINT (end) - XINT (start),
			 XINT (end) - XINT (start));
  return Qt;
}

/* Replace properties of text from START to END with new list of
   properties PROPERTIES.  BUFFER is the buffer containing
   the text.  This does not obey any hooks.
   You can provide the interval that START is located in as I,
   or pass NULL for I and this function will find it.
   START and END can be in any order.  */

void
set_text_properties_1 (Lisp_Object start, Lisp_Object end, Lisp_Object properties, Lisp_Object buffer, INTERVAL i)
{
  register INTERVAL prev_changed = NULL_INTERVAL;
  register EMACS_INT s, len;
  INTERVAL unchanged;

  if (XINT (start) < XINT (end))
    {
      s = XINT (start);
      len = XINT (end) - s;
    }
  else if (XINT (end) < XINT (start))
    {
      s = XINT (end);
      len = XINT (start) - s;
    }
  else
    return;

  if (i == 0)
    i = find_interval (BUF_INTERVALS (XBUFFER (buffer)), s);

  if (i->position != s)
    {
      unchanged = i;
      i = split_interval_right (unchanged, s - unchanged->position);

      if (LENGTH (i) > len)
	{
	  copy_properties (unchanged, i);
	  i = split_interval_left (i, len);
	  set_properties (properties, i, buffer);
	  return;
	}

      set_properties (properties, i, buffer);

      if (LENGTH (i) == len)
	return;

      prev_changed = i;
      len -= LENGTH (i);
      i = next_interval (i);
    }

  /* We are starting at the beginning of an interval I.  LEN is positive.  */
  do
    {
      if (i == 0)
	abort ();

      if (LENGTH (i) >= len)
	{
	  if (LENGTH (i) > len)
	    i = split_interval_left (i, len);

	  /* We have to call set_properties even if we are going to
	     merge the intervals, so as to make the undo records
	     and cause redisplay to happen.  */
	  set_properties (properties, i, buffer);
	  if (!NULL_INTERVAL_P (prev_changed))
	    merge_interval_left (i);
	  return;
	}

      len -= LENGTH (i);

      /* We have to call set_properties even if we are going to
	 merge the intervals, so as to make the undo records
	 and cause redisplay to happen.  */
      set_properties (properties, i, buffer);
      if (NULL_INTERVAL_P (prev_changed))
	prev_changed = i;
      else
	prev_changed = i = merge_interval_left (i);

      i = next_interval (i);
    }
  while (len > 0);
}

DEFUN ("remove-text-properties", Fremove_text_properties,
       Sremove_text_properties, 3, 4, 0,
       doc: /* Remove some properties from text from START to END.
The third argument PROPERTIES is a property list
whose property names specify the properties to remove.
\(The values stored in PROPERTIES are ignored.)
If the optional fourth argument OBJECT is a buffer (or nil, which means
the current buffer), START and END are buffer positions (integers or
markers).  If OBJECT is a string, START and END are 0-based indices into it.
Return t if any property was actually removed, nil otherwise.

Use `set-text-properties' if you want to remove all text properties.  */)
  (Lisp_Object start, Lisp_Object end, Lisp_Object properties, Lisp_Object object)
{
  register INTERVAL i, unchanged;
  register EMACS_INT s, len;
  register int modified = 0;

  if (NILP (object))
    XSETBUFFER (object, current_buffer);

  i = validate_interval_range (object, &start, &end, soft);
  if (NULL_INTERVAL_P (i))
    return Qnil;

  s = XINT (start);
  len = XINT (end) - s;

  if (i->position != s)
    {
      /* No properties on this first interval -- return if
         it covers the entire region.  */
      if (! interval_has_some_properties (properties, i))
	{
	  EMACS_INT got = (LENGTH (i) - (s - i->position));
	  if (got >= len)
	    return Qnil;
	  len -= got;
	  i = next_interval (i);
	}
      /* Split away the beginning of this interval; what we don't
	 want to modify.  */
      else
	{
	  unchanged = i;
	  i = split_interval_right (unchanged, s - unchanged->position);
	  copy_properties (unchanged, i);
	}
    }

  if (BUFFERP (object))
    modify_region (XBUFFER (object), XINT (start), XINT (end), 1);

  /* We are at the beginning of an interval, with len to scan */
  for (;;)
    {
      if (i == 0)
	abort ();

      if (LENGTH (i) >= len)
	{
	  if (! interval_has_some_properties (properties, i))
	    return modified ? Qt : Qnil;

	  if (LENGTH (i) == len)
	    {
	      remove_properties (properties, Qnil, i, object);
	      if (BUFFERP (object))
		signal_after_change (XINT (start), XINT (end) - XINT (start),
				     XINT (end) - XINT (start));
	      return Qt;
	    }

	  /* i has the properties, and goes past the change limit */
	  unchanged = i;
	  i = split_interval_left (i, len);
	  copy_properties (unchanged, i);
	  remove_properties (properties, Qnil, i, object);
	  if (BUFFERP (object))
	    signal_after_change (XINT (start), XINT (end) - XINT (start),
				 XINT (end) - XINT (start));
	  return Qt;
	}

      len -= LENGTH (i);
      modified += remove_properties (properties, Qnil, i, object);
      i = next_interval (i);
    }
}

DEFUN ("remove-list-of-text-properties", Fremove_list_of_text_properties,
       Sremove_list_of_text_properties, 3, 4, 0,
       doc: /* Remove some properties from text from START to END.
The third argument LIST-OF-PROPERTIES is a list of property names to remove.
If the optional fourth argument OBJECT is a buffer (or nil, which means
the current buffer), START and END are buffer positions (integers or
markers).  If OBJECT is a string, START and END are 0-based indices into it.
Return t if any property was actually removed, nil otherwise.  */)
  (Lisp_Object start, Lisp_Object end, Lisp_Object list_of_properties, Lisp_Object object)
{
  register INTERVAL i, unchanged;
  register EMACS_INT s, len;
  register int modified = 0;
  Lisp_Object properties;
  properties = list_of_properties;

  if (NILP (object))
    XSETBUFFER (object, current_buffer);

  i = validate_interval_range (object, &start, &end, soft);
  if (NULL_INTERVAL_P (i))
    return Qnil;

  s = XINT (start);
  len = XINT (end) - s;

  if (i->position != s)
    {
      /* No properties on this first interval -- return if
         it covers the entire region.  */
      if (! interval_has_some_properties_list (properties, i))
	{
	  EMACS_INT got = (LENGTH (i) - (s - i->position));
	  if (got >= len)
	    return Qnil;
	  len -= got;
	  i = next_interval (i);
	}
      /* Split away the beginning of this interval; what we don't
	 want to modify.  */
      else
	{
	  unchanged = i;
	  i = split_interval_right (unchanged, s - unchanged->position);
	  copy_properties (unchanged, i);
	}
    }

  /* We are at the beginning of an interval, with len to scan.
     The flag `modified' records if changes have been made.
     When object is a buffer, we must call modify_region before changes are
     made and signal_after_change when we are done.
     We call modify_region before calling remove_properties if modified == 0,
     and we call signal_after_change before returning if modified != 0. */
  for (;;)
    {
      if (i == 0)
	abort ();

      if (LENGTH (i) >= len)
	{
	  if (! interval_has_some_properties_list (properties, i))
	    {
	      if (modified)
		{
		  if (BUFFERP (object))
		    signal_after_change (XINT (start),
					 XINT (end) - XINT (start),
					 XINT (end) - XINT (start));
		  return Qt;
		}
	      else
		return Qnil;
	    }
	  else if (LENGTH (i) == len)
	    {
	      if (!modified && BUFFERP (object))
		modify_region (XBUFFER (object), XINT (start), XINT (end), 1);
	      remove_properties (Qnil, properties, i, object);
	      if (BUFFERP (object))
		signal_after_change (XINT (start), XINT (end) - XINT (start),
				     XINT (end) - XINT (start));
	      return Qt;
	    }
	  else
	    { /* i has the properties, and goes past the change limit.  */
	      unchanged = i;
	      i = split_interval_left (i, len);
	      copy_properties (unchanged, i);
	      if (!modified && BUFFERP (object))
		modify_region (XBUFFER (object), XINT (start), XINT (end), 1);
	      remove_properties (Qnil, properties, i, object);
	      if (BUFFERP (object))
		signal_after_change (XINT (start), XINT (end) - XINT (start),
				     XINT (end) - XINT (start));
	      return Qt;
	    }
	}
      if (interval_has_some_properties_list (properties, i))
	{
	  if (!modified && BUFFERP (object))
	    modify_region (XBUFFER (object), XINT (start), XINT (end), 1);
	  remove_properties (Qnil, properties, i, object);
	  modified = 1;
	}
      len -= LENGTH (i);
      i = next_interval (i);
    }
}

DEFUN ("text-property-any", Ftext_property_any,
       Stext_property_any, 4, 5, 0,
       doc: /* Check text from START to END for property PROPERTY equaling VALUE.
If so, return the position of the first character whose property PROPERTY
is `eq' to VALUE.  Otherwise return nil.
If the optional fifth argument OBJECT is a buffer (or nil, which means
the current buffer), START and END are buffer positions (integers or
markers).  If OBJECT is a string, START and END are 0-based indices into it.  */)
  (Lisp_Object start, Lisp_Object end, Lisp_Object property, Lisp_Object value, Lisp_Object object)
{
  register INTERVAL i;
  register EMACS_INT e, pos;

  if (NILP (object))
    XSETBUFFER (object, current_buffer);
  i = validate_interval_range (object, &start, &end, soft);
  if (NULL_INTERVAL_P (i))
    return (!NILP (value) || EQ (start, end) ? Qnil : start);
  e = XINT (end);

  while (! NULL_INTERVAL_P (i))
    {
      if (i->position >= e)
	break;
      if (EQ (textget (i->plist, property), value))
	{
	  pos = i->position;
	  if (pos < XINT (start))
	    pos = XINT (start);
	  return make_number (pos);
	}
      i = next_interval (i);
    }
  return Qnil;
}

DEFUN ("text-property-not-all", Ftext_property_not_all,
       Stext_property_not_all, 4, 5, 0,
       doc: /* Check text from START to END for property PROPERTY not equaling VALUE.
If so, return the position of the first character whose property PROPERTY
is not `eq' to VALUE.  Otherwise, return nil.
If the optional fifth argument OBJECT is a buffer (or nil, which means
the current buffer), START and END are buffer positions (integers or
markers).  If OBJECT is a string, START and END are 0-based indices into it.  */)
  (Lisp_Object start, Lisp_Object end, Lisp_Object property, Lisp_Object value, Lisp_Object object)
{
  register INTERVAL i;
  register EMACS_INT s, e;

  if (NILP (object))
    XSETBUFFER (object, current_buffer);
  i = validate_interval_range (object, &start, &end, soft);
  if (NULL_INTERVAL_P (i))
    return (NILP (value) || EQ (start, end)) ? Qnil : start;
  s = XINT (start);
  e = XINT (end);

  while (! NULL_INTERVAL_P (i))
    {
      if (i->position >= e)
	break;
      if (! EQ (textget (i->plist, property), value))
	{
	  if (i->position > s)
	    s = i->position;
	  return make_number (s);
	}
      i = next_interval (i);
    }
  return Qnil;
}


/* Return the direction from which the text-property PROP would be
   inherited by any new text inserted at POS: 1 if it would be
   inherited from the char after POS, -1 if it would be inherited from
   the char before POS, and 0 if from neither.
   BUFFER can be either a buffer or nil (meaning current buffer).  */

int
text_property_stickiness (Lisp_Object prop, Lisp_Object pos, Lisp_Object buffer)
{
  Lisp_Object prev_pos, front_sticky;
  int is_rear_sticky = 1, is_front_sticky = 0; /* defaults */
  Lisp_Object defalt = Fassq (prop, Vtext_property_default_nonsticky);

  if (NILP (buffer))
    XSETBUFFER (buffer, current_buffer);

  if (CONSP (defalt) && !NILP (XCDR (defalt)))
    is_rear_sticky = 0;

  if (XINT (pos) > BUF_BEGV (XBUFFER (buffer)))
    /* Consider previous character.  */
    {
      Lisp_Object rear_non_sticky;

      prev_pos = make_number (XINT (pos) - 1);
      rear_non_sticky = Fget_text_property (prev_pos, Qrear_nonsticky, buffer);

      if (!NILP (CONSP (rear_non_sticky)
		 ? Fmemq (prop, rear_non_sticky)
		 : rear_non_sticky))
	/* PROP is rear-non-sticky.  */
	is_rear_sticky = 0;
    }
  else
    return 0;

  /* Consider following character.  */
  /* This signals an arg-out-of-range error if pos is outside the
     buffer's accessible range.  */
  front_sticky = Fget_text_property (pos, Qfront_sticky, buffer);

  if (EQ (front_sticky, Qt)
      || (CONSP (front_sticky)
	  && !NILP (Fmemq (prop, front_sticky))))
    /* PROP is inherited from after.  */
    is_front_sticky = 1;

  /* Simple cases, where the properties are consistent.  */
  if (is_rear_sticky && !is_front_sticky)
    return -1;
  else if (!is_rear_sticky && is_front_sticky)
    return 1;
  else if (!is_rear_sticky && !is_front_sticky)
    return 0;

  /* The stickiness properties are inconsistent, so we have to
     disambiguate.  Basically, rear-sticky wins, _except_ if the
     property that would be inherited has a value of nil, in which case
     front-sticky wins.  */
  if (XINT (pos) == BUF_BEGV (XBUFFER (buffer))
      || NILP (Fget_text_property (prev_pos, prop, buffer)))
    return 1;
  else
    return -1;
}


/* Copying properties between objects. */

/* Add properties from START to END of SRC, starting at POS in DEST.
   SRC and DEST may each refer to strings or buffers.
   Optional sixth argument PROP causes only that property to be copied.
   Properties are copied to DEST as if by `add-text-properties'.
   Return t if any property value actually changed, nil otherwise.  */

/* Note this can GC when DEST is a buffer.  */

Lisp_Object
copy_text_properties (Lisp_Object start, Lisp_Object end, Lisp_Object src, Lisp_Object pos, Lisp_Object dest, Lisp_Object prop)
{
  INTERVAL i;
  Lisp_Object res;
  Lisp_Object stuff;
  Lisp_Object plist;
  EMACS_INT s, e, e2, p, len;
  int modified = 0;
  struct gcpro gcpro1, gcpro2;

  i = validate_interval_range (src, &start, &end, soft);
  if (NULL_INTERVAL_P (i))
    return Qnil;

  CHECK_NUMBER_COERCE_MARKER (pos);
  {
    Lisp_Object dest_start, dest_end;

    dest_start = pos;
    XSETFASTINT (dest_end, XINT (dest_start) + (XINT (end) - XINT (start)));
    /* Apply this to a copy of pos; it will try to increment its arguments,
       which we don't want.  */
    validate_interval_range (dest, &dest_start, &dest_end, soft);
  }

  s = XINT (start);
  e = XINT (end);
  p = XINT (pos);

  stuff = Qnil;

  while (s < e)
    {
      e2 = i->position + LENGTH (i);
      if (e2 > e)
	e2 = e;
      len = e2 - s;

      plist = i->plist;
      if (! NILP (prop))
	while (! NILP (plist))
	  {
	    if (EQ (Fcar (plist), prop))
	      {
		plist = Fcons (prop, Fcons (Fcar (Fcdr (plist)), Qnil));
		break;
	      }
	    plist = Fcdr (Fcdr (plist));
	  }
      if (! NILP (plist))
	{
	  /* Must defer modifications to the interval tree in case src
	     and dest refer to the same string or buffer.  */
	  stuff = Fcons (Fcons (make_number (p),
				Fcons (make_number (p + len),
				       Fcons (plist, Qnil))),
			stuff);
	}

      i = next_interval (i);
      if (NULL_INTERVAL_P (i))
	break;

      p += len;
      s = i->position;
    }

  GCPRO2 (stuff, dest);

  while (! NILP (stuff))
    {
      res = Fcar (stuff);
      res = Fadd_text_properties (Fcar (res), Fcar (Fcdr (res)),
				  Fcar (Fcdr (Fcdr (res))), dest);
      if (! NILP (res))
	modified++;
      stuff = Fcdr (stuff);
    }

  UNGCPRO;

  return modified ? Qt : Qnil;
}


/* Return a list representing the text properties of OBJECT between
   START and END.  if PROP is non-nil, report only on that property.
   Each result list element has the form (S E PLIST), where S and E
   are positions in OBJECT and PLIST is a property list containing the
   text properties of OBJECT between S and E.  Value is nil if OBJECT
   doesn't contain text properties between START and END.  */

Lisp_Object
text_property_list (Lisp_Object object, Lisp_Object start, Lisp_Object end, Lisp_Object prop)
{
  struct interval *i;
  Lisp_Object result;

  result = Qnil;

  i = validate_interval_range (object, &start, &end, soft);
  if (!NULL_INTERVAL_P (i))
    {
      EMACS_INT s = XINT (start);
      EMACS_INT e = XINT (end);

      while (s < e)
	{
	  EMACS_INT interval_end, len;
	  Lisp_Object plist;

	  interval_end = i->position + LENGTH (i);
	  if (interval_end > e)
	    interval_end = e;
	  len = interval_end - s;

	  plist = i->plist;

	  if (!NILP (prop))
	    for (; CONSP (plist); plist = Fcdr (XCDR (plist)))
	      if (EQ (XCAR (plist), prop))
		{
		  plist = Fcons (prop, Fcons (Fcar (XCDR (plist)), Qnil));
		  break;
		}

	  if (!NILP (plist))
	    result = Fcons (Fcons (make_number (s),
				   Fcons (make_number (s + len),
					  Fcons (plist, Qnil))),
			    result);

	  i = next_interval (i);
	  if (NULL_INTERVAL_P (i))
	    break;
	  s = i->position;
	}
    }

  return result;
}


/* Add text properties to OBJECT from LIST.  LIST is a list of triples
   (START END PLIST), where START and END are positions and PLIST is a
   property list containing the text properties to add.  Adjust START
   and END positions by DELTA before adding properties.  Value is
   non-zero if OBJECT was modified.  */

int
add_text_properties_from_list (Lisp_Object object, Lisp_Object list, Lisp_Object delta)
{
  struct gcpro gcpro1, gcpro2;
  int modified_p = 0;

  GCPRO2 (list, object);

  for (; CONSP (list); list = XCDR (list))
    {
      Lisp_Object item, start, end, plist, tem;

      item = XCAR (list);
      start = make_number (XINT (XCAR (item)) + XINT (delta));
      end = make_number (XINT (XCAR (XCDR (item))) + XINT (delta));
      plist = XCAR (XCDR (XCDR (item)));

      tem = Fadd_text_properties (start, end, plist, object);
      if (!NILP (tem))
	modified_p = 1;
    }

  UNGCPRO;
  return modified_p;
}



/* Modify end-points of ranges in LIST destructively, and return the
   new list.  LIST is a list as returned from text_property_list.
   Discard properties that begin at or after NEW_END, and limit
   end-points to NEW_END.  */

Lisp_Object
extend_property_ranges (Lisp_Object list, Lisp_Object new_end)
{
  Lisp_Object prev = Qnil, head = list;
  EMACS_INT max = XINT (new_end);

  for (; CONSP (list); prev = list, list = XCDR (list))
    {
      Lisp_Object item, beg, end;

      item = XCAR (list);
      beg = XCAR (item);
      end = XCAR (XCDR (item));

      if (XINT (beg) >= max)
	{
	  /* The start-point is past the end of the new string.
	     Discard this property.  */
	  if (EQ (head, list))
	    head = XCDR (list);
	  else
	    XSETCDR (prev, XCDR (list));
	}
      else if (XINT (end) > max)
	/* The end-point is past the end of the new string.  */
	XSETCAR (XCDR (item), new_end);
    }

  return head;
}



/* Call the modification hook functions in LIST, each with START and END.  */

static void
call_mod_hooks (Lisp_Object list, Lisp_Object start, Lisp_Object end)
{
  struct gcpro gcpro1;
  GCPRO1 (list);
  while (!NILP (list))
    {
      call2 (Fcar (list), start, end);
      list = Fcdr (list);
    }
  UNGCPRO;
}

/* Check for read-only intervals between character positions START ... END,
   in BUF, and signal an error if we find one.

   Then check for any modification hooks in the range.
   Create a list of all these hooks in lexicographic order,
   eliminating consecutive extra copies of the same hook.  Then call
   those hooks in order, with START and END - 1 as arguments.  */

void
verify_interval_modification (struct buffer *buf,
			      EMACS_INT start, EMACS_INT end)
{
  register INTERVAL intervals = BUF_INTERVALS (buf);
  register INTERVAL i;
  Lisp_Object hooks;
  register Lisp_Object prev_mod_hooks;
  Lisp_Object mod_hooks;
  struct gcpro gcpro1;

  hooks = Qnil;
  prev_mod_hooks = Qnil;
  mod_hooks = Qnil;

  interval_insert_behind_hooks = Qnil;
  interval_insert_in_front_hooks = Qnil;

  if (NULL_INTERVAL_P (intervals))
    return;

  if (start > end)
    {
      EMACS_INT temp = start;
      start = end;
      end = temp;
    }

  /* For an insert operation, check the two chars around the position.  */
  if (start == end)
    {
      INTERVAL prev = NULL;
      Lisp_Object before, after;

      /* Set I to the interval containing the char after START,
	 and PREV to the interval containing the char before START.
	 Either one may be null.  They may be equal.  */
      i = find_interval (intervals, start);

      if (start == BUF_BEGV (buf))
	prev = 0;
      else if (i->position == start)
	prev = previous_interval (i);
      else if (i->position < start)
	prev = i;
      if (start == BUF_ZV (buf))
	i = 0;

      /* If Vinhibit_read_only is set and is not a list, we can
	 skip the read_only checks.  */
      if (NILP (Vinhibit_read_only) || CONSP (Vinhibit_read_only))
	{
	  /* If I and PREV differ we need to check for the read-only
	     property together with its stickiness.  If either I or
	     PREV are 0, this check is all we need.
	     We have to take special care, since read-only may be
	     indirectly defined via the category property.  */
	  if (i != prev)
	    {
	      if (! NULL_INTERVAL_P (i))
		{
		  after = textget (i->plist, Qread_only);

		  /* If interval I is read-only and read-only is
		     front-sticky, inhibit insertion.
		     Check for read-only as well as category.  */
		  if (! NILP (after)
		      && NILP (Fmemq (after, Vinhibit_read_only)))
		    {
		      Lisp_Object tem;

		      tem = textget (i->plist, Qfront_sticky);
		      if (TMEM (Qread_only, tem)
			  || (NILP (Fplist_get (i->plist, Qread_only))
			      && TMEM (Qcategory, tem)))
			text_read_only (after);
		    }
		}

	      if (! NULL_INTERVAL_P (prev))
		{
		  before = textget (prev->plist, Qread_only);

		  /* If interval PREV is read-only and read-only isn't
		     rear-nonsticky, inhibit insertion.
		     Check for read-only as well as category.  */
		  if (! NILP (before)
		      && NILP (Fmemq (before, Vinhibit_read_only)))
		    {
		      Lisp_Object tem;

		      tem = textget (prev->plist, Qrear_nonsticky);
		      if (! TMEM (Qread_only, tem)
			  && (! NILP (Fplist_get (prev->plist,Qread_only))
			      || ! TMEM (Qcategory, tem)))
			text_read_only (before);
		    }
		}
	    }
	  else if (! NULL_INTERVAL_P (i))
	    {
	      after = textget (i->plist, Qread_only);

	      /* If interval I is read-only and read-only is
		 front-sticky, inhibit insertion.
		 Check for read-only as well as category.  */
	      if (! NILP (after) && NILP (Fmemq (after, Vinhibit_read_only)))
		{
		  Lisp_Object tem;

		  tem = textget (i->plist, Qfront_sticky);
		  if (TMEM (Qread_only, tem)
		      || (NILP (Fplist_get (i->plist, Qread_only))
			  && TMEM (Qcategory, tem)))
		    text_read_only (after);

		  tem = textget (prev->plist, Qrear_nonsticky);
		  if (! TMEM (Qread_only, tem)
		      && (! NILP (Fplist_get (prev->plist, Qread_only))
			  || ! TMEM (Qcategory, tem)))
		    text_read_only (after);
		}
	    }
	}

      /* Run both insert hooks (just once if they're the same).  */
      if (!NULL_INTERVAL_P (prev))
	interval_insert_behind_hooks
	  = textget (prev->plist, Qinsert_behind_hooks);
      if (!NULL_INTERVAL_P (i))
	interval_insert_in_front_hooks
	  = textget (i->plist, Qinsert_in_front_hooks);
    }
  else
    {
      /* Loop over intervals on or next to START...END,
	 collecting their hooks.  */

      i = find_interval (intervals, start);
      do
	{
	  if (! INTERVAL_WRITABLE_P (i))
	    text_read_only (textget (i->plist, Qread_only));

	  if (!inhibit_modification_hooks)
	    {
	      mod_hooks = textget (i->plist, Qmodification_hooks);
	      if (! NILP (mod_hooks) && ! EQ (mod_hooks, prev_mod_hooks))
		{
		  hooks = Fcons (mod_hooks, hooks);
		  prev_mod_hooks = mod_hooks;
		}
	    }

	  i = next_interval (i);
	}
      /* Keep going thru the interval containing the char before END.  */
      while (! NULL_INTERVAL_P (i) && i->position < end);

      if (!inhibit_modification_hooks)
	{
	  GCPRO1 (hooks);
	  hooks = Fnreverse (hooks);
	  while (! EQ (hooks, Qnil))
	    {
	      call_mod_hooks (Fcar (hooks), make_number (start),
			      make_number (end));
	      hooks = Fcdr (hooks);
	    }
	  UNGCPRO;
	}
    }
}

/* Run the interval hooks for an insertion on character range START ... END.
   verify_interval_modification chose which hooks to run;
   this function is called after the insertion happens
   so it can indicate the range of inserted text.  */

void
report_interval_modification (Lisp_Object start, Lisp_Object end)
{
  if (! NILP (interval_insert_behind_hooks))
    call_mod_hooks (interval_insert_behind_hooks, start, end);
  if (! NILP (interval_insert_in_front_hooks)
      && ! EQ (interval_insert_in_front_hooks,
	       interval_insert_behind_hooks))
    call_mod_hooks (interval_insert_in_front_hooks, start, end);
}

void
syms_of_textprop (void)
{
  DEFVAR_LISP ("default-text-properties", Vdefault_text_properties,
	       doc: /* Property-list used as default values.
The value of a property in this list is seen as the value for every
character that does not have its own value for that property.  */);
  Vdefault_text_properties = Qnil;

  DEFVAR_LISP ("char-property-alias-alist", Vchar_property_alias_alist,
	       doc: /* Alist of alternative properties for properties without a value.
Each element should look like (PROPERTY ALTERNATIVE1 ALTERNATIVE2...).
If a piece of text has no direct value for a particular property, then
this alist is consulted.  If that property appears in the alist, then
the first non-nil value from the associated alternative properties is
returned. */);
  Vchar_property_alias_alist = Qnil;

  DEFVAR_LISP ("inhibit-point-motion-hooks", Vinhibit_point_motion_hooks,
	       doc: /* If non-nil, don't run `point-left' and `point-entered' text properties.
This also inhibits the use of the `intangible' text property.  */);
  Vinhibit_point_motion_hooks = Qnil;

  DEFVAR_LISP ("text-property-default-nonsticky",
	       Vtext_property_default_nonsticky,
	       doc: /* Alist of properties vs the corresponding non-stickiness.
Each element has the form (PROPERTY . NONSTICKINESS).

If a character in a buffer has PROPERTY, new text inserted adjacent to
the character doesn't inherit PROPERTY if NONSTICKINESS is non-nil,
inherits it if NONSTICKINESS is nil.  The `front-sticky' and
`rear-nonsticky' properties of the character override NONSTICKINESS.  */);
  /* Text properties `syntax-table'and `display' should be nonsticky
     by default.  */
  Vtext_property_default_nonsticky
    = Fcons (Fcons (intern_c_string ("syntax-table"), Qt),
	     Fcons (Fcons (intern_c_string ("display"), Qt), Qnil));

  staticpro (&interval_insert_behind_hooks);
  staticpro (&interval_insert_in_front_hooks);
  interval_insert_behind_hooks = Qnil;
  interval_insert_in_front_hooks = Qnil;


  /* Common attributes one might give text */

  DEFSYM (Qforeground, "foreground");
  DEFSYM (Qbackground, "background");
  DEFSYM (Qfont, "font");
  DEFSYM (Qstipple, "stipple");
  DEFSYM (Qunderline, "underline");
  DEFSYM (Qread_only, "read-only");
  DEFSYM (Qinvisible, "invisible");
  DEFSYM (Qintangible, "intangible");
  DEFSYM (Qcategory, "category");
  DEFSYM (Qlocal_map, "local-map");
  DEFSYM (Qfront_sticky, "front-sticky");
  DEFSYM (Qrear_nonsticky, "rear-nonsticky");
  DEFSYM (Qmouse_face, "mouse-face");
  DEFSYM (Qminibuffer_prompt, "minibuffer-prompt");

  /* Properties that text might use to specify certain actions */

  DEFSYM (Qmouse_left, "mouse-left");
  DEFSYM (Qmouse_entered, "mouse-entered");
  DEFSYM (Qpoint_left, "point-left");
  DEFSYM (Qpoint_entered, "point-entered");

  defsubr (&Stext_properties_at);
  defsubr (&Sget_text_property);
  defsubr (&Sget_char_property);
  defsubr (&Sget_char_property_and_overlay);
  defsubr (&Snext_char_property_change);
  defsubr (&Sprevious_char_property_change);
  defsubr (&Snext_single_char_property_change);
  defsubr (&Sprevious_single_char_property_change);
  defsubr (&Snext_property_change);
  defsubr (&Snext_single_property_change);
  defsubr (&Sprevious_property_change);
  defsubr (&Sprevious_single_property_change);
  defsubr (&Sadd_text_properties);
  defsubr (&Sput_text_property);
  defsubr (&Sset_text_properties);
  defsubr (&Sremove_text_properties);
  defsubr (&Sremove_list_of_text_properties);
  defsubr (&Stext_property_any);
  defsubr (&Stext_property_not_all);
}
