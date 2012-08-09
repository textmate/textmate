/* undo handling for GNU Emacs.
   Copyright (C) 1990, 1993-1994, 2000-2012  Free Software Foundation, Inc.

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
#include "buffer.h"
#include "commands.h"
#include "window.h"

/* Last buffer for which undo information was recorded.  */
/* BEWARE: This is not traced by the GC, so never dereference it!  */
static struct buffer *last_undo_buffer;

/* Position of point last time we inserted a boundary.  */
static struct buffer *last_boundary_buffer;
static EMACS_INT last_boundary_position;

Lisp_Object Qinhibit_read_only;

/* Marker for function call undo list elements.  */

Lisp_Object Qapply;

/* The first time a command records something for undo.
   it also allocates the undo-boundary object
   which will be added to the list at the end of the command.
   This ensures we can't run out of space while trying to make
   an undo-boundary.  */
static Lisp_Object pending_boundary;

/* Record point as it was at beginning of this command (if necessary)
   and prepare the undo info for recording a change.
   PT is the position of point that will naturally occur as a result of the
   undo record that will be added just after this command terminates.  */

static void
record_point (EMACS_INT pt)
{
  int at_boundary;

  /* Don't record position of pt when undo_inhibit_record_point holds.  */
  if (undo_inhibit_record_point)
    return;

  /* Allocate a cons cell to be the undo boundary after this command.  */
  if (NILP (pending_boundary))
    pending_boundary = Fcons (Qnil, Qnil);

  if ((current_buffer != last_undo_buffer)
      /* Don't call Fundo_boundary for the first change.  Otherwise we
	 risk overwriting last_boundary_position in Fundo_boundary with
	 PT of the current buffer and as a consequence not insert an
	 undo boundary because last_boundary_position will equal pt in
	 the test at the end of the present function (Bug#731).  */
      && (MODIFF > SAVE_MODIFF))
    Fundo_boundary ();
  last_undo_buffer = current_buffer;

  if (CONSP (BVAR (current_buffer, undo_list)))
    {
      /* Set AT_BOUNDARY to 1 only when we have nothing other than
         marker adjustment before undo boundary.  */

      Lisp_Object tail = BVAR (current_buffer, undo_list), elt;

      while (1)
	{
	  if (NILP (tail))
	    elt = Qnil;
	  else
	    elt = XCAR (tail);
	  if (NILP (elt) || ! (CONSP (elt) && MARKERP (XCAR (elt))))
	    break;
	  tail = XCDR (tail);
	}
      at_boundary = NILP (elt);
    }
  else
    at_boundary = 1;

  if (MODIFF <= SAVE_MODIFF)
    record_first_change ();

  /* If we are just after an undo boundary, and
     point wasn't at start of deleted range, record where it was.  */
  if (at_boundary
      && current_buffer == last_boundary_buffer
      && last_boundary_position != pt)
    BVAR (current_buffer, undo_list)
      = Fcons (make_number (last_boundary_position), BVAR (current_buffer, undo_list));
}

/* Record an insertion that just happened or is about to happen,
   for LENGTH characters at position BEG.
   (It is possible to record an insertion before or after the fact
   because we don't need to record the contents.)  */

void
record_insert (EMACS_INT beg, EMACS_INT length)
{
  Lisp_Object lbeg, lend;

  if (EQ (BVAR (current_buffer, undo_list), Qt))
    return;

  record_point (beg);

  /* If this is following another insertion and consecutive with it
     in the buffer, combine the two.  */
  if (CONSP (BVAR (current_buffer, undo_list)))
    {
      Lisp_Object elt;
      elt = XCAR (BVAR (current_buffer, undo_list));
      if (CONSP (elt)
	  && INTEGERP (XCAR (elt))
	  && INTEGERP (XCDR (elt))
	  && XINT (XCDR (elt)) == beg)
	{
	  XSETCDR (elt, make_number (beg + length));
	  return;
	}
    }

  XSETFASTINT (lbeg, beg);
  XSETINT (lend, beg + length);
  BVAR (current_buffer, undo_list) = Fcons (Fcons (lbeg, lend),
                                     BVAR (current_buffer, undo_list));
}

/* Record that a deletion is about to take place,
   of the characters in STRING, at location BEG.  */

void
record_delete (EMACS_INT beg, Lisp_Object string)
{
  Lisp_Object sbeg;

  if (EQ (BVAR (current_buffer, undo_list), Qt))
    return;

  if (PT == beg + SCHARS (string))
    {
      XSETINT (sbeg, -beg);
      record_point (PT);
    }
  else
    {
      XSETFASTINT (sbeg, beg);
      record_point (beg);
    }

  BVAR (current_buffer, undo_list)
    = Fcons (Fcons (string, sbeg), BVAR (current_buffer, undo_list));
}

/* Record the fact that MARKER is about to be adjusted by ADJUSTMENT.
   This is done only when a marker points within text being deleted,
   because that's the only case where an automatic marker adjustment
   won't be inverted automatically by undoing the buffer modification.  */

void
record_marker_adjustment (Lisp_Object marker, EMACS_INT adjustment)
{
  if (EQ (BVAR (current_buffer, undo_list), Qt))
    return;

  /* Allocate a cons cell to be the undo boundary after this command.  */
  if (NILP (pending_boundary))
    pending_boundary = Fcons (Qnil, Qnil);

  if (current_buffer != last_undo_buffer)
    Fundo_boundary ();
  last_undo_buffer = current_buffer;

  BVAR (current_buffer, undo_list)
    = Fcons (Fcons (marker, make_number (adjustment)),
	     BVAR (current_buffer, undo_list));
}

/* Record that a replacement is about to take place,
   for LENGTH characters at location BEG.
   The replacement must not change the number of characters.  */

void
record_change (EMACS_INT beg, EMACS_INT length)
{
  record_delete (beg, make_buffer_string (beg, beg + length, 1));
  record_insert (beg, length);
}

/* Record that an unmodified buffer is about to be changed.
   Record the file modification date so that when undoing this entry
   we can tell whether it is obsolete because the file was saved again.  */

void
record_first_change (void)
{
  struct buffer *base_buffer = current_buffer;

  if (EQ (BVAR (current_buffer, undo_list), Qt))
    return;

  if (current_buffer != last_undo_buffer)
    Fundo_boundary ();
  last_undo_buffer = current_buffer;

  if (base_buffer->base_buffer)
    base_buffer = base_buffer->base_buffer;

  BVAR (current_buffer, undo_list) =
    Fcons (Fcons (Qt, INTEGER_TO_CONS (base_buffer->modtime)),
	   BVAR (current_buffer, undo_list));
}

/* Record a change in property PROP (whose old value was VAL)
   for LENGTH characters starting at position BEG in BUFFER.  */

void
record_property_change (EMACS_INT beg, EMACS_INT length,
			Lisp_Object prop, Lisp_Object value,
			Lisp_Object buffer)
{
  Lisp_Object lbeg, lend, entry;
  struct buffer *obuf = current_buffer, *buf = XBUFFER (buffer);
  int boundary = 0;

  if (EQ (BVAR (buf, undo_list), Qt))
    return;

  /* Allocate a cons cell to be the undo boundary after this command.  */
  if (NILP (pending_boundary))
    pending_boundary = Fcons (Qnil, Qnil);

  if (buf != last_undo_buffer)
    boundary = 1;
  last_undo_buffer = buf;

  /* Switch temporarily to the buffer that was changed.  */
  current_buffer = buf;

  if (boundary)
    Fundo_boundary ();

  if (MODIFF <= SAVE_MODIFF)
    record_first_change ();

  XSETINT (lbeg, beg);
  XSETINT (lend, beg + length);
  entry = Fcons (Qnil, Fcons (prop, Fcons (value, Fcons (lbeg, lend))));
  BVAR (current_buffer, undo_list) = Fcons (entry, BVAR (current_buffer, undo_list));

  current_buffer = obuf;
}

DEFUN ("undo-boundary", Fundo_boundary, Sundo_boundary, 0, 0, 0,
       doc: /* Mark a boundary between units of undo.
An undo command will stop at this point,
but another undo command will undo to the previous boundary.  */)
  (void)
{
  Lisp_Object tem;
  if (EQ (BVAR (current_buffer, undo_list), Qt))
    return Qnil;
  tem = Fcar (BVAR (current_buffer, undo_list));
  if (!NILP (tem))
    {
      /* One way or another, cons nil onto the front of the undo list.  */
      if (!NILP (pending_boundary))
	{
	  /* If we have preallocated the cons cell to use here,
	     use that one.  */
	  XSETCDR (pending_boundary, BVAR (current_buffer, undo_list));
	  BVAR (current_buffer, undo_list) = pending_boundary;
	  pending_boundary = Qnil;
	}
      else
	BVAR (current_buffer, undo_list) = Fcons (Qnil, BVAR (current_buffer, undo_list));
    }
  last_boundary_position = PT;
  last_boundary_buffer = current_buffer;
  return Qnil;
}

/* At garbage collection time, make an undo list shorter at the end,
   returning the truncated list.  How this is done depends on the
   variables undo-limit, undo-strong-limit and undo-outer-limit.
   In some cases this works by calling undo-outer-limit-function.  */

void
truncate_undo_list (struct buffer *b)
{
  Lisp_Object list;
  Lisp_Object prev, next, last_boundary;
  int size_so_far = 0;

  /* Make sure that calling undo-outer-limit-function
     won't cause another GC.  */
  int count = inhibit_garbage_collection ();

  /* Make the buffer current to get its local values of variables such
     as undo_limit.  Also so that Vundo_outer_limit_function can
     tell which buffer to operate on.  */
  record_unwind_protect (set_buffer_if_live, Fcurrent_buffer ());
  set_buffer_internal (b);

  list = BVAR (b, undo_list);

  prev = Qnil;
  next = list;
  last_boundary = Qnil;

  /* If the first element is an undo boundary, skip past it.  */
  if (CONSP (next) && NILP (XCAR (next)))
    {
      /* Add in the space occupied by this element and its chain link.  */
      size_so_far += sizeof (struct Lisp_Cons);

      /* Advance to next element.  */
      prev = next;
      next = XCDR (next);
    }

  /* Always preserve at least the most recent undo record
     unless it is really horribly big.

     Skip, skip, skip the undo, skip, skip, skip the undo,
     Skip, skip, skip the undo, skip to the undo bound'ry.  */

  while (CONSP (next) && ! NILP (XCAR (next)))
    {
      Lisp_Object elt;
      elt = XCAR (next);

      /* Add in the space occupied by this element and its chain link.  */
      size_so_far += sizeof (struct Lisp_Cons);
      if (CONSP (elt))
	{
	  size_so_far += sizeof (struct Lisp_Cons);
	  if (STRINGP (XCAR (elt)))
	    size_so_far += (sizeof (struct Lisp_String) - 1
			    + SCHARS (XCAR (elt)));
	}

      /* Advance to next element.  */
      prev = next;
      next = XCDR (next);
    }

  /* If by the first boundary we have already passed undo_outer_limit,
     we're heading for memory full, so offer to clear out the list.  */
  if (INTEGERP (Vundo_outer_limit)
      && size_so_far > XINT (Vundo_outer_limit)
      && !NILP (Vundo_outer_limit_function))
    {
      Lisp_Object tem;
      struct buffer *temp = last_undo_buffer;

      /* Normally the function this calls is undo-outer-limit-truncate.  */
      tem = call1 (Vundo_outer_limit_function, make_number (size_so_far));
      if (! NILP (tem))
	{
	  /* The function is responsible for making
	     any desired changes in buffer-undo-list.  */
	  unbind_to (count, Qnil);
	  return;
	}
      /* That function probably used the minibuffer, and if so, that
	 changed last_undo_buffer.  Change it back so that we don't
	 force next change to make an undo boundary here.  */
      last_undo_buffer = temp;
    }

  if (CONSP (next))
    last_boundary = prev;

  /* Keep additional undo data, if it fits in the limits.  */
  while (CONSP (next))
    {
      Lisp_Object elt;
      elt = XCAR (next);

      /* When we get to a boundary, decide whether to truncate
	 either before or after it.  The lower threshold, undo_limit,
	 tells us to truncate after it.  If its size pushes past
	 the higher threshold undo_strong_limit, we truncate before it.  */
      if (NILP (elt))
	{
	  if (size_so_far > undo_strong_limit)
	    break;
	  last_boundary = prev;
	  if (size_so_far > undo_limit)
	    break;
	}

      /* Add in the space occupied by this element and its chain link.  */
      size_so_far += sizeof (struct Lisp_Cons);
      if (CONSP (elt))
	{
	  size_so_far += sizeof (struct Lisp_Cons);
	  if (STRINGP (XCAR (elt)))
	    size_so_far += (sizeof (struct Lisp_String) - 1
			    + SCHARS (XCAR (elt)));
	}

      /* Advance to next element.  */
      prev = next;
      next = XCDR (next);
    }

  /* If we scanned the whole list, it is short enough; don't change it.  */
  if (NILP (next))
    ;
  /* Truncate at the boundary where we decided to truncate.  */
  else if (!NILP (last_boundary))
    XSETCDR (last_boundary, Qnil);
  /* There's nothing we decided to keep, so clear it out.  */
  else
    BVAR (b, undo_list) = Qnil;

  unbind_to (count, Qnil);
}

DEFUN ("primitive-undo", Fprimitive_undo, Sprimitive_undo, 2, 2, 0,
       doc: /* Undo N records from the front of the list LIST.
Return what remains of the list.  */)
  (Lisp_Object n, Lisp_Object list)
{
  struct gcpro gcpro1, gcpro2;
  Lisp_Object next;
  int count = SPECPDL_INDEX ();
  register int arg;
  Lisp_Object oldlist;
  int did_apply = 0;

#if 0  /* This is a good feature, but would make undo-start
	  unable to do what is expected.  */
  Lisp_Object tem;

  /* If the head of the list is a boundary, it is the boundary
     preceding this command.  Get rid of it and don't count it.  */
  tem = Fcar (list);
  if (NILP (tem))
    list = Fcdr (list);
#endif

  CHECK_NUMBER (n);
  arg = XINT (n);
  next = Qnil;
  GCPRO2 (next, list);
  /* I don't think we need to gcpro oldlist, as we use it only
     to check for EQ.  ++kfs  */

  /* In a writable buffer, enable undoing read-only text that is so
     because of text properties.  */
  if (NILP (BVAR (current_buffer, read_only)))
    specbind (Qinhibit_read_only, Qt);

  /* Don't let `intangible' properties interfere with undo.  */
  specbind (Qinhibit_point_motion_hooks, Qt);

  oldlist = BVAR (current_buffer, undo_list);

  while (arg > 0)
    {
      while (CONSP (list))
	{
	  next = XCAR (list);
	  list = XCDR (list);
	  /* Exit inner loop at undo boundary.  */
	  if (NILP (next))
	    break;
	  /* Handle an integer by setting point to that value.  */
	  if (INTEGERP (next))
	    SET_PT (clip_to_bounds (BEGV, XINT (next), ZV));
	  else if (CONSP (next))
	    {
	      Lisp_Object car, cdr;

	      car = XCAR (next);
	      cdr = XCDR (next);
	      if (EQ (car, Qt))
		{
		  /* Element (t high . low) records previous modtime.  */
		  struct buffer *base_buffer = current_buffer;
		  time_t mod_time;
		  CONS_TO_INTEGER (cdr, time_t, mod_time);

		  if (current_buffer->base_buffer)
		    base_buffer = current_buffer->base_buffer;

		  /* If this records an obsolete save
		     (not matching the actual disk file)
		     then don't mark unmodified.  */
		  if (mod_time != base_buffer->modtime)
		    continue;
#ifdef CLASH_DETECTION
		  Funlock_buffer ();
#endif /* CLASH_DETECTION */
		  Fset_buffer_modified_p (Qnil);
		}
	      else if (EQ (car, Qnil))
		{
		  /* Element (nil PROP VAL BEG . END) is property change.  */
		  Lisp_Object beg, end, prop, val;

		  prop = Fcar (cdr);
		  cdr = Fcdr (cdr);
		  val = Fcar (cdr);
		  cdr = Fcdr (cdr);
		  beg = Fcar (cdr);
		  end = Fcdr (cdr);

		  if (XINT (beg) < BEGV || XINT (end) > ZV)
		    error ("Changes to be undone are outside visible portion of buffer");
		  Fput_text_property (beg, end, prop, val, Qnil);
		}
	      else if (INTEGERP (car) && INTEGERP (cdr))
		{
		  /* Element (BEG . END) means range was inserted.  */

		  if (XINT (car) < BEGV
		      || XINT (cdr) > ZV)
		    error ("Changes to be undone are outside visible portion of buffer");
		  /* Set point first thing, so that undoing this undo
		     does not send point back to where it is now.  */
		  Fgoto_char (car);
		  Fdelete_region (car, cdr);
		}
	      else if (EQ (car, Qapply))
		{
		  /* Element (apply FUN . ARGS) means call FUN to undo.  */
		  struct buffer *save_buffer = current_buffer;

		  car = Fcar (cdr);
		  cdr = Fcdr (cdr);
		  if (INTEGERP (car))
		    {
		      /* Long format: (apply DELTA START END FUN . ARGS).  */
		      Lisp_Object delta = car;
		      Lisp_Object start = Fcar (cdr);
		      Lisp_Object end   = Fcar (Fcdr (cdr));
		      Lisp_Object start_mark = Fcopy_marker (start, Qnil);
		      Lisp_Object end_mark   = Fcopy_marker (end, Qt);

		      cdr = Fcdr (Fcdr (cdr));
		      apply1 (Fcar (cdr), Fcdr (cdr));

		      /* Check that the function did what the entry said it
			 would do.  */
		      if (!EQ (start, Fmarker_position (start_mark))
			  || (XINT (delta) + XINT (end)
			      != marker_position (end_mark)))
			error ("Changes to be undone by function different than announced");
		      Fset_marker (start_mark, Qnil, Qnil);
		      Fset_marker (end_mark, Qnil, Qnil);
		    }
		  else
		    apply1 (car, cdr);

		  if (save_buffer != current_buffer)
		    error ("Undo function switched buffer");
		  did_apply = 1;
		}
	      else if (STRINGP (car) && INTEGERP (cdr))
		{
		  /* Element (STRING . POS) means STRING was deleted.  */
		  Lisp_Object membuf;
		  EMACS_INT pos = XINT (cdr);

		  membuf = car;
		  if (pos < 0)
		    {
		      if (-pos < BEGV || -pos > ZV)
			error ("Changes to be undone are outside visible portion of buffer");
		      SET_PT (-pos);
		      Finsert (1, &membuf);
		    }
		  else
		    {
		      if (pos < BEGV || pos > ZV)
			error ("Changes to be undone are outside visible portion of buffer");
		      SET_PT (pos);

		      /* Now that we record marker adjustments
			 (caused by deletion) for undo,
			 we should always insert after markers,
			 so that undoing the marker adjustments
			 put the markers back in the right place.  */
		      Finsert (1, &membuf);
		      SET_PT (pos);
		    }
		}
	      else if (MARKERP (car) && INTEGERP (cdr))
		{
		  /* (MARKER . INTEGER) means a marker MARKER
		     was adjusted by INTEGER.  */
		  if (XMARKER (car)->buffer)
		    Fset_marker (car,
				 make_number (marker_position (car) - XINT (cdr)),
				 Fmarker_buffer (car));
		}
	    }
	}
      arg--;
    }


  /* Make sure an apply entry produces at least one undo entry,
     so the test in `undo' for continuing an undo series
     will work right.  */
  if (did_apply
      && EQ (oldlist, BVAR (current_buffer, undo_list)))
    BVAR (current_buffer, undo_list)
      = Fcons (list3 (Qapply, Qcdr, Qnil), BVAR (current_buffer, undo_list));

  UNGCPRO;
  return unbind_to (count, list);
}

void
syms_of_undo (void)
{
  DEFSYM (Qinhibit_read_only, "inhibit-read-only");
  DEFSYM (Qapply, "apply");

  pending_boundary = Qnil;
  staticpro (&pending_boundary);

  last_undo_buffer = NULL;
  last_boundary_buffer = NULL;

  defsubr (&Sprimitive_undo);
  defsubr (&Sundo_boundary);

  DEFVAR_INT ("undo-limit", undo_limit,
	      doc: /* Keep no more undo information once it exceeds this size.
This limit is applied when garbage collection happens.
When a previous command increases the total undo list size past this
value, the earlier commands that came before it are forgotten.

The size is counted as the number of bytes occupied,
which includes both saved text and other data.  */);
  undo_limit = 80000;

  DEFVAR_INT ("undo-strong-limit", undo_strong_limit,
	      doc: /* Don't keep more than this much size of undo information.
This limit is applied when garbage collection happens.
When a previous command increases the total undo list size past this
value, that command and the earlier commands that came before it are forgotten.
However, the most recent buffer-modifying command's undo info
is never discarded for this reason.

The size is counted as the number of bytes occupied,
which includes both saved text and other data.  */);
  undo_strong_limit = 120000;

  DEFVAR_LISP ("undo-outer-limit", Vundo_outer_limit,
	      doc: /* Outer limit on size of undo information for one command.
At garbage collection time, if the current command has produced
more than this much undo information, it discards the info and displays
a warning.  This is a last-ditch limit to prevent memory overflow.

The size is counted as the number of bytes occupied, which includes
both saved text and other data.  A value of nil means no limit.  In
this case, accumulating one huge undo entry could make Emacs crash as
a result of memory overflow.

In fact, this calls the function which is the value of
`undo-outer-limit-function' with one argument, the size.
The text above describes the behavior of the function
that variable usually specifies.  */);
  Vundo_outer_limit = make_number (12000000);

  DEFVAR_LISP ("undo-outer-limit-function", Vundo_outer_limit_function,
	       doc: /* Function to call when an undo list exceeds `undo-outer-limit'.
This function is called with one argument, the current undo list size
for the most recent command (since the last undo boundary).
If the function returns t, that means truncation has been fully handled.
If it returns nil, the other forms of truncation are done.

Garbage collection is inhibited around the call to this function,
so it must make sure not to do a lot of consing.  */);
  Vundo_outer_limit_function = Qnil;

  DEFVAR_BOOL ("undo-inhibit-record-point", undo_inhibit_record_point,
	       doc: /* Non-nil means do not record `point' in `buffer-undo-list'.  */);
  undo_inhibit_record_point = 0;
}
