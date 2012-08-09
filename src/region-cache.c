/* Caching facts about regions of the buffer, for optimization.

Copyright (C) 1985-1989, 1993, 1995, 2001-2012
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
#include <stdio.h>
#include <setjmp.h>

#include "lisp.h"
#include "buffer.h"
#include "region-cache.h"


/* Data structures.  */

/* The region cache.

   We want something that maps character positions in a buffer onto
   values.  The representation should deal well with long runs of
   characters with the same value.

   The tricky part: the representation should be very cheap to
   maintain in the presence of many insertions and deletions.  If the
   overhead of maintaining the cache is too high, the speedups it
   offers will be worthless.


   We represent the region cache as a sorted array of struct
   boundary's, each of which contains a buffer position and a value;
   the value applies to all the characters after the buffer position,
   until the position of the next boundary, or the end of the buffer.

   The cache always has a boundary whose position is BUF_BEG, so
   there's always a value associated with every character in the
   buffer.  Since the cache is sorted, this is always the first
   element of the cache.

   To facilitate the insertion and deletion of boundaries in the
   cache, the cache has a gap, just like Emacs's text buffers do.

   To help boundary positions float along with insertions and
   deletions, all boundary positions before the cache gap are stored
   relative to BUF_BEG (buf) (thus they're >= 0), and all boundary
   positions after the gap are stored relative to BUF_Z (buf) (thus
   they're <= 0).  Look at BOUNDARY_POS to see this in action.  See
   revalidate_region_cache to see how this helps.  */

struct boundary {
  ptrdiff_t pos;
  int value;
};

struct region_cache {
  /* A sorted array of locations where the known-ness of the buffer
     changes.  */
  struct boundary *boundaries;

  /* boundaries[gap_start ... gap_start + gap_len - 1] is the gap.  */
  ptrdiff_t gap_start, gap_len;

  /* The number of elements allocated to boundaries, not including the
     gap.  */
  ptrdiff_t cache_len;

  /* The areas that haven't changed since the last time we cleaned out
     invalid entries from the cache.  These overlap when the buffer is
     entirely unchanged.  */
  ptrdiff_t beg_unchanged, end_unchanged;

  /* The first and last positions in the buffer.  Because boundaries
     store their positions relative to the start (BEG) and end (Z) of
     the buffer, knowing these positions allows us to accurately
     interpret positions without having to pass the buffer structure
     or its endpoints around all the time.

     Yes, buffer_beg is always 1.  It's there for symmetry with
     buffer_end and the BEG and BUF_BEG macros.  */
  ptrdiff_t buffer_beg, buffer_end;
};

/* Return the position of boundary i in cache c.  */
#define BOUNDARY_POS(c, i) \
  ((i) < (c)->gap_start \
   ? (c)->buffer_beg + (c)->boundaries[(i)].pos \
   : (c)->buffer_end + (c)->boundaries[(c)->gap_len + (i)].pos)

/* Return the value for text after boundary i in cache c.  */
#define BOUNDARY_VALUE(c, i) \
  ((i) < (c)->gap_start \
   ? (c)->boundaries[(i)].value \
   : (c)->boundaries[(c)->gap_len + (i)].value)

/* Set the value for text after boundary i in cache c to v.  */
#define SET_BOUNDARY_VALUE(c, i, v) \
  ((i) < (c)->gap_start \
   ? ((c)->boundaries[(i)].value = (v))\
   : ((c)->boundaries[(c)->gap_len + (i)].value = (v)))


/* How many elements to add to the gap when we resize the buffer.  */
#define NEW_CACHE_GAP (40)

/* See invalidate_region_cache; if an invalidation would throw away
   information about this many characters, call
   revalidate_region_cache before doing the new invalidation, to
   preserve that information, instead of throwing it away.  */
#define PRESERVE_THRESHOLD (500)

static void revalidate_region_cache (struct buffer *buf, struct region_cache *c);


/* Interface: Allocating, initializing, and disposing of region caches.  */

struct region_cache *
new_region_cache (void)
{
  struct region_cache *c
    = (struct region_cache *) xmalloc (sizeof (struct region_cache));

  c->gap_start = 0;
  c->gap_len = NEW_CACHE_GAP;
  c->cache_len = 0;
  c->boundaries =
    (struct boundary *) xmalloc ((c->gap_len + c->cache_len)
                                 * sizeof (*c->boundaries));

  c->beg_unchanged = 0;
  c->end_unchanged = 0;
  c->buffer_beg = BEG;
  c->buffer_end = BEG;

  /* Insert the boundary for the buffer start.  */
  c->cache_len++;
  c->gap_len--;
  c->gap_start++;
  c->boundaries[0].pos   = 0;  /* from buffer_beg */
  c->boundaries[0].value = 0;

  return c;
}

void
free_region_cache (struct region_cache *c)
{
  xfree (c->boundaries);
  xfree (c);
}


/* Finding positions in the cache.  */

/* Return the index of the last boundary in cache C at or before POS.
   In other words, return the boundary that specifies the value for
   the region POS..(POS + 1).

   This operation should be logarithmic in the number of cache
   entries.  It would be nice if it took advantage of locality of
   reference, too, by searching entries near the last entry found.  */
static ptrdiff_t
find_cache_boundary (struct region_cache *c, ptrdiff_t pos)
{
  ptrdiff_t low = 0, high = c->cache_len;

  while (low + 1 < high)
    {
      /* mid is always a valid index, because low < high and ">> 1"
         rounds down.  */
      ptrdiff_t mid = (low >> 1) + (high >> 1) + (low & high & 1);
      ptrdiff_t boundary = BOUNDARY_POS (c, mid);

      if (pos < boundary)
        high = mid;
      else
        low = mid;
    }

  /* Some testing.  */
  if (BOUNDARY_POS (c, low) > pos
      || (low + 1 < c->cache_len
          && BOUNDARY_POS (c, low + 1) <= pos))
      abort ();

  return low;
}



/* Moving the cache gap around, inserting, and deleting.  */


/* Move the gap of cache C to index POS, and make sure it has space
   for at least MIN_SIZE boundaries.  */
static void
move_cache_gap (struct region_cache *c, ptrdiff_t pos, ptrdiff_t min_size)
{
  /* Copy these out of the cache and into registers.  */
  ptrdiff_t gap_start = c->gap_start;
  ptrdiff_t gap_len = c->gap_len;
  ptrdiff_t buffer_beg = c->buffer_beg;
  ptrdiff_t buffer_end = c->buffer_end;

  if (pos < 0
      || pos > c->cache_len)
    abort ();

  /* We mustn't ever try to put the gap before the dummy start
     boundary.  That must always be start-relative.  */
  if (pos == 0)
    abort ();

  /* Need we move the gap right?  */
  while (gap_start < pos)
    {
      /* Copy one boundary from after to before the gap, and
         convert its position to start-relative.  */
      c->boundaries[gap_start].pos
        = (buffer_end
           + c->boundaries[gap_start + gap_len].pos
           - buffer_beg);
      c->boundaries[gap_start].value
        = c->boundaries[gap_start + gap_len].value;
      gap_start++;
    }

  /* To enlarge the gap, we need to re-allocate the boundary array, and
     then shift the area after the gap to the new end.  Since the cost
     is proportional to the amount of stuff after the gap, we do the
     enlargement here, after a right shift but before a left shift,
     when the portion after the gap is smallest.  */
  if (gap_len < min_size)
    {
      ptrdiff_t i;

      c->boundaries =
	xpalloc (c->boundaries, &c->cache_len, min_size, -1,
		 sizeof *c->boundaries);

      /* Some systems don't provide a version of the copy routine that
         can be trusted to shift memory upward into an overlapping
         region.  memmove isn't widely available.  */
      min_size -= gap_len;
      for (i = c->cache_len - 1; i >= gap_start; i--)
        {
          c->boundaries[i + min_size].pos   = c->boundaries[i + gap_len].pos;
          c->boundaries[i + min_size].value = c->boundaries[i + gap_len].value;
        }

      gap_len = min_size;
    }

  /* Need we move the gap left?  */
  while (pos < gap_start)
    {
      gap_start--;

      /* Copy one region from before to after the gap, and
         convert its position to end-relative.  */
      c->boundaries[gap_start + gap_len].pos
        = c->boundaries[gap_start].pos + buffer_beg - buffer_end;
      c->boundaries[gap_start + gap_len].value
        = c->boundaries[gap_start].value;
    }

  /* Assign these back into the cache.  */
  c->gap_start = gap_start;
  c->gap_len  = gap_len;
}


/* Insert a new boundary in cache C; it will have cache index I,
   and have the specified POS and VALUE.  */
static void
insert_cache_boundary (struct region_cache *c, ptrdiff_t i, ptrdiff_t pos,
		       int value)
{
  /* i must be a valid cache index.  */
  if (i < 0 || i > c->cache_len)
    abort ();

  /* We must never want to insert something before the dummy first
     boundary.  */
  if (i == 0)
    abort ();

  /* We must only be inserting things in order.  */
  if (! (BOUNDARY_POS (c, i - 1) < pos
         && (i == c->cache_len
             || pos < BOUNDARY_POS (c, i))))
    abort ();

  /* The value must be different from the ones around it.  However, we
     temporarily create boundaries that establish the same value as
     the subsequent boundary, so we're not going to flag that case.  */
  if (BOUNDARY_VALUE (c, i - 1) == value)
    abort ();

  move_cache_gap (c, i, 1);

  c->boundaries[i].pos = pos - c->buffer_beg;
  c->boundaries[i].value = value;
  c->gap_start++;
  c->gap_len--;
  c->cache_len++;
}


/* Delete the i'th entry from cache C if START <= i < END.  */

static void
delete_cache_boundaries (struct region_cache *c,
			 ptrdiff_t start, ptrdiff_t end)
{
  ptrdiff_t len = end - start;

  /* Gotta be in range.  */
  if (start < 0
      || end > c->cache_len)
    abort ();

  /* Gotta be in order.  */
  if (start > end)
    abort ();

  /* Can't delete the dummy entry.  */
  if (start == 0
      && end >= 1)
    abort ();

  /* Minimize gap motion.  If we're deleting nothing, do nothing.  */
  if (len == 0)
    ;
  /* If the gap is before the region to delete, delete from the start
     forward.  */
  else if (c->gap_start <= start)
    {
      move_cache_gap (c, start, 0);
      c->gap_len += len;
    }
  /* If the gap is after the region to delete, delete from the end
     backward.  */
  else if (end <= c->gap_start)
    {
      move_cache_gap (c, end, 0);
      c->gap_start -= len;
      c->gap_len   += len;
    }
  /* If the gap is in the region to delete, just expand it.  */
  else
    {
      c->gap_start = start;
      c->gap_len   += len;
    }

  c->cache_len -= len;
}



/* Set the value for a region.  */

/* Set the value in cache C for the region START..END to VALUE.  */
static void
set_cache_region (struct region_cache *c,
		  ptrdiff_t start, ptrdiff_t end, int value)
{
  if (start > end)
    abort ();
  if (start < c->buffer_beg
      || end   > c->buffer_end)
    abort ();

  /* Eliminate this case; then we can assume that start and end-1 are
     both the locations of real characters in the buffer.  */
  if (start == end)
    return;

  {
    /* We need to make sure that there are no boundaries in the area
       between start to end; the whole area will have the same value,
       so those boundaries will not be necessary.

       Let start_ix be the cache index of the boundary governing the
       first character of start..end, and let end_ix be the cache
       index of the earliest boundary after the last character in
       start..end.  (This tortured terminology is intended to answer
       all the "< or <=?" sort of questions.)  */
    ptrdiff_t start_ix = find_cache_boundary (c, start);
    ptrdiff_t end_ix   = find_cache_boundary (c, end - 1) + 1;

    /* We must remember the value established by the last boundary
       before end; if that boundary's domain stretches beyond end,
       we'll need to create a new boundary at end, and that boundary
       must have that remembered value.  */
    int value_at_end = BOUNDARY_VALUE (c, end_ix - 1);

    /* Delete all boundaries strictly within start..end; this means
       those whose indices are between start_ix (exclusive) and end_ix
       (exclusive).  */
    delete_cache_boundaries (c, start_ix + 1, end_ix);

    /* Make sure we have the right value established going in to
       start..end from the left, and no unnecessary boundaries.  */
    if (BOUNDARY_POS (c, start_ix) == start)
      {
        /* Is this boundary necessary?  If no, remove it; if yes, set
           its value.  */
        if (start_ix > 0
            && BOUNDARY_VALUE (c, start_ix - 1) == value)
          {
            delete_cache_boundaries (c, start_ix, start_ix + 1);
            start_ix--;
          }
        else
          SET_BOUNDARY_VALUE (c, start_ix, value);
      }
    else
      {
        /* Do we need to add a new boundary here?  */
        if (BOUNDARY_VALUE (c, start_ix) != value)
          {
            insert_cache_boundary (c, start_ix + 1, start, value);
            start_ix++;
          }
      }

    /* This is equivalent to letting end_ix float (like a buffer
       marker does) with the insertions and deletions we may have
       done.  */
    end_ix = start_ix + 1;

    /* Make sure we have the correct value established as we leave
       start..end to the right.  */
    if (end == c->buffer_end)
      /* There is no text after start..end; nothing to do.  */
      ;
    else if (end_ix >= c->cache_len
             || end < BOUNDARY_POS (c, end_ix))
      {
        /* There is no boundary at end, but we may need one.  */
        if (value_at_end != value)
          insert_cache_boundary (c, end_ix, end, value_at_end);
      }
    else
      {
        /* There is a boundary at end; should it be there?  */
        if (value == BOUNDARY_VALUE (c, end_ix))
          delete_cache_boundaries (c, end_ix, end_ix + 1);
      }
  }
}



/* Interface: Invalidating the cache.  Private: Re-validating the cache.  */

/* Indicate that a section of BUF has changed, to invalidate CACHE.
   HEAD is the number of chars unchanged at the beginning of the buffer.
   TAIL is the number of chars unchanged at the end of the buffer.
      NOTE: this is *not* the same as the ending position of modified
      region.
   (This way of specifying regions makes more sense than absolute
   buffer positions in the presence of insertions and deletions; the
   args to pass are the same before and after such an operation.)  */
void
invalidate_region_cache (struct buffer *buf, struct region_cache *c,
			 ptrdiff_t head, ptrdiff_t tail)
{
  /* Let chead = c->beg_unchanged, and
         ctail = c->end_unchanged.
     If z-tail < beg+chead by a large amount, or
        z-ctail < beg+head by a large amount,

     then cutting back chead and ctail to head and tail would lose a
     lot of information that we could preserve by revalidating the
     cache before processing this invalidation.  Losing that
     information may be more costly than revalidating the cache now.
     So go ahead and call revalidate_region_cache if it seems that it
     might be worthwhile.  */
  if (((BUF_BEG (buf) + c->beg_unchanged) - (BUF_Z (buf) - tail)
       > PRESERVE_THRESHOLD)
      || ((BUF_BEG (buf) + head) - (BUF_Z (buf) - c->end_unchanged)
          > PRESERVE_THRESHOLD))
    revalidate_region_cache (buf, c);


  if (head < c->beg_unchanged)
    c->beg_unchanged = head;
  if (tail < c->end_unchanged)
    c->end_unchanged = tail;

  /* We now know nothing about the region between the unchanged head
     and the unchanged tail (call it the "modified region"), not even
     its length.

     If the modified region has shrunk in size (deletions do this),
     then the cache may now contain boundaries originally located in
     text that doesn't exist any more.

     If the modified region has increased in size (insertions do
     this), then there may now be boundaries in the modified region
     whose positions are wrong.

     Even calling BOUNDARY_POS on boundaries still in the unchanged
     head or tail may well give incorrect answers now, since
     c->buffer_beg and c->buffer_end may well be wrong now.  (Well,
     okay, c->buffer_beg never changes, so boundaries in the unchanged
     head will still be okay.  But it's the principle of the thing.)

     So things are generally a mess.

     But we don't clean up this mess here; that would be expensive,
     and this function gets called every time any buffer modification
     occurs.  Rather, we can clean up everything in one swell foop,
     accounting for all the modifications at once, by calling
     revalidate_region_cache before we try to consult the cache the
     next time.  */
}


/* Clean out any cache entries applying to the modified region, and
   make the positions of the remaining entries accurate again.

   After calling this function, the mess described in the comment in
   invalidate_region_cache is cleaned up.

   This function operates by simply throwing away everything it knows
   about the modified region.  It doesn't care exactly which
   insertions and deletions took place; it just tosses it all.

   For example, if you insert a single character at the beginning of
   the buffer, and a single character at the end of the buffer (for
   example), without calling this function in between the two
   insertions, then the entire cache will be freed of useful
   information.  On the other hand, if you do manage to call this
   function in between the two insertions, then the modified regions
   will be small in both cases, no information will be tossed, and the
   cache will know that it doesn't have knowledge of the first and
   last characters any more.

   Calling this function may be expensive; it does binary searches in
   the cache, and causes cache gap motion.  */

static void
revalidate_region_cache (struct buffer *buf, struct region_cache *c)
{
  /* The boundaries now in the cache are expressed relative to the
     buffer_beg and buffer_end values stored in the cache.  Now,
     buffer_beg and buffer_end may not be the same as BUF_BEG (buf)
     and BUF_Z (buf), so we have two different "bases" to deal with
     --- the cache's, and the buffer's.  */

  /* If the entire buffer is still valid, don't waste time.  Yes, this
     should be a >, not a >=; think about what beg_unchanged and
     end_unchanged get set to when the only change has been an
     insertion.  */
  if (c->buffer_beg + c->beg_unchanged
      > c->buffer_end - c->end_unchanged)
    return;

  /* If all the text we knew about as of the last cache revalidation
     is still there, then all of the information in the cache is still
     valid.  Because c->buffer_beg and c->buffer_end are out-of-date,
     the modified region appears from the cache's point of view to be
     a null region located someplace in the buffer.

     Now, invalidating that empty string will have no actual affect on
     the cache; instead, we need to update the cache's basis first
     (which will give the modified region the same size in the cache
     as it has in the buffer), and then invalidate the modified
     region. */
  if (c->buffer_beg + c->beg_unchanged
      == c->buffer_end - c->end_unchanged)
    {
      /* Move the gap so that all the boundaries in the unchanged head
         are expressed beg-relative, and all the boundaries in the
         unchanged tail are expressed end-relative.  That done, we can
         plug in the new buffer beg and end, and all the positions
         will be accurate.

         The boundary which has jurisdiction over the modified region
         should be left before the gap.  */
      move_cache_gap (c,
                      (find_cache_boundary (c, (c->buffer_beg
                                                + c->beg_unchanged))
                       + 1),
                      0);

      c->buffer_beg = BUF_BEG (buf);
      c->buffer_end = BUF_Z   (buf);

      /* Now that the cache's basis has been changed, the modified
         region actually takes up some space in the cache, so we can
         invalidate it.  */
      set_cache_region (c,
                        c->buffer_beg + c->beg_unchanged,
                        c->buffer_end - c->end_unchanged,
                        0);
    }

  /* Otherwise, there is a non-empty region in the cache which
     corresponds to the modified region of the buffer.  */
  else
    {
      ptrdiff_t modified_ix;

      /* These positions are correct, relative to both the cache basis
         and the buffer basis.  */
      set_cache_region (c,
                        c->buffer_beg + c->beg_unchanged,
                        c->buffer_end - c->end_unchanged,
                        0);

      /* Now the cache contains only boundaries that are in the
         unchanged head and tail; we've disposed of any boundaries
         whose positions we can't be sure of given the information
         we've saved.

         If we put the cache gap between the unchanged head and the
         unchanged tail, we can adjust all the boundary positions at
         once, simply by setting buffer_beg and buffer_end.

         The boundary which has jurisdiction over the modified region
         should be left before the gap.  */
      modified_ix =
        find_cache_boundary (c, (c->buffer_beg + c->beg_unchanged)) + 1;
      move_cache_gap (c, modified_ix, 0);

      c->buffer_beg = BUF_BEG (buf);
      c->buffer_end = BUF_Z   (buf);

      /* Now, we may have shrunk the buffer when we changed the basis,
         and brought the boundaries we created for the start and end
         of the modified region together, giving them the same
         position.  If that's the case, we should collapse them into
         one boundary.  Or we may even delete them both, if the values
         before and after them are the same.  */
      if (modified_ix < c->cache_len
          && (BOUNDARY_POS (c, modified_ix - 1)
              == BOUNDARY_POS (c, modified_ix)))
        {
          int value_after = BOUNDARY_VALUE (c, modified_ix);

          /* Should we remove both of the boundaries?  Yes, if the
             latter boundary is now establishing the same value that
             the former boundary's predecessor does.  */
          if (modified_ix - 1 > 0
              && value_after == BOUNDARY_VALUE (c, modified_ix - 2))
            delete_cache_boundaries (c, modified_ix - 1, modified_ix + 1);
          else
            {
              /* We do need a boundary here; collapse the two
                 boundaries into one.  */
              SET_BOUNDARY_VALUE (c, modified_ix - 1, value_after);
              delete_cache_boundaries (c, modified_ix, modified_ix + 1);
            }
        }
    }

  /* Now the entire cache is valid.  */
  c->beg_unchanged
    = c->end_unchanged
      = c->buffer_end - c->buffer_beg;
}


/* Interface: Adding information to the cache.  */

/* Assert that the region of BUF between START and END (absolute
   buffer positions) is "known," for the purposes of CACHE (e.g. "has
   no newlines", in the case of the line cache).  */
void
know_region_cache (struct buffer *buf, struct region_cache *c,
		   ptrdiff_t start, ptrdiff_t end)
{
  revalidate_region_cache (buf, c);

  set_cache_region (c, start, end, 1);
}


/* Interface: using the cache.  */

/* Return true if the text immediately after POS in BUF is known, for
   the purposes of CACHE.  If NEXT is non-zero, set *NEXT to the nearest
   position after POS where the knowledge changes.  */
int
region_cache_forward (struct buffer *buf, struct region_cache *c,
		      ptrdiff_t pos, ptrdiff_t *next)
{
  revalidate_region_cache (buf, c);

  {
    ptrdiff_t i = find_cache_boundary (c, pos);
    int i_value = BOUNDARY_VALUE (c, i);
    ptrdiff_t j;

    /* Beyond the end of the buffer is unknown, by definition.  */
    if (pos >= BUF_Z (buf))
      {
        if (next) *next = BUF_Z (buf);
        i_value = 0;
      }
    else if (next)
      {
        /* Scan forward from i to find the next differing position.  */
        for (j = i + 1; j < c->cache_len; j++)
          if (BOUNDARY_VALUE (c, j) != i_value)
            break;

        if (j < c->cache_len)
          *next = BOUNDARY_POS (c, j);
        else
          *next = BUF_Z (buf);
      }

    return i_value;
  }
}

/* Return true if the text immediately before POS in BUF is known, for
   the purposes of CACHE.  If NEXT is non-zero, set *NEXT to the nearest
   position before POS where the knowledge changes.  */
int region_cache_backward (struct buffer *buf, struct region_cache *c,
			   ptrdiff_t pos, ptrdiff_t *next)
{
  revalidate_region_cache (buf, c);

  /* Before the beginning of the buffer is unknown, by
     definition. */
  if (pos <= BUF_BEG (buf))
    {
      if (next) *next = BUF_BEG (buf);
      return 0;
    }

  {
    ptrdiff_t i = find_cache_boundary (c, pos - 1);
    int i_value = BOUNDARY_VALUE (c, i);
    ptrdiff_t j;

    if (next)
      {
        /* Scan backward from i to find the next differing position.  */
        for (j = i - 1; j >= 0; j--)
          if (BOUNDARY_VALUE (c, j) != i_value)
            break;

        if (j >= 0)
          *next = BOUNDARY_POS (c, j + 1);
        else
          *next = BUF_BEG (buf);
      }

    return i_value;
  }
}


/* Debugging: pretty-print a cache to the standard error output.  */

void pp_cache (struct region_cache *) EXTERNALLY_VISIBLE;
void
pp_cache (struct region_cache *c)
{
  ptrdiff_t i;
  ptrdiff_t beg_u = c->buffer_beg + c->beg_unchanged;
  ptrdiff_t end_u = c->buffer_end - c->end_unchanged;

  fprintf (stderr,
           "basis: %"pD"d..%"pD"d    modified: %"pD"d..%"pD"d\n",
           c->buffer_beg, c->buffer_end,
           beg_u, end_u);

  for (i = 0; i < c->cache_len; i++)
    {
      ptrdiff_t pos = BOUNDARY_POS (c, i);

      putc (((pos < beg_u) ? 'v'
             : (pos == beg_u) ? '-'
             : ' '),
            stderr);
      putc (((pos > end_u) ? '^'
             : (pos == end_u) ? '-'
             : ' '),
            stderr);
      fprintf (stderr, "%"pD"d : %d\n", pos, BOUNDARY_VALUE (c, i));
    }
}
