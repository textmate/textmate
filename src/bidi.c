/* Low-level bidirectional buffer/string-scanning functions for GNU Emacs.
   Copyright (C) 2000-2001, 2004-2005, 2009-2012
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

/* Written by Eli Zaretskii <eliz@gnu.org>.

   A sequential implementation of the Unicode Bidirectional algorithm,
   (UBA) as per UAX#9, a part of the Unicode Standard.

   Unlike the reference and most other implementations, this one is
   designed to be called once for every character in the buffer or
   string.

   The main entry point is bidi_move_to_visually_next.  Each time it
   is called, it finds the next character in the visual order, and
   returns its information in a special structure.  The caller is then
   expected to process this character for display or any other
   purposes, and call bidi_move_to_visually_next for the next
   character.  See the comments in bidi_move_to_visually_next for more
   details about its algorithm that finds the next visual-order
   character by resolving their levels on the fly.

   Two other entry points are bidi_paragraph_init and
   bidi_mirror_char.  The first determines the base direction of a
   paragraph, while the second returns the mirrored version of its
   argument character.

   A few auxiliary entry points are used to initialize the bidi
   iterator for iterating an object (buffer or string), push and pop
   the bidi iterator state, and save and restore the state of the bidi
   cache.

   If you want to understand the code, you will have to read it
   together with the relevant portions of UAX#9.  The comments include
   references to UAX#9 rules, for that very reason.

   A note about references to UAX#9 rules: if the reference says
   something like "X9/Retaining", it means that you need to refer to
   rule X9 and to its modifications described in the "Implementation
   Notes" section of UAX#9, under "Retaining Format Codes".  */

#include <config.h>
#include <stdio.h>
#include <setjmp.h>

#include "lisp.h"
#include "buffer.h"
#include "character.h"
#include "dispextern.h"

static int bidi_initialized = 0;

static Lisp_Object bidi_type_table, bidi_mirror_table;

#define LRM_CHAR   0x200E
#define RLM_CHAR   0x200F
#define BIDI_EOB   -1

/* Data type for describing the bidirectional character categories.  */
typedef enum {
  UNKNOWN_BC,
  NEUTRAL,
  WEAK,
  STRONG
} bidi_category_t;

/* UAX#9 says to search only for L, AL, or R types of characters, and
   ignore RLE, RLO, LRE, and LRO, when determining the base paragraph
   level.  Yudit indeed ignores them.  This variable is therefore set
   by default to ignore them, but setting it to zero will take them
   into account.  */
extern int bidi_ignore_explicit_marks_for_paragraph_level EXTERNALLY_VISIBLE;
int bidi_ignore_explicit_marks_for_paragraph_level = 1;

static Lisp_Object paragraph_start_re, paragraph_separate_re;
static Lisp_Object Qparagraph_start, Qparagraph_separate;


/***********************************************************************
			Utilities
 ***********************************************************************/

/* Return the bidi type of a character CH, subject to the current
   directional OVERRIDE.  */
static inline bidi_type_t
bidi_get_type (int ch, bidi_dir_t override)
{
  bidi_type_t default_type;

  if (ch == BIDI_EOB)
    return NEUTRAL_B;
  if (ch < 0 || ch > MAX_CHAR)
    abort ();

  default_type = (bidi_type_t) XINT (CHAR_TABLE_REF (bidi_type_table, ch));
  /* Every valid character code, even those that are unassigned by the
     UCD, have some bidi-class property, according to
     DerivedBidiClass.txt file.  Therefore, if we ever get UNKNOWN_BT
     (= zero) code from CHAR_TABLE_REF, that's a bug.  */
  if (default_type == UNKNOWN_BT)
    abort ();

  if (override == NEUTRAL_DIR)
    return default_type;

  switch (default_type)
    {
      /* Although UAX#9 does not tell, it doesn't make sense to
	 override NEUTRAL_B and LRM/RLM characters.  */
      case NEUTRAL_B:
      case LRE:
      case LRO:
      case RLE:
      case RLO:
      case PDF:
	return default_type;
      default:
	switch (ch)
	  {
	    case LRM_CHAR:
	    case RLM_CHAR:
	      return default_type;
	    default:
	      if (override == L2R) /* X6 */
		return STRONG_L;
	      else if (override == R2L)
		return STRONG_R;
	      else
		abort ();	/* can't happen: handled above */
	  }
    }
}

static inline void
bidi_check_type (bidi_type_t type)
{
  xassert (UNKNOWN_BT <= type && type <= NEUTRAL_ON);
}

/* Given a bidi TYPE of a character, return its category.  */
static inline bidi_category_t
bidi_get_category (bidi_type_t type)
{
  switch (type)
    {
      case UNKNOWN_BT:
	return UNKNOWN_BC;
      case STRONG_L:
      case STRONG_R:
      case STRONG_AL:
      case LRE:
      case LRO:
      case RLE:
      case RLO:
	return STRONG;
      case PDF:		/* ??? really?? */
      case WEAK_EN:
      case WEAK_ES:
      case WEAK_ET:
      case WEAK_AN:
      case WEAK_CS:
      case WEAK_NSM:
      case WEAK_BN:
	return WEAK;
      case NEUTRAL_B:
      case NEUTRAL_S:
      case NEUTRAL_WS:
      case NEUTRAL_ON:
	return NEUTRAL;
      default:
	abort ();
    }
}

/* Return the mirrored character of C, if it has one.  If C has no
   mirrored counterpart, return C.
   Note: The conditions in UAX#9 clause L4 regarding the surrounding
   context must be tested by the caller.  */
int
bidi_mirror_char (int c)
{
  Lisp_Object val;

  if (c == BIDI_EOB)
    return c;
  if (c < 0 || c > MAX_CHAR)
    abort ();

  val = CHAR_TABLE_REF (bidi_mirror_table, c);
  if (INTEGERP (val))
    {
      int v = XINT (val);

      if (v < 0 || v > MAX_CHAR)
	abort ();

      return v;
    }

  return c;
}

/* Determine the start-of-run (sor) directional type given the two
   embedding levels on either side of the run boundary.  Also, update
   the saved info about previously seen characters, since that info is
   generally valid for a single level run.  */
static inline void
bidi_set_sor_type (struct bidi_it *bidi_it, int level_before, int level_after)
{
  int higher_level = (level_before > level_after ? level_before : level_after);

  /* The prev_was_pdf gork is required for when we have several PDFs
     in a row.  In that case, we want to compute the sor type for the
     next level run only once: when we see the first PDF.  That's
     because the sor type depends only on the higher of the two levels
     that we find on the two sides of the level boundary (see UAX#9,
     clause X10), and so we don't need to know the final embedding
     level to which we descend after processing all the PDFs.  */
  if (!bidi_it->prev_was_pdf || level_before < level_after)
    /* FIXME: should the default sor direction be user selectable?  */
    bidi_it->sor = ((higher_level & 1) != 0 ? R2L : L2R);
  if (level_before > level_after)
    bidi_it->prev_was_pdf = 1;

  bidi_it->prev.type = UNKNOWN_BT;
  bidi_it->last_strong.type = bidi_it->last_strong.type_after_w1
    = bidi_it->last_strong.orig_type = UNKNOWN_BT;
  bidi_it->prev_for_neutral.type = (bidi_it->sor == R2L ? STRONG_R : STRONG_L);
  bidi_it->prev_for_neutral.charpos = bidi_it->charpos;
  bidi_it->prev_for_neutral.bytepos = bidi_it->bytepos;
  bidi_it->next_for_neutral.type = bidi_it->next_for_neutral.type_after_w1
    = bidi_it->next_for_neutral.orig_type = UNKNOWN_BT;
  bidi_it->ignore_bn_limit = -1; /* meaning it's unknown */
}

/* Push the current embedding level and override status; reset the
   current level to LEVEL and the current override status to OVERRIDE.  */
static inline void
bidi_push_embedding_level (struct bidi_it *bidi_it,
			   int level, bidi_dir_t override)
{
  bidi_it->stack_idx++;
  xassert (bidi_it->stack_idx < BIDI_MAXLEVEL);
  bidi_it->level_stack[bidi_it->stack_idx].level = level;
  bidi_it->level_stack[bidi_it->stack_idx].override = override;
}

/* Pop the embedding level and directional override status from the
   stack, and return the new level.  */
static inline int
bidi_pop_embedding_level (struct bidi_it *bidi_it)
{
  /* UAX#9 says to ignore invalid PDFs.  */
  if (bidi_it->stack_idx > 0)
    bidi_it->stack_idx--;
  return bidi_it->level_stack[bidi_it->stack_idx].level;
}

/* Record in SAVED_INFO the information about the current character.  */
static inline void
bidi_remember_char (struct bidi_saved_info *saved_info,
		    struct bidi_it *bidi_it)
{
  saved_info->charpos = bidi_it->charpos;
  saved_info->bytepos = bidi_it->bytepos;
  saved_info->type = bidi_it->type;
  bidi_check_type (bidi_it->type);
  saved_info->type_after_w1 = bidi_it->type_after_w1;
  bidi_check_type (bidi_it->type_after_w1);
  saved_info->orig_type = bidi_it->orig_type;
  bidi_check_type (bidi_it->orig_type);
}

/* Copy the bidi iterator from FROM to TO.  To save cycles, this only
   copies the part of the level stack that is actually in use.  */
static inline void
bidi_copy_it (struct bidi_it *to, struct bidi_it *from)
{
  int i;

  /* Copy everything except the level stack and beyond.  */
  memcpy (to, from, offsetof (struct bidi_it, level_stack[0]));

  /* Copy the active part of the level stack.  */
  to->level_stack[0] = from->level_stack[0]; /* level zero is always in use */
  for (i = 1; i <= from->stack_idx; i++)
    to->level_stack[i] = from->level_stack[i];
}


/***********************************************************************
			Caching the bidi iterator states
 ***********************************************************************/

#define BIDI_CACHE_CHUNK 200
static struct bidi_it *bidi_cache;
static ptrdiff_t bidi_cache_size = 0;
enum { elsz = sizeof (struct bidi_it) };
static ptrdiff_t bidi_cache_idx;	/* next unused cache slot */
static ptrdiff_t bidi_cache_last_idx;	/* slot of last cache hit */
static ptrdiff_t bidi_cache_start = 0;	/* start of cache for this
					   "stack" level */

/* 5-slot stack for saving the start of the previous level of the
   cache.  xdisp.c maintains a 5-slot stack for its iterator state,
   and we need the same size of our stack.  */
static ptrdiff_t bidi_cache_start_stack[IT_STACK_SIZE];
static int bidi_cache_sp;

/* Size of header used by bidi_shelve_cache.  */
enum
  {
    bidi_shelve_header_size
      = (sizeof (bidi_cache_idx) + sizeof (bidi_cache_start_stack)
	 + sizeof (bidi_cache_sp) + sizeof (bidi_cache_start)
	 + sizeof (bidi_cache_last_idx))
  };

/* Reset the cache state to the empty state.  We only reset the part
   of the cache relevant to iteration of the current object.  Previous
   objects, which are pushed on the display iterator's stack, are left
   intact.  This is called when the cached information is no more
   useful for the current iteration, e.g. when we were reseated to a
   new position on the same object.  */
static inline void
bidi_cache_reset (void)
{
  bidi_cache_idx = bidi_cache_start;
  bidi_cache_last_idx = -1;
}

/* Shrink the cache to its minimal size.  Called when we init the bidi
   iterator for reordering a buffer or a string that does not come
   from display properties, because that means all the previously
   cached info is of no further use.  */
static inline void
bidi_cache_shrink (void)
{
  if (bidi_cache_size > BIDI_CACHE_CHUNK)
    {
      bidi_cache
	= (struct bidi_it *) xrealloc (bidi_cache, BIDI_CACHE_CHUNK * elsz);
      bidi_cache_size = BIDI_CACHE_CHUNK;
    }
  bidi_cache_reset ();
}

static inline void
bidi_cache_fetch_state (ptrdiff_t idx, struct bidi_it *bidi_it)
{
  int current_scan_dir = bidi_it->scan_dir;

  if (idx < bidi_cache_start || idx >= bidi_cache_idx)
    abort ();

  bidi_copy_it (bidi_it, &bidi_cache[idx]);
  bidi_it->scan_dir = current_scan_dir;
  bidi_cache_last_idx = idx;
}

/* Find a cached state with a given CHARPOS and resolved embedding
   level less or equal to LEVEL.  if LEVEL is -1, disregard the
   resolved levels in cached states.  DIR, if non-zero, means search
   in that direction from the last cache hit.  */
static inline ptrdiff_t
bidi_cache_search (EMACS_INT charpos, int level, int dir)
{
  ptrdiff_t i, i_start;

  if (bidi_cache_idx > bidi_cache_start)
    {
      if (bidi_cache_last_idx == -1)
	bidi_cache_last_idx = bidi_cache_idx - 1;
      if (charpos < bidi_cache[bidi_cache_last_idx].charpos)
	{
	  dir = -1;
	  i_start = bidi_cache_last_idx - 1;
	}
      else if (charpos > (bidi_cache[bidi_cache_last_idx].charpos
			  + bidi_cache[bidi_cache_last_idx].nchars - 1))
	{
	  dir = 1;
	  i_start = bidi_cache_last_idx + 1;
	}
      else if (dir)
	i_start = bidi_cache_last_idx;
      else
	{
	  dir = -1;
	  i_start = bidi_cache_idx - 1;
	}

      if (dir < 0)
	{
	  /* Linear search for now; FIXME!  */
	  for (i = i_start; i >= bidi_cache_start; i--)
	    if (bidi_cache[i].charpos <= charpos
		&& charpos < bidi_cache[i].charpos + bidi_cache[i].nchars
		&& (level == -1 || bidi_cache[i].resolved_level <= level))
	      return i;
	}
      else
	{
	  for (i = i_start; i < bidi_cache_idx; i++)
	    if (bidi_cache[i].charpos <= charpos
		&& charpos < bidi_cache[i].charpos + bidi_cache[i].nchars
		&& (level == -1 || bidi_cache[i].resolved_level <= level))
	      return i;
	}
    }

  return -1;
}

/* Find a cached state where the resolved level changes to a value
   that is lower than LEVEL, and return its cache slot index.  DIR is
   the direction to search, starting with the last used cache slot.
   If DIR is zero, we search backwards from the last occupied cache
   slot.  BEFORE, if non-zero, means return the index of the slot that
   is ``before'' the level change in the search direction.  That is,
   given the cached levels like this:

	 1122333442211
	  AB        C

   and assuming we are at the position cached at the slot marked with
   C, searching backwards (DIR = -1) for LEVEL = 2 will return the
   index of slot B or A, depending whether BEFORE is, respectively,
   non-zero or zero.  */
static ptrdiff_t
bidi_cache_find_level_change (int level, int dir, int before)
{
  if (bidi_cache_idx)
    {
      ptrdiff_t i = dir ? bidi_cache_last_idx : bidi_cache_idx - 1;
      int incr = before ? 1 : 0;

      xassert (!dir || bidi_cache_last_idx >= 0);

      if (!dir)
	dir = -1;
      else if (!incr)
	i += dir;

      if (dir < 0)
	{
	  while (i >= bidi_cache_start + incr)
	    {
	      if (bidi_cache[i - incr].resolved_level >= 0
		  && bidi_cache[i - incr].resolved_level < level)
		return i;
	      i--;
	    }
	}
      else
	{
	  while (i < bidi_cache_idx - incr)
	    {
	      if (bidi_cache[i + incr].resolved_level >= 0
		  && bidi_cache[i + incr].resolved_level < level)
		return i;
	      i++;
	    }
	}
    }

  return -1;
}

static inline void
bidi_cache_ensure_space (ptrdiff_t idx)
{
  /* Enlarge the cache as needed.  */
  if (idx >= bidi_cache_size)
    {
      /* The bidi cache cannot be larger than the largest Lisp string
	 or buffer.  */
      ptrdiff_t string_or_buffer_bound
	= max (BUF_BYTES_MAX, STRING_BYTES_BOUND);

      /* Also, it cannot be larger than what C can represent.  */
      ptrdiff_t c_bound
	= (min (PTRDIFF_MAX, SIZE_MAX) - bidi_shelve_header_size) / elsz;

      bidi_cache
	= xpalloc (bidi_cache, &bidi_cache_size,
		   max (BIDI_CACHE_CHUNK, idx - bidi_cache_size + 1),
		   min (string_or_buffer_bound, c_bound), elsz);
    }
}

static inline void
bidi_cache_iterator_state (struct bidi_it *bidi_it, int resolved)
{
  ptrdiff_t idx;

  /* We should never cache on backward scans.  */
  if (bidi_it->scan_dir == -1)
    abort ();
  idx = bidi_cache_search (bidi_it->charpos, -1, 1);

  if (idx < 0)
    {
      idx = bidi_cache_idx;
      bidi_cache_ensure_space (idx);
      /* Character positions should correspond to cache positions 1:1.
	 If we are outside the range of cached positions, the cache is
	 useless and must be reset.  */
      if (idx > bidi_cache_start &&
	  (bidi_it->charpos > (bidi_cache[idx - 1].charpos
			       + bidi_cache[idx - 1].nchars)
	   || bidi_it->charpos < bidi_cache[bidi_cache_start].charpos))
	{
	  bidi_cache_reset ();
	  idx = bidi_cache_start;
	}
      if (bidi_it->nchars <= 0)
	abort ();
      bidi_copy_it (&bidi_cache[idx], bidi_it);
      if (!resolved)
	bidi_cache[idx].resolved_level = -1;
    }
  else
    {
      /* Copy only the members which could have changed, to avoid
	 costly copying of the entire struct.  */
      bidi_cache[idx].type = bidi_it->type;
      bidi_check_type (bidi_it->type);
      bidi_cache[idx].type_after_w1 = bidi_it->type_after_w1;
      bidi_check_type (bidi_it->type_after_w1);
      if (resolved)
	bidi_cache[idx].resolved_level = bidi_it->resolved_level;
      else
	bidi_cache[idx].resolved_level = -1;
      bidi_cache[idx].invalid_levels = bidi_it->invalid_levels;
      bidi_cache[idx].invalid_rl_levels = bidi_it->invalid_rl_levels;
      bidi_cache[idx].next_for_neutral = bidi_it->next_for_neutral;
      bidi_cache[idx].next_for_ws = bidi_it->next_for_ws;
      bidi_cache[idx].ignore_bn_limit = bidi_it->ignore_bn_limit;
      bidi_cache[idx].disp_pos = bidi_it->disp_pos;
      bidi_cache[idx].disp_prop = bidi_it->disp_prop;
    }

  bidi_cache_last_idx = idx;
  if (idx >= bidi_cache_idx)
    bidi_cache_idx = idx + 1;
}

static inline bidi_type_t
bidi_cache_find (EMACS_INT charpos, int level, struct bidi_it *bidi_it)
{
  ptrdiff_t i = bidi_cache_search (charpos, level, bidi_it->scan_dir);

  if (i >= bidi_cache_start)
    {
      bidi_dir_t current_scan_dir = bidi_it->scan_dir;

      bidi_copy_it (bidi_it, &bidi_cache[i]);
      bidi_cache_last_idx = i;
      /* Don't let scan direction from the cached state override
	 the current scan direction.  */
      bidi_it->scan_dir = current_scan_dir;
      return bidi_it->type;
    }

  return UNKNOWN_BT;
}

static inline int
bidi_peek_at_next_level (struct bidi_it *bidi_it)
{
  if (bidi_cache_idx == bidi_cache_start || bidi_cache_last_idx == -1)
    abort ();
  return bidi_cache[bidi_cache_last_idx + bidi_it->scan_dir].resolved_level;
}


/***********************************************************************
	     Pushing and popping the bidi iterator state
 ***********************************************************************/

/* Push the bidi iterator state in preparation for reordering a
   different object, e.g. display string found at certain buffer
   position.  Pushing the bidi iterator boils down to saving its
   entire state on the cache and starting a new cache "stacked" on top
   of the current cache.  */
void
bidi_push_it (struct bidi_it *bidi_it)
{
  /* Save the current iterator state in its entirety after the last
     used cache slot.  */
  bidi_cache_ensure_space (bidi_cache_idx);
  memcpy (&bidi_cache[bidi_cache_idx++], bidi_it, sizeof (struct bidi_it));

  /* Push the current cache start onto the stack.  */
  xassert (bidi_cache_sp < IT_STACK_SIZE);
  bidi_cache_start_stack[bidi_cache_sp++] = bidi_cache_start;

  /* Start a new level of cache, and make it empty.  */
  bidi_cache_start = bidi_cache_idx;
  bidi_cache_last_idx = -1;
}

/* Restore the iterator state saved by bidi_push_it and return the
   cache to the corresponding state.  */
void
bidi_pop_it (struct bidi_it *bidi_it)
{
  if (bidi_cache_start <= 0)
    abort ();

  /* Reset the next free cache slot index to what it was before the
     call to bidi_push_it.  */
  bidi_cache_idx = bidi_cache_start - 1;

  /* Restore the bidi iterator state saved in the cache.  */
  memcpy (bidi_it, &bidi_cache[bidi_cache_idx], sizeof (struct bidi_it));

  /* Pop the previous cache start from the stack.  */
  if (bidi_cache_sp <= 0)
    abort ();
  bidi_cache_start = bidi_cache_start_stack[--bidi_cache_sp];

  /* Invalidate the last-used cache slot data.  */
  bidi_cache_last_idx = -1;
}

static ptrdiff_t bidi_cache_total_alloc;

/* Stash away a copy of the cache and its control variables.  */
void *
bidi_shelve_cache (void)
{
  unsigned char *databuf;
  ptrdiff_t alloc;

  /* Empty cache.  */
  if (bidi_cache_idx == 0)
    return NULL;

  alloc = (bidi_shelve_header_size
	   + bidi_cache_idx * sizeof (struct bidi_it));
  databuf = xmalloc (alloc);
  bidi_cache_total_alloc += alloc;

  memcpy (databuf, &bidi_cache_idx, sizeof (bidi_cache_idx));
  memcpy (databuf + sizeof (bidi_cache_idx),
	  bidi_cache, bidi_cache_idx * sizeof (struct bidi_it));
  memcpy (databuf + sizeof (bidi_cache_idx)
	  + bidi_cache_idx * sizeof (struct bidi_it),
	  bidi_cache_start_stack, sizeof (bidi_cache_start_stack));
  memcpy (databuf + sizeof (bidi_cache_idx)
	  + bidi_cache_idx * sizeof (struct bidi_it)
	  + sizeof (bidi_cache_start_stack),
	  &bidi_cache_sp, sizeof (bidi_cache_sp));
  memcpy (databuf + sizeof (bidi_cache_idx)
	  + bidi_cache_idx * sizeof (struct bidi_it)
	  + sizeof (bidi_cache_start_stack) + sizeof (bidi_cache_sp),
	  &bidi_cache_start, sizeof (bidi_cache_start));
  memcpy (databuf + sizeof (bidi_cache_idx)
	  + bidi_cache_idx * sizeof (struct bidi_it)
	  + sizeof (bidi_cache_start_stack) + sizeof (bidi_cache_sp)
	  + sizeof (bidi_cache_start),
	  &bidi_cache_last_idx, sizeof (bidi_cache_last_idx));

  return databuf;
}

/* Restore the cache state from a copy stashed away by
   bidi_shelve_cache, and free the buffer used to stash that copy.
   JUST_FREE non-zero means free the buffer, but don't restore the
   cache; used when the corresponding iterator is discarded instead of
   being restored.  */
void
bidi_unshelve_cache (void *databuf, int just_free)
{
  unsigned char *p = databuf;

  if (!p)
    {
      if (!just_free)
	{
	  /* A NULL pointer means an empty cache.  */
	  bidi_cache_start = 0;
	  bidi_cache_sp = 0;
	  bidi_cache_reset ();
	}
    }
  else
    {
      if (just_free)
	{
	  ptrdiff_t idx;

	  memcpy (&idx, p, sizeof (bidi_cache_idx));
	  bidi_cache_total_alloc
	    -= bidi_shelve_header_size + idx * sizeof (struct bidi_it);
	}
      else
	{
	  memcpy (&bidi_cache_idx, p, sizeof (bidi_cache_idx));
	  bidi_cache_ensure_space (bidi_cache_idx);
	  memcpy (bidi_cache, p + sizeof (bidi_cache_idx),
		  bidi_cache_idx * sizeof (struct bidi_it));
	  memcpy (bidi_cache_start_stack,
		  p + sizeof (bidi_cache_idx)
		  + bidi_cache_idx * sizeof (struct bidi_it),
		  sizeof (bidi_cache_start_stack));
	  memcpy (&bidi_cache_sp,
		  p + sizeof (bidi_cache_idx)
		  + bidi_cache_idx * sizeof (struct bidi_it)
		  + sizeof (bidi_cache_start_stack),
		  sizeof (bidi_cache_sp));
	  memcpy (&bidi_cache_start,
		  p + sizeof (bidi_cache_idx)
		  + bidi_cache_idx * sizeof (struct bidi_it)
		  + sizeof (bidi_cache_start_stack) + sizeof (bidi_cache_sp),
		  sizeof (bidi_cache_start));
	  memcpy (&bidi_cache_last_idx,
		  p + sizeof (bidi_cache_idx)
		  + bidi_cache_idx * sizeof (struct bidi_it)
		  + sizeof (bidi_cache_start_stack) + sizeof (bidi_cache_sp)
		  + sizeof (bidi_cache_start),
		  sizeof (bidi_cache_last_idx));
	  bidi_cache_total_alloc
	    -= (bidi_shelve_header_size
		+ bidi_cache_idx * sizeof (struct bidi_it));
	}

      xfree (p);
    }
}


/***********************************************************************
			Initialization
 ***********************************************************************/
static void
bidi_initialize (void)
{
  bidi_type_table = uniprop_table (intern ("bidi-class"));
  if (NILP (bidi_type_table))
    abort ();
  staticpro (&bidi_type_table);

  bidi_mirror_table = uniprop_table (intern ("mirroring"));
  if (NILP (bidi_mirror_table))
    abort ();
  staticpro (&bidi_mirror_table);

  Qparagraph_start = intern ("paragraph-start");
  staticpro (&Qparagraph_start);
  paragraph_start_re = Fsymbol_value (Qparagraph_start);
  if (!STRINGP (paragraph_start_re))
    paragraph_start_re = build_string ("\f\\|[ \t]*$");
  staticpro (&paragraph_start_re);
  Qparagraph_separate = intern ("paragraph-separate");
  staticpro (&Qparagraph_separate);
  paragraph_separate_re = Fsymbol_value (Qparagraph_separate);
  if (!STRINGP (paragraph_separate_re))
    paragraph_separate_re = build_string ("[ \t\f]*$");
  staticpro (&paragraph_separate_re);

  bidi_cache_sp = 0;
  bidi_cache_total_alloc = 0;

  bidi_initialized = 1;
}

/* Do whatever UAX#9 clause X8 says should be done at paragraph's
   end.  */
static inline void
bidi_set_paragraph_end (struct bidi_it *bidi_it)
{
  bidi_it->invalid_levels = 0;
  bidi_it->invalid_rl_levels = -1;
  bidi_it->stack_idx = 0;
  bidi_it->resolved_level = bidi_it->level_stack[0].level;
}

/* Initialize the bidi iterator from buffer/string position CHARPOS.  */
void
bidi_init_it (EMACS_INT charpos, EMACS_INT bytepos, int frame_window_p,
	      struct bidi_it *bidi_it)
{
  if (! bidi_initialized)
    bidi_initialize ();
  if (charpos >= 0)
    bidi_it->charpos = charpos;
  if (bytepos >= 0)
    bidi_it->bytepos = bytepos;
  bidi_it->frame_window_p = frame_window_p;
  bidi_it->nchars = -1;	/* to be computed in bidi_resolve_explicit_1 */
  bidi_it->first_elt = 1;
  bidi_set_paragraph_end (bidi_it);
  bidi_it->new_paragraph = 1;
  bidi_it->separator_limit = -1;
  bidi_it->type = NEUTRAL_B;
  bidi_it->type_after_w1 = NEUTRAL_B;
  bidi_it->orig_type = NEUTRAL_B;
  bidi_it->prev_was_pdf = 0;
  bidi_it->prev.type = bidi_it->prev.type_after_w1
    = bidi_it->prev.orig_type = UNKNOWN_BT;
  bidi_it->last_strong.type = bidi_it->last_strong.type_after_w1
    = bidi_it->last_strong.orig_type = UNKNOWN_BT;
  bidi_it->next_for_neutral.charpos = -1;
  bidi_it->next_for_neutral.type
    = bidi_it->next_for_neutral.type_after_w1
    = bidi_it->next_for_neutral.orig_type = UNKNOWN_BT;
  bidi_it->prev_for_neutral.charpos = -1;
  bidi_it->prev_for_neutral.type
    = bidi_it->prev_for_neutral.type_after_w1
    = bidi_it->prev_for_neutral.orig_type = UNKNOWN_BT;
  bidi_it->sor = L2R;	 /* FIXME: should it be user-selectable? */
  bidi_it->disp_pos = -1;	/* invalid/unknown */
  bidi_it->disp_prop = 0;
  /* We can only shrink the cache if we are at the bottom level of its
     "stack".  */
  if (bidi_cache_start == 0)
    bidi_cache_shrink ();
  else
    bidi_cache_reset ();
}

/* Perform initializations for reordering a new line of bidi text.  */
static void
bidi_line_init (struct bidi_it *bidi_it)
{
  bidi_it->scan_dir = 1; /* FIXME: do we need to have control on this? */
  bidi_it->resolved_level = bidi_it->level_stack[0].level;
  bidi_it->level_stack[0].override = NEUTRAL_DIR; /* X1 */
  bidi_it->invalid_levels = 0;
  bidi_it->invalid_rl_levels = -1;
  /* Setting this to zero will force its recomputation the first time
     we need it for W5.  */
  bidi_it->next_en_pos = 0;
  bidi_it->next_en_type = UNKNOWN_BT;
  bidi_it->next_for_ws.type = UNKNOWN_BT;
  bidi_set_sor_type (bidi_it,
		     (bidi_it->paragraph_dir == R2L ? 1 : 0),
		     bidi_it->level_stack[0].level); /* X10 */

  bidi_cache_reset ();
}


/***********************************************************************
			Fetching characters
 ***********************************************************************/

/* Count bytes in string S between BEG/BEGBYTE and END.  BEG and END
   are zero-based character positions in S, BEGBYTE is byte position
   corresponding to BEG.  UNIBYTE, if non-zero, means S is a unibyte
   string.  */
static inline EMACS_INT
bidi_count_bytes (const unsigned char *s, const EMACS_INT beg,
		  const EMACS_INT begbyte, const EMACS_INT end, int unibyte)
{
  EMACS_INT pos = beg;
  const unsigned char *p = s + begbyte, *start = p;

  if (unibyte)
    p = s + end;
  else
    {
      if (!CHAR_HEAD_P (*p))
	abort ();

      while (pos < end)
	{
	  p += BYTES_BY_CHAR_HEAD (*p);
	  pos++;
	}
    }

  return p - start;
}

/* Fetch and returns the character at byte position BYTEPOS.  If S is
   non-NULL, fetch the character from string S; otherwise fetch the
   character from the current buffer.  UNIBYTE non-zero means S is a
   unibyte string.  */
static inline int
bidi_char_at_pos (EMACS_INT bytepos, const unsigned char *s, int unibyte)
{
  if (s)
    {
      if (unibyte)
	return s[bytepos];
      else
	return STRING_CHAR (s + bytepos);
    }
  else
    return FETCH_MULTIBYTE_CHAR (bytepos);
}

/* Fetch and return the character at BYTEPOS/CHARPOS.  If that
   character is covered by a display string, treat the entire run of
   covered characters as a single character, either u+2029 or u+FFFC,
   and return their combined length in CH_LEN and NCHARS.  DISP_POS
   specifies the character position of the next display string, or -1
   if not yet computed.  When the next character is at or beyond that
   position, the function updates DISP_POS with the position of the
   next display string.  DISP_PROP non-zero means that there's really
   a display string at DISP_POS, as opposed to when we searched till
   DISP_POS without finding one.  If DISP_PROP is 2, it means the
   display spec is of the form `(space ...)', which is replaced with
   u+2029 to handle it as a paragraph separator.  STRING->s is the C
   string to iterate, or NULL if iterating over a buffer or a Lisp
   string; in the latter case, STRING->lstring is the Lisp string.  */
static inline int
bidi_fetch_char (EMACS_INT bytepos, EMACS_INT charpos, EMACS_INT *disp_pos,
		 int *disp_prop, struct bidi_string_data *string,
		 int frame_window_p, EMACS_INT *ch_len, EMACS_INT *nchars)
{
  int ch;
  EMACS_INT endpos
    = (string->s || STRINGP (string->lstring)) ? string->schars : ZV;
  struct text_pos pos;
  int len;

  /* If we got past the last known position of display string, compute
     the position of the next one.  That position could be at CHARPOS.  */
  if (charpos < endpos && charpos > *disp_pos)
    {
      SET_TEXT_POS (pos, charpos, bytepos);
      *disp_pos = compute_display_string_pos (&pos, string, frame_window_p,
					      disp_prop);
    }

  /* Fetch the character at BYTEPOS.  */
  if (charpos >= endpos)
    {
      ch = BIDI_EOB;
      *ch_len = 1;
      *nchars = 1;
      *disp_pos = endpos;
      *disp_prop = 0;
    }
  else if (charpos >= *disp_pos && *disp_prop)
    {
      EMACS_INT disp_end_pos;

      /* We don't expect to find ourselves in the middle of a display
	 property.  Hopefully, it will never be needed.  */
      if (charpos > *disp_pos)
	abort ();
      /* Text covered by `display' properties and overlays with
	 display properties or display strings is handled as a single
	 character that represents the entire run of characters
	 covered by the display property.  */
      if (*disp_prop == 2)
	{
	  /* `(space ...)' display specs are handled as paragraph
	     separators for the purposes of the reordering; see UAX#9
	     section 3 and clause HL1 in section 4.3 there.  */
	  ch = 0x2029;
	}
      else
	{
	  /* All other display specs are handled as the Unicode Object
	     Replacement Character.  */
	  ch = 0xFFFC;
	}
      disp_end_pos = compute_display_string_end (*disp_pos, string);
      if (disp_end_pos < 0)
	{
	  /* Somebody removed the display string from the buffer
	     behind our back.  Recover by processing this buffer
	     position as if no display property were present there to
	     begin with.  */
	  *disp_prop = 0;
	  goto normal_char;
	}
      *nchars = disp_end_pos - *disp_pos;
      if (*nchars <= 0)
	abort ();
      if (string->s)
	*ch_len = bidi_count_bytes (string->s, *disp_pos, bytepos,
				    disp_end_pos, string->unibyte);
      else if (STRINGP (string->lstring))
	*ch_len = bidi_count_bytes (SDATA (string->lstring), *disp_pos,
				    bytepos, disp_end_pos, string->unibyte);
      else
	*ch_len = CHAR_TO_BYTE (disp_end_pos) - bytepos;
    }
  else
    {
    normal_char:
      if (string->s)
	{

	  if (!string->unibyte)
	    {
	      ch = STRING_CHAR_AND_LENGTH (string->s + bytepos, len);
	      *ch_len = len;
	    }
	  else
	    {
	      ch = UNIBYTE_TO_CHAR (string->s[bytepos]);
	      *ch_len = 1;
	    }
	}
      else if (STRINGP (string->lstring))
	{
	  if (!string->unibyte)
	    {
	      ch = STRING_CHAR_AND_LENGTH (SDATA (string->lstring) + bytepos,
					   len);
	      *ch_len = len;
	    }
	  else
	    {
	      ch = UNIBYTE_TO_CHAR (SREF (string->lstring, bytepos));
	      *ch_len = 1;
	    }
	}
      else
	{
	  ch = STRING_CHAR_AND_LENGTH (BYTE_POS_ADDR (bytepos), len);
	  *ch_len = len;
	}
      *nchars = 1;
    }

  /* If we just entered a run of characters covered by a display
     string, compute the position of the next display string.  */
  if (charpos + *nchars <= endpos && charpos + *nchars > *disp_pos
      && *disp_prop)
    {
      SET_TEXT_POS (pos, charpos + *nchars, bytepos + *ch_len);
      *disp_pos = compute_display_string_pos (&pos, string, frame_window_p,
					      disp_prop);
    }

  return ch;
}


/***********************************************************************
			Determining paragraph direction
 ***********************************************************************/

/* Check if buffer position CHARPOS/BYTEPOS is the end of a paragraph.
   Value is the non-negative length of the paragraph separator
   following the buffer position, -1 if position is at the beginning
   of a new paragraph, or -2 if position is neither at beginning nor
   at end of a paragraph.  */
static EMACS_INT
bidi_at_paragraph_end (EMACS_INT charpos, EMACS_INT bytepos)
{
  Lisp_Object sep_re;
  Lisp_Object start_re;
  EMACS_INT val;

  sep_re = paragraph_separate_re;
  start_re = paragraph_start_re;

  val = fast_looking_at (sep_re, charpos, bytepos, ZV, ZV_BYTE, Qnil);
  if (val < 0)
    {
      if (fast_looking_at (start_re, charpos, bytepos, ZV, ZV_BYTE, Qnil) >= 0)
	val = -1;
      else
	val = -2;
    }

  return val;
}

/* On my 2005-vintage machine, searching back for paragraph start
   takes ~1 ms per line.  And bidi_paragraph_init is called 4 times
   when user types C-p.  The number below limits each call to
   bidi_paragraph_init to about 10 ms.  */
#define MAX_PARAGRAPH_SEARCH 7500

/* Find the beginning of this paragraph by looking back in the buffer.
   Value is the byte position of the paragraph's beginning, or
   BEGV_BYTE if paragraph_start_re is still not found after looking
   back MAX_PARAGRAPH_SEARCH lines in the buffer.  */
static EMACS_INT
bidi_find_paragraph_start (EMACS_INT pos, EMACS_INT pos_byte)
{
  Lisp_Object re = paragraph_start_re;
  EMACS_INT limit = ZV, limit_byte = ZV_BYTE;
  EMACS_INT n = 0;

  while (pos_byte > BEGV_BYTE
	 && n++ < MAX_PARAGRAPH_SEARCH
	 && fast_looking_at (re, pos, pos_byte, limit, limit_byte, Qnil) < 0)
    {
      /* FIXME: What if the paragraph beginning is covered by a
	 display string?  And what if a display string covering some
	 of the text over which we scan back includes
	 paragraph_start_re?  */
      pos = find_next_newline_no_quit (pos - 1, -1);
      pos_byte = CHAR_TO_BYTE (pos);
    }
  if (n >= MAX_PARAGRAPH_SEARCH)
    pos_byte = BEGV_BYTE;
  return pos_byte;
}

/* Determine the base direction, a.k.a. base embedding level, of the
   paragraph we are about to iterate through.  If DIR is either L2R or
   R2L, just use that.  Otherwise, determine the paragraph direction
   from the first strong directional character of the paragraph.

   NO_DEFAULT_P non-zero means don't default to L2R if the paragraph
   has no strong directional characters and both DIR and
   bidi_it->paragraph_dir are NEUTRAL_DIR.  In that case, search back
   in the buffer until a paragraph is found with a strong character,
   or until hitting BEGV.  In the latter case, fall back to L2R.  This
   flag is used in current-bidi-paragraph-direction.

   Note that this function gives the paragraph separator the same
   direction as the preceding paragraph, even though Emacs generally
   views the separator as not belonging to any paragraph.  */
void
bidi_paragraph_init (bidi_dir_t dir, struct bidi_it *bidi_it, int no_default_p)
{
  EMACS_INT bytepos = bidi_it->bytepos;
  int string_p = bidi_it->string.s != NULL || STRINGP (bidi_it->string.lstring);
  EMACS_INT pstartbyte;
  /* Note that begbyte is a byte position, while end is a character
     position.  Yes, this is ugly, but we are trying to avoid costly
     calls to BYTE_TO_CHAR and its ilk.  */
  EMACS_INT begbyte = string_p ? 0 : BEGV_BYTE;
  EMACS_INT end = string_p ? bidi_it->string.schars : ZV;

  /* Special case for an empty buffer. */
  if (bytepos == begbyte && bidi_it->charpos == end)
    dir = L2R;
  /* We should never be called at EOB or before BEGV.  */
  else if (bidi_it->charpos >= end || bytepos < begbyte)
    abort ();

  if (dir == L2R)
    {
      bidi_it->paragraph_dir = L2R;
      bidi_it->new_paragraph = 0;
    }
  else if (dir == R2L)
    {
      bidi_it->paragraph_dir = R2L;
      bidi_it->new_paragraph = 0;
    }
  else if (dir == NEUTRAL_DIR)	/* P2 */
    {
      int ch;
      EMACS_INT ch_len, nchars;
      EMACS_INT pos, disp_pos = -1;
      int disp_prop = 0;
      bidi_type_t type;
      const unsigned char *s;

      if (!bidi_initialized)
	bidi_initialize ();

      /* If we are inside a paragraph separator, we are just waiting
	 for the separator to be exhausted; use the previous paragraph
	 direction.  But don't do that if we have been just reseated,
	 because we need to reinitialize below in that case.  */
      if (!bidi_it->first_elt
	  && bidi_it->charpos < bidi_it->separator_limit)
	return;

      /* If we are on a newline, get past it to where the next
	 paragraph might start.  But don't do that at BEGV since then
	 we are potentially in a new paragraph that doesn't yet
	 exist.  */
      pos = bidi_it->charpos;
      s = (STRINGP (bidi_it->string.lstring)
	   ? SDATA (bidi_it->string.lstring)
	   : bidi_it->string.s);
      if (bytepos > begbyte
	  && bidi_char_at_pos (bytepos, s, bidi_it->string.unibyte) == '\n')
	{
	  bytepos++;
	  pos++;
	}

      /* We are either at the beginning of a paragraph or in the
	 middle of it.  Find where this paragraph starts.  */
      if (string_p)
	{
	  /* We don't support changes of paragraph direction inside a
	     string.  It is treated as a single paragraph.  */
	  pstartbyte = 0;
	}
      else
	pstartbyte = bidi_find_paragraph_start (pos, bytepos);
      bidi_it->separator_limit = -1;
      bidi_it->new_paragraph = 0;

      /* The following loop is run more than once only if NO_DEFAULT_P
	 is non-zero, and only if we are iterating on a buffer.  */
      do {
	bytepos = pstartbyte;
	if (!string_p)
	  pos = BYTE_TO_CHAR (bytepos);
	ch = bidi_fetch_char (bytepos, pos, &disp_pos, &disp_prop,
			      &bidi_it->string,
			      bidi_it->frame_window_p, &ch_len, &nchars);
	type = bidi_get_type (ch, NEUTRAL_DIR);

	for (pos += nchars, bytepos += ch_len;
	     (bidi_get_category (type) != STRONG)
	       || (bidi_ignore_explicit_marks_for_paragraph_level
		   && (type == RLE || type == RLO
		       || type == LRE || type == LRO));
	     type = bidi_get_type (ch, NEUTRAL_DIR))
	  {
	    if (pos >= end)
	      {
		/* Pretend there's a paragraph separator at end of
		   buffer/string.  */
		type = NEUTRAL_B;
		break;
	      }
	    if (!string_p
		&& type == NEUTRAL_B
		&& bidi_at_paragraph_end (pos, bytepos) >= -1)
	      break;
	    /* Fetch next character and advance to get past it.  */
	    ch = bidi_fetch_char (bytepos, pos, &disp_pos,
				  &disp_prop, &bidi_it->string,
				  bidi_it->frame_window_p, &ch_len, &nchars);
	    pos += nchars;
	    bytepos += ch_len;
	  }
	if ((type == STRONG_R || type == STRONG_AL) /* P3 */
	    || (!bidi_ignore_explicit_marks_for_paragraph_level
		&& (type == RLO || type == RLE)))
	  bidi_it->paragraph_dir = R2L;
	else if (type == STRONG_L
		 || (!bidi_ignore_explicit_marks_for_paragraph_level
		     && (type == LRO || type == LRE)))
	  bidi_it->paragraph_dir = L2R;
	if (!string_p
	    && no_default_p && bidi_it->paragraph_dir == NEUTRAL_DIR)
	  {
	    /* If this paragraph is at BEGV, default to L2R.  */
	    if (pstartbyte == BEGV_BYTE)
	      bidi_it->paragraph_dir = L2R; /* P3 and HL1 */
	    else
	      {
		EMACS_INT prevpbyte = pstartbyte;
		EMACS_INT p = BYTE_TO_CHAR (pstartbyte), pbyte = pstartbyte;

		/* Find the beginning of the previous paragraph, if any.  */
		while (pbyte > BEGV_BYTE && prevpbyte >= pstartbyte)
		  {
		    /* FXIME: What if p is covered by a display
		       string?  See also a FIXME inside
		       bidi_find_paragraph_start.  */
		    p--;
		    pbyte = CHAR_TO_BYTE (p);
		    prevpbyte = bidi_find_paragraph_start (p, pbyte);
		  }
		pstartbyte = prevpbyte;
	      }
	  }
      } while (!string_p
	       && no_default_p && bidi_it->paragraph_dir == NEUTRAL_DIR);
    }
  else
    abort ();

  /* Contrary to UAX#9 clause P3, we only default the paragraph
     direction to L2R if we have no previous usable paragraph
     direction.  This is allowed by the HL1 clause.  */
  if (bidi_it->paragraph_dir != L2R && bidi_it->paragraph_dir != R2L)
    bidi_it->paragraph_dir = L2R; /* P3 and HL1 ``higher-level protocols'' */
  if (bidi_it->paragraph_dir == R2L)
    bidi_it->level_stack[0].level = 1;
  else
    bidi_it->level_stack[0].level = 0;

  bidi_line_init (bidi_it);
}


/***********************************************************************
		 Resolving explicit and implicit levels.
  The rest of this file constitutes the core of the UBA implementation.
 ***********************************************************************/

static inline int
bidi_explicit_dir_char (int ch)
{
  bidi_type_t ch_type;

  if (!bidi_initialized)
    abort ();
  ch_type = (bidi_type_t) XINT (CHAR_TABLE_REF (bidi_type_table, ch));
  return (ch_type == LRE || ch_type == LRO
	  || ch_type == RLE || ch_type == RLO
	  || ch_type == PDF);
}

/* A helper function for bidi_resolve_explicit.  It advances to the
   next character in logical order and determines the new embedding
   level and directional override, but does not take into account
   empty embeddings.  */
static int
bidi_resolve_explicit_1 (struct bidi_it *bidi_it)
{
  int curchar;
  bidi_type_t type;
  int current_level;
  int new_level;
  bidi_dir_t override;
  int string_p = bidi_it->string.s != NULL || STRINGP (bidi_it->string.lstring);

  /* If reseat()'ed, don't advance, so as to start iteration from the
     position where we were reseated.  bidi_it->bytepos can be less
     than BEGV_BYTE after reseat to BEGV.  */
  if (bidi_it->bytepos < (string_p ? 0 : BEGV_BYTE)
      || bidi_it->first_elt)
    {
      bidi_it->first_elt = 0;
      if (string_p)
	{
	  const unsigned char *p
	    = (STRINGP (bidi_it->string.lstring)
	       ? SDATA (bidi_it->string.lstring)
	       : bidi_it->string.s);

	  if (bidi_it->charpos < 0)
	    bidi_it->charpos = 0;
	  bidi_it->bytepos = bidi_count_bytes (p, 0, 0, bidi_it->charpos,
					       bidi_it->string.unibyte);
	}
      else
	{
	  if (bidi_it->charpos < BEGV)
	    bidi_it->charpos = BEGV;
	  bidi_it->bytepos = CHAR_TO_BYTE (bidi_it->charpos);
	}
    }
  /* Don't move at end of buffer/string.  */
  else if (bidi_it->charpos < (string_p ? bidi_it->string.schars : ZV))
    {
      /* Advance to the next character, skipping characters covered by
	 display strings (nchars > 1).  */
      if (bidi_it->nchars <= 0)
	abort ();
      bidi_it->charpos += bidi_it->nchars;
      if (bidi_it->ch_len == 0)
	abort ();
      bidi_it->bytepos += bidi_it->ch_len;
    }

  current_level = bidi_it->level_stack[bidi_it->stack_idx].level; /* X1 */
  override = bidi_it->level_stack[bidi_it->stack_idx].override;
  new_level = current_level;

  if (bidi_it->charpos >= (string_p ? bidi_it->string.schars : ZV))
    {
      curchar = BIDI_EOB;
      bidi_it->ch_len = 1;
      bidi_it->nchars = 1;
      bidi_it->disp_pos = (string_p ? bidi_it->string.schars : ZV);
      bidi_it->disp_prop = 0;
    }
  else
    {
      /* Fetch the character at BYTEPOS.  If it is covered by a
	 display string, treat the entire run of covered characters as
	 a single character u+FFFC.  */
      curchar = bidi_fetch_char (bidi_it->bytepos, bidi_it->charpos,
				 &bidi_it->disp_pos, &bidi_it->disp_prop,
				 &bidi_it->string, bidi_it->frame_window_p,
				 &bidi_it->ch_len, &bidi_it->nchars);
    }
  bidi_it->ch = curchar;

  /* Don't apply directional override here, as all the types we handle
     below will not be affected by the override anyway, and we need
     the original type unaltered.  The override will be applied in
     bidi_resolve_weak.  */
  type = bidi_get_type (curchar, NEUTRAL_DIR);
  bidi_it->orig_type = type;
  bidi_check_type (bidi_it->orig_type);

  if (type != PDF)
    bidi_it->prev_was_pdf = 0;

  bidi_it->type_after_w1 = UNKNOWN_BT;

  switch (type)
    {
      case RLE:	/* X2 */
      case RLO:	/* X4 */
	bidi_it->type_after_w1 = type;
	bidi_check_type (bidi_it->type_after_w1);
	type = WEAK_BN; /* X9/Retaining */
	if (bidi_it->ignore_bn_limit <= -1)
	  {
	    if (current_level <= BIDI_MAXLEVEL - 4)
	      {
		/* Compute the least odd embedding level greater than
		   the current level.  */
		new_level = ((current_level + 1) & ~1) + 1;
		if (bidi_it->type_after_w1 == RLE)
		  override = NEUTRAL_DIR;
		else
		  override = R2L;
		if (current_level == BIDI_MAXLEVEL - 4)
		  bidi_it->invalid_rl_levels = 0;
		bidi_push_embedding_level (bidi_it, new_level, override);
	      }
	    else
	      {
		bidi_it->invalid_levels++;
		/* See the commentary about invalid_rl_levels below.  */
		if (bidi_it->invalid_rl_levels < 0)
		  bidi_it->invalid_rl_levels = 0;
		bidi_it->invalid_rl_levels++;
	      }
	  }
	else if (bidi_it->prev.type_after_w1 == WEAK_EN /* W5/Retaining */
		 || (bidi_it->next_en_pos > bidi_it->charpos
		     && bidi_it->next_en_type == WEAK_EN))
	  type = WEAK_EN;
	break;
      case LRE:	/* X3 */
      case LRO:	/* X5 */
	bidi_it->type_after_w1 = type;
	bidi_check_type (bidi_it->type_after_w1);
	type = WEAK_BN; /* X9/Retaining */
	if (bidi_it->ignore_bn_limit <= -1)
	  {
	    if (current_level <= BIDI_MAXLEVEL - 5)
	      {
		/* Compute the least even embedding level greater than
		   the current level.  */
		new_level = ((current_level + 2) & ~1);
		if (bidi_it->type_after_w1 == LRE)
		  override = NEUTRAL_DIR;
		else
		  override = L2R;
		bidi_push_embedding_level (bidi_it, new_level, override);
	      }
	    else
	      {
		bidi_it->invalid_levels++;
		/* invalid_rl_levels counts invalid levels encountered
		   while the embedding level was already too high for
		   LRE/LRO, but not for RLE/RLO.  That is because
		   there may be exactly one PDF which we should not
		   ignore even though invalid_levels is non-zero.
		   invalid_rl_levels helps to know what PDF is
		   that.  */
		if (bidi_it->invalid_rl_levels >= 0)
		  bidi_it->invalid_rl_levels++;
	      }
	  }
	else if (bidi_it->prev.type_after_w1 == WEAK_EN /* W5/Retaining */
		 || (bidi_it->next_en_pos > bidi_it->charpos
		     && bidi_it->next_en_type == WEAK_EN))
	  type = WEAK_EN;
	break;
      case PDF:	/* X7 */
	bidi_it->type_after_w1 = type;
	bidi_check_type (bidi_it->type_after_w1);
	type = WEAK_BN; /* X9/Retaining */
	if (bidi_it->ignore_bn_limit <= -1)
	  {
	    if (!bidi_it->invalid_rl_levels)
	      {
		new_level = bidi_pop_embedding_level (bidi_it);
		bidi_it->invalid_rl_levels = -1;
		if (bidi_it->invalid_levels)
		  bidi_it->invalid_levels--;
		/* else nothing: UAX#9 says to ignore invalid PDFs */
	      }
	    if (!bidi_it->invalid_levels)
	      new_level = bidi_pop_embedding_level (bidi_it);
	    else
	      {
		bidi_it->invalid_levels--;
		bidi_it->invalid_rl_levels--;
	      }
	  }
	else if (bidi_it->prev.type_after_w1 == WEAK_EN /* W5/Retaining */
		 || (bidi_it->next_en_pos > bidi_it->charpos
		     && bidi_it->next_en_type == WEAK_EN))
	  type = WEAK_EN;
	break;
      default:
	/* Nothing.  */
	break;
    }

  bidi_it->type = type;
  bidi_check_type (bidi_it->type);

  return new_level;
}

/* Given an iterator state in BIDI_IT, advance one character position
   in the buffer/string to the next character (in the logical order),
   resolve any explicit embeddings and directional overrides, and
   return the embedding level of the character after resolving
   explicit directives and ignoring empty embeddings.  */
static int
bidi_resolve_explicit (struct bidi_it *bidi_it)
{
  int prev_level = bidi_it->level_stack[bidi_it->stack_idx].level;
  int new_level  = bidi_resolve_explicit_1 (bidi_it);
  EMACS_INT eob = bidi_it->string.s ? bidi_it->string.schars : ZV;
  const unsigned char *s
    = (STRINGP (bidi_it->string.lstring)
       ? SDATA (bidi_it->string.lstring)
       : bidi_it->string.s);

  if (prev_level < new_level
      && bidi_it->type == WEAK_BN
      && bidi_it->ignore_bn_limit == -1 /* only if not already known */
      && bidi_it->charpos < eob		/* not already at EOB */
      && bidi_explicit_dir_char (bidi_char_at_pos (bidi_it->bytepos
						   + bidi_it->ch_len, s,
						   bidi_it->string.unibyte)))
    {
      /* Avoid pushing and popping embedding levels if the level run
	 is empty, as this breaks level runs where it shouldn't.
	 UAX#9 removes all the explicit embedding and override codes,
	 so empty embeddings disappear without a trace.  We need to
	 behave as if we did the same.  */
      struct bidi_it saved_it;
      int level = prev_level;

      bidi_copy_it (&saved_it, bidi_it);

      while (bidi_explicit_dir_char (bidi_char_at_pos (bidi_it->bytepos
						       + bidi_it->ch_len, s,
						       bidi_it->string.unibyte)))
	{
	  /* This advances to the next character, skipping any
	     characters covered by display strings.  */
	  level = bidi_resolve_explicit_1 (bidi_it);
	  /* If string.lstring was relocated inside bidi_resolve_explicit_1,
	     a pointer to its data is no longer valid.  */
	  if (STRINGP (bidi_it->string.lstring))
	    s = SDATA (bidi_it->string.lstring);
	}

      if (bidi_it->nchars <= 0)
	abort ();
      if (level == prev_level)	/* empty embedding */
	saved_it.ignore_bn_limit = bidi_it->charpos + bidi_it->nchars;
      else			/* this embedding is non-empty */
	saved_it.ignore_bn_limit = -2;

      bidi_copy_it (bidi_it, &saved_it);
      if (bidi_it->ignore_bn_limit > -1)
	{
	  /* We pushed a level, but we shouldn't have.  Undo that. */
	  if (!bidi_it->invalid_rl_levels)
	    {
	      new_level = bidi_pop_embedding_level (bidi_it);
	      bidi_it->invalid_rl_levels = -1;
	      if (bidi_it->invalid_levels)
		bidi_it->invalid_levels--;
	    }
	  if (!bidi_it->invalid_levels)
	    new_level = bidi_pop_embedding_level (bidi_it);
	  else
	    {
	      bidi_it->invalid_levels--;
	      bidi_it->invalid_rl_levels--;
	    }
	}
    }

  if (bidi_it->type == NEUTRAL_B)	/* X8 */
    {
      bidi_set_paragraph_end (bidi_it);
      /* This is needed by bidi_resolve_weak below, and in L1.  */
      bidi_it->type_after_w1 = bidi_it->type;
      bidi_check_type (bidi_it->type_after_w1);
    }

  return new_level;
}

/* Advance in the buffer/string, resolve weak types and return the
   type of the next character after weak type resolution.  */
static bidi_type_t
bidi_resolve_weak (struct bidi_it *bidi_it)
{
  bidi_type_t type;
  bidi_dir_t override;
  int prev_level = bidi_it->level_stack[bidi_it->stack_idx].level;
  int new_level  = bidi_resolve_explicit (bidi_it);
  int next_char;
  bidi_type_t type_of_next;
  struct bidi_it saved_it;
  EMACS_INT eob
    = ((STRINGP (bidi_it->string.lstring) || bidi_it->string.s)
       ? bidi_it->string.schars : ZV);

  type = bidi_it->type;
  override = bidi_it->level_stack[bidi_it->stack_idx].override;

  if (type == UNKNOWN_BT
      || type == LRE
      || type == LRO
      || type == RLE
      || type == RLO
      || type == PDF)
    abort ();

  if (new_level != prev_level
      || bidi_it->type == NEUTRAL_B)
    {
      /* We've got a new embedding level run, compute the directional
         type of sor and initialize per-run variables (UAX#9, clause
         X10).  */
      bidi_set_sor_type (bidi_it, prev_level, new_level);
    }
  else if (type == NEUTRAL_S || type == NEUTRAL_WS
	   || type == WEAK_BN || type == STRONG_AL)
    bidi_it->type_after_w1 = type;	/* needed in L1 */
  bidi_check_type (bidi_it->type_after_w1);

  /* Level and directional override status are already recorded in
     bidi_it, and do not need any change; see X6.  */
  if (override == R2L)		/* X6 */
    type = STRONG_R;
  else if (override == L2R)
    type = STRONG_L;
  else
    {
      if (type == WEAK_NSM)	/* W1 */
	{
	  /* Note that we don't need to consider the case where the
	     prev character has its type overridden by an RLO or LRO,
	     because then either the type of this NSM would have been
	     also overridden, or the previous character is outside the
	     current level run, and thus not relevant to this NSM.
	     This is why NSM gets the type_after_w1 of the previous
	     character.  */
	  if (bidi_it->prev.type_after_w1 != UNKNOWN_BT
	      /* if type_after_w1 is NEUTRAL_B, this NSM is at sor */
	      && bidi_it->prev.type_after_w1 != NEUTRAL_B)
	    type = bidi_it->prev.type_after_w1;
	  else if (bidi_it->sor == R2L)
	    type = STRONG_R;
	  else if (bidi_it->sor == L2R)
	    type = STRONG_L;
	  else /* shouldn't happen! */
	    abort ();
	}
      if (type == WEAK_EN	/* W2 */
	  && bidi_it->last_strong.type_after_w1 == STRONG_AL)
	type = WEAK_AN;
      else if (type == STRONG_AL) /* W3 */
	type = STRONG_R;
      else if ((type == WEAK_ES	/* W4 */
		&& bidi_it->prev.type_after_w1 == WEAK_EN
		&& bidi_it->prev.orig_type == WEAK_EN)
	       || (type == WEAK_CS
		   && ((bidi_it->prev.type_after_w1 == WEAK_EN
			&& bidi_it->prev.orig_type == WEAK_EN)
		       || bidi_it->prev.type_after_w1 == WEAK_AN)))
	{
	  const unsigned char *s
	    = (STRINGP (bidi_it->string.lstring)
	       ? SDATA (bidi_it->string.lstring)
	       : bidi_it->string.s);

	  next_char = (bidi_it->charpos + bidi_it->nchars >= eob
		       ? BIDI_EOB
		       : bidi_char_at_pos (bidi_it->bytepos + bidi_it->ch_len,
					   s, bidi_it->string.unibyte));
	  type_of_next = bidi_get_type (next_char, override);

	  if (type_of_next == WEAK_BN
	      || bidi_explicit_dir_char (next_char))
	    {
	      bidi_copy_it (&saved_it, bidi_it);
	      while (bidi_resolve_explicit (bidi_it) == new_level
		     && bidi_it->type == WEAK_BN)
		;
	      type_of_next = bidi_it->type;
	      bidi_copy_it (bidi_it, &saved_it);
	    }

	  /* If the next character is EN, but the last strong-type
	     character is AL, that next EN will be changed to AN when
	     we process it in W2 above.  So in that case, this ES
	     should not be changed into EN.  */
	  if (type == WEAK_ES
	      && type_of_next == WEAK_EN
	      && bidi_it->last_strong.type_after_w1 != STRONG_AL)
	    type = WEAK_EN;
	  else if (type == WEAK_CS)
	    {
	      if (bidi_it->prev.type_after_w1 == WEAK_AN
		  && (type_of_next == WEAK_AN
		      /* If the next character is EN, but the last
			 strong-type character is AL, EN will be later
			 changed to AN when we process it in W2 above.
			 So in that case, this ES should not be
			 changed into EN.  */
		      || (type_of_next == WEAK_EN
			  && bidi_it->last_strong.type_after_w1 == STRONG_AL)))
		type = WEAK_AN;
	      else if (bidi_it->prev.type_after_w1 == WEAK_EN
		       && type_of_next == WEAK_EN
		       && bidi_it->last_strong.type_after_w1 != STRONG_AL)
		type = WEAK_EN;
	    }
	}
      else if (type == WEAK_ET	/* W5: ET with EN before or after it */
	       || type == WEAK_BN)	/* W5/Retaining */
	{
	  if (bidi_it->prev.type_after_w1 == WEAK_EN) /* ET/BN w/EN before it */
	    type = WEAK_EN;
	  else if (bidi_it->next_en_pos > bidi_it->charpos
		   && bidi_it->next_en_type != WEAK_BN)
	    {
	      if (bidi_it->next_en_type == WEAK_EN) /* ET/BN with EN after it */
		type = WEAK_EN;
	    }
	  else if (bidi_it->next_en_pos >=0)
	    {
	      EMACS_INT en_pos = bidi_it->charpos + bidi_it->nchars;
	      const unsigned char *s = (STRINGP (bidi_it->string.lstring)
					? SDATA (bidi_it->string.lstring)
					: bidi_it->string.s);

	      if (bidi_it->nchars <= 0)
		abort ();
	      next_char
		= (bidi_it->charpos + bidi_it->nchars >= eob
		   ? BIDI_EOB
		   : bidi_char_at_pos (bidi_it->bytepos + bidi_it->ch_len, s,
				       bidi_it->string.unibyte));
	      type_of_next = bidi_get_type (next_char, override);

	      if (type_of_next == WEAK_ET
		  || type_of_next == WEAK_BN
		  || bidi_explicit_dir_char (next_char))
		{
		  bidi_copy_it (&saved_it, bidi_it);
		  while (bidi_resolve_explicit (bidi_it) == new_level
			 && (bidi_it->type == WEAK_BN
			     || bidi_it->type == WEAK_ET))
		    ;
		  type_of_next = bidi_it->type;
		  en_pos = bidi_it->charpos;
		  bidi_copy_it (bidi_it, &saved_it);
		}
	      /* Remember this position, to speed up processing of the
		 next ETs.  */
	      bidi_it->next_en_pos = en_pos;
	      if (type_of_next == WEAK_EN)
		{
		  /* If the last strong character is AL, the EN we've
		     found will become AN when we get to it (W2). */
		  if (bidi_it->last_strong.type_after_w1 == STRONG_AL)
		    type_of_next = WEAK_AN;
		  else if (type == WEAK_BN)
		    type = NEUTRAL_ON; /* W6/Retaining */
		  else
		    type = WEAK_EN;
		}
	      else if (type_of_next == NEUTRAL_B)
		/* Record the fact that there are no more ENs from
		   here to the end of paragraph, to avoid entering the
		   loop above ever again in this paragraph.  */
		bidi_it->next_en_pos = -1;
	      /* Record the type of the character where we ended our search.  */
	      bidi_it->next_en_type = type_of_next;
	    }
	}
    }

  if (type == WEAK_ES || type == WEAK_ET || type == WEAK_CS /* W6 */
      || (type == WEAK_BN
	  && (bidi_it->prev.type_after_w1 == WEAK_CS	    /* W6/Retaining */
	      || bidi_it->prev.type_after_w1 == WEAK_ES
	      || bidi_it->prev.type_after_w1 == WEAK_ET)))
    type = NEUTRAL_ON;

  /* Store the type we've got so far, before we clobber it with strong
     types in W7 and while resolving neutral types.  But leave alone
     the original types that were recorded above, because we will need
     them for the L1 clause.  */
  if (bidi_it->type_after_w1 == UNKNOWN_BT)
    bidi_it->type_after_w1 = type;
  bidi_check_type (bidi_it->type_after_w1);

  if (type == WEAK_EN)	/* W7 */
    {
      if ((bidi_it->last_strong.type_after_w1 == STRONG_L)
	  || (bidi_it->last_strong.type == UNKNOWN_BT && bidi_it->sor == L2R))
	type = STRONG_L;
    }

  bidi_it->type = type;
  bidi_check_type (bidi_it->type);
  return type;
}

/* Resolve the type of a neutral character according to the type of
   surrounding strong text and the current embedding level.  */
static inline bidi_type_t
bidi_resolve_neutral_1 (bidi_type_t prev_type, bidi_type_t next_type, int lev)
{
  /* N1: European and Arabic numbers are treated as though they were R.  */
  if (next_type == WEAK_EN || next_type == WEAK_AN)
    next_type = STRONG_R;
  if (prev_type == WEAK_EN || prev_type == WEAK_AN)
    prev_type = STRONG_R;

  if (next_type == prev_type)	/* N1 */
    return next_type;
  else if ((lev & 1) == 0)	/* N2 */
    return STRONG_L;
  else
    return STRONG_R;
}

static bidi_type_t
bidi_resolve_neutral (struct bidi_it *bidi_it)
{
  int prev_level = bidi_it->level_stack[bidi_it->stack_idx].level;
  bidi_type_t type = bidi_resolve_weak (bidi_it);
  int current_level = bidi_it->level_stack[bidi_it->stack_idx].level;

  if (!(type == STRONG_R
	|| type == STRONG_L
	|| type == WEAK_BN
	|| type == WEAK_EN
	|| type == WEAK_AN
	|| type == NEUTRAL_B
	|| type == NEUTRAL_S
	|| type == NEUTRAL_WS
	|| type == NEUTRAL_ON))
    abort ();

  if ((type != NEUTRAL_B /* Don't risk entering the long loop below if
			    we are already at paragraph end.  */
       && bidi_get_category (type) == NEUTRAL)
      || (type == WEAK_BN && prev_level == current_level))
    {
      if (bidi_it->next_for_neutral.type != UNKNOWN_BT)
	type = bidi_resolve_neutral_1 (bidi_it->prev_for_neutral.type,
				       bidi_it->next_for_neutral.type,
				       current_level);
      /* The next two "else if" clauses are shortcuts for the
	 important special case when we have a long sequence of
	 neutral or WEAK_BN characters, such as whitespace or nulls or
	 other control characters, on the base embedding level of the
	 paragraph, and that sequence goes all the way to the end of
	 the paragraph and follows a character whose resolved
	 directionality is identical to the base embedding level.
	 (This is what happens in a buffer with plain L2R text that
	 happens to include long sequences of control characters.)  By
	 virtue of N1, the result of examining this long sequence will
	 always be either STRONG_L or STRONG_R, depending on the base
	 embedding level.  So we use this fact directly instead of
	 entering the expensive loop in the "else" clause.  */
      else if (current_level == 0
	       && bidi_it->prev_for_neutral.type == STRONG_L
	       && !bidi_explicit_dir_char (bidi_it->ch))
	type = bidi_resolve_neutral_1 (bidi_it->prev_for_neutral.type,
				       STRONG_L, current_level);
      else if (/* current level is 1 */
	       current_level == 1
	       /* base embedding level is also 1 */
	       && bidi_it->level_stack[0].level == 1
	       /* previous character is one of those considered R for
		  the purposes of W5 */
	       && (bidi_it->prev_for_neutral.type == STRONG_R
		   || bidi_it->prev_for_neutral.type == WEAK_EN
		   || bidi_it->prev_for_neutral.type == WEAK_AN)
	       && !bidi_explicit_dir_char (bidi_it->ch))
	type = bidi_resolve_neutral_1 (bidi_it->prev_for_neutral.type,
				       STRONG_R, current_level);
      else
	{
	  /* Arrrgh!!  The UAX#9 algorithm is too deeply entrenched in
	     the assumption of batch-style processing; see clauses W4,
	     W5, and especially N1, which require to look far forward
	     (as well as back) in the buffer/string.  May the fleas of
	     a thousand camels infest the armpits of those who design
	     supposedly general-purpose algorithms by looking at their
	     own implementations, and fail to consider other possible
	     implementations!  */
	  struct bidi_it saved_it;
	  bidi_type_t next_type;

	  if (bidi_it->scan_dir == -1)
	    abort ();

	  bidi_copy_it (&saved_it, bidi_it);
	  /* Scan the text forward until we find the first non-neutral
	     character, and then use that to resolve the neutral we
	     are dealing with now.  We also cache the scanned iterator
	     states, to salvage some of the effort later.  */
	  bidi_cache_iterator_state (bidi_it, 0);
	  do {
	    /* Record the info about the previous character, so that
	       it will be cached below with this state.  */
	    if (bidi_it->type_after_w1 != WEAK_BN /* W1/Retaining */
		&& bidi_it->type != WEAK_BN)
	      bidi_remember_char (&bidi_it->prev, bidi_it);
	    type = bidi_resolve_weak (bidi_it);
	    /* Paragraph separators have their levels fully resolved
	       at this point, so cache them as resolved.  */
	    bidi_cache_iterator_state (bidi_it, type == NEUTRAL_B);
	    /* FIXME: implement L1 here, by testing for a newline and
	       resetting the level for any sequence of whitespace
	       characters adjacent to it.  */
	  } while (!(type == NEUTRAL_B
		     || (type != WEAK_BN
			 && bidi_get_category (type) != NEUTRAL)
		     /* This is all per level run, so stop when we
			reach the end of this level run.  */
		     || (bidi_it->level_stack[bidi_it->stack_idx].level
			 != current_level)));

	  bidi_remember_char (&saved_it.next_for_neutral, bidi_it);

	  switch (type)
	    {
	      case STRONG_L:
	      case STRONG_R:
	      case STRONG_AL:
		/* Actually, STRONG_AL cannot happen here, because
		   bidi_resolve_weak converts it to STRONG_R, per W3.  */
		xassert (type != STRONG_AL);
		next_type = type;
		break;
	      case WEAK_EN:
	      case WEAK_AN:
		/* N1: ``European and Arabic numbers are treated as
		   though they were R.''  */
		next_type = STRONG_R;
		break;
	      case WEAK_BN:
		if (!bidi_explicit_dir_char (bidi_it->ch))
		  abort ();		/* can't happen: BNs are skipped */
		/* FALLTHROUGH */
	      case NEUTRAL_B:
		/* Marched all the way to the end of this level run.
		   We need to use the eor type, whose information is
		   stored by bidi_set_sor_type in the prev_for_neutral
		   member.  */
		if (saved_it.type != WEAK_BN
		    || bidi_get_category (bidi_it->prev.type_after_w1) == NEUTRAL)
		  next_type = bidi_it->prev_for_neutral.type;
		else
		  {
		    /* This is a BN which does not adjoin neutrals.
		       Leave its type alone.  */
		    bidi_copy_it (bidi_it, &saved_it);
		    return bidi_it->type;
		  }
		break;
	      default:
		abort ();
	    }
	  type = bidi_resolve_neutral_1 (saved_it.prev_for_neutral.type,
					 next_type, current_level);
	  saved_it.next_for_neutral.type = next_type;
	  saved_it.type = type;
	  bidi_check_type (next_type);
	  bidi_check_type (type);
	  bidi_copy_it (bidi_it, &saved_it);
	}
    }
  return type;
}

/* Given an iterator state in BIDI_IT, advance one character position
   in the buffer/string to the next character (in the logical order),
   resolve the bidi type of that next character, and return that
   type.  */
static bidi_type_t
bidi_type_of_next_char (struct bidi_it *bidi_it)
{
  bidi_type_t type;

  /* This should always be called during a forward scan.  */
  if (bidi_it->scan_dir != 1)
    abort ();

  /* Reset the limit until which to ignore BNs if we step out of the
     area where we found only empty levels.  */
  if ((bidi_it->ignore_bn_limit > -1
       && bidi_it->ignore_bn_limit <= bidi_it->charpos)
      || (bidi_it->ignore_bn_limit == -2
	  && !bidi_explicit_dir_char (bidi_it->ch)))
    bidi_it->ignore_bn_limit = -1;

  type = bidi_resolve_neutral (bidi_it);

  return type;
}

/* Given an iterator state BIDI_IT, advance one character position in
   the buffer/string to the next character (in the current scan
   direction), resolve the embedding and implicit levels of that next
   character, and return the resulting level.  */
static int
bidi_level_of_next_char (struct bidi_it *bidi_it)
{
  bidi_type_t type;
  int level, prev_level = -1;
  struct bidi_saved_info next_for_neutral;
  EMACS_INT next_char_pos = -2;

  if (bidi_it->scan_dir == 1)
    {
      EMACS_INT eob
	= ((bidi_it->string.s || STRINGP (bidi_it->string.lstring))
	   ? bidi_it->string.schars : ZV);

      /* There's no sense in trying to advance if we hit end of text.  */
      if (bidi_it->charpos >= eob)
	return bidi_it->resolved_level;

      /* Record the info about the previous character.  */
      if (bidi_it->type_after_w1 != WEAK_BN /* W1/Retaining */
	  && bidi_it->type != WEAK_BN)
	bidi_remember_char (&bidi_it->prev, bidi_it);
      if (bidi_it->type_after_w1 == STRONG_R
	  || bidi_it->type_after_w1 == STRONG_L
	  || bidi_it->type_after_w1 == STRONG_AL)
	bidi_remember_char (&bidi_it->last_strong, bidi_it);
      /* FIXME: it sounds like we don't need both prev and
	 prev_for_neutral members, but I'm leaving them both for now.  */
      if (bidi_it->type == STRONG_R || bidi_it->type == STRONG_L
	  || bidi_it->type == WEAK_EN || bidi_it->type == WEAK_AN)
	bidi_remember_char (&bidi_it->prev_for_neutral, bidi_it);

      /* If we overstepped the characters used for resolving neutrals
	 and whitespace, invalidate their info in the iterator.  */
      if (bidi_it->charpos >= bidi_it->next_for_neutral.charpos)
	bidi_it->next_for_neutral.type = UNKNOWN_BT;
      if (bidi_it->next_en_pos >= 0
	  && bidi_it->charpos >= bidi_it->next_en_pos)
	{
	  bidi_it->next_en_pos = 0;
	  bidi_it->next_en_type = UNKNOWN_BT;
	}
      if (bidi_it->next_for_ws.type != UNKNOWN_BT
	  && bidi_it->charpos >= bidi_it->next_for_ws.charpos)
	bidi_it->next_for_ws.type = UNKNOWN_BT;

      /* This must be taken before we fill the iterator with the info
	 about the next char.  If we scan backwards, the iterator
	 state must be already cached, so there's no need to know the
	 embedding level of the previous character, since we will be
	 returning to our caller shortly.  */
      prev_level = bidi_it->level_stack[bidi_it->stack_idx].level;
    }
  next_for_neutral = bidi_it->next_for_neutral;

  /* Perhaps the character we want is already cached.  If it is, the
     call to bidi_cache_find below will return a type other than
     UNKNOWN_BT.  */
  if (bidi_cache_idx > bidi_cache_start && !bidi_it->first_elt)
    {
      int bob = ((bidi_it->string.s || STRINGP (bidi_it->string.lstring))
		 ? 0 : 1);
      if (bidi_it->scan_dir > 0)
	{
	  if (bidi_it->nchars <= 0)
	    abort ();
	  next_char_pos = bidi_it->charpos + bidi_it->nchars;
	}
      else if (bidi_it->charpos >= bob)
	/* Implementation note: we allow next_char_pos to be as low as
	   0 for buffers or -1 for strings, and that is okay because
	   that's the "position" of the sentinel iterator state we
	   cached at the beginning of the iteration.  */
	next_char_pos = bidi_it->charpos - 1;
      if (next_char_pos >= bob - 1)
	type = bidi_cache_find (next_char_pos, -1, bidi_it);
      else
	type = UNKNOWN_BT;
    }
  else
    type = UNKNOWN_BT;
  if (type != UNKNOWN_BT)
    {
      /* Don't lose the information for resolving neutrals!  The
	 cached states could have been cached before their
	 next_for_neutral member was computed.  If we are on our way
	 forward, we can simply take the info from the previous
	 state.  */
      if (bidi_it->scan_dir == 1
	  && bidi_it->next_for_neutral.type == UNKNOWN_BT)
	bidi_it->next_for_neutral = next_for_neutral;

      /* If resolved_level is -1, it means this state was cached
	 before it was completely resolved, so we cannot return
	 it.  */
      if (bidi_it->resolved_level != -1)
	return bidi_it->resolved_level;
    }
  if (bidi_it->scan_dir == -1)
    /* If we are going backwards, the iterator state is already cached
       from previous scans, and should be fully resolved.  */
    abort ();

  if (type == UNKNOWN_BT)
    type = bidi_type_of_next_char (bidi_it);

  if (type == NEUTRAL_B)
    return bidi_it->resolved_level;

  level = bidi_it->level_stack[bidi_it->stack_idx].level;
  if ((bidi_get_category (type) == NEUTRAL /* && type != NEUTRAL_B */)
      || (type == WEAK_BN && prev_level == level))
    {
      if (bidi_it->next_for_neutral.type == UNKNOWN_BT)
	abort ();

      /* If the cached state shows a neutral character, it was not
	 resolved by bidi_resolve_neutral, so do it now.  */
      type = bidi_resolve_neutral_1 (bidi_it->prev_for_neutral.type,
				     bidi_it->next_for_neutral.type,
				     level);
    }

  if (!(type == STRONG_R
	|| type == STRONG_L
	|| type == WEAK_BN
	|| type == WEAK_EN
	|| type == WEAK_AN))
    abort ();
  bidi_it->type = type;
  bidi_check_type (bidi_it->type);

  /* For L1 below, we need to know, for each WS character, whether
     it belongs to a sequence of WS characters preceding a newline
     or a TAB or a paragraph separator.  */
  if (bidi_it->orig_type == NEUTRAL_WS
      && bidi_it->next_for_ws.type == UNKNOWN_BT)
    {
      int ch;
      EMACS_INT clen = bidi_it->ch_len;
      EMACS_INT bpos = bidi_it->bytepos;
      EMACS_INT cpos = bidi_it->charpos;
      EMACS_INT disp_pos = bidi_it->disp_pos;
      EMACS_INT nc = bidi_it->nchars;
      struct bidi_string_data bs = bidi_it->string;
      bidi_type_t chtype;
      int fwp = bidi_it->frame_window_p;
      int dpp = bidi_it->disp_prop;

      if (bidi_it->nchars <= 0)
	abort ();
      do {
	ch = bidi_fetch_char (bpos += clen, cpos += nc, &disp_pos, &dpp, &bs,
			      fwp, &clen, &nc);
	if (ch == '\n' || ch == BIDI_EOB)
	  chtype = NEUTRAL_B;
	else
	  chtype = bidi_get_type (ch, NEUTRAL_DIR);
      } while (chtype == NEUTRAL_WS || chtype == WEAK_BN
	       || bidi_explicit_dir_char (ch)); /* L1/Retaining */
      bidi_it->next_for_ws.type = chtype;
      bidi_check_type (bidi_it->next_for_ws.type);
      bidi_it->next_for_ws.charpos = cpos;
      bidi_it->next_for_ws.bytepos = bpos;
    }

  /* Resolve implicit levels, with a twist: PDFs get the embedding
     level of the embedding they terminate.  See below for the
     reason.  */
  if (bidi_it->orig_type == PDF
      /* Don't do this if this formatting code didn't change the
	 embedding level due to invalid or empty embeddings.  */
      && prev_level != level)
    {
      /* Don't look in UAX#9 for the reason for this: it's our own
	 private quirk.  The reason is that we want the formatting
	 codes to be delivered so that they bracket the text of their
	 embedding.  For example, given the text

	     {RLO}teST{PDF}

	 we want it to be displayed as

	     {PDF}STet{RLO}

	 not as

	     STet{RLO}{PDF}

	 which will result because we bump up the embedding level as
	 soon as we see the RLO and pop it as soon as we see the PDF,
	 so RLO itself has the same embedding level as "teST", and
	 thus would be normally delivered last, just before the PDF.
	 The switch below fiddles with the level of PDF so that this
	 ugly side effect does not happen.

	 (This is, of course, only important if the formatting codes
	 are actually displayed, but Emacs does need to display them
	 if the user wants to.)  */
      level = prev_level;
    }
  else if (bidi_it->orig_type == NEUTRAL_B /* L1 */
	   || bidi_it->orig_type == NEUTRAL_S
	   || bidi_it->ch == '\n' || bidi_it->ch == BIDI_EOB
	   || (bidi_it->orig_type == NEUTRAL_WS
	       && (bidi_it->next_for_ws.type == NEUTRAL_B
		   || bidi_it->next_for_ws.type == NEUTRAL_S)))
    level = bidi_it->level_stack[0].level;
  else if ((level & 1) == 0) /* I1 */
    {
      if (type == STRONG_R)
	level++;
      else if (type == WEAK_EN || type == WEAK_AN)
	level += 2;
    }
  else			/* I2 */
    {
      if (type == STRONG_L || type == WEAK_EN || type == WEAK_AN)
	level++;
    }

  bidi_it->resolved_level = level;
  return level;
}

/* Move to the other edge of a level given by LEVEL.  If END_FLAG is
   non-zero, we are at the end of a level, and we need to prepare to
   resume the scan of the lower level.

   If this level's other edge is cached, we simply jump to it, filling
   the iterator structure with the iterator state on the other edge.
   Otherwise, we walk the buffer or string until we come back to the
   same level as LEVEL.

   Note: we are not talking here about a ``level run'' in the UAX#9
   sense of the term, but rather about a ``level'' which includes
   all the levels higher than it.  In other words, given the levels
   like this:

         11111112222222333333334443343222222111111112223322111
                A      B                    C

   and assuming we are at point A scanning left to right, this
   function moves to point C, whereas the UAX#9 ``level 2 run'' ends
   at point B.  */
static void
bidi_find_other_level_edge (struct bidi_it *bidi_it, int level, int end_flag)
{
  int dir = end_flag ? -bidi_it->scan_dir : bidi_it->scan_dir;
  ptrdiff_t idx;

  /* Try the cache first.  */
  if ((idx = bidi_cache_find_level_change (level, dir, end_flag))
      >= bidi_cache_start)
    bidi_cache_fetch_state (idx, bidi_it);
  else
    {
      int new_level;

      if (end_flag)
	abort (); /* if we are at end of level, its edges must be cached */

      bidi_cache_iterator_state (bidi_it, 1);
      do {
	new_level = bidi_level_of_next_char (bidi_it);
	bidi_cache_iterator_state (bidi_it, 1);
      } while (new_level >= level);
    }
}

void
bidi_move_to_visually_next (struct bidi_it *bidi_it)
{
  int old_level, new_level, next_level;
  struct bidi_it sentinel;
  struct gcpro gcpro1;

  if (bidi_it->charpos < 0 || bidi_it->bytepos < 0)
    abort ();

  if (bidi_it->scan_dir == 0)
    {
      bidi_it->scan_dir = 1;	/* default to logical order */
    }

  /* The code below can call eval, and thus cause GC.  If we are
     iterating a Lisp string, make sure it won't be GCed.  */
  if (STRINGP (bidi_it->string.lstring))
    GCPRO1 (bidi_it->string.lstring);

  /* If we just passed a newline, initialize for the next line.  */
  if (!bidi_it->first_elt
      && (bidi_it->ch == '\n' || bidi_it->ch == BIDI_EOB))
    bidi_line_init (bidi_it);

  /* Prepare the sentinel iterator state, and cache it.  When we bump
     into it, scanning backwards, we'll know that the last non-base
     level is exhausted.  */
  if (bidi_cache_idx == bidi_cache_start)
    {
      bidi_copy_it (&sentinel, bidi_it);
      if (bidi_it->first_elt)
	{
	  sentinel.charpos--;	/* cached charpos needs to be monotonic */
	  sentinel.bytepos--;
	  sentinel.ch = '\n';	/* doesn't matter, but why not? */
	  sentinel.ch_len = 1;
	  sentinel.nchars = 1;
	}
      bidi_cache_iterator_state (&sentinel, 1);
    }

  old_level = bidi_it->resolved_level;
  new_level = bidi_level_of_next_char (bidi_it);

  /* Reordering of resolved levels (clause L2) is implemented by
     jumping to the other edge of the level and flipping direction of
     scanning the text whenever we find a level change.  */
  if (new_level != old_level)
    {
      int ascending = new_level > old_level;
      int level_to_search = ascending ? old_level + 1 : old_level;
      int incr = ascending ? 1 : -1;
      int expected_next_level = old_level + incr;

      /* Jump (or walk) to the other edge of this level.  */
      bidi_find_other_level_edge (bidi_it, level_to_search, !ascending);
      /* Switch scan direction and peek at the next character in the
	 new direction.  */
      bidi_it->scan_dir = -bidi_it->scan_dir;

      /* The following loop handles the case where the resolved level
	 jumps by more than one.  This is typical for numbers inside a
	 run of text with left-to-right embedding direction, but can
	 also happen in other situations.  In those cases the decision
	 where to continue after a level change, and in what direction,
	 is tricky.  For example, given a text like below:

	          abcdefgh
	          11336622

	 (where the numbers below the text show the resolved levels),
	 the result of reordering according to UAX#9 should be this:

		  efdcghba

	 This is implemented by the loop below which flips direction
	 and jumps to the other edge of the level each time it finds
	 the new level not to be the expected one.  The expected level
	 is always one more or one less than the previous one.  */
      next_level = bidi_peek_at_next_level (bidi_it);
      while (next_level != expected_next_level)
	{
	  expected_next_level += incr;
	  level_to_search += incr;
	  bidi_find_other_level_edge (bidi_it, level_to_search, !ascending);
	  bidi_it->scan_dir = -bidi_it->scan_dir;
	  next_level = bidi_peek_at_next_level (bidi_it);
	}

      /* Finally, deliver the next character in the new direction.  */
      next_level = bidi_level_of_next_char (bidi_it);
    }

  /* Take note when we have just processed the newline that precedes
     the end of the paragraph.  The next time we are about to be
     called, set_iterator_to_next will automatically reinit the
     paragraph direction, if needed.  We do this at the newline before
     the paragraph separator, because the next character might not be
     the first character of the next paragraph, due to the bidi
     reordering, whereas we _must_ know the paragraph base direction
     _before_ we process the paragraph's text, since the base
     direction affects the reordering.  */
  if (bidi_it->scan_dir == 1
      && (bidi_it->ch == '\n' || bidi_it->ch == BIDI_EOB))
    {
      /* The paragraph direction of the entire string, once
	 determined, is in effect for the entire string.  Setting the
	 separator limit to the end of the string prevents
	 bidi_paragraph_init from being called automatically on this
	 string.  */
      if (bidi_it->string.s || STRINGP (bidi_it->string.lstring))
	bidi_it->separator_limit = bidi_it->string.schars;
      else if (bidi_it->bytepos < ZV_BYTE)
	{
	  EMACS_INT sep_len
	    = bidi_at_paragraph_end (bidi_it->charpos + bidi_it->nchars,
				     bidi_it->bytepos + bidi_it->ch_len);
	  if (bidi_it->nchars <= 0)
	    abort ();
	  if (sep_len >= 0)
	    {
	      bidi_it->new_paragraph = 1;
	      /* Record the buffer position of the last character of the
		 paragraph separator.  */
	      bidi_it->separator_limit
		= bidi_it->charpos + bidi_it->nchars + sep_len;
	    }
	}
    }

  if (bidi_it->scan_dir == 1 && bidi_cache_idx > bidi_cache_start)
    {
      /* If we are at paragraph's base embedding level and beyond the
	 last cached position, the cache's job is done and we can
	 discard it.  */
      if (bidi_it->resolved_level == bidi_it->level_stack[0].level
	  && bidi_it->charpos > (bidi_cache[bidi_cache_idx - 1].charpos
				 + bidi_cache[bidi_cache_idx - 1].nchars - 1))
	bidi_cache_reset ();
	/* But as long as we are caching during forward scan, we must
	   cache each state, or else the cache integrity will be
	   compromised: it assumes cached states correspond to buffer
	   positions 1:1.  */
      else
	bidi_cache_iterator_state (bidi_it, 1);
    }

  if (STRINGP (bidi_it->string.lstring))
    UNGCPRO;
}

/* This is meant to be called from within the debugger, whenever you
   wish to examine the cache contents.  */
void bidi_dump_cached_states (void) EXTERNALLY_VISIBLE;
void
bidi_dump_cached_states (void)
{
  ptrdiff_t i;
  int ndigits = 1;

  if (bidi_cache_idx == 0)
    {
      fprintf (stderr, "The cache is empty.\n");
      return;
    }
  fprintf (stderr, "Total of  %"pD"d state%s in cache:\n",
	   bidi_cache_idx, bidi_cache_idx == 1 ? "" : "s");

  for (i = bidi_cache[bidi_cache_idx - 1].charpos; i > 0; i /= 10)
    ndigits++;
  fputs ("ch  ", stderr);
  for (i = 0; i < bidi_cache_idx; i++)
    fprintf (stderr, "%*c", ndigits, bidi_cache[i].ch);
  fputs ("\n", stderr);
  fputs ("lvl ", stderr);
  for (i = 0; i < bidi_cache_idx; i++)
    fprintf (stderr, "%*d", ndigits, bidi_cache[i].resolved_level);
  fputs ("\n", stderr);
  fputs ("pos ", stderr);
  for (i = 0; i < bidi_cache_idx; i++)
    fprintf (stderr, "%*"pI"d", ndigits, bidi_cache[i].charpos);
  fputs ("\n", stderr);
}
