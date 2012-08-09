/* Code for doing intervals.
   Copyright (C) 1993-1995, 1997-1998, 2001-2012  Free Software Foundation, Inc.

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


/* NOTES:

   Have to ensure that we can't put symbol nil on a plist, or some
   functions may work incorrectly.

   An idea:  Have the owner of the tree keep count of splits and/or
   insertion lengths (in intervals), and balance after every N.

   Need to call *_left_hook when buffer is killed.

   Scan for zero-length, or 0-length to see notes about handling
   zero length interval-markers.

   There are comments around about freeing intervals.  It might be
   faster to explicitly free them (put them on the free list) than
   to GC them.

*/


#include <config.h>
#include <setjmp.h>
#include <intprops.h>
#include "lisp.h"
#include "intervals.h"
#include "buffer.h"
#include "puresize.h"
#include "keyboard.h"
#include "keymap.h"

/* Test for membership, allowing for t (actually any non-cons) to mean the
   universal set.  */

#define TMEM(sym, set) (CONSP (set) ? ! NILP (Fmemq (sym, set)) : ! NILP (set))

static Lisp_Object merge_properties_sticky (Lisp_Object, Lisp_Object);
static INTERVAL merge_interval_right (INTERVAL);
static INTERVAL reproduce_tree (INTERVAL, INTERVAL);
static INTERVAL reproduce_tree_obj (INTERVAL, Lisp_Object);

/* Utility functions for intervals.  */


/* Create the root interval of some object, a buffer or string.  */

INTERVAL
create_root_interval (Lisp_Object parent)
{
  INTERVAL new;

  CHECK_IMPURE (parent);

  new = make_interval ();

  if (BUFFERP (parent))
    {
      new->total_length = (BUF_Z (XBUFFER (parent))
			   - BUF_BEG (XBUFFER (parent)));
      CHECK_TOTAL_LENGTH (new);
      BUF_INTERVALS (XBUFFER (parent)) = new;
      new->position = BEG;
    }
  else if (STRINGP (parent))
    {
      new->total_length = SCHARS (parent);
      CHECK_TOTAL_LENGTH (new);
      STRING_SET_INTERVALS (parent, new);
      new->position = 0;
    }

  SET_INTERVAL_OBJECT (new, parent);

  return new;
}

/* Make the interval TARGET have exactly the properties of SOURCE */

void
copy_properties (register INTERVAL source, register INTERVAL target)
{
  if (DEFAULT_INTERVAL_P (source) && DEFAULT_INTERVAL_P (target))
    return;

  COPY_INTERVAL_CACHE (source, target);
  target->plist = Fcopy_sequence (source->plist);
}

/* Merge the properties of interval SOURCE into the properties
   of interval TARGET.  That is to say, each property in SOURCE
   is added to TARGET if TARGET has no such property as yet.  */

static void
merge_properties (register INTERVAL source, register INTERVAL target)
{
  register Lisp_Object o, sym, val;

  if (DEFAULT_INTERVAL_P (source) && DEFAULT_INTERVAL_P (target))
    return;

  MERGE_INTERVAL_CACHE (source, target);

  o = source->plist;
  while (CONSP (o))
    {
      sym = XCAR (o);
      o = XCDR (o);
      CHECK_CONS (o);

      val = target->plist;
      while (CONSP (val) && !EQ (XCAR (val), sym))
	{
	  val = XCDR (val);
	  if (!CONSP (val))
	    break;
	  val = XCDR (val);
	}

      if (NILP (val))
	{
	  val = XCAR (o);
	  target->plist = Fcons (sym, Fcons (val, target->plist));
	}
      o = XCDR (o);
    }
}

/* Return 1 if the two intervals have the same properties,
   0 otherwise.  */

int
intervals_equal (INTERVAL i0, INTERVAL i1)
{
  register Lisp_Object i0_cdr, i0_sym;
  register Lisp_Object i1_cdr, i1_val;

  if (DEFAULT_INTERVAL_P (i0) && DEFAULT_INTERVAL_P (i1))
    return 1;

  if (DEFAULT_INTERVAL_P (i0) || DEFAULT_INTERVAL_P (i1))
    return 0;

  i0_cdr = i0->plist;
  i1_cdr = i1->plist;
  while (CONSP (i0_cdr) && CONSP (i1_cdr))
    {
      i0_sym = XCAR (i0_cdr);
      i0_cdr = XCDR (i0_cdr);
      if (!CONSP (i0_cdr))
	return 0;		/* abort (); */
      i1_val = i1->plist;
      while (CONSP (i1_val) && !EQ (XCAR (i1_val), i0_sym))
	{
	  i1_val = XCDR (i1_val);
	  if (!CONSP (i1_val))
	    return 0;		/* abort (); */
	  i1_val = XCDR (i1_val);
	}

      /* i0 has something i1 doesn't.  */
      if (EQ (i1_val, Qnil))
	return 0;

      /* i0 and i1 both have sym, but it has different values in each.  */
      if (!CONSP (i1_val)
	  || (i1_val = XCDR (i1_val), !CONSP (i1_val))
	  || !EQ (XCAR (i1_val), XCAR (i0_cdr)))
	return 0;

      i0_cdr = XCDR (i0_cdr);

      i1_cdr = XCDR (i1_cdr);
      if (!CONSP (i1_cdr))
	return 0;		/* abort (); */
      i1_cdr = XCDR (i1_cdr);
    }

  /* Lengths of the two plists were equal.  */
  return (NILP (i0_cdr) && NILP (i1_cdr));
}


/* Traverse an interval tree TREE, performing FUNCTION on each node.
   No guarantee is made about the order of traversal.
   Pass FUNCTION two args: an interval, and ARG.  */

void
traverse_intervals_noorder (INTERVAL tree, void (*function) (INTERVAL, Lisp_Object), Lisp_Object arg)
{
  /* Minimize stack usage.  */
  while (!NULL_INTERVAL_P (tree))
    {
      (*function) (tree, arg);
      if (NULL_INTERVAL_P (tree->right))
	tree = tree->left;
      else
	{
	  traverse_intervals_noorder (tree->left, function, arg);
	  tree = tree->right;
	}
    }
}

/* Traverse an interval tree TREE, performing FUNCTION on each node.
   Pass FUNCTION two args: an interval, and ARG.  */

void
traverse_intervals (INTERVAL tree, EMACS_INT position,
		    void (*function) (INTERVAL, Lisp_Object), Lisp_Object arg)
{
  while (!NULL_INTERVAL_P (tree))
    {
      traverse_intervals (tree->left, position, function, arg);
      position += LEFT_TOTAL_LENGTH (tree);
      tree->position = position;
      (*function) (tree, arg);
      position += LENGTH (tree); tree = tree->right;
    }
}

#if 0

static int icount;
static int idepth;
static int zero_length;

/* These functions are temporary, for debugging purposes only.  */

INTERVAL search_interval, found_interval;

void
check_for_interval (INTERVAL i)
{
  if (i == search_interval)
    {
      found_interval = i;
      icount++;
    }
}

INTERVAL
search_for_interval (INTERVAL i, INTERVAL tree)
{
  icount = 0;
  search_interval = i;
  found_interval = NULL_INTERVAL;
  traverse_intervals_noorder (tree, &check_for_interval, Qnil);
  return found_interval;
}

static void
inc_interval_count (INTERVAL i)
{
  icount++;
  if (LENGTH (i) == 0)
    zero_length++;
  if (depth > idepth)
    idepth = depth;
}

int
count_intervals (INTERVAL i)
{
  icount = 0;
  idepth = 0;
  zero_length = 0;
  traverse_intervals_noorder (i, &inc_interval_count, Qnil);

  return icount;
}

static INTERVAL
root_interval (INTERVAL interval)
{
  register INTERVAL i = interval;

  while (! ROOT_INTERVAL_P (i))
    i = INTERVAL_PARENT (i);

  return i;
}
#endif

/* Assuming that a left child exists, perform the following operation:

     A		  B
    / \		 / \
   B       =>       A
  / \		   / \
     c		  c
*/

static inline INTERVAL
rotate_right (INTERVAL interval)
{
  INTERVAL i;
  INTERVAL B = interval->left;
  EMACS_INT old_total = interval->total_length;

  /* Deal with any Parent of A;  make it point to B.  */
  if (! ROOT_INTERVAL_P (interval))
    {
      if (AM_LEFT_CHILD (interval))
	INTERVAL_PARENT (interval)->left = B;
      else
	INTERVAL_PARENT (interval)->right = B;
    }
  COPY_INTERVAL_PARENT (B, interval);

  /* Make B the parent of A */
  i = B->right;
  B->right = interval;
  SET_INTERVAL_PARENT (interval, B);

  /* Make A point to c */
  interval->left = i;
  if (! NULL_INTERVAL_P (i))
    SET_INTERVAL_PARENT (i, interval);

  /* A's total length is decreased by the length of B and its left child.  */
  interval->total_length -= B->total_length - LEFT_TOTAL_LENGTH (interval);
  CHECK_TOTAL_LENGTH (interval);

  /* B must have the same total length of A.  */
  B->total_length = old_total;
  CHECK_TOTAL_LENGTH (B);

  return B;
}

/* Assuming that a right child exists, perform the following operation:

    A               B
   / \	           / \
      B	   =>     A
     / \         / \
    c               c
*/

static inline INTERVAL
rotate_left (INTERVAL interval)
{
  INTERVAL i;
  INTERVAL B = interval->right;
  EMACS_INT old_total = interval->total_length;

  /* Deal with any parent of A;  make it point to B.  */
  if (! ROOT_INTERVAL_P (interval))
    {
      if (AM_LEFT_CHILD (interval))
	INTERVAL_PARENT (interval)->left = B;
      else
	INTERVAL_PARENT (interval)->right = B;
    }
  COPY_INTERVAL_PARENT (B, interval);

  /* Make B the parent of A */
  i = B->left;
  B->left = interval;
  SET_INTERVAL_PARENT (interval, B);

  /* Make A point to c */
  interval->right = i;
  if (! NULL_INTERVAL_P (i))
    SET_INTERVAL_PARENT (i, interval);

  /* A's total length is decreased by the length of B and its right child.  */
  interval->total_length -= B->total_length - RIGHT_TOTAL_LENGTH (interval);
  CHECK_TOTAL_LENGTH (interval);

  /* B must have the same total length of A.  */
  B->total_length = old_total;
  CHECK_TOTAL_LENGTH (B);

  return B;
}

/* Balance an interval tree with the assumption that the subtrees
   themselves are already balanced.  */

static INTERVAL
balance_an_interval (INTERVAL i)
{
  register EMACS_INT old_diff, new_diff;

  while (1)
    {
      old_diff = LEFT_TOTAL_LENGTH (i) - RIGHT_TOTAL_LENGTH (i);
      if (old_diff > 0)
	{
	  /* Since the left child is longer, there must be one.  */
	  new_diff = i->total_length - i->left->total_length
	    + RIGHT_TOTAL_LENGTH (i->left) - LEFT_TOTAL_LENGTH (i->left);
	  if (eabs (new_diff) >= old_diff)
	    break;
	  i = rotate_right (i);
	  balance_an_interval (i->right);
	}
      else if (old_diff < 0)
	{
	  /* Since the right child is longer, there must be one.  */
	  new_diff = i->total_length - i->right->total_length
	    + LEFT_TOTAL_LENGTH (i->right) - RIGHT_TOTAL_LENGTH (i->right);
	  if (eabs (new_diff) >= -old_diff)
	    break;
	  i = rotate_left (i);
	  balance_an_interval (i->left);
	}
      else
	break;
    }
  return i;
}

/* Balance INTERVAL, potentially stuffing it back into its parent
   Lisp Object.  */

static inline INTERVAL
balance_possible_root_interval (register INTERVAL interval)
{
  Lisp_Object parent;
  int have_parent = 0;

  if (!INTERVAL_HAS_OBJECT (interval) && !INTERVAL_HAS_PARENT (interval))
    return interval;

  if (INTERVAL_HAS_OBJECT (interval))
    {
      have_parent = 1;
      GET_INTERVAL_OBJECT (parent, interval);
    }
  interval = balance_an_interval (interval);

  if (have_parent)
    {
      if (BUFFERP (parent))
	BUF_INTERVALS (XBUFFER (parent)) = interval;
      else if (STRINGP (parent))
	STRING_SET_INTERVALS (parent, interval);
    }

  return interval;
}

/* Balance the interval tree TREE.  Balancing is by weight
   (the amount of text).  */

static INTERVAL
balance_intervals_internal (register INTERVAL tree)
{
  /* Balance within each side.  */
  if (tree->left)
    balance_intervals_internal (tree->left);
  if (tree->right)
    balance_intervals_internal (tree->right);
  return balance_an_interval (tree);
}

/* Advertised interface to balance intervals.  */

INTERVAL
balance_intervals (INTERVAL tree)
{
  if (tree == NULL_INTERVAL)
    return NULL_INTERVAL;

  return balance_intervals_internal (tree);
}

/* Split INTERVAL into two pieces, starting the second piece at
   character position OFFSET (counting from 0), relative to INTERVAL.
   INTERVAL becomes the left-hand piece, and the right-hand piece
   (second, lexicographically) is returned.

   The size and position fields of the two intervals are set based upon
   those of the original interval.  The property list of the new interval
   is reset, thus it is up to the caller to do the right thing with the
   result.

   Note that this does not change the position of INTERVAL;  if it is a root,
   it is still a root after this operation.  */

INTERVAL
split_interval_right (INTERVAL interval, EMACS_INT offset)
{
  INTERVAL new = make_interval ();
  EMACS_INT position = interval->position;
  EMACS_INT new_length = LENGTH (interval) - offset;

  new->position = position + offset;
  SET_INTERVAL_PARENT (new, interval);

  if (NULL_RIGHT_CHILD (interval))
    {
      interval->right = new;
      new->total_length = new_length;
      CHECK_TOTAL_LENGTH (new);
    }
  else
    {
      /* Insert the new node between INTERVAL and its right child.  */
      new->right = interval->right;
      SET_INTERVAL_PARENT (interval->right, new);
      interval->right = new;
      new->total_length = new_length + new->right->total_length;
      CHECK_TOTAL_LENGTH (new);
      balance_an_interval (new);
    }

  balance_possible_root_interval (interval);

  return new;
}

/* Split INTERVAL into two pieces, starting the second piece at
   character position OFFSET (counting from 0), relative to INTERVAL.
   INTERVAL becomes the right-hand piece, and the left-hand piece
   (first, lexicographically) is returned.

   The size and position fields of the two intervals are set based upon
   those of the original interval.  The property list of the new interval
   is reset, thus it is up to the caller to do the right thing with the
   result.

   Note that this does not change the position of INTERVAL;  if it is a root,
   it is still a root after this operation.  */

INTERVAL
split_interval_left (INTERVAL interval, EMACS_INT offset)
{
  INTERVAL new = make_interval ();
  EMACS_INT new_length = offset;

  new->position = interval->position;
  interval->position = interval->position + offset;
  SET_INTERVAL_PARENT (new, interval);

  if (NULL_LEFT_CHILD (interval))
    {
      interval->left = new;
      new->total_length = new_length;
      CHECK_TOTAL_LENGTH (new);
    }
  else
    {
      /* Insert the new node between INTERVAL and its left child.  */
      new->left = interval->left;
      SET_INTERVAL_PARENT (new->left, new);
      interval->left = new;
      new->total_length = new_length + new->left->total_length;
      CHECK_TOTAL_LENGTH (new);
      balance_an_interval (new);
    }

  balance_possible_root_interval (interval);

  return new;
}

/* Return the proper position for the first character
   described by the interval tree SOURCE.
   This is 1 if the parent is a buffer,
   0 if the parent is a string or if there is no parent.

   Don't use this function on an interval which is the child
   of another interval!  */

static int
interval_start_pos (INTERVAL source)
{
  Lisp_Object parent;

  if (NULL_INTERVAL_P (source))
    return 0;

  if (! INTERVAL_HAS_OBJECT (source))
    return 0;
  GET_INTERVAL_OBJECT (parent, source);
  if (BUFFERP (parent))
    return BUF_BEG (XBUFFER (parent));
  return 0;
}

/* Find the interval containing text position POSITION in the text
   represented by the interval tree TREE.  POSITION is a buffer
   position (starting from 1) or a string index (starting from 0).
   If POSITION is at the end of the buffer or string,
   return the interval containing the last character.

   The `position' field, which is a cache of an interval's position,
   is updated in the interval found.  Other functions (e.g., next_interval)
   will update this cache based on the result of find_interval.  */

INTERVAL
find_interval (register INTERVAL tree, register EMACS_INT position)
{
  /* The distance from the left edge of the subtree at TREE
                    to POSITION.  */
  register EMACS_INT relative_position;

  if (NULL_INTERVAL_P (tree))
    return NULL_INTERVAL;

  relative_position = position;
  if (INTERVAL_HAS_OBJECT (tree))
    {
      Lisp_Object parent;
      GET_INTERVAL_OBJECT (parent, tree);
      if (BUFFERP (parent))
	relative_position -= BUF_BEG (XBUFFER (parent));
    }

  if (relative_position > TOTAL_LENGTH (tree))
    abort ();			/* Paranoia */

  if (!handling_signal)
    tree = balance_possible_root_interval (tree);

  while (1)
    {
      if (relative_position < LEFT_TOTAL_LENGTH (tree))
	{
	  tree = tree->left;
	}
      else if (! NULL_RIGHT_CHILD (tree)
	       && relative_position >= (TOTAL_LENGTH (tree)
					- RIGHT_TOTAL_LENGTH (tree)))
	{
	  relative_position -= (TOTAL_LENGTH (tree)
				- RIGHT_TOTAL_LENGTH (tree));
	  tree = tree->right;
	}
      else
	{
	  tree->position
	    = (position - relative_position /* left edge of *tree.  */
	       + LEFT_TOTAL_LENGTH (tree)); /* left edge of this interval.  */

	  return tree;
	}
    }
}

/* Find the succeeding interval (lexicographically) to INTERVAL.
   Sets the `position' field based on that of INTERVAL (see
   find_interval).  */

INTERVAL
next_interval (register INTERVAL interval)
{
  register INTERVAL i = interval;
  register EMACS_INT next_position;

  if (NULL_INTERVAL_P (i))
    return NULL_INTERVAL;
  next_position = interval->position + LENGTH (interval);

  if (! NULL_RIGHT_CHILD (i))
    {
      i = i->right;
      while (! NULL_LEFT_CHILD (i))
	i = i->left;

      i->position = next_position;
      return i;
    }

  while (! NULL_PARENT (i))
    {
      if (AM_LEFT_CHILD (i))
	{
	  i = INTERVAL_PARENT (i);
	  i->position = next_position;
	  return i;
	}

      i = INTERVAL_PARENT (i);
    }

  return NULL_INTERVAL;
}

/* Find the preceding interval (lexicographically) to INTERVAL.
   Sets the `position' field based on that of INTERVAL (see
   find_interval).  */

INTERVAL
previous_interval (register INTERVAL interval)
{
  register INTERVAL i;

  if (NULL_INTERVAL_P (interval))
    return NULL_INTERVAL;

  if (! NULL_LEFT_CHILD (interval))
    {
      i = interval->left;
      while (! NULL_RIGHT_CHILD (i))
	i = i->right;

      i->position = interval->position - LENGTH (i);
      return i;
    }

  i = interval;
  while (! NULL_PARENT (i))
    {
      if (AM_RIGHT_CHILD (i))
	{
	  i = INTERVAL_PARENT (i);

	  i->position = interval->position - LENGTH (i);
	  return i;
	}
      i = INTERVAL_PARENT (i);
    }

  return NULL_INTERVAL;
}

/* Find the interval containing POS given some non-NULL INTERVAL
   in the same tree.  Note that we need to update interval->position
   if we go down the tree.
   To speed up the process, we assume that the ->position of
   I and all its parents is already uptodate.  */
INTERVAL
update_interval (register INTERVAL i, EMACS_INT pos)
{
  if (NULL_INTERVAL_P (i))
    return NULL_INTERVAL;

  while (1)
    {
      if (pos < i->position)
	{
	  /* Move left. */
	  if (pos >= i->position - TOTAL_LENGTH (i->left))
	    {
	      i->left->position = i->position - TOTAL_LENGTH (i->left)
		+ LEFT_TOTAL_LENGTH (i->left);
	      i = i->left;		/* Move to the left child */
	    }
	  else if (NULL_PARENT (i))
	    error ("Point before start of properties");
	  else
	      i = INTERVAL_PARENT (i);
	  continue;
	}
      else if (pos >= INTERVAL_LAST_POS (i))
	{
	  /* Move right. */
	  if (pos < INTERVAL_LAST_POS (i) + TOTAL_LENGTH (i->right))
	    {
	      i->right->position = INTERVAL_LAST_POS (i)
	        + LEFT_TOTAL_LENGTH (i->right);
	      i = i->right;		/* Move to the right child */
	    }
	  else if (NULL_PARENT (i))
	    error ("Point %"pI"d after end of properties", pos);
	  else
            i = INTERVAL_PARENT (i);
	  continue;
	}
      else
	return i;
    }
}


#if 0
/* Traverse a path down the interval tree TREE to the interval
   containing POSITION, adjusting all nodes on the path for
   an addition of LENGTH characters.  Insertion between two intervals
   (i.e., point == i->position, where i is second interval) means
   text goes into second interval.

   Modifications are needed to handle the hungry bits -- after simply
   finding the interval at position (don't add length going down),
   if it's the beginning of the interval, get the previous interval
   and check the hungry bits of both.  Then add the length going back up
   to the root.  */

static INTERVAL
adjust_intervals_for_insertion (INTERVAL tree, EMACS_INT position,
				EMACS_INT length)
{
  register EMACS_INT relative_position;
  register INTERVAL this;

  if (TOTAL_LENGTH (tree) == 0)	/* Paranoia */
    abort ();

  /* If inserting at point-max of a buffer, that position
     will be out of range */
  if (position > TOTAL_LENGTH (tree))
    position = TOTAL_LENGTH (tree);
  relative_position = position;
  this = tree;

  while (1)
    {
      if (relative_position <= LEFT_TOTAL_LENGTH (this))
	{
	  this->total_length += length;
	  CHECK_TOTAL_LENGTH (this);
	  this = this->left;
	}
      else if (relative_position > (TOTAL_LENGTH (this)
				    - RIGHT_TOTAL_LENGTH (this)))
	{
	  relative_position -= (TOTAL_LENGTH (this)
				- RIGHT_TOTAL_LENGTH (this));
	  this->total_length += length;
	  CHECK_TOTAL_LENGTH (this);
	  this = this->right;
	}
      else
	{
	  /* If we are to use zero-length intervals as buffer pointers,
	     then this code will have to change.  */
	  this->total_length += length;
	  CHECK_TOTAL_LENGTH (this);
	  this->position = LEFT_TOTAL_LENGTH (this)
	                   + position - relative_position + 1;
	  return tree;
	}
    }
}
#endif

/* Effect an adjustment corresponding to the addition of LENGTH characters
   of text.  Do this by finding the interval containing POSITION in the
   interval tree TREE, and then adjusting all of its ancestors by adding
   LENGTH to them.

   If POSITION is the first character of an interval, meaning that point
   is actually between the two intervals, make the new text belong to
   the interval which is "sticky".

   If both intervals are "sticky", then make them belong to the left-most
   interval.  Another possibility would be to create a new interval for
   this text, and make it have the merged properties of both ends.  */

static INTERVAL
adjust_intervals_for_insertion (INTERVAL tree,
				EMACS_INT position, EMACS_INT length)
{
  register INTERVAL i;
  register INTERVAL temp;
  int eobp = 0;
  Lisp_Object parent;
  EMACS_INT offset;

  if (TOTAL_LENGTH (tree) == 0)	/* Paranoia */
    abort ();

  GET_INTERVAL_OBJECT (parent, tree);
  offset = (BUFFERP (parent) ? BUF_BEG (XBUFFER (parent)) : 0);

  /* If inserting at point-max of a buffer, that position will be out
     of range.  Remember that buffer positions are 1-based.  */
  if (position >= TOTAL_LENGTH (tree) + offset)
    {
      position = TOTAL_LENGTH (tree) + offset;
      eobp = 1;
    }

  i = find_interval (tree, position);

  /* If in middle of an interval which is not sticky either way,
     we must not just give its properties to the insertion.
     So split this interval at the insertion point.

     Originally, the if condition here was this:
	(! (position == i->position || eobp)
	 && END_NONSTICKY_P (i)
	 && FRONT_NONSTICKY_P (i))
     But, these macros are now unreliable because of introduction of
     Vtext_property_default_nonsticky.  So, we always check properties
     one by one if POSITION is in middle of an interval.  */
  if (! (position == i->position || eobp))
    {
      Lisp_Object tail;
      Lisp_Object front, rear;

      tail = i->plist;

      /* Properties font-sticky and rear-nonsticky override
         Vtext_property_default_nonsticky.  So, if they are t, we can
         skip one by one checking of properties.  */
      rear = textget (i->plist, Qrear_nonsticky);
      if (! CONSP (rear) && ! NILP (rear))
	{
	  /* All properties are nonsticky.  We split the interval.  */
	  goto check_done;
	}
      front = textget (i->plist, Qfront_sticky);
      if (! CONSP (front) && ! NILP (front))
	{
	  /* All properties are sticky.  We don't split the interval.  */
	  tail = Qnil;
	  goto check_done;
	}

      /* Does any actual property pose an actual problem?  We break
         the loop if we find a nonsticky property.  */
      for (; CONSP (tail); tail = Fcdr (XCDR (tail)))
	{
	  Lisp_Object prop, tmp;
	  prop = XCAR (tail);

	  /* Is this particular property front-sticky?  */
	  if (CONSP (front) && ! NILP (Fmemq (prop, front)))
	    continue;

	  /* Is this particular property rear-nonsticky?  */
	  if (CONSP (rear) && ! NILP (Fmemq (prop, rear)))
	    break;

	  /* Is this particular property recorded as sticky or
             nonsticky in Vtext_property_default_nonsticky?  */
	  tmp = Fassq (prop, Vtext_property_default_nonsticky);
	  if (CONSP (tmp))
	    {
	      if (NILP (tmp))
		continue;
	      break;
	    }

	  /* By default, a text property is rear-sticky, thus we
	     continue the loop.  */
	}

    check_done:
      /* If any property is a real problem, split the interval.  */
      if (! NILP (tail))
	{
	  temp = split_interval_right (i, position - i->position);
	  copy_properties (i, temp);
	  i = temp;
	}
    }

  /* If we are positioned between intervals, check the stickiness of
     both of them.  We have to do this too, if we are at BEG or Z.  */
  if (position == i->position || eobp)
    {
      register INTERVAL prev;

      if (position == BEG)
	prev = 0;
      else if (eobp)
	{
	  prev = i;
	  i = 0;
	}
      else
	prev = previous_interval (i);

      /* Even if we are positioned between intervals, we default
	 to the left one if it exists.  We extend it now and split
	 off a part later, if stickiness demands it.  */
      for (temp = prev ? prev : i; temp; temp = INTERVAL_PARENT_OR_NULL (temp))
	{
	  temp->total_length += length;
	  CHECK_TOTAL_LENGTH (temp);
	  temp = balance_possible_root_interval (temp);
	}

      /* If at least one interval has sticky properties,
	 we check the stickiness property by property.

	 Originally, the if condition here was this:
		(END_NONSTICKY_P (prev) || FRONT_STICKY_P (i))
	 But, these macros are now unreliable because of introduction
	 of Vtext_property_default_nonsticky.  So, we always have to
	 check stickiness of properties one by one.  If cache of
	 stickiness is implemented in the future, we may be able to
	 use those macros again.  */
      if (1)
	{
	  Lisp_Object pleft, pright;
	  struct interval newi;

	  pleft = NULL_INTERVAL_P (prev) ? Qnil : prev->plist;
	  pright = NULL_INTERVAL_P (i) ? Qnil : i->plist;
	  newi.plist = merge_properties_sticky (pleft, pright);

	  if (! prev) /* i.e. position == BEG */
	    {
	      if (! intervals_equal (i, &newi))
		{
		  i = split_interval_left (i, length);
		  i->plist = newi.plist;
		}
	    }
	  else if (! intervals_equal (prev, &newi))
	    {
	      prev = split_interval_right (prev,
					   position - prev->position);
	      prev->plist = newi.plist;
	      if (! NULL_INTERVAL_P (i)
		  && intervals_equal (prev, i))
		merge_interval_right (prev);
	    }

	  /* We will need to update the cache here later.  */
	}
      else if (! prev && ! NILP (i->plist))
        {
	  /* Just split off a new interval at the left.
	     Since I wasn't front-sticky, the empty plist is ok.  */
	  i = split_interval_left (i, length);
        }
    }

  /* Otherwise just extend the interval.  */
  else
    {
      for (temp = i; temp; temp = INTERVAL_PARENT_OR_NULL (temp))
	{
	  temp->total_length += length;
	  CHECK_TOTAL_LENGTH (temp);
	  temp = balance_possible_root_interval (temp);
	}
    }

  return tree;
}

/* Any property might be front-sticky on the left, rear-sticky on the left,
   front-sticky on the right, or rear-sticky on the right; the 16 combinations
   can be arranged in a matrix with rows denoting the left conditions and
   columns denoting the right conditions:
      _  __  _
_     FR FR FR FR
FR__   0  1  2  3
 _FR   4  5  6  7
FR     8  9  A  B
  FR   C  D  E  F

   left-props  = '(front-sticky (p8 p9 pa pb pc pd pe pf)
		   rear-nonsticky (p4 p5 p6 p7 p8 p9 pa pb)
		   p0 L p1 L p2 L p3 L p4 L p5 L p6 L p7 L
		   p8 L p9 L pa L pb L pc L pd L pe L pf L)
   right-props = '(front-sticky (p2 p3 p6 p7 pa pb pe pf)
		   rear-nonsticky (p1 p2 p5 p6 p9 pa pd pe)
		   p0 R p1 R p2 R p3 R p4 R p5 R p6 R p7 R
		   p8 R p9 R pa R pb R pc R pd R pe R pf R)

   We inherit from whoever has a sticky side facing us.  If both sides
   do (cases 2, 3, E, and F), then we inherit from whichever side has a
   non-nil value for the current property.  If both sides do, then we take
   from the left.

   When we inherit a property, we get its stickiness as well as its value.
   So, when we merge the above two lists, we expect to get this:

   result      = '(front-sticky (p6 p7 pa pb pc pd pe pf)
		   rear-nonsticky (p6 pa)
		   p0 L p1 L p2 L p3 L p6 R p7 R
		   pa R pb R pc L pd L pe L pf L)

   The optimizable special cases are:
       left rear-nonsticky = nil, right front-sticky = nil (inherit left)
       left rear-nonsticky = t,   right front-sticky = t   (inherit right)
       left rear-nonsticky = t,   right front-sticky = nil (inherit none)
*/

static Lisp_Object
merge_properties_sticky (Lisp_Object pleft, Lisp_Object pright)
{
  register Lisp_Object props, front, rear;
  Lisp_Object lfront, lrear, rfront, rrear;
  register Lisp_Object tail1, tail2, sym, lval, rval, cat;
  int use_left, use_right;
  int lpresent;

  props = Qnil;
  front = Qnil;
  rear  = Qnil;
  lfront = textget (pleft, Qfront_sticky);
  lrear  = textget (pleft, Qrear_nonsticky);
  rfront = textget (pright, Qfront_sticky);
  rrear  = textget (pright, Qrear_nonsticky);

  /* Go through each element of PRIGHT.  */
  for (tail1 = pright; CONSP (tail1); tail1 = Fcdr (XCDR (tail1)))
    {
      Lisp_Object tmp;

      sym = XCAR (tail1);

      /* Sticky properties get special treatment.  */
      if (EQ (sym, Qrear_nonsticky) || EQ (sym, Qfront_sticky))
	continue;

      rval = Fcar (XCDR (tail1));
      for (tail2 = pleft; CONSP (tail2); tail2 = Fcdr (XCDR (tail2)))
	if (EQ (sym, XCAR (tail2)))
	  break;

      /* Indicate whether the property is explicitly defined on the left.
	 (We know it is defined explicitly on the right
	 because otherwise we don't get here.)  */
      lpresent = ! NILP (tail2);
      lval = (NILP (tail2) ? Qnil : Fcar (Fcdr (tail2)));

      /* Even if lrear or rfront say nothing about the stickiness of
	 SYM, Vtext_property_default_nonsticky may give default
	 stickiness to SYM.  */
      tmp = Fassq (sym, Vtext_property_default_nonsticky);
      use_left = (lpresent
		  && ! (TMEM (sym, lrear)
			|| (CONSP (tmp) && ! NILP (XCDR (tmp)))));
      use_right = (TMEM (sym, rfront)
		   || (CONSP (tmp) && NILP (XCDR (tmp))));
      if (use_left && use_right)
	{
	  if (NILP (lval))
	    use_left = 0;
	  else if (NILP (rval))
	    use_right = 0;
	}
      if (use_left)
	{
	  /* We build props as (value sym ...) rather than (sym value ...)
	     because we plan to nreverse it when we're done.  */
	  props = Fcons (lval, Fcons (sym, props));
	  if (TMEM (sym, lfront))
	    front = Fcons (sym, front);
	  if (TMEM (sym, lrear))
	    rear = Fcons (sym, rear);
	}
      else if (use_right)
	{
	  props = Fcons (rval, Fcons (sym, props));
	  if (TMEM (sym, rfront))
	    front = Fcons (sym, front);
	  if (TMEM (sym, rrear))
	    rear = Fcons (sym, rear);
	}
    }

  /* Now go through each element of PLEFT.  */
  for (tail2 = pleft; CONSP (tail2); tail2 = Fcdr (XCDR (tail2)))
    {
      Lisp_Object tmp;

      sym = XCAR (tail2);

      /* Sticky properties get special treatment.  */
      if (EQ (sym, Qrear_nonsticky) || EQ (sym, Qfront_sticky))
	continue;

      /* If sym is in PRIGHT, we've already considered it.  */
      for (tail1 = pright; CONSP (tail1); tail1 = Fcdr (XCDR (tail1)))
	if (EQ (sym, XCAR (tail1)))
	  break;
      if (! NILP (tail1))
	continue;

      lval = Fcar (XCDR (tail2));

      /* Even if lrear or rfront say nothing about the stickiness of
	 SYM, Vtext_property_default_nonsticky may give default
	 stickiness to SYM.  */
      tmp = Fassq (sym, Vtext_property_default_nonsticky);

      /* Since rval is known to be nil in this loop, the test simplifies.  */
      if (! (TMEM (sym, lrear) || (CONSP (tmp) && ! NILP (XCDR (tmp)))))
	{
	  props = Fcons (lval, Fcons (sym, props));
	  if (TMEM (sym, lfront))
	    front = Fcons (sym, front);
	}
      else if (TMEM (sym, rfront) || (CONSP (tmp) && NILP (XCDR (tmp))))
	{
	  /* The value is nil, but we still inherit the stickiness
	     from the right.  */
	  front = Fcons (sym, front);
	  if (TMEM (sym, rrear))
	    rear = Fcons (sym, rear);
	}
    }
  props = Fnreverse (props);
  if (! NILP (rear))
    props = Fcons (Qrear_nonsticky, Fcons (Fnreverse (rear), props));

  cat = textget (props, Qcategory);
  if (! NILP (front)
      &&
      /* If we have inherited a front-stick category property that is t,
	 we don't need to set up a detailed one.  */
      ! (! NILP (cat) && SYMBOLP (cat)
	 && EQ (Fget (cat, Qfront_sticky), Qt)))
    props = Fcons (Qfront_sticky, Fcons (Fnreverse (front), props));
  return props;
}


/* Delete a node I from its interval tree by merging its subtrees
   into one subtree which is then returned.  Caller is responsible for
   storing the resulting subtree into its parent.  */

static INTERVAL
delete_node (register INTERVAL i)
{
  register INTERVAL migrate, this;
  register EMACS_INT migrate_amt;

  if (NULL_INTERVAL_P (i->left))
    return i->right;
  if (NULL_INTERVAL_P (i->right))
    return i->left;

  migrate = i->left;
  migrate_amt = i->left->total_length;
  this = i->right;
  this->total_length += migrate_amt;
  while (! NULL_INTERVAL_P (this->left))
    {
      this = this->left;
      this->total_length += migrate_amt;
    }
  CHECK_TOTAL_LENGTH (this);
  this->left = migrate;
  SET_INTERVAL_PARENT (migrate, this);

  return i->right;
}

/* Delete interval I from its tree by calling `delete_node'
   and properly connecting the resultant subtree.

   I is presumed to be empty; that is, no adjustments are made
   for the length of I.  */

static void
delete_interval (register INTERVAL i)
{
  register INTERVAL parent;
  EMACS_INT amt = LENGTH (i);

  if (amt > 0)			/* Only used on zero-length intervals now.  */
    abort ();

  if (ROOT_INTERVAL_P (i))
    {
      Lisp_Object owner;
      GET_INTERVAL_OBJECT (owner, i);
      parent = delete_node (i);
      if (! NULL_INTERVAL_P (parent))
	SET_INTERVAL_OBJECT (parent, owner);

      if (BUFFERP (owner))
	BUF_INTERVALS (XBUFFER (owner)) = parent;
      else if (STRINGP (owner))
	STRING_SET_INTERVALS (owner, parent);
      else
	abort ();

      return;
    }

  parent = INTERVAL_PARENT (i);
  if (AM_LEFT_CHILD (i))
    {
      parent->left = delete_node (i);
      if (! NULL_INTERVAL_P (parent->left))
	SET_INTERVAL_PARENT (parent->left, parent);
    }
  else
    {
      parent->right = delete_node (i);
      if (! NULL_INTERVAL_P (parent->right))
	SET_INTERVAL_PARENT (parent->right, parent);
    }
}

/* Find the interval in TREE corresponding to the relative position
   FROM and delete as much as possible of AMOUNT from that interval.
   Return the amount actually deleted, and if the interval was
   zeroed-out, delete that interval node from the tree.

   Note that FROM is actually origin zero, aka relative to the
   leftmost edge of tree.  This is appropriate since we call ourselves
   recursively on subtrees.

   Do this by recursing down TREE to the interval in question, and
   deleting the appropriate amount of text.  */

static EMACS_INT
interval_deletion_adjustment (register INTERVAL tree, register EMACS_INT from,
			      register EMACS_INT amount)
{
  register EMACS_INT relative_position = from;

  if (NULL_INTERVAL_P (tree))
    return 0;

  /* Left branch.  */
  if (relative_position < LEFT_TOTAL_LENGTH (tree))
    {
      EMACS_INT subtract = interval_deletion_adjustment (tree->left,
							 relative_position,
							 amount);
      tree->total_length -= subtract;
      CHECK_TOTAL_LENGTH (tree);
      return subtract;
    }
  /* Right branch.  */
  else if (relative_position >= (TOTAL_LENGTH (tree)
				 - RIGHT_TOTAL_LENGTH (tree)))
    {
      EMACS_INT subtract;

      relative_position -= (tree->total_length
			    - RIGHT_TOTAL_LENGTH (tree));
      subtract = interval_deletion_adjustment (tree->right,
					       relative_position,
					       amount);
      tree->total_length -= subtract;
      CHECK_TOTAL_LENGTH (tree);
      return subtract;
    }
  /* Here -- this node.  */
  else
    {
      /* How much can we delete from this interval?  */
      EMACS_INT my_amount = ((tree->total_length
			       - RIGHT_TOTAL_LENGTH (tree))
			      - relative_position);

      if (amount > my_amount)
	amount = my_amount;

      tree->total_length -= amount;
      CHECK_TOTAL_LENGTH (tree);
      if (LENGTH (tree) == 0)
	delete_interval (tree);

      return amount;
    }

  /* Never reach here.  */
}

/* Effect the adjustments necessary to the interval tree of BUFFER to
   correspond to the deletion of LENGTH characters from that buffer
   text.  The deletion is effected at position START (which is a
   buffer position, i.e. origin 1).  */

static void
adjust_intervals_for_deletion (struct buffer *buffer,
			       EMACS_INT start, EMACS_INT length)
{
  register EMACS_INT left_to_delete = length;
  register INTERVAL tree = BUF_INTERVALS (buffer);
  Lisp_Object parent;
  EMACS_INT offset;

  GET_INTERVAL_OBJECT (parent, tree);
  offset = (BUFFERP (parent) ? BUF_BEG (XBUFFER (parent)) : 0);

  if (NULL_INTERVAL_P (tree))
    return;

  if (start > offset + TOTAL_LENGTH (tree)
      || start + length > offset + TOTAL_LENGTH (tree))
    abort ();

  if (length == TOTAL_LENGTH (tree))
    {
      BUF_INTERVALS (buffer) = NULL_INTERVAL;
      return;
    }

  if (ONLY_INTERVAL_P (tree))
    {
      tree->total_length -= length;
      CHECK_TOTAL_LENGTH (tree);
      return;
    }

  if (start > offset + TOTAL_LENGTH (tree))
    start = offset + TOTAL_LENGTH (tree);
  while (left_to_delete > 0)
    {
      left_to_delete -= interval_deletion_adjustment (tree, start - offset,
						      left_to_delete);
      tree = BUF_INTERVALS (buffer);
      if (left_to_delete == tree->total_length)
	{
	  BUF_INTERVALS (buffer) = NULL_INTERVAL;
	  return;
	}
    }
}

/* Make the adjustments necessary to the interval tree of BUFFER to
   represent an addition or deletion of LENGTH characters starting
   at position START.  Addition or deletion is indicated by the sign
   of LENGTH.

   The two inline functions (one static) pacify Sun C 5.8, a pre-C99
   compiler that does not allow calling a static function (here,
   adjust_intervals_for_deletion) from a non-static inline function.  */

void
offset_intervals (struct buffer *buffer, EMACS_INT start, EMACS_INT length)
{
  if (NULL_INTERVAL_P (BUF_INTERVALS (buffer)) || length == 0)
    return;

  if (length > 0)
    adjust_intervals_for_insertion (BUF_INTERVALS (buffer), start, length);
  else
    {
      IF_LINT (if (length < - TYPE_MAXIMUM (EMACS_INT)) abort ();)
      adjust_intervals_for_deletion (buffer, start, -length);
    }
}

/* Merge interval I with its lexicographic successor. The resulting
   interval is returned, and has the properties of the original
   successor.  The properties of I are lost.  I is removed from the
   interval tree.

   IMPORTANT:
   The caller must verify that this is not the last (rightmost)
   interval.  */

static INTERVAL
merge_interval_right (register INTERVAL i)
{
  register EMACS_INT absorb = LENGTH (i);
  register INTERVAL successor;

  /* Zero out this interval.  */
  i->total_length -= absorb;
  CHECK_TOTAL_LENGTH (i);

  /* Find the succeeding interval.  */
  if (! NULL_RIGHT_CHILD (i))      /* It's below us.  Add absorb
				      as we descend.  */
    {
      successor = i->right;
      while (! NULL_LEFT_CHILD (successor))
	{
	  successor->total_length += absorb;
	  CHECK_TOTAL_LENGTH (successor);
	  successor = successor->left;
	}

      successor->total_length += absorb;
      CHECK_TOTAL_LENGTH (successor);
      delete_interval (i);
      return successor;
    }

  successor = i;
  while (! NULL_PARENT (successor))	   /* It's above us.  Subtract as
					      we ascend.  */
    {
      if (AM_LEFT_CHILD (successor))
	{
	  successor = INTERVAL_PARENT (successor);
	  delete_interval (i);
	  return successor;
	}

      successor = INTERVAL_PARENT (successor);
      successor->total_length -= absorb;
      CHECK_TOTAL_LENGTH (successor);
    }

  /* This must be the rightmost or last interval and cannot
     be merged right.  The caller should have known.  */
  abort ();
}

/* Merge interval I with its lexicographic predecessor. The resulting
   interval is returned, and has the properties of the original predecessor.
   The properties of I are lost.  Interval node I is removed from the tree.

   IMPORTANT:
   The caller must verify that this is not the first (leftmost) interval.  */

INTERVAL
merge_interval_left (register INTERVAL i)
{
  register EMACS_INT absorb = LENGTH (i);
  register INTERVAL predecessor;

  /* Zero out this interval.  */
  i->total_length -= absorb;
  CHECK_TOTAL_LENGTH (i);

  /* Find the preceding interval.  */
  if (! NULL_LEFT_CHILD (i))	/* It's below us. Go down,
				   adding ABSORB as we go.  */
    {
      predecessor = i->left;
      while (! NULL_RIGHT_CHILD (predecessor))
	{
	  predecessor->total_length += absorb;
	  CHECK_TOTAL_LENGTH (predecessor);
	  predecessor = predecessor->right;
	}

      predecessor->total_length += absorb;
      CHECK_TOTAL_LENGTH (predecessor);
      delete_interval (i);
      return predecessor;
    }

  predecessor = i;
  while (! NULL_PARENT (predecessor))	/* It's above us.  Go up,
				   subtracting ABSORB.  */
    {
      if (AM_RIGHT_CHILD (predecessor))
	{
	  predecessor = INTERVAL_PARENT (predecessor);
	  delete_interval (i);
	  return predecessor;
	}

      predecessor = INTERVAL_PARENT (predecessor);
      predecessor->total_length -= absorb;
      CHECK_TOTAL_LENGTH (predecessor);
    }

  /* This must be the leftmost or first interval and cannot
     be merged left.  The caller should have known.  */
  abort ();
}

/* Make an exact copy of interval tree SOURCE which descends from
   PARENT.  This is done by recursing through SOURCE, copying
   the current interval and its properties, and then adjusting
   the pointers of the copy.  */

static INTERVAL
reproduce_tree (INTERVAL source, INTERVAL parent)
{
  register INTERVAL t = make_interval ();

  memcpy (t, source, INTERVAL_SIZE);
  copy_properties (source, t);
  SET_INTERVAL_PARENT (t, parent);
  if (! NULL_LEFT_CHILD (source))
    t->left = reproduce_tree (source->left, t);
  if (! NULL_RIGHT_CHILD (source))
    t->right = reproduce_tree (source->right, t);

  return t;
}

static INTERVAL
reproduce_tree_obj (INTERVAL source, Lisp_Object parent)
{
  register INTERVAL t = make_interval ();

  memcpy (t, source, INTERVAL_SIZE);
  copy_properties (source, t);
  SET_INTERVAL_OBJECT (t, parent);
  if (! NULL_LEFT_CHILD (source))
    t->left = reproduce_tree (source->left, t);
  if (! NULL_RIGHT_CHILD (source))
    t->right = reproduce_tree (source->right, t);

  return t;
}

#if 0
/* Nobody calls this.  Perhaps it's a vestige of an earlier design.  */

/* Make a new interval of length LENGTH starting at START in the
   group of intervals INTERVALS, which is actually an interval tree.
   Returns the new interval.

   Generate an error if the new positions would overlap an existing
   interval.  */

static INTERVAL
make_new_interval (INTERVAL intervals, EMACS_INT start, EMACS_INT length)
{
  INTERVAL slot;

  slot = find_interval (intervals, start);
  if (start + length > slot->position + LENGTH (slot))
    error ("Interval would overlap");

  if (start == slot->position && length == LENGTH (slot))
    return slot;

  if (slot->position == start)
    {
      /* New right node.  */
      split_interval_right (slot, length);
      return slot;
    }

  if (slot->position + LENGTH (slot) == start + length)
    {
      /* New left node.  */
      split_interval_left (slot, LENGTH (slot) - length);
      return slot;
    }

  /* Convert interval SLOT into three intervals.  */
  split_interval_left (slot, start - slot->position);
  split_interval_right (slot, length);
  return slot;
}
#endif

/* Insert the intervals of SOURCE into BUFFER at POSITION.
   LENGTH is the length of the text in SOURCE.

   The `position' field of the SOURCE intervals is assumed to be
   consistent with its parent; therefore, SOURCE must be an
   interval tree made with copy_interval or must be the whole
   tree of a buffer or a string.

   This is used in insdel.c when inserting Lisp_Strings into the
   buffer.  The text corresponding to SOURCE is already in the buffer
   when this is called.  The intervals of new tree are a copy of those
   belonging to the string being inserted; intervals are never
   shared.

   If the inserted text had no intervals associated, and we don't
   want to inherit the surrounding text's properties, this function
   simply returns -- offset_intervals should handle placing the
   text in the correct interval, depending on the sticky bits.

   If the inserted text had properties (intervals), then there are two
   cases -- either insertion happened in the middle of some interval,
   or between two intervals.

   If the text goes into the middle of an interval, then new
   intervals are created in the middle with only the properties of
   the new text, *unless* the macro MERGE_INSERTIONS is true, in
   which case the new text has the union of its properties and those
   of the text into which it was inserted.

   If the text goes between two intervals, then if neither interval
   had its appropriate sticky property set (front_sticky, rear_sticky),
   the new text has only its properties.  If one of the sticky properties
   is set, then the new text "sticks" to that region and its properties
   depend on merging as above.  If both the preceding and succeeding
   intervals to the new text are "sticky", then the new text retains
   only its properties, as if neither sticky property were set.  Perhaps
   we should consider merging all three sets of properties onto the new
   text...  */

void
graft_intervals_into_buffer (INTERVAL source, EMACS_INT position,
			     EMACS_INT length, struct buffer *buffer,
			     int inherit)
{
  register INTERVAL under, over, this;
  register INTERVAL tree;
  EMACS_INT over_used;

  tree = BUF_INTERVALS (buffer);

  /* If the new text has no properties, then with inheritance it
     becomes part of whatever interval it was inserted into.
     To prevent inheritance, we must clear out the properties
     of the newly inserted text.  */
  if (NULL_INTERVAL_P (source))
    {
      Lisp_Object buf;
      if (!inherit && !NULL_INTERVAL_P (tree) && length > 0)
	{
	  XSETBUFFER (buf, buffer);
	  set_text_properties_1 (make_number (position),
				 make_number (position + length),
				 Qnil, buf, 0);
	}
      if (! NULL_INTERVAL_P (BUF_INTERVALS (buffer)))
	/* Shouldn't be necessary.  --Stef  */
	BUF_INTERVALS (buffer) = balance_an_interval (BUF_INTERVALS (buffer));
      return;
    }

  eassert (length == TOTAL_LENGTH (source));

  if ((BUF_Z (buffer) - BUF_BEG (buffer)) == length)
    {  /* The inserted text constitutes the whole buffer, so
	 simply copy over the interval structure.  */
	  Lisp_Object buf;
	  XSETBUFFER (buf, buffer);
	  BUF_INTERVALS (buffer) = reproduce_tree_obj (source, buf);
      BUF_INTERVALS (buffer)->position = BUF_BEG (buffer);
      eassert (BUF_INTERVALS (buffer)->up_obj == 1);
	  return;
	}
  else if (NULL_INTERVAL_P (tree))
    { /* Create an interval tree in which to place a copy
	 of the intervals of the inserted string.  */
	Lisp_Object buf;
	XSETBUFFER (buf, buffer);
	tree = create_root_interval (buf);
      }
  /* Paranoia -- the text has already been added, so this buffer
     should be of non-zero length.  */
  else if (TOTAL_LENGTH (tree) == 0)
    abort ();

  this = under = find_interval (tree, position);
  if (NULL_INTERVAL_P (under))	/* Paranoia.  */
    abort ();
  over = find_interval (source, interval_start_pos (source));

  /* Here for insertion in the middle of an interval.
     Split off an equivalent interval to the right,
     then don't bother with it any more.  */

  if (position > under->position)
    {
      INTERVAL end_unchanged
	= split_interval_left (this, position - under->position);
      copy_properties (under, end_unchanged);
      under->position = position;
    }
  else
    {
      /* This call may have some effect because previous_interval may
         update `position' fields of intervals.  Thus, don't ignore it
         for the moment.  Someone please tell me the truth (K.Handa).  */
      INTERVAL prev = previous_interval (under);
      (void) prev;
#if 0
      /* But, this code surely has no effect.  And, anyway,
         END_NONSTICKY_P is unreliable now.  */
      if (prev && !END_NONSTICKY_P (prev))
	prev = 0;
#endif /* 0 */
    }

  /* Insertion is now at beginning of UNDER.  */

  /* The inserted text "sticks" to the interval `under',
     which means it gets those properties.
     The properties of under are the result of
     adjust_intervals_for_insertion, so stickiness has
     already been taken care of.  */

  /* OVER is the interval we are copying from next.
     OVER_USED says how many characters' worth of OVER
     have already been copied into target intervals.
     UNDER is the next interval in the target.  */
  over_used = 0;
  while (! NULL_INTERVAL_P (over))
    {
      /* If UNDER is longer than OVER, split it.  */
      if (LENGTH (over) - over_used < LENGTH (under))
	{
	  this = split_interval_left (under, LENGTH (over) - over_used);
	  copy_properties (under, this);
	}
      else
	this = under;

      /* THIS is now the interval to copy or merge into.
	 OVER covers all of it.  */
      if (inherit)
	merge_properties (over, this);
      else
	copy_properties (over, this);

      /* If THIS and OVER end at the same place,
	 advance OVER to a new source interval.  */
      if (LENGTH (this) == LENGTH (over) - over_used)
	{
	  over = next_interval (over);
	  over_used = 0;
	}
      else
	/* Otherwise just record that more of OVER has been used.  */
	over_used += LENGTH (this);

      /* Always advance to a new target interval.  */
      under = next_interval (this);
    }

  if (! NULL_INTERVAL_P (BUF_INTERVALS (buffer)))
    BUF_INTERVALS (buffer) = balance_an_interval (BUF_INTERVALS (buffer));
  return;
}

/* Get the value of property PROP from PLIST,
   which is the plist of an interval.
   We check for direct properties, for categories with property PROP,
   and for PROP appearing on the default-text-properties list.  */

Lisp_Object
textget (Lisp_Object plist, register Lisp_Object prop)
{
  return lookup_char_property (plist, prop, 1);
}

Lisp_Object
lookup_char_property (Lisp_Object plist, register Lisp_Object prop, int textprop)
{
  register Lisp_Object tail, fallback = Qnil;

  for (tail = plist; CONSP (tail); tail = Fcdr (XCDR (tail)))
    {
      register Lisp_Object tem;
      tem = XCAR (tail);
      if (EQ (prop, tem))
	return Fcar (XCDR (tail));
      if (EQ (tem, Qcategory))
	{
	  tem = Fcar (XCDR (tail));
	  if (SYMBOLP (tem))
	    fallback = Fget (tem, prop);
	}
    }

  if (! NILP (fallback))
    return fallback;
  /* Check for alternative properties */
  tail = Fassq (prop, Vchar_property_alias_alist);
  if (! NILP (tail))
    {
      tail = XCDR (tail);
      for (; NILP (fallback) && CONSP (tail); tail = XCDR (tail))
	fallback = Fplist_get (plist, XCAR (tail));
    }

  if (textprop && NILP (fallback) && CONSP (Vdefault_text_properties))
    fallback = Fplist_get (Vdefault_text_properties, prop);
  return fallback;
}


/* Set point in BUFFER "temporarily" to CHARPOS, which corresponds to
   byte position BYTEPOS.  */

void
temp_set_point_both (struct buffer *buffer,
		     EMACS_INT charpos, EMACS_INT bytepos)
{
  /* In a single-byte buffer, the two positions must be equal.  */
  if (BUF_ZV (buffer) == BUF_ZV_BYTE (buffer)
      && charpos != bytepos)
    abort ();

  if (charpos > bytepos)
    abort ();

  if (charpos > BUF_ZV (buffer) || charpos < BUF_BEGV (buffer))
    abort ();

  SET_BUF_PT_BOTH (buffer, charpos, bytepos);
}

/* Set point "temporarily", without checking any text properties.  */

void
temp_set_point (struct buffer *buffer, EMACS_INT charpos)
{
  temp_set_point_both (buffer, charpos,
		       buf_charpos_to_bytepos (buffer, charpos));
}

/* Set point in BUFFER to CHARPOS.  If the target position is
   before an intangible character, move to an ok place.  */

void
set_point (EMACS_INT charpos)
{
  set_point_both (charpos, buf_charpos_to_bytepos (current_buffer, charpos));
}

/* If there's an invisible character at position POS + TEST_OFFS in the
   current buffer, and the invisible property has a `stickiness' such that
   inserting a character at position POS would inherit the property it,
   return POS + ADJ, otherwise return POS.  If TEST_INTANG is non-zero,
   then intangibility is required as well as invisibility.

   TEST_OFFS should be either 0 or -1, and ADJ should be either 1 or -1.

   Note that `stickiness' is determined by overlay marker insertion types,
   if the invisible property comes from an overlay.  */

static EMACS_INT
adjust_for_invis_intang (EMACS_INT pos, EMACS_INT test_offs, EMACS_INT adj,
			 int test_intang)
{
  Lisp_Object invis_propval, invis_overlay;
  Lisp_Object test_pos;

  if ((adj < 0 && pos + adj < BEGV) || (adj > 0 && pos + adj > ZV))
    /* POS + ADJ would be beyond the buffer bounds, so do no adjustment.  */
    return pos;

  test_pos = make_number (pos + test_offs);

  invis_propval
    = get_char_property_and_overlay (test_pos, Qinvisible, Qnil,
				     &invis_overlay);

  if ((!test_intang
       || ! NILP (Fget_char_property (test_pos, Qintangible, Qnil)))
      && TEXT_PROP_MEANS_INVISIBLE (invis_propval)
      /* This next test is true if the invisible property has a stickiness
	 such that an insertion at POS would inherit it.  */
      && (NILP (invis_overlay)
	  /* Invisible property is from a text-property.  */
	  ? (text_property_stickiness (Qinvisible, make_number (pos), Qnil)
	     == (test_offs == 0 ? 1 : -1))
	  /* Invisible property is from an overlay.  */
	  : (test_offs == 0
	     ? XMARKER (OVERLAY_START (invis_overlay))->insertion_type == 0
	     : XMARKER (OVERLAY_END (invis_overlay))->insertion_type == 1)))
    pos += adj;

  return pos;
}

/* Set point in BUFFER to CHARPOS, which corresponds to byte
   position BYTEPOS.  If the target position is
   before an intangible character, move to an ok place.  */

void
set_point_both (EMACS_INT charpos, EMACS_INT bytepos)
{
  register INTERVAL to, from, toprev, fromprev;
  EMACS_INT buffer_point;
  EMACS_INT old_position = PT;
  /* This ensures that we move forward past intangible text when the
     initial position is the same as the destination, in the rare
     instances where this is important, e.g. in line-move-finish
     (simple.el).  */
  int backwards = (charpos < old_position ? 1 : 0);
  int have_overlays;
  EMACS_INT original_position;

  BVAR (current_buffer, point_before_scroll) = Qnil;

  if (charpos == PT)
    return;

  /* In a single-byte buffer, the two positions must be equal.  */
  eassert (ZV != ZV_BYTE || charpos == bytepos);

  /* Check this now, before checking if the buffer has any intervals.
     That way, we can catch conditions which break this sanity check
     whether or not there are intervals in the buffer.  */
  eassert (charpos <= ZV && charpos >= BEGV);

  have_overlays = (current_buffer->overlays_before
		   || current_buffer->overlays_after);

  /* If we have no text properties and overlays,
     then we can do it quickly.  */
  if (NULL_INTERVAL_P (BUF_INTERVALS (current_buffer)) && ! have_overlays)
    {
      temp_set_point_both (current_buffer, charpos, bytepos);
      return;
    }

  /* Set TO to the interval containing the char after CHARPOS,
     and TOPREV to the interval containing the char before CHARPOS.
     Either one may be null.  They may be equal.  */
  to = find_interval (BUF_INTERVALS (current_buffer), charpos);
  if (charpos == BEGV)
    toprev = 0;
  else if (to && to->position == charpos)
    toprev = previous_interval (to);
  else
    toprev = to;

  buffer_point = (PT == ZV ? ZV - 1 : PT);

  /* Set FROM to the interval containing the char after PT,
     and FROMPREV to the interval containing the char before PT.
     Either one may be null.  They may be equal.  */
  /* We could cache this and save time.  */
  from = find_interval (BUF_INTERVALS (current_buffer), buffer_point);
  if (buffer_point == BEGV)
    fromprev = 0;
  else if (from && from->position == PT)
    fromprev = previous_interval (from);
  else if (buffer_point != PT)
    fromprev = from, from = 0;
  else
    fromprev = from;

  /* Moving within an interval.  */
  if (to == from && toprev == fromprev && INTERVAL_VISIBLE_P (to)
      && ! have_overlays)
    {
      temp_set_point_both (current_buffer, charpos, bytepos);
      return;
    }

  original_position = charpos;

  /* If the new position is between two intangible characters
     with the same intangible property value,
     move forward or backward until a change in that property.  */
  if (NILP (Vinhibit_point_motion_hooks)
      && ((! NULL_INTERVAL_P (to) && ! NULL_INTERVAL_P (toprev))
	  || have_overlays)
      /* Intangibility never stops us from positioning at the beginning
	 or end of the buffer, so don't bother checking in that case.  */
      && charpos != BEGV && charpos != ZV)
    {
      Lisp_Object pos;
      Lisp_Object intangible_propval;

      if (backwards)
	{
	  /* If the preceding character is both intangible and invisible,
	     and the invisible property is `rear-sticky', perturb it so
	     that the search starts one character earlier -- this ensures
	     that point can never move to the end of an invisible/
	     intangible/rear-sticky region.  */
	  charpos = adjust_for_invis_intang (charpos, -1, -1, 1);

	  XSETINT (pos, charpos);

	  /* If following char is intangible,
	     skip back over all chars with matching intangible property.  */

	  intangible_propval = Fget_char_property (pos, Qintangible, Qnil);

	  if (! NILP (intangible_propval))
	    {
	      while (XINT (pos) > BEGV
		     && EQ (Fget_char_property (make_number (XINT (pos) - 1),
						Qintangible, Qnil),
			    intangible_propval))
		pos = Fprevious_char_property_change (pos, Qnil);

	      /* Set CHARPOS from POS, and if the final intangible character
		 that we skipped over is also invisible, and the invisible
		 property is `front-sticky', perturb it to be one character
		 earlier -- this ensures that point can never move to the
		 beginning of an invisible/intangible/front-sticky region.  */
	      charpos = adjust_for_invis_intang (XINT (pos), 0, -1, 0);
	    }
	}
      else
	{
	  /* If the following character is both intangible and invisible,
	     and the invisible property is `front-sticky', perturb it so
	     that the search starts one character later -- this ensures
	     that point can never move to the beginning of an
	     invisible/intangible/front-sticky region.  */
	  charpos = adjust_for_invis_intang (charpos, 0, 1, 1);

	  XSETINT (pos, charpos);

	  /* If preceding char is intangible,
	     skip forward over all chars with matching intangible property.  */

	  intangible_propval = Fget_char_property (make_number (charpos - 1),
						   Qintangible, Qnil);

	  if (! NILP (intangible_propval))
	    {
	      while (XINT (pos) < ZV
		     && EQ (Fget_char_property (pos, Qintangible, Qnil),
			    intangible_propval))
		pos = Fnext_char_property_change (pos, Qnil);

	      /* Set CHARPOS from POS, and if the final intangible character
		 that we skipped over is also invisible, and the invisible
		 property is `rear-sticky', perturb it to be one character
		 later -- this ensures that point can never move to the
		 end of an invisible/intangible/rear-sticky region.  */
	      charpos = adjust_for_invis_intang (XINT (pos), -1, 1, 0);
	    }
	}

      bytepos = buf_charpos_to_bytepos (current_buffer, charpos);
    }

  if (charpos != original_position)
    {
      /* Set TO to the interval containing the char after CHARPOS,
	 and TOPREV to the interval containing the char before CHARPOS.
	 Either one may be null.  They may be equal.  */
      to = find_interval (BUF_INTERVALS (current_buffer), charpos);
      if (charpos == BEGV)
	toprev = 0;
      else if (to && to->position == charpos)
	toprev = previous_interval (to);
      else
	toprev = to;
    }

  /* Here TO is the interval after the stopping point
     and TOPREV is the interval before the stopping point.
     One or the other may be null.  */

  temp_set_point_both (current_buffer, charpos, bytepos);

  /* We run point-left and point-entered hooks here, if the
     two intervals are not equivalent.  These hooks take
     (old_point, new_point) as arguments.  */
  if (NILP (Vinhibit_point_motion_hooks)
      && (! intervals_equal (from, to)
	  || ! intervals_equal (fromprev, toprev)))
    {
      Lisp_Object leave_after, leave_before, enter_after, enter_before;

      if (fromprev)
	leave_before = textget (fromprev->plist, Qpoint_left);
      else
	leave_before = Qnil;

      if (from)
	leave_after = textget (from->plist, Qpoint_left);
      else
	leave_after = Qnil;

      if (toprev)
	enter_before = textget (toprev->plist, Qpoint_entered);
      else
	enter_before = Qnil;

      if (to)
	enter_after = textget (to->plist, Qpoint_entered);
      else
	enter_after = Qnil;

      if (! EQ (leave_before, enter_before) && !NILP (leave_before))
      	call2 (leave_before, make_number (old_position),
      	       make_number (charpos));
      if (! EQ (leave_after, enter_after) && !NILP (leave_after))
      	call2 (leave_after, make_number (old_position),
      	       make_number (charpos));

      if (! EQ (enter_before, leave_before) && !NILP (enter_before))
      	call2 (enter_before, make_number (old_position),
      	       make_number (charpos));
      if (! EQ (enter_after, leave_after) && !NILP (enter_after))
      	call2 (enter_after, make_number (old_position),
      	       make_number (charpos));
    }
}

/* Move point to POSITION, unless POSITION is inside an intangible
   segment that reaches all the way to point.  */

void
move_if_not_intangible (EMACS_INT position)
{
  Lisp_Object pos;
  Lisp_Object intangible_propval;

  XSETINT (pos, position);

  if (! NILP (Vinhibit_point_motion_hooks))
    /* If intangible is inhibited, always move point to POSITION.  */
    ;
  else if (PT < position && XINT (pos) < ZV)
    {
      /* We want to move forward, so check the text before POSITION.  */

      intangible_propval = Fget_char_property (pos,
					       Qintangible, Qnil);

      /* If following char is intangible,
	 skip back over all chars with matching intangible property.  */
      if (! NILP (intangible_propval))
	while (XINT (pos) > BEGV
	       && EQ (Fget_char_property (make_number (XINT (pos) - 1),
					  Qintangible, Qnil),
		      intangible_propval))
	  pos = Fprevious_char_property_change (pos, Qnil);
    }
  else if (XINT (pos) > BEGV)
    {
      /* We want to move backward, so check the text after POSITION.  */

      intangible_propval = Fget_char_property (make_number (XINT (pos) - 1),
					       Qintangible, Qnil);

      /* If following char is intangible,
	 skip forward over all chars with matching intangible property.  */
      if (! NILP (intangible_propval))
	while (XINT (pos) < ZV
	       && EQ (Fget_char_property (pos, Qintangible, Qnil),
		      intangible_propval))
	  pos = Fnext_char_property_change (pos, Qnil);

    }
  else if (position < BEGV)
    position = BEGV;
  else if (position > ZV)
    position = ZV;

  /* If the whole stretch between PT and POSITION isn't intangible,
     try moving to POSITION (which means we actually move farther
     if POSITION is inside of intangible text).  */

  if (XINT (pos) != PT)
    SET_PT (position);
}

/* If text at position POS has property PROP, set *VAL to the property
   value, *START and *END to the beginning and end of a region that
   has the same property, and return 1.  Otherwise return 0.

   OBJECT is the string or buffer to look for the property in;
   nil means the current buffer. */

int
get_property_and_range (EMACS_INT pos, Lisp_Object prop, Lisp_Object *val,
			EMACS_INT *start, EMACS_INT *end, Lisp_Object object)
{
  INTERVAL i, prev, next;

  if (NILP (object))
    i = find_interval (BUF_INTERVALS (current_buffer), pos);
  else if (BUFFERP (object))
    i = find_interval (BUF_INTERVALS (XBUFFER (object)), pos);
  else if (STRINGP (object))
    i = find_interval (STRING_INTERVALS (object), pos);
  else
    abort ();

  if (NULL_INTERVAL_P (i) || (i->position + LENGTH (i) <= pos))
    return 0;
  *val = textget (i->plist, prop);
  if (NILP (*val))
    return 0;

  next = i;			/* remember it in advance */
  prev = previous_interval (i);
  while (! NULL_INTERVAL_P (prev)
	 && EQ (*val, textget (prev->plist, prop)))
    i = prev, prev = previous_interval (prev);
  *start = i->position;

  next = next_interval (i);
  while (! NULL_INTERVAL_P (next)
	 && EQ (*val, textget (next->plist, prop)))
    i = next, next = next_interval (next);
  *end = i->position + LENGTH (i);

  return 1;
}

/* Return the proper local keymap TYPE for position POSITION in
   BUFFER; TYPE should be one of `keymap' or `local-map'.  Use the map
   specified by the PROP property, if any.  Otherwise, if TYPE is
   `local-map' use BUFFER's local map.

   POSITION must be in the accessible part of BUFFER.  */

Lisp_Object
get_local_map (register EMACS_INT position, register struct buffer *buffer,
	       Lisp_Object type)
{
  Lisp_Object prop, lispy_position, lispy_buffer;
  EMACS_INT old_begv, old_zv, old_begv_byte, old_zv_byte;

  /* Perhaps we should just change `position' to the limit.  */
  if (position > BUF_ZV (buffer) || position < BUF_BEGV (buffer))
    abort ();

  /* Ignore narrowing, so that a local map continues to be valid even if
     the visible region contains no characters and hence no properties.  */
  old_begv = BUF_BEGV (buffer);
  old_zv = BUF_ZV (buffer);
  old_begv_byte = BUF_BEGV_BYTE (buffer);
  old_zv_byte = BUF_ZV_BYTE (buffer);

  SET_BUF_BEGV_BOTH (buffer, BUF_BEG (buffer), BUF_BEG_BYTE (buffer));
  SET_BUF_ZV_BOTH (buffer, BUF_Z (buffer), BUF_Z_BYTE (buffer));

  XSETFASTINT (lispy_position, position);
  XSETBUFFER (lispy_buffer, buffer);
  /* First check if the CHAR has any property.  This is because when
     we click with the mouse, the mouse pointer is really pointing
     to the CHAR after POS.  */
  prop = Fget_char_property (lispy_position, type, lispy_buffer);
  /* If not, look at the POS's properties.  This is necessary because when
     editing a field with a `local-map' property, we want insertion at the end
     to obey the `local-map' property.  */
  if (NILP (prop))
    prop = get_pos_property (lispy_position, type, lispy_buffer);

  SET_BUF_BEGV_BOTH (buffer, old_begv, old_begv_byte);
  SET_BUF_ZV_BOTH (buffer, old_zv, old_zv_byte);

  /* Use the local map only if it is valid.  */
  prop = get_keymap (prop, 0, 0);
  if (CONSP (prop))
    return prop;

  if (EQ (type, Qkeymap))
    return Qnil;
  else
    return BVAR (buffer, keymap);
}

/* Produce an interval tree reflecting the intervals in
   TREE from START to START + LENGTH.
   The new interval tree has no parent and has a starting-position of 0.  */

INTERVAL
copy_intervals (INTERVAL tree, EMACS_INT start, EMACS_INT length)
{
  register INTERVAL i, new, t;
  register EMACS_INT got, prevlen;

  if (NULL_INTERVAL_P (tree) || length <= 0)
    return NULL_INTERVAL;

  i = find_interval (tree, start);
  if (NULL_INTERVAL_P (i) || LENGTH (i) == 0)
    abort ();

  /* If there is only one interval and it's the default, return nil.  */
  if ((start - i->position + 1 + length) < LENGTH (i)
      && DEFAULT_INTERVAL_P (i))
    return NULL_INTERVAL;

  new = make_interval ();
  new->position = 0;
  got = (LENGTH (i) - (start - i->position));
  new->total_length = length;
  CHECK_TOTAL_LENGTH (new);
  copy_properties (i, new);

  t = new;
  prevlen = got;
  while (got < length)
    {
      i = next_interval (i);
      t = split_interval_right (t, prevlen);
      copy_properties (i, t);
      prevlen = LENGTH (i);
      got += prevlen;
    }

  return balance_an_interval (new);
}

/* Give STRING the properties of BUFFER from POSITION to LENGTH.  */

void
copy_intervals_to_string (Lisp_Object string, struct buffer *buffer,
			  EMACS_INT position, EMACS_INT length)
{
  INTERVAL interval_copy = copy_intervals (BUF_INTERVALS (buffer),
					   position, length);
  if (NULL_INTERVAL_P (interval_copy))
    return;

  SET_INTERVAL_OBJECT (interval_copy, string);
  STRING_SET_INTERVALS (string, interval_copy);
}

/* Return 1 if strings S1 and S2 have identical properties; 0 otherwise.
   Assume they have identical characters.  */

int
compare_string_intervals (Lisp_Object s1, Lisp_Object s2)
{
  INTERVAL i1, i2;
  EMACS_INT pos = 0;
  EMACS_INT end = SCHARS (s1);

  i1 = find_interval (STRING_INTERVALS (s1), 0);
  i2 = find_interval (STRING_INTERVALS (s2), 0);

  while (pos < end)
    {
      /* Determine how far we can go before we reach the end of I1 or I2.  */
      EMACS_INT len1 = (i1 != 0 ? INTERVAL_LAST_POS (i1) : end) - pos;
      EMACS_INT len2 = (i2 != 0 ? INTERVAL_LAST_POS (i2) : end) - pos;
      EMACS_INT distance = min (len1, len2);

      /* If we ever find a mismatch between the strings,
	 they differ.  */
      if (! intervals_equal (i1, i2))
	return 0;

      /* Advance POS till the end of the shorter interval,
	 and advance one or both interval pointers for the new position.  */
      pos += distance;
      if (len1 == distance)
	i1 = next_interval (i1);
      if (len2 == distance)
	i2 = next_interval (i2);
    }
  return 1;
}

/* Recursively adjust interval I in the current buffer
   for setting enable_multibyte_characters to MULTI_FLAG.
   The range of interval I is START ... END in characters,
   START_BYTE ... END_BYTE in bytes.  */

static void
set_intervals_multibyte_1 (INTERVAL i, int multi_flag,
			   EMACS_INT start, EMACS_INT start_byte,
			   EMACS_INT end, EMACS_INT end_byte)
{
  /* Fix the length of this interval.  */
  if (multi_flag)
    i->total_length = end - start;
  else
    i->total_length = end_byte - start_byte;
  CHECK_TOTAL_LENGTH (i);

  if (TOTAL_LENGTH (i) == 0)
    {
      delete_interval (i);
      return;
    }

  /* Recursively fix the length of the subintervals.  */
  if (i->left)
    {
      EMACS_INT left_end, left_end_byte;

      if (multi_flag)
	{
	  EMACS_INT temp;
	  left_end_byte = start_byte + LEFT_TOTAL_LENGTH (i);
	  left_end = BYTE_TO_CHAR (left_end_byte);

	  temp = CHAR_TO_BYTE (left_end);

	  /* If LEFT_END_BYTE is in the middle of a character,
	     adjust it and LEFT_END to a char boundary.  */
	  if (left_end_byte > temp)
	    {
	      left_end_byte = temp;
	    }
	  if (left_end_byte < temp)
	    {
	      left_end--;
	      left_end_byte = CHAR_TO_BYTE (left_end);
	    }
	}
      else
	{
	  left_end = start + LEFT_TOTAL_LENGTH (i);
	  left_end_byte = CHAR_TO_BYTE (left_end);
	}

      set_intervals_multibyte_1 (i->left, multi_flag, start, start_byte,
				 left_end, left_end_byte);
    }
  if (i->right)
    {
      EMACS_INT right_start_byte, right_start;

      if (multi_flag)
	{
	  EMACS_INT temp;

	  right_start_byte = end_byte - RIGHT_TOTAL_LENGTH (i);
	  right_start = BYTE_TO_CHAR (right_start_byte);

	  /* If RIGHT_START_BYTE is in the middle of a character,
	     adjust it and RIGHT_START to a char boundary.  */
	  temp = CHAR_TO_BYTE (right_start);

	  if (right_start_byte < temp)
	    {
	      right_start_byte = temp;
	    }
	  if (right_start_byte > temp)
	    {
	      right_start++;
	      right_start_byte = CHAR_TO_BYTE (right_start);
	    }
	}
      else
	{
	  right_start = end - RIGHT_TOTAL_LENGTH (i);
	  right_start_byte = CHAR_TO_BYTE (right_start);
	}

      set_intervals_multibyte_1 (i->right, multi_flag,
				 right_start, right_start_byte,
				 end, end_byte);
    }

  /* Rounding to char boundaries can theoretically ake this interval
     spurious.  If so, delete one child, and copy its property list
     to this interval.  */
  if (LEFT_TOTAL_LENGTH (i) + RIGHT_TOTAL_LENGTH (i) >= TOTAL_LENGTH (i))
    {
      if ((i)->left)
	{
	  (i)->plist = (i)->left->plist;
	  (i)->left->total_length = 0;
	  delete_interval ((i)->left);
	}
      else
	{
	  (i)->plist = (i)->right->plist;
	  (i)->right->total_length = 0;
	  delete_interval ((i)->right);
	}
    }
}

/* Update the intervals of the current buffer
   to fit the contents as multibyte (if MULTI_FLAG is 1)
   or to fit them as non-multibyte (if MULTI_FLAG is 0).  */

void
set_intervals_multibyte (int multi_flag)
{
  if (BUF_INTERVALS (current_buffer))
    set_intervals_multibyte_1 (BUF_INTERVALS (current_buffer), multi_flag,
			       BEG, BEG_BYTE, Z, Z_BYTE);
}
