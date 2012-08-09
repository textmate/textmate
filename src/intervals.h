/* Definitions and global variables for intervals.
   Copyright (C) 1993-1994, 2000-2012  Free Software Foundation, Inc.

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

#include "dispextern.h"

#define NULL_INTERVAL ((INTERVAL)0)
#define INTERVAL_DEFAULT NULL_INTERVAL

/* Basic data type for use of intervals.  */

struct interval
{
  /* The first group of entries deal with the tree structure.  */

  EMACS_INT total_length;       /* Length of myself and both children.  */
  EMACS_INT position;	        /* Cache of interval's character position.  */
				/* This field is usually updated
				   simultaneously with an interval
				   traversal, there is no guarantee
				   that it is valid for a random
				   interval.  */
  struct interval *left;	/* Intervals which precede me.  */
  struct interval *right;	/* Intervals which succeed me.  */

  /* Parent in the tree, or the Lisp_Object containing this interval tree.  */
  union
  {
    struct interval *interval;
    Lisp_Object obj;
  } up;
  unsigned int up_obj : 1;

  unsigned gcmarkbit : 1;

  /* The remaining components are `properties' of the interval.
     The first four are duplicates for things which can be on the list,
     for purposes of speed.  */

  unsigned int write_protect : 1;   /* Non-zero means can't modify.  */
  unsigned int visible : 1;	    /* Zero means don't display.  */
  unsigned int front_sticky : 1;    /* Non-zero means text inserted just
				       before this interval goes into it.  */
  unsigned int rear_sticky : 1;	    /* Likewise for just after it.  */

  /* Properties of this interval.
     The mark bit on this field says whether this particular interval
     tree node has been visited.  Since intervals should never be
     shared, GC aborts if it seems to have visited an interval twice.  */
  Lisp_Object plist;
};

/* These are macros for dealing with the interval tree.  */

/* Size of the structure used to represent an interval.  */
#define INTERVAL_SIZE (sizeof (struct interval))

/* Size of a pointer to an interval structure.  */
#define INTERVAL_PTR_SIZE (sizeof (struct interval *))

#define NULL_INTERVAL_P(i) ((i) == NULL_INTERVAL)

/* True if this interval has no right child.  */
#define NULL_RIGHT_CHILD(i) ((i)->right == NULL_INTERVAL)

/* True if this interval has no left child.  */
#define NULL_LEFT_CHILD(i) ((i)->left == NULL_INTERVAL)

/* True if this interval has no parent.  */
#define NULL_PARENT(i) ((i)->up_obj || (i)->up.interval == 0)

/* True if this interval is the left child of some other interval.  */
#define AM_LEFT_CHILD(i) (! NULL_PARENT (i) \
			  && INTERVAL_PARENT (i)->left == (i))

/* True if this interval is the right child of some other interval.  */
#define AM_RIGHT_CHILD(i) (! NULL_PARENT (i) \
			   && INTERVAL_PARENT (i)->right == (i))

/* True if this interval has no children.  */
#define LEAF_INTERVAL_P(i) ((i)->left == NULL_INTERVAL \
			    && (i)->right == NULL_INTERVAL)

/* True if this interval has no parent and is therefore the root.  */
#define ROOT_INTERVAL_P(i) (NULL_PARENT (i))

/* True if this interval is the only interval in the interval tree.  */
#define ONLY_INTERVAL_P(i) (ROOT_INTERVAL_P ((i)) && LEAF_INTERVAL_P ((i)))

/* True if this interval has both left and right children.  */
#define BOTH_KIDS_P(i) ((i)->left != NULL_INTERVAL     \
			&& (i)->right != NULL_INTERVAL)

/* The total size of all text represented by this interval and all its
   children in the tree.   This is zero if the interval is null.  */
#define TOTAL_LENGTH(i) ((i) == NULL_INTERVAL ? 0 : (i)->total_length)

/* The size of text represented by this interval alone.  */
#define LENGTH(i) ((i) == NULL_INTERVAL ? 0 : (TOTAL_LENGTH ((i))          \
					       - TOTAL_LENGTH ((i)->right) \
					       - TOTAL_LENGTH ((i)->left)))

/* The position of the character just past the end of I.  Note that
   the position cache i->position must be valid for this to work.  */
#define INTERVAL_LAST_POS(i) ((i)->position + LENGTH ((i)))

/* The total size of the left subtree of this interval.  */
#define LEFT_TOTAL_LENGTH(i) ((i)->left ? (i)->left->total_length : 0)

/* The total size of the right subtree of this interval.  */
#define RIGHT_TOTAL_LENGTH(i) ((i)->right ? (i)->right->total_length : 0)


/* These macros are for dealing with the interval properties.  */

/* True if this is a default interval, which is the same as being null
   or having no properties.  */
#define DEFAULT_INTERVAL_P(i) (NULL_INTERVAL_P (i) || EQ ((i)->plist, Qnil))

/* Test what type of parent we have.  Three possibilities: another
   interval, a buffer or string object, or NULL_INTERVAL.  */
#define INTERVAL_HAS_PARENT(i) ((i)->up_obj == 0 && (i)->up.interval != 0)
#define INTERVAL_HAS_OBJECT(i) ((i)->up_obj)

/* Set/get parent of an interval.

   The choice of macros is dependent on the type needed.  Don't add
   casts to get around this, it will break some development work in
   progress.  */
#define SET_INTERVAL_PARENT(i,p) \
   ((i)->up_obj = 0, (i)->up.interval = (p))
#define SET_INTERVAL_OBJECT(i,o) \
   (eassert (BUFFERP (o) || STRINGP (o)), (i)->up_obj = 1, (i)->up.obj = (o))
#define INTERVAL_PARENT(i) \
   (eassert ((i) != 0 && (i)->up_obj == 0),(i)->up.interval)
#define GET_INTERVAL_OBJECT(d,s) (eassert((s)->up_obj == 1), (d) = (s)->up.obj)

/* Make the parent of D be whatever the parent of S is, regardless of
   type.  This is used when balancing an interval tree.  */
#define COPY_INTERVAL_PARENT(d,s) \
   ((d)->up = (s)->up, (d)->up_obj = (s)->up_obj)

/* Get the parent interval, if any, otherwise a null pointer.  Useful
   for walking up to the root in a "for" loop; use this to get the
   "next" value, and test the result to see if it's NULL_INTERVAL.  */
#define INTERVAL_PARENT_OR_NULL(i) \
   (INTERVAL_HAS_PARENT (i) ? INTERVAL_PARENT (i) : 0)

/* Abort if interval I's size is negative.  */
#define CHECK_TOTAL_LENGTH(i)		     \
  do					     \
    {					     \
      if ((i)->total_length < 0)	     \
	abort ();			     \
    }					     \
  while (0)

/* Reset this interval to its vanilla, or no-property state.  */
#define RESET_INTERVAL(i) \
{ \
    (i)->total_length = (i)->position = 0;    \
    (i)->left = (i)->right = NULL_INTERVAL;   \
    SET_INTERVAL_PARENT (i, NULL_INTERVAL);   \
    (i)->write_protect = 0;                   \
    (i)->visible = 0;                         \
    (i)->front_sticky = (i)->rear_sticky = 0; \
    (i)->plist = Qnil;         	              \
}

/* Copy the cached property values of interval FROM to interval TO.  */
#define COPY_INTERVAL_CACHE(from,to) \
{ \
  (to)->write_protect = (from)->write_protect; \
  (to)->visible = (from)->visible;             \
  (to)->front_sticky = (from)->front_sticky;   \
  (to)->rear_sticky = (from)->rear_sticky;     \
}

/* Copy only the set bits of FROM's cache.  */
#define MERGE_INTERVAL_CACHE(from,to) \
{ \
  if ((from)->write_protect) (to)->write_protect = 1; \
  if ((from)->visible) (to)->visible = 1;             \
  if ((from)->front_sticky) (to)->front_sticky = 1;   \
  if ((from)->rear_sticky) (to)->rear_sticky = 1;     \
}

/* Macro determining whether the properties of an interval being
   inserted should be merged with the properties of the text where
   they are being inserted.  */
#define MERGE_INSERTIONS(i) 1

/* Macro determining if an invisible interval should be displayed
   as a special glyph, or not at all.  */
#define DISPLAY_INVISIBLE_GLYPH(i) 0

/* Is this interval visible?  Replace later with cache access.  */
#define INTERVAL_VISIBLE_P(i) \
  (! NULL_INTERVAL_P (i) && NILP (textget ((i)->plist, Qinvisible)))

/* Is this interval writable?  Replace later with cache access.  */
#define INTERVAL_WRITABLE_P(i)					\
  (! NULL_INTERVAL_P (i)					\
   && (NILP (textget ((i)->plist, Qread_only))			\
       || ((CONSP (Vinhibit_read_only)				\
	    ? !NILP (Fmemq (textget ((i)->plist, Qread_only),	\
			    Vinhibit_read_only))		\
	    : !NILP (Vinhibit_read_only)))))			\

/* Macros to tell whether insertions before or after this interval
   should stick to it.  */
/* Replace later with cache access */
/*#define FRONT_STICKY_P(i) ((i)->front_sticky != 0)
  #define END_STICKY_P(i) ((i)->rear_sticky != 0)*/
/* As we now have Vtext_property_default_nonsticky, these macros are
   unreliable now.  Currently, they are never used.  */
#define FRONT_STICKY_P(i) \
  (! NULL_INTERVAL_P (i) && ! NILP (textget ((i)->plist, Qfront_sticky)))
#define END_NONSTICKY_P(i) \
  (! NULL_INTERVAL_P (i) && ! NILP (textget ((i)->plist, Qrear_nonsticky)))
#define FRONT_NONSTICKY_P(i) \
  (! NULL_INTERVAL_P (i) && ! EQ (Qt, textget ((i)->plist, Qfront_sticky)))


/* If PROP is the `invisible' property of a character,
   this is 1 if the character should be treated as invisible,
   and 2 if it is invisible but with an ellipsis.  */

#define TEXT_PROP_MEANS_INVISIBLE(prop)				\
  (EQ (BVAR (current_buffer, invisibility_spec), Qt)			\
   ? !NILP (prop)						\
   : invisible_p (prop, BVAR (current_buffer, invisibility_spec)))

/* Declared in alloc.c.  */

extern INTERVAL make_interval (void);

/* Declared in intervals.c.  */

extern INTERVAL create_root_interval (Lisp_Object);
extern void copy_properties (INTERVAL, INTERVAL);
extern int intervals_equal (INTERVAL, INTERVAL);
extern void traverse_intervals (INTERVAL, EMACS_INT,
                                void (*) (INTERVAL, Lisp_Object),
                                Lisp_Object);
extern void traverse_intervals_noorder (INTERVAL,
                                        void (*) (INTERVAL, Lisp_Object),
                                        Lisp_Object);
extern INTERVAL split_interval_right (INTERVAL, EMACS_INT);
extern INTERVAL split_interval_left (INTERVAL, EMACS_INT);
extern INTERVAL find_interval (INTERVAL, EMACS_INT);
extern INTERVAL next_interval (INTERVAL);
extern INTERVAL previous_interval (INTERVAL);
extern INTERVAL merge_interval_left (INTERVAL);
extern void offset_intervals (struct buffer *, EMACS_INT, EMACS_INT);
extern void graft_intervals_into_buffer (INTERVAL, EMACS_INT, EMACS_INT,
                                         struct buffer *, int);
extern void verify_interval_modification (struct buffer *,
					  EMACS_INT, EMACS_INT);
extern INTERVAL balance_intervals (INTERVAL);
extern void copy_intervals_to_string (Lisp_Object, struct buffer *,
                                             EMACS_INT, EMACS_INT);
extern INTERVAL copy_intervals (INTERVAL, EMACS_INT, EMACS_INT);
extern int compare_string_intervals (Lisp_Object, Lisp_Object);
extern Lisp_Object textget (Lisp_Object, Lisp_Object);
extern Lisp_Object lookup_char_property (Lisp_Object, Lisp_Object, int);
extern void move_if_not_intangible (EMACS_INT);
extern int get_property_and_range (EMACS_INT, Lisp_Object, Lisp_Object *,
                                   EMACS_INT *, EMACS_INT *, Lisp_Object);
extern Lisp_Object get_local_map (EMACS_INT, struct buffer *, Lisp_Object);
extern INTERVAL update_interval (INTERVAL, EMACS_INT);
extern void set_intervals_multibyte (int);
extern INTERVAL validate_interval_range (Lisp_Object, Lisp_Object *,
                                         Lisp_Object *, int);
extern INTERVAL interval_of (EMACS_INT, Lisp_Object);

/* Defined in xdisp.c.  */
extern int invisible_p (Lisp_Object, Lisp_Object);

/* Declared in textprop.c.  */

/* Types of hooks.  */
extern Lisp_Object Qpoint_left;
extern Lisp_Object Qpoint_entered;
extern Lisp_Object Qmodification_hooks;
extern Lisp_Object Qcategory;
extern Lisp_Object Qlocal_map;
extern Lisp_Object Qkeymap;

/* Visual properties text (including strings) may have.  */
extern Lisp_Object Qfont;
extern Lisp_Object Qinvisible, Qintangible;

/* Sticky properties.  */
extern Lisp_Object Qfront_sticky, Qrear_nonsticky;

EXFUN (Fget_char_property, 3);
EXFUN (Fget_text_property, 3);
EXFUN (Ftext_properties_at, 2);
EXFUN (Fnext_property_change, 3);
EXFUN (Fadd_text_properties, 4);
EXFUN (Fset_text_properties, 4);
EXFUN (Fremove_text_properties, 4);
EXFUN (Fremove_list_of_text_properties, 4);
EXFUN (Ftext_property_any, 5);
EXFUN (Fprevious_single_char_property_change, 4);
extern Lisp_Object copy_text_properties (Lisp_Object, Lisp_Object,
                                         Lisp_Object, Lisp_Object,
                                         Lisp_Object, Lisp_Object);
extern Lisp_Object set_text_properties (Lisp_Object, Lisp_Object,
                                        Lisp_Object, Lisp_Object,
                                        Lisp_Object);
extern void set_text_properties_1 (Lisp_Object, Lisp_Object,
                                   Lisp_Object, Lisp_Object, INTERVAL);

Lisp_Object text_property_list (Lisp_Object, Lisp_Object, Lisp_Object,
                                Lisp_Object);
int add_text_properties_from_list (Lisp_Object, Lisp_Object, Lisp_Object);
Lisp_Object extend_property_ranges (Lisp_Object, Lisp_Object);
Lisp_Object get_char_property_and_overlay (Lisp_Object, Lisp_Object,
                                           Lisp_Object, Lisp_Object*);
extern int text_property_stickiness (Lisp_Object prop, Lisp_Object pos,
                                     Lisp_Object buffer);
extern Lisp_Object get_pos_property (Lisp_Object pos, Lisp_Object prop,
                                     Lisp_Object object);

extern void syms_of_textprop (void);

#include "composite.h"
