/* Composite sequence support.
   Copyright (C) 2001-2012 Free Software Foundation, Inc.
   Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011
     National Institute of Advanced Industrial Science and Technology (AIST)
     Registration Number H14PRO021
   Copyright (C) 2003, 2006
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

#include <config.h>
#include <setjmp.h>
#include "lisp.h"
#include "buffer.h"
#include "character.h"
#include "coding.h"
#include "intervals.h"
#include "window.h"
#include "frame.h"
#include "dispextern.h"
#include "font.h"
#include "termhooks.h"


/* Emacs uses special text property `composition' to support character
   composition.  A sequence of characters that have the same (i.e. eq)
   `composition' property value is treated as a single composite
   sequence (we call it just `composition' here after).  Characters in
   a composition are all composed somehow on the screen.

   The property value has this form when the composition is made:
	((LENGTH . COMPONENTS) . MODIFICATION-FUNC)
   then turns to this form:
	(COMPOSITION-ID . (LENGTH COMPONENTS-VEC . MODIFICATION-FUNC))
   when the composition is registered in composition_hash_table and
   composition_table.  These rather peculiar structures were designed
   to make it easy to distinguish them quickly (we can do that by
   checking only the first element) and to extract LENGTH (from the
   former form) and COMPOSITION-ID (from the latter form).

   We register a composition when it is displayed, or when the width
   is required (for instance, to calculate columns).

   LENGTH -- Length of the composition.  This information is used to
	check the validity of the composition.

   COMPONENTS --  Character, string, vector, list, or nil.

	If it is nil, characters in the text are composed relatively
	according to their metrics in font glyphs.

	If it is a character or a string, the character or characters
	in the string are composed relatively.

	If it is a vector or list of integers, the element is a
	character or an encoded composition rule.  The characters are
	composed according to the rules.  (2N)th elements are
	characters to be composed and (2N+1)th elements are
	composition rules to tell how to compose (2N+2)th element with
	the previously composed 2N glyphs.

   COMPONENTS-VEC -- Vector of integers.  In a relative composition,
	the elements are the characters to be composed.  In a rule-base
	composition, the elements are characters or encoded
	composition rules.

   MODIFICATION-FUNC -- If non nil, it is a function to call when the
	composition gets invalid after a modification in a buffer.  If
	it is nil, a function in `composition-function-table' of the
	first character in the sequence is called.

   COMPOSITION-ID --Identification number of the composition.  It is
	used as an index to composition_table for the composition.

   When Emacs has to display a composition or has to know its
   displaying width, the function get_composition_id is called.  It
   returns COMPOSITION-ID so that the caller can access the
   information about the composition through composition_table.  If a
   COMPOSITION-ID has not yet been assigned to the composition,
   get_composition_id checks the validity of `composition' property,
   and, if valid, assigns a new ID, registers the information in
   composition_hash_table and composition_table, and changes the form
   of the property value.  If the property is invalid,
   get_composition_id returns -1 without changing the property value.

   We use two tables to keep the information about composition;
   composition_hash_table and composition_table.

   The former is a hash table whose keys are COMPONENTS-VECs and
   values are the corresponding COMPOSITION-IDs.  This hash table is
   weak, but as each key (COMPONENTS-VEC) is also kept as a value of the
   `composition' property, it won't be collected as garbage until all
   bits of text that have the same COMPONENTS-VEC are deleted.

   The latter is a table of pointers to `struct composition' indexed
   by COMPOSITION-ID.  This structure keeps the other information (see
   composite.h).

   In general, a text property holds information about individual
   characters.  But, a `composition' property holds information about
   a sequence of characters (in this sense, it is like the `intangible'
   property).  That means that we should not share the property value
   in adjacent compositions -- we can't distinguish them if they have the
   same property.  So, after any changes, we call
   `update_compositions' and change a property of one of adjacent
   compositions to a copy of it.  This function also runs a proper
   composition modification function to make a composition that gets
   invalid by the change valid again.

   As the value of the `composition' property holds information about a
   specific range of text, the value gets invalid if we change the
   text in the range.  We treat the `composition' property as always
   rear-nonsticky (currently by setting default-text-properties to
   (rear-nonsticky (composition))) and we never make properties of
   adjacent compositions identical.  Thus, any such changes make the
   range just shorter.  So, we can check the validity of the `composition'
   property by comparing LENGTH information with the actual length of
   the composition.

*/


Lisp_Object Qcomposition;

/* Table of pointers to the structure `composition' indexed by
   COMPOSITION-ID.  This structure is for storing information about
   each composition except for COMPONENTS-VEC.  */
struct composition **composition_table;

/* The current size of `composition_table'.  */
static ptrdiff_t composition_table_size;

/* Number of compositions currently made. */
ptrdiff_t n_compositions;

/* Hash table for compositions.  The key is COMPONENTS-VEC of
   `composition' property.  The value is the corresponding
   COMPOSITION-ID.  */
Lisp_Object composition_hash_table;

static Lisp_Object Qauto_composed;
static Lisp_Object Qauto_composition_function;
/* Maximum number of characters to look back for
   auto-compositions.  */
#define MAX_AUTO_COMPOSITION_LOOKBACK 3

static Lisp_Object Fcomposition_get_gstring (Lisp_Object, Lisp_Object,
					     Lisp_Object, Lisp_Object);

/* Temporary variable used in macros COMPOSITION_XXX.  */
Lisp_Object composition_temp;


/* Return COMPOSITION-ID of a composition at buffer position
   CHARPOS/BYTEPOS and length NCHARS.  The `composition' property of
   the sequence is PROP.  STRING, if non-nil, is a string that
   contains the composition instead of the current buffer.

   If the composition is invalid, return -1.  */

ptrdiff_t
get_composition_id (EMACS_INT charpos, EMACS_INT bytepos, EMACS_INT nchars,
		    Lisp_Object prop, Lisp_Object string)
{
  Lisp_Object id, length, components, key, *key_contents;
  ptrdiff_t glyph_len;
  struct Lisp_Hash_Table *hash_table = XHASH_TABLE (composition_hash_table);
  ptrdiff_t hash_index;
  EMACS_UINT hash_code;
  enum composition_method method;
  struct composition *cmp;
  EMACS_INT i;
  int ch;

  /* Maximum length of a string of glyphs.  XftGlyphExtents limits
     this to INT_MAX, and Emacs limits it further.  Divide INT_MAX - 1
     by 2 because x_produce_glyphs computes glyph_len * 2 + 1.  Divide
     the size by MAX_MULTIBYTE_LENGTH because encode_terminal_code
     multiplies glyph_len by MAX_MULTIBYTE_LENGTH.  */
  enum {
    GLYPH_LEN_MAX = min ((INT_MAX - 1) / 2,
			 min (PTRDIFF_MAX, SIZE_MAX) / MAX_MULTIBYTE_LENGTH)
  };

  /* PROP should be
	Form-A: ((LENGTH . COMPONENTS) . MODIFICATION-FUNC)
     or
	Form-B: (COMPOSITION-ID . (LENGTH COMPONENTS-VEC . MODIFICATION-FUNC))
  */
  if (nchars == 0 || !CONSP (prop))
    goto invalid_composition;

  id = XCAR (prop);
  if (INTEGERP (id))
    {
      /* PROP should be Form-B.  */
      if (XINT (id) < 0 || XINT (id) >= n_compositions)
	goto invalid_composition;
      return XINT (id);
    }

  /* PROP should be Form-A.
     Thus, ID should be (LENGTH . COMPONENTS).  */
  if (!CONSP (id))
    goto invalid_composition;
  length = XCAR (id);
  if (!INTEGERP (length) || XINT (length) != nchars)
    goto invalid_composition;

  components = XCDR (id);

  /* Check if the same composition has already been registered or not
     by consulting composition_hash_table.  The key for this table is
     COMPONENTS (converted to a vector COMPONENTS-VEC) or, if it is
     nil, vector of characters in the composition range.  */
  if (INTEGERP (components))
    key = Fmake_vector (make_number (1), components);
  else if (STRINGP (components) || CONSP (components))
    key = Fvconcat (1, &components);
  else if (VECTORP (components))
    key = components;
  else if (NILP (components))
    {
      key = Fmake_vector (make_number (nchars), Qnil);
      if (STRINGP (string))
	for (i = 0; i < nchars; i++)
	  {
	    FETCH_STRING_CHAR_ADVANCE (ch, string, charpos, bytepos);
	    XVECTOR (key)->contents[i] = make_number (ch);
	  }
      else
	for (i = 0; i < nchars; i++)
	  {
	    FETCH_CHAR_ADVANCE (ch, charpos, bytepos);
	    XVECTOR (key)->contents[i] = make_number (ch);
	  }
    }
  else
    goto invalid_composition;

  hash_index = hash_lookup (hash_table, key, &hash_code);
  if (hash_index >= 0)
    {
      /* We have already registered the same composition.  Change PROP
	 from Form-A above to Form-B while replacing COMPONENTS with
	 COMPONENTS-VEC stored in the hash table.  We can directly
	 modify the cons cell of PROP because it is not shared.  */
      key = HASH_KEY (hash_table, hash_index);
      id = HASH_VALUE (hash_table, hash_index);
      XSETCAR (prop, id);
      XSETCDR (prop, Fcons (make_number (nchars), Fcons (key, XCDR (prop))));
      return XINT (id);
    }

  /* This composition is a new one.  We must register it.  */

  /* Check if we have sufficient memory to store this information.  */
  if (composition_table_size <= n_compositions)
    composition_table = xpalloc (composition_table, &composition_table_size,
				 1, -1, sizeof *composition_table);

  key_contents = XVECTOR (key)->contents;

  /* Check if the contents of COMPONENTS are valid if COMPONENTS is a
     vector or a list.  It should be a sequence of:
	char1 rule1 char2 rule2 char3 ...    ruleN charN+1  */

  if (VECTORP (components)
      && ASIZE (components) >= 2
      && VECTORP (AREF (components, 0)))
    {
      /* COMPONENTS is a glyph-string.  */
      EMACS_INT len = ASIZE (key);

      for (i = 1; i < len; i++)
	if (! VECTORP (AREF (key, i)))
	  goto invalid_composition;
    }
  else if (VECTORP (components) || CONSP (components))
    {
      EMACS_INT len = ASIZE (key);

      /* The number of elements should be odd.  */
      if ((len % 2) == 0)
	goto invalid_composition;
      /* All elements should be integers (character or encoded
         composition rule).  */
      for (i = 0; i < len; i++)
	{
	  if (!INTEGERP (key_contents[i]))
	    goto invalid_composition;
	}
    }

  /* Change PROP from Form-A above to Form-B.  We can directly modify
     the cons cell of PROP because it is not shared.  */
  XSETFASTINT (id, n_compositions);
  XSETCAR (prop, id);
  XSETCDR (prop, Fcons (make_number (nchars), Fcons (key, XCDR (prop))));

  /* Register the composition in composition_hash_table.  */
  hash_index = hash_put (hash_table, key, id, hash_code);

  method = (NILP (components)
	    ? COMPOSITION_RELATIVE
	    : ((INTEGERP (components) || STRINGP (components))
	       ? COMPOSITION_WITH_ALTCHARS
	       : COMPOSITION_WITH_RULE_ALTCHARS));

  glyph_len = (method == COMPOSITION_WITH_RULE_ALTCHARS
	       ? (ASIZE (key) + 1) / 2
	       : ASIZE (key));

  if (GLYPH_LEN_MAX < glyph_len)
    memory_full (SIZE_MAX);

  /* Register the composition in composition_table.  */
  cmp = (struct composition *) xmalloc (sizeof (struct composition));

  cmp->method = method;
  cmp->hash_index = hash_index;
  cmp->glyph_len = glyph_len;
  cmp->offsets = xnmalloc (glyph_len, 2 * sizeof *cmp->offsets);
  cmp->font = NULL;

  if (cmp->method != COMPOSITION_WITH_RULE_ALTCHARS)
    {
      /* Relative composition.  */
      cmp->width = 0;
      for (i = 0; i < glyph_len; i++)
	{
	  int this_width;
	  ch = XINT (key_contents[i]);
	  /* TAB in a composition means display glyphs with padding
	     space on the left or right.  */
	  this_width = (ch == '\t' ? 1 : CHAR_WIDTH (ch));
	  if (cmp->width < this_width)
	    cmp->width = this_width;
	}
    }
  else
    {
      /* Rule-base composition.  */
      float leftmost = 0.0, rightmost;

      ch = XINT (key_contents[0]);
      rightmost = ch != '\t' ? CHAR_WIDTH (ch) : 1;

      for (i = 1; i < glyph_len; i += 2)
	{
	  int rule, gref, nref;
	  int this_width;
	  float this_left;

	  rule = XINT (key_contents[i]);
	  ch = XINT (key_contents[i + 1]);
	  this_width = ch != '\t' ? CHAR_WIDTH (ch) : 1;

	  /* A composition rule is specified by an integer value
	     that encodes global and new reference points (GREF and
	     NREF).  GREF and NREF are specified by numbers as
	     below:
		0---1---2 -- ascent
		|       |
		|       |
		|       |
		9--10--11 -- center
		|       |
	     ---3---4---5--- baseline
		|       |
		6---7---8 -- descent
	  */
	  COMPOSITION_DECODE_REFS (rule, gref, nref);
	  this_left = (leftmost
		       + (gref % 3) * (rightmost - leftmost) / 2.0
		       - (nref % 3) * this_width / 2.0);

	  if (this_left < leftmost)
	    leftmost = this_left;
	  if (this_left + this_width > rightmost)
	    rightmost = this_left + this_width;
	}

      cmp->width = rightmost - leftmost;
      if (cmp->width < (rightmost - leftmost))
	/* To get a ceiling integer value.  */
	cmp->width++;
    }

  composition_table[n_compositions] = cmp;

  return n_compositions++;

 invalid_composition:
  /* Would it be better to remove this `composition' property?  */
  return -1;
}


/* Find a static composition at or nearest to position POS of OBJECT
   (buffer or string).

   OBJECT defaults to the current buffer.  If there's a composition at
   POS, set *START and *END to the start and end of the sequence,
   *PROP to the `composition' property, and return 1.

   If there's no composition at POS and LIMIT is negative, return 0.

   Otherwise, search for a composition forward (LIMIT > POS) or
   backward (LIMIT < POS).  In this case, LIMIT bounds the search.

   If a composition is found, set *START, *END, and *PROP as above,
   and return 1, else return 0.

   This doesn't check the validity of composition.  */

int
find_composition (EMACS_INT pos, EMACS_INT limit,
		  EMACS_INT *start, EMACS_INT *end,
		  Lisp_Object *prop, Lisp_Object object)
{
  Lisp_Object val;

  if (get_property_and_range (pos, Qcomposition, prop, start, end, object))
    return 1;

  if (limit < 0 || limit == pos)
    return 0;

  if (limit > pos)		/* search forward */
    {
      val = Fnext_single_property_change (make_number (pos), Qcomposition,
					  object, make_number (limit));
      pos = XINT (val);
      if (pos == limit)
	return 0;
    }
  else				/* search backward */
    {
      if (get_property_and_range (pos - 1, Qcomposition, prop, start, end,
				  object))
	return 1;
      val = Fprevious_single_property_change (make_number (pos), Qcomposition,
					      object, make_number (limit));
      pos = XINT (val);
      if (pos == limit)
	return 0;
      pos--;
    }
  get_property_and_range (pos, Qcomposition, prop, start, end, object);
  return 1;
}

/* Run a proper function to adjust the composition sitting between
   FROM and TO with property PROP.  */

static void
run_composition_function (EMACS_INT from, EMACS_INT to, Lisp_Object prop)
{
  Lisp_Object func;
  EMACS_INT start, end;

  func = COMPOSITION_MODIFICATION_FUNC (prop);
  /* If an invalid composition precedes or follows, try to make them
     valid too.  */
  if (from > BEGV
      && find_composition (from - 1, -1, &start, &end, &prop, Qnil)
      && !COMPOSITION_VALID_P (start, end, prop))
    from = start;
  if (to < ZV
      && find_composition (to, -1, &start, &end, &prop, Qnil)
      && !COMPOSITION_VALID_P (start, end, prop))
    to = end;
  if (!NILP (Ffboundp (func)))
    call2 (func, make_number (from), make_number (to));
}

/* Make invalid compositions adjacent to or inside FROM and TO valid.
   CHECK_MASK is bitwise `or' of mask bits defined by macros
   CHECK_XXX (see the comment in composite.h).

   It also resets the text-property `auto-composed' to a proper region
   so that automatic character composition works correctly later while
   displaying the region.

   This function is called when a buffer text is changed.  If the
   change is deletion, FROM == TO.  Otherwise, FROM < TO.  */

void
update_compositions (EMACS_INT from, EMACS_INT to, int check_mask)
{
  Lisp_Object prop;
  EMACS_INT start, end;
  /* The beginning and end of the region to set the property
     `auto-composed' to nil.  */
  EMACS_INT min_pos = from, max_pos = to;

  if (inhibit_modification_hooks)
    return;

  /* If FROM and TO are not in a valid range, do nothing.  */
  if (! (BEGV <= from && from <= to && to <= ZV))
    return;

  if (check_mask & CHECK_HEAD)
    {
      /* FROM should be at composition boundary.  But, insertion or
	 deletion will make two compositions adjacent and
	 indistinguishable when they have same (eq) property.  To
	 avoid it, in such a case, we change the property of the
	 latter to the copy of it.  */
      if (from > BEGV
	  && find_composition (from - 1, -1, &start, &end, &prop, Qnil)
	  && COMPOSITION_VALID_P (start, end, prop))
	{
	  min_pos = start;
	  if (end > to)
	    max_pos = end;
	  if (from < end)
	    Fput_text_property (make_number (from), make_number (end),
				Qcomposition,
				Fcons (XCAR (prop), XCDR (prop)), Qnil);
	  run_composition_function (start, end, prop);
	  from = end;
	}
      else if (from < ZV
	       && find_composition (from, -1, &start, &from, &prop, Qnil)
	       && COMPOSITION_VALID_P (start, from, prop))
	{
	  if (from > to)
	    max_pos = from;
	  run_composition_function (start, from, prop);
	}
    }

  if (check_mask & CHECK_INSIDE)
    {
      /* In this case, we are sure that (check & CHECK_TAIL) is also
         nonzero.  Thus, here we should check only compositions before
         (to - 1).  */
      while (from < to - 1
	     && find_composition (from, to, &start, &from, &prop, Qnil)
	     && COMPOSITION_VALID_P (start, from, prop)
	     && from < to - 1)
	run_composition_function (start, from, prop);
    }

  if (check_mask & CHECK_TAIL)
    {
      if (from < to
	  && find_composition (to - 1, -1, &start, &end, &prop, Qnil)
	  && COMPOSITION_VALID_P (start, end, prop))
	{
	  /* TO should be also at composition boundary.  But,
	     insertion or deletion will make two compositions adjacent
	     and indistinguishable when they have same (eq) property.
	     To avoid it, in such a case, we change the property of
	     the former to the copy of it.  */
	  if (to < end)
	    {
	      Fput_text_property (make_number (start), make_number (to),
				  Qcomposition,
				  Fcons (XCAR (prop), XCDR (prop)), Qnil);
	      max_pos = end;
	    }
	  run_composition_function (start, end, prop);
	}
      else if (to < ZV
	       && find_composition (to, -1, &start, &end, &prop, Qnil)
	       && COMPOSITION_VALID_P (start, end, prop))
	{
	  run_composition_function (start, end, prop);
	  max_pos = end;
	}
    }
  if (min_pos < max_pos)
    {
      int count = SPECPDL_INDEX ();

      specbind (Qinhibit_read_only, Qt);
      specbind (Qinhibit_modification_hooks, Qt);
      specbind (Qinhibit_point_motion_hooks, Qt);
      Fremove_list_of_text_properties (make_number (min_pos),
				       make_number (max_pos),
				       Fcons (Qauto_composed, Qnil), Qnil);
      unbind_to (count, Qnil);
    }
}


/* Modify composition property values in LIST destructively.  LIST is
   a list as returned from text_property_list.  Change values to the
   top-level copies of them so that none of them are `eq'.  */

void
make_composition_value_copy (Lisp_Object list)
{
  Lisp_Object plist, val;

  for (; CONSP (list); list = XCDR (list))
    {
      plist = XCAR (XCDR (XCDR (XCAR (list))));
      while (CONSP (plist) && CONSP (XCDR (plist)))
	{
	  if (EQ (XCAR (plist), Qcomposition)
	      && (val = XCAR (XCDR (plist)), CONSP (val)))
	    XSETCAR (XCDR (plist), Fcons (XCAR (val), XCDR (val)));
	  plist = XCDR (XCDR (plist));
	}
    }
}


/* Make text in the region between START and END a composition that
   has COMPONENTS and MODIFICATION-FUNC.

   If STRING is non-nil, then operate on characters contained between
   indices START and END in STRING.  */

void
compose_text (EMACS_INT start, EMACS_INT end, Lisp_Object components,
	      Lisp_Object modification_func, Lisp_Object string)
{
  Lisp_Object prop;

  prop = Fcons (Fcons (make_number (end - start), components),
		modification_func);
  Fput_text_property  (make_number (start), make_number (end),
		       Qcomposition, prop, string);
}


static Lisp_Object autocmp_chars (Lisp_Object, EMACS_INT, EMACS_INT,
                                  EMACS_INT, struct window *,
                                  struct face *, Lisp_Object);


/* Lisp glyph-string handlers */

/* Hash table for automatic composition.  The key is a header of a
   lgstring (Lispy glyph-string), and the value is a body of a
   lgstring.  */

static Lisp_Object gstring_hash_table;

static Lisp_Object gstring_lookup_cache (Lisp_Object);

static Lisp_Object
gstring_lookup_cache (Lisp_Object header)
{
  struct Lisp_Hash_Table *h = XHASH_TABLE (gstring_hash_table);
  ptrdiff_t i = hash_lookup (h, header, NULL);

  return (i >= 0 ? HASH_VALUE (h, i) : Qnil);
}

Lisp_Object
composition_gstring_put_cache (Lisp_Object gstring, EMACS_INT len)
{
  struct Lisp_Hash_Table *h = XHASH_TABLE (gstring_hash_table);
  EMACS_UINT hash;
  Lisp_Object header, copy;
  EMACS_INT i;

  header = LGSTRING_HEADER (gstring);
  hash = h->hashfn (h, header);
  if (len < 0)
    {
      EMACS_INT j, glyph_len = LGSTRING_GLYPH_LEN (gstring);
      for (j = 0; j < glyph_len; j++)
	if (NILP (LGSTRING_GLYPH (gstring, j)))
	  break;
      len = j;
    }

  lint_assume (len <= TYPE_MAXIMUM (EMACS_INT) - 2);
  copy = Fmake_vector (make_number (len + 2), Qnil);
  LGSTRING_SET_HEADER (copy, Fcopy_sequence (header));
  for (i = 0; i < len; i++)
    LGSTRING_SET_GLYPH (copy, i, Fcopy_sequence (LGSTRING_GLYPH (gstring, i)));
  i = hash_put (h, LGSTRING_HEADER (copy), copy, hash);
  LGSTRING_SET_ID (copy, make_number (i));
  return copy;
}

Lisp_Object
composition_gstring_from_id (ptrdiff_t id)
{
  struct Lisp_Hash_Table *h = XHASH_TABLE (gstring_hash_table);

  return HASH_VALUE (h, id);
}

static Lisp_Object fill_gstring_header (Lisp_Object, Lisp_Object,
                                        Lisp_Object, Lisp_Object,
                                        Lisp_Object);

int
composition_gstring_p (Lisp_Object gstring)
{
  Lisp_Object header;
  EMACS_INT i;

  if (! VECTORP (gstring) || ASIZE (gstring) < 2)
    return 0;
  header = LGSTRING_HEADER (gstring);
  if (! VECTORP (header) || ASIZE (header) < 2)
    return 0;
  if (! NILP (LGSTRING_FONT (gstring))
      && (! FONT_OBJECT_P (LGSTRING_FONT (gstring))
	  && ! CODING_SYSTEM_P (LGSTRING_FONT (gstring))))
    return 0;
  for (i = 1; i < ASIZE (LGSTRING_HEADER (gstring)); i++)
    if (! NATNUMP (AREF (LGSTRING_HEADER (gstring), i)))
      return 0;
  if (! NILP (LGSTRING_ID (gstring)) && ! NATNUMP (LGSTRING_ID (gstring)))
    return 0;
  for (i = 0; i < LGSTRING_GLYPH_LEN (gstring); i++)
    {
      Lisp_Object glyph = LGSTRING_GLYPH (gstring, i);
      if (NILP (glyph))
	break;
      if (! VECTORP (glyph) || ASIZE (glyph) != LGLYPH_SIZE)
	return 0;
    }
  return 1;
}

int
composition_gstring_width (Lisp_Object gstring, EMACS_INT from, EMACS_INT to,
			   struct font_metrics *metrics)
{
  Lisp_Object *glyph;
  int width = 0;

  if (metrics)
    {
      Lisp_Object font_object = LGSTRING_FONT (gstring);

      if (FONT_OBJECT_P (font_object))
	{
	  struct font *font = XFONT_OBJECT (font_object);

	  metrics->ascent = font->ascent;
	  metrics->descent = font->descent;
	}
      else
	{
	  metrics->ascent = 1;
	  metrics->descent = 0;
	}
      metrics->width = metrics->lbearing = metrics->rbearing = 0;
    }
  for (glyph = &LGSTRING_GLYPH (gstring, from); from < to; from++, glyph++)
    {
      int x;

      if (NILP (LGLYPH_ADJUSTMENT (*glyph)))
	width += LGLYPH_WIDTH (*glyph);
      else
	width += LGLYPH_WADJUST (*glyph);
      if (metrics)
	{
	  x = metrics->width + LGLYPH_LBEARING (*glyph) + LGLYPH_XOFF (*glyph);
	  if (metrics->lbearing > x)
	    metrics->lbearing = x;
	  x = metrics->width + LGLYPH_RBEARING (*glyph) + LGLYPH_XOFF (*glyph);
	  if (metrics->rbearing < x)
	    metrics->rbearing = x;
	  metrics->width = width;
	  x = LGLYPH_ASCENT (*glyph) - LGLYPH_YOFF (*glyph);
	  if (metrics->ascent < x)
	    metrics->ascent = x;
	  x = LGLYPH_DESCENT (*glyph) + LGLYPH_YOFF (*glyph);
	  if (metrics->descent < x)
	    metrics->descent = x;
	}
    }
  return width;
}


static Lisp_Object gstring_work;
static Lisp_Object gstring_work_headers;

static Lisp_Object
fill_gstring_header (Lisp_Object header, Lisp_Object start, Lisp_Object end, Lisp_Object font_object, Lisp_Object string)
{
  EMACS_INT from, to, from_byte;
  EMACS_INT len, i;

  if (NILP (string))
    {
      if (NILP (BVAR (current_buffer, enable_multibyte_characters)))
	error ("Attempt to shape unibyte text");
      validate_region (&start, &end);
      from = XFASTINT (start);
      to = XFASTINT (end);
      from_byte = CHAR_TO_BYTE (from);
    }
  else
    {
      CHECK_STRING (string);
      if (! STRING_MULTIBYTE (string))
	error ("Attempt to shape unibyte text");
      /* FROM and TO are checked by the caller.  */
      from = XINT (start);
      to = XINT (end);
      if (from < 0 || from > to || to > SCHARS (string))
	args_out_of_range_3 (string, start, end);
      from_byte = string_char_to_byte (string, from);
    }

  len = to - from;
  if (len == 0)
    error ("Attempt to shape zero-length text");
  if (VECTORP (header))
    {
      if (ASIZE (header) != len + 1)
	args_out_of_range (header, make_number (len + 1));
    }
  else
    {
      if (len <= 8)
	header = AREF (gstring_work_headers, len - 1);
      else
	header = Fmake_vector (make_number (len + 1), Qnil);
    }

  ASET (header, 0, font_object);
  for (i = 0; i < len; i++)
    {
      int c;

      if (NILP (string))
	FETCH_CHAR_ADVANCE_NO_CHECK (c, from, from_byte);
      else
	FETCH_STRING_CHAR_ADVANCE_NO_CHECK (c, string, from, from_byte);
      ASET (header, i + 1, make_number (c));
    }
  return header;
}

static void
fill_gstring_body (Lisp_Object gstring)
{
  Lisp_Object font_object = LGSTRING_FONT (gstring);
  Lisp_Object header = AREF (gstring, 0);
  EMACS_INT len = LGSTRING_CHAR_LEN (gstring);
  EMACS_INT i;

  for (i = 0; i < len; i++)
    {
      Lisp_Object g = LGSTRING_GLYPH (gstring, i);
      int c = XFASTINT (AREF (header, i + 1));

      if (NILP (g))
	{
	  g = LGLYPH_NEW ();
	  LGSTRING_SET_GLYPH (gstring, i, g);
	}
      LGLYPH_SET_FROM (g, i);
      LGLYPH_SET_TO (g, i);
      LGLYPH_SET_CHAR (g, c);
      if (FONT_OBJECT_P (font_object))
	{
	  font_fill_lglyph_metrics (g, font_object);
	}
      else
	{
	  int width = XFASTINT (CHAR_TABLE_REF (Vchar_width_table, c));

	  LGLYPH_SET_CODE (g, c);
	  LGLYPH_SET_LBEARING (g, 0);
	  LGLYPH_SET_RBEARING (g, width);
	  LGLYPH_SET_WIDTH (g, width);
	  LGLYPH_SET_ASCENT (g, 1);
	  LGLYPH_SET_DESCENT (g, 0);
	}
      LGLYPH_SET_ADJUSTMENT (g, Qnil);
    }
  if (i < LGSTRING_GLYPH_LEN (gstring))
    LGSTRING_SET_GLYPH (gstring, i, Qnil);
}


/* Try to compose the characters at CHARPOS according to composition
   rule RULE ([PATTERN PREV-CHARS FUNC]).  LIMIT limits the characters
   to compose.  STRING, if not nil, is a target string.  WIN is a
   window where the characters are being displayed.  If characters are
   successfully composed, return the composition as a glyph-string
   object.  Otherwise return nil.  */

static Lisp_Object
autocmp_chars (Lisp_Object rule, EMACS_INT charpos, EMACS_INT bytepos, EMACS_INT limit, struct window *win, struct face *face, Lisp_Object string)
{
  int count = SPECPDL_INDEX ();
  FRAME_PTR f = XFRAME (win->frame);
  Lisp_Object pos = make_number (charpos);
  EMACS_INT to;
  EMACS_INT pt = PT, pt_byte = PT_BYTE;
  Lisp_Object re, font_object, lgstring;
  EMACS_INT len;

  record_unwind_save_match_data ();
  re = AREF (rule, 0);
  if (NILP (re))
    len = 1;
  else if (! STRINGP (re))
    return unbind_to (count, Qnil);
  else if ((len = fast_looking_at (re, charpos, bytepos, limit, -1, string))
	   > 0)
    {
      if (NILP (string))
	len = BYTE_TO_CHAR (bytepos + len) - charpos;
      else
	len = string_byte_to_char (string, bytepos + len) - charpos;
    }
  if (len <= 0)
    return unbind_to (count, Qnil);
  to = limit = charpos + len;
#ifdef HAVE_WINDOW_SYSTEM
  if (FRAME_WINDOW_P (f))
    {
      font_object = font_range (charpos, &to, win, face, string);
      if (! FONT_OBJECT_P (font_object)
	  || (! NILP (re)
	      && to < limit
	      && (fast_looking_at (re, charpos, bytepos, to, -1, string) <= 0)))
	return unbind_to (count, Qnil);
    }
  else
#endif	/* not HAVE_WINDOW_SYSTEM */
    font_object = win->frame;
  lgstring = Fcomposition_get_gstring (pos, make_number (to), font_object,
				       string);
  if (NILP (LGSTRING_ID (lgstring)))
    {
      Lisp_Object args[6];

      /* Save point as marker before calling out to lisp.  */
      if (NILP (string))
	{
	  Lisp_Object m = Fmake_marker ();
	  set_marker_both (m, Qnil, pt, pt_byte);
	  record_unwind_protect (restore_point_unwind, m);
	}

      args[0] = Vauto_composition_function;
      args[1] = AREF (rule, 2);
      args[2] = pos;
      args[3] = make_number (to);
      args[4] = font_object;
      args[5] = string;
      lgstring = safe_call (6, args);
    }
  return unbind_to (count, lgstring);
}

static Lisp_Object _work_val;

/* 1 iff the character C is composable.  Characters of general
   category Z? or C? are not composable except for ZWNJ and ZWJ. */

#define CHAR_COMPOSABLE_P(C)						\
  ((C) > ' '								\
   && ((C) == 0x200C || (C) == 0x200D					\
       || (_work_val = CHAR_TABLE_REF (Vunicode_category_table, (C)),	\
	   (INTEGERP (_work_val)					\
	    && (XINT (_work_val) <= UNICODE_CATEGORY_So)))))

/* Update cmp_it->stop_pos to the next position after CHARPOS (and
   BYTEPOS) where character composition may happen.  If BYTEPOS is
   negative, compute it.  ENDPOS is a limit of searching.  If it is
   less than CHARPOS, search backward to ENDPOS+1 assuming that
   set_iterator_to_next works in reverse order.  In this case, if a
   composition closest to CHARPOS is found, set cmp_it->stop_pos to
   the last character of the composition.

   If no composition is found, set cmp_it->ch to -2.  If a static
   composition is found, set cmp_it->ch to -1.  Otherwise, set
   cmp_it->ch to the character that triggers the automatic
   composition.  */

void
composition_compute_stop_pos (struct composition_it *cmp_it, EMACS_INT charpos, EMACS_INT bytepos, EMACS_INT endpos, Lisp_Object string)
{
  EMACS_INT start, end;
  int c;
  Lisp_Object prop, val;
  /* This is from forward_to_next_line_start in xdisp.c.  */
  const int MAX_NEWLINE_DISTANCE = 500;

  if (charpos < endpos)
    {
      if (endpos > charpos + MAX_NEWLINE_DISTANCE)
	endpos = charpos + MAX_NEWLINE_DISTANCE;
    }
  else if (endpos < charpos)
    {
      /* We search backward for a position to check composition.  */
      if (endpos < 0)
	{
	  /* But we don't know where to stop the searching.  */
	  endpos = NILP (string) ? BEGV - 1 : -1;
	  /* Usually we don't reach ENDPOS because we stop searching
	     at an uncomposable character (NL, LRE, etc).  */
	}
    }
  cmp_it->id = -1;
  cmp_it->ch = -2;
  cmp_it->reversed_p = 0;
  cmp_it->stop_pos = endpos;
  if (charpos == endpos)
    return;
  /* FIXME: Bidi is not yet handled well in static composition.  */
  if (charpos < endpos
      && find_composition (charpos, endpos, &start, &end, &prop, string)
      && start >= charpos
      && COMPOSITION_VALID_P (start, end, prop))
    {
      cmp_it->stop_pos = endpos = start;
      cmp_it->ch = -1;
    }
  if (NILP (BVAR (current_buffer, enable_multibyte_characters))
      || NILP (Vauto_composition_mode))
    return;
  if (bytepos < 0)
    {
      if (NILP (string))
	bytepos = CHAR_TO_BYTE (charpos);
      else
	bytepos = string_char_to_byte (string, charpos);
    }

  start = charpos;
  if (charpos < endpos)
    {
      /* Forward search.  */
      while (charpos < endpos)
	{
	  if (STRINGP (string))
	    FETCH_STRING_CHAR_ADVANCE (c, string, charpos, bytepos);
	  else
	    FETCH_CHAR_ADVANCE (c, charpos, bytepos);
	  if (c == '\n')
	    {
	      cmp_it->ch = -2;
	      break;
	    }
	  val = CHAR_TABLE_REF (Vcomposition_function_table, c);
	  if (! NILP (val))
	    {
	      Lisp_Object elt;
	      int ridx;

	      for (ridx = 0; CONSP (val); val = XCDR (val), ridx++)
		{
		  elt = XCAR (val);
		  if (VECTORP (elt) && ASIZE (elt) == 3
		      && NATNUMP (AREF (elt, 1))
		      && charpos - 1 - XFASTINT (AREF (elt, 1)) >= start)
		    break;
		}
	      if (CONSP (val))
		{
		  cmp_it->rule_idx = ridx;
		  cmp_it->lookback = XFASTINT (AREF (elt, 1));
		  cmp_it->stop_pos = charpos - 1 - cmp_it->lookback;
		  cmp_it->ch = c;
		  return;
		}
	    }
	}
      if (charpos == endpos)
	{
	  /* We couldn't find a composition point before ENDPOS.  But,
	     some character after ENDPOS may be composed with
	     characters before ENDPOS.  So, we should stop at the safe
	     point.  */
	  charpos = endpos - MAX_AUTO_COMPOSITION_LOOKBACK;
	  if (charpos < start)
	    charpos = start;
	}
    }
  else if (charpos > endpos)
    {
      /* Search backward for a pattern that may be composed and the
	 position of (possibly) the last character of the match is
	 closest to (but not after) START.  The reason for the last
	 character is that set_iterator_to_next works in reverse order,
	 and thus we must stop at the last character for composition
	 check.  */
      unsigned char *p;
      int len;
      /* Limit byte position used in fast_looking_at.  This is the
	 byte position of the character after START. */
      EMACS_INT limit;

      if (NILP (string))
	p = BYTE_POS_ADDR (bytepos);
      else
	p = SDATA (string) + bytepos;
      c = STRING_CHAR_AND_LENGTH (p, len);
      limit = bytepos + len;
      while (CHAR_COMPOSABLE_P (c))
	{
	  val = CHAR_TABLE_REF (Vcomposition_function_table, c);
	  if (! NILP (val))
	    {
	      Lisp_Object elt;
	      int ridx, back, blen;

	      for (ridx = 0; CONSP (val); val = XCDR (val), ridx++)
		{
		  elt = XCAR (val);
		  if (VECTORP (elt) && ASIZE (elt) == 3
		      && NATNUMP (AREF (elt, 1))
		      && charpos - (back = XFASTINT (AREF (elt, 1))) > endpos)
		    {
		      EMACS_INT cpos = charpos - back, bpos;

		      if (back == 0)
			bpos = bytepos;
		      else
			bpos = (NILP (string) ? CHAR_TO_BYTE (cpos)
				: string_char_to_byte (string, cpos));
		      if (STRINGP (AREF (elt, 0)))
			blen = fast_looking_at (AREF (elt, 0), cpos, bpos,
						start + 1, limit, string);
		      else
			blen = 1;
		      if (blen > 0)
			{
			  /* Make CPOS point to the last character of
			     match.  Note that BLEN is byte-length.  */
			  if (blen > 1)
			    {
			      bpos += blen;
			      if (NILP (string))
				cpos = BYTE_TO_CHAR (bpos) - 1;
			      else
				cpos = string_byte_to_char (string, bpos) - 1;
			    }
			  back = cpos - (charpos - back);
			  if (cmp_it->stop_pos < cpos
			      || (cmp_it->stop_pos == cpos
				  && cmp_it->lookback < back))
			    {
			      cmp_it->rule_idx = ridx;
			      cmp_it->stop_pos = cpos;
			      cmp_it->ch = c;
			      cmp_it->lookback = back;
			      cmp_it->nchars = back + 1;
			    }
			}
		    }
		}
	    }
	  if (charpos - 1 == endpos)
	    break;
	  if (STRINGP (string))
	    {
	      p--, bytepos--;
	      while (! CHAR_HEAD_P (*p))
		p--, bytepos--;
	      charpos--;
	    }
	  else
	    {
	      DEC_BOTH (charpos, bytepos);
	      p = BYTE_POS_ADDR (bytepos);
	    }
	  c = STRING_CHAR (p);
	}
      if (cmp_it->ch >= 0)
	/* We found a position to check.  */
	return;
      /* Skip all uncomposable characters.  */
      if (NILP (string))
	{
	  while (charpos - 1 > endpos && ! CHAR_COMPOSABLE_P (c))
	    {
	      DEC_BOTH (charpos, bytepos);
	      c = FETCH_MULTIBYTE_CHAR (bytepos);
	    }
	}
      else
	{
	  while (charpos - 1 > endpos && ! CHAR_COMPOSABLE_P (c))
	    {
	      p--;
	      while (! CHAR_HEAD_P (*p))
		p--;
	      charpos--;
	      c = STRING_CHAR (p);
	    }
	}
    }
  cmp_it->stop_pos = charpos;
}

/* Check if the character at CHARPOS (and BYTEPOS) is composed
   (possibly with the following characters) on window W.  ENDPOS limits
   characters to be composed.  FACE, in non-NULL, is a base face of
   the character.  If STRING is not nil, it is a string containing the
   character to check, and CHARPOS and BYTEPOS are indices in the
   string.  In that case, FACE must not be NULL.

   If the character is composed, setup members of CMP_IT (id, nglyphs,
   from, to, reversed_p), and return 1.  Otherwise, update
   CMP_IT->stop_pos, and return 0.  */

int
composition_reseat_it (struct composition_it *cmp_it, EMACS_INT charpos, EMACS_INT bytepos, EMACS_INT endpos, struct window *w, struct face *face, Lisp_Object string)
{
  if (endpos < 0)
    endpos = NILP (string) ? BEGV : 0;

  if (cmp_it->ch == -2)
    {
      composition_compute_stop_pos (cmp_it, charpos, bytepos, endpos, string);
      if (cmp_it->ch == -2 || cmp_it->stop_pos != charpos)
	/* The current position is not composed.  */
	return 0;
    }

  if (cmp_it->ch < 0)
    {
      /* We are looking at a static composition.  */
      EMACS_INT start, end;
      Lisp_Object prop;

      find_composition (charpos, -1, &start, &end, &prop, string);
      cmp_it->id = get_composition_id (charpos, bytepos, end - start,
				       prop, string);
      if (cmp_it->id < 0)
	goto no_composition;
      cmp_it->nchars = end - start;
      cmp_it->nglyphs = composition_table[cmp_it->id]->glyph_len;
    }
  else if (w)
    {
      Lisp_Object lgstring = Qnil;
      Lisp_Object val, elt;
      EMACS_INT i;

      val = CHAR_TABLE_REF (Vcomposition_function_table, cmp_it->ch);
      for (i = 0; i < cmp_it->rule_idx; i++, val = XCDR (val));
      if (charpos < endpos)
	{
	  for (; CONSP (val); val = XCDR (val))
	    {
	      elt = XCAR (val);
	      if (! VECTORP (elt) || ASIZE (elt) != 3
		  || ! INTEGERP (AREF (elt, 1)))
		continue;
	      if (XFASTINT (AREF (elt, 1)) != cmp_it->lookback)
		goto no_composition;
	      lgstring = autocmp_chars (elt, charpos, bytepos, endpos,
					w, face, string);
	      if (composition_gstring_p (lgstring))
		break;
	      lgstring = Qnil;
	      /* Composition failed perhaps because the font doesn't
		 support sufficient range of characters.  Try the
		 other composition rules if any.  */
	    }
	  cmp_it->reversed_p = 0;
	}
      else
	{
	  EMACS_INT cpos = charpos, bpos = bytepos;

	  while (1)
	    {
	      elt = XCAR (val);
	      if (cmp_it->lookback > 0)
		{
		  cpos = charpos - cmp_it->lookback;
		  if (STRINGP (string))
		    bpos = string_char_to_byte (string, cpos);
		  else
		    bpos = CHAR_TO_BYTE (cpos);
		}
	      lgstring = autocmp_chars (elt, cpos, bpos, charpos + 1, w, face,
					string);
	      if (composition_gstring_p (lgstring)
		  && cpos + LGSTRING_CHAR_LEN (lgstring) - 1 == charpos)
		break;
	      /* Composition failed or didn't cover the current
		 character.  */
	      if (cmp_it->lookback == 0)
		goto no_composition;
	      lgstring = Qnil;
	      /* Try to find a shorter composition that starts after CPOS.  */
	      composition_compute_stop_pos (cmp_it, charpos, bytepos, cpos,
					    string);
	      if (cmp_it->ch == -2 || cmp_it->stop_pos < charpos)
		goto no_composition;
	      val = CHAR_TABLE_REF (Vcomposition_function_table, cmp_it->ch);
	      for (i = 0; i < cmp_it->rule_idx; i++, val = XCDR (val));
	    }
	  cmp_it->reversed_p = 1;
	}
      if (NILP (lgstring))
	goto no_composition;
      if (NILP (LGSTRING_ID (lgstring)))
	lgstring = composition_gstring_put_cache (lgstring, -1);
      cmp_it->id = XINT (LGSTRING_ID (lgstring));
      for (i = 0; i < LGSTRING_GLYPH_LEN (lgstring); i++)
	if (NILP (LGSTRING_GLYPH (lgstring, i)))
	  break;
      cmp_it->nglyphs = i;
      cmp_it->from = 0;
      cmp_it->to = i;
    }
  else
    goto no_composition;
  return 1;

 no_composition:
  if (charpos == endpos)
    return 0;
  if (charpos < endpos)
    {
      charpos++;
      if (NILP (string))
	INC_POS (bytepos);
      else
	bytepos += BYTES_BY_CHAR_HEAD (*(SDATA (string) + bytepos));
    }
  else
    {
      charpos--;
      /* BYTEPOS is calculated in composition_compute_stop_pos */
      bytepos = -1;
    }
  composition_compute_stop_pos (cmp_it, charpos, bytepos, endpos, string);
  return 0;
}

/* Update charpos, nchars, nbytes, and width of the current grapheme
   cluster.

   If the composition is static or automatic in L2R context, the
   cluster is identified by CMP_IT->from, and CHARPOS is the position
   of the first character of the cluster.  In this case, update
   CMP_IT->to too.

   If the composition is automatic in R2L context, the cluster is
   identified by CMP_IT->to, and CHARPOS is the position of the last
   character of the cluster.  In this case, update CMP_IT->from too.

   The return value is the character code of the first character of
   the cluster, or -1 if the composition is somehow broken.  */

int
composition_update_it (struct composition_it *cmp_it, EMACS_INT charpos, EMACS_INT bytepos, Lisp_Object string)
{
  int i, c IF_LINT (= 0);

  if (cmp_it->ch < 0)
    {
      /* static composition */
      struct composition *cmp = composition_table[cmp_it->id];

      cmp_it->charpos = charpos;
      cmp_it->to = cmp_it->nglyphs;
      if (cmp_it->nglyphs == 0)
	c = -1;
      else
	{
	  for (i = 0; i < cmp->glyph_len; i++)
	    /* TAB in a composition means display glyphs with padding
	       space on the left or right.  */
	    if ((c = COMPOSITION_GLYPH (cmp, i)) != '\t')
	      break;
	  if (c == '\t')
	    c = ' ';
	}
      cmp_it->width = cmp->width;
      charpos += cmp_it->nchars;
      if (STRINGP (string))
	cmp_it->nbytes = string_char_to_byte (string, charpos) - bytepos;
      else
	cmp_it->nbytes = CHAR_TO_BYTE (charpos) - bytepos;
    }
  else
    {
      /* automatic composition */
      Lisp_Object gstring = composition_gstring_from_id (cmp_it->id);
      Lisp_Object glyph;
      EMACS_INT from;

      if (cmp_it->nglyphs == 0)
	{
	  cmp_it->nchars = LGSTRING_CHAR_LEN (gstring);
	  cmp_it->width = 0;
	  cmp_it->from = cmp_it->to = 0;
	  return -1;
	}
      if (! cmp_it->reversed_p)
	{
	  glyph = LGSTRING_GLYPH (gstring, cmp_it->from);
	  from = LGLYPH_FROM (glyph);
	  for (cmp_it->to = cmp_it->from + 1; cmp_it->to < cmp_it->nglyphs;
	       cmp_it->to++)
	    {
	      glyph = LGSTRING_GLYPH (gstring, cmp_it->to);
	      if (LGLYPH_FROM (glyph) != from)
		break;
	    }
	  cmp_it->charpos = charpos;
	}
      else
	{
	  glyph = LGSTRING_GLYPH (gstring, cmp_it->to - 1);
	  from = LGLYPH_FROM (glyph);
	  cmp_it->charpos = charpos - (LGLYPH_TO (glyph) - from);
	  for (cmp_it->from = cmp_it->to - 1; cmp_it->from > 0;
	       cmp_it->from--)
	    {
	      glyph = LGSTRING_GLYPH (gstring, cmp_it->from - 1);
	      if (LGLYPH_FROM (glyph) != from)
		break;
	    }
	}
      glyph = LGSTRING_GLYPH (gstring, cmp_it->from);
      cmp_it->nchars = LGLYPH_TO (glyph) + 1 - from;
      cmp_it->nbytes = 0;
      cmp_it->width = 0;
      for (i = cmp_it->nchars - 1; i >= 0; i--)
	{
	  c = XINT (LGSTRING_CHAR (gstring, i));
	  cmp_it->nbytes += CHAR_BYTES (c);
	  cmp_it->width += CHAR_WIDTH (c);
	}
    }
  return c;
}


struct position_record
{
  EMACS_INT pos, pos_byte;
  unsigned char *p;
};

/* Update the members of POSITION to the next character boundary.  */
#define FORWARD_CHAR(POSITION, STOP)					\
  do {									\
    (POSITION).pos++;							\
    if ((POSITION).pos == (STOP))					\
      {									\
	(POSITION).p = GAP_END_ADDR;					\
	(POSITION).pos_byte = GPT_BYTE;					\
      }									\
    else								\
      {									\
	(POSITION).pos_byte += BYTES_BY_CHAR_HEAD (*((POSITION).p));	\
	(POSITION).p += BYTES_BY_CHAR_HEAD (*((POSITION).p));		\
      }									\
  } while (0)

/* Update the members of POSITION to the previous character boundary.  */
#define BACKWARD_CHAR(POSITION, STOP)		\
  do {						\
    if ((POSITION).pos == (STOP))		\
      (POSITION).p = GPT_ADDR;			\
    do {					\
      (POSITION).pos_byte--;			\
      (POSITION).p--;				\
    } while (! CHAR_HEAD_P (*((POSITION).p)));	\
    (POSITION).pos--;				\
  } while (0)

/* This is like find_composition, but find an automatic composition
   instead.  It is assured that POS is not within a static
   composition.  If found, set *GSTRING to the glyph-string
   representing the composition, and return 1.  Otherwise, *GSTRING to
   Qnil, and return 0.  */

static int
find_automatic_composition (EMACS_INT pos, EMACS_INT limit,
			    EMACS_INT *start, EMACS_INT *end,
			    Lisp_Object *gstring, Lisp_Object string)
{
  EMACS_INT head, tail, stop;
  /* Forward limit position of checking a composition taking a
     looking-back count into account.  */
  EMACS_INT fore_check_limit;
  struct position_record cur, prev;
  int c;
  Lisp_Object window;
  struct window *w;
  int need_adjustment = 0;

  window = Fget_buffer_window (Fcurrent_buffer (), Qnil);
  if (NILP (window))
    return 0;
  w = XWINDOW (window);

  cur.pos = pos;
  if (NILP (string))
    {
      head = BEGV, tail = ZV, stop = GPT;
      cur.pos_byte = CHAR_TO_BYTE (cur.pos);
      cur.p = BYTE_POS_ADDR (cur.pos_byte);
    }
  else
    {
      head = 0, tail = SCHARS (string), stop = -1;
      cur.pos_byte = string_char_to_byte (string, cur.pos);
      cur.p = SDATA (string) + cur.pos_byte;
    }
  if (limit < 0)
    /* Finding a composition covering the character after POS is the
       same as setting LIMIT to POS.  */
    limit = pos;
  if (limit <= pos)
    fore_check_limit = min (tail, pos + 1 + MAX_AUTO_COMPOSITION_LOOKBACK);
  else
    fore_check_limit = min (tail, limit + MAX_AUTO_COMPOSITION_LOOKBACK);

  /* Provided that we have these possible compositions now:

	   POS:	1 2 3 4 5 6 7 8 9
		        |-A-|
		  |-B-|-C-|--D--|

     Here, it is known that characters after positions 1 and 9 can
     never be composed (i.e. ! CHAR_COMPOSABLE_P (CH)), and
     composition A is an invalid one because it's partially covered by
     the valid composition C.  And to know whether a composition is
     valid or not, the only way is to start searching forward from a
     position that can not be a tail part of composition (it's 2 in
     the above case).

     Now we have these cases (1 through 4):

                   -- character after POS is ... --
                   not composable         composable
     LIMIT <= POS  (1)                    (3)
     POS < LIMIT   (2)                    (4)

     Among them, in case (2), we simply search forward from POS.

     In the other cases, we at first rewind back to the position where
     the previous character is not composable or the beginning of
     buffer (string), then search compositions forward.  In case (1)
     and (3) we repeat this process until a composition is found.  */

  while (1)
    {
      c = STRING_CHAR (cur.p);
      if (! CHAR_COMPOSABLE_P (c))
	{
	  if (limit <= pos)	/* case (1)  */
	    {
	      do {
		if (cur.pos <= limit)
		  return 0;
		BACKWARD_CHAR (cur, stop);
		c = STRING_CHAR (cur.p);
	      } while (! CHAR_COMPOSABLE_P (c));
	      fore_check_limit = cur.pos + 1;
	    }
	  else			/* case (2) */
	    /* No need of rewinding back.  */
	    goto search_forward;
	}

      /* Rewind back to the position where we can safely search
	 forward for compositions.  It is assured that the character
	 at cur.pos is composable.  */
      while (head < cur.pos)
	{
	  prev = cur;
	  BACKWARD_CHAR (cur, stop);
	  c = STRING_CHAR (cur.p);
	  if (! CHAR_COMPOSABLE_P (c))
	    {
	      cur = prev;
	      break;
	    }
	}

    search_forward:
      /* Now search forward. */
      *gstring = Qnil;
      prev = cur;	/* remember the start of searching position. */
      while (cur.pos < fore_check_limit)
	{
	  Lisp_Object val;

	  c = STRING_CHAR (cur.p);
	  for (val = CHAR_TABLE_REF (Vcomposition_function_table, c);
	       CONSP (val); val = XCDR (val))
	    {
	      Lisp_Object elt = XCAR (val);

	      if (VECTORP (elt) && ASIZE (elt) == 3 && NATNUMP (AREF (elt, 1)))
		{
		  EMACS_INT check_pos = cur.pos - XFASTINT (AREF (elt, 1));
		  struct position_record check;

		  if (check_pos < head
		      || (limit <= pos ? pos < check_pos
			  : limit <= check_pos))
		    continue;
		  for (check = cur; check_pos < check.pos; )
		    BACKWARD_CHAR (check, stop);
		  *gstring = autocmp_chars (elt, check.pos, check.pos_byte,
					    tail, w, NULL, string);
		  need_adjustment = 1;
		  if (NILP (*gstring))
		    {
		      /* As we have called Lisp, there's a possibility
			 that buffer/string is relocated.  */
		      if (NILP (string))
			cur.p  = BYTE_POS_ADDR (cur.pos_byte);
		      else
			cur.p = SDATA (string) + cur.pos_byte;
		    }
		  else
		    {
		      /* We found a candidate of a target composition.  */
		      *start = check.pos;
		      *end = check.pos + LGSTRING_CHAR_LEN (*gstring);
		      if (pos < limit
			  ? pos < *end
			  : *start <= pos && pos < *end)
			/* This is the target composition. */
			return 1;
		      cur.pos = *end;
		      if (NILP (string))
			{
			  cur.pos_byte = CHAR_TO_BYTE (cur.pos);
			  cur.p  = BYTE_POS_ADDR (cur.pos_byte);
			}
		      else
			{
			  cur.pos_byte = string_char_to_byte (string, cur.pos);
			  cur.p = SDATA (string) + cur.pos_byte;
			}
		      break;
		    }
		}
	    }
	  if (! CONSP (val))
	    /* We found no composition here.  */
	    FORWARD_CHAR (cur, stop);
	}

      if (pos < limit)		/* case (2) and (4)*/
	return 0;
      if (! NILP (*gstring))
	return 1;
      if (prev.pos == head)
	return 0;
      cur = prev;
      if (need_adjustment)
	{
	  if (NILP (string))
	    cur.p  = BYTE_POS_ADDR (cur.pos_byte);
	  else
	    cur.p = SDATA (string) + cur.pos_byte;
	}
      BACKWARD_CHAR (cur, stop);
    }
}

/* Return the adjusted point provided that point is moved from LAST_PT
   to NEW_PT.  */

EMACS_INT
composition_adjust_point (EMACS_INT last_pt, EMACS_INT new_pt)
{
  EMACS_INT i, beg, end;
  Lisp_Object val;

  if (new_pt == BEGV || new_pt == ZV)
    return new_pt;

  /* At first check the static composition. */
  if (get_property_and_range (new_pt, Qcomposition, &val, &beg, &end, Qnil)
      && COMPOSITION_VALID_P (beg, end, val))
    {
      if (beg < new_pt /* && end > new_pt   <- It's always the case.  */
	  && (last_pt <= beg || last_pt >= end))
	return (new_pt < last_pt ? beg : end);
      return new_pt;
    }

  if (NILP (BVAR (current_buffer, enable_multibyte_characters))
      || NILP (Vauto_composition_mode))
    return new_pt;

  /* Next check the automatic composition.  */
  if (! find_automatic_composition (new_pt, (EMACS_INT) -1, &beg, &end, &val,
				    Qnil)
      || beg == new_pt)
    return new_pt;
  for (i = 0; i < LGSTRING_GLYPH_LEN (val); i++)
    {
      Lisp_Object glyph = LGSTRING_GLYPH (val, i);

      if (NILP (glyph))
	break;
      if (beg + LGLYPH_FROM (glyph) == new_pt)
	return new_pt;
      if (beg + LGLYPH_TO (glyph) >= new_pt)
	return (new_pt < last_pt
		? beg + LGLYPH_FROM (glyph)
		: beg + LGLYPH_TO (glyph) + 1);
    }
  return new_pt;
}

DEFUN ("composition-get-gstring", Fcomposition_get_gstring,
       Scomposition_get_gstring, 4, 4, 0,
       doc: /* Return a glyph-string for characters between FROM and TO.
If the glyph string is for graphic display, FONT-OBJECT must be
a font-object to use for those characters.
Otherwise (for terminal display), FONT-OBJECT must be a terminal ID, a
frame, or nil for the selected frame's terminal device.

If the optional 4th argument STRING is not nil, it is a string
containing the target characters between indices FROM and TO.

A glyph-string is a vector containing information about how to display
a specific character sequence.  The format is:
   [HEADER ID GLYPH ...]

HEADER is a vector of this form:
    [FONT-OBJECT CHAR ...]
where
    FONT-OBJECT is a font-object for all glyphs in the glyph-string,
    or the terminal coding system of the specified terminal.
    CHARs are characters to be composed by GLYPHs.

ID is an identification number of the glyph-string.  It may be nil if
not yet shaped.

GLYPH is a vector whose elements have this form:
    [ FROM-IDX TO-IDX C CODE WIDTH LBEARING RBEARING ASCENT DESCENT
      [ [X-OFF Y-OFF WADJUST] | nil] ]
where
    FROM-IDX and TO-IDX are used internally and should not be touched.
    C is the character of the glyph.
    CODE is the glyph-code of C in FONT-OBJECT.
    WIDTH thru DESCENT are the metrics (in pixels) of the glyph.
    X-OFF and Y-OFF are offsets to the base position for the glyph.
    WADJUST is the adjustment to the normal width of the glyph.

If GLYPH is nil, the remaining elements of the glyph-string vector
should be ignored.  */)
  (Lisp_Object from, Lisp_Object to, Lisp_Object font_object, Lisp_Object string)
{
  Lisp_Object gstring, header;
  EMACS_INT frompos, topos;

  CHECK_NATNUM (from);
  CHECK_NATNUM (to);
  if (! FONT_OBJECT_P (font_object))
    {
      struct coding_system *coding;
      struct terminal *terminal = get_terminal (font_object, 1);

      coding = ((TERMINAL_TERMINAL_CODING (terminal)->common_flags
		 & CODING_REQUIRE_ENCODING_MASK)
		? TERMINAL_TERMINAL_CODING (terminal) : &safe_terminal_coding);
      font_object = CODING_ID_NAME (coding->id);
    }

  header = fill_gstring_header (Qnil, from, to, font_object, string);
  gstring = gstring_lookup_cache (header);
  if (! NILP (gstring))
    return gstring;

  frompos = XINT (from);
  topos = XINT (to);
  if (LGSTRING_GLYPH_LEN (gstring_work) < topos - frompos)
    gstring_work = Fmake_vector (make_number (topos - frompos + 2), Qnil);
  LGSTRING_SET_HEADER (gstring_work, header);
  LGSTRING_SET_ID (gstring_work, Qnil);
  fill_gstring_body (gstring_work);
  return gstring_work;
}


/* Emacs Lisp APIs.  */

DEFUN ("compose-region-internal", Fcompose_region_internal,
       Scompose_region_internal, 2, 4, 0,
       doc: /* Internal use only.

Compose text in the region between START and END.
Optional 3rd and 4th arguments are COMPONENTS and MODIFICATION-FUNC
for the composition.  See `compose-region' for more details.  */)
  (Lisp_Object start, Lisp_Object end, Lisp_Object components, Lisp_Object modification_func)
{
  validate_region (&start, &end);
  if (!NILP (components)
      && !INTEGERP (components)
      && !CONSP (components)
      && !STRINGP (components))
    CHECK_VECTOR (components);

  compose_text (XINT (start), XINT (end), components, modification_func, Qnil);
  return Qnil;
}

DEFUN ("compose-string-internal", Fcompose_string_internal,
       Scompose_string_internal, 3, 5, 0,
       doc: /* Internal use only.

Compose text between indices START and END of STRING.
Optional 4th and 5th arguments are COMPONENTS and MODIFICATION-FUNC
for the composition.  See `compose-string' for more details.  */)
  (Lisp_Object string, Lisp_Object start, Lisp_Object end, Lisp_Object components, Lisp_Object modification_func)
{
  CHECK_STRING (string);
  CHECK_NUMBER (start);
  CHECK_NUMBER (end);

  if (XINT (start) < 0 ||
      XINT (start) > XINT (end)
      || XINT (end) > SCHARS (string))
    args_out_of_range (start, end);

  compose_text (XINT (start), XINT (end), components, modification_func, string);
  return string;
}

DEFUN ("find-composition-internal", Ffind_composition_internal,
       Sfind_composition_internal, 4, 4, 0,
       doc: /* Internal use only.

Return information about composition at or nearest to position POS.
See `find-composition' for more details.  */)
  (Lisp_Object pos, Lisp_Object limit, Lisp_Object string, Lisp_Object detail_p)
{
  Lisp_Object prop, tail, gstring;
  EMACS_INT start, end, from, to;
  int id;

  CHECK_NUMBER_COERCE_MARKER (pos);
  from = XINT (pos);
  if (!NILP (limit))
    {
      CHECK_NUMBER_COERCE_MARKER (limit);
      to = XINT (limit);
    }
  else
    to = -1;

  if (!NILP (string))
    {
      CHECK_STRING (string);
      if (XINT (pos) < 0 || XINT (pos) > SCHARS (string))
	args_out_of_range (string, pos);
    }
  else
    {
      if (XINT (pos) < BEGV || XINT (pos) > ZV)
	args_out_of_range (Fcurrent_buffer (), pos);
    }

  if (!find_composition (from, to, &start, &end, &prop, string))
    {
      if (!NILP (BVAR (current_buffer, enable_multibyte_characters))
	  && ! NILP (Vauto_composition_mode)
	  && find_automatic_composition (from, to, &start, &end, &gstring,
					 string))
	return list3 (make_number (start), make_number (end), gstring);
      return Qnil;
    }
  if ((end <= XINT (pos) || start > XINT (pos)))
    {
      EMACS_INT s, e;

      if (find_automatic_composition (from, to, &s, &e, &gstring, string)
	  && (e <= XINT (pos) ? e > end : s < start))
	return list3 (make_number (s), make_number (e), gstring);
    }
  if (!COMPOSITION_VALID_P (start, end, prop))
    return Fcons (make_number (start), Fcons (make_number (end),
					      Fcons (Qnil, Qnil)));
  if (NILP (detail_p))
    return Fcons (make_number (start), Fcons (make_number (end),
					      Fcons (Qt, Qnil)));

  if (COMPOSITION_REGISTERD_P (prop))
    id = COMPOSITION_ID (prop);
  else
    {
      EMACS_INT start_byte = (NILP (string)
			      ? CHAR_TO_BYTE (start)
			      : string_char_to_byte (string, start));
      id = get_composition_id (start, start_byte, end - start, prop, string);
    }

  if (id >= 0)
    {
      Lisp_Object components, relative_p, mod_func;
      enum composition_method method = COMPOSITION_METHOD (prop);
      int width = composition_table[id]->width;

      components = Fcopy_sequence (COMPOSITION_COMPONENTS (prop));
      relative_p = (method == COMPOSITION_WITH_RULE_ALTCHARS
		    ? Qnil : Qt);
      mod_func = COMPOSITION_MODIFICATION_FUNC (prop);
      tail = Fcons (components,
		    Fcons (relative_p,
			   Fcons (mod_func,
				  Fcons (make_number (width), Qnil))));
    }
  else
    tail = Qnil;

  return Fcons (make_number (start), Fcons (make_number (end), tail));
}


void
syms_of_composite (void)
{
  int i;

  DEFSYM (Qcomposition, "composition");

  /* Make a hash table for static composition.  */
  {
    Lisp_Object args[6];

    args[0] = QCtest;
    args[1] = Qequal;
    args[2] = QCweakness;
    /* We used to make the hash table weak so that unreferenced
       compositions can be garbage-collected.  But, usually once
       created compositions are repeatedly used in an Emacs session,
       and thus it's not worth to save memory in such a way.  So, we
       make the table not weak.  */
    args[3] = Qnil;
    args[4] = QCsize;
    args[5] = make_number (311);
    composition_hash_table = Fmake_hash_table (6, args);
    staticpro (&composition_hash_table);
  }

  /* Make a hash table for glyph-string.  */
  {
    Lisp_Object args[6];
    args[0] = QCtest;
    args[1] = Qequal;
    args[2] = QCweakness;
    args[3] = Qnil;
    args[4] = QCsize;
    args[5] = make_number (311);
    gstring_hash_table = Fmake_hash_table (6, args);
    staticpro (&gstring_hash_table);
  }

  staticpro (&gstring_work_headers);
  gstring_work_headers = Fmake_vector (make_number (8), Qnil);
  for (i = 0; i < 8; i++)
    ASET (gstring_work_headers, i, Fmake_vector (make_number (i + 2), Qnil));
  staticpro (&gstring_work);
  gstring_work = Fmake_vector (make_number (10), Qnil);

  /* Text property `composition' should be nonsticky by default.  */
  Vtext_property_default_nonsticky
    = Fcons (Fcons (Qcomposition, Qt), Vtext_property_default_nonsticky);

  DEFVAR_LISP ("compose-chars-after-function", Vcompose_chars_after_function,
	       doc: /* Function to adjust composition of buffer text.

This function is called with three arguments: FROM, TO, and OBJECT.
FROM and TO specify the range of text whose composition should be
adjusted.  OBJECT, if non-nil, is a string that contains the text.

This function is called after a text with `composition' property is
inserted or deleted to keep `composition' property of buffer text
valid.

The default value is the function `compose-chars-after'.  */);
  Vcompose_chars_after_function = intern_c_string ("compose-chars-after");

  DEFSYM (Qauto_composed, "auto-composed");
  DEFSYM (Qauto_composition_function, "auto-composition-function");

  DEFVAR_LISP ("auto-composition-mode", Vauto_composition_mode,
	       doc: /* Non-nil if Auto-Composition mode is enabled.
Use the command `auto-composition-mode' to change this variable. */);
  Vauto_composition_mode = Qt;

  DEFVAR_LISP ("auto-composition-function", Vauto_composition_function,
	       doc: /* Function to call to compose characters automatically.
This function is called from the display routine with four arguments:
FROM, TO, WINDOW, and STRING.

If STRING is nil, the function must compose characters in the region
between FROM and TO in the current buffer.

Otherwise, STRING is a string, and FROM and TO are indices into the
string.  In this case, the function must compose characters in the
string.  */);
  Vauto_composition_function = Qnil;

  DEFVAR_LISP ("composition-function-table", Vcomposition_function_table,
	       doc: /* Char-table of functions for automatic character composition.
For each character that has to be composed automatically with
preceding and/or following characters, this char-table contains
a function to call to compose that character.

The element at index C in the table, if non-nil, is a list of
composition rules of this form: ([PATTERN PREV-CHARS FUNC] ...)

PATTERN is a regular expression which C and the surrounding
characters must match.

PREV-CHARS is a non-negative integer (less than 4) specifying how many
characters before C to check the matching with PATTERN.  If it is 0,
PATTERN must match C and the following characters.  If it is 1,
PATTERN must match a character before C and the following characters.

If PREV-CHARS is 0, PATTERN can be nil, which means that the
single character C should be composed.

FUNC is a function to return a glyph-string representing a
composition of the characters that match PATTERN.  It is
called with one argument GSTRING.

GSTRING is a template of a glyph-string to return.  It is already
filled with a proper header for the characters to compose, and
glyphs corresponding to those characters one by one.  The
function must return a new glyph-string with the same header as
GSTRING, or modify GSTRING itself and return it.

See also the documentation of `auto-composition-mode'.  */);
  Vcomposition_function_table = Fmake_char_table (Qnil, Qnil);

  defsubr (&Scompose_region_internal);
  defsubr (&Scompose_string_internal);
  defsubr (&Sfind_composition_internal);
  defsubr (&Scomposition_get_gstring);
}
