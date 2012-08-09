/* GNU Emacs routines to deal with case tables.
   Copyright (C) 1993-1994, 2001-2012  Free Software Foundation, Inc.

Author: Howard Gayle

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

static Lisp_Object Qcase_table_p, Qcase_table;
Lisp_Object Vascii_downcase_table;
static Lisp_Object Vascii_upcase_table;
Lisp_Object Vascii_canon_table;
static Lisp_Object Vascii_eqv_table;

static void set_canon (Lisp_Object case_table, Lisp_Object range, Lisp_Object elt);
static void set_identity (Lisp_Object table, Lisp_Object c, Lisp_Object elt);
static void shuffle (Lisp_Object table, Lisp_Object c, Lisp_Object elt);

DEFUN ("case-table-p", Fcase_table_p, Scase_table_p, 1, 1, 0,
       doc: /* Return t if OBJECT is a case table.
See `set-case-table' for more information on these data structures.  */)
  (Lisp_Object object)
{
  Lisp_Object up, canon, eqv;

  if (! CHAR_TABLE_P (object))
    return Qnil;
  if (! EQ (XCHAR_TABLE (object)->purpose, Qcase_table))
    return Qnil;

  up = XCHAR_TABLE (object)->extras[0];
  canon = XCHAR_TABLE (object)->extras[1];
  eqv = XCHAR_TABLE (object)->extras[2];

  return ((NILP (up) || CHAR_TABLE_P (up))
	  && ((NILP (canon) && NILP (eqv))
 	      || (CHAR_TABLE_P (canon)
		  && (NILP (eqv) || CHAR_TABLE_P (eqv))))
	  ? Qt : Qnil);
}

static Lisp_Object
check_case_table (Lisp_Object obj)
{
  CHECK_TYPE (!NILP (Fcase_table_p (obj)), Qcase_table_p, obj);
  return (obj);
}

DEFUN ("current-case-table", Fcurrent_case_table, Scurrent_case_table, 0, 0, 0,
       doc: /* Return the case table of the current buffer.  */)
  (void)
{
  return BVAR (current_buffer, downcase_table);
}

DEFUN ("standard-case-table", Fstandard_case_table, Sstandard_case_table, 0, 0, 0,
       doc: /* Return the standard case table.
This is the one used for new buffers.  */)
  (void)
{
  return Vascii_downcase_table;
}

static Lisp_Object set_case_table (Lisp_Object table, int standard);

DEFUN ("set-case-table", Fset_case_table, Sset_case_table, 1, 1, 0,
       doc: /* Select a new case table for the current buffer.
A case table is a char-table which maps characters
to their lower-case equivalents.  It also has three \"extra\" slots
which may be additional char-tables or nil.
These slots are called UPCASE, CANONICALIZE and EQUIVALENCES.
UPCASE maps each non-upper-case character to its upper-case equivalent.
 (The value in UPCASE for an upper-case character is never used.)
 If lower and upper case characters are in 1-1 correspondence,
 you may use nil and the upcase table will be deduced from DOWNCASE.
CANONICALIZE maps each character to a canonical equivalent;
 any two characters that are related by case-conversion have the same
 canonical equivalent character; it may be nil, in which case it is
 deduced from DOWNCASE and UPCASE.
EQUIVALENCES is a map that cyclically permutes each equivalence class
 (of characters with the same canonical equivalent); it may be nil,
 in which case it is deduced from CANONICALIZE.  */)
  (Lisp_Object table)
{
  return set_case_table (table, 0);
}

DEFUN ("set-standard-case-table", Fset_standard_case_table,
       Sset_standard_case_table, 1, 1, 0,
       doc: /* Select a new standard case table for new buffers.
See `set-case-table' for more info on case tables.  */)
  (Lisp_Object table)
{
  return set_case_table (table, 1);
}

static Lisp_Object
set_case_table (Lisp_Object table, int standard)
{
  Lisp_Object up, canon, eqv;

  check_case_table (table);

  up = XCHAR_TABLE (table)->extras[0];
  canon = XCHAR_TABLE (table)->extras[1];
  eqv = XCHAR_TABLE (table)->extras[2];

  if (NILP (up))
    {
      up = Fmake_char_table (Qcase_table, Qnil);
      map_char_table (set_identity, Qnil, table, up);
      map_char_table (shuffle, Qnil, table, up);
      XCHAR_TABLE (table)->extras[0] = up;
    }

  if (NILP (canon))
    {
      canon = Fmake_char_table (Qcase_table, Qnil);
      XCHAR_TABLE (table)->extras[1] = canon;
      map_char_table (set_canon, Qnil, table, table);
    }

  if (NILP (eqv))
    {
      eqv = Fmake_char_table (Qcase_table, Qnil);
      map_char_table (set_identity, Qnil, canon, eqv);
      map_char_table (shuffle, Qnil, canon, eqv);
      XCHAR_TABLE (table)->extras[2] = eqv;
    }

  /* This is so set_image_of_range_1 in regex.c can find the EQV table.  */
  XCHAR_TABLE (canon)->extras[2] = eqv;

  if (standard)
    {
      Vascii_downcase_table = table;
      Vascii_upcase_table = up;
      Vascii_canon_table = canon;
      Vascii_eqv_table = eqv;
    }
  else
    {
      BVAR (current_buffer, downcase_table) = table;
      BVAR (current_buffer, upcase_table) = up;
      BVAR (current_buffer, case_canon_table) = canon;
      BVAR (current_buffer, case_eqv_table) = eqv;
    }

  return table;
}

/* The following functions are called in map_char_table.  */

/* Set CANON char-table element for characters in RANGE to a
   translated ELT by UP and DOWN char-tables.  This is done only when
   ELT is a character.  The char-tables CANON, UP, and DOWN are in
   CASE_TABLE.  */

static void
set_canon (Lisp_Object case_table, Lisp_Object range, Lisp_Object elt)
{
  Lisp_Object up = XCHAR_TABLE (case_table)->extras[0];
  Lisp_Object canon = XCHAR_TABLE (case_table)->extras[1];

  if (NATNUMP (elt))
    Fset_char_table_range (canon, range, Faref (case_table, Faref (up, elt)));
}

/* Set elements of char-table TABLE for C to C itself.  C may be a
   cons specifying a character range.  In that case, set characters in
   that range to themselves.  This is done only when ELT is a
   character.  This is called in map_char_table.  */

static void
set_identity (Lisp_Object table, Lisp_Object c, Lisp_Object elt)
{
  if (NATNUMP (elt))
    {
      int from;
      unsigned to;

      if (CONSP (c))
	{
	  from = XINT (XCAR (c));
	  to = XINT (XCDR (c));
	}
      else
	from = to = XINT (c);
      for (to++; from < to; from++)
	CHAR_TABLE_SET (table, from, make_number (from));
    }
}

/* Permute the elements of TABLE (which is initially an identity
   mapping) so that it has one cycle for each equivalence class
   induced by the translation table on which map_char_table is
   operated.  */

static void
shuffle (Lisp_Object table, Lisp_Object c, Lisp_Object elt)
{
  if (NATNUMP (elt))
    {
      int from;
      unsigned to;

      if (CONSP (c))
	{
	  from = XINT (XCAR (c));
	  to = XINT (XCDR (c));
	}
      else
	from = to = XINT (c);

      for (to++; from < to; from++)
	{
	  Lisp_Object tem = Faref (table, elt);
	  Faset (table, elt, make_number (from));
	  Faset (table, make_number (from), tem);
	}
    }
}

void
init_casetab_once (void)
{
  register int i;
  Lisp_Object down, up;
  DEFSYM (Qcase_table, "case-table");

  /* Intern this now in case it isn't already done.
     Setting this variable twice is harmless.
     But don't staticpro it here--that is done in alloc.c.  */
  Qchar_table_extra_slots = intern_c_string ("char-table-extra-slots");

  /* Now we are ready to set up this property, so we can
     create char tables.  */
  Fput (Qcase_table, Qchar_table_extra_slots, make_number (3));

  down = Fmake_char_table (Qcase_table, Qnil);
  Vascii_downcase_table = down;
  XCHAR_TABLE (down)->purpose = Qcase_table;

  for (i = 0; i < 128; i++)
    {
      int c = (i >= 'A' && i <= 'Z') ? i + ('a' - 'A') : i;
      CHAR_TABLE_SET (down, i, make_number (c));
    }

  XCHAR_TABLE (down)->extras[1] = Fcopy_sequence (down);

  up = Fmake_char_table (Qcase_table, Qnil);
  XCHAR_TABLE (down)->extras[0] = up;

  for (i = 0; i < 128; i++)
    {
      int c = ((i >= 'A' && i <= 'Z') ? i + ('a' - 'A')
	       : ((i >= 'a' && i <= 'z') ? i + ('A' - 'a')
		  : i));
      CHAR_TABLE_SET (up, i, make_number (c));
    }

  XCHAR_TABLE (down)->extras[2] = Fcopy_sequence (up);

  /* Fill in what isn't filled in.  */
  set_case_table (down, 1);
}

void
syms_of_casetab (void)
{
  DEFSYM (Qcase_table_p, "case-table-p");

  staticpro (&Vascii_canon_table);
  staticpro (&Vascii_downcase_table);
  staticpro (&Vascii_eqv_table);
  staticpro (&Vascii_upcase_table);

  defsubr (&Scase_table_p);
  defsubr (&Scurrent_case_table);
  defsubr (&Sstandard_case_table);
  defsubr (&Sset_case_table);
  defsubr (&Sset_standard_case_table);
}
