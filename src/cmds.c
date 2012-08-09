/* Simple built-in editing commands.

Copyright (C) 1985, 1993-1998, 2001-2012  Free Software Foundation, Inc.

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
#include "commands.h"
#include "buffer.h"
#include "character.h"
#include "syntax.h"
#include "window.h"
#include "keyboard.h"
#include "keymap.h"
#include "dispextern.h"
#include "frame.h"

static Lisp_Object Qkill_forward_chars, Qkill_backward_chars;

/* A possible value for a buffer's overwrite-mode variable.  */
static Lisp_Object Qoverwrite_mode_binary;

static int internal_self_insert (int, EMACS_INT);

DEFUN ("forward-point", Fforward_point, Sforward_point, 1, 1, 0,
       doc: /* Return buffer position N characters after (before if N negative) point.  */)
  (Lisp_Object n)
{
  CHECK_NUMBER (n);

  return make_number (PT + XINT (n));
}

DEFUN ("forward-char", Fforward_char, Sforward_char, 0, 1, "^p",
       doc: /* Move point N characters forward (backward if N is negative).
On reaching end or beginning of buffer, stop and signal error.

Depending on the bidirectional context, the movement may be to the
right or to the left on the screen.  This is in contrast with
\\[right-char], which see.  */)
  (Lisp_Object n)
{
  if (NILP (n))
    XSETFASTINT (n, 1);
  else
    CHECK_NUMBER (n);

  /* This used to just set point to point + XINT (n), and then check
     to see if it was within boundaries.  But now that SET_PT can
     potentially do a lot of stuff (calling entering and exiting
     hooks, etcetera), that's not a good approach.  So we validate the
     proposed position, then set point.  */
  {
    EMACS_INT new_point = PT + XINT (n);

    if (new_point < BEGV)
      {
	SET_PT (BEGV);
	xsignal0 (Qbeginning_of_buffer);
      }
    if (new_point > ZV)
      {
	SET_PT (ZV);
	xsignal0 (Qend_of_buffer);
      }

    SET_PT (new_point);
  }

  return Qnil;
}

DEFUN ("backward-char", Fbackward_char, Sbackward_char, 0, 1, "^p",
       doc: /* Move point N characters backward (forward if N is negative).
On attempt to pass beginning or end of buffer, stop and signal error.

Depending on the bidirectional context, the movement may be to the
right or to the left on the screen.  This is in contrast with
\\[left-char], which see.  */)
  (Lisp_Object n)
{
  if (NILP (n))
    XSETFASTINT (n, 1);
  else
    CHECK_NUMBER (n);

  XSETINT (n, - XINT (n));
  return Fforward_char (n);
}

DEFUN ("forward-line", Fforward_line, Sforward_line, 0, 1, "^p",
       doc: /* Move N lines forward (backward if N is negative).
Precisely, if point is on line I, move to the start of line I + N
\("start of line" in the logical order).
If there isn't room, go as far as possible (no error).
Returns the count of lines left to move.  If moving forward,
that is N - number of lines moved; if backward, N + number moved.
With positive N, a non-empty line at the end counts as one line
successfully moved (for the return value).  */)
  (Lisp_Object n)
{
  EMACS_INT opoint = PT, opoint_byte = PT_BYTE;
  EMACS_INT pos, pos_byte;
  EMACS_INT count, shortage;

  if (NILP (n))
    count = 1;
  else
    {
      CHECK_NUMBER (n);
      count = XINT (n);
    }

  if (count <= 0)
    shortage = scan_newline (PT, PT_BYTE, BEGV, BEGV_BYTE, count - 1, 1);
  else
    shortage = scan_newline (PT, PT_BYTE, ZV, ZV_BYTE, count, 1);

  /* Since scan_newline does TEMP_SET_PT_BOTH,
     and we want to set PT "for real",
     go back to the old point and then come back here.  */
  pos = PT;
  pos_byte = PT_BYTE;
  TEMP_SET_PT_BOTH (opoint, opoint_byte);
  SET_PT_BOTH (pos, pos_byte);

  if (shortage > 0
      && (count <= 0
	  || (ZV > BEGV
	      && PT != opoint
	      && (FETCH_BYTE (PT_BYTE - 1) != '\n'))))
    shortage--;

  return make_number (count <= 0 ? - shortage : shortage);
}

DEFUN ("beginning-of-line", Fbeginning_of_line, Sbeginning_of_line, 0, 1, "^p",
       doc: /* Move point to beginning of current line (in the logical order).
With argument N not nil or 1, move forward N - 1 lines first.
If point reaches the beginning or end of buffer, it stops there.

This function constrains point to the current field unless this moves
point to a different line than the original, unconstrained result.
If N is nil or 1, and a front-sticky field starts at point, the point
does not move.  To ignore field boundaries bind
`inhibit-field-text-motion' to t, or use the `forward-line' function
instead.  For instance, `(forward-line 0)' does the same thing as
`(beginning-of-line)', except that it ignores field boundaries.  */)
  (Lisp_Object n)
{
  if (NILP (n))
    XSETFASTINT (n, 1);
  else
    CHECK_NUMBER (n);

  SET_PT (XINT (Fline_beginning_position (n)));

  return Qnil;
}

DEFUN ("end-of-line", Fend_of_line, Send_of_line, 0, 1, "^p",
       doc: /* Move point to end of current line (in the logical order).
With argument N not nil or 1, move forward N - 1 lines first.
If point reaches the beginning or end of buffer, it stops there.
To ignore intangibility, bind `inhibit-point-motion-hooks' to t.

This function constrains point to the current field unless this moves
point to a different line than the original, unconstrained result.  If
N is nil or 1, and a rear-sticky field ends at point, the point does
not move.  To ignore field boundaries bind `inhibit-field-text-motion'
to t.  */)
  (Lisp_Object n)
{
  EMACS_INT newpos;

  if (NILP (n))
    XSETFASTINT (n, 1);
  else
    CHECK_NUMBER (n);

  while (1)
    {
      newpos = XINT (Fline_end_position (n));
      SET_PT (newpos);

      if (PT > newpos
	  && FETCH_CHAR (PT - 1) == '\n')
	{
	  /* If we skipped over a newline that follows
	     an invisible intangible run,
	     move back to the last tangible position
	     within the line.  */

	  SET_PT (PT - 1);
	  break;
	}
      else if (PT > newpos && PT < ZV
	       && FETCH_CHAR (PT) != '\n')
	/* If we skipped something intangible
	   and now we're not really at eol,
	   keep going.  */
	n = make_number (1);
      else
	break;
    }

  return Qnil;
}

DEFUN ("delete-char", Fdelete_char, Sdelete_char, 1, 2, "p\nP",
       doc: /* Delete the following N characters (previous if N is negative).
Optional second arg KILLFLAG non-nil means kill instead (save in kill ring).
Interactively, N is the prefix arg, and KILLFLAG is set if
N was explicitly specified.

The command `delete-forward-char' is preferable for interactive use.  */)
  (Lisp_Object n, Lisp_Object killflag)
{
  EMACS_INT pos;

  CHECK_NUMBER (n);

  pos = PT + XINT (n);
  if (NILP (killflag))
    {
      if (XINT (n) < 0)
	{
	  if (pos < BEGV)
	    xsignal0 (Qbeginning_of_buffer);
	  else
	    del_range (pos, PT);
	}
      else
	{
	  if (pos > ZV)
	    xsignal0 (Qend_of_buffer);
	  else
	    del_range (PT, pos);
	}
    }
  else
    {
      call1 (Qkill_forward_chars, n);
    }
  return Qnil;
}

static int nonundocount;

/* Note that there's code in command_loop_1 which typically avoids
   calling this.  */
DEFUN ("self-insert-command", Fself_insert_command, Sself_insert_command, 1, 1, "p",
       doc: /* Insert the character you type.
Whichever character you type to run this command is inserted.
Before insertion, `expand-abbrev' is executed if the inserted character does
not have word syntax and the previous character in the buffer does.
After insertion, the value of `auto-fill-function' is called if the
`auto-fill-chars' table has a non-nil value for the inserted character.
At the end, it runs `post-self-insert-hook'.  */)
  (Lisp_Object n)
{
  int remove_boundary = 1;
  CHECK_NATNUM (n);

  if (!EQ (Vthis_command, KVAR (current_kboard, Vlast_command)))
    nonundocount = 0;

  if (NILP (Vexecuting_kbd_macro)
      && !EQ (minibuf_window, selected_window))
    {
      if (nonundocount <= 0 || nonundocount >= 20)
	{
	  remove_boundary = 0;
	  nonundocount = 0;
	}
      nonundocount++;
    }

  if (remove_boundary
      && CONSP (BVAR (current_buffer, undo_list))
      && NILP (XCAR (BVAR (current_buffer, undo_list))))
    /* Remove the undo_boundary that was just pushed.  */
    BVAR (current_buffer, undo_list) = XCDR (BVAR (current_buffer, undo_list));

  /* Barf if the key that invoked this was not a character.  */
  if (!CHARACTERP (last_command_event))
    bitch_at_user ();
  {
    int character = translate_char (Vtranslation_table_for_input,
				    (int) XINT (last_command_event));
    int val = internal_self_insert (character, XFASTINT (n));
    if (val == 2)
      nonundocount = 0;
    frame_make_pointer_invisible ();
  }

  return Qnil;
}

/* Insert N times character C

   If this insertion is suitable for direct output (completely simple),
   return 0.  A value of 1 indicates this *might* not have been simple.
   A value of 2 means this did things that call for an undo boundary.  */

static Lisp_Object Qexpand_abbrev;
static Lisp_Object Qpost_self_insert_hook;

static int
internal_self_insert (int c, EMACS_INT n)
{
  int hairy = 0;
  Lisp_Object tem;
  register enum syntaxcode synt;
  Lisp_Object overwrite;
  /* Length of multi-byte form of C.  */
  int len;
  /* Working buffer and pointer for multi-byte form of C.  */
  unsigned char str[MAX_MULTIBYTE_LENGTH];
  EMACS_INT chars_to_delete = 0;
  EMACS_INT spaces_to_insert = 0;

  overwrite = BVAR (current_buffer, overwrite_mode);
  if (!NILP (Vbefore_change_functions) || !NILP (Vafter_change_functions))
    hairy = 1;

  /* At first, get multi-byte form of C in STR.  */
  if (!NILP (BVAR (current_buffer, enable_multibyte_characters)))
    {
      len = CHAR_STRING (c, str);
      if (len == 1)
	/* If C has modifier bits, this makes C an appropriate
           one-byte char.  */
	c = *str;
    }
  else
    {
      str[0] = (SINGLE_BYTE_CHAR_P (c)
		? c
		: multibyte_char_to_unibyte (c));
      len = 1;
    }
  if (!NILP (overwrite)
      && PT < ZV)
    {
      /* In overwrite-mode, we substitute a character at point (C2,
	 hereafter) by C.  For that, we delete C2 in advance.  But,
	 just substituting C2 by C may move a remaining text in the
	 line to the right or to the left, which is not preferable.
	 So we insert more spaces or delete more characters in the
	 following cases: if C is narrower than C2, after deleting C2,
	 we fill columns with spaces, if C is wider than C2, we delete
	 C2 and several characters following C2.  */

      /* This is the character after point.  */
      int c2 = FETCH_CHAR (PT_BYTE);

      /* Overwriting in binary-mode always replaces C2 by C.
	 Overwriting in textual-mode doesn't always do that.
	 It inserts newlines in the usual way,
	 and inserts any character at end of line
	 or before a tab if it doesn't use the whole width of the tab.  */
      if (EQ (overwrite, Qoverwrite_mode_binary))
	chars_to_delete = n;
      else if (c != '\n' && c2 != '\n')
	{
	  EMACS_INT pos = PT;
	  EMACS_INT pos_byte = PT_BYTE;

	  /* FIXME: Check for integer overflow when calculating
	     target_clm and actual_clm.  */

	  /* Column the cursor should be placed at after this insertion.
	     The correct value should be calculated only when necessary.  */
	  EMACS_INT target_clm = (current_column ()
				  + n * XINT (Fchar_width (make_number (c))));

	  /* The actual cursor position after the trial of moving
	     to column TARGET_CLM.  It is greater than TARGET_CLM
	     if the TARGET_CLM is middle of multi-column
	     character.  In that case, the new point is set after
	     that character.  */
	  EMACS_INT actual_clm
	    = XFASTINT (Fmove_to_column (make_number (target_clm), Qnil));

	  chars_to_delete = PT - pos;

	  if (actual_clm > target_clm)
	    {
	      /* We will delete too many columns.  Let's fill columns
		 by spaces so that the remaining text won't move.  */
	      EMACS_INT actual = PT_BYTE;
	      DEC_POS (actual);
	      if (FETCH_CHAR (actual) == '\t')
		/* Rather than add spaces, let's just keep the tab. */
		chars_to_delete--;
	      else
		spaces_to_insert = actual_clm - target_clm;
	    }

	  SET_PT_BOTH (pos, pos_byte);
	}
      hairy = 2;
    }

  synt = SYNTAX (c);

  if (!NILP (BVAR (current_buffer, abbrev_mode))
      && synt != Sword
      && NILP (BVAR (current_buffer, read_only))
      && PT > BEGV
      && (SYNTAX (!NILP (BVAR (current_buffer, enable_multibyte_characters))
		  ? XFASTINT (Fprevious_char ())
		  : UNIBYTE_TO_CHAR (XFASTINT (Fprevious_char ())))
	  == Sword))
    {
      int modiff = MODIFF;
      Lisp_Object sym;

      sym = call0 (Qexpand_abbrev);

      /* If we expanded an abbrev which has a hook,
	 and the hook has a non-nil `no-self-insert' property,
	 return right away--don't really self-insert.  */
      if (SYMBOLP (sym) && ! NILP (sym) && ! NILP (XSYMBOL (sym)->function)
	  && SYMBOLP (XSYMBOL (sym)->function))
	{
	  Lisp_Object prop;
	  prop = Fget (XSYMBOL (sym)->function, intern ("no-self-insert"));
	  if (! NILP (prop))
	    return 1;
	}

      if (MODIFF != modiff)
	hairy = 2;
    }

  if (chars_to_delete)
    {
      int mc = ((NILP (BVAR (current_buffer, enable_multibyte_characters))
		 && SINGLE_BYTE_CHAR_P (c))
		? UNIBYTE_TO_CHAR (c) : c);
      Lisp_Object string = Fmake_string (make_number (n), make_number (mc));

      if (spaces_to_insert)
	{
	  tem = Fmake_string (make_number (spaces_to_insert),
			      make_number (' '));
	  string = concat2 (string, tem);
	}

      replace_range (PT, PT + chars_to_delete, string, 1, 1, 1);
      Fforward_char (make_number (n + spaces_to_insert));
    }
  else if (n > 1)
    {
      USE_SAFE_ALLOCA;
      char *strn, *p;
      SAFE_NALLOCA (strn, len, n);
      for (p = strn; n > 0; n--, p += len)
	memcpy (p, str, len);
      insert_and_inherit (strn, p - strn);
      SAFE_FREE ();
    }
  else if (n > 0)
    insert_and_inherit ((char *) str, len);

  if ((CHAR_TABLE_P (Vauto_fill_chars)
       ? !NILP (CHAR_TABLE_REF (Vauto_fill_chars, c))
       : (c == ' ' || c == '\n'))
      && !NILP (BVAR (current_buffer, auto_fill_function)))
    {
      Lisp_Object auto_fill_result;

      if (c == '\n')
	/* After inserting a newline, move to previous line and fill
	   that.  Must have the newline in place already so filling and
	   justification, if any, know where the end is going to be.  */
	SET_PT_BOTH (PT - 1, PT_BYTE - 1);
      auto_fill_result = call0 (BVAR (current_buffer, auto_fill_function));
      /* Test PT < ZV in case the auto-fill-function is strange.  */
      if (c == '\n' && PT < ZV)
	SET_PT_BOTH (PT + 1, PT_BYTE + 1);
      if (!NILP (auto_fill_result))
	hairy = 2;
    }

  /* Run hooks for electric keys.  */
  Frun_hooks (1, &Qpost_self_insert_hook);

  return hairy;
}

/* module initialization */

void
syms_of_cmds (void)
{
  DEFSYM (Qkill_backward_chars, "kill-backward-chars");
  DEFSYM (Qkill_forward_chars, "kill-forward-chars");
  DEFSYM (Qoverwrite_mode_binary, "overwrite-mode-binary");
  DEFSYM (Qexpand_abbrev, "expand-abbrev");
  DEFSYM (Qpost_self_insert_hook, "post-self-insert-hook");

  DEFVAR_LISP ("post-self-insert-hook", Vpost_self_insert_hook,
	       doc: /* Hook run at the end of `self-insert-command'.
This is run after inserting the character.  */);
  Vpost_self_insert_hook = Qnil;

  defsubr (&Sforward_point);
  defsubr (&Sforward_char);
  defsubr (&Sbackward_char);
  defsubr (&Sforward_line);
  defsubr (&Sbeginning_of_line);
  defsubr (&Send_of_line);

  defsubr (&Sdelete_char);
  defsubr (&Sself_insert_command);
}

void
keys_of_cmds (void)
{
  int n;

  nonundocount = 0;
  initial_define_key (global_map, Ctl ('I'), "self-insert-command");
  for (n = 040; n < 0177; n++)
    initial_define_key (global_map, n, "self-insert-command");
#ifdef MSDOS
  for (n = 0200; n < 0240; n++)
    initial_define_key (global_map, n, "self-insert-command");
#endif
  for (n = 0240; n < 0400; n++)
    initial_define_key (global_map, n, "self-insert-command");

  initial_define_key (global_map, Ctl ('A'), "beginning-of-line");
  initial_define_key (global_map, Ctl ('B'), "backward-char");
  initial_define_key (global_map, Ctl ('E'), "end-of-line");
  initial_define_key (global_map, Ctl ('F'), "forward-char");
}
