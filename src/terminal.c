/* Functions related to terminal devices.
   Copyright (C) 2005-2012 Free Software Foundation, Inc.

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
#include "frame.h"
#include "termchar.h"
#include "termhooks.h"
#include "charset.h"
#include "coding.h"
#include "keyboard.h"

/* Chain of all terminals currently in use. */
struct terminal *terminal_list;

/* The first unallocated terminal id. */
static int next_terminal_id;

/* The initial terminal device, created by initial_term_init. */
struct terminal *initial_terminal;

static void delete_initial_terminal (struct terminal *);



void
ring_bell (struct frame *f)
{
  if (!NILP (Vring_bell_function))
    {
      Lisp_Object function;

      /* Temporarily set the global variable to nil
	 so that if we get an error, it stays nil
	 and we don't call it over and over.

	 We don't specbind it, because that would carefully
	 restore the bad value if there's an error
	 and make the loop of errors happen anyway.  */

      function = Vring_bell_function;
      Vring_bell_function = Qnil;

      call0 (function);

      Vring_bell_function = function;
    }
  else if (FRAME_TERMINAL (f)->ring_bell_hook)
    (*FRAME_TERMINAL (f)->ring_bell_hook) (f);
}

void
update_begin (struct frame *f)
{
  if (FRAME_TERMINAL (f)->update_begin_hook)
    (*FRAME_TERMINAL (f)->update_begin_hook) (f);
}

void
update_end (struct frame *f)
{
  if (FRAME_TERMINAL (f)->update_end_hook)
    (*FRAME_TERMINAL (f)->update_end_hook) (f);
}

/* Specify how many text lines, from the top of the window,
   should be affected by insert-lines and delete-lines operations.
   This, and those operations, are used only within an update
   that is bounded by calls to update_begin and update_end.  */

void
set_terminal_window (struct frame *f, int size)
{
  if (FRAME_TERMINAL (f)->set_terminal_window_hook)
    (*FRAME_TERMINAL (f)->set_terminal_window_hook) (f, size);
}

/* Move cursor to row/column position VPOS/HPOS.  HPOS/VPOS are
   frame-relative coordinates.  */

void
cursor_to (struct frame *f, int vpos, int hpos)
{
  if (FRAME_TERMINAL (f)->cursor_to_hook)
    (*FRAME_TERMINAL (f)->cursor_to_hook) (f, vpos, hpos);
}

/* Similar but don't take any account of the wasted characters.  */

void
raw_cursor_to (struct frame *f, int row, int col)
{
  if (FRAME_TERMINAL (f)->raw_cursor_to_hook)
    (*FRAME_TERMINAL (f)->raw_cursor_to_hook) (f, row, col);
}

/* Erase operations */

/* Clear from cursor to end of frame. */
void
clear_to_end (struct frame *f)
{
  if (FRAME_TERMINAL (f)->clear_to_end_hook)
    (*FRAME_TERMINAL (f)->clear_to_end_hook) (f);
}

/* Clear entire frame */

void
clear_frame (struct frame *f)
{
  if (FRAME_TERMINAL (f)->clear_frame_hook)
    (*FRAME_TERMINAL (f)->clear_frame_hook) (f);
}

/* Clear from cursor to end of line.
   Assume that the line is already clear starting at column first_unused_hpos.

   Note that the cursor may be moved, on terminals lacking a `ce' string.  */

void
clear_end_of_line (struct frame *f, int first_unused_hpos)
{
  if (FRAME_TERMINAL (f)->clear_end_of_line_hook)
    (*FRAME_TERMINAL (f)->clear_end_of_line_hook) (f, first_unused_hpos);
}

/* Output LEN glyphs starting at STRING at the nominal cursor position.
   Advance the nominal cursor over the text.  */

void
write_glyphs (struct frame *f, struct glyph *string, int len)
{
  if (FRAME_TERMINAL (f)->write_glyphs_hook)
    (*FRAME_TERMINAL (f)->write_glyphs_hook) (f, string, len);
}

/* Insert LEN glyphs from START at the nominal cursor position.

   If start is zero, insert blanks instead of a string at start */

void
insert_glyphs (struct frame *f, struct glyph *start, int len)
{
  if (len <= 0)
    return;

  if (FRAME_TERMINAL (f)->insert_glyphs_hook)
    (*FRAME_TERMINAL (f)->insert_glyphs_hook) (f, start, len);
}

/* Delete N glyphs at the nominal cursor position. */

void
delete_glyphs (struct frame *f, int n)
{
  if (FRAME_TERMINAL (f)->delete_glyphs_hook)
    (*FRAME_TERMINAL (f)->delete_glyphs_hook) (f, n);
}

/* Insert N lines at vpos VPOS.  If N is negative, delete -N lines.  */

void
ins_del_lines (struct frame *f, int vpos, int n)
{
  if (FRAME_TERMINAL (f)->ins_del_lines_hook)
    (*FRAME_TERMINAL (f)->ins_del_lines_hook) (f, vpos, n);
}




/* Return the terminal object specified by TERMINAL.  TERMINAL may be
   a terminal object, a frame, or nil for the terminal device of the
   current frame.  If THROW is zero, return NULL for failure,
   otherwise throw an error.  */

struct terminal *
get_terminal (Lisp_Object terminal, int throw)
{
  struct terminal *result = NULL;

  if (NILP (terminal))
    terminal = selected_frame;

  if (TERMINALP (terminal))
    result = XTERMINAL (terminal);
  else if (FRAMEP (terminal))
    result = FRAME_TERMINAL (XFRAME (terminal));

  if (result && !result->name)
    result = NULL;

  if (result == NULL && throw)
    wrong_type_argument (Qterminal_live_p, terminal);

  return result;
}



/* Create a new terminal object and add it to the terminal list. */

struct terminal *
create_terminal (void)
{
  struct terminal *terminal = allocate_terminal ();
  Lisp_Object terminal_coding, keyboard_coding;

  terminal->name = NULL;
  terminal->next_terminal = terminal_list;
  terminal_list = terminal;

  terminal->id = next_terminal_id++;

  terminal->keyboard_coding =
    (struct coding_system *) xmalloc (sizeof (struct coding_system));
  terminal->terminal_coding =
    (struct coding_system *) xmalloc (sizeof (struct coding_system));

  /* If default coding systems for the terminal and the keyboard are
     already defined, use them in preference to the defaults.  This is
     needed when Emacs runs in daemon mode.  */
  keyboard_coding =
    find_symbol_value (intern ("default-keyboard-coding-system"));
  if (NILP (keyboard_coding)
      || EQ (keyboard_coding, Qunbound)
      || NILP (Fcoding_system_p (keyboard_coding)))
    keyboard_coding = Qno_conversion;
  terminal_coding =
    find_symbol_value (intern ("default-terminal-coding-system"));
  if (NILP (terminal_coding)
      || EQ (terminal_coding, Qunbound)
      || NILP (Fcoding_system_p (terminal_coding)))
    terminal_coding = Qundecided;

  setup_coding_system (keyboard_coding, terminal->keyboard_coding);
  setup_coding_system (terminal_coding, terminal->terminal_coding);

  terminal->param_alist = Qnil;
  terminal->charset_list = Qnil;
  terminal->Vselection_alist = Qnil;
  return terminal;
}

/* Low-level function to close all frames on a terminal, remove it
   from the terminal list and free its memory.  */

void
delete_terminal (struct terminal *terminal)
{
  struct terminal **tp;
  Lisp_Object tail, frame;

  /* Protect against recursive calls.  delete_frame calls the
     delete_terminal_hook when we delete our last frame.  */
  if (!terminal->name)
    return;
  xfree (terminal->name);
  terminal->name = NULL;

  /* Check for live frames that are still on this terminal. */
  FOR_EACH_FRAME (tail, frame)
    {
      struct frame *f = XFRAME (frame);
      if (FRAME_LIVE_P (f) && f->terminal == terminal)
        {
	  /* Pass Qnoelisp rather than Qt.  */
          delete_frame (frame, Qnoelisp);
        }
    }

  for (tp = &terminal_list; *tp != terminal; tp = &(*tp)->next_terminal)
    if (! *tp)
      abort ();
  *tp = terminal->next_terminal;

  xfree (terminal->keyboard_coding);
  terminal->keyboard_coding = NULL;
  xfree (terminal->terminal_coding);
  terminal->terminal_coding = NULL;

  if (terminal->kboard && --terminal->kboard->reference_count == 0)
    {
      delete_kboard (terminal->kboard);
      terminal->kboard = NULL;
    }
}

Lisp_Object Qrun_hook_with_args;
static Lisp_Object Qdelete_terminal_functions;
DEFUN ("delete-terminal", Fdelete_terminal, Sdelete_terminal, 0, 2, 0,
       doc: /* Delete TERMINAL by deleting all frames on it and closing the terminal.
TERMINAL may be a terminal object, a frame, or nil (meaning the
selected frame's terminal).

Normally, you may not delete a display if all other displays are suspended,
but if the second argument FORCE is non-nil, you may do so. */)
  (Lisp_Object terminal, Lisp_Object force)
{
  struct terminal *t = get_terminal (terminal, 0);

  if (!t)
    return Qnil;

  if (NILP (force))
    {
      struct terminal *p = terminal_list;
      while (p && (p == t || !TERMINAL_ACTIVE_P (p)))
	p = p->next_terminal;

      if (!p)
	error ("Attempt to delete the sole active display terminal");
    }

  if (NILP (Vrun_hooks))
    ;
  else if (EQ (force, Qnoelisp))
    pending_funcalls
      = Fcons (list3 (Qrun_hook_with_args,
		      Qdelete_terminal_functions, terminal),
	       pending_funcalls);
  else
    safe_call2 (Qrun_hook_with_args, Qdelete_terminal_functions, terminal);

  if (t->delete_terminal_hook)
    (*t->delete_terminal_hook) (t);
  else
    delete_terminal (t);

  return Qnil;
}


DEFUN ("frame-terminal", Fframe_terminal, Sframe_terminal, 0, 1, 0,
       doc: /* Return the terminal that FRAME is displayed on.
If FRAME is nil, the selected frame is used.

The terminal device is represented by its integer identifier.  */)
  (Lisp_Object frame)
{
  struct terminal *t;

  if (NILP (frame))
    frame = selected_frame;

  CHECK_LIVE_FRAME (frame);

  t = FRAME_TERMINAL (XFRAME (frame));

  if (!t)
    return Qnil;
  else
    {
      Lisp_Object terminal;
      XSETTERMINAL (terminal, t);
      return terminal;
    }
}

DEFUN ("terminal-live-p", Fterminal_live_p, Sterminal_live_p, 1, 1, 0,
       doc: /* Return non-nil if OBJECT is a terminal which has not been deleted.
Value is nil if OBJECT is not a live display terminal.
If object is a live display terminal, the return value indicates what
sort of output terminal it uses.  See the documentation of `framep' for
possible return values.  */)
  (Lisp_Object object)
{
  struct terminal *t;

  t = get_terminal (object, 0);

  if (!t)
    return Qnil;

  switch (t->type)
    {
    case output_initial: /* The initial frame is like a termcap frame. */
    case output_termcap:
      return Qt;
    case output_x_window:
      return Qx;
    case output_w32:
      return Qw32;
    case output_msdos_raw:
      return Qpc;
    case output_mac:
      return Qmac;
    case output_ns:
      return Qns;
    default:
      abort ();
    }
}

DEFUN ("terminal-list", Fterminal_list, Sterminal_list, 0, 0, 0,
       doc: /* Return a list of all terminal devices.  */)
  (void)
{
  Lisp_Object terminal, terminals = Qnil;
  struct terminal *t;

  for (t = terminal_list; t; t = t->next_terminal)
    {
      XSETTERMINAL (terminal, t);
      terminals = Fcons (terminal, terminals);
    }

  return terminals;
}

DEFUN ("terminal-name", Fterminal_name, Sterminal_name, 0, 1, 0,
       doc: /* Return the name of the terminal device TERMINAL.
It is not guaranteed that the returned value is unique among opened devices.

TERMINAL may be a terminal object, a frame, or nil (meaning the
selected frame's terminal). */)
  (Lisp_Object terminal)
{
  struct terminal *t
    = TERMINALP (terminal) ? XTERMINAL (terminal) : get_terminal (terminal, 1);

  return t->name ? build_string (t->name) : Qnil;
}



/* Set the value of terminal parameter PARAMETER in terminal D to VALUE.
   Return the previous value.  */

static Lisp_Object
store_terminal_param (struct terminal *t, Lisp_Object parameter, Lisp_Object value)
{
  Lisp_Object old_alist_elt = Fassq (parameter, t->param_alist);
  if (EQ (old_alist_elt, Qnil))
    {
      t->param_alist = Fcons (Fcons (parameter, value), t->param_alist);
      return Qnil;
    }
  else
    {
      Lisp_Object result = Fcdr (old_alist_elt);
      Fsetcdr (old_alist_elt, value);
      return result;
    }
}


DEFUN ("terminal-parameters", Fterminal_parameters, Sterminal_parameters, 0, 1, 0,
       doc: /* Return the parameter-alist of terminal TERMINAL.
The value is a list of elements of the form (PARM . VALUE), where PARM
is a symbol.

TERMINAL can be a terminal object, a frame, or nil (meaning the
selected frame's terminal).  */)
  (Lisp_Object terminal)
{
  struct terminal *t
    = TERMINALP (terminal) ? XTERMINAL (terminal) : get_terminal (terminal, 1);
  return Fcopy_alist (t->param_alist);
}

DEFUN ("terminal-parameter", Fterminal_parameter, Sterminal_parameter, 2, 2, 0,
       doc: /* Return TERMINAL's value for parameter PARAMETER.
TERMINAL can be a terminal object, a frame, or nil (meaning the
selected frame's terminal).  */)
  (Lisp_Object terminal, Lisp_Object parameter)
{
  Lisp_Object value;
  struct terminal *t
    = TERMINALP (terminal) ? XTERMINAL (terminal) : get_terminal (terminal, 1);
  CHECK_SYMBOL (parameter);
  value = Fcdr (Fassq (parameter, t->param_alist));
  return value;
}

DEFUN ("set-terminal-parameter", Fset_terminal_parameter,
       Sset_terminal_parameter, 3, 3, 0,
       doc: /* Set TERMINAL's value for parameter PARAMETER to VALUE.
Return the previous value of PARAMETER.

TERMINAL can be a terminal object, a frame or nil (meaning the
selected frame's terminal).  */)
  (Lisp_Object terminal, Lisp_Object parameter, Lisp_Object value)
{
  struct terminal *t
    = TERMINALP (terminal) ? XTERMINAL (terminal) : get_terminal (terminal, 1);
  return store_terminal_param (t, parameter, value);
}



/* Create the bootstrap display terminal for the initial frame.
   Returns a terminal of type output_initial.  */

struct terminal *
init_initial_terminal (void)
{
  if (initialized || terminal_list || tty_list)
    abort ();

  initial_terminal = create_terminal ();
  initial_terminal->type = output_initial;
  initial_terminal->name = xstrdup ("initial_terminal");
  initial_terminal->kboard = initial_kboard;
  initial_terminal->delete_terminal_hook = &delete_initial_terminal;
  /* All other hooks are NULL. */

  return initial_terminal;
}

/* Deletes the bootstrap terminal device.
   Called through delete_terminal_hook. */

static void
delete_initial_terminal (struct terminal *terminal)
{
  if (terminal != initial_terminal)
    abort ();

  delete_terminal (terminal);
  initial_terminal = NULL;
}

void
syms_of_terminal (void)
{

  DEFVAR_LISP ("ring-bell-function", Vring_bell_function,
    doc: /* Non-nil means call this function to ring the bell.
The function should accept no arguments.  */);
  Vring_bell_function = Qnil;

  DEFVAR_LISP ("delete-terminal-functions", Vdelete_terminal_functions,
    doc: /* Special hook run when a terminal is deleted.
Each function is called with argument, the terminal.
This may be called just before actually deleting the terminal,
or some time later.  */);
  Vdelete_terminal_functions = Qnil;
  DEFSYM (Qdelete_terminal_functions, "delete-terminal-functions");
  DEFSYM (Qrun_hook_with_args, "run-hook-with-args");

  defsubr (&Sdelete_terminal);
  defsubr (&Sframe_terminal);
  defsubr (&Sterminal_live_p);
  defsubr (&Sterminal_list);
  defsubr (&Sterminal_name);
  defsubr (&Sterminal_parameters);
  defsubr (&Sterminal_parameter);
  defsubr (&Sset_terminal_parameter);

  Fprovide (intern_c_string ("multi-tty"), Qnil);
}
