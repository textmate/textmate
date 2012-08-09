/* Definitions needed by most editing commands.
   Copyright (C) 1985, 1994, 2001-2012  Free Software Foundation, Inc.

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


#define Ctl(c) ((c)&037)

/* Define the names of keymaps, just so people can refer to them in
   calls to initial_define_key.  These should *not* be used after
   initialization; use-global-map doesn't affect these; it sets
   current_global_map instead.  */
extern Lisp_Object global_map;
extern Lisp_Object meta_map;
extern Lisp_Object control_x_map;

/* If not Qnil, this is a switch-frame event which we decided to put
   off until the end of a key sequence.  This should be read as the
   next command input, after any Vunread_command_events.

   read_key_sequence uses this to delay switch-frame events until the
   end of the key sequence; Fread_char uses it to put off switch-frame
   events until a non-ASCII event is acceptable as input.  */
extern Lisp_Object unread_switch_frame;

/* Nonzero means ^G can quit instantly */
extern int immediate_quit;

/* Nonzero if input is coming from the keyboard */

#define INTERACTIVE (NILP (Vexecuting_kbd_macro) && !noninteractive)

/* Set this nonzero to force reconsideration of mode line. */

extern int update_mode_lines;
