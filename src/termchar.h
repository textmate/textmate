/* Flags and parameters describing terminal's characteristics.
   Copyright (C) 1985-1986, 2001-2012  Free Software Foundation, Inc.

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

/* Each termcap frame points to its own struct tty_output object in
   the output_data.tty field.  The tty_output structure contains the
   information that is specific to termcap frames. */

struct tty_output
{
  /* The Emacs structure for the tty device this frame is on. */
  struct tty_display_info *display_info;

  /* There is nothing else here at the moment... */
};

/* Parameters that are shared between frames on the same tty device. */

struct tty_display_info
{
  struct tty_display_info *next; /* Chain of all tty devices. */

  char *name;                   /* The name of the device file or 0 if
                                   stdin/stdout. */
  char *type;                   /* The type of the tty. */

  /* Input/output */

  FILE *input;                  /* The stream to be used for terminal input.
                                   NULL if the terminal is suspended. */
  FILE *output;                 /* The stream to be used for terminal output.
                                   NULL if the terminal is suspended. */

  FILE *termscript;             /* If nonzero, send all terminal output
                                   characters to this stream also.  */

  struct emacs_tty *old_tty;    /* The initial tty mode bits */

  int term_initted;             /* 1 if we have been through init_sys_modes. */


  int reference_count;          /* Number of frames that are on this display. */

  struct terminal *terminal;    /* Points back to the generic terminal
                                   structure.  This is sometimes handy. */

  /* Info on cursor positioning.  */
  struct cm *Wcm;

  /* Redisplay. */

  Lisp_Object top_frame;        /* The topmost frame on this tty. */

  /* The previous frame we displayed on this tty.  */
  struct frame *previous_frame;
  int previous_color_mode;

  /* Information about the range of text currently shown in
     mouse-face.  */
  Mouse_HLInfo mouse_highlight;

  /* Buffer used internally by termcap (see tgetent in the Termcap
     manual).  Only init_tty and delete_tty should change this.  */
  char *termcap_term_buffer;

  /* Buffer storing terminal description strings (see tgetstr in the
     Termcap manual).  Only init_tty and delete_tty should change
     this.  */
  char *termcap_strings_buffer;

  /* Strings, numbers and flags taken from the termcap entry.  */

  const char *TS_ins_line;	/* "al" */
  const char *TS_ins_multi_lines; /* "AL" (one parameter, # lines to insert) */
  const char *TS_bell;		/* "bl" */
  const char *TS_clr_to_bottom;	/* "cd" */
  const char *TS_clr_line;	/* "ce", clear to end of line */
  const char *TS_clr_frame;	/* "cl" */
  const char *TS_set_scroll_region; /* "cs" (2 params, first line and last line) */
  const char *TS_set_scroll_region_1; /* "cS" (4 params: total lines,
                                   lines above scroll region, lines below it,
                                   total lines again) */
  const char *TS_del_char;	/* "dc" */
  const char *TS_del_multi_chars; /* "DC" (one parameter, # chars to delete) */
  const char *TS_del_line;	/* "dl" */
  const char *TS_del_multi_lines; /* "DL" (one parameter, # lines to delete) */
  const char *TS_delete_mode;	/* "dm", enter character-delete mode */
  const char *TS_end_delete_mode; /* "ed", leave character-delete mode */
  const char *TS_end_insert_mode; /* "ei", leave character-insert mode */
  const char *TS_ins_char;	/* "ic" */
  const char *TS_ins_multi_chars; /* "IC" (one parameter, # chars to insert) */
  const char *TS_insert_mode;	/* "im", enter character-insert mode */
  const char *TS_pad_inserted_char; /* "ip".  Just padding, no commands.  */
  const char *TS_end_keypad_mode; /* "ke" */
  const char *TS_keypad_mode;	/* "ks" */
  const char *TS_pad_char;	/* "pc", char to use as padding */
  const char *TS_repeat;	/* "rp" (2 params, # times to repeat
				   and character to be repeated) */
  const char *TS_end_standout_mode; /* "se" */
  const char *TS_fwd_scroll;	/* "sf" */
  const char *TS_standout_mode;	/* "so" */
  const char *TS_rev_scroll;	/* "sr" */
  const char *TS_end_termcap_modes; /* "te" */
  const char *TS_termcap_modes;	/* "ti" */
  const char *TS_visible_bell;	/* "vb" */
  const char *TS_cursor_normal;	/* "ve" */
  const char *TS_cursor_visible; /* "vs" */
  const char *TS_cursor_invisible; /* "vi" */
  const char *TS_set_window;	/* "wi" (4 params, start and end of window,
                                   each as vpos and hpos) */

  const char *TS_enter_bold_mode; /* "md" -- turn on bold (extra bright mode).  */
  const char *TS_enter_dim_mode; /* "mh" -- turn on half-bright mode.  */
  const char *TS_enter_blink_mode; /* "mb" -- enter blinking mode.  */
  const char *TS_enter_reverse_mode; /* "mr" -- enter reverse video mode.  */
  const char *TS_exit_underline_mode; /* "us" -- start underlining.  */
  const char *TS_enter_underline_mode; /* "ue" -- end underlining.  */

  /* "as"/"ae" -- start/end alternate character set.  Not really
     supported, yet.  */
  const char *TS_enter_alt_charset_mode;
  const char *TS_exit_alt_charset_mode;

  const char *TS_exit_attribute_mode; /* "me" -- switch appearances off.  */

  /* Value of the "NC" (no_color_video) capability, or 0 if not present.  */
  int TN_no_color_video;

  int TN_max_colors;            /* "Co" -- number of colors.  */

  /* "pa" -- max. number of color pairs on screen.  Not handled yet.
     Could be a problem if not equal to TN_max_colors * TN_max_colors.  */
  int TN_max_pairs;

  /* "op" -- SVr4 set default pair to its original value.  */
  const char *TS_orig_pair;

  /* "AF"/"AB" or "Sf"/"Sb"-- set ANSI or SVr4 foreground/background color.
     1 param, the color index.  */
  const char *TS_set_foreground;
  const char *TS_set_background;

  int TF_hazeltine;             /* termcap hz flag. */
  int TF_insmode_motion;        /* termcap mi flag: can move while in insert mode. */
  int TF_standout_motion;       /* termcap mi flag: can move while in standout mode. */
  int TF_underscore;            /* termcap ul flag: _ underlines if over-struck on
                                   non-blank position.  Must clear before writing _.  */
  int TF_teleray;               /* termcap xt flag: many weird consequences.
                                   For t1061. */

  int RPov;                     /* # chars to start a TS_repeat */

  int delete_in_insert_mode;    /* delete mode == insert mode */

  int se_is_so;                 /* 1 if same string both enters and leaves
                                   standout mode */

  int costs_set;                /* Nonzero if costs have been calculated. */

  int insert_mode;              /* Nonzero when in insert mode.  */
  int standout_mode;            /* Nonzero when in standout mode.  */



  /* 1 if should obey 0200 bit in input chars as "Meta", 2 if should
     keep 0200 bit in input chars.  0 to ignore the 0200 bit.  */

  int meta_key;

  /* Size of window specified by higher levels.
   This is the number of lines, from the top of frame downwards,
   which can participate in insert-line/delete-line operations.

   Effectively it excludes the bottom frame_lines - specified_window_size
   lines from those operations.  */

  int specified_window;

  /* Flag used in tty_show/hide_cursor.  */

  int cursor_hidden;

  /* Nonzero means use ^S/^Q for flow control.  */
  int flow_control;

};

/* A chain of structures for all tty devices currently in use. */
extern struct tty_display_info *tty_list;


#define FRAME_TTY(f)                            \
  (((f)->output_method == output_termcap	\
    || (f)->output_method == output_msdos_raw)	\
   ? (f)->terminal->display_info.tty            \
   : (abort (), (struct tty_display_info *) 0))

#define CURTTY() FRAME_TTY (SELECTED_FRAME())
