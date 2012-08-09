/* MS-DOS specific Lisp utilities.  Coded by Manabu Higashida, 1991.
   Major changes May-July 1993 Morten Welinder (only 10% original code left)
   Copyright (C) 1991, 1993, 1996-1998, 2001-2012 Free Software Foundation, Inc.

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

#ifdef MSDOS
/* The entire file is within this conditional */

#include <stdio.h>
#include <dos.h>
#include <setjmp.h>
#include "lisp.h"
#include "buffer.h"
#include "termchar.h"
#include "frame.h"
#include "termhooks.h"
#include "blockinput.h"
#include "window.h"
#include "dosfns.h"
#include "msdos.h"
#include "dispextern.h"
#include "character.h"
#include "coding.h"
#include "process.h"
#include <dpmi.h>
#include <go32.h>
#include <dirent.h>
#include <sys/vfs.h>
#include <unistd.h>
#include <grp.h>
#include <crt0.h>

DEFUN ("int86", Fint86, Sint86, 2, 2, 0,
       doc: /* Call specific MS-DOS interrupt number INTERRUPT with REGISTERS.
Return the updated REGISTER vector.

INTERRUPT should be an integer in the range 0 to 255.
REGISTERS should be a vector produced by `make-register' and
`set-register-value'.  */)
  (Lisp_Object interrupt, Lisp_Object registers)
{
  register int i;
  int no;
  union REGS inregs, outregs;

  CHECK_NUMBER (interrupt);
  no = (unsigned long) XINT (interrupt);
  CHECK_VECTOR (registers);
  if (no < 0 || no > 0xff || ASIZE (registers) != 8)
    return Qnil;
  for (i = 0; i < 8; i++)
    CHECK_NUMBER (XVECTOR (registers)->contents[i]);

  inregs.x.ax    = (unsigned long) XFASTINT (XVECTOR (registers)->contents[0]);
  inregs.x.bx    = (unsigned long) XFASTINT (XVECTOR (registers)->contents[1]);
  inregs.x.cx    = (unsigned long) XFASTINT (XVECTOR (registers)->contents[2]);
  inregs.x.dx    = (unsigned long) XFASTINT (XVECTOR (registers)->contents[3]);
  inregs.x.si    = (unsigned long) XFASTINT (XVECTOR (registers)->contents[4]);
  inregs.x.di    = (unsigned long) XFASTINT (XVECTOR (registers)->contents[5]);
  inregs.x.cflag = (unsigned long) XFASTINT (XVECTOR (registers)->contents[6]);
  inregs.x.flags = (unsigned long) XFASTINT (XVECTOR (registers)->contents[7]);

  int86 (no, &inregs, &outregs);

  XVECTOR (registers)->contents[0] = make_number (outregs.x.ax);
  XVECTOR (registers)->contents[1] = make_number (outregs.x.bx);
  XVECTOR (registers)->contents[2] = make_number (outregs.x.cx);
  XVECTOR (registers)->contents[3] = make_number (outregs.x.dx);
  XVECTOR (registers)->contents[4] = make_number (outregs.x.si);
  XVECTOR (registers)->contents[5] = make_number (outregs.x.di);
  XVECTOR (registers)->contents[6] = make_number (outregs.x.cflag);
  XVECTOR (registers)->contents[7] = make_number (outregs.x.flags);

  return registers;
}

DEFUN ("msdos-memget", Fdos_memget, Sdos_memget, 2, 2, 0,
       doc: /* Read DOS memory at offset ADDRESS into VECTOR.
Return the updated VECTOR.  */)
  (Lisp_Object address, Lisp_Object vector)
{
  register int i;
  int offs, len;
  char *buf;

  CHECK_NUMBER (address);
  offs = (unsigned long) XINT (address);
  CHECK_VECTOR (vector);
  len = ASIZE (vector);
  if (len < 1 || len > 2048 || offs < 0 || offs > 0xfffff - len)
    return Qnil;
  buf = alloca (len);
  dosmemget (offs, len, buf);

  for (i = 0; i < len; i++)
    XVECTOR (vector)->contents[i] = make_number (buf[i]);

  return vector;
}

DEFUN ("msdos-memput", Fdos_memput, Sdos_memput, 2, 2, 0,
       doc: /* Write DOS memory at offset ADDRESS from VECTOR.  */)
  (Lisp_Object address, Lisp_Object vector)
{
  register int i;
  int offs, len;
  char *buf;

  CHECK_NUMBER (address);
  offs = (unsigned long) XINT (address);
  CHECK_VECTOR (vector);
  len = ASIZE (vector);
  if (len < 1 || len > 2048 || offs < 0 || offs > 0xfffff - len)
    return Qnil;
  buf = alloca (len);

  for (i = 0; i < len; i++)
    {
      CHECK_NUMBER (XVECTOR (vector)->contents[i]);
      buf[i] = (unsigned char) XFASTINT (XVECTOR (vector)->contents[i]) & 0xFF;
    }

  dosmemput (buf, len, offs);
  return Qt;
}

DEFUN ("msdos-set-keyboard", Fmsdos_set_keyboard, Smsdos_set_keyboard, 1, 2, 0,
       doc: /* Set keyboard layout according to COUNTRY-CODE.
If the optional argument ALLKEYS is non-nil, the keyboard is mapped for
all keys; otherwise it is only used when the ALT key is pressed.
The current keyboard layout is available in dos-keyboard-code.  */)
  (Lisp_Object country_code, Lisp_Object allkeys)
{
  CHECK_NUMBER (country_code);
  if (!dos_set_keyboard (XINT (country_code), !NILP (allkeys)))
    return Qnil;
  return Qt;
}

#ifndef HAVE_X_WINDOWS
/* Later we might want to control the mouse interface with this function,
   e.g., with respect to non-80 column screen modes.  */

DEFUN ("msdos-mouse-p", Fmsdos_mouse_p, Smsdos_mouse_p, 0, 0, 0,
       doc: /* Report whether a mouse is present.  */)
  (void)
{
  if (have_mouse)
    return Qt;
  else
    return Qnil;
}
#endif

DEFUN ("msdos-mouse-init", Fmsdos_mouse_init, Smsdos_mouse_init, 0, 0, "",
       doc: /* Initialize and enable mouse if available.  */)
  (void)
{
  if (have_mouse)
    {
      have_mouse = 1;
      mouse_init ();
      return Qt;
    }
  return Qnil;
}

DEFUN ("msdos-mouse-enable", Fmsdos_mouse_enable, Smsdos_mouse_enable, 0, 0, "",
       doc: /* Enable mouse if available.  */)
  (void)
{
  if (have_mouse)
    {
      have_mouse = 1;
      mouse_on ();
    }
  return have_mouse ? Qt : Qnil;
}

DEFUN ("msdos-mouse-disable", Fmsdos_mouse_disable, Smsdos_mouse_disable, 0, 0, "",
       doc: /* Disable mouse if available.  */)
  (void)
{
  mouse_off ();
  if (have_mouse) have_mouse = -1;
  return Qnil;
}

DEFUN ("insert-startup-screen", Finsert_startup_screen, Sinsert_startup_screen, 0, 0, "",
       doc: /* Insert copy of screen contents prior to starting Emacs.
Return nil if startup screen is not available.  */)
  (void)
{
  char *s;
  int rows, cols, i, j;

  if (!dos_get_saved_screen (&s, &rows, &cols))
    return Qnil;

  for (i = 0; i < rows; i++)
    {
      for (j = 0; j < cols; j++)
	{
	  insert_char (*s);
	  s += 2;
	}
      insert_char ('\n');
    }

  return Qt;
}

unsigned char dos_country_info[DOS_COUNTRY_INFO];
static unsigned char usa_country_info[DOS_COUNTRY_INFO] = {
  0, 0,				/* date format */
  '$', 0, 0, 0, 0,		/* currency string */
  ',', 0,			/* thousands separator */
  '.', 0,			/* decimal separator */
  '/', 0,			/* date separator */
  ':', 0,			/* time separator */
  0,				/* currency format */
  2,				/* digits after decimal in currency */
  0,				/* time format */
  0, 0, 0, 0,			/* address of case map routine, GPF if used */
  ' ', 0,			/* data-list separator (?) */
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0	/* reserved */
};

#ifndef HAVE_X_WINDOWS
static unsigned dos_windows_version;
char parent_vm_title[50];	/* Ralf Brown says 30 is enough */
int w95_set_virtual_machine_title (const char *);

void
restore_parent_vm_title (void)
{
  if (NILP (Vdos_windows_version))
    return;
  if ((dos_windows_version & 0xff) >= 4 && parent_vm_title[0])
    w95_set_virtual_machine_title (parent_vm_title);
  delay (50);
}
#endif /* !HAVE_X_WINDOWS */

void
init_dosfns (void)
{
  union REGS regs;
  _go32_dpmi_registers dpmiregs;
  unsigned long xbuf = _go32_info_block.linear_address_of_transfer_buffer;

#ifndef SYSTEM_MALLOC
  extern void get_lim_data (void);

  get_lim_data (); /* why the hell isn't this called elsewhere? */
#endif

  regs.x.ax = 0x3000;
  intdos (&regs, &regs);
  Vdos_version = Fcons (make_number (regs.h.al), make_number (regs.h.ah));

  /* Obtain the country code via DPMI, use DJGPP transfer buffer.  */
  dpmiregs.x.ax = 0x3800;
  dpmiregs.x.ds = xbuf >> 4;
  dpmiregs.x.dx = 0;
  dpmiregs.x.ss = dpmiregs.x.sp = dpmiregs.x.flags = 0;
  _go32_dpmi_simulate_int (0x21, &dpmiregs);
  if (dpmiregs.x.flags & 1)
    {
      dos_country_code = 1;	/* assume USA if 213800 failed */
      memcpy (dos_country_info, usa_country_info, DOS_COUNTRY_INFO);
    }
  else
    {
      dos_country_code = dpmiregs.x.bx;
      dosmemget (xbuf, DOS_COUNTRY_INFO, dos_country_info);
    }

  dos_set_keyboard (dos_country_code, 0);

  regs.x.ax = 0x6601;
  intdos (&regs, &regs);
  if (regs.x.cflag)
    /* Estimate code page from country code */
    switch (dos_country_code)
      {
      case 45: /* Denmark */
      case 47: /* Norway */
	dos_codepage = 865;
	break;
      default:
	/* US */
	dos_codepage = 437;
      }
  else
    dos_codepage = regs.x.bx & 0xffff;

#ifndef HAVE_X_WINDOWS
  parent_vm_title[0] = '\0';

  /* If we are running from DOS box on MS-Windows, get Windows version.  */
  dpmiregs.x.ax = 0x1600;	/* enhanced mode installation check */
  dpmiregs.x.ss = dpmiregs.x.sp = dpmiregs.x.flags = 0;
  _go32_dpmi_simulate_int (0x2f, &dpmiregs);
  /* We only support Windows-specific features when we run on Windows 9X
     or on Windows 3.X/enhanced mode.

     Int 2Fh/AX=1600h returns:

     AL = 00:  no Windows at all;
     AL = 01:  Windows/386 2.x;
     AL = 80h: Windows 3.x in mode other than enhanced;
     AL = FFh: Windows/386 2.x

     We also check AH > 0 (Windows 3.1 or later), in case AL tricks us.  */
  if (dpmiregs.h.al > 2 && dpmiregs.h.al != 0x80 && dpmiregs.h.al != 0xff
      && (dpmiregs.h.al > 3 || dpmiregs.h.ah > 0))
    {
      dos_windows_version = dpmiregs.x.ax;
      Vdos_windows_version =
	Fcons (make_number (dpmiregs.h.al), make_number (dpmiregs.h.ah));

      /* Save the current title of this virtual machine, so we can restore
	 it before exiting.  Otherwise, Windows 95 will continue to use
	 the title we set even after we are history, stupido...  */
      if (dpmiregs.h.al >= 4)
	{
	  dpmiregs.x.ax = 0x168e;
	  dpmiregs.x.dx = 3;	/* get VM title */
	  dpmiregs.x.cx = sizeof (parent_vm_title) - 1;
	  dpmiregs.x.es = __tb >> 4;
	  dpmiregs.x.di = __tb & 15;
	  dpmiregs.x.sp = dpmiregs.x.ss = dpmiregs.x.flags = 0;
	  _go32_dpmi_simulate_int (0x2f, &dpmiregs);
	  if (dpmiregs.x.ax == 1)
	    dosmemget (__tb, sizeof (parent_vm_title), parent_vm_title);
	}
    }
  else
    {
      dos_windows_version = 0;
      Vdos_windows_version = Qnil;
    }
#endif /* !HAVE_X_WINDOWS */

  /* Without this, we never see hidden files.
     Don't OR it with the previous value, so the value recorded at dump
     time, possibly with `preserve-case' flags set, won't get through.  */
  __opendir_flags = __OPENDIR_FIND_HIDDEN;

#if __DJGPP_MINOR__ == 0
  /* Under LFN, preserve the case of files as recorded in the directory
     (in DJGPP 2.01 and later this is automagically done by the library).  */
  if (!NILP (Fmsdos_long_file_names ()))
    __opendir_flags |= __OPENDIR_PRESERVE_CASE;
#endif /* __DJGPP_MINOR__ == 0 */
}

#ifndef HAVE_X_WINDOWS

/* Emulation of some X window features from xfns.c and xfaces.c.  */

/* Standard VGA colors, in the order of their standard numbering
   in the default VGA palette.  */
static char *vga_colors[16] = {
  "black", "blue", "green", "cyan", "red", "magenta", "brown",
  "lightgray", "darkgray", "lightblue", "lightgreen", "lightcyan",
  "lightred", "lightmagenta", "yellow", "white"
};

/* Given a color name, return its index, or -1 if not found.  Note
   that this only performs case-insensitive comparison against the
   standard names.  For anything more sophisticated, like matching
   "gray" with "grey" or translating X color names into their MSDOS
   equivalents, call the Lisp function Qtty_color_desc (defined
   on lisp/term/tty-colors.el).  */
int
msdos_stdcolor_idx (const char *name)
{
  int i;

  for (i = 0; i < sizeof (vga_colors) / sizeof (vga_colors[0]); i++)
    if (xstrcasecmp (name, vga_colors[i]) == 0)
      return i;

  return
    strcmp (name, unspecified_fg) == 0 ? FACE_TTY_DEFAULT_FG_COLOR
    : strcmp (name, unspecified_bg) == 0 ? FACE_TTY_DEFAULT_BG_COLOR
    : FACE_TTY_DEFAULT_COLOR;
}

/* Given a color index, return its standard name.  */
Lisp_Object
msdos_stdcolor_name (int idx)
{
  extern Lisp_Object Qunspecified;

  if (idx == FACE_TTY_DEFAULT_FG_COLOR)
    return build_string (unspecified_fg);
  else if (idx == FACE_TTY_DEFAULT_BG_COLOR)
    return build_string (unspecified_bg);
  else if (idx >= 0 && idx < sizeof (vga_colors) / sizeof (vga_colors[0]))
    return build_string (vga_colors[idx]);
  else
    return Qunspecified;	/* meaning the default */
}

/* Support for features that are available when we run in a DOS box
   on MS-Windows.  */
int
ms_windows_version (void)
{
  return dos_windows_version;
}

/* Set the title of the current virtual machine, to appear on
   the caption bar of that machine's window.  */

int
w95_set_virtual_machine_title (const char *title_string)
{
  /* Only Windows 9X (version 4 and higher) support this function.  */
  if (!NILP (Vdos_windows_version)
      && (dos_windows_version & 0xff) >= 4)
    {
      _go32_dpmi_registers regs;
      dosmemput (title_string, strlen (title_string) + 1, __tb);
      regs.x.ax = 0x168e;
      regs.x.dx = 1;
      regs.x.es = __tb >> 4;
      regs.x.di = __tb & 15;
      regs.x.sp = regs.x.ss = regs.x.flags = 0;
      _go32_dpmi_simulate_int (0x2f, &regs);
      return regs.x.ax == 1;
    }
  return 0;
}

/* Change the title of frame F to NAME.
   If NAME is nil, use the frame name as the title.

   If Emacs is not run from a DOS box on Windows 9X, this only
   sets the name in the frame struct, but has no other effects.  */

void
x_set_title (struct frame *f, Lisp_Object name)
{
  /* Don't change the title if it's already NAME.  */
  if (EQ (name, f->title))
    return;

  update_mode_lines = 1;

  f->title = name;

  if (NILP (name))
    name = f->name;

  if (FRAME_MSDOS_P (f))
    {
      BLOCK_INPUT;
      w95_set_virtual_machine_title (SDATA (name));
      UNBLOCK_INPUT;
    }
}
#endif /* !HAVE_X_WINDOWS */

DEFUN ("file-system-info", Ffile_system_info, Sfile_system_info, 1, 1, 0,
       doc: /* Return storage information about the file system FILENAME is on.
Value is a list of floats (TOTAL FREE AVAIL), where TOTAL is the total
storage of the file system, FREE is the free storage, and AVAIL is the
storage available to a non-superuser.  All 3 numbers are in bytes.
If the underlying system call fails, value is nil.  */)
  (Lisp_Object filename)
{
  struct statfs stfs;
  Lisp_Object encoded, value;

  CHECK_STRING (filename);
  filename = Fexpand_file_name (filename, Qnil);
  encoded = ENCODE_FILE (filename);

  if (statfs (SDATA (encoded), &stfs))
    value = Qnil;
  else
    value = list3 (make_float ((double) stfs.f_bsize * stfs.f_blocks),
		   make_float ((double) stfs.f_bsize * stfs.f_bfree),
		   make_float ((double) stfs.f_bsize * stfs.f_bavail));

  return value;
}

/* System depended enumeration of and access to system processes a-la
   ps(1).  Here, we only return info about the running Emacs process.
   (There are no other processes on DOS, right?)  */

Lisp_Object
list_system_processes (void)
{
  Lisp_Object proclist = Qnil;

  proclist = Fcons (make_fixnum_or_float (getpid ()), proclist);

  return proclist;
}

Lisp_Object
system_process_attributes (Lisp_Object pid)
{
  int proc_id;
  Lisp_Object attrs = Qnil;

  CHECK_NUMBER_OR_FLOAT (pid);
  proc_id = FLOATP (pid) ? XFLOAT_DATA (pid) : XINT (pid);

  if (proc_id == getpid ())
    {
      EMACS_INT uid, gid;
      char *usr;
      struct group *gr;
      char cmd[FILENAME_MAX];
      char *cmdline = NULL, *p, *q;
      size_t cmdline_size = 0;
      int i;
      Lisp_Object cmd_str, decoded_cmd, tem;
      double pmem;
      EXFUN (Fget_internal_run_time, 0);
#ifndef SYSTEM_MALLOC
      extern unsigned long ret_lim_data ();
#endif

      uid = getuid ();
      attrs = Fcons (Fcons (Qeuid, make_fixnum_or_float (uid)), attrs);
      usr = getlogin ();
      if (usr)
	attrs = Fcons (Fcons (Quser, build_string (usr)), attrs);
      gid = getgid ();
      attrs = Fcons (Fcons (Qegid, make_fixnum_or_float (gid)), attrs);
      gr = getgrgid (gid);
      if (gr)
	attrs = Fcons (Fcons (Qgroup, build_string (gr->gr_name)), attrs);
      strcpy (cmd, basename (__crt0_argv[0]));
      /* Command name is encoded in locale-coding-system; decode it.  */
      cmd_str = make_unibyte_string (cmd, strlen (cmd));
      decoded_cmd = code_convert_string_norecord (cmd_str,
						  Vlocale_coding_system, 0);
      attrs = Fcons (Fcons (Qcomm, decoded_cmd), attrs);
      /* Pretend we have 0 as PPID.  */
      attrs = Fcons (Fcons (Qppid, make_number (0)), attrs);
      attrs = Fcons (Fcons (Qpgrp, pid), attrs);
      attrs = Fcons (Fcons (Qttname, build_string ("/dev/tty")), attrs);
      /* We are never idle!  */
      tem = Fget_internal_run_time ();
      attrs = Fcons (Fcons (Qtime, tem), attrs);
      attrs = Fcons (Fcons (Qthcount, make_number (1)), attrs);
      attrs = Fcons (Fcons (Qstart,
			    Fsymbol_value (intern ("before-init-time"))),
		     attrs);
      attrs = Fcons (Fcons (Qvsize,
			    make_fixnum_or_float ((unsigned long)sbrk (0)/1024)),
		     attrs);
      attrs = Fcons (Fcons (Qetime, tem), attrs);
#ifndef SYSTEM_MALLOC
      /* ret_lim_data is on vm-limit.c, which is not compiled in under
	 SYSTEM_MALLOC.  */
      pmem = (double)((unsigned long) sbrk (0)) / ret_lim_data () * 100.0;
      if (pmem > 100)
#endif
	pmem = 100;
      attrs = Fcons (Fcons (Qpmem, make_float (pmem)), attrs);
      /* Pass 1: Count how much storage we need.  */
      for (i = 0; i < __crt0_argc; i++)
	{
	  cmdline_size += strlen (__crt0_argv[i]) + 1; /* +1 for blank delim */
	  if (strpbrk (__crt0_argv[i], " \t\n\r\v\f"))
	    {
	      cmdline_size += 2;
	      for (p = __crt0_argv[i]; *p; p++)
		{
		  if (*p == '"')
		    cmdline_size++;
		}
	    }
	}
      /* Pass 2: Allocate storage and concatenate argv[].  */
      cmdline = xmalloc (cmdline_size + 1);
      for (i = 0, q = cmdline; i < __crt0_argc; i++)
	{
	  if (strpbrk (__crt0_argv[i], " \t\n\r\v\f"))
	    {
	      *q++ = '"';
	      for (p = __crt0_argv[i]; *p; p++)
		{
		  if (*p == '\"')
		    *q++ = '\\';
		  *q++ = *p;
		}
	      *q++ = '"';
	    }
	  else
	    {
	      strcpy (q, __crt0_argv[i]);
	      q += strlen (__crt0_argv[i]);
	    }
	  *q++ = ' ';
	}
      /* Remove the trailing blank.  */
      if (q > cmdline)
	q[-1] = '\0';

      /* Command line is encoded in locale-coding-system; decode it.  */
      cmd_str = make_unibyte_string (cmdline, strlen (cmdline));
      decoded_cmd = code_convert_string_norecord (cmd_str,
						  Vlocale_coding_system, 0);
      xfree (cmdline);
      attrs = Fcons (Fcons (Qargs, decoded_cmd), attrs);
    }

  return attrs;
}

void
dos_cleanup (void)
{
  struct tty_display_info *tty;

#ifndef HAVE_X_WINDOWS
  restore_parent_vm_title ();
#endif
  /* Make sure the termscript file is committed, in case we are
     crashing and some vital info was written there.  */
  if (FRAMEP (selected_frame))
    {
      struct frame *sf = XFRAME (selected_frame);

      if (FRAME_LIVE_P (sf)
	  && (FRAME_MSDOS_P (sf) || FRAME_TERMCAP_P (sf)))
	{
	  tty = CURTTY ();
	  if (tty->termscript)
	    {
	      fflush (tty->termscript);
	      fsync (fileno (tty->termscript));
	    }
	}
    }
}

/*
 *	Define everything
 */
void
syms_of_dosfns (void)
{
  defsubr (&Sint86);
  defsubr (&Sdos_memget);
  defsubr (&Sdos_memput);
  defsubr (&Smsdos_mouse_init);
  defsubr (&Smsdos_mouse_enable);
  defsubr (&Smsdos_set_keyboard);
  defsubr (&Sinsert_startup_screen);
  defsubr (&Smsdos_mouse_disable);
  defsubr (&Sfile_system_info);
#ifndef HAVE_X_WINDOWS
  defsubr (&Smsdos_mouse_p);
#endif

  DEFVAR_INT ("dos-country-code", dos_country_code,
	      doc: /* The country code returned by Dos when Emacs was started.
Usually this is the international telephone prefix.  */);

  DEFVAR_INT ("dos-codepage", dos_codepage,
	      doc: /* The codepage active when Emacs was started.
The following are known:
	437	United States
	850	Multilingual (Latin I)
	852	Slavic (Latin II)
	857	Turkish
	860	Portugal
	861	Iceland
	863	Canada (French)
	865	Norway/Denmark  */);

  DEFVAR_INT ("dos-timezone-offset", dos_timezone_offset,
	      doc: /* The current timezone offset to UTC in minutes.
Implicitly modified when the TZ variable is changed.  */);

  DEFVAR_LISP ("dos-version", Vdos_version,
	       doc: /* The (MAJOR . MINOR) Dos version (subject to modification with setver).  */);

#ifndef HAVE_X_WINDOWS
  DEFVAR_LISP ("dos-windows-version", Vdos_windows_version,
	       doc: /* The (MAJOR . MINOR) Windows version for DOS session on MS-Windows.  */);
#endif

  DEFVAR_LISP ("dos-display-scancodes", Vdos_display_scancodes,
	       doc: /* *Controls whether DOS raw keyboard events are displayed as you type.
When non-nil, the keyboard scan-codes are displayed at the bottom right
corner of the display (typically at the end of the mode line).
The output format is: scan code:char code*modifiers.  */);

  Vdos_display_scancodes = Qnil;

  DEFVAR_INT ("dos-hyper-key", dos_hyper_key,
	      doc: /* *If set to 1, use right ALT key as hyper key.
If set to 2, use right CTRL key as hyper key.  */);
  dos_hyper_key = 0;

  DEFVAR_INT ("dos-super-key", dos_super_key,
	      doc: /* *If set to 1, use right ALT key as super key.
If set to 2, use right CTRL key as super key.  */);
  dos_super_key = 0;

  DEFVAR_INT ("dos-keypad-mode", dos_keypad_mode,
	      doc: /* *Controls what key code is returned by a key in the numeric keypad.
The `numlock ON' action is only taken if no modifier keys are pressed.
The value is an integer constructed by adding the following bits together:

  0x00	Digit key returns digit    (if numlock ON)
  0x01	Digit key returns kp-digit (if numlock ON)
  0x02	Digit key returns M-digit  (if numlock ON)
  0x03	Digit key returns edit key (if numlock ON)

  0x00	Grey key returns char      (if numlock ON)
  0x04	Grey key returns kp-key    (if numlock ON)

  0x00	Digit key returns digit    (if numlock OFF)
  0x10	Digit key returns kp-digit (if numlock OFF)
  0x20	Digit key returns M-digit  (if numlock OFF)
  0x30	Digit key returns edit key (if numlock OFF)

  0x00	Grey key returns char      (if numlock OFF)
  0x40	Grey key returns kp-key    (if numlock OFF)

  0x200	ALT-0..ALT-9 in top-row produces shifted codes.  */);
  dos_keypad_mode = 0x75;

  DEFVAR_INT ("dos-keyboard-layout", dos_keyboard_layout,
	      doc: /* Contains the country code for the current keyboard layout.
Use msdos-set-keyboard to select another keyboard layout.  */);
  dos_keyboard_layout = 1;	/* US */

  DEFVAR_INT ("dos-decimal-point", dos_decimal_point,
	      doc: /* The character to produce when kp-decimal key is pressed.
If non-zero, this variable contains the character to be returned when the
decimal point key in the numeric keypad is pressed when Num Lock is on.
If zero, the decimal point key returns the country code specific value.  */);
  dos_decimal_point = 0;
}
#endif /* MSDOS */

