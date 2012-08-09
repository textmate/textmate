/* 16-bit Windows Selection processing for emacs on MS-Windows

Copyright (C) 1996-1997, 2001-2012  Free Software Foundation, Inc.

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

/* These functions work by using WinOldAp interface.  WinOldAp
   (WINOLDAP.MOD) is a Microsoft Windows extension supporting
   "old" (character-mode) application access to Dynamic Data Exchange,
   menus, and the Windows clipboard.  */

/* Written by Dale P. Smith <dpsm@en.com>  */
/* Adapted to DJGPP by Eli Zaretskii <eliz@gnu.org>  */

#ifdef MSDOS

#include <config.h>
#include <dpmi.h>
#include <go32.h>
#include <sys/farptr.h>
#include <setjmp.h>
#include "lisp.h"
#include "dispextern.h"	/* frame.h seems to want this */
#include "frame.h"	/* Need this to get the X window of selected_frame */
#include "blockinput.h"
#include "buffer.h"
#include "character.h"
#include "coding.h"
#include "composite.h"

/* If ever some function outside this file will need to call any
   clipboard-related function, the following prototypes and constants
   should be put on a header file.  Right now, nobody else uses them.  */

#define CF_TEXT      0x01
#define CF_BITMAP    0x02
#define CF_METAFILE  0x03
#define CF_SYLK	     0x04
#define CF_DIF	     0x05
#define CF_TIFF	     0x06
#define CF_OEMTEXT   0x07
#define CF_DIBBITMAP 0x08
#define CF_WINWRITE  0x80
#define CF_DSPTEXT   0x81
#define CF_DSPBITMAP 0x82

unsigned identify_winoldap_version (void);
unsigned open_clipboard (void);
unsigned empty_clipboard (void);
unsigned set_clipboard_data (unsigned, void *, unsigned, int);
unsigned get_clipboard_data_size (unsigned);
unsigned get_clipboard_data (unsigned, void *, unsigned, int);
unsigned close_clipboard (void);
unsigned clipboard_compact (unsigned);

Lisp_Object QCLIPBOARD, QPRIMARY;

/* The segment address and the size of the buffer in low
   memory used to move data between us and WinOldAp module.  */
static struct {
  unsigned long size;
  unsigned short rm_segment;
} clipboard_xfer_buf_info;

/* The last text we put into the clipboard.  This is used to prevent
   passing back our own text from the clipboard, instead of using the
   kill ring.  The former is undesirable because the clipboard data
   could be MULEtilated by inappropriately chosen
   (next-)selection-coding-system.  For this reason, we must store the
   text *after* it was encoded/Unix-to-DOS-converted.  */
static unsigned char *last_clipboard_text;

/* The size of allocated storage for storing the clipboard data.  */
static size_t clipboard_storage_size;

/* C functions to access the Windows 3.1x clipboard from DOS apps.

   The information was obtained from the Microsoft Knowledge Base,
   article Q67675 and can be found at:
   http://www.microsoft.com/kb/developr/win_dk/q67675.htm  */

/* See also Ralf Brown's Interrupt List.

   I also seem to remember reading about this in Dr. Dobbs Journal a
   while ago, but if you knew my memory...  :-)

   Dale P. Smith <dpsm@en.com> */

/* Return the WinOldAp support version, or 0x1700 if not supported.  */
unsigned
identify_winoldap_version (void)
{
  __dpmi_regs regs;

  /* Calls Int 2Fh/AX=1700h
     Return Values   AX == 1700H: Clipboard functions not available
                        <> 1700H: AL = Major version number
				  AH = Minor version number */
  regs.x.ax = 0x1700;
  __dpmi_int (0x2f, &regs);
  return regs.x.ax;
}

/* Open the clipboard, return non-zero if successful.  */
unsigned
open_clipboard (void)
{
  __dpmi_regs regs;

  /* Is WINOLDAP supported?  */
  /* Kludge alert!!  If WinOldAp is not supported, we return a 0,
     which is the same as ``Clipboard already open''.  Currently,
     this is taken as an error by all the functions that use
     `open_clipboard', but if somebody someday will use that ``open''
     clipboard, they will have interesting time debugging it...  */
  if (identify_winoldap_version () == 0x1700)
    return 0;

  /* Calls Int 2Fh/AX=1701h
     Return Values   AX == 0: Clipboard already open
			<> 0: Clipboard opened */
  regs.x.ax = 0x1701;
  __dpmi_int (0x2f, &regs);
  return regs.x.ax;
}

/* Empty clipboard, return non-zero if successful.  */
unsigned
empty_clipboard (void)
{
  __dpmi_regs regs;

  /* Calls Int 2Fh/AX=1702h
     Return Values   AX == 0: Error occurred
			<> 0: OK, Clipboard emptied */
  regs.x.ax = 0x1702;
  __dpmi_int (0x2f, &regs);
  return regs.x.ax;
}

/* Ensure we have a buffer in low memory with enough memory for data
   of size WANT_SIZE.  Return the linear address of the buffer.  */
static unsigned long
alloc_xfer_buf (unsigned want_size)
{
  __dpmi_regs regs;

  /* If the usual DJGPP transfer buffer is large enough, use that.  */
  if (want_size <= _go32_info_block.size_of_transfer_buffer)
    return __tb & 0xfffff;

  /* Don't even try to allocate more than 1MB of memory: DOS cannot
     possibly handle that (it will overflow the BX register below).  */
  if (want_size > 0xfffff)
    return 0;

  /* Need size rounded up to the nearest paragraph, and in
     paragraph units (1 paragraph = 16 bytes).  */
  clipboard_xfer_buf_info.size = (want_size + 15) >> 4;

  /* The NT DPMI host crashes us if we free DOS memory via the
     DPMI service.  Work around by calling DOS allocate/free block.  */
  regs.h.ah = 0x48;
  regs.x.bx = clipboard_xfer_buf_info.size;
  __dpmi_int (0x21, &regs);
  if (regs.x.flags & 1)
    {
      clipboard_xfer_buf_info.size = 0;
      return 0;
    }

  clipboard_xfer_buf_info.rm_segment = regs.x.ax;
  return (((int)clipboard_xfer_buf_info.rm_segment) << 4) & 0xfffff;
}

/* Free our clipboard buffer.  We always free it after use, because
   keeping it leaves less free conventional memory for subprocesses.
   The clipboard buffer tends to be large in size, because for small
   clipboard data sizes we use the DJGPP transfer buffer.  */
static void
free_xfer_buf (void)
{
  /* If the size is 0, we used DJGPP transfer buffer, so don't free.  */
  if (clipboard_xfer_buf_info.size)
    {
      __dpmi_regs regs;

      /* The NT DPMI host crashes us if we free DOS memory via
	 the DPMI service.  Work around by calling DOS free block.  */
      regs.h.ah = 0x49;
      regs.x.es = clipboard_xfer_buf_info.rm_segment;
      __dpmi_int (0x21, &regs);
      clipboard_xfer_buf_info.size = 0;
    }
}

/* Copy data into the clipboard, return zero if successful.  */
unsigned
set_clipboard_data (unsigned Format, void *Data, unsigned Size, int Raw)
{
  __dpmi_regs regs;
  unsigned truelen;
  unsigned long xbuf_addr, buf_offset;
  unsigned char *dp = Data, *dstart = dp;

  if (Format != CF_OEMTEXT)
    return 3;

  /* need to know final size after '\r' chars are inserted (the
     standard CF_OEMTEXT clipboard format uses CRLF line endings,
     while Emacs uses just LF internally).  */
  truelen = Size + 1;		/* +1 for the terminating null */

  if (!Raw)
    {
      /* avoid using strchr because it recomputes the length everytime */
      while ((dp = memchr (dp, '\n', Size - (dp - dstart))) != 0)
	{
	  truelen++;
	  dp++;
	}
    }

  if (clipboard_compact (truelen) < truelen)
    return 1;

  if ((xbuf_addr = alloc_xfer_buf (truelen)) == 0)
    return 1;

  /* Move the buffer into the low memory, convert LF into CR-LF if needed.  */
  if (Raw)
    {
      dosmemput (Data, Size, xbuf_addr);

      /* Terminate with a null, otherwise Windows does strange things
	 when the text size is an integral multiple of 32 bytes. */
      _farpokeb (_dos_ds, xbuf_addr + Size, '\0');
    }
  else
    {
      dp = Data;
      buf_offset = xbuf_addr;
      _farsetsel (_dos_ds);
      while (Size--)
	{
	  /* Don't allow them to put binary data into the clipboard, since
	     it will cause yanked data to be truncated at the first null.  */
	  if (*dp == '\0')
	    return 2;
	  if (*dp == '\n')
	    _farnspokeb (buf_offset++, '\r');
	  _farnspokeb (buf_offset++, *dp++);
	}

      /* Terminate with a null, otherwise Windows does strange things
	 when the text size is an integral multiple of 32 bytes. */
      _farnspokeb (buf_offset, '\0');
    }

  /* Stash away the data we are about to put into the clipboard, so we
     could later check inside get_clipboard_data whether the clipboard
     still holds our data.  */
  if (clipboard_storage_size < truelen)
    {
      clipboard_storage_size = truelen + 100;
      last_clipboard_text =
	(char *) xrealloc (last_clipboard_text, clipboard_storage_size);
    }
  if (last_clipboard_text)
    dosmemget (xbuf_addr, truelen, last_clipboard_text);

  /* Calls Int 2Fh/AX=1703h with:
	             DX = WinOldAp-Supported Clipboard format
                     ES:BX = Pointer to data
                     SI:CX = Size of data in bytes
     Return Values   AX == 0: Error occurred
			<> 0: OK.  Data copied into the Clipboard.  */
  regs.x.ax = 0x1703;
  regs.x.dx = Format;
  regs.x.si = truelen >> 16;
  regs.x.cx = truelen & 0xffff;
  regs.x.es = xbuf_addr >> 4;
  regs.x.bx = xbuf_addr & 15;
  __dpmi_int (0x2f, &regs);

  free_xfer_buf ();

  /* If the above failed, invalidate the local copy of the clipboard.  */
  if (regs.x.ax == 0)
    *last_clipboard_text = '\0';

  /* Zero means success, otherwise (1, 2, or 3) it's an error.  */
  return regs.x.ax > 0 ? 0 : 3;
}

/* Return the size of the clipboard data of format FORMAT.  */
unsigned
get_clipboard_data_size (unsigned Format)
{
  __dpmi_regs regs;

  /* Calls Int 2Fh/AX=1704h with:
		     DX = WinOldAp-Supported Clipboard format
     Return Values   DX:AX == Size of the data in bytes, including any
                              headers.
                           == 0 If data in this format is not in
			   the clipboard.  */
  regs.x.ax = 0x1704;
  regs.x.dx = Format;
  __dpmi_int (0x2f, &regs);
  return ( (((unsigned)regs.x.dx) << 16) | regs.x.ax);
}

/* Get clipboard data, return its length.
   Warning: this doesn't check whether DATA has enough space to hold
   SIZE bytes.  */
unsigned
get_clipboard_data (unsigned Format, void *Data, unsigned Size, int Raw)
{
  __dpmi_regs regs;
  unsigned long xbuf_addr;
  unsigned char *dp = Data;

  if (Format != CF_OEMTEXT)
    return 0;

  if (Size == 0)
    return 0;

  if ((xbuf_addr = alloc_xfer_buf (Size)) == 0)
    return 0;

  /* Calls Int 2Fh/AX=1705h with:
		     DX = WinOldAp-Supported Clipboard format
		     ES:BX = Pointer to data buffer to hold data
     Return Values   AX == 0: Error occurred (or data in this format is not
                              in the clipboard)
                        <> 0: OK  */
  regs.x.ax = 0x1705;
  regs.x.dx = Format;
  regs.x.es = xbuf_addr >> 4;
  regs.x.bx = xbuf_addr & 15;
  __dpmi_int (0x2f, &regs);
  if (regs.x.ax != 0)
    {
      unsigned char null_char = '\0';
      unsigned long xbuf_beg = xbuf_addr;

      /* If last_clipboard_text is NULL, we don't want to slow down
	 the next loop by an additional test.  */
      register unsigned char *lcdp =
	last_clipboard_text == NULL ? &null_char : last_clipboard_text;

      /* Copy data from low memory, remove CR
	 characters before LF if needed.  */
      _farsetsel (_dos_ds);
      while (Size--)
	{
	  register unsigned char c = _farnspeekb (xbuf_addr++);

	  if (*lcdp == c)
	    lcdp++;

	  if ((*dp++ = c) == '\r' && !Raw && _farnspeekb (xbuf_addr) == '\n')
	    {
	      dp--;
	      *dp++ = '\n';
	      xbuf_addr++;
	      if (*lcdp == '\n')
		lcdp++;
	    }
	  /* Windows reportedly rounds up the size of clipboard data
	     (passed in SIZE) to a multiple of 32, and removes trailing
	     spaces from each line without updating SIZE.  We therefore
	     bail out when we see the first null character.  */
	  else if (c == '\0')
	    break;
	}

      /* If the text in clipboard is identical to what we put there
	 last time set_clipboard_data was called, pretend there's no
	 data in the clipboard.  This is so we don't pass our own text
	 from the clipboard (which might be troublesome if the killed
	 text includes null characters).  */
      if (last_clipboard_text &&
	  xbuf_addr - xbuf_beg == (long)(lcdp - last_clipboard_text))
	dp = (unsigned char *)Data + 1;
    }

  free_xfer_buf ();

  return (unsigned) (dp - (unsigned char *)Data - 1);
}

/* Close clipboard, return non-zero if successful.  */
unsigned
close_clipboard (void)
{
  __dpmi_regs regs;

  /* Calls Int 2Fh/AX=1708h
     Return Values   AX == 0: Error occurred
                        <> 0: OK */
  regs.x.ax = 0x1708;
  __dpmi_int (0x2f, &regs);
  return regs.x.ax;
}

/* Compact clipboard data so that at least SIZE bytes is available.  */
unsigned
clipboard_compact (unsigned Size)
{
  __dpmi_regs regs;

  /* Calls Int 2Fh/AX=1709H with:
                     SI:CX = Desired memory size in bytes.
     Return Values   DX:AX == Number of bytes of largest block of free memory.
                           == 0 if error or no memory  */
  regs.x.ax = 0x1709;
  regs.x.si = Size >> 16;
  regs.x.cx = Size & 0xffff;
  __dpmi_int (0x2f, &regs);
  return ((unsigned)regs.x.dx << 16) | regs.x.ax;
}

static char no_mem_msg[] =
  "(Not enough DOS memory to put saved text into clipboard.)";
static char binary_msg[] =
  "(Binary characters in saved text; clipboard data not set.)";
static char system_error_msg[] =
  "(Clipboard interface failure; clipboard data not set.)";

DEFUN ("w16-set-clipboard-data", Fw16_set_clipboard_data, Sw16_set_clipboard_data, 1, 2, 0,
       doc: /* This sets the clipboard data to the given text.  */)
  (Lisp_Object string, Lisp_Object frame)
{
  unsigned ok = 1, put_status = 0;
  int nbytes, no_crlf_conversion;
  unsigned char *src, *dst = NULL;

  CHECK_STRING (string);

  if (NILP (frame))
    frame = Fselected_frame ();

  CHECK_LIVE_FRAME (frame);
  if ( !FRAME_MSDOS_P (XFRAME (frame)))
    goto done;

  BLOCK_INPUT;

  if (!open_clipboard ())
    goto error;

  nbytes = SBYTES (string);
  src = SDATA (string);

  /* Do we need to encode this text?  */
  for (dst = src; dst < src + nbytes; dst++)
    {
      if (*dst == '\0' || *dst >= 0x80)
	break;
    }
  if (dst >= src + nbytes)
    {
      /* No multibyte characters in text.  We need not encode it, but we
	 will have to convert it to DOS CR-LF style.  */
      no_crlf_conversion = 0;
      Vlast_coding_system_used = Qraw_text;
      dst = NULL;	/* so we don't try to free a random pointer */
    }
  else
    {
      /* We must encode contents of STRING according to what
	 clipboard-coding-system specifies.  */
      struct coding_system coding;
      Lisp_Object coding_system =
	NILP (Vnext_selection_coding_system) ?
	Vselection_coding_system : Vnext_selection_coding_system;

      setup_coding_system (Fcheck_coding_system (coding_system), &coding);
      coding.dst_bytes = nbytes * 4;
      coding.destination = (unsigned char *) xmalloc (coding.dst_bytes);
      Vnext_selection_coding_system = Qnil;
      coding.mode |= CODING_MODE_LAST_BLOCK;
      dst = coding.destination;
      encode_coding_object (&coding, string, 0, 0,
			    SCHARS (string), nbytes, Qnil);
      no_crlf_conversion = 1;
      nbytes = coding.produced;
      Vlast_coding_system_used = CODING_ID_NAME (coding.id);
      src = dst;
    }

  ok = empty_clipboard ()
    && ((put_status
	 = set_clipboard_data (CF_OEMTEXT, src, nbytes, no_crlf_conversion))
	== 0);

  if (!no_crlf_conversion)
  close_clipboard ();

  if (ok) goto unblock;

 error:

  ok = 0;

 unblock:
  xfree (dst);
  UNBLOCK_INPUT;

  /* Notify user if the text is too large to fit into DOS memory.
     (This will happen somewhere after 600K bytes (470K in DJGPP v1.x),
     depending on user system configuration.)  If we just silently
     fail the function, people might wonder why their text sometimes
     doesn't make it to the clipboard.  */
  if (put_status)
    {
      switch (put_status)
	{
	  case 1:
	    message2 (no_mem_msg, sizeof (no_mem_msg) - 1, 0);
	    break;
	  case 2:
	    message2 (binary_msg, sizeof (binary_msg) - 1, 0);
	    break;
	  case 3:
	    message2 (system_error_msg, sizeof (system_error_msg) - 1, 0);
	    break;
	}
      sit_for (make_number (2), 0, 2);
    }

 done:

  return (ok && put_status == 0 ? string : Qnil);
}

DEFUN ("w16-get-clipboard-data", Fw16_get_clipboard_data, Sw16_get_clipboard_data, 0, 1, 0,
       doc: /* This gets the clipboard data in text format.  */)
  (Lisp_Object frame)
{
  unsigned data_size, truelen;
  unsigned char *htext = NULL;
  Lisp_Object ret = Qnil;
  int require_decoding = 0;

  if (NILP (frame))
    frame = Fselected_frame ();

  CHECK_LIVE_FRAME (frame);
  if ( !FRAME_MSDOS_P (XFRAME (frame)))
    goto done;

  BLOCK_INPUT;

  if (!open_clipboard ())
    goto unblock;

  if ((data_size = get_clipboard_data_size (CF_OEMTEXT)) == 0 ||
      (htext = (unsigned char *)xmalloc (data_size)) == 0)
    goto closeclip;

  /* need to know final size after '\r' chars are removed because
     we can't change the string size manually, and doing an extra
     copy is silly */
  if ((truelen = get_clipboard_data (CF_OEMTEXT, htext, data_size, 0)) == 0)
    goto closeclip;

  /* Do we need to decode it?  */
  {
    /* If the clipboard data contains any 8-bit Latin-1 code, we
       need to decode it.  */
    int i;

    for (i = 0; i < truelen; i++)
      {
	if (htext[i] >= 0x80)
	  {
	    require_decoding = 1;
	    break;
	  }
      }
  }
  if (require_decoding)
    {
      struct coding_system coding;
      Lisp_Object coding_system = Vnext_selection_coding_system;

      truelen = get_clipboard_data (CF_OEMTEXT, htext, data_size, 1);
      if (NILP (coding_system))
	coding_system = Vselection_coding_system;
      setup_coding_system (Fcheck_coding_system (coding_system), &coding);
      coding.source = htext;
      coding.mode |= CODING_MODE_LAST_BLOCK;
      /* We explicitly disable composition handling because selection
	 data should not contain any composition sequence.  */
      coding.mode &= CODING_ANNOTATION_MASK;
      decode_coding_object (&coding, Qnil, 0, 0, truelen, truelen, Qt);
      ret = coding.dst_object;
      Vlast_coding_system_used = CODING_ID_NAME (coding.id);
    }
  else
    {
      ret = make_unibyte_string ((char *) htext, truelen);
      Vlast_coding_system_used = Qraw_text;
    }

  xfree (htext);
  Vnext_selection_coding_system = Qnil;

 closeclip:
  close_clipboard ();

 unblock:
  UNBLOCK_INPUT;

 done:

  return (ret);
}

/* Support checking for a clipboard selection. */

DEFUN ("x-selection-exists-p", Fx_selection_exists_p, Sx_selection_exists_p,
       0, 2, 0,
       doc: /* Whether there is an owner for the given X selection.
SELECTION should be the name of the selection in question, typically
one of the symbols `PRIMARY', `SECONDARY', or `CLIPBOARD'.  (X expects
these literal upper-case names.)  The symbol nil is the same as
`PRIMARY', and t is the same as `SECONDARY'.

TERMINAL should be a terminal object or a frame specifying the X
server to query.  If omitted or nil, that stands for the selected
frame's display, or the first available X display.  */)
  (Lisp_Object selection, Lisp_Object terminal)
{
  CHECK_SYMBOL (selection);

  /* Return nil for SECONDARY selection.  For PRIMARY (or nil)
     selection, check if there is some text on the kill-ring;
     for CLIPBOARD, check if the clipboard currently has valid
     text format contents.

     The test for killed text on the kill-ring emulates the Emacs
     behavior on X, where killed text is also put into X selection
     by the X interface code.  (On MSDOS, killed text is only put
     into the clipboard if we run under Windows, so we cannot check
     the clipboard alone.)  */
  if ((EQ (selection, Qnil) || EQ (selection, QPRIMARY))
      && ! NILP (Fsymbol_value (Fintern_soft (build_string ("kill-ring"),
					      Qnil))))
    return Qt;

  if (EQ (selection, QCLIPBOARD))
    {
      Lisp_Object val = Qnil;

      if (open_clipboard ())
	{
	  if (get_clipboard_data_size (CF_OEMTEXT))
	    val = Qt;
	  close_clipboard ();
	}
      return val;
    }
  return Qnil;
}

void
syms_of_win16select (void)
{
  defsubr (&Sw16_set_clipboard_data);
  defsubr (&Sw16_get_clipboard_data);
  defsubr (&Sx_selection_exists_p);

  DEFVAR_LISP ("selection-coding-system", Vselection_coding_system,
	       doc: /* Coding system for communicating with other programs.

For MS-Windows and MS-DOS:
When sending or receiving text via selection and clipboard, the text
is encoded or decoded by this coding system.  The default value is
the current system default encoding on 9x/Me, `utf-16le-dos'
\(Unicode) on NT/W2K/XP, and `iso-latin-1-dos' on MS-DOS.

For X Windows:
When sending text via selection and clipboard, if the target
data-type matches with the type of this coding system, it is used
for encoding the text.  Otherwise (including the case that this
variable is nil), a proper coding system is used as below:

data-type	coding system
---------	-------------
UTF8_STRING	utf-8
COMPOUND_TEXT	compound-text-with-extensions
STRING		iso-latin-1
C_STRING	no-conversion

When receiving text, if this coding system is non-nil, it is used
for decoding regardless of the data-type.  If this is nil, a
proper coding system is used according to the data-type as above.

See also the documentation of the variable `x-select-request-type' how
to control which data-type to request for receiving text.

The default value is nil.  */);
  Vselection_coding_system = intern ("iso-latin-1-dos");

  DEFVAR_LISP ("next-selection-coding-system", Vnext_selection_coding_system,
	       doc: /* Coding system for the next communication with other programs.
Usually, `selection-coding-system' is used for communicating with
other programs (X Windows clients or MS Windows programs).  But, if this
variable is set, it is used for the next communication only.
After the communication, this variable is set to nil.  */);
  Vnext_selection_coding_system = Qnil;

  QPRIMARY   = intern ("PRIMARY");	staticpro (&QPRIMARY);
  QCLIPBOARD = intern ("CLIPBOARD");	staticpro (&QCLIPBOARD);
}

#endif /* MSDOS */
