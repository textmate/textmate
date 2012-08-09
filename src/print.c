/* Lisp object printing and output streams.

Copyright (C) 1985-1986, 1988, 1993-1995, 1997-2012
  Free Software Foundation, Inc.

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
#include "buffer.h"
#include "character.h"
#include "charset.h"
#include "keyboard.h"
#include "frame.h"
#include "window.h"
#include "process.h"
#include "dispextern.h"
#include "termchar.h"
#include "intervals.h"
#include "blockinput.h"
#include "termhooks.h"		/* For struct terminal.  */
#include "font.h"

Lisp_Object Qstandard_output;

static Lisp_Object Qtemp_buffer_setup_hook;

/* These are used to print like we read.  */

static Lisp_Object Qfloat_output_format;

#include <math.h>
#include <float.h>
#include <ftoastr.h>

/* Default to values appropriate for IEEE floating point.  */
#ifndef DBL_DIG
#define DBL_DIG 15
#endif

/* Avoid actual stack overflow in print.  */
static int print_depth;

/* Level of nesting inside outputting backquote in new style.  */
static int new_backquote_output;

/* Detect most circularities to print finite output.  */
#define PRINT_CIRCLE 200
static Lisp_Object being_printed[PRINT_CIRCLE];

/* When printing into a buffer, first we put the text in this
   block, then insert it all at once.  */
static char *print_buffer;

/* Size allocated in print_buffer.  */
static EMACS_INT print_buffer_size;
/* Chars stored in print_buffer.  */
static EMACS_INT print_buffer_pos;
/* Bytes stored in print_buffer.  */
static EMACS_INT print_buffer_pos_byte;

Lisp_Object Qprint_escape_newlines;
static Lisp_Object Qprint_escape_multibyte, Qprint_escape_nonascii;

/* Vprint_number_table is a table, that keeps objects that are going to
   be printed, to allow use of #n= and #n# to express sharing.
   For any given object, the table can give the following values:
     t    the object will be printed only once.
     -N   the object will be printed several times and will take number N.
     N    the object has been printed so we can refer to it as #N#.
   print_number_index holds the largest N already used.
   N has to be striclty larger than 0 since we need to distinguish -N.  */
static int print_number_index;
static void print_interval (INTERVAL interval, Lisp_Object printcharfun);

/* GDB resets this to zero on W32 to disable OutputDebugString calls.  */
int print_output_debug_flag EXTERNALLY_VISIBLE = 1;


/* Low level output routines for characters and strings */

/* Lisp functions to do output using a stream
   must have the stream in a variable called printcharfun
   and must start with PRINTPREPARE, end with PRINTFINISH,
   and use PRINTDECLARE to declare common variables.
   Use PRINTCHAR to output one character,
   or call strout to output a block of characters. */

#define PRINTDECLARE							\
   struct buffer *old = current_buffer;					\
   EMACS_INT old_point = -1, start_point = -1;				\
   EMACS_INT old_point_byte = -1, start_point_byte = -1;		\
   int specpdl_count = SPECPDL_INDEX ();				\
   int free_print_buffer = 0;						\
   int multibyte = !NILP (BVAR (current_buffer, enable_multibyte_characters));	\
   Lisp_Object original

#define PRINTPREPARE							\
   original = printcharfun;						\
   if (NILP (printcharfun)) printcharfun = Qt;				\
   if (BUFFERP (printcharfun))						\
     {									\
       if (XBUFFER (printcharfun) != current_buffer)			\
	 Fset_buffer (printcharfun);					\
       printcharfun = Qnil;						\
     }									\
   if (MARKERP (printcharfun))						\
     {									\
       EMACS_INT marker_pos;						\
       if (! XMARKER (printcharfun)->buffer)				\
         error ("Marker does not point anywhere");			\
       if (XMARKER (printcharfun)->buffer != current_buffer)		\
         set_buffer_internal (XMARKER (printcharfun)->buffer);		\
       marker_pos = marker_position (printcharfun);			\
       if (marker_pos < BEGV || marker_pos > ZV)			\
	 error ("Marker is outside the accessible part of the buffer"); \
       old_point = PT;							\
       old_point_byte = PT_BYTE;					\
       SET_PT_BOTH (marker_pos,						\
		    marker_byte_position (printcharfun));		\
       start_point = PT;						\
       start_point_byte = PT_BYTE;					\
       printcharfun = Qnil;						\
     }									\
   if (NILP (printcharfun))						\
     {									\
       Lisp_Object string;						\
       if (NILP (BVAR (current_buffer, enable_multibyte_characters))		\
	   && ! print_escape_multibyte)					\
         specbind (Qprint_escape_multibyte, Qt);			\
       if (! NILP (BVAR (current_buffer, enable_multibyte_characters))		\
	   && ! print_escape_nonascii)					\
         specbind (Qprint_escape_nonascii, Qt);				\
       if (print_buffer != 0)						\
	 {								\
	   string = make_string_from_bytes (print_buffer,		\
					    print_buffer_pos,		\
					    print_buffer_pos_byte);	\
	   record_unwind_protect (print_unwind, string);		\
	 }								\
       else								\
	 {								\
	   ptrdiff_t new_size = 1000;					\
	   print_buffer = (char *) xmalloc (new_size);			\
	   print_buffer_size = new_size;				\
	   free_print_buffer = 1;					\
	 }								\
       print_buffer_pos = 0;						\
       print_buffer_pos_byte = 0;					\
     }									\
   if (EQ (printcharfun, Qt) && ! noninteractive)			\
     setup_echo_area_for_printing (multibyte);

#define PRINTFINISH							\
   if (NILP (printcharfun))						\
     {									\
       if (print_buffer_pos != print_buffer_pos_byte			\
	   && NILP (BVAR (current_buffer, enable_multibyte_characters)))	\
	 {								\
	   unsigned char *temp						\
	     = (unsigned char *) alloca (print_buffer_pos + 1);		\
	   copy_text ((unsigned char *) print_buffer, temp,		\
		      print_buffer_pos_byte, 1, 0);			\
	   insert_1_both ((char *) temp, print_buffer_pos,		\
			  print_buffer_pos, 0, 1, 0);			\
	 }								\
       else								\
	 insert_1_both (print_buffer, print_buffer_pos,			\
			print_buffer_pos_byte, 0, 1, 0);		\
       signal_after_change (PT - print_buffer_pos, 0, print_buffer_pos);\
     }									\
   if (free_print_buffer)						\
     {									\
       xfree (print_buffer);						\
       print_buffer = 0;						\
     }									\
   unbind_to (specpdl_count, Qnil);					\
   if (MARKERP (original))						\
     set_marker_both (original, Qnil, PT, PT_BYTE);			\
   if (old_point >= 0)							\
     SET_PT_BOTH (old_point + (old_point >= start_point			\
			       ? PT - start_point : 0),			\
		  old_point_byte + (old_point_byte >= start_point_byte	\
				    ? PT_BYTE - start_point_byte : 0));	\
   if (old != current_buffer)						\
     set_buffer_internal (old);

#define PRINTCHAR(ch) printchar (ch, printcharfun)

/* This is used to restore the saved contents of print_buffer
   when there is a recursive call to print.  */

static Lisp_Object
print_unwind (Lisp_Object saved_text)
{
  memcpy (print_buffer, SDATA (saved_text), SCHARS (saved_text));
  return Qnil;
}


/* Print character CH using method FUN.  FUN nil means print to
   print_buffer.  FUN t means print to echo area or stdout if
   non-interactive.  If FUN is neither nil nor t, call FUN with CH as
   argument.  */

static void
printchar (unsigned int ch, Lisp_Object fun)
{
  if (!NILP (fun) && !EQ (fun, Qt))
    call1 (fun, make_number (ch));
  else
    {
      unsigned char str[MAX_MULTIBYTE_LENGTH];
      int len = CHAR_STRING (ch, str);

      QUIT;

      if (NILP (fun))
	{
	  if (print_buffer_size - len <= print_buffer_pos_byte)
	    {
	      ptrdiff_t new_size;
	      if (STRING_BYTES_BOUND / 2 < print_buffer_size)
		string_overflow ();
	      new_size = print_buffer_size * 2;
	      print_buffer = (char *) xrealloc (print_buffer, new_size);
	      print_buffer_size = new_size;
	    }
	  memcpy (print_buffer + print_buffer_pos_byte, str, len);
	  print_buffer_pos += 1;
	  print_buffer_pos_byte += len;
	}
      else if (noninteractive)
	{
	  fwrite (str, 1, len, stdout);
	  noninteractive_need_newline = 1;
	}
      else
	{
	  int multibyte_p
	    = !NILP (BVAR (current_buffer, enable_multibyte_characters));

	  setup_echo_area_for_printing (multibyte_p);
	  insert_char (ch);
	  message_dolog ((char *) str, len, 0, multibyte_p);
	}
    }
}


/* Output SIZE characters, SIZE_BYTE bytes from string PTR using
   method PRINTCHARFUN.  If SIZE < 0, use the string length of PTR for
   both SIZE and SIZE_BYTE.  PRINTCHARFUN nil means output to
   print_buffer.  PRINTCHARFUN t means output to the echo area or to
   stdout if non-interactive.  If neither nil nor t, call Lisp
   function PRINTCHARFUN for each character printed.  MULTIBYTE
   non-zero means PTR contains multibyte characters.

   In the case where PRINTCHARFUN is nil, it is safe for PTR to point
   to data in a Lisp string.  Otherwise that is not safe.  */

static void
strout (const char *ptr, EMACS_INT size, EMACS_INT size_byte,
	Lisp_Object printcharfun)
{
  if (size < 0)
    size_byte = size = strlen (ptr);

  if (NILP (printcharfun))
    {
      if (print_buffer_size - size_byte < print_buffer_pos_byte)
	{
	  ptrdiff_t new_size;
	  if (STRING_BYTES_BOUND / 2 - size_byte < print_buffer_size)
	    string_overflow ();
	  new_size = print_buffer_size * 2 + size_byte;
	  print_buffer = (char *) xrealloc (print_buffer, new_size);
	  print_buffer_size = new_size;
	}
      memcpy (print_buffer + print_buffer_pos_byte, ptr, size_byte);
      print_buffer_pos += size;
      print_buffer_pos_byte += size_byte;
    }
  else if (noninteractive && EQ (printcharfun, Qt))
    {
      fwrite (ptr, 1, size_byte, stdout);
      noninteractive_need_newline = 1;
    }
  else if (EQ (printcharfun, Qt))
    {
      /* Output to echo area.  We're trying to avoid a little overhead
	 here, that's the reason we don't call printchar to do the
	 job.  */
      int i;
      int multibyte_p
	= !NILP (BVAR (current_buffer, enable_multibyte_characters));

      setup_echo_area_for_printing (multibyte_p);
      message_dolog (ptr, size_byte, 0, multibyte_p);

      if (size == size_byte)
	{
	  for (i = 0; i < size; ++i)
	    insert_char ((unsigned char) *ptr++);
	}
      else
	{
	  int len;
	  for (i = 0; i < size_byte; i += len)
	    {
	      int ch = STRING_CHAR_AND_LENGTH ((const unsigned char *) ptr + i,
					       len);
	      insert_char (ch);
	    }
	}
    }
  else
    {
      /* PRINTCHARFUN is a Lisp function.  */
      EMACS_INT i = 0;

      if (size == size_byte)
	{
	  while (i < size_byte)
	    {
	      int ch = ptr[i++];
	      PRINTCHAR (ch);
	    }
	}
      else
	{
	  while (i < size_byte)
	    {
	      /* Here, we must convert each multi-byte form to the
		 corresponding character code before handing it to
		 PRINTCHAR.  */
	      int len;
	      int ch = STRING_CHAR_AND_LENGTH ((const unsigned char *) ptr + i,
					       len);
	      PRINTCHAR (ch);
	      i += len;
	    }
	}
    }
}

/* Print the contents of a string STRING using PRINTCHARFUN.
   It isn't safe to use strout in many cases,
   because printing one char can relocate.  */

static void
print_string (Lisp_Object string, Lisp_Object printcharfun)
{
  if (EQ (printcharfun, Qt) || NILP (printcharfun))
    {
      EMACS_INT chars;

      if (print_escape_nonascii)
	string = string_escape_byte8 (string);

      if (STRING_MULTIBYTE (string))
	chars = SCHARS (string);
      else if (! print_escape_nonascii
	       && (EQ (printcharfun, Qt)
		   ? ! NILP (BVAR (&buffer_defaults, enable_multibyte_characters))
		   : ! NILP (BVAR (current_buffer, enable_multibyte_characters))))
	{
	  /* If unibyte string STRING contains 8-bit codes, we must
	     convert STRING to a multibyte string containing the same
	     character codes.  */
	  Lisp_Object newstr;
	  EMACS_INT bytes;

	  chars = SBYTES (string);
	  bytes = count_size_as_multibyte (SDATA (string), chars);
	  if (chars < bytes)
	    {
	      newstr = make_uninit_multibyte_string (chars, bytes);
	      memcpy (SDATA (newstr), SDATA (string), chars);
	      str_to_multibyte (SDATA (newstr), bytes, chars);
	      string = newstr;
	    }
	}
      else
	chars = SBYTES (string);

      if (EQ (printcharfun, Qt))
	{
	  /* Output to echo area.  */
	  EMACS_INT nbytes = SBYTES (string);
	  char *buffer;

	  /* Copy the string contents so that relocation of STRING by
	     GC does not cause trouble.  */
	  USE_SAFE_ALLOCA;

	  SAFE_ALLOCA (buffer, char *, nbytes);
	  memcpy (buffer, SDATA (string), nbytes);

	  strout (buffer, chars, SBYTES (string), printcharfun);

	  SAFE_FREE ();
	}
      else
	/* No need to copy, since output to print_buffer can't GC.  */
	strout (SSDATA (string), chars, SBYTES (string), printcharfun);
    }
  else
    {
      /* Otherwise, string may be relocated by printing one char.
	 So re-fetch the string address for each character.  */
      EMACS_INT i;
      EMACS_INT size = SCHARS (string);
      EMACS_INT size_byte = SBYTES (string);
      struct gcpro gcpro1;
      GCPRO1 (string);
      if (size == size_byte)
	for (i = 0; i < size; i++)
	  PRINTCHAR (SREF (string, i));
      else
	for (i = 0; i < size_byte; )
	  {
	    /* Here, we must convert each multi-byte form to the
	       corresponding character code before handing it to PRINTCHAR.  */
	    int len;
	    int ch = STRING_CHAR_AND_LENGTH (SDATA (string) + i, len);
	    PRINTCHAR (ch);
	    i += len;
	  }
      UNGCPRO;
    }
}

DEFUN ("write-char", Fwrite_char, Swrite_char, 1, 2, 0,
       doc: /* Output character CHARACTER to stream PRINTCHARFUN.
PRINTCHARFUN defaults to the value of `standard-output' (which see).  */)
  (Lisp_Object character, Lisp_Object printcharfun)
{
  PRINTDECLARE;

  if (NILP (printcharfun))
    printcharfun = Vstandard_output;
  CHECK_NUMBER (character);
  PRINTPREPARE;
  PRINTCHAR (XINT (character));
  PRINTFINISH;
  return character;
}

/* Used from outside of print.c to print a block of SIZE
   single-byte chars at DATA on the default output stream.
   Do not use this on the contents of a Lisp string.  */

void
write_string (const char *data, int size)
{
  PRINTDECLARE;
  Lisp_Object printcharfun;

  printcharfun = Vstandard_output;

  PRINTPREPARE;
  strout (data, size, size, printcharfun);
  PRINTFINISH;
}

/* Used to print a block of SIZE single-byte chars at DATA on a
   specified stream PRINTCHARFUN.
   Do not use this on the contents of a Lisp string.  */

static void
write_string_1 (const char *data, int size, Lisp_Object printcharfun)
{
  PRINTDECLARE;

  PRINTPREPARE;
  strout (data, size, size, printcharfun);
  PRINTFINISH;
}


void
temp_output_buffer_setup (const char *bufname)
{
  int count = SPECPDL_INDEX ();
  register struct buffer *old = current_buffer;
  register Lisp_Object buf;

  record_unwind_protect (set_buffer_if_live, Fcurrent_buffer ());

  Fset_buffer (Fget_buffer_create (build_string (bufname)));

  Fkill_all_local_variables ();
  delete_all_overlays (current_buffer);
  BVAR (current_buffer, directory) = BVAR (old, directory);
  BVAR (current_buffer, read_only) = Qnil;
  BVAR (current_buffer, filename) = Qnil;
  BVAR (current_buffer, undo_list) = Qt;
  eassert (current_buffer->overlays_before == NULL);
  eassert (current_buffer->overlays_after == NULL);
  BVAR (current_buffer, enable_multibyte_characters)
    = BVAR (&buffer_defaults, enable_multibyte_characters);
  specbind (Qinhibit_read_only, Qt);
  specbind (Qinhibit_modification_hooks, Qt);
  Ferase_buffer ();
  XSETBUFFER (buf, current_buffer);

  Frun_hooks (1, &Qtemp_buffer_setup_hook);

  unbind_to (count, Qnil);

  specbind (Qstandard_output, buf);
}

static void print (Lisp_Object obj, register Lisp_Object printcharfun, int escapeflag);
static void print_preprocess (Lisp_Object obj);
static void print_preprocess_string (INTERVAL interval, Lisp_Object arg);
static void print_object (Lisp_Object obj, register Lisp_Object printcharfun, int escapeflag);

DEFUN ("terpri", Fterpri, Sterpri, 0, 1, 0,
       doc: /* Output a newline to stream PRINTCHARFUN.
If PRINTCHARFUN is omitted or nil, the value of `standard-output' is used.  */)
  (Lisp_Object printcharfun)
{
  PRINTDECLARE;

  if (NILP (printcharfun))
    printcharfun = Vstandard_output;
  PRINTPREPARE;
  PRINTCHAR ('\n');
  PRINTFINISH;
  return Qt;
}

DEFUN ("prin1", Fprin1, Sprin1, 1, 2, 0,
       doc: /* Output the printed representation of OBJECT, any Lisp object.
Quoting characters are printed when needed to make output that `read'
can handle, whenever this is possible.  For complex objects, the behavior
is controlled by `print-level' and `print-length', which see.

OBJECT is any of the Lisp data types: a number, a string, a symbol,
a list, a buffer, a window, a frame, etc.

A printed representation of an object is text which describes that object.

Optional argument PRINTCHARFUN is the output stream, which can be one
of these:

   - a buffer, in which case output is inserted into that buffer at point;
   - a marker, in which case output is inserted at marker's position;
   - a function, in which case that function is called once for each
     character of OBJECT's printed representation;
   - a symbol, in which case that symbol's function definition is called; or
   - t, in which case the output is displayed in the echo area.

If PRINTCHARFUN is omitted, the value of `standard-output' (which see)
is used instead.  */)
  (Lisp_Object object, Lisp_Object printcharfun)
{
  PRINTDECLARE;

  if (NILP (printcharfun))
    printcharfun = Vstandard_output;
  PRINTPREPARE;
  print (object, printcharfun, 1);
  PRINTFINISH;
  return object;
}

/* a buffer which is used to hold output being built by prin1-to-string */
Lisp_Object Vprin1_to_string_buffer;

DEFUN ("prin1-to-string", Fprin1_to_string, Sprin1_to_string, 1, 2, 0,
       doc: /* Return a string containing the printed representation of OBJECT.
OBJECT can be any Lisp object.  This function outputs quoting characters
when necessary to make output that `read' can handle, whenever possible,
unless the optional second argument NOESCAPE is non-nil.  For complex objects,
the behavior is controlled by `print-level' and `print-length', which see.

OBJECT is any of the Lisp data types: a number, a string, a symbol,
a list, a buffer, a window, a frame, etc.

A printed representation of an object is text which describes that object.  */)
  (Lisp_Object object, Lisp_Object noescape)
{
  Lisp_Object printcharfun;
  /* struct gcpro gcpro1, gcpro2; */
  Lisp_Object save_deactivate_mark;
  int count = SPECPDL_INDEX ();
  struct buffer *previous;

  specbind (Qinhibit_modification_hooks, Qt);

  {
    PRINTDECLARE;

    /* Save and restore this--we are altering a buffer
       but we don't want to deactivate the mark just for that.
       No need for specbind, since errors deactivate the mark.  */
    save_deactivate_mark = Vdeactivate_mark;
    /* GCPRO2 (object, save_deactivate_mark); */
    abort_on_gc++;

    printcharfun = Vprin1_to_string_buffer;
    PRINTPREPARE;
    print (object, printcharfun, NILP (noescape));
    /* Make Vprin1_to_string_buffer be the default buffer after PRINTFINISH */
    PRINTFINISH;
  }

  previous = current_buffer;
  set_buffer_internal (XBUFFER (Vprin1_to_string_buffer));
  object = Fbuffer_string ();
  if (SBYTES (object) == SCHARS (object))
    STRING_SET_UNIBYTE (object);

  /* Note that this won't make prepare_to_modify_buffer call
     ask-user-about-supersession-threat because this buffer
     does not visit a file.  */
  Ferase_buffer ();
  set_buffer_internal (previous);

  Vdeactivate_mark = save_deactivate_mark;
  /* UNGCPRO; */

  abort_on_gc--;
  return unbind_to (count, object);
}

DEFUN ("princ", Fprinc, Sprinc, 1, 2, 0,
       doc: /* Output the printed representation of OBJECT, any Lisp object.
No quoting characters are used; no delimiters are printed around
the contents of strings.

OBJECT is any of the Lisp data types: a number, a string, a symbol,
a list, a buffer, a window, a frame, etc.

A printed representation of an object is text which describes that object.

Optional argument PRINTCHARFUN is the output stream, which can be one
of these:

   - a buffer, in which case output is inserted into that buffer at point;
   - a marker, in which case output is inserted at marker's position;
   - a function, in which case that function is called once for each
     character of OBJECT's printed representation;
   - a symbol, in which case that symbol's function definition is called; or
   - t, in which case the output is displayed in the echo area.

If PRINTCHARFUN is omitted, the value of `standard-output' (which see)
is used instead.  */)
  (Lisp_Object object, Lisp_Object printcharfun)
{
  PRINTDECLARE;

  if (NILP (printcharfun))
    printcharfun = Vstandard_output;
  PRINTPREPARE;
  print (object, printcharfun, 0);
  PRINTFINISH;
  return object;
}

DEFUN ("print", Fprint, Sprint, 1, 2, 0,
       doc: /* Output the printed representation of OBJECT, with newlines around it.
Quoting characters are printed when needed to make output that `read'
can handle, whenever this is possible.  For complex objects, the behavior
is controlled by `print-level' and `print-length', which see.

OBJECT is any of the Lisp data types: a number, a string, a symbol,
a list, a buffer, a window, a frame, etc.

A printed representation of an object is text which describes that object.

Optional argument PRINTCHARFUN is the output stream, which can be one
of these:

   - a buffer, in which case output is inserted into that buffer at point;
   - a marker, in which case output is inserted at marker's position;
   - a function, in which case that function is called once for each
     character of OBJECT's printed representation;
   - a symbol, in which case that symbol's function definition is called; or
   - t, in which case the output is displayed in the echo area.

If PRINTCHARFUN is omitted, the value of `standard-output' (which see)
is used instead.  */)
  (Lisp_Object object, Lisp_Object printcharfun)
{
  PRINTDECLARE;
  struct gcpro gcpro1;

  if (NILP (printcharfun))
    printcharfun = Vstandard_output;
  GCPRO1 (object);
  PRINTPREPARE;
  PRINTCHAR ('\n');
  print (object, printcharfun, 1);
  PRINTCHAR ('\n');
  PRINTFINISH;
  UNGCPRO;
  return object;
}

/* The subroutine object for external-debugging-output is kept here
   for the convenience of the debugger.  */
Lisp_Object Qexternal_debugging_output;

DEFUN ("external-debugging-output", Fexternal_debugging_output, Sexternal_debugging_output, 1, 1, 0,
       doc: /* Write CHARACTER to stderr.
You can call print while debugging emacs, and pass it this function
to make it write to the debugging output.  */)
  (Lisp_Object character)
{
  CHECK_NUMBER (character);
  putc ((int) XINT (character), stderr);

#ifdef WINDOWSNT
  /* Send the output to a debugger (nothing happens if there isn't one).  */
  if (print_output_debug_flag)
    {
      char buf[2] = {(char) XINT (character), '\0'};
      OutputDebugString (buf);
    }
#endif

  return character;
}

/* This function is never called.  Its purpose is to prevent
   print_output_debug_flag from being optimized away.  */

extern void debug_output_compilation_hack (int) EXTERNALLY_VISIBLE;
void
debug_output_compilation_hack (int x)
{
  print_output_debug_flag = x;
}

#if defined (GNU_LINUX)

/* This functionality is not vitally important in general, so we rely on
   non-portable ability to use stderr as lvalue.  */

#define WITH_REDIRECT_DEBUGGING_OUTPUT 1

static FILE *initial_stderr_stream = NULL;

DEFUN ("redirect-debugging-output", Fredirect_debugging_output, Sredirect_debugging_output,
       1, 2,
       "FDebug output file: \nP",
       doc: /* Redirect debugging output (stderr stream) to file FILE.
If FILE is nil, reset target to the initial stderr stream.
Optional arg APPEND non-nil (interactively, with prefix arg) means
append to existing target file.  */)
  (Lisp_Object file, Lisp_Object append)
{
  if (initial_stderr_stream != NULL)
    {
      BLOCK_INPUT;
      fclose (stderr);
      UNBLOCK_INPUT;
    }
  stderr = initial_stderr_stream;
  initial_stderr_stream = NULL;

  if (STRINGP (file))
    {
      file = Fexpand_file_name (file, Qnil);
      initial_stderr_stream = stderr;
      stderr = fopen (SSDATA (file), NILP (append) ? "w" : "a");
      if (stderr == NULL)
	{
	  stderr = initial_stderr_stream;
	  initial_stderr_stream = NULL;
	  report_file_error ("Cannot open debugging output stream",
			     Fcons (file, Qnil));
	}
    }
  return Qnil;
}
#endif /* GNU_LINUX */


/* This is the interface for debugging printing.  */

void
debug_print (Lisp_Object arg)
{
  Fprin1 (arg, Qexternal_debugging_output);
  fprintf (stderr, "\r\n");
}

void safe_debug_print (Lisp_Object) EXTERNALLY_VISIBLE;
void
safe_debug_print (Lisp_Object arg)
{
  int valid = valid_lisp_object_p (arg);

  if (valid > 0)
    debug_print (arg);
  else
    fprintf (stderr, "#<%s_LISP_OBJECT 0x%08"pI"x>\r\n",
	     !valid ? "INVALID" : "SOME",
	     XHASH (arg));
}


DEFUN ("error-message-string", Ferror_message_string, Serror_message_string,
       1, 1, 0,
       doc: /* Convert an error value (ERROR-SYMBOL . DATA) to an error message.
See Info anchor `(elisp)Definition of signal' for some details on how this
error message is constructed.  */)
  (Lisp_Object obj)
{
  struct buffer *old = current_buffer;
  Lisp_Object value;
  struct gcpro gcpro1;

  /* If OBJ is (error STRING), just return STRING.
     That is not only faster, it also avoids the need to allocate
     space here when the error is due to memory full.  */
  if (CONSP (obj) && EQ (XCAR (obj), Qerror)
      && CONSP (XCDR (obj))
      && STRINGP (XCAR (XCDR (obj)))
      && NILP (XCDR (XCDR (obj))))
    return XCAR (XCDR (obj));

  print_error_message (obj, Vprin1_to_string_buffer, 0, Qnil);

  set_buffer_internal (XBUFFER (Vprin1_to_string_buffer));
  value = Fbuffer_string ();

  GCPRO1 (value);
  Ferase_buffer ();
  set_buffer_internal (old);
  UNGCPRO;

  return value;
}

/* Print an error message for the error DATA onto Lisp output stream
   STREAM (suitable for the print functions).
   CONTEXT is a C string describing the context of the error.
   CALLER is the Lisp function inside which the error was signaled.  */

void
print_error_message (Lisp_Object data, Lisp_Object stream, const char *context,
		     Lisp_Object caller)
{
  Lisp_Object errname, errmsg, file_error, tail;
  struct gcpro gcpro1;
  int i;

  if (context != 0)
    write_string_1 (context, -1, stream);

  /* If we know from where the error was signaled, show it in
   *Messages*.  */
  if (!NILP (caller) && SYMBOLP (caller))
    {
      Lisp_Object cname = SYMBOL_NAME (caller);
      char *name = alloca (SBYTES (cname));
      memcpy (name, SDATA (cname), SBYTES (cname));
      message_dolog (name, SBYTES (cname), 0, 0);
      message_dolog (": ", 2, 0, 0);
    }

  errname = Fcar (data);

  if (EQ (errname, Qerror))
    {
      data = Fcdr (data);
      if (!CONSP (data))
	data = Qnil;
      errmsg = Fcar (data);
      file_error = Qnil;
    }
  else
    {
      Lisp_Object error_conditions;
      errmsg = Fget (errname, Qerror_message);
      error_conditions = Fget (errname, Qerror_conditions);
      file_error = Fmemq (Qfile_error, error_conditions);
    }

  /* Print an error message including the data items.  */

  tail = Fcdr_safe (data);
  GCPRO1 (tail);

  /* For file-error, make error message by concatenating
     all the data items.  They are all strings.  */
  if (!NILP (file_error) && CONSP (tail))
    errmsg = XCAR (tail), tail = XCDR (tail);

  if (STRINGP (errmsg))
    Fprinc (errmsg, stream);
  else
    write_string_1 ("peculiar error", -1, stream);

  for (i = 0; CONSP (tail); tail = XCDR (tail), i = 1)
    {
      Lisp_Object obj;

      write_string_1 (i ? ", " : ": ", 2, stream);
      obj = XCAR (tail);
      if (!NILP (file_error) || EQ (errname, Qend_of_file))
	Fprinc (obj, stream);
      else
	Fprin1 (obj, stream);
    }

  UNGCPRO;
}



/*
 * The buffer should be at least as large as the max string size of the
 * largest float, printed in the biggest notation.  This is undoubtedly
 * 20d float_output_format, with the negative of the C-constant "HUGE"
 * from <math.h>.
 *
 * On the vax the worst case is -1e38 in 20d format which takes 61 bytes.
 *
 * I assume that IEEE-754 format numbers can take 329 bytes for the worst
 * case of -1e307 in 20d float_output_format. What is one to do (short of
 * re-writing _doprnt to be more sane)?
 * 			-wsr
 * Given the above, the buffer must be least FLOAT_TO_STRING_BUFSIZE bytes.
 */

void
float_to_string (char *buf, double data)
{
  char *cp;
  int width;

  /* Check for plus infinity in a way that won't lose
     if there is no plus infinity.  */
  if (data == data / 2 && data > 1.0)
    {
      strcpy (buf, "1.0e+INF");
      return;
    }
  /* Likewise for minus infinity.  */
  if (data == data / 2 && data < -1.0)
    {
      strcpy (buf, "-1.0e+INF");
      return;
    }
  /* Check for NaN in a way that won't fail if there are no NaNs.  */
  if (! (data * 0.0 >= 0.0))
    {
      /* Prepend "-" if the NaN's sign bit is negative.
	 The sign bit of a double is the bit that is 1 in -0.0.  */
      int i;
      union { double d; char c[sizeof (double)]; } u_data, u_minus_zero;
      u_data.d = data;
      u_minus_zero.d = - 0.0;
      for (i = 0; i < sizeof (double); i++)
	if (u_data.c[i] & u_minus_zero.c[i])
	  {
	    *buf++ = '-';
	    break;
	  }

      strcpy (buf, "0.0e+NaN");
      return;
    }

  if (NILP (Vfloat_output_format)
      || !STRINGP (Vfloat_output_format))
  lose:
    {
      /* Generate the fewest number of digits that represent the
	 floating point value without losing information.  */
      dtoastr (buf, FLOAT_TO_STRING_BUFSIZE - 2, 0, 0, data);
      /* The decimal point must be printed, or the byte compiler can
	 get confused (Bug#8033). */
      width = 1;
    }
  else			/* oink oink */
    {
      /* Check that the spec we have is fully valid.
	 This means not only valid for printf,
	 but meant for floats, and reasonable.  */
      cp = SSDATA (Vfloat_output_format);

      if (cp[0] != '%')
	goto lose;
      if (cp[1] != '.')
	goto lose;

      cp += 2;

      /* Check the width specification.  */
      width = -1;
      if ('0' <= *cp && *cp <= '9')
	{
	  width = 0;
	  do
	    {
	      width = (width * 10) + (*cp++ - '0');
	      if (DBL_DIG < width)
		goto lose;
	    }
	  while (*cp >= '0' && *cp <= '9');

	  /* A precision of zero is valid only for %f.  */
	  if (width == 0 && *cp != 'f')
	    goto lose;
	}

      if (*cp != 'e' && *cp != 'f' && *cp != 'g')
	goto lose;

      if (cp[1] != 0)
	goto lose;

      sprintf (buf, SSDATA (Vfloat_output_format), data);
    }

  /* Make sure there is a decimal point with digit after, or an
     exponent, so that the value is readable as a float.  But don't do
     this with "%.0f"; it's valid for that not to produce a decimal
     point.  Note that width can be 0 only for %.0f.  */
  if (width != 0)
    {
      for (cp = buf; *cp; cp++)
	if ((*cp < '0' || *cp > '9') && *cp != '-')
	  break;

      if (*cp == '.' && cp[1] == 0)
	{
	  cp[1] = '0';
	  cp[2] = 0;
	}
      else if (*cp == 0)
	{
	  *cp++ = '.';
	  *cp++ = '0';
	  *cp++ = 0;
	}
    }
}


static void
print (Lisp_Object obj, register Lisp_Object printcharfun, int escapeflag)
{
  new_backquote_output = 0;

  /* Reset print_number_index and Vprint_number_table only when
     the variable Vprint_continuous_numbering is nil.  Otherwise,
     the values of these variables will be kept between several
     print functions.  */
  if (NILP (Vprint_continuous_numbering)
      || NILP (Vprint_number_table))
    {
      print_number_index = 0;
      Vprint_number_table = Qnil;
    }

  /* Construct Vprint_number_table for print-gensym and print-circle.  */
  if (!NILP (Vprint_gensym) || !NILP (Vprint_circle))
    {
      /* Construct Vprint_number_table.
	 This increments print_number_index for the objects added.  */
      print_depth = 0;
      print_preprocess (obj);

      if (HASH_TABLE_P (Vprint_number_table))
	{ /* Remove unnecessary objects, which appear only once in OBJ;
	     that is, whose status is Qt.
	     Maybe a better way to do that is to copy elements to
	     a new hash table.  */
	  struct Lisp_Hash_Table *h = XHASH_TABLE (Vprint_number_table);
	  EMACS_INT i;

	  for (i = 0; i < HASH_TABLE_SIZE (h); ++i)
	    if (!NILP (HASH_HASH (h, i))
		&& EQ (HASH_VALUE (h, i), Qt))
	      Fremhash (HASH_KEY (h, i), Vprint_number_table);
	}
    }

  print_depth = 0;
  print_object (obj, printcharfun, escapeflag);
}

#define PRINT_CIRCLE_CANDIDATE_P(obj)					\
  (STRINGP (obj) || CONSP (obj)						\
   || (VECTORLIKEP (obj)						\
      && (VECTORP (obj) || COMPILEDP (obj)				\
	  || CHAR_TABLE_P (obj) || SUB_CHAR_TABLE_P (obj)		\
	  || HASH_TABLE_P (obj) || FONTP (obj)))			\
   || (! NILP (Vprint_gensym)						\
       && SYMBOLP (obj)							\
       && !SYMBOL_INTERNED_P (obj)))

/* Construct Vprint_number_table according to the structure of OBJ.
   OBJ itself and all its elements will be added to Vprint_number_table
   recursively if it is a list, vector, compiled function, char-table,
   string (its text properties will be traced), or a symbol that has
   no obarray (this is for the print-gensym feature).
   The status fields of Vprint_number_table mean whether each object appears
   more than once in OBJ: Qnil at the first time, and Qt after that .  */
static void
print_preprocess (Lisp_Object obj)
{
  int i;
  EMACS_INT size;
  int loop_count = 0;
  Lisp_Object halftail;

  /* Give up if we go so deep that print_object will get an error.  */
  /* See similar code in print_object.  */
  if (print_depth >= PRINT_CIRCLE)
    error ("Apparently circular structure being printed");

  /* Avoid infinite recursion for circular nested structure
     in the case where Vprint_circle is nil.  */
  if (NILP (Vprint_circle))
    {
      for (i = 0; i < print_depth; i++)
	if (EQ (obj, being_printed[i]))
	  return;
      being_printed[print_depth] = obj;
    }

  print_depth++;
  halftail = obj;

 loop:
  if (PRINT_CIRCLE_CANDIDATE_P (obj))
    {
      if (!HASH_TABLE_P (Vprint_number_table))
	{
	  Lisp_Object args[2];
	  args[0] = QCtest;
	  args[1] = Qeq;
	  Vprint_number_table = Fmake_hash_table (2, args);
	}

      /* In case print-circle is nil and print-gensym is t,
	 add OBJ to Vprint_number_table only when OBJ is a symbol.  */
      if (! NILP (Vprint_circle) || SYMBOLP (obj))
	{
	  Lisp_Object num = Fgethash (obj, Vprint_number_table, Qnil);
	  if (!NILP (num)
	      /* If Vprint_continuous_numbering is non-nil and OBJ is a gensym,
		 always print the gensym with a number.  This is a special for
		 the lisp function byte-compile-output-docform.  */
	      || (!NILP (Vprint_continuous_numbering)
		  && SYMBOLP (obj)
		  && !SYMBOL_INTERNED_P (obj)))
	    { /* OBJ appears more than once.	Let's remember that.  */
	      if (!INTEGERP (num))
		{
		  print_number_index++;
		  /* Negative number indicates it hasn't been printed yet.  */
		  Fputhash (obj, make_number (- print_number_index),
			    Vprint_number_table);
		}
	      print_depth--;
	      return;
	    }
	  else
	    /* OBJ is not yet recorded.  Let's add to the table.  */
	    Fputhash (obj, Qt, Vprint_number_table);
	}

      switch (XTYPE (obj))
	{
	case Lisp_String:
	  /* A string may have text properties, which can be circular.  */
	  traverse_intervals_noorder (STRING_INTERVALS (obj),
				      print_preprocess_string, Qnil);
	  break;

	case Lisp_Cons:
	  /* Use HALFTAIL and LOOP_COUNT to detect circular lists,
	     just as in print_object.  */
	  if (loop_count && EQ (obj, halftail))
	    break;
	  print_preprocess (XCAR (obj));
	  obj = XCDR (obj);
	  loop_count++;
	  if (!(loop_count & 1))
	    halftail = XCDR (halftail);
	  goto loop;

	case Lisp_Vectorlike:
	  size = ASIZE (obj);
	  if (size & PSEUDOVECTOR_FLAG)
	    size &= PSEUDOVECTOR_SIZE_MASK;
	  for (i = 0; i < size; i++)
	    print_preprocess (XVECTOR (obj)->contents[i]);
	  if (HASH_TABLE_P (obj))
	    { /* For hash tables, the key_and_value slot is past
		 `size' because it needs to be marked specially in case
		 the table is weak.  */
	      struct Lisp_Hash_Table *h = XHASH_TABLE (obj);
	      print_preprocess (h->key_and_value);
	    }
	  break;

	default:
	  break;
	}
    }
  print_depth--;
}

static void
print_preprocess_string (INTERVAL interval, Lisp_Object arg)
{
  print_preprocess (interval->plist);
}

static void print_check_string_charset_prop (INTERVAL interval, Lisp_Object string);

#define PRINT_STRING_NON_CHARSET_FOUND 1
#define PRINT_STRING_UNSAFE_CHARSET_FOUND 2

/* Bitwise or of the above macros. */
static int print_check_string_result;

static void
print_check_string_charset_prop (INTERVAL interval, Lisp_Object string)
{
  Lisp_Object val;

  if (NILP (interval->plist)
      || (print_check_string_result == (PRINT_STRING_NON_CHARSET_FOUND
					| PRINT_STRING_UNSAFE_CHARSET_FOUND)))
    return;
  for (val = interval->plist; CONSP (val) && ! EQ (XCAR (val), Qcharset);
       val = XCDR (XCDR (val)));
  if (! CONSP (val))
    {
      print_check_string_result |= PRINT_STRING_NON_CHARSET_FOUND;
      return;
    }
  if (! (print_check_string_result & PRINT_STRING_NON_CHARSET_FOUND))
    {
      if (! EQ (val, interval->plist)
	  || CONSP (XCDR (XCDR (val))))
	print_check_string_result |= PRINT_STRING_NON_CHARSET_FOUND;
    }
  if (NILP (Vprint_charset_text_property)
      || ! (print_check_string_result & PRINT_STRING_UNSAFE_CHARSET_FOUND))
    {
      int i, c;
      EMACS_INT charpos = interval->position;
      EMACS_INT bytepos = string_char_to_byte (string, charpos);
      Lisp_Object charset;

      charset = XCAR (XCDR (val));
      for (i = 0; i < LENGTH (interval); i++)
	{
	  FETCH_STRING_CHAR_ADVANCE (c, string, charpos, bytepos);
	  if (! ASCII_CHAR_P (c)
	      && ! EQ (CHARSET_NAME (CHAR_CHARSET (c)), charset))
	    {
	      print_check_string_result |= PRINT_STRING_UNSAFE_CHARSET_FOUND;
	      break;
	    }
	}
    }
}

/* The value is (charset . nil).  */
static Lisp_Object print_prune_charset_plist;

static Lisp_Object
print_prune_string_charset (Lisp_Object string)
{
  print_check_string_result = 0;
  traverse_intervals (STRING_INTERVALS (string), 0,
		      print_check_string_charset_prop, string);
  if (! (print_check_string_result & PRINT_STRING_UNSAFE_CHARSET_FOUND))
    {
      string = Fcopy_sequence (string);
      if (print_check_string_result & PRINT_STRING_NON_CHARSET_FOUND)
	{
	  if (NILP (print_prune_charset_plist))
	    print_prune_charset_plist = Fcons (Qcharset, Qnil);
	  Fremove_text_properties (make_number (0),
				   make_number (SCHARS (string)),
				   print_prune_charset_plist, string);
	}
      else
	Fset_text_properties (make_number (0), make_number (SCHARS (string)),
			      Qnil, string);
    }
  return string;
}

static void
print_object (Lisp_Object obj, register Lisp_Object printcharfun, int escapeflag)
{
  char buf[max (sizeof "from..to..in " + 2 * INT_STRLEN_BOUND (EMACS_INT),
		max (sizeof " . #" + INT_STRLEN_BOUND (printmax_t),
		     40))];

  QUIT;

  /* See similar code in print_preprocess.  */
  if (print_depth >= PRINT_CIRCLE)
    error ("Apparently circular structure being printed");

  /* Detect circularities and truncate them.  */
  if (PRINT_CIRCLE_CANDIDATE_P (obj))
    {
      if (NILP (Vprint_circle) && NILP (Vprint_gensym))
	{
	  /* Simple but incomplete way.  */
	  int i;
	  for (i = 0; i < print_depth; i++)
	    if (EQ (obj, being_printed[i]))
	      {
		sprintf (buf, "#%d", i);
		strout (buf, -1, -1, printcharfun);
		return;
	      }
	  being_printed[print_depth] = obj;
	}
      else
	{
	  /* With the print-circle feature.  */
	  Lisp_Object num = Fgethash (obj, Vprint_number_table, Qnil);
	  if (INTEGERP (num))
	    {
	      EMACS_INT n = XINT (num);
	      if (n < 0)
		{ /* Add a prefix #n= if OBJ has not yet been printed;
		     that is, its status field is nil.  */
		  sprintf (buf, "#%"pI"d=", -n);
		  strout (buf, -1, -1, printcharfun);
		  /* OBJ is going to be printed.  Remember that fact.  */
		  Fputhash (obj, make_number (- n), Vprint_number_table);
		}
	      else
		{
		  /* Just print #n# if OBJ has already been printed.  */
		  sprintf (buf, "#%"pI"d#", n);
		  strout (buf, -1, -1, printcharfun);
		  return;
		}
	    }
	}
    }

  print_depth++;

  switch (XTYPE (obj))
    {
    case_Lisp_Int:
      sprintf (buf, "%"pI"d", XINT (obj));
      strout (buf, -1, -1, printcharfun);
      break;

    case Lisp_Float:
      {
	char pigbuf[FLOAT_TO_STRING_BUFSIZE];

	float_to_string (pigbuf, XFLOAT_DATA (obj));
	strout (pigbuf, -1, -1, printcharfun);
      }
      break;

    case Lisp_String:
      if (!escapeflag)
	print_string (obj, printcharfun);
      else
	{
	  register EMACS_INT i_byte;
	  struct gcpro gcpro1;
	  unsigned char *str;
	  EMACS_INT size_byte;
	  /* 1 means we must ensure that the next character we output
	     cannot be taken as part of a hex character escape.  */
	  int need_nonhex = 0;
	  int multibyte = STRING_MULTIBYTE (obj);

	  GCPRO1 (obj);

	  if (! EQ (Vprint_charset_text_property, Qt))
	    obj = print_prune_string_charset (obj);

	  if (!NULL_INTERVAL_P (STRING_INTERVALS (obj)))
	    {
	      PRINTCHAR ('#');
	      PRINTCHAR ('(');
	    }

	  PRINTCHAR ('\"');
	  str = SDATA (obj);
	  size_byte = SBYTES (obj);

	  for (i_byte = 0; i_byte < size_byte;)
	    {
	      /* Here, we must convert each multi-byte form to the
		 corresponding character code before handing it to PRINTCHAR.  */
	      int len;
	      int c;

	      if (multibyte)
		{
		  c = STRING_CHAR_AND_LENGTH (str + i_byte, len);
		  i_byte += len;
		}
	      else
		c = str[i_byte++];

	      QUIT;

	      if (c == '\n' && print_escape_newlines)
		{
		  PRINTCHAR ('\\');
		  PRINTCHAR ('n');
		}
	      else if (c == '\f' && print_escape_newlines)
		{
		  PRINTCHAR ('\\');
		  PRINTCHAR ('f');
		}
	      else if (multibyte
		       && (CHAR_BYTE8_P (c)
			   || (! ASCII_CHAR_P (c) && print_escape_multibyte)))
		{
		  /* When multibyte is disabled,
		     print multibyte string chars using hex escapes.
		     For a char code that could be in a unibyte string,
		     when found in a multibyte string, always use a hex escape
		     so it reads back as multibyte.  */
		  char outbuf[50];

		  if (CHAR_BYTE8_P (c))
		    sprintf (outbuf, "\\%03o", CHAR_TO_BYTE8 (c));
		  else
		    {
		      sprintf (outbuf, "\\x%04x", c);
		      need_nonhex = 1;
		    }
		  strout (outbuf, -1, -1, printcharfun);
		}
	      else if (! multibyte
		       && SINGLE_BYTE_CHAR_P (c) && ! ASCII_BYTE_P (c)
		       && print_escape_nonascii)
		{
		  /* When printing in a multibyte buffer
		     or when explicitly requested,
		     print single-byte non-ASCII string chars
		     using octal escapes.  */
		  char outbuf[5];
		  sprintf (outbuf, "\\%03o", c);
		  strout (outbuf, -1, -1, printcharfun);
		}
	      else
		{
		  /* If we just had a hex escape, and this character
		     could be taken as part of it,
		     output `\ ' to prevent that.  */
		  if (need_nonhex)
		    {
		      need_nonhex = 0;
		      if ((c >= 'a' && c <= 'f')
			  || (c >= 'A' && c <= 'F')
			  || (c >= '0' && c <= '9'))
			strout ("\\ ", -1, -1, printcharfun);
		    }

		  if (c == '\"' || c == '\\')
		    PRINTCHAR ('\\');
		  PRINTCHAR (c);
		}
	    }
	  PRINTCHAR ('\"');

	  if (!NULL_INTERVAL_P (STRING_INTERVALS (obj)))
	    {
	      traverse_intervals (STRING_INTERVALS (obj),
				  0, print_interval, printcharfun);
	      PRINTCHAR (')');
	    }

	  UNGCPRO;
	}
      break;

    case Lisp_Symbol:
      {
	register int confusing;
	register unsigned char *p = SDATA (SYMBOL_NAME (obj));
	register unsigned char *end = p + SBYTES (SYMBOL_NAME (obj));
	register int c;
	int i, i_byte;
	EMACS_INT size_byte;
	Lisp_Object name;

	name = SYMBOL_NAME (obj);

	if (p != end && (*p == '-' || *p == '+')) p++;
	if (p == end)
	  confusing = 0;
	/* If symbol name begins with a digit, and ends with a digit,
	   and contains nothing but digits and `e', it could be treated
	   as a number.  So set CONFUSING.

	   Symbols that contain periods could also be taken as numbers,
	   but periods are always escaped, so we don't have to worry
	   about them here.  */
	else if (*p >= '0' && *p <= '9'
		 && end[-1] >= '0' && end[-1] <= '9')
	  {
	    while (p != end && ((*p >= '0' && *p <= '9')
				/* Needed for \2e10.  */
				|| *p == 'e' || *p == 'E'))
	      p++;
	    confusing = (end == p);
	  }
	else
	  confusing = 0;

	size_byte = SBYTES (name);

	if (! NILP (Vprint_gensym) && !SYMBOL_INTERNED_P (obj))
	  {
	    PRINTCHAR ('#');
	    PRINTCHAR (':');
	  }
	else if (size_byte == 0)
	  {
	    PRINTCHAR ('#');
	    PRINTCHAR ('#');
	    break;
	  }

	for (i = 0, i_byte = 0; i_byte < size_byte;)
	  {
	    /* Here, we must convert each multi-byte form to the
	       corresponding character code before handing it to PRINTCHAR.  */
	    FETCH_STRING_CHAR_ADVANCE (c, name, i, i_byte);
	    QUIT;

	    if (escapeflag)
	      {
		if (c == '\"' || c == '\\' || c == '\''
		    || c == ';' || c == '#' || c == '(' || c == ')'
		    || c == ',' || c == '.' || c == '`'
		    || c == '[' || c == ']' || c == '?' || c <= 040
		    || confusing)
		  PRINTCHAR ('\\'), confusing = 0;
	      }
	    PRINTCHAR (c);
	  }
      }
      break;

    case Lisp_Cons:
      /* If deeper than spec'd depth, print placeholder.  */
      if (INTEGERP (Vprint_level)
	  && print_depth > XINT (Vprint_level))
	strout ("...", -1, -1, printcharfun);
      else if (print_quoted && CONSP (XCDR (obj)) && NILP (XCDR (XCDR (obj)))
	       && (EQ (XCAR (obj), Qquote)))
	{
	  PRINTCHAR ('\'');
	  print_object (XCAR (XCDR (obj)), printcharfun, escapeflag);
	}
      else if (print_quoted && CONSP (XCDR (obj)) && NILP (XCDR (XCDR (obj)))
	       && (EQ (XCAR (obj), Qfunction)))
	{
	  PRINTCHAR ('#');
	  PRINTCHAR ('\'');
	  print_object (XCAR (XCDR (obj)), printcharfun, escapeflag);
	}
      else if (print_quoted && CONSP (XCDR (obj)) && NILP (XCDR (XCDR (obj)))
	       && ((EQ (XCAR (obj), Qbackquote))))
	{
	  print_object (XCAR (obj), printcharfun, 0);
	  new_backquote_output++;
	  print_object (XCAR (XCDR (obj)), printcharfun, escapeflag);
	  new_backquote_output--;
	}
      else if (print_quoted && CONSP (XCDR (obj)) && NILP (XCDR (XCDR (obj)))
	       && new_backquote_output
	       && ((EQ (XCAR (obj), Qbackquote)
		    || EQ (XCAR (obj), Qcomma)
		    || EQ (XCAR (obj), Qcomma_at)
		    || EQ (XCAR (obj), Qcomma_dot))))
	{
	  print_object (XCAR (obj), printcharfun, 0);
	  new_backquote_output--;
	  print_object (XCAR (XCDR (obj)), printcharfun, escapeflag);
	  new_backquote_output++;
	}
      else
	{
	  PRINTCHAR ('(');

	  {
	    printmax_t i, print_length;
	    Lisp_Object halftail = obj;

	    /* Negative values of print-length are invalid in CL.
	       Treat them like nil, as CMUCL does.  */
	    if (NATNUMP (Vprint_length))
	      print_length = XFASTINT (Vprint_length);
	    else
	      print_length = TYPE_MAXIMUM (printmax_t);

	    i = 0;
	    while (CONSP (obj))
	      {
		/* Detect circular list.  */
		if (NILP (Vprint_circle))
		  {
		    /* Simple but incomplete way.  */
		    if (i != 0 && EQ (obj, halftail))
		      {
			sprintf (buf, " . #%"pMd, i / 2);
			strout (buf, -1, -1, printcharfun);
			goto end_of_list;
		      }
		  }
		else
		  {
		    /* With the print-circle feature.  */
		    if (i != 0)
		      {
			Lisp_Object num = Fgethash (obj, Vprint_number_table, Qnil);
			if (INTEGERP (num))
			  {
			    strout (" . ", 3, 3, printcharfun);
			    print_object (obj, printcharfun, escapeflag);
			    goto end_of_list;
			  }
		      }
		  }

		if (i)
		  PRINTCHAR (' ');

		if (print_length <= i)
		  {
		    strout ("...", 3, 3, printcharfun);
		    goto end_of_list;
		  }

		i++;
		print_object (XCAR (obj), printcharfun, escapeflag);

		obj = XCDR (obj);
		if (!(i & 1))
		  halftail = XCDR (halftail);
	      }
	  }

	  /* OBJ non-nil here means it's the end of a dotted list.  */
	  if (!NILP (obj))
	    {
	      strout (" . ", 3, 3, printcharfun);
	      print_object (obj, printcharfun, escapeflag);
	    }

	end_of_list:
	  PRINTCHAR (')');
	}
      break;

    case Lisp_Vectorlike:
      if (PROCESSP (obj))
	{
	  if (escapeflag)
	    {
	      strout ("#<process ", -1, -1, printcharfun);
	      print_string (XPROCESS (obj)->name, printcharfun);
	      PRINTCHAR ('>');
	    }
	  else
	    print_string (XPROCESS (obj)->name, printcharfun);
	}
      else if (BOOL_VECTOR_P (obj))
	{
	  ptrdiff_t i;
	  register unsigned char c;
	  struct gcpro gcpro1;
	  EMACS_INT size_in_chars
	    = ((XBOOL_VECTOR (obj)->size + BOOL_VECTOR_BITS_PER_CHAR - 1)
	       / BOOL_VECTOR_BITS_PER_CHAR);

	  GCPRO1 (obj);

	  PRINTCHAR ('#');
	  PRINTCHAR ('&');
	  sprintf (buf, "%"pI"d", XBOOL_VECTOR (obj)->size);
	  strout (buf, -1, -1, printcharfun);
	  PRINTCHAR ('\"');

	  /* Don't print more characters than the specified maximum.
	     Negative values of print-length are invalid.  Treat them
	     like a print-length of nil.  */
	  if (NATNUMP (Vprint_length)
	      && XFASTINT (Vprint_length) < size_in_chars)
	    size_in_chars = XFASTINT (Vprint_length);

	  for (i = 0; i < size_in_chars; i++)
	    {
	      QUIT;
	      c = XBOOL_VECTOR (obj)->data[i];
	      if (c == '\n' && print_escape_newlines)
		{
		  PRINTCHAR ('\\');
		  PRINTCHAR ('n');
		}
	      else if (c == '\f' && print_escape_newlines)
		{
		  PRINTCHAR ('\\');
		  PRINTCHAR ('f');
		}
	      else if (c > '\177')
		{
		  /* Use octal escapes to avoid encoding issues.  */
		  PRINTCHAR ('\\');
		  PRINTCHAR ('0' + ((c >> 6) & 3));
		  PRINTCHAR ('0' + ((c >> 3) & 7));
		  PRINTCHAR ('0' + (c & 7));
		}
	      else
		{
		  if (c == '\"' || c == '\\')
		    PRINTCHAR ('\\');
		  PRINTCHAR (c);
		}
	    }
	  PRINTCHAR ('\"');

	  UNGCPRO;
	}
      else if (SUBRP (obj))
	{
	  strout ("#<subr ", -1, -1, printcharfun);
	  strout (XSUBR (obj)->symbol_name, -1, -1, printcharfun);
	  PRINTCHAR ('>');
	}
      else if (WINDOWP (obj))
	{
	  strout ("#<window ", -1, -1, printcharfun);
	  sprintf (buf, "%"pI"d", XFASTINT (XWINDOW (obj)->sequence_number));
	  strout (buf, -1, -1, printcharfun);
	  if (!NILP (XWINDOW (obj)->buffer))
	    {
	      strout (" on ", -1, -1, printcharfun);
	      print_string (BVAR (XBUFFER (XWINDOW (obj)->buffer), name), printcharfun);
	    }
	  PRINTCHAR ('>');
	}
      else if (TERMINALP (obj))
	{
	  struct terminal *t = XTERMINAL (obj);
	  strout ("#<terminal ", -1, -1, printcharfun);
	  sprintf (buf, "%d", t->id);
	  strout (buf, -1, -1, printcharfun);
	  if (t->name)
	    {
	      strout (" on ", -1, -1, printcharfun);
	      strout (t->name, -1, -1, printcharfun);
	    }
	  PRINTCHAR ('>');
	}
      else if (HASH_TABLE_P (obj))
	{
	  struct Lisp_Hash_Table *h = XHASH_TABLE (obj);
	  int i;
	  EMACS_INT real_size, size;
#if 0
	  strout ("#<hash-table", -1, -1, printcharfun);
	  if (SYMBOLP (h->test))
	    {
	      PRINTCHAR (' ');
	      PRINTCHAR ('\'');
	      strout (SDATA (SYMBOL_NAME (h->test)), -1, -1, printcharfun);
	      PRINTCHAR (' ');
	      strout (SDATA (SYMBOL_NAME (h->weak)), -1, -1, printcharfun);
	      PRINTCHAR (' ');
	      sprintf (buf, "%"pI"d/%"pI"d", h->count, ASIZE (h->next));
	      strout (buf, -1, -1, printcharfun);
	    }
	  sprintf (buf, " %p", h);
	  strout (buf, -1, -1, printcharfun);
	  PRINTCHAR ('>');
#endif
	  /* Implement a readable output, e.g.:
	    #s(hash-table size 2 test equal data (k1 v1 k2 v2)) */
	  /* Always print the size. */
	  sprintf (buf, "#s(hash-table size %"pI"d", ASIZE (h->next));
	  strout (buf, -1, -1, printcharfun);

	  if (!NILP (h->test))
	    {
	      strout (" test ", -1, -1, printcharfun);
	      print_object (h->test, printcharfun, escapeflag);
	    }

	  if (!NILP (h->weak))
	    {
	      strout (" weakness ", -1, -1, printcharfun);
	      print_object (h->weak, printcharfun, escapeflag);
	    }

	  if (!NILP (h->rehash_size))
	    {
	      strout (" rehash-size ", -1, -1, printcharfun);
	      print_object (h->rehash_size, printcharfun, escapeflag);
	    }

	  if (!NILP (h->rehash_threshold))
	    {
	      strout (" rehash-threshold ", -1, -1, printcharfun);
	      print_object (h->rehash_threshold, printcharfun, escapeflag);
	    }

	  strout (" data ", -1, -1, printcharfun);

	  /* Print the data here as a plist. */
	  real_size = HASH_TABLE_SIZE (h);
	  size = real_size;

	  /* Don't print more elements than the specified maximum.  */
	  if (NATNUMP (Vprint_length)
	      && XFASTINT (Vprint_length) < size)
	    size = XFASTINT (Vprint_length);

	  PRINTCHAR ('(');
	  for (i = 0; i < size; i++)
	    if (!NILP (HASH_HASH (h, i)))
	      {
		if (i) PRINTCHAR (' ');
		print_object (HASH_KEY (h, i), printcharfun, escapeflag);
		PRINTCHAR (' ');
		print_object (HASH_VALUE (h, i), printcharfun, escapeflag);
	      }

	  if (size < real_size)
	    strout (" ...", 4, 4, printcharfun);

	  PRINTCHAR (')');
	  PRINTCHAR (')');

	}
      else if (BUFFERP (obj))
	{
	  if (NILP (BVAR (XBUFFER (obj), name)))
	    strout ("#<killed buffer>", -1, -1, printcharfun);
	  else if (escapeflag)
	    {
	      strout ("#<buffer ", -1, -1, printcharfun);
	      print_string (BVAR (XBUFFER (obj), name), printcharfun);
	      PRINTCHAR ('>');
	    }
	  else
	    print_string (BVAR (XBUFFER (obj), name), printcharfun);
	}
      else if (WINDOW_CONFIGURATIONP (obj))
	{
	  strout ("#<window-configuration>", -1, -1, printcharfun);
	}
      else if (FRAMEP (obj))
	{
	  strout ((FRAME_LIVE_P (XFRAME (obj))
		   ? "#<frame " : "#<dead frame "),
		  -1, -1, printcharfun);
	  print_string (XFRAME (obj)->name, printcharfun);
	  sprintf (buf, " %p", XFRAME (obj));
	  strout (buf, -1, -1, printcharfun);
	  PRINTCHAR ('>');
	}
      else if (FONTP (obj))
	{
	  EMACS_INT i;

	  if (! FONT_OBJECT_P (obj))
	    {
	      if (FONT_SPEC_P (obj))
		strout ("#<font-spec", -1, -1, printcharfun);
	      else
		strout ("#<font-entity", -1, -1, printcharfun);
	      for (i = 0; i < FONT_SPEC_MAX; i++)
		{
		  PRINTCHAR (' ');
		  if (i < FONT_WEIGHT_INDEX || i > FONT_WIDTH_INDEX)
		    print_object (AREF (obj, i), printcharfun, escapeflag);
		  else
		    print_object (font_style_symbolic (obj, i, 0),
				  printcharfun, escapeflag);
		}
	    }
	  else
	    {
	      strout ("#<font-object ", -1, -1, printcharfun);
	      print_object (AREF (obj, FONT_NAME_INDEX), printcharfun,
			    escapeflag);
	    }
	  PRINTCHAR ('>');
	}
      else
	{
	  EMACS_INT size = ASIZE (obj);
	  if (COMPILEDP (obj))
	    {
	      PRINTCHAR ('#');
	      size &= PSEUDOVECTOR_SIZE_MASK;
	    }
	  if (CHAR_TABLE_P (obj) || SUB_CHAR_TABLE_P (obj))
	    {
	      /* We print a char-table as if it were a vector,
		 lumping the parent and default slots in with the
		 character slots.  But we add #^ as a prefix.  */

	      /* Make each lowest sub_char_table start a new line.
		 Otherwise we'll make a line extremely long, which
		 results in slow redisplay.  */
	      if (SUB_CHAR_TABLE_P (obj)
		  && XINT (XSUB_CHAR_TABLE (obj)->depth) == 3)
		PRINTCHAR ('\n');
	      PRINTCHAR ('#');
	      PRINTCHAR ('^');
	      if (SUB_CHAR_TABLE_P (obj))
		PRINTCHAR ('^');
	      size &= PSEUDOVECTOR_SIZE_MASK;
	    }
	  if (size & PSEUDOVECTOR_FLAG)
	    goto badtype;

	  PRINTCHAR ('[');
	  {
	    register int i;
	    register Lisp_Object tem;
	    EMACS_INT real_size = size;

	    /* Don't print more elements than the specified maximum.  */
	    if (NATNUMP (Vprint_length)
		&& XFASTINT (Vprint_length) < size)
	      size = XFASTINT (Vprint_length);

	    for (i = 0; i < size; i++)
	      {
		if (i) PRINTCHAR (' ');
		tem = XVECTOR (obj)->contents[i];
		print_object (tem, printcharfun, escapeflag);
	      }
	    if (size < real_size)
	      strout (" ...", 4, 4, printcharfun);
	  }
	  PRINTCHAR (']');
	}
      break;

    case Lisp_Misc:
      switch (XMISCTYPE (obj))
	{
	case Lisp_Misc_Marker:
	  strout ("#<marker ", -1, -1, printcharfun);
	  /* Do you think this is necessary?  */
	  if (XMARKER (obj)->insertion_type != 0)
	    strout ("(moves after insertion) ", -1, -1, printcharfun);
	  if (! XMARKER (obj)->buffer)
	    strout ("in no buffer", -1, -1, printcharfun);
	  else
	    {
	      sprintf (buf, "at %"pI"d", marker_position (obj));
	      strout (buf, -1, -1, printcharfun);
	      strout (" in ", -1, -1, printcharfun);
	      print_string (BVAR (XMARKER (obj)->buffer, name), printcharfun);
	    }
	  PRINTCHAR ('>');
	  break;

	case Lisp_Misc_Overlay:
	  strout ("#<overlay ", -1, -1, printcharfun);
	  if (! XMARKER (OVERLAY_START (obj))->buffer)
	    strout ("in no buffer", -1, -1, printcharfun);
	  else
	    {
	      sprintf (buf, "from %"pI"d to %"pI"d in ",
		       marker_position (OVERLAY_START (obj)),
		       marker_position (OVERLAY_END   (obj)));
	      strout (buf, -1, -1, printcharfun);
	      print_string (BVAR (XMARKER (OVERLAY_START (obj))->buffer, name),
			    printcharfun);
	    }
	  PRINTCHAR ('>');
	  break;

      /* Remaining cases shouldn't happen in normal usage, but let's print
	 them anyway for the benefit of the debugger.  */
	case Lisp_Misc_Free:
	  strout ("#<misc free cell>", -1, -1, printcharfun);
	  break;

	case Lisp_Misc_Save_Value:
	  strout ("#<save_value ", -1, -1, printcharfun);
	  sprintf (buf, "ptr=%p int=%"pD"d",
                   XSAVE_VALUE (obj)->pointer,
                   XSAVE_VALUE (obj)->integer);
	  strout (buf, -1, -1, printcharfun);
	  PRINTCHAR ('>');
	  break;

	default:
	  goto badtype;
	}
      break;

    default:
    badtype:
      {
	/* We're in trouble if this happens!
	   Probably should just abort () */
	strout ("#<EMACS BUG: INVALID DATATYPE ", -1, -1, printcharfun);
	if (MISCP (obj))
	  sprintf (buf, "(MISC 0x%04x)", (int) XMISCTYPE (obj));
	else if (VECTORLIKEP (obj))
	  sprintf (buf, "(PVEC 0x%08"pI"x)", ASIZE (obj));
	else
	  sprintf (buf, "(0x%02x)", (int) XTYPE (obj));
	strout (buf, -1, -1, printcharfun);
	strout (" Save your buffers immediately and please report this bug>",
		-1, -1, printcharfun);
      }
    }

  print_depth--;
}


/* Print a description of INTERVAL using PRINTCHARFUN.
   This is part of printing a string that has text properties.  */

void
print_interval (INTERVAL interval, Lisp_Object printcharfun)
{
  if (NILP (interval->plist))
    return;
  PRINTCHAR (' ');
  print_object (make_number (interval->position), printcharfun, 1);
  PRINTCHAR (' ');
  print_object (make_number (interval->position + LENGTH (interval)),
		printcharfun, 1);
  PRINTCHAR (' ');
  print_object (interval->plist, printcharfun, 1);
}


void
syms_of_print (void)
{
  DEFSYM (Qtemp_buffer_setup_hook, "temp-buffer-setup-hook");

  DEFVAR_LISP ("standard-output", Vstandard_output,
	       doc: /* Output stream `print' uses by default for outputting a character.
This may be any function of one argument.
It may also be a buffer (output is inserted before point)
or a marker (output is inserted and the marker is advanced)
or the symbol t (output appears in the echo area).  */);
  Vstandard_output = Qt;
  DEFSYM (Qstandard_output, "standard-output");

  DEFVAR_LISP ("float-output-format", Vfloat_output_format,
	       doc: /* The format descriptor string used to print floats.
This is a %-spec like those accepted by `printf' in C,
but with some restrictions.  It must start with the two characters `%.'.
After that comes an integer precision specification,
and then a letter which controls the format.
The letters allowed are `e', `f' and `g'.
Use `e' for exponential notation \"DIG.DIGITSeEXPT\"
Use `f' for decimal point notation \"DIGITS.DIGITS\".
Use `g' to choose the shorter of those two formats for the number at hand.
The precision in any of these cases is the number of digits following
the decimal point.  With `f', a precision of 0 means to omit the
decimal point.  0 is not allowed with `e' or `g'.

A value of nil means to use the shortest notation
that represents the number without losing information.  */);
  Vfloat_output_format = Qnil;
  DEFSYM (Qfloat_output_format, "float-output-format");

  DEFVAR_LISP ("print-length", Vprint_length,
	       doc: /* Maximum length of list to print before abbreviating.
A value of nil means no limit.  See also `eval-expression-print-length'.  */);
  Vprint_length = Qnil;

  DEFVAR_LISP ("print-level", Vprint_level,
	       doc: /* Maximum depth of list nesting to print before abbreviating.
A value of nil means no limit.  See also `eval-expression-print-level'.  */);
  Vprint_level = Qnil;

  DEFVAR_BOOL ("print-escape-newlines", print_escape_newlines,
	       doc: /* Non-nil means print newlines in strings as `\\n'.
Also print formfeeds as `\\f'.  */);
  print_escape_newlines = 0;

  DEFVAR_BOOL ("print-escape-nonascii", print_escape_nonascii,
	       doc: /* Non-nil means print unibyte non-ASCII chars in strings as \\OOO.
\(OOO is the octal representation of the character code.)
Only single-byte characters are affected, and only in `prin1'.
When the output goes in a multibyte buffer, this feature is
enabled regardless of the value of the variable.  */);
  print_escape_nonascii = 0;

  DEFVAR_BOOL ("print-escape-multibyte", print_escape_multibyte,
	       doc: /* Non-nil means print multibyte characters in strings as \\xXXXX.
\(XXXX is the hex representation of the character code.)
This affects only `prin1'.  */);
  print_escape_multibyte = 0;

  DEFVAR_BOOL ("print-quoted", print_quoted,
	       doc: /* Non-nil means print quoted forms with reader syntax.
I.e., (quote foo) prints as 'foo, (function foo) as #'foo.  */);
  print_quoted = 0;

  DEFVAR_LISP ("print-gensym", Vprint_gensym,
	       doc: /* Non-nil means print uninterned symbols so they will read as uninterned.
I.e., the value of (make-symbol \"foobar\") prints as #:foobar.
When the uninterned symbol appears within a recursive data structure,
and the symbol appears more than once, in addition use the #N# and #N=
constructs as needed, so that multiple references to the same symbol are
shared once again when the text is read back.  */);
  Vprint_gensym = Qnil;

  DEFVAR_LISP ("print-circle", Vprint_circle,
	       doc: /* *Non-nil means print recursive structures using #N= and #N# syntax.
If nil, printing proceeds recursively and may lead to
`max-lisp-eval-depth' being exceeded or an error may occur:
\"Apparently circular structure being printed.\"  Also see
`print-length' and `print-level'.
If non-nil, shared substructures anywhere in the structure are printed
with `#N=' before the first occurrence (in the order of the print
representation) and `#N#' in place of each subsequent occurrence,
where N is a positive decimal integer.  */);
  Vprint_circle = Qnil;

  DEFVAR_LISP ("print-continuous-numbering", Vprint_continuous_numbering,
	       doc: /* *Non-nil means number continuously across print calls.
This affects the numbers printed for #N= labels and #M# references.
See also `print-circle', `print-gensym', and `print-number-table'.
This variable should not be set with `setq'; bind it with a `let' instead.  */);
  Vprint_continuous_numbering = Qnil;

  DEFVAR_LISP ("print-number-table", Vprint_number_table,
	       doc: /* A vector used internally to produce `#N=' labels and `#N#' references.
The Lisp printer uses this vector to detect Lisp objects referenced more
than once.

When you bind `print-continuous-numbering' to t, you should probably
also bind `print-number-table' to nil.  This ensures that the value of
`print-number-table' can be garbage-collected once the printing is
done.  If all elements of `print-number-table' are nil, it means that
the printing done so far has not found any shared structure or objects
that need to be recorded in the table.  */);
  Vprint_number_table = Qnil;

  DEFVAR_LISP ("print-charset-text-property", Vprint_charset_text_property,
	       doc: /* A flag to control printing of `charset' text property on printing a string.
The value must be nil, t, or `default'.

If the value is nil, don't print the text property `charset'.

If the value is t, always print the text property `charset'.

If the value is `default', print the text property `charset' only when
the value is different from what is guessed in the current charset
priorities.  */);
  Vprint_charset_text_property = Qdefault;

  /* prin1_to_string_buffer initialized in init_buffer_once in buffer.c */
  staticpro (&Vprin1_to_string_buffer);

  defsubr (&Sprin1);
  defsubr (&Sprin1_to_string);
  defsubr (&Serror_message_string);
  defsubr (&Sprinc);
  defsubr (&Sprint);
  defsubr (&Sterpri);
  defsubr (&Swrite_char);
  defsubr (&Sexternal_debugging_output);
#ifdef WITH_REDIRECT_DEBUGGING_OUTPUT
  defsubr (&Sredirect_debugging_output);
#endif

  DEFSYM (Qexternal_debugging_output, "external-debugging-output");
  DEFSYM (Qprint_escape_newlines, "print-escape-newlines");
  DEFSYM (Qprint_escape_multibyte, "print-escape-multibyte");
  DEFSYM (Qprint_escape_nonascii, "print-escape-nonascii");

  print_prune_charset_plist = Qnil;
  staticpro (&print_prune_charset_plist);
}
