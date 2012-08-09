/* Record indices of function doc strings stored in a file.
   Copyright (C) 1985-1986, 1993-1995, 1997-2012
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

#include <sys/types.h>
#include <sys/file.h>	/* Must be after sys/types.h for USG*/
#include <ctype.h>
#include <setjmp.h>
#include <fcntl.h>
#include <unistd.h>

#include "lisp.h"
#include "buffer.h"
#include "keyboard.h"
#include "character.h"
#include "keymap.h"
#include "buildobj.h"

Lisp_Object Qfunction_documentation;

extern Lisp_Object Qclosure;
/* Buffer used for reading from documentation file.  */
static char *get_doc_string_buffer;
static ptrdiff_t get_doc_string_buffer_size;

static unsigned char *read_bytecode_pointer;
static Lisp_Object Fdocumentation_property (Lisp_Object, Lisp_Object,
					    Lisp_Object);
static Lisp_Object Fsnarf_documentation (Lisp_Object);

/* readchar in lread.c calls back here to fetch the next byte.
   If UNREADFLAG is 1, we unread a byte.  */

int
read_bytecode_char (int unreadflag)
{
  if (unreadflag)
    {
      read_bytecode_pointer--;
      return 0;
    }
  return *read_bytecode_pointer++;
}

/* Extract a doc string from a file.  FILEPOS says where to get it.
   If it is an integer, use that position in the standard DOC-... file.
   If it is (FILE . INTEGER), use FILE as the file name
   and INTEGER as the position in that file.
   But if INTEGER is negative, make it positive.
   (A negative integer is used for user variables, so we can distinguish
   them without actually fetching the doc string.)

   If the location does not point to the beginning of a docstring
   (e.g. because the file has been modified and the location is stale),
   return nil.

   If UNIBYTE is nonzero, always make a unibyte string.

   If DEFINITION is nonzero, assume this is for reading
   a dynamic function definition; convert the bytestring
   and the constants vector with appropriate byte handling,
   and return a cons cell.  */

Lisp_Object
get_doc_string (Lisp_Object filepos, int unibyte, int definition)
{
  char *from, *to;
  register int fd;
  register char *name;
  register char *p, *p1;
  EMACS_INT minsize;
  EMACS_INT offset, position;
  Lisp_Object file, tem;

  if (INTEGERP (filepos))
    {
      file = Vdoc_file_name;
      position = XINT (filepos);
    }
  else if (CONSP (filepos))
    {
      file = XCAR (filepos);
      position = XINT (XCDR (filepos));
    }
  else
    return Qnil;

  if (position < 0)
    position = - position;

  if (!STRINGP (Vdoc_directory))
    return Qnil;

  if (!STRINGP (file))
    return Qnil;

  /* Put the file name in NAME as a C string.
     If it is relative, combine it with Vdoc_directory.  */

  tem = Ffile_name_absolute_p (file);
  file = ENCODE_FILE (file);
  if (NILP (tem))
    {
      Lisp_Object docdir = ENCODE_FILE (Vdoc_directory);
      minsize = SCHARS (docdir);
      /* sizeof ("../etc/") == 8 */
      if (minsize < 8)
	minsize = 8;
      name = (char *) alloca (minsize + SCHARS (file) + 8);
      strcpy (name, SSDATA (docdir));
      strcat (name, SSDATA (file));
    }
  else
    {
      name = SSDATA (file);
    }

  fd = emacs_open (name, O_RDONLY, 0);
  if (fd < 0)
    {
#ifndef CANNOT_DUMP
      if (!NILP (Vpurify_flag))
	{
	  /* Preparing to dump; DOC file is probably not installed.
	     So check in ../etc.  */
	  strcpy (name, "../etc/");
	  strcat (name, SSDATA (file));

	  fd = emacs_open (name, O_RDONLY, 0);
	}
#endif
      if (fd < 0)
	error ("Cannot open doc string file \"%s\"", name);
    }

  /* Seek only to beginning of disk block.  */
  /* Make sure we read at least 1024 bytes before `position'
     so we can check the leading text for consistency.  */
  offset = min (position, max (1024, position % (8 * 1024)));
  if (0 > lseek (fd, position - offset, 0))
    {
      emacs_close (fd);
      error ("Position %"pI"d out of range in doc string file \"%s\"",
	     position, name);
    }

  /* Read the doc string into get_doc_string_buffer.
     P points beyond the data just read.  */

  p = get_doc_string_buffer;
  while (1)
    {
      ptrdiff_t space_left = (get_doc_string_buffer_size - 1
			      - (p - get_doc_string_buffer));
      int nread;

      /* Allocate or grow the buffer if we need to.  */
      if (space_left <= 0)
	{
	  ptrdiff_t in_buffer = p - get_doc_string_buffer;
	  get_doc_string_buffer =
	    xpalloc (get_doc_string_buffer, &get_doc_string_buffer_size,
		     16 * 1024, -1, 1);
	  p = get_doc_string_buffer + in_buffer;
	  space_left = (get_doc_string_buffer_size - 1
			- (p - get_doc_string_buffer));
	}

      /* Read a disk block at a time.
         If we read the same block last time, maybe skip this?  */
      if (space_left > 1024 * 8)
	space_left = 1024 * 8;
      nread = emacs_read (fd, p, space_left);
      if (nread < 0)
	{
	  emacs_close (fd);
	  error ("Read error on documentation file");
	}
      p[nread] = 0;
      if (!nread)
	break;
      if (p == get_doc_string_buffer)
	p1 = strchr (p + offset, '\037');
      else
	p1 = strchr (p, '\037');
      if (p1)
	{
	  *p1 = 0;
	  p = p1;
	  break;
	}
      p += nread;
    }
  emacs_close (fd);

  /* Sanity checking.  */
  if (CONSP (filepos))
    {
      int test = 1;
      if (get_doc_string_buffer[offset - test++] != ' ')
	return Qnil;
      while (get_doc_string_buffer[offset - test] >= '0'
	     && get_doc_string_buffer[offset - test] <= '9')
	test++;
      if (get_doc_string_buffer[offset - test++] != '@'
	  || get_doc_string_buffer[offset - test] != '#')
	return Qnil;
    }
  else
    {
      int test = 1;
      if (get_doc_string_buffer[offset - test++] != '\n')
	return Qnil;
      while (get_doc_string_buffer[offset - test] > ' ')
	test++;
      if (get_doc_string_buffer[offset - test] != '\037')
	return Qnil;
    }

  /* Scan the text and perform quoting with ^A (char code 1).
     ^A^A becomes ^A, ^A0 becomes a null char, and ^A_ becomes a ^_.  */
  from = get_doc_string_buffer + offset;
  to = get_doc_string_buffer + offset;
  while (from != p)
    {
      if (*from == 1)
	{
	  int c;

	  from++;
	  c = *from++;
	  if (c == 1)
	    *to++ = c;
	  else if (c == '0')
	    *to++ = 0;
	  else if (c == '_')
	    *to++ = 037;
	  else
	    {
	      unsigned char uc = c;
	      error ("\
Invalid data in documentation file -- %c followed by code %03o",
		     1, uc);
	    }
	}
      else
	*to++ = *from++;
    }

  /* If DEFINITION, read from this buffer
     the same way we would read bytes from a file.  */
  if (definition)
    {
      read_bytecode_pointer = (unsigned char *) get_doc_string_buffer + offset;
      return Fread (Qlambda);
    }

  if (unibyte)
    return make_unibyte_string (get_doc_string_buffer + offset,
				to - (get_doc_string_buffer + offset));
  else
    {
      /* The data determines whether the string is multibyte.  */
      EMACS_INT nchars =
	multibyte_chars_in_text (((unsigned char *) get_doc_string_buffer
				  + offset),
				 to - (get_doc_string_buffer + offset));
      return make_string_from_bytes (get_doc_string_buffer + offset,
				     nchars,
				     to - (get_doc_string_buffer + offset));
    }
}

/* Get a string from position FILEPOS and pass it through the Lisp reader.
   We use this for fetching the bytecode string and constants vector
   of a compiled function from the .elc file.  */

Lisp_Object
read_doc_string (Lisp_Object filepos)
{
  return get_doc_string (filepos, 0, 1);
}

static int
reread_doc_file (Lisp_Object file)
{
#if 0
  Lisp_Object reply, prompt[3];
  struct gcpro gcpro1;
  GCPRO1 (file);
  prompt[0] = build_string ("File ");
  prompt[1] = NILP (file) ? Vdoc_file_name : file;
  prompt[2] = build_string (" is out of sync.  Reload? ");
  reply = Fy_or_n_p (Fconcat (3, prompt));
  UNGCPRO;
  if (NILP (reply))
    return 0;
#endif

  if (NILP (file))
    Fsnarf_documentation (Vdoc_file_name);
  else
    Fload (file, Qt, Qt, Qt, Qnil);

  return 1;
}

DEFUN ("documentation", Fdocumentation, Sdocumentation, 1, 2, 0,
       doc: /* Return the documentation string of FUNCTION.
Unless a non-nil second argument RAW is given, the
string is passed through `substitute-command-keys'.  */)
  (Lisp_Object function, Lisp_Object raw)
{
  Lisp_Object fun;
  Lisp_Object funcar;
  Lisp_Object doc;
  int try_reload = 1;

 documentation:

  doc = Qnil;

  if (SYMBOLP (function))
    {
      Lisp_Object tem = Fget (function, Qfunction_documentation);
      if (!NILP (tem))
	return Fdocumentation_property (function, Qfunction_documentation,
					raw);
    }

  fun = Findirect_function (function, Qnil);
  if (SUBRP (fun))
    {
      if (XSUBR (fun)->doc == 0)
	return Qnil;
      /* FIXME: This is not portable, as it assumes that string
	 pointers have the top bit clear.  */
      else if ((intptr_t) XSUBR (fun)->doc >= 0)
	doc = build_string (XSUBR (fun)->doc);
      else
	doc = make_number ((intptr_t) XSUBR (fun)->doc);
    }
  else if (COMPILEDP (fun))
    {
      if ((ASIZE (fun) & PSEUDOVECTOR_SIZE_MASK) <= COMPILED_DOC_STRING)
	return Qnil;
      else
	{
	  Lisp_Object tem = AREF (fun, COMPILED_DOC_STRING);
	  if (STRINGP (tem))
	    doc = tem;
	  else if (NATNUMP (tem) || CONSP (tem))
	    doc = tem;
	  else
	    return Qnil;
	}
    }
  else if (STRINGP (fun) || VECTORP (fun))
    {
      return build_string ("Keyboard macro.");
    }
  else if (CONSP (fun))
    {
      funcar = Fcar (fun);
      if (!SYMBOLP (funcar))
	xsignal1 (Qinvalid_function, fun);
      else if (EQ (funcar, Qkeymap))
	return build_string ("Prefix command (definition is a keymap associating keystrokes with commands).");
      else if (EQ (funcar, Qlambda)
	       || (EQ (funcar, Qclosure) && (fun = XCDR (fun), 1))
	       || EQ (funcar, Qautoload))
	{
	  Lisp_Object tem1 = Fcdr (Fcdr (fun));
	  Lisp_Object tem = Fcar (tem1);
	  if (STRINGP (tem))
	    doc = tem;
	  /* Handle a doc reference--but these never come last
	     in the function body, so reject them if they are last.  */
	  else if ((NATNUMP (tem) || (CONSP (tem) && INTEGERP (XCDR (tem))))
		   && !NILP (XCDR (tem1)))
	    doc = tem;
	  else
	    return Qnil;
	}
      else if (EQ (funcar, Qmacro))
	return Fdocumentation (Fcdr (fun), raw);
      else
	goto oops;
    }
  else
    {
    oops:
      xsignal1 (Qinvalid_function, fun);
    }

  /* Check for an advised function.  Its doc string
     has an `ad-advice-info' text property.  */
  if (STRINGP (doc))
    {
      Lisp_Object innerfunc;
      innerfunc = Fget_text_property (make_number (0),
				      intern ("ad-advice-info"),
				      doc);
      if (! NILP (innerfunc))
	doc = call1 (intern ("ad-make-advised-docstring"), innerfunc);
    }

  /* If DOC is 0, it's typically because of a dumped file missing
     from the DOC file (bug in src/Makefile.in).  */
  if (EQ (doc, make_number (0)))
    doc = Qnil;
  if (INTEGERP (doc) || CONSP (doc))
    {
      Lisp_Object tem;
      tem = get_doc_string (doc, 0, 0);
      if (NILP (tem) && try_reload)
	{
	  /* The file is newer, we need to reset the pointers.  */
	  struct gcpro gcpro1, gcpro2;
	  GCPRO2 (function, raw);
	  try_reload = reread_doc_file (Fcar_safe (doc));
	  UNGCPRO;
	  if (try_reload)
	    {
	      try_reload = 0;
	      goto documentation;
	    }
	}
      else
	doc = tem;
    }

  if (NILP (raw))
    doc = Fsubstitute_command_keys (doc);
  return doc;
}

DEFUN ("documentation-property", Fdocumentation_property,
       Sdocumentation_property, 2, 3, 0,
       doc: /* Return the documentation string that is SYMBOL's PROP property.
Third argument RAW omitted or nil means pass the result through
`substitute-command-keys' if it is a string.

This differs from `get' in that it can refer to strings stored in the
`etc/DOC' file; and that it evaluates documentation properties that
aren't strings.  */)
  (Lisp_Object symbol, Lisp_Object prop, Lisp_Object raw)
{
  int try_reload = 1;
  Lisp_Object tem;

 documentation_property:

  tem = Fget (symbol, prop);
  if (EQ (tem, make_number (0)))
    tem = Qnil;
  if (INTEGERP (tem) || (CONSP (tem) && INTEGERP (XCDR (tem))))
    {
      Lisp_Object doc = tem;
      tem = get_doc_string (tem, 0, 0);
      if (NILP (tem) && try_reload)
	{
	  /* The file is newer, we need to reset the pointers.  */
	  struct gcpro gcpro1, gcpro2, gcpro3;
	  GCPRO3 (symbol, prop, raw);
	  try_reload = reread_doc_file (Fcar_safe (doc));
	  UNGCPRO;
	  if (try_reload)
	    {
	      try_reload = 0;
	      goto documentation_property;
	    }
	}
    }
  else if (!STRINGP (tem))
    /* Feval protects its argument.  */
    tem = Feval (tem, Qnil);

  if (NILP (raw) && STRINGP (tem))
    tem = Fsubstitute_command_keys (tem);
  return tem;
}

/* Scanning the DOC files and placing docstring offsets into functions.  */

static void
store_function_docstring (Lisp_Object obj, EMACS_INT offset)
/* Use EMACS_INT because we get offset from pointer subtraction.  */
{
  /* Don't use indirect_function here, or defaliases will apply their
     docstrings to the base functions (Bug#2603).  */
  Lisp_Object fun = SYMBOLP (obj) ? XSYMBOL (obj)->function : obj;

  /* The type determines where the docstring is stored.  */

  /* Lisp_Subrs have a slot for it.  */
  if (SUBRP (fun))
    {
      intptr_t negative_offset = - offset;
      XSUBR (fun)->doc = (char *) negative_offset;
    }

  /* If it's a lisp form, stick it in the form.  */
  else if (CONSP (fun))
    {
      Lisp_Object tem;

      tem = XCAR (fun);
      if (EQ (tem, Qlambda) || EQ (tem, Qautoload)
	  || (EQ (tem, Qclosure) && (fun = XCDR (fun), 1)))
	{
	  tem = Fcdr (Fcdr (fun));
	  if (CONSP (tem) && INTEGERP (XCAR (tem)))
	    XSETCAR (tem, make_number (offset));
	}
      else if (EQ (tem, Qmacro))
	store_function_docstring (XCDR (fun), offset);
    }

  /* Bytecode objects sometimes have slots for it.  */
  else if (COMPILEDP (fun))
    {
      /* This bytecode object must have a slot for the
	 docstring, since we've found a docstring for it.  */
      if ((ASIZE (fun) & PSEUDOVECTOR_SIZE_MASK) > COMPILED_DOC_STRING)
	ASET (fun, COMPILED_DOC_STRING, make_number (offset));
    }
}

static const char buildobj[] = BUILDOBJ;

DEFUN ("Snarf-documentation", Fsnarf_documentation, Ssnarf_documentation,
       1, 1, 0,
       doc: /* Used during Emacs initialization to scan the `etc/DOC...' file.
This searches the `etc/DOC...' file for doc strings and
records them in function and variable definitions.
The function takes one argument, FILENAME, a string;
it specifies the file name (without a directory) of the DOC file.
That file is found in `../etc' now; later, when the dumped Emacs is run,
the same file name is found in the `doc-directory'.  */)
  (Lisp_Object filename)
{
  int fd;
  char buf[1024 + 1];
  register EMACS_INT filled;
  register EMACS_INT pos;
  register char *p;
  Lisp_Object sym;
  char *name;
  int skip_file = 0;

  CHECK_STRING (filename);

  if
#ifndef CANNOT_DUMP
    (!NILP (Vpurify_flag))
#else /* CANNOT_DUMP */
      (0)
#endif /* CANNOT_DUMP */
    {
      name = (char *) alloca (SCHARS (filename) + 14);
      strcpy (name, "../etc/");
    }
  else
    {
      CHECK_STRING (Vdoc_directory);
      name = (char *) alloca (SCHARS (filename)
			  + SCHARS (Vdoc_directory) + 1);
      strcpy (name, SSDATA (Vdoc_directory));
    }
  strcat (name, SSDATA (filename)); 	/*** Add this line ***/

  /* Vbuild_files is nil when temacs is run, and non-nil after that.  */
  if (NILP (Vbuild_files))
  {
    const char *beg, *end;

    for (beg = buildobj; *beg; beg = end)
      {
        EMACS_INT len;

        while (*beg && isspace (*beg)) ++beg;

        for (end = beg; *end && ! isspace (*end); ++end)
          if (*end == '/') beg = end+1;  /* skip directory part  */

        len = end - beg;
        if (len > 4 && end[-4] == '.' && end[-3] == 'o')
          len -= 2;  /* Just take .o if it ends in .obj  */

        if (len > 0)
          Vbuild_files = Fcons (make_string (beg, len), Vbuild_files);
      }
    Vbuild_files = Fpurecopy (Vbuild_files);
  }

  fd = emacs_open (name, O_RDONLY, 0);
  if (fd < 0)
    report_file_error ("Opening doc string file",
		       Fcons (build_string (name), Qnil));
  Vdoc_file_name = filename;
  filled = 0;
  pos = 0;
  while (1)
    {
      register char *end;
      if (filled < 512)
	filled += emacs_read (fd, &buf[filled], sizeof buf - 1 - filled);
      if (!filled)
	break;

      buf[filled] = 0;
      p = buf;
      end = buf + (filled < 512 ? filled : filled - 128);
      while (p != end && *p != '\037') p++;
      /* p points to ^_Ffunctionname\n or ^_Vvarname\n or ^_Sfilename\n.  */
      if (p != end)
	{
	  end = strchr (p, '\n');

          /* See if this is a file name, and if it is a file in build-files.  */
          if (p[1] == 'S')
            {
              skip_file = 0;
              if (end - p > 4 && end[-2] == '.'
                  && (end[-1] == 'o' || end[-1] == 'c'))
                {
                  EMACS_INT len = end - p - 2;
                  char *fromfile = alloca (len + 1);
                  strncpy (fromfile, &p[2], len);
                  fromfile[len] = 0;
                  if (fromfile[len-1] == 'c')
                    fromfile[len-1] = 'o';

                  skip_file = NILP (Fmember (build_string (fromfile),
                                             Vbuild_files));
                }
            }

	  sym = oblookup (Vobarray, p + 2,
			  multibyte_chars_in_text ((unsigned char *) p + 2,
						   end - p - 2),
			  end - p - 2);
	  /* Check skip_file so that when a function is defined several
	     times in different files (typically, once in xterm, once in
	     w32term, ...), we only pay attention to the one that
	     matters.  */
	  if (! skip_file && SYMBOLP (sym))
	    {
	      /* Attach a docstring to a variable?  */
	      if (p[1] == 'V')
		{
		  /* Install file-position as variable-documentation property
		     and make it negative for a user-variable
		     (doc starts with a `*').  */
		  Fput (sym, Qvariable_documentation,
			make_number ((pos + end + 1 - buf)
				     * (end[1] == '*' ? -1 : 1)));
		}

	      /* Attach a docstring to a function?  */
	      else if (p[1] == 'F')
		store_function_docstring (sym, pos + end + 1 - buf);

	      else if (p[1] == 'S')
		; /* Just a source file name boundary marker.  Ignore it.  */

	      else
		error ("DOC file invalid at position %"pI"d", pos);
	    }
	}
      pos += end - buf;
      filled -= end - buf;
      memmove (buf, end, filled);
    }
  emacs_close (fd);
  return Qnil;
}

DEFUN ("substitute-command-keys", Fsubstitute_command_keys,
       Ssubstitute_command_keys, 1, 1, 0,
       doc: /* Substitute key descriptions for command names in STRING.
Each substring of the form \\=\\[COMMAND] is replaced by either a
keystroke sequence that invokes COMMAND, or "M-x COMMAND" if COMMAND
is not on any keys.

Each substring of the form \\=\\{MAPVAR} is replaced by a summary of
the value of MAPVAR as a keymap.  This summary is similar to the one
produced by `describe-bindings'.  The summary ends in two newlines
\(used by the helper function `help-make-xrefs' to find the end of the
summary).

Each substring of the form \\=\\<MAPVAR> specifies the use of MAPVAR
as the keymap for future \\=\\[COMMAND] substrings.
\\=\\= quotes the following character and is discarded;
thus, \\=\\=\\=\\= puts \\=\\= into the output, and \\=\\=\\=\\[ puts \\=\\[ into the output.

Return the original STRING if no substitutions are made.
Otherwise, return a new string, without any text properties.  */)
  (Lisp_Object string)
{
  char *buf;
  int changed = 0;
  register unsigned char *strp;
  register char *bufp;
  ptrdiff_t idx;
  ptrdiff_t bsize;
  Lisp_Object tem;
  Lisp_Object keymap;
  unsigned char *start;
  ptrdiff_t length, length_byte;
  Lisp_Object name;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;
  int multibyte;
  ptrdiff_t nchars;

  if (NILP (string))
    return Qnil;

  CHECK_STRING (string);
  tem = Qnil;
  keymap = Qnil;
  name = Qnil;
  GCPRO4 (string, tem, keymap, name);

  multibyte = STRING_MULTIBYTE (string);
  nchars = 0;

  /* KEYMAP is either nil (which means search all the active keymaps)
     or a specified local map (which means search just that and the
     global map).  If non-nil, it might come from Voverriding_local_map,
     or from a \\<mapname> construct in STRING itself..  */
  keymap = KVAR (current_kboard, Voverriding_terminal_local_map);
  if (NILP (keymap))
    keymap = Voverriding_local_map;

  bsize = SBYTES (string);
  bufp = buf = (char *) xmalloc (bsize);

  strp = SDATA (string);
  while (strp < SDATA (string) + SBYTES (string))
    {
      if (strp[0] == '\\' && strp[1] == '=')
	{
	  /* \= quotes the next character;
	     thus, to put in \[ without its special meaning, use \=\[.  */
	  changed = 1;
	  strp += 2;
	  if (multibyte)
	    {
	      int len;

	      STRING_CHAR_AND_LENGTH (strp, len);
	      if (len == 1)
		*bufp = *strp;
	      else
		memcpy (bufp, strp, len);
	      strp += len;
	      bufp += len;
	      nchars++;
	    }
	  else
	    *bufp++ = *strp++, nchars++;
	}
      else if (strp[0] == '\\' && strp[1] == '[')
	{
	  ptrdiff_t start_idx;
	  int follow_remap = 1;

	  changed = 1;
	  strp += 2;		/* skip \[ */
	  start = strp;
	  start_idx = start - SDATA (string);

	  while ((strp - SDATA (string)
		  < SBYTES (string))
		 && *strp != ']')
	    strp++;
	  length_byte = strp - start;

	  strp++;		/* skip ] */

	  /* Save STRP in IDX.  */
	  idx = strp - SDATA (string);
	  name = Fintern (make_string ((char *) start, length_byte), Qnil);

	do_remap:
	  tem = Fwhere_is_internal (name, keymap, Qt, Qnil, Qnil);

	  if (VECTORP (tem) && ASIZE (tem) > 1
	      && EQ (AREF (tem, 0), Qremap) && SYMBOLP (AREF (tem, 1))
	      && follow_remap)
	    {
	      name = AREF (tem, 1);
	      follow_remap = 0;
	      goto do_remap;
	    }

	  /* Note the Fwhere_is_internal can GC, so we have to take
	     relocation of string contents into account.  */
	  strp = SDATA (string) + idx;
	  start = SDATA (string) + start_idx;

	  if (NILP (tem))	/* but not on any keys */
	    {
	      ptrdiff_t offset = bufp - buf;
	      if (STRING_BYTES_BOUND - 4 < bsize)
		string_overflow ();
	      buf = (char *) xrealloc (buf, bsize += 4);
	      bufp = buf + offset;
	      memcpy (bufp, "M-x ", 4);
	      bufp += 4;
	      nchars += 4;
	      if (multibyte)
		length = multibyte_chars_in_text (start, length_byte);
	      else
		length = length_byte;
	      goto subst;
	    }
	  else
	    {			/* function is on a key */
	      tem = Fkey_description (tem, Qnil);
	      goto subst_string;
	    }
	}
      /* \{foo} is replaced with a summary of the keymap (symbol-value foo).
	 \<foo> just sets the keymap used for \[cmd].  */
      else if (strp[0] == '\\' && (strp[1] == '{' || strp[1] == '<'))
	{
	  struct buffer *oldbuf;
	  ptrdiff_t start_idx;
	  /* This is for computing the SHADOWS arg for describe_map_tree.  */
	  Lisp_Object active_maps = Fcurrent_active_maps (Qnil, Qnil);
	  Lisp_Object earlier_maps;

	  changed = 1;
	  strp += 2;		/* skip \{ or \< */
	  start = strp;
	  start_idx = start - SDATA (string);

	  while ((strp - SDATA (string) < SBYTES (string))
		 && *strp != '}' && *strp != '>')
	    strp++;

	  length_byte = strp - start;
	  strp++;			/* skip } or > */

	  /* Save STRP in IDX.  */
	  idx = strp - SDATA (string);

	  /* Get the value of the keymap in TEM, or nil if undefined.
	     Do this while still in the user's current buffer
	     in case it is a local variable.  */
	  name = Fintern (make_string ((char *) start, length_byte), Qnil);
	  tem = Fboundp (name);
	  if (! NILP (tem))
	    {
	      tem = Fsymbol_value (name);
	      if (! NILP (tem))
		{
		  tem = get_keymap (tem, 0, 1);
		  /* Note that get_keymap can GC.  */
		  strp = SDATA (string) + idx;
		  start = SDATA (string) + start_idx;
		}
	    }

	  /* Now switch to a temp buffer.  */
	  oldbuf = current_buffer;
	  set_buffer_internal (XBUFFER (Vprin1_to_string_buffer));

	  if (NILP (tem))
	    {
	      name = Fsymbol_name (name);
	      insert_string ("\nUses keymap \"");
	      insert_from_string (name, 0, 0,
				  SCHARS (name),
				  SBYTES (name), 1);
	      insert_string ("\", which is not currently defined.\n");
	      if (start[-1] == '<') keymap = Qnil;
	    }
	  else if (start[-1] == '<')
	    keymap = tem;
	  else
	    {
	      /* Get the list of active keymaps that precede this one.
		 If this one's not active, get nil.  */
	      earlier_maps = Fcdr (Fmemq (tem, Freverse (active_maps)));
	      describe_map_tree (tem, 1, Fnreverse (earlier_maps),
				 Qnil, (char *)0, 1, 0, 0, 1);
	    }
	  tem = Fbuffer_string ();
	  Ferase_buffer ();
	  set_buffer_internal (oldbuf);

	subst_string:
	  start = SDATA (tem);
	  length = SCHARS (tem);
	  length_byte = SBYTES (tem);
	subst:
	  {
	    ptrdiff_t offset = bufp - buf;
	    if (STRING_BYTES_BOUND - length_byte < bsize)
	      string_overflow ();
	    buf = (char *) xrealloc (buf, bsize += length_byte);
	    bufp = buf + offset;
	    memcpy (bufp, start, length_byte);
	    bufp += length_byte;
	    nchars += length;
	    /* Check STRING again in case gc relocated it.  */
	    strp = SDATA (string) + idx;
	  }
	}
      else if (! multibyte)		/* just copy other chars */
	*bufp++ = *strp++, nchars++;
      else
	{
	  int len;

	  STRING_CHAR_AND_LENGTH (strp, len);
	  if (len == 1)
	    *bufp = *strp;
	  else
	    memcpy (bufp, strp, len);
	  strp += len;
	  bufp += len;
	  nchars++;
	}
    }

  if (changed)			/* don't bother if nothing substituted */
    tem = make_string_from_bytes (buf, nchars, bufp - buf);
  else
    tem = string;
  xfree (buf);
  RETURN_UNGCPRO (tem);
}

void
syms_of_doc (void)
{
  DEFSYM (Qfunction_documentation, "function-documentation");

  DEFVAR_LISP ("internal-doc-file-name", Vdoc_file_name,
	       doc: /* Name of file containing documentation strings of built-in symbols.  */);
  Vdoc_file_name = Qnil;

  DEFVAR_LISP ("build-files", Vbuild_files,
               doc: /* A list of files used to build this Emacs binary.  */);
  Vbuild_files = Qnil;

  defsubr (&Sdocumentation);
  defsubr (&Sdocumentation_property);
  defsubr (&Ssnarf_documentation);
  defsubr (&Ssubstitute_command_keys);
}
