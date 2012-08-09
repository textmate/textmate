/* Generate doc-string file for GNU Emacs from source files.
   Copyright (C) 1985-1986, 1992-1994, 1997, 1999-2012
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


/* The arguments given to this program are all the C and Lisp source files
 of GNU Emacs.  .elc and .el and .c files are allowed.
 A .o file can also be specified; the .c file it was made from is used.
 This helps the makefile pass the correct list of files.
 Option -d DIR means change to DIR before looking for files.

 The results, which go to standard output or to a file
 specified with -a or -o (-a to append, -o to start from nothing),
 are entries containing function or variable names and their documentation.
 Each entry starts with a ^_ character.
 Then comes F for a function or V for a variable.
 Then comes the function or variable name, terminated with a newline.
 Then comes the documentation for that function or variable.
 */

#include <config.h>

/* defined to be emacs_main, sys_fopen, etc. in config.h */
#undef main
#undef fopen
#undef chdir

#include <stdio.h>
#include <stdlib.h>
#ifdef MSDOS
#include <fcntl.h>
#endif /* MSDOS */
#ifdef WINDOWSNT
#include <fcntl.h>
#include <direct.h>
#endif /* WINDOWSNT */

#ifdef DOS_NT
#define READ_TEXT "rt"
#define READ_BINARY "rb"
#else  /* not DOS_NT */
#define READ_TEXT "r"
#define READ_BINARY "r"
#endif /* not DOS_NT */

#ifndef DIRECTORY_SEP
#define DIRECTORY_SEP '/'
#endif

#ifndef IS_DIRECTORY_SEP
#define IS_DIRECTORY_SEP(_c_) ((_c_) == DIRECTORY_SEP)
#endif

/* Use this to suppress gcc's `...may be used before initialized' warnings. */
#ifdef lint
# define IF_LINT(Code) Code
#else
# define IF_LINT(Code) /* empty */
#endif

static int scan_file (char *filename);
static int scan_lisp_file (const char *filename, const char *mode);
static int scan_c_file (char *filename, const char *mode);
static void fatal (const char *s1, const char *s2) NO_RETURN;
static void start_globals (void);
static void write_globals (void);

#ifdef MSDOS
/* s/msdos.h defines this as sys_chdir, but we're not linking with the
   file where that function is defined.  */
#undef chdir
#endif

#include <unistd.h>

/* Stdio stream for output to the DOC file.  */
FILE *outfile;

/* Name this program was invoked with.  */
char *progname;

/* Nonzero if this invocation is generating globals.h.  */
int generate_globals;

/* Print error message.  `s1' is printf control string, `s2' is arg for it.  */

/* VARARGS1 */
static void
error (const char *s1, const char *s2)
{
  fprintf (stderr, "%s: ", progname);
  fprintf (stderr, s1, s2);
  fprintf (stderr, "\n");
}

/* Print error message and exit.  */

/* VARARGS1 */
static void
fatal (const char *s1, const char *s2)
{
  error (s1, s2);
  exit (EXIT_FAILURE);
}

/* Like malloc but get fatal error if memory is exhausted.  */

static void *
xmalloc (unsigned int size)
{
  void *result = (void *) malloc (size);
  if (result == NULL)
    fatal ("virtual memory exhausted", 0);
  return result;
}

/* Like realloc but get fatal error if memory is exhausted.  */

static void *
xrealloc (void *arg, unsigned int size)
{
  void *result = (void *) realloc (arg, size);
  if (result == NULL)
    fatal ("virtual memory exhausted", 0);
  return result;
}


int
main (int argc, char **argv)
{
  int i;
  int err_count = 0;
  int first_infile;

  progname = argv[0];

  outfile = stdout;

  /* Don't put CRs in the DOC file.  */
#ifdef MSDOS
  _fmode = O_BINARY;
#if 0  /* Suspicion is that this causes hanging.
	  So instead we require people to use -o on MSDOS.  */
  (stdout)->_flag &= ~_IOTEXT;
  _setmode (fileno (stdout), O_BINARY);
#endif
  outfile = 0;
#endif /* MSDOS */
#ifdef WINDOWSNT
  _fmode = O_BINARY;
  _setmode (fileno (stdout), O_BINARY);
#endif /* WINDOWSNT */

  /* If first two args are -o FILE, output to FILE.  */
  i = 1;
  if (argc > i + 1 && !strcmp (argv[i], "-o"))
    {
      outfile = fopen (argv[i + 1], "w");
      i += 2;
    }
  if (argc > i + 1 && !strcmp (argv[i], "-a"))
    {
      outfile = fopen (argv[i + 1], "a");
      i += 2;
    }
  if (argc > i + 1 && !strcmp (argv[i], "-d"))
    {
      if (chdir (argv[i + 1]) != 0)
	{
	  perror (argv[i + 1]);
	  return EXIT_FAILURE;
	}
      i += 2;
    }
  if (argc > i && !strcmp (argv[i], "-g"))
    {
      generate_globals = 1;
      ++i;
    }

  if (outfile == 0)
    fatal ("No output file specified", "");

  if (generate_globals)
    start_globals ();

  first_infile = i;
  for (; i < argc; i++)
    {
      int j;
      /* Don't process one file twice.  */
      for (j = first_infile; j < i; j++)
	if (! strcmp (argv[i], argv[j]))
	  break;
      if (j == i)
	err_count += scan_file (argv[i]);
    }

  if (err_count == 0 && generate_globals)
    write_globals ();

  return (err_count > 0 ? EXIT_FAILURE : EXIT_SUCCESS);
}

/* Add a source file name boundary marker in the output file.  */
static void
put_filename (char *filename)
{
  char *tmp;

  for (tmp = filename; *tmp; tmp++)
    {
      if (IS_DIRECTORY_SEP(*tmp))
	filename = tmp + 1;
    }

  putc (037, outfile);
  putc ('S', outfile);
  fprintf (outfile, "%s\n", filename);
}

/* Read file FILENAME and output its doc strings to outfile.  */
/* Return 1 if file is not found, 0 if it is found.  */

static int
scan_file (char *filename)
{

  size_t len = strlen (filename);

  if (!generate_globals)
    put_filename (filename);
  if (len > 4 && !strcmp (filename + len - 4, ".elc"))
    return scan_lisp_file (filename, READ_BINARY);
  else if (len > 3 && !strcmp (filename + len - 3, ".el"))
    return scan_lisp_file (filename, READ_TEXT);
  else
    return scan_c_file (filename, READ_TEXT);
}

static void
start_globals (void)
{
  fprintf (outfile, "/* This file was auto-generated by make-docfile.  */\n");
  fprintf (outfile, "/* DO NOT EDIT.  */\n");
  fprintf (outfile, "struct emacs_globals {\n");
}

static char input_buffer[128];

/* Some state during the execution of `read_c_string_or_comment'.  */
struct rcsoc_state
{
  /* A count of spaces and newlines that have been read, but not output.  */
  unsigned pending_spaces, pending_newlines;

  /* Where we're reading from.  */
  FILE *in_file;

  /* If non-zero, a buffer into which to copy characters.  */
  char *buf_ptr;
  /* If non-zero, a file into which to copy characters.  */
  FILE *out_file;

  /* A keyword we look for at the beginning of lines.  If found, it is
     not copied, and SAW_KEYWORD is set to true.  */
  const char *keyword;
  /* The current point we've reached in an occurrence of KEYWORD in
     the input stream.  */
  const char *cur_keyword_ptr;
  /* Set to true if we saw an occurrence of KEYWORD.  */
  int saw_keyword;
};

/* Output CH to the file or buffer in STATE.  Any pending newlines or
   spaces are output first.  */

static inline void
put_char (int ch, struct rcsoc_state *state)
{
  int out_ch;
  do
    {
      if (state->pending_newlines > 0)
	{
	  state->pending_newlines--;
	  out_ch = '\n';
	}
      else if (state->pending_spaces > 0)
	{
	  state->pending_spaces--;
	  out_ch = ' ';
	}
      else
	out_ch = ch;

      if (state->out_file)
	putc (out_ch, state->out_file);
      if (state->buf_ptr)
	*state->buf_ptr++ = out_ch;
    }
  while (out_ch != ch);
}

/* If in the middle of scanning a keyword, continue scanning with
   character CH, otherwise output CH to the file or buffer in STATE.
   Any pending newlines or spaces are output first, as well as any
   previously scanned characters that were thought to be part of a
   keyword, but were in fact not.  */

static void
scan_keyword_or_put_char (int ch, struct rcsoc_state *state)
{
  if (state->keyword
      && *state->cur_keyword_ptr == ch
      && (state->cur_keyword_ptr > state->keyword
	  || state->pending_newlines > 0))
    /* We might be looking at STATE->keyword at some point.
       Keep looking until we know for sure.  */
    {
      if (*++state->cur_keyword_ptr == '\0')
	/* Saw the whole keyword.  Set SAW_KEYWORD flag to true.  */
	{
	  state->saw_keyword = 1;

	  /* Reset the scanning pointer.  */
	  state->cur_keyword_ptr = state->keyword;

	  /* Canonicalize whitespace preceding a usage string.  */
	  state->pending_newlines = 2;
	  state->pending_spaces = 0;

	  /* Skip any whitespace between the keyword and the
	     usage string.  */
	  do
	    ch = getc (state->in_file);
	  while (ch == ' ' || ch == '\n');

	  /* Output the open-paren we just read.  */
	  put_char (ch, state);

	  /* Skip the function name and replace it with `fn'.  */
	  do
	    ch = getc (state->in_file);
	  while (ch != ' ' && ch != ')');
	  put_char ('f', state);
	  put_char ('n', state);

	  /* Put back the last character.  */
	  ungetc (ch, state->in_file);
	}
    }
  else
    {
      if (state->keyword && state->cur_keyword_ptr > state->keyword)
	/* We scanned the beginning of a potential usage
	   keyword, but it was a false alarm.  Output the
	   part we scanned.  */
	{
	  const char *p;

	  for (p = state->keyword; p < state->cur_keyword_ptr; p++)
	    put_char (*p, state);

	  state->cur_keyword_ptr = state->keyword;
	}

      put_char (ch, state);
    }
}


/* Skip a C string or C-style comment from INFILE, and return the
   character that follows.  COMMENT non-zero means skip a comment.  If
   PRINTFLAG is positive, output string contents to outfile.  If it is
   negative, store contents in buf.  Convert escape sequences \n and
   \t to newline and tab; discard \ followed by newline.
   If SAW_USAGE is non-zero, then any occurrences of the string `usage:'
   at the beginning of a line will be removed, and *SAW_USAGE set to
   true if any were encountered.  */

static int
read_c_string_or_comment (FILE *infile, int printflag, int comment, int *saw_usage)
{
  register int c;
  struct rcsoc_state state;

  state.in_file = infile;
  state.buf_ptr = (printflag < 0 ? input_buffer : 0);
  state.out_file = (printflag > 0 ? outfile : 0);
  state.pending_spaces = 0;
  state.pending_newlines = 0;
  state.keyword = (saw_usage ? "usage:" : 0);
  state.cur_keyword_ptr = state.keyword;
  state.saw_keyword = 0;

  c = getc (infile);
  if (comment)
    while (c == '\n' || c == '\r' || c == '\t' || c == ' ')
      c = getc (infile);

  while (c != EOF)
    {
      while (c != EOF && (comment ? c != '*' : c != '"'))
	{
	  if (c == '\\')
	    {
	      c = getc (infile);
	      if (c == '\n' || c == '\r')
		{
		  c = getc (infile);
		  continue;
		}
	      if (c == 'n')
		c = '\n';
	      if (c == 't')
		c = '\t';
	    }

	  if (c == ' ')
	    state.pending_spaces++;
	  else if (c == '\n')
	    {
	      state.pending_newlines++;
	      state.pending_spaces = 0;
	    }
	  else
	    scan_keyword_or_put_char (c, &state);

	  c = getc (infile);
	}

      if (c != EOF)
	c = getc (infile);

      if (comment)
	{
	  if (c == '/')
	    {
	      c = getc (infile);
	      break;
	    }

	  scan_keyword_or_put_char ('*', &state);
	}
      else
	{
	  if (c != '"')
	    break;

	  /* If we had a "", concatenate the two strings.  */
	  c = getc (infile);
	}
    }

  if (printflag < 0)
    *state.buf_ptr = 0;

  if (saw_usage)
    *saw_usage = state.saw_keyword;

  return c;
}



/* Write to file OUT the argument names of function FUNC, whose text is in BUF.
   MINARGS and MAXARGS are the minimum and maximum number of arguments.  */

static void
write_c_args (FILE *out, char *func, char *buf, int minargs, int maxargs)
{
  register char *p;
  int in_ident = 0;
  char *ident_start IF_LINT (= NULL);
  size_t ident_length = 0;

  fprintf (out, "(fn");

  if (*buf == '(')
    ++buf;

  for (p = buf; *p; p++)
    {
      char c = *p;

      /* Notice when a new identifier starts.  */
      if ((('A' <= c && c <= 'Z')
	   || ('a' <= c && c <= 'z')
	   || ('0' <= c && c <= '9')
	   || c == '_')
	  != in_ident)
	{
	  if (!in_ident)
	    {
	      in_ident = 1;
	      ident_start = p;
	    }
	  else
	    {
	      in_ident = 0;
	      ident_length = p - ident_start;
	    }
	}

      /* Found the end of an argument, write out the last seen
	 identifier.  */
      if (c == ',' || c == ')')
	{
	  if (ident_length == 0)
	    {
	      error ("empty arg list for `%s' should be (void), not ()", func);
	      continue;
	    }

	  if (strncmp (ident_start, "void", ident_length) == 0)
	    continue;

	  putc (' ', out);

	  if (minargs == 0 && maxargs > 0)
	    fprintf (out, "&optional ");

	  minargs--;
	  maxargs--;

	  /* In C code, `default' is a reserved word, so we spell it
	     `defalt'; demangle that here.  */
	  if (ident_length == 6 && strncmp (ident_start, "defalt", 6) == 0)
	    fprintf (out, "DEFAULT");
	  else
	    while (ident_length-- > 0)
	      {
		c = *ident_start++;
		if (c >= 'a' && c <= 'z')
		  /* Upcase the letter.  */
		  c += 'A' - 'a';
		else if (c == '_')
		  /* Print underscore as hyphen.  */
		  c = '-';
		putc (c, out);
	      }
	}
    }

  putc (')', out);
}

/* The types of globals.  */
enum global_type
{
  EMACS_INTEGER,
  BOOLEAN,
  LISP_OBJECT,
  INVALID
};

/* A single global.  */
struct global
{
  enum global_type type;
  char *name;
};

/* All the variable names we saw while scanning C sources in `-g'
   mode.  */
int num_globals;
int num_globals_allocated;
struct global *globals;

static void
add_global (enum global_type type, char *name)
{
  /* Ignore the one non-symbol that can occur.  */
  if (strcmp (name, "..."))
    {
      ++num_globals;

      if (num_globals_allocated == 0)
	{
	  num_globals_allocated = 100;
	  globals = xmalloc (num_globals_allocated * sizeof (struct global));
	}
      else if (num_globals == num_globals_allocated)
	{
	  num_globals_allocated *= 2;
	  globals = xrealloc (globals,
			      num_globals_allocated * sizeof (struct global));
	}

      globals[num_globals - 1].type = type;
      globals[num_globals - 1].name = name;
    }
}

static int
compare_globals (const void *a, const void *b)
{
  const struct global *ga = a;
  const struct global *gb = b;
  return strcmp (ga->name, gb->name);
}

static void
write_globals (void)
{
  int i;
  qsort (globals, num_globals, sizeof (struct global), compare_globals);
  for (i = 0; i < num_globals; ++i)
    {
      char const *type;

      switch (globals[i].type)
	{
	case EMACS_INTEGER:
	  type = "EMACS_INT";
	  break;
	case BOOLEAN:
	  type = "int";
	  break;
	case LISP_OBJECT:
	  type = "Lisp_Object";
	  break;
	default:
	  fatal ("not a recognized DEFVAR_", 0);
	}

      fprintf (outfile, "  %s f_%s;\n", type, globals[i].name);
      fprintf (outfile, "#define %s globals.f_%s\n",
	       globals[i].name, globals[i].name);
      while (i + 1 < num_globals
	     && !strcmp (globals[i].name, globals[i + 1].name))
	++i;
    }

  fprintf (outfile, "};\n");
  fprintf (outfile, "extern struct emacs_globals globals;\n");
}


/* Read through a c file.  If a .o file is named,
   the corresponding .c or .m file is read instead.
   Looks for DEFUN constructs such as are defined in ../src/lisp.h.
   Accepts any word starting DEF... so it finds DEFSIMPLE and DEFPRED.  */

static int
scan_c_file (char *filename, const char *mode)
{
  FILE *infile;
  register int c;
  register int commas;
  int minargs, maxargs;
  int extension = filename[strlen (filename) - 1];

  if (extension == 'o')
    filename[strlen (filename) - 1] = 'c';

  infile = fopen (filename, mode);

  if (infile == NULL && extension == 'o')
    {
      /* try .m */
      filename[strlen (filename) - 1] = 'm';
      infile = fopen (filename, mode);
      if (infile == NULL)
        filename[strlen (filename) - 1] = 'c'; /* don't confuse people */
    }

  /* No error if non-ex input file */
  if (infile == NULL)
    {
      perror (filename);
      return 0;
    }

  /* Reset extension to be able to detect duplicate files.  */
  filename[strlen (filename) - 1] = extension;

  c = '\n';
  while (!feof (infile))
    {
      int doc_keyword = 0;
      int defunflag = 0;
      int defvarperbufferflag = 0;
      int defvarflag = 0;
      enum global_type type = INVALID;

      if (c != '\n' && c != '\r')
	{
	  c = getc (infile);
	  continue;
	}
      c = getc (infile);
      if (c == ' ')
	{
	  while (c == ' ')
	    c = getc (infile);
	  if (c != 'D')
	    continue;
	  c = getc (infile);
	  if (c != 'E')
	    continue;
	  c = getc (infile);
	  if (c != 'F')
	    continue;
	  c = getc (infile);
	  if (c != 'V')
	    continue;
	  c = getc (infile);
	  if (c != 'A')
	    continue;
	  c = getc (infile);
	  if (c != 'R')
	    continue;
	  c = getc (infile);
	  if (c != '_')
	    continue;

	  defvarflag = 1;

	  c = getc (infile);
	  defvarperbufferflag = (c == 'P');
	  if (generate_globals)
	    {
	      if (c == 'I')
		type = EMACS_INTEGER;
	      else if (c == 'L')
		type = LISP_OBJECT;
	      else if (c == 'B')
		type = BOOLEAN;
	    }

	  c = getc (infile);
	  /* We need to distinguish between DEFVAR_BOOL and
	     DEFVAR_BUFFER_DEFAULTS.  */
	  if (generate_globals && type == BOOLEAN && c != 'O')
	    type = INVALID;
	}
      else if (c == 'D')
	{
	  c = getc (infile);
	  if (c != 'E')
	    continue;
	  c = getc (infile);
	  if (c != 'F')
	    continue;
	  c = getc (infile);
	  defunflag = c == 'U';
	}
      else continue;

      if (generate_globals && (!defvarflag || defvarperbufferflag
			       || type == INVALID))
	continue;

      while (c != '(')
	{
	  if (c < 0)
	    goto eof;
	  c = getc (infile);
	}

      /* Lisp variable or function name.  */
      c = getc (infile);
      if (c != '"')
	continue;
      c = read_c_string_or_comment (infile, -1, 0, 0);

      if (generate_globals)
	{
	  int i = 0;
	  char *name;

	  /* Skip "," and whitespace.  */
	  do
	    {
	      c = getc (infile);
	    }
	  while (c == ',' || c == ' ' || c == '\t' || c == '\n' || c == '\r');

	  /* Read in the identifier.  */
	  do
	    {
	      input_buffer[i++] = c;
	      c = getc (infile);
	    }
	  while (! (c == ',' || c == ' ' || c == '\t' ||
		    c == '\n' || c == '\r'));
	  input_buffer[i] = '\0';

	  name = xmalloc (i + 1);
	  memcpy (name, input_buffer, i + 1);
	  add_global (type, name);
	  continue;
	}

      /* DEFVAR_LISP ("name", addr, "doc")
	 DEFVAR_LISP ("name", addr /\* doc *\/)
	 DEFVAR_LISP ("name", addr, doc: /\* doc *\/)  */

      if (defunflag)
	commas = 5;
      else if (defvarperbufferflag)
	commas = 3;
      else if (defvarflag)
	commas = 1;
      else  /* For DEFSIMPLE and DEFPRED */
	commas = 2;

      while (commas)
	{
	  if (c == ',')
	    {
	      commas--;

	      if (defunflag && (commas == 1 || commas == 2))
		{
		  int scanned = 0;
		  do
		    c = getc (infile);
		  while (c == ' ' || c == '\n' || c == '\r' || c == '\t');
		  if (c < 0)
		    goto eof;
		  ungetc (c, infile);
		  if (commas == 2) /* pick up minargs */
		    scanned = fscanf (infile, "%d", &minargs);
		  else /* pick up maxargs */
		    if (c == 'M' || c == 'U') /* MANY || UNEVALLED */
		      maxargs = -1;
		    else
		      scanned = fscanf (infile, "%d", &maxargs);
		  if (scanned < 0)
		    goto eof;
		}
	    }

	  if (c == EOF)
	    goto eof;
	  c = getc (infile);
	}

      while (c == ' ' || c == '\n' || c == '\r' || c == '\t')
	c = getc (infile);

      if (c == '"')
	c = read_c_string_or_comment (infile, 0, 0, 0);

      while (c != EOF && c != ',' && c != '/')
	c = getc (infile);
      if (c == ',')
	{
	  c = getc (infile);
	  while (c == ' ' || c == '\n' || c == '\r' || c == '\t')
	    c = getc (infile);
	  while ((c >= 'a' && c <= 'z') || (c >= 'Z' && c <= 'Z'))
	    c = getc (infile);
	  if (c == ':')
	    {
	      doc_keyword = 1;
	      c = getc (infile);
	      while (c == ' ' || c == '\n' || c == '\r' || c == '\t')
		c = getc (infile);
	    }
	}

      if (c == '"'
	  || (c == '/'
	      && (c = getc (infile),
		  ungetc (c, infile),
		  c == '*')))
	{
	  int comment = c != '"';
	  int saw_usage;

	  putc (037, outfile);
	  putc (defvarflag ? 'V' : 'F', outfile);
	  fprintf (outfile, "%s\n", input_buffer);

	  if (comment)
	    getc (infile); 	/* Skip past `*' */
	  c = read_c_string_or_comment (infile, 1, comment, &saw_usage);

	  /* If this is a defun, find the arguments and print them.  If
	     this function takes MANY or UNEVALLED args, then the C source
	     won't give the names of the arguments, so we shouldn't bother
	     trying to find them.

	     Various doc-string styles:
	      0: DEFUN (..., "DOC") (args)            [!comment]
	      1: DEFUN (..., /\* DOC *\/ (args))      [comment && !doc_keyword]
	      2: DEFUN (..., doc: /\* DOC *\/) (args) [comment && doc_keyword]
	  */
	  if (defunflag && maxargs != -1 && !saw_usage)
	    {
	      char argbuf[1024], *p = argbuf;

	      if (!comment || doc_keyword)
		while (c != ')')
		  {
		    if (c < 0)
		      goto eof;
		    c = getc (infile);
		  }

	      /* Skip into arguments.  */
	      while (c != '(')
		{
		  if (c < 0)
		    goto eof;
		  c = getc (infile);
		}
	      /* Copy arguments into ARGBUF.  */
	      *p++ = c;
	      do
		*p++ = c = getc (infile);
	      while (c != ')');
	      *p = '\0';
	      /* Output them.  */
	      fprintf (outfile, "\n\n");
	      write_c_args (outfile, input_buffer, argbuf, minargs, maxargs);
	    }
	  else if (defunflag && maxargs == -1 && !saw_usage)
	    /* The DOC should provide the usage form.  */
	    fprintf (stderr, "Missing `usage' for function `%s'.\n",
		     input_buffer);
	}
    }
 eof:
  fclose (infile);
  return 0;
}

/* Read a file of Lisp code, compiled or interpreted.
 Looks for
  (defun NAME ARGS DOCSTRING ...)
  (defmacro NAME ARGS DOCSTRING ...)
  (defsubst NAME ARGS DOCSTRING ...)
  (autoload (quote NAME) FILE DOCSTRING ...)
  (defvar NAME VALUE DOCSTRING)
  (defconst NAME VALUE DOCSTRING)
  (fset (quote NAME) (make-byte-code ... DOCSTRING ...))
  (fset (quote NAME) #[... DOCSTRING ...])
  (defalias (quote NAME) #[... DOCSTRING ...])
  (custom-declare-variable (quote NAME) VALUE DOCSTRING ...)
 starting in column zero.
 (quote NAME) may appear as 'NAME as well.

 We also look for #@LENGTH CONTENTS^_ at the beginning of the line.
 When we find that, we save it for the following defining-form,
 and we use that instead of reading a doc string within that defining-form.

 For defvar, defconst, and fset we skip to the docstring with a kludgy
 formatting convention: all docstrings must appear on the same line as the
 initial open-paren (the one in column zero) and must contain a backslash
 and a newline immediately after the initial double-quote.  No newlines
 must appear between the beginning of the form and the first double-quote.
 For defun, defmacro, and autoload, we know how to skip over the
 arglist, but the doc string must still have a backslash and newline
 immediately after the double quote.
 The only source files that must follow this convention are preloaded
 uncompiled ones like loaddefs.el and bindings.el; aside
 from that, it is always the .elc file that we look at, and they are no
 problem because byte-compiler output follows this convention.
 The NAME and DOCSTRING are output.
 NAME is preceded by `F' for a function or `V' for a variable.
 An entry is output only if DOCSTRING has \ newline just after the opening "
 */

static void
skip_white (FILE *infile)
{
  char c = ' ';
  while (c == ' ' || c == '\t' || c == '\n' || c == '\r')
    c = getc (infile);
  ungetc (c, infile);
}

static void
read_lisp_symbol (FILE *infile, char *buffer)
{
  char c;
  char *fillp = buffer;

  skip_white (infile);
  while (1)
    {
      c = getc (infile);
      if (c == '\\')
	*(++fillp) = getc (infile);
      else if (c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '(' || c == ')')
	{
	  ungetc (c, infile);
	  *fillp = 0;
	  break;
	}
      else
	*fillp++ = c;
    }

  if (! buffer[0])
    fprintf (stderr, "## expected a symbol, got '%c'\n", c);

  skip_white (infile);
}

static int
scan_lisp_file (const char *filename, const char *mode)
{
  FILE *infile;
  register int c;
  char *saved_string = 0;

  if (generate_globals)
    fatal ("scanning lisp file when -g specified", 0);

  infile = fopen (filename, mode);
  if (infile == NULL)
    {
      perror (filename);
      return 0;				/* No error */
    }

  c = '\n';
  while (!feof (infile))
    {
      char buffer[BUFSIZ];
      char type;

      /* If not at end of line, skip till we get to one.  */
      if (c != '\n' && c != '\r')
	{
	  c = getc (infile);
	  continue;
	}
      /* Skip the line break.  */
      while (c == '\n' || c == '\r')
	c = getc (infile);
      /* Detect a dynamic doc string and save it for the next expression.  */
      if (c == '#')
	{
	  c = getc (infile);
	  if (c == '@')
	    {
	      size_t length = 0;
	      size_t i;

	      /* Read the length.  */
	      while ((c = getc (infile),
		      c >= '0' && c <= '9'))
		{
		  length *= 10;
		  length += c - '0';
		}

	      if (length <= 1)
		fatal ("invalid dynamic doc string length", "");

	      if (c != ' ')
		fatal ("space not found after dynamic doc string length", "");

	      /* The next character is a space that is counted in the length
		 but not part of the doc string.
		 We already read it, so just ignore it.  */
	      length--;

	      /* Read in the contents.  */
	      free (saved_string);
	      saved_string = (char *) xmalloc (length);
	      for (i = 0; i < length; i++)
		saved_string[i] = getc (infile);
	      /* The last character is a ^_.
		 That is needed in the .elc file
		 but it is redundant in DOC.  So get rid of it here.  */
	      saved_string[length - 1] = 0;
	      /* Skip the line break.  */
	      while (c == '\n' || c == '\r')
		c = getc (infile);
	      /* Skip the following line.  */
	      while (c != '\n' && c != '\r')
		c = getc (infile);
	    }
	  continue;
	}

      if (c != '(')
	continue;

      read_lisp_symbol (infile, buffer);

      if (! strcmp (buffer, "defun")
	  || ! strcmp (buffer, "defmacro")
	  || ! strcmp (buffer, "defsubst"))
	{
	  type = 'F';
	  read_lisp_symbol (infile, buffer);

	  /* Skip the arguments: either "nil" or a list in parens */

	  c = getc (infile);
	  if (c == 'n') /* nil */
	    {
	      if ((c = getc (infile)) != 'i'
		  || (c = getc (infile)) != 'l')
		{
		  fprintf (stderr, "## unparsable arglist in %s (%s)\n",
			   buffer, filename);
		  continue;
		}
	    }
	  else if (c != '(')
	    {
	      fprintf (stderr, "## unparsable arglist in %s (%s)\n",
		       buffer, filename);
	      continue;
	    }
	  else
	    while (c != ')')
	      c = getc (infile);
	  skip_white (infile);

	  /* If the next three characters aren't `dquote bslash newline'
	     then we're not reading a docstring.
	   */
	  if ((c = getc (infile)) != '"'
	      || (c = getc (infile)) != '\\'
	      || ((c = getc (infile)) != '\n' && c != '\r'))
	    {
#ifdef DEBUG
	      fprintf (stderr, "## non-docstring in %s (%s)\n",
		       buffer, filename);
#endif
	      continue;
	    }
	}

      /* defcustom can only occur in uncompiled Lisp files.  */
      else if (! strcmp (buffer, "defvar")
	       || ! strcmp (buffer, "defconst")
	       || ! strcmp (buffer, "defcustom"))
	{
	  char c1 = 0, c2 = 0;
	  type = 'V';
	  read_lisp_symbol (infile, buffer);

	  if (saved_string == 0)
	    {

	      /* Skip until the end of line; remember two previous chars.  */
	      while (c != '\n' && c != '\r' && c >= 0)
		{
		  c2 = c1;
		  c1 = c;
		  c = getc (infile);
		}

	      /* If two previous characters were " and \,
		 this is a doc string.  Otherwise, there is none.  */
	      if (c2 != '"' || c1 != '\\')
		{
#ifdef DEBUG
		  fprintf (stderr, "## non-docstring in %s (%s)\n",
			   buffer, filename);
#endif
		  continue;
		}
	    }
	}

      else if (! strcmp (buffer, "custom-declare-variable")
	       || ! strcmp (buffer, "defvaralias")
	       )
	{
	  char c1 = 0, c2 = 0;
	  type = 'V';

	  c = getc (infile);
	  if (c == '\'')
	    read_lisp_symbol (infile, buffer);
	  else
	    {
	      if (c != '(')
		{
		  fprintf (stderr,
			   "## unparsable name in custom-declare-variable in %s\n",
			   filename);
		  continue;
		}
	      read_lisp_symbol (infile, buffer);
	      if (strcmp (buffer, "quote"))
		{
		  fprintf (stderr,
			   "## unparsable name in custom-declare-variable in %s\n",
			   filename);
		  continue;
		}
	      read_lisp_symbol (infile, buffer);
	      c = getc (infile);
	      if (c != ')')
		{
		  fprintf (stderr,
			   "## unparsable quoted name in custom-declare-variable in %s\n",
			   filename);
		  continue;
		}
	    }

	  if (saved_string == 0)
	    {
	      /* Skip to end of line; remember the two previous chars.  */
	      while (c != '\n' && c != '\r' && c >= 0)
		{
		  c2 = c1;
		  c1 = c;
		  c = getc (infile);
		}

	      /* If two previous characters were " and \,
		 this is a doc string.  Otherwise, there is none.  */
	      if (c2 != '"' || c1 != '\\')
		{
#ifdef DEBUG
		  fprintf (stderr, "## non-docstring in %s (%s)\n",
			   buffer, filename);
#endif
		  continue;
		}
	    }
	}

      else if (! strcmp (buffer, "fset") || ! strcmp (buffer, "defalias"))
	{
	  char c1 = 0, c2 = 0;
	  type = 'F';

	  c = getc (infile);
	  if (c == '\'')
	    read_lisp_symbol (infile, buffer);
	  else
	    {
	      if (c != '(')
		{
		  fprintf (stderr, "## unparsable name in fset in %s\n",
			   filename);
		  continue;
		}
	      read_lisp_symbol (infile, buffer);
	      if (strcmp (buffer, "quote"))
		{
		  fprintf (stderr, "## unparsable name in fset in %s\n",
			   filename);
		  continue;
		}
	      read_lisp_symbol (infile, buffer);
	      c = getc (infile);
	      if (c != ')')
		{
		  fprintf (stderr,
			   "## unparsable quoted name in fset in %s\n",
			   filename);
		  continue;
		}
	    }

	  if (saved_string == 0)
	    {
	      /* Skip to end of line; remember the two previous chars.  */
	      while (c != '\n' && c != '\r' && c >= 0)
		{
		  c2 = c1;
		  c1 = c;
		  c = getc (infile);
		}

	      /* If two previous characters were " and \,
		 this is a doc string.  Otherwise, there is none.  */
	      if (c2 != '"' || c1 != '\\')
		{
#ifdef DEBUG
		  fprintf (stderr, "## non-docstring in %s (%s)\n",
			   buffer, filename);
#endif
		  continue;
		}
	    }
	}

      else if (! strcmp (buffer, "autoload"))
	{
	  type = 'F';
	  c = getc (infile);
	  if (c == '\'')
	    read_lisp_symbol (infile, buffer);
	  else
	    {
	      if (c != '(')
		{
		  fprintf (stderr, "## unparsable name in autoload in %s\n",
			   filename);
		  continue;
		}
	      read_lisp_symbol (infile, buffer);
	      if (strcmp (buffer, "quote"))
		{
		  fprintf (stderr, "## unparsable name in autoload in %s\n",
			   filename);
		  continue;
		}
	      read_lisp_symbol (infile, buffer);
	      c = getc (infile);
	      if (c != ')')
		{
		  fprintf (stderr,
			   "## unparsable quoted name in autoload in %s\n",
			   filename);
		  continue;
		}
	    }
	  skip_white (infile);
	  if ((c = getc (infile)) != '\"')
	    {
	      fprintf (stderr, "## autoload of %s unparsable (%s)\n",
		       buffer, filename);
	      continue;
	    }
	  read_c_string_or_comment (infile, 0, 0, 0);
	  skip_white (infile);

	  if (saved_string == 0)
	    {
	      /* If the next three characters aren't `dquote bslash newline'
		 then we're not reading a docstring.  */
	      if ((c = getc (infile)) != '"'
		  || (c = getc (infile)) != '\\'
		  || ((c = getc (infile)) != '\n' && c != '\r'))
		{
#ifdef DEBUG
		  fprintf (stderr, "## non-docstring in %s (%s)\n",
			   buffer, filename);
#endif
		  continue;
		}
	    }
	}

#ifdef DEBUG
      else if (! strcmp (buffer, "if")
	       || ! strcmp (buffer, "byte-code"))
	;
#endif

      else
	{
#ifdef DEBUG
	  fprintf (stderr, "## unrecognized top-level form, %s (%s)\n",
		   buffer, filename);
#endif
	  continue;
	}

      /* At this point, we should either use the previous
	 dynamic doc string in saved_string
	 or gobble a doc string from the input file.

	 In the latter case, the opening quote (and leading
	 backslash-newline) have already been read.  */

      putc (037, outfile);
      putc (type, outfile);
      fprintf (outfile, "%s\n", buffer);
      if (saved_string)
	{
	  fputs (saved_string, outfile);
	  /* Don't use one dynamic doc string twice.  */
	  free (saved_string);
	  saved_string = 0;
	}
      else
	read_c_string_or_comment (infile, 1, 0, 0);
    }
  fclose (infile);
  return 0;
}


/* make-docfile.c ends here */
