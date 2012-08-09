/* Tags file maker to go with GNU Emacs           -*- coding: latin-1 -*-

Copyright (C) 1984 The Regents of the University of California

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:
1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the
   distribution.
3. Neither the name of the University nor the names of its
   contributors may be used to endorse or promote products derived
   from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS
BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


Copyright (C) 1984, 1987-1989, 1993-1995, 1998-2012
  Free Software Foundation, Inc.

This file is not considered part of GNU Emacs.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.  */


/* NB To comply with the above BSD license, copyright information is
reproduced in etc/ETAGS.README.  That file should be updated when the
above notices are.

To the best of our knowledge, this code was originally based on the
ctags.c distributed with BSD4.2, which was copyrighted by the
University of California, as described above. */


/*
 * Authors:
 * 1983 Ctags originally by Ken Arnold.
 * 1984 Fortran added by Jim Kleckner.
 * 1984 Ed Pelegri-Llopart added C typedefs.
 * 1985 Emacs TAGS format by Richard Stallman.
 * 1989 Sam Kendall added C++.
 * 1992 Joseph B. Wells improved C and C++ parsing.
 * 1993 Francesco Potortì reorganized C and C++.
 * 1994 Line-by-line regexp tags by Tom Tromey.
 * 2001 Nested classes by Francesco Potortì (concept by Mykola Dzyuba).
 * 2002 #line directives by Francesco Potortì.
 *
 * Francesco Potortì <pot@gnu.org> has maintained and improved it since 1993.
 */

/*
 * If you want to add support for a new language, start by looking at the LUA
 * language, which is the simplest.  Alternatively, consider distributing etags
 * together with a configuration file containing regexp definitions for etags.
 */

char pot_etags_version[] = "@(#) pot revision number is 17.38.1.4";

#define	TRUE	1
#define	FALSE	0

#ifdef DEBUG
#  undef DEBUG
#  define DEBUG TRUE
#else
#  define DEBUG  FALSE
#  define NDEBUG		/* disable assert */
#endif

#ifdef HAVE_CONFIG_H
# include <config.h>
  /* This is probably not necessary any more.  On some systems, config.h
     used to define static as nothing for the sake of unexec.  We don't
     want that here since we don't use unexec.  None of these systems
     are supported any more, but the idea is still mentioned in
     etc/PROBLEMS.  */
# undef static
# ifndef PTR			/* for XEmacs */
#   define PTR void *
# endif
#else  /* no config.h */
# if defined (__STDC__) && (__STDC__ || defined (__SUNPRO_C))
#   define PTR void *		/* for generic pointers */
# else /* not standard C */
#   define const		/* remove const for old compilers' sake */
#   define PTR long *		/* don't use void* */
# endif
#endif /* !HAVE_CONFIG_H */

#ifndef _GNU_SOURCE
# define _GNU_SOURCE 1		/* enables some compiler checks on GNU */
#endif

/* WIN32_NATIVE is for XEmacs.
   MSDOS, WINDOWSNT, DOS_NT are for Emacs. */
#ifdef WIN32_NATIVE
# undef MSDOS
# undef  WINDOWSNT
# define WINDOWSNT
#endif /* WIN32_NATIVE */

#ifdef MSDOS
# undef MSDOS
# define MSDOS TRUE
# include <fcntl.h>
# include <sys/param.h>
# include <io.h>
# ifndef HAVE_CONFIG_H
#   define DOS_NT
#   include <sys/config.h>
# endif
#else
# define MSDOS FALSE
#endif /* MSDOS */

#ifdef WINDOWSNT
# include <fcntl.h>
# include <direct.h>
# include <io.h>
# define MAXPATHLEN _MAX_PATH
# undef HAVE_NTGUI
# undef  DOS_NT
# define DOS_NT
# ifndef HAVE_GETCWD
#   define HAVE_GETCWD
# endif /* undef HAVE_GETCWD */
#else /* not WINDOWSNT */
#endif /* !WINDOWSNT */

#include <unistd.h>
#ifndef HAVE_UNISTD_H
# if defined (HAVE_GETCWD) && !defined (WINDOWSNT)
    extern char *getcwd (char *buf, size_t size);
# endif
#endif /* HAVE_UNISTD_H */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>

#include <assert.h>
#ifdef NDEBUG
# undef  assert			/* some systems have a buggy assert.h */
# define assert(x) ((void) 0)
#endif

#ifdef NO_LONG_OPTIONS		/* define this if you don't have GNU getopt */
# define NO_LONG_OPTIONS TRUE
# define getopt_long(argc,argv,optstr,lopts,lind) getopt (argc, argv, optstr)
  extern char *optarg;
  extern int optind, opterr;
#else
# define NO_LONG_OPTIONS FALSE
# include <getopt.h>
#endif /* NO_LONG_OPTIONS */

#ifndef HAVE_CONFIG_H		/* this is a standalone compilation */
# ifdef __CYGWIN__         	/* compiling on Cygwin */
			     !!! NOTICE !!!
 the regex.h distributed with Cygwin is not compatible with etags, alas!
If you want regular expression support, you should delete this notice and
	      arrange to use the GNU regex.h and regex.c.
# endif
#endif
#include <regex.h>

/* Define CTAGS to make the program "ctags" compatible with the usual one.
 Leave it undefined to make the program "etags", which makes emacs-style
 tag tables and tags typedefs, #defines and struct/union/enum by default. */
#ifdef CTAGS
# undef  CTAGS
# define CTAGS TRUE
#else
# define CTAGS FALSE
#endif

#define streq(s,t)	(assert ((s)!=NULL || (t)!=NULL), !strcmp (s, t))
#define strcaseeq(s,t)	(assert ((s)!=NULL && (t)!=NULL), !etags_strcasecmp (s, t))
#define strneq(s,t,n)	(assert ((s)!=NULL || (t)!=NULL), !strncmp (s, t, n))
#define strncaseeq(s,t,n) (assert ((s)!=NULL && (t)!=NULL), !etags_strncasecmp (s, t, n))

#define CHARS 256		/* 2^sizeof(char) */
#define CHAR(x)		((unsigned int)(x) & (CHARS - 1))
#define	iswhite(c)	(_wht[CHAR (c)]) /* c is white (see white) */
#define notinname(c)	(_nin[CHAR (c)]) /* c is not in a name (see nonam) */
#define	begtoken(c)	(_btk[CHAR (c)]) /* c can start token (see begtk) */
#define	intoken(c)	(_itk[CHAR (c)]) /* c can be in token (see midtk) */
#define	endtoken(c)	(_etk[CHAR (c)]) /* c ends tokens (see endtk) */

#define ISALNUM(c)	isalnum (CHAR (c))
#define ISALPHA(c)	isalpha (CHAR (c))
#define ISDIGIT(c)	isdigit (CHAR (c))
#define ISLOWER(c)	islower (CHAR (c))

#define lowcase(c)	tolower (CHAR (c))


/*
 *	xnew, xrnew -- allocate, reallocate storage
 *
 * SYNOPSIS:	Type *xnew (int n, Type);
 *		void xrnew (OldPointer, int n, Type);
 */
#if DEBUG
# include "chkmalloc.h"
# define xnew(n,Type)	  ((Type *) trace_malloc (__FILE__, __LINE__, \
						  (n) * sizeof (Type)))
# define xrnew(op,n,Type) ((op) = (Type *) trace_realloc (__FILE__, __LINE__, \
					(char *) (op), (n) * sizeof (Type)))
#else
# define xnew(n,Type)	  ((Type *) xmalloc ((n) * sizeof (Type)))
# define xrnew(op,n,Type) ((op) = (Type *) xrealloc ( \
					(char *) (op), (n) * sizeof (Type)))
#endif

#define bool int

typedef void Lang_function (FILE *);

typedef struct
{
  const char *suffix;           /* file name suffix for this compressor */
  const char *command;		/* takes one arg and decompresses to stdout */
} compressor;

typedef struct
{
  const char *name;             /* language name */
  const char *help; 		/* detailed help for the language */
  Lang_function *function;	/* parse function */
  const char **suffixes;        /* name suffixes of this language's files */
  const char **filenames;       /* names of this language's files */
  const char **interpreters;    /* interpreters for this language */
  bool metasource;		/* source used to generate other sources */
} language;

typedef struct fdesc
{
  struct fdesc *next;		/* for the linked list */
  char *infname;		/* uncompressed input file name */
  char *infabsname;		/* absolute uncompressed input file name */
  char *infabsdir;		/* absolute dir of input file */
  char *taggedfname;		/* file name to write in tagfile */
  language *lang;		/* language of file */
  char *prop;			/* file properties to write in tagfile */
  bool usecharno;		/* etags tags shall contain char number */
  bool written;			/* entry written in the tags file */
} fdesc;

typedef struct node_st
{				/* sorting structure */
  struct node_st *left, *right;	/* left and right sons */
  fdesc *fdp;			/* description of file to whom tag belongs */
  char *name; 			/* tag name */
  char *regex;			/* search regexp */
  bool valid;			/* write this tag on the tag file */
  bool is_func;			/* function tag: use regexp in CTAGS mode */
  bool been_warned;		/* warning already given for duplicated tag */
  int lno;			/* line number tag is on */
  long cno;			/* character number line starts on */
} node;

/*
 * A `linebuffer' is a structure which holds a line of text.
 * `readline_internal' reads a line from a stream into a linebuffer
 * and works regardless of the length of the line.
 * SIZE is the size of BUFFER, LEN is the length of the string in
 * BUFFER after readline reads it.
 */
typedef struct
{
  long size;
  int len;
  char *buffer;
} linebuffer;

/* Used to support mixing of --lang and file names. */
typedef struct
{
  enum {
    at_language,		/* a language specification */
    at_regexp,			/* a regular expression */
    at_filename,		/* a file name */
    at_stdin,			/* read from stdin here */
    at_end			/* stop parsing the list */
  } arg_type;			/* argument type */
  language *lang;		/* language associated with the argument */
  char *what;			/* the argument itself */
} argument;

/* Structure defining a regular expression. */
typedef struct regexp
{
  struct regexp *p_next;	/* pointer to next in list */
  language *lang;		/* if set, use only for this language */
  char *pattern;		/* the regexp pattern */
  char *name;			/* tag name */
  struct re_pattern_buffer *pat; /* the compiled pattern */
  struct re_registers regs;	/* re registers */
  bool error_signaled;		/* already signaled for this regexp */
  bool force_explicit_name;	/* do not allow implicit tag name */
  bool ignore_case;		/* ignore case when matching */
  bool multi_line;		/* do a multi-line match on the whole file */
} regexp;


/* Many compilers barf on this:
	Lang_function Ada_funcs;
   so let's write it this way */
static void Ada_funcs (FILE *);
static void Asm_labels (FILE *);
static void C_entries (int c_ext, FILE *);
static void default_C_entries (FILE *);
static void plain_C_entries (FILE *);
static void Cjava_entries (FILE *);
static void Cobol_paragraphs (FILE *);
static void Cplusplus_entries (FILE *);
static void Cstar_entries (FILE *);
static void Erlang_functions (FILE *);
static void Forth_words (FILE *);
static void Fortran_functions (FILE *);
static void HTML_labels (FILE *);
static void Lisp_functions (FILE *);
static void Lua_functions (FILE *);
static void Makefile_targets (FILE *);
static void Pascal_functions (FILE *);
static void Perl_functions (FILE *);
static void PHP_functions (FILE *);
static void PS_functions (FILE *);
static void Prolog_functions (FILE *);
static void Python_functions (FILE *);
static void Scheme_functions (FILE *);
static void TeX_commands (FILE *);
static void Texinfo_nodes (FILE *);
static void Yacc_entries (FILE *);
static void just_read_file (FILE *);

static void print_language_names (void);
static void print_version (void);
static void print_help (argument *);
int main (int, char **);

static compressor *get_compressor_from_suffix (char *, char **);
static language *get_language_from_langname (const char *);
static language *get_language_from_interpreter (char *);
static language *get_language_from_filename (char *, bool);
static void readline (linebuffer *, FILE *);
static long readline_internal (linebuffer *, FILE *);
static bool nocase_tail (const char *);
static void get_tag (char *, char **);

static void analyse_regex (char *);
static void free_regexps (void);
static void regex_tag_multiline (void);
static void error (const char *, const char *);
static void suggest_asking_for_help (void) NO_RETURN;
void fatal (const char *, const char *) NO_RETURN;
static void pfatal (const char *) NO_RETURN;
static void add_node (node *, node **);

static void init (void);
static void process_file_name (char *, language *);
static void process_file (FILE *, char *, language *);
static void find_entries (FILE *);
static void free_tree (node *);
static void free_fdesc (fdesc *);
static void pfnote (char *, bool, char *, int, int, long);
static void make_tag (const char *, int, bool, char *, int, int, long);
static void invalidate_nodes (fdesc *, node **);
static void put_entries (node *);

static char *concat (const char *, const char *, const char *);
static char *skip_spaces (char *);
static char *skip_non_spaces (char *);
static char *savenstr (const char *, int);
static char *savestr (const char *);
static char *etags_strchr (const char *, int);
static char *etags_strrchr (const char *, int);
static int etags_strcasecmp (const char *, const char *);
static int etags_strncasecmp (const char *, const char *, int);
static char *etags_getcwd (void);
static char *relative_filename (char *, char *);
static char *absolute_filename (char *, char *);
static char *absolute_dirname (char *, char *);
static bool filename_is_absolute (char *f);
static void canonicalize_filename (char *);
static void linebuffer_init (linebuffer *);
static void linebuffer_setlen (linebuffer *, int);
static PTR xmalloc (size_t);
static PTR xrealloc (char *, size_t);


static char searchar = '/';	/* use /.../ searches */

static char *tagfile;		/* output file */
static char *progname;		/* name this program was invoked with */
static char *cwd;		/* current working directory */
static char *tagfiledir;	/* directory of tagfile */
static FILE *tagf;		/* ioptr for tags file */
static ptrdiff_t whatlen_max;	/* maximum length of any 'what' member */

static fdesc *fdhead;		/* head of file description list */
static fdesc *curfdp;		/* current file description */
static int lineno;		/* line number of current line */
static long charno;		/* current character number */
static long linecharno;		/* charno of start of current line */
static char *dbp;		/* pointer to start of current tag */

static const int invalidcharno = -1;

static node *nodehead;		/* the head of the binary tree of tags */
static node *last_node;		/* the last node created */

static linebuffer lb;		/* the current line */
static linebuffer filebuf;	/* a buffer containing the whole file */
static linebuffer token_name;	/* a buffer containing a tag name */

/* boolean "functions" (see init)	*/
static bool _wht[CHARS], _nin[CHARS], _itk[CHARS], _btk[CHARS], _etk[CHARS];
static const char
  /* white chars */
  *white = " \f\t\n\r\v",
  /* not in a name */
  *nonam = " \f\t\n\r()=,;",	/* look at make_tag before modifying! */
  /* token ending chars */
  *endtk = " \t\n\r\"'#()[]{}=-+%*/&|^~!<>;,.:?",
  /* token starting chars */
  *begtk = "ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz$~@",
  /* valid in-token chars */
  *midtk = "ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz$0123456789";

static bool append_to_tagfile;	/* -a: append to tags */
/* The next five default to TRUE in C and derived languages.  */
static bool typedefs;		/* -t: create tags for C and Ada typedefs */
static bool typedefs_or_cplusplus; /* -T: create tags for C typedefs, level */
				/* 0 struct/enum/union decls, and C++ */
				/* member functions. */
static bool constantypedefs;	/* -d: create tags for C #define, enum */
				/* constants and variables. */
				/* -D: opposite of -d.  Default under ctags. */
static bool globals;		/* create tags for global variables */
static bool members;		/* create tags for C member variables */
static bool declarations;	/* --declarations: tag them and extern in C&Co*/
static bool no_line_directive;	/* ignore #line directives (undocumented) */
static bool no_duplicates;	/* no duplicate tags for ctags (undocumented) */
static bool update;		/* -u: update tags */
static bool vgrind_style;	/* -v: create vgrind style index output */
static bool no_warnings;	/* -w: suppress warnings (undocumented) */
static bool cxref_style;	/* -x: create cxref style output */
static bool cplusplus;		/* .[hc] means C++, not C (undocumented) */
static bool ignoreindent;	/* -I: ignore indentation in C */
static bool packages_only;	/* --packages-only: in Ada, only tag packages*/

/* STDIN is defined in LynxOS system headers */
#ifdef STDIN
# undef STDIN
#endif

#define STDIN 0x1001		/* returned by getopt_long on --parse-stdin */
static bool parsing_stdin;	/* --parse-stdin used */

static regexp *p_head;		/* list of all regexps */
static bool need_filebuf;	/* some regexes are multi-line */

static struct option longopts[] =
{
  { "append",             no_argument,       NULL,               'a'   },
  { "packages-only",      no_argument,       &packages_only,     TRUE  },
  { "c++",                no_argument,       NULL,               'C'   },
  { "declarations",       no_argument,       &declarations,      TRUE  },
  { "no-line-directive",  no_argument,       &no_line_directive, TRUE  },
  { "no-duplicates",      no_argument,       &no_duplicates,     TRUE  },
  { "help",               no_argument,       NULL,               'h'   },
  { "help",               no_argument,       NULL,               'H'   },
  { "ignore-indentation", no_argument,       NULL,               'I'   },
  { "language",           required_argument, NULL,               'l'   },
  { "members",            no_argument,       &members,           TRUE  },
  { "no-members",         no_argument,       &members,           FALSE },
  { "output",             required_argument, NULL,               'o'   },
  { "regex",              required_argument, NULL,               'r'   },
  { "no-regex",           no_argument,       NULL,               'R'   },
  { "ignore-case-regex",  required_argument, NULL,               'c'   },
  { "parse-stdin",        required_argument, NULL,               STDIN },
  { "version",            no_argument,       NULL,               'V'   },

#if CTAGS /* Ctags options */
  { "backward-search",    no_argument,       NULL,               'B'   },
  { "cxref",              no_argument,       NULL,               'x'   },
  { "defines",            no_argument,       NULL,               'd'   },
  { "globals",            no_argument,       &globals,           TRUE  },
  { "typedefs",           no_argument,       NULL,               't'   },
  { "typedefs-and-c++",   no_argument,       NULL,               'T'   },
  { "update",             no_argument,       NULL,               'u'   },
  { "vgrind",             no_argument,       NULL,               'v'   },
  { "no-warn",            no_argument,       NULL,               'w'   },

#else /* Etags options */
  { "no-defines",         no_argument,       NULL,               'D'   },
  { "no-globals",         no_argument,       &globals,           FALSE },
  { "include",            required_argument, NULL,               'i'   },
#endif
  { NULL }
};

static compressor compressors[] =
{
  { "z", "gzip -d -c"},
  { "Z", "gzip -d -c"},
  { "gz", "gzip -d -c"},
  { "GZ", "gzip -d -c"},
  { "bz2", "bzip2 -d -c" },
  { "xz", "xz -d -c" },
  { NULL }
};

/*
 * Language stuff.
 */

/* Ada code */
static const char *Ada_suffixes [] =
  { "ads", "adb", "ada", NULL };
static const char Ada_help [] =
"In Ada code, functions, procedures, packages, tasks and types are\n\
tags.  Use the `--packages-only' option to create tags for\n\
packages only.\n\
Ada tag names have suffixes indicating the type of entity:\n\
	Entity type:	Qualifier:\n\
	------------	----------\n\
	function	/f\n\
	procedure	/p\n\
	package spec	/s\n\
	package body	/b\n\
	type		/t\n\
	task		/k\n\
Thus, `M-x find-tag <RET> bidule/b <RET>' will go directly to the\n\
body of the package `bidule', while `M-x find-tag <RET> bidule <RET>'\n\
will just search for any tag `bidule'.";

/* Assembly code */
static const char *Asm_suffixes [] =
  { "a",	/* Unix assembler */
    "asm", /* Microcontroller assembly */
    "def", /* BSO/Tasking definition includes  */
    "inc", /* Microcontroller include files */
    "ins", /* Microcontroller include files */
    "s", "sa", /* Unix assembler */
    "S",   /* cpp-processed Unix assembler */
    "src", /* BSO/Tasking C compiler output */
    NULL
  };
static const char Asm_help [] =
"In assembler code, labels appearing at the beginning of a line,\n\
followed by a colon, are tags.";


/* Note that .c and .h can be considered C++, if the --c++ flag was
   given, or if the `class' or `template' keywords are met inside the file.
   That is why default_C_entries is called for these. */
static const char *default_C_suffixes [] =
  { "c", "h", NULL };
#if CTAGS				/* C help for Ctags */
static const char default_C_help [] =
"In C code, any C function is a tag.  Use -t to tag typedefs.\n\
Use -T to tag definitions of `struct', `union' and `enum'.\n\
Use -d to tag `#define' macro definitions and `enum' constants.\n\
Use --globals to tag global variables.\n\
You can tag function declarations and external variables by\n\
using `--declarations', and struct members by using `--members'.";
#else					/* C help for Etags */
static const char default_C_help [] =
"In C code, any C function or typedef is a tag, and so are\n\
definitions of `struct', `union' and `enum'.  `#define' macro\n\
definitions and `enum' constants are tags unless you specify\n\
`--no-defines'.  Global variables are tags unless you specify\n\
`--no-globals' and so are struct members unless you specify\n\
`--no-members'.  Use of `--no-globals', `--no-defines' and\n\
`--no-members' can make the tags table file much smaller.\n\
You can tag function declarations and external variables by\n\
using `--declarations'.";
#endif	/* C help for Ctags and Etags */

static const char *Cplusplus_suffixes [] =
  { "C", "c++", "cc", "cpp", "cxx", "H", "h++", "hh", "hpp", "hxx",
    "M",			/* Objective C++ */
    "pdb",			/* PostScript with C syntax */
    NULL };
static const char Cplusplus_help [] =
"In C++ code, all the tag constructs of C code are tagged.  (Use\n\
--help --lang=c --lang=c++ for full help.)\n\
In addition to C tags, member functions are also recognized.  Member\n\
variables are recognized unless you use the `--no-members' option.\n\
Tags for variables and functions in classes are named `CLASS::VARIABLE'\n\
and `CLASS::FUNCTION'.  `operator' definitions have tag names like\n\
`operator+'.";

static const char *Cjava_suffixes [] =
  { "java", NULL };
static char Cjava_help [] =
"In Java code, all the tags constructs of C and C++ code are\n\
tagged.  (Use --help --lang=c --lang=c++ --lang=java for full help.)";


static const char *Cobol_suffixes [] =
  { "COB", "cob", NULL };
static char Cobol_help [] =
"In Cobol code, tags are paragraph names; that is, any word\n\
starting in column 8 and followed by a period.";

static const char *Cstar_suffixes [] =
  { "cs", "hs", NULL };

static const char *Erlang_suffixes [] =
  { "erl", "hrl", NULL };
static const char Erlang_help [] =
"In Erlang code, the tags are the functions, records and macros\n\
defined in the file.";

const char *Forth_suffixes [] =
  { "fth", "tok", NULL };
static const char Forth_help [] =
"In Forth code, tags are words defined by `:',\n\
constant, code, create, defer, value, variable, buffer:, field.";

static const char *Fortran_suffixes [] =
  { "F", "f", "f90", "for", NULL };
static const char Fortran_help [] =
"In Fortran code, functions, subroutines and block data are tags.";

static const char *HTML_suffixes [] =
  { "htm", "html", "shtml", NULL };
static const char HTML_help [] =
"In HTML input files, the tags are the `title' and the `h1', `h2',\n\
`h3' headers.  Also, tags are `name=' in anchors and all\n\
occurrences of `id='.";

static const char *Lisp_suffixes [] =
  { "cl", "clisp", "el", "l", "lisp", "LSP", "lsp", "ml", NULL };
static const char Lisp_help [] =
"In Lisp code, any function defined with `defun', any variable\n\
defined with `defvar' or `defconst', and in general the first\n\
argument of any expression that starts with `(def' in column zero\n\
is a tag.";

static const char *Lua_suffixes [] =
  { "lua", "LUA", NULL };
static const char Lua_help [] =
"In Lua scripts, all functions are tags.";

static const char *Makefile_filenames [] =
  { "Makefile", "makefile", "GNUMakefile", "Makefile.in", "Makefile.am", NULL};
static const char Makefile_help [] =
"In makefiles, targets are tags; additionally, variables are tags\n\
unless you specify `--no-globals'.";

static const char *Objc_suffixes [] =
  { "lm",			/* Objective lex file */
    "m",			/* Objective C file */
     NULL };
static const char Objc_help [] =
"In Objective C code, tags include Objective C definitions for classes,\n\
class categories, methods and protocols.  Tags for variables and\n\
functions in classes are named `CLASS::VARIABLE' and `CLASS::FUNCTION'.\n\
(Use --help --lang=c --lang=objc --lang=java for full help.)";

static const char *Pascal_suffixes [] =
  { "p", "pas", NULL };
static const char Pascal_help [] =
"In Pascal code, the tags are the functions and procedures defined\n\
in the file.";
/* " // this is for working around an Emacs highlighting bug... */

static const char *Perl_suffixes [] =
  { "pl", "pm", NULL };
static const char *Perl_interpreters [] =
  { "perl", "@PERL@", NULL };
static const char Perl_help [] =
"In Perl code, the tags are the packages, subroutines and variables\n\
defined by the `package', `sub', `my' and `local' keywords.  Use\n\
`--globals' if you want to tag global variables.  Tags for\n\
subroutines are named `PACKAGE::SUB'.  The name for subroutines\n\
defined in the default package is `main::SUB'.";

static const char *PHP_suffixes [] =
  { "php", "php3", "php4", NULL };
static const char PHP_help [] =
"In PHP code, tags are functions, classes and defines.  Unless you use\n\
the `--no-members' option, vars are tags too.";

static const char *plain_C_suffixes [] =
  { "pc",			/* Pro*C file */
     NULL };

static const char *PS_suffixes [] =
  { "ps", "psw", NULL };	/* .psw is for PSWrap */
static const char PS_help [] =
"In PostScript code, the tags are the functions.";

static const char *Prolog_suffixes [] =
  { "prolog", NULL };
static const char Prolog_help [] =
"In Prolog code, tags are predicates and rules at the beginning of\n\
line.";

static const char *Python_suffixes [] =
  { "py", NULL };
static const char Python_help [] =
"In Python code, `def' or `class' at the beginning of a line\n\
generate a tag.";

/* Can't do the `SCM' or `scm' prefix with a version number. */
static const char *Scheme_suffixes [] =
  { "oak", "sch", "scheme", "SCM", "scm", "SM", "sm", "ss", "t", NULL };
static const char Scheme_help [] =
"In Scheme code, tags include anything defined with `def' or with a\n\
construct whose name starts with `def'.  They also include\n\
variables set with `set!' at top level in the file.";

static const char *TeX_suffixes [] =
  { "bib", "clo", "cls", "ltx", "sty", "TeX", "tex", NULL };
static const char TeX_help [] =
"In LaTeX text, the argument of any of the commands `\\chapter',\n\
`\\section', `\\subsection', `\\subsubsection', `\\eqno', `\\label',\n\
`\\ref', `\\cite', `\\bibitem', `\\part', `\\appendix', `\\entry',\n\
`\\index', `\\def', `\\newcommand', `\\renewcommand',\n\
`\\newenvironment' or `\\renewenvironment' is a tag.\n\
\n\
Other commands can be specified by setting the environment variable\n\
`TEXTAGS' to a colon-separated list like, for example,\n\
     TEXTAGS=\"mycommand:myothercommand\".";


static const char *Texinfo_suffixes [] =
  { "texi", "texinfo", "txi", NULL };
static const char Texinfo_help [] =
"for texinfo files, lines starting with @node are tagged.";

static const char *Yacc_suffixes [] =
  { "y", "y++", "ym", "yxx", "yy", NULL }; /* .ym is Objective yacc file */
static const char Yacc_help [] =
"In Bison or Yacc input files, each rule defines as a tag the\n\
nonterminal it constructs.  The portions of the file that contain\n\
C code are parsed as C code (use --help --lang=c --lang=yacc\n\
for full help).";

static const char auto_help [] =
"`auto' is not a real language, it indicates to use\n\
a default language for files base on file name suffix and file contents.";

static const char none_help [] =
"`none' is not a real language, it indicates to only do\n\
regexp processing on files.";

static const char no_lang_help [] =
"No detailed help available for this language.";


/*
 * Table of languages.
 *
 * It is ok for a given function to be listed under more than one
 * name.  I just didn't.
 */

static language lang_names [] =
{
  { "ada",       Ada_help,       Ada_funcs,         Ada_suffixes       },
  { "asm",       Asm_help,       Asm_labels,        Asm_suffixes       },
  { "c",         default_C_help, default_C_entries, default_C_suffixes },
  { "c++",       Cplusplus_help, Cplusplus_entries, Cplusplus_suffixes },
  { "c*",        no_lang_help,   Cstar_entries,     Cstar_suffixes     },
  { "cobol",     Cobol_help,     Cobol_paragraphs,  Cobol_suffixes     },
  { "erlang",    Erlang_help,    Erlang_functions,  Erlang_suffixes    },
  { "forth",     Forth_help,     Forth_words,       Forth_suffixes     },
  { "fortran",   Fortran_help,   Fortran_functions, Fortran_suffixes   },
  { "html",      HTML_help,      HTML_labels,       HTML_suffixes      },
  { "java",      Cjava_help,     Cjava_entries,     Cjava_suffixes     },
  { "lisp",      Lisp_help,      Lisp_functions,    Lisp_suffixes      },
  { "lua",       Lua_help,       Lua_functions,     Lua_suffixes       },
  { "makefile",  Makefile_help,Makefile_targets,NULL,Makefile_filenames},
  { "objc",      Objc_help,      plain_C_entries,   Objc_suffixes      },
  { "pascal",    Pascal_help,    Pascal_functions,  Pascal_suffixes    },
  { "perl",Perl_help,Perl_functions,Perl_suffixes,NULL,Perl_interpreters},
  { "php",       PHP_help,       PHP_functions,     PHP_suffixes       },
  { "postscript",PS_help,        PS_functions,      PS_suffixes        },
  { "proc",      no_lang_help,   plain_C_entries,   plain_C_suffixes   },
  { "prolog",    Prolog_help,    Prolog_functions,  Prolog_suffixes    },
  { "python",    Python_help,    Python_functions,  Python_suffixes    },
  { "scheme",    Scheme_help,    Scheme_functions,  Scheme_suffixes    },
  { "tex",       TeX_help,       TeX_commands,      TeX_suffixes       },
  { "texinfo",   Texinfo_help,   Texinfo_nodes,     Texinfo_suffixes   },
  { "yacc",      Yacc_help,Yacc_entries,Yacc_suffixes,NULL,NULL,TRUE},
  { "auto",      auto_help },                      /* default guessing scheme */
  { "none",      none_help,      just_read_file }, /* regexp matching only */
  { NULL }                /* end of list */
};


static void
print_language_names (void)
{
  language *lang;
  const char **name, **ext;

  puts ("\nThese are the currently supported languages, along with the\n\
default file names and dot suffixes:");
  for (lang = lang_names; lang->name != NULL; lang++)
    {
      printf ("  %-*s", 10, lang->name);
      if (lang->filenames != NULL)
	for (name = lang->filenames; *name != NULL; name++)
	  printf (" %s", *name);
      if (lang->suffixes != NULL)
	for (ext = lang->suffixes; *ext != NULL; ext++)
	  printf (" .%s", *ext);
      puts ("");
    }
  puts ("where `auto' means use default language for files based on file\n\
name suffix, and `none' means only do regexp processing on files.\n\
If no language is specified and no matching suffix is found,\n\
the first line of the file is read for a sharp-bang (#!) sequence\n\
followed by the name of an interpreter.  If no such sequence is found,\n\
Fortran is tried first; if no tags are found, C is tried next.\n\
When parsing any C file, a \"class\" or \"template\" keyword\n\
switches to C++.");
  puts ("Compressed files are supported using gzip, bzip2, and xz.\n\
\n\
For detailed help on a given language use, for example,\n\
etags --help --lang=ada.");
}

#ifndef EMACS_NAME
# define EMACS_NAME "standalone"
#endif
#ifndef VERSION
# define VERSION "17.38.1.4"
#endif
static void
print_version (void)
{
  /* Makes it easier to update automatically. */
  char emacs_copyright[] = "Copyright (C) 2012 Free Software Foundation, Inc.";

  printf ("%s (%s %s)\n", (CTAGS) ? "ctags" : "etags", EMACS_NAME, VERSION);
  puts (emacs_copyright);
  puts ("This program is distributed under the terms in ETAGS.README");

  exit (EXIT_SUCCESS);
}

#ifndef PRINT_UNDOCUMENTED_OPTIONS_HELP
# define PRINT_UNDOCUMENTED_OPTIONS_HELP FALSE
#endif

static void
print_help (argument *argbuffer)
{
  bool help_for_lang = FALSE;

  for (; argbuffer->arg_type != at_end; argbuffer++)
    if (argbuffer->arg_type == at_language)
      {
	if (help_for_lang)
	  puts ("");
	puts (argbuffer->lang->help);
	help_for_lang = TRUE;
      }

  if (help_for_lang)
    exit (EXIT_SUCCESS);

  printf ("Usage: %s [options] [[regex-option ...] file-name] ...\n\
\n\
These are the options accepted by %s.\n", progname, progname);
  if (NO_LONG_OPTIONS)
    puts ("WARNING: long option names do not work with this executable,\n\
as it is not linked with GNU getopt.");
  else
    puts ("You may use unambiguous abbreviations for the long option names.");
  puts ("  A - as file name means read names from stdin (one per line).\n\
Absolute names are stored in the output file as they are.\n\
Relative ones are stored relative to the output file's directory.\n");

  puts ("-a, --append\n\
        Append tag entries to existing tags file.");

  puts ("--packages-only\n\
        For Ada files, only generate tags for packages.");

  if (CTAGS)
    puts ("-B, --backward-search\n\
        Write the search commands for the tag entries using '?', the\n\
        backward-search command instead of '/', the forward-search command.");

  /* This option is mostly obsolete, because etags can now automatically
     detect C++.  Retained for backward compatibility and for debugging and
     experimentation.  In principle, we could want to tag as C++ even
     before any "class" or "template" keyword.
  puts ("-C, --c++\n\
        Treat files whose name suffix defaults to C language as C++ files.");
  */

  puts ("--declarations\n\
	In C and derived languages, create tags for function declarations,");
  if (CTAGS)
    puts ("\tand create tags for extern variables if --globals is used.");
  else
    puts
      ("\tand create tags for extern variables unless --no-globals is used.");

  if (CTAGS)
    puts ("-d, --defines\n\
        Create tag entries for C #define constants and enum constants, too.");
  else
    puts ("-D, --no-defines\n\
        Don't create tag entries for C #define constants and enum constants.\n\
	This makes the tags file smaller.");

  if (!CTAGS)
    puts ("-i FILE, --include=FILE\n\
        Include a note in tag file indicating that, when searching for\n\
        a tag, one should also consult the tags file FILE after\n\
        checking the current file.");

  puts ("-l LANG, --language=LANG\n\
        Force the following files to be considered as written in the\n\
	named language up to the next --language=LANG option.");

  if (CTAGS)
    puts ("--globals\n\
	Create tag entries for global variables in some languages.");
  else
    puts ("--no-globals\n\
	Do not create tag entries for global variables in some\n\
	languages.  This makes the tags file smaller.");

  if (PRINT_UNDOCUMENTED_OPTIONS_HELP)
    puts ("--no-line-directive\n\
        Ignore #line preprocessor directives in C and derived languages.");

  if (CTAGS)
    puts ("--members\n\
	Create tag entries for members of structures in some languages.");
  else
    puts ("--no-members\n\
	Do not create tag entries for members of structures\n\
	in some languages.");

  puts ("-r REGEXP, --regex=REGEXP or --regex=@regexfile\n\
        Make a tag for each line matching a regular expression pattern\n\
	in the following files.  {LANGUAGE}REGEXP uses REGEXP for LANGUAGE\n\
	files only.  REGEXFILE is a file containing one REGEXP per line.\n\
	REGEXP takes the form /TAGREGEXP/TAGNAME/MODS, where TAGNAME/ is\n\
	optional.  The TAGREGEXP pattern is anchored (as if preceded by ^).");
  puts ("	If TAGNAME/ is present, the tags created are named.\n\
	For example Tcl named tags can be created with:\n\
	  --regex=\"/proc[ \\t]+\\([^ \\t]+\\)/\\1/.\".\n\
	MODS are optional one-letter modifiers: `i' means to ignore case,\n\
	`m' means to allow multi-line matches, `s' implies `m' and\n\
	causes dot to match any character, including newline.");

  puts ("-R, --no-regex\n\
        Don't create tags from regexps for the following files.");

  puts ("-I, --ignore-indentation\n\
        In C and C++ do not assume that a closing brace in the first\n\
        column is the final brace of a function or structure definition.");

  puts ("-o FILE, --output=FILE\n\
        Write the tags to FILE.");

  puts ("--parse-stdin=NAME\n\
        Read from standard input and record tags as belonging to file NAME.");

  if (CTAGS)
    {
      puts ("-t, --typedefs\n\
        Generate tag entries for C and Ada typedefs.");
      puts ("-T, --typedefs-and-c++\n\
        Generate tag entries for C typedefs, C struct/enum/union tags,\n\
        and C++ member functions.");
    }

  if (CTAGS)
    puts ("-u, --update\n\
        Update the tag entries for the given files, leaving tag\n\
        entries for other files in place.  Currently, this is\n\
        implemented by deleting the existing entries for the given\n\
        files and then rewriting the new entries at the end of the\n\
        tags file.  It is often faster to simply rebuild the entire\n\
        tag file than to use this.");

  if (CTAGS)
    {
      puts ("-v, --vgrind\n\
        Print on the standard output an index of items intended for\n\
        human consumption, similar to the output of vgrind.  The index\n\
        is sorted, and gives the page number of each item.");

      if (PRINT_UNDOCUMENTED_OPTIONS_HELP)
	puts ("-w, --no-duplicates\n\
        Do not create duplicate tag entries, for compatibility with\n\
	traditional ctags.");

      if (PRINT_UNDOCUMENTED_OPTIONS_HELP)
	puts ("-w, --no-warn\n\
        Suppress warning messages about duplicate tag entries.");

      puts ("-x, --cxref\n\
        Like --vgrind, but in the style of cxref, rather than vgrind.\n\
        The output uses line numbers instead of page numbers, but\n\
        beyond that the differences are cosmetic; try both to see\n\
        which you like.");
    }

  puts ("-V, --version\n\
        Print the version of the program.\n\
-h, --help\n\
        Print this help message.\n\
        Followed by one or more `--language' options prints detailed\n\
        help about tag generation for the specified languages.");

  print_language_names ();

  puts ("");
  puts ("Report bugs to bug-gnu-emacs@gnu.org");

  exit (EXIT_SUCCESS);
}


int
main (int argc, char **argv)
{
  int i;
  unsigned int nincluded_files;
  char **included_files;
  argument *argbuffer;
  int current_arg, file_count;
  linebuffer filename_lb;
  bool help_asked = FALSE;
  ptrdiff_t len;
 char *optstring;
 int opt;


#ifdef DOS_NT
  _fmode = O_BINARY;   /* all of files are treated as binary files */
#endif /* DOS_NT */

  progname = argv[0];
  nincluded_files = 0;
  included_files = xnew (argc, char *);
  current_arg = 0;
  file_count = 0;

  /* Allocate enough no matter what happens.  Overkill, but each one
     is small. */
  argbuffer = xnew (argc, argument);

  /*
   * Always find typedefs and structure tags.
   * Also default to find macro constants, enum constants, struct
   * members and global variables.  Do it for both etags and ctags.
   */
  typedefs = typedefs_or_cplusplus = constantypedefs = TRUE;
  globals = members = TRUE;

  /* When the optstring begins with a '-' getopt_long does not rearrange the
     non-options arguments to be at the end, but leaves them alone. */
  optstring = concat (NO_LONG_OPTIONS ? "" : "-",
		      "ac:Cf:Il:o:r:RSVhH",
		      (CTAGS) ? "BxdtTuvw" : "Di:");

  while ((opt = getopt_long (argc, argv, optstring, longopts, NULL)) != EOF)
    switch (opt)
      {
      case 0:
	/* If getopt returns 0, then it has already processed a
	   long-named option.  We should do nothing.  */
	break;

      case 1:
	/* This means that a file name has been seen.  Record it. */
	argbuffer[current_arg].arg_type = at_filename;
	argbuffer[current_arg].what     = optarg;
	len = strlen (optarg);
	if (whatlen_max < len)
	  whatlen_max = len;
	++current_arg;
	++file_count;
	break;

      case STDIN:
	/* Parse standard input.  Idea by Vivek <vivek@etla.org>. */
	argbuffer[current_arg].arg_type = at_stdin;
	argbuffer[current_arg].what     = optarg;
	len = strlen (optarg);
	if (whatlen_max < len)
	  whatlen_max = len;
	++current_arg;
	++file_count;
	if (parsing_stdin)
	  fatal ("cannot parse standard input more than once", (char *)NULL);
	parsing_stdin = TRUE;
	break;

	/* Common options. */
      case 'a': append_to_tagfile = TRUE;	break;
      case 'C': cplusplus = TRUE;		break;
      case 'f':		/* for compatibility with old makefiles */
      case 'o':
	if (tagfile)
	  {
	    error ("-o option may only be given once.", (char *)NULL);
	    suggest_asking_for_help ();
	    /* NOTREACHED */
	  }
	tagfile = optarg;
	break;
      case 'I':
      case 'S':		/* for backward compatibility */
	ignoreindent = TRUE;
	break;
      case 'l':
	{
	  language *lang = get_language_from_langname (optarg);
	  if (lang != NULL)
	    {
	      argbuffer[current_arg].lang = lang;
	      argbuffer[current_arg].arg_type = at_language;
	      ++current_arg;
	    }
	}
	break;
      case 'c':
	/* Backward compatibility: support obsolete --ignore-case-regexp. */
	optarg = concat (optarg, "i", ""); /* memory leak here */
	/* FALLTHRU */
      case 'r':
	argbuffer[current_arg].arg_type = at_regexp;
	argbuffer[current_arg].what = optarg;
	len = strlen (optarg);
	if (whatlen_max < len)
	  whatlen_max = len;
	++current_arg;
	break;
      case 'R':
	argbuffer[current_arg].arg_type = at_regexp;
	argbuffer[current_arg].what = NULL;
	++current_arg;
	break;
      case 'V':
	print_version ();
	break;
      case 'h':
      case 'H':
	help_asked = TRUE;
	break;

	/* Etags options */
      case 'D': constantypedefs = FALSE;			break;
      case 'i': included_files[nincluded_files++] = optarg;	break;

	/* Ctags options. */
      case 'B': searchar = '?';					break;
      case 'd': constantypedefs = TRUE;				break;
      case 't': typedefs = TRUE;				break;
      case 'T': typedefs = typedefs_or_cplusplus = TRUE;	break;
      case 'u': update = TRUE;					break;
      case 'v': vgrind_style = TRUE;			  /*FALLTHRU*/
      case 'x': cxref_style = TRUE;				break;
      case 'w': no_warnings = TRUE;				break;
      default:
	suggest_asking_for_help ();
	/* NOTREACHED */
      }

  /* No more options.  Store the rest of arguments. */
  for (; optind < argc; optind++)
    {
      argbuffer[current_arg].arg_type = at_filename;
      argbuffer[current_arg].what = argv[optind];
      len = strlen (argv[optind]);
      if (whatlen_max < len)
	whatlen_max = len;
      ++current_arg;
      ++file_count;
    }

  argbuffer[current_arg].arg_type = at_end;

  if (help_asked)
    print_help (argbuffer);
    /* NOTREACHED */

  if (nincluded_files == 0 && file_count == 0)
    {
      error ("no input files specified.", (char *)NULL);
      suggest_asking_for_help ();
      /* NOTREACHED */
    }

  if (tagfile == NULL)
    tagfile = savestr (CTAGS ? "tags" : "TAGS");
  cwd = etags_getcwd ();	/* the current working directory */
  if (cwd[strlen (cwd) - 1] != '/')
    {
      char *oldcwd = cwd;
      cwd = concat (oldcwd, "/", "");
      free (oldcwd);
    }

  /* Compute base directory for relative file names. */
  if (streq (tagfile, "-")
      || strneq (tagfile, "/dev/", 5))
    tagfiledir = cwd;		 /* relative file names are relative to cwd */
  else
    {
      canonicalize_filename (tagfile);
      tagfiledir = absolute_dirname (tagfile, cwd);
    }

  init ();			/* set up boolean "functions" */

  linebuffer_init (&lb);
  linebuffer_init (&filename_lb);
  linebuffer_init (&filebuf);
  linebuffer_init (&token_name);

  if (!CTAGS)
    {
      if (streq (tagfile, "-"))
	{
	  tagf = stdout;
#ifdef DOS_NT
	  /* Switch redirected `stdout' to binary mode (setting `_fmode'
	     doesn't take effect until after `stdout' is already open). */
	  if (!isatty (fileno (stdout)))
	    setmode (fileno (stdout), O_BINARY);
#endif /* DOS_NT */
	}
      else
	tagf = fopen (tagfile, append_to_tagfile ? "a" : "w");
      if (tagf == NULL)
	pfatal (tagfile);
    }

  /*
   * Loop through files finding functions.
   */
  for (i = 0; i < current_arg; i++)
    {
      static language *lang;	/* non-NULL if language is forced */
      char *this_file;

      switch (argbuffer[i].arg_type)
	{
	case at_language:
	  lang = argbuffer[i].lang;
	  break;
	case at_regexp:
	  analyse_regex (argbuffer[i].what);
	  break;
	case at_filename:
	      this_file = argbuffer[i].what;
	      /* Input file named "-" means read file names from stdin
		 (one per line) and use them. */
	      if (streq (this_file, "-"))
		{
		  if (parsing_stdin)
		    fatal ("cannot parse standard input AND read file names from it",
			   (char *)NULL);
		  while (readline_internal (&filename_lb, stdin) > 0)
		    process_file_name (filename_lb.buffer, lang);
		}
	      else
		process_file_name (this_file, lang);
	  break;
        case at_stdin:
          this_file = argbuffer[i].what;
          process_file (stdin, this_file, lang);
          break;
	}
    }

  free_regexps ();
  free (lb.buffer);
  free (filebuf.buffer);
  free (token_name.buffer);

  if (!CTAGS || cxref_style)
    {
      /* Write the remaining tags to tagf (ETAGS) or stdout (CXREF). */
      put_entries (nodehead);
      free_tree (nodehead);
      nodehead = NULL;
      if (!CTAGS)
	{
	  fdesc *fdp;

	  /* Output file entries that have no tags. */
	  for (fdp = fdhead; fdp != NULL; fdp = fdp->next)
	    if (!fdp->written)
	      fprintf (tagf, "\f\n%s,0\n", fdp->taggedfname);

	  while (nincluded_files-- > 0)
	    fprintf (tagf, "\f\n%s,include\n", *included_files++);

	  if (fclose (tagf) == EOF)
	    pfatal (tagfile);
	}

      exit (EXIT_SUCCESS);
    }

  /* From here on, we are in (CTAGS && !cxref_style) */
  if (update)
    {
      char *cmd =
	xmalloc (strlen (tagfile) + whatlen_max +
		 sizeof "mv..OTAGS;fgrep -v '\t\t' OTAGS >;rm OTAGS");
      for (i = 0; i < current_arg; ++i)
	{
	  switch (argbuffer[i].arg_type)
	    {
	    case at_filename:
	    case at_stdin:
	      break;
	    default:
	      continue;		/* the for loop */
	    }
	  strcpy (cmd, "mv ");
	  strcat (cmd, tagfile);
	  strcat (cmd, " OTAGS;fgrep -v '\t");
	  strcat (cmd, argbuffer[i].what);
	  strcat (cmd, "\t' OTAGS >");
	  strcat (cmd, tagfile);
	  strcat (cmd, ";rm OTAGS");
	  if (system (cmd) != EXIT_SUCCESS)
	    fatal ("failed to execute shell command", (char *)NULL);
	}
      free (cmd);
      append_to_tagfile = TRUE;
    }

  tagf = fopen (tagfile, append_to_tagfile ? "a" : "w");
  if (tagf == NULL)
    pfatal (tagfile);
  put_entries (nodehead);	/* write all the tags (CTAGS) */
  free_tree (nodehead);
  nodehead = NULL;
  if (fclose (tagf) == EOF)
    pfatal (tagfile);

  if (CTAGS)
    if (append_to_tagfile || update)
      {
	char *cmd = xmalloc (2 * strlen (tagfile) + sizeof "sort -u -o..");
	/* Maybe these should be used:
	   setenv ("LC_COLLATE", "C", 1);
	   setenv ("LC_ALL", "C", 1); */
	strcpy (cmd, "sort -u -o ");
	strcat (cmd, tagfile);
	strcat (cmd, " ");
	strcat (cmd, tagfile);
	exit (system (cmd));
      }
  return EXIT_SUCCESS;
}


/*
 * Return a compressor given the file name.  If EXTPTR is non-zero,
 * return a pointer into FILE where the compressor-specific
 * extension begins.  If no compressor is found, NULL is returned
 * and EXTPTR is not significant.
 * Idea by Vladimir Alexiev <vladimir@cs.ualberta.ca> (1998)
 */
static compressor *
get_compressor_from_suffix (char *file, char **extptr)
{
  compressor *compr;
  char *slash, *suffix;

  /* File has been processed by canonicalize_filename,
     so we don't need to consider backslashes on DOS_NT.  */
  slash = etags_strrchr (file, '/');
  suffix = etags_strrchr (file, '.');
  if (suffix == NULL || suffix < slash)
    return NULL;
  if (extptr != NULL)
    *extptr = suffix;
  suffix += 1;
  /* Let those poor souls who live with DOS 8+3 file name limits get
     some solace by treating foo.cgz as if it were foo.c.gz, etc.
     Only the first do loop is run if not MSDOS */
  do
    {
      for (compr = compressors; compr->suffix != NULL; compr++)
	if (streq (compr->suffix, suffix))
	  return compr;
      if (!MSDOS)
	break;			/* do it only once: not really a loop */
      if (extptr != NULL)
	*extptr = ++suffix;
    } while (*suffix != '\0');
  return NULL;
}



/*
 * Return a language given the name.
 */
static language *
get_language_from_langname (const char *name)
{
  language *lang;

  if (name == NULL)
    error ("empty language name", (char *)NULL);
  else
    {
      for (lang = lang_names; lang->name != NULL; lang++)
	if (streq (name, lang->name))
	  return lang;
      error ("unknown language \"%s\"", name);
    }

  return NULL;
}


/*
 * Return a language given the interpreter name.
 */
static language *
get_language_from_interpreter (char *interpreter)
{
  language *lang;
  const char **iname;

  if (interpreter == NULL)
    return NULL;
  for (lang = lang_names; lang->name != NULL; lang++)
    if (lang->interpreters != NULL)
      for (iname = lang->interpreters; *iname != NULL; iname++)
	if (streq (*iname, interpreter))
	    return lang;

  return NULL;
}



/*
 * Return a language given the file name.
 */
static language *
get_language_from_filename (char *file, int case_sensitive)
{
  language *lang;
  const char **name, **ext, *suffix;

  /* Try whole file name first. */
  for (lang = lang_names; lang->name != NULL; lang++)
    if (lang->filenames != NULL)
      for (name = lang->filenames; *name != NULL; name++)
	if ((case_sensitive)
	    ? streq (*name, file)
	    : strcaseeq (*name, file))
	  return lang;

  /* If not found, try suffix after last dot. */
  suffix = etags_strrchr (file, '.');
  if (suffix == NULL)
    return NULL;
  suffix += 1;
  for (lang = lang_names; lang->name != NULL; lang++)
    if (lang->suffixes != NULL)
      for (ext = lang->suffixes; *ext != NULL; ext++)
	if ((case_sensitive)
	    ? streq (*ext, suffix)
	    : strcaseeq (*ext, suffix))
	  return lang;
  return NULL;
}


/*
 * This routine is called on each file argument.
 */
static void
process_file_name (char *file, language *lang)
{
  struct stat stat_buf;
  FILE *inf;
  fdesc *fdp;
  compressor *compr;
  char *compressed_name, *uncompressed_name;
  char *ext, *real_name;
  int retval;

  canonicalize_filename (file);
  if (streq (file, tagfile) && !streq (tagfile, "-"))
    {
      error ("skipping inclusion of %s in self.", file);
      return;
    }
  if ((compr = get_compressor_from_suffix (file, &ext)) == NULL)
    {
      compressed_name = NULL;
      real_name = uncompressed_name = savestr (file);
    }
  else
    {
      real_name = compressed_name = savestr (file);
      uncompressed_name = savenstr (file, ext - file);
    }

  /* If the canonicalized uncompressed name
     has already been dealt with, skip it silently. */
  for (fdp = fdhead; fdp != NULL; fdp = fdp->next)
    {
      assert (fdp->infname != NULL);
      if (streq (uncompressed_name, fdp->infname))
	goto cleanup;
    }

  if (stat (real_name, &stat_buf) != 0)
    {
      /* Reset real_name and try with a different name. */
      real_name = NULL;
      if (compressed_name != NULL) /* try with the given suffix */
	{
	  if (stat (uncompressed_name, &stat_buf) == 0)
	    real_name = uncompressed_name;
	}
      else			/* try all possible suffixes */
	{
	  for (compr = compressors; compr->suffix != NULL; compr++)
	    {
	      compressed_name = concat (file, ".", compr->suffix);
	      if (stat (compressed_name, &stat_buf) != 0)
		{
		  if (MSDOS)
		    {
		      char *suf = compressed_name + strlen (file);
		      size_t suflen = strlen (compr->suffix) + 1;
		      for ( ; suf[1]; suf++, suflen--)
			{
			  memmove (suf, suf + 1, suflen);
			  if (stat (compressed_name, &stat_buf) == 0)
			    {
			      real_name = compressed_name;
			      break;
			    }
			}
		      if (real_name != NULL)
			break;
		    } /* MSDOS */
		  free (compressed_name);
		  compressed_name = NULL;
		}
	      else
		{
		  real_name = compressed_name;
		  break;
		}
	    }
	}
      if (real_name == NULL)
	{
	  perror (file);
	  goto cleanup;
	}
    } /* try with a different name */

  if (!S_ISREG (stat_buf.st_mode))
    {
      error ("skipping %s: it is not a regular file.", real_name);
      goto cleanup;
    }
  if (real_name == compressed_name)
    {
      char *cmd = concat (compr->command, " ", real_name);
      inf = (FILE *) popen (cmd, "r");
      free (cmd);
    }
  else
    inf = fopen (real_name, "r");
  if (inf == NULL)
    {
      perror (real_name);
      goto cleanup;
    }

  process_file (inf, uncompressed_name, lang);

  if (real_name == compressed_name)
    retval = pclose (inf);
  else
    retval = fclose (inf);
  if (retval < 0)
    pfatal (file);

 cleanup:
  free (compressed_name);
  free (uncompressed_name);
  last_node = NULL;
  curfdp = NULL;
  return;
}

static void
process_file (FILE *fh, char *fn, language *lang)
{
  static const fdesc emptyfdesc;
  fdesc *fdp;

  /* Create a new input file description entry. */
  fdp = xnew (1, fdesc);
  *fdp = emptyfdesc;
  fdp->next = fdhead;
  fdp->infname = savestr (fn);
  fdp->lang = lang;
  fdp->infabsname = absolute_filename (fn, cwd);
  fdp->infabsdir = absolute_dirname (fn, cwd);
  if (filename_is_absolute (fn))
    {
      /* An absolute file name.  Canonicalize it. */
      fdp->taggedfname = absolute_filename (fn, NULL);
    }
  else
    {
      /* A file name relative to cwd.  Make it relative
	 to the directory of the tags file. */
      fdp->taggedfname = relative_filename (fn, tagfiledir);
    }
  fdp->usecharno = TRUE;	/* use char position when making tags */
  fdp->prop = NULL;
  fdp->written = FALSE;		/* not written on tags file yet */

  fdhead = fdp;
  curfdp = fdhead;		/* the current file description */

  find_entries (fh);

  /* If not Ctags, and if this is not metasource and if it contained no #line
     directives, we can write the tags and free all nodes pointing to
     curfdp. */
  if (!CTAGS
      && curfdp->usecharno	/* no #line directives in this file */
      && !curfdp->lang->metasource)
    {
      node *np, *prev;

      /* Look for the head of the sublist relative to this file.  See add_node
	 for the structure of the node tree. */
      prev = NULL;
      for (np = nodehead; np != NULL; prev = np, np = np->left)
	if (np->fdp == curfdp)
	  break;

      /* If we generated tags for this file, write and delete them. */
      if (np != NULL)
	{
	  /* This is the head of the last sublist, if any.  The following
	     instructions depend on this being true. */
	  assert (np->left == NULL);

	  assert (fdhead == curfdp);
	  assert (last_node->fdp == curfdp);
	  put_entries (np);	/* write tags for file curfdp->taggedfname */
	  free_tree (np);	/* remove the written nodes */
	  if (prev == NULL)
	    nodehead = NULL;	/* no nodes left */
	  else
	    prev->left = NULL;	/* delete the pointer to the sublist */
	}
    }
}

/*
 * This routine sets up the boolean pseudo-functions which work
 * by setting boolean flags dependent upon the corresponding character.
 * Every char which is NOT in that string is not a white char.  Therefore,
 * all of the array "_wht" is set to FALSE, and then the elements
 * subscripted by the chars in "white" are set to TRUE.  Thus "_wht"
 * of a char is TRUE if it is the string "white", else FALSE.
 */
static void
init (void)
{
  register const char *sp;
  register int i;

  for (i = 0; i < CHARS; i++)
    iswhite (i) = notinname (i) = begtoken (i) = intoken (i) = endtoken (i) = FALSE;
  for (sp = white; *sp != '\0'; sp++) iswhite (*sp) = TRUE;
  for (sp = nonam; *sp != '\0'; sp++) notinname (*sp) = TRUE;
  notinname ('\0') = notinname ('\n');
  for (sp = begtk; *sp != '\0'; sp++) begtoken (*sp) = TRUE;
  begtoken ('\0') = begtoken ('\n');
  for (sp = midtk; *sp != '\0'; sp++) intoken (*sp) = TRUE;
  intoken ('\0') = intoken ('\n');
  for (sp = endtk; *sp != '\0'; sp++) endtoken (*sp) = TRUE;
  endtoken ('\0') = endtoken ('\n');
}

/*
 * This routine opens the specified file and calls the function
 * which finds the function and type definitions.
 */
static void
find_entries (FILE *inf)
{
  char *cp;
  language *lang = curfdp->lang;
  Lang_function *parser = NULL;

  /* If user specified a language, use it. */
  if (lang != NULL && lang->function != NULL)
    {
      parser = lang->function;
    }

  /* Else try to guess the language given the file name. */
  if (parser == NULL)
    {
      lang = get_language_from_filename (curfdp->infname, TRUE);
      if (lang != NULL && lang->function != NULL)
	{
	  curfdp->lang = lang;
	  parser = lang->function;
	}
    }

  /* Else look for sharp-bang as the first two characters. */
  if (parser == NULL
      && readline_internal (&lb, inf) > 0
      && lb.len >= 2
      && lb.buffer[0] == '#'
      && lb.buffer[1] == '!')
    {
      char *lp;

      /* Set lp to point at the first char after the last slash in the
         line or, if no slashes, at the first nonblank.  Then set cp to
	 the first successive blank and terminate the string. */
      lp = etags_strrchr (lb.buffer+2, '/');
      if (lp != NULL)
	lp += 1;
      else
	lp = skip_spaces (lb.buffer + 2);
      cp = skip_non_spaces (lp);
      *cp = '\0';

      if (strlen (lp) > 0)
	{
	  lang = get_language_from_interpreter (lp);
	  if (lang != NULL && lang->function != NULL)
	    {
	      curfdp->lang = lang;
	      parser = lang->function;
	    }
	}
    }

  /* We rewind here, even if inf may be a pipe.  We fail if the
     length of the first line is longer than the pipe block size,
     which is unlikely. */
  rewind (inf);

  /* Else try to guess the language given the case insensitive file name. */
  if (parser == NULL)
    {
      lang = get_language_from_filename (curfdp->infname, FALSE);
      if (lang != NULL && lang->function != NULL)
	{
	  curfdp->lang = lang;
	  parser = lang->function;
	}
    }

  /* Else try Fortran or C. */
  if (parser == NULL)
    {
      node *old_last_node = last_node;

      curfdp->lang = get_language_from_langname ("fortran");
      find_entries (inf);

      if (old_last_node == last_node)
	/* No Fortran entries found.  Try C. */
	{
	  /* We do not tag if rewind fails.
	     Only the file name will be recorded in the tags file. */
	  rewind (inf);
	  curfdp->lang = get_language_from_langname (cplusplus ? "c++" : "c");
	  find_entries (inf);
	}
      return;
    }

  if (!no_line_directive
      && curfdp->lang != NULL && curfdp->lang->metasource)
    /* It may be that this is a bingo.y file, and we already parsed a bingo.c
       file, or anyway we parsed a file that is automatically generated from
       this one.  If this is the case, the bingo.c file contained #line
       directives that generated tags pointing to this file.  Let's delete
       them all before parsing this file, which is the real source. */
    {
      fdesc **fdpp = &fdhead;
      while (*fdpp != NULL)
	if (*fdpp != curfdp
	    && streq ((*fdpp)->taggedfname, curfdp->taggedfname))
	  /* We found one of those!  We must delete both the file description
	     and all tags referring to it. */
	  {
	    fdesc *badfdp = *fdpp;

	    /* Delete the tags referring to badfdp->taggedfname
	       that were obtained from badfdp->infname. */
	    invalidate_nodes (badfdp, &nodehead);

	    *fdpp = badfdp->next; /* remove the bad description from the list */
	    free_fdesc (badfdp);
	  }
	else
	  fdpp = &(*fdpp)->next; /* advance the list pointer */
    }

  assert (parser != NULL);

  /* Generic initializations before reading from file. */
  linebuffer_setlen (&filebuf, 0); /* reset the file buffer */

  /* Generic initializations before parsing file with readline. */
  lineno = 0;		       /* reset global line number */
  charno = 0;		       /* reset global char number */
  linecharno = 0;	       /* reset global char number of line start */

  parser (inf);

  regex_tag_multiline ();
}


/*
 * Check whether an implicitly named tag should be created,
 * then call `pfnote'.
 * NAME is a string that is internally copied by this function.
 *
 * TAGS format specification
 * Idea by Sam Kendall <kendall@mv.mv.com> (1997)
 * The following is explained in some more detail in etc/ETAGS.EBNF.
 *
 * make_tag creates tags with "implicit tag names" (unnamed tags)
 * if the following are all true, assuming NONAM=" \f\t\n\r()=,;":
 *  1. NAME does not contain any of the characters in NONAM;
 *  2. LINESTART contains name as either a rightmost, or rightmost but
 *     one character, substring;
 *  3. the character, if any, immediately before NAME in LINESTART must
 *     be a character in NONAM;
 *  4. the character, if any, immediately after NAME in LINESTART must
 *     also be a character in NONAM.
 *
 * The implementation uses the notinname() macro, which recognizes the
 * characters stored in the string `nonam'.
 * etags.el needs to use the same characters that are in NONAM.
 */
static void
make_tag (const char *name, 	/* tag name, or NULL if unnamed */
	  int namelen,		/* tag length */
	  int is_func,		/* tag is a function */
	  char *linestart,	/* start of the line where tag is */
	  int linelen,          /* length of the line where tag is */
	  int lno,		/* line number */
	  long int cno)		/* character number */
{
  bool named = (name != NULL && namelen > 0);
  char *nname = NULL;

  if (!CTAGS && named)		/* maybe set named to false */
    /* Let's try to make an implicit tag name, that is, create an unnamed tag
       such that etags.el can guess a name from it. */
    {
      int i;
      register const char *cp = name;

      for (i = 0; i < namelen; i++)
	if (notinname (*cp++))
	  break;
      if (i == namelen)				/* rule #1 */
	{
	  cp = linestart + linelen - namelen;
	  if (notinname (linestart[linelen-1]))
	    cp -= 1;				/* rule #4 */
	  if (cp >= linestart			/* rule #2 */
	      && (cp == linestart
		  || notinname (cp[-1]))	/* rule #3 */
	      && strneq (name, cp, namelen))	/* rule #2 */
	    named = FALSE;	/* use implicit tag name */
	}
    }

  if (named)
    nname = savenstr (name, namelen);

  pfnote (nname, is_func, linestart, linelen, lno, cno);
}

/* Record a tag. */
static void
pfnote (char *name, int is_func, char *linestart, int linelen, int lno, long int cno)
                		/* tag name, or NULL if unnamed */
                  		/* tag is a function */
                     		/* start of the line where tag is */
                 		/* length of the line where tag is */
             			/* line number */
              			/* character number */
{
  register node *np;

  assert (name == NULL || name[0] != '\0');
  if (CTAGS && name == NULL)
    return;

  np = xnew (1, node);

  /* If ctags mode, change name "main" to M<thisfilename>. */
  if (CTAGS && !cxref_style && streq (name, "main"))
    {
      register char *fp = etags_strrchr (curfdp->taggedfname, '/');
      np->name = concat ("M", fp == NULL ? curfdp->taggedfname : fp + 1, "");
      fp = etags_strrchr (np->name, '.');
      if (fp != NULL && fp[1] != '\0' && fp[2] == '\0')
	fp[0] = '\0';
    }
  else
    np->name = name;
  np->valid = TRUE;
  np->been_warned = FALSE;
  np->fdp = curfdp;
  np->is_func = is_func;
  np->lno = lno;
  if (np->fdp->usecharno)
    /* Our char numbers are 0-base, because of C language tradition?
       ctags compatibility?  old versions compatibility?   I don't know.
       Anyway, since emacs's are 1-base we expect etags.el to take care
       of the difference.  If we wanted to have 1-based numbers, we would
       uncomment the +1 below. */
    np->cno = cno /* + 1 */ ;
  else
    np->cno = invalidcharno;
  np->left = np->right = NULL;
  if (CTAGS && !cxref_style)
    {
      if (strlen (linestart) < 50)
	np->regex = concat (linestart, "$", "");
      else
	np->regex = savenstr (linestart, 50);
    }
  else
    np->regex = savenstr (linestart, linelen);

  add_node (np, &nodehead);
}

/*
 * free_tree ()
 *	recurse on left children, iterate on right children.
 */
static void
free_tree (register node *np)
{
  while (np)
    {
      register node *node_right = np->right;
      free_tree (np->left);
      free (np->name);
      free (np->regex);
      free (np);
      np = node_right;
    }
}

/*
 * free_fdesc ()
 *	delete a file description
 */
static void
free_fdesc (register fdesc *fdp)
{
  free (fdp->infname);
  free (fdp->infabsname);
  free (fdp->infabsdir);
  free (fdp->taggedfname);
  free (fdp->prop);
  free (fdp);
}

/*
 * add_node ()
 *	Adds a node to the tree of nodes.  In etags mode, sort by file
 *  	name.  In ctags mode, sort by tag name.  Make no attempt at
 *    	balancing.
 *
 *	add_node is the only function allowed to add nodes, so it can
 *	maintain state.
 */
static void
add_node (node *np, node **cur_node_p)
{
  register int dif;
  register node *cur_node = *cur_node_p;

  if (cur_node == NULL)
    {
      *cur_node_p = np;
      last_node = np;
      return;
    }

  if (!CTAGS)
    /* Etags Mode */
    {
      /* For each file name, tags are in a linked sublist on the right
	 pointer.  The first tags of different files are a linked list
	 on the left pointer.  last_node points to the end of the last
	 used sublist. */
      if (last_node != NULL && last_node->fdp == np->fdp)
	{
	  /* Let's use the same sublist as the last added node. */
	  assert (last_node->right == NULL);
	  last_node->right = np;
	  last_node = np;
	}
      else if (cur_node->fdp == np->fdp)
	{
	  /* Scanning the list we found the head of a sublist which is
	     good for us.  Let's scan this sublist. */
	  add_node (np, &cur_node->right);
	}
      else
	/* The head of this sublist is not good for us.  Let's try the
	   next one. */
	add_node (np, &cur_node->left);
    } /* if ETAGS mode */

  else
    {
      /* Ctags Mode */
      dif = strcmp (np->name, cur_node->name);

      /*
       * If this tag name matches an existing one, then
       * do not add the node, but maybe print a warning.
       */
      if (no_duplicates && !dif)
	{
	  if (np->fdp == cur_node->fdp)
	    {
	      if (!no_warnings)
		{
		  fprintf (stderr, "Duplicate entry in file %s, line %d: %s\n",
			   np->fdp->infname, lineno, np->name);
		  fprintf (stderr, "Second entry ignored\n");
		}
	    }
	  else if (!cur_node->been_warned && !no_warnings)
	    {
	      fprintf
		(stderr,
		 "Duplicate entry in files %s and %s: %s (Warning only)\n",
		 np->fdp->infname, cur_node->fdp->infname, np->name);
	      cur_node->been_warned = TRUE;
	    }
	  return;
	}

      /* Actually add the node */
      add_node (np, dif < 0 ? &cur_node->left : &cur_node->right);
    } /* if CTAGS mode */
}

/*
 * invalidate_nodes ()
 *	Scan the node tree and invalidate all nodes pointing to the
 *	given file description (CTAGS case) or free them (ETAGS case).
 */
static void
invalidate_nodes (fdesc *badfdp, node **npp)
{
  node *np = *npp;

  if (np == NULL)
    return;

  if (CTAGS)
    {
      if (np->left != NULL)
	invalidate_nodes (badfdp, &np->left);
      if (np->fdp == badfdp)
	np->valid = FALSE;
      if (np->right != NULL)
	invalidate_nodes (badfdp, &np->right);
    }
  else
    {
      assert (np->fdp != NULL);
      if (np->fdp == badfdp)
	{
	  *npp = np->left;	/* detach the sublist from the list */
	  np->left = NULL;	/* isolate it */
	  free_tree (np);	/* free it */
	  invalidate_nodes (badfdp, npp);
	}
      else
	invalidate_nodes (badfdp, &np->left);
    }
}


static int total_size_of_entries (node *);
static int number_len (long);

/* Length of a non-negative number's decimal representation. */
static int
number_len (long int num)
{
  int len = 1;
  while ((num /= 10) > 0)
    len += 1;
  return len;
}

/*
 * Return total number of characters that put_entries will output for
 * the nodes in the linked list at the right of the specified node.
 * This count is irrelevant with etags.el since emacs 19.34 at least,
 * but is still supplied for backward compatibility.
 */
static int
total_size_of_entries (register node *np)
{
  register int total = 0;

  for (; np != NULL; np = np->right)
    if (np->valid)
      {
	total += strlen (np->regex) + 1;		/* pat\177 */
	if (np->name != NULL)
	  total += strlen (np->name) + 1;		/* name\001 */
	total += number_len ((long) np->lno) + 1;	/* lno, */
	if (np->cno != invalidcharno)			/* cno */
	  total += number_len (np->cno);
	total += 1;					/* newline */
      }

  return total;
}

static void
put_entries (register node *np)
{
  register char *sp;
  static fdesc *fdp = NULL;

  if (np == NULL)
    return;

  /* Output subentries that precede this one */
  if (CTAGS)
    put_entries (np->left);

  /* Output this entry */
  if (np->valid)
    {
      if (!CTAGS)
	{
	  /* Etags mode */
	  if (fdp != np->fdp)
	    {
	      fdp = np->fdp;
	      fprintf (tagf, "\f\n%s,%d\n",
		       fdp->taggedfname, total_size_of_entries (np));
	      fdp->written = TRUE;
	    }
	  fputs (np->regex, tagf);
	  fputc ('\177', tagf);
	  if (np->name != NULL)
	    {
	      fputs (np->name, tagf);
	      fputc ('\001', tagf);
	    }
	  fprintf (tagf, "%d,", np->lno);
	  if (np->cno != invalidcharno)
	    fprintf (tagf, "%ld", np->cno);
	  fputs ("\n", tagf);
	}
      else
	{
	  /* Ctags mode */
	  if (np->name == NULL)
	    error ("internal error: NULL name in ctags mode.", (char *)NULL);

	  if (cxref_style)
	    {
	      if (vgrind_style)
		fprintf (stdout, "%s %s %d\n",
			 np->name, np->fdp->taggedfname, (np->lno + 63) / 64);
	      else
		fprintf (stdout, "%-16s %3d %-16s %s\n",
			 np->name, np->lno, np->fdp->taggedfname, np->regex);
	    }
	  else
	    {
	      fprintf (tagf, "%s\t%s\t", np->name, np->fdp->taggedfname);

	      if (np->is_func)
		{		/* function or #define macro with args */
		  putc (searchar, tagf);
		  putc ('^', tagf);

		  for (sp = np->regex; *sp; sp++)
		    {
		      if (*sp == '\\' || *sp == searchar)
			putc ('\\', tagf);
		      putc (*sp, tagf);
		    }
		  putc (searchar, tagf);
		}
	      else
		{		/* anything else; text pattern inadequate */
		  fprintf (tagf, "%d", np->lno);
		}
	      putc ('\n', tagf);
	    }
	}
    } /* if this node contains a valid tag */

  /* Output subentries that follow this one */
  put_entries (np->right);
  if (!CTAGS)
    put_entries (np->left);
}


/* C extensions. */
#define C_EXT	0x00fff		/* C extensions */
#define C_PLAIN 0x00000		/* C */
#define C_PLPL	0x00001		/* C++ */
#define C_STAR	0x00003		/* C* */
#define C_JAVA	0x00005		/* JAVA */
#define C_AUTO  0x01000		/* C, but switch to C++ if `class' is met */
#define YACC	0x10000		/* yacc file */

/*
 * The C symbol tables.
 */
enum sym_type
{
  st_none,
  st_C_objprot, st_C_objimpl, st_C_objend,
  st_C_gnumacro,
  st_C_ignore, st_C_attribute,
  st_C_javastruct,
  st_C_operator,
  st_C_class, st_C_template,
  st_C_struct, st_C_extern, st_C_enum, st_C_define, st_C_typedef
};

static unsigned int hash (const char *, unsigned int);
static struct C_stab_entry * in_word_set (const char *, unsigned int);
static enum sym_type C_symtype (char *, int, int);

/* Feed stuff between (but not including) %[ and %] lines to:
     gperf -m 5
%[
%compare-strncmp
%enum
%struct-type
struct C_stab_entry { char *name; int c_ext; enum sym_type type; }
%%
if,		0,			st_C_ignore
for,		0,			st_C_ignore
while,		0,			st_C_ignore
switch,		0,			st_C_ignore
return,		0,			st_C_ignore
__attribute__,	0,			st_C_attribute
GTY,            0,                      st_C_attribute
@interface,	0,			st_C_objprot
@protocol,	0,			st_C_objprot
@implementation,0,			st_C_objimpl
@end,		0,			st_C_objend
import,		(C_JAVA & ~C_PLPL),	st_C_ignore
package,	(C_JAVA & ~C_PLPL),	st_C_ignore
friend,		C_PLPL,			st_C_ignore
extends,	(C_JAVA & ~C_PLPL),	st_C_javastruct
implements,	(C_JAVA & ~C_PLPL),	st_C_javastruct
interface,	(C_JAVA & ~C_PLPL),	st_C_struct
class,		0,			st_C_class
namespace,	C_PLPL,			st_C_struct
domain,		C_STAR,			st_C_struct
union,		0,			st_C_struct
struct,		0,			st_C_struct
extern,		0,			st_C_extern
enum,		0,			st_C_enum
typedef,	0,			st_C_typedef
define,		0,			st_C_define
undef,		0,			st_C_define
operator,	C_PLPL,			st_C_operator
template,	0,			st_C_template
# DEFUN used in emacs, the next three used in glibc (SYSCALL only for mach).
DEFUN,		0,			st_C_gnumacro
SYSCALL,	0,			st_C_gnumacro
ENTRY,		0,			st_C_gnumacro
PSEUDO,		0,			st_C_gnumacro
# These are defined inside C functions, so currently they are not met.
# EXFUN used in glibc, DEFVAR_* in emacs.
#EXFUN,		0,			st_C_gnumacro
#DEFVAR_,	0,			st_C_gnumacro
%]
and replace lines between %< and %> with its output, then:
 - remove the #if characterset check
 - make in_word_set static and not inline. */
/*%<*/
/* C code produced by gperf version 3.0.1 */
/* Command-line: gperf -m 5  */
/* Computed positions: -k'2-3' */

struct C_stab_entry { const char *name; int c_ext; enum sym_type type; };
/* maximum key range = 33, duplicates = 0 */

static inline unsigned int
hash (register const char *str, register unsigned int len)
{
  static unsigned char asso_values[] =
    {
      35, 35, 35, 35, 35, 35, 35, 35, 35, 35,
      35, 35, 35, 35, 35, 35, 35, 35, 35, 35,
      35, 35, 35, 35, 35, 35, 35, 35, 35, 35,
      35, 35, 35, 35, 35, 35, 35, 35, 35, 35,
      35, 35, 35, 35, 35, 35, 35, 35, 35, 35,
      35, 35, 35, 35, 35, 35, 35, 35, 35, 35,
      35, 35, 35, 35, 35, 35, 35, 35, 35,  3,
      26, 35, 35, 35, 35, 35, 35, 35, 27, 35,
      35, 35, 35, 24,  0, 35, 35, 35, 35,  0,
      35, 35, 35, 35, 35,  1, 35, 16, 35,  6,
      23,  0,  0, 35, 22,  0, 35, 35,  5,  0,
       0, 15,  1, 35,  6, 35,  8, 19, 35, 16,
       4,  5, 35, 35, 35, 35, 35, 35, 35, 35,
      35, 35, 35, 35, 35, 35, 35, 35, 35, 35,
      35, 35, 35, 35, 35, 35, 35, 35, 35, 35,
      35, 35, 35, 35, 35, 35, 35, 35, 35, 35,
      35, 35, 35, 35, 35, 35, 35, 35, 35, 35,
      35, 35, 35, 35, 35, 35, 35, 35, 35, 35,
      35, 35, 35, 35, 35, 35, 35, 35, 35, 35,
      35, 35, 35, 35, 35, 35, 35, 35, 35, 35,
      35, 35, 35, 35, 35, 35, 35, 35, 35, 35,
      35, 35, 35, 35, 35, 35, 35, 35, 35, 35,
      35, 35, 35, 35, 35, 35, 35, 35, 35, 35,
      35, 35, 35, 35, 35, 35, 35, 35, 35, 35,
      35, 35, 35, 35, 35, 35, 35, 35, 35, 35,
      35, 35, 35, 35, 35, 35
    };
  register int hval = len;

  switch (hval)
    {
      default:
        hval += asso_values[(unsigned char)str[2]];
      /*FALLTHROUGH*/
      case 2:
        hval += asso_values[(unsigned char)str[1]];
        break;
    }
  return hval;
}

static struct C_stab_entry *
in_word_set (register const char *str, register unsigned int len)
{
  enum
    {
      TOTAL_KEYWORDS = 33,
      MIN_WORD_LENGTH = 2,
      MAX_WORD_LENGTH = 15,
      MIN_HASH_VALUE = 2,
      MAX_HASH_VALUE = 34
    };

  static struct C_stab_entry wordlist[] =
    {
      {""}, {""},
      {"if",		0,			st_C_ignore},
      {"GTY",           0,                      st_C_attribute},
      {"@end",		0,			st_C_objend},
      {"union",		0,			st_C_struct},
      {"define",		0,			st_C_define},
      {"import",		(C_JAVA & ~C_PLPL),	st_C_ignore},
      {"template",	0,			st_C_template},
      {"operator",	C_PLPL,			st_C_operator},
      {"@interface",	0,			st_C_objprot},
      {"implements",	(C_JAVA & ~C_PLPL),	st_C_javastruct},
      {"friend",		C_PLPL,			st_C_ignore},
      {"typedef",	0,			st_C_typedef},
      {"return",		0,			st_C_ignore},
      {"@implementation",0,			st_C_objimpl},
      {"@protocol",	0,			st_C_objprot},
      {"interface",	(C_JAVA & ~C_PLPL),	st_C_struct},
      {"extern",		0,			st_C_extern},
      {"extends",	(C_JAVA & ~C_PLPL),	st_C_javastruct},
      {"struct",		0,			st_C_struct},
      {"domain",		C_STAR,			st_C_struct},
      {"switch",		0,			st_C_ignore},
      {"enum",		0,			st_C_enum},
      {"for",		0,			st_C_ignore},
      {"namespace",	C_PLPL,			st_C_struct},
      {"class",		0,			st_C_class},
      {"while",		0,			st_C_ignore},
      {"undef",		0,			st_C_define},
      {"package",	(C_JAVA & ~C_PLPL),	st_C_ignore},
      {"__attribute__",	0,			st_C_attribute},
      {"SYSCALL",	0,			st_C_gnumacro},
      {"ENTRY",		0,			st_C_gnumacro},
      {"PSEUDO",		0,			st_C_gnumacro},
      {"DEFUN",		0,			st_C_gnumacro}
    };

  if (len <= MAX_WORD_LENGTH && len >= MIN_WORD_LENGTH)
    {
      register int key = hash (str, len);

      if (key <= MAX_HASH_VALUE && key >= 0)
        {
          register const char *s = wordlist[key].name;

          if (*str == *s && !strncmp (str + 1, s + 1, len - 1) && s[len] == '\0')
            return &wordlist[key];
        }
    }
  return 0;
}
/*%>*/

static enum sym_type
C_symtype (char *str, int len, int c_ext)
{
  register struct C_stab_entry *se = in_word_set (str, len);

  if (se == NULL || (se->c_ext && !(c_ext & se->c_ext)))
    return st_none;
  return se->type;
}


/*
 * Ignoring __attribute__ ((list))
 */
static bool inattribute;	/* looking at an __attribute__ construct */

/*
 * C functions and variables are recognized using a simple
 * finite automaton.  fvdef is its state variable.
 */
static enum
{
  fvnone,			/* nothing seen */
  fdefunkey,			/* Emacs DEFUN keyword seen */
  fdefunname,			/* Emacs DEFUN name seen */
  foperator,			/* func: operator keyword seen (cplpl) */
  fvnameseen,			/* function or variable name seen */
  fstartlist,			/* func: just after open parenthesis */
  finlist,			/* func: in parameter list */
  flistseen,			/* func: after parameter list */
  fignore,			/* func: before open brace */
  vignore			/* var-like: ignore until ';' */
} fvdef;

static bool fvextern;		/* func or var: extern keyword seen; */

/*
 * typedefs are recognized using a simple finite automaton.
 * typdef is its state variable.
 */
static enum
{
  tnone,			/* nothing seen */
  tkeyseen,			/* typedef keyword seen */
  ttypeseen,			/* defined type seen */
  tinbody,			/* inside typedef body */
  tend,				/* just before typedef tag */
  tignore			/* junk after typedef tag */
} typdef;

/*
 * struct-like structures (enum, struct and union) are recognized
 * using another simple finite automaton.  `structdef' is its state
 * variable.
 */
static enum
{
  snone,			/* nothing seen yet,
				   or in struct body if bracelev > 0 */
  skeyseen,			/* struct-like keyword seen */
  stagseen,			/* struct-like tag seen */
  scolonseen			/* colon seen after struct-like tag */
} structdef;

/*
 * When objdef is different from onone, objtag is the name of the class.
 */
static const char *objtag = "<uninited>";

/*
 * Yet another little state machine to deal with preprocessor lines.
 */
static enum
{
  dnone,			/* nothing seen */
  dsharpseen,			/* '#' seen as first char on line */
  ddefineseen,			/* '#' and 'define' seen */
  dignorerest			/* ignore rest of line */
} definedef;

/*
 * State machine for Objective C protocols and implementations.
 * Idea by Tom R.Hageman <tom@basil.icce.rug.nl> (1995)
 */
static enum
{
  onone,			/* nothing seen */
  oprotocol,			/* @interface or @protocol seen */
  oimplementation,		/* @implementations seen */
  otagseen,			/* class name seen */
  oparenseen,			/* parenthesis before category seen */
  ocatseen,			/* category name seen */
  oinbody,			/* in @implementation body */
  omethodsign,			/* in @implementation body, after +/- */
  omethodtag,			/* after method name */
  omethodcolon,			/* after method colon */
  omethodparm,			/* after method parameter */
  oignore			/* wait for @end */
} objdef;


/*
 * Use this structure to keep info about the token read, and how it
 * should be tagged.  Used by the make_C_tag function to build a tag.
 */
static struct tok
{
  char *line;			/* string containing the token */
  int offset;			/* where the token starts in LINE */
  int length;			/* token length */
  /*
    The previous members can be used to pass strings around for generic
    purposes.  The following ones specifically refer to creating tags.  In this
    case the token contained here is the pattern that will be used to create a
    tag.
  */
  bool valid;			/* do not create a tag; the token should be
				   invalidated whenever a state machine is
				   reset prematurely */
  bool named;			/* create a named tag */
  int lineno;			/* source line number of tag */
  long linepos;			/* source char number of tag */
} token;			/* latest token read */

/*
 * Variables and functions for dealing with nested structures.
 * Idea by Mykola Dzyuba <mdzyuba@yahoo.com> (2001)
 */
static void pushclass_above (int, char *, int);
static void popclass_above (int);
static void write_classname (linebuffer *, const char *qualifier);

static struct {
  char **cname;			/* nested class names */
  int *bracelev;		/* nested class brace level */
  int nl;			/* class nesting level (elements used) */
  int size;			/* length of the array */
} cstack;			/* stack for nested declaration tags */
/* Current struct nesting depth (namespace, class, struct, union, enum). */
#define nestlev		(cstack.nl)
/* After struct keyword or in struct body, not inside a nested function. */
#define instruct	(structdef == snone && nestlev > 0			\
			 && bracelev == cstack.bracelev[nestlev-1] + 1)

static void
pushclass_above (int bracelev, char *str, int len)
{
  int nl;

  popclass_above (bracelev);
  nl = cstack.nl;
  if (nl >= cstack.size)
    {
      int size = cstack.size *= 2;
      xrnew (cstack.cname, size, char *);
      xrnew (cstack.bracelev, size, int);
    }
  assert (nl == 0 || cstack.bracelev[nl-1] < bracelev);
  cstack.cname[nl] = (str == NULL) ? NULL : savenstr (str, len);
  cstack.bracelev[nl] = bracelev;
  cstack.nl = nl + 1;
}

static void
popclass_above (int bracelev)
{
  int nl;

  for (nl = cstack.nl - 1;
       nl >= 0 && cstack.bracelev[nl] >= bracelev;
       nl--)
    {
      free (cstack.cname[nl]);
      cstack.nl = nl;
    }
}

static void
write_classname (linebuffer *cn, const char *qualifier)
{
  int i, len;
  int qlen = strlen (qualifier);

  if (cstack.nl == 0 || cstack.cname[0] == NULL)
    {
      len = 0;
      cn->len = 0;
      cn->buffer[0] = '\0';
    }
  else
    {
      len = strlen (cstack.cname[0]);
      linebuffer_setlen (cn, len);
      strcpy (cn->buffer, cstack.cname[0]);
    }
  for (i = 1; i < cstack.nl; i++)
    {
      char *s;
      int slen;

      s = cstack.cname[i];
      if (s == NULL)
	continue;
      slen = strlen (s);
      len += slen + qlen;
      linebuffer_setlen (cn, len);
      strncat (cn->buffer, qualifier, qlen);
      strncat (cn->buffer, s, slen);
    }
}


static bool consider_token (char *, int, int, int *, int, int, bool *);
static void make_C_tag (bool);

/*
 * consider_token ()
 *	checks to see if the current token is at the start of a
 *	function or variable, or corresponds to a typedef, or
 * 	is a struct/union/enum tag, or #define, or an enum constant.
 *
 *	*IS_FUNC gets TRUE if the token is a function or #define macro
 *	with args.  C_EXTP points to which language we are looking at.
 *
 * Globals
 *	fvdef			IN OUT
 *	structdef		IN OUT
 *	definedef		IN OUT
 *	typdef			IN OUT
 *	objdef			IN OUT
 */

static bool
consider_token (register char *str, register int len, register int c, int *c_extp, int bracelev, int parlev, int *is_func_or_var)
                        	/* IN: token pointer */
                      		/* IN: token length */
                    		/* IN: first char after the token */
                 		/* IN, OUT: C extensions mask */
                  		/* IN: brace level */
                		/* IN: parenthesis level */
                          	/* OUT: function or variable found */
{
  /* When structdef is stagseen, scolonseen, or snone with bracelev > 0,
     structtype is the type of the preceding struct-like keyword, and
     structbracelev is the brace level where it has been seen. */
  static enum sym_type structtype;
  static int structbracelev;
  static enum sym_type toktype;


  toktype = C_symtype (str, len, *c_extp);

  /*
   * Skip __attribute__
   */
  if (toktype == st_C_attribute)
    {
      inattribute = TRUE;
      return FALSE;
     }

   /*
    * Advance the definedef state machine.
    */
   switch (definedef)
     {
     case dnone:
       /* We're not on a preprocessor line. */
       if (toktype == st_C_gnumacro)
	 {
	   fvdef = fdefunkey;
	   return FALSE;
	 }
       break;
     case dsharpseen:
       if (toktype == st_C_define)
	 {
	   definedef = ddefineseen;
	 }
       else
	 {
	   definedef = dignorerest;
	 }
       return FALSE;
     case ddefineseen:
       /*
	* Make a tag for any macro, unless it is a constant
	* and constantypedefs is FALSE.
	*/
       definedef = dignorerest;
       *is_func_or_var = (c == '(');
       if (!*is_func_or_var && !constantypedefs)
	 return FALSE;
       else
	 return TRUE;
     case dignorerest:
       return FALSE;
     default:
       error ("internal error: definedef value.", (char *)NULL);
     }

   /*
    * Now typedefs
    */
   switch (typdef)
     {
     case tnone:
       if (toktype == st_C_typedef)
	 {
	   if (typedefs)
	     typdef = tkeyseen;
	   fvextern = FALSE;
	   fvdef = fvnone;
	   return FALSE;
	 }
       break;
     case tkeyseen:
       switch (toktype)
	 {
	 case st_none:
	 case st_C_class:
	 case st_C_struct:
	 case st_C_enum:
	   typdef = ttypeseen;
	 }
       break;
     case ttypeseen:
       if (structdef == snone && fvdef == fvnone)
	 {
	   fvdef = fvnameseen;
	   return TRUE;
	 }
       break;
     case tend:
       switch (toktype)
	 {
	 case st_C_class:
	 case st_C_struct:
	 case st_C_enum:
	   return FALSE;
	 }
       return TRUE;
     }

   switch (toktype)
     {
     case st_C_javastruct:
       if (structdef == stagseen)
	 structdef = scolonseen;
       return FALSE;
     case st_C_template:
     case st_C_class:
       if ((*c_extp & C_AUTO)	/* automatic detection of C++ language */
	   && bracelev == 0
	   && definedef == dnone && structdef == snone
	   && typdef == tnone && fvdef == fvnone)
	 *c_extp = (*c_extp | C_PLPL) & ~C_AUTO;
       if (toktype == st_C_template)
	 break;
       /* FALLTHRU */
     case st_C_struct:
     case st_C_enum:
       if (parlev == 0
	   && fvdef != vignore
	   && (typdef == tkeyseen
	       || (typedefs_or_cplusplus && structdef == snone)))
	 {
	   structdef = skeyseen;
	   structtype = toktype;
	   structbracelev = bracelev;
	   if (fvdef == fvnameseen)
	     fvdef = fvnone;
	 }
       return FALSE;
     }

   if (structdef == skeyseen)
     {
       structdef = stagseen;
       return TRUE;
     }

   if (typdef != tnone)
     definedef = dnone;

   /* Detect Objective C constructs. */
   switch (objdef)
     {
     case onone:
       switch (toktype)
	 {
	 case st_C_objprot:
	   objdef = oprotocol;
	   return FALSE;
	 case st_C_objimpl:
	   objdef = oimplementation;
	   return FALSE;
	 }
       break;
     case oimplementation:
       /* Save the class tag for functions or variables defined inside. */
       objtag = savenstr (str, len);
       objdef = oinbody;
       return FALSE;
     case oprotocol:
       /* Save the class tag for categories. */
       objtag = savenstr (str, len);
       objdef = otagseen;
       *is_func_or_var = TRUE;
       return TRUE;
     case oparenseen:
       objdef = ocatseen;
       *is_func_or_var = TRUE;
       return TRUE;
     case oinbody:
       break;
     case omethodsign:
       if (parlev == 0)
	 {
	   fvdef = fvnone;
	   objdef = omethodtag;
	   linebuffer_setlen (&token_name, len);
	   strncpy (token_name.buffer, str, len);
	   token_name.buffer[len] = '\0';
	   return TRUE;
	 }
       return FALSE;
     case omethodcolon:
       if (parlev == 0)
	 objdef = omethodparm;
       return FALSE;
     case omethodparm:
       if (parlev == 0)
	 {
	   fvdef = fvnone;
	   objdef = omethodtag;
	   linebuffer_setlen (&token_name, token_name.len + len);
	   strncat (token_name.buffer, str, len);
	   return TRUE;
	 }
       return FALSE;
     case oignore:
       if (toktype == st_C_objend)
	 {
	   /* Memory leakage here: the string pointed by objtag is
	      never released, because many tests would be needed to
	      avoid breaking on incorrect input code.  The amount of
	      memory leaked here is the sum of the lengths of the
	      class tags.
	   free (objtag); */
	   objdef = onone;
	 }
       return FALSE;
     }

   /* A function, variable or enum constant? */
   switch (toktype)
     {
     case st_C_extern:
       fvextern = TRUE;
       switch  (fvdef)
	 {
	 case finlist:
	 case flistseen:
	 case fignore:
	 case vignore:
	   break;
	 default:
	   fvdef = fvnone;
	 }
       return FALSE;
     case st_C_ignore:
       fvextern = FALSE;
       fvdef = vignore;
       return FALSE;
     case st_C_operator:
       fvdef = foperator;
       *is_func_or_var = TRUE;
       return TRUE;
     case st_none:
       if (constantypedefs
	   && structdef == snone
	   && structtype == st_C_enum && bracelev > structbracelev)
	 return TRUE;		/* enum constant */
       switch (fvdef)
	 {
	 case fdefunkey:
	   if (bracelev > 0)
	     break;
	   fvdef = fdefunname;	/* GNU macro */
	   *is_func_or_var = TRUE;
	   return TRUE;
	 case fvnone:
	   switch (typdef)
	     {
	     case ttypeseen:
	       return FALSE;
	     case tnone:
	       if ((strneq (str, "asm", 3) && endtoken (str[3]))
		   || (strneq (str, "__asm__", 7) && endtoken (str[7])))
		 {
		   fvdef = vignore;
		   return FALSE;
		 }
	       break;
	     }
	  /* FALLTHRU */
	  case fvnameseen:
	  if (len >= 10 && strneq (str+len-10, "::operator", 10))
	    {
	      if (*c_extp & C_AUTO) /* automatic detection of C++ */
		*c_extp = (*c_extp | C_PLPL) & ~C_AUTO;
	      fvdef = foperator;
	      *is_func_or_var = TRUE;
	      return TRUE;
	    }
	  if (bracelev > 0 && !instruct)
	    break;
	  fvdef = fvnameseen;	/* function or variable */
	  *is_func_or_var = TRUE;
	  return TRUE;
	}
      break;
    }

  return FALSE;
}


/*
 * C_entries often keeps pointers to tokens or lines which are older than
 * the line currently read.  By keeping two line buffers, and switching
 * them at end of line, it is possible to use those pointers.
 */
static struct
{
  long linepos;
  linebuffer lb;
} lbs[2];

#define current_lb_is_new (newndx == curndx)
#define switch_line_buffers() (curndx = 1 - curndx)

#define curlb (lbs[curndx].lb)
#define newlb (lbs[newndx].lb)
#define curlinepos (lbs[curndx].linepos)
#define newlinepos (lbs[newndx].linepos)

#define plainc ((c_ext & C_EXT) == C_PLAIN)
#define cplpl (c_ext & C_PLPL)
#define cjava ((c_ext & C_JAVA) == C_JAVA)

#define CNL_SAVE_DEFINEDEF()						\
do {									\
  curlinepos = charno;							\
  readline (&curlb, inf);						\
  lp = curlb.buffer;							\
  quotednl = FALSE;							\
  newndx = curndx;							\
} while (0)

#define CNL()								\
do {									\
  CNL_SAVE_DEFINEDEF();							\
  if (savetoken.valid)							\
    {									\
      token = savetoken;						\
      savetoken.valid = FALSE;						\
    }									\
  definedef = dnone;							\
} while (0)


static void
make_C_tag (int isfun)
{
  /* This function is never called when token.valid is FALSE, but
     we must protect against invalid input or internal errors. */
  if (token.valid)
    make_tag (token_name.buffer, token_name.len, isfun, token.line,
	      token.offset+token.length+1, token.lineno, token.linepos);
  else if (DEBUG)
    {				  /* this branch is optimized away if !DEBUG */
      make_tag (concat ("INVALID TOKEN:-->", token_name.buffer, ""),
		token_name.len + 17, isfun, token.line,
		token.offset+token.length+1, token.lineno, token.linepos);
      error ("INVALID TOKEN", NULL);
    }

  token.valid = FALSE;
}


/*
 * C_entries ()
 *	This routine finds functions, variables, typedefs,
 * 	#define's, enum constants and struct/union/enum definitions in
 * 	C syntax and adds them to the list.
 */
static void
C_entries (int c_ext, FILE *inf)
               			/* extension of C */
               			/* input file */
{
  register char c;		/* latest char read; '\0' for end of line */
  register char *lp;		/* pointer one beyond the character `c' */
  int curndx, newndx;		/* indices for current and new lb */
  register int tokoff;		/* offset in line of start of current token */
  register int toklen;		/* length of current token */
  const char *qualifier;        /* string used to qualify names */
  int qlen;			/* length of qualifier */
  int bracelev;			/* current brace level */
  int bracketlev;		/* current bracket level */
  int parlev;			/* current parenthesis level */
  int attrparlev;		/* __attribute__ parenthesis level */
  int templatelev;		/* current template level */
  int typdefbracelev;		/* bracelev where a typedef struct body begun */
  bool incomm, inquote, inchar, quotednl, midtoken;
  bool yacc_rules;		/* in the rules part of a yacc file */
  struct tok savetoken = {0};	/* token saved during preprocessor handling */


  linebuffer_init (&lbs[0].lb);
  linebuffer_init (&lbs[1].lb);
  if (cstack.size == 0)
    {
      cstack.size = (DEBUG) ? 1 : 4;
      cstack.nl = 0;
      cstack.cname = xnew (cstack.size, char *);
      cstack.bracelev = xnew (cstack.size, int);
    }

  tokoff = toklen = typdefbracelev = 0; /* keep compiler quiet */
  curndx = newndx = 0;
  lp = curlb.buffer;
  *lp = 0;

  fvdef = fvnone; fvextern = FALSE; typdef = tnone;
  structdef = snone; definedef = dnone; objdef = onone;
  yacc_rules = FALSE;
  midtoken = inquote = inchar = incomm = quotednl = FALSE;
  token.valid = savetoken.valid = FALSE;
  bracelev = bracketlev = parlev = attrparlev = templatelev = 0;
  if (cjava)
    { qualifier = "."; qlen = 1; }
  else
    { qualifier = "::"; qlen = 2; }


  while (!feof (inf))
    {
      c = *lp++;
      if (c == '\\')
	{
	  /* If we are at the end of the line, the next character is a
	     '\0'; do not skip it, because it is what tells us
	     to read the next line.  */
	  if (*lp == '\0')
	    {
	      quotednl = TRUE;
	      continue;
	    }
	  lp++;
	  c = ' ';
	}
      else if (incomm)
	{
	  switch (c)
	    {
	    case '*':
	      if (*lp == '/')
		{
		  c = *lp++;
		  incomm = FALSE;
		}
	      break;
	    case '\0':
	      /* Newlines inside comments do not end macro definitions in
		 traditional cpp. */
	      CNL_SAVE_DEFINEDEF ();
	      break;
	    }
	  continue;
	}
      else if (inquote)
	{
	  switch (c)
	    {
	    case '"':
	      inquote = FALSE;
	      break;
	    case '\0':
	      /* Newlines inside strings do not end macro definitions
		 in traditional cpp, even though compilers don't
		 usually accept them. */
	      CNL_SAVE_DEFINEDEF ();
	      break;
	    }
	  continue;
	}
      else if (inchar)
	{
	  switch (c)
	    {
	    case '\0':
	      /* Hmmm, something went wrong. */
	      CNL ();
	      /* FALLTHRU */
	    case '\'':
	      inchar = FALSE;
	      break;
	    }
	  continue;
	}
      else switch (c)
	{
	case '"':
	  inquote = TRUE;
	  if (bracketlev > 0)
	    continue;
	  if (inattribute)
	    break;
	  switch (fvdef)
	    {
	    case fdefunkey:
	    case fstartlist:
	    case finlist:
	    case fignore:
	    case vignore:
	      break;
	    default:
	      fvextern = FALSE;
	      fvdef = fvnone;
	    }
	  continue;
	case '\'':
	  inchar = TRUE;
	  if (bracketlev > 0)
	    continue;
	  if (inattribute)
	    break;
	  if (fvdef != finlist && fvdef != fignore && fvdef != vignore)
	    {
	      fvextern = FALSE;
	      fvdef = fvnone;
	    }
	  continue;
	case '/':
	  if (*lp == '*')
	    {
	      incomm = TRUE;
	      lp++;
	      c = ' ';
	      if (bracketlev > 0)
		continue;
	    }
	  else if (/* cplpl && */ *lp == '/')
	    {
	      c = '\0';
	    }
	  break;
	case '%':
	  if ((c_ext & YACC) && *lp == '%')
	    {
	      /* Entering or exiting rules section in yacc file. */
	      lp++;
	      definedef = dnone; fvdef = fvnone; fvextern = FALSE;
	      typdef = tnone; structdef = snone;
	      midtoken = inquote = inchar = incomm = quotednl = FALSE;
	      bracelev = 0;
	      yacc_rules = !yacc_rules;
	      continue;
	    }
	  else
	    break;
	case '#':
	  if (definedef == dnone)
	    {
	      char *cp;
	      bool cpptoken = TRUE;

	      /* Look back on this line.  If all blanks, or nonblanks
		 followed by an end of comment, this is a preprocessor
		 token. */
	      for (cp = newlb.buffer; cp < lp-1; cp++)
		if (!iswhite (*cp))
		  {
		    if (*cp == '*' && cp[1] == '/')
		      {
			cp++;
			cpptoken = TRUE;
		      }
		    else
		      cpptoken = FALSE;
		  }
	      if (cpptoken)
		definedef = dsharpseen;
	    } /* if (definedef == dnone) */
	  continue;
	case '[':
	  bracketlev++;
	  continue;
	default:
	  if (bracketlev > 0)
	    {
	      if (c == ']')
		--bracketlev;
	      else if (c == '\0')
		CNL_SAVE_DEFINEDEF ();
	      continue;
	    }
	  break;
	} /* switch (c) */


      /* Consider token only if some involved conditions are satisfied. */
      if (typdef != tignore
	  && definedef != dignorerest
	  && fvdef != finlist
	  && templatelev == 0
	  && (definedef != dnone
	      || structdef != scolonseen)
	  && !inattribute)
	{
	  if (midtoken)
	    {
	      if (endtoken (c))
		{
		  if (c == ':' && *lp == ':' && begtoken (lp[1]))
		    /* This handles :: in the middle,
		       but not at the beginning of an identifier.
		       Also, space-separated :: is not recognized. */
		    {
		      if (c_ext & C_AUTO) /* automatic detection of C++ */
			c_ext = (c_ext | C_PLPL) & ~C_AUTO;
		      lp += 2;
		      toklen += 2;
		      c = lp[-1];
		      goto still_in_token;
		    }
		  else
		    {
		      bool funorvar = FALSE;

		      if (yacc_rules
			  || consider_token (newlb.buffer + tokoff, toklen, c,
					     &c_ext, bracelev, parlev,
					     &funorvar))
			{
			  if (fvdef == foperator)
			    {
			      char *oldlp = lp;
			      lp = skip_spaces (lp-1);
			      if (*lp != '\0')
				lp += 1;
			      while (*lp != '\0'
				     && !iswhite (*lp) && *lp != '(')
				lp += 1;
			      c = *lp++;
			      toklen += lp - oldlp;
			    }
			  token.named = FALSE;
			  if (!plainc
			      && nestlev > 0 && definedef == dnone)
			    /* in struct body */
			    {
                              write_classname (&token_name, qualifier);
			      linebuffer_setlen (&token_name,
						 token_name.len+qlen+toklen);
			      strcat (token_name.buffer, qualifier);
			      strncat (token_name.buffer,
				       newlb.buffer + tokoff, toklen);
			      token.named = TRUE;
			    }
			  else if (objdef == ocatseen)
			    /* Objective C category */
			    {
			      int len = strlen (objtag) + 2 + toklen;
			      linebuffer_setlen (&token_name, len);
			      strcpy (token_name.buffer, objtag);
			      strcat (token_name.buffer, "(");
			      strncat (token_name.buffer,
				       newlb.buffer + tokoff, toklen);
			      strcat (token_name.buffer, ")");
			      token.named = TRUE;
			    }
			  else if (objdef == omethodtag
				   || objdef == omethodparm)
			    /* Objective C method */
			    {
			      token.named = TRUE;
			    }
			  else if (fvdef == fdefunname)
			    /* GNU DEFUN and similar macros */
			    {
			      bool defun = (newlb.buffer[tokoff] == 'F');
			      int off = tokoff;
			      int len = toklen;

			      /* Rewrite the tag so that emacs lisp DEFUNs
				 can be found by their elisp name */
			      if (defun)
				{
				  off += 1;
				  len -= 1;
				}
			      linebuffer_setlen (&token_name, len);
			      strncpy (token_name.buffer,
				       newlb.buffer + off, len);
			      token_name.buffer[len] = '\0';
			      if (defun)
				while (--len >= 0)
				  if (token_name.buffer[len] == '_')
				    token_name.buffer[len] = '-';
			      token.named = defun;
			    }
			  else
			    {
			      linebuffer_setlen (&token_name, toklen);
			      strncpy (token_name.buffer,
				       newlb.buffer + tokoff, toklen);
			      token_name.buffer[toklen] = '\0';
			      /* Name macros and members. */
			      token.named = (structdef == stagseen
					     || typdef == ttypeseen
					     || typdef == tend
					     || (funorvar
						 && definedef == dignorerest)
					     || (funorvar
						 && definedef == dnone
						 && structdef == snone
						 && bracelev > 0));
			    }
			  token.lineno = lineno;
			  token.offset = tokoff;
			  token.length = toklen;
			  token.line = newlb.buffer;
			  token.linepos = newlinepos;
			  token.valid = TRUE;

			  if (definedef == dnone
			      && (fvdef == fvnameseen
				  || fvdef == foperator
				  || structdef == stagseen
				  || typdef == tend
				  || typdef == ttypeseen
				  || objdef != onone))
			    {
			      if (current_lb_is_new)
				switch_line_buffers ();
			    }
			  else if (definedef != dnone
				   || fvdef == fdefunname
				   || instruct)
			    make_C_tag (funorvar);
			}
		      else /* not yacc and consider_token failed */
			{
			  if (inattribute && fvdef == fignore)
			    {
			      /* We have just met __attribute__ after a
				 function parameter list: do not tag the
				 function again. */
			      fvdef = fvnone;
			    }
			}
		      midtoken = FALSE;
		    }
		} /* if (endtoken (c)) */
	      else if (intoken (c))
		still_in_token:
		{
		  toklen++;
		  continue;
		}
	    } /* if (midtoken) */
	  else if (begtoken (c))
	    {
	      switch (definedef)
		{
		case dnone:
		  switch (fvdef)
		    {
		    case fstartlist:
		      /* This prevents tagging fb in
			 void (__attribute__((noreturn)) *fb) (void);
			 Fixing this is not easy and not very important. */
		      fvdef = finlist;
		      continue;
		    case flistseen:
		      if (plainc || declarations)
			{
			  make_C_tag (TRUE); /* a function */
			  fvdef = fignore;
			}
		      break;
		    }
		  if (structdef == stagseen && !cjava)
		    {
		      popclass_above (bracelev);
		      structdef = snone;
		    }
		  break;
		case dsharpseen:
		  savetoken = token;
		  break;
		}
	      if (!yacc_rules || lp == newlb.buffer + 1)
		{
		  tokoff = lp - 1 - newlb.buffer;
		  toklen = 1;
		  midtoken = TRUE;
		}
	      continue;
	    } /* if (begtoken) */
	} /* if must look at token */


      /* Detect end of line, colon, comma, semicolon and various braces
	 after having handled a token.*/
      switch (c)
	{
	case ':':
	  if (inattribute)
	    break;
	  if (yacc_rules && token.offset == 0 && token.valid)
	    {
	      make_C_tag (FALSE); /* a yacc function */
	      break;
	    }
	  if (definedef != dnone)
	    break;
	  switch (objdef)
	    {
	    case  otagseen:
	      objdef = oignore;
	      make_C_tag (TRUE); /* an Objective C class */
	      break;
	    case omethodtag:
	    case omethodparm:
	      objdef = omethodcolon;
	      linebuffer_setlen (&token_name, token_name.len + 1);
	      strcat (token_name.buffer, ":");
	      break;
	    }
	  if (structdef == stagseen)
	    {
	      structdef = scolonseen;
	      break;
	    }
	  /* Should be useless, but may be work as a safety net. */
	  if (cplpl && fvdef == flistseen)
	    {
	      make_C_tag (TRUE); /* a function */
	      fvdef = fignore;
	      break;
	    }
	  break;
	case ';':
	  if (definedef != dnone || inattribute)
	    break;
	  switch (typdef)
	    {
	    case tend:
	    case ttypeseen:
	      make_C_tag (FALSE); /* a typedef */
	      typdef = tnone;
	      fvdef = fvnone;
	      break;
	    case tnone:
	    case tinbody:
	    case tignore:
	      switch (fvdef)
		{
		case fignore:
		  if (typdef == tignore || cplpl)
		    fvdef = fvnone;
		  break;
		case fvnameseen:
		  if ((globals && bracelev == 0 && (!fvextern || declarations))
		      || (members && instruct))
		    make_C_tag (FALSE); /* a variable */
		  fvextern = FALSE;
		  fvdef = fvnone;
		  token.valid = FALSE;
		  break;
		case flistseen:
		  if ((declarations
		       && (cplpl || !instruct)
		       && (typdef == tnone || (typdef != tignore && instruct)))
		      || (members
			  && plainc && instruct))
		    make_C_tag (TRUE);  /* a function */
		  /* FALLTHRU */
		default:
		  fvextern = FALSE;
		  fvdef = fvnone;
		  if (declarations
		       && cplpl && structdef == stagseen)
		    make_C_tag (FALSE);	/* forward declaration */
		  else
		    token.valid = FALSE;
		} /* switch (fvdef) */
	      /* FALLTHRU */
	    default:
	      if (!instruct)
		typdef = tnone;
	    }
	  if (structdef == stagseen)
	    structdef = snone;
	  break;
	case ',':
	  if (definedef != dnone || inattribute)
	    break;
	  switch (objdef)
	    {
	    case omethodtag:
	    case omethodparm:
	      make_C_tag (TRUE); /* an Objective C method */
	      objdef = oinbody;
	      break;
	    }
	  switch (fvdef)
	    {
	    case fdefunkey:
	    case foperator:
	    case fstartlist:
	    case finlist:
	    case fignore:
	    case vignore:
	      break;
	    case fdefunname:
	      fvdef = fignore;
	      break;
	    case fvnameseen:
	      if (parlev == 0
		  && ((globals
		       && bracelev == 0
		       && templatelev == 0
		       && (!fvextern || declarations))
		      || (members && instruct)))
		  make_C_tag (FALSE); /* a variable */
	      break;
	    case flistseen:
	      if ((declarations && typdef == tnone && !instruct)
		  || (members && typdef != tignore && instruct))
		{
		  make_C_tag (TRUE); /* a function */
		  fvdef = fvnameseen;
		}
	      else if (!declarations)
		fvdef = fvnone;
	      token.valid = FALSE;
	      break;
	    default:
	      fvdef = fvnone;
	    }
	  if (structdef == stagseen)
	    structdef = snone;
	  break;
	case ']':
	  if (definedef != dnone || inattribute)
	    break;
	  if (structdef == stagseen)
	    structdef = snone;
	  switch (typdef)
	    {
	    case ttypeseen:
	    case tend:
	      typdef = tignore;
	      make_C_tag (FALSE);	/* a typedef */
	      break;
	    case tnone:
	    case tinbody:
	      switch (fvdef)
		{
		case foperator:
		case finlist:
		case fignore:
		case vignore:
		  break;
		case fvnameseen:
		  if ((members && bracelev == 1)
		      || (globals && bracelev == 0
			  && (!fvextern || declarations)))
		    make_C_tag (FALSE); /* a variable */
		  /* FALLTHRU */
		default:
		  fvdef = fvnone;
		}
	      break;
	    }
	  break;
	case '(':
	  if (inattribute)
	    {
	      attrparlev++;
	      break;
	    }
	  if (definedef != dnone)
	    break;
	  if (objdef == otagseen && parlev == 0)
	    objdef = oparenseen;
	  switch (fvdef)
	    {
	    case fvnameseen:
	      if (typdef == ttypeseen
		  && *lp != '*'
		  && !instruct)
		{
		  /* This handles constructs like:
		     typedef void OperatorFun (int fun); */
		  make_C_tag (FALSE);
		  typdef = tignore;
		  fvdef = fignore;
		  break;
		}
	      /* FALLTHRU */
	    case foperator:
	      fvdef = fstartlist;
	      break;
	    case flistseen:
	      fvdef = finlist;
	      break;
	    }
	  parlev++;
	  break;
	case ')':
	  if (inattribute)
	    {
	      if (--attrparlev == 0)
		inattribute = FALSE;
	      break;
	    }
	  if (definedef != dnone)
	    break;
	  if (objdef == ocatseen && parlev == 1)
	    {
	      make_C_tag (TRUE); /* an Objective C category */
	      objdef = oignore;
	    }
	  if (--parlev == 0)
	    {
	      switch (fvdef)
		{
		case fstartlist:
		case finlist:
		  fvdef = flistseen;
		  break;
		}
	      if (!instruct
		  && (typdef == tend
		      || typdef == ttypeseen))
		{
		  typdef = tignore;
		  make_C_tag (FALSE); /* a typedef */
		}
	    }
	  else if (parlev < 0)	/* can happen due to ill-conceived #if's. */
	    parlev = 0;
	  break;
	case '{':
	  if (definedef != dnone)
	    break;
	  if (typdef == ttypeseen)
	    {
	      /* Whenever typdef is set to tinbody (currently only
		 here), typdefbracelev should be set to bracelev. */
	      typdef = tinbody;
	      typdefbracelev = bracelev;
	    }
	  switch (fvdef)
	    {
	    case flistseen:
	      make_C_tag (TRUE);    /* a function */
	      /* FALLTHRU */
	    case fignore:
	      fvdef = fvnone;
	      break;
	    case fvnone:
	      switch (objdef)
		{
		case otagseen:
		  make_C_tag (TRUE); /* an Objective C class */
		  objdef = oignore;
		  break;
		case omethodtag:
		case omethodparm:
		  make_C_tag (TRUE); /* an Objective C method */
		  objdef = oinbody;
		  break;
		default:
		  /* Neutralize `extern "C" {' grot. */
		  if (bracelev == 0 && structdef == snone && nestlev == 0
		      && typdef == tnone)
		    bracelev = -1;
		}
	      break;
	    }
	  switch (structdef)
	    {
	    case skeyseen:	   /* unnamed struct */
	      pushclass_above (bracelev, NULL, 0);
	      structdef = snone;
	      break;
	    case stagseen:	   /* named struct or enum */
	    case scolonseen:	   /* a class */
	      pushclass_above (bracelev,token.line+token.offset, token.length);
	      structdef = snone;
	      make_C_tag (FALSE);  /* a struct or enum */
	      break;
	    }
	  bracelev += 1;
	  break;
	case '*':
	  if (definedef != dnone)
	    break;
	  if (fvdef == fstartlist)
	    {
	      fvdef = fvnone;	/* avoid tagging `foo' in `foo (*bar()) ()' */
	      token.valid = FALSE;
	    }
	  break;
	case '}':
	  if (definedef != dnone)
	    break;
	  bracelev -= 1;
	  if (!ignoreindent && lp == newlb.buffer + 1)
	    {
	      if (bracelev != 0)
		token.valid = FALSE; /* unexpected value, token unreliable */
	      bracelev = 0;	/* reset brace level if first column */
	      parlev = 0;	/* also reset paren level, just in case... */
	    }
	  else if (bracelev < 0)
	    {
	      token.valid = FALSE; /* something gone amiss, token unreliable */
	      bracelev = 0;
	    }
	  if (bracelev == 0 && fvdef == vignore)
	    fvdef = fvnone;		/* end of function */
	  popclass_above (bracelev);
	  structdef = snone;
	  /* Only if typdef == tinbody is typdefbracelev significant. */
	  if (typdef == tinbody && bracelev <= typdefbracelev)
	    {
	      assert (bracelev == typdefbracelev);
	      typdef = tend;
	    }
	  break;
	case '=':
	  if (definedef != dnone)
	    break;
	  switch (fvdef)
	    {
	    case foperator:
	    case finlist:
	    case fignore:
	    case vignore:
	      break;
	    case fvnameseen:
	      if ((members && bracelev == 1)
		  || (globals && bracelev == 0 && (!fvextern || declarations)))
		make_C_tag (FALSE); /* a variable */
	      /* FALLTHRU */
	    default:
	      fvdef = vignore;
	    }
	  break;
	case '<':
	  if (cplpl
	      && (structdef == stagseen || fvdef == fvnameseen))
	    {
	      templatelev++;
	      break;
	    }
	  goto resetfvdef;
	case '>':
	  if (templatelev > 0)
	    {
	      templatelev--;
	      break;
	    }
	  goto resetfvdef;
	case '+':
	case '-':
	  if (objdef == oinbody && bracelev == 0)
	    {
	      objdef = omethodsign;
	      break;
	    }
	  /* FALLTHRU */
	resetfvdef:
	case '#': case '~': case '&': case '%': case '/':
	case '|': case '^': case '!': case '.': case '?':
	  if (definedef != dnone)
	    break;
	  /* These surely cannot follow a function tag in C. */
	  switch (fvdef)
	    {
	    case foperator:
	    case finlist:
	    case fignore:
	    case vignore:
	      break;
	    default:
	      fvdef = fvnone;
	    }
	  break;
	case '\0':
	  if (objdef == otagseen)
	    {
	      make_C_tag (TRUE); /* an Objective C class */
	      objdef = oignore;
	    }
	  /* If a macro spans multiple lines don't reset its state. */
	  if (quotednl)
	    CNL_SAVE_DEFINEDEF ();
	  else
	    CNL ();
	  break;
	} /* switch (c) */

    } /* while not eof */

  free (lbs[0].lb.buffer);
  free (lbs[1].lb.buffer);
}

/*
 * Process either a C++ file or a C file depending on the setting
 * of a global flag.
 */
static void
default_C_entries (FILE *inf)
{
  C_entries (cplusplus ? C_PLPL : C_AUTO, inf);
}

/* Always do plain C. */
static void
plain_C_entries (FILE *inf)
{
  C_entries (0, inf);
}

/* Always do C++. */
static void
Cplusplus_entries (FILE *inf)
{
  C_entries (C_PLPL, inf);
}

/* Always do Java. */
static void
Cjava_entries (FILE *inf)
{
  C_entries (C_JAVA, inf);
}

/* Always do C*. */
static void
Cstar_entries (FILE *inf)
{
  C_entries (C_STAR, inf);
}

/* Always do Yacc. */
static void
Yacc_entries (FILE *inf)
{
  C_entries (YACC, inf);
}


/* Useful macros. */
#define LOOP_ON_INPUT_LINES(file_pointer, line_buffer, char_pointer)	\
  for (;			/* loop initialization */		\
       !feof (file_pointer)	/* loop test */				\
       &&			/* instructions at start of loop */	\
	  (readline (&line_buffer, file_pointer),			\
           char_pointer = line_buffer.buffer,				\
	   TRUE);							\
      )

#define LOOKING_AT(cp, kw)  /* kw is the keyword, a literal string */	\
  ((assert ("" kw), TRUE)   /* syntax error if not a literal string */	\
   && strneq ((cp), kw, sizeof (kw)-1)		/* cp points at kw */	\
   && notinname ((cp)[sizeof (kw)-1])		/* end of kw */		\
   && ((cp) = skip_spaces ((cp)+sizeof (kw)-1))) /* skip spaces */

/* Similar to LOOKING_AT but does not use notinname, does not skip */
#define LOOKING_AT_NOCASE(cp, kw) /* the keyword is a literal string */	\
  ((assert ("" kw), TRUE) /* syntax error if not a literal string */	\
   && strncaseeq ((cp), kw, sizeof (kw)-1)	/* cp points at kw */	\
   && ((cp) += sizeof (kw)-1))			/* skip spaces */

/*
 * Read a file, but do no processing.  This is used to do regexp
 * matching on files that have no language defined.
 */
static void
just_read_file (FILE *inf)
{
  while (!feof (inf))
    readline (&lb, inf);
}


/* Fortran parsing */

static void F_takeprec (void);
static void F_getit (FILE *);

static void
F_takeprec (void)
{
  dbp = skip_spaces (dbp);
  if (*dbp != '*')
    return;
  dbp++;
  dbp = skip_spaces (dbp);
  if (strneq (dbp, "(*)", 3))
    {
      dbp += 3;
      return;
    }
  if (!ISDIGIT (*dbp))
    {
      --dbp;			/* force failure */
      return;
    }
  do
    dbp++;
  while (ISDIGIT (*dbp));
}

static void
F_getit (FILE *inf)
{
  register char *cp;

  dbp = skip_spaces (dbp);
  if (*dbp == '\0')
    {
      readline (&lb, inf);
      dbp = lb.buffer;
      if (dbp[5] != '&')
	return;
      dbp += 6;
      dbp = skip_spaces (dbp);
    }
  if (!ISALPHA (*dbp) && *dbp != '_' && *dbp != '$')
    return;
  for (cp = dbp + 1; *cp != '\0' && intoken (*cp); cp++)
    continue;
  make_tag (dbp, cp-dbp, TRUE,
	    lb.buffer, cp - lb.buffer + 1, lineno, linecharno);
}


static void
Fortran_functions (FILE *inf)
{
  LOOP_ON_INPUT_LINES (inf, lb, dbp)
    {
      if (*dbp == '%')
	dbp++;			/* Ratfor escape to fortran */
      dbp = skip_spaces (dbp);
      if (*dbp == '\0')
	continue;

      if (LOOKING_AT_NOCASE (dbp, "recursive"))
	dbp = skip_spaces (dbp);

      if (LOOKING_AT_NOCASE (dbp, "pure"))
	dbp = skip_spaces (dbp);

      if (LOOKING_AT_NOCASE (dbp, "elemental"))
	dbp = skip_spaces (dbp);

      switch (lowcase (*dbp))
	{
	case 'i':
	  if (nocase_tail ("integer"))
	    F_takeprec ();
	  break;
	case 'r':
	  if (nocase_tail ("real"))
	    F_takeprec ();
	  break;
	case 'l':
	  if (nocase_tail ("logical"))
	    F_takeprec ();
	  break;
	case 'c':
	  if (nocase_tail ("complex") || nocase_tail ("character"))
	    F_takeprec ();
	  break;
	case 'd':
	  if (nocase_tail ("double"))
	    {
	      dbp = skip_spaces (dbp);
	      if (*dbp == '\0')
		continue;
	      if (nocase_tail ("precision"))
		break;
	      continue;
	    }
	  break;
	}
      dbp = skip_spaces (dbp);
      if (*dbp == '\0')
	continue;
      switch (lowcase (*dbp))
	{
	case 'f':
	  if (nocase_tail ("function"))
	    F_getit (inf);
	  continue;
	case 's':
	  if (nocase_tail ("subroutine"))
	    F_getit (inf);
	  continue;
	case 'e':
	  if (nocase_tail ("entry"))
	    F_getit (inf);
	  continue;
	case 'b':
	  if (nocase_tail ("blockdata") || nocase_tail ("block data"))
	    {
	      dbp = skip_spaces (dbp);
	      if (*dbp == '\0')	/* assume un-named */
		make_tag ("blockdata", 9, TRUE,
			  lb.buffer, dbp - lb.buffer, lineno, linecharno);
	      else
		F_getit (inf);	/* look for name */
	    }
	  continue;
	}
    }
}


/*
 * Ada parsing
 * Original code by
 * Philippe Waroquiers (1998)
 */

/* Once we are positioned after an "interesting" keyword, let's get
   the real tag value necessary. */
static void
Ada_getit (FILE *inf, const char *name_qualifier)
{
  register char *cp;
  char *name;
  char c;

  while (!feof (inf))
    {
      dbp = skip_spaces (dbp);
      if (*dbp == '\0'
	  || (dbp[0] == '-' && dbp[1] == '-'))
	{
	  readline (&lb, inf);
	  dbp = lb.buffer;
	}
      switch (lowcase (*dbp))
        {
        case 'b':
          if (nocase_tail ("body"))
            {
              /* Skipping body of   procedure body   or   package body or ....
		 resetting qualifier to body instead of spec. */
              name_qualifier = "/b";
              continue;
            }
          break;
        case 't':
          /* Skipping type of   task type   or   protected type ... */
          if (nocase_tail ("type"))
            continue;
          break;
        }
      if (*dbp == '"')
	{
	  dbp += 1;
	  for (cp = dbp; *cp != '\0' && *cp != '"'; cp++)
	    continue;
	}
      else
	{
	  dbp = skip_spaces (dbp);
	  for (cp = dbp;
	       (*cp != '\0'
		&& (ISALPHA (*cp) || ISDIGIT (*cp) || *cp == '_' || *cp == '.'));
	       cp++)
	    continue;
	  if (cp == dbp)
	    return;
	}
      c = *cp;
      *cp = '\0';
      name = concat (dbp, name_qualifier, "");
      *cp = c;
      make_tag (name, strlen (name), TRUE,
		lb.buffer, cp - lb.buffer + 1, lineno, linecharno);
      free (name);
      if (c == '"')
	dbp = cp + 1;
      return;
    }
}

static void
Ada_funcs (FILE *inf)
{
  bool inquote = FALSE;
  bool skip_till_semicolumn = FALSE;

  LOOP_ON_INPUT_LINES (inf, lb, dbp)
    {
      while (*dbp != '\0')
	{
	  /* Skip a string i.e. "abcd". */
	  if (inquote || (*dbp == '"'))
	    {
	      dbp = etags_strchr (dbp + !inquote, '"');
	      if (dbp != NULL)
		{
		  inquote = FALSE;
		  dbp += 1;
		  continue;	/* advance char */
		}
	      else
		{
		  inquote = TRUE;
		  break;	/* advance line */
		}
	    }

	  /* Skip comments. */
	  if (dbp[0] == '-' && dbp[1] == '-')
	    break;		/* advance line */

	  /* Skip character enclosed in single quote i.e. 'a'
	     and skip single quote starting an attribute i.e. 'Image. */
	  if (*dbp == '\'')
	    {
	      dbp++ ;
	      if (*dbp != '\0')
		dbp++;
	      continue;
	    }

	  if (skip_till_semicolumn)
	    {
	      if (*dbp == ';')
		skip_till_semicolumn = FALSE;
	      dbp++;
	      continue;         /* advance char */
	    }

	  /* Search for beginning of a token.  */
	  if (!begtoken (*dbp))
	    {
	      dbp++;
	      continue;		/* advance char */
	    }

	  /* We are at the beginning of a token. */
	  switch (lowcase (*dbp))
	    {
	    case 'f':
	      if (!packages_only && nocase_tail ("function"))
		Ada_getit (inf, "/f");
	      else
		break;		/* from switch */
	      continue;		/* advance char */
	    case 'p':
	      if (!packages_only && nocase_tail ("procedure"))
		Ada_getit (inf, "/p");
	      else if (nocase_tail ("package"))
		Ada_getit (inf, "/s");
	      else if (nocase_tail ("protected")) /* protected type */
		Ada_getit (inf, "/t");
	      else
		break;		/* from switch */
	      continue;		/* advance char */

	    case 'u':
	      if (typedefs && !packages_only && nocase_tail ("use"))
		{
		  /* when tagging types, avoid tagging  use type Pack.Typename;
		     for this, we will skip everything till a ; */
		  skip_till_semicolumn = TRUE;
		  continue;     /* advance char */
		}

	    case 't':
	      if (!packages_only && nocase_tail ("task"))
		Ada_getit (inf, "/k");
	      else if (typedefs && !packages_only && nocase_tail ("type"))
		{
		  Ada_getit (inf, "/t");
		  while (*dbp != '\0')
		    dbp += 1;
		}
	      else
		break;		/* from switch */
	      continue;		/* advance char */
	    }

	  /* Look for the end of the token. */
	  while (!endtoken (*dbp))
	    dbp++;

	} /* advance char */
    } /* advance line */
}


/*
 * Unix and microcontroller assembly tag handling
 * Labels:  /^[a-zA-Z_.$][a-zA_Z0-9_.$]*[: ^I^J]/
 * Idea by Bob Weiner, Motorola Inc. (1994)
 */
static void
Asm_labels (FILE *inf)
{
  register char *cp;

  LOOP_ON_INPUT_LINES (inf, lb, cp)
    {
      /* If first char is alphabetic or one of [_.$], test for colon
	 following identifier. */
      if (ISALPHA (*cp) || *cp == '_' || *cp == '.' || *cp == '$')
 	{
 	  /* Read past label. */
	  cp++;
 	  while (ISALNUM (*cp) || *cp == '_' || *cp == '.' || *cp == '$')
 	    cp++;
 	  if (*cp == ':' || iswhite (*cp))
	    /* Found end of label, so copy it and add it to the table. */
	    make_tag (lb.buffer, cp - lb.buffer, TRUE,
		      lb.buffer, cp - lb.buffer + 1, lineno, linecharno);
 	}
    }
}


/*
 * Perl support
 * Perl sub names: /^sub[ \t\n]+[^ \t\n{]+/
 * Perl variable names: /^(my|local).../
 * Original code by Bart Robinson <lomew@cs.utah.edu> (1995)
 * Additions by Michael Ernst <mernst@alum.mit.edu> (1997)
 * Ideas by Kai Großjohann <Kai.Grossjohann@CS.Uni-Dortmund.DE> (2001)
 */
static void
Perl_functions (FILE *inf)
{
  char *package = savestr ("main"); /* current package name */
  register char *cp;

  LOOP_ON_INPUT_LINES (inf, lb, cp)
    {
      cp = skip_spaces (cp);

      if (LOOKING_AT (cp, "package"))
	{
	  free (package);
	  get_tag (cp, &package);
	}
      else if (LOOKING_AT (cp, "sub"))
	{
	  char *pos;
	  char *sp = cp;

	  while (!notinname (*cp))
	    cp++;
	  if (cp == sp)
	    continue;		/* nothing found */
	  if ((pos = etags_strchr (sp, ':')) != NULL
	      && pos < cp && pos[1] == ':')
	    /* The name is already qualified. */
	    make_tag (sp, cp - sp, TRUE,
		      lb.buffer, cp - lb.buffer + 1, lineno, linecharno);
	  else
	    /* Qualify it. */
	    {
	      char savechar, *name;

	      savechar = *cp;
	      *cp = '\0';
	      name = concat (package, "::", sp);
	      *cp = savechar;
	      make_tag (name, strlen (name), TRUE,
			lb.buffer, cp - lb.buffer + 1, lineno, linecharno);
	      free (name);
	    }
 	}
       else if (globals)	/* only if we are tagging global vars */
 	{
	  /* Skip a qualifier, if any. */
	  bool qual = LOOKING_AT (cp, "my") || LOOKING_AT (cp, "local");
 	  /* After "my" or "local", but before any following paren or space. */
	  char *varstart = cp;

 	  if (qual		/* should this be removed?  If yes, how? */
	      && (*cp == '$' || *cp == '@' || *cp == '%'))
 	    {
 	      varstart += 1;
	      do
 		cp++;
 	      while (ISALNUM (*cp) || *cp == '_');
 	    }
 	  else if (qual)
 	    {
 	      /* Should be examining a variable list at this point;
 		 could insist on seeing an open parenthesis. */
 	      while (*cp != '\0' && *cp != ';' && *cp != '=' &&  *cp != ')')
 		cp++;
 	    }
	  else
	    continue;

 	  make_tag (varstart, cp - varstart, FALSE,
		    lb.buffer, cp - lb.buffer + 1, lineno, linecharno);
	}
    }
  free (package);
}


/*
 * Python support
 * Look for /^[\t]*def[ \t\n]+[^ \t\n(:]+/ or /^class[ \t\n]+[^ \t\n(:]+/
 * Idea by Eric S. Raymond <esr@thyrsus.com> (1997)
 * More ideas by seb bacon <seb@jamkit.com> (2002)
 */
static void
Python_functions (FILE *inf)
{
  register char *cp;

  LOOP_ON_INPUT_LINES (inf, lb, cp)
    {
      cp = skip_spaces (cp);
      if (LOOKING_AT (cp, "def") || LOOKING_AT (cp, "class"))
	{
	  char *name = cp;
	  while (!notinname (*cp) && *cp != ':')
	    cp++;
	  make_tag (name, cp - name, TRUE,
		    lb.buffer, cp - lb.buffer + 1, lineno, linecharno);
	}
    }
}


/*
 * PHP support
 * Look for:
 *  - /^[ \t]*function[ \t\n]+[^ \t\n(]+/
 *  - /^[ \t]*class[ \t\n]+[^ \t\n]+/
 *  - /^[ \t]*define\(\"[^\"]+/
 * Only with --members:
 *  - /^[ \t]*var[ \t\n]+\$[^ \t\n=;]/
 * Idea by Diez B. Roggisch (2001)
 */
static void
PHP_functions (FILE *inf)
{
  register char *cp, *name;
  bool search_identifier = FALSE;

  LOOP_ON_INPUT_LINES (inf, lb, cp)
    {
      cp = skip_spaces (cp);
      name = cp;
      if (search_identifier
	  && *cp != '\0')
	{
	  while (!notinname (*cp))
	    cp++;
	  make_tag (name, cp - name, TRUE,
		    lb.buffer, cp - lb.buffer + 1, lineno, linecharno);
	  search_identifier = FALSE;
	}
      else if (LOOKING_AT (cp, "function"))
	{
	  if (*cp == '&')
	    cp = skip_spaces (cp+1);
	  if (*cp != '\0')
	    {
	      name = cp;
	      while (!notinname (*cp))
		cp++;
	      make_tag (name, cp - name, TRUE,
			lb.buffer, cp - lb.buffer + 1, lineno, linecharno);
	    }
	  else
	    search_identifier = TRUE;
	}
      else if (LOOKING_AT (cp, "class"))
	{
	  if (*cp != '\0')
	    {
	      name = cp;
	      while (*cp != '\0' && !iswhite (*cp))
		cp++;
	      make_tag (name, cp - name, FALSE,
			lb.buffer, cp - lb.buffer + 1, lineno, linecharno);
	    }
	  else
	    search_identifier = TRUE;
	}
      else if (strneq (cp, "define", 6)
	       && (cp = skip_spaces (cp+6))
	       && *cp++ == '('
	       && (*cp == '"' || *cp == '\''))
	{
	  char quote = *cp++;
	  name = cp;
	  while (*cp != quote && *cp != '\0')
	    cp++;
	  make_tag (name, cp - name, FALSE,
		    lb.buffer, cp - lb.buffer + 1, lineno, linecharno);
	}
      else if (members
	       && LOOKING_AT (cp, "var")
	       && *cp == '$')
	{
	  name = cp;
	  while (!notinname (*cp))
	    cp++;
	  make_tag (name, cp - name, FALSE,
		    lb.buffer, cp - lb.buffer + 1, lineno, linecharno);
	}
    }
}


/*
 * Cobol tag functions
 * We could look for anything that could be a paragraph name.
 * i.e. anything that starts in column 8 is one word and ends in a full stop.
 * Idea by Corny de Souza (1993)
 */
static void
Cobol_paragraphs (FILE *inf)
{
  register char *bp, *ep;

  LOOP_ON_INPUT_LINES (inf, lb, bp)
    {
      if (lb.len < 9)
	continue;
      bp += 8;

      /* If eoln, compiler option or comment ignore whole line. */
      if (bp[-1] != ' ' || !ISALNUM (bp[0]))
        continue;

      for (ep = bp; ISALNUM (*ep) || *ep == '-'; ep++)
	continue;
      if (*ep++ == '.')
	make_tag (bp, ep - bp, TRUE,
		  lb.buffer, ep - lb.buffer + 1, lineno, linecharno);
    }
}


/*
 * Makefile support
 * Ideas by Assar Westerlund <assar@sics.se> (2001)
 */
static void
Makefile_targets (FILE *inf)
{
  register char *bp;

  LOOP_ON_INPUT_LINES (inf, lb, bp)
    {
      if (*bp == '\t' || *bp == '#')
	continue;
      while (*bp != '\0' && *bp != '=' && *bp != ':')
	bp++;
      if (*bp == ':' || (globals && *bp == '='))
	{
	  /* We should detect if there is more than one tag, but we do not.
	     We just skip initial and final spaces. */
	  char * namestart = skip_spaces (lb.buffer);
	  while (--bp > namestart)
	    if (!notinname (*bp))
	      break;
	  make_tag (namestart, bp - namestart + 1, TRUE,
		    lb.buffer, bp - lb.buffer + 2, lineno, linecharno);
	}
    }
}


/*
 * Pascal parsing
 * Original code by Mosur K. Mohan (1989)
 *
 *  Locates tags for procedures & functions.  Doesn't do any type- or
 *  var-definitions.  It does look for the keyword "extern" or
 *  "forward" immediately following the procedure statement; if found,
 *  the tag is skipped.
 */
static void
Pascal_functions (FILE *inf)
{
  linebuffer tline;		/* mostly copied from C_entries */
  long save_lcno;
  int save_lineno, namelen, taglen;
  char c, *name;

  bool				/* each of these flags is TRUE if: */
    incomment,			/* point is inside a comment */
    inquote,			/* point is inside '..' string */
    get_tagname,		/* point is after PROCEDURE/FUNCTION
				   keyword, so next item = potential tag */
    found_tag,			/* point is after a potential tag */
    inparms,			/* point is within parameter-list */
    verify_tag;			/* point has passed the parm-list, so the
				   next token will determine whether this
				   is a FORWARD/EXTERN to be ignored, or
				   whether it is a real tag */

  save_lcno = save_lineno = namelen = taglen = 0; /* keep compiler quiet */
  name = NULL;			/* keep compiler quiet */
  dbp = lb.buffer;
  *dbp = '\0';
  linebuffer_init (&tline);

  incomment = inquote = FALSE;
  found_tag = FALSE;		/* have a proc name; check if extern */
  get_tagname = FALSE;		/* found "procedure" keyword	     */
  inparms = FALSE;		/* found '(' after "proc"            */
  verify_tag = FALSE;		/* check if "extern" is ahead        */


  while (!feof (inf))		/* long main loop to get next char */
    {
      c = *dbp++;
      if (c == '\0')		/* if end of line */
	{
	  readline (&lb, inf);
	  dbp = lb.buffer;
	  if (*dbp == '\0')
	    continue;
	  if (!((found_tag && verify_tag)
		|| get_tagname))
	    c = *dbp++;		/* only if don't need *dbp pointing
				   to the beginning of the name of
				   the procedure or function */
	}
      if (incomment)
	{
	  if (c == '}')		/* within { } comments */
	    incomment = FALSE;
	  else if (c == '*' && *dbp == ')') /* within (* *) comments */
	    {
	      dbp++;
	      incomment = FALSE;
	    }
	  continue;
	}
      else if (inquote)
	{
	  if (c == '\'')
	    inquote = FALSE;
	  continue;
	}
      else
	switch (c)
	  {
	  case '\'':
	    inquote = TRUE;	/* found first quote */
	    continue;
	  case '{':		/* found open { comment */
	    incomment = TRUE;
	    continue;
	  case '(':
	    if (*dbp == '*')	/* found open (* comment */
	      {
		incomment = TRUE;
		dbp++;
	      }
	    else if (found_tag)	/* found '(' after tag, i.e., parm-list */
	      inparms = TRUE;
	    continue;
	  case ')':		/* end of parms list */
	    if (inparms)
	      inparms = FALSE;
	    continue;
	  case ';':
	    if (found_tag && !inparms) /* end of proc or fn stmt */
	      {
		verify_tag = TRUE;
		break;
	      }
	    continue;
	  }
      if (found_tag && verify_tag && (*dbp != ' '))
	{
	  /* Check if this is an "extern" declaration. */
	  if (*dbp == '\0')
	    continue;
	  if (lowcase (*dbp == 'e'))
	    {
	      if (nocase_tail ("extern")) /* superfluous, really! */
		{
		  found_tag = FALSE;
		  verify_tag = FALSE;
		}
	    }
	  else if (lowcase (*dbp) == 'f')
	    {
	      if (nocase_tail ("forward")) /* check for forward reference */
		{
		  found_tag = FALSE;
		  verify_tag = FALSE;
		}
	    }
	  if (found_tag && verify_tag) /* not external proc, so make tag */
	    {
	      found_tag = FALSE;
	      verify_tag = FALSE;
	      make_tag (name, namelen, TRUE,
			tline.buffer, taglen, save_lineno, save_lcno);
	      continue;
	    }
	}
      if (get_tagname)		/* grab name of proc or fn */
	{
	  char *cp;

	  if (*dbp == '\0')
	    continue;

	  /* Find block name. */
	  for (cp = dbp + 1; *cp != '\0' && !endtoken (*cp); cp++)
	    continue;

	  /* Save all values for later tagging. */
	  linebuffer_setlen (&tline, lb.len);
	  strcpy (tline.buffer, lb.buffer);
	  save_lineno = lineno;
	  save_lcno = linecharno;
	  name = tline.buffer + (dbp - lb.buffer);
	  namelen = cp - dbp;
	  taglen = cp - lb.buffer + 1;

	  dbp = cp;		/* set dbp to e-o-token */
	  get_tagname = FALSE;
	  found_tag = TRUE;
	  continue;

	  /* And proceed to check for "extern". */
	}
      else if (!incomment && !inquote && !found_tag)
	{
	  /* Check for proc/fn keywords. */
	  switch (lowcase (c))
	    {
	    case 'p':
	      if (nocase_tail ("rocedure")) /* c = 'p', dbp has advanced */
		get_tagname = TRUE;
	      continue;
	    case 'f':
	      if (nocase_tail ("unction"))
		get_tagname = TRUE;
	      continue;
	    }
	}
    } /* while not eof */

  free (tline.buffer);
}


/*
 * Lisp tag functions
 *  look for (def or (DEF, quote or QUOTE
 */

static void L_getit (void);

static void
L_getit (void)
{
  if (*dbp == '\'')		/* Skip prefix quote */
    dbp++;
  else if (*dbp == '(')
  {
    dbp++;
    /* Try to skip "(quote " */
    if (!LOOKING_AT (dbp, "quote") && !LOOKING_AT (dbp, "QUOTE"))
      /* Ok, then skip "(" before name in (defstruct (foo)) */
      dbp = skip_spaces (dbp);
  }
  get_tag (dbp, NULL);
}

static void
Lisp_functions (FILE *inf)
{
  LOOP_ON_INPUT_LINES (inf, lb, dbp)
    {
      if (dbp[0] != '(')
	continue;

      if (strneq (dbp+1, "def", 3) || strneq (dbp+1, "DEF", 3))
	{
	  dbp = skip_non_spaces (dbp);
	  dbp = skip_spaces (dbp);
	  L_getit ();
	}
      else
	{
	  /* Check for (foo::defmumble name-defined ... */
	  do
	    dbp++;
	  while (!notinname (*dbp) && *dbp != ':');
	  if (*dbp == ':')
	    {
	      do
		dbp++;
	      while (*dbp == ':');

	      if (strneq (dbp, "def", 3) || strneq (dbp, "DEF", 3))
		{
		  dbp = skip_non_spaces (dbp);
		  dbp = skip_spaces (dbp);
		  L_getit ();
		}
	    }
	}
    }
}


/*
 * Lua script language parsing
 * Original code by David A. Capello <dacap@users.sourceforge.net> (2004)
 *
 *  "function" and "local function" are tags if they start at column 1.
 */
static void
Lua_functions (FILE *inf)
{
  register char *bp;

  LOOP_ON_INPUT_LINES (inf, lb, bp)
    {
      if (bp[0] != 'f' && bp[0] != 'l')
	continue;

      (void)LOOKING_AT (bp, "local"); /* skip possible "local" */

      if (LOOKING_AT (bp, "function"))
	get_tag (bp, NULL);
    }
}


/*
 * PostScript tags
 * Just look for lines where the first character is '/'
 * Also look at "defineps" for PSWrap
 * Ideas by:
 *   Richard Mlynarik <mly@adoc.xerox.com> (1997)
 *   Masatake Yamato <masata-y@is.aist-nara.ac.jp> (1999)
 */
static void
PS_functions (FILE *inf)
{
  register char *bp, *ep;

  LOOP_ON_INPUT_LINES (inf, lb, bp)
    {
      if (bp[0] == '/')
	{
	  for (ep = bp+1;
	       *ep != '\0' && *ep != ' ' && *ep != '{';
	       ep++)
	    continue;
	  make_tag (bp, ep - bp, TRUE,
		    lb.buffer, ep - lb.buffer + 1, lineno, linecharno);
	}
      else if (LOOKING_AT (bp, "defineps"))
	get_tag (bp, NULL);
    }
}


/*
 * Forth tags
 * Ignore anything after \ followed by space or in ( )
 * Look for words defined by :
 * Look for constant, code, create, defer, value, and variable
 * OBP extensions:  Look for buffer:, field,
 * Ideas by Eduardo Horvath <eeh@netbsd.org> (2004)
 */
static void
Forth_words (FILE *inf)
{
  register char *bp;

  LOOP_ON_INPUT_LINES (inf, lb, bp)
    while ((bp = skip_spaces (bp))[0] != '\0')
      if (bp[0] == '\\' && iswhite (bp[1]))
	break;			/* read next line */
      else if (bp[0] == '(' && iswhite (bp[1]))
	do			/* skip to ) or eol */
	  bp++;
	while (*bp != ')' && *bp != '\0');
      else if ((bp[0] == ':' && iswhite (bp[1]) && bp++)
	       || LOOKING_AT_NOCASE (bp, "constant")
	       || LOOKING_AT_NOCASE (bp, "code")
	       || LOOKING_AT_NOCASE (bp, "create")
	       || LOOKING_AT_NOCASE (bp, "defer")
	       || LOOKING_AT_NOCASE (bp, "value")
	       || LOOKING_AT_NOCASE (bp, "variable")
	       || LOOKING_AT_NOCASE (bp, "buffer:")
	       || LOOKING_AT_NOCASE (bp, "field"))
	get_tag (skip_spaces (bp), NULL); /* Yay!  A definition! */
      else
	bp = skip_non_spaces (bp);
}


/*
 * Scheme tag functions
 * look for (def... xyzzy
 *          (def... (xyzzy
 *          (def ... ((...(xyzzy ....
 *          (set! xyzzy
 * Original code by Ken Haase (1985?)
 */
static void
Scheme_functions (FILE *inf)
{
  register char *bp;

  LOOP_ON_INPUT_LINES (inf, lb, bp)
    {
      if (strneq (bp, "(def", 4) || strneq (bp, "(DEF", 4))
	{
	  bp = skip_non_spaces (bp+4);
	  /* Skip over open parens and white space.  Don't continue past
	     '\0'. */
	  while (*bp && notinname (*bp))
	    bp++;
	  get_tag (bp, NULL);
	}
      if (LOOKING_AT (bp, "(SET!") || LOOKING_AT (bp, "(set!"))
	get_tag (bp, NULL);
    }
}


/* Find tags in TeX and LaTeX input files.  */

/* TEX_toktab is a table of TeX control sequences that define tags.
 * Each entry records one such control sequence.
 *
 * Original code from who knows whom.
 * Ideas by:
 *   Stefan Monnier (2002)
 */

static linebuffer *TEX_toktab = NULL; /* Table with tag tokens */

/* Default set of control sequences to put into TEX_toktab.
   The value of environment var TEXTAGS is prepended to this.  */
static const char *TEX_defenv = "\
:chapter:section:subsection:subsubsection:eqno:label:ref:cite:bibitem\
:part:appendix:entry:index:def\
:newcommand:renewcommand:newenvironment:renewenvironment";

static void TEX_mode (FILE *);
static void TEX_decode_env (const char *, const char *);

static char TEX_esc = '\\';
static char TEX_opgrp = '{';
static char TEX_clgrp = '}';

/*
 * TeX/LaTeX scanning loop.
 */
static void
TeX_commands (FILE *inf)
{
  char *cp;
  linebuffer *key;

  /* Select either \ or ! as escape character.  */
  TEX_mode (inf);

  /* Initialize token table once from environment. */
  if (TEX_toktab == NULL)
    TEX_decode_env ("TEXTAGS", TEX_defenv);

  LOOP_ON_INPUT_LINES (inf, lb, cp)
    {
      /* Look at each TEX keyword in line. */
      for (;;)
	{
	  /* Look for a TEX escape. */
	  while (*cp++ != TEX_esc)
	    if (cp[-1] == '\0' || cp[-1] == '%')
	      goto tex_next_line;

	  for (key = TEX_toktab; key->buffer != NULL; key++)
	    if (strneq (cp, key->buffer, key->len))
	      {
		register char *p;
		int namelen, linelen;
		bool opgrp = FALSE;

		cp = skip_spaces (cp + key->len);
		if (*cp == TEX_opgrp)
		  {
		    opgrp = TRUE;
		    cp++;
		  }
		for (p = cp;
		     (!iswhite (*p) && *p != '#' &&
		      *p != TEX_opgrp && *p != TEX_clgrp);
		     p++)
		  continue;
		namelen = p - cp;
		linelen = lb.len;
		if (!opgrp || *p == TEX_clgrp)
		  {
		    while (*p != '\0' && *p != TEX_opgrp && *p != TEX_clgrp)
		      p++;
		    linelen = p - lb.buffer + 1;
		  }
		make_tag (cp, namelen, TRUE,
			  lb.buffer, linelen, lineno, linecharno);
		goto tex_next_line; /* We only tag a line once */
	      }
	}
    tex_next_line:
      ;
    }
}

#define TEX_LESC '\\'
#define TEX_SESC '!'

/* Figure out whether TeX's escapechar is '\\' or '!' and set grouping
   chars accordingly. */
static void
TEX_mode (FILE *inf)
{
  int c;

  while ((c = getc (inf)) != EOF)
    {
      /* Skip to next line if we hit the TeX comment char. */
      if (c == '%')
	while (c != '\n' && c != EOF)
	  c = getc (inf);
      else if (c == TEX_LESC || c == TEX_SESC )
	break;
    }

  if (c == TEX_LESC)
    {
      TEX_esc = TEX_LESC;
      TEX_opgrp = '{';
      TEX_clgrp = '}';
    }
  else
    {
      TEX_esc = TEX_SESC;
      TEX_opgrp = '<';
      TEX_clgrp = '>';
    }
  /* If the input file is compressed, inf is a pipe, and rewind may fail.
     No attempt is made to correct the situation. */
  rewind (inf);
}

/* Read environment and prepend it to the default string.
   Build token table. */
static void
TEX_decode_env (const char *evarname, const char *defenv)
{
  register const char *env, *p;
  int i, len;

  /* Append default string to environment. */
  env = getenv (evarname);
  if (!env)
    env = defenv;
  else
    env = concat (env, defenv, "");

  /* Allocate a token table */
  for (len = 1, p = env; p;)
    if ((p = etags_strchr (p, ':')) && *++p != '\0')
      len++;
  TEX_toktab = xnew (len, linebuffer);

  /* Unpack environment string into token table. Be careful about */
  /* zero-length strings (leading ':', "::" and trailing ':') */
  for (i = 0; *env != '\0';)
    {
      p = etags_strchr (env, ':');
      if (!p)			/* End of environment string. */
	p = env + strlen (env);
      if (p - env > 0)
	{			/* Only non-zero strings. */
	  TEX_toktab[i].buffer = savenstr (env, p - env);
	  TEX_toktab[i].len = p - env;
	  i++;
	}
      if (*p)
	env = p + 1;
      else
	{
	  TEX_toktab[i].buffer = NULL; /* Mark end of table. */
	  TEX_toktab[i].len = 0;
	  break;
	}
    }
}


/* Texinfo support.  Dave Love, Mar. 2000.  */
static void
Texinfo_nodes (FILE *inf)
{
  char *cp, *start;
  LOOP_ON_INPUT_LINES (inf, lb, cp)
    if (LOOKING_AT (cp, "@node"))
      {
	start = cp;
	while (*cp != '\0' && *cp != ',')
	  cp++;
	make_tag (start, cp - start, TRUE,
		  lb.buffer, cp - lb.buffer + 1, lineno, linecharno);
      }
}


/*
 * HTML support.
 * Contents of <title>, <h1>, <h2>, <h3> are tags.
 * Contents of <a name=xxx> are tags with name xxx.
 *
 * Francesco Potortì, 2002.
 */
static void
HTML_labels (FILE *inf)
{
  bool getnext = FALSE;		/* next text outside of HTML tags is a tag */
  bool skiptag = FALSE;		/* skip to the end of the current HTML tag */
  bool intag = FALSE;		/* inside an html tag, looking for ID= */
  bool inanchor = FALSE;	/* when INTAG, is an anchor, look for NAME= */
  char *end;


  linebuffer_setlen (&token_name, 0); /* no name in buffer */

  LOOP_ON_INPUT_LINES (inf, lb, dbp)
    for (;;)			/* loop on the same line */
      {
	if (skiptag)		/* skip HTML tag */
	  {
	    while (*dbp != '\0' && *dbp != '>')
	      dbp++;
	    if (*dbp == '>')
	      {
		dbp += 1;
		skiptag = FALSE;
		continue;	/* look on the same line */
	      }
	    break;		/* go to next line */
	  }

	else if (intag)	/* look for "name=" or "id=" */
	  {
	    while (*dbp != '\0' && *dbp != '>'
		   && lowcase (*dbp) != 'n' && lowcase (*dbp) != 'i')
	      dbp++;
	    if (*dbp == '\0')
	      break;		/* go to next line */
	    if (*dbp == '>')
	      {
		dbp += 1;
		intag = FALSE;
		continue;	/* look on the same line */
	      }
	    if ((inanchor && LOOKING_AT_NOCASE (dbp, "name="))
		|| LOOKING_AT_NOCASE (dbp, "id="))
	      {
		bool quoted = (dbp[0] == '"');

		if (quoted)
		  for (end = ++dbp; *end != '\0' && *end != '"'; end++)
		    continue;
		else
		  for (end = dbp; *end != '\0' && intoken (*end); end++)
		    continue;
		linebuffer_setlen (&token_name, end - dbp);
		strncpy (token_name.buffer, dbp, end - dbp);
		token_name.buffer[end - dbp] = '\0';

		dbp = end;
		intag = FALSE;	/* we found what we looked for */
		skiptag = TRUE; /* skip to the end of the tag */
		getnext = TRUE;	/* then grab the text */
		continue;	/* look on the same line */
	      }
	    dbp += 1;
	  }

	else if (getnext)	/* grab next tokens and tag them */
	  {
	    dbp = skip_spaces (dbp);
	    if (*dbp == '\0')
	      break;		/* go to next line */
	    if (*dbp == '<')
	      {
		intag = TRUE;
		inanchor = (lowcase (dbp[1]) == 'a' && !intoken (dbp[2]));
		continue;	/* look on the same line */
	      }

	    for (end = dbp + 1; *end != '\0' && *end != '<'; end++)
	      continue;
	    make_tag (token_name.buffer, token_name.len, TRUE,
		      dbp, end - dbp, lineno, linecharno);
	    linebuffer_setlen (&token_name, 0);	/* no name in buffer */
	    getnext = FALSE;
	    break;		/* go to next line */
	  }

	else			/* look for an interesting HTML tag */
	  {
	    while (*dbp != '\0' && *dbp != '<')
	      dbp++;
	    if (*dbp == '\0')
	      break;		/* go to next line */
	    intag = TRUE;
	    if (lowcase (dbp[1]) == 'a' && !intoken (dbp[2]))
	      {
		inanchor = TRUE;
		continue;	/* look on the same line */
	      }
	    else if (LOOKING_AT_NOCASE (dbp, "<title>")
		     || LOOKING_AT_NOCASE (dbp, "<h1>")
		     || LOOKING_AT_NOCASE (dbp, "<h2>")
		     || LOOKING_AT_NOCASE (dbp, "<h3>"))
	      {
		intag = FALSE;
		getnext = TRUE;
		continue;	/* look on the same line */
	      }
	    dbp += 1;
	  }
      }
}


/*
 * Prolog support
 *
 * Assumes that the predicate or rule starts at column 0.
 * Only the first clause of a predicate or rule is added.
 * Original code by Sunichirou Sugou (1989)
 * Rewritten by Anders Lindgren (1996)
 */
static size_t prolog_pr (char *, char *);
static void prolog_skip_comment (linebuffer *, FILE *);
static size_t prolog_atom (char *, size_t);

static void
Prolog_functions (FILE *inf)
{
  char *cp, *last;
  size_t len;
  size_t allocated;

  allocated = 0;
  len = 0;
  last = NULL;

  LOOP_ON_INPUT_LINES (inf, lb, cp)
    {
      if (cp[0] == '\0')	/* Empty line */
	continue;
      else if (iswhite (cp[0])) /* Not a predicate */
	continue;
      else if (cp[0] == '/' && cp[1] == '*')	/* comment. */
	prolog_skip_comment (&lb, inf);
      else if ((len = prolog_pr (cp, last)) > 0)
	{
	  /* Predicate or rule.  Store the function name so that we
	     only generate a tag for the first clause.  */
	  if (last == NULL)
	    last = xnew (len + 1, char);
	  else if (len + 1 > allocated)
	    xrnew (last, len + 1, char);
	  allocated = len + 1;
	  strncpy (last, cp, len);
	  last[len] = '\0';
	}
    }
  free (last);
}


static void
prolog_skip_comment (linebuffer *plb, FILE *inf)
{
  char *cp;

  do
    {
      for (cp = plb->buffer; *cp != '\0'; cp++)
	if (cp[0] == '*' && cp[1] == '/')
	  return;
      readline (plb, inf);
    }
  while (!feof (inf));
}

/*
 * A predicate or rule definition is added if it matches:
 *     <beginning of line><Prolog Atom><whitespace>(
 * or  <beginning of line><Prolog Atom><whitespace>:-
 *
 * It is added to the tags database if it doesn't match the
 * name of the previous clause header.
 *
 * Return the size of the name of the predicate or rule, or 0 if no
 * header was found.
 */
static size_t
prolog_pr (char *s, char *last)

                		/* Name of last clause. */
{
  size_t pos;
  size_t len;

  pos = prolog_atom (s, 0);
  if (! pos)
    return 0;

  len = pos;
  pos = skip_spaces (s + pos) - s;

  if ((s[pos] == '.'
       || (s[pos] == '(' && (pos += 1))
       || (s[pos] == ':' && s[pos + 1] == '-' && (pos += 2)))
      && (last == NULL		/* save only the first clause */
	  || len != strlen (last)
	  || !strneq (s, last, len)))
	{
	  make_tag (s, len, TRUE, s, pos, lineno, linecharno);
	  return len;
	}
  else
    return 0;
}

/*
 * Consume a Prolog atom.
 * Return the number of bytes consumed, or 0 if there was an error.
 *
 * A prolog atom, in this context, could be one of:
 * - An alphanumeric sequence, starting with a lower case letter.
 * - A quoted arbitrary string. Single quotes can escape themselves.
 *   Backslash quotes everything.
 */
static size_t
prolog_atom (char *s, size_t pos)
{
  size_t origpos;

  origpos = pos;

  if (ISLOWER (s[pos]) || (s[pos] == '_'))
    {
      /* The atom is unquoted. */
      pos++;
      while (ISALNUM (s[pos]) || (s[pos] == '_'))
	{
	  pos++;
	}
      return pos - origpos;
    }
  else if (s[pos] == '\'')
    {
      pos++;

      for (;;)
	{
	  if (s[pos] == '\'')
	    {
	      pos++;
	      if (s[pos] != '\'')
		break;
	      pos++;		/* A double quote */
	    }
	  else if (s[pos] == '\0')
	    /* Multiline quoted atoms are ignored. */
	    return 0;
	  else if (s[pos] == '\\')
	    {
	      if (s[pos+1] == '\0')
		return 0;
	      pos += 2;
	    }
	  else
	    pos++;
	}
      return pos - origpos;
    }
  else
    return 0;
}


/*
 * Support for Erlang
 *
 * Generates tags for functions, defines, and records.
 * Assumes that Erlang functions start at column 0.
 * Original code by Anders Lindgren (1996)
 */
static int erlang_func (char *, char *);
static void erlang_attribute (char *);
static int erlang_atom (char *);

static void
Erlang_functions (FILE *inf)
{
  char *cp, *last;
  int len;
  int allocated;

  allocated = 0;
  len = 0;
  last = NULL;

  LOOP_ON_INPUT_LINES (inf, lb, cp)
    {
      if (cp[0] == '\0')	/* Empty line */
	continue;
      else if (iswhite (cp[0])) /* Not function nor attribute */
	continue;
      else if (cp[0] == '%')	/* comment */
	continue;
      else if (cp[0] == '"')	/* Sometimes, strings start in column one */
	continue;
      else if (cp[0] == '-') 	/* attribute, e.g. "-define" */
	{
	  erlang_attribute (cp);
	  if (last != NULL)
	    {
	      free (last);
	      last = NULL;
	    }
	}
      else if ((len = erlang_func (cp, last)) > 0)
	{
	  /*
	   * Function.  Store the function name so that we only
	   * generates a tag for the first clause.
	   */
	  if (last == NULL)
	    last = xnew (len + 1, char);
	  else if (len + 1 > allocated)
	    xrnew (last, len + 1, char);
	  allocated = len + 1;
	  strncpy (last, cp, len);
	  last[len] = '\0';
	}
    }
  free (last);
}


/*
 * A function definition is added if it matches:
 *     <beginning of line><Erlang Atom><whitespace>(
 *
 * It is added to the tags database if it doesn't match the
 * name of the previous clause header.
 *
 * Return the size of the name of the function, or 0 if no function
 * was found.
 */
static int
erlang_func (char *s, char *last)

                		/* Name of last clause. */
{
  int pos;
  int len;

  pos = erlang_atom (s);
  if (pos < 1)
    return 0;

  len = pos;
  pos = skip_spaces (s + pos) - s;

  /* Save only the first clause. */
  if (s[pos++] == '('
      && (last == NULL
	  || len != (int)strlen (last)
	  || !strneq (s, last, len)))
	{
	  make_tag (s, len, TRUE, s, pos, lineno, linecharno);
	  return len;
	}

  return 0;
}


/*
 * Handle attributes.  Currently, tags are generated for defines
 * and records.
 *
 * They are on the form:
 * -define(foo, bar).
 * -define(Foo(M, N), M+N).
 * -record(graph, {vtab = notable, cyclic = true}).
 */
static void
erlang_attribute (char *s)
{
  char *cp = s;

  if ((LOOKING_AT (cp, "-define") || LOOKING_AT (cp, "-record"))
      && *cp++ == '(')
    {
      int len = erlang_atom (skip_spaces (cp));
      if (len > 0)
	make_tag (cp, len, TRUE, s, cp + len - s, lineno, linecharno);
    }
  return;
}


/*
 * Consume an Erlang atom (or variable).
 * Return the number of bytes consumed, or -1 if there was an error.
 */
static int
erlang_atom (char *s)
{
  int pos = 0;

  if (ISALPHA (s[pos]) || s[pos] == '_')
    {
      /* The atom is unquoted. */
      do
	pos++;
      while (ISALNUM (s[pos]) || s[pos] == '_');
    }
  else if (s[pos] == '\'')
    {
      for (pos++; s[pos] != '\''; pos++)
	if (s[pos] == '\0'	/* multiline quoted atoms are ignored */
	    || (s[pos] == '\\' && s[++pos] == '\0'))
	  return 0;
      pos++;
    }

  return pos;
}


static char *scan_separators (char *);
static void add_regex (char *, language *);
static char *substitute (char *, char *, struct re_registers *);

/*
 * Take a string like "/blah/" and turn it into "blah", verifying
 * that the first and last characters are the same, and handling
 * quoted separator characters.  Actually, stops on the occurrence of
 * an unquoted separator.  Also process \t, \n, etc. and turn into
 * appropriate characters. Works in place.  Null terminates name string.
 * Returns pointer to terminating separator, or NULL for
 * unterminated regexps.
 */
static char *
scan_separators (char *name)
{
  char sep = name[0];
  char *copyto = name;
  bool quoted = FALSE;

  for (++name; *name != '\0'; ++name)
    {
      if (quoted)
	{
	  switch (*name)
	    {
	    case 'a': *copyto++ = '\007'; break; /* BEL (bell)		 */
	    case 'b': *copyto++ = '\b'; break;	 /* BS (back space)	 */
	    case 'd': *copyto++ = 0177; break;	 /* DEL (delete)	 */
	    case 'e': *copyto++ = 033; break;	 /* ESC (delete)	 */
	    case 'f': *copyto++ = '\f'; break;	 /* FF (form feed)	 */
	    case 'n': *copyto++ = '\n'; break;	 /* NL (new line)	 */
	    case 'r': *copyto++ = '\r'; break;	 /* CR (carriage return) */
	    case 't': *copyto++ = '\t'; break;	 /* TAB (horizontal tab) */
	    case 'v': *copyto++ = '\v'; break;	 /* VT (vertical tab)    */
	    default:
	      if (*name == sep)
		*copyto++ = sep;
	      else
		{
		  /* Something else is quoted, so preserve the quote. */
		  *copyto++ = '\\';
		  *copyto++ = *name;
		}
	      break;
	    }
	  quoted = FALSE;
	}
      else if (*name == '\\')
	quoted = TRUE;
      else if (*name == sep)
	break;
      else
	*copyto++ = *name;
    }
  if (*name != sep)
    name = NULL;		/* signal unterminated regexp */

  /* Terminate copied string. */
  *copyto = '\0';
  return name;
}

/* Look at the argument of --regex or --no-regex and do the right
   thing.  Same for each line of a regexp file. */
static void
analyse_regex (char *regex_arg)
{
  if (regex_arg == NULL)
    {
      free_regexps ();		/* --no-regex: remove existing regexps */
      return;
    }

  /* A real --regexp option or a line in a regexp file. */
  switch (regex_arg[0])
    {
      /* Comments in regexp file or null arg to --regex. */
    case '\0':
    case ' ':
    case '\t':
      break;

      /* Read a regex file.  This is recursive and may result in a
	 loop, which will stop when the file descriptors are exhausted. */
    case '@':
      {
	FILE *regexfp;
	linebuffer regexbuf;
	char *regexfile = regex_arg + 1;

	/* regexfile is a file containing regexps, one per line. */
	regexfp = fopen (regexfile, "r");
	if (regexfp == NULL)
	  {
	    pfatal (regexfile);
	    return;
	  }
	linebuffer_init (&regexbuf);
	while (readline_internal (&regexbuf, regexfp) > 0)
	  analyse_regex (regexbuf.buffer);
	free (regexbuf.buffer);
	fclose (regexfp);
      }
      break;

      /* Regexp to be used for a specific language only. */
    case '{':
      {
	language *lang;
	char *lang_name = regex_arg + 1;
	char *cp;

	for (cp = lang_name; *cp != '}'; cp++)
	  if (*cp == '\0')
	    {
	      error ("unterminated language name in regex: %s", regex_arg);
	      return;
	    }
	*cp++ = '\0';
	lang = get_language_from_langname (lang_name);
	if (lang == NULL)
	  return;
	add_regex (cp, lang);
      }
      break;

      /* Regexp to be used for any language. */
    default:
      add_regex (regex_arg, NULL);
      break;
    }
}

/* Separate the regexp pattern, compile it,
   and care for optional name and modifiers. */
static void
add_regex (char *regexp_pattern, language *lang)
{
  static struct re_pattern_buffer zeropattern;
  char sep, *pat, *name, *modifiers;
  char empty[] = "";
  const char *err;
  struct re_pattern_buffer *patbuf;
  regexp *rp;
  bool
    force_explicit_name = TRUE, /* do not use implicit tag names */
    ignore_case = FALSE,	/* case is significant */
    multi_line = FALSE,		/* matches are done one line at a time */
    single_line = FALSE;	/* dot does not match newline */


  if (strlen (regexp_pattern) < 3)
    {
      error ("null regexp", (char *)NULL);
      return;
    }
  sep = regexp_pattern[0];
  name = scan_separators (regexp_pattern);
  if (name == NULL)
    {
      error ("%s: unterminated regexp", regexp_pattern);
      return;
    }
  if (name[1] == sep)
    {
      error ("null name for regexp \"%s\"", regexp_pattern);
      return;
    }
  modifiers = scan_separators (name);
  if (modifiers == NULL)	/* no terminating separator --> no name */
    {
      modifiers = name;
      name = empty;
    }
  else
    modifiers += 1;		/* skip separator */

  /* Parse regex modifiers. */
  for (; modifiers[0] != '\0'; modifiers++)
    switch (modifiers[0])
      {
      case 'N':
	if (modifiers == name)
	  error ("forcing explicit tag name but no name, ignoring", NULL);
	force_explicit_name = TRUE;
	break;
      case 'i':
	ignore_case = TRUE;
	break;
      case 's':
	single_line = TRUE;
	/* FALLTHRU */
      case 'm':
	multi_line = TRUE;
	need_filebuf = TRUE;
	break;
      default:
	{
	  char wrongmod [2];
	  wrongmod[0] = modifiers[0];
	  wrongmod[1] = '\0';
	  error ("invalid regexp modifier `%s', ignoring", wrongmod);
	}
	break;
      }

  patbuf = xnew (1, struct re_pattern_buffer);
  *patbuf = zeropattern;
  if (ignore_case)
    {
      static char lc_trans[CHARS];
      int i;
      for (i = 0; i < CHARS; i++)
	lc_trans[i] = lowcase (i);
      patbuf->translate = lc_trans;	/* translation table to fold case  */
    }

  if (multi_line)
    pat = concat ("^", regexp_pattern, ""); /* anchor to beginning of line */
  else
    pat = regexp_pattern;

  if (single_line)
    re_set_syntax (RE_SYNTAX_EMACS | RE_DOT_NEWLINE);
  else
    re_set_syntax (RE_SYNTAX_EMACS);

  err = re_compile_pattern (pat, strlen (pat), patbuf);
  if (multi_line)
    free (pat);
  if (err != NULL)
    {
      error ("%s while compiling pattern", err);
      return;
    }

  rp = p_head;
  p_head = xnew (1, regexp);
  p_head->pattern = savestr (regexp_pattern);
  p_head->p_next = rp;
  p_head->lang = lang;
  p_head->pat = patbuf;
  p_head->name = savestr (name);
  p_head->error_signaled = FALSE;
  p_head->force_explicit_name = force_explicit_name;
  p_head->ignore_case = ignore_case;
  p_head->multi_line = multi_line;
}

/*
 * Do the substitutions indicated by the regular expression and
 * arguments.
 */
static char *
substitute (char *in, char *out, struct re_registers *regs)
{
  char *result, *t;
  int size, dig, diglen;

  result = NULL;
  size = strlen (out);

  /* Pass 1: figure out how much to allocate by finding all \N strings. */
  if (out[size - 1] == '\\')
    fatal ("pattern error in \"%s\"", out);
  for (t = etags_strchr (out, '\\');
       t != NULL;
       t = etags_strchr (t + 2, '\\'))
    if (ISDIGIT (t[1]))
      {
	dig = t[1] - '0';
	diglen = regs->end[dig] - regs->start[dig];
	size += diglen - 2;
      }
    else
      size -= 1;

  /* Allocate space and do the substitutions. */
  assert (size >= 0);
  result = xnew (size + 1, char);

  for (t = result; *out != '\0'; out++)
    if (*out == '\\' && ISDIGIT (*++out))
      {
	dig = *out - '0';
	diglen = regs->end[dig] - regs->start[dig];
	strncpy (t, in + regs->start[dig], diglen);
	t += diglen;
      }
    else
      *t++ = *out;
  *t = '\0';

  assert (t <= result + size);
  assert (t - result == (int)strlen (result));

  return result;
}

/* Deallocate all regexps. */
static void
free_regexps (void)
{
  regexp *rp;
  while (p_head != NULL)
    {
      rp = p_head->p_next;
      free (p_head->pattern);
      free (p_head->name);
      free (p_head);
      p_head = rp;
    }
  return;
}

/*
 * Reads the whole file as a single string from `filebuf' and looks for
 * multi-line regular expressions, creating tags on matches.
 * readline already dealt with normal regexps.
 *
 * Idea by Ben Wing <ben@666.com> (2002).
 */
static void
regex_tag_multiline (void)
{
  char *buffer = filebuf.buffer;
  regexp *rp;
  char *name;

  for (rp = p_head; rp != NULL; rp = rp->p_next)
    {
      int match = 0;

      if (!rp->multi_line)
	continue;		/* skip normal regexps */

      /* Generic initializations before parsing file from memory. */
      lineno = 1;		/* reset global line number */
      charno = 0;		/* reset global char number */
      linecharno = 0;		/* reset global char number of line start */

      /* Only use generic regexps or those for the current language. */
      if (rp->lang != NULL && rp->lang != curfdp->lang)
	continue;

      while (match >= 0 && match < filebuf.len)
	{
	  match = re_search (rp->pat, buffer, filebuf.len, charno,
			     filebuf.len - match, &rp->regs);
	  switch (match)
	    {
	    case -2:
	      /* Some error. */
	      if (!rp->error_signaled)
		{
		  error ("regexp stack overflow while matching \"%s\"",
			 rp->pattern);
		  rp->error_signaled = TRUE;
		}
	      break;
	    case -1:
	      /* No match. */
	      break;
	    default:
	      if (match == rp->regs.end[0])
		{
		  if (!rp->error_signaled)
		    {
		      error ("regexp matches the empty string: \"%s\"",
			     rp->pattern);
		      rp->error_signaled = TRUE;
		    }
		  match = -3;	/* exit from while loop */
		  break;
		}

	      /* Match occurred.  Construct a tag. */
	      while (charno < rp->regs.end[0])
		if (buffer[charno++] == '\n')
		  lineno++, linecharno = charno;
	      name = rp->name;
	      if (name[0] == '\0')
		name = NULL;
	      else /* make a named tag */
		name = substitute (buffer, rp->name, &rp->regs);
	      if (rp->force_explicit_name)
		/* Force explicit tag name, if a name is there. */
		pfnote (name, TRUE, buffer + linecharno,
			charno - linecharno + 1, lineno, linecharno);
	      else
		make_tag (name, strlen (name), TRUE, buffer + linecharno,
			  charno - linecharno + 1, lineno, linecharno);
	      break;
	    }
	}
    }
}


static bool
nocase_tail (const char *cp)
{
  register int len = 0;

  while (*cp != '\0' && lowcase (*cp) == lowcase (dbp[len]))
    cp++, len++;
  if (*cp == '\0' && !intoken (dbp[len]))
    {
      dbp += len;
      return TRUE;
    }
  return FALSE;
}

static void
get_tag (register char *bp, char **namepp)
{
  register char *cp = bp;

  if (*bp != '\0')
    {
      /* Go till you get to white space or a syntactic break */
      for (cp = bp + 1; !notinname (*cp); cp++)
	continue;
      make_tag (bp, cp - bp, TRUE,
		lb.buffer, cp - lb.buffer + 1, lineno, linecharno);
    }

  if (namepp != NULL)
    *namepp = savenstr (bp, cp - bp);
}

/*
 * Read a line of text from `stream' into `lbp', excluding the
 * newline or CR-NL, if any.  Return the number of characters read from
 * `stream', which is the length of the line including the newline.
 *
 * On DOS or Windows we do not count the CR character, if any before the
 * NL, in the returned length; this mirrors the behavior of Emacs on those
 * platforms (for text files, it translates CR-NL to NL as it reads in the
 * file).
 *
 * If multi-line regular expressions are requested, each line read is
 * appended to `filebuf'.
 */
static long
readline_internal (linebuffer *lbp, register FILE *stream)
{
  char *buffer = lbp->buffer;
  register char *p = lbp->buffer;
  register char *pend;
  int chars_deleted;

  pend = p + lbp->size;		/* Separate to avoid 386/IX compiler bug.  */

  for (;;)
    {
      register int c = getc (stream);
      if (p == pend)
	{
	  /* We're at the end of linebuffer: expand it. */
	  lbp->size *= 2;
	  xrnew (buffer, lbp->size, char);
	  p += buffer - lbp->buffer;
	  pend = buffer + lbp->size;
	  lbp->buffer = buffer;
	}
      if (c == EOF)
	{
	  *p = '\0';
	  chars_deleted = 0;
	  break;
	}
      if (c == '\n')
	{
	  if (p > buffer && p[-1] == '\r')
	    {
	      p -= 1;
#ifdef DOS_NT
	     /* Assume CRLF->LF translation will be performed by Emacs
		when loading this file, so CRs won't appear in the buffer.
		It would be cleaner to compensate within Emacs;
		however, Emacs does not know how many CRs were deleted
		before any given point in the file.  */
	      chars_deleted = 1;
#else
	      chars_deleted = 2;
#endif
	    }
	  else
	    {
	      chars_deleted = 1;
	    }
	  *p = '\0';
	  break;
	}
      *p++ = c;
    }
  lbp->len = p - buffer;

  if (need_filebuf		/* we need filebuf for multi-line regexps */
      && chars_deleted > 0)	/* not at EOF */
    {
      while (filebuf.size <= filebuf.len + lbp->len + 1) /* +1 for \n */
	{
	  /* Expand filebuf. */
	  filebuf.size *= 2;
	  xrnew (filebuf.buffer, filebuf.size, char);
	}
      strncpy (filebuf.buffer + filebuf.len, lbp->buffer, lbp->len);
      filebuf.len += lbp->len;
      filebuf.buffer[filebuf.len++] = '\n';
      filebuf.buffer[filebuf.len] = '\0';
    }

  return lbp->len + chars_deleted;
}

/*
 * Like readline_internal, above, but in addition try to match the
 * input line against relevant regular expressions and manage #line
 * directives.
 */
static void
readline (linebuffer *lbp, FILE *stream)
{
  long result;

  linecharno = charno;		/* update global char number of line start */
  result = readline_internal (lbp, stream); /* read line */
  lineno += 1;			/* increment global line number */
  charno += result;		/* increment global char number */

  /* Honor #line directives. */
  if (!no_line_directive)
    {
      static bool discard_until_line_directive;

      /* Check whether this is a #line directive. */
      if (result > 12 && strneq (lbp->buffer, "#line ", 6))
	{
	  unsigned int lno;
	  int start = 0;

	  if (sscanf (lbp->buffer, "#line %u \"%n", &lno, &start) >= 1
	      && start > 0)	/* double quote character found */
	    {
	      char *endp = lbp->buffer + start;

	      while ((endp = etags_strchr (endp, '"')) != NULL
		     && endp[-1] == '\\')
		endp++;
	      if (endp != NULL)
		/* Ok, this is a real #line directive.  Let's deal with it. */
		{
		  char *taggedabsname;	/* absolute name of original file */
		  char *taggedfname;	/* name of original file as given */
		  char *name;		/* temp var */

		  discard_until_line_directive = FALSE; /* found it */
		  name = lbp->buffer + start;
		  *endp = '\0';
		  canonicalize_filename (name);
		  taggedabsname = absolute_filename (name, tagfiledir);
		  if (filename_is_absolute (name)
		      || filename_is_absolute (curfdp->infname))
		    taggedfname = savestr (taggedabsname);
		  else
		    taggedfname = relative_filename (taggedabsname,tagfiledir);

		  if (streq (curfdp->taggedfname, taggedfname))
		    /* The #line directive is only a line number change.  We
		       deal with this afterwards. */
		    free (taggedfname);
		  else
		    /* The tags following this #line directive should be
		       attributed to taggedfname.  In order to do this, set
		       curfdp accordingly. */
		    {
		      fdesc *fdp; /* file description pointer */

		      /* Go look for a file description already set up for the
			 file indicated in the #line directive.  If there is
			 one, use it from now until the next #line
			 directive. */
		      for (fdp = fdhead; fdp != NULL; fdp = fdp->next)
			if (streq (fdp->infname, curfdp->infname)
			    && streq (fdp->taggedfname, taggedfname))
			  /* If we remove the second test above (after the &&)
			     then all entries pertaining to the same file are
			     coalesced in the tags file.  If we use it, then
			     entries pertaining to the same file but generated
			     from different files (via #line directives) will
			     go into separate sections in the tags file.  These
			     alternatives look equivalent.  The first one
			     destroys some apparently useless information. */
			  {
			    curfdp = fdp;
			    free (taggedfname);
			    break;
			  }
		      /* Else, if we already tagged the real file, skip all
			 input lines until the next #line directive. */
		      if (fdp == NULL) /* not found */
			for (fdp = fdhead; fdp != NULL; fdp = fdp->next)
			  if (streq (fdp->infabsname, taggedabsname))
			    {
			      discard_until_line_directive = TRUE;
			      free (taggedfname);
			      break;
			    }
		      /* Else create a new file description and use that from
			 now on, until the next #line directive. */
		      if (fdp == NULL) /* not found */
			{
			  fdp = fdhead;
			  fdhead = xnew (1, fdesc);
			  *fdhead = *curfdp; /* copy curr. file description */
			  fdhead->next = fdp;
			  fdhead->infname = savestr (curfdp->infname);
			  fdhead->infabsname = savestr (curfdp->infabsname);
			  fdhead->infabsdir = savestr (curfdp->infabsdir);
			  fdhead->taggedfname = taggedfname;
			  fdhead->usecharno = FALSE;
			  fdhead->prop = NULL;
			  fdhead->written = FALSE;
			  curfdp = fdhead;
			}
		    }
		  free (taggedabsname);
		  lineno = lno - 1;
		  readline (lbp, stream);
		  return;
		} /* if a real #line directive */
	    } /* if #line is followed by a number */
	} /* if line begins with "#line " */

      /* If we are here, no #line directive was found. */
      if (discard_until_line_directive)
	{
	  if (result > 0)
	    {
	      /* Do a tail recursion on ourselves, thus discarding the contents
		 of the line buffer. */
	      readline (lbp, stream);
	      return;
	    }
	  /* End of file. */
	  discard_until_line_directive = FALSE;
	  return;
	}
    } /* if #line directives should be considered */

  {
    int match;
    regexp *rp;
    char *name;

    /* Match against relevant regexps. */
    if (lbp->len > 0)
      for (rp = p_head; rp != NULL; rp = rp->p_next)
	{
	  /* Only use generic regexps or those for the current language.
	     Also do not use multiline regexps, which is the job of
	     regex_tag_multiline. */
	  if ((rp->lang != NULL && rp->lang != fdhead->lang)
	      || rp->multi_line)
	    continue;

	  match = re_match (rp->pat, lbp->buffer, lbp->len, 0, &rp->regs);
	  switch (match)
	    {
	    case -2:
	      /* Some error. */
	      if (!rp->error_signaled)
		{
		  error ("regexp stack overflow while matching \"%s\"",
			 rp->pattern);
		  rp->error_signaled = TRUE;
		}
	      break;
	    case -1:
	      /* No match. */
	      break;
	    case 0:
	      /* Empty string matched. */
	      if (!rp->error_signaled)
		{
		  error ("regexp matches the empty string: \"%s\"", rp->pattern);
		  rp->error_signaled = TRUE;
		}
	      break;
	    default:
	      /* Match occurred.  Construct a tag. */
	      name = rp->name;
	      if (name[0] == '\0')
		name = NULL;
	      else /* make a named tag */
		name = substitute (lbp->buffer, rp->name, &rp->regs);
	      if (rp->force_explicit_name)
		/* Force explicit tag name, if a name is there. */
		pfnote (name, TRUE, lbp->buffer, match, lineno, linecharno);
	      else
		make_tag (name, strlen (name), TRUE,
			  lbp->buffer, match, lineno, linecharno);
	      break;
	    }
	}
  }
}


/*
 * Return a pointer to a space of size strlen(cp)+1 allocated
 * with xnew where the string CP has been copied.
 */
static char *
savestr (const char *cp)
{
  return savenstr (cp, strlen (cp));
}

/*
 * Return a pointer to a space of size LEN+1 allocated with xnew where
 * the string CP has been copied for at most the first LEN characters.
 */
static char *
savenstr (const char *cp, int len)
{
  register char *dp;

  dp = xnew (len + 1, char);
  strncpy (dp, cp, len);
  dp[len] = '\0';
  return dp;
}

/*
 * Return the ptr in sp at which the character c last
 * appears; NULL if not found
 *
 * Identical to POSIX strrchr, included for portability.
 */
static char *
etags_strrchr (register const char *sp, register int c)
{
  register const char *r;

  r = NULL;
  do
    {
      if (*sp == c)
	r = sp;
  } while (*sp++);
  return (char *)r;
}

/*
 * Return the ptr in sp at which the character c first
 * appears; NULL if not found
 *
 * Identical to POSIX strchr, included for portability.
 */
static char *
etags_strchr (register const char *sp, register int c)
{
  do
    {
      if (*sp == c)
	return (char *)sp;
    } while (*sp++);
  return NULL;
}

/*
 * Compare two strings, ignoring case for alphabetic characters.
 *
 * Same as BSD's strcasecmp, included for portability.
 */
static int
etags_strcasecmp (register const char *s1, register const char *s2)
{
  while (*s1 != '\0'
	 && (ISALPHA (*s1) && ISALPHA (*s2)
	     ? lowcase (*s1) == lowcase (*s2)
	     : *s1 == *s2))
    s1++, s2++;

  return (ISALPHA (*s1) && ISALPHA (*s2)
	  ? lowcase (*s1) - lowcase (*s2)
	  : *s1 - *s2);
}

/*
 * Compare two strings, ignoring case for alphabetic characters.
 * Stop after a given number of characters
 *
 * Same as BSD's strncasecmp, included for portability.
 */
static int
etags_strncasecmp (register const char *s1, register const char *s2, register int n)
{
  while (*s1 != '\0' && n-- > 0
	 && (ISALPHA (*s1) && ISALPHA (*s2)
	     ? lowcase (*s1) == lowcase (*s2)
	     : *s1 == *s2))
    s1++, s2++;

  if (n < 0)
    return 0;
  else
    return (ISALPHA (*s1) && ISALPHA (*s2)
	    ? lowcase (*s1) - lowcase (*s2)
	    : *s1 - *s2);
}

/* Skip spaces (end of string is not space), return new pointer. */
static char *
skip_spaces (char *cp)
{
  while (iswhite (*cp))
    cp++;
  return cp;
}

/* Skip non spaces, except end of string, return new pointer. */
static char *
skip_non_spaces (char *cp)
{
  while (*cp != '\0' && !iswhite (*cp))
    cp++;
  return cp;
}

/* Print error message and exit.  */
void
fatal (const char *s1, const char *s2)
{
  error (s1, s2);
  exit (EXIT_FAILURE);
}

static void
pfatal (const char *s1)
{
  perror (s1);
  exit (EXIT_FAILURE);
}

static void
suggest_asking_for_help (void)
{
  fprintf (stderr, "\tTry `%s %s' for a complete list of options.\n",
	   progname, NO_LONG_OPTIONS ? "-h" : "--help");
  exit (EXIT_FAILURE);
}

/* Print error message.  `s1' is printf control string, `s2' is arg for it. */
static void
error (const char *s1, const char *s2)
{
  fprintf (stderr, "%s: ", progname);
  fprintf (stderr, s1, s2);
  fprintf (stderr, "\n");
}

/* Return a newly-allocated string whose contents
   concatenate those of s1, s2, s3.  */
static char *
concat (const char *s1, const char *s2, const char *s3)
{
  int len1 = strlen (s1), len2 = strlen (s2), len3 = strlen (s3);
  char *result = xnew (len1 + len2 + len3 + 1, char);

  strcpy (result, s1);
  strcpy (result + len1, s2);
  strcpy (result + len1 + len2, s3);
  result[len1 + len2 + len3] = '\0';

  return result;
}


/* Does the same work as the system V getcwd, but does not need to
   guess the buffer size in advance. */
static char *
etags_getcwd (void)
{
#ifdef HAVE_GETCWD
  int bufsize = 200;
  char *path = xnew (bufsize, char);

  while (getcwd (path, bufsize) == NULL)
    {
      if (errno != ERANGE)
	pfatal ("getcwd");
      bufsize *= 2;
      free (path);
      path = xnew (bufsize, char);
    }

  canonicalize_filename (path);
  return path;

#else /* not HAVE_GETCWD */
#if MSDOS

  char *p, path[MAXPATHLEN + 1]; /* Fixed size is safe on MSDOS.  */

  getwd (path);

  for (p = path; *p != '\0'; p++)
    if (*p == '\\')
      *p = '/';
    else
      *p = lowcase (*p);

  return strdup (path);
#else /* not MSDOS */
  linebuffer path;
  FILE *pipe;

  linebuffer_init (&path);
  pipe = (FILE *) popen ("pwd 2>/dev/null", "r");
  if (pipe == NULL || readline_internal (&path, pipe) == 0)
    pfatal ("pwd");
  pclose (pipe);

  return path.buffer;
#endif /* not MSDOS */
#endif /* not HAVE_GETCWD */
}

/* Return a newly allocated string containing the file name of FILE
   relative to the absolute directory DIR (which should end with a slash). */
static char *
relative_filename (char *file, char *dir)
{
  char *fp, *dp, *afn, *res;
  int i;

  /* Find the common root of file and dir (with a trailing slash). */
  afn = absolute_filename (file, cwd);
  fp = afn;
  dp = dir;
  while (*fp++ == *dp++)
    continue;
  fp--, dp--;			/* back to the first differing char */
#ifdef DOS_NT
  if (fp == afn && afn[0] != '/') /* cannot build a relative name */
    return afn;
#endif
  do				/* look at the equal chars until '/' */
    fp--, dp--;
  while (*fp != '/');

  /* Build a sequence of "../" strings for the resulting relative file name. */
  i = 0;
  while ((dp = etags_strchr (dp + 1, '/')) != NULL)
    i += 1;
  res = xnew (3*i + strlen (fp + 1) + 1, char);
  res[0] = '\0';
  while (i-- > 0)
    strcat (res, "../");

  /* Add the file name relative to the common root of file and dir. */
  strcat (res, fp + 1);
  free (afn);

  return res;
}

/* Return a newly allocated string containing the absolute file name
   of FILE given DIR (which should end with a slash). */
static char *
absolute_filename (char *file, char *dir)
{
  char *slashp, *cp, *res;

  if (filename_is_absolute (file))
    res = savestr (file);
#ifdef DOS_NT
  /* We don't support non-absolute file names with a drive
     letter, like `d:NAME' (it's too much hassle).  */
  else if (file[1] == ':')
    fatal ("%s: relative file names with drive letters not supported", file);
#endif
  else
    res = concat (dir, file, "");

  /* Delete the "/dirname/.." and "/." substrings. */
  slashp = etags_strchr (res, '/');
  while (slashp != NULL && slashp[0] != '\0')
    {
      if (slashp[1] == '.')
	{
	  if (slashp[2] == '.'
	      && (slashp[3] == '/' || slashp[3] == '\0'))
	    {
	      cp = slashp;
	      do
		cp--;
	      while (cp >= res && !filename_is_absolute (cp));
	      if (cp < res)
		cp = slashp;	/* the absolute name begins with "/.." */
#ifdef DOS_NT
	      /* Under MSDOS and NT we get `d:/NAME' as absolute
		 file name, so the luser could say `d:/../NAME'.
		 We silently treat this as `d:/NAME'.  */
	      else if (cp[0] != '/')
		cp = slashp;
#endif
              memmove (cp, slashp + 3, strlen (slashp + 2));
	      slashp = cp;
	      continue;
	    }
	  else if (slashp[2] == '/' || slashp[2] == '\0')
	    {
	      memmove (slashp, slashp + 2, strlen (slashp + 1));
	      continue;
	    }
	}

      slashp = etags_strchr (slashp + 1, '/');
    }

  if (res[0] == '\0')		/* just a safety net: should never happen */
    {
      free (res);
      return savestr ("/");
    }
  else
    return res;
}

/* Return a newly allocated string containing the absolute
   file name of dir where FILE resides given DIR (which should
   end with a slash). */
static char *
absolute_dirname (char *file, char *dir)
{
  char *slashp, *res;
  char save;

  slashp = etags_strrchr (file, '/');
  if (slashp == NULL)
    return savestr (dir);
  save = slashp[1];
  slashp[1] = '\0';
  res = absolute_filename (file, dir);
  slashp[1] = save;

  return res;
}

/* Whether the argument string is an absolute file name.  The argument
   string must have been canonicalized with canonicalize_filename. */
static bool
filename_is_absolute (char *fn)
{
  return (fn[0] == '/'
#ifdef DOS_NT
	  || (ISALPHA (fn[0]) && fn[1] == ':' && fn[2] == '/')
#endif
	  );
}

/* Downcase DOS drive letter and collapse separators into single slashes.
   Works in place. */
static void
canonicalize_filename (register char *fn)
{
  register char* cp;
  char sep = '/';

#ifdef DOS_NT
  /* Canonicalize drive letter case.  */
# define ISUPPER(c)	isupper (CHAR (c))
  if (fn[0] != '\0' && fn[1] == ':' && ISUPPER (fn[0]))
    fn[0] = lowcase (fn[0]);

  sep = '\\';
#endif

  /* Collapse multiple separators into a single slash. */
  for (cp = fn; *cp != '\0'; cp++, fn++)
    if (*cp == sep)
      {
	*fn = '/';
	while (cp[1] == sep)
	  cp++;
      }
    else
      *fn = *cp;
  *fn = '\0';
}


/* Initialize a linebuffer for use. */
static void
linebuffer_init (linebuffer *lbp)
{
  lbp->size = (DEBUG) ? 3 : 200;
  lbp->buffer = xnew (lbp->size, char);
  lbp->buffer[0] = '\0';
  lbp->len = 0;
}

/* Set the minimum size of a string contained in a linebuffer. */
static void
linebuffer_setlen (linebuffer *lbp, int toksize)
{
  while (lbp->size <= toksize)
    {
      lbp->size *= 2;
      xrnew (lbp->buffer, lbp->size, char);
    }
  lbp->len = toksize;
}

/* Like malloc but get fatal error if memory is exhausted. */
static PTR
xmalloc (size_t size)
{
  PTR result = (PTR) malloc (size);
  if (result == NULL)
    fatal ("virtual memory exhausted", (char *)NULL);
  return result;
}

static PTR
xrealloc (char *ptr, size_t size)
{
  PTR result = (PTR) realloc (ptr, size);
  if (result == NULL)
    fatal ("virtual memory exhausted", (char *)NULL);
  return result;
}

/*
 * Local Variables:
 * indent-tabs-mode: t
 * tab-width: 8
 * fill-column: 79
 * c-font-lock-extra-types: ("FILE" "bool" "language" "linebuffer" "fdesc" "node" "regexp")
 * c-file-style: "gnu"
 * End:
 */

/* etags.c ends here */
