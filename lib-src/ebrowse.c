/* ebrowse.c --- parsing files for the ebrowse C++ browser

Copyright (C) 1992-2012  Free Software Foundation, Inc.

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
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>
#include <getopt.h>

/* The SunOS compiler doesn't have SEEK_END.  */
#ifndef SEEK_END
#define SEEK_END 2
#endif

/* Conditionalize function prototypes.  */

/* Value is non-zero if strings X and Y compare equal.  */

#define streq(X, Y) (*(X) == *(Y) && strcmp ((X) + 1, (Y) + 1) == 0)

#include <min-max.h>

/* Files are read in chunks of this number of bytes.  */

#define READ_CHUNK_SIZE (100 * 1024)

/* The character used as a separator in path lists (like $PATH).  */

#if defined (__MSDOS__)
#define PATH_LIST_SEPARATOR ';'
#define FILENAME_EQ(X,Y)    (strcasecmp (X,Y) == 0)
#else
#if defined (WINDOWSNT)
#define PATH_LIST_SEPARATOR ';'
#define FILENAME_EQ(X,Y)    (stricmp (X,Y) == 0)
#else
#define PATH_LIST_SEPARATOR ':'
#define FILENAME_EQ(X,Y)    (streq (X,Y))
#endif
#endif
/* The default output file name.  */

#define DEFAULT_OUTFILE "BROWSE"

/* A version string written to the output file.  Change this whenever
   the structure of the output file changes.  */

#define EBROWSE_FILE_VERSION "ebrowse 5.0"

/* The output file consists of a tree of Lisp objects, with major
   nodes built out of Lisp structures.  These are the heads of the
   Lisp structs with symbols identifying their type.  */

#define TREE_HEADER_STRUCT	"[ebrowse-hs "
#define TREE_STRUCT		"[ebrowse-ts "
#define MEMBER_STRUCT		"[ebrowse-ms "
#define CLASS_STRUCT		"[ebrowse-cs "

/* The name of the symbol table entry for global functions, variables,
   defines etc.  This name also appears in the browser display.  */

#define GLOBALS_NAME "*Globals*"

/* Token definitions.  */

enum token
{
  YYEOF = 0,			/* end of file */
  CSTRING = 256,		/* string constant */
  CCHAR,			/* character constant */
  CINT,				/* integral constant */
  CFLOAT,			/* real constant */

  ELLIPSIS,			/* ... */
  LSHIFTASGN,			/* <<= */
  RSHIFTASGN,			/* >>= */
  ARROWSTAR,			/* ->* */
  IDENT,			/* identifier */
  DIVASGN,			/* /= */
  INC,				/* ++ */
  ADDASGN,			/* += */
  DEC,				/* -- */
  ARROW,			/* -> */
  SUBASGN,			/* -= */
  MULASGN,			/* *= */
  MODASGN,			/* %= */
  LOR,				/* || */
  ORASGN,			/* |= */
  LAND,				/* && */
  ANDASGN,			/* &= */
  XORASGN,			/* ^= */
  POINTSTAR,			/* .* */
  DCOLON,			/* :: */
  EQ,				/* == */
  NE,				/* != */
  LE,				/* <= */
  LSHIFT,			/* << */
  GE,				/* >= */
  RSHIFT,			/* >> */

/* Keywords.  The undef's are there because these
   three symbols are very likely to be defined somewhere.  */
#undef BOOL
#undef TRUE
#undef FALSE

  ASM,				/* asm */
  AUTO,				/* auto */
  BREAK,			/* break */
  CASE,				/* case  */
  CATCH,			/* catch */
  CHAR,				/* char */
  CLASS,			/* class */
  CONST,			/* const */
  CONTINUE,			/* continue */
  DEFAULT,			/* default */
  DELETE,			/* delete */
  DO,				/* do */
  DOUBLE,			/* double */
  ELSE,				/* else */
  ENUM,				/* enum */
  EXTERN,			/* extern */
  FLOAT,			/* float */
  FOR,				/* for */
  FRIEND,			/* friend */
  GOTO,				/* goto */
  IF,				/* if */
  T_INLINE,			/* inline */
  INT,				/* int */
  LONG,				/* long */
  NEW,				/* new */
  OPERATOR,			/* operator */
  PRIVATE,			/* private */
  PROTECTED,			/* protected */
  PUBLIC,			/* public */
  REGISTER,			/* register */
  RETURN,			/* return */
  SHORT,			/* short */
  SIGNED,			/* signed */
  SIZEOF,			/* sizeof */
  STATIC,			/* static */
  STRUCT,			/* struct */
  SWITCH,			/* switch */
  TEMPLATE,			/* template */
  THIS,				/* this */
  THROW,			/* throw */
  TRY,				/* try */
  TYPEDEF,			/* typedef */
  UNION,			/* union */
  UNSIGNED,			/* unsigned */
  VIRTUAL,			/* virtual */
  VOID,				/* void */
  VOLATILE,			/* volatile */
  WHILE,			/* while */
  MUTABLE,			/* mutable */
  BOOL,				/* bool */
  TRUE,				/* true */
  FALSE,			/* false */
  SIGNATURE,			/* signature (GNU extension) */
  NAMESPACE,			/* namespace */
  EXPLICIT,			/* explicit */
  TYPENAME,			/* typename */
  CONST_CAST,			/* const_cast */
  DYNAMIC_CAST,			/* dynamic_cast */
  REINTERPRET_CAST,		/* reinterpret_cast */
  STATIC_CAST,			/* static_cast */
  TYPEID,			/* typeid */
  USING,			/* using */
  WCHAR				/* wchar_t */
};

/* Storage classes, in a wider sense.  */

enum sc
{
  SC_UNKNOWN,
  SC_MEMBER,			/* Is an instance member.  */
  SC_STATIC,			/* Is static member.  */
  SC_FRIEND,			/* Is friend function.  */
  SC_TYPE			/* Is a type definition.  */
};

/* Member visibility.  */

enum visibility
{
  V_PUBLIC,
  V_PROTECTED,
  V_PRIVATE
};

/* Member flags.  */

#define F_VIRTUAL	1	/* Is virtual function.  */
#define F_INLINE	2	/* Is inline function.  */
#define F_CONST		4	/* Is const.  */
#define F_PURE		8	/* Is pure virtual function.  */
#define F_MUTABLE	16	/* Is mutable.  */
#define F_TEMPLATE	32	/* Is a template.  */
#define F_EXPLICIT	64	/* Is explicit constructor.  */
#define F_THROW		128	/* Has a throw specification.  */
#define F_EXTERNC	256	/* Is declared extern "C".  */
#define F_DEFINE	512	/* Is a #define.  */

/* Two macros to set and test a bit in an int.  */

#define SET_FLAG(F, FLAG)	((F) |= (FLAG))
#define HAS_FLAG(F, FLAG)	(((F) & (FLAG)) != 0)

/* Structure describing a class member.  */

struct member
{
  struct member *next;		/* Next in list of members.  */
  struct member *anext;		/* Collision chain in member_table.  */
  struct member **list;		/* Pointer to list in class.  */
  unsigned param_hash;		/* Hash value for parameter types.  */
  int vis;			/* Visibility (public, ...).  */
  int flags;			/* See F_* above.  */
  char *regexp;			/* Matching regular expression.  */
  const char *filename;		/* Don't free this shared string.  */
  int pos;			/* Buffer position of occurrence.  */
  char *def_regexp;		/* Regular expression matching definition.  */
  const char *def_filename;	/* File name of definition.  */
  int def_pos;			/* Buffer position of definition.  */
  char name[1];			/* Member name.  */
};

/* Structures of this type are used to connect class structures with
   their super and subclasses.  */

struct link
{
  struct sym *sym;		/* The super or subclass.  */
  struct link *next;		/* Next in list or NULL.  */
};

/* Structure used to record namespace aliases.  */

struct alias
{
  struct alias *next;		/* Next in list.  */
  struct sym *namesp;		/* Namespace in which defined.  */
  struct link *aliasee;		/* List of aliased namespaces (A::B::C...).  */
  char name[1];			/* Alias name.  */
};

/* The structure used to describe a class in the symbol table,
   or a namespace in all_namespaces.  */

struct sym
{
  int flags;			/* Is class a template class?.  */
  unsigned char visited;	/* Used to find circles.  */
  struct sym *next;             /* Hash collision list.  */
  struct link *subs;		/* List of subclasses.  */
  struct link *supers;		/* List of superclasses.  */
  struct member *vars;		/* List of instance variables.  */
  struct member *fns;		/* List of instance functions.  */
  struct member *static_vars;	/* List of static variables.  */
  struct member *static_fns;	/* List of static functions.  */
  struct member *friends;	/* List of friend functions.  */
  struct member *types;		/* List of local types.  */
  char *regexp;			/* Matching regular expression.  */
  int pos;			/* Buffer position.  */
  const char *filename;		/* File in which it can be found.  */
  const char *sfilename; 	/* File in which members can be found.  */
  struct sym *namesp;		/* Namespace in which defined. .  */
  char name[1];                 /* Name of the class.  */
};

/* Experimental: Print info for `--position-info'.  We print
   '(CLASS-NAME SCOPE MEMBER-NAME).  */

#define P_DEFN	1
#define P_DECL  2

int info_where;
struct sym *info_cls = NULL;
struct member *info_member = NULL;

/* Experimental.  For option `--position-info', the buffer position we
   are interested in.  When this position is reached, print out
   information about what we know about that point.  */

int info_position = -1;

/* Command line options structure for getopt_long.  */

struct option options[] =
{
  {"append", 			no_argument, 	   NULL, 'a'},
  {"files", 			required_argument, NULL, 'f'},
  {"help", 			no_argument, 	   NULL, -2},
  {"min-regexp-length", 	required_argument, NULL, 'm'},
  {"max-regexp-length", 	required_argument, NULL, 'M'},
  {"no-nested-classes", 	no_argument, 	   NULL, 'n'},
  {"no-regexps", 		no_argument, 	   NULL, 'x'},
  {"no-structs-or-unions", 	no_argument, 	   NULL, 's'},
  {"output-file", 		required_argument, NULL, 'o'},
  {"position-info", 		required_argument, NULL, 'p'},
  {"search-path", 		required_argument, NULL, 'I'},
  {"verbose", 			no_argument, 	   NULL, 'v'},
  {"version", 			no_argument, 	   NULL, -3},
  {"very-verbose", 		no_argument, 	   NULL, 'V'},
  {NULL, 			0, 		   NULL, 0}
};

/* Semantic values of tokens.  Set by yylex..  */

unsigned yyival;		/* Set for token CINT.  */
char *yytext;			/* Set for token IDENT.  */
char *yytext_end;

/* Output file.  */

FILE *yyout;

/* Current line number.  */

int yyline;

/* The name of the current input file.  */

const char *filename;

/* Three character class vectors, and macros to test membership
   of characters.  */

char is_ident[255];
char is_digit[255];
char is_white[255];

#define IDENTP(C)	is_ident[(unsigned char) (C)]
#define DIGITP(C)	is_digit[(unsigned char) (C)]
#define WHITEP(C)	is_white[(unsigned char) (C)]

/* Command line flags.  */

int f_append;
int f_verbose;
int f_very_verbose;
int f_structs = 1;
int f_regexps = 1;
int f_nested_classes = 1;

/* Maximum and minimum lengths of regular expressions matching a
   member, class etc., for writing them to the output file.  These are
   overridable from the command line.  */

int min_regexp = 5;
int max_regexp = 50;

/* Input buffer.  */

char *inbuffer;
char *in;
size_t inbuffer_size;

/* Return the current buffer position in the input file.  */

#define BUFFER_POS() (in - inbuffer)

/* If current lookahead is CSTRING, the following points to the
   first character in the string constant.  Used for recognizing
   extern "C".  */

char *string_start;

/* The size of the hash tables for classes.and members.  Should be
   prime.  */

#define TABLE_SIZE 1001

/* The hash table for class symbols.  */

struct sym *class_table[TABLE_SIZE];

/* Hash table containing all member structures.  This is generally
   faster for member lookup than traversing the member lists of a
   `struct sym'.  */

struct member *member_table[TABLE_SIZE];

/* Hash table for namespace aliases */

struct alias *namespace_alias_table[TABLE_SIZE];

/* The special class symbol used to hold global functions,
   variables etc.  */

struct sym *global_symbols;

/* The current namespace.  */

struct sym *current_namespace;

/* The list of all known namespaces.  */

struct sym *all_namespaces;

/* Stack of namespaces we're currently nested in, during the parse.  */

struct sym **namespace_stack;
int namespace_stack_size;
int namespace_sp;

/* The current lookahead token.  */

int tk = -1;

/* Structure describing a keyword.  */

struct kw
{
  const char *name;		/* Spelling.  */
  int tk;			/* Token value.  */
  struct kw *next;		/* Next in collision chain.  */
};

/* Keywords are lookup up in a hash table of their own.  */

#define KEYWORD_TABLE_SIZE 1001
struct kw *keyword_table[KEYWORD_TABLE_SIZE];

/* Search path.  */

struct search_path
{
  char *path;
  struct search_path *next;
};

struct search_path *search_path;
struct search_path *search_path_tail;

/* Function prototypes.  */

static char *matching_regexp (void);
static struct sym *add_sym (const char *, struct sym *);
static void add_global_defn (char *, char *, int, unsigned, int, int, int);
static void add_global_decl (char *, char *, int, unsigned, int, int, int);
static struct member *add_member (struct sym *, char *, int, int, unsigned);
static void class_definition (struct sym *, int, int, int);
static char *operator_name (int *);
static void parse_qualified_param_ident_or_type (char **);
static void usage (int) NO_RETURN;
static void version (void) NO_RETURN;



/***********************************************************************
			      Utilities
 ***********************************************************************/

/* Print an error in a printf-like style with the current input file
   name and line number.  */

static void
yyerror (const char *format, const char *s)
{
  fprintf (stderr, "%s:%d: ", filename, yyline);
  fprintf (stderr, format, s);
  putc ('\n', stderr);
}


/* Like malloc but print an error and exit if not enough memory is
   available.  */

static void *
xmalloc (size_t nbytes)
{
  void *p = malloc (nbytes);
  if (p == NULL)
    {
      yyerror ("out of memory", NULL);
      exit (EXIT_FAILURE);
    }
  return p;
}


/* Like realloc but print an error and exit if out of memory.  */

static void *
xrealloc (void *p, size_t sz)
{
  p = realloc (p, sz);
  if (p == NULL)
    {
      yyerror ("out of memory", NULL);
      exit (EXIT_FAILURE);
    }
  return p;
}


/* Like strdup, but print an error and exit if not enough memory is
   available..  If S is null, return null.  */

static char *
xstrdup (char *s)
{
  if (s)
    s = strcpy (xmalloc (strlen (s) + 1), s);
  return s;
}



/***********************************************************************
			       Symbols
 ***********************************************************************/

/* Initialize the symbol table.  This currently only sets up the
   special symbol for globals (`*Globals*').  */

static void
init_sym (void)
{
  global_symbols = add_sym (GLOBALS_NAME, NULL);
}


/* Add a symbol for class NAME to the symbol table.  NESTED_IN_CLASS
   is the class in which class NAME was found.  If it is null,
   this means the scope of NAME is the current namespace.

   If a symbol for NAME already exists, return that.  Otherwise
   create a new symbol and set it to default values.  */

static struct sym *
add_sym (const char *name, struct sym *nested_in_class)
{
  struct sym *sym;
  unsigned h;
  const char *s;
  struct sym *scope = nested_in_class ? nested_in_class : current_namespace;

  for (s = name, h = 0; *s; ++s)
    h = (h << 1) ^ *s;
  h %= TABLE_SIZE;

  for (sym = class_table[h]; sym; sym = sym->next)
    if (streq (name, sym->name)
	&& ((!sym->namesp && !scope)
	    || (sym->namesp && scope
		&& streq (sym->namesp->name, scope->name))))
      break;

  if (sym == NULL)
    {
      if (f_very_verbose)
	{
	  putchar ('\t');
	  puts (name);
	}

      sym = (struct sym *) xmalloc (sizeof *sym + strlen (name));
      memset (sym, 0, sizeof *sym);
      strcpy (sym->name, name);
      sym->namesp = scope;
      sym->next = class_table[h];
      class_table[h] = sym;
    }

  return sym;
}


/* Add links between superclass SUPER and subclass SUB.  */

static void
add_link (struct sym *super, struct sym *sub)
{
  struct link *lnk, *lnk2, *p, *prev;

  /* See if a link already exists.  */
  for (p = super->subs, prev = NULL;
       p && strcmp (sub->name, p->sym->name) > 0;
       prev = p, p = p->next)
    ;

  /* Avoid duplicates.  */
  if (p == NULL || p->sym != sub)
    {
      lnk = (struct link *) xmalloc (sizeof *lnk);
      lnk2 = (struct link *) xmalloc (sizeof *lnk2);

      lnk->sym = sub;
      lnk->next = p;

      if (prev)
	prev->next = lnk;
      else
	super->subs = lnk;

      lnk2->sym = super;
      lnk2->next = sub->supers;
      sub->supers = lnk2;
    }
}


/* Find in class CLS member NAME.

   VAR non-zero means look for a member variable; otherwise a function
   is searched.  SC specifies what kind of member is searched---a
   static, or per-instance member etc.  HASH is a hash code for the
   parameter types of functions.  Value is a pointer to the member
   found or null if not found.  */

static struct member *
find_member (struct sym *cls, char *name, int var, int sc, unsigned int hash)
{
  struct member **list;
  struct member *p;
  unsigned name_hash = 0;
  char *s;
  int i;

  switch (sc)
    {
    case SC_FRIEND:
      list = &cls->friends;
      break;

    case SC_TYPE:
      list = &cls->types;
      break;

    case SC_STATIC:
      list = var ? &cls->static_vars : &cls->static_fns;
      break;

    default:
      list = var ? &cls->vars : &cls->fns;
      break;
    }

  for (s = name; *s; ++s)
    name_hash = (name_hash << 1) ^ *s;
  i = name_hash % TABLE_SIZE;

  for (p = member_table[i]; p; p = p->anext)
    if (p->list == list && p->param_hash == hash && streq (name, p->name))
      break;

  return p;
}


/* Add to class CLS information for the declaration of member NAME.
   REGEXP is a regexp matching the declaration, if non-null.  POS is
   the position in the source where the declaration is found.  HASH is
   a hash code for the parameter list of the member, if it's a
   function.  VAR non-zero means member is a variable or type.  SC
   specifies the type of member (instance member, static, ...).  VIS
   is the member's visibility (public, protected, private).  FLAGS is
   a bit set giving additional information about the member (see the
   F_* defines).  */

static void
add_member_decl (struct sym *cls, char *name, char *regexp, int pos, unsigned int hash, int var, int sc, int vis, int flags)
{
  struct member *m;

  m = find_member (cls, name, var, sc, hash);
  if (m == NULL)
    m = add_member (cls, name, var, sc, hash);

  /* Have we seen a new filename?  If so record that.  */
  if (!cls->filename || !FILENAME_EQ (cls->filename, filename))
    m->filename = filename;

  m->regexp = regexp;
  m->pos = pos;
  m->flags = flags;

  switch (vis)
    {
    case PRIVATE:
      m->vis = V_PRIVATE;
      break;

    case PROTECTED:
      m->vis = V_PROTECTED;
      break;

    case PUBLIC:
      m->vis = V_PUBLIC;
      break;
    }

  info_where = P_DECL;
  info_cls = cls;
  info_member = m;
}


/* Add to class CLS information for the definition of member NAME.
   REGEXP is a regexp matching the declaration, if non-null.  POS is
   the position in the source where the declaration is found.  HASH is
   a hash code for the parameter list of the member, if it's a
   function.  VAR non-zero means member is a variable or type.  SC
   specifies the type of member (instance member, static, ...).  VIS
   is the member's visibility (public, protected, private).  FLAGS is
   a bit set giving additional information about the member (see the
   F_* defines).  */

static void
add_member_defn (struct sym *cls, char *name, char *regexp, int pos, unsigned int hash, int var, int sc, int flags)
{
  struct member *m;

  if (sc == SC_UNKNOWN)
    {
      m = find_member (cls, name, var, SC_MEMBER, hash);
      if (m == NULL)
	{
	  m = find_member (cls, name, var, SC_STATIC, hash);
	  if (m == NULL)
	    m = add_member (cls, name, var, sc, hash);
	}
    }
  else
    {
      m = find_member (cls, name, var, sc, hash);
      if (m == NULL)
	m = add_member (cls, name, var, sc, hash);
    }

  if (!cls->sfilename)
    cls->sfilename = filename;

  if (!FILENAME_EQ (cls->sfilename, filename))
    m->def_filename = filename;

  m->def_regexp = regexp;
  m->def_pos = pos;
  m->flags |= flags;

  info_where = P_DEFN;
  info_cls = cls;
  info_member = m;
}


/* Add a symbol for a define named NAME to the symbol table.
   REGEXP is a regular expression matching the define in the source,
   if it is non-null.  POS is the position in the file.  */

static void
add_define (char *name, char *regexp, int pos)
{
  add_global_defn (name, regexp, pos, 0, 1, SC_FRIEND, F_DEFINE);
  add_global_decl (name, regexp, pos, 0, 1, SC_FRIEND, F_DEFINE);
}


/* Add information for the global definition of NAME.
   REGEXP is a regexp matching the declaration, if non-null.  POS is
   the position in the source where the declaration is found.  HASH is
   a hash code for the parameter list of the member, if it's a
   function.  VAR non-zero means member is a variable or type.  SC
   specifies the type of member (instance member, static, ...).  VIS
   is the member's visibility (public, protected, private).  FLAGS is
   a bit set giving additional information about the member (see the
   F_* defines).  */

static void
add_global_defn (char *name, char *regexp, int pos, unsigned int hash, int var, int sc, int flags)
{
  int i;
  struct sym *sym;

  /* Try to find out for which classes a function is a friend, and add
     what we know about it to them.  */
  if (!var)
    for (i = 0; i < TABLE_SIZE; ++i)
      for (sym = class_table[i]; sym; sym = sym->next)
	if (sym != global_symbols && sym->friends)
	  if (find_member (sym, name, 0, SC_FRIEND, hash))
	    add_member_defn (sym, name, regexp, pos, hash, 0,
			     SC_FRIEND, flags);

  /* Add to global symbols.  */
  add_member_defn (global_symbols, name, regexp, pos, hash, var, sc, flags);
}


/* Add information for the global declaration of NAME.
   REGEXP is a regexp matching the declaration, if non-null.  POS is
   the position in the source where the declaration is found.  HASH is
   a hash code for the parameter list of the member, if it's a
   function.  VAR non-zero means member is a variable or type.  SC
   specifies the type of member (instance member, static, ...).  VIS
   is the member's visibility (public, protected, private).  FLAGS is
   a bit set giving additional information about the member (see the
   F_* defines).  */

static void
add_global_decl (char *name, char *regexp, int pos, unsigned int hash, int var, int sc, int flags)
{
  /* Add declaration only if not already declared.  Header files must
     be processed before source files for this to have the right effect.
     I do not want to handle implicit declarations at the moment.  */
  struct member *m;
  struct member *found;

  m = found = find_member (global_symbols, name, var, sc, hash);
  if (m == NULL)
    m = add_member (global_symbols, name, var, sc, hash);

  /* Definition already seen => probably last declaration implicit.
     Override.  This means that declarations must always be added to
     the symbol table before definitions.  */
  if (!found)
    {
      if (!global_symbols->filename
	  || !FILENAME_EQ (global_symbols->filename, filename))
	m->filename = filename;

      m->regexp = regexp;
      m->pos = pos;
      m->vis = V_PUBLIC;
      m->flags = flags;

      info_where = P_DECL;
      info_cls = global_symbols;
      info_member = m;
    }
}


/* Add a symbol for member NAME to class CLS.
   VAR non-zero means it's a variable.  SC specifies the kind of
   member.  HASH is a hash code for the parameter types of a function.
   Value is a pointer to the member's structure.  */

static struct member *
add_member (struct sym *cls, char *name, int var, int sc, unsigned int hash)
{
  struct member *m = (struct member *) xmalloc (sizeof *m + strlen (name));
  struct member **list;
  struct member *p;
  struct member *prev;
  unsigned name_hash = 0;
  int i;
  char *s;

  strcpy (m->name, name);
  m->param_hash = hash;

  m->vis = 0;
  m->flags = 0;
  m->regexp = NULL;
  m->filename = NULL;
  m->pos = 0;
  m->def_regexp = NULL;
  m->def_filename = NULL;
  m->def_pos = 0;

  assert (cls != NULL);

  switch (sc)
    {
    case SC_FRIEND:
      list = &cls->friends;
      break;

    case SC_TYPE:
      list = &cls->types;
      break;

    case SC_STATIC:
      list = var ? &cls->static_vars : &cls->static_fns;
      break;

    default:
      list = var ? &cls->vars : &cls->fns;
      break;
    }

  for (s = name; *s; ++s)
    name_hash = (name_hash << 1) ^ *s;
  i = name_hash % TABLE_SIZE;
  m->anext = member_table[i];
  member_table[i] = m;
  m->list = list;

  /* Keep the member list sorted.  It's cheaper to do it here than to
     sort them in Lisp.  */
  for (prev = NULL, p = *list;
       p && strcmp (name, p->name) > 0;
       prev = p, p = p->next)
    ;

  m->next = p;
  if (prev)
    prev->next = m;
  else
    *list = m;
  return m;
}


/* Given the root R of a class tree, step through all subclasses
   recursively, marking functions as virtual that are declared virtual
   in base classes.  */

static void
mark_virtual (struct sym *r)
{
  struct link *p;
  struct member *m, *m2;

  for (p = r->subs; p; p = p->next)
    {
      for (m = r->fns; m; m = m->next)
        if (HAS_FLAG (m->flags, F_VIRTUAL))
          {
            for (m2 = p->sym->fns; m2; m2 = m2->next)
              if (m->param_hash == m2->param_hash && streq (m->name, m2->name))
                SET_FLAG (m2->flags, F_VIRTUAL);
          }

      mark_virtual (p->sym);
    }
}


/* For all roots of the class tree, mark functions as virtual that
   are virtual because of a virtual declaration in a base class.  */

static void
mark_inherited_virtual (void)
{
  struct sym *r;
  int i;

  for (i = 0; i < TABLE_SIZE; ++i)
    for (r = class_table[i]; r; r = r->next)
      if (r->supers == NULL)
        mark_virtual (r);
}


/* Create and return a symbol for a namespace with name NAME.  */

static struct sym *
make_namespace (char *name, struct sym *context)
{
  struct sym *s = (struct sym *) xmalloc (sizeof *s + strlen (name));
  memset (s, 0, sizeof *s);
  strcpy (s->name, name);
  s->next = all_namespaces;
  s->namesp = context;
  all_namespaces = s;
  return s;
}


/* Find the symbol for namespace NAME.  If not found, return NULL */

static struct sym *
check_namespace (char *name, struct sym *context)
{
  struct sym *p = NULL;

  for (p = all_namespaces; p; p = p->next)
    {
      if (streq (p->name, name) && (p->namesp == context))
	    break;
    }

  return p;
}

/* Find the symbol for namespace NAME.  If not found, add a new symbol
   for NAME to all_namespaces.  */

static struct sym *
find_namespace (char *name, struct sym *context)
{
  struct sym *p = check_namespace (name, context);

  if (p == NULL)
    p = make_namespace (name, context);

  return p;
}


/* Find namespace alias with name NAME. If not found return NULL. */

static struct link *
check_namespace_alias (char *name)
{
  struct link *p = NULL;
  struct alias *al;
  unsigned h;
  char *s;

  for (s = name, h = 0; *s; ++s)
    h = (h << 1) ^ *s;
  h %= TABLE_SIZE;

  for (al = namespace_alias_table[h]; al; al = al->next)
    if (streq (name, al->name) && (al->namesp == current_namespace))
      {
        p = al->aliasee;
        break;
      }

  return p;
}

/* Register the name NEW_NAME as an alias for namespace list OLD_NAME.  */

static void
register_namespace_alias (char *new_name, struct link *old_name)
{
  unsigned h;
  char *s;
  struct alias *al;

  for (s = new_name, h = 0; *s; ++s)
    h = (h << 1) ^ *s;
  h %= TABLE_SIZE;


  /* Is it already in the table of aliases?  */
  for (al = namespace_alias_table[h]; al; al = al->next)
    if (streq (new_name, al->name) && (al->namesp == current_namespace))
      return;

  al = (struct alias *) xmalloc (sizeof *al + strlen (new_name));
  strcpy (al->name, new_name);
  al->next = namespace_alias_table[h];
  al->namesp = current_namespace;
  al->aliasee = old_name;
  namespace_alias_table[h] = al;
}


/* Enter namespace with name NAME.  */

static void
enter_namespace (char *name)
{
  struct sym *p = find_namespace (name, current_namespace);

  if (namespace_sp == namespace_stack_size)
    {
      int size = max (10, 2 * namespace_stack_size);
      namespace_stack
	= (struct sym **) xrealloc ((void *)namespace_stack,
				    size * sizeof *namespace_stack);
      namespace_stack_size = size;
    }

  namespace_stack[namespace_sp++] = current_namespace;
  current_namespace = p;
}


/* Leave the current namespace.  */

static void
leave_namespace (void)
{
  assert (namespace_sp > 0);
  current_namespace = namespace_stack[--namespace_sp];
}



/***********************************************************************
		       Writing the Output File
 ***********************************************************************/

/* Write string S to the output file FP in a Lisp-readable form.
   If S is null, write out `()'.  */

static inline void
putstr (const char *s, FILE *fp)
{
  if (!s)
    {
      putc ('(', fp);
      putc (')', fp);
      putc (' ', fp);
    }
  else
    {
      putc ('"', fp);
      fputs (s, fp);
      putc ('"', fp);
      putc (' ', fp);
    }
}

/* A dynamically allocated buffer for constructing a scope name.  */

char *scope_buffer;
int scope_buffer_size;
int scope_buffer_len;


/* Make sure scope_buffer has enough room to add LEN chars to it.  */

static void
ensure_scope_buffer_room (int len)
{
  if (scope_buffer_len + len >= scope_buffer_size)
    {
      int new_size = max (2 * scope_buffer_size, scope_buffer_len + len);
      scope_buffer = (char *) xrealloc (scope_buffer, new_size);
      scope_buffer_size = new_size;
    }
}


/* Recursively add the scope names of symbol P and the scopes of its
   namespaces to scope_buffer.  Value is a pointer to the complete
   scope name constructed.  */

static char *
sym_scope_1 (struct sym *p)
{
  int len;

  if (p->namesp)
    sym_scope_1 (p->namesp);

  if (*scope_buffer)
    {
      ensure_scope_buffer_room (3);
      strcat (scope_buffer, "::");
      scope_buffer_len += 2;
    }

  len = strlen (p->name);
  ensure_scope_buffer_room (len + 1);
  strcat (scope_buffer, p->name);
  scope_buffer_len += len;

  if (HAS_FLAG (p->flags, F_TEMPLATE))
    {
      ensure_scope_buffer_room (3);
      strcat (scope_buffer, "<>");
      scope_buffer_len += 2;
    }

  return scope_buffer;
}


/* Return the scope of symbol P in printed representation, i.e.
   as it would appear in a C*+ source file.  */

static char *
sym_scope (struct sym *p)
{
  if (!scope_buffer)
    {
      scope_buffer_size = 1024;
      scope_buffer = (char *) xmalloc (scope_buffer_size);
    }

  *scope_buffer = '\0';
  scope_buffer_len = 0;

  if (p->namesp)
    sym_scope_1 (p->namesp);

  return scope_buffer;
}


/* Dump the list of members M to file FP.  Value is the length of the
   list.  */

static int
dump_members (FILE *fp, struct member *m)
{
  int n;

  putc ('(', fp);

  for (n = 0; m; m = m->next, ++n)
    {
      fputs (MEMBER_STRUCT, fp);
      putstr (m->name, fp);
      putstr (NULL, fp);		/* FIXME? scope for globals */
      fprintf (fp, "%u ", (unsigned) m->flags);
      putstr (m->filename, fp);
      putstr (m->regexp, fp);
      fprintf (fp, "%u ", (unsigned) m->pos);
      fprintf (fp, "%u ", (unsigned) m->vis);
      putc (' ', fp);
      putstr (m->def_filename, fp);
      putstr (m->def_regexp, fp);
      fprintf (fp, "%u", (unsigned) m->def_pos);
      putc (']', fp);
      putc ('\n', fp);
    }

  putc (')', fp);
  putc ('\n', fp);
  return n;
}


/* Dump class ROOT to stream FP.  */

static void
dump_sym (FILE *fp, struct sym *root)
{
  fputs (CLASS_STRUCT, fp);
  putstr (root->name, fp);

  /* Print scope, if any.  */
  if (root->namesp)
    putstr (sym_scope (root), fp);
  else
    putstr (NULL, fp);

  /* Print flags.  */
  fprintf (fp, "%u", root->flags);
  putstr (root->filename, fp);
  putstr (root->regexp, fp);
  fprintf (fp, "%u", (unsigned) root->pos);
  putstr (root->sfilename, fp);
  putc (']', fp);
  putc ('\n', fp);
}


/* Dump class ROOT and its subclasses to file FP.  Value is the
   number of classes written.  */

static int
dump_tree (FILE *fp, struct sym *root)
{
  struct link *lk;
  unsigned n = 0;

  dump_sym (fp, root);

  if (f_verbose)
    {
      putchar ('+');
      fflush (stdout);
    }

  putc ('(', fp);

  for (lk = root->subs; lk; lk = lk->next)
    {
      fputs (TREE_STRUCT, fp);
      n += dump_tree (fp, lk->sym);
      putc (']', fp);
    }

  putc (')', fp);

  dump_members (fp, root->vars);
  n += dump_members (fp, root->fns);
  dump_members (fp, root->static_vars);
  n += dump_members (fp, root->static_fns);
  n += dump_members (fp, root->friends);
  dump_members (fp, root->types);

  /* Superclasses.  */
  putc ('(', fp);
  putc (')', fp);

  /* Mark slot.  */
  putc ('(', fp);
  putc (')', fp);

  putc ('\n', fp);
  return n;
}


/* Dump the entire class tree to file FP.  */

static void
dump_roots (FILE *fp)
{
  int i, n = 0;
  struct sym *r;

  /* Output file header containing version string, command line
     options etc.  */
  if (!f_append)
    {
      fputs (TREE_HEADER_STRUCT, fp);
      putstr (EBROWSE_FILE_VERSION, fp);

      putc ('\"', fp);
      if (!f_structs)
	fputs (" -s", fp);
      if (f_regexps)
	fputs (" -x", fp);
      putc ('\"', fp);
      fputs (" ()", fp);
      fputs (" ()", fp);
      putc (']', fp);
    }

  /* Mark functions as virtual that are so because of functions
     declared virtual in base classes.  */
  mark_inherited_virtual ();

  /* Dump the roots of the graph.  */
  for (i = 0; i < TABLE_SIZE; ++i)
    for (r = class_table[i]; r; r = r->next)
      if (!r->supers)
        {
	  fputs (TREE_STRUCT, fp);
          n += dump_tree (fp, r);
	  putc (']', fp);
        }

  if (f_verbose)
    putchar ('\n');
}



/***********************************************************************
				Scanner
 ***********************************************************************/

#ifdef DEBUG
#define INCREMENT_LINENO 			\
do {						\
  if (f_very_verbose)				\
    {						\
      ++yyline;					\
      printf ("%d:\n", yyline);			\
    }						\
  else						\
    ++yyline;					\
} while (0)
#else
#define INCREMENT_LINENO	++yyline
#endif

/* Define two macros for accessing the input buffer (current input
   file).  GET(C) sets C to the next input character and advances the
   input pointer.  UNGET retracts the input pointer.  */

#define GET(C)	((C) = *in++)
#define UNGET() (--in)


/* Process a preprocessor line.  Value is the next character from the
   input buffer not consumed.  */

static int
process_pp_line (void)
{
  int in_comment = 0, in_string = 0;
  int c;
  char *p = yytext;

  /* Skip over white space.  The `#' has been consumed already.  */
  while (WHITEP (GET (c)))
    ;

  /* Read the preprocessor command (if any).  */
  while (IDENTP (c))
    {
      *p++ = c;
      GET (c);
    }

  /* Is it a `define'?  */
  *p = '\0';

  if (*yytext && streq (yytext, "define"))
    {
      p = yytext;
      while (WHITEP (c))
	GET (c);
      while (IDENTP (c))
	{
	  *p++ = c;
	  GET (c);
	}

      *p = '\0';

      if (*yytext)
	{
	  char *regexp = matching_regexp ();
	  int pos = BUFFER_POS ();
	  add_define (yytext, regexp, pos);
	}
    }

  while (c && (c != '\n' || in_comment || in_string))
    {
      if (c == '\\')
	GET (c);
      else if (c == '/' && !in_comment)
	{
	  if (GET (c) == '*')
	    in_comment = 1;
	}
      else if (c == '*' && in_comment)
	{
	  if (GET (c) == '/')
	    in_comment = 0;
	}
      else if (c == '"')
	in_string = !in_string;

      if (c == '\n')
	INCREMENT_LINENO;

      GET (c);
    }

  return c;
}


/* Value is the next token from the input buffer.  */

static int
yylex (void)
{
  int c;
  char end_char;
  char *p;

  for (;;)
    {
      while (WHITEP (GET (c)))
        ;

      switch (c)
        {
        case '\n':
          INCREMENT_LINENO;
          break;

        case '\r':
          break;

        case 0:
          /* End of file.  */
          return YYEOF;

        case '\\':
          GET (c);
          break;

        case '"':
        case '\'':
          /* String and character constants.  */
          end_char = c;
          string_start = in;
          while (GET (c) && c != end_char)
            {
              switch (c)
                {
                case '\\':
                  /* Escape sequences.  */
                  if (!GET (c))
                    {
                      if (end_char == '\'')
                        yyerror ("EOF in character constant", NULL);
                      else
                        yyerror ("EOF in string constant", NULL);
                      goto end_string;
                    }
                  else switch (c)
                    {
                    case '\n':
		      INCREMENT_LINENO;
                    case 'a':
                    case 'b':
                    case 'f':
                    case 'n':
                    case 'r':
                    case 't':
                    case 'v':
                      break;

                    case 'x':
                      {
                        /* Hexadecimal escape sequence.  */
                        int i;
                        for (i = 0; i < 2; ++i)
                          {
                            GET (c);

                            if (c >= '0' && c <= '7')
                              ;
                            else if (c >= 'a' && c <= 'f')
                              ;
                            else if (c >= 'A' && c <= 'F')
                              ;
                            else
                              {
                                UNGET ();
                                break;
                              }
                          }
                      }
                      break;

                    case '0':
                      {
                        /* Octal escape sequence.  */
                        int i;
                        for (i = 0; i < 3; ++i)
                          {
                            GET (c);

                            if (c >= '0' && c <= '7')
                              ;
                            else
                              {
                                UNGET ();
                                break;
                              }
                          }
                      }
                      break;

                    default:
                      break;
                    }
                  break;

                case '\n':
                  if (end_char == '\'')
                    yyerror ("newline in character constant", NULL);
                  else
                    yyerror ("newline in string constant", NULL);
                  INCREMENT_LINENO;
                  break;

                default:
                  break;
                }
            }

        end_string:
          return end_char == '\'' ? CCHAR : CSTRING;

        case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g':
        case 'h': case 'i': case 'j': case 'k': case 'l': case 'm': case 'n':
        case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u':
        case 'v': case 'w': case 'x': case 'y': case 'z':
        case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G':
        case 'H': case 'I': case 'J': case 'K': case 'L': case 'M': case 'N':
        case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T': case 'U':
        case 'V': case 'W': case 'X': case 'Y': case 'Z': case '_':
          {
            /* Identifier and keywords.  */
            unsigned hash;
            struct kw *k;

            p = yytext;
            *p++ = hash = c;

            while (IDENTP (GET (*p)))
	      {
		hash = (hash << 1) ^ *p++;
		if (p == yytext_end - 1)
		  {
		    int size = yytext_end - yytext;
		    yytext = (char *) xrealloc (yytext, 2 * size);
		    yytext_end = yytext + 2 * size;
		    p = yytext + size - 1;
		  }
	      }

            UNGET ();
            *p = 0;

            for (k = keyword_table[hash % KEYWORD_TABLE_SIZE]; k; k = k->next)
              if (streq (k->name, yytext))
                return k->tk;

            return IDENT;
          }

        case '/':
          /* C and C++ comments, '/' and '/='.  */
          switch (GET (c))
            {
            case '*':
              while (GET (c))
                {
                  switch (c)
                    {
                    case '*':
                      if (GET (c) == '/')
                        goto comment_end;
                      UNGET ();
                      break;
                    case '\\':
                      GET (c);
                      break;
                    case '\n':
                      INCREMENT_LINENO;
                      break;
                    }
                }
            comment_end:;
              break;

            case '=':
              return DIVASGN;

            case '/':
	      while (GET (c) && c != '\n')
		;
	      /* Don't try to read past the end of the input buffer if
		 the file ends in a C++ comment without a newline.  */
	      if (c == 0)
		return YYEOF;

	      INCREMENT_LINENO;
	      break;

            default:
              UNGET ();
              return '/';
            }
          break;

        case '+':
          if (GET (c) == '+')
            return INC;
          else if (c == '=')
            return ADDASGN;
          UNGET ();
          return '+';

        case '-':
          switch (GET (c))
            {
            case '-':
              return DEC;
            case '>':
              if (GET (c) == '*')
                return ARROWSTAR;
              UNGET ();
              return ARROW;
            case '=':
              return SUBASGN;
            }
          UNGET ();
          return '-';

        case '*':
          if (GET (c) == '=')
            return MULASGN;
          UNGET ();
          return '*';

        case '%':
          if (GET (c) == '=')
            return MODASGN;
          UNGET ();
          return '%';

        case '|':
          if (GET (c) == '|')
            return LOR;
          else if (c == '=')
            return ORASGN;
          UNGET ();
          return '|';

        case '&':
          if (GET (c) == '&')
            return LAND;
          else if (c == '=')
            return ANDASGN;
          UNGET ();
          return '&';

        case '^':
          if (GET (c) == '=')
            return XORASGN;
          UNGET ();
          return '^';

        case '.':
          if (GET (c) == '*')
            return POINTSTAR;
          else if (c == '.')
            {
              if (GET (c) != '.')
                yyerror ("invalid token '..' ('...' assumed)", NULL);
              UNGET ();
              return ELLIPSIS;
            }
          else if (!DIGITP (c))
            {
              UNGET ();
              return '.';
            }
          goto mantissa;

        case ':':
          if (GET (c) == ':')
            return DCOLON;
          UNGET ();
          return ':';

        case '=':
          if (GET (c) == '=')
            return EQ;
          UNGET ();
          return '=';

        case '!':
          if (GET (c) == '=')
            return NE;
          UNGET ();
          return '!';

        case '<':
          switch (GET (c))
            {
            case '=':
              return LE;
            case '<':
              if (GET (c) == '=')
                return LSHIFTASGN;
              UNGET ();
              return LSHIFT;
            }
          UNGET ();
          return '<';

        case '>':
          switch (GET (c))
            {
            case '=':
              return GE;
            case '>':
              if (GET (c) == '=')
                return RSHIFTASGN;
              UNGET ();
              return RSHIFT;
            }
          UNGET ();
          return '>';

        case '#':
          c = process_pp_line ();
          if (c == 0)
            return YYEOF;
          break;

        case '(': case ')': case '[': case ']': case '{': case '}':
        case ';': case ',': case '?': case '~':
          return c;

        case '0':
          yyival = 0;

          if (GET (c) == 'x' || c == 'X')
            {
              while (GET (c))
                {
                  if (DIGITP (c))
                    yyival = yyival * 16 + c - '0';
                  else if (c >= 'a' && c <= 'f')
                    yyival = yyival * 16 + c - 'a' + 10;
                  else if (c >= 'A' && c <= 'F')
                    yyival = yyival * 16 + c - 'A' + 10;
                  else
                    break;
                }

              goto int_suffixes;
            }
          else if (c == '.')
            goto mantissa;

          while (c >= '0' && c <= '7')
            {
              yyival = (yyival << 3) + c - '0';
              GET (c);
            }

        int_suffixes:
          /* Integer suffixes.  */
          while (isalpha (c))
            GET (c);
          UNGET ();
          return CINT;

        case '1': case '2': case '3': case '4': case '5': case '6':
        case '7': case '8': case '9':
          /* Integer or floating constant, part before '.'.  */
          yyival = c - '0';

          while (GET (c) && DIGITP (c))
            yyival = 10 * yyival + c - '0';

          if (c != '.')
            goto int_suffixes;

        mantissa:
          /* Digits following '.'.  */
          while (DIGITP (c))
            GET (c);

          /* Optional exponent.  */
          if (c == 'E' || c == 'e')
            {
              if (GET (c) == '-' || c == '+')
                GET (c);

              while (DIGITP (c))
                GET (c);
            }

          /* Optional type suffixes.  */
          while (isalpha (c))
            GET (c);
	  UNGET ();
          return CFLOAT;

        default:
          break;
        }
    }
}


/* Actually local to matching_regexp.  These variables must be in
   global scope for the case that `static' get's defined away.  */

static char *matching_regexp_buffer, *matching_regexp_end_buf;


/* Value is the string from the start of the line to the current
   position in the input buffer, or maybe a bit more if that string is
   shorter than min_regexp.  */

static char *
matching_regexp (void)
{
  char *p;
  char *s;
  char *t;

  if (!f_regexps)
    return NULL;

  if (matching_regexp_buffer == NULL)
    {
      matching_regexp_buffer = (char *) xmalloc (max_regexp);
      matching_regexp_end_buf = &matching_regexp_buffer[max_regexp] - 1;
    }

  /* Scan back to previous newline of buffer start.  */
  for (p = in - 1; p > inbuffer && *p != '\n'; --p)
    ;

  if (*p == '\n')
    {
      while (in - p < min_regexp && p > inbuffer)
        {
          /* Line probably not significant enough */
          for (--p; p > inbuffer && *p != '\n'; --p)
            ;
        }
      if (*p == '\n')
        ++p;
    }

  /* Copy from end to make sure significant portions are included.
     This implies that in the browser a regular expressing of the form
     `^.*{regexp}' has to be used.  */
  for (s = matching_regexp_end_buf - 1, t = in;
       s > matching_regexp_buffer && t > p;)
    {
      *--s = *--t;

      if (*s == '"' || *s == '\\')
        *--s = '\\';
    }

  *(matching_regexp_end_buf - 1) = '\0';
  return xstrdup (s);
}


/* Return a printable representation of token T.  */

static const char *
token_string (int t)
{
  static char b[3];

  switch (t)
    {
    case CSTRING:               return "string constant";
    case CCHAR:                 return "char constant";
    case CINT:                  return "int constant";
    case CFLOAT:                return "floating constant";
    case ELLIPSIS:              return "...";
    case LSHIFTASGN:            return "<<=";
    case RSHIFTASGN:            return ">>=";
    case ARROWSTAR:             return "->*";
    case IDENT:                 return "identifier";
    case DIVASGN:               return "/=";
    case INC:                   return "++";
    case ADDASGN:               return "+=";
    case DEC:                   return "--";
    case ARROW:                 return "->";
    case SUBASGN:               return "-=";
    case MULASGN:               return "*=";
    case MODASGN:               return "%=";
    case LOR:                   return "||";
    case ORASGN:                return "|=";
    case LAND:                  return "&&";
    case ANDASGN:               return "&=";
    case XORASGN:               return "^=";
    case POINTSTAR:             return ".*";
    case DCOLON:                return "::";
    case EQ:                    return "==";
    case NE:                    return "!=";
    case LE:                    return "<=";
    case LSHIFT:                return "<<";
    case GE:                    return ">=";
    case RSHIFT:                return ">>";
    case ASM:                   return "asm";
    case AUTO:                  return "auto";
    case BREAK:                 return "break";
    case CASE:                  return "case";
    case CATCH:                 return "catch";
    case CHAR:                  return "char";
    case CLASS:                 return "class";
    case CONST:                 return "const";
    case CONTINUE:              return "continue";
    case DEFAULT:               return "default";
    case DELETE:                return "delete";
    case DO:                    return "do";
    case DOUBLE:                return "double";
    case ELSE:                  return "else";
    case ENUM:                  return "enum";
    case EXTERN:                return "extern";
    case FLOAT:                 return "float";
    case FOR:                   return "for";
    case FRIEND:                return "friend";
    case GOTO:                  return "goto";
    case IF:                    return "if";
    case T_INLINE:              return "inline";
    case INT:                   return "int";
    case LONG:                  return "long";
    case NEW:                   return "new";
    case OPERATOR:              return "operator";
    case PRIVATE:               return "private";
    case PROTECTED:             return "protected";
    case PUBLIC:                return "public";
    case REGISTER:              return "register";
    case RETURN:                return "return";
    case SHORT:                 return "short";
    case SIGNED:                return "signed";
    case SIZEOF:                return "sizeof";
    case STATIC:                return "static";
    case STRUCT:                return "struct";
    case SWITCH:                return "switch";
    case TEMPLATE:              return "template";
    case THIS:                  return "this";
    case THROW:                 return "throw";
    case TRY:                   return "try";
    case TYPEDEF:               return "typedef";
    case UNION:                 return "union";
    case UNSIGNED:              return "unsigned";
    case VIRTUAL:               return "virtual";
    case VOID:                  return "void";
    case VOLATILE:              return "volatile";
    case WHILE:                 return "while";
    case MUTABLE:		return "mutable";
    case BOOL:			return "bool";
    case TRUE:			return "true";
    case FALSE:			return "false";
    case SIGNATURE:		return "signature";
    case NAMESPACE:		return "namespace";
    case EXPLICIT:		return "explicit";
    case TYPENAME:		return "typename";
    case CONST_CAST:		return "const_cast";
    case DYNAMIC_CAST:		return "dynamic_cast";
    case REINTERPRET_CAST:	return "reinterpret_cast";
    case STATIC_CAST:		return "static_cast";
    case TYPEID:		return "typeid";
    case USING:			return "using";
    case WCHAR:			return "wchar_t";
    case YYEOF:                 return "EOF";

    default:
      if (t < 255)
	{
	  b[0] = t;
	  b[1] = '\0';
	  return b;
	}
      else
	return "???";
    }
}


/* Reinitialize the scanner for a new input file.  */

static void
re_init_scanner (void)
{
  in = inbuffer;
  yyline = 1;

  if (yytext == NULL)
    {
      int size = 256;
      yytext = (char *) xmalloc (size * sizeof *yytext);
      yytext_end = yytext + size;
    }
}


/* Insert a keyword NAME with token value TKV into the keyword hash
   table.  */

static void
insert_keyword (const char *name, int tkv)
{
  const char *s;
  unsigned h = 0;
  struct kw *k = (struct kw *) xmalloc (sizeof *k);

  for (s = name; *s; ++s)
    h = (h << 1) ^ *s;

  h %= KEYWORD_TABLE_SIZE;
  k->name = name;
  k->tk = tkv;
  k->next = keyword_table[h];
  keyword_table[h] = k;
}


/* Initialize the scanner for the first file.  This sets up the
   character class vectors and fills the keyword hash table.  */

static void
init_scanner (void)
{
  int i;

  /* Allocate the input buffer */
  inbuffer_size = READ_CHUNK_SIZE + 1;
  inbuffer = in = (char *) xmalloc (inbuffer_size);
  yyline = 1;

  /* Set up character class vectors.  */
  for (i = 0; i < sizeof is_ident; ++i)
    {
      if (i == '_' || isalnum (i))
        is_ident[i] = 1;

      if (i >= '0' && i <= '9')
        is_digit[i] = 1;

      if (i == ' ' || i == '\t' || i == '\f' || i == '\v')
        is_white[i] = 1;
    }

  /* Fill keyword hash table.  */
  insert_keyword ("and", LAND);
  insert_keyword ("and_eq", ANDASGN);
  insert_keyword ("asm", ASM);
  insert_keyword ("auto", AUTO);
  insert_keyword ("bitand", '&');
  insert_keyword ("bitor", '|');
  insert_keyword ("bool", BOOL);
  insert_keyword ("break", BREAK);
  insert_keyword ("case", CASE);
  insert_keyword ("catch", CATCH);
  insert_keyword ("char", CHAR);
  insert_keyword ("class", CLASS);
  insert_keyword ("compl", '~');
  insert_keyword ("const", CONST);
  insert_keyword ("const_cast", CONST_CAST);
  insert_keyword ("continue", CONTINUE);
  insert_keyword ("default", DEFAULT);
  insert_keyword ("delete", DELETE);
  insert_keyword ("do", DO);
  insert_keyword ("double", DOUBLE);
  insert_keyword ("dynamic_cast", DYNAMIC_CAST);
  insert_keyword ("else", ELSE);
  insert_keyword ("enum", ENUM);
  insert_keyword ("explicit", EXPLICIT);
  insert_keyword ("extern", EXTERN);
  insert_keyword ("false", FALSE);
  insert_keyword ("float", FLOAT);
  insert_keyword ("for", FOR);
  insert_keyword ("friend", FRIEND);
  insert_keyword ("goto", GOTO);
  insert_keyword ("if", IF);
  insert_keyword ("inline", T_INLINE);
  insert_keyword ("int", INT);
  insert_keyword ("long", LONG);
  insert_keyword ("mutable", MUTABLE);
  insert_keyword ("namespace", NAMESPACE);
  insert_keyword ("new", NEW);
  insert_keyword ("not", '!');
  insert_keyword ("not_eq", NE);
  insert_keyword ("operator", OPERATOR);
  insert_keyword ("or", LOR);
  insert_keyword ("or_eq", ORASGN);
  insert_keyword ("private", PRIVATE);
  insert_keyword ("protected", PROTECTED);
  insert_keyword ("public", PUBLIC);
  insert_keyword ("register", REGISTER);
  insert_keyword ("reinterpret_cast", REINTERPRET_CAST);
  insert_keyword ("return", RETURN);
  insert_keyword ("short", SHORT);
  insert_keyword ("signed", SIGNED);
  insert_keyword ("sizeof", SIZEOF);
  insert_keyword ("static", STATIC);
  insert_keyword ("static_cast", STATIC_CAST);
  insert_keyword ("struct", STRUCT);
  insert_keyword ("switch", SWITCH);
  insert_keyword ("template", TEMPLATE);
  insert_keyword ("this", THIS);
  insert_keyword ("throw", THROW);
  insert_keyword ("true", TRUE);
  insert_keyword ("try", TRY);
  insert_keyword ("typedef", TYPEDEF);
  insert_keyword ("typeid", TYPEID);
  insert_keyword ("typename", TYPENAME);
  insert_keyword ("union", UNION);
  insert_keyword ("unsigned", UNSIGNED);
  insert_keyword ("using", USING);
  insert_keyword ("virtual", VIRTUAL);
  insert_keyword ("void", VOID);
  insert_keyword ("volatile", VOLATILE);
  insert_keyword ("wchar_t", WCHAR);
  insert_keyword ("while", WHILE);
  insert_keyword ("xor", '^');
  insert_keyword ("xor_eq", XORASGN);
}



/***********************************************************************
				Parser
 ***********************************************************************/

/* Match the current lookahead token and set it to the next token.  */

#define MATCH() (tk = yylex ())

/* Return the lookahead token.  If current lookahead token is cleared,
   read a new token.  */

#define LA1 (tk == -1 ? (tk = yylex ()) : tk)

/* Is the current lookahead equal to the token T? */

#define LOOKING_AT(T) (tk == (T))

/* Is the current lookahead one of T1 or T2?  */

#define LOOKING_AT2(T1, T2)	(tk == (T1) || tk == (T2))

/* Is the current lookahead one of T1, T2 or T3?  */

#define LOOKING_AT3(T1, T2, T3)	(tk == (T1) || tk == (T2) || tk == (T3))

/* Is the current lookahead one of T1...T4?  */

#define LOOKING_AT4(T1, T2, T3, T4) \
     (tk == (T1) || tk == (T2) || tk == (T3) || tk == (T4))

/* Match token T if current lookahead is T.  */

#define MATCH_IF(T) if (LOOKING_AT (T)) MATCH (); else ((void) 0)

/* Skip to matching token if current token is T.  */

#define SKIP_MATCHING_IF(T) \
  if (LOOKING_AT (T)) skip_matching (); else ((void) 0)


/* Skip forward until a given token TOKEN or YYEOF is seen and return
   the current lookahead token after skipping.  */

static int
skip_to (int token)
{
  while (!LOOKING_AT2 (YYEOF, token))
    MATCH ();
  return tk;
}

/* Skip over pairs of tokens (parentheses, square brackets,
   angle brackets, curly brackets) matching the current lookahead.  */

static void
skip_matching (void)
{
  int open, close, n;

  switch (open = LA1)
    {
    case '{':
      close = '}';
      break;

    case '(':
      close = ')';
      break;

    case '<':
      close = '>';
      break;

    case '[':
      close = ']';
      break;

    default:
      abort ();
    }

  for (n = 0;;)
    {
      if (LOOKING_AT (open))
        ++n;
      else if (LOOKING_AT (close))
        --n;
      else if (LOOKING_AT (YYEOF))
        break;

      MATCH ();

      if (n == 0)
        break;
    }
}

static void
skip_initializer (void)
{
  for (;;)
    {
      switch (LA1)
	{
	case ';':
	case ',':
	case YYEOF:
	  return;

	case '{':
	case '[':
	case '(':
	  skip_matching ();
	  break;

	default:
	  MATCH ();
	  break;
	}
    }
}

/* Build qualified namespace alias (A::B::c) and return it. */

static struct link *
match_qualified_namespace_alias (void)
{
  struct link *head = NULL;
  struct link *cur = NULL;
  struct link *tmp = NULL;

  for (;;)
    {
      MATCH ();
      switch (LA1)
        {
        case IDENT:
          tmp = (struct link *) xmalloc (sizeof *cur);
          tmp->sym = find_namespace (yytext, cur ? cur->sym : NULL);
          tmp->next = NULL;
          if (head)
            {
              cur = cur->next = tmp;
            }
          else
            {
              head = cur = tmp;
            }
          break;
        case DCOLON:
          /* Just skip */
          break;
        default:
          return head;
          break;
        }
    }
}

/* Re-initialize the parser by resetting the lookahead token.  */

static void
re_init_parser (void)
{
  tk = -1;
}


/* Parse a parameter list, including the const-specifier,
   pure-specifier, and throw-list that may follow a parameter list.
   Return in FLAGS what was seen following the parameter list.
   Returns a hash code for the parameter types.  This value is used to
   distinguish between overloaded functions.  */

static unsigned
parm_list (int *flags)
{
  unsigned hash = 0;
  int type_seen = 0;

  while (!LOOKING_AT2 (YYEOF, ')'))
    {
      switch (LA1)
        {
	  /* Skip over grouping parens or parameter lists in parameter
	     declarations.  */
        case '(':
          skip_matching ();
          break;

	  /* Next parameter.  */
        case ',':
          MATCH ();
          type_seen = 0;
          break;

          /* Ignore the scope part of types, if any.  This is because
             some types need scopes when defined outside of a class body,
             and don't need them inside the class body.  This means that
             we have to look for the last IDENT in a sequence of
             IDENT::IDENT::...  */
        case IDENT:
          if (!type_seen)
            {
	      char *last_id;
	      unsigned ident_type_hash = 0;

	      parse_qualified_param_ident_or_type (&last_id);
	      if (last_id)
		{
		  /* LAST_ID null means something like `X::*'.  */
		  for (; *last_id; ++last_id)
		    ident_type_hash = (ident_type_hash << 1) ^ *last_id;
		  hash = (hash << 1) ^ ident_type_hash;
		  type_seen = 1;
		}
            }
	  else
	    MATCH ();
          break;

        case VOID:
          /* This distinction is made to make `func (void)' equivalent
             to `func ()'.  */
          type_seen = 1;
          MATCH ();
          if (!LOOKING_AT (')'))
            hash = (hash << 1) ^ VOID;
          break;

        case BOOL:      case CHAR:      case CLASS:     case CONST:
        case DOUBLE:    case ENUM:      case FLOAT:     case INT:
        case LONG:      case SHORT:     case SIGNED:    case STRUCT:
        case UNION:     case UNSIGNED:  case VOLATILE:  case WCHAR:
        case ELLIPSIS:
          type_seen = 1;
          hash = (hash << 1) ^ LA1;
          MATCH ();
          break;

        case '*':       case '&':       case '[':       case ']':
          hash = (hash << 1) ^ LA1;
          MATCH ();
          break;

        default:
          MATCH ();
          break;
        }
    }

  if (LOOKING_AT (')'))
    {
      MATCH ();

      if (LOOKING_AT (CONST))
        {
          /* We can overload the same function on `const' */
          hash = (hash << 1) ^ CONST;
          SET_FLAG (*flags, F_CONST);
          MATCH ();
        }

      if (LOOKING_AT (THROW))
        {
          MATCH ();
          SKIP_MATCHING_IF ('(');
          SET_FLAG (*flags, F_THROW);
        }

      if (LOOKING_AT ('='))
        {
          MATCH ();
          if (LOOKING_AT (CINT) && yyival == 0)
            {
              MATCH ();
              SET_FLAG (*flags, F_PURE);
            }
        }
    }

  return hash;
}


/* Print position info to stdout.  */

static void
print_info (void)
{
  if (info_position >= 0 && BUFFER_POS () <= info_position)
    if (info_cls)
      printf ("(\"%s\" \"%s\" \"%s\" %d)\n",
	      info_cls->name, sym_scope (info_cls),
	      info_member->name, info_where);
}


/* Parse a member declaration within the class body of CLS.  VIS is
   the access specifier for the member (private, protected,
   public).  */

static void
member (struct sym *cls, int vis)
{
  char *id = NULL;
  int sc = SC_MEMBER;
  char *regexp = NULL;
  int pos;
  int is_constructor;
  int anonymous = 0;
  int flags = 0;
  int class_tag;
  int type_seen = 0;
  int paren_seen = 0;
  unsigned hash = 0;
  int tilde = 0;

  while (!LOOKING_AT4 (';', '{', '}', YYEOF))
    {
      switch (LA1)
        {
        default:
          MATCH ();
          break;

          /* A function or class may follow.  */
        case TEMPLATE:
          MATCH ();
          SET_FLAG (flags, F_TEMPLATE);
          /* Skip over template argument list */
          SKIP_MATCHING_IF ('<');
          break;

        case EXPLICIT:
          SET_FLAG (flags, F_EXPLICIT);
          goto typeseen;

        case MUTABLE:
          SET_FLAG (flags, F_MUTABLE);
          goto typeseen;

        case T_INLINE:
          SET_FLAG (flags, F_INLINE);
          goto typeseen;

        case VIRTUAL:
          SET_FLAG (flags, F_VIRTUAL);
          goto typeseen;

        case '[':
          skip_matching ();
          break;

        case ENUM:
          sc = SC_TYPE;
          goto typeseen;

        case TYPEDEF:
          sc = SC_TYPE;
          goto typeseen;

        case FRIEND:
          sc = SC_FRIEND;
          goto typeseen;

        case STATIC:
          sc = SC_STATIC;
          goto typeseen;

        case '~':
	  tilde = 1;
          MATCH ();
          break;

        case IDENT:
	  /* Remember IDENTS seen so far.  Among these will be the member
	     name.  */
	  id = (char *) xrealloc (id, strlen (yytext) + 2);
	  if (tilde)
	    {
	      *id = '~';
	      strcpy (id + 1, yytext);
	    }
	  else
	    strcpy (id, yytext);
	  MATCH ();
	  break;

        case OPERATOR:
	  {
	    char *s = operator_name (&sc);
	    id = (char *) xrealloc (id, strlen (s) + 1);
	    strcpy (id, s);
	  }
          break;

        case '(':
          /* Most probably the beginning of a parameter list.  */
          MATCH ();
          paren_seen = 1;

          if (id && cls)
            {
              if (!(is_constructor = streq (id, cls->name)))
                regexp = matching_regexp ();
            }
          else
            is_constructor = 0;

          pos = BUFFER_POS ();
          hash = parm_list (&flags);

          if (is_constructor)
            regexp = matching_regexp ();

          if (id && cls != NULL)
	    add_member_decl (cls, id, regexp, pos, hash, 0, sc, vis, flags);

          while (!LOOKING_AT3 (';', '{', YYEOF))
            MATCH ();

          if (LOOKING_AT ('{') && id && cls)
	    add_member_defn (cls, id, regexp, pos, hash, 0, sc, flags);

	  free (id);
          id = NULL;
          sc = SC_MEMBER;
          break;

        case STRUCT: case UNION: case CLASS:
          /* Nested class */
          class_tag = LA1;
          type_seen = 1;
          MATCH ();
          anonymous = 1;

          /* More than one ident here to allow for MS-DOS specialties
             like `_export class' etc.  The last IDENT seen counts
             as the class name.  */
	  while (!LOOKING_AT4 (YYEOF, ';', ':', '{'))
	    {
	      if (LOOKING_AT (IDENT))
		anonymous = 0;
	      MATCH ();
	    }

          if (LOOKING_AT2 (':', '{'))
	    class_definition (anonymous ? NULL : cls, class_tag, flags, 1);
          else
            skip_to (';');
          break;

        case INT:       case CHAR:      case LONG:      case UNSIGNED:
        case SIGNED:    case CONST:     case DOUBLE:    case VOID:
        case SHORT:     case VOLATILE:  case BOOL:      case WCHAR:
        case TYPENAME:
        typeseen:
          type_seen = 1;
          MATCH ();
          break;
        }
    }

  if (LOOKING_AT (';'))
    {
      /* The end of a member variable, a friend declaration or an access
         declaration.  We don't want to add friend classes as members.  */
      if (id && sc != SC_FRIEND && cls)
        {
          regexp = matching_regexp ();
          pos = BUFFER_POS ();

          if (cls != NULL)
            {
              if (type_seen || !paren_seen)
		add_member_decl (cls, id, regexp, pos, 0, 1, sc, vis, 0);
              else
		add_member_decl (cls, id, regexp, pos, hash, 0, sc, vis, 0);
            }
        }

      MATCH ();
      print_info ();
    }
  else if (LOOKING_AT ('{'))
    {
      /* A named enum.  */
      if (sc == SC_TYPE && id && cls)
        {
          regexp = matching_regexp ();
          pos = BUFFER_POS ();

          if (cls != NULL)
            {
              add_member_decl (cls, id, regexp, pos, 0, 1, sc, vis, 0);
              add_member_defn (cls, id, regexp, pos, 0, 1, sc, 0);
            }
        }

      skip_matching ();
      print_info ();
    }

  free (id);
}


/* Parse the body of class CLS.  TAG is the tag of the class (struct,
   union, class).  */

static void
class_body (struct sym *cls, int tag)
{
  int vis = tag == CLASS ? PRIVATE : PUBLIC;
  int temp;

  while (!LOOKING_AT2 (YYEOF, '}'))
    {
      switch (LA1)
        {
        case PRIVATE: case PROTECTED: case PUBLIC:
          temp = LA1;
          MATCH ();

          if (LOOKING_AT (':'))
            {
              vis = temp;
              MATCH ();
            }
          else
            {
              /* Probably conditional compilation for inheritance list.
                 We don't known whether there comes more of this.
                 This is only a crude fix that works most of the time.  */
              do
                {
                  MATCH ();
                }
              while (LOOKING_AT2 (IDENT, ',')
                     || LOOKING_AT3 (PUBLIC, PROTECTED, PRIVATE));
            }
          break;

        case TYPENAME:
        case USING:
          skip_to (';');
          break;

          /* Try to synchronize */
        case CHAR:      case CLASS:     case CONST:
        case DOUBLE:    case ENUM:      case FLOAT:     case INT:
        case LONG:      case SHORT:     case SIGNED:    case STRUCT:
        case UNION:     case UNSIGNED:  case VOID:      case VOLATILE:
        case TYPEDEF:   case STATIC:    case T_INLINE:  case FRIEND:
        case VIRTUAL:   case TEMPLATE:  case IDENT:     case '~':
        case BOOL:      case WCHAR:     case EXPLICIT:  case MUTABLE:
          member (cls, vis);
          break;

        default:
          MATCH ();
          break;
        }
    }
}


/* Parse a qualified identifier.  Current lookahead is IDENT.  A
   qualified ident has the form `X<..>::Y<...>::T<...>.  Returns a
   symbol for that class.  */

static struct sym *
parse_classname (void)
{
  struct sym *last_class = NULL;

  while (LOOKING_AT (IDENT))
    {
      last_class = add_sym (yytext, last_class);
      MATCH ();

      if (LOOKING_AT ('<'))
        {
          skip_matching ();
          SET_FLAG (last_class->flags, F_TEMPLATE);
        }

      if (!LOOKING_AT (DCOLON))
        break;

      MATCH ();
    }

  return last_class;
}


/* Parse an operator name.  Add the `static' flag to *SC if an
   implicitly static operator has been parsed.  Value is a pointer to
   a static buffer holding the constructed operator name string.  */

static char *
operator_name (int *sc)
{
  static size_t id_size = 0;
  static char *id = NULL;
  const char *s;
  size_t len;

  MATCH ();

  if (LOOKING_AT2 (NEW, DELETE))
    {
      /* `new' and `delete' are implicitly static.  */
      if (*sc != SC_FRIEND)
        *sc = SC_STATIC;

      s = token_string (LA1);
      MATCH ();

      len = strlen (s) + 10;
      if (len > id_size)
	{
	  size_t new_size = max (len, 2 * id_size);
	  id = (char *) xrealloc (id, new_size);
	  id_size = new_size;
	}
      strcpy (id, s);

      /* Vector new or delete?  */
      if (LOOKING_AT ('['))
	{
	  strcat (id, "[");
	  MATCH ();

	  if (LOOKING_AT (']'))
	    {
	      strcat (id, "]");
	      MATCH ();
	    }
	}
    }
  else
    {
      size_t tokens_matched = 0;

      len = 20;
      if (len > id_size)
	{
	  int new_size = max (len, 2 * id_size);
	  id = (char *) xrealloc (id, new_size);
	  id_size = new_size;
	}
      strcpy (id, "operator");

      /* Beware access declarations of the form "X::f;" Beware of
	 `operator () ()'.  Yet another difficulty is found in
	 GCC 2.95's STL: `operator == __STL_NULL_TMPL_ARGS (...'.  */
      while (!(LOOKING_AT ('(') && tokens_matched)
	     && !LOOKING_AT2 (';', YYEOF))
        {
	  s = token_string (LA1);
	  len += strlen (s) + 2;
	  if (len > id_size)
	    {
	      size_t new_size = max (len, 2 * id_size);
	      id = (char *) xrealloc (id, new_size);
	      id_size = new_size;
	    }

	  if (*s != ')' && *s != ']')
	    strcat (id, " ");
          strcat (id, s);
          MATCH ();

	  /* If this is a simple operator like `+', stop now.  */
	  if (!isalpha ((unsigned char) *s) && *s != '(' && *s != '[')
	    break;

	  ++tokens_matched;
        }
    }

  return id;
}


/* This one consumes the last IDENT of a qualified member name like
   `X::Y::z'.  This IDENT is returned in LAST_ID.  Value is the
   symbol structure for the ident.  */

static struct sym *
parse_qualified_ident_or_type (char **last_id)
{
  struct sym *cls = NULL;
  char *id = NULL;
  size_t id_size = 0;
  int enter = 0;

  while (LOOKING_AT (IDENT))
    {
      int len = strlen (yytext) + 1;
      if (len > id_size)
	{
	  id = (char *) xrealloc (id, len);
	  id_size = len;
	}
      strcpy (id, yytext);
      *last_id = id;
      MATCH ();

      SKIP_MATCHING_IF ('<');

      if (LOOKING_AT (DCOLON))
	{
	  struct sym *pcn = NULL;
	  struct link *pna = check_namespace_alias (id);
	  if (pna)
	    {
	      do
		{
		  enter_namespace (pna->sym->name);
		  enter++;
		  pna = pna->next;
		}
	      while (pna);
	    }
	  else if ((pcn = check_namespace (id, current_namespace)))
	    {
	      enter_namespace (pcn->name);
	      enter++;
	    }
	  else
	    cls = add_sym (id, cls);

	  *last_id = NULL;
	  free (id);
	  id = NULL;
	  id_size = 0;
	  MATCH ();
	}
      else
	break;
    }

  while (enter--)
    leave_namespace ();

  return cls;
}


/* This one consumes the last IDENT of a qualified member name like
   `X::Y::z'.  This IDENT is returned in LAST_ID.  Value is the
   symbol structure for the ident.  */

static void
parse_qualified_param_ident_or_type (char **last_id)
{
  struct sym *cls = NULL;
  static char *id = NULL;
  static int id_size = 0;

  assert (LOOKING_AT (IDENT));

  do
    {
      int len = strlen (yytext) + 1;
      if (len > id_size)
	{
	  id = (char *) xrealloc (id, len);
	  id_size = len;
	}
      strcpy (id, yytext);
      *last_id = id;
      MATCH ();

      SKIP_MATCHING_IF ('<');

      if (LOOKING_AT (DCOLON))
	{
	  cls = add_sym (id, cls);
	  *last_id = NULL;
	  MATCH ();
	}
      else
	break;
    }
  while (LOOKING_AT (IDENT));
}


/* Parse a class definition.

   CONTAINING is the class containing the class being parsed or null.
   This may also be null if NESTED != 0 if the containing class is
   anonymous.  TAG is the tag of the class (struct, union, class).
   NESTED is non-zero if we are parsing a nested class.

   Current lookahead is the class name.  */

static void
class_definition (struct sym *containing, int tag, int flags, int nested)
{
  struct sym *current;
  struct sym *base_class;

  /* Set CURRENT to null if no entry has to be made for the class
     parsed.  This is the case for certain command line flag
     settings.  */
  if ((tag != CLASS && !f_structs) || (nested && !f_nested_classes))
    current = NULL;
  else
    {
      current = add_sym (yytext, containing);
      current->pos = BUFFER_POS ();
      current->regexp = matching_regexp ();
      current->filename = filename;
      current->flags = flags;
    }

  /* If at ':', base class list follows.  */
  if (LOOKING_AT (':'))
    {
      int done = 0;
      MATCH ();

      while (!done)
        {
          switch (LA1)
            {
            case VIRTUAL: case PUBLIC: case PROTECTED: case PRIVATE:
              MATCH ();
              break;

            case IDENT:
              base_class = parse_classname ();
              if (base_class && current && base_class != current)
                add_link (base_class, current);
              break;

              /* The `,' between base classes or the end of the base
                 class list.  Add the previously found base class.
                 It's done this way to skip over sequences of
                 `A::B::C' until we reach the end.

                 FIXME: it is now possible to handle `class X : public B::X'
                 because we have enough information.  */
            case ',':
              MATCH ();
              break;

            default:
              /* A syntax error, possibly due to preprocessor constructs
                 like

                 #ifdef SOMETHING
                 class A : public B
                 #else
                 class A : private B.

                 MATCH until we see something like `;' or `{'.  */
              while (!LOOKING_AT3 (';', YYEOF, '{'))
                MATCH ();
	      done = 1;

            case '{':
              done = 1;
	      break;
            }
        }
    }

  /* Parse the class body if there is one.  */
  if (LOOKING_AT ('{'))
    {
      if (tag != CLASS && !f_structs)
        skip_matching ();
      else
        {
          MATCH ();
          class_body (current, tag);

          if (LOOKING_AT ('}'))
            {
              MATCH ();
              if (LOOKING_AT (';') && !nested)
                MATCH ();
            }
        }
    }
}

/* Add to class *CLS information for the declaration of variable or
   type *ID.  If *CLS is null, this means a global declaration.  SC is
   the storage class of *ID.  FLAGS is a bit set giving additional
   information about the member (see the F_* defines).  */

static void
add_declarator (struct sym **cls, char **id, int flags, int sc)
{
  if (LOOKING_AT2 (';', ','))
    {
      /* The end of a member variable or of an access declaration
         `X::f'.  To distinguish between them we have to know whether
         type information has been seen.  */
      if (*id)
        {
          char *regexp = matching_regexp ();
          int pos = BUFFER_POS ();

          if (*cls)
	    add_member_defn (*cls, *id, regexp, pos, 0, 1, SC_UNKNOWN, flags);
          else
            add_global_defn (*id, regexp, pos, 0, 1, sc, flags);
        }

      MATCH ();
      print_info ();
    }
  else if (LOOKING_AT ('{'))
    {
      if (sc == SC_TYPE && *id)
        {
          /* A named enumeration.  */
          char *regexp = matching_regexp ();
          int pos = BUFFER_POS ();
          add_global_defn (*id, regexp, pos, 0, 1, sc, flags);
        }

      skip_matching ();
      print_info ();
    }

  free (*id);
  *id = NULL;
  *cls = NULL;
}

/* Parse a declaration.  */

static void
declaration (int flags)
{
  char *id = NULL;
  struct sym *cls = NULL;
  char *regexp = NULL;
  int pos = 0;
  unsigned hash = 0;
  int is_constructor;
  int sc = 0;

  while (!LOOKING_AT3 (';', '{', YYEOF))
    {
      switch (LA1)
        {
        default:
          MATCH ();
          break;

        case '[':
          skip_matching ();
          break;

        case ENUM:
        case TYPEDEF:
          sc = SC_TYPE;
          MATCH ();
          break;

        case STATIC:
          sc = SC_STATIC;
          MATCH ();
          break;

        case INT:       case CHAR:      case LONG:      case UNSIGNED:
        case SIGNED:    case CONST:     case DOUBLE:    case VOID:
        case SHORT:     case VOLATILE:  case BOOL:      case WCHAR:
          MATCH ();
          break;

        case CLASS: case STRUCT: case UNION:
          /* This is for the case `STARTWRAP class X : ...' or
             `declare (X, Y)\n class A : ...'.  */
          if (id)
	    {
	      free (id);
	      return;
	    }

        case '=':
          /* Assumed to be the start of an initialization in this
	     context.  */
	  skip_initializer ();
          break;

	case ',':
	  add_declarator (&cls, &id, flags, sc);
	  break;

        case OPERATOR:
	  {
	    char *s = operator_name (&sc);
	    id = (char *) xrealloc (id, strlen (s) + 1);
	    strcpy (id, s);
	  }
          break;

        case T_INLINE:
          SET_FLAG (flags, F_INLINE);
          MATCH ();
          break;

        case '~':
	  MATCH ();
	  if (LOOKING_AT (IDENT))
	    {
	      id = (char *) xrealloc (id, strlen (yytext) + 2);
	      *id = '~';
	      strcpy (id + 1, yytext);
	      MATCH ();
	    }
          break;

        case IDENT:
	  cls = parse_qualified_ident_or_type (&id);
          break;

        case '(':
          /* Most probably the beginning of a parameter list.  */
          if (cls)
            {
              MATCH ();

              if (id && cls)
                {
                  if (!(is_constructor = streq (id, cls->name)))
                    regexp = matching_regexp ();
                }
              else
                is_constructor = 0;

              pos = BUFFER_POS ();
              hash = parm_list (&flags);

              if (is_constructor)
                regexp = matching_regexp ();

              if (id && cls)
		add_member_defn (cls, id, regexp, pos, hash, 0,
				 SC_UNKNOWN, flags);
            }
          else
            {
              /* This may be a C functions, but also a macro
                 call of the form `declare (A, B)' --- such macros
                 can be found in some class libraries.  */
              MATCH ();

              if (id)
                {
                  regexp = matching_regexp ();
                  pos = BUFFER_POS ();
                  hash = parm_list (&flags);
                  add_global_decl (id, regexp, pos, hash, 0, sc, flags);
                }

              /* This is for the case that the function really is
                 a macro with no `;' following it.  If a CLASS directly
                 follows, we would miss it otherwise.  */
              if (LOOKING_AT3 (CLASS, STRUCT, UNION))
                return;
            }

          while (!LOOKING_AT3 (';', '{', YYEOF))
            MATCH ();

          if (!cls && id && LOOKING_AT ('{'))
	    add_global_defn (id, regexp, pos, hash, 0, sc, flags);

	  free (id);
          id = NULL;
          break;
        }
    }

  add_declarator (&cls, &id, flags, sc);
}


/* Parse a list of top-level declarations/definitions.  START_FLAGS
   says in which context we are parsing.  If it is F_EXTERNC, we are
   parsing in an `extern "C"' block.  Value is 1 if EOF is reached, 0
   otherwise.  */

static int
globals (int start_flags)
{
  int anonymous;
  int class_tk;
  int flags = start_flags;

  for (;;)
    {
      char *prev_in = in;

      switch (LA1)
        {
        case NAMESPACE:
          {
            MATCH ();

            if (LOOKING_AT (IDENT))
              {
                char *namespace_name = xstrdup (yytext);
                MATCH ();

                if (LOOKING_AT ('='))
                  {
		    struct link *qna = match_qualified_namespace_alias ();
		    if (qna)
                      register_namespace_alias (namespace_name, qna);

                    if (skip_to (';') == ';')
                      MATCH ();
                  }
                else if (LOOKING_AT ('{'))
                  {
                    MATCH ();
                    enter_namespace (namespace_name);
                    globals (0);
                    leave_namespace ();
                    MATCH_IF ('}');
                  }

		free (namespace_name);
              }
          }
          break;

        case EXTERN:
          MATCH ();
          if (LOOKING_AT (CSTRING) && *string_start == 'C'
              && *(string_start + 1) == '"')
            {
              /* This is `extern "C"'.  */
              MATCH ();

              if (LOOKING_AT ('{'))
                {
                  MATCH ();
                  globals (F_EXTERNC);
                  MATCH_IF ('}');
                }
              else
                SET_FLAG (flags, F_EXTERNC);
            }
          break;

        case TEMPLATE:
          MATCH ();
          SKIP_MATCHING_IF ('<');
          SET_FLAG (flags, F_TEMPLATE);
          break;

        case CLASS: case STRUCT: case UNION:
          class_tk = LA1;
          MATCH ();
          anonymous = 1;

          /* More than one ident here to allow for MS-DOS and OS/2
             specialties like `far', `_Export' etc.  Some C++ libs
             have constructs like `_OS_DLLIMPORT(_OS_CLIENT)' in front
             of the class name.  */
	  while (!LOOKING_AT4 (YYEOF, ';', ':', '{'))
	    {
	      if (LOOKING_AT (IDENT))
		anonymous = 0;
	      MATCH ();
	    }

          /* Don't add anonymous unions.  */
          if (LOOKING_AT2 (':', '{') && !anonymous)
              class_definition (NULL, class_tk, flags, 0);
          else
            {
              if (skip_to (';') == ';')
                MATCH ();
            }

          flags = start_flags;
          break;

        case YYEOF:
          return 1;

        case '}':
          return 0;

        default:
          declaration (flags);
          flags = start_flags;
          break;
        }

      if (prev_in == in)
        yyerror ("parse error", NULL);
    }
}


/* Parse the current input file.  */

static void
yyparse (void)
{
  while (globals (0) == 0)
    MATCH_IF ('}');
}



/***********************************************************************
			     Main Program
 ***********************************************************************/

/* Add the list of paths PATH_LIST to the current search path for
   input files.  */

static void
add_search_path (char *path_list)
{
  while (*path_list)
    {
      char *start = path_list;
      struct search_path *p;

      while (*path_list && *path_list != PATH_LIST_SEPARATOR)
        ++path_list;

      p = (struct search_path *) xmalloc (sizeof *p);
      p->path = (char *) xmalloc (path_list - start + 1);
      memcpy (p->path, start, path_list - start);
      p->path[path_list - start] = '\0';
      p->next = NULL;

      if (search_path_tail)
        {
          search_path_tail->next = p;
          search_path_tail = p;
        }
      else
        search_path = search_path_tail = p;

      while (*path_list == PATH_LIST_SEPARATOR)
        ++path_list;
    }
}


/* Open FILE and return a file handle for it, or -1 if FILE cannot be
   opened.  Try to find FILE in search_path first, then try the
   unchanged file name.  */

static FILE *
open_file (char *file)
{
  FILE *fp = NULL;
  static char *buffer;
  static int buffer_size;
  struct search_path *path;
  int flen = strlen (file) + 1;	/* +1 for the slash */

  filename = xstrdup (file);

  for (path = search_path; path && fp == NULL; path = path->next)
    {
      int len = strlen (path->path) + flen;

      if (len + 1 >= buffer_size)
	{
	  buffer_size = max (len + 1, 2 * buffer_size);
	  buffer = (char *) xrealloc (buffer, buffer_size);
	}

      strcpy (buffer, path->path);
      strcat (buffer, "/");
      strcat (buffer, file);
      fp = fopen (buffer, "r");
    }

  /* Try the original file name.  */
  if (fp == NULL)
     fp = fopen (file, "r");

  if (fp == NULL)
    yyerror ("cannot open", NULL);

  return fp;
}


/* Display usage information and exit program.  */

#define USAGE "\
Usage: ebrowse [options] {files}\n\
\n\
  -a, --append                  append output to existing file\n\
  -f, --files=FILES             read input file names from FILE\n\
  -I, --search-path=LIST        set search path for input files\n\
  -m, --min-regexp-length=N     set minimum regexp length to N\n\
  -M, --max-regexp-length=N     set maximum regexp length to N\n\
  -n, --no-nested-classes       exclude nested classes\n\
  -o, --output-file=FILE        set output file name to FILE\n\
  -p, --position-info           print info about position in file\n\
  -s, --no-structs-or-unions    don't record structs or unions\n\
  -v, --verbose                 be verbose\n\
  -V, --very-verbose            be very verbose\n\
  -x, --no-regexps		don't record regular expressions\n\
      --help                    display this help\n\
      --version			display version info\n\
"

static void
usage (int error)
{
  puts (USAGE);
  exit (error ? EXIT_FAILURE : EXIT_SUCCESS);
}


/* Display version and copyright info.  The VERSION macro is set
   from config.h and contains the Emacs version.  */

#ifndef VERSION
# define VERSION "21"
#endif

static void
version (void)
{
  /* Makes it easier to update automatically. */
  char emacs_copyright[] = "Copyright (C) 2012 Free Software Foundation, Inc.";

  printf ("ebrowse %s\n", VERSION);
  puts (emacs_copyright);
  puts ("This program is distributed under the same terms as Emacs.");
  exit (EXIT_SUCCESS);
}


/* Parse one input file FILE, adding classes and members to the symbol
   table.  */

static void
process_file (char *file)
{
  FILE *fp;

  fp = open_file (file);
  if (fp)
    {
      size_t nread, nbytes;

      /* Give a progress indication if needed.  */
      if (f_very_verbose)
        {
          puts (filename);
          fflush (stdout);
        }
      else if (f_verbose)
        {
          putchar ('.');
          fflush (stdout);
        }

      /* Read file to inbuffer.  */
      for (nread = 0;;)
	{
	  if (nread + READ_CHUNK_SIZE >= inbuffer_size)
	    {
	      inbuffer_size = nread + READ_CHUNK_SIZE + 1;
	      inbuffer = (char *) xrealloc (inbuffer, inbuffer_size);
	    }

	  nbytes = fread (inbuffer + nread, 1, READ_CHUNK_SIZE, fp);
	  if (nbytes == 0)
	    break;
	  nread += nbytes;
	}
      inbuffer[nread] = '\0';

      /* Reinitialize scanner and parser for the new input file.  */
      re_init_scanner ();
      re_init_parser ();

      /* Parse it and close the file.  */
      yyparse ();
      fclose (fp);
    }
}


/* Read a line from stream FP and return a pointer to a static buffer
   containing its contents without the terminating newline.  Value
   is null when EOF is reached.  */

static char *
read_line (FILE *fp)
{
  static char *buffer;
  static int buffer_size;
  int i = 0, c;

  while ((c = getc (fp)) != EOF && c != '\n')
    {
      if (i >= buffer_size)
	{
	  buffer_size = max (100, buffer_size * 2);
	  buffer = (char *) xrealloc (buffer, buffer_size);
	}

      buffer[i++] = c;
    }

  if (c == EOF && i == 0)
    return NULL;

  if (i == buffer_size)
    {
      buffer_size = max (100, buffer_size * 2);
      buffer = (char *) xrealloc (buffer, buffer_size);
    }

  buffer[i] = '\0';
  if (i > 0 && buffer[i - 1] == '\r')
    buffer[i - 1] = '\0';
  return buffer;
}


/* Main entry point.  */

int
main (int argc, char **argv)
{
  int i;
  int any_inputfiles = 0;
  static const char *out_filename = DEFAULT_OUTFILE;
  static char **input_filenames = NULL;
  static int input_filenames_size = 0;
  static int n_input_files;

  filename = "command line";
  yyout = stdout;

  while ((i = getopt_long (argc, argv, "af:I:m:M:no:p:svVx",
                           options, NULL)) != EOF)
    {
      switch (i)
        {
	  /* Experimental.  */
	case 'p':
	  info_position = atoi (optarg);
	  break;

        case 'n':
          f_nested_classes = 0;
          break;

        case 'x':
          f_regexps = 0;
          break;

          /* Add the name of a file containing more input files.  */
        case 'f':
	  if (n_input_files == input_filenames_size)
	    {
	      input_filenames_size = max (10, 2 * input_filenames_size);
	      input_filenames = (char **) xrealloc ((void *)input_filenames,
						    input_filenames_size);
	    }
          input_filenames[n_input_files++] = xstrdup (optarg);
          break;

          /* Append new output to output file instead of truncating it.  */
        case 'a':
          f_append = 1;
          break;

          /* Include structs in the output */
        case 's':
          f_structs = 0;
          break;

          /* Be verbose (give a progress indication).  */
        case 'v':
          f_verbose = 1;
          break;

          /* Be very verbose (print file names as they are processed).  */
        case 'V':
          f_verbose = 1;
          f_very_verbose = 1;
          break;

          /* Change the name of the output file.  */
        case 'o':
	  out_filename = optarg;
          break;

          /* Set minimum length for regular expression strings
             when recorded in the output file.  */
        case 'm':
          min_regexp = atoi (optarg);
          break;

          /* Set maximum length for regular expression strings
             when recorded in the output file.  */
        case 'M':
          max_regexp = atoi (optarg);
          break;

          /* Add to search path.  */
        case 'I':
          add_search_path (optarg);
          break;

          /* Display help */
        case -2:
          usage (0);
          break;

	case -3:
	  version ();
	  break;
        }
    }

  /* Call init_scanner after command line flags have been processed to be
     able to add keywords depending on command line (not yet
     implemented).  */
  init_scanner ();
  init_sym ();

  /* Open output file */
  if (*out_filename)
    {
      if (f_append)
	{
	  /* Check that the file to append to exists, and is not
	     empty.  More specifically, it should be a valid file
	     produced by a previous run of ebrowse, but that's too
	     difficult to check.  */
	  FILE *fp;
	  int rc;

	  fp = fopen (out_filename, "r");
	  if (fp == NULL)
	    {
	      yyerror ("file `%s' must exist for --append", out_filename);
	      exit (EXIT_FAILURE);
	    }

	  rc = fseek (fp, 0, SEEK_END);
	  if (rc == -1)
	    {
	      yyerror ("error seeking in file `%s'", out_filename);
	      exit (EXIT_FAILURE);
	    }

	  rc = ftell (fp);
	  if (rc == -1)
	    {
	      yyerror ("error getting size of file `%s'", out_filename);
	      exit (EXIT_FAILURE);
	    }

	  else if (rc == 0)
	    {
	      yyerror ("file `%s' is empty", out_filename);
	      /* It may be ok to use an empty file for appending.
		 exit (EXIT_FAILURE); */
	    }

	  fclose (fp);
	}

      yyout = fopen (out_filename, f_append ? "a" : "w");
      if (yyout == NULL)
	{
	  yyerror ("cannot open output file `%s'", out_filename);
	  exit (EXIT_FAILURE);
	}
    }

  /* Process input files specified on the command line.  */
  while (optind < argc)
    {
      process_file (argv[optind++]);
      any_inputfiles = 1;
    }

  /* Process files given on stdin if no files specified.  */
  if (!any_inputfiles && n_input_files == 0)
    {
      char *file;
      while ((file = read_line (stdin)) != NULL)
	process_file (file);
    }
  else
    {
      /* Process files from `--files=FILE'.  Every line in FILE names
	 one input file to process.  */
      for (i = 0; i < n_input_files; ++i)
        {
          FILE *fp = fopen (input_filenames[i], "r");

          if (fp == NULL)
            yyerror ("cannot open input file `%s'", input_filenames[i]);
          else
            {
	      char *file;
	      while ((file = read_line (fp)) != NULL)
		process_file (file);
              fclose (fp);
            }
        }
    }

  /* Write output file.  */
  dump_roots (yyout);

  /* Close output file.  */
  if (yyout != stdout)
    fclose (yyout);

  return EXIT_SUCCESS;
}

/* ebrowse.c ends here */
