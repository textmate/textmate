/* Manipulation of keymaps
   Copyright (C) 1985-1988, 1993-1995, 1998-2012 Free Software Foundation, Inc.

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

/* Old BUGS:
   - [M-C-a] != [?\M-\C-a]
   - [M-f2] != [?\e f2].
   - (define-key map [menu-bar foo] <bla>) does not always place <bla>
     at the head of the menu (if `foo' was already bound earlier and
     then unbound, for example).
   TODO:
   - allow many more Meta -> ESC mappings (like Hyper -> C-e for Emacspeak)
   - Think about the various defaulting that's currently hard-coded in
     keyboard.c (uppercase->lowercase, char->charset, button-events, ...)
     and make it more generic.  Maybe we should allow mappings of the
     form (PREDICATE . BINDING) as generalization of the default binding,
     tho probably a cleaner way to attack this is to allow functional
     keymaps (i.e. keymaps that are implemented as functions that implement
     a few different methods like `lookup', `map', ...).
   - Make [a] equivalent to [?a].
   BEWARE:
   - map-keymap should work meaningfully even if entries are added/removed
     to the keymap while iterating through it:
       start - removed <= visited <= start + added
 */

#include <config.h>
#include <stdio.h>
#include <setjmp.h>
#include "lisp.h"
#include "commands.h"
#include "buffer.h"
#include "character.h"
#include "charset.h"
#include "keyboard.h"
#include "frame.h"
#include "termhooks.h"
#include "blockinput.h"
#include "puresize.h"
#include "intervals.h"
#include "keymap.h"
#include "window.h"

/* Actually allocate storage for these variables */

Lisp_Object current_global_map;	/* Current global keymap */

Lisp_Object global_map;		/* default global key bindings */

Lisp_Object meta_map;		/* The keymap used for globally bound
				   ESC-prefixed default commands */

Lisp_Object control_x_map;	/* The keymap used for globally bound
				   C-x-prefixed default commands */

				/* The keymap used by the minibuf for local
				   bindings when spaces are allowed in the
				   minibuf */

				/* The keymap used by the minibuf for local
				   bindings when spaces are not encouraged
				   in the minibuf */

/* keymap used for minibuffers when doing completion */
/* keymap used for minibuffers when doing completion and require a match */
static Lisp_Object Qkeymapp, Qnon_ascii;
Lisp_Object Qkeymap, Qmenu_item, Qremap;
static Lisp_Object QCadvertised_binding;

/* Alist of elements like (DEL . "\d").  */
static Lisp_Object exclude_keys;

/* Pre-allocated 2-element vector for Fcommand_remapping to use.  */
static Lisp_Object command_remapping_vector;

/* Hash table used to cache a reverse-map to speed up calls to where-is.  */
static Lisp_Object where_is_cache;
/* Which keymaps are reverse-stored in the cache.  */
static Lisp_Object where_is_cache_keymaps;

static Lisp_Object Flookup_key (Lisp_Object, Lisp_Object, Lisp_Object);
static Lisp_Object store_in_keymap (Lisp_Object, Lisp_Object, Lisp_Object);

static Lisp_Object define_as_prefix (Lisp_Object, Lisp_Object);
static void describe_command (Lisp_Object, Lisp_Object);
static void describe_translation (Lisp_Object, Lisp_Object);
static void describe_map (Lisp_Object, Lisp_Object,
                          void (*) (Lisp_Object, Lisp_Object),
			  int, Lisp_Object, Lisp_Object*, int, int);
static void describe_vector (Lisp_Object, Lisp_Object, Lisp_Object,
                             void (*) (Lisp_Object, Lisp_Object), int,
                             Lisp_Object, Lisp_Object, int, int);
static void silly_event_symbol_error (Lisp_Object);
static Lisp_Object get_keyelt (Lisp_Object, int);

/* Keymap object support - constructors and predicates.			*/

DEFUN ("make-keymap", Fmake_keymap, Smake_keymap, 0, 1, 0,
       doc: /* Construct and return a new keymap, of the form (keymap CHARTABLE . ALIST).
CHARTABLE is a char-table that holds the bindings for all characters
without modifiers.  All entries in it are initially nil, meaning
"command undefined".  ALIST is an assoc-list which holds bindings for
function keys, mouse events, and any other things that appear in the
input stream.  Initially, ALIST is nil.

The optional arg STRING supplies a menu name for the keymap
in case you use it as a menu with `x-popup-menu'.  */)
  (Lisp_Object string)
{
  Lisp_Object tail;
  if (!NILP (string))
    tail = Fcons (string, Qnil);
  else
    tail = Qnil;
  return Fcons (Qkeymap,
		Fcons (Fmake_char_table (Qkeymap, Qnil), tail));
}

DEFUN ("make-sparse-keymap", Fmake_sparse_keymap, Smake_sparse_keymap, 0, 1, 0,
       doc: /* Construct and return a new sparse keymap.
Its car is `keymap' and its cdr is an alist of (CHAR . DEFINITION),
which binds the character CHAR to DEFINITION, or (SYMBOL . DEFINITION),
which binds the function key or mouse event SYMBOL to DEFINITION.
Initially the alist is nil.

The optional arg STRING supplies a menu name for the keymap
in case you use it as a menu with `x-popup-menu'.  */)
  (Lisp_Object string)
{
  if (!NILP (string))
    {
      if (!NILP (Vpurify_flag))
	string = Fpurecopy (string);
      return Fcons (Qkeymap, Fcons (string, Qnil));
    }
  return Fcons (Qkeymap, Qnil);
}

/* This function is used for installing the standard key bindings
   at initialization time.

   For example:

   initial_define_key (control_x_map, Ctl('X'), "exchange-point-and-mark");  */

void
initial_define_key (Lisp_Object keymap, int key, const char *defname)
{
  store_in_keymap (keymap, make_number (key), intern_c_string (defname));
}

void
initial_define_lispy_key (Lisp_Object keymap, const char *keyname, const char *defname)
{
  store_in_keymap (keymap, intern_c_string (keyname), intern_c_string (defname));
}

DEFUN ("keymapp", Fkeymapp, Skeymapp, 1, 1, 0,
       doc: /* Return t if OBJECT is a keymap.

A keymap is a list (keymap . ALIST),
or a symbol whose function definition is itself a keymap.
ALIST elements look like (CHAR . DEFN) or (SYMBOL . DEFN);
a vector of densely packed bindings for small character codes
is also allowed as an element.  */)
  (Lisp_Object object)
{
  return (KEYMAPP (object) ? Qt : Qnil);
}

DEFUN ("keymap-prompt", Fkeymap_prompt, Skeymap_prompt, 1, 1, 0,
       doc: /* Return the prompt-string of a keymap MAP.
If non-nil, the prompt is shown in the echo-area
when reading a key-sequence to be looked-up in this keymap.  */)
  (Lisp_Object map)
{
  map = get_keymap (map, 0, 0);
  while (CONSP (map))
    {
      Lisp_Object tem = XCAR (map);
      if (STRINGP (tem))
	return tem;
      else if (KEYMAPP (tem))
	{
	  tem = Fkeymap_prompt (tem);
	  if (!NILP (tem))
	    return tem;
	}
      map = XCDR (map);
    }
  return Qnil;
}

/* Check that OBJECT is a keymap (after dereferencing through any
   symbols).  If it is, return it.

   If AUTOLOAD is non-zero and OBJECT is a symbol whose function value
   is an autoload form, do the autoload and try again.
   If AUTOLOAD is nonzero, callers must assume GC is possible.

   If the map needs to be autoloaded, but AUTOLOAD is zero (and ERROR
   is zero as well), return Qt.

   ERROR_IF_NOT_KEYMAP controls how we respond if OBJECT isn't a keymap.
   If ERROR_IF_NOT_KEYMAP is non-zero, signal an error; otherwise,
   just return Qnil.

   Note that most of the time, we don't want to pursue autoloads.
   Functions like Faccessible_keymaps which scan entire keymap trees
   shouldn't load every autoloaded keymap.  I'm not sure about this,
   but it seems to me that only read_key_sequence, Flookup_key, and
   Fdefine_key should cause keymaps to be autoloaded.

   This function can GC when AUTOLOAD is non-zero, because it calls
   do_autoload which can GC.  */

Lisp_Object
get_keymap (Lisp_Object object, int error_if_not_keymap, int autoload)
{
  Lisp_Object tem;

 autoload_retry:
  if (NILP (object))
    goto end;
  if (CONSP (object) && EQ (XCAR (object), Qkeymap))
    return object;

  tem = indirect_function (object);
  if (CONSP (tem))
    {
      if (EQ (XCAR (tem), Qkeymap))
	return tem;

      /* Should we do an autoload?  Autoload forms for keymaps have
	 Qkeymap as their fifth element.  */
      if ((autoload || !error_if_not_keymap) && EQ (XCAR (tem), Qautoload)
	  && SYMBOLP (object))
	{
	  Lisp_Object tail;

	  tail = Fnth (make_number (4), tem);
	  if (EQ (tail, Qkeymap))
	    {
	      if (autoload)
		{
		  struct gcpro gcpro1, gcpro2;

		  GCPRO2 (tem, object);
		  do_autoload (tem, object);
		  UNGCPRO;

		  goto autoload_retry;
		}
	      else
	      	return object;
	    }
	}
    }

 end:
  if (error_if_not_keymap)
    wrong_type_argument (Qkeymapp, object);
  return Qnil;
}

/* Return the parent map of KEYMAP, or nil if it has none.
   We assume that KEYMAP is a valid keymap.  */

static Lisp_Object
keymap_parent (Lisp_Object keymap, int autoload)
{
  Lisp_Object list;

  keymap = get_keymap (keymap, 1, autoload);

  /* Skip past the initial element `keymap'.  */
  list = XCDR (keymap);
  for (; CONSP (list); list = XCDR (list))
    {
      /* See if there is another `keymap'.  */
      if (KEYMAPP (list))
	return list;
    }

  return get_keymap (list, 0, autoload);
}

DEFUN ("keymap-parent", Fkeymap_parent, Skeymap_parent, 1, 1, 0,
       doc: /* Return the parent keymap of KEYMAP.
If KEYMAP has no parent, return nil.  */)
  (Lisp_Object keymap)
{
  return keymap_parent (keymap, 1);
}

/* Check whether MAP is one of MAPS parents.  */
static int
keymap_memberp (Lisp_Object map, Lisp_Object maps)
{
  if (NILP (map)) return 0;
  while (KEYMAPP (maps) && !EQ (map, maps))
    maps = keymap_parent (maps, 0);
  return (EQ (map, maps));
}

/* Set the parent keymap of MAP to PARENT.  */

DEFUN ("set-keymap-parent", Fset_keymap_parent, Sset_keymap_parent, 2, 2, 0,
       doc: /* Modify KEYMAP to set its parent map to PARENT.
Return PARENT.  PARENT should be nil or another keymap.  */)
  (Lisp_Object keymap, Lisp_Object parent)
{
  Lisp_Object list, prev;
  struct gcpro gcpro1, gcpro2;

  /* Flush any reverse-map cache.  */
  where_is_cache = Qnil; where_is_cache_keymaps = Qt;

  GCPRO2 (keymap, parent);
  keymap = get_keymap (keymap, 1, 1);

  if (!NILP (parent))
    {
      parent = get_keymap (parent, 1, 0);

      /* Check for cycles.  */
      if (keymap_memberp (keymap, parent))
	error ("Cyclic keymap inheritance");
    }

  /* Skip past the initial element `keymap'.  */
  prev = keymap;
  while (1)
    {
      list = XCDR (prev);
      /* If there is a parent keymap here, replace it.
	 If we came to the end, add the parent in PREV.  */
      if (!CONSP (list) || KEYMAPP (list))
	{
	  CHECK_IMPURE (prev);
	  XSETCDR (prev, parent);
	  RETURN_UNGCPRO (parent);
	}
      prev = list;
    }
}


/* Look up IDX in MAP.  IDX may be any sort of event.
   Note that this does only one level of lookup; IDX must be a single
   event, not a sequence.

   MAP must be a keymap or a list of keymaps.

   If T_OK is non-zero, bindings for Qt are treated as default
   bindings; any key left unmentioned by other tables and bindings is
   given the binding of Qt.

   If T_OK is zero, bindings for Qt are not treated specially.

   If NOINHERIT, don't accept a subkeymap found in an inherited keymap.

   Returns Qunbound if no binding was found (and returns Qnil if a nil
   binding was found).  */

static Lisp_Object
access_keymap_1 (Lisp_Object map, Lisp_Object idx, int t_ok, int noinherit, int autoload)
{
  /* If idx is a list (some sort of mouse click, perhaps?),
     the index we want to use is the car of the list, which
     ought to be a symbol.  */
  idx = EVENT_HEAD (idx);

  /* If idx is a symbol, it might have modifiers, which need to
     be put in the canonical order.  */
  if (SYMBOLP (idx))
    idx = reorder_modifiers (idx);
  else if (INTEGERP (idx))
    /* Clobber the high bits that can be present on a machine
       with more than 24 bits of integer.  */
    XSETFASTINT (idx, XINT (idx) & (CHAR_META | (CHAR_META - 1)));

  /* Handle the special meta -> esc mapping.  */
  if (INTEGERP (idx) && XFASTINT (idx) & meta_modifier)
    {
      /* See if there is a meta-map.  If there's none, there is
         no binding for IDX, unless a default binding exists in MAP.  */
      struct gcpro gcpro1;
      Lisp_Object event_meta_binding, event_meta_map;
      GCPRO1 (map);
      /* A strange value in which Meta is set would cause
	 infinite recursion.  Protect against that.  */
      if (XINT (meta_prefix_char) & CHAR_META)
	meta_prefix_char = make_number (27);
      event_meta_binding = access_keymap_1 (map, meta_prefix_char, t_ok,
					    noinherit, autoload);
      event_meta_map = get_keymap (event_meta_binding, 0, autoload);
      UNGCPRO;
      if (CONSP (event_meta_map))
	{
	  map = event_meta_map;
	  idx = make_number (XFASTINT (idx) & ~meta_modifier);
	}
      else if (t_ok)
	/* Set IDX to t, so that we only find a default binding.  */
	idx = Qt;
      else
	/* An explicit nil binding, or no binding at all.  */
	return NILP (event_meta_binding) ? Qnil : Qunbound;
    }

  /* t_binding is where we put a default binding that applies,
     to use in case we do not find a binding specifically
     for this key sequence.  */
  {
    Lisp_Object tail;
    Lisp_Object t_binding = Qunbound;
    Lisp_Object retval = Qunbound;
    Lisp_Object retval_tail = Qnil;
    struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;

    GCPRO4 (tail, idx, t_binding, retval);

    for (tail = (CONSP (map) && EQ (Qkeymap, XCAR (map))) ? XCDR (map) : map;
	 (CONSP (tail)
	  || (tail = get_keymap (tail, 0, autoload), CONSP (tail)));
	 tail = XCDR (tail))
      {
	/* Qunbound in VAL means we have found no binding.  */
	Lisp_Object val = Qunbound;
	Lisp_Object binding = XCAR (tail);
	Lisp_Object submap = get_keymap (binding, 0, autoload);

	if (EQ (binding, Qkeymap))
	  {
	    if (noinherit || NILP (retval))
	      /* If NOINHERIT, stop here, the rest is inherited.  */
	      break;
	    else if (!EQ (retval, Qunbound))
	      {
		Lisp_Object parent_entry;
		eassert (KEYMAPP (retval));
		parent_entry
		  = get_keymap (access_keymap_1 (tail, idx,
						 t_ok, 0, autoload),
				0, autoload);
		if (KEYMAPP (parent_entry))
		  {
		    if (CONSP (retval_tail))
		      XSETCDR (retval_tail, parent_entry);
		    else
		      {
			retval_tail = Fcons (retval, parent_entry);
			retval = Fcons (Qkeymap, retval_tail);
		      }
		  }
		break;
	      }
	  }
	else if (CONSP (submap))
	  {
	    val = access_keymap_1 (submap, idx, t_ok, noinherit, autoload);
	  }
	else if (CONSP (binding))
	  {
	    Lisp_Object key = XCAR (binding);

	    if (EQ (key, idx))
	      val = XCDR (binding);
	    else if (t_ok && EQ (key, Qt))
	      {
		t_binding = XCDR (binding);
		t_ok = 0;
	      }
	  }
	else if (VECTORP (binding))
	  {
	    if (INTEGERP (idx) && XFASTINT (idx) < ASIZE (binding))
	      val = AREF (binding, XFASTINT (idx));
	  }
	else if (CHAR_TABLE_P (binding))
	  {
	    /* Character codes with modifiers
	       are not included in a char-table.
	       All character codes without modifiers are included.  */
	    if (INTEGERP (idx) && (XFASTINT (idx) & CHAR_MODIFIER_MASK) == 0)
	      {
		val = Faref (binding, idx);
		/* `nil' has a special meaning for char-tables, so
		   we use something else to record an explicitly
		   unbound entry.  */
		if (NILP (val))
		  val = Qunbound;
	      }
	  }

	/* If we found a binding, clean it up and return it.  */
	if (!EQ (val, Qunbound))
	  {
	    if (EQ (val, Qt))
	      /* A Qt binding is just like an explicit nil binding
		 (i.e. it shadows any parent binding but not bindings in
		 keymaps of lower precedence).  */
	      val = Qnil;

	    val = get_keyelt (val, autoload);

	    if (!KEYMAPP (val))
	      {
		if (NILP (retval) || EQ (retval, Qunbound))
		  retval = val;
		if (!NILP (val))
		  break;  /* Shadows everything that follows.  */
	      }
	    else if (NILP (retval) || EQ (retval, Qunbound))
	      retval = val;
	    else if (CONSP (retval_tail))
	      {
		XSETCDR (retval_tail, Fcons (val, Qnil));
		retval_tail = XCDR (retval_tail);
	      }
	    else
	      {
		retval_tail = Fcons (val, Qnil);
		retval = Fcons (Qkeymap, Fcons (retval, retval_tail));
	      }
	  }
	QUIT;
      }
    UNGCPRO;
    return EQ (Qunbound, retval) ? get_keyelt (t_binding, autoload) : retval;
  }
}

Lisp_Object
access_keymap (Lisp_Object map, Lisp_Object idx,
	       int t_ok, int noinherit, int autoload)
{
  Lisp_Object val = access_keymap_1 (map, idx, t_ok, noinherit, autoload);
  return EQ (val, Qunbound) ? Qnil : val;
}

static void
map_keymap_item (map_keymap_function_t fun, Lisp_Object args, Lisp_Object key, Lisp_Object val, void *data)
{
  if (EQ (val, Qt))
    val = Qnil;
  (*fun) (key, val, args, data);
}

static void
map_keymap_char_table_item (Lisp_Object args, Lisp_Object key, Lisp_Object val)
{
  if (!NILP (val))
    {
      map_keymap_function_t fun
	= (map_keymap_function_t) XSAVE_VALUE (XCAR (args))->pointer;
      args = XCDR (args);
      /* If the key is a range, make a copy since map_char_table modifies
	 it in place.  */
      if (CONSP (key))
	key = Fcons (XCAR (key), XCDR (key));
      map_keymap_item (fun, XCDR (args), key, val,
		       XSAVE_VALUE (XCAR (args))->pointer);
    }
}

/* Call FUN for every binding in MAP and stop at (and return) the parent.
   FUN is called with 4 arguments: FUN (KEY, BINDING, ARGS, DATA).  */
static Lisp_Object
map_keymap_internal (Lisp_Object map,
		     map_keymap_function_t fun,
		     Lisp_Object args,
		     void *data)
{
  struct gcpro gcpro1, gcpro2, gcpro3;
  Lisp_Object tail
    = (CONSP (map) && EQ (Qkeymap, XCAR (map))) ? XCDR (map) : map;

  GCPRO3 (map, args, tail);
  for (; CONSP (tail) && !EQ (Qkeymap, XCAR (tail)); tail = XCDR (tail))
    {
      Lisp_Object binding = XCAR (tail);

      if (KEYMAPP (binding))	/* An embedded parent.  */
	break;
      else if (CONSP (binding))
	map_keymap_item (fun, args, XCAR (binding), XCDR (binding), data);
      else if (VECTORP (binding))
	{
	  /* Loop over the char values represented in the vector.  */
	  int len = ASIZE (binding);
	  int c;
	  for (c = 0; c < len; c++)
	    {
	      Lisp_Object character;
	      XSETFASTINT (character, c);
	      map_keymap_item (fun, args, character, AREF (binding, c), data);
	    }
	}
      else if (CHAR_TABLE_P (binding))
	{
	  map_char_table (map_keymap_char_table_item, Qnil, binding,
			  Fcons (make_save_value ((void *) fun, 0),
				 Fcons (make_save_value (data, 0),
					args)));
	}
    }
  UNGCPRO;
  return tail;
}

static void
map_keymap_call (Lisp_Object key, Lisp_Object val, Lisp_Object fun, void *dummy)
{
  call2 (fun, key, val);
}

/* Same as map_keymap_internal, but traverses parent keymaps as well.
   A non-zero AUTOLOAD indicates that autoloaded keymaps should be loaded.  */
void
map_keymap (Lisp_Object map, map_keymap_function_t fun, Lisp_Object args, void *data, int autoload)
{
  struct gcpro gcpro1;
  GCPRO1 (args);
  map = get_keymap (map, 1, autoload);
  while (CONSP (map))
    {
      if (KEYMAPP (XCAR (map)))
	{
	  map_keymap (XCAR (map), fun, args, data, autoload);
	  map = XCDR (map);
	}
      else
	map = map_keymap_internal (map, fun, args, data);
      if (!CONSP (map))
	map = get_keymap (map, 0, autoload);
    }
  UNGCPRO;
}

static Lisp_Object Qkeymap_canonicalize;

/* Same as map_keymap, but does it right, properly eliminating duplicate
   bindings due to inheritance.   */
void
map_keymap_canonical (Lisp_Object map, map_keymap_function_t fun, Lisp_Object args, void *data)
{
  struct gcpro gcpro1;
  GCPRO1 (args);
  /* map_keymap_canonical may be used from redisplay (e.g. when building menus)
     so be careful to ignore errors and to inhibit redisplay.  */
  map = safe_call1 (Qkeymap_canonicalize, map);
  /* No need to use `map_keymap' here because canonical map has no parent.  */
  map_keymap_internal (map, fun, args, data);
  UNGCPRO;
}

DEFUN ("map-keymap-internal", Fmap_keymap_internal, Smap_keymap_internal, 2, 2, 0,
       doc: /* Call FUNCTION once for each event binding in KEYMAP.
FUNCTION is called with two arguments: the event that is bound, and
the definition it is bound to.  The event may be a character range.
If KEYMAP has a parent, this function returns it without processing it.  */)
  (Lisp_Object function, Lisp_Object keymap)
{
  struct gcpro gcpro1;
  GCPRO1 (function);
  keymap = get_keymap (keymap, 1, 1);
  keymap = map_keymap_internal (keymap, map_keymap_call, function, NULL);
  UNGCPRO;
  return keymap;
}

DEFUN ("map-keymap", Fmap_keymap, Smap_keymap, 2, 3, 0,
       doc: /* Call FUNCTION once for each event binding in KEYMAP.
FUNCTION is called with two arguments: the event that is bound, and
the definition it is bound to.  The event may be a character range.

If KEYMAP has a parent, the parent's bindings are included as well.
This works recursively: if the parent has itself a parent, then the
grandparent's bindings are also included and so on.
usage: (map-keymap FUNCTION KEYMAP)  */)
  (Lisp_Object function, Lisp_Object keymap, Lisp_Object sort_first)
{
  if (! NILP (sort_first))
    return call2 (intern ("map-keymap-sorted"), function, keymap);

  map_keymap (keymap, map_keymap_call, function, NULL, 1);
  return Qnil;
}

/* Given OBJECT which was found in a slot in a keymap,
   trace indirect definitions to get the actual definition of that slot.
   An indirect definition is a list of the form
   (KEYMAP . INDEX), where KEYMAP is a keymap or a symbol defined as one
   and INDEX is the object to look up in KEYMAP to yield the definition.

   Also if OBJECT has a menu string as the first element,
   remove that.  Also remove a menu help string as second element.

   If AUTOLOAD is nonzero, load autoloadable keymaps
   that are referred to with indirection.

   This can GC because menu_item_eval_property calls Feval.  */

static Lisp_Object
get_keyelt (Lisp_Object object, int autoload)
{
  while (1)
    {
      if (!(CONSP (object)))
	/* This is really the value.  */
	return object;

      /* If the keymap contents looks like (keymap ...) or (lambda ...)
	 then use itself. */
      else if (EQ (XCAR (object), Qkeymap) || EQ (XCAR (object), Qlambda))
	return object;

      /* If the keymap contents looks like (menu-item name . DEFN)
	 or (menu-item name DEFN ...) then use DEFN.
	 This is a new format menu item.  */
      else if (EQ (XCAR (object), Qmenu_item))
	{
	  if (CONSP (XCDR (object)))
	    {
	      Lisp_Object tem;

	      object = XCDR (XCDR (object));
	      tem = object;
	      if (CONSP (object))
		object = XCAR (object);

	      /* If there's a `:filter FILTER', apply FILTER to the
		 menu-item's definition to get the real definition to
		 use.  */
	      for (; CONSP (tem) && CONSP (XCDR (tem)); tem = XCDR (tem))
		if (EQ (XCAR (tem), QCfilter) && autoload)
		  {
		    Lisp_Object filter;
		    filter = XCAR (XCDR (tem));
		    filter = list2 (filter, list2 (Qquote, object));
		    object = menu_item_eval_property (filter);
		    break;
		  }
	    }
	  else
	    /* Invalid keymap.  */
	    return object;
	}

      /* If the keymap contents looks like (STRING . DEFN), use DEFN.
	 Keymap alist elements like (CHAR MENUSTRING . DEFN)
	 will be used by HierarKey menus.  */
      else if (STRINGP (XCAR (object)))
	{
	  object = XCDR (object);
	  /* Also remove a menu help string, if any,
	     following the menu item name.  */
	  if (CONSP (object) && STRINGP (XCAR (object)))
	    object = XCDR (object);
	  /* Also remove the sublist that caches key equivalences, if any.  */
	  if (CONSP (object) && CONSP (XCAR (object)))
	    {
	      Lisp_Object carcar;
	      carcar = XCAR (XCAR (object));
	      if (NILP (carcar) || VECTORP (carcar))
		object = XCDR (object);
	    }
	}

      /* If the contents are (KEYMAP . ELEMENT), go indirect.  */
      else if (KEYMAPP (XCAR (object)))
	error ("Wow, indirect keymap entry!!");
      else
	return object;
    }
}

static Lisp_Object
store_in_keymap (Lisp_Object keymap, register Lisp_Object idx, Lisp_Object def)
{
  /* Flush any reverse-map cache.  */
  where_is_cache = Qnil;
  where_is_cache_keymaps = Qt;

  if (EQ (idx, Qkeymap))
    error ("`keymap' is reserved for embedded parent maps");

  /* If we are preparing to dump, and DEF is a menu element
     with a menu item indicator, copy it to ensure it is not pure.  */
  if (CONSP (def) && PURE_P (def)
      && (EQ (XCAR (def), Qmenu_item) || STRINGP (XCAR (def))))
    def = Fcons (XCAR (def), XCDR (def));

  if (!CONSP (keymap) || !EQ (XCAR (keymap), Qkeymap))
    error ("attempt to define a key in a non-keymap");

  /* If idx is a cons, and the car part is a character, idx must be of
     the form (FROM-CHAR . TO-CHAR).  */
  if (CONSP (idx) && CHARACTERP (XCAR (idx)))
    CHECK_CHARACTER_CDR (idx);
  else
    /* If idx is a list (some sort of mouse click, perhaps?),
       the index we want to use is the car of the list, which
       ought to be a symbol.  */
    idx = EVENT_HEAD (idx);

  /* If idx is a symbol, it might have modifiers, which need to
     be put in the canonical order.  */
  if (SYMBOLP (idx))
    idx = reorder_modifiers (idx);
  else if (INTEGERP (idx))
    /* Clobber the high bits that can be present on a machine
       with more than 24 bits of integer.  */
    XSETFASTINT (idx, XINT (idx) & (CHAR_META | (CHAR_META - 1)));

  /* Scan the keymap for a binding of idx.  */
  {
    Lisp_Object tail;

    /* The cons after which we should insert new bindings.  If the
       keymap has a table element, we record its position here, so new
       bindings will go after it; this way, the table will stay
       towards the front of the alist and character lookups in dense
       keymaps will remain fast.  Otherwise, this just points at the
       front of the keymap.  */
    Lisp_Object insertion_point;

    insertion_point = keymap;
    for (tail = XCDR (keymap); CONSP (tail); tail = XCDR (tail))
      {
	Lisp_Object elt;

	elt = XCAR (tail);
	if (VECTORP (elt))
	  {
	    if (NATNUMP (idx) && XFASTINT (idx) < ASIZE (elt))
	      {
		CHECK_IMPURE (elt);
		ASET (elt, XFASTINT (idx), def);
		return def;
	      }
	    else if (CONSP (idx) && CHARACTERP (XCAR (idx)))
	      {
		int from = XFASTINT (XCAR (idx));
		int to = XFASTINT (XCDR (idx));

		if (to >= ASIZE (elt))
		  to = ASIZE (elt) - 1;
		for (; from <= to; from++)
		  ASET (elt, from, def);
		if (to == XFASTINT (XCDR (idx)))
		  /* We have defined all keys in IDX.  */
		  return def;
	      }
	    insertion_point = tail;
	  }
	else if (CHAR_TABLE_P (elt))
	  {
	    /* Character codes with modifiers
	       are not included in a char-table.
	       All character codes without modifiers are included.  */
	    if (NATNUMP (idx) && !(XFASTINT (idx) & CHAR_MODIFIER_MASK))
	      {
		Faset (elt, idx,
		       /* `nil' has a special meaning for char-tables, so
			  we use something else to record an explicitly
			  unbound entry.  */
		       NILP (def) ? Qt : def);
		return def;
	      }
	    else if (CONSP (idx) && CHARACTERP (XCAR (idx)))
	      {
		Fset_char_table_range (elt, idx, NILP (def) ? Qt : def);
		return def;
	      }
	    insertion_point = tail;
	  }
	else if (CONSP (elt))
	  {
	    if (EQ (Qkeymap, XCAR (elt)))
	      { /* A sub keymap.  This might be due to a lookup that found
		   two matching bindings (maybe because of a sub keymap).
		   It almost never happens (since the second binding normally
		   only happens in the inherited part of the keymap), but
		   if it does, we want to update the sub-keymap since the
		   main one might be temporary (built by access_keymap).  */
		tail = insertion_point = elt;
	      }
	    else if (EQ (idx, XCAR (elt)))
	      {
		CHECK_IMPURE (elt);
		XSETCDR (elt, def);
		return def;
	      }
	    else if (CONSP (idx) && CHARACTERP (XCAR (idx)))
	      {
		int from = XFASTINT (XCAR (idx));
		int to = XFASTINT (XCDR (idx));

		if (from <= XFASTINT (XCAR (elt))
		    && to >= XFASTINT (XCAR (elt)))
		  {
		    XSETCDR (elt, def);
		    if (from == to)
		      return def;
		  }
	      }
	  }
	else if (EQ (elt, Qkeymap))
	  /* If we find a 'keymap' symbol in the spine of KEYMAP,
	     then we must have found the start of a second keymap
	     being used as the tail of KEYMAP, and a binding for IDX
	     should be inserted before it.  */
	  goto keymap_end;

	QUIT;
      }

  keymap_end:
    /* We have scanned the entire keymap, and not found a binding for
       IDX.  Let's add one.  */
    {
      Lisp_Object elt;

      if (CONSP (idx) && CHARACTERP (XCAR (idx)))
	{
	  /* IDX specifies a range of characters, and not all of them
	     were handled yet, which means this keymap doesn't have a
	     char-table.  So, we insert a char-table now.  */
	  elt = Fmake_char_table (Qkeymap, Qnil);
	  Fset_char_table_range (elt, idx, NILP (def) ? Qt : def);
	}
      else
	elt = Fcons (idx, def);
      CHECK_IMPURE (insertion_point);
      XSETCDR (insertion_point, Fcons (elt, XCDR (insertion_point)));
    }
  }

  return def;
}

static Lisp_Object Fcopy_keymap (Lisp_Object);

static Lisp_Object
copy_keymap_item (Lisp_Object elt)
{
  Lisp_Object res, tem;

  if (!CONSP (elt))
    return elt;

  res = tem = elt;

  /* Is this a new format menu item.  */
  if (EQ (XCAR (tem), Qmenu_item))
    {
      /* Copy cell with menu-item marker.  */
      res = elt = Fcons (XCAR (tem), XCDR (tem));
      tem = XCDR (elt);
      if (CONSP (tem))
	{
	  /* Copy cell with menu-item name.  */
	  XSETCDR (elt, Fcons (XCAR (tem), XCDR (tem)));
	  elt = XCDR (elt);
	  tem = XCDR (elt);
	}
      if (CONSP (tem))
	{
	  /* Copy cell with binding and if the binding is a keymap,
	     copy that.  */
	  XSETCDR (elt, Fcons (XCAR (tem), XCDR (tem)));
	  elt = XCDR (elt);
	  tem = XCAR (elt);
	  if (CONSP (tem) && EQ (XCAR (tem), Qkeymap))
	    XSETCAR (elt, Fcopy_keymap (tem));
	  tem = XCDR (elt);
	  if (CONSP (tem) && CONSP (XCAR (tem)))
	    /* Delete cache for key equivalences.  */
	    XSETCDR (elt, XCDR (tem));
	}
    }
  else
    {
      /* It may be an old format menu item.
	 Skip the optional menu string.  */
      if (STRINGP (XCAR (tem)))
	{
	  /* Copy the cell, since copy-alist didn't go this deep.  */
	  res = elt = Fcons (XCAR (tem), XCDR (tem));
	  tem = XCDR (elt);
	  /* Also skip the optional menu help string.  */
	  if (CONSP (tem) && STRINGP (XCAR (tem)))
	    {
	      XSETCDR (elt, Fcons (XCAR (tem), XCDR (tem)));
	      elt = XCDR (elt);
	      tem = XCDR (elt);
	    }
	  /* There may also be a list that caches key equivalences.
	     Just delete it for the new keymap.  */
	  if (CONSP (tem)
	      && CONSP (XCAR (tem))
	      && (NILP (XCAR (XCAR (tem)))
		  || VECTORP (XCAR (XCAR (tem)))))
	    {
	      XSETCDR (elt, XCDR (tem));
	      tem = XCDR (tem);
	    }
	  if (CONSP (tem) && EQ (XCAR (tem), Qkeymap))
	    XSETCDR (elt, Fcopy_keymap (tem));
	}
      else if (EQ (XCAR (tem), Qkeymap))
	res = Fcopy_keymap (elt);
    }
  return res;
}

static void
copy_keymap_1 (Lisp_Object chartable, Lisp_Object idx, Lisp_Object elt)
{
  Fset_char_table_range (chartable, idx, copy_keymap_item (elt));
}

DEFUN ("copy-keymap", Fcopy_keymap, Scopy_keymap, 1, 1, 0,
       doc: /* Return a copy of the keymap KEYMAP.
The copy starts out with the same definitions of KEYMAP,
but changing either the copy or KEYMAP does not affect the other.
Any key definitions that are subkeymaps are recursively copied.
However, a key definition which is a symbol whose definition is a keymap
is not copied.  */)
  (Lisp_Object keymap)
{
  register Lisp_Object copy, tail;
  keymap = get_keymap (keymap, 1, 0);
  copy = tail = Fcons (Qkeymap, Qnil);
  keymap = XCDR (keymap);		/* Skip the `keymap' symbol.  */

  while (CONSP (keymap) && !EQ (XCAR (keymap), Qkeymap))
    {
      Lisp_Object elt = XCAR (keymap);
      if (CHAR_TABLE_P (elt))
	{
	  elt = Fcopy_sequence (elt);
	  map_char_table (copy_keymap_1, Qnil, elt, elt);
	}
      else if (VECTORP (elt))
	{
	  int i;
	  elt = Fcopy_sequence (elt);
	  for (i = 0; i < ASIZE (elt); i++)
	    ASET (elt, i, copy_keymap_item (AREF (elt, i)));
	}
      else if (CONSP (elt))
	{
	  if (EQ (XCAR (elt), Qkeymap))
	    /* This is a sub keymap.  */
	    elt = Fcopy_keymap (elt);
	  else
	    elt = Fcons (XCAR (elt), copy_keymap_item (XCDR (elt)));
	}
      XSETCDR (tail, Fcons (elt, Qnil));
      tail = XCDR (tail);
      keymap = XCDR (keymap);
    }
  XSETCDR (tail, keymap);
  return copy;
}

/* Simple Keymap mutators and accessors.				*/

/* GC is possible in this function if it autoloads a keymap.  */

DEFUN ("define-key", Fdefine_key, Sdefine_key, 3, 3, 0,
       doc: /* In KEYMAP, define key sequence KEY as DEF.
KEYMAP is a keymap.

KEY is a string or a vector of symbols and characters, representing a
sequence of keystrokes and events.  Non-ASCII characters with codes
above 127 (such as ISO Latin-1) can be represented by vectors.
Two types of vector have special meanings:
 [remap COMMAND] remaps any key binding for COMMAND.
 [t] creates a default definition, which applies to any event with no
    other definition in KEYMAP.

DEF is anything that can be a key's definition:
 nil (means key is undefined in this keymap),
 a command (a Lisp function suitable for interactive calling),
 a string (treated as a keyboard macro),
 a keymap (to define a prefix key),
 a symbol (when the key is looked up, the symbol will stand for its
    function definition, which should at that time be one of the above,
    or another symbol whose function definition is used, etc.),
 a cons (STRING . DEFN), meaning that DEFN is the definition
    (DEFN should be a valid definition in its own right),
 or a cons (MAP . CHAR), meaning use definition of CHAR in keymap MAP,
 or an extended menu item definition.
 (See info node `(elisp)Extended Menu Items'.)

If KEYMAP is a sparse keymap with a binding for KEY, the existing
binding is altered.  If there is no binding for KEY, the new pair
binding KEY to DEF is added at the front of KEYMAP.  */)
  (Lisp_Object keymap, Lisp_Object key, Lisp_Object def)
{
  register int idx;
  register Lisp_Object c;
  register Lisp_Object cmd;
  int metized = 0;
  int meta_bit;
  int length;
  struct gcpro gcpro1, gcpro2, gcpro3;

  GCPRO3 (keymap, key, def);
  keymap = get_keymap (keymap, 1, 1);

  CHECK_VECTOR_OR_STRING (key);

  length = XFASTINT (Flength (key));
  if (length == 0)
    RETURN_UNGCPRO (Qnil);

  if (SYMBOLP (def) && !EQ (Vdefine_key_rebound_commands, Qt))
    Vdefine_key_rebound_commands = Fcons (def, Vdefine_key_rebound_commands);

  meta_bit = (VECTORP (key) || (STRINGP (key) && STRING_MULTIBYTE (key))
	      ? meta_modifier : 0x80);

  if (VECTORP (def) && ASIZE (def) > 0 && CONSP (AREF (def, 0)))
    { /* DEF is apparently an XEmacs-style keyboard macro.  */
      Lisp_Object tmp = Fmake_vector (make_number (ASIZE (def)), Qnil);
      int i = ASIZE (def);
      while (--i >= 0)
	{
	  Lisp_Object defi = AREF (def, i);
	  if (CONSP (defi) && lucid_event_type_list_p (defi))
	    defi = Fevent_convert_list (defi);
	  ASET (tmp, i, defi);
	}
      def = tmp;
    }

  idx = 0;
  while (1)
    {
      c = Faref (key, make_number (idx));

      if (CONSP (c))
	{
	  /* C may be a Lucid style event type list or a cons (FROM .
	     TO) specifying a range of characters.  */
	  if (lucid_event_type_list_p (c))
	    c = Fevent_convert_list (c);
	  else if (CHARACTERP (XCAR (c)))
	    CHECK_CHARACTER_CDR (c);
	}

      if (SYMBOLP (c))
	silly_event_symbol_error (c);

      if (INTEGERP (c)
	  && (XINT (c) & meta_bit)
	  && !metized)
	{
	  c = meta_prefix_char;
	  metized = 1;
	}
      else
	{
	  if (INTEGERP (c))
	    XSETINT (c, XINT (c) & ~meta_bit);

	  metized = 0;
	  idx++;
	}

      if (!INTEGERP (c) && !SYMBOLP (c)
	  && (!CONSP (c)
	      /* If C is a range, it must be a leaf.  */
	      || (INTEGERP (XCAR (c)) && idx != length)))
	message_with_string ("Key sequence contains invalid event %s", c, 1);

      if (idx == length)
	RETURN_UNGCPRO (store_in_keymap (keymap, c, def));

      cmd = access_keymap (keymap, c, 0, 1, 1);

      /* If this key is undefined, make it a prefix.  */
      if (NILP (cmd))
	cmd = define_as_prefix (keymap, c);

      keymap = get_keymap (cmd, 0, 1);
      if (!CONSP (keymap))
	{
	  const char *trailing_esc = ((EQ (c, meta_prefix_char) && metized)
				      ? (idx == 0 ? "ESC" : " ESC")
				      : "");

	  /* We must use Fkey_description rather than just passing key to
	     error; key might be a vector, not a string.  */
	  error ("Key sequence %s starts with non-prefix key %s%s",
		 SDATA (Fkey_description (key, Qnil)),
		 SDATA (Fkey_description (Fsubstring (key, make_number (0),
						      make_number (idx)),
					  Qnil)),
		 trailing_esc);
	}
    }
}

/* This function may GC (it calls Fkey_binding).  */

DEFUN ("command-remapping", Fcommand_remapping, Scommand_remapping, 1, 3, 0,
       doc: /* Return the remapping for command COMMAND.
Returns nil if COMMAND is not remapped (or not a symbol).

If the optional argument POSITION is non-nil, it specifies a mouse
position as returned by `event-start' and `event-end', and the
remapping occurs in the keymaps associated with it.  It can also be a
number or marker, in which case the keymap properties at the specified
buffer position instead of point are used.  The KEYMAPS argument is
ignored if POSITION is non-nil.

If the optional argument KEYMAPS is non-nil, it should be a list of
keymaps to search for command remapping.  Otherwise, search for the
remapping in all currently active keymaps.  */)
  (Lisp_Object command, Lisp_Object position, Lisp_Object keymaps)
{
  if (!SYMBOLP (command))
    return Qnil;

  ASET (command_remapping_vector, 1, command);

  if (NILP (keymaps))
    command = Fkey_binding (command_remapping_vector, Qnil, Qt, position);
  else
    command = Flookup_key (Fcons (Qkeymap, keymaps),
			   command_remapping_vector, Qnil);
  return INTEGERP (command) ? Qnil : command;
}

/* Value is number if KEY is too long; nil if valid but has no definition. */
/* GC is possible in this function.  */

DEFUN ("lookup-key", Flookup_key, Slookup_key, 2, 3, 0,
       doc: /* In keymap KEYMAP, look up key sequence KEY.  Return the definition.
A value of nil means undefined.  See doc of `define-key'
for kinds of definitions.

A number as value means KEY is "too long";
that is, characters or symbols in it except for the last one
fail to be a valid sequence of prefix characters in KEYMAP.
The number is how many characters at the front of KEY
it takes to reach a non-prefix key.

Normally, `lookup-key' ignores bindings for t, which act as default
bindings, used when nothing else in the keymap applies; this makes it
usable as a general function for probing keymaps.  However, if the
third optional argument ACCEPT-DEFAULT is non-nil, `lookup-key' will
recognize the default bindings, just as `read-key-sequence' does.  */)
  (Lisp_Object keymap, Lisp_Object key, Lisp_Object accept_default)
{
  register int idx;
  register Lisp_Object cmd;
  register Lisp_Object c;
  int length;
  int t_ok = !NILP (accept_default);
  struct gcpro gcpro1, gcpro2;

  GCPRO2 (keymap, key);
  keymap = get_keymap (keymap, 1, 1);

  CHECK_VECTOR_OR_STRING (key);

  length = XFASTINT (Flength (key));
  if (length == 0)
    RETURN_UNGCPRO (keymap);

  idx = 0;
  while (1)
    {
      c = Faref (key, make_number (idx++));

      if (CONSP (c) && lucid_event_type_list_p (c))
	c = Fevent_convert_list (c);

      /* Turn the 8th bit of string chars into a meta modifier.  */
      if (STRINGP (key) && XINT (c) & 0x80 && !STRING_MULTIBYTE (key))
	XSETINT (c, (XINT (c) | meta_modifier) & ~0x80);

      /* Allow string since binding for `menu-bar-select-buffer'
	 includes the buffer name in the key sequence.  */
      if (!INTEGERP (c) && !SYMBOLP (c) && !CONSP (c) && !STRINGP (c))
	message_with_string ("Key sequence contains invalid event %s", c, 1);

      cmd = access_keymap (keymap, c, t_ok, 0, 1);
      if (idx == length)
	RETURN_UNGCPRO (cmd);

      keymap = get_keymap (cmd, 0, 1);
      if (!CONSP (keymap))
	RETURN_UNGCPRO (make_number (idx));

      QUIT;
    }
}

/* Make KEYMAP define event C as a keymap (i.e., as a prefix).
   Assume that currently it does not define C at all.
   Return the keymap.  */

static Lisp_Object
define_as_prefix (Lisp_Object keymap, Lisp_Object c)
{
  Lisp_Object cmd;

  cmd = Fmake_sparse_keymap (Qnil);
  store_in_keymap (keymap, c, cmd);

  return cmd;
}

/* Append a key to the end of a key sequence.  We always make a vector.  */

static Lisp_Object
append_key (Lisp_Object key_sequence, Lisp_Object key)
{
  Lisp_Object args[2];

  args[0] = key_sequence;

  args[1] = Fcons (key, Qnil);
  return Fvconcat (2, args);
}

/* Given a event type C which is a symbol,
   signal an error if is a mistake such as RET or M-RET or C-DEL, etc.  */

static void
silly_event_symbol_error (Lisp_Object c)
{
  Lisp_Object parsed, base, name, assoc;
  int modifiers;

  parsed = parse_modifiers (c);
  modifiers = XFASTINT (XCAR (XCDR (parsed)));
  base = XCAR (parsed);
  name = Fsymbol_name (base);
  /* This alist includes elements such as ("RET" . "\\r").  */
  assoc = Fassoc (name, exclude_keys);

  if (! NILP (assoc))
    {
      char new_mods[sizeof ("\\A-\\C-\\H-\\M-\\S-\\s-")];
      char *p = new_mods;
      Lisp_Object keystring;
      if (modifiers & alt_modifier)
	{ *p++ = '\\'; *p++ = 'A'; *p++ = '-'; }
      if (modifiers & ctrl_modifier)
	{ *p++ = '\\'; *p++ = 'C'; *p++ = '-'; }
      if (modifiers & hyper_modifier)
	{ *p++ = '\\'; *p++ = 'H'; *p++ = '-'; }
      if (modifiers & meta_modifier)
	{ *p++ = '\\'; *p++ = 'M'; *p++ = '-'; }
      if (modifiers & shift_modifier)
	{ *p++ = '\\'; *p++ = 'S'; *p++ = '-'; }
      if (modifiers & super_modifier)
	{ *p++ = '\\'; *p++ = 's'; *p++ = '-'; }
      *p = 0;

      c = reorder_modifiers (c);
      keystring = concat2 (build_string (new_mods), XCDR (assoc));

      error ((modifiers & ~meta_modifier
	      ? "To bind the key %s, use [?%s], not [%s]"
	      : "To bind the key %s, use \"%s\", not [%s]"),
	     SDATA (SYMBOL_NAME (c)), SDATA (keystring),
	     SDATA (SYMBOL_NAME (c)));
    }
}

/* Global, local, and minor mode keymap stuff.				*/

/* We can't put these variables inside current_minor_maps, since under
   some systems, static gets macro-defined to be the empty string.
   Ickypoo.  */
static Lisp_Object *cmm_modes = NULL, *cmm_maps = NULL;
static ptrdiff_t cmm_size = 0;

/* Store a pointer to an array of the currently active minor modes in
   *modeptr, a pointer to an array of the keymaps of the currently
   active minor modes in *mapptr, and return the number of maps
   *mapptr contains.

   This function always returns a pointer to the same buffer, and may
   free or reallocate it, so if you want to keep it for a long time or
   hand it out to lisp code, copy it.  This procedure will be called
   for every key sequence read, so the nice lispy approach (return a
   new assoclist, list, what have you) for each invocation would
   result in a lot of consing over time.

   If we used xrealloc/xmalloc and ran out of memory, they would throw
   back to the command loop, which would try to read a key sequence,
   which would call this function again, resulting in an infinite
   loop.  Instead, we'll use realloc/malloc and silently truncate the
   list, let the key sequence be read, and hope some other piece of
   code signals the error.  */
ptrdiff_t
current_minor_maps (Lisp_Object **modeptr, Lisp_Object **mapptr)
{
  ptrdiff_t i = 0;
  int list_number = 0;
  Lisp_Object alist, assoc, var, val;
  Lisp_Object emulation_alists;
  Lisp_Object lists[2];

  emulation_alists = Vemulation_mode_map_alists;
  lists[0] = Vminor_mode_overriding_map_alist;
  lists[1] = Vminor_mode_map_alist;

  for (list_number = 0; list_number < 2; list_number++)
    {
      if (CONSP (emulation_alists))
	{
	  alist = XCAR (emulation_alists);
	  emulation_alists = XCDR (emulation_alists);
	  if (SYMBOLP (alist))
	    alist = find_symbol_value (alist);
	  list_number = -1;
	}
      else
	alist = lists[list_number];

      for ( ; CONSP (alist); alist = XCDR (alist))
	if ((assoc = XCAR (alist), CONSP (assoc))
	    && (var = XCAR (assoc), SYMBOLP (var))
	    && (val = find_symbol_value (var), !EQ (val, Qunbound))
	    && !NILP (val))
	  {
	    Lisp_Object temp;

	    /* If a variable has an entry in Vminor_mode_overriding_map_alist,
	       and also an entry in Vminor_mode_map_alist,
	       ignore the latter.  */
	    if (list_number == 1)
	      {
		val = assq_no_quit (var, lists[0]);
		if (!NILP (val))
		  continue;
	      }

	    if (i >= cmm_size)
	      {
		ptrdiff_t newsize, allocsize;
		Lisp_Object *newmodes, *newmaps;

		/* Check for size calculation overflow.  Other code
		   (e.g., read_key_sequence) adds 3 to the count
		   later, so subtract 3 from the limit here.  */
		if (min (PTRDIFF_MAX, SIZE_MAX) / (2 * sizeof *newmodes) - 3
		    < cmm_size)
		  break;

		newsize = cmm_size == 0 ? 30 : cmm_size * 2;
		allocsize = newsize * sizeof *newmodes;

		/* Use malloc here.  See the comment above this function.
		   Avoid realloc here; it causes spurious traps on GNU/Linux [KFS] */
		BLOCK_INPUT;
		newmodes = (Lisp_Object *) malloc (allocsize);
		if (newmodes)
		  {
		    if (cmm_modes)
		      {
			memcpy (newmodes, cmm_modes,
				cmm_size * sizeof cmm_modes[0]);
			free (cmm_modes);
		      }
		    cmm_modes = newmodes;
		  }

		newmaps = (Lisp_Object *) malloc (allocsize);
		if (newmaps)
		  {
		    if (cmm_maps)
		      {
			memcpy (newmaps, cmm_maps,
				cmm_size * sizeof cmm_maps[0]);
			free (cmm_maps);
		      }
		    cmm_maps = newmaps;
		  }
		UNBLOCK_INPUT;

		if (newmodes == NULL || newmaps == NULL)
		  break;
		cmm_size = newsize;
	      }

	    /* Get the keymap definition--or nil if it is not defined.  */
	    temp = Findirect_function (XCDR (assoc), Qt);
	    if (!NILP (temp))
	      {
		cmm_modes[i] = var;
		cmm_maps [i] = temp;
		i++;
	      }
	  }
    }

  if (modeptr) *modeptr = cmm_modes;
  if (mapptr)  *mapptr  = cmm_maps;
  return i;
}

DEFUN ("current-active-maps", Fcurrent_active_maps, Scurrent_active_maps,
       0, 2, 0,
       doc: /* Return a list of the currently active keymaps.
OLP if non-nil indicates that we should obey `overriding-local-map' and
`overriding-terminal-local-map'.  POSITION can specify a click position
like in the respective argument of `key-binding'. */)
  (Lisp_Object olp, Lisp_Object position)
{
  int count = SPECPDL_INDEX ();

  Lisp_Object keymaps = Fcons (current_global_map, Qnil);

  /* If a mouse click position is given, our variables are based on
     the buffer clicked on, not the current buffer.  So we may have to
     switch the buffer here. */

  if (CONSP (position))
    {
      Lisp_Object window;

      window = POSN_WINDOW (position);

      if (WINDOWP (window)
	  && BUFFERP (XWINDOW (window)->buffer)
	  && XBUFFER (XWINDOW (window)->buffer) != current_buffer)
	{
	  /* Arrange to go back to the original buffer once we're done
	     processing the key sequence.  We don't use
	     save_excursion_{save,restore} here, in analogy to
	     `read-key-sequence' to avoid saving point.  Maybe this
	     would not be a problem here, but it is easier to keep
	     things the same.
	  */

	  record_unwind_protect (Fset_buffer, Fcurrent_buffer ());

	  set_buffer_internal (XBUFFER (XWINDOW (window)->buffer));
	}
    }

  if (!NILP (olp))
    {
      if (!NILP (KVAR (current_kboard, Voverriding_terminal_local_map)))
	keymaps = Fcons (KVAR (current_kboard, Voverriding_terminal_local_map),
			 keymaps);
      /* The doc said that overriding-terminal-local-map should
	 override overriding-local-map.  The code used them both,
	 but it seems clearer to use just one.  rms, jan 2005.  */
      else if (!NILP (Voverriding_local_map))
	keymaps = Fcons (Voverriding_local_map, keymaps);
    }
  if (NILP (XCDR (keymaps)))
    {
      Lisp_Object *maps;
      int nmaps, i;
      EMACS_INT pt
	= INTEGERP (position) ? XINT (position)
	: MARKERP (position) ? marker_position (position)
	: PT;
      /* This usually returns the buffer's local map,
	 but that can be overridden by a `local-map' property.  */
      Lisp_Object local_map = get_local_map (pt, current_buffer, Qlocal_map);
      /* This returns nil unless there is a `keymap' property.  */
      Lisp_Object keymap = get_local_map (pt, current_buffer, Qkeymap);

      if (CONSP (position))
	{
	  Lisp_Object string = POSN_STRING (position);

	  /* For a mouse click, get the local text-property keymap
	     of the place clicked on, rather than point.  */

	  if (POSN_INBUFFER_P (position))
	    {
	      Lisp_Object pos;

	      pos = POSN_BUFFER_POSN (position);
	      if (INTEGERP (pos)
		  && XINT (pos) >= BEG && XINT (pos) <= Z)
		{
		  local_map = get_local_map (XINT (pos),
					     current_buffer, Qlocal_map);

		  keymap = get_local_map (XINT (pos),
					  current_buffer, Qkeymap);
		}
	    }

	  /* If on a mode line string with a local keymap,
	     or for a click on a string, i.e. overlay string or a
	     string displayed via the `display' property,
	     consider `local-map' and `keymap' properties of
	     that string.  */

	  if (CONSP (string) && STRINGP (XCAR (string)))
	    {
	      Lisp_Object pos, map;

	      pos = XCDR (string);
	      string = XCAR (string);
	      if (INTEGERP (pos)
		  && XINT (pos) >= 0
		  && XINT (pos) < SCHARS (string))
		{
		  map = Fget_text_property (pos, Qlocal_map, string);
		  if (!NILP (map))
		    local_map = map;

		  map = Fget_text_property (pos, Qkeymap, string);
		  if (!NILP (map))
		    keymap = map;
		}
	    }

	}

      if (!NILP (local_map))
	keymaps = Fcons (local_map, keymaps);

      /* Now put all the minor mode keymaps on the list.  */
      nmaps = current_minor_maps (0, &maps);

      for (i = --nmaps; i >= 0; i--)
	if (!NILP (maps[i]))
	  keymaps = Fcons (maps[i], keymaps);

      if (!NILP (keymap))
	keymaps = Fcons (keymap, keymaps);
    }

  unbind_to (count, Qnil);

  return keymaps;
}

/* GC is possible in this function if it autoloads a keymap.  */

DEFUN ("key-binding", Fkey_binding, Skey_binding, 1, 4, 0,
       doc: /* Return the binding for command KEY in current keymaps.
KEY is a string or vector, a sequence of keystrokes.
The binding is probably a symbol with a function definition.

Normally, `key-binding' ignores bindings for t, which act as default
bindings, used when nothing else in the keymap applies; this makes it
usable as a general function for probing keymaps.  However, if the
optional second argument ACCEPT-DEFAULT is non-nil, `key-binding' does
recognize the default bindings, just as `read-key-sequence' does.

Like the normal command loop, `key-binding' will remap the command
resulting from looking up KEY by looking up the command in the
current keymaps.  However, if the optional third argument NO-REMAP
is non-nil, `key-binding' returns the unmapped command.

If KEY is a key sequence initiated with the mouse, the used keymaps
will depend on the clicked mouse position with regard to the buffer
and possible local keymaps on strings.

If the optional argument POSITION is non-nil, it specifies a mouse
position as returned by `event-start' and `event-end', and the lookup
occurs in the keymaps associated with it instead of KEY.  It can also
be a number or marker, in which case the keymap properties at the
specified buffer position instead of point are used.
  */)
  (Lisp_Object key, Lisp_Object accept_default, Lisp_Object no_remap, Lisp_Object position)
{
  Lisp_Object value;

  if (NILP (position) && VECTORP (key))
    {
      Lisp_Object event
	/* mouse events may have a symbolic prefix indicating the
	   scrollbar or mode line */
	= AREF (key, SYMBOLP (AREF (key, 0)) && ASIZE (key) > 1 ? 1 : 0);

      /* We are not interested in locations without event data */

      if (EVENT_HAS_PARAMETERS (event) && CONSP (XCDR (event)))
	{
	  Lisp_Object kind = EVENT_HEAD_KIND (EVENT_HEAD (event));
	  if (EQ (kind, Qmouse_click))
	    position = EVENT_START (event);
	}
    }

  value = Flookup_key (Fcons (Qkeymap, Fcurrent_active_maps (Qt, position)),
		       key, accept_default);

  if (NILP (value) || INTEGERP (value))
    return Qnil;

  /* If the result of the ordinary keymap lookup is an interactive
     command, look for a key binding (ie. remapping) for that command.  */

  if (NILP (no_remap) && SYMBOLP (value))
    {
      Lisp_Object value1;
      if (value1 = Fcommand_remapping (value, position, Qnil), !NILP (value1))
	value = value1;
    }

  return value;
}

/* GC is possible in this function if it autoloads a keymap.  */

DEFUN ("local-key-binding", Flocal_key_binding, Slocal_key_binding, 1, 2, 0,
       doc: /* Return the binding for command KEYS in current local keymap only.
KEYS is a string or vector, a sequence of keystrokes.
The binding is probably a symbol with a function definition.

If optional argument ACCEPT-DEFAULT is non-nil, recognize default
bindings; see the description of `lookup-key' for more details about this.  */)
  (Lisp_Object keys, Lisp_Object accept_default)
{
  register Lisp_Object map;
  map = BVAR (current_buffer, keymap);
  if (NILP (map))
    return Qnil;
  return Flookup_key (map, keys, accept_default);
}

/* GC is possible in this function if it autoloads a keymap.  */

DEFUN ("global-key-binding", Fglobal_key_binding, Sglobal_key_binding, 1, 2, 0,
       doc: /* Return the binding for command KEYS in current global keymap only.
KEYS is a string or vector, a sequence of keystrokes.
The binding is probably a symbol with a function definition.
This function's return values are the same as those of `lookup-key'
\(which see).

If optional argument ACCEPT-DEFAULT is non-nil, recognize default
bindings; see the description of `lookup-key' for more details about this.  */)
  (Lisp_Object keys, Lisp_Object accept_default)
{
  return Flookup_key (current_global_map, keys, accept_default);
}

/* GC is possible in this function if it autoloads a keymap.  */

DEFUN ("minor-mode-key-binding", Fminor_mode_key_binding, Sminor_mode_key_binding, 1, 2, 0,
       doc: /* Find the visible minor mode bindings of KEY.
Return an alist of pairs (MODENAME . BINDING), where MODENAME is
the symbol which names the minor mode binding KEY, and BINDING is
KEY's definition in that mode.  In particular, if KEY has no
minor-mode bindings, return nil.  If the first binding is a
non-prefix, all subsequent bindings will be omitted, since they would
be ignored.  Similarly, the list doesn't include non-prefix bindings
that come after prefix bindings.

If optional argument ACCEPT-DEFAULT is non-nil, recognize default
bindings; see the description of `lookup-key' for more details about this.  */)
  (Lisp_Object key, Lisp_Object accept_default)
{
  Lisp_Object *modes, *maps;
  int nmaps;
  Lisp_Object binding;
  int i, j;
  struct gcpro gcpro1, gcpro2;

  nmaps = current_minor_maps (&modes, &maps);
  /* Note that all these maps are GCPRO'd
     in the places where we found them.  */

  binding = Qnil;
  GCPRO2 (key, binding);

  for (i = j = 0; i < nmaps; i++)
    if (!NILP (maps[i])
	&& !NILP (binding = Flookup_key (maps[i], key, accept_default))
	&& !INTEGERP (binding))
      {
	if (KEYMAPP (binding))
	  maps[j++] = Fcons (modes[i], binding);
	else if (j == 0)
	  RETURN_UNGCPRO (Fcons (Fcons (modes[i], binding), Qnil));
      }

  UNGCPRO;
  return Flist (j, maps);
}

DEFUN ("define-prefix-command", Fdefine_prefix_command, Sdefine_prefix_command, 1, 3, 0,
       doc: /* Define COMMAND as a prefix command.  COMMAND should be a symbol.
A new sparse keymap is stored as COMMAND's function definition and its value.
If a second optional argument MAPVAR is given, the map is stored as
its value instead of as COMMAND's value; but COMMAND is still defined
as a function.
The third optional argument NAME, if given, supplies a menu name
string for the map.  This is required to use the keymap as a menu.
This function returns COMMAND.  */)
  (Lisp_Object command, Lisp_Object mapvar, Lisp_Object name)
{
  Lisp_Object map;
  map = Fmake_sparse_keymap (name);
  Ffset (command, map);
  if (!NILP (mapvar))
    Fset (mapvar, map);
  else
    Fset (command, map);
  return command;
}

DEFUN ("use-global-map", Fuse_global_map, Suse_global_map, 1, 1, 0,
       doc: /* Select KEYMAP as the global keymap.  */)
  (Lisp_Object keymap)
{
  keymap = get_keymap (keymap, 1, 1);
  current_global_map = keymap;

  return Qnil;
}

DEFUN ("use-local-map", Fuse_local_map, Suse_local_map, 1, 1, 0,
       doc: /* Select KEYMAP as the local keymap.
If KEYMAP is nil, that means no local keymap.  */)
  (Lisp_Object keymap)
{
  if (!NILP (keymap))
    keymap = get_keymap (keymap, 1, 1);

  BVAR (current_buffer, keymap) = keymap;

  return Qnil;
}

DEFUN ("current-local-map", Fcurrent_local_map, Scurrent_local_map, 0, 0, 0,
       doc: /* Return current buffer's local keymap, or nil if it has none.
Normally the local keymap is set by the major mode with `use-local-map'.  */)
  (void)
{
  return BVAR (current_buffer, keymap);
}

DEFUN ("current-global-map", Fcurrent_global_map, Scurrent_global_map, 0, 0, 0,
       doc: /* Return the current global keymap.  */)
  (void)
{
  return current_global_map;
}

DEFUN ("current-minor-mode-maps", Fcurrent_minor_mode_maps, Scurrent_minor_mode_maps, 0, 0, 0,
       doc: /* Return a list of keymaps for the minor modes of the current buffer.  */)
  (void)
{
  Lisp_Object *maps;
  int nmaps = current_minor_maps (0, &maps);

  return Flist (nmaps, maps);
}

/* Help functions for describing and documenting keymaps.		*/

struct accessible_keymaps_data {
  Lisp_Object maps, tail, thisseq;
  /* Does the current sequence end in the meta-prefix-char?  */
  int is_metized;
};

static void
accessible_keymaps_1 (Lisp_Object key, Lisp_Object cmd, Lisp_Object args, void *data)
/* Use void* data to be compatible with map_keymap_function_t.  */
{
  struct accessible_keymaps_data *d = data; /* Cast! */
  Lisp_Object maps = d->maps;
  Lisp_Object tail = d->tail;
  Lisp_Object thisseq = d->thisseq;
  int is_metized = d->is_metized && INTEGERP (key);
  Lisp_Object tem;

  cmd = get_keymap (get_keyelt (cmd, 0), 0, 0);
  if (NILP (cmd))
    return;

  /* Look for and break cycles.  */
  while (!NILP (tem = Frassq (cmd, maps)))
    {
      Lisp_Object prefix = XCAR (tem);
      int lim = XINT (Flength (XCAR (tem)));
      if (lim <= XINT (Flength (thisseq)))
	{ /* This keymap was already seen with a smaller prefix.  */
	  int i = 0;
	  while (i < lim && EQ (Faref (prefix, make_number (i)),
				Faref (thisseq, make_number (i))))
	    i++;
	  if (i >= lim)
	    /* `prefix' is a prefix of `thisseq' => there's a cycle.  */
	    return;
	}
      /* This occurrence of `cmd' in `maps' does not correspond to a cycle,
	 but maybe `cmd' occurs again further down in `maps', so keep
	 looking.  */
      maps = XCDR (Fmemq (tem, maps));
    }

  /* If the last key in thisseq is meta-prefix-char,
     turn it into a meta-ized keystroke.  We know
     that the event we're about to append is an
     ascii keystroke since we're processing a
     keymap table.  */
  if (is_metized)
    {
      int meta_bit = meta_modifier;
      Lisp_Object last = make_number (XINT (Flength (thisseq)) - 1);
      tem = Fcopy_sequence (thisseq);

      Faset (tem, last, make_number (XINT (key) | meta_bit));

      /* This new sequence is the same length as
	 thisseq, so stick it in the list right
	 after this one.  */
      XSETCDR (tail,
	       Fcons (Fcons (tem, cmd), XCDR (tail)));
    }
  else
    {
      tem = append_key (thisseq, key);
      nconc2 (tail, Fcons (Fcons (tem, cmd), Qnil));
    }
}

/* This function cannot GC.  */

DEFUN ("accessible-keymaps", Faccessible_keymaps, Saccessible_keymaps,
       1, 2, 0,
       doc: /* Find all keymaps accessible via prefix characters from KEYMAP.
Returns a list of elements of the form (KEYS . MAP), where the sequence
KEYS starting from KEYMAP gets you to MAP.  These elements are ordered
so that the KEYS increase in length.  The first element is ([] . KEYMAP).
An optional argument PREFIX, if non-nil, should be a key sequence;
then the value includes only maps for prefixes that start with PREFIX.  */)
  (Lisp_Object keymap, Lisp_Object prefix)
{
  Lisp_Object maps, tail;
  int prefixlen = XINT (Flength (prefix));

  /* no need for gcpro because we don't autoload any keymaps.  */

  if (!NILP (prefix))
    {
      /* If a prefix was specified, start with the keymap (if any) for
	 that prefix, so we don't waste time considering other prefixes.  */
      Lisp_Object tem;
      tem = Flookup_key (keymap, prefix, Qt);
      /* Flookup_key may give us nil, or a number,
	 if the prefix is not defined in this particular map.
	 It might even give us a list that isn't a keymap.  */
      tem = get_keymap (tem, 0, 0);
      /* If the keymap is autoloaded `tem' is not a cons-cell, but we still
	 want to return it.  */
      if (!NILP (tem))
	{
	  /* Convert PREFIX to a vector now, so that later on
	     we don't have to deal with the possibility of a string.  */
	  if (STRINGP (prefix))
	    {
	      int i, i_byte, c;
	      Lisp_Object copy;

	      copy = Fmake_vector (make_number (SCHARS (prefix)), Qnil);
	      for (i = 0, i_byte = 0; i < SCHARS (prefix);)
		{
		  int i_before = i;

		  FETCH_STRING_CHAR_ADVANCE (c, prefix, i, i_byte);
		  if (SINGLE_BYTE_CHAR_P (c) && (c & 0200))
		    c ^= 0200 | meta_modifier;
		  ASET (copy, i_before, make_number (c));
		}
	      prefix = copy;
	    }
	  maps = Fcons (Fcons (prefix, tem), Qnil);
	}
      else
	return Qnil;
    }
  else
    maps = Fcons (Fcons (Fmake_vector (make_number (0), Qnil),
			 get_keymap (keymap, 1, 0)),
		  Qnil);

  /* For each map in the list maps,
     look at any other maps it points to,
     and stick them at the end if they are not already in the list.

     This is a breadth-first traversal, where tail is the queue of
     nodes, and maps accumulates a list of all nodes visited.  */

  for (tail = maps; CONSP (tail); tail = XCDR (tail))
    {
      struct accessible_keymaps_data data;
      register Lisp_Object thismap = Fcdr (XCAR (tail));
      Lisp_Object last;

      data.thisseq = Fcar (XCAR (tail));
      data.maps = maps;
      data.tail = tail;
      last = make_number (XINT (Flength (data.thisseq)) - 1);
      /* Does the current sequence end in the meta-prefix-char?  */
      data.is_metized = (XINT (last) >= 0
		    /* Don't metize the last char of PREFIX.  */
		    && XINT (last) >= prefixlen
		    && EQ (Faref (data.thisseq, last), meta_prefix_char));

      /* Since we can't run lisp code, we can't scan autoloaded maps.  */
      if (CONSP (thismap))
	map_keymap (thismap, accessible_keymaps_1, Qnil, &data, 0);
    }
  return maps;
}
static Lisp_Object Qsingle_key_description, Qkey_description;

/* This function cannot GC.  */

DEFUN ("key-description", Fkey_description, Skey_description, 1, 2, 0,
       doc: /* Return a pretty description of key-sequence KEYS.
Optional arg PREFIX is the sequence of keys leading up to KEYS.
For example, [?\C-x ?l] is converted into the string \"C-x l\".

The `kbd' macro is an approximate inverse of this.  */)
  (Lisp_Object keys, Lisp_Object prefix)
{
  int len = 0;
  int i, i_byte;
  Lisp_Object *args;
  int size = XINT (Flength (keys));
  Lisp_Object list;
  Lisp_Object sep = build_string (" ");
  Lisp_Object key;
  int add_meta = 0;

  if (!NILP (prefix))
    size += XINT (Flength (prefix));

  /* This has one extra element at the end that we don't pass to Fconcat.  */
  args = (Lisp_Object *) alloca (size * 4 * sizeof (Lisp_Object));

  /* In effect, this computes
     (mapconcat 'single-key-description keys " ")
     but we shouldn't use mapconcat because it can do GC.  */

 next_list:
  if (!NILP (prefix))
    list = prefix, prefix = Qnil;
  else if (!NILP (keys))
    list = keys, keys = Qnil;
  else
    {
      if (add_meta)
	{
	  args[len] = Fsingle_key_description (meta_prefix_char, Qnil);
	  len += 2;
	}
      else if (len == 0)
	return empty_unibyte_string;
      return Fconcat (len - 1, args);
    }

  if (STRINGP (list))
    size = SCHARS (list);
  else if (VECTORP (list))
    size = ASIZE (list);
  else if (CONSP (list))
    size = XINT (Flength (list));
  else
    wrong_type_argument (Qarrayp, list);

  i = i_byte = 0;

  while (i < size)
    {
      if (STRINGP (list))
	{
	  int c;
	  FETCH_STRING_CHAR_ADVANCE (c, list, i, i_byte);
	  if (SINGLE_BYTE_CHAR_P (c) && (c & 0200))
	    c ^= 0200 | meta_modifier;
	  XSETFASTINT (key, c);
	}
      else if (VECTORP (list))
	{
	  key = AREF (list, i); i++;
	}
      else
	{
	  key = XCAR (list);
	  list = XCDR (list);
	  i++;
	}

      if (add_meta)
	{
	  if (!INTEGERP (key)
	      || EQ (key, meta_prefix_char)
	      || (XINT (key) & meta_modifier))
	    {
	      args[len++] = Fsingle_key_description (meta_prefix_char, Qnil);
	      args[len++] = sep;
	      if (EQ (key, meta_prefix_char))
		continue;
	    }
	  else
	    XSETINT (key, (XINT (key) | meta_modifier) & ~0x80);
	  add_meta = 0;
	}
      else if (EQ (key, meta_prefix_char))
	{
	  add_meta = 1;
	  continue;
	}
      args[len++] = Fsingle_key_description (key, Qnil);
      args[len++] = sep;
    }
  goto next_list;
}


char *
push_key_description (EMACS_INT ch, char *p, int force_multibyte)
{
  int c, c2;

  /* Clear all the meaningless bits above the meta bit.  */
  c = ch & (meta_modifier | ~ - meta_modifier);
  c2 = c & ~(alt_modifier | ctrl_modifier | hyper_modifier
	     | meta_modifier | shift_modifier | super_modifier);

  if (! CHARACTERP (make_number (c2)))
    {
      /* KEY_DESCRIPTION_SIZE is large enough for this.  */
      p += sprintf (p, "[%d]", c);
      return p;
    }

  if (c & alt_modifier)
    {
      *p++ = 'A';
      *p++ = '-';
      c -= alt_modifier;
    }
  if ((c & ctrl_modifier) != 0
      || (c2 < ' ' && c2 != 27 && c2 != '\t' && c2 != Ctl ('M')))
    {
      *p++ = 'C';
      *p++ = '-';
      c &= ~ctrl_modifier;
    }
  if (c & hyper_modifier)
    {
      *p++ = 'H';
      *p++ = '-';
      c -= hyper_modifier;
    }
  if (c & meta_modifier)
    {
      *p++ = 'M';
      *p++ = '-';
      c -= meta_modifier;
    }
  if (c & shift_modifier)
    {
      *p++ = 'S';
      *p++ = '-';
      c -= shift_modifier;
    }
  if (c & super_modifier)
    {
      *p++ = 's';
      *p++ = '-';
      c -= super_modifier;
    }
  if (c < 040)
    {
      if (c == 033)
	{
	  *p++ = 'E';
	  *p++ = 'S';
	  *p++ = 'C';
	}
      else if (c == '\t')
	{
	  *p++ = 'T';
	  *p++ = 'A';
	  *p++ = 'B';
	}
      else if (c == Ctl ('M'))
	{
	  *p++ = 'R';
	  *p++ = 'E';
	  *p++ = 'T';
	}
      else
	{
	  /* `C-' already added above.  */
	  if (c > 0 && c <= Ctl ('Z'))
	    *p++ = c + 0140;
	  else
	    *p++ = c + 0100;
	}
    }
  else if (c == 0177)
    {
      *p++ = 'D';
      *p++ = 'E';
      *p++ = 'L';
    }
  else if (c == ' ')
   {
      *p++ = 'S';
      *p++ = 'P';
      *p++ = 'C';
    }
  else if (c < 128
	   || (NILP (BVAR (current_buffer, enable_multibyte_characters))
	       && SINGLE_BYTE_CHAR_P (c)
	       && !force_multibyte))
    {
      *p++ = c;
    }
  else
    {
      /* Now we are sure that C is a valid character code.  */
      if (NILP (BVAR (current_buffer, enable_multibyte_characters))
	  && ! force_multibyte)
	*p++ = multibyte_char_to_unibyte (c);
      else
	p += CHAR_STRING (c, (unsigned char *) p);
    }

  return p;
}

/* This function cannot GC.  */

DEFUN ("single-key-description", Fsingle_key_description,
       Ssingle_key_description, 1, 2, 0,
       doc: /* Return a pretty description of command character KEY.
Control characters turn into C-whatever, etc.
Optional argument NO-ANGLES non-nil means don't put angle brackets
around function keys and event symbols.  */)
  (Lisp_Object key, Lisp_Object no_angles)
{
  if (CONSP (key) && lucid_event_type_list_p (key))
    key = Fevent_convert_list (key);

  if (CONSP (key) && INTEGERP (XCAR (key)) && INTEGERP (XCDR (key)))
    /* An interval from a map-char-table.  */
    return concat3 (Fsingle_key_description (XCAR (key), no_angles),
		    build_string (".."),
		    Fsingle_key_description (XCDR (key), no_angles));

  key = EVENT_HEAD (key);

  if (INTEGERP (key))		/* Normal character.  */
    {
      char tem[KEY_DESCRIPTION_SIZE], *p;

      p = push_key_description (XINT (key), tem, 1);
      *p = 0;
      return make_specified_string (tem, -1, p - tem, 1);
    }
  else if (SYMBOLP (key))	/* Function key or event-symbol.  */
    {
      if (NILP (no_angles))
	{
	  char *buffer;
	  Lisp_Object result;
	  USE_SAFE_ALLOCA;
	  SAFE_ALLOCA (buffer, char *,
		       sizeof "<>" + SBYTES (SYMBOL_NAME (key)));
	  esprintf (buffer, "<%s>", SDATA (SYMBOL_NAME (key)));
	  result = build_string (buffer);
	  SAFE_FREE ();
	  return result;
	}
      else
	return Fsymbol_name (key);
    }
  else if (STRINGP (key))	/* Buffer names in the menubar.  */
    return Fcopy_sequence (key);
  else
    error ("KEY must be an integer, cons, symbol, or string");
  return Qnil;
}

static char *
push_text_char_description (register unsigned int c, register char *p)
{
  if (c >= 0200)
    {
      *p++ = 'M';
      *p++ = '-';
      c -= 0200;
    }
  if (c < 040)
    {
      *p++ = '^';
      *p++ = c + 64;		/* 'A' - 1 */
    }
  else if (c == 0177)
    {
      *p++ = '^';
      *p++ = '?';
    }
  else
    *p++ = c;
  return p;
}

/* This function cannot GC.  */

DEFUN ("text-char-description", Ftext_char_description, Stext_char_description, 1, 1, 0,
       doc: /* Return a pretty description of file-character CHARACTER.
Control characters turn into "^char", etc.  This differs from
`single-key-description' which turns them into "C-char".
Also, this function recognizes the 2**7 bit as the Meta character,
whereas `single-key-description' uses the 2**27 bit for Meta.
See Info node `(elisp)Describing Characters' for examples.  */)
  (Lisp_Object character)
{
  /* Currently MAX_MULTIBYTE_LENGTH is 4 (< 6).  */
  char str[6];
  int c;

  CHECK_NUMBER (character);

  c = XINT (character);
  if (!ASCII_CHAR_P (c))
    {
      int len = CHAR_STRING (c, (unsigned char *) str);

      return make_multibyte_string (str, 1, len);
    }

  *push_text_char_description (c & 0377, str) = 0;

  return build_string (str);
}

static int where_is_preferred_modifier;

/* Return 0 if SEQ uses non-preferred modifiers or non-char events.
   Else, return 2 if SEQ uses the where_is_preferred_modifier,
   and 1 otherwise.  */
static int
preferred_sequence_p (Lisp_Object seq)
{
  int i;
  int len = XINT (Flength (seq));
  int result = 1;

  for (i = 0; i < len; i++)
    {
      Lisp_Object ii, elt;

      XSETFASTINT (ii, i);
      elt = Faref (seq, ii);

      if (!INTEGERP (elt))
	return 0;
      else
	{
	  int modifiers = XINT (elt) & (CHAR_MODIFIER_MASK & ~CHAR_META);
	  if (modifiers == where_is_preferred_modifier)
	    result = 2;
	  else if (modifiers)
	    return 0;
	}
    }

  return result;
}


/* where-is - finding a command in a set of keymaps.			*/

static void where_is_internal_1 (Lisp_Object key, Lisp_Object binding,
                                 Lisp_Object args, void *data);

/* Like Flookup_key, but uses a list of keymaps SHADOW instead of a single map.
   Returns the first non-nil binding found in any of those maps.
   If REMAP is true, pass the result of the lookup through command
   remapping before returning it.  */

static Lisp_Object
shadow_lookup (Lisp_Object shadow, Lisp_Object key, Lisp_Object flag,
	       int remap)
{
  Lisp_Object tail, value;

  for (tail = shadow; CONSP (tail); tail = XCDR (tail))
    {
      value = Flookup_key (XCAR (tail), key, flag);
      if (NATNUMP (value))
	{
	  value = Flookup_key (XCAR (tail),
			       Fsubstring (key, make_number (0), value), flag);
	  if (!NILP (value))
	    return Qnil;
	}
      else if (!NILP (value))
	{
	  Lisp_Object remapping;
	  if (remap && SYMBOLP (value)
	      && (remapping = Fcommand_remapping (value, Qnil, shadow),
		  !NILP (remapping)))
	    return remapping;
	  else
	    return value;
	}
    }
  return Qnil;
}

static Lisp_Object Vmouse_events;

struct where_is_internal_data {
  Lisp_Object definition, this, last;
  int last_is_meta, noindirect;
  Lisp_Object sequences;
};

/* This function can't GC, AFAIK.  */
/* Return the list of bindings found.  This list is ordered "longest
   to shortest".  It may include bindings that are actually shadowed
   by others, as well as duplicate bindings and remapping bindings.
   The list returned is potentially shared with where_is_cache, so
   be careful not to modify it via side-effects.  */

static Lisp_Object
where_is_internal (Lisp_Object definition, Lisp_Object keymaps,
		   int noindirect, int nomenus)
{
  Lisp_Object maps = Qnil;
  Lisp_Object found;
  struct where_is_internal_data data;

  /* Only important use of caching is for the menubar
     (i.e. where-is-internal called with (def nil t nil nil)).  */
  if (nomenus && !noindirect)
    {
      /* Check heuristic-consistency of the cache.  */
      if (NILP (Fequal (keymaps, where_is_cache_keymaps)))
	where_is_cache = Qnil;

      if (NILP (where_is_cache))
	{
	  /* We need to create the cache.  */
	  Lisp_Object args[2];
	  where_is_cache = Fmake_hash_table (0, args);
	  where_is_cache_keymaps = Qt;
	}
      else
	/* We can reuse the cache.  */
	return Fgethash (definition, where_is_cache, Qnil);
    }
  else
    /* Kill the cache so that where_is_internal_1 doesn't think
       we're filling it up.  */
    where_is_cache = Qnil;

  found = keymaps;
  while (CONSP (found))
    {
      maps =
	nconc2 (maps,
		Faccessible_keymaps (get_keymap (XCAR (found), 1, 0), Qnil));
      found = XCDR (found);
    }

  data.sequences = Qnil;
  for (; CONSP (maps); maps = XCDR (maps))
    {
      /* Key sequence to reach map, and the map that it reaches */
      register Lisp_Object this, map, tem;

      /* In order to fold [META-PREFIX-CHAR CHAR] sequences into
	 [M-CHAR] sequences, check if last character of the sequence
	 is the meta-prefix char.  */
      Lisp_Object last;
      int last_is_meta;

      this = Fcar (XCAR (maps));
      map  = Fcdr (XCAR (maps));
      last = make_number (XINT (Flength (this)) - 1);
      last_is_meta = (XINT (last) >= 0
		      && EQ (Faref (this, last), meta_prefix_char));

      /* if (nomenus && !preferred_sequence_p (this)) */
      if (nomenus && XINT (last) >= 0
	  && SYMBOLP (tem = Faref (this, make_number (0)))
	  && !NILP (Fmemq (XCAR (parse_modifiers (tem)), Vmouse_events)))
	/* If no menu entries should be returned, skip over the
	   keymaps bound to `menu-bar' and `tool-bar' and other
	   non-ascii prefixes like `C-down-mouse-2'.  */
	continue;

      QUIT;

      data.definition = definition;
      data.noindirect = noindirect;
      data.this = this;
      data.last = last;
      data.last_is_meta = last_is_meta;

      if (CONSP (map))
	map_keymap (map, where_is_internal_1, Qnil, &data, 0);
    }

  if (nomenus && !noindirect)
    { /* Remember for which keymaps this cache was built.
	 We do it here (late) because we want to keep where_is_cache_keymaps
	 set to t while the cache isn't fully filled.  */
      where_is_cache_keymaps = keymaps;
      /* During cache-filling, data.sequences is not filled by
	 where_is_internal_1.  */
      return Fgethash (definition, where_is_cache, Qnil);
    }
  else
    return data.sequences;
}

/* This function can GC if Flookup_key autoloads any keymaps.  */

DEFUN ("where-is-internal", Fwhere_is_internal, Swhere_is_internal, 1, 5, 0,
       doc: /* Return list of keys that invoke DEFINITION.
If KEYMAP is a keymap, search only KEYMAP and the global keymap.
If KEYMAP is nil, search all the currently active keymaps, except
 for `overriding-local-map' (which is ignored).
If KEYMAP is a list of keymaps, search only those keymaps.

If optional 3rd arg FIRSTONLY is non-nil, return the first key sequence found,
rather than a list of all possible key sequences.
If FIRSTONLY is the symbol `non-ascii', return the first binding found,
no matter what it is.
If FIRSTONLY has another non-nil value, prefer bindings
that use the modifier key specified in `where-is-preferred-modifier'
\(or their meta variants) and entirely reject menu bindings.

If optional 4th arg NOINDIRECT is non-nil, don't follow indirections
to other keymaps or slots.  This makes it possible to search for an
indirect definition itself.

The optional 5th arg NO-REMAP alters how command remapping is handled:

- If another command OTHER-COMMAND is remapped to DEFINITION, normally
  search for the bindings of OTHER-COMMAND and include them in the
  returned list.  But if NO-REMAP is non-nil, include the vector
  [remap OTHER-COMMAND] in the returned list instead, without
  searching for those other bindings.

- If DEFINITION is remapped to OTHER-COMMAND, normally return the
  bindings for OTHER-COMMAND.  But if NO-REMAP is non-nil, return the
  bindings for DEFINITION instead, ignoring its remapping.  */)
  (Lisp_Object definition, Lisp_Object keymap, Lisp_Object firstonly, Lisp_Object noindirect, Lisp_Object no_remap)
{
  /* The keymaps in which to search.  */
  Lisp_Object keymaps;
  /* Potentially relevant bindings in "shortest to longest" order.  */
  Lisp_Object sequences = Qnil;
    /* Actually relevant bindings.  */
  Lisp_Object found = Qnil;
  /* 1 means ignore all menu bindings entirely.  */
  int nomenus = !NILP (firstonly) && !EQ (firstonly, Qnon_ascii);
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4, gcpro5, gcpro6;
  /* List of sequences found via remapping.  Keep them in a separate
     variable, so as to push them later, since we prefer
     non-remapped binding.  */
  Lisp_Object remapped_sequences = Qnil;
  /* Whether or not we're handling remapped sequences.  This is needed
     because remapping is not done recursively by Fcommand_remapping: you
     can't remap a remapped command.  */
  int remapped = 0;
  Lisp_Object tem = Qnil;

  /* Refresh the C version of the modifier preference.  */
  where_is_preferred_modifier
    = parse_solitary_modifier (Vwhere_is_preferred_modifier);

  /* Find the relevant keymaps.  */
  if (CONSP (keymap) && KEYMAPP (XCAR (keymap)))
    keymaps = keymap;
  else if (!NILP (keymap))
    keymaps = Fcons (keymap, Fcons (current_global_map, Qnil));
  else
    keymaps = Fcurrent_active_maps (Qnil, Qnil);

  GCPRO6 (definition, keymaps, found, sequences, remapped_sequences, tem);

  tem = Fcommand_remapping (definition, Qnil, keymaps);
  /* If `definition' is remapped to tem', then OT1H no key will run
     that command (since they will run `tem' instead), so we should
     return nil; but OTOH all keys bound to `definition' (or to `tem')
     will run the same command.
     So for menu-shortcut purposes, we want to find all the keys bound (maybe
     via remapping) to `tem'.  But for the purpose of finding the keys that
     run `definition', then we'd want to just return nil.
     We choose to make it work right for menu-shortcuts, since it's the most
     common use.
     Known bugs: if you remap switch-to-buffer to toto, C-h f switch-to-buffer
     will tell you that switch-to-buffer is bound to C-x b even though C-x b
     will run toto instead.  And if `toto' is itself remapped to forward-char,
     then C-h f toto will tell you that it's bound to C-f even though C-f does
     not run toto and it won't tell you that C-x b does run toto.  */
  if (NILP (no_remap) && !NILP (tem))
    definition = tem;

  if (SYMBOLP (definition)
      && !NILP (firstonly)
      && !NILP (tem = Fget (definition, QCadvertised_binding)))
    {
      /* We have a list of advertised bindings.  */
      while (CONSP (tem))
	if (EQ (shadow_lookup (keymaps, XCAR (tem), Qnil, 0), definition))
	  RETURN_UNGCPRO (XCAR (tem));
	else
	  tem = XCDR (tem);
      if (EQ (shadow_lookup (keymaps, tem, Qnil, 0), definition))
	RETURN_UNGCPRO (tem);
    }

  sequences = Freverse (where_is_internal (definition, keymaps,
					   !NILP (noindirect), nomenus));

  while (CONSP (sequences)
	 /* If we're at the end of the `sequences' list and we haven't
	    considered remapped sequences yet, copy them over and
	    process them.  */
	 || (!remapped && (sequences = remapped_sequences,
			   remapped = 1,
			   CONSP (sequences))))
    {
      Lisp_Object sequence, function;

      sequence = XCAR (sequences);
      sequences = XCDR (sequences);

      /* Verify that this key binding is not shadowed by another
	 binding for the same key, before we say it exists.

	 Mechanism: look for local definition of this key and if
	 it is defined and does not match what we found then
	 ignore this key.

	 Either nil or number as value from Flookup_key
	 means undefined.  */
      if (NILP (Fequal (shadow_lookup (keymaps, sequence, Qnil, remapped),
			definition)))
	continue;

      /* If the current sequence is a command remapping with
	 format [remap COMMAND], find the key sequences
	 which run COMMAND, and use those sequences instead.  */
      if (NILP (no_remap) && !remapped
	  && VECTORP (sequence) && ASIZE (sequence) == 2
	  && EQ (AREF (sequence, 0), Qremap)
	  && (function = AREF (sequence, 1), SYMBOLP (function)))
	{
	  Lisp_Object seqs = where_is_internal (function, keymaps,
						!NILP (noindirect), nomenus);
	  remapped_sequences = nconc2 (Freverse (seqs), remapped_sequences);
	  continue;
	}

      /* Don't annoy user with strings from a menu such as the
	 entries from the "Edit => Paste from Kill Menu".
	 Change them all to "(any string)", so that there
	 seems to be only one menu item to report.  */
      if (! NILP (sequence))
	{
	  Lisp_Object tem1;
	  tem1 = Faref (sequence, make_number (ASIZE (sequence) - 1));
	  if (STRINGP (tem1))
	    Faset (sequence, make_number (ASIZE (sequence) - 1),
		   build_string ("(any string)"));
	}

      /* It is a true unshadowed match.  Record it, unless it's already
	 been seen (as could happen when inheriting keymaps).  */
      if (NILP (Fmember (sequence, found)))
	found = Fcons (sequence, found);

      /* If firstonly is Qnon_ascii, then we can return the first
	 binding we find.  If firstonly is not Qnon_ascii but not
	 nil, then we should return the first ascii-only binding
	 we find.  */
      if (EQ (firstonly, Qnon_ascii))
	RETURN_UNGCPRO (sequence);
      else if (!NILP (firstonly)
	       && 2 == preferred_sequence_p (sequence))
	RETURN_UNGCPRO (sequence);
    }

  UNGCPRO;

  found = Fnreverse (found);

  /* firstonly may have been t, but we may have gone all the way through
     the keymaps without finding an all-ASCII key sequence.  So just
     return the best we could find.  */
  if (NILP (firstonly))
    return found;
  else if (where_is_preferred_modifier == 0)
    return Fcar (found);
  else
    { /* Maybe we did not find a preferred_modifier binding, but we did find
	 some ASCII binding.  */
      Lisp_Object bindings = found;
      while (CONSP (bindings))
	if (preferred_sequence_p (XCAR (bindings)))
	  return XCAR (bindings);
	else
	  bindings = XCDR (bindings);
      return Fcar (found);
    }
}

/* This function can GC because get_keyelt can.  */

static void
where_is_internal_1 (Lisp_Object key, Lisp_Object binding, Lisp_Object args, void *data)
{
  struct where_is_internal_data *d = data; /* Cast! */
  Lisp_Object definition = d->definition;
  int noindirect = d->noindirect;
  Lisp_Object this = d->this;
  Lisp_Object last = d->last;
  int last_is_meta = d->last_is_meta;
  Lisp_Object sequence;

  /* Search through indirections unless that's not wanted.  */
  if (!noindirect)
    binding = get_keyelt (binding, 0);

  /* End this iteration if this element does not match
     the target.  */

  if (!(!NILP (where_is_cache)	/* everything "matches" during cache-fill.  */
	|| EQ (binding, definition)
	|| (CONSP (definition) && !NILP (Fequal (binding, definition)))))
    /* Doesn't match.  */
    return;

  /* We have found a match.  Construct the key sequence where we found it.  */
  if (INTEGERP (key) && last_is_meta)
    {
      sequence = Fcopy_sequence (this);
      Faset (sequence, last, make_number (XINT (key) | meta_modifier));
    }
  else
    {
      if (CONSP (key))
	key = Fcons (XCAR (key), XCDR (key));
      sequence = append_key (this, key);
    }

  if (!NILP (where_is_cache))
    {
      Lisp_Object sequences = Fgethash (binding, where_is_cache, Qnil);
      Fputhash (binding, Fcons (sequence, sequences), where_is_cache);
    }
  else
    d->sequences = Fcons (sequence, d->sequences);
}

/* describe-bindings - summarizing all the bindings in a set of keymaps.  */

DEFUN ("describe-buffer-bindings", Fdescribe_buffer_bindings, Sdescribe_buffer_bindings, 1, 3, 0,
       doc: /* Insert the list of all defined keys and their definitions.
The list is inserted in the current buffer, while the bindings are
looked up in BUFFER.
The optional argument PREFIX, if non-nil, should be a key sequence;
then we display only bindings that start with that prefix.
The optional argument MENUS, if non-nil, says to mention menu bindings.
\(Ordinarily these are omitted from the output.)  */)
  (Lisp_Object buffer, Lisp_Object prefix, Lisp_Object menus)
{
  Lisp_Object outbuf, shadow;
  int nomenu = NILP (menus);
  register Lisp_Object start1;
  struct gcpro gcpro1;

  const char *alternate_heading
    = "\
Keyboard translations:\n\n\
You type        Translation\n\
--------        -----------\n";

  CHECK_BUFFER (buffer);

  shadow = Qnil;
  GCPRO1 (shadow);

  outbuf = Fcurrent_buffer ();

  /* Report on alternates for keys.  */
  if (STRINGP (KVAR (current_kboard, Vkeyboard_translate_table)) && !NILP (prefix))
    {
      int c;
      const unsigned char *translate = SDATA (KVAR (current_kboard, Vkeyboard_translate_table));
      int translate_len = SCHARS (KVAR (current_kboard, Vkeyboard_translate_table));

      for (c = 0; c < translate_len; c++)
	if (translate[c] != c)
	  {
	    char buf[KEY_DESCRIPTION_SIZE];
	    char *bufend;

	    if (alternate_heading)
	      {
		insert_string (alternate_heading);
		alternate_heading = 0;
	      }

	    bufend = push_key_description (translate[c], buf, 1);
	    insert (buf, bufend - buf);
	    Findent_to (make_number (16), make_number (1));
	    bufend = push_key_description (c, buf, 1);
	    insert (buf, bufend - buf);

	    insert ("\n", 1);

	    /* Insert calls signal_after_change which may GC. */
	    translate = SDATA (KVAR (current_kboard, Vkeyboard_translate_table));
	  }

      insert ("\n", 1);
    }

  if (!NILP (Vkey_translation_map))
    describe_map_tree (Vkey_translation_map, 0, Qnil, prefix,
		       "Key translations", nomenu, 1, 0, 0);


  /* Print the (major mode) local map.  */
  start1 = Qnil;
  if (!NILP (KVAR (current_kboard, Voverriding_terminal_local_map)))
    start1 = KVAR (current_kboard, Voverriding_terminal_local_map);
  else if (!NILP (Voverriding_local_map))
    start1 = Voverriding_local_map;

  if (!NILP (start1))
    {
      describe_map_tree (start1, 1, shadow, prefix,
			 "\f\nOverriding Bindings", nomenu, 0, 0, 0);
      shadow = Fcons (start1, shadow);
    }
  else
    {
      /* Print the minor mode and major mode keymaps.  */
      int i, nmaps;
      Lisp_Object *modes, *maps;

      /* Temporarily switch to `buffer', so that we can get that buffer's
	 minor modes correctly.  */
      Fset_buffer (buffer);

      nmaps = current_minor_maps (&modes, &maps);
      Fset_buffer (outbuf);

      start1 = get_local_map (BUF_PT (XBUFFER (buffer)),
			      XBUFFER (buffer), Qkeymap);
      if (!NILP (start1))
	{
	  describe_map_tree (start1, 1, shadow, prefix,
			     "\f\n`keymap' Property Bindings", nomenu,
			     0, 0, 0);
	  shadow = Fcons (start1, shadow);
	}

      /* Print the minor mode maps.  */
      for (i = 0; i < nmaps; i++)
	{
	  /* The title for a minor mode keymap
	     is constructed at run time.
	     We let describe_map_tree do the actual insertion
	     because it takes care of other features when doing so.  */
	  char *title, *p;

	  if (!SYMBOLP (modes[i]))
	    abort ();

	  p = title = (char *) alloca (42 + SCHARS (SYMBOL_NAME (modes[i])));
	  *p++ = '\f';
	  *p++ = '\n';
	  *p++ = '`';
	  memcpy (p, SDATA (SYMBOL_NAME (modes[i])),
		  SCHARS (SYMBOL_NAME (modes[i])));
	  p += SCHARS (SYMBOL_NAME (modes[i]));
	  *p++ = '\'';
	  memcpy (p, " Minor Mode Bindings", strlen (" Minor Mode Bindings"));
	  p += strlen (" Minor Mode Bindings");
	  *p = 0;

	  describe_map_tree (maps[i], 1, shadow, prefix,
			     title, nomenu, 0, 0, 0);
	  shadow = Fcons (maps[i], shadow);
	}

      start1 = get_local_map (BUF_PT (XBUFFER (buffer)),
			      XBUFFER (buffer), Qlocal_map);
      if (!NILP (start1))
	{
	  if (EQ (start1, BVAR (XBUFFER (buffer), keymap)))
	    describe_map_tree (start1, 1, shadow, prefix,
			       "\f\nMajor Mode Bindings", nomenu, 0, 0, 0);
	  else
	    describe_map_tree (start1, 1, shadow, prefix,
			       "\f\n`local-map' Property Bindings",
			       nomenu, 0, 0, 0);

	  shadow = Fcons (start1, shadow);
	}
    }

  describe_map_tree (current_global_map, 1, shadow, prefix,
		     "\f\nGlobal Bindings", nomenu, 0, 1, 0);

  /* Print the function-key-map translations under this prefix.  */
  if (!NILP (KVAR (current_kboard, Vlocal_function_key_map)))
    describe_map_tree (KVAR (current_kboard, Vlocal_function_key_map), 0, Qnil, prefix,
		       "\f\nFunction key map translations", nomenu, 1, 0, 0);

  /* Print the input-decode-map translations under this prefix.  */
  if (!NILP (KVAR (current_kboard, Vinput_decode_map)))
    describe_map_tree (KVAR (current_kboard, Vinput_decode_map), 0, Qnil, prefix,
		       "\f\nInput decoding map translations", nomenu, 1, 0, 0);

  UNGCPRO;
  return Qnil;
}

/* Insert a description of the key bindings in STARTMAP,
    followed by those of all maps reachable through STARTMAP.
   If PARTIAL is nonzero, omit certain "uninteresting" commands
    (such as `undefined').
   If SHADOW is non-nil, it is a list of maps;
    don't mention keys which would be shadowed by any of them.
   PREFIX, if non-nil, says mention only keys that start with PREFIX.
   TITLE, if not 0, is a string to insert at the beginning.
   TITLE should not end with a colon or a newline; we supply that.
   If NOMENU is not 0, then omit menu-bar commands.

   If TRANSL is nonzero, the definitions are actually key translations
   so print strings and vectors differently.

   If ALWAYS_TITLE is nonzero, print the title even if there are no maps
   to look through.

   If MENTION_SHADOW is nonzero, then when something is shadowed by SHADOW,
   don't omit it; instead, mention it but say it is shadowed.

   Any inserted text ends in two newlines (used by `help-make-xrefs').  */

void
describe_map_tree (Lisp_Object startmap, int partial, Lisp_Object shadow,
		   Lisp_Object prefix, const char *title, int nomenu, int transl,
		   int always_title, int mention_shadow)
{
  Lisp_Object maps, orig_maps, seen, sub_shadows;
  struct gcpro gcpro1, gcpro2, gcpro3;
  int something = 0;
  const char *key_heading
    = "\
key             binding\n\
---             -------\n";

  orig_maps = maps = Faccessible_keymaps (startmap, prefix);
  seen = Qnil;
  sub_shadows = Qnil;
  GCPRO3 (maps, seen, sub_shadows);

  if (nomenu)
    {
      Lisp_Object list;

      /* Delete from MAPS each element that is for the menu bar.  */
      for (list = maps; CONSP (list); list = XCDR (list))
	{
	  Lisp_Object elt, elt_prefix, tem;

	  elt = XCAR (list);
	  elt_prefix = Fcar (elt);
	  if (ASIZE (elt_prefix) >= 1)
	    {
	      tem = Faref (elt_prefix, make_number (0));
	      if (EQ (tem, Qmenu_bar))
		maps = Fdelq (elt, maps);
	    }
	}
    }

  if (!NILP (maps) || always_title)
    {
      if (title)
	{
	  insert_string (title);
	  if (!NILP (prefix))
	    {
	      insert_string (" Starting With ");
	      insert1 (Fkey_description (prefix, Qnil));
	    }
	  insert_string (":\n");
	}
      insert_string (key_heading);
      something = 1;
    }

  for (; CONSP (maps); maps = XCDR (maps))
    {
      register Lisp_Object elt, elt_prefix, tail;

      elt = XCAR (maps);
      elt_prefix = Fcar (elt);

      sub_shadows = Qnil;

      for (tail = shadow; CONSP (tail); tail = XCDR (tail))
	{
	  Lisp_Object shmap;

	  shmap = XCAR (tail);

	  /* If the sequence by which we reach this keymap is zero-length,
	     then the shadow map for this keymap is just SHADOW.  */
	  if ((STRINGP (elt_prefix) && SCHARS (elt_prefix) == 0)
	      || (VECTORP (elt_prefix) && ASIZE (elt_prefix) == 0))
	    ;
	  /* If the sequence by which we reach this keymap actually has
	     some elements, then the sequence's definition in SHADOW is
	     what we should use.  */
	  else
	    {
	      shmap = Flookup_key (shmap, Fcar (elt), Qt);
	      if (INTEGERP (shmap))
		shmap = Qnil;
	    }

	  /* If shmap is not nil and not a keymap,
	     it completely shadows this map, so don't
	     describe this map at all.  */
	  if (!NILP (shmap) && !KEYMAPP (shmap))
	    goto skip;

	  if (!NILP (shmap))
	    sub_shadows = Fcons (shmap, sub_shadows);
	}

      /* Maps we have already listed in this loop shadow this map.  */
      for (tail = orig_maps; !EQ (tail, maps); tail = XCDR (tail))
	{
	  Lisp_Object tem;
	  tem = Fequal (Fcar (XCAR (tail)), elt_prefix);
	  if (!NILP (tem))
	    sub_shadows = Fcons (XCDR (XCAR (tail)), sub_shadows);
	}

      describe_map (Fcdr (elt), elt_prefix,
		    transl ? describe_translation : describe_command,
		    partial, sub_shadows, &seen, nomenu, mention_shadow);

    skip: ;
    }

  if (something)
    insert_string ("\n");

  UNGCPRO;
}

static int previous_description_column;

static void
describe_command (Lisp_Object definition, Lisp_Object args)
{
  register Lisp_Object tem1;
  EMACS_INT column = current_column ();
  int description_column;

  /* If column 16 is no good, go to col 32;
     but don't push beyond that--go to next line instead.  */
  if (column > 30)
    {
      insert_char ('\n');
      description_column = 32;
    }
  else if (column > 14 || (column > 10 && previous_description_column == 32))
    description_column = 32;
  else
    description_column = 16;

  Findent_to (make_number (description_column), make_number (1));
  previous_description_column = description_column;

  if (SYMBOLP (definition))
    {
      tem1 = SYMBOL_NAME (definition);
      insert1 (tem1);
      insert_string ("\n");
    }
  else if (STRINGP (definition) || VECTORP (definition))
    insert_string ("Keyboard Macro\n");
  else if (KEYMAPP (definition))
    insert_string ("Prefix Command\n");
  else
    insert_string ("??\n");
}

static void
describe_translation (Lisp_Object definition, Lisp_Object args)
{
  register Lisp_Object tem1;

  Findent_to (make_number (16), make_number (1));

  if (SYMBOLP (definition))
    {
      tem1 = SYMBOL_NAME (definition);
      insert1 (tem1);
      insert_string ("\n");
    }
  else if (STRINGP (definition) || VECTORP (definition))
    {
      insert1 (Fkey_description (definition, Qnil));
      insert_string ("\n");
    }
  else if (KEYMAPP (definition))
    insert_string ("Prefix Command\n");
  else
    insert_string ("??\n");
}

/* describe_map puts all the usable elements of a sparse keymap
   into an array of `struct describe_map_elt',
   then sorts them by the events.  */

struct describe_map_elt { Lisp_Object event; Lisp_Object definition; int shadowed; };

/* qsort comparison function for sorting `struct describe_map_elt' by
   the event field.  */

static int
describe_map_compare (const void *aa, const void *bb)
{
  const struct describe_map_elt *a = aa, *b = bb;
  if (INTEGERP (a->event) && INTEGERP (b->event))
    return ((XINT (a->event) > XINT (b->event))
	    - (XINT (a->event) < XINT (b->event)));
  if (!INTEGERP (a->event) && INTEGERP (b->event))
    return 1;
  if (INTEGERP (a->event) && !INTEGERP (b->event))
    return -1;
  if (SYMBOLP (a->event) && SYMBOLP (b->event))
    return (!NILP (Fstring_lessp (a->event, b->event)) ? -1
	    : !NILP (Fstring_lessp (b->event, a->event)) ? 1
	    : 0);
  return 0;
}

/* Describe the contents of map MAP, assuming that this map itself is
   reached by the sequence of prefix keys PREFIX (a string or vector).
   PARTIAL, SHADOW, NOMENU are as in `describe_map_tree' above.  */

static void
describe_map (Lisp_Object map, Lisp_Object prefix,
	      void (*elt_describer) (Lisp_Object, Lisp_Object),
	      int partial, Lisp_Object shadow,
	      Lisp_Object *seen, int nomenu, int mention_shadow)
{
  Lisp_Object tail, definition, event;
  Lisp_Object tem;
  Lisp_Object suppress;
  Lisp_Object kludge;
  int first = 1;
  struct gcpro gcpro1, gcpro2, gcpro3;

  /* These accumulate the values from sparse keymap bindings,
     so we can sort them and handle them in order.  */
  int length_needed = 0;
  struct describe_map_elt *vect;
  int slots_used = 0;
  int i;

  suppress = Qnil;

  if (partial)
    suppress = intern ("suppress-keymap");

  /* This vector gets used to present single keys to Flookup_key.  Since
     that is done once per keymap element, we don't want to cons up a
     fresh vector every time.  */
  kludge = Fmake_vector (make_number (1), Qnil);
  definition = Qnil;

  GCPRO3 (prefix, definition, kludge);

  map = call1 (Qkeymap_canonicalize, map);

  for (tail = map; CONSP (tail); tail = XCDR (tail))
    length_needed++;

  vect = ((struct describe_map_elt *)
	  alloca (sizeof (struct describe_map_elt) * length_needed));

  for (tail = map; CONSP (tail); tail = XCDR (tail))
    {
      QUIT;

      if (VECTORP (XCAR (tail))
	  || CHAR_TABLE_P (XCAR (tail)))
	describe_vector (XCAR (tail),
			 prefix, Qnil, elt_describer, partial, shadow, map,
			 1, mention_shadow);
      else if (CONSP (XCAR (tail)))
	{
	  int this_shadowed = 0;

	  event = XCAR (XCAR (tail));

	  /* Ignore bindings whose "prefix" are not really valid events.
	     (We get these in the frames and buffers menu.)  */
	  if (!(SYMBOLP (event) || INTEGERP (event)))
	    continue;

	  if (nomenu && EQ (event, Qmenu_bar))
	    continue;

	  definition = get_keyelt (XCDR (XCAR (tail)), 0);

	  /* Don't show undefined commands or suppressed commands.  */
	  if (NILP (definition)) continue;
	  if (SYMBOLP (definition) && partial)
	    {
	      tem = Fget (definition, suppress);
	      if (!NILP (tem))
		continue;
	    }

	  /* Don't show a command that isn't really visible
	     because a local definition of the same key shadows it.  */

	  ASET (kludge, 0, event);
	  if (!NILP (shadow))
	    {
	      tem = shadow_lookup (shadow, kludge, Qt, 0);
	      if (!NILP (tem))
		{
		  /* If both bindings are keymaps, this key is a prefix key,
		     so don't say it is shadowed.  */
		  if (KEYMAPP (definition) && KEYMAPP (tem))
		    ;
		  /* Avoid generating duplicate entries if the
		     shadowed binding has the same definition.  */
		  else if (mention_shadow && !EQ (tem, definition))
		    this_shadowed = 1;
		  else
		    continue;
		}
	    }

	  tem = Flookup_key (map, kludge, Qt);
	  if (!EQ (tem, definition)) continue;

	  vect[slots_used].event = event;
	  vect[slots_used].definition = definition;
	  vect[slots_used].shadowed = this_shadowed;
	  slots_used++;
	}
      else if (EQ (XCAR (tail), Qkeymap))
	{
	  /* The same keymap might be in the structure twice, if we're
	     using an inherited keymap.  So skip anything we've already
	     encountered.  */
	  tem = Fassq (tail, *seen);
	  if (CONSP (tem) && !NILP (Fequal (XCAR (tem), prefix)))
	    break;
	  *seen = Fcons (Fcons (tail, prefix), *seen);
	}
    }

  /* If we found some sparse map events, sort them.  */

  qsort (vect, slots_used, sizeof (struct describe_map_elt),
	 describe_map_compare);

  /* Now output them in sorted order.  */

  for (i = 0; i < slots_used; i++)
    {
      Lisp_Object start, end;

      if (first)
	{
	  previous_description_column = 0;
	  insert ("\n", 1);
	  first = 0;
	}

      ASET (kludge, 0, vect[i].event);
      start = vect[i].event;
      end = start;

      definition = vect[i].definition;

      /* Find consecutive chars that are identically defined.  */
      if (INTEGERP (vect[i].event))
	{
	  while (i + 1 < slots_used
		 && EQ (vect[i+1].event, make_number (XINT (vect[i].event) + 1))
		 && !NILP (Fequal (vect[i + 1].definition, definition))
		 && vect[i].shadowed == vect[i + 1].shadowed)
	    i++;
	  end = vect[i].event;
	}

      /* Now START .. END is the range to describe next.  */

      /* Insert the string to describe the event START.  */
      insert1 (Fkey_description (kludge, prefix));

      if (!EQ (start, end))
	{
	  insert (" .. ", 4);

	  ASET (kludge, 0, end);
	  /* Insert the string to describe the character END.  */
	  insert1 (Fkey_description (kludge, prefix));
	}

      /* Print a description of the definition of this character.
	 elt_describer will take care of spacing out far enough
	 for alignment purposes.  */
      (*elt_describer) (vect[i].definition, Qnil);

      if (vect[i].shadowed)
	{
	  SET_PT (PT - 1);
	  insert_string ("\n  (that binding is currently shadowed by another mode)");
	  SET_PT (PT + 1);
	}
    }

  UNGCPRO;
}

static void
describe_vector_princ (Lisp_Object elt, Lisp_Object fun)
{
  Findent_to (make_number (16), make_number (1));
  call1 (fun, elt);
  Fterpri (Qnil);
}

DEFUN ("describe-vector", Fdescribe_vector, Sdescribe_vector, 1, 2, 0,
       doc: /* Insert a description of contents of VECTOR.
This is text showing the elements of vector matched against indices.
DESCRIBER is the output function used; nil means use `princ'.  */)
  (Lisp_Object vector, Lisp_Object describer)
{
  int count = SPECPDL_INDEX ();
  if (NILP (describer))
    describer = intern ("princ");
  specbind (Qstandard_output, Fcurrent_buffer ());
  CHECK_VECTOR_OR_CHAR_TABLE (vector);
  describe_vector (vector, Qnil, describer, describe_vector_princ, 0,
		   Qnil, Qnil, 0, 0);

  return unbind_to (count, Qnil);
}

/* Insert in the current buffer a description of the contents of VECTOR.
   We call ELT_DESCRIBER to insert the description of one value found
   in VECTOR.

   ELT_PREFIX describes what "comes before" the keys or indices defined
   by this vector.  This is a human-readable string whose size
   is not necessarily related to the situation.

   If the vector is in a keymap, ELT_PREFIX is a prefix key which
   leads to this keymap.

   If the vector is a chartable, ELT_PREFIX is the vector
   of bytes that lead to the character set or portion of a character
   set described by this chartable.

   If PARTIAL is nonzero, it means do not mention suppressed commands
   (that assumes the vector is in a keymap).

   SHADOW is a list of keymaps that shadow this map.
   If it is non-nil, then we look up the key in those maps
   and we don't mention it now if it is defined by any of them.

   ENTIRE_MAP is the keymap in which this vector appears.
   If the definition in effect in the whole map does not match
   the one in this vector, we ignore this one.

   ARGS is simply passed as the second argument to ELT_DESCRIBER.

   KEYMAP_P is 1 if vector is known to be a keymap, so map ESC to M-.

   ARGS is simply passed as the second argument to ELT_DESCRIBER.  */

static void
describe_vector (Lisp_Object vector, Lisp_Object prefix, Lisp_Object args,
		 void (*elt_describer) (Lisp_Object, Lisp_Object),
		 int partial, Lisp_Object shadow, Lisp_Object entire_map,
		 int keymap_p, int mention_shadow)
{
  Lisp_Object definition;
  Lisp_Object tem2;
  Lisp_Object elt_prefix = Qnil;
  int i;
  Lisp_Object suppress;
  Lisp_Object kludge;
  int first = 1;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;
  /* Range of elements to be handled.  */
  int from, to, stop;
  Lisp_Object character;
  int starting_i;

  suppress = Qnil;

  definition = Qnil;

  if (!keymap_p)
    {
      /* Call Fkey_description first, to avoid GC bug for the other string.  */
      if (!NILP (prefix) && XFASTINT (Flength (prefix)) > 0)
	{
	  Lisp_Object tem;
	  tem = Fkey_description (prefix, Qnil);
	  elt_prefix = concat2 (tem, build_string (" "));
	}
      prefix = Qnil;
    }

  /* This vector gets used to present single keys to Flookup_key.  Since
     that is done once per vector element, we don't want to cons up a
     fresh vector every time.  */
  kludge = Fmake_vector (make_number (1), Qnil);
  GCPRO4 (elt_prefix, prefix, definition, kludge);

  if (partial)
    suppress = intern ("suppress-keymap");

  from = 0;
  if (CHAR_TABLE_P (vector))
    stop = MAX_5_BYTE_CHAR + 1, to = MAX_CHAR + 1;
  else
    stop = to = ASIZE (vector);

  for (i = from; ; i++)
    {
      int this_shadowed = 0;
      int range_beg, range_end;
      Lisp_Object val;

      QUIT;

      if (i == stop)
	{
	  if (i == to)
	    break;
	  stop = to;
	}

      starting_i = i;

      if (CHAR_TABLE_P (vector))
	{
	  range_beg = i;
	  i = stop - 1;
	  val = char_table_ref_and_range (vector, range_beg, &range_beg, &i);
	}
      else
	val = AREF (vector, i);
      definition = get_keyelt (val, 0);

      if (NILP (definition)) continue;

      /* Don't mention suppressed commands.  */
      if (SYMBOLP (definition) && partial)
	{
	  Lisp_Object tem;

	  tem = Fget (definition, suppress);

	  if (!NILP (tem)) continue;
	}

      character = make_number (starting_i);
      ASET (kludge, 0, character);

      /* If this binding is shadowed by some other map, ignore it.  */
      if (!NILP (shadow))
	{
	  Lisp_Object tem;

	  tem = shadow_lookup (shadow, kludge, Qt, 0);

	  if (!NILP (tem))
	    {
	      if (mention_shadow)
		this_shadowed = 1;
	      else
		continue;
	    }
	}

      /* Ignore this definition if it is shadowed by an earlier
	 one in the same keymap.  */
      if (!NILP (entire_map))
	{
	  Lisp_Object tem;

	  tem = Flookup_key (entire_map, kludge, Qt);

	  if (!EQ (tem, definition))
	    continue;
	}

      if (first)
	{
	  insert ("\n", 1);
	  first = 0;
	}

      /* Output the prefix that applies to every entry in this map.  */
      if (!NILP (elt_prefix))
	insert1 (elt_prefix);

      insert1 (Fkey_description (kludge, prefix));

      /* Find all consecutive characters or rows that have the same
         definition.  But, VECTOR is a char-table, we had better put a
         boundary between normal characters (-#x3FFF7F) and 8-bit
         characters (#x3FFF80-).  */
      if (CHAR_TABLE_P (vector))
	{
	  while (i + 1 < stop
		 && (range_beg = i + 1, range_end = stop - 1,
		   val = char_table_ref_and_range (vector, range_beg,
						   &range_beg, &range_end),
		   tem2 = get_keyelt (val, 0),
		   !NILP (tem2))
		 && !NILP (Fequal (tem2, definition)))
	    i = range_end;
	}
      else
	while (i + 1 < stop
	       && (tem2 = get_keyelt (AREF (vector, i + 1), 0),
		   !NILP (tem2))
	       && !NILP (Fequal (tem2, definition)))
	  i++;

      /* If we have a range of more than one character,
	 print where the range reaches to.  */

      if (i != starting_i)
	{
	  insert (" .. ", 4);

	  ASET (kludge, 0, make_number (i));

	  if (!NILP (elt_prefix))
	    insert1 (elt_prefix);

	  insert1 (Fkey_description (kludge, prefix));
	}

      /* Print a description of the definition of this character.
	 elt_describer will take care of spacing out far enough
	 for alignment purposes.  */
      (*elt_describer) (definition, args);

      if (this_shadowed)
	{
	  SET_PT (PT - 1);
	  insert_string ("  (binding currently shadowed)");
	  SET_PT (PT + 1);
	}
    }

  if (CHAR_TABLE_P (vector) && ! NILP (XCHAR_TABLE (vector)->defalt))
    {
      if (!NILP (elt_prefix))
	insert1 (elt_prefix);
      insert ("default", 7);
      (*elt_describer) (XCHAR_TABLE (vector)->defalt, args);
    }

  UNGCPRO;
}

/* Apropos - finding all symbols whose names match a regexp.		*/
static Lisp_Object apropos_predicate;
static Lisp_Object apropos_accumulate;

static void
apropos_accum (Lisp_Object symbol, Lisp_Object string)
{
  register Lisp_Object tem;

  tem = Fstring_match (string, Fsymbol_name (symbol), Qnil);
  if (!NILP (tem) && !NILP (apropos_predicate))
    tem = call1 (apropos_predicate, symbol);
  if (!NILP (tem))
    apropos_accumulate = Fcons (symbol, apropos_accumulate);
}

DEFUN ("apropos-internal", Fapropos_internal, Sapropos_internal, 1, 2, 0,
       doc: /* Show all symbols whose names contain match for REGEXP.
If optional 2nd arg PREDICATE is non-nil, (funcall PREDICATE SYMBOL) is done
for each symbol and a symbol is mentioned only if that returns non-nil.
Return list of symbols found.  */)
  (Lisp_Object regexp, Lisp_Object predicate)
{
  Lisp_Object tem;
  CHECK_STRING (regexp);
  apropos_predicate = predicate;
  apropos_accumulate = Qnil;
  map_obarray (Vobarray, apropos_accum, regexp);
  tem = Fsort (apropos_accumulate, Qstring_lessp);
  apropos_accumulate = Qnil;
  apropos_predicate = Qnil;
  return tem;
}

void
syms_of_keymap (void)
{
  DEFSYM (Qkeymap, "keymap");
  staticpro (&apropos_predicate);
  staticpro (&apropos_accumulate);
  apropos_predicate = Qnil;
  apropos_accumulate = Qnil;

  DEFSYM (Qkeymap_canonicalize, "keymap-canonicalize");

  /* Now we are ready to set up this property, so we can
     create char tables.  */
  Fput (Qkeymap, Qchar_table_extra_slots, make_number (0));

  /* Initialize the keymaps standardly used.
     Each one is the value of a Lisp variable, and is also
     pointed to by a C variable */

  global_map = Fmake_keymap (Qnil);
  Fset (intern_c_string ("global-map"), global_map);

  current_global_map = global_map;
  staticpro (&global_map);
  staticpro (&current_global_map);

  meta_map = Fmake_keymap (Qnil);
  Fset (intern_c_string ("esc-map"), meta_map);
  Ffset (intern_c_string ("ESC-prefix"), meta_map);

  control_x_map = Fmake_keymap (Qnil);
  Fset (intern_c_string ("ctl-x-map"), control_x_map);
  Ffset (intern_c_string ("Control-X-prefix"), control_x_map);

  exclude_keys
    = pure_cons (pure_cons (make_pure_c_string ("DEL"), make_pure_c_string ("\\d")),
		 pure_cons (pure_cons (make_pure_c_string ("TAB"), make_pure_c_string ("\\t")),
		    pure_cons (pure_cons (make_pure_c_string ("RET"), make_pure_c_string ("\\r")),
			   pure_cons (pure_cons (make_pure_c_string ("ESC"), make_pure_c_string ("\\e")),
				  pure_cons (pure_cons (make_pure_c_string ("SPC"), make_pure_c_string (" ")),
					 Qnil)))));
  staticpro (&exclude_keys);

  DEFVAR_LISP ("define-key-rebound-commands", Vdefine_key_rebound_commands,
	       doc: /* List of commands given new key bindings recently.
This is used for internal purposes during Emacs startup;
don't alter it yourself.  */);
  Vdefine_key_rebound_commands = Qt;

  DEFVAR_LISP ("minibuffer-local-map", Vminibuffer_local_map,
	       doc: /* Default keymap to use when reading from the minibuffer.  */);
  Vminibuffer_local_map = Fmake_sparse_keymap (Qnil);

  DEFVAR_LISP ("minibuffer-local-ns-map", Vminibuffer_local_ns_map,
	       doc: /* Local keymap for the minibuffer when spaces are not allowed.  */);
  Vminibuffer_local_ns_map = Fmake_sparse_keymap (Qnil);
  Fset_keymap_parent (Vminibuffer_local_ns_map, Vminibuffer_local_map);


  DEFVAR_LISP ("minor-mode-map-alist", Vminor_mode_map_alist,
	       doc: /* Alist of keymaps to use for minor modes.
Each element looks like (VARIABLE . KEYMAP); KEYMAP is used to read
key sequences and look up bindings if VARIABLE's value is non-nil.
If two active keymaps bind the same key, the keymap appearing earlier
in the list takes precedence.  */);
  Vminor_mode_map_alist = Qnil;

  DEFVAR_LISP ("minor-mode-overriding-map-alist", Vminor_mode_overriding_map_alist,
	       doc: /* Alist of keymaps to use for minor modes, in current major mode.
This variable is an alist just like `minor-mode-map-alist', and it is
used the same way (and before `minor-mode-map-alist'); however,
it is provided for major modes to bind locally.  */);
  Vminor_mode_overriding_map_alist = Qnil;

  DEFVAR_LISP ("emulation-mode-map-alists", Vemulation_mode_map_alists,
	       doc: /* List of keymap alists to use for emulations modes.
It is intended for modes or packages using multiple minor-mode keymaps.
Each element is a keymap alist just like `minor-mode-map-alist', or a
symbol with a variable binding which is a keymap alist, and it is used
the same way.  The "active" keymaps in each alist are used before
`minor-mode-map-alist' and `minor-mode-overriding-map-alist'.  */);
  Vemulation_mode_map_alists = Qnil;

  DEFVAR_LISP ("where-is-preferred-modifier", Vwhere_is_preferred_modifier,
	       doc: /* Preferred modifier key to use for `where-is'.
When a single binding is requested, `where-is' will return one that
uses this modifier key if possible.  If nil, or if no such binding
exists, bindings using keys without modifiers (or only with meta) will
be preferred.  */);
  Vwhere_is_preferred_modifier = Qnil;
  where_is_preferred_modifier = 0;

  staticpro (&Vmouse_events);
  Vmouse_events = pure_cons (intern_c_string ("menu-bar"),
		  pure_cons (intern_c_string ("tool-bar"),
		  pure_cons (intern_c_string ("header-line"),
		  pure_cons (intern_c_string ("mode-line"),
		  pure_cons (intern_c_string ("mouse-1"),
		  pure_cons (intern_c_string ("mouse-2"),
		  pure_cons (intern_c_string ("mouse-3"),
		  pure_cons (intern_c_string ("mouse-4"),
		  pure_cons (intern_c_string ("mouse-5"),
			     Qnil)))))))));

  DEFSYM (Qsingle_key_description, "single-key-description");
  DEFSYM (Qkey_description, "key-description");
  DEFSYM (Qkeymapp, "keymapp");
  DEFSYM (Qnon_ascii, "non-ascii");
  DEFSYM (Qmenu_item, "menu-item");
  DEFSYM (Qremap, "remap");
  DEFSYM (QCadvertised_binding, ":advertised-binding");

  command_remapping_vector = Fmake_vector (make_number (2), Qremap);
  staticpro (&command_remapping_vector);

  where_is_cache_keymaps = Qt;
  where_is_cache = Qnil;
  staticpro (&where_is_cache);
  staticpro (&where_is_cache_keymaps);

  defsubr (&Skeymapp);
  defsubr (&Skeymap_parent);
  defsubr (&Skeymap_prompt);
  defsubr (&Sset_keymap_parent);
  defsubr (&Smake_keymap);
  defsubr (&Smake_sparse_keymap);
  defsubr (&Smap_keymap_internal);
  defsubr (&Smap_keymap);
  defsubr (&Scopy_keymap);
  defsubr (&Scommand_remapping);
  defsubr (&Skey_binding);
  defsubr (&Slocal_key_binding);
  defsubr (&Sglobal_key_binding);
  defsubr (&Sminor_mode_key_binding);
  defsubr (&Sdefine_key);
  defsubr (&Slookup_key);
  defsubr (&Sdefine_prefix_command);
  defsubr (&Suse_global_map);
  defsubr (&Suse_local_map);
  defsubr (&Scurrent_local_map);
  defsubr (&Scurrent_global_map);
  defsubr (&Scurrent_minor_mode_maps);
  defsubr (&Scurrent_active_maps);
  defsubr (&Saccessible_keymaps);
  defsubr (&Skey_description);
  defsubr (&Sdescribe_vector);
  defsubr (&Ssingle_key_description);
  defsubr (&Stext_char_description);
  defsubr (&Swhere_is_internal);
  defsubr (&Sdescribe_buffer_bindings);
  defsubr (&Sapropos_internal);
}

void
keys_of_keymap (void)
{
  initial_define_key (global_map, 033, "ESC-prefix");
  initial_define_key (global_map, Ctl ('X'), "Control-X-prefix");
}
