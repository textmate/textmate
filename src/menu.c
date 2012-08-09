/* Platform-independent code for terminal communications.

Copyright (C) 1986, 1988, 1993-1994, 1996, 1999-2012
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
#include <limits.h> /* for INT_MAX */

#include "lisp.h"
#include "keyboard.h"
#include "keymap.h"
#include "frame.h"
#include "window.h"
#include "termhooks.h"
#include "blockinput.h"
#include "dispextern.h"

#ifdef USE_X_TOOLKIT
#include "../lwlib/lwlib.h"
#endif

#ifdef HAVE_X_WINDOWS
#include "xterm.h"
#endif

#ifdef HAVE_NS
#include "nsterm.h"
#endif

#ifdef USE_GTK
#include "gtkutil.h"
#endif

#ifdef HAVE_NTGUI
#include "w32term.h"

extern AppendMenuW_Proc unicode_append_menu;
extern HMENU current_popup_menu;

#endif /* HAVE_NTGUI  */

#include "menu.h"

/* Define HAVE_BOXES if menus can handle radio and toggle buttons.  */
#if defined (USE_X_TOOLKIT) || defined (USE_GTK) || defined (HAVE_NTGUI)
#define HAVE_BOXES 1
#endif

Lisp_Object menu_items;

/* If non-nil, means that the global vars defined here are already in use.
   Used to detect cases where we try to re-enter this non-reentrant code.  */
#if ! (defined USE_GTK || defined USE_MOTIF)
static
#endif
Lisp_Object menu_items_inuse;

/* Number of slots currently allocated in menu_items.  */
int menu_items_allocated;

/* This is the index in menu_items of the first empty slot.  */
int menu_items_used;

/* The number of panes currently recorded in menu_items,
   excluding those within submenus.  */
int menu_items_n_panes;

/* Current depth within submenus.  */
static int menu_items_submenu_depth;

void
init_menu_items (void)
{
  if (!NILP (menu_items_inuse))
    error ("Trying to use a menu from within a menu-entry");

  if (NILP (menu_items))
    {
      menu_items_allocated = 60;
      menu_items = Fmake_vector (make_number (menu_items_allocated), Qnil);
    }

  menu_items_inuse = Qt;
  menu_items_used = 0;
  menu_items_n_panes = 0;
  menu_items_submenu_depth = 0;
}

/* Call at the end of generating the data in menu_items.  */

void
finish_menu_items (void)
{
}

Lisp_Object
unuse_menu_items (Lisp_Object dummy)
{
  return menu_items_inuse = Qnil;
}

/* Call when finished using the data for the current menu
   in menu_items.  */

void
discard_menu_items (void)
{
  /* Free the structure if it is especially large.
     Otherwise, hold on to it, to save time.  */
  if (menu_items_allocated > 200)
    {
      menu_items = Qnil;
      menu_items_allocated = 0;
    }
  xassert (NILP (menu_items_inuse));
}

#ifdef HAVE_NS
static Lisp_Object
cleanup_popup_menu (Lisp_Object arg)
{
  discard_menu_items ();
  return Qnil;
}
#endif

/* This undoes save_menu_items, and it is called by the specpdl unwind
   mechanism.  */

static Lisp_Object
restore_menu_items (Lisp_Object saved)
{
  menu_items = XCAR (saved);
  menu_items_inuse = (! NILP (menu_items) ? Qt : Qnil);
  menu_items_allocated = (VECTORP (menu_items) ? ASIZE (menu_items) : 0);
  saved = XCDR (saved);
  menu_items_used = XINT (XCAR (saved));
  saved = XCDR (saved);
  menu_items_n_panes = XINT (XCAR (saved));
  saved = XCDR (saved);
  menu_items_submenu_depth = XINT (XCAR (saved));
  return Qnil;
}

/* Push the whole state of menu_items processing onto the specpdl.
   It will be restored when the specpdl is unwound.  */

void
save_menu_items (void)
{
  Lisp_Object saved = list4 (!NILP (menu_items_inuse) ? menu_items : Qnil,
			     make_number (menu_items_used),
			     make_number (menu_items_n_panes),
			     make_number (menu_items_submenu_depth));
  record_unwind_protect (restore_menu_items, saved);
  menu_items_inuse = Qnil;
  menu_items = Qnil;
}


/* Make the menu_items vector twice as large.  */

static void
grow_menu_items (void)
{
  if ((INT_MAX - MENU_ITEMS_PANE_LENGTH) / 2 < menu_items_allocated)
    memory_full (SIZE_MAX);
  menu_items_allocated *= 2;
  menu_items = larger_vector (menu_items, menu_items_allocated, Qnil);
}

#if (defined USE_X_TOOLKIT || defined USE_GTK || defined HAVE_NS \
     || defined HAVE_NTGUI)

/* Begin a submenu.  */

static void
push_submenu_start (void)
{
  if (menu_items_used + 1 > menu_items_allocated)
    grow_menu_items ();

  XVECTOR (menu_items)->contents[menu_items_used++] = Qnil;
  menu_items_submenu_depth++;
}

/* End a submenu.  */

static void
push_submenu_end (void)
{
  if (menu_items_used + 1 > menu_items_allocated)
    grow_menu_items ();

  XVECTOR (menu_items)->contents[menu_items_used++] = Qlambda;
  menu_items_submenu_depth--;
}

#endif /* USE_X_TOOLKIT || USE_GTK || HAVE_NS || defined HAVE_NTGUI */

/* Indicate boundary between left and right.  */

static void
push_left_right_boundary (void)
{
  if (menu_items_used + 1 > menu_items_allocated)
    grow_menu_items ();

  XVECTOR (menu_items)->contents[menu_items_used++] = Qquote;
}

/* Start a new menu pane in menu_items.
   NAME is the pane name.  PREFIX_VEC is a prefix key for this pane.  */

static void
push_menu_pane (Lisp_Object name, Lisp_Object prefix_vec)
{
  if (menu_items_used + MENU_ITEMS_PANE_LENGTH > menu_items_allocated)
    grow_menu_items ();

  if (menu_items_submenu_depth == 0)
    menu_items_n_panes++;
  XVECTOR (menu_items)->contents[menu_items_used++] = Qt;
  XVECTOR (menu_items)->contents[menu_items_used++] = name;
  XVECTOR (menu_items)->contents[menu_items_used++] = prefix_vec;
}

/* Push one menu item into the current pane.  NAME is the string to
   display.  ENABLE if non-nil means this item can be selected.  KEY
   is the key generated by choosing this item, or nil if this item
   doesn't really have a definition.  DEF is the definition of this
   item.  EQUIV is the textual description of the keyboard equivalent
   for this item (or nil if none).  TYPE is the type of this menu
   item, one of nil, `toggle' or `radio'. */

static void
push_menu_item (Lisp_Object name, Lisp_Object enable, Lisp_Object key, Lisp_Object def, Lisp_Object equiv, Lisp_Object type, Lisp_Object selected, Lisp_Object help)
{
  if (menu_items_used + MENU_ITEMS_ITEM_LENGTH > menu_items_allocated)
    grow_menu_items ();

  ASET (menu_items, menu_items_used + MENU_ITEMS_ITEM_NAME,	name);
  ASET (menu_items, menu_items_used + MENU_ITEMS_ITEM_ENABLE,	enable);
  ASET (menu_items, menu_items_used + MENU_ITEMS_ITEM_VALUE,	key);
  ASET (menu_items, menu_items_used + MENU_ITEMS_ITEM_EQUIV_KEY, equiv);
  ASET (menu_items, menu_items_used + MENU_ITEMS_ITEM_DEFINITION, def);
  ASET (menu_items, menu_items_used + MENU_ITEMS_ITEM_TYPE,	type);
  ASET (menu_items, menu_items_used + MENU_ITEMS_ITEM_SELECTED,	selected);
  ASET (menu_items, menu_items_used + MENU_ITEMS_ITEM_HELP,	help);

  menu_items_used += MENU_ITEMS_ITEM_LENGTH;
}

/* Args passed between single_keymap_panes and single_menu_item.  */
struct skp
  {
     Lisp_Object pending_maps;
     int maxdepth;
     int notbuttons;
  };

static void single_menu_item (Lisp_Object, Lisp_Object, Lisp_Object,
                              void *);

/* This is a recursive subroutine of keymap_panes.
   It handles one keymap, KEYMAP.
   The other arguments are passed along
   or point to local variables of the previous function.

   If we encounter submenus deeper than MAXDEPTH levels, ignore them.  */

static void
single_keymap_panes (Lisp_Object keymap, Lisp_Object pane_name,
		     Lisp_Object prefix, int maxdepth)
{
  struct skp skp;
  struct gcpro gcpro1;

  skp.pending_maps = Qnil;
  skp.maxdepth = maxdepth;
  skp.notbuttons = 0;

  if (maxdepth <= 0)
    return;

  push_menu_pane (pane_name, prefix);

#ifndef HAVE_BOXES
  /* Remember index for first item in this pane so we can go back and
     add a prefix when (if) we see the first button.  After that, notbuttons
     is set to 0, to mark that we have seen a button and all non button
     items need a prefix.  */
  skp.notbuttons = menu_items_used;
#endif

  GCPRO1 (skp.pending_maps);
  map_keymap_canonical (keymap, single_menu_item, Qnil, &skp);
  UNGCPRO;

  /* Process now any submenus which want to be panes at this level.  */
  while (CONSP (skp.pending_maps))
    {
      Lisp_Object elt, eltcdr, string;
      elt = XCAR (skp.pending_maps);
      eltcdr = XCDR (elt);
      string = XCAR (eltcdr);
      /* We no longer discard the @ from the beginning of the string here.
	 Instead, we do this in *menu_show.  */
      single_keymap_panes (Fcar (elt), string, XCDR (eltcdr), maxdepth - 1);
      skp.pending_maps = XCDR (skp.pending_maps);
    }
}

/* This is a subroutine of single_keymap_panes that handles one
   keymap entry.
   KEY is a key in a keymap and ITEM is its binding.
   SKP->PENDING_MAPS_PTR is a list of keymaps waiting to be made into
   separate panes.
   If we encounter submenus deeper than SKP->MAXDEPTH levels, ignore them.  */

static void
single_menu_item (Lisp_Object key, Lisp_Object item, Lisp_Object dummy, void *skp_v)
{
  Lisp_Object map, item_string, enabled;
  struct gcpro gcpro1, gcpro2;
  int res;
  struct skp *skp = skp_v;

  /* Parse the menu item and leave the result in item_properties.  */
  GCPRO2 (key, item);
  res = parse_menu_item (item, 0);
  UNGCPRO;
  if (!res)
    return;			/* Not a menu item.  */

  map = XVECTOR (item_properties)->contents[ITEM_PROPERTY_MAP];

  enabled = XVECTOR (item_properties)->contents[ITEM_PROPERTY_ENABLE];
  item_string = XVECTOR (item_properties)->contents[ITEM_PROPERTY_NAME];

  if (!NILP (map) && SREF (item_string, 0) == '@')
    {
      if (!NILP (enabled))
	/* An enabled separate pane. Remember this to handle it later.  */
	skp->pending_maps = Fcons (Fcons (map, Fcons (item_string, key)),
				   skp->pending_maps);
      return;
    }

#if defined (HAVE_X_WINDOWS) || defined (MSDOS)
#ifndef HAVE_BOXES
  /* Simulate radio buttons and toggle boxes by putting a prefix in
     front of them.  */
  {
    Lisp_Object prefix = Qnil;
    Lisp_Object type = XVECTOR (item_properties)->contents[ITEM_PROPERTY_TYPE];
    if (!NILP (type))
      {
	Lisp_Object selected
	  = XVECTOR (item_properties)->contents[ITEM_PROPERTY_SELECTED];

	if (skp->notbuttons)
	  /* The first button. Line up previous items in this menu.  */
	  {
	    int idx = skp->notbuttons; /* Index for first item this menu.  */
	    int submenu = 0;
	    Lisp_Object tem;
	    while (idx < menu_items_used)
	      {
		tem
		  = XVECTOR (menu_items)->contents[idx + MENU_ITEMS_ITEM_NAME];
		if (NILP (tem))
		  {
		    idx++;
		    submenu++;		/* Skip sub menu.  */
		  }
		else if (EQ (tem, Qlambda))
		  {
		    idx++;
		    submenu--;		/* End sub menu.  */
		  }
		else if (EQ (tem, Qt))
		  idx += 3;		/* Skip new pane marker. */
		else if (EQ (tem, Qquote))
		  idx++;		/* Skip a left, right divider. */
		else
		  {
		    if (!submenu && SREF (tem, 0) != '\0'
			&& SREF (tem, 0) != '-')
		      XVECTOR (menu_items)->contents[idx + MENU_ITEMS_ITEM_NAME]
			= concat2 (build_string ("    "), tem);
		    idx += MENU_ITEMS_ITEM_LENGTH;
		  }
	      }
	    skp->notbuttons = 0;
	  }

	/* Calculate prefix, if any, for this item.  */
	if (EQ (type, QCtoggle))
	  prefix = build_string (NILP (selected) ? "[ ] " : "[X] ");
	else if (EQ (type, QCradio))
	  prefix = build_string (NILP (selected) ? "( ) " : "(*) ");
      }
    /* Not a button. If we have earlier buttons, then we need a prefix.  */
    else if (!skp->notbuttons && SREF (item_string, 0) != '\0'
	     && SREF (item_string, 0) != '-')
      prefix = build_string ("    ");

    if (!NILP (prefix))
      item_string = concat2 (prefix, item_string);
  }
#endif /* not HAVE_BOXES */

#if ! defined (USE_X_TOOLKIT) && ! defined (USE_GTK)
  if (!NILP (map))
    /* Indicate visually that this is a submenu.  */
    item_string = concat2 (item_string, build_string (" >"));
#endif

#endif /* HAVE_X_WINDOWS || MSDOS */

  push_menu_item (item_string, enabled, key,
		  XVECTOR (item_properties)->contents[ITEM_PROPERTY_DEF],
		  XVECTOR (item_properties)->contents[ITEM_PROPERTY_KEYEQ],
		  XVECTOR (item_properties)->contents[ITEM_PROPERTY_TYPE],
		  XVECTOR (item_properties)->contents[ITEM_PROPERTY_SELECTED],
		  XVECTOR (item_properties)->contents[ITEM_PROPERTY_HELP]);

#if defined (USE_X_TOOLKIT) || defined (USE_GTK) || defined (HAVE_NS) || defined (HAVE_NTGUI)
  /* Display a submenu using the toolkit.  */
  if (! (NILP (map) || NILP (enabled)))
    {
      push_submenu_start ();
      single_keymap_panes (map, Qnil, key, skp->maxdepth - 1);
      push_submenu_end ();
    }
#endif
}

/* Look through KEYMAPS, a vector of keymaps that is NMAPS long,
   and generate menu panes for them in menu_items.  */

static void
keymap_panes (Lisp_Object *keymaps, int nmaps)
{
  int mapno;

  init_menu_items ();

  /* Loop over the given keymaps, making a pane for each map.
     But don't make a pane that is empty--ignore that map instead.
     P is the number of panes we have made so far.  */
  for (mapno = 0; mapno < nmaps; mapno++)
    single_keymap_panes (keymaps[mapno],
			 Fkeymap_prompt (keymaps[mapno]), Qnil, 10);

  finish_menu_items ();
}


/* Push the items in a single pane defined by the alist PANE.  */
static void
list_of_items (Lisp_Object pane)
{
  Lisp_Object tail, item, item1;

  for (tail = pane; CONSP (tail); tail = XCDR (tail))
    {
      item = XCAR (tail);
      if (STRINGP (item))
	push_menu_item (ENCODE_MENU_STRING (item), Qnil, Qnil, Qt,
			Qnil, Qnil, Qnil, Qnil);
      else if (CONSP (item))
	{
	  item1 = XCAR (item);
	  CHECK_STRING (item1);
	  push_menu_item (ENCODE_MENU_STRING (item1), Qt, XCDR (item),
			  Qt, Qnil, Qnil, Qnil, Qnil);
	}
      else
	push_left_right_boundary ();

    }
}

/* Push all the panes and items of a menu described by the
   alist-of-alists MENU.
   This handles old-fashioned calls to x-popup-menu.  */
void
list_of_panes (Lisp_Object menu)
{
  Lisp_Object tail;

  init_menu_items ();

  for (tail = menu; CONSP (tail); tail = XCDR (tail))
    {
      Lisp_Object elt, pane_name, pane_data;
      elt = XCAR (tail);
      pane_name = Fcar (elt);
      CHECK_STRING (pane_name);
      push_menu_pane (ENCODE_MENU_STRING (pane_name), Qnil);
      pane_data = Fcdr (elt);
      CHECK_CONS (pane_data);
      list_of_items (pane_data);
    }

  finish_menu_items ();
}

/* Set up data in menu_items for a menu bar item
   whose event type is ITEM_KEY (with string ITEM_NAME)
   and whose contents come from the list of keymaps MAPS.  */
int
parse_single_submenu (Lisp_Object item_key, Lisp_Object item_name, Lisp_Object maps)
{
  Lisp_Object length;
  int len;
  Lisp_Object *mapvec;
  int i;
  int top_level_items = 0;

  length = Flength (maps);
  len = XINT (length);

  /* Convert the list MAPS into a vector MAPVEC.  */
  mapvec = (Lisp_Object *) alloca (len * sizeof (Lisp_Object));
  for (i = 0; i < len; i++)
    {
      mapvec[i] = Fcar (maps);
      maps = Fcdr (maps);
    }

  /* Loop over the given keymaps, making a pane for each map.
     But don't make a pane that is empty--ignore that map instead.  */
  for (i = 0; i < len; i++)
    {
      if (!KEYMAPP (mapvec[i]))
	{
	  /* Here we have a command at top level in the menu bar
	     as opposed to a submenu.  */
	  top_level_items = 1;
	  push_menu_pane (Qnil, Qnil);
	  push_menu_item (item_name, Qt, item_key, mapvec[i],
			  Qnil, Qnil, Qnil, Qnil);
	}
      else
	{
	  Lisp_Object prompt;
	  prompt = Fkeymap_prompt (mapvec[i]);
	  single_keymap_panes (mapvec[i],
			       !NILP (prompt) ? prompt : item_name,
			       item_key, 10);
	}
    }

  return top_level_items;
}


#if defined (USE_X_TOOLKIT) || defined (USE_GTK) || defined (HAVE_NS) || defined (HAVE_NTGUI)

/* Allocate a widget_value, blocking input.  */

widget_value *
xmalloc_widget_value (void)
{
  widget_value *value;

  BLOCK_INPUT;
  value = malloc_widget_value ();
  UNBLOCK_INPUT;

  return value;
}

/* This recursively calls free_widget_value on the tree of widgets.
   It must free all data that was malloc'ed for these widget_values.
   In Emacs, many slots are pointers into the data of Lisp_Strings, and
   must be left alone.  */

void
free_menubar_widget_value_tree (widget_value *wv)
{
  if (! wv) return;

  wv->name = wv->value = wv->key = (char *) 0xDEADBEEF;

  if (wv->contents && (wv->contents != (widget_value*)1))
    {
      free_menubar_widget_value_tree (wv->contents);
      wv->contents = (widget_value *) 0xDEADBEEF;
    }
  if (wv->next)
    {
      free_menubar_widget_value_tree (wv->next);
      wv->next = (widget_value *) 0xDEADBEEF;
    }
  BLOCK_INPUT;
  free_widget_value (wv);
  UNBLOCK_INPUT;
}

/* Create a tree of widget_value objects
   representing the panes and items
   in menu_items starting at index START, up to index END.  */

widget_value *
digest_single_submenu (int start, int end, int top_level_items)
{
  widget_value *wv, *prev_wv, *save_wv, *first_wv;
  int i;
  int submenu_depth = 0;
  widget_value **submenu_stack;
  int panes_seen = 0;

  submenu_stack
    = (widget_value **) alloca (menu_items_used * sizeof (widget_value *));
  wv = xmalloc_widget_value ();
  wv->name = "menu";
  wv->value = 0;
  wv->enabled = 1;
  wv->button_type = BUTTON_TYPE_NONE;
  wv->help = Qnil;
  first_wv = wv;
  save_wv = 0;
  prev_wv = 0;

  /* Loop over all panes and items made by the preceding call
     to parse_single_submenu and construct a tree of widget_value objects.
     Ignore the panes and items used by previous calls to
     digest_single_submenu, even though those are also in menu_items.  */
  i = start;
  while (i < end)
    {
      if (EQ (XVECTOR (menu_items)->contents[i], Qnil))
	{
	  submenu_stack[submenu_depth++] = save_wv;
	  save_wv = prev_wv;
	  prev_wv = 0;
	  i++;
	}
      else if (EQ (XVECTOR (menu_items)->contents[i], Qlambda))
	{
	  prev_wv = save_wv;
	  save_wv = submenu_stack[--submenu_depth];
	  i++;
	}
      else if (EQ (XVECTOR (menu_items)->contents[i], Qt)
	       && submenu_depth != 0)
	i += MENU_ITEMS_PANE_LENGTH;
      /* Ignore a nil in the item list.
	 It's meaningful only for dialog boxes.  */
      else if (EQ (XVECTOR (menu_items)->contents[i], Qquote))
	i += 1;
      else if (EQ (XVECTOR (menu_items)->contents[i], Qt))
	{
	  /* Create a new pane.  */
	  Lisp_Object pane_name;
	  const char *pane_string;

	  panes_seen++;

	  pane_name = XVECTOR (menu_items)->contents[i + MENU_ITEMS_PANE_NAME];

#ifdef HAVE_NTGUI
	  if (STRINGP (pane_name))
	    {
	      if (unicode_append_menu)
		/* Encode as UTF-8 for now.  */
		pane_name = ENCODE_UTF_8 (pane_name);
	      else if (STRING_MULTIBYTE (pane_name))
		pane_name = ENCODE_SYSTEM (pane_name);

	      ASET (menu_items, i + MENU_ITEMS_PANE_NAME, pane_name);
	    }
#elif defined (USE_LUCID) && defined (HAVE_XFT)
	  if (STRINGP (pane_name))
            {
              pane_name = ENCODE_UTF_8 (pane_name);
	      ASET (menu_items, i + MENU_ITEMS_PANE_NAME, pane_name);
            }
#elif !defined (HAVE_MULTILINGUAL_MENU)
	  if (STRINGP (pane_name) && STRING_MULTIBYTE (pane_name))
	    {
	      pane_name = ENCODE_MENU_STRING (pane_name);
	      ASET (menu_items, i + MENU_ITEMS_PANE_NAME, pane_name);
	    }
#endif

	  pane_string = (NILP (pane_name)
			 ? "" : SSDATA (pane_name));
	  /* If there is just one top-level pane, put all its items directly
	     under the top-level menu.  */
	  if (menu_items_n_panes == 1)
	    pane_string = "";

	  /* If the pane has a meaningful name,
	     make the pane a top-level menu item
	     with its items as a submenu beneath it.  */
	  if (strcmp (pane_string, ""))
	    {
	      wv = xmalloc_widget_value ();
	      if (save_wv)
		save_wv->next = wv;
	      else
		first_wv->contents = wv;
	      wv->lname = pane_name;
              /* Set value to 1 so update_submenu_strings can handle '@'  */
	      wv->value = (char *)1;
	      wv->enabled = 1;
	      wv->button_type = BUTTON_TYPE_NONE;
	      wv->help = Qnil;
	      save_wv = wv;
	    }
	  else
	    save_wv = first_wv;

	  prev_wv = 0;
	  i += MENU_ITEMS_PANE_LENGTH;
	}
      else
	{
	  /* Create a new item within current pane.  */
	  Lisp_Object item_name, enable, descrip, def, type, selected;
	  Lisp_Object help;

	  /* All items should be contained in panes.  */
	  if (panes_seen == 0)
	    abort ();

	  item_name = AREF (menu_items, i + MENU_ITEMS_ITEM_NAME);
	  enable = AREF (menu_items, i + MENU_ITEMS_ITEM_ENABLE);
	  descrip = AREF (menu_items, i + MENU_ITEMS_ITEM_EQUIV_KEY);
	  def = AREF (menu_items, i + MENU_ITEMS_ITEM_DEFINITION);
	  type = AREF (menu_items, i + MENU_ITEMS_ITEM_TYPE);
	  selected = AREF (menu_items, i + MENU_ITEMS_ITEM_SELECTED);
	  help = AREF (menu_items, i + MENU_ITEMS_ITEM_HELP);

#ifdef HAVE_NTGUI
	  if (STRINGP (item_name))
	    {
	      if (unicode_append_menu)
		item_name = ENCODE_UTF_8 (item_name);
	      else if (STRING_MULTIBYTE (item_name))
		item_name = ENCODE_SYSTEM (item_name);

	      ASET (menu_items, i + MENU_ITEMS_ITEM_NAME, item_name);
	    }

	  if (STRINGP (descrip) && STRING_MULTIBYTE (descrip))
	    {
	      descrip = ENCODE_SYSTEM (descrip);
	      ASET (menu_items, i + MENU_ITEMS_ITEM_EQUIV_KEY, descrip);
	    }
#elif USE_LUCID
	  if (STRINGP (item_name))
	    {
              item_name = ENCODE_UTF_8 (item_name);
	      ASET (menu_items, i + MENU_ITEMS_ITEM_NAME, item_name);
	    }

	  if (STRINGP (descrip))
	    {
	      descrip = ENCODE_UTF_8 (descrip);
	      ASET (menu_items, i + MENU_ITEMS_ITEM_EQUIV_KEY, descrip);
	    }
#elif !defined (HAVE_MULTILINGUAL_MENU)
          if (STRING_MULTIBYTE (item_name))
	    {
	      item_name = ENCODE_MENU_STRING (item_name);
	      ASET (menu_items, i + MENU_ITEMS_ITEM_NAME, item_name);
	    }

          if (STRINGP (descrip) && STRING_MULTIBYTE (descrip))
	    {
	      descrip = ENCODE_MENU_STRING (descrip);
	      ASET (menu_items, i + MENU_ITEMS_ITEM_EQUIV_KEY, descrip);
	    }
#endif

	  wv = xmalloc_widget_value ();
	  if (prev_wv)
	    prev_wv->next = wv;
	  else
	    save_wv->contents = wv;

	  wv->lname = item_name;
	  if (!NILP (descrip))
	    wv->lkey = descrip;
	  wv->value = 0;
	  /* The intptr_t cast avoids a warning.  There's no problem
	     as long as pointers have enough bits to hold small integers.  */
	  wv->call_data = (!NILP (def) ? (void *) (intptr_t) i : 0);
	  wv->enabled = !NILP (enable);

	  if (NILP (type))
	    wv->button_type = BUTTON_TYPE_NONE;
	  else if (EQ (type, QCradio))
	    wv->button_type = BUTTON_TYPE_RADIO;
	  else if (EQ (type, QCtoggle))
	    wv->button_type = BUTTON_TYPE_TOGGLE;
	  else
	    abort ();

	  wv->selected = !NILP (selected);
	  if (! STRINGP (help))
	    help = Qnil;

	  wv->help = help;

	  prev_wv = wv;

	  i += MENU_ITEMS_ITEM_LENGTH;
	}
    }

  /* If we have just one "menu item"
     that was originally a button, return it by itself.  */
  if (top_level_items && first_wv->contents && first_wv->contents->next == 0)
    {
      wv = first_wv->contents;
      free_widget_value (first_wv);
      return wv;
    }

  return first_wv;
}

/* Walk through the widget_value tree starting at FIRST_WV and update
   the char * pointers from the corresponding lisp values.
   We do this after building the whole tree, since GC may happen while the
   tree is constructed, and small strings are relocated.  So we must wait
   until no GC can happen before storing pointers into lisp values.  */
void
update_submenu_strings (widget_value *first_wv)
{
  widget_value *wv;

  for (wv = first_wv; wv; wv = wv->next)
    {
      if (STRINGP (wv->lname))
        {
          wv->name = SSDATA (wv->lname);

          /* Ignore the @ that means "separate pane".
             This is a kludge, but this isn't worth more time.  */
          if (wv->value == (char *)1)
            {
              if (wv->name[0] == '@')
		wv->name++;
              wv->value = 0;
            }
        }

      if (STRINGP (wv->lkey))
        wv->key = SSDATA (wv->lkey);

      if (wv->contents)
        update_submenu_strings (wv->contents);
    }
}

/* Find the menu selection and store it in the keyboard buffer.
   F is the frame the menu is on.
   MENU_BAR_ITEMS_USED is the length of VECTOR.
   VECTOR is an array of menu events for the whole menu.  */

void
find_and_call_menu_selection (FRAME_PTR f, int menu_bar_items_used, Lisp_Object vector, void *client_data)
{
  Lisp_Object prefix, entry;
  Lisp_Object *subprefix_stack;
  int submenu_depth = 0;
  int i;

  entry = Qnil;
  subprefix_stack = (Lisp_Object *) alloca (menu_bar_items_used * sizeof (Lisp_Object));
  prefix = Qnil;
  i = 0;

  while (i < menu_bar_items_used)
    {
      if (EQ (XVECTOR (vector)->contents[i], Qnil))
	{
	  subprefix_stack[submenu_depth++] = prefix;
	  prefix = entry;
	  i++;
	}
      else if (EQ (XVECTOR (vector)->contents[i], Qlambda))
	{
	  prefix = subprefix_stack[--submenu_depth];
	  i++;
	}
      else if (EQ (XVECTOR (vector)->contents[i], Qt))
	{
	  prefix = XVECTOR (vector)->contents[i + MENU_ITEMS_PANE_PREFIX];
	  i += MENU_ITEMS_PANE_LENGTH;
	}
      else
	{
	  entry = XVECTOR (vector)->contents[i + MENU_ITEMS_ITEM_VALUE];
	  /* Treat the pointer as an integer.  There's no problem
	     as long as pointers have enough bits to hold small integers.  */
	  if ((intptr_t) client_data == i)
	    {
	      int j;
	      struct input_event buf;
	      Lisp_Object frame;
	      EVENT_INIT (buf);

	      XSETFRAME (frame, f);
	      buf.kind = MENU_BAR_EVENT;
	      buf.frame_or_window = frame;
	      buf.arg = frame;
	      kbd_buffer_store_event (&buf);

	      for (j = 0; j < submenu_depth; j++)
		if (!NILP (subprefix_stack[j]))
		  {
		    buf.kind = MENU_BAR_EVENT;
		    buf.frame_or_window = frame;
		    buf.arg = subprefix_stack[j];
		    kbd_buffer_store_event (&buf);
		  }

	      if (!NILP (prefix))
		{
		  buf.kind = MENU_BAR_EVENT;
		  buf.frame_or_window = frame;
		  buf.arg = prefix;
		  kbd_buffer_store_event (&buf);
		}

	      buf.kind = MENU_BAR_EVENT;
	      buf.frame_or_window = frame;
	      buf.arg = entry;
	      kbd_buffer_store_event (&buf);

	      return;
	    }
	  i += MENU_ITEMS_ITEM_LENGTH;
	}
    }
}

#endif /* USE_X_TOOLKIT || USE_GTK || HAVE_NS || HAVE_NTGUI */

#ifdef HAVE_NS
/* As above, but return the menu selection instead of storing in kb buffer.
   If keymaps==1, return full prefixes to selection. */
Lisp_Object
find_and_return_menu_selection (FRAME_PTR f, int keymaps, void *client_data)
{
  Lisp_Object prefix, entry;
  int i;
  Lisp_Object *subprefix_stack;
  int submenu_depth = 0;

  prefix = entry = Qnil;
  i = 0;
  subprefix_stack =
    (Lisp_Object *)alloca (menu_items_used * sizeof (Lisp_Object));

  while (i < menu_items_used)
    {
      if (EQ (XVECTOR (menu_items)->contents[i], Qnil))
        {
          subprefix_stack[submenu_depth++] = prefix;
          prefix = entry;
          i++;
        }
      else if (EQ (XVECTOR (menu_items)->contents[i], Qlambda))
        {
          prefix = subprefix_stack[--submenu_depth];
          i++;
        }
      else if (EQ (XVECTOR (menu_items)->contents[i], Qt))
        {
          prefix
            = XVECTOR (menu_items)->contents[i + MENU_ITEMS_PANE_PREFIX];
          i += MENU_ITEMS_PANE_LENGTH;
        }
      /* Ignore a nil in the item list.
         It's meaningful only for dialog boxes.  */
      else if (EQ (XVECTOR (menu_items)->contents[i], Qquote))
        i += 1;
      else
        {
          entry
            = XVECTOR (menu_items)->contents[i + MENU_ITEMS_ITEM_VALUE];
          if ((EMACS_INT)client_data ==  (EMACS_INT)(&XVECTOR (menu_items)->contents[i]))
            {
              if (keymaps != 0)
                {
                  int j;

                  entry = Fcons (entry, Qnil);
                  if (!NILP (prefix))
                    entry = Fcons (prefix, entry);
                  for (j = submenu_depth - 1; j >= 0; j--)
                    if (!NILP (subprefix_stack[j]))
                      entry = Fcons (subprefix_stack[j], entry);
                }
              return entry;
            }
          i += MENU_ITEMS_ITEM_LENGTH;
        }
    }
  return Qnil;
}
#endif  /* HAVE_NS */

DEFUN ("x-popup-menu", Fx_popup_menu, Sx_popup_menu, 2, 2, 0,
       doc: /* Pop up a deck-of-cards menu and return user's selection.
POSITION is a position specification.  This is either a mouse button event
or a list ((XOFFSET YOFFSET) WINDOW)
where XOFFSET and YOFFSET are positions in pixels from the top left
corner of WINDOW.  (WINDOW may be a window or a frame object.)
This controls the position of the top left of the menu as a whole.
If POSITION is t, it means to use the current mouse position.

MENU is a specifier for a menu.  For the simplest case, MENU is a keymap.
The menu items come from key bindings that have a menu string as well as
a definition; actually, the "definition" in such a key binding looks like
\(STRING . REAL-DEFINITION).  To give the menu a title, put a string into
the keymap as a top-level element.

If REAL-DEFINITION is nil, that puts a nonselectable string in the menu.
Otherwise, REAL-DEFINITION should be a valid key binding definition.

You can also use a list of keymaps as MENU.
  Then each keymap makes a separate pane.

When MENU is a keymap or a list of keymaps, the return value is the
list of events corresponding to the user's choice. Note that
`x-popup-menu' does not actually execute the command bound to that
sequence of events.

Alternatively, you can specify a menu of multiple panes
  with a list of the form (TITLE PANE1 PANE2...),
where each pane is a list of form (TITLE ITEM1 ITEM2...).
Each ITEM is normally a cons cell (STRING . VALUE);
but a string can appear as an item--that makes a nonselectable line
in the menu.
With this form of menu, the return value is VALUE from the chosen item.

If POSITION is nil, don't display the menu at all, just precalculate the
cached information about equivalent key sequences.

If the user gets rid of the menu without making a valid choice, for
instance by clicking the mouse away from a valid choice or by typing
keyboard input, then this normally results in a quit and
`x-popup-menu' does not return.  But if POSITION is a mouse button
event (indicating that the user invoked the menu with the mouse) then
no quit occurs and `x-popup-menu' returns nil.  */)
  (Lisp_Object position, Lisp_Object menu)
{
  Lisp_Object keymap, tem;
  int xpos = 0, ypos = 0;
  Lisp_Object title;
  const char *error_name = NULL;
  Lisp_Object selection = Qnil;
  FRAME_PTR f = NULL;
  Lisp_Object x, y, window;
  int keymaps = 0;
  int for_click = 0;
  int specpdl_count = SPECPDL_INDEX ();
  struct gcpro gcpro1;

  if (NILP (position))
    /* This is an obsolete call, which wants us to precompute the
       keybinding equivalents, but we don't do that any more anyway.  */
    return Qnil;

#ifdef HAVE_MENUS
  {
    int get_current_pos_p = 0;
    /* FIXME!!  check_w32 (); or check_x (); or check_ns (); */

    /* Decode the first argument: find the window and the coordinates.  */
    if (EQ (position, Qt)
	|| (CONSP (position) && (EQ (XCAR (position), Qmenu_bar)
				 || EQ (XCAR (position), Qtool_bar))))
      {
	get_current_pos_p = 1;
      }
    else
      {
	tem = Fcar (position);
	if (CONSP (tem))
	  {
	    window = Fcar (Fcdr (position));
	    x = XCAR (tem);
	    y = Fcar (XCDR (tem));
	  }
	else
	  {
	    for_click = 1;
	    tem = Fcar (Fcdr (position));  /* EVENT_START (position) */
	    window = Fcar (tem);	     /* POSN_WINDOW (tem) */
	    tem = Fcar (Fcdr (Fcdr (tem))); /* POSN_WINDOW_POSN (tem) */
	    x = Fcar (tem);
	    y = Fcdr (tem);
	  }

	/* If a click happens in an external tool bar or a detached
	   tool bar, x and y is NIL.  In that case, use the current
	   mouse position.  This happens for the help button in the
	   tool bar.  Ideally popup-menu should pass NIL to
	   this function, but it doesn't.  */
	if (NILP (x) && NILP (y))
	  get_current_pos_p = 1;
      }

    if (get_current_pos_p)
      {
	/* Use the mouse's current position.  */
	FRAME_PTR new_f = SELECTED_FRAME ();
#ifdef HAVE_X_WINDOWS
	/* Can't use mouse_position_hook for X since it returns
	   coordinates relative to the window the mouse is in,
	   we need coordinates relative to the edit widget always.  */
	if (new_f != 0)
	  {
	    int cur_x, cur_y;

	    mouse_position_for_popup (new_f, &cur_x, &cur_y);
	    /* cur_x/y may be negative, so use make_number.  */
	    x = make_number (cur_x);
	    y = make_number (cur_y);
	  }

#else /* not HAVE_X_WINDOWS */
	Lisp_Object bar_window;
	enum scroll_bar_part part;
	Time time;
        void (*mouse_position_hook) (struct frame **, int,
                                     Lisp_Object *,
                                     enum scroll_bar_part *,
                                     Lisp_Object *,
                                     Lisp_Object *,
                                     Time *) =
	  FRAME_TERMINAL (new_f)->mouse_position_hook;

	if (mouse_position_hook)
	  (*mouse_position_hook) (&new_f, 1, &bar_window,
				  &part, &x, &y, &time);
#endif /* not HAVE_X_WINDOWS */

	if (new_f != 0)
	  XSETFRAME (window, new_f);
	else
	  {
	    window = selected_window;
	    XSETFASTINT (x, 0);
	    XSETFASTINT (y, 0);
	  }
      }

    CHECK_NUMBER (x);
    CHECK_NUMBER (y);

    /* Decode where to put the menu.  */

    if (FRAMEP (window))
      {
	f = XFRAME (window);
	xpos = 0;
	ypos = 0;
      }
    else if (WINDOWP (window))
      {
	struct window *win = XWINDOW (window);
	CHECK_LIVE_WINDOW (window);
	f = XFRAME (WINDOW_FRAME (win));

	xpos = WINDOW_LEFT_EDGE_X (win);
	ypos = WINDOW_TOP_EDGE_Y (win);
      }
    else
      /* ??? Not really clean; should be CHECK_WINDOW_OR_FRAME,
	 but I don't want to make one now.  */
      CHECK_WINDOW (window);

    xpos += XINT (x);
    ypos += XINT (y);

    /* FIXME: Find a more general check!  */
    if (!(FRAME_X_P (f) || FRAME_MSDOS_P (f)
	  || FRAME_W32_P (f) || FRAME_NS_P (f)))
      error ("Can not put GUI menu on this terminal");

    XSETFRAME (Vmenu_updating_frame, f);
  }
#endif /* HAVE_MENUS */

  /* Now parse the lisp menus.  */
  record_unwind_protect (unuse_menu_items, Qnil);

  title = Qnil;
  GCPRO1 (title);

  /* Decode the menu items from what was specified.  */

  keymap = get_keymap (menu, 0, 0);
  if (CONSP (keymap))
    {
      /* We were given a keymap.  Extract menu info from the keymap.  */
      Lisp_Object prompt;

      /* Extract the detailed info to make one pane.  */
      keymap_panes (&menu, 1);

      /* Search for a string appearing directly as an element of the keymap.
	 That string is the title of the menu.  */
      prompt = Fkeymap_prompt (keymap);
      if (!NILP (prompt))
	title = prompt;
#ifdef HAVE_NS		/* Is that needed and NS-specific?  --Stef  */
      else
	title = build_string ("Select");
#endif

      /* Make that be the pane title of the first pane.  */
      if (!NILP (prompt) && menu_items_n_panes >= 0)
	ASET (menu_items, MENU_ITEMS_PANE_NAME, prompt);

      keymaps = 1;
    }
  else if (CONSP (menu) && KEYMAPP (XCAR (menu)))
    {
      /* We were given a list of keymaps.  */
      int nmaps = XFASTINT (Flength (menu));
      Lisp_Object *maps
	= (Lisp_Object *) alloca (nmaps * sizeof (Lisp_Object));
      int i;

      title = Qnil;

      /* The first keymap that has a prompt string
	 supplies the menu title.  */
      for (tem = menu, i = 0; CONSP (tem); tem = XCDR (tem))
	{
	  Lisp_Object prompt;

	  maps[i++] = keymap = get_keymap (XCAR (tem), 1, 0);

	  prompt = Fkeymap_prompt (keymap);
	  if (NILP (title) && !NILP (prompt))
	    title = prompt;
	}

      /* Extract the detailed info to make one pane.  */
      keymap_panes (maps, nmaps);

      /* Make the title be the pane title of the first pane.  */
      if (!NILP (title) && menu_items_n_panes >= 0)
	ASET (menu_items, MENU_ITEMS_PANE_NAME, title);

      keymaps = 1;
    }
  else
    {
      /* We were given an old-fashioned menu.  */
      title = Fcar (menu);
      CHECK_STRING (title);

      list_of_panes (Fcdr (menu));

      keymaps = 0;
    }

  unbind_to (specpdl_count, Qnil);

#ifdef HAVE_MENUS
#ifdef HAVE_WINDOW_SYSTEM
  /* Hide a previous tip, if any.  */
  Fx_hide_tip ();
#endif

#ifdef HAVE_NTGUI     /* FIXME: Is it really w32-specific?  --Stef  */
  /* If resources from a previous popup menu still exist, does nothing
     until the `menu_free_timer' has freed them (see w32fns.c). This
     can occur if you press ESC or click outside a menu without selecting
     a menu item.
  */
  if (current_popup_menu)
    {
      discard_menu_items ();
      FRAME_X_DISPLAY_INFO (f)->grabbed = 0;
      UNGCPRO;
      return Qnil;
    }
#endif

#ifdef HAVE_NS			/* FIXME: ns-specific, why? --Stef  */
  record_unwind_protect (cleanup_popup_menu, Qnil);
#endif

  /* Display them in a menu.  */
  BLOCK_INPUT;

  /* FIXME: Use a terminal hook!  */
#if defined HAVE_NTGUI
  selection = w32_menu_show (f, xpos, ypos, for_click,
			     keymaps, title, &error_name);
#elif defined HAVE_NS
  selection = ns_menu_show (f, xpos, ypos, for_click,
			    keymaps, title, &error_name);
#else /* MSDOS and X11 */
  /* Assume last_event_timestamp is the timestamp of the button event.
     Is this assumption ever violated?  We can't use the timestamp
     stored within POSITION because there the top bits from the actual
     timestamp may be truncated away (Bug#4930).  */
  selection = xmenu_show (f, xpos, ypos, for_click,
			  keymaps, title, &error_name,
			  last_event_timestamp);
#endif

  UNBLOCK_INPUT;

#ifdef HAVE_NS
  unbind_to (specpdl_count, Qnil);
#else
  discard_menu_items ();
#endif

#ifdef HAVE_NTGUI     /* FIXME: Is it really w32-specific?  --Stef  */
  FRAME_X_DISPLAY_INFO (f)->grabbed = 0;
#endif

#endif /* HAVE_MENUS */

  UNGCPRO;

  if (error_name) error ("%s", error_name);
  return selection;
}

void
syms_of_menu (void)
{
  staticpro (&menu_items);
  menu_items = Qnil;
  menu_items_inuse = Qnil;

  defsubr (&Sx_popup_menu);
}
