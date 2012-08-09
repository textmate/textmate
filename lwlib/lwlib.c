/* A general interface to the widgets of different toolkits.

Copyright (C) 1992, 1993 Lucid, Inc.
Copyright (C) 1994-1996, 1999-2012  Free Software Foundation, Inc.

This file is part of the Lucid Widget Library.

The Lucid Widget Library is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

The Lucid Widget Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <setjmp.h>
#include <lisp.h>

#include <sys/types.h>
#include <stdio.h>
#include <ctype.h>
#include "lwlib-int.h"
#include "lwlib-utils.h"
#include <X11/StringDefs.h>

#if defined (USE_LUCID)
#include "lwlib-Xlw.h"
#endif
#if defined (USE_MOTIF)
#include "lwlib-Xm.h"
#else /* not USE_MOTIF */
#if defined (USE_LUCID)
#define USE_XAW
#endif /* not USE_MOTIF && USE_LUCID */
#endif
#if defined (USE_XAW)
#ifdef HAVE_XAW3D
#include <X11/Xaw3d/Paned.h>
#else /* !HAVE_XAW3D */
#include <X11/Xaw/Paned.h>
#endif /* HAVE_XAW3D */
#include "lwlib-Xaw.h"
#endif

#if !defined (USE_LUCID) && !defined (USE_MOTIF)
 #error  At least one of USE_LUCID or USE_MOTIF must be defined.
#endif

#ifndef max
#define max(x, y) ((x) > (y) ? (x) : (y))
#endif

/* List of all widgets managed by the library. */
static widget_info*
all_widget_info = NULL;

#ifdef USE_MOTIF
const char *lwlib_toolkit_type = "motif";
#else
const char *lwlib_toolkit_type = "lucid";
#endif

static widget_value *merge_widget_value (widget_value *,
                                         widget_value *,
                                         int, int *);
static void instantiate_widget_instance (widget_instance *);
static int my_strcasecmp (const char *, const char *);
static void safe_free_str (char *);
static void free_widget_value_tree (widget_value *);
static widget_value *copy_widget_value_tree (widget_value *,
                                             change_type);
static widget_info *allocate_widget_info (const char *, const char *, LWLIB_ID,
                                          widget_value *,
                                          lw_callback, lw_callback,
                                          lw_callback, lw_callback);
static void free_widget_info (widget_info *);
static void mark_widget_destroyed (Widget, XtPointer, XtPointer);
static widget_instance *allocate_widget_instance (widget_info *,
                                                  Widget, Boolean);
static void free_widget_instance (widget_instance *);
static widget_info *get_widget_info (LWLIB_ID, Boolean);
static widget_instance *get_widget_instance (Widget, Boolean);
static widget_instance *find_instance (LWLIB_ID, Widget, Boolean);
static Boolean safe_strcmp (const char *, const char *);
static Widget name_to_widget (widget_instance *, const char *);
static void set_one_value (widget_instance *, widget_value *, Boolean);
static void update_one_widget_instance (widget_instance *, Boolean);
static void update_all_widget_values (widget_info *, Boolean);
static void initialize_widget_instance (widget_instance *);
static widget_creation_function find_in_table (const char *, const widget_creation_entry *);
static Boolean dialog_spec_p (const char *);
static void destroy_one_instance (widget_instance *);
static void lw_pop_all_widgets (LWLIB_ID, Boolean);
static Boolean get_one_value (widget_instance *, widget_value *);
static void show_one_widget_busy (Widget, Boolean);
/* utility functions for widget_instance and widget_info */
char *
safe_strdup (const char *s)
{
  char *result;
  if (! s) return 0;
  result = (char *) xmalloc (strlen (s) + 1);
  strcpy (result, s);
  return result;
}

/* Like strcmp but ignore differences in case.  */

static int
my_strcasecmp (const char *s1, const char *s2)
{
  while (1)
    {
      int c1 = *s1++;
      int c2 = *s2++;
      if (isupper (c1))
	c1 = tolower (c1);
      if (isupper (c2))
	c2 = tolower (c2);
      if (c1 != c2)
	return (c1 > c2 ? 1 : -1);
      if (c1 == 0)
	return 0;
    }
}

static void
safe_free_str (char *s)
{
  xfree (s);
}

static widget_value *widget_value_free_list = 0;
static int malloc_cpt = 0;

widget_value *
malloc_widget_value (void)
{
  widget_value *wv;
  if (widget_value_free_list)
    {
      wv = widget_value_free_list;
      widget_value_free_list = wv->free_list;
      wv->free_list = 0;
    }
  else
    {
      wv = (widget_value *) xmalloc (sizeof (widget_value));
      malloc_cpt++;
    }
  memset ((void*) wv, 0, sizeof (widget_value));
  return wv;
}

/* this is analogous to free().  It frees only what was allocated
   by malloc_widget_value(), and no substructures.
 */
void
free_widget_value (widget_value *wv)
{
  if (wv->free_list)
    abort ();

  if (malloc_cpt > 25)
    {
      /* When the number of already allocated cells is too big,
	 We free it.  */
      xfree (wv);
      malloc_cpt--;
    }
  else
    {
      wv->free_list = widget_value_free_list;
      widget_value_free_list = wv;
    }
}

static void
free_widget_value_tree (widget_value *wv)
{
  if (!wv)
    return;

  xfree (wv->name);
  xfree (wv->value);
  xfree (wv->key);

  wv->name = wv->value = wv->key = (char *) 0xDEADBEEF;

  if (wv->toolkit_data && wv->free_toolkit_data)
    {
      XtFree (wv->toolkit_data);
      wv->toolkit_data = (void *) 0xDEADBEEF;
    }

  if (wv->contents && (wv->contents != (widget_value*)1))
    {
      free_widget_value_tree (wv->contents);
      wv->contents = (widget_value *) 0xDEADBEEF;
    }
  if (wv->next)
    {
      free_widget_value_tree (wv->next);
      wv->next = (widget_value *) 0xDEADBEEF;
    }
  free_widget_value (wv);
}

static widget_value *
copy_widget_value_tree (widget_value *val, change_type change)
{
  widget_value* copy;

  if (!val)
    return NULL;
  if (val == (widget_value *) 1)
    return val;

  copy = malloc_widget_value ();
  copy->name = safe_strdup (val->name);
  copy->value = safe_strdup (val->value);
  copy->key = safe_strdup (val->key);
  copy->help = val->help;
  copy->enabled = val->enabled;
  copy->button_type = val->button_type;
  copy->selected = val->selected;
  copy->edited = False;
  copy->change = change;
  copy->this_one_change = change;
  copy->contents = copy_widget_value_tree (val->contents, change);
  copy->call_data = val->call_data;
  copy->next = copy_widget_value_tree (val->next, change);
  copy->toolkit_data = NULL;
  copy->free_toolkit_data = False;
  return copy;
}

static widget_info *
allocate_widget_info (const char* type,
                      const char* name,
                      LWLIB_ID id,
                      widget_value* val,
                      lw_callback pre_activate_cb,
                      lw_callback selection_cb,
                      lw_callback post_activate_cb,
                      lw_callback highlight_cb)
{
  widget_info* info = (widget_info*) xmalloc (sizeof (widget_info));
  info->type = safe_strdup (type);
  info->name = safe_strdup (name);
  info->id = id;
  info->val = copy_widget_value_tree (val, STRUCTURAL_CHANGE);
  info->busy = False;
  info->pre_activate_cb = pre_activate_cb;
  info->selection_cb = selection_cb;
  info->post_activate_cb = post_activate_cb;
  info->highlight_cb = highlight_cb;
  info->instances = NULL;

  info->next = all_widget_info;
  all_widget_info = info;

  return info;
}

static void
free_widget_info (widget_info *info)
{
  safe_free_str (info->type);
  safe_free_str (info->name);
  free_widget_value_tree (info->val);
  memset ((void*)info, 0xDEADBEEF, sizeof (widget_info));
  xfree (info);
}

static void
mark_widget_destroyed (Widget widget, XtPointer closure, XtPointer call_data)
{
  widget_instance* instance = (widget_instance*)closure;

  /* be very conservative */
  if (instance->widget == widget)
    instance->widget = NULL;
}

static widget_instance *
allocate_widget_instance (widget_info* info, Widget parent, Boolean pop_up_p)
{
  widget_instance* instance =
    (widget_instance*) xmalloc (sizeof (widget_instance));
  memset (instance, 0, sizeof *instance);
  instance->parent = parent;
  instance->pop_up_p = pop_up_p;
  instance->info = info;
  instance->next = info->instances;
  info->instances = instance;

  instantiate_widget_instance (instance);

  XtAddCallback (instance->widget, XtNdestroyCallback,
		 mark_widget_destroyed, (XtPointer)instance);
  return instance;
}

static void
free_widget_instance (widget_instance *instance)
{
  memset ((void*)instance, 0xDEADBEEF, sizeof (widget_instance));
  xfree (instance);
}

static widget_info *
get_widget_info (LWLIB_ID id, Boolean remove_p)
{
  widget_info* info;
  widget_info* prev;
  for (prev = NULL, info = all_widget_info;
       info;
       prev = info, info = info->next)
    if (info->id == id)
     {
       if (remove_p)
	 {
	   if (prev)
	     prev->next = info->next;
	   else
	     all_widget_info = info->next;
	 }
      return info;
     }
  return NULL;
}

/* Internal function used by the library dependent implementation to get the
   widget_value for a given widget in an instance */
widget_info *
lw_get_widget_info (LWLIB_ID id)
{
  return get_widget_info (id, 0);
}

static widget_instance *
get_widget_instance (Widget widget, Boolean remove_p)
{
  widget_info* info;
  widget_instance* instance;
  widget_instance* prev;
  for (info = all_widget_info; info; info = info->next)
    for (prev = NULL, instance = info->instances;
	 instance;
	 prev = instance, instance = instance->next)
      if (instance->widget == widget)
	{
	  if (remove_p)
	    {
	      if (prev)
		prev->next = instance->next;
	      else
		info->instances = instance->next;
	    }
	  return instance;
	}
  return (widget_instance *) 0;
}

/* Value is a pointer to the widget_instance corresponding to
   WIDGET, or null if WIDGET is not a lwlib widget.  */

widget_instance *
lw_get_widget_instance (Widget widget)
{
  return get_widget_instance (widget, False);
}

static widget_instance*
find_instance (LWLIB_ID id, Widget parent, Boolean pop_up_p)
{
  widget_info* info = get_widget_info (id, False);
  widget_instance* instance;

  if (info)
    for (instance = info->instances; instance; instance = instance->next)
      if (instance->parent == parent && instance->pop_up_p == pop_up_p)
	return instance;

  return NULL;
}


/* utility function for widget_value */
static Boolean
safe_strcmp (const char *s1, const char *s2)
{
  if (!!s1 ^ !!s2) return True;
  return (s1 && s2) ? strcmp (s1, s2) : s1 ? False : !!s2;
}


#if 0
# define EXPLAIN(name, oc, nc, desc, a1, a2)				\
   printf ("Change: \"%s\"\tmax(%s=%d,%s=%d)\t%s %d %d\n",		\
	   name,							\
	   (oc == NO_CHANGE ? "none" :					\
	    (oc == INVISIBLE_CHANGE ? "invisible" :			\
	     (oc == VISIBLE_CHANGE ? "visible" :			\
	      (oc == STRUCTURAL_CHANGE ? "structural" : "???")))),	\
	   oc,								\
	   (nc == NO_CHANGE ? "none" :					\
	    (nc == INVISIBLE_CHANGE ? "invisible" :			\
	     (nc == VISIBLE_CHANGE ? "visible" :			\
	      (nc == STRUCTURAL_CHANGE ? "structural" : "???")))),	\
	   nc, desc, a1, a2)
#else
# define EXPLAIN(name, oc, nc, desc, a1, a2) ((void) 0)
#endif


static widget_value *
merge_widget_value (widget_value *val1,
                    widget_value *val2,
                    int level,
                    int *change_p)
{
  change_type change, this_one_change;
  widget_value* merged_next;
  widget_value* merged_contents;

  if (!val1)
    {
      if (val2)
	{
	  *change_p = 1;
	  return copy_widget_value_tree (val2, STRUCTURAL_CHANGE);
	}
      else
	return NULL;
    }
  if (!val2)
    {
      *change_p = 1;
      free_widget_value_tree (val1);
      return NULL;
    }

  change = NO_CHANGE;

  if (safe_strcmp (val1->name, val2->name))
    {
      EXPLAIN (val1->name, change, STRUCTURAL_CHANGE, "name change",
	       val1->name, val2->name);
      change = max (change, STRUCTURAL_CHANGE);
      safe_free_str (val1->name);
      val1->name = safe_strdup (val2->name);
    }
  if (safe_strcmp (val1->value, val2->value))
    {
      EXPLAIN (val1->name, change, VISIBLE_CHANGE, "value change",
	       val1->value, val2->value);
      change = max (change, VISIBLE_CHANGE);
      safe_free_str (val1->value);
      val1->value = safe_strdup (val2->value);
    }
  if (safe_strcmp (val1->key, val2->key))
    {
      EXPLAIN (val1->name, change, VISIBLE_CHANGE, "key change",
	       val1->key, val2->key);
      change = max (change, VISIBLE_CHANGE);
      safe_free_str (val1->key);
      val1->key = safe_strdup (val2->key);
    }
  if (! EQ (val1->help, val2->help))
    {
      EXPLAIN (val1->name, change, VISIBLE_CHANGE, "help change",
	       val1->help, val2->help);
      change = max (change, VISIBLE_CHANGE);
      val1->help = val2->help;
    }
  if (val1->enabled != val2->enabled)
    {
      EXPLAIN (val1->name, change, VISIBLE_CHANGE, "enablement change",
	       val1->enabled, val2->enabled);
      change = max (change, VISIBLE_CHANGE);
      val1->enabled = val2->enabled;
    }
  if (val1->button_type != val2->button_type)
    {
      EXPLAIN (val1->name, change, VISIBLE_CHANGE, "button type change",
	       val1->button_type, val2->button_type);
      change = max (change, VISIBLE_CHANGE);
      val1->button_type = val2->button_type;
    }
  if (val1->selected != val2->selected)
    {
      EXPLAIN (val1->name, change, VISIBLE_CHANGE, "selection change",
	       val1->selected, val2->selected);
      change = max (change, VISIBLE_CHANGE);
      val1->selected = val2->selected;
    }
  if (val1->call_data != val2->call_data)
    {
      EXPLAIN (val1->name, change, INVISIBLE_CHANGE, "call-data change",
	       val1->call_data, val2->call_data);
      change = max (change, INVISIBLE_CHANGE);
      val1->call_data = val2->call_data;
    }

  if (level > 0)
    {
      merged_contents =
	merge_widget_value (val1->contents, val2->contents, level - 1,
			    change_p);

      if (val1->contents && !merged_contents)
	{
	  /* This used to say INVISIBLE_CHANGE,
	     but it is visible and vitally important when
	     the contents of the menu bar itself are entirely deleted.

	     But maybe it doesn't matter.  This fails to fix the bug.  */
	  EXPLAIN (val1->name, change, STRUCTURAL_CHANGE, "(contents gone)",
		   0, 0);
	  change = max (change, STRUCTURAL_CHANGE);
	}
      else if (merged_contents && merged_contents->change != NO_CHANGE)
	{
	  EXPLAIN (val1->name, change, INVISIBLE_CHANGE, "(contents change)",
		   0, 0);
	  change = max (change, INVISIBLE_CHANGE);
#if 0 /* This was replaced by the August 9 1996 change in lwlib-Xm.c.  */
#ifdef USE_MOTIF
	  change = max (merged_contents->change, change);
#endif
#endif
	}

      val1->contents = merged_contents;
    }

  this_one_change = change;

  merged_next = merge_widget_value (val1->next, val2->next, level, change_p);

  if (val1->next && !merged_next)
    {
      EXPLAIN (val1->name, change, STRUCTURAL_CHANGE, "(following gone)",
	       0, 0);
      change = max (change, STRUCTURAL_CHANGE);
    }
  else if (merged_next)
    {
      if (merged_next->change)
	EXPLAIN (val1->name, change, merged_next->change, "(following change)",
		 0, 0);
      change = max (change, merged_next->change);
    }

  val1->next = merged_next;

  val1->this_one_change = this_one_change;
  val1->change = change;

  if (change > NO_CHANGE && val1->toolkit_data)
    {
      *change_p = 1;
      if (val1->free_toolkit_data)
	XtFree (val1->toolkit_data);
      val1->toolkit_data = NULL;
    }

  return val1;
}


/* modifying the widgets */
static Widget
name_to_widget (widget_instance *instance, const char *name)
{
  Widget widget = NULL;

  if (!instance->widget)
    return NULL;

  if (!strcmp (XtName (instance->widget), name))
    widget = instance->widget;
  else
    {
      int length = strlen (name) + 2;
      char* real_name = (char *) xmalloc (length);
      real_name [0] = '*';
      strcpy (real_name + 1, name);

      widget = XtNameToWidget (instance->widget, real_name);

      xfree (real_name);
    }
  return widget;
}

static void
set_one_value (widget_instance* instance, widget_value* val, Boolean deep_p)
{
  Widget widget = name_to_widget (instance, val->name);

  if (widget)
    {
#if defined (USE_LUCID)
      if (lw_lucid_widget_p (instance->widget))
	xlw_update_one_widget (instance, widget, val, deep_p);
#endif
#if defined (USE_MOTIF)
      if (lw_motif_widget_p (instance->widget))
	xm_update_one_widget (instance, widget, val, deep_p);
#endif
#if defined (USE_XAW)
      if (lw_xaw_widget_p (instance->widget))
	xaw_update_one_widget (instance, widget, val, deep_p);
#endif
    }
}

static void
update_one_widget_instance (widget_instance* instance, Boolean deep_p)
{
  widget_value *val;

  if (!instance->widget)
    /* the widget was destroyed */
    return;

  for (val = instance->info->val; val; val = val->next)
    if (val->change != NO_CHANGE)
      set_one_value (instance, val, deep_p);
}

static void
update_all_widget_values (widget_info* info, Boolean deep_p)
{
  widget_instance* instance;
  widget_value* val;

  for (instance = info->instances; instance; instance = instance->next)
    update_one_widget_instance (instance, deep_p);

  for (val = info->val; val; val = val->next)
    val->change = NO_CHANGE;
}

int
lw_modify_all_widgets (LWLIB_ID id, widget_value* val, Boolean deep_p)
{
  widget_info* info = get_widget_info (id, False);
  widget_value* new_val;
  widget_value* next_new_val;
  widget_value* cur;
  widget_value* prev;
  widget_value* next;
  int		found;
  int change_p = 0;

  if (!info)
    return 0;

  for (new_val = val; new_val; new_val = new_val->next)
    {
      next_new_val = new_val->next;
      new_val->next = NULL;
      found = False;
      for (prev = NULL, cur = info->val; cur; prev = cur, cur = cur->next)
	if (!strcmp (cur->name, new_val->name))
	  {
	    found = True;
	    next = cur->next;
	    cur->next = NULL;
	    cur = merge_widget_value (cur, new_val, deep_p ? 1000 : 1,
				      &change_p);
	    if (prev)
	      prev->next = cur ? cur : next;
	    else
	      info->val = cur ? cur : next;
	    if (cur)
	      cur->next = next;
	    break;
	  }
      if (!found)
	{
	  /* Could not find it, add it */
	  if (prev)
	    prev->next = copy_widget_value_tree (new_val, STRUCTURAL_CHANGE);
	  else
	    info->val = copy_widget_value_tree (new_val, STRUCTURAL_CHANGE);
	  change_p = 1;
	}
      new_val->next = next_new_val;
    }

  update_all_widget_values (info, deep_p);
  return change_p;
}


/* creating the widgets */

static void
initialize_widget_instance (widget_instance *instance)
{
  widget_value* val;

  for (val = instance->info->val; val; val = val->next)
    val->change = STRUCTURAL_CHANGE;

  update_one_widget_instance (instance, True);

  for (val = instance->info->val; val; val = val->next)
    val->change = NO_CHANGE;
}


static widget_creation_function
find_in_table (const char *type, const widget_creation_entry *table)
{
  const widget_creation_entry* cur;
  for (cur = table; cur->type; cur++)
    if (!my_strcasecmp (type, cur->type))
      return cur->function;
  return NULL;
}

static Boolean
dialog_spec_p (const char *name)
{
  /* return True if name matches [EILPQeilpq][1-9][Bb] or
     [EILPQeilpq][1-9][Bb][Rr][1-9] */
  if (!name)
    return False;

  switch (name [0])
    {
    case 'E': case 'I': case 'L': case 'P': case 'Q':
    case 'e': case 'i': case 'l': case 'p': case 'q':
      if (name [1] >= '0' && name [1] <= '9')
	{
	  if (name [2] != 'B' && name [2] != 'b')
	    return False;
	  if (!name [3])
	    return True;
	  if ((name [3] == 'T' || name [3] == 't') && !name [4])
	    return True;
	  if ((name [3] == 'R' || name [3] == 'r')
	      && name [4] >= '0' && name [4] <= '9' && !name [5])
	    return True;
	  return False;
	}
      else
	return False;

    default:
      return False;
    }
}

static void
instantiate_widget_instance (widget_instance *instance)
{
  widget_creation_function function = NULL;

#if defined (USE_LUCID)
  if (!function)
    function = find_in_table (instance->info->type, xlw_creation_table);
#endif
#if defined(USE_MOTIF)
  if (!function)
    function = find_in_table (instance->info->type, xm_creation_table);
#endif
#if defined (USE_XAW)
  if (!function)
    function = find_in_table (instance->info->type, xaw_creation_table);
#endif

  if (!function)
    {
      if (dialog_spec_p (instance->info->type))
	{
#if defined (USE_LUCID)
	  /* not yet */
#endif
#if defined(USE_MOTIF)
	  if (!function)
	    function = xm_create_dialog;
#endif
#if defined (USE_XAW)
	  if (!function)
	    function = xaw_create_dialog;
#endif
	}
    }

  if (!function)
    {
      printf ("No creation function for widget type %s\n",
	      instance->info->type);
      abort ();
    }

  instance->widget = (*function) (instance);

  if (!instance->widget)
    abort ();

  /*   XtRealizeWidget (instance->widget);*/
}

void
lw_register_widget (const char* type,
                    const char* name,
                    LWLIB_ID id,
                    widget_value* val,
                    lw_callback pre_activate_cb,
                    lw_callback selection_cb,
                    lw_callback post_activate_cb,
                    lw_callback highlight_cb)
{
  if (!get_widget_info (id, False))
    allocate_widget_info (type, name, id, val, pre_activate_cb, selection_cb,
			  post_activate_cb, highlight_cb);
}

Widget
lw_get_widget (LWLIB_ID id, Widget parent, Boolean pop_up_p)
{
  widget_instance* instance;

  instance = find_instance (id, parent, pop_up_p);
  return instance ? instance->widget : NULL;
}

Widget
lw_make_widget (LWLIB_ID id, Widget parent, Boolean pop_up_p)
{
  widget_instance* instance;
  widget_info* info;

  instance = find_instance (id, parent, pop_up_p);
  if (!instance)
    {
      info = get_widget_info (id, False);
      if (!info)
	return NULL;
      instance = allocate_widget_instance (info, parent, pop_up_p);
      initialize_widget_instance (instance);
    }
  if (!instance->widget)
    abort ();
  return instance->widget;
}

Widget
lw_create_widget (const char* type, const char* name, LWLIB_ID id, widget_value* val,
		  Widget parent, Boolean pop_up_p,
		  lw_callback pre_activate_cb, lw_callback selection_cb,
		  lw_callback post_activate_cb, lw_callback highlight_cb)
{
  lw_register_widget (type, name, id, val, pre_activate_cb, selection_cb,
		      post_activate_cb, highlight_cb);
  return lw_make_widget (id, parent, pop_up_p);
}


/* destroying the widgets */
static void
destroy_one_instance (widget_instance *instance)
{
  /* Remove the destroy callback on the widget; that callback will try to
     dereference the instance object (to set its widget slot to 0, since the
     widget is dead.)  Since the instance is now dead, we don't have to worry
     about the fact that its widget is dead too.

     This happens in the Phase2Destroy of the widget, so this callback would
     not have been run until arbitrarily long after the instance was freed.
   */
  if (instance->widget)
    XtRemoveCallback (instance->widget, XtNdestroyCallback,
		      mark_widget_destroyed, (XtPointer)instance);

  if (instance->widget)
    {
      /* The else are pretty tricky here, including the empty statement
	 at the end because it would be very bad to destroy a widget
	 twice. */
#if defined (USE_LUCID)
      if (lw_lucid_widget_p (instance->widget))
	xlw_destroy_instance (instance);
      else
#endif
#if defined (USE_MOTIF)
      if (lw_motif_widget_p (instance->widget))
	xm_destroy_instance (instance);
      else
#endif
#if defined (USE_XAW)
      if (lw_xaw_widget_p (instance->widget))
	xaw_destroy_instance (instance);
      else
#endif
	{
	  /* Empty compound statement to terminate if-then-else chain.  */
	}
    }

  free_widget_instance (instance);
}

void
lw_destroy_widget (Widget w)
{
  widget_instance* instance = get_widget_instance (w, True);

  if (instance)
    {
      widget_info *info = instance->info;
      /* instance has already been removed from the list; free it */
      destroy_one_instance (instance);
      /* if there are no instances left, free the info too */
      if (!info->instances)
	lw_destroy_all_widgets (info->id);
    }
}

void
lw_destroy_all_widgets (LWLIB_ID id)
{
  widget_info* info = get_widget_info (id, True);
  widget_instance* instance;
  widget_instance* next;

  if (info)
    {
      for (instance = info->instances; instance; )
	{
	  next = instance->next;
	  destroy_one_instance (instance);
	  instance = next;
	}
      free_widget_info (info);
    }
}

void
lw_destroy_everything (void)
{
  while (all_widget_info)
    lw_destroy_all_widgets (all_widget_info->id);
}

void
lw_destroy_all_pop_ups (void)
{
  widget_info* info;
  widget_info* next;
  widget_instance* instance;

  for (info = all_widget_info; info; info = next)
    {
      next = info->next;
      instance = info->instances;
      if (instance && instance->pop_up_p)
	lw_destroy_all_widgets (info->id);
    }
}

#ifdef USE_MOTIF
extern Widget first_child (Widget);	/* garbage */
#endif

Widget
lw_raise_all_pop_up_widgets (void)
{
  widget_info* info;
  widget_instance* instance;
  Widget result = NULL;

  for (info = all_widget_info; info; info = info->next)
    for (instance = info->instances; instance; instance = instance->next)
      if (instance->pop_up_p)
	{
	  Widget widget = instance->widget;
	  if (widget)
	    {
	      if (XtIsManaged (widget)
#ifdef USE_MOTIF
		  /* What a complete load of crap!!!!
		     When a dialogShell is on the screen, it is not managed!
		   */
		  || (lw_motif_widget_p (instance->widget) &&
		      XtIsManaged (first_child (widget)))
#endif
		  )
		{
		  if (!result)
		    result = widget;
		  XMapRaised (XtDisplay (widget), XtWindow (widget));
		}
	    }
	}
  return result;
}

static void
lw_pop_all_widgets (LWLIB_ID id, Boolean up)
{
  widget_info* info = get_widget_info (id, False);
  widget_instance* instance;

  if (info)
    for (instance = info->instances; instance; instance = instance->next)
      if (instance->pop_up_p && instance->widget)
	{
#if defined (USE_LUCID)
	  if (lw_lucid_widget_p (instance->widget))
	    {
	      XtRealizeWidget (instance->widget);
	      xlw_pop_instance (instance, up);
	    }
#endif
#if defined (USE_MOTIF)
	  if (lw_motif_widget_p (instance->widget))
	    {
	      XtRealizeWidget (instance->widget);
	      xm_pop_instance (instance, up);
	    }
#endif
#if defined (USE_XAW)
	  if (lw_xaw_widget_p (instance->widget))
	    {
	      XtRealizeWidget (XtParent (instance->widget));
	      XtRealizeWidget (instance->widget);
	      xaw_pop_instance (instance, up);
	    }
#endif
	}
}

void
lw_pop_up_all_widgets (LWLIB_ID id)
{
  lw_pop_all_widgets (id, True);
}

void
lw_pop_down_all_widgets (LWLIB_ID id)
{
  lw_pop_all_widgets (id, False);
}

void
lw_popup_menu (Widget widget, XEvent *event)
{
#if defined (USE_LUCID)
  if (lw_lucid_widget_p (widget))
    xlw_popup_menu (widget, event);
#endif
#if defined (USE_MOTIF)
  if (lw_motif_widget_p (widget))
    xm_popup_menu (widget, event);
#endif
#if defined (USE_XAW)
  if (lw_xaw_widget_p (widget))
    xaw_popup_menu (widget, event);
#endif
}

/* get the values back */
static Boolean
get_one_value (widget_instance *instance, widget_value *val)
{
  Widget widget = name_to_widget (instance, val->name);

  if (widget)
    {
#if defined (USE_LUCID)
      if (lw_lucid_widget_p (instance->widget))
	xlw_update_one_value (instance, widget, val);
#endif
#if defined (USE_MOTIF)
      if (lw_motif_widget_p (instance->widget))
	xm_update_one_value (instance, widget, val);
#endif
#if defined (USE_XAW)
      if (lw_xaw_widget_p (instance->widget))
	xaw_update_one_value (instance, widget, val);
#endif
      return True;
    }
  else
    return False;
}

Boolean
lw_get_some_values (LWLIB_ID id, widget_value *val_out)
{
  widget_info* info = get_widget_info (id, False);
  widget_instance* instance;
  widget_value* val;
  Boolean result = False;

  if (!info)
    return False;

  instance = info->instances;
  if (!instance)
    return False;

  for (val = val_out; val; val = val->next)
    if (get_one_value (instance, val))
      result = True;

  return result;
}

widget_value*
lw_get_all_values (LWLIB_ID id)
{
  widget_info* info = get_widget_info (id, False);
  widget_value* val = info->val;
  if (lw_get_some_values (id, val))
    return val;
  else
    return NULL;
}

/* internal function used by the library dependent implementation to get the
   widget_value for a given widget in an instance */
widget_value*
lw_get_widget_value_for_widget (widget_instance *instance, Widget w)
{
  char* name = XtName (w);
  widget_value* cur;
  for (cur = instance->info->val; cur; cur = cur->next)
    if (!strcmp (cur->name, name))
      return cur;
  return NULL;
}

/* update other instances value when one thing changed */

/* To forbid recursive calls */
static Boolean lwlib_updating;

/* This function can be used as an XtCallback for the widgets that get
   modified to update other instances of the widgets.  Closure should be the
   widget_instance. */
void
lw_internal_update_other_instances (Widget widget,
                                    XtPointer closure,
                                    XtPointer call_data)
{
  widget_instance* instance = (widget_instance*)closure;
  char* name = XtName (widget);
  widget_info* info;
  widget_instance* cur;
  widget_value* val;

  /* Avoid possibly infinite recursion.  */
  if (lwlib_updating)
    return;

  /* protect against the widget being destroyed */
  if (XtWidgetBeingDestroyedP (widget))
    return;

  /* Return immediately if there are no other instances */
  info = instance->info;
  if (!info->instances->next)
    return;

  lwlib_updating = True;

  for (val = info->val; val && strcmp (val->name, name); val = val->next);

  if (val && get_one_value (instance, val))
    for (cur = info->instances; cur; cur = cur->next)
      if (cur != instance)
	set_one_value (cur, val, True);

  lwlib_updating = False;
}


/* get the id */

LWLIB_ID
lw_get_widget_id (Widget w)
{
  widget_instance* instance = get_widget_instance (w, False);

  return instance ? instance->info->id : 0;
}

/* set the keyboard focus */
void
lw_set_keyboard_focus (Widget parent, Widget w)
{
#if defined (USE_MOTIF)
  xm_set_keyboard_focus (parent, w);
#else
  XtSetKeyboardFocus (parent, w);
#endif
}

/* Show busy */
static void
show_one_widget_busy (Widget w, Boolean flag)
{
  Pixel foreground = 0;
  Pixel background = 1;
  Widget widget_to_invert = XtNameToWidget (w, "*sheet");
  if (!widget_to_invert)
    widget_to_invert = w;

  XtVaGetValues (widget_to_invert,
		 XtNforeground, &foreground,
		 XtNbackground, &background,
		 NULL);
  XtVaSetValues (widget_to_invert,
		 XtNforeground, background,
		 XtNbackground, foreground,
		 NULL);
}

void
lw_show_busy (Widget w, Boolean busy)
{
  widget_instance* instance = get_widget_instance (w, False);
  widget_info* info;
  widget_instance* next;

  if (instance)
    {
      info = instance->info;
      if (info->busy != busy)
	{
	  for (next = info->instances; next; next = next->next)
	    if (next->widget)
	      show_one_widget_busy (next->widget, busy);
	  info->busy = busy;
	}
    }
}

/* This hack exists because Lucid/Athena need to execute the strange
   function below to support geometry management. */
void
lw_refigure_widget (Widget w, Boolean doit)
{
#if defined (USE_XAW)
  XawPanedSetRefigureMode (w, doit);
#endif
#if defined (USE_MOTIF)
  if (doit)
    XtManageChild (w);
  else
    XtUnmanageChild (w);
#endif
}

/* Toolkit independent way of determining if an event window is in the
   menubar. */
Boolean
lw_window_is_in_menubar (Window win, Widget menubar_widget)
{
  return menubar_widget
#if defined (USE_LUCID)
      && XtWindow (menubar_widget) == win;
#endif
#if defined (USE_MOTIF)
      && ((XtWindow (menubar_widget) == win)
	  || (XtWindowToWidget (XtDisplay (menubar_widget), win)
	      && (XtParent (XtWindowToWidget (XtDisplay (menubar_widget), win))
		  == menubar_widget)));
#endif
}

/* Motif hack to set the main window areas. */
void
lw_set_main_areas (Widget parent, Widget menubar, Widget work_area)
{
#if defined (USE_MOTIF)
  xm_set_main_areas (parent, menubar, work_area);
#endif
}

/* Manage resizing for Motif.  This disables resizing when the menubar
   is about to be modified. */
void
lw_allow_resizing (Widget w, Boolean flag)
{
#if defined (USE_MOTIF)
  xm_manage_resizing (w, flag);
#endif
}


/* Value is non-zero if LABEL is a menu separator.  If it is, *TYPE is
   set to an appropriate enumerator of type enum menu_separator.
   MOTIF_P non-zero means map separator types not supported by Motif
   to similar ones that are supported.  */

int
lw_separator_p (const char *label, enum menu_separator *type, int motif_p)
{
  int separator_p = 0;

  if (strlen (label) >= 3
      && memcmp (label, "--:", 3) == 0)
    {
      static struct separator_table
      {
	const char *name;
	enum menu_separator type;
      }
      separator_names[] =
      {
	{"space",		      SEPARATOR_NO_LINE},
	{"noLine",		      SEPARATOR_NO_LINE},
	{"singleLine",		      SEPARATOR_SINGLE_LINE},
	{"doubleLine",		      SEPARATOR_DOUBLE_LINE},
	{"singleDashedLine",	      SEPARATOR_SINGLE_DASHED_LINE},
	{"doubleDashedLine",	      SEPARATOR_DOUBLE_DASHED_LINE},
	{"shadowEtchedIn",	      SEPARATOR_SHADOW_ETCHED_IN},
	{"shadowEtchedOut",	      SEPARATOR_SHADOW_ETCHED_OUT},
	{"shadowEtchedInDash",	      SEPARATOR_SHADOW_ETCHED_IN_DASH},
	{"shadowEtchedOutDash",	      SEPARATOR_SHADOW_ETCHED_OUT_DASH},
	{"shadowDoubleEtchedIn",      SEPARATOR_SHADOW_DOUBLE_ETCHED_IN},
	{"shadowDoubleEtchedOut",     SEPARATOR_SHADOW_DOUBLE_ETCHED_OUT},
	{"shadowDoubleEtchedInDash",  SEPARATOR_SHADOW_DOUBLE_ETCHED_IN_DASH},
	{"shadowDoubleEtchedOutDash", SEPARATOR_SHADOW_DOUBLE_ETCHED_OUT_DASH},
	{0,0}
      };

      int i;

      label += 3;
      for (i = 0; separator_names[i].name; ++i)
	if (strcmp (label, separator_names[i].name) == 0)
	  {
	    separator_p = 1;
	    *type = separator_names[i].type;

	    /* If separator type is not supported under Motif,
	       use a similar one.  */
	    if (motif_p && *type >= SEPARATOR_SHADOW_DOUBLE_ETCHED_IN)
	      *type -= 4;
	    break;
	  }
    }
  else if (strlen (label) > 3
	   && memcmp (label, "--", 2) == 0
	   && label[2] != '-')
    {
      /* Alternative, more Emacs-style names.  */
      static struct separator_table
      {
	const char *name;
	enum menu_separator type;
      }
      separator_names[] =
      {
	{"space",			 SEPARATOR_NO_LINE},
	{"no-line",			 SEPARATOR_NO_LINE},
	{"single-line",			 SEPARATOR_SINGLE_LINE},
	{"double-line",			 SEPARATOR_DOUBLE_LINE},
	{"single-dashed-line",		 SEPARATOR_SINGLE_DASHED_LINE},
	{"double-dashed-line",		 SEPARATOR_DOUBLE_DASHED_LINE},
	{"shadow-etched-in",		 SEPARATOR_SHADOW_ETCHED_IN},
	{"shadow-etched-out",		 SEPARATOR_SHADOW_ETCHED_OUT},
	{"shadow-etched-in-dash",	 SEPARATOR_SHADOW_ETCHED_IN_DASH},
	{"shadow-etched-out-dash",	 SEPARATOR_SHADOW_ETCHED_OUT_DASH},
	{"shadow-double-etched-in",	 SEPARATOR_SHADOW_DOUBLE_ETCHED_IN},
	{"shadow-double-etched-out",     SEPARATOR_SHADOW_DOUBLE_ETCHED_OUT},
	{"shadow-double-etched-in-dash", SEPARATOR_SHADOW_DOUBLE_ETCHED_IN_DASH},
	{"shadow-double-etched-out-dash",SEPARATOR_SHADOW_DOUBLE_ETCHED_OUT_DASH},
	{0,0}
      };

      int i;

      label += 2;
      for (i = 0; separator_names[i].name; ++i)
	if (strcmp (label, separator_names[i].name) == 0)
	  {
	    separator_p = 1;
	    *type = separator_names[i].type;

	    /* If separator type is not supported under Motif,
	       use a similar one.  */
	    if (motif_p && *type >= SEPARATOR_SHADOW_DOUBLE_ETCHED_IN)
	      *type -= 4;
	    break;
	  }
    }
  else
    {
      /* Old-style separator, maybe.  It's a separator if it contains
	 only dashes.  */
      while (*label == '-')
	++label;
      separator_p = *label == 0;
      *type = SEPARATOR_SHADOW_ETCHED_IN;
    }

  return separator_p;
}
