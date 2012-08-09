/* The lwlib interface to Athena widgets.

Copyright (C) 1993 Chuck Thompson <cthomp@cs.uiuc.edu>
Copyright (C) 1994, 2001-2012 Free Software Foundation, Inc.

This file is part of the Lucid Widget Library.

The Lucid Widget Library is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
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

#include <stdio.h>
#include <setjmp.h>
#include <ctype.h>

#include <lisp.h>

#include "lwlib-Xaw.h"

#include <X11/StringDefs.h>
#include <X11/IntrinsicP.h>
#include <X11/CoreP.h>
#include <X11/Shell.h>

#ifdef HAVE_XAW3D
#include <X11/Xaw3d/Scrollbar.h>
#include <X11/Xaw3d/Paned.h>
#include <X11/Xaw3d/Dialog.h>
#include <X11/Xaw3d/Form.h>
#include <X11/Xaw3d/Command.h>
#include <X11/Xaw3d/Label.h>
#else /* !HAVE_XAW3D */
#include <X11/Xaw/Scrollbar.h>
#include <X11/Xaw/Paned.h>
#include <X11/Xaw/Dialog.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Label.h>
#endif /* HAVE_XAW3D */

#include <X11/Xatom.h>

#ifdef HAVE_XFT
#include <X11/Xft/Xft.h>

struct widget_xft_data
{
  Widget widget;
  XftFont *xft_font;
  XftDraw *xft_draw;
  XftColor xft_fg, xft_bg;
  int p_width, p_height;
  Pixmap p;
};


#endif

static void xaw_generic_callback (Widget widget,
                                  XtPointer closure,
                                  XtPointer call_data);


Boolean
lw_xaw_widget_p (Widget widget)
{
  return (XtIsSubclass (widget, scrollbarWidgetClass) ||
	  XtIsSubclass (widget, dialogWidgetClass));
}


#ifdef HAVE_XFT
static void
fill_xft_data (struct widget_xft_data *data, Widget widget, XftFont *font)
{
  Pixel bg, fg;
  XColor colors[2];

  data->widget = widget;
  data->xft_font = font;
  XtVaGetValues (widget,
                 XtNbackground, &bg,
                 XtNforeground, &fg,
                 NULL);

  colors[0].pixel = data->xft_fg.pixel = fg;
  colors[1].pixel = data->xft_bg.pixel = bg;
  XQueryColors (XtDisplay (widget),
                DefaultColormapOfScreen (XtScreen (widget)),
                colors, 2);

  data->xft_fg.color.alpha = 0xFFFF;
  data->xft_fg.color.red = colors[0].red;
  data->xft_fg.color.green = colors[0].green;
  data->xft_fg.color.blue = colors[0].blue;
  data->xft_bg.color.alpha = 0xFFFF;
  data->xft_bg.color.red = colors[1].red;
  data->xft_bg.color.green = colors[1].green;
  data->xft_bg.color.blue = colors[1].blue;

  data->p = None;
  data->xft_draw = 0;
  data->p_width = data->p_height = 0;
}

static XftFont*
openFont (Widget widget, char *name)
{
  char *fname = name;
  int screen = XScreenNumberOfScreen (XtScreen (widget));
  int len = strlen (fname), i = len-1;
  XftFont *fn;

  /* Try to convert Gtk-syntax (Sans 9) to Xft syntax Sans-9.  */
  while (i > 0 && isdigit (fname[i]))
    --i;
  if (fname[i] == ' ')
    {
      fname = xstrdup (name);
      fname[i] = '-';
    }

  fn = XftFontOpenName (XtDisplay (widget), screen, fname);
  if (fname != name) xfree (fname);

  return fn;
}

static int
get_text_width_and_height (Widget widget, char *text,
                           XftFont *xft_font,
                           int *height)
{
  int w = 0, h = 0;
  char *bp = text;
  
  while (bp && *bp != '\0')
    {
      XGlyphInfo gi;
      char *cp = strchr (bp, '\n');
      XftTextExtentsUtf8 (XtDisplay (widget), xft_font,
                          (FcChar8 *) bp,
                          cp ? cp - bp : strlen (bp),
                          &gi);
      bp = cp ? cp + 1 : NULL;
      h += xft_font->height;
      if (w < gi.width) w = gi.width;
    }

  *height = h;
  return w;
}

static void
draw_text (struct widget_xft_data *data, char *lbl, int inverse)
{
  Screen *sc = XtScreen (data->widget);
  int screen = XScreenNumberOfScreen (sc);
  int y = data->xft_font->ascent;
  int x = inverse ? 0 : 2;
  char *bp = lbl;

  data->xft_draw = XftDrawCreate (XtDisplay (data->widget),
                                  data->p,
                                  DefaultVisual (XtDisplay (data->widget),
                                                 screen),
                                  DefaultColormapOfScreen (sc));
  XftDrawRect (data->xft_draw,
               inverse ? &data->xft_fg : &data->xft_bg,
               0, 0, data->p_width, data->p_height);

  if (!inverse) y += 2;
  while (bp && *bp != '\0')
    {
      char *cp = strchr (bp, '\n');
      XftDrawStringUtf8 (data->xft_draw,
                         inverse ? &data->xft_bg : &data->xft_fg,
                         data->xft_font, x, y,
                         (FcChar8 *) bp,
                         cp ? cp - bp : strlen (bp));
      bp = cp ? cp + 1 : NULL;
      /* 1.2 gives reasonable line spacing.  */
      y += data->xft_font->height * 1.2;
    }

}


static void
set_text (struct widget_xft_data *data, Widget toplevel, char *lbl, int margin)
{
  int width, height;

  width = get_text_width_and_height (data->widget, lbl, data->xft_font,
                                     &height);
  data->p_width = width + margin;
  data->p_height = height + margin;

  data->p = XCreatePixmap (XtDisplay (data->widget),
                           XtWindow (toplevel),
                           data->p_width,
                           data->p_height,
                           DefaultDepthOfScreen (XtScreen (data->widget)));
  draw_text (data, lbl, 0);
  XtVaSetValues (data->widget, XtNbitmap, data->p, NULL);
}

static struct widget_xft_data *
find_xft_data (Widget widget)
{
  widget_instance *inst = NULL;
  Widget parent = XtParent (widget);
  struct widget_xft_data *data = NULL;
  int nr;
  while (parent && !inst) 
    {
      inst = lw_get_widget_instance (parent);
      parent = XtParent (parent);
    }
  if (!inst || !inst->xft_data || !inst->xft_data[0].xft_font) return 0;

  for (nr = 0; data == NULL && nr < inst->nr_xft_data; ++nr) 
    {
      if (inst->xft_data[nr].widget == widget) 
        data = &inst->xft_data[nr];
    }

  return data;
}

static void
command_press (Widget widget,
               XEvent* event,
               String *params,
               Cardinal *num_params)
{
  struct widget_xft_data *data = find_xft_data (widget);
  if (data) 
    {
      char *lbl;
      /* Since this isn't used for rectangle buttons, use it to for armed.  */
      XtVaSetValues (widget, XtNcornerRoundPercent, 1, NULL);

      XtVaGetValues (widget, XtNlabel, &lbl, NULL);
      draw_text (data, lbl, 1);
    }
}

static void
command_reset (Widget widget,
               XEvent* event,
               String *params,
               Cardinal *num_params)
{
  struct widget_xft_data *data = find_xft_data (widget);
  if (data) 
    {
      Dimension cr;
      XtVaGetValues (widget, XtNcornerRoundPercent, &cr, NULL);
      if (cr == 1) 
        {
          char *lbl;
          XtVaSetValues (widget, XtNcornerRoundPercent, 0, NULL);
          XtVaGetValues (widget, XtNlabel, &lbl, NULL);
          draw_text (data, lbl, 0);
        }
    }
}


#endif

void
xaw_update_one_widget (widget_instance *instance,
                       Widget widget,
		       widget_value *val,
                       Boolean deep_p)
{
  if (XtIsSubclass (widget, dialogWidgetClass))
    {

#ifdef HAVE_XFT
      if (instance->xft_data && instance->xft_data[0].xft_font)
        {
          set_text (&instance->xft_data[0], instance->parent,
                    val->contents->value, 10);
        }
#endif
      XtVaSetValues (widget, XtNlabel, val->contents->value, NULL);
    }
  else if (XtIsSubclass (widget, commandWidgetClass))
    {
      Dimension bw = 0;
      Arg al[10];
      int ac = 0;

      XtVaGetValues (widget, XtNborderWidth, &bw, NULL);
      if (bw == 0)
	/* Don't let buttons end up with 0 borderwidth, that's ugly...
	   Yeah, all this should really be done through app-defaults files
	   or fallback resources, but that's a whole different can of worms
	   that I don't feel like opening right now.  Making Athena widgets
	   not look like shit is just entirely too much work.
	 */
	{
	  XtSetArg (al[0], XtNborderWidth, 1);
	  XtSetValues (widget, al, 1);
	}

      XtSetSensitive (widget, val->enabled);
      XtSetArg (al[ac], XtNlabel, val->value);ac++;
      /* Force centered button text.  Se above. */
      XtSetArg (al[ac], XtNjustify, XtJustifyCenter);ac++;
#ifdef HAVE_XFT
      if (instance->xft_data && instance->xft_data[0].xft_font)
        {
          int th;
          int nr;
          for (nr = 0; nr < instance->nr_xft_data; ++nr)
            if (instance->xft_data[nr].widget == widget)
              break;
          if (nr < instance->nr_xft_data)
            {
              set_text (&instance->xft_data[nr], instance->parent,
                        val->value, 6);

              /* Must set internalHeight to twice the highlight thickness,
                 or else it gets overwritten by our pixmap.  Probably a bug.  */
              XtVaGetValues (widget, XtNhighlightThickness, &th, NULL);
              XtSetArg (al[ac], XtNinternalHeight, 2*th);ac++;
            }
        }
#endif
      XtSetValues (widget, al, ac);
      XtRemoveAllCallbacks (widget, XtNcallback);
      XtAddCallback (widget, XtNcallback, xaw_generic_callback, instance);
    }
}

void
xaw_update_one_value (widget_instance *instance,
                      Widget widget,
                      widget_value *val)
{
  /* This function is not used by the scrollbars and those are the only
     Athena widget implemented at the moment so do nothing. */
  return;
}

void
xaw_destroy_instance (widget_instance *instance)
{
#ifdef HAVE_XFT
  if (instance->xft_data) 
    {
      int i;
      for (i = 0; i < instance->nr_xft_data; ++i) 
        {
          if (instance->xft_data[i].xft_draw)
            XftDrawDestroy (instance->xft_data[i].xft_draw);
          if (instance->xft_data[i].p != None) 
            {
              XtVaSetValues (instance->xft_data[i].widget, XtNbitmap, None,
                             NULL);
              XFreePixmap (XtDisplay (instance->widget),
                           instance->xft_data[i].p);
            }
        }
      if (instance->xft_data[0].xft_font)
        XftFontClose (XtDisplay (instance->widget),
                      instance->xft_data[0].xft_font);
      xfree (instance->xft_data);
    }
#endif
  if (XtIsSubclass (instance->widget, dialogWidgetClass))
    /* Need to destroy the Shell too. */
    XtDestroyWidget (XtParent (instance->widget));
  else
    XtDestroyWidget (instance->widget);
}

void
xaw_popup_menu (Widget widget, XEvent *event)
{
  /* An Athena menubar has not been implemented. */
  return;
}

void
xaw_pop_instance (widget_instance *instance, Boolean up)
{
  Widget widget = instance->widget;

  if (up)
    {
      if (XtIsSubclass (widget, dialogWidgetClass))
	{
	  /* For dialogs, we need to call XtPopup on the parent instead
	     of calling XtManageChild on the widget.
	     Also we need to hack the shell's WM_PROTOCOLS to get it to
	     understand what the close box is supposed to do!!
	   */
	  Display *dpy = XtDisplay (widget);
	  Widget shell = XtParent (widget);
	  Atom props [2];
	  int i = 0;
	  props [i++] = XInternAtom (dpy, "WM_DELETE_WINDOW", False);
	  XChangeProperty (dpy, XtWindow (shell),
			   XInternAtom (dpy, "WM_PROTOCOLS", False),
			   XA_ATOM, 32, PropModeAppend,
			   (unsigned char *) props, i);

	  /* Center the widget in its parent.  Why isn't this kind of crap
	     done automatically?  I thought toolkits were supposed to make
	     life easier?
	   */
	  {
	    unsigned int x, y, w, h;
	    Widget topmost = instance->parent;
	    Arg args[2];

	    w = shell->core.width;
	    h = shell->core.height;
	    while (topmost->core.parent && XtIsRealized (topmost->core.parent))
	      topmost = topmost->core.parent;
	    if (topmost->core.width < w) x = topmost->core.x;
	    else x = topmost->core.x + ((topmost->core.width - w) / 2);
	    if (topmost->core.height < h) y = topmost->core.y;
	    else y = topmost->core.y + ((topmost->core.height - h) / 2);
	    /* Using XtMoveWidget caused the widget to come
	       out in the wrong place with vtwm.
	       Question of virtual vs real coords, perhaps.  */
	    XtSetArg (args[0], XtNx, x);
	    XtSetArg (args[1], XtNy, y);
	    XtSetValues (shell, args, 2);
	  }

	  /* Finally, pop it up. */
	  XtPopup (shell, XtGrabNonexclusive);
	}
      else
	XtManageChild (widget);
    }
  else
    {
      if (XtIsSubclass (widget, dialogWidgetClass))
	XtUnmanageChild (XtParent (widget));
      else
	XtUnmanageChild (widget);
    }
}


/* Dialog boxes */

static char overrideTrans[] =
	"<Message>WM_PROTOCOLS: lwlib_delete_dialog()";
/* Dialogs pop down on any key press */
static char dialogOverride[] =
       "<KeyPress>Escape:	lwlib_delete_dialog()";
static void wm_delete_window (Widget w,
                              XEvent *event,
                              String *params,
                              Cardinal *num_params);
static XtActionsRec xaw_actions [] = {
  {"lwlib_delete_dialog", wm_delete_window}
};
static Boolean actions_initted = False;

#ifdef HAVE_XFT
static XtActionsRec button_actions[] = 
  {
    { "my_reset", command_reset },
    { "my_press", command_press },
  };
char buttonTrans[] =
  "<Leave>: reset() my_reset()\n"
  "<Btn1Down>: set() my_press()\n"
  "<Btn1Up>:  my_reset() notify() unset()\n";
#endif

static Widget
make_dialog (char* name,
             Widget parent,
             Boolean pop_up_p,
             char* shell_title,
             char* icon_name,
             Boolean text_input_slot,
             Boolean radio_box,
             Boolean list,
             int left_buttons,
             int right_buttons,
             widget_instance *instance)
{
  Arg av [20];
  int ac = 0;
  int i, bc;
  char button_name [255];
  Widget shell;
  Widget dialog;
  Widget button;
  XtTranslations override;
#ifdef HAVE_XFT
  XftFont *xft_font = 0;
  XtTranslations button_override;
#endif

  if (! pop_up_p) abort (); /* not implemented */
  if (text_input_slot) abort (); /* not implemented */
  if (radio_box) abort (); /* not implemented */
  if (list) abort (); /* not implemented */

  if (! actions_initted)
    {
      XtAppContext app = XtWidgetToApplicationContext (parent);
      XtAppAddActions (app, xaw_actions,
		       sizeof (xaw_actions) / sizeof (xaw_actions[0]));
#ifdef HAVE_XFT
      XtAppAddActions (app, button_actions,
		       sizeof (button_actions) / sizeof (button_actions[0]));
#endif
      actions_initted = True;
    }

  override = XtParseTranslationTable (overrideTrans);

  ac = 0;
  XtSetArg (av[ac], XtNtitle, shell_title); ac++;
  XtSetArg (av[ac], XtNallowShellResize, True); ac++;

  /* Don't allow any geometry request from the user.  */
  XtSetArg (av[ac], XtNgeometry, 0); ac++;

  shell = XtCreatePopupShell ("dialog", transientShellWidgetClass,
			      parent, av, ac);
  XtOverrideTranslations (shell, override);

  ac = 0;
  dialog = XtCreateManagedWidget (name, dialogWidgetClass, shell, av, ac);
  override = XtParseTranslationTable (dialogOverride);
  XtOverrideTranslations (dialog, override);

#ifdef HAVE_XFT
  {
    int num;
    Widget *ch = NULL;
    Widget w = 0;
    XtVaGetValues (dialog,
                   XtNnumChildren, &num,
                   XtNchildren, &ch, NULL);
    for (i = 0; i < num; ++i) 
      {
        if (!XtIsSubclass (ch[i], commandWidgetClass)
            && XtIsSubclass (ch[i], labelWidgetClass))
          {
            w = ch[i];
            break;
          }
      }
    instance->xft_data = 0;
    instance->nr_xft_data = 0;
    if (w) 
      {
        XtResource rec[] = 
          { { "font", "Font", XtRString, sizeof(String), 0, XtRString,
              (XtPointer)"Sans-10" }};
        char *fontName = NULL;
        XtVaGetSubresources (dialog, &fontName, "Dialog", "dialog",
                             rec, 1, (String)NULL);
        if (fontName)
          {
            XFontStruct *xfn = XLoadQueryFont (XtDisplay (dialog), fontName);
            if (!xfn)
              xft_font = openFont (dialog, fontName);
            else
              XFreeFont (XtDisplay (dialog), xfn);
          }
        
        if (xft_font) 
          {
            instance->nr_xft_data = left_buttons + right_buttons + 1;
            instance->xft_data = calloc (instance->nr_xft_data,
                                         sizeof(*instance->xft_data));

            fill_xft_data (&instance->xft_data[0], w, xft_font);
          }
      }

    button_override = XtParseTranslationTable (buttonTrans);
  }
#endif

  bc = 0;
  button = 0;
  for (i = 0; i < left_buttons; i++)
    {
      ac = 0;
      XtSetArg (av [ac], XtNfromHoriz, button); ac++;
      XtSetArg (av [ac], XtNleft, XtChainLeft); ac++;
      XtSetArg (av [ac], XtNright, XtChainLeft); ac++;
      XtSetArg (av [ac], XtNtop, XtChainBottom); ac++;
      XtSetArg (av [ac], XtNbottom, XtChainBottom); ac++;
      XtSetArg (av [ac], XtNresizable, True); ac++;
#ifdef HAVE_XAW3D
      if (DefaultDepthOfScreen (XtScreen (dialog)) >= 16)
        {
          /* Turn of dithered shadow if we can.  Looks bad */
          XtSetArg (av [ac], "beNiceToColormap", False); ac++;
        }
#endif
      sprintf (button_name, "button%d", ++bc);
      button = XtCreateManagedWidget (button_name, commandWidgetClass,
				      dialog, av, ac);
#ifdef HAVE_XFT
      if (xft_font)
        {
          fill_xft_data (&instance->xft_data[bc], button, xft_font);
          XtOverrideTranslations (button, button_override);
        }
#endif
    }

  for (i = 0; i < right_buttons; i++)
    {
      ac = 0;
      XtSetArg (av [ac], XtNfromHoriz, button); ac++;
      if (i == 0) 
        {
          /* Separator to the other buttons. */
          XtSetArg (av [ac], XtNhorizDistance, 30); ac++;
        }
      XtSetArg (av [ac], XtNleft, XtChainRight); ac++;
      XtSetArg (av [ac], XtNright, XtChainRight); ac++;
      XtSetArg (av [ac], XtNtop, XtChainBottom); ac++;
      XtSetArg (av [ac], XtNbottom, XtChainBottom); ac++;
      XtSetArg (av [ac], XtNresizable, True); ac++;
#ifdef HAVE_XAW3D
      if (DefaultDepthOfScreen (XtScreen (dialog)) >= 16)
        {
          /* Turn of dithered shadow if we can.  Looks bad */
          XtSetArg (av [ac], "beNiceToColormap", False); ac++;
        }
#endif
      sprintf (button_name, "button%d", ++bc);
      button = XtCreateManagedWidget (button_name, commandWidgetClass,
				      dialog, av, ac);
#ifdef HAVE_XFT
      if (xft_font)
        {
          fill_xft_data (&instance->xft_data[bc], button, xft_font);
          XtOverrideTranslations (button, button_override);
        }
#endif
    }

  return dialog;
}

Widget
xaw_create_dialog (widget_instance *instance)
{
  char *name = instance->info->type;
  Widget parent = instance->parent;
  Widget widget;
  Boolean pop_up_p = instance->pop_up_p;
  char *shell_name = 0;
  char *icon_name = 0;
  Boolean text_input_slot = False;
  Boolean radio_box = False;
  Boolean list = False;
  int total_buttons;
  int left_buttons = 0;
  int right_buttons = 1;

  switch (name [0]) {
  case 'E': case 'e':
    icon_name = "dbox-error";
    shell_name = "Error";
    break;

  case 'I': case 'i':
    icon_name = "dbox-info";
    shell_name = "Information";
    break;

  case 'L': case 'l':
    list = True;
    icon_name = "dbox-question";
    shell_name = "Prompt";
    break;

  case 'P': case 'p':
    text_input_slot = True;
    icon_name = "dbox-question";
    shell_name = "Prompt";
    break;

  case 'Q': case 'q':
    icon_name = "dbox-question";
    shell_name = "Question";
    break;
  }

  total_buttons = name [1] - '0';

  if (name [3] == 'T' || name [3] == 't')
    {
      text_input_slot = False;
      radio_box = True;
    }
  else if (name [3])
    right_buttons = name [4] - '0';

  left_buttons = total_buttons - right_buttons;

  widget = make_dialog (name, parent, pop_up_p,
			shell_name, icon_name, text_input_slot, radio_box,
			list, left_buttons, right_buttons, instance);
  return widget;
}


static void
xaw_generic_callback (Widget widget, XtPointer closure, XtPointer call_data)
{
  widget_instance *instance = (widget_instance *) closure;
  Widget instance_widget;
  LWLIB_ID id;
  XtPointer user_data;

  lw_internal_update_other_instances (widget, closure, call_data);

  if (! instance)
    return;
  if (widget->core.being_destroyed)
    return;

  instance_widget = instance->widget;
  if (!instance_widget)
    return;

  id = instance->info->id;

  /* Damn!  Athena doesn't give us a way to hang our own data on the
     buttons, so we have to go find it...  I guess this assumes that
     all instances of a button have the same call data. */
  {
    widget_value *val = instance->info->val->contents;
    char *name = XtName (widget);
    while (val)
      {
	if (val->name && !strcmp (val->name, name))
	  break;
	val = val->next;
      }
    if (! val) abort ();
    user_data = val->call_data;
  }

  if (instance->info->selection_cb)
    instance->info->selection_cb (widget, id, user_data);
}

static void
wm_delete_window (Widget w,
                  XEvent *event,
                  String *params,
                  Cardinal *num_params)
{
  LWLIB_ID id;
  Cardinal nkids;
  int i;
  Widget *kids = 0;
  Widget widget = 0, shell;

  if (XtIsSubclass (w, dialogWidgetClass))
    shell = XtParent (w);
  else
    shell = w;

  if (! XtIsSubclass (shell, shellWidgetClass))
    abort ();
  XtVaGetValues (shell, XtNnumChildren, &nkids, NULL);
  XtVaGetValues (shell, XtNchildren, &kids, NULL);
  if (!kids || !*kids)
    abort ();
  for (i = 0; i < nkids; i++)
    {
      widget = kids[i];
      if (XtIsSubclass (widget, dialogWidgetClass))
	break;
    }
  if (! widget) return;

  id = lw_get_widget_id (widget);
  if (! id) abort ();

  {
    widget_info *info = lw_get_widget_info (id);
    if (! info) abort ();
    if (info->selection_cb)
      info->selection_cb (widget, id, (XtPointer) -1);
  }

  lw_destroy_all_widgets (id);
}



static Widget
xaw_create_main (widget_instance *instance)
{
  Arg al[1];
  int ac;

  /* Create a vertical Paned to hold menubar */
  ac = 0;
  XtSetArg (al[ac], XtNborderWidth, 0); ac++;
  return XtCreateWidget (instance->info->name, panedWidgetClass,
			 instance->parent, al, ac);
}

widget_creation_entry
xaw_creation_table [] =
{
  {"main",			xaw_create_main},
  {NULL, NULL}
};
