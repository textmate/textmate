/* Functions for handling font and other changes dynamically.

Copyright (C) 2009-2012  Free Software Foundation, Inc.

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

#include <float.h>
#include <limits.h>
#include <setjmp.h>
#include <fcntl.h>
#include "lisp.h"
#include "xterm.h"
#include "xsettings.h"
#include "frame.h"
#include "keyboard.h"
#include "blockinput.h"
#include "termhooks.h"
#include "termopts.h"

#include <X11/Xproto.h>

#ifdef HAVE_GSETTINGS
#include <glib-object.h>
#include <gio/gio.h>
#endif

#ifdef HAVE_GCONF
#include <gconf/gconf-client.h>
#endif

#ifdef HAVE_XFT
#include <X11/Xft/Xft.h>
#endif

static char *current_mono_font;
static char *current_font;
static struct x_display_info *first_dpyinfo;
static Lisp_Object Qmonospace_font_name, Qfont_name, Qfont_render,
  Qtool_bar_style;
static Lisp_Object current_tool_bar_style;

/* Store an config changed event in to the event queue.  */

static void
store_config_changed_event (Lisp_Object arg, Lisp_Object display_name)
{
  struct input_event event;
  EVENT_INIT (event);
  event.kind = CONFIG_CHANGED_EVENT;
  event.frame_or_window = display_name;
  event.arg = arg;
  kbd_buffer_store_event (&event);
}

/* Return non-zero if DPYINFO is still valid.  */
static int
dpyinfo_valid (struct x_display_info *dpyinfo)
{
  int found = 0;
  if (dpyinfo != NULL)
    {
      struct x_display_info *d;
      for (d = x_display_list; !found && d; d = d->next)
        found = d == dpyinfo && d->display == dpyinfo->display;
    }
  return found;
}

/* Store a monospace font change event if the monospaced font changed.  */

#if defined HAVE_XFT && (defined HAVE_GSETTINGS || defined HAVE_GCONF)
static void
store_monospaced_changed (const char *newfont)
{
  if (current_mono_font != NULL && strcmp (newfont, current_mono_font) == 0)
    return; /* No change. */

  xfree (current_mono_font);
  current_mono_font = xstrdup (newfont);

  if (dpyinfo_valid (first_dpyinfo) && use_system_font)
    {
      store_config_changed_event (Qmonospace_font_name,
                                  XCAR (first_dpyinfo->name_list_element));
    }
}
#endif

/* Store a font name change event if the font name changed.  */

#ifdef HAVE_XFT
static void
store_font_name_changed (const char *newfont)
{
  if (current_font != NULL && strcmp (newfont, current_font) == 0)
    return; /* No change. */

  xfree (current_font);
  current_font = xstrdup (newfont);

  if (dpyinfo_valid (first_dpyinfo))
    {
      store_config_changed_event (Qfont_name,
                                  XCAR (first_dpyinfo->name_list_element));
    }
}
#endif /* HAVE_XFT */

/* Map TOOL_BAR_STYLE from a string to its corresponding Lisp value.
   Return Qnil if TOOL_BAR_STYLE is not known.  */

static Lisp_Object
map_tool_bar_style (const char *tool_bar_style)
{
  Lisp_Object style = Qnil;
  if (tool_bar_style)
    {
      if (strcmp (tool_bar_style, "both") == 0)
        style = Qboth;
      else if (strcmp (tool_bar_style, "both-horiz") == 0)
        style = Qboth_horiz;
      else if (strcmp (tool_bar_style, "icons") == 0)
        style = Qimage;
      else if (strcmp (tool_bar_style, "text") == 0)
        style = Qtext;
    }

  return style;
}

/* Store a tool bar style change event if the tool bar style changed.  */

static void
store_tool_bar_style_changed (const char *newstyle,
                              struct x_display_info *dpyinfo)
{
  Lisp_Object style = map_tool_bar_style (newstyle);
  if (EQ (current_tool_bar_style, style))
    return; /* No change. */

  current_tool_bar_style = style;
  if (dpyinfo_valid (dpyinfo))
    store_config_changed_event (Qtool_bar_style,
                                XCAR (dpyinfo->name_list_element));
}


#define XSETTINGS_FONT_NAME       "Gtk/FontName"
#define XSETTINGS_TOOL_BAR_STYLE  "Gtk/ToolbarStyle"

enum {
  SEEN_AA         = 0x01,
  SEEN_HINTING    = 0x02,
  SEEN_RGBA       = 0x04,
  SEEN_LCDFILTER  = 0x08,
  SEEN_HINTSTYLE  = 0x10,
  SEEN_DPI        = 0x20,
  SEEN_FONT       = 0x40,
  SEEN_TB_STYLE   = 0x80,
};
struct xsettings
{
#ifdef HAVE_XFT
  FcBool aa, hinting;
  int rgba, lcdfilter, hintstyle;
  double dpi;

  char *font;
#endif

  char *tb_style;

  unsigned seen;
};

#ifdef HAVE_GSETTINGS
#define GSETTINGS_SCHEMA         "org.gnome.desktop.interface"
#define GSETTINGS_TOOL_BAR_STYLE "toolbar-style"

#ifdef HAVE_XFT
#define GSETTINGS_MONO_FONT  "monospace-font-name"
#define GSETTINGS_FONT_NAME  "font-name"
#endif


/* The single GSettings instance, or NULL if not connected to GSettings.  */

static GSettings *gsettings_client;

/* Callback called when something changed in GSettings.  */

static void
something_changed_gsettingsCB (GSettings *settings,
                               gchar *key,
                               gpointer user_data)
{
  GVariant *val;

  if (strcmp (key, GSETTINGS_TOOL_BAR_STYLE) == 0)
    {
      val = g_settings_get_value (settings, GSETTINGS_TOOL_BAR_STYLE);
      if (val)
        {
          g_variant_ref_sink (val);
          if (g_variant_is_of_type (val, G_VARIANT_TYPE_STRING))
            {
              const gchar *newstyle = g_variant_get_string (val, NULL);
              store_tool_bar_style_changed (newstyle, first_dpyinfo);
            }
          g_variant_unref (val);
        }
    }
#ifdef HAVE_XFT
  else if (strcmp (key, GSETTINGS_MONO_FONT) == 0)
    {
      val = g_settings_get_value (settings, GSETTINGS_MONO_FONT);
      if (val)
        {
          g_variant_ref_sink (val);
          if (g_variant_is_of_type (val, G_VARIANT_TYPE_STRING))
            {
              const gchar *newfont = g_variant_get_string (val, NULL);
              store_monospaced_changed (newfont);
            }
          g_variant_unref (val);
        }
    }
  else if (strcmp (key, GSETTINGS_FONT_NAME) == 0)
    {
      val = g_settings_get_value (settings, GSETTINGS_FONT_NAME);
      if (val)
        {
          g_variant_ref_sink (val);
          if (g_variant_is_of_type (val, G_VARIANT_TYPE_STRING))
            {
              const gchar *newfont = g_variant_get_string (val, NULL);
              store_font_name_changed (newfont);
            }
          g_variant_unref (val);
        }
    }
#endif /* HAVE_XFT */
}

#endif /* HAVE_GSETTINGS */

#ifdef HAVE_GCONF
#define GCONF_TOOL_BAR_STYLE "/desktop/gnome/interface/toolbar_style"
#ifdef HAVE_XFT
#define GCONF_MONO_FONT  "/desktop/gnome/interface/monospace_font_name"
#define GCONF_FONT_NAME  "/desktop/gnome/interface/font_name"
#endif

/* The single GConf instance, or NULL if not connected to GConf.  */

static GConfClient *gconf_client;

/* Callback called when something changed in GConf that we care about.  */

static void
something_changed_gconfCB (GConfClient *client,
                           guint cnxn_id,
                           GConfEntry *entry,
                           gpointer user_data)
{
  GConfValue *v = gconf_entry_get_value (entry);
  const char *key = gconf_entry_get_key (entry);

  if (!v || v->type != GCONF_VALUE_STRING || ! key) return;
  if (strcmp (key, GCONF_TOOL_BAR_STYLE) == 0)
    {
      const char *value = gconf_value_get_string (v);
      store_tool_bar_style_changed (value, first_dpyinfo);
    }
#ifdef HAVE_XFT
  else if (strcmp (key, GCONF_MONO_FONT) == 0)
    {
      const char *value = gconf_value_get_string (v);
      store_monospaced_changed (value);
    }
  else if (strcmp (key, GCONF_FONT_NAME) == 0)
    {
      const char *value = gconf_value_get_string (v);
      store_font_name_changed (value);
    }
#endif /* HAVE_XFT */
}

#endif /* HAVE_GCONF */

#ifdef HAVE_XFT

/* Older fontconfig versions don't have FC_LCD_*.  */
#ifndef FC_LCD_NONE
#define FC_LCD_NONE 0
#endif
#ifndef FC_LCD_DEFAULT
#define FC_LCD_DEFAULT 1
#endif
#ifndef FC_LCD_FILTER
#define FC_LCD_FILTER "lcdfilter"
#endif

#endif /* HAVE_XFT */

/* Find the window that contains the XSETTINGS property values.  */

static void
get_prop_window (struct x_display_info *dpyinfo)
{
  Display *dpy = dpyinfo->display;

  XGrabServer (dpy);
  dpyinfo->xsettings_window = XGetSelectionOwner (dpy,
                                                  dpyinfo->Xatom_xsettings_sel);
  if (dpyinfo->xsettings_window != None)
    /* Select events so we can detect if window is deleted or if settings
       are changed.  */
    XSelectInput (dpy, dpyinfo->xsettings_window,
                  PropertyChangeMask|StructureNotifyMask);

  XUngrabServer (dpy);
}

#define SWAP32(nr) (((nr) << 24) | (((nr) << 8) & 0xff0000)     \
                    | (((nr) >> 8) & 0xff00) | ((nr) >> 24))
#define SWAP16(nr) (((nr) << 8) | ((nr) >> 8))
#define PAD(nr)    (((nr) + 3) & ~3)

/* Parse xsettings and extract those that deal with Xft.
   See http://freedesktop.org/wiki/Specifications/XSettingsRegistry
   and http://standards.freedesktop.org/xsettings-spec/xsettings-spec-0.5.html.

   Layout of prop.  First is a header:

   bytes   type     what
   ------------------------------------
   1      CARD8    byte-order
   3               unused
   4      CARD32   SERIAL
   4      CARD32   N_SETTINGS

   Then N_SETTINGS records, with header:

   bytes   type          what
   ------------------------------------
   1      SETTING_TYPE  type (0 = integer, 1 = string, 2 RGB color).
   1                    unused
   2      CARD16        n == name-length
   n      STRING8       name
   p                    unused, p=pad_to_even_4(n)
   4      CARD32        last-change-serial

   and then the value, For string:

   bytes   type          what
   ------------------------------------
   4      CARD32        n = value-length
   n      STRING8       value
   p                    unused, p=pad_to_even_4(n)

   For integer:

   bytes   type          what
   ------------------------------------
   4      INT32         value

   For RGB color:

   bytes   type          what
   ------------------------------------
   2      CARD16        red
   2      CARD16        blue
   2      CARD16        green
   2      CARD16        alpha

   Returns non-zero if some Xft settings was seen, zero otherwise.
*/

static int
parse_settings (unsigned char *prop,
                long unsigned int bytes,
                struct xsettings *settings)
{
  Lisp_Object byteorder = Fbyteorder ();
  int my_bo = XFASTINT (byteorder) == 'B' ? MSBFirst : LSBFirst;
  int that_bo = prop[0];
  CARD32 n_settings;
  int bytes_parsed = 0;
  int settings_seen = 0;
  int i = 0;

  /* First 4 bytes is a serial number, skip that.  */

  if (bytes < 12) return BadLength;
  memcpy (&n_settings, prop+8, 4);
  if (my_bo != that_bo) n_settings = SWAP32 (n_settings);
  bytes_parsed = 12;

  memset (settings, 0, sizeof (*settings));

  while (bytes_parsed+4 < bytes && settings_seen < 7
         && i < n_settings)
    {
      int type = prop[bytes_parsed++];
      CARD16 nlen;
      CARD32 vlen, ival = 0;
      char name[128]; /* The names we are looking for are not this long.  */
      char sval[128]; /* The values we are looking for are not this long.  */
      int want_this;
      int to_cpy;

      sval[0] = '\0';
      ++i;
      ++bytes_parsed; /* Padding */

      memcpy (&nlen, prop+bytes_parsed, 2);
      bytes_parsed += 2;
      if (my_bo != that_bo) nlen = SWAP16 (nlen);
      if (bytes_parsed+nlen > bytes) return BadLength;
      to_cpy = nlen > 127 ? 127 : nlen;
      memcpy (name, prop+bytes_parsed, to_cpy);
      name[to_cpy] = '\0';

      bytes_parsed += nlen;
      bytes_parsed = PAD (bytes_parsed);

      bytes_parsed += 4; /* Skip serial for this value */
      if (bytes_parsed > bytes) return BadLength;

      want_this =
#ifdef HAVE_XFT
        (nlen > 6 && strncmp (name, "Xft/", 4) == 0)
        || strcmp (XSETTINGS_FONT_NAME, name) == 0
        ||
#endif
        strcmp (XSETTINGS_TOOL_BAR_STYLE, name) == 0;

      switch (type)
        {
        case 0: /* Integer */
          if (bytes_parsed+4 > bytes) return BadLength;
          if (want_this)
            {
              memcpy (&ival, prop+bytes_parsed, 4);
              if (my_bo != that_bo) ival = SWAP32 (ival);
            }
          bytes_parsed += 4;
          break;

        case 1: /* String */
          if (bytes_parsed+4 > bytes) return BadLength;
          memcpy (&vlen, prop+bytes_parsed, 4);
          bytes_parsed += 4;
          if (my_bo != that_bo) vlen = SWAP32 (vlen);
          if (want_this)
            {
              to_cpy = vlen > 127 ? 127 : vlen;
              memcpy (sval, prop+bytes_parsed, to_cpy);
              sval[to_cpy] = '\0';
            }
          bytes_parsed += vlen;
          bytes_parsed = PAD (bytes_parsed);
          break;

        case 2: /* RGB value */
          /* No need to parse this */
          if (bytes_parsed+8 > bytes) return BadLength;
          bytes_parsed += 8; /* 4 values (r, b, g, alpha), 2 bytes each.  */
          break;

        default: /* Parse Error */
          return BadValue;
        }

      if (want_this)
        {
          ++settings_seen;
          if (strcmp (name, XSETTINGS_TOOL_BAR_STYLE) == 0)
            {
              settings->tb_style = xstrdup (sval);
              settings->seen |= SEEN_TB_STYLE;
            }
#ifdef HAVE_XFT
          else if (strcmp (name, XSETTINGS_FONT_NAME) == 0)
            {
              settings->font = xstrdup (sval);
              settings->seen |= SEEN_FONT;
            }
          else if (strcmp (name, "Xft/Antialias") == 0)
            {
              settings->seen |= SEEN_AA;
              settings->aa = ival != 0;
            }
          else if (strcmp (name, "Xft/Hinting") == 0)
            {
              settings->seen |= SEEN_HINTING;
              settings->hinting = ival != 0;
            }
# ifdef FC_HINT_STYLE
          else if (strcmp (name, "Xft/HintStyle") == 0)
            {
              settings->seen |= SEEN_HINTSTYLE;
              if (strcmp (sval, "hintnone") == 0)
                settings->hintstyle = FC_HINT_NONE;
              else if (strcmp (sval, "hintslight") == 0)
                settings->hintstyle = FC_HINT_SLIGHT;
              else if (strcmp (sval, "hintmedium") == 0)
                settings->hintstyle = FC_HINT_MEDIUM;
              else if (strcmp (sval, "hintfull") == 0)
                settings->hintstyle = FC_HINT_FULL;
              else
                settings->seen &= ~SEEN_HINTSTYLE;
            }
# endif
          else if (strcmp (name, "Xft/RGBA") == 0)
            {
              settings->seen |= SEEN_RGBA;
              if (strcmp (sval, "none") == 0)
                settings->rgba = FC_RGBA_NONE;
              else if (strcmp (sval, "rgb") == 0)
                settings->rgba = FC_RGBA_RGB;
              else if (strcmp (sval, "bgr") == 0)
                settings->rgba = FC_RGBA_BGR;
              else if (strcmp (sval, "vrgb") == 0)
                settings->rgba = FC_RGBA_VRGB;
              else if (strcmp (sval, "vbgr") == 0)
                settings->rgba = FC_RGBA_VBGR;
              else
                settings->seen &= ~SEEN_RGBA;
            }
          else if (strcmp (name, "Xft/DPI") == 0)
            {
              settings->seen |= SEEN_DPI;
              settings->dpi = (double)ival/1024.0;
            }
          else if (strcmp (name, "Xft/lcdfilter") == 0)
            {
              settings->seen |= SEEN_LCDFILTER;
              if (strcmp (sval, "none") == 0)
                settings->lcdfilter = FC_LCD_NONE;
              else if (strcmp (sval, "lcddefault") == 0)
                settings->lcdfilter = FC_LCD_DEFAULT;
              else
                settings->seen &= ~SEEN_LCDFILTER;
            }
#endif /* HAVE_XFT */
        }
    }

  return settings_seen;
}

/* Read settings from the XSettings property window on display for DPYINFO.
   Store settings read in SETTINGS.
   Return non-zero if successful, zero if not.  */

static int
read_settings (struct x_display_info *dpyinfo, struct xsettings *settings)
{
  Atom act_type;
  int act_form;
  unsigned long nitems, bytes_after;
  unsigned char *prop = NULL;
  Display *dpy = dpyinfo->display;
  int rc;

  x_catch_errors (dpy);
  rc = XGetWindowProperty (dpy,
                           dpyinfo->xsettings_window,
                           dpyinfo->Xatom_xsettings_prop,
                           0, LONG_MAX, False, AnyPropertyType,
                           &act_type, &act_form, &nitems, &bytes_after,
                           &prop);

  if (rc == Success && prop != NULL && act_form == 8 && nitems > 0
      && act_type == dpyinfo->Xatom_xsettings_prop)
    rc = parse_settings (prop, nitems, settings);

  XFree (prop);

  x_uncatch_errors ();

  return rc != 0;
}

/* Apply Xft settings in SETTINGS to the Xft library.
   If SEND_EVENT_P is non-zero store a Lisp event that Xft settings changed.  */

static void
apply_xft_settings (struct x_display_info *dpyinfo,
                    int send_event_p,
                    struct xsettings *settings)
{
#ifdef HAVE_XFT
  FcPattern *pat;
  struct xsettings oldsettings;
  int changed = 0;

  memset (&oldsettings, 0, sizeof (oldsettings));
  pat = FcPatternCreate ();
  XftDefaultSubstitute (dpyinfo->display,
                        XScreenNumberOfScreen (dpyinfo->screen),
                        pat);
  FcPatternGetBool (pat, FC_ANTIALIAS, 0, &oldsettings.aa);
  FcPatternGetBool (pat, FC_HINTING, 0, &oldsettings.hinting);
#ifdef FC_HINT_STYLE
  FcPatternGetInteger (pat, FC_HINT_STYLE, 0, &oldsettings.hintstyle);
#endif
  FcPatternGetInteger (pat, FC_LCD_FILTER, 0, &oldsettings.lcdfilter);
  FcPatternGetInteger (pat, FC_RGBA, 0, &oldsettings.rgba);
  FcPatternGetDouble (pat, FC_DPI, 0, &oldsettings.dpi);

  if ((settings->seen & SEEN_AA) != 0 && oldsettings.aa != settings->aa)
    {
      FcPatternDel (pat, FC_ANTIALIAS);
      FcPatternAddBool (pat, FC_ANTIALIAS, settings->aa);
      ++changed;
      oldsettings.aa = settings->aa;
    }

  if ((settings->seen & SEEN_HINTING) != 0
      && oldsettings.hinting != settings->hinting)
    {
      FcPatternDel (pat, FC_HINTING);
      FcPatternAddBool (pat, FC_HINTING, settings->hinting);
      ++changed;
      oldsettings.hinting = settings->hinting;
    }
  if ((settings->seen & SEEN_RGBA) != 0 && oldsettings.rgba != settings->rgba)
    {
      FcPatternDel (pat, FC_RGBA);
      FcPatternAddInteger (pat, FC_RGBA, settings->rgba);
      oldsettings.rgba = settings->rgba;
      ++changed;
    }

  /* Older fontconfig versions don't have FC_LCD_FILTER. */
  if ((settings->seen & SEEN_LCDFILTER) != 0
      && oldsettings.lcdfilter != settings->lcdfilter)
    {
      FcPatternDel (pat, FC_LCD_FILTER);
      FcPatternAddInteger (pat, FC_LCD_FILTER, settings->lcdfilter);
      ++changed;
      oldsettings.lcdfilter = settings->lcdfilter;
    }

#ifdef FC_HINT_STYLE
  if ((settings->seen & SEEN_HINTSTYLE) != 0
      && oldsettings.hintstyle != settings->hintstyle)
    {
      FcPatternDel (pat, FC_HINT_STYLE);
      FcPatternAddInteger (pat, FC_HINT_STYLE, settings->hintstyle);
      ++changed;
      oldsettings.hintstyle = settings->hintstyle;
    }
#endif

  if ((settings->seen & SEEN_DPI) != 0 && oldsettings.dpi != settings->dpi
      && settings->dpi > 0)
    {
      Lisp_Object frame, tail;

      FcPatternDel (pat, FC_DPI);
      FcPatternAddDouble (pat, FC_DPI, settings->dpi);
      ++changed;
      oldsettings.dpi = settings->dpi;

      /* Change the DPI on this display and all frames on the display.  */
      dpyinfo->resy = dpyinfo->resx = settings->dpi;
      FOR_EACH_FRAME (tail, frame)
        if (FRAME_X_P (XFRAME (frame))
            && FRAME_X_DISPLAY_INFO (XFRAME (frame)) == dpyinfo)
          XFRAME (frame)->resy = XFRAME (frame)->resx = settings->dpi;
    }

  if (changed)
    {
      static char const format[] =
	"Antialias: %d, Hinting: %d, RGBA: %d, LCDFilter: %d, "
	"Hintstyle: %d, DPI: %lf";
      enum
      {
	d_formats = 5,
	d_growth = INT_BUFSIZE_BOUND (int) - sizeof "%d",
	lf_formats = 1,
	max_f_integer_digits = DBL_MAX_10_EXP + 1,
	f_precision = 6,
	lf_growth = (sizeof "-." + max_f_integer_digits + f_precision
		     - sizeof "%lf")
      };
      char buf[sizeof format + d_formats * d_growth + lf_formats * lf_growth];

      XftDefaultSet (dpyinfo->display, pat);
      if (send_event_p)
        store_config_changed_event (Qfont_render,
                                    XCAR (dpyinfo->name_list_element));
      sprintf (buf, format, oldsettings.aa, oldsettings.hinting,
	       oldsettings.rgba, oldsettings.lcdfilter,
	       oldsettings.hintstyle, oldsettings.dpi);
      Vxft_settings = build_string (buf);
    }
  else
    FcPatternDestroy (pat);
#endif /* HAVE_XFT */
}

/* Read XSettings from the display for DPYINFO.
   If SEND_EVENT_P is non-zero store a Lisp event settings that changed.  */

static void
read_and_apply_settings (struct x_display_info *dpyinfo, int send_event_p)
{
  struct xsettings settings;

  if (!read_settings (dpyinfo, &settings))
    return;

  apply_xft_settings (dpyinfo, True, &settings);
  if (settings.seen & SEEN_TB_STYLE)
    {
      if (send_event_p)
        store_tool_bar_style_changed (settings.tb_style, dpyinfo);
      else
        current_tool_bar_style = map_tool_bar_style (settings.tb_style);
      xfree (settings.tb_style);
    }
#ifdef HAVE_XFT
  if (settings.seen & SEEN_FONT)
    {
      if (send_event_p)
        store_font_name_changed (settings.font);
      else
        {
          xfree (current_font);
          current_font = xstrdup (settings.font);
        }
      xfree (settings.font);
    }
#endif
}

/* Check if EVENT for the display in DPYINFO is XSettings related.  */

void
xft_settings_event (struct x_display_info *dpyinfo, XEvent *event)
{
  int check_window_p = 0;
  int apply_settings = 0;

  switch (event->type)
    {
    case DestroyNotify:
      if (dpyinfo->xsettings_window == event->xany.window)
        check_window_p = 1;
      break;

    case ClientMessage:
      if (event->xclient.message_type == dpyinfo->Xatom_xsettings_mgr
          && event->xclient.data.l[1] == dpyinfo->Xatom_xsettings_sel
          && event->xclient.window == dpyinfo->root_window)
        check_window_p = 1;
      break;

    case PropertyNotify:
      if (event->xproperty.window == dpyinfo->xsettings_window
          && event->xproperty.state == PropertyNewValue
          && event->xproperty.atom == dpyinfo->Xatom_xsettings_prop)
        apply_settings = 1;
      break;
    }


  if (check_window_p)
    {
      dpyinfo->xsettings_window = None;
      get_prop_window (dpyinfo);
      if (dpyinfo->xsettings_window != None)
        apply_settings = 1;
    }

  if (apply_settings)
    read_and_apply_settings (dpyinfo, True);
}

/* Initialize GSettings and read startup values.  */

static void
init_gsettings (void)
{
#ifdef HAVE_GSETTINGS
  GVariant *val;
  const gchar *const *schemas;
  int schema_found = 0;

#ifdef HAVE_G_TYPE_INIT
  g_type_init ();
#endif

  schemas = g_settings_list_schemas ();
  if (schemas == NULL) return;
  while (! schema_found && *schemas != NULL)
    schema_found = strcmp (*schemas++, GSETTINGS_SCHEMA) == 0;
  if (!schema_found) return;

  gsettings_client = g_settings_new (GSETTINGS_SCHEMA);
  if (!gsettings_client) return;
  g_object_ref_sink (G_OBJECT (gsettings_client));
  g_signal_connect (G_OBJECT (gsettings_client), "changed",
                    G_CALLBACK (something_changed_gsettingsCB), NULL);

  val = g_settings_get_value (gsettings_client, GSETTINGS_TOOL_BAR_STYLE);
  if (val)
    {
      g_variant_ref_sink (val);
      if (g_variant_is_of_type (val, G_VARIANT_TYPE_STRING))
        current_tool_bar_style
          = map_tool_bar_style (g_variant_get_string (val, NULL));
      g_variant_unref (val);
    }

#ifdef HAVE_XFT
  val = g_settings_get_value (gsettings_client, GSETTINGS_MONO_FONT);
  if (val)
    {
      g_variant_ref_sink (val);
      if (g_variant_is_of_type (val, G_VARIANT_TYPE_STRING))
        current_mono_font = xstrdup (g_variant_get_string (val, NULL));
      g_variant_unref (val);
    }

  val = g_settings_get_value (gsettings_client, GSETTINGS_FONT_NAME);
  if (val)
    {
      g_variant_ref_sink (val);
      if (g_variant_is_of_type (val, G_VARIANT_TYPE_STRING))
        current_font = xstrdup (g_variant_get_string (val, NULL));
      g_variant_unref (val);
    }
#endif /* HAVE_XFT */

#endif /* HAVE_GSETTINGS */
}

/* Init GConf and read startup values.  */

static void
init_gconf (void)
{
#if defined (HAVE_GCONF)
  char *s;

#ifdef HAVE_G_TYPE_INIT
  g_type_init ();
#endif

  gconf_client = gconf_client_get_default ();
  gconf_client_set_error_handling (gconf_client, GCONF_CLIENT_HANDLE_NONE);
  gconf_client_add_dir (gconf_client,
                        GCONF_TOOL_BAR_STYLE,
                        GCONF_CLIENT_PRELOAD_ONELEVEL,
                        NULL);
  gconf_client_notify_add (gconf_client,
                           GCONF_TOOL_BAR_STYLE,
                           something_changed_gconfCB,
                           NULL, NULL, NULL);

  s = gconf_client_get_string (gconf_client, GCONF_TOOL_BAR_STYLE, NULL);
  if (s)
    {
      current_tool_bar_style = map_tool_bar_style (s);
      g_free (s);
    }

#ifdef HAVE_XFT
  s = gconf_client_get_string (gconf_client, GCONF_MONO_FONT, NULL);
  if (s)
    {
      current_mono_font = xstrdup (s);
      g_free (s);
    }
  s = gconf_client_get_string (gconf_client, GCONF_FONT_NAME, NULL);
  if (s)
    {
      current_font = xstrdup (s);
      g_free (s);
    }
  gconf_client_add_dir (gconf_client,
                        GCONF_MONO_FONT,
                        GCONF_CLIENT_PRELOAD_ONELEVEL,
                        NULL);
  gconf_client_notify_add (gconf_client,
                           GCONF_MONO_FONT,
                           something_changed_gconfCB,
                           NULL, NULL, NULL);
  gconf_client_add_dir (gconf_client,
                        GCONF_FONT_NAME,
                        GCONF_CLIENT_PRELOAD_ONELEVEL,
                        NULL);
  gconf_client_notify_add (gconf_client,
                           GCONF_FONT_NAME,
                           something_changed_gconfCB,
                           NULL, NULL, NULL);
#endif /* HAVE_XFT */
#endif /* HAVE_GCONF */
}

/* Init Xsettings and read startup values.  */

static void
init_xsettings (struct x_display_info *dpyinfo)
{
  Display *dpy = dpyinfo->display;

  BLOCK_INPUT;

  /* Select events so we can detect client messages sent when selection
     owner changes.  */
  XSelectInput (dpy, dpyinfo->root_window, StructureNotifyMask);

  get_prop_window (dpyinfo);
  if (dpyinfo->xsettings_window != None)
    read_and_apply_settings (dpyinfo, False);

  UNBLOCK_INPUT;
}

void
xsettings_initialize (struct x_display_info *dpyinfo)
{
  if (first_dpyinfo == NULL) first_dpyinfo = dpyinfo;
  init_gconf ();
  init_xsettings (dpyinfo);
  init_gsettings ();
}

/* Return the system monospaced font.
   May be NULL if not known.  */

const char *
xsettings_get_system_font (void)
{
  return current_mono_font;
}

#ifdef USE_LUCID
/* Return the system font.
   May be NULL if not known.  */

const char *
xsettings_get_system_normal_font (void)
{
  return current_font;
}
#endif

DEFUN ("font-get-system-normal-font", Ffont_get_system_normal_font,
       Sfont_get_system_normal_font,
       0, 0, 0,
       doc: /* Get the system default application font. */)
  (void)
{
  return current_font ? build_string (current_font) : Qnil;
}

DEFUN ("font-get-system-font", Ffont_get_system_font, Sfont_get_system_font,
       0, 0, 0,
       doc: /* Get the system default fixed width font. */)
  (void)
{
  return current_mono_font ? build_string (current_mono_font) : Qnil;
}

DEFUN ("tool-bar-get-system-style", Ftool_bar_get_system_style,
       Stool_bar_get_system_style, 0, 0, 0,
       doc: /* Get the system tool bar style.
If no system tool bar style is known, return `tool-bar-style' if set to a
known style.  Otherwise return image.  */)
  (void)
{
  if (EQ (Vtool_bar_style, Qimage)
      || EQ (Vtool_bar_style, Qtext)
      || EQ (Vtool_bar_style, Qboth)
      || EQ (Vtool_bar_style, Qboth_horiz)
      || EQ (Vtool_bar_style, Qtext_image_horiz))
    return Vtool_bar_style;
  if (!NILP (current_tool_bar_style))
    return current_tool_bar_style;
  return Qimage;
}

void
syms_of_xsettings (void)
{
  current_mono_font = NULL;
  current_font = NULL;
  first_dpyinfo = NULL;
#ifdef HAVE_GSETTINGS
  gsettings_client = NULL;
#endif
#ifdef HAVE_GCONF
  gconf_client = NULL;
#endif

  DEFSYM (Qmonospace_font_name, "monospace-font-name");
  DEFSYM (Qfont_name, "font-name");
  DEFSYM (Qfont_render, "font-render");
  defsubr (&Sfont_get_system_font);
  defsubr (&Sfont_get_system_normal_font);

  DEFVAR_BOOL ("font-use-system-font", use_system_font,
    doc: /* *Non-nil means to apply the system defined font dynamically.
When this is non-nil and the system defined fixed width font changes, we
update frames dynamically.
If this variable is nil, Emacs ignores system font changes.  */);
  use_system_font = 0;

  DEFVAR_LISP ("xft-settings", Vxft_settings,
               doc: /* Font settings applied to Xft.  */);
  Vxft_settings = make_string ("", 0);

#ifdef HAVE_XFT
  Fprovide (intern_c_string ("font-render-setting"), Qnil);
#if defined (HAVE_GCONF) || defined (HAVE_GSETTINGS)
  Fprovide (intern_c_string ("system-font-setting"), Qnil);
#endif
#endif

  current_tool_bar_style = Qnil;
  DEFSYM (Qtool_bar_style, "tool-bar-style");
  defsubr (&Stool_bar_get_system_style);

  Fprovide (intern_c_string ("dynamic-setting"), Qnil);
}
