/* NeXT/Open/GNUstep / MacOSX Cocoa selection processing for emacs.
   Copyright (C) 1993-1994, 2005-2006, 2008-2012
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

/*
Originally by Carl Edman
Updated by Christian Limpach (chris@nice.ch)
OpenStep/Rhapsody port by Scott Bender (sbender@harmony-ds.com)
MacOSX/Aqua port by Christophe de Dinechin (descubes@earthlink.net)
GNUstep port and post-20 update by Adrian Robert (arobert@cogsci.ucsd.edu)
*/

/* This should be the first include, as it may set up #defines affecting
   interpretation of even the system includes. */
#include <config.h>
#include <setjmp.h>

#include "lisp.h"
#include "nsterm.h"
#include "termhooks.h"
#include "keyboard.h"

Lisp_Object QCLIPBOARD, QSECONDARY, QTEXT, QFILE_NAME;

static Lisp_Object Vselection_alist;

static Lisp_Object Qforeign_selection;

/* NSGeneralPboard is pretty much analogous to X11 CLIPBOARD */
NSString *NXPrimaryPboard;
NSString *NXSecondaryPboard;



/* ==========================================================================

    Internal utility functions

   ========================================================================== */


static NSString *
symbol_to_nsstring (Lisp_Object sym)
{
  CHECK_SYMBOL (sym);
  if (EQ (sym, QCLIPBOARD))   return NSGeneralPboard;
  if (EQ (sym, QPRIMARY))     return NXPrimaryPboard;
  if (EQ (sym, QSECONDARY))   return NXSecondaryPboard;
  if (EQ (sym, QTEXT))        return NSStringPboardType;
  return [NSString stringWithUTF8String: SDATA (XSYMBOL (sym)->xname)];
}

static NSPasteboard *
ns_symbol_to_pb (Lisp_Object symbol)
{
  return [NSPasteboard pasteboardWithName: symbol_to_nsstring (symbol)];
}

static Lisp_Object
ns_string_to_symbol (NSString *t)
{
  if ([t isEqualToString: NSGeneralPboard])
    return QCLIPBOARD;
  if ([t isEqualToString: NXPrimaryPboard])
    return QPRIMARY;
  if ([t isEqualToString: NXSecondaryPboard])
    return QSECONDARY;
  if ([t isEqualToString: NSStringPboardType])
    return QTEXT;
  if ([t isEqualToString: NSFilenamesPboardType])
    return QFILE_NAME;
  if ([t isEqualToString: NSTabularTextPboardType])
    return QTEXT;
  return intern ([t UTF8String]);
}


static Lisp_Object
clean_local_selection_data (Lisp_Object obj)
{
  if (CONSP (obj)
      && INTEGERP (XCAR (obj))
      && CONSP (XCDR (obj))
      && INTEGERP (XCAR (XCDR (obj)))
      && NILP (XCDR (XCDR (obj))))
    obj = Fcons (XCAR (obj), XCDR (obj));

  if (CONSP (obj)
      && INTEGERP (XCAR (obj))
      && INTEGERP (XCDR (obj)))
    {
      if (XINT (XCAR (obj)) == 0)
        return XCDR (obj);
      if (XINT (XCAR (obj)) == -1)
        return make_number (- XINT (XCDR (obj)));
    }

  if (VECTORP (obj))
    {
      int i;
      int size = ASIZE (obj);
      Lisp_Object copy;

      if (size == 1)
        return clean_local_selection_data (AREF (obj, 0));
      copy = Fmake_vector (make_number (size), Qnil);
      for (i = 0; i < size; i++)
        ASET (copy, i, clean_local_selection_data (AREF (obj, i)));
      return copy;
    }

  return obj;
}


static void
ns_declare_pasteboard (id pb)
{
  [pb declareTypes: ns_send_types owner: NSApp];
}


static void
ns_undeclare_pasteboard (id pb)
{
  [pb declareTypes: [NSArray array] owner: nil];
}


static void
ns_string_to_pasteboard_internal (id pb, Lisp_Object str, NSString *gtype)
{
  if (EQ (str, Qnil))
    {
      [pb declareTypes: [NSArray array] owner: nil];
    }
  else
    {
      char *utfStr;
      NSString *type, *nsStr;
      NSEnumerator *tenum;

      CHECK_STRING (str);

      utfStr = SDATA (str);
      nsStr = [[NSString alloc] initWithBytesNoCopy: utfStr
                                             length: SBYTES (str)
                                           encoding: NSUTF8StringEncoding
                                       freeWhenDone: NO];
      if (gtype == nil)
        {
          [pb declareTypes: ns_send_types owner: nil];
          tenum = [ns_send_types objectEnumerator];
          while ( (type = [tenum nextObject]) )
            [pb setString: nsStr forType: type];
        }
      else
        {
          [pb setString: nsStr forType: gtype];
        }
      [nsStr release];
    }
}


Lisp_Object
ns_get_local_selection (Lisp_Object selection_name,
                       Lisp_Object target_type)
{
  Lisp_Object local_value;
  Lisp_Object handler_fn, value, type, check;
  int count;

  local_value = assq_no_quit (selection_name, Vselection_alist);

  if (NILP (local_value)) return Qnil;

  count = specpdl_ptr - specpdl;
  specbind (Qinhibit_quit, Qt);
  CHECK_SYMBOL (target_type);
  handler_fn = Fcdr (Fassq (target_type, Vselection_converter_alist));
  if (!NILP (handler_fn))
    value = call3 (handler_fn, selection_name, target_type,
                XCAR (XCDR (local_value)));
  else
    value = Qnil;
  unbind_to (count, Qnil);

  check = value;
  if (CONSP (value) && SYMBOLP (XCAR (value)))
    {
      type = XCAR (value);
      check = XCDR (value);
    }

  if (STRINGP (check) || VECTORP (check) || SYMBOLP (check)
      || INTEGERP (check) || NILP (value))
    return value;

  if (CONSP (check)
      && INTEGERP (XCAR (check))
      && (INTEGERP (XCDR (check))||
          (CONSP (XCDR (check))
           && INTEGERP (XCAR (XCDR (check)))
           && NILP (XCDR (XCDR (check))))))
    return value;

  // FIXME: Why `quit' rather than `error'?
  Fsignal (Qquit, Fcons (build_string (
      "invalid data returned by selection-conversion function"),
                        Fcons (handler_fn, Fcons (value, Qnil))));
  // FIXME: Beware, `quit' can return!!
  return Qnil;
}


static Lisp_Object
ns_get_foreign_selection (Lisp_Object symbol, Lisp_Object target)
{
  id pb;
  pb = ns_symbol_to_pb (symbol);
  return pb != nil ? ns_string_from_pasteboard (pb) : Qnil;
}




/* ==========================================================================

    Functions used externally

   ========================================================================== */


Lisp_Object
ns_string_from_pasteboard (id pb)
{
  NSString *type, *str;
  const char *utfStr;
  int length;

  type = [pb availableTypeFromArray: ns_return_types];
  if (type == nil)
    {
      Fsignal (Qquit,
              Fcons (build_string ("empty or unsupported pasteboard type"),
                    Qnil));
    return Qnil;
    }

  /* get the string */
  if (! (str = [pb stringForType: type]))
    {
      NSData *data = [pb dataForType: type];
      if (data != nil)
        str = [[NSString alloc] initWithData: data
                                    encoding: NSUTF8StringEncoding];
      if (str != nil)
        {
          [str autorelease];
        }
      else
        {
          Fsignal (Qquit,
                  Fcons (build_string ("pasteboard doesn't contain valid data"),
                        Qnil));
          return Qnil;
        }
    }

  /* assume UTF8 */
  NS_DURING
    {
      /* EOL conversion: PENDING- is this too simple? */
      NSMutableString *mstr = [[str mutableCopy] autorelease];
      [mstr replaceOccurrencesOfString: @"\r\n" withString: @"\n"
            options: NSLiteralSearch range: NSMakeRange (0, [mstr length])];
      [mstr replaceOccurrencesOfString: @"\r" withString: @"\n"
            options: NSLiteralSearch range: NSMakeRange (0, [mstr length])];

      utfStr = [mstr UTF8String];
      length = [mstr lengthOfBytesUsingEncoding: NSUTF8StringEncoding];

#if ! defined (NS_IMPL_COCOA) || MAC_OS_X_VERSION_MAX_ALLOWED < MAC_OS_X_VERSION_10_4
      if (!utfStr) 
        {
          utfStr = [mstr cString];
          length = strlen (utfStr);
        }
#endif
    }
  NS_HANDLER
    {
      message1 ("ns_string_from_pasteboard: UTF8String failed\n");
#if defined (NS_IMPL_COCOA) && MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4
      utfStr = "Conversion failed";
#else
      utfStr = [str lossyCString];
#endif
      length = strlen (utfStr);
    }
  NS_ENDHANDLER

    return make_string (utfStr, length);
}


void
ns_string_to_pasteboard (id pb, Lisp_Object str)
{
  ns_string_to_pasteboard_internal (pb, str, nil);
}



/* ==========================================================================

    Lisp Defuns

   ========================================================================== */


DEFUN ("x-own-selection-internal", Fx_own_selection_internal,
       Sx_own_selection_internal, 2, 3, 0,
       doc: /* Assert an X selection of type SELECTION and value VALUE.
SELECTION is a symbol, typically `PRIMARY', `SECONDARY', or `CLIPBOARD'.
\(Those are literal upper-case symbol names, since that's what X expects.)
VALUE is typically a string, or a cons of two markers, but may be
anything that the functions on `selection-converter-alist' know about.

FRAME should be a frame that should own the selection.  If omitted or
nil, it defaults to the selected frame.

On Nextstep, FRAME is unused.  */)
     (Lisp_Object selection, Lisp_Object value, Lisp_Object frame)
{
  id pb;
  Lisp_Object old_value, new_value;
  NSString *type;
  Lisp_Object successful_p = Qnil, rest;
  Lisp_Object target_symbol, data;


  check_ns ();
  CHECK_SYMBOL (selection);
  if (NILP (value))
      error ("selection value may not be nil.");
  pb = ns_symbol_to_pb (selection);
  if (pb == nil) return Qnil;

  ns_declare_pasteboard (pb);
  old_value = assq_no_quit (selection, Vselection_alist);
  new_value = Fcons (selection, Fcons (value, Qnil));

  if (NILP (old_value))
    Vselection_alist = Fcons (new_value, Vselection_alist);
  else
    Fsetcdr (old_value, Fcdr (new_value));

  /* We only support copy of text.  */
  type = NSStringPboardType;
  target_symbol = ns_string_to_symbol (type);
  data = ns_get_local_selection (selection, target_symbol);
  if (!NILP (data))
    {
      if (STRINGP (data))
        ns_string_to_pasteboard_internal (pb, data, type);
      successful_p = Qt;
    }

  if (!EQ (Vns_sent_selection_hooks, Qunbound))
    {
      for (rest = Vns_sent_selection_hooks; CONSP (rest); rest = Fcdr (rest))
        call3 (Fcar (rest), selection, target_symbol, successful_p);
    }
  
  return value;
}


DEFUN ("x-disown-selection-internal", Fx_disown_selection_internal,
       Sx_disown_selection_internal, 1, 3, 0,
       doc: /* If we own the selection SELECTION, disown it.
Disowning it means there is no such selection.

Sets the last-change time for the selection to TIME-OBJECT (by default
the time of the last event).

TERMINAL should be a terminal object or a frame specifying the X
server to query.  If omitted or nil, that stands for the selected
frame's display, or the first available X display.

On Nextstep, the TIME-OBJECT and TERMINAL arguments are unused.
On MS-DOS, all this does is return non-nil if we own the selection.  */)
  (Lisp_Object selection, Lisp_Object time_object, Lisp_Object terminal)
{
  id pb;
  check_ns ();
  CHECK_SYMBOL (selection);
  if (NILP (assq_no_quit (selection, Vselection_alist))) return Qnil;

  pb = ns_symbol_to_pb (selection);
  if (pb != nil) ns_undeclare_pasteboard (pb);
  return Qt;
}


DEFUN ("x-selection-exists-p", Fx_selection_exists_p, Sx_selection_exists_p,
       0, 2, 0, doc: /* Whether there is an owner for the given X selection.
SELECTION should be the name of the selection in question, typically
one of the symbols `PRIMARY', `SECONDARY', or `CLIPBOARD'.  (X expects
these literal upper-case names.)  The symbol nil is the same as
`PRIMARY', and t is the same as `SECONDARY'.

TERMINAL should be a terminal object or a frame specifying the X
server to query.  If omitted or nil, that stands for the selected
frame's display, or the first available X display.

On Nextstep, TERMINAL is unused.  */)
     (Lisp_Object selection, Lisp_Object terminal)
{
  id pb;
  NSArray *types;

  check_ns ();
  CHECK_SYMBOL (selection);
  if (EQ (selection, Qnil)) selection = QPRIMARY;
  if (EQ (selection, Qt)) selection = QSECONDARY;
  pb = ns_symbol_to_pb (selection);
  if (pb == nil) return Qnil;
  
  types = [pb types];
  return ([types count] == 0) ? Qnil : Qt;
}


DEFUN ("x-selection-owner-p", Fx_selection_owner_p, Sx_selection_owner_p,
       0, 2, 0,
       doc: /* Whether the current Emacs process owns the given X Selection.
The arg should be the name of the selection in question, typically one of
the symbols `PRIMARY', `SECONDARY', or `CLIPBOARD'.
\(Those are literal upper-case symbol names, since that's what X expects.)
For convenience, the symbol nil is the same as `PRIMARY',
and t is the same as `SECONDARY'.

TERMINAL should be a terminal object or a frame specifying the X
server to query.  If omitted or nil, that stands for the selected
frame's display, or the first available X display.

On Nextstep, TERMINAL is unused.  */)
     (Lisp_Object selection, Lisp_Object terminal)
{
  check_ns ();
  CHECK_SYMBOL (selection);
  if (EQ (selection, Qnil)) selection = QPRIMARY;
  if (EQ (selection, Qt)) selection = QSECONDARY;
  return (NILP (Fassq (selection, Vselection_alist))) ? Qnil : Qt;
}


DEFUN ("x-get-selection-internal", Fx_get_selection_internal,
       Sx_get_selection_internal, 2, 4, 0,
       doc: /* Return text selected from some X window.
SELECTION-SYMBOL is typically `PRIMARY', `SECONDARY', or `CLIPBOARD'.
\(Those are literal upper-case symbol names, since that's what X expects.)
TARGET-TYPE is the type of data desired, typically `STRING'.

TIME-STAMP is the time to use in the XConvertSelection call for foreign
selections.  If omitted, defaults to the time for the last event.

TERMINAL should be a terminal object or a frame specifying the X
server to query.  If omitted or nil, that stands for the selected
frame's display, or the first available X display.

On Nextstep, TIME-STAMP and TERMINAL are unused.  */)
     (Lisp_Object selection_name, Lisp_Object target_type,
      Lisp_Object time_stamp, Lisp_Object terminal)
{
  Lisp_Object val;

  check_ns ();
  CHECK_SYMBOL (selection_name);
  CHECK_SYMBOL (target_type);
  val = ns_get_local_selection (selection_name, target_type);
  if (NILP (val))
    val = ns_get_foreign_selection (selection_name, target_type);
  if (CONSP (val) && SYMBOLP (Fcar (val)))
    {
      val = Fcdr (val);
      if (CONSP (val) && NILP (Fcdr (val)))
        val = Fcar (val);
    }
  val = clean_local_selection_data (val);
  return val;
}


DEFUN ("ns-get-selection-internal", Fns_get_selection_internal,
       Sns_get_selection_internal, 1, 1, 0,
       doc: /* Returns the value of SELECTION as a string.
SELECTION is a symbol, typically `PRIMARY', `SECONDARY', or `CLIPBOARD'. */)
     (Lisp_Object selection)
{
  id pb;
  check_ns ();
  pb = ns_symbol_to_pb (selection);
  return pb != nil ? ns_string_from_pasteboard (pb) : Qnil;
}


DEFUN ("ns-store-selection-internal", Fns_store_selection_internal,
       Sns_store_selection_internal, 2, 2, 0,
       doc: /* Sets the string value of SELECTION.
SELECTION is a symbol, typically `PRIMARY', `SECONDARY', or `CLIPBOARD'. */)
     (Lisp_Object selection, Lisp_Object string)
{
  id pb;
  check_ns ();
  pb = ns_symbol_to_pb (selection);
  if (pb != nil) ns_string_to_pasteboard (pb, string);
  return Qnil;
}


void
nxatoms_of_nsselect (void)
{
  NXPrimaryPboard = @"Selection";
  NXSecondaryPboard = @"Secondary";
}

void
syms_of_nsselect (void)
{
  QCLIPBOARD = intern_c_string ("CLIPBOARD");	staticpro (&QCLIPBOARD);
  QSECONDARY = intern_c_string ("SECONDARY");	staticpro (&QSECONDARY);
  QTEXT      = intern_c_string ("TEXT"); 	staticpro (&QTEXT);
  QFILE_NAME = intern_c_string ("FILE_NAME"); 	staticpro (&QFILE_NAME);

  defsubr (&Sx_disown_selection_internal);
  defsubr (&Sx_get_selection_internal);
  defsubr (&Sx_own_selection_internal);
  defsubr (&Sx_selection_exists_p);
  defsubr (&Sx_selection_owner_p);
  defsubr (&Sns_get_selection_internal);
  defsubr (&Sns_store_selection_internal);

  Vselection_alist = Qnil;
  staticpro (&Vselection_alist);

  DEFVAR_LISP ("ns-sent-selection-hooks", Vns_sent_selection_hooks,
               "A list of functions to be called when Emacs answers a selection request.\n\
The functions are called with four arguments:\n\
  - the selection name (typically `PRIMARY', `SECONDARY', or `CLIPBOARD');\n\
  - the selection-type which Emacs was asked to convert the\n\
    selection into before sending (for example, `STRING' or `LENGTH');\n\
  - a flag indicating success or failure for responding to the request.\n\
We might have failed (and declined the request) for any number of reasons,\n\
including being asked for a selection that we no longer own, or being asked\n\
to convert into a type that we don't know about or that is inappropriate.\n\
This hook doesn't let you change the behavior of Emacs's selection replies,\n\
it merely informs you that they have happened.");
  Vns_sent_selection_hooks = Qnil;

  DEFVAR_LISP ("selection-converter-alist", Vselection_converter_alist,
               "An alist associating X Windows selection-types with functions.\n\
These functions are called to convert the selection, with three args:\n\
the name of the selection (typically `PRIMARY', `SECONDARY', or `CLIPBOARD');\n\
a desired type to which the selection should be converted;\n\
and the local selection value (whatever was given to `x-own-selection').\n\
\n\
The function should return the value to send to the X server\n\
\(typically a string).  A return value of nil\n\
means that the conversion could not be done.\n\
A return value which is the symbol `NULL'\n\
means that a side-effect was executed,\n\
and there is no meaningful selection value.");
  Vselection_converter_alist = Qnil;

  DEFVAR_LISP ("ns-lost-selection-hooks", Vns_lost_selection_hooks,
               "A list of functions to be called when Emacs loses an X selection.\n\
\(This happens when some other X client makes its own selection\n\
or when a Lisp program explicitly clears the selection.)\n\
The functions are called with one argument, the selection type\n\
\(a symbol, typically `PRIMARY', `SECONDARY', or `CLIPBOARD').");
  Vns_lost_selection_hooks = Qnil;

  Qforeign_selection = intern_c_string ("foreign-selection");
  staticpro (&Qforeign_selection);
}

