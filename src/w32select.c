/* Selection processing for Emacs on the Microsoft W32 API.

Copyright (C) 1993-1994, 2001-2012  Free Software Foundation, Inc.

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

/* Written by Kevin Gallo, Benjamin Riefenstahl */


/*
 * Notes on usage of selection-coding-system and
 * next-selection-coding-system on MS Windows:
 *
 * The selection coding system variables apply only to the version of
 * the clipboard data that is closest in type, i.e. when a 16-bit
 * Unicode coding system is given, they apply to he Unicode clipboard
 * (CF_UNICODETEXT), when a well-known console codepage is given, they
 * apply to the console version of the clipboard data (CF_OEMTEXT),
 * else they apply to the normal 8-bit text clipboard (CF_TEXT).
 *
 * When pasting (getting data from the OS), the clipboard format that
 * matches the {next-}selection-coding-system is retrieved.  If
 * Unicode is requested, but not available, 8-bit text (CF_TEXT) is
 * used.  In all other cases the OS will transparently convert
 * formats, so no other fallback is needed.
 *
 * When copying or cutting (sending data to the OS), the data is
 * announced and stored internally, but only actually rendered on
 * request.  The requestor determines the format provided.  The
 * {next-}selection-coding-system is only used, when its corresponding
 * clipboard type matches the type requested.
 *
 * Scenarios to use the facilities for customizing the selection
 * coding system are:
 *
 *   ;; Generally use KOI8-R instead of the russian MS codepage for
 *   ;; the 8-bit clipboard.
 *   (set-selection-coding-system 'koi8-r-dos)
 *
 * Or
 *
 *   ;; Create a special clipboard copy function that uses codepage
 *   ;; 1253 (Greek) to copy Greek text to a specific non-Unicode
 *   ;; application.
 *   (defun greek-copy (beg end)
 *     (interactive "r")
 *     (set-next-selection-coding-system 'cp1253-dos)
 *     (copy-region-as-kill beg end))
 *   (global-set-key "\C-c\C-c" 'greek-copy)
 */

/*
 * Ideas for further directions:
 *
 * The encoding and decoding routines could be moved to Lisp code
 * similar to how xselect.c does it (using well-known routine names
 * for the delayed rendering).  If the definition of which clipboard
 * types should be supported is also moved to Lisp, functionality
 * could be expanded to CF_HTML, CF_RTF and maybe other types.
 */

#include <config.h>
#include <setjmp.h>
#include "lisp.h"
#include "w32term.h"	/* for all of the w32 includes */
#include "w32heap.h"	/* os_subtype */
#include "blockinput.h"
#include "charset.h"
#include "coding.h"
#include "composite.h"


static HGLOBAL convert_to_handle_as_ascii (void);
static HGLOBAL convert_to_handle_as_coded (Lisp_Object coding_system);
static Lisp_Object render (Lisp_Object oformat);
static Lisp_Object render_locale (void);
static Lisp_Object render_all (Lisp_Object ignore);
static void run_protected (Lisp_Object (*code) (Lisp_Object), Lisp_Object arg);
static Lisp_Object lisp_error_handler (Lisp_Object error);
static LRESULT CALLBACK owner_callback (HWND win, UINT msg,
					WPARAM wp, LPARAM lp);
static HWND create_owner (void);

static void setup_config (void);
static BOOL WINAPI enum_locale_callback (/*const*/ char* loc_string);
static UINT cp_from_locale (LCID lcid, UINT format);
static Lisp_Object coding_from_cp (UINT codepage);
static Lisp_Object validate_coding_system (Lisp_Object coding_system);
static void setup_windows_coding_system (Lisp_Object coding_system,
					 struct coding_system * coding);


/* A remnant from X11: Symbol for the CLIPBORD selection type.  Other
   selections are not used on Windows, so we don't need symbols for
   PRIMARY and SECONDARY.  */
Lisp_Object QCLIPBOARD;

/* Internal pseudo-constants, initialized in globals_of_w32select()
   based on current system parameters. */
static LCID DEFAULT_LCID;
static UINT ANSICP, OEMCP;
static Lisp_Object QUNICODE, QANSICP, QOEMCP;

/* A hidden window just for the clipboard management. */
static HWND clipboard_owner;
/* A flag to tell WM_DESTROYCLIPBOARD who is to blame this time (just
   checking GetClipboardOwner() doesn't work, sadly). */
static int modifying_clipboard = 0;

/* Configured transfer parameters, based on the last inspection of
   selection-coding-system.  */
static Lisp_Object cfg_coding_system;
static UINT cfg_codepage;
static LCID cfg_lcid;
static UINT cfg_clipboard_type;

/* The current state for delayed rendering. */
static Lisp_Object current_text;
static Lisp_Object current_coding_system;
static int current_requires_encoding, current_num_nls;
static UINT current_clipboard_type;
static LCID current_lcid;

#if TRACE
#define ONTRACE(stmt) stmt
#else
#define ONTRACE(stmt) /*stmt*/
#endif


/* This function assumes that there is no multibyte character in
   current_text, so we can short-cut encoding.  */

static HGLOBAL
convert_to_handle_as_ascii (void)
{
  HGLOBAL htext = NULL;
  int nbytes;
  int truelen;
  unsigned char *src;
  unsigned char *dst;

  ONTRACE (fprintf (stderr, "convert_to_handle_as_ascii\n"));

  nbytes = SBYTES (current_text) + 1;
  src = SDATA (current_text);

  /* We need to add to the size the number of LF chars where we have
     to insert CR chars (the standard CF_TEXT clipboard format uses
     CRLF line endings, while Emacs uses just LF internally).  */

  truelen = nbytes + current_num_nls;

  if ((htext = GlobalAlloc (GMEM_MOVEABLE | GMEM_DDESHARE, truelen)) == NULL)
    return NULL;

  if ((dst = (unsigned char *) GlobalLock (htext)) == NULL)
    {
      GlobalFree (htext);
      return NULL;
    }

  /* convert to CRLF line endings expected by clipboard */
  while (1)
    {
      unsigned char *next;
      /* copy next line or remaining bytes including '\0' */
      next = _memccpy (dst, src, '\n', nbytes);
      if (next)
	{
	  /* copied one line ending with '\n' */
	  int copied = next - dst;
	  nbytes -= copied;
	  src += copied;
	  /* insert '\r' before '\n' */
	  next[-1] = '\r';
	  next[0] = '\n';
	  dst = next + 1;
	}
      else
	/* copied remaining partial line -> now finished */
	break;
    }

  GlobalUnlock (htext);

  return htext;
}

/* This function assumes that there are multibyte or NUL characters in
   current_text, or that we need to construct Unicode.  It runs the
   text through the encoding machinery.  */

static HGLOBAL
convert_to_handle_as_coded (Lisp_Object coding_system)
{
  HGLOBAL htext;
  unsigned char *dst = NULL;
  struct coding_system coding;

  ONTRACE (fprintf (stderr, "convert_to_handle_as_coded: %s\n",
		    SDATA (SYMBOL_NAME (coding_system))));

  setup_windows_coding_system (coding_system, &coding);
  coding.dst_bytes = SBYTES (current_text) * 2;
  coding.destination = (unsigned char *) xmalloc (coding.dst_bytes);
  encode_coding_object (&coding, current_text, 0, 0,
			SCHARS (current_text), SBYTES (current_text), Qnil);

  htext = GlobalAlloc (GMEM_MOVEABLE | GMEM_DDESHARE, coding.produced +2);

  if (htext != NULL)
    dst = (unsigned char *) GlobalLock (htext);

  if (dst != NULL)
    {
      memcpy (dst, coding.destination, coding.produced);
      /* Add the string terminator.  Add two NULs in case we are
	 producing Unicode here.  */
      dst[coding.produced] = dst[coding.produced+1] = '\0';

      GlobalUnlock (htext);
    }

  xfree (coding.destination);

  return htext;
}

static Lisp_Object
render (Lisp_Object oformat)
{
  HGLOBAL htext = NULL;
  UINT format = XFASTINT (oformat);

  ONTRACE (fprintf (stderr, "render\n"));

  if (NILP (current_text))
    return Qnil;

  if (current_requires_encoding || format == CF_UNICODETEXT)
    {
      if (format == current_clipboard_type)
	htext = convert_to_handle_as_coded (current_coding_system);
      else
	switch (format)
	  {
	  case CF_UNICODETEXT:
	    htext = convert_to_handle_as_coded (QUNICODE);
	    break;
	  case CF_TEXT:
	  case CF_OEMTEXT:
	    {
	      Lisp_Object cs;
	      cs = coding_from_cp (cp_from_locale (current_lcid, format));
	      htext = convert_to_handle_as_coded (cs);
	      break;
	    }
	  }
    }
  else
    htext = convert_to_handle_as_ascii ();

  ONTRACE (fprintf (stderr, "render: htext = 0x%08X\n", (unsigned) htext));

  if (htext == NULL)
    return Qnil;

  if (SetClipboardData (format, htext) == NULL)
    {
      GlobalFree (htext);
      return Qnil;
    }

  return Qt;
}

static Lisp_Object
render_locale (void)
{
  HANDLE hlocale = NULL;
  LCID * lcid_ptr;

  ONTRACE (fprintf (stderr, "render_locale\n"));

  if (current_lcid == LOCALE_NEUTRAL || current_lcid == DEFAULT_LCID)
    return Qt;

  hlocale = GlobalAlloc (GMEM_MOVEABLE | GMEM_DDESHARE, sizeof (current_lcid));
  if (hlocale == NULL)
    return Qnil;

  if ((lcid_ptr = (LCID *) GlobalLock (hlocale)) == NULL)
    {
      GlobalFree (hlocale);
      return Qnil;
    }

  *lcid_ptr = current_lcid;
  GlobalUnlock (hlocale);

  if (SetClipboardData (CF_LOCALE, hlocale) == NULL)
    {
      GlobalFree (hlocale);
      return Qnil;
    }

  return Qt;
}

/* At the end of the program, we want to ensure that our clipboard
   data survives us.  This code will do that.  */

static Lisp_Object
render_all (Lisp_Object ignore)
{
  ONTRACE (fprintf (stderr, "render_all\n"));

  /* According to the docs we should not call OpenClipboard() here,
     but testing on W2K and working code in other projects shows that
     it is actually necessary.  */

  OpenClipboard (NULL);

  /* There is no useful means to report errors here, there are none
     expected anyway, and even if there were errors, they wouldn't do
     any harm.  So we just go ahead and do what has to be done without
     bothering with error handling.  */

  ++modifying_clipboard;
  EmptyClipboard ();
  --modifying_clipboard;

  /* For text formats that we don't render here, the OS can use its
     own translation rules instead, so we don't really need to offer
     everything.  To minimize memory consumption we cover three
     possible situations based on our primary format as detected from
     selection-coding-system (see setup_config()):

     - Post CF_TEXT only.  Let the OS convert to CF_OEMTEXT and the OS
       (on NT) or the application (on 9x/Me) convert to
       CF_UNICODETEXT.

     - Post CF_OEMTEXT only.  Similar automatic conversions happen as
       for CF_TEXT.

     - Post CF_UNICODETEXT + CF_TEXT.  9x itself ignores
       CF_UNICODETEXT, even though some applications can still handle
       it.

       Note 1: We render the less capable CF_TEXT *before* the more
       capable CF_UNICODETEXT, to prevent clobbering through automatic
       conversions, just in case.

       Note 2: We could check os_subtype here and only render the
       additional CF_TEXT on 9x/Me.  But OTOH with
       current_clipboard_type == CF_UNICODETEXT we don't involve the
       automatic conversions anywhere else, so to get consistent
       results, we probably don't want to rely on it here either.  */

  render_locale ();

  if (current_clipboard_type == CF_UNICODETEXT)
    render (make_number (CF_TEXT));
  render (make_number (current_clipboard_type));

  CloseClipboard ();

  return Qnil;
}

static void
run_protected (Lisp_Object (*code) (Lisp_Object), Lisp_Object arg)
{
  /* FIXME: This works but it doesn't feel right.  Too much fiddling
     with global variables and calling strange looking functions.  Is
     this really the right way to run Lisp callbacks?  */

  extern int waiting_for_input; /* from keyboard.c */
  int owfi;

  BLOCK_INPUT;

  /* Fsignal calls abort() if it sees that waiting_for_input is
     set.  */
  owfi = waiting_for_input;
  waiting_for_input = 0;

  internal_condition_case_1 (code, arg, Qt, lisp_error_handler);

  waiting_for_input = owfi;

  UNBLOCK_INPUT;
}

static Lisp_Object
lisp_error_handler (Lisp_Object error)
{
  Vsignaling_function = Qnil;
  cmd_error_internal (error, "Error in delayed clipboard rendering: ");
  Vinhibit_quit = Qt;
  return Qt;
}


static LRESULT CALLBACK
owner_callback (HWND win, UINT msg, WPARAM wp, LPARAM lp)
{
  switch (msg)
    {
    case WM_RENDERFORMAT:
      ONTRACE (fprintf (stderr, "WM_RENDERFORMAT\n"));
      run_protected (render, make_number (wp));
      return 0;

    case WM_RENDERALLFORMATS:
      ONTRACE (fprintf (stderr, "WM_RENDERALLFORMATS\n"));
      run_protected (render_all, Qnil);
      return 0;

    case WM_DESTROYCLIPBOARD:
      if (!modifying_clipboard)
	{
	  ONTRACE (fprintf (stderr, "WM_DESTROYCLIPBOARD (other)\n"));
	  current_text = Qnil;
	  current_coding_system = Qnil;
	}
      else
	{
	  ONTRACE (fprintf (stderr, "WM_DESTROYCLIPBOARD (self)\n"));
	}
      return 0;

    case WM_DESTROY:
      if (win == clipboard_owner)
	clipboard_owner = NULL;
      break;
    }

  return DefWindowProc (win, msg, wp, lp);
}

static HWND
create_owner (void)
{
  static const char CLASSNAME[] = "Emacs Clipboard";
  WNDCLASS wc;

  memset (&wc, 0, sizeof (wc));
  wc.lpszClassName = CLASSNAME;
  wc.lpfnWndProc = owner_callback;
  RegisterClass (&wc);

  return CreateWindow (CLASSNAME, CLASSNAME, 0, 0, 0, 0, 0, NULL, NULL,
		       NULL, NULL);
}

/* Called on exit by term_ntproc() in w32.c */

void
term_w32select (void)
{
  /* This is needed to trigger WM_RENDERALLFORMATS. */
  if (clipboard_owner != NULL)
    DestroyWindow (clipboard_owner);
}

static void
setup_config (void)
{
  const char *coding_name;
  const char *cp;
  char *end;
  int slen;
  Lisp_Object coding_system;
  Lisp_Object dos_coding_system;

  CHECK_SYMBOL (Vselection_coding_system);

  coding_system = NILP (Vnext_selection_coding_system) ?
    Vselection_coding_system : Vnext_selection_coding_system;

  dos_coding_system = validate_coding_system (coding_system);
  if (NILP (dos_coding_system))
    Fsignal (Qerror,
	     list2 (build_string ("Coding system is invalid or doesn't have "
				  "an eol variant for dos line ends"),
		    coding_system));

  /* Check if we have it cached */
  if (!NILP (cfg_coding_system)
      && EQ (cfg_coding_system, dos_coding_system))
    return;
  cfg_coding_system = dos_coding_system;

  /* Set some sensible fallbacks */
  cfg_codepage = ANSICP;
  cfg_lcid = LOCALE_NEUTRAL;
  cfg_clipboard_type = CF_TEXT;

  /* Interpret the coding system symbol name */
  coding_name = SDATA (SYMBOL_NAME (cfg_coding_system));

  /* "(.*-)?utf-16.*" -> CF_UNICODETEXT */
  cp = strstr (coding_name, "utf-16");
  if (cp != NULL && (cp == coding_name || cp[-1] == '-'))
    {
      cfg_clipboard_type = CF_UNICODETEXT;
      return;
    }

  /* "cp[0-9]+.*" or "windows-[0-9]+.*" -> CF_TEXT or CF_OEMTEXT */
  slen = strlen (coding_name);
  if (slen >= 4 && coding_name[0] == 'c' && coding_name[1] == 'p')
    cp = coding_name + 2;
  else if (slen >= 10 && memcmp (coding_name, "windows-", 8) == 0)
    cp = coding_name + 8;
  else
    return;

  end = (char*)cp;
  cfg_codepage = strtol (cp, &end, 10);

  /* Error return from strtol() or number of digits < 2 -> Restore the
     default and drop it. */
  if (cfg_codepage == 0 || (end-cp) < 2 )
    {
      cfg_codepage = ANSICP;
      return;
    }

  /* Is it the currently active system default? */
  if (cfg_codepage == ANSICP)
    {
      /* cfg_clipboard_type = CF_TEXT; */
      return;
    }
  if (cfg_codepage == OEMCP)
    {
      cfg_clipboard_type = CF_OEMTEXT;
      return;
    }

  /* Else determine a suitable locale the hard way. */
  EnumSystemLocales (enum_locale_callback, LCID_INSTALLED);
}

static BOOL WINAPI
enum_locale_callback (/*const*/ char* loc_string)
{
  LCID lcid;
  UINT codepage;

  lcid = strtoul (loc_string, NULL, 16);

  /* Is the wanted codepage the "ANSI" codepage for this locale? */
  codepage = cp_from_locale (lcid, CF_TEXT);
  if (codepage == cfg_codepage)
    {
      cfg_lcid = lcid;
      cfg_clipboard_type = CF_TEXT;
      return FALSE; /* Stop enumeration */
    }

  /* Is the wanted codepage the OEM codepage for this locale? */
  codepage = cp_from_locale (lcid, CF_OEMTEXT);
  if (codepage == cfg_codepage)
    {
      cfg_lcid = lcid;
      cfg_clipboard_type = CF_OEMTEXT;
      return FALSE; /* Stop enumeration */
    }

  return TRUE; /* Continue enumeration */
}

static UINT
cp_from_locale (LCID lcid, UINT format)
{
  char buffer[20] = "";
  UINT variant, cp;

  variant =
    format == CF_TEXT ? LOCALE_IDEFAULTANSICODEPAGE : LOCALE_IDEFAULTCODEPAGE;

  GetLocaleInfo (lcid, variant, buffer, sizeof (buffer));
  cp = strtoul (buffer, NULL, 10);

  if (cp == CP_ACP)
    return ANSICP;
  else if (cp == CP_OEMCP)
    return OEMCP;
  else
    return cp;
}

static Lisp_Object
coding_from_cp (UINT codepage)
{
  char buffer[30];
  sprintf (buffer, "cp%d-dos", (int) codepage);
  return intern (buffer);
  /* We don't need to check that this coding system actually exists
     right here, because that is done later for all coding systems
     used, regardless of where they originate.  */
}

static Lisp_Object
validate_coding_system (Lisp_Object coding_system)
{
  Lisp_Object eol_type;

  /* Make sure the input is valid. */
  if (NILP (Fcoding_system_p (coding_system)))
    return Qnil;

  /* Make sure we use a DOS coding system as mandated by the system
     specs. */
  eol_type = Fcoding_system_eol_type (coding_system);

  /* Already a DOS coding system? */
  if (EQ (eol_type, make_number (1)))
    return coding_system;

  /* Get EOL_TYPE vector of the base of CODING_SYSTEM.  */
  if (!VECTORP (eol_type))
    {
      eol_type = Fcoding_system_eol_type (Fcoding_system_base (coding_system));
      if (!VECTORP (eol_type))
	return Qnil;
    }

  return AREF (eol_type, 1);
}

static void
setup_windows_coding_system (Lisp_Object coding_system,
			     struct coding_system * coding)
{
  memset (coding, 0, sizeof (*coding));
  setup_coding_system (coding_system, coding);

  /* Unset CODING_ANNOTATE_COMPOSITION_MASK.  Previous code had
     comments about crashes in encode_coding_iso2022 trying to
     dereference a null pointer when composition was on.  Selection
     data should not contain any composition sequence on Windows.

     CODING_ANNOTATION_MASK also includes
     CODING_ANNOTATE_DIRECTION_MASK and CODING_ANNOTATE_CHARSET_MASK,
     which both apply to ISO6429 only.  We don't know if these really
     need to be unset on Windows, but it probably doesn't hurt
     either.  */
  coding->mode &= ~CODING_ANNOTATION_MASK;
  coding->mode |= CODING_MODE_LAST_BLOCK | CODING_MODE_SAFE_ENCODING;
}



DEFUN ("w32-set-clipboard-data", Fw32_set_clipboard_data,
       Sw32_set_clipboard_data, 1, 2, 0,
       doc: /* This sets the clipboard data to the given text.  */)
  (Lisp_Object string, Lisp_Object ignored)
{
  BOOL ok = TRUE;
  int nbytes;
  unsigned char *src;
  unsigned char *dst;
  unsigned char *end;

  /* This parameter used to be the current frame, but we don't use
     that any more. */
  (void) ignored;

  CHECK_STRING (string);

  setup_config ();

  current_text = string;
  current_coding_system = cfg_coding_system;
  current_clipboard_type = cfg_clipboard_type;
  current_lcid = cfg_lcid;
  current_num_nls = 0;
  current_requires_encoding = 0;

  BLOCK_INPUT;

  /* Check for non-ASCII characters.  While we are at it, count the
     number of LFs, so we know how many CRs we will have to add later
     (just in the case where we can use our internal ASCII rendering,
     see code and comment in convert_to_handle_as_ascii() above).  */
  nbytes = SBYTES (string);
  src = SDATA (string);

  for (dst = src, end = src+nbytes; dst < end; dst++)
    {
      if (*dst == '\n')
	current_num_nls++;
      else if (*dst >= 0x80 || *dst == 0)
	{
	  current_requires_encoding = 1;
	  break;
	}
    }

  if (!current_requires_encoding)
    {
      /* If all we have is ASCII we don't need to pretend we offer
	 anything fancy. */
      current_coding_system = Qraw_text;
      current_clipboard_type = CF_TEXT;
      current_lcid = LOCALE_NEUTRAL;
    }

  if (!OpenClipboard (clipboard_owner))
    goto error;

  ++modifying_clipboard;
  ok = EmptyClipboard ();
  --modifying_clipboard;

  /* If we have something non-ASCII we may want to set a locale.  We
     do that directly (non-delayed), as it's just a small bit.  */
  if (ok)
    ok = !NILP (render_locale ());

  if (ok)
    {
      if (clipboard_owner == NULL)
	{
	  /* If for some reason we don't have a clipboard_owner, we
	     just set the text format as chosen by the configuration
	     and than forget about the whole thing.  */
	  ok = !NILP (render (make_number (current_clipboard_type)));
	  current_text = Qnil;
	  current_coding_system = Qnil;
	}
      else
	{
	  /* Advertise all supported formats so that whatever the
	     requestor chooses, only one encoding step needs to be
	     made.  This is intentionally different from what we do in
	     the handler for WM_RENDERALLFORMATS.  */
	  SetClipboardData (CF_UNICODETEXT, NULL);
	  SetClipboardData (CF_TEXT, NULL);
	  SetClipboardData (CF_OEMTEXT, NULL);
	}
    }

  CloseClipboard ();

  /* With delayed rendering we haven't really "used" this coding
     system yet, and it's even unclear if we ever will.  But this is a
     way to tell the upper level what we *would* use under ideal
     circumstances.

     We don't signal the actually used coding-system later when we
     finally render, because that can happen at any time and we don't
     want to disturb the "foreground" action. */
  if (ok)
    Vlast_coding_system_used = current_coding_system;

  Vnext_selection_coding_system = Qnil;

  if (ok) goto done;

 error:

  ok = FALSE;
  current_text = Qnil;
  current_coding_system = Qnil;

 done:
  UNBLOCK_INPUT;

  return (ok ? string : Qnil);
}


DEFUN ("w32-get-clipboard-data", Fw32_get_clipboard_data,
       Sw32_get_clipboard_data, 0, 1, 0,
       doc: /* This gets the clipboard data in text format.  */)
  (Lisp_Object ignored)
{
  HGLOBAL htext;
  Lisp_Object ret = Qnil;
  UINT actual_clipboard_type;
  int use_configured_coding_system = 1;

  /* This parameter used to be the current frame, but we don't use
     that any more. */
  (void) ignored;

  /* Don't pass our own text from the clipboard (which might be
     troublesome if the killed text includes null characters).  */
  if (!NILP (current_text))
    return ret;

  setup_config ();
  actual_clipboard_type = cfg_clipboard_type;

  BLOCK_INPUT;

  if (!OpenClipboard (clipboard_owner))
    goto done;

  if ((htext = GetClipboardData (actual_clipboard_type)) == NULL)
    {
      /* If we want CF_UNICODETEXT but can't get it, the current
	 coding system is useless.  OTOH we can still try and decode
	 CF_TEXT based on the locale that the system gives us and that
	 we get down below.  */
      if (actual_clipboard_type == CF_UNICODETEXT)
	{
	  htext = GetClipboardData (CF_TEXT);
	  if (htext != NULL)
	    {
	      actual_clipboard_type = CF_TEXT;
	      use_configured_coding_system = 0;
	    }
	}
    }
  if (htext == NULL)
    goto closeclip;

  {
    unsigned char *src;
    unsigned char *dst;
    int nbytes;
    int truelen;
    int require_decoding = 0;

    if ((src = (unsigned char *) GlobalLock (htext)) == NULL)
      goto closeclip;

    /* If the clipboard data contains any non-ascii code, we need to
       decode it with a coding system.  */
    if (actual_clipboard_type == CF_UNICODETEXT)
      {
	nbytes = lstrlenW ((WCHAR *)src) * 2;
	require_decoding = 1;
      }
    else
      {
	int i;

	nbytes = strlen (src);

	for (i = 0; i < nbytes; i++)
	  {
	    if (src[i] >= 0x80)
	      {
		require_decoding = 1;
		break;
	      }
	  }
      }

    if (require_decoding)
      {
	struct coding_system coding;
	Lisp_Object coding_system = Qnil;
	Lisp_Object dos_coding_system;

	/* `next-selection-coding-system' should override everything,
	   even when the locale passed by the system disagrees.  The
	   only exception is when `next-selection-coding-system'
	   requested CF_UNICODETEXT and we couldn't get that. */
	if (use_configured_coding_system
	    && !NILP (Vnext_selection_coding_system))
	    coding_system = Vnext_selection_coding_system;

	/* If we have CF_TEXT or CF_OEMTEXT, we want to check out
	   CF_LOCALE, too. */
	else if (actual_clipboard_type != CF_UNICODETEXT)
	  {
	    HGLOBAL hlocale;
	    LCID lcid = DEFAULT_LCID;
	    UINT cp;

	    /* Documentation says that the OS always generates
	       CF_LOCALE info automatically, so the locale handle
	       should always be present.  Fact is that this is not
	       always true on 9x ;-(.  */
	    hlocale = GetClipboardData (CF_LOCALE);
	    if (hlocale != NULL)
	      {
		const LCID * lcid_ptr;
		lcid_ptr = (const LCID *) GlobalLock (hlocale);
		if (lcid_ptr != NULL)
		  {
		    lcid = *lcid_ptr;
		    GlobalUnlock (hlocale);
		  }

		/* 9x has garbage as the sort order (to be exact there
		   is another instance of the language id in the upper
		   word).  We don't care about sort order anyway, so
		   we just filter out the unneeded mis-information to
		   avoid irritations. */
		lcid = MAKELCID (LANGIDFROMLCID (lcid), SORT_DEFAULT);
	      }

	    /* If we are using fallback from CF_UNICODETEXT, we can't
	       use the configured coding system.  Also we don't want
	       to use it, if the system has supplied us with a locale
	       and it is not just the system default. */
	    if (!use_configured_coding_system || lcid != DEFAULT_LCID)
	      {
		cp = cp_from_locale (lcid, actual_clipboard_type);
		/* If it's just our current standard setting anyway,
		   use the coding system that the user has selected.
		   Otherwise create a new spec to match the locale
		   that was specified by the other side or the
		   system.  */
		if (!use_configured_coding_system || cp != cfg_codepage)
		  coding_system = coding_from_cp (cp);
	      }
	  }

	if (NILP (coding_system))
	  coding_system = Vselection_coding_system;
	Vnext_selection_coding_system = Qnil;

	dos_coding_system = validate_coding_system (coding_system);
	if (!NILP (dos_coding_system))
	  {
	    setup_windows_coding_system (dos_coding_system, &coding);
	    coding.source = src;
	    decode_coding_object (&coding, Qnil, 0, 0, nbytes, nbytes, Qt);
	    ret = coding.dst_object;

	    Vlast_coding_system_used = CODING_ID_NAME (coding.id);
	  }
      }
    else
      {
	/* FIXME: We may want to repeat the code in this branch for
	   the Unicode case. */

	/* Need to know final size after CR chars are removed because
	   we can't change the string size manually, and doing an
	   extra copy is silly.  We only remove CR when it appears as
	   part of CRLF.  */

	truelen = nbytes;
	dst = src;
	/* avoid using strchr because it recomputes the length everytime */
	while ((dst = memchr (dst, '\r', nbytes - (dst - src))) != NULL)
	  {
	    if (dst[1] == '\n')	/* safe because of trailing '\0' */
	      truelen--;
	    dst++;
	  }

	ret = make_uninit_string (truelen);

	/* Convert CRLF line endings (the standard CF_TEXT clipboard
	   format) to LF endings as used internally by Emacs.  */

	dst = SDATA (ret);
	while (1)
	  {
	    unsigned char *next;
	    /* copy next line or remaining bytes excluding '\0' */
	    next = _memccpy (dst, src, '\r', nbytes);
	    if (next)
	      {
		/* copied one line ending with '\r' */
		int copied = next - dst;
		nbytes -= copied;
		dst += copied;
		src += copied;
		if (*src == '\n')
		  dst--;	/* overwrite '\r' with '\n' */
	      }
	    else
	      /* copied remaining partial line -> now finished */
	      break;
	  }

	Vlast_coding_system_used = Qraw_text;
      }

    GlobalUnlock (htext);
  }

 closeclip:
  CloseClipboard ();

 done:
  UNBLOCK_INPUT;

  return (ret);
}

/* Support checking for a clipboard selection. */

DEFUN ("x-selection-exists-p", Fx_selection_exists_p, Sx_selection_exists_p,
       0, 2, 0,
       doc: /* Whether there is an owner for the given X selection.
SELECTION should be the name of the selection in question, typically
one of the symbols `PRIMARY', `SECONDARY', or `CLIPBOARD'.  (X expects
these literal upper-case names.)  The symbol nil is the same as
`PRIMARY', and t is the same as `SECONDARY'.

TERMINAL should be a terminal object or a frame specifying the X
server to query.  If omitted or nil, that stands for the selected
frame's display, or the first available X display.  */)
  (Lisp_Object selection, Lisp_Object terminal)
{
  CHECK_SYMBOL (selection);

  /* Return nil for PRIMARY and SECONDARY selections; for CLIPBOARD, check
     if the clipboard currently has valid text format contents. */

  if (EQ (selection, QCLIPBOARD))
    {
      Lisp_Object val = Qnil;

      setup_config ();

      if (OpenClipboard (NULL))
	{
	  UINT format = 0;
	  while ((format = EnumClipboardFormats (format)))
	    /* Check CF_TEXT in addition to cfg_clipboard_type,
	       because we can fall back on that if CF_UNICODETEXT is
	       not available.  Actually a check for CF_TEXT only
	       should be enough.  */
	    if (format == cfg_clipboard_type || format == CF_TEXT)
	      {
		val = Qt;
		break;
	      }
	  CloseClipboard ();
	}
      return val;
    }
  return Qnil;
}

/* One-time init.  Called in the un-dumped Emacs, but not in the
   dumped version. */

void
syms_of_w32select (void)
{
  defsubr (&Sw32_set_clipboard_data);
  defsubr (&Sw32_get_clipboard_data);
  defsubr (&Sx_selection_exists_p);

  DEFVAR_LISP ("selection-coding-system", Vselection_coding_system,
	       doc: /* Coding system for communicating with other programs.

For MS-Windows and MS-DOS:
When sending or receiving text via selection and clipboard, the text
is encoded or decoded by this coding system.  The default value is
the current system default encoding on 9x/Me, `utf-16le-dos'
\(Unicode) on NT/W2K/XP, and `iso-latin-1-dos' on MS-DOS.

For X Windows:
When sending text via selection and clipboard, if the target
data-type matches with the type of this coding system, it is used
for encoding the text.  Otherwise (including the case that this
variable is nil), a proper coding system is used as below:

data-type	coding system
---------	-------------
UTF8_STRING	utf-8
COMPOUND_TEXT	compound-text-with-extensions
STRING		iso-latin-1
C_STRING	no-conversion

When receiving text, if this coding system is non-nil, it is used
for decoding regardless of the data-type.  If this is nil, a
proper coding system is used according to the data-type as above.

See also the documentation of the variable `x-select-request-type' how
to control which data-type to request for receiving text.

The default value is nil.  */);
  /* The actual value is set dynamically in the dumped Emacs, see
     below. */
  Vselection_coding_system = Qnil;

  DEFVAR_LISP ("next-selection-coding-system", Vnext_selection_coding_system,
	       doc: /* Coding system for the next communication with other programs.
Usually, `selection-coding-system' is used for communicating with
other programs (X Windows clients or MS Windows programs).  But, if this
variable is set, it is used for the next communication only.
After the communication, this variable is set to nil.  */);
  Vnext_selection_coding_system = Qnil;

  DEFSYM (QCLIPBOARD, "CLIPBOARD");

  cfg_coding_system = Qnil;     staticpro (&cfg_coding_system);
  current_text = Qnil;		staticpro (&current_text);
  current_coding_system = Qnil; staticpro (&current_coding_system);

  DEFSYM (QUNICODE, "utf-16le-dos");
  QANSICP = Qnil; staticpro (&QANSICP);
  QOEMCP = Qnil;  staticpro (&QOEMCP);
}

/* One-time init.  Called in the dumped Emacs, but not in the
   un-dumped version. */

void
globals_of_w32select (void)
{
  DEFAULT_LCID = GetUserDefaultLCID ();
  /* Drop the sort order from the LCID, so we can compare this with
     CF_LOCALE objects that have the same fix on 9x.  */
  DEFAULT_LCID = MAKELCID (LANGIDFROMLCID (DEFAULT_LCID), SORT_DEFAULT);

  ANSICP = GetACP ();
  OEMCP = GetOEMCP ();

  QANSICP = coding_from_cp (ANSICP);
  QOEMCP = coding_from_cp (OEMCP);

  if (os_subtype == OS_NT)
    Vselection_coding_system = QUNICODE;
  else if (inhibit_window_system)
    Vselection_coding_system = QOEMCP;
  else
    Vselection_coding_system = QANSICP;

  clipboard_owner = create_owner ();
}
