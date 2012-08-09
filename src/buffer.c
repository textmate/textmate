/* Buffer manipulation primitives for GNU Emacs.

Copyright (C) 1985-1989, 1993-1995, 1997-2012 Free Software Foundation, Inc.

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

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/param.h>
#include <errno.h>
#include <stdio.h>
#include <setjmp.h>
#include <unistd.h>

#include <verify.h>

#include "lisp.h"
#include "intervals.h"
#include "window.h"
#include "commands.h"
#include "buffer.h"
#include "character.h"
#include "region-cache.h"
#include "indent.h"
#include "blockinput.h"
#include "keyboard.h"
#include "keymap.h"
#include "frame.h"

struct buffer *current_buffer;		/* the current buffer */

/* First buffer in chain of all buffers (in reverse order of creation).
   Threaded through ->header.next.buffer.  */

struct buffer *all_buffers;

/* This structure holds the default values of the buffer-local variables
   defined with DEFVAR_PER_BUFFER, that have special slots in each buffer.
   The default value occupies the same slot in this structure
   as an individual buffer's value occupies in that buffer.
   Setting the default value also goes through the alist of buffers
   and stores into each buffer that does not say it has a local value.  */

DECL_ALIGN (struct buffer, buffer_defaults);

/* A Lisp_Object pointer to the above, used for staticpro */

static Lisp_Object Vbuffer_defaults;

/* This structure marks which slots in a buffer have corresponding
   default values in buffer_defaults.
   Each such slot has a nonzero value in this structure.
   The value has only one nonzero bit.

   When a buffer has its own local value for a slot,
   the entry for that slot (found in the same slot in this structure)
   is turned on in the buffer's local_flags array.

   If a slot in this structure is -1, then even though there may
   be a DEFVAR_PER_BUFFER for the slot, there is no default value for it;
   and the corresponding slot in buffer_defaults is not used.

   If a slot in this structure corresponding to a DEFVAR_PER_BUFFER is
   zero, that is a bug */

struct buffer buffer_local_flags;

/* This structure holds the names of symbols whose values may be
   buffer-local.  It is indexed and accessed in the same way as the above. */

DECL_ALIGN (struct buffer, buffer_local_symbols);

/* A Lisp_Object pointer to the above, used for staticpro */
static Lisp_Object Vbuffer_local_symbols;

/* Return the symbol of the per-buffer variable at offset OFFSET in
   the buffer structure.  */

#define PER_BUFFER_SYMBOL(OFFSET) \
      (*(Lisp_Object *)((OFFSET) + (char *) &buffer_local_symbols))

/* Maximum length of an overlay vector.  */
#define OVERLAY_COUNT_MAX						\
  ((ptrdiff_t) min (MOST_POSITIVE_FIXNUM,				\
		    min (PTRDIFF_MAX, SIZE_MAX) / sizeof (Lisp_Object)))

/* Flags indicating which built-in buffer-local variables
   are permanent locals.  */
static char buffer_permanent_local_flags[MAX_PER_BUFFER_VARS];

/* Number of per-buffer variables used.  */

int last_per_buffer_idx;

static Lisp_Object Fset_buffer_major_mode (Lisp_Object);
static Lisp_Object Fdelete_overlay (Lisp_Object);
static void call_overlay_mod_hooks (Lisp_Object list, Lisp_Object overlay,
                                    int after, Lisp_Object arg1,
                                    Lisp_Object arg2, Lisp_Object arg3);
static void swap_out_buffer_local_variables (struct buffer *b);
static void reset_buffer_local_variables (struct buffer *b, int permanent_too);

/* Alist of all buffer names vs the buffers. */
/* This used to be a variable, but is no longer,
 to prevent lossage due to user rplac'ing this alist or its elements.  */
Lisp_Object Vbuffer_alist;

static Lisp_Object Qkill_buffer_query_functions;

/* Hook run before changing a major mode.  */
static Lisp_Object Qchange_major_mode_hook;

Lisp_Object Qfirst_change_hook;
Lisp_Object Qbefore_change_functions;
Lisp_Object Qafter_change_functions;
static Lisp_Object Qucs_set_table_for_input;

static Lisp_Object Qfundamental_mode, Qmode_class, Qpermanent_local;
static Lisp_Object Qpermanent_local_hook;

static Lisp_Object Qprotected_field;

static Lisp_Object QSFundamental;	/* A string "Fundamental" */

static Lisp_Object Qkill_buffer_hook;
static Lisp_Object Qbuffer_list_update_hook;

static Lisp_Object Qget_file_buffer;

static Lisp_Object Qoverlayp;

Lisp_Object Qpriority, Qbefore_string, Qafter_string;

static Lisp_Object Qevaporate;

Lisp_Object Qmodification_hooks;
Lisp_Object Qinsert_in_front_hooks;
Lisp_Object Qinsert_behind_hooks;

static void alloc_buffer_text (struct buffer *, ptrdiff_t);
static void free_buffer_text (struct buffer *b);
static struct Lisp_Overlay * copy_overlays (struct buffer *, struct Lisp_Overlay *);
static void modify_overlay (struct buffer *, EMACS_INT, EMACS_INT);
static Lisp_Object buffer_lisp_local_variables (struct buffer *);

/* For debugging; temporary.  See set_buffer_internal.  */
/* Lisp_Object Qlisp_mode, Vcheck_symbol; */

void
nsberror (Lisp_Object spec)
{
  if (STRINGP (spec))
    error ("No buffer named %s", SDATA (spec));
  error ("Invalid buffer argument");
}

DEFUN ("buffer-live-p", Fbuffer_live_p, Sbuffer_live_p, 1, 1, 0,
       doc: /* Return non-nil if OBJECT is a buffer which has not been killed.
Value is nil if OBJECT is not a buffer or if it has been killed.  */)
  (Lisp_Object object)
{
  return ((BUFFERP (object) && ! NILP (BVAR (XBUFFER (object), name)))
	  ? Qt : Qnil);
}

DEFUN ("buffer-list", Fbuffer_list, Sbuffer_list, 0, 1, 0,
       doc: /* Return a list of all existing live buffers.
If the optional arg FRAME is a frame, we return the buffer list in the
proper order for that frame: the buffers show in FRAME come first,
followed by the rest of the buffers.  */)
  (Lisp_Object frame)
{
  Lisp_Object general;
  general = Fmapcar (Qcdr, Vbuffer_alist);

  if (FRAMEP (frame))
    {
      Lisp_Object framelist, prevlist, tail;
      Lisp_Object args[3];

      CHECK_FRAME (frame);
      framelist = Fcopy_sequence (XFRAME (frame)->buffer_list);
      prevlist = Fnreverse (Fcopy_sequence
			    (XFRAME (frame)->buried_buffer_list));

      /* Remove from GENERAL any buffer that duplicates one in
         FRAMELIST or PREVLIST.  */
      tail = framelist;
      while (CONSP (tail))
	{
	  general = Fdelq (XCAR (tail), general);
	  tail = XCDR (tail);
	}
      tail = prevlist;
      while (CONSP (tail))
	{
	  general = Fdelq (XCAR (tail), general);
	  tail = XCDR (tail);
	}

      args[0] = framelist;
      args[1] = general;
      args[2] = prevlist;
      return Fnconc (3, args);
    }
  else
    return general;
}

/* Like Fassoc, but use Fstring_equal to compare
   (which ignores text properties),
   and don't ever QUIT.  */

static Lisp_Object
assoc_ignore_text_properties (register Lisp_Object key, Lisp_Object list)
{
  register Lisp_Object tail;
  for (tail = list; CONSP (tail); tail = XCDR (tail))
    {
      register Lisp_Object elt, tem;
      elt = XCAR (tail);
      tem = Fstring_equal (Fcar (elt), key);
      if (!NILP (tem))
	return elt;
    }
  return Qnil;
}

DEFUN ("get-buffer", Fget_buffer, Sget_buffer, 1, 1, 0,
       doc: /* Return the buffer named BUFFER-OR-NAME.
BUFFER-OR-NAME must be either a string or a buffer.  If BUFFER-OR-NAME
is a string and there is no buffer with that name, return nil.  If
BUFFER-OR-NAME is a buffer, return it as given.  */)
  (register Lisp_Object buffer_or_name)
{
  if (BUFFERP (buffer_or_name))
    return buffer_or_name;
  CHECK_STRING (buffer_or_name);

  return Fcdr (assoc_ignore_text_properties (buffer_or_name, Vbuffer_alist));
}

DEFUN ("get-file-buffer", Fget_file_buffer, Sget_file_buffer, 1, 1, 0,
       doc: /* Return the buffer visiting file FILENAME (a string).
The buffer's `buffer-file-name' must match exactly the expansion of FILENAME.
If there is no such live buffer, return nil.
See also `find-buffer-visiting'.  */)
  (register Lisp_Object filename)
{
  register Lisp_Object tail, buf, tem;
  Lisp_Object handler;

  CHECK_STRING (filename);
  filename = Fexpand_file_name (filename, Qnil);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (filename, Qget_file_buffer);
  if (!NILP (handler))
    {
      Lisp_Object handled_buf = call2 (handler, Qget_file_buffer,
				       filename);
      return BUFFERP (handled_buf) ? handled_buf : Qnil;
    }

  for (tail = Vbuffer_alist; CONSP (tail); tail = XCDR (tail))
    {
      buf = Fcdr (XCAR (tail));
      if (!BUFFERP (buf)) continue;
      if (!STRINGP (BVAR (XBUFFER (buf), filename))) continue;
      tem = Fstring_equal (BVAR (XBUFFER (buf), filename), filename);
      if (!NILP (tem))
	return buf;
    }
  return Qnil;
}

Lisp_Object
get_truename_buffer (register Lisp_Object filename)
{
  register Lisp_Object tail, buf, tem;

  for (tail = Vbuffer_alist; CONSP (tail); tail = XCDR (tail))
    {
      buf = Fcdr (XCAR (tail));
      if (!BUFFERP (buf)) continue;
      if (!STRINGP (BVAR (XBUFFER (buf), file_truename))) continue;
      tem = Fstring_equal (BVAR (XBUFFER (buf), file_truename), filename);
      if (!NILP (tem))
	return buf;
    }
  return Qnil;
}

DEFUN ("get-buffer-create", Fget_buffer_create, Sget_buffer_create, 1, 1, 0,
       doc: /* Return the buffer specified by BUFFER-OR-NAME, creating a new one if needed.
If BUFFER-OR-NAME is a string and a live buffer with that name exists,
return that buffer.  If no such buffer exists, create a new buffer with
that name and return it.  If BUFFER-OR-NAME starts with a space, the new
buffer does not keep undo information.

If BUFFER-OR-NAME is a buffer instead of a string, return it as given,
even if it is dead.  The return value is never nil.  */)
  (register Lisp_Object buffer_or_name)
{
  register Lisp_Object buffer, name;
  register struct buffer *b;

  buffer = Fget_buffer (buffer_or_name);
  if (!NILP (buffer))
    return buffer;

  if (SCHARS (buffer_or_name) == 0)
    error ("Empty string for buffer name is not allowed");

  b = allocate_buffer ();

  /* An ordinary buffer uses its own struct buffer_text.  */
  b->text = &b->own_text;
  b->base_buffer = 0;

  BUF_GAP_SIZE (b) = 20;
  BLOCK_INPUT;
  /* We allocate extra 1-byte at the tail and keep it always '\0' for
     anchoring a search.  */
  alloc_buffer_text (b, BUF_GAP_SIZE (b) + 1);
  UNBLOCK_INPUT;
  if (! BUF_BEG_ADDR (b))
    buffer_memory_full (BUF_GAP_SIZE (b) + 1);

  b->pt = BEG;
  b->begv = BEG;
  b->zv = BEG;
  b->pt_byte = BEG_BYTE;
  b->begv_byte = BEG_BYTE;
  b->zv_byte = BEG_BYTE;

  BUF_GPT (b) = BEG;
  BUF_GPT_BYTE (b) = BEG_BYTE;

  BUF_Z (b) = BEG;
  BUF_Z_BYTE (b) = BEG_BYTE;
  BUF_MODIFF (b) = 1;
  BUF_CHARS_MODIFF (b) = 1;
  BUF_OVERLAY_MODIFF (b) = 1;
  BUF_SAVE_MODIFF (b) = 1;
  BUF_INTERVALS (b) = 0;
  BUF_UNCHANGED_MODIFIED (b) = 1;
  BUF_OVERLAY_UNCHANGED_MODIFIED (b) = 1;
  BUF_END_UNCHANGED (b) = 0;
  BUF_BEG_UNCHANGED (b) = 0;
  *(BUF_GPT_ADDR (b)) = *(BUF_Z_ADDR (b)) = 0; /* Put an anchor '\0'.  */
  b->text->inhibit_shrinking = 0;

  b->newline_cache = 0;
  b->width_run_cache = 0;
  BVAR (b, width_table) = Qnil;
  b->prevent_redisplay_optimizations_p = 1;

  /* Put this on the chain of all buffers including killed ones.  */
  b->header.next.buffer = all_buffers;
  all_buffers = b;

  /* An ordinary buffer normally doesn't need markers
     to handle BEGV and ZV.  */
  BVAR (b, pt_marker) = Qnil;
  BVAR (b, begv_marker) = Qnil;
  BVAR (b, zv_marker) = Qnil;

  name = Fcopy_sequence (buffer_or_name);
  STRING_SET_INTERVALS (name, NULL_INTERVAL);
  BVAR (b, name) = name;

  BVAR (b, undo_list) = (SREF (name, 0) != ' ') ? Qnil : Qt;

  reset_buffer (b);
  reset_buffer_local_variables (b, 1);

  BVAR (b, mark) = Fmake_marker ();
  BUF_MARKERS (b) = NULL;
  BVAR (b, name) = name;

  /* Put this in the alist of all live buffers.  */
  XSETBUFFER (buffer, b);
  Vbuffer_alist = nconc2 (Vbuffer_alist, Fcons (Fcons (name, buffer), Qnil));
  /* And run buffer-list-update-hook.  */
  if (!NILP (Vrun_hooks))
    call1 (Vrun_hooks, Qbuffer_list_update_hook);

  /* An error in calling the function here (should someone redefine it)
     can lead to infinite regress until you run out of stack.  rms
     says that's not worth protecting against.  */
  if (!NILP (Ffboundp (Qucs_set_table_for_input)))
    /* buffer is on buffer-alist, so no gcpro.  */
    call1 (Qucs_set_table_for_input, buffer);

  return buffer;
}


/* Return a list of overlays which is a copy of the overlay list
   LIST, but for buffer B.  */

static struct Lisp_Overlay *
copy_overlays (struct buffer *b, struct Lisp_Overlay *list)
{
  Lisp_Object buffer;
  struct Lisp_Overlay *result = NULL, *tail = NULL;

  XSETBUFFER (buffer, b);

  for (; list; list = list->next)
    {
      Lisp_Object overlay, start, end, old_overlay;
      EMACS_INT charpos;

      XSETMISC (old_overlay, list);
      charpos = marker_position (OVERLAY_START (old_overlay));
      start = Fmake_marker ();
      Fset_marker (start, make_number (charpos), buffer);
      XMARKER (start)->insertion_type
	= XMARKER (OVERLAY_START (old_overlay))->insertion_type;

      charpos = marker_position (OVERLAY_END (old_overlay));
      end = Fmake_marker ();
      Fset_marker (end, make_number (charpos), buffer);
      XMARKER (end)->insertion_type
	= XMARKER (OVERLAY_END (old_overlay))->insertion_type;

      overlay = allocate_misc ();
      XMISCTYPE (overlay) = Lisp_Misc_Overlay;
      OVERLAY_START (overlay) = start;
      OVERLAY_END (overlay) = end;
      OVERLAY_PLIST (overlay) = Fcopy_sequence (OVERLAY_PLIST (old_overlay));
      XOVERLAY (overlay)->next = NULL;

      if (tail)
	tail = tail->next = XOVERLAY (overlay);
      else
	result = tail = XOVERLAY (overlay);
    }

  return result;
}


/* Clone per-buffer values of buffer FROM.

   Buffer TO gets the same per-buffer values as FROM, with the
   following exceptions: (1) TO's name is left untouched, (2) markers
   are copied and made to refer to TO, and (3) overlay lists are
   copied.  */

static void
clone_per_buffer_values (struct buffer *from, struct buffer *to)
{
  Lisp_Object to_buffer;
  int offset;

  XSETBUFFER (to_buffer, to);

  /* buffer-local Lisp variables start at `undo_list',
     tho only the ones from `name' on are GC'd normally.  */
  for (offset = PER_BUFFER_VAR_OFFSET (FIRST_FIELD_PER_BUFFER);
       offset <= PER_BUFFER_VAR_OFFSET (LAST_FIELD_PER_BUFFER);
       offset += sizeof (Lisp_Object))
    {
      Lisp_Object obj;

      /* Don't touch the `name' which should be unique for every buffer.  */
      if (offset == PER_BUFFER_VAR_OFFSET (name))
	continue;

      obj = PER_BUFFER_VALUE (from, offset);
      if (MARKERP (obj) && XMARKER (obj)->buffer == from)
	{
	  struct Lisp_Marker *m = XMARKER (obj);
	  obj = Fmake_marker ();
	  XMARKER (obj)->insertion_type = m->insertion_type;
	  set_marker_both (obj, to_buffer, m->charpos, m->bytepos);
	}

      PER_BUFFER_VALUE (to, offset) = obj;
    }

  memcpy (to->local_flags, from->local_flags, sizeof to->local_flags);

  to->overlays_before = copy_overlays (to, from->overlays_before);
  to->overlays_after = copy_overlays (to, from->overlays_after);

  /* Get (a copy of) the alist of Lisp-level local variables of FROM
     and install that in TO.  */
  BVAR (to, local_var_alist) = buffer_lisp_local_variables (from);
}


/* If buffer B has markers to record PT, BEGV and ZV when it is not
   current, update these markers.  */

static void
record_buffer_markers (struct buffer *b)
{
  if (! NILP (BVAR (b, pt_marker)))
    {
      Lisp_Object buffer;

      eassert (!NILP (BVAR (b, begv_marker)));
      eassert (!NILP (BVAR (b, zv_marker)));

      XSETBUFFER (buffer, b);
      set_marker_both (BVAR (b, pt_marker), buffer, b->pt, b->pt_byte);
      set_marker_both (BVAR (b, begv_marker), buffer, b->begv, b->begv_byte);
      set_marker_both (BVAR (b, zv_marker), buffer, b->zv, b->zv_byte);
    }
}


/* If buffer B has markers to record PT, BEGV and ZV when it is not
   current, fetch these values into B->begv etc.  */

static void
fetch_buffer_markers (struct buffer *b)
{
  if (! NILP (BVAR (b, pt_marker)))
    {
      Lisp_Object m;

      eassert (!NILP (BVAR (b, begv_marker)));
      eassert (!NILP (BVAR (b, zv_marker)));

      m = BVAR (b, pt_marker);
      SET_BUF_PT_BOTH (b, marker_position (m), marker_byte_position (m));

      m = BVAR (b, begv_marker);
      SET_BUF_BEGV_BOTH (b, marker_position (m), marker_byte_position (m));

      m = BVAR (b, zv_marker);
      SET_BUF_ZV_BOTH (b, marker_position (m), marker_byte_position (m));
    }
}


DEFUN ("make-indirect-buffer", Fmake_indirect_buffer, Smake_indirect_buffer,
       2, 3,
       "bMake indirect buffer (to buffer): \nBName of indirect buffer: ",
       doc: /* Create and return an indirect buffer for buffer BASE-BUFFER, named NAME.
BASE-BUFFER should be a live buffer, or the name of an existing buffer.
NAME should be a string which is not the name of an existing buffer.
Optional argument CLONE non-nil means preserve BASE-BUFFER's state,
such as major and minor modes, in the indirect buffer.
CLONE nil means the indirect buffer's state is reset to default values.  */)
  (Lisp_Object base_buffer, Lisp_Object name, Lisp_Object clone)
{
  Lisp_Object buf, tem;
  struct buffer *b;

  CHECK_STRING (name);
  buf = Fget_buffer (name);
  if (!NILP (buf))
    error ("Buffer name `%s' is in use", SDATA (name));

  tem = base_buffer;
  base_buffer = Fget_buffer (base_buffer);
  if (NILP (base_buffer))
    error ("No such buffer: `%s'", SDATA (tem));
  if (NILP (BVAR (XBUFFER (base_buffer), name)))
    error ("Base buffer has been killed");

  if (SCHARS (name) == 0)
    error ("Empty string for buffer name is not allowed");

  b = allocate_buffer ();

  b->base_buffer = (XBUFFER (base_buffer)->base_buffer
		    ? XBUFFER (base_buffer)->base_buffer
		    : XBUFFER (base_buffer));

  /* Use the base buffer's text object.  */
  b->text = b->base_buffer->text;

  b->pt = b->base_buffer->pt;
  b->begv = b->base_buffer->begv;
  b->zv = b->base_buffer->zv;
  b->pt_byte = b->base_buffer->pt_byte;
  b->begv_byte = b->base_buffer->begv_byte;
  b->zv_byte = b->base_buffer->zv_byte;

  b->newline_cache = 0;
  b->width_run_cache = 0;
  BVAR (b, width_table) = Qnil;

  /* Put this on the chain of all buffers including killed ones.  */
  b->header.next.buffer = all_buffers;
  all_buffers = b;

  name = Fcopy_sequence (name);
  STRING_SET_INTERVALS (name, NULL_INTERVAL);
  BVAR (b, name) = name;

  reset_buffer (b);
  reset_buffer_local_variables (b, 1);

  /* Put this in the alist of all live buffers.  */
  XSETBUFFER (buf, b);
  Vbuffer_alist = nconc2 (Vbuffer_alist, Fcons (Fcons (name, buf), Qnil));

  BVAR (b, mark) = Fmake_marker ();
  BVAR (b, name) = name;

  /* The multibyte status belongs to the base buffer.  */
  BVAR (b, enable_multibyte_characters) = BVAR (b->base_buffer, enable_multibyte_characters);

  /* Make sure the base buffer has markers for its narrowing.  */
  if (NILP (BVAR (b->base_buffer, pt_marker)))
    {
      eassert (NILP (BVAR (b->base_buffer, begv_marker)));
      eassert (NILP (BVAR (b->base_buffer, zv_marker)));

      BVAR (b->base_buffer, pt_marker) = Fmake_marker ();
      set_marker_both (BVAR (b->base_buffer, pt_marker), base_buffer,
		       b->base_buffer->pt,
		       b->base_buffer->pt_byte);

      BVAR (b->base_buffer, begv_marker) = Fmake_marker ();
      set_marker_both (BVAR (b->base_buffer, begv_marker), base_buffer,
		       b->base_buffer->begv,
		       b->base_buffer->begv_byte);

      BVAR (b->base_buffer, zv_marker) = Fmake_marker ();
      set_marker_both (BVAR (b->base_buffer, zv_marker), base_buffer,
		       b->base_buffer->zv,
		       b->base_buffer->zv_byte);
      XMARKER (BVAR (b->base_buffer, zv_marker))->insertion_type = 1;
    }

  if (NILP (clone))
    {
      /* Give the indirect buffer markers for its narrowing.  */
      BVAR (b, pt_marker) = Fmake_marker ();
      set_marker_both (BVAR (b, pt_marker), buf, b->pt, b->pt_byte);
      BVAR (b, begv_marker) = Fmake_marker ();
      set_marker_both (BVAR (b, begv_marker), buf, b->begv, b->begv_byte);
      BVAR (b, zv_marker) = Fmake_marker ();
      set_marker_both (BVAR (b, zv_marker), buf, b->zv, b->zv_byte);
      XMARKER (BVAR (b, zv_marker))->insertion_type = 1;
    }
  else
    {
      struct buffer *old_b = current_buffer;

      clone_per_buffer_values (b->base_buffer, b);
      BVAR (b, filename) = Qnil;
      BVAR (b, file_truename) = Qnil;
      BVAR (b, display_count) = make_number (0);
      BVAR (b, backed_up) = Qnil;
      BVAR (b, auto_save_file_name) = Qnil;
      set_buffer_internal_1 (b);
      Fset (intern ("buffer-save-without-query"), Qnil);
      Fset (intern ("buffer-file-number"), Qnil);
      Fset (intern ("buffer-stale-function"), Qnil);
      set_buffer_internal_1 (old_b);
    }

  /* Run buffer-list-update-hook.  */
  if (!NILP (Vrun_hooks))
    call1 (Vrun_hooks, Qbuffer_list_update_hook);

  return buf;
}

void
delete_all_overlays (struct buffer *b)
{
  Lisp_Object overlay;

  /* `reset_buffer' blindly sets the list of overlays to NULL, so we
     have to empty the list, otherwise we end up with overlays that
     think they belong to this buffer while the buffer doesn't know about
     them any more.  */
  while (b->overlays_before)
    {
      XSETMISC (overlay, b->overlays_before);
      Fdelete_overlay (overlay);
    }
  while (b->overlays_after)
    {
      XSETMISC (overlay, b->overlays_after);
      Fdelete_overlay (overlay);
    }
  eassert (b->overlays_before == NULL);
  eassert (b->overlays_after == NULL);
}

/* Reinitialize everything about a buffer except its name and contents
   and local variables.
   If called on an already-initialized buffer, the list of overlays
   should be deleted before calling this function, otherwise we end up
   with overlays that claim to belong to the buffer but the buffer
   claims it doesn't belong to it.  */

void
reset_buffer (register struct buffer *b)
{
  BVAR (b, filename) = Qnil;
  BVAR (b, file_truename) = Qnil;
  BVAR (b, directory) = (current_buffer) ? BVAR (current_buffer, directory) : Qnil;
  b->modtime = 0;
  b->modtime_size = -1;
  XSETFASTINT (BVAR (b, save_length), 0);
  b->last_window_start = 1;
  /* It is more conservative to start out "changed" than "unchanged".  */
  b->clip_changed = 0;
  b->prevent_redisplay_optimizations_p = 1;
  BVAR (b, backed_up) = Qnil;
  BUF_AUTOSAVE_MODIFF (b) = 0;
  b->auto_save_failure_time = 0;
  BVAR (b, auto_save_file_name) = Qnil;
  BVAR (b, read_only) = Qnil;
  b->overlays_before = NULL;
  b->overlays_after = NULL;
  b->overlay_center = BEG;
  BVAR (b, mark_active) = Qnil;
  BVAR (b, point_before_scroll) = Qnil;
  BVAR (b, file_format) = Qnil;
  BVAR (b, auto_save_file_format) = Qt;
  BVAR (b, last_selected_window) = Qnil;
  XSETINT (BVAR (b, display_count), 0);
  BVAR (b, display_time) = Qnil;
  BVAR (b, enable_multibyte_characters) = BVAR (&buffer_defaults, enable_multibyte_characters);
  BVAR (b, cursor_type) = BVAR (&buffer_defaults, cursor_type);
  BVAR (b, extra_line_spacing) = BVAR (&buffer_defaults, extra_line_spacing);

  b->display_error_modiff = 0;
}

/* Reset buffer B's local variables info.
   Don't use this on a buffer that has already been in use;
   it does not treat permanent locals consistently.
   Instead, use Fkill_all_local_variables.

   If PERMANENT_TOO is 1, then we reset permanent
   buffer-local variables.  If PERMANENT_TOO is 0,
   we preserve those.  */

static void
reset_buffer_local_variables (register struct buffer *b, int permanent_too)
{
  register int offset;
  int i;

  /* Reset the major mode to Fundamental, together with all the
     things that depend on the major mode.
     default-major-mode is handled at a higher level.
     We ignore it here.  */
  BVAR (b, major_mode) = Qfundamental_mode;
  BVAR (b, keymap) = Qnil;
  BVAR (b, mode_name) = QSFundamental;
  BVAR (b, minor_modes) = Qnil;

  /* If the standard case table has been altered and invalidated,
     fix up its insides first.  */
  if (! (CHAR_TABLE_P (XCHAR_TABLE (Vascii_downcase_table)->extras[0])
	 && CHAR_TABLE_P (XCHAR_TABLE (Vascii_downcase_table)->extras[1])
	 && CHAR_TABLE_P (XCHAR_TABLE (Vascii_downcase_table)->extras[2])))
    Fset_standard_case_table (Vascii_downcase_table);

  BVAR (b, downcase_table) = Vascii_downcase_table;
  BVAR (b, upcase_table) = XCHAR_TABLE (Vascii_downcase_table)->extras[0];
  BVAR (b, case_canon_table) = XCHAR_TABLE (Vascii_downcase_table)->extras[1];
  BVAR (b, case_eqv_table) = XCHAR_TABLE (Vascii_downcase_table)->extras[2];
  BVAR (b, invisibility_spec) = Qt;

  /* Reset all (or most) per-buffer variables to their defaults.  */
  if (permanent_too)
    BVAR (b, local_var_alist) = Qnil;
  else
    {
      Lisp_Object tmp, prop, last = Qnil;
      for (tmp = BVAR (b, local_var_alist); CONSP (tmp); tmp = XCDR (tmp))
	if (!NILP (prop = Fget (XCAR (XCAR (tmp)), Qpermanent_local)))
	  {
	    /* If permanent-local, keep it.  */
	    last = tmp;
	    if (EQ (prop, Qpermanent_local_hook))
	      {
		/* This is a partially permanent hook variable.
		   Preserve only the elements that want to be preserved.  */
		Lisp_Object list, newlist;
		list = XCDR (XCAR (tmp));
		if (!CONSP (list))
		  newlist = list;
		else
		  for (newlist = Qnil; CONSP (list); list = XCDR (list))
		    {
		      Lisp_Object elt = XCAR (list);
		      /* Preserve element ELT if it's t,
			 if it is a function with a `permanent-local-hook' property,
			 or if it's not a symbol.  */
		      if (! SYMBOLP (elt)
			  || EQ (elt, Qt)
			  || !NILP (Fget (elt, Qpermanent_local_hook)))
			newlist = Fcons (elt, newlist);
		    }
		XSETCDR (XCAR (tmp), Fnreverse (newlist));
	      }
	  }
	/* Delete this local variable.  */
	else if (NILP (last))
	  BVAR (b, local_var_alist) = XCDR (tmp);
	else
	  XSETCDR (last, XCDR (tmp));
    }

  for (i = 0; i < last_per_buffer_idx; ++i)
    if (permanent_too || buffer_permanent_local_flags[i] == 0)
      SET_PER_BUFFER_VALUE_P (b, i, 0);

  /* For each slot that has a default value,
     copy that into the slot.  */

  /* buffer-local Lisp variables start at `undo_list',
     tho only the ones from `name' on are GC'd normally.  */
  for (offset = PER_BUFFER_VAR_OFFSET (FIRST_FIELD_PER_BUFFER);
       offset <= PER_BUFFER_VAR_OFFSET (LAST_FIELD_PER_BUFFER);
       offset += sizeof (Lisp_Object))
    {
      int idx = PER_BUFFER_IDX (offset);
      if ((idx > 0
	   && (permanent_too
	       || buffer_permanent_local_flags[idx] == 0)))
	PER_BUFFER_VALUE (b, offset) = PER_BUFFER_DEFAULT (offset);
    }
}

/* We split this away from generate-new-buffer, because rename-buffer
   and set-visited-file-name ought to be able to use this to really
   rename the buffer properly.  */

DEFUN ("generate-new-buffer-name", Fgenerate_new_buffer_name,
       Sgenerate_new_buffer_name, 1, 2, 0,
       doc: /* Return a string that is the name of no existing buffer based on NAME.
If there is no live buffer named NAME, then return NAME.
Otherwise modify name by appending `<NUMBER>', incrementing NUMBER
\(starting at 2) until an unused name is found, and then return that name.
Optional second argument IGNORE specifies a name that is okay to use (if
it is in the sequence to be tried) even if a buffer with that name exists.  */)
  (register Lisp_Object name, Lisp_Object ignore)
{
  register Lisp_Object gentemp, tem;
  EMACS_INT count;
  char number[INT_BUFSIZE_BOUND (EMACS_INT) + sizeof "<>"];

  CHECK_STRING (name);

  tem = Fstring_equal (name, ignore);
  if (!NILP (tem))
    return name;
  tem = Fget_buffer (name);
  if (NILP (tem))
    return name;

  count = 1;
  while (1)
    {
      sprintf (number, "<%"pI"d>", ++count);
      gentemp = concat2 (name, build_string (number));
      tem = Fstring_equal (gentemp, ignore);
      if (!NILP (tem))
	return gentemp;
      tem = Fget_buffer (gentemp);
      if (NILP (tem))
	return gentemp;
    }
}


DEFUN ("buffer-name", Fbuffer_name, Sbuffer_name, 0, 1, 0,
       doc: /* Return the name of BUFFER, as a string.
BUFFER defaults to the current buffer.
Return nil if BUFFER has been killed.  */)
  (register Lisp_Object buffer)
{
  if (NILP (buffer))
    return BVAR (current_buffer, name);
  CHECK_BUFFER (buffer);
  return BVAR (XBUFFER (buffer), name);
}

DEFUN ("buffer-file-name", Fbuffer_file_name, Sbuffer_file_name, 0, 1, 0,
       doc: /* Return name of file BUFFER is visiting, or nil if none.
No argument or nil as argument means use the current buffer.  */)
  (register Lisp_Object buffer)
{
  if (NILP (buffer))
    return BVAR (current_buffer, filename);
  CHECK_BUFFER (buffer);
  return BVAR (XBUFFER (buffer), filename);
}

DEFUN ("buffer-base-buffer", Fbuffer_base_buffer, Sbuffer_base_buffer,
       0, 1, 0,
       doc: /* Return the base buffer of indirect buffer BUFFER.
If BUFFER is not indirect, return nil.
BUFFER defaults to the current buffer.  */)
  (register Lisp_Object buffer)
{
  struct buffer *base;
  Lisp_Object base_buffer;

  if (NILP (buffer))
    base = current_buffer->base_buffer;
  else
    {
      CHECK_BUFFER (buffer);
      base = XBUFFER (buffer)->base_buffer;
    }

  if (! base)
    return Qnil;
  XSETBUFFER (base_buffer, base);
  return base_buffer;
}

DEFUN ("buffer-local-value", Fbuffer_local_value,
       Sbuffer_local_value, 2, 2, 0,
       doc: /* Return the value of VARIABLE in BUFFER.
If VARIABLE does not have a buffer-local binding in BUFFER, the value
is the default binding of the variable. */)
  (register Lisp_Object variable, register Lisp_Object buffer)
{
  register struct buffer *buf;
  register Lisp_Object result;
  struct Lisp_Symbol *sym;

  CHECK_SYMBOL (variable);
  CHECK_BUFFER (buffer);
  buf = XBUFFER (buffer);
  sym = XSYMBOL (variable);

 start:
  switch (sym->redirect)
    {
    case SYMBOL_VARALIAS: sym = indirect_variable (sym); goto start;
    case SYMBOL_PLAINVAL: result = SYMBOL_VAL (sym); break;
    case SYMBOL_LOCALIZED:
      { /* Look in local_var_alist.  */
	struct Lisp_Buffer_Local_Value *blv = SYMBOL_BLV (sym);
	XSETSYMBOL (variable, sym); /* Update In case of aliasing.  */
	result = Fassoc (variable, BVAR (buf, local_var_alist));
	if (!NILP (result))
	  {
	    if (blv->fwd)
	      { /* What binding is loaded right now?  */
		Lisp_Object current_alist_element = blv->valcell;

		/* The value of the currently loaded binding is not
		   stored in it, but rather in the realvalue slot.
		   Store that value into the binding it belongs to
		   in case that is the one we are about to use.  */

		XSETCDR (current_alist_element,
			 do_symval_forwarding (blv->fwd));
	      }
	    /* Now get the (perhaps updated) value out of the binding.  */
	    result = XCDR (result);
	  }
	else
	  result = Fdefault_value (variable);
	break;
      }
    case SYMBOL_FORWARDED:
      {
	union Lisp_Fwd *fwd = SYMBOL_FWD (sym);
	if (BUFFER_OBJFWDP (fwd))
	  result = PER_BUFFER_VALUE (buf, XBUFFER_OBJFWD (fwd)->offset);
	else
	  result = Fdefault_value (variable);
	break;
      }
    default: abort ();
    }

  if (!EQ (result, Qunbound))
    return result;

  xsignal1 (Qvoid_variable, variable);
}

/* Return an alist of the Lisp-level buffer-local bindings of
   buffer BUF.  That is, don't include the variables maintained
   in special slots in the buffer object.  */

static Lisp_Object
buffer_lisp_local_variables (struct buffer *buf)
{
  Lisp_Object result = Qnil;
  register Lisp_Object tail;
  for (tail = BVAR (buf, local_var_alist); CONSP (tail); tail = XCDR (tail))
    {
      Lisp_Object val, elt;

      elt = XCAR (tail);

      /* Reference each variable in the alist in buf.
	 If inquiring about the current buffer, this gets the current values,
	 so store them into the alist so the alist is up to date.
	 If inquiring about some other buffer, this swaps out any values
	 for that buffer, making the alist up to date automatically.  */
      val = find_symbol_value (XCAR (elt));
      /* Use the current buffer value only if buf is the current buffer.  */
      if (buf != current_buffer)
	val = XCDR (elt);

      result = Fcons (EQ (val, Qunbound)
		      ? XCAR (elt)
		      : Fcons (XCAR (elt), val),
		      result);
    }

  return result;
}

DEFUN ("buffer-local-variables", Fbuffer_local_variables,
       Sbuffer_local_variables, 0, 1, 0,
       doc: /* Return an alist of variables that are buffer-local in BUFFER.
Most elements look like (SYMBOL . VALUE), describing one variable.
For a symbol that is locally unbound, just the symbol appears in the value.
Note that storing new VALUEs in these elements doesn't change the variables.
No argument or nil as argument means use current buffer as BUFFER.  */)
  (register Lisp_Object buffer)
{
  register struct buffer *buf;
  register Lisp_Object result;

  if (NILP (buffer))
    buf = current_buffer;
  else
    {
      CHECK_BUFFER (buffer);
      buf = XBUFFER (buffer);
    }

  result = buffer_lisp_local_variables (buf);

  /* Add on all the variables stored in special slots.  */
  {
    int offset, idx;

    /* buffer-local Lisp variables start at `undo_list',
       tho only the ones from `name' on are GC'd normally.  */
    for (offset = PER_BUFFER_VAR_OFFSET (FIRST_FIELD_PER_BUFFER);
	 offset <= PER_BUFFER_VAR_OFFSET (LAST_FIELD_PER_BUFFER);
	 /* sizeof EMACS_INT == sizeof Lisp_Object */
	 offset += (sizeof (EMACS_INT)))
      {
	idx = PER_BUFFER_IDX (offset);
	if ((idx == -1 || PER_BUFFER_VALUE_P (buf, idx))
	    && SYMBOLP (PER_BUFFER_SYMBOL (offset)))
	  {
	    Lisp_Object sym = PER_BUFFER_SYMBOL (offset);
	    Lisp_Object val = PER_BUFFER_VALUE (buf, offset);
	    result = Fcons (EQ (val, Qunbound) ? sym : Fcons (sym, val),
			    result);
	  }
      }
  }

  return result;
}

DEFUN ("buffer-modified-p", Fbuffer_modified_p, Sbuffer_modified_p,
       0, 1, 0,
       doc: /* Return t if BUFFER was modified since its file was last read or saved.
No argument or nil as argument means use current buffer as BUFFER.  */)
  (register Lisp_Object buffer)
{
  register struct buffer *buf;
  if (NILP (buffer))
    buf = current_buffer;
  else
    {
      CHECK_BUFFER (buffer);
      buf = XBUFFER (buffer);
    }

  return BUF_SAVE_MODIFF (buf) < BUF_MODIFF (buf) ? Qt : Qnil;
}

DEFUN ("set-buffer-modified-p", Fset_buffer_modified_p, Sset_buffer_modified_p,
       1, 1, 0,
       doc: /* Mark current buffer as modified or unmodified according to FLAG.
A non-nil FLAG means mark the buffer modified.  */)
  (register Lisp_Object flag)
{
  register int already;
  register Lisp_Object fn;
  Lisp_Object buffer, window;

#ifdef CLASH_DETECTION
  /* If buffer becoming modified, lock the file.
     If buffer becoming unmodified, unlock the file.  */

  fn = BVAR (current_buffer, file_truename);
  /* Test buffer-file-name so that binding it to nil is effective.  */
  if (!NILP (fn) && ! NILP (BVAR (current_buffer, filename)))
    {
      already = SAVE_MODIFF < MODIFF;
      if (!already && !NILP (flag))
	lock_file (fn);
      else if (already && NILP (flag))
	unlock_file (fn);
    }
#endif /* CLASH_DETECTION */

  /* Here we have a problem.  SAVE_MODIFF is used here to encode
     buffer-modified-p (as SAVE_MODIFF<MODIFF) as well as
     recent-auto-save-p (as SAVE_MODIFF<auto_save_modified).  So if we
     modify SAVE_MODIFF to affect one, we may affect the other
     as well.
     E.g. if FLAG is nil we need to set SAVE_MODIFF to MODIFF, but
     if SAVE_MODIFF<auto_save_modified that means we risk changing
     recent-auto-save-p from t to nil.
     Vice versa, if FLAG is non-nil and SAVE_MODIFF>=auto_save_modified
     we risk changing recent-auto-save-p from nil to t.  */
  SAVE_MODIFF = (NILP (flag)
		 /* FIXME: This unavoidably sets recent-auto-save-p to nil.  */
		 ? MODIFF
		 /* Let's try to preserve recent-auto-save-p.  */
		 : SAVE_MODIFF < MODIFF ? SAVE_MODIFF
		 /* If SAVE_MODIFF == auto_save_modified == MODIFF,
		    we can either decrease SAVE_MODIFF and auto_save_modified
		    or increase MODIFF.  */
		 : MODIFF++);

  /* Set update_mode_lines only if buffer is displayed in some window.
     Packages like jit-lock or lazy-lock preserve a buffer's modified
     state by recording/restoring the state around blocks of code.
     Setting update_mode_lines makes redisplay consider all windows
     (on all frames).  Stealth fontification of buffers not displayed
     would incur additional redisplay costs if we'd set
     update_modes_lines unconditionally.

     Ideally, I think there should be another mechanism for fontifying
     buffers without "modifying" buffers, or redisplay should be
     smarter about updating the `*' in mode lines.  --gerd  */
  XSETBUFFER (buffer, current_buffer);
  window = Fget_buffer_window (buffer, Qt);
  if (WINDOWP (window))
    {
      ++update_mode_lines;
      current_buffer->prevent_redisplay_optimizations_p = 1;
    }

  return flag;
}

DEFUN ("restore-buffer-modified-p", Frestore_buffer_modified_p,
       Srestore_buffer_modified_p, 1, 1, 0,
       doc: /* Like `set-buffer-modified-p', with a difference concerning redisplay.
It is not ensured that mode lines will be updated to show the modified
state of the current buffer.  Use with care.  */)
  (Lisp_Object flag)
{
#ifdef CLASH_DETECTION
  Lisp_Object fn;

  /* If buffer becoming modified, lock the file.
     If buffer becoming unmodified, unlock the file.  */

  fn = BVAR (current_buffer, file_truename);
  /* Test buffer-file-name so that binding it to nil is effective.  */
  if (!NILP (fn) && ! NILP (BVAR (current_buffer, filename)))
    {
      int already = SAVE_MODIFF < MODIFF;
      if (!already && !NILP (flag))
	lock_file (fn);
      else if (already && NILP (flag))
	unlock_file (fn);
    }
#endif /* CLASH_DETECTION */

  SAVE_MODIFF = NILP (flag) ? MODIFF : 0;
  return flag;
}

DEFUN ("buffer-modified-tick", Fbuffer_modified_tick, Sbuffer_modified_tick,
       0, 1, 0,
       doc: /* Return BUFFER's tick counter, incremented for each change in text.
Each buffer has a tick counter which is incremented each time the
text in that buffer is changed.  It wraps around occasionally.
No argument or nil as argument means use current buffer as BUFFER.  */)
  (register Lisp_Object buffer)
{
  register struct buffer *buf;
  if (NILP (buffer))
    buf = current_buffer;
  else
    {
      CHECK_BUFFER (buffer);
      buf = XBUFFER (buffer);
    }

  return make_number (BUF_MODIFF (buf));
}

DEFUN ("buffer-chars-modified-tick", Fbuffer_chars_modified_tick,
       Sbuffer_chars_modified_tick, 0, 1, 0,
       doc: /* Return BUFFER's character-change tick counter.
Each buffer has a character-change tick counter, which is set to the
value of the buffer's tick counter \(see `buffer-modified-tick'), each
time text in that buffer is inserted or deleted.  By comparing the
values returned by two individual calls of `buffer-chars-modified-tick',
you can tell whether a character change occurred in that buffer in
between these calls.  No argument or nil as argument means use current
buffer as BUFFER.  */)
  (register Lisp_Object buffer)
{
  register struct buffer *buf;
  if (NILP (buffer))
    buf = current_buffer;
  else
    {
      CHECK_BUFFER (buffer);
      buf = XBUFFER (buffer);
    }

  return make_number (BUF_CHARS_MODIFF (buf));
}

DEFUN ("rename-buffer", Frename_buffer, Srename_buffer, 1, 2,
       "(list (read-string \"Rename buffer (to new name): \" \
	      nil 'buffer-name-history (buffer-name (current-buffer))) \
	      current-prefix-arg)",
       doc: /* Change current buffer's name to NEWNAME (a string).
If second arg UNIQUE is nil or omitted, it is an error if a
buffer named NEWNAME already exists.
If UNIQUE is non-nil, come up with a new name using
`generate-new-buffer-name'.
Interactively, you can set UNIQUE with a prefix argument.
We return the name we actually gave the buffer.
This does not change the name of the visited file (if any).  */)
  (register Lisp_Object newname, Lisp_Object unique)
{
  register Lisp_Object tem, buf;

  CHECK_STRING (newname);

  if (SCHARS (newname) == 0)
    error ("Empty string is invalid as a buffer name");

  tem = Fget_buffer (newname);
  if (!NILP (tem))
    {
      /* Don't short-circuit if UNIQUE is t.  That is a useful way to
	 rename the buffer automatically so you can create another
	 with the original name.  It makes UNIQUE equivalent to
	 (rename-buffer (generate-new-buffer-name NEWNAME)).  */
      if (NILP (unique) && XBUFFER (tem) == current_buffer)
	return BVAR (current_buffer, name);
      if (!NILP (unique))
	newname = Fgenerate_new_buffer_name (newname, BVAR (current_buffer, name));
      else
	error ("Buffer name `%s' is in use", SDATA (newname));
    }

  BVAR (current_buffer, name) = newname;

  /* Catch redisplay's attention.  Unless we do this, the mode lines for
     any windows displaying current_buffer will stay unchanged.  */
  update_mode_lines++;

  XSETBUFFER (buf, current_buffer);
  Fsetcar (Frassq (buf, Vbuffer_alist), newname);
  if (NILP (BVAR (current_buffer, filename))
      && !NILP (BVAR (current_buffer, auto_save_file_name)))
    call0 (intern ("rename-auto-save-file"));

  /* Run buffer-list-update-hook.  */
  if (!NILP (Vrun_hooks))
    call1 (Vrun_hooks, Qbuffer_list_update_hook);

  /* Refetch since that last call may have done GC.  */
  return BVAR (current_buffer, name);
}

DEFUN ("other-buffer", Fother_buffer, Sother_buffer, 0, 3, 0,
       doc: /* Return most recently selected buffer other than BUFFER.
Buffers not visible in windows are preferred to visible buffers, unless
optional second argument VISIBLE-OK is non-nil.  Ignore the argument
BUFFER unless it denotes a live buffer.  If the optional third argument
FRAME is non-nil, use that frame's buffer list instead of the selected
frame's buffer list.

The buffer is found by scanning the selected or specified frame's buffer
list first, followed by the list of all buffers.  If no other buffer
exists, return the buffer `*scratch*' (creating it if necessary).  */)
  (register Lisp_Object buffer, Lisp_Object visible_ok, Lisp_Object frame)
{
  Lisp_Object Fset_buffer_major_mode (Lisp_Object buffer);
  Lisp_Object tail, buf, pred;
  Lisp_Object notsogood = Qnil;

  if (NILP (frame))
    frame = selected_frame;

  CHECK_FRAME (frame);

  pred = frame_buffer_predicate (frame);
  /* Consider buffers that have been seen in the frame first.  */
  tail = XFRAME (frame)->buffer_list;
  for (; CONSP (tail); tail = XCDR (tail))
    {
      buf = XCAR (tail);
      if (BUFFERP (buf) && !EQ (buf, buffer)
	  && !NILP (BVAR (XBUFFER (buf), name))
	  && (SREF (BVAR (XBUFFER (buf), name), 0) != ' ')
	  /* If the frame has a buffer_predicate, disregard buffers that
	     don't fit the predicate.  */
	  && (NILP (pred) || !NILP (call1 (pred, buf))))
	{
	  if (!NILP (visible_ok)
	      || NILP (Fget_buffer_window (buf, Qvisible)))
	    return buf;
	  else if (NILP (notsogood))
	    notsogood = buf;
	}
    }

  /* Consider alist of all buffers next.  */
  tail = Vbuffer_alist;
  for (; CONSP (tail); tail = XCDR (tail))
    {
      buf = Fcdr (XCAR (tail));
      if (BUFFERP (buf) && !EQ (buf, buffer)
	  && !NILP (BVAR (XBUFFER (buf), name))
	  && (SREF (BVAR (XBUFFER (buf), name), 0) != ' ')
	  /* If the frame has a buffer_predicate, disregard buffers that
	     don't fit the predicate.  */
	  && (NILP (pred) || !NILP (call1 (pred, buf))))
	{
	  if (!NILP (visible_ok)
	      || NILP (Fget_buffer_window (buf, Qvisible)))
	    return buf;
	  else if (NILP (notsogood))
	    notsogood = buf;
	}
    }

  if (!NILP (notsogood))
    return notsogood;
  else
    {
      buf = Fget_buffer (build_string ("*scratch*"));
      if (NILP (buf))
	{
	  buf = Fget_buffer_create (build_string ("*scratch*"));
	  Fset_buffer_major_mode (buf);
	}
      return buf;
    }
}

/* The following function is a safe variant of Fother_buffer: It doesn't
   pay attention to any frame-local buffer lists, doesn't care about
   visibility of buffers, and doesn't evaluate any frame predicates.  */

Lisp_Object
other_buffer_safely (Lisp_Object buffer)
{
  Lisp_Object Fset_buffer_major_mode (Lisp_Object buffer);
  Lisp_Object tail, buf;

  tail = Vbuffer_alist;
  for (; CONSP (tail); tail = XCDR (tail))
    {
      buf = Fcdr (XCAR (tail));
      if (BUFFERP (buf) && !EQ (buf, buffer)
	  && !NILP (BVAR (XBUFFER (buf), name))
	  && (SREF (BVAR (XBUFFER (buf), name), 0) != ' '))
	return buf;
    }

  buf = Fget_buffer (build_string ("*scratch*"));
  if (NILP (buf))
    {
      buf = Fget_buffer_create (build_string ("*scratch*"));
      Fset_buffer_major_mode (buf);
    }

  return buf;
}

DEFUN ("buffer-enable-undo", Fbuffer_enable_undo, Sbuffer_enable_undo,
       0, 1, "",
       doc: /* Start keeping undo information for buffer BUFFER.
No argument or nil as argument means do this for the current buffer.  */)
  (register Lisp_Object buffer)
{
  Lisp_Object real_buffer;

  if (NILP (buffer))
    XSETBUFFER (real_buffer, current_buffer);
  else
    {
      real_buffer = Fget_buffer (buffer);
      if (NILP (real_buffer))
	nsberror (buffer);
    }

  if (EQ (BVAR (XBUFFER (real_buffer), undo_list), Qt))
    BVAR (XBUFFER (real_buffer), undo_list) = Qnil;

  return Qnil;
}

/*
  DEFVAR_LISP ("kill-buffer-hook", ..., "\
Hook to be run (by `run-hooks', which see) when a buffer is killed.\n\
The buffer being killed will be current while the hook is running.\n\
See `kill-buffer'."
 */
DEFUN ("kill-buffer", Fkill_buffer, Skill_buffer, 0, 1, "bKill buffer: ",
       doc: /* Kill buffer BUFFER-OR-NAME.
The argument may be a buffer or the name of an existing buffer.
Argument nil or omitted means kill the current buffer.  Return t if the
buffer is actually killed, nil otherwise.

This function calls `replace-buffer-in-windows' for cleaning up all
windows currently displaying the buffer to be killed.  The functions in
`kill-buffer-query-functions' are called with the buffer to be killed as
the current buffer.  If any of them returns nil, the buffer is not
killed.  The hook `kill-buffer-hook' is run before the buffer is
actually killed.  The buffer being killed will be current while the hook
is running.

Any processes that have this buffer as the `process-buffer' are killed
with SIGHUP.  */)
  (Lisp_Object buffer_or_name)
{
  Lisp_Object buffer;
  register struct buffer *b;
  register Lisp_Object tem;
  register struct Lisp_Marker *m;
  struct gcpro gcpro1;

  if (NILP (buffer_or_name))
    buffer = Fcurrent_buffer ();
  else
    buffer = Fget_buffer (buffer_or_name);
  if (NILP (buffer))
    nsberror (buffer_or_name);

  b = XBUFFER (buffer);

  /* Avoid trouble for buffer already dead.  */
  if (NILP (BVAR (b, name)))
    return Qnil;

  /* Query if the buffer is still modified.  */
  if (INTERACTIVE && !NILP (BVAR (b, filename))
      && BUF_MODIFF (b) > BUF_SAVE_MODIFF (b))
    {
      GCPRO1 (buffer);
      tem = do_yes_or_no_p (format2 ("Buffer %s modified; kill anyway? ",
				     BVAR (b, name), make_number (0)));
      UNGCPRO;
      if (NILP (tem))
	return Qnil;
    }

  /* Run hooks with the buffer to be killed the current buffer.  */
  {
    int count = SPECPDL_INDEX ();
    Lisp_Object arglist[1];

    record_unwind_protect (save_excursion_restore, save_excursion_save ());
    set_buffer_internal (b);

    /* First run the query functions; if any query is answered no,
       don't kill the buffer.  */
    arglist[0] = Qkill_buffer_query_functions;
    tem = Frun_hook_with_args_until_failure (1, arglist);
    if (NILP (tem))
      return unbind_to (count, Qnil);

    /* Then run the hooks.  */
    Frun_hooks (1, &Qkill_buffer_hook);
    unbind_to (count, Qnil);
  }

  /* We have no more questions to ask.  Verify that it is valid
     to kill the buffer.  This must be done after the questions
     since anything can happen within do_yes_or_no_p.  */

  /* Don't kill the minibuffer now current.  */
  if (EQ (buffer, XWINDOW (minibuf_window)->buffer))
    return Qnil;

  if (NILP (BVAR (b, name)))
    return Qnil;

  /* When we kill a base buffer, kill all its indirect buffers.
     We do it at this stage so nothing terrible happens if they
     ask questions or their hooks get errors.  */
  if (! b->base_buffer)
    {
      struct buffer *other;

      GCPRO1 (buffer);

      for (other = all_buffers; other; other = other->header.next.buffer)
	/* all_buffers contains dead buffers too;
	   don't re-kill them.  */
	if (other->base_buffer == b && !NILP (BVAR (other, name)))
	  {
	    Lisp_Object buf;
	    XSETBUFFER (buf, other);
	    Fkill_buffer (buf);
	  }

      UNGCPRO;
    }

  /* Run replace_buffer_in_windows before making another buffer current
     since set-window-buffer-start-and-point will refuse to make another
     buffer current if the selected window does not show the current
     buffer.  (Bug#10114) */
  replace_buffer_in_windows (buffer);

     /* Make this buffer not be current.
     In the process, notice if this is the sole visible buffer
     and give up if so.  */
  if (b == current_buffer)
    {
      tem = Fother_buffer (buffer, Qnil, Qnil);
      Fset_buffer (tem);
      if (b == current_buffer)
	return Qnil;
    }

  /* Notice if the buffer to kill is the sole visible buffer
     when we're currently in the mini-buffer, and give up if so.  */
  XSETBUFFER (tem, current_buffer);
  if (EQ (tem, XWINDOW (minibuf_window)->buffer))
    {
      tem = Fother_buffer (buffer, Qnil, Qnil);
      if (EQ (buffer, tem))
	return Qnil;
    }

  /* Now there is no question: we can kill the buffer.  */

#ifdef CLASH_DETECTION
  /* Unlock this buffer's file, if it is locked.  */
  unlock_buffer (b);
#endif /* CLASH_DETECTION */

  GCPRO1 (buffer);
  kill_buffer_processes (buffer);
  UNGCPRO;

  /* Killing buffer processes may run sentinels which may
     have called kill-buffer.  */

  if (NILP (BVAR (b, name)))
    return Qnil;

  /* These may run Lisp code and into infinite loops (if someone
     insisted on circular lists) so allow quitting here.  */
  frames_discard_buffer (buffer);

  clear_charpos_cache (b);

  tem = Vinhibit_quit;
  Vinhibit_quit = Qt;
  /* Remove the buffer from the list of all buffers.  */
  Vbuffer_alist = Fdelq (Frassq (buffer, Vbuffer_alist), Vbuffer_alist);
  /* If replace_buffer_in_windows didn't do its job correctly fix that
     now.  */
  replace_buffer_in_windows_safely (buffer);
  Vinhibit_quit = tem;

  /* Delete any auto-save file, if we saved it in this session.
     But not if the buffer is modified.  */
  if (STRINGP (BVAR (b, auto_save_file_name))
      && BUF_AUTOSAVE_MODIFF (b) != 0
      && BUF_SAVE_MODIFF (b) < BUF_AUTOSAVE_MODIFF (b)
      && BUF_SAVE_MODIFF (b) < BUF_MODIFF (b)
      && NILP (Fsymbol_value (intern ("auto-save-visited-file-name"))))
    {
      Lisp_Object delete;
      delete = Fsymbol_value (intern ("delete-auto-save-files"));
      if (! NILP (delete))
	internal_delete_file (BVAR (b, auto_save_file_name));
    }

  if (b->base_buffer)
    {
      /* Unchain all markers that belong to this indirect buffer.
	 Don't unchain the markers that belong to the base buffer
	 or its other indirect buffers.  */
      for (m = BUF_MARKERS (b); m; )
	{
	  struct Lisp_Marker *next = m->next;
	  if (m->buffer == b)
	    unchain_marker (m);
	  m = next;
	}
    }
  else
    {
      /* Unchain all markers of this buffer and its indirect buffers.
	 and leave them pointing nowhere.  */
      for (m = BUF_MARKERS (b); m; )
	{
	  struct Lisp_Marker *next = m->next;
	  m->buffer = 0;
	  m->next = NULL;
	  m = next;
	}
      BUF_MARKERS (b) = NULL;
      BUF_INTERVALS (b) = NULL_INTERVAL;

      /* Perhaps we should explicitly free the interval tree here... */
    }

  /* Reset the local variables, so that this buffer's local values
     won't be protected from GC.  They would be protected
     if they happened to remain cached in their symbols.
     This gets rid of them for certain.  */
  swap_out_buffer_local_variables (b);
  reset_buffer_local_variables (b, 1);

  BVAR (b, name) = Qnil;

  BLOCK_INPUT;
  if (! b->base_buffer)
    free_buffer_text (b);

  if (b->newline_cache)
    {
      free_region_cache (b->newline_cache);
      b->newline_cache = 0;
    }
  if (b->width_run_cache)
    {
      free_region_cache (b->width_run_cache);
      b->width_run_cache = 0;
    }
  BVAR (b, width_table) = Qnil;
  UNBLOCK_INPUT;
  BVAR (b, undo_list) = Qnil;

  /* Run buffer-list-update-hook.  */
  if (!NILP (Vrun_hooks))
    call1 (Vrun_hooks, Qbuffer_list_update_hook);

  return Qt;
}

/* Move association for BUFFER to the front of buffer (a)lists.  Since
   we do this each time BUFFER is selected visibly, the more recently
   selected buffers are always closer to the front of those lists.  This
   means that other_buffer is more likely to choose a relevant buffer.

   Note that this moves BUFFER to the front of the buffer lists of the
   selected frame even if BUFFER is not shown there.  If BUFFER is not
   shown in the selected frame, consider the present behavior a feature.
   `select-window' gets this right since it shows BUFFER in the selected
   window when calling us.  */

void
record_buffer (Lisp_Object buffer)
{
  Lisp_Object aelt, aelt_cons, tem;
  register struct frame *f = XFRAME (selected_frame);

  CHECK_BUFFER (buffer);

  /* Update Vbuffer_alist (we know that it has an entry for BUFFER).
     Don't allow quitting since this might leave the buffer list in an
     inconsistent state.  */
  tem = Vinhibit_quit;
  Vinhibit_quit = Qt;
  aelt = Frassq (buffer, Vbuffer_alist);
  aelt_cons = Fmemq (aelt, Vbuffer_alist);
  Vbuffer_alist = Fdelq (aelt, Vbuffer_alist);
  XSETCDR (aelt_cons, Vbuffer_alist);
  Vbuffer_alist = aelt_cons;
  Vinhibit_quit = tem;

  /* Update buffer list of selected frame.  */
  f->buffer_list = Fcons (buffer, Fdelq (buffer, f->buffer_list));
  f->buried_buffer_list = Fdelq (buffer, f->buried_buffer_list);

  /* Run buffer-list-update-hook.  */
  if (!NILP (Vrun_hooks))
    call1 (Vrun_hooks, Qbuffer_list_update_hook);
}


/* Move BUFFER to the end of the buffer (a)lists.  Do nothing if the
   buffer is killed.  For the selected frame's buffer list this moves
   BUFFER to its end even if it was never shown in that frame.  If
   this happens we have a feature, hence `bury-buffer-internal' should be
   called only when BUFFER was shown in the selected frame.  */

DEFUN ("bury-buffer-internal", Fbury_buffer_internal, Sbury_buffer_internal,
       1, 1, 0,
       doc: /* Move BUFFER to the end of the buffer list.  */)
  (Lisp_Object buffer)
{
  Lisp_Object aelt, aelt_cons, tem;
  register struct frame *f = XFRAME (selected_frame);

  CHECK_BUFFER (buffer);

  /* Update Vbuffer_alist (we know that it has an entry for BUFFER).
     Don't allow quitting since this might leave the buffer list in an
     inconsistent state.  */
  tem = Vinhibit_quit;
  Vinhibit_quit = Qt;
  aelt = Frassq (buffer, Vbuffer_alist);
  aelt_cons = Fmemq (aelt, Vbuffer_alist);
  Vbuffer_alist = Fdelq (aelt, Vbuffer_alist);
  XSETCDR (aelt_cons, Qnil);
  Vbuffer_alist = nconc2 (Vbuffer_alist, aelt_cons);
  Vinhibit_quit = tem;

  /* Update buffer lists of selected frame.  */
  f->buffer_list = Fdelq (buffer, f->buffer_list);
  f->buried_buffer_list = Fcons (buffer, Fdelq (buffer, f->buried_buffer_list));

  /* Run buffer-list-update-hook.  */
  if (!NILP (Vrun_hooks))
    call1 (Vrun_hooks, Qbuffer_list_update_hook);

  return Qnil;
}

DEFUN ("set-buffer-major-mode", Fset_buffer_major_mode, Sset_buffer_major_mode, 1, 1, 0,
       doc: /* Set an appropriate major mode for BUFFER.
For the *scratch* buffer, use `initial-major-mode', otherwise choose a mode
according to `default-major-mode'.
Use this function before selecting the buffer, since it may need to inspect
the current buffer's major mode.  */)
  (Lisp_Object buffer)
{
  int count;
  Lisp_Object function;

  CHECK_BUFFER (buffer);

  if (STRINGP (BVAR (XBUFFER (buffer), name))
      && strcmp (SSDATA (BVAR (XBUFFER (buffer), name)), "*scratch*") == 0)
    function = find_symbol_value (intern ("initial-major-mode"));
  else
    {
      function = BVAR (&buffer_defaults, major_mode);
      if (NILP (function)
	  && NILP (Fget (BVAR (current_buffer, major_mode), Qmode_class)))
	function = BVAR (current_buffer, major_mode);
    }

  if (NILP (function) || EQ (function, Qfundamental_mode))
    return Qnil;

  count = SPECPDL_INDEX ();

  /* To select a nonfundamental mode,
     select the buffer temporarily and then call the mode function. */

  record_unwind_protect (save_excursion_restore, save_excursion_save ());

  Fset_buffer (buffer);
  call0 (function);

  return unbind_to (count, Qnil);
}

DEFUN ("current-buffer", Fcurrent_buffer, Scurrent_buffer, 0, 0, 0,
       doc: /* Return the current buffer as a Lisp object.  */)
  (void)
{
  register Lisp_Object buf;
  XSETBUFFER (buf, current_buffer);
  return buf;
}

/* Set the current buffer to B.

   We previously set windows_or_buffers_changed here to invalidate
   global unchanged information in beg_unchanged and end_unchanged.
   This is no longer necessary because we now compute unchanged
   information on a buffer-basis.  Every action affecting other
   windows than the selected one requires a select_window at some
   time, and that increments windows_or_buffers_changed.  */

void
set_buffer_internal (register struct buffer *b)
{
  if (current_buffer != b)
    set_buffer_internal_1 (b);
}

/* Set the current buffer to B, and do not set windows_or_buffers_changed.
   This is used by redisplay.  */

void
set_buffer_internal_1 (register struct buffer *b)
{
  register struct buffer *old_buf;
  register Lisp_Object tail;

#ifdef USE_MMAP_FOR_BUFFERS
  if (b->text->beg == NULL)
    enlarge_buffer_text (b, 0);
#endif /* USE_MMAP_FOR_BUFFERS */

  if (current_buffer == b)
    return;

  old_buf = current_buffer;
  current_buffer = b;
  last_known_column_point = -1;   /* invalidate indentation cache */

  if (old_buf)
    {
      /* Put the undo list back in the base buffer, so that it appears
	 that an indirect buffer shares the undo list of its base.  */
      if (old_buf->base_buffer)
	BVAR (old_buf->base_buffer, undo_list) = BVAR (old_buf, undo_list);

      /* If the old current buffer has markers to record PT, BEGV and ZV
	 when it is not current, update them now.  */
      record_buffer_markers (old_buf);
    }

  /* Get the undo list from the base buffer, so that it appears
     that an indirect buffer shares the undo list of its base.  */
  if (b->base_buffer)
    BVAR (b, undo_list) = BVAR (b->base_buffer, undo_list);

  /* If the new current buffer has markers to record PT, BEGV and ZV
     when it is not current, fetch them now.  */
  fetch_buffer_markers (b);

  /* Look down buffer's list of local Lisp variables
     to find and update any that forward into C variables. */

  do
    {
      for (tail = BVAR (b, local_var_alist); CONSP (tail); tail = XCDR (tail))
	{
	  Lisp_Object var = XCAR (XCAR (tail));
	  struct Lisp_Symbol *sym = XSYMBOL (var);
	  if (sym->redirect == SYMBOL_LOCALIZED /* Just to be sure.  */
	      && SYMBOL_BLV (sym)->fwd)
	    /* Just reference the variable
	       to cause it to become set for this buffer.  */
	    Fsymbol_value (var);
	}
    }
  /* Do the same with any others that were local to the previous buffer */
  while (b != old_buf && (b = old_buf, b));
}

/* Switch to buffer B temporarily for redisplay purposes.
   This avoids certain things that don't need to be done within redisplay.  */

void
set_buffer_temp (struct buffer *b)
{
  register struct buffer *old_buf;

  if (current_buffer == b)
    return;

  old_buf = current_buffer;
  current_buffer = b;

  /* If the old current buffer has markers to record PT, BEGV and ZV
     when it is not current, update them now.  */
  record_buffer_markers (old_buf);

  /* If the new current buffer has markers to record PT, BEGV and ZV
     when it is not current, fetch them now.  */
  fetch_buffer_markers (b);
}

DEFUN ("set-buffer", Fset_buffer, Sset_buffer, 1, 1, 0,
       doc: /* Make buffer BUFFER-OR-NAME current for editing operations.
BUFFER-OR-NAME may be a buffer or the name of an existing buffer.  See
also `save-excursion' when you want to make a buffer current
temporarily.  This function does not display the buffer, so its effect
ends when the current command terminates.  Use `switch-to-buffer' or
`pop-to-buffer' to switch buffers permanently.  */)
  (register Lisp_Object buffer_or_name)
{
  register Lisp_Object buffer;
  buffer = Fget_buffer (buffer_or_name);
  if (NILP (buffer))
    nsberror (buffer_or_name);
  if (NILP (BVAR (XBUFFER (buffer), name)))
    error ("Selecting deleted buffer");
  set_buffer_internal (XBUFFER (buffer));
  return buffer;
}

/* Set the current buffer to BUFFER provided it is alive.  */

Lisp_Object
set_buffer_if_live (Lisp_Object buffer)
{
  if (! NILP (BVAR (XBUFFER (buffer), name)))
    Fset_buffer (buffer);
  return Qnil;
}

DEFUN ("barf-if-buffer-read-only", Fbarf_if_buffer_read_only,
				   Sbarf_if_buffer_read_only, 0, 0, 0,
       doc: /* Signal a `buffer-read-only' error if the current buffer is read-only.  */)
  (void)
{
  if (!NILP (BVAR (current_buffer, read_only))
      && NILP (Vinhibit_read_only))
    xsignal1 (Qbuffer_read_only, Fcurrent_buffer ());
  return Qnil;
}

DEFUN ("erase-buffer", Ferase_buffer, Serase_buffer, 0, 0, "*",
       doc: /* Delete the entire contents of the current buffer.
Any narrowing restriction in effect (see `narrow-to-region') is removed,
so the buffer is truly empty after this.  */)
  (void)
{
  Fwiden ();

  del_range (BEG, Z);

  current_buffer->last_window_start = 1;
  /* Prevent warnings, or suspension of auto saving, that would happen
     if future size is less than past size.  Use of erase-buffer
     implies that the future text is not really related to the past text.  */
  XSETFASTINT (BVAR (current_buffer, save_length), 0);
  return Qnil;
}

void
validate_region (register Lisp_Object *b, register Lisp_Object *e)
{
  CHECK_NUMBER_COERCE_MARKER (*b);
  CHECK_NUMBER_COERCE_MARKER (*e);

  if (XINT (*b) > XINT (*e))
    {
      Lisp_Object tem;
      tem = *b;  *b = *e;  *e = tem;
    }

  if (!(BEGV <= XINT (*b) && XINT (*b) <= XINT (*e)
        && XINT (*e) <= ZV))
    args_out_of_range (*b, *e);
}

/* Advance BYTE_POS up to a character boundary
   and return the adjusted position.  */

static EMACS_INT
advance_to_char_boundary (EMACS_INT byte_pos)
{
  int c;

  if (byte_pos == BEG)
    /* Beginning of buffer is always a character boundary.  */
    return BEG;

  c = FETCH_BYTE (byte_pos);
  if (! CHAR_HEAD_P (c))
    {
      /* We should advance BYTE_POS only when C is a constituent of a
         multibyte sequence.  */
      EMACS_INT orig_byte_pos = byte_pos;

      do
	{
	  byte_pos--;
	  c = FETCH_BYTE (byte_pos);
	}
      while (! CHAR_HEAD_P (c) && byte_pos > BEG);
      INC_POS (byte_pos);
      if (byte_pos < orig_byte_pos)
	byte_pos = orig_byte_pos;
      /* If C is a constituent of a multibyte sequence, BYTE_POS was
         surely advance to the correct character boundary.  If C is
         not, BYTE_POS was unchanged.  */
    }

  return byte_pos;
}

DEFUN ("buffer-swap-text", Fbuffer_swap_text, Sbuffer_swap_text,
       1, 1, 0,
       doc: /* Swap the text between current buffer and BUFFER.  */)
  (Lisp_Object buffer)
{
  struct buffer *other_buffer;
  CHECK_BUFFER (buffer);
  other_buffer = XBUFFER (buffer);

  if (NILP (BVAR (other_buffer, name)))
    error ("Cannot swap a dead buffer's text");

  /* Actually, it probably works just fine.
   * if (other_buffer == current_buffer)
   *   error ("Cannot swap a buffer's text with itself"); */

  /* Actually, this may be workable as well, tho probably only if they're
     *both* indirect.  */
  if (other_buffer->base_buffer
      || current_buffer->base_buffer)
    error ("Cannot swap indirect buffers's text");

  { /* This is probably harder to make work.  */
    struct buffer *other;
    for (other = all_buffers; other; other = other->header.next.buffer)
      if (other->base_buffer == other_buffer
	  || other->base_buffer == current_buffer)
	error ("One of the buffers to swap has indirect buffers");
  }

#define swapfield(field, type) \
  do {							\
    type tmp##field = other_buffer->field;		\
    other_buffer->field = current_buffer->field;	\
    current_buffer->field = tmp##field;			\
  } while (0)
#define swapfield_(field, type) \
  do {							\
    type tmp##field = BVAR (other_buffer, field);		\
    BVAR (other_buffer, field) = BVAR (current_buffer, field);	\
    BVAR (current_buffer, field) = tmp##field;			\
  } while (0)

  swapfield (own_text, struct buffer_text);
  eassert (current_buffer->text == &current_buffer->own_text);
  eassert (other_buffer->text == &other_buffer->own_text);
#ifdef REL_ALLOC
  r_alloc_reset_variable ((POINTER_TYPE **) &current_buffer->own_text.beg,
			  (POINTER_TYPE **) &other_buffer->own_text.beg);
  r_alloc_reset_variable ((POINTER_TYPE **) &other_buffer->own_text.beg,
			  (POINTER_TYPE **) &current_buffer->own_text.beg);
#endif /* REL_ALLOC */

  swapfield (pt, EMACS_INT);
  swapfield (pt_byte, EMACS_INT);
  swapfield (begv, EMACS_INT);
  swapfield (begv_byte, EMACS_INT);
  swapfield (zv, EMACS_INT);
  swapfield (zv_byte, EMACS_INT);
  eassert (!current_buffer->base_buffer);
  eassert (!other_buffer->base_buffer);
  current_buffer->clip_changed = 1;	other_buffer->clip_changed = 1;
  swapfield (newline_cache, struct region_cache *);
  swapfield (width_run_cache, struct region_cache *);
  current_buffer->prevent_redisplay_optimizations_p = 1;
  other_buffer->prevent_redisplay_optimizations_p = 1;
  swapfield (overlays_before, struct Lisp_Overlay *);
  swapfield (overlays_after, struct Lisp_Overlay *);
  swapfield (overlay_center, EMACS_INT);
  swapfield_ (undo_list, Lisp_Object);
  swapfield_ (mark, Lisp_Object);
  swapfield_ (enable_multibyte_characters, Lisp_Object);
  swapfield_ (bidi_display_reordering, Lisp_Object);
  swapfield_ (bidi_paragraph_direction, Lisp_Object);
  /* FIXME: Not sure what we should do with these *_marker fields.
     Hopefully they're just nil anyway.  */
  swapfield_ (pt_marker, Lisp_Object);
  swapfield_ (begv_marker, Lisp_Object);
  swapfield_ (zv_marker, Lisp_Object);
  BVAR (current_buffer, point_before_scroll) = Qnil;
  BVAR (other_buffer, point_before_scroll) = Qnil;

  current_buffer->text->modiff++;	  other_buffer->text->modiff++;
  current_buffer->text->chars_modiff++;	  other_buffer->text->chars_modiff++;
  current_buffer->text->overlay_modiff++; other_buffer->text->overlay_modiff++;
  current_buffer->text->beg_unchanged = current_buffer->text->gpt;
  current_buffer->text->end_unchanged = current_buffer->text->gpt;
  other_buffer->text->beg_unchanged = other_buffer->text->gpt;
  other_buffer->text->end_unchanged = other_buffer->text->gpt;
  {
    struct Lisp_Marker *m;
    for (m = BUF_MARKERS (current_buffer); m; m = m->next)
      if (m->buffer == other_buffer)
	m->buffer = current_buffer;
      else
	/* Since there's no indirect buffer in sight, markers on
	   BUF_MARKERS(buf) should either be for `buf' or dead.  */
	eassert (!m->buffer);
    for (m = BUF_MARKERS (other_buffer); m; m = m->next)
      if (m->buffer == current_buffer)
	m->buffer = other_buffer;
      else
	/* Since there's no indirect buffer in sight, markers on
	   BUF_MARKERS(buf) should either be for `buf' or dead.  */
	eassert (!m->buffer);
  }
  { /* Some of the C code expects that w->buffer == w->pointm->buffer.
       So since we just swapped the markers between the two buffers, we need
       to undo the effect of this swap for window markers.  */
    Lisp_Object w = Fselected_window (), ws = Qnil;
    Lisp_Object buf1, buf2;
    XSETBUFFER (buf1, current_buffer); XSETBUFFER (buf2, other_buffer);

    while (NILP (Fmemq (w, ws)))
      {
	ws = Fcons (w, ws);
	if (MARKERP (XWINDOW (w)->pointm)
	    && (EQ (XWINDOW (w)->buffer, buf1)
		|| EQ (XWINDOW (w)->buffer, buf2)))
	  Fset_marker (XWINDOW (w)->pointm,
		       make_number (BUF_BEGV (XBUFFER (XWINDOW (w)->buffer))),
		       XWINDOW (w)->buffer);
	w = Fnext_window (w, Qt, Qt);
      }
  }

  if (current_buffer->text->intervals)
    (eassert (EQ (current_buffer->text->intervals->up.obj, buffer)),
     XSETBUFFER (current_buffer->text->intervals->up.obj, current_buffer));
  if (other_buffer->text->intervals)
    (eassert (EQ (other_buffer->text->intervals->up.obj, Fcurrent_buffer ())),
     XSETBUFFER (other_buffer->text->intervals->up.obj, other_buffer));

  return Qnil;
}

DEFUN ("set-buffer-multibyte", Fset_buffer_multibyte, Sset_buffer_multibyte,
       1, 1, 0,
       doc: /* Set the multibyte flag of the current buffer to FLAG.
If FLAG is t, this makes the buffer a multibyte buffer.
If FLAG is nil, this makes the buffer a single-byte buffer.
In these cases, the buffer contents remain unchanged as a sequence of
bytes but the contents viewed as characters do change.
If FLAG is `to', this makes the buffer a multibyte buffer by changing
all eight-bit bytes to eight-bit characters.
If the multibyte flag was really changed, undo information of the
current buffer is cleared.  */)
  (Lisp_Object flag)
{
  struct Lisp_Marker *tail, *markers;
  struct buffer *other;
  EMACS_INT begv, zv;
  int narrowed = (BEG != BEGV || Z != ZV);
  int modified_p = !NILP (Fbuffer_modified_p (Qnil));
  Lisp_Object old_undo = BVAR (current_buffer, undo_list);
  struct gcpro gcpro1;

  if (current_buffer->base_buffer)
    error ("Cannot do `set-buffer-multibyte' on an indirect buffer");

  /* Do nothing if nothing actually changes.  */
  if (NILP (flag) == NILP (BVAR (current_buffer, enable_multibyte_characters)))
    return flag;

  GCPRO1 (old_undo);

  /* Don't record these buffer changes.  We will put a special undo entry
     instead.  */
  BVAR (current_buffer, undo_list) = Qt;

  /* If the cached position is for this buffer, clear it out.  */
  clear_charpos_cache (current_buffer);

  if (NILP (flag))
    begv = BEGV_BYTE, zv = ZV_BYTE;
  else
    begv = BEGV, zv = ZV;

  if (narrowed)
    Fwiden ();

  if (NILP (flag))
    {
      EMACS_INT pos, stop;
      unsigned char *p;

      /* Do this first, so it can use CHAR_TO_BYTE
	 to calculate the old correspondences.  */
      set_intervals_multibyte (0);

      BVAR (current_buffer, enable_multibyte_characters) = Qnil;

      Z = Z_BYTE;
      BEGV = BEGV_BYTE;
      ZV = ZV_BYTE;
      GPT = GPT_BYTE;
      TEMP_SET_PT_BOTH (PT_BYTE, PT_BYTE);


      for (tail = BUF_MARKERS (current_buffer); tail; tail = tail->next)
	tail->charpos = tail->bytepos;

      /* Convert multibyte form of 8-bit characters to unibyte.  */
      pos = BEG;
      stop = GPT;
      p = BEG_ADDR;
      while (1)
	{
	  int c, bytes;

	  if (pos == stop)
	    {
	      if (pos == Z)
		break;
	      p = GAP_END_ADDR;
	      stop = Z;
	    }
	  if (ASCII_BYTE_P (*p))
	    p++, pos++;
	  else if (CHAR_BYTE8_HEAD_P (*p))
	    {
	      c = STRING_CHAR_AND_LENGTH (p, bytes);
	      /* Delete all bytes for this 8-bit character but the
		 last one, and change the last one to the character
		 code.  */
	      bytes--;
	      del_range_2 (pos, pos, pos + bytes, pos + bytes, 0);
	      p = GAP_END_ADDR;
	      *p++ = c;
	      pos++;
	      if (begv > pos)
		begv -= bytes;
	      if (zv > pos)
		zv -= bytes;
	      stop = Z;
	    }
	  else
	    {
	      bytes = BYTES_BY_CHAR_HEAD (*p);
	      p += bytes, pos += bytes;
	    }
	}
      if (narrowed)
	Fnarrow_to_region (make_number (begv), make_number (zv));
    }
  else
    {
      EMACS_INT pt = PT;
      EMACS_INT pos, stop;
      unsigned char *p, *pend;

      /* Be sure not to have a multibyte sequence striding over the GAP.
	 Ex: We change this: "...abc\302 _GAP_ \241def..."
	     to: "...abc _GAP_ \302\241def..."  */

      if (EQ (flag, Qt)
	  && GPT_BYTE > 1 && GPT_BYTE < Z_BYTE
	  && ! CHAR_HEAD_P (*(GAP_END_ADDR)))
	{
	  unsigned char *q = GPT_ADDR - 1;

	  while (! CHAR_HEAD_P (*q) && q > BEG_ADDR) q--;
	  if (LEADING_CODE_P (*q))
	    {
	      EMACS_INT new_gpt = GPT_BYTE - (GPT_ADDR - q);

	      move_gap_both (new_gpt, new_gpt);
	    }
	}

      /* Make the buffer contents valid as multibyte by converting
	 8-bit characters to multibyte form.  */
      pos = BEG;
      stop = GPT;
      p = BEG_ADDR;
      pend = GPT_ADDR;
      while (1)
	{
	  int bytes;

	  if (pos == stop)
	    {
	      if (pos == Z)
		break;
	      p = GAP_END_ADDR;
	      pend = Z_ADDR;
	      stop = Z;
	    }

	  if (ASCII_BYTE_P (*p))
	    p++, pos++;
	  else if (EQ (flag, Qt)
		   && ! CHAR_BYTE8_HEAD_P (*p)
		   && (bytes = MULTIBYTE_LENGTH (p, pend)) > 0)
	    p += bytes, pos += bytes;
	  else
	    {
	      unsigned char tmp[MAX_MULTIBYTE_LENGTH];
	      int c;

	      c = BYTE8_TO_CHAR (*p);
	      bytes = CHAR_STRING (c, tmp);
	      *p = tmp[0];
	      TEMP_SET_PT_BOTH (pos + 1, pos + 1);
	      bytes--;
	      insert_1_both ((char *) tmp + 1, bytes, bytes, 1, 0, 0);
	      /* Now the gap is after the just inserted data.  */
	      pos = GPT;
	      p = GAP_END_ADDR;
	      if (pos <= begv)
		begv += bytes;
	      if (pos <= zv)
		zv += bytes;
	      if (pos <= pt)
		pt += bytes;
	      pend = Z_ADDR;
	      stop = Z;
	    }
	}

      if (pt != PT)
	TEMP_SET_PT (pt);

      if (narrowed)
	Fnarrow_to_region (make_number (begv), make_number (zv));

      /* Do this first, so that chars_in_text asks the right question.
	 set_intervals_multibyte needs it too.  */
      BVAR (current_buffer, enable_multibyte_characters) = Qt;

      GPT_BYTE = advance_to_char_boundary (GPT_BYTE);
      GPT = chars_in_text (BEG_ADDR, GPT_BYTE - BEG_BYTE) + BEG;

      Z = chars_in_text (GAP_END_ADDR, Z_BYTE - GPT_BYTE) + GPT;

      BEGV_BYTE = advance_to_char_boundary (BEGV_BYTE);
      if (BEGV_BYTE > GPT_BYTE)
	BEGV = chars_in_text (GAP_END_ADDR, BEGV_BYTE - GPT_BYTE) + GPT;
      else
	BEGV = chars_in_text (BEG_ADDR, BEGV_BYTE - BEG_BYTE) + BEG;

      ZV_BYTE = advance_to_char_boundary (ZV_BYTE);
      if (ZV_BYTE > GPT_BYTE)
	ZV = chars_in_text (GAP_END_ADDR, ZV_BYTE - GPT_BYTE) + GPT;
      else
	ZV = chars_in_text (BEG_ADDR, ZV_BYTE - BEG_BYTE) + BEG;

      {
	EMACS_INT byte = advance_to_char_boundary (PT_BYTE);
	EMACS_INT position;

	if (byte > GPT_BYTE)
	  position = chars_in_text (GAP_END_ADDR, byte - GPT_BYTE) + GPT;
	else
	  position = chars_in_text (BEG_ADDR, byte - BEG_BYTE) + BEG;
	TEMP_SET_PT_BOTH (position, byte);
      }

      tail = markers = BUF_MARKERS (current_buffer);

      /* This prevents BYTE_TO_CHAR (that is, buf_bytepos_to_charpos) from
	 getting confused by the markers that have not yet been updated.
	 It is also a signal that it should never create a marker.  */
      BUF_MARKERS (current_buffer) = NULL;

      for (; tail; tail = tail->next)
	{
	  tail->bytepos = advance_to_char_boundary (tail->bytepos);
	  tail->charpos = BYTE_TO_CHAR (tail->bytepos);
	}

      /* Make sure no markers were put on the chain
	 while the chain value was incorrect.  */
      if (BUF_MARKERS (current_buffer))
	abort ();

      BUF_MARKERS (current_buffer) = markers;

      /* Do this last, so it can calculate the new correspondences
	 between chars and bytes.  */
      set_intervals_multibyte (1);
    }

  if (!EQ (old_undo, Qt))
    {
      /* Represent all the above changes by a special undo entry.  */
      BVAR (current_buffer, undo_list) = Fcons (list3 (Qapply,
						intern ("set-buffer-multibyte"),
						NILP (flag) ? Qt : Qnil),
					 old_undo);
    }

  UNGCPRO;

  /* Changing the multibyteness of a buffer means that all windows
     showing that buffer must be updated thoroughly.  */
  current_buffer->prevent_redisplay_optimizations_p = 1;
  ++windows_or_buffers_changed;

  /* Copy this buffer's new multibyte status
     into all of its indirect buffers.  */
  for (other = all_buffers; other; other = other->header.next.buffer)
    if (other->base_buffer == current_buffer && !NILP (BVAR (other, name)))
      {
	BVAR (other, enable_multibyte_characters)
	  = BVAR (current_buffer, enable_multibyte_characters);
	other->prevent_redisplay_optimizations_p = 1;
      }

  /* Restore the modifiedness of the buffer.  */
  if (!modified_p && !NILP (Fbuffer_modified_p (Qnil)))
    Fset_buffer_modified_p (Qnil);

  /* Update coding systems of this buffer's process (if any).  */
  {
    Lisp_Object process;

    process = Fget_buffer_process (Fcurrent_buffer ());
    if (PROCESSP (process))
      setup_process_coding_systems (process);
  }

  return flag;
}

DEFUN ("kill-all-local-variables", Fkill_all_local_variables,
       Skill_all_local_variables, 0, 0, 0,
       doc: /* Switch to Fundamental mode by killing current buffer's local variables.
Most local variable bindings are eliminated so that the default values
become effective once more.  Also, the syntax table is set from
`standard-syntax-table', the local keymap is set to nil,
and the abbrev table from `fundamental-mode-abbrev-table'.
This function also forces redisplay of the mode line.

Every function to select a new major mode starts by
calling this function.

As a special exception, local variables whose names have
a non-nil `permanent-local' property are not eliminated by this function.

The first thing this function does is run
the normal hook `change-major-mode-hook'.  */)
  (void)
{
  Frun_hooks (1, &Qchange_major_mode_hook);

  /* Make sure none of the bindings in local_var_alist
     remain swapped in, in their symbols.  */

  swap_out_buffer_local_variables (current_buffer);

  /* Actually eliminate all local bindings of this buffer.  */

  reset_buffer_local_variables (current_buffer, 0);

  /* Force mode-line redisplay.  Useful here because all major mode
     commands call this function.  */
  update_mode_lines++;

  return Qnil;
}

/* Make sure no local variables remain set up with buffer B
   for their current values.  */

static void
swap_out_buffer_local_variables (struct buffer *b)
{
  Lisp_Object oalist, alist, buffer;

  XSETBUFFER (buffer, b);
  oalist = BVAR (b, local_var_alist);

  for (alist = oalist; CONSP (alist); alist = XCDR (alist))
    {
      Lisp_Object sym = XCAR (XCAR (alist));
      eassert (XSYMBOL (sym)->redirect == SYMBOL_LOCALIZED);
      /* Need not do anything if some other buffer's binding is
	 now cached.  */
      if (EQ (SYMBOL_BLV (XSYMBOL (sym))->where, buffer))
	{
	  /* Symbol is set up for this buffer's old local value:
	     swap it out!  */
	  swap_in_global_binding (XSYMBOL (sym));
	}
    }
}

/* Find all the overlays in the current buffer that contain position POS.
   Return the number found, and store them in a vector in *VEC_PTR.
   Store in *LEN_PTR the size allocated for the vector.
   Store in *NEXT_PTR the next position after POS where an overlay starts,
     or ZV if there are no more overlays between POS and ZV.
   Store in *PREV_PTR the previous position before POS where an overlay ends,
     or where an overlay starts which ends at or after POS;
     or BEGV if there are no such overlays from BEGV to POS.
   NEXT_PTR and/or PREV_PTR may be 0, meaning don't store that info.

   *VEC_PTR and *LEN_PTR should contain a valid vector and size
   when this function is called.

   If EXTEND is non-zero, we make the vector bigger if necessary.
   If EXTEND is zero, we never extend the vector,
   and we store only as many overlays as will fit.
   But we still return the total number of overlays.

   If CHANGE_REQ is true, then any position written into *PREV_PTR or
   *NEXT_PTR is guaranteed to be not equal to POS, unless it is the
   default (BEGV or ZV).  */

ptrdiff_t
overlays_at (EMACS_INT pos, int extend, Lisp_Object **vec_ptr,
	     ptrdiff_t *len_ptr,
	     EMACS_INT *next_ptr, EMACS_INT *prev_ptr, int change_req)
{
  Lisp_Object overlay, start, end;
  struct Lisp_Overlay *tail;
  ptrdiff_t idx = 0;
  ptrdiff_t len = *len_ptr;
  Lisp_Object *vec = *vec_ptr;
  EMACS_INT next = ZV;
  EMACS_INT prev = BEGV;
  int inhibit_storing = 0;

  for (tail = current_buffer->overlays_before; tail; tail = tail->next)
    {
      EMACS_INT startpos, endpos;

      XSETMISC (overlay, tail);

      start = OVERLAY_START (overlay);
      end = OVERLAY_END (overlay);
      endpos = OVERLAY_POSITION (end);
      if (endpos < pos)
	{
	  if (prev < endpos)
	    prev = endpos;
	  break;
	}
      startpos = OVERLAY_POSITION (start);
      /* This one ends at or after POS
	 so its start counts for PREV_PTR if it's before POS.  */
      if (prev < startpos && startpos < pos)
	prev = startpos;
      if (endpos == pos)
	continue;
      if (startpos <= pos)
	{
	  if (idx == len)
	    {
	      /* The supplied vector is full.
		 Either make it bigger, or don't store any more in it.  */
	      if (extend)
		{
		  vec = xpalloc (vec, len_ptr, 1, OVERLAY_COUNT_MAX,
				 sizeof *vec);
		  *vec_ptr = vec;
		  len = *len_ptr;
		}
	      else
		inhibit_storing = 1;
	    }

	  if (!inhibit_storing)
	    vec[idx] = overlay;
	  /* Keep counting overlays even if we can't return them all.  */
	  idx++;
	}
      else if (startpos < next)
	next = startpos;
    }

  for (tail = current_buffer->overlays_after; tail; tail = tail->next)
    {
      EMACS_INT startpos, endpos;

      XSETMISC (overlay, tail);

      start = OVERLAY_START (overlay);
      end = OVERLAY_END (overlay);
      startpos = OVERLAY_POSITION (start);
      if (pos < startpos)
	{
	  if (startpos < next)
	    next = startpos;
	  break;
	}
      endpos = OVERLAY_POSITION (end);
      if (pos < endpos)
	{
	  if (idx == len)
	    {
	      if (extend)
		{
		  vec = xpalloc (vec, len_ptr, 1, OVERLAY_COUNT_MAX,
				 sizeof *vec);
		  *vec_ptr = vec;
		  len = *len_ptr;
		}
	      else
		inhibit_storing = 1;
	    }

	  if (!inhibit_storing)
	    vec[idx] = overlay;
	  idx++;

	  if (startpos < pos && startpos > prev)
	    prev = startpos;
	}
      else if (endpos < pos && endpos > prev)
	prev = endpos;
      else if (endpos == pos && startpos > prev
	       && (!change_req || startpos < pos))
	prev = startpos;
    }

  if (next_ptr)
    *next_ptr = next;
  if (prev_ptr)
    *prev_ptr = prev;
  return idx;
}

/* Find all the overlays in the current buffer that overlap the range
   BEG-END, or are empty at BEG, or are empty at END provided END
   denotes the position at the end of the current buffer.

   Return the number found, and store them in a vector in *VEC_PTR.
   Store in *LEN_PTR the size allocated for the vector.
   Store in *NEXT_PTR the next position after POS where an overlay starts,
     or ZV if there are no more overlays.
   Store in *PREV_PTR the previous position before POS where an overlay ends,
     or BEGV if there are no previous overlays.
   NEXT_PTR and/or PREV_PTR may be 0, meaning don't store that info.

   *VEC_PTR and *LEN_PTR should contain a valid vector and size
   when this function is called.

   If EXTEND is non-zero, we make the vector bigger if necessary.
   If EXTEND is zero, we never extend the vector,
   and we store only as many overlays as will fit.
   But we still return the total number of overlays.  */

static ptrdiff_t
overlays_in (EMACS_INT beg, EMACS_INT end, int extend,
	     Lisp_Object **vec_ptr, ptrdiff_t *len_ptr,
	     EMACS_INT *next_ptr, EMACS_INT *prev_ptr)
{
  Lisp_Object overlay, ostart, oend;
  struct Lisp_Overlay *tail;
  ptrdiff_t idx = 0;
  ptrdiff_t len = *len_ptr;
  Lisp_Object *vec = *vec_ptr;
  EMACS_INT next = ZV;
  EMACS_INT prev = BEGV;
  int inhibit_storing = 0;
  int end_is_Z = end == Z;

  for (tail = current_buffer->overlays_before; tail; tail = tail->next)
    {
      EMACS_INT startpos, endpos;

      XSETMISC (overlay, tail);

      ostart = OVERLAY_START (overlay);
      oend = OVERLAY_END (overlay);
      endpos = OVERLAY_POSITION (oend);
      if (endpos < beg)
	{
	  if (prev < endpos)
	    prev = endpos;
	  break;
	}
      startpos = OVERLAY_POSITION (ostart);
      /* Count an interval if it overlaps the range, is empty at the
	 start of the range, or is empty at END provided END denotes the
	 end of the buffer.  */
      if ((beg < endpos && startpos < end)
	  || (startpos == endpos
	      && (beg == endpos || (end_is_Z && endpos == end))))
	{
	  if (idx == len)
	    {
	      /* The supplied vector is full.
		 Either make it bigger, or don't store any more in it.  */
	      if (extend)
		{
		  vec = xpalloc (vec, len_ptr, 1, OVERLAY_COUNT_MAX,
				 sizeof *vec);
		  *vec_ptr = vec;
		  len = *len_ptr;
		}
	      else
		inhibit_storing = 1;
	    }

	  if (!inhibit_storing)
	    vec[idx] = overlay;
	  /* Keep counting overlays even if we can't return them all.  */
	  idx++;
	}
      else if (startpos < next)
	next = startpos;
    }

  for (tail = current_buffer->overlays_after; tail; tail = tail->next)
    {
      EMACS_INT startpos, endpos;

      XSETMISC (overlay, tail);

      ostart = OVERLAY_START (overlay);
      oend = OVERLAY_END (overlay);
      startpos = OVERLAY_POSITION (ostart);
      if (end < startpos)
	{
	  if (startpos < next)
	    next = startpos;
	  break;
	}
      endpos = OVERLAY_POSITION (oend);
      /* Count an interval if it overlaps the range, is empty at the
	 start of the range, or is empty at END provided END denotes the
	 end of the buffer.  */
      if ((beg < endpos && startpos < end)
	  || (startpos == endpos
	      && (beg == endpos || (end_is_Z && endpos == end))))
	{
	  if (idx == len)
	    {
	      if (extend)
		{
		  vec = xpalloc (vec, len_ptr, 1, OVERLAY_COUNT_MAX,
				 sizeof *vec);
		  *vec_ptr = vec;
		  len = *len_ptr;
		}
	      else
		inhibit_storing = 1;
	    }

	  if (!inhibit_storing)
	    vec[idx] = overlay;
	  idx++;
	}
      else if (endpos < beg && endpos > prev)
	prev = endpos;
    }

  if (next_ptr)
    *next_ptr = next;
  if (prev_ptr)
    *prev_ptr = prev;
  return idx;
}


/* Return non-zero if there exists an overlay with a non-nil
   `mouse-face' property overlapping OVERLAY.  */

int
mouse_face_overlay_overlaps (Lisp_Object overlay)
{
  EMACS_INT start = OVERLAY_POSITION (OVERLAY_START (overlay));
  EMACS_INT end = OVERLAY_POSITION (OVERLAY_END (overlay));
  ptrdiff_t n, i, size;
  Lisp_Object *v, tem;

  size = 10;
  v = (Lisp_Object *) alloca (size * sizeof *v);
  n = overlays_in (start, end, 0, &v, &size, NULL, NULL);
  if (n > size)
    {
      v = (Lisp_Object *) alloca (n * sizeof *v);
      overlays_in (start, end, 0, &v, &n, NULL, NULL);
    }

  for (i = 0; i < n; ++i)
    if (!EQ (v[i], overlay)
	&& (tem = Foverlay_get (overlay, Qmouse_face),
	    !NILP (tem)))
      break;

  return i < n;
}



/* Fast function to just test if we're at an overlay boundary.  */
int
overlay_touches_p (EMACS_INT pos)
{
  Lisp_Object overlay;
  struct Lisp_Overlay *tail;

  for (tail = current_buffer->overlays_before; tail; tail = tail->next)
    {
      EMACS_INT endpos;

      XSETMISC (overlay ,tail);
      if (!OVERLAYP (overlay))
	abort ();

      endpos = OVERLAY_POSITION (OVERLAY_END (overlay));
      if (endpos < pos)
	break;
      if (endpos == pos || OVERLAY_POSITION (OVERLAY_START (overlay)) == pos)
	return 1;
    }

  for (tail = current_buffer->overlays_after; tail; tail = tail->next)
    {
      EMACS_INT startpos;

      XSETMISC (overlay, tail);
      if (!OVERLAYP (overlay))
	abort ();

      startpos = OVERLAY_POSITION (OVERLAY_START (overlay));
      if (pos < startpos)
	break;
      if (startpos == pos || OVERLAY_POSITION (OVERLAY_END (overlay)) == pos)
	return 1;
    }
  return 0;
}

struct sortvec
{
  Lisp_Object overlay;
  EMACS_INT beg, end;
  EMACS_INT priority;
};

static int
compare_overlays (const void *v1, const void *v2)
{
  const struct sortvec *s1 = (const struct sortvec *) v1;
  const struct sortvec *s2 = (const struct sortvec *) v2;
  if (s1->priority != s2->priority)
    return s1->priority < s2->priority ? -1 : 1;
  if (s1->beg != s2->beg)
    return s1->beg < s2->beg ? -1 : 1;
  if (s1->end != s2->end)
    return s2->end < s1->end ? -1 : 1;
  /* Avoid the non-determinism of qsort by choosing an arbitrary ordering
     between "equal" overlays.  The result can still change between
     invocations of Emacs, but it won't change in the middle of
     `find_field' (bug#6830).  */
  if (XHASH (s1->overlay) != XHASH (s2->overlay))
    return XHASH (s1->overlay) < XHASH (s2->overlay) ? -1 : 1;
  return 0;
}

/* Sort an array of overlays by priority.  The array is modified in place.
   The return value is the new size; this may be smaller than the original
   size if some of the overlays were invalid or were window-specific.  */
ptrdiff_t
sort_overlays (Lisp_Object *overlay_vec, ptrdiff_t noverlays, struct window *w)
{
  ptrdiff_t i, j;
  struct sortvec *sortvec;
  sortvec = (struct sortvec *) alloca (noverlays * sizeof (struct sortvec));

  /* Put the valid and relevant overlays into sortvec.  */

  for (i = 0, j = 0; i < noverlays; i++)
    {
      Lisp_Object tem;
      Lisp_Object overlay;

      overlay = overlay_vec[i];
      if (OVERLAY_VALID (overlay)
	  && OVERLAY_POSITION (OVERLAY_START (overlay)) > 0
	  && OVERLAY_POSITION (OVERLAY_END (overlay)) > 0)
	{
	  /* If we're interested in a specific window, then ignore
	     overlays that are limited to some other window.  */
	  if (w)
	    {
	      Lisp_Object window;

	      window = Foverlay_get (overlay, Qwindow);
	      if (WINDOWP (window) && XWINDOW (window) != w)
		continue;
	    }

	  /* This overlay is good and counts: put it into sortvec.  */
	  sortvec[j].overlay = overlay;
	  sortvec[j].beg = OVERLAY_POSITION (OVERLAY_START (overlay));
	  sortvec[j].end = OVERLAY_POSITION (OVERLAY_END (overlay));
	  tem = Foverlay_get (overlay, Qpriority);
	  if (INTEGERP (tem))
	    sortvec[j].priority = XINT (tem);
	  else
	    sortvec[j].priority = 0;
	  j++;
	}
    }
  noverlays = j;

  /* Sort the overlays into the proper order: increasing priority.  */

  if (noverlays > 1)
    qsort (sortvec, noverlays, sizeof (struct sortvec), compare_overlays);

  for (i = 0; i < noverlays; i++)
    overlay_vec[i] = sortvec[i].overlay;
  return (noverlays);
}

struct sortstr
{
  Lisp_Object string, string2;
  ptrdiff_t size;
  EMACS_INT priority;
};

struct sortstrlist
{
  struct sortstr *buf;	/* An array that expands as needed; never freed.  */
  ptrdiff_t size;	/* Allocated length of that array.  */
  ptrdiff_t used;	/* How much of the array is currently in use.  */
  ptrdiff_t bytes;	/* Total length of the strings in buf.  */
};

/* Buffers for storing information about the overlays touching a given
   position.  These could be automatic variables in overlay_strings, but
   it's more efficient to hold onto the memory instead of repeatedly
   allocating and freeing it.  */
static struct sortstrlist overlay_heads, overlay_tails;
static unsigned char *overlay_str_buf;

/* Allocated length of overlay_str_buf.  */
static ptrdiff_t overlay_str_len;

/* A comparison function suitable for passing to qsort.  */
static int
cmp_for_strings (const void *as1, const void *as2)
{
  struct sortstr *s1 = (struct sortstr *)as1;
  struct sortstr *s2 = (struct sortstr *)as2;
  if (s1->size != s2->size)
    return s2->size < s1->size ? -1 : 1;
  if (s1->priority != s2->priority)
    return s1->priority < s2->priority ? -1 : 1;
  return 0;
}

static void
record_overlay_string (struct sortstrlist *ssl, Lisp_Object str,
		       Lisp_Object str2, Lisp_Object pri, ptrdiff_t size)
{
  EMACS_INT nbytes;

  if (ssl->used == ssl->size)
    ssl->buf = xpalloc (ssl->buf, &ssl->size, 5, -1, sizeof *ssl->buf);
  ssl->buf[ssl->used].string = str;
  ssl->buf[ssl->used].string2 = str2;
  ssl->buf[ssl->used].size = size;
  ssl->buf[ssl->used].priority = (INTEGERP (pri) ? XINT (pri) : 0);
  ssl->used++;

  if (NILP (BVAR (current_buffer, enable_multibyte_characters)))
    nbytes = SCHARS (str);
  else if (! STRING_MULTIBYTE (str))
    nbytes = count_size_as_multibyte (SDATA (str),
				      SBYTES (str));
  else
    nbytes = SBYTES (str);

  if (INT_ADD_OVERFLOW (ssl->bytes, nbytes))
    memory_full (SIZE_MAX);
  ssl->bytes += nbytes;

  if (STRINGP (str2))
    {
      if (NILP (BVAR (current_buffer, enable_multibyte_characters)))
	nbytes = SCHARS (str2);
      else if (! STRING_MULTIBYTE (str2))
	nbytes = count_size_as_multibyte (SDATA (str2),
					  SBYTES (str2));
      else
	nbytes = SBYTES (str2);

      if (INT_ADD_OVERFLOW (ssl->bytes, nbytes))
	memory_full (SIZE_MAX);
      ssl->bytes += nbytes;
    }
}

/* Return the concatenation of the strings associated with overlays that
   begin or end at POS, ignoring overlays that are specific to a window
   other than W.  The strings are concatenated in the appropriate order:
   shorter overlays nest inside longer ones, and higher priority inside
   lower.  Normally all of the after-strings come first, but zero-sized
   overlays have their after-strings ride along with the before-strings
   because it would look strange to print them inside-out.

   Returns the string length, and stores the contents indirectly through
   PSTR, if that variable is non-null.  The string may be overwritten by
   subsequent calls.  */

EMACS_INT
overlay_strings (EMACS_INT pos, struct window *w, unsigned char **pstr)
{
  Lisp_Object overlay, window, str;
  struct Lisp_Overlay *ov;
  EMACS_INT startpos, endpos;
  int multibyte = ! NILP (BVAR (current_buffer, enable_multibyte_characters));

  overlay_heads.used = overlay_heads.bytes = 0;
  overlay_tails.used = overlay_tails.bytes = 0;
  for (ov = current_buffer->overlays_before; ov; ov = ov->next)
    {
      XSETMISC (overlay, ov);
      eassert (OVERLAYP (overlay));

      startpos = OVERLAY_POSITION (OVERLAY_START (overlay));
      endpos = OVERLAY_POSITION (OVERLAY_END (overlay));
      if (endpos < pos)
	break;
      if (endpos != pos && startpos != pos)
	continue;
      window = Foverlay_get (overlay, Qwindow);
      if (WINDOWP (window) && XWINDOW (window) != w)
	continue;
      if (startpos == pos
	  && (str = Foverlay_get (overlay, Qbefore_string), STRINGP (str)))
	record_overlay_string (&overlay_heads, str,
			       (startpos == endpos
				? Foverlay_get (overlay, Qafter_string)
				: Qnil),
			       Foverlay_get (overlay, Qpriority),
			       endpos - startpos);
      else if (endpos == pos
	  && (str = Foverlay_get (overlay, Qafter_string), STRINGP (str)))
	record_overlay_string (&overlay_tails, str, Qnil,
			       Foverlay_get (overlay, Qpriority),
			       endpos - startpos);
    }
  for (ov = current_buffer->overlays_after; ov; ov = ov->next)
    {
      XSETMISC (overlay, ov);
      eassert (OVERLAYP (overlay));

      startpos = OVERLAY_POSITION (OVERLAY_START (overlay));
      endpos = OVERLAY_POSITION (OVERLAY_END (overlay));
      if (startpos > pos)
	break;
      if (endpos != pos && startpos != pos)
	continue;
      window = Foverlay_get (overlay, Qwindow);
      if (WINDOWP (window) && XWINDOW (window) != w)
	continue;
      if (startpos == pos
	  && (str = Foverlay_get (overlay, Qbefore_string), STRINGP (str)))
	record_overlay_string (&overlay_heads, str,
			       (startpos == endpos
				? Foverlay_get (overlay, Qafter_string)
				: Qnil),
			       Foverlay_get (overlay, Qpriority),
			       endpos - startpos);
      else if (endpos == pos
	       && (str = Foverlay_get (overlay, Qafter_string), STRINGP (str)))
	record_overlay_string (&overlay_tails, str, Qnil,
			       Foverlay_get (overlay, Qpriority),
			       endpos - startpos);
    }
  if (overlay_tails.used > 1)
    qsort (overlay_tails.buf, overlay_tails.used, sizeof (struct sortstr),
	   cmp_for_strings);
  if (overlay_heads.used > 1)
    qsort (overlay_heads.buf, overlay_heads.used, sizeof (struct sortstr),
	   cmp_for_strings);
  if (overlay_heads.bytes || overlay_tails.bytes)
    {
      Lisp_Object tem;
      EMACS_INT i;
      unsigned char *p;
      ptrdiff_t total;

      if (INT_ADD_OVERFLOW (overlay_heads.bytes, overlay_tails.bytes))
	memory_full (SIZE_MAX);
      total = overlay_heads.bytes + overlay_tails.bytes;
      if (total > overlay_str_len)
	overlay_str_buf = xpalloc (overlay_str_buf, &overlay_str_len,
				   total - overlay_str_len, -1, 1);

      p = overlay_str_buf;
      for (i = overlay_tails.used; --i >= 0;)
	{
	  EMACS_INT nbytes;
	  tem = overlay_tails.buf[i].string;
	  nbytes = copy_text (SDATA (tem), p,
			      SBYTES (tem),
			      STRING_MULTIBYTE (tem), multibyte);
	  p += nbytes;
	}
      for (i = 0; i < overlay_heads.used; ++i)
	{
	  EMACS_INT nbytes;
	  tem = overlay_heads.buf[i].string;
	  nbytes = copy_text (SDATA (tem), p,
			      SBYTES (tem),
			      STRING_MULTIBYTE (tem), multibyte);
	  p += nbytes;
	  tem = overlay_heads.buf[i].string2;
	  if (STRINGP (tem))
	    {
	      nbytes = copy_text (SDATA (tem), p,
				  SBYTES (tem),
				  STRING_MULTIBYTE (tem), multibyte);
	      p += nbytes;
	    }
	}
      if (p != overlay_str_buf + total)
	abort ();
      if (pstr)
	*pstr = overlay_str_buf;
      return total;
    }
  return 0;
}

/* Shift overlays in BUF's overlay lists, to center the lists at POS.  */

void
recenter_overlay_lists (struct buffer *buf, EMACS_INT pos)
{
  Lisp_Object overlay, beg, end;
  struct Lisp_Overlay *prev, *tail, *next;

  /* See if anything in overlays_before should move to overlays_after.  */

  /* We don't strictly need prev in this loop; it should always be nil.
     But we use it for symmetry and in case that should cease to be true
     with some future change.  */
  prev = NULL;
  for (tail = buf->overlays_before; tail; prev = tail, tail = next)
    {
      next = tail->next;
      XSETMISC (overlay, tail);

      /* If the overlay is not valid, get rid of it.  */
      if (!OVERLAY_VALID (overlay))
#if 1
	abort ();
#else
	{
	  /* Splice the cons cell TAIL out of overlays_before.  */
	  if (!NILP (prev))
	    XCDR (prev) = next;
	  else
	    buf->overlays_before = next;
	  tail = prev;
	  continue;
	}
#endif

      beg = OVERLAY_START (overlay);
      end = OVERLAY_END (overlay);

      if (OVERLAY_POSITION (end) > pos)
	{
	  /* OVERLAY needs to be moved.  */
	  EMACS_INT where = OVERLAY_POSITION (beg);
	  struct Lisp_Overlay *other, *other_prev;

	  /* Splice the cons cell TAIL out of overlays_before.  */
	  if (prev)
	    prev->next = next;
	  else
	    buf->overlays_before = next;

	  /* Search thru overlays_after for where to put it.  */
	  other_prev = NULL;
	  for (other = buf->overlays_after; other;
	       other_prev = other, other = other->next)
	    {
	      Lisp_Object otherbeg, otheroverlay;

	      XSETMISC (otheroverlay, other);
	      eassert (OVERLAY_VALID (otheroverlay));

	      otherbeg = OVERLAY_START (otheroverlay);
	      if (OVERLAY_POSITION (otherbeg) >= where)
		break;
	    }

	  /* Add TAIL to overlays_after before OTHER.  */
	  tail->next = other;
	  if (other_prev)
	    other_prev->next = tail;
	  else
	    buf->overlays_after = tail;
	  tail = prev;
	}
      else
	/* We've reached the things that should stay in overlays_before.
	   All the rest of overlays_before must end even earlier,
	   so stop now.  */
	break;
    }

  /* See if anything in overlays_after should be in overlays_before.  */
  prev = NULL;
  for (tail = buf->overlays_after; tail; prev = tail, tail = next)
    {
      next = tail->next;
      XSETMISC (overlay, tail);

      /* If the overlay is not valid, get rid of it.  */
      if (!OVERLAY_VALID (overlay))
#if 1
	abort ();
#else
	{
	  /* Splice the cons cell TAIL out of overlays_after.  */
	  if (!NILP (prev))
	    XCDR (prev) = next;
	  else
	    buf->overlays_after = next;
	  tail = prev;
	  continue;
	}
#endif

      beg = OVERLAY_START (overlay);
      end = OVERLAY_END (overlay);

      /* Stop looking, when we know that nothing further
	 can possibly end before POS.  */
      if (OVERLAY_POSITION (beg) > pos)
	break;

      if (OVERLAY_POSITION (end) <= pos)
	{
	  /* OVERLAY needs to be moved.  */
	  EMACS_INT where = OVERLAY_POSITION (end);
	  struct Lisp_Overlay *other, *other_prev;

	  /* Splice the cons cell TAIL out of overlays_after.  */
	  if (prev)
	    prev->next = next;
	  else
	    buf->overlays_after = next;

	  /* Search thru overlays_before for where to put it.  */
	  other_prev = NULL;
	  for (other = buf->overlays_before; other;
	       other_prev = other, other = other->next)
	    {
	      Lisp_Object otherend, otheroverlay;

	      XSETMISC (otheroverlay, other);
	      eassert (OVERLAY_VALID (otheroverlay));

	      otherend = OVERLAY_END (otheroverlay);
	      if (OVERLAY_POSITION (otherend) <= where)
		break;
	    }

	  /* Add TAIL to overlays_before before OTHER.  */
	  tail->next = other;
	  if (other_prev)
	    other_prev->next = tail;
	  else
	    buf->overlays_before = tail;
	  tail = prev;
	}
    }

  buf->overlay_center = pos;
}

void
adjust_overlays_for_insert (EMACS_INT pos, EMACS_INT length)
{
  /* After an insertion, the lists are still sorted properly,
     but we may need to update the value of the overlay center.  */
  if (current_buffer->overlay_center >= pos)
    current_buffer->overlay_center += length;
}

void
adjust_overlays_for_delete (EMACS_INT pos, EMACS_INT length)
{
  if (current_buffer->overlay_center < pos)
    /* The deletion was to our right.  No change needed; the before- and
       after-lists are still consistent.  */
    ;
  else if (current_buffer->overlay_center > pos + length)
    /* The deletion was to our left.  We need to adjust the center value
       to account for the change in position, but the lists are consistent
       given the new value.  */
    current_buffer->overlay_center -= length;
  else
    /* We're right in the middle.  There might be things on the after-list
       that now belong on the before-list.  Recentering will move them,
       and also update the center point.  */
    recenter_overlay_lists (current_buffer, pos);
}

/* Fix up overlays that were garbled as a result of permuting markers
   in the range START through END.  Any overlay with at least one
   endpoint in this range will need to be unlinked from the overlay
   list and reinserted in its proper place.
   Such an overlay might even have negative size at this point.
   If so, we'll make the overlay empty. */
void
fix_start_end_in_overlays (register EMACS_INT start, register EMACS_INT end)
{
  Lisp_Object overlay;
  struct Lisp_Overlay *before_list IF_LINT (= NULL);
  struct Lisp_Overlay *after_list IF_LINT (= NULL);
  /* These are either nil, indicating that before_list or after_list
     should be assigned, or the cons cell the cdr of which should be
     assigned.  */
  struct Lisp_Overlay *beforep = NULL, *afterp = NULL;
  /* 'Parent', likewise, indicates a cons cell or
     current_buffer->overlays_before or overlays_after, depending
     which loop we're in.  */
  struct Lisp_Overlay *tail, *parent;
  EMACS_INT startpos, endpos;

  /* This algorithm shifts links around instead of consing and GCing.
     The loop invariant is that before_list (resp. after_list) is a
     well-formed list except that its last element, the CDR of beforep
     (resp. afterp) if beforep (afterp) isn't nil or before_list
     (after_list) if it is, is still uninitialized.  So it's not a bug
     that before_list isn't initialized, although it may look
     strange.  */
  for (parent = NULL, tail = current_buffer->overlays_before; tail;)
    {
      XSETMISC (overlay, tail);

      endpos = OVERLAY_POSITION (OVERLAY_END (overlay));
      startpos = OVERLAY_POSITION (OVERLAY_START (overlay));

      /* If the overlay is backwards, make it empty.  */
      if (endpos < startpos)
	{
	  startpos = endpos;
	  Fset_marker (OVERLAY_START (overlay), make_number (startpos),
		       Qnil);
	}

      if (endpos < start)
	break;

      if (endpos < end
	  || (startpos >= start && startpos < end))
	{
	  /* Add it to the end of the wrong list.  Later on,
	     recenter_overlay_lists will move it to the right place.  */
	  if (endpos < current_buffer->overlay_center)
	    {
	      if (!afterp)
		after_list = tail;
	      else
		afterp->next = tail;
	      afterp = tail;
	    }
	  else
	    {
	      if (!beforep)
		before_list = tail;
	      else
		beforep->next = tail;
	      beforep = tail;
	    }
	  if (!parent)
	    current_buffer->overlays_before = tail->next;
	  else
	    parent->next = tail->next;
	  tail = tail->next;
	}
      else
	parent = tail, tail = parent->next;
    }
  for (parent = NULL, tail = current_buffer->overlays_after; tail;)
    {
      XSETMISC (overlay, tail);

      startpos = OVERLAY_POSITION (OVERLAY_START (overlay));
      endpos = OVERLAY_POSITION (OVERLAY_END (overlay));

      /* If the overlay is backwards, make it empty.  */
      if (endpos < startpos)
	{
	  startpos = endpos;
	  Fset_marker (OVERLAY_START (overlay), make_number (startpos),
		       Qnil);
	}

      if (startpos >= end)
	break;

      if (startpos >= start
	  || (endpos >= start && endpos < end))
	{
	  if (endpos < current_buffer->overlay_center)
	    {
	      if (!afterp)
		after_list = tail;
	      else
		afterp->next = tail;
	      afterp = tail;
	    }
	  else
	    {
	      if (!beforep)
		before_list = tail;
	      else
		beforep->next = tail;
	      beforep = tail;
	    }
	  if (!parent)
	    current_buffer->overlays_after = tail->next;
	  else
	    parent->next = tail->next;
	  tail = tail->next;
	}
      else
	parent = tail, tail = parent->next;
    }

  /* Splice the constructed (wrong) lists into the buffer's lists,
     and let the recenter function make it sane again.  */
  if (beforep)
    {
      beforep->next = current_buffer->overlays_before;
      current_buffer->overlays_before = before_list;
    }
  recenter_overlay_lists (current_buffer, current_buffer->overlay_center);

  if (afterp)
    {
      afterp->next = current_buffer->overlays_after;
      current_buffer->overlays_after = after_list;
    }
  recenter_overlay_lists (current_buffer, current_buffer->overlay_center);
}

/* We have two types of overlay: the one whose ending marker is
   after-insertion-marker (this is the usual case) and the one whose
   ending marker is before-insertion-marker.  When `overlays_before'
   contains overlays of the latter type and the former type in this
   order and both overlays end at inserting position, inserting a text
   increases only the ending marker of the latter type, which results
   in incorrect ordering of `overlays_before'.

   This function fixes ordering of overlays in the slot
   `overlays_before' of the buffer *BP.  Before the insertion, `point'
   was at PREV, and now is at POS.  */

void
fix_overlays_before (struct buffer *bp, EMACS_INT prev, EMACS_INT pos)
{
  /* If parent is nil, replace overlays_before; otherwise, parent->next.  */
  struct Lisp_Overlay *tail = bp->overlays_before, *parent = NULL, *right_pair;
  Lisp_Object tem;
  EMACS_INT end IF_LINT (= 0);

  /* After the insertion, the several overlays may be in incorrect
     order.  The possibility is that, in the list `overlays_before',
     an overlay which ends at POS appears after an overlay which ends
     at PREV.  Since POS is greater than PREV, we must fix the
     ordering of these overlays, by moving overlays ends at POS before
     the overlays ends at PREV.  */

  /* At first, find a place where disordered overlays should be linked
     in.  It is where an overlay which end before POS exists. (i.e. an
     overlay whose ending marker is after-insertion-marker if disorder
     exists).  */
  while (tail
	 && (XSETMISC (tem, tail),
	     (end = OVERLAY_POSITION (OVERLAY_END (tem))) >= pos))
    {
      parent = tail;
      tail = tail->next;
    }

  /* If we don't find such an overlay,
     or the found one ends before PREV,
     or the found one is the last one in the list,
     we don't have to fix anything.  */
  if (!tail || end < prev || !tail->next)
    return;

  right_pair = parent;
  parent = tail;
  tail = tail->next;

  /* Now, end position of overlays in the list TAIL should be before
     or equal to PREV.  In the loop, an overlay which ends at POS is
     moved ahead to the place indicated by the CDR of RIGHT_PAIR.  If
     we found an overlay which ends before PREV, the remaining
     overlays are in correct order.  */
  while (tail)
    {
      XSETMISC (tem, tail);
      end = OVERLAY_POSITION (OVERLAY_END (tem));

      if (end == pos)
	{			/* This overlay is disordered. */
	  struct Lisp_Overlay *found = tail;

	  /* Unlink the found overlay.  */
	  tail = found->next;
	  parent->next = tail;
	  /* Move an overlay at RIGHT_PLACE to the next of the found one,
	     and link it into the right place.  */
	  if (!right_pair)
	    {
	      found->next = bp->overlays_before;
	      bp->overlays_before = found;
	    }
	  else
	    {
	      found->next = right_pair->next;
	      right_pair->next = found;
	    }
	}
      else if (end == prev)
	{
	  parent = tail;
	  tail = tail->next;
	}
      else			/* No more disordered overlay. */
	break;
    }
}

DEFUN ("overlayp", Foverlayp, Soverlayp, 1, 1, 0,
       doc: /* Return t if OBJECT is an overlay.  */)
  (Lisp_Object object)
{
  return (OVERLAYP (object) ? Qt : Qnil);
}

DEFUN ("make-overlay", Fmake_overlay, Smake_overlay, 2, 5, 0,
       doc: /* Create a new overlay with range BEG to END in BUFFER.
If omitted, BUFFER defaults to the current buffer.
BEG and END may be integers or markers.
The fourth arg FRONT-ADVANCE, if non-nil, makes the marker
for the front of the overlay advance when text is inserted there
\(which means the text *is not* included in the overlay).
The fifth arg REAR-ADVANCE, if non-nil, makes the marker
for the rear of the overlay advance when text is inserted there
\(which means the text *is* included in the overlay).  */)
  (Lisp_Object beg, Lisp_Object end, Lisp_Object buffer, Lisp_Object front_advance, Lisp_Object rear_advance)
{
  Lisp_Object overlay;
  struct buffer *b;

  if (NILP (buffer))
    XSETBUFFER (buffer, current_buffer);
  else
    CHECK_BUFFER (buffer);
  if (MARKERP (beg)
      && ! EQ (Fmarker_buffer (beg), buffer))
    error ("Marker points into wrong buffer");
  if (MARKERP (end)
      && ! EQ (Fmarker_buffer (end), buffer))
    error ("Marker points into wrong buffer");

  CHECK_NUMBER_COERCE_MARKER (beg);
  CHECK_NUMBER_COERCE_MARKER (end);

  if (XINT (beg) > XINT (end))
    {
      Lisp_Object temp;
      temp = beg; beg = end; end = temp;
    }

  b = XBUFFER (buffer);

  beg = Fset_marker (Fmake_marker (), beg, buffer);
  end = Fset_marker (Fmake_marker (), end, buffer);

  if (!NILP (front_advance))
    XMARKER (beg)->insertion_type = 1;
  if (!NILP (rear_advance))
    XMARKER (end)->insertion_type = 1;

  overlay = allocate_misc ();
  XMISCTYPE (overlay) = Lisp_Misc_Overlay;
  XOVERLAY (overlay)->start = beg;
  XOVERLAY (overlay)->end = end;
  XOVERLAY (overlay)->plist = Qnil;
  XOVERLAY (overlay)->next = NULL;

  /* Put the new overlay on the wrong list.  */
  end = OVERLAY_END (overlay);
  if (OVERLAY_POSITION (end) < b->overlay_center)
    {
      if (b->overlays_after)
	XOVERLAY (overlay)->next = b->overlays_after;
      b->overlays_after = XOVERLAY (overlay);
    }
  else
    {
      if (b->overlays_before)
	XOVERLAY (overlay)->next = b->overlays_before;
      b->overlays_before = XOVERLAY (overlay);
    }

  /* This puts it in the right list, and in the right order.  */
  recenter_overlay_lists (b, b->overlay_center);

  /* We don't need to redisplay the region covered by the overlay, because
     the overlay has no properties at the moment.  */

  return overlay;
}

/* Mark a section of BUF as needing redisplay because of overlays changes.  */

static void
modify_overlay (struct buffer *buf, EMACS_INT start, EMACS_INT end)
{
  if (start > end)
    {
      EMACS_INT temp = start;
      start = end;
      end = temp;
    }

  BUF_COMPUTE_UNCHANGED (buf, start, end);

  /* If this is a buffer not in the selected window,
     we must do other windows.  */
  if (buf != XBUFFER (XWINDOW (selected_window)->buffer))
    windows_or_buffers_changed = 1;
  /* If multiple windows show this buffer, we must do other windows.  */
  else if (buffer_shared > 1)
    windows_or_buffers_changed = 1;
  /* If we modify an overlay at the end of the buffer, we cannot
     be sure that window end is still valid.  */
  else if (end >= ZV && start <= ZV)
    windows_or_buffers_changed = 1;

  ++BUF_OVERLAY_MODIFF (buf);
}


static struct Lisp_Overlay *
unchain_overlay (struct Lisp_Overlay *list, struct Lisp_Overlay *overlay)
{
  struct Lisp_Overlay *tmp, *prev;
  for (tmp = list, prev = NULL; tmp; prev = tmp, tmp = tmp->next)
    if (tmp == overlay)
      {
	if (prev)
	  prev->next = tmp->next;
	else
	  list = tmp->next;
	overlay->next = NULL;
	break;
      }
  return list;
}

DEFUN ("move-overlay", Fmove_overlay, Smove_overlay, 3, 4, 0,
       doc: /* Set the endpoints of OVERLAY to BEG and END in BUFFER.
If BUFFER is omitted, leave OVERLAY in the same buffer it inhabits now.
If BUFFER is omitted, and OVERLAY is in no buffer, put it in the current
buffer.  */)
  (Lisp_Object overlay, Lisp_Object beg, Lisp_Object end, Lisp_Object buffer)
{
  struct buffer *b, *ob;
  Lisp_Object obuffer;
  int count = SPECPDL_INDEX ();

  CHECK_OVERLAY (overlay);
  if (NILP (buffer))
    buffer = Fmarker_buffer (OVERLAY_START (overlay));
  if (NILP (buffer))
    XSETBUFFER (buffer, current_buffer);
  CHECK_BUFFER (buffer);

  if (MARKERP (beg)
      && ! EQ (Fmarker_buffer (beg), buffer))
    error ("Marker points into wrong buffer");
  if (MARKERP (end)
      && ! EQ (Fmarker_buffer (end), buffer))
    error ("Marker points into wrong buffer");

  CHECK_NUMBER_COERCE_MARKER (beg);
  CHECK_NUMBER_COERCE_MARKER (end);

  if (XINT (beg) == XINT (end) && ! NILP (Foverlay_get (overlay, Qevaporate)))
    return Fdelete_overlay (overlay);

  if (XINT (beg) > XINT (end))
    {
      Lisp_Object temp;
      temp = beg; beg = end; end = temp;
    }

  specbind (Qinhibit_quit, Qt);

  obuffer = Fmarker_buffer (OVERLAY_START (overlay));
  b = XBUFFER (buffer);
  ob = BUFFERP (obuffer) ? XBUFFER (obuffer) : (struct buffer *) 0;

  /* If the overlay has changed buffers, do a thorough redisplay.  */
  if (!EQ (buffer, obuffer))
    {
      /* Redisplay where the overlay was.  */
      if (!NILP (obuffer))
	{
	  EMACS_INT o_beg;
	  EMACS_INT o_end;

	  o_beg = OVERLAY_POSITION (OVERLAY_START (overlay));
	  o_end = OVERLAY_POSITION (OVERLAY_END (overlay));

	  modify_overlay (ob, o_beg, o_end);
	}

      /* Redisplay where the overlay is going to be.  */
      modify_overlay (b, XINT (beg), XINT (end));
    }
  else
    /* Redisplay the area the overlay has just left, or just enclosed.  */
    {
      EMACS_INT o_beg, o_end;

      o_beg = OVERLAY_POSITION (OVERLAY_START (overlay));
      o_end = OVERLAY_POSITION (OVERLAY_END (overlay));

      if (o_beg == XINT (beg))
	modify_overlay (b, o_end, XINT (end));
      else if (o_end == XINT (end))
	modify_overlay (b, o_beg, XINT (beg));
      else
	{
	  if (XINT (beg) < o_beg) o_beg = XINT (beg);
	  if (XINT (end) > o_end) o_end = XINT (end);
	  modify_overlay (b, o_beg, o_end);
	}
    }

  if (!NILP (obuffer))
    {
      ob->overlays_before
	= unchain_overlay (ob->overlays_before, XOVERLAY (overlay));
      ob->overlays_after
	= unchain_overlay (ob->overlays_after, XOVERLAY (overlay));
      eassert (XOVERLAY (overlay)->next == NULL);
    }

  Fset_marker (OVERLAY_START (overlay), beg, buffer);
  Fset_marker (OVERLAY_END   (overlay), end, buffer);

  /* Put the overlay on the wrong list.  */
  end = OVERLAY_END (overlay);
  if (OVERLAY_POSITION (end) < b->overlay_center)
    {
      XOVERLAY (overlay)->next = b->overlays_after;
      b->overlays_after = XOVERLAY (overlay);
    }
  else
    {
      XOVERLAY (overlay)->next = b->overlays_before;
      b->overlays_before = XOVERLAY (overlay);
    }

  /* This puts it in the right list, and in the right order.  */
  recenter_overlay_lists (b, b->overlay_center);

  return unbind_to (count, overlay);
}

DEFUN ("delete-overlay", Fdelete_overlay, Sdelete_overlay, 1, 1, 0,
       doc: /* Delete the overlay OVERLAY from its buffer.  */)
  (Lisp_Object overlay)
{
  Lisp_Object buffer;
  struct buffer *b;
  int count = SPECPDL_INDEX ();

  CHECK_OVERLAY (overlay);

  buffer = Fmarker_buffer (OVERLAY_START (overlay));
  if (NILP (buffer))
    return Qnil;

  b = XBUFFER (buffer);
  specbind (Qinhibit_quit, Qt);

  b->overlays_before = unchain_overlay (b->overlays_before,XOVERLAY (overlay));
  b->overlays_after  = unchain_overlay (b->overlays_after, XOVERLAY (overlay));
  eassert (XOVERLAY (overlay)->next == NULL);
  modify_overlay (b,
		  marker_position (OVERLAY_START (overlay)),
		  marker_position (OVERLAY_END   (overlay)));
  Fset_marker (OVERLAY_START (overlay), Qnil, Qnil);
  Fset_marker (OVERLAY_END   (overlay), Qnil, Qnil);

  /* When deleting an overlay with before or after strings, turn off
     display optimizations for the affected buffer, on the basis that
     these strings may contain newlines.  This is easier to do than to
     check for that situation during redisplay.  */
  if (!windows_or_buffers_changed
      && (!NILP (Foverlay_get (overlay, Qbefore_string))
	  || !NILP (Foverlay_get (overlay, Qafter_string))))
    b->prevent_redisplay_optimizations_p = 1;

  return unbind_to (count, Qnil);
}

/* Overlay dissection functions.  */

DEFUN ("overlay-start", Foverlay_start, Soverlay_start, 1, 1, 0,
       doc: /* Return the position at which OVERLAY starts.  */)
  (Lisp_Object overlay)
{
  CHECK_OVERLAY (overlay);

  return (Fmarker_position (OVERLAY_START (overlay)));
}

DEFUN ("overlay-end", Foverlay_end, Soverlay_end, 1, 1, 0,
       doc: /* Return the position at which OVERLAY ends.  */)
  (Lisp_Object overlay)
{
  CHECK_OVERLAY (overlay);

  return (Fmarker_position (OVERLAY_END (overlay)));
}

DEFUN ("overlay-buffer", Foverlay_buffer, Soverlay_buffer, 1, 1, 0,
       doc: /* Return the buffer OVERLAY belongs to.
Return nil if OVERLAY has been deleted.  */)
  (Lisp_Object overlay)
{
  CHECK_OVERLAY (overlay);

  return Fmarker_buffer (OVERLAY_START (overlay));
}

DEFUN ("overlay-properties", Foverlay_properties, Soverlay_properties, 1, 1, 0,
       doc: /* Return a list of the properties on OVERLAY.
This is a copy of OVERLAY's plist; modifying its conses has no effect on
OVERLAY.  */)
  (Lisp_Object overlay)
{
  CHECK_OVERLAY (overlay);

  return Fcopy_sequence (XOVERLAY (overlay)->plist);
}


DEFUN ("overlays-at", Foverlays_at, Soverlays_at, 1, 1, 0,
       doc: /* Return a list of the overlays that contain the character at POS.  */)
  (Lisp_Object pos)
{
  ptrdiff_t len, noverlays;
  Lisp_Object *overlay_vec;
  Lisp_Object result;

  CHECK_NUMBER_COERCE_MARKER (pos);

  len = 10;
  /* We can't use alloca here because overlays_at can call xrealloc.  */
  overlay_vec = (Lisp_Object *) xmalloc (len * sizeof (Lisp_Object));

  /* Put all the overlays we want in a vector in overlay_vec.
     Store the length in len.  */
  noverlays = overlays_at (XINT (pos), 1, &overlay_vec, &len,
			   (EMACS_INT *) 0, (EMACS_INT *) 0, 0);

  /* Make a list of them all.  */
  result = Flist (noverlays, overlay_vec);

  xfree (overlay_vec);
  return result;
}

DEFUN ("overlays-in", Foverlays_in, Soverlays_in, 2, 2, 0,
       doc: /* Return a list of the overlays that overlap the region BEG ... END.
Overlap means that at least one character is contained within the overlay
and also contained within the specified region.
Empty overlays are included in the result if they are located at BEG,
between BEG and END, or at END provided END denotes the position at the
end of the buffer.  */)
  (Lisp_Object beg, Lisp_Object end)
{
  ptrdiff_t len, noverlays;
  Lisp_Object *overlay_vec;
  Lisp_Object result;

  CHECK_NUMBER_COERCE_MARKER (beg);
  CHECK_NUMBER_COERCE_MARKER (end);

  len = 10;
  overlay_vec = (Lisp_Object *) xmalloc (len * sizeof (Lisp_Object));

  /* Put all the overlays we want in a vector in overlay_vec.
     Store the length in len.  */
  noverlays = overlays_in (XINT (beg), XINT (end), 1, &overlay_vec, &len,
			   NULL, NULL);

  /* Make a list of them all.  */
  result = Flist (noverlays, overlay_vec);

  xfree (overlay_vec);
  return result;
}

DEFUN ("next-overlay-change", Fnext_overlay_change, Snext_overlay_change,
       1, 1, 0,
       doc: /* Return the next position after POS where an overlay starts or ends.
If there are no overlay boundaries from POS to (point-max),
the value is (point-max).  */)
  (Lisp_Object pos)
{
  ptrdiff_t i, len, noverlays;
  EMACS_INT endpos;
  Lisp_Object *overlay_vec;

  CHECK_NUMBER_COERCE_MARKER (pos);

  len = 10;
  overlay_vec = (Lisp_Object *) xmalloc (len * sizeof (Lisp_Object));

  /* Put all the overlays we want in a vector in overlay_vec.
     Store the length in len.
     endpos gets the position where the next overlay starts.  */
  noverlays = overlays_at (XINT (pos), 1, &overlay_vec, &len,
			   &endpos, (EMACS_INT *) 0, 1);

  /* If any of these overlays ends before endpos,
     use its ending point instead.  */
  for (i = 0; i < noverlays; i++)
    {
      Lisp_Object oend;
      EMACS_INT oendpos;

      oend = OVERLAY_END (overlay_vec[i]);
      oendpos = OVERLAY_POSITION (oend);
      if (oendpos < endpos)
	endpos = oendpos;
    }

  xfree (overlay_vec);
  return make_number (endpos);
}

DEFUN ("previous-overlay-change", Fprevious_overlay_change,
       Sprevious_overlay_change, 1, 1, 0,
       doc: /* Return the previous position before POS where an overlay starts or ends.
If there are no overlay boundaries from (point-min) to POS,
the value is (point-min).  */)
  (Lisp_Object pos)
{
  EMACS_INT prevpos;
  Lisp_Object *overlay_vec;
  ptrdiff_t len;

  CHECK_NUMBER_COERCE_MARKER (pos);

  /* At beginning of buffer, we know the answer;
     avoid bug subtracting 1 below.  */
  if (XINT (pos) == BEGV)
    return pos;

  len = 10;
  overlay_vec = (Lisp_Object *) xmalloc (len * sizeof (Lisp_Object));

  /* Put all the overlays we want in a vector in overlay_vec.
     Store the length in len.
     prevpos gets the position of the previous change.  */
  overlays_at (XINT (pos), 1, &overlay_vec, &len,
	       (EMACS_INT *) 0, &prevpos, 1);

  xfree (overlay_vec);
  return make_number (prevpos);
}

/* These functions are for debugging overlays.  */

DEFUN ("overlay-lists", Foverlay_lists, Soverlay_lists, 0, 0, 0,
       doc: /* Return a pair of lists giving all the overlays of the current buffer.
The car has all the overlays before the overlay center;
the cdr has all the overlays after the overlay center.
Recentering overlays moves overlays between these lists.
The lists you get are copies, so that changing them has no effect.
However, the overlays you get are the real objects that the buffer uses.  */)
  (void)
{
  struct Lisp_Overlay *ol;
  Lisp_Object before = Qnil, after = Qnil, tmp;
  for (ol = current_buffer->overlays_before; ol; ol = ol->next)
    {
      XSETMISC (tmp, ol);
      before = Fcons (tmp, before);
    }
  for (ol = current_buffer->overlays_after; ol; ol = ol->next)
    {
      XSETMISC (tmp, ol);
      after = Fcons (tmp, after);
    }
  return Fcons (Fnreverse (before), Fnreverse (after));
}

DEFUN ("overlay-recenter", Foverlay_recenter, Soverlay_recenter, 1, 1, 0,
       doc: /* Recenter the overlays of the current buffer around position POS.
That makes overlay lookup faster for positions near POS (but perhaps slower
for positions far away from POS).  */)
  (Lisp_Object pos)
{
  CHECK_NUMBER_COERCE_MARKER (pos);

  recenter_overlay_lists (current_buffer, XINT (pos));
  return Qnil;
}

DEFUN ("overlay-get", Foverlay_get, Soverlay_get, 2, 2, 0,
       doc: /* Get the property of overlay OVERLAY with property name PROP.  */)
  (Lisp_Object overlay, Lisp_Object prop)
{
  CHECK_OVERLAY (overlay);
  return lookup_char_property (XOVERLAY (overlay)->plist, prop, 0);
}

DEFUN ("overlay-put", Foverlay_put, Soverlay_put, 3, 3, 0,
       doc: /* Set one property of overlay OVERLAY: give property PROP value VALUE.
VALUE will be returned.*/)
  (Lisp_Object overlay, Lisp_Object prop, Lisp_Object value)
{
  Lisp_Object tail, buffer;
  int changed;

  CHECK_OVERLAY (overlay);

  buffer = Fmarker_buffer (OVERLAY_START (overlay));

  for (tail = XOVERLAY (overlay)->plist;
       CONSP (tail) && CONSP (XCDR (tail));
       tail = XCDR (XCDR (tail)))
    if (EQ (XCAR (tail), prop))
      {
	changed = !EQ (XCAR (XCDR (tail)), value);
	XSETCAR (XCDR (tail), value);
	goto found;
      }
  /* It wasn't in the list, so add it to the front.  */
  changed = !NILP (value);
  XOVERLAY (overlay)->plist
    = Fcons (prop, Fcons (value, XOVERLAY (overlay)->plist));
 found:
  if (! NILP (buffer))
    {
      if (changed)
	modify_overlay (XBUFFER (buffer),
			marker_position (OVERLAY_START (overlay)),
			marker_position (OVERLAY_END   (overlay)));
      if (EQ (prop, Qevaporate) && ! NILP (value)
	  && (OVERLAY_POSITION (OVERLAY_START (overlay))
	      == OVERLAY_POSITION (OVERLAY_END (overlay))))
	Fdelete_overlay (overlay);
    }

  return value;
}

/* Subroutine of report_overlay_modification.  */

/* Lisp vector holding overlay hook functions to call.
   Vector elements come in pairs.
   Each even-index element is a list of hook functions.
   The following odd-index element is the overlay they came from.

   Before the buffer change, we fill in this vector
   as we call overlay hook functions.
   After the buffer change, we get the functions to call from this vector.
   This way we always call the same functions before and after the change.  */
static Lisp_Object last_overlay_modification_hooks;

/* Number of elements actually used in last_overlay_modification_hooks.  */
static int last_overlay_modification_hooks_used;

/* Add one functionlist/overlay pair
   to the end of last_overlay_modification_hooks.  */

static void
add_overlay_mod_hooklist (Lisp_Object functionlist, Lisp_Object overlay)
{
  int oldsize = ASIZE (last_overlay_modification_hooks);

  if (last_overlay_modification_hooks_used == oldsize)
    last_overlay_modification_hooks = larger_vector
      (last_overlay_modification_hooks, oldsize * 2, Qnil);
  ASET (last_overlay_modification_hooks, last_overlay_modification_hooks_used,
	functionlist); last_overlay_modification_hooks_used++;
  ASET (last_overlay_modification_hooks, last_overlay_modification_hooks_used,
	overlay);      last_overlay_modification_hooks_used++;
}

/* Run the modification-hooks of overlays that include
   any part of the text in START to END.
   If this change is an insertion, also
   run the insert-before-hooks of overlay starting at END,
   and the insert-after-hooks of overlay ending at START.

   This is called both before and after the modification.
   AFTER is nonzero when we call after the modification.

   ARG1, ARG2, ARG3 are arguments to pass to the hook functions.
   When AFTER is nonzero, they are the start position,
   the position after the inserted new text,
   and the length of deleted or replaced old text.  */

void
report_overlay_modification (Lisp_Object start, Lisp_Object end, int after,
			     Lisp_Object arg1, Lisp_Object arg2, Lisp_Object arg3)
{
  Lisp_Object prop, overlay;
  struct Lisp_Overlay *tail;
  /* 1 if this change is an insertion.  */
  int insertion = (after ? XFASTINT (arg3) == 0 : EQ (start, end));
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;

  overlay = Qnil;
  tail = NULL;

  /* We used to run the functions as soon as we found them and only register
     them in last_overlay_modification_hooks for the purpose of the `after'
     case.  But running elisp code as we traverse the list of overlays is
     painful because the list can be modified by the elisp code so we had to
     copy at several places.  We now simply do a read-only traversal that
     only collects the functions to run and we run them afterwards.  It's
     simpler, especially since all the code was already there.  -stef  */

  if (!after)
    {
      /* We are being called before a change.
	 Scan the overlays to find the functions to call.  */
      last_overlay_modification_hooks_used = 0;
      for (tail = current_buffer->overlays_before; tail; tail = tail->next)
	{
	  EMACS_INT startpos, endpos;
	  Lisp_Object ostart, oend;

	  XSETMISC (overlay, tail);

	  ostart = OVERLAY_START (overlay);
	  oend = OVERLAY_END (overlay);
	  endpos = OVERLAY_POSITION (oend);
	  if (XFASTINT (start) > endpos)
	    break;
	  startpos = OVERLAY_POSITION (ostart);
	  if (insertion && (XFASTINT (start) == startpos
			    || XFASTINT (end) == startpos))
	    {
	      prop = Foverlay_get (overlay, Qinsert_in_front_hooks);
	      if (!NILP (prop))
		add_overlay_mod_hooklist (prop, overlay);
	    }
	  if (insertion && (XFASTINT (start) == endpos
			    || XFASTINT (end) == endpos))
	    {
	      prop = Foverlay_get (overlay, Qinsert_behind_hooks);
	      if (!NILP (prop))
		add_overlay_mod_hooklist (prop, overlay);
	    }
	  /* Test for intersecting intervals.  This does the right thing
	     for both insertion and deletion.  */
	  if (XFASTINT (end) > startpos && XFASTINT (start) < endpos)
	    {
	      prop = Foverlay_get (overlay, Qmodification_hooks);
	      if (!NILP (prop))
		add_overlay_mod_hooklist (prop, overlay);
	    }
	}

      for (tail = current_buffer->overlays_after; tail; tail = tail->next)
	{
	  EMACS_INT startpos, endpos;
	  Lisp_Object ostart, oend;

	  XSETMISC (overlay, tail);

	  ostart = OVERLAY_START (overlay);
	  oend = OVERLAY_END (overlay);
	  startpos = OVERLAY_POSITION (ostart);
	  endpos = OVERLAY_POSITION (oend);
	  if (XFASTINT (end) < startpos)
	    break;
	  if (insertion && (XFASTINT (start) == startpos
			    || XFASTINT (end) == startpos))
	    {
	      prop = Foverlay_get (overlay, Qinsert_in_front_hooks);
	      if (!NILP (prop))
		add_overlay_mod_hooklist (prop, overlay);
	    }
	  if (insertion && (XFASTINT (start) == endpos
			    || XFASTINT (end) == endpos))
	    {
	      prop = Foverlay_get (overlay, Qinsert_behind_hooks);
	      if (!NILP (prop))
		add_overlay_mod_hooklist (prop, overlay);
	    }
	  /* Test for intersecting intervals.  This does the right thing
	     for both insertion and deletion.  */
	  if (XFASTINT (end) > startpos && XFASTINT (start) < endpos)
	    {
	      prop = Foverlay_get (overlay, Qmodification_hooks);
	      if (!NILP (prop))
		add_overlay_mod_hooklist (prop, overlay);
	    }
	}
    }

  GCPRO4 (overlay, arg1, arg2, arg3);
  {
    /* Call the functions recorded in last_overlay_modification_hooks.
       First copy the vector contents, in case some of these hooks
       do subsequent modification of the buffer.  */
    int size = last_overlay_modification_hooks_used;
    Lisp_Object *copy = (Lisp_Object *) alloca (size * sizeof (Lisp_Object));
    int i;

    memcpy (copy, XVECTOR (last_overlay_modification_hooks)->contents,
	    size * sizeof (Lisp_Object));
    gcpro1.var = copy;
    gcpro1.nvars = size;

    for (i = 0; i < size;)
      {
	Lisp_Object prop_i, overlay_i;
	prop_i = copy[i++];
	overlay_i = copy[i++];
	call_overlay_mod_hooks (prop_i, overlay_i, after, arg1, arg2, arg3);
      }
  }
  UNGCPRO;
}

static void
call_overlay_mod_hooks (Lisp_Object list, Lisp_Object overlay, int after,
			Lisp_Object arg1, Lisp_Object arg2, Lisp_Object arg3)
{
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;

  GCPRO4 (list, arg1, arg2, arg3);

  while (CONSP (list))
    {
      if (NILP (arg3))
	call4 (XCAR (list), overlay, after ? Qt : Qnil, arg1, arg2);
      else
	call5 (XCAR (list), overlay, after ? Qt : Qnil, arg1, arg2, arg3);
      list = XCDR (list);
    }
  UNGCPRO;
}

/* Delete any zero-sized overlays at position POS, if the `evaporate'
   property is set.  */
void
evaporate_overlays (EMACS_INT pos)
{
  Lisp_Object overlay, hit_list;
  struct Lisp_Overlay *tail;

  hit_list = Qnil;
  if (pos <= current_buffer->overlay_center)
    for (tail = current_buffer->overlays_before; tail; tail = tail->next)
      {
	EMACS_INT endpos;
	XSETMISC (overlay, tail);
	endpos = OVERLAY_POSITION (OVERLAY_END (overlay));
	if (endpos < pos)
	  break;
	if (endpos == pos && OVERLAY_POSITION (OVERLAY_START (overlay)) == pos
	    && ! NILP (Foverlay_get (overlay, Qevaporate)))
	  hit_list = Fcons (overlay, hit_list);
      }
  else
    for (tail = current_buffer->overlays_after; tail; tail = tail->next)
      {
	EMACS_INT startpos;
	XSETMISC (overlay, tail);
	startpos = OVERLAY_POSITION (OVERLAY_START (overlay));
	if (startpos > pos)
	  break;
	if (startpos == pos && OVERLAY_POSITION (OVERLAY_END (overlay)) == pos
	    && ! NILP (Foverlay_get (overlay, Qevaporate)))
	  hit_list = Fcons (overlay, hit_list);
      }
  for (; CONSP (hit_list); hit_list = XCDR (hit_list))
    Fdelete_overlay (XCAR (hit_list));
}

/* Somebody has tried to store a value with an unacceptable type
   in the slot with offset OFFSET.  */

void
buffer_slot_type_mismatch (Lisp_Object newval, int type)
{
  Lisp_Object predicate;

  switch (type)
    {
    case_Lisp_Int:    predicate = Qintegerp; break;
    case Lisp_String: predicate = Qstringp;  break;
    case Lisp_Symbol: predicate = Qsymbolp;  break;
    default: abort ();
    }

  wrong_type_argument (predicate, newval);
}


/***********************************************************************
			 Allocation with mmap
 ***********************************************************************/

#ifdef USE_MMAP_FOR_BUFFERS

#include <sys/types.h>
#include <sys/mman.h>

#ifndef MAP_ANON
#ifdef MAP_ANONYMOUS
#define MAP_ANON MAP_ANONYMOUS
#else
#define MAP_ANON 0
#endif
#endif

#ifndef MAP_FAILED
#define MAP_FAILED ((void *) -1)
#endif

#include <stdio.h>

#if MAP_ANON == 0
#include <fcntl.h>
#endif

#include "coding.h"


/* Memory is allocated in regions which are mapped using mmap(2).
   The current implementation lets the system select mapped
   addresses;  we're not using MAP_FIXED in general, except when
   trying to enlarge regions.

   Each mapped region starts with a mmap_region structure, the user
   area starts after that structure, aligned to MEM_ALIGN.

	+-----------------------+
	| struct mmap_info +	|
	| padding		|
	+-----------------------+
	| user data		|
	|			|
	|			|
	+-----------------------+  */

struct mmap_region
{
  /* User-specified size.  */
  size_t nbytes_specified;

  /* Number of bytes mapped */
  size_t nbytes_mapped;

  /* Pointer to the location holding the address of the memory
     allocated with the mmap'd block.  The variable actually points
     after this structure.  */
  POINTER_TYPE **var;

  /* Next and previous in list of all mmap'd regions.  */
  struct mmap_region *next, *prev;
};

/* Doubly-linked list of mmap'd regions.  */

static struct mmap_region *mmap_regions;

/* File descriptor for mmap.  If we don't have anonymous mapping,
   /dev/zero will be opened on it.  */

static int mmap_fd;

/* Temporary storage for mmap_set_vars, see there.  */

static struct mmap_region *mmap_regions_1;
static int mmap_fd_1;

/* Page size on this system.  */

static int mmap_page_size;

/* 1 means mmap has been initialized.  */

static int mmap_initialized_p;

/* Value is X rounded up to the next multiple of N.  */

#define ROUND(X, N)	(((X) + (N) - 1) / (N) * (N))

/* Size of mmap_region structure plus padding.  */

#define MMAP_REGION_STRUCT_SIZE	\
     ROUND (sizeof (struct mmap_region), MEM_ALIGN)

/* Given a pointer P to the start of the user-visible part of a mapped
   region, return a pointer to the start of the region.  */

#define MMAP_REGION(P) \
     ((struct mmap_region *) ((char *) (P) - MMAP_REGION_STRUCT_SIZE))

/* Given a pointer P to the start of a mapped region, return a pointer
   to the start of the user-visible part of the region.  */

#define MMAP_USER_AREA(P) \
     ((POINTER_TYPE *) ((char *) (P) + MMAP_REGION_STRUCT_SIZE))

#define MEM_ALIGN	sizeof (double)

/* Predicate returning true if part of the address range [START .. END]
   is currently mapped.  Used to prevent overwriting an existing
   memory mapping.

   Default is to conservatively assume the address range is occupied by
   something else.  This can be overridden by system configuration
   files if system-specific means to determine this exists.  */

#ifndef MMAP_ALLOCATED_P
#define MMAP_ALLOCATED_P(start, end) 1
#endif

/* Perform necessary initializations for the use of mmap.  */

static void
mmap_init (void)
{
#if MAP_ANON == 0
  /* The value of mmap_fd is initially 0 in temacs, and -1
     in a dumped Emacs.  */
  if (mmap_fd <= 0)
    {
      /* No anonymous mmap -- we need the file descriptor.  */
      mmap_fd = open ("/dev/zero", O_RDONLY);
      if (mmap_fd == -1)
	fatal ("Cannot open /dev/zero: %s", emacs_strerror (errno));
    }
#endif /* MAP_ANON == 0 */

  if (mmap_initialized_p)
    return;
  mmap_initialized_p = 1;

#if MAP_ANON != 0
  mmap_fd = -1;
#endif

  mmap_page_size = getpagesize ();
}

/* Return a region overlapping address range START...END, or null if
   none.  END is not including, i.e. the last byte in the range
   is at END - 1.  */

static struct mmap_region *
mmap_find (POINTER_TYPE *start, POINTER_TYPE *end)
{
  struct mmap_region *r;
  char *s = (char *) start, *e = (char *) end;

  for (r = mmap_regions; r; r = r->next)
    {
      char *rstart = (char *) r;
      char *rend   = rstart + r->nbytes_mapped;

      if (/* First byte of range, i.e. START, in this region?  */
	  (s >= rstart && s < rend)
	  /* Last byte of range, i.e. END - 1, in this region?  */
	  || (e > rstart && e <= rend)
	  /* First byte of this region in the range?  */
	  || (rstart >= s && rstart < e)
	  /* Last byte of this region in the range?  */
	  || (rend > s && rend <= e))
	break;
    }

  return r;
}


/* Unmap a region.  P is a pointer to the start of the user-araa of
   the region.  Value is non-zero if successful.  */

static int
mmap_free_1 (struct mmap_region *r)
{
  if (r->next)
    r->next->prev = r->prev;
  if (r->prev)
    r->prev->next = r->next;
  else
    mmap_regions = r->next;

  if (munmap ((POINTER_TYPE *) r, r->nbytes_mapped) == -1)
    {
      fprintf (stderr, "munmap: %s\n", emacs_strerror (errno));
      return 0;
    }

  return 1;
}


/* Enlarge region R by NPAGES pages.  NPAGES < 0 means shrink R.
   Value is non-zero if successful.  */

static int
mmap_enlarge (struct mmap_region *r, int npages)
{
  char *region_end = (char *) r + r->nbytes_mapped;
  size_t nbytes;
  int success = 0;

  if (npages < 0)
    {
      /* Unmap pages at the end of the region.  */
      nbytes = - npages * mmap_page_size;
      if (munmap (region_end - nbytes, nbytes) == -1)
	fprintf (stderr, "munmap: %s\n", emacs_strerror (errno));
      else
	{
	  r->nbytes_mapped -= nbytes;
	  success = 1;
	}
    }
  else if (npages > 0)
    {
      nbytes = npages * mmap_page_size;

      /* Try to map additional pages at the end of the region.  We
	 cannot do this if the address range is already occupied by
	 something else because mmap deletes any previous mapping.
	 I'm not sure this is worth doing, let's see.  */
      if (!MMAP_ALLOCATED_P (region_end, region_end + nbytes))
	{
	  POINTER_TYPE *p;

	  p = mmap (region_end, nbytes, PROT_READ | PROT_WRITE,
		    MAP_ANON | MAP_PRIVATE | MAP_FIXED, mmap_fd, 0);
	  if (p == MAP_FAILED)
	    ; /* fprintf (stderr, "mmap: %s\n", emacs_strerror (errno)); */
	  else if (p != (POINTER_TYPE *) region_end)
	    {
	      /* Kernels are free to choose a different address.  In
		 that case, unmap what we've mapped above; we have
		 no use for it.  */
	      if (munmap (p, nbytes) == -1)
		fprintf (stderr, "munmap: %s\n", emacs_strerror (errno));
	    }
	  else
	    {
	      r->nbytes_mapped += nbytes;
	      success = 1;
	    }
	}
    }

  return success;
}


/* Set or reset variables holding references to mapped regions.  If
   RESTORE_P is zero, set all variables to null.  If RESTORE_P is
   non-zero, set all variables to the start of the user-areas
   of mapped regions.

   This function is called from Fdump_emacs to ensure that the dumped
   Emacs doesn't contain references to memory that won't be mapped
   when Emacs starts.  */

void
mmap_set_vars (int restore_p)
{
  struct mmap_region *r;

  if (restore_p)
    {
      mmap_regions = mmap_regions_1;
      mmap_fd = mmap_fd_1;
      for (r = mmap_regions; r; r = r->next)
	*r->var = MMAP_USER_AREA (r);
    }
  else
    {
      for (r = mmap_regions; r; r = r->next)
	*r->var = NULL;
      mmap_regions_1 = mmap_regions;
      mmap_regions = NULL;
      mmap_fd_1 = mmap_fd;
      mmap_fd = -1;
    }
}


/* Allocate a block of storage large enough to hold NBYTES bytes of
   data.  A pointer to the data is returned in *VAR.  VAR is thus the
   address of some variable which will use the data area.

   The allocation of 0 bytes is valid.

   If we can't allocate the necessary memory, set *VAR to null, and
   return null.  */

static POINTER_TYPE *
mmap_alloc (POINTER_TYPE **var, size_t nbytes)
{
  void *p;
  size_t map;

  mmap_init ();

  map = ROUND (nbytes + MMAP_REGION_STRUCT_SIZE, mmap_page_size);
  p = mmap (NULL, map, PROT_READ | PROT_WRITE, MAP_ANON | MAP_PRIVATE,
	    mmap_fd, 0);

  if (p == MAP_FAILED)
    {
      if (errno != ENOMEM)
	fprintf (stderr, "mmap: %s\n", emacs_strerror (errno));
      p = NULL;
    }
  else
    {
      struct mmap_region *r = (struct mmap_region *) p;

      r->nbytes_specified = nbytes;
      r->nbytes_mapped = map;
      r->var = var;
      r->prev = NULL;
      r->next = mmap_regions;
      if (r->next)
	r->next->prev = r;
      mmap_regions = r;

      p = MMAP_USER_AREA (p);
    }

  return *var = p;
}


/* Free a block of relocatable storage whose data is pointed to by
   PTR.  Store 0 in *PTR to show there's no block allocated.  */

static void
mmap_free (POINTER_TYPE **var)
{
  mmap_init ();

  if (*var)
    {
      mmap_free_1 (MMAP_REGION (*var));
      *var = NULL;
    }
}


/* Given a pointer at address VAR to data allocated with mmap_alloc,
   resize it to size NBYTES.  Change *VAR to reflect the new block,
   and return this value.  If more memory cannot be allocated, then
   leave *VAR unchanged, and return null.  */

static POINTER_TYPE *
mmap_realloc (POINTER_TYPE **var, size_t nbytes)
{
  POINTER_TYPE *result;

  mmap_init ();

  if (*var == NULL)
    result = mmap_alloc (var, nbytes);
  else if (nbytes == 0)
    {
      mmap_free (var);
      result = mmap_alloc (var, nbytes);
    }
  else
    {
      struct mmap_region *r = MMAP_REGION (*var);
      size_t room = r->nbytes_mapped - MMAP_REGION_STRUCT_SIZE;

      if (room < nbytes)
	{
	  /* Must enlarge.  */
	  POINTER_TYPE *old_ptr = *var;

	  /* Try to map additional pages at the end of the region.
	     If that fails, allocate a new region,  copy data
	     from the old region, then free it.  */
	  if (mmap_enlarge (r, (ROUND (nbytes - room, mmap_page_size)
				/ mmap_page_size)))
	    {
	      r->nbytes_specified = nbytes;
	      *var = result = old_ptr;
	    }
	  else if (mmap_alloc (var, nbytes))
	    {
	      memcpy (*var, old_ptr, r->nbytes_specified);
	      mmap_free_1 (MMAP_REGION (old_ptr));
	      result = *var;
	      r = MMAP_REGION (result);
	      r->nbytes_specified = nbytes;
	    }
	  else
	    {
	      *var = old_ptr;
	      result = NULL;
	    }
	}
      else if (room - nbytes >= mmap_page_size)
	{
	  /* Shrinking by at least a page.  Let's give some
	     memory back to the system.

	     The extra parens are to make the division happens first,
	     on positive values, so we know it will round towards
	     zero.  */
	  mmap_enlarge (r, - ((room - nbytes) / mmap_page_size));
	  result = *var;
	  r->nbytes_specified = nbytes;
	}
      else
	{
	  /* Leave it alone.  */
	  result = *var;
	  r->nbytes_specified = nbytes;
	}
    }

  return result;
}


#endif /* USE_MMAP_FOR_BUFFERS */



/***********************************************************************
			    Buffer-text Allocation
 ***********************************************************************/

/* Allocate NBYTES bytes for buffer B's text buffer.  */

static void
alloc_buffer_text (struct buffer *b, ptrdiff_t nbytes)
{
  POINTER_TYPE *p;

  BLOCK_INPUT;
#if defined USE_MMAP_FOR_BUFFERS
  p = mmap_alloc ((POINTER_TYPE **) &b->text->beg, nbytes);
#elif defined REL_ALLOC
  p = r_alloc ((POINTER_TYPE **) &b->text->beg, nbytes);
#else
  p = xmalloc (nbytes);
#endif

  if (p == NULL)
    {
      UNBLOCK_INPUT;
      memory_full (nbytes);
    }

  b->text->beg = (unsigned char *) p;
  UNBLOCK_INPUT;
}

/* Enlarge buffer B's text buffer by DELTA bytes.  DELTA < 0 means
   shrink it.  */

void
enlarge_buffer_text (struct buffer *b, EMACS_INT delta)
{
  POINTER_TYPE *p;
  ptrdiff_t nbytes = (BUF_Z_BYTE (b) - BUF_BEG_BYTE (b) + BUF_GAP_SIZE (b) + 1
		      + delta);
  BLOCK_INPUT;
#if defined USE_MMAP_FOR_BUFFERS
  p = mmap_realloc ((POINTER_TYPE **) &b->text->beg, nbytes);
#elif defined REL_ALLOC
  p = r_re_alloc ((POINTER_TYPE **) &b->text->beg, nbytes);
#else
  p = xrealloc (b->text->beg, nbytes);
#endif

  if (p == NULL)
    {
      UNBLOCK_INPUT;
      memory_full (nbytes);
    }

  BUF_BEG_ADDR (b) = (unsigned char *) p;
  UNBLOCK_INPUT;
}


/* Free buffer B's text buffer.  */

static void
free_buffer_text (struct buffer *b)
{
  BLOCK_INPUT;

#if defined USE_MMAP_FOR_BUFFERS
  mmap_free ((POINTER_TYPE **) &b->text->beg);
#elif defined REL_ALLOC
  r_alloc_free ((POINTER_TYPE **) &b->text->beg);
#else
  xfree (b->text->beg);
#endif

  BUF_BEG_ADDR (b) = NULL;
  UNBLOCK_INPUT;
}



/***********************************************************************
			    Initialization
 ***********************************************************************/

void
init_buffer_once (void)
{
  int idx;

  memset (buffer_permanent_local_flags, 0, sizeof buffer_permanent_local_flags);

  /* Make sure all markable slots in buffer_defaults
     are initialized reasonably, so mark_buffer won't choke.  */
  reset_buffer (&buffer_defaults);
  eassert (EQ (BVAR (&buffer_defaults, name), make_number (0)));
  reset_buffer_local_variables (&buffer_defaults, 1);
  eassert (EQ (BVAR (&buffer_local_symbols, name), make_number (0)));
  reset_buffer (&buffer_local_symbols);
  reset_buffer_local_variables (&buffer_local_symbols, 1);
  /* Prevent GC from getting confused.  */
  buffer_defaults.text = &buffer_defaults.own_text;
  buffer_local_symbols.text = &buffer_local_symbols.own_text;
  BUF_INTERVALS (&buffer_defaults) = 0;
  BUF_INTERVALS (&buffer_local_symbols) = 0;
  XSETPVECTYPESIZE (&buffer_defaults, PVEC_BUFFER, 0);
  XSETBUFFER (Vbuffer_defaults, &buffer_defaults);
  XSETPVECTYPESIZE (&buffer_local_symbols, PVEC_BUFFER, 0);
  XSETBUFFER (Vbuffer_local_symbols, &buffer_local_symbols);

  /* Set up the default values of various buffer slots.  */
  /* Must do these before making the first buffer! */

  /* real setup is done in bindings.el */
  BVAR (&buffer_defaults, mode_line_format) = make_pure_c_string ("%-");
  BVAR (&buffer_defaults, header_line_format) = Qnil;
  BVAR (&buffer_defaults, abbrev_mode) = Qnil;
  BVAR (&buffer_defaults, overwrite_mode) = Qnil;
  BVAR (&buffer_defaults, case_fold_search) = Qt;
  BVAR (&buffer_defaults, auto_fill_function) = Qnil;
  BVAR (&buffer_defaults, selective_display) = Qnil;
#ifndef old
  BVAR (&buffer_defaults, selective_display_ellipses) = Qt;
#endif
  BVAR (&buffer_defaults, abbrev_table) = Qnil;
  BVAR (&buffer_defaults, display_table) = Qnil;
  BVAR (&buffer_defaults, undo_list) = Qnil;
  BVAR (&buffer_defaults, mark_active) = Qnil;
  BVAR (&buffer_defaults, file_format) = Qnil;
  BVAR (&buffer_defaults, auto_save_file_format) = Qt;
  buffer_defaults.overlays_before = NULL;
  buffer_defaults.overlays_after = NULL;
  buffer_defaults.overlay_center = BEG;

  XSETFASTINT (BVAR (&buffer_defaults, tab_width), 8);
  BVAR (&buffer_defaults, truncate_lines) = Qnil;
  BVAR (&buffer_defaults, word_wrap) = Qnil;
  BVAR (&buffer_defaults, ctl_arrow) = Qt;
  BVAR (&buffer_defaults, bidi_display_reordering) = Qt;
  BVAR (&buffer_defaults, bidi_paragraph_direction) = Qnil;
  BVAR (&buffer_defaults, cursor_type) = Qt;
  BVAR (&buffer_defaults, extra_line_spacing) = Qnil;
  BVAR (&buffer_defaults, cursor_in_non_selected_windows) = Qt;

  BVAR (&buffer_defaults, enable_multibyte_characters) = Qt;
  BVAR (&buffer_defaults, buffer_file_coding_system) = Qnil;
  XSETFASTINT (BVAR (&buffer_defaults, fill_column), 70);
  XSETFASTINT (BVAR (&buffer_defaults, left_margin), 0);
  BVAR (&buffer_defaults, cache_long_line_scans) = Qnil;
  BVAR (&buffer_defaults, file_truename) = Qnil;
  XSETFASTINT (BVAR (&buffer_defaults, display_count), 0);
  XSETFASTINT (BVAR (&buffer_defaults, left_margin_cols), 0);
  XSETFASTINT (BVAR (&buffer_defaults, right_margin_cols), 0);
  BVAR (&buffer_defaults, left_fringe_width) = Qnil;
  BVAR (&buffer_defaults, right_fringe_width) = Qnil;
  BVAR (&buffer_defaults, fringes_outside_margins) = Qnil;
  BVAR (&buffer_defaults, scroll_bar_width) = Qnil;
  BVAR (&buffer_defaults, vertical_scroll_bar_type) = Qt;
  BVAR (&buffer_defaults, indicate_empty_lines) = Qnil;
  BVAR (&buffer_defaults, indicate_buffer_boundaries) = Qnil;
  BVAR (&buffer_defaults, fringe_indicator_alist) = Qnil;
  BVAR (&buffer_defaults, fringe_cursor_alist) = Qnil;
  BVAR (&buffer_defaults, scroll_up_aggressively) = Qnil;
  BVAR (&buffer_defaults, scroll_down_aggressively) = Qnil;
  BVAR (&buffer_defaults, display_time) = Qnil;

  /* Assign the local-flags to the slots that have default values.
     The local flag is a bit that is used in the buffer
     to say that it has its own local value for the slot.
     The local flag bits are in the local_var_flags slot of the buffer.  */

  /* Nothing can work if this isn't true */
  { verify (sizeof (EMACS_INT) == sizeof (Lisp_Object)); }

  /* 0 means not a lisp var, -1 means always local, else mask */
  memset (&buffer_local_flags, 0, sizeof buffer_local_flags);
  XSETINT (BVAR (&buffer_local_flags, filename), -1);
  XSETINT (BVAR (&buffer_local_flags, directory), -1);
  XSETINT (BVAR (&buffer_local_flags, backed_up), -1);
  XSETINT (BVAR (&buffer_local_flags, save_length), -1);
  XSETINT (BVAR (&buffer_local_flags, auto_save_file_name), -1);
  XSETINT (BVAR (&buffer_local_flags, read_only), -1);
  XSETINT (BVAR (&buffer_local_flags, major_mode), -1);
  XSETINT (BVAR (&buffer_local_flags, mode_name), -1);
  XSETINT (BVAR (&buffer_local_flags, undo_list), -1);
  XSETINT (BVAR (&buffer_local_flags, mark_active), -1);
  XSETINT (BVAR (&buffer_local_flags, point_before_scroll), -1);
  XSETINT (BVAR (&buffer_local_flags, file_truename), -1);
  XSETINT (BVAR (&buffer_local_flags, invisibility_spec), -1);
  XSETINT (BVAR (&buffer_local_flags, file_format), -1);
  XSETINT (BVAR (&buffer_local_flags, auto_save_file_format), -1);
  XSETINT (BVAR (&buffer_local_flags, display_count), -1);
  XSETINT (BVAR (&buffer_local_flags, display_time), -1);
  XSETINT (BVAR (&buffer_local_flags, enable_multibyte_characters), -1);

  idx = 1;
  XSETFASTINT (BVAR (&buffer_local_flags, mode_line_format), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_local_flags, abbrev_mode), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_local_flags, overwrite_mode), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_local_flags, case_fold_search), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_local_flags, auto_fill_function), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_local_flags, selective_display), idx); ++idx;
#ifndef old
  XSETFASTINT (BVAR (&buffer_local_flags, selective_display_ellipses), idx); ++idx;
#endif
  XSETFASTINT (BVAR (&buffer_local_flags, tab_width), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_local_flags, truncate_lines), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_local_flags, word_wrap), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_local_flags, ctl_arrow), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_local_flags, fill_column), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_local_flags, left_margin), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_local_flags, abbrev_table), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_local_flags, display_table), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_local_flags, syntax_table), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_local_flags, cache_long_line_scans), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_local_flags, category_table), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_local_flags, bidi_display_reordering), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_local_flags, bidi_paragraph_direction), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_local_flags, buffer_file_coding_system), idx);
  /* Make this one a permanent local.  */
  buffer_permanent_local_flags[idx++] = 1;
  XSETFASTINT (BVAR (&buffer_local_flags, left_margin_cols), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_local_flags, right_margin_cols), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_local_flags, left_fringe_width), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_local_flags, right_fringe_width), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_local_flags, fringes_outside_margins), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_local_flags, scroll_bar_width), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_local_flags, vertical_scroll_bar_type), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_local_flags, indicate_empty_lines), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_local_flags, indicate_buffer_boundaries), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_local_flags, fringe_indicator_alist), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_local_flags, fringe_cursor_alist), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_local_flags, scroll_up_aggressively), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_local_flags, scroll_down_aggressively), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_local_flags, header_line_format), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_local_flags, cursor_type), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_local_flags, extra_line_spacing), idx); ++idx;
  XSETFASTINT (BVAR (&buffer_local_flags, cursor_in_non_selected_windows), idx); ++idx;

  /* Need more room? */
  if (idx >= MAX_PER_BUFFER_VARS)
    abort ();
  last_per_buffer_idx = idx;

  Vbuffer_alist = Qnil;
  current_buffer = 0;
  all_buffers = 0;

  QSFundamental = make_pure_c_string ("Fundamental");

  Qfundamental_mode = intern_c_string ("fundamental-mode");
  BVAR (&buffer_defaults, major_mode) = Qfundamental_mode;

  Qmode_class = intern_c_string ("mode-class");

  Qprotected_field = intern_c_string ("protected-field");

  Qpermanent_local = intern_c_string ("permanent-local");

  Qkill_buffer_hook = intern_c_string ("kill-buffer-hook");
  Fput (Qkill_buffer_hook, Qpermanent_local, Qt);

  Qucs_set_table_for_input = intern_c_string ("ucs-set-table-for-input");

  /* super-magic invisible buffer */
  Vprin1_to_string_buffer = Fget_buffer_create (make_pure_c_string (" prin1"));
  Vbuffer_alist = Qnil;

  Fset_buffer (Fget_buffer_create (make_pure_c_string ("*scratch*")));

  inhibit_modification_hooks = 0;
}

void
init_buffer (void)
{
  char *pwd;
  Lisp_Object temp;
  ptrdiff_t len;

#ifdef USE_MMAP_FOR_BUFFERS
 {
   /* When using the ralloc implementation based on mmap(2), buffer
      text pointers will have been set to null in the dumped Emacs.
      Map new memory.  */
   struct buffer *b;

   for (b = all_buffers; b; b = b->header.next.buffer)
     if (b->text->beg == NULL)
       enlarge_buffer_text (b, 0);
 }
#endif /* USE_MMAP_FOR_BUFFERS */

  Fset_buffer (Fget_buffer_create (build_string ("*scratch*")));
  if (NILP (BVAR (&buffer_defaults, enable_multibyte_characters)))
    Fset_buffer_multibyte (Qnil);

  pwd = get_current_dir_name ();

  if (!pwd)
    fatal ("`get_current_dir_name' failed: %s\n", strerror (errno));

  /* Maybe this should really use some standard subroutine
     whose definition is filename syntax dependent.  */
  len = strlen (pwd);
  if (!(IS_DIRECTORY_SEP (pwd[len - 1])))
    {
      /* Grow buffer to add directory separator and '\0'.  */
      pwd = (char *) realloc (pwd, len + 2);
      if (!pwd)
	fatal ("`get_current_dir_name' failed: %s\n", strerror (errno));
      pwd[len] = DIRECTORY_SEP;
      pwd[len + 1] = '\0';
    }

  BVAR (current_buffer, directory) = make_unibyte_string (pwd, strlen (pwd));
  if (! NILP (BVAR (&buffer_defaults, enable_multibyte_characters)))
    /* At this moment, we still don't know how to decode the
       directory name.  So, we keep the bytes in multibyte form so
       that ENCODE_FILE correctly gets the original bytes.  */
    BVAR (current_buffer, directory)
      = string_to_multibyte (BVAR (current_buffer, directory));

  /* Add /: to the front of the name
     if it would otherwise be treated as magic.  */
  temp = Ffind_file_name_handler (BVAR (current_buffer, directory), Qt);
  if (! NILP (temp)
      /* If the default dir is just /, TEMP is non-nil
	 because of the ange-ftp completion handler.
	 However, it is not necessary to turn / into /:/.
	 So avoid doing that.  */
      && strcmp ("/", SSDATA (BVAR (current_buffer, directory))))
    BVAR (current_buffer, directory)
      = concat2 (build_string ("/:"), BVAR (current_buffer, directory));

  temp = get_minibuffer (0);
  BVAR (XBUFFER (temp), directory) = BVAR (current_buffer, directory);

  free (pwd);
}

/* Similar to defvar_lisp but define a variable whose value is the Lisp
   Object stored in the current buffer.  address is the address of the slot
   in the buffer that is current now. */

/* TYPE is nil for a general Lisp variable.
   An integer specifies a type; then only Lisp values
   with that type code are allowed (except that nil is allowed too).
   LNAME is the Lisp-level variable name.
   VNAME is the name of the buffer slot.
   DOC is a dummy where you write the doc string as a comment.  */
#define DEFVAR_PER_BUFFER(lname, vname, type, doc)			\
  do {									\
    static struct Lisp_Buffer_Objfwd bo_fwd;				\
    defvar_per_buffer (&bo_fwd, lname, vname, type);			\
  } while (0)

static void
defvar_per_buffer (struct Lisp_Buffer_Objfwd *bo_fwd, const char *namestring,
		   Lisp_Object *address, Lisp_Object type)
{
  struct Lisp_Symbol *sym;
  int offset;

  sym = XSYMBOL (intern (namestring));
  offset = (char *)address - (char *)current_buffer;

  bo_fwd->type = Lisp_Fwd_Buffer_Obj;
  bo_fwd->offset = offset;
  bo_fwd->slottype = type;
  sym->declared_special = 1;
  sym->redirect = SYMBOL_FORWARDED;
  {
    /* I tried to do the job without a cast, but it seems impossible.
       union Lisp_Fwd *fwd; &(fwd->u_buffer_objfwd) = bo_fwd;  */
    SET_SYMBOL_FWD (sym, (union Lisp_Fwd *)bo_fwd);
  }
  XSETSYMBOL (PER_BUFFER_SYMBOL (offset), sym);

  if (PER_BUFFER_IDX (offset) == 0)
    /* Did a DEFVAR_PER_BUFFER without initializing the corresponding
       slot of buffer_local_flags */
    abort ();
}


/* initialize the buffer routines */
void
syms_of_buffer (void)
{
  staticpro (&last_overlay_modification_hooks);
  last_overlay_modification_hooks
    = Fmake_vector (make_number (10), Qnil);

  staticpro (&Vbuffer_defaults);
  staticpro (&Vbuffer_local_symbols);
  staticpro (&Qfundamental_mode);
  staticpro (&Qmode_class);
  staticpro (&QSFundamental);
  staticpro (&Vbuffer_alist);
  staticpro (&Qprotected_field);
  staticpro (&Qpermanent_local);
  staticpro (&Qkill_buffer_hook);

  DEFSYM (Qpermanent_local_hook, "permanent-local-hook");
  DEFSYM (Qoverlayp, "overlayp");
  DEFSYM (Qevaporate, "evaporate");
  DEFSYM (Qmodification_hooks, "modification-hooks");
  DEFSYM (Qinsert_in_front_hooks, "insert-in-front-hooks");
  DEFSYM (Qinsert_behind_hooks, "insert-behind-hooks");
  DEFSYM (Qget_file_buffer, "get-file-buffer");
  DEFSYM (Qpriority, "priority");
  DEFSYM (Qbefore_string, "before-string");
  DEFSYM (Qafter_string, "after-string");
  DEFSYM (Qfirst_change_hook, "first-change-hook");
  DEFSYM (Qbefore_change_functions, "before-change-functions");
  DEFSYM (Qafter_change_functions, "after-change-functions");
  DEFSYM (Qkill_buffer_query_functions, "kill-buffer-query-functions");

  /* The next one is initialized in init_buffer_once.  */
  staticpro (&Qucs_set_table_for_input);

  Fput (Qprotected_field, Qerror_conditions,
	pure_cons (Qprotected_field, pure_cons (Qerror, Qnil)));
  Fput (Qprotected_field, Qerror_message,
	make_pure_c_string ("Attempt to modify a protected field"));

  DEFVAR_BUFFER_DEFAULTS ("default-mode-line-format",
			  mode_line_format,
			  doc: /* Default value of `mode-line-format' for buffers that don't override it.
This is the same as (default-value 'mode-line-format).  */);

  DEFVAR_BUFFER_DEFAULTS ("default-header-line-format",
			  header_line_format,
			  doc: /* Default value of `header-line-format' for buffers that don't override it.
This is the same as (default-value 'header-line-format).  */);

  DEFVAR_BUFFER_DEFAULTS ("default-cursor-type", cursor_type,
			  doc: /* Default value of `cursor-type' for buffers that don't override it.
This is the same as (default-value 'cursor-type).  */);

  DEFVAR_BUFFER_DEFAULTS ("default-line-spacing",
			  extra_line_spacing,
			  doc: /* Default value of `line-spacing' for buffers that don't override it.
This is the same as (default-value 'line-spacing).  */);

  DEFVAR_BUFFER_DEFAULTS ("default-cursor-in-non-selected-windows",
			  cursor_in_non_selected_windows,
			  doc: /* Default value of `cursor-in-non-selected-windows'.
This is the same as (default-value 'cursor-in-non-selected-windows).  */);

  DEFVAR_BUFFER_DEFAULTS ("default-abbrev-mode",
			  abbrev_mode,
			  doc: /* Default value of `abbrev-mode' for buffers that do not override it.
This is the same as (default-value 'abbrev-mode).  */);

  DEFVAR_BUFFER_DEFAULTS ("default-ctl-arrow",
			  ctl_arrow,
			  doc: /* Default value of `ctl-arrow' for buffers that do not override it.
This is the same as (default-value 'ctl-arrow).  */);

  DEFVAR_BUFFER_DEFAULTS ("default-enable-multibyte-characters",
			  enable_multibyte_characters,
			  doc: /* *Default value of `enable-multibyte-characters' for buffers not overriding it.
This is the same as (default-value 'enable-multibyte-characters).  */);

  DEFVAR_BUFFER_DEFAULTS ("default-buffer-file-coding-system",
			  buffer_file_coding_system,
			  doc: /* Default value of `buffer-file-coding-system' for buffers not overriding it.
This is the same as (default-value 'buffer-file-coding-system).  */);

  DEFVAR_BUFFER_DEFAULTS ("default-truncate-lines",
			  truncate_lines,
			  doc: /* Default value of `truncate-lines' for buffers that do not override it.
This is the same as (default-value 'truncate-lines).  */);

  DEFVAR_BUFFER_DEFAULTS ("default-fill-column",
			  fill_column,
			  doc: /* Default value of `fill-column' for buffers that do not override it.
This is the same as (default-value 'fill-column).  */);

  DEFVAR_BUFFER_DEFAULTS ("default-left-margin",
			  left_margin,
			  doc: /* Default value of `left-margin' for buffers that do not override it.
This is the same as (default-value 'left-margin).  */);

  DEFVAR_BUFFER_DEFAULTS ("default-tab-width",
			  tab_width,
			  doc: /* Default value of `tab-width' for buffers that do not override it.
This is the same as (default-value 'tab-width).  */);

  DEFVAR_BUFFER_DEFAULTS ("default-case-fold-search",
			  case_fold_search,
			  doc: /* Default value of `case-fold-search' for buffers that don't override it.
This is the same as (default-value 'case-fold-search).  */);

  DEFVAR_BUFFER_DEFAULTS ("default-left-margin-width",
			  left_margin_cols,
			  doc: /* Default value of `left-margin-width' for buffers that don't override it.
This is the same as (default-value 'left-margin-width).  */);

  DEFVAR_BUFFER_DEFAULTS ("default-right-margin-width",
			  right_margin_cols,
			  doc: /* Default value of `right-margin-width' for buffers that don't override it.
This is the same as (default-value 'right-margin-width).  */);

  DEFVAR_BUFFER_DEFAULTS ("default-left-fringe-width",
			  left_fringe_width,
			  doc: /* Default value of `left-fringe-width' for buffers that don't override it.
This is the same as (default-value 'left-fringe-width).  */);

  DEFVAR_BUFFER_DEFAULTS ("default-right-fringe-width",
			  right_fringe_width,
			  doc: /* Default value of `right-fringe-width' for buffers that don't override it.
This is the same as (default-value 'right-fringe-width).  */);

  DEFVAR_BUFFER_DEFAULTS ("default-fringes-outside-margins",
			  fringes_outside_margins,
			  doc: /* Default value of `fringes-outside-margins' for buffers that don't override it.
This is the same as (default-value 'fringes-outside-margins).  */);

  DEFVAR_BUFFER_DEFAULTS ("default-scroll-bar-width",
			  scroll_bar_width,
			  doc: /* Default value of `scroll-bar-width' for buffers that don't override it.
This is the same as (default-value 'scroll-bar-width).  */);

  DEFVAR_BUFFER_DEFAULTS ("default-vertical-scroll-bar",
			  vertical_scroll_bar_type,
			  doc: /* Default value of `vertical-scroll-bar' for buffers that don't override it.
This is the same as (default-value 'vertical-scroll-bar).  */);

  DEFVAR_BUFFER_DEFAULTS ("default-indicate-empty-lines",
			  indicate_empty_lines,
			  doc: /* Default value of `indicate-empty-lines' for buffers that don't override it.
This is the same as (default-value 'indicate-empty-lines).  */);

  DEFVAR_BUFFER_DEFAULTS ("default-indicate-buffer-boundaries",
			  indicate_buffer_boundaries,
			  doc: /* Default value of `indicate-buffer-boundaries' for buffers that don't override it.
This is the same as (default-value 'indicate-buffer-boundaries).  */);

  DEFVAR_BUFFER_DEFAULTS ("default-fringe-indicator-alist",
			  fringe_indicator_alist,
			  doc: /* Default value of `fringe-indicator-alist' for buffers that don't override it.
This is the same as (default-value 'fringe-indicator-alist').  */);

  DEFVAR_BUFFER_DEFAULTS ("default-fringe-cursor-alist",
			  fringe_cursor_alist,
			  doc: /* Default value of `fringe-cursor-alist' for buffers that don't override it.
This is the same as (default-value 'fringe-cursor-alist').  */);

  DEFVAR_BUFFER_DEFAULTS ("default-scroll-up-aggressively",
			  scroll_up_aggressively,
			  doc: /* Default value of `scroll-up-aggressively'.
This value applies in buffers that don't have their own local values.
This is the same as (default-value 'scroll-up-aggressively).  */);

  DEFVAR_BUFFER_DEFAULTS ("default-scroll-down-aggressively",
			  scroll_down_aggressively,
			  doc: /* Default value of `scroll-down-aggressively'.
This value applies in buffers that don't have their own local values.
This is the same as (default-value 'scroll-down-aggressively).  */);

  DEFVAR_PER_BUFFER ("header-line-format",
		     &BVAR (current_buffer, header_line_format),
		     Qnil,
		     doc: /* Analogous to `mode-line-format', but controls the header line.
The header line appears, optionally, at the top of a window;
the mode line appears at the bottom.  */);

  DEFVAR_PER_BUFFER ("mode-line-format", &BVAR (current_buffer, mode_line_format),
		     Qnil,
		     doc: /* Template for displaying mode line for current buffer.
Each buffer has its own value of this variable.
Value may be nil, a string, a symbol or a list or cons cell.
A value of nil means don't display a mode line.
For a symbol, its value is used (but it is ignored if t or nil).
 A string appearing directly as the value of a symbol is processed verbatim
 in that the %-constructs below are not recognized.
 Note that unless the symbol is marked as a `risky-local-variable', all
 properties in any strings, as well as all :eval and :propertize forms
 in the value of that symbol will be ignored.
For a list of the form `(:eval FORM)', FORM is evaluated and the result
 is used as a mode line element.  Be careful--FORM should not load any files,
 because that can cause an infinite recursion.
For a list of the form `(:propertize ELT PROPS...)', ELT is displayed
 with the specified properties PROPS applied.
For a list whose car is a symbol, the symbol's value is taken,
 and if that is non-nil, the cadr of the list is processed recursively.
 Otherwise, the caddr of the list (if there is one) is processed.
For a list whose car is a string or list, each element is processed
 recursively and the results are effectively concatenated.
For a list whose car is an integer, the cdr of the list is processed
  and padded (if the number is positive) or truncated (if negative)
  to the width specified by that number.
A string is printed verbatim in the mode line except for %-constructs:
  (%-constructs are allowed when the string is the entire mode-line-format
   or when it is found in a cons-cell or a list)
  %b -- print buffer name.      %f -- print visited file name.
  %F -- print frame name.
  %* -- print %, * or hyphen.   %+ -- print *, % or hyphen.
	%& is like %*, but ignore read-only-ness.
	% means buffer is read-only and * means it is modified.
	For a modified read-only buffer, %* gives % and %+ gives *.
  %s -- print process status.   %l -- print the current line number.
  %c -- print the current column number (this makes editing slower).
        To make the column number update correctly in all cases,
	`column-number-mode' must be non-nil.
  %i -- print the size of the buffer.
  %I -- like %i, but use k, M, G, etc., to abbreviate.
  %p -- print percent of buffer above top of window, or Top, Bot or All.
  %P -- print percent of buffer above bottom of window, perhaps plus Top,
        or print Bottom or All.
  %n -- print Narrow if appropriate.
  %t -- visited file is text or binary (if OS supports this distinction).
  %z -- print mnemonics of keyboard, terminal, and buffer coding systems.
  %Z -- like %z, but including the end-of-line format.
  %e -- print error message about full memory.
  %@ -- print @ or hyphen.  @ means that default-directory is on a
        remote machine.
  %[ -- print one [ for each recursive editing level.  %] similar.
  %% -- print %.   %- -- print infinitely many dashes.
Decimal digits after the % specify field width to which to pad.  */);

  DEFVAR_BUFFER_DEFAULTS ("default-major-mode", major_mode,
			  doc: /* *Value of `major-mode' for new buffers.  */);

  DEFVAR_PER_BUFFER ("major-mode", &BVAR (current_buffer, major_mode),
		     make_number (Lisp_Symbol),
		     doc: /* Symbol for current buffer's major mode.
The default value (normally `fundamental-mode') affects new buffers.
A value of nil means to use the current buffer's major mode, provided
it is not marked as "special".

When a mode is used by default, `find-file' switches to it before it
reads the contents into the buffer and before it finishes setting up
the buffer.  Thus, the mode and its hooks should not expect certain
variables such as `buffer-read-only' and `buffer-file-coding-system'
to be set up.  */);

  DEFVAR_PER_BUFFER ("mode-name", &BVAR (current_buffer, mode_name),
                     Qnil,
		     doc: /* Pretty name of current buffer's major mode.
Usually a string, but can use any of the constructs for `mode-line-format',
which see.
Format with `format-mode-line' to produce a string value.  */);

  DEFVAR_PER_BUFFER ("local-abbrev-table", &BVAR (current_buffer, abbrev_table), Qnil,
		     doc: /* Local (mode-specific) abbrev table of current buffer.  */);

  DEFVAR_PER_BUFFER ("abbrev-mode", &BVAR (current_buffer, abbrev_mode), Qnil,
		     doc: /*  Non-nil if Abbrev mode is enabled.
Use the command `abbrev-mode' to change this variable.  */);

  DEFVAR_PER_BUFFER ("case-fold-search", &BVAR (current_buffer, case_fold_search),
		     Qnil,
		     doc: /* *Non-nil if searches and matches should ignore case.  */);

  DEFVAR_PER_BUFFER ("fill-column", &BVAR (current_buffer, fill_column),
		     make_number (LISP_INT_TAG),
		     doc: /* *Column beyond which automatic line-wrapping should happen.
Interactively, you can set the buffer local value using \\[set-fill-column].  */);

  DEFVAR_PER_BUFFER ("left-margin", &BVAR (current_buffer, left_margin),
		     make_number (LISP_INT_TAG),
		     doc: /* *Column for the default `indent-line-function' to indent to.
Linefeed indents to this column in Fundamental mode.  */);

  DEFVAR_PER_BUFFER ("tab-width", &BVAR (current_buffer, tab_width),
		     make_number (LISP_INT_TAG),
		     doc: /* *Distance between tab stops (for display of tab characters), in columns.
This should be an integer greater than zero.  */);

  DEFVAR_PER_BUFFER ("ctl-arrow", &BVAR (current_buffer, ctl_arrow), Qnil,
		     doc: /* *Non-nil means display control chars with uparrow.
A value of nil means use backslash and octal digits.
This variable does not apply to characters whose display is specified
in the current display table (if there is one).  */);

  DEFVAR_PER_BUFFER ("enable-multibyte-characters",
		     &BVAR (current_buffer, enable_multibyte_characters),
		     Qnil,
		     doc: /* Non-nil means the buffer contents are regarded as multi-byte characters.
Otherwise they are regarded as unibyte.  This affects the display,
file I/O and the behavior of various editing commands.

This variable is buffer-local but you cannot set it directly;
use the function `set-buffer-multibyte' to change a buffer's representation.
See also Info node `(elisp)Text Representations'.  */);
  XSYMBOL (intern_c_string ("enable-multibyte-characters"))->constant = 1;

  DEFVAR_PER_BUFFER ("buffer-file-coding-system",
		     &BVAR (current_buffer, buffer_file_coding_system), Qnil,
		     doc: /* Coding system to be used for encoding the buffer contents on saving.
This variable applies to saving the buffer, and also to `write-region'
and other functions that use `write-region'.
It does not apply to sending output to subprocesses, however.

If this is nil, the buffer is saved without any code conversion
unless some coding system is specified in `file-coding-system-alist'
for the buffer file.

If the text to be saved cannot be encoded as specified by this variable,
an alternative encoding is selected by `select-safe-coding-system', which see.

The variable `coding-system-for-write', if non-nil, overrides this variable.

This variable is never applied to a way of decoding a file while reading it.  */);

  DEFVAR_PER_BUFFER ("bidi-display-reordering",
		     &BVAR (current_buffer, bidi_display_reordering), Qnil,
		     doc: /* Non-nil means reorder bidirectional text for display in the visual order.  */);

  DEFVAR_PER_BUFFER ("bidi-paragraph-direction",
		     &BVAR (current_buffer, bidi_paragraph_direction), Qnil,
		     doc: /* *If non-nil, forces directionality of text paragraphs in the buffer.

If this is nil (the default), the direction of each paragraph is
determined by the first strong directional character of its text.
The values of `right-to-left' and `left-to-right' override that.
Any other value is treated as nil.

This variable has no effect unless the buffer's value of
\`bidi-display-reordering' is non-nil.  */);

 DEFVAR_PER_BUFFER ("truncate-lines", &BVAR (current_buffer, truncate_lines), Qnil,
		     doc: /* *Non-nil means do not display continuation lines.
Instead, give each line of text just one screen line.

Note that this is overridden by the variable
`truncate-partial-width-windows' if that variable is non-nil
and this buffer is not full-frame width.

Minibuffers set this variable to nil.  */);

  DEFVAR_PER_BUFFER ("word-wrap", &BVAR (current_buffer, word_wrap), Qnil,
		     doc: /* *Non-nil means to use word-wrapping for continuation lines.
When word-wrapping is on, continuation lines are wrapped at the space
or tab character nearest to the right window edge.
If nil, continuation lines are wrapped at the right screen edge.

This variable has no effect if long lines are truncated (see
`truncate-lines' and `truncate-partial-width-windows').  If you use
word-wrapping, you might want to reduce the value of
`truncate-partial-width-windows', since wrapping can make text readable
in narrower windows.  */);

  DEFVAR_PER_BUFFER ("default-directory", &BVAR (current_buffer, directory),
		     make_number (Lisp_String),
		     doc: /* Name of default directory of current buffer.  Should end with slash.
To interactively change the default directory, use command `cd'.  */);

  DEFVAR_PER_BUFFER ("auto-fill-function", &BVAR (current_buffer, auto_fill_function),
		     Qnil,
		     doc: /* Function called (if non-nil) to perform auto-fill.
It is called after self-inserting any character specified in
the `auto-fill-chars' table.
NOTE: This variable is not a hook;
its value may not be a list of functions.  */);

  DEFVAR_PER_BUFFER ("buffer-file-name", &BVAR (current_buffer, filename),
		     make_number (Lisp_String),
		     doc: /* Name of file visited in current buffer, or nil if not visiting a file.  */);

  DEFVAR_PER_BUFFER ("buffer-file-truename", &BVAR (current_buffer, file_truename),
		     make_number (Lisp_String),
		     doc: /* Abbreviated truename of file visited in current buffer, or nil if none.
The truename of a file is calculated by `file-truename'
and then abbreviated with `abbreviate-file-name'.  */);

  DEFVAR_PER_BUFFER ("buffer-auto-save-file-name",
		     &BVAR (current_buffer, auto_save_file_name),
		     make_number (Lisp_String),
		     doc: /* Name of file for auto-saving current buffer.
If it is nil, that means don't auto-save this buffer.  */);

  DEFVAR_PER_BUFFER ("buffer-read-only", &BVAR (current_buffer, read_only), Qnil,
		     doc: /* Non-nil if this buffer is read-only.  */);

  DEFVAR_PER_BUFFER ("buffer-backed-up", &BVAR (current_buffer, backed_up), Qnil,
		     doc: /* Non-nil if this buffer's file has been backed up.
Backing up is done before the first time the file is saved.  */);

  DEFVAR_PER_BUFFER ("buffer-saved-size", &BVAR (current_buffer, save_length),
		     make_number (LISP_INT_TAG),
		     doc: /* Length of current buffer when last read in, saved or auto-saved.
0 initially.
-1 means auto-saving turned off until next real save.

If you set this to -2, that means don't turn off auto-saving in this buffer
if its text size shrinks.   If you use `buffer-swap-text' on a buffer,
you probably should set this to -2 in that buffer.  */);

  DEFVAR_PER_BUFFER ("selective-display", &BVAR (current_buffer, selective_display),
		     Qnil,
		     doc: /* Non-nil enables selective display.
An integer N as value means display only lines
that start with less than N columns of space.
A value of t means that the character ^M makes itself and
all the rest of the line invisible; also, when saving the buffer
in a file, save the ^M as a newline.  */);

#ifndef old
  DEFVAR_PER_BUFFER ("selective-display-ellipses",
		     &BVAR (current_buffer, selective_display_ellipses),
		     Qnil,
		     doc: /* Non-nil means display ... on previous line when a line is invisible.  */);
#endif

  DEFVAR_PER_BUFFER ("overwrite-mode", &BVAR (current_buffer, overwrite_mode), Qnil,
		     doc: /* Non-nil if self-insertion should replace existing text.
The value should be one of `overwrite-mode-textual',
`overwrite-mode-binary', or nil.
If it is `overwrite-mode-textual', self-insertion still
inserts at the end of a line, and inserts when point is before a tab,
until the tab is filled in.
If `overwrite-mode-binary', self-insertion replaces newlines and tabs too.  */);

  DEFVAR_PER_BUFFER ("buffer-display-table", &BVAR (current_buffer, display_table),
		     Qnil,
		     doc: /* Display table that controls display of the contents of current buffer.

If this variable is nil, the value of `standard-display-table' is used.
Each window can have its own, overriding display table, see
`set-window-display-table' and `window-display-table'.

The display table is a char-table created with `make-display-table'.
A char-table is an array indexed by character codes.  Normal array
primitives `aref' and `aset' can be used to access elements of a char-table.

Each of the char-table elements control how to display the corresponding
text character: the element at index C in the table says how to display
the character whose code is C.  Each element should be a vector of
characters or nil.  The value nil means display the character in the
default fashion; otherwise, the characters from the vector are delivered
to the screen instead of the original character.

For example, (aset buffer-display-table ?X [?Y]) tells Emacs
to display a capital Y instead of each X character.

In addition, a char-table has six extra slots to control the display of:

  the end of a truncated screen line (extra-slot 0, a single character);
  the end of a continued line (extra-slot 1, a single character);
  the escape character used to display character codes in octal
    (extra-slot 2, a single character);
  the character used as an arrow for control characters (extra-slot 3,
    a single character);
  the decoration indicating the presence of invisible lines (extra-slot 4,
    a vector of characters);
  the character used to draw the border between side-by-side windows
    (extra-slot 5, a single character).

See also the functions `display-table-slot' and `set-display-table-slot'.  */);

  DEFVAR_PER_BUFFER ("left-margin-width", &BVAR (current_buffer, left_margin_cols),
		     Qnil,
		     doc: /* *Width of left marginal area for display of a buffer.
A value of nil means no marginal area.  */);

  DEFVAR_PER_BUFFER ("right-margin-width", &BVAR (current_buffer, right_margin_cols),
		     Qnil,
		     doc: /* *Width of right marginal area for display of a buffer.
A value of nil means no marginal area.  */);

  DEFVAR_PER_BUFFER ("left-fringe-width", &BVAR (current_buffer, left_fringe_width),
		     Qnil,
		     doc: /* *Width of this buffer's left fringe (in pixels).
A value of 0 means no left fringe is shown in this buffer's window.
A value of nil means to use the left fringe width from the window's frame.  */);

  DEFVAR_PER_BUFFER ("right-fringe-width", &BVAR (current_buffer, right_fringe_width),
		     Qnil,
		     doc: /* *Width of this buffer's right fringe (in pixels).
A value of 0 means no right fringe is shown in this buffer's window.
A value of nil means to use the right fringe width from the window's frame.  */);

  DEFVAR_PER_BUFFER ("fringes-outside-margins", &BVAR (current_buffer, fringes_outside_margins),
		     Qnil,
		     doc: /* *Non-nil means to display fringes outside display margins.
A value of nil means to display fringes between margins and buffer text.  */);

  DEFVAR_PER_BUFFER ("scroll-bar-width", &BVAR (current_buffer, scroll_bar_width),
		     Qnil,
		     doc: /* *Width of this buffer's scroll bars in pixels.
A value of nil means to use the scroll bar width from the window's frame.  */);

  DEFVAR_PER_BUFFER ("vertical-scroll-bar", &BVAR (current_buffer, vertical_scroll_bar_type),
		     Qnil,
		     doc: /* *Position of this buffer's vertical scroll bar.
The value takes effect whenever you tell a window to display this buffer;
for instance, with `set-window-buffer' or when `display-buffer' displays it.

A value of `left' or `right' means put the vertical scroll bar at that side
of the window; a value of nil means don't show any vertical scroll bars.
A value of t (the default) means do whatever the window's frame specifies.  */);

  DEFVAR_PER_BUFFER ("indicate-empty-lines",
		     &BVAR (current_buffer, indicate_empty_lines), Qnil,
		     doc: /* *Visually indicate empty lines after the buffer end.
If non-nil, a bitmap is displayed in the left fringe of a window on
window-systems.  */);

  DEFVAR_PER_BUFFER ("indicate-buffer-boundaries",
		     &BVAR (current_buffer, indicate_buffer_boundaries), Qnil,
		     doc: /* *Visually indicate buffer boundaries and scrolling.
If non-nil, the first and last line of the buffer are marked in the fringe
of a window on window-systems with angle bitmaps, or if the window can be
scrolled, the top and bottom line of the window are marked with up and down
arrow bitmaps.

If value is a symbol `left' or `right', both angle and arrow bitmaps
are displayed in the left or right fringe, resp.  Any other value
that doesn't look like an alist means display the angle bitmaps in
the left fringe but no arrows.

You can exercise more precise control by using an alist as the
value.  Each alist element (INDICATOR . POSITION) specifies
where to show one of the indicators.  INDICATOR is one of `top',
`bottom', `up', `down', or t, which specifies the default position,
and POSITION is one of `left', `right', or nil, meaning do not show
this indicator.

For example, ((top . left) (t . right)) places the top angle bitmap in
left fringe, the bottom angle bitmap in right fringe, and both arrow
bitmaps in right fringe.  To show just the angle bitmaps in the left
fringe, but no arrow bitmaps, use ((top .  left) (bottom . left)).  */);

  DEFVAR_PER_BUFFER ("fringe-indicator-alist",
		     &BVAR (current_buffer, fringe_indicator_alist), Qnil,
		     doc: /* *Mapping from logical to physical fringe indicator bitmaps.
The value is an alist where each element (INDICATOR . BITMAPS)
specifies the fringe bitmaps used to display a specific logical
fringe indicator.

INDICATOR specifies the logical indicator type which is one of the
following symbols: `truncation' , `continuation', `overlay-arrow',
`top', `bottom', `top-bottom', `up', `down', empty-line', or `unknown'.

BITMAPS is a list of symbols (LEFT RIGHT [LEFT1 RIGHT1]) which specifies
the actual bitmap shown in the left or right fringe for the logical
indicator.  LEFT and RIGHT are the bitmaps shown in the left and/or
right fringe for the specific indicator.  The LEFT1 or RIGHT1 bitmaps
are used only for the `bottom' and `top-bottom' indicators when the
last (only) line has no final newline.  BITMAPS may also be a single
symbol which is used in both left and right fringes.  */);

  DEFVAR_PER_BUFFER ("fringe-cursor-alist",
		     &BVAR (current_buffer, fringe_cursor_alist), Qnil,
		     doc: /* *Mapping from logical to physical fringe cursor bitmaps.
The value is an alist where each element (CURSOR . BITMAP)
specifies the fringe bitmaps used to display a specific logical
cursor type in the fringe.

CURSOR specifies the logical cursor type which is one of the following
symbols: `box' , `hollow', `bar', `hbar', or `hollow-small'.  The last
one is used to show a hollow cursor on narrow lines display lines
where the normal hollow cursor will not fit.

BITMAP is the corresponding fringe bitmap shown for the logical
cursor type.  */);

  DEFVAR_PER_BUFFER ("scroll-up-aggressively",
		     &BVAR (current_buffer, scroll_up_aggressively), Qnil,
		     doc: /* How far to scroll windows upward.
If you move point off the bottom, the window scrolls automatically.
This variable controls how far it scrolls.  The value nil, the default,
means scroll to center point.  A fraction means scroll to put point
that fraction of the window's height from the bottom of the window.
When the value is 0.0, point goes at the bottom line, which in the
simple case that you moved off with C-f means scrolling just one line.
1.0 means point goes at the top, so that in that simple case, the
window scrolls by a full window height.  Meaningful values are
between 0.0 and 1.0, inclusive.  */);

  DEFVAR_PER_BUFFER ("scroll-down-aggressively",
		     &BVAR (current_buffer, scroll_down_aggressively), Qnil,
		     doc: /* How far to scroll windows downward.
If you move point off the top, the window scrolls automatically.
This variable controls how far it scrolls.  The value nil, the default,
means scroll to center point.  A fraction means scroll to put point
that fraction of the window's height from the top of the window.
When the value is 0.0, point goes at the top line, which in the
simple case that you moved off with C-b means scrolling just one line.
1.0 means point goes at the bottom, so that in that simple case, the
window scrolls by a full window height.  Meaningful values are
between 0.0 and 1.0, inclusive.  */);

/*DEFVAR_LISP ("debug-check-symbol", &Vcheck_symbol,
    "Don't ask.");
*/

  DEFVAR_LISP ("before-change-functions", Vbefore_change_functions,
	       doc: /* List of functions to call before each text change.
Two arguments are passed to each function: the positions of
the beginning and end of the range of old text to be changed.
\(For an insertion, the beginning and end are at the same place.)
No information is given about the length of the text after the change.

Buffer changes made while executing the `before-change-functions'
don't call any before-change or after-change functions.
That's because `inhibit-modification-hooks' is temporarily set non-nil.

If an unhandled error happens in running these functions,
the variable's value remains nil.  That prevents the error
from happening repeatedly and making Emacs nonfunctional.  */);
  Vbefore_change_functions = Qnil;

  DEFVAR_LISP ("after-change-functions", Vafter_change_functions,
	       doc: /* List of functions to call after each text change.
Three arguments are passed to each function: the positions of
the beginning and end of the range of changed text,
and the length in bytes of the pre-change text replaced by that range.
\(For an insertion, the pre-change length is zero;
for a deletion, that length is the number of bytes deleted,
and the post-change beginning and end are at the same place.)

Buffer changes made while executing the `after-change-functions'
don't call any before-change or after-change functions.
That's because `inhibit-modification-hooks' is temporarily set non-nil.

If an unhandled error happens in running these functions,
the variable's value remains nil.  That prevents the error
from happening repeatedly and making Emacs nonfunctional.  */);
  Vafter_change_functions = Qnil;

  DEFVAR_LISP ("first-change-hook", Vfirst_change_hook,
	       doc: /* A list of functions to call before changing a buffer which is unmodified.
The functions are run using the `run-hooks' function.  */);
  Vfirst_change_hook = Qnil;

  DEFVAR_PER_BUFFER ("buffer-undo-list", &BVAR (current_buffer, undo_list), Qnil,
		     doc: /* List of undo entries in current buffer.
Recent changes come first; older changes follow newer.

An entry (BEG . END) represents an insertion which begins at
position BEG and ends at position END.

An entry (TEXT . POSITION) represents the deletion of the string TEXT
from (abs POSITION).  If POSITION is positive, point was at the front
of the text being deleted; if negative, point was at the end.

An entry (t HIGH . LOW) indicates that the buffer previously had
\"unmodified\" status.  HIGH and LOW are the high and low 16-bit portions
of the visited file's modification time, as of that time.  If the
modification time of the most recent save is different, this entry is
obsolete.

An entry (nil PROPERTY VALUE BEG . END) indicates that a text property
was modified between BEG and END.  PROPERTY is the property name,
and VALUE is the old value.

An entry (apply FUN-NAME . ARGS) means undo the change with
\(apply FUN-NAME ARGS).

An entry (apply DELTA BEG END FUN-NAME . ARGS) supports selective undo
in the active region.  BEG and END is the range affected by this entry
and DELTA is the number of bytes added or deleted in that range by
this change.

An entry (MARKER . DISTANCE) indicates that the marker MARKER
was adjusted in position by the offset DISTANCE (an integer).

An entry of the form POSITION indicates that point was at the buffer
location given by the integer.  Undoing an entry of this form places
point at POSITION.

Entries with value `nil' mark undo boundaries.  The undo command treats
the changes between two undo boundaries as a single step to be undone.

If the value of the variable is t, undo information is not recorded.  */);

  DEFVAR_PER_BUFFER ("mark-active", &BVAR (current_buffer, mark_active), Qnil,
		     doc: /* Non-nil means the mark and region are currently active in this buffer.  */);

  DEFVAR_PER_BUFFER ("cache-long-line-scans", &BVAR (current_buffer, cache_long_line_scans), Qnil,
		     doc: /* Non-nil means that Emacs should use caches to handle long lines more quickly.

Normally, the line-motion functions work by scanning the buffer for
newlines.  Columnar operations (like `move-to-column' and
`compute-motion') also work by scanning the buffer, summing character
widths as they go.  This works well for ordinary text, but if the
buffer's lines are very long (say, more than 500 characters), these
motion functions will take longer to execute.  Emacs may also take
longer to update the display.

If `cache-long-line-scans' is non-nil, these motion functions cache the
results of their scans, and consult the cache to avoid rescanning
regions of the buffer until the text is modified.  The caches are most
beneficial when they prevent the most searching---that is, when the
buffer contains long lines and large regions of characters with the
same, fixed screen width.

When `cache-long-line-scans' is non-nil, processing short lines will
become slightly slower (because of the overhead of consulting the
cache), and the caches will use memory roughly proportional to the
number of newlines and characters whose screen width varies.

The caches require no explicit maintenance; their accuracy is
maintained internally by the Emacs primitives.  Enabling or disabling
the cache should not affect the behavior of any of the motion
functions; it should only affect their performance.  */);

  DEFVAR_PER_BUFFER ("point-before-scroll", &BVAR (current_buffer, point_before_scroll), Qnil,
		     doc: /* Value of point before the last series of scroll operations, or nil.  */);

  DEFVAR_PER_BUFFER ("buffer-file-format", &BVAR (current_buffer, file_format), Qnil,
		     doc: /* List of formats to use when saving this buffer.
Formats are defined by `format-alist'.  This variable is
set when a file is visited.  */);

  DEFVAR_PER_BUFFER ("buffer-auto-save-file-format",
		     &BVAR (current_buffer, auto_save_file_format), Qnil,
		     doc: /* *Format in which to write auto-save files.
Should be a list of symbols naming formats that are defined in `format-alist'.
If it is t, which is the default, auto-save files are written in the
same format as a regular save would use.  */);

  DEFVAR_PER_BUFFER ("buffer-invisibility-spec",
		     &BVAR (current_buffer, invisibility_spec), Qnil,
		     doc: /* Invisibility spec of this buffer.
The default is t, which means that text is invisible
if it has a non-nil `invisible' property.
If the value is a list, a text character is invisible if its `invisible'
property is an element in that list (or is a list with members in common).
If an element is a cons cell of the form (PROP . ELLIPSIS),
then characters with property value PROP are invisible,
and they have an ellipsis as well if ELLIPSIS is non-nil.  */);

  DEFVAR_PER_BUFFER ("buffer-display-count",
		     &BVAR (current_buffer, display_count), Qnil,
		     doc: /* A number incremented each time this buffer is displayed in a window.
The function `set-window-buffer' increments it.  */);

  DEFVAR_PER_BUFFER ("buffer-display-time",
		     &BVAR (current_buffer, display_time), Qnil,
		     doc: /* Time stamp updated each time this buffer is displayed in a window.
The function `set-window-buffer' updates this variable
to the value obtained by calling `current-time'.
If the buffer has never been shown in a window, the value is nil.  */);

  DEFVAR_LISP ("transient-mark-mode", Vtransient_mark_mode,
	       doc: /*  Non-nil if Transient Mark mode is enabled.
See the command `transient-mark-mode' for a description of this minor mode.

Non-nil also enables highlighting of the region whenever the mark is active.
The variable `highlight-nonselected-windows' controls whether to highlight
all windows or just the selected window.

Lisp programs may give this variable certain special values:

- A value of `lambda' enables Transient Mark mode temporarily.
  It is disabled again after any subsequent action that would
  normally deactivate the mark (e.g. buffer modification).

- A value of (only . OLDVAL) enables Transient Mark mode
  temporarily.  After any subsequent point motion command that is
  not shift-translated, or any other action that would normally
  deactivate the mark (e.g. buffer modification), the value of
  `transient-mark-mode' is set to OLDVAL.  */);
  Vtransient_mark_mode = Qnil;

  DEFVAR_LISP ("inhibit-read-only", Vinhibit_read_only,
	       doc: /* *Non-nil means disregard read-only status of buffers or characters.
If the value is t, disregard `buffer-read-only' and all `read-only'
text properties.  If the value is a list, disregard `buffer-read-only'
and disregard a `read-only' text property if the property value
is a member of the list.  */);
  Vinhibit_read_only = Qnil;

  DEFVAR_PER_BUFFER ("cursor-type", &BVAR (current_buffer, cursor_type), Qnil,
		     doc: /* Cursor to use when this buffer is in the selected window.
Values are interpreted as follows:

  t 		  use the cursor specified for the frame
  nil		  don't display a cursor
  box		  display a filled box cursor
  hollow	  display a hollow box cursor
  bar		  display a vertical bar cursor with default width
  (bar . WIDTH)	  display a vertical bar cursor with width WIDTH
  hbar		  display a horizontal bar cursor with default height
  (hbar . HEIGHT) display a horizontal bar cursor with height HEIGHT
  ANYTHING ELSE	  display a hollow box cursor

When the buffer is displayed in a non-selected window, the
cursor's appearance is instead controlled by the variable
`cursor-in-non-selected-windows'.  */);

  DEFVAR_PER_BUFFER ("line-spacing",
		     &BVAR (current_buffer, extra_line_spacing), Qnil,
		     doc: /* Additional space to put between lines when displaying a buffer.
The space is measured in pixels, and put below lines on graphic displays,
see `display-graphic-p'.
If value is a floating point number, it specifies the spacing relative
to the default frame line height.  A value of nil means add no extra space.  */);

  DEFVAR_PER_BUFFER ("cursor-in-non-selected-windows",
		     &BVAR (current_buffer, cursor_in_non_selected_windows), Qnil,
		     doc: /* *Non-nil means show a cursor in non-selected windows.
If nil, only shows a cursor in the selected window.
If t, displays a cursor related to the usual cursor type
\(a solid box becomes hollow, a bar becomes a narrower bar).
You can also specify the cursor type as in the `cursor-type' variable.
Use Custom to set this variable and update the display."  */);

  DEFVAR_LISP ("kill-buffer-query-functions", Vkill_buffer_query_functions,
	       doc: /* List of functions called with no args to query before killing a buffer.
The buffer being killed will be current while the functions are running.
If any of them returns nil, the buffer is not killed.  */);
  Vkill_buffer_query_functions = Qnil;

  DEFVAR_LISP ("change-major-mode-hook", Vchange_major_mode_hook,
	       doc: /* Normal hook run before changing the major mode of a buffer.
The function `kill-all-local-variables' runs this before doing anything else.  */);
  Vchange_major_mode_hook = Qnil;
  DEFSYM (Qchange_major_mode_hook, "change-major-mode-hook");

  DEFVAR_LISP ("buffer-list-update-hook", Vbuffer_list_update_hook,
	       doc: /* Hook run when the buffer list changes.
Functions running this hook are `get-buffer-create',
`make-indirect-buffer', `rename-buffer', `kill-buffer',
and `bury-buffer-internal'.  */);
  Vbuffer_list_update_hook = Qnil;
  DEFSYM (Qbuffer_list_update_hook, "buffer-list-update-hook");

  defsubr (&Sbuffer_live_p);
  defsubr (&Sbuffer_list);
  defsubr (&Sget_buffer);
  defsubr (&Sget_file_buffer);
  defsubr (&Sget_buffer_create);
  defsubr (&Smake_indirect_buffer);
  defsubr (&Sgenerate_new_buffer_name);
  defsubr (&Sbuffer_name);
/*defsubr (&Sbuffer_number);*/
  defsubr (&Sbuffer_file_name);
  defsubr (&Sbuffer_base_buffer);
  defsubr (&Sbuffer_local_value);
  defsubr (&Sbuffer_local_variables);
  defsubr (&Sbuffer_modified_p);
  defsubr (&Sset_buffer_modified_p);
  defsubr (&Sbuffer_modified_tick);
  defsubr (&Sbuffer_chars_modified_tick);
  defsubr (&Srename_buffer);
  defsubr (&Sother_buffer);
  defsubr (&Sbuffer_enable_undo);
  defsubr (&Skill_buffer);
  defsubr (&Sbury_buffer_internal);
  defsubr (&Sset_buffer_major_mode);
  defsubr (&Scurrent_buffer);
  defsubr (&Sset_buffer);
  defsubr (&Sbarf_if_buffer_read_only);
  defsubr (&Serase_buffer);
  defsubr (&Sbuffer_swap_text);
  defsubr (&Sset_buffer_multibyte);
  defsubr (&Skill_all_local_variables);

  defsubr (&Soverlayp);
  defsubr (&Smake_overlay);
  defsubr (&Sdelete_overlay);
  defsubr (&Smove_overlay);
  defsubr (&Soverlay_start);
  defsubr (&Soverlay_end);
  defsubr (&Soverlay_buffer);
  defsubr (&Soverlay_properties);
  defsubr (&Soverlays_at);
  defsubr (&Soverlays_in);
  defsubr (&Snext_overlay_change);
  defsubr (&Sprevious_overlay_change);
  defsubr (&Soverlay_recenter);
  defsubr (&Soverlay_lists);
  defsubr (&Soverlay_get);
  defsubr (&Soverlay_put);
  defsubr (&Srestore_buffer_modified_p);
}

void
keys_of_buffer (void)
{
  initial_define_key (control_x_map, 'b', "switch-to-buffer");
  initial_define_key (control_x_map, 'k', "kill-buffer");

  /* This must not be in syms_of_buffer, because Qdisabled is not
     initialized when that function gets called.  */
  Fput (intern_c_string ("erase-buffer"), Qdisabled, Qt);
}
