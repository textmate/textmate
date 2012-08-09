# Copyright (C) 1992-1998, 2000-2012  Free Software Foundation, Inc.
#
# This file is part of GNU Emacs.
#
# GNU Emacs is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.
#
# GNU Emacs is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GNU Emacs; see the file COPYING.  If not, write to the
# Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
# Boston, MA 02110-1301, USA.

# Force loading of symbols, enough to give us gdb_valbits etc.
set main
# With some compilers, we need this to give us struct Lisp_Symbol etc.:
set Fmake_symbol

# Find lwlib source files too.
dir ../lwlib
#dir /gd/gnu/lesstif-0.89.9/lib/Xm

# Don't enter GDB when user types C-g to quit.
# This has one unfortunate effect: you can't type C-c
# at the GDB to stop Emacs, when using X.
# However, C-z works just as well in that case.
handle 2 noprint pass

# Make it work like SIGINT normally does.
handle SIGTSTP nopass

# Pass on user signals
handle SIGUSR1 noprint pass
handle SIGUSR2 noprint pass

# Don't pass SIGALRM to Emacs.  This makes problems when
# debugging.
handle SIGALRM ignore

# $valmask and $tagmask are mask values set up by the xreload macro below.

# Use $bugfix so that the value isn't a constant.
# Using a constant runs into GDB bugs sometimes.
define xgetptr
  set $bugfix = $arg0
  set $ptr = (gdb_use_union ? (gdb_use_lsb ? $bugfix.u.val << gdb_gctypebits : $bugfix.u.val) : $bugfix & $valmask) | gdb_data_seg_bits
end

define xgetint
  set $bugfix = $arg0
  set $int = gdb_use_union ? $bugfix.s.val : (gdb_use_lsb ? $bugfix : $bugfix << gdb_gctypebits) >> gdb_gctypebits
end

define xgettype
  set $bugfix = $arg0
  set $type = gdb_use_union ? $bugfix.s.type : (enum Lisp_Type) (gdb_use_lsb ? $bugfix & $tagmask : $bugfix >> gdb_valbits)
end

# Set up something to print out s-expressions.
# We save and restore print_output_debug_flag to prevent the w32 port
# from calling OutputDebugString, which causes GDB to display each
# character twice (yuk!).
define pr
  pp $
end
document pr
Print the emacs s-expression which is $.
Works only when an inferior emacs is executing.
end

# Print out s-expressions
define pp
  set $tmp = $arg0
  set $output_debug = print_output_debug_flag
  set print_output_debug_flag = 0
  set safe_debug_print ($tmp)
  set print_output_debug_flag = $output_debug
end
document pp
Print the argument as an emacs s-expression
Works only when an inferior emacs is executing.
end

# Print value of lisp variable
define pv
  set $tmp = "$arg0"
  set $output_debug = print_output_debug_flag
  set print_output_debug_flag = 0
  set safe_debug_print (find_symbol_value (intern ($tmp)))
  set print_output_debug_flag = $output_debug
end
document pv
Print the value of the lisp variable given as argument.
Works only when an inferior emacs is executing.
end

# Print out current buffer point and boundaries
define ppt
  set $b = current_buffer
  set $t = $b->text
  printf "BUF PT: %d", $b->pt
  if ($b->pt != $b->pt_byte)
    printf "[%d]", $b->pt_byte
  end
  printf " of 1..%d", $t->z
  if ($t->z != $t->z_byte)
    printf "[%d]", $t->z_byte
  end
  if ($b->begv != 1 || $b->zv != $t->z)
    printf " NARROW=%d..%d", $b->begv, $b->zv
    if ($b->begv != $b->begv_byte || $b->zv != $b->zv_byte)
      printf " [%d..%d]", $b->begv_byte, $b->zv_byte
    end
  end
  printf " GAP: %d", $t->gpt
  if ($t->gpt != $t->gpt_byte)
    printf "[%d]", $t->gpt_byte
  end
  printf " SZ=%d\n", $t->gap_size
end
document ppt
Print current buffer's point and boundaries.
Prints values of point, beg, end, narrow, and gap for current buffer.
end

define pitmethod
  set $itmethod = $arg0
  # output $itmethod
  if ($itmethod == 0)
    printf "GET_FROM_BUFFER"
  end
  if ($itmethod == 1)
    printf "GET_FROM_DISPLAY_VECTOR"
  end
  if ($itmethod == 2)
    printf "GET_FROM_STRING"
  end
  if ($itmethod == 3)
    printf "GET_FROM_C_STRING"
  end
  if ($itmethod == 4)
    printf "GET_FROM_IMAGE"
  end
  if ($itmethod == 5)
    printf "GET_FROM_STRETCH"
  end
  if ($itmethod < 0 || $itmethod > 5)
    output $itmethod
  end
end
document pitmethod
Pretty print it->method given as first arg
end

# Print out iterator given as first arg
define pitx
  set $it = $arg0
  printf "cur=%d", $it->current.pos.charpos
  if ($it->current.pos.charpos != $it->current.pos.bytepos)
    printf "[%d]", $it->current.pos.bytepos
  end
  printf " pos=%d", $it->position.charpos
  if ($it->position.charpos != $it->position.bytepos)
    printf "[%d]", $it->position.bytepos
  end
  printf " start=%d", $it->start.pos.charpos
  if ($it->start.pos.charpos != $it->start.pos.bytepos)
    printf "[%d]", $it->start.pos.bytepos
  end
  printf " end=%d", $it->end_charpos
  printf " stop=%d", $it->stop_charpos
  printf " face=%d", $it->face_id
  if ($it->multibyte_p)
    printf " MB"
  end
  if ($it->header_line_p)
    printf " HL"
  end
  if ($it->n_overlay_strings > 0)
    printf " nov=%d", $it->n_overlay_strings
  end
  if ($it->sp != 0)
    printf " sp=%d", $it->sp
  end
  # IT_CHARACTER
  if ($it->what == 0)
    if ($it->len == 1 && $it->c >= ' ' && it->c < 255)
      printf " ch='%c'", $it->c
    else
      printf " ch=[%d,%d]", $it->c, $it->len
    end
  else
    printf " "
    # output $it->what
    if ($it->what == 0)
      printf "IT_CHARACTER"
    end
    if ($it->what == 1)
      printf "IT_COMPOSITION"
    end
    if ($it->what == 2)
      printf "IT_IMAGE"
    end
    if ($it->what == 3)
      printf "IT_STRETCH"
    end
    if ($it->what == 4)
      printf "IT_EOB"
    end
    if ($it->what == 5)
      printf "IT_TRUNCATION"
    end
    if ($it->what == 6)
      printf "IT_CONTINUATION"
    end
    if ($it->what < 0 || $it->what > 6)
      output $it->what
    end
  end
  if ($it->method != 0)
    # !GET_FROM_BUFFER
    printf " next="
    pitmethod $it->method
    if ($it->method == 2)
      # GET_FROM_STRING
      printf "[%d]", $it->current.string_pos.charpos
    end
    if ($it->method == 4)
      # GET_FROM_IMAGE
      printf "[%d]", $it->image_id
    end
  end
  printf "\n"
  if ($it->bidi_p)
    printf "BIDI: base_stop=%d prev_stop=%d level=%d\n", $it->base_level_stop, $it->prev_stop, $it->bidi_it.resolved_level
  end
  if ($it->region_beg_charpos >= 0)
    printf "reg=%d-%d ", $it->region_beg_charpos, $it->region_end_charpos
  end
  printf "vpos=%d hpos=%d", $it->vpos, $it->hpos,
  printf " y=%d lvy=%d", $it->current_y, $it->last_visible_y
  printf " x=%d vx=%d-%d", $it->current_x, $it->first_visible_x, $it->last_visible_x
  printf " w=%d", $it->pixel_width
  printf " a+d=%d+%d=%d", $it->ascent, $it->descent, $it->ascent+$it->descent
  printf " max=%d+%d=%d", $it->max_ascent, $it->max_descent, $it->max_ascent+$it->max_descent
  printf "\n"
  set $i = 0
  while ($i < $it->sp && $i < 4)
    set $e = $it->stack[$i]
    printf "stack[%d]: ", $i
    pitmethod $e.method
    printf "[%d]", $e.position.charpos
    printf "\n"
    set $i = $i + 1
  end
end
document pitx
Pretty print a display iterator.
Take one arg, an iterator object or pointer.
end

define pit
  pitx it
end
document pit
Pretty print the display iterator it.
end

define prowx
  set $row = $arg0
  printf "y=%d x=%d pwid=%d", $row->y, $row->x, $row->pixel_width
  printf " a+d=%d+%d=%d", $row->ascent, $row->height-$row->ascent, $row->height
  printf " phys=%d+%d=%d", $row->phys_ascent, $row->phys_height-$row->phys_ascent, $row->phys_height
  printf " vis=%d\n", $row->visible_height
  printf "used=(LMargin=%d,Text=%d,RMargin=%d) Hash=%d\n", $row->used[0], $row->used[1], $row->used[2], $row->hash
  printf "start=%d end=%d", $row->start.pos.charpos, $row->end.pos.charpos
  if ($row->enabled_p)
    printf " ENA"
  end
  if ($row->displays_text_p)
    printf " DISP"
  end
  if ($row->mode_line_p)
    printf " MODEL"
  end
  if ($row->continued_p)
    printf " CONT"
  end
  if ($row-> truncated_on_left_p)
    printf " TRUNC:L"
  end
  if ($row-> truncated_on_right_p)
    printf " TRUNC:R"
  end
  if ($row->starts_in_middle_of_char_p)
    printf " STARTMID"
  end
  if ($row->ends_in_middle_of_char_p)
    printf " ENDMID"
  end
  if ($row->ends_in_newline_from_string_p)
    printf " ENDNLFS"
  end
  if ($row->ends_at_zv_p)
    printf " ENDZV"
  end
  if ($row->overlapped_p)
    printf " OLAPD"
  end
  if ($row->overlapping_p)
    printf " OLAPNG"
  end
  printf "\n"
end
document prowx
Pretty print information about glyph_row.
Takes one argument, a row object or pointer.
end

define prow
  prowx row
end
document prow
Pretty print information about glyph_row in row.
end


define pcursorx
  set $cp = $arg0
  printf "y=%d x=%d vpos=%d hpos=%d", $cp->y, $cp->x, $cp->vpos, $cp->hpos
end
document pcursorx
Pretty print a window cursor.
end

define pcursor
  printf "output: "
  pcursorx output_cursor
  printf "\n"
end
document pcursor
Pretty print the output_cursor.
end

define pwinx
  set $w = $arg0
  xgetint $w->sequence_number
  if ($w->mini_p != Qnil)
    printf "Mini "
  end
  printf "Window %d ", $int
  xgetptr $w->buffer
  set $tem = (struct buffer *) $ptr
  xgetptr $tem->name_
  printf "%s", ((struct Lisp_String *) $ptr)->data
  printf "\n"
  xgetptr $w->start
  set $tem = (struct Lisp_Marker *) $ptr
  printf "start=%d end:", $tem->charpos
  if ($w->window_end_valid != Qnil)
    xgetint $w->window_end_pos
    printf "pos=%d", $int
    xgetint $w->window_end_vpos
    printf " vpos=%d", $int
  else
    printf "invalid"
  end
  printf " vscroll=%d", $w->vscroll
  if ($w->force_start != Qnil)
    printf " FORCE_START"
  end
  if ($w->must_be_updated_p)
    printf " MUST_UPD"
  end
  printf "\n"
  printf "cursor: "
  pcursorx $w->cursor
  printf "  phys: "
  pcursorx $w->phys_cursor
  if ($w->phys_cursor_on_p)
    printf " ON"
  else
    printf " OFF"
  end
  printf " blk="
  if ($w->last_cursor_off_p != $w->cursor_off_p)
    if ($w->last_cursor_off_p)
      printf "ON->"
    else
      printf "OFF->"
    end
  end
  if ($w->cursor_off_p)
    printf "ON"
  else
    printf "OFF"
  end
  printf "\n"
end
document pwinx
Pretty print a window structure.
Takes one argument, a pointer to a window structure.
end

define pwin
  pwinx w
end
document pwin
Pretty print window structure w.
end

define pbiditype
  if ($arg0 == 0)
    printf "UNDEF"
  end
  if ($arg0 == 1)
    printf "L"
  end
  if ($arg0 == 2)
    printf "R"
  end
  if ($arg0 == 3)
    printf "EN"
  end
  if ($arg0 == 4)
    printf "AN"
  end
  if ($arg0 == 5)
    printf "BN"
  end
  if ($arg0 == 6)
    printf "B"
  end
  if ($arg0 < 0 || $arg0 > 6)
    printf "%d??", $arg0
  end
end
document pbiditype
Print textual description of bidi type given as first argument.
end

define pgx
  set $g = $arg0
  # CHAR_GLYPH
  if ($g.type == 0)
    if ($g.u.ch >= ' ' && $g.u.ch < 127)
      printf "CHAR[%c]", $g.u.ch
    else
      printf "CHAR[0x%x]", $g.u.ch
    end
  end
  # COMPOSITE_GLYPH
  if ($g.type == 1)
    printf "COMP[%d (%d..%d)]", $g.u.cmp.id, $g.slice.cmp.from, $g.slice.cmp.to
  end
  # GLYPHLESS_GLYPH
  if ($g.type == 2)
    printf "GLYPHLESS["
    if ($g.u.glyphless.method == 0)
      printf "THIN]"
    end
    if ($g.u.glyphless.method == 1)
      printf "EMPTY]"
    end
    if ($g.u.glyphless.method == 2)
      printf "ACRO]"
    end
    if ($g.u.glyphless.method == 3)
      printf "HEX]"
    end
  end
  # IMAGE_GLYPH
  if ($g.type == 3)
    printf "IMAGE[%d]", $g.u.img_id
  end
  # STRETCH_GLYPH
  if ($g.type == 4)
    printf "STRETCH[%d+%d]", $g.u.stretch.height, $g.u.stretch.ascent
  end
  xgettype ($g.object)
  if ($type == Lisp_String)
    printf " str=%x[%d]", $g.object, $g.charpos
  else
    printf " pos=%d", $g.charpos
  end
  # For characters, print their resolved level and bidi type
  if ($g.type == 0)
    printf " blev=%d,btyp=", $g.resolved_level
    pbiditype $g.bidi_type
  end
  printf " w=%d a+d=%d+%d", $g.pixel_width, $g.ascent, $g.descent
  # If not DEFAULT_FACE_ID
  if ($g.face_id != 0)
    printf " face=%d", $g.face_id
  end
  if ($g.voffset)
    printf " vof=%d", $g.voffset
  end
  if ($g.multibyte_p)
    printf " MB"
  end
  if ($g.padding_p)
    printf " PAD"
  end
  if ($g.glyph_not_available_p)
    printf " N/A"
  end
  if ($g.overlaps_vertically_p)
    printf " OVL"
  end
  if ($g.avoid_cursor_p)
    printf " AVOID"
  end
  if ($g.left_box_line_p)
    printf " ["
  end
  if ($g.right_box_line_p)
    printf " ]"
  end
  if ($g.slice.img.x || $g.slice.img.y || $g.slice.img.width || $g.slice.img.height)
    printf " slice=%d,%d,%d,%d" ,$g.slice.img.x, $g.slice.img.y, $g.slice.img.width, $g.slice.img.height
  end
  printf "\n"
end
document pgx
Pretty print a glyph structure.
Takes one argument, a pointer to a glyph structure.
end

define pg
  set $pgidx = 0
  pgx glyph
end
document pg
Pretty print glyph structure glyph.
end

define pgi
  set $pgidx = $arg0
  pgx (&glyph[$pgidx])
end
document pgi
Pretty print glyph structure glyph[I].
Takes one argument, a integer I.
end

define pgn
  set $pgidx = $pgidx + 1
  pgx (&glyph[$pgidx])
end
document pgn
Pretty print next glyph structure.
end

define pgrowx
  set $row = $arg0
  set $area = 0
  set $xofs = $row->x
  while ($area < 3)
    set $used = $row->used[$area]
    if ($used > 0)
      set $gl0 = $row->glyphs[$area]
      set $pgidx = 0
      printf "%s: %d glyphs\n", ($area == 0 ? "LEFT" : $area == 2 ? "RIGHT" : "TEXT"), $used
      while ($pgidx < $used)
	printf "%3d %4d: ", $pgidx, $xofs
	pgx $gl0[$pgidx]
	set $xofs = $xofs + $gl0[$pgidx]->pixel_width
	set $pgidx = $pgidx + 1
      end
    end
    set $area = $area + 1
  end
end
document pgrowx
Pretty print all glyphs in a row structure.
Takes one argument, a pointer to a row structure.
end

define pgrow
  pgrowx row
end
document pgrow
Pretty print all glyphs in row structure row.
end

define pgrowit
  pgrowx it->glyph_row
end
document pgrowit
Pretty print all glyphs in it->glyph_row.
end

define prowlims
  printf "edges=(%d,%d),r2l=%d,cont=%d,trunc=(%d,%d),at_zv=%d\n", $arg0->minpos.charpos, $arg0->maxpos.charpos, $arg0->reversed_p, $arg0->continued_p, $arg0->truncated_on_left_p, $arg0->truncated_on_right_p, $arg0->ends_at_zv_p
end
document prowlims
Print important attributes of a glyph_row structure.
Takes one argument, a pointer to a glyph_row structure.
end

define pmtxrows
  set $mtx = $arg0
  set $gl = $mtx->rows
  set $glend = $mtx->rows + $mtx->nrows - 1
  set $i = 0
  while ($gl < $glend)
    printf "%d: ", $i
    prowlims $gl
    set $gl = $gl + 1
    set $i = $i + 1
  end
end
document pmtxrows
Print data about glyph rows in a glyph matrix.
Takes one argument, a pointer to a glyph_matrix structure.
end

define xtype
  xgettype $
  output $type
  echo \n
  if $type == Lisp_Misc
    xmisctype
  else
    if $type == Lisp_Vectorlike
      xvectype
    end
  end
end
document xtype
Print the type of $, assuming it is an Emacs Lisp value.
If the first type printed is Lisp_Vector or Lisp_Misc,
a second line gives the more precise type.
end

define xvectype
  xgetptr $
  set $size = ((struct Lisp_Vector *) $ptr)->header.size
  output ($size & PVEC_FLAG) ? (enum pvec_type) ($size & PVEC_TYPE_MASK) : $size & ~gdb_array_mark_flag
  echo \n
end
document xvectype
Print the size or vector subtype of $.
This command assumes that $ is a vector or pseudovector.
end

define xmisctype
  xgetptr $
  output (enum Lisp_Misc_Type) (((struct Lisp_Free *) $ptr)->type)
  echo \n
end
document xmisctype
Assume that $ is some misc type and print its specific type.
end

define xint
  xgetint $
  print $int
end
document xint
Print $ as an Emacs Lisp integer.  This gets the sign right.
end

define xptr
  xgetptr $
  print (void *) $ptr
end
document xptr
Print the pointer portion of an Emacs Lisp value in $.
end

define xmarker
  xgetptr $
  print (struct Lisp_Marker *) $ptr
end
document xmarker
Print $ as a marker pointer.
This command assumes that $ is an Emacs Lisp marker value.
end

define xoverlay
  xgetptr $
  print (struct Lisp_Overlay *) $ptr
end
document xoverlay
Print $ as a overlay pointer.
This command assumes that $ is an Emacs Lisp overlay value.
end

define xmiscfree
  xgetptr $
  print (struct Lisp_Free *) $ptr
end
document xmiscfree
Print $ as a misc free-cell pointer.
This command assumes that $ is an Emacs Lisp Misc value.
end

define xintfwd
  xgetptr $
  print (struct Lisp_Intfwd *) $ptr
end
document xintfwd
Print $ as an integer forwarding pointer.
This command assumes that $ is an Emacs Lisp Misc value.
end

define xboolfwd
  xgetptr $
  print (struct Lisp_Boolfwd *) $ptr
end
document xboolfwd
Print $ as a boolean forwarding pointer.
This command assumes that $ is an Emacs Lisp Misc value.
end

define xobjfwd
  xgetptr $
  print (struct Lisp_Objfwd *) $ptr
end
document xobjfwd
Print $ as an object forwarding pointer.
This command assumes that $ is an Emacs Lisp Misc value.
end

define xbufobjfwd
  xgetptr $
  print (struct Lisp_Buffer_Objfwd *) $ptr
end
document xbufobjfwd
Print $ as a buffer-local object forwarding pointer.
This command assumes that $ is an Emacs Lisp Misc value.
end

define xkbobjfwd
  xgetptr $
  print (struct Lisp_Kboard_Objfwd *) $ptr
end
document xkbobjfwd
Print $ as a kboard-local object forwarding pointer.
This command assumes that $ is an Emacs Lisp Misc value.
end

define xbuflocal
  xgetptr $
  print (struct Lisp_Buffer_Local_Value *) $ptr
end
document xbuflocal
Print $ as a buffer-local-value pointer.
This command assumes that $ is an Emacs Lisp Misc value.
end

define xsymbol
  set $sym = $
  xgetptr $sym
  print (struct Lisp_Symbol *) $ptr
  xprintsym $sym
  echo \n
end
document xsymbol
Print the name and address of the symbol $.
This command assumes that $ is an Emacs Lisp symbol value.
end

define xstring
  xgetptr $
  print (struct Lisp_String *) $ptr
  xprintstr $
  echo \n
end
document xstring
Print the contents and address of the string $.
This command assumes that $ is an Emacs Lisp string value.
end

define xvector
  xgetptr $
  print (struct Lisp_Vector *) $ptr
  output ($->header.size > 50) ? 0 : ($->contents[0])@($->header.size & ~gdb_array_mark_flag)
echo \n
end
document xvector
Print the contents and address of the vector $.
This command assumes that $ is an Emacs Lisp vector value.
end

define xprocess
  xgetptr $
  print (struct Lisp_Process *) $ptr
  output *$
  echo \n
end
document xprocess
Print the address of the struct Lisp_process to which $ points.
This command assumes that $ is a Lisp_Object.
end

define xframe
  xgetptr $
  print (struct frame *) $ptr
  xgetptr $->name
  set $ptr = (struct Lisp_String *) $ptr
  xprintstr $ptr
  echo \n
end
document xframe
Print $ as a frame pointer.
This command assumes $ is an Emacs Lisp frame value.
end

define xcompiled
  xgetptr $
  print (struct Lisp_Vector *) $ptr
  output ($->contents[0])@($->header.size & 0xff)
end
document xcompiled
Print $ as a compiled function pointer.
This command assumes that $ is an Emacs Lisp compiled value.
end

define xwindow
  xgetptr $
  print (struct window *) $ptr
  set $window = (struct window *) $ptr
  xgetint $window->total_cols
  set $width=$int
  xgetint $window->total_lines
  set $height=$int
  xgetint $window->left_col
  set $left=$int
  xgetint $window->top_line
  set $top=$int
  printf "%dx%d+%d+%d\n", $width, $height, $left, $top
end
document xwindow
Print $ as a window pointer, assuming it is an Emacs Lisp window value.
Print the window's position as "WIDTHxHEIGHT+LEFT+TOP".
end

define xwinconfig
  xgetptr $
  print (struct save_window_data *) $ptr
end
document xwinconfig
Print $ as a window configuration pointer.
This command assumes that $ is an Emacs Lisp window configuration value.
end

define xsubr
  xgetptr $
  print (struct Lisp_Subr *) $ptr
  output *$
  echo \n
end
document xsubr
Print the address of the subr which the Lisp_Object $ points to.
end

define xchartable
  xgetptr $
  print (struct Lisp_Char_Table *) $ptr
  printf "Purpose: "
  xprintsym $->purpose
  printf "  %d extra slots", ($->header.size & 0x1ff) - 68
  echo \n
end
document xchartable
Print the address of the char-table $, and its purpose.
This command assumes that $ is an Emacs Lisp char-table value.
end

define xsubchartable
  xgetptr $
  print (struct Lisp_Sub_Char_Table *) $ptr
  xgetint $->depth
  set $depth = $int
  xgetint $->min_char
  printf "Depth: %d, Min char: %d (0x%x)\n", $depth, $int, $int
end
document xsubchartable
Print the address of the sub-char-table $, its depth and min-char.
This command assumes that $ is an Emacs Lisp sub-char-table value.
end

define xboolvector
  xgetptr $
  print (struct Lisp_Bool_Vector *) $ptr
  output ($->header.size > 256) ? 0 : ($->data[0])@((($->header.size & ~gdb_array_mark_flag) + 7)/ 8)
  echo \n
end
document xboolvector
Print the contents and address of the bool-vector $.
This command assumes that $ is an Emacs Lisp bool-vector value.
end

define xbuffer
  xgetptr $
  print (struct buffer *) $ptr
  xgetptr $->name_
  output ((struct Lisp_String *) $ptr)->data
  echo \n
end
document xbuffer
Set $ as a buffer pointer and the name of the buffer.
This command assumes $ is an Emacs Lisp buffer value.
end

define xhashtable
  xgetptr $
  print (struct Lisp_Hash_Table *) $ptr
end
document xhashtable
Set $ as a hash table pointer.
This command assumes that $ is an Emacs Lisp hash table value.
end

define xcons
  xgetptr $
  print (struct Lisp_Cons *) $ptr
  output/x *$
  echo \n
end
document xcons
Print the contents of $ as an Emacs Lisp cons.
end

define nextcons
  p $.u.cdr
  xcons
end
document nextcons
Print the contents of the next cell in a list.
This command assumes that the last thing you printed was a cons cell contents
(type struct Lisp_Cons) or a pointer to one.
end
define xcar
  xgetptr $
  xgettype $
  print/x ($type == Lisp_Cons ? ((struct Lisp_Cons *) $ptr)->car : 0)
end
document xcar
Assume that $ is an Emacs Lisp pair and print its car.
end

define xcdr
  xgetptr $
  xgettype $
  print/x ($type == Lisp_Cons ? ((struct Lisp_Cons *) $ptr)->u.cdr : 0)
end
document xcdr
Assume that $ is an Emacs Lisp pair and print its cdr.
end

define xlist
  xgetptr $
  set $cons = (struct Lisp_Cons *) $ptr
  xgetptr Qnil
  set $nil = $ptr
  set $i = 0
  while $cons != $nil && $i < 10
    p/x $cons->car
    xpr
    xgetptr $cons->u.cdr
    set $cons = (struct Lisp_Cons *) $ptr
    set $i = $i + 1
    printf "---\n"
  end
  if $cons == $nil
    printf "nil\n"
  else
    printf "...\n"
    p $ptr
  end
end
document xlist
Print $ assuming it is a list.
end

define xfloat
  xgetptr $
  print ((struct Lisp_Float *) $ptr)->u.data
end
document xfloat
Print $ assuming it is a lisp floating-point number.
end

define xscrollbar
  xgetptr $
  print (struct scrollbar *) $ptr
output *$
echo \n
end
document xscrollbar
Print $ as a scrollbar pointer.
end

define xpr
  xtype
  if $type == Lisp_Int
    xint
  end
  if $type == Lisp_Symbol
    xsymbol
  end
  if $type == Lisp_String
    xstring
  end
  if $type == Lisp_Cons
    xcons
  end
  if $type == Lisp_Float
    xfloat
  end
  if $type == Lisp_Misc
    set $misc = (enum Lisp_Misc_Type) (((struct Lisp_Free *) $ptr)->type)
    if $misc == Lisp_Misc_Free
      xmiscfree
    end
    if $misc == Lisp_Misc_Boolfwd
      xboolfwd
    end
    if $misc == Lisp_Misc_Marker
      xmarker
    end
    if $misc == Lisp_Misc_Intfwd
      xintfwd
    end
    if $misc == Lisp_Misc_Boolfwd
      xboolfwd
    end
    if $misc == Lisp_Misc_Objfwd
      xobjfwd
    end
    if $misc == Lisp_Misc_Buffer_Objfwd
      xbufobjfwd
    end
    if $misc == Lisp_Misc_Buffer_Local_Value
      xbuflocal
    end
#    if $misc == Lisp_Misc_Some_Buffer_Local_Value
#      xvalue
#    end
    if $misc == Lisp_Misc_Overlay
      xoverlay
    end
    if $misc == Lisp_Misc_Kboard_Objfwd
      xkbobjfwd
    end
#    if $misc == Lisp_Misc_Save_Value
#      xsavevalue
#    end
  end
  if $type == Lisp_Vectorlike
    set $size = ((struct Lisp_Vector *) $ptr)->header.size
    if ($size & PVEC_FLAG)
      set $vec = (enum pvec_type) ($size & PVEC_TYPE_MASK)
      if $vec == PVEC_NORMAL_VECTOR
	xvector
      end
      if $vec == PVEC_PROCESS
	xprocess
      end
      if $vec == PVEC_FRAME
	xframe
      end
      if $vec == PVEC_COMPILED
	xcompiled
      end
      if $vec == PVEC_WINDOW
	xwindow
      end
      if $vec == PVEC_WINDOW_CONFIGURATION
	xwinconfig
      end
      if $vec == PVEC_SUBR
	xsubr
      end
      if $vec == PVEC_CHAR_TABLE
	xchartable
      end
      if $vec == PVEC_BOOL_VECTOR
	xboolvector
      end
      if $vec == PVEC_BUFFER
	xbuffer
      end
      if $vec == PVEC_HASH_TABLE
	xhashtable
      end
    else
      xvector
    end
  end
end
document xpr
Print $ as a lisp object of any type.
end

define xprintstr
  set $data = (char *) $arg0->data
  output ($arg0->size > 1000) ? 0 : ($data[0])@($arg0->size_byte < 0 ? $arg0->size & ~gdb_array_mark_flag : $arg0->size_byte)
end

define xprintsym
  xgetptr $arg0
  set $sym = (struct Lisp_Symbol *) $ptr
  xgetptr $sym->xname
  set $sym_name = (struct Lisp_String *) $ptr
  xprintstr $sym_name
end
document xprintsym
  Print argument as a symbol.
end

define xcoding
  set $tmp = (struct Lisp_Hash_Table *) ((Vcoding_system_hash_table & $valmask) | gdb_data_seg_bits)
  set $tmp = (struct Lisp_Vector *) (($tmp->key_and_value & $valmask) | gdb_data_seg_bits)
  set $name = $tmp->contents[$arg0 * 2]
  print $name
  pr
  print $tmp->contents[$arg0 * 2 + 1]
  pr
end
document xcoding
  Print the name and attributes of coding system that has ID (argument).
end

define xcharset
  set $tmp = (struct Lisp_Hash_Table *) ((Vcharset_hash_table & $valmask) | gdb_data_seg_bits)
  set $tmp = (struct Lisp_Vector *) (($tmp->key_and_value & $valmask) | gdb_data_seg_bits)
  p $tmp->contents[charset_table[$arg0].hash_index * 2]
  pr
end
document xcharset
  Print the name of charset that has ID (argument).
end

define xfontset
  xgetptr $
  set $tbl = (struct Lisp_Char_Table *) $ptr
  print $tbl
  xgetint $tbl->extras[0]
  printf " ID:%d", $int
  xgettype $tbl->extras[1]
  xgetptr $tbl->extras[1]
  if $type == Lisp_String
    set $ptr = (struct Lisp_String *) $ptr
    printf " Name:"
    xprintstr $ptr
  else
    xgetptr $tbl->extras[2]
    set $ptr = (struct Lisp_Char_Table *) $ptr
    xgetptr $ptr->extras[1]
    set $ptr = (struct Lisp_String *) $ptr
    printf " Realized from:"
    xprintstr $ptr
  end
  echo \n
end

define xfont
  xgetptr $
  set $size = (((struct Lisp_Vector *) $ptr)->header.size & 0x1FF)
  if $size == FONT_SPEC_MAX
    print (struct font_spec *) $ptr
  else
    if $size == FONT_ENTITY_MAX
      print (struct font_entity *) $ptr
    else
      print (struct font *) $ptr
    end
  end
end
document xfont
Print $ assuming it is a list font (font-spec, font-entity, or font-object).
end

define xbacktrace
  set $bt = backtrace_list
  while $bt
    xgettype (*$bt->function)
    if $type == Lisp_Symbol
      xprintsym (*$bt->function)
      printf " (0x%x)\n", $bt->args
    else
      xgetptr *$bt->function
      printf "0x%x ", $ptr
      if $type == Lisp_Vectorlike
	xgetptr (*$bt->function)
        set $size = ((struct Lisp_Vector *) $ptr)->header.size
        output ($size & PVEC_FLAG) ? (enum pvec_type) ($size & PVEC_TYPE_MASK) : $size & ~gdb_array_mark_flag
      else
        printf "Lisp type %d", $type
      end
      echo \n
    end
    set $bt = $bt->next
  end
end
document xbacktrace
  Print a backtrace of Lisp function calls from backtrace_list.
  Set a breakpoint at Fsignal and call this to see from where
  an error was signaled.
end

define xprintbytestr
  set $data = (char *) $arg0->data
  printf "Bytecode: "
  output/u ($arg0->size > 1000) ? 0 : ($data[0])@($arg0->size_byte < 0 ? $arg0->size & ~gdb_array_mark_flag : $arg0->size_byte)
end
document xprintbytestr
  Print a string of byte code.
end

define xwhichsymbols
  set $output_debug = print_output_debug_flag
  set print_output_debug_flag = 0
  set safe_debug_print (which_symbols ($arg0, $arg1))
  set print_output_debug_flag = $output_debug
end
document xwhichsymbols
  Print symbols which references a given lisp object
  either as its symbol value or symbol function.
  Call with two arguments: the lisp object and the
  maximum number of symbols referencing it to produce.
end

define xbytecode
  set $bt = byte_stack_list
  while $bt
    xgetptr $bt->byte_string
    set $ptr = (struct Lisp_String *) $ptr
    xprintbytestr $ptr
    printf "\n0x%x => ", $bt->byte_string
    xwhichsymbols $bt->byte_string 5
    set $bt = $bt->next
  end
end
document xbytecode
  Print a backtrace of the byte code stack.
end

# Show Lisp backtrace after normal backtrace.
define hookpost-backtrace
  set $bt = backtrace_list
  if $bt
    echo \n
    echo Lisp Backtrace:\n
    xbacktrace
  end
end

define xreload
  set $tagmask = (((long)1 << gdb_gctypebits) - 1)
  # The consing_since_gc business widens the 1 to EMACS_INT,
  # a symbol not directly visible to GDB.
  set $valmask = gdb_use_lsb ? ~($tagmask) : ((consing_since_gc - consing_since_gc + 1) << gdb_valbits) - 1
end
document xreload
  When starting Emacs a second time in the same gdb session under
  FreeBSD 2.2.5, gdb 4.13, $valmask have lost
  their values.  (The same happens on current (2000) versions of GNU/Linux
  with gdb 5.0.)
  This function reloads them.
end
xreload

# Flush display (X only)
define ff
  set x_flush (0)
end
document ff
Flush pending X window display updates to screen.
Works only when an inferior emacs is executing.
end


define hook-run
  xreload
end

# Call xreload if a new Emacs executable is loaded.
define hookpost-run
  xreload
end

set print pretty on
set print sevenbit-strings

show environment DISPLAY
show environment TERM

# People get bothered when they see messages about non-existent functions...
xgetptr globals.f_Vsystem_type
# $ptr is NULL in temacs
if ($ptr != 0)
  set $tem = (struct Lisp_Symbol *) $ptr
  xgetptr $tem->xname
  set $tem = (struct Lisp_String *) $ptr
  set $tem = (char *) $tem->data

  # Don't let abort actually run, as it will make stdio stop working and
  # therefore the `pr' command above as well.
  if $tem[0] == 'w' && $tem[1] == 'i' && $tem[2] == 'n' && $tem[3] == 'd'
    # The windows-nt build replaces abort with its own function.
    break w32_abort
  else
    break abort
  end
end

# x_error_quitter is defined only on X.  But window-system is set up
# only at run time, during Emacs startup, so we need to defer setting
# the breakpoint.  init_sys_modes is the first function called on
# every platform after init_display, where window-system is set.
tbreak init_sys_modes
commands
  silent
  xgetptr globals.f_Vinitial_window_system
  set $tem = (struct Lisp_Symbol *) $ptr
  xgetptr $tem->xname
  set $tem = (struct Lisp_String *) $ptr
  set $tem = (char *) $tem->data
  # If we are running in synchronous mode, we want a chance to look
  # around before Emacs exits.  Perhaps we should put the break
  # somewhere else instead...
  if $tem[0] == 'x' && $tem[1] == '\0'
    break x_error_quitter
  end
  continue
end
