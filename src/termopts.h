/* Flags and parameters describing user options for handling the terminal.
   Copyright (C) 1985-1986, 1990, 2001-2012  Free Software Foundation, Inc.

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


/* Nonzero means use ^S/^Q as cretinous flow control.  */
extern int flow_control;

/* Nonzero means use interrupt-driven input.  */
extern int interrupt_input;

/* Nonzero while interrupts are temporarily deferred during redisplay.  */
extern int interrupts_deferred;

/* Terminal has meta key */
extern int meta_key;

