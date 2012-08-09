/* Cursor motion subroutines for GNU Emacs.
   Copyright (C) 1985, 1995, 2001-2012  Free Software Foundation, Inc.
    based primarily on public domain code written by Chris Torek

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

#include "lisp.h"
#include "frame.h"
#include "cm.h"
#include "termhooks.h"
#include "termchar.h"
#include "tparam.h"

#define	BIG	9999		/* 9999 good on VAXen.  For 16 bit machines
				   use about 2000.... */

int cost;		/* sums up costs */

/* ARGSUSED */
int
evalcost (int c)
{
  cost++;
  return c;
}

/* The terminal to use for low-level output. */
struct tty_display_info *current_tty;

int
cmputc (int c)
{
  if (current_tty->termscript)
    putc (c & 0177, current_tty->termscript);
  putc (c & 0177, current_tty->output);
  return c;
}

/* NEXT TWO ARE DONE WITH MACROS */
#if 0
/*
 * Assume the cursor is at row row, column col.  Normally used only after
 * clearing the screen, when the cursor is at (0, 0), but what the heck,
 * let's let the guy put it anywhere.
 */

static
at (tty, row, col) {
  curY (tty) = row;
  curX (tty)  = col;
}

/*
 * Add n columns to the current cursor position.
 */

static
addcol (tty, n) {
  curX (tty) += n;

    /*
     * If cursor hit edge of screen, what happened?
     * N.B.: DO NOT!! write past edge of screen.  If you do, you
     * deserve what you get.  Furthermore, on terminals with
     * autowrap (but not magicwrap), don't write in the last column
     * of the last line.
     */

  if (curX (tty) == tty->Wcm->cm_cols) {
	/*
	 * Well, if magicwrap, still there, past the edge of the
	 * screen (!).  If autowrap, on the col 0 of the next line.
	 * Otherwise on last column.
	 */

	if (tty->Wcm->cm_magicwrap)
	    ;			/* "limbo" */
	else if (tty->Wcm->cm_autowrap) {
          curX (tty) = 0;
          curY (tty) ++;		/* Beware end of screen! */
	}
	else
          curX (tty)--;
    }
}
#endif

/*
 * Terminals with magicwrap (xn) don't all behave identically.
 * The VT100 leaves the cursor in the last column but will wrap before
 * printing the next character.  I hear that the Concept terminal does
 * the wrap immediately but ignores the next newline it sees.  And some
 * terminals just have buggy firmware, and think that the cursor is still
 * in limbo if we use direct cursor addressing from the phantom column.
 * The only guaranteed safe thing to do is to emit a CRLF immediately
 * after we reach the last column; this takes us to a known state.
 */
void
cmcheckmagic (struct tty_display_info *tty)
{
  if (curX (tty) == FrameCols (tty))
    {
      if (!MagicWrap (tty) || curY (tty) >= FrameRows (tty) - 1)
	abort ();
      if (tty->termscript)
	putc ('\r', tty->termscript);
      putc ('\r', tty->output);
      if (tty->termscript)
	putc ('\n', tty->termscript);
      putc ('\n', tty->output);
      curX (tty) = 0;
      curY (tty)++;
    }
}


/*
 * (Re)Initialize the cost factors, given the output speed of the terminal
 * in the variable ospeed.  (Note: this holds B300, B9600, etc -- ie stuff
 * out of <sgtty.h>.)
 */

void
cmcostinit (struct tty_display_info *tty)
{
    char *p;

#define	COST(x,e)	(x ? (cost = 0, tputs (x, 1, e), cost) : BIG)
#define CMCOST(x,e)	((x == 0) ? BIG : (p = tgoto(x, 0, 0), COST(p ,e)))

    tty->Wcm->cc_up =	 COST (tty->Wcm->cm_up, evalcost);
    tty->Wcm->cc_down =	 COST (tty->Wcm->cm_down, evalcost);
    tty->Wcm->cc_left =	 COST (tty->Wcm->cm_left, evalcost);
    tty->Wcm->cc_right = COST (tty->Wcm->cm_right, evalcost);
    tty->Wcm->cc_home =	 COST (tty->Wcm->cm_home, evalcost);
    tty->Wcm->cc_cr =	 COST (tty->Wcm->cm_cr, evalcost);
    tty->Wcm->cc_ll =	 COST (tty->Wcm->cm_ll, evalcost);
    tty->Wcm->cc_tab =	 tty->Wcm->cm_tabwidth ? COST (tty->Wcm->cm_tab, evalcost) : BIG;

    /*
     * These last three are actually minimum costs.  When (if) they are
     * candidates for the least-cost motion, the real cost is computed.
     * (Note that "0" is the assumed to generate the minimum cost.
     * While this is not necessarily true, I have yet to see a terminal
     * for which is not; all the terminals that have variable-cost
     * cursor motion seem to take straight numeric values.  --ACT)
     */

    tty->Wcm->cc_abs =  CMCOST (tty->Wcm->cm_abs, evalcost);
    tty->Wcm->cc_habs = CMCOST (tty->Wcm->cm_habs, evalcost);
    tty->Wcm->cc_vabs = CMCOST (tty->Wcm->cm_vabs, evalcost);

#undef CMCOST
#undef COST
}

/*
 * Calculate the cost to move from (srcy, srcx) to (dsty, dstx) using
 * up and down, and left and right, motions, and tabs.  If doit is set
 * actually perform the motion.
 */

static int
calccost (struct tty_display_info *tty,
          int srcy, int srcx, int dsty, int dstx, int doit)
{
    register int    deltay,
                    deltax,
                    c,
                    totalcost;
    int     ntabs,
            n2tabs,
            tabx,
            tab2x,
            tabcost;
    register const char *p;

    /* If have just wrapped on a terminal with xn,
       don't believe the cursor position: give up here
       and force use of absolute positioning.  */

    if (curX (tty) == tty->Wcm->cm_cols)
      goto fail;

    totalcost = 0;
    if ((deltay = dsty - srcy) == 0)
	goto x;
    if (deltay < 0)
	p = tty->Wcm->cm_up, c = tty->Wcm->cc_up, deltay = -deltay;
    else
	p = tty->Wcm->cm_down, c = tty->Wcm->cc_down;
    if (c == BIG) {		/* caint get thar from here */
	if (doit)
	    printf ("OOPS");
	return c;
    }
    totalcost = c * deltay;
    if (doit)
      do
          emacs_tputs (tty, p, 1, cmputc);
      while (0 < --deltay);
x:
    if ((deltax = dstx - srcx) == 0)
	goto done;
    if (deltax < 0) {
	p = tty->Wcm->cm_left, c = tty->Wcm->cc_left, deltax = -deltax;
	goto dodelta;		/* skip all the tab junk */
    }
    /* Tabs (the toughie) */
    if (tty->Wcm->cc_tab >= BIG || !tty->Wcm->cm_usetabs)
	goto olddelta;		/* forget it! */

    /*
     * ntabs is # tabs towards but not past dstx; n2tabs is one more
     * (ie past dstx), but this is only valid if that is not past the
     * right edge of the screen.  We can check that at the same time
     * as we figure out where we would be if we use the tabs (which
     * we will put into tabx (for ntabs) and tab2x (for n2tabs)).
     */

    ntabs = (deltax + srcx % tty->Wcm->cm_tabwidth) / tty->Wcm->cm_tabwidth;
    n2tabs = ntabs + 1;
    tabx = (srcx / tty->Wcm->cm_tabwidth + ntabs) * tty->Wcm->cm_tabwidth;
    tab2x = tabx + tty->Wcm->cm_tabwidth;

    if (tab2x >= tty->Wcm->cm_cols)	/* too far (past edge) */
	n2tabs = 0;

    /*
     * Now set tabcost to the cost for using ntabs, and c to the cost
     * for using n2tabs, then pick the minimum.
     */

		   /* cost for ntabs           +    cost for right motion */
    tabcost = ntabs ? ntabs * tty->Wcm->cc_tab + (dstx - tabx) * tty->Wcm->cc_right
		    : BIG;

		   /* cost for n2tabs          +    cost for left motion */
    c = n2tabs  ?    n2tabs * tty->Wcm->cc_tab + (tab2x - dstx) * tty->Wcm->cc_left
		: BIG;

    if (c < tabcost)		/* then cheaper to overshoot & back up */
	ntabs = n2tabs, tabcost = c, tabx = tab2x;

    if (tabcost >= BIG)		/* caint use tabs */
	goto newdelta;

    /*
     * See if tabcost is less than just moving right
     */

    if (tabcost < (deltax * tty->Wcm->cc_right)) {
	totalcost += tabcost;	/* use the tabs */
	if (doit)
	    while (--ntabs >= 0)
              emacs_tputs (tty, tty->Wcm->cm_tab, 1, cmputc);
	srcx = tabx;
    }

    /*
     * Now might as well just recompute the delta.
     */

newdelta:
    if ((deltax = dstx - srcx) == 0)
	goto done;
olddelta:
    if (deltax > 0)
	p = tty->Wcm->cm_right, c = tty->Wcm->cc_right;
    else
	p = tty->Wcm->cm_left, c = tty->Wcm->cc_left, deltax = -deltax;

dodelta:
    if (c == BIG) {		/* caint get thar from here */
fail:
	if (doit)
	    printf ("OOPS");
	return BIG;
    }
    totalcost += c * deltax;
    if (doit)
      do
          emacs_tputs (tty, p, 1, cmputc);
      while (0 < --deltax);
done:
    return totalcost;
}

#if 0
void
losecursor (void)
{
  curY = -1;
}
#endif

#define	USEREL	0
#define	USEHOME	1
#define	USELL	2
#define	USECR	3

void
cmgoto (struct tty_display_info *tty, int row, int col)
{
    int     homecost,
            crcost,
            llcost,
            relcost,
            directcost;
    int     use IF_LINT (= 0);
    char *p;
    const char *dcm;

  /* First the degenerate case */
    if (row == curY (tty) && col == curX (tty)) /* already there */
    return;

    if (curY (tty) >= 0 && curX (tty) >= 0)
    {
      /* We may have quick ways to go to the upper-left, bottom-left,
       * start-of-line, or start-of-next-line.  Or it might be best to
       * start where we are.  Examine the options, and pick the cheapest.
       */

      relcost = calccost (tty, curY (tty), curX (tty), row, col, 0);
      use = USEREL;
      if ((homecost = tty->Wcm->cc_home) < BIG)
          homecost += calccost (tty, 0, 0, row, col, 0);
      if (homecost < relcost)
          relcost = homecost, use = USEHOME;
      if ((llcost = tty->Wcm->cc_ll) < BIG)
          llcost += calccost (tty, tty->Wcm->cm_rows - 1, 0, row, col, 0);
      if (llcost < relcost)
          relcost = llcost, use = USELL;
      if ((crcost = tty->Wcm->cc_cr) < BIG) {
	  if (tty->Wcm->cm_autolf)
            if (curY (tty) + 1 >= tty->Wcm->cm_rows)
                crcost = BIG;
	      else
                crcost += calccost (tty, curY (tty) + 1, 0, row, col, 0);
	  else
            crcost += calccost (tty, curY (tty), 0, row, col, 0);
      }
      if (crcost < relcost)
	  relcost = crcost, use = USECR;
      directcost = tty->Wcm->cc_abs, dcm = tty->Wcm->cm_abs;
      if (row == curY (tty) && tty->Wcm->cc_habs < BIG)
	  directcost = tty->Wcm->cc_habs, dcm = tty->Wcm->cm_habs;
      else if (col == curX (tty) && tty->Wcm->cc_vabs < BIG)
	  directcost = tty->Wcm->cc_vabs, dcm = tty->Wcm->cm_vabs;
    }
  else
    {
      directcost = 0, relcost = 100000;
      dcm = tty->Wcm->cm_abs;
    }

  /*
   * In the following comparison, the = in <= is because when the costs
   * are the same, it looks nicer (I think) to move directly there.
   */
  if (directcost <= relcost)
    {
      /* compute REAL direct cost */
      cost = 0;
      p = (dcm == tty->Wcm->cm_habs
           ? tgoto (dcm, row, col)
           : tgoto (dcm, col, row));
      emacs_tputs (tty, p, 1, evalcost);
      if (cost <= relcost)
	{	/* really is cheaper */
	  emacs_tputs (tty, p, 1, cmputc);
	  curY (tty) = row, curX (tty) = col;
	  return;
	}
    }

  switch (use)
    {
    case USEHOME:
      emacs_tputs (tty, tty->Wcm->cm_home, 1, cmputc);
      curY (tty) = 0, curX (tty) = 0;
      break;

    case USELL:
      emacs_tputs (tty, tty->Wcm->cm_ll, 1, cmputc);
      curY (tty) = tty->Wcm->cm_rows - 1, curX (tty) = 0;
      break;

    case USECR:
      emacs_tputs (tty, tty->Wcm->cm_cr, 1, cmputc);
      if (tty->Wcm->cm_autolf)
	curY (tty)++;
      curX (tty) = 0;
      break;
    }

  (void) calccost (tty, curY (tty), curX (tty), row, col, 1);
  curY (tty) = row, curX (tty) = col;
}

/* Clear out all terminal info.
   Used before copying into it the info on the actual terminal.
 */

void
Wcm_clear (struct tty_display_info *tty)
{
  memset (tty->Wcm, 0, sizeof (struct cm));
  UP = 0;
  BC = 0;
}

/*
 * Initialized stuff
 * Return 0 if can do CM.
 * Return -1 if cannot.
 * Return -2 if size not specified.
 */

int
Wcm_init (struct tty_display_info *tty)
{
#if 0
  if (tty->Wcm->cm_abs && !tty->Wcm->cm_ds)
    return 0;
#endif
  if (tty->Wcm->cm_abs)
    return 0;
  /* Require up and left, and, if no absolute, down and right */
  if (!tty->Wcm->cm_up || !tty->Wcm->cm_left)
    return - 1;
  if (!tty->Wcm->cm_abs && (!tty->Wcm->cm_down || !tty->Wcm->cm_right))
    return - 1;
  /* Check that we know the size of the screen.... */
  if (tty->Wcm->cm_rows <= 0 || tty->Wcm->cm_cols <= 0)
    return - 2;
  return 0;
}
