/* Cursor motion calculation definitions for GNU Emacs
   Copyright (C) 1985, 1989, 2001-2012  Free Software Foundation, Inc.

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

/* Holds the minimum and maximum costs for the parameterized capabilities.  */
struct parmcap
  {
    int mincost, maxcost;
  };

/* This structure holds everything needed to do cursor motion except the pad
   character (PC) and the output speed of the terminal (ospeed), which
   termcap wants in global variables.  */

struct cm
  {
    /* Cursor position.  -1 in *both* variables means the cursor
       position is unknown, in order to force absolute cursor motion. */

    int cm_curY;			/* Current row */
    int cm_curX;			/* Current column */

    /* Capabilities from termcap */
    const char *cm_up;		/* up (up) */
    const char *cm_down;	/* down (do) */
    const char *cm_left;	/* left (le) */
    const char *cm_right;	/* right (nd) */
    const char *cm_home;	/* home (ho) */
    const char *cm_cr;		/* carriage return (cr) */
    const char *cm_ll;		/* last line (ll) */
    const char *cm_tab;		/* tab (ta) */
    const char *cm_backtab;	/* backtab (bt) */
    char *cm_abs;		/* absolute (cm) */
    const char *cm_habs;	/* horizontal absolute (ch) */
    const char *cm_vabs;	/* vertical absolute (cv) */
#if 0
    const char *cm_ds;		/* "don't send" string (ds) */
#endif
    const char *cm_multiup;	/* multiple up (UP) */
    const char *cm_multidown;	/* multiple down (DO) */
    const char *cm_multileft;	/* multiple left (LE) */
    const char *cm_multiright;	/* multiple right (RI) */
    int cm_cols;		/* number of cols on screen (co) */
    int cm_rows;		/* number of rows on screen (li) */
    int cm_tabwidth;		/* tab width (it) */
    unsigned int cm_autowrap:1;	/* autowrap flag (am) */
    unsigned int cm_magicwrap:1; /* VT-100: cursor stays in last col but
				    will cm_wrap if next char is
				    printing (xn) */
    unsigned int cm_usetabs:1;	/* if set, use tabs */
    unsigned int cm_losewrap:1;	/* if reach right margin, forget cursor
				   location */
    unsigned int cm_autolf:1;	/* \r performs a \r\n (rn) */

    /* Parameterized capabilities.  This needs to be a struct since
       the costs are accessed through pointers.  */

#if 0
    struct parmcap cc_abs;	/* absolute (cm) */
    struct parmcap cc_habs;	/* horizontal absolute (ch) */
    struct parmcap cc_vabs;	/* vertical absolute (cv) */
    struct parmcap cc_multiup;	/* multiple up (UP) */
    struct parmcap cc_multidown; /* multiple down (DO) */
    struct parmcap cc_multileft; /* multiple left (LE) */
    struct parmcap cc_multiright; /* multiple right (RI) */
#endif

    /* Costs for the non-parameterized capabilities */
    int cc_up;			/* cost for up */
    int cc_down;		/* etc. */
    int cc_left;
    int cc_right;
    int cc_home;
    int cc_cr;
    int cc_ll;
    int cc_tab;
    int cc_backtab;
    /* These are temporary, until the code is installed to use the
       struct parmcap fields above.  */
    int cc_abs;
    int cc_habs;
    int cc_vabs;
  };

/* Shorthand */
#ifndef NoCMShortHand
#define curY(tty)		(tty)->Wcm->cm_curY
#define curX(tty)		(tty)->Wcm->cm_curX
#define Up(tty)			(tty)->Wcm->cm_up
#define Down(tty)		(tty)->Wcm->cm_down
#define Left(tty)		(tty)->Wcm->cm_left
#define Right(tty)		(tty)->Wcm->cm_right
#define Tab(tty)		(tty)->Wcm->cm_tab
#define BackTab(tty)		(tty)->Wcm->cm_backtab
#define TabWidth(tty)		(tty)->Wcm->cm_tabwidth
#define CR(tty)			(tty)->Wcm->cm_cr
#define Home(tty)		(tty)->Wcm->cm_home
#define LastLine(tty)		(tty)->Wcm->cm_ll
#define AbsPosition(tty)	(tty)->Wcm->cm_abs
#define ColPosition(tty)	(tty)->Wcm->cm_habs
#define RowPosition(tty)	(tty)->Wcm->cm_vabs
#define MultiUp(tty)		(tty)->Wcm->cm_multiup
#define MultiDown(tty)		(tty)->Wcm->cm_multidown
#define MultiLeft(tty)		(tty)->Wcm->cm_multileft
#define MultiRight(tty)		(tty)->Wcm->cm_multiright
#define AutoWrap(tty)		(tty)->Wcm->cm_autowrap
#define MagicWrap(tty)		(tty)->Wcm->cm_magicwrap
#define UseTabs(tty)		(tty)->Wcm->cm_usetabs
#define FrameRows(tty)		(tty)->Wcm->cm_rows
#define FrameCols(tty)		(tty)->Wcm->cm_cols

#define UpCost(tty)		(tty)->Wcm->cc_up
#define DownCost(tty)		(tty)->Wcm->cc_down
#define LeftCost(tty)		(tty)->Wcm->cc_left
#define RightCost(tty)		(tty)->Wcm->cc_right
#define HomeCost(tty)		(tty)->Wcm->cc_home
#define CRCost(tty)		(tty)->Wcm->cc_cr
#define LastLineCost(tty)	(tty)->Wcm->cc_ll
#define TabCost(tty)		(tty)->Wcm->cc_tab
#define BackTabCost(tty)	(tty)->Wcm->cc_backtab
#define AbsPositionCost(tty)	(tty)->Wcm->cc_abs
#define ColPositionCost(tty)	(tty)->Wcm->cc_habs
#define RowPositionCost(tty)	(tty)->Wcm->cc_vabs
#define MultiUpCost(tty)	(tty)->Wcm->cc_multiup
#define MultiDownCost(tty)	(tty)->Wcm->cc_multidown
#define MultiLeftCost(tty)	(tty)->Wcm->cc_multileft
#define MultiRightCost(tty)	(tty)->Wcm->cc_multiright
#endif

#define cmat(tty,row,col)	(curY(tty) = (row), curX(tty) = (col))
#define cmplus(tty,n)					            \
  {                                                                 \
    if ((curX (tty) += (n)) >= FrameCols (tty) && !MagicWrap (tty)) \
      {                                                             \
	if ((tty)->Wcm->cm_losewrap) losecursor (tty);              \
	else if (AutoWrap (tty)) curX (tty) = 0, curY (tty)++;      \
	else curX (tty)--;                                          \
      }                                                             \
  }

#define losecursor(tty)	 (curX(tty) = -1, curY(tty) = -1)

extern int cost;
extern int evalcost (int c);

#define emacs_tputs(tty, str, affcnt, putc) (current_tty = (tty), tputs (str, affcnt, putc))

extern struct tty_display_info *current_tty;
extern void cmcheckmagic (struct tty_display_info *);
extern int cmputc (int);
extern void cmcostinit (struct tty_display_info *);
extern void cmgoto (struct tty_display_info *, int, int);
extern void Wcm_clear (struct tty_display_info *);
extern int Wcm_init (struct tty_display_info *);
