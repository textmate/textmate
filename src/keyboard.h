/* Declarations useful when processing input.
   Copyright (C) 1985-1987, 1993, 2001-2012  Free Software Foundation, Inc.

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

#include "systime.h"		/* for EMACS_TIME, Time */
#include "coding.h"             /* for ENCODE_UTF_8 and ENCODE_SYSTEM */

/* Lisp fields in struct keyboard are hidden from most code and accessed
   via the KVAR macro, below.  Only select pieces of code, like the GC,
   are allowed to use KBOARD_INTERNAL_FIELD.  */
#define KBOARD_INTERNAL_FIELD(field) field ## _

/* Most code should use this macro to access Lisp fields in struct
   kboard.  */
#define KVAR(kboard, field) ((kboard)->KBOARD_INTERNAL_FIELD (field))

/* Each KBOARD represents one logical input stream from which Emacs
   gets input.  If we are using ordinary terminals, it has one KBOARD
   object for each terminal device.
   Usually each X display screen has its own KBOARD,
   but when two of them are on the same X server,
   we assume they share a keyboard and give them one KBOARD in common.

   Some Lisp variables are per-kboard; they are stored in the KBOARD structure
   and accessed indirectly via a Lisp_Misc_Kboard_Objfwd object.

   So that definition of keyboard macros, and reading of prefix arguments,
   can happen in parallel on various KBOARDs at once,
   the state information for those activities is stored in the KBOARD.

   Emacs has two states for reading input:

   ** Any kboard.  Emacs can accept input from any KBOARD,
   and as soon as any of them provides a complete command, Emacs can run it.

   ** Single kboard.  Then Emacs is running a command for one KBOARD
   and can only read input from that KBOARD.

   All input, from all KBOARDs, goes together in a single event queue
   at interrupt level.  read_char sees the events sequentially,
   but deals with them in accord with the current input state.

   In the any-kboard state, read_key_sequence processes input from any KBOARD
   immediately.  When a new event comes in from a particular KBOARD,
   read_key_sequence switches to that KBOARD.  As a result,
   as soon as a complete key arrives from some KBOARD or other,
   Emacs starts executing that key's binding.  It switches to the
   single-kboard state for the execution of that command,
   so that that command can get input only from its own KBOARD.

   While in the single-kboard state, read_char can consider input only
   from the current KBOARD.  If events come from other KBOARDs, they
   are put aside for later in the KBOARDs' kbd_queue lists.
   The flag kbd_queue_has_data in a KBOARD is 1 if this has happened.
   When Emacs goes back to the any-kboard state, it looks at all the KBOARDs
   to find those; and it tries processing their input right away.  */

typedef struct kboard KBOARD;
struct kboard
  {
    KBOARD *next_kboard;

    /* If non-nil, a keymap that overrides all others but applies only to
       this KBOARD.  Lisp code that uses this instead of calling read-char
       can effectively wait for input in the any-kboard state, and hence
       avoid blocking out the other KBOARDs.  See universal-argument in
       lisp/simple.el for an example.  */
    Lisp_Object KBOARD_INTERNAL_FIELD (Voverriding_terminal_local_map);

    /* Last command executed by the editor command loop, not counting
       commands that set the prefix argument.  */
    Lisp_Object KBOARD_INTERNAL_FIELD (Vlast_command);

    /* Normally same as last-command, but never modified by other commands.  */
    Lisp_Object KBOARD_INTERNAL_FIELD (Vreal_last_command);

    /* User-supplied table to translate input characters through.  */
    Lisp_Object KBOARD_INTERNAL_FIELD (Vkeyboard_translate_table);

    /* Last command that may be repeated by `repeat'.  */
    Lisp_Object KBOARD_INTERNAL_FIELD (Vlast_repeatable_command);

    /* The prefix argument for the next command, in raw form.  */
    Lisp_Object KBOARD_INTERNAL_FIELD (Vprefix_arg);

    /* Saved prefix argument for the last command, in raw form.  */
    Lisp_Object KBOARD_INTERNAL_FIELD (Vlast_prefix_arg);

    /* Unread events specific to this kboard.  */
    Lisp_Object KBOARD_INTERNAL_FIELD (kbd_queue);

    /* Non-nil while a kbd macro is being defined.  */
    Lisp_Object KBOARD_INTERNAL_FIELD (defining_kbd_macro);

    /* The start of storage for the current keyboard macro.  */
    Lisp_Object *kbd_macro_buffer;

    /* Where to store the next keystroke of the macro.  */
    Lisp_Object *kbd_macro_ptr;

    /* The finalized section of the macro starts at kbd_macro_buffer and
       ends before this.  This is not the same as kbd_macro_ptr, because
       we advance this to kbd_macro_ptr when a key's command is complete.
       This way, the keystrokes for "end-kbd-macro" are not included in the
       macro.  This also allows us to throw away the events added to the
       macro by the last command: all the events between kbd_macro_end and
       kbd_macro_ptr belong to the last command; see
       cancel-kbd-macro-events.  */
    Lisp_Object *kbd_macro_end;

    /* Allocated size of kbd_macro_buffer.  */
    ptrdiff_t kbd_macro_bufsize;

    /* Last anonymous kbd macro defined.  */
    Lisp_Object KBOARD_INTERNAL_FIELD (Vlast_kbd_macro);

    /* Alist of system-specific X windows key symbols.  */
    Lisp_Object KBOARD_INTERNAL_FIELD (Vsystem_key_alist);

    /* Cache for modify_event_symbol.  */
    Lisp_Object KBOARD_INTERNAL_FIELD (system_key_syms);

    /* The kind of display: x, w32, ...  */
    Lisp_Object KBOARD_INTERNAL_FIELD (Vwindow_system);

    /* Keymap mapping keys to alternative preferred forms.
       See the DEFVAR for more documentation.  */
    Lisp_Object KBOARD_INTERNAL_FIELD (Vlocal_function_key_map);

    /* Keymap mapping ASCII function key sequences onto their preferred
       forms.  Initialized by the terminal-specific lisp files.  See the
       DEFVAR for more documentation.  */
    Lisp_Object KBOARD_INTERNAL_FIELD (Vinput_decode_map);

    /* Minibufferless frames on this display use this frame's minibuffer.  */
    Lisp_Object KBOARD_INTERNAL_FIELD (Vdefault_minibuffer_frame);

    /* Number of displays using this KBOARD.  Normally 1, but can be
       larger when you have multiple screens on a single X display.  */
    int reference_count;

    /* The text we're echoing in the modeline - partial key sequences,
       usually.  This is nil when not echoing.  */
    Lisp_Object KBOARD_INTERNAL_FIELD (echo_string);

    /* This flag indicates that events were put into kbd_queue
       while Emacs was running for some other KBOARD.
       The flag means that, when Emacs goes into the any-kboard state again,
       it should check this KBOARD to see if there is a complete command
       waiting.

       Note that the kbd_queue field can be non-nil even when
       kbd_queue_has_data is 0.  When we push back an incomplete
       command, then this flag is 0, meaning we don't want to try
       reading from this KBOARD again until more input arrives.  */
    char kbd_queue_has_data;

    /* Nonzero means echo each character as typed.  */
    char immediate_echo;

    /* If we have echoed a prompt string specified by the user,
       this is its length in characters.  Otherwise this is -1.  */
    char echo_after_prompt;
  };

/* Temporarily used before a frame has been opened. */
extern KBOARD *initial_kboard;

/* In the single-kboard state, this is the kboard
   from which input is accepted.

   In the any-kboard state, this is the kboard from which we are
   right now considering input.  We can consider input from another
   kboard, but doing so requires throwing to wrong_kboard_jmpbuf.  */
extern KBOARD *current_kboard;

/* A list of all kboard objects, linked through next_kboard.  */
extern KBOARD *all_kboards;

/* Total number of times read_char has returned, modulo UINTMAX_MAX + 1.  */
extern uintmax_t num_input_events;

/* Nonzero means polling for input is temporarily suppressed.  */
extern int poll_suppress_count;

/* Vector holding the key sequence that invoked the current command.
   It is reused for each command, and it may be longer than the current
   sequence; this_command_key_count indicates how many elements
   actually mean something.  */
extern Lisp_Object this_command_keys;
extern int this_command_key_count;

/* The frame in which the last input event occurred, or Qmacro if the
   last event came from a macro.  We use this to determine when to
   generate switch-frame events.  This may be cleared by functions
   like Fselect_frame, to make sure that a switch-frame event is
   generated by the next character.  */
extern Lisp_Object internal_last_event_frame;

extern Lisp_Object Qrecompute_lucid_menubar, Qactivate_menubar_hook;

/* This holds a Lisp vector that holds the properties of a single
   menu item while decoding it in parse_menu_item.
   Using a Lisp vector to hold this information while we decode it
   takes care of protecting all the data from GC.  */
extern Lisp_Object item_properties;

/* This describes the elements of item_properties.
   The first element is not a property, it is a pointer to the item properties
   that is saved for GC protection. */
#define ITEM_PROPERTY_ITEM 0
/* The item string.  */
#define ITEM_PROPERTY_NAME 1
/* Start of initialize to nil */
/* The binding: nil, a command or a keymap.  */
#define ITEM_PROPERTY_DEF 2
/* The keymap if the binding is a keymap, otherwise nil.  */
#define ITEM_PROPERTY_MAP 3
/* Nil, :radio or :toggle.  */
#define ITEM_PROPERTY_TYPE 4
/* Nil or a string describing an equivalent key binding.  */
#define ITEM_PROPERTY_KEYEQ 5
/* Not nil if a selected toggle box or radio button, otherwise nil.  */
#define ITEM_PROPERTY_SELECTED 6
/* Place for a help string. Not yet used.  */
#define ITEM_PROPERTY_HELP 7
/* Start of initialize to t */
/* Last property. */
/* Not nil if item is enabled.  */
#define ITEM_PROPERTY_ENABLE 8

/* This holds a Lisp vector that holds the results of decoding
   the keymaps or alist-of-alists that specify a menu.

   It describes the panes and items within the panes.

   Each pane is described by 3 elements in the vector:
   t, the pane name, the pane's prefix key.
   Then follow the pane's items, with 5 elements per item:
   the item string, the enable flag, the item's value,
   the definition, and the equivalent keyboard key's description string.

   In some cases, multiple levels of menus may be described.
   A single vector slot containing nil indicates the start of a submenu.
   A single vector slot containing lambda indicates the end of a submenu.
   The submenu follows a menu item which is the way to reach the submenu.

   A single vector slot containing quote indicates that the
   following items should appear on the right of a dialog box.

   Using a Lisp vector to hold this information while we decode it
   takes care of protecting all the data from GC.  */
extern Lisp_Object menu_items;

/* If non-nil, means that the global vars defined here are already in use.
   Used to detect cases where we try to re-enter this non-reentrant code.  */
#if defined USE_GTK || defined USE_MOTIF
extern Lisp_Object menu_items_inuse;
#endif

/* Number of slots currently allocated in menu_items.  */
extern int menu_items_allocated;

/* This is the index in menu_items of the first empty slot.  */
extern int menu_items_used;

/* The number of panes currently recorded in menu_items,
   excluding those within submenus.  */
extern int menu_items_n_panes;

#define MENU_ITEMS_PANE_NAME 1
#define MENU_ITEMS_PANE_PREFIX 2
#define MENU_ITEMS_PANE_LENGTH 3

enum menu_item_idx
{
  MENU_ITEMS_ITEM_NAME = 0,
  MENU_ITEMS_ITEM_ENABLE,
  MENU_ITEMS_ITEM_VALUE,
  MENU_ITEMS_ITEM_EQUIV_KEY,
  MENU_ITEMS_ITEM_DEFINITION,
  MENU_ITEMS_ITEM_TYPE,
  MENU_ITEMS_ITEM_SELECTED,
  MENU_ITEMS_ITEM_HELP,
  MENU_ITEMS_ITEM_LENGTH
};

extern Lisp_Object unuse_menu_items (Lisp_Object dummy);

/* This is how to deal with multibyte text if HAVE_MULTILINGUAL_MENU
   isn't defined.  The use of HAVE_MULTILINGUAL_MENU could probably be
   confined to an extended version of this with sections of code below
   using it unconditionally.  */
#ifndef HAVE_NTGUI
#if defined (USE_GTK) || defined (HAVE_NS)
# define ENCODE_MENU_STRING(str) ENCODE_UTF_8 (str)
#elif defined HAVE_X_I18N
#define ENCODE_MENU_STRING(str) ENCODE_SYSTEM (str)
#else
#define ENCODE_MENU_STRING(str) string_make_unibyte (str)
#endif /* USE_GTK  */
#else /* HAVE_NTGUI */
#define ENCODE_MENU_STRING(str) (str)
#endif

#if defined (HAVE_NS) || defined (HAVE_NTGUI) || defined (USE_GTK)

/* Definitions copied from lwlib.h */

enum button_type
{
  BUTTON_TYPE_NONE,
  BUTTON_TYPE_TOGGLE,
  BUTTON_TYPE_RADIO
};

/* This structure is based on the one in ../lwlib/lwlib.h, with unused portions
   removed.  No term uses these. */
typedef struct _widget_value
{
  /* name of widget */
  Lisp_Object   lname;
  const char*	name;
  /* value (meaning depend on widget type) */
  const char*	value;
  /* keyboard equivalent. no implications for XtTranslations */
  Lisp_Object   lkey;
  const char*	key;
  /* Help string or nil if none.
     GC finds this string through the frame's menu_bar_vector
     or through menu_items.  */
  Lisp_Object	help;
  /* true if enabled */
  unsigned char	enabled;
  /* true if selected */
  unsigned char selected;
  /* The type of a button.  */
  enum button_type button_type;
#if defined (HAVE_NTGUI)
  /* true if menu title */
  unsigned char title;
#endif
  /* Contents of the sub-widgets, also selected slot for checkbox */
  struct _widget_value*	contents;
  /* data passed to callback */
  void	*call_data;
  /* next one in the list */
  struct _widget_value*	next;
#ifdef USE_GTK
  struct _widget_value *free_list;
#endif
} widget_value;

#endif /* HAVE_NS || HAVE_NTGUI */


/* Macros for dealing with lispy events.  */

/* True if EVENT has data fields describing it (i.e. a mouse click).  */
#define EVENT_HAS_PARAMETERS(event) (CONSP (event))

/* Extract the head from an event.
   This works on composite and simple events.  */
#define EVENT_HEAD(event) \
  (EVENT_HAS_PARAMETERS (event) ? XCAR (event) : (event))

/* Extract the starting and ending positions from a composite event.  */
#define EVENT_START(event) (XCAR (XCDR (event)))
#define EVENT_END(event) (XCAR (XCDR (XCDR (event))))

/* Extract the click count from a multi-click event.  */
#define EVENT_CLICK_COUNT(event) (Fnth (make_number (2), (event)))

/* Extract the fields of a position.  */
#define POSN_WINDOW(posn) (XCAR (posn))
#define POSN_POSN(posn) (XCAR (XCDR (posn)))
#define POSN_SET_POSN(posn,x) (XSETCAR (XCDR (posn), (x)))
#define POSN_WINDOW_POSN(posn) (XCAR (XCDR (XCDR (posn))))
#define POSN_TIMESTAMP(posn) (XCAR (XCDR (XCDR (XCDR (posn)))))
#define POSN_SCROLLBAR_PART(posn)	(Fnth (make_number (4), (posn)))

/* A cons (STRING . STRING-CHARPOS), or nil in mouse-click events.
   It's a cons if the click is over a string in the mode line.  */

#define POSN_STRING(posn) (Fnth (make_number (4), (posn)))

/* If POSN_STRING is nil, event refers to buffer location.  */

#define POSN_INBUFFER_P(posn) (NILP (POSN_STRING (posn)))
#define POSN_BUFFER_POSN(posn) (Fnth (make_number (5), (posn)))

/* Some of the event heads.  */
extern Lisp_Object Qswitch_frame;

/* Properties on event heads.  */
extern Lisp_Object Qevent_kind;

/* The values of Qevent_kind properties.  */
extern Lisp_Object Qmouse_click;

extern Lisp_Object Qhelp_echo;

/* Getting the kind of an event head.  */
#define EVENT_HEAD_KIND(event_head) \
  (Fget ((event_head), Qevent_kind))

/* Symbols to use for non-text mouse positions.  */
extern Lisp_Object Qmode_line, Qvertical_line, Qheader_line;

/* True while doing kbd input.  */
extern int waiting_for_input;

/* Address (if not 0) of EMACS_TIME to zero out if a SIGIO interrupt
   happens.  */
extern EMACS_TIME *input_available_clear_time;

#if defined HAVE_WINDOW_SYSTEM && !defined USE_GTK && !defined HAVE_NS
extern int ignore_mouse_drag_p;
#endif

/* The primary selection.  */
extern Lisp_Object QPRIMARY;

/* Forward declaration for prototypes.  */
struct input_event;

extern Lisp_Object parse_modifiers (Lisp_Object);
extern Lisp_Object reorder_modifiers (Lisp_Object);
extern Lisp_Object read_char (int, ptrdiff_t, Lisp_Object *, Lisp_Object,
                              int *, EMACS_TIME *);
extern int parse_solitary_modifier (Lisp_Object symbol);


/* This is like Vthis_command, except that commands never set it.  */
extern Lisp_Object real_this_command;

/* Non-nil disable property on a command means
   do not execute it; call disabled-command-function's value instead.  */
extern Lisp_Object QCtoggle, QCradio;

/* An event header symbol HEAD may have a property named
   Qevent_symbol_element_mask, which is of the form (BASE MODIFIERS);
   BASE is the base, unmodified version of HEAD, and MODIFIERS is the
   mask of modifiers applied to it.  If present, this is used to help
   speed up parse_modifiers.  */
extern Lisp_Object Qevent_symbol_element_mask;

/* The timestamp of the last input event we received from the X server.
   X Windows wants this for selection ownership.  */
extern Time last_event_timestamp;

extern int quit_char;

extern int timers_run;

extern int menu_separator_name_p (const char *);
extern int parse_menu_item (Lisp_Object, int);

extern void init_kboard (KBOARD *);
extern void delete_kboard (KBOARD *);
extern void not_single_kboard_state (KBOARD *);
extern void push_kboard (struct kboard *);
extern void push_frame_kboard (struct frame *);
extern void pop_kboard (void);
extern void temporarily_switch_to_single_kboard (struct frame *);
extern void record_asynch_buffer_change (void);
extern void input_poll_signal (int);
extern void start_polling (void);
extern void stop_polling (void);
extern void set_poll_suppress_count (int);
extern void gobble_input (int);
extern int input_polling_used (void);
extern void clear_input_pending (void);
extern int requeued_events_pending_p (void);
extern void bind_polling_period (int);
extern int make_ctrl_char (int);
extern void stuff_buffered_input (Lisp_Object);
extern void clear_waiting_for_input (void);
extern void swallow_events (int);
extern int lucid_event_type_list_p (Lisp_Object);
extern void kbd_buffer_store_event (struct input_event *);
extern void kbd_buffer_store_event_hold (struct input_event *,
                                         struct input_event *);
extern void kbd_buffer_unget_event (struct input_event *);
extern void poll_for_input_1 (void);
extern void show_help_echo (Lisp_Object, Lisp_Object, Lisp_Object,
                            Lisp_Object);
extern void gen_help_event (Lisp_Object, Lisp_Object, Lisp_Object,
                            Lisp_Object, EMACS_INT);
extern void kbd_buffer_store_help_event (Lisp_Object, Lisp_Object);
extern Lisp_Object menu_item_eval_property (Lisp_Object);
extern int  kbd_buffer_events_waiting (int);
extern void add_user_signal (int, const char *);

extern int tty_read_avail_input (struct terminal *, int,
                                 struct input_event *);
extern EMACS_TIME timer_check (void);
extern void mark_kboards (void);

#ifdef WINDOWSNT
extern const char *const lispy_function_keys[];
#endif
