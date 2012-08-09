/* Block-relocating memory allocator.
   Copyright (C) 1993, 1995, 2000-2012  Free Software Foundation, Inc.

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

/* NOTES:

   Only relocate the blocs necessary for SIZE in r_alloc_sbrk,
   rather than all of them.  This means allowing for a possible
   hole between the first bloc and the end of malloc storage.  */

#ifdef emacs

#include <config.h>
#include <setjmp.h>
#include "lisp.h"		/* Needed for VALBITS.  */
#include "blockinput.h"

#include <unistd.h>

typedef POINTER_TYPE *POINTER;
typedef size_t SIZE;

#ifdef DOUG_LEA_MALLOC
#define M_TOP_PAD           -2
extern int mallopt (int, int);
#else /* not DOUG_LEA_MALLOC */
#ifndef SYSTEM_MALLOC
extern size_t __malloc_extra_blocks;
#endif /* SYSTEM_MALLOC */
#endif /* not DOUG_LEA_MALLOC */

#else /* not emacs */

#include <stddef.h>

typedef size_t SIZE;
typedef void *POINTER;

#include <unistd.h>
#include <malloc.h>

#endif	/* not emacs */


#include "getpagesize.h"

#define NIL ((POINTER) 0)

/* A flag to indicate whether we have initialized ralloc yet.  For
   Emacs's sake, please do not make this local to malloc_init; on some
   machines, the dumping procedure makes all static variables
   read-only.  On these machines, the word static is #defined to be
   the empty string, meaning that r_alloc_initialized becomes an
   automatic variable, and loses its value each time Emacs is started
   up.  */

static int r_alloc_initialized = 0;

static void r_alloc_init (void);


/* Declarations for working with the malloc, ralloc, and system breaks.  */

/* Function to set the real break value.  */
POINTER (*real_morecore) (long int);

/* The break value, as seen by malloc.  */
static POINTER virtual_break_value;

/* The address of the end of the last data in use by ralloc,
   including relocatable blocs as well as malloc data.  */
static POINTER break_value;

/* This is the size of a page.  We round memory requests to this boundary.  */
static int page_size;

/* Whenever we get memory from the system, get this many extra bytes.  This
   must be a multiple of page_size.  */
static int extra_bytes;

/* Macros for rounding.  Note that rounding to any value is possible
   by changing the definition of PAGE.  */
#define PAGE (getpagesize ())
#define ALIGNED(addr) (((unsigned long int) (addr) & (page_size - 1)) == 0)
#define ROUNDUP(size) (((unsigned long int) (size) + page_size - 1) \
		       & ~(page_size - 1))
#define ROUND_TO_PAGE(addr) (addr & (~(page_size - 1)))

#define MEM_ALIGN sizeof (double)
#define MEM_ROUNDUP(addr) (((unsigned long int)(addr) + MEM_ALIGN - 1) \
				   & ~(MEM_ALIGN - 1))

/* The hook `malloc' uses for the function which gets more space
   from the system.  */

#ifndef SYSTEM_MALLOC
extern POINTER (*__morecore) (long int);
#endif



/***********************************************************************
		      Implementation using sbrk
 ***********************************************************************/

/* Data structures of heaps and blocs.  */

/* The relocatable objects, or blocs, and the malloc data
   both reside within one or more heaps.
   Each heap contains malloc data, running from `start' to `bloc_start',
   and relocatable objects, running from `bloc_start' to `free'.

   Relocatable objects may relocate within the same heap
   or may move into another heap; the heaps themselves may grow
   but they never move.

   We try to make just one heap and make it larger as necessary.
   But sometimes we can't do that, because we can't get contiguous
   space to add onto the heap.  When that happens, we start a new heap.  */

typedef struct heap
{
  struct heap *next;
  struct heap *prev;
  /* Start of memory range of this heap.  */
  POINTER start;
  /* End of memory range of this heap.  */
  POINTER end;
  /* Start of relocatable data in this heap.  */
  POINTER bloc_start;
  /* Start of unused space in this heap.  */
  POINTER free;
  /* First bloc in this heap.  */
  struct bp *first_bloc;
  /* Last bloc in this heap.  */
  struct bp *last_bloc;
} *heap_ptr;

#define NIL_HEAP ((heap_ptr) 0)
#define HEAP_PTR_SIZE (sizeof (struct heap))

/* This is the first heap object.
   If we need additional heap objects, each one resides at the beginning of
   the space it covers.   */
static struct heap heap_base;

/* Head and tail of the list of heaps.  */
static heap_ptr first_heap, last_heap;

/* These structures are allocated in the malloc arena.
   The linked list is kept in order of increasing '.data' members.
   The data blocks abut each other; if b->next is non-nil, then
   b->data + b->size == b->next->data.

   An element with variable==NIL denotes a freed block, which has not yet
   been collected.  They may only appear while r_alloc_freeze_level > 0,
   and will be freed when the arena is thawed.  Currently, these blocs are
   not reusable, while the arena is frozen.  Very inefficient.  */

typedef struct bp
{
  struct bp *next;
  struct bp *prev;
  POINTER *variable;
  POINTER data;
  SIZE size;
  POINTER new_data;		/* temporarily used for relocation */
  struct heap *heap; 		/* Heap this bloc is in.  */
} *bloc_ptr;

#define NIL_BLOC ((bloc_ptr) 0)
#define BLOC_PTR_SIZE (sizeof (struct bp))

/* Head and tail of the list of relocatable blocs.  */
static bloc_ptr first_bloc, last_bloc;

static int use_relocatable_buffers;

/* If >0, no relocation whatsoever takes place.  */
static int r_alloc_freeze_level;


/* Functions to get and return memory from the system.  */

/* Find the heap that ADDRESS falls within.  */

static heap_ptr
find_heap (POINTER address)
{
  heap_ptr heap;

  for (heap = last_heap; heap; heap = heap->prev)
    {
      if (heap->start <= address && address <= heap->end)
	return heap;
    }

  return NIL_HEAP;
}

/* Find SIZE bytes of space in a heap.
   Try to get them at ADDRESS (which must fall within some heap's range)
   if we can get that many within one heap.

   If enough space is not presently available in our reserve, this means
   getting more page-aligned space from the system.  If the returned space
   is not contiguous to the last heap, allocate a new heap, and append it
   to the heap list.

   obtain does not try to keep track of whether space is in use or not
   in use.  It just returns the address of SIZE bytes that fall within a
   single heap.  If you call obtain twice in a row with the same arguments,
   you typically get the same value.  It's the caller's responsibility to
   keep track of what space is in use.

   Return the address of the space if all went well, or zero if we couldn't
   allocate the memory.  */

static POINTER
obtain (POINTER address, SIZE size)
{
  heap_ptr heap;
  SIZE already_available;

  /* Find the heap that ADDRESS falls within.  */
  for (heap = last_heap; heap; heap = heap->prev)
    {
      if (heap->start <= address && address <= heap->end)
	break;
    }

  if (! heap)
    abort ();

  /* If we can't fit SIZE bytes in that heap,
     try successive later heaps.  */
  while (heap && (char *) address + size > (char *) heap->end)
    {
      heap = heap->next;
      if (heap == NIL_HEAP)
	break;
      address = heap->bloc_start;
    }

  /* If we can't fit them within any existing heap,
     get more space.  */
  if (heap == NIL_HEAP)
    {
      POINTER new = (*real_morecore)(0);
      SIZE get;

      already_available = (char *)last_heap->end - (char *)address;

      if (new != last_heap->end)
	{
	  /* Someone else called sbrk.  Make a new heap.  */

	  heap_ptr new_heap = (heap_ptr) MEM_ROUNDUP (new);
	  POINTER bloc_start = (POINTER) MEM_ROUNDUP ((POINTER)(new_heap + 1));

	  if ((*real_morecore) ((char *) bloc_start - (char *) new) != new)
	    return 0;

	  new_heap->start = new;
	  new_heap->end = bloc_start;
	  new_heap->bloc_start = bloc_start;
	  new_heap->free = bloc_start;
	  new_heap->next = NIL_HEAP;
	  new_heap->prev = last_heap;
	  new_heap->first_bloc = NIL_BLOC;
	  new_heap->last_bloc = NIL_BLOC;
	  last_heap->next = new_heap;
	  last_heap = new_heap;

	  address = bloc_start;
	  already_available = 0;
	}

      /* Add space to the last heap (which we may have just created).
	 Get some extra, so we can come here less often.  */

      get = size + extra_bytes - already_available;
      get = (char *) ROUNDUP ((char *)last_heap->end + get)
	- (char *) last_heap->end;

      if ((*real_morecore) (get) != last_heap->end)
	return 0;

      last_heap->end = (char *) last_heap->end + get;
    }

  return address;
}

/* Return unused heap space to the system
   if there is a lot of unused space now.
   This can make the last heap smaller;
   it can also eliminate the last heap entirely.  */

static void
relinquish (void)
{
  register heap_ptr h;
  long excess = 0;

  /* Add the amount of space beyond break_value
     in all heaps which have extend beyond break_value at all.  */

  for (h = last_heap; h && break_value < h->end; h = h->prev)
    {
      excess += (char *) h->end - (char *) ((break_value < h->bloc_start)
					    ? h->bloc_start : break_value);
    }

  if (excess > extra_bytes * 2 && (*real_morecore) (0) == last_heap->end)
    {
      /* Keep extra_bytes worth of empty space.
	 And don't free anything unless we can free at least extra_bytes.  */
      excess -= extra_bytes;

      if ((char *)last_heap->end - (char *)last_heap->bloc_start <= excess)
	{
	  /* This heap should have no blocs in it.  */
	  if (last_heap->first_bloc != NIL_BLOC
	      || last_heap->last_bloc != NIL_BLOC)
	    abort ();

	  /* Return the last heap, with its header, to the system.  */
	  excess = (char *)last_heap->end - (char *)last_heap->start;
	  last_heap = last_heap->prev;
	  last_heap->next = NIL_HEAP;
	}
      else
	{
	  excess = (char *) last_heap->end
			- (char *) ROUNDUP ((char *)last_heap->end - excess);
	  last_heap->end = (char *) last_heap->end - excess;
	}

      if ((*real_morecore) (- excess) == 0)
	{
	  /* If the system didn't want that much memory back, adjust
             the end of the last heap to reflect that.  This can occur
             if break_value is still within the original data segment.  */
	  last_heap->end = (char *) last_heap->end + excess;
	  /* Make sure that the result of the adjustment is accurate.
             It should be, for the else clause above; the other case,
             which returns the entire last heap to the system, seems
             unlikely to trigger this mode of failure.  */
	  if (last_heap->end != (*real_morecore) (0))
	    abort ();
	}
    }
}

/* Return the total size in use by relocating allocator,
   above where malloc gets space.  */

long
r_alloc_size_in_use (void)
{
  return (char *) break_value - (char *) virtual_break_value;
}

/* The meat - allocating, freeing, and relocating blocs.  */

/* Find the bloc referenced by the address in PTR.  Returns a pointer
   to that block.  */

static bloc_ptr
find_bloc (POINTER *ptr)
{
  register bloc_ptr p = first_bloc;

  while (p != NIL_BLOC)
    {
      /* Consistency check. Don't return inconsistent blocs.
	 Don't abort here, as callers might be expecting this, but
	 callers that always expect a bloc to be returned should abort
	 if one isn't to avoid a memory corruption bug that is
	 difficult to track down.  */
      if (p->variable == ptr && p->data == *ptr)
	return p;

      p = p->next;
    }

  return p;
}

/* Allocate a bloc of SIZE bytes and append it to the chain of blocs.
   Returns a pointer to the new bloc, or zero if we couldn't allocate
   memory for the new block.  */

static bloc_ptr
get_bloc (SIZE size)
{
  register bloc_ptr new_bloc;
  register heap_ptr heap;

  if (! (new_bloc = (bloc_ptr) malloc (BLOC_PTR_SIZE))
      || ! (new_bloc->data = obtain (break_value, size)))
    {
      free (new_bloc);

      return 0;
    }

  break_value = (char *) new_bloc->data + size;

  new_bloc->size = size;
  new_bloc->next = NIL_BLOC;
  new_bloc->variable = (POINTER *) NIL;
  new_bloc->new_data = 0;

  /* Record in the heap that this space is in use.  */
  heap = find_heap (new_bloc->data);
  heap->free = break_value;

  /* Maintain the correspondence between heaps and blocs.  */
  new_bloc->heap = heap;
  heap->last_bloc = new_bloc;
  if (heap->first_bloc == NIL_BLOC)
    heap->first_bloc = new_bloc;

  /* Put this bloc on the doubly-linked list of blocs.  */
  if (first_bloc)
    {
      new_bloc->prev = last_bloc;
      last_bloc->next = new_bloc;
      last_bloc = new_bloc;
    }
  else
    {
      first_bloc = last_bloc = new_bloc;
      new_bloc->prev = NIL_BLOC;
    }

  return new_bloc;
}

/* Calculate new locations of blocs in the list beginning with BLOC,
   relocating it to start at ADDRESS, in heap HEAP.  If enough space is
   not presently available in our reserve, call obtain for
   more space.

   Store the new location of each bloc in its new_data field.
   Do not touch the contents of blocs or break_value.  */

static int
relocate_blocs (bloc_ptr bloc, heap_ptr heap, POINTER address)
{
  register bloc_ptr b = bloc;

  /* No need to ever call this if arena is frozen, bug somewhere!  */
  if (r_alloc_freeze_level)
    abort ();

  while (b)
    {
      /* If bloc B won't fit within HEAP,
	 move to the next heap and try again.  */
      while (heap && (char *) address + b->size > (char *) heap->end)
	{
	  heap = heap->next;
	  if (heap == NIL_HEAP)
	    break;
	  address = heap->bloc_start;
	}

      /* If BLOC won't fit in any heap,
	 get enough new space to hold BLOC and all following blocs.  */
      if (heap == NIL_HEAP)
	{
	  register bloc_ptr tb = b;
	  register SIZE s = 0;

	  /* Add up the size of all the following blocs.  */
	  while (tb != NIL_BLOC)
	    {
	      if (tb->variable)
		s += tb->size;

	      tb = tb->next;
	    }

	  /* Get that space.  */
	  address = obtain (address, s);
	  if (address == 0)
	    return 0;

	  heap = last_heap;
	}

      /* Record the new address of this bloc
	 and update where the next bloc can start.  */
      b->new_data = address;
      if (b->variable)
	address = (char *) address + b->size;
      b = b->next;
    }

  return 1;
}

/* Update the records of which heaps contain which blocs, starting
   with heap HEAP and bloc BLOC.  */

static void
update_heap_bloc_correspondence (bloc_ptr bloc, heap_ptr heap)
{
  register bloc_ptr b;

  /* Initialize HEAP's status to reflect blocs before BLOC.  */
  if (bloc != NIL_BLOC && bloc->prev != NIL_BLOC && bloc->prev->heap == heap)
    {
      /* The previous bloc is in HEAP.  */
      heap->last_bloc = bloc->prev;
      heap->free = (char *) bloc->prev->data + bloc->prev->size;
    }
  else
    {
      /* HEAP contains no blocs before BLOC.  */
      heap->first_bloc = NIL_BLOC;
      heap->last_bloc = NIL_BLOC;
      heap->free = heap->bloc_start;
    }

  /* Advance through blocs one by one.  */
  for (b = bloc; b != NIL_BLOC; b = b->next)
    {
      /* Advance through heaps, marking them empty,
	 till we get to the one that B is in.  */
      while (heap)
	{
	  if (heap->bloc_start <= b->data && b->data <= heap->end)
	    break;
	  heap = heap->next;
	  /* We know HEAP is not null now,
	     because there has to be space for bloc B.  */
	  heap->first_bloc = NIL_BLOC;
	  heap->last_bloc = NIL_BLOC;
	  heap->free = heap->bloc_start;
	}

      /* Update HEAP's status for bloc B.  */
      heap->free = (char *) b->data + b->size;
      heap->last_bloc = b;
      if (heap->first_bloc == NIL_BLOC)
	heap->first_bloc = b;

      /* Record that B is in HEAP.  */
      b->heap = heap;
    }

  /* If there are any remaining heaps and no blocs left,
     mark those heaps as empty.  */
  heap = heap->next;
  while (heap)
    {
      heap->first_bloc = NIL_BLOC;
      heap->last_bloc = NIL_BLOC;
      heap->free = heap->bloc_start;
      heap = heap->next;
    }
}

/* Resize BLOC to SIZE bytes.  This relocates the blocs
   that come after BLOC in memory.  */

static int
resize_bloc (bloc_ptr bloc, SIZE size)
{
  register bloc_ptr b;
  heap_ptr heap;
  POINTER address;
  SIZE old_size;

  /* No need to ever call this if arena is frozen, bug somewhere!  */
  if (r_alloc_freeze_level)
    abort ();

  if (bloc == NIL_BLOC || size == bloc->size)
    return 1;

  for (heap = first_heap; heap != NIL_HEAP; heap = heap->next)
    {
      if (heap->bloc_start <= bloc->data && bloc->data <= heap->end)
	break;
    }

  if (heap == NIL_HEAP)
    abort ();

  old_size = bloc->size;
  bloc->size = size;

  /* Note that bloc could be moved into the previous heap.  */
  address = (bloc->prev ? (char *) bloc->prev->data + bloc->prev->size
	     : (char *) first_heap->bloc_start);
  while (heap)
    {
      if (heap->bloc_start <= address && address <= heap->end)
	break;
      heap = heap->prev;
    }

  if (! relocate_blocs (bloc, heap, address))
    {
      bloc->size = old_size;
      return 0;
    }

  if (size > old_size)
    {
      for (b = last_bloc; b != bloc; b = b->prev)
	{
	  if (!b->variable)
	    {
	      b->size = 0;
	      b->data = b->new_data;
            }
	  else
	    {
	      if (b->new_data != b->data)
		memmove (b->new_data, b->data, b->size);
	      *b->variable = b->data = b->new_data;
            }
	}
      if (!bloc->variable)
	{
	  bloc->size = 0;
	  bloc->data = bloc->new_data;
	}
      else
	{
	  if (bloc->new_data != bloc->data)
	    memmove (bloc->new_data, bloc->data, old_size);
	  memset ((char *) bloc->new_data + old_size, 0, size - old_size);
	  *bloc->variable = bloc->data = bloc->new_data;
	}
    }
  else
    {
      for (b = bloc; b != NIL_BLOC; b = b->next)
	{
	  if (!b->variable)
	    {
	      b->size = 0;
	      b->data = b->new_data;
            }
	  else
	    {
	      if (b->new_data != b->data)
		memmove (b->new_data, b->data, b->size);
	      *b->variable = b->data = b->new_data;
	    }
	}
    }

  update_heap_bloc_correspondence (bloc, heap);

  break_value = (last_bloc ? (char *) last_bloc->data + last_bloc->size
		 : (char *) first_heap->bloc_start);
  return 1;
}

/* Free BLOC from the chain of blocs, relocating any blocs above it.
   This may return space to the system.  */

static void
free_bloc (bloc_ptr bloc)
{
  heap_ptr heap = bloc->heap;

  if (r_alloc_freeze_level)
    {
      bloc->variable = (POINTER *) NIL;
      return;
    }

  resize_bloc (bloc, 0);

  if (bloc == first_bloc && bloc == last_bloc)
    {
      first_bloc = last_bloc = NIL_BLOC;
    }
  else if (bloc == last_bloc)
    {
      last_bloc = bloc->prev;
      last_bloc->next = NIL_BLOC;
    }
  else if (bloc == first_bloc)
    {
      first_bloc = bloc->next;
      first_bloc->prev = NIL_BLOC;
    }
  else
    {
      bloc->next->prev = bloc->prev;
      bloc->prev->next = bloc->next;
    }

  /* Update the records of which blocs are in HEAP.  */
  if (heap->first_bloc == bloc)
    {
      if (bloc->next != 0 && bloc->next->heap == heap)
	heap->first_bloc = bloc->next;
      else
	heap->first_bloc = heap->last_bloc = NIL_BLOC;
    }
  if (heap->last_bloc == bloc)
    {
      if (bloc->prev != 0 && bloc->prev->heap == heap)
	heap->last_bloc = bloc->prev;
      else
	heap->first_bloc = heap->last_bloc = NIL_BLOC;
    }

  relinquish ();
  free (bloc);
}

/* Interface routines.  */

/* Obtain SIZE bytes of storage from the free pool, or the system, as
   necessary.  If relocatable blocs are in use, this means relocating
   them.  This function gets plugged into the GNU malloc's __morecore
   hook.

   We provide hysteresis, never relocating by less than extra_bytes.

   If we're out of memory, we should return zero, to imitate the other
   __morecore hook values - in particular, __default_morecore in the
   GNU malloc package.  */

POINTER
r_alloc_sbrk (long int size)
{
  register bloc_ptr b;
  POINTER address;

  if (! r_alloc_initialized)
    r_alloc_init ();

  if (! use_relocatable_buffers)
    return (*real_morecore) (size);

  if (size == 0)
    return virtual_break_value;

  if (size > 0)
    {
      /* Allocate a page-aligned space.  GNU malloc would reclaim an
	 extra space if we passed an unaligned one.  But we could
	 not always find a space which is contiguous to the previous.  */
      POINTER new_bloc_start;
      heap_ptr h = first_heap;
      SIZE get = ROUNDUP (size);

      address = (POINTER) ROUNDUP (virtual_break_value);

      /* Search the list upward for a heap which is large enough.  */
      while ((char *) h->end < (char *) MEM_ROUNDUP ((char *)address + get))
	{
	  h = h->next;
	  if (h == NIL_HEAP)
	    break;
	  address = (POINTER) ROUNDUP (h->start);
	}

      /* If not found, obtain more space.  */
      if (h == NIL_HEAP)
	{
	  get += extra_bytes + page_size;

	  if (! obtain (address, get))
	    return 0;

	  if (first_heap == last_heap)
	    address = (POINTER) ROUNDUP (virtual_break_value);
	  else
	    address = (POINTER) ROUNDUP (last_heap->start);
	  h = last_heap;
	}

      new_bloc_start = (POINTER) MEM_ROUNDUP ((char *)address + get);

      if (first_heap->bloc_start < new_bloc_start)
	{
	  /* This is no clean solution - no idea how to do it better.  */
	  if (r_alloc_freeze_level)
	    return NIL;

	  /* There is a bug here: if the above obtain call succeeded, but the
	     relocate_blocs call below does not succeed, we need to free
	     the memory that we got with obtain.  */

	  /* Move all blocs upward.  */
	  if (! relocate_blocs (first_bloc, h, new_bloc_start))
	    return 0;

	  /* Note that (POINTER)(h+1) <= new_bloc_start since
	     get >= page_size, so the following does not destroy the heap
	     header.  */
	  for (b = last_bloc; b != NIL_BLOC; b = b->prev)
	    {
	      if (b->new_data != b->data)
		memmove (b->new_data, b->data, b->size);
	      *b->variable = b->data = b->new_data;
	    }

	  h->bloc_start = new_bloc_start;

	  update_heap_bloc_correspondence (first_bloc, h);
	}
      if (h != first_heap)
	{
	  /* Give up managing heaps below the one the new
	     virtual_break_value points to.  */
	  first_heap->prev = NIL_HEAP;
	  first_heap->next = h->next;
	  first_heap->start = h->start;
	  first_heap->end = h->end;
	  first_heap->free = h->free;
	  first_heap->first_bloc = h->first_bloc;
	  first_heap->last_bloc = h->last_bloc;
	  first_heap->bloc_start = h->bloc_start;

	  if (first_heap->next)
	    first_heap->next->prev = first_heap;
	  else
	    last_heap = first_heap;
	}

      memset (address, 0, size);
    }
  else /* size < 0 */
    {
      SIZE excess = (char *)first_heap->bloc_start
		      - ((char *)virtual_break_value + size);

      address = virtual_break_value;

      if (r_alloc_freeze_level == 0 && excess > 2 * extra_bytes)
	{
	  excess -= extra_bytes;
	  first_heap->bloc_start
	    = (POINTER) MEM_ROUNDUP ((char *)first_heap->bloc_start - excess);

	  relocate_blocs (first_bloc, first_heap, first_heap->bloc_start);

	  for (b = first_bloc; b != NIL_BLOC; b = b->next)
	    {
	      if (b->new_data != b->data)
		memmove (b->new_data, b->data, b->size);
	      *b->variable = b->data = b->new_data;
	    }
	}

      if ((char *)virtual_break_value + size < (char *)first_heap->start)
	{
	  /* We found an additional space below the first heap */
	  first_heap->start = (POINTER) ((char *)virtual_break_value + size);
	}
    }

  virtual_break_value = (POINTER) ((char *)address + size);
  break_value = (last_bloc
		 ? (char *) last_bloc->data + last_bloc->size
		 : (char *) first_heap->bloc_start);
  if (size < 0)
    relinquish ();

  return address;
}


/* Allocate a relocatable bloc of storage of size SIZE.  A pointer to
   the data is returned in *PTR.  PTR is thus the address of some variable
   which will use the data area.

   The allocation of 0 bytes is valid.
   In case r_alloc_freeze_level is set, a best fit of unused blocs could be
   done before allocating a new area.  Not yet done.

   If we can't allocate the necessary memory, set *PTR to zero, and
   return zero.  */

POINTER
r_alloc (POINTER *ptr, SIZE size)
{
  register bloc_ptr new_bloc;

  if (! r_alloc_initialized)
    r_alloc_init ();

  new_bloc = get_bloc (MEM_ROUNDUP (size));
  if (new_bloc)
    {
      new_bloc->variable = ptr;
      *ptr = new_bloc->data;
    }
  else
    *ptr = 0;

  return *ptr;
}

/* Free a bloc of relocatable storage whose data is pointed to by PTR.
   Store 0 in *PTR to show there's no block allocated.  */

void
r_alloc_free (register POINTER *ptr)
{
  register bloc_ptr dead_bloc;

  if (! r_alloc_initialized)
    r_alloc_init ();

  dead_bloc = find_bloc (ptr);
  if (dead_bloc == NIL_BLOC)
    abort (); /* Double free? PTR not originally used to allocate?  */

  free_bloc (dead_bloc);
  *ptr = 0;

#ifdef emacs
  refill_memory_reserve ();
#endif
}

/* Given a pointer at address PTR to relocatable data, resize it to SIZE.
   Do this by shifting all blocks above this one up in memory, unless
   SIZE is less than or equal to the current bloc size, in which case
   do nothing.

   In case r_alloc_freeze_level is set, a new bloc is allocated, and the
   memory copied to it.  Not very efficient.  We could traverse the
   bloc_list for a best fit of free blocs first.

   Change *PTR to reflect the new bloc, and return this value.

   If more memory cannot be allocated, then leave *PTR unchanged, and
   return zero.  */

POINTER
r_re_alloc (POINTER *ptr, SIZE size)
{
  register bloc_ptr bloc;

  if (! r_alloc_initialized)
    r_alloc_init ();

  if (!*ptr)
    return r_alloc (ptr, size);
  if (!size)
    {
      r_alloc_free (ptr);
      return r_alloc (ptr, 0);
    }

  bloc = find_bloc (ptr);
  if (bloc == NIL_BLOC)
    abort (); /* Already freed? PTR not originally used to allocate?  */

  if (size < bloc->size)
    {
      /* Wouldn't it be useful to actually resize the bloc here?  */
      /* I think so too, but not if it's too expensive...  */
      if ((bloc->size - MEM_ROUNDUP (size) >= page_size)
          && r_alloc_freeze_level == 0)
	{
	  resize_bloc (bloc, MEM_ROUNDUP (size));
	  /* Never mind if this fails, just do nothing...  */
	  /* It *should* be infallible!  */
	}
    }
  else if (size > bloc->size)
    {
      if (r_alloc_freeze_level)
	{
	  bloc_ptr new_bloc;
	  new_bloc = get_bloc (MEM_ROUNDUP (size));
	  if (new_bloc)
	    {
	      new_bloc->variable = ptr;
	      *ptr = new_bloc->data;
	      bloc->variable = (POINTER *) NIL;
	    }
          else
	    return NIL;
	}
      else
	{
	  if (! resize_bloc (bloc, MEM_ROUNDUP (size)))
	    return NIL;
        }
    }
  return *ptr;
}

/* Disable relocations, after making room for at least SIZE bytes
   of non-relocatable heap if possible.  The relocatable blocs are
   guaranteed to hold still until thawed, even if this means that
   malloc must return a null pointer.  */

void
r_alloc_freeze (long int size)
{
  if (! r_alloc_initialized)
    r_alloc_init ();

  /* If already frozen, we can't make any more room, so don't try.  */
  if (r_alloc_freeze_level > 0)
    size = 0;
  /* If we can't get the amount requested, half is better than nothing.  */
  while (size > 0 && r_alloc_sbrk (size) == 0)
    size /= 2;
  ++r_alloc_freeze_level;
  if (size > 0)
    r_alloc_sbrk (-size);
}

void
r_alloc_thaw (void)
{

  if (! r_alloc_initialized)
    r_alloc_init ();

  if (--r_alloc_freeze_level < 0)
    abort ();

  /* This frees all unused blocs.  It is not too inefficient, as the resize
     and memcpy is done only once.  Afterwards, all unreferenced blocs are
     already shrunk to zero size.  */
  if (!r_alloc_freeze_level)
    {
      bloc_ptr *b = &first_bloc;
      while (*b)
	if (!(*b)->variable)
	  free_bloc (*b);
	else
	  b = &(*b)->next;
    }
}


#if defined (emacs) && defined (DOUG_LEA_MALLOC)

/* Reinitialize the morecore hook variables after restarting a dumped
   Emacs.  This is needed when using Doug Lea's malloc from GNU libc.  */
void
r_alloc_reinit (void)
{
  /* Only do this if the hook has been reset, so that we don't get an
     infinite loop, in case Emacs was linked statically.  */
  if (__morecore != r_alloc_sbrk)
    {
      real_morecore = __morecore;
      __morecore = r_alloc_sbrk;
    }
}

#endif /* emacs && DOUG_LEA_MALLOC */

#ifdef DEBUG

#include <assert.h>

void
r_alloc_check (void)
{
  int found = 0;
  heap_ptr h, ph = 0;
  bloc_ptr b, pb = 0;

  if (!r_alloc_initialized)
    return;

  assert (first_heap);
  assert (last_heap->end <= (POINTER) sbrk (0));
  assert ((POINTER) first_heap < first_heap->start);
  assert (first_heap->start <= virtual_break_value);
  assert (virtual_break_value <= first_heap->end);

  for (h = first_heap; h; h = h->next)
    {
      assert (h->prev == ph);
      assert ((POINTER) ROUNDUP (h->end) == h->end);
#if 0 /* ??? The code in ralloc.c does not really try to ensure
	 the heap start has any sort of alignment.
	 Perhaps it should.  */
      assert ((POINTER) MEM_ROUNDUP (h->start) == h->start);
#endif
      assert ((POINTER) MEM_ROUNDUP (h->bloc_start) == h->bloc_start);
      assert (h->start <= h->bloc_start && h->bloc_start <= h->end);

      if (ph)
	{
	  assert (ph->end < h->start);
	  assert (h->start <= (POINTER)h && (POINTER)(h+1) <= h->bloc_start);
	}

      if (h->bloc_start <= break_value && break_value <= h->end)
	found = 1;

      ph = h;
    }

  assert (found);
  assert (last_heap == ph);

  for (b = first_bloc; b; b = b->next)
    {
      assert (b->prev == pb);
      assert ((POINTER) MEM_ROUNDUP (b->data) == b->data);
      assert ((SIZE) MEM_ROUNDUP (b->size) == b->size);

      ph = 0;
      for (h = first_heap; h; h = h->next)
	{
	  if (h->bloc_start <= b->data && b->data + b->size <= h->end)
	    break;
	  ph = h;
	}

      assert (h);

      if (pb && pb->data + pb->size != b->data)
	{
	  assert (ph && b->data == h->bloc_start);
	  while (ph)
	    {
	      if (ph->bloc_start <= pb->data
		  && pb->data + pb->size <= ph->end)
		{
		  assert (pb->data + pb->size + b->size > ph->end);
		  break;
		}
	      else
		{
		  assert (ph->bloc_start + b->size > ph->end);
		}
	      ph = ph->prev;
	    }
	}
      pb = b;
    }

  assert (last_bloc == pb);

  if (last_bloc)
    assert (last_bloc->data + last_bloc->size == break_value);
  else
    assert (first_heap->bloc_start == break_value);
}

#endif /* DEBUG */

/* Update the internal record of which variable points to some data to NEW.
   Used by buffer-swap-text in Emacs to restore consistency after it
   swaps the buffer text between two buffer objects.  The OLD pointer
   is checked to ensure that memory corruption does not occur due to
   misuse.  */
void
r_alloc_reset_variable (POINTER *old, POINTER *new)
{
  bloc_ptr bloc = first_bloc;

  /* Find the bloc that corresponds to the data pointed to by pointer.
     find_bloc cannot be used, as it has internal consistency checks
     which fail when the variable needs resetting.  */
  while (bloc != NIL_BLOC)
    {
      if (bloc->data == *new)
	break;

      bloc = bloc->next;
    }

  if (bloc == NIL_BLOC || bloc->variable != old)
    abort (); /* Already freed? OLD not originally used to allocate?  */

  /* Update variable to point to the new location.  */
  bloc->variable = new;
}

void
r_alloc_inhibit_buffer_relocation (int inhibit)
{
  if (use_relocatable_buffers < 0)
    use_relocatable_buffers = 0;
  if (inhibit)
    use_relocatable_buffers++;
  else if (use_relocatable_buffers > 0)
    use_relocatable_buffers--;
}


/***********************************************************************
			    Initialization
 ***********************************************************************/

/* Initialize various things for memory allocation.  */

static void
r_alloc_init (void)
{
  if (r_alloc_initialized)
    return;
  r_alloc_initialized = 1;

  page_size = PAGE;
#ifndef SYSTEM_MALLOC
  real_morecore = __morecore;
  __morecore = r_alloc_sbrk;

  first_heap = last_heap = &heap_base;
  first_heap->next = first_heap->prev = NIL_HEAP;
  first_heap->start = first_heap->bloc_start
    = virtual_break_value = break_value = (*real_morecore) (0);
  if (break_value == NIL)
    abort ();

  extra_bytes = ROUNDUP (50000);
#endif

#ifdef DOUG_LEA_MALLOC
  BLOCK_INPUT;
  mallopt (M_TOP_PAD, 64 * 4096);
  UNBLOCK_INPUT;
#else
#ifndef SYSTEM_MALLOC
  /* Give GNU malloc's morecore some hysteresis
     so that we move all the relocatable blocks much less often.  */
  __malloc_extra_blocks = 64;
#endif
#endif

#ifndef SYSTEM_MALLOC
  first_heap->end = (POINTER) ROUNDUP (first_heap->start);

  /* The extra call to real_morecore guarantees that the end of the
     address space is a multiple of page_size, even if page_size is
     not really the page size of the system running the binary in
     which page_size is stored.  This allows a binary to be built on a
     system with one page size and run on a system with a smaller page
     size.  */
  (*real_morecore) ((char *) first_heap->end - (char *) first_heap->start);

  /* Clear the rest of the last page; this memory is in our address space
     even though it is after the sbrk value.  */
  /* Doubly true, with the additional call that explicitly adds the
     rest of that page to the address space.  */
  memset (first_heap->start, 0,
	  (char *) first_heap->end - (char *) first_heap->start);
  virtual_break_value = break_value = first_heap->bloc_start = first_heap->end;
#endif

  use_relocatable_buffers = 1;
}
