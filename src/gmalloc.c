/* This file is no longer automatically generated from libc.  */

#define _MALLOC_INTERNAL

/* The malloc headers and source files from the C library follow here.  */

/* Declarations for `malloc' and friends.
   Copyright (C) 1990, 1991, 1992, 1993, 1995, 1996, 1999, 2002, 2003, 2004,
                 2005, 2006, 2007 Free Software Foundation, Inc.
		  Written May 1989 by Mike Haertel.

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public
License along with this library; see the file COPYING.  If
not, write to the Free Software Foundation, Inc., 51 Franklin Street,
Fifth Floor, Boston, MA 02110-1301, USA.

   The author may be reached (Email) at the address mike@ai.mit.edu,
   or (US mail) as Mike Haertel c/o Free Software Foundation.  */

#ifndef _MALLOC_H

#define _MALLOC_H	1

#ifdef _MALLOC_INTERNAL

#ifdef	HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef HAVE_PTHREAD
#define USE_PTHREAD
#endif

#undef	PP
#define	PP(args)	args
#undef	__ptr_t
#define	__ptr_t		void *

#include <string.h>
#include <limits.h>
#include <unistd.h>

#ifdef USE_PTHREAD
#include <pthread.h>
#endif

#endif	/* _MALLOC_INTERNAL.  */


#ifdef	__cplusplus
extern "C"
{
#endif

#include <stddef.h>
#define	__malloc_size_t		size_t
#define	__malloc_ptrdiff_t	ptrdiff_t


/* Allocate SIZE bytes of memory.  */
extern __ptr_t malloc PP ((__malloc_size_t __size));
/* Re-allocate the previously allocated block
   in __ptr_t, making the new block SIZE bytes long.  */
extern __ptr_t realloc PP ((__ptr_t __ptr, __malloc_size_t __size));
/* Allocate NMEMB elements of SIZE bytes each, all initialized to 0.  */
extern __ptr_t calloc PP ((__malloc_size_t __nmemb, __malloc_size_t __size));
/* Free a block allocated by `malloc', `realloc' or `calloc'.  */
extern void free PP ((__ptr_t __ptr));

/* Allocate SIZE bytes allocated to ALIGNMENT bytes.  */
#if !defined (_MALLOC_INTERNAL) || defined (MSDOS) /* Avoid conflict.  */
extern __ptr_t memalign PP ((__malloc_size_t __alignment,
			     __malloc_size_t __size));
extern int posix_memalign PP ((__ptr_t *, __malloc_size_t,
			       __malloc_size_t size));
#endif

/* Allocate SIZE bytes on a page boundary.  */
#if ! (defined (_MALLOC_INTERNAL) && defined (GMALLOC_INHIBIT_VALLOC))
extern __ptr_t valloc PP ((__malloc_size_t __size));
#endif

#ifdef USE_PTHREAD
/* Set up mutexes and make malloc etc. thread-safe.  */
extern void malloc_enable_thread PP ((void));
#endif

#ifdef _MALLOC_INTERNAL

/* The allocator divides the heap into blocks of fixed size; large
   requests receive one or more whole blocks, and small requests
   receive a fragment of a block.  Fragment sizes are powers of two,
   and all fragments of a block are the same size.  When all the
   fragments in a block have been freed, the block itself is freed.  */
#define INT_BIT		(CHAR_BIT * sizeof (int))
#define BLOCKLOG	(INT_BIT > 16 ? 12 : 9)
#define BLOCKSIZE	(1 << BLOCKLOG)
#define BLOCKIFY(SIZE)	(((SIZE) + BLOCKSIZE - 1) / BLOCKSIZE)

/* Determine the amount of memory spanned by the initial heap table
   (not an absolute limit).  */
#define HEAP		(INT_BIT > 16 ? 4194304 : 65536)

/* Number of contiguous free blocks allowed to build up at the end of
   memory before they will be returned to the system.  */
#define FINAL_FREE_BLOCKS	8

/* Data structure giving per-block information.  */
typedef union
  {
    /* Heap information for a busy block.  */
    struct
      {
	/* Zero for a large (multiblock) object, or positive giving the
	   logarithm to the base two of the fragment size.  */
	int type;
	union
	  {
	    struct
	      {
		__malloc_size_t nfree; /* Free frags in a fragmented block.  */
		__malloc_size_t first; /* First free fragment of the block.  */
	      } frag;
	    /* For a large object, in its first block, this has the number
	       of blocks in the object.  In the other blocks, this has a
	       negative number which says how far back the first block is.  */
	    __malloc_ptrdiff_t size;
	  } info;
      } busy;
    /* Heap information for a free block
       (that may be the first of a free cluster).  */
    struct
      {
	__malloc_size_t size;	/* Size (in blocks) of a free cluster.  */
	__malloc_size_t next;	/* Index of next free cluster.  */
	__malloc_size_t prev;	/* Index of previous free cluster.  */
      } free;
  } malloc_info;

/* Pointer to first block of the heap.  */
extern char *_heapbase;

/* Table indexed by block number giving per-block information.  */
extern malloc_info *_heapinfo;

/* Address to block number and vice versa.  */
#define BLOCK(A)	(((char *) (A) - _heapbase) / BLOCKSIZE + 1)
#define ADDRESS(B)	((__ptr_t) (((B) - 1) * BLOCKSIZE + _heapbase))

/* Current search index for the heap table.  */
extern __malloc_size_t _heapindex;

/* Limit of valid info table indices.  */
extern __malloc_size_t _heaplimit;

/* Doubly linked lists of free fragments.  */
struct list
  {
    struct list *next;
    struct list *prev;
  };

/* Free list headers for each fragment size.  */
extern struct list _fraghead[];

/* List of blocks allocated with `memalign' (or `valloc').  */
struct alignlist
  {
    struct alignlist *next;
    __ptr_t aligned;		/* The address that memaligned returned.  */
    __ptr_t exact;		/* The address that malloc returned.  */
  };
extern struct alignlist *_aligned_blocks;

/* Instrumentation.  */
extern __malloc_size_t _chunks_used;
extern __malloc_size_t _bytes_used;
extern __malloc_size_t _chunks_free;
extern __malloc_size_t _bytes_free;

/* Internal versions of `malloc', `realloc', and `free'
   used when these functions need to call each other.
   They are the same but don't call the hooks.  */
extern __ptr_t _malloc_internal PP ((__malloc_size_t __size));
extern __ptr_t _realloc_internal PP ((__ptr_t __ptr, __malloc_size_t __size));
extern void _free_internal PP ((__ptr_t __ptr));
extern __ptr_t _malloc_internal_nolock PP ((__malloc_size_t __size));
extern __ptr_t _realloc_internal_nolock PP ((__ptr_t __ptr, __malloc_size_t __size));
extern void _free_internal_nolock PP ((__ptr_t __ptr));

#ifdef USE_PTHREAD
extern pthread_mutex_t _malloc_mutex, _aligned_blocks_mutex;
extern int _malloc_thread_enabled_p;
#define LOCK()					\
  do {						\
    if (_malloc_thread_enabled_p)		\
      pthread_mutex_lock (&_malloc_mutex);	\
  } while (0)
#define UNLOCK()				\
  do {						\
    if (_malloc_thread_enabled_p)		\
      pthread_mutex_unlock (&_malloc_mutex);	\
  } while (0)
#define LOCK_ALIGNED_BLOCKS()				\
  do {							\
    if (_malloc_thread_enabled_p)			\
      pthread_mutex_lock (&_aligned_blocks_mutex);	\
  } while (0)
#define UNLOCK_ALIGNED_BLOCKS()				\
  do {							\
    if (_malloc_thread_enabled_p)			\
      pthread_mutex_unlock (&_aligned_blocks_mutex);	\
  } while (0)
#else
#define LOCK()
#define UNLOCK()
#define LOCK_ALIGNED_BLOCKS()
#define UNLOCK_ALIGNED_BLOCKS()
#endif

#endif /* _MALLOC_INTERNAL.  */

/* Given an address in the middle of a malloc'd object,
   return the address of the beginning of the object.  */
extern __ptr_t malloc_find_object_address PP ((__ptr_t __ptr));

/* Underlying allocation function; successive calls should
   return contiguous pieces of memory.  */
extern __ptr_t (*__morecore) PP ((__malloc_ptrdiff_t __size));

/* Default value of `__morecore'.  */
extern __ptr_t __default_morecore PP ((__malloc_ptrdiff_t __size));

/* If not NULL, this function is called after each time
   `__morecore' is called to increase the data size.  */
extern void (*__after_morecore_hook) PP ((void));

/* Number of extra blocks to get each time we ask for more core.
   This reduces the frequency of calling `(*__morecore)'.  */
extern __malloc_size_t __malloc_extra_blocks;

/* Nonzero if `malloc' has been called and done its initialization.  */
extern int __malloc_initialized;
/* Function called to initialize malloc data structures.  */
extern int __malloc_initialize PP ((void));

/* Hooks for debugging versions.  */
extern void (*__malloc_initialize_hook) PP ((void));
extern void (*__free_hook) PP ((__ptr_t __ptr));
extern __ptr_t (*__malloc_hook) PP ((__malloc_size_t __size));
extern __ptr_t (*__realloc_hook) PP ((__ptr_t __ptr, __malloc_size_t __size));
extern __ptr_t (*__memalign_hook) PP ((__malloc_size_t __size,
				       __malloc_size_t __alignment));

/* Return values for `mprobe': these are the kinds of inconsistencies that
   `mcheck' enables detection of.  */
enum mcheck_status
  {
    MCHECK_DISABLED = -1,	/* Consistency checking is not turned on.  */
    MCHECK_OK,			/* Block is fine.  */
    MCHECK_FREE,		/* Block freed twice.  */
    MCHECK_HEAD,		/* Memory before the block was clobbered.  */
    MCHECK_TAIL			/* Memory after the block was clobbered.  */
  };

/* Activate a standard collection of debugging hooks.  This must be called
   before `malloc' is ever called.  ABORTFUNC is called with an error code
   (see enum above) when an inconsistency is detected.  If ABORTFUNC is
   null, the standard function prints on stderr and then calls `abort'.  */
extern int mcheck PP ((void (*__abortfunc) PP ((enum mcheck_status))));

/* Check for aberrations in a particular malloc'd block.  You must have
   called `mcheck' already.  These are the same checks that `mcheck' does
   when you free or reallocate a block.  */
extern enum mcheck_status mprobe PP ((__ptr_t __ptr));

/* Activate a standard collection of tracing hooks.  */
extern void mtrace PP ((void));
extern void muntrace PP ((void));

/* Statistics available to the user.  */
struct mstats
  {
    __malloc_size_t bytes_total; /* Total size of the heap. */
    __malloc_size_t chunks_used; /* Chunks allocated by the user. */
    __malloc_size_t bytes_used;	/* Byte total of user-allocated chunks. */
    __malloc_size_t chunks_free; /* Chunks in the free list. */
    __malloc_size_t bytes_free;	/* Byte total of chunks in the free list. */
  };

/* Pick up the current statistics. */
extern struct mstats mstats PP ((void));

/* Call WARNFUN with a warning message when memory usage is high.  */
extern void memory_warnings PP ((__ptr_t __start,
				 void (*__warnfun) PP ((const char *))));


/* Relocating allocator.  */

/* Allocate SIZE bytes, and store the address in *HANDLEPTR.  */
extern __ptr_t r_alloc PP ((__ptr_t *__handleptr, __malloc_size_t __size));

/* Free the storage allocated in HANDLEPTR.  */
extern void r_alloc_free PP ((__ptr_t *__handleptr));

/* Adjust the block at HANDLEPTR to be SIZE bytes long.  */
extern __ptr_t r_re_alloc PP ((__ptr_t *__handleptr, __malloc_size_t __size));


#ifdef	__cplusplus
}
#endif

#endif /* malloc.h  */
/* Memory allocator `malloc'.
   Copyright 1990, 1991, 1992, 1993, 1994, 1995 Free Software Foundation, Inc.
		  Written May 1989 by Mike Haertel.

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public
License along with this library; see the file COPYING.  If
not, write to the Free Software Foundation, Inc., 51 Franklin Street,
Fifth Floor, Boston, MA 02110-1301, USA.

   The author may be reached (Email) at the address mike@ai.mit.edu,
   or (US mail) as Mike Haertel c/o Free Software Foundation.  */

#ifndef	_MALLOC_INTERNAL
#define _MALLOC_INTERNAL
#include <malloc.h>
#endif
#include <errno.h>

/* On Cygwin there are two heaps.  temacs uses the static heap
   (defined in sheap.c and managed with bss_sbrk), and the dumped
   emacs uses the Cygwin heap (managed with sbrk).  When emacs starts
   on Cygwin, it reinitializes malloc, and we save the old info for
   use by free and realloc if they're called with a pointer into the
   static heap.

   Currently (2011-08-16) the Cygwin build doesn't use ralloc.c; if
   this is changed in the future, we'll have to similarly deal with
   reinitializing ralloc. */
#ifdef CYGWIN
extern __ptr_t bss_sbrk PP ((ptrdiff_t __size));
extern int bss_sbrk_did_unexec;
char *bss_sbrk_heapbase;	/* _heapbase for static heap */
malloc_info *bss_sbrk_heapinfo;	/* _heapinfo for static heap */
#endif
__ptr_t (*__morecore) PP ((__malloc_ptrdiff_t __size)) = __default_morecore;

/* Debugging hook for `malloc'.  */
__ptr_t (*__malloc_hook) PP ((__malloc_size_t __size));

/* Pointer to the base of the first block.  */
char *_heapbase;

/* Block information table.  Allocated with align/__free (not malloc/free).  */
malloc_info *_heapinfo;

/* Number of info entries.  */
static __malloc_size_t heapsize;

/* Search index in the info table.  */
__malloc_size_t _heapindex;

/* Limit of valid info table indices.  */
__malloc_size_t _heaplimit;

/* Free lists for each fragment size.  */
struct list _fraghead[BLOCKLOG];

/* Instrumentation.  */
__malloc_size_t _chunks_used;
__malloc_size_t _bytes_used;
__malloc_size_t _chunks_free;
__malloc_size_t _bytes_free;

/* Are you experienced?  */
int __malloc_initialized;

__malloc_size_t __malloc_extra_blocks;

void (*__malloc_initialize_hook) PP ((void));
void (*__after_morecore_hook) PP ((void));

#if defined GC_MALLOC_CHECK && defined GC_PROTECT_MALLOC_STATE

/* Some code for hunting a bug writing into _heapinfo.

   Call this macro with argument PROT non-zero to protect internal
   malloc state against writing to it, call it with a zero argument to
   make it readable and writable.

   Note that this only works if BLOCKSIZE == page size, which is
   the case on the i386.  */

#include <sys/types.h>
#include <sys/mman.h>

static int state_protected_p;
static __malloc_size_t last_state_size;
static malloc_info *last_heapinfo;

void
protect_malloc_state (protect_p)
     int protect_p;
{
  /* If _heapinfo has been relocated, make sure its old location
     isn't left read-only; it will be reused by malloc.  */
  if (_heapinfo != last_heapinfo
      && last_heapinfo
      && state_protected_p)
    mprotect (last_heapinfo, last_state_size, PROT_READ | PROT_WRITE);

  last_state_size = _heaplimit * sizeof *_heapinfo;
  last_heapinfo   = _heapinfo;

  if (protect_p != state_protected_p)
    {
      state_protected_p = protect_p;
      if (mprotect (_heapinfo, last_state_size,
		    protect_p ? PROT_READ : PROT_READ | PROT_WRITE) != 0)
	abort ();
    }
}

#define PROTECT_MALLOC_STATE(PROT) protect_malloc_state (PROT)

#else
#define PROTECT_MALLOC_STATE(PROT)	/* empty */
#endif


/* Aligned allocation.  */
static __ptr_t align PP ((__malloc_size_t));
static __ptr_t
align (size)
     __malloc_size_t size;
{
  __ptr_t result;
  unsigned long int adj;

  /* align accepts an unsigned argument, but __morecore accepts a
     signed one.  This could lead to trouble if SIZE overflows a
     signed int type accepted by __morecore.  We just punt in that
     case, since they are requesting a ludicrous amount anyway.  */
  if ((__malloc_ptrdiff_t)size < 0)
    result = 0;
  else
    result = (*__morecore) (size);
  adj = (unsigned long int) ((unsigned long int) ((char *) result -
						  (char *) NULL)) % BLOCKSIZE;
  if (adj != 0)
    {
      __ptr_t new;
      adj = BLOCKSIZE - adj;
      new = (*__morecore) (adj);
      result = (char *) result + adj;
    }

  if (__after_morecore_hook)
    (*__after_morecore_hook) ();

  return result;
}

/* Get SIZE bytes, if we can get them starting at END.
   Return the address of the space we got.
   If we cannot get space at END, fail and return 0.  */
static __ptr_t get_contiguous_space PP ((__malloc_ptrdiff_t, __ptr_t));
static __ptr_t
get_contiguous_space (size, position)
     __malloc_ptrdiff_t size;
     __ptr_t position;
{
  __ptr_t before;
  __ptr_t after;

  before = (*__morecore) (0);
  /* If we can tell in advance that the break is at the wrong place,
     fail now.  */
  if (before != position)
    return 0;

  /* Allocate SIZE bytes and get the address of them.  */
  after = (*__morecore) (size);
  if (!after)
    return 0;

  /* It was not contiguous--reject it.  */
  if (after != position)
    {
      (*__morecore) (- size);
      return 0;
    }

  return after;
}


/* This is called when `_heapinfo' and `heapsize' have just
   been set to describe a new info table.  Set up the table
   to describe itself and account for it in the statistics.  */
static inline void
register_heapinfo (void)
{
  __malloc_size_t block, blocks;

  block = BLOCK (_heapinfo);
  blocks = BLOCKIFY (heapsize * sizeof (malloc_info));

  /* Account for the _heapinfo block itself in the statistics.  */
  _bytes_used += blocks * BLOCKSIZE;
  ++_chunks_used;

  /* Describe the heapinfo block itself in the heapinfo.  */
  _heapinfo[block].busy.type = 0;
  _heapinfo[block].busy.info.size = blocks;
  /* Leave back-pointers for malloc_find_address.  */
  while (--blocks > 0)
    _heapinfo[block + blocks].busy.info.size = -blocks;
}

#ifdef USE_PTHREAD
pthread_mutex_t _malloc_mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t _aligned_blocks_mutex = PTHREAD_MUTEX_INITIALIZER;
int _malloc_thread_enabled_p;

static void
malloc_atfork_handler_prepare ()
{
  LOCK ();
  LOCK_ALIGNED_BLOCKS ();
}

static void
malloc_atfork_handler_parent ()
{
  UNLOCK_ALIGNED_BLOCKS ();
  UNLOCK ();
}

static void
malloc_atfork_handler_child ()
{
  UNLOCK_ALIGNED_BLOCKS ();
  UNLOCK ();
}

/* Set up mutexes and make malloc etc. thread-safe.  */
void
malloc_enable_thread ()
{
  if (_malloc_thread_enabled_p)
    return;

  /* Some pthread implementations call malloc for statically
     initialized mutexes when they are used first.  To avoid such a
     situation, we initialize mutexes here while their use is
     disabled in malloc etc.  */
  pthread_mutex_init (&_malloc_mutex, NULL);
  pthread_mutex_init (&_aligned_blocks_mutex, NULL);
  pthread_atfork (malloc_atfork_handler_prepare,
		  malloc_atfork_handler_parent,
		  malloc_atfork_handler_child);
  _malloc_thread_enabled_p = 1;
}
#endif

static void
malloc_initialize_1 ()
{
#ifdef GC_MCHECK
  mcheck (NULL);
#endif

#ifdef CYGWIN
  if (bss_sbrk_did_unexec)
    /* we're reinitializing the dumped emacs */
    {
      bss_sbrk_heapbase = _heapbase;
      bss_sbrk_heapinfo = _heapinfo;
      memset (_fraghead, 0, BLOCKLOG * sizeof (struct list));
    }
#endif

  if (__malloc_initialize_hook)
    (*__malloc_initialize_hook) ();

  heapsize = HEAP / BLOCKSIZE;
  _heapinfo = (malloc_info *) align (heapsize * sizeof (malloc_info));
  if (_heapinfo == NULL)
    return;
  memset (_heapinfo, 0, heapsize * sizeof (malloc_info));
  _heapinfo[0].free.size = 0;
  _heapinfo[0].free.next = _heapinfo[0].free.prev = 0;
  _heapindex = 0;
  _heapbase = (char *) _heapinfo;
  _heaplimit = BLOCK (_heapbase + heapsize * sizeof (malloc_info));

  register_heapinfo ();

  __malloc_initialized = 1;
  PROTECT_MALLOC_STATE (1);
  return;
}

/* Set everything up and remember that we have.
   main will call malloc which calls this function.  That is before any threads
   or signal handlers has been set up, so we don't need thread protection.  */
int
__malloc_initialize ()
{
  if (__malloc_initialized)
    return 0;

  malloc_initialize_1 ();

  return __malloc_initialized;
}

static int morecore_recursing;

/* Get neatly aligned memory, initializing or
   growing the heap info table as necessary. */
static __ptr_t morecore_nolock PP ((__malloc_size_t));
static __ptr_t
morecore_nolock (size)
     __malloc_size_t size;
{
  __ptr_t result;
  malloc_info *newinfo, *oldinfo;
  __malloc_size_t newsize;

  if (morecore_recursing)
    /* Avoid recursion.  The caller will know how to handle a null return.  */
    return NULL;

  result = align (size);
  if (result == NULL)
    return NULL;

  PROTECT_MALLOC_STATE (0);

  /* Check if we need to grow the info table.  */
  if ((__malloc_size_t) BLOCK ((char *) result + size) > heapsize)
    {
      /* Calculate the new _heapinfo table size.  We do not account for the
	 added blocks in the table itself, as we hope to place them in
	 existing free space, which is already covered by part of the
	 existing table.  */
      newsize = heapsize;
      do
	newsize *= 2;
      while ((__malloc_size_t) BLOCK ((char *) result + size) > newsize);

      /* We must not reuse existing core for the new info table when called
	 from realloc in the case of growing a large block, because the
	 block being grown is momentarily marked as free.  In this case
	 _heaplimit is zero so we know not to reuse space for internal
	 allocation.  */
      if (_heaplimit != 0)
	{
	  /* First try to allocate the new info table in core we already
	     have, in the usual way using realloc.  If realloc cannot
	     extend it in place or relocate it to existing sufficient core,
	     we will get called again, and the code above will notice the
	     `morecore_recursing' flag and return null.  */
	  int save = errno;	/* Don't want to clobber errno with ENOMEM.  */
	  morecore_recursing = 1;
	  newinfo = (malloc_info *) _realloc_internal_nolock
	    (_heapinfo, newsize * sizeof (malloc_info));
	  morecore_recursing = 0;
	  if (newinfo == NULL)
	    errno = save;
	  else
	    {
	      /* We found some space in core, and realloc has put the old
		 table's blocks on the free list.  Now zero the new part
		 of the table and install the new table location.  */
	      memset (&newinfo[heapsize], 0,
		      (newsize - heapsize) * sizeof (malloc_info));
	      _heapinfo = newinfo;
	      heapsize = newsize;
	      goto got_heap;
	    }
	}

      /* Allocate new space for the malloc info table.  */
      while (1)
  	{
 	  newinfo = (malloc_info *) align (newsize * sizeof (malloc_info));

 	  /* Did it fail?  */
 	  if (newinfo == NULL)
 	    {
 	      (*__morecore) (-size);
 	      return NULL;
 	    }

 	  /* Is it big enough to record status for its own space?
 	     If so, we win.  */
 	  if ((__malloc_size_t) BLOCK ((char *) newinfo
 				       + newsize * sizeof (malloc_info))
 	      < newsize)
 	    break;

 	  /* Must try again.  First give back most of what we just got.  */
 	  (*__morecore) (- newsize * sizeof (malloc_info));
 	  newsize *= 2;
  	}

      /* Copy the old table to the beginning of the new,
	 and zero the rest of the new table.  */
      memcpy (newinfo, _heapinfo, heapsize * sizeof (malloc_info));
      memset (&newinfo[heapsize], 0,
	      (newsize - heapsize) * sizeof (malloc_info));
      oldinfo = _heapinfo;
      _heapinfo = newinfo;
      heapsize = newsize;

      register_heapinfo ();

      /* Reset _heaplimit so _free_internal never decides
	 it can relocate or resize the info table.  */
      _heaplimit = 0;
      _free_internal_nolock (oldinfo);
      PROTECT_MALLOC_STATE (0);

      /* The new heap limit includes the new table just allocated.  */
      _heaplimit = BLOCK ((char *) newinfo + heapsize * sizeof (malloc_info));
      return result;
    }

 got_heap:
  _heaplimit = BLOCK ((char *) result + size);
  return result;
}

/* Allocate memory from the heap.  */
__ptr_t
_malloc_internal_nolock (size)
     __malloc_size_t size;
{
  __ptr_t result;
  __malloc_size_t block, blocks, lastblocks, start;
  register __malloc_size_t i;
  struct list *next;

  /* ANSI C allows `malloc (0)' to either return NULL, or to return a
     valid address you can realloc and free (though not dereference).

     It turns out that some extant code (sunrpc, at least Ultrix's version)
     expects `malloc (0)' to return non-NULL and breaks otherwise.
     Be compatible.  */

#if	0
  if (size == 0)
    return NULL;
#endif

  PROTECT_MALLOC_STATE (0);

  if (size < sizeof (struct list))
    size = sizeof (struct list);

  /* Determine the allocation policy based on the request size.  */
  if (size <= BLOCKSIZE / 2)
    {
      /* Small allocation to receive a fragment of a block.
	 Determine the logarithm to base two of the fragment size. */
      register __malloc_size_t log = 1;
      --size;
      while ((size /= 2) != 0)
	++log;

      /* Look in the fragment lists for a
	 free fragment of the desired size. */
      next = _fraghead[log].next;
      if (next != NULL)
	{
	  /* There are free fragments of this size.
	     Pop a fragment out of the fragment list and return it.
	     Update the block's nfree and first counters. */
	  result = (__ptr_t) next;
	  next->prev->next = next->next;
	  if (next->next != NULL)
	    next->next->prev = next->prev;
	  block = BLOCK (result);
	  if (--_heapinfo[block].busy.info.frag.nfree != 0)
	    _heapinfo[block].busy.info.frag.first = (unsigned long int)
	      ((unsigned long int) ((char *) next->next - (char *) NULL)
	       % BLOCKSIZE) >> log;

	  /* Update the statistics.  */
	  ++_chunks_used;
	  _bytes_used += 1 << log;
	  --_chunks_free;
	  _bytes_free -= 1 << log;
	}
      else
	{
	  /* No free fragments of the desired size, so get a new block
	     and break it into fragments, returning the first.  */
#ifdef GC_MALLOC_CHECK
	  result = _malloc_internal_nolock (BLOCKSIZE);
	  PROTECT_MALLOC_STATE (0);
#elif defined (USE_PTHREAD)
	  result = _malloc_internal_nolock (BLOCKSIZE);
#else
	  result = malloc (BLOCKSIZE);
#endif
	  if (result == NULL)
	    {
	      PROTECT_MALLOC_STATE (1);
	      goto out;
	    }

	  /* Link all fragments but the first into the free list.  */
	  next = (struct list *) ((char *) result + (1 << log));
	  next->next = NULL;
	  next->prev = &_fraghead[log];
	  _fraghead[log].next = next;

	  for (i = 2; i < (__malloc_size_t) (BLOCKSIZE >> log); ++i)
	    {
	      next = (struct list *) ((char *) result + (i << log));
	      next->next = _fraghead[log].next;
	      next->prev = &_fraghead[log];
	      next->prev->next = next;
	      next->next->prev = next;
	    }

	  /* Initialize the nfree and first counters for this block.  */
	  block = BLOCK (result);
	  _heapinfo[block].busy.type = log;
	  _heapinfo[block].busy.info.frag.nfree = i - 1;
	  _heapinfo[block].busy.info.frag.first = i - 1;

	  _chunks_free += (BLOCKSIZE >> log) - 1;
	  _bytes_free += BLOCKSIZE - (1 << log);
	  _bytes_used -= BLOCKSIZE - (1 << log);
	}
    }
  else
    {
      /* Large allocation to receive one or more blocks.
	 Search the free list in a circle starting at the last place visited.
	 If we loop completely around without finding a large enough
	 space we will have to get more memory from the system.  */
      blocks = BLOCKIFY (size);
      start = block = _heapindex;
      while (_heapinfo[block].free.size < blocks)
	{
	  block = _heapinfo[block].free.next;
	  if (block == start)
	    {
	      /* Need to get more from the system.  Get a little extra.  */
	      __malloc_size_t wantblocks = blocks + __malloc_extra_blocks;
	      block = _heapinfo[0].free.prev;
	      lastblocks = _heapinfo[block].free.size;
	      /* Check to see if the new core will be contiguous with the
		 final free block; if so we don't need to get as much.  */
	      if (_heaplimit != 0 && block + lastblocks == _heaplimit &&
		  /* We can't do this if we will have to make the heap info
                     table bigger to accommodate the new space.  */
		  block + wantblocks <= heapsize &&
		  get_contiguous_space ((wantblocks - lastblocks) * BLOCKSIZE,
					ADDRESS (block + lastblocks)))
		{
 		  /* We got it contiguously.  Which block we are extending
		     (the `final free block' referred to above) might have
		     changed, if it got combined with a freed info table.  */
 		  block = _heapinfo[0].free.prev;
  		  _heapinfo[block].free.size += (wantblocks - lastblocks);
		  _bytes_free += (wantblocks - lastblocks) * BLOCKSIZE;
 		  _heaplimit += wantblocks - lastblocks;
		  continue;
		}
	      result = morecore_nolock (wantblocks * BLOCKSIZE);
	      if (result == NULL)
		goto out;
	      block = BLOCK (result);
	      /* Put the new block at the end of the free list.  */
	      _heapinfo[block].free.size = wantblocks;
	      _heapinfo[block].free.prev = _heapinfo[0].free.prev;
	      _heapinfo[block].free.next = 0;
	      _heapinfo[0].free.prev = block;
	      _heapinfo[_heapinfo[block].free.prev].free.next = block;
	      ++_chunks_free;
	      /* Now loop to use some of that block for this allocation.  */
	    }
	}

      /* At this point we have found a suitable free list entry.
	 Figure out how to remove what we need from the list. */
      result = ADDRESS (block);
      if (_heapinfo[block].free.size > blocks)
	{
	  /* The block we found has a bit left over,
	     so relink the tail end back into the free list. */
	  _heapinfo[block + blocks].free.size
	    = _heapinfo[block].free.size - blocks;
	  _heapinfo[block + blocks].free.next
	    = _heapinfo[block].free.next;
	  _heapinfo[block + blocks].free.prev
	    = _heapinfo[block].free.prev;
	  _heapinfo[_heapinfo[block].free.prev].free.next
	    = _heapinfo[_heapinfo[block].free.next].free.prev
	    = _heapindex = block + blocks;
	}
      else
	{
	  /* The block exactly matches our requirements,
	     so just remove it from the list. */
	  _heapinfo[_heapinfo[block].free.next].free.prev
	    = _heapinfo[block].free.prev;
	  _heapinfo[_heapinfo[block].free.prev].free.next
	    = _heapindex = _heapinfo[block].free.next;
	  --_chunks_free;
	}

      _heapinfo[block].busy.type = 0;
      _heapinfo[block].busy.info.size = blocks;
      ++_chunks_used;
      _bytes_used += blocks * BLOCKSIZE;
      _bytes_free -= blocks * BLOCKSIZE;

      /* Mark all the blocks of the object just allocated except for the
	 first with a negative number so you can find the first block by
	 adding that adjustment.  */
      while (--blocks > 0)
	_heapinfo[block + blocks].busy.info.size = -blocks;
    }

  PROTECT_MALLOC_STATE (1);
 out:
  return result;
}

__ptr_t
_malloc_internal (size)
     __malloc_size_t size;
{
  __ptr_t result;

  LOCK ();
  result = _malloc_internal_nolock (size);
  UNLOCK ();

  return result;
}

__ptr_t
malloc (size)
     __malloc_size_t size;
{
  __ptr_t (*hook) (__malloc_size_t);

  if (!__malloc_initialized && !__malloc_initialize ())
    return NULL;

  /* Copy the value of __malloc_hook to an automatic variable in case
     __malloc_hook is modified in another thread between its
     NULL-check and the use.

     Note: Strictly speaking, this is not a right solution.  We should
     use mutexes to access non-read-only variables that are shared
     among multiple threads.  We just leave it for compatibility with
     glibc malloc (i.e., assignments to __malloc_hook) for now.  */
  hook = __malloc_hook;
  return (hook != NULL ? *hook : _malloc_internal) (size);
}

#ifndef _LIBC

/* On some ANSI C systems, some libc functions call _malloc, _free
   and _realloc.  Make them use the GNU functions.  */

__ptr_t
_malloc (size)
     __malloc_size_t size;
{
  return malloc (size);
}

void
_free (ptr)
     __ptr_t ptr;
{
  free (ptr);
}

__ptr_t
_realloc (ptr, size)
     __ptr_t ptr;
     __malloc_size_t size;
{
  return realloc (ptr, size);
}

#endif
/* Free a block of memory allocated by `malloc'.
   Copyright 1990, 1991, 1992, 1994, 1995 Free Software Foundation, Inc.
		  Written May 1989 by Mike Haertel.

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public
License along with this library; see the file COPYING.  If
not, write to the Free Software Foundation, Inc., 51 Franklin Street,
Fifth Floor, Boston, MA 02110-1301, USA.

   The author may be reached (Email) at the address mike@ai.mit.edu,
   or (US mail) as Mike Haertel c/o Free Software Foundation.  */

#ifndef	_MALLOC_INTERNAL
#define _MALLOC_INTERNAL
#include <malloc.h>
#endif


/* Debugging hook for free.  */
void (*__free_hook) PP ((__ptr_t __ptr));

/* List of blocks allocated by memalign.  */
struct alignlist *_aligned_blocks = NULL;

/* Return memory to the heap.
   Like `_free_internal' but don't lock mutex.  */
void
_free_internal_nolock (ptr)
     __ptr_t ptr;
{
  int type;
  __malloc_size_t block, blocks;
  register __malloc_size_t i;
  struct list *prev, *next;
  __ptr_t curbrk;
  const __malloc_size_t lesscore_threshold
    /* Threshold of free space at which we will return some to the system.  */
    = FINAL_FREE_BLOCKS + 2 * __malloc_extra_blocks;

  register struct alignlist *l;

  if (ptr == NULL)
    return;

#ifdef CYGWIN
  if (ptr < _heapbase)
    /* We're being asked to free something in the static heap. */
    return;
#endif

  PROTECT_MALLOC_STATE (0);

  LOCK_ALIGNED_BLOCKS ();
  for (l = _aligned_blocks; l != NULL; l = l->next)
    if (l->aligned == ptr)
      {
	l->aligned = NULL;	/* Mark the slot in the list as free.  */
	ptr = l->exact;
	break;
      }
  UNLOCK_ALIGNED_BLOCKS ();

  block = BLOCK (ptr);

  type = _heapinfo[block].busy.type;
  switch (type)
    {
    case 0:
      /* Get as many statistics as early as we can.  */
      --_chunks_used;
      _bytes_used -= _heapinfo[block].busy.info.size * BLOCKSIZE;
      _bytes_free += _heapinfo[block].busy.info.size * BLOCKSIZE;

      /* Find the free cluster previous to this one in the free list.
	 Start searching at the last block referenced; this may benefit
	 programs with locality of allocation.  */
      i = _heapindex;
      if (i > block)
	while (i > block)
	  i = _heapinfo[i].free.prev;
      else
	{
	  do
	    i = _heapinfo[i].free.next;
	  while (i > 0 && i < block);
	  i = _heapinfo[i].free.prev;
	}

      /* Determine how to link this block into the free list.  */
      if (block == i + _heapinfo[i].free.size)
	{
	  /* Coalesce this block with its predecessor.  */
	  _heapinfo[i].free.size += _heapinfo[block].busy.info.size;
	  block = i;
	}
      else
	{
	  /* Really link this block back into the free list.  */
	  _heapinfo[block].free.size = _heapinfo[block].busy.info.size;
	  _heapinfo[block].free.next = _heapinfo[i].free.next;
	  _heapinfo[block].free.prev = i;
	  _heapinfo[i].free.next = block;
	  _heapinfo[_heapinfo[block].free.next].free.prev = block;
	  ++_chunks_free;
	}

      /* Now that the block is linked in, see if we can coalesce it
	 with its successor (by deleting its successor from the list
	 and adding in its size).  */
      if (block + _heapinfo[block].free.size == _heapinfo[block].free.next)
	{
	  _heapinfo[block].free.size
	    += _heapinfo[_heapinfo[block].free.next].free.size;
	  _heapinfo[block].free.next
	    = _heapinfo[_heapinfo[block].free.next].free.next;
	  _heapinfo[_heapinfo[block].free.next].free.prev = block;
	  --_chunks_free;
	}

      /* How many trailing free blocks are there now?  */
      blocks = _heapinfo[block].free.size;

      /* Where is the current end of accessible core?  */
      curbrk = (*__morecore) (0);

      if (_heaplimit != 0 && curbrk == ADDRESS (_heaplimit))
	{
	  /* The end of the malloc heap is at the end of accessible core.
	     It's possible that moving _heapinfo will allow us to
	     return some space to the system.  */

 	  __malloc_size_t info_block = BLOCK (_heapinfo);
 	  __malloc_size_t info_blocks = _heapinfo[info_block].busy.info.size;
 	  __malloc_size_t prev_block = _heapinfo[block].free.prev;
 	  __malloc_size_t prev_blocks = _heapinfo[prev_block].free.size;
 	  __malloc_size_t next_block = _heapinfo[block].free.next;
 	  __malloc_size_t next_blocks = _heapinfo[next_block].free.size;

	  if (/* Win if this block being freed is last in core, the info table
		 is just before it, the previous free block is just before the
		 info table, and the two free blocks together form a useful
		 amount to return to the system.  */
	      (block + blocks == _heaplimit &&
	       info_block + info_blocks == block &&
	       prev_block != 0 && prev_block + prev_blocks == info_block &&
	       blocks + prev_blocks >= lesscore_threshold) ||
	      /* Nope, not the case.  We can also win if this block being
		 freed is just before the info table, and the table extends
		 to the end of core or is followed only by a free block,
		 and the total free space is worth returning to the system.  */
	      (block + blocks == info_block &&
	       ((info_block + info_blocks == _heaplimit &&
		 blocks >= lesscore_threshold) ||
		(info_block + info_blocks == next_block &&
		 next_block + next_blocks == _heaplimit &&
		 blocks + next_blocks >= lesscore_threshold)))
	      )
	    {
	      malloc_info *newinfo;
	      __malloc_size_t oldlimit = _heaplimit;

	      /* Free the old info table, clearing _heaplimit to avoid
		 recursion into this code.  We don't want to return the
		 table's blocks to the system before we have copied them to
		 the new location.  */
	      _heaplimit = 0;
	      _free_internal_nolock (_heapinfo);
	      _heaplimit = oldlimit;

	      /* Tell malloc to search from the beginning of the heap for
		 free blocks, so it doesn't reuse the ones just freed.  */
	      _heapindex = 0;

	      /* Allocate new space for the info table and move its data.  */
	      newinfo = (malloc_info *) _malloc_internal_nolock (info_blocks
								 * BLOCKSIZE);
	      PROTECT_MALLOC_STATE (0);
	      memmove (newinfo, _heapinfo, info_blocks * BLOCKSIZE);
	      _heapinfo = newinfo;

	      /* We should now have coalesced the free block with the
		 blocks freed from the old info table.  Examine the entire
		 trailing free block to decide below whether to return some
		 to the system.  */
	      block = _heapinfo[0].free.prev;
	      blocks = _heapinfo[block].free.size;
 	    }

	  /* Now see if we can return stuff to the system.  */
	  if (block + blocks == _heaplimit && blocks >= lesscore_threshold)
	    {
	      register __malloc_size_t bytes = blocks * BLOCKSIZE;
	      _heaplimit -= blocks;
	      (*__morecore) (-bytes);
	      _heapinfo[_heapinfo[block].free.prev].free.next
		= _heapinfo[block].free.next;
	      _heapinfo[_heapinfo[block].free.next].free.prev
		= _heapinfo[block].free.prev;
	      block = _heapinfo[block].free.prev;
	      --_chunks_free;
	      _bytes_free -= bytes;
	    }
	}

      /* Set the next search to begin at this block.  */
      _heapindex = block;
      break;

    default:
      /* Do some of the statistics.  */
      --_chunks_used;
      _bytes_used -= 1 << type;
      ++_chunks_free;
      _bytes_free += 1 << type;

      /* Get the address of the first free fragment in this block.  */
      prev = (struct list *) ((char *) ADDRESS (block) +
			      (_heapinfo[block].busy.info.frag.first << type));

      if (_heapinfo[block].busy.info.frag.nfree == (BLOCKSIZE >> type) - 1)
	{
	  /* If all fragments of this block are free, remove them
	     from the fragment list and free the whole block.  */
	  next = prev;
	  for (i = 1; i < (__malloc_size_t) (BLOCKSIZE >> type); ++i)
	    next = next->next;
	  prev->prev->next = next;
	  if (next != NULL)
	    next->prev = prev->prev;
	  _heapinfo[block].busy.type = 0;
	  _heapinfo[block].busy.info.size = 1;

	  /* Keep the statistics accurate.  */
	  ++_chunks_used;
	  _bytes_used += BLOCKSIZE;
	  _chunks_free -= BLOCKSIZE >> type;
	  _bytes_free -= BLOCKSIZE;

#if defined (GC_MALLOC_CHECK) || defined (USE_PTHREAD)
	  _free_internal_nolock (ADDRESS (block));
#else
	  free (ADDRESS (block));
#endif
	}
      else if (_heapinfo[block].busy.info.frag.nfree != 0)
	{
	  /* If some fragments of this block are free, link this
	     fragment into the fragment list after the first free
	     fragment of this block. */
	  next = (struct list *) ptr;
	  next->next = prev->next;
	  next->prev = prev;
	  prev->next = next;
	  if (next->next != NULL)
	    next->next->prev = next;
	  ++_heapinfo[block].busy.info.frag.nfree;
	}
      else
	{
	  /* No fragments of this block are free, so link this
	     fragment into the fragment list and announce that
	     it is the first free fragment of this block. */
	  prev = (struct list *) ptr;
	  _heapinfo[block].busy.info.frag.nfree = 1;
	  _heapinfo[block].busy.info.frag.first = (unsigned long int)
	    ((unsigned long int) ((char *) ptr - (char *) NULL)
	     % BLOCKSIZE >> type);
	  prev->next = _fraghead[type].next;
	  prev->prev = &_fraghead[type];
	  prev->prev->next = prev;
	  if (prev->next != NULL)
	    prev->next->prev = prev;
	}
      break;
    }

  PROTECT_MALLOC_STATE (1);
}

/* Return memory to the heap.
   Like `free' but don't call a __free_hook if there is one.  */
void
_free_internal (ptr)
     __ptr_t ptr;
{
  LOCK ();
  _free_internal_nolock (ptr);
  UNLOCK ();
}

/* Return memory to the heap.  */

void
free (ptr)
     __ptr_t ptr;
{
  void (*hook) (__ptr_t) = __free_hook;

  if (hook != NULL)
    (*hook) (ptr);
  else
    _free_internal (ptr);
}

/* Define the `cfree' alias for `free'.  */
#ifdef weak_alias
weak_alias (free, cfree)
#else
void
cfree (ptr)
     __ptr_t ptr;
{
  free (ptr);
}
#endif
/* Change the size of a block allocated by `malloc'.
   Copyright 1990, 1991, 1992, 1993, 1994, 1995 Free Software Foundation, Inc.
		     Written May 1989 by Mike Haertel.

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public
License along with this library; see the file COPYING.  If
not, write to the Free Software Foundation, Inc., 51 Franklin Street,
Fifth Floor, Boston, MA 02110-1301, USA.

   The author may be reached (Email) at the address mike@ai.mit.edu,
   or (US mail) as Mike Haertel c/o Free Software Foundation.  */

#ifndef	_MALLOC_INTERNAL
#define _MALLOC_INTERNAL
#include <malloc.h>
#endif


#define min(A, B) ((A) < (B) ? (A) : (B))

/* On Cygwin the dumped emacs may try to realloc storage allocated in
   the static heap.  We just malloc space in the new heap and copy the
   data.  */
#ifdef CYGWIN
__ptr_t
special_realloc (ptr, size)
     __ptr_t ptr;
     __malloc_size_t size;
{
  __ptr_t result;
  int type;
  __malloc_size_t block, oldsize;

  block = ((char *) ptr - bss_sbrk_heapbase) / BLOCKSIZE + 1;
  type = bss_sbrk_heapinfo[block].busy.type;
  oldsize =
    type == 0 ? bss_sbrk_heapinfo[block].busy.info.size * BLOCKSIZE
    : (__malloc_size_t) 1 << type;
  result = _malloc_internal_nolock (size);
  if (result != NULL)
    memcpy (result, ptr, min (oldsize, size));
  return result;
}
#endif

/* Debugging hook for realloc.  */
__ptr_t (*__realloc_hook) PP ((__ptr_t __ptr, __malloc_size_t __size));

/* Resize the given region to the new size, returning a pointer
   to the (possibly moved) region.  This is optimized for speed;
   some benchmarks seem to indicate that greater compactness is
   achieved by unconditionally allocating and copying to a
   new region.  This module has incestuous knowledge of the
   internals of both free and malloc. */
__ptr_t
_realloc_internal_nolock (ptr, size)
     __ptr_t ptr;
     __malloc_size_t size;
{
  __ptr_t result;
  int type;
  __malloc_size_t block, blocks, oldlimit;

  if (size == 0)
    {
      _free_internal_nolock (ptr);
      return _malloc_internal_nolock (0);
    }
  else if (ptr == NULL)
    return _malloc_internal_nolock (size);

#ifdef CYGWIN
  if (ptr < _heapbase)
    /* ptr points into the static heap */
    return special_realloc (ptr, size);
#endif

  block = BLOCK (ptr);

  PROTECT_MALLOC_STATE (0);

  type = _heapinfo[block].busy.type;
  switch (type)
    {
    case 0:
      /* Maybe reallocate a large block to a small fragment.  */
      if (size <= BLOCKSIZE / 2)
	{
	  result = _malloc_internal_nolock (size);
	  if (result != NULL)
	    {
	      memcpy (result, ptr, size);
	      _free_internal_nolock (ptr);
	      goto out;
	    }
	}

      /* The new size is a large allocation as well;
	 see if we can hold it in place. */
      blocks = BLOCKIFY (size);
      if (blocks < _heapinfo[block].busy.info.size)
	{
	  /* The new size is smaller; return
	     excess memory to the free list. */
	  _heapinfo[block + blocks].busy.type = 0;
	  _heapinfo[block + blocks].busy.info.size
	    = _heapinfo[block].busy.info.size - blocks;
	  _heapinfo[block].busy.info.size = blocks;
	  /* We have just created a new chunk by splitting a chunk in two.
	     Now we will free this chunk; increment the statistics counter
	     so it doesn't become wrong when _free_internal decrements it.  */
	  ++_chunks_used;
	  _free_internal_nolock (ADDRESS (block + blocks));
	  result = ptr;
	}
      else if (blocks == _heapinfo[block].busy.info.size)
	/* No size change necessary.  */
	result = ptr;
      else
	{
	  /* Won't fit, so allocate a new region that will.
	     Free the old region first in case there is sufficient
	     adjacent free space to grow without moving. */
	  blocks = _heapinfo[block].busy.info.size;
	  /* Prevent free from actually returning memory to the system.  */
	  oldlimit = _heaplimit;
	  _heaplimit = 0;
	  _free_internal_nolock (ptr);
	  result = _malloc_internal_nolock (size);
	  PROTECT_MALLOC_STATE (0);
	  if (_heaplimit == 0)
	    _heaplimit = oldlimit;
	  if (result == NULL)
	    {
	      /* Now we're really in trouble.  We have to unfree
		 the thing we just freed.  Unfortunately it might
		 have been coalesced with its neighbors.  */
	      if (_heapindex == block)
	        (void) _malloc_internal_nolock (blocks * BLOCKSIZE);
	      else
		{
		  __ptr_t previous
		    = _malloc_internal_nolock ((block - _heapindex) * BLOCKSIZE);
		  (void) _malloc_internal_nolock (blocks * BLOCKSIZE);
		  _free_internal_nolock (previous);
		}
	      goto out;
	    }
	  if (ptr != result)
	    memmove (result, ptr, blocks * BLOCKSIZE);
	}
      break;

    default:
      /* Old size is a fragment; type is logarithm
	 to base two of the fragment size.  */
      if (size > (__malloc_size_t) (1 << (type - 1)) &&
	  size <= (__malloc_size_t) (1 << type))
	/* The new size is the same kind of fragment.  */
	result = ptr;
      else
	{
	  /* The new size is different; allocate a new space,
	     and copy the lesser of the new size and the old. */
	  result = _malloc_internal_nolock (size);
	  if (result == NULL)
	    goto out;
	  memcpy (result, ptr, min (size, (__malloc_size_t) 1 << type));
	  _free_internal_nolock (ptr);
	}
      break;
    }

  PROTECT_MALLOC_STATE (1);
 out:
  return result;
}

__ptr_t
_realloc_internal (ptr, size)
     __ptr_t ptr;
     __malloc_size_t size;
{
  __ptr_t result;

  LOCK ();
  result = _realloc_internal_nolock (ptr, size);
  UNLOCK ();

  return result;
}

__ptr_t
realloc (ptr, size)
     __ptr_t ptr;
     __malloc_size_t size;
{
  __ptr_t (*hook) (__ptr_t, __malloc_size_t);

  if (!__malloc_initialized && !__malloc_initialize ())
    return NULL;

  hook = __realloc_hook;
  return (hook != NULL ? *hook : _realloc_internal) (ptr, size);
}
/* Copyright (C) 1991, 1992, 1994 Free Software Foundation, Inc.

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public
License along with this library; see the file COPYING.  If
not, write to the Free Software Foundation, Inc., 51 Franklin Street,
Fifth Floor, Boston, MA 02110-1301, USA.

   The author may be reached (Email) at the address mike@ai.mit.edu,
   or (US mail) as Mike Haertel c/o Free Software Foundation.  */

#ifndef	_MALLOC_INTERNAL
#define	_MALLOC_INTERNAL
#include <malloc.h>
#endif

/* Allocate an array of NMEMB elements each SIZE bytes long.
   The entire array is initialized to zeros.  */
__ptr_t
calloc (nmemb, size)
     register __malloc_size_t nmemb;
     register __malloc_size_t size;
{
  register __ptr_t result = malloc (nmemb * size);

  if (result != NULL)
    (void) memset (result, 0, nmemb * size);

  return result;
}
/* Copyright (C) 1991, 1992, 1993, 1994, 1995 Free Software Foundation, Inc.
This file is part of the GNU C Library.

The GNU C Library is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

The GNU C Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with the GNU C Library; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,
MA 02110-1301, USA.  */

#ifndef	_MALLOC_INTERNAL
#define	_MALLOC_INTERNAL
#include <malloc.h>
#endif

/* uClibc defines __GNU_LIBRARY__, but it is not completely
   compatible.  */
#if !defined (__GNU_LIBRARY__) || defined (__UCLIBC__)
#define	__sbrk	sbrk
#else /* __GNU_LIBRARY__ && ! defined (__UCLIBC__) */
/* It is best not to declare this and cast its result on foreign operating
   systems with potentially hostile include files.  */

#include <stddef.h>
extern __ptr_t __sbrk PP ((ptrdiff_t increment));
#endif /* __GNU_LIBRARY__ && ! defined (__UCLIBC__) */

#ifndef NULL
#define NULL 0
#endif

/* Allocate INCREMENT more bytes of data space,
   and return the start of data space, or NULL on errors.
   If INCREMENT is negative, shrink data space.  */
__ptr_t
__default_morecore (increment)
     __malloc_ptrdiff_t increment;
{
  __ptr_t result;
#if defined (CYGWIN)
  if (!bss_sbrk_did_unexec)
    {
      return bss_sbrk (increment);
    }
#endif
  result = (__ptr_t) __sbrk (increment);
  if (result == (__ptr_t) -1)
    return NULL;
  return result;
}
/* Copyright (C) 1991, 92, 93, 94, 95, 96 Free Software Foundation, Inc.

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public
License along with this library; see the file COPYING.  If
not, write to the Free Software Foundation, Inc., 51 Franklin Street,
Fifth Floor, Boston, MA 02110-1301, USA.  */

#ifndef	_MALLOC_INTERNAL
#define _MALLOC_INTERNAL
#include <malloc.h>
#endif

__ptr_t (*__memalign_hook) PP ((__malloc_size_t __size,
				__malloc_size_t __alignment));

__ptr_t
memalign (alignment, size)
     __malloc_size_t alignment;
     __malloc_size_t size;
{
  __ptr_t result;
  unsigned long int adj, lastadj;
  __ptr_t (*hook) (__malloc_size_t, __malloc_size_t) = __memalign_hook;

  if (hook)
    return (*hook) (alignment, size);

  /* Allocate a block with enough extra space to pad the block with up to
     (ALIGNMENT - 1) bytes if necessary.  */
  result = malloc (size + alignment - 1);
  if (result == NULL)
    return NULL;

  /* Figure out how much we will need to pad this particular block
     to achieve the required alignment.  */
  adj = (unsigned long int) ((char *) result - (char *) NULL) % alignment;

  do
    {
      /* Reallocate the block with only as much excess as it needs.  */
      free (result);
      result = malloc (adj + size);
      if (result == NULL)	/* Impossible unless interrupted.  */
	return NULL;

      lastadj = adj;
      adj = (unsigned long int) ((char *) result - (char *) NULL) % alignment;
      /* It's conceivable we might have been so unlucky as to get a
	 different block with weaker alignment.  If so, this block is too
	 short to contain SIZE after alignment correction.  So we must
	 try again and get another block, slightly larger.  */
    } while (adj > lastadj);

  if (adj != 0)
    {
      /* Record this block in the list of aligned blocks, so that `free'
	 can identify the pointer it is passed, which will be in the middle
	 of an allocated block.  */

      struct alignlist *l;
      LOCK_ALIGNED_BLOCKS ();
      for (l = _aligned_blocks; l != NULL; l = l->next)
	if (l->aligned == NULL)
	  /* This slot is free.  Use it.  */
	  break;
      if (l == NULL)
	{
	  l = (struct alignlist *) malloc (sizeof (struct alignlist));
	  if (l != NULL)
	    {
	      l->next = _aligned_blocks;
	      _aligned_blocks = l;
	    }
	}
      if (l != NULL)
	{
	  l->exact = result;
	  result = l->aligned = (char *) result + alignment - adj;
	}
      UNLOCK_ALIGNED_BLOCKS ();
      if (l == NULL)
	{
	  free (result);
	  result = NULL;
	}
    }

  return result;
}

#ifndef ENOMEM
#define ENOMEM 12
#endif

#ifndef EINVAL
#define EINVAL 22
#endif

int
posix_memalign (memptr, alignment, size)
     __ptr_t *memptr;
     __malloc_size_t alignment;
     __malloc_size_t size;
{
  __ptr_t mem;

  if (alignment == 0
      || alignment % sizeof (__ptr_t) != 0
      || (alignment & (alignment - 1)) != 0)
    return EINVAL;

  mem = memalign (alignment, size);
  if (mem == NULL)
    return ENOMEM;

  *memptr = mem;

  return 0;
}

/* Allocate memory on a page boundary.
   Copyright (C) 1991, 92, 93, 94, 96 Free Software Foundation, Inc.

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public
License along with this library; see the file COPYING.  If
not, write to the Free Software Foundation, Inc., 51 Franklin Street,
Fifth Floor, Boston, MA 02110-1301, USA.

   The author may be reached (Email) at the address mike@ai.mit.edu,
   or (US mail) as Mike Haertel c/o Free Software Foundation.  */

#if defined (_MALLOC_INTERNAL) && defined (GMALLOC_INHIBIT_VALLOC)

/* Emacs defines GMALLOC_INHIBIT_VALLOC to avoid this definition
   on MSDOS, where it conflicts with a system header file.  */

#define ELIDE_VALLOC

#endif

#ifndef	ELIDE_VALLOC

#if defined (__GNU_LIBRARY__) || defined (_LIBC)
#include <stddef.h>
#include <sys/cdefs.h>
#if defined (__GLIBC__) && __GLIBC__ >= 2
/* __getpagesize is already declared in <unistd.h> with return type int */
#else
extern size_t __getpagesize PP ((void));
#endif
#else
#include "getpagesize.h"
#define	 __getpagesize()	getpagesize ()
#endif

#ifndef	_MALLOC_INTERNAL
#define	_MALLOC_INTERNAL
#include <malloc.h>
#endif

static __malloc_size_t pagesize;

__ptr_t
valloc (size)
     __malloc_size_t size;
{
  if (pagesize == 0)
    pagesize = __getpagesize ();

  return memalign (pagesize, size);
}

#endif	/* Not ELIDE_VALLOC.  */

#ifdef GC_MCHECK

/* Standard debugging hooks for `malloc'.
   Copyright 1990, 1991, 1992, 1993, 1994 Free Software Foundation, Inc.
   Written May 1989 by Mike Haertel.

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public
License along with this library; see the file COPYING.  If
not, write to the Free Software Foundation, Inc., 51 Franklin Street,
Fifth Floor, Boston, MA 02110-1301, USA.

   The author may be reached (Email) at the address mike@ai.mit.edu,
   or (US mail) as Mike Haertel c/o Free Software Foundation.  */

#ifdef emacs
#include <stdio.h>
#else
#ifndef	_MALLOC_INTERNAL
#define	_MALLOC_INTERNAL
#include <malloc.h>
#include <stdio.h>
#endif
#endif

/* Old hook values.  */
static void (*old_free_hook) (__ptr_t ptr);
static __ptr_t (*old_malloc_hook) (__malloc_size_t size);
static __ptr_t (*old_realloc_hook) (__ptr_t ptr, __malloc_size_t size);

/* Function to call when something awful happens.  */
static void (*abortfunc) (enum mcheck_status);

/* Arbitrary magical numbers.  */
#define MAGICWORD	0xfedabeeb
#define MAGICFREE	0xd8675309
#define MAGICBYTE	((char) 0xd7)
#define MALLOCFLOOD	((char) 0x93)
#define FREEFLOOD	((char) 0x95)

struct hdr
  {
    __malloc_size_t size;		/* Exact size requested by user.  */
    unsigned long int magic;	/* Magic number to check header integrity.  */
  };

static enum mcheck_status checkhdr (const struct hdr *);
static enum mcheck_status
checkhdr (hdr)
     const struct hdr *hdr;
{
  enum mcheck_status status;
  switch (hdr->magic)
    {
    default:
      status = MCHECK_HEAD;
      break;
    case MAGICFREE:
      status = MCHECK_FREE;
      break;
    case MAGICWORD:
      if (((char *) &hdr[1])[hdr->size] != MAGICBYTE)
	status = MCHECK_TAIL;
      else
	status = MCHECK_OK;
      break;
    }
  if (status != MCHECK_OK)
    (*abortfunc) (status);
  return status;
}

static void freehook (__ptr_t);
static void
freehook (ptr)
     __ptr_t ptr;
{
  struct hdr *hdr;

  if (ptr)
    {
      hdr = ((struct hdr *) ptr) - 1;
      checkhdr (hdr);
      hdr->magic = MAGICFREE;
      memset (ptr, FREEFLOOD, hdr->size);
    }
  else
    hdr = NULL;

  __free_hook = old_free_hook;
  free (hdr);
  __free_hook = freehook;
}

static __ptr_t mallochook (__malloc_size_t);
static __ptr_t
mallochook (size)
     __malloc_size_t size;
{
  struct hdr *hdr;

  __malloc_hook = old_malloc_hook;
  hdr = (struct hdr *) malloc (sizeof (struct hdr) + size + 1);
  __malloc_hook = mallochook;
  if (hdr == NULL)
    return NULL;

  hdr->size = size;
  hdr->magic = MAGICWORD;
  ((char *) &hdr[1])[size] = MAGICBYTE;
  memset ((__ptr_t) (hdr + 1), MALLOCFLOOD, size);
  return (__ptr_t) (hdr + 1);
}

static __ptr_t reallochook (__ptr_t, __malloc_size_t);
static __ptr_t
reallochook (ptr, size)
     __ptr_t ptr;
     __malloc_size_t size;
{
  struct hdr *hdr = NULL;
  __malloc_size_t osize = 0;

  if (ptr)
    {
      hdr = ((struct hdr *) ptr) - 1;
      osize = hdr->size;

      checkhdr (hdr);
      if (size < osize)
	memset ((char *) ptr + size, FREEFLOOD, osize - size);
    }

  __free_hook = old_free_hook;
  __malloc_hook = old_malloc_hook;
  __realloc_hook = old_realloc_hook;
  hdr = (struct hdr *) realloc ((__ptr_t) hdr, sizeof (struct hdr) + size + 1);
  __free_hook = freehook;
  __malloc_hook = mallochook;
  __realloc_hook = reallochook;
  if (hdr == NULL)
    return NULL;

  hdr->size = size;
  hdr->magic = MAGICWORD;
  ((char *) &hdr[1])[size] = MAGICBYTE;
  if (size > osize)
    memset ((char *) (hdr + 1) + osize, MALLOCFLOOD, size - osize);
  return (__ptr_t) (hdr + 1);
}

static void
mabort (status)
     enum mcheck_status status;
{
  const char *msg;
  switch (status)
    {
    case MCHECK_OK:
      msg = "memory is consistent, library is buggy";
      break;
    case MCHECK_HEAD:
      msg = "memory clobbered before allocated block";
      break;
    case MCHECK_TAIL:
      msg = "memory clobbered past end of allocated block";
      break;
    case MCHECK_FREE:
      msg = "block freed twice";
      break;
    default:
      msg = "bogus mcheck_status, library is buggy";
      break;
    }
#ifdef __GNU_LIBRARY__
  __libc_fatal (msg);
#else
  fprintf (stderr, "mcheck: %s\n", msg);
  fflush (stderr);
  abort ();
#endif
}

static int mcheck_used = 0;

int
mcheck (func)
     void (*func) (enum mcheck_status);
{
  abortfunc = (func != NULL) ? func : &mabort;

  /* These hooks may not be safely inserted if malloc is already in use.  */
  if (!__malloc_initialized && !mcheck_used)
    {
      old_free_hook = __free_hook;
      __free_hook = freehook;
      old_malloc_hook = __malloc_hook;
      __malloc_hook = mallochook;
      old_realloc_hook = __realloc_hook;
      __realloc_hook = reallochook;
      mcheck_used = 1;
    }

  return mcheck_used ? 0 : -1;
}

enum mcheck_status
mprobe (__ptr_t ptr)
{
  return mcheck_used ? checkhdr (ptr) : MCHECK_DISABLED;
}

#endif /* GC_MCHECK */
