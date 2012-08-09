/* Declarations of functions and data types used for SHA512 and SHA384 sum
   library functions.
   Copyright (C) 2005-2006, 2008-2011 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

#ifndef SHA512_H
# define SHA512_H 1

# include <stdio.h>

# include "u64.h"

# ifdef __cplusplus
extern "C" {
# endif

/* Structure to save state of computation between the single steps.  */
struct sha512_ctx
{
  u64 state[8];

  u64 total[2];
  size_t buflen;
  u64 buffer[32];
};

enum { SHA384_DIGEST_SIZE = 384 / 8 };
enum { SHA512_DIGEST_SIZE = 512 / 8 };

/* Initialize structure containing state of computation. */
extern void sha512_init_ctx (struct sha512_ctx *ctx);
extern void sha384_init_ctx (struct sha512_ctx *ctx);

/* Starting with the result of former calls of this function (or the
   initialization function update the context for the next LEN bytes
   starting at BUFFER.
   It is necessary that LEN is a multiple of 128!!! */
extern void sha512_process_block (const void *buffer, size_t len,
                                  struct sha512_ctx *ctx);

/* Starting with the result of former calls of this function (or the
   initialization function update the context for the next LEN bytes
   starting at BUFFER.
   It is NOT required that LEN is a multiple of 128.  */
extern void sha512_process_bytes (const void *buffer, size_t len,
                                  struct sha512_ctx *ctx);

/* Process the remaining bytes in the buffer and put result from CTX
   in first 64 (48) bytes following RESBUF.  The result is always in little
   endian byte order, so that a byte-wise output yields to the wanted
   ASCII representation of the message digest.  */
extern void *sha512_finish_ctx (struct sha512_ctx *ctx, void *resbuf);
extern void *sha384_finish_ctx (struct sha512_ctx *ctx, void *resbuf);


/* Put result from CTX in first 64 (48) bytes following RESBUF.  The result is
   always in little endian byte order, so that a byte-wise output yields
   to the wanted ASCII representation of the message digest.

   IMPORTANT: On some systems it is required that RESBUF is correctly
   aligned for a 32 bits value.  */
extern void *sha512_read_ctx (const struct sha512_ctx *ctx, void *resbuf);
extern void *sha384_read_ctx (const struct sha512_ctx *ctx, void *resbuf);


/* Compute SHA512 (SHA384) message digest for bytes read from STREAM.  The
   resulting message digest number will be written into the 64 (48) bytes
   beginning at RESBLOCK.  */
extern int sha512_stream (FILE *stream, void *resblock);
extern int sha384_stream (FILE *stream, void *resblock);

/* Compute SHA512 (SHA384) message digest for LEN bytes beginning at BUFFER.  The
   result is always in little endian byte order, so that a byte-wise
   output yields to the wanted ASCII representation of the message
   digest.  */
extern void *sha512_buffer (const char *buffer, size_t len, void *resblock);
extern void *sha384_buffer (const char *buffer, size_t len, void *resblock);

# ifdef __cplusplus
}
# endif

#endif
