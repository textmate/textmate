/*
 * Minimal git-compat-util.h shim for vendored xdiff.
 *
 * The upstream xdiff sources (in github.com/git/git, xdiff/) include
 * "git-compat-util.h" — a 1100-line portability header. xdiff itself
 * uses only a small fraction of it: xmalloc/xcalloc/xrealloc and the
 * BUG() macro. Everything else it needs is in libc and POSIX.
 *
 * We provide a shim of those four symbols here so the upstream xdiff
 * sources can be vendored byte-for-byte unchanged. When resyncing
 * from upstream, do not edit the xdiff sources; only this file may
 * need adjustment if a future xdiff version starts using a new
 * symbol from git-compat-util.h.
 */

#ifndef VENDORED_XDIFF_GIT_COMPAT_UTIL_H
#define VENDORED_XDIFF_GIT_COMPAT_UTIL_H

#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>
#include <stdio.h>
#include <stdarg.h>
#include <regex.h>
#include <limits.h>

#ifdef __cplusplus
extern "C" {
#endif

void* xmalloc (size_t size);
void* xcalloc (size_t nmemb, size_t size);
void* xrealloc (void* ptr, size_t size);

void xdiff_compat_bug (char const* file, int line, char const* fmt, ...)
	__attribute__((noreturn, format(printf, 3, 4)));

#define BUG(...) xdiff_compat_bug(__FILE__, __LINE__, __VA_ARGS__)

#define UNUSED       __attribute__((unused))
#define MAYBE_UNUSED __attribute__((unused))

#define bitsizeof(x) (CHAR_BIT * sizeof(x))

#define maximum_signed_value_of_type(a)   (INTMAX_MAX  >> (bitsizeof(intmax_t)  - bitsizeof(a)))
#define maximum_unsigned_value_of_type(a) (UINTMAX_MAX >> (bitsizeof(uintmax_t) - bitsizeof(a)))

#define signed_add_overflows(a, b)   ((b) > maximum_signed_value_of_type(a)   - (a))
#define unsigned_add_overflows(a, b) ((b) > maximum_unsigned_value_of_type(a) - (a))

#include <assert.h>
static inline int regexec_buf (regex_t const* preg, char const* buf, size_t size,
                               size_t nmatch, regmatch_t pmatch[], int eflags)
{
	assert(nmatch > 0 && pmatch);
	pmatch[0].rm_so = 0;
	pmatch[0].rm_eo = size;
	return regexec(preg, buf, nmatch, pmatch, eflags | REG_STARTEND);
}

#ifdef __cplusplus
}
#endif

#endif
