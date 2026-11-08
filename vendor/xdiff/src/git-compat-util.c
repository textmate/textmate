#include "git-compat-util.h"

void* xmalloc (size_t size)
{
	void* p = malloc(size);
	if(!p)
		xdiff_compat_bug(__FILE__, __LINE__, "xmalloc(%zu) failed", size);
	return p;
}

void* xcalloc (size_t nmemb, size_t size)
{
	void* p = calloc(nmemb, size);
	if(!p)
		xdiff_compat_bug(__FILE__, __LINE__, "xcalloc(%zu, %zu) failed", nmemb, size);
	return p;
}

void* xrealloc (void* ptr, size_t size)
{
	void* p = realloc(ptr, size);
	if(!p)
		xdiff_compat_bug(__FILE__, __LINE__, "xrealloc(%zu) failed", size);
	return p;
}

void xdiff_compat_bug (char const* file, int line, char const* fmt, ...)
{
	va_list ap;
	fprintf(stderr, "BUG: %s:%d: ", file, line);
	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
	fputc('\n', stderr);
	abort();
}
