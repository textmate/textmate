#ifndef OAK_MISC_H_35H67VAO
#define OAK_MISC_H_35H67VAO

#ifndef sizeofA
#define sizeofA(a) (sizeof(a)/sizeof(a[0]))
#endif

#ifndef SQ
#define SQ(x) ((x)*(x))
#endif

#ifndef STRINGIFY
#define xSTRINGIFY(number) #number
#define STRINGIFY(number)  xSTRINGIFY(number)
#endif

__attribute__ ((format (printf, 1, 2))) inline void perrorf (char const* format, ...)
{
	char* err = strerror(errno);
	char* msg = NULL;

	va_list ap;
	va_start(ap, format);
	vasprintf(&msg, format, ap);
	va_end(ap);

	fprintf(stderr, "%s: %s\n", msg, err);
	free(msg);
}

#endif /* end of include guard: OAK_MISC_H_35H67VAO */
