#ifndef OAK_MISC_H_35H67VAO
#define OAK_MISC_H_35H67VAO

#ifndef PUBLIC
#define PUBLIC __attribute__((__visibility__("default")))
#endif

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

#endif /* end of include guard: OAK_MISC_H_35H67VAO */
