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

#ifndef OAK_EXTERN_C_BEGIN
#ifdef __cplusplus
#define OAK_EXTERN_C_BEGIN extern "C" {
#else
#define OAK_EXTERN_C_BEGIN
#endif
#endif

#ifndef OAK_EXTERN_C_END
#ifdef __cplusplus
#define OAK_EXTERN_C_END }
#else
#define OAK_EXTERN_C_END
#endif
#endif

#endif /* end of include guard: OAK_MISC_H_35H67VAO */
