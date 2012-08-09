/* Fake unistd.h: config.h already provides most of the relevant things. */

#ifndef _UNISTD_H
#define _UNISTD_H

extern ssize_t readlink (const char *, char *, size_t);
extern int symlink (char const *, char const *);

#endif	/* _UNISTD_H */

