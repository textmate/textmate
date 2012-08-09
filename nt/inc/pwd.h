#ifndef _PWD_H_
#define _PWD_H_
/*
 * pwd.h doesn't exist on NT, so we put together our own.
 */

struct passwd {
    char     *pw_name;
    char     *pw_passwd;
    unsigned  pw_uid;  /* Vista's TrustedInstaller has a very large RID */
    unsigned  pw_gid;
    int       pw_quota;
    char     *pw_gecos;
    char     *pw_dir;
    char     *pw_shell;
};

typedef unsigned uid_t;
typedef uid_t gid_t;

struct passwd * getpwnam (char *);
struct passwd * getpwuid (unsigned);


#endif /* _PWD_H_ */

