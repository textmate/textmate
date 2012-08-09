/*
 * sys\ioctl.h doesn't exist on NT...rather than including it conditionally
 * in many of the source files, we just extend the include path so that the
 * compiler will pick this up empty header instead.
 */

