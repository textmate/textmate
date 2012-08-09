#include <time.h>
time_t mktime_internal (struct tm *,
                        struct tm * (*) (time_t const *, struct tm *),
                        time_t *);
