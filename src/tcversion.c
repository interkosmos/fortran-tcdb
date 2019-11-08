/* tcversion.c */
/* Simple wrapper function for `*tcversion`. */
#include <tcutil.h>

const char *tc_version()
{
    return tcversion;
}
