/*
 * crnl.c  2007/05/30  K.Kosako
 *
 * !!! You should enable USE_CRNL_AS_LINE_TERMINATOR. !!!
 * 
 * USE_CRNL_AS_LINE_TERMINATOR config test program.
 */
#include <stdio.h>
#include <string.h>
#include "oniguruma.h"

static int nfail = 0;

static void result(int no, int from, int to,
		   int expected_from, int expected_to)
{
  fprintf(stderr, "%3d: ", no);
  if (from == expected_from && to == expected_to) {
    fprintf(stderr, "Success\n");
  }
  else {
    fprintf(stderr, "Fail: expected: (%d-%d), result: (%d-%d)\n",
	    expected_from, expected_to, from, to);

    nfail++;
  }
}

static int
x(int no, char* pattern_arg, char* str_arg,
  int expected_from, int expected_to)
{
  int r;
  unsigned char *start, *range, *end;
  regex_t* reg;
  OnigErrorInfo einfo;
  OnigRegion *region;
  UChar *pattern, *str;

  pattern = (UChar* )pattern_arg;
  str     = (UChar* )str_arg;

  r = onig_new(&reg, pattern, pattern + strlen((char* )pattern),
	ONIG_OPTION_DEFAULT, ONIG_ENCODING_ASCII, ONIG_SYNTAX_DEFAULT, &einfo);
  if (r != ONIG_NORMAL) {
    char s[ONIG_MAX_ERROR_MESSAGE_LEN];
    onig_error_code_to_str(s, r, &einfo);
    fprintf(stderr, "ERROR: %s\n", s);
    return -1;
  }

  region = onig_region_new();

  end   = str + strlen((char* )str);
  start = str;
  range = end;
  r = onig_search(reg, str, end, start, range, region, ONIG_OPTION_NONE);
  if (r >= 0 || r == ONIG_MISMATCH) {
    result(no, region->beg[0], region->end[0], expected_from, expected_to);
  }
  else if (r == ONIG_MISMATCH) {
    result(no, r, -1, expected_from, expected_to);
  }
  else { /* error */
    char s[ONIG_MAX_ERROR_MESSAGE_LEN];
    onig_error_code_to_str(s, r);
    fprintf(stderr, "ERROR: %s\n", s);
    return -1;
  }

  onig_region_free(region, 1 /* 1:free self, 0:free contents only */);
  onig_free(reg);
  return 0;
}

static int
f(int no, char* pattern_arg, char* str_arg)
{
  return x(no, pattern_arg, str_arg, -1, -1);
}

extern int main(int argc, char* argv[])
{
  x( 1, "",        "\r\n",        0,  0);
  x( 2, ".",       "\r\n",        0,  1);
  f( 3, "..",      "\r\n");
  x( 4, "^",       "\r\n",        0,  0);
  x( 5, "\\n^",    "\r\nf",       1,  2);
  x( 6, "\\n^a",   "\r\na",       1,  3);
  x( 7, "$",       "\r\n",        0,  0);
  x( 8, "T$",      "T\r\n",       0,  1);
  x( 9, "T$",      "T\raT\r\n",   3,  4);
  x(10, "\\z",     "\r\n",        2,  2);
  f(11, "a\\z",    "a\r\n");
  x(12, "\\Z",     "\r\n",        0,  0);
  x(13, "\\Z",     "\r\na",       3,  3);
  x(14, "\\Z",     "\r\n\r\n\n",  4,  4);
  x(15, "\\Z",     "\r\n\r\nX",   5,  5);
  x(16, "a\\Z",    "a\r\n",       0,  1);
  x(17, "aaaaaaaaaaaaaaa\\Z",   "aaaaaaaaaaaaaaa\r\n",  0,  15);
  x(18, "a|$",     "b\r\n",       1,  1);
  x(19, "$|b",     "\rb",         1,  2);
  x(20, "a$|ab$",  "\r\nab\r\n",  2,  4);

  x(21, "a|\\Z",       "b\r\n",       1,  1);
  x(22, "\\Z|b",       "\rb",         1,  2);
  x(23, "a\\Z|ab\\Z",  "\r\nab\r\n",  2,  4);
  x(24, "(?=a$).",     "a\r\n",       0,  1);
  f(25, "(?=a$).",     "a\r");
  x(26, "(?!a$)..",    "a\r",         0,  2);
  x(27, "(?<=a$).\\n", "a\r\n",       1,  3);
  f(28, "(?<!a$).\\n", "a\r\n");
  x(29, "(?=a\\Z).",     "a\r\n",       0,  1);
  f(30, "(?=a\\Z).",     "a\r");
  x(31, "(?!a\\Z)..",    "a\r",         0,  2);

  onig_end();

  if (nfail > 0) {
    fprintf(stderr, "\n");
    fprintf(stderr, "!!! You have to enable USE_CRNL_AS_LINE_TERMINATOR\n");
    fprintf(stderr, "!!! in regenc.h for this test program.\n");
    fprintf(stderr, "\n");
  }

  return 0;
}
