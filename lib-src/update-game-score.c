/* update-game-score.c --- Update a score file

Copyright (C) 2002-2012  Free Software Foundation, Inc.

Author: Colin Walters <walters@debian.org>

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.  */


/* This program allows a game to securely and atomically update a
   score file.  It should be installed setuid, owned by an appropriate
   user like `games'.

   Alternatively, it can be compiled without HAVE_SHARED_GAME_DIR
   defined, and in that case it will store scores in the user's home
   directory (it should NOT be setuid).

   Created 2002/03/22.
*/

#include <config.h>

#include <unistd.h>
#include <errno.h>
#include <limits.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <pwd.h>
#include <ctype.h>
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#include <sys/stat.h>

/* Needed for SunOS4, for instance.  */
extern char *optarg;
extern int optind, opterr;

static int usage (int err) NO_RETURN;

#define MAX_ATTEMPTS 5
#define MAX_SCORES 200
#define MAX_DATA_LEN 1024

#ifndef HAVE_DIFFTIME
/* OK on POSIX (time_t is arithmetic type) modulo overflow in subtraction.  */
#define difftime(t1, t0) (double)((t1) - (t0))
#endif

static int
usage (int err)
{
  fprintf (stdout, "Usage: update-game-score [-m MAX] [-r] [-d DIR] game/scorefile SCORE DATA\n");
  fprintf (stdout, "       update-game-score -h\n");
  fprintf (stdout, " -h\t\tDisplay this help.\n");
  fprintf (stdout, " -m MAX\t\tLimit the maximum number of scores to MAX.\n");
  fprintf (stdout, " -r\t\tSort the scores in increasing order.\n");
  fprintf (stdout, " -d DIR\t\tStore scores in DIR (only if not setuid).\n");
  exit (err);
}

static int lock_file (const char *filename, void **state);
static int unlock_file (const char *filename, void *state);

struct score_entry
{
  long score;
  char *username;
  char *data;
};

static int read_scores (const char *filename, struct score_entry **scores,
			int *count);
static int push_score (struct score_entry **scores, int *count,
		       int newscore, char *username, char *newdata);
static void sort_scores (struct score_entry *scores, int count, int reverse);
static int write_scores (const char *filename,
			 const struct score_entry *scores, int count);

static void lose (const char *msg) NO_RETURN;

static void
lose (const char *msg)
{
  fprintf (stderr, "%s\n", msg);
  exit (EXIT_FAILURE);
}

static void lose_syserr (const char *msg) NO_RETURN;

/* Taken from sysdep.c.  */
#ifndef HAVE_STRERROR
#ifndef WINDOWSNT
char *
strerror (int errnum)
{
  extern char *sys_errlist[];
  extern int sys_nerr;

  if (errnum >= 0 && errnum < sys_nerr)
    return sys_errlist[errnum];
  return (char *) "Unknown error";
}
#endif /* not WINDOWSNT */
#endif /* ! HAVE_STRERROR */

static void
lose_syserr (const char *msg)
{
  fprintf (stderr, "%s: %s\n", msg, strerror (errno));
  exit (EXIT_FAILURE);
}

static char *
get_user_id (void)
{
  struct passwd *buf = getpwuid (getuid ());
  if (!buf)
    {
      long uid = getuid ();
      char *name = malloc (sizeof uid * CHAR_BIT / 3 + 1);
      if (name)
	sprintf (name, "%ld", uid);
      return name;
    }
  return buf->pw_name;
}

static const char *
get_prefix (int running_suid, const char *user_prefix)
{
  if (!running_suid && user_prefix == NULL)
    lose ("Not using a shared game directory, and no prefix given.");
  if (running_suid)
    {
#ifdef HAVE_SHARED_GAME_DIR
      return HAVE_SHARED_GAME_DIR;
#else
      lose ("This program was compiled without HAVE_SHARED_GAME_DIR,\n and should not be suid.");
#endif
    }
  return user_prefix;
}

int
main (int argc, char **argv)
{
  int c, running_suid;
  void *lockstate;
  char *user_id, *scorefile;
  const char *prefix, *user_prefix = NULL;
  struct stat buf;
  struct score_entry *scores;
  int newscore, scorecount, reverse = 0, max = MAX_SCORES;
  char *newdata;

  srand (time (0));

  while ((c = getopt (argc, argv, "hrm:d:")) != -1)
    switch (c)
      {
      case 'h':
	usage (EXIT_SUCCESS);
	break;
      case 'd':
	user_prefix = optarg;
	break;
      case 'r':
	reverse = 1;
	break;
      case 'm':
	max = atoi (optarg);
	if (max > MAX_SCORES)
	  max = MAX_SCORES;
	break;
      default:
	usage (EXIT_FAILURE);
      }

  if (optind+3 != argc)
    usage (EXIT_FAILURE);

  running_suid = (getuid () != geteuid ());

  prefix = get_prefix (running_suid, user_prefix);

  scorefile = malloc (strlen (prefix) + strlen (argv[optind]) + 2);
  if (!scorefile)
    lose_syserr ("Couldn't allocate score file");

  strcpy (scorefile, prefix);
  strcat (scorefile, "/");
  strcat (scorefile, argv[optind]);
  newscore = atoi (argv[optind+1]);
  newdata = argv[optind+2];
  if (strlen (newdata) > MAX_DATA_LEN)
    newdata[MAX_DATA_LEN] = '\0';

  user_id = get_user_id ();
  if (user_id == NULL)
    lose_syserr ("Couldn't determine user id");

  if (stat (scorefile, &buf) < 0)
    lose_syserr ("Failed to access scores file");

  if (lock_file (scorefile, &lockstate) < 0)
    lose_syserr ("Failed to lock scores file");

  if (read_scores (scorefile, &scores, &scorecount) < 0)
    {
      unlock_file (scorefile, lockstate);
      lose_syserr ("Failed to read scores file");
    }
  push_score (&scores, &scorecount, newscore, user_id, newdata);
  sort_scores (scores, scorecount, reverse);
  /* Limit the number of scores.  If we're using reverse sorting, then
     also increment the beginning of the array, to skip over the
     *smallest* scores.  Otherwise, just decrementing the number of
     scores suffices, since the smallest is at the end. */
  if (scorecount > MAX_SCORES)
    {
      if (reverse)
	scores += (scorecount - MAX_SCORES);
      scorecount = MAX_SCORES;
    }
  if (write_scores (scorefile, scores, scorecount) < 0)
    {
      unlock_file (scorefile, lockstate);
      lose_syserr ("Failed to write scores file");
    }
  unlock_file (scorefile, lockstate);
  exit (EXIT_SUCCESS);
}

static int
read_score (FILE *f, struct score_entry *score)
{
  int c;
  if (feof (f))
    return 1;
  while ((c = getc (f)) != EOF
	 && isdigit (c))
    {
      score->score *= 10;
      score->score += (c-48);
    }
  while ((c = getc (f)) != EOF
	 && isspace (c))
    ;
  if (c == EOF)
    return -1;
  ungetc (c, f);
#ifdef HAVE_GETDELIM
  {
    size_t count = 0;
    if (getdelim (&score->username, &count, ' ', f) < 1
	|| score->username == NULL)
      return -1;
    /* Trim the space */
    score->username[strlen (score->username)-1] = '\0';
  }
#else
  {
    int unameread = 0;
    int unamelen = 30;
    char *username = malloc (unamelen);
    if (!username)
      return -1;

    while ((c = getc (f)) != EOF
	   && !isspace (c))
      {
	if (unameread >= unamelen-1)
	  if (!(username = realloc (username, unamelen *= 2)))
	    return -1;
	username[unameread] = c;
	unameread++;
      }
    if (c == EOF)
      return -1;
    username[unameread] = '\0';
    score->username = username;
  }
#endif
#ifdef HAVE_GETLINE
  score->data = NULL;
  errno = 0;
  {
    size_t len;
    if (getline (&score->data, &len, f) < 0)
      return -1;
    score->data[strlen (score->data)-1] = '\0';
  }
#else
  {
    int cur = 0;
    int len = 16;
    char *buf = malloc (len);
    if (!buf)
      return -1;
    while ((c = getc (f)) != EOF
	   && c != '\n')
      {
	if (cur >= len-1)
	  {
	    if (!(buf = realloc (buf, len *= 2)))
	      return -1;
	  }
	buf[cur] = c;
	cur++;
      }
    score->data = buf;
    score->data[cur] = '\0';
  }
#endif
  return 0;
}

static int
read_scores (const char *filename, struct score_entry **scores, int *count)
{
  int readval, scorecount, cursize;
  struct score_entry *ret;
  FILE *f = fopen (filename, "r");
  if (!f)
    return -1;
  scorecount = 0;
  cursize = 16;
  ret = (struct score_entry *) malloc (sizeof (struct score_entry) * cursize);
  if (!ret)
    return -1;
  while ((readval = read_score (f, &ret[scorecount])) == 0)
    {
      /* We encountered an error.  */
      if (readval < 0)
	return -1;
      scorecount++;
      if (scorecount >= cursize)
	{
	  cursize *= 2;
	  ret = (struct score_entry *)
	    realloc (ret, (sizeof (struct score_entry) * cursize));
	  if (!ret)
	    return -1;
	}
    }
  *count = scorecount;
  *scores = ret;
  return 0;
}

static int
score_compare (const void *a, const void *b)
{
  const struct score_entry *sa = (const struct score_entry *) a;
  const struct score_entry *sb = (const struct score_entry *) b;
  return (sb->score > sa->score) - (sb->score < sa->score);
}

static int
score_compare_reverse (const void *a, const void *b)
{
  const struct score_entry *sa = (const struct score_entry *) a;
  const struct score_entry *sb = (const struct score_entry *) b;
  return (sa->score > sb->score) - (sa->score < sb->score);
}

int
push_score (struct score_entry **scores, int *count, int newscore, char *username, char *newdata)
{
 struct score_entry *newscores
   = (struct score_entry *) realloc (*scores,
				     sizeof (struct score_entry) * ((*count) + 1));
  if (!newscores)
    return -1;
  newscores[*count].score = newscore;
  newscores[*count].username = username;
  newscores[*count].data = newdata;
  (*count) += 1;
  *scores = newscores;
  return 0;
}

static void
sort_scores (struct score_entry *scores, int count, int reverse)
{
  qsort (scores, count, sizeof (struct score_entry),
	reverse ? score_compare_reverse : score_compare);
}

static int
write_scores (const char *filename, const struct score_entry *scores, int count)
{
  FILE *f;
  int i;
  char *tempfile = malloc (strlen (filename) + strlen (".tempXXXXXX") + 1);
  if (!tempfile)
    return -1;
  strcpy (tempfile, filename);
  strcat (tempfile, ".tempXXXXXX");
#ifdef HAVE_MKSTEMP
  if (mkstemp (tempfile) < 0
#else
  if (mktemp (tempfile) != tempfile
#endif
      || !(f = fopen (tempfile, "w")))
    return -1;
  for (i = 0; i < count; i++)
    if (fprintf (f, "%ld %s %s\n", scores[i].score, scores[i].username,
		scores[i].data) < 0)
      return -1;
  fclose (f);
  if (rename (tempfile, filename) < 0)
    return -1;
  if (chmod (filename, 0644) < 0)
    return -1;
  return 0;
}

static int
lock_file (const char *filename, void **state)
{
  int fd;
  struct stat buf;
  int attempts = 0;
  const char *lockext = ".lockfile";
  char *lockpath = malloc (strlen (filename) + strlen (lockext) + 60);
  if (!lockpath)
    return -1;
  strcpy (lockpath, filename);
  strcat (lockpath, lockext);
  *state = lockpath;
 trylock:
  attempts++;
  /* If the lock is over an hour old, delete it. */
  if (stat (lockpath, &buf) == 0
      && (difftime (buf.st_ctime, time (NULL) > 60*60)))
    unlink (lockpath);
  fd = open (lockpath, O_CREAT | O_EXCL, 0600);
  if (fd < 0)
    {
      if (errno == EEXIST)
	{
	  /* Break the lock; we won't corrupt the file, but we might
	     lose some scores. */
	  if (attempts > MAX_ATTEMPTS)
	    {
	      unlink (lockpath);
	      attempts = 0;
	    }
	  sleep ((rand () % 2)+1);
	  goto trylock;
	}
      else
	return -1;
    }
  close (fd);
  return 0;
}

static int
unlock_file (const char *filename, void *state)
{
  char *lockpath = (char *) state;
  int ret = unlink (lockpath);
  int saved_errno = errno;
  free (lockpath);
  errno = saved_errno;
  return ret;
}


/* update-game-score.c ends here */
