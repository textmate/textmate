/* sound.c -- sound support.
   Copyright (C) 1998-1999, 2001-2012 Free Software Foundation, Inc.

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

/* Written by Gerd Moellmann <gerd@gnu.org>.  Tested with Luigi's
   driver on FreeBSD 2.2.7 with a SoundBlaster 16.  */

/*
  Modified by Ben Key <Bkey1@tampabay.rr.com> to add a partial
  implementation of the play-sound specification for Windows.

  Notes:
  In the Windows implementation of play-sound-internal only the
  :file and :volume keywords are supported.  The :device keyword,
  if present, is ignored.  The :data keyword, if present, will
  cause an error to be generated.

  The Windows implementation of play-sound is implemented via the
  Win32 API functions mciSendString, waveOutGetVolume, and
  waveOutSetVolume which are exported by Winmm.dll.
*/

#include <config.h>

#if defined HAVE_SOUND

/* BEGIN: Common Includes */
#include <fcntl.h>
#include <unistd.h>
#include <sys/types.h>
#include <errno.h>
#include <setjmp.h>
#include "lisp.h"
#include "dispextern.h"
#include "atimer.h"
#include <signal.h>
#include "syssignal.h"
/* END: Common Includes */


/* BEGIN: Non Windows Includes */
#ifndef WINDOWSNT

#include <sys/ioctl.h>

/* FreeBSD has machine/soundcard.h.  Voxware sound driver docs mention
   sys/soundcard.h.  So, let's try whatever's there.  */

#ifdef HAVE_MACHINE_SOUNDCARD_H
#include <machine/soundcard.h>
#endif
#ifdef HAVE_SYS_SOUNDCARD_H
#include <sys/soundcard.h>
#endif
#ifdef HAVE_SOUNDCARD_H
#include <soundcard.h>
#endif
#ifdef HAVE_ALSA
#ifdef ALSA_SUBDIR_INCLUDE
#include <alsa/asoundlib.h>
#else
#include <asoundlib.h>
#endif /* ALSA_SUBDIR_INCLUDE */
#endif /* HAVE_ALSA */

/* END: Non Windows Includes */

#else /* WINDOWSNT */

/* BEGIN: Windows Specific Includes */
#include <stdio.h>
#include <limits.h>
#include <windows.h>
#include <mmsystem.h>
/* END: Windows Specific Includes */

#endif /* WINDOWSNT */

/* BEGIN: Common Definitions */

/* Symbols.  */

static Lisp_Object QCvolume, QCdevice;
static Lisp_Object Qsound;
static Lisp_Object Qplay_sound_functions;

/* Indices of attributes in a sound attributes vector.  */

enum sound_attr
{
  SOUND_FILE,
  SOUND_DATA,
  SOUND_DEVICE,
  SOUND_VOLUME,
  SOUND_ATTR_SENTINEL
};

#ifdef HAVE_ALSA
static void alsa_sound_perror (const char *, int) NO_RETURN;
#endif
static void sound_perror (const char *) NO_RETURN;
static void sound_warning (const char *);
static int parse_sound (Lisp_Object, Lisp_Object *);

/* END: Common Definitions */

/* BEGIN: Non Windows Definitions */
#ifndef WINDOWSNT

#ifndef DEFAULT_SOUND_DEVICE
#define DEFAULT_SOUND_DEVICE "/dev/dsp"
#endif
#ifndef DEFAULT_ALSA_SOUND_DEVICE
#define DEFAULT_ALSA_SOUND_DEVICE "default"
#endif


/* Structure forward declarations.  */

struct sound;
struct sound_device;

/* The file header of RIFF-WAVE files (*.wav).  Files are always in
   little-endian byte-order.  */

struct wav_header
{
  u_int32_t magic;
  u_int32_t length;
  u_int32_t chunk_type;
  u_int32_t chunk_format;
  u_int32_t chunk_length;
  u_int16_t format;
  u_int16_t channels;
  u_int32_t sample_rate;
  u_int32_t bytes_per_second;
  u_int16_t sample_size;
  u_int16_t precision;
  u_int32_t chunk_data;
  u_int32_t data_length;
};

/* The file header of Sun adio files (*.au).  Files are always in
   big-endian byte-order.  */

struct au_header
{
  /* ASCII ".snd" */
  u_int32_t magic_number;

  /* Offset of data part from start of file. Minimum value is 24.  */
  u_int32_t data_offset;

  /* Size of data part, 0xffffffff if unknown.  */
  u_int32_t data_size;

  /* Data encoding format.
     1	8-bit ISDN u-law
     2  8-bit linear PCM (REF-PCM)
     3  16-bit linear PCM
     4	24-bit linear PCM
     5	32-bit linear PCM
     6	32-bit IEEE floating-point
     7	64-bit IEEE floating-point
     23 8-bit u-law compressed using CCITT 0.721 ADPCM voice data
     encoding scheme.  */
  u_int32_t encoding;

  /* Number of samples per second.  */
  u_int32_t sample_rate;

  /* Number of interleaved channels.  */
  u_int32_t channels;
};

/* Maximum of all sound file headers sizes.  */

#define MAX_SOUND_HEADER_BYTES \
     max (sizeof (struct wav_header), sizeof (struct au_header))

/* Interface structure for sound devices.  */

struct sound_device
{
  /* The name of the device or null meaning use a default device name.  */
  char *file;

  /* File descriptor of the device.  */
  int fd;

  /* Device-dependent format.  */
  int format;

  /* Volume (0..100).  Zero means unspecified.  */
  int volume;

  /* Sample size.  */
  int sample_size;

  /* Sample rate.  */
  int sample_rate;

  /* Bytes per second.  */
  int bps;

  /* 1 = mono, 2 = stereo, 0 = don't set.  */
  int channels;

  /* Open device SD.  */
  void (* open) (struct sound_device *sd);

  /* Close device SD.  */
  void (* close) (struct sound_device *sd);

  /* Configure SD according to device-dependent parameters.  */
  void (* configure) (struct sound_device *device);

  /* Choose a device-dependent format for outputting sound S.  */
  void (* choose_format) (struct sound_device *sd,
                          struct sound *s);

  /* Return a preferred data size in bytes to be sent to write (below)
     each time.  2048 is used if this is NULL.  */
  EMACS_INT (* period_size) (struct sound_device *sd);

  /* Write NYBTES bytes from BUFFER to device SD.  */
  void (* write) (struct sound_device *sd, const char *buffer,
                  EMACS_INT nbytes);

  /* A place for devices to store additional data.  */
  void *data;
};

/* An enumerator for each supported sound file type.  */

enum sound_type
{
  RIFF,
  SUN_AUDIO
};

/* Interface structure for sound files.  */

struct sound
{
  /* The type of the file.  */
  enum sound_type type;

  /* File descriptor of a sound file.  */
  int fd;

  /* Pointer to sound file header.  This contains header_size bytes
     read from the start of a sound file.  */
  char *header;

  /* Number of bytes read from sound file.  This is always <=
     MAX_SOUND_HEADER_BYTES.  */
  int header_size;

  /* Sound data, if a string.  */
  Lisp_Object data;

  /* Play sound file S on device SD.  */
  void (* play) (struct sound *s, struct sound_device *sd);
};

/* These are set during `play-sound-internal' so that sound_cleanup has
   access to them.  */

static struct sound_device *current_sound_device;
static struct sound *current_sound;

/* Function prototypes.  */

static void vox_open (struct sound_device *);
static void vox_configure (struct sound_device *);
static void vox_close (struct sound_device *sd);
static void vox_choose_format (struct sound_device *, struct sound *);
static int vox_init (struct sound_device *);
static void vox_write (struct sound_device *, const char *, EMACS_INT);
static void find_sound_type (struct sound *);
static u_int32_t le2hl (u_int32_t);
static u_int16_t le2hs (u_int16_t);
static u_int32_t be2hl (u_int32_t);
static int wav_init (struct sound *);
static void wav_play (struct sound *, struct sound_device *);
static int au_init (struct sound *);
static void au_play (struct sound *, struct sound_device *);

#if 0 /* Currently not used.  */
static u_int16_t be2hs (u_int16_t);
#endif

/* END: Non Windows Definitions */
#else /* WINDOWSNT */

/* BEGIN: Windows Specific Definitions */
static int do_play_sound (const char *, unsigned long);
/*
  END: Windows Specific Definitions */
#endif /* WINDOWSNT */


/***********************************************************************
			       General
 ***********************************************************************/

/* BEGIN: Common functions */

/* Like perror, but signals an error.  */

static void
sound_perror (const char *msg)
{
  int saved_errno = errno;

  turn_on_atimers (1);
#ifdef SIGIO
  sigunblock (sigmask (SIGIO));
#endif
  if (saved_errno != 0)
    error ("%s: %s", msg, strerror (saved_errno));
  else
    error ("%s", msg);
}


/* Display a warning message.  */

static void
sound_warning (const char *msg)
{
  message ("%s", msg);
}


/* Parse sound specification SOUND, and fill ATTRS with what is
   found.  Value is non-zero if SOUND Is a valid sound specification.
   A valid sound specification is a list starting with the symbol
   `sound'.  The rest of the list is a property list which may
   contain the following key/value pairs:

   - `:file FILE'

   FILE is the sound file to play.  If it isn't an absolute name,
   it's searched under `data-directory'.

   - `:data DATA'

   DATA is a string containing sound data.  Either :file or :data
   may be present, but not both.

   - `:device DEVICE'

   DEVICE is the name of the device to play on, e.g. "/dev/dsp2".
   If not specified, a default device is used.

   - `:volume VOL'

   VOL must be an integer in the range [0, 100], or a float in the
   range [0, 1].  */

static int
parse_sound (Lisp_Object sound, Lisp_Object *attrs)
{
  /* SOUND must be a list starting with the symbol `sound'.  */
  if (!CONSP (sound) || !EQ (XCAR (sound), Qsound))
    return 0;

  sound = XCDR (sound);
  attrs[SOUND_FILE] = Fplist_get (sound, QCfile);
  attrs[SOUND_DATA] = Fplist_get (sound, QCdata);
  attrs[SOUND_DEVICE] = Fplist_get (sound, QCdevice);
  attrs[SOUND_VOLUME] = Fplist_get (sound, QCvolume);

#ifndef WINDOWSNT
  /* File name or data must be specified.  */
  if (!STRINGP (attrs[SOUND_FILE])
      && !STRINGP (attrs[SOUND_DATA]))
    return 0;
#else /* WINDOWSNT */
  /*
    Data is not supported in Windows.  Therefore a
    File name MUST be supplied.
  */
  if (!STRINGP (attrs[SOUND_FILE]))
    {
      return 0;
    }
#endif /* WINDOWSNT */

  /* Volume must be in the range 0..100 or unspecified.  */
  if (!NILP (attrs[SOUND_VOLUME]))
    {
      if (INTEGERP (attrs[SOUND_VOLUME]))
	{
	  if (XINT (attrs[SOUND_VOLUME]) < 0
	      || XINT (attrs[SOUND_VOLUME]) > 100)
	    return 0;
	}
      else if (FLOATP (attrs[SOUND_VOLUME]))
	{
	  if (XFLOAT_DATA (attrs[SOUND_VOLUME]) < 0
	      || XFLOAT_DATA (attrs[SOUND_VOLUME]) > 1)
	    return 0;
	}
      else
	return 0;
    }

#ifndef WINDOWSNT
  /* Device must be a string or unspecified.  */
  if (!NILP (attrs[SOUND_DEVICE])
      && !STRINGP (attrs[SOUND_DEVICE]))
    return 0;
#endif  /* WINDOWSNT */
  /*
    Since device is ignored in Windows, it does not matter
    what it is.
   */
  return 1;
}

/* END: Common functions */

/* BEGIN: Non Windows functions */
#ifndef WINDOWSNT

/* Find out the type of the sound file whose file descriptor is FD.
   S is the sound file structure to fill in.  */

static void
find_sound_type (struct sound *s)
{
  if (!wav_init (s) && !au_init (s))
    error ("Unknown sound format");
}


/* Function installed by play-sound-internal with record_unwind_protect.  */

static Lisp_Object
sound_cleanup (Lisp_Object arg)
{
  if (current_sound_device->close)
    current_sound_device->close (current_sound_device);
  if (current_sound->fd > 0)
    emacs_close (current_sound->fd);
  xfree (current_sound_device);
  xfree (current_sound);

  return Qnil;
}

/***********************************************************************
			Byte-order Conversion
 ***********************************************************************/

/* Convert 32-bit value VALUE which is in little-endian byte-order
   to host byte-order.  */

static u_int32_t
le2hl (u_int32_t value)
{
#ifdef WORDS_BIGENDIAN
  unsigned char *p = (unsigned char *) &value;
  value = p[0] + (p[1] << 8) + (p[2] << 16) + (p[3] << 24);
#endif
  return value;
}


/* Convert 16-bit value VALUE which is in little-endian byte-order
   to host byte-order.  */

static u_int16_t
le2hs (u_int16_t value)
{
#ifdef WORDS_BIGENDIAN
  unsigned char *p = (unsigned char *) &value;
  value = p[0] + (p[1] << 8);
#endif
  return value;
}


/* Convert 32-bit value VALUE which is in big-endian byte-order
   to host byte-order.  */

static u_int32_t
be2hl (u_int32_t value)
{
#ifndef WORDS_BIGENDIAN
  unsigned char *p = (unsigned char *) &value;
  value = p[3] + (p[2] << 8) + (p[1] << 16) + (p[0] << 24);
#endif
  return value;
}


#if 0 /* Currently not used.  */

/* Convert 16-bit value VALUE which is in big-endian byte-order
   to host byte-order.  */

static u_int16_t
be2hs (u_int16_t value)
{
#ifndef WORDS_BIGENDIAN
  unsigned char *p = (unsigned char *) &value;
  value = p[1] + (p[0] << 8);
#endif
  return value;
}

#endif /* 0 */

/***********************************************************************
			  RIFF-WAVE (*.wav)
 ***********************************************************************/

/* Try to initialize sound file S from S->header.  S->header
   contains the first MAX_SOUND_HEADER_BYTES number of bytes from the
   sound file.  If the file is a WAV-format file, set up interface
   functions in S and convert header fields to host byte-order.
   Value is non-zero if the file is a WAV file.  */

static int
wav_init (struct sound *s)
{
  struct wav_header *header = (struct wav_header *) s->header;

  if (s->header_size < sizeof *header
      || memcmp (s->header, "RIFF", 4) != 0)
    return 0;

  /* WAV files are in little-endian order.  Convert the header
     if on a big-endian machine.  */
  header->magic = le2hl (header->magic);
  header->length = le2hl (header->length);
  header->chunk_type = le2hl (header->chunk_type);
  header->chunk_format = le2hl (header->chunk_format);
  header->chunk_length = le2hl (header->chunk_length);
  header->format = le2hs (header->format);
  header->channels = le2hs (header->channels);
  header->sample_rate = le2hl (header->sample_rate);
  header->bytes_per_second = le2hl (header->bytes_per_second);
  header->sample_size = le2hs (header->sample_size);
  header->precision = le2hs (header->precision);
  header->chunk_data = le2hl (header->chunk_data);
  header->data_length = le2hl (header->data_length);

  /* Set up the interface functions for WAV.  */
  s->type = RIFF;
  s->play = wav_play;

  return 1;
}


/* Play RIFF-WAVE audio file S on sound device SD.  */

static void
wav_play (struct sound *s, struct sound_device *sd)
{
  struct wav_header *header = (struct wav_header *) s->header;

  /* Let the device choose a suitable device-dependent format
     for the file.  */
  sd->choose_format (sd, s);

  /* Configure the device.  */
  sd->sample_size = header->sample_size;
  sd->sample_rate = header->sample_rate;
  sd->bps = header->bytes_per_second;
  sd->channels = header->channels;
  sd->configure (sd);

  /* Copy sound data to the device.  The WAV file specification is
     actually more complex.  This simple scheme worked with all WAV
     files I found so far.  If someone feels inclined to implement the
     whole RIFF-WAVE spec, please do.  */
  if (STRINGP (s->data))
    sd->write (sd, SSDATA (s->data) + sizeof *header,
	       SBYTES (s->data) - sizeof *header);
  else
    {
      char *buffer;
      EMACS_INT nbytes = 0;
      EMACS_INT blksize = sd->period_size ? sd->period_size (sd) : 2048;
      EMACS_INT data_left = header->data_length;

      buffer = (char *) alloca (blksize);
      lseek (s->fd, sizeof *header, SEEK_SET);
      while (data_left > 0
             && (nbytes = emacs_read (s->fd, buffer, blksize)) > 0)
        {
          /* Don't play possible garbage at the end of file */
          if (data_left < nbytes) nbytes = data_left;
          data_left -= nbytes;
          sd->write (sd, buffer, nbytes);
        }

      if (nbytes < 0)
	sound_perror ("Error reading sound file");
    }
}


/***********************************************************************
			   Sun Audio (*.au)
 ***********************************************************************/

/* Sun audio file encodings.  */

enum au_encoding
{
  AU_ENCODING_ULAW_8 = 1,
  AU_ENCODING_8,
  AU_ENCODING_16,
  AU_ENCODING_24,
  AU_ENCODING_32,
  AU_ENCODING_IEEE32,
  AU_ENCODING_IEEE64,
  AU_COMPRESSED = 23,
  AU_ENCODING_ALAW_8 = 27
};


/* Try to initialize sound file S from S->header.  S->header
   contains the first MAX_SOUND_HEADER_BYTES number of bytes from the
   sound file.  If the file is a AU-format file, set up interface
   functions in S and convert header fields to host byte-order.
   Value is non-zero if the file is an AU file.  */

static int
au_init (struct sound *s)
{
  struct au_header *header = (struct au_header *) s->header;

  if (s->header_size < sizeof *header
      || memcmp (s->header, ".snd", 4) != 0)
    return 0;

  header->magic_number = be2hl (header->magic_number);
  header->data_offset = be2hl (header->data_offset);
  header->data_size = be2hl (header->data_size);
  header->encoding = be2hl (header->encoding);
  header->sample_rate = be2hl (header->sample_rate);
  header->channels = be2hl (header->channels);

  /* Set up the interface functions for AU.  */
  s->type = SUN_AUDIO;
  s->play = au_play;

  return 1;
}


/* Play Sun audio file S on sound device SD.  */

static void
au_play (struct sound *s, struct sound_device *sd)
{
  struct au_header *header = (struct au_header *) s->header;

  sd->sample_size = 0;
  sd->sample_rate = header->sample_rate;
  sd->bps = 0;
  sd->channels = header->channels;
  sd->choose_format (sd, s);
  sd->configure (sd);

  if (STRINGP (s->data))
    sd->write (sd, SSDATA (s->data) + header->data_offset,
	       SBYTES (s->data) - header->data_offset);
  else
    {
      EMACS_INT blksize = sd->period_size ? sd->period_size (sd) : 2048;
      char *buffer;
      EMACS_INT nbytes;

      /* Seek */
      lseek (s->fd, header->data_offset, SEEK_SET);

      /* Copy sound data to the device.  */
      buffer = (char *) alloca (blksize);
      while ((nbytes = emacs_read (s->fd, buffer, blksize)) > 0)
	sd->write (sd, buffer, nbytes);

      if (nbytes < 0)
	sound_perror ("Error reading sound file");
    }
}


/***********************************************************************
		       Voxware Driver Interface
 ***********************************************************************/

/* This driver is available on GNU/Linux, and the free BSDs.  FreeBSD
   has a compatible own driver aka Luigi's driver.  */


/* Open device SD.  If SD->file is non-null, open that device,
   otherwise use a default device name.  */

static void
vox_open (struct sound_device *sd)
{
  const char *file;

  /* Open the sound device.  Default is /dev/dsp.  */
  if (sd->file)
    file = sd->file;
  else
    file = DEFAULT_SOUND_DEVICE;

  sd->fd = emacs_open (file, O_WRONLY, 0);
  if (sd->fd < 0)
    sound_perror (file);
}


/* Configure device SD from parameters in it.  */

static void
vox_configure (struct sound_device *sd)
{
  int val;

  xassert (sd->fd >= 0);

  /* On GNU/Linux, it seems that the device driver doesn't like to be
     interrupted by a signal.  Block the ones we know to cause
     troubles.  */
  turn_on_atimers (0);
#ifdef SIGIO
  sigblock (sigmask (SIGIO));
#endif

  val = sd->format;
  if (ioctl (sd->fd, SNDCTL_DSP_SETFMT, &sd->format) < 0
      || val != sd->format)
    sound_perror ("Could not set sound format");

  val = sd->channels != 1;
  if (ioctl (sd->fd, SNDCTL_DSP_STEREO, &val) < 0
      || val != (sd->channels != 1))
    sound_perror ("Could not set stereo/mono");

  /* I think bps and sampling_rate are the same, but who knows.
     Check this. and use SND_DSP_SPEED for both.  */
  if (sd->sample_rate > 0)
    {
      val = sd->sample_rate;
      if (ioctl (sd->fd, SNDCTL_DSP_SPEED, &sd->sample_rate) < 0)
	sound_perror ("Could not set sound speed");
      else if (val != sd->sample_rate)
	sound_warning ("Could not set sample rate");
    }

  if (sd->volume > 0)
    {
      int volume = sd->volume & 0xff;
      volume |= volume << 8;
      /* This may fail if there is no mixer.  Ignore the failure.  */
      ioctl (sd->fd, SOUND_MIXER_WRITE_PCM, &volume);
    }

  turn_on_atimers (1);
#ifdef SIGIO
  sigunblock (sigmask (SIGIO));
#endif
}


/* Close device SD if it is open.  */

static void
vox_close (struct sound_device *sd)
{
  if (sd->fd >= 0)
    {
      /* On GNU/Linux, it seems that the device driver doesn't like to
	 be interrupted by a signal.  Block the ones we know to cause
	 troubles.  */
#ifdef SIGIO
      sigblock (sigmask (SIGIO));
#endif
      turn_on_atimers (0);

      /* Flush sound data, and reset the device.  */
      ioctl (sd->fd, SNDCTL_DSP_SYNC, NULL);

      turn_on_atimers (1);
#ifdef SIGIO
      sigunblock (sigmask (SIGIO));
#endif

      /* Close the device.  */
      emacs_close (sd->fd);
      sd->fd = -1;
    }
}


/* Choose device-dependent format for device SD from sound file S.  */

static void
vox_choose_format (struct sound_device *sd, struct sound *s)
{
  if (s->type == RIFF)
    {
      struct wav_header *h = (struct wav_header *) s->header;
      if (h->precision == 8)
	sd->format = AFMT_U8;
      else if (h->precision == 16)
	sd->format = AFMT_S16_LE;
      else
	error ("Unsupported WAV file format");
    }
  else if (s->type == SUN_AUDIO)
    {
      struct au_header *header = (struct au_header *) s->header;
      switch (header->encoding)
	{
	case AU_ENCODING_ULAW_8:
	case AU_ENCODING_IEEE32:
	case AU_ENCODING_IEEE64:
	  sd->format = AFMT_MU_LAW;
	  break;

	case AU_ENCODING_8:
	case AU_ENCODING_16:
	case AU_ENCODING_24:
	case AU_ENCODING_32:
	  sd->format = AFMT_S16_LE;
	  break;

	default:
	  error ("Unsupported AU file format");
	}
    }
  else
    abort ();
}


/* Initialize device SD.  Set up the interface functions in the device
   structure.  */

static int
vox_init (struct sound_device *sd)
{
  const char *file;
  int fd;

  /* Open the sound device.  Default is /dev/dsp.  */
  if (sd->file)
    file = sd->file;
  else
    file = DEFAULT_SOUND_DEVICE;
  fd = emacs_open (file, O_WRONLY, 0);
  if (fd >= 0)
    emacs_close (fd);
  else
    return 0;

  sd->fd = -1;
  sd->open = vox_open;
  sd->close = vox_close;
  sd->configure = vox_configure;
  sd->choose_format = vox_choose_format;
  sd->write = vox_write;
  sd->period_size = NULL;

  return 1;
}

/* Write NBYTES bytes from BUFFER to device SD.  */

static void
vox_write (struct sound_device *sd, const char *buffer, EMACS_INT nbytes)
{
  if (emacs_write (sd->fd, buffer, nbytes) != nbytes)
    sound_perror ("Error writing to sound device");
}

#ifdef HAVE_ALSA
/***********************************************************************
		       ALSA Driver Interface
 ***********************************************************************/

/* This driver is available on GNU/Linux. */

static void
alsa_sound_perror (const char *msg, int err)
{
  error ("%s: %s", msg, snd_strerror (err));
}

struct alsa_params
{
  snd_pcm_t *handle;
  snd_pcm_hw_params_t *hwparams;
  snd_pcm_sw_params_t *swparams;
  snd_pcm_uframes_t period_size;
};

/* Open device SD.  If SD->file is non-null, open that device,
   otherwise use a default device name.  */

static void
alsa_open (struct sound_device *sd)
{
  const char *file;
  struct alsa_params *p;
  int err;

  /* Open the sound device.  Default is "default".  */
  if (sd->file)
    file = sd->file;
  else
    file = DEFAULT_ALSA_SOUND_DEVICE;

  p = xmalloc (sizeof (*p));
  p->handle = NULL;
  p->hwparams = NULL;
  p->swparams = NULL;

  sd->fd = -1;
  sd->data = p;


  err = snd_pcm_open (&p->handle, file, SND_PCM_STREAM_PLAYBACK, 0);
  if (err < 0)
    alsa_sound_perror (file, err);
}

static EMACS_INT
alsa_period_size (struct sound_device *sd)
{
  struct alsa_params *p = (struct alsa_params *) sd->data;
  int fact = snd_pcm_format_size (sd->format, 1) * sd->channels;
  return p->period_size * (fact > 0 ? fact : 1);
}

static void
alsa_configure (struct sound_device *sd)
{
  int val, err, dir;
  unsigned uval;
  struct alsa_params *p = (struct alsa_params *) sd->data;
  snd_pcm_uframes_t buffer_size;

  xassert (p->handle != 0);

  err = snd_pcm_hw_params_malloc (&p->hwparams);
  if (err < 0)
    alsa_sound_perror ("Could not allocate hardware parameter structure", err);

  err = snd_pcm_sw_params_malloc (&p->swparams);
  if (err < 0)
    alsa_sound_perror ("Could not allocate software parameter structure", err);

  err = snd_pcm_hw_params_any (p->handle, p->hwparams);
  if (err < 0)
    alsa_sound_perror ("Could not initialize hardware parameter structure", err);

  err = snd_pcm_hw_params_set_access (p->handle, p->hwparams,
                                      SND_PCM_ACCESS_RW_INTERLEAVED);
  if (err < 0)
    alsa_sound_perror ("Could not set access type", err);

  val = sd->format;
  err = snd_pcm_hw_params_set_format (p->handle, p->hwparams, val);
  if (err < 0)
    alsa_sound_perror ("Could not set sound format", err);

  uval = sd->sample_rate;
  err = snd_pcm_hw_params_set_rate_near (p->handle, p->hwparams, &uval, 0);
  if (err < 0)
    alsa_sound_perror ("Could not set sample rate", err);

  val = sd->channels;
  err = snd_pcm_hw_params_set_channels (p->handle, p->hwparams, val);
  if (err < 0)
    alsa_sound_perror ("Could not set channel count", err);

  err = snd_pcm_hw_params (p->handle, p->hwparams);
  if (err < 0)
    alsa_sound_perror ("Could not set parameters", err);


  err = snd_pcm_hw_params_get_period_size (p->hwparams, &p->period_size, &dir);
  if (err < 0)
    alsa_sound_perror ("Unable to get period size for playback", err);

  err = snd_pcm_hw_params_get_buffer_size (p->hwparams, &buffer_size);
  if (err < 0)
    alsa_sound_perror ("Unable to get buffer size for playback", err);

  err = snd_pcm_sw_params_current (p->handle, p->swparams);
  if (err < 0)
    alsa_sound_perror ("Unable to determine current swparams for playback",
                       err);

  /* Start the transfer when the buffer is almost full */
  err = snd_pcm_sw_params_set_start_threshold (p->handle, p->swparams,
                                               (buffer_size / p->period_size)
                                               * p->period_size);
  if (err < 0)
    alsa_sound_perror ("Unable to set start threshold mode for playback", err);

  /* Allow the transfer when at least period_size samples can be processed */
  err = snd_pcm_sw_params_set_avail_min (p->handle, p->swparams, p->period_size);
  if (err < 0)
    alsa_sound_perror ("Unable to set avail min for playback", err);

  err = snd_pcm_sw_params (p->handle, p->swparams);
  if (err < 0)
    alsa_sound_perror ("Unable to set sw params for playback\n", err);

  snd_pcm_hw_params_free (p->hwparams);
  p->hwparams = NULL;
  snd_pcm_sw_params_free (p->swparams);
  p->swparams = NULL;

  err = snd_pcm_prepare (p->handle);
  if (err < 0)
    alsa_sound_perror ("Could not prepare audio interface for use", err);

  if (sd->volume > 0)
    {
      int chn;
      snd_mixer_t *handle;
      snd_mixer_elem_t *e;
      const char *file = sd->file ? sd->file : DEFAULT_ALSA_SOUND_DEVICE;

      if (snd_mixer_open (&handle, 0) >= 0)
        {
          if (snd_mixer_attach (handle, file) >= 0
              && snd_mixer_load (handle) >= 0
              && snd_mixer_selem_register (handle, NULL, NULL) >= 0)
            for (e = snd_mixer_first_elem (handle);
                 e;
                 e = snd_mixer_elem_next (e))
              {
                if (snd_mixer_selem_has_playback_volume (e))
                  {
                    long pmin, pmax, vol;
                    snd_mixer_selem_get_playback_volume_range (e, &pmin, &pmax);
                    vol = pmin + (sd->volume * (pmax - pmin)) / 100;

                    for (chn = 0; chn <= SND_MIXER_SCHN_LAST; chn++)
                      snd_mixer_selem_set_playback_volume (e, chn, vol);
                  }
              }
          snd_mixer_close (handle);
        }
    }
}


/* Close device SD if it is open.  */

static void
alsa_close (struct sound_device *sd)
{
  struct alsa_params *p = (struct alsa_params *) sd->data;
  if (p)
    {
      if (p->hwparams)
        snd_pcm_hw_params_free (p->hwparams);
      if (p->swparams)
        snd_pcm_sw_params_free (p->swparams);
      if (p->handle)
        {
          snd_pcm_drain (p->handle);
          snd_pcm_close (p->handle);
        }
      xfree (p);
    }
}

/* Choose device-dependent format for device SD from sound file S.  */

static void
alsa_choose_format (struct sound_device *sd, struct sound *s)
{
  if (s->type == RIFF)
    {
      struct wav_header *h = (struct wav_header *) s->header;
      if (h->precision == 8)
	sd->format = SND_PCM_FORMAT_U8;
      else if (h->precision == 16)
          sd->format = SND_PCM_FORMAT_S16_LE;
      else
	error ("Unsupported WAV file format");
    }
  else if (s->type == SUN_AUDIO)
    {
      struct au_header *header = (struct au_header *) s->header;
      switch (header->encoding)
	{
	case AU_ENCODING_ULAW_8:
	  sd->format = SND_PCM_FORMAT_MU_LAW;
          break;
	case AU_ENCODING_ALAW_8:
	  sd->format = SND_PCM_FORMAT_A_LAW;
          break;
	case AU_ENCODING_IEEE32:
          sd->format = SND_PCM_FORMAT_FLOAT_BE;
          break;
	case AU_ENCODING_IEEE64:
	  sd->format = SND_PCM_FORMAT_FLOAT64_BE;
	  break;
	case AU_ENCODING_8:
	  sd->format = SND_PCM_FORMAT_S8;
	  break;
	case AU_ENCODING_16:
	  sd->format = SND_PCM_FORMAT_S16_BE;
	  break;
	case AU_ENCODING_24:
	  sd->format = SND_PCM_FORMAT_S24_BE;
	  break;
	case AU_ENCODING_32:
	  sd->format = SND_PCM_FORMAT_S32_BE;
	  break;

	default:
	  error ("Unsupported AU file format");
	}
    }
  else
    abort ();
}


/* Write NBYTES bytes from BUFFER to device SD.  */

static void
alsa_write (struct sound_device *sd, const char *buffer, EMACS_INT nbytes)
{
  struct alsa_params *p = (struct alsa_params *) sd->data;

  /* The the third parameter to snd_pcm_writei is frames, not bytes. */
  int fact = snd_pcm_format_size (sd->format, 1) * sd->channels;
  EMACS_INT nwritten = 0;
  int err;

  while (nwritten < nbytes)
    {
      snd_pcm_uframes_t frames = (nbytes - nwritten)/fact;
      if (frames == 0) break;

      err = snd_pcm_writei (p->handle, buffer + nwritten, frames);
      if (err < 0)
        {
          if (err == -EPIPE)
            {	/* under-run */
              err = snd_pcm_prepare (p->handle);
              if (err < 0)
                alsa_sound_perror ("Can't recover from underrun, prepare failed",
                                   err);
            }
          else if (err == -ESTRPIPE)
            {
              while ((err = snd_pcm_resume (p->handle)) == -EAGAIN)
                sleep (1);	/* wait until the suspend flag is released */
              if (err < 0)
                {
                  err = snd_pcm_prepare (p->handle);
                  if (err < 0)
                    alsa_sound_perror ("Can't recover from suspend, "
                                       "prepare failed",
                                       err);
                }
            }
          else
            alsa_sound_perror ("Error writing to sound device", err);

        }
      else
        nwritten += err * fact;
    }
}

static void
snd_error_quiet (const char *file, int line, const char *function, int err,
		 const char *fmt)
{
}

/* Initialize device SD.  Set up the interface functions in the device
   structure.  */

static int
alsa_init (struct sound_device *sd)
{
  const char *file;
  snd_pcm_t *handle;
  int err;

  /* Open the sound device.  Default is "default".  */
  if (sd->file)
    file = sd->file;
  else
    file = DEFAULT_ALSA_SOUND_DEVICE;

  snd_lib_error_set_handler ((snd_lib_error_handler_t) snd_error_quiet);
  err = snd_pcm_open (&handle, file, SND_PCM_STREAM_PLAYBACK, 0);
  snd_lib_error_set_handler (NULL);
  if (err < 0)
      return 0;
  snd_pcm_close (handle);

  sd->fd = -1;
  sd->open = alsa_open;
  sd->close = alsa_close;
  sd->configure = alsa_configure;
  sd->choose_format = alsa_choose_format;
  sd->write = alsa_write;
  sd->period_size = alsa_period_size;

  return 1;
}

#endif /* HAVE_ALSA */


/* END: Non Windows functions */
#else /* WINDOWSNT */

/* BEGIN: Windows specific functions */

#define SOUND_WARNING(fun, error, text)            \
  {                                                \
    char buf[1024];                                \
    char err_string[MAXERRORLENGTH];               \
    fun (error, err_string, sizeof (err_string));  \
    _snprintf (buf, sizeof (buf), "%s\nError: %s", \
	       text, err_string);		   \
    sound_warning (buf);                           \
  }

static int
do_play_sound (const char *psz_file, unsigned long ui_volume)
{
  int i_result = 0;
  MCIERROR mci_error = 0;
  char sz_cmd_buf[520] = {0};
  char sz_ret_buf[520] = {0};
  MMRESULT mm_result = MMSYSERR_NOERROR;
  unsigned long ui_volume_org = 0;
  BOOL b_reset_volume = FALSE;

  memset (sz_cmd_buf, 0, sizeof (sz_cmd_buf));
  memset (sz_ret_buf, 0, sizeof (sz_ret_buf));
  sprintf (sz_cmd_buf,
           "open \"%s\" alias GNUEmacs_PlaySound_Device wait",
           psz_file);
  mci_error = mciSendString (sz_cmd_buf, sz_ret_buf, sizeof (sz_ret_buf), NULL);
  if (mci_error != 0)
    {
      SOUND_WARNING (mciGetErrorString, mci_error,
		     "The open mciSendString command failed to open "
		     "the specified sound file.");
      i_result = (int) mci_error;
      return i_result;
    }
  if ((ui_volume > 0) && (ui_volume != UINT_MAX))
    {
      mm_result = waveOutGetVolume ((HWAVEOUT) WAVE_MAPPER, &ui_volume_org);
      if (mm_result == MMSYSERR_NOERROR)
        {
          b_reset_volume = TRUE;
          mm_result = waveOutSetVolume ((HWAVEOUT) WAVE_MAPPER, ui_volume);
          if (mm_result != MMSYSERR_NOERROR)
            {
	      SOUND_WARNING (waveOutGetErrorText, mm_result,
			     "waveOutSetVolume failed to set the volume level "
			     "of the WAVE_MAPPER device.\n"
			     "As a result, the user selected volume level will "
			     "not be used.");
            }
        }
      else
        {
          SOUND_WARNING (waveOutGetErrorText, mm_result,
			 "waveOutGetVolume failed to obtain the original "
                         "volume level of the WAVE_MAPPER device.\n"
                         "As a result, the user selected volume level will "
                         "not be used.");
        }
    }
  memset (sz_cmd_buf, 0, sizeof (sz_cmd_buf));
  memset (sz_ret_buf, 0, sizeof (sz_ret_buf));
  strcpy (sz_cmd_buf, "play GNUEmacs_PlaySound_Device wait");
  mci_error = mciSendString (sz_cmd_buf, sz_ret_buf, sizeof (sz_ret_buf), NULL);
  if (mci_error != 0)
    {
      SOUND_WARNING (mciGetErrorString, mci_error,
		     "The play mciSendString command failed to play the "
		     "opened sound file.");
      i_result = (int) mci_error;
    }
  memset (sz_cmd_buf, 0, sizeof (sz_cmd_buf));
  memset (sz_ret_buf, 0, sizeof (sz_ret_buf));
  strcpy (sz_cmd_buf, "close GNUEmacs_PlaySound_Device wait");
  mci_error = mciSendString (sz_cmd_buf, sz_ret_buf, sizeof (sz_ret_buf), NULL);
  if (b_reset_volume == TRUE)
    {
      mm_result = waveOutSetVolume ((HWAVEOUT) WAVE_MAPPER, ui_volume_org);
      if (mm_result != MMSYSERR_NOERROR)
        {
          SOUND_WARNING (waveOutGetErrorText, mm_result,
			 "waveOutSetVolume failed to reset the original volume "
                         "level of the WAVE_MAPPER device.");
        }
    }
  return i_result;
}

/* END: Windows specific functions */

#endif /* WINDOWSNT */

DEFUN ("play-sound-internal", Fplay_sound_internal, Splay_sound_internal, 1, 1, 0,
       doc: /* Play sound SOUND.

Internal use only, use `play-sound' instead.  */)
  (Lisp_Object sound)
{
  Lisp_Object attrs[SOUND_ATTR_SENTINEL];
  int count = SPECPDL_INDEX ();

#ifndef WINDOWSNT
  Lisp_Object file;
  struct gcpro gcpro1, gcpro2;
  Lisp_Object args[2];
#else /* WINDOWSNT */
  int len = 0;
  Lisp_Object lo_file = {0};
  char * psz_file = NULL;
  unsigned long ui_volume_tmp = UINT_MAX;
  unsigned long ui_volume = UINT_MAX;
  int i_result = 0;
#endif /* WINDOWSNT */

  /* Parse the sound specification.  Give up if it is invalid.  */
  if (!parse_sound (sound, attrs))
    error ("Invalid sound specification");

#ifndef WINDOWSNT
  file = Qnil;
  GCPRO2 (sound, file);
  current_sound_device = (struct sound_device *) xmalloc (sizeof (struct sound_device));
  memset (current_sound_device, 0, sizeof (struct sound_device));
  current_sound = (struct sound *) xmalloc (sizeof (struct sound));
  memset (current_sound, 0, sizeof (struct sound));
  record_unwind_protect (sound_cleanup, Qnil);
  current_sound->header = (char *) alloca (MAX_SOUND_HEADER_BYTES);

  if (STRINGP (attrs[SOUND_FILE]))
    {
      /* Open the sound file.  */
      current_sound->fd = openp (Fcons (Vdata_directory, Qnil),
				 attrs[SOUND_FILE], Qnil, &file, Qnil);
      if (current_sound->fd < 0)
	sound_perror ("Could not open sound file");

      /* Read the first bytes from the file.  */
      current_sound->header_size
	= emacs_read (current_sound->fd, current_sound->header,
		      MAX_SOUND_HEADER_BYTES);
      if (current_sound->header_size < 0)
	sound_perror ("Invalid sound file header");
    }
  else
    {
      current_sound->data = attrs[SOUND_DATA];
      current_sound->header_size = min (MAX_SOUND_HEADER_BYTES, SBYTES (current_sound->data));
      memcpy (current_sound->header, SDATA (current_sound->data),
	      current_sound->header_size);
    }

  /* Find out the type of sound.  Give up if we can't tell.  */
  find_sound_type (current_sound);

  /* Set up a device.  */
  if (STRINGP (attrs[SOUND_DEVICE]))
    {
      int len = SCHARS (attrs[SOUND_DEVICE]);
      current_sound_device->file = (char *) alloca (len + 1);
      strcpy (current_sound_device->file, SSDATA (attrs[SOUND_DEVICE]));
    }

  if (INTEGERP (attrs[SOUND_VOLUME]))
    current_sound_device->volume = XFASTINT (attrs[SOUND_VOLUME]);
  else if (FLOATP (attrs[SOUND_VOLUME]))
    current_sound_device->volume = XFLOAT_DATA (attrs[SOUND_VOLUME]) * 100;

  args[0] = Qplay_sound_functions;
  args[1] = sound;
  Frun_hook_with_args (2, args);

#ifdef HAVE_ALSA
  if (!alsa_init (current_sound_device))
#endif
    if (!vox_init (current_sound_device))
      error ("No usable sound device driver found");

  /* Open the device.  */
  current_sound_device->open (current_sound_device);

  /* Play the sound.  */
  current_sound->play (current_sound, current_sound_device);

  /* Clean up.  */
  UNGCPRO;

#else /* WINDOWSNT */

  lo_file = Fexpand_file_name (attrs[SOUND_FILE], Qnil);
  len = XSTRING (lo_file)->size;
  psz_file = (char *) alloca (len + 1);
  strcpy (psz_file, XSTRING (lo_file)->data);
  if (INTEGERP (attrs[SOUND_VOLUME]))
    {
      ui_volume_tmp = XFASTINT (attrs[SOUND_VOLUME]);
    }
  else if (FLOATP (attrs[SOUND_VOLUME]))
    {
      ui_volume_tmp = XFLOAT_DATA (attrs[SOUND_VOLUME]) * 100;
    }
  /*
    Based on some experiments I have conducted, a value of 100 or less
    for the sound volume is much too low.  You cannot even hear it.
    A value of UINT_MAX indicates that you wish for the sound to played
    at the maximum possible volume.  A value of UINT_MAX/2 plays the
    sound at 50% maximum volume.  Therefore the value passed to do_play_sound
    (and thus to waveOutSetVolume) must be some fraction of UINT_MAX.
    The following code adjusts the user specified volume level appropriately.
  */
  if ((ui_volume_tmp > 0) && (ui_volume_tmp <= 100))
    {
      ui_volume = ui_volume_tmp * (UINT_MAX / 100);
    }
  i_result = do_play_sound (psz_file, ui_volume);

#endif /* WINDOWSNT */

  unbind_to (count, Qnil);
  return Qnil;
}

/***********************************************************************
			    Initialization
 ***********************************************************************/

void
syms_of_sound (void)
{
  DEFSYM (QCdevice, ":device");
  DEFSYM (QCvolume, ":volume");
  DEFSYM (Qsound, "sound");
  DEFSYM (Qplay_sound_functions, "play-sound-functions");

  defsubr (&Splay_sound_internal);
}


void
init_sound (void)
{
}

#endif /* HAVE_SOUND */
