/* Audio hardware handlers (OSS, ALSA, Sun, Windows, Mac OSX, Jack, HPUX, NetBSD, OpenBSD, pulseaudio, portaudio) 
 *
 * In many cases, only callback driven transfers are supported, so ideally we'd have:
 * int mus_audio_playback(caller_data, start_func, fill_func, end_func)
 *   returns error indication or MUS_NO_ERROR
 *   calls start_func at startup: void start(caller_data, ...)?
 *   each times it needs a bufferfull, calls fill_func: bool fill(caller_data, void *buf, buf_size_in_samples, buf_data_type)
 *     perhaps returns false to signal normal quit?
 *   at end (either via fill or some interrupt), calls end(caller_data, ...)?
 */

/*
 * layout of this file:
 *    error handlers
 *    OSS
 *    ALSA
 *    Sun
 *    Windows 95/98
 *    OSX
 *    JACK
 *    HPUX
 *    NetBSD/OpenBSD
 *    PulseAudio (in progress?)
 *    PortAudio
 */

/*
 * int mus_audio_open_output(int dev, int srate, int chans, int format, int size)
 * int mus_audio_open_input(int dev, int srate, int chans, int format, int size)
 * int mus_audio_write(int line, char *buf, int bytes)
 * int mus_audio_close(int line)
 * int mus_audio_read(int line, char *buf, int bytes)
 * int mus_audio_initialize(void) does whatever is needed to get set up
 * char *mus_audio_moniker(void) returns some brief description of the overall audio setup (don't free return string).
 */

#include <mus-config.h>

#if USE_SND && __APPLE__ && USE_MOTIF
  #undef USE_MOTIF
  #define USE_NO_GUI 1
  /* Xt's Boolean (/usr/include/X11/Intrinsic.h = char) collides with MacTypes.h Boolean, (actually,
   *   unsigned char in /Developer/SDKs/MacOSX10.4u.sdk/System/Library/Frameworks/CoreFoundation.framework/Versions/A/Headers/CFBase.h)
   *   but we want snd.h for other stuff, so, if Motif is in use, don't load its headers at this time
   *   perhaps we could use the -funsigned-char switch in gcc
   */
#endif

#if USE_SND && __APPLE__ && HAVE_RUBY
  /* if using Ruby, OpenTransport.h T_* definitions collide with Ruby's -- it isn't needed here, so... */
  #define REDEFINE_HAVE_RUBY 1
  #undef HAVE_RUBY
#endif

#if USE_SND
  #include "snd.h"
#else
  #define PRINT_BUFFER_SIZE 512
  #define LABEL_BUFFER_SIZE 64
#endif

#if USE_SND && __APPLE__
  #define USE_MOTIF 1
  #undef USE_NO_GUI
  #if REDEFINE_HAVE_RUBY
    #define HAVE_RUBY 1
  #endif
#endif

#include <math.h>
#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <stdlib.h>
#ifndef _MSC_VER
  #include <unistd.h>
#endif
#include <string.h>

#ifdef __APPLE__
#include <CoreServices/CoreServices.h>
#include <CoreAudio/CoreAudio.h>
/* these pull in stdbool.h apparently, so they have to precede sndlib.h */
#endif

#define HAVE_JACK_IN_LINUX (MUS_JACK && __linux__)

#include "_sndlib.h"
#include "sndlib-strings.h"

#if WITH_AUDIO

enum {MUS_AUDIO_DEFAULT_0, MUS_AUDIO_DUPLEX_DEFAULT, MUS_AUDIO_LINE_OUT,
      MUS_AUDIO_LINE_IN, MUS_AUDIO_MICROPHONE, MUS_AUDIO_SPEAKERS, MUS_AUDIO_DIGITAL_OUT,
      MUS_AUDIO_DAC_OUT, MUS_AUDIO_MIXER, MUS_AUDIO_AUX_OUTPUT
};


#define MUS_STANDARD_ERROR(Error_Type, Error_Message) \
  mus_print("%s\n  [%s[%d] %s]", Error_Message, __FILE__, __LINE__, __func__)

#define MUS_STANDARD_IO_ERROR(Error_Type, IO_Func, IO_Name) \
  mus_print("%s %s: %s\n  [%s[%d] %s]", IO_Func, IO_Name, strerror(errno), __FILE__, __LINE__, __func__)


static char *version_name = NULL;
static bool audio_initialized = false;




/* ------------------------------- OSS ----------------------------------------- */

/* Thanks to Yair K. for OSS v4 changes.  22-Jan-08 */

#if (HAVE_OSS || HAVE_ALSA || HAVE_JACK_IN_LINUX)
/* actually it's not impossible that someday we'll have ALSA but not OSS... */
#define AUDIO_OK 1

#include <sys/ioctl.h>
#include <sys/soundcard.h>

#if ((SOUND_VERSION > 360) && (defined(OSS_SYSINFO)))
  #define NEW_OSS 1
#endif

#define MUS_OSS_WRITE_RATE     SNDCTL_DSP_SPEED
#define MUS_OSS_WRITE_CHANNELS SNDCTL_DSP_CHANNELS
#define MUS_OSS_SET_FORMAT     SNDCTL_DSP_SETFMT
#define MUS_OSS_GET_FORMATS    SNDCTL_DSP_GETFMTS

#define DAC_NAME "/dev/dsp"
#define MIXER_NAME "/dev/mixer"
/* some programs use /dev/audio */

/* there can be more than one sound card installed, and a card can be handled through
 * more than one /dev/dsp device, so we can't use a global dac device here.
 * The caller has to keep track of the various cards (via AUDIO_SYSTEM) --
 * I toyed with embedding all that in mus_audio_open_output and mus_audio_write, but
 * decided it's better to keep them explicit -- the caller may want entirely
 * different (non-synchronous) streams going to the various cards.  This same
 * code (AUDIO_SYSTEM(n)->devn) should work in Windoze (see below), and
 * might work on the Mac -- something for a rainy day...
 */

#define RETURN_ERROR_EXIT(Message_Type, Audio_Line, Ur_Message) \
  do { \
       char *Message; Message = Ur_Message; \
       if (Audio_Line != -1) \
          linux_audio_close(Audio_Line); \
       if ((Message) && (strlen(Message) > 0)) \
         { \
           mus_print("%s\n  [%s[%d] %s]", \
                     Message, \
                     __FILE__, __LINE__, __func__); \
           free(Message); \
         } \
       else mus_print("%s\n  [%s[%d] %s]", \
                      mus_error_type_to_string(Message_Type), \
                      __FILE__, __LINE__, __func__); \
       return(MUS_ERROR); \
     } while (false)

static int FRAGMENTS = 4;
static int FRAGMENT_SIZE = 12;
static bool fragments_locked = false;

/* defaults here are FRAGMENTS 16 and FRAGMENT_SIZE 12; these values however
 * cause about a .5 second delay, which is not acceptable in "real-time" situations.
 *
 * this changed 22-May-01: these are causing more trouble than they're worth
 */

static void oss_mus_oss_set_buffers(int num, int size) {FRAGMENTS = num; FRAGMENT_SIZE = size; fragments_locked = true;}

#define MAX_SOUNDCARDS 8
#define MAX_DSPS 8
#define MAX_MIXERS 8
/* there can be (apparently) any number of mixers and dsps per soundcard, but 8 is enough! */

static int *audio_fd = NULL; 
static int *audio_open_ctr = NULL; 
static int *audio_dsp = NULL; 
static int *audio_mixer = NULL; 
static int *audio_mode = NULL; 

static int sound_cards = 0;
#ifdef NEW_OSS
  static int new_oss_running = 0;
#endif
static char *dev_name = NULL;

static char *oss_mus_audio_moniker(void)
{
  char version[LABEL_BUFFER_SIZE];
  if (version_name == NULL) version_name = (char *)calloc(LABEL_BUFFER_SIZE, sizeof(char));
  if (SOUND_VERSION < 361)
    {
      snprintf(version, LABEL_BUFFER_SIZE, "%d", SOUND_VERSION);
      snprintf(version_name, LABEL_BUFFER_SIZE, "OSS %c.%c.%c", version[0], version[1], version[2]);
    }
  else
    snprintf(version_name, LABEL_BUFFER_SIZE, "OSS %x.%x.%x", 
         (SOUND_VERSION >> 16) & 0xff, 
         (SOUND_VERSION >> 8) & 0xff, 
         SOUND_VERSION & 0xff);
  return(version_name);
}

static char *dac_name(int sys, int offset)
{
  if ((sys < sound_cards) && (audio_mixer[sys] >= -1))
    {
      snprintf(dev_name, LABEL_BUFFER_SIZE, "%s%d", DAC_NAME, audio_dsp[sys] + offset);
      return(dev_name);
    }
  return((char *)DAC_NAME);
}

#define MIXER_SIZE SOUND_MIXER_NRDEVICES
static int **mixer_state = NULL;
static int *init_srate = NULL, *init_chans = NULL, *init_format = NULL;

static int oss_mus_audio_initialize(void) 
{
  /* here we need to set up the map of /dev/dsp and /dev/mixer to a given system */
  /* since this info is not passed to us by OSS, we have to work at it... */
  /* for the time being, I'll ignore auxiliary dsp and mixer ports (each is a special case) */
  int i, fd = -1, md, err = 0;
  char dname[LABEL_BUFFER_SIZE];
  int amp, old_mixer_amp, old_dsp_amp, new_mixer_amp, responsive_field;
  int devmask;
#ifdef NEW_OSS
  int status, ignored;
  oss_sysinfo sysinfo;
  static mixer_info mixinfo;
  int sysinfo_ok = 0;
#endif
  int num_mixers, num_dsps, nmix, ndsp;
  if (!audio_initialized)
    {
      audio_initialized = true;
      audio_fd = (int *)calloc(MAX_SOUNDCARDS, sizeof(int));
      audio_open_ctr = (int *)calloc(MAX_SOUNDCARDS, sizeof(int));
      audio_dsp = (int *)calloc(MAX_SOUNDCARDS, sizeof(int));
      audio_mixer = (int *)calloc(MAX_SOUNDCARDS, sizeof(int));
      audio_mode = (int *)calloc(MAX_SOUNDCARDS, sizeof(int));
      dev_name = (char *)calloc(LABEL_BUFFER_SIZE, sizeof(char));
      init_srate = (int *)calloc(MAX_SOUNDCARDS, sizeof(int));
      init_chans = (int *)calloc(MAX_SOUNDCARDS, sizeof(int));
      init_format = (int *)calloc(MAX_SOUNDCARDS, sizeof(int));
      mixer_state = (int **)calloc(MAX_SOUNDCARDS, sizeof(int *));
      for (i = 0; i < MAX_SOUNDCARDS; i++) mixer_state[i] = (int *)calloc(MIXER_SIZE, sizeof(int));
      for (i = 0; i < MAX_SOUNDCARDS; i++)
    {
      audio_fd[i] = -1;
      audio_open_ctr[i] = 0;
      audio_dsp[i] = -1;
      audio_mixer[i] = -1;
    }

      num_mixers = MAX_MIXERS;
      num_dsps = MAX_DSPS;
#ifdef NEW_OSS
      fd = open(DAC_NAME, O_WRONLY | O_NONBLOCK, 0);
      if (fd == -1) fd = open(MIXER_NAME, O_RDONLY | O_NONBLOCK, 0);
      if (fd != -1)
    {
      status = ioctl(fd, OSS_GETVERSION, &ignored);
      new_oss_running = (status == 0);
      if (new_oss_running)
        {
          status = ioctl(fd, OSS_SYSINFO, &sysinfo);
          sysinfo_ok = (status == 0);
        }
      if ((new_oss_running) && (sysinfo_ok))
        {
          num_mixers = sysinfo.nummixers;
          num_dsps = sysinfo.numaudios;
        }
      close(fd);
    }
#endif

      /* need to get which /dev/dsp lines match which /dev/mixer lines,
       *   find out how many separate systems (soundcards) are available,
       *   fill the audio_dsp and audio_mixer arrays with the system-related numbers,
       * since we have no way to tell from OSS info which mixers/dsps are the
       *   main ones, we'll do some messing aound to try to deduce this info.
       * for example, SB uses two dsp ports and two mixers per card, whereas
       *  Ensoniq uses 2 dsps and 1 mixer.
       * 
       * the data we are gathering here:
       *   int audio_dsp[MAX_SOUNDCARDS] -> main_dsp_port[n] (-1 => no such system dsp)
       *   int audio_mixer[MAX_SOUNDCARDS] -> main_mixer_port[n]
       *   int sound_cards = 0 -> usable systems
       * all auxiliary ports are currently ignored (SB equalizer, etc)
       */
      sound_cards = 0;
      ndsp = 0;
      nmix = 0;
      while ((nmix < num_mixers) && 
         (ndsp < num_dsps))
    {
      /* for each mixer, find associated main dsp (assumed to be first in /dev/dsp ordering) */
      /*   if mixer's dsp overlaps or we run out of dsps first, ignore it (aux mixer) */
      /* our by-guess-or-by-gosh method here is to try to open the mixer.
       *   if that fails, quit (if very first, try at least to get the dsp setup)
       *   find volume field, if none, go on, else read current volume
       *   open next unchecked dsp, try to set volume, read current, if different we found a match -- set and go on.
       *     if no change, move to next dsp and try again, if no more dsps, quit (checking for null case as before)
       */
      snprintf(dname, LABEL_BUFFER_SIZE, "%s%d", MIXER_NAME, nmix);
      md = open(dname, O_RDWR, 0);
      if (md == -1)
        {
          if (errno == EBUSY) 
        {
          mus_print("%s is busy: can't access it [%s[%d] %s]", 
                dname,
                __FILE__, __LINE__, __func__); 
          nmix++;
          continue;
        }
          else break;
        }
      snprintf(dname, LABEL_BUFFER_SIZE, "%s%d", DAC_NAME, ndsp);
      fd = open(dname, O_RDWR | O_NONBLOCK, 0);
      if (fd == -1) fd = open(dname, O_RDONLY | O_NONBLOCK, 0);
      if (fd == -1) fd = open(dname, O_WRONLY | O_NONBLOCK, 0); /* some output devices need this */
      if (fd == -1)
        {
          close(md); 
          if (errno == EBUSY) /* in linux /usr/include/asm-generic/errno-base.h */
        {
          fprintf(stderr, "%s is busy: can't access it\n", dname); 
          ndsp++;
          continue;
        }
          else 
        {
          if ((errno != ENXIO) && (errno != ENODEV) && (errno != ENOENT))
            fprintf(stderr, "%s: %s! ", dname, strerror(errno));
          break;
        }
        }
#ifdef NEW_OSS				  
      status = ioctl(md, SOUND_MIXER_INFO, &mixinfo);
#endif
      err = ioctl(md, SOUND_MIXER_READ_DEVMASK, &devmask);
      responsive_field = SOUND_MIXER_VOLUME;
      for (i = 0; i < SOUND_MIXER_NRDEVICES; i++)
        if ((1 << i) & devmask)
          {
        responsive_field = i;
        break;
          }
      if (!err)
        {
          err = ioctl(md, MIXER_READ(responsive_field), &old_mixer_amp);
          if (!err)
        {
          err = ioctl(fd, MIXER_READ(responsive_field), &old_dsp_amp);
          if ((!err) && (old_dsp_amp == old_mixer_amp))
            {
              if (old_mixer_amp == 0) amp = 50; else amp = 0; /* 0..100 */
              err = ioctl(fd, MIXER_WRITE(responsive_field), &amp);
              if (!err)
            {
              err = ioctl(md, MIXER_READ(responsive_field), &new_mixer_amp);
              if (!err)
                {
                  if (new_mixer_amp == amp)
                {
                  /* found one! */
                  audio_dsp[sound_cards] = ndsp; ndsp++;
                  audio_mixer[sound_cards] = nmix; nmix++;
                  sound_cards++;
                }
                  else ndsp++;
                  err = ioctl(fd, MIXER_WRITE(responsive_field), &old_dsp_amp);
                }
              else nmix++;
            }
              else ndsp++;
            }
          else ndsp++;
        }
          else nmix++;
        }
      else nmix++;
      close(fd);
      close(md);
    }
      if (sound_cards == 0)
    {
      fd = open(DAC_NAME, O_WRONLY | O_NONBLOCK, 0);
      if (fd != -1)
        {
          sound_cards = 1;
          audio_dsp[0] = 0;
          audio_mixer[0] = -2; /* hmmm -- need a way to see /dev/dsp as lonely outpost */
          close(fd);
          fd = open(MIXER_NAME, O_RDONLY | O_NONBLOCK, 0);
          if (fd == -1)
        audio_mixer[0] = -3;
          else close(fd);
        }
    }
    }
  return(MUS_NO_ERROR);
}

int mus_audio_reinitialize(void)
{
  /* an experiment */
  audio_initialized = false;
  return(mus_audio_initialize());
}

static int linux_audio_open(const char *pathname, int flags, mode_t mode, int system)
{
  /* sometimes this is simply searching for a device (so failure is not a mus_error) */
  if (audio_fd[system] == -1) 
    {
      audio_fd[system] = open(pathname, flags, mode);
      audio_open_ctr[system] = 0;
    }
  else audio_open_ctr[system]++;
  return(audio_fd[system]);
}

static int linux_audio_open_with_error(const char *pathname, int flags, mode_t mode, int system)
{
  int fd;
  static bool already_warned = false;
  if ((system < 0) ||
      (system >= MAX_SOUNDCARDS))
    return(-1);

  fd = linux_audio_open(pathname, flags, mode, system);
  if ((fd == -1) &&
      (!already_warned))
    {
      already_warned = true;
      MUS_STANDARD_IO_ERROR(MUS_AUDIO_CANT_OPEN,
                ((mode == O_RDONLY) ? "open read" : 
                 (mode == O_WRONLY) ? "open write" : "open read/write"),
                pathname);
    }
  return(fd);
}

static int find_system(int line)
{
  int i;
  for (i = 0; i < sound_cards; i++)
    if (line == audio_fd[i])
      return(i);
  return(MUS_ERROR);
}

static int linux_audio_close(int fd)
{
  if (fd != -1)
    {
      int err = 0, sys;
      sys = find_system(fd);
      if (sys != -1)
    {
      if (audio_open_ctr[sys] > 0) 
        audio_open_ctr[sys]--;
      else 
        {
          err = close(fd);
          audio_open_ctr[sys] = 0;
          audio_fd[sys] = -1;
        }
    }
      else err = close(fd);
      if (err) RETURN_ERROR_EXIT(MUS_AUDIO_CANT_CLOSE, -1,
                 mus_format("close %d failed: %s",
                        fd, strerror(errno)));
    }
  /* is this an error? */
  return(MUS_NO_ERROR);
}

static int to_oss_format(int snd_format)
{
  switch (snd_format)
    {
    case MUS_BYTE:    return(AFMT_S8);     break;
    case MUS_BSHORT:  return(AFMT_S16_BE); break;
    case MUS_UBYTE:   return(AFMT_U8);     break;
    case MUS_MULAW:   return(AFMT_MU_LAW); break;
    case MUS_ALAW:    return(AFMT_A_LAW);  break;
    case MUS_LSHORT:  return(AFMT_S16_LE); break;
    case MUS_UBSHORT: return(AFMT_U16_BE); break;
    case MUS_ULSHORT: return(AFMT_U16_LE); break;
#ifdef NEW_OSS
    case MUS_LINT:    return(AFMT_S32_LE); break;
    case MUS_BINT:    return(AFMT_S32_BE); break;
#endif
    }
  return(MUS_ERROR);
}

static bool fragment_set_failed = false;

static int oss_mus_audio_open_output(int ur_dev, int srate, int chans, int format, int size)
{
  int oss_format, buffer_info, audio_out = -1, sys, dev;
  char *dev_name;
#ifndef NEW_OSS
  int stereo;
#endif
  sys = MUS_AUDIO_SYSTEM(ur_dev);
  dev = MUS_AUDIO_DEVICE(ur_dev);
  oss_format = to_oss_format(format); 
  if (oss_format == MUS_ERROR) 
    RETURN_ERROR_EXIT(MUS_AUDIO_FORMAT_NOT_AVAILABLE, -1,
              mus_format("format %d (%s) not available",
                 format, 
                 mus_data_format_name(format)));

  if (dev == MUS_AUDIO_DEFAULT)
    audio_out = linux_audio_open_with_error(dev_name = dac_name(sys, 0), 
                        O_WRONLY, 0, sys);
  else audio_out = linux_audio_open_with_error(dev_name = dac_name(sys, (dev == MUS_AUDIO_AUX_OUTPUT) ? 1 : 0), 
                           O_RDWR, 0, sys);
  if (audio_out == -1) return(MUS_ERROR);

  /* ioctl(audio_out, SNDCTL_DSP_RESET, 0); */ /* causes clicks */
  if ((fragments_locked) && 
      (!(fragment_set_failed)) &&
      ((dev == MUS_AUDIO_DUPLEX_DEFAULT) || 
       (size != 0))) /* only set if user has previously called set_oss_buffers */
    {
      buffer_info = (FRAGMENTS << 16) | (FRAGMENT_SIZE);
      if (ioctl(audio_out, SNDCTL_DSP_SETFRAGMENT, &buffer_info) == -1)
        {
          /* older Linuces (or OSS's?) refuse to handle the fragment reset if O_RDWR used --
           * someone at OSS forgot to update the version number when this was fixed, so
           * I have no way to get around this except to try and retry...
           */
          linux_audio_close(audio_out);
          audio_out = linux_audio_open_with_error(dev_name = dac_name(sys, (dev == MUS_AUDIO_AUX_OUTPUT) ? 1 : 0), 
                          O_WRONLY, 0, sys);
      if (audio_out == -1) return(MUS_ERROR);
          buffer_info = (FRAGMENTS << 16) | (FRAGMENT_SIZE);
          if (ioctl(audio_out, SNDCTL_DSP_SETFRAGMENT, &buffer_info) == -1) 
        {
          char *tmp;
          tmp = mus_format("can't set %s fragments to: %d x %d",
                   dev_name, FRAGMENTS, FRAGMENT_SIZE); /* not an error if ALSA OSS-emulation */
          fprintf(stderr, "%s\n", tmp);
          fragment_set_failed = true;
          free(tmp);
        }
        }
    }
  if ((ioctl(audio_out, MUS_OSS_SET_FORMAT, &oss_format) == -1) || 
      (oss_format != to_oss_format(format)))
    RETURN_ERROR_EXIT(MUS_AUDIO_FORMAT_NOT_AVAILABLE, audio_out,
              mus_format("data format %d (%s) not available on %s",
                 format, 
                 mus_data_format_name(format), 
                 dev_name));
#ifdef NEW_OSS
  if (ioctl(audio_out, MUS_OSS_WRITE_CHANNELS, &chans) == -1) 
    RETURN_ERROR_EXIT(MUS_AUDIO_CHANNELS_NOT_AVAILABLE, audio_out,
              mus_format("can't get %d channels on %s",
                 chans, dev_name));
#else
  if (chans == 2) stereo = 1; else stereo = 0;
  if ((ioctl(audio_out, SNDCTL_DSP_STEREO, &stereo) == -1) || 
      ((chans == 2) && (stereo == 0)))
    RETURN_ERROR_EXIT(MUS_AUDIO_CHANNELS_NOT_AVAILABLE, audio_out,
              mus_format("can't get %d channels on %s",
                 chans, dev_name));
#endif
  if (ioctl(audio_out, MUS_OSS_WRITE_RATE, &srate) == -1) 
    RETURN_ERROR_EXIT(MUS_AUDIO_SRATE_NOT_AVAILABLE, audio_out,
              mus_format("can't set srate of %s to %d",
                 dev_name, srate));
  /* http://www.4front-tech.com/pguide/audio.html says this order has to be followed */
  return(audio_out);
}

static int oss_mus_audio_write(int line, char *buf, int bytes)
{
  int err;
  if (line < 0) return(-1);
  errno = 0;
  err = write(line, buf, bytes);
  if (err != bytes)
    {
      if (errno != 0)
    RETURN_ERROR_EXIT(MUS_AUDIO_WRITE_ERROR, -1,
              mus_format("write error: %s", strerror(errno)));
      else RETURN_ERROR_EXIT(MUS_AUDIO_WRITE_ERROR, -1,
                 mus_format("wrote %d bytes of requested %d", err, bytes));
    }
  return(MUS_NO_ERROR);
}

static int oss_mus_audio_close(int line)
{
  return(linux_audio_close(line));
}

static int oss_mus_audio_read(int line, char *buf, int bytes)
{
  int err;
  if (line < 0) return(-1);
  errno = 0;
  err = read(line, buf, bytes);
  if (err != bytes) 
    {
      if (errno != 0)
    RETURN_ERROR_EXIT(MUS_AUDIO_READ_ERROR, -1,
              mus_format("read error: %s", strerror(errno)));
      else RETURN_ERROR_EXIT(MUS_AUDIO_READ_ERROR, -1,
                 mus_format("read %d bytes of requested %d", err, bytes));
    }
  return(MUS_NO_ERROR);
}

static char *oss_unsrc(int srcbit)
{
  if (srcbit == 0)
    return(mus_strdup("none"));
  else
    {
      bool need_and = false;
      char *buf;
      buf = (char *)calloc(PRINT_BUFFER_SIZE, sizeof(char));
      if (srcbit & SOUND_MASK_MIC) {need_and = true; strcat(buf, "mic");}
      if (srcbit & SOUND_MASK_LINE) {if (need_and) strcat(buf, " and "); need_and = true; strcat(buf, "line in");}
      if (srcbit & SOUND_MASK_CD) {if (need_and) strcat(buf, " and "); strcat(buf, "cd");}
      return(buf);
    }
}


static int oss_mus_audio_open_input(int ur_dev, int srate, int chans, int format, int requested_size)
{
  /* dev can be MUS_AUDIO_DEFAULT or MUS_AUDIO_DUPLEX_DEFAULT as well as the obvious others */
  int audio_fd = -1, oss_format, buffer_info, sys, dev, srcbit, cursrc, err;
  char *dev_name;
#ifndef NEW_OSS
  int stereo;
#endif
  sys = MUS_AUDIO_SYSTEM(ur_dev);
  dev = MUS_AUDIO_DEVICE(ur_dev);
  oss_format = to_oss_format(format);
  if (oss_format == MUS_ERROR)
    RETURN_ERROR_EXIT(MUS_AUDIO_FORMAT_NOT_AVAILABLE, -1,
              mus_format("format %d (%s) not available",
                 format, 
                 mus_data_format_name(format)));

  if (((dev == MUS_AUDIO_DEFAULT) || (dev == MUS_AUDIO_DUPLEX_DEFAULT)) && (sys == 0))
    audio_fd = linux_audio_open(dev_name = dac_name(sys, 0), 
                O_RDWR, 0, sys);
  else audio_fd = linux_audio_open(dev_name = dac_name(sys, 0), O_RDONLY, 0, sys);
  if (audio_fd == -1)
    {
      if (dev == MUS_AUDIO_DUPLEX_DEFAULT)
    RETURN_ERROR_EXIT(MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE, -1,
               mus_format("can't open %s: %s",
                  dev_name, strerror(errno)));
      if ((audio_fd = linux_audio_open(dev_name = dac_name(sys, 0), O_RDONLY, 0, sys)) == -1)
        {
          if ((errno == EACCES) || (errno == ENOENT))
        RETURN_ERROR_EXIT(MUS_AUDIO_NO_READ_PERMISSION, -1,
                  mus_format("can't open %s: %s\n  to get input in Linux, we need read permission on /dev/dsp",
                     dev_name, 
                     strerror(errno)));
          else RETURN_ERROR_EXIT(MUS_AUDIO_NO_INPUT_AVAILABLE, -1,
                 mus_format("can't open %s: %s",
                        dev_name, 
                        strerror(errno)));
        }
    }
#ifdef SNDCTL_DSP_SETDUPLEX
  else 
    ioctl(audio_fd, SNDCTL_DSP_SETDUPLEX, &err); /* not always a no-op! */
#endif
  /* need to make sure the desired recording source is active -- does this actually have any effect? */
  switch (dev)
    {
    case MUS_AUDIO_MICROPHONE: srcbit = SOUND_MASK_MIC;                   break;
    case MUS_AUDIO_LINE_IN:    srcbit = SOUND_MASK_LINE;                  break;
    case MUS_AUDIO_DUPLEX_DEFAULT: 
    case MUS_AUDIO_DEFAULT:    srcbit = SOUND_MASK_LINE | SOUND_MASK_MIC; break;
    default:                   srcbit = 0;                                break;

    }
  ioctl(audio_fd, MIXER_READ(SOUND_MIXER_RECSRC), &cursrc);
  srcbit = (srcbit | cursrc);
  ioctl(audio_fd, MIXER_WRITE(SOUND_MIXER_RECSRC), &srcbit);
  ioctl(audio_fd, MIXER_READ(SOUND_MIXER_RECSRC), &cursrc);
  if (cursrc != srcbit)
    {
      char *str1, *str2;
      str1 = oss_unsrc(srcbit);
      str2 = oss_unsrc(cursrc);
      mus_print("weird: tried to set recorder source to %s, but got %s?", str1, str2);
      free(str1);
      free(str2);
    }
  if ((fragments_locked) && (requested_size != 0))
    {
      buffer_info = (FRAGMENTS << 16) | (FRAGMENT_SIZE);
      ioctl(audio_fd, SNDCTL_DSP_SETFRAGMENT, &buffer_info);
    }
  if ((ioctl(audio_fd, MUS_OSS_SET_FORMAT, &oss_format) == -1) ||
      (oss_format != to_oss_format(format)))
    RETURN_ERROR_EXIT(MUS_AUDIO_FORMAT_NOT_AVAILABLE, audio_fd,
              mus_format("can't set %s format to %d (%s)",
                 dev_name, format, 
                 mus_data_format_name(format)));
#ifdef NEW_OSS
  if (ioctl(audio_fd, MUS_OSS_WRITE_CHANNELS, &chans) == -1) 
    RETURN_ERROR_EXIT(MUS_AUDIO_CHANNELS_NOT_AVAILABLE, audio_fd,
              mus_format("can't get %d channels on %s",
                 chans, dev_name));
#else
  if (chans == 2) stereo = 1; else stereo = 0;
  if ((ioctl(audio_fd, SNDCTL_DSP_STEREO, &stereo) == -1) || 
      ((chans == 2) && (stereo == 0))) 
    RETURN_ERROR_EXIT(MUS_AUDIO_CHANNELS_NOT_AVAILABLE, audio_fd,
              mus_format("can't get %d channels on %s",
                 chans, dev_name));
#endif
  if (ioctl(audio_fd, MUS_OSS_WRITE_RATE, &srate) == -1) 
    RETURN_ERROR_EXIT(MUS_AUDIO_SRATE_NOT_AVAILABLE, audio_fd,
              mus_format("can't set srate to %d on %s",
                 srate, dev_name));
  return(audio_fd);
}


#if (!HAVE_ALSA)
static int oss_formats(int ur_dev, int *val)
{
  int fd, formats = 0, sys, ind;

  sys = MUS_AUDIO_SYSTEM(ur_dev);
  /* dev = MUS_AUDIO_DEVICE(ur_dev); */

  fd = open(dac_name(sys, 0), O_WRONLY, 0);
  if (fd == -1) fd = open(DAC_NAME, O_WRONLY, 0);
  if (fd == -1) 
    {
      RETURN_ERROR_EXIT(MUS_AUDIO_CANT_OPEN, -1,
            mus_format("can't open %s: %s",
                   DAC_NAME, strerror(errno)));
      return(MUS_ERROR);
    }
  
  ioctl(fd, MUS_OSS_GET_FORMATS, &formats);
  ind = 1;
  if (formats & (to_oss_format(MUS_BSHORT)))  val[ind++] = MUS_BSHORT;
  if (formats & (to_oss_format(MUS_LSHORT)))  val[ind++] = MUS_LSHORT;
  if (formats & (to_oss_format(MUS_MULAW)))   val[ind++] = MUS_MULAW;
  if (formats & (to_oss_format(MUS_ALAW)))    val[ind++] = MUS_ALAW;
  if (formats & (to_oss_format(MUS_BYTE)))    val[ind++] = MUS_BYTE;
  if (formats & (to_oss_format(MUS_UBYTE)))   val[ind++] = MUS_UBYTE;
  if (formats & (to_oss_format(MUS_UBSHORT))) val[ind++] = MUS_UBSHORT;
  if (formats & (to_oss_format(MUS_ULSHORT))) val[ind++] = MUS_ULSHORT;
  val[0] = ind - 1;
  return(MUS_NO_ERROR);
}
#endif




/* ------------------------------- ALSA, OSS, Jack-in-Linux ----------------------------------- */

static int api = MUS_ALSA_API;
int mus_audio_api(void) {return(api);}

/* hopefully first call to sndlib will be this... */
static int probe_api(void);
static int (*vect_mus_audio_initialize)(void);

/* FIXME: add a suitable default for all other vectors
   so that a call happening before mus_audio_initialize
   can be detected */
/* I don't think this is necessary -- documentation discusses this
 * (mus_sound_initialize calls mus_audio_initialize)
 */

/* vectors for the rest of the sndlib api */
static void  (*vect_mus_oss_set_buffers)(int num, int size);
static char* (*vect_mus_audio_moniker)(void);
static int   (*vect_mus_audio_open_output)(int ur_dev, int srate, int chans, int format, int size);
static int   (*vect_mus_audio_open_input)(int ur_dev, int srate, int chans, int format, int requested_size);
static int   (*vect_mus_audio_write)(int id, char *buf, int bytes);
static int   (*vect_mus_audio_read)(int id, char *buf, int bytes);
static int   (*vect_mus_audio_close)(int id);

/* vectors for the rest of the sndlib api */
int mus_audio_initialize(void) 
{
  return(probe_api());
}

void mus_oss_set_buffers(int num, int size) 
{
  vect_mus_oss_set_buffers(num, size);
}

#if HAVE_ALSA 
static char* alsa_mus_audio_moniker(void);
#endif

char* mus_audio_moniker(void) 
{
#if (HAVE_OSS && HAVE_ALSA)
  char *both_names;
  both_names = (char *)calloc(PRINT_BUFFER_SIZE, sizeof(char));
  /* need to be careful here since these use the same constant buffer */
  strcpy(both_names, oss_mus_audio_moniker());
  strcat(both_names, ", ");
  strcat(both_names, alsa_mus_audio_moniker());
  return(both_names); /* tiny memory leak ... */
#else
  return(vect_mus_audio_moniker());
#endif
}

int mus_audio_open_output(int ur_dev, int srate, int chans, int format, int size) 
{
  return(vect_mus_audio_open_output(ur_dev, srate, chans, format, size));
}

int mus_audio_open_input(int ur_dev, int srate, int chans, int format, int requested_size) 
{
  return(vect_mus_audio_open_input(ur_dev, srate, chans, format, requested_size));
}

int mus_audio_write(int id, char *buf, int bytes) 
{
  return(vect_mus_audio_write(id, buf, bytes));
}

int mus_audio_read(int id, char *buf, int bytes) 
{
  return(vect_mus_audio_read(id, buf, bytes));
}

int mus_audio_close(int id) 
{
  return(vect_mus_audio_close(id));
}

#if HAVE_JACK_IN_LINUX
  static int jack_mus_audio_initialize(void);
#endif

#if (!HAVE_ALSA)
static int probe_api(void) 
{
#if HAVE_JACK_IN_LINUX
  {
    int jackprobe = jack_mus_audio_initialize();
    if (jackprobe == MUS_ERROR)
      {
#endif
  /* go for the oss api */
  api = MUS_OSS_API;
  vect_mus_audio_initialize = oss_mus_audio_initialize;
  vect_mus_oss_set_buffers = oss_mus_oss_set_buffers;
  vect_mus_audio_moniker = oss_mus_audio_moniker;
  vect_mus_audio_open_output = oss_mus_audio_open_output;
  vect_mus_audio_open_input = oss_mus_audio_open_input;
  vect_mus_audio_write = oss_mus_audio_write;
  vect_mus_audio_read = oss_mus_audio_read;
  vect_mus_audio_close = oss_mus_audio_close;
  return(vect_mus_audio_initialize());
#if HAVE_JACK_IN_LINUX
      }
    return(jackprobe);
  }
#endif
}
#endif

#endif


/* ------------------------------- ALSA ----------------------------------------- */
/*
 * Changed the names of the environment variables to use MUS, not SNDLIB.
 * reformatted and reorganized to be like the rest of the code
 * changed default device to "default"
 *    -- Bill 3-Feb-06
 *
 * error handling (mus_error) changed by Bill 14-Nov-02
 * 0.5 support removed by Bill 24-Mar-02
 *
 * changed for 0.9.x api by Fernando Lopez-Lezcano <nando@ccrma.stanford.edu>
 *
 *  sndlib "exports" only one soundcard with two directions (if they are available),
 *  and only deals with the alsa library pcm's. It does not scan for available
 *  cards and devices at the hardware level. Which device it uses can be defined by:
 *
 *  - setting variables in the environment (searched for in the following order):
 *    MUS_ALSA_PLAYBACK_DEVICE
 *       defines the name of the playback device
 *    MUS_ALSA_CAPTURE_DEVICE
 *       defines the name of the capture device
 *    MUS_ALSA_DEVICE
 *       defines the name of the playback and capture device
 *    use the first two if the playback and capture devices are different or the
 *    third if they are the same. 
 *  - if no variables are found in the environment sndlib tries to probe for a
 *    default device named "sndlib" (in alsa 0.9 devices are configured in 
 *    /usr/share/alsa/alsa.conf or in ~/.asoundrc)
 *  - if "sndlib" is not a valid device "hw:0,0" was used [but now it looks for "default"] (which by default should
 *    point to the first device of the first card
 *
 *  Some default settings are controllable through the environment as well:
 *    MUS_ALSA_BUFFER_SIZE = size of each buffer in frames
 *    MUS_ALSA_BUFFERS = number of buffers
 *
 * changed 18-Sep-00 by Bill: new error handling: old mus_audio_error folded into
 *  mus_error; mus_error itself should be used only for "real" errors -- things
 *  that can cause a throw (a kind of global jump elsewhere); use mus_print for informational
 *  stuff -- in Snd, mus_print will also save everything printed in the error dialog.
 *  In a few cases, I tried to fix the code to unwind before mus_error, and in others
 *  I've changed mus_error to mus_print, but some of these may be mistaken.
 *  Look for ?? below for areas where I'm not sure I rewrote code correctly.
 *
 * changed for 0.6.x api by Paul Barton-Davis, pbd@op.net
 *
 * changed for 0.5.x api by Fernando Lopez-Lezcano, nando@ccrma.stanford.edu
 *   04-10-2000:
 *     based on original 0.4.x code by Paul Barton-Davis (not much left of it :-)
 *     also Bill's code and Jaroslav Kysela (aplay.c and friends)
 *
 * Changes:
 * 04/25/2000: finished major rework, snd-dac now automatically decides which
 *             device or devices it uses for playback. Multiple device use is
 *             for now restricted to only two at most (more changes in Bill's
 *             needed to be able to support more). Four channel playback in 
 *             Ensoniq AudioPCI and relatives possible (with proper settings
 *             of the mixer) as well as using two separate cards. 
 * 04/11/2000: added reporting of alsa sound formats
*/

#if HAVE_ALSA

#if (!HAVE_OSS)
#define AUDIO_OK 1
#endif

#include <sys/ioctl.h>

#if HAVE_ALSA
  #include <alsa/asoundlib.h>
#else
  #include <sys/asoundlib.h>
#endif

#if SND_LIB_VERSION < ((0<<16)|(6<<8)|(0))
  #error ALSA version is too old -- audio.c needs 0.9 or later
#endif

/* prototypes for the alsa sndlib functions */
static int   alsa_mus_audio_initialize(void);
static void  alsa_mus_oss_set_buffers(int num, int size);
static int   alsa_mus_audio_open_output(int ur_dev, int srate, int chans, int format, int size);
static int   alsa_mus_audio_open_input(int ur_dev, int srate, int chans, int format, int requested_size);
static int   alsa_mus_audio_write(int id, char *buf, int bytes);
static int   alsa_mus_audio_read(int id, char *buf, int bytes);
static int   alsa_mus_audio_close(int id);

/* decide which api to activate */

static int probe_api(void) 
{
#if HAVE_JACK_IN_LINUX
  int jackprobe;
  jackprobe = jack_mus_audio_initialize();
  if (jackprobe == MUS_ERROR)
    {
#endif
    int card = -1;
    if ((snd_card_next(&card) >= 0) && (card >= 0))
      {
    /* the alsa library has detected one or more cards */
    api = MUS_ALSA_API;
    vect_mus_audio_initialize = alsa_mus_audio_initialize;
    vect_mus_oss_set_buffers = alsa_mus_oss_set_buffers;
    vect_mus_audio_moniker = alsa_mus_audio_moniker;
    vect_mus_audio_open_output = alsa_mus_audio_open_output;
    vect_mus_audio_open_input = alsa_mus_audio_open_input;
    vect_mus_audio_write = alsa_mus_audio_write;
    vect_mus_audio_read = alsa_mus_audio_read;
    vect_mus_audio_close = alsa_mus_audio_close;
      } 
    else 
      {
    /* go for the oss api */
        api = MUS_OSS_API;
    vect_mus_audio_initialize = oss_mus_audio_initialize;
    vect_mus_oss_set_buffers = oss_mus_oss_set_buffers;
    vect_mus_audio_moniker = oss_mus_audio_moniker;
    vect_mus_audio_open_output = oss_mus_audio_open_output;
    vect_mus_audio_open_input = oss_mus_audio_open_input;
    vect_mus_audio_write = oss_mus_audio_write;
    vect_mus_audio_read = oss_mus_audio_read;
    vect_mus_audio_close = oss_mus_audio_close;
      }
    /* will the _real_ mus_audio_initialize please stand up? */
    return(vect_mus_audio_initialize());
#if HAVE_JACK_IN_LINUX
    }
  return(jackprobe);
#endif
}

/* convert a sndlib sample format to an alsa sample format */

static snd_pcm_format_t to_alsa_format(int snd_format)
{
  switch (snd_format) 
    {
    case MUS_BYTE:     return(SND_PCM_FORMAT_S8); 
    case MUS_UBYTE:    return(SND_PCM_FORMAT_U8); 
    case MUS_MULAW:    return(SND_PCM_FORMAT_MU_LAW); 
    case MUS_ALAW:     return(SND_PCM_FORMAT_A_LAW); 
    case MUS_BSHORT:   return(SND_PCM_FORMAT_S16_BE); 
    case MUS_LSHORT:   return(SND_PCM_FORMAT_S16_LE); 
    case MUS_UBSHORT:  return(SND_PCM_FORMAT_U16_BE); 
    case MUS_ULSHORT:  return(SND_PCM_FORMAT_U16_LE); 
    case MUS_B24INT:   return(SND_PCM_FORMAT_S24_BE); 
    case MUS_L24INT:   return(SND_PCM_FORMAT_S24_LE); 
    case MUS_BINT:     return(SND_PCM_FORMAT_S32_BE); 
    case MUS_LINT:     return(SND_PCM_FORMAT_S32_LE); 
    case MUS_BINTN:    return(SND_PCM_FORMAT_S32_BE); 
    case MUS_LINTN:    return(SND_PCM_FORMAT_S32_LE); 
    case MUS_BFLOAT:   return(SND_PCM_FORMAT_FLOAT_BE); 
    case MUS_LFLOAT:   return(SND_PCM_FORMAT_FLOAT_LE); 
    case MUS_BDOUBLE:  return(SND_PCM_FORMAT_FLOAT64_BE); 
    case MUS_LDOUBLE:  return(SND_PCM_FORMAT_FLOAT64_LE); 
    }
  return((snd_pcm_format_t)MUS_ERROR);
}

/* FIXME: this is not taking yet into account the 
 * number of bits that a given alsa format is actually
 * using... 
 */

static int to_mus_format(int alsa_format) 
{
  /* alsa format definitions from asoundlib.h (0.9 cvs 6/27/2001) */
  switch (alsa_format)
    {
    case SND_PCM_FORMAT_S8:         return(MUS_BYTE);
    case SND_PCM_FORMAT_U8:         return(MUS_UBYTE);
    case SND_PCM_FORMAT_S16_LE:     return(MUS_LSHORT);
    case SND_PCM_FORMAT_S16_BE:     return(MUS_BSHORT);
    case SND_PCM_FORMAT_U16_LE:     return(MUS_ULSHORT);
    case SND_PCM_FORMAT_U16_BE:     return(MUS_UBSHORT);
    case SND_PCM_FORMAT_S24_LE:     return(MUS_L24INT);
    case SND_PCM_FORMAT_S24_BE:     return(MUS_B24INT);
    case SND_PCM_FORMAT_S32_LE:     return(MUS_LINTN); /* 32bit normalized plays 24bit and 16bit files with same amplitude bound (for 24 bit cards) */
    case SND_PCM_FORMAT_S32_BE:     return(MUS_BINTN);
    case SND_PCM_FORMAT_FLOAT_LE:   return(MUS_LFLOAT);
    case SND_PCM_FORMAT_FLOAT_BE:   return(MUS_BFLOAT);
    case SND_PCM_FORMAT_FLOAT64_LE: return(MUS_LDOUBLE);
    case SND_PCM_FORMAT_FLOAT64_BE: return(MUS_BDOUBLE);
    case SND_PCM_FORMAT_MU_LAW:     return(MUS_MULAW);
    case SND_PCM_FORMAT_A_LAW:      return(MUS_ALAW);
    /* formats with no translation in snd */
    case SND_PCM_FORMAT_U24_LE:
    case SND_PCM_FORMAT_U24_BE:
    case SND_PCM_FORMAT_U32_LE:
    case SND_PCM_FORMAT_U32_BE:
    case SND_PCM_FORMAT_IEC958_SUBFRAME_LE:
    case SND_PCM_FORMAT_IEC958_SUBFRAME_BE:
    case SND_PCM_FORMAT_IMA_ADPCM:
    case SND_PCM_FORMAT_MPEG:
    case SND_PCM_FORMAT_GSM:
    case SND_PCM_FORMAT_SPECIAL:
    default:
      return(MUS_ERROR);
    }
}

/* convert a sndlib device into an alsa device number and channel
 * [has to be coordinated with following function!] 
 */

/* very simplistic approach, device mapping should also depend
 * on which card we're dealing with, digital i/o devices should
 * be identified as such and so on 
 */

/* NOTE: in the Delta1010 digital i/o is just a pair of channels
 * in the 10 channel playback frame or 12 channel capture frame,
 * how do we specify that???
 */

static int to_alsa_device(int dev, int *adev, snd_pcm_stream_t *achan)
{
  switch (dev) 
    {
      /* default values are a problem because the concept does
       * not imply a direction (playback or capture). This works
       * fine as long as both directions of a device are symetric,
       * the Midiman 1010, for example, has 10 channel frames for
       * playback and 12 channel frames for capture and breaks 
       * the recorder (probes the default, defaults to output, 
       * uses the values for input). 
       */
    case MUS_AUDIO_DEFAULT:
    case MUS_AUDIO_DUPLEX_DEFAULT:
    case MUS_AUDIO_LINE_OUT:
      /* analog output */
      (*adev) = 0;
      (*achan) = SND_PCM_STREAM_PLAYBACK;
      break;

    case MUS_AUDIO_AUX_OUTPUT:
      /* extra analog output */
      (*adev) = 1;
      (*achan) = SND_PCM_STREAM_PLAYBACK;
      break;

    case MUS_AUDIO_DAC_OUT:
      /* analog outputs */
      (*adev) = 2;
      (*achan) = SND_PCM_STREAM_PLAYBACK;
      break;

    case MUS_AUDIO_MICROPHONE:
    case MUS_AUDIO_LINE_IN:
      /* analog input */
      (*adev) = 0;
      (*achan) = SND_PCM_STREAM_CAPTURE;
      break;

    default:
      return(MUS_ERROR);
      break;
    }
  return(0);
}

/* convert an alsa device into a sndlib device 
 * [has to be coordinated with previous function!] 
 *
 * naming here is pretty much arbitrary. We have to have
 * a bidirectional mapping between sndlib devices and
 * alsa devices and that's just not possible (I think). 
 * This stopgap mapping ignores digital input and output
 * devices - how to differentiate them in alsa?
 */

static int to_sndlib_device(int dev, int channel) 
{
  switch (channel) 
    {
    case SND_PCM_STREAM_PLAYBACK:
      switch (dev) 
    {
      /* works only for the first three outputs */
    case 0: return(MUS_AUDIO_LINE_OUT);
    case 1: return(MUS_AUDIO_AUX_OUTPUT);
    case 2: return(MUS_AUDIO_DAC_OUT);
    default:
      return(MUS_ERROR);
    }
    case SND_PCM_STREAM_CAPTURE:
      switch (dev) 
    {
    case 0: return(MUS_AUDIO_LINE_IN);
    default:
      return(MUS_ERROR);
    }
      break;
    }
  return(MUS_ERROR);
}


static int alsa_mus_error(int type, char *message)
{
  if (message)
    {
      mus_print("%s", message);
      free(message);
    }
  return(MUS_ERROR);
}


/* dump current hardware and software configuration */

static void alsa_dump_configuration(char *name, snd_pcm_hw_params_t *hw_params, snd_pcm_sw_params_t *sw_params)
{
  int err; 
  char *str;
  size_t len;
  snd_output_t *buf;

#if (SND_LIB_MAJOR == 0) || ((SND_LIB_MAJOR == 1) && (SND_LIB_MINOR == 0) && (SND_LIB_SUBMINOR < 8))
  return; /* avoid Alsa bug */
#endif

  err = snd_output_buffer_open(&buf);
  if (err < 0) 
    {
      mus_print("could not open dump buffer: %s", snd_strerror(err));
    } 
  else 
    {
      if (hw_params) 
    {
      snd_output_puts(buf, "hw_params status of ");
      snd_output_puts(buf, name);
      snd_output_puts(buf, "\n");
      err = snd_pcm_hw_params_dump(hw_params, buf);
      if (err < 0) 
        mus_print("snd_pcm_hw_params_dump: %s", snd_strerror(err));
    }
      if (sw_params) 
    {
      snd_output_puts(buf, "sw_params status of ");
      snd_output_puts(buf, name);
      snd_output_puts(buf, "\n");
      err = snd_pcm_sw_params_dump(sw_params, buf);
      if (err < 0) 
        mus_print("snd_pcm_hw_params_dump: %s", snd_strerror(err));
    }
      snd_output_putc(buf, '\0');
      len = snd_output_buffer_string(buf, &str);
      if (len > 1) 
    mus_print("status of %s\n%s", name, str);
      snd_output_close(buf);
    }
}

/* get hardware params for a pcm */

static snd_pcm_hw_params_t *alsa_get_hardware_params(const char *name, snd_pcm_stream_t stream, int mode)
{
  int err;
  snd_pcm_t *handle;
  if ((err = snd_pcm_open(&handle, name, stream, mode | SND_PCM_NONBLOCK)) != 0) 
    {
      alsa_mus_error(MUS_AUDIO_CANT_OPEN, 
             mus_format("open pcm %s for stream %d: %s",
                name, stream, snd_strerror(err)));
      return(NULL);
    }
  else 
    {
      snd_pcm_hw_params_t *params;
      params = (snd_pcm_hw_params_t *)calloc(1, snd_pcm_hw_params_sizeof());
      if (params == NULL) 
    {
      snd_pcm_close(handle);
      alsa_mus_error(MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE, 
             mus_format("could not allocate memory for hardware params"));
    } 
      else 
    {
      err = snd_pcm_hw_params_any(handle, params);
      if (err < 0) 
        {
          snd_pcm_close(handle);
          alsa_mus_error(MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE, 
                 mus_format("snd_pcm_hw_params_any: pcm %s, stream %d, error: %s",
                    name, stream, snd_strerror(err)));
        } 
      else 
        {
          snd_pcm_close(handle);
          return(params);
        }
    }
    }
  return(NULL);
}

/* allocate software params structure */

static snd_pcm_sw_params_t *alsa_get_software_params(void)
{
  snd_pcm_sw_params_t *params = NULL;
  params = (snd_pcm_sw_params_t *)calloc(1, snd_pcm_sw_params_sizeof());
  if (params == NULL) 
    {
      alsa_mus_error(MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE, 
             mus_format("could not allocate memory for software params"));
    } 
  return(params);
}

/* probe a device name against the list of available pcm devices */

static bool alsa_probe_device_name(const char *name)
{
  snd_config_t *conf;
  snd_config_iterator_t pos, next;
  int err;
  
  err = snd_config_update();
  if (err < 0) 
    {
      mus_print("snd_config_update: %s", snd_strerror(err));
      return(false);
    }

  err = snd_config_search(snd_config, "pcm", &conf);
  if (err < 0) 
    {
      mus_print("snd_config_search: %s", snd_strerror(err));
      return(false);
    }

  snd_config_for_each(pos, next, conf) 
    {
      snd_config_t *c = snd_config_iterator_entry(pos);
      const char *id;
      int err = snd_config_get_id(c, &id);
      if (err == 0) {
    int result = strncmp(name, id, strlen(id));
    if (result == 0 &&
        (name[strlen(id)] == '\0' || name[strlen(id)] == ':')) 
      {
        return(true);
      }
      }
    }
  return(false);
}

/* check a device name against the list of available pcm devices */

static int alsa_check_device_name(const char *name)
{
  if (!alsa_probe_device_name(name)) 
    {
      return(alsa_mus_error(MUS_AUDIO_CANT_READ, 
                mus_format("alsa could not find device \"%s\" in configuration", 
                       name)));
    } 
  return(MUS_NO_ERROR);
}


/* set scheduling priority to SCHED_FIFO 
 * this will only work if the program that uses sndlib is run as root or is suid root 
 */

/* whether we want to trace calls 
 *
 * set to "1" to print function trace information in the 
 * snd error window
 */

static int alsa_trace = 0;

/* this should go away as it is oss specific */

static int fragment_size = 512; 
static int fragments = 4;

static void alsa_mus_oss_set_buffers(int num, int size) 
{
  fragments = num; 
  fragment_size = size; 
}

/* total number of soundcards in our setup, set by initialize_audio */

/* static int sound_cards = 0; */

/* return the number of cards that are available */

/* return the type of driver we're dealing with */

static char *alsa_mus_audio_moniker(void)
{
  if (version_name == NULL) version_name = (char *)calloc(LABEL_BUFFER_SIZE, sizeof(char));
  snprintf(version_name, LABEL_BUFFER_SIZE, "ALSA %s", SND_LIB_VERSION_STR);
  return(version_name);
}

/* handles for both directions of the virtual device */

static snd_pcm_t *handles[2] = {NULL, NULL};

/* hardware and software parameter sctructure pointers */

static snd_pcm_hw_params_t *alsa_hw_params[2] = {NULL, NULL}; /* avoid bogus free */
static snd_pcm_sw_params_t *alsa_sw_params[2] = {NULL, NULL};

/* some defaults */

static int alsa_open_mode = SND_PCM_ASYNC;
static int alsa_buffers = 3;
/* size of buffer in number of samples per channel, 
 * at 44100 approximately 5.9mSecs
 */
static int alsa_samples_per_channel = 1024;
static snd_pcm_access_t alsa_interleave = SND_PCM_ACCESS_RW_INTERLEAVED;
static int alsa_max_capture_channels = 32;

/* first default name for pcm configuration */

static char *alsa_sndlib_device_name = (char *)"sndlib";

/* second default for playback and capture: hardware pcm, first card, first device */
/* pcms used by sndlib, playback and capture */

static char *alsa_playback_device_name = NULL;
static char *alsa_capture_device_name = NULL;



/* -------- tie these names into scheme/ruby -------- */

static int alsa_get_max_buffers(void)
{
  unsigned int max_periods = 0, max_rec_periods = 0;
  int dir = 0;

  if (alsa_hw_params[SND_PCM_STREAM_PLAYBACK])
    snd_pcm_hw_params_get_periods_max(alsa_hw_params[SND_PCM_STREAM_PLAYBACK], &max_periods, &dir);

  if (alsa_hw_params[SND_PCM_STREAM_CAPTURE]) 
    {
      snd_pcm_hw_params_get_periods_max(alsa_hw_params[SND_PCM_STREAM_CAPTURE], &max_rec_periods, &dir);

      if (max_periods > max_rec_periods) 
    max_periods = max_rec_periods;
    }
  return(max_periods);
}

static int alsa_get_min_buffers(void)
{
  unsigned int min_periods = 0, min_rec_periods = 0;
  int dir = 0;
  if (alsa_hw_params[SND_PCM_STREAM_PLAYBACK])
    snd_pcm_hw_params_get_periods_min(alsa_hw_params[SND_PCM_STREAM_PLAYBACK], &min_periods, &dir);

  if (alsa_hw_params[SND_PCM_STREAM_CAPTURE]) 
    {
      snd_pcm_hw_params_get_periods_min(alsa_hw_params[SND_PCM_STREAM_CAPTURE], &min_rec_periods, &dir);

      if (min_periods < min_rec_periods) 
    min_periods = min_rec_periods;
    }
  return(min_periods);
}

static int alsa_clamp_buffers(int bufs)
{
  int minb, maxb;
  minb = alsa_get_min_buffers();
  maxb = alsa_get_max_buffers();
  if (bufs > maxb)
    bufs = maxb;
  if (bufs < minb)
    bufs = minb;
  return(bufs);
}

static snd_pcm_uframes_t alsa_get_min_buffer_size(void)
{
  snd_pcm_uframes_t min_buffer_size = 0, min_rec_buffer_size = 0;
  if (alsa_hw_params[SND_PCM_STREAM_PLAYBACK])
    snd_pcm_hw_params_get_buffer_size_min(alsa_hw_params[SND_PCM_STREAM_PLAYBACK], &min_buffer_size);

  if (alsa_hw_params[SND_PCM_STREAM_CAPTURE]) 
    {
      snd_pcm_hw_params_get_buffer_size_min(alsa_hw_params[SND_PCM_STREAM_CAPTURE], &min_rec_buffer_size);
      if (min_buffer_size < min_rec_buffer_size) 
    min_buffer_size = min_rec_buffer_size;
    }
  return(min_buffer_size);
}

static snd_pcm_uframes_t alsa_get_max_buffer_size(void)
{
  snd_pcm_uframes_t max_buffer_size = 0, max_rec_buffer_size = 0;
  if (alsa_hw_params[SND_PCM_STREAM_PLAYBACK])
    snd_pcm_hw_params_get_buffer_size_max(alsa_hw_params[SND_PCM_STREAM_PLAYBACK], &max_buffer_size);

  if (alsa_hw_params[SND_PCM_STREAM_CAPTURE]) 
    {
      snd_pcm_hw_params_get_buffer_size_max(alsa_hw_params[SND_PCM_STREAM_CAPTURE], &max_rec_buffer_size);
      if (max_buffer_size > max_rec_buffer_size) 
    max_buffer_size = max_rec_buffer_size;
    }
  return(max_buffer_size);
}

static snd_pcm_uframes_t alsa_clamp_buffer_size(snd_pcm_uframes_t buf_size)
{
  snd_pcm_uframes_t minb, maxb;
  minb = alsa_get_min_buffer_size();
  maxb = alsa_get_max_buffer_size();
  if (buf_size > maxb)
    buf_size = maxb;
  if (buf_size < minb)
    buf_size = minb;
  return(buf_size);
}

static bool alsa_set_playback_parameters(void)
{
  /* playback stream parameters */
  if (alsa_hw_params[SND_PCM_STREAM_PLAYBACK]) free(alsa_hw_params[SND_PCM_STREAM_PLAYBACK]);
  alsa_hw_params[SND_PCM_STREAM_PLAYBACK] = alsa_get_hardware_params(alsa_playback_device_name, SND_PCM_STREAM_PLAYBACK, alsa_open_mode);
  if (alsa_hw_params[SND_PCM_STREAM_PLAYBACK]) 
    {
      snd_pcm_uframes_t size;
      int old_buffers;
      old_buffers = alsa_buffers;
      if (alsa_sw_params[SND_PCM_STREAM_PLAYBACK]) free(alsa_sw_params[SND_PCM_STREAM_PLAYBACK]);
      alsa_sw_params[SND_PCM_STREAM_PLAYBACK] = alsa_get_software_params();
      sound_cards = 1;
      alsa_buffers = alsa_clamp_buffers(alsa_buffers);
      if (alsa_buffers <= 0)
    {
      alsa_buffers = old_buffers;
      return(false);
    }
      size = alsa_clamp_buffer_size((snd_pcm_uframes_t)(alsa_samples_per_channel * alsa_buffers));
      if (size <= 0) return(false);
      alsa_samples_per_channel = size / alsa_buffers;
    }
  return(alsa_hw_params[SND_PCM_STREAM_PLAYBACK] && alsa_sw_params[SND_PCM_STREAM_PLAYBACK]);
}

static bool alsa_set_capture_parameters(void)
{  
  /* capture stream parameters */
  if (alsa_hw_params[SND_PCM_STREAM_CAPTURE]) free(alsa_hw_params[SND_PCM_STREAM_CAPTURE]);
  alsa_hw_params[SND_PCM_STREAM_CAPTURE] = alsa_get_hardware_params(alsa_capture_device_name, SND_PCM_STREAM_CAPTURE, alsa_open_mode);
  if (alsa_hw_params[SND_PCM_STREAM_CAPTURE]) 
    {
      snd_pcm_uframes_t size;
      int old_buffers;
      old_buffers = alsa_buffers;
      if (alsa_sw_params[SND_PCM_STREAM_CAPTURE]) free(alsa_sw_params[SND_PCM_STREAM_CAPTURE]);
      alsa_sw_params[SND_PCM_STREAM_CAPTURE] = alsa_get_software_params();
      sound_cards = 1;
      alsa_buffers = alsa_clamp_buffers(alsa_buffers);
      if (alsa_buffers <= 0)
    {
      alsa_buffers = old_buffers;
      return(false);
    }
      size = alsa_clamp_buffer_size((snd_pcm_uframes_t)(alsa_samples_per_channel * alsa_buffers));
      if (size <= 0) return(false);
      alsa_samples_per_channel = size / alsa_buffers;
    }
  return(alsa_hw_params[SND_PCM_STREAM_CAPTURE] && alsa_sw_params[SND_PCM_STREAM_CAPTURE]);
}


char *mus_alsa_playback_device(void) {return(alsa_playback_device_name);}
char *mus_alsa_set_playback_device(const char *name) 
{
  if (alsa_check_device_name(name) == MUS_NO_ERROR)
    {
      char *old_name = alsa_playback_device_name;
      alsa_playback_device_name = mus_strdup(name); 
      if (!alsa_set_playback_parameters())
    {
      alsa_playback_device_name = old_name; /* try to back out of the mistake */
      alsa_set_playback_parameters();
    }
    }
  return(alsa_playback_device_name);
}

char *mus_alsa_capture_device(void) {return(alsa_capture_device_name);}
char *mus_alsa_set_capture_device(const char *name) 
{
  if (alsa_check_device_name(name) == MUS_NO_ERROR)
    {
      char *old_name = alsa_capture_device_name;
      alsa_capture_device_name = mus_strdup(name); 
      if (!alsa_set_capture_parameters())
    {
      alsa_capture_device_name = old_name;
      alsa_set_capture_parameters();
    }
    }
  return(alsa_capture_device_name);
}

char *mus_alsa_device(void) {return(alsa_sndlib_device_name);}
char *mus_alsa_set_device(const char *name) 
{
  if (alsa_check_device_name(name) == MUS_NO_ERROR)
    {
      alsa_sndlib_device_name = mus_strdup(name);
      mus_alsa_set_playback_device(name);
      mus_alsa_set_capture_device(name);
    }
  return(alsa_sndlib_device_name);
}

int mus_alsa_buffer_size(void) {return(alsa_samples_per_channel);}
int mus_alsa_set_buffer_size(int size) 
{
  snd_pcm_uframes_t bsize;
  if (alsa_buffers == 0) alsa_buffers = 1;
  if (size > 0)
    {
      bsize = alsa_clamp_buffer_size((snd_pcm_uframes_t)(size * alsa_buffers));
      alsa_samples_per_channel = bsize / alsa_buffers;
    }
  return(alsa_samples_per_channel);
}

int mus_alsa_buffers(void) {return(alsa_buffers);}
int mus_alsa_set_buffers(int num) 
{
  snd_pcm_uframes_t size;
  if (num > 0)
    {
      alsa_buffers = alsa_clamp_buffers(num);
      if (alsa_buffers > 0)
    {
      size = alsa_clamp_buffer_size((snd_pcm_uframes_t)(alsa_samples_per_channel * alsa_buffers));
      alsa_samples_per_channel = size / alsa_buffers;
    }
    }
  return(alsa_buffers);
}

static bool alsa_squelch_warning = false;
bool mus_alsa_squelch_warning(void) {return(alsa_squelch_warning);}
bool mus_alsa_set_squelch_warning(bool val) 
{
  alsa_squelch_warning = val; 
  return(val);
}




/* get a device name from the environment */

static char *alsa_get_device_from_env(const char *name)
{
  char *string = getenv(name);
  if (string) 
    if (alsa_check_device_name(string) == MUS_NO_ERROR) 
      return(string);
  return(NULL);
}

/* get an integer from the environment */

static int alsa_get_int_from_env(const char *name, int *value, int min, int max)
{
  char *string = getenv(name);
  if (string) 
    {
      char *end;
      long int result = strtol(string, &end, 10);
      if (((min != -1) && (max != -1)) &&
      (result < min || result > max)) 
    {
      return(alsa_mus_error(MUS_AUDIO_CANT_READ, 
                mus_format("%s ignored: out of range, value=%d, min=%d, max=%d",
                       name, (int)result, min, max)));
    } 
      else 
    {
      if (errno == ERANGE) 
        {
          return(alsa_mus_error(MUS_AUDIO_CANT_READ, 
                    mus_format("%s ignored: strlol conversion out of range",
                           name)));
        } 
      else 
        {
          if ((*string != '\0') && (*end == '\0'))
        {
          *value = (int)result;
          return(MUS_NO_ERROR);
        } 
          else 
        {
          return(alsa_mus_error(MUS_AUDIO_CANT_READ, 
                    mus_format("%s ignored: value is \"%s\", not an integer",
                           name, string)));
        }
        }
    }
    }
  return(MUS_ERROR);
}

/* initialize the audio subsystem */

/* define environment variable names */
#define MUS_ALSA_PLAYBACK_DEVICE_ENV_NAME "MUS_ALSA_PLAYBACK_DEVICE"
#define MUS_ALSA_CAPTURE_DEVICE_ENV_NAME  "MUS_ALSA_CAPTURE_DEVICE"
#define MUS_ALSA_DEVICE_ENV_NAME          "MUS_ALSA_DEVICE"
#define MUS_ALSA_BUFFERS_ENV_NAME         "MUS_ALSA_BUFFERS"
#define MUS_ALSA_BUFFER_SIZE_ENV_NAME     "MUS_ALSA_BUFFER_SIZE"
#define MUS_ALSA_TRACE_ENV_NAME           "MUS_ALSA_TRACE"

static int alsa_mus_audio_initialize(void) 
{
  char *name = NULL;
  char *pname;
  char *cname;
  int value = 0, alsa_buffer_size = 0;

  if (audio_initialized) 
    return(0);

  sound_cards = 0;

  /* get trace flag from environment */
  if (alsa_get_int_from_env(MUS_ALSA_TRACE_ENV_NAME, &value, 0, 1) == MUS_NO_ERROR) 
    alsa_trace = value;

  /* try to get device names from environment */
  pname = alsa_get_device_from_env(MUS_ALSA_PLAYBACK_DEVICE_ENV_NAME);
  if ((pname) && (alsa_probe_device_name(pname)))
    alsa_playback_device_name = pname;

  cname = alsa_get_device_from_env(MUS_ALSA_CAPTURE_DEVICE_ENV_NAME);
  if ((cname) && (alsa_probe_device_name(cname)))
    alsa_capture_device_name = cname;
    
  name = alsa_get_device_from_env(MUS_ALSA_DEVICE_ENV_NAME);
  if ((name) && (alsa_probe_device_name(name)))
    {
      if (!alsa_playback_device_name) 
    alsa_playback_device_name = name;

      if (!alsa_capture_device_name) 
    alsa_capture_device_name = name;

      alsa_sndlib_device_name = name;
    }

  /* now check that we have a plausible name */
  if (!alsa_probe_device_name(alsa_sndlib_device_name))
    {
      alsa_sndlib_device_name = (char *)"default";
      if (!alsa_probe_device_name(alsa_sndlib_device_name))
    {
      alsa_sndlib_device_name = (char *)"plughw:0";
      if (!alsa_probe_device_name(alsa_sndlib_device_name))
        alsa_sndlib_device_name = (char *)"hw:0";
    }
    }
    
  /* if no device name set yet, try for special sndlib name first */
  if (!alsa_playback_device_name) 
    {
      if (alsa_probe_device_name(alsa_sndlib_device_name)) 
    alsa_playback_device_name = alsa_sndlib_device_name;
      else alsa_playback_device_name = (char *)"hw:0";
    }

  if (!alsa_capture_device_name) 
    {
      if (alsa_probe_device_name(alsa_sndlib_device_name)) 
    alsa_capture_device_name = alsa_sndlib_device_name;
      else alsa_capture_device_name = (char *)"hw:0";
    }

  alsa_get_int_from_env(MUS_ALSA_BUFFERS_ENV_NAME, &alsa_buffers, -1, -1);
  alsa_get_int_from_env(MUS_ALSA_BUFFER_SIZE_ENV_NAME, &alsa_buffer_size, -1, -1);

  if ((alsa_buffer_size > 0) && (alsa_buffers > 0))
    alsa_samples_per_channel = alsa_buffer_size / alsa_buffers;

  if (!alsa_set_playback_parameters())
    {
      /* somehow we got a device that passed muster with alsa_probe_device_name, but doesn't return hw params! */
      alsa_playback_device_name = (char *)"plughw:0";
      if (!alsa_set_playback_parameters())
    {
      alsa_playback_device_name = (char *)"hw:0";
      if (!alsa_set_playback_parameters())
        return(MUS_ERROR);
    }
    }

  if (!alsa_set_capture_parameters())
    {
      alsa_capture_device_name = (char *)"plughw:0";
      if (!alsa_set_capture_parameters())
    {
      alsa_capture_device_name = (char *)"hw:0";
      if (!alsa_set_capture_parameters())
        return(MUS_ERROR);
    }
    }

  if ((!alsa_hw_params[SND_PCM_STREAM_CAPTURE]) ||
      (!alsa_hw_params[SND_PCM_STREAM_PLAYBACK]))
    return(MUS_ERROR);

  audio_initialized = true;
  return(0);
}

/* open an input or output stream */

static int alsa_audio_open(int ur_dev, int srate, int chans, int format, int size)
{
  int device, alsa_device;
  snd_pcm_format_t alsa_format;
  snd_pcm_stream_t alsa_stream;
  char *alsa_name;
  int frames, periods;
  int err;
  snd_pcm_t *handle;
  snd_pcm_hw_params_t *hw_params = NULL;
  snd_pcm_sw_params_t *sw_params = NULL;
  
  if ((!audio_initialized) && 
      (mus_audio_initialize() != MUS_NO_ERROR))
    return(MUS_ERROR);
  if (chans <= 0) return(MUS_ERROR);
  
  if (alsa_trace) 
    mus_print("%s: %x rate=%d, chans=%d, format=%d:%s, size=%d", 
          __func__, ur_dev, srate, chans, format, 
          mus_data_format_to_string(format), size);

  /* card = MUS_AUDIO_SYSTEM(ur_dev); */
  device = MUS_AUDIO_DEVICE(ur_dev);

  if ((err = to_alsa_device(device, &alsa_device, &alsa_stream)) < 0) 
    {
      return(alsa_mus_error(MUS_AUDIO_DEVICE_NOT_AVAILABLE, 
                mus_format("%s: cannot translate device %d to alsa",
                       snd_strerror(err), device)));
    }
  if ((alsa_format = to_alsa_format(format)) == (snd_pcm_format_t)MUS_ERROR) 
    {
      return(alsa_mus_error(MUS_AUDIO_FORMAT_NOT_AVAILABLE, 
                mus_format("could not change %s<%d> to alsa format", 
                       mus_data_format_to_string(format), format)));
    }

  alsa_name = (alsa_stream == SND_PCM_STREAM_PLAYBACK) ? alsa_playback_device_name : alsa_capture_device_name;
  if ((err = snd_pcm_open(&handle, alsa_name, alsa_stream, alsa_open_mode)) != 0) 
    {
      /* snd_pcm_close(handle); */
      /* this segfaults in some versions of ALSA */
      return(alsa_mus_error(MUS_AUDIO_CANT_OPEN, 
                mus_format("open pcm %s stream %s: %s",
                       alsa_name, snd_pcm_stream_name(alsa_stream), 
                       snd_strerror(err))));
    }
  handles[alsa_stream] = handle;
  hw_params = alsa_hw_params[alsa_stream];
  sw_params = alsa_sw_params[alsa_stream];
  if ((err = snd_pcm_hw_params_any(handle, hw_params)) < 0) 
    {
      snd_pcm_close(handle);
      handles[alsa_stream] = NULL;
      alsa_dump_configuration(alsa_name, hw_params, sw_params);
      return(alsa_mus_error(MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE, 
                mus_format("%s: no parameter configurations available for %s", 
                       snd_strerror(err), alsa_name)));
    }

  err = snd_pcm_hw_params_set_access(handle, hw_params, alsa_interleave);
  if (err < 0) 
    {
      snd_pcm_close(handle);
      handles[alsa_stream] = NULL;
      alsa_dump_configuration(alsa_name, hw_params, sw_params);
      return(alsa_mus_error(MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE, 
                mus_format("%s: %s: access type %s not available", 
                       snd_strerror(err), alsa_name, snd_pcm_access_name(alsa_interleave))));
    }

  periods = alsa_buffers;
  err = snd_pcm_hw_params_set_periods(handle, hw_params, periods, 0);
  if (err < 0) 
    {
      unsigned int minp, maxp;
      int dir;
      snd_pcm_hw_params_get_periods_min(hw_params, &minp, &dir);
      snd_pcm_hw_params_get_periods_max(hw_params, &maxp, &dir);
      snd_pcm_close(handle);
      handles[alsa_stream] = NULL;
      alsa_dump_configuration(alsa_name, hw_params, sw_params);
      return(alsa_mus_error(MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE, 
                mus_format("%s: %s: cannot set number of periods to %d, min is %d, max is %d", 
                       snd_strerror(err), alsa_name, periods, (int)minp, (int)maxp)));
    }

  frames = size / chans / mus_bytes_per_sample(format);

  err = snd_pcm_hw_params_set_buffer_size(handle, hw_params, frames * periods);
  if (err < 0) 
    {
      snd_pcm_uframes_t minp, maxp;
      snd_pcm_hw_params_get_buffer_size_min(hw_params, &minp);
      snd_pcm_hw_params_get_buffer_size_max(hw_params, &maxp);
      snd_pcm_close(handle);
      handles[alsa_stream] = NULL;
      alsa_dump_configuration(alsa_name, hw_params, sw_params);
      return(alsa_mus_error(MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE, 
                mus_format("%s: %s: cannot set buffer size to %d periods of %d frames; \
total requested buffer size is %d frames, minimum allowed is %d, maximum is %d", 
                       snd_strerror(err), alsa_name, periods, frames, periods * frames, (int)minp, (int)maxp)));
    }

  err = snd_pcm_hw_params_set_format(handle, hw_params, alsa_format);
  if (err < 0) 
    {
      snd_pcm_close(handle);
      handles[alsa_stream] = NULL;
      alsa_dump_configuration(alsa_name, hw_params, sw_params);
      return(alsa_mus_error(MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE, 
                mus_format("%s: %s: cannot set format to %s", 
                       snd_strerror(err), alsa_name, snd_pcm_format_name(alsa_format))));
    }

  err = snd_pcm_hw_params_set_channels(handle, hw_params, chans);
  if (err < 0) 
    {
      snd_pcm_close(handle);
      handles[alsa_stream] = NULL;
      alsa_dump_configuration(alsa_name, hw_params, sw_params);
      return(alsa_mus_error(MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE, 
                mus_format("%s: %s: cannot set channels to %d", 
                       snd_strerror(err), alsa_name, chans)));
    }

  {
    unsigned int new_rate;
    new_rate = srate;
    /* r is unsigned int so it can't be negative */
    err = snd_pcm_hw_params_set_rate_near(handle, hw_params, &new_rate, 0);
    if ((new_rate != (unsigned int)srate) && (!alsa_squelch_warning))
      {
    mus_print("%s: could not set rate to exactly %d, set to %d instead",
          alsa_name, srate, new_rate);
      }
  }

  err = snd_pcm_hw_params(handle, hw_params);
  if (err < 0) 
    {
      snd_pcm_close(handle);
      handles[alsa_stream] = NULL;
      alsa_dump_configuration(alsa_name, hw_params, sw_params);
      return(alsa_mus_error(MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE, 
                mus_format("%s: cannot set hardware parameters for %s", 
                       snd_strerror(err), alsa_name)));
    }

  snd_pcm_sw_params_current(handle, sw_params);
  err = snd_pcm_sw_params(handle, sw_params);
  if (err < 0) 
    {
      snd_pcm_close(handle);
      handles[alsa_stream] = NULL;
      alsa_dump_configuration(alsa_name, hw_params, sw_params);
      return(alsa_mus_error(MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE, 
                mus_format("%s: cannot set software parameters for %s", 
                       snd_strerror(err), alsa_name)));
    }

  /* for now the id for the stream is the direction identifier, that is
     not a problem because we only advertise one card with two devices */
  return(alsa_stream);
}

/* sndlib support for opening output devices */

static int alsa_mus_audio_open_output(int ur_dev, int srate, int chans, int format, int size)
{
  return(alsa_audio_open(ur_dev, srate, chans, format, size));
}

/* sndlib support for opening input devices */

static int alsa_mus_audio_open_input(int ur_dev, int srate, int chans, int format, int size)
{
  return(alsa_audio_open(ur_dev, srate, chans, format, size));
}

/* sndlib support for closing a device */

/* to force it to stop, snd_pcm_drop */

static bool xrun_warned = false;

static int alsa_mus_audio_close(int id)
{
  int err = 0;
  xrun_warned = false;
  if (id == MUS_ERROR) return(MUS_ERROR);
  if (alsa_trace) mus_print( "%s: %d", __func__, id); 
  if (handles[id]) 
    {
      err = snd_pcm_drain(handles[id]);
      if (err != 0) 
    mus_print("snd_pcm_drain: %s", snd_strerror(err)); 

      err = snd_pcm_close(handles[id]);
      if (err != 0) 
    return(alsa_mus_error(MUS_AUDIO_CANT_CLOSE, 
                  mus_format("snd_pcm_close: %s", 
                     snd_strerror(err)))); 
      handles[id] = NULL;
    }
  return(MUS_NO_ERROR);
}

/* recover from underruns or overruns */

static int recover_from_xrun(int id)
{
  int err;
  snd_pcm_status_t *status;
  snd_pcm_state_t state;
  snd_pcm_status_alloca(&status);
  err = snd_pcm_status(handles[id], status);
  if (err < 0) 
    {
      mus_print("%s: snd_pcm_status: %s", __func__, snd_strerror(err));
      return(MUS_ERROR);
    }
  state = snd_pcm_status_get_state(status);
  if (state == SND_PCM_STATE_XRUN) 
    {
      if (!xrun_warned)
    {
      xrun_warned = true;
      mus_print("[under|over]run detected");
    }
      err = snd_pcm_prepare(handles[id]);
      if (err < 0) 
    mus_print("snd_pcm_prepare: %s", snd_strerror(err));
      else return(MUS_NO_ERROR);
    }
  else mus_print("%s: error, current state is %s", __func__, snd_pcm_state_name(state));
  return(MUS_ERROR);
}

/* sndlib support for writing a buffer to an output device */

static int alsa_mus_audio_write(int id, char *buf, int bytes)
{
  snd_pcm_sframes_t status;
  ssize_t frames;
  if (id == MUS_ERROR) return(MUS_ERROR);
  frames = snd_pcm_bytes_to_frames(handles[id], bytes);
  status = snd_pcm_writei(handles[id], buf, frames);
  if ((status == -EAGAIN) || 
      ((status >= 0) && (status < frames)))
    snd_pcm_wait(handles[id], 1000);
  else
    {
      if (status == -EPIPE) 
    return(recover_from_xrun(id));
      else 
    {
      if (status < 0) 
        {
          mus_print("snd_pcm_writei: %s", snd_strerror(status));
          return(MUS_ERROR);
        }
    }
    }
  return(MUS_NO_ERROR);
}

/* sndlib support for reading a buffer from an input device */

static int alsa_mus_audio_read(int id, char *buf, int bytes)
{
  snd_pcm_sframes_t status;
  ssize_t frames;
  if (id == MUS_ERROR) return(MUS_ERROR);
  frames = snd_pcm_bytes_to_frames(handles[id], bytes);
  status = snd_pcm_readi(handles[id], buf, frames);
  if ((status == -EAGAIN) || 
      ((status >= 0) && (status < frames)))
    snd_pcm_wait(handles[id], 1000);
  else 
    {
      if (status == -EPIPE) 
    return(recover_from_xrun(id));
      else 
    {
      if (status < 0) 
        {
          mus_print("snd_pcm_readi: %s", snd_strerror(status));
          return(MUS_ERROR);
        }
    }
    }
  return(MUS_NO_ERROR);
}

/* read state of the audio hardware */

static int alsa_chans(int ur_dev, int *info)
{
  int card;
  int device;
  int alsa_device = 0;
  snd_pcm_stream_t alsa_stream = SND_PCM_STREAM_PLAYBACK;

  if ((!audio_initialized) && 
      (mus_audio_initialize() != MUS_NO_ERROR))
    return(MUS_ERROR);
  
  card = MUS_AUDIO_SYSTEM(ur_dev);
  device = MUS_AUDIO_DEVICE(ur_dev);
  to_alsa_device(device, &alsa_device, &alsa_stream);

  if (card > 0 || alsa_device > 0) 
    return(alsa_mus_error(MUS_AUDIO_CANT_READ, NULL));

  if ((alsa_stream == SND_PCM_STREAM_CAPTURE) &&
      (alsa_capture_device_name) &&
      (strcmp(alsa_capture_device_name, "default") == 0))
    {
      if (info)
    info[0] = 2;
      else return(2);
    }

  {
    unsigned int max_channels = 0;
    snd_pcm_hw_params_get_channels_max(alsa_hw_params[alsa_stream], &max_channels);

    if ((alsa_stream == SND_PCM_STREAM_CAPTURE) &&
    (max_channels > (unsigned int)alsa_max_capture_channels))
      {
    /* limit number of capture channels to a reasonable maximum, if the user
       specifies a plug pcm as the capture pcm then the returned number of channels
       would be MAXINT (or whatever the name is for a really big number). At this
       point there is no support in the alsa api to distinguish between default
       parameters or those that have been set by a user on purpose, of for querying
       the hardware pcm device that is hidden by the plug device to see what is the
       real number of channels for the device we are dealing with. We could also try
       to flag this as an error to the user and exit the program */
    max_channels = alsa_max_capture_channels;
      }

    if (info)
      {
    unsigned int tmp = 0;
    info[0] = max_channels;
    snd_pcm_hw_params_get_channels_min(alsa_hw_params[alsa_stream], &tmp); 
    info[1] = tmp;
    info[2] = max_channels;
      }

    return(max_channels);
  }
}


static int alsa_formats(int ur_dev, int chan, int *val)
{
  int card;
  int device;
  int alsa_device = 0;
  snd_pcm_stream_t alsa_stream = SND_PCM_STREAM_PLAYBACK;
  int f, err;
  
  if ((!audio_initialized) && 
      (mus_audio_initialize() != MUS_NO_ERROR))
    return(MUS_ERROR);
  
  card = MUS_AUDIO_SYSTEM(ur_dev);
  device = MUS_AUDIO_DEVICE(ur_dev);
  to_alsa_device(device, &alsa_device, &alsa_stream);

  if (card > 0 || alsa_device > 0) 
    return(alsa_mus_error(MUS_AUDIO_CANT_READ, NULL));

  {
    int format;
    snd_pcm_format_mask_t *mask;

    snd_pcm_format_mask_alloca(&mask);
    snd_pcm_hw_params_get_format_mask(alsa_hw_params[alsa_stream], mask); 

    for (format = 0, f = 1; format < SND_PCM_FORMAT_LAST; format++) 
      {
    err = snd_pcm_format_mask_test(mask, (snd_pcm_format_t)format);
    if (err > 0) 
      {
        if ((f < chan) && 
        (to_mus_format(format) != MUS_ERROR))
          val[f++] = to_mus_format(format);
      }
      }
    val[0] = f - 1;
  }
  
  return(MUS_NO_ERROR);
}

#endif /* HAVE_ALSA */



/* -------------------------------- SUN -------------------------------- */
/*
 * Thanks to Seppo Ingalsuo for several bugfixes.
 * record case improved after perusal of Snack 1.6/src/jkAudio_sun.c
 */

/* apparently input other than 8000 is 16-bit, 8000 is (?) mulaw */

#if (defined(__sun) || defined(__SVR4)) && (!(defined(AUDIO_OK)))
#define AUDIO_OK 1

#include <sys/types.h>
#ifdef SUNOS
  #include <stropts.h>
#endif
#include <sys/filio.h>

#ifdef SUNOS
#include <sun/audioio.h>
#else
#include <sys/audioio.h>
#endif

#include <sys/mixer.h>

int mus_audio_initialize(void) {return(MUS_NO_ERROR);}

#define DAC_NAME "/dev/audio"
#define AUDIODEV_ENV "AUDIODEV"

#define RETURN_ERROR_EXIT(Error_Type, Audio_Line, Ur_Error_Message) \
  do { char *Error_Message; Error_Message = Ur_Error_Message; \
    if (Audio_Line != -1) close(Audio_Line); \
    if (Error_Message) \
      {MUS_STANDARD_ERROR(Error_Type, Error_Message); free(Error_Message);} \
    else MUS_STANDARD_ERROR(Error_Type, mus_error_type_to_string(Error_Type)); \
    return(MUS_ERROR); \
  } while (false)

char *mus_audio_moniker(void) 
{
#ifndef AUDIO_DEV_AMD
  struct audio_device ad;
#else
  int ad;
#endif
  int audio_fd, err;
  char *dev_name;
  if (getenv(AUDIODEV_ENV) != NULL) 
    dev_name = getenv(AUDIODEV_ENV); 
  else dev_name = (char *)DAC_NAME;
  audio_fd = open(dev_name, O_RDONLY | O_NONBLOCK, 0);
  if (audio_fd == -1) 
    {
      audio_fd = open("/dev/audioctl", O_RDONLY | O_NONBLOCK, 0);
      if (audio_fd == -1) return("sun probably");
    }
  err = ioctl(audio_fd, AUDIO_GETDEV, &ad); 
  if (err == -1) 
    {
      close(audio_fd); 
      return("sun?");
    }
  mus_audio_close(audio_fd);

  if (version_name == NULL) version_name = (char *)calloc(PRINT_BUFFER_SIZE, sizeof(char));
#ifndef AUDIO_DEV_AMD
  snprintf(version_name, LABEL_BUFFER_SIZE, "audio: %s (%s)", ad.name, ad.version);
#else
  switch (ad)
    {
    case AUDIO_DEV_AMD:        snprintf(version_name, LABEL_BUFFER_SIZE, "audio: amd");        break;
  #ifdef AUDIO_DEV_CS4231
    case AUDIO_DEV_CS4231:     snprintf(version_name, LABEL_BUFFER_SIZE, "audio: cs4231");     break;
  #endif
    case AUDIO_DEV_SPEAKERBOX: snprintf(version_name, LABEL_BUFFER_SIZE, "audio: speakerbox"); break;
    case AUDIO_DEV_CODEC:      snprintf(version_name, LABEL_BUFFER_SIZE, "audio: codec");      break;
    default:                   snprintf(version_name, LABEL_BUFFER_SIZE, "audio: unknown");    break;
    }
#endif
  return(version_name);
}

static int to_sun_format(int format)
{
  switch (format)
    {
#if MUS_LITTLE_ENDIAN
    case MUS_LSHORT: /* Solaris on Intel? */
#else
    case MUS_BSHORT: 
#endif
      return(AUDIO_ENCODING_LINEAR); 
      break;
    case MUS_BYTE: 
#if defined(AUDIO_ENCODING_LINEAR8)
      return(AUDIO_ENCODING_LINEAR8); break;
#else
      return(AUDIO_ENCODING_LINEAR);
      break;
#endif
    case MUS_MULAW: return(AUDIO_ENCODING_ULAW); break;
    case MUS_ALAW:  return(AUDIO_ENCODING_ALAW); break;
      /* there's also AUDIO_ENCODING_DVI */
    }
  return(MUS_ERROR);
}

int mus_audio_open_output(int ur_dev, int srate, int chans, int format, int size)
{
  struct audio_info info;
  char *dev_name;
  int encode, bits, dev;
  int audio_fd, err;
  dev = MUS_AUDIO_DEVICE(ur_dev);
  encode = to_sun_format(format);
  if (encode == MUS_ERROR) 
    RETURN_ERROR_EXIT(MUS_AUDIO_FORMAT_NOT_AVAILABLE, -1,
              mus_format("format %d (%s) not available",
                 format, 
                 mus_data_format_name(format)));
  if (getenv(AUDIODEV_ENV) != NULL) 
    dev_name = getenv(AUDIODEV_ENV); 
  else dev_name = (char *)DAC_NAME;
  if (dev != MUS_AUDIO_DUPLEX_DEFAULT)
    audio_fd = open(dev_name, O_WRONLY, 0);
  else audio_fd = open(dev_name, O_RDWR, 0);
  if (audio_fd == -1) 
    RETURN_ERROR_EXIT(MUS_AUDIO_CANT_OPEN, -1,
              mus_format("can't open output %s: %s",
                 dev_name, strerror(errno)));
  AUDIO_INITINFO(&info);
  if (dev == MUS_AUDIO_LINE_OUT)
    info.play.port = AUDIO_LINE_OUT;
  else
    {
      if (dev == MUS_AUDIO_SPEAKERS)
    /* OR may not be available */
    info.play.port = AUDIO_SPEAKER | AUDIO_HEADPHONE;
      else 
    info.play.port = AUDIO_SPEAKER;
    }
  info.play.sample_rate = srate; 
  info.play.channels = chans;
  bits = 8 * mus_bytes_per_sample(format);
  info.play.precision = bits;
  info.play.encoding = encode;
  err = ioctl(audio_fd, AUDIO_SETINFO, &info); 
  if (err == -1) 
    {
      ioctl(audio_fd, AUDIO_GETINFO, &info); 

      if ((int)info.play.channels != chans) 
    RETURN_ERROR_EXIT(MUS_AUDIO_CHANNELS_NOT_AVAILABLE, audio_fd,
              mus_format("can't set output %s channels to %d",
                     dev_name, chans));
      
      if (((int)info.play.precision != bits) || 
      ((int)info.play.encoding != encode)) 
    RETURN_ERROR_EXIT(MUS_AUDIO_FORMAT_NOT_AVAILABLE, audio_fd,
              mus_format("can't set output %s format to %d bits, %d encode (%s)",
                     dev_name,
                     bits, encode, 
                     mus_data_format_name(format)));
      
      if ((int)info.play.sample_rate != srate) 
    RETURN_ERROR_EXIT(MUS_AUDIO_CHANNELS_NOT_AVAILABLE, audio_fd,
              mus_format("can't set output %s srate to %d",
                     dev_name, srate));
    }
  /* man audio sez the play.buffer_size field is not currently supported */
  /* but since the default buffer size is 8180! we need ioctl(audio_fd, I_SETSIG, ...) */
#ifdef SUNOS
  ioctl(audio_fd, I_FLUSH, FLUSHR);
#endif
  return(audio_fd);
}

int mus_audio_write(int line, char *buf, int bytes)
{
  if (write(line, buf, bytes) != bytes) 
    RETURN_ERROR_EXIT(MUS_AUDIO_WRITE_ERROR, -1,
              mus_format("write error: %s", strerror(errno)));
  return(MUS_NO_ERROR);
}

int mus_audio_close(int line)
{
  write(line, (char *)NULL, 0);
  close(line);
  return(MUS_NO_ERROR);
}

int mus_audio_read(int line, char *buf, int bytes)
{
  int total = 0;
  char *curbuf;
  /* ioctl(line, AUDIO_DRAIN, NULL) */
  /* this seems to return 8-12 bytes fewer than requested -- perverse! */
  /* should I buffer data internally? */

  /* apparently we need to loop here ... */
  curbuf = buf;
  while (total < bytes)
    {
      int bytes_available;
      ioctl(line, FIONREAD, &bytes_available);
      if (bytes_available > 0)
    {
      int bytes_read;
      if ((total + bytes_available) > bytes) bytes_available = bytes - total;
      bytes_read = read(line, curbuf, bytes_available);
      if (bytes_read > 0)
        {
          total += bytes_read;
          curbuf = (char *)(buf + total);
        }
      /* else return anyway?? */
    }
    }
  return(MUS_NO_ERROR);
}

int mus_audio_open_input(int ur_dev, int srate, int chans, int format, int size)
{
  struct audio_info info;
  int indev, encode, bits, dev, audio_fd, err;
  char *dev_name;
  dev = MUS_AUDIO_DEVICE(ur_dev);
  encode = to_sun_format(format);
  bits = 8 * mus_bytes_per_sample(format);
  if (encode == -1) 
    RETURN_ERROR_EXIT(MUS_AUDIO_FORMAT_NOT_AVAILABLE, -1,
              mus_format("format %d bits, %d encode (%s) not available",
                 bits, encode, 
                 mus_data_format_name(format)));
  if (getenv(AUDIODEV_ENV) != NULL) 
    dev_name = getenv(AUDIODEV_ENV); 
  else dev_name = (char *)DAC_NAME;
  if (dev != MUS_AUDIO_DUPLEX_DEFAULT)
    audio_fd = open(dev_name, O_RDONLY, 0);
  else audio_fd = open(dev_name, O_RDWR, 0);
  if (audio_fd == -1) 
    RETURN_ERROR_EXIT(MUS_AUDIO_CANT_OPEN, -1,
              mus_format("can't open input %s: %s",
                 dev_name, strerror(errno)));
  AUDIO_INITINFO(&info);
  /*  ioctl(audio_fd, AUDIO_GETINFO, &info); */
  info.record.sample_rate = srate;
  info.record.channels = chans;
  err = ioctl(audio_fd, AUDIO_SETINFO, &info); 
  if (err == -1) 
    RETURN_ERROR_EXIT(MUS_AUDIO_CANT_OPEN, audio_fd,
              mus_format("can't set srate %d and chans %d for input %s",
                 srate, chans,
                 dev_name));
  ioctl(audio_fd, AUDIO_GETINFO, &info);
  if (info.record.sample_rate != (unsigned int)srate) 
    mus_print("%s[%d]: sampling rate: %d != %d\n", 
          __FILE__, __LINE__, 
          info.record.sample_rate, srate);
  if (info.record.channels != (unsigned int)chans) 
    mus_print("%s[%d]: channels: %d != %d\n", 
          __FILE__, __LINE__, 
          info.record.channels, chans);

  info.record.precision = bits; /* was play, changed 10-Jul-03 thanks to Jürgen Keil */
  info.record.encoding = encode;
  err = ioctl(audio_fd, AUDIO_SETINFO, &info); 
  if (err == -1) 
    RETURN_ERROR_EXIT(MUS_AUDIO_CANT_OPEN, audio_fd,
              mus_format("can't set bits %d, encode %d (format %s) for input %s",
                 bits, encode, mus_data_format_name(format),
                 dev_name));
  ioctl(audio_fd, AUDIO_GETINFO, &info);

  /* these cannot be OR'd */
  if (dev == MUS_AUDIO_LINE_IN) 
    indev = AUDIO_LINE_IN; 
  else indev = AUDIO_MICROPHONE;
  info.record.port = indev;
  err = ioctl(audio_fd, AUDIO_SETINFO, &info); 
  if (err == -1) 
    RETURN_ERROR_EXIT(MUS_AUDIO_CANT_WRITE, audio_fd,
              mus_format("can't set record.port to %d for %s",
                 indev, dev_name));
  err = ioctl(audio_fd, AUDIO_GETINFO, &info);
  if (err == -1) 
    RETURN_ERROR_EXIT(MUS_AUDIO_CANT_READ, audio_fd,
              mus_format("can't getinfo on input %s (line: %d)",
                 dev_name, 
                 audio_fd));
  else 
    {
      if ((int)info.record.port != indev) 
    RETURN_ERROR_EXIT(MUS_AUDIO_DEVICE_NOT_AVAILABLE, audio_fd,
              mus_format("confusion in record.port: %d != %d (%s)",
                     (int)info.record.port, indev,
                     dev_name));
      if ((int)info.record.channels != chans) 
    RETURN_ERROR_EXIT(MUS_AUDIO_CHANNELS_NOT_AVAILABLE, audio_fd,
              mus_format("confusion in record.channels: %d != %d (%s)",
                     (int)info.record.channels, chans,
                     dev_name));
      if (((int)info.record.precision != bits) || 
      ((int)info.record.encoding != encode)) 
    RETURN_ERROR_EXIT(MUS_AUDIO_FORMAT_NOT_AVAILABLE, audio_fd,
              mus_format("confusion in record.precision|encoding: %d != %d or %d != %d (%s)",
                     (int)info.record.precision, bits,
                     (int)info.record.encoding, encode,
                     dev_name));
    }
  /* this may be a bad idea */
  info.record.buffer_size = size;
  err = ioctl(audio_fd, AUDIO_SETINFO, &info); 
  if (err == -1) 
    RETURN_ERROR_EXIT(MUS_AUDIO_CANT_WRITE, audio_fd,
              mus_format("can't set buffer size to %d on input %s",
                 size,
                 dev_name));
  return(audio_fd);
}

#if 0
/* pause can be implemented with play.pause and record.pause */

static const char *sun_format_name(int format)
{
  switch (format)
    {
#ifdef AUDIO_ENCODING_ALAW
    case AUDIO_ENCODING_ALAW: return("alaw"); break;
#endif
#ifdef AUDIO_ENCODING_ULAW
    case AUDIO_ENCODING_ULAW: return("ulaw"); break;
#endif
#ifdef AUDIO_ENCODING_DVI
    case AUDIO_ENCODING_DVI: return("dvi adpcm"); break;
#endif
#ifdef AUDIO_ENCODING_LINEAR8
    case AUDIO_ENCODING_LINEAR8: return("linear"); break;
#else
  #ifdef AUDIO_ENCODING_PCM8
    case AUDIO_ENCODING_PCM8: return("linear"); break;
  #endif
#endif
#ifdef AUDIO_ENCODING_LINEAR
    case AUDIO_ENCODING_LINEAR: return("linear"); break;
#else
  #ifdef AUDIO_ENCODING_PCM16
    case AUDIO_ENCODING_PCM16: return("linear"); break;
  #endif
#endif
#ifdef AUDIO_ENCODING_NONE
    case AUDIO_ENCODING_NONE: return("not audio"); break; /* dbri interface configured for something else */
#endif      
    }
  return("unknown");
}

static const char *sun_in_device_name(int dev)
{
  if (dev == AUDIO_MICROPHONE) return("microphone");
  if (dev == AUDIO_LINE_IN) return("line in");
  if (dev == AUDIO_INTERNAL_CD_IN) return("cd");
  if (dev == (AUDIO_MICROPHONE | AUDIO_LINE_IN)) return("microphone + line in");
  if (dev == (AUDIO_MICROPHONE | AUDIO_LINE_IN | AUDIO_INTERNAL_CD_IN)) return("microphone + line in + cd");
  if (dev == (AUDIO_MICROPHONE | AUDIO_INTERNAL_CD_IN)) return("microphone + cd");
  if (dev == (AUDIO_LINE_IN | AUDIO_INTERNAL_CD_IN)) return("line in + cd");
  return("unknown");
}

static const char *sun_out_device_name(int dev)
{
  if (dev == AUDIO_SPEAKER) return("speakers");
  if (dev == AUDIO_LINE_OUT) return("line out");
  if (dev == AUDIO_HEADPHONE) return("headphones");
  if (dev == (AUDIO_SPEAKER | AUDIO_LINE_OUT)) return("speakers + line out");
  if (dev == (AUDIO_SPEAKER | AUDIO_LINE_OUT | AUDIO_HEADPHONE)) return("speakers + line out + headphones");
  if (dev == (AUDIO_SPEAKER | AUDIO_HEADPHONE)) return("speakers + headphones");
  if (dev == (AUDIO_LINE_OUT | AUDIO_HEADPHONE)) return("line out + headphones");
  return("unknown");
}


static char *sun_vol_name = NULL;
static char *sun_volume_name(float vol, int balance, int chans)
{
  if (sun_vol_name == NULL) sun_vol_name = (char *)calloc(LABEL_BUFFER_SIZE, sizeof(char));
  if (chans != 2)
    snprintf(sun_vol_name, LABEL_BUFFER_SIZE, "%.3f", vol);
  else 
    {
      snprintf(sun_vol_name, LABEL_BUFFER_SIZE, "%.3f %.3f",
           vol * (float)(AUDIO_RIGHT_BALANCE - balance) / (float)AUDIO_RIGHT_BALANCE,
           vol * (float)balance / (float)AUDIO_RIGHT_BALANCE);
    }
  return(sun_vol_name);
}

#endif
#endif



/* ------------------------------- WINDOZE ----------------------------------------- */

#if defined(_MSC_VER) && (!(defined(__CYGWIN__)))
#define AUDIO_OK 1

#include <windows.h>
#include <mmsystem.h>

#define BUFFER_FILLED 1
#define BUFFER_EMPTY 2

#define OUTPUT_LINE 1
#define INPUT_LINE 2

#define SOUND_UNREADY 0
#define SOUND_INITIALIZED 1
#define SOUND_RUNNING 2

static int buffer_size = 1024;
static int db_state[2];
static int sound_state = 0;
static int current_chans = 1;
static int current_datum_size = 2;
static int current_buf = 0;
WAVEHDR wh[2];
HWAVEOUT fd;
HWAVEIN record_fd;
WAVEHDR rec_wh;
static int rec_state = SOUND_UNREADY;

static MMRESULT win_in_err = 0, win_out_err = 0;
static char errstr[128], getstr[128];

static char *win_err_buf = NULL;
static mus_print_handler_t *old_handler;

static void win_mus_print(char *msg)
{
  if ((win_in_err == 0) && (win_out_err == 0))
    (*old_handler)(msg);
  else
    {
      if (win_in_err) 
    waveInGetErrorText(win_in_err, getstr, PRINT_BUFFER_SIZE);
      else waveOutGetErrorText(win_out_err, getstr, PRINT_BUFFER_SIZE);
      snprintf(errstr, PRINT_BUFFER_SIZE, "%s [%s]", msg, getstr);
      (*old_handler)(errstr);
    }
}

static void start_win_print(void)
{
  if (old_handler != win_mus_print)
    old_handler = mus_print_set_handler(win_mus_print);
}

static void end_win_print(void)
{
  if (old_handler == win_mus_print)
    mus_print_set_handler(NULL);
  else mus_print_set_handler(old_handler);
}

#define RETURN_ERROR_EXIT(Error_Type, Ur_Error_Message) \
  do { char *Error_Message; Error_Message = Ur_Error_Message; \
    if (Error_Message) \
      {MUS_STANDARD_ERROR(Error_Type, Error_Message); free(Error_Message);} \
    else MUS_STANDARD_ERROR(Error_Type, mus_error_type_to_string(Error_Type)); \
    end_win_print(); \
    return(MUS_ERROR); \
  } while (false)



DWORD CALLBACK next_buffer(HWAVEOUT w, UINT msg, DWORD user_data, DWORD p1, DWORD p2)
{
  if (msg == WOM_DONE)
    {
      db_state[current_buf] = BUFFER_EMPTY;
    }
  return(0);
}

int mus_audio_open_output(int ur_dev, int srate, int chans, int format, int size) 
{
  WAVEFORMATEX wf;
  int dev;
  start_win_print();
  dev = MUS_AUDIO_DEVICE(ur_dev);
  wf.nChannels = chans;
  current_chans = chans;
  wf.wFormatTag = WAVE_FORMAT_PCM;
  wf.cbSize = 0;
  if (format == MUS_UBYTE) 
    {
      wf.wBitsPerSample = 8;
      current_datum_size = 1;
    }
  else 
    {
      wf.wBitsPerSample = 16;
      current_datum_size = 2;
    }
  wf.nSamplesPerSec = srate;
  wf.nBlockAlign = chans * current_datum_size;
  wf.nAvgBytesPerSec = wf.nBlockAlign * wf.nSamplesPerSec;
#if _MSC_VER
  win_out_err = waveOutOpen(&fd, WAVE_MAPPER, &wf, (DWORD (*)(HWAVEOUT,UINT,DWORD,DWORD,DWORD))next_buffer, 0, CALLBACK_FUNCTION); 
#else
  win_out_err = waveOutOpen(&fd, WAVE_MAPPER, &wf, (DWORD)next_buffer, 0, CALLBACK_FUNCTION); 
#endif
  /* 0 here = user_data above, other case = WAVE_FORMAT_QUERY */
  if (win_out_err) 
    RETURN_ERROR_EXIT(MUS_AUDIO_DEVICE_NOT_AVAILABLE,
              mus_format("can't open %d", dev));
  waveOutPause(fd);
  if (size <= 0) 
    buffer_size = 1024; 
  else buffer_size = size;
  wh[0].dwBufferLength = buffer_size * current_datum_size;
  wh[0].dwFlags = 0;
  wh[0].dwLoops = 0;
  wh[0].lpData = (char *)calloc(wh[0].dwBufferLength, sizeof(char));
  if ((wh[0].lpData) == 0) 
    {
      waveOutClose(fd); 
      RETURN_ERROR_EXIT(MUS_AUDIO_SIZE_NOT_AVAILABLE,
            mus_format("can't allocate buffer size %d for output %d", buffer_size, dev));
    }
  win_out_err = waveOutPrepareHeader(fd, &(wh[0]), sizeof(WAVEHDR));
  if (win_out_err) 
    {
      free(wh[0].lpData); 
      waveOutClose(fd);  
      RETURN_ERROR_EXIT(MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE,
            mus_format("can't setup output 'header' for %d", dev));
    }
  db_state[0] = BUFFER_EMPTY;
  wh[1].dwBufferLength = buffer_size * current_datum_size;
  wh[1].dwFlags = 0;
  wh[1].dwLoops = 0;
  wh[1].lpData = (char *)calloc(wh[0].dwBufferLength, sizeof(char));
  if ((wh[1].lpData) == 0) 
    {
      free(wh[0].lpData); 
      waveOutClose(fd); 
      RETURN_ERROR_EXIT(MUS_AUDIO_SIZE_NOT_AVAILABLE,
            mus_format("can't allocate buffer size %d for output %d", buffer_size, dev));
    }
  win_out_err = waveOutPrepareHeader(fd, &(wh[1]), sizeof(WAVEHDR));
  if (win_out_err) 
    {
      waveOutUnprepareHeader(fd, &(wh[0]), sizeof(WAVEHDR)); 
      free(wh[0].lpData); 
      free(wh[1].lpData); 
      waveOutClose(fd);  
      RETURN_ERROR_EXIT(MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE,
            mus_format("can't setup output 'header' for %d", dev));
    }
  db_state[1] = BUFFER_EMPTY;
  sound_state = SOUND_INITIALIZED;
  current_buf = 0;
  end_win_print();
  return(OUTPUT_LINE);
}

static MMRESULT fill_buffer(int dbi, char *inbuf, int instart, int bytes)
{
  int i, j;
  win_out_err = 0;
  if (sound_state == SOUND_UNREADY) return(0);
  for (i = instart, j = 0; j < bytes; j++, i++)
    wh[dbi].lpData[j] = inbuf[i];
  wh[dbi].dwBufferLength = bytes;
  db_state[dbi] = BUFFER_FILLED;
  if ((sound_state == SOUND_INITIALIZED) && 
      (dbi == 1))
    {
      sound_state = SOUND_RUNNING;
      win_out_err = waveOutRestart(fd);
    }
  return(win_out_err);
}

static void wait_for_empty_buffer(int buf)
{
  while (db_state[buf] != BUFFER_EMPTY)
    {
      Sleep(1);      /* in millisecs, so even this may be too much if buf = 256 bytes */
    }
}

int mus_audio_write(int line, char *buf, int bytes) 
{
  int lim, leftover, start;
  start_win_print();
  if (line != OUTPUT_LINE) 
    RETURN_ERROR_EXIT(MUS_AUDIO_CANT_WRITE,
              mus_format("write error: line %d != %d?",
                 line, OUTPUT_LINE));
  win_out_err = 0;
  leftover = bytes;
  start = 0;
  if (sound_state == SOUND_UNREADY) 
    {
      end_win_print(); 
      return(MUS_NO_ERROR);
    }
  while (leftover > 0)
    {
      lim = leftover;
      if (lim > buffer_size) lim = buffer_size;
      leftover -= lim;
      wait_for_empty_buffer(current_buf);
      win_out_err = fill_buffer(current_buf, buf, start, lim);
      if (win_out_err) 
    RETURN_ERROR_EXIT(MUS_AUDIO_CANT_WRITE,
              mus_format("write error on %d",
                     line));
      win_out_err = waveOutWrite(fd, &wh[current_buf], sizeof(WAVEHDR));
      if (win_out_err) 
    RETURN_ERROR_EXIT(MUS_AUDIO_CANT_WRITE,
              mus_format("write error on %d",
                     line));
      start += lim;
      current_buf++;
      if (current_buf > 1) current_buf = 0;
    }
  return(MUS_NO_ERROR);
}

static float unlog(unsigned short val)
{
  /* 1.0 linear is 0xffff, rest is said to be "logarithmic", whatever that really means here */
  if (val == 0) return(0.0);
  return((float)val / 65536.0);
  /* return(pow(2.0, amp) - 1.0); */ /* doc seems to be bogus */
}

static char *mixer_status_name(int status)
{
  switch (status)
    {
    case MIXERLINE_LINEF_ACTIVE: return(", (active)"); break;
    case MIXERLINE_LINEF_DISCONNECTED: return(", (disconnected)"); break;
    case MIXERLINE_LINEF_SOURCE: return(", (source)"); break;
    default: return(""); break;
    }
}

static char *mixer_target_name(int type)
{
  switch (type)
    {
    case MIXERLINE_TARGETTYPE_UNDEFINED: return("undefined"); break;
    case MIXERLINE_TARGETTYPE_WAVEOUT: return("output"); break;
    case MIXERLINE_TARGETTYPE_WAVEIN: return("input"); break;
    case MIXERLINE_TARGETTYPE_MIDIOUT: return("midi output"); break;
    case MIXERLINE_TARGETTYPE_MIDIIN: return("midi input"); break;
    case MIXERLINE_TARGETTYPE_AUX: return("aux"); break;
    default: return(""); break;
    }
}

static char *mixer_component_name(int type)
{
  switch (type)
    {
    case MIXERLINE_COMPONENTTYPE_DST_UNDEFINED: return("undefined"); break;
    case MIXERLINE_COMPONENTTYPE_DST_DIGITAL: return("digital"); break;
    case MIXERLINE_COMPONENTTYPE_DST_LINE: return("line"); break;
    case MIXERLINE_COMPONENTTYPE_DST_MONITOR: return("monitor"); break;
    case MIXERLINE_COMPONENTTYPE_DST_SPEAKERS: return("speakers"); break;
    case MIXERLINE_COMPONENTTYPE_DST_HEADPHONES: return("headphones"); break;
    case MIXERLINE_COMPONENTTYPE_DST_TELEPHONE: return("telephone"); break;
    case MIXERLINE_COMPONENTTYPE_DST_WAVEIN: return("wave in"); break;
    case MIXERLINE_COMPONENTTYPE_DST_VOICEIN: return("voice in"); break;
    case MIXERLINE_COMPONENTTYPE_SRC_UNDEFINED: return("undefined"); break;
    case MIXERLINE_COMPONENTTYPE_SRC_DIGITAL: return("digital"); break;
    case MIXERLINE_COMPONENTTYPE_SRC_LINE: return("line"); break;
    case MIXERLINE_COMPONENTTYPE_SRC_MICROPHONE: return("mic"); break;
    case MIXERLINE_COMPONENTTYPE_SRC_SYNTHESIZER: return("synth"); break;
    case MIXERLINE_COMPONENTTYPE_SRC_COMPACTDISC: return("CD"); break;
    case MIXERLINE_COMPONENTTYPE_SRC_TELEPHONE: return("telephone"); break;
    case MIXERLINE_COMPONENTTYPE_SRC_PCSPEAKER: return("speaker"); break;
    case MIXERLINE_COMPONENTTYPE_SRC_WAVEOUT: return("wave out"); break;
    case MIXERLINE_COMPONENTTYPE_SRC_AUXILIARY: return("aux"); break;
    case MIXERLINE_COMPONENTTYPE_SRC_ANALOG: return("analog"); break;
    default: return(""); break;
    }
}

char *mus_audio_moniker(void) {return("MS audio");} /* version number of some sort? */

int mus_audio_initialize(void) 
{
  return(MUS_NO_ERROR);
}

int mus_audio_close(int line) 
{
  int i;
  win_out_err = 0; 
  win_in_err = 0;
  if (line == OUTPUT_LINE)
    {
      /* fill with a few zeros, wait for empty flag */
      if (sound_state != SOUND_UNREADY)
        {
          wait_for_empty_buffer(current_buf);
          for (i = 0; i < 128; i++) wh[current_buf].lpData[i] = 0;
          wait_for_empty_buffer(current_buf);
          win_out_err = waveOutClose(fd);
      i = 0;
          while (win_out_err == WAVERR_STILLPLAYING)
            {
          Sleep(1);
              win_out_err = waveOutClose(fd);
          i++;
          if (i > 1024) break;
            }
          db_state[0] = BUFFER_EMPTY;
          db_state[1] = BUFFER_EMPTY;
          sound_state = SOUND_UNREADY;
          waveOutUnprepareHeader(fd, &(wh[0]), sizeof(WAVEHDR));
          waveOutUnprepareHeader(fd, &(wh[1]), sizeof(WAVEHDR));
          free(wh[0].lpData);
          free(wh[1].lpData);
          if (win_out_err) 
        RETURN_ERROR_EXIT(MUS_AUDIO_CANT_CLOSE,
                  mus_format("close failed on %d",
                     line));
        }
    }
  else 
    {
      if (line == INPUT_LINE)
        {
          if (rec_state != SOUND_UNREADY)
            {
              waveInReset(record_fd);
              waveInClose(record_fd);
              waveInUnprepareHeader(record_fd, &rec_wh, sizeof(WAVEHDR));
              if (rec_wh.lpData) 
        {
          free(rec_wh.lpData);
          rec_wh.lpData = NULL;
        }
              rec_state = SOUND_UNREADY;
            }
        }
      else 
    RETURN_ERROR_EXIT(MUS_AUDIO_CANT_CLOSE,
              mus_format("can't close unrecognized line %d",
                     line));
    }
  return(MUS_NO_ERROR);
}

  /*
   * waveInAddBuffer sends buffer to get data
   * MM_WIM_DATA lParam->WAVEHDR dwBytesRecorded =>how much data actually in buffer
   */

static int current_record_chans = 0, current_record_datum_size = 0;

DWORD CALLBACK next_input_buffer(HWAVEIN w, UINT msg, DWORD user_data, DWORD p1, DWORD p2)
{
  if (msg == WIM_DATA)
    {
      /* grab data */
      /* p1->dwBytesRecorded */
    }
  return(0);
}

int mus_audio_open_input(int ur_dev, int srate, int chans, int format, int size) 
{
  WAVEFORMATEX wf;
  int dev;
  win_in_err = 0;
  dev = MUS_AUDIO_DEVICE(ur_dev);
  wf.nChannels = chans;
  current_record_chans = chans;

  wf.wFormatTag = WAVE_FORMAT_PCM;
  wf.cbSize = 0;
  if (format == MUS_UBYTE) 
    {
      wf.wBitsPerSample = 8;
      current_record_datum_size = 1;
    }
  else 
    {
      wf.wBitsPerSample = 16;
      current_record_datum_size = 2;
    }
  wf.nSamplesPerSec = srate;
  wf.nBlockAlign = chans * current_datum_size;
  wf.nAvgBytesPerSec = wf.nBlockAlign * wf.nSamplesPerSec;

  rec_wh.dwBufferLength = size * current_record_datum_size;
  rec_wh.dwFlags = 0;
  rec_wh.dwLoops = 0;
  rec_wh.lpData = (char *)calloc(rec_wh.dwBufferLength, sizeof(char));
  if ((rec_wh.lpData) == 0) 
    RETURN_ERROR_EXIT(MUS_AUDIO_SIZE_NOT_AVAILABLE,
              mus_format("can't allocated %d bytes for input buffer of %d", size, dev));
#if _MSC_VER
  win_in_err = waveInOpen(&record_fd, WAVE_MAPPER, &wf, (DWORD (*)(HWAVEIN,UINT,DWORD,DWORD,DWORD))next_input_buffer, 0, CALLBACK_FUNCTION);
  /* why isn't the simple cast (DWORD) correct here as below? -- the docs say the 4th arg's type is DWORD */
#else
  win_in_err = waveInOpen(&record_fd, WAVE_MAPPER, &wf, (DWORD)next_input_buffer, 0, CALLBACK_FUNCTION);
#endif
  if (win_in_err) 
    {
      free(rec_wh.lpData);
      RETURN_ERROR_EXIT(MUS_AUDIO_DEVICE_NOT_AVAILABLE,
            mus_format("can't open input device %d", dev));
    }
  win_in_err = waveInPrepareHeader(record_fd, &(rec_wh), sizeof(WAVEHDR));
  if (win_in_err) 
    {
      free(rec_wh.lpData);
      waveInClose(record_fd);
      RETURN_ERROR_EXIT(MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE,
            mus_format("can't prepare input 'header' for %d", dev));
    }
  return(MUS_NO_ERROR);
}

int mus_audio_read(int line, char *buf, int bytes) 
{
  win_in_err = 0;
  return(MUS_ERROR);
}

#endif



/* ------------------------------- Mac OSX ----------------------------------------- */

/* this code based primarily on the CoreAudio headers and portaudio pa_mac_core.c,
 *   and to a much lesser extent, coreaudio.pdf and the HAL/Daisy examples.
 */

#ifdef __APPLE__
#define AUDIO_OK 1

#include <AvailabilityMacros.h>
#define HAVE_AUDIODEVICEDESTROYIOPROCID (defined(MAC_OS_X_VERSION_10_5))

/*
#include <CoreServices/CoreServices.h>
#include <CoreAudio/CoreAudio.h>
*/
/* ./System/Library/Frameworks/CoreAudio.framework/Headers/CoreAudio.h */

static const char* osx_error(OSStatus err) 
{
  if (err == noErr) return("no error");
  switch (err) 
    {
    case kAudioHardwareNoError:               return("no error");                         break;
    case kAudioHardwareUnspecifiedError:      return("unspecified audio hardware error"); break;
    case kAudioHardwareNotRunningError:       return("audio hardware not running");       break;
    case kAudioHardwareUnknownPropertyError:  return("unknown property");                 break;
    case kAudioHardwareBadPropertySizeError:  return("bad property");                     break;
    case kAudioHardwareBadDeviceError:        return("bad device");                       break;
    case kAudioHardwareBadStreamError:        return("bad stream");                       break;
    case kAudioHardwareIllegalOperationError: return("illegal operation");                break;
    case kAudioDeviceUnsupportedFormatError:  return("unsupported format");               break;
    case kAudioDevicePermissionsError:        return("device permissions error");         break;
    }
  return("unknown error");
}

#define MAX_BUFS 4
static char **bufs = NULL;
static unsigned int in_buf = 0, out_buf = 0;

static OSStatus writer(AudioDeviceID inDevice, 
               const AudioTimeStamp *inNow, 
               const AudioBufferList *InputData, const AudioTimeStamp *InputTime, 
               AudioBufferList *OutputData, const AudioTimeStamp *OutputTime, 
               void *appGlobals)
{
  AudioBuffer abuf;
  char *aplbuf, *sndbuf;
  abuf = OutputData->mBuffers[0];
  aplbuf = (char *)(abuf.mData);
  sndbuf = bufs[out_buf];
  memmove((void *)aplbuf, (void *)sndbuf, abuf.mDataByteSize);
  out_buf++;
  if (out_buf >= MAX_BUFS) out_buf = 0;
  return(noErr);
}

static OSStatus reader(AudioDeviceID inDevice, 
               const AudioTimeStamp *inNow, 
               const AudioBufferList *InputData, const AudioTimeStamp *InputTime, 
               AudioBufferList *OutputData, const AudioTimeStamp *OutputTime, 
               void *appGlobals)
{
  AudioBuffer abuf;
  char *aplbuf, *sndbuf;
  abuf = InputData->mBuffers[0];
  aplbuf = (char *)(abuf.mData);
  sndbuf = bufs[out_buf];
  memmove((void *)sndbuf, (void *)aplbuf, abuf.mDataByteSize);
  out_buf++;
  if (out_buf >= MAX_BUFS) out_buf = 0;
  return(noErr);
}


static AudioDeviceID device = kAudioDeviceUnknown;
static bool writing = false, open_for_input = false;

#if HAVE_AUDIODEVICEDESTROYIOPROCID
  static AudioDeviceIOProcID read_procId, write_procId;
#endif 

int mus_audio_close(int line) 
{
  OSStatus err = noErr;
  UInt32 sizeof_running;
  UInt32 running;
  if (open_for_input)
    {
      in_buf = 0;
      err = AudioDeviceStop(device, (AudioDeviceIOProc)reader);
      if (err == noErr) 
#if HAVE_AUDIODEVICEDESTROYIOPROCID
    err = AudioDeviceDestroyIOProcID(device, read_procId);
#else
        err = AudioDeviceRemoveIOProc(device, (AudioDeviceIOProc)reader);
#endif
    }
  else
    {
      if ((in_buf > 0) && (!writing))
    {
      /* short enough sound that we never got started? */
#if HAVE_AUDIODEVICEDESTROYIOPROCID
      err = AudioDeviceCreateIOProcID(device, (AudioDeviceIOProc)writer, NULL, &write_procId);
#else
      err = AudioDeviceAddIOProc(device, (AudioDeviceIOProc)writer, NULL);
#endif
      if (err == noErr)
        err = AudioDeviceStart(device, (AudioDeviceIOProc)writer); /* writer will be called right away */
      if (err == noErr)
        writing = true;
    }
      if (writing)
    {
      /* send out waiting buffers */
      sizeof_running = sizeof(UInt32);
      while (in_buf == out_buf)
        {
          /* err = AudioDeviceGetProperty(device, 0, false, kAudioDevicePropertyDeviceIsRunning, &sizeof_running, &running); */
          {
        AudioObjectPropertyAddress device_address = { kAudioDevicePropertyDeviceIsRunning,
                                  kAudioDevicePropertyScopeOutput,
                                  kAudioObjectPropertyElementMaster };
        err = AudioObjectGetPropertyData(device, &device_address, 0, NULL, &sizeof_running, &running);
          }	      
        }
      while (in_buf != out_buf)
        {
          /* err = AudioDeviceGetProperty(device, 0, false, kAudioDevicePropertyDeviceIsRunning, &sizeof_running, &running); */
          {
        AudioObjectPropertyAddress device_address = { kAudioDevicePropertyDeviceIsRunning,
                                  kAudioDevicePropertyScopeOutput,
                                  kAudioObjectPropertyElementMaster };
        err = AudioObjectGetPropertyData(device, &device_address, 0, NULL, &sizeof_running, &running);
          }
        }
      in_buf = 0;
      err = AudioDeviceStop(device, (AudioDeviceIOProc)writer);
      if (err == noErr) 
#if HAVE_AUDIODEVICEDESTROYIOPROCID
        err = AudioDeviceDestroyIOProcID(device, write_procId);
#else
        err = AudioDeviceRemoveIOProc(device, (AudioDeviceIOProc)writer);
#endif
      writing = false;
    }
    }
  device = kAudioDeviceUnknown;
  if (err == noErr)
    return(MUS_NO_ERROR);
  return(MUS_ERROR);
}

typedef enum {CONVERT_NOT, CONVERT_COPY, CONVERT_SKIP, CONVERT_COPY_AND_SKIP, CONVERT_SKIP_N, CONVERT_COPY_AND_SKIP_N} audio_convert_t;
static audio_convert_t conversion_choice = CONVERT_NOT;
static float conversion_multiplier = 1.0;
static int dac_out_chans, dac_out_srate;
static int incoming_out_chans = 1, incoming_out_srate = 44100;
static unsigned int fill_point = 0;
static unsigned int bufsize = 0, current_bufsize = 0;
static bool match_dac_to_sound = true;


bool mus_audio_output_properties_mutable(bool mut)
{
  match_dac_to_sound = mut;
  return(mut);
}


/* I'm getting bogus buffer sizes from the audio conversion stuff from Apple,
 *   and I think AudioConvert doesn't handle cases like 4->6 chans correctly
 *   so, I'll just do the conversions myself -- there is little need here
 *   for non-integer srate conversion anyway, and the rest is trivial.
 */

int mus_audio_open_output(int dev, int srate, int chans, int format, int size) 
{
  OSStatus err = noErr;
  UInt32 sizeof_device, sizeof_format, sizeof_bufsize;
  AudioStreamBasicDescription device_desc;

  device = 0;
  sizeof_device = sizeof(AudioDeviceID);
  sizeof_bufsize = sizeof(unsigned int);

  /* err = AudioHardwareGetProperty(kAudioHardwarePropertyDefaultOutputDevice, &sizeof_device, (void *)(&device)); */
  {
    AudioObjectPropertyAddress device_address = { kAudioHardwarePropertyDefaultOutputDevice,
                          kAudioObjectPropertyScopeGlobal,
                          kAudioObjectPropertyElementMaster };
    err = AudioObjectGetPropertyData(kAudioObjectSystemObject, &device_address, 0, NULL, &sizeof_device, &device);
  }

  bufsize = 4096;
  if (err == noErr) 
    {
      /* err = AudioDeviceGetProperty(device, 0, false, kAudioDevicePropertyBufferSize, &sizeof_bufsize, &bufsize); */
      {
    AudioObjectPropertyAddress device_address = { kAudioDevicePropertyBufferSize,
                              kAudioDevicePropertyScopeOutput,
                              kAudioObjectPropertyElementMaster };
    err = AudioObjectGetPropertyData(device, &device_address, 0, NULL, &sizeof_bufsize, &bufsize);
      }
    }
  if (err != noErr) 
    {
      fprintf(stderr, "open audio output err: %d %s\n", (int)err, osx_error(err));
      return(MUS_ERROR);
    }

  sizeof_format = sizeof(AudioStreamBasicDescription);
  /* err = AudioDeviceGetProperty(device, 0, false, kAudioDevicePropertyStreamFormat, &sizeof_format, &device_desc); */
  {
    AudioObjectPropertyAddress device_address = { kAudioDevicePropertyStreamFormat,
                          kAudioDevicePropertyScopeOutput,
                          kAudioObjectPropertyElementMaster };
    err = AudioObjectGetPropertyData(device, &device_address, 0, NULL, &sizeof_format, &device_desc);
  }

  if (err != noErr)
    {
      fprintf(stderr, "open audio output (get device format) err: %d %s\n", (int)err, osx_error(err));
      return(MUS_ERROR);
    }

  if (match_dac_to_sound)
    {
      /* now check for srate/chan mismatches and so on */
      
      /* current DAC state: device_desc.mChannelsPerFrame, (int)(device_desc.mSampleRate) */
      /* apparently get stream format can return noErr but chans == 0?? */

      if (((int)device_desc.mChannelsPerFrame != chans) || 
      ((int)(device_desc.mSampleRate) != srate))
    {
      /* try to match DAC settings to current sound */
      device_desc.mChannelsPerFrame = chans;
      device_desc.mSampleRate = srate;
      device_desc.mBytesPerPacket = chans * 4; /* assume 1 frame/packet and float32 data */
      device_desc.mBytesPerFrame = chans * 4;
      sizeof_format = sizeof(AudioStreamBasicDescription);
      /* err = AudioDeviceSetProperty(device, 0, 0, false, kAudioDevicePropertyStreamFormat, sizeof_format, &device_desc); */
      {
        AudioObjectPropertyAddress device_address = { kAudioDevicePropertyStreamFormat,
                              kAudioDevicePropertyScopeOutput,
                              kAudioObjectPropertyElementMaster };
        err = AudioObjectSetPropertyData(device, &device_address, 0, NULL, sizeof_format, &device_desc);
      }
      
      /* this error is bogus in some cases -- other audio systems just ignore it,
       *   but in my case (a standard MacIntel with no special audio hardware), if I leave
       *   this block out, the sound is played back at the wrong rate, and the volume
       *   of outa is set to 0.0?? 
       */
      
      if (err != noErr)
        {
          /* it must have failed for some reason -- look for closest match available */
          /* if srate = 22050 try 44100, if chans = 1 try 2 */
          /* the "get closest match" business appears to be completely bogus... */

          device_desc.mChannelsPerFrame = (chans == 1) ? 2 : chans;
          device_desc.mSampleRate = (srate == 22050) ? 44100 : srate;
          device_desc.mBytesPerPacket = device_desc.mChannelsPerFrame * 4; /* assume 1 frame/packet and float32 data */
          device_desc.mBytesPerFrame = device_desc.mChannelsPerFrame * 4;
          sizeof_format = sizeof(AudioStreamBasicDescription);
          /* err = AudioDeviceSetProperty(device, 0, 0, false, kAudioDevicePropertyStreamFormat, sizeof_format, &device_desc); */
          {
        AudioObjectPropertyAddress device_address = { kAudioDevicePropertyStreamFormat,
                                  kAudioDevicePropertyScopeOutput,
                                  kAudioObjectPropertyElementMaster };
        err = AudioObjectSetPropertyData(device, &device_address, 0, NULL, sizeof_format, &device_desc);
          }
          if (err != noErr)
        {
          sizeof_format = sizeof(AudioStreamBasicDescription);
          /* err = AudioDeviceGetProperty(device, 0, false, kAudioDevicePropertyStreamFormatMatch, &sizeof_format, &device_desc); */
          {
            AudioObjectPropertyAddress device_address = { kAudioDevicePropertyStreamFormatMatch,
                                  kAudioDevicePropertyScopeOutput,
                                  kAudioObjectPropertyElementMaster };
            err = AudioObjectGetPropertyData(device, &device_address, 0, NULL, &sizeof_format, &device_desc);
          }

          if (err == noErr)
            {
              /* match suggests: device_desc.mChannelsPerFrame, (int)(device_desc.mSampleRate) */
              /* try to set DAC to reflect that match */
              /* a bug here in emagic 2|6 -- we can get 6 channel match, but then can't set it?? */

              sizeof_format = sizeof(AudioStreamBasicDescription);
              /* err = AudioDeviceSetProperty(device, 0, 0, false, kAudioDevicePropertyStreamFormat, sizeof_format, &device_desc); */
              {
            AudioObjectPropertyAddress device_address = { kAudioDevicePropertyStreamFormat,
                                      kAudioDevicePropertyScopeOutput,
                                      kAudioObjectPropertyElementMaster };
            err = AudioObjectSetPropertyData(device, &device_address, 0, NULL, sizeof_format, &device_desc);
              }
              if (err != noErr) 
            {
              /* no luck -- get current DAC settings at least */
              sizeof_format = sizeof(AudioStreamBasicDescription);
              /* AudioDeviceGetProperty(device, 0, false, kAudioDevicePropertyStreamFormat, &sizeof_format, &device_desc); */
              {
                AudioObjectPropertyAddress device_address = { kAudioDevicePropertyStreamFormat,
                                      kAudioDevicePropertyScopeOutput,
                                      kAudioObjectPropertyElementMaster };
                err = AudioObjectGetPropertyData(device, &device_address, 0, NULL, &sizeof_format, &device_desc);
              }
            }
            }
        }
          else 
        {
          /* nothing matches? -- get current DAC settings */
          sizeof_format = sizeof(AudioStreamBasicDescription);
          /* AudioDeviceGetProperty(device, 0, false, kAudioDevicePropertyStreamFormat, &sizeof_format, &device_desc); */
          {
            AudioObjectPropertyAddress device_address = { kAudioDevicePropertyStreamFormat,
                                  kAudioDevicePropertyScopeOutput,
                                  kAudioObjectPropertyElementMaster };
            err = AudioObjectGetPropertyData(device, &device_address, 0, NULL, &sizeof_format, &device_desc);
          }
        }
        }
    }
    } /* end mismatch check */

  /* now DAC claims it is ready for device_desc.mChannelsPerFrame, (int)(device_desc.mSampleRate) */
  dac_out_chans = device_desc.mChannelsPerFrame; /* use better variable names */
  dac_out_srate = (int)(device_desc.mSampleRate);

  open_for_input = false;
  if ((bufs == NULL) || (bufsize > current_bufsize))
    {
      int i;
      if (bufs)
    {
      for (i = 0; i < MAX_BUFS; i++) free(bufs[i]);
      free(bufs);
    }
      bufs = (char **)calloc(MAX_BUFS, sizeof(char *));
      for (i = 0; i < MAX_BUFS; i++)
    bufs[i] = (char *)calloc(bufsize, sizeof(char));
      current_bufsize = bufsize;
    }

  in_buf = 0;
  out_buf = 0;
  fill_point = 0;

  if (!match_dac_to_sound)
    {
      incoming_out_srate = dac_out_srate;
      incoming_out_chans = dac_out_chans;
      conversion_choice = CONVERT_NOT;
      conversion_multiplier = 1.0;
      return(MUS_NO_ERROR);
    }

  incoming_out_srate = srate;
  incoming_out_chans = chans;

  if (incoming_out_chans == dac_out_chans)
    {
      if (incoming_out_srate == dac_out_srate)
    {
      conversion_choice = CONVERT_NOT;
      conversion_multiplier = 1.0;
    }
      else 
    {
      /* here we don't get very fancy -- assume dac/2=in */
      conversion_choice = CONVERT_COPY;
      conversion_multiplier = 2.0;
    }
    }
  else
    {
      if (incoming_out_srate == dac_out_srate)
    {
      if ((dac_out_chans == 2) && (incoming_out_chans == 1)) /* the usual case */
        {
          conversion_choice = CONVERT_SKIP;
          conversion_multiplier = 2.0;
        }
      else
        {
          conversion_choice = CONVERT_SKIP_N;
          conversion_multiplier = ((float)dac_out_chans / (float)incoming_out_chans);
        }
    }
      else 
    {
      if ((dac_out_chans == 2) && (incoming_out_chans == 1)) /* the usual case */
        {
          conversion_choice = CONVERT_COPY_AND_SKIP;
          conversion_multiplier = 4.0;
        }
      else
        {
          conversion_choice = CONVERT_COPY_AND_SKIP_N;
          conversion_multiplier = ((float)dac_out_chans / (float)incoming_out_chans) * 2;
        }
    }
    }
  return(MUS_NO_ERROR);
}


static void convert_incoming(char *to_buf, int fill_point, int lim, char *buf)
{
  int i, j, k, jc, kc, ic;
  switch (conversion_choice)
    {
    case CONVERT_NOT:
      /* no conversion needed */
      for (i = 0; i < lim; i++)
    to_buf[i + fill_point] = buf[i];
      break;

    case CONVERT_COPY:
      /* copy sample to mimic lower srate */
      for (i = 0, j = fill_point; i < lim; i += 8, j += 16)
    for (k = 0; k < 8; k++)
      {
        to_buf[j + k] = buf[i + k];
        to_buf[j + k + 8] = buf[i + k];
      }
      break;

    case CONVERT_SKIP:
      /* skip sample for empty chan */
      for (i = 0, j = fill_point; i < lim; i += 4, j += 8)
    for (k = 0; k < 4; k++)
      {
        to_buf[j + k] = buf[i + k];
        to_buf[j + k + 4] = 0;
      }
      break;

    case CONVERT_SKIP_N:
      /* copy incoming_out_chans then skip up to dac_out_chans */
      jc = dac_out_chans * 4;
      ic = incoming_out_chans * 4;
      for (i = 0, j = fill_point; i < lim; i += ic, j += jc)
    {
      for (k = 0; k < ic; k++) to_buf[j + k] = buf[i + k];
      for (k = ic; k < jc; k++) to_buf[j + k] = 0;
    }
      break;

    case CONVERT_COPY_AND_SKIP:
      for (i = 0, j = fill_point; i < lim; i += 4, j += 16)
    for (k = 0; k < 4; k++)
      {
        to_buf[j + k] = buf[i + k];
        to_buf[j + k + 4] = 0;
        to_buf[j + k + 8] = buf[i + k];
        to_buf[j + k + 12] = 0;
      }
      break;

    case CONVERT_COPY_AND_SKIP_N:
      /* copy for each active chan, skip rest */
      jc = dac_out_chans * 8;
      ic = incoming_out_chans * 4;
      kc = dac_out_chans * 4;
      for (i = 0, j = fill_point; i < lim; i += ic, j += jc)
    {
      for (k = 0; k < ic; k++) 
        {
          to_buf[j + k] = buf[i + k];
          to_buf[j + k + kc] = buf[i + k];	      
        }
      for (k = ic; k < kc; k++) 
        {
          to_buf[j + k] = 0;
          to_buf[j + k + kc] = 0;
        }
    }
      break;
    }
}


int mus_audio_write(int line, char *buf, int bytes) 
{
  OSStatus err = noErr;
  unsigned int lim, bp, out_bytes;
  UInt32 sizeof_running;
  UInt32 running;
  char *to_buf;

  to_buf = bufs[in_buf];
  out_bytes = (unsigned int)(bytes * conversion_multiplier);
  if ((fill_point + out_bytes) > bufsize)
    out_bytes = bufsize - fill_point;
  lim = (unsigned int)(out_bytes / conversion_multiplier);

  if (!writing)
    {
      convert_incoming(to_buf, fill_point, lim, buf);
      fill_point += out_bytes;
      if (fill_point >= bufsize)
    {
      in_buf++;
      fill_point = 0;
      if (in_buf == MAX_BUFS)
        {
          in_buf = 0;
#if HAVE_AUDIODEVICEDESTROYIOPROCID
          err = AudioDeviceCreateIOProcID(device, (AudioDeviceIOProc)writer, NULL, &write_procId);
#else
          err = AudioDeviceAddIOProc(device, (AudioDeviceIOProc)writer, NULL);
#endif
          if (err == noErr)
        err = AudioDeviceStart(device, (AudioDeviceIOProc)writer); /* writer will be called right away */
          if (err == noErr)
        {
          writing = true;
          return(MUS_NO_ERROR);
        }
          else return(MUS_ERROR);
        }
    }
      return(MUS_NO_ERROR);
    }
  if ((fill_point == 0) && (in_buf == out_buf))
    {
      bp = out_buf;
      sizeof_running = sizeof(UInt32);
      while (bp == out_buf)
    {
      /* i.e. just kill time without hanging */
      /* err = AudioDeviceGetProperty(device, 0, false, kAudioDevicePropertyDeviceIsRunning, &sizeof_running, &running); */
      {
        AudioObjectPropertyAddress device_address = { kAudioDevicePropertyDeviceIsRunning,
                              kAudioDevicePropertyScopeOutput,
                              kAudioObjectPropertyElementMaster };
        err = AudioObjectGetPropertyData(device, &device_address, 0, NULL, &sizeof_running, &running);
      }
      /* usleep(10); */
    }
    }
  to_buf = bufs[in_buf];
  if (fill_point == 0) memset((void *)to_buf, 0, bufsize);
  convert_incoming(to_buf, fill_point, lim, buf);
  fill_point += out_bytes;
  if (fill_point >= bufsize)
    {
      in_buf++;
      fill_point = 0;
      if (in_buf >= MAX_BUFS) in_buf = 0;
    }
  return(MUS_NO_ERROR);
}

int mus_audio_open_input(int dev, int srate, int chans, int format, int size) 
{
  OSStatus err = noErr;
  UInt32 sizeof_device;
  UInt32 sizeof_bufsize;

  sizeof_device = sizeof(AudioDeviceID);
  sizeof_bufsize = sizeof(unsigned int);

  device = 0;
  /* err = AudioHardwareGetProperty(kAudioHardwarePropertyDefaultInputDevice, &sizeof_device, (void *)(&device)); */
  {
    AudioObjectPropertyAddress device_address = { kAudioHardwarePropertyDefaultInputDevice,
                          kAudioObjectPropertyScopeGlobal,
                          kAudioObjectPropertyElementMaster };
    err = AudioObjectGetPropertyData(kAudioObjectSystemObject, &device_address, 0, NULL, &sizeof_device, &device);
  }

  bufsize = 4096;
  if (err == noErr) 
    {
      /* err = AudioDeviceGetProperty(device, 0, true, kAudioDevicePropertyBufferSize, &sizeof_bufsize, &bufsize); */
      {
    AudioObjectPropertyAddress device_address = { kAudioDevicePropertyBufferSize,
                              kAudioDevicePropertyScopeInput,
                              kAudioObjectPropertyElementMaster };
    err = AudioObjectGetPropertyData(device, &device_address, 0, NULL, &sizeof_bufsize, &bufsize);
      }
    }
  if (err != noErr) 
    {
      fprintf(stderr, "open audio input err: %d %s\n", (int)err, osx_error(err));
      return(MUS_ERROR);
    }
  open_for_input = true;
  /* assume for now that recorder (higher level) will enforce match */
  if ((bufs == NULL) || (bufsize > current_bufsize))
    {
      int i;
      if (bufs)
    {
      for (i = 0; i < MAX_BUFS; i++) free(bufs[i]);
      free(bufs);
    }
      bufs = (char **)calloc(MAX_BUFS, sizeof(char *));
      for (i = 0; i < MAX_BUFS; i++)
    bufs[i] = (char *)calloc(bufsize, sizeof(char));
      current_bufsize = bufsize;
    }
  in_buf = 0;
  out_buf = 0;
  fill_point = 0;
  incoming_out_srate = srate;
  incoming_out_chans = chans;

#if HAVE_AUDIODEVICEDESTROYIOPROCID
  err = AudioDeviceCreateIOProcID(device, (AudioDeviceIOProc)reader, NULL, &read_procId);
#else
  err = AudioDeviceAddIOProc(device, (AudioDeviceIOProc)reader, NULL);
#endif

  if (err == noErr)
    err = AudioDeviceStart(device, (AudioDeviceIOProc)reader);
  if (err != noErr) 
    {
      fprintf(stderr, "add open audio input err: %d %s\n", (int)err, osx_error(err));
      return(MUS_ERROR);
    }
  return(MUS_NO_ERROR);
}

int mus_audio_read(int line, char *buf, int bytes) 
{
  OSStatus err = noErr;
  unsigned int bp;
  UInt32 sizeof_running;
  UInt32 running;
  char *to_buf;

  if (in_buf == out_buf)
    {
      bp = out_buf;
      sizeof_running = sizeof(UInt32);
      while (bp == out_buf)
    {
      /* err = AudioDeviceGetProperty(device, 0, true, kAudioDevicePropertyDeviceIsRunning, &sizeof_running, &running); */
      {
        AudioObjectPropertyAddress device_address = { kAudioDevicePropertyDeviceIsRunning,
                              kAudioDevicePropertyScopeInput,
                              kAudioObjectPropertyElementMaster };
        err = AudioObjectGetPropertyData(device, &device_address, 0, NULL, &sizeof_running, &running);
      }
      if (err != noErr) 
        fprintf(stderr, "wait err: %s ", osx_error(err));
    }
    }
  to_buf = bufs[in_buf];
  if (bytes <= (int)bufsize)
    memmove((void *)buf, (void *)to_buf, bytes);
  else memmove((void *)buf, (void *)to_buf, bufsize);
  in_buf++;
  if (in_buf >= MAX_BUFS) in_buf = 0;
  return(MUS_ERROR);
}

int mus_audio_initialize(void) {return(MUS_NO_ERROR);}

char *mus_audio_moniker(void) {return((char *)"Mac OSX audio");}
#endif




/* ------------------------------- JACK ----------------------------------------- */

/* Kjetil S. Matheussen. k.s.matheussen@notam02.no */
/* Based on code from ceres. */

#if MUS_JACK
#define AUDIO_OK 1
#include <pthread.h>
#include <jack/jack.h>
#include <samplerate.h>
#include <sys/mman.h>
#include <signal.h>

#if MUS_LITTLE_ENDIAN
#  define MUS_COMP_SHORT MUS_LSHORT
#  define MUS_COMP_FLOAT MUS_LFLOAT
#else
#  define MUS_COMP_SHORT MUS_BSHORT
#  define MUS_COMP_FLOAT MUS_BFLOAT
#endif

#define SRC_QUALITY SRC_SINC_BEST_QUALITY

#if defined(__i386__) || defined(__x86_64)

static inline void __attribute__ ((__unused__)) atomic_add(volatile int* __mem, int __val)
{
  __asm__ __volatile__ ("lock; addl %1,%0"
            : "=m" (*__mem) : "ir" (__val), "m" (*__mem));
}

#elif defined(__powerpc__) || defined(__ppc__)

#ifdef __PPC405__ 
#define _STWCX "sync \n\tstwcx. " 
#else 
#define _STWCX "stwcx. " 
#endif 

static inline void __attribute__ ((__unused__)) atomic_add(volatile int* __mem, int __val)
{
  int __tmp;
  __asm__ __volatile__ (
    "/* Inline atomic add */\n"
    "0:\t"
    "lwarx    %0,0,%2 \n\t"
    "add%I3   %0,%0,%3 \n\t"
    _STWCX "  %0,0,%2 \n\t"
    "bne-     0b \n\t"
    "/* End atomic add */"
    : "=&b"(__tmp), "=m" (*__mem)
    : "r" (__mem), "Ir"(__val), "m" (*__mem)
    : "cr0");
}
#else
#error "Seems like an unsupported hardware for jack. Please contact k.s.matheussen@notam02.no"
#endif
 
 
/*************/
/* Jack Part */
/*************/

#define SNDJACK_BUFFERSIZE 32768

typedef jack_default_audio_sample_t sample_t;
typedef jack_nframes_t nframes_t;

struct SndjackChannel{
  jack_port_t *port;
  sample_t *buffer;
};

static jack_client_t *sndjack_client = NULL;


/*************************/
/* Variables for reading */
/*************************/
static int sndjack_num_read_channels_allocated=0;
static int sndjack_num_read_channels_inuse=0;
static struct SndjackChannel *sndjack_read_channels=NULL;
static pthread_cond_t sndjack_read_cond= PTHREAD_COND_INITIALIZER;
static pthread_mutex_t sndjack_read_mutex= PTHREAD_MUTEX_INITIALIZER;
static int sj_r_buffersize=0;
static int sj_r_writeplace=0;
static int sj_r_readplace=0;
static int sj_r_unread=0;
static int sj_r_xrun=0;
static int sj_r_totalxrun=0;

/*************************/
/* Variables for writing */
/*************************/
static pthread_cond_t sndjack_cond= PTHREAD_COND_INITIALIZER;
static pthread_mutex_t sndjack_mutex=  PTHREAD_MUTEX_INITIALIZER;

enum{SJ_STOPPED,SJ_RUNNING,SJ_ABOUTTOSTOP};

// Variables for the ringbuffer:
static  int sj_writeplace=0;
static  int sj_readplace=0;
static  int sj_unread=0;
static  int sj_buffersize;
static int sj_jackbuffersize; // number of frames sent to sndjack_process.
static int sj_totalxrun=0;
static int sj_xrun=0;
static int sj_status=SJ_STOPPED;

static int sndjack_num_channels_allocated=0;
static int sndjack_num_channels_inuse=0;
static struct SndjackChannel *sndjack_channels=NULL;
static int sndjack_read_format;

static SRC_STATE **sndjack_srcstates;
static double sndjack_srcratio=1.0;

static int jack_mus_watchdog_counter=0;


#define SJ_MAX(a,b) (((a)>(b))?(a):(b))

static void sndjack_read_process(jack_nframes_t nframes){
  int i,ch;
  sample_t *out[sndjack_num_channels_allocated];

  if (sndjack_num_read_channels_inuse==0) return;

  for (ch=0;ch<sndjack_num_read_channels_allocated;ch++){
    out[ch]=(sample_t*)jack_port_get_buffer(sndjack_read_channels[ch].port,nframes);
  }

  for (i=0;i<nframes;i++){
    if (sj_r_unread==sj_buffersize){
      sj_r_xrun+=nframes-i;
      goto exit;
    }
    for (ch=0;ch<sndjack_num_read_channels_inuse;ch++)
      sndjack_read_channels[ch].buffer[sj_r_writeplace]=out[ch][i];
    atomic_add(&sj_r_unread,1);
    sj_r_writeplace++;
    if (sj_r_writeplace==sj_r_buffersize)
      sj_r_writeplace=0;
  }
 exit:
  pthread_cond_broadcast(&sndjack_read_cond);
}


static void sndjack_write_process(jack_nframes_t nframes){
  int ch,i;
  sample_t *out[sndjack_num_channels_allocated];

  for (ch=0;ch<sndjack_num_channels_allocated;ch++){
    out[ch]=(sample_t*)jack_port_get_buffer(sndjack_channels[ch].port,nframes);
  }

  if (sj_status==SJ_STOPPED){
    for (ch=0;ch<sndjack_num_channels_allocated;ch++){
      memset(out[ch],0,nframes*sizeof(sample_t));
    }
  }else{

    // First null out unused channels, if any.
    if (sndjack_num_channels_inuse==1 && sndjack_num_channels_allocated>=2){
      for (ch=2;ch<sndjack_num_channels_allocated;ch++){
    memset(out[ch],0,nframes*sizeof(sample_t));
      }
    }else{
      for (ch=sndjack_num_channels_inuse;ch<sndjack_num_channels_allocated;ch++){
    memset(out[ch],0,nframes*sizeof(sample_t));
      }
    }

    for (i=0;i<nframes;i++){
      if (sj_unread==0){	
    if (sj_status==SJ_RUNNING)
      sj_xrun+=nframes-i;
    for (;i<nframes;i++){
      for (ch=0;ch<sndjack_num_channels_inuse;ch++){
        out[ch][i]=0.0f;
      }
    }
    break;
      }

      if (sndjack_num_channels_inuse==1 && sndjack_num_channels_allocated>=2){
    for (ch=0;ch<2;ch++){
      out[ch][i]=sndjack_channels[0].buffer[sj_readplace];
    }
      }else{
    for (ch=0;ch<sndjack_num_channels_inuse;ch++){
      out[ch][i]=sndjack_channels[ch].buffer[sj_readplace];
    }
      }
      atomic_add(&sj_unread,-1);
      sj_readplace++;
      if (sj_readplace==sj_buffersize)
    sj_readplace=0;
    }
    
    pthread_cond_broadcast(&sndjack_cond);

    if (sj_status==SJ_ABOUTTOSTOP && sj_unread==0)
      sj_status=SJ_STOPPED;
  }

}
 


static int sndjack_process(jack_nframes_t nframes, void *arg){
  sndjack_read_process(nframes);
  sndjack_write_process(nframes);
  return 0;
}


static int sndjack_read(void *buf,int bytes,int chs){
  int i,ch;
  int nframes=bytes /
    sndjack_read_format==MUS_COMP_FLOAT ? sizeof(float) :
    sndjack_read_format==MUS_COMP_SHORT ? sizeof(short) :
    1;
  float *buf_f=(float *)buf;
  short *buf_s=(short *)buf;
  char *buf_c=(char *)buf;

  for (i=0;i<nframes;i++){
    while(sj_r_unread==0){
      pthread_cond_wait(&sndjack_read_cond,&sndjack_read_mutex);
      jack_mus_watchdog_counter++;
    }

    if (sj_r_xrun>0){
      sj_r_totalxrun+=sj_r_xrun;
      sj_r_xrun=0;
      return -1;
    }
    for (ch=0;ch<chs;ch++){
      switch (sndjack_read_format){
      case MUS_BYTE:
    buf_c[i*chs+ch]=sndjack_read_channels[ch].buffer[sj_r_readplace] * 127.9f;
    break;
      case MUS_COMP_SHORT:
    buf_s[i*chs+ch]=sndjack_read_channels[ch].buffer[sj_r_readplace] * 32767.9f;
    break;
      case MUS_COMP_FLOAT:
    buf_f[i*chs+ch]=sndjack_read_channels[ch].buffer[sj_r_readplace];
    break;
      }}
    atomic_add(&sj_r_unread,-1);
    sj_r_readplace++;
    if (sj_r_readplace==sj_r_buffersize)
      sj_r_readplace=0;
  }
  return 0;
}

static void sndjack_write(sample_t **buf,int nframes,int latencyframes,int chs){
  int ch;
  int i;

  if (sj_xrun>0){
    if (sj_status==SJ_RUNNING){
      printf("Warning. %d frames delayed.\n",sj_xrun);
      sj_totalxrun+=sj_xrun;
    }
    sj_xrun=0;
  }

  for (i=0;i<nframes;i++){
    while(
      sj_status==SJ_RUNNING
      && (sj_unread==sj_buffersize
          || sj_unread >= SJ_MAX(sj_jackbuffersize*2, latencyframes))
      )
      {
    jack_mus_watchdog_counter++;
    pthread_cond_wait(&sndjack_cond,&sndjack_mutex);
      }

    for (ch=0;ch<chs;ch++)
      sndjack_channels[ch].buffer[sj_writeplace]=buf[ch][i];

    atomic_add(&sj_unread,1);
    sj_writeplace++;
    if (sj_writeplace==sj_buffersize)
      sj_writeplace=0;
  }

  if (sj_status==SJ_STOPPED)
    if (sj_unread>=sj_jackbuffersize)
      sj_status=SJ_RUNNING;
}
 
static int sndjack_buffersizecallback(jack_nframes_t nframes, void *arg){
  sj_jackbuffersize=nframes;
  return 0;
}

static int sndjack_getnumoutchannels(void){
  char *a=getenv("SNDLIB_NUM_JACK_CHANNELS");
  if (a!=NULL){
    int num_ch=atoi(a);
    return
      (num_ch<=0 || num_ch > 100000)
      ? 2
      : num_ch;
  }else{
    int lokke=0;
    const char **ports=jack_get_ports(sndjack_client,NULL,NULL,JackPortIsPhysical|JackPortIsInput);
    while(ports!=NULL && ports[lokke]!=NULL){
      lokke++;
    }
    
    if (lokke<2) return 2;
    return lokke;
  }
}

static int sndjack_getnuminchannels(void){
  char *a=getenv("SNDLIB_NUM_JACK_CHANNELS");
  if (a!=NULL){
    int num_ch=atoi(a);
    return
      (num_ch<=0 || num_ch > 100000)
      ? 2
      : num_ch;
  }else{
    int lokke=0;
    const char **ports=jack_get_ports(sndjack_client,NULL,NULL,JackPortIsPhysical|JackPortIsOutput);
    while(ports!=NULL && ports[lokke]!=NULL){
      lokke++;
    }
    if (lokke<2) return 2;
    return lokke;
  }
}


static int sndjack_init(void){
  int ch;
  int numch;
  int num=0;

  {
    jack_status_t status;
    sndjack_client=jack_client_open("sndlib",JackNoStartServer,&status,NULL);
    if (sndjack_client == NULL) {
#if 0
      fprintf (stderr, "jack_client_open() failed, "
           "status = 0x%2.0x\n", status);
      if (status & JackServerFailed) {
    fprintf (stderr, "Unable to connect to JACK server\n");
      }
#endif
      return -1;
    }
  }

  pthread_mutex_init(&sndjack_mutex,NULL);
  pthread_cond_init(&sndjack_cond,NULL);
  pthread_mutex_init(&sndjack_read_mutex,NULL);
  pthread_cond_init(&sndjack_read_cond,NULL);

  jack_set_process_callback(sndjack_client,sndjack_process,NULL);

  sndjack_num_channels_allocated = numch = sndjack_getnumoutchannels();
  sndjack_num_read_channels_allocated    = sndjack_getnuminchannels();
     
  sndjack_channels=(struct SndjackChannel *)calloc(sizeof(struct SndjackChannel),numch);
  sndjack_read_channels=(struct SndjackChannel *)calloc(sizeof(struct SndjackChannel),sndjack_num_read_channels_allocated);

  for (ch=0;ch<numch;ch++){
    sndjack_channels[ch].buffer=(sample_t *)calloc(sizeof(sample_t),SNDJACK_BUFFERSIZE);
  }
  for (ch=0;ch<sndjack_num_read_channels_allocated;ch++){
    sndjack_read_channels[ch].buffer=(sample_t *)calloc(sizeof(sample_t),SNDJACK_BUFFERSIZE);
  }
  sj_buffersize=SNDJACK_BUFFERSIZE;

  for (ch=0;ch<numch;ch++){
    char temp[500];
    sprintf(temp, "out_%d",ch+1);
    if ((sndjack_channels[ch].port=jack_port_register(
                             sndjack_client,
                             mus_strdup(temp),
                             JACK_DEFAULT_AUDIO_TYPE,
                             JackPortIsOutput,
                             0
                             ))==NULL)
      {
    fprintf(stderr, "Error. Could not register jack port.\n");
    goto failed_register;
      }
  }

  for (ch=0;ch<sndjack_num_read_channels_allocated;ch++){
    char temp[500];
    sprintf(temp, "in_%d",ch+1);
    if ((sndjack_read_channels[ch].port=jack_port_register(
                              sndjack_client,
                              mus_strdup(temp),
                              JACK_DEFAULT_AUDIO_TYPE,
                              JackPortIsInput,
                              0
                              ))==NULL)
      {
    fprintf(stderr, "Error. Could not register jack port.\n");
    goto failed_register;
      }
  }




  sj_jackbuffersize=jack_get_buffer_size(sndjack_client);
  jack_set_buffer_size_callback(sndjack_client,sndjack_buffersizecallback,NULL);

  if (jack_activate (sndjack_client)) {
    fprintf (stderr, "Error. Cannot activate jack client.\n");
    goto failed_activate;
  }

  if (getenv("SNDLIB_JACK_DONT_AUTOCONNECT")==NULL){

    const char **outportnames=jack_get_ports(sndjack_client,NULL,NULL,JackPortIsPhysical|JackPortIsInput);
    for (ch=0;outportnames && outportnames[ch]!=NULL && ch<numch;ch++){
      if (
      jack_connect(
               sndjack_client,
               jack_port_name(sndjack_channels[ch].port),
               outportnames[ch]
               )
      )
    {
      printf ("Warning. Cannot connect jack output port %d: \"%s\".\n",ch,outportnames[ch]);
    }
    }

    const char **inportnames=jack_get_ports(sndjack_client,NULL,NULL,JackPortIsPhysical|JackPortIsOutput);
    for (ch=0;inportnames && inportnames[ch]!=NULL && ch<numch;ch++){
    if (
    jack_connect(
             sndjack_client,
             inportnames[ch],
             jack_port_name(sndjack_read_channels[ch].port)
             )
    )
      {
    printf ("Warning. Cannot connect jack input port %d: \"%s\".\n",ch,inportnames[ch]);
      }
    }
  }

  return 0;
  
  // failed_connect:
 failed_activate:
  jack_deactivate(sndjack_client);
  
 failed_register:
  jack_client_close(sndjack_client);
  sndjack_client=NULL;

  return -1;
}
static void sndjack_cleanup(void){
  int ch;
  for (ch=0;ch<sndjack_num_channels_allocated;ch++){
    src_delete(sndjack_srcstates[ch]);
  }
  jack_deactivate(sndjack_client);
  jack_client_close(sndjack_client);

}


/***************/
/* Sndlib Part */
/***************/

static int sndjack_format;
static sample_t **sndjack_buffer;
static sample_t *sndjack_srcbuffer;

static int sndjack_dev;
static int sndjack_read_dev;

/* prototypes for the jack sndlib functions */
static int   jack_mus_audio_initialize(void);
static void  jack_mus_oss_set_buffers(int num, int size);
static char* jack_mus_audio_moniker(void);
static int   jack_mus_audio_open_output(int ur_dev, int srate, int chans, int format, int size);
static int   jack_mus_audio_open_input(int ur_dev, int srate, int chans, int format, int requested_size);
static int   jack_mus_audio_write(int id, char *buf, int bytes);
static int   jack_mus_audio_read(int id, char *buf, int bytes);
static int   jack_mus_audio_close(int id);

#if (!HAVE_JACK_IN_LINUX) // Ie. Not using Linux.
int mus_audio_open_output(int ur_dev, int srate, int chans, int format, int size) 
{
  return(jack_mus_audio_open_output(ur_dev, srate, chans, format, size));
}

int mus_audio_open_input(int ur_dev, int srate, int chans, int format, int requested_size) 
{
  return(jack_mus_audio_open_input(ur_dev, srate, chans, format, requested_size));
}

int mus_audio_write(int id, char *buf, int bytes) 
{
  return(jack_mus_audio_write(id, buf, bytes));
}

int mus_audio_read(int id, char *buf, int bytes) 
{
  return(jack_mus_audio_read(id, buf, bytes));
}

int mus_audio_close(int id) 
{
  return(jack_mus_audio_close(id));
}

int mus_audio_initialize(void){
  return jack_mus_audio_initialize();
}

char* mus_audio_moniker(void) 
{
  return(jack_mus_audio_moniker());
}
#endif


static int jack_mus_audio_initialize(void) {
  int ch;

  if (audio_initialized){
    return MUS_NO_ERROR;
  }

  if (sndjack_init()!=0)
    return MUS_ERROR;

  sndjack_buffer=(sample_t **)calloc(sizeof(sample_t*),sndjack_num_channels_allocated);
  for (ch=0;ch<sndjack_num_channels_allocated;ch++)
    sndjack_buffer[ch]=(sample_t *)calloc(sizeof(sample_t),SNDJACK_BUFFERSIZE);
  sndjack_srcbuffer=(sample_t *)calloc(sizeof(sample_t),SNDJACK_BUFFERSIZE);

  sndjack_srcstates=(SRC_STATE **)calloc(sizeof(SRC_STATE*),sndjack_num_channels_allocated);
  for (ch=0;ch<sndjack_num_channels_allocated;ch++){
    sndjack_srcstates[ch]=src_new(SRC_QUALITY,1,NULL);
  }

  atexit(sndjack_cleanup);

  api = MUS_JACK_API;
  vect_mus_audio_initialize = jack_mus_audio_initialize;
  vect_mus_oss_set_buffers = jack_mus_oss_set_buffers;
  vect_mus_audio_moniker = jack_mus_audio_moniker;
  vect_mus_audio_open_output = jack_mus_audio_open_output;
  vect_mus_audio_open_input = jack_mus_audio_open_input;
  vect_mus_audio_write = jack_mus_audio_write;
  vect_mus_audio_read = jack_mus_audio_read;
  vect_mus_audio_close = jack_mus_audio_close;

  audio_initialized = true;

#if 0  

  /* Locking all future memory shouldn't be that necessary, and might even freeze the machine in certain situations. */
  /* So remove MCL_FUTURE from the mlockall call. (No. We can't do that. It can screw up code using the realtime extension. -Kjetil.*/
  munlockall();
  //mlockall(MCL_CURRENT);
  
  // Instead we just do this: (which is not enough, but maybe better than nothing)
  {
    mlock(sndjack_channels,sizeof(struct SndjackChannel)*sndjack_num_channels_allocated);
    mlock(sndjack_read_channels,sizeof(struct SndjackChannel)*sndjack_num_read_channels_allocated);

    for (ch=0;ch<numch;ch++){
      mlock(sndjack_channels[ch].buffer,sizeof(sample_t)*SNDJACK_BUFFERSIZE);
    }
    for (ch=0;ch<sndjack_num_read_channels_allocated;ch++){
      mlock(sndjack_read_channels[ch].buffer,sizeof(sample_t)*SNDJACK_BUFFERSIZE);
    }
  }
#endif

  return MUS_NO_ERROR;
}

// ??
static void  jack_mus_oss_set_buffers(int num, int size){
}

static int jack_mus_isrunning=0;
static pid_t jack_mus_player_pid;  
static pthread_t jack_mus_watchdog_thread;

static void *jack_mus_audio_watchdog(void *arg){
#if MUS_JACK
  struct sched_param par;

  par.sched_priority = sched_get_priority_max(SCHED_RR);
  if (sched_setscheduler(0,SCHED_RR,&par)==-1){
    fprintf(stderr, "SNDLIB: Unable to set SCHED_RR realtime priority for the watchdog thread. No watchdog.\n");
    goto exit;
  }

  for (;;){
    int last=jack_mus_watchdog_counter;
    sleep(1);

    if (jack_mus_isrunning && jack_mus_watchdog_counter<last+10){
      struct sched_param par;
      fprintf(stderr, "SNDLIB: Setting player to non-realtime for 2 seconds.\n");

      par.sched_priority = 0;
      if (sched_setscheduler(jack_mus_player_pid,SCHED_OTHER,&par)==-1){
    fprintf(stderr, "SNDLIB: Unable to set non-realtime priority. Must kill player thread. Sorry!\n");
    while(1){
      kill(jack_mus_player_pid,SIGKILL);
      sleep(2);
    }
      }

      sleep(2);

      if (jack_mus_isrunning){
    par.sched_priority = sched_get_priority_min(SCHED_RR)+1;
    if (sched_setscheduler(jack_mus_player_pid,SCHED_RR,&par)==-1){
      fprintf(stderr, "SNDLIB: Could not set back to realtime priority...\n");
    }else
      fprintf(stderr, "SNDLIB: Play thread set back to realtime priority.\n");
      }

    }
  }
 exit:
  fprintf(stderr, "SNDLIB: Watchdog exiting\n");
#endif
  return NULL;
}



static void jack_mus_audio_set_realtime(void){
#if HAVE_JACK_IN_LINUX
  struct sched_param par;
  static int watchdog_started=0;

  jack_mus_player_pid=getpid();

  if (watchdog_started==0){
    if (pthread_create(&jack_mus_watchdog_thread,NULL,jack_mus_audio_watchdog,NULL)!=0){
      fprintf(stderr, "Could not create watchdog. Not running realtime\n");
      return;
    }
    watchdog_started=1;
  }

  jack_mus_isrunning=1;

  par.sched_priority = sched_get_priority_min(SCHED_RR)+1;
  if (sched_setscheduler(0,SCHED_RR,&par)==-1){
    fprintf(stderr, "SNDLIB: Unable to set SCHED_RR realtime priority for the player thread.\n");
  }{
    //fprintf(stderr, "Set realtime priority\n");
  }
#endif
}

static void jack_mus_audio_set_non_realtime(void){
#if HAVE_JACK_IN_LINUX
  struct sched_param par;
  par.sched_priority = 0;
  sched_setscheduler(0,SCHED_OTHER,&par);
  //fprintf(stderr, "Set non-realtime priority\n");
  jack_mus_isrunning=0;
#endif
}

int jack_mus_audio_open_output(int dev, int srate, int chans, int format, int size){
  if (sndjack_client==NULL){
    if (jack_mus_audio_initialize()==MUS_ERROR)
      return MUS_ERROR;
  }
  
  if (sndjack_num_channels_allocated<chans){
    printf("Error. Can not play back %d channels. (Only %d)\n",chans,sndjack_num_channels_allocated);
    return MUS_ERROR;
  }

  if (format!=MUS_BYTE && format!=MUS_COMP_SHORT && format!=MUS_COMP_FLOAT){
    printf("Error, unable to handle format %s.\n",mus_data_format_to_string(format));
    return MUS_ERROR;
  }

  while(sj_status!=SJ_STOPPED) usleep(5);

  sj_unread=0;
  sj_writeplace=0;
  sj_readplace=0;


  if (srate!=jack_get_sample_rate(sndjack_client)){
    int lokke;
    //printf("Warning, sample-rate differs between snd and jack. Sound will not be played correctly! %d/%d\n",srate,jack_get_sample_rate(sndjack_client));
    sndjack_srcratio=(double)jack_get_sample_rate(sndjack_client)/(double)srate;
    for (lokke=0;lokke<chans;lokke++){
      src_reset(sndjack_srcstates[lokke]);
    }
  }else{
    sndjack_srcratio=1.0;
  }

  sndjack_format=format;
  sndjack_num_channels_inuse=chans;
  sndjack_dev=dev;

  jack_mus_audio_set_realtime();

  return(MUS_NO_ERROR);
}
 

#define MUS_BYTE_TO_SAMPLE(n) (((mus_float_t)(n) / (mus_float_t)(1 << 7)))

static int sndjack_from_byte(int ch,int chs,char *buf,float *out,int bytes){
  int i;
  int len=bytes/chs;
  if (len>SNDJACK_BUFFERSIZE) return -1;

  for (i=0;i<len;i++){
    out[i]=MUS_BYTE_TO_SAMPLE(buf[i*chs+ch]);
  }
  return len;
}

static int sndjack_from_short(int ch,int chs,short *buf,float *out,int bytes){
  int i;
  int len=bytes/(sizeof(short)*chs);
  if (len>SNDJACK_BUFFERSIZE) return -1;

  for (i=0;i<len;i++){
    out[i]=(float)buf[i*chs+ch]/32768.1f;
  }
  return len;
}

static int sndjack_from_float(int ch,int chs,float *buf,float *out,int bytes){
  int i;
  int len=bytes/(sizeof(float)*chs);
  if (len>SNDJACK_BUFFERSIZE) return -1;

  for (i=0;i<len;i++){
    out[i]=buf[i*chs+ch];
  }
  return len;
}


int jack_mus_audio_write(int line, char *buf, int bytes){
  int i;
  int ch;
  int outlen=0;

  for (ch=0;ch<sndjack_num_channels_inuse;ch++){
    int len = 0;
    float *buf2=sndjack_srcratio==1.0?sndjack_buffer[ch]:sndjack_srcbuffer;

    switch (sndjack_format){
    case MUS_BYTE:
      len=sndjack_from_byte(ch,sndjack_num_channels_inuse,buf,buf2,bytes);
      break;
    case MUS_COMP_SHORT:
      len=sndjack_from_short(ch,sndjack_num_channels_inuse,(short *)buf,buf2,bytes);
      break;
    case MUS_COMP_FLOAT:
      len=sndjack_from_float(ch,sndjack_num_channels_inuse,(float *)buf,buf2,bytes);
      break;
    }
    if (len<0){
      printf("Errur. Input buffer to large for mus_audio_write.\n");
      return MUS_ERROR;
    }

    if (sndjack_srcratio!=1.0){
      SRC_DATA src_data={
    buf2,sndjack_buffer[ch],
    len,SNDJACK_BUFFERSIZE,
    0,0,
    0,
    sndjack_srcratio
      };
      int res=src_process(sndjack_srcstates[ch],&src_data);
      if (res!=0){
    printf("Error while resampling. (%s)\n",src_strerror(res));
    return MUS_ERROR;
      }
      if (src_data.input_frames!=len){
    printf("Unsuccessfull resampling: Should have used %d bytes, used %ld.",len,(long int)(src_data.input_frames));
    return MUS_ERROR;
      }
      if (ch>0 && src_data.output_frames_gen!=outlen){
    printf("Error, src_process did not output the same number of frames as previous resampled channel (%ld/%d).\n"
           "Please report this problem to k.s.matheussen@notam02.no. Thanks!\n",(long int)(src_data.output_frames_gen),outlen);
    return MUS_ERROR;
      }
      outlen=src_data.output_frames_gen;
    }else{
      outlen=len;
    }
  }


  sndjack_write(sndjack_buffer,outlen,outlen*2,sndjack_num_channels_inuse);

  return MUS_NO_ERROR;
}
 
int jack_mus_audio_close(int line) 
{
  jack_mus_audio_set_non_realtime();
  if (line==sndjack_dev){
    sj_status=SJ_ABOUTTOSTOP;
    sndjack_num_channels_inuse=0;
  }
  return MUS_NO_ERROR;
 }

int jack_mus_audio_open_input(int dev, int srate, int chans, int format, int size){
  if (sndjack_client==NULL){
    if (jack_mus_audio_initialize()==MUS_ERROR)
      return MUS_ERROR;
  }
  
  if (sndjack_num_read_channels_allocated<chans){
    printf("Error. Can not record %d channels. (Only %d)\n",chans,sndjack_num_read_channels_allocated);
    return MUS_ERROR;
  }

  printf("dev: %d\n" ,dev);
  if (format!=MUS_BYTE && format!=MUS_COMP_SHORT && format!=MUS_COMP_FLOAT){
    printf("Error, unable to handle format %s.\n",mus_data_format_to_string(format));
    return MUS_ERROR;
  }

  if (srate!=jack_get_sample_rate(sndjack_client)){
    printf("Warning, jacks samplerate is %d (and not %d), and the recording will use this samplerate too.\n",jack_get_sample_rate(sndjack_client),srate);
  }

  sndjack_read_format=format;
  sndjack_num_read_channels_inuse=chans;
  sndjack_read_dev=dev;

  return(MUS_NO_ERROR);
}


int jack_mus_audio_read(int line, char *buf, int bytes){
  if (sndjack_read(buf,bytes,sndjack_num_read_channels_inuse)==-1)
    return(MUS_ERROR);
  return MUS_NO_ERROR;
}


char *jack_mus_audio_moniker(void) 
{
  return((char *)"Jack");
}
#endif
 


/* ------------------------------- HPUX ----------------------------------------- */

/* if this is basically the same as the Sun case with different macro names,
 * then it could perhaps be updated to match the new Sun version above --
 * Sun version changed 28-Jan-99
 */

#if defined(__hpux) && (!(defined(AUDIO_OK)))
#define AUDIO_OK 1
#include <sys/audio.h>


#define RETURN_ERROR_EXIT(Error_Type, Audio_Line, Ur_Error_Message) \
  do { char *Error_Message; Error_Message = Ur_Error_Message; \
    if (Audio_Line != -1) close(Audio_Line); \
    if (Error_Message) \
      {MUS_STANDARD_ERROR(Error_Type, Error_Message); free(Error_Message);} \
    else MUS_STANDARD_ERROR(Error_Type, mus_error_type_to_string(Error_Type)); \
    return(MUS_ERROR); \
  } while (false)


char *mus_audio_moniker(void) 
{
  return("HPUX audio");
}


int mus_audio_open_output(int ur_dev, int srate, int chans, int format, int size)
{
  int fd, i, dev;
  struct audio_describe desc;

  dev = MUS_AUDIO_DEVICE(ur_dev);
  fd = open("/dev/audio", O_RDWR);
  if (fd == -1) 
    RETURN_ERROR_EXIT(MUS_AUDIO_CANT_OPEN, -1,
              mus_format("can't open /dev/audio for output: %s",
                 strerror(errno)));

  ioctl(fd, AUDIO_SET_CHANNELS, chans);
  if (dev == MUS_AUDIO_SPEAKERS)
    ioctl(fd, AUDIO_SET_OUTPUT, AUDIO_OUT_SPEAKER);
  else
    if (dev == MUS_AUDIO_LINE_OUT)
      ioctl(fd, AUDIO_SET_OUTPUT, AUDIO_OUT_LINE);
    else ioctl(fd, AUDIO_SET_OUTPUT, AUDIO_OUT_HEADPHONE);

  if (format == MUS_BSHORT)
    ioctl(fd, AUDIO_SET_DATA_FORMAT, AUDIO_FORMAT_LINEAR16BIT);
  else
    {
      if (format == MUS_MULAW)
    ioctl(fd, AUDIO_SET_DATA_FORMAT, AUDIO_FORMAT_ULAW);
      else 
    {
      if (format == MUS_ALAW)
        ioctl(fd, AUDIO_SET_DATA_FORMAT, AUDIO_FORMAT_ALAW);
      else 
        RETURN_ERROR_EXIT(MUS_AUDIO_FORMAT_NOT_AVAILABLE, fd,
                  mus_format("can't set output format to %d (%s) for %d",
                     format, mus_data_format_to_string(format),
                     dev));
    }
    }

  ioctl(fd, AUDIO_DESCRIBE, &desc);
  for (i = 0; i < desc.nrates; i++) 
    if (srate == desc.sample_rate[i]) 
      break;

  if (i == desc.nrates) 
    RETURN_ERROR_EXIT(SRATE_NOT_AVAILABLE, fd,
              mus_format("can't set srate to %d on %d",
                 srate, dev));

  ioctl(fd, AUDIO_SET_SAMPLE_RATE, srate);
  return(fd);
}


int mus_audio_write(int line, char *buf, int bytes)
{
  write(line, buf, bytes);
  return(MUS_NO_ERROR);
}


int mus_audio_close(int line) 
{
  close(line);
  return(MUS_NO_ERROR);
}


int mus_audio_initialize(void) 
{
  return(MUS_NO_ERROR);
}


/* struct audio_status status_b;
 * ioctl(devAudio, AUDIO_GET_STATUS, &status_b)
 * not_busy = (status_b.transmit_status == AUDIO_DONE);
*/

int mus_audio_open_input(int ur_dev, int srate, int chans, int format, int size) 
{
  int fd, i, dev;
  struct audio_describe desc;

  dev = MUS_AUDIO_DEVICE(ur_dev);
  fd = open("/dev/audio", O_RDWR);
  if (fd == -1)
    RETURN_ERROR_EXIT(MUS_AUDIO_CANT_OPEN, NULL,
              mus_format("can't open /dev/audio for input: %s",
                 strerror(errno)));

  ioctl(fd, AUDIO_SET_CHANNELS, chans);
  if (dev == MUS_AUDIO_MICROPHONE)
    ioctl(fd, AUDIO_SET_INPUT, AUDIO_IN_MIKE);
  else ioctl(fd, AUDIO_SET_INPUT, AUDIO_IN_LINE);

  if (format == MUS_BSHORT)
    ioctl(fd, AUDIO_SET_DATA_FORMAT, AUDIO_FORMAT_LINEAR16BIT);
  else
    {
      if (format == MUS_MULAW)
    ioctl(fd, AUDIO_SET_DATA_FORMAT, AUDIO_FORMAT_ULAW);
      else 
    {
      if (format == MUS_ALAW)
        ioctl(fd, AUDIO_SET_DATA_FORMAT, AUDIO_FORMAT_ALAW);
      else 
        RETURN_ERROR_EXIT(MUS_AUDIO_FORMAT_NOT_AVAILABLE, fd,
                  mus_format("can't set input format to %d (%s) on %d",
                     format, mus_data_format_to_string(format),
                     dev));
    }
    }

  ioctl(fd, AUDIO_DESCRIBE, &desc);
  for (i = 0; i < desc.nrates; i++) 
    if (srate == desc.sample_rate[i]) 
      break;

  if (i == desc.nrates) 
    RETURN_ERROR_EXIT(MUS_AUDIO_SRATE_NOT_AVAILABLE, fd,
              mus_format("can't set srate to %d on %d",
                 srate, dev));

  ioctl(fd, AUDIO_SET_SAMPLE_RATE, srate);
  return(fd);
}


int mus_audio_read(int line, char *buf, int bytes) 
{
  read(line, buf, bytes);
  return(MUS_NO_ERROR);
}

#endif



/* ------------------------------- NETBSD/OpenBSD ----------------------------------------- */

#if (__NetBSD__ || __OpenBSD__) && (!(defined(AUDIO_OK)))
#define AUDIO_OK 1

/* started from Xanim a long time ago..., bugfixes from Thomas Klausner 30-Jul-05, worked into better shape Aug-05 */
#include <fcntl.h>
#include <sys/audioio.h>
#include <sys/ioctl.h>


#define RETURN_ERROR_EXIT(Error_Type, Audio_Line, Ur_Error_Message) \
  do { char *Error_Message; Error_Message = Ur_Error_Message; \
    if (Audio_Line != -1) close(Audio_Line); \
    if (Error_Message) \
      {MUS_STANDARD_ERROR(Error_Type, Error_Message); free(Error_Message);} \
    else MUS_STANDARD_ERROR(Error_Type, mus_error_type_to_string(Error_Type)); \
    return(MUS_ERROR); \
  } while (false)


static int bsd_format_to_sndlib(int encoding)
{
  switch (encoding)
    {
    case AUDIO_ENCODING_ULAW:       return(MUS_MULAW);   break;
    case AUDIO_ENCODING_ALAW:       return(MUS_ALAW);    break;
    case AUDIO_ENCODING_LINEAR:     return(MUS_BSHORT);  break; /* "sun compatible" so probably big-endian? */
    case AUDIO_ENCODING_SLINEAR:
    case AUDIO_ENCODING_LINEAR8:    return(MUS_BYTE);    break;
    case AUDIO_ENCODING_SLINEAR_LE: return(MUS_LSHORT);  break;
    case AUDIO_ENCODING_SLINEAR_BE: return(MUS_BSHORT);  break;
    case AUDIO_ENCODING_ULINEAR_LE: return(MUS_ULSHORT); break;
    case AUDIO_ENCODING_ULINEAR_BE: return(MUS_UBSHORT); break;
    case AUDIO_ENCODING_ULINEAR:    return(MUS_UBYTE);   break;
    case AUDIO_ENCODING_NONE:
    case AUDIO_ENCODING_ADPCM: 
    default:                        return(MUS_UNKNOWN); break;
    }
  return(MUS_UNKNOWN);
}


static int sndlib_format_to_bsd(int encoding)
{
  switch (encoding)
    {
    case MUS_MULAW:   return(AUDIO_ENCODING_ULAW);       break;
    case MUS_ALAW:    return(AUDIO_ENCODING_ALAW);       break;
    case MUS_BYTE:    return(AUDIO_ENCODING_SLINEAR);    break;
    case MUS_LSHORT:  return(AUDIO_ENCODING_SLINEAR_LE); break;
    case MUS_BSHORT:  return(AUDIO_ENCODING_SLINEAR_BE); break;
    case MUS_ULSHORT: return(AUDIO_ENCODING_ULINEAR_LE); break;
    case MUS_UBSHORT: return(AUDIO_ENCODING_ULINEAR_BE); break;
    case MUS_UBYTE:   return(AUDIO_ENCODING_ULINEAR);    break;
    }
  return(AUDIO_ENCODING_NONE);
}


int mus_audio_initialize(void) 
{
  return(MUS_NO_ERROR);
}


char *mus_audio_moniker(void) 
{
#if __NetBSD__
  return("NetBSD audio");
#else
  return("OpenBSD audio");
#endif
}


static int cur_chans = 1, cur_srate = 22050;

int mus_audio_write(int line, char *buf, int bytes) 
{
  /* trouble... AUDIO_WSEEK always returns 0, no way to tell that I'm about to
   *   hit "hiwat", but when I do, it hangs.  Can't use AUDIO_DRAIN --
   *   it introduces interruptions.  Not sure what to do...
   */
  int b = 0;

  b = write(line, buf, bytes);
  usleep(10000);

  if ((b != bytes) && (b > 0)) /* b <= 0 presumably some sort of error, and we want to avoid infinite recursion below */
    {
      /* hangs at close if we don't handle this somehow */
      if ((cur_chans == 1) || (cur_srate == 22050))
    sleep(1);
      else usleep(10000);
      mus_audio_write(line, (char *)(buf + b), bytes - b);
    }
  return(MUS_NO_ERROR);
}


int mus_audio_close(int line) 
{
  usleep(100000);
  ioctl(line, AUDIO_FLUSH, 0);
  close(line);
  return(MUS_NO_ERROR);
}


static int netbsd_default_outputs = (AUDIO_HEADPHONE | AUDIO_LINE_OUT | AUDIO_SPEAKER); 

int mus_audio_open_output(int dev, int srate, int chans, int format, int size) 
{
  int line, encode;
  audio_info_t a_info;

  line = open("/dev/sound", O_WRONLY | O_NDELAY); /* /dev/audio assumes mono 8-bit mulaw */
  if (line == -1)
    {
      if (errno == EBUSY) 
    return(mus_error(MUS_AUDIO_CANT_OPEN, NULL));
      else return(mus_error(MUS_AUDIO_DEVICE_NOT_AVAILABLE, NULL));
    }
  AUDIO_INITINFO(&a_info);

  /* a_info.blocksize = size; */
  encode = sndlib_format_to_bsd(format);
  if (encode == AUDIO_ENCODING_NONE)
    RETURN_ERROR_EXIT(MUS_AUDIO_FORMAT_NOT_AVAILABLE, -1,
              mus_format("format %d (%s) not available",
                 format, 
                 mus_data_format_name(format)));

  a_info.play.encoding = encode;
  a_info.mode = AUMODE_PLAY | AUMODE_PLAY_ALL;
  a_info.play.precision = mus_bytes_per_sample(format) * 8;
  a_info.play.sample_rate = srate;

  if (dev == MUS_AUDIO_LINE_OUT)
    a_info.play.port = AUDIO_LINE_OUT;
  else
    {
      if (dev == MUS_AUDIO_SPEAKERS)
    a_info.play.port = AUDIO_SPEAKER | (netbsd_default_outputs & AUDIO_HEADPHONE);
      else a_info.play.port = netbsd_default_outputs;
    }

  a_info.play.channels = chans;
  ioctl(line, AUDIO_SETINFO, &a_info);
  /* actually doesn't set the "ports" field -- always 0 */

  ioctl(line, AUDIO_GETINFO, &a_info);

  if ((int)(a_info.play.sample_rate) != srate)
    mus_print("srate: %d -> %d\n", srate, a_info.play.sample_rate);
  if ((int)(a_info.play.encoding) != sndlib_format_to_bsd(format))
    mus_print("encoding: %d -> %d\n", sndlib_format_to_bsd(format), a_info.play.encoding);
  if ((int)(a_info.play.channels) != chans)
    mus_print("chans: %d -> %d\n", chans, a_info.play.channels);

  cur_chans = chans;
  cur_srate = srate;

  return(line);
}


int mus_audio_read(int line, char *buf, int bytes) 
{
  read(line, buf, bytes);
  return(MUS_NO_ERROR);
}


static int netbsd_formats(int ur_dev, int *val)
{
  int i, audio_fd, err, dev;
  audio_info_t info;
  bool ok = true;
  audio_encoding_t e_info;

  dev = MUS_AUDIO_DEVICE(ur_dev);
  AUDIO_INITINFO(&info);
  audio_fd = open("/dev/sound", O_RDONLY | O_NONBLOCK, 0);
  if (audio_fd == -1) 
    RETURN_ERROR_EXIT(MUS_AUDIO_CANT_READ, -1,
              mus_format("can't open /dev/sound: %s",
                 strerror(errno)));
  err = ioctl(audio_fd, AUDIO_GETINFO, &info); 
  if (err == -1) 
    RETURN_ERROR_EXIT(MUS_AUDIO_CANT_READ, audio_fd,
              mus_format("can't get dac info"));

  for (i = 0; ; i++)
    {
      e_info.index = i;
      err = ioctl(audio_fd, AUDIO_GETENC, &e_info);
      if (err != 0) break;
      val[i + 1] = bsd_format_to_sndlib(e_info.encoding);
    }
  val[0] = i;
  return(MUS_NO_ERROR);
}



int mus_audio_open_input(int ur_dev, int srate, int chans, int format, int size) 
{
  audio_info_t info;
  int encode, bits, dev, audio_fd, err;

  dev = MUS_AUDIO_DEVICE(ur_dev);
  encode = sndlib_format_to_bsd(format);
  bits = 8 * mus_bytes_per_sample(format);
  if (encode == AUDIO_ENCODING_NONE) 
    RETURN_ERROR_EXIT(MUS_AUDIO_FORMAT_NOT_AVAILABLE, -1,
              mus_format("format %s not available for recording",
                 mus_data_format_name(format)));

  if (dev != MUS_AUDIO_DUPLEX_DEFAULT)
    audio_fd = open("/dev/sound", O_RDONLY, 0);
  else audio_fd = open("/dev/sound", O_RDWR, 0);
  if (audio_fd == -1) 
    RETURN_ERROR_EXIT(MUS_AUDIO_CANT_OPEN, -1,
              mus_format("can't open /dev/sound: %s",
                 strerror(errno)));

  AUDIO_INITINFO(&info);
  info.record.sample_rate = srate;
  info.record.channels = chans;
  info.record.precision = bits;
  info.record.encoding = encode;
  info.record.port = AUDIO_MICROPHONE;
  err = ioctl(audio_fd, AUDIO_SETINFO, &info); 
  if (err == -1) 
    RETURN_ERROR_EXIT(MUS_AUDIO_CANT_WRITE, audio_fd,
              mus_format("can't set up for recording"));
  return(audio_fd);
}

#endif



/* -------------------------------- PULSEAUDIO -------------------------------- */

#if defined(MUS_PULSEAUDIO) && (!(defined(AUDIO_OK)))
#define AUDIO_OK 1


/* this code compiles/loads, but I don't know if it works -- paplay itself
 *   doesn't work on my machine due to either a libtool/dlopen mismatch
 *   or some problem with "pulse-rt".
 */


#include <pulse/simple.h>
#include <pulse/error.h>
#include <pulse/gccmacro.h>


static int sndlib_to_pa_format(int format)
{
  switch (format)
    {
    case MUS_BYTE:   return(PA_SAMPLE_U8);
    case MUS_LSHORT: return(PA_SAMPLE_S16LE);
    case MUS_BSHORT: return(PA_SAMPLE_S16BE);
    case MUS_LINT:   return(PA_SAMPLE_S32LE);
    case MUS_BINT:   return(PA_SAMPLE_S32BE);
    case MUS_LFLOAT: return(PA_SAMPLE_FLOAT32LE);
    case MUS_BFLOAT: return(PA_SAMPLE_FLOAT32BE);
    case MUS_ALAW:   return(PA_SAMPLE_ALAW);
    case MUS_MULAW:  return(PA_SAMPLE_ULAW);

    default: 
      fprintf(stderr, "unsupported data format: %d\n", format);
      return(0);
      break;
    }
} 


static pa_simple *pa_out = NULL, *pa_in = NULL;

int mus_audio_open_output(int dev, int srate, int chans, int format, int size) 
{
  pa_sample_spec *spec;
  int error;

  spec = (pa_sample_spec *)malloc(sizeof(pa_sample_spec));
  spec->format = sndlib_to_pa_format(format);
  spec->rate = srate;
  spec->channels = chans;

  pa_out = pa_simple_new(NULL, "snd", PA_STREAM_PLAYBACK, NULL, "playback", spec, NULL, NULL, &error);
  free(spec);
  if (!pa_out)
    {
      fprintf(stderr, "can't play: %s\n", pa_strerror(error));
      return(MUS_ERROR);
    }
  return(0);
}


int mus_audio_open_input(int dev, int srate, int chans, int format, int size) 
{
  return(MUS_ERROR);
}


int mus_audio_write(int line, char *buf, int bytes) 
{
  int error;
  pa_simple_write(pa_out, (unsigned char *)buf, (size_t)bytes, &error);
}


int mus_audio_close(int line) 
{
  int error;
  pa_simple_drain(pa_out, &error);
  pa_simple_free(pa_out);
  pa_out = NULL;
}


int mus_audio_read(int line, char *buf, int bytes) 
{
  return(MUS_ERROR);
}


int mus_audio_initialize(void) 
{
  return(MUS_ERROR);
}


char *mus_audio_moniker(void) 
{
  return(mus_format("pulseaudio %s", pa_get_library_version()));
}


#endif



/* -------------------------------- PORTAUDIO -------------------------------- */

#if defined(MUS_PORTAUDIO) && (!(defined(AUDIO_OK)))
#define AUDIO_OK 1

#include <portaudio.h>

#define PA_OUT_STREAM 0
#define PA_IN_STREAM 1

static unsigned long sndlib_to_portaudio_format(int format)
{
  switch (format)
    {
    case MUS_BYTE:   return(paInt8);
    case MUS_LSHORT: return(paInt16);
    case MUS_BSHORT: return(paInt16);
    case MUS_LINT:   return(paInt32);
    case MUS_BINT:   return(paInt32);
    case MUS_LFLOAT: return(paFloat32);
    case MUS_BFLOAT: return(paFloat32);
    }
  return(paInt16);
}

static PaStream *out_stream = NULL;

int mus_audio_open_output(int dev, int srate, int chans, int format, int size) 
{
  PaStreamParameters output_pars;
  PaError err;

  output_pars.device = Pa_GetDefaultOutputDevice();
  output_pars.channelCount = chans;
  output_pars.sampleFormat = sndlib_to_portaudio_format(format);
  output_pars.suggestedLatency = Pa_GetDeviceInfo(output_pars.device)->defaultHighOutputLatency;
  output_pars.hostApiSpecificStreamInfo = NULL;

  err = Pa_OpenStream(&out_stream, NULL, &output_pars, srate, 1024, paClipOff, NULL, NULL); /* 1024 = frames [dac_size] but can we use "size"? */
  if (err == paNoError)
    err = Pa_StartStream(out_stream);

  if (err != paNoError)
    {
      fprintf(stderr, "portaudio open output: %s\n", Pa_GetErrorText(err));
      return(MUS_ERROR);
    }
  return(PA_OUT_STREAM);
}


static PaStream *in_stream = NULL;

int mus_audio_open_input(int dev, int srate, int chans, int format, int size) 
{
  PaStreamParameters input_pars;
  PaError err;

  input_pars.device = Pa_GetDefaultInputDevice();
  input_pars.channelCount = chans;
  input_pars.sampleFormat = sndlib_to_portaudio_format(format);
  input_pars.suggestedLatency = Pa_GetDeviceInfo(input_pars.device)->defaultHighInputLatency;
  input_pars.hostApiSpecificStreamInfo = NULL;

  err = Pa_OpenStream(&in_stream, &input_pars, NULL, srate, 1024, paClipOff, NULL, NULL);
  if (err == paNoError)
    err = Pa_StartStream(in_stream);

  if (err != paNoError)
    {
      fprintf(stderr, "portaudio open input: %s\n", Pa_GetErrorText(err));
      return(MUS_ERROR);
    }
  return(MUS_NO_ERROR);

}


int mus_audio_write(int line, char *buf, int bytes) 
{
  PaError err;
  err = Pa_WriteStream(out_stream, buf, 1024);

  if (err != paNoError)
    {
      fprintf(stderr, "portaudio write: %s\n", Pa_GetErrorText(err));
      return(MUS_ERROR);
    }
  return(MUS_NO_ERROR);
}


int mus_audio_close(int line) 
{
  PaError err;
  if (line == PA_IN_STREAM)
    err = Pa_CloseStream(in_stream);
  else err = Pa_CloseStream(out_stream);

  if (err != paNoError)
    {
      fprintf(stderr, "portaudio close: %s\n", Pa_GetErrorText(err));
      return(MUS_ERROR);
    }
  return(MUS_NO_ERROR);
}


int mus_audio_read(int line, char *buf, int bytes) 
{
  PaError err;
  err = Pa_ReadStream(in_stream, buf, 1024);

  if (err != paNoError)
    {
      fprintf(stderr, "portaudio read: %s\n", Pa_GetErrorText(err));
      return(MUS_ERROR);
    }
  return(MUS_NO_ERROR);
}


static bool portaudio_initialized = false;

int mus_audio_initialize(void) 
{
  PaError err;

  if (portaudio_initialized) return(MUS_NO_ERROR);
  portaudio_initialized = true;

  err = Pa_Initialize();
  if (err == paNoError)
    return(MUS_NO_ERROR);

  fprintf(stderr, "portaudio initialize: %s\n", Pa_GetErrorText(err));
  return(MUS_ERROR);
}


char *mus_audio_moniker(void) 
{
  return((char *)Pa_GetVersionText());
}
#endif




/* ------------------------------- STUBS ----------------------------------------- */

#ifndef AUDIO_OK
int mus_audio_open_output(int dev, int srate, int chans, int format, int size) {return(MUS_ERROR);}
int mus_audio_open_input(int dev, int srate, int chans, int format, int size) {return(MUS_ERROR);}
int mus_audio_write(int line, char *buf, int bytes) {return(MUS_ERROR);}
int mus_audio_close(int line) {return(MUS_ERROR);}
int mus_audio_read(int line, char *buf, int bytes) {return(MUS_ERROR);}
int mus_audio_initialize(void) {return(MUS_ERROR);}
char *mus_audio_moniker(void) {return((char *)"no audio support");}
#endif



/* for CLM */
void mus_reset_audio_c(void)
{
  audio_initialized = false;
  version_name = NULL;
}


#if HAVE_ALSA || HAVE_OSS

void mus_audio_alsa_channel_info(int dev, int *info);
void mus_audio_alsa_channel_info(int dev, int *info)
{
#if MUS_JACK
  if (api == MUS_JACK_API) 
    {
      info[0] = sndjack_num_channels_allocated;
      return;
    }
#endif

#if HAVE_ALSA
  if (api == MUS_ALSA_API) 
    {
      alsa_chans(dev, info);
      return;
    }
#endif

#if HAVE_OSS
  info[0] = 2;
#endif

}


int mus_audio_alsa_samples_per_channel(int dev);
int mus_audio_alsa_samples_per_channel(int dev)
{
#if HAVE_ALSA
  return(alsa_samples_per_channel);
#else
  return(1024);
#endif
}


void mus_audio_alsa_device_list(int ur_dev, int chan, int *val);
void mus_audio_alsa_device_list(int ur_dev, int chan, int *val)
{
#if HAVE_ALSA
  int i = 1;
  
  if (!audio_initialized) mus_audio_initialize();
  
  if (alsa_hw_params[SND_PCM_STREAM_PLAYBACK]) 
    val[i++] = to_sndlib_device(0, SND_PCM_STREAM_PLAYBACK);

  if (alsa_hw_params[SND_PCM_STREAM_CAPTURE]) 
    val[i++] = to_sndlib_device(0, SND_PCM_STREAM_CAPTURE);

  val[0] = (i - 1);
#endif
}

#define MUS_AUDIO_DIRECTION_PLAYBACK 0
#define MUS_AUDIO_DIRECTION_RECORD 1

int mus_audio_alsa_device_direction(int dev);
int mus_audio_alsa_device_direction(int dev)
{
#if HAVE_OSS
  switch (MUS_AUDIO_DEVICE(dev))
    {
    case MUS_AUDIO_DIGITAL_OUT: case MUS_AUDIO_LINE_OUT: case MUS_AUDIO_DEFAULT:
    case MUS_AUDIO_SPEAKERS: case MUS_AUDIO_MIXER:
    case MUS_AUDIO_AUX_OUTPUT: case MUS_AUDIO_DAC_OUT: 
      return(MUS_AUDIO_DIRECTION_PLAYBACK);

    default:  
      return(MUS_AUDIO_DIRECTION_RECORD);
    }
#else
  {
    int card, device, alsa_device = 0;
    snd_pcm_stream_t alsa_stream = SND_PCM_STREAM_PLAYBACK;
  
    if ((!audio_initialized) && 
    (mus_audio_initialize() != MUS_NO_ERROR))
      return(MUS_ERROR);
  
    card = MUS_AUDIO_SYSTEM(dev);
    device = MUS_AUDIO_DEVICE(dev);
    to_alsa_device(device, &alsa_device, &alsa_stream);

    if (card > 0 || alsa_device > 0) 
      return(alsa_mus_error(MUS_AUDIO_CANT_READ, NULL));
    return(alsa_stream);
  }
#endif
}
#endif


int mus_audio_device_channels(int dev)
{
#if MUS_JACK
  if (api == MUS_JACK_API) 
    {
      return(sndjack_num_channels_allocated);
    }
#endif
 
#if HAVE_ALSA
  if (api == MUS_ALSA_API) 
    {
      return(alsa_chans(dev, NULL));
    }
#endif

  return(2); /* netbsd hpux sun mac and oss with quibbles */
}


int mus_audio_compatible_format(int dev) /* snd-dac and sndplay */
{
#if HAVE_ALSA
  int err, i;
  int ival[32];
  if (api == MUS_ALSA_API) 
    {
      err = alsa_formats(dev, 32, ival);
      if (err != MUS_ERROR)
    {
      for (i = 1; i <= ival[0]; i++)
        if (ival[i] == MUS_AUDIO_COMPATIBLE_FORMAT) 
          return(MUS_AUDIO_COMPATIBLE_FORMAT);

      for (i = 1; i <= ival[0]; i++) 
        if ((ival[i] == MUS_BINT) || (ival[i] == MUS_LINT) ||
            (ival[i] == MUS_BFLOAT) || (ival[i] == MUS_LFLOAT) ||
        (ival[i] == MUS_BSHORT) || (ival[i] == MUS_LSHORT))
          return(ival[i]);

      for (i = 1; i <= ival[0]; i++) 
        if ((ival[i] == MUS_MULAW) || (ival[i] == MUS_ALAW) ||
            (ival[i] == MUS_UBYTE) || (ival[i] == MUS_BYTE))
          return(ival[i]);

      return(ival[1]);
    }
    }
#endif

#if MUS_JACK
  if (api == MUS_JACK_API) 
    {
      return(MUS_COMP_FLOAT);
    }
#endif

  return(MUS_AUDIO_COMPATIBLE_FORMAT);
}


static int look_for_format (int *mixer_vals, int format)
{
  int i, lim;
  lim = mixer_vals[0];
  for (i = 1; i <= lim; i++)
    if (mixer_vals[i] == format)
      return(format);
  return(-1);
}


int mus_audio_device_format(int dev) /* snd-dac */
{
  int mixer_vals[16];
  int format;

  /* we return the new format, so mixer_vals is just a local collector of possible formats */
  mixer_vals[0] = 0;

#if (!WITH_AUDIO)
  return(MUS_AUDIO_COMPATIBLE_FORMAT);
#endif

#if HAVE_OSS
  if (api == MUS_OSS_API) 
    oss_formats(dev, mixer_vals);
#endif

#if HAVE_ALSA
  if (api == MUS_ALSA_API) 
    alsa_formats(dev, 16, mixer_vals);
#endif

#if MUS_JACK
  if (api == MUS_JACK_API) 
    {
      mixer_vals[0] = 1;
      mixer_vals[1] = MUS_COMP_FLOAT;
    }
#endif

#if HAVE_SUN
  mixer_vals[0] = 2;
  mixer_vals[1] = MUS_LSHORT;
  mixer_vals[2] = MUS_MULAW;
#endif

#if __APPLE__
  mixer_vals[0] = 1;
#if MUS_LITTLE_ENDIAN
  mixer_vals[1] = MUS_LFLOAT;
#else
  mixer_vals[1] = MUS_BFLOAT;
#endif
#endif

#if __NetBSD__ || __OpenBSD__
  netbsd_formats(dev, mixer_vals);
#endif

  format = look_for_format(mixer_vals, MUS_AUDIO_COMPATIBLE_FORMAT);
  if (format != -1)
    return(format);

#if MUS_LITTLE_ENDIAN
  format = look_for_format(mixer_vals, MUS_LFLOAT);
  if (format == -1)
    {
      format = look_for_format(mixer_vals, MUS_LSHORT);
      if (format == -1)
    format = mixer_vals[1];
    }
#else
  format = look_for_format(mixer_vals, MUS_BFLOAT);
  if (format == -1)
    {
      format = look_for_format(mixer_vals, MUS_BSHORT);
      if (format == -1)
    format = mixer_vals[1];
    }
#endif
  return(format);
}


#else
/* not WITH_AUDIO */

int mus_audio_open_output(int dev, int srate, int chans, int format, int size) {return(-1);}
int mus_audio_open_input(int dev, int srate, int chans, int format, int size) {return(-1);}
int mus_audio_write(int line, char *buf, int bytes) {return(-1);}
int mus_audio_close(int line) {return(-1);}
int mus_audio_read(int line, char *buf, int bytes) {return(-1);}
int mus_audio_initialize(void) {return(-1);}
char *mus_audio_moniker(void) {return((char *)"no audio support");}

void mus_reset_audio_c(void) {}

int mus_audio_device_channels(int dev)   {return(0);}
int mus_audio_compatible_format(int dev) {return(0);}
int mus_audio_device_format(int dev)     {return(0);}

#endif
