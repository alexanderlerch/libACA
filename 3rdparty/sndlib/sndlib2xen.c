/* Tie sndlib into Xen */

#include <mus-config.h>

#if USE_SND
  #include "snd.h"
#else
  #if HAVE_RUBY
    #define PROC_FALSE "false"
    #define PROC_TRUE "true"
  #endif
  #if HAVE_SCHEME || HAVE_FORTH
    #define PROC_FALSE "#f"
    #define PROC_TRUE "#t"
  #endif
#endif

#include <stddef.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <string.h>
#include <errno.h>


#ifdef _MSC_VER
  #pragma warning(disable: 4244)
#endif

#include "_sndlib.h"
#include "sndlib-strings.h"
#include "vct.h"
#include "clm.h"
#include "sndlib2xen.h"
#include "clm2xen.h"

#ifndef S_setB
  #if HAVE_RUBY
    #define S_setB "set_"
  #endif
  #if HAVE_SCHEME
    #define S_setB "set! "
  #endif
  #if HAVE_FORTH
    #define S_setB "set-"
  #endif
#endif


#if (!HAVE_SCHEME)
struct sound_data {
  mus_long_t length;
  int chans;
  mus_float_t **data;
  bool wrapped;
};

int mus_sound_data_chans(sound_data *sd)
{
  return(sd->chans);
}

mus_long_t mus_sound_data_length(sound_data *sd)
{
  return(sd->length);
}

static mus_float_t **mus_sound_data_data(sound_data *sd)
{
  return(sd->data);
}

static mus_float_t *mus_sound_data_channel_data(sound_data *sd, int chan)
{
  return(sd->data[chan]);
}

static sound_data *c_make_sound_data(int chans, mus_long_t frames)
{
  int i;
  sound_data *sd;

  sd = (sound_data *)malloc(sizeof(sound_data));
  sd->length = frames;
  sd->chans = chans;
  sd->wrapped = false;
  sd->data = (mus_float_t **)calloc(chans, sizeof(mus_float_t *));
  for (i = 0; i < chans; i++)
    sd->data[i] = (mus_float_t *)calloc(frames, sizeof(mus_float_t));
  return(sd);
}

static sound_data *sound_data_copy(sound_data *sd)
{
  int i, chans;
  mus_long_t len;
  sound_data *sdnew;

  chans = mus_sound_data_chans(sd);
  len = mus_sound_data_length(sd);

  sdnew = c_make_sound_data(chans, len);
  for (i = 0; i < chans; i++)
    memcpy((void *)(mus_sound_data_channel_data(sdnew, i)), (void *)(mus_sound_data_channel_data(sd, i)), len * sizeof(mus_float_t));
  return(sdnew);
}

static void sound_data_free(sound_data *sd)
{
  if (sd)
    {
      mus_float_t **data;
      data = mus_sound_data_data(sd);
      if ((data) && (!(sd->wrapped)))
	{
	  int i, chans;
	  chans = mus_sound_data_chans(sd);
	  for (i = 0; i < chans; i++)
	    if (data[i]) free(data[i]);
	  free(data);
	}
      sd->data = NULL;
      sd->chans = 0;
      free(sd);
    }
}

#else

/* in s7, sound-data is float-vector, so a sound-data object is an s7_pointer to an s7 vector
 */

int mus_sound_data_chans(sound_data *sd)
{
  s7_pointer p = (s7_pointer)sd;
  return((int)((s7_vector_dimensions(p))[0]));
}

mus_long_t mus_sound_data_length(sound_data *sd)
{
  s7_pointer p = (s7_pointer)sd;
  return((mus_long_t)(s7_vector_length(p) / mus_sound_data_chans(sd)));
}

static mus_float_t *mus_sound_data_channel_data(sound_data *sd, int chan)
{
  s7_pointer p = (s7_pointer)sd;
  if (chan == 0)
    return((mus_float_t *)(s7_float_vector_elements(p)));
  return((mus_float_t *)((s7_Double *)(s7_float_vector_elements(p) + chan * (s7_vector_length(p) / mus_sound_data_chans(sd)))));
}
#endif


/* -------------------------------------------------------------------------------- */

mus_float_t mus_sound_data_ref(sound_data *sd, int chan, mus_long_t pos)
{
  if ((sd) &&
      (chan < mus_sound_data_chans(sd)) &&
      (pos >= 0) &&
      (pos < mus_sound_data_length(sd)))
    return(mus_sound_data_channel_data(sd, chan)[pos]);
  return(0.0);
}

mus_float_t mus_sound_data_set(sound_data *sd, int chan, mus_long_t pos, mus_float_t val)
{
  if ((sd) &&
      (chan < mus_sound_data_chans(sd)) &&
      (pos >= 0) &&
      (pos < mus_sound_data_length(sd)))
    {
      mus_float_t *d;
      d = mus_sound_data_channel_data(sd, chan);
      d[pos] = val;
    }
  return(val);
}

void mus_sound_data_add_frame(sound_data *sd, mus_long_t pos, mus_float_t *data)
{
  int i, chans;
  chans = mus_sound_data_chans(sd);

  if ((pos >= 0) &&
      (pos < mus_sound_data_length(sd)))
    for (i = 0; i < chans; i++)
      {
	mus_float_t *d;
	d = mus_sound_data_channel_data(sd, i);
	d[pos] += data[i];
      }
}


/* originally I tried to simplify C GC by using global static strings that were 
 *   freed whenever the associated function was called again, on the assumption
 *   that the preceding value was now unused.  In a multithread context, that
 *   assumption is false, so I didn't use code like this:
 *
 *  static char *tmpstr = NULL;
 *
 *  static char *local_mus_expand_filename(char *name)
 *  {
 *    if (tmpstr) {free(tmpstr); tmpstr = NULL;}
 *       tmpstr = mus_expand_filename(name);
 *    return(tmpstr);
 *  }
 */

static XEN g_mus_sound_loop_info(XEN gfilename)
{
  #define H_mus_sound_loop_info "(" S_mus_sound_loop_info " filename): synth loop info for sound as a list: (start1 \
end1 start2 end2 base-note base-detune mode1 mode2)"
  int *res;
  XEN sres = XEN_EMPTY_LIST;
  char *str = NULL;

  XEN_ASSERT_TYPE(XEN_STRING_P(gfilename), gfilename, XEN_ONLY_ARG, S_mus_sound_loop_info, "a string"); 

  res = mus_sound_loop_info(str = mus_expand_filename(XEN_TO_C_STRING(gfilename)));
  if (str) free(str);
  if (res)
    {
      sres = XEN_LIST_8(C_TO_XEN_INT(res[0]), C_TO_XEN_INT(res[1]), C_TO_XEN_INT(res[2]),
			C_TO_XEN_INT(res[3]), C_TO_XEN_INT(res[4]), C_TO_XEN_INT(res[5]),
			C_TO_XEN_INT(res[6]), C_TO_XEN_INT(res[7]));
      free(res);
    }
  return(sres);
}


static XEN g_mus_sound_mark_info(XEN gfilename)
{
  #define H_mus_sound_mark_info "(" S_mus_sound_mark_info " filename): aifc header mark info as a list of lists: ((id pos)...)"
  int *mark_ids, *mark_positions;
  int marks = 0;
  XEN sres = XEN_EMPTY_LIST;
  char *str = NULL;

  XEN_ASSERT_TYPE(XEN_STRING_P(gfilename), gfilename, XEN_ONLY_ARG, S_mus_sound_mark_info, "a string"); 

  marks = mus_sound_mark_info(str = mus_expand_filename(XEN_TO_C_STRING(gfilename)), &mark_ids, &mark_positions);
  if (str) free(str);
  if (marks > 0)
    {
      int i;
      for (i = 0; i < marks; i++)
	sres = XEN_CONS(XEN_LIST_2(C_TO_XEN_INT(mark_ids[i]),
				   C_TO_XEN_INT(mark_positions[i])),
			sres);
    }
  return(sres);
}


static XEN gmus_sound(const char *caller, int (*func)(const char *file), XEN gfilename)
{
  char *str = NULL;
  XEN result;

  XEN_ASSERT_TYPE(XEN_STRING_P(gfilename), gfilename, XEN_ONLY_ARG, caller, "a string"); 
  str = mus_expand_filename(XEN_TO_C_STRING(gfilename));
  result = C_TO_XEN_INT((*func)(str));
  if (str) free(str);
  return(result);
}


static XEN gmus_sound_set(const char *caller, int (*func)(const char *file, int newval), XEN gfilename, XEN val)
{
  char *str = NULL;
  XEN result;

  XEN_ASSERT_TYPE(XEN_STRING_P(gfilename), gfilename, XEN_ARG_1, caller, "a string"); 
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ARG_2, caller, "an integer");
  str = mus_expand_filename(XEN_TO_C_STRING(gfilename));
  result = C_TO_XEN_INT((*func)(str, XEN_TO_C_INT(val)));
  if (str) free(str);
  return(result);
}


static XEN glmus_sound(const char *caller, mus_long_t (*func)(const char *file), XEN gfilename)
{
  char *str = NULL;
  XEN result;

  XEN_ASSERT_TYPE(XEN_STRING_P(gfilename), gfilename, XEN_ONLY_ARG, caller, "a string"); 
  str = mus_expand_filename(XEN_TO_C_STRING(gfilename));
  result = C_TO_XEN_LONG_LONG((*func)(str));
  if (str) free(str);
  return(result);
}


static XEN glmus_sound_set(const char *caller, int (*func)(const char *file, mus_long_t newval), XEN gfilename, XEN val)
{
  char *str = NULL;
  XEN result;

  XEN_ASSERT_TYPE(XEN_STRING_P(gfilename), gfilename, XEN_ARG_1, caller, "a string"); 
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_2, caller, "a number");
  str = mus_expand_filename(XEN_TO_C_STRING(gfilename));
  result = C_TO_XEN_LONG_LONG((*func)(str, XEN_TO_C_LONG_LONG(val)));
  if (str) free(str);
  return(result);
}


static XEN g_mus_sound_samples(XEN filename) 
{
  #define H_mus_sound_samples "(" S_mus_sound_samples " filename): samples (frames * channels) in sound file"
  return(glmus_sound(S_mus_sound_samples, mus_sound_samples, filename));
}


static XEN g_mus_sound_set_samples(XEN filename, XEN val) 
{
  return(glmus_sound_set(S_setB S_mus_sound_samples, mus_sound_set_samples, filename, val));
}


XEN g_mus_sound_frames(XEN filename) 
{
  #define H_mus_sound_frames "(" S_mus_sound_frames " filename): frames (samples / channel) in sound file"
  return(glmus_sound(S_mus_sound_frames, mus_sound_frames, filename));
}


static XEN g_mus_sound_datum_size(XEN filename) 
{
  #define H_mus_sound_datum_size "(" S_mus_sound_datum_size " filename): bytes per sample used by the data in sound file (data format dependent)"
  return(gmus_sound(S_mus_sound_datum_size, mus_sound_datum_size, filename));
}


static XEN g_mus_sound_data_location(XEN filename) 
{
  #define H_mus_sound_data_location "(" S_mus_sound_data_location " filename): location (in bytes) of first sample of sound data"
  return(glmus_sound(S_mus_sound_data_location, mus_sound_data_location, filename));
}


static XEN g_mus_sound_set_data_location(XEN filename, XEN val) 
{
  return(glmus_sound_set(S_setB S_mus_sound_data_location, mus_sound_set_data_location, filename, val));
}


XEN g_mus_sound_chans(XEN filename) 
{
  #define H_mus_sound_chans "(" S_mus_sound_chans " filename): channels of data in sound file"
  return(gmus_sound(S_mus_sound_chans, mus_sound_chans, filename));
}


static XEN g_mus_sound_set_chans(XEN filename, XEN val) 
{
  return(gmus_sound_set(S_setB S_mus_sound_chans, mus_sound_set_chans, filename, val));
}


XEN g_mus_sound_srate(XEN filename) 
{
  #define H_mus_sound_srate "(" S_mus_sound_srate " filename): sampling rate of sound file"
  return(gmus_sound(S_mus_sound_srate, mus_sound_srate, filename));
}


static XEN g_mus_sound_set_srate(XEN filename, XEN val) 
{
  return(gmus_sound_set(S_setB S_mus_sound_srate, mus_sound_set_srate, filename, val));
}


static XEN g_mus_sound_header_type(XEN filename) 
{
  #define H_mus_sound_header_type "(" S_mus_sound_header_type " filename): header type (e.g. " S_mus_aifc ") of sound file"
  return(gmus_sound(S_mus_sound_header_type, mus_sound_header_type, filename));
}


static XEN g_mus_sound_set_header_type(XEN filename, XEN val) 
{
  return(gmus_sound_set(S_setB S_mus_sound_header_type, mus_sound_set_header_type, filename, val));
}


static XEN g_mus_sound_data_format(XEN filename) 
{
  #define H_mus_sound_data_format "(" S_mus_sound_data_format " filename): data format (e.g. " S_mus_bshort ") of data in sound file"
  return(gmus_sound(S_mus_sound_data_format, mus_sound_data_format, filename));
}


static XEN g_mus_sound_set_data_format(XEN filename, XEN val) 
{
  return(gmus_sound_set(S_setB S_mus_sound_data_format, mus_sound_set_data_format, filename, val));
}


static XEN g_mus_sound_length(XEN filename) 
{
  #define H_mus_sound_length "(" S_mus_sound_length " filename): sound file length in bytes"
  return(glmus_sound(S_mus_sound_length, mus_sound_length, filename));
}


static XEN g_mus_sound_type_specifier(XEN filename) 
{
  #define H_mus_sound_type_specifier "(" S_mus_sound_type_specifier " filename): original sound file header type identifier (e.g. 0x2e736e64)"
  return(gmus_sound(S_mus_sound_type_specifier, mus_sound_type_specifier, filename));
}


static XEN g_mus_sound_forget(XEN filename) 
{
  #define H_mus_sound_forget "(" S_mus_sound_forget " filename): remove 'filename' from sound cache.  If you create, then later \
delete a sound file, " S_mus_sound_forget " can be used to clear it from sndlib's cache of sound files"
  return(gmus_sound(S_mus_sound_forget, mus_sound_forget, filename));
}


static XEN g_mus_sound_prune(void) 
{
  #define H_mus_sound_prune "(" S_mus_sound_prune "): remove all defunct entries from sndlib's sound file cache."
  return(C_TO_XEN_INT(mus_sound_prune()));
}


static XEN g_mus_sound_comment(XEN gfilename) 
{
  #define H_mus_sound_comment "(" S_mus_sound_comment " filename): comment (a string) found in sound file's header"
  char *res = NULL, *str = NULL; 
  XEN newstr;

  XEN_ASSERT_TYPE(XEN_STRING_P(gfilename), gfilename, XEN_ONLY_ARG, S_mus_sound_comment, "a string"); 

  res = mus_sound_comment(str = mus_expand_filename(XEN_TO_C_STRING(gfilename)));
  if (str) free(str);
  newstr = C_TO_XEN_STRING(res);
  if (res) free(res);
  return(newstr);
}


static XEN g_mus_sound_write_date(XEN filename) 
{
  char *str = NULL;
  XEN result;

  #define H_mus_sound_write_date "(" S_mus_sound_write_date " filename): write date of sound file"
  XEN_ASSERT_TYPE(XEN_STRING_P(filename), filename, XEN_ONLY_ARG, S_mus_sound_write_date, "a string"); 
  str = mus_expand_filename(XEN_TO_C_STRING(filename));
  result = C_TO_XEN_ULONG((unsigned long)mus_sound_write_date(str)); /* actually time_t */
  if (str) free(str);
  return(result);
}


static XEN g_mus_header_writable(XEN head, XEN data)
{
  #define H_mus_header_writable "(" S_mus_header_writable " header-type data-format) returns " PROC_TRUE " if the header can handle the data format"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(head), head, XEN_ARG_1, S_mus_header_writable, "a header type");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(data), data, XEN_ARG_2, S_mus_header_writable, "a data format");
  return(C_TO_XEN_BOOLEAN(mus_header_writable(XEN_TO_C_INT(head), XEN_TO_C_INT(data))));
}

static XEN g_mus_header_raw_defaults(void)
{
  #define H_mus_header_raw_defaults "(" S_mus_header_raw_defaults "): returns list '(srate chans format) of current raw sound default attributes"
  int srate, chans, data_format;
  mus_header_raw_defaults(&srate, &chans, &data_format);
  return(XEN_LIST_3(C_TO_XEN_INT(srate),
		    C_TO_XEN_INT(chans),
		    C_TO_XEN_INT(data_format)));
}


static XEN g_mus_header_set_raw_defaults(XEN lst)
{
  XEN_ASSERT_TYPE((XEN_LIST_P(lst)) && (XEN_LIST_LENGTH(lst) == 3), lst, XEN_ONLY_ARG, S_mus_header_raw_defaults, "a list: '(srate chans data-format)");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(XEN_CAR(lst)), XEN_CAR(lst), XEN_ARG_1, S_mus_header_raw_defaults, "an integer = srate");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(XEN_CADR(lst)), XEN_CADR(lst), XEN_ARG_2, S_mus_header_raw_defaults, "an integer = chans");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(XEN_CADDR(lst)), XEN_CADDR(lst), XEN_ARG_3, S_mus_header_raw_defaults, "an integer = data-format");
  mus_header_set_raw_defaults(XEN_TO_C_INT(XEN_CAR(lst)),
			      XEN_TO_C_INT(XEN_CADR(lst)),
			      XEN_TO_C_INT(XEN_CADDR(lst)));
  return(lst);
}


static XEN g_mus_header_type_name(XEN type) 
{
  #define H_mus_header_type_name "(" S_mus_header_type_name " type): header type (e.g. " S_mus_aiff ") as a string"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(type), type, XEN_ONLY_ARG, S_mus_header_type_name, "an integer (header-type id)"); 
  return(C_TO_XEN_STRING(mus_header_type_name(XEN_TO_C_INT(type))));
}


static XEN g_mus_header_type_to_string(XEN type) 
{
  #define H_mus_header_type_to_string "(" S_mus_header_type_to_string " type): header type (e.g. " S_mus_aiff ") as a string"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(type), type, XEN_ONLY_ARG, S_mus_header_type_to_string, "an integer (header-type id)"); 
  return(C_TO_XEN_STRING(mus_header_type_to_string(XEN_TO_C_INT(type))));
}


static XEN g_mus_data_format_name(XEN format) 
{
  #define H_mus_data_format_name "(" S_mus_data_format_name " format): data format (e.g. " S_mus_bshort ") as a string"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(format), format, XEN_ONLY_ARG, S_mus_data_format_name, "an integer (data-format id)"); 
  return(C_TO_XEN_STRING(mus_data_format_name(XEN_TO_C_INT(format))));
}


static XEN g_mus_data_format_to_string(XEN format) 
{
  #define H_mus_data_format_to_string "(" S_mus_data_format_to_string " format): data format (e.g. " S_mus_bshort ") as a string"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(format), format, XEN_ONLY_ARG, S_mus_data_format_to_string, "an integer (data-format id)"); 
  return(C_TO_XEN_STRING(mus_data_format_to_string(XEN_TO_C_INT(format))));
}


static XEN g_mus_bytes_per_sample(XEN format) 
{
  #define H_mus_bytes_per_sample "(" S_mus_bytes_per_sample " format): number of bytes per sample in \
format (e.g. " S_mus_bshort " = 2)"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(format), format, XEN_ONLY_ARG, S_mus_bytes_per_sample, "an integer (data-format id)"); 
  return(C_TO_XEN_INT(mus_bytes_per_sample(XEN_TO_C_INT(format))));
}


static XEN g_mus_sound_duration(XEN gfilename) 
{
  #define H_mus_sound_duration "(" S_mus_sound_duration " filename): duration (in seconds) of sound file"
  float res;
  char *str = NULL;

  XEN_ASSERT_TYPE(XEN_STRING_P(gfilename), gfilename, XEN_ONLY_ARG, S_mus_sound_duration, "a string"); 
  res = mus_sound_duration(str = mus_expand_filename(XEN_TO_C_STRING(gfilename)));
  if (str) free(str);
  return(C_TO_XEN_DOUBLE(res));

}


static XEN g_mus_oss_set_buffers(XEN num, XEN size)
{
  #define H_mus_oss_set_buffers "(" S_mus_oss_set_buffers " num size): set Linux OSS 'fragment' number and size. \
If Snd's controls seem sluggish, try (" S_mus_oss_set_buffers " 4 12) or even (" S_mus_oss_set_buffers " 2 12). \
This reduces the on-card buffering, but may introduce clicks."

#if (HAVE_OSS || HAVE_ALSA)
  XEN_ASSERT_TYPE(XEN_INTEGER_P(num), num, XEN_ARG_1, S_mus_oss_set_buffers, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(size), size, XEN_ARG_2, S_mus_oss_set_buffers, "an integer");
  mus_oss_set_buffers(XEN_TO_C_INT(num),
			    XEN_TO_C_INT(size));
#endif
  return(XEN_FALSE);
}


static XEN g_mus_alsa_buffers(void)
{
  #define H_mus_alsa_buffers "(" S_mus_alsa_buffers "): current number of ALSA periods."
#if HAVE_ALSA
  return(C_TO_XEN_INT(mus_alsa_buffers()));
#endif
  return(XEN_FALSE);
}


static XEN g_mus_alsa_set_buffers(XEN val)
{
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, S_setB S_mus_alsa_buffers, "an integer");
#if HAVE_ALSA
  return(C_TO_XEN_INT(mus_alsa_set_buffers(XEN_TO_C_INT(val))));
#endif
  return(XEN_FALSE);
}


static XEN g_mus_alsa_buffer_size(void)
{
  #define H_mus_alsa_buffer_size "(" S_mus_alsa_buffer_size "): current size of ALSA buffers."
#if HAVE_ALSA
  return(C_TO_XEN_INT(mus_alsa_buffer_size()));
#endif
  return(XEN_FALSE);
}


static XEN g_mus_alsa_set_buffer_size(XEN val)
{
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, S_setB S_mus_alsa_buffer_size, "an integer");
#if HAVE_ALSA
  return(C_TO_XEN_INT(mus_alsa_set_buffer_size(XEN_TO_C_INT(val))));
#endif
  return(XEN_FALSE);
}


static XEN g_mus_alsa_device(void)
{
  #define H_mus_alsa_device "(" S_mus_alsa_device "): current ALSA device."
#if HAVE_ALSA
  return(C_TO_XEN_STRING(mus_alsa_device()));
#endif
  return(XEN_FALSE);
}


static XEN g_mus_alsa_set_device(XEN val)
{
  XEN_ASSERT_TYPE(XEN_STRING_P(val), val, XEN_ONLY_ARG, S_setB S_mus_alsa_device, "a string (ALSA device name)");
#if HAVE_ALSA
  return(C_TO_XEN_STRING(mus_alsa_set_device(XEN_TO_C_STRING(val))));
#endif
  return(XEN_FALSE);
}


static XEN g_mus_alsa_playback_device(void)
{
  #define H_mus_alsa_playback_device "(" S_mus_alsa_playback_device "): current ALSA playback device."
#if HAVE_ALSA
  return(C_TO_XEN_STRING(mus_alsa_playback_device()));
#endif
  return(XEN_FALSE);
}


static XEN g_mus_alsa_set_playback_device(XEN val)
{
  XEN_ASSERT_TYPE(XEN_STRING_P(val), val, XEN_ONLY_ARG, S_setB S_mus_alsa_playback_device, "a string (ALSA device name)");
#if HAVE_ALSA
  return(C_TO_XEN_STRING(mus_alsa_set_playback_device(XEN_TO_C_STRING(val))));
#endif
  return(XEN_FALSE);
}


static XEN g_mus_alsa_capture_device(void)
{
  #define H_mus_alsa_capture_device "(" S_mus_alsa_capture_device "): current ALSA capture device."
#if HAVE_ALSA
  return(C_TO_XEN_STRING(mus_alsa_capture_device()));
#endif
  return(XEN_FALSE);
}


static XEN g_mus_alsa_set_capture_device(XEN val)
{
  XEN_ASSERT_TYPE(XEN_STRING_P(val), val, XEN_ONLY_ARG, S_setB S_mus_alsa_capture_device, "a string (ALSA device name)");
#if HAVE_ALSA
  return(C_TO_XEN_STRING(mus_alsa_set_capture_device(XEN_TO_C_STRING(val))));
#endif
  return(XEN_FALSE);
}


static XEN g_mus_alsa_squelch_warning(void)
{
  #define H_mus_alsa_squelch_warning "(" S_mus_alsa_squelch_warning "): whether to squelch ALSA srate mismatch warnings."
#if HAVE_ALSA
  return(C_TO_XEN_BOOLEAN(mus_alsa_squelch_warning()));
#endif
  return(XEN_FALSE);
}


static XEN g_mus_alsa_set_squelch_warning(XEN val)
{
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(val), val, XEN_ONLY_ARG, S_setB S_mus_alsa_squelch_warning, "a boolean");
#if HAVE_ALSA
  return(C_TO_XEN_BOOLEAN(mus_alsa_set_squelch_warning(XEN_TO_C_BOOLEAN(val))));
#endif
  return(XEN_FALSE);
}


static XEN g_mus_sound_maxamp_exists(XEN file)
{
  #define H_mus_sound_maxamp_exists "(" S_mus_sound_maxamp_exists " filename): " PROC_TRUE " if sound's maxamp data is available \
in the sound cache; if it isn't, a call on " S_mus_sound_maxamp " has to open and read the data to get the maxamp."
  bool val;
  char *str = NULL;

  XEN_ASSERT_TYPE(XEN_STRING_P(file), file, XEN_ONLY_ARG, S_mus_sound_maxamp_exists, "a string");
  val = mus_sound_maxamp_exists(str = mus_expand_filename(XEN_TO_C_STRING(file)));
  if (str) free(str);
  return(C_TO_XEN_BOOLEAN(val));
}


XEN g_mus_sound_maxamp(XEN file)
{
  #define H_mus_sound_maxamp "(" S_mus_sound_maxamp " filename): maxamps in sound (a list of paired amps (floats) and locations (samples))"
  int chans;
  char *filename;
  XEN res = XEN_EMPTY_LIST;

  XEN_ASSERT_TYPE(XEN_STRING_P(file), file, XEN_ONLY_ARG, S_mus_sound_maxamp, "a string");

  filename = mus_expand_filename(XEN_TO_C_STRING(file));
  chans = mus_sound_chans(filename);
  if (chans > 0)
    {
      mus_long_t rtn;
      int i;
      mus_float_t *vals;
      mus_long_t *times;

      vals = (mus_float_t *)calloc(chans, sizeof(mus_float_t));
      times = (mus_long_t *)calloc(chans, sizeof(mus_long_t));

      rtn = mus_sound_maxamps(filename, chans, vals, times);
      if (rtn != MUS_ERROR)
	for (i = chans - 1; i >= 0; i--)
	  res = XEN_CONS(C_TO_XEN_LONG_LONG(times[i]),
		  XEN_CONS(C_TO_XEN_DOUBLE(vals[i]), res));
      free(vals);
      free(times);
      if (filename) free(filename);
    }
  else 
    {
      if (filename) free(filename);
      XEN_ERROR(BAD_HEADER,
		XEN_LIST_1(C_TO_XEN_STRING(S_mus_sound_maxamp ": chans <= 0")));
    }
  return(res);
}


static XEN g_mus_sound_set_maxamp(XEN file, XEN vals)
{
  int chans;
  char *filename;

  XEN_ASSERT_TYPE(XEN_STRING_P(file), file, XEN_ARG_1, S_setB S_mus_sound_maxamp, "a string");
  XEN_ASSERT_TYPE(XEN_LIST_P(vals), vals, XEN_ARG_2, S_setB S_mus_sound_maxamp, "a list");

  filename = mus_expand_filename(XEN_TO_C_STRING(file));
  chans = mus_sound_chans(filename);

  /* presumably any actual error here will be trapped via mus-error (raised in mus_header_read via read_sound_file_header),
   *   so chans <= 0 is actually in the header?
   */
  if (chans > 0)
    {
      XEN lst;
      int i, j, len;
      mus_float_t *mvals;
      mus_long_t *times;

      len = XEN_LIST_LENGTH(vals);
      if (len < (chans * 2))
	XEN_WRONG_TYPE_ARG_ERROR(S_setB S_mus_sound_maxamp, 2, vals, "max amp list length must = 2 * chans");
      if (len > chans * 2) len = chans * 2;

      mvals = (mus_float_t *)calloc(chans, sizeof(mus_float_t));
      times = (mus_long_t *)calloc(chans, sizeof(mus_long_t));

      for (i = 0, j = 0, lst = XEN_COPY_ARG(vals); i < len; i += 2, j++, lst = XEN_CDDR(lst))
	{
	  times[j] = XEN_TO_C_LONG_LONG_OR_ELSE(XEN_CAR(lst), 0);
	  mvals[j] = XEN_TO_C_DOUBLE(XEN_CADR(lst));
	}

      mus_sound_set_maxamps(filename, chans, mvals, times);
      free(mvals);
      free(times);
      if (filename) free(filename);
    }
  else 
    {
      if (filename) free(filename);
      XEN_ERROR(BAD_HEADER,
		XEN_LIST_1(C_TO_XEN_STRING(S_setB S_mus_sound_maxamp ": chans <= 0")));
    }
  return(vals);
}


static XEN g_mus_sound_open_input(XEN file)
{
  #define H_mus_sound_open_input "(" S_mus_sound_open_input " filename): open filename for (low-level) sound input, \
return file descriptor (an integer)"
  int fd;
  char *str = NULL;

  XEN_ASSERT_TYPE(XEN_STRING_P(file), file, XEN_ONLY_ARG, S_mus_sound_open_input, "a string");
  fd = mus_sound_open_input(str = mus_expand_filename(XEN_TO_C_STRING(file)));
  if (str) free(str);
  return(C_TO_XEN_INT(fd));
}


static XEN g_mus_sound_open_output(XEN file, XEN srate, XEN chans, XEN data_format, XEN header_type, XEN comment)
{

  #define H_mus_sound_open_output "(" S_mus_sound_open_output " filename :optional (srate 44100) (chans 1) data-format header-type (comment \"\")): \
open filename for (low-level) sound output with the given srate and so on; return the file descriptor (an integer). \
The file size is normally set later via " S_mus_sound_close_output ". srate is an integer, comment is a string, \
data-format is a sndlib format indicator such as " S_mus_bshort ", if " PROC_FALSE " if defaults to a format compatible with sndlib, \
header-type is a sndlib type indicator such as " S_mus_aiff "; sndlib currently only writes 5 or so header types."

  int fd = -1, df;
  XEN_ASSERT_TYPE(XEN_STRING_P(file), file, XEN_ARG_1, S_mus_sound_open_output, "a string");
  XEN_ASSERT_TYPE(XEN_INTEGER_OR_BOOLEAN_IF_BOUND_P(srate), srate, XEN_ARG_2, S_mus_sound_open_output, "an integer or " PROC_FALSE);
  XEN_ASSERT_TYPE(XEN_INTEGER_OR_BOOLEAN_IF_BOUND_P(chans), chans, XEN_ARG_3, S_mus_sound_open_output, "an integer or " PROC_FALSE);
  XEN_ASSERT_TYPE(XEN_INTEGER_OR_BOOLEAN_IF_BOUND_P(data_format), data_format, XEN_ARG_4, S_mus_sound_open_output, "a data-format or " PROC_FALSE);
  XEN_ASSERT_TYPE(XEN_INTEGER_OR_BOOLEAN_IF_BOUND_P(header_type), header_type, XEN_ARG_5, S_mus_sound_open_output, "a header-type or " PROC_FALSE);
  XEN_ASSERT_TYPE((XEN_STRING_P(comment) || (XEN_NOT_BOUND_P(comment))), comment, XEN_ARG_6, S_mus_sound_open_output, "a string");

  df = XEN_TO_C_INT_OR_ELSE(data_format, (int)MUS_OUT_FORMAT);
  if (mus_data_format_p(df))
    {
      int ht;
#if MUS_LITTLE_ENDIAN
      ht = XEN_TO_C_INT_OR_ELSE(header_type, (int)MUS_RIFF);
#else
      ht = XEN_TO_C_INT_OR_ELSE(header_type, (int)MUS_NEXT);
#endif
      /* now check that data format and header type are ok together */
      if (mus_header_type_p(ht))
	{
	  int chns;

	  if (!mus_header_writable(ht, df))
	    {
	      if ((XEN_INTEGER_P(data_format)) &&
		  (XEN_INTEGER_P(header_type)))
		XEN_ERROR(XEN_ERROR_TYPE("bad-data"),
			  XEN_LIST_3(C_TO_XEN_STRING(S_mus_sound_open_output ": ~A header can't handle ~A data"),
				     C_TO_XEN_STRING(mus_header_type_name(ht)),
				     C_TO_XEN_STRING(mus_data_format_name(df))));

	      if (!(XEN_INTEGER_P(data_format)))
		{
		  switch (df)
		    {
		    case MUS_BFLOAT:  df = MUS_LFLOAT;  break;
		    case MUS_BDOUBLE: df = MUS_LDOUBLE; break;
		    case MUS_BINT:    df = MUS_LINT;    break;
		    case MUS_LFLOAT:  df = MUS_BFLOAT;  break;
		    case MUS_LDOUBLE: df = MUS_BDOUBLE; break;
		    case MUS_LINT:    df = MUS_BINT;    break;
		    }
		  if (!mus_header_writable(ht, df))
		    {
		      int i;
		      for (i = 1; i < MUS_NUM_DATA_FORMATS; i++) /* MUS_UNSUPPORTED is 0 */
			{
			  df = i;
			  if (mus_header_writable(ht, df))
			    break;
			}
		    }
		}
	      else
		{
		  ht = MUS_NEXT;
		}
	  
	      if (!mus_header_writable(ht, df))
		XEN_ERROR(XEN_ERROR_TYPE("bad-data"),
			  XEN_LIST_3(C_TO_XEN_STRING(S_mus_sound_open_output ": ~A header can't handle ~A data"),
				     C_TO_XEN_STRING(mus_header_type_name(ht)),
				     C_TO_XEN_STRING(mus_data_format_name(df))));
	    }

	  chns = XEN_TO_C_INT_OR_ELSE(chans, 1);
	  if (chns > 0)
	    {
	      const char *com = NULL;
	      char *str = NULL;
	      if (XEN_STRING_P(comment)) com = XEN_TO_C_STRING(comment);
	      fd = mus_sound_open_output(str = mus_expand_filename(XEN_TO_C_STRING(file)),
					 XEN_TO_C_INT_OR_ELSE(srate, 44100),  /* not DEFAULT_OUTPUT_SRATE here because that depends on Snd */
					 chns, df, ht, com);
	      if (str) free(str);
	    }
	  else XEN_OUT_OF_RANGE_ERROR(S_mus_sound_open_output, 3, chans, "chans <= 0?");
	}
      else XEN_OUT_OF_RANGE_ERROR(S_mus_sound_open_output, 5, header_type, "invalid header type");
    }
  else XEN_OUT_OF_RANGE_ERROR(S_mus_sound_open_output, 4, data_format, "invalid data format");
  return(C_TO_XEN_INT(fd));
}


static XEN g_mus_sound_reopen_output(XEN file, XEN chans, XEN data_format, XEN header_type, XEN data_loc)
{

  #define H_mus_sound_reopen_output "(" S_mus_sound_reopen_output " filename :optional (chans 1) data-format header-type data-location): \
reopen (without alteration) filename for output \
data-format and header-type are sndlib indicators such as " S_mus_bshort " or " S_mus_aiff ". \
data-location should be retrieved from a previous call to " S_mus_sound_data_location "."

  int fd = -1, df;
  char *filename;
  XEN_ASSERT_TYPE(XEN_STRING_P(file), file, XEN_ARG_1, S_mus_sound_reopen_output, "a string");
  XEN_ASSERT_TYPE(XEN_INTEGER_OR_BOOLEAN_IF_BOUND_P(chans), chans, XEN_ARG_2, S_mus_sound_reopen_output, "an integer or " PROC_FALSE);
  XEN_ASSERT_TYPE(XEN_INTEGER_OR_BOOLEAN_IF_BOUND_P(data_format), data_format, XEN_ARG_3, S_mus_sound_reopen_output, "a data-format or " PROC_FALSE);
  XEN_ASSERT_TYPE(XEN_INTEGER_OR_BOOLEAN_IF_BOUND_P(header_type), header_type, XEN_ARG_4, S_mus_sound_reopen_output, "a header-type or " PROC_FALSE);
  XEN_ASSERT_TYPE(XEN_LONG_LONG_P(data_loc) || XEN_FALSE_P(data_loc) || XEN_NOT_BOUND_P(data_loc), 
		  data_loc, XEN_ARG_5, S_mus_sound_reopen_output, "an integer or " PROC_FALSE);

  filename = mus_expand_filename(XEN_TO_C_STRING(file));

  /* use existing settings if unbound as args */
  if (XEN_INTEGER_P(data_format))
    df = XEN_TO_C_INT(data_format);
  else
    {
      df = mus_sound_data_format(filename);
      if (df == MUS_ERROR)
	df = MUS_OUT_FORMAT;
    }
  if (mus_data_format_p(df))
    {
      int ht;
      if (XEN_INTEGER_P(header_type))
	ht = XEN_TO_C_INT(header_type);
      else
	{
	  ht = mus_sound_header_type(filename);
	  if (ht == MUS_ERROR)
#if MUS_LITTLE_ENDIAN
	    ht = MUS_RIFF;
#else
	    ht = MUS_NEXT;
#endif
	}
      if (mus_header_type_p(ht))
	{
	  int chns;
	  if (XEN_INTEGER_P(chans))
	    chns = XEN_TO_C_INT(chans);
	  else
	    {
	      chns = mus_sound_chans(filename);
	      if (chns == MUS_ERROR)
		chns = 1;
	    }
	  if (chns > 0)
	    {
	      mus_long_t dloc;
	      if (XEN_LONG_LONG_P(data_loc))
		dloc = XEN_TO_C_LONG_LONG(data_loc);
	      else
		{
		  dloc = mus_sound_data_location(filename);
		  if (dloc == MUS_ERROR)
		    dloc = 0;
		}
	      fd = mus_sound_reopen_output(filename, chns, df, ht, dloc);
	      if (filename) free(filename);
	    }
	  else XEN_OUT_OF_RANGE_ERROR(S_mus_sound_reopen_output, 2, chans, "chans <= 0?");
	}
      else XEN_OUT_OF_RANGE_ERROR(S_mus_sound_reopen_output, 4, header_type, "invalid header type");
    }
  else XEN_OUT_OF_RANGE_ERROR(S_mus_sound_reopen_output, 3, data_format, "invalid data format");
  return(C_TO_XEN_INT(fd));
}


#if _MSC_VER
#ifndef STDIN_FILENO
#define STDIN_FILENO 0
#endif
#ifndef STDOUT_FILENO
#define STDOUT_FILENO 1
#endif
#ifndef STDERR_FILENO
#define STDERR_FILENO 2
#endif
#endif

static XEN g_mus_sound_close_input(XEN fd)
{
  #define H_mus_sound_close_input "(" S_mus_sound_close_input " fd): close (low-level) file fd that was opened \
by " S_mus_sound_open_input "."
  int nfd;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(fd), fd, XEN_ONLY_ARG, S_mus_sound_close_input, "an integer");
  nfd = XEN_TO_C_INT(fd);
  if ((nfd < 0) || (nfd == STDIN_FILENO) || (nfd == STDOUT_FILENO) || (nfd == STDERR_FILENO))
    XEN_OUT_OF_RANGE_ERROR(S_mus_sound_close_input, 1, fd, "invalid file number");
  return(C_TO_XEN_INT(mus_sound_close_input(XEN_TO_C_INT(fd))));
}


static XEN g_mus_sound_close_output(XEN fd, XEN bytes)
{
  #define H_mus_sound_close_output "(" S_mus_sound_close_output " fd bytes): close (low-level) file fd \
that was opened by " S_mus_sound_open_output " after updating its header (if any) to reflect bytes, the new file data size"

  int nfd;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(fd), fd, XEN_ARG_1, S_mus_sound_close_output, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(bytes), bytes, XEN_ARG_2, S_mus_sound_close_output, "a number");
  nfd = XEN_TO_C_INT(fd);
  if ((nfd < 0) || (nfd == STDIN_FILENO) || (nfd == STDOUT_FILENO) || (nfd == STDERR_FILENO))
    XEN_OUT_OF_RANGE_ERROR(S_mus_sound_close_output, 1, fd, "invalid file number");
  return(C_TO_XEN_INT(mus_sound_close_output(XEN_TO_C_INT(fd),
					     XEN_TO_C_LONG_LONG_OR_ELSE(bytes, 0))));
}


static XEN g_mus_sound_read(XEN fd, XEN beg, XEN end, XEN chans, XEN sv)
{
  #define H_mus_sound_read "(" S_mus_sound_read " fd beg end chans sdata): read sound data from file fd, \
filling sound-data sdata's buffers starting at beg (buffer location), going to end"

  sound_data *sd;
  mus_long_t bg, nd;
  int i, nchans;
  XEN val;
  mus_float_t **bufs;

  XEN_ASSERT_TYPE(XEN_INTEGER_P(fd), fd, XEN_ARG_1, S_mus_sound_read, "an integer");
  XEN_ASSERT_TYPE(XEN_LONG_LONG_P(beg), beg, XEN_ARG_2, S_mus_sound_read, "an integer");
  XEN_ASSERT_TYPE(XEN_LONG_LONG_P(end), end, XEN_ARG_3, S_mus_sound_read, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(chans), chans, XEN_ARG_4, S_mus_sound_read, "an integer");
  XEN_ASSERT_TYPE(sound_data_p(sv), sv, XEN_ARG_5, S_mus_sound_read, "a sound-data object");

  sd = XEN_TO_SOUND_DATA(sv);
  bg = XEN_TO_C_LONG_LONG(beg);
  nd = XEN_TO_C_LONG_LONG(end);
  if ((nd - bg) >= mus_sound_data_length(sd))
    XEN_ERROR(XEN_ERROR_TYPE("out-of-range"),
	      XEN_LIST_4(C_TO_XEN_STRING(S_mus_sound_read ": end - beg (~A - ~A) >= sound-data array length, ~A"),
			 end, 
			 beg, 
			 C_TO_XEN_LONG_LONG(mus_sound_data_length(sd))));

  nchans = mus_sound_data_chans(sd);
  bufs = (mus_float_t **)malloc(nchans * sizeof(mus_float_t *));
  for (i = 0; i < nchans; i++)
    bufs[i] = mus_sound_data_channel_data(sd, i);

  val = C_TO_XEN_INT(mus_file_read(XEN_TO_C_INT(fd), bg, nd, XEN_TO_C_INT(chans), bufs));
  
  free(bufs);
  return(val);
}


static XEN g_mus_sound_write(XEN fd, XEN beg, XEN end, XEN chans, XEN sv)
{
  #define H_mus_sound_write "(" S_mus_sound_write " fd beg end chans sdata): write sound-data sdata's data to file fd, \
starting at beg (buffer location), going to end"

  sound_data *sd;
  mus_long_t bg, nd;
  int i, nchans;
  XEN val;
  mus_float_t **bufs;

  XEN_ASSERT_TYPE(XEN_INTEGER_P(fd), fd, XEN_ARG_1, S_mus_sound_write, "an integer");
  XEN_ASSERT_TYPE(XEN_LONG_LONG_P(beg), beg, XEN_ARG_2, S_mus_sound_write, "an integer");
  XEN_ASSERT_TYPE(XEN_LONG_LONG_P(end), end, XEN_ARG_3, S_mus_sound_write, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(chans), chans, XEN_ARG_4, S_mus_sound_write, "an integer");
  XEN_ASSERT_TYPE(sound_data_p(sv), sv, XEN_ARG_5, S_mus_sound_write, "a sound-data object");

  /* even here we can write memory that doesn't belong to us if clipping */
  sd = XEN_TO_SOUND_DATA(sv);
  bg = XEN_TO_C_LONG_LONG(beg);
  nd = XEN_TO_C_LONG_LONG(end);
  if ((nd - bg) >= mus_sound_data_length(sd))
    XEN_ERROR(XEN_ERROR_TYPE("out-of-range"),
	      XEN_LIST_4(C_TO_XEN_STRING(S_mus_sound_write ": end - beg (~A - ~A) >= sound-data array length, ~A"),
			 end, 
			 beg, 
			 C_TO_XEN_LONG_LONG(mus_sound_data_length(sd))));

  nchans = mus_sound_data_chans(sd);
  bufs = (mus_float_t **)malloc(nchans * sizeof(mus_float_t *));
  for (i = 0; i < nchans; i++)
    bufs[i] = mus_sound_data_channel_data(sd, i);

  val = C_TO_XEN_INT(mus_file_write(XEN_TO_C_INT(fd), bg, nd, XEN_TO_C_INT(chans), bufs));
  
  free(bufs);
  return(val);
}


static XEN g_mus_sound_seek_frame(XEN fd, XEN offset)
{
  #define H_mus_sound_seek_frame "(" S_mus_sound_seek_frame " fd frame): move the current read/write location in file fd \
to the frame offset"

  XEN_ASSERT_TYPE(XEN_INTEGER_P(fd), fd, XEN_ARG_1, S_mus_sound_seek_frame, "an integer");
  XEN_ASSERT_TYPE(XEN_LONG_LONG_P(offset), offset, XEN_ARG_2, S_mus_sound_seek_frame, "an integer");
  return(C_TO_XEN_LONG_LONG(mus_file_seek_frame(XEN_TO_C_INT(fd),
					    XEN_TO_C_LONG_LONG(offset))));
}


/* since mus-audio-read|write assume sound-data vectors communicating with Scheme,
 *   we can't assume that the user has had a chance to deal with type problems.
 *   So, we keep track of each audio line's current read|write format and
 *   translate in mus_audio_read|write if necessary
 */

static int audio_io_size = 0;
static int *audio_io_lines = NULL;
static int *audio_io_formats = NULL;

int mus_audio_io_format(int line);
int mus_audio_io_format(int line)
{
  int i;
  for (i = 0; i < audio_io_size; i++)
    if (audio_io_lines[i] == line)
      return(audio_io_formats[i]);
  return(MUS_UNKNOWN);
}


#define audio_io_read_format(Line) (mus_audio_io_format(Line) & 0xffff)
#define audio_io_write_format(Line) (mus_audio_io_format(Line) >> 16)

static void audio_io_set_format(int line, int format)
{
  int i, old_size;

  for (i = 0; i < audio_io_size; i++)
    if (audio_io_lines[i] == line)
      {
	audio_io_formats[i] = format;
	return;
      }

  /* we get here if the table is not big enough */
  old_size = audio_io_size;
  audio_io_size += 8;
  if (old_size == 0)
    {
      audio_io_lines = (int *)malloc(audio_io_size * sizeof(int));
      audio_io_formats = (int *)malloc(audio_io_size * sizeof(int));
    }
  else
    {
      audio_io_lines = (int *)realloc(audio_io_lines, audio_io_size * sizeof(int));
      audio_io_formats = (int *)realloc(audio_io_formats, audio_io_size * sizeof(int));
    }
  for (i = old_size + 1; i < audio_io_size; i++)
    {
      audio_io_lines[i] = -1;
      audio_io_formats[i] = MUS_UNKNOWN;
    }
  audio_io_lines[old_size] = line;
  audio_io_formats[old_size] = format;
}


#define audio_io_set_read_format(Line, Format) audio_io_set_format(Line, ((mus_audio_io_format(Line) & 0xffff0000) | Format))
#define audio_io_set_write_format(Line, Format) audio_io_set_format(Line, ((mus_audio_io_format(Line) & 0xffff) | (Format << 16)))


static XEN g_mus_audio_open_output(XEN dev, XEN srate, XEN chans, XEN format, XEN size)
{
  #if HAVE_SCHEME
    #define audio_open_example "(" S_mus_audio_open_output " " S_mus_audio_default " 22050 1 " S_mus_lshort " 256)"
  #endif
  #if HAVE_RUBY
    #define audio_open_example "mus_audio_open_output(Mus_audio_default, 22050, 1, Mus_lshort, 256)"
  #endif
  #if HAVE_FORTH
    #define audio_open_example "mus-audio-default 22050 1 mus-lshort 256 mus-audio-open-output"
  #endif

  #define H_mus_audio_open_output "(" S_mus_audio_open_output " device srate chans format bytes): \
open the audio device ready for output at the given srate and so on; \
return the audio line number:\n  " audio_open_example

  int line, idev, ifmt, isize, israte, ichans;

  XEN_ASSERT_TYPE(XEN_INTEGER_P(dev), dev, XEN_ARG_1, S_mus_audio_open_output, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(srate), srate, XEN_ARG_2, S_mus_audio_open_output, "a number");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(chans), chans, XEN_ARG_3, S_mus_audio_open_output, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(format), format, XEN_ARG_4, S_mus_audio_open_output, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(size), size, XEN_ARG_5, S_mus_audio_open_output, "a number");

  idev = XEN_TO_C_INT(dev);
  israte = XEN_TO_C_INT_OR_ELSE(srate, 0);
  ichans = XEN_TO_C_INT(chans);
  ifmt = XEN_TO_C_INT(format);
  isize = XEN_TO_C_INT_OR_ELSE(size, 0);

  if (!(mus_data_format_p(ifmt)))
    XEN_OUT_OF_RANGE_ERROR(S_mus_audio_open_output, 4, format, "invalid data format");
  if (isize < 0)
    XEN_OUT_OF_RANGE_ERROR(S_mus_audio_open_output, 5, size, "size < 0?");
  if (israte <= 0)
    XEN_OUT_OF_RANGE_ERROR(S_mus_audio_open_output, 2, srate, "srate <= 0?");
  if ((ichans <= 0) || (ichans > 256))
    XEN_OUT_OF_RANGE_ERROR(S_mus_audio_open_output, 3, chans, "chans <= 0 or > 256?");

  line = mus_audio_open_output(idev, israte, ichans, ifmt, isize);
  audio_io_set_write_format(line, ifmt);
  return(C_TO_XEN_INT(line));
}


static XEN g_mus_audio_open_input(XEN dev, XEN srate, XEN chans, XEN format, XEN size)
{
  #define H_mus_audio_open_input "(" S_mus_audio_open_input " device srate chans format bufsize): \
open the audio device ready for input with the indicated attributes; return the audio line number"

  int line, idev, ifmt, isize, israte, ichans;

  XEN_ASSERT_TYPE(XEN_INTEGER_P(dev), dev, XEN_ARG_1, S_mus_audio_open_input, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(srate), srate, XEN_ARG_2, S_mus_audio_open_input, "a number");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(chans), chans, XEN_ARG_3, S_mus_audio_open_input, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(format), format, XEN_ARG_4, S_mus_audio_open_input, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(size), size, XEN_ARG_5, S_mus_audio_open_input, "a number");

  idev = XEN_TO_C_INT(dev);
  israte = XEN_TO_C_INT_OR_ELSE(srate, 0);
  ichans = XEN_TO_C_INT(chans);
  ifmt = XEN_TO_C_INT(format);
  isize = XEN_TO_C_INT_OR_ELSE(size, 0);

  if (!(mus_data_format_p(ifmt)))
    XEN_OUT_OF_RANGE_ERROR(S_mus_audio_open_input, 4, format, "invalid data format");
  if (isize < 0)
    XEN_OUT_OF_RANGE_ERROR(S_mus_audio_open_input, 5, size, "size < 0?");
  if (israte <= 0)
    XEN_OUT_OF_RANGE_ERROR(S_mus_audio_open_input, 2, srate, "srate <= 0?");
  if (ichans <= 0)
    XEN_OUT_OF_RANGE_ERROR(S_mus_audio_open_input, 3, chans, "chans <= 0?");

  line = mus_audio_open_input(idev, israte, ichans, ifmt, isize);
  audio_io_set_read_format(line, ifmt);
  return(C_TO_XEN_INT(line));
}


static XEN g_mus_audio_close(XEN line)
{
  int res;
  #define H_mus_audio_close "(" S_mus_audio_close " line): close the audio hardware line"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(line), line, XEN_ONLY_ARG, S_mus_audio_close, "an integer");
  res = (int)XEN_TO_C_INT(line);
  if (res < 0)
    XEN_OUT_OF_RANGE_ERROR(S_mus_audio_close, 1, line, "line < 0?");
  res = mus_audio_close(res);
  return(C_TO_XEN_INT(res));
}


/* these take a sndlib buffer (sound_data) and handle the conversion to the interleaved char* internally */
/* so, they take "frames", not "bytes", and a sound_data object, not char* etc */

static XEN g_mus_audio_write(XEN line, XEN sdata, XEN frames, XEN start)
{
  #define H_mus_audio_write "(" S_mus_audio_write " line sdata frames (start 0)): write frames of data (channels * frames = samples) \
to the audio line from sound-data sdata."

  char *obuf;
  sound_data *sd;
  int outbytes, val, fmt, fd, i, chans;
  mus_long_t frms, beg = 0;
  mus_float_t **bufs;

  XEN_ASSERT_TYPE(XEN_INTEGER_P(line), line, XEN_ARG_1, S_mus_audio_write, "an integer");
  XEN_ASSERT_TYPE(sound_data_p(sdata), sdata, XEN_ARG_2, S_mus_audio_write, "a sound-data object");
  XEN_ASSERT_TYPE(XEN_LONG_LONG_P(frames), frames, XEN_ARG_3, S_mus_audio_write, "an integer");
  XEN_ASSERT_TYPE(XEN_LONG_LONG_P(start) || XEN_NOT_BOUND_P(start), start, XEN_ARG_4, S_mus_audio_write, "an integer");

  sd = XEN_TO_SOUND_DATA(sdata);
  frms = XEN_TO_C_LONG_LONG(frames);

  if (frms > mus_sound_data_length(sd))
    XEN_OUT_OF_RANGE_ERROR(S_mus_audio_write, 3, frames, "frames > sound-data buffer length");

  if (XEN_BOUND_P(start))
    beg = XEN_TO_C_LONG_LONG(start);

  chans = mus_sound_data_chans(sd);

  fd = XEN_TO_C_INT(line);
  fmt = audio_io_write_format(fd);
  outbytes = frms * chans * mus_bytes_per_sample(fmt);
  obuf = (char *)calloc(outbytes, sizeof(char));

  bufs = (mus_float_t **)malloc(chans * sizeof(mus_float_t *));
  for (i = 0; i < chans; i++)
    bufs[i] = mus_sound_data_channel_data(sd, i);

  mus_file_write_buffer(fmt, beg, beg + frms - 1, chans, bufs, obuf, true); /* true -> clipped */
  val = mus_audio_write(fd, obuf, outbytes);
  free(obuf);
  free(bufs);

  return(C_TO_XEN_INT(val));
}


static XEN g_mus_audio_read(XEN line, XEN sdata, XEN frames)
{
  #define H_mus_audio_read "(" S_mus_audio_read " line sdata frames): read frames of data (channels * frames = samples) \
from the audio line into sound-data sdata."

  char *inbuf;
  sound_data *sd;
  int val, inbytes, fd, fmt, i, chans;
  mus_long_t frms;
  mus_float_t **bufs;

  XEN_ASSERT_TYPE(XEN_INTEGER_P(line), line, XEN_ARG_1, S_mus_audio_read, "an integer");
  XEN_ASSERT_TYPE(sound_data_p(sdata), sdata, XEN_ARG_2, S_mus_audio_read, "a sound-data object");
  XEN_ASSERT_TYPE(XEN_LONG_LONG_P(frames), frames, XEN_ARG_3, S_mus_audio_read, "an integer");

  sd = XEN_TO_SOUND_DATA(sdata);
  frms = XEN_TO_C_LONG_LONG(frames);
  fd = XEN_TO_C_INT(line);

  chans = mus_sound_data_chans(sd);
  fmt = audio_io_read_format(fd);
  inbytes = frms * chans * mus_bytes_per_sample(fmt);
  inbuf = (char *)calloc(inbytes, sizeof(char));
  val = mus_audio_read(fd, inbuf, inbytes);

  bufs = (mus_float_t **)malloc(chans * sizeof(mus_float_t *));
  for (i = 0; i < chans; i++)
    bufs[i] = mus_sound_data_channel_data(sd, i);

  mus_file_read_buffer(fmt, 0, chans, frms, bufs, inbuf); /* frms here because this is "nints" not "end"! */
  free(inbuf);
  free(bufs);

  return(C_TO_XEN_INT(val));
}


/* global default clipping values */

static XEN g_mus_clipping(void)
{
  #define H_mus_clipping "(" S_mus_clipping "): whether sound data written to a file should be clipped"
  return(C_TO_XEN_BOOLEAN(mus_clipping()));
}


static XEN g_mus_set_clipping(XEN clipped)
{
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(clipped), clipped, XEN_ONLY_ARG, S_setB S_mus_clipping, "a boolean");
  return(C_TO_XEN_BOOLEAN(mus_set_clipping(XEN_TO_C_BOOLEAN(clipped))));
}


/* file local clipping values */

static XEN g_mus_file_clipping(XEN fd)
{
  #define H_mus_file_clipping "(" S_mus_file_clipping " fd): whether sound data written to file 'fd' should be clipped"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(fd), fd, XEN_ONLY_ARG, S_mus_file_clipping, "an integer");
  return(C_TO_XEN_BOOLEAN(mus_file_clipping(XEN_TO_C_INT(fd))));
}


static XEN g_mus_file_set_clipping(XEN fd, XEN clipped)
{
  XEN_ASSERT_TYPE(XEN_INTEGER_P(fd), fd, XEN_ARG_1, S_setB S_mus_file_clipping, "an integer");
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(clipped), clipped, XEN_ARG_2, S_setB S_mus_file_clipping, "a boolean");
  return(C_TO_XEN_BOOLEAN(mus_file_set_clipping(XEN_TO_C_INT(fd), XEN_TO_C_BOOLEAN(clipped))));
}



XEN g_mus_expand_filename(XEN file)
{
  #define H_mus_expand_filename "(" S_mus_expand_filename " name): expand 'name' into a canonical or absolute filename, that is, \
one in which all directories in the path are explicit."
  char *str = NULL;
  XEN result;

  XEN_ASSERT_TYPE(XEN_STRING_P(file), file, XEN_ONLY_ARG, S_mus_expand_filename, "a string");
  str = mus_expand_filename(XEN_TO_C_STRING(file));
  result = C_TO_XEN_STRING(str);
  if (str) free(str);
  return(result);
}


static XEN g_mus_sound_report_cache(XEN file)
{
  #define H_mus_sound_report_cache "(" S_mus_sound_report_cache " (name)): print the current sound \
cache info to the file given or stdout"
  FILE *fd;
  const char *name;
  char *str = NULL;

  if (XEN_NOT_BOUND_P(file))
    {
      mus_sound_report_cache(stdout);
      return(XEN_FALSE);
    }

  XEN_ASSERT_TYPE(XEN_STRING_P(file), file, XEN_ONLY_ARG, S_mus_sound_report_cache, "a string");
  name = XEN_TO_C_STRING(file);
  if (name)
    {
      str = mus_expand_filename(name);
      if (str)
	{
	  fd = FOPEN(str, "w");
	  free(str);
	  if (fd)
	    {
	      mus_sound_report_cache(fd);
	      FCLOSE(fd, name);
	      return(file);
	    }
	}
    }

  XEN_ERROR(XEN_ERROR_TYPE("cannot-save"),
	    XEN_LIST_3(C_TO_XEN_STRING(S_mus_sound_report_cache ": ~S ~A"),
		       file,
		       C_TO_XEN_STRING(STRERROR(errno))));
  return(XEN_FALSE);
}


static XEN g_mus_error_type_to_string(XEN err)
{
  #define H_mus_error_type_to_string "(" S_mus_error_type_to_string " err): string description of err (a sndlib error type)"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(err), err, XEN_ONLY_ARG, S_mus_error_type_to_string, "an integer");
  return(C_TO_XEN_STRING((char *)mus_error_type_to_string(XEN_TO_C_INT(err))));
}


static XEN g_array_to_file(XEN filename, XEN data, XEN len, XEN srate, XEN channels)
{
  #define H_array_to_file "(" S_array_to_file " filename data len srate channels): write 'data', \
a vct of interleaved samples, to the sound file 'filename' set up to have the given \
srate and channels.  'len' samples are written."

  /* this exists for compatibility with the Common Lisp version of CLM. Ideally, we'd get rid
   *   of it and provide vct<->file and sound-data<->file instead so that the channels aren't
   *   handled through interleaving.  But that means extensive changes to the Lisp code...
   */

  mus_long_t olen, samps;
  vct *v;

  XEN_ASSERT_TYPE(XEN_STRING_P(filename), filename, XEN_ARG_1, S_array_to_file, "a string");
  XEN_ASSERT_TYPE(MUS_VCT_P(data), data, XEN_ARG_2, S_array_to_file, "a vct");
  XEN_ASSERT_TYPE(XEN_LONG_LONG_P(len), len, XEN_ARG_3, S_array_to_file, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(srate), srate, XEN_ARG_4, S_array_to_file, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(channels), channels, XEN_ARG_5, S_array_to_file, "an integer");

  v = XEN_TO_VCT(data);
  samps = XEN_TO_C_LONG_LONG(len);
  if (samps <= 0)
    XEN_OUT_OF_RANGE_ERROR(S_array_to_file, 3, len, "samples <= 0?");
  if (samps > mus_vct_length(v))
    samps = mus_vct_length(v);

  olen = mus_float_array_to_file(XEN_TO_C_STRING(filename),
				 mus_vct_data(v),
				 samps,
				 XEN_TO_C_INT(srate),
				 XEN_TO_C_INT(channels));
  return(C_TO_XEN_LONG_LONG(olen));
}


static XEN g_file_to_array(XEN filename, XEN chan, XEN start, XEN samples, XEN data)
{
  #define H_file_to_array "(" S_file_to_array " filename chan start samples data): read the sound file \
'filename' placing samples from channel 'chan' into the vct 'data' starting in the file \
at frame 'start' and reading 'samples' samples altogether."

  int chn;
  mus_long_t samps;
  vct *v;
  const char *name = NULL;

  XEN_ASSERT_TYPE(XEN_STRING_P(filename), filename, XEN_ARG_1, S_file_to_array, "a string");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(chan), chan, XEN_ARG_2, S_file_to_array, "an integer");
  XEN_ASSERT_TYPE(XEN_LONG_LONG_P(start), start, XEN_ARG_3, S_file_to_array, "an integer");
  XEN_ASSERT_TYPE(XEN_LONG_LONG_P(samples), samples, XEN_ARG_4, S_file_to_array, "an integer");
  XEN_ASSERT_TYPE((MUS_VCT_P(data)), data, XEN_ARG_5, S_file_to_array, "a vct");

  name = XEN_TO_C_STRING(filename);
  if (!(mus_file_probe(name)))
    XEN_ERROR(NO_SUCH_FILE,
	      XEN_LIST_3(C_TO_XEN_STRING(S_file_to_array ": ~S ~A"),
			 filename,
			 C_TO_XEN_STRING(STRERROR(errno))));

  v = XEN_TO_VCT(data);

  samps = XEN_TO_C_LONG_LONG(samples);
  if (samps <= 0) 
    XEN_OUT_OF_RANGE_ERROR(S_file_to_array, 4, samples, "samples <= 0?");
  if (samps > mus_vct_length(v))
    samps = mus_vct_length(v);

  chn = XEN_TO_C_INT(chan);
  if ((chn < 0) || (chn > mus_sound_chans(name)))
    XEN_ERROR(NO_SUCH_CHANNEL,
	      XEN_LIST_4(C_TO_XEN_STRING(S_file_to_array ": invalid chan: ~A, ~S has ~A chans"),
			 chan,
			 filename,
			 C_TO_XEN_INT(mus_sound_chans(name))));

  if (mus_sound_chans(name) <= 0)
    XEN_ERROR(BAD_HEADER,
	      XEN_LIST_2(C_TO_XEN_STRING(S_file_to_array ": ~S chans <= 0"),
			 filename));

  mus_file_to_float_array(name, chn, XEN_TO_C_LONG_LONG(start), samps, mus_vct_data(v));
  return(data);
}


static XEN new_sound_hook;

static void g_new_sound_hook(const char *filename)
{
  if (XEN_HOOKED(new_sound_hook))
    {
#if HAVE_SCHEME
      s7_call(s7, new_sound_hook, s7_cons(s7, C_TO_XEN_STRING(filename), XEN_EMPTY_LIST));
#else
      XEN procs, fname;
      fname = C_TO_XEN_STRING(filename);
      procs = XEN_HOOK_PROCEDURES(new_sound_hook);
      while (XEN_NOT_NULL_P(procs))
	{
	  XEN_CALL_1(XEN_CAR(procs), fname, S_new_sound_hook);
	  procs = XEN_CDR (procs);
	}
#endif
    }
}


#if HAVE_OSS
#define S_mus_audio_reinitialize "mus-audio-reinitialize"
static XEN g_mus_audio_reinitialize(void)
{
  #define H_mus_audio_reinitialize "(" S_mus_audio_reinitialize "): force audio device re-initialization"
  return(C_TO_XEN_INT(mus_audio_reinitialize()));
}
#endif


#if (!HAVE_SCHEME)
static XEN_OBJECT_TYPE sound_data_tag = 0;

bool sound_data_p(XEN obj) {return(XEN_OBJECT_TYPE_P(obj, sound_data_tag));}

#define SOUND_DATA_P(Obj) XEN_OBJECT_TYPE_P(Obj, sound_data_tag)

static XEN g_sound_data_p(XEN obj) 
{
  #define H_sound_data_p "(" S_sound_data_p " obj): is 'obj' is a sound-data object"
  return(C_TO_XEN_BOOLEAN(sound_data_p(obj)));
}


XEN_MAKE_OBJECT_FREE_PROCEDURE(sound_data, free_sound_data, sound_data_free)


static char *sound_data_to_string(sound_data *sd)
{
  char *buf;
  int chans; /* len=print length */
  mus_long_t len, sdlen;
  char flt[24];

  if (sd == NULL) return(NULL);

  len = mus_vct_print_length();
  sdlen = mus_sound_data_length(sd);
  if (len > sdlen) len = sdlen;

  chans = mus_sound_data_chans(sd);
  buf = (char *)calloc(64 + len * 24 * chans, sizeof(char));

  sprintf(buf, "#<sound-data[chans=%d, length=%lld]:", chans, sdlen);
  if (len > 0)
    {
      int i, chn;
      for (chn = 0; chn < chans; chn++)
	{
	  mus_float_t *d;
	  d = mus_sound_data_channel_data(sd, chn);
	  snprintf(flt, 24, "\n    (%.3f", d[0]);
	  strcat(buf, flt);
	  for (i = 1; i < len; i++)
	    {
	      snprintf(flt, 24, " %.3f", d[i]);
	      strcat(buf, flt);
	    }
	  if (sdlen > mus_vct_print_length())
	    strcat(buf, " ...");
	  strcat(buf, ")");
	}
    }
  strcat(buf, ">");
  return(buf);
}

#else

bool sound_data_p(XEN obj) 
{
  return((s7_is_float_vector(obj)) &&
	 (s7_vector_rank(obj) == 2));
}

#define SOUND_DATA_P(Obj) sound_data_p(Obj)

static XEN g_sound_data_p(XEN obj) 
{
  #define H_sound_data_p "(" S_sound_data_p " obj): is 'obj' is a sound-data object"
  return(C_TO_XEN_BOOLEAN(sound_data_p(obj)));
}
#endif

#if (!HAVE_SCHEME)
XEN_MAKE_OBJECT_PRINT_PROCEDURE(sound_data, print_sound_data, sound_data_to_string)


static bool sound_data_equalp(sound_data *sd1, sound_data *sd2)
{
  if (sd1 == sd2) return(true);
  if ((sd1) && (sd2) &&
      (mus_sound_data_chans(sd1) == mus_sound_data_chans(sd2)) &&
      (mus_sound_data_length(sd1) == mus_sound_data_length(sd2)))
    {
      int chn, chans;
      chans = mus_sound_data_chans(sd1);
      for (chn = 0; chn < chans; chn++)
	if (!(mus_arrays_are_equal(mus_sound_data_channel_data(sd1, chn), 
				   mus_sound_data_channel_data(sd2, chn),
				   mus_float_equal_fudge_factor(),
				   mus_sound_data_length(sd1))))
	  return(false);
      return(true);
    }
  return(false);
}
#endif


#if (!HAVE_SCHEME)
static XEN equalp_sound_data(XEN obj1, XEN obj2)
{
#if HAVE_RUBY || HAVE_FORTH
  if ((!(SOUND_DATA_P(obj1))) || (!(SOUND_DATA_P(obj2)))) return(XEN_FALSE);
#endif
  return(C_TO_XEN_BOOLEAN(sound_data_equalp(XEN_TO_SOUND_DATA(obj1), XEN_TO_SOUND_DATA(obj2))));
}
#endif

static XEN g_sound_data_length(XEN obj)
{
  #define H_sound_data_length "(" S_sound_data_length " sd): length (in samples) of each channel of sound-data sd"
  sound_data *sd;
  XEN_ASSERT_TYPE(SOUND_DATA_P(obj), obj, XEN_ONLY_ARG, S_sound_data_length, "a sound-data object");
  sd = XEN_TO_SOUND_DATA(obj);
  return(C_TO_XEN_LONG_LONG(mus_sound_data_length(sd)));
}


static XEN g_sound_data_chans(XEN obj)
{
  #define H_sound_data_chans "(" S_sound_data_chans " sd): number of channels in sound-data sd"
  sound_data *sd;
  XEN_ASSERT_TYPE(SOUND_DATA_P(obj), obj, XEN_ONLY_ARG, S_sound_data_chans, "a sound-data object");
  sd = XEN_TO_SOUND_DATA(obj);
  return(C_TO_XEN_INT(mus_sound_data_chans(sd)));
}

#if HAVE_SCHEME
static XEN make_sound_data(int chans, mus_long_t frames)
{
  #define H_make_sound_data "(" S_make_sound_data " chans frames): return a new sound-data object with 'chans' channels, each having 'frames' samples"
  s7_Int di[2];
  di[0] = chans;
  di[1] = frames;
  return(s7_make_float_vector(s7, chans * frames, 2, di));
}

XEN wrap_sound_data(int chans, mus_long_t frames, mus_float_t **data)
{
  s7_Int di[2];
  di[0] = chans;
  di[1] = frames;
  return(s7_make_float_vector_wrapper(s7, chans * frames, (s7_Double *)data, 2, di));
}

#else

static XEN make_sound_data(int chans, mus_long_t frames)
{
  #define H_make_sound_data "(" S_make_sound_data " chans frames): return a new sound-data object with 'chans' channels, each having 'frames' samples"
  sound_data *sd;
  sd = c_make_sound_data(chans, frames);
  XEN_MAKE_AND_RETURN_OBJECT(sound_data_tag, sd, 0, free_sound_data);
}


XEN wrap_sound_data(int chans, mus_long_t frames, mus_float_t **data)
{
  sound_data *sd;
  sd = (sound_data *)malloc(sizeof(sound_data));
  sd->length = frames;
  sd->chans = chans;
  sd->wrapped = true;
  sd->data = data;
  XEN_MAKE_AND_RETURN_OBJECT(sound_data_tag, sd, 0, free_sound_data);
}
#endif

static XEN g_make_sound_data(XEN chans, XEN frames)
{
  int chns;
  mus_long_t frms;

  XEN_ASSERT_TYPE(XEN_INTEGER_P(chans), chans, XEN_ARG_1, S_make_sound_data, "an integer");
  XEN_ASSERT_TYPE(XEN_LONG_LONG_P(frames), frames, XEN_ARG_2, S_make_sound_data, "an integer");

  chns = XEN_TO_C_INT(chans);
  if (chns <= 0)
    XEN_OUT_OF_RANGE_ERROR(S_make_sound_data, 1, chans, "chans <= 0?");
  if (chns > (1 << 26))
    XEN_OUT_OF_RANGE_ERROR(S_make_sound_data, 1, chans, "chans arg too large");

  frms = XEN_TO_C_LONG_LONG(frames);
  if (frms <= 0)
    XEN_OUT_OF_RANGE_ERROR(S_make_sound_data, 2, frames, "frames <= 0?");
  if ((frms > mus_max_malloc()) ||
      (((mus_long_t)(frms * sizeof(mus_float_t))) > mus_max_malloc()))
    XEN_OUT_OF_RANGE_ERROR(S_make_sound_data, 2, frames, "frames arg too large (see mus-max-malloc)");

  return(make_sound_data(chns, frms));
			 
}


static XEN g_sound_data_ref(XEN obj, XEN chan, XEN frame_num)
{
  #define H_sound_data_ref "(" S_sound_data_ref " sd chan i): sample in channel chan at location i of sound-data sd: sd[chan][i]"
  sound_data *sd;
  mus_long_t loc;
  int chn;

  XEN_ASSERT_TYPE(SOUND_DATA_P(obj), obj, XEN_ARG_1, S_sound_data_ref, "a sound-data object");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(chan), chan, XEN_ARG_2, S_sound_data_ref, "an integer");
  XEN_ASSERT_TYPE(XEN_LONG_LONG_P(frame_num), frame_num, XEN_ARG_3, S_sound_data_ref, "an integer");

  sd = XEN_TO_SOUND_DATA(obj);

  chn = XEN_TO_C_INT(chan);
  if (chn < 0)
    XEN_OUT_OF_RANGE_ERROR(S_sound_data_ref, 2, chan, "invalid channel");
  if (chn >= mus_sound_data_chans(sd))
    XEN_ERROR(XEN_ERROR_TYPE("out-of-range"),
	      XEN_LIST_3(C_TO_XEN_STRING(S_sound_data_ref ": chan: ~A >= sound-data chans, ~A"),
			 chan, 
			 C_TO_XEN_INT(mus_sound_data_chans(sd))));

  loc = XEN_TO_C_LONG_LONG(frame_num);
  if (loc < 0)
    XEN_OUT_OF_RANGE_ERROR(S_sound_data_ref, 3, frame_num, "invalid frame");
  if (loc >= mus_sound_data_length(sd))
    XEN_ERROR(XEN_ERROR_TYPE("out-of-range"),
	      XEN_LIST_3(C_TO_XEN_STRING(S_sound_data_ref ": frame: ~A >= sound-data length, ~A"),
			 frame_num, 
			 C_TO_XEN_LONG_LONG(mus_sound_data_length(sd))));

  return(C_TO_XEN_DOUBLE(mus_sound_data_ref(sd, chn, loc)));
}


XEN g_sound_data_maxamp(XEN obj)
{
  #define H_sound_data_maxamp "(" S_sound_data_maxamp " sd): list of maxamps of data in sd"
  sound_data *sd;
  int i, chans;
  mus_long_t j, len;

  XEN lst = XEN_EMPTY_LIST;
  XEN_ASSERT_TYPE(SOUND_DATA_P(obj), obj, XEN_ARG_1, S_sound_data_maxamp, "a sound-data object");

  sd = XEN_TO_SOUND_DATA(obj);
  chans = mus_sound_data_chans(sd);
  len = mus_sound_data_length(sd);

  for (i = chans - 1; i >= 0; i--)
    {
      mus_float_t mx;
      mus_float_t *buf;
      mx = -10000.0;
      buf = mus_sound_data_channel_data(sd, i);
      for (j = 0; j < len; j++)
	{
	  mus_float_t x;
	  x = fabs(buf[j]);
	  if (x > mx) mx = x;
	}
      lst = XEN_CONS(C_TO_XEN_DOUBLE(mx), lst);
    }
  return(lst);
}


static mus_float_t sound_data_peak(sound_data *sd)
{
  int chans, chn;
  mus_long_t i, len;
  mus_float_t mx = 0.0;

  chans = mus_sound_data_chans(sd);
  len = mus_sound_data_length(sd);

  for (chn = 0; chn < chans; chn++)
    {
      mus_float_t *d;
      d = mus_sound_data_channel_data(sd, chn);
      for (i = 0; i < len; i++) 
	if (fabs(d[i]) > mx)
	  mx = fabs(d[i]);
    }
  return(mx);
}


static XEN g_sound_data_peak(XEN obj)
{
  #define H_sound_data_peak "(" S_sound_data_peak " sd): overall maxamp of data in sd"
  XEN_ASSERT_TYPE(SOUND_DATA_P(obj), obj, XEN_ARG_1, S_sound_data_maxamp, "a sound-data object");
  return(C_TO_XEN_DOUBLE(sound_data_peak(XEN_TO_SOUND_DATA(obj))));
}


#if HAVE_FORTH
static XEN sound_data_apply(XEN obj, XEN chan, XEN i)
{
  return(g_sound_data_ref(obj, chan, i));
}
#endif


static XEN g_sound_data_set(XEN obj, XEN chan, XEN frame_num, XEN val)
{
  #define H_sound_data_setB "(" S_sound_data_setB " sd chan i val): set sound-data sd's i-th element in channel chan to val: sd[chan][i] = val"
  sound_data *sd;
  int chn;
  mus_long_t loc;
  mus_float_t *d;

  XEN_ASSERT_TYPE(SOUND_DATA_P(obj), obj, XEN_ARG_1, S_sound_data_setB, "a sound-data object");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(chan), chan, XEN_ARG_2, S_sound_data_setB, "an integer");
  XEN_ASSERT_TYPE(XEN_LONG_LONG_P(frame_num), frame_num, XEN_ARG_3, S_sound_data_setB, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_4, S_sound_data_setB, "a number");

  sd = XEN_TO_SOUND_DATA(obj);

  chn = XEN_TO_C_INT(chan);
  if (chn < 0)
    XEN_OUT_OF_RANGE_ERROR(S_sound_data_setB, 2, chan, "invalid channel");
  if (chn >= mus_sound_data_chans(sd))
    XEN_ERROR(XEN_ERROR_TYPE("out-of-range"),
	      XEN_LIST_3(C_TO_XEN_STRING(S_sound_data_setB ": chan: ~A >= sound-data chans, ~A"),
			 chan, 
			 C_TO_XEN_INT(mus_sound_data_chans(sd))));

  loc = XEN_TO_C_LONG_LONG(frame_num);
  if (loc < 0)
    XEN_OUT_OF_RANGE_ERROR(S_sound_data_setB, 3, frame_num, "invalid frame");
  if (loc >= mus_sound_data_length(sd))
    XEN_ERROR(XEN_ERROR_TYPE("out-of-range"),
	      XEN_LIST_3(C_TO_XEN_STRING(S_sound_data_setB ": frame: ~A >= sound-data length, ~A"),
			 frame_num, 
			 C_TO_XEN_LONG_LONG(mus_sound_data_length(sd))));

  d = mus_sound_data_channel_data(sd, chn);
  d[loc] = XEN_TO_C_DOUBLE(val);
  return(val);
}


static void sound_data_scale(sound_data *sd, mus_float_t scaler)
{
  int chans, chn;
  mus_long_t i, len;

  chans = mus_sound_data_chans(sd);
  len = mus_sound_data_length(sd);

  if (scaler == 0.0)
    {
      for (chn = 0; chn < chans; chn++)
	mus_clear_array(mus_sound_data_channel_data(sd, chn), len);
    }
  else
    {
      if (scaler != 1.0)
	for (chn = 0; chn < chans; chn++)
	  {
	    mus_float_t *d;
	    d = mus_sound_data_channel_data(sd, chn);
	    for (i = 0; i < len; i++) 
	      d[i] *= scaler;
	  }
    }
}


static XEN g_sound_data_scaleB(XEN sdobj, XEN scl)
{
  #define H_sound_data_scaleB "(" S_sound_data_scaleB " sd scl): scales (multiplies) sound-data sd's data by scl"

  XEN_ASSERT_TYPE(SOUND_DATA_P(sdobj), sdobj, XEN_ARG_1, S_sound_data_scaleB, "a sound-data object");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(scl), scl, XEN_ARG_2, S_sound_data_scaleB, "a number");

  sound_data_scale(XEN_TO_SOUND_DATA(sdobj), XEN_TO_C_DOUBLE(scl));
  return(sdobj);
}


#if (!HAVE_SCHEME)
static sound_data *sound_data_fill(sound_data *sd, mus_float_t scaler)
{
  int chans, chn;
  mus_long_t i, len;

  chans = mus_sound_data_chans(sd);
  len = mus_sound_data_length(sd);

  if (scaler == 0.0)
    {
      for (chn = 0; chn < chans; chn++)
	mus_clear_array(mus_sound_data_channel_data(sd, chn), len);
    }
  else
    {
      for (chn = 0; chn < chans; chn++)
	{
	  mus_float_t *d;
	  d = mus_sound_data_channel_data(sd, chn);
	  for (i = 0; i < len; i++) 
	    d[i] = scaler;
	}
    }
  return(sd);
}


static XEN g_sound_data_fillB(XEN sdobj, XEN scl)
{
  #define H_sound_data_fillB "(" S_sound_data_fillB " sd value): fills the sound-data object sd with value"

  XEN_ASSERT_TYPE(SOUND_DATA_P(sdobj), sdobj, XEN_ARG_1, S_sound_data_fillB, "a sound-data object");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(scl), scl, XEN_ARG_2, S_sound_data_fillB, "a number");

  sound_data_fill(XEN_TO_SOUND_DATA(sdobj), XEN_TO_C_DOUBLE(scl));
  return(sdobj);
}

static XEN g_sound_data_copy(XEN obj)
{
  sound_data *sdnew;
  #define H_sound_data_copy "(" S_sound_data_copy " sd): returns a copy of the sound-data object sd"

  XEN_ASSERT_TYPE(SOUND_DATA_P(obj), obj, XEN_ONLY_ARG, S_sound_data_copy, "a sound-data object");

  sdnew = sound_data_copy(XEN_TO_SOUND_DATA(obj));
  XEN_MAKE_AND_RETURN_OBJECT(sound_data_tag, sdnew, 0, free_sound_data);
}
#endif

static void sound_data_reverse(sound_data *sd)
{
  int chn, chans;
  mus_long_t i, j, len;

  chans = mus_sound_data_chans(sd);
  len = mus_sound_data_length(sd);

  for (chn = 0; chn < chans; chn++)
    {
      mus_float_t *d;
      d = mus_sound_data_channel_data(sd, chn);
      for (i = 0, j = len - 1; i < j; i++, j--)
	{
	  mus_float_t tmp;
	  tmp = d[i];
	  d[i] = d[j];
	  d[j] = tmp;
	}
    }
}


static XEN g_sound_data_reverseB(XEN obj)
{
  #define H_sound_data_reverseB "(" S_sound_data_reverseB " sd): reverses the elements (within each channel) of sound-data object sd"

  XEN_ASSERT_TYPE(SOUND_DATA_P(obj), obj, XEN_ONLY_ARG, S_sound_data_reverseB, "a sound-data object");

  sound_data_reverse(XEN_TO_SOUND_DATA(obj));
  return(obj);
}


static void sound_data_add(sound_data *sd1, sound_data *sd2)
{
  int i, chns;
  mus_long_t j, len;

  chns = mus_sound_data_chans(sd1);
  if (chns > mus_sound_data_chans(sd2)) chns = mus_sound_data_chans(sd2);
  len = mus_sound_data_length(sd1);
  if (len > mus_sound_data_length(sd2)) len = mus_sound_data_length(sd2);

  for (i = 0; i < chns; i++)
    {
      mus_float_t *d1, *d2;
      d1 = mus_sound_data_channel_data(sd1, i);
      d2 = mus_sound_data_channel_data(sd2, i);
      for (j = 0; j < len; j++)
	d1[j] += d2[j];
    }
}


static XEN g_sound_data_addB(XEN obj1, XEN obj2)
{
  #define H_sound_data_addB "(" S_sound_data_addB " sd1 sd2): adds (element-wise) sd2 to sd1, returning sd1"

  XEN_ASSERT_TYPE(SOUND_DATA_P(obj1), obj1, XEN_ARG_1, S_sound_data_addB, "a sound-data object");
  XEN_ASSERT_TYPE(SOUND_DATA_P(obj2), obj2, XEN_ARG_2, S_sound_data_addB, "a sound-data object");

  sound_data_add(XEN_TO_SOUND_DATA(obj1), XEN_TO_SOUND_DATA(obj2));
  return(obj1);
}


static void sound_data_multiply(sound_data *sd1, sound_data *sd2)
{
  int i, chns;
  mus_long_t j, len;

  chns = mus_sound_data_chans(sd1);
  if (chns > mus_sound_data_chans(sd2)) chns = mus_sound_data_chans(sd2);
  len = mus_sound_data_length(sd1);
  if (len > mus_sound_data_length(sd2)) len = mus_sound_data_length(sd2);

  for (i = 0; i < chns; i++)
    {
      mus_float_t *d1, *d2;
      d1 = mus_sound_data_channel_data(sd1, i);
      d2 = mus_sound_data_channel_data(sd2, i);
      for (j = 0; j < len; j++)
	d1[j] *= d2[j];
    }
}


static XEN g_sound_data_multiplyB(XEN obj1, XEN obj2)
{
  #define H_sound_data_multiplyB "(" S_sound_data_multiplyB " sd1 sd2): multiplies (element-wise) sd1 by sd2, returning sd1"

  XEN_ASSERT_TYPE(SOUND_DATA_P(obj1), obj1, XEN_ARG_1, S_sound_data_multiplyB, "a sound-data object");
  XEN_ASSERT_TYPE(SOUND_DATA_P(obj2), obj2, XEN_ARG_2, S_sound_data_multiplyB, "a sound-data object");

  sound_data_multiply(XEN_TO_SOUND_DATA(obj1), XEN_TO_SOUND_DATA(obj2));
  return(obj1);
}


static void sound_data_offset(sound_data *sd, mus_float_t off)
{
  if (off != 0.0)
    {
      int i, chans;
      mus_long_t j, len;

      len = mus_sound_data_length(sd);
      chans = mus_sound_data_chans(sd);

      for (i = 0; i < chans; i++)
	{
	  mus_float_t *d;
	  d = mus_sound_data_channel_data(sd, i);
	  for (j = 0; j < len; j++)
	    d[j] += off;
	}
    }
}


static XEN g_sound_data_offsetB(XEN obj, XEN offset)
{
  #define H_sound_data_offsetB "(" S_sound_data_offsetB " sd val): adds val to each element of sd, returning sd"

  XEN_ASSERT_TYPE(SOUND_DATA_P(obj), obj, XEN_ARG_1, S_sound_data_offsetB, "a sound-data object");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(offset), offset, XEN_ARG_2, S_sound_data_offsetB, "a number");

  sound_data_offset(XEN_TO_SOUND_DATA(obj), XEN_TO_C_DOUBLE(offset));
  return(obj);
}


static XEN g_sound_data_add(XEN obj1, XEN obj2)
{
  #define H_sound_data_add "(" S_sound_data_add " obj1 obj2): adds obj1 to obj2, either or both of which can be sound-data objects"

  XEN_ASSERT_TYPE(SOUND_DATA_P(obj1) || XEN_NUMBER_P(obj1), obj1, XEN_ARG_1, S_sound_data_add, "a sound-data object or a number");
  XEN_ASSERT_TYPE(SOUND_DATA_P(obj2) || XEN_NUMBER_P(obj2), obj2, XEN_ARG_2, S_sound_data_add, "a sound-data object or a number");

  if (SOUND_DATA_P(obj1))
    {
      if (SOUND_DATA_P(obj2))
	return(g_sound_data_addB(obj1, obj2));
      return(g_sound_data_offsetB(obj1, obj2));
    }
  if (SOUND_DATA_P(obj2))
    return(g_sound_data_offsetB(obj2, obj1));
  return(C_TO_XEN_DOUBLE(XEN_TO_C_DOUBLE(obj1) + XEN_TO_C_DOUBLE(obj2)));
}


static XEN g_sound_data_multiply(XEN obj1, XEN obj2)
{
  #define H_sound_data_multiply "(" S_sound_data_multiply " obj1 obj2): multiplies obj1 by obj2, either or both of which can be sound-data objects"

  XEN_ASSERT_TYPE(SOUND_DATA_P(obj1) || XEN_NUMBER_P(obj1), obj1, XEN_ARG_1, S_sound_data_multiply, "a sound-data object or a number");
  XEN_ASSERT_TYPE(SOUND_DATA_P(obj2) || XEN_NUMBER_P(obj2), obj2, XEN_ARG_2, S_sound_data_multiply, "a sound-data object or a number");

  if (SOUND_DATA_P(obj1))
    {
      if (SOUND_DATA_P(obj2))
	return(g_sound_data_multiplyB(obj1, obj2));
      return(g_sound_data_scaleB(obj1, obj2));
    }
  if (SOUND_DATA_P(obj2))
    return(g_sound_data_scaleB(obj2, obj1));
  return(C_TO_XEN_DOUBLE(XEN_TO_C_DOUBLE(obj1) * XEN_TO_C_DOUBLE(obj2)));
}



static XEN g_sound_data_to_vct(XEN sdobj, XEN chan, XEN vobj)
{
  #define H_sound_data_to_vct "(" S_sound_data_to_vct " sd chan v): copies sound-data sd's channel chan data into vct v"
  vct *v;
  sound_data *sd;
  int chn;
  mus_long_t len, sdlen;

  XEN_ASSERT_TYPE(SOUND_DATA_P(sdobj), sdobj, XEN_ARG_1, S_sound_data_to_vct, "a sound-data object");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(chan), chan, XEN_ARG_2, S_sound_data_to_vct, "an integer");
  XEN_ASSERT_TYPE(XEN_NOT_BOUND_P(vobj) || MUS_VCT_P(vobj), vobj, XEN_ARG_3, S_sound_data_to_vct, "a vct");

  sd = XEN_TO_SOUND_DATA(sdobj);
  sdlen = mus_sound_data_length(sd);

  chn = XEN_TO_C_INT_OR_ELSE(chan, 0);
  if (chn < 0)
    XEN_OUT_OF_RANGE_ERROR(S_sound_data_to_vct, 2, chan, "invalid channel");
  if (chn >= mus_sound_data_chans(sd))
    XEN_ERROR(XEN_ERROR_TYPE("out-of-range"),
	      XEN_LIST_3(C_TO_XEN_STRING(S_sound_data_to_vct ": chan: ~A >= sound-data chans, ~A"),
			 chan, 
			 C_TO_XEN_INT(mus_sound_data_chans(sd))));

  if (!(MUS_VCT_P(vobj))) 
    vobj = xen_make_vct(sdlen, (mus_float_t *)calloc(sdlen, sizeof(mus_float_t)));
  v = XEN_TO_VCT(vobj);

  if (sdlen < mus_vct_length(v)) 
    len = sdlen; 
  else len = mus_vct_length(v);

  memcpy((void *)(mus_vct_data(v)), (void *)(mus_sound_data_channel_data(sd, chn)), len * sizeof(mus_float_t));

  return(vobj);
}


static XEN g_vct_to_sound_data(XEN vobj, XEN sdobj, XEN chan)
{
  #define H_vct_to_sound_data "(" S_vct_to_sound_data " v sd chan): copies vct v's data into sound-data sd's channel chan"
  vct *v;
  sound_data *sd;
  XEN obj = XEN_FALSE;
  int chn;
  mus_long_t len;

  XEN_ASSERT_TYPE(MUS_VCT_P(vobj), vobj, XEN_ARG_1, S_vct_to_sound_data, "a vct");
  XEN_ASSERT_TYPE(XEN_NOT_BOUND_P(sdobj) || SOUND_DATA_P(sdobj), sdobj, XEN_ARG_2, S_vct_to_sound_data, "a sound-data object");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(chan), chan, XEN_ARG_3, S_vct_to_sound_data, "an integer");

  v = XEN_TO_VCT(vobj);
  chn = XEN_TO_C_INT_OR_ELSE(chan, 0);
  if (chn < 0)
    XEN_OUT_OF_RANGE_ERROR(S_vct_to_sound_data, 3, chan, "invalid channel");
  if (!(SOUND_DATA_P(sdobj)))
    {
      if (chn > 0)
	XEN_OUT_OF_RANGE_ERROR(S_vct_to_sound_data, 3, chan, "invalid channel");
      obj = make_sound_data(1, mus_vct_length(v));
      sd = XEN_TO_SOUND_DATA(obj);
    }
  else
    {
      sd = XEN_TO_SOUND_DATA(sdobj);
      if (chn >= mus_sound_data_chans(sd))
	XEN_ERROR(XEN_ERROR_TYPE("out-of-range"),
		  XEN_LIST_3(C_TO_XEN_STRING(S_vct_to_sound_data ": chan: ~A >= sound-data chans, ~A"),
			     chan, 
			     C_TO_XEN_INT(mus_sound_data_chans(sd))));
      obj = sdobj;
    }
  if (mus_sound_data_length(sd) < mus_vct_length(v)) 
    len = mus_sound_data_length(sd); 
  else len = mus_vct_length(v);
  memcpy((void *)(mus_sound_data_channel_data(sd, chn)), (void *)(mus_vct_data(v)), len * sizeof(mus_float_t));
  return(obj);
}


static void *mus_long_t_memmove(char *dest, const char *source, mus_long_t length)
{
  /* not void, as in gnulib, because that makes C++ unhappy! */
  char *d0 = dest;
  if (source < dest)
    /* Moving from low mem to hi mem; start at end.  */
    for (source += length, dest += length; length; --length)
      *--dest = *--source;
  else 
    if (source != dest)
      {
	/* Moving from hi mem to low mem; start at beginning.  */
	for (; length; --length)
	  *dest++ = *source++;
      }
  return((void *)d0);
}


#define S_sound_data_to_sound_data "sound-data->sound-data"

static XEN g_sound_data_to_sound_data(XEN sd_in, XEN sd_out, XEN start, XEN frames, XEN cycle_length)
{
  #define H_sound_data_to_sound_data "(" S_sound_data_to_sound_data " sd-in sd-out start frames cycle-length): \
copies sound-data sd-in's data from 0 for 'frames' frames into 'sd-out' starting at 'start', wrapping around if sd-out's end (or cycle-length) is reached."

  sound_data *sdi, *sdo;
  int i, chans;
  mus_long_t j = 0, len, beg, olen, ilen, cycle;

  XEN_ASSERT_TYPE(SOUND_DATA_P(sd_in), sd_in, XEN_ARG_1, S_sound_data_to_sound_data, "a sound-data object");
  XEN_ASSERT_TYPE(SOUND_DATA_P(sd_out), sd_out, XEN_ARG_2, S_sound_data_to_sound_data, "a sound-data object");
  XEN_ASSERT_TYPE(XEN_LONG_LONG_P(start), start, XEN_ARG_3, S_sound_data_to_sound_data, "an integer");
  XEN_ASSERT_TYPE(XEN_LONG_LONG_P(frames), frames, XEN_ARG_4, S_sound_data_to_sound_data, "an integer");
  XEN_ASSERT_TYPE(XEN_LONG_LONG_P(cycle_length), cycle_length, XEN_ARG_5, S_sound_data_to_sound_data, "an integer");

  sdi = XEN_TO_SOUND_DATA(sd_in);
  sdo = XEN_TO_SOUND_DATA(sd_out);

  ilen = mus_sound_data_length(sdi);
  olen = mus_sound_data_length(sdo);

  beg = XEN_TO_C_LONG_LONG(start);
  if (beg < 0)
    XEN_ERROR(XEN_ERROR_TYPE("out-of-range"),
	      XEN_LIST_2(C_TO_XEN_STRING(S_sound_data_to_sound_data ": start: ~A < 0"),
			 start));
  if (beg >= olen) beg = 0;

  len = XEN_TO_C_LONG_LONG(frames);
  if ((len < 0) || (len > ilen))
    XEN_ERROR(XEN_ERROR_TYPE("out-of-range"),
	      XEN_LIST_2(C_TO_XEN_STRING(S_sound_data_to_sound_data ": frames: ~A?"),
			 frames));

  cycle = XEN_TO_C_LONG_LONG(cycle_length);
  if (beg >= cycle) beg = 0;
  if (cycle > olen) cycle = olen;

  chans = mus_sound_data_chans(sdo);
  if (chans > mus_sound_data_chans(sdi))
    chans = mus_sound_data_chans(sdi);

  if ((beg + len) < cycle)
    {
      for (i = 0; i < chans; i++)
	mus_long_t_memmove((char *)(mus_sound_data_channel_data(sdo, i) + beg), 
			   (const char *)(mus_sound_data_channel_data(sdi, i)), 
			   len * sizeof(mus_float_t));
      j = beg + len;
    }
  else
    {
      for (i = 0; i < chans; i++)
	{
	  mus_long_t k;
	  mus_float_t *od, *id;

	  od = mus_sound_data_channel_data(sdo, i);
	  id = mus_sound_data_channel_data(sdi, i);
	  j = beg;
	  for (k = 0; k < len; k++)
	    {
	      od[j++] = id[k];
	      if (j == cycle) j = 0;
	    }
	}
    }
  return(C_TO_XEN_LONG_LONG(j));
}


#if HAVE_FORTH
#define S_sound_data_to_vector          "sound-data->vector"

static XEN g_sound_data_to_vector(XEN sdata)
{
#define H_sound_data_to_vector "(" S_sound_data_to_vector " sd):  \
returns a vector of chans dimensions containing all channels of sound-data sd as vct."
  int chn, chans;
  sound_data *sd;
  FTH vec;

  XEN_ASSERT_TYPE(SOUND_DATA_P(sdata), sdata, XEN_ONLY_ARG, S_sound_data_to_vector, "a sound-data object");
  sd = XEN_TO_SOUND_DATA(sdata);
  chans = mus_sound_data_chans(sd);
  vec = XEN_MAKE_VECTOR(chans, FTH_NIL);

  for (chn = 0; chn < chans; chn++)
    XEN_VECTOR_SET(vec, chn, g_sound_data_to_vct(sdata, C_TO_XEN_INT(chn), XEN_UNDEFINED));
  return vec;
}
#endif


#if HAVE_RUBY
static XEN sound_data_each(XEN obj)
{
  int j, chans;
  mus_long_t i, len;
  sound_data *sd;

  sd = XEN_TO_SOUND_DATA(obj);
  len = mus_sound_data_length(sd);
  chans = mus_sound_data_chans(sd);

  for (j = 0; j < chans; j++)
    {
      mus_float_t *d;
      d = mus_sound_data_channel_data(sd, j);
      for (i = 0; i < len; i++)
	rb_yield(C_TO_XEN_DOUBLE(d[i]));
    }
  return(obj);
}


static XEN sound_data_compare(XEN vr1, XEN vr2)
{
  mus_long_t i, len;
  int j, sd1chans, sd2chans;
  sound_data *sd1, *sd2;
  if ((SOUND_DATA_P(vr1)) && (SOUND_DATA_P(vr2)))
    {
      sd1 = XEN_TO_SOUND_DATA(vr1);
      sd2 = XEN_TO_SOUND_DATA(vr2);
      sd1chans = mus_sound_data_chans(sd1);
      sd2chans = mus_sound_data_chans(sd2);

      if (sd1chans > sd2chans) 
	return(C_TO_XEN_INT(1));
      if (sd1chans < sd2chans)
	return(C_TO_XEN_INT(-1));

      len = mus_sound_data_length(sd1);
      if (len > mus_sound_data_length(sd2)) len = mus_sound_data_length(sd2);

      for (j = 0; j < sd1chans; j++)
	{
	  mus_float_t *d1, *d2;
	  d1 = mus_sound_data_channel_data(sd1, j);
	  d2 = mus_sound_data_channel_data(sd2, j);
	  for (i = 0; i < len; i++) 
	    if (d1[i] < d2[i])
	      return(C_TO_XEN_INT(-1));
	    else
	      if (d1[i] > d2[i])
		return(C_TO_XEN_INT(1));
	}
      len = mus_sound_data_length(sd1) - mus_sound_data_length(sd2);
      if (len == 0) return(XEN_ZERO);
      if (len > 0) return(C_TO_XEN_INT(1));
    }
  return(C_TO_XEN_INT(-1));
}


static XEN sound_data_size(XEN obj)
{
  sound_data *sd;
  sd = XEN_TO_SOUND_DATA(obj);
  return(C_TO_XEN_LONG_LONG(mus_sound_data_length(sd) * mus_sound_data_chans(sd)));
}


static XEN sound_data_chans(XEN obj)
{
  sound_data *sd;
  sd = XEN_TO_SOUND_DATA(obj);
  return(C_TO_XEN_INT(mus_sound_data_chans(sd)));
}


static XEN g_rb_sound_data_fill(XEN obj, XEN val)
{
  sound_data_fill(XEN_TO_SOUND_DATA(obj), XEN_TO_C_DOUBLE(val));
  return(val);
}


static XEN g_rb_make_sound_data(XEN self, XEN chans, XEN frames)
{
  return(g_make_sound_data(chans, frames));
}
#endif



static XEN g_mus_max_malloc(void)
{
  #define H_mus_max_malloc "(" S_mus_max_malloc "): maximum number of bytes we will try to malloc."
  return(C_TO_XEN_LONG_LONG(mus_max_malloc()));
}


static XEN g_mus_set_max_malloc(XEN val)
{
  XEN_ASSERT_TYPE(XEN_LONG_LONG_P(val), val, XEN_ONLY_ARG, S_setB S_mus_max_malloc, "an integer");
  return(C_TO_XEN_LONG_LONG(mus_set_max_malloc(XEN_TO_C_LONG_LONG(val))));
}



static XEN g_mus_max_table_size(void)
{
  #define H_mus_max_table_size "(" S_mus_max_table_size "): maximum table size."
  return(C_TO_XEN_LONG_LONG(mus_max_table_size()));
}


static XEN g_mus_set_max_table_size(XEN val)
{
  XEN_ASSERT_TYPE(XEN_LONG_LONG_P(val), val, XEN_ONLY_ARG, S_setB S_mus_max_table_size, "an integer");
  return(C_TO_XEN_LONG_LONG(mus_set_max_table_size(XEN_TO_C_LONG_LONG(val))));
}


#if __APPLE__
#define S_mus_audio_output_properties_mutable "mus-audio-output-properties-mutable"
static XEN g_mus_audio_output_properties_mutable(XEN mut)
{
  #define H_mus_audio_output_properties_mutable "(" S_mus_audio_output_properties_mutable " val): can DAC settings be changed to match the current sound"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(mut), mut, XEN_ONLY_ARG, S_mus_audio_output_properties_mutable, "a boolean");
  return(C_TO_XEN_BOOLEAN(mus_audio_output_properties_mutable(XEN_TO_C_BOOLEAN(mut))));
}
#endif


#ifdef XEN_ARGIFY_1
XEN_NARGIFY_1(g_sound_data_length_w, g_sound_data_length)
XEN_NARGIFY_1(g_sound_data_chans_w, g_sound_data_chans)
#if (!HAVE_SCHEME)
XEN_NARGIFY_1(g_sound_data_copy_w, g_sound_data_copy)
XEN_NARGIFY_2(g_sound_data_fillB_w, g_sound_data_fillB)
#endif
XEN_NARGIFY_2(g_sound_data_addB_w, g_sound_data_addB)
XEN_NARGIFY_2(g_sound_data_add_w, g_sound_data_add)
XEN_NARGIFY_2(g_sound_data_offsetB_w, g_sound_data_offsetB)
XEN_NARGIFY_2(g_sound_data_multiplyB_w, g_sound_data_multiplyB)
XEN_NARGIFY_2(g_sound_data_multiply_w, g_sound_data_multiply)
XEN_NARGIFY_3(g_sound_data_ref_w, g_sound_data_ref)
XEN_NARGIFY_4(g_sound_data_set_w, g_sound_data_set)
XEN_NARGIFY_2(g_make_sound_data_w, g_make_sound_data)
XEN_NARGIFY_1(g_sound_data_p_w, g_sound_data_p)
XEN_NARGIFY_1(g_sound_data_maxamp_w, g_sound_data_maxamp)
XEN_NARGIFY_1(g_sound_data_peak_w, g_sound_data_peak)
XEN_NARGIFY_2(g_sound_data_scaleB_w, g_sound_data_scaleB)
XEN_NARGIFY_1(g_sound_data_reverseB_w, g_sound_data_reverseB)
XEN_ARGIFY_3(g_sound_data_to_vct_w, g_sound_data_to_vct)
XEN_NARGIFY_5(g_sound_data_to_sound_data_w, g_sound_data_to_sound_data)
XEN_ARGIFY_3(g_vct_to_sound_data_w, g_vct_to_sound_data)
XEN_NARGIFY_1(g_mus_sound_samples_w, g_mus_sound_samples)
XEN_NARGIFY_2(g_mus_sound_set_samples_w, g_mus_sound_set_samples)
XEN_NARGIFY_1(g_mus_sound_frames_w, g_mus_sound_frames)
XEN_NARGIFY_1(g_mus_sound_duration_w, g_mus_sound_duration)
XEN_NARGIFY_1(g_mus_sound_datum_size_w, g_mus_sound_datum_size)
XEN_NARGIFY_1(g_mus_sound_data_location_w, g_mus_sound_data_location)
XEN_NARGIFY_2(g_mus_sound_set_data_location_w, g_mus_sound_set_data_location)
XEN_NARGIFY_1(g_mus_sound_chans_w, g_mus_sound_chans)
XEN_NARGIFY_2(g_mus_sound_set_chans_w, g_mus_sound_set_chans)
XEN_NARGIFY_1(g_mus_sound_srate_w, g_mus_sound_srate)
XEN_NARGIFY_2(g_mus_sound_set_srate_w, g_mus_sound_set_srate)
XEN_NARGIFY_1(g_mus_sound_header_type_w, g_mus_sound_header_type)
XEN_NARGIFY_2(g_mus_sound_set_header_type_w, g_mus_sound_set_header_type)
XEN_NARGIFY_1(g_mus_sound_data_format_w, g_mus_sound_data_format)
XEN_NARGIFY_2(g_mus_sound_set_data_format_w, g_mus_sound_set_data_format)
XEN_NARGIFY_1(g_mus_sound_length_w, g_mus_sound_length)
XEN_NARGIFY_1(g_mus_sound_type_specifier_w, g_mus_sound_type_specifier)
XEN_NARGIFY_1(g_mus_header_type_name_w, g_mus_header_type_name)
XEN_NARGIFY_1(g_mus_header_type_to_string_w, g_mus_header_type_to_string)
XEN_NARGIFY_1(g_mus_data_format_name_w, g_mus_data_format_name)
XEN_NARGIFY_1(g_mus_data_format_to_string_w, g_mus_data_format_to_string)
XEN_NARGIFY_1(g_mus_sound_comment_w, g_mus_sound_comment)
XEN_NARGIFY_1(g_mus_sound_write_date_w, g_mus_sound_write_date)
XEN_NARGIFY_1(g_mus_bytes_per_sample_w, g_mus_bytes_per_sample)
XEN_NARGIFY_1(g_mus_sound_loop_info_w, g_mus_sound_loop_info)
XEN_NARGIFY_1(g_mus_sound_mark_info_w, g_mus_sound_mark_info)
XEN_NARGIFY_1(g_mus_sound_maxamp_w, g_mus_sound_maxamp)
XEN_NARGIFY_2(g_mus_sound_set_maxamp_w, g_mus_sound_set_maxamp)
XEN_NARGIFY_1(g_mus_sound_maxamp_exists_w, g_mus_sound_maxamp_exists)
XEN_NARGIFY_1(g_mus_sound_open_input_w, g_mus_sound_open_input)
XEN_NARGIFY_1(g_mus_sound_close_input_w, g_mus_sound_close_input)

XEN_NARGIFY_1(g_mus_audio_close_w, g_mus_audio_close)
XEN_ARGIFY_4(g_mus_audio_write_w, g_mus_audio_write)
XEN_NARGIFY_3(g_mus_audio_read_w, g_mus_audio_read)
XEN_NARGIFY_5(g_mus_audio_open_output_w, g_mus_audio_open_output)
XEN_NARGIFY_5(g_mus_audio_open_input_w, g_mus_audio_open_input)

XEN_NARGIFY_0(g_mus_clipping_w, g_mus_clipping)
XEN_NARGIFY_1(g_mus_set_clipping_w, g_mus_set_clipping)
XEN_NARGIFY_1(g_mus_file_clipping_w, g_mus_file_clipping)
XEN_NARGIFY_2(g_mus_file_set_clipping_w, g_mus_file_set_clipping)
XEN_NARGIFY_0(g_mus_header_raw_defaults_w, g_mus_header_raw_defaults)
XEN_NARGIFY_1(g_mus_header_set_raw_defaults_w, g_mus_header_set_raw_defaults)
XEN_NARGIFY_2(g_mus_header_writable_w, g_mus_header_writable)
XEN_NARGIFY_1(g_mus_expand_filename_w, g_mus_expand_filename)
XEN_ARGIFY_6(g_mus_sound_open_output_w, g_mus_sound_open_output)
XEN_ARGIFY_5(g_mus_sound_reopen_output_w, g_mus_sound_reopen_output)
XEN_NARGIFY_2(g_mus_sound_close_output_w, g_mus_sound_close_output)
XEN_NARGIFY_5(g_mus_sound_read_w, g_mus_sound_read)
XEN_NARGIFY_5(g_mus_sound_write_w, g_mus_sound_write)
XEN_NARGIFY_2(g_mus_sound_seek_frame_w, g_mus_sound_seek_frame)
XEN_ARGIFY_1(g_mus_sound_report_cache_w, g_mus_sound_report_cache)
XEN_NARGIFY_1(g_mus_sound_forget_w, g_mus_sound_forget)
XEN_NARGIFY_0(g_mus_sound_prune_w, g_mus_sound_prune)
XEN_NARGIFY_1(g_mus_error_type_to_string_w, g_mus_error_type_to_string)
XEN_NARGIFY_2(g_mus_oss_set_buffers_w, g_mus_oss_set_buffers)
XEN_NARGIFY_5(g_array_to_file_w, g_array_to_file)
XEN_NARGIFY_5(g_file_to_array_w, g_file_to_array)
XEN_NARGIFY_0(g_mus_alsa_buffers_w, g_mus_alsa_buffers)
XEN_NARGIFY_1(g_mus_alsa_set_buffers_w, g_mus_alsa_set_buffers)
XEN_NARGIFY_0(g_mus_alsa_buffer_size_w, g_mus_alsa_buffer_size)
XEN_NARGIFY_1(g_mus_alsa_set_buffer_size_w, g_mus_alsa_set_buffer_size)
XEN_NARGIFY_0(g_mus_alsa_device_w, g_mus_alsa_device)
XEN_NARGIFY_1(g_mus_alsa_set_device_w, g_mus_alsa_set_device)
XEN_NARGIFY_0(g_mus_alsa_playback_device_w, g_mus_alsa_playback_device)
XEN_NARGIFY_1(g_mus_alsa_set_playback_device_w, g_mus_alsa_set_playback_device)
XEN_NARGIFY_0(g_mus_alsa_capture_device_w, g_mus_alsa_capture_device)
XEN_NARGIFY_1(g_mus_alsa_set_capture_device_w, g_mus_alsa_set_capture_device)
XEN_NARGIFY_0(g_mus_alsa_squelch_warning_w, g_mus_alsa_squelch_warning)
XEN_NARGIFY_1(g_mus_alsa_set_squelch_warning_w, g_mus_alsa_set_squelch_warning)

#if HAVE_OSS
  XEN_NARGIFY_0(g_mus_audio_reinitialize_w, g_mus_audio_reinitialize)
#endif

#if __APPLE__
XEN_NARGIFY_1(g_mus_audio_output_properties_mutable_w, g_mus_audio_output_properties_mutable)
#endif

XEN_NARGIFY_0(g_mus_max_malloc_w, g_mus_max_malloc)
XEN_NARGIFY_1(g_mus_set_max_malloc_w, g_mus_set_max_malloc)
XEN_NARGIFY_0(g_mus_max_table_size_w, g_mus_max_table_size)
XEN_NARGIFY_1(g_mus_set_max_table_size_w, g_mus_set_max_table_size)

#else
#define g_sound_data_length_w g_sound_data_length
#define g_sound_data_chans_w g_sound_data_chans
#if (!HAVE_SCHEME)
#define g_sound_data_copy_w g_sound_data_copy
#define g_sound_data_fillB_w g_sound_data_fillB
#endif
#define g_sound_data_addB_w g_sound_data_addB
#define g_sound_data_add_w g_sound_data_add
#define g_sound_data_offsetB_w g_sound_data_offsetB
#define g_sound_data_multiplyB_w g_sound_data_multiplyB
#define g_sound_data_multiply_w g_sound_data_multiply
#define g_sound_data_ref_w g_sound_data_ref
#define g_sound_data_set_w g_sound_data_set
#define g_make_sound_data_w g_make_sound_data
#define g_sound_data_p_w g_sound_data_p
#define g_sound_data_maxamp_w g_sound_data_maxamp
#define g_sound_data_peak_w g_sound_data_peak
#define g_sound_data_scaleB_w g_sound_data_scaleB
#define g_sound_data_reverseB_w g_sound_data_reverseB
#define g_sound_data_to_vct_w g_sound_data_to_vct
#define g_sound_data_to_sound_data_w g_sound_data_to_sound_data
#define g_vct_to_sound_data_w g_vct_to_sound_data
#define g_mus_sound_samples_w g_mus_sound_samples
#define g_mus_sound_set_samples_w g_mus_sound_set_samples
#define g_mus_sound_frames_w g_mus_sound_frames
#define g_mus_sound_duration_w g_mus_sound_duration
#define g_mus_sound_datum_size_w g_mus_sound_datum_size
#define g_mus_sound_data_location_w g_mus_sound_data_location
#define g_mus_sound_set_data_location_w g_mus_sound_set_data_location
#define g_mus_sound_chans_w g_mus_sound_chans
#define g_mus_sound_set_chans_w g_mus_sound_set_chans
#define g_mus_sound_srate_w g_mus_sound_srate
#define g_mus_sound_set_srate_w g_mus_sound_set_srate
#define g_mus_sound_header_type_w g_mus_sound_header_type
#define g_mus_sound_set_header_type_w g_mus_sound_set_header_type
#define g_mus_sound_data_format_w g_mus_sound_data_format
#define g_mus_sound_set_data_format_w g_mus_sound_set_data_format
#define g_mus_sound_length_w g_mus_sound_length
#define g_mus_sound_type_specifier_w g_mus_sound_type_specifier
#define g_mus_header_type_name_w g_mus_header_type_name
#define g_mus_header_writable_w g_mus_header_writable
#define g_mus_data_format_name_w g_mus_data_format_name
#define g_mus_header_type_to_string_w g_mus_header_type_to_string
#define g_mus_data_format_to_string_w g_mus_data_format_to_string
#define g_mus_sound_comment_w g_mus_sound_comment
#define g_mus_sound_write_date_w g_mus_sound_write_date
#define g_mus_bytes_per_sample_w g_mus_bytes_per_sample
#define g_mus_sound_loop_info_w g_mus_sound_loop_info
#define g_mus_sound_mark_info_w g_mus_sound_mark_info
#define g_mus_sound_maxamp_w g_mus_sound_maxamp
#define g_mus_sound_set_maxamp_w g_mus_sound_set_maxamp
#define g_mus_sound_maxamp_exists_w g_mus_sound_maxamp_exists
#define g_mus_sound_open_input_w g_mus_sound_open_input
#define g_mus_sound_close_input_w g_mus_sound_close_input

#define g_mus_audio_close_w g_mus_audio_close
#define g_mus_audio_write_w g_mus_audio_write
#define g_mus_audio_read_w g_mus_audio_read
#define g_mus_audio_open_output_w g_mus_audio_open_output
#define g_mus_audio_open_input_w g_mus_audio_open_input

#define g_mus_clipping_w g_mus_clipping
#define g_mus_set_clipping_w g_mus_set_clipping
#define g_mus_file_clipping_w g_mus_file_clipping
#define g_mus_file_set_clipping_w g_mus_file_set_clipping
#define g_mus_header_raw_defaults_w g_mus_header_raw_defaults
#define g_mus_header_set_raw_defaults_w g_mus_header_set_raw_defaults
#define g_mus_expand_filename_w g_mus_expand_filename
#define g_mus_sound_open_output_w g_mus_sound_open_output
#define g_mus_sound_reopen_output_w g_mus_sound_reopen_output
#define g_mus_sound_close_output_w g_mus_sound_close_output
#define g_mus_sound_read_w g_mus_sound_read
#define g_mus_sound_write_w g_mus_sound_write
#define g_mus_sound_seek_frame_w g_mus_sound_seek_frame
#define g_mus_sound_report_cache_w g_mus_sound_report_cache
#define g_mus_sound_forget_w g_mus_sound_forget
#define g_mus_sound_prune_w g_mus_sound_prune
#define g_mus_error_type_to_string_w g_mus_error_type_to_string
#define g_mus_oss_set_buffers_w g_mus_oss_set_buffers
#define g_array_to_file_w g_array_to_file
#define g_file_to_array_w g_file_to_array
#define g_mus_alsa_buffers_w g_mus_alsa_buffers
#define g_mus_alsa_set_buffers_w g_mus_alsa_set_buffers
#define g_mus_alsa_buffer_size_w g_mus_alsa_buffer_size
#define g_mus_alsa_set_buffer_size_w g_mus_alsa_set_buffer_size
#define g_mus_alsa_device_w g_mus_alsa_device
#define g_mus_alsa_set_device_w g_mus_alsa_set_device
#define g_mus_alsa_playback_device_w g_mus_alsa_playback_device
#define g_mus_alsa_set_playback_device_w g_mus_alsa_set_playback_device
#define g_mus_alsa_capture_device_w g_mus_alsa_capture_device
#define g_mus_alsa_set_capture_device_w g_mus_alsa_set_capture_device
#define g_mus_alsa_squelch_warning_w g_mus_alsa_squelch_warning
#define g_mus_alsa_set_squelch_warning_w g_mus_alsa_set_squelch_warning
#if HAVE_OSS
  #define g_mus_audio_reinitialize_w g_mus_audio_reinitialize
#endif
#if __APPLE__
  #define g_mus_audio_output_properties_mutable_w g_mus_audio_output_properties_mutable
#endif

#define g_mus_max_malloc_w g_mus_max_malloc
#define g_mus_set_max_malloc_w g_mus_set_max_malloc
#define g_mus_max_table_size_w g_mus_max_table_size
#define g_mus_set_max_table_size_w g_mus_set_max_table_size

#endif

#if HAVE_SCHEME
#ifndef _MSC_VER
static size_t c_object_value_location, c_object_type_location, cell_type_location;
static int c_object_built_in_type;

#if (SIZEOF_VOID_P == 4)
  #define C_OBJECT_VALUE_LOCATION 12
#else
  #define C_OBJECT_VALUE_LOCATION 16
#endif
#define C_OBJECT_TYPE_LOCATION 8
#define CELL_TYPE_LOCATION 0
#define C_OBJECT_BUILT_IN_TYPE 17
#endif

static int sound_data_number_location;

#ifndef _MSC_VER
#define SOUND_DATA_NUMBER_LOCATION 8
#define SOUND_DATA_DENOMINATOR_LOCATION 16
#else
static int SOUND_DATA_NUMBER_LOCATION, SOUND_DATA_DENOMINATOR_LOCATION;
#endif


#if (!WITH_GMP)
static s7_pointer sound_data_set_direct, sound_data_set_four;
static s7_pointer g_sound_data_set_direct(s7_scheme *sc, s7_pointer args)
{
  sound_data *sd;
  sd = (sound_data *)s7_car_value(sc, args);
  if (sd)
    {
      mus_long_t loc, chan;
      s7_pointer val;
      mus_float_t *d;

      chan = s7_number_to_integer(sc, s7_cadr_value(sc, args));
      if ((chan < 0) || (chan >= mus_sound_data_chans(sd)))
	XEN_OUT_OF_RANGE_ERROR(S_sound_data_setB, 2, s7_cadr(args), "channel number out of range");

      loc = s7_number_to_integer(sc, s7_cadr_value(sc, s7_cdr(args)));
      if ((loc < 0) || (loc>= mus_sound_data_length(sd)))
	XEN_OUT_OF_RANGE_ERROR(S_sound_data_setB, 2, s7_caddr(args), "index out of range");

      val = s7_call_direct(sc, s7_cadddr(args));
      d = mus_sound_data_channel_data(sd, chan);
      d[loc] = s7_real(val);
      return(val);
    }
  XEN_ASSERT_TYPE(false, s7_car_value(sc, args), XEN_ARG_1, "sound-data-set!", "a sound-data object");
  return(s7_f(sc));
}

static s7_pointer sound_data_set_direct_looped;
static s7_pointer g_sound_data_set_direct_looped(s7_scheme *sc, s7_pointer args)
{
  s7_Int pos, end, chan;
  s7_pointer stepper, vc, val, chn, samp;
  s7_Int *step, *stop;
  sound_data *sd;
  gf *gf1;

  /* fprintf(stderr, "args: %s\n", DISPLAY(args)); */
  
  vc = s7_cadr_value(sc, args);                      /* (0 sd k i (...)) or possibly (0 sdata i 1 0.1) etc */
  sd = (sound_data *)vc;
  if (sd)
    {
      s7_pointer chn_slot = NULL, samp_slot = NULL;

      stepper = s7_car(args);
      /* cadr is the sound-data object */
      chn = s7_caddr(args);
      samp = s7_cadddr(args);
      val = s7_cadddr(s7_cdr(args));

      /* here we might be stepping through the channel which makes things messy 
       */

      step = ((s7_Int *)((unsigned char *)(stepper) + SOUND_DATA_NUMBER_LOCATION));
      stop = ((s7_Int *)((unsigned char *)(stepper) + SOUND_DATA_DENOMINATOR_LOCATION));
      pos = (*step);
      end = (*stop);

      if (s7_is_symbol(samp))
	{
	  samp_slot = s7_slot(sc, samp);
	  if (!samp_slot) return(NULL);
	}
      if (s7_is_symbol(chn))
	{
	  chn_slot = s7_slot(sc, chn);
	  if (!chn_slot) return(NULL);
	}
      
      if ((samp_slot) && 
	  (s7_slot_value(samp_slot) == stepper) &&
	  ((!chn_slot) || (s7_slot_value(chn_slot) != stepper)))
	{
	  mus_float_t *d;
	  if ((pos < 0) ||
	      (end > mus_sound_data_length(sd)))
	    XEN_OUT_OF_RANGE_ERROR("sound-data-set!", 3, s7_cadddr(args), "index out of range");   

	  if (chn_slot)
	    chan = s7_number_to_integer(sc, s7_slot_value(chn_slot));
	  else chan = s7_number_to_integer(sc, chn);
	  if ((chan < 0) ||
	      (chan >= mus_sound_data_chans(sd)))
	    XEN_OUT_OF_RANGE_ERROR("sound-data-set!", 2, chn, "channel number out of range");   

	  d = mus_sound_data_channel_data(sd, chan);

	  if (s7_is_real(val))
	    {
	      double x;
	      x = s7_number_to_real(sc, val);
	      for (; pos < end; pos++) 
		d[pos] = x;
	      (*step) = end;
	      return(args);
	    }
	  
	  /* ---------------------------------------- */
	  gf1 = find_gf(sc, val);
	  if (gf1)
	    {
	      if (gf1->func_1)
		{
		  void *gen;
		  mus_float_t (*func)(void *p);
		  gen = gf1->gen;
		  func = gf1->func_1;
		  for (; pos < end; pos++) 
		    d[pos] = func(gen);
		  (*step) = end;
		  gf_free(gf1);
		  return(args);
		}
	      if (gf1->func)
		{
		  for (; pos < end; pos++)
		    {
		      (*step) = pos;
		      d[pos] = gf1->func(gf1);
		    }
		  (*step) = end;
		  gf_free(gf1);
		  return(args);
		}
	      gf_free(gf1);
	    }
	  /* ---------------------------------------- */
	  
	  if (s7_function_choice_is_direct_to_real(sc, val))
	    {
	      for (; pos < end; pos++)
		{
		  (*step) = pos;
		  d[pos] = s7_call_direct_to_real_and_free(sc, val);
		}
	      (*step) = end;
	      return(args);
	    }
	}
      else
	{
	  s7_Int smp;
	  mus_float_t *d;
	  if ((chn_slot) && 
	      (s7_slot_value(chn_slot) == stepper) &&
	      ((!samp_slot) || (s7_slot_value(samp_slot) != stepper)))
	    {
	      if ((pos < 0) ||
		  (end > mus_sound_data_chans(sd)))
		XEN_OUT_OF_RANGE_ERROR("sound-data-set!", 2, s7_cadddr(args), "channel number out of range");   

	      if (samp_slot)
		smp = s7_number_to_integer(sc, s7_slot_value(samp_slot));
	      else smp = s7_number_to_integer(sc, samp);

	      if ((smp < 0) ||
		  (smp >= mus_sound_data_length(sd)))
		XEN_OUT_OF_RANGE_ERROR("sound-data-set!", 3, chn, "index out of range");   

	      if (s7_is_real(val))
		{
		  double x;
		  x = s7_number_to_real(sc, val);
		  for (; pos < end; pos++)
		    {
		      d = mus_sound_data_channel_data(sd, pos);
		      d[smp] = x;
		    }
		  (*step) = end;
		  return(args);
		}

	      /* ---------------------------------------- */
	      gf1 = find_gf(sc, val);
	      if (gf1)
		{
		  for (; pos < end; pos++)
		    {
		      (*step) = pos;
		      d = mus_sound_data_channel_data(sd, pos);
		      d[smp] = gf1->func(gf1);
		    }
		  (*step) = end;
		  gf_free(gf1);
		  return(args);
		}
	      gf_free(gf1);
	    }
	  /* ---------------------------------------- */
	}
      return(NULL);
    }
  XEN_ASSERT_TYPE(false, s7_cadr_value(sc, args), XEN_ARG_1, "sound-data-set!", "a sound-data object");
  return(s7_f(sc));
}
#endif

static s7_pointer sound_data_ref_three;
static s7_pointer g_sound_data_ref_three(s7_scheme *sc, s7_pointer args)
{
  sound_data *sd;
  sd = (sound_data *)s7_car_value(sc, args);
  if (sd)
    {
      mus_long_t chn, loc;
      chn = s7_number_to_integer(sc, s7_cadr_value(sc, args));
      if ((chn < 0) || 
	  (chn >= mus_sound_data_chans(sd)))
	XEN_OUT_OF_RANGE_ERROR("sound-data-ref", 2, s7_cadr_value(sc, args), "channel number out of range");   
      loc = s7_number_to_integer(sc, s7_cadr_value(sc, s7_cdr(args)));
      if ((loc < 0) || 
	  (loc > mus_sound_data_length(sd)))
	XEN_OUT_OF_RANGE_ERROR(S_sound_data_ref, 2, s7_cadr_value(sc, s7_cdr(args)), "index out of range");
      return(s7_make_real(sc, mus_sound_data_channel_data(sd, chn)[loc]));
    }
  XEN_ASSERT_TYPE(false, s7_car_value(sc, args), XEN_ARG_1, "sound-data-ref", "a sound-data object");
  return(s7_f(sc));
}

static s7_pointer sound_data_ref_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if ((args == 3) &&
      (s7_is_symbol(s7_cadr(expr))) &&
      (s7_is_symbol(s7_caddr(expr))) &&
      (s7_is_symbol(s7_cadddr(expr))))
    {
      s7_function_choice_set_direct(sc, expr);
      return(sound_data_ref_three);
    }
  return(f);
}

static s7_pointer sound_data_set_chooser(s7_scheme *sc, s7_pointer f, int args, s7_pointer expr)
{
  if (args == 4)
    {
#if (!WITH_GMP)
      s7_pointer arg1, arg2, arg3, arg4;
      arg1 = s7_cadr(expr);
      arg2 = s7_caddr(expr);
      arg3 = s7_cadddr(expr);
      arg4 = s7_cadddr(s7_cdr(expr));

      if ((s7_is_symbol(arg1)) &&
	  (s7_is_symbol(arg2)) &&
	  (s7_is_symbol(arg3)) &&
	  (s7_is_pair(arg4)) &&
	  (s7_function_choice_is_direct(sc, arg4)) &&
	  (s7_function_returns_temp(sc, arg4)))
	{
	  s7_function_choice_set_direct(sc, expr);
	  return(sound_data_set_direct);
	}
      return(sound_data_set_four);
#endif
    }
  return(f);
}
#endif


void mus_sndlib_xen_initialize(void)
{
  mus_sound_initialize();

#if (!HAVE_SCHEME)
  sound_data_tag = XEN_MAKE_OBJECT_TYPE("SoundData", sizeof(sound_data));
#endif

#if HAVE_FORTH
  fth_set_object_inspect(sound_data_tag, print_sound_data);
  fth_set_object_equal(sound_data_tag, equalp_sound_data);
  fth_set_object_to_array(sound_data_tag, g_sound_data_to_vector);
  fth_set_object_length(sound_data_tag, g_sound_data_length);
  fth_set_object_free(sound_data_tag, free_sound_data);
  fth_set_object_apply(sound_data_tag, XEN_PROCEDURE_CAST sound_data_apply, 2, 0, 0);
#endif

#if HAVE_RUBY
  Init_Hook();
  rb_include_module(sound_data_tag, rb_mComparable);
  rb_include_module(sound_data_tag, rb_mEnumerable);
  rb_define_method(sound_data_tag, "to_s",   XEN_PROCEDURE_CAST print_sound_data,     0);
  rb_define_method(sound_data_tag, "eql?",   XEN_PROCEDURE_CAST equalp_sound_data,    1);
  rb_define_method(sound_data_tag, "==",     XEN_PROCEDURE_CAST equalp_sound_data,    1);
  rb_define_method(sound_data_tag, "each",   XEN_PROCEDURE_CAST sound_data_each,      0);
  rb_define_method(sound_data_tag, "<=>",    XEN_PROCEDURE_CAST sound_data_compare,   1);
  rb_define_method(sound_data_tag, "[]",     XEN_PROCEDURE_CAST g_sound_data_ref,     2);
  rb_define_method(sound_data_tag, "[]=",    XEN_PROCEDURE_CAST g_sound_data_set,     3);
  rb_define_method(sound_data_tag, "length", XEN_PROCEDURE_CAST sound_data_size,      0);
  rb_define_method(sound_data_tag, "size",   XEN_PROCEDURE_CAST sound_data_size,      0);
  rb_define_method(sound_data_tag, "fill",   XEN_PROCEDURE_CAST g_rb_sound_data_fill, 1);
  rb_define_method(sound_data_tag, "dup",    XEN_PROCEDURE_CAST g_sound_data_copy,    0);
  rb_define_method(sound_data_tag, "chans",  XEN_PROCEDURE_CAST sound_data_chans,     0);
  rb_define_method(sound_data_tag, "peak",   XEN_PROCEDURE_CAST g_sound_data_peak,    0);
  rb_define_method(sound_data_tag, "offset!",   XEN_PROCEDURE_CAST g_sound_data_offsetB,   1);
  rb_define_method(sound_data_tag, "multiply!", XEN_PROCEDURE_CAST g_sound_data_multiplyB, 1);

  rb_define_method(sound_data_tag, "add!",      XEN_PROCEDURE_CAST g_sound_data_addB,      1);
  rb_define_method(sound_data_tag, "scale!",    XEN_PROCEDURE_CAST g_sound_data_scaleB,    1);
  rb_define_method(sound_data_tag, "reverse!",  XEN_PROCEDURE_CAST g_sound_data_reverseB,  0);

  rb_define_singleton_method(sound_data_tag, "new", XEN_PROCEDURE_CAST g_rb_make_sound_data, 2);
#endif

  XEN_DEFINE_CONSTANT(S_mus_out_format,           MUS_OUT_FORMAT,           "sample format for fastest IO");
  XEN_DEFINE_CONSTANT(S_mus_unsupported,          MUS_UNSUPPORTED,          "unsupported header id");
  XEN_DEFINE_CONSTANT(S_mus_next,                 MUS_NEXT,                 "NeXT (Sun) sound header id");
  XEN_DEFINE_CONSTANT(S_mus_aifc,                 MUS_AIFC,                 "AIFC sound header id");
  XEN_DEFINE_CONSTANT(S_mus_rf64,                 MUS_RF64,                 "RF64 sound header id");
  XEN_DEFINE_CONSTANT(S_mus_riff,                 MUS_RIFF,                 "RIFF (MS wave) sound header id");
  XEN_DEFINE_CONSTANT(S_mus_nist,                 MUS_NIST,                 "NIST (Sphere) sound header id");
  XEN_DEFINE_CONSTANT(S_mus_raw,                  MUS_RAW,                  "raw (headerless) sound header id");
  XEN_DEFINE_CONSTANT(S_mus_ircam,                MUS_IRCAM,                "IRCAM sound header id");
  XEN_DEFINE_CONSTANT(S_mus_aiff,                 MUS_AIFF,                 "AIFF (old-style) sound header id");
  XEN_DEFINE_CONSTANT(S_mus_bicsf,                MUS_BICSF,                "BICSF header id");
  XEN_DEFINE_CONSTANT(S_mus_voc,                  MUS_VOC,                  "VOC header id");
  XEN_DEFINE_CONSTANT(S_mus_svx,                  MUS_SVX,                  "SVX (IFF) header id");
  XEN_DEFINE_CONSTANT(S_mus_soundfont,            MUS_SOUNDFONT,            "soundfont header id");
  XEN_DEFINE_CONSTANT(S_mus_caff,                 MUS_CAFF,                 "Apple Core Audio File Format header id");

  XEN_DEFINE_CONSTANT(S_mus_unknown,              MUS_UNKNOWN,              "unknown data format");
  XEN_DEFINE_CONSTANT(S_mus_bshort,               MUS_BSHORT,               "big-endian short data format id");
  XEN_DEFINE_CONSTANT(S_mus_lshort,               MUS_LSHORT,               "little-endian short data format id");
  XEN_DEFINE_CONSTANT(S_mus_mulaw,                MUS_MULAW,                "mulaw (8-bit) data format id");
  XEN_DEFINE_CONSTANT(S_mus_alaw,                 MUS_ALAW,                 "alaw (8-bit) data format id");
  XEN_DEFINE_CONSTANT(S_mus_byte,                 MUS_BYTE,                 "signed byte data format id");
  XEN_DEFINE_CONSTANT(S_mus_ubyte,                MUS_UBYTE,                "unsigned byte data format id");
  XEN_DEFINE_CONSTANT(S_mus_bfloat,               MUS_BFLOAT,               "big-endian float data format id");
  XEN_DEFINE_CONSTANT(S_mus_lfloat,               MUS_LFLOAT,               "little-endian float data format id");
  XEN_DEFINE_CONSTANT(S_mus_bint,                 MUS_BINT,                 "big-endian int data format id");
  XEN_DEFINE_CONSTANT(S_mus_lint,                 MUS_LINT,                 "little-endian int data format id");
  XEN_DEFINE_CONSTANT(S_mus_bintn,                MUS_BINTN,                "normalized big-endian int data format id");
  XEN_DEFINE_CONSTANT(S_mus_lintn,                MUS_LINTN,                "normalized little-endian int data format id");
  XEN_DEFINE_CONSTANT(S_mus_b24int,               MUS_B24INT,               "big-endian 24-bit data format id");
  XEN_DEFINE_CONSTANT(S_mus_l24int,               MUS_L24INT,               "little-endian 24-bit data format id");
  XEN_DEFINE_CONSTANT(S_mus_bdouble,              MUS_BDOUBLE,              "big-endian double data format id");
  XEN_DEFINE_CONSTANT(S_mus_ldouble,              MUS_LDOUBLE,              "little-endian double data format id");
  XEN_DEFINE_CONSTANT(S_mus_ubshort,              MUS_UBSHORT,              "unsigned big-endian short data format id");
  XEN_DEFINE_CONSTANT(S_mus_ulshort,              MUS_ULSHORT,              "unsigned little-endian short data format id");
  XEN_DEFINE_CONSTANT(S_mus_bdouble_unscaled,     MUS_BDOUBLE_UNSCALED,     "unscaled big-endian double data format id");
  XEN_DEFINE_CONSTANT(S_mus_ldouble_unscaled,     MUS_LDOUBLE_UNSCALED,     "unscaled little-endian double data format id");
  XEN_DEFINE_CONSTANT(S_mus_bfloat_unscaled,      MUS_BFLOAT_UNSCALED,      "unscaled big-endian float data format id");
  XEN_DEFINE_CONSTANT(S_mus_lfloat_unscaled,      MUS_LFLOAT_UNSCALED,      "unscaled little-endian float data format id");

  XEN_DEFINE_CONSTANT(S_mus_audio_default,        MUS_AUDIO_DEFAULT,        "default audio device");

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_sound_samples, g_mus_sound_samples_w, H_mus_sound_samples, 
				   S_setB S_mus_sound_samples, g_mus_sound_set_samples_w, 1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_sound_data_location, g_mus_sound_data_location_w, H_mus_sound_data_location,
				   S_setB S_mus_sound_data_location, g_mus_sound_set_data_location_w, 1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_sound_chans, g_mus_sound_chans_w, H_mus_sound_chans,
				   S_setB S_mus_sound_chans, g_mus_sound_set_chans_w, 1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_sound_srate, g_mus_sound_srate_w, H_mus_sound_srate,
				   S_setB S_mus_sound_srate, g_mus_sound_set_srate_w, 1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_sound_header_type, g_mus_sound_header_type_w, H_mus_sound_header_type,
				   S_setB S_mus_sound_header_type, g_mus_sound_set_header_type_w, 1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_sound_data_format, g_mus_sound_data_format_w, H_mus_sound_data_format,
				   S_setB S_mus_sound_data_format, g_mus_sound_set_data_format_w, 1, 0, 2, 0);

  XEN_DEFINE_SAFE_PROCEDURE(S_sound_data_length,        g_sound_data_length_w,          1, 0, 0, H_sound_data_length);
  XEN_DEFINE_SAFE_PROCEDURE(S_sound_data_chans,         g_sound_data_chans_w,           1, 0, 0, H_sound_data_chans);
#if HAVE_SCHEME
  s7_eval_c_string(s7, "(define sound-data-copy copy)");
  s7_eval_c_string(s7, "(define sound-data-fill! fill!)");
#else
  XEN_DEFINE_SAFE_PROCEDURE(S_sound_data_copy,          g_sound_data_copy_w,            1, 0, 0, H_sound_data_copy);
  XEN_DEFINE_SAFE_PROCEDURE(S_sound_data_fillB,         g_sound_data_fillB_w,           2, 0, 0, H_sound_data_fillB);
#endif
  XEN_DEFINE_SAFE_PROCEDURE(S_sound_data_addB,          g_sound_data_addB_w,            2, 0, 0, H_sound_data_addB);
  XEN_DEFINE_SAFE_PROCEDURE(S_sound_data_add,           g_sound_data_add_w,             2, 0, 0, H_sound_data_add);
  XEN_DEFINE_SAFE_PROCEDURE(S_sound_data_offsetB,       g_sound_data_offsetB_w,         2, 0, 0, H_sound_data_offsetB);
  XEN_DEFINE_SAFE_PROCEDURE(S_sound_data_multiplyB,     g_sound_data_multiplyB_w,       2, 0, 0, H_sound_data_multiplyB);
  XEN_DEFINE_SAFE_PROCEDURE(S_sound_data_multiply,      g_sound_data_multiply_w,        2, 0, 0, H_sound_data_multiply);
  XEN_DEFINE_SAFE_PROCEDURE(S_make_sound_data,          g_make_sound_data_w,            2, 0, 0, H_make_sound_data);
  XEN_DEFINE_SAFE_PROCEDURE(S_sound_data_p,             g_sound_data_p_w,               1, 0, 0, H_sound_data_p);
  XEN_DEFINE_SAFE_PROCEDURE(S_sound_data_maxamp,        g_sound_data_maxamp_w,          1, 0, 0, H_sound_data_maxamp);
  XEN_DEFINE_SAFE_PROCEDURE(S_sound_data_peak,          g_sound_data_peak_w,            1, 0, 0, H_sound_data_peak);
  XEN_DEFINE_SAFE_PROCEDURE(S_sound_data_setB,          g_sound_data_set_w,             4, 0, 0, H_sound_data_setB);
  XEN_DEFINE_SAFE_PROCEDURE(S_sound_data_scaleB,        g_sound_data_scaleB_w,          2, 0, 0, H_sound_data_scaleB);
  XEN_DEFINE_SAFE_PROCEDURE(S_sound_data_reverseB,      g_sound_data_reverseB_w,        1, 0, 0, H_sound_data_reverseB);
  XEN_DEFINE_SAFE_PROCEDURE(S_sound_data_to_vct,        g_sound_data_to_vct_w,          1, 2, 0, H_sound_data_to_vct);
  XEN_DEFINE_SAFE_PROCEDURE(S_sound_data_to_sound_data, g_sound_data_to_sound_data_w,   5, 0, 0, H_sound_data_to_sound_data);
  XEN_DEFINE_SAFE_PROCEDURE(S_vct_to_sound_data,        g_vct_to_sound_data_w,          1, 2, 0, H_vct_to_sound_data);

  XEN_DEFINE_SAFE_PROCEDURE(S_mus_sound_frames,         g_mus_sound_frames_w,           1, 0, 0, H_mus_sound_frames);
  XEN_DEFINE_SAFE_PROCEDURE(S_mus_sound_duration,       g_mus_sound_duration_w,         1, 0, 0, H_mus_sound_duration);
  XEN_DEFINE_SAFE_PROCEDURE(S_mus_sound_datum_size,     g_mus_sound_datum_size_w,       1, 0, 0, H_mus_sound_datum_size);
  XEN_DEFINE_SAFE_PROCEDURE(S_mus_sound_length,         g_mus_sound_length_w,           1, 0, 0, H_mus_sound_length);
  XEN_DEFINE_SAFE_PROCEDURE(S_mus_sound_type_specifier, g_mus_sound_type_specifier_w,   1, 0, 0, H_mus_sound_type_specifier);
  XEN_DEFINE_SAFE_PROCEDURE(S_mus_header_type_name,     g_mus_header_type_name_w,       1, 0, 0, H_mus_header_type_name);
  XEN_DEFINE_SAFE_PROCEDURE(S_mus_header_type_to_string,g_mus_header_type_to_string_w,  1, 0, 0, H_mus_header_type_to_string);
  XEN_DEFINE_SAFE_PROCEDURE(S_mus_header_writable,      g_mus_header_writable_w,        2, 0, 0, H_mus_header_writable);
  XEN_DEFINE_SAFE_PROCEDURE(S_mus_data_format_name,     g_mus_data_format_name_w,       1, 0, 0, H_mus_data_format_name);
  XEN_DEFINE_SAFE_PROCEDURE(S_mus_data_format_to_string,g_mus_data_format_to_string_w,  1, 0, 0, H_mus_data_format_to_string);
  XEN_DEFINE_SAFE_PROCEDURE(S_mus_sound_comment,        g_mus_sound_comment_w,          1, 0, 0, H_mus_sound_comment);
  XEN_DEFINE_SAFE_PROCEDURE(S_mus_sound_write_date,     g_mus_sound_write_date_w,       1, 0, 0, H_mus_sound_write_date);
  XEN_DEFINE_SAFE_PROCEDURE(S_mus_bytes_per_sample,     g_mus_bytes_per_sample_w,       1, 0, 0, H_mus_bytes_per_sample);
  XEN_DEFINE_SAFE_PROCEDURE(S_mus_sound_loop_info,      g_mus_sound_loop_info_w,        1, 0, 0, H_mus_sound_loop_info);
  XEN_DEFINE_SAFE_PROCEDURE(S_mus_sound_mark_info,      g_mus_sound_mark_info_w,        1, 0, 0, H_mus_sound_mark_info);
  XEN_DEFINE_SAFE_PROCEDURE(S_mus_sound_maxamp_exists,  g_mus_sound_maxamp_exists_w,    1, 0, 0, H_mus_sound_maxamp_exists);
  XEN_DEFINE_SAFE_PROCEDURE(S_mus_sound_forget,         g_mus_sound_forget_w,           1, 0, 0, H_mus_sound_forget);
  XEN_DEFINE_SAFE_PROCEDURE(S_mus_sound_prune,          g_mus_sound_prune_w,            0, 0, 0, H_mus_sound_prune);
  XEN_DEFINE_SAFE_PROCEDURE(S_mus_sound_open_input,     g_mus_sound_open_input_w,       1, 0, 0, H_mus_sound_open_input);
  XEN_DEFINE_SAFE_PROCEDURE(S_mus_sound_close_input,    g_mus_sound_close_input_w,      1, 0, 0, H_mus_sound_close_input);

  XEN_DEFINE_SAFE_PROCEDURE(S_mus_audio_close,          g_mus_audio_close_w,            1, 0, 0, H_mus_audio_close);
  XEN_DEFINE_SAFE_PROCEDURE(S_mus_audio_write,          g_mus_audio_write_w,            3, 1, 0, H_mus_audio_write);
  XEN_DEFINE_SAFE_PROCEDURE(S_mus_audio_read,           g_mus_audio_read_w,             3, 0, 0, H_mus_audio_read);
  XEN_DEFINE_SAFE_PROCEDURE(S_mus_audio_open_output,    g_mus_audio_open_output_w,      5, 0, 0, H_mus_audio_open_output);
  XEN_DEFINE_SAFE_PROCEDURE(S_mus_audio_open_input,     g_mus_audio_open_input_w,       5, 0, 0, H_mus_audio_open_input);

  XEN_DEFINE_SAFE_PROCEDURE(S_mus_expand_filename,      g_mus_expand_filename_w,        1, 0, 0, H_mus_expand_filename);
  XEN_DEFINE_SAFE_PROCEDURE(S_mus_sound_open_output,    g_mus_sound_open_output_w,      1, 5, 0, H_mus_sound_open_output);
  XEN_DEFINE_SAFE_PROCEDURE(S_mus_sound_reopen_output,  g_mus_sound_reopen_output_w,    1, 4, 0, H_mus_sound_reopen_output);
  XEN_DEFINE_SAFE_PROCEDURE(S_mus_sound_close_output,   g_mus_sound_close_output_w,     2, 0, 0, H_mus_sound_close_output);
  XEN_DEFINE_SAFE_PROCEDURE(S_mus_sound_read,           g_mus_sound_read_w,             5, 0, 0, H_mus_sound_read);
  XEN_DEFINE_SAFE_PROCEDURE(S_mus_sound_write,          g_mus_sound_write_w,            5, 0, 0, H_mus_sound_write);
  XEN_DEFINE_SAFE_PROCEDURE(S_mus_sound_seek_frame,     g_mus_sound_seek_frame_w,       2, 0, 0, H_mus_sound_seek_frame);
  XEN_DEFINE_SAFE_PROCEDURE(S_mus_sound_report_cache,   g_mus_sound_report_cache_w,     0, 1, 0, H_mus_sound_report_cache);
  XEN_DEFINE_SAFE_PROCEDURE(S_mus_error_type_to_string, g_mus_error_type_to_string_w,   1, 0, 0, H_mus_error_type_to_string);
  XEN_DEFINE_SAFE_PROCEDURE(S_mus_oss_set_buffers,      g_mus_oss_set_buffers_w,        2, 0, 0, H_mus_oss_set_buffers);
  XEN_DEFINE_SAFE_PROCEDURE(S_array_to_file,            g_array_to_file_w,              5, 0, 0, H_array_to_file);
  XEN_DEFINE_SAFE_PROCEDURE(S_file_to_array,            g_file_to_array_w,              5, 0, 0, H_file_to_array);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_header_raw_defaults, g_mus_header_raw_defaults_w, H_mus_header_raw_defaults,
				   S_setB S_mus_header_raw_defaults, g_mus_header_set_raw_defaults_w, 0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_clipping, g_mus_clipping_w, H_mus_clipping,
				   S_setB S_mus_clipping, g_mus_set_clipping_w, 0, 0, 1, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_file_clipping, g_mus_file_clipping_w, H_mus_file_clipping,
				   S_setB S_mus_file_clipping, g_mus_file_set_clipping_w, 1, 0, 2, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_sound_data_ref, g_sound_data_ref_w, H_sound_data_ref,
				   S_setB S_sound_data_ref, g_sound_data_set_w,  3, 0, 4, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_sound_maxamp, g_mus_sound_maxamp_w, H_mus_sound_maxamp,
				   S_setB S_mus_sound_maxamp, g_mus_sound_set_maxamp_w, 1, 0, 2, 0);

  /* these are no-ops if not ALSA, but that makes it easier to maintain global initialization files */
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_alsa_buffers, g_mus_alsa_buffers_w, H_mus_alsa_buffers,
				   S_setB S_mus_alsa_buffers, g_mus_alsa_set_buffers_w, 0, 0, 1, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_alsa_buffer_size, g_mus_alsa_buffer_size_w, H_mus_alsa_buffer_size,
				   S_setB S_mus_alsa_buffer_size, g_mus_alsa_set_buffer_size_w, 0, 0, 1, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_alsa_device, g_mus_alsa_device_w, H_mus_alsa_device,
				   S_setB S_mus_alsa_device, g_mus_alsa_set_device_w, 0, 0, 1, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_alsa_playback_device, g_mus_alsa_playback_device_w, H_mus_alsa_playback_device,
				   S_setB S_mus_alsa_playback_device, g_mus_alsa_set_playback_device_w, 0, 0, 1, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_alsa_capture_device, g_mus_alsa_capture_device_w, H_mus_alsa_capture_device,
				   S_setB S_mus_alsa_capture_device, g_mus_alsa_set_capture_device_w, 0, 0, 1, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_alsa_squelch_warning, g_mus_alsa_squelch_warning_w, H_mus_alsa_squelch_warning,
				   S_setB S_mus_alsa_squelch_warning, g_mus_alsa_set_squelch_warning_w, 0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_max_malloc, g_mus_max_malloc_w, H_mus_max_malloc,
				   S_setB S_mus_max_malloc, g_mus_set_max_malloc_w, 0, 0, 1, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_max_table_size, g_mus_max_table_size_w, H_mus_max_table_size,
				   S_setB S_mus_max_table_size, g_mus_set_max_table_size_w, 0, 0, 1, 0);

#if HAVE_OSS
  XEN_DEFINE_PROCEDURE(S_mus_audio_reinitialize,   g_mus_audio_reinitialize_w, 0, 0, 0,  H_mus_audio_reinitialize);
#endif

#if HAVE_FORTH
  XEN_DEFINE_PROCEDURE(S_sound_data_to_vector, g_sound_data_to_vector /* no _w! */, 1, 0, 0, H_sound_data_to_vector);
#endif

#if __APPLE__
  XEN_DEFINE_PROCEDURE(S_mus_audio_output_properties_mutable, g_mus_audio_output_properties_mutable_w, 1, 0, 0, H_mus_audio_output_properties_mutable);
#endif

#if HAVE_SCHEME
  {
    s7_pointer f;
    /* sound-data-set! */
    f = s7_name_to_value(s7, "sound-data-set!");
    s7_function_set_chooser(s7, f, sound_data_set_chooser);
#if (!WITH_GMP)
    sound_data_set_direct = s7_make_function(s7, "sound-data-set!", g_sound_data_set_direct, 4, 0, false, "sound-data-set! optimization");
    s7_function_set_class(sound_data_set_direct, f);
    
    sound_data_set_four = s7_make_function(s7, "sound-data-set!", g_sound_data_set_w, 4, 0, false, "sound-data-set! optimization");

    sound_data_set_direct_looped = s7_make_function(s7, "sound-data-set!", g_sound_data_set_direct_looped, 4, 0, false, "sound-data-set! optimization");
    s7_function_set_class(sound_data_set_direct_looped, f);
    s7_function_set_looped(sound_data_set_direct, sound_data_set_direct_looped);
    s7_function_set_looped(sound_data_set_four, sound_data_set_direct_looped);
#endif

    /* sound-data-ref */
    f = s7_name_to_value(s7, "sound-data-ref");
    s7_function_set_chooser(s7, f, sound_data_ref_chooser);

    sound_data_ref_three = s7_make_function(s7, "sound-data-ref", g_sound_data_ref_three, 3, 0, false, "sound-data-ref optimization");
    s7_function_set_class(sound_data_ref_three, f);
    s7_function_set_returns_temp(sound_data_ref_three);
  }

#ifndef _MSC_VER
  sound_data_number_location = s7_number_offset(s7);
  if (sound_data_number_location != SOUND_DATA_NUMBER_LOCATION) fprintf(stderr, "number location: %ld %d\n", (long int)sound_data_number_location, SOUND_DATA_NUMBER_LOCATION);

  c_object_value_location = s7_c_object_value_offset(s7);
  if (c_object_value_location != C_OBJECT_VALUE_LOCATION) fprintf(stderr, "value location: %ld %d\n", (long int)c_object_value_location, C_OBJECT_VALUE_LOCATION);
  c_object_type_location = s7_c_object_type_offset(s7);
  if (c_object_type_location != C_OBJECT_TYPE_LOCATION) fprintf(stderr, "object type location: %ld %d\n", (long int)c_object_type_location, C_OBJECT_TYPE_LOCATION);
  cell_type_location = s7_type_offset(s7);
  if (cell_type_location != CELL_TYPE_LOCATION) fprintf(stderr, "cell type location: %ld %d\n", (long int)cell_type_location, CELL_TYPE_LOCATION);
  c_object_built_in_type = s7_c_object_built_in_type(s7);
  if (c_object_built_in_type != C_OBJECT_BUILT_IN_TYPE) fprintf(stderr, "object type: %d %d\n", c_object_built_in_type, C_OBJECT_BUILT_IN_TYPE);
#else
  SOUND_DATA_NUMBER_LOCATION = s7_number_offset(s7);
  SOUND_DATA_DENOMINATOR_LOCATION = s7_denominator_offset(s7);
#endif
#endif


  #define H_new_sound_hook S_new_sound_hook "(name): called when a new sound file is being created"
  new_sound_hook = XEN_DEFINE_HOOK(S_new_sound_hook, "(make-hook 'name)", 1, H_new_sound_hook);
  mus_header_write_set_hook(g_new_sound_hook);

  XEN_PROVIDE("sndlib");
}
