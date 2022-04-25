/* sndins.c -- Sndins for Snd/CLM
 *
 * Copyright (c) 2003-2012 Michael Scholz <mi-scholz@users.sourceforge.net>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 * Commentary:
 *
 * helper functions
 * fcomb functions
 * instruments: fm-violin, jc-reverb, nrev, freeverb
 *
 * mus_any *mus_make_fcomb(mus_float_t scaler, int size,
 *		mus_float_t a0, mus_float_t a1);
 * int mus_fcomb_p(mus_any *ptr);
 * mus_float_t mus_fcomb(mus_any *ptr, mus_float_t input, mus_float_t ignored);
 *
 * mus_long_t ins_fm_violin(mus_float_t start, mus_float_t dur, [...]);
 * mus_long_t ins_jc_reverb(mus_float_t start, mus_float_t dur, [...]);
 * mus_long_t ins_nrev(mus_float_t start, mus_float_t dur, [...]);
 * mus_long_t ins_freeverb(mus_float_t start, mus_float_t dur, [...]);
 *
 * void Init_sndins(void);
 */

#if HAVE_CONFIG_H
#include <mus-config.h>
#endif
#if HAVE_STRING_H
#include <string.h>
#endif
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <stdarg.h>

#include "_sndlib.h"
#include "xen.h"
#include "clm.h"
#include "vct.h"
#include "clm2xen.h"
#include "sndins.h"

#ifndef XEN_PROVIDED_P
#if HAVE_SCHEME
#define XEN_PROVIDED_P(feature)						\
	XEN_TO_C_BOOLEAN(XEN_MEMBER(C_STRING_TO_XEN_SYMBOL(feature),	\
	XEN_NAME_AS_C_STRING_TO_VALUE("*features*")))
#elif HAVE_RUBY
#define XEN_PROVIDED_P(feature)	XEN_TO_C_BOOLEAN(rb_provided(feature))
#elif HAVE_FORTH
#define XEN_PROVIDED_P(feature)	fth_provided_p(feature)
#else
#define XEN_PROVIDED_P(feature)	false
#endif
#endif				/* !XEN_PROVIDED */

#define INS_NO_MEMORY	XEN_ERROR_TYPE("sndins-no-memory-error")
#define INS_MISC	XEN_ERROR_TYPE("sndins-misc-error")

#define INS_ERROR(error, caller, msg)					\
	XEN_ERROR(error, XEN_LIST_2(C_TO_XEN_STRING(caller),		\
	    C_TO_XEN_STRING(msg)))
#define INS_MISC_ERROR(caller, msg)					\
	INS_ERROR(INS_MISC, caller, msg)
#define INS_NO_MEMORY_ERROR()						\
	INS_ERROR(INS_NO_MEMORY, c__FUNCTION__, "cannot allocate memory")

#define SC_startime			"startime"
#define SC_duration			"duration"
#define SC_frequency			"frequency"
#define SC_amplitude			"amplitude"
#define SC_degree			"degree"
#define SC_distance			"distance"
#define SC_volume			"volume"
#define SC_delay1			"delay1"
#define SC_delay2			"delay2"
#define SC_delay3			"delay3"
#define SC_delay4			"delay4"
#define SC_doubled			"doubled"
#define SC_base				"base"
#define SC_damping			"damping"
#define SC_global			"global"
#define SC_predelay			"predelay"
#define SC_combtuning			"combtuning"
#define SC_allpasstuning		"allpasstuning"
#define SC_scaler			"scaler"
#define SC_size				"size"
#define SC_a0				"a0"
#define SC_a1				"a1"
#if HAVE_RUBY
#define SC_amp_env			"amp_env"
#define SC_fm_index			"fm_index"
#define SC_reverb_amount		"reverb_amount"
#define SC_low_pass			"low_pass"
#define SC_periodic_vibrato_rate	"periodic_vibrato_rate"
#define SC_periodic_vibrato_amplitude	"periodic_vibrato_amplitude"
#define SC_random_vibrato_rate		"random_vibrato_rate"
#define SC_random_vibrato_amplitude	"random_vibrato_amplitude"
#define SC_noise_freq			"noise_freq"
#define SC_noise_amount			"noise_amount"
#define SC_ind_noise_freq		"ind_noise_freq"
#define SC_ind_noise_amount		"ind_noise_amount"
#define SC_amp_noise_freq		"amp_noise_freq"
#define SC_amp_noise_amount		"amp_noise_amount"
#define SC_gliss_env			"gliss_env"
#define SC_glissando_amount		"glissando_amount"
#define SC_fm1_env			"fm1_env"
#define SC_fm2_env			"fm2_env"
#define SC_fm3_env			"fm3_env"
#define SC_fm1_rat			"fm1_rat"
#define SC_fm2_rat			"fm2_rat"
#define SC_fm3_rat			"fm3_rat"
#define SC_fm1_index			"fm1_index"
#define SC_fm2_index			"fm2_index"
#define SC_fm3_index			"fm3_index"
#define SC_index_type			"index_type"
#define SC_no_waveshaping		"no_waveshaping"
#define SC_reverb_factor		"reverb_factor"
#define SC_lp_coeff			"lp_coeff"
#define SC_lp_out_coeff			"lp_out_coeff"
#define SC_output_scale			"output_scale"
#define SC_room_decay			"room_decay"
#define SC_output_gain			"output_gain"
#define SC_output_mixer			"output_mixer"
#define SC_scale_room_decay		"scale_room_decay"
#define SC_offset_room_decay		"offset_room_decay"
#define SC_scale_damping		"scale_dumping"
#define SC_stereo_spread		"stereo_spread"
#else				/* !HAVE_RUBY */
#define SC_amp_env			"amp-env"
#define SC_fm_index			"fm-index"
#define SC_reverb_amount		"reverb-amount"
#define SC_low_pass			"low-pass"
#define SC_periodic_vibrato_rate	"periodic-vibrato-rate"
#define SC_periodic_vibrato_amplitude	"periodic-vibrato-amplitude"
#define SC_random_vibrato_rate		"random-vibrato-rate"
#define SC_random_vibrato_amplitude	"random-vibrato-amplitude"
#define SC_noise_freq			"noise-freq"
#define SC_noise_amount			"noise-amount"
#define SC_ind_noise_freq		"ind-noise-freq"
#define SC_ind_noise_amount		"ind-noise-amount"
#define SC_amp_noise_freq		"amp-noise-freq"
#define SC_amp_noise_amount		"amp-noise-amount"
#define SC_gliss_env			"gliss-env"
#define SC_glissando_amount		"glissando-amount"
#define SC_fm1_env			"fm1-env"
#define SC_fm2_env			"fm2-env"
#define SC_fm3_env			"fm3-env"
#define SC_fm1_rat			"fm1-rat"
#define SC_fm2_rat			"fm2-rat"
#define SC_fm3_rat			"fm3-rat"
#define SC_fm1_index			"fm1-index"
#define SC_fm2_index			"fm2-index"
#define SC_fm3_index			"fm3-index"
#define SC_index_type			"index-type"
#define SC_no_waveshaping		"no-waveshaping"
#define SC_reverb_factor		"reverb-factor"
#define SC_lp_coeff			"lp-coeff"
#define SC_lp_out_coeff			"lp-out-coeff"
#define SC_output_scale			"output-scale"
#define SC_room_decay			"room-decay"
#define SC_output_gain			"output-gain"
#define SC_output_mixer			"output-mixer"
#define SC_scale_room_decay		"scale-room-decay"
#define SC_offset_room_decay		"offset-room-decay"
#define SC_scale_damping		"scale-dumping"
#define SC_stereo_spread		"stereo-spread"
#endif				/* !HAVE_RUBY */

enum {
	C_startime,
	C_duration,
	C_frequency,
	C_amplitude,
	C_amp_env,
	C_fm_index,
	C_reverb_amount,
	C_degree,
	C_distance,
	C_periodic_vibrato_rate,
	C_periodic_vibrato_amplitude,
	C_random_vibrato_rate,
	C_random_vibrato_amplitude,
	C_noise_freq,
	C_noise_amount,
	C_ind_noise_freq,
	C_ind_noise_amount,
	C_amp_noise_freq,
	C_amp_noise_amount,
	C_gliss_env,
	C_glissando_amount,
	C_fm1_env,
	C_fm2_env,
	C_fm3_env,
	C_fm1_rat,
	C_fm2_rat,
	C_fm3_rat,
	C_fm1_index,
	C_fm2_index,
	C_fm3_index,
	C_base,
	C_index_type,
	C_no_waveshaping,
	C_low_pass,
	C_volume,
	C_delay1,
	C_delay2,
	C_delay3,
	C_delay4,
	C_doubled,
	C_reverb_factor,
	C_lp_coeff,
	C_lp_out_coeff,
	C_output_scale,
	C_room_decay,
	C_damping,
	C_global,
	C_predelay,
	C_output_gain,
	C_output_mixer,
	C_scale_room_decay,
	C_offset_room_decay,
	C_combtuning,
	C_allpasstuning,
	C_scale_damping,
	C_stereo_spread,
	C_scaler,
	C_size,
	C_a0,
	C_a1,
	NKEYS
};

static char    *keywords[NKEYS] = {
	SC_startime,
	SC_duration,
	SC_frequency,
	SC_amplitude,
	SC_amp_env,
	SC_fm_index,
	SC_reverb_amount,
	SC_degree,
	SC_distance,
	SC_periodic_vibrato_rate,
	SC_periodic_vibrato_amplitude,
	SC_random_vibrato_rate,
	SC_random_vibrato_amplitude,
	SC_noise_freq,
	SC_noise_amount,
	SC_ind_noise_freq,
	SC_ind_noise_amount,
	SC_amp_noise_freq,
	SC_amp_noise_amount,
	SC_gliss_env,
	SC_glissando_amount,
	SC_fm1_env,
	SC_fm2_env,
	SC_fm3_env,
	SC_fm1_rat,
	SC_fm2_rat,
	SC_fm3_rat,
	SC_fm1_index,
	SC_fm2_index,
	SC_fm3_index,
	SC_base,
	SC_index_type,
	SC_no_waveshaping,
	SC_low_pass,
	SC_volume,
	SC_delay1,
	SC_delay2,
	SC_delay3,
	SC_delay4,
	SC_doubled,
	SC_reverb_factor,
	SC_lp_coeff,
	SC_lp_out_coeff,
	SC_output_scale,
	SC_room_decay,
	SC_damping,
	SC_global,
	SC_predelay,
	SC_output_gain,
	SC_output_mixer,
	SC_scale_room_decay,
	SC_offset_room_decay,
	SC_combtuning,
	SC_allpasstuning,
	SC_scale_damping,
	SC_stereo_spread,
	SC_scaler,
	SC_size,
	SC_a0,
	SC_a1
};

#if HAVE_RUBY
#define INS_OUTPUT		"output"
#define INS_REVERB		"reverb"
#define INS_VERBOSE		"clm_verbose"
#define INS_LOCSIG_TYPE		"clm_locsig_type"
#define INS_DECAY_TIME		"clm_decay_time"
#define INS_LIST_BEG		"["
#define INS_LIST_END		"]"
#define INS_FALSE		"false"
#define INS_TRUE		"true"
#define INS_SYMBOL_PREFIX	":"
#else
#define INS_OUTPUT		"*output*"
#define INS_REVERB		"*reverb*"
#define INS_VERBOSE		"*clm-verbose*"
#define INS_LOCSIG_TYPE		"*clm-locsig-type*"
#define INS_DECAY_TIME		"*clm-decay-time*"
#define INS_LIST_BEG		"'("
#define INS_LIST_END		")"
#define INS_FALSE		"#f"
#define INS_TRUE		"#t"
#define INS_SYMBOL_PREFIX	"'"
#endif

/* utility functions */
static mus_float_t *array2array(mus_float_t *list, int len);
static bool	array_equal_p(mus_float_t *env1, int len1,
		    mus_float_t *env2, int len2);
static int	feq  (mus_float_t x, int i);
static char    *format_array(mus_float_t *line, int size);
static bool	get_global_boolean(const char *name, bool def);
static mus_float_t get_global_float(const char *name, mus_float_t def);
static int	get_global_int(const char *name, int def);
static mus_any *get_global_mus_gen(const char *name);
static int	ins_message(const char *fmt,...);
static int     *int_array2array(int *list, int len);
static bool	prime_p(int x);
static mus_float_t *xen_list2array(XEN list);
static int     *xen_list2iarray(XEN list);

/* fcomb generator */
static mus_float_t *fcomb_data(mus_any *ptr);
static char    *fcomb_describe(mus_any *ptr);
static bool	fcomb_equal_p(mus_any *p1, mus_any *p2);
static int	fcomb_free(mus_any *ptr);
static mus_long_t fcomb_length(mus_any *ptr);
static void	fcomb_reset(mus_any *ptr);
static mus_float_t fcomb_scaler(mus_any *ptr);
static mus_float_t fcomb_xcoeff(mus_any *ptr, int index);
static mus_float_t *fcomb_xcoeffs(mus_any *ptr);
static mus_float_t set_fcomb_scaler(mus_any *ptr, mus_float_t val);
static mus_float_t set_fcomb_xcoeff(mus_any *ptr, int index, mus_float_t val);

/* XEN fcomb */
static XEN	c_fcomb(XEN gen, XEN input);
static XEN	c_fcomb_p(XEN gen);
static XEN	c_make_fcomb(XEN args);

/* instruments */
#if HAVE_FORTH
static void	c_fm_violin(XEN args);
static void	c_freeverb(XEN args);
static void	c_jc_reverb(XEN args);
static void	c_nrev(XEN args);
#else
static XEN	c_fm_violin(XEN args);
static XEN	c_freeverb(XEN args);
static XEN	c_jc_reverb(XEN args);
static XEN	c_nrev(XEN args);
#endif

static XEN	allkeys[NKEYS];

static void
init_keywords(void)
{
	int i;

	for (i = 0; i < NKEYS; i++)
		allkeys[i] = XEN_MAKE_KEYWORD(keywords[i]);
}

static mus_any *
get_global_mus_gen(const char *name)
{
	if (XEN_DEFINED_P(name)) {
		XEN value;

		value = XEN_NAME_AS_C_STRING_TO_VALUE(name);
		if (mus_xen_p(value))
			return (XEN_TO_MUS_ANY(value));
	}
	return (NULL);
}

static bool
get_global_boolean(const char *name, bool def)
{
	if (XEN_DEFINED_P(name)) {
		XEN value;

		value = XEN_NAME_AS_C_STRING_TO_VALUE(name);
		if (XEN_BOOLEAN_P(value))
			return (XEN_TO_C_BOOLEAN(value));
	}
	return (def);
}

static int
get_global_int(const char *name, int def)
{
	if (XEN_DEFINED_P(name)) {
		XEN value;

		value = XEN_NAME_AS_C_STRING_TO_VALUE(name);
		if (XEN_NUMBER_P(value))
			return (XEN_TO_C_INT(value));
	}
	return (def);
}

static mus_float_t
get_global_float(const char *name, mus_float_t def)
{
	if (XEN_DEFINED_P(name)) {
		XEN value;

		value = XEN_NAME_AS_C_STRING_TO_VALUE(name);
		if (XEN_NUMBER_P(value))
			return (XEN_TO_C_DOUBLE(value));
	}
	return (def);
}

/*
 * Return mus_float_t array initialized with contents of XEN list.
 * (mus_make_env())
 */
static mus_float_t *
xen_list2array(XEN list)
{
	int len, i = 0;
	mus_float_t *flist;
	XEN lst;

	len = XEN_LIST_LENGTH(list);
	if (len == 0)
		return (NULL);

	flist = malloc(len * sizeof(mus_float_t));
	if (flist == NULL) {
		INS_NO_MEMORY_ERROR();
		/* NOTREACHED */
		return (NULL);
	}
	for (i = 0, lst = XEN_COPY_ARG(list); i < len; i++, lst = XEN_CDR(lst))
		flist[i] = XEN_TO_C_DOUBLE_OR_ELSE(XEN_CAR(lst), 0.0);
	return (flist);
}

/*
 * Return int array initialized with contents of XEN list.
 * (mus_make_mixer())
 */
static int *
xen_list2iarray(XEN list)
{
	int len, i = 0;
	int *ilist;
	XEN lst;

	len = XEN_LIST_LENGTH(list);
	if (len == 0)
		return (NULL);

	ilist = malloc(len * sizeof(int));
	if (ilist == NULL) {
		INS_NO_MEMORY_ERROR();
		/* NOTREACHED */
		return (NULL);
	}
	for (i = 0, lst = XEN_COPY_ARG(list); i < len; i++, lst = XEN_CDR(lst))
		ilist[i] = XEN_TO_C_INT_OR_ELSE(XEN_CAR(lst), 0);
	return (ilist);
}

/*
 * Return mus_float_t array initialized with contents of mus_float_t list.
 * (mus_make_env())
 */
static mus_float_t *
array2array(mus_float_t *list, int len)
{
	int i = 0;
	mus_float_t *flist;

	if (len == 0)
		return (NULL);

	flist = malloc(len * sizeof(mus_float_t));
	if (flist == NULL) {
		INS_NO_MEMORY_ERROR();
		/* NOTREACHED */
		return (NULL);
	}
	for (i = 0; i < len; i++)
		flist[i] = (mus_float_t)list[i];
	return (flist);
}

/*
 * Return int array initialized with contents of int list.
 * (mus_make_mixer())
 */
static int *
int_array2array(int *list, int len)
{
	int i = 0;
	int *ilist;

	if (len == 0)
		return (NULL);

	ilist = malloc(len * sizeof(int));
	if (ilist == NULL) {
		INS_NO_MEMORY_ERROR();
		/* NOTREACHED */
		return (NULL);
	}
	for (i = 0; i < len; i++)
		ilist[i] = (int)list[i];
	return (ilist);
}

/*
 * ins_fm_violin: easy_case
 */
static bool
array_equal_p(mus_float_t *env1, int len1, mus_float_t *env2, int len2)
{
	if (env1 != NULL && env2 != NULL && len1 == len2) {
		int i;

		for (i = 0; i < len1; i++)
			if (env1[i] != env2[i])
				return (false);
		return (true);
	}
	return (false);
}

/*
 * Print message in listener.
 */
#if HAVE_SCHEME
#define INS_COMMENT_STRING ";;"
#else
#define INS_COMMENT_STRING XEN_COMMENT_STRING
#endif

static int
ins_message(const char *fmt,...)
{
	int len, result;
	va_list ap;
	char *str;

	len = strlen(fmt) + 128;
	str = malloc(len * sizeof(char));
	if (str == NULL) {
		INS_NO_MEMORY_ERROR();
		/* NOTREACHED */
		return (-1);
	}
	va_start(ap, fmt);
	result = vsnprintf(str, len, fmt, ap);
	va_end(ap);

	if (XEN_PROVIDED_P("snd") && !XEN_PROVIDED_P("snd-nogui"))
		mus_print(INS_COMMENT_STRING " %s", str);
	else
		fprintf(stdout, INS_COMMENT_STRING " %s", str);
	free(str);
	return (result);
}

static int
feq(mus_float_t x, int i)
{
	return (fabs(x - i) < 0.00001);
}

static bool
prime_p(int x)
{
	if (x == 2)
		return (true);
	if (x % 2) {
		int i = 0;

		for (i = 3; i < sqrt(x); i += 2)
			if ((x % i) == 0)
				return (false);
		return (true);
	}
	return (false);
}

#define INS_FMT_SIZE 128

static char *
format_array(mus_float_t *line, int size)
{
	int i, lim = size;
	char *str = NULL;

	if (lim > mus_array_print_length())
		lim = mus_array_print_length();

	str = calloc(INS_FMT_SIZE * lim * 2, sizeof(char));
	if (str == NULL) {
		INS_NO_MEMORY_ERROR();
		/* NOTREACHED */
		return (NULL);
	}
	if (line != NULL) {
		char *tmp;

		tmp = calloc(INS_FMT_SIZE, sizeof(char));
		if (tmp == NULL) {
			INS_NO_MEMORY_ERROR();
			/* NOTREACHED */
			return (NULL);
		}
		strcpy(str, "[");
		for (i = 0; i < lim - 1; i++) {
			mus_snprintf(tmp, INS_FMT_SIZE, "%.3f ", line[i]);
			strcat(str, tmp);
		}
		mus_snprintf(tmp, INS_FMT_SIZE, "%.3f%s]",
		    line[i], (size > lim) ? "..." : "");
		strcat(str, tmp);
		free(tmp);
	} else
		strcpy(str, "nil");
	return (str);
}

/* --- fcomb --- */

typedef struct {
	mus_any_class  *core;
	int		loc;
	int		size;	/* mus_length(gen) */
	mus_float_t    *line;	/* mus_data(gen) */
	mus_float_t	xs  [2];/* a0, a1; mus_xcoeff(gen, 0 or 1) */
	mus_float_t	xscl;	/* mus_scaler(gen) */
	mus_float_t	x1;
} fcomb;

static int	MUS_FCOMB = 0;

int
mus_fcomb_p(mus_any *ptr)
{
	return (ptr != NULL && mus_type(ptr) == MUS_FCOMB);
}

#define INS_DESCRIBE_SIZE 2048
#define FC_DESC								\
	"fcomb: scaler: %.3f, a0: %.3f, a1: %.3f, x1: %.3f, line[%d]: %s"

static char *
fcomb_describe(mus_any *ptr)
{
	char *desc;
	fcomb *gen = (fcomb *)ptr;

	desc = calloc(INS_DESCRIBE_SIZE, sizeof(char));
	if (desc == NULL) {
		INS_NO_MEMORY_ERROR();
		/* NOTREACHED */
		return (NULL);
	}
	if (mus_fcomb_p(ptr)) {
		char *s;

		s = format_array(gen->line, gen->size);
		mus_snprintf(desc, INS_DESCRIBE_SIZE, FC_DESC,
		    gen->xscl, gen->xs[0], gen->xs[1], gen->x1, gen->size, s);
		free(s);
	} else
		mus_snprintf(desc, INS_DESCRIBE_SIZE, "not an fcomb gen");
	return (desc);
}

static bool
fcomb_equal_p(mus_any *p1, mus_any *p2)
{
	fcomb *g1 = (fcomb *)p1;
	fcomb *g2 = (fcomb *)p2;

	return (mus_fcomb_p(p1) && ((p1 == p2) ||
	    (mus_fcomb_p(p2) &&
	    g1->xs[0] == g2->xs[0] &&
	    g1->xs[1] == g2->xs[1] &&
	    g1->x1 == g2->x1)));
}

static mus_long_t
fcomb_length(mus_any *ptr)
{
	fcomb *gen = (fcomb *)ptr;

	return ((mus_long_t)(gen->size));
}

static mus_float_t *
fcomb_data(mus_any *ptr)
{
	fcomb *gen = (fcomb *)ptr;

	return (gen->line);
}

static mus_float_t
fcomb_scaler(mus_any *ptr)
{
	fcomb *gen = (fcomb *)ptr;

	return (gen->xscl);
}

static mus_float_t
set_fcomb_scaler(mus_any *ptr, mus_float_t val)
{
	fcomb *gen = (fcomb *)ptr;

	gen->xscl = val;
	return (val);
}

static mus_float_t
fcomb_xcoeff(mus_any *ptr, int index)
{
	fcomb *gen = (fcomb *)ptr;

	if (index < 0 || index > 1)
		index = 0;
	return (gen->xs[index]);
}

static mus_float_t
set_fcomb_xcoeff(mus_any *ptr, int index, mus_float_t val)
{
	fcomb *gen = (fcomb *)ptr;

	if (index < 0 || index > 1)
		index = 0;
	gen->xs[index] = val;
	return (val);
}

static mus_float_t *
fcomb_xcoeffs(mus_any *ptr)
{
	fcomb *gen = (fcomb *)ptr;

	return (gen->xs);
}

static void
fcomb_reset(mus_any *ptr)
{
	fcomb *gen = (fcomb *)ptr;

	gen->x1 = 0.0;
}

static int
fcomb_free(mus_any *ptr)
{
	fcomb *gen = (fcomb *)ptr;

	if (gen != NULL) {
		if (gen->line != NULL)
			free(gen->line);
		gen->line = NULL;
		free(gen);
	}
	return (0);
}

/*
 * Now the actual run-time code executed by fcomb the extra "ignored"
 * argument is for the run method
 */
mus_float_t
mus_fcomb(mus_any *ptr, mus_float_t input,
    mus_float_t ignored __attribute__((unused)))
{
	fcomb *gen = (fcomb *)ptr;
	mus_float_t tap_result, filter_result;

	tap_result = gen->line[gen->loc];
	filter_result = (gen->xs[0] * tap_result) + (gen->xs[1] * gen->x1);
	gen->x1 = tap_result;
	gen->line[gen->loc] = input + filter_result * gen->xscl;
	gen->loc++;
	if (gen->loc >= gen->size)
		gen->loc = 0;
	return (tap_result);
}

/*
 * Now a function to make a new generator.
 */
mus_any        *
mus_make_fcomb(mus_float_t scaler, int size, mus_float_t a0, mus_float_t a1)
{
	fcomb *gen;

	gen = calloc(1, sizeof(fcomb));
	if (gen == NULL) {
		INS_NO_MEMORY_ERROR();
		/* NOTREACHED */
		return (NULL);
	}
	gen->line = calloc(size, sizeof(mus_float_t));
	if (gen->line == NULL) {
		INS_NO_MEMORY_ERROR();
		/* NOTREACHED */
		return (NULL);
	} else {
		int i;

		for (i = 0; i < size; i++)
			gen->line[i] = 0.0;
	}

	MUS_FCOMB = mus_make_generator_type();
	gen->core = mus_make_generator(MUS_FCOMB, "fcomb", fcomb_free,
	    fcomb_describe, fcomb_equal_p);
	if (gen->core == NULL) {
		INS_NO_MEMORY_ERROR();
		/* NOTREACHED */
		return (NULL);
	}
	gen->loc = 0;
	gen->xscl = scaler;
	gen->x1 = 0.0;
	gen->xs[0] = a0;
	gen->xs[1] = a1;
	gen->size = size;
	mus_generator_set_data(gen->core, fcomb_data);
	mus_generator_set_length(gen->core, fcomb_length);
	mus_generator_set_reset(gen->core, fcomb_reset);
	mus_generator_set_scaler(gen->core, fcomb_scaler);
	mus_generator_set_set_scaler(gen->core, set_fcomb_scaler);
	mus_generator_set_xcoeff(gen->core, fcomb_xcoeff);
	mus_generator_set_set_xcoeff(gen->core, set_fcomb_xcoeff);
	mus_generator_set_xcoeffs(gen->core, fcomb_xcoeffs);
	return ((mus_any *)gen);
}

/*
 * That is the end of the C side; the rest ties this generator into
 * Scheme/Ruby/Forth via the Xen package in Snd's case, it's actually
 * not needed because the generator is only called from C.
 */

#define h_make_fcomb_args "\
keyword arguments with default values:\n\
 :scaler 1.0\n\
 :size   1\n\
 :a0     0.0\n\
 :a1     0.0\n\
Return new fcomb generator."

#if HAVE_RUBY
#define S_make_fcomb	"make_fcomb"
#else
#define S_make_fcomb	"make-fcomb"
#endif

#if HAVE_RUBY
#define H_make_fcomb	S_make_fcomb "(*args)\n" h_make_fcomb_args
#elif HAVE_FORTH
#define H_make_fcomb	S_make_fcomb " ( args -- gen )\n" h_make_fcomb_args
#else				/* HAVE_SCHEME */
#define H_make_fcomb	"(" S_make_fcomb " . args)\n" h_make_fcomb_args
#endif

#define MAX_TABLE_SIZE (1024 * 1024 * 20)

static XEN
c_make_fcomb(XEN args)
{
#define FCOMB_LAST_KEY 4
	int i, keyn, argn, vals, lst_len, size;
	mus_float_t scaler, a0, a1;
	XEN kargs[FCOMB_LAST_KEY * 2], keys[FCOMB_LAST_KEY];
	int orig_arg[FCOMB_LAST_KEY] = {0};
	mus_any *ge;

	argn = 0;
	lst_len = XEN_LIST_LENGTH(args);
	size = 1;
	scaler = 1.0;
	a0 = a1 = 0.0;
	keys[argn++] = allkeys[C_scaler];
	keys[argn++] = allkeys[C_size];
	keys[argn++] = allkeys[C_a0];
	keys[argn++] = allkeys[C_a1];

	for (i = 0; i < FCOMB_LAST_KEY * 2; i++)
		kargs[i] = XEN_UNDEFINED;
	for (i = 0; i < lst_len; i++)
		kargs[i] = XEN_LIST_REF(args, i);

	vals = mus_optkey_unscramble(S_make_fcomb, argn, keys, kargs, orig_arg);
	if (vals > 0) {
		keyn = 0;
		scaler = mus_optkey_to_float(keys[keyn], S_make_fcomb,
		    orig_arg[keyn], scaler);
		keyn++;
		size = mus_optkey_to_int(keys[keyn], S_make_fcomb,
		    orig_arg[keyn], size);
		if (size < 0)
			XEN_OUT_OF_RANGE_ERROR(S_make_fcomb, orig_arg[keyn],
			    keys[keyn], "size < 0?");
		if (size > MAX_TABLE_SIZE)
			XEN_OUT_OF_RANGE_ERROR(S_make_fcomb, orig_arg[keyn],
			    keys[keyn], "size too large");
		keyn++;
		a0 = mus_optkey_to_float(keys[keyn], S_make_fcomb,
		    orig_arg[keyn], a0);
		keyn++;
		a1 = mus_optkey_to_float(keys[keyn], S_make_fcomb,
		    orig_arg[keyn], a1);
	}
	ge = mus_make_fcomb(scaler, size, a0, a1);
	if (ge) {
		fcomb *g = (fcomb *)ge;

		return (mus_xen_to_object(mus_any_to_mus_xen_with_two_vcts(ge,
		    xen_make_vct(size, g->line), xen_make_vct(2, g->xs))));
	}
	return (XEN_FALSE);
}

#define S_fcomb_p	"fcomb?"
#define h_fcomb_p_doc	"\
Return " INS_TRUE " if GEN is an fcomb generator, otherwise " INS_FALSE "."

#if HAVE_RUBY
#define H_fcomb_p	S_fcomb_p "(gen)  " h_fcomb_p_doc
#elif HAVE_FORTH
#define H_fcomb_p	S_fcomb_p " ( gen -- f )  " h_fcomb_p_doc
#else				/* HAVE_SCHEME */
#define H_fcomb_p	"(" S_fcomb_p " gen)  " h_fcomb_p_doc
#endif

#define XEN_FCOMB_P(obj)						\
	(mus_xen_p(obj) && mus_fcomb_p(XEN_TO_MUS_ANY(obj)))

static XEN
c_fcomb_p(XEN obj)
{
	return (C_TO_XEN_BOOLEAN(XEN_FCOMB_P(obj)));
}

#define S_fcomb		"fcomb"
#define h_fcomb_doc	"Return result of running fcomb generator."

#if HAVE_RUBY
#define H_fcomb	S_fcomb	"(gen, input=0.0)  " h_fcomb_doc
#elif HAVE_FORTH
#define H_fcomb	S_fcomb	" ( gen input=0.0 -- res )  " h_fcomb_doc
#else				/* HAVE_SCHEME */
#define H_fcomb		"(" S_fcomb " gen (input 0.0))  " h_fcomb_doc
#endif

static XEN
c_fcomb(XEN gen, XEN input)
{
	mus_any *ge;
	mus_float_t fm = 0.0;

	ge = XEN_TO_MUS_ANY(gen);
	XEN_ASSERT_TYPE(mus_fcomb_p(ge), gen, XEN_ARG_1, S_fcomb, "an fcomb");
	if (XEN_BOUND_P(input))
		fm = XEN_TO_C_DOUBLE(input);
	return (C_TO_XEN_DOUBLE(mus_fcomb(ge, fm, 0.0)));
}

/* --- instruments --- */
#if HAVE_RUBY
#define S_fm_violin	"fm_violin"
#define S_jc_reverb	"jc_reverb"
#else
#define S_fm_violin	"fm-violin"
#define S_jc_reverb	"jc-reverb"
#endif
#define S_nrev		"nrev"
#define S_freeverb	"freeverb"

mus_long_t
ins_fm_violin(mus_float_t start, mus_float_t dur, mus_float_t freq,
    mus_float_t amp, mus_float_t fm_index, mus_float_t *amp_env,
    int amp_len, mus_float_t periodic_vibrato_rate,
    mus_float_t periodic_vibrato_amp, mus_float_t random_vibrato_rate,
    mus_float_t random_vibrato_amp, mus_float_t noise_freq,
    mus_float_t noise_amount, mus_float_t ind_noise_freq,
    mus_float_t ind_noise_amount, mus_float_t amp_noise_freq,
    mus_float_t amp_noise_amount, mus_float_t *gliss_env, int gliss_len,
    mus_float_t gliss_amount, mus_float_t *fm1_env, int fm1_len,
    mus_float_t *fm2_env, int fm2_len, mus_float_t *fm3_env, int fm3_len,
    mus_float_t fm1_rat, mus_float_t fm2_rat, mus_float_t fm3_rat,
    mus_float_t fm1_index, mus_float_t fm2_index, mus_float_t fm3_index,
    mus_float_t base, mus_float_t degree, mus_float_t distance,
    mus_float_t reverb_amount, bool index_type, bool no_waveshaping,
    mus_any *out, mus_any *rev, mus_interp_t mode)
{
	mus_long_t i, beg, len;
	int out_chans, rev_chans;
	bool vln, easy_case, modulate;
	mus_float_t frq_scl, maxdev, index1, index2, index3, vib;
	mus_float_t logfrq, sqrtfrq, norm, mod;
	mus_float_t *partials = NULL;
	mus_float_t fuzz, ind_fuzz, amp_fuzz;
	mus_any *carrier, *fmosc1, *fmosc2 = NULL, *fmosc3 = NULL;
	mus_any *ampf, *indf1, *indf2 = NULL, *indf3 = NULL, *pervib;
	mus_any *fm_noi = NULL, *ind_noi = NULL, *amp_noi = NULL;
	mus_any *ranvib, *frqf = NULL, *loc;

	out_chans = 1;
	rev_chans = 0;
	vln = true;
	easy_case = modulate = false;
	frq_scl = maxdev = index1 = index2 = index3 = vib = 0.0;
	logfrq = sqrtfrq = norm = mod = fuzz = 0.0;
	ind_fuzz = amp_fuzz = 1.0;
	beg = mus_seconds_to_samples(start);
	len = beg + mus_seconds_to_samples(dur);
	frq_scl = mus_hz_to_radians(freq);
	modulate = (fm_index != 0.0);
	maxdev = frq_scl * fm_index;
	logfrq = log(freq);
	sqrtfrq = sqrt(freq);
	vln = index_type;	/* true: violin, false: cello */

	if (fm1_index != 0.0)
		index1 = fm1_index;
	else {
		index1 = vln ?
		    (maxdev * 5.0 / logfrq) :
		    (maxdev * 7.5 / logfrq);
		if (index1 > M_PI)
			index1 = M_PI;
	}
	if (fm2_index != 0.0)
		index2 = fm2_index;
	else {
		index2 = vln ?
		    (maxdev * 3.0 * (8.5 - logfrq) / (3.0 + freq * 0.001)) :
		    (maxdev * 3.0 * 15.0 / sqrtfrq);
		if (index2 > M_PI)
			index2 = M_PI;
	}
	if (fm3_index != 0.0)
		index3 = fm3_index;
	else {
		index3 = vln ? (maxdev * 4.0 / sqrtfrq) :
		    (maxdev * 8.0 / sqrtfrq);
		if (index3 > M_PI)
			index3 = M_PI;
	}
	easy_case = (noise_amount == 0.0 && !no_waveshaping &&
	    array_equal_p(fm1_env, fm1_len, fm2_env, fm2_len) &&
	    array_equal_p(fm1_env, fm1_len, fm3_env, fm3_len) &&
	    feq(fm1_rat, (int)floor(fm1_rat)) &&
	    feq(fm2_rat, (int)floor(fm2_rat)) &&
	    feq(fm3_rat, (int)floor(fm3_rat)));
	if (easy_case && modulate)
		norm = 1.0;
	else
		norm = index1;
	carrier = mus_make_oscil(freq, 0.0);

	if (modulate) {
		if (easy_case) {
			int nparts;

			nparts = (int)floor(fm1_rat);
			if ((int)floor(fm2_rat) > nparts)
				nparts = (int)floor(fm2_rat);
			if ((int)floor(fm3_rat) > nparts)
				nparts = (int)floor(fm3_rat);
			nparts++;
			partials = calloc(nparts, sizeof(mus_float_t));
			partials[(int)(fm1_rat)] = index1;
			partials[(int)(fm2_rat)] = index2;
			partials[(int)(fm3_rat)] = index3;
			fmosc1 = mus_make_polyshape(freq * fm1_rat, 0.0,
			    mus_partials_to_polynomial(nparts, partials, 1),
			    nparts, MUS_CHEBYSHEV_FIRST_KIND);
		} else
			fmosc1 = mus_make_oscil(freq * fm1_rat, 0.0);
		indf1 = mus_make_env(fm1_env, fm1_len / 2, norm, 0.0, 1.0,
		    dur, 0, NULL);
		if (!easy_case) {
			fmosc2 = mus_make_oscil(freq * fm2_rat, 0.0);
			fmosc3 = mus_make_oscil(freq * fm3_rat, 0.0);
			indf2 = mus_make_env(fm2_env, fm2_len / 2, index2,
			    0.0, 1.0, dur, 0, NULL);
			indf3 = mus_make_env(fm3_env, fm3_len / 2, index3,
			    0.0, 1.0, dur, 0, NULL);
		}
	}
	ampf = mus_make_env(amp_env, amp_len / 2, amp, 0.0, base, dur, 0, NULL);
	frqf = mus_make_env(gliss_env, gliss_len / 2, gliss_amount * frq_scl,
	    0.0, 1.0, dur, 0, NULL);
	pervib = mus_make_triangle_wave(periodic_vibrato_rate,
	    periodic_vibrato_amp * frq_scl, 0.0);
	ranvib = mus_make_rand_interp(random_vibrato_rate,
	    random_vibrato_amp * frq_scl);
	if (noise_amount != 0.0)
		fm_noi = mus_make_rand(noise_freq, noise_amount * M_PI);
	if (ind_noise_amount != 0.0 && ind_noise_freq != 0.0)
		ind_noi = mus_make_rand_interp(ind_noise_freq,
		    ind_noise_amount);
	if (amp_noise_amount != 0.0 && amp_noise_freq != 0.0)
		amp_noi = mus_make_rand_interp(amp_noise_freq,
		    amp_noise_amount);
	if (degree == 0.0)
		degree = mus_random(90.0);
	if (out)
		out_chans = mus_channels(out);
	if (rev)
		rev_chans = mus_channels(rev);
	loc = mus_make_locsig(degree, distance, reverb_amount,
	    out_chans, out, rev_chans, rev, mode);

	for (i = beg; i < len; i++) {
		if (fm_noi)
			fuzz = mus_rand(fm_noi, 0.0);
		vib = mus_env(frqf) + mus_triangle_wave(pervib, 0.0) +
		    mus_rand_interp(ranvib, 0.0);
		if (ind_noi)
			ind_fuzz = 1.0 + mus_rand_interp(ind_noi, 0.0);
		if (amp_noi)
			amp_fuzz = 1.0 + mus_rand_interp(amp_noi, 0.0);
		if (modulate) {
			if (easy_case)
				mod = mus_env(indf1) *
				    mus_polyshape_unmodulated(fmosc1, vib);
			else
				mod = mus_env(indf1) * mus_oscil(fmosc1,
				    fuzz + fm1_rat * vib, 0.0) +
				    mus_env(indf2) * mus_oscil(fmosc2,
				    fuzz + fm2_rat * vib, 0.0) +
				    mus_env(indf3) * mus_oscil(fmosc3,
				    fuzz + fm3_rat * vib, 0.0);
		}
		mus_locsig(loc, i, mus_env(ampf) * amp_fuzz *
		    mus_oscil(carrier, vib + ind_fuzz * mod, 0.0));
	}

	mus_free(pervib);
	mus_free(ranvib);
	mus_free(carrier);
	mus_free(fmosc1);
	mus_free(ampf);
	mus_free(indf1);
	mus_free(loc);
	if (fm_noi)
		mus_free(fm_noi);
	if (ind_noi)
		mus_free(ind_noi);
	if (amp_noi)
		mus_free(amp_noi);
	if (frqf)
		mus_free(frqf);
	if (fmosc2)
		mus_free(fmosc2);
	if (fmosc3)
		mus_free(fmosc3);
	if (indf2)
		mus_free(indf2);
	if (indf3)
		mus_free(indf3);
	if (partials)
		free(partials);
	return (i);
}

#define JC_WARN	"is not set up for doubled reverb in quad"

mus_long_t
ins_jc_reverb(mus_float_t start, mus_float_t dur, mus_float_t volume,
    bool low_pass, bool doubled, mus_float_t delay1, mus_float_t delay2,
    mus_float_t delay3, mus_float_t delay4, mus_float_t *amp_env, int amp_len,
    mus_any *out, mus_any *rev)
{
	mus_long_t i, beg, len;
	int del_len, chans, rev_chans;
	bool chan2, chan4;
	mus_float_t delA, delB;
	mus_float_t allpass_sum, comb_sum, comb_sum_1, comb_sum_2, all_sums;
	mus_any *allpass1, *allpass2, *allpass3;
	mus_any *comb1, *comb2, *comb3, *comb4;
	mus_any *outdel1, *outdel2 = NULL, *outdel3 = NULL, *outdel4 = NULL;
	mus_any *env_a = NULL;

	del_len = chans = rev_chans = 0;
	chan2 = chan4 = false;
	delA = delB = 0.0;
	allpass_sum = comb_sum = comb_sum_1 = comb_sum_2 = all_sums = 0.0;
	beg = mus_seconds_to_samples(start);

	if (dur > 0.0)
		len = beg + mus_seconds_to_samples(dur);
	else
		len = beg + mus_seconds_to_samples(1.0) +
		    mus_sound_frames(mus_file_name(rev));

	chans = mus_channels(out);
	rev_chans = mus_channels(rev);
	allpass1 = mus_make_all_pass(-0.7, 0.7, 1051, NULL, 1051,
	    MUS_INTERP_LINEAR);
	allpass2 = mus_make_all_pass(-0.7, 0.7, 337, NULL, 337,
	    MUS_INTERP_LINEAR);
	allpass3 = mus_make_all_pass(-0.7, 0.7, 113, NULL, 113,
	    MUS_INTERP_LINEAR);
	comb1 = mus_make_comb(0.742, 4799, NULL, 4799, MUS_INTERP_LINEAR);
	comb2 = mus_make_comb(0.733, 4999, NULL, 4999, MUS_INTERP_LINEAR);
	comb3 = mus_make_comb(0.715, 5399, NULL, 5399, MUS_INTERP_LINEAR);
	comb4 = mus_make_comb(0.697, 5801, NULL, 5801, MUS_INTERP_LINEAR);
	if (chans > 1)
		chan2 = true;
	if (chans == 4)
		chan4 = true;
	del_len = mus_seconds_to_samples(delay1);
	outdel1 = mus_make_delay(del_len, NULL, del_len, MUS_INTERP_LINEAR);
	if (chan2) {
		del_len = mus_seconds_to_samples(delay2);
		outdel2 = mus_make_delay(del_len, NULL, del_len,
		    MUS_INTERP_LINEAR);
	}
	if (doubled || chan4) {
		del_len = mus_seconds_to_samples(delay3);
		outdel3 = mus_make_delay(del_len, NULL, del_len,
		    MUS_INTERP_LINEAR);
	}
	if (chan4 || (doubled && chan2)) {
		del_len = mus_seconds_to_samples(delay4);
		outdel4 = mus_make_delay(del_len, NULL, del_len,
		    MUS_INTERP_LINEAR);
	}
	if (amp_env)
		env_a = mus_make_env(amp_env, amp_len / 2, volume, 0.0, 1.0,
		    dur, 0, NULL);
	if (doubled && chan4)
		INS_MISC_ERROR(S_jc_reverb, JC_WARN);

	for (i = beg; i < len; i++) {
		int j;
		mus_float_t ho;

		for (j = 0, ho = 0.0; j < rev_chans; j++)
			ho += mus_in_any(i, j, rev);

		allpass_sum = mus_all_pass_unmodulated(allpass3,
		    mus_all_pass_unmodulated(allpass2,
		    mus_all_pass_unmodulated(allpass1, ho)));
		comb_sum_2 = comb_sum_1;
		comb_sum_1 = comb_sum;
		comb_sum = mus_comb_unmodulated(comb1, allpass_sum) +
		    mus_comb_unmodulated(comb2, allpass_sum) +
		    mus_comb_unmodulated(comb3, allpass_sum) +
		    mus_comb_unmodulated(comb4, allpass_sum);

		if (low_pass)
			all_sums = 0.25 * (comb_sum + comb_sum_2) +
			    0.5 * comb_sum_1;
		else
			all_sums = comb_sum;

		delA = mus_delay_unmodulated(outdel1, all_sums);

		if (doubled)
			delA += mus_delay_unmodulated(outdel3, all_sums);

		if (env_a)
			volume = mus_env(env_a);

		mus_out_any(i, delA * volume, 0, out);

		if (chan2) {
			delB = mus_delay_unmodulated(outdel2, all_sums);

			if (doubled)
				delB += mus_delay_unmodulated(outdel4,
				    all_sums);

			mus_out_any(i, delB * volume, 1, out);

			if (chan4) {
				mus_out_any(i, volume *
				    mus_delay_unmodulated(outdel3, all_sums),
				    2, out);
				mus_out_any(i, volume *
				    mus_delay_unmodulated(outdel4, all_sums),
				    3, out);
			}
		}
	}
	mus_free(allpass1);
	mus_free(allpass2);
	mus_free(allpass3);
	mus_free(comb1);
	mus_free(comb2);
	mus_free(comb3);
	mus_free(comb4);
	mus_free(outdel1);
	if (outdel2)
		mus_free(outdel2);
	if (outdel3)
		mus_free(outdel3);
	if (outdel4)
		mus_free(outdel4);
	if (env_a)
		mus_free(env_a);
	return (i);
}

mus_long_t
ins_nrev(mus_float_t start, mus_float_t dur, mus_float_t reverb_factor,
    mus_float_t lp_coeff, mus_float_t lp_out_coeff, mus_float_t output_scale,
    mus_float_t volume, mus_float_t *amp_env, int amp_len,
    mus_any *out, mus_any *rev)
{
	mus_long_t i, beg, len;
	int chans, rev_chans, val;
	int dly_len[15] = {1433, 1601, 1867, 2053, 2251, 2399, 347, 113,
	37, 59, 53, 43, 37, 29, 19};
	mus_float_t srscale;
	mus_float_t sample_a, sample_b, sample_c, sample_d;
	mus_float_t inrev, outrev;
	mus_any *allpass1, *allpass2, *allpass3, *allpass4;
	mus_any *allpass5, *allpass6, *allpass7, *allpass8;
	mus_any *comb1, *comb2, *comb3, *comb4, *comb5, *comb6;
	mus_any *low, *low_a, *low_b, *low_c, *low_d, *env_a;

	chans = rev_chans = val = 0;
	srscale = mus_srate() / 25641.0;
	sample_a = sample_b = sample_c = sample_d = inrev = outrev = 0.0;
	beg = mus_seconds_to_samples(start);
	if (dur > 0.0)
		len = beg + mus_seconds_to_samples(dur);
	else
		len = beg + mus_seconds_to_samples(1.0) +
		    mus_sound_frames(mus_file_name(rev));
	chans = mus_channels(out);
	rev_chans = mus_channels(rev);
	env_a = mus_make_env(amp_env, amp_len / 2, output_scale, 0.0, 1.0,
	    dur, 0, NULL);
	for (i = 0; i < 14; i++) {
		int x;

		x = dly_len[i];
		val = (int)floor(srscale * x);
		if ((val % 2) == 0)
			val++;
		while (!prime_p(val))
			val += 2;
		dly_len[i] = val;
	}
	i = 0;
	comb1 = mus_make_comb(reverb_factor * 0.822, dly_len[i], NULL,
	    dly_len[i], MUS_INTERP_LINEAR);
	i++;
	comb2 = mus_make_comb(reverb_factor * 0.802, dly_len[i], NULL,
	    dly_len[i], MUS_INTERP_LINEAR);
	i++;
	comb3 = mus_make_comb(reverb_factor * 0.773, dly_len[i], NULL,
	    dly_len[i], MUS_INTERP_LINEAR);
	i++;
	comb4 = mus_make_comb(reverb_factor * 0.753, dly_len[i], NULL,
	    dly_len[i], MUS_INTERP_LINEAR);
	i++;
	comb5 = mus_make_comb(reverb_factor * 0.753, dly_len[i], NULL,
	    dly_len[i], MUS_INTERP_LINEAR);
	i++;
	comb6 = mus_make_comb(reverb_factor * 0.733, dly_len[i], NULL,
	    dly_len[i], MUS_INTERP_LINEAR);
	i++;
	low = mus_make_one_pole(lp_out_coeff, lp_coeff - 1.0);
	low_a = mus_make_one_pole(lp_out_coeff, lp_coeff - 1.0);
	low_b = mus_make_one_pole(lp_out_coeff, lp_coeff - 1.0);
	low_c = mus_make_one_pole(lp_out_coeff, lp_coeff - 1.0);
	low_d = mus_make_one_pole(lp_out_coeff, lp_coeff - 1.0);
	allpass1 = mus_make_all_pass(-0.7, 0.7, dly_len[i], NULL, dly_len[i],
	    MUS_INTERP_LINEAR);
	i++;
	allpass2 = mus_make_all_pass(-0.7, 0.7, dly_len[i], NULL, dly_len[i],
	    MUS_INTERP_LINEAR);
	i++;
	allpass3 = mus_make_all_pass(-0.7, 0.7, dly_len[i], NULL, dly_len[i],
	    MUS_INTERP_LINEAR);
	i += 2;
	allpass4 = mus_make_all_pass(-0.7, 0.7, dly_len[i], NULL, dly_len[i],
	    MUS_INTERP_LINEAR);
	i++;
	allpass5 = mus_make_all_pass(-0.7, 0.7, dly_len[i], NULL, dly_len[i],
	    MUS_INTERP_LINEAR);
	i++;
	allpass6 = mus_make_all_pass(-0.7, 0.7, dly_len[i], NULL, dly_len[i],
	    MUS_INTERP_LINEAR);
	i++;
	allpass7 = mus_make_all_pass(-0.7, 0.7, dly_len[i], NULL, dly_len[i],
	    MUS_INTERP_LINEAR);
	i++;
	allpass8 = mus_make_all_pass(-0.7, 0.7, dly_len[i], NULL, dly_len[i],
	    MUS_INTERP_LINEAR);
	for (i = beg; i < len; i++) {
		int j;
		mus_float_t ho;

		for (j = 0, ho = 0.0; j < rev_chans; j++)
			ho += mus_in_any(i, j, rev);

		inrev = volume * mus_env(env_a) * ho;
		outrev = mus_all_pass_unmodulated(allpass4,
		    mus_one_pole(low, mus_all_pass_unmodulated(allpass3,
		    mus_all_pass_unmodulated(allpass2,
		    mus_all_pass_unmodulated(allpass1,
		    mus_comb_unmodulated(comb1, inrev) +
		    mus_comb_unmodulated(comb2, inrev) +
		    mus_comb_unmodulated(comb3, inrev) +
		    mus_comb_unmodulated(comb4, inrev) +
		    mus_comb_unmodulated(comb5, inrev) +
		    mus_comb_unmodulated(comb6, inrev))))));
		sample_a = output_scale * mus_one_pole(low_a,
		    mus_all_pass_unmodulated(allpass5, outrev));
		sample_b = output_scale * mus_one_pole(low_b,
		    mus_all_pass_unmodulated(allpass6, outrev));
		sample_c = output_scale * mus_one_pole(low_c,
		    mus_all_pass_unmodulated(allpass7, outrev));
		sample_d = output_scale * mus_one_pole(low_d,
		    mus_all_pass_unmodulated(allpass8, outrev));

		if (chans == 2)
			mus_out_any(i, (sample_a + sample_d) / 2.0, 0, out);
		else
			mus_out_any(i, sample_a, 0, out);
		if (chans == 2 || chans == 4) {
			if (chans == 2)
				mus_out_any(i,
				    (sample_b + sample_c) / 2.0, 1, out);
			else
				mus_out_any(i, sample_b, 1, out);
		}
		if (chans == 4) {
			mus_out_any(i, sample_c, 2, out);
			mus_out_any(i, sample_d, 3, out);
		}
	}
	mus_free(allpass1);
	mus_free(allpass2);
	mus_free(allpass3);
	mus_free(allpass4);
	mus_free(allpass5);
	mus_free(allpass6);
	mus_free(allpass7);
	mus_free(allpass8);
	mus_free(comb1);
	mus_free(comb2);
	mus_free(comb3);
	mus_free(comb4);
	mus_free(comb5);
	mus_free(comb6);
	mus_free(env_a);
	mus_free(low);
	mus_free(low_a);
	mus_free(low_b);
	mus_free(low_c);
	mus_free(low_d);
	return (i);
}

#define FV_WARN	"input must be mono or in channels must equal out channels"

mus_long_t
ins_freeverb(mus_float_t start, mus_float_t dur, mus_float_t room_decay,
    mus_float_t damping, mus_float_t global, mus_float_t predelay,
    mus_float_t output_gain, mus_float_t scale_room_decay,
    mus_float_t offset_room_decay, mus_float_t scale_damping,
    mus_float_t stereo_spread, int *combtuning, int comb_len,
    int *allpasstuning, int all_len, mus_any *output_mixer,
    mus_any *out, mus_any *rev)
{
	mus_long_t i, beg, len;
	int out_chans, in_chans;
	mus_float_t srate_scale;
	mus_float_t local_gain, global_gain;
	mus_float_t tmp;
	mus_any *out_mix = NULL, *out_buf = NULL, *f_out = NULL, *f_in = NULL;

	out_chans = in_chans = 0;
	srate_scale = mus_srate() / 44100.0;
	local_gain = global_gain = 0.0;
	beg = mus_seconds_to_samples(start);

	if (dur > 0.0)
		len = beg + mus_seconds_to_samples(dur);
	else
		len = beg + mus_seconds_to_samples(1.0) +
		    mus_sound_frames(mus_file_name(rev));

	out_chans = mus_channels(out);
	in_chans = mus_channels(rev);
	if (in_chans > 1 && in_chans != out_chans)
		INS_MISC_ERROR(S_freeverb, FV_WARN);

	out_buf = mus_make_empty_frame(out_chans);
	f_out = mus_make_empty_frame(out_chans);
	f_in = mus_make_empty_frame(in_chans);
	local_gain = (1.0 - global)*(1.0 - 1.0 / out_chans) + 1.0 / out_chans;

	tmp = (out_chans * out_chans - out_chans);
	if (tmp < 1.0)
		tmp = 1.0;

	global_gain = (out_chans - local_gain * out_chans) / tmp;

	if (mus_mixer_p(output_mixer))
		out_mix = output_mixer;
	else {
		out_mix = mus_make_empty_mixer(out_chans);
		for (i = 0; i < out_chans; i++) {
			int j;

			for (j = 0; j < out_chans; j++)
				mus_mixer_set(out_mix, i, j,
				    ((output_gain * ((i == j) ?
				    local_gain : global_gain)) / out_chans));
		}
	}
	{
		int j, k, size;
		mus_float_t room_decay_val;
		mus_any **predelays, ***allpasses, ***combs;

		predelays = malloc(in_chans * sizeof(mus_any *));
		if (predelays == NULL)
			INS_NO_MEMORY_ERROR();
		allpasses = malloc(out_chans * sizeof(mus_any **));
		if (allpasses == NULL)
			INS_NO_MEMORY_ERROR();
		combs = malloc(out_chans * sizeof(mus_any **));
		if (combs == NULL)
			INS_NO_MEMORY_ERROR();
		room_decay_val = room_decay * scale_room_decay +
		    offset_room_decay;

		for (i = 0; i < out_chans; i++) {
			allpasses[i] = malloc(all_len * sizeof(mus_any *));
			if (allpasses[i] == NULL)
				INS_NO_MEMORY_ERROR();
			combs[i] = malloc(comb_len * sizeof(mus_any *));
			if (combs[i] == NULL)
				INS_NO_MEMORY_ERROR();
		}
		/* predelays */
		for (i = 0; i < in_chans; i++) {
			size = (int)floor(mus_srate() * predelay);
			predelays[i] = mus_make_delay(size, NULL, size,
			    MUS_INTERP_LINEAR);
		}
		for (i = 0; i < out_chans; i++) {
			/* comb filters */
			for (j = 0; j < comb_len; j++) {
				mus_float_t dmp;

				dmp = scale_damping * damping;
				size = (int)floor(srate_scale * combtuning[j]);
				if ((i % 2) != 0)
					size += (int)floor(srate_scale *
					    stereo_spread);
				combs[i][j] = mus_make_fcomb(room_decay_val,
				    size, 1.0 - dmp, dmp);
			}
			/* allpass filters */
			for (j = 0; j < all_len; j++) {
				size = (int)floor(srate_scale *
				    allpasstuning[j]);
				if ((i % 2) != 0)
					size += (int)floor(srate_scale *
					    stereo_spread);
				allpasses[i][j] = mus_make_all_pass(0.5, -1.0,
				    size, NULL, size, MUS_INTERP_LINEAR);
			}
		}
		/* run loop */
		for (i = beg; i < len; i++) {
			f_in = mus_file_to_frame(rev, i, f_in);
			if (in_chans > 1) {
				for (j = 0; j < out_chans; j++) {
					mus_frame_set(f_in, j,
					    mus_delay_unmodulated(predelays[j],
					    mus_frame_ref(f_in, j)));
					mus_frame_set(f_out, j, 0.0);
					for (k = 0; k < comb_len; k++)
						mus_frame_set(f_out, j,
						    mus_frame_ref(f_out, j) +
						    mus_fcomb(combs[j][k],
						    mus_frame_ref(f_in, j),
						    0.0));
				}
			} else {
				mus_frame_set(f_in, 0,
				    mus_delay_unmodulated(predelays[0],
				    mus_frame_ref(f_in, 0)));
				for (j = 0; j < out_chans; j++) {
					mus_frame_set(f_out, j, 0.0);
					for (k = 0; k < comb_len; k++)
						mus_frame_set(f_out, j,
						    mus_frame_ref(f_out, j) +
						    mus_fcomb(combs[j][k],
						    mus_frame_ref(f_in, 0),
						    0.0));
				}
			}
			for (j = 0; j < out_chans; j++)
				for (k = 0; k < all_len; k++)
					mus_frame_set(f_out, j,
					    mus_all_pass_unmodulated(
					    allpasses[j][k],
					    mus_frame_ref(f_out, j)));
			mus_frame_to_file(out, i, mus_frame_to_frame(out_mix,
			    f_out, out_buf));
		}		/* run loop */
		for (i = 0; i < in_chans; i++)
			mus_free(predelays[i]);
		free(predelays);
		for (i = 0; i < out_chans; i++) {
			for (j = 0; j < comb_len; j++)
				mus_free(combs[i][j]);
			free(combs[i]);
			for (j = 0; j < all_len; j++)
				mus_free(allpasses[i][j]);
			free(allpasses[i]);
		}
		free(combs);
		free(allpasses);
	}			/* block */
	if (!output_mixer)
		mus_free(out_mix);
	mus_free(out_buf);
	mus_free(f_out);
	mus_free(f_in);
	return (i);
}

#define h_fm_violin_args "\
keyword arguments with default values:\n\
 :startime                      0.0\n\
 :duration                      1.0\n\
 :frequency                     440.0\n\
 :amplitude                     0.5\n\
 :fm-index                      1.0\n\
 :amp-env                       " INS_LIST_BEG " 0 0 25 1 75 1 100 0 " INS_LIST_END "\n\
 :periodic-vibrato-rate         5.0\n\
 :periodic-vibrato-amplitude    0.0025\n\
 :random-vibrato-rate           16.0\n\
 :random-vibrato-amplitude      0.005\n\
 :noise-freq         		1000.0\n\
 :noise-amount       		0.0\n\
 :ind-noise-freq     		10.0\n\
 :ind-noise-amount   		0.0\n\
 :amp-noise-freq     		20.0\n\
 :amp-noise-amount   		0.0\n\
 :gliss-env          		" INS_LIST_BEG " 0 0 100 0 " INS_LIST_END "\n\
 :glissando-amount   		0.0\n\
 :fm1-env      			" INS_LIST_BEG " 0 1 25 0.4 75 0.6 100 0 " INS_LIST_END "\n\
 :fm2-env      			" INS_LIST_BEG " 0 1 25 0.4 75 0.6 100 0 " INS_LIST_END "\n\
 :fm3-env      			" INS_LIST_BEG " 0 1 25 0.4 75 0.6 100 0 " INS_LIST_END "\n\
 :fm1-rat      			1.0\n\
 :fm2-rat      			3.0\n\
 :fm3-rat      			4.0\n\
 :fm1-index    			" INS_FALSE "\n\
 :fm2-index    			" INS_FALSE "\n\
 :fm3-index    			" INS_FALSE "\n\
 :base                          1.0\n\
 :degree                        0.0\n\
 :distance                      1.0\n\
 :reverb-amount                 0.01\n\
 :index-type                    " INS_SYMBOL_PREFIX "violin (" INS_SYMBOL_PREFIX "cello or " INS_SYMBOL_PREFIX "violin)\n\
 :no-waveshaping                " INS_FALSE

#if HAVE_RUBY
#define H_fm_violin S_fm_violin "(*args)\n" h_fm_violin_args "\n\
require 'ws'\n\
require 'sndins'\n\
with_sound(:reverb, :jc_reverb, :reverb_data, [:volume, 0.8]) do\n\
  fm_violin(0, 1, 440, 0.2, :fm_index, 1.3)\n\
end"
#elif HAVE_FORTH
#define H_fm_violin 	S_fm_violin " ( args -- )\n" h_fm_violin_args "\n\
require clm\n\
dl-load sndins Init_sndins\n\
0 1 440 0.2 :fm-index 1.3 <'> fm-violin\n\
  :reverb <'> jc-reverb :reverb-data #( :volume 0.8 ) with-sound"
#else				/* HAVE_SCHEME */
#define H_fm_violin	"(" S_fm_violin " . args)\n" h_fm_violin_args "\n\
(load \"ws.scm\")\n\
(load-extension \"libsndins\" \"Init_sndins\")\n\
(with-sound (:reverb jc-reverb :reverb-data '(:volume 0.8))\n\
  (fm-violin 0 1 440 0.2 :fm-index 1.3))"
#endif

#if HAVE_FORTH
static void
#else
static XEN
#endif
c_fm_violin(XEN args)
{
#define V_LAST_KEY 33
#define XEN_V_INS_VIOLIN 1
#define XEN_V_INS_CELLO  0
	mus_long_t result;
	int i, vals, lst_len, mode, amp_len, gls_len;
	int fm1_len, fm2_len, fm3_len;
	bool index_type, no_waveshaping;
	bool amp_del, gls_del, fm1_del, fm2_del, fm3_del;
	mus_float_t start, dur, freq, amp, fm_index;
	mus_float_t tamp_env[8] = {0.0, 0.0, 25.0, 1.0, 75.0, 1.0, 100.0, 0.0};
	mus_float_t periodic_vibrato_rate, periodic_vibrato_amp;
	mus_float_t random_vibrato_rate, random_vibrato_amp;
	mus_float_t noise_freq, noise_amount;
	mus_float_t ind_noise_freq, ind_noise_amount;
	mus_float_t amp_noise_freq, amp_noise_amount;
	mus_float_t tgliss_env[4] = {0.0, 0.0, 100.0, 0.0};
	mus_float_t gliss_amount;
	mus_float_t tfm_env[8] = {0.0, 1.0, 25.0, 0.4, 75.0, 0.6, 100.0, 0.0};
	mus_float_t fm1_rat, fm2_rat, fm3_rat;
	mus_float_t fm1_index, fm2_index, fm3_index, base;
	mus_float_t degree, distance, reverb_amount;
	mus_float_t *amp_env = NULL, *gliss_env = NULL;
	mus_float_t *fm1_env = NULL, *fm2_env = NULL, *fm3_env = NULL;
	mus_any *out = NULL, *rev = NULL;
	XEN kargs[V_LAST_KEY * 2], keys[V_LAST_KEY];
	int orig_arg[V_LAST_KEY] = {0};

	lst_len = XEN_LIST_LENGTH(args);
	mode = MUS_INTERP_LINEAR;
	amp_len = fm1_len = fm2_len = fm3_len = 8;
	gls_len = 4;
	index_type = (bool)XEN_V_INS_VIOLIN;
	no_waveshaping = amp_del = gls_del = false;
	fm1_del = fm2_del = fm3_del = false;
	start = 0.0;
	dur = 1.0;
	freq = 440.0;
	amp = 0.5;
	fm_index = 1.0;
	periodic_vibrato_rate = 5.0;
	periodic_vibrato_amp = 0.0025;
	random_vibrato_rate = 16.0;
	random_vibrato_amp = 0.005;
	noise_freq = 1000.0;
	noise_amount = 0.0;
	ind_noise_freq = 10.0;
	ind_noise_amount = 0.0;
	amp_noise_freq = 20.0;
	amp_noise_amount = 0.0;
	gliss_amount = 0.0;
	fm1_rat = 1.0;
	fm2_rat = 3.0;
	fm3_rat = 4.0;
	fm1_index = fm2_index = fm3_index = 0.0;
	base = 1.0;
	degree = 0.0;
	distance = 1.0;
	reverb_amount = 0.01;
	i = 0;
	keys[i++] = allkeys[C_startime];
	keys[i++] = allkeys[C_duration];
	keys[i++] = allkeys[C_frequency];
	keys[i++] = allkeys[C_amplitude];
	keys[i++] = allkeys[C_fm_index];
	keys[i++] = allkeys[C_amp_env];
	keys[i++] = allkeys[C_periodic_vibrato_rate];
	keys[i++] = allkeys[C_periodic_vibrato_amplitude];
	keys[i++] = allkeys[C_random_vibrato_rate];
	keys[i++] = allkeys[C_random_vibrato_amplitude];
	keys[i++] = allkeys[C_noise_freq];
	keys[i++] = allkeys[C_noise_amount];
	keys[i++] = allkeys[C_ind_noise_freq];
	keys[i++] = allkeys[C_ind_noise_amount];
	keys[i++] = allkeys[C_amp_noise_freq];
	keys[i++] = allkeys[C_amp_noise_amount];
	keys[i++] = allkeys[C_gliss_env];
	keys[i++] = allkeys[C_glissando_amount];
	keys[i++] = allkeys[C_fm1_env];
	keys[i++] = allkeys[C_fm2_env];
	keys[i++] = allkeys[C_fm3_env];
	keys[i++] = allkeys[C_fm1_rat];
	keys[i++] = allkeys[C_fm2_rat];
	keys[i++] = allkeys[C_fm3_rat];
	keys[i++] = allkeys[C_fm1_index];
	keys[i++] = allkeys[C_fm2_index];
	keys[i++] = allkeys[C_fm3_index];
	keys[i++] = allkeys[C_base];
	keys[i++] = allkeys[C_degree];
	keys[i++] = allkeys[C_distance];
	keys[i++] = allkeys[C_reverb_amount];
	keys[i++] = allkeys[C_index_type];
	keys[i++] = allkeys[C_no_waveshaping];

	for (i = 0; i < V_LAST_KEY * 2; i++)
		kargs[i] = XEN_UNDEFINED;
	for (i = 0; i < lst_len; i++)
		kargs[i] = XEN_LIST_REF(args, i);

	vals = mus_optkey_unscramble(S_fm_violin, V_LAST_KEY, keys, kargs,
	    orig_arg);
	if (vals > 0) {
		i = 0;
		start = mus_optkey_to_float(keys[i], S_fm_violin,
		    orig_arg[i], start);
		i++;
		dur = mus_optkey_to_float(keys[i], S_fm_violin,
		    orig_arg[i], dur);
		i++;
		freq = mus_optkey_to_float(keys[i], S_fm_violin,
		    orig_arg[i], freq);
		i++;
		amp = mus_optkey_to_float(keys[i], S_fm_violin,
		    orig_arg[i], amp);
		i++;
		fm_index = mus_optkey_to_float(keys[i], S_fm_violin,
		    orig_arg[i], fm_index);
		i++;
		if (!(XEN_KEYWORD_P(keys[i]))) {
			XEN_ASSERT_TYPE(XEN_LIST_P(keys[i]), keys[i],
			    orig_arg[i], S_fm_violin, "a list");
			amp_env = xen_list2array(keys[i]);
			amp_len = XEN_LIST_LENGTH(keys[i]);
		}
		i++;
		periodic_vibrato_rate = mus_optkey_to_float(keys[i],
		    S_fm_violin, orig_arg[i], periodic_vibrato_rate);
		i++;
		periodic_vibrato_amp = mus_optkey_to_float(keys[i],
		    S_fm_violin, orig_arg[i], periodic_vibrato_amp);
		i++;
		random_vibrato_rate = mus_optkey_to_float(keys[i],
		    S_fm_violin, orig_arg[i], random_vibrato_rate);
		i++;
		random_vibrato_amp = mus_optkey_to_float(keys[i],
		    S_fm_violin, orig_arg[i], random_vibrato_amp);
		i++;
		noise_freq = mus_optkey_to_float(keys[i], S_fm_violin,
		    orig_arg[i], noise_freq);
		i++;
		noise_amount = mus_optkey_to_float(keys[i], S_fm_violin,
		    orig_arg[i], noise_amount);
		i++;
		ind_noise_freq = mus_optkey_to_float(keys[i], S_fm_violin,
		    orig_arg[i], ind_noise_freq);
		i++;
		ind_noise_amount = mus_optkey_to_float(keys[i], S_fm_violin,
		    orig_arg[i], ind_noise_amount);
		i++;
		amp_noise_freq = mus_optkey_to_float(keys[i], S_fm_violin,
		    orig_arg[i], amp_noise_freq);
		i++;
		amp_noise_amount = mus_optkey_to_float(keys[i], S_fm_violin,
		    orig_arg[i], amp_noise_amount);
		i++;
		if (!(XEN_KEYWORD_P(keys[i]))) {
			XEN_ASSERT_TYPE(XEN_LIST_P(keys[i]), keys[i],
			    orig_arg[i], S_fm_violin, "a list");
			gliss_env = xen_list2array(keys[i]);
			gls_len = XEN_LIST_LENGTH(keys[i]);
		}
		i++;
		gliss_amount = mus_optkey_to_float(keys[i], S_fm_violin,
		    orig_arg[i], gliss_amount);
		i++;
		if (!(XEN_KEYWORD_P(keys[i]))) {
			XEN_ASSERT_TYPE(XEN_LIST_P(keys[i]), keys[i],
			    orig_arg[i], S_fm_violin, "a list");
			fm1_env = xen_list2array(keys[i]);
			fm1_len = XEN_LIST_LENGTH(keys[i]);
		}
		i++;
		if (!(XEN_KEYWORD_P(keys[i]))) {
			XEN_ASSERT_TYPE(XEN_LIST_P(keys[i]), keys[i],
			    orig_arg[i], S_fm_violin, "a list");
			fm2_env = xen_list2array(keys[i]);
			fm2_len = XEN_LIST_LENGTH(keys[i]);
		}
		i++;
		if (!(XEN_KEYWORD_P(keys[i]))) {
			XEN_ASSERT_TYPE(XEN_LIST_P(keys[i]), keys[i],
			    orig_arg[i], S_fm_violin, "a list");
			fm3_env = xen_list2array(keys[i]);
			fm3_len = XEN_LIST_LENGTH(keys[i]);
		}
		i++;
		fm1_rat = mus_optkey_to_float(keys[i], S_fm_violin,
		    orig_arg[i], fm1_rat);
		i++;
		fm2_rat = mus_optkey_to_float(keys[i], S_fm_violin,
		    orig_arg[i], fm2_rat);
		i++;
		fm3_rat = mus_optkey_to_float(keys[i], S_fm_violin,
		    orig_arg[i], fm3_rat);
		i++;
		if (!(XEN_KEYWORD_P(keys[i])))
			if (XEN_NUMBER_P(keys[i]))
				fm1_index = XEN_TO_C_DOUBLE(keys[i]);
		i++;
		if (!(XEN_KEYWORD_P(keys[i])))
			if (XEN_NUMBER_P(keys[i]))
				fm2_index = XEN_TO_C_DOUBLE(keys[i]);
		i++;
		if (!(XEN_KEYWORD_P(keys[i])))
			if (XEN_NUMBER_P(keys[i]))
				fm3_index = XEN_TO_C_DOUBLE(keys[i]);
		i++;
		base = mus_optkey_to_float(keys[i], S_fm_violin,
		    orig_arg[i], base);
		i++;
		degree = mus_optkey_to_float(keys[i], S_fm_violin,
		    orig_arg[i], degree);
		i++;
		distance = mus_optkey_to_float(keys[i], S_fm_violin,
		    orig_arg[i], distance);
		i++;
		reverb_amount = mus_optkey_to_float(keys[i], S_fm_violin,
		    orig_arg[i], reverb_amount);
		i++;
		if (!(XEN_KEYWORD_P(keys[i]))) {
			/* cello == 0, violin == 1 */
			if (XEN_SYMBOL_P(keys[i])) {
				if (XEN_EQ_P(keys[i],
				    C_STRING_TO_XEN_SYMBOL("cello")))
					index_type = XEN_V_INS_CELLO;
			} else if (XEN_INTEGER_P(keys[i]))
				index_type = XEN_TO_C_INT(keys[i]);
		}
		i++;
		if (!(XEN_KEYWORD_P(keys[i]))) {
			XEN_ASSERT_TYPE(XEN_BOOLEAN_P(keys[i]), keys[i],
			    orig_arg[i], S_fm_violin, "a bool");
			no_waveshaping = XEN_TO_C_BOOLEAN(keys[i]);
		}
	}
	if (amp_env == NULL) {
		amp_env = array2array(tamp_env, 8);
		amp_del = true;
	}
	if (gliss_env == NULL) {
		gliss_env = array2array(tgliss_env, 4);
		gls_del = true;
	}
	if (fm1_env == NULL) {
		fm1_env = array2array(tfm_env, 8);
		fm1_del = true;
	}
	if (fm2_env == NULL) {
		fm2_env = array2array(tfm_env, 8);
		fm2_del = true;
	}
	if (fm3_env == NULL) {
		fm3_env = array2array(tfm_env, 8);
		fm3_del = true;
	}
	if (!mus_output_p(out = get_global_mus_gen(INS_OUTPUT)))
		INS_MISC_ERROR(S_fm_violin, "needs an output generator");
	rev = get_global_mus_gen(INS_REVERB);
	mode = get_global_int(INS_LOCSIG_TYPE, MUS_INTERP_LINEAR);

	result = ins_fm_violin(start, dur, freq, amp, fm_index,
	    amp_env, amp_len, periodic_vibrato_rate, periodic_vibrato_amp,
	    random_vibrato_rate, random_vibrato_amp, noise_freq, noise_amount,
	    ind_noise_freq, ind_noise_amount, amp_noise_freq, amp_noise_amount,
	    gliss_env, gls_len, gliss_amount, fm1_env, fm1_len,
	    fm2_env, fm2_len, fm3_env, fm3_len, fm1_rat, fm2_rat, fm3_rat,
	    fm1_index, fm2_index, fm3_index, base, degree, distance,
	    reverb_amount, index_type, no_waveshaping, out, rev, mode);

	if (amp_del)
		free(amp_env);
	if (gls_del)
		free(gliss_env);
	if (fm1_del)
		free(fm1_env);
	if (fm2_del)
		free(fm2_env);
	if (fm3_del)
		free(fm3_env);
#if !HAVE_FORTH
	return (C_TO_XEN_INT(result));
#endif
}

#define INS_REV_MSG(caller, in_chans, out_chans)			\
	ins_message("%s on %d in and %d out channels\n", 		\
	    caller, in_chans, out_chans)
#define INS_REV_DUR(rev)						\
	(mus_samples_to_seconds(mus_length(rev)) +			\
	    get_global_float(INS_DECAY_TIME, 1.0))

#define h_jc_reverb_args "\
keyword arguments with default values:\n\
 :volume     1.0\n\
 :delay1     0.013\n\
 :delay2     0.011\n\
 :delay3     0.015\n\
 :delay4     0.017\n\
 :low-pass   " INS_FALSE "\n\
 :doubled    " INS_FALSE "\n\
 :amp-env    " INS_FALSE

#if HAVE_RUBY
#define H_jc_reverb	S_jc_reverb "(*args)\n" h_jc_reverb_args "\n\
require 'ws'\n\
require 'sndins'\n\
with_sound(:reverb, :jc_reverb, :reverb_data, [:volume, 0.8]) do\n\
  fm_violin(0, 1, 440, 0.2, :fm_index, 1.3)\n\
end"
#elif HAVE_FORTH
#define H_jc_reverb 	S_jc_reverb " ( args -- )\n" h_jc_reverb_args "\n\
require clm\n\
dl-load sndins Init_sndins\n\
0 1 440 0.2 :fm-index 1.3 <'> fm-violin\n\
  :reverb <'> jc-reverb :reverb-data #( :volume 0.8 ) with-sound"
#else				/* HAVE_SCHEME */
#define H_jc_reverb 	"(" S_jc_reverb " . args)\n" h_jc_reverb_args "\n\
(load-from-path \"ws.scm\")\n\
(load-extension \"libsndins\" \"Init_sndins\")\n\
(with-sound (:reverb jc-reverb :reverb-data '(:volume 0.8))\n\
  (fm-violin 0 1 440 0.2 :fm-index 1.3))"
#endif

#if HAVE_FORTH
static void
#else
static XEN
#endif
c_jc_reverb(XEN args)
{
#define JC_LAST_KEY 8
	mus_long_t result;
	int i, vals, lst_len, amp_len;
	bool low_pass, doubled;
	mus_float_t volume, delay1, delay2, delay3, delay4;
	mus_float_t *amp_env = NULL;
	mus_any *out = NULL, *rev = NULL;
	int orig_arg[JC_LAST_KEY] = {0};
	XEN kargs[JC_LAST_KEY * 2], keys[JC_LAST_KEY];

	amp_len = 0;
	lst_len = XEN_LIST_LENGTH(args);
	low_pass = doubled = false;
	volume = 1.0;
	delay1 = 0.013;
	delay2 = 0.011;
	delay3 = 0.015;
	delay4 = 0.017;
	i = 0;
	keys[i++] = allkeys[C_low_pass];
	keys[i++] = allkeys[C_volume];
	keys[i++] = allkeys[C_doubled];
	keys[i++] = allkeys[C_delay1];
	keys[i++] = allkeys[C_delay2];
	keys[i++] = allkeys[C_delay3];
	keys[i++] = allkeys[C_delay4];
	keys[i++] = allkeys[C_amp_env];

	for (i = 0; i < JC_LAST_KEY * 2; i++)
		kargs[i] = XEN_UNDEFINED;
	for (i = 0; i < lst_len; i++)
		kargs[i] = XEN_LIST_REF(args, i);

	vals = mus_optkey_unscramble(S_jc_reverb, JC_LAST_KEY, keys, kargs,
	    orig_arg);
	if (vals > 0) {
		i = 0;
		if (!(XEN_KEYWORD_P(keys[i]))) {
			XEN_ASSERT_TYPE(XEN_BOOLEAN_P(keys[i]), keys[i],
			    orig_arg[i], S_jc_reverb, "a bool");
			low_pass = XEN_TO_C_BOOLEAN(keys[i]);
		}
		i++;
		volume = mus_optkey_to_float(keys[i], S_jc_reverb,
		    orig_arg[i], volume);
		i++;
		if (!(XEN_KEYWORD_P(keys[i]))) {
			XEN_ASSERT_TYPE(XEN_BOOLEAN_P(keys[i]), keys[i],
			    orig_arg[i], S_jc_reverb, "a bool");
			doubled = XEN_TO_C_BOOLEAN(keys[i]);
		}
		i++;
		delay1 = mus_optkey_to_float(keys[i], S_jc_reverb,
		    orig_arg[i], delay1);
		i++;
		delay2 = mus_optkey_to_float(keys[i], S_jc_reverb,
		    orig_arg[i], delay2);
		i++;
		delay3 = mus_optkey_to_float(keys[i], S_jc_reverb,
		    orig_arg[i], delay3);
		i++;
		delay4 = mus_optkey_to_float(keys[i], S_jc_reverb,
		    orig_arg[i], delay4);
		i++;
		if (!(XEN_KEYWORD_P(keys[i]))) {
			XEN_ASSERT_TYPE(XEN_LIST_P(keys[i]), keys[i],
			    orig_arg[i], S_jc_reverb, "a list");
			amp_env = xen_list2array(keys[i]);
			amp_len = XEN_LIST_LENGTH(keys[i]);
		}
	}
	if (!mus_output_p(out = get_global_mus_gen(INS_OUTPUT)))
		INS_MISC_ERROR(S_jc_reverb, "needs an output generator");
	if (!mus_input_p(rev = get_global_mus_gen(INS_REVERB)))
		INS_MISC_ERROR(S_jc_reverb, "needs an input generator");
	if (get_global_boolean(INS_VERBOSE, false))
		INS_REV_MSG(S_jc_reverb, mus_channels(rev), mus_channels(out));

	result = ins_jc_reverb(0.0, INS_REV_DUR(rev), volume, low_pass,
	    doubled, delay1, delay2, delay3, delay4, amp_env, amp_len,
	    out, rev);
#if !HAVE_FORTH
	return (C_TO_XEN_INT(result));
#endif
}

#define h_nrev_args "\
keyword arguments with default values:\n\
 :reverb-factor   1.09\n\
 :lp-coeff        0.7\n\
 :lp-out-coeff    0.85\n\
 :output-scale    1.0\n\
 :amp-env         " INS_LIST_BEG " 0 1 1 1 " INS_LIST_END "\n\
 :volume          1.0"

#if HAVE_RUBY
#define H_nrev		S_nrev "(*args)\n" h_nrev_args "\n\
require 'ws'\n\
require 'sndins'\n\
with_sound(:reverb, :nrev, :reverb_data, [:lp_coeff, 0.6]) do\n\
  fm_violin(0, 1, 440, 0.7, :fm_index, 1.3)\n\
end"
#elif HAVE_FORTH
#define H_nrev 		S_nrev " ( args -- )\n" h_nrev_args "\n\
require clm\n\
dl-load sndins Init_sndins\n\
0 1 440 0.7 :fm-index 1.3 <'> fm-violin\n\
  :reverb <'> nrev :reverb-data #( :lp-coeff 0.6 ) with-sound"
#else				/* HAVE_SCHEME */
#define H_nrev 		"(" S_nrev " . args)\n" h_nrev_args "\n\
(load \"ws.scm\")\n\
(load-extension \"libsndins\" \"Init_sndins\")\n\
(with-sound (:reverb nrev :reverb-data '(:lp-coeff 0.6))\n\
  (fm-violin 0 1 440 0.7 :fm-index 1.3))"
#endif

#if HAVE_FORTH
static void
#else
static XEN
#endif
c_nrev(XEN args)
{
#define N_LAST_KEY 6
	mus_long_t result;
	int i, vals, lst_len, amp_len;
	bool amp_del;
	mus_float_t lp_coeff, lp_out_coeff;
	mus_float_t output_scale, reverb_factor, volume;
	mus_float_t tamp_env[4] = {0, 1, 1, 1};
	mus_float_t *amp_env = NULL;
	mus_any *out = NULL, *rev = NULL;
	int orig_arg[N_LAST_KEY] = {0};
	XEN kargs[N_LAST_KEY * 2], keys[N_LAST_KEY];

	lst_len = XEN_LIST_LENGTH(args);
	amp_len = 4;
	amp_del = false;
	lp_coeff = 0.7;
	lp_out_coeff = 0.85;
	output_scale = 1.0;
	reverb_factor = 1.09;
	volume = 1.0;
	i = 0;
	keys[i++] = allkeys[C_reverb_factor];
	keys[i++] = allkeys[C_lp_coeff];
	keys[i++] = allkeys[C_lp_out_coeff];
	keys[i++] = allkeys[C_output_scale];
	keys[i++] = allkeys[C_amp_env];
	keys[i++] = allkeys[C_volume];

	for (i = 0; i < N_LAST_KEY * 2; i++)
		kargs[i] = XEN_UNDEFINED;
	for (i = 0; i < lst_len; i++)
		kargs[i] = XEN_LIST_REF(args, i);

	vals = mus_optkey_unscramble(S_nrev, N_LAST_KEY, keys, kargs, orig_arg);
	if (vals > 0) {
		i = 0;
		reverb_factor = mus_optkey_to_float(keys[i], S_nrev,
		    orig_arg[i], reverb_factor);
		i++;
		lp_coeff = mus_optkey_to_float(keys[i], S_nrev,
		    orig_arg[i], lp_coeff);
		i++;
		lp_out_coeff = mus_optkey_to_float(keys[i], S_nrev,
		    orig_arg[i], lp_out_coeff);
		i++;
		output_scale = mus_optkey_to_float(keys[i], S_nrev,
		    orig_arg[i], output_scale);
		i++;
		if (!(XEN_KEYWORD_P(keys[i]))) {
			XEN_ASSERT_TYPE(XEN_LIST_P(keys[i]), keys[i],
			    orig_arg[i], S_nrev, "a list");
			amp_env = xen_list2array(keys[i]);
			amp_len = XEN_LIST_LENGTH(keys[i]);
		}
		i++;
		volume = mus_optkey_to_float(keys[i], S_nrev,
		    orig_arg[i], volume);
	}
	if (amp_env == NULL) {
		amp_env = array2array(tamp_env, 4);
		amp_del = true;
	}
	if (!mus_output_p(out = get_global_mus_gen(INS_OUTPUT)))
		INS_MISC_ERROR(S_nrev, "needs an output generator");
	if (!mus_input_p(rev = get_global_mus_gen(INS_REVERB)))
		INS_MISC_ERROR(S_nrev, "needs an input generator");
	if (get_global_boolean(INS_VERBOSE, false))
		INS_REV_MSG(S_nrev, mus_channels(rev), mus_channels(out));

	result = ins_nrev(0.0, INS_REV_DUR(rev), reverb_factor, lp_coeff,
	    lp_out_coeff, output_scale, volume, amp_env, amp_len, out, rev);

	if (amp_del)
		free(amp_env);
#if !HAVE_FORTH
	return (C_TO_XEN_INT(result));
#endif
}

#define h_freeverb_args "\
keyword arguments with default values:\n\
 :room-decay          0.5\n\
 :damping             0.5\n\
 :global              0.3\n\
 :predelay            0.03\n\
 :output-gain         1.0\n\
 :output-mixer        " INS_FALSE "\n\
 :scale-room-decay    0.28\n\
 :offset-room-decay   0.7\n\
 :combtuning          " INS_LIST_BEG " 1116 1188 1277 1356 1422 1491 1557 1617 " INS_LIST_END "\n\
 :allpasstuning       " INS_LIST_BEG " 556 441 341 225 " INS_LIST_END "\n\
 :scale-damping       0.4\n\
 :stereo-spread       23.0"

#if HAVE_RUBY
#define H_freeverb	S_freeverb "(*args)\n" h_freeverb_args "\n\
require 'ws'\n\
require 'sndins'\n\
with_sound(:reverb, :freeverb, :reverb_data, [:room_decay, 0.8]) do\n\
  fm_violin(0, 1, 440, 0.7)\n\
end"
#elif HAVE_FORTH
#define H_freeverb 	S_freeverb " ( args -- )\n" h_freeverb_args "\n\
require clm\n\
dl-load sndins Init_sndins\n\
0 1 440 0.7 <'> fm-violin\n\
  :reverb <'> freeverb\n\
  :reverb-data #( :room-decay 0.8 ) with-sound"
#else				/* HAVE_SCHEME */
#define H_freeverb 	"(" S_freeverb " . args)\n" h_freeverb_args "\n\
(load \"ws.scm\")\n\
(load-extension \"libsndins\" \"Init_sndins\")\n\
(with-sound (:reverb freeverb :reverb-data '(:room-decay 0.8))\n\
  (fm-violin 0 1 440 0.7))"
#endif

#if HAVE_FORTH
static void
#else
static XEN
#endif
c_freeverb(XEN args)
{
#define F_LAST_KEY 12
	mus_long_t result;
	int i, vals, lst_len, numcombs, numallpasses;
	int tcombtun[8] = {1116, 1188, 1277, 1356, 1422, 1491, 1557, 1617};
	int tallpass[4] = {556, 441, 341, 225};
	int *combtuning = NULL, *allpasstuning = NULL;
	bool comb_del, allpass_del;
	mus_float_t room_decay, global, damping;
	mus_float_t predelay, output_gain, scale_room_decay;
	mus_float_t offset_room_decay, scale_damping, stereo_spread;
	mus_any *output_mixer = NULL, *out = NULL, *rev = NULL;
	int orig_arg[F_LAST_KEY] = {0};
	XEN kargs[F_LAST_KEY * 2], keys[F_LAST_KEY];

	lst_len = XEN_LIST_LENGTH(args);
	numcombs = 8;
	numallpasses = 4;
	comb_del = allpass_del = false;
	room_decay = 0.5;
	global = 0.3;
	damping = 0.5;
	predelay = 0.03;
	output_gain = 1.0;
	scale_room_decay = 0.28;
	offset_room_decay = 0.7;
	scale_damping = 0.4;
	stereo_spread = 23.0;
	i = 0;
	keys[i++] = allkeys[C_room_decay];
	keys[i++] = allkeys[C_damping];
	keys[i++] = allkeys[C_global];
	keys[i++] = allkeys[C_predelay];
	keys[i++] = allkeys[C_output_gain];
	keys[i++] = allkeys[C_output_mixer];
	keys[i++] = allkeys[C_scale_room_decay];
	keys[i++] = allkeys[C_offset_room_decay];
	keys[i++] = allkeys[C_combtuning];
	keys[i++] = allkeys[C_allpasstuning];
	keys[i++] = allkeys[C_scale_damping];
	keys[i++] = allkeys[C_stereo_spread];

	for (i = 0; i < F_LAST_KEY * 2; i++)
		kargs[i] = XEN_UNDEFINED;
	for (i = 0; i < lst_len; i++)
		kargs[i] = XEN_LIST_REF(args, i);

	vals = mus_optkey_unscramble(S_freeverb, F_LAST_KEY, keys, kargs,
	    orig_arg);
	if (vals > 0) {
		i = 0;
		room_decay = mus_optkey_to_float(keys[i], S_freeverb,
		    orig_arg[i], room_decay);
		i++;
		damping = mus_optkey_to_float(keys[i], S_freeverb,
		    orig_arg[i], damping);
		i++;
		global = mus_optkey_to_float(keys[i], S_freeverb,
		    orig_arg[i], global);
		i++;
		predelay = mus_optkey_to_float(keys[i], S_freeverb,
		    orig_arg[i], predelay);
		i++;
		output_gain = mus_optkey_to_float(keys[i], S_freeverb,
		    orig_arg[i], output_gain);
		i++;
		output_mixer = mus_optkey_to_mus_any(keys[i], S_freeverb,
		    orig_arg[i], output_mixer);
		i++;
		scale_room_decay = mus_optkey_to_float(keys[i], S_freeverb,
		    orig_arg[i], scale_room_decay);
		i++;
		offset_room_decay = mus_optkey_to_float(keys[i], S_freeverb,
		    orig_arg[i], offset_room_decay);
		i++;
		if (!(XEN_KEYWORD_P(keys[i]))) {
			XEN_ASSERT_TYPE(XEN_LIST_P(keys[i]), keys[i],
			    orig_arg[i], S_freeverb, "a list");
			combtuning = xen_list2iarray(keys[i]);
			numcombs = XEN_LIST_LENGTH(keys[i]);
		}
		i++;
		if (!(XEN_KEYWORD_P(keys[i]))) {
			XEN_ASSERT_TYPE(XEN_LIST_P(keys[i]), keys[i],
			    orig_arg[i], S_freeverb, "a list");
			allpasstuning = xen_list2iarray(keys[i]);
			numallpasses = XEN_LIST_LENGTH(keys[i]);
		}
		i++;
		scale_damping = mus_optkey_to_float(keys[i], S_freeverb,
		    orig_arg[i], scale_damping);
		i++;
		stereo_spread = mus_optkey_to_float(keys[i], S_freeverb,
		    orig_arg[i], stereo_spread);
	}
	if (!combtuning) {
		combtuning = int_array2array(tcombtun, 8);
		comb_del = true;
	}
	if (!allpasstuning) {
		allpasstuning = int_array2array(tallpass, 4);
		allpass_del = true;
	}
	if (!mus_output_p(out = get_global_mus_gen(INS_OUTPUT)))
		INS_MISC_ERROR(S_freeverb, "needs an output generator");
	if (!mus_input_p(rev = get_global_mus_gen(INS_REVERB)))
		INS_MISC_ERROR(S_freeverb, "needs an input generator");
	if (get_global_boolean(INS_VERBOSE, false))
		INS_REV_MSG(S_freeverb, mus_channels(rev), mus_channels(out));

	result = ins_freeverb(0.0, INS_REV_DUR(rev), room_decay, damping,
	    global, predelay, output_gain, scale_room_decay, offset_room_decay,
	    scale_damping, stereo_spread, combtuning, numcombs, allpasstuning,
	    numallpasses, output_mixer, out, rev);

	if (comb_del)
		free(combtuning);
	if (allpass_del)
		free(allpasstuning);
#if !HAVE_FORTH
	return (C_TO_XEN_INT(result));
#endif
}

#ifdef XEN_ARGIFY_1
XEN_VARGIFY(x_make_fcomb, c_make_fcomb)
XEN_ARGIFY_2(x_fcomb, c_fcomb)
XEN_NARGIFY_1(x_fcomb_p, c_fcomb_p)
XEN_VARGIFY(x_fm_violin, c_fm_violin)
XEN_VARGIFY(x_jc_reverb, c_jc_reverb)
XEN_VARGIFY(x_nrev, c_nrev)
XEN_VARGIFY(x_freeverb, c_freeverb)
#else				/* !XEN_ARGIFY_1 */
#define x_make_fcomb		c_make_fcomb
#define x_fcomb			c_fcomb
#define x_fcomb_p 		c_fcomb_p
#define x_fm_violin 		c_fm_violin
#define x_jc_reverb		c_jc_reverb
#define x_nrev			c_nrev
#define x_freeverb		c_freeverb
#endif				/* XEN_ARGIFY_1 */

void
Init_sndins(void)
{
	init_keywords();
	XEN_DEFINE_PROCEDURE(S_make_fcomb, x_make_fcomb, 0, 0, 1, H_make_fcomb);
	XEN_DEFINE_PROCEDURE(S_fcomb, x_fcomb, 1, 1, 0, H_fcomb);
	XEN_DEFINE_PROCEDURE(S_fcomb_p, x_fcomb_p, 1, 0, 0, H_fcomb_p);
#if HAVE_FORTH
	fth_define_void_procedure(S_fm_violin, x_fm_violin, 0, 0, 1,
	    H_fm_violin);
	fth_define_void_procedure(S_jc_reverb, x_jc_reverb, 0, 0, 1,
	    H_jc_reverb);
	fth_define_void_procedure(S_nrev, x_nrev, 0, 0, 1, H_nrev);
	fth_define_void_procedure(S_freeverb, x_freeverb, 0, 0, 1, H_freeverb);
#else
	XEN_DEFINE_PROCEDURE(S_fm_violin, x_fm_violin, 0, 0, 1, H_fm_violin);
	XEN_DEFINE_PROCEDURE(S_jc_reverb, x_jc_reverb, 0, 0, 1, H_jc_reverb);
	XEN_DEFINE_PROCEDURE(S_nrev, x_nrev, 0, 0, 1, H_nrev);
	XEN_DEFINE_PROCEDURE(S_freeverb, x_freeverb, 0, 0, 1, H_freeverb);
#endif
	XEN_PROVIDE("sndins");
}

/*
 * sndins.c ends here
 */
