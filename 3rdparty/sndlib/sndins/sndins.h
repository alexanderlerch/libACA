/* sndins.h -- Sndins for Snd/CLM
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
 */

#ifndef _SNDINS_H_
#define _SNDINS_H_

#undef __BEGIN_DECLS
#undef __END_DECLS
#ifdef __cplusplus
#define __BEGIN_DECLS	extern "C" {
#define __END_DECLS	}
#else
#define __BEGIN_DECLS
#define __END_DECLS
#endif

__BEGIN_DECLS

mus_any        *mus_make_fcomb(mus_float_t scaler, int size,
		    mus_float_t a0, mus_float_t a1);
int		mus_fcomb_p(mus_any *ptr);
mus_float_t	mus_fcomb(mus_any *ptr, mus_float_t input, mus_float_t ignored);

mus_long_t	ins_fm_violin(mus_float_t start, mus_float_t dur,
		    mus_float_t freq, mus_float_t amp, mus_float_t fm_index,
		    mus_float_t *amp_env, int amp_len,
		    mus_float_t periodic_vibrato_rate,
		    mus_float_t periodic_vibrato_amp,
		    mus_float_t random_vibrato_rate, 
		    mus_float_t random_vibrato_amp, mus_float_t noise_freq,
		    mus_float_t noise_amount, mus_float_t ind_noise_freq, 
		    mus_float_t ind_noise_amount, mus_float_t amp_noise_freq, 
		    mus_float_t amp_noise_amount, mus_float_t *gliss_env, 
		    int gliss_len, mus_float_t gliss_amount, 
		    mus_float_t *fm1_env, int fm1_len, 
		    mus_float_t *fm2_env, int fm2_len, 
		    mus_float_t *fm3_env, int fm3_len,
		    mus_float_t fm1_rat, mus_float_t fm2_rat, 
		    mus_float_t fm3_rat, mus_float_t fm1_index, 
		    mus_float_t fm2_index, mus_float_t fm3_index, 
		    mus_float_t base, mus_float_t degree, 
		    mus_float_t distance, mus_float_t reverb_amount, 
		    bool index_type, bool no_waveshaping, mus_any *out, 
		    mus_any *rev, mus_interp_t mode);
mus_long_t	ins_jc_reverb(mus_float_t start, mus_float_t dur, 
		    mus_float_t volume, bool low_pass, bool doubled, 
		    mus_float_t delay1, mus_float_t delay2, 
		    mus_float_t delay3, mus_float_t delay4, 
		    mus_float_t *amp_env, int amp_len, 
		    mus_any *out, mus_any *rev);
mus_long_t	ins_nrev(mus_float_t start, mus_float_t dur, 
		    mus_float_t reverb_factor, mus_float_t lp_coeff, 
		    mus_float_t lp_out_coeff, mus_float_t output_scale, 
		    mus_float_t volume, mus_float_t *amp_env, int amp_len, 
		    mus_any *out, mus_any *rev);
mus_long_t	ins_freeverb(mus_float_t start, mus_float_t dur, 
		    mus_float_t room_decay, mus_float_t damping, 
		    mus_float_t global, mus_float_t predelay, 
		    mus_float_t output_gain, mus_float_t scale_room_decay, 
		    mus_float_t offset_room_decay, mus_float_t scale_damping, 
		    mus_float_t stereo_spread, int *combtuning, int comb_len, 
		    int *allpasstuning, int all_len, mus_any *output_mixer, 
		    mus_any *out, mus_any *rev);

void		Init_sndins(void);

__END_DECLS

#endif				/* _SNDINS_H_ */

/*
 * sndins.h ends here
 */
