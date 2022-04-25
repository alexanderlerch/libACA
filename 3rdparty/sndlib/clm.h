#ifndef CLM_H
#define CLM_H

#define MUS_VERSION 5
#define MUS_REVISION 14
#define MUS_DATE "19-Apr-13"

/* isn't mus_env_interp backwards? */

#include "sndlib.h"

#ifndef _MSC_VER
  #include <sys/param.h>
#endif
#if HAVE_COMPLEX_TRIG
  #include <complex.h>
#endif

#if(!defined(M_PI))
  #define M_PI 3.14159265358979323846264338327
  #define M_PI_2 (M_PI / 2.0)
#endif

#define MUS_DEFAULT_SAMPLING_RATE 44100.0
#define MUS_DEFAULT_FILE_BUFFER_SIZE 8192
#define MUS_DEFAULT_ARRAY_PRINT_LENGTH 8

typedef enum {MUS_NOT_SPECIAL, MUS_SIMPLE_FILTER, MUS_FULL_FILTER, MUS_OUTPUT, MUS_INPUT, MUS_DELAY_LINE} mus_clm_extended_t;

typedef struct mus_any_class mus_any_class;
typedef struct {
  struct mus_any_class *core;
} mus_any;


typedef enum {MUS_INTERP_NONE, MUS_INTERP_LINEAR, MUS_INTERP_SINUSOIDAL, MUS_INTERP_ALL_PASS, 
	      MUS_INTERP_LAGRANGE, MUS_INTERP_BEZIER, MUS_INTERP_HERMITE} mus_interp_t;

typedef enum {MUS_RECTANGULAR_WINDOW, MUS_HANN_WINDOW, MUS_WELCH_WINDOW, MUS_PARZEN_WINDOW, MUS_BARTLETT_WINDOW,
	      MUS_HAMMING_WINDOW, MUS_BLACKMAN2_WINDOW, MUS_BLACKMAN3_WINDOW, MUS_BLACKMAN4_WINDOW,
	      MUS_EXPONENTIAL_WINDOW, MUS_RIEMANN_WINDOW, MUS_KAISER_WINDOW, MUS_CAUCHY_WINDOW, MUS_POISSON_WINDOW,
	      MUS_GAUSSIAN_WINDOW, MUS_TUKEY_WINDOW, MUS_DOLPH_CHEBYSHEV_WINDOW, MUS_HANN_POISSON_WINDOW, 
	      MUS_CONNES_WINDOW, MUS_SAMARAKI_WINDOW, MUS_ULTRASPHERICAL_WINDOW, 
	      MUS_BARTLETT_HANN_WINDOW, MUS_BOHMAN_WINDOW, MUS_FLAT_TOP_WINDOW,
	      MUS_BLACKMAN5_WINDOW, MUS_BLACKMAN6_WINDOW, MUS_BLACKMAN7_WINDOW, MUS_BLACKMAN8_WINDOW, MUS_BLACKMAN9_WINDOW, MUS_BLACKMAN10_WINDOW,
	      MUS_RV2_WINDOW, MUS_RV3_WINDOW, MUS_RV4_WINDOW, MUS_MLT_SINE_WINDOW, MUS_PAPOULIS_WINDOW, MUS_DPSS_WINDOW, MUS_SINC_WINDOW,
	      MUS_NUM_FFT_WINDOWS} mus_fft_window_t;

typedef enum {MUS_SPECTRUM_IN_DB, MUS_SPECTRUM_NORMALIZED, MUS_SPECTRUM_RAW} mus_spectrum_t;
typedef enum {MUS_CHEBYSHEV_EITHER_KIND, MUS_CHEBYSHEV_FIRST_KIND, MUS_CHEBYSHEV_SECOND_KIND} mus_polynomial_t;

#define MUS_MAX_CLM_SINC_WIDTH 65536
#define MUS_MAX_CLM_SRC 65536.0

#ifdef __cplusplus
extern "C" {
#endif

MUS_EXPORT void mus_initialize(void);

MUS_EXPORT int mus_make_generator_type(void);

MUS_EXPORT mus_any_class *mus_generator_class(mus_any *ptr);
MUS_EXPORT mus_any_class *mus_make_generator(int type, char *name, 
					     int (*release)(mus_any *ptr), 
					     char *(*describe)(mus_any *ptr), 
					     bool (*equalp)(mus_any *gen1, mus_any *gen2));
MUS_EXPORT void mus_generator_set_release(mus_any_class *p, int (*release)(mus_any *ptr));
MUS_EXPORT void mus_generator_set_describe(mus_any_class *p, char *(*describe)(mus_any *ptr));
MUS_EXPORT void mus_generator_set_equalp(mus_any_class *p, bool (*equalp)(mus_any *gen1, mus_any *gen2));
MUS_EXPORT void mus_generator_set_data(mus_any_class *p, mus_float_t *(*data)(mus_any *ptr));
MUS_EXPORT void mus_generator_set_set_data(mus_any_class *p, mus_float_t *(*set_data)(mus_any *ptr, mus_float_t *new_data));
MUS_EXPORT void mus_generator_set_length(mus_any_class *p, mus_long_t (*length)(mus_any *ptr));
MUS_EXPORT void mus_generator_set_set_length(mus_any_class *p, mus_long_t (*set_length)(mus_any *ptr, mus_long_t new_length));
MUS_EXPORT void mus_generator_set_frequency(mus_any_class *p, mus_float_t (*frequency)(mus_any *ptr));
MUS_EXPORT void mus_generator_set_set_frequency(mus_any_class *p, mus_float_t (*set_frequency)(mus_any *ptr, mus_float_t new_freq));
MUS_EXPORT void mus_generator_set_phase(mus_any_class *p, mus_float_t (*phase)(mus_any *ptr));
MUS_EXPORT void mus_generator_set_set_phase(mus_any_class *p, mus_float_t (*set_phase)(mus_any *ptr, mus_float_t new_phase));
MUS_EXPORT void mus_generator_set_scaler(mus_any_class *p, mus_float_t (*scaler)(mus_any *ptr));
MUS_EXPORT void mus_generator_set_set_scaler(mus_any_class *p, mus_float_t (*set_scaler)(mus_any *ptr, mus_float_t val));
MUS_EXPORT void mus_generator_set_increment(mus_any_class *p, mus_float_t (*increment)(mus_any *ptr));
MUS_EXPORT void mus_generator_set_set_increment(mus_any_class *p, mus_float_t (*set_increment)(mus_any *ptr, mus_float_t val));
MUS_EXPORT void mus_generator_set_run(mus_any_class *p, mus_float_t (*run)(mus_any *gen, mus_float_t arg1, mus_float_t arg2));
MUS_EXPORT void mus_generator_set_closure(mus_any_class *p, void *(*closure)(mus_any *gen));
MUS_EXPORT void mus_generator_set_channels(mus_any_class *p, int (*channels)(mus_any *ptr));
MUS_EXPORT void mus_generator_set_offset(mus_any_class *p, mus_float_t (*offset)(mus_any *ptr));
MUS_EXPORT void mus_generator_set_set_offset(mus_any_class *p, mus_float_t (*set_offset)(mus_any *ptr, mus_float_t val));
MUS_EXPORT void mus_generator_set_width(mus_any_class *p, mus_float_t (*width)(mus_any *ptr));
MUS_EXPORT void mus_generator_set_set_width(mus_any_class *p, mus_float_t (*set_width)(mus_any *ptr, mus_float_t val));
MUS_EXPORT void mus_generator_set_xcoeff(mus_any_class *p, mus_float_t (*xcoeff)(mus_any *ptr, int index));
MUS_EXPORT void mus_generator_set_set_xcoeff(mus_any_class *p, mus_float_t (*set_xcoeff)(mus_any *ptr, int index, mus_float_t val));
MUS_EXPORT void mus_generator_set_hop(mus_any_class *p, mus_long_t (*hop)(mus_any *ptr));
MUS_EXPORT void mus_generator_set_set_hop(mus_any_class *p, mus_long_t (*set_hop)(mus_any *ptr, mus_long_t new_length));
MUS_EXPORT void mus_generator_set_ramp(mus_any_class *p, mus_long_t (*ramp)(mus_any *ptr));
MUS_EXPORT void mus_generator_set_set_ramp(mus_any_class *p, mus_long_t (*set_ramp)(mus_any *ptr, mus_long_t new_length));
MUS_EXPORT void mus_generator_set_read_sample(mus_any_class *p, mus_float_t (*read_sample)(mus_any *ptr, mus_long_t samp, int chan));
MUS_EXPORT void mus_generator_set_write_sample(mus_any_class *p, mus_float_t (*write_sample)(mus_any *ptr, mus_long_t samp, int chan, mus_float_t data));
MUS_EXPORT void mus_generator_set_file_name(mus_any_class *p, char *(*file_name)(mus_any *ptr));
MUS_EXPORT void mus_generator_set_end(mus_any_class *p, int (*end)(mus_any *ptr));
MUS_EXPORT void mus_generator_set_location(mus_any_class *p, mus_long_t (*location)(mus_any *ptr));
MUS_EXPORT void mus_generator_set_set_location(mus_any_class *p, mus_long_t (*set_location)(mus_any *ptr, mus_long_t loc));
MUS_EXPORT void mus_generator_set_channel(mus_any_class *p, int (*channel)(mus_any *ptr));
MUS_EXPORT void mus_generator_set_ycoeff(mus_any_class *p, mus_float_t (*ycoeff)(mus_any *ptr, int index));
MUS_EXPORT void mus_generator_set_set_ycoeff(mus_any_class *p, mus_float_t (*set_ycoeff)(mus_any *ptr, int index, mus_float_t val));
MUS_EXPORT void mus_generator_set_xcoeffs(mus_any_class *p, mus_float_t *(*xcoeffs)(mus_any *ptr));
MUS_EXPORT void mus_generator_set_ycoeffs(mus_any_class *p, mus_float_t *(*ycoeffs)(mus_any *ptr));
MUS_EXPORT void mus_generator_set_reset(mus_any_class *p, void (*reset)(mus_any *ptr));
MUS_EXPORT void mus_generator_set_set_closure(mus_any_class *p, void *(*set_closure)(mus_any *gen, void *e));
MUS_EXPORT void mus_generator_set_extended_type(mus_any_class *p, mus_clm_extended_t extended_type);

MUS_EXPORT void mus_generator_set_feeder(mus_any *g, mus_float_t (*feed)(void *arg, int direction));

MUS_EXPORT mus_float_t mus_radians_to_hz(mus_float_t radians);
MUS_EXPORT mus_float_t mus_hz_to_radians(mus_float_t hz);
MUS_EXPORT mus_float_t mus_degrees_to_radians(mus_float_t degrees);
MUS_EXPORT mus_float_t mus_radians_to_degrees(mus_float_t radians);
MUS_EXPORT mus_float_t mus_db_to_linear(mus_float_t x);
MUS_EXPORT mus_float_t mus_linear_to_db(mus_float_t x);

MUS_EXPORT mus_float_t mus_srate(void);
MUS_EXPORT mus_float_t mus_set_srate(mus_float_t val);
MUS_EXPORT mus_long_t mus_seconds_to_samples(mus_float_t secs);
MUS_EXPORT mus_float_t mus_samples_to_seconds(mus_long_t samps);
MUS_EXPORT int mus_array_print_length(void);
MUS_EXPORT int mus_set_array_print_length(int val);
MUS_EXPORT mus_float_t mus_float_equal_fudge_factor(void);
MUS_EXPORT mus_float_t mus_set_float_equal_fudge_factor(mus_float_t val);

MUS_EXPORT mus_float_t mus_ring_modulate(mus_float_t s1, mus_float_t s2);
MUS_EXPORT mus_float_t mus_amplitude_modulate(mus_float_t s1, mus_float_t s2, mus_float_t s3);
MUS_EXPORT mus_float_t mus_contrast_enhancement(mus_float_t sig, mus_float_t index);
MUS_EXPORT mus_float_t mus_dot_product(mus_float_t *data1, mus_float_t *data2, mus_long_t size);
#if HAVE_COMPLEX_TRIG
  MUS_EXPORT complex double mus_edot_product(complex double freq, complex double *data, mus_long_t size);
#endif

MUS_EXPORT void mus_clear_array(mus_float_t *arr, mus_long_t size);
MUS_EXPORT bool mus_arrays_are_equal(mus_float_t *arr1, mus_float_t *arr2, mus_float_t fudge, mus_long_t len);
MUS_EXPORT mus_float_t mus_polynomial(mus_float_t *coeffs, mus_float_t x, int ncoeffs);
MUS_EXPORT void mus_multiply_arrays(mus_float_t *data, mus_float_t *window, mus_long_t len);
MUS_EXPORT void mus_rectangular_to_polar(mus_float_t *rl, mus_float_t *im, mus_long_t size);
MUS_EXPORT void mus_rectangular_to_magnitudes(mus_float_t *rl, mus_float_t *im, mus_long_t size);
MUS_EXPORT void mus_polar_to_rectangular(mus_float_t *rl, mus_float_t *im, mus_long_t size);
MUS_EXPORT mus_float_t mus_array_interp(mus_float_t *wave, mus_float_t phase, mus_long_t size);
MUS_EXPORT double mus_bessi0(mus_float_t x);
MUS_EXPORT mus_float_t mus_interpolate(mus_interp_t type, mus_float_t x, mus_float_t *table, mus_long_t table_size, mus_float_t y);
MUS_EXPORT bool mus_interp_type_p(int val);
MUS_EXPORT bool mus_fft_window_p(int val);

MUS_EXPORT int mus_data_format_zero(int format);
MUS_EXPORT bool mus_run_exists(mus_any *gen);


/* -------- generic functions -------- */

MUS_EXPORT int mus_type(mus_any *ptr);
MUS_EXPORT int mus_free(mus_any *ptr);
MUS_EXPORT char *mus_describe(mus_any *gen);
MUS_EXPORT bool mus_equalp(mus_any *g1, mus_any *g2);
MUS_EXPORT mus_float_t mus_phase(mus_any *gen);
MUS_EXPORT mus_float_t mus_set_phase(mus_any *gen, mus_float_t val);
MUS_EXPORT mus_float_t mus_set_frequency(mus_any *gen, mus_float_t val);
MUS_EXPORT mus_float_t mus_frequency(mus_any *gen);
MUS_EXPORT mus_float_t mus_run(mus_any *gen, mus_float_t arg1, mus_float_t arg2);
MUS_EXPORT mus_long_t mus_length(mus_any *gen);
MUS_EXPORT mus_long_t mus_set_length(mus_any *gen, mus_long_t len);
MUS_EXPORT mus_long_t mus_order(mus_any *gen);
MUS_EXPORT mus_float_t *mus_data(mus_any *gen);
MUS_EXPORT mus_float_t *mus_set_data(mus_any *gen, mus_float_t *data);
MUS_EXPORT const char *mus_name(mus_any *ptr);
MUS_EXPORT const char *mus_set_name(mus_any *ptr, const char *new_name);
MUS_EXPORT mus_float_t mus_scaler(mus_any *gen);
MUS_EXPORT mus_float_t mus_set_scaler(mus_any *gen, mus_float_t val);
MUS_EXPORT mus_float_t mus_offset(mus_any *gen);
MUS_EXPORT mus_float_t mus_set_offset(mus_any *gen, mus_float_t val);
MUS_EXPORT mus_float_t mus_width(mus_any *gen);
MUS_EXPORT mus_float_t mus_set_width(mus_any *gen, mus_float_t val);
MUS_EXPORT char *mus_file_name(mus_any *ptr);
MUS_EXPORT void mus_reset(mus_any *ptr);
MUS_EXPORT mus_float_t *mus_xcoeffs(mus_any *ptr);
MUS_EXPORT mus_float_t *mus_ycoeffs(mus_any *ptr);
MUS_EXPORT mus_float_t mus_xcoeff(mus_any *ptr, int index);
MUS_EXPORT mus_float_t mus_set_xcoeff(mus_any *ptr, int index, mus_float_t val);
MUS_EXPORT mus_float_t mus_ycoeff(mus_any *ptr, int index);
MUS_EXPORT mus_float_t mus_set_ycoeff(mus_any *ptr, int index, mus_float_t val);
MUS_EXPORT mus_float_t mus_increment(mus_any *rd);
MUS_EXPORT mus_float_t mus_set_increment(mus_any *rd, mus_float_t dir);
MUS_EXPORT mus_long_t mus_location(mus_any *rd);
MUS_EXPORT mus_long_t mus_set_location(mus_any *rd, mus_long_t loc);
MUS_EXPORT int mus_channel(mus_any *rd);
MUS_EXPORT int mus_channels(mus_any *ptr);
MUS_EXPORT int mus_position(mus_any *ptr); /* only C, envs (snd-env.c), shares slot with mus_channels */
MUS_EXPORT int mus_interp_type(mus_any *ptr);
MUS_EXPORT mus_long_t mus_ramp(mus_any *ptr);
MUS_EXPORT mus_long_t mus_set_ramp(mus_any *ptr, mus_long_t val);
MUS_EXPORT mus_long_t mus_hop(mus_any *ptr);
MUS_EXPORT mus_long_t mus_set_hop(mus_any *ptr, mus_long_t val);
MUS_EXPORT mus_float_t mus_feedforward(mus_any *gen);
MUS_EXPORT mus_float_t mus_set_feedforward(mus_any *gen, mus_float_t val);
MUS_EXPORT mus_float_t mus_feedback(mus_any *rd);
MUS_EXPORT mus_float_t mus_set_feedback(mus_any *rd, mus_float_t dir);


/* -------- generators -------- */

MUS_EXPORT mus_float_t mus_oscil(mus_any *o, mus_float_t fm, mus_float_t pm);
MUS_EXPORT mus_float_t mus_oscil_unmodulated(mus_any *ptr);
MUS_EXPORT mus_float_t mus_oscil_fm(mus_any *ptr, mus_float_t fm);
MUS_EXPORT mus_float_t mus_oscil_pm(mus_any *ptr, mus_float_t pm);
MUS_EXPORT bool mus_oscil_p(mus_any *ptr);
MUS_EXPORT mus_any *mus_make_oscil(mus_float_t freq, mus_float_t phase);

MUS_EXPORT bool mus_oscil_bank_p(mus_any *ptr);
MUS_EXPORT mus_float_t mus_oscil_bank(mus_any *ptr);
MUS_EXPORT mus_any *mus_make_oscil_bank(int size, mus_float_t *freqs, mus_float_t *phases, mus_float_t *amps);

MUS_EXPORT mus_any *mus_make_ncos(mus_float_t freq, int n);
MUS_EXPORT mus_float_t mus_ncos(mus_any *ptr, mus_float_t fm);
MUS_EXPORT bool mus_ncos_p(mus_any *ptr);

MUS_EXPORT mus_any *mus_make_nsin(mus_float_t freq, int n);
MUS_EXPORT mus_float_t mus_nsin(mus_any *ptr, mus_float_t fm);
MUS_EXPORT bool mus_nsin_p(mus_any *ptr);

MUS_EXPORT mus_any *mus_make_nrxysin(mus_float_t frequency, mus_float_t y_over_x, int n, mus_float_t r);
MUS_EXPORT mus_float_t mus_nrxysin(mus_any *ptr, mus_float_t fm);
MUS_EXPORT bool mus_nrxysin_p(mus_any *ptr);

MUS_EXPORT mus_any *mus_make_nrxycos(mus_float_t frequency, mus_float_t y_over_x, int n, mus_float_t r);
MUS_EXPORT mus_float_t mus_nrxycos(mus_any *ptr, mus_float_t fm);
MUS_EXPORT bool mus_nrxycos_p(mus_any *ptr);

MUS_EXPORT mus_any *mus_make_rxykcos(mus_float_t freq, mus_float_t phase, mus_float_t r, mus_float_t ratio);
MUS_EXPORT mus_float_t mus_rxykcos(mus_any *ptr, mus_float_t fm);
MUS_EXPORT bool mus_rxykcos_p(mus_any *ptr);

MUS_EXPORT mus_any *mus_make_rxyksin(mus_float_t freq, mus_float_t phase, mus_float_t r, mus_float_t ratio);
MUS_EXPORT mus_float_t mus_rxyksin(mus_any *ptr, mus_float_t fm);
MUS_EXPORT bool mus_rxyksin_p(mus_any *ptr);

MUS_EXPORT mus_float_t mus_delay(mus_any *gen, mus_float_t input, mus_float_t pm);
MUS_EXPORT mus_float_t mus_delay_unmodulated(mus_any *ptr, mus_float_t input);
MUS_EXPORT mus_float_t mus_tap(mus_any *gen, mus_float_t loc);
MUS_EXPORT mus_float_t mus_tap_unmodulated(mus_any *gen);
MUS_EXPORT mus_any *mus_make_delay(int size, mus_float_t *line, int line_size, mus_interp_t type);
MUS_EXPORT bool mus_delay_p(mus_any *ptr);
MUS_EXPORT bool mus_tap_p(mus_any *ptr);
MUS_EXPORT bool mus_delay_line_p(mus_any *gen); /* added 2-Mar-03 for tap error checks */
MUS_EXPORT mus_float_t mus_delay_tick(mus_any *ptr, mus_float_t input);
MUS_EXPORT mus_float_t mus_delay_unmodulated_noz(mus_any *ptr, mus_float_t input);

MUS_EXPORT mus_float_t mus_comb(mus_any *gen, mus_float_t input, mus_float_t pm);
MUS_EXPORT mus_float_t mus_comb_unmodulated(mus_any *gen, mus_float_t input);
MUS_EXPORT mus_any *mus_make_comb(mus_float_t scaler, int size, mus_float_t *line, int line_size, mus_interp_t type);
MUS_EXPORT bool mus_comb_p(mus_any *ptr);
MUS_EXPORT mus_float_t mus_comb_unmodulated_noz(mus_any *ptr, mus_float_t input);

MUS_EXPORT mus_float_t mus_comb_bank(mus_any *bank, mus_float_t inval);
MUS_EXPORT mus_any *mus_make_comb_bank(int size, mus_any **combs);
MUS_EXPORT bool mus_comb_bank_p(mus_any *g);

MUS_EXPORT mus_float_t mus_notch(mus_any *gen, mus_float_t input, mus_float_t pm);
MUS_EXPORT mus_float_t mus_notch_unmodulated(mus_any *gen, mus_float_t input);
MUS_EXPORT mus_any *mus_make_notch(mus_float_t scaler, int size, mus_float_t *line, int line_size, mus_interp_t type);
MUS_EXPORT bool mus_notch_p(mus_any *ptr);
MUS_EXPORT mus_float_t mus_notch_unmodulated_noz(mus_any *ptr, mus_float_t input);

MUS_EXPORT mus_float_t mus_all_pass(mus_any *gen, mus_float_t input, mus_float_t pm);
MUS_EXPORT mus_float_t mus_all_pass_unmodulated(mus_any *gen, mus_float_t input);
MUS_EXPORT mus_any *mus_make_all_pass(mus_float_t backward, mus_float_t forward, int size, mus_float_t *line, int line_size, mus_interp_t type);
MUS_EXPORT bool mus_all_pass_p(mus_any *ptr);
MUS_EXPORT mus_float_t mus_all_pass_unmodulated_noz(mus_any *ptr, mus_float_t input);

MUS_EXPORT mus_float_t mus_all_pass_bank(mus_any *bank, mus_float_t inval);
MUS_EXPORT mus_any *mus_make_all_pass_bank(int size, mus_any **combs);
MUS_EXPORT bool mus_all_pass_bank_p(mus_any *g);

MUS_EXPORT mus_any *mus_make_moving_average(int size, mus_float_t *line);
MUS_EXPORT bool mus_moving_average_p(mus_any *ptr);
MUS_EXPORT mus_float_t mus_moving_average(mus_any *ptr, mus_float_t input);

MUS_EXPORT mus_any *mus_make_moving_max(int size, mus_float_t *line);
MUS_EXPORT bool mus_moving_max_p(mus_any *ptr);
MUS_EXPORT mus_float_t mus_moving_max(mus_any *ptr, mus_float_t input);

MUS_EXPORT mus_float_t mus_table_lookup(mus_any *gen, mus_float_t fm);
MUS_EXPORT mus_float_t mus_table_lookup_unmodulated(mus_any *gen);
MUS_EXPORT mus_any *mus_make_table_lookup(mus_float_t freq, mus_float_t phase, mus_float_t *wave, mus_long_t wave_size, mus_interp_t type);
MUS_EXPORT bool mus_table_lookup_p(mus_any *ptr);
MUS_EXPORT mus_float_t *mus_partials_to_wave(mus_float_t *partial_data, int partials, mus_float_t *table, mus_long_t table_size, bool normalize);
MUS_EXPORT mus_float_t *mus_phase_partials_to_wave(mus_float_t *partial_data, int partials, mus_float_t *table, mus_long_t table_size, bool normalize);

MUS_EXPORT mus_float_t mus_sawtooth_wave(mus_any *gen, mus_float_t fm);
MUS_EXPORT mus_any *mus_make_sawtooth_wave(mus_float_t freq, mus_float_t amp, mus_float_t phase);
MUS_EXPORT bool mus_sawtooth_wave_p(mus_any *gen);

MUS_EXPORT mus_float_t mus_square_wave(mus_any *gen, mus_float_t fm);
MUS_EXPORT mus_any *mus_make_square_wave(mus_float_t freq, mus_float_t amp, mus_float_t phase);
MUS_EXPORT bool mus_square_wave_p(mus_any *gen);

MUS_EXPORT mus_float_t mus_triangle_wave(mus_any *gen, mus_float_t fm);
MUS_EXPORT mus_any *mus_make_triangle_wave(mus_float_t freq, mus_float_t amp, mus_float_t phase);
MUS_EXPORT bool mus_triangle_wave_p(mus_any *gen);
MUS_EXPORT mus_float_t mus_triangle_wave_unmodulated(mus_any *ptr);
 
MUS_EXPORT mus_float_t mus_pulse_train(mus_any *gen, mus_float_t fm);
MUS_EXPORT mus_any *mus_make_pulse_train(mus_float_t freq, mus_float_t amp, mus_float_t phase);
MUS_EXPORT bool mus_pulse_train_p(mus_any *gen);
MUS_EXPORT mus_float_t mus_pulse_train_unmodulated(mus_any *ptr);

MUS_EXPORT void mus_set_rand_seed(unsigned long seed);
MUS_EXPORT unsigned long mus_rand_seed(void);
MUS_EXPORT mus_float_t mus_random(mus_float_t amp);
MUS_EXPORT mus_float_t mus_frandom(mus_float_t amp);
MUS_EXPORT int mus_irandom(int amp);

MUS_EXPORT mus_float_t mus_rand(mus_any *gen, mus_float_t fm);
MUS_EXPORT mus_any *mus_make_rand(mus_float_t freq, mus_float_t base);
MUS_EXPORT bool mus_rand_p(mus_any *ptr);
MUS_EXPORT mus_any *mus_make_rand_with_distribution(mus_float_t freq, mus_float_t base, mus_float_t *distribution, int distribution_size);

MUS_EXPORT mus_float_t mus_rand_interp(mus_any *gen, mus_float_t fm);
MUS_EXPORT mus_any *mus_make_rand_interp(mus_float_t freq, mus_float_t base);
MUS_EXPORT bool mus_rand_interp_p(mus_any *ptr);
MUS_EXPORT mus_any *mus_make_rand_interp_with_distribution(mus_float_t freq, mus_float_t base, mus_float_t *distribution, int distribution_size);
MUS_EXPORT mus_float_t mus_rand_interp_unmodulated(mus_any *ptr);
MUS_EXPORT mus_float_t mus_rand_unmodulated(mus_any *ptr);

MUS_EXPORT mus_float_t mus_asymmetric_fm(mus_any *gen, mus_float_t index, mus_float_t fm);
MUS_EXPORT mus_float_t mus_asymmetric_fm_unmodulated(mus_any *gen, mus_float_t index);
MUS_EXPORT mus_any *mus_make_asymmetric_fm(mus_float_t freq, mus_float_t phase, mus_float_t r, mus_float_t ratio);
MUS_EXPORT bool mus_asymmetric_fm_p(mus_any *ptr);

MUS_EXPORT mus_float_t mus_one_zero(mus_any *gen, mus_float_t input);
MUS_EXPORT mus_any *mus_make_one_zero(mus_float_t a0, mus_float_t a1);
MUS_EXPORT bool mus_one_zero_p(mus_any *gen);

MUS_EXPORT mus_float_t mus_one_pole(mus_any *gen, mus_float_t input);
MUS_EXPORT mus_any *mus_make_one_pole(mus_float_t a0, mus_float_t b1);
MUS_EXPORT bool mus_one_pole_p(mus_any *gen);

MUS_EXPORT mus_float_t mus_two_zero(mus_any *gen, mus_float_t input);
MUS_EXPORT mus_any *mus_make_two_zero(mus_float_t a0, mus_float_t a1, mus_float_t a2);
MUS_EXPORT bool mus_two_zero_p(mus_any *gen);
MUS_EXPORT mus_any *mus_make_two_zero_from_frequency_and_radius(mus_float_t frequency, mus_float_t radius);

MUS_EXPORT mus_float_t mus_two_pole(mus_any *gen, mus_float_t input);
MUS_EXPORT mus_any *mus_make_two_pole(mus_float_t a0, mus_float_t b1, mus_float_t b2);
MUS_EXPORT bool mus_two_pole_p(mus_any *gen);
MUS_EXPORT mus_any *mus_make_two_pole_from_frequency_and_radius(mus_float_t frequency, mus_float_t radius);

MUS_EXPORT mus_float_t mus_one_pole_all_pass(mus_any *f, mus_float_t input);
MUS_EXPORT mus_any *mus_make_one_pole_all_pass(int size, mus_float_t coeff);
MUS_EXPORT bool mus_one_pole_all_pass_p(mus_any *ptr);

MUS_EXPORT mus_float_t mus_formant(mus_any *ptr, mus_float_t input); 
MUS_EXPORT mus_any *mus_make_formant(mus_float_t frequency, mus_float_t radius);
MUS_EXPORT bool mus_formant_p(mus_any *ptr);
MUS_EXPORT void mus_set_formant_radius_and_frequency(mus_any *ptr, mus_float_t radius, mus_float_t frequency);
MUS_EXPORT mus_float_t mus_formant_with_frequency(mus_any *ptr, mus_float_t input, mus_float_t freq_in_radians);

MUS_EXPORT mus_float_t mus_formant_bank(mus_any *bank, mus_float_t inval);
MUS_EXPORT mus_float_t mus_formant_bank_with_inputs(mus_any *bank, mus_float_t *inval);
  MUS_EXPORT mus_any *mus_make_formant_bank(int size, mus_any **formants, mus_float_t *amps);
MUS_EXPORT bool mus_formant_bank_p(mus_any *g);

MUS_EXPORT mus_float_t mus_firmant(mus_any *ptr, mus_float_t input);
MUS_EXPORT mus_any *mus_make_firmant(mus_float_t frequency, mus_float_t radius);
MUS_EXPORT bool mus_firmant_p(mus_any *ptr);
MUS_EXPORT mus_float_t mus_firmant_with_frequency(mus_any *ptr, mus_float_t input, mus_float_t freq_in_radians);

MUS_EXPORT mus_float_t mus_filter(mus_any *ptr, mus_float_t input);
MUS_EXPORT mus_any *mus_make_filter(int order, mus_float_t *xcoeffs, mus_float_t *ycoeffs, mus_float_t *state);
MUS_EXPORT bool mus_filter_p(mus_any *ptr);

MUS_EXPORT mus_float_t mus_fir_filter(mus_any *ptr, mus_float_t input);
MUS_EXPORT mus_any *mus_make_fir_filter(int order, mus_float_t *xcoeffs, mus_float_t *state);
MUS_EXPORT bool mus_fir_filter_p(mus_any *ptr);

MUS_EXPORT mus_float_t mus_iir_filter(mus_any *ptr, mus_float_t input);
MUS_EXPORT mus_any *mus_make_iir_filter(int order, mus_float_t *ycoeffs, mus_float_t *state);
MUS_EXPORT bool mus_iir_filter_p(mus_any *ptr);
MUS_EXPORT mus_float_t *mus_make_fir_coeffs(int order, mus_float_t *env, mus_float_t *aa);

MUS_EXPORT mus_float_t *mus_filter_set_xcoeffs(mus_any *ptr, mus_float_t *new_data);
MUS_EXPORT mus_float_t *mus_filter_set_ycoeffs(mus_any *ptr, mus_float_t *new_data);
MUS_EXPORT int mus_filter_set_order(mus_any *ptr, int order);

MUS_EXPORT mus_float_t mus_filtered_comb(mus_any *ptr, mus_float_t input, mus_float_t pm);
MUS_EXPORT mus_float_t mus_filtered_comb_unmodulated(mus_any *ptr, mus_float_t input);
MUS_EXPORT bool mus_filtered_comb_p(mus_any *ptr);
MUS_EXPORT mus_any *mus_make_filtered_comb(mus_float_t scaler, int size, mus_float_t *line, int line_size, mus_interp_t type, mus_any *filt);

MUS_EXPORT mus_float_t mus_filtered_comb_bank(mus_any *bank, mus_float_t inval);
MUS_EXPORT mus_any *mus_make_filtered_comb_bank(int size, mus_any **combs);
MUS_EXPORT bool mus_filtered_comb_bank_p(mus_any *g);

MUS_EXPORT mus_float_t mus_wave_train(mus_any *gen, mus_float_t fm);
MUS_EXPORT mus_float_t mus_wave_train_unmodulated(mus_any *gen);
MUS_EXPORT mus_any *mus_make_wave_train(mus_float_t freq, mus_float_t phase, mus_float_t *wave, mus_long_t wsize, mus_interp_t type);
MUS_EXPORT bool mus_wave_train_p(mus_any *gen);

MUS_EXPORT mus_float_t *mus_partials_to_polynomial(int npartials, mus_float_t *partials, mus_polynomial_t kind);
MUS_EXPORT mus_float_t *mus_normalize_partials(int num_partials, mus_float_t *partials);

MUS_EXPORT mus_any *mus_make_polyshape(mus_float_t frequency, mus_float_t phase, mus_float_t *coeffs, int size, int cheby_choice);
MUS_EXPORT mus_float_t mus_polyshape(mus_any *ptr, mus_float_t index, mus_float_t fm);
MUS_EXPORT mus_float_t mus_polyshape_unmodulated(mus_any *ptr, mus_float_t index);
#define mus_polyshape_no_input(Obj) mus_polyshape(Obj, 1.0, 0.0)
MUS_EXPORT bool mus_polyshape_p(mus_any *ptr);

MUS_EXPORT mus_any *mus_make_polywave(mus_float_t frequency, mus_float_t *coeffs, int n, int cheby_choice);
MUS_EXPORT bool mus_polywave_p(mus_any *ptr);
MUS_EXPORT mus_float_t mus_polywave_unmodulated(mus_any *ptr);
MUS_EXPORT mus_float_t mus_polywave(mus_any *ptr, mus_float_t fm);
MUS_EXPORT mus_float_t mus_chebyshev_t_sum(mus_float_t x, int n, mus_float_t *tn);
MUS_EXPORT mus_float_t mus_chebyshev_u_sum(mus_float_t x, int n, mus_float_t *un);
MUS_EXPORT mus_float_t mus_chebyshev_tu_sum(mus_float_t x, int n, mus_float_t *tn, mus_float_t *un);

MUS_EXPORT mus_float_t mus_env(mus_any *ptr);
MUS_EXPORT mus_any *mus_make_env(mus_float_t *brkpts, int npts, double scaler, double offset, double base, double duration, mus_long_t end, mus_float_t *odata);
MUS_EXPORT bool mus_env_p(mus_any *ptr);
MUS_EXPORT double mus_env_interp(double x, mus_any *env);
MUS_EXPORT mus_long_t *mus_env_passes(mus_any *gen);        /* for Snd */
MUS_EXPORT double *mus_env_rates(mus_any *gen);        /* for Snd */
MUS_EXPORT double mus_env_offset(mus_any *gen);        /* for Snd */
MUS_EXPORT double mus_env_scaler(mus_any *gen);        /* for Snd */
MUS_EXPORT double mus_env_initial_power(mus_any *gen); /* for Snd */
MUS_EXPORT int mus_env_breakpoints(mus_any *gen);      /* for Snd */
MUS_EXPORT mus_float_t mus_env_any(mus_any *e, mus_float_t (*connect_points)(mus_float_t val));
#define mus_make_env_with_length(Brkpts, Pts, Scaler, Offset, Base, Length) mus_make_env(Brkpts, Pts, Scaler, Offset, Base, 0.0, (Length) - 1, NULL)

MUS_EXPORT mus_any *mus_make_pulsed_env(mus_any *e, mus_any *p);
MUS_EXPORT bool mus_pulsed_env_p(mus_any *ptr);
MUS_EXPORT mus_float_t mus_pulsed_env(mus_any *pl, mus_float_t inval);
MUS_EXPORT mus_float_t mus_pulsed_env_unmodulated(mus_any *pl);

MUS_EXPORT bool mus_frame_p(mus_any *ptr);
MUS_EXPORT bool mus_frame_or_mixer_p(mus_any *ptr);
MUS_EXPORT mus_any *mus_make_empty_frame(int chans);
MUS_EXPORT mus_any *mus_make_frame(int chans, ...);
MUS_EXPORT mus_any *mus_frame_add(mus_any *f1, mus_any *f2, mus_any *res);
MUS_EXPORT mus_any *mus_frame_multiply(mus_any *f1, mus_any *f2, mus_any *res);
MUS_EXPORT mus_any *mus_frame_scale(mus_any *uf1, mus_float_t scl, mus_any *ures);
MUS_EXPORT mus_any *mus_frame_offset(mus_any *uf1, mus_float_t offset, mus_any *ures);
MUS_EXPORT mus_float_t mus_frame_ref(mus_any *f, int chan);
MUS_EXPORT mus_float_t mus_frame_set(mus_any *f, int chan, mus_float_t val);
MUS_EXPORT mus_any *mus_frame_copy(mus_any *uf);
MUS_EXPORT mus_float_t mus_frame_fill(mus_any *uf, mus_float_t val);

MUS_EXPORT bool mus_mixer_p(mus_any *ptr);
MUS_EXPORT mus_any *mus_make_empty_mixer(int chans);
MUS_EXPORT mus_any *mus_make_identity_mixer(int chans);
MUS_EXPORT mus_any *mus_make_mixer(int chans, ...);
MUS_EXPORT mus_float_t mus_mixer_ref(mus_any *f, int in, int out);
MUS_EXPORT mus_float_t mus_mixer_set(mus_any *f, int in, int out, mus_float_t val);
MUS_EXPORT mus_any *mus_frame_to_frame(mus_any *f, mus_any *in, mus_any *out);
MUS_EXPORT mus_any *mus_sample_to_frame(mus_any *f, mus_float_t in, mus_any *out);
MUS_EXPORT mus_float_t mus_frame_to_sample(mus_any *f, mus_any *in);
MUS_EXPORT mus_any *mus_mixer_multiply(mus_any *f1, mus_any *f2, mus_any *res);
MUS_EXPORT mus_any *mus_mixer_add(mus_any *f1, mus_any *f2, mus_any *res);
MUS_EXPORT mus_any *mus_mixer_scale(mus_any *uf1, mus_float_t scaler, mus_any *ures);
MUS_EXPORT mus_any *mus_mixer_offset(mus_any *uf1, mus_float_t offset, mus_any *ures);
MUS_EXPORT mus_any *mus_make_scalar_mixer(int chans, mus_float_t scalar);
MUS_EXPORT mus_any *mus_mixer_copy(mus_any *uf);
MUS_EXPORT mus_float_t mus_mixer_fill(mus_any *uf, mus_float_t val);

MUS_EXPORT bool mus_file_to_sample_p(mus_any *ptr);
MUS_EXPORT mus_any *mus_make_file_to_sample(const char *filename);
MUS_EXPORT mus_any *mus_make_file_to_sample_with_buffer_size(const char *filename, mus_long_t buffer_size);
MUS_EXPORT mus_float_t mus_file_to_sample(mus_any *ptr, mus_long_t samp, int chan);
MUS_EXPORT mus_float_t mus_in_any_from_file(mus_any *ptr, mus_long_t samp, int chan);

MUS_EXPORT mus_float_t mus_readin(mus_any *rd);
MUS_EXPORT mus_any *mus_make_readin_with_buffer_size(const char *filename, int chan, mus_long_t start, int direction, mus_long_t buffer_size);
#define mus_make_readin(Filename, Chan, Start, Direction) mus_make_readin_with_buffer_size(Filename, Chan, Start, Direction, mus_file_buffer_size())
MUS_EXPORT bool mus_readin_p(mus_any *ptr);

MUS_EXPORT bool mus_output_p(mus_any *ptr);
MUS_EXPORT bool mus_input_p(mus_any *ptr);
MUS_EXPORT mus_float_t mus_in_any(mus_long_t frame, int chan, mus_any *IO);
MUS_EXPORT bool mus_in_any_is_safe(mus_any *IO);

MUS_EXPORT mus_any *mus_make_file_to_frame(const char *filename);
MUS_EXPORT bool mus_file_to_frame_p(mus_any *ptr);
MUS_EXPORT mus_any *mus_file_to_frame(mus_any *ptr, mus_long_t samp, mus_any *f);
MUS_EXPORT mus_any *mus_make_file_to_frame_with_buffer_size(const char *filename, mus_long_t buffer_size);

MUS_EXPORT bool mus_sample_to_file_p(mus_any *ptr);
MUS_EXPORT mus_any *mus_make_sample_to_file_with_comment(const char *filename, int out_chans, int out_format, int out_type, const char *comment);
#define mus_make_sample_to_file(Filename, Chans, OutFormat, OutType) mus_make_sample_to_file_with_comment(Filename, Chans, OutFormat, OutType, NULL)
MUS_EXPORT mus_float_t mus_sample_to_file(mus_any *ptr, mus_long_t samp, int chan, mus_float_t val);
MUS_EXPORT mus_any *mus_continue_sample_to_file(const char *filename);
MUS_EXPORT int mus_close_file(mus_any *ptr);
MUS_EXPORT mus_any *mus_sample_to_file_add(mus_any *out1, mus_any *out2);

MUS_EXPORT mus_float_t mus_out_any(mus_long_t frame, mus_float_t val, int chan, mus_any *IO);
MUS_EXPORT mus_float_t mus_safe_out_any_to_file(mus_long_t samp, mus_float_t val, int chan, mus_any *IO);
MUS_EXPORT bool mus_out_any_is_safe(mus_any *IO);
MUS_EXPORT mus_float_t mus_out_any_to_file(mus_any *ptr, mus_long_t samp, int chan, mus_float_t val);
MUS_EXPORT mus_long_t mus_out_any_data_start(mus_any *IO);
MUS_EXPORT mus_long_t mus_out_any_data_end(mus_any *IO);
MUS_EXPORT int mus_out_any_channels(mus_any *IO);
MUS_EXPORT mus_float_t **mus_out_any_buffers(mus_any *IO);
MUS_EXPORT void mus_out_any_set_end(mus_any *IO, mus_long_t end);

MUS_EXPORT bool mus_frame_to_file_p(mus_any *ptr);
MUS_EXPORT mus_any *mus_frame_to_file(mus_any *ptr, mus_long_t samp, mus_any *data);
MUS_EXPORT mus_any *mus_make_frame_to_file_with_comment(const char *filename, int chans, int out_format, int out_type, const char *comment);
#define mus_make_frame_to_file(Filename, Chans, OutFormat, OutType) mus_make_frame_to_file_with_comment(Filename, Chans, OutFormat, OutType, NULL)
MUS_EXPORT mus_any *mus_continue_frame_to_file(const char *filename);

MUS_EXPORT void mus_locsig(mus_any *ptr, mus_long_t loc, mus_float_t val);
MUS_EXPORT mus_any *mus_make_locsig(mus_float_t degree, mus_float_t distance, mus_float_t reverb, int chans, mus_any *output, int rev_chans, mus_any *revput, mus_interp_t type);
MUS_EXPORT bool mus_locsig_p(mus_any *ptr);
MUS_EXPORT mus_float_t mus_locsig_ref(mus_any *ptr, int chan);
MUS_EXPORT mus_float_t mus_locsig_set(mus_any *ptr, int chan, mus_float_t val);
MUS_EXPORT mus_float_t mus_locsig_reverb_ref(mus_any *ptr, int chan);
MUS_EXPORT mus_float_t mus_locsig_reverb_set(mus_any *ptr, int chan, mus_float_t val);
MUS_EXPORT void mus_move_locsig(mus_any *ptr, mus_float_t degree, mus_float_t distance);
MUS_EXPORT mus_any *mus_locsig_outf(mus_any *ptr);
MUS_EXPORT mus_any *mus_locsig_revf(mus_any *ptr);
MUS_EXPORT void *mus_locsig_closure(mus_any *ptr);
MUS_EXPORT void mus_locsig_set_detour(mus_any *ptr, void (*detour)(mus_any *ptr, mus_long_t val));
MUS_EXPORT bool mus_locsig_output_is_safe(mus_any *ptr);
MUS_EXPORT int mus_locsig_channels(mus_any *ptr);
MUS_EXPORT int mus_locsig_reverb_channels(mus_any *ptr);
MUS_EXPORT mus_any *mus_locsig_out_writer(mus_any *ptr);
MUS_EXPORT mus_any *mus_locsig_rev_writer(mus_any *ptr);

MUS_EXPORT bool mus_move_sound_p(mus_any *ptr);
MUS_EXPORT mus_float_t mus_move_sound(mus_any *ptr, mus_long_t loc, mus_float_t val);
MUS_EXPORT mus_any *mus_make_move_sound(mus_long_t start, mus_long_t end, int out_channels, int rev_channels,
					mus_any *doppler_delay, mus_any *doppler_env, mus_any *rev_env,
					mus_any **out_delays, mus_any **out_envs, mus_any **rev_envs,
					int *out_map, mus_any *output, mus_any *revput, bool free_arrays, bool free_gens);
MUS_EXPORT mus_any *mus_move_sound_outf(mus_any *ptr);
MUS_EXPORT mus_any *mus_move_sound_revf(mus_any *ptr);
MUS_EXPORT void *mus_move_sound_closure(mus_any *ptr);
MUS_EXPORT void mus_move_sound_set_detour(mus_any *ptr, void (*detour)(mus_any *ptr, mus_long_t val));

MUS_EXPORT mus_any *mus_make_src(mus_float_t (*input)(void *arg, int direction), mus_float_t srate, int width, void *closure);
MUS_EXPORT mus_any *mus_make_src_with_init(mus_float_t (*input)(void *arg, int direction), mus_float_t srate, int width, void *closure, void (*init)(void *p, mus_any *g));
MUS_EXPORT mus_float_t mus_src(mus_any *srptr, mus_float_t sr_change, mus_float_t (*input)(void *arg, int direction));
MUS_EXPORT bool mus_src_p(mus_any *ptr);
MUS_EXPORT mus_float_t mus_src_20(mus_any *srptr, mus_float_t (*input)(void *arg, int direction));
MUS_EXPORT mus_float_t mus_src_05(mus_any *srptr, mus_float_t (*input)(void *arg, int direction));

MUS_EXPORT bool mus_convolve_p(mus_any *ptr);
MUS_EXPORT mus_float_t mus_convolve(mus_any *ptr, mus_float_t (*input)(void *arg, int direction));
MUS_EXPORT mus_any *mus_make_convolve(mus_float_t (*input)(void *arg, int direction), mus_float_t *filter, mus_long_t fftsize, mus_long_t filtersize, void *closure);

MUS_EXPORT mus_float_t *mus_spectrum(mus_float_t *rdat, mus_float_t *idat, mus_float_t *window, mus_long_t n, mus_spectrum_t type);
MUS_EXPORT void mus_fft(mus_float_t *rl, mus_float_t *im, mus_long_t n, int is);
MUS_EXPORT mus_float_t *mus_make_fft_window(mus_fft_window_t type, mus_long_t size, mus_float_t beta);
MUS_EXPORT mus_float_t *mus_make_fft_window_with_window(mus_fft_window_t type, mus_long_t size, mus_float_t beta, mus_float_t mu, mus_float_t *window);
MUS_EXPORT const char *mus_fft_window_name(mus_fft_window_t win);
MUS_EXPORT const char **mus_fft_window_names(void);

MUS_EXPORT mus_float_t *mus_autocorrelate(mus_float_t *data, mus_long_t n);
MUS_EXPORT mus_float_t *mus_correlate(mus_float_t *data1, mus_float_t *data2, mus_long_t n);
MUS_EXPORT mus_float_t *mus_convolution(mus_float_t *rl1, mus_float_t *rl2, mus_long_t n);
MUS_EXPORT void mus_convolve_files(const char *file1, const char *file2, mus_float_t maxamp, const char *output_file);
MUS_EXPORT mus_float_t *mus_cepstrum(mus_float_t *data, mus_long_t n);

MUS_EXPORT bool mus_granulate_p(mus_any *ptr);
MUS_EXPORT mus_float_t mus_granulate(mus_any *ptr, mus_float_t (*input)(void *arg, int direction));
MUS_EXPORT mus_float_t mus_granulate_with_editor(mus_any *ptr, mus_float_t (*input)(void *arg, int direction), int (*edit)(void *closure));
MUS_EXPORT mus_any *mus_make_granulate(mus_float_t (*input)(void *arg, int direction), 
				       mus_float_t expansion, mus_float_t length, mus_float_t scaler, 
				       mus_float_t hop, mus_float_t ramp, mus_float_t jitter, int max_size, 
				       int (*edit)(void *closure),
				       void *closure);
MUS_EXPORT int mus_granulate_grain_max_length(mus_any *ptr);
MUS_EXPORT void mus_granulate_set_edit_function(mus_any *ptr, int (*edit)(void *closure));

MUS_EXPORT mus_long_t mus_set_file_buffer_size(mus_long_t size);
MUS_EXPORT mus_long_t mus_file_buffer_size(void);

MUS_EXPORT void mus_mix(const char *outfile, const char *infile, mus_long_t out_start, mus_long_t out_samps, mus_long_t in_start, mus_any *mx, mus_any ***envs);
MUS_EXPORT void mus_mix_with_reader_and_writer(mus_any *outf, mus_any *inf, mus_long_t out_start, mus_long_t out_frames, mus_long_t in_start, mus_any *umx, mus_any ***envs);
MUS_EXPORT mus_float_t mus_apply(mus_any *gen, mus_float_t f1, mus_float_t f2);

MUS_EXPORT bool mus_phase_vocoder_p(mus_any *ptr);
MUS_EXPORT mus_any *mus_make_phase_vocoder(mus_float_t (*input)(void *arg, int direction), 
					   int fftsize, int overlap, int interp,
					   mus_float_t pitch,
					   bool (*analyze)(void *arg, mus_float_t (*input)(void *arg1, int direction)),
					   int (*edit)(void *arg), /* return value is ignored (int return type is intended to be consistent with granulate) */
					   mus_float_t (*synthesize)(void *arg), 
					   void *closure);
MUS_EXPORT mus_float_t mus_phase_vocoder(mus_any *ptr, mus_float_t (*input)(void *arg, int direction));
MUS_EXPORT mus_float_t mus_phase_vocoder_with_editors(mus_any *ptr, 
						mus_float_t (*input)(void *arg, int direction),
						bool (*analyze)(void *arg, mus_float_t (*input)(void *arg1, int direction)),
						int (*edit)(void *arg), 
						mus_float_t (*synthesize)(void *arg));

MUS_EXPORT mus_float_t *mus_phase_vocoder_amp_increments(mus_any *ptr);
MUS_EXPORT mus_float_t *mus_phase_vocoder_amps(mus_any *ptr);
MUS_EXPORT mus_float_t *mus_phase_vocoder_freqs(mus_any *ptr);
MUS_EXPORT mus_float_t *mus_phase_vocoder_phases(mus_any *ptr);
MUS_EXPORT mus_float_t *mus_phase_vocoder_phase_increments(mus_any *ptr);


MUS_EXPORT mus_any *mus_make_ssb_am(mus_float_t freq, int order);
MUS_EXPORT bool mus_ssb_am_p(mus_any *ptr);
MUS_EXPORT mus_float_t mus_ssb_am_unmodulated(mus_any *ptr, mus_float_t insig);
MUS_EXPORT mus_float_t mus_ssb_am(mus_any *ptr, mus_float_t insig, mus_float_t fm);

MUS_EXPORT void mus_clear_sinc_tables(void);
MUS_EXPORT void *mus_environ(mus_any *gen);
MUS_EXPORT void *mus_set_environ(mus_any *gen, void *e);


/* used only in run.lisp */
MUS_EXPORT mus_any *mus_make_frame_with_data(int chans, mus_float_t *data);
MUS_EXPORT mus_any *mus_make_mixer_with_data(int chans, mus_float_t *data);

#ifdef __cplusplus
}
#endif

#endif


/* Change log.
 *
 * 19-Apr:     rxyk!cos and rxyk!sin from generators.scm.
 * 11-Apr:     mus_tap_p as a better name for mus_delay_line_p.
 * 27-Mar:     comb-bank, all-pass-bank, filtered-comb-bank, pulsed-env, oscil-bank.
 * 21-Mar:     one-pole-all-pass generator.
 * 14-Mar:     formant-bank generator.
 *             removed mus_delay_tick_noz.
 * 4-Mar:      moving_max generator.
 *             removed the unstable filter check in make_two_pole.
 * 21-Jan-13:  changed mus_formant_bank parameters.
 * --------
 * 22-Dec:     removed all the safety settings.
 * 15-Nov:     removed mus_env_t, mus_env_type, and other recently deprecated stuff.
 * 15-Jul:     more changes for clm2xen.
 * 4-July-12:  moved various struct definitions to clm.c
 *             added accessors for mus_any_class etc.
 * --------
 * 1-Sep:      mus_type.
 * 20-Aug:     changed type of mus_locsig to void, added mus_locsig_function_reset.
 *             removed function-as-output-location from locsig et al.
 * 14-Jul-11:  removed pthread stuff.
 * --------
 * 7-Mar-10:   protect in-any and out-any from sample numbers less than 0.
 * --------
 * 14-Oct:     sine-summation, sum-of-sines, sum-of-cosines removed.
 * 28-Aug:     changed some fft-related sizes from int to mus_long_t.
 * 17-Aug:     mus_frame|mixer_copy|fill.
 * 27-Jul:     mus_float_t for Float, and mus_long_t for off_t.
 * 15-Jun:     mus_rectangular_to_magnitudes (polar, but ignore phases).
 * 11-Jun:     mus_cepstrum.
 * 11-May:     MUS_ENV_LINEAR and friends, also mus_env_linear|exponential.
 *             mus_frame_to_frame_mono|stereo.
 * 12-Mar:     sinc, papoulis and dpss (slepian windows).
 * 1-Jan-09:   added MUS_EXPORT.
 * --------
 * 11-Dec:     deprecated the sine-summation, sum-of-cosines, and sum-of-sines generators.
 * 30-Oct:     mus_sample_to_file_add.
 *             mus_describe once again allocates a fresh output string.
 *             finally removed sine-bank.
 * 9-Oct:      various thread-related internal changes.
 * 14-Jul:     mus_data_format_zero.
 * 12-Jul:     mus_interp_type_p and mus_fft_window_p for C++'s benefit.
 * 1-July:     mus-safety and various ints changed to mus_long_t.
 * 20-Jun:     support for pthreads.
 * 16-Jun:     changed init_mus_module to mus_initialize.
 * 30-May:     changed polyshape to use cos and added cheby_choice arg to mus_make_polyshape.
 * 27-May:     mus_waveshape retired -- generators.scm has a wrapper for it.
 *             clm_free, clm_realloc etc for rt work.
 *             mus_chebyshev_tu_sum.
 * 25-May:     mus_polywave algorithm changed.
 * 17-May:     mus_normalize_partials.
 * 12-Apr:     added choice arg to mus_make_polywave.
 * 8-Apr:      polywave uses sine-bank if highest harmonic out of Chebyshev range.
 * 1-Mar:      mus_set_name.
 * 26-Feb:     removed mus_cosines (use mus_length)
 * 24-Feb:     removed mus_make_env_with_start, added mus_make_env_with_length
 * 20-Feb:     clm 4:
 *             polywave for polyshape and waveshape.  
 *             mus_formant_with_frequency. 
 *             firmant generator.
 *             removed mus_formant_radius and mus_set_formant_radius.
 *             removed "gain" arg from mus_make_formant.
 *             reversed the order of the arguments to mus_make_formant.
 *             fixed long-standing bug in gain calculation in mus_formant.
 *             mus_env_any for arbitrary connecting functions.
 * 15-Feb:     nrxysin and nrxycos for sine-summation.
 * 12-Feb:     nsin for sum_of_sines, ncos for sum_of_cosines.
 * 4-Feb:      clm_default_frequency (clm2xen) and *clm-default-frequency* (ws.scm).
 * 7-Jan-08:   :dur replaced by :length in make-env.
 * --------
 * 19-Oct:     all *_0 *_1 *_2 names now use _fm|_pm|_unmodulated|_no_input.
 * 17-Oct:     replace some method macros with functions (def-clm-struct local methods need true names).
 * 15-Oct:     mus_oscil_1 -> _fm, _2->_pm.
 *             mus_phase_vocoder_outctr accessors changed to use mus_location.
 * 11-Oct:     changed default srate to 44100.
 * 5-Oct:      mus_oscil_2.
 * 6-Sep:      changed asymmetric-fm to use cos(sin) and added amplitude normalization.
 * 6-Aug:      mus_autocorrelate, mus_correlate.
 * 3-Aug:      blackman5..10 and Rife-Vincent (RV2..4 fft), mlt-sine windows.
 * 16-July:    removed start arg from mus_make_env (see mus_make_env_with_start).
 * 5-July:     changed some mus_float_ts to doubles in env funcs.
 *               exp envs now use repeated multiplies rather than direct exp call.
 * 19-June:    mus-increment on gens with a notion of frequency (phase increment); 
 *               to make room for this, asymmetric-fm ratio and sine-summation b moved to mus-offset.
 * 22-Feb:     mus_big_fft and mus_spectrum_t.
 * 21-Feb:     mus_fft_window_name.
 * 14-Feb-07:  three more fft window choices.
 * --------
 * 27-Nov:     move-sound array access parallel to locsig.
 * 22-Nov:     had to add non-backwards-compatible reverb chans arg to mus_make_locsig.
 * 21-Nov:     mus_float_equal_fudge_factor, mus_arrays_are_equal.
 * 30-July:    renamed average to moving_average.
 * 28-July:    renamed make_ppolar and make_zpolar to make_two_pole|zero_from_radius_and_frequency.
 *             added mus_scaler and mus_frequency methods for two_pole and two_zero.
 * 21-July:    removed mus_wrapper field -- old way can't work since we need the original XEN object.
 * 3-July:     mus_move_sound (dlocsig) generator.
 *             changed return type of mus_locsig to float.
 * 28-June:    mus_filtered_comb generator.
 * 8-May:      mus_apply now takes 3 args: gen, two doubles (rather than bug-prone varargs).
 * 1-Mar-06:   granulate now has a local random number seed (settable via the mus-location method).
 * --------
 * 20-Dec:     samaraki and ultraspherical windows.
 *               this required a non-backwards-compatible additional argument in mus_make_fft_window_with_window.
 * 1-Nov:      mus_filter_set_x|ycoeffs, mus_filter_set_order (needed by Snd).
 * 1-May:      mus-scaler|feedback ok with delay and average.
 * 18-Apr:     mus_set_environ.
 * 11-Apr:     mus_mixer|frame_offset, mus_frame_scale (for higher level generic functions).
 * 23-Mar:     frame_to_frame arg interpretation changed.
 * 21-Mar:     mus_make_readin|file_to_sample|file_to_frame_with_buffer_size.
 * 16-Mar:     polyshape generator (waveshaper as polynomial + oscil)
 *             mus_chebyshev_first|second_kind.
 *             mus_partials_to_waveshape no longer normalizes the partials.
 * 18-Feb:     mus_interpolate.
 * 14-Feb:     deprecated mus_restart_env and mus_clear_filter_state.
 * 7-Feb-05:   mus_reset method, replaces mus_restart_env and mus_clear_filter_state.
 * --------
 * 20-Dec:     changed "jitter" handling if hop < .05 in granulate.
 * 15-Dec:     mus_generator? for type checks (clm2xen).
 * 11-Sep:     removed buffer generator.
 * 6-Sep:      removed mus_oscil_bank, mus_bank.
 * 24-Aug:     removed mus_inspect method -- overlaps mus_describe and is useless given gdb capabilities.
 * 27-July:    mus_granulate_with_editor and mus_phase_vocoder_with_editors.
 * 21-July:    edit-func as run-time arg to granulate (for CL/clm compatibility)
 * 19-July:    clm 3:
 *             deprecated mus_ina|b, mus-outa|b|c|d.
 *             mus_make_frame_to_file_with_comment, mus_mixer_scale, mus_make_frame|mixer_with_data.
 *             mus_make_scalar_mixer, mus_mixer_add, mus_continue_frame_to_file.
 *             changed pv_* to phase_vocoder_*
 * 28-June:    ssb_am + added fm arg (ssb_am_1 is the previous form).
 * 21-June:    wrapper method.
 * 14-June:    ssb_am generator.
 *             deprecated mus-a*|b*, replaced by mus-x|ycoeff.
 * 9-June:     mus_edot_product.
 * 7-June:     removed mus-x*|y* generic functions.
 * 24-May:     distribution arg to make-rand, make-rand-interp.
 * 11-May:     type arg to mus_make_table_lookup|wave_train, MUS_INTERP_NONE, MUS_INTERP_HERMITE.
 *             mus-interp-type.
 * 10-May:     changed MUS_LINEAR and MUS_SINUSOIDAL to MUS_INTERP_LINEAR and MUS_INTERP_SINUSOIDAL.
 *             mus-linear renamed mus-interp-linear, mus-sinusoidal renamed mus-interp-sinusoidal.
 *             added type arg to mus_make_delay|all_pass|comb|notch.
 *             added mus_delay_tick, all-pass delay line interpolation.
 * 3-May:      envelope arg to make-rand and make-rand-interp to give any arbitrary random number distribution.
 *             added mus_make_rand_with_distribution and mus_make_rand_interp_with_distribution.
 *             rand/rand-interp mus-data returns distribution (weight) function, mus-length its length.
 *             locsig mus-data returns output scalers, mus-xcoeffs returns reverb scalers
 * 26-Apr:     mus_sum_of_sines changed to mus_sine_bank.
 *             new mus_sum_of_sines parallels mus_sum_of_cosines.
 *             deprecated mus_sin.
 * 14-Apr:     changed "2" to "_to_" in several function names.
 * 12-Apr:     mus_average, mus_average_p, mus_make_average.
 * 17-Mar:     edit function added to mus_granulate.
 *             replaced MUS_DATA_POSITION with MUS_DATA_WRAPPER.
 * 22-Jan:     various "environ" variables renamed for Windows' benefit.
 * 5-Jan-04:   env_interp bugfix.
 * --------
 * 29-Sep:     removed length arg from spectrum in clm2xen.
 * 24-Aug:     changed mus_length|ramp|hop type to mus_long_t.
 * 21-Aug:     export MUS_INPUT and friends (needed for specialized INA handlers).
 * 11-Aug:     int -> bool.
 * 7-Aug:      removed mus_type.
 * 20-July:    more run methods.
 * 15-July:    linear->dB check for 0.0 arg.
 * 27-June:    mus_samples_to_seconds and mus_seconds_to_samples.
 * 9-June:     mus_mix_with_reader_and_writer.
 * 27-May:     bugfix: interpolating all-pass ("zall-pass") had an extra delay.
 * 25-Apr:     mus_spectrum and mus_convolution now return mus_float_t*.
 * 9-Apr:      removed MUS_HANNING_WINDOW (use MUS_HANN_WINDOW).
 * 3-Mar:      mus_delay_line_p for tap error checking.
 * 27-Feb:     mus_length for env -> original duration in samples.
 * 21-Feb:     mus_set_cosines added, mus_cosines moved to hop slot.
 *             mus_[set_]x1/x2/y1/y2.
 * 10-Feb:     mus_file_name moved into the mus_input|output structs.
 *             folded mus_input|output into mus_any.
 *             moved mus_frame|mixer declarations into clm.c.
 *             all mus_input|output|frame|mixer pointers->mus_any.
 *             all method void pointers->mus_any.
 * 7-Feb:      split strings out of clm2xen.c into clm-strings.h.
 * 3-Feb:      mus_offset for envs, mus_width for square_wave et al.
 *             new core class fields(10) for various methods.
 * 7-Jan-03:   mus_src with very large sr_change segfault bugfix.
 * --------
 * 17-Dec:     mus_env_offset|initial_power for Snd exp env optimizations.
 * 13-Sep:     mus_frandom and mus_irandom(for Snd optimizer).
 * 19-Aug:     changed internal phase-vocoder array accessor names
 * 13-Aug:     set!(*-ref) for frame, locsig, mixer, locsig-reverb.
 * 29-Jul:     various *_1 cases for the optimizer.
 * 15-Jul:     mus_continue_sample2file.
 * 10-Jul:     mus_file_name.
 * 7-Jun:      fftw support added.
 * 31-May:     changed mus_any_class.
 * 3-May:      many int->mus_long_t changes for large files.
 * 8-Apr:      off-by-1 env bug(Lisp/C are now identical), env_interp of exp env beyond end bugfix.
 * 1-Apr:      sine-summation n=0 bugfix.
 * 27-Mar:     negative degree locsig bugfix.
 * 18-Mar:     mus_move_locsig.
 * 15-Mar:     n-chan locsig(and reverb scalers), 'type' arg to mus_make_locsig.
 * 6-Mar:      mus_scaler in asymmetric-fm now refers to the "r" parameter, "a" in sine-summation.
 * 5-Mar:      dumb typo in asymmetric-fm generator fixed.
 * 19-Feb:     buffer reallocation redundant free bugfix.
 * 25-Jan-02:  mus_increment of env returns base.
 * --------
 * 10-Dec:     add outctr calls, phase-vocoder bugfixes, thanks to Scott Wilson.
 * 21-Oct:     fill in some set-data methods.
 * 1-Sep:      mus_polar2rectangular.
 * 6-July:     scm -> xen.
 * 26-May:     mus_rand_seed.
 * 22-May:     locsig reverb distance calc was upside down.
 * 18-May:     mus_describe and mus_inspect returned string should not be freed any more.
 * 7-May:      filled in some leftover equal_p methods.
 * 1-Apr:      mus_make_file2sample_with_comment and mus_length for file->sample/sample->file.
 *             mus_file_buffer_size.
 * 26-Mar:     extended_type field added to mus_any_class for more robust type checking.
 * 16-Mar:     mus_phase of env -> current_value.
 * 28-Feb:     added mus_position(currently only for envs).
 * 8-Feb:      clm2scm.h.
 * 24-Jan:     mus-bank in clm2scm.
 * 5-Jan:      clm2scm gens are applicable.
 * 4-Jan:      mus_bank.
 * 2-Jan-01:   mus_run method.
 * --------
 * 28-Dec:     mus_clear_filter_state and other minor tweaks for Snd.
 * 28-Nov:     Dolph-Chebyshev window(under HAVE_GSL flag -- needs complex trig support).
 * 8-Nov:      mus_clear_sinc_tables.
 * 12-Oct:     mus_formant_bank takes one input(can't remember why I had an array here)
 * 27-Sep:     mus_array_interp bugfix(imitates mus.lisp now).
 * 18-Sep:     clm now assumes it's used as a part of sndlib.
 * 11-Sep:     generalized set! to generic functions in clm2scm.c.
 * 31-Aug:     changed formant field setters(thanks to Anders Vinjar).
 * 10-Aug:     removed built-in setf support(clm2scm.c).
 * 31-Jul:     mus_granulate tries to protect against illegal length and ramp values.
 * 24-Jul:     mus_make_fir_coeffs.
 * 20-Jul:     sum_of_sines, atan2 to rectangular->polar, phase_vocoder gen.
 * 22-June:    made mus_bessi0 local again.
 * 1-June:     bugfixes for linuxppc 2000.
 * 19-May:     mus_apply.
 * 8-May:      added "const" and XEN_PROCEDURE_CAST(for c++), made mus_bessi0 global.
 * 24-Apr:     changed formant radius to match lisp version(it's now 1-old_radius)
 * 20-Apr:     mus_convolve_files
 * 7-Apr:      src width bug fixed
 * 31-Mar:     finally implemented set-location for envs.
 * 14-Feb:     buffer-full?.
 * 1-Feb:      removed mus_phasepartials2waveshape.
 * 3-Jan-00:   format and type args added to make_sample2file, 
 *             mus_file_close. 
 *             removed make_file_input and make_file_output.
 * --------
 * 29-Dec:     various bugfixes especially in envelope handlers.
 * 19-Nov:     mus_oscil_bank and mus_formant_bank.
 * 5-Nov:      mus_sin exported.
 * 4-Oct:(scm) make-env arg order changed to reflect mus.lisp.
 * 29-Sep:     implemented mus-increment and mus-frequency for granulate(as in mus.lisp).
 *             clm's fft renamed mus-fft to avoid collision with snd's version.
 *             added max_size arg to make_granulate(to reflect mus.lisp).
 * 25-Sep-99:  added width arg to make_src -- forgot this somehow in first pass.
 *             decided to make mus_inspect return char* like mus_describe.
 */
