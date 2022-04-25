#!/usr/bin/env ruby
# fmviolin.rb -- CLM fmviolin.clm --> RBM fmviolin.rb

# Translator/Author: Michael Scholz <mi-scholz@users.sourceforge.net>
# Created: Fri Oct 18 11:29:08 CEST 2002
# Changed: Sat Jul 28 03:12:42 CEST 2012

# Commentary:

# A translation of Bill Schottstaedt's clm/fmviolin.clm from Lisp
# into Ruby.

# short_example
# long_example

# Code:

$clm_c_version = true

require "sndlib"
if $clm_c_version
  require "sndins"
else
  require "v"		# fm_violin_rb, jc_reverb_rb
  require "clm-ins"	# nrev_rb
  require "freeverb"	# freeverb_rb
  class Instrument
    alias fm_violin fm_violin_rb
    alias jc_reverb jc_reverb_rb
    alias nrev nrev_rb 
    alias freeverb freeverb_rb 
  end
end
require "clm"
require "ws"

$clm_file_name	= "test-ins-r.snd"
$clm_play	= true
$clm_statistics	= true
$clm_verbose	= true
$clm_srate	= 44100
$clm_channels	= 2
$clm_reverb_channels = 2
$clm_delete_reverb = true

# show progress of long example
$show = true
$t = 0.0

def main
  ARGV.length > 0 ? short_example() : long_example()
end

CELLO = 0
VIOLIN = 1

def restore_fm_violin_defaults
  $fm_index                   = 1.0
  $amp_env                    = [0, 0, 25, 1, 75, 1, 100, 0]
  $periodic_vibrato_rate      = 5.0
  $periodic_vibrato_amplitude = 0.0025
  $random_vibrato_rate        = 16.0
  $random_vibrato_amplitude   = 0.005
  $noise_freq                 = 1000.0
  $noise_amount               = 0.0
  $ind_noise_freq             = 10.0
  $ind_noise_amount           = 0.0
  $amp_noise_freq             = 20.0
  $amp_noise_amount           = 0.0
  $gliss_env                  = [0, 0, 100, 0]
  $glissando_amount           = 0.0
  $fm1_env                    = [0, 1, 25, 0.4, 75, 0.6, 100, 0]
  $fm2_env                    = [0, 1, 25, 0.4, 75, 0.6, 100, 0]
  $fm3_env                    = [0, 1, 25, 0.4, 75, 0.6, 100, 0]
  $fm1_rat                    = 1.0
  $fm2_rat                    = 3.0
  $fm3_rat                    = 4.0
  $fm1_index                  = false
  $fm2_index                  = false
  $fm3_index                  = false
  $base                       = 0.0
  $degree                     = 0.0
  $distance                   = 1.0
  $reverb_amount              = 0.01
  $index_type                 = VIOLIN
  $no_waveshaping             = false
end

def old_fm_violin(start, dur, freq, amp, *args)
  fm_violin(start, dur, freq, amp,
	    :fm_index,         get_args(args, :fm_index, $fm_index),
	    :amp_env,          get_args(args, :amp_env, $amp_env),
	    :periodic_vibrato_rate, get_args(args, :periodic_vibrato_rate,
	      $periodic_vibrato_rate),
	    :periodic_vibrato_amplitude,
            get_args(args, :periodic_vibrato_amplitude, 
	      $periodic_vibrato_amplitude),
	    :random_vibrato_rate,   get_args(args, :random_vibrato_rate, 
	      $random_vibrato_rate),
	    :random_vibrato_amplitude,
            get_args(args, :random_vibrato_amplitude, 
	      $random_vibrato_amplitude),
	    :noise_freq,       get_args(args, :noise_freq, $noise_freq),
	    :noise_amount,     get_args(args, :noise_amount, $noise_amount),
	    :ind_noise_freq,   get_args(args, :ind_noise_freq, $ind_noise_freq),
	    :ind_noise_amount, get_args(args, :ind_noise_amount, 
	      $ind_noise_amount),
	    :amp_noise_freq,   get_args(args, :amp_noise_freq, $amp_noise_freq),
	    :amp_noise_amount, get_args(args, :amp_noise_amount, 
	      $amp_noise_amount),
	    :gliss_env,        get_args(args, :gliss_env, $gliss_env),
	    :glissando_amount, get_args(args, :glissando_amount, 
	      $glissando_amount),
	    :fm1_env,          get_args(args, :fm1_env, $fm1_env),
	    :fm2_env,          get_args(args, :fm2_env, $fm2_env),
	    :fm3_env,          get_args(args, :fm3_env, $fm3_env),
	    :fm1_rat,          get_args(args, :fm1_rat, $fm1_rat),
	    :fm2_rat,          get_args(args, :fm2_rat, $fm2_rat),
	    :fm3_rat,          get_args(args, :fm3_rat, $fm3_rat),
	    :fm1_index,        get_args(args, :fm1_index, $fm1_index),
	    :fm2_index,        get_args(args, :fm2_index, $fm2_index),
	    :fm3_index,        get_args(args, :fm3_index, $fm3_index),
	    :base,             get_args(args, :base, $base),
	    :degree,           get_args(args, :degree, $degree),
	    :distance,         get_args(args, :distance, $distance), 
	    :reverb_amount,    get_args(args, :reverb_amount, $reverb_amount),
	    :index_type,       get_args(args, :index_type, $index_type),
	    :no_waveshaping,   get_args(args, :no_waveshaping, $no_waveshaping))
end

def vln_one_sin(start, dur, freq, amp, *args)
  old_fm_violin(start, dur, freq, amp * 0.125,
                *args + [:degree, random(90.0), :noise_amount, 0.0])
end

def vln_one_sin_ran(start, dur, freq, amp, *args)
  old_fm_violin(start, dur, freq, amp * 0.125, *args + [:degree, random(90.0)])
end

def vln_one_sin_exp(start, dur, freq, amp, *args)
  vln_one_sin(start, dur, freq, amp, *args + [:base, 0.03125])
end

alias violin vln_one_sin

def cel_one_sum(start, dur, freq, amp, *args)
  old_fm_violin(start, dur, freq, amp * 0.125,
    *args + [:degree, random(90.0), :index_type, CELLO])
end

restore_fm_violin_defaults()

def violin_new(beg, dur, freq, amp, *args)
  fm_index      = get_args(args, :fm_index, 1.0)
  amp_env       = get_args(args, :amp_env, [0, 0, 221, 1, 240, 0])
  reverb_amount = get_args(args, :reverb_amount, 0.2)
  nfreq = if(freq > 400.0)
            freq * (0.99 + 0.02 * random(1.0))
          else
            if(freq > 200.0)
              freq * (0.995 + 0.01 * random(1.0))
            else
              freq * (0.999 + 0.002 * random(1.0))
            end
          end
  6.times do |i|
    fm_violin(beg + i * 0.05 * random(1.0),
              dur + i * 0.1 * random(1.0),
              nfreq, amp,
              :reverb_amount, reverb_amount,
              :fm_index, fm_index * (0.75 + random(1.0)),
              :amp_env, amp_env)
  end
end

def short_example
  with_sound(:reverb, :nrev, :reverb_data, [:lp_coeff, 0.6]) do
    violin_new(0, 8.53, 993.323, 0.03,
      :fm_index, 0.75, :reverb_amount, 0.2, :amp_env, [0, 0, 221, 1, 240, 0])
    violin_new(5, 4.53, 993.323 * 5.0 / 6.0, 0.02,
      :fm_index, 0.55, :reverb_amount, 0.2, :amp_env, [0, 0, 221, 1, 240, 0])
  end
end

def my_times
  if defined? Process.times
    Process.times
  else
    Time.times
  end
end

def long_example
  i = 0
  st = my_times
  trace_var(:$t) do |t|
    if $show
      message("%2d: score %3d   utime %7.3f",
	i += 1, t, my_times.utime - st.utime)
    end
  end
  with_sound(:reverb, :freeverb, :reverb_data, [:room_decay, 0.8]) do
    tap = [0, 0, 1, 1, 99, 1, 100, 0]
    metalamp = [0, 0, 0.5000, 1, 5, 1, 10, 0.5000, 15, 0.2500, 35, 0.1, 100, 0]
    whoosh = [0, 0, 75, 0.1, 90, 0.3, 97, 0.6, 100, 1]
    mamp = [0, 0, 50, 1, 100, 0]
    n_amp = [0, 0, 65, 1, 100, 0]

    $t = 0.0
    restore_fm_violin_defaults()
    $glissando_amount = 0.0
    $reverb_amount = 0.1
    $amp_env = metalamp
    $fm1_rat = 6.718
    $fm2_rat = 4.414
    $fm3_rat = 5.141
    vln_one_sin($t + 0, 1.6, 164.5868, 0.16, :fm_index, 2.1087)
    vln_one_sin($t + 0.0003, 4, 164.5868, 0.2600, :fm_index, 1.5488)
    vln_one_sin($t + 0.0005, 1.2, 125.9513, 0.2600, :fm_index, 2.2999)
    vln_one_sin($t + 0.0005, 2.8, 125.9513, 0.16, :fm_index, 1.6818)
    vln_one_sin($t + 0.0013, 4, 24.4994, 0.3, :fm_index, 2.4557)
    vln_one_sin($t + 0.0033, 3, 24.4994, 0.3, :fm_index, 1.9387)
    vln_one_sin($t + 0.0035, 2.8, 24.4994, 0.2600, :fm_index, 2.3828)
    vln_one_sin($t + 0.0040, 0.8, 24.4994, 0.16, :fm_index, 1.7348)
    vln_one_sin($t + 0.0043, 4, 24.4994, 0.3, :fm_index, 2.0886)

    $t += 6.0
    $fm1_rat = 2.718
    $fm2_rat = 4.414
    $fm3_rat = 3.141
    vln_one_sin($t + 0.0003, 1.2, 88.8854, 0.16, :fm_index, 2.0711)
    vln_one_sin($t + 0.0003, 4, 88.8854, 0.2600, :fm_index, 2.0225)
    vln_one_sin($t + 0.0005, 1.2, 102.7186, 0.2600, :fm_index, 1.9300)
    vln_one_sin($t + 0.0010, 1.2, 32.7025, 0.3600, :fm_index, 1.9269)
    vln_one_sin($t + 0.0015, 2.8, 32.7025, 0.2600, :fm_index, 2.2153)
    vln_one_sin($t + 0.0023, 2, 32.7025, 0.2600, :fm_index, 2.1968)
    vln_one_sin($t + 0.0023, 4, 32.7025, 0.3600, :fm_index, 1.6091)
    vln_one_sin($t + 0.0025, 2, 32.7025, 0.2600, :fm_index, 2.1766)
    vln_one_sin($t + 0.0035, 1.2, 32.7025, 0.16, :fm_index, 1.5157)
    vln_one_sin($t + 0.0040, 0.8, 32.7025, 0.16, :fm_index, 1.8092)
    vln_one_sin($t + 0.0043, 2, 32.7025, 0.2600, :fm_index, 1.6198)

    $t += 6.0
    $reverb_amount = 0.2
    $fm3_rat = 5.141
    vln_one_sin($t + 0.0003, 1.2, 177.7708, 0.3, :fm_index, 1.9631)
    vln_one_sin($t + 0.0003, 4, 177.7708, 0.3, :fm_index, 1.9647)
    vln_one_sin($t + 0.0005, 2.8, 336.2471, 0.16, :fm_index, 2.3977)
    vln_one_sin($t + 0.0008, 1.2, 336.2471, 0.2500, :fm_index, 2.4103)
    vln_one_sin($t + 0.0010, 2, 65.4050, 0.3, :fm_index, 1.8419)
    vln_one_sin($t + 0.0033, 2, 65.4050, 0.3, :fm_index, 2.4540)
    vln_one_sin($t + 0.0043, 4, 65.4050, 0.16, :fm_index, 2.2909)

    $t += 6.0
    $reverb_amount = 0.1
    vln_one_sin($t + 0.0003, 1.2, 11.1107, 0.3, :fm_index, 1.8715)
    vln_one_sin($t + 0.0003, 4, 11.1107, 0.3, :fm_index, 2.4590)
    vln_one_sin($t + 0.0005, 2.8, 21.0154, 0.16, :fm_index, 2.3802)
    vln_one_sin($t + 0.0008, 1.2, 21.0154, 0.2500, :fm_index, 1.7564)
    vln_one_sin($t + 0.0010, 2, 4.0878, 0.3, :fm_index, 2.2529)
    vln_one_sin($t + 0.0033, 2, 4.0878, 0.3, :fm_index, 1.9693)
    vln_one_sin($t + 0.0043, 4, 4.0878, 0.16, :fm_index, 2.2534)

    $t += 6.0
    restore_fm_violin_defaults()
    $noise_amount = 0.1
    vln_one_sin_ran($t + 0, 5.4, 116.5400, 0.2500, :fm_index, 2.2822, 
      :reverb_amount, 0.0280, 
      :amp_env,
      [0, 0, 0.0556, 1, 4.0937, 0.6, 9.1414, 0.3, 24.2845, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.0100, 5.4, 43.6538, 0.2500, :fm_index, 2.0867, 
      :reverb_amount, 0.0202,
      :amp_env,
      [0, 0, 0.0556, 1, 4.0937, 0.6, 9.1414, 0.3, 24.2845, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.0200, 5.4, 130.8100, 0.2500, :fm_index, 1.9652, 
      :reverb_amount, 0.0270,
      :amp_env, 
      [0, 0, 0.0556, 1, 4.0937, 0.6, 9.1414, 0.3, 24.2845, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.0250, 5.4, 87.3075, 0.2500, :fm_index, 2.1524, 
      :reverb_amount, 0.0260,
      :amp_env, 
      [0, 0, 0.0556, 1, 4.0937, 0.6, 9.1414, 0.3, 24.2845, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.0300, 4.5, 261.6200, 0.15, :fm_index, 2.1384, 
      :reverb_amount, 0.0242,
      :amp_env, 
      [0, 0, 0.0667, 1, 4.1044, 0.6, 9.1515, 0.3, 24.2929, 0.1, 100, 0])
    vln_one_sin_ran($t + 0.0300, 4.5, 174.6150, 0.15, :fm_index, 2.1425, 
      :reverb_amount, 0.0265,
      :amp_env, 
      [0, 0, 0.0667, 1, 4.1044, 0.6, 9.1515, 0.3, 24.2929, 0.1, 100, 0])
    vln_one_sin_ran($t + 0.0300, 4.5, 130.8100, 0.15, :fm_index, 1.9805, 
      :reverb_amount, 0.0201,
      :amp_env, 
      [0, 0, 0.0667, 1, 4.1044, 0.6, 9.1515, 0.3, 24.2929, 0.1, 100, 0]) 
    vln_one_sin_ran($t + 0.0350, 4.5, 43.6538, 0.15, :fm_index, 2.4876, 
      :reverb_amount, 0.0329,
      :amp_env, 
      [0, 0, 0.0667, 1, 4.1044, 0.6, 9.1515, 0.3, 24.2929, 0.1, 100, 0])
    vln_one_sin_ran($t + 0.0400, 3.6, 220, 0.15, :fm_index, 1.8282, 
      :reverb_amount, 0.0244, :noise_amount, 0.15,
      :amp_env, 
      [0, 0, 0.0833, 1, 4.1204, 0.6, 9.1667, 0.3, 24.3056, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.0400, 3.6, 174.6150, 0.15, :fm_index, 2.3479, 
      :reverb_amount, 0.0200, :noise_amount, 0.15,
      :amp_env, 
      [0, 0, 0.0833, 1, 4.1204, 0.6, 9.1667, 0.3, 24.3056, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.0400, 3.6, 523.2400, 0.15, :fm_index, 1.6424, 
      :reverb_amount, 0.0286, :noise_amount, 0.15,
      :amp_env, 
      [0, 0, 0.0833, 1, 4.1204, 0.6, 9.1667, 0.3, 24.3056, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.0450, 3.6, 349.2300, 0.15, :fm_index, 1.6449, 
      :reverb_amount, 0.0333, :noise_amount, 0.15,
      :amp_env, 
      [0, 0, 0.0833, 1, 4.1204, 0.6, 9.1667, 0.3, 24.3056, 0.1, 100.0, 0])

    vln_one_sin_ran($t + 0.0500, 6, 699.4600, 0.15,
      :fm_index, 1.5570, :reverb_amount, 0.1300, :amp_env, tap)
    vln_one_sin_ran($t + 0.0500, 6, 1397.9200, 0.15, 
      :fm_index, 1.5131, :reverb_amount, 0.1300, :amp_env, tap)
    vln_one_sin_ran($t + 0.0500, 6, 783.9800, 0.15, 
      :fm_index, 2.2031, :reverb_amount, 0.1300, :amp_env, tap)
    vln_one_sin_ran($t + 0.0500, 6, 1046.4800, 0.15, 
      :fm_index, 2.2724, :reverb_amount, 0.1300, :amp_env, tap)
    vln_one_sin_ran($t + 0.0600, 9, 21.8269, 0.15, 
      :fm_index, 2.1048, :reverb_amount, 0.1, :amp_env, tap)
    vln_one_sin_ran($t + 0.0600, 8, 87.3075, 0.15, 
      :fm_index, 1.8854, :reverb_amount, 0.1, :amp_env, tap)
    vln_one_sin_ran($t + 0.0600, 7, 65.4050, 0.15, 
      :fm_index, 1.6781, :reverb_amount, 0.1, :amp_env, tap)
    vln_one_sin_ran($t + 0.0600, 8, 43.6538, 0.15, 
      :fm_index, 1.7862, :reverb_amount, 0.1, :amp_env, tap)
    vln_one_sin_ran($t + 0.0700, 6, 175.6150, 0.15, 
      :fm_index, 2.2656, :reverb_amount, 0.1, :amp_env, tap)
    vln_one_sin_ran($t + 0.0700, 6, 350.2300, 0.15, 
      :fm_index, 2.4241, :reverb_amount, 0.1, :amp_env, tap)
    vln_one_sin_ran($t + 0.0700, 6, 131.8100, 0.15, 
      :fm_index, 2.4294, :reverb_amount, 0.1, :amp_env, tap)
    vln_one_sin_ran($t + 0.0700, 6, 32.7025, 0.15, 
      :fm_index, 1.5779, :reverb_amount, 0.1, :amp_env, tap)
    vln_one_sin_ran($t + 0.0800, 6, 196.9950, 0.15, 
      :fm_index, 1.8511, :reverb_amount, 0.1, :amp_env, tap)
    vln_one_sin_ran($t + 0.0800, 6, 1047.4800, 0.15, 
      :fm_index, 2.2148, :reverb_amount, 0.1, :amp_env, tap)
    vln_one_sin_ran($t + 0.0800, 6, 831.6200, 0.15, 
      :fm_index, 1.9913, :reverb_amount, 0.1, :amp_env, tap)
    vln_one_sin_ran($t + 0.0800, 6, 2793.8400, 0.15, 
      :fm_index, 2.2607, :reverb_amount, 0.1, :amp_env, tap)
    vln_one_sin_ran($t + 0.2700, 6, 784.9800, 0.16, 
      :fm_index, 2.0693, :reverb_amount, 0.1, :amp_env, tap)
    vln_one_sin_ran($t + 0.2700, 6, 64.4050, 0.16, 
      :fm_index, 1.6920, :reverb_amount, 0.1, :amp_env, tap)
    vln_one_sin_ran($t + 0.2700, 6, 208.6550, 0.16, 
      :fm_index, 2.2597, :reverb_amount, 0.1, :amp_env, tap)
    vln_one_sin_ran($t + 0.2700, 6, 43.6538, 0.16,
      :fm_index, 2.2522, :reverb_amount, 0.1, :amp_env, tap)

    $t += 12.0
    restore_fm_violin_defaults()
    vln_one_sin_ran($t + 0, 1.8, 349.2300, 0.16, :fm_index, 2.1541, 
      :reverb_amount, 0.0225, :noise_amount, 0.0500,
      :amp_env, 
      [0, 0, 0.1667, 1, 4.2003, 0.6, 9.2424, 0.3, 24.3687, 0.1, 100, 0])
    vln_one_sin_ran($t + 0.0100, 2.7000, 146.8300, 0.16, :fm_index, 2.3335,
      :reverb_amount, 0.0274, :noise_amount, 0.0500,
      :amp_env,
      [0, 0, 0.1111, 1, 4.1470, 0.6, 9.1919, 0.3, 24.3266, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.0200, 1.8, 880, 0.16, :fm_index, 2.1910, 
      :reverb_amount, 0.0279, :noise_amount, 0.0500,
      :amp_env,
      [0, 0, 0.1667, 1, 4.2003, 0.6, 9.2424, 0.3, 24.3687, 0.1, 100, 0])
    vln_one_sin_ran($t + 0.0250, 3.6, 73.4150, 0.16, :fm_index, 2.1410,
      :reverb_amount, 0.0223, :noise_amount, 0.0500,
      :amp_env, 
      [0, 0, 0.0833, 1, 4.1204, 0.6, 9.1667, 0.3, 24.3056, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.0300, 2.7000, 87.3075, 0.16, :fm_index, 1.8491, 
      :reverb_amount, 0.0217, :noise_amount, 0.0010,
      :amp_env,
      [0, 0, 0.1111, 1, 4.1470, 0.6, 9.1919, 0.3, 24.3266, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.0300, 2.7000, 75.5662, 0.16, :fm_index, 1.9191, 
      :reverb_amount, 0.0204, :noise_amount, 0.0010,
      :amp_env,
      [0, 0, 0.1111, 1, 4.1470, 0.6, 9.1919, 0.3, 24.3266, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.0400, 3.6, 52.3432, 0.16, :fm_index, 1.6090, 
      :reverb_amount, 0.0296, :noise_amount, 0.0010,
      :amp_env,
      [0, 0, 0.0833, 1, 4.1204, 0.6, 9.1667, 0.3, 24.3056, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.0450, 1.8, 73.4150, 0.16, :fm_index, 2.2201, 
      :reverb_amount, 0.0221, :noise_amount, 0.0010,
      :amp_env,
      [0, 0, 0.1667, 1, 4.2003, 0.6, 9.2424, 0.3, 24.3687, 0.1, 100, 0])
    vln_one_sin_ran($t + 0.0500, 4, 116.5400, 0.0600, :fm_index, 2.0230, 
      :reverb_amount, 0.1, :amp_env, tap, :noise_amount, 0.0010)
    vln_one_sin_ran($t + 0.0500, 4, 97.9975, 0.0600, :fm_index, 1.7284, 
      :reverb_amount, 0.1, :amp_env, tap, :noise_amount, 0.0010)
    vln_one_sin_ran($t + 0.0600, 4, 36.7075, 0.0600, :fm_index, 1.6845, 
      :reverb_amount, 0.1, :amp_env, tap, :noise_amount, 0.0010)
    vln_one_sin_ran($t + 0.0650, 4, 97.9975, 0.0600, :fm_index, 2.4616, 
      :reverb_amount, 0.1, :amp_env, tap, :noise_amount, 0.0010)

    $t += 7.0
    vln_one_sin_ran($t + 0, 1.8, 261.6200, 0.16, :fm_index, 2.2576, 
      :reverb_amount, 0.0286, :noise_amount, 0.0100,
      :amp_env,
      [0, 0, 0.1667, 1, 4.2003, 0.6, 9.2424, 0.3, 24.3687, 0.1, 100, 0])
    vln_one_sin_ran($t + 0.0100, 2.7000, 130.8100, 0.16, :fm_index, 2.1530, 
      :reverb_amount, 0.0330, :noise_amount, 0.0100,
      :amp_env, 
      [0, 0, 0.1111, 1, 4.1470, 0.6, 9.1919, 0.3, 24.3266, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.0200, 1.8, 523.2400, 0.16, :fm_index, 2.0608,
      :reverb_amount, 0.0235, :noise_amount, 0.0100,
      :amp_env, 
      [0, 0, 0.1667, 1, 4.2003, 0.6, 9.2424, 0.3, 24.3687, 0.1, 100, 0])
    vln_one_sin_ran($t + 0.0250, 3.6, 65.4050, 0.16, :fm_index, 2.2203, 
      :reverb_amount, 0.0234, :noise_amount, 0.0100,
      :amp_env,
      [0, 0, 0.0833, 1, 4.1204, 0.6, 9.1667, 0.3, 24.3056, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.0300, 2.7000, 65.4050, 0.16, :fm_index, 1.7089, 
      :reverb_amount, 0.0208, :noise_amount, 0.0010,
      :amp_env,
      [0, 0, 0.1111, 1, 4.1470, 0.6, 9.1919, 0.3, 24.3266, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.0300, 2.7000, 130.8100, 0.16, :fm_index, 2.2948,
      :reverb_amount, 0.0269, :noise_amount, 0.0010,
      :amp_env,
      [0, 0, 0.1111, 1, 4.1470, 0.6, 9.1919, 0.3, 24.3266, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.0400, 3.6, 32.7025, 0.16, :fm_index, 1.7677, 
      :reverb_amount, 0.0288, :noise_amount, 0.0010,
      :amp_env,
      [0, 0, 0.0833, 1, 4.1204, 0.6, 9.1667, 0.3, 24.3056, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.0450, 1.8, 32.7025, 0.16, :fm_index, 1.9030, 
      :reverb_amount, 0.0209, :noise_amount, 0.0010,
      :amp_env,
      [0, 0, 0.1667, 1, 4.2003, 0.6, 9.2424, 0.3, 24.3687, 0.1, 100, 0])
    vln_one_sin_ran($t + 0.0500, 4, 65.4050, 0.0600, :fm_index, 2.2757, 
      :reverb_amount, 0.1, :amp_env, tap, :noise_amount, 0.0010)
    vln_one_sin_ran($t + 0.0500, 4, 65.4050, 0.0600, :fm_index, 2.2435, 
      :reverb_amount, 0.1, :amp_env, tap, :noise_amount, 0.0010)
    vln_one_sin_ran($t + 0.0600, 4, 32.7025, 0.0600, :fm_index, 1.9619, 
      :reverb_amount, 0.1, :amp_env, tap, :noise_amount, 0.0010)
    vln_one_sin_ran($t + 0.0650, 4, 65.4050, 0.0600, :fm_index, 2.0207, 
      :reverb_amount, 0.1, :amp_env, tap, :noise_amount, 0.0010)

    $t += 6.0
    vln_one_sin_ran($t + 0.0100, 0.9, 3135.9200, 0.16, :fm_index, 2.1204, 
      :reverb_amount, 0.0024, :noise_amount, 0.0100,
      :amp_env,
      [0, 0, 0.3333, 1, 4.3603, 0.6, 9.3939, 0.3, 24.4950, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.0100, 0.4500, 1567.96, 0.16, :fm_index, 2.0691, 
      :reverb_amount, 0.0025, :noise_amount, 0.0100,
      :amp_env,
      [0, 0, 0.6667, 1, 4.6801, 0.6, 9.6970, 0.3, 24.7475, 0.1, 100, 0])
    vln_one_sin_ran($t + 0.0200, 0.9, 6271.8400, 0.16, :fm_index, 2.2081, 
      :reverb_amount, 0.0022, :noise_amount, 0.0100,
      :amp_env,
      [0, 0, 0.3333, 1, 4.3603, 0.6, 9.3939, 0.3, 24.4950, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.0250, 0.9, 783.9800, 0.16, :fm_index, 1.8719, 
      :reverb_amount, 0.0022, :noise_amount, 0.0100,
      :amp_env,
      [0, 0, 0.3333, 1, 4.3603, 0.6, 9.3939, 0.3, 24.4950, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.0300, 0.2700, 783.9800, 0.16, :fm_index, 1.9705, 
      :reverb_amount, 0.0020, :noise_amount, 0.0100,
      :amp_env, 
      [0, 0, 1.1111, 1, 5.1066, 0.6, 10.1010, 0.3, 25.0842, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.0300, 0.6300, 1567.96, 0.16, :fm_index, 1.6778, 
      :reverb_amount, 0.0021, :noise_amount, 0.0100,
      :amp_env,
      [0, 0, 0.4762, 1, 4.4974, 0.6, 9.5238, 0.3, 24.6032, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.0400, 0.9, 391.9900, 0.16, :fm_index, 1.9558, 
      :reverb_amount, 0.0023, :noise_amount, 0.0100,
      :amp_env,
      [0, 0, 0.3333, 1, 4.3603, 0.6, 9.3939, 0.3, 24.4950, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.0450, 0.4500, 195.9950, 0.16, :fm_index, 2.1344, 
      :reverb_amount, 0.0027, :noise_amount, 0.0100,
      :amp_env,
      [0, 0, 0.6667, 1, 4.6801, 0.6, 9.6970, 0.3, 24.7475, 0.1, 100, 0])
    vln_one_sin_ran($t + 0.0500, 2, 783.9800, 0.16, 
      :reverb_amount, 0.0100, :amp_env, tap, :noise_amount, 0.0090) 
    vln_one_sin_ran($t + 0.0500, 1, 1567.9600, 0.16, 
      :reverb_amount, 0.0100, :amp_env, tap, :noise_amount, 0.0090)
    vln_one_sin_ran($t + 0.0600, 2, 391.9900, 0.16, 
      :reverb_amount, 0.0100, :amp_env, tap, :noise_amount, 0.0090)
    vln_one_sin_ran($t + 0.0650, 1, 783.9800, 0.16, 
      :reverb_amount, 0.0100, :amp_env, tap, :noise_amount, 0.0090)
    vln_one_sin_ran($t + 0.0700, 2, 195.9950, 0.16, 
      :reverb_amount, 0.0100, :amp_env, tap, :noise_amount, 0.0040)
    vln_one_sin_ran($t + 0.0700, 1, 1567.9600, 0.16, 
      :reverb_amount, 0.0100, :amp_env, tap, :noise_amount, 0.0040)
    vln_one_sin_ran($t + 0.0800, 1, 784.9800, 0.16, 
      :reverb_amount, 0.0100, :amp_env, tap, :noise_amount, 0.0040)
    vln_one_sin_ran($t + 0.0850, 2, 391.9900, 0.16, 
      :reverb_amount, 0.0100, :amp_env, tap, :noise_amount, 0.0040)

    $t += 6.0
    vln_one_sin_ran($t + 0.0100, 0.9, 97.9975, 0.1, :fm_index, 2.0885, 
      :reverb_amount, 0.0031, :noise_amount, 0.0100,
      :amp_env,
      [0, 0, 0.3333, 1, 4.3603, 0.6, 9.3939, 0.3, 24.4950, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.0100, 1.8, 48.9988, 0.1, :fm_index, 2.2269, 
      :reverb_amount, 0.0026, :noise_amount, 0.0100,
      :amp_env,
      [0, 0, 0.1667, 1, 4.2003, 0.6, 9.2424, 0.3, 24.3687, 0.1, 100, 0])
    vln_one_sin_ran($t + 0.0200, 0.9, 195.9950, 0.1, :fm_index, 2.0305, 
      :reverb_amount, 0.0032, :noise_amount, 0.0100,
      :amp_env,
      [0, 0, 0.3333, 1, 4.3603, 0.6, 9.3939, 0.3, 24.4950, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.0250, 0.9, 24.4994, 0.1, :fm_index, 2.4934, 
      :reverb_amount, 0.0025, :noise_amount, 0.0100,
      :amp_env, 
      [0, 0, 0.3333, 1, 4.3603, 0.6, 9.3939, 0.3, 24.4950, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.0300, 1.8, 97.9975, 0.1, :fm_index, 2.4039, 
      :reverb_amount, 0.0023, :noise_amount, 0.0400,
      :amp_env, 
      [0, 0, 0.1667, 1, 4.2003, 0.6, 9.2424, 0.3, 24.3687, 0.1, 100, 0])
    vln_one_sin_ran($t + 0.0300, 0.9, 195.9950, 0.1, :fm_index, 1.5159, 
      :reverb_amount, 0.0021, :noise_amount, 0.0400,
      :amp_env, 
      [0, 0, 0.3333, 1, 4.3603, 0.6, 9.3939, 0.3, 24.4950, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.0300, 0.9, 392.9900, 0.1, :fm_index, 2.2122, 
      :reverb_amount, 0.0028, :noise_amount, 0.0400,
      :amp_env, 
      [0, 0, 0.3333, 1, 4.3603, 0.6, 9.3939, 0.3, 24.4950, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.0300, 1.8, 784.9800, 0.1, :fm_index, 2.1574, 
      :reverb_amount, 0.0020, :noise_amount, 0.0400,
      :amp_env, 
      [0, 0, 0.1667, 1, 4.2003, 0.6, 9.2424, 0.3, 24.3687, 0.1, 100, 0])
    vln_one_sin_ran($t + 0.0300, 2.7000, 24.4994, 0.1, :fm_index, 2.1963, 
      :reverb_amount, 0.0031, :noise_amount, 0.0100,
      :amp_env, 
      [0, 0, 0.1111, 1, 4.1470, 0.6, 9.1919, 0.3, 24.3266, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.0300, 1.8, 48.9988, 0.1, :fm_index, 1.9761, 
      :reverb_amount, 0.0032, :noise_amount, 0.0100,
      :amp_env, 
      [0, 0, 0.1667, 1, 4.2003, 0.6, 9.2424, 0.3, 24.3687, 0.1, 100, 0])
    vln_one_sin_ran($t + 0.0400, 2.7000, 12.2497, 0.1, :fm_index, 1.5088, 
      :reverb_amount, 0.0021, :noise_amount, 0.0100,
      :amp_env, 
      [0, 0, 0.1111, 1, 4.1470, 0.6, 9.1919, 0.3, 24.3266, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.0450, 1.8, 6.1248, 0.1, :fm_index, 1.7384, 
      :reverb_amount, 0.0021, :noise_amount, 0.0100,
      :amp_env, 
      [0, 0, 0.1667, 1, 4.2003, 0.6, 9.2424, 0.3, 24.3687, 0.1, 100, 0])
    vln_one_sin_ran($t + 0.0500, 2, 24.4994, 0.1,
      :reverb_amount, 0.1, :amp_env, tap, :noise_amount, 0.0040)
    vln_one_sin_ran($t + 0.0500, 1, 48.9988, 0.1,
      :reverb_amount, 0.1, :amp_env, tap, :noise_amount, 0.0040)
    vln_one_sin_ran($t + 0.0600, 2, 12.2497, 0.1, 
      :reverb_amount, 0.1, :amp_env, tap, :noise_amount, 0.0040)
    vln_one_sin_ran($t + 0.0650, 1, 24.4994, 0.1,
      :reverb_amount, 0.1, :amp_env, tap, :noise_amount, 0.0040)
    vln_one_sin_ran($t + 0.0700, 2, 6.1248, 0.1, :fm_index, 1.2474,
      :reverb_amount, 0.1, :amp_env, tap, :noise_amount, 0.0040)
    vln_one_sin_ran($t + 0.0800, 1, 25.4994, 0.1, :fm_index, 1.1080, 
      :reverb_amount, 0.1, :amp_env, tap, :noise_amount, 0.0040)
    vln_one_sin_ran($t + 0.0850, 2, 12.2497, 0.1, :fm_index, 1.0859, 
      :reverb_amount, 0.1, :amp_env, tap, :noise_amount, 0.0040)
    vln_one_sin_ran($t + 0.0900, 4, 97.9975, 0.1, :fm_index, 2.4788, 
      :reverb_amount, 0.1, :amp_env, tap, :noise_amount, 0.0040)
    vln_one_sin_ran($t + 0.0900, 3, 48.9988, 0.1, :fm_index, 1.8980, 
      :reverb_amount, 0.1, :amp_env, tap, :noise_amount, 0.0040)
    vln_one_sin_ran($t + 0.0900, 3, 25.4994, 0.1, :fm_index, 2.1151, 
      :reverb_amount, 0.1, :amp_env, tap, :noise_amount, 0.0040)
    vln_one_sin_ran($t + 0.0950, 5, 12.2497, 0.1, :fm_index, 2.3224, 
      :reverb_amount, 0.1, :amp_env, tap, :noise_amount, 0.0040)

    $t += 6.0
    vln_one_sin_ran($t + 0.2100, 0.9, 123.4725, 0.1, :reverb_amount, 0.0031,
      :amp_env, 
      [0, 0, 0.3333, 1, 4.3603, 0.6, 9.3939, 0.3, 24.4950, 0.1, 100.0, 0],
      :noise_amount, 0.0100)
    vln_one_sin_ran($t + 0.2100, 1.8, 61.7363, 0.1, :reverb_amount, 0.0023,
      :amp_env, 
      [0, 0, 0.1667, 1, 4.2003, 0.6, 9.2424, 0.3, 24.3687, 0.1, 100, 0],
      :noise_amount, 0.0100)
    vln_one_sin_ran($t + 0.2200, 0.9, 246.9450, 0.1, :reverb_amount, 0.0023,
      :amp_env, 
      [0, 0, 0.3333, 1, 4.3603, 0.6, 9.3939, 0.3, 24.4950, 0.1, 100.0, 0],
      :noise_amount, 0.0100)
    vln_one_sin_ran($t + 0.2250, 0.9, 30.8681, 0.1, :reverb_amount, 0.0026,
      :amp_env, 
      [0, 0, 0.3333, 1, 4.3603, 0.6, 9.3939, 0.3, 24.4950, 0.1, 100.0, 0],
      :noise_amount, 0.0100)
    vln_one_sin_ran($t + 0.2300, 1.8, 123.4725, 0.1, :reverb_amount, 0.0027,
      :amp_env,
      [0, 0, 0.1667, 1, 4.2003, 0.6, 9.2424, 0.3, 24.3687, 0.1, 100, 0],
      :noise_amount, 0.0400)
    vln_one_sin_ran($t + 0.2300, 0.9, 246.9450, 0.1, :reverb_amount, 0.0026,
      :amp_env, 
      [0, 0, 0.3333, 1, 4.3603, 0.6, 9.3939, 0.3, 24.4950, 0.1, 100.0, 0],
      :noise_amount, 0.0400)
    vln_one_sin_ran($t + 0.2300, 0.9, 494.8900, 0.1, :reverb_amount, 0.0020,
      :amp_env, 
      [0, 0, 0.3333, 1, 4.3603, 0.6, 9.3939, 0.3, 24.4950, 0.1, 100.0, 0],
      :noise_amount, 0.0400)
    vln_one_sin_ran($t + 0.2300, 1.8, 988.7800, 0.1, :reverb_amount, 0.0025,
      :amp_env,
      [0, 0, 0.1667, 1, 4.2003, 0.6, 9.2424, 0.3, 24.3687, 0.1, 100, 0],
      :noise_amount, 0.0400)
    vln_one_sin_ran($t + 0.2300, 2.7000, 30.8681, 0.1, :reverb_amount, 0.0028,
      :amp_env, 
      [0, 0, 0.1111, 1, 4.1470, 0.6, 9.1919, 0.3, 24.3266, 0.1, 100.0, 0],
      :noise_amount, 0.0100)
    vln_one_sin_ran($t + 0.2300, 1.8, 61.7363, 0.1, :reverb_amount, 0.0023,
      :amp_env, 
      [0, 0, 0.1667, 1, 4.2003, 0.6, 9.2424, 0.3, 24.3687, 0.1, 100, 0],
      :noise_amount, 0.0100)
    vln_one_sin_ran($t + 0.2400, 2.7000, 15.4341, 0.1, :reverb_amount, 0.0030,
      :amp_env, 
      [0, 0, 0.1111, 1, 4.1470, 0.6, 9.1919, 0.3, 24.3266, 0.1, 100.0, 0],
      :noise_amount, 0.0100)
    vln_one_sin_ran($t + 0.2450, 1.8, 20.5788, 0.1, :reverb_amount, 0.0023,
      :amp_env, 
      [0, 0, 0.1667, 1, 4.2003, 0.6, 9.2424, 0.3, 24.3687, 0.1, 100, 0],
      :noise_amount, 0.0100)
    vln_one_sin_ran($t + 0.2500, 2, 30.8681, 0.1, 
      :reverb_amount, 0.1, :amp_env, tap, :noise_amount, 0.0090)
    vln_one_sin_ran($t + 0.2500, 1, 61.7363, 0.1,
      :reverb_amount, 0.1, :amp_env, tap, :noise_amount, 0.0090)
    vln_one_sin_ran($t + 0.2600, 2, 15.4341, 0.1,
      :reverb_amount, 0.1, :amp_env, tap, :noise_amount, 0.0090) 
    vln_one_sin_ran($t + 0.2650, 1, 30.8681, 0.1,
      :reverb_amount, 0.1, :amp_env, tap, :noise_amount, 0.0090)
    vln_one_sin_ran($t + 0.2710, 2, 30.8681, 0.1, 
      :reverb_amount, 0.1, :amp_env, tap, :noise_amount, 0.0040)
    vln_one_sin_ran($t + 0.2710, 1, 61.7363, 0.1, 
      :reverb_amount, 0.1, :amp_env, tap, :noise_amount, 0.0040)
    vln_one_sin_ran($t + 0.2810, 1, 31.8681, 0.1, 
      :reverb_amount, 0.1, :amp_env, tap, :noise_amount, 0.0040)
    vln_one_sin_ran($t + 0.2860, 2, 15.4341, 0.1, 
      :reverb_amount, 0.1, :amp_env, tap, :noise_amount, 0.0040)
    
    $t += 8.0
    yup = [0, 0, 1, 1, 100, 0]
    vln_one_sin_ran($t + 0.0100, 0.9, 3135.9200, 0.16, :fm_index, 1.7299, 
      :reverb_amount, 0.0026, :noise_amount, 0.0100,
      :amp_env, 
      [0, 0, 0.3333, 1, 4.3603, 0.6, 9.3939, 0.3, 24.4950, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.0100, 0.45, 1464.6987, 0.16, :fm_index, 1.9173, 
      :reverb_amount, 0.0027, :noise_amount, 0.0100,
      :amp_env, 
      [0, 0, 0.6667, 1, 4.6801, 0.6, 9.6970, 0.3, 24.7475, 0.1, 100, 0])
    vln_one_sin_ran($t + 0.0200, 0.9, 6714.0048, 0.16, :fm_index, 2.4604, 
      :reverb_amount, 0.0032, :noise_amount, 0.0100,
      :amp_env, 
      [0, 0, 0.3333, 1, 4.3603, 0.6, 9.3939, 0.3, 24.4950, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.0250, 0.9, 684.1190, 0.16, :fm_index, 1.9969, 
      :reverb_amount, 0.0021, :noise_amount, 0.0100,
      :amp_env, 
      [0, 0, 0.3333, 1, 4.3603, 0.6, 9.3939, 0.3, 24.4950, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.0300, 0.2700, 684.1190, 0.16, :fm_index, 2.0022, 
      :reverb_amount, 0.0026, :noise_amount, 0.0100,
      :amp_env,
      [0, 0, 1.1111, 1, 5.1066, 0.6, 10.1010, 0.3, 25.0842, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.0300, 0.63, 1464.6987, 0.16, :fm_index, 2.1058, 
      :reverb_amount, 0.0027, :noise_amount, 0.0100,
      :amp_env, 
      [0, 0, 0.4762, 1, 4.4974, 0.6, 9.5238, 0.3, 24.6032, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.0400, 0.9, 319.5325, 0.16, :fm_index, 2.2293, 
      :reverb_amount, 0.0029, :noise_amount, 0.0100,
      :amp_env, 
      [0, 0, 0.3333, 1, 4.3603, 0.6, 9.3939, 0.3, 24.4950, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.0450, 0.4500, 149.2445, 0.16, :fm_index, 1.5780, 
      :reverb_amount, 0.0025, :noise_amount, 0.0100,
      :amp_env, 
      [0, 0, 0.6667, 1, 4.6801, 0.6, 9.6970, 0.3, 24.7475, 0.1, 100, 0])
    vln_one_sin_ran($t + 0.0500, 1, 684.1190, 0.16, 
      :reverb_amount, 0.0100, :amp_env, yup, :noise_amount, 0.0090)
    vln_one_sin_ran($t + 0.0500, 1, 1464.6987, 0.16,
      :reverb_amount, 0.0100, :amp_env, yup, :noise_amount, 0.0090)
    vln_one_sin_ran($t + 0.0600, 1, 319.5325, 0.16,
      :reverb_amount, 0.0100, :amp_env, yup, :noise_amount, 0.0090)
    vln_one_sin_ran($t + 0.0650, 1, 684.1190, 0.16,
      :reverb_amount, 0.0100, :amp_env, yup, :noise_amount, 0.0090)
    vln_one_sin_ran($t + 0.0700, 1, 149.2445, 0.16,
      :reverb_amount, 0.0100, :amp_env, yup, :noise_amount, 0.0040)
    vln_one_sin_ran($t + 0.0700, 1, 1464.6987, 0.16,
      :reverb_amount, 0.0100, :amp_env, yup, :noise_amount, 0.0040)
    vln_one_sin_ran($t + 0.0800, 1, 561.6022, 0.16,
      :reverb_amount, 0.0100, :amp_env, yup, :noise_amount, 0.0040)
    vln_one_sin_ran($t + 0.0850, 1, 319.5325, 0.16,
      :reverb_amount, 0.0100, :amp_env, yup, :noise_amount, 0.0040)

    $t += 3.0
    vln_one_sin_ran($t + 0.0100, 0.9, 3135.9200, 0.16, :fm_index, 1.6329, 
      :reverb_amount, 0.0031, :noise_amount, 0.0100,
      :amp_env, 
      [0, 0, 0.3333, 1, 4.3603, 0.6, 9.3939, 0.3, 24.4950, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.0100, 0.45, 1810.5774, 0.16, :fm_index, 1.8298, 
      :reverb_amount, 0.0031, :noise_amount, 0.0100,
      :amp_env, 
      [0, 0, 0.6667, 1, 4.6801, 0.6, 9.6970, 0.3, 24.7475, 0.1, 100, 0])
    vln_one_sin_ran($t + 0.0200, 0.9, 5431.4135, 0.16, :fm_index, 2.1640, 
      :reverb_amount, 0.0022, :noise_amount, 0.0100,
      :amp_env, 
      [0, 0, 0.3333, 1, 4.3603, 0.6, 9.3939, 0.3, 24.4950, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.0250, 0.9, 1045.3680, 0.16, :fm_index, 1.6971, 
      :reverb_amount, 0.0032, :noise_amount, 0.0100,
      :amp_env, 
      [0, 0, 0.3333, 1, 4.3603, 0.6, 9.3939, 0.3, 24.4950, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.0300, 0.27, 1045.3680, 0.16, :fm_index, 2.4855, 
      :reverb_amount, 0.0028, :noise_amount, 0.0100,
      :amp_env, 
      [0, 0, 1.1111, 1, 5.1066, 0.6, 10.1010, 0.3, 25.0842, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.0300, 0.63, 1810.5774, 0.16, :fm_index, 2.1604, 
      :reverb_amount, 0.0020, :noise_amount, 0.0100,
      :amp_env, 
      [0, 0, 0.4762, 1, 4.4974, 0.6, 9.5238, 0.3, 24.6032, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.0400, 0.9, 603.5612, 0.16, :fm_index, 2.4204, 
      :reverb_amount, 0.0031, :noise_amount, 0.0100,
      :amp_env, 
      [0, 0, 0.3333, 1, 4.3603, 0.6, 9.3939, 0.3, 24.4950, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.0450, 0.4500, 348.4765, 0.16, :fm_index, 2.3918, 
      :reverb_amount, 0.0026, :noise_amount, 0.0100,
      :amp_env, 
      [0, 0, 0.6667, 1, 4.6801, 0.6, 9.6970, 0.3, 24.7475, 0.1, 100, 0])
    vln_one_sin_ran($t + 0.0460, 0.9, 201.1989, 0.16, :fm_index, 1.5205, 
      :reverb_amount, 0.0024, :noise_amount, 0.0100,
      :amp_env, 
      [0, 0, 0.3333, 1, 4.3603, 0.6, 9.3939, 0.3, 24.4950, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.0460, 0.9, 116.1656, 0.16, :fm_index, 2.3049, 
      :reverb_amount, 0.0028, :noise_amount, 0.0100,
      :amp_env, 
      [0, 0, 0.3333, 1, 4.3603, 0.6, 9.3939, 0.3, 24.4950, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.0500, 0.9, 3135.9200, 0.16, :fm_index, 2.4363, 
      :reverb_amount, 0.0021, :noise_amount, 0.0100,
      :amp_env, 
      [0, 0, 0.3333, 1, 4.3603, 0.6, 9.3939, 0.3, 24.4950, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.0500, 0.45, 1464.6987, 0.16, :fm_index, 2.3865, 
      :reverb_amount, 0.0027, :noise_amount, 0.0100,
      :amp_env, 
      [0, 0, 0.6667, 1, 4.6801, 0.6, 9.6970, 0.3, 24.7475, 0.1, 100, 0])
    vln_one_sin_ran($t + 0.0600, 0.9, 6714.0048, 0.16, :fm_index, 1.7354, 
      :reverb_amount, 0.0021, :noise_amount, 0.0100,
      :amp_env, 
      [0, 0, 0.3333, 1, 4.3603, 0.6, 9.3939, 0.3, 24.4950, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.0650, 0.9, 684.1190, 0.16, :fm_index, 1.8282, 
      :reverb_amount, 0.0025, :noise_amount, 0.0100,
      :amp_env, 
      [0, 0, 0.3333, 1, 4.3603, 0.6, 9.3939, 0.3, 24.4950, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.0700, 0.2700, 684.1190, 0.16, :fm_index, 2.3923, 
      :reverb_amount, 0.0025, :noise_amount, 0.0100,
      :amp_env, 
      [0, 0, 1.1111, 1, 5.1066, 0.6, 10.1010, 0.3, 25.0842, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.0700, 0.63, 1464.6987, 0.16, :fm_index, 2.2789, 
      :reverb_amount, 0.0028, :noise_amount, 0.0100,
      :amp_env, 
      [0, 0, 0.4762, 1, 4.4974, 0.6, 9.5238, 0.3, 24.6032, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.0800, 0.9, 319.5325, 0.16, :fm_index, 1.5438, 
      :reverb_amount, 0.0027, :noise_amount, 0.0100,
      :amp_env, 
      [0, 0, 0.3333, 1, 4.3603, 0.6, 9.3939, 0.3, 24.4950, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.0850, 0.4500, 149.2445, 0.16, :fm_index, 2.4210, 
      :reverb_amount, 0.0028, :noise_amount, 0.0100,
      :amp_env, 
      [0, 0, 0.6667, 1, 4.6801, 0.6, 9.6970, 0.3, 24.7475, 0.1, 100, 0])
    vln_one_sin_ran($t + 0.0860, 0.9, 69.7078, 0.16, :fm_index, 2.0288, 
      :reverb_amount, 0.0029, :noise_amount, 0.0100,
      :amp_env, 
      [0, 0, 0.3333, 1, 4.3603, 0.6, 9.3939, 0.3, 24.4950, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.0860, 0.9, 32.5585, 0.16, :fm_index, 1.8254, 
      :reverb_amount, 0.0028, :noise_amount, 0.0100,
      :amp_env, 
      [0, 0, 0.3333, 1, 4.3603, 0.6, 9.3939, 0.3, 24.4950, 0.1, 100.0, 0])

    $t += 3.0
    $reverb_amount = 0.0
    $noise_amount = 0.01
    vln_one_sin_ran($t + 0.0500, 0.9, 3135.9200, 0.16, :fm_index, 1.7334,
      :amp_env, 
      [0, 0, 0.3333, 1, 4.3603, 0.6, 9.3939, 0.3, 24.4950, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.0500, 0.4500, 1810.5774, 0.16, :fm_index, 2.3629,
      :amp_env, 
      [0, 0, 0.6667, 1, 4.6801, 0.6, 9.6970, 0.3, 24.7475, 0.1, 100, 0])
    vln_one_sin_ran($t + 0.0600, 0.9, 5431.4135, 0.16, :fm_index, 2.2744,
      :amp_env, 
      [0, 0, 0.3333, 1, 4.3603, 0.6, 9.3939, 0.3, 24.4950, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.0650, 0.9, 1045.3680, 0.16, :fm_index, 1.8722,
      :amp_env, 
      [0, 0, 0.3333, 1, 4.3603, 0.6, 9.3939, 0.3, 24.4950, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.1100, 0.2700, 1045.3680, 0.16, :fm_index, 2.3139,
      :amp_env, 
      [0, 0, 1.1111, 1, 5.1066, 0.6, 10.101, 0.3, 25.0842, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.1100, 0.6300, 1810.5774, 0.16, :fm_index, 1.6216,
      :amp_env, 
      [0, 0, 0.4762, 1, 4.4974, 0.6, 9.5238, 0.3, 24.6032, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.1200, 0.9, 603.5612, 0.16, :fm_index, 1.5308,
      :amp_env, 
      [0, 0, 0.3333, 1, 4.3603, 0.6, 9.3939, 0.3, 24.4950, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.1650, 0.4500, 348.4765, 0.16, :fm_index, 2.0346,
      :amp_env, 
      [0, 0, 0.6667, 1, 4.6801, 0.6, 9.6970, 0.3, 24.7475, 0.1, 100, 0])
    vln_one_sin_ran($t + 0.1660, 0.9, 201.1989, 0.16, :fm_index, 1.8176,
      :amp_env, 
      [0, 0, 0.3333, 1, 4.3603, 0.6, 9.3939, 0.3, 24.4950, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.1660, 0.9, 116.1656, 0.16, :fm_index, 1.7145,
      :amp_env, 
      [0, 0, 0.3333, 1, 4.3603, 0.6, 9.3939, 0.3, 24.4950, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.1700, 0.9, 3135.9200, 0.16, :fm_index, 2.4459,
      :amp_env, 
      [0, 0, 0.3333, 1, 4.3603, 0.6, 9.3939, 0.3, 24.4950, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.1700, 0.4500, 1464.6987, 0.16, :fm_index, 2.4644,
      :amp_env, 
      [0, 0, 0.6667, 1, 4.6801, 0.6, 9.6970, 0.3, 24.7475, 0.1, 100, 0])
    vln_one_sin_ran($t + 0.1800, 0.9, 6714.0048, 0.16, :fm_index, 1.9985,
      :amp_env, 
      [0, 0, 0.3333, 1, 4.3603, 0.6, 9.3939, 0.3, 24.4950, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.1850, 0.9, 684.1190, 0.16, :fm_index, 2.4542,
      :amp_env, 
      [0, 0, 0.3333, 1, 4.3603, 0.6, 9.3939, 0.3, 24.4950, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.2900, 0.2700, 684.1190, 0.16, :fm_index, 2.3391,
      :amp_env, 
      [0, 0, 1.1111, 1, 5.1066, 0.6, 10.101, 0.3, 25.0842, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.2900, 0.6300, 1464.6987, 0.16, :fm_index, 1.5138,
      :amp_env, 
      [0, 0, 0.4762, 1, 4.4974, 0.6, 9.5238, 0.3, 24.6032, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.3, 0.9, 319.5325, 0.16, :fm_index, 1.5440,
      :amp_env, 
      [0, 0, 0.3333, 1, 4.3603, 0.6, 9.3939, 0.3, 24.4950, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.3050, 0.4500, 149.2445, 0.16, :fm_index, 2.2283,
      :amp_env, 
      [0, 0, 0.6667, 1, 4.6801, 0.6, 9.6970, 0.3, 24.7475, 0.1, 100, 0])
    vln_one_sin_ran($t + 0.3060, 0.9, 69.7078, 0.16, :fm_index, 1.9498,
      :amp_env, 
      [0, 0, 0.3333, 1, 4.3603, 0.6, 9.3939, 0.3, 24.4950, 0.1, 100.0, 0])
    vln_one_sin_ran($t + 0.3060, 0.9, 32.5585, 0.16, :fm_index, 2.2943,
      :amp_env, 
      [0, 0, 0.3333, 1, 4.3603, 0.6, 9.3939, 0.3, 24.4950, 0.1, 100.0, 0])

    $t += 3.0
    restore_fm_violin_defaults()
    $amp_env = metalamp
    $glissando_amount = 0.0
    $reverb_amount = 0.01
    $fm1_rat = 2.718
    $fm2_rat = 1.414
    $fm3_rat = 3.141
    vln_one_sin($t + 0.2600, 1.2, 355.5416, 0.16, :fm_index, 2.0375)
    vln_one_sin($t + 0.2600, 1.5, 354.8319, 0.16, :fm_index, 1.8744)
    vln_one_sin($t + 0.2603, 0.9, 356.2527, 0.16, :fm_index, 1.8743)
    vln_one_sin($t + 0.2605, 0.9, 409.2356, 0.16, :fm_index, 2.0808)
    vln_one_sin($t + 0.2605, 2.1, 410.0541, 0.16, :fm_index, 1.9219)
    vln_one_sin($t + 0.2608, 1.2, 130.8100, 0.16, :fm_index, 1.5746)
    vln_one_sin($t + 0.2613, 3, 130.2883, 0.16, :fm_index, 2.3771)
    vln_one_sin($t + 0.2615, 0.9, 130.2883, 0.16, :fm_index, 1.7765)
    vln_one_sin($t + 0.2615, 2.1, 130.5489, 0.2600, :fm_index, 1.6485)
    vln_one_sin($t + 0.2625, 2, 130.8100, 0.16, :fm_index, 2.1416)
    vln_one_sin($t + 0.2633, 2, 130.5488, 0.16, :fm_index, 2.0883)

    $t += 4.0
    newf = [0, 0, 0.5, 1, 5, 1, 10, 0.5, 15, 0.25, 35, 0.1, 100, 0]
    $noise_amount = 0.0001
    $emp_env = newf
    $reverb_amount = 0.01
    vln_one_sin_ran($t + 0.2605, 0.8, 523.2400, 0.16, :fm_index, 2.3056)
    vln_one_sin_ran($t + 0.2605, 1, 247.1611, 0.16, :fm_index, 1.6308)
    vln_one_sin_ran($t + 0.2610, 0.6, 1107.6991, 0.16, :fm_index, 1.9364)
    vln_one_sin_ran($t + 0.2613, 2, 116.7506, 0.16, :fm_index, 2.3740)
    vln_one_sin_ran($t + 0.2615, 0.6, 116.7506, 0.16, :fm_index, 1.8374)
    vln_one_sin_ran($t + 0.2615, 1.4, 247.1611, 0.16, :fm_index, 1.7250)
    vln_one_sin_ran($t + 0.2620, 0.6, 55.1491, 0.16, :fm_index, 1.5495)
    vln_one_sin_ran($t + 0.2623, 1, 26.0506, 0.16, :fm_index, 1.7235)
    vln_one_sin_ran($t + 0.2623, 2, 12.3054, 0.16, :fm_index, 1.8818)
    vln_one_sin_ran($t + 0.2623, 2, 5.8127, 0.16, :fm_index, 1.9537)
    vln_one_sin_ran($t + 0.2625, 2, 523.2400, 0.16, :fm_index, 2.1593)
    vln_one_sin_ran($t + 0.2625, 1, 256.2390, 0.16, :fm_index, 1.9851)
    vln_one_sin_ran($t + 0.2630, 2, 1068.4561, 0.16, :fm_index, 1.8015)
    vln_one_sin_ran($t + 0.2633, 2, 125.4843, 0.16, :fm_index, 1.6161)
    vln_one_sin_ran($t + 0.2635, 0.6, 125.4843, 0.16, :fm_index, 2.2767)
    vln_one_sin_ran($t + 0.2635, 1.4, 256.2390, 0.16, :fm_index, 2.0835)
    vln_one_sin_ran($t + 0.2640, 0.4, 61.4517, 0.16, :fm_index, 1.5310)
    vln_one_sin_ran($t + 0.2643, 1, 30.0939, 0.16, :fm_index, 1.5803)
    vln_one_sin_ran($t + 0.2643, 2, 14.7374, 0.16, :fm_index, 1.9586)
    vln_one_sin_ran($t + 0.2643, 2, 7.2172, 0.16, :fm_index, 1.7270)
    vln_one_sin_ran($t + 0.2645, 6, 28.4710, 0.16, :fm_index, 1.5983)
    vln_one_sin_ran($t + 0.2648, 10, 25.6239, 0.16, :fm_index, 1.7285)
    vln_one_sin_ran($t + 0.2648, 8, 21.3532, 0.16, :fm_index, 1.7955)
    vln_one_sin_ran($t + 0.2648, 8, 17.0826, 0.16, :fm_index, 2.0866)

    $t += 4.0
    $reverb_amount = 0.001
    $noise_amount = 0.004
    $fm1_rat = 3.141
    $fm2_rat = 1.414
    $fm3_rat = 2.718
    vln_one_sin_ran($t + 0.2600, 1.6, 1643.4968, 0.16, :fm_index, 2.1104)
    vln_one_sin_ran($t + 0.2600, 2, 1643.4968, 0.16, :fm_index, 1.5191)
    vln_one_sin_ran($t + 0.2603, 1.2, 1643.4968, 0.16, :fm_index, 2.0478)
    vln_one_sin_ran($t + 0.2603, 4, 1643.4968, 0.16, :fm_index, 2.0473)
    vln_one_sin_ran($t + 0.2605, 1.2, 1422.1663, 0.16, :fm_index, 1.9845)
    vln_one_sin_ran($t + 0.2605, 2.8, 1422.1663, 0.16, :fm_index, 2.0429)
    vln_one_sin_ran($t + 0.2605, 1.2, 1422.1663, 0.16, :fm_index, 1.6184)
    vln_one_sin_ran($t + 0.2608, 1.6, 523.2400, 0.16, :fm_index, 2.3908)
    vln_one_sin_ran($t + 0.2608, 2, 523.2400, 0.16, :fm_index, 1.6733)
    vln_one_sin_ran($t + 0.2610, 1.2, 523.2400, 0.16, :fm_index, 2.0431)
    vln_one_sin_ran($t + 0.2610, 4, 523.2400, 0.16, :fm_index, 1.7430)
    vln_one_sin_ran($t + 0.2613, 1.2, 523.2400, 0.16, :fm_index, 2.2030)
    vln_one_sin_ran($t + 0.2613, 2.8, 523.2400, 0.16, :fm_index, 2.0149)
    vln_one_sin_ran($t + 0.2615, 1.2, 523.2400, 0.16, :fm_index, 2.2310)
    vln_one_sin_ran($t + 0.2615, 2, 523.2400, 0.16, :fm_index, 2.1625)
    vln_one_sin_ran($t + 0.2618, 4, 523.2400, 0.16, :fm_index, 2.0000)
    vln_one_sin_ran($t + 0.2618, 4, 523.2400, 0.16, :fm_index, 2.2034)
    vln_one_sin_ran($t + 0.2620, 3, 523.2400, 0.16, :fm_index, 2.0186)
    vln_one_sin_ran($t + 0.2620, 1.5, 523.2400, 0.16, :fm_index, 2.1373)
    vln_one_sin_ran($t + 0.2623, 3, 523.2400, 0.16, :fm_index, 1.9046)
    vln_one_sin_ran($t + 0.2623, 3, 523.2400, 0.16, :fm_index, 2.1834)
    vln_one_sin_ran($t + 0.2625, 1.2, 523.2400, 0.16, :fm_index, 1.8266)
    vln_one_sin_ran($t + 0.2625, 2.8, 523.2400, 0.16, :fm_index, 1.5937)
    vln_one_sin_ran($t + 0.2628, 0.8, 523.2400, 0.16, :fm_index, 1.9762)
    vln_one_sin_ran($t + 0.2628, 2, 523.2400, 0.16, :fm_index, 1.8954)
    vln_one_sin_ran($t + 0.2630, 4, 523.2400, 0.16, :fm_index, 2.3302)
    vln_one_sin_ran($t + 0.2630, 4, 523.2400, 0.16, :fm_index, 2.4949)

    $t += 4.0
    $fm1_rat = 3.414
    $fm2_rat = 1.414
    $fm3_rat = 2.718
    vln_one_sin_ran($t + 0.2600, 1.6, 821.7484, 0.16, :fm_index, 2.4793)
    vln_one_sin_ran($t + 0.2600, 2, 821.7484, 0.16, :fm_index, 2.4789)
    vln_one_sin_ran($t + 0.2603, 1.2, 821.7484, 0.16, :fm_index, 2.0827)
    vln_one_sin_ran($t + 0.2603, 4, 821.7484, 0.16, :fm_index, 2.4769)
    vln_one_sin_ran($t + 0.2605, 1.2, 711.0832, 0.16, :fm_index, 2.4094)
    vln_one_sin_ran($t + 0.2605, 2.8, 711.0832, 0.16, :fm_index, 2.4031)
    vln_one_sin_ran($t + 0.2605, 1.2, 711.0832, 0.16, :fm_index, 2.1428)
    vln_one_sin_ran($t + 0.2608, 1.6, 261.6200, 0.16, :fm_index, 2.3129)
    vln_one_sin_ran($t + 0.2608, 2, 261.6200, 0.16, :fm_index, 2.3488)
    vln_one_sin_ran($t + 0.2610, 1.2, 261.6200, 0.16, :fm_index, 2.1466)
    vln_one_sin_ran($t + 0.2610, 4, 261.6200, 0.16, :fm_index, 1.6938)
    vln_one_sin_ran($t + 0.2613, 1.2, 261.6200, 0.16, :fm_index, 2.1287)
    vln_one_sin_ran($t + 0.2613, 2.8, 261.6200, 0.16, :fm_index, 2.1917)
    vln_one_sin_ran($t + 0.2615, 1.2, 261.6200, 0.16, :fm_index, 2.3583)
    vln_one_sin_ran($t + 0.2615, 2, 261.6200, 0.16, :fm_index, 1.8368)
    vln_one_sin_ran($t + 0.2618, 4, 261.6200, 0.16, :fm_index, 1.5107)
    vln_one_sin_ran($t + 0.2618, 4, 261.6200, 0.16, :fm_index, 1.6218)
    vln_one_sin_ran($t + 0.2620, 3, 261.6200, 0.16, :fm_index, 1.9041)
    vln_one_sin_ran($t + 0.2620, 1.5, 261.6200, 0.16, :fm_index, 1.5748)
    vln_one_sin_ran($t + 0.2623, 3, 261.6200, 0.16, :fm_index, 1.9339)
    vln_one_sin_ran($t + 0.2623, 3, 261.6200, 0.16, :fm_index, 2.0489)
    vln_one_sin_ran($t + 0.2625, 1.2, 261.6200, 0.16, :fm_index, 2.0888)
    vln_one_sin_ran($t + 0.2625, 2.8, 261.6200, 0.16, :fm_index, 1.7306)
    vln_one_sin_ran($t + 0.2628, 0.8, 261.6200, 0.16, :fm_index, 2.3257)
    vln_one_sin_ran($t + 0.2628, 2, 261.6200, 0.16, :fm_index, 2.4755)
    vln_one_sin_ran($t + 0.2630, 4, 261.6200, 0.16, :fm_index, 1.9459)
    vln_one_sin_ran($t + 0.2630, 4, 261.6200, 0.16, :fm_index, 1.5782)
    
    $t += 4.0
    $fm1_rat = 3.414
    $fm2_rat = 1.414
    $fm3_rat = 2.718
    vln_one_sin_ran($t + 0.2600, 1.6, 3286.9937, 0.16, :fm_index, 1.6655)
    vln_one_sin_ran($t + 0.2600, 2, 3286.9937, 0.16, :fm_index, 1.9356)
    vln_one_sin_ran($t + 0.2603, 1.2, 3286.9937, 0.16, :fm_index, 1.5665)
    vln_one_sin_ran($t + 0.2603, 4, 3286.9937, 0.16, :fm_index, 1.6701)
    vln_one_sin_ran($t + 0.2605, 1.2, 2844.3326, 0.16, :fm_index, 2.3273)
    vln_one_sin_ran($t + 0.2605, 2.8, 2844.3326, 0.16, :fm_index, 1.5520)
    vln_one_sin_ran($t + 0.2605, 1.2, 2844.3326, 0.16, :fm_index, 2.4104)
    vln_one_sin_ran($t + 0.2608, 1.6, 1046.4800, 0.16, :fm_index, 2.1075)
    vln_one_sin_ran($t + 0.2608, 2, 1046.4800, 0.16, :fm_index, 1.7004)
    vln_one_sin_ran($t + 0.2610, 1.2, 1046.4800, 0.16, :fm_index, 1.6502)
    vln_one_sin_ran($t + 0.2610, 4, 1046.4800, 0.16, :fm_index, 2.4591)
    vln_one_sin_ran($t + 0.2613, 1.2, 1046.4800, 0.16, :fm_index, 2.1491)
    vln_one_sin_ran($t + 0.2613, 2.8, 1046.4800, 0.16, :fm_index, 2.1594)
    vln_one_sin_ran($t + 0.2615, 1.2, 1046.4800, 0.16, :fm_index, 2.4783)
    vln_one_sin_ran($t + 0.2615, 2, 1046.4800, 0.16, :fm_index, 2.2080)
    vln_one_sin_ran($t + 0.2618, 4, 1046.4800, 0.16, :fm_index, 1.5844)
    vln_one_sin_ran($t + 0.2618, 4, 1046.4800, 0.16, :fm_index, 1.5440)
    vln_one_sin_ran($t + 0.2620, 3, 1046.4800, 0.16, :fm_index, 1.9857)
    vln_one_sin_ran($t + 0.2620, 1.5, 1046.4800, 0.16, :fm_index, 1.5165)
    vln_one_sin_ran($t + 0.2623, 3, 1046.4800, 0.16, :fm_index, 1.8309)
    vln_one_sin_ran($t + 0.2623, 3, 1046.4800, 0.16, :fm_index, 2.1236)
    vln_one_sin_ran($t + 0.2625, 1.2, 1046.4800, 0.1, :fm_index, 2.4074)
    vln_one_sin_ran($t + 0.2625, 2.8, 1046.4800, 0.1, :fm_index, 1.6315)
    vln_one_sin_ran($t + 0.2628, 0.8, 1046.4800, 0.1, :fm_index, 1.8061)
    vln_one_sin_ran($t + 0.2628, 2, 1046.4800, 0.1, :fm_index, 2.3664)
    vln_one_sin_ran($t + 0.2630, 4, 1046.4800, 0.1, :fm_index, 2.2490)
    vln_one_sin_ran($t + 0.2630, 4, 1046.4800, 0.1, :fm_index, 2.4081)

    $t += 4.0
    $reverb_amount = 0.01
    vln_one_sin_ran($t + 0.2600, 1.6, 1643.4968, 0.16, :fm_index, 1.9284)
    vln_one_sin_ran($t + 0.2600, 2, 1643.4968, 0.16, :fm_index, 2.2171)
    vln_one_sin_ran($t + 0.2603, 1.2, 1643.4968, 0.16, :fm_index, 2.2272)
    vln_one_sin_ran($t + 0.2603, 4, 1643.4968, 0.16, :fm_index, 1.5677)
    vln_one_sin_ran($t + 0.2605, 1.2, 1422.1663, 0.16, :fm_index, 2.0476)
    vln_one_sin_ran($t + 0.2605, 2.8, 1422.1663, 0.16, :fm_index, 2.3289)
    vln_one_sin_ran($t + 0.2605, 1.2, 1422.1663, 0.16, :fm_index, 2.0269)
    vln_one_sin_ran($t + 0.2608, 1.6, 523.2400, 0.16, :fm_index, 1.7767)
    vln_one_sin_ran($t + 0.2608, 2, 523.2400, 0.16, :fm_index, 1.8117)
    vln_one_sin_ran($t + 0.2610, 1.2, 523.2400, 0.16, :fm_index, 1.5694)
    vln_one_sin_ran($t + 0.2610, 4, 523.2400, 0.16, :fm_index, 1.6869)
    vln_one_sin_ran($t + 0.2613, 1.2, 523.2400, 0.16, :fm_index, 1.9340)
    vln_one_sin_ran($t + 0.2613, 2.8, 523.2400, 0.16, :fm_index, 2.3986)
    vln_one_sin_ran($t + 0.2615, 1.2, 523.2400, 0.16, :fm_index, 2.4593)
    vln_one_sin_ran($t + 0.2615, 2, 523.2400, 0.16, :fm_index, 2.3430)
    vln_one_sin_ran($t + 0.2618, 4, 523.2400, 0.16, :fm_index, 2.2650)
    vln_one_sin_ran($t + 0.2618, 4, 523.2400, 0.16, :fm_index, 2.3015)
    vln_one_sin_ran($t + 0.2620, 3, 523.2400, 0.16, :fm_index, 1.9909)
    vln_one_sin_ran($t + 0.2620, 1.5, 523.2400, 0.16, :fm_index, 2.3916)
    vln_one_sin_ran($t + 0.2623, 3, 523.2400, 0.16, :fm_index, 2.0401)
    vln_one_sin_ran($t + 0.2623, 3, 523.2400, 0.16, :fm_index, 1.8484)
    vln_one_sin_ran($t + 0.2625, 1.2, 523.2400, 0.16, :fm_index, 2.3138)
    vln_one_sin_ran($t + 0.2625, 2.8, 523.2400, 0.16, :fm_index, 1.6295)
    vln_one_sin_ran($t + 0.2628, 0.8, 523.2400, 0.16, :fm_index, 2.2344)
    vln_one_sin_ran($t + 0.2628, 2, 523.2400, 0.16, :fm_index, 1.8423)
    vln_one_sin_ran($t + 0.2630, 4, 523.2400, 0.16, :fm_index, 2.2086)
    vln_one_sin_ran($t + 0.2630, 4, 523.2400, 0.16, :fm_index, 2.3130)
    
    $t += 4.0
    $noise_amount = 0.0001
    $fm1_rat = 2.718
    $fm2_rat = 1.141
    $fm3_rat = 3.141
    $reverb_amount = 0.01
    vln_one_sin_ran($t + 0.2605, 0.8, 523.2400, 0.16, :fm_index, 2.0123)
    vln_one_sin_ran($t + 0.2605, 1, 493.8728, 0.16, :fm_index, 2.1176)
    vln_one_sin_ran($t + 0.2610, 0.6, 554.3535, 0.16, :fm_index, 1.9163)
    vln_one_sin_ran($t + 0.2613, 2, 466.1539, 0.16, :fm_index, 1.5048)
    vln_one_sin_ran($t + 0.2615, 0.6, 466.1539, 0.16, :fm_index, 1.5242)
    vln_one_sin_ran($t + 0.2615, 1.4, 493.8728, 0.16, :fm_index, 1.9509)
    vln_one_sin_ran($t + 0.2620, 0.6, 439.9907, 0.16, :fm_index, 2.2131)
    vln_one_sin_ran($t + 0.2623, 1, 415.2959, 0.16, :fm_index, 1.7326)
    vln_one_sin_ran($t + 0.2623, 2, 391.9871, 0.16, :fm_index, 1.9936)
    vln_one_sin_ran($t + 0.2623, 2, 369.9866, 0.16, :fm_index, 2.1103)
    vln_one_sin_ran($t + 0.2625, 2, 523.2400, 0.16, :fm_index, 1.6206)
    vln_one_sin_ran($t + 0.2625, 1, 522.7173, 0.16, :fm_index, 1.8598)
    vln_one_sin_ran($t + 0.2630, 2, 523.7632, 0.16, :fm_index, 1.8015)
    vln_one_sin_ran($t + 0.2633, 2, 522.1951, 0.16, :fm_index, 2.3575)
    vln_one_sin_ran($t + 0.2635, 0.6, 522.1951, 0.16, :fm_index, 1.5010)
    vln_one_sin_ran($t + 0.2635, 1.4, 522.7173, 0.16, :fm_index, 2.4075)
    vln_one_sin_ran($t + 0.2640, 0.4, 521.6734, 0.16, :fm_index, 2.0721)
    vln_one_sin_ran($t + 0.2643, 1, 521.1523, 0.16, :fm_index, 2.0433)
    vln_one_sin_ran($t + 0.2643, 2, 520.6316, 0.16, :fm_index, 1.9788)
    vln_one_sin_ran($t + 0.2643, 2, 520.1115, 0.16, :fm_index, 1.6770)

    $t += 8.0
    $noise_amount = 0.004
    vln_one_sin_ran($t + 0.2600, 0.8, 1046.4800, 0.16, :fm_index, 1.5610)
    vln_one_sin_ran($t + 0.2600, 1, 1044.3912, 0.16, :fm_index, 2.3514)
    vln_one_sin_ran($t + 0.2603, 0.6, 1048.5730, 0.16, :fm_index, 1.9958)
    vln_one_sin_ran($t + 0.2603, 2, 1042.3066, 0.16, :fm_index, 1.9654)
    vln_one_sin_ran($t + 0.2605, 0.6, 1042.3066, 0.16, :fm_index, 1.5285)
    vln_one_sin_ran($t + 0.2605, 1.4, 1044.3912, 0.16, :fm_index, 1.8881)
    vln_one_sin_ran($t + 0.2608, 0.6, 1040.2262, 0.16, :fm_index, 1.8682)
    vln_one_sin_ran($t + 0.2608, 0.8, 523.2400, 0.16, :fm_index, 1.8296)
    vln_one_sin_ran($t + 0.2610, 1, 522.1956, 0.16, :fm_index, 2.1899)
    vln_one_sin_ran($t + 0.2610, 0.6, 524.2865, 0.16, :fm_index, 1.9614)
    vln_one_sin_ran($t + 0.2613, 2, 521.1533, 0.16, :fm_index, 1.7483)
    vln_one_sin_ran($t + 0.2615, 0.6, 521.1533, 0.16, :fm_index, 1.8717)
    vln_one_sin_ran($t + 0.2615, 1.4, 522.1956, 0.16, :fm_index, 1.5619)
    vln_one_sin_ran($t + 0.2620, 0.6, 520.1131, 0.16, :fm_index, 2.4331)
    vln_one_sin_ran($t + 0.2623, 1, 519.0749, 0.16, :fm_index, 2.4153)
    vln_one_sin_ran($t + 0.2623, 2, 518.0388, 0.16, :fm_index, 1.5477)
    vln_one_sin_ran($t + 0.2623, 2, 517.0048, 0.16, :fm_index, 1.9956)
    vln_one_sin_ran($t + 0.2625, 2, 523.2400, 0.16, :fm_index, 1.8111)
    vln_one_sin_ran($t + 0.2625, 1, 522.7173, 0.16, :fm_index, 2.4820)
    vln_one_sin_ran($t + 0.2630, 2, 523.7632, 0.16, :fm_index, 1.5744)
    vln_one_sin_ran($t + 0.2633, 2, 522.1951, 0.16, :fm_index, 1.9950)
    vln_one_sin_ran($t + 0.2635, 0.6, 522.1951, 0.16, :fm_index, 1.9792)
    vln_one_sin_ran($t + 0.2635, 1.4, 522.7173, 0.16, :fm_index, 1.7415)
    vln_one_sin_ran($t + 0.2640, 0.4, 521.6734, 0.16, :fm_index, 2.0884)
    vln_one_sin_ran($t + 0.2643, 1, 521.1523, 0.16, :fm_index, 2.3605)
    vln_one_sin_ran($t + 0.2643, 2, 520.6316, 0.16, :fm_index, 1.7817)
    vln_one_sin_ran($t + 0.2643, 2, 520.1115, 0.16, :fm_index, 2.0283)

    $t += 4.0
    $reverb_amount = 0.1
    $fm1_rat = 2.718
    $fm2_rat = 1.414
    $fm3_rat = 3.141
    $glissando_amount = 0.0
    vln_one_sin_ran($t + 0.2600, 1.6, 177.7708, 0.16, :fm_index, 1.6447)
    vln_one_sin_ran($t + 0.2600, 2, 177.7708, 0.16, :fm_index, 2.4875)
    vln_one_sin_ran($t + 0.2603, 1.2, 177.7708, 0.16, :fm_index, 1.6126)
    vln_one_sin_ran($t + 0.2603, 4, 177.7708, 0.16, :fm_index, 2.3122)
    vln_one_sin_ran($t + 0.2605, 1.2, 205.4371, 0.16, :fm_index, 2.4116)
    vln_one_sin_ran($t + 0.2605, 2.8, 205.4371, 0.16, :fm_index, 1.5337)
    vln_one_sin_ran($t + 0.2608, 1.2, 205.4371, 0.16, :fm_index, 2.0307)
    vln_one_sin_ran($t + 0.2608, 1.6, 65.4050, 0.16, :fm_index, 2.2341)
    vln_one_sin_ran($t + 0.2610, 2, 65.4050, 0.16, :fm_index, 2.4683)
    vln_one_sin_ran($t + 0.2610, 1.2, 65.4050, 0.16, :fm_index, 2.0643)
    vln_one_sin_ran($t + 0.2613, 4, 65.4050, 0.16, :fm_index, 2.1925)
    vln_one_sin_ran($t + 0.2615, 1.2, 65.4050, 0.16, :fm_index, 2.1325)
    vln_one_sin_ran($t + 0.2615, 2.8, 65.4050, 0.16, :fm_index, 1.5847)
    vln_one_sin_ran($t + 0.2620, 1.2, 65.4050, 0.16, :fm_index, 1.8781)
    vln_one_sin_ran($t + 0.2623, 2, 65.4050, 0.16, :fm_index, 2.0283)
    vln_one_sin_ran($t + 0.2623, 4, 65.4050, 0.16, :fm_index, 2.4739)
    vln_one_sin_ran($t + 0.2623, 4, 65.4050, 0.16, :fm_index, 2.2333)
    vln_one_sin_ran($t + 0.2625, 2, 65.4050, 0.16, :fm_index, 2.2194)
    vln_one_sin_ran($t + 0.2625, 1, 65.4050, 0.16, :fm_index, 2.4491)
    vln_one_sin_ran($t + 0.2630, 2, 65.4050, 0.16, :fm_index, 1.5672)
    vln_one_sin_ran($t + 0.2633, 2, 65.4050, 0.16, :fm_index, 2.3254)
    vln_one_sin_ran($t + 0.2635, 1.2, 65.4050, 0.16, :fm_index, 1.8302)
    vln_one_sin_ran($t + 0.2635, 2.8, 65.4050, 0.16, :fm_index, 1.9201)
    vln_one_sin_ran($t + 0.2640, 0.8, 65.4050, 0.16, :fm_index, 1.9164)
    vln_one_sin_ran($t + 0.2643, 2, 65.4050, 0.16, :fm_index, 1.9483)
    vln_one_sin_ran($t + 0.2643, 4, 65.4050, 0.16, :fm_index, 2.4247)
    vln_one_sin_ran($t + 0.2643, 4, 65.4050, 0.16, :fm_index, 2.0419)

    $t += 4.0
    $fm1_rat = 2.718
    $fm2_rat = 4.414
    $fm3_rat = 3.141
    vln_one_sin_ran($t + 0.2600, 1.6, 88.8854, 0.16, :fm_index, 2.2832)
    vln_one_sin_ran($t + 0.2600, 2, 88.8854, 0.16, :fm_index, 1.6588)
    vln_one_sin_ran($t + 0.2603, 1.2, 88.8854, 0.16, :fm_index, 2.2392)
    vln_one_sin_ran($t + 0.2603, 4, 88.8854, 0.16, :fm_index, 1.7354)
    vln_one_sin_ran($t + 0.2605, 1.2, 102.7186, 0.16, :fm_index, 1.6692)
    vln_one_sin_ran($t + 0.2605, 2.8, 102.7186, 0.16, :fm_index, 2.1518)
    vln_one_sin_ran($t + 0.2608, 1.2, 102.7186, 0.16, :fm_index, 2.2439)
    vln_one_sin_ran($t + 0.2608, 1.6, 32.7025, 0.16, :fm_index, 2.1665)
    vln_one_sin_ran($t + 0.2610, 2, 32.7025, 0.16, :fm_index, 1.7947)
    vln_one_sin_ran($t + 0.2610, 1.2, 32.7025, 0.16, :fm_index, 2.0740)
    vln_one_sin_ran($t + 0.2613, 4, 32.7025, 0.16, :fm_index, 1.9705)
    vln_one_sin_ran($t + 0.2615, 1.2, 32.7025, 0.16, :fm_index, 1.9447)
    vln_one_sin_ran($t + 0.2615, 2.8, 32.7025, 0.16, :fm_index, 2.4918)
    vln_one_sin_ran($t + 0.2620, 1.2, 32.7025, 0.16, :fm_index, 1.6275)
    vln_one_sin_ran($t + 0.2623, 2, 32.7025, 0.16, :fm_index, 2.2355)
    vln_one_sin_ran($t + 0.2623, 4, 32.7025, 0.16, :fm_index, 2.0084)
    vln_one_sin_ran($t + 0.2623, 4, 32.7025, 0.16, :fm_index, 1.8964)
    vln_one_sin_ran($t + 0.2625, 2, 32.7025, 0.16, :fm_index, 2.3937)
    vln_one_sin_ran($t + 0.2625, 1, 32.7025, 0.16, :fm_index, 1.8634)
    vln_one_sin_ran($t + 0.2630, 2, 32.7025, 0.16, :fm_index, 1.5217)
    vln_one_sin_ran($t + 0.2633, 2, 32.7025, 0.16, :fm_index, 1.9275)
    vln_one_sin_ran($t + 0.2635, 1.2, 32.7025, 0.16, :fm_index, 2.4413)
    vln_one_sin_ran($t + 0.2635, 2.8, 32.7025, 0.16, :fm_index, 2.3242)
    vln_one_sin_ran($t + 0.2640, 0.8, 32.7025, 0.16, :fm_index, 2.3267)
    vln_one_sin_ran($t + 0.2643, 2, 32.7025, 0.16, :fm_index, 1.7004)
    vln_one_sin_ran($t + 0.2643, 4, 32.7025, 0.16, :fm_index, 1.8785)
    vln_one_sin_ran($t + 0.2643, 4, 32.7025, 0.16, :fm_index, 2.4573)

    $t += 4.0
    $fm1_rat = 2.718
    $fm2_rat = 4.414
    $fm3_rat = 5.141
    vln_one_sin_ran($t + 0.2600, 1.6, 22.2213, 0.16, :fm_index, 1.6232)
    vln_one_sin_ran($t + 0.2600, 2, 22.2213, 0.16, :fm_index, 1.5982)
    vln_one_sin_ran($t + 0.2603, 1.2, 22.2213, 0.16, :fm_index, 2.1585)
    vln_one_sin_ran($t + 0.2603, 4, 22.2213, 0.16, :fm_index, 2.2207)
    vln_one_sin_ran($t + 0.2605, 1.2, 42.0309, 0.16, :fm_index, 1.5294)
    vln_one_sin_ran($t + 0.2605, 2.8, 42.0309, 0.16, :fm_index, 1.9544)
    vln_one_sin_ran($t + 0.2608, 1.2, 42.0309, 0.16, :fm_index, 2.4016)
    vln_one_sin_ran($t + 0.2608, 1.6, 8.1756, 0.16, :fm_index, 1.5267)
    vln_one_sin_ran($t + 0.2610, 2, 8.1756, 0.16, :fm_index, 2.4190)
    vln_one_sin_ran($t + 0.2610, 1.2, 8.1756, 0.16, :fm_index, 2.2757)
    vln_one_sin_ran($t + 0.2613, 4, 8.1756, 0.16, :fm_index, 2.3607)
    vln_one_sin_ran($t + 0.2615, 1.2, 8.1756, 0.16, :fm_index, 1.8698)
    vln_one_sin_ran($t + 0.2615, 2.8, 8.1756, 0.16, :fm_index, 2.3753)
    vln_one_sin_ran($t + 0.2620, 1.2, 8.1756, 0.16, :fm_index, 2.3392)
    vln_one_sin_ran($t + 0.2623, 2, 8.1756, 0.16, :fm_index, 1.5088)
    vln_one_sin_ran($t + 0.2623, 4, 8.1756, 0.16, :fm_index, 2.2084)
    vln_one_sin_ran($t + 0.2623, 4, 8.1756, 0.16, :fm_index, 1.9512)
    vln_one_sin_ran($t + 0.2625, 2, 8.1756, 0.16, :fm_index, 2.0399)
    vln_one_sin_ran($t + 0.2625, 1, 8.1756, 0.16, :fm_index, 1.7053)
    vln_one_sin_ran($t + 0.2630, 2, 8.1756, 0.16, :fm_index, 2.3204)
    vln_one_sin_ran($t + 0.2633, 2, 8.1756, 0.16, :fm_index, 1.6336)
    vln_one_sin_ran($t + 0.2635, 1.2, 8.1756, 0.16, :fm_index, 1.9483)
    vln_one_sin_ran($t + 0.2635, 2.8, 8.1756, 0.16, :fm_index, 2.3255)
    vln_one_sin_ran($t + 0.2640, 0.8, 8.1756, 0.16, :fm_index, 1.7331)
    vln_one_sin_ran($t + 0.2643, 2, 8.1756, 0.16, :fm_index, 1.9318)
    vln_one_sin_ran($t + 0.2643, 4, 8.1756, 0.16, :fm_index, 1.6908)
    vln_one_sin_ran($t + 0.2643, 4, 8.1756, 0.16, :fm_index, 2.4103)

    $t += 4.0
    vln_one_sin_ran($t + 0.2600, 1.6, 11.1107, 0.16, :fm_index, 1.6371)
    vln_one_sin_ran($t + 0.2600, 2, 11.1107, 0.16, :fm_index, 1.8971)
    vln_one_sin_ran($t + 0.2603, 1.2, 11.1107, 0.16, :fm_index, 1.9065)
    vln_one_sin_ran($t + 0.2603, 4, 11.1107, 0.16, :fm_index, 2.2143)
    vln_one_sin_ran($t + 0.2605, 1.2, 21.0154, 0.16, :fm_index, 1.8011)
    vln_one_sin_ran($t + 0.2605, 2.8, 21.0154, 0.16, :fm_index, 2.1950)
    vln_one_sin_ran($t + 0.2608, 1.2, 21.0154, 0.16, :fm_index, 2.3563)
    vln_one_sin_ran($t + 0.2608, 1.6, 4.0878, 0.16, :fm_index, 2.3181)
    vln_one_sin_ran($t + 0.2610, 2, 4.0878, 0.16, :fm_index, 2.0776)
    vln_one_sin_ran($t + 0.2610, 1.2, 4.0878, 0.16, :fm_index, 1.8336)
    vln_one_sin_ran($t + 0.2613, 4, 4.0878, 0.16, :fm_index, 1.5019)
    vln_one_sin_ran($t + 0.2615, 1.2, 4.0878, 0.16, :fm_index, 2.2368)
    vln_one_sin_ran($t + 0.2615, 2.8, 4.0878, 0.16, :fm_index, 1.7462)
    vln_one_sin_ran($t + 0.2620, 1.2, 4.0878, 0.16, :fm_index, 1.9604)
    vln_one_sin_ran($t + 0.2623, 2, 4.0878, 0.16, :fm_index, 2.2361)
    vln_one_sin_ran($t + 0.2623, 4, 4.0878, 0.16, :fm_index, 1.9972)
    vln_one_sin_ran($t + 0.2623, 4, 4.0878, 0.16, :fm_index, 2.4870)
    vln_one_sin_ran($t + 0.2625, 2, 4.0878, 0.16, :fm_index, 2.0762)
    vln_one_sin_ran($t + 0.2625, 1, 4.0878, 0.16, :fm_index, 2.2973)
    vln_one_sin_ran($t + 0.2630, 2, 4.0878, 0.16, :fm_index, 2.2350)
    vln_one_sin_ran($t + 0.2633, 2, 4.0878, 0.16, :fm_index, 2.1613)
    vln_one_sin_ran($t + 0.2635, 1.2, 4.0878, 0.16, :fm_index, 2.0640)
    vln_one_sin_ran($t + 0.2635, 2.8, 4.0878, 0.16, :fm_index, 2.1738)
    vln_one_sin_ran($t + 0.2640, 0.8, 4.0878, 0.16, :fm_index, 1.5188)
    vln_one_sin_ran($t + 0.2643, 2, 4.0878, 0.16, :fm_index, 1.8766)
    vln_one_sin_ran($t + 0.2643, 4, 4.0878, 0.16, :fm_index, 2.3083)
    vln_one_sin_ran($t + 0.2643, 4, 4.0878, 0.16, :fm_index, 2.2215)

    $t += 4.0
    vln_one_sin_ran($t + 0.2600, 1.6, 66.5893, 0.16, :fm_index, 1.7041)
    vln_one_sin_ran($t + 0.2600, 2, 66.4564, 0.16, :fm_index, 2.0296)
    vln_one_sin_ran($t + 0.2603, 1.2, 66.7225, 0.16, :fm_index, 1.8321) 
    vln_one_sin_ran($t + 0.2603, 4, 66.3237, 0.16, :fm_index, 2.1550)
    vln_one_sin_ran($t + 0.2605, 1.2, 125.4490, 0.16, :fm_index, 2.1806)
    vln_one_sin_ran($t + 0.2605, 2.8, 125.6999, 0.16, :fm_index, 2.3570) 
    vln_one_sin_ran($t + 0.2608, 1.2, 125.1986, 0.16, :fm_index, 1.9861) 
    vln_one_sin_ran($t + 0.2608, 1.6, 24.4994, 0.16, :fm_index, 1.6412) 
    vln_one_sin_ran($t + 0.2610, 2, 24.4505, 0.16, :fm_index, 1.9770)
    vln_one_sin_ran($t + 0.2610, 1.2, 24.5484, 0.16, :fm_index, 2.0103)
    vln_one_sin_ran($t + 0.2613, 4, 24.4017, 0.16, :fm_index, 2.0663) 
    vln_one_sin_ran($t + 0.2615, 1.2, 24.4017, 0.16, :fm_index, 2.1521)
    vln_one_sin_ran($t + 0.2615, 2.8, 24.4505, 0.16, :fm_index, 2.4453)
    vln_one_sin_ran($t + 0.2620, 1.2, 24.3530, 0.16, :fm_index, 2.0930)
    vln_one_sin_ran($t + 0.2623, 2, 24.6960, 0.16, :fm_index, 2.3423)
    vln_one_sin_ran($t + 0.2623, 4, 24.7454, 0.16, :fm_index, 2.0856)
    vln_one_sin_ran($t + 0.2623, 4, 24.7948, 0.16, :fm_index, 1.9570)
    vln_one_sin_ran($t + 0.2625, 2, 24.4994, 0.16, :fm_index, 2.4642)
    vln_one_sin_ran($t + 0.2625, 1, 24.4749, 0.16, :fm_index, 1.9901)
    vln_one_sin_ran($t + 0.2630, 2, 24.5239, 0.16, :fm_index, 1.9972)
    vln_one_sin_ran($t + 0.2633, 2, 24.4505, 0.16, :fm_index, 1.9148)
    vln_one_sin_ran($t + 0.2635, 1.2, 24.4505, 0.16, :fm_index, 1.9017)
    vln_one_sin_ran($t + 0.2635, 2.8, 24.4749, 0.16, :fm_index, 2.4958)
    vln_one_sin_ran($t + 0.2640, 0.8, 24.4260, 0.16, :fm_index, 2.2518)
    vln_one_sin_ran($t + 0.2643, 2, 24.5975, 0.16, :fm_index, 2.1120)
    vln_one_sin_ran($t + 0.2643, 4, 24.6221, 0.16, :fm_index, 2.3154)
    vln_one_sin_ran($t + 0.2643, 4, 24.6467, 0.16, :fm_index, 1.9240)

    $t += 4.0
    $fm1_rat = 6.718
    $fm2_rat = 4.414
    $fm3_rat = 5.141
    vln_one_sin_ran($t + 0.2600, 1.6, 164.5868, 0.16, :fm_index, 1.9587)
    vln_one_sin_ran($t + 0.2600, 2, 164.5868, 0.16, :fm_index, 1.5071)
    vln_one_sin_ran($t + 0.2603, 1.2, 164.5868, 0.16, :fm_index, 1.7690)
    vln_one_sin_ran($t + 0.2603, 4, 164.5868, 0.16, :fm_index, 1.7686)
    vln_one_sin_ran($t + 0.2605, 1.2, 125.9513, 0.16, :fm_index, 1.5702)
    vln_one_sin_ran($t + 0.2605, 2.8, 125.9513, 0.16, :fm_index, 2.1962)
    vln_one_sin_ran($t + 0.2608, 1.2, 125.9513, 0.16, :fm_index, 1.7701)
    vln_one_sin_ran($t + 0.2608, 1.6, 24.4994, 0.16, :fm_index, 2.1665)
    vln_one_sin_ran($t + 0.2610, 2, 24.4994, 0.16, :fm_index, 1.9345)
    vln_one_sin_ran($t + 0.2610, 1.2, 24.4994, 0.16, :fm_index, 2.2037)
    vln_one_sin_ran($t + 0.2613, 4, 24.4994, 0.16, :fm_index, 1.6826)
    vln_one_sin_ran($t + 0.2615, 1.2, 24.4994, 0.16, :fm_index, 1.5410)
    vln_one_sin_ran($t + 0.2615, 2.8, 24.4994, 0.16, :fm_index, 1.8293)
    vln_one_sin_ran($t + 0.2620, 1.2, 24.4994, 0.16, :fm_index, 2.1468)
    vln_one_sin_ran($t + 0.2623, 2, 24.4994, 0.16, :fm_index, 2.0758)
    vln_one_sin_ran($t + 0.2623, 4, 24.4994, 0.16, :fm_index, 2.4138)
    vln_one_sin_ran($t + 0.2623, 4, 24.4994, 0.16, :fm_index, 1.8479)
    vln_one_sin_ran($t + 0.2625, 3, 24.4994, 0.16, :fm_index, 2.4639)
    vln_one_sin_ran($t + 0.2625, 1.5, 24.4994, 0.16, :fm_index, 2.3995)
    vln_one_sin_ran($t + 0.2630, 3, 24.4994, 0.16, :fm_index, 1.8609)
    vln_one_sin_ran($t + 0.2633, 3, 24.4994, 0.16, :fm_index, 2.4506)
    vln_one_sin_ran($t + 0.2635, 1.2, 24.4994, 0.16, :fm_index, 2.1577)
    vln_one_sin_ran($t + 0.2635, 2.8, 24.4994, 0.16, :fm_index, 1.6663)
    vln_one_sin_ran($t + 0.2640, 0.8, 24.4994, 0.16, :fm_index, 2.1166)
    vln_one_sin_ran($t + 0.2643, 2, 24.4994, 0.16, :fm_index, 1.9362)
    vln_one_sin_ran($t + 0.2643, 4, 24.4994, 0.16, :fm_index, 2.2052)
    vln_one_sin_ran($t + 0.2643, 4, 24.4994, 0.16, :fm_index, 2.0102)

    $t += 4.0
    restore_fm_violin_defaults()
    $glissando_amount = 0.8
    $reverb_amount = 0.01
    $gliss_env = whoosh
    $amp_env = metalamp
    $fm1_rat = 2.718
    $fm2_rat = 1.141
    $fm3_rat = 3.141
    vln_one_sin($t + 0.2600, 0.4, 1046.4800, 0.16, :fm_index, 2.3870)
    vln_one_sin($t + 0.2600, 0.5000, 1044.3912, 0.16, :fm_index, 2.4309)
    vln_one_sin($t + 0.2603, 0.3, 1048.5730, 0.16, :fm_index, 2.15)
    vln_one_sin($t + 0.2603, 0.5000, 1042.3066, 0.16, :fm_index, 1.7211)
    vln_one_sin($t + 0.2610, 0.3, 524.2865, 0.16, :fm_index, 2.1751)
    vln_one_sin($t + 0.2620, 0.3, 520.1131, 0.16, :fm_index, 1.5433)
    vln_one_sin($t + 0.2623, 0.4, 517.0048, 0.16, :fm_index, 2.4335)
    vln_one_sin($t + 0.2625, 0.4, 523.2400, 0.16, :fm_index, 2.2778)
    vln_one_sin($t + 0.2635, 0.3, 522.1951, 0.16, :fm_index, 1.9441)
    vln_one_sin($t + 0.2643, 0.4, 520.6316, 0.16, :fm_index, 2.4656)

    $t += 4.0
    restore_fm_violin_defaults()
    vln_one_sin($t + 0.1200, 0.4, 2092.9600, 0.16,
     :fm_index, 3, :reverb_amount, 0, :amp_env, metalamp,
     :fm2_rat, 1.1410, :fm3_rat, 3.1410, :fm1_rat, 2.7180)
    vln_one_sin($t + 0.1200, 0.5000, 2088.7820, 0.16,
     :fm_index, 3, :reverb_amount, 0, :amp_env, metalamp,
     :fm2_rat, 1.1410, :fm3_rat, 3.1410, :fm1_rat, 2.7180)
    vln_one_sin($t + 0.1200, 0.3, 2097.1460, 0.16,
     :fm_index, 3, :reverb_amount, 0, :amp_env, metalamp,
     :fm2_rat, 1.1410, :fm3_rat, 3.1410, :fm1_rat, 2.7180)
    vln_one_sin($t + 0.1200, 0.5000, 2084.6130, 0.16,
     :fm_index, 3, :reverb_amount, 0, :amp_env, metalamp,
     :fm2_rat, 1.1410, :fm3_rat, 3.1410, :fm1_rat, 2.7180)
    vln_one_sin($t + 0.1210, 0.3, 1048.5730, 0.16,
     :fm_index, 3, :reverb_amount, 0, :amp_env, metalamp,
     :fm2_rat, 1.1410, :fm3_rat, 3.1410, :fm1_rat, 2.7180)
    vln_one_sin($t + 0.1220, 0.3, 1040.2260, 0.16,
     :fm_index, 3, :reverb_amount, 0, :amp_env, metalamp,
     :fm2_rat, 1.1410, :fm3_rat, 3.1410, :fm1_rat, 2.7180)
    vln_one_sin($t + 0.1220, 0.5000, 1034.0100, 0.16,
     :fm_index, 3, :reverb_amount, 0, :amp_env, metalamp,
     :fm2_rat, 1.1410, :fm3_rat, 3.1410, :fm1_rat, 2.7180)
    vln_one_sin($t + 0.1230, 0.5000, 1046.4800, 0.16,
     :fm_index, 3, :reverb_amount, 0, :amp_env, metalamp,
     :fm2_rat, 1.1410, :fm3_rat, 3.1410, :fm1_rat, 2.7180)
    vln_one_sin($t + 0.1240, 0.3, 1044.3900, 0.16,
     :fm_index, 3, :reverb_amount, 0, :amp_env, metalamp,
     :fm2_rat, 1.1410, :fm3_rat, 3.1410, :fm1_rat, 2.7180)

    z1amp = [0, 0, 20, 0.5000, 40, 0.1, 60, 0.2, 80, 1, 100, 0]
    z2amp = [0, 0, 20, 1, 60, 0.1, 75, 0.3, 100, 0]
    $fm1_rat = 2.718
    $fm2_rat = 4.414
    $fm3_rat = 5.141
    vln_one_sin($t + 0.1240, 0.5000, 1041.2630, 0.16,
      :fm_index, 3, :reverb_amount, 0, :amp_env, metalamp,
      :fm2_rat, 1.1410, :fm3_rat, 3.1410, :fm1_rat, 2.7180)
    $t += 2.0
    vln_one_sin_ran($t + 0.4880, 1.1770, 416.6072, 0.0110, :fm_index, 1.1140, 
      :reverb_amount, 0.1, :amp_env, z1amp, :noise_amount, 0.0050)
    vln_one_sin_ran($t + 0.5050, 2.4900, 859.5863, 0.0083, :fm_index, 0.5890, 
      :reverb_amount, 0.1, :amp_env, z2amp, :noise_amount, 0.0050)
    vln_one_sin_ran($t + 1.0590, 1.0550, 1758.0816, 0.0053, :fm_index, 1.8640, 
      :reverb_amount, 0.1, :amp_env, z2amp, :noise_amount, 0.0050)
    vln_one_sin_ran($t + 1.0930, 1.8580, 229.0566, 0.0110, :fm_index, 1.9690, 
      :reverb_amount, 0.1, :amp_env, z2amp, :noise_amount, 0.0050)
    vln_one_sin_ran($t + 1.3490, 3.3680, 479.1994, 0.0083, :fm_index, 1.9970, 
      :reverb_amount, 0.1, :amp_env, z1amp, :noise_amount, 0.0050)
    vln_one_sin_ran($t + 1.5010, 3.0680, 411.8241, 0.0110, :fm_index, 1.5390, 
      :reverb_amount, 0.1, :amp_env, z2amp, :noise_amount, 0.0050)
    vln_one_sin_ran($t + 1.5200, 2.8290, 984.8456, 0.0053, :fm_index, 0.0560, 
      :reverb_amount, 0.1, :amp_env, z1amp, :noise_amount, 0.0050)
    vln_one_sin_ran($t + 1.6100, 0.7040, 1767.7444, 0.0053, :fm_index, 1.2620, 
      :reverb_amount, 0.1, :amp_env, z2amp, :noise_amount, 0.0050)
    vln_one_sin_ran($t + 1.8480, 3.0510, 859.7203, 0.0083, :fm_index, 1.6080, 
      :reverb_amount, 0.1, :amp_env, z2amp, :noise_amount, 0.0050)
    vln_one_sin_ran($t + 2.4880, 3.2350, 231.9431, 0.0110, :fm_index, 0.9690, 
      :reverb_amount, 0.1, :amp_env, z1amp, :noise_amount, 0.0050)
    vln_one_sin_ran($t + 2.5610, 3.2810, 475.2009, 0.0083, :fm_index, 0.3740, 
      :reverb_amount, 0.1, :amp_env, z2amp, :noise_amount, 0.0050)
    vln_one_sin_ran($t + 2.7970, 2.8400, 988.8375, 0.0053, :fm_index, 0.4200, 
      :reverb_amount, 0.1, :amp_env, z2amp, :noise_amount, 0.0050)
    vln_one_sin_ran($t + 3.0620, 1.0210, 411.7247, 0.0110, :fm_index, 0.1370, 
      :reverb_amount, 0.1, :amp_env, z2amp, :noise_amount, 0.0050)
    vln_one_sin_ran($t + 3.2130, 1.1610, 848.5959, 0.0083, :fm_index, 1.3120, 
      :reverb_amount, 0.1, :amp_env, z2amp, :noise_amount, 0.0050)
    vln_one_sin_ran($t + 3.4410, 2.6160, 390.0600, 0.0110, :fm_index, 1.9030, 
      :reverb_amount, 0.1, :amp_env, z1amp, :noise_amount, 0.0050)
    vln_one_sin_ran($t + 3.4490, 0.7000, 802.3538, 0.0083, :fm_index, 1.5940, 
      :reverb_amount, 0.1, :amp_env, z2amp, :noise_amount, 0.0050)
    vln_one_sin_ran($t + 3.5270, 2.5080, 1773.9366, 0.0053, :fm_index, 1.8030, 
      :reverb_amount, 0.1, :amp_env, z1amp, :noise_amount, 0.0050)
    vln_one_sin_ran($t + 3.7820, 2.7990, 232.4344, 0.0110, :fm_index, 0.0590, 
      :reverb_amount, 0.1, :amp_env, z2amp, :noise_amount, 0.0050)
    vln_one_sin_ran($t + 3.7830, 2.7660, 1650.1434, 0.0053, :fm_index, 0.4400, 
      :reverb_amount, 0.1, :amp_env, z2amp, :noise_amount, 0.0050)
    vln_one_sin_ran($t + 3.7890, 3.1560, 475.7231, 0.0083, :fm_index, 0.7370, 
      :reverb_amount, 0.1, :amp_env, z2amp, :noise_amount, 0.0050)
    vln_one_sin_ran($t + 4.1540, 2.1290, 976.0237, 0.0053, :fm_index, 1.2690, 
      :reverb_amount, 0.1, :amp_env, z2amp, :noise_amount, 0.0050)
    vln_one_sin_ran($t + 4.4890, 3.3650, 390.0525, 0.0110, :fm_index, 1.4580, 
      :reverb_amount, 0.1, :amp_env, z2amp, :noise_amount, 0.0050)
    vln_one_sin_ran($t + 4.7450, 1.5070, 1665.9722, 0.0053, :fm_index, 1.9330, 
      :reverb_amount, 0.1, :amp_env, z2amp, :noise_amount, 0.0050)
    vln_one_sin_ran($t + 4.8320, 1.4430, 798.1238, 0.0083, :fm_index, 0.8560, 
      :reverb_amount, 0.1, :amp_env, z1amp, :noise_amount, 0.0050)
    vln_one_sin_ran($t + 4.9440, 3.1560, 229.0528, 0.0110, :fm_index, 1.8300, 
      :reverb_amount, 0.1, :amp_env, z2amp, :noise_amount, 0.0050)
    vln_one_sin_ran($t + 5.3930, 1.1100, 473.7225, 0.0083, :fm_index, 1.6260, 
      :reverb_amount, 0.1, :amp_env, z1amp, :noise_amount, 0.0050)
    vln_one_sin_ran($t + 5.6970, 1.6170, 988.7953, 0.0053, :fm_index, 0.4230, 
      :reverb_amount, 0.1, :amp_env, z2amp, :noise_amount, 0.0050)
    vln_one_sin_ran($t + 6.0620, 1.3190, 390.9769, 0.0110, :fm_index, 0.4100, 
      :reverb_amount, 0.1, :amp_env, z2amp, :noise_amount, 0.0050)
    vln_one_sin_ran($t + 6.0840, 3.3660, 804.6413, 0.0083, :fm_index, 1.8760, 
      :reverb_amount, 0.1, :amp_env, z2amp, :noise_amount, 0.0050)
    vln_one_sin_ran($t + 6.1740, 2.7210, 418.6819, 0.0110, :fm_index, 0.0910, 
      :reverb_amount, 0.1, :amp_env, z2amp, :noise_amount, 0.0050)
    vln_one_sin_ran($t + 6.5700, 3.4460, 845.4019, 0.0077, :fm_index, 0.7660, 
      :reverb_amount, 0.1, :amp_env, z2amp, :noise_amount, 0.0050)
    vln_one_sin_ran($t + 6.6440, 1.1790, 1656.5756, 0.0049, :fm_index, 0.2960, 
      :reverb_amount, 0.1, :amp_env, z1amp, :noise_amount, 0.0050)
    vln_one_sin_ran($t + 6.6600, 2.8520, 1758.9788, 0.0049, :fm_index, 0.4520, 
      :reverb_amount, 0.1, :amp_env, z2amp, :noise_amount, 0.0050)
    vln_one_sin_ran($t + 6.8270, 1.8840, 387.0009, 0.0099, :fm_index, 1.3010, 
      :reverb_amount, 0.1, :amp_env, z1amp, :noise_amount, 0.0050)
    vln_one_sin_ran($t + 6.8870, 3.4040, 796.7213, 0.0077, :fm_index, 1.1820, 
      :reverb_amount, 0.1, :amp_env, z2amp, :noise_amount, 0.0050)
    vln_one_sin_ran($t + 6.9640, 3.3230, 416.3916, 0.0099, :fm_index, 0.6290,
      :reverb_amount, 0.1, :amp_env, z2amp, :noise_amount, 0.0050)
    vln_one_sin_ran($t + 7.1320, 1.7050, 1637.2303, 0.0049, :fm_index, 1.0570, 
      :reverb_amount, 0.1, :amp_env, z1amp, :noise_amount, 0.0050)
    vln_one_sin_ran($t + 7.15, 3.1250, 1762.4906, 0.0049, :fm_index, 1.3170, 
      :reverb_amount, 0.1, :amp_env, z2amp, :noise_amount, 0.0050)
    vln_one_sin_ran($t + 7.3860, 2.9670, 852.0487, 0.0077, :fm_index, 1.4790, 
      :reverb_amount, 0.1, :amp_env, z1amp, :noise_amount, 0.0050)
    vln_one_sin_ran($t + 7.6670, 0.6780, 413.7094, 0.0099, :fm_index, 0.9470, 
      :reverb_amount, 0.1, :amp_env, z2amp, :noise_amount, 0.0050)
    vln_one_sin_ran($t + 7.8780, 2.7490, 1749.7509, 0.0049, :fm_index, 0.5040, 
      :reverb_amount, 0.1, :amp_env, z1amp, :noise_amount, 0.0050)
    vln_one_sin_ran($t + 7.9730, 0.5990, 848.1253, 0.0077, :fm_index, 1.9380, 
      :reverb_amount, 0.1, :amp_env, z1amp, :noise_amount, 0.0050)
    vln_one_sin_ran($t + 8.0880, 3.3360, 229.9144, 0.0099, :fm_index, 1.3930, 
      :reverb_amount, 0.1, :amp_env, z2amp, :noise_amount, 0.0050)
    vln_one_sin_ran($t + 8.1170, 1.1300, 984.0816, 0.0049, :fm_index, 0.3560, 
      :reverb_amount, 0.1, :amp_env, z2amp, :noise_amount, 0.0050)
    vln_one_sin_ran($t + 8.4640, 1.7330, 478.7184, 0.0077, :fm_index, 0.2840, 
      :reverb_amount, 0.1, :amp_env, z2amp, :noise_amount, 0.0050)
    vln_one_sin_ran($t + 8.5760, 0.5680, 413.4253, 0.0099, :fm_index, 1.5020,
      :reverb_amount, 0.1, :amp_env, z2amp, :noise_amount, 0.0050)
    vln_one_sin_ran($t + 8.8200, 1.2150, 230.9588, 0.0099, :fm_index, 1.0990, 
      :reverb_amount, 0.1, :amp_env, z1amp, :noise_amount, 0.0050)
    vln_one_sin_ran($t + 8.8320, 3.4590, 473.8903, 0.0077, :fm_index, 0.7680, 
      :reverb_amount, 0.1, :amp_env, z2amp, :noise_amount, 0.0050)
    vln_one_sin_ran($t + 8.8320, 0.7260, 857.2875, 0.0077, :fm_index, 0.7520, 
      :reverb_amount, 0.1, :amp_env, z2amp, :noise_amount, 0.0050)

    $t += 12.0
    indfunc = [0, 1, 20, 0, 100, 0]
    indfunc2 = [0, 1, 90, 1, 100, 0]
    ampfunc = [0, 1, 6, 1, 10, 0.5000, 20, 0.3630, 30, 0.2700, 40, 0.2, 
               50, 0.1200, 60, 0.0800, 70, 0.0400, 100, 0]
    ampfunc1 = [0, 0, 1, 1, 3, 1, 10, 0.5000, 30, 0.2, 60, 0.0500, 100, 0]
    restore_fm_violin_defaults()
    violin($t + 0.2600, 0.0500, 80, 0.8, :fm_index, 5, :reverb_amount, 0, 
      :amp_env, ampfunc1, :fm1_env, indfunc2)
    violin($t + 1.2610, 0.2, 80, 0.8, :fm_index, 4, :reverb_amount, 0, 
      :amp_env, ampfunc, :fm1_env, indfunc, :fm2_rat, 0.6875)
    violin($t + 2.2600, 0.0500, 80, 0.8, :fm_index, 5, :reverb_amount, 0,
      :amp_env, ampfunc1, :fm1_env, indfunc2) 
    violin($t + 2.2620, 0.2, 80, 0.8, :fm_index, 5, :reverb_amount, 0, 
      :amp_env, ampfunc, :fm1_env, indfunc, :fm2_rat, 0.6875)
    violin($t + 3.2600, 0.0500, 80, 0.8, :fm_index, 6, :reverb_amount, 0, 
      :amp_env, ampfunc1, :fm1_env, indfunc2) 
    violin($t + 3.2630, 0.2, 80, 0.8, :fm_index, 6, :reverb_amount, 0, 
      :amp_env, ampfunc, :fm1_env, indfunc, :fm2_rat, 0.6875)
    violin($t + 4.2600, 0.0500, 80, 0.3, :fm_index, 4, :reverb_amount, 0, 
      :amp_env, ampfunc1, :fm1_env, indfunc2) 
    violin($t + 4.2620, 0.1, 160, 0.3, :fm_index, 4, :reverb_amount, 0, 
      :amp_env, ampfunc, :fm1_env, indfunc, :fm2_rat, 0.6875)
    violin($t + 4.2620, 0.2500, 80, 0.8, :fm_index, 4, :reverb_amount, 0, 
      :amp_env, ampfunc, :fm1_env, indfunc, :fm2_rat, 0.6875)
    violin($t + 5.2600, 0.0500, 80, 0.5000, :fm_index, 4, :reverb_amount, 0,
      :amp_env, ampfunc1, :fm1_env, indfunc2)
    violin($t + 5.2610, 0.1, 210, 0.3, :fm_index, 4, :reverb_amount, 0, 
      :amp_env, ampfunc, :fm1_env, indfunc, :fm2_rat, 0.6875)
    violin($t + 5.2620, 0.2, 80, 0.1, :fm_index, 4, :reverb_amount, 0, 
      :amp_env, ampfunc, :fm1_env, indfunc, :fm2_rat, 0.6875)
    violin($t + 5.2630, 0.2500, 320, 0.1, :fm_index, 2, :reverb_amount, 0, 
      :amp_env, ampfunc, :fm1_env, indfunc, :fm2_rat, 0.6875)
    violin($t + 6.2600, 0.0500, 80, 0.8, :fm_index, 4, :reverb_amount, 0, 
      :amp_env, ampfunc1, :fm1_env, indfunc2) 
    violin($t + 6.2610, 0.1, 210, 0.1, :fm_index, 2, :reverb_amount, 0, 
      :amp_env, ampfunc, :fm1_env, indfunc, :fm2_rat, 0.6875)
    violin($t + 6.2620, 0.2, 80, 0.2, :fm_index, 4, :reverb_amount, 0, 
      :amp_env, ampfunc, :fm1_env, indfunc, :fm2_rat, 0.6875)
    violin($t + 6.2630, 0.2500, 320, 0.3, :reverb_amount, 0,
      :amp_env, ampfunc, :fm1_env, indfunc, :fm2_rat, 0.6875) 
    violin($t + 7.2600, 0.0500, 80, 0.8, :fm_index, 2, :reverb_amount, 0, 
      :amp_env, ampfunc1, :fm1_env, indfunc2) 
    violin($t + 7.2610, 0.1, 210, 0.1, :fm_index, 2, :reverb_amount, 0, 
      :amp_env, ampfunc, :fm1_env, indfunc, :fm2_rat, 0.6875)
    violin($t + 7.2620, 0.2, 80, 0.2, :fm_index, 2, :reverb_amount, 0, 
      :amp_env, ampfunc, :fm1_env, indfunc, :fm2_rat, 0.6875)
    violin($t + 7.2630, 0.2500, 320, 0.3, :reverb_amount, 0, 
      :amp_env, ampfunc, :fm1_env, indfunc, :fm2_rat, 0.6875)

    $t += 8.0
    $glissando_amount = 0.0
    $noise_amount = 0.004
    $fm1_rat = 3.141
    $fm2_rat = 4.414
    $fm3_rat = 2.718
    vln_one_sin_ran($t + 0.2600, 4, 3286.9937, 0.16, :fm_index, 2.2165, 
      :reverb_amount, 0.0100, :amp_env, n_amp, :fm1_env, n_amp)
    vln_one_sin_ran($t + 0.2603, 4, 1046.4800, 0.16, :fm_index, 2.3234, 
      :reverb_amount, 0.0100, :amp_env, n_amp, :fm1_env, n_amp)
    vln_one_sin_ran($t + 0.2605, 4, 2844.3326, 0.16, :fm_index, 2.4790, 
      :reverb_amount, 0.1, :amp_env, n_amp, :fm1_env, n_amp)
    vln_one_sin_ran($t + 0.2608, 4, 821.7484, 0.1, :fm_index, 1.8667, 
      :reverb_amount, 0.0100, :amp_env, n_amp, :fm1_env, n_amp)
    vln_one_sin_ran($t + 0.2610, 4, 261.6200, 0.1, :fm_index, 1.8523, 
      :reverb_amount, 0.0100, :amp_env, n_amp, :fm1_env, n_amp)
    vln_one_sin_ran($t + 0.2613, 4, 711.0832, 0.1, :fm_index, 2.2300, 
      :reverb_amount, 0.1, :amp_env, n_amp, :fm1_env, n_amp)
    vln_one_sin_ran($t + 0.2615, 4, 205.4371, 0.0600, :fm_index, 1.5187, 
      :reverb_amount, 0.0100, :amp_env, n_amp, :fm1_env, n_amp)
    vln_one_sin_ran($t + 0.2618, 4, 65.4050, 0.0600, :fm_index, 2.4074, 
      :reverb_amount, 0.0100, :amp_env, n_amp, :fm1_env, n_amp)
    vln_one_sin_ran($t + 0.2620, 4, 177.7708, 0.0600, :fm_index, 2.4481, 
      :reverb_amount, 0.1, :amp_env, n_amp, :fm1_env, n_amp)
    vln_one_sin_ran($t + 0.2623, 4, 51.3593, 0.0100, :fm_index, 2.3069, 
      :reverb_amount, 0.0100, :amp_env, n_amp, :fm1_env, n_amp)
    vln_one_sin_ran($t + 0.2625, 4, 16.3513, 0.0100, :fm_index, 2.1008, 
      :reverb_amount, 0.0100, :amp_env, n_amp, :fm1_env, n_amp)
    vln_one_sin_ran($t + 0.2628, 4, 44.4427, 0.0100, :fm_index, 2.4860, 
      :reverb_amount, 0.1, :amp_env, n_amp, :fm1_env, n_amp)

    $t += 8.0
    restore_fm_violin_defaults()
    vln_one_sin($t + 0.2603, 1.2, 88.8854, 0.1,
      :fm_index, 2.3144, :reverb_amount, 0.2, :amp_env, mamp,
      :fm2_rat, 4.4140, :fm3_rat, 5.1410, :fm1_rat, 2.7180)
    vln_one_sin($t + 0.2603, 4, 88.8854, 0.1,
      :fm_index, 2.1690, :reverb_amount, 0.2, :amp_env, mamp,
      :fm2_rat, 4.4140, :fm3_rat, 5.1410, :fm1_rat, 2.7180)
    vln_one_sin($t + 0.2605, 2.8, 168.1236, 0.0500,
      :fm_index, 2.1850, :reverb_amount, 0.2, :amp_env, mamp,
      :fm2_rat, 4.4140, :fm3_rat, 5.1410, :fm1_rat, 2.7180)
    vln_one_sin($t + 0.2608, 1.2, 168.1236, 0.0800,
      :fm_index, 1.7743, :reverb_amount, 0.2, :amp_env, mamp,
      :fm2_rat, 4.4140, :fm3_rat, 5.1410, :fm1_rat, 2.7180)
    vln_one_sin($t + 0.2610, 2, 32.7025, 0.1,
      :fm_index, 2.4925, :reverb_amount, 0.2, :amp_env, mamp,
      :fm2_rat, 4.4140, :fm3_rat, 5.1410, :fm1_rat, 2.7180)
    vln_one_sin($t + 0.2633, 2, 32.7025, 0.1,
      :fm_index, 2.1325, :reverb_amount, 0.2, :amp_env, mamp,
      :fm2_rat, 4.4140, :fm3_rat, 5.1410, :fm1_rat, 2.7180)
    vln_one_sin($t + 0.2643, 4, 32.7025, 0.0500,
      :fm_index, 1.7578, :reverb_amount, 0.2, :amp_env, mamp,
      :fm2_rat, 4.4140, :fm3_rat, 5.1410, :fm1_rat, 2.7180)

    $t += 8.0
    vln_one_sin_ran($t + 0.2600, 6.6830, 244.8160, 0.0060,
      :fm_index, 2, :reverb_amount, 0.2, :noise_amount, 0.0040)
    vln_one_sin_ran($t + 0.2600, 5.5170, 495.4040, 0.0060,
      :fm_index, 2, :reverb_amount, 0.2, :noise_amount, 0.0040)
    vln_one_sin_ran($t + 0.2600, 7.5350, 980.6190, 0.0020,
      :fm_index, 2, :reverb_amount, 0.2, :noise_amount, 0.0040)
    vln_one_sin_ran($t + 0.2600, 7.1990, 1965.4290, 0.0020,
      :fm_index, 0.8, :reverb_amount, 0.2, :noise_amount, 0.0040)
    vln_one_sin_ran($t + 0.2600, 4.0790, 3835.3170, 0.0020,
      :fm_index, 0.8, :reverb_amount, 0.2, :noise_amount, 0.0040)
    vln_one_sin_ran($t + 0.5170, 4.7400, 1320.9670, 0.0020,
      :fm_index, 0.8, :reverb_amount, 0.2, :noise_amount, 0.0040)
    vln_one_sin_ran($t + 0.7040, 7.2080, 655.5670, 0.0040,
      :fm_index, 2, :reverb_amount, 0.2, :noise_amount, 0.0040)

    $t += 9.0
    updown = [0, 0, 15, 1, 100, 0]
    $glissando_amount = 0.0
    $reverb_amount = 0.9
    vln_one_sin_exp($t + 0.5450, 6.4650, 366.3330, 0.0320, :fm_index, 1.0480,
      :amp_env, 
      [0, 0, 1.5468, 1, 2.0882, 0.7, 2.3202, 1, 98.4532, 0.7500, 100, 0])
    vln_one_sin_exp($t + 0.5950, 8.4340, 1172.5830, 0.0180, :fm_index, 1.1350,
      :amp_env, 
      [0, 0, 1.1857, 1.0, 1.6007, 0.7, 1.7785, 1, 98.8143, 0.5556, 100, 0])
    vln_one_sin_exp($t + 0.7650, 1.6210, 369.9940, 0.0170, :fm_index, 0.0960,
      :amp_env, 
      [0, 0, 6.1690, 1.0, 8.3282, 0.7, 9.2535, 1, 93.8310, 0.5294, 100, 0])
    vln_one_sin_exp($t + 0.8820, 3.0640, 246.9420, 0.0170, :fm_index, 0.0020,
      :amp_env, 
      [0, 0, 3.2637, 1, 4.4060, 0.7, 4.8956, 1.0, 96.7363, 0.5294, 100, 0])
    vln_one_sin_exp($t + 0.9250, 3.1170, 123.4710, 0.0380, :fm_index, 0.2330,
      :amp_env, 
      [0, 0, 3.2082, 1, 4.3311, 0.7, 4.8123, 1, 96.7918, 0.7895, 100, 0])
    vln_one_sin_exp($t + 0.9810, 3.5670, 123.4710, 0.0420, :fm_index, 0.2330,
      :amp_env, 
      [0, 0, 2.8035, 1, 3.7847, 0.7, 4.2052, 1.0, 97.1965, 0.8095, 100, 0])
    vln_one_sin_exp($t + 1.1280, 1.0450, 246.9420, 0.0170, :fm_index, 1.2050,
      :amp_env, 
      [0, 0, 9.5694, 1, 12.9187, 0.7, 14.3541, 1, 90.4306, 0.5294, 100, 0])
    vln_one_sin_exp($t + 1.2550, 3.3870, 374.1370, 0.0170, :fm_index, 0.1800,
      :amp_env, 
      [0, 0, 2.9525, 1.0, 3.9858, 0.7, 4.4287, 1, 97.0475, 0.5294, 100, 0])
    vln_one_sin_exp($t + 1.2990, 8.3050, 1576.9120, 0.0200, :fm_index, 0.2990,
      :amp_env, 
      [0, 0, 1.2041, 1, 1.6255, 0.7, 1.8061, 1, 98.7959, 0.6, 100, 0])
    vln_one_sin_exp($t + 1.3300, 4.4630, 246.9420, 0.0170, :fm_index, 0.0020,
      :amp_env, 
      [0, 0, 2.2406, 1, 3.0249, 0.7, 3.3610, 1.0, 97.7594, 0.5294, 100, 0])
    vln_one_sin_exp($t + 1.6600, 8.9940, 1576.9120, 0.0200, :fm_index, 0.2990,
      :amp_env, 
      [0, 0, 1.1119, 1, 1.5010, 0.7, 1.6678, 1, 98.8881, 0.6, 100, 0])
    vln_one_sin_exp($t + 1.9060, 8.8360, 1172.5830, 0.0180, :fm_index, 1.1350,
      :amp_env, 
      [0, 0, 1.1317, 1, 1.5278, 0.7, 1.6976, 1, 98.8683, 0.5556, 100, 0])
    vln_one_sin_exp($t + 2.1510, 4.9320, 374.1370, 0.0170, :fm_index, 0.1800,
      :amp_env, 
      [0, 0, 2.0276, 1, 2.7372, 0.7, 3.0414, 1, 97.9724, 0.5294, 100, 0])
    vln_one_sin_exp($t + 2.2720, 2.3250, 369.9940, 0.0170, :fm_index, 1.1030,
      :amp_env, 
      [0, 0, 4.3011, 1, 5.8065, 0.7, 6.4516, 1, 95.6989, 0.5294, 100, 0])
    vln_one_sin_exp($t + 3.6960, 3.5540, 366.3330, 0.0310, :fm_index, 1.0480,
      :amp_env, 
      [0, 0, 2.8137, 1, 3.7985, 0.7, 4.2206, 1, 97.1863, 0.7419, 100, 0])
    vln_one_sin_exp($t + 4.7240, 0.6040, 246.9420, 0.0170, :fm_index, 1.2050,
      :amp_env, 
      [0, 0, 16.5563, 1, 22.351, 0.7, 24.8344, 1, 83.4437, 0.5294, 100, 0])
    vln_one_sin_exp($t + 4.9420, 2.5010, 123.4710, 0.0330, :fm_index, 0.2330,
      :amp_env, 
      [0, 0, 3.9984, 1, 5.3978, 0.7, 5.9976, 1, 96.0016, 0.7576, 100, 0])
    vln_one_sin_exp($t + 5.0340, 2.3860, 246.9420, 0.0170, :fm_index, 0.0020,
      :amp_env, 
      [0, 0, 4.1911, 1, 5.6580, 0.7, 6.2867, 1, 95.8089, 0.5294, 100, 0])
    vln_one_sin_exp($t + 5.3850, 1.4510, 369.9940, 0.0170, :fm_index, 1.1030,
      :amp_env, 
      [0, 0, 6.8918, 1, 9.3039, 0.7, 10.3377, 1, 93.1082, 0.5294, 100, 0])
    vln_one_sin_exp($t + 5.5670, 2.6550, 374.1370, 0.0170, :fm_index, 0.1800,
      :amp_env, 
      [0, 0, 3.7665, 1, 5.0847, 0.7, 5.6497, 1, 96.2335, 0.5294, 100, 0])
    vln_one_sin_exp($t + 5.9830, 2.9860, 123.4710, 0.0380, :fm_index, 0.2330,
      :amp_env, 
      [0, 0, 3.3490, 1, 4.5211, 0.7, 5.0234, 1, 96.6510, 0.7895, 100, 0])
    vln_one_sin_exp($t + 6.4910, 0.6110, 123.9770, 0.0170, :fm_index, 0.7550,
      :amp_env, 
      [0, 0, 16.3666, 1, 22.0949, 0.7, 24.55, 1, 83.6334, 0.5294, 100, 0])
    vln_one_sin_exp($t + 6.7570, 1.4440, 123.4710, 0.0170, :fm_index, 0.0020,
      :amp_env, 
      [0, 0, 6.9252, 1, 9.3490, 0.7, 10.3878, 1, 93.0748, 0.5294, 100, 0])
    vln_one_sin_exp($t + 6.7750, 0.5370, 92.4435, 0.0330, :fm_index, 0.9200,
      :amp_env, 
      [0, 0, 18.622, 1, 25.1397, 0.7, 27.9330, 1, 81.3780, 0.7576, 100, 0])
    vln_one_sin_exp($t + 6.7750, 10.5370, 92.4435, 0.0130, :fm_index, 0.9200,
      :amp_env, 
      [0, 0, 0.9490, 1, 1.2812, 0.7, 1.4236, 1, 99.0510, 0.3846, 100, 0])
    vln_one_sin_exp($t + 6.9380, 0.6520, 122.2995, 0.0170, :fm_index, 1.8380,
      :amp_env, 
      [0, 0, 15.3374, 1, 20.706, 0.7, 23.0061, 1, 84.6626, 0.5294, 100, 0])
    vln_one_sin_exp($t + 7.2350, 3.7250, 586.2915, 0.0180, :fm_index, 1.1350, 
      :amp_env, 
      [0, 0, 2.6846, 1, 3.6242, 0.7, 4.0268, 1, 97.3154, 0.5556, 100, 0]) 
    vln_one_sin_exp($t + 7.2560, 2.8900, 183.1665, 0.0260, :fm_index, 1.0480, 
      :amp_env, 
      [0, 0, 3.4602, 1, 4.6713, 0.7, 5.1903, 1, 96.5398, 0.6923, 100, 0]) 
    vln_one_sin_exp($t + 7.2710, 1.6210, 187.0685, 0.0170, :fm_index, 0.1800, 
      :amp_env, 
      [0, 0, 6.169, 1.0, 8.3282, 0.7, 9.2535, 1, 93.8310, 0.5294, 100, 0]) 
    vln_one_sin_exp($t + 7.2920, 2.0160, 183.1665, 0.0290, :fm_index, 1.0480,
      :amp_env, 
      [0, 0, 4.9603, 1, 6.6964, 0.7, 7.4405, 1, 95.0397, 0.7241, 100, 0])
    vln_one_sin_exp($t + 7.2920, 12.0160, 183.1665, 0.0290, :fm_index, 1.0480,
      :amp_env, 
      [0, 0, 0.832, 1, 1.1235, 0.7, 1.248, 1.0, 99.1678, 0.7241, 100, 0])
    vln_one_sin_exp($t + 7.3300, 0.7300, 184.9970, 0.0170, :fm_index, 0.0960,
      :amp_env, 
      [0, 0, 13.699, 1, 18.4932, 0.7, 20.548, 1.0, 86.3014, 0.529, 100, 0])
    vln_one_sin_exp($t + 7.3570, 1.9600, 183.1665, 0.0280, :fm_index, 1.0480,
      :amp_env, 
      [0, 0, 5.1020, 1.0, 6.8878, 0.7, 7.6531, 1, 94.8980, 0.7143, 100, 0])
    vln_one_sin_exp($t + 7.3820, 2.2450, 61.7355, 0.0330, :fm_index, 0.2330,
      :amp_env, 
      [0, 0, 4.4543, 1, 6.0134, 0.7, 6.6815, 1, 95.5457, 0.7576, 100, 0])
    vln_one_sin_exp($t + 7.3820, 12.2450, 61.7355, 0.0330, :fm_index, 0.2330,
      :amp_env, 
      [0, 0, 0.8167, 1, 1.1025, 0.7, 1.2250, 1, 99.1833, 0.7576, 100, 0])
    vln_one_sin_exp($t + 7.5410, 3.0130, 246.5050, 0.0360, :fm_index, 1.1350,
      :amp_env, 
      [0, 0, 3.3190, 1.0, 4.4806, 0.7, 4.9784, 1, 96.6810, 0.7778, 100, 0])
    vln_one_sin_exp($t + 7.5570, 2.3220, 1251.5960, 0.0400, :fm_index, 0.2990,
      :amp_env, 
      [0, 0, 4.3066, 1, 5.8140, 0.7, 6.4599, 1, 95.6934, 0.8, 100, 0])
    vln_one_sin_exp($t + 7.5570, 18.3220, 1251.5960, 0.0200, :fm_index, 0.2990,
      :amp_env, 
      [0, 0, 0.5458, 1.000, 0.7368, 0.7, 0.8187, 1, 99.4542, 0.6, 100, 0])
    vln_one_sin_exp($t + 8.1060, 1.9900, 183.1665, 0.0230, :fm_index, 1.0480,
      :amp_env, 
      [0, 0, 5.0251, 1.0, 6.7839, 0.7, 7.5377, 1, 94.9749, 0.6522, 100, 0])
    vln_one_sin_exp($t + 8.2570, 1.9180, 61.7355, 0.0330, :fm_index, 0.2330,
      :amp_env, 
      [0, 0, 5.2138, 1, 7.0386, 0.7, 7.8206, 1, 94.7862, 0.7576, 100, 0])
    vln_one_sin_exp($t + 8.6370, 1.3090, 183.1665, 0.0310, :fm_index, 1.0480,
      :amp_env, 
      [0, 0, 7.6394, 1, 10.3132, 0.7, 11.4591, 1, 92.3606, 0.7419, 100, 0])
    vln_one_sin_exp($t + 9.0330, 1.1590, 183.1665, 0.0250, :fm_index, 1.0480,
      :amp_env, 
      [0, 0, 8.6281, 1, 11.6480, 0.7, 12.9422, 1, 91.3719, 0.6800, 100, 0])
    vln_one_sin_exp($t + 9.0980, 1.2400, 30.8675, 0.0330, :fm_index, 0.2330,
      :amp_env, 
      [0, 0, 8.0645, 1, 10.8871, 0.7, 12.0968, 1, 91.9355, 0.7576, 100, 0])
    vln_one_sin_exp($t + 9.0980, 11.2400, 30.8675, 0.0130, :fm_index, 0.2330,
      :amp_env, 
      [0, 0, 0.8897, 1, 1.2011, 0.7, 1.3345, 1, 99.1103, 0.3846, 100, 0])
    vln_one_sin_exp($t + 9.1260, 0.2600, 123.4710, 0.0170, :fm_index, 1.2050,
      :amp_env, 
      [0, 0, 38.462, 1, 51.9231, 0.7, 57.6923, 1, 61.5385, 0.5294, 100, 0])
    vln_one_sin_exp($t + 9.1260, 10.2600, 123.4710, 0.0170, :fm_index, 1.2050,
      :amp_env, 
      [0, 0, 0.9747, 1, 1.3158, 0.7, 1.4620, 1, 99.0253, 0.5294, 100, 0])
    vln_one_sin($t + 0.0600, 13.8770, 3951.1200, 0.0090, 
      :amp_env, updown)
    vln_one_sin($t + 0.2600, 14.8770, 123.4725, 0.0170, :fm_index, 1.5, 
      :amp_env, updown)
    vln_one_sin($t + 0.2600, 13.8770, 61.7363, 0.0170, :fm_index, 1.5, 
      :amp_env, updown)
    vln_one_sin($t + 0.2600, 12.8770, 30.8681, 0.0170, :fm_index, 1.5, 
      :amp_env, updown)
    vln_one_sin($t + 0.2600, 11.8770, 15.4341, 0.0170, :fm_index, 1.5, 
      :amp_env, updown)

    $t += 28.0
    restore_fm_violin_defaults()
    cel_one_sum($t + 0.2620, 0.3906, 440, 0.4500, :fm_index, 1.2, 
      :reverb_amount, 0.0013,
      :amp_env, 
      [0, 0, 0.7680, 1, 4.7774, 0.6, 9.7891, 0.3, 24.8243, 0.1, 100, 0])
    cel_one_sum($t + 0.2640, 0.5220, 220, 0.4500, :fm_index, 1.2, 
      :reverb_amount, 0.0012,
      :amp_env, 
      [0, 0, 0.5747, 1.0, 4.5919, 0.6, 9.6134, 0.3, 24.6778, 0.1, 100, 0])
    cel_one_sum($t + 0.2660, 1.5660, 880, 0.4500, :fm_index, 1.2, 
      :reverb_amount, 0.0014,
      :amp_env, 
      [0, 0, 0.1916, 1.0, 4.2242, 0.6, 9.2651, 0.3, 24.3876, 0.1, 100, 0])
    cel_one_sum($t + 0.2680, 1.5660, 110, 0.4500, :fm_index, 1.2, 
      :reverb_amount, 0.0013,
      :amp_env, 
      [0, 0, 0.1916, 1.0, 4.2242, 0.6, 9.2651, 0.3, 24.3876, 0.1, 100, 0])
    $t += 3.0
    vln_one_sin($t + 0.8600, 0.9, 733.3330, 0.1875,
      :fm_index, 0.2, :distance, 1.0, :reverb_amount, 0.0012, 
      :amp_env, 
      [0, 0, 0.3333, 1, 4.3603, 0.6, 9.3939, 0.3, 24.4950, 0.1, 100, 0])
    vln_one_sin($t + 0.8600, 0.2250, 550, 0.1875,
      :fm_index, 0.2, :distance, 1.0, :reverb_amount, 0.0015, 
      :amp_env, 
      [0, 0, 1.3333, 1, 5.3199, 0.6, 10.3030, 0.3, 25.2525, 0.1, 100, 0])
    vln_one_sin($t + 0.8600, 0.4500, 586.6670, 0.3750,
      :fm_index, 0.2, :distance, 1.0, :reverb_amount, 0.0013, 
      :amp_env, 
      [0, 0, 0.6667, 1, 4.6801, 0.6, 9.6970, 0.3, 24.7475, 0.1, 100, 0])
    vln_one_sin($t + 0.9020, 0.9, 733.3330, 0.1875,
      :fm_index, 0.4, :distance, 1.0, :reverb_amount, 0.0013, 
      :amp_env, 
      [0, 0, 0.3333, 1, 4.3603, 0.6, 9.3939, 0.3, 24.4950, 0.1, 100, 0])
    vln_one_sin($t + 0.9020, 0.2250, 550, 0.1875,
      :fm_index, 0.4, :distance, 1.0, :reverb_amount, 0.0010, 
      :amp_env, 
      [0, 0, 1.3333, 1, 5.3199, 0.6, 10.3030, 0.3, 25.2525, 0.1, 100, 0])
    vln_one_sin($t + 0.9020, 0.4500, 586.6670, 0.3750,
      :fm_index, 0.4, :distance, 1.0, :reverb_amount, 0.0015, 
      :amp_env, 
      [0, 0, 0.6667, 1, 4.6801, 0.6, 9.6970, 0.3, 24.7475, 0.1, 100, 0])
    vln_one_sin($t + 0.9430, 0.9, 366.6670, 0.1875,
      :fm_index, 0.6, :distance, 1.0, :reverb_amount, 0.0016, 
      :amp_env, 
      [0, 0, 0.3333, 1, 4.3603, 0.6, 9.3939, 0.3, 24.4950, 0.1, 100, 0])
    vln_one_sin($t + 0.9430, 0.2250, 275, 0.1875,
      :fm_index, 0.6, :distance, 1.0, :reverb_amount, 0.0015, 
      :amp_env,
      [0, 0, 1.3333, 1, 5.3199, 0.6, 10.3030, 0.3, 25.2525, 0.1, 100, 0])
    vln_one_sin($t + 0.9430, 0.4500, 293.3340, 0.3750,
      :fm_index, 0.6, :distance, 1.0, :reverb_amount, 0.0015, 
      :amp_env, 
      [0, 0, 0.6667, 1, 4.6801, 0.6, 9.6970, 0.3, 24.7475, 0.1, 100, 0])
    vln_one_sin($t + 0.9850, 0.9, 733.3330, 0.1875,
      :fm_index, 0.8, :distance, 1.0, :reverb_amount, 0.0010, 
      :amp_env, 
      [0, 0, 0.3333, 1, 4.3603, 0.6, 9.3939, 0.3, 24.4950, 0.1, 100, 0])
    vln_one_sin($t + 0.9850, 0.2250, 550, 0.1875,
      :fm_index, 0.8, :distance, 1.0, :reverb_amount, 0.0013, 
      :amp_env, 
      [0, 0, 1.3333, 1, 5.3199, 0.6, 10.3030, 0.3, 25.2525, 0.1, 100, 0])
    untrace_var(:$t)
  end
end

unless provided?(:snd)
  main()
end

# fmviolin.rb ends here
