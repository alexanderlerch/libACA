#!/usr/bin/env ruby -w
# grani.rb -- grani.ins CL --> Ruby -*- snd-ruby -*-

# Translator: Michael Scholz <mi-scholz@users.sourceforge.net>
# Created: Tue Feb 01 00:47:00 CET 2005
# Last: Thu Oct 15 00:15:57 CEST 2009

# Original header:

# ;;; grani: a granular synthesis instrument
# ;;;   by Fernando Lopez-Lezcano
# ;;;   http://ccrma.stanford.edu/~nando/clm/grani/
# ;;;
# ;;;   Original grani.ins instrument written for the 220a Course by
# ;;;   Fernando Lopez-Lezcano & Juan Pampin, November 6 1996
# ;;;
# ;;; Mar 21 1997: working with hop and grain-dur envelopes
# ;;; Mar 22 1997: working with src envelope (grain wise) & src spread
# ;;; Jan 26 1998: started work on new version
# ;;; Nov  7 1998: input soundfile duration calculation wrong
# ;;; Nov 10 1998: bug in in-samples (thanks to Kristopher D. Giesing for this one)
# ;;; Dec 20 1998: added standard locsig code
# ;;; Feb 19 1999: added "nil" as default value of where to avoid warning (by bill)
# ;;; Jan 10 2000: added input-channel to select which channel of the input file 
# ;;;              to process.
# ;;;              added grain-start-in-seconds to be able to specify input file
# ;;;              locations in seconds for the grain-start envelope
# ;;; May 06 2002: fixed array passing of where-bins in clisp (reported by Charles
# ;;;              Nichols and jennifer l doering
# ;;; Mar 27 2003: added option for sending grains to all channels (requested by
# ;;;              Oded Ben-Tal)

require "ws"
require "env"

# ;;; calculate a random spread around a center of 0
def random_spread(spread)
  spread.nonzero? ? (random(spread) - spread / 2.0) : 0.0
end

# ;;; create a constant envelope if argument is a number
def envelope_or_number(val)
  case val
  when Numeric
    [0, val, 1, val]
  when Vct
    vct2list(val)
  when Array
    val
  else
    error("%s: Number, Array, or Vct required", get_func_name())
  end
end

# ;;; create a vct from an envelope
def make_gr_env(env, length = 512)
  length_1 = (length - 1).to_f
  make_vct!(length) do |i| envelope_interp(i / length_1, env) end
end

# ;;; Grain envelopes
def raised_cosine(*args)
  duty_cycle = get_args(args, :duty_cycle, 100)
  length     = get_args(args, :length, 128)
  active = length * duty_cycle.to_f * 0.01
  incr = PI / (active - 1.0)
  start = (length - active) / 2.0
  fin = (length + active) / 2.0
  s = -1
  make_vct!(length) do |i|
    sine = if i >= start and i < fin
             s += 1
             sin(s * incr)
           else
             0.0
           end
    sine * sine
  end
end

# ;;;=============================================================================
# ;;; Granular synthesis instrument
# ;;;=============================================================================
# 
# ;;; input-channel:
# ;;;   from which channel in the input file are samples read
# ;;; amp-envelope:
# ;;;   amplitude envelope for the note
# ;;; grain-envelope:
# ;;; grain-envelope-end:
# ;;;   envelopes for each individual grain. The envelope applied in the result
# ;;;   of interpolating both envelopes. The interpolation is controlled by
# ;;;   grain-envelope-trasition. If "grain-envelope-end" is nil interpolation
# ;;;   is turned off and only grain-envelope is applied to the grains. 
# ;;; grain-envelope-trasition:
# ;;;   an enveloper that controls the interpolation between the two grain envelopes
# ;;;   0 -> selects "grain-envelope"
# ;;;   1 -> selects "grain-envelope-end"
# ;;; grain-envelope-array-size
# ;;;   size of the array passed to make-table-lookup
# ;;; grain-duration:
# ;;;   envelope that controls grain duration in seconds
# ;;; srate-linear:
# ;;;   t -> sample rate envelope is linear
# ;;;   nil -> sample rate envelope is exponential
# ;;; srate:
# ;;;   envelope that controls sample rate conversion. The envelope is an
# ;;;   exponential envelope, the base and error bound of the conversion
# ;;;   are controlled by "srate-base" and "srate-error".
# ;;; srate-spread:
# ;;;   random spread of sample rate conversion around "srate"
# ;;; srate-base:
# ;;;   base for the exponential conversion
# ;;;   for example: base = (expt 2 (/ 12)) creates a semitone envelope
# ;;; srate-error:
# ;;;   error bound for the exponential conversion. 
# ;;; grain-start:
# ;;;   envelope that determines the starting point of the current grain in
# ;;;   the input file. "y"->0 starts the grain at the beginning of the input
# ;;;   file. "y"->1 starts the grain at the end of the input file.
# ;;; grain-start-spread:
# ;;;   random spread around the value of "grain-start"
# ;;; grain-start-in-seconds:
# ;;;   nil -> grain-start y envelope expressed in percent of the duration of the input file
# ;;;   t   -> grain-start y envelope expressed in seconds
# ;;; grain-density:
# ;;;   envelope that controls the number of grains per second generated in the output file

# ;;; grain-density-spread:

Grani_to_locsig            = 0
Grani_to_grain_duration    = 1
Grani_to_grain_start       = 2
Grani_to_grain_sample_rate = 3
Grani_to_grain_random      = 4
Grani_to_grain_allchans    = 5

def grani(start, dur, amp, file, *args)
  input_channel             = get_args(args, :input_channel, 0)
  grains                    = get_args(args, :grains, 0)
  amp_envelope              = get_args(args, :amp_envelope, [0, 0, 0.3, 1, 0.7, 1, 1, 0])
  grain_envelope            = get_args(args, :grain_envelope, [0, 0, 0.3, 1, 0.7, 1, 1, 0])
  grain_envelope_end        = get_args(args, :grain_envelope_end, false)
  grain_envelope_transition = get_args(args, :grain_envelope_transition, [0, 0, 1, 1])
  grain_envelope_array_size = get_args(args, :grain_envelope_array_size, 512)
  grain_duration            = get_args(args, :grain_duration, 0.1)
  grain_duration_spread     = get_args(args, :grain_spread, 0.0)
  grain_duration_limit      = get_args(args, :grain_limit, 0.002)
  srate                     = get_args(args, :srate, 0.0)
  srate_spread              = get_args(args, :srate_spread, 0.0)
  srate_linear              = get_args(args, :srate_linear, false)
  srate_base                = get_args(args, :srate_base, 2.0 ** (1.0 / 12))
  srate_error               = get_args(args, :srate_error, 0.01)
  grain_start               = get_args(args, :grain_start, [0, 0, 1, 1])
  grain_start_spread        = get_args(args, :grain_start_spread, 0.0)
  grain_start_in_seconds    = get_args(args, :grain_start_in_seconds, false)
  grain_density             = get_args(args, :grain_density, 10.0)
  grain_density_spread      = get_args(args, :grain_density_spread, 0.0)
  reverb_amount             = get_args(args, :reverb_amount, 0.01)
  reverse                   = get_args(args, :reverse, false)
  where_to                  = get_args(args, :where_to, 0)
  where_bins                = get_args(args, :where_bins, [])
  grain_distance            = get_args(args, :grain_distance, 1.0)
  grain_distance_spread     = get_args(args, :grain_distance_spread, 0.0)
  grain_degree              = get_args(args, :grain_degree, 45.0)
  grain_degree_spread       = get_args(args, :grain_degree_spread, 0.0)
  beg, fin = times2samples(start, dur)
  in_file_channels = mus_sound_chans(file)
  in_file_sr       = mus_sound_srate(file).to_f
  in_file_dur      = mus_sound_frames(file) / in_file_sr
  in_file_reader   = make_readin(:file, file, :channel, [input_channel, in_file_channels - 1].min)
  last_in_sample   = (in_file_dur * in_file_sr).round
  srate_ratio      = in_file_sr / mus_srate()
  sr_env = make_env(:envelope, if srate_linear
                                 envelope_or_number(srate)
                               else
                                 exp_envelope(envelope_or_number(srate),
                                              :base, srate_base,
                                              :error, srate_error)
                               end,
                    :scaler, srate_ratio,
                    :duration, dur)
  sr_spread_env = make_env(:envelope, envelope_or_number(srate_spread), :duration, dur)
  amp_env = make_env(:envelope, amp_envelope, :scaler, amp, :duration, dur)
  gr_dur = make_env(:envelope, envelope_or_number(grain_duration), :duration, dur)
  gr_dur_spread = make_env(:envelope, envelope_or_number(grain_duration_spread), :duration, dur)
  gr_start_scaler = (grain_start_in_seconds ? 1.0 : in_file_dur)
  gr_start = make_env(:envelope, envelope_or_number(grain_start), :duration, dur)
  gr_start_spread = make_env(:envelope, envelope_or_number(grain_start_spread), :duration, dur)
  gr_dens_env = make_env(:envelope, envelope_or_number(grain_density), :duration, dur)
  gr_dens_spread_env = make_env(:envelope, envelope_or_number(grain_density_spread), :duration, dur)
  gr_env = make_table_lookup(:frequency, 1.0, "initial-phase".intern, 0.0,
                             :wave, if vct?(grain_envelope)
                                      grain_envelope
                                    else
                                      make_gr_env(grain_envelope, grain_envelope_array_size)
                                    end)
  gr_env_end = make_table_lookup(:frequency, 1.0, "initial-phase".intern, 0.0,
                                 :wave, if grain_envelope_end
                                          if vct?(grain_envelope_end)
                                            grain_envelope_end
                                          else
                                            make_gr_env(grain_envelope_end,
                                                        grain_envelope_array_size)
                                          end
                                        else
                                          make_vct(512)
                                        end)
  gr_int_env = make_env(:envelope, envelope_or_number(grain_envelope_transition), :duration, dur)
  interp_gr_envs = grain_envelope_end
  gr_dist = make_env(:envelope, envelope_or_number(grain_distance), :duration, dur)
  gr_dist_spread = make_env(:envelope, envelope_or_number(grain_distance_spread), :duration, dur)
  gr_degree = make_env(:envelope, envelope_or_number(grain_degree), :duration, dur)
  gr_degree_spread = make_env(:envelope, envelope_or_number(grain_degree_spread), :duration, dur)
  loc = make_locsig(:degree, 45.0,
                    :distance, 1.0,
                    :output, @ws_output,
                    :revout, @ws_reverb,
                    :channels, @channels)
  gr_start_sample = beg
  gr_samples = 0
  gr_offset = 1
  gr_dens = 0.0
  gr_dens_spread = 0.0
  grain_counter = 0
  samples = 0
  first_grain = true
  set_mus_increment(in_file_reader, -1) if reverse
  loop do
    if gr_offset < gr_samples
      gr_where = env(gr_int_env) if interp_gr_envs
      val = if interp_gr_envs
              (1 - gr_where) * table_lookup(gr_env) + gr_where * table_lookup(gr_env_end)
            else
              table_lookup(gr_env)
            end
      locsig(loc, gr_start_sample + gr_offset, val * env(amp_env) * readin(in_file_reader))
      gr_offset += 1
    else
      if first_grain
        first_grain = false
        gr_start_sample = beg
      else
        gr_start_sample += seconds2samples(1.0 / (gr_dens + gr_dens_spread))
        if (gr_start_sample > fin) or (grains.nonzero? and (grain_counter >= grains))
          break
        end
      end
      gr_offset = 0
      gr_from_beg = gr_start_sample - beg
      set_mus_location(amp_env, gr_from_beg)
      set_mus_location(gr_dur, gr_from_beg)
      set_mus_location(gr_dur_spread, gr_from_beg)
      set_mus_location(sr_env, gr_from_beg)
      set_mus_location(sr_spread_env, gr_from_beg)
      set_mus_location(gr_start, gr_from_beg)
      set_mus_location(gr_start_spread, gr_from_beg)
      set_mus_location(gr_dens_env, gr_from_beg)
      set_mus_location(gr_dens_spread_env, gr_from_beg)
      in_start_value = env(gr_start) * gr_start_scaler +
        random_spread(env(gr_start_spread) * gr_start_scaler)
      in_start = (in_start_value * in_file_sr).round
      gr_duration = [grain_duration_limit, env(gr_dur) + random_spread(env(gr_dur_spread))].max
      gr_samples = seconds2samples(gr_duration)
      gr_srate = if srate_linear
                   env(sr_env) + random_spread(env(sr_spread_env))
                 else
                   env(sr_env) * srate_base ** random_spread(env(sr_spread_env))
                 end
      set_mus_increment(in_file_reader, gr_srate)
      in_samples = gr_samples / (1.0 / srate_ratio)
      set_mus_phase(gr_env, 0.0)
      set_mus_phase(gr_env_end, 0.0)
      set_mus_frequency(gr_env, 1.0 / gr_duration)
      set_mus_frequency(gr_env_end, 1.0 / gr_duration)
      gr_dens = env(gr_dens_env)
      gr_dens_spread = random_spread(env(gr_dens_spread_env))
      samples += gr_samples
      grain_counter += 1
      where = case where_to
              when Grani_to_grain_duration
                gr_duration
              when Grani_to_grain_start
                in_start_value
              when Grani_to_grain_sample_rate
                gr_srate
              when Grani_to_grain_random
                random(1.0)
              else
                Grani_to_locsig
              end
      if where.nonzero? and where_bins.length > 1
        (where_bins.length - 1).times do |chn|
          locsig_set!(loc, chn,
                      ((where_bins[chn] < where and where < where_bins[chn + 1]) ? 1.0 : 0.0))
        end
      else
        if where_to == Grani_to_grain_allchans
          @channels.times do |chn| locsig_set!(loc, chn, 1.0) end
        else
          set_mus_location(gr_dist, gr_from_beg)
          set_mus_location(gr_dist_spread, gr_from_beg)
          set_mus_location(gr_degree, gr_from_beg)
          set_mus_location(gr_degree_spread, gr_from_beg)
          deg = env(gr_degree) + random_spread(env(gr_degree_spread))
          dist = env(gr_dist) + random_spread(env(gr_dist_spread))
          dist_scl = 1.0 / [dist, 1.0].max
          if @ws_reverb
            locsig_reverb_set!(loc, 0, reverb_amount * (1.0 / sqrt([dist, 1.0].max)))
          end
          if @channels == 1
            locsig_set!(loc, 0, dist_scl)
          else
            if @channels == 2
              frac = [90.0, [0.0, deg].max].min / 90.0
              locsig_set!(loc, 0, dist_scl * (1.0 - frac))
              locsig_set!(loc, 1, dist_scl * frac)
            else
              if @channels > 2
                locsig_set!(loc, 0, if 0 <= deg and deg <= 90
                                      dist_scl * ((90.0 - deg) / 90.0)
                                    else
                                      if 270 <= deg and deg <= 360
                                        dist_scl * ((deg - 270.0) / 90.0)
                                      else
                                        0.0
                                      end
                                    end)
                locsig_set!(loc, 1, if 90 <= deg and deg <= 180
                                      dist_scl * (180.0 - deg) / 90.0
                                    else
                                      if 0 <= deg and deg <= 90
                                        dist_scl * (deg / 90.0)
                                      else
                                        0.0
                                      end
                                    end)
                locsig_set!(loc, 2, if 180 <= deg and deg <= 270
                                      dist_scl * (270.0 - deg) / 90.0
                                    else
                                      if 90 <= deg and deg <= 180
                                        dist_scl * (deg - 90.0) / 90.0
                                      else
                                        0.0
                                      end
                                    end)
                if @channels > 3
                  locsig_set!(loc, 3, if 270 <= deg and deg <= 360
                                        dist_scl * (360.0 - deg) / 90.0
                                      else
                                        if 180 <= deg and deg <= 270
                                          dist_scl * (deg - 180.0) / 90.0
                                        else
                                          0.0
                                        end
                                      end)
                end
              end
            end
          end
        end
      end
      in_start = if (in_start + in_samples) > last_in_sample
                   last_in_sample - in_samples
                 else
                   [in_start, 0].max
                 end
      set_mus_location(in_file_reader, in_start)
    end
  end
  mus_close(in_file_reader)
end

=begin
with_sound(:play, 1, :statistics, true, :channels, 1, :reverb, nil) do
  grani(0.0, 2.0, 5.0, "/usr/gnu/sound/SFiles/oboe.snd", :grain_envelope, raised_cosine())
end
=end

# grani.rb ends here
