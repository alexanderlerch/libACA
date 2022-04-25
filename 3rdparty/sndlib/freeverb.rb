# freeverb.rb -- CLM -> Snd/Ruby translation of freeverb.ins

# Translator/Author: Michael Scholz <mi-scholz@users.sourceforge.net>
# Created: Tue Apr 08 03:53:20 CEST 2003
# Changed: Thu Oct 15 00:14:35 CEST 2009

# Original notes of Fernando Lopez-Lezcano

# ;; Freeverb - Free, studio-quality reverb SOURCE CODE in the public domain
# ;;
# ;; Written by Jezar at Dreampoint, June 2000
# ;; http://www.dreampoint.co.uk
# ;;
# ;; Translated into clm-2 by Fernando Lopez-Lezcano <nando@ccrma.stanford.edu>
# ;; Version 1.0 for clm-2 released in January 2001
# ;; http://ccrma.stanford.edu/~nando/clm/freeverb/
# ;;
# ;; Changes to the original code by Jezar (by Fernando Lopez-Lezcano):
# ;; - the clm version can now work with a mono input or an n-channel input
# ;;   stream (in the latter case the number of channels of the input and output
# ;;   streams must match.
# ;; - the "wet" parameter has been eliminated as it does not apply to the model
# ;;   that clm uses to generate reverberation
# ;; - the "width" parameter name has been changed to :global. It now controls the
# ;;   coefficients of an NxN matrix that specifies how the output of the reverbs
# ;;   is mixed into the output stream.
# ;; - predelays for the input channels have been added.
# ;; - damping can be controlled individually for each channel. 

# For more information see clm-2/freeverb/index.html.

# Code:

require "ws"
with_silence(LoadError) do require "sndins" end

# Snd-Ruby's freeverb and fcomb (see sndins.so for a faster one).

unless provided? :sndins
  class Fcomb
    def initialize(scaler, size, a0, a1)
      @feedback = scaler.to_f
      @delay = make_delay(size.to_i)
      @filter = make_one_zero(a0, a1)
    end
    attr_accessor :feedback

    def fcomb(input = 0.0)
      delay(@delay, input + one_zero(@filter, tap(@delay)) * @feedback)
    end

    def inspect
      format("#<%s: %s, %s, feedback: %0.3f>",
             self.class, @delay.inspect, @filter.inspect, @feedback)
    end
  end
  
  def make_fcomb(scaler = 0.0, size = 1, a0 = 0.0, a1 = 0.0)
    Fcomb.new(scaler, size, a0, a1)
  end

  def fcomb(gen, input = 0.0)
    gen.fcomb(input)
  end

  def fcomb?(obj)
    obj.kind_of?(Fcomb)
  end
end

add_help(:freeverb_rb, "freeverb_rb(*args)
        :room_decay,        0.5,
        :damping,           0.5,
        :global,            0.3,
        :predelay,          0.03,
        :output_gain,       1.0,
        :output_mixer,      nil,
        :scale_room_decay,  0.28,
        :offset_room_decay, 0.7,
        :combtuning,        [1116, 1188, 1277, 1356, 1422, 1491, 1557, 1617],
        :allpasstuning,     [556, 441, 341, 225],
        :scale_damping,     0.4,
        :stereo_spread,     23,
with_sound(:reverb, :freeverb_rb) do fm_violin_rb(0, 1, 440, 0.3) end
This is the Ruby version of freeverb.  For a faster one see sndins.so.")
def freeverb_rb(*args)
  room_decay, damping, global, predelay, output_gain, output_mixer = nil
  scale_room_decay, offset_room_decay, combtuning, allpasstuning, scale_damping, stereo_spread = nil
  optkey(args, binding,
         [:room_decay, 0.5],
         [:damping, 0.5],
         [:global, 0.3],
         [:predelay, 0.03],
         [:output_gain, 1.0],
         [:output_mixer, nil],
         [:scale_room_decay, 0.28],
         [:offset_room_decay, 0.7],
         [:combtuning, [1116, 1188, 1277, 1356, 1422, 1491, 1557, 1617]],
         [:allpasstuning, [556, 441, 341, 225]],
         [:scale_damping, 0.4],
         [:stereo_spread, 23])
  out_mix = (mixer?(output_mixer) ? output_mixer : make_mixer(@channels))
  out_buf = make_frame(@channels)
  out_gain = output_gain
  f_out = make_frame(@channels)
  predelays = make_array(@reverb_channels)
  local_gain = (1.0 - global) * (1.0 - 1.0 / @channels) + 1.0 / @channels
  global_gain = (@channels - local_gain * @channels) / [(@channels * @channels - @channels), 1].max
  srate_scale = @srate / 44100.0
  room_decay_val = room_decay * scale_room_decay + offset_room_decay
  numcombs = combtuning.length
  numallpasses = allpasstuning.length
  combs = make_array(@channels) do make_array(numcombs) end
  allpasses = make_array(@channels) do make_array(numallpasses) end
  if @reverb_channels > 1 and @reverb_channels != @channels
    error("input must be mono or input channels must equal output channels")
  end
  unless mixer?(output_mixer)
    if output_mixer.kind_of?(Array)
      @channels.times do |i|
        @channels.times do |j|
          mixer_set!(out_mix, i, j, output_mixer[i][j])
        end
      end
    else
      @channels.times do |i|
        @channels.times do |j|
          mixer_set!(out_mix, i, j, (out_gain * (i == j ? local_gain : global_gain)) / @channels)
        end
      end
    end
  end
  if predelay.kind_of?(Array)
    @reverb_channels.times do |c|
      predelays[c] = make_delay(:size, (@srate * predelay[c]).to_i)
    end
  else
    @reverb_channels.times do |c|
      predelays[c] = make_delay(:size, (@srate * predelay).to_i)
    end
  end
  if damping.kind_of?(Array)
    @channels.times do |c|
      combtuning.each_with_index do |tuning, i|
        l = (srate_scale * tuning).round
        dmp = scale_damping * damping[i]
        l += (srate_scale * stereo_spread).round if c.odd?
        combs[c][i] = make_fcomb(room_decay_val, l, 1.0 - dmp, dmp)
      end
    end
  else
    @channels.times do |c|
      combtuning.each_with_index do |tuning, i|
        l = (srate_scale * tuning).round
        dmp = scale_damping * damping
        l += (srate_scale * stereo_spread).round if c.odd?
        combs[c][i] = make_fcomb(room_decay_val, l, 1.0 - dmp, dmp)
      end
    end
  end
  @channels.times do |c|
    allpasstuning.each_with_index do |tuning, i|
      l = (srate_scale * tuning).round
      l += (srate_scale * stereo_spread).round if c.odd?
      allpasses[c][i] = make_all_pass(:size, l, :feedforward, -1, :feedback, 0.5)
    end
  end
  run_reverb(:frames) do |f_in, i|
    if @reverb_channels > 1
      @channels.times do |c|
        frame_set!(f_in, c, delay(predelays[c], frame_ref(f_in, c)))
        frame_set!(f_out, c, 0.0)
        numcombs.times do |j|
          frame_set!(f_out, c, frame_ref(f_out, c) + fcomb(combs[c][j], frame_ref(f_in, c)))
        end
      end
    else
      frame_set!(f_in, 0, delay(predelays[0], frame_ref(f_in, 0)))
      @channels.times do |c|
        frame_set!(f_out, c, 0.0)
        numcombs.times do |j|
          frame_set!(f_out, c, frame_ref(f_out, c) + fcomb(combs[c][j], frame_ref(f_in, 0)))
        end
      end
    end
    @channels.times do |c|
      numallpasses.times do |j|
        frame_set!(f_out, c, all_pass(allpasses[c][j], frame_ref(f_out, c)))
      end
    end
    frame2frame(f_out, out_mix, out_buf)
  end
end

class Snd_Instrument
  alias freeverb freeverb_rb
end

=begin
with_sound(:reverb, :freeverb_rb,
           :reverb_data, [:room_decay, 0.9],
           :channels, 2,
           :reverb_channels, 1,
           :output, "fvrb-test.snd",
           :play, 1,
           :statistics, true) do
  fm_violin_rb(0, 1, 440, 0.5)
end
=end

# freeverb.rb ends here
