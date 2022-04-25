# maxf.rb -- CLM -> Snd/Ruby translation of maxf.ins

# Translator/Author: Michael Scholz <mi-scholz@users.sourceforge.net>
# Created: Mon Mar 24 11:24:23 CET 2003
# Changed: Thu Oct 15 00:16:58 CEST 2009

# It follows the original header of Juan Reyes.

# ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
# ;;
# ;;  maxf.ins
# ;;  This is Max Mathews (mvm) new filter (2002)
# ;;  High-Q, 2-Integrator, filter with
# ;;  Two Poles, and one Zero at the Origin
# ;;
# ;; It synthesizes equal-tempered frequencies
# ;; integer & just scales out of a wide-band input
# ;; signal.
# ;; Based on  Max's code (filter.cpp)
# ;;
# ;;  This heuristic might be called Modal Synthesis.
# ;;  But as well it can also be  additive synthesis in
# ;;  which a resonator is  initialized to generate the
# ;;  exponentially decaying sinusoids at the desired
# ;;  phase.
# ;;
# ;;   This implementation written by Juan Reyes with dsp 
# ;;   assistance from JOS.
# ;;   This  version Oct-30, 2002
# ;;
# ;;   Change gain(att) of input file if clipping
# ;;
# ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

require "ws"

CLM = Struct.new("CLM", :yy1, :yy2, :zz1, :zz2, :pp1, :pp2, :pp3, :out)

add_help(:maxfilter, "maxfilter(file, start, *args)
    :att           = 1.0
    :numf          = 1
    :freqfactor    = 1.0
    :amplitude     = 1.0
    :amp-env       = [0, 1, 100, 1]
    :degree        = kernel_rand(90.0)
    :distance      = 1.0
    :reverb_amount = 0.2

This is Max Mathews (mvm) new filter (2002) High-Q, 2-Integrator,
filter with Two Poles, and one Zero at the Origin

It synthesizes equal-tempered frequencies integer & just scales
out of a wide-band input signal.
Based on Max's code (filter.cpp)

This heuristic might be called Modal Synthesis.  But as well it
can also be additive synthesis in which a resonator is
initialized to generate the exponentially decaying sinusoids at
the desired phase.

   :att  =  1   in-file attenuation
   :numf =  1   1 filter
   :numf =  4   4 filters
   :numf =  9   9 filters
   :numf = 12  12 filters
   :numf = 13  13 filters")
def maxfilter(file, start = 0, *args)
  att, numf, freqfactor, amplitude, amp_env, degree, distance, reverb_amount = nil
  optkey(args, binding,
         [:att, 1.0],
         [:numf, 1],
         [:freqfactor, 1.0],
         [:amplitude, 1.0],
         [:amp_env, [0, 1, 100, 1]],
         [:degree, kernel_rand(90.0)],
         [:distance, 1.0],
         [:reverb_amount, 0.2])
  rda, snd   = make_general_reader(file, :channel, 0)
  formfil    = CLM.new(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
  dur        = duration(file)
  ampf       = make_env(:envelope, amp_env, :scaler, amplitude, :duration, dur)
  state_0    = make_array( 1) do make_array(3, 0.0) end
  state_1    = make_array(12) do make_array(3, 0.0) end
  state_2    = make_array( 9) do make_array(3, 0.0) end
  state_3    = make_array(13) do make_array(3, 0.0) end
  state_4    = make_array( 4) do make_array(3, 0.0) end
  state_5    = make_array( 2) do make_array(3, 0.0) end
  case numf
  when 1
    Snd.display "State 0 (default): One filter"
    state_0[0] = 7.54e-002, 2000.0 * freqfactor, 2.0
  when 2
    Snd.display "State 5: Two filters"
    state_5[0] = 7.54e-003, 200.0 * freqfactor, 4.0
    state_5[1] = 7.54e-004, 800.0 * freqfactor, 1.0
  when 4
    Snd.display "State 4: Four filters"
    state_4[0] = 7.54e-002, 1000.0 * freqfactor, 0.5
    state_4[1] = 3.225e-002, 400.0 * freqfactor, 3.0
    state_4[2] = 1.14e-002,  800.0 * freqfactor, 2.8
    state_4[3] = 7.54e-002, 1600.0 * freqfactor, 1.0
  when 9
    Snd.display "State 2: Streached overtone string  9 filters"
    state_2[0] = 1.07e-002,  100.0, 2.5
    state_2[1] = 1.07e-002,  202.0, 0.75
    state_2[2] = 1.07e-002,  305.0, 0.5
    state_2[3] = 7.077e-003, 408.0, 0.4
    state_2[4] = 1.07e-002,  501.0, 0.3
    state_2[5] = 1.07e-002,  612.0, 0.25
    state_2[6] = 1.07e-003,  715.0, 0.25
    state_2[7] = 1.07e-002,  817.0, 0.2
    state_2[8] = 1.07e-002,  920.0, 0.18
  when 12
    Snd.display "State 1: Risset bell long  12 filters"
    state_1[0]  = 5.025e-002, 224.0, 3.7
    state_1[1]  = 5.025e-002, 225.0, 3.3
    state_1[2]  = 5.025e-002, 368.0, 2.8
    state_1[3]  = 5.025e-002, 369.0, 2.4
    state_1[4]  = 1.047e-002, 476.0, 1.9
    state_1[5]  = 5.025e-002, 680.0, 1.7
    state_1[6]  = 5.025e-002, 800.0, 1.5
    state_1[7]  = 4.05e-002, 1096.0, 1.1
    state_1[8]  = 4.05e-002, 1099.0, 0.9
    state_1[9]  = 4.05e-002, 1200.0, 0.6
    state_1[10] = 3.78e-002, 1504.0, 0.4
    state_1[11] = 4.05e-002, 1628.0, 0.3
  when 13
    Snd.display "State 3: Open major chord with repeated octave  12 filters"
    state_3[0]  = 5.025e-002,  100.0, 2.0
    state_3[1]  = 5.025e-002,  251.0, 2.0
    state_3[2]  = 5.025e-002,  299.0, 2.0
    state_3[3]  = 5.025e-002,  401.0, 2.0
    state_3[4]  = 5.025e-002,  199.0, 2.0
    state_3[5]  = 5.025e-002,  501.0, 2.0
    state_3[6]  = 5.025e-002,  599.0, 2.0
    state_3[7]  = 5.025e-002,  801.0, 2.0
    state_3[8]  = 5.025e-002,  201.0, 2.0
    state_3[9]  = 5.025e-002,  749.0, 2.0
    state_3[10] = 5.025e-002,  900.0, 2.0
    state_3[11] = 5.025e-004, 1205.0, 2.0
    state_3[12] = 5.025e-004, 1205.0, 2.0
  else
    Snd.display "Please leave default or enter [1] [2] [4] [9] [12] [13]"
    numf = 1
  end
  mvmfilt = lambda do |b, sample|
    b[:yy2] = (b[:pp1] * b[:yy1] + b[:pp2] * b[:zz1]) - b[:pp3] * sample
    b[:zz2] = b[:zz1] - b[:pp2] * b[:yy2]
    b[:zz1] = b[:zz2]
    b[:yy1] = b[:yy2]
    b[:out] = b[:yy1]
  end
  set_coeffs = lambda do |b, ary|
    famp, ffreq, fdecay = ary
    tper = 1.0 / @srate
    centerfreq = (2.0 * PI * ffreq) / @srate
    maxdecay = (2.0 * tper) / (centerfreq * centerfreq)
    mindecay = tper / centerfreq
    fdecay = if fdecay >= maxdecay
               maxdecay
             else
               fdecay.to_f
             end
    fdecay = mindecay if fdecay <= mindecay
    b[:pp1] = 1.0 - 2.0 / (fdecay * @srate)
    b[:pp2] = (2.0 * PI * ffreq) / @srate
    b[:pp3] = b[:pp2] * famp
  end
  run_instrument(start, dur, :degree, degree, :distance, distance, :reverb_amount, reverb_amount) do
    outval_a = att * general_readin(rda)
    add_fl = 0.0
    numf.times do |j|
      case numf
      when 1
        set_coeffs.call(formfil, state_0[j])
      when 2
        set_coeffs.call(formfil, state_5[j])
      when 4
        set_coeffs.call(formfil, state_4[j])
      when 9
        set_coeffs.call(formfil, state_2[j])
      when 12
        set_coeffs.call(formfil, state_1[j])
      when 13
        set_coeffs.call(formfil, state_3[j])
      end
      filsig = mvmfilt.call(formfil, outval_a)
      add_fl += filsig
    end
    env(ampf) * add_fl
  end
  close_general_reader(snd, rda)
end

=begin
ifile = "dog.snd"
ofile = "rmax_dog.snd"
stats = [1, 2, 4, 9, 12, 13]
with_sound(:play, 1, :statistics, true, :channels, 4, :output, ofile, :reverb, :jc_reverb,
           :comment, format("maxfilter test, filters %s, source %s", stats.inspect, ifile)) do
  stats.each_with_index do |val, i| maxfilter(ifile, i, :numf, val) end
end

with_sound(:srate, 22050) do maxfilter("dog.snd", 0) end
with_sound(:srate, 44100) do maxfilter("dog.snd", 0, :numf, 12) end
with_sound(:srate, 44100) do maxfilter("dog.snd", 0, :numf, 13, :att, 0.75) end
with_sound(:srate, 44100) do maxfilter("dog.snd", 0, :numf, 2, :att, 0.25, :freqfactor, 0.5) end
=end

# maxf.rb ends here
