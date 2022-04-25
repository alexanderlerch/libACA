# clm-ins.rb -- CLM instruments translated to Snd/Ruby

# Translator: Michael Scholz <mi-scholz@users.sourceforge.net>
# Created: Tue Sep 16 01:27:09 CEST 2003
# Changed: Sun Dec 23 01:00:48 CET 2012

# Instruments work with
#   with_sound (CLM (sample2file gens) and Snd)
#   with_dac   (dac output, except at least for fullmix)
#
# Tested with Snd 13.x and Ruby 2.x.x

# pluck                  reson
# vox                    cellon
# fofins                 jl_reverb
# fm_trumpet             gran_synth
# pqw_vox                touch_tone
# stereo_flute           spectra
# fm_bell                two_tab
# fm_insect              lbj_piano
# fm_drum                resflt
# gong                   scratch
# attract                pins
# pqw                    zc
# tubebell               zn
# wurley                 za
# rhodey                 clm_expsrc     exp_snd
# hammondoid             expfil
# metal                  graph_eq
# drone                  anoi
# canter                 fullmix
# nrev                   grani
#
#  class Ssb_fm < Musgen
#   initialize(freq)
#   inspect
#   to_s
#   run_func(val1, val2)
#   ssb_fm(modsig)

# bes_fm(start, dur, freq, amp, ratio, index)
#
# make_ssb_fm(freq)
# ssb_fm?(obj)
# ssb_fm(gen, modsig)
#
# class Fm2 < Musgen
#  initialize(f1, f2, f3, f4, p1, p2, p3, p4)
#  inspect
#  to_s
#  run_func(val1, val2)
#  fm2(index)
#
# make_fm2(f1, f2, f3, f4, p1, p2, p3, p4)
# fm2?(obj)
# fm2(gen, index)

# comments from clm-ins.scm

$now = 0.0

require "ws"
require "spectr"
require "env"
include Math
with_silence do
  require "matrix"
end

def normalize_partials(partials)
  sum = 0.0
  parts = partials.dup
  len = parts.length
  1.step(len - 1, 2) do |i| sum += parts[i].abs end
  1.step(len - 1, 2) do |i| parts[i] /= sum end
  parts
end unless defined? normalize_partials

# violin is defined as an example in ws.rb
def violin_test(start = 0.0, dur = 1.0)
  violin(start, dur, 440, 0.5)
  $now = start + dur + 0.2
end

require "v"

# fm_violin is defined in v.rb
def fm_violin_test(start = 0.0, dur = 1.0)
  fm_violin(start, dur, 440, 0.5)
  $now = start + dur + 0.2
end

# PLUCK
#
# The Karplus-Strong algorithm as extended by David Jaffe and Julius
#  Smith -- see Jaffe and Smith, "Extensions of the Karplus-Strong
#  Plucked-String Algorithm" CMJ vol 7 no 2 Summer 1983, reprinted in
#  "The Music Machine".  translated from CLM's pluck.ins
add_help(:pluck,
         "pluck(start, dur, freq, amp, weighting, lossfact) \
implements the Jaffe-Smith plucked string physical model. 
'weighting' is the ratio of the once-delayed to the twice-delayed samples.  \
It defaults to 0.5=shortest decay. 
Anything other than 0.5 = longer decay.  Must be between 0 and less than 1.0. 
'lossfact' can be used to shorten decays.  \
Most useful values are between 0.8 and 1.0. pluck(0, 1, 330, 0.3, 0.95, 0.95)")
def pluck(start, dur, freq, amp, weighting = 0.5, lossfact = 0.9)
  get_optimum_c = lambda do |s, o, p|
    pa = (1.0 / o) * atan2(s * sin(o), (1.0 - s) + s * cos(o))
    tmp_int = (p - pa).floor
    pc = p - pa - tmp_int
    until pc >= 0.1
      tmp_int -= 1
      pc += 1.0
    end
    [tmp_int, (sin(o) - sin(o * pc)) / sin(o + o * pc)]
  end
  tune_it = lambda do |f, s1|
    p = @srate / f
    s = s1.zero? ? 0.5 : s1
    o = hz2radians(f)
    t1, c1 = get_optimum_c.call(s, o, p)
    t2, c2 = get_optimum_c.call(1.0 - s, o, p)
    if s != 0.5 and c1.abs < c2.abs
      [1.0 - s, c1, t1]
    else
      [s, c2, t2]
    end
  end
  wt0, c, dlen = tune_it.call(freq, weighting)
  lf = lossfact.zero? ? 1.0 : [1.0, lossfact].min
  wt = wt0.zero? ? 0.5 : [1.0, wt0].min
  tab = make_vct(dlen)
  # get initial waveform in "tab" -- here we can introduce 0's to
  # simulate different pick positions, and so on -- see the CMJ
  # article for numerous extensions.  The normal case is to load it
  # with white noise (between -1 and 1).
  allp = make_one_zero(lf * (1.0 - wt), lf * wt)
  feedb = make_one_zero(c, 1.0)     # or feedb = make_one_zero(1.0, c)
  dlen.times do |i| tab[i] = 1.0 - random(2.0) end
  run_instrument(start, dur) do
    val = tab.clm_cycle
    tab[tab.clm_cycle_index] = (1.0 - c) * one_zero(feedb, one_zero(allp, val))
    amp * val
  end
end

def pluck_test(start = 0.0, dur = 1.0)
  pluck(start, dur, 330, 0.3, 0.95, 0.95)
  $now = start + dur + 0.2
end

# formant center frequencies for a male speaker (vox and pqw_vox)
Formants = {
  :I  => [390, 1990, 2550], :E   => [530, 1840, 2480], :AE => [660, 1720, 2410],
  :UH => [520, 1190, 2390], :A   => [730, 1090, 2440], :OW => [570,  840, 2410],
  :U  => [440, 1020, 2240], :OO  => [300,  870, 2240], :ER => [490, 1350, 1690],
  :W  => [300,  610, 2200], :LL  => [380,  880, 2575], :R  => [420, 1300, 1600],
  :Y  => [300, 2200, 3065], :EE  => [260, 3500, 3800], :LH => [280, 1450, 1600],
  :L  => [300, 1300, 3000], :I2  => [350, 2300, 3340], :B  => [200,  800, 1750],
  :D  => [300, 1700, 2600], :G   => [250, 1350, 2000], :M  => [280,  900, 2200],
  :N  => [280, 1700, 2600], :NG  => [280, 2300, 2750], :P  => [300,  800, 1750],
  :T  => [200, 1700, 2600], :K   => [350, 1350, 2000], :F  => [175,  900, 4400],
  :TH => [200, 1400, 2200], :S   => [200, 1300, 2500], :SH => [200, 1800, 2000],
  :V  => [175, 1100, 2400], :THE => [200, 1600, 2200], :Z  => [200, 1300, 2500],
  :ZH => [175, 1800, 2000], :ZZ  => [900, 2400, 3800], :VV => [565, 1045, 2400]}

# MLBVOI
# 
# translation from MUS10 of Marc LeBrun's waveshaping voice instrument
# (using FM here) this version translated (and simplified slightly)
# from CLM's mlbvoi.ins
def vox(start, dur, freq, amp, ampfun, freqfun, freqscl, voxfun, index, vibscl)
  f1 = []
  f2 = []
  f3 = []
  (voxfun.length - 1).step(1, -2) do |i|
    phon = Formants[voxfun[i]]
    x = voxfun[i - 1]
    f1.unshift(phon[0])
    f1.unshift(x)
    f2.unshift(phon[1])
    f2.unshift(x)
    f3.unshift(phon[2])
    f3.unshift(x)
  end
  car_os = make_oscil(:frequency, 0)
  of0 = make_oscil(:frequency, 0)
  of1 = make_oscil(:frequency, 0)
  of2 = make_oscil(:frequency, 0)
  of3 = make_oscil(:frequency, 0)
  of4 = make_oscil(:frequency, 0)
  of5 = make_oscil(:frequency, 0)
  ampf = make_env(:envelope, ampfun, :scaler, amp, :duration, dur)
  frmf1 = make_env(:envelope, f1, :duration, dur)
  frmf2 = make_env(:envelope, f2, :duration, dur)
  frmf3 = make_env(:envelope, f3, :duration, dur)
  freqf = make_env(:envelope, freqfun, :duration, dur, :scaler, freqscl * freq, :offset, freq)
  per_vib = make_triangle_wave(:frequency, 6, :amplitude, freq * vibscl)
  ran_vib = make_rand_interp(:frequency, 20, :amplitude, freq * 0.01)
  run_instrument(start, dur) do
    frq = env(freqf) + triangle_wave(per_vib) + rand_interp(ran_vib)
    car = index * oscil(car_os, hz2radians(frq))
    frm = env(frmf1)
    frm0 = frm / frq.to_f
    frm_int = frm0.floor
    if frm_int.even?
      frq0 = hz2radians(frm_int * frq)
      frq1 = hz2radians((frm_int + 1) * frq)
      amp1 = frm0 - frm_int
      amp0 = 1.0 - amp1
    else
      frq1 = hz2radians(frm_int * frq)
      frq0 = hz2radians((frm_int + 1) * frq)
      amp0 = frm0 - frm_int
      amp1 = 1.0 - amp0
    end
    frm = env(frmf2)
    frm0 = frm / frq.to_f
    frm_int = frm0.floor
    if frm_int.even?
      frq2 = hz2radians(frm_int * frq)
      frq3 = hz2radians((frm_int + 1) * frq)
      amp3 = frm0 - frm_int
      amp2 = 1.0 - amp3
    else
      frq3 = hz2radians(frm_int * frq)
      frq2 = hz2radians((frm_int + 1) * frq)
      amp2 = frm0 - frm_int
      amp3 = 1.0 - amp2
    end
    frm = env(frmf3)
    frm0 = frm / frq.to_f
    frm_int = frm0.floor
    if frm_int.even?
      frq4 = hz2radians(frm_int * frq)
      frq5 = hz2radians((frm_int + 1) * frq)
      amp5 = frm0 - frm_int
      amp4 = 1.0 - amp5
    else
      frq5 = hz2radians(frm_int * frq)
      frq4 = hz2radians((frm_int + 1) * frq)
      amp4 = frm0 - frm_int
      amp5 = 1.0 - amp4
    end
    env(ampf) * (0.8 * (amp0 * oscil(of0, frq0 + 0.2 * car) +
                        amp1 * oscil(of1, frq1 + 0.2 * car)) +
                        0.15 * (amp2 * oscil(of2, frq2 + 0.5 * car) +
                                amp3 * oscil(of3, frq3 + 0.5 * car)) +
                                0.05 * (amp4 * oscil(of4, frq4 + car) +
                                        amp5 * oscil(of5, frq5 + car)))
  end
end

def vox_test(start = 0.0, dur = 1.0)
  amp_env = [0, 0, 25, 1, 75, 1, 100, 0]
  frq_env = [0, 0, 5, 0.5, 10, 0, 100, 1]
  examp1 = [0, :E, 25, :AE, 35, :ER, 65, :ER, 75, :I, 100, :UH]
  examp2 = [0, :I, 5, :OW, 10, :I, 50, :AE, 100, :OO]

  $now = start
  vox($now, dur, 170, 0.4, amp_env, frq_env, 0.1, examp1, 0.05, 0.1)
  $now += dur + 0.2
  vox($now, dur, 300, 0.4, amp_env, frq_env, 0.1, examp2, 0.02, 0.1)
  $now += dur + 0.2
  vox($now, 5, 600, 0.4, amp_env, frq_env, 0.1, examp2, 0.01, 0.1)
  $now += 5.0 + 0.2
end

# FOF example
add_help(:fofins,
         "fofins(beg, dur, frq, amp, vib, f0, a0, f1, a1, \
f2, a2, ae=[0, 0, 25, 1, 75, 1, 100, 0]) \
produces FOF synthesis: \
fofins(0, 1, 270, 0.2, 0.001, 730, 0.6, 1090, 0.3, 2440, 0.1)")
def fofins(start, dur, frq, amp, vib, f0, a0, f1, a1, f2, a2,
           ae = [0, 0, 25, 1, 75, 1, 100,0])
  ampf = make_env(:envelope, ae, :scaler, amp, :duration, dur)
  frq0 = hz2radians(f0)
  frq1 = hz2radians(f1)
  frq2 = hz2radians(f2)
  foflen = @srate == 22050.0 ? 100 : 200
  vibr = make_oscil(:frequency, 6)
  win_freq = TWO_PI / foflen
  wt0 = make_wave_train(:size, foflen, :frequency, frq)
  foftab = mus_data(wt0)
  foflen.times do |i|
    foftab[i] = (a0 * sin(i * frq0) + a1 * sin(i * frq1) +
                     a2 * sin(i * frq2)) * 0.5 * (1.0 - cos(i * win_freq))
  end
  run_instrument(start, dur) do
    env(ampf) * wave_train(wt0, vib * oscil(vibr))
  end
end

def fofins_test(start = 0.0, dur = 1.0)
  fofins(start, dur, 270, 0.2, 0.001, 730, 0.6, 1090, 0.3, 2440, 0.1)
  $now = start + dur + 0.2
end

# FM TRUMPET
#
# Dexter Morrill's FM-trumpet: from CMJ feb 77 p51
def fm_trumpet(start, dur, *args)
  frq1, frq2, amp1, amp2, ampatt1, ampdec1, ampatt2, ampdec2 = nil
  modfrq1, modind11, modind12, modfrq2, modind21, modind22 = nil
  rvibamp, rvibfrq, vibamp, vibfrq, vibatt, vibdec = nil
  frqskw, frqatt, ampenv1, ampenv2 = nil
  indenv1, indenv2, degree, distance, reverb_amount = nil
  optkey(args, binding,
         [:frq1, 250.0],
         [:frq2, 1500.0],
         [:amp1, 0.5],
         [:amp2, 0.1],
         [:ampatt1, 0.03],
         [:ampdec1, 0.35],
         [:ampatt2, 0.03],
         [:ampdec2, 0.3],
         [:modfrq1, 250.0],
         [:modind11, 0.0],
         [:modind12, 2.66],
         [:modfrq2, 250.0],
         [:modind21, 0.0],
         [:modind22, 1.8],
         [:rvibamp, 0.007],
         [:rvibfrq, 125.0],
         [:vibamp, 0.007],
         [:vibfrq, 7.0],
         [:vibatt, 0.6],
         [:vibdec, 0.2],
         [:frqskw, 0.03],
         [:frqatt, 0.06],
         [:ampenv1, [0, 0, 25, 1, 75, 0.9, 100, 0]],
         [:ampenv2, [0, 0, 25, 1, 75, 0.9, 100, 0]],
         [:indenv1, [0, 0, 25, 1, 75, 0.9, 100, 0]],
         [:indenv2, [0, 0, 25, 1, 75, 0.9, 100, 0]],
         [:degree, 0.0],
         [:distance, 1.0],
         [:reverb_amount, 0.005])
  dur = dur.to_f
  per_vib_f = make_env(:envelope,
                       stretch_envelope([0, 1, 25, 0.1, 75, 0, 100, 0],
                                        25,
                                        [100 * (vibatt / dur), 45].min,
                                        75,
                                        [100 * (1.0 - vibdec / dur), 55].max),
                       :scaler, vibamp, :duration, dur)

  ran_vib = make_rand_interp(:frequency, rvibfrq, :amplitude, rvibamp)
  per_vib = make_oscil(:frequency, vibfrq)
  dec_01 = [75, 100 * (1.0 - 0.01 / dur)].max
  frq_f = make_env(:envelope,
                   stretch_envelope([0, 0, 25, 1, 75, 1, 100, 0],
                                    25,
                                    [25, 100 * (frqatt / dur)].min,
                                    75,
                                    dec_01),
                   :scaler, frqskw, :duration, dur)
  ampattpt1 = [25, 100 * (ampatt1 / dur)].min
  ampdecpt1 = [75, 100 * (1.0 - ampdec1 / dur)].max
  ampattpt2 = [25, 100 * (ampatt2 / dur)].min
  ampdecpt2 = [75, 100 * (1.0 - ampdec2 / dur)].max
  mod1_f = make_env(:envelope,
                    stretch_envelope(indenv1, 25, ampattpt1, 75, dec_01),
                    :scaler, modfrq1 * (modind12 - modind11), :duration, dur)
  mod1 = make_oscil(:frequency, 0.0)
  car1 = make_oscil(:frequency, 0.0)
  # set frequency to zero here because it is handled multiplicatively below
  car1_f = make_env(:envelope,
                    stretch_envelope(ampenv1, 25, ampattpt1, 75, ampdecpt1),
                    :scaler, amp1, :duration, dur)
  mod2_f = make_env(:envelope,
                    stretch_envelope(indenv2, 25, ampattpt2, 75, dec_01),
                    :scaler, modfrq2 * (modind22 - modind21), :duration, dur)
  mod2 = make_oscil(:frequency, 0.0)
  car2 = make_oscil(:frequency, 0.0)
  car2_f = make_env(:envelope,
                    stretch_envelope(ampenv2, 25, ampattpt2, 75, ampdecpt2),
                    :scaler, amp2, :duration, dur)
  run_instrument(start, dur,
                 :degree, degree,
                 :distance, distance,
                 :reverb_amount, reverb_amount) do
    frq_change = hz2radians((1.0 +
                             rand_interp(ran_vib)) *
                             (1.0 + env(per_vib_f) *
                              oscil(per_vib)) *
                              (1.0 + env(frq_f)))
    env(car1_f) *
      oscil(car1, frq_change *
            (frq1 + env(mod1_f) * oscil(mod1, modfrq1 * frq_change))) +
      env(car2_f) *
      oscil(car2, frq_change *
            (frq2 + env(mod2_f) * oscil(mod2, modfrq2 * frq_change)))
  end
end

def fm_trumpet_test(start = 0.0, dur = 1.0)
  fm_trumpet(start, dur)
  $now = start + dur + 0.2
end

# PQWVOX
#
# translation of CLM pqwvox.ins (itself translated from MUS10 of MLB's
# waveshaping voice instrument (using phase quadrature waveshaping))
add_help(:pqw_vox,
         "pqw_vox(start, dur, freq, spacing_freq, \
amp, ampfun, freqfun, freqscl, phonemes, formant_amps, formant_shapes)  \
produces vocal sounds using phase quadrature waveshaping")
def pqw_vox(start, dur, freq, spacing_freq, amp, ampfun, freqfun, freqscl,
            phonemes, formant_amps, formant_shapes)
  vox_fun = lambda do |phons, which, newenv|
    # make an envelope from which entry of phoneme data referred to by phons
    if phons.empty?
      newenv
    else
      vox_fun.call(phons[2..-1],
                   which, newenv + [phons[0], Formants[phons[1]][which]])
    end
  end
  car_sin = make_oscil(:frequency, 0.0)
  car_cos = make_oscil(:frequency, 0.0, :initial_phase, HALF_PI)
  frq_ratio = spacing_freq / freq.to_f
  fs = formant_amps.length
  sin_evens = Array.new(fs)
  cos_evens = Array.new(fs)
  sin_odds = Array.new(fs)
  cos_odds = Array.new(fs)
  amps = Array.new(fs)
  frmfs = Array.new(fs)
  sin_coeffs = Array.new(fs)
  cos_coeffs = Array.new(fs)
  ampf = make_env(:envelope, ampfun, :scaler, amp, :duration, dur)
  freqf = make_env(:envelope, freqfun, :scaler, freqscl * freq,
                   :duration, dur, :offset, freq)
  per_vib = make_triangle_wave(:frequency, 6.0, :amplitude, freq * 0.1)
  ran_vib = make_rand_interp(:frequency, 20.0, :amplitude, freq * 0.05)
  fs.times do |i|
    sin_evens[i] = make_oscil(:frequency, 0.0)
    sin_odds[i] = make_oscil(:frequency, 0.0)
    cos_evens[i] = make_oscil(:frequency, 0.0, :initial_phase, HALF_PI)
    cos_odds[i] = make_oscil(:frequency, 0.0, :initial_phase, HALF_PI)
    amps[i] = formant_amps[i]
    shape = normalize_partials(formant_shapes[i])
    cos_coeffs[i] = partials2polynomial(shape, 1)
    sin_coeffs[i] = partials2polynomial(shape, 0)
    frmfs[i] = make_env(:envelope, vox_fun.call(phonemes, i, []),
                        :duration, dur)
  end
  run_instrument(start, dur) do
    frq = env(freqf) + triangle_wave(per_vib) + rand_interp(ran_vib)
    frqscl = hz2radians(frq * frq_ratio)
    carsin = oscil(car_sin, frqscl)
    carcos = oscil(car_cos, frqscl)
    sum = 0.0
    fs.times do |j|
      frm = env(frmfs[j])
      frm0 = frm / frq
      frm_int = frm0.floor
      if frm_int.even?
        even_freq = hz2radians(frm_int * frq)
        odd_freq = hz2radians((frm_int + 1.0) * frq)
        odd_amp = frm0 - frm_int
        even_amp = 1.0 - odd_amp
      else
        odd_freq = hz2radians(frm_int * frq)
        even_freq = hz2radians((frm_int + 1.0) * frq)
        even_amp = frm0 - frm_int
        odd_amp = 1.0 - even_amp
      end
      fax = polynomial(cos_coeffs[j], carcos)
      yfax = carsin * polynomial(sin_coeffs[j], carcos)
      sum = sum + amps[j] *
        (even_amp * (yfax * oscil(sin_evens[j], even_freq) -
                     fax * oscil(cos_evens[j], even_freq)) +
                     odd_amp * (yfax * oscil(sin_odds[j], odd_freq) -
                                fax * oscil(cos_odds[j], odd_freq)))
    end
    env(ampf) * sum
  end
end

def pqw_vox_test(start = 0.0, dur = 1.0)
  ampfun = [0, 0, 50, 1, 100, 0]
  freqfun = [0, 0, 100, 0]
  freqramp = [0, 0, 100, 1]
  sh1 = [[1, 1, 2, 0.5],
    [1, 0.5, 2, 0.5, 3, 1],
    [1, 1, 4, 0.5]]
  sh2 = [[1, 1, 2, 0.5],
    [1, 1, 2, 0.5, 3, 0.2, 4, 0.1],
    [1, 1, 3, 0.1, 4, 0.5]]
  sh3 = [[1, 1, 2, 0.5],
    [1, 1, 4, 0.1],
    [1, 1, 2, 0.1, 4, 0.05]]
  sh4 = [[1, 1, 2, 0.5, 3, 0.1, 4, 0.01],
    [1, 1, 4, 0.1],
    [1, 1, 2, 0.1, 4, 0.05]]

  $now = start
  pqw_vox($now, dur, 300, 300, 0.5, ampfun, freqfun, 0.0,
          [0, :L, 100, :L], [0.33, 0.33, 0.33], sh1)
  $now += dur + 0.2
  pqw_vox($now, dur, 200, 200, 0.1, ampfun, freqramp, 0.1,
          [0, :UH, 100, :ER], [0.8, 0.15, 0.05], sh2)
  $now += dur + 0.2
  pqw_vox($now, dur, 100, 314, 0.1, ampfun, freqramp, 0.1,
          [0, :UH, 100, :ER], [0.8, 0.15, 0.05], sh2)
  $now += dur + 0.2
  pqw_vox($now, dur, 200, 314, 0.1, ampfun, freqramp, 0.01,
          [0, :UH, 100, :ER], [0.8, 0.15, 0.05], sh3)
  $now += dur + 0.2
  pqw_vox($now, dur, 100, 414, 0.2, ampfun, freqramp, 0.01,
          [0, :OW, 50, :E, 100, :ER], [0.8, 0.15, 0.05], sh4)
  $now += dur + 0.2
end

# STEREO-FLUTE
# slightly simplified [MS]
add_help(:stereo_flute,
        "stereo_flute(start, dur, freq, flow, *key_args) 
 :flow_envelope   = [0, 1, 100, 1]
 :decay           = 0.01
 :noise           = 0.0356
 :embouchure_size = 0.5
 :fbk_scl1        = 0.5
 :fbk_scl2        = 0.55
 :out_scl         = 1.0
 :a0              = 0.7
 :b1              = -0.3
 :vib_rate        = 5
 :vib_amount      = 0.03
 :ran_rate        = 5
 :ran_amount      = 0.03
is a physical model of a flute: \
stereo_flute(0, 1, 440, 0.55, :flow_envelope, [0, 0, 1, 1, 2, 1, 3, 0])")
def stereo_flute(start, dur, freq, flow, *args)
  flow_envelope, decay, noise, embouchure_size = nil
  fbk_scl1, fbk_scl2, out_scl = nil
  a0, b1, vib_rate, vib_amount, ran_rate, ran_amount = nil
  optkey(args, binding,
         [:flow_envelope, [0, 1, 100, 1]],
         [:decay, 0.01],   # additional time for instrument to decay
         [:noise, 0.0356],
         [:embouchure_size, 0.5],
         [:fbk_scl1, 0.5], # these two are crucial for good results
         [:fbk_scl2, 0.55],
         [:out_scl, 1.0],
         [:a0, 0.7],       # filter coefficients
         [:b1, -0.3],
         [:vib_rate, 5],
         [:vib_amount, 0.03],
         [:ran_rate, 5],
         [:ran_amount, 0.03])
  flowf = make_env(:envelope, flow_envelope, :scaler, flow,
                   :length, seconds2samples(dur - decay))
  periodic_vib = make_oscil(:frequency, vib_rate)
  ran_vib = make_rand_interp(:frequency, ran_rate)
  breath = make_rand(:frequency, @srate / 2.0, :amplitude, 1)
  period_samples = (@srate / freq).floor
  embouchure_samples = (embouchure_size * period_samples).floor
  embouchure = make_delay(embouchure_samples, :initial_element, 0.0)
  bore = make_delay(period_samples)
  reflection_lp_filter = make_one_pole(a0, b1)
  out_sig = current_diff = previous_out_sig = previous_dc_blocked_a = 0.0
  run_instrument(start, dur) do
    delay_sig = delay(bore, out_sig)
    emb_sig = delay(embouchure, current_diff)
    current_flow = vib_amount * oscil(periodic_vib) +
      ran_amount * rand_interp(ran_vib) + env(flowf)
    current_diff = (current_flow + noise * current_flow * rand(breath)) +
      fbk_scl1 * delay_sig
    current_exitation = emb_sig - emb_sig * emb_sig * emb_sig
    out_sig = one_pole(reflection_lp_filter,
                       current_exitation + fbk_scl2 * delay_sig)
    # NB the DC blocker is not in the cicuit. It is applied to the
    # out_sig but the result is not fed back into the system.
    dc_blocked_a = (out_sig - previous_out_sig) + 0.995 * previous_dc_blocked_a
    previous_out_sig = out_sig
    previous_dc_blocked_a = dc_blocked_a
    out_scl * dc_blocked_a
  end
end

def flute_test(start = 0.0, dur = 1.0)
  stereo_flute(start, dur, 440, 0.55, :flow_envelope, [0, 0, 1, 1, 2, 1, 3, 0])
  $now = start + dur + 0.2
end

# FM-BELL
add_help(:fm_bell,
         "fm_bell(startime, dur, frequency, amplitude, \
[amp-env=[...], [index_env=[...], [index=1.0]]]) \
mixes in one fm bell note" )
def fm_bell(start, dur, freq, amp,
            amp_env = [0, 0, 0.1, 1, 10, 0.6, 25, 0.3,
              50, 0.15, 90, 0.1, 100, 0],
            index_env = [0, 1, 2, 1.1, 25, 0.75, 75, 0.5, 100, 0.2],
            index = 1.0)
  fm_ind1 = hz2radians(32.0 * freq)
  fm_ind2 = hz2radians(4.0 * (8.0 - freq / 50.0))
  fm_ind3 = fm_ind2 * 0.705 * (1.4 - freq / 250.0)
  fm_ind4 = hz2radians(32.0 * (20.0 - freq / 20.0))
  mod1 = make_oscil(:frequency, freq * 2.0)
  mod2 = make_oscil(:frequency, freq * 1.41)
  mod3 = make_oscil(:frequency, freq * 2.82)
  mod4 = make_oscil(:frequency, freq * 2.4)
  car1 = make_oscil(:frequency, freq)
  car2 = make_oscil(:frequency, freq)
  car3 = make_oscil(:frequency, freq * 2.4)
  indf = make_env(:envelope, index_env, :scaler, index, :duration, dur)
  ampf = make_env(:envelope, amp_env, :scaler, amp, :duration, dur)
  run_instrument(start, dur) do
    fmenv = env(indf)
    env(ampf) *
      (oscil(car1, fmenv * fm_ind1 * oscil(mod1)) +
       0.15 *
       oscil(car2, fmenv * (fm_ind2 * oscil(mod2) + fm_ind3 * oscil(mod3))) +
       0.15 *
       oscil(car3, fmenv * fm_ind4 * oscil(mod4)))
  end
end

def fm_bell_test(start = 0.0, dur = 1.0)
  fm_bell(start, dur, 440, 0.5)
  $now = start + dur + 0.2
end

# FM-INSECT
def fm_insect(start, dur, freq, amp, amp_env,
              mod_freq, mod_skew, mod_freq_env, mod_index, mod_index_env,
              fm_index, fm_ratio, *args)
  degree, distance, reverb_amount = nil
  optkey(args, binding,
         [:degree, 0.0],
         [:distance, 1.0],
         [:reverb_amount, 0.005])
  carrier = make_oscil(:frequency, freq)
  fm1_osc = make_oscil(:frequency, mod_freq)
  fm2_osc = make_oscil(:frequency, fm_ratio * freq)
  ampf = make_env(:envelope, amp_env, :scaler, amp, :duration, dur)
  indf = make_env(:envelope, mod_index_env,
                  :scaler, hz2radians(mod_index),
                  :duration, dur)
  modfrqf = make_env(:envelope, mod_freq_env,
                     :scaler, hz2radians(mod_skew),
                     :duration, dur)
  fm2_amp = hz2radians(fm_index * fm_ratio * freq)
  run_instrument(start, dur,
                 :degree, degree,
                 :distance, distance,
                 :reverb_amount, reverb_amount) do
    garble_in = env(indf) * oscil(fm1_osc, env(modfrqf))
    garble_out = fm2_amp * oscil(fm2_osc, garble_in)
    env(ampf) * oscil(carrier, garble_out + garble_in)
  end
end

def fm_insect_test(start = 0.0, dur = 1.0)
  locust = [0, 0, 40, 1, 95, 1, 100, 0.5]
  bug_hi = [0, 1, 25, 0.7, 75, 0.78, 100, 1]
  amp = [0, 0, 25, 1, 75, 0.7, 100, 0]

  $now = start
  fm_insect($now + 0.000, 1.699, 4142.627, 0.015, amp, 60, -16.707,
            locust, 500.866, bug_hi, 0.346, 0.5)
  fm_insect($now + 0.195, 0.233, 4126.284, 0.030, amp, 60, -12.142,
            locust, 649.490, bug_hi, 0.407, 0.5)
  fm_insect($now + 0.217, 2.057, 3930.258, 0.045, amp, 60,  -3.011,
            locust, 562.087, bug_hi, 0.591, 0.5)
  fm_insect($now + 2.100, 1.500,  900.627, 0.060, amp, 40, -16.707,
            locust, 300.866, bug_hi, 0.346, 0.5)
  fm_insect($now + 3.000, 1.500,  900.627, 0.060, amp, 40, -16.707,
            locust, 300.866, bug_hi, 0.046, 0.5)
  fm_insect($now + 3.450, 1.500,  900.627, 0.090, amp, 40, -16.707,
            locust, 300.866, bug_hi, 0.006, 0.5)
  fm_insect($now + 3.950, 1.500,  900.627, 0.120, amp, 40, -10.707,
            locust, 300.866, bug_hi, 0.346, 0.5)
  fm_insect($now + 4.300, 1.500,  900.627, 0.090, amp, 40, -20.707,
            locust, 300.866, bug_hi, 0.246, 0.5)
  $now += 6.0
end

# FM-DRUM
#
# Jan Mattox's fm drum:
def fm_drum(start, dur, freq, amp, index, high = false,
            degree = 0.0, distance = 1.0, rev_amount = 0.01)
  casrat = high ? 8.525 : 3.515
  fmrat = high ? 3.414 : 1.414
  glsf = make_env(:envelope, [0, 0, 25, 0, 75, 1, 100, 1],
                  :scaler, high ? hz2radians(66) : 0.0, :duration, dur)
  ampfun = [0, 0, 3, 0.05, 5, 0.2, 7, 0.8, 8, 0.95,
    10, 1.0, 12, 0.95, 20, 0.3, 30, 0.1, 100, 0]
  atdrpt = 100 * (high ? 0.01 : 0.015) / dur
  ampf = make_env(:envelope,
                  stretch_envelope(ampfun, 10, atdrpt, 15,
                                   [atdrpt + 1,
                                     100 - 100 * ((dur - 0.2) / dur)].max),
                  :scaler, amp, :duration, dur)
  indxfun = [0, 0, 5, 0.014, 10, 0.033, 15, 0.061, 20, 0.099,
    25, 0.153, 30, 0.228, 35, 0.332, 40, 0.477,
    45, 0.681, 50, 0.964, 55, 0.681, 60, 0.478, 65, 0.332,
    70, 0.228, 75, 0.153, 80, 0.099, 85, 0.061,
    90, 0.033, 95, 0.0141, 100, 0]
  indxpt = 100 - 100 * ((dur - 0.1) / dur)
  divindxf = stretch_envelope(indxfun, 50, atdrpt, 65, indxpt)
  indxf = make_env(:envelope, divindxf, :duration, dur,
                   :scaler, [hz2radians(index * fmrat * freq), PI].min)
  mindxf = make_env(:envelope, divindxf, :duration, dur,
                    :scaler, [hz2radians(index * casrat * freq), PI].min)
  devf = make_env(:envelope,
                  stretch_envelope(ampfun, 10, atdrpt, 90,
                                   [atdrpt + 1,
                                     100 - 100 * ((dur - 0.05) / dur)].max),
                  :scaler, [hz2radians(7000), PI].min, :duration, dur)
  rn = make_rand(:frequency, 7000, :amplitude, 1)
  carrier = make_oscil(:frequency, freq)
  fmosc = make_oscil(:frequency, freq * fmrat)
  cascade = make_oscil(:frequency, freq * casrat)
  run_instrument(start, dur,
                 :degree, degree,
                 :distance, distance,
                 :reverb_amount, rev_amount) do
    gls = env(glsf)
    env(ampf) *
      oscil(carrier,
            gls +
            env(indxf) *
            oscil(fmosc,
                  gls * fmrat +
                  env(mindxf) * oscil(cascade,
                                      gls * casrat +
                                      env(devf) * rand(rn))))
  end
end

def fm_drum_test(start = 0.0, dur = 1.0)
  $now = start
  fm_drum($now, dur, 55, 0.3, 5, false)
  $now += dur + 0.2
  fm_drum($now, dur, 66, 0.3, 4, true)
  $now += dur + 0.2
end

# FM-GONG
#
# Paul Weineke's gong.
def gong(start, dur, freq, amp, *args)
  degree, distance, reverb_amount = nil
  optkey(args, binding,
         [:degree, 0.0],
         [:distance, 1.0],
         [:reverb_amount, 0.005])
  mfq1 = freq * 1.16
  mfq2 = freq * 3.14
  mfq3 = freq * 1.005
  indx01 = hz2radians(0.01 * mfq1)
  indx11 = hz2radians(0.30 * mfq1)
  indx02 = hz2radians(0.01 * mfq2)
  indx12 = hz2radians(0.38 * mfq2)
  indx03 = hz2radians(0.01 * mfq3)
  indx13 = hz2radians(0.50 * mfq3)
  atpt = 5
  atdur = 100 * (0.002 / dur)
  expf = [0, 0, 3, 1, 15, 0.5, 27, 0.25, 50, 0.1, 100, 0]
  rise = [0, 0, 15, 0.3, 30, 1.0, 75, 0.5, 100, 0]
  fmup = [0, 0, 75, 1.0, 98, 1.0, 100, 0]
  fmdwn = [0, 0, 2, 1.0, 100, 0]
  ampfun = make_env(:envelope, stretch_envelope(expf, atpt, atdur),
                    :scaler, amp, :duration, dur)
  indxfun1 = make_env(:envelope, fmup, :scaler, indx11 - indx01,
                      :duration, dur, :offset, indx01)
  indxfun2 = make_env(:envelope, fmdwn, :scaler, indx12 - indx02,
                      :duration, dur, :offset, indx02)
  indxfun3 = make_env(:envelope, rise, :scaler, indx13 - indx03,
                      :duration, dur, :offset, indx03)
  carrier = make_oscil(:frequency, freq)
  mod1 = make_oscil(:frequency, mfq1)
  mod2 = make_oscil(:frequency, mfq2)
  mod3 = make_oscil(:frequency, mfq3)
  run_instrument(start, dur,
                 :degree, degree,
                 :distance, distance,
                 :reverb_amount, reverb_amount) do
    env(ampfun) *
      oscil(carrier,
            env(indxfun1) * oscil(mod1) +
            env(indxfun2) * oscil(mod2) +
            env(indxfun3) * oscil(mod3))
  end
end

def gong_test(start = 0.0, dur = 1.0)
  gong(start, dur, 261.61, 0.6)
  $now = start + dur + 0.2
end

# ATTRACT
#
# by James McCartney, from CMJ vol 21 no 3 p 6
def attract(start, dur, amp, c)
  a = b = 0.2
  dt = 0.04
  scale = (0.5 * amp) / c
  x = -1.0
  y = z = 0.0
  run_instrument(start, dur) do
    x1 = x - dt * (y + z)
    y = y + dt * (x + a * y)
    z = z + dt * ((b + x * z) - c * z)
    x = x1
    scale * x
  end
end

def attract_test(start = 0.0, dur = 1.0)
  attract(start, dur, 0.5, 2.0)
  $now = start + dur + 0.2
end

# PQW
#
# phase-quadrature waveshaping used to create asymmetric (i.e. single
# side-band) spectra.  The basic idea here is a variant of sin x sin y
# - cos x cos y = cos (x + y)
def pqw(start, dur, spacing_freq, carrier_freq,
        amp, ampfun, indexfun, partials, *args)
  degree, distance, reverb_amount = nil
  optkey(args, binding,
         [:degree, 0.0],
         [:distance, 1.0],
         [:reverb_amount, 0.005])
  normalized_partials = normalize_partials(partials)
  spacing_cos = make_oscil(:frequency, spacing_freq, :initial_phase, HALF_PI)
  spacing_sin = make_oscil(:frequency, spacing_freq)
  carrier_cos = make_oscil(:frequency, carrier_freq, :initial_phase, HALF_PI)
  carrier_sin = make_oscil(:frequency, carrier_freq)
  sin_coeffs = partials2polynomial(normalized_partials, 0)
  cos_coeffs = partials2polynomial(normalized_partials, 1)
  amp_env = make_env(:envelope, ampfun, :scaler, amp, :duration, dur)
  ind_env = make_env(:envelope, indexfun, :duration, dur)
  r = carrier_freq / spacing_freq.to_f
  tr = make_triangle_wave(:frequency, 5,
                          :amplitude, hz2radians(0.005 * spacing_freq))
  rn = make_rand_interp(:frequency, 12,
                        :amplitude, hz2radians(0.005 * spacing_freq))
  run_instrument(start, dur,
                 :degree, degree,
                 :distance, distance,
                 :reverb_amount, reverb_amount) do
    vib = triangle_wave(tr) + rand_interp(rn)
    ax = [1.0, env(ind_env)].min * oscil(spacing_cos, vib)
    fax = polynomial(cos_coeffs, ax)
    yfax = oscil(spacing_sin, vib) * polynomial(sin_coeffs, ax)
    env(amp_env) *
      (oscil(carrier_sin, vib * r) * yfax -
       oscil(carrier_cos, vib * r) * fax)
  end
end

def pqw_test(start = 0.0, dur = 1.0)
  pqw(start, dur, 200, 1000, 0.2,
      [0, 0, 25, 1, 100, 0], [0, 1, 100, 0], [2, 0.1, 3, 0.3, 6, 0.5])
  $now = start + dur + 0.2
  # to see the asymmetric spectrum most clearly, set the index function
  # above to [0, 1, 100, 1]
end

# taken from Perry Cook's stkv1.tar.Z (Synthesis Toolkit), but I was
# in a bit of a hurry and may not have made slavishly accurate
# translations.  Please let me (bil@ccrma.stanford.edu) know of any
# serious (non-envelope) errors.
#
# from Perry Cook's TubeBell.cpp
def tubebell(start, dur, freq, amp, base = 32.0)
  osc0 = make_oscil(freq * 0.995)
  osc1 = make_oscil(freq * 0.995 * 1.414)
  osc2 = make_oscil(freq * 1.005)
  osc3 = make_oscil(freq * 1.414)
  ampenv1 = make_env(:envelope, [0, 0, 0.005, 1, dur, 0],
                     :base, base, :duration, dur)
  ampenv2 = make_env(:envelope, [0, 0, 0.001, 1, dur, 0],
                     :base, 2 * base, :duration, dur)
  ampmod = make_oscil(:frequency, 2.0)
  g0 = 0.5 * amp * 0.707
  g1 = 0.203
  g2 = 0.5 * amp
  g3 = 0.144
  run_instrument(start, dur) do
    (0.007 *
     oscil(ampmod) + 0.993) *
     (g0 * env(ampenv1) * oscil(osc0, g1 * oscil(osc1)) +
      g2 * env(ampenv2) * oscil(osc2, g3 * oscil(osc3)))
  end
end

def tubebell_test(start = 0.0, dur = 1.0)
  tubebell(start, dur, 440, 0.2, 32)
  $now = start + dur + 0.2
end

# from Perry Cook's Wurley.cpp
def wurley(start, dur, freq, amp)
  osc0 = make_oscil(freq)
  osc1 = make_oscil(freq * 4.0)
  osc2 = make_oscil(510.0)
  osc3 = make_oscil(510.0)
  ampmod = make_oscil(:frequency, 8.0)
  g0 = 0.5 * amp
  g1 = 0.307
  g2 = 0.5 * amp * 0.307
  g3 = 0.117
  dur = [dur, 0.3].max
  ampenv = make_env(:envelope, [0, 0, 1, 1, 9, 1, 10, 0], :duration, dur)
  indenv = make_env(:envelope, [0, 0, 0.001, 1, 0.15, 0, dur, 0],
                    :duration, dur)
  resenv = make_env(:envelope, [0, 0, 0.001, 1, 0.25, 0, dur, 0],
                    :duration, dur)
  run_instrument(start, dur) do
    env(ampenv) *
      (1.0 + 0.007 * oscil(ampmod)) *
      (g0 * oscil(osc0, g1 * oscil(osc1)) +
       env(resenv) * g2 * oscil(osc2, g3 * env(indenv) * oscil(osc3)))
  end
end

def wurley_test(start = 0.0, dur = 1.0)
  wurley(start, dur, 440, 0.2)
  $now = start + dur + 0.2
end

# from Perry Cook's Rhodey.cpp
def rhodey(start, dur, freq, amp, base = 0.5)
  osc0 = make_oscil(freq)
  osc1 = make_oscil(freq * 0.5)
  osc2 = make_oscil(freq)
  osc3 = make_oscil(freq * 15.0)
  dur = [dur, 0.3].max
  ampenv1 = make_env(:envelope, [0, 0, 0.005, 1, dur, 0],
                     :base, base, :duration, dur)
  ampenv2 = make_env(:envelope, [0, 0, 0.001, 1, dur, 0],
                     :base, base * 1.5, :duration, dur)
  ampenv3 = make_env(:envelope, [0, 0, 0.001, 1, 0.25, 0, dur, 0],
                     :base, base * 4, :duration, dur)
  g0 = 0.5 * amp
  g1 = 0.535
  g2 = 0.5 * amp
  g3 = 0.109
  run_instrument(start, dur) do
    g0 * env(ampenv1) * oscil(osc0, g1 * oscil(osc1)) +
    g2 * env(ampenv2) * oscil(osc2, env(ampenv3) * g3 * oscil(osc3))
  end
end

def rhodey_test(start = 0.0, dur = 1.0)
  rhodey(start, dur, 440, 0.2, 0.5)
  $now = start + dur + 0.2
end

# from Perry Cook's BeeThree.cpp
def hammondoid(start, dur, freq, amp)
  osc0 = make_oscil(freq * 0.999)
  osc1 = make_oscil(freq * 1.997)
  osc2 = make_oscil(freq * 3.006)
  osc3 = make_oscil(freq * 6.009)
  dur = [dur, 0.1].max
  ampenv1 = make_env(:envelope, [0, 0, 0.005, 1, dur - 0.008, 1, dur, 0],
                     :duration, dur)
  ampenv2 = make_env(:envelope, [0, 0, 0.005, 1, dur, 0], :duration, dur)
  g0 = 0.25 * 0.75 * amp
  g1 = 0.25 * 0.75 * amp
  g2 = 0.5 * amp
  g3 = 0.5 * 0.75 * amp
  run_instrument(start, dur) do
    env(ampenv1) *
      (g0 * oscil(osc0) +
       g1 * oscil(osc1) +
       g2 * oscil(osc2)) +
       env(ampenv2) * g3 * oscil(osc3)
  end
end

def hammondoid_test(start = 0.0, dur = 1.0)
  hammondoid(start, dur, 440, 0.2)
  $now = start + dur + 0.2
end

# from Perry Cook's HeavyMtl.cpp
def metal(start, dur, freq, amp)
  osc0 = make_oscil(freq)
  osc1 = make_oscil(freq * 4.0 * 0.999)
  osc2 = make_oscil(freq * 3.0 * 1.001)
  osc3 = make_oscil(freq * 0.5 * 1.002)
  dur = [dur, 0.1].max
  ampenv0 = make_env(:envelope, [0, 0, 0.001, 1, dur - 0.002, 1, dur, 0],
                     :duration, dur)
  ampenv1 = make_env(:envelope, [0, 0, 0.001, 1, dur - 0.011, 1, dur, 0],
                     :duration, dur)
  ampenv2 = make_env(:envelope, [0, 0, 0.01, 1, dur - 0.015, 1, dur, 0],
                     :duration, dur)
  ampenv3 = make_env(:envelope, [0, 0, 0.03, 1, dur - 0.04, 1, dur, 0],
                     :duration, dur)
  g0 = 0.615 * amp
  g1 = 0.202
  g2 = 0.574
  g3 = 0.116
  run_instrument(start, dur) do
    g0 *
      env(ampenv0) *
      oscil(osc0,
            g1 * env(ampenv1) * oscil(osc1, g2 * env(ampenv2) * oscil(osc2)) +
            g3 * env(ampenv3) * oscil(osc3))
  end
end

def metal_test(start = 0.0, dur = 1.0)
  metal(start, dur, 440, 0.2)
  $now = start + dur + 0.2
end

# DRONE
def drone(start, dur, freq, amp, ampfun, synth,
          ampat, ampdc, amtrev, deg, dis, rvibamt, rvibfreq)
  waveform = partials2wave(synth)
  amp *= 0.25
  s = make_table_lookup(:frequency, freq, :wave, waveform)
  amp_env = make_env(:envelope,
                     stretch_envelope(ampfun, 25, 100 * (ampat / dur.to_f), 75,
                                      100 - 100 * (ampdc / dur.to_f)),
                     :scaler, amp, :duration, dur)
  ran_vib = make_rand(:frequency, rvibfreq,
                      :amplitude, hz2radians(rvibamt * freq))
  run_instrument(start, dur,
                 :distance, dis,
                 :degree, deg,
                 :reverb_amount, amtrev) do
    env(amp_env) * table_lookup(s, rand(ran_vib).abs)
  end
end

# CANTER
def canter(start, dur, pitch, amp, deg, dis, pcrev,
           ampfun, ranfun, skewfun, skewpc,
           ranpc, ranfreq, indexfun, atdr, dcdr,
           ampfun1, indfun1, fmtfun1,
           ampfun2, indfun2, fmtfun2,
           ampfun3, indfun3, fmtfun3,
           ampfun4, indfun4, fmtfun4)
  amp *= 0.25
  dur = dur.to_f
  pitch = pitch.to_f
  rangetop = 910.0
  rangebot = 400.0
  k = (100 * (log(pitch / rangebot) / log(rangetop / rangebot))).floor
  mfq = pitch
  atpt = 100 * (atdr / dur)
  dcpt = 100 - 100 * (dcdr / dur)
  lfmt1 = envelope_interp(k, fmtfun1)
  harm1 = (0.5 + lfmt1 / pitch).floor
  dev11 = hz2radians(envelope_interp(k, indfun1) * mfq)
  dev01 = dev11 * 0.5
  lamp1 = envelope_interp(k, ampfun1) * amp * (1 - (harm1 - lfmt1 / pitch).abs)
  lfmt2 = envelope_interp(k, fmtfun2)
  harm2 = (0.5 + lfmt2 / pitch).floor
  dev12 = hz2radians(envelope_interp(k, indfun2) * mfq)
  dev02 = dev12 * 0.5
  lamp2 = envelope_interp(k, ampfun2) * amp * (1 - (harm2 - lfmt2 / pitch).abs)
  lfmt3 = envelope_interp(k, fmtfun3)
  harm3 = (0.5 + lfmt3 / pitch).floor
  dev13 = hz2radians(envelope_interp(k, indfun3) * mfq)
  dev03 = dev13 * 0.5
  lamp3 = envelope_interp(k, ampfun3) * amp * (1 - (harm3 - lfmt3 / pitch).abs)
  lfmt4 = envelope_interp(k, fmtfun4)
  harm4 = (0.5 + lfmt4 / pitch).floor
  dev14 = hz2radians(envelope_interp(k, indfun4) * mfq)
  dev04 = dev14 * 0.5
  lamp4 = envelope_interp(k, ampfun4) * amp * (1 - (harm4 - lfmt4 / pitch).abs)
  tampfun = make_env(:envelope, stretch_envelope(ampfun, 25, atpt, 75, dcpt),
                     :duration, dur)
  tskwfun = make_env(:envelope, stretch_envelope(skewfun, 25, atpt, 75, dcpt),
                     :scaler, hz2radians(pitch * skewpc.to_f), :duration, dur)
  tranfun = make_env(:envelope, stretch_envelope(ranfun, 25, atpt, 75, dcpt),
                     :duration, dur)
  tidxfun = make_env(:envelope, stretch_envelope(indexfun, 25, atpt, 75, dcpt),
                     :duration, dur)
  modgen = make_oscil(:frequency, pitch)
  gen1 = make_oscil(:frequency, pitch * harm1)
  gen2 = make_oscil(:frequency, pitch * harm2)
  gen3 = make_oscil(:frequency, pitch * harm3)
  gen4 = make_oscil(:frequency, pitch * harm4)
  ranvib = make_rand(:frequency, ranfreq, :amplitude, hz2radians(ranpc * pitch))
  run_instrument(start, dur,
                 :degree, deg,
                 :distance, dis,
                 :reverb_amount, pcrev) do
    frqval = env(tskwfun) + env(tranfun) * rand(ranvib)
    modval = oscil(modgen, frqval)
    ampval = env(tampfun)
    indval = env(tidxfun)
    lamp1 *
      ampval *
      oscil(gen1, ((dev01 + indval * dev11) * modval + frqval) * harm1) +
      lamp2 *
      ampval *
      oscil(gen2, ((dev02 + indval * dev12) * modval + frqval) * harm2) +
      lamp3 *
      ampval *
      oscil(gen3, ((dev03 + indval * dev13) * modval + frqval) * harm3) +
      lamp4 *
      ampval *
      oscil(gen4, ((dev04 + indval * dev14) * modval + frqval) * harm4)
  end
end

# NREV (the most popular Samson box reverb)
#
# reverb_factor controls the length of the decay -- it should not
#   exceed (/ 1.0 .823)
# lp_coeff controls the strength of the low pass filter inserted
#   in the feedback loop
# volume can be used to boost the reverb output
def nrev_rb(*args)
  reverb_factor, lp_coeff, volume = nil
  optkey(args, binding,
         [:reverb_factor, 1.09],
         [:lp_coeff, 0.7],
         [:volume, 1.0])
  next_prime = lambda do |val|
    if val.prime?
      val
    else
      next_prime.call(val + 2)
    end
  end
  srscale = @srate / 25641
  dly_len = [1433, 1601, 1867, 2053, 2251, 2399,
    347, 113, 37, 59, 53, 43, 37, 29, 19]
  dly_len.map! do |x|
    val = (x * srscale).round
    val += 1 if val.even?
    next_prime.call(val)
  end
  comb1 = make_comb(0.822 * reverb_factor, dly_len[0])
  comb2 = make_comb(0.802 * reverb_factor, dly_len[1])
  comb3 = make_comb(0.773 * reverb_factor, dly_len[2])
  comb4 = make_comb(0.753 * reverb_factor, dly_len[3])
  comb5 = make_comb(0.753 * reverb_factor, dly_len[4])
  comb6 = make_comb(0.733 * reverb_factor, dly_len[5])
  low = make_one_pole(lp_coeff, lp_coeff - 1.0)
  chan2 = (@channels > 1)
  chan4 = (@channels == 4)
  allpass1 = make_all_pass(-0.7, 0.7, dly_len[6])
  allpass2 = make_all_pass(-0.7, 0.7, dly_len[7])
  allpass3 = make_all_pass(-0.7, 0.7, dly_len[8])
  allpass4 = make_all_pass(-0.7, 0.7, dly_len[9]) # 10 for quad
  allpass5 = make_all_pass(-0.7, 0.7, dly_len[11])
  allpass6 = (chan2 ? make_all_pass(-0.7, 0.7, dly_len[12]) : nil)
  allpass7 = (chan4 ? make_all_pass(-0.7, 0.7, dly_len[13]) : nil)
  allpass8 = (chan4 ? make_all_pass(-0.7, 0.7, dly_len[14]) : nil)
  reverb_frame = make_frame(@channels)
  run_reverb() do |val, i|
    rev = volume * val
    outrev = all_pass(allpass4,
                      one_pole(low,
                               all_pass(allpass3,
                                        all_pass(allpass2,
                                                 all_pass(allpass1,
                                                          comb(comb1, rev) +
                                                          comb(comb2, rev) +
                                                          comb(comb3, rev) +
                                                          comb(comb4, rev) +
                                                          comb(comb5, rev) +
                                                          comb(comb6, rev))))))
    frame_set!(reverb_frame, 0, all_pass(allpass5, outrev))
    frame_set!(reverb_frame, 1, all_pass(allpass6, outrev)) if chan2
    frame_set!(reverb_frame, 2, all_pass(allpass7, outrev)) if chan4
    frame_set!(reverb_frame, 3, all_pass(allpass8, outrev)) if chan4
    reverb_frame
  end
end

class Snd_Instrument
  alias nrev nrev_rb
end

def drone_canter_test(start = 0.0, dur = 1.0)
  fmt1 = [0, 1200, 100, 1000]
  fmt2 = [0, 2250, 100, 1800]
  fmt3 = [0, 4500, 100, 4500]
  fmt4 = [0, 6750, 100, 8100]
  amp1 = [0, 0.67, 100, 0.7]
  amp2 = [0, 0.95, 100, 0.95]
  amp3 = [0, 0.28, 100, 0.33]
  amp4 = [0, 0.14, 100, 0.15]
  ind1 = [0, 0.75, 100, 0.65]
  ind2 = [0, 0.75, 100, 0.75]
  ind3 = [0, 1, 100, 1]
  ind4 = [0, 1, 100, 1]
  skwf = [0, 0, 100, 0]
  ampf = [0, 0, 25, 1, 75, 1, 100, 0]
  ranf = [0, 0.5, 100, 0.5]
  index = [0, 1, 100, 1]
  solid = [0, 0, 5, 1, 95, 1, 100, 0]
  bassdr2 = [0.5, 0.06, 1, 0.62, 1.5, 0.07, 2, 0.6, 2.5, 0.08, 3, 0.56,
    4, 0.24, 5, 0.98, 6, 0.53, 7, 0.16, 8, 0.33, 9, 0.62, 10, 0.12,
    12, 0.14, 14, 0.86, 16, 0.12, 23, 0.14, 24, 0.17]
  tenordr = [0.3, 0.04, 1, 0.81, 2, 0.27, 3, 0.2, 4, 0.21, 5, 0.18,
    6, 0.35, 7, 0.03, 8, 0.07, 9, 0.02, 10, 0.025, 11, 0.035]

  $now = start
  drone($now, 4, 115, 0.125, solid, bassdr2, 0.1, 0.5, 0.03, 45, 1, 0.01, 10)
  drone($now, 4, 229, 0.125, solid, tenordr, 0.1, 0.5, 0.03, 45, 1, 0.01, 11)
  drone($now, 4, 229.5, 0.125, solid, tenordr, 0.1, 0.5, 0.03, 45, 1, 0.01, 9)
  canter($now, 2.100, 918.000, 0.175, 45.0, 1, 0.05,
         ampf, ranf, skwf, 0.050, 0.01, 10, index, 0.005, 0.005,
         amp1, ind1, fmt1, amp2, ind2, fmt2, amp3, ind3, fmt3, amp4, ind4, fmt4)
  canter($now + 2.100, 0.300, 688.500, 0.175, 45.0, 1, 0.05,
         ampf, ranf, skwf, 0.050, 0.01, 10, index, 0.005, 0.005,
         amp1, ind1, fmt1, amp2, ind2, fmt2, amp3, ind3, fmt3, amp4, ind4, fmt4)
  canter($now + 2.400, 0.040, 826.200, 0.175, 45.0, 1, 0.05,
         ampf, ranf, skwf, 0.050, 0.01, 10, index, 0.005, 0.005,
         amp1, ind1, fmt1, amp2, ind2, fmt2, amp3, ind3, fmt3, amp4, ind4, fmt4)
  canter($now + 2.440, 0.560, 459.000, 0.175, 45.0, 1, 0.05,
         ampf, ranf, skwf, 0.050, 0.01, 10, index, 0.005, 0.005,
         amp1, ind1, fmt1, amp2, ind2, fmt2, amp3, ind3, fmt3, amp4, ind4, fmt4)
  canter($now + 3.000, 0.040, 408.000, 0.175, 45.0, 1, 0.05,
         ampf, ranf, skwf, 0.050, 0.01, 10, index, 0.005, 0.005,
         amp1, ind1, fmt1, amp2, ind2, fmt2, amp3, ind3, fmt3, amp4, ind4, fmt4)
  canter($now + 3.040, 0.040, 619.650, 0.175, 45.0, 1, 0.05,
         ampf, ranf, skwf, 0.050, 0.01, 10, index, 0.005, 0.005,
         amp1, ind1, fmt1, amp2, ind2, fmt2, amp3, ind3, fmt3, amp4, ind4, fmt4)
  canter($now + 3.080, 0.040, 408.000, 0.175, 45.0, 1, 0.05,
         ampf, ranf, skwf, 0.050, 0.01, 10, index, 0.005, 0.005,
         amp1, ind1, fmt1, amp2, ind2, fmt2, amp3, ind3, fmt3, amp4, ind4, fmt4)
  canter($now + 3.120, 0.040, 688.500, 0.175, 45.0, 1, 0.05,
         ampf, ranf, skwf, 0.050, 0.01, 10, index, 0.005, 0.005,
         amp1, ind1, fmt1, amp2, ind2, fmt2, amp3, ind3, fmt3, amp4, ind4, fmt4)
  canter($now + 3.160, 0.290, 459.000, 0.175, 45.0, 1, 0.05,
         ampf, ranf, skwf, 0.050, 0.01, 10, index, 0.005, 0.005,
         amp1, ind1, fmt1, amp2, ind2, fmt2, amp3, ind3, fmt3, amp4, ind4, fmt4)
  canter($now + 3.450, 0.150, 516.375, 0.175, 45.0, 1, 0.05,
         ampf, ranf, skwf, 0.050, 0.01, 10, index, 0.005, 0.005,
         amp1, ind1, fmt1, amp2, ind2, fmt2, amp3, ind3, fmt3, amp4, ind4, fmt4)
  canter($now + 3.600, 0.040, 826.200, 0.175, 45.0, 1, 0.05,
         ampf, ranf, skwf, 0.050, 0.01, 10, index, 0.005, 0.005,
         amp1, ind1, fmt1, amp2, ind2, fmt2, amp3, ind3, fmt3, amp4, ind4, fmt4)
  canter($now + 3.640, 0.040, 573.750, 0.175, 45.0, 1, 0.05,
         ampf, ranf, skwf, 0.050, 0.01, 10, index, 0.005, 0.005,
         amp1, ind1, fmt1, amp2, ind2, fmt2, amp3, ind3, fmt3, amp4, ind4, fmt4)
  canter($now + 3.680, 0.040, 619.650, 0.175, 45.0, 1, 0.05,
         ampf, ranf, skwf, 0.050, 0.01, 10, index, 0.005, 0.005,
         amp1, ind1, fmt1, amp2, ind2, fmt2, amp3, ind3, fmt3, amp4, ind4, fmt4)
  canter($now + 3.720, 0.180, 573.750, 0.175, 45.0, 1, 0.05,
         ampf, ranf, skwf, 0.050, 0.01, 10, index, 0.005, 0.005,
         amp1, ind1, fmt1, amp2, ind2, fmt2, amp3, ind3, fmt3, amp4, ind4, fmt4)
  canter($now + 3.900, 0.040, 688.500, 0.175, 45.0, 1, 0.05,
         ampf, ranf, skwf, 0.050, 0.01, 10, index, 0.005, 0.005,
         amp1, ind1, fmt1, amp2, ind2, fmt2, amp3, ind3, fmt3, amp4, ind4, fmt4)
  canter($now + 3.940, 0.260, 459.000, 0.175, 45.0, 1, 0.05,
         ampf, ranf, skwf, 0.050, 0.01, 10, index, 0.005, 0.005,
         amp1, ind1, fmt1, amp2, ind2, fmt2, amp3, ind3, fmt3, amp4, ind4, fmt4)
  $now += 4.4
end

# RESON
def reson(start, dur, pitch, amp, numformants,
          indxfun, skewfun, pcskew, skewat, skewdc,
          vibfreq, vibpc, ranvibfreq, ranvibpc,
          degree, distance, rev_amount, data)
  # data is a list of lists of form
  # [ampf, resonfrq, resonamp, ampat, ampdc, dev0, dev1, indxat, indxdc]
  dur = dur.to_f
  pitch = pitch.to_f
  modulator = make_oscil(:frequency, pitch)
  carriers = Array.new(numformants)
  ampfs = Array.new(numformants)
  indfs = Array.new(numformants)
  c_rats = Array.new(numformants)
  frqf = make_env(:envelope,
                  stretch_envelope(skewfun, 25, 100 * (skewat / dur), 75,
                                   100 - (100 * (skewdc / dur))),
                  :scaler, hz2radians(pcskew * pitch), :duration, dur)
  pervib = make_triangle_wave(:frequency, vibfreq,
                              :amplitude, hz2radians(vibpc * pitch))
  ranvib = make_rand_interp(:frequency, ranvibfreq,
                            :amplitude, hz2radians(ranvibpc * pitch))
  totalamp = 0.0
  numformants.times do |i| totalamp += data[i][2] end
  numformants.times do |i|
    frmdat = data[i]
    ampf = frmdat[0]
    freq = frmdat[1]
    rfamp = frmdat[2]
    ampat = 100 * (frmdat[3] / dur)
    ampdc = 100 - 100 * (frmdat[4] / dur)
    dev0 = hz2radians(frmdat[5] * freq)
    dev1 = hz2radians(frmdat[6] * freq)
    indxat = 100 * (frmdat[7] / dur)
    indxdc = 100 - 100 * (frmdat[8] / dur)
    harm = (freq / pitch).round
    rsamp = 1.0 - (harm - freq / pitch).abs
    cfq = pitch * harm
    ampat = 25 if ampat.zero?
    ampdc = 75 if ampdc.zero?
    indxat = 25 if indxat.zero?
    indxdc = 75 if indxdc.zero?
    indfs[i] = make_env(:envelope,
                        stretch_envelope(indxfun, 25, indxat, 75, indxdc),
                        :scaler, dev1 - dev0,
                        :offset, dev0,
                        :duration, dur)
    ampfs[i] = make_env(:envelope,
                        stretch_envelope(ampf, 25, ampat, 75, ampdc),
                        :scaler, rsamp * amp * (rfamp / totalamp),
                        :duration, dur)
    c_rats[i] = harm
    carriers[i] = make_oscil(:frequency, cfq)
  end
  run_instrument(start, dur,
                 :degree, degree,
                 :distance, distance,
                 :reverb_amount, rev_amount) do
    vib = triangle_wave(pervib) + rand_interp(ranvib) + env(frqf)
    modsig = oscil(modulator, vib)
    outsum = 0.0
    numformants.times do |j|
      outsum += env(ampfs[j]) *
        oscil(carriers[j],
              vib * c_rats[j] +
              env(indfs[j]) * modsig)
    end
    outsum
  end
end

def reson_test(start = 0.0, dur = 1.0)
  data = [[[0, 0, 100, 1], 1200, 0.5, 0.1, 0.1, 0, 1.0, 0.1, 0.1],
    [[0, 1, 100, 0], 2400, 0.5, 0.1, 0.1, 0, 1.0, 0.1, 0.1]]

  reson(start, dur, 440, 0.5, 2, [0, 0, 100, 1], [0, 0, 100, 1],
        0.1, 0.1, 0.1, 5, 0.01, 5, 0.01, 0, 1.0, 0.01, data)
  $now = start + dur + 0.2
end

# STK's feedback-fm instrument named CelloN in Sambox-land
def cellon(start, dur, pitch0, amp, ampfun, betafun,
           beta0, beta1, betaat, betadc, ampat, ampdc, dis, pcrev, deg, pitch1,
           glissfun = [0, 0, 100, 0], glissat = 0.0, glissdc = 0.0,
           pvibfreq = 0.0, pvibpc = 0.0,
           pvibfun = [0, 1, 100, 1], pvibat = 0.0, pvibdc = 0.0,
           rvibfreq = 0.0, rvibpc = 0.0, rvibfun = [0, 1, 100, 1])

  pit1 = pitch1.zero? ? pitch0 : pitch1
  car = make_oscil(:frequency, pitch0)
  low = make_one_zero(0.5, -0.5)
  fmosc = make_oscil(:frequency, pitch0)
  fm = 0.0
  dur = dur.to_f
  pitch0 = pitch0.to_f
  pvib = make_triangle_wave(:frequency, pvibfreq, :amplitude, 1.0)
  rvib = make_rand_interp(:frequency, rvibfreq, :amplitude, 1.0)
  ampap = (ampat > 0.0 ? (100 * (ampat / dur)) : 25)
  ampdp = (ampdc > 0.0 ? (100 * (1.0 - ampdc / dur)) : 75)
  glsap = (glissat > 0.0 ? (100 * (glissat / dur)) : 25)
  glsdp = (glissdc > 0.0 ? (100 * (1.0 - glissdc / dur)) : 75)
  betap = (betaat > 0.0 ? (100 * (betaat / dur)) : 25)
  betdp = (betadc > 0.0 ? (100 * (1.0 - betadc / dur)) : 75)
  pvbap = (pvibat > 0.0 ? (100 * (pvibat / dur)) : 25)
  pvbdp = (pvibdc > 0.0 ? (100 * (1.0 - pvibdc / dur)) : 75)
  pvibenv = make_env(:envelope,
                     stretch_envelope(pvibfun, 25, pvbap, 75, pvbdp),
                     :scaler, hz2radians(pvibpc * pitch0),
                     :duration, dur)
  rvibenv = make_env(:envelope, stretch_envelope(rvibfun),
                     :duration, dur,
                     :scaler, hz2radians(rvibpc * pitch0))
  glisenv = make_env(:envelope,
                     stretch_envelope(glissfun, 25, glsap, 75, glsdp),
                     :scaler, hz2radians(pit1 - pitch0),
                     :duration, dur)
  amplenv = make_env(:envelope, stretch_envelope(ampfun, 25, ampap, 75, ampdp),
                     :scaler, amp,
                     :duration, dur)
  betaenv = make_env(:envelope, stretch_envelope(betafun, 25, betap, 75, betdp),
                     :scaler, beta1 - beta0,
                     :offset, beta0,
                     :duration, dur)
  run_instrument(start, dur,
                 :degree, deg,
                 :distance, dis,
                 :reverb_amount, pcrev) do
    vib = env(pvibenv) * triangle_wave(pvib) +
      env(rvibenv) * rand_interp(rvib) +
      env(glisenv)
    fm = one_zero(low, env(betaenv) * oscil(fmosc, fm + vib))
    env(amplenv) * oscil(car, fm + vib)
  end
end

def cellon_test(start = 0.0, dur = 1.0)
  cellon(start, dur, 220, 0.5, 
         [0, 0, 25, 1, 75, 1, 100, 0],        # ampfun
         [0, 0, 25, 1, 75, 1, 100, 0],        # betafun
         0.75, 1.0, 0, 0, 0, 0, 1, 0, 0, 220, 
         [0, 0, 25, 1, 75, 1, 100, 0],        # glissfun
         0, 0, 0, 0,
         [0, 0, 100, 0],                      # pvibfun
         0, 0, 0, 0,
         [0, 0, 100, 0])                      # rvibfun
  $now = start + dur + 0.2
end

# JL-REVERB
def jl_reverb(*args)
  allpass1 = make_all_pass(-0.7, 0.7, 2111)
  allpass2 = make_all_pass(-0.7, 0.7,  673)
  allpass3 = make_all_pass(-0.7, 0.7,  223)
  comb1 = make_comb(0.742,  9601)
  comb2 = make_comb(0.733, 10007)
  comb3 = make_comb(0.715, 10799)
  comb4 = make_comb(0.697, 11597)
  outdel1 = make_delay((0.013 * @srate).round)
  outdel2 = (@channels > 1 ? make_delay((0.011 * @srate).round) : false)
  reverb_frame = make_frame(@channels)
  run_reverb() do |ho, i|
    allpass_sum = all_pass(allpass3, all_pass(allpass2, all_pass(allpass1, ho)))
    comb_sum = (comb(comb1, allpass_sum) + comb(comb2, allpass_sum) +
                comb(comb3, allpass_sum) + comb(comb4, allpass_sum))
    frame_set!(reverb_frame, 0, delay(outdel1, comb_sum))
    frame_set!(reverb_frame, 1, delay(outdel2, comb_sum)) if outdel2
    reverb_frame
  end
end

# GRAN-SYNTH
def gran_synth(start, dur, freq, grain_dur, interval, amp)
  grain_env = make_env(:envelope, [0, 0, 25, 1, 75, 1, 100, 0],
                       :duration, grain_dur)
  carrier = make_oscil(:frequency, freq)
  grain_size = ([grain_dur, interval].max * @srate).ceil
  grains = make_wave_train(:size, grain_size, :frequency, 1.0 / interval)
  grain = mus_data(grains)
  grain_size.times do |i| grain[i] = env(grain_env) * oscil(carrier) end
  run_instrument(start, dur) do
    amp * wave_train(grains)
  end
end

def gran_synth_test(start = 0.0, dur = 1.0)
  gran_synth(start, dur, 100, 0.0189, 0.02, 0.4)
  $now = start + dur + 0.2
end

# TOUCH-TONE
def touch_tone(start, number)
  touch_tab_1 = [0, 697, 697, 697, 770, 770, 770, 852, 852, 852, 941, 941, 941]
  touch_tab_2 = [0, 1209, 1336, 1477, 1209, 1336,
    1477, 1209, 1336, 1477, 1209, 1336, 1477]
  number.length.times do |i|
    k = number[i]
    ii = if k.kind_of?(Numeric)
          k.zero? ? 11 : k
        else
          k == ?* ? 10 : 12
        end
    frq1 = make_oscil(:frequency, touch_tab_1[ii])
    frq2 = make_oscil(:frequency, touch_tab_2[ii])
    run_instrument(start + i * 0.3, 0.2) do
      0.25 * (oscil(frq1) + oscil(frq2))
    end
  end
end

def touch_tone_test(start = 0.0, dur = 1.0)
  touch_tone(start, [4, 8, 3, 4, 6, 2, 1])
  $now = start + dur * 7 + 0.2  # 7 digits
end

# SPECTRA
def spectra(start, dur, freq, amp,
            partials = [1, 1, 2, 0.5],
            amp_envelope = [0, 0, 50, 1, 100, 0],
            vibrato_amplitude = 0.005,
            vibrato_speed = 5.0,
            degree = 0.0,
            distance = 1.0,
            rev_amount = 0.005)
  waveform = partials2wave(partials)
  frq = hz2radians(freq)
  s = make_table_lookup(:frequency, freq, :wave, waveform)
  amp_env = make_env(:envelope, amp_envelope, :scaler, amp, :duration, dur)
  per_vib = make_triangle_wave(:frequency, vibrato_speed,
                               :amplitude, vibrato_amplitude * frq)
  ran_vib = make_rand_interp(:frequency, vibrato_speed + 1.0,
                             :amplitude, vibrato_amplitude * frq)
  run_instrument(start, dur,
                 :degree, degree,
                 :distance, distance,
                 :reverb_amount, rev_amount) do
    env(amp_env) *
      table_lookup(s, triangle_wave(per_vib) + rand_interp(ran_vib))
  end
end

def spectra_test(start = 0.0, dur = 1.0)
  spectra(start, dur, 440.0, 0.8, P_a4,
          [0, 0, 1, 1, 5, 0.9, 12, 0.5, 25, 0.25, 100, 0])
  $now = start + dur + 0.2
end

# TWO-TAB
#
# interpolate between two waveforms (this could be extended to
# implement all the various wavetable-based synthesis techniques).
def two_tab(start, dur, freq, amp,
            partial_1 = [1.0, 1.0, 2.0, 0.5],
            partial_2 = [1.0, 0.0, 3.0, 1.0],
            amp_envelope = [0, 0, 50, 1, 100, 0],
            interp_func = [0, 1, 100, 0],
            vibrato_amplitude = 0.005,
            vibrato_speed = 5.0,
            degree = 0.0,
            distance = 1.0,
            rev_amount = 0.005)
  waveform_1 = partials2wave(partial_1)
  waveform_2 = partials2wave(partial_2)
  frq = hz2radians(freq)
  s_1 = make_table_lookup(:frequency, freq, :wave, waveform_1)
  s_2 = make_table_lookup(:frequency, freq, :wave, waveform_2)
  amp_env = make_env(:envelope, amp_envelope, :scaler, amp, :duration, dur)
  interp_env = make_env(:envelope, interp_func, :duration, dur)
  per_vib = make_triangle_wave(:frequency, vibrato_speed,
                               :amplitude, vibrato_amplitude * frq)
  ran_vib = make_rand_interp(:frequency, vibrato_speed + 1.0,
                             :amplitude, vibrato_amplitude * frq)
  run_instrument(start, dur,
                 :degree, degree,
                 :distance, distance,
                 :reverb_amount, rev_amount) do
    vib = triangle_wave(per_vib) + rand_interp(ran_vib)
    intrp = env(interp_env)
    env(amp_env) *
      (intrp * table_lookup(s_1, vib) +
       (1.0 - intrp) * table_lookup(s_2, vib))
  end
end

def two_tab_test(start = 0.0, dur = 1.0)
  two_tab(start, dur, 440, 0.5)
  $now = start + dur + 0.2
end

# LBJ-PIANO
$clm_piano_attack_duration = 0.04
$clm_piano_release_duration = 0.2
$clm_db_drop_per_second = -10.0

Piano_Spectra = [[1.97, 0.0326, 2.99, 0.0086, 3.95, 0.0163, 4.97,
  0.0178, 5.98, 0.0177, 6.95, 0.0315, 8.02, 0.0001, 8.94, 0.0076,
  9.96, 0.0134, 10.99, 0.0284, 11.98, 0.0229, 13.02, 0.0229, 13.89,
  0.0010, 15.06, 0.0090, 16.00, 0.0003, 17.08, 0.0078, 18.16, 0.0064,
  19.18, 0.0129, 20.21, 0.0085, 21.27, 0.0225, 22.32, 0.0061, 23.41,
  0.0102, 24.48, 0.0005, 25.56, 0.0016, 26.64, 0.0018, 27.70, 0.0113,
  28.80, 0.0111, 29.91, 0.0158, 31.06, 0.0093, 32.17, 0.0017, 33.32,
  0.0002, 34.42, 0.0018, 35.59, 0.0027, 36.74, 0.0055, 37.90, 0.0037,
  39.06, 0.0064, 40.25, 0.0033, 41.47, 0.0014, 42.53, 0.0004, 43.89,
  0.0010, 45.12, 0.0039, 46.33, 0.0039, 47.64, 0.0009, 48.88, 0.0016,
  50.13, 0.0006, 51.37, 0.0010, 52.70, 0.0002, 54.00, 0.0004, 55.30,
  0.0008, 56.60, 0.0025, 57.96, 0.0010, 59.30, 0.0012, 60.67, 0.0011,
  61.99, 0.0003, 62.86, 0.0001, 64.36, 0.0005, 64.86, 0.0001, 66.26,
  0.0004, 67.70, 0.0006, 68.94, 0.0002, 70.10, 0.0001, 70.58, 0.0002,
  72.01, 0.0007, 73.53, 0.0006, 75.00, 0.0002, 77.03, 0.0005, 78.00,
  0.0002, 79.57, 0.0006, 81.16, 0.0005, 82.70, 0.0005, 84.22, 0.0003,
  85.41, 0.0002, 87.46, 0.0001, 90.30, 0.0001, 94.02, 0.0001, 95.26,
  0.0002, 109.39, 0.0003],
  [1.98, 0.0194, 2.99, 0.0210, 3.97, 0.0276, 4.96, 0.0297, 5.96, 0.0158,
    6.99, 0.0207, 8.01, 0.0009, 9.00, 0.0101, 10.00, 0.0297, 11.01,
    0.0289, 12.02, 0.0211, 13.04, 0.0127, 14.07, 0.0061, 15.08, 0.0174,
    16.13, 0.0009, 17.12, 0.0093, 18.16, 0.0117, 19.21, 0.0122, 20.29,
    0.0108, 21.30, 0.0077, 22.38, 0.0132, 23.46, 0.0073, 24.14, 0.0002,
    25.58, 0.0026, 26.69, 0.0035, 27.77, 0.0053, 28.88, 0.0024, 30.08,
    0.0027, 31.13, 0.0075, 32.24, 0.0027, 33.36, 0.0004, 34.42, 0.0004,
    35.64, 0.0019, 36.78, 0.0037, 38.10, 0.0009, 39.11, 0.0027, 40.32,
    0.0010, 41.51, 0.0013, 42.66, 0.0019, 43.87, 0.0007, 45.13, 0.0017,
    46.35, 0.0019, 47.65, 0.0021, 48.89, 0.0014, 50.18, 0.0023, 51.42,
    0.0015, 52.73, 0.0002, 54.00, 0.0005, 55.34, 0.0006, 56.60, 0.0010,
    57.96, 0.0016, 58.86, 0.0005, 59.30, 0.0004, 60.75, 0.0005, 62.22,
    0.0003, 63.55, 0.0005, 64.82, 0.0003, 66.24, 0.0003, 67.63, 0.0011,
    69.09, 0.0007, 70.52, 0.0004, 72.00, 0.0005, 73.50, 0.0008, 74.95,
    0.0003, 77.13, 0.0013, 78.02, 0.0002, 79.48, 0.0004, 82.59, 0.0004,
    84.10, 0.0003],
   [2.00, 0.0313, 2.99, 0.0109, 4.00, 0.0215, 5.00, 0.0242, 5.98, 0.0355,
     7.01, 0.0132, 8.01, 0.0009, 9.01, 0.0071, 10.00, 0.0258, 11.03,
     0.0221, 12.02, 0.0056, 13.06, 0.0196, 14.05, 0.0160, 15.11, 0.0107,
     16.11, 0.0003, 17.14, 0.0111, 18.21, 0.0085, 19.23, 0.0010, 20.28,
     0.0048, 21.31, 0.0128, 22.36, 0.0051, 23.41, 0.0041, 24.05, 0.0006,
     25.54, 0.0019, 26.62, 0.0028, 27.72, 0.0034, 28.82, 0.0062, 29.89,
     0.0039, 30.98, 0.0058, 32.08, 0.0011, 33.21, 0.0002, 34.37, 0.0008,
     35.46, 0.0018, 36.62, 0.0036, 37.77, 0.0018, 38.92, 0.0042, 40.07,
     0.0037, 41.23, 0.0011, 42.67, 0.0003, 43.65, 0.0018, 44.68, 0.0025,
     45.99, 0.0044, 47.21, 0.0051, 48.40, 0.0044, 49.67, 0.0005, 50.88,
     0.0019, 52.15, 0.0003, 53.42, 0.0008, 54.69, 0.0010, 55.98, 0.0005,
     57.26, 0.0013, 58.53, 0.0027, 59.83, 0.0011, 61.21, 0.0027, 62.54,
     0.0003, 63.78, 0.0003, 65.20, 0.0001, 66.60, 0.0006, 67.98, 0.0008,
     69.37, 0.0019, 70.73, 0.0007, 72.14, 0.0004, 73.62, 0.0002, 74.40,
     0.0003, 76.52, 0.0006, 77.97, 0.0002, 79.49, 0.0004, 80.77, 0.0003,
     81.00, 0.0001, 82.47, 0.0005, 83.97, 0.0001, 87.27, 0.0002],
   [2.00, 0.0257, 2.99, 0.0142, 3.97, 0.0202, 4.95, 0.0148, 5.95, 0.0420,
     6.95, 0.0037, 7.94, 0.0004, 8.94, 0.0172, 9.95, 0.0191, 10.96, 0.0115,
     11.97, 0.0059, 12.98, 0.0140, 14.00, 0.0178, 15.03, 0.0121, 16.09,
     0.0002, 17.07, 0.0066, 18.08, 0.0033, 19.15, 0.0022, 20.18, 0.0057,
     21.22, 0.0077, 22.29, 0.0037, 23.33, 0.0066, 24.97, 0.0002, 25.49,
     0.0019, 26.55, 0.0042, 27.61, 0.0043, 28.73, 0.0038, 29.81, 0.0084,
     30.91, 0.0040, 32.03, 0.0025, 33.14, 0.0005, 34.26, 0.0003, 35.38,
     0.0019, 36.56, 0.0037, 37.68, 0.0049, 38.86, 0.0036, 40.11, 0.0011,
     41.28, 0.0008, 42.50, 0.0004, 43.60, 0.0002, 44.74, 0.0022, 45.99,
     0.0050, 47.20, 0.0009, 48.40, 0.0036, 49.68, 0.0004, 50.92, 0.0009,
     52.17, 0.0005, 53.46, 0.0007, 54.76, 0.0006, 56.06, 0.0005, 57.34,
     0.0011, 58.67, 0.0005, 59.95, 0.0015, 61.37, 0.0008, 62.72, 0.0004,
     65.42, 0.0009, 66.96, 0.0003, 68.18, 0.0003, 69.78, 0.0003, 71.21,
     0.0004, 72.45, 0.0002, 74.22, 0.0003, 75.44, 0.0001, 76.53, 0.0003,
     78.31, 0.0004, 79.83, 0.0003, 80.16, 0.0001, 81.33, 0.0003, 82.44,
     0.0001, 83.17, 0.0002, 84.81, 0.0003, 85.97, 0.0003, 89.08, 0.0001,
     90.70, 0.0002, 92.30, 0.0002, 95.59, 0.0002, 97.22, 0.0003, 98.86,
     0.0001, 108.37, 0.0001, 125.54, 0.0001],
   [1.99, 0.0650, 3.03, 0.0040, 4.03, 0.0059, 5.02, 0.0090, 5.97, 0.0227,
     6.98, 0.0050, 8.04, 0.0020, 9.00, 0.0082, 9.96, 0.0078, 11.01, 0.0056,
     12.01, 0.0095, 13.02, 0.0050, 14.04, 0.0093, 15.08, 0.0064, 16.14,
     0.0017, 17.06, 0.0020, 18.10, 0.0025, 19.14, 0.0023, 20.18, 0.0015,
     21.24, 0.0032, 22.29, 0.0029, 23.32, 0.0014, 24.37, 0.0005, 25.43,
     0.0030, 26.50, 0.0022, 27.60, 0.0027, 28.64, 0.0024, 29.76, 0.0035,
     30.81, 0.0136, 31.96, 0.0025, 33.02, 0.0003, 34.13, 0.0005, 35.25,
     0.0007, 36.40, 0.0014, 37.51, 0.0020, 38.64, 0.0012, 39.80, 0.0019,
     40.97, 0.0004, 42.09, 0.0003, 43.24, 0.0003, 44.48, 0.0002, 45.65,
     0.0024, 46.86, 0.0005, 48.07, 0.0013, 49.27, 0.0008, 50.49, 0.0006,
     52.95, 0.0001, 54.23, 0.0005, 55.45, 0.0004, 56.73, 0.0001, 58.03,
     0.0003, 59.29, 0.0002, 60.59, 0.0003, 62.04, 0.0002, 65.89, 0.0002,
     67.23, 0.0002, 68.61, 0.0002, 69.97, 0.0004, 71.36, 0.0005, 85.42,
     0.0001],
   [1.98, 0.0256, 2.96, 0.0158, 3.95, 0.0310, 4.94, 0.0411, 5.95, 0.0238,
     6.94, 0.0152, 7.93, 0.0011, 8.95, 0.0185, 9.92, 0.0166, 10.93, 0.0306,
     11.94, 0.0258, 12.96, 0.0202, 13.97, 0.0403, 14.95, 0.0228, 15.93,
     0.0005, 17.01, 0.0072, 18.02, 0.0034, 19.06, 0.0028, 20.08, 0.0124,
     21.13, 0.0137, 22.16, 0.0102, 23.19, 0.0058, 23.90, 0.0013, 25.30,
     0.0039, 26.36, 0.0039, 27.41, 0.0025, 28.47, 0.0071, 29.64, 0.0031,
     30.60, 0.0027, 31.71, 0.0021, 32.84, 0.0003, 33.82, 0.0002, 35.07,
     0.0019, 36.09, 0.0054, 37.20, 0.0038, 38.33, 0.0024, 39.47, 0.0055,
     40.55, 0.0016, 41.77, 0.0006, 42.95, 0.0002, 43.27, 0.0018, 44.03,
     0.0006, 45.25, 0.0019, 46.36, 0.0033, 47.50, 0.0024, 48.87, 0.0012,
     50.03, 0.0016, 51.09, 0.0004, 53.52, 0.0017, 54.74, 0.0012, 56.17,
     0.0003, 57.40, 0.0011, 58.42, 0.0020, 59.70, 0.0007, 61.29, 0.0008,
     62.56, 0.0003, 63.48, 0.0002, 64.83, 0.0002, 66.12, 0.0012, 67.46,
     0.0017, 68.81, 0.0003, 69.13, 0.0003, 70.53, 0.0002, 71.84, 0.0001,
     73.28, 0.0002, 75.52, 0.0010, 76.96, 0.0005, 77.93, 0.0003, 78.32,
     0.0003, 79.73, 0.0003, 81.69, 0.0002, 82.52, 0.0001, 84.01, 0.0001,
     84.61, 0.0002, 86.88, 0.0001, 88.36, 0.0002, 89.85, 0.0002, 91.35,
     0.0003, 92.86, 0.0002, 93.40, 0.0001, 105.28, 0.0002, 106.22, 0.0002,
     107.45, 0.0001, 108.70, 0.0003, 122.08, 0.0002],
   [1.97, 0.0264, 2.97, 0.0211, 3.98, 0.0234, 4.98, 0.0307, 5.96, 0.0085,
     6.94, 0.0140, 7.93, 0.0005, 8.96, 0.0112, 9.96, 0.0209, 10.98, 0.0194,
     11.98, 0.0154, 12.99, 0.0274, 13.99, 0.0127, 15.01, 0.0101, 15.99,
     0.0002, 17.04, 0.0011, 18.08, 0.0032, 19.14, 0.0028, 20.12, 0.0054,
     21.20, 0.0053, 22.13, 0.0028, 23.22, 0.0030, 24.32, 0.0006, 25.24,
     0.0004, 26.43, 0.0028, 27.53, 0.0048, 28.52, 0.0039, 29.54, 0.0047,
     30.73, 0.0044, 31.82, 0.0007, 32.94, 0.0008, 34.04, 0.0012, 35.13,
     0.0018, 36.29, 0.0007, 37.35, 0.0075, 38.51, 0.0045, 39.66, 0.0014,
     40.90, 0.0004, 41.90, 0.0002, 43.08, 0.0002, 44.24, 0.0017, 45.36,
     0.0013, 46.68, 0.0020, 47.79, 0.0015, 48.98, 0.0010, 50.21, 0.0012,
     51.34, 0.0001, 53.82, 0.0003, 55.09, 0.0004, 56.23, 0.0005, 57.53,
     0.0004, 58.79, 0.0005, 59.30, 0.0002, 60.03, 0.0002, 61.40, 0.0003,
     62.84, 0.0001, 66.64, 0.0001, 67.97, 0.0001, 69.33, 0.0001, 70.68,
     0.0001, 73.57, 0.0002, 75.76, 0.0002, 76.45, 0.0001, 79.27, 0.0001,
     80.44, 0.0002, 81.87, 0.0002],
   [2.00, 0.0311, 2.99, 0.0086, 3.99, 0.0266, 4.97, 0.0123, 5.98, 0.0235,
     6.97, 0.0161, 7.97, 0.0008, 8.96, 0.0088, 9.96, 0.0621, 10.99, 0.0080,
     11.99, 0.0034, 12.99, 0.0300, 14.03, 0.0228, 15.04, 0.0105, 16.03,
     0.0004, 17.06, 0.0036, 18.09, 0.0094, 18.95, 0.0009, 20.17, 0.0071,
     21.21, 0.0161, 22.25, 0.0106, 23.28, 0.0104, 24.33, 0.0008, 25.38,
     0.0030, 26.46, 0.0035, 27.50, 0.0026, 28.59, 0.0028, 29.66, 0.0128,
     30.75, 0.0139, 31.81, 0.0038, 32.93, 0.0006, 34.04, 0.0004, 35.16,
     0.0005, 36.25, 0.0023, 37.35, 0.0012, 38.46, 0.0021, 39.59, 0.0035,
     40.71, 0.0006, 41.86, 0.0007, 42.42, 0.0001, 43.46, 0.0003, 44.17,
     0.0032, 45.29, 0.0013, 46.57, 0.0004, 47.72, 0.0011, 48.79, 0.0005,
     50.11, 0.0005, 51.29, 0.0003, 52.47, 0.0002, 53.68, 0.0004, 55.02,
     0.0005, 56.18, 0.0003, 57.41, 0.0003, 58.75, 0.0007, 59.33, 0.0009,
     60.00, 0.0004, 61.34, 0.0001, 64.97, 0.0003, 65.20, 0.0002, 66.48,
     0.0002, 67.83, 0.0002, 68.90, 0.0003, 70.25, 0.0003, 71.59, 0.0002,
     73.68, 0.0001, 75.92, 0.0001, 77.08, 0.0002, 78.45, 0.0002, 81.56,
     0.0002, 82.99, 0.0001, 88.39, 0.0001], 
   [0.97, 0.0059, 1.98, 0.0212, 2.99, 0.0153, 3.99, 0.0227, 4.96, 0.0215,
     5.97, 0.0153, 6.98, 0.0085, 7.98, 0.0007, 8.97, 0.0179, 9.98, 0.0512,
     10.98, 0.0322, 12.00, 0.0098, 13.02, 0.0186, 14.00, 0.0099, 15.05,
     0.0109, 15.88, 0.0011, 17.07, 0.0076, 18.11, 0.0071, 19.12, 0.0045,
     20.16, 0.0038, 21.23, 0.0213, 22.27, 0.0332, 23.34, 0.0082, 24.34,
     0.0014, 25.42, 0.0024, 26.47, 0.0012, 27.54, 0.0014, 28.60, 0.0024,
     29.72, 0.0026, 30.10, 0.0008, 31.91, 0.0021, 32.13, 0.0011, 33.02,
     0.0007, 34.09, 0.0014, 35.17, 0.0007, 36.27, 0.0024, 37.39, 0.0029,
     38.58, 0.0014, 39.65, 0.0017, 40.95, 0.0012, 41.97, 0.0004, 42.43,
     0.0002, 43.49, 0.0001, 44.31, 0.0012, 45.42, 0.0031, 46.62, 0.0017,
     47.82, 0.0013, 49.14, 0.0013, 50.18, 0.0010, 51.54, 0.0003, 53.90,
     0.0006, 55.06, 0.0010, 56.31, 0.0003, 57.63, 0.0001, 59.02, 0.0003,
     60.09, 0.0004, 60.35, 0.0004, 61.62, 0.0009, 63.97, 0.0001, 65.19,
     0.0001, 65.54, 0.0002, 66.92, 0.0002, 67.94, 0.0002, 69.17, 0.0003,
     69.60, 0.0004, 70.88, 0.0002, 72.24, 0.0002, 76.12, 0.0001, 78.94,
     0.0001, 81.75, 0.0001, 82.06, 0.0001, 83.53, 0.0001, 90.29, 0.0002,
     91.75, 0.0001, 92.09, 0.0002, 93.28, 0.0001, 97.07, 0.0001],
   [1.98, 0.0159, 2.98, 0.1008, 3.98, 0.0365, 4.98, 0.0133, 5.97, 0.0101,
     6.97, 0.0115, 7.97, 0.0007, 8.99, 0.0349, 10.01, 0.0342, 11.01,
     0.0236, 12.00, 0.0041, 13.02, 0.0114, 14.05, 0.0137, 15.06, 0.0100,
     16.05, 0.0007, 17.04, 0.0009, 18.12, 0.0077, 19.15, 0.0023, 20.12,
     0.0017, 21.24, 0.0113, 22.26, 0.0126, 23.30, 0.0093, 24.36, 0.0007,
     25.43, 0.0007, 26.47, 0.0009, 27.55, 0.0013, 28.59, 0.0025, 29.61,
     0.0010, 30.77, 0.0021, 31.86, 0.0023, 32.96, 0.0003, 34.03, 0.0007,
     35.06, 0.0005, 36.20, 0.0006, 37.34, 0.0006, 38.36, 0.0009, 39.60,
     0.0016, 40.69, 0.0005, 41.77, 0.0002, 42.92, 0.0002, 44.02, 0.0003,
     45.24, 0.0006, 46.33, 0.0004, 47.50, 0.0007, 48.71, 0.0007, 49.87,
     0.0002, 51.27, 0.0002, 53.42, 0.0003, 55.88, 0.0003, 57.10, 0.0004,
     58.34, 0.0002, 59.86, 0.0003, 61.13, 0.0003, 67.18, 0.0001, 68.50,
     0.0001, 71.17, 0.0001, 83.91, 0.0001, 90.55, 0.0001],
   [0.98, 0.0099, 2.00, 0.0181, 2.99, 0.0353, 3.98, 0.0285, 4.97, 0.0514,
     5.96, 0.0402, 6.96, 0.0015, 7.98, 0.0012, 8.98, 0.0175, 9.98, 0.0264,
     10.98, 0.0392, 11.98, 0.0236, 13.00, 0.0153, 14.04, 0.0049, 15.00,
     0.0089, 16.01, 0.0001, 17.03, 0.0106, 18.03, 0.0028, 19.05, 0.0024,
     20.08, 0.0040, 21.11, 0.0103, 22.12, 0.0104, 23.20, 0.0017, 24.19,
     0.0008, 25.20, 0.0007, 26.24, 0.0011, 27.36, 0.0009, 27.97, 0.0030,
     29.40, 0.0044, 30.37, 0.0019, 31.59, 0.0017, 32.65, 0.0008, 33.59,
     0.0005, 34.79, 0.0009, 35.75, 0.0027, 36.88, 0.0035, 37.93, 0.0039,
     39.00, 0.0031, 40.08, 0.0025, 41.16, 0.0010, 43.25, 0.0004, 44.52,
     0.0012, 45.62, 0.0023, 45.85, 0.0012, 47.00, 0.0006, 47.87, 0.0008,
     48.99, 0.0003, 50.48, 0.0003, 51.62, 0.0001, 52.43, 0.0001, 53.56,
     0.0002, 54.76, 0.0002, 56.04, 0.0002, 56.68, 0.0006, 57.10, 0.0003,
     58.28, 0.0005, 59.47, 0.0003, 59.96, 0.0002, 60.67, 0.0001, 63.08,
     0.0002, 64.29, 0.0002, 66.72, 0.0001, 67.97, 0.0001, 68.65, 0.0001,
     70.43, 0.0001, 79.38, 0.0001, 80.39, 0.0001, 82.39, 0.0001],
   [1.00, 0.0765, 1.99, 0.0151, 2.99, 0.0500, 3.99, 0.0197, 5.00, 0.0260,
     6.00, 0.0145, 6.98, 0.0128, 7.97, 0.0004, 8.98, 0.0158, 9.99, 0.0265,
     11.02, 0.0290, 12.02, 0.0053, 13.03, 0.0242, 14.03, 0.0103, 15.06,
     0.0054, 16.04, 0.0006, 17.08, 0.0008, 18.10, 0.0058, 19.16, 0.0011,
     20.16, 0.0055, 21.18, 0.0040, 22.20, 0.0019, 23.22, 0.0014, 24.05,
     0.0005, 25.31, 0.0019, 26.38, 0.0018, 27.44, 0.0022, 28.45, 0.0024,
     29.57, 0.0073, 30.58, 0.0032, 31.66, 0.0071, 32.73, 0.0015, 33.85,
     0.0005, 34.96, 0.0003, 36.00, 0.0020, 37.11, 0.0018, 38.18, 0.0055,
     39.23, 0.0006, 40.33, 0.0004, 41.52, 0.0003, 43.41, 0.0028, 45.05,
     0.0003, 45.99, 0.0002, 47.07, 0.0003, 48.52, 0.0002, 49.48, 0.0003,
     50.63, 0.0003, 51.81, 0.0002, 54.05, 0.0002, 55.24, 0.0001, 56.62,
     0.0001, 57.81, 0.0004, 59.16, 0.0013, 60.23, 0.0003, 66.44, 0.0001,
     68.99, 0.0004, 75.49, 0.0001, 87.56, 0.0004],
   [0.98, 0.0629, 1.99, 0.0232, 2.98, 0.0217, 4.00, 0.0396, 4.98, 0.0171,
     5.97, 0.0098, 6.99, 0.0167, 7.99, 0.0003, 8.98, 0.0192, 9.98, 0.0266,
     10.99, 0.0256, 12.01, 0.0061, 13.02, 0.0135, 14.02, 0.0062, 15.05,
     0.0158, 16.06, 0.0018, 17.08, 0.0101, 18.09, 0.0053, 19.11, 0.0074,
     20.13, 0.0020, 21.17, 0.0052, 22.22, 0.0077, 23.24, 0.0035, 24.00,
     0.0009, 25.32, 0.0016, 26.40, 0.0022, 27.43, 0.0005, 28.55, 0.0026,
     29.60, 0.0026, 30.65, 0.0010, 31.67, 0.0019, 32.77, 0.0008, 33.81,
     0.0003, 34.91, 0.0003, 36.01, 0.0005, 37.11, 0.0010, 38.20, 0.0014,
     39.29, 0.0039, 40.43, 0.0012, 41.50, 0.0006, 43.38, 0.0017, 43.75,
     0.0002, 44.94, 0.0005, 46.13, 0.0002, 47.11, 0.0003, 48.28, 0.0005,
     48.42, 0.0005, 49.44, 0.0003, 50.76, 0.0004, 51.93, 0.0002, 54.15,
     0.0003, 55.31, 0.0005, 55.50, 0.0003, 56.98, 0.0003, 57.90, 0.0004,
     60.33, 0.0002, 61.39, 0.0001, 61.59, 0.0001, 65.09, 0.0002, 66.34,
     0.0001, 68.85, 0.0001, 70.42, 0.0002, 71.72, 0.0001, 73.05, 0.0003,
     79.65, 0.0001, 85.28, 0.0002, 93.52, 0.0001],
   [1.02, 0.0185, 1.99, 0.0525, 2.98, 0.0613, 3.99, 0.0415, 4.98, 0.0109,
     5.97, 0.0248, 6.99, 0.0102, 7.98, 0.0005, 8.98, 0.0124, 9.99, 0.0103,
     10.99, 0.0124, 12.00, 0.0016, 13.01, 0.0029, 14.03, 0.0211, 15.04,
     0.0128, 16.07, 0.0021, 17.09, 0.0009, 18.09, 0.0043, 19.14, 0.0022,
     20.13, 0.0016, 21.20, 0.0045, 22.21, 0.0088, 23.26, 0.0046, 24.29,
     0.0013, 25.35, 0.0009, 26.39, 0.0028, 27.49, 0.0009, 28.51, 0.0006,
     29.58, 0.0012, 30.70, 0.0010, 31.74, 0.0019, 32.75, 0.0002, 33.85,
     0.0001, 34.95, 0.0005, 36.02, 0.0003, 37.16, 0.0009, 38.25, 0.0018,
     39.35, 0.0008, 40.54, 0.0004, 41.61, 0.0002, 43.40, 0.0004, 43.74,
     0.0003, 45.05, 0.0001, 46.11, 0.0003, 47.40, 0.0002, 48.36, 0.0004,
     49.55, 0.0004, 50.72, 0.0002, 52.00, 0.0001, 55.58, 0.0002, 57.02,
     0.0001, 57.98, 0.0002, 59.13, 0.0003, 61.56, 0.0001, 66.56, 0.0001,
     87.65, 0.0002],
   [1.00, 0.0473, 1.99, 0.0506, 2.99, 0.0982, 3.99, 0.0654, 5.00, 0.0196,
     5.99, 0.0094, 6.99, 0.0118, 7.93, 0.0001, 8.99, 0.0057, 10.01, 0.0285,
     11.01, 0.0142, 12.03, 0.0032, 13.03, 0.0056, 14.06, 0.0064, 15.06,
     0.0059, 16.11, 0.0005, 17.09, 0.0033, 18.14, 0.0027, 19.15, 0.0014,
     20.17, 0.0010, 21.21, 0.0059, 22.26, 0.0043, 23.31, 0.0031, 24.31,
     0.0018, 25.33, 0.0009, 26.41, 0.0005, 27.47, 0.0015, 28.53, 0.0015,
     29.58, 0.0041, 30.65, 0.0025, 31.73, 0.0011, 32.83, 0.0010, 34.98,
     0.0003, 36.07, 0.0009, 37.23, 0.0001, 38.26, 0.0020, 39.41, 0.0014,
     40.53, 0.0005, 41.40, 0.0003, 42.80, 0.0002, 43.48, 0.0028, 43.93,
     0.0001, 45.03, 0.0003, 46.18, 0.0007, 47.41, 0.0001, 48.57, 0.0002,
     49.67, 0.0001, 50.83, 0.0002, 54.39, 0.0001, 55.58, 0.0002, 57.97,
     0.0005, 58.11, 0.0002, 59.21, 0.0001, 60.42, 0.0002, 61.66, 0.0001], 
   [1.00, 0.0503, 2.00, 0.0963, 2.99, 0.1304, 3.99, 0.0218, 4.98, 0.0041,
     5.98, 0.0292, 6.98, 0.0482, 7.99, 0.0005, 8.99, 0.0280, 10.00, 0.0237,
     11.00, 0.0152, 12.02, 0.0036, 12.95, 0.0022, 14.06, 0.0111, 15.07,
     0.0196, 16.08, 0.0016, 17.11, 0.0044, 18.13, 0.0073, 19.17, 0.0055,
     20.19, 0.0028, 21.20, 0.0012, 22.27, 0.0068, 23.30, 0.0036, 24.35,
     0.0012, 25.35, 0.0002, 26.46, 0.0005, 27.47, 0.0005, 28.59, 0.0009,
     29.65, 0.0021, 30.70, 0.0020, 31.78, 0.0012, 32.89, 0.0010, 35.06,
     0.0005, 36.16, 0.0008, 37.27, 0.0010, 38.36, 0.0010, 39.47, 0.0014,
     40.58, 0.0004, 41.43, 0.0007, 41.82, 0.0003, 43.48, 0.0008, 44.53,
     0.0001, 45.25, 0.0003, 46.43, 0.0002, 47.46, 0.0002, 48.76, 0.0005,
     49.95, 0.0004, 50.96, 0.0002, 51.12, 0.0002, 52.33, 0.0001, 54.75,
     0.0001, 55.75, 0.0002, 56.90, 0.0002, 58.17, 0.0002, 59.40, 0.0004,
     60.62, 0.0002, 65.65, 0.0001, 66.91, 0.0002, 69.91, 0.0001, 71.25,
     0.0002],
   [1.00, 0.1243, 1.98, 0.1611, 3.00, 0.0698, 3.98, 0.0390, 5.00, 0.0138,
     5.99, 0.0154, 7.01, 0.0287, 8.01, 0.0014, 9.01, 0.0049, 10.00, 0.0144,
     11.01, 0.0055, 12.05, 0.0052, 13.01, 0.0011, 14.05, 0.0118, 15.07,
     0.0154, 16.12, 0.0028, 17.14, 0.0061, 18.25, 0.0007, 19.22, 0.0020,
     20.24, 0.0011, 21.27, 0.0029, 22.30, 0.0046, 23.34, 0.0049, 24.35,
     0.0004, 25.45, 0.0003, 26.47, 0.0007, 27.59, 0.0008, 28.16, 0.0009,
     29.12, 0.0002, 29.81, 0.0006, 30.81, 0.0009, 31.95, 0.0004, 33.00,
     0.0011, 34.12, 0.0005, 35.18, 0.0003, 36.30, 0.0008, 37.38, 0.0003,
     38.55, 0.0003, 39.64, 0.0006, 40.77, 0.0007, 41.52, 0.0006, 41.89,
     0.0006, 43.04, 0.0011, 43.60, 0.0009, 44.31, 0.0002, 45.68, 0.0002,
     46.56, 0.0003, 47.60, 0.0001, 48.83, 0.0006, 50.01, 0.0003, 51.27,
     0.0003, 56.04, 0.0005, 57.21, 0.0003, 58.56, 0.0004, 59.83, 0.0003,
     61.05, 0.0001, 62.20, 0.0001, 67.37, 0.0002, 76.53, 0.0001],
   [0.99, 0.0222, 1.99, 0.0678, 2.99, 0.0683, 4.00, 0.0191, 5.00, 0.0119,
     6.01, 0.0232, 6.98, 0.0336, 7.99, 0.0082, 9.01, 0.0201, 10.01, 0.0189,
     11.01, 0.0041, 12.01, 0.0053, 13.05, 0.0154, 14.04, 0.0159, 15.06,
     0.0092, 16.11, 0.0038, 17.12, 0.0014, 18.15, 0.0091, 19.16, 0.0006,
     20.30, 0.0012, 21.25, 0.0061, 22.28, 0.0099, 23.34, 0.0028, 24.38,
     0.0012, 25.43, 0.0016, 26.49, 0.0048, 27.55, 0.0025, 28.62, 0.0015,
     29.71, 0.0032, 30.78, 0.0077, 31.88, 0.0011, 32.97, 0.0007, 34.08,
     0.0006, 35.16, 0.0008, 36.28, 0.0004, 37.41, 0.0006, 38.54, 0.0005,
     39.62, 0.0002, 40.80, 0.0003, 41.93, 0.0001, 43.06, 0.0002, 44.21,
     0.0003, 45.38, 0.0002, 46.54, 0.0007, 47.78, 0.0003, 48.95, 0.0004,
     50.10, 0.0003, 51.37, 0.0002, 53.79, 0.0003, 56.20, 0.0001, 58.71,
     0.0002, 66.47, 0.0003],
   [1.01, 0.0241, 1.99, 0.1011, 2.98, 0.0938, 3.98, 0.0081, 4.99, 0.0062,
     5.99, 0.0291, 6.99, 0.0676, 7.59, 0.0004, 8.98, 0.0127, 9.99, 0.0112,
     10.99, 0.0142, 12.00, 0.0029, 13.02, 0.0071, 14.02, 0.0184, 15.03,
     0.0064, 16.07, 0.0010, 17.09, 0.0011, 18.11, 0.0010, 19.15, 0.0060,
     20.19, 0.0019, 21.24, 0.0025, 22.29, 0.0013, 23.31, 0.0050, 25.41,
     0.0030, 26.50, 0.0018, 27.53, 0.0006, 28.63, 0.0012, 29.66, 0.0013,
     30.77, 0.0020, 31.84, 0.0006, 34.04, 0.0001, 35.14, 0.0001, 36.32,
     0.0004, 37.41, 0.0007, 38.53, 0.0007, 39.67, 0.0009, 40.85, 0.0003,
     45.49, 0.0002, 46.65, 0.0001, 47.81, 0.0004, 49.01, 0.0002, 53.91,
     0.0002, 55.14, 0.0002, 57.69, 0.0002],
   [1.00, 0.0326, 2.00, 0.1066, 2.99, 0.1015, 4.00, 0.0210, 4.97, 0.0170,
     5.99, 0.0813, 6.98, 0.0820, 7.96, 0.0011, 8.99, 0.0248, 10.03, 0.0107,
     11.01, 0.0126, 12.01, 0.0027, 13.01, 0.0233, 14.04, 0.0151, 15.05,
     0.0071, 16.04, 0.0002, 17.10, 0.0061, 18.12, 0.0059, 19.15, 0.0087,
     20.23, 0.0005, 21.25, 0.0040, 22.30, 0.0032, 23.35, 0.0004, 24.40,
     0.0001, 25.45, 0.0030, 26.54, 0.0022, 27.60, 0.0003, 28.70, 0.0009,
     29.80, 0.0029, 30.85, 0.0006, 31.97, 0.0006, 34.19, 0.0004, 35.30,
     0.0003, 36.43, 0.0007, 37.56, 0.0005, 38.68, 0.0019, 39.88, 0.0013,
     41.00, 0.0003, 43.35, 0.0003, 44.51, 0.0002, 45.68, 0.0006, 46.93,
     0.0010, 48.11, 0.0006, 49.29, 0.0003, 55.58, 0.0002],
   [0.98, 0.0113, 1.99, 0.0967, 3.00, 0.0719, 3.98, 0.0345, 4.98, 0.0121,
     6.00, 0.0621, 7.00, 0.0137, 7.98, 0.0006, 9.01, 0.0314, 10.01, 0.0171,
     11.02, 0.0060, 12.03, 0.0024, 13.05, 0.0077, 14.07, 0.0040, 15.12,
     0.0032, 16.13, 0.0004, 17.15, 0.0011, 18.20, 0.0028, 19.18, 0.0003,
     20.26, 0.0003, 21.31, 0.0025, 22.35, 0.0021, 23.39, 0.0005, 25.55,
     0.0002, 26.62, 0.0014, 27.70, 0.0003, 28.78, 0.0005, 29.90, 0.0030,
     31.01, 0.0011, 32.12, 0.0005, 34.31, 0.0001, 35.50, 0.0002, 36.62,
     0.0002, 37.76, 0.0005, 38.85, 0.0002, 40.09, 0.0004, 43.60, 0.0001,
     44.73, 0.0002, 46.02, 0.0002, 47.25, 0.0004, 48.44, 0.0004],
   [0.99, 0.0156, 1.98, 0.0846, 2.98, 0.0178, 3.98, 0.0367, 4.98, 0.0448,
     5.98, 0.0113, 6.99, 0.0189, 8.00, 0.0011, 9.01, 0.0247, 10.02, 0.0089,
     11.01, 0.0184, 12.03, 0.0105, 13.00, 0.0039, 14.07, 0.0116, 15.09,
     0.0078, 16.13, 0.0008, 17.14, 0.0064, 18.19, 0.0029, 19.22, 0.0028,
     20.25, 0.0017, 21.32, 0.0043, 22.37, 0.0055, 23.42, 0.0034, 24.48,
     0.0004, 25.54, 0.0002, 26.61, 0.0017, 27.70, 0.0011, 28.80, 0.0002,
     29.89, 0.0019, 30.97, 0.0028, 32.09, 0.0007, 34.30, 0.0002, 35.44,
     0.0003, 36.55, 0.0001, 37.69, 0.0004, 38.93, 0.0002, 40.05, 0.0005,
     41.20, 0.0005, 42.37, 0.0002, 43.54, 0.0003, 44.73, 0.0001, 45.95,
     0.0002, 47.16, 0.0001, 48.43, 0.0005, 49.65, 0.0004, 55.90, 0.0002,
     59.81, 0.0004], 
   [1.01, 0.0280, 2.00, 0.0708, 2.99, 0.0182, 3.99, 0.0248, 4.98, 0.0245,
     5.98, 0.0279, 6.98, 0.0437, 7.99, 0.0065, 8.99, 0.0299, 10.00, 0.0073,
     10.99, 0.0011, 12.03, 0.0122, 13.03, 0.0028, 14.08, 0.0044, 15.11,
     0.0097, 16.15, 0.0010, 17.17, 0.0025, 18.19, 0.0017, 19.24, 0.0008,
     20.28, 0.0040, 21.32, 0.0024, 22.38, 0.0008, 23.46, 0.0032, 24.52,
     0.0010, 25.59, 0.0008, 26.68, 0.0009, 27.76, 0.0012, 28.88, 0.0003,
     29.95, 0.0005, 31.05, 0.0017, 32.14, 0.0002, 33.29, 0.0003, 37.88,
     0.0002, 39.03, 0.0002, 40.19, 0.0004, 41.37, 0.0003, 43.74, 0.0002,
     46.20, 0.0001, 48.68, 0.0001, 49.93, 0.0001, 51.19, 0.0002],
   [1.00, 0.0225, 1.99, 0.0921, 2.98, 0.0933, 3.99, 0.0365, 4.99, 0.0100,
     5.98, 0.0213, 6.98, 0.0049, 7.98, 0.0041, 8.98, 0.0090, 9.99, 0.0068,
     11.01, 0.0040, 12.03, 0.0086, 13.02, 0.0015, 14.04, 0.0071, 15.09,
     0.0082, 16.14, 0.0011, 17.15, 0.0014, 18.18, 0.0010, 19.26, 0.0013,
     20.26, 0.0005, 21.33, 0.0006, 22.36, 0.0011, 23.46, 0.0016, 24.52,
     0.0004, 25.59, 0.0002, 26.70, 0.0006, 27.78, 0.0007, 28.87, 0.0002,
     30.03, 0.0008, 31.14, 0.0010, 32.24, 0.0006, 33.37, 0.0002, 35.67,
     0.0003, 37.99, 0.0004, 39.17, 0.0004, 40.35, 0.0005, 41.53, 0.0001,
     46.42, 0.0001],
   [1.00, 0.0465, 1.99, 0.0976, 2.98, 0.0678, 4.00, 0.0727, 4.99, 0.0305,
     5.98, 0.0210, 6.98, 0.0227, 8.00, 0.0085, 9.01, 0.0183, 10.02, 0.0258,
     11.05, 0.0003, 12.06, 0.0061, 13.05, 0.0021, 14.10, 0.0089, 15.12,
     0.0077, 16.16, 0.0016, 17.21, 0.0061, 18.23, 0.0011, 19.29, 0.0031,
     20.36, 0.0031, 21.41, 0.0007, 22.48, 0.0013, 23.55, 0.0020, 24.64,
     0.0004, 25.74, 0.0005, 26.81, 0.0006, 27.95, 0.0006, 29.03, 0.0001,
     30.22, 0.0010, 31.30, 0.0004, 32.48, 0.0001, 33.60, 0.0002, 38.30,
     0.0003],
   [1.00, 0.0674, 1.99, 0.0841, 2.98, 0.0920, 3.99, 0.0328, 4.99, 0.0368,
     5.98, 0.0206, 6.99, 0.0246, 8.01, 0.0048, 9.01, 0.0218, 10.03, 0.0155,
     11.05, 0.0048, 12.06, 0.0077, 13.00, 0.0020, 14.10, 0.0083, 15.15,
     0.0084, 16.18, 0.0015, 17.22, 0.0039, 18.27, 0.0032, 19.34, 0.0026,
     20.40, 0.0012, 21.47, 0.0009, 22.54, 0.0008, 23.62, 0.0016, 24.71,
     0.0005, 25.82, 0.0004, 26.91, 0.0002, 28.03, 0.0008, 29.17, 0.0002,
     30.32, 0.0028, 31.45, 0.0004, 32.61, 0.0005, 33.77, 0.0001, 36.14,
     0.0003, 37.32, 0.0002, 38.54, 0.0005, 39.75, 0.0002, 42.23, 0.0002,
     48.65, 0.0001], 
   [1.01, 0.0423, 1.99, 0.0240, 2.98, 0.0517, 4.00, 0.0493, 5.00, 0.0324,
     6.00, 0.0094, 6.99, 0.0449, 7.99, 0.0050, 9.00, 0.0197, 10.03, 0.0132,
     11.03, 0.0009, 12.07, 0.0017, 13.08, 0.0023, 14.12, 0.0094, 15.16,
     0.0071, 16.21, 0.0020, 17.25, 0.0005, 18.30, 0.0027, 19.04, 0.0004,
     20.43, 0.0022, 21.51, 0.0002, 22.59, 0.0006, 23.72, 0.0018, 24.80,
     0.0002, 25.88, 0.0002, 27.03, 0.0002, 28.09, 0.0006, 29.31, 0.0002,
     30.46, 0.0004, 31.61, 0.0007, 32.78, 0.0005, 33.95, 0.0001, 36.34,
     0.0002, 37.56, 0.0001, 38.80, 0.0001, 40.02, 0.0001, 44.14, 0.0001],
   [1.00, 0.0669, 1.99, 0.0909, 2.99, 0.0410, 3.98, 0.0292, 4.98, 0.0259,
     5.98, 0.0148, 6.98, 0.0319, 7.99, 0.0076, 9.01, 0.0056, 10.02, 0.0206,
     11.04, 0.0032, 12.05, 0.0085, 13.08, 0.0040, 14.12, 0.0037, 15.16,
     0.0030, 16.20, 0.0013, 17.24, 0.0021, 18.30, 0.0010, 19.36, 0.0015,
     20.44, 0.0013, 21.50, 0.0009, 22.60, 0.0015, 23.69, 0.0014, 24.80,
     0.0006, 25.87, 0.0002, 27.02, 0.0006, 28.12, 0.0002, 29.28, 0.0003,
     30.43, 0.0002, 31.59, 0.0007, 32.79, 0.0001, 35.14, 0.0001, 37.57,
     0.0001, 40.03, 0.0002, 41.28, 0.0004, 44.10, 0.0001],
   [0.99, 0.0421, 1.99, 0.1541, 2.98, 0.0596, 3.98, 0.0309, 4.98, 0.0301,
     5.99, 0.0103, 7.00, 0.0240, 8.01, 0.0073, 9.01, 0.0222, 10.04, 0.0140,
     11.05, 0.0033, 12.08, 0.0045, 13.13, 0.0009, 14.13, 0.0015, 15.21,
     0.0026, 16.24, 0.0003, 17.30, 0.0004, 18.35, 0.0010, 19.39, 0.0003,
     20.50, 0.0015, 21.57, 0.0003, 22.68, 0.0011, 23.80, 0.0005, 24.90,
     0.0008, 26.02, 0.0002, 27.16, 0.0001, 28.30, 0.0006, 29.48, 0.0002,
     31.81, 0.0005, 33.00, 0.0003, 34.21, 0.0001, 37.89, 0.0001],
   [0.99, 0.0389, 2.00, 0.2095, 3.00, 0.0835, 3.99, 0.0289, 5.00, 0.0578,
     5.99, 0.0363, 7.01, 0.0387, 8.01, 0.0056, 9.04, 0.0173, 10.05, 0.0175,
     11.08, 0.0053, 12.10, 0.0056, 13.15, 0.0064, 14.19, 0.0036, 15.22,
     0.0019, 16.29, 0.0010, 17.36, 0.0017, 18.43, 0.0018, 19.51, 0.0004,
     20.60, 0.0011, 21.70, 0.0003, 22.82, 0.0003, 23.95, 0.0001, 25.05,
     0.0004, 26.17, 0.0001, 28.50, 0.0003, 29.68, 0.0001, 32.07, 0.0003,
     33.28, 0.0004, 34.52, 0.0001], 
   [1.00, 0.1238, 1.99, 0.2270, 3.00, 0.0102, 3.99, 0.0181, 4.98, 0.0415,
     6.00, 0.0165, 7.01, 0.0314, 8.02, 0.0148, 9.04, 0.0203, 10.05, 0.0088,
     11.07, 0.0062, 12.11, 0.0070, 13.14, 0.0054, 14.19, 0.0028, 15.24,
     0.0044, 16.30, 0.0029, 17.38, 0.0009, 18.45, 0.0026, 19.56, 0.0003,
     20.65, 0.0025, 21.74, 0.0014, 22.87, 0.0013, 23.99, 0.0007, 25.15,
     0.0002, 27.46, 0.0004, 28.39, 0.0006, 28.65, 0.0004, 29.85, 0.0001,
     31.05, 0.0002, 32.27, 0.0003, 33.52, 0.0002, 34.76, 0.0003],
   [1.00, 0.1054, 2.00, 0.2598, 2.99, 0.0369, 3.98, 0.0523, 4.99, 0.0020,
     5.99, 0.0051, 7.00, 0.0268, 8.01, 0.0027, 9.04, 0.0029, 10.05, 0.0081,
     11.08, 0.0047, 12.12, 0.0051, 13.16, 0.0091, 14.19, 0.0015, 15.27,
     0.0030, 16.34, 0.0017, 17.42, 0.0006, 18.51, 0.0003, 19.61, 0.0007,
     20.72, 0.0003, 21.84, 0.0001, 22.99, 0.0010, 24.13, 0.0001, 28.44,
     0.0001, 30.09, 0.0001],
   [0.99, 0.0919, 2.00, 0.0418, 2.99, 0.0498, 3.99, 0.0135, 4.99, 0.0026,
     6.00, 0.0155, 7.01, 0.0340, 8.02, 0.0033, 9.04, 0.0218, 10.08, 0.0084,
     11.11, 0.0057, 12.15, 0.0051, 13.21, 0.0043, 14.25, 0.0015, 15.31,
     0.0023, 16.40, 0.0008, 17.48, 0.0004, 18.59, 0.0016, 19.71, 0.0010,
     20.84, 0.0018, 21.98, 0.0002, 23.11, 0.0013, 24.26, 0.0003, 26.67,
     0.0002, 29.12, 0.0002, 30.37, 0.0002, 31.62, 0.0003, 32.92, 0.0001],
   [0.99, 0.1174, 1.99, 0.1126, 2.99, 0.0370, 3.99, 0.0159, 5.01, 0.0472,
     6.01, 0.0091, 7.03, 0.0211, 8.05, 0.0015, 9.07, 0.0098, 10.11, 0.0038,
     11.15, 0.0042, 12.20, 0.0018, 13.24, 0.0041, 14.32, 0.0033, 15.41,
     0.0052, 16.49, 0.0001, 17.61, 0.0004, 18.71, 0.0004, 19.84, 0.0004,
     20.99, 0.0002, 22.14, 0.0006, 23.31, 0.0006, 24.50, 0.0004, 25.70,
     0.0002, 28.09, 0.0002, 28.66, 0.0002, 32.00, 0.0001],
   [1.00, 0.1085, 2.00, 0.1400, 2.99, 0.0173, 3.99, 0.0229, 5.00, 0.0272,
     6.02, 0.0077, 7.03, 0.0069, 8.04, 0.0017, 9.08, 0.0045, 10.10, 0.0030,
     11.15, 0.0040, 12.20, 0.0007, 13.25, 0.0019, 14.32, 0.0008, 15.42,
     0.0024, 16.50, 0.0002, 17.59, 0.0005, 18.71, 0.0003, 19.83, 0.0002,
     20.98, 0.0005, 23.29, 0.0008],
   [1.00, 0.0985, 2.00, 0.1440, 2.99, 0.0364, 3.99, 0.0425, 5.00, 0.0190,
     6.01, 0.0089, 7.03, 0.0278, 8.04, 0.0006, 9.07, 0.0083, 10.10, 0.0021,
     11.14, 0.0050, 12.18, 0.0005, 13.26, 0.0036, 14.33, 0.0005, 15.41,
     0.0026, 17.62, 0.0004, 18.75, 0.0004, 19.89, 0.0003, 21.04, 0.0012,
     22.21, 0.0002, 23.38, 0.0004, 27.04, 0.0001],
   [0.99, 0.1273, 2.00, 0.1311, 2.99, 0.0120, 4.00, 0.0099, 5.00, 0.0235,
     6.02, 0.0068, 7.03, 0.0162, 8.06, 0.0009, 9.08, 0.0083, 10.12, 0.0014,
     11.17, 0.0050, 12.24, 0.0010, 13.29, 0.0013, 14.39, 0.0022, 15.48,
     0.0011, 16.59, 0.0002, 17.70, 0.0003, 18.84, 0.0010, 20.00, 0.0003,
     21.17, 0.0003, 23.56, 0.0004, 28.79, 0.0003], 
   [1.00, 0.1018, 2.00, 0.1486, 3.00, 0.0165, 4.00, 0.0186, 5.01, 0.0194,
     6.02, 0.0045, 7.04, 0.0083, 8.06, 0.0012, 9.10, 0.0066, 10.15, 0.0009,
     11.19, 0.0008, 12.26, 0.0011, 13.34, 0.0028, 14.45, 0.0006, 15.53,
     0.0009, 16.66, 0.0002, 17.79, 0.0006, 18.94, 0.0005, 20.11, 0.0003,
     21.29, 0.0005, 22.49, 0.0003, 23.73, 0.0005, 26.22, 0.0001, 27.52,
     0.0001, 28.88, 0.0002],
   [1.00, 0.1889, 1.99, 0.1822, 3.00, 0.0363, 4.00, 0.0047, 5.01, 0.0202,
     6.03, 0.0053, 7.05, 0.0114, 8.01, 0.0002, 9.13, 0.0048, 10.17, 0.0010,
     11.23, 0.0033, 12.30, 0.0010, 13.38, 0.0006, 14.50, 0.0002, 15.62,
     0.0010, 20.27, 0.0001, 21.47, 0.0001],
   [1.00, 0.0522, 1.99, 0.0763, 2.99, 0.0404, 4.00, 0.0139, 5.01, 0.0185,
     6.01, 0.0021, 7.06, 0.0045, 8.09, 0.0002, 9.11, 0.0003, 10.17, 0.0006,
     11.25, 0.0004, 12.32, 0.0005, 13.40, 0.0003, 14.53, 0.0003, 15.65,
     0.0007, 16.80, 0.0001, 17.95, 0.0002, 19.14, 0.0006, 20.34, 0.0002,
     21.56, 0.0003],
   [0.99, 0.1821, 1.99, 0.0773, 3.00, 0.0125, 4.01, 0.0065, 5.01, 0.0202,
     6.03, 0.0071, 7.05, 0.0090, 8.08, 0.0006, 9.13, 0.0008, 10.18, 0.0013,
     11.25, 0.0010, 12.33, 0.0012, 13.42, 0.0006, 14.54, 0.0005, 15.65,
     0.0004, 17.97, 0.0002, 19.15, 0.0001],
   [1.00, 0.1868, 2.00, 0.0951, 3.00, 0.0147, 4.01, 0.0134, 5.02, 0.0184,
     6.04, 0.0132, 7.06, 0.0011, 8.11, 0.0008, 9.15, 0.0010, 10.22, 0.0012,
     11.30, 0.0011, 12.40, 0.0003, 13.11, 0.0004, 13.49, 0.0002, 14.62,
     0.0003, 15.77, 0.0001],
   [1.00, 0.1933, 2.00, 0.0714, 3.00, 0.0373, 4.00, 0.0108, 5.02, 0.0094,
     6.02, 0.0010, 7.07, 0.0022, 8.11, 0.0002, 9.16, 0.0065, 10.23, 0.0015,
     11.31, 0.0023, 12.40, 0.0003, 13.53, 0.0014, 14.66, 0.0002, 15.81,
     0.0011, 18.20, 0.0002, 19.41, 0.0001],
   [0.99, 0.2113, 1.99, 0.0877, 3.00, 0.0492, 4.01, 0.0094, 5.02, 0.0144,
     6.04, 0.0103, 7.07, 0.0117, 8.12, 0.0006, 9.19, 0.0019, 10.25, 0.0007,
     11.35, 0.0017, 12.45, 0.0010, 13.58, 0.0003, 14.74, 0.0003, 15.91,
     0.0003, 19.57, 0.0002],
   [0.99, 0.2455, 1.99, 0.0161, 3.00, 0.0215, 4.01, 0.0036, 5.03, 0.0049,
     6.04, 0.0012, 7.09, 0.0036, 8.14, 0.0011, 9.21, 0.0009, 10.30, 0.0001,
     11.40, 0.0012, 12.50, 0.0001, 13.66, 0.0005, 14.84, 0.0001], 
   [1.00, 0.1132, 2.00, 0.0252, 3.00, 0.0292, 4.01, 0.0136, 5.03, 0.0045,
     6.06, 0.0022, 7.11, 0.0101, 8.17, 0.0004, 9.23, 0.0010, 10.33, 0.0012,
     11.44, 0.0013, 12.58, 0.0011, 13.75, 0.0002, 14.93, 0.0005, 16.14, 0.0002],
   [1.00, 0.1655, 2.00, 0.0445, 3.00, 0.0120, 4.00, 0.0038, 5.02, 0.0015,
     6.07, 0.0038, 7.11, 0.0003, 8.19, 0.0002, 9.25, 0.0010, 10.36, 0.0011,
     11.48, 0.0005, 12.63, 0.0002, 13.79, 0.0003, 16.24, 0.0002],
   [0.99, 0.3637, 1.99, 0.0259, 3.01, 0.0038, 4.01, 0.0057, 5.03, 0.0040,
     6.07, 0.0067, 7.12, 0.0014, 8.19, 0.0004, 9.27, 0.0003, 10.38, 0.0002,
     12.67, 0.0001],
   [1.00, 0.1193, 2.00, 0.0230, 3.00, 0.0104, 4.01, 0.0084, 5.04, 0.0047,
     6.08, 0.0035, 7.13, 0.0041, 8.20, 0.0002, 9.29, 0.0005, 10.40, 0.0005,
     11.53, 0.0003, 12.70, 0.0002, 13.91, 0.0002],
   [1.00, 0.0752, 2.00, 0.0497, 3.00, 0.0074, 4.02, 0.0076, 5.05, 0.0053,
     6.09, 0.0043, 7.15, 0.0024, 8.22, 0.0001, 9.32, 0.0006, 10.45, 0.0002,
     11.58, 0.0001, 12.78, 0.0001, 15.22, 0.0001],
   [1.00, 0.2388, 2.00, 0.0629, 3.01, 0.0159, 4.04, 0.0063, 5.07, 0.0051,
     6.12, 0.0045, 7.19, 0.0026, 8.29, 0.0015, 9.43, 0.0001, 11.75, 0.0002],
   [1.00, 0.1919, 2.01, 0.0116, 3.01, 0.0031, 4.03, 0.0090, 5.07, 0.0061,
     6.13, 0.0036, 7.19, 0.0013, 8.30, 0.0016, 9.13, 0.0001, 10.59, 0.0002,
     11.78, 0.0002],
   [1.00, 0.1296, 2.00, 0.0135, 3.01, 0.0041, 4.04, 0.0045, 5.09, 0.0028,
     6.14, 0.0046, 7.23, 0.0007, 8.32, 0.0007, 9.50, 0.0001],
   [1.00, 0.0692, 2.00, 0.0209, 3.02, 0.0025, 4.05, 0.0030, 5.09, 0.0047,
     6.17, 0.0022, 7.25, 0.0015, 8.36, 0.0015, 9.53, 0.0010, 10.69, 0.0001,
     13.40, 0.0001],
   [1.00, 0.1715, 2.00, 0.0142, 3.01, 0.0024, 4.03, 0.0015, 5.07, 0.0017,
     6.13, 0.0018, 7.22, 0.0009, 8.33, 0.0014, 9.51, 0.0007, 10.69, 0.0002], 
   [1.00, 0.1555, 2.01, 0.0148, 3.02, 0.0007, 4.06, 0.0006, 5.10, 0.0005,
     6.16, 0.0008, 7.26, 0.0009, 8.39, 0.0008, 9.58, 0.0002], 
   [1.00, 0.1357, 2.00, 0.0116, 3.02, 0.0026, 4.04, 0.0009, 5.09, 0.0004,
     6.17, 0.0005, 7.27, 0.0002, 8.40, 0.0001], 
   [1.00, 0.2185, 2.01, 0.0087, 3.03, 0.0018, 4.06, 0.0025, 5.11, 0.0020,
     6.20, 0.0012, 7.32, 0.0005, 8.46, 0.0001, 9.66, 0.0003], 
   [1.00, 0.2735, 2.00, 0.0038, 3.02, 0.0008, 4.06, 0.0012, 5.12, 0.0008,
     6.22, 0.0011, 7.35, 0.0003, 8.50, 0.0002], 
   [1.00, 0.1441, 1.99, 0.0062, 3.01, 0.0023, 4.05, 0.0011, 5.11, 0.0012,
     6.20, 0.0003, 7.33, 0.0004, 8.50, 0.0001], 
   [1.00, 0.0726, 2.01, 0.0293, 3.03, 0.0022, 5.14, 0.0005, 6.26, 0.0011,
     7.41, 0.0002, 8.63, 0.0002], 
   [1.00, 0.0516, 2.00, 0.0104, 3.02, 0.0029, 5.15, 0.0002, 6.27, 0.0001],
   [1.00, 0.0329, 2.00, 0.0033, 3.03, 0.0013, 4.10, 0.0005, 5.19,
     0.0004, 6.32, 0.0002],
   [1.00, 0.0179, 1.99, 0.0012, 3.04, 0.0005, 4.10, 0.0017, 5.20, 0.0005,
     6.35, 0.0001], 
   [1.00, 0.0334, 2.01, 0.0033, 3.04, 0.0011, 4.13, 0.0003, 5.22, 0.0003],
   [0.99, 0.0161, 2.01, 0.0100, 3.04, 0.0020, 4.13, 0.0003],
   [1.00, 0.0475, 1.99, 0.0045, 3.03, 0.0035, 4.12, 0.0011],
   [1.00, 0.0593, 2.00, 0.0014, 4.17, 0.0002],
   [1.00, 0.0249, 2.01, 0.0016],
   [1.00, 0.0242, 2.00, 0.0038, 4.19, 0.0002],
   [1.00, 0.0170, 2.02, 0.0030],
   [1.00, 0.0381, 2.00, 0.0017, 3.09, 0.0002],
   [1.00, 0.0141, 2.03, 0.0005, 3.11, 0.0003, 4.26, 0.0001],
   [1.00, 0.0122, 2.03, 0.0024],
   [1.00, 0.0107, 2.07, 0.0007, 3.12, 0.0004],
   [1.00, 0.0250, 2.02, 0.0026, 3.15, 0.0002],
   [1.01, 0.0092],
   [1.01, 0.0102, 2.09, 0.0005],
   [1.00, 0.0080, 2.00, 0.0005, 3.19, 0.0001],
   [1.01, 0.0298, 2.01, 0.0005]]

def lbj_piano(start, dur, freq, amp, *args)
  pfreq, degree, distance, reverb_amount = nil
  optkey(args, binding,
         [:pfreq, freq],
         [:degree, 45.0],
         [:distance, 1.0],
         [:reverb_amount, 0.0])
  get_piano_partials = lambda do |frq|
    Piano_Spectra[(12 * (log(frq / 32.703) / log(2))).round]
  end
  make_piano_ampfun = lambda do |dr|
    release_amp = db2linear($clm_db_drop_per_second * dr)
    attack_time = $clm_piano_attack_duration * 100.0 / dr
    [0, 0, attack_time / 4, 1.0, attack_time, 1.0, 100, release_amp]
  end
  # This thing sounds pretty good down low, below middle c or so.
  # Unfortunately, there are some tens of partials down there and
  # we're using exponential envelopes.  You're going to wait for a
  # long long time just to hear a single low note.  The high notes
  # sound pretty rotten--they just don't sparkle; I have a feeling
  # that this is due to the low amplitude of the original data, and
  # the lack of mechanical noise.
  #
  # The only thing you can do to alter the sound of a piano note is to
  # set the pfreq parameter.  Pfreq is used to look up the partials.
  # By default, it's set to the requested frequency.  Setting it to a
  # neighboring freq is useful when you're repeating notes.  Note that
  # there's no nyquist detection; a high freq with a low pfreq, will
  # give you fold over (hmmm...maybe I can get those high notes to
  # sparkle after all).
  partials = normalize_partials(get_piano_partials.call(pfreq))
  newdur = dur + $clm_piano_attack_duration + $clm_piano_release_duration
  env1dur = newdur - $clm_piano_release_duration
  env1samples = (env1dur * @srate).floor
  siz = (partials.length / 2).floor
  oscils = Array.new(siz)
  alist = make_vct(siz)
  ampfun1 = make_piano_ampfun.call(env1dur)
  ampenv1 = make_env(:envelope, ampfun1, :scaler, amp,
                     :duration, env1dur, :base, 10000.0)
  releaseamp = ampfun1.last
  ampenv2 = make_env(:envelope, [0, 1, 100, 0], :scaler, amp * releaseamp,
                     :duration, env1dur, :base, 1.0)
  sktr = j = 0
  0.step(partials.length - 1, 2) do |i|
    alist[j] = partials[i + 1]
    oscils[j] = make_oscil(:frequency, partials[i] * freq)
    j += 1
  end
  run_instrument(start, newdur,
                 :degree, degree,
                 :distance, distance,
                 :reverb_amount, reverb_amount) do
    sktr += 1
    let(0.0) do |sum|
      oscils.each_with_index do |osc, i|
        sum = sum + oscil(osc) * alist[i]
      end
      sum * env(((sktr > env1samples) ? ampenv2 : ampenv1))
    end
  end
end

def lbj_piano_test(start = 0.0, dur = 1.0)
  lbj_piano(start, dur, 440.0, 0.5)
  $now = start + dur + 0.24 + 0.2
end

# RESFLT
def resflt(start, dur, driver,
           ranfreq, noiamp, noifun, cosamp, cosfreq1, cosfreq0, cosnum,
           ampcosfun, freqcosfun,
           frq1, r1, g1, frq2, r2, g2, frq3, r3, g3, *args)
  degree, distance, reverb_amount = nil
  optkey(args, binding,
         [:degree, 0.0],
         [:distance, 1.0],
         [:reverb_amount, 0.005])
  # driver=0 -- use sum of cosines to drive the filter,
  # driver=1 -- use white noise
  # if noise used, ranfreq=frequency of random number generator,
  #                noiamp=amplitude thereof,
  #                noifun=amplitude envelope on white noise
  # if sum-of-cosines (i.e. a band-limited pulse train),
  #                cosamp=amplitude of pulse train,
  #                cosfreq1=top frequency (given freqcosfun)
  #                  (i.e. pulse frequency)
  #                cosfreq0=bottom frequency,
  #                cosnum=number of cosines in the pulse,
  #                ampcosfun=amplitude envelope on pulse train
  #                freqcosfun=frequency envelope on pulse train
  # There are then 3 resonators, centered at frq1, frq2, frq3,
  # with pole-radius r1, r2, and r3 respectively, and
  # with gains of g1, g2, and g3.
  f1 = make_two_pole(frq1, r1)
  f2 = make_two_pole(frq2, r2)
  f3 = make_two_pole(frq3, r3)
  with_noise = (driver == 1)
  frqf = if with_noise
           nil
         else
           make_env(:envelope, freqcosfun, :duration, dur,
                    :scaler, hz2radians(cosfreq1 - cosfreq0))
         end
  ampf = if with_noise
           make_env(:envelope, noifun, :scaler, noiamp, :duration, dur)
         else
           make_env(:envelope, ampcosfun, :scaler, cosamp, :duration, dur)
         end
  rn = if with_noise
         make_rand(:frequency, ranfreq)
       else
         nil
       end
  cn = if with_noise
         nil
       else
         make_sum_of_cosines(:frequency, cosfreq0, :cosines, cosnum)
       end
  if with_noise
    run_instrument(start, dur,
                   :distance, distance,
                   :degree, degree,
                   :reverb_amount, reverb_amount) do
      input1 = env(ampf) * rand(rn)
      two_pole(f1, input1 * g1) +
        two_pole(f2, input1 * g2) +
        two_pole(f3, input1 * g3)
    end
  else
    run_instrument(start, dur,
                   :distance, distance,
                   :degree, degree,
                   :reverb_amount, reverb_amount) do
      input1 = env(ampf) * sum_of_cosines(cn, env(frqf))
      two_pole(f1, input1 * g1) +
        two_pole(f2, input1 * g2) +
        two_pole(f3, input1 * g3)
    end
  end
end

def resflt_test(start = 0.0, dur = 1.0)
  $now = start
  resflt($now, dur, 0, 0, 0, nil,
         0.1, 200, 230, 10, [0, 0, 50, 1, 100, 0], [0, 0, 100, 1],
         500, 0.995, 0.1, 1000, 0.995, 0.1, 2000, 0.995, 0.1)
  $now += dur + 0.2
  resflt($now, dur, 1, 10000, 0.01, [0, 0, 50, 1, 100, 0],
         0, 0, 0, 0, nil, nil,
         500, 0.995, 0.1, 1000, 0.995, 0.1, 2000, 0.995, 0.1)
  $now += dur + 0.2
end

# SCRATCH
def scratch(start, file, src_ratio, turntable)
  assert_type(File.exists?(file), file, 1, "an existing file")
  f = make_file2sample(file)
  turn_i = 1
  turns = turntable.length
  cur_sample = seconds2samples(turntable[0])
  turn_sample = seconds2samples(turntable[turn_i])
  rd = make_src(:srate, src_ratio)
  forwards = (src_ratio > 0.0)
  set_mus_increment(rd, -src_ratio) if forwards and turn_sample < cur_sample
  turning = 0
  last_val = last_val2 = 0.0
  run_instrument(start, ws_duration(file)) do
    break if turn_i >= turns
    val = src(rd, 0.0, lambda do |dir|
                inval = file2sample(f, cur_sample)
                cur_sample += dir
                inval
              end)
    if turning.zero?
      # we past turn point going forwards
      if forwards and cur_sample >= turn_sample
        turning = 1
        # we past turn point going backwards
      elsif (not forwards) and cur_sample <= turn_sample
        turning = -1
      end
    else
      # wait for an inflection...
      if (last_val2 <= last_val and last_val >= val) or
          (last_val2 >= last_val and last_val <= val)
        turn_i += 1
        if turn_i < turns
          turn_sample = seconds2samples(turntable[turn_i])
          forwards = (not forwards)
          set_mus_increment(rd, -mus_increment(rd))
        end
        turning = 0
      end
    end
    last_val2, last_val = last_val, val
    val
  end
  mus_close(f)
end

def scratch_test(start = 0.0, dur = 1.0)
  scratch(start, "fyow.snd", [dur, 1.5].min, [0.0, 0.5, 0.25, 1.0])
  $now = start + mus_sound_duration("fyow.snd") + 0.2
end

# PINS
#
# spectral modeling (SMS)
def pins(start, dur, file, amp, *args)
  assert_type(File.exists?(file), file, 2, "an existing file")
  transposition, time_scaler, fftsize, highest_bin, max_peaks, attack = nil
  optkey(args, binding,
         [:transposition, 1.0], # this can be used to transpose the sound
         [:time_scaler, 1.0],   # this can make things happen faster
                                # (< 1.0)/slower (> 1.0) in the output
         [:fftsize, 256],       # should be a power of 2
                                # at 22050 srate, this is ok for
                                # sounds above 300Hz or so, below that
                                # you need 512 or 1024, at 44100,
                                # probably best to double these sizes
                                # -- it takes some searching
                                # sometimes.
         [:highest_bin, 128],   # how high in fft data should we search for pks
         [:max_peaks, 16],      # how many spectral peaks to track at the max
         :attack)               # whether to use original attack via time domain
                                # splice do the sliding fft shuffle,
                                # translate to polar coordinates, find
                                # spectral peaks, match with current,
                                # do some interesting transformation,
                                # resynthesize using oscils All the
                                # envelopes are created on the fly.
                                # max-peaks is how many of these peaks
                                # we are willing to track at any given
                                # time.
  fil = make_file2sample(file)
  file_duration = ws_duration(file)
  fdr = make_vct(fftsize)
  fdi = make_vct(fftsize)
  window = make_fft_window(Blackman2_window, fftsize)
  fftamps = make_vct(fftsize)
  max_oscils = 2 * max_peaks
  current_peak_freqs = make_vct(max_oscils)
  last_peak_freqs = make_vct(max_oscils)
  current_peak_amps = make_vct(max_oscils)
  last_peak_amps = make_vct(max_oscils)
  peak_amps = make_vct(max_peaks)
  peak_freqs = make_vct(max_peaks)
  resynth_oscils = make_array(max_oscils) do make_oscil(:frequency, 0) end
  # run-time generated amplitude and frequency envelopes
  amps = make_vct(max_oscils)
  rates = make_vct(max_oscils)
  freqs = make_vct(max_oscils)
  sweeps = make_vct(max_oscils)
  lowest_magnitude = 0.001
  hop = (fftsize / 4).floor
  outhop = (time_scaler * hop).floor
  ifreq = 1.0 / outhop
  ihifreq = hz2radians(ifreq)
  # integrate Blackman-Harris window = .42323*window
  fftscale = 1.0 / (fftsize * 0.42323)
  # width and shift by fftsize
  fft_mag = @srate / fftsize
  furthest_away_accepted = 0.1
  filptr = 0
  cur_oscils = max_oscils
  ramped = (attack or 0)
  splice_attack = attack.kind_of?(Numeric)
  attack_size = (attack or 1)
  ramp_ind = 0
  ramped_attack = make_vct(attack_size)
  if (dur / time_scaler) > file_duration
    error("%s is %1.3f seconds long, \
but we'll need %1.3f seconds of data for this note",
          file, file_duration, dur / time_scaler)
  end
  trigger = outhop
  vct_scale!(window, fftscale)
  run_instrument(start, dur) do
    if splice_attack
      ramp = 1.0 / attack_size
      # my experience in translating SMS, and rumor via Greg Sandell
      # leads me to believe that there is in fact no way to model some
      # attacks successfully in this manner, so this block simply
      # splices the original attack on to the rest of the note.
      # "attack" is the number of samples to include bodily.
      out_val = amp * file2sample(fil, filptr)
      filptr += 1
      if filptr > attack_size
        mult = 1.0
        attack_size.times do |j|
          ramped_attack[j] = mult * file2sample(fil, filptr + j)
          mult -= ramp
        end
        splice_attack = false
      end
      # if out_val
      out_val
    else
      if trigger >= outhop
        peaks = 0
        # get next block of data and apply window to it
        trigger = 0
        fftsize.times do |j|
          fdr[j] = window[j] * file2sample(fil, filptr)
          filptr += 1
        end
        vct_fill!(fdi, 0.0)
        filptr -= fftsize - hop
        # get the fft
        mus_fft(fdr, fdi, fftsize, 1)
        # change to polar coordinates (ignoring phases)
        highest_bin.times do |j|
          # no need to paw through the upper half
          # (so (<= highest-bin (floor fft-size 2)))
          x = fdr[j]
          y = fdi[j]
          fftamps[j] = 2.0 * sqrt(x * x + y * y)
        end
        max_oscils.times do |j|
          last_peak_freqs[j] = current_peak_freqs[j]
          last_peak_amps[j] = current_peak_amps[j]
          current_peak_amps[j] = 0.0
        end
        vct_fill!(peak_amps, 0.0)
        ra = fftamps[0]
        la = ca = 0.0
        # search for current peaks following Xavier Serra's recommendations in
        # "A System for Sound Analysis/Transformation/Synthesis 
        #      Based on a Deterministic Plus Stochastic Decomposition"
        highest_bin.times do |j|
          la, ca, ra = ca, ra, fftamps[j]
          if ca > lowest_magnitude and ca > ra and ca > la
            # found a local maximum above the current threshold
            # (its bin number is j-1)
            logla = log10(la)
            logca = log10(ca)
            logra = log10(ra)
            offset = (0.5 * (logla - logra)) / (logla + -2 * logca + logra)
            amp_1 = 10.0 ** (logca - (0.25 * (logla - logra) * offset))
            freq = fft_mag * (j + offset - 1)
            if peaks == max_peaks
              # gotta either flush this peak,
              # or find current lowest and flush him
              minp = 0
              minpeak = peak_amps[0]
              1.upto(max_peaks - 1) do |k|
                if peak_amps[k] < minpeak
                  minp = k
                  minpeak = peak_amps[k]
                end
              end
              if amp_1 > minpeak
                peak_freqs[minp] = freq
                peak_amps[minp] = amp_1
              end
            else
              peak_freqs[peaks] = freq
              peak_amps[peaks] = amp_1
              peaks += 1
            end
          end
        end
        # now we have the current peaks -- match them to the previous
        # set and do something interesting with the result the end
        # results are reflected in the updated values in the rates and
        # sweeps arrays.  search for fits between last and current,
        # set rates/sweeps for those found try to go by largest amp
        # first
        peaks.times do |j|
          maxp = 0
          maxpk = peak_amps[0]
          1.upto(max_peaks - 1) do |k|
            if peak_amps[k] > maxpk
              maxp = k
              maxpk = peak_amps[k]
            end
          end
          # now maxp points to next largest unmatched peak
          if maxpk > 0.0
            closestp = -1
            closestamp = 10.0
            current_freq = peak_freqs[maxp]
            icf = 1.0 / current_freq
            max_peaks.times do |k|
              if last_peak_amps[k] > 0.0
                closeness = icf * (last_peak_freqs[k] - current_freq).abs
                if closeness < closestamp
                  closestamp = closeness
                  closestp = k
                end
              end
            end
            if closestamp < furthest_away_accepted
              # peak_amp is transferred to appropriate current_amp and zeroed,
              current_peak_amps[closestp] = peak_amps[maxp]
              peak_amps[maxp] = 0.0
              current_peak_freqs[closestp] = current_freq
            end
          end
        end
        max_peaks.times do |j|
          if peak_amps[j] > 0.0
            # find a place for a new oscil and start it up
            new_place = -1
            max_oscils.times do |k|
              if last_peak_amps[k].zero? and current_peak_amps[k].zero?
                new_place = k
                break
              end
            end
            current_peak_amps[new_place] = peak_amps[j]
            peak_amps[j] = 0.0
            current_peak_freqs[new_place] = peak_freqs[j]
            last_peak_freqs[new_place] = peak_freqs[j]
            set_mus_frequency(resynth_oscils[new_place],
                              transposition * peak_freqs[j])
          end
        end
        cur_oscils = 0
        max_oscils.times do |j|
          rates[j] = ifreq * (current_peak_amps[j] - last_peak_amps[j])
          if current_peak_amps[j].nonzero? or last_peak_amps[j].nonzero?
            cur_oscils = j
          end
          sweeps[j] = ihifreq * transposition *
            (current_peak_freqs[j] - last_peak_freqs[j])
        end
        cur_oscils += 1
      end
      # run oscils, update envelopes
      trigger += 1
      if ramped.zero?
        sum = 0.0
      else
        sum = ramped_attack[ramp_ind]
        ramp_ind += 1
        ramped = 0 if ramp_ind == ramped
      end
      cur_oscils.times do |j|
        if amps[j].nonzero? or rates[j].nonzero?
          sum = sum + amps[j] * oscil(resynth_oscils[j], freqs[j])
          amps[j] += rates[j]
          freqs[j] += sweeps[j]
        end
      end
      # else out_val
      amp * sum
    end
  end
  mus_close(fil)
end

def pins_test(start = 0.0, dur = 1.0)
  pins(start, dur, "fyow.snd", 1, :time_scaler, 2)
  $now = start + dur + 0.2
end

# ZC
def zc(start, dur, freq, amp, length1, length2, feedback)
  s = make_pulse_train(:frequency, freq)
  d0 = make_comb(:size, length1,
                 :max_size, [length1, length2].max + 1,
                 :scaler, feedback)
  zenv = make_env(:envelope, [0, 0, 1, 1],
                  :scaler, length2 - length1,
                  :duration, dur)
  run_instrument(start, dur) do
    comb(d0, amp * pulse_train(s), env(zenv))
  end
end

def zc_test(start = 0.0, dur = 1.0)
  $now = start
  zc($now, dur, 100, 0.4, 20, 100, 0.95)
  $now += dur + 0.2
  zc($now, dur, 100, 0.4, 100, 20, 0.95)
  $now += dur + 0.2
end

# ZN
#
# notches are spaced at srate/len, feedforward sets depth thereof so
# sweep of len from 20 to 100 sweeps the notches down from 1000 Hz to
# ca 200 Hz so we hear our downward glissando beneath the pulses.
def zn(start, dur, freq, amp, length1, length2, feedforward)
  s = make_pulse_train(:frequency, freq)
  d0 = make_notch(:size, length1,
                  :max_size, [length1, length2].max + 1,
                  :scaler, feedforward)
  zenv = make_env(:envelope, [0, 0, 1, 1],
                  :scaler, length2 - length1,
                  :duration, dur)
  run_instrument(start, dur) do
    notch(d0, amp * pulse_train(s), env(zenv))
  end
end

def zn_test(start = 0.0, dur = 1.0)
  $now = start
  zn($now, dur, 100, 0.5, 20, 100, 0.95)
  $now += dur + 0.2
  zn($now, dur, 100, 0.5, 100, 20, 0.95)
  $now += dur + 0.2
end

# ZA
def za(start, dur, freq, amp, length1, length2, feedback, feedforward)
  s = make_pulse_train(:frequency, freq)
  d0 = make_all_pass(:feedback, feedback,
                     :feedforward, feedforward,
                     :size, length1,
                     :max_size, [length1, length2].max + 1)
  zenv = make_env(:envelope, [0, 0, 1, 1],
                  :scaler, length2 - length1,
                  :duration, dur)
  run_instrument(start, dur) do
    all_pass(d0, amp * pulse_train(s), env(zenv))
  end
end

def za_test(start = 0.0, dur = 1.0)
  $now = start
  za($now, dur, 100, 0.3, 20, 100, 0.95, 0.95)
  $now += dur + 0.2
  za($now, dur, 100, 0.3, 100, 20, 0.95, 0.95)
  $now += dur + 0.2
end

# CLM-EXPSRC
def clm_expsrc(start, dur, in_file, exp_ratio, src_ratio, amp,
               rev = false, start_in_file = 0)
  assert_type(File.exists?(in_file), in_file, 0, "an existing file")
  stf = (start_in_file * srate(in_file)).floor
  fda = make_readin(in_file, :channel, 0, :start, stf)
  exa = make_granulate(lambda do |dir| readin(fda) end, :expansion, exp_ratio)
  srca = make_src(lambda do |dir| granulate(exa) end, :srate, src_ratio)
  two_chans = (channels(in_file) == 2) and (channels(@output) == 2)
  revit = @reverb and rev
  beg = seconds2samples(start)
  fin = seconds2samples(dur) + beg
  # to satisfy with_sound-option :info and :notehook
  with_sound_info(get_func_name, start, dur)
  if two_chans
    fdb = make_readin(in_file, :channel, 1, :srate, stf)
    exb = make_granulate(lambda do |dir| readin(fdb) end, :expansion, exp_ratio)
    srcb = make_src(lambda do |dir| granulate(exb) end, :srate, src_ratio)
    if revit
      rev_amp = rev * 0.5
      (beg..fin).each do |i|
        vala = src(srca) * amp
        valb = src(srcb) * amp
        outa(i, vala, @output)
        outb(i, valb, @output)
        outa(i, (vala + valb) * rev_amp, @reverb)
      end
    else                        # !revit
      (beg..fin).each do |i|
        outa(i, src(srca) * amp, @output)
        outb(i, src(srcb) * amp, @output)
      end
    end                         # revit
  else                          # !two_chans
    if revit
      rev_amp = rev
      (beg..fin).each do |i|
        vala = src(srca) * amp
        outa(i, vala, @output)
        outa(i, vala * rev_amp, @reverb)
      end
    else                        # !revit
      (beg..fin).each do |i|
        outa(i, src(srca) * amp, @output)
      end
    end                         # revit
  end                           # two_chans
end

def clm_expsrc_test(start = 0.0, dur = 1.0)
  clm_expsrc(start, dur, "oboe.snd", 2.0, 1.0, 1.0)
  $now = start + dur + 0.2
end

# EXP-SND
#
# granulate with envelopes on the expansion amount, segment envelope
# shape, segment length, hop length, and input file resampling rate
def exp_snd(file, start, dur, amp,
            exp_amt = 1.0, ramp = 0.4, seglen = 0.15,
            sr = 1.0, hop = 0.05, ampenv = nil)
  assert_type(File.exists?(file), file, 0, "an existing file")
  f0 = make_ws_reader(file, :start, 0)
  expenv = make_env(:envelope,
                    (exp_amt.kind_of?(Array) ?
                     exp_amt : [0, exp_amt, 1, exp_amt]),
                    :duration, dur)
  lenenv = make_env(:envelope,
                    (seglen.kind_of?(Array) ?
                     seglen : [0, seglen, 1, seglen]),
                    :duration, dur)
  max_seg_len, initial_seg_len = if seglen
                                   if seglen.kind_of?(Array)
                                     [max_envelope(seglen), seglen[1]]
                                   else
                                     [seglen, seglen]
                                   end
                                 else
                                   [0.15, 0.15]
                                 end
  scaler_amp = ((max_seg_len > 0.15) ? ((0.6 * 0.15) / max_seg_len) : 0.6)
  srenv = make_env(:envelope, (sr.kind_of?(Array) ? sr : [0, sr, 1, sr]),
                   :duration, dur)
  rampdata = (ramp.kind_of?(Array) ? ramp : [0, ramp, 1, ramp])
  rampenv = make_env(:envelope, rampdata, :duration, dur)
  initial_ramp_time = if ramp
                        if ramp.kind_of?(Array)
                          ramp[1]
                        else
                          ramp
                        end
                      else
                        0.4
                      end
  hopenv = make_env(:envelope, (hop.kind_of?(Array) ? hop : [0, hop, 1, hop]),
                    :duration, dur)
  max_out_hop, initial_out_hop = if hop
                                   if hop.kind_of?(Array)
                                     [max_envelope(hop), hop[1]]
                                   else
                                     [hop, hop]
                                   end
                                 else
                                   [0.05, 0.05]
                                 end
  min_exp_amt, initial_exp_amt = if exp_amt
                                   if exp_amt.kind_of?(Array)
                                     [min_envelope(exp_amt), exp_amt[1]]
                                   else
                                     [exp_amt, exp_amt]
                                   end
                                 else
                                   [1.0, 1.0]
                                 end
  max_in_hop = max_out_hop / min_exp_amt.to_f
  max_len = (@srate * ([max_out_hop, max_in_hop].max + max_seg_len)).ceil
  ampe = make_env(:envelope, (ampenv or [0, 0, 0.5, 1, 1, 0]),
                  :scaler, amp,
                  :duration, dur)
  ex_a = make_granulate(:input, lambda do |dir| ws_readin(f0) end,
                        :expansion, initial_exp_amt,
                        :max_size, max_len,
                        :ramp, initial_ramp_time,
                        :hop, initial_out_hop,
                        :length, initial_seg_len,
                        :scaler, scaler_amp)
  ex_samp = next_samp = 0.0
  vol = env(ampe)
  val_a0 = vol * granulate(ex_a)
  val_a1 = vol * granulate(ex_a)
  if min_envelope(rampdata) <= 0.0 or max_envelope(rampdata) >= 0.5
    error("ramp argument to expand must always be between 0.0 and 0.5: %1.3f",
          ramp)
  else
    run_instrument(start, dur) do
      expa = env(expenv)  # current expansion amount
      segl = env(lenenv)  # current segment length
      resa = env(srenv)   # current resampling increment
      rmpl = env(rampenv) # current ramp length (0 to 0.5)
      hp = env(hopenv)    # current hop size
      # now we set the granulate generator internal state to reflect all
      # these envelopes
      sl = (segl * @srate).floor
      rl = (rmpl * sl).floor
      vol = env(ampe)
      set_mus_length(ex_a, sl)
      set_mus_ramp(ex_a, rl)
      set_mus_frequency(ex_a, hp)
      set_mus_increment(ex_a, expa)
      next_samp += resa
      if next_samp > (ex_samp + 1)
        (next_samp - ex_samp).floor.times do
          val_a0, val_a1 = val_a1, vol * granulate(ex_a)
          ex_samp += 1
        end
      end
      if next_samp == ex_samp
        val_a0
      else
        val_a0 + (next_samp - ex_samp) * (val_a1 - val_a0)
      end
    end
    close_ws_reader(f0)
  end
end

def exp_snd_test(start = 0.0, dur = 1.0)
  $now = start
  exp_snd("fyow.snd", $now, dur, 1, [0, 1, 1, 3], 0.4, 0.15,
          [0, 2, 1, 0.5], 0.05)
  $now += dur + 0.2
  exp_snd("oboe.snd", $now, dur, 1, [0, 1, 1, 3], 0.4, 0.15,
          [0, 2, 1, 0.5], 0.2)
  $now += dur + 0.2
end

# EXPFIL
Grn = Struct.new("Grn",
                 :rampval, :rampinc,
                 :loc, :segctr, :whichseg, :ramplen, :steadylen, :trigger)

def expfil(start, dur, hopsecs, rampsecs, steadysecs, file1, file2)
  assert_type(File.exists?(file1), file1, 5, "an existing file")
  assert_type(File.exists?(file2), file2, 6, "an existing file")
  fil1 = make_file2sample(file1)
  fil2 = make_file2sample(file2)
  hop = seconds2samples(hopsecs)
  ramplen = seconds2samples(rampsecs)
  steadylen = seconds2samples(steadysecs)
  grn1 = Grn.new(0.0, 1.0 / ramplen, 0, 0, 0, ramplen, steadylen, 0)
  grn2 = Grn.new(0.0, 1.0 / ramplen, 0, 0, 0, ramplen, steadylen, 0)
  beg = seconds2samples(start)
  out1 = beg
  out2 = hop + beg
  run_instrument(start, dur) do |i|
    val = 0.0
    if i == out1
      inval = ina(grn1.loc, fil1)
      grn1.loc += 1
      if grn1.whichseg.zero? # ramp-up
        inval *= grn1.rampval
        grn1.rampval += grn1.rampinc
        grn1.segctr += 1
        if grn1.segctr == grn1.ramplen
          grn1.segctr = 0
          grn1.whichseg += 1
        end
      else
        if grn1.whichseg == 1 # steady-state
          grn1.segctr += 1
          if grn1.segctr == grn1.steadylen
            grn1.segctr = 0
            grn1.whichseg += 1
          end
        else # ramp-down
          inval *= grn1.rampval
          grn1.segctr += 1
          grn1.rampval -= grn1.rampinc
          if grn1.segctr == grn1.ramplen
            grn1.segctr = 0
            grn1.trigger = 1
            grn1.whichseg = 0
            grn1.rampval = 0.0
          end
        end
      end
      val += inval
      out1 += 1
      if grn1.trigger == 1
        grn1.trigger = 0
        out1 += hop
      end
    end
    if i == out2
      inval = ina(grn2.loc, fil2)
      grn2.loc += 1
      if grn2.whichseg.zero? # ramp-up
        inval *= grn2.rampval
        grn2.rampval += grn2.rampinc
        grn2.segctr += 1
        if grn2.segctr == grn2.ramplen
          grn2.segctr = 0
          grn2.whichseg += 1
        end
      else
        if grn2.whichseg == 1 # steady-state
          grn2.segctr += 1
          if grn2.segctr == grn2.steadylen
            grn2.segctr = 0
            grn2.whichseg += 1
          end
        else # ramp-down
          inval *= grn2.rampval
          grn2.segctr += 1
          grn2.rampval -= grn2.rampinc
          if grn2.segctr == grn2.ramplen
            grn2.segctr = 0
            grn2.trigger = 1
            grn2.whichseg = 0
            grn2.rampval = 0.0
          end
        end
      end
      val += inval
      out2 += 1
      if grn2.trigger == 1
        grn2.trigger = 0
        out2 += hop
      end
    end
    val
  end
end

def expfil_test(start = 0.0, dur = 1.0)
  expfil(start, dur, 0.2, 0.01, 0.1, "oboe.snd", "fyow.snd")
  $now = start + dur + 0.2
end

# GRAPH-EQ
#
=begin
From: Marco Trevisani <marco@ccrma.Stanford.EDU>

This should work like a Graphic Equalizer....
Very easy to use. Just some note:

"amp" & "amp-env" apply an enveloppe to the final result of the
filtering.  

"dur" as ""standard"" in my instruments, when dur = 0 it will take the length
of the sndfile input, otherwise the duration in seconds.

"gain-freq-list" is a list of gains and frequencies to
filter --in this order gain and frequencies--. There is no limit to
the size of the list. Gain can be a number or an
envelope. Unfortunatelly in this version they cant alternate, one
should chose, all envelopes or all numbers i.e.: 
case 1 -> '( .1 440.0 .3 1500.0 .2 330.0 ...etc) or 
case 2 -> '((0 .1 1 .5) 440.0 (0 1 1 .01) 1500 (0 .3 1 .5) 330.0 ...etc) 
'( .1 440.0 (0 1 1 .01) 1500 ..etc) <<< again, this is not allowed ..

"offset-gain" This apply to all the gains if case 1. It adds or
subtracts an offset to all the gains in the list. This number can be positive
or negative. In case the result is a negative number --let's say offset =
-.4 and, like in case 1, the first gain is .1, the result would be
-.3 -- the instrument will pass a gain equal to 0.  

"filt-gain-scale" & "filt-gain-base" will apply to the elements of the
envelopes if we are in case 2, gains are envelopes.

"stats" if #t --default-- prints the number of seconds processed, if
nil doesnt print anything, which will speed up a bit the process.
=end
#
def graph_eq(file, start, dur, *args)
  assert_type(File.exists?(file), file, 0, "an existing file")
  or_beg, amplitude, amp_env, amp_base, offset_gain = nil
  gain_freq_list, filt_gain_scale, filt_gain_base, a1 = nil
  optkey(args, binding,
         [:or_beg, 0],
         [:amplitude, 1],
         [:amp_env, [0, 1, 0.8, 1, 1, 0]],
         [:amp_base, 1],
         [:offset_gain, 0],
         [:gain_freq_list, [[0, 1, 1, 0], 440, [0, 0, 1, 1], 660]],
         [:filt_gain_scale, 1],
         [:filt_gain_base, 1],
         [:a1, 0.99])
  durata = (dur.zero? ? ws_duration(file) : dur)
  len = seconds2samples(durata)
  or_start = (or_beg * ws_srate(file)).round
  rd_a = make_ws_reader(file, :start, or_start)
  half_list = gain_freq_list.length / 2
  ampenv = make_env(:envelope, amp_env,
                    :scaler, amplitude,
                    :duration, durata,
                    :base, amp_base)
  gain_list = []
  0.step(gain_freq_list.length - 1, 2) do |i|
    gain_list << gain_freq_list[i]
  end
  freq_list = []
  1.step(gain_freq_list.length - 1, 2) do |i|
    freq_list << gain_freq_list[i]
  end
  if_list_in_gain = gain_list[0].kind_of?(Array)
  env_size = (if_list_in_gain ? Array.new(freq_list.length) : nil)
  frm_size = Array.new(freq_list.length)
  gains = Vct.new(len, 1.0)
  half_list.times do |i|
    gval = gain_list[i]
    fval = freq_list[i]
    if gval.kind_of?(Array)
      env_size[i] = make_env(:envelope, gval,
                             :scaler, filt_gain_scale,
                             :duration, durata,
                             :base, filt_gain_base)
      frm_size[i] = make_formant(fval, a1)
    else
      gains[i] = (offset_gain + gval < 0) ? 0 : (offset_gain + gain)
      frm_size[i] = make_formant(fval, a1)
    end
  end
  run_instrument(start, durata) do
    outval = 0.0
    inval = ws_readin(rd_a)
    half_list.times do |j|
      if if_list_in_gain
        gains[j] = env(env_size[j]) * (1.0 - a1)
      end
      outval = outval + gains[j] * formant(frm_size[j], inval)
    end
    env(ampenv) * outval
  end
  close_ws_reader(rd_a)
end

def graph_eq_test(start = 0.0, dur = 1.0)
  graph_eq("oboe.snd", start, dur, :amplitude, 50.0)
  $now = start + dur + 0.2
end

# ANOI
#
# a kind of noise reduction -- on-going average spectrum is squelched
# to some extent obviously aimed at intermittent signal in background
# noise
# this is based on Perry Cook's Scrubber.m
def anoi(infile, start, dur, fftsize = 128, amp_scaler = 1.0, r = TWO_PI)
  assert_type(File.exists?(infile), infile, 0, "an existing file")
  freq_inc = (fftsize / 2).floor
  fdi = make_vct(fftsize)
  fdr = make_vct(fftsize)
  spectr = make_vct(freq_inc, 1.0)
  scales = make_vct(freq_inc, 1.0)
  diffs = make_vct(freq_inc)
  win = make_fft_window(Blackman2_window, fftsize)
  k = 0
  amp = 0.0
  incr = amp_scaler * 4.0 / @srate
  file = make_file2sample(infile)
  radius = 1.0 - r / fftsize.to_f
  bin = @srate / fftsize
  fs = make_array(freq_inc) do |i| make_formant(i * bin, radius) end
  samp = 0
  run_instrument(start, dur) do
    inval = file2sample(file, samp)
    samp += 1
    fdr[k] = inval
    k += 1
    amp += incr if amp < amp_scaler
    if k >= fftsize
      k = 0
      spectrum(fdr, fdi, win, 1)
      freq_inc.times do |j|
        spectr[j] = 0.9 * spectr[j] + 0.1 * fdr[j]
        if spectr[j] >= fdr[j]
          diffs[j] = scales[j] / -fftsize
        else
          diffs[j] = ((fdr[j] - spectr[j]) / fdr[j] - scales[j]) / fftsize
        end
      end
    end
    outval = 0.0
    1.upto(freq_inc - 1) do |j|
      cur_scale = scales[j]
      outval = outval + cur_scale * formant(fs[j], inval)
      scales[j] += diffs[j]
    end
    amp * outval
  end
end

def anoi_test(start = 0.0, dur = 1.0)
  anoi("fyow.snd", start, dur, 128, 2.0)
  $now = start + dur + 0.2
end

=begin
Date: Fri, 25 Sep 1998 09:56:41 +0300
From: Matti Koskinen <mjkoskin@sci.fi>
To: linux-audio-dev@ginette.musique.umontreal.ca
Subject: [linux-audio-dev] Announce: alpha version of denoising
[...]
  I wrote a simple denoiser called anoi after it's parent
  clm-instrument anoi.ins.

  anoi tries to remove white noise like tape hiss from wav-
  files. Removing of noise succeeds ok, but depending of the
  original sound, some distortion can be audible.

  If someone is interested, http://www.sci.fi/~mjkoskin
  contains tarred and gzipped file.

  Now only monophonic wav-files can be denoised, but adding
  others isn't too difficult. 

-matti
mjkoskin@sci.fi
=end

# FULLMIX
#
# "matrix" can be a simple amplitude or a list of lists each inner
#     list represents one input channel's amps into one output channel
#     each element of the list can be a number, a list (turned into an
#     env) or an env
def fullmix(in_file,
            start = 0.0,
            outdur = nil,
            inbeg = 0.0,
            matrix = nil,
            srate = nil,
            reverb_amount = 0.05)
  assert_type(File.exists?(in_file), in_file, 0, "an existing file")
  dur = Float((outdur or (ws_duration(in_file) / Float((srate or 1.0)).abs)))
  in_chans = ws_channels(in_file)
  inloc = (Float(inbeg) * ws_srate(in_file)).round
  mx = file = envs = rev_mx = revframe = false
  if srate
    file = make_array(in_chans) do |chn|
      make_ws_reader(in_file, :start, inloc, :channel, chn)
    end
  else
    file = in_file
  end
  mx = if matrix
         make_mixer([in_chans, @channels].max)
       else
         make_scalar_mixer([in_chans, @channels].max, 1.0)
       end
  if @ws_reverb and reverb_amount.positive?
    rev_mx = make_mixer(in_chans)
    in_chans.times do |chn|
      mixer_set!(rev_mx, chn, 0, reverb_amount)
    end
    revframe = make_frame(1)
  end
  case matrix
  when Array
    in_chans.times do |ichn|
      inlist = matrix[ichn]
      @channels.times do |ochn|
        outn = inlist[ochn]
        case outn
        when Numeric
          mixer_set!(mx, ichn, ochn, outn)
        when Array, Mus
          unless envs
            envs = make_array(in_chans) do
              make_array(@channels, false)
            end
          end
          if env?(outn)
            envs[ichn][ochn] = outn
          else
            envs[ichn][ochn] = make_env(:envelope, outn, :duration, dur)
          end
        else
          Snd.warning("unknown element in matrix: %s", outn.inspect)
        end
      end
    end
  when Numeric
    # matrix is a number in this case (a global scaler)
    in_chans.times do |i|
      if i < @channels
        mixer_set!(mx, i, i, matrix)
      end
    end
  end
  start = Float(start)
  # to satisfy with_sound-option :info and :notehook
  with_sound_info(get_func_name, start, dur)
  run_fullmix(start, dur, in_chans, srate,
              inloc, file, mx, rev_mx, revframe, envs)
  array?(file) and file.each do |rd| close_ws_reader(rd) end
end

class Snd_Instrument
  def run_fullmix(start, dur, in_chans, sr,
                  inloc, file, mx, rev_mx, revframe, envs)
    beg = seconds2samples(start)
    samps = seconds2samples(dur)
    unless sr
      @out_snd = with_closed_sound(@out_snd) do |snd_name|
        mus_mix(snd_name, file, beg, samps, inloc, mx, envs)
      end
      if rev_mx
        @rev_snd = with_closed_sound(@rev_snd) do |snd_name|
          mus_mix(snd_name, file, beg, samps, inloc, rev_mx, false)
        end
      end
    else
      out_data = make_sound_data(@channels, samps)
      if rev_mx
        rev_data = make_sound_data(@reverb_channels, samps)
      end
      inframe = make_frame(in_chans)
      outframe = make_frame(@channels)
      srcs = make_array(in_chans) do
        make_src(:srate, sr)
      end
      samps.times do |i|
        if envs
          in_chans.times do |chn|
            @channels.times do |ochn|
              if envs[chn] and env?(envs[chn][ochn])
                mixer_set!(mx, chn, ochn, env(envs[chn][ochn]))
              end
            end
          end
        end
        in_chans.times do |chn|
          frame_set!(inframe, chn,
                     src(srcs[chn], 0.0,
                         lambda do |dir| ws_readin(file[chn]) end))
        end
        frame2sound_data!(out_data, i, frame2frame(inframe, mx, outframe))
        if rev_mx
          frame2sound_data!(rev_data, i, frame2frame(inframe, rev_mx, revframe))
        end
      end
      @channels.times do |chn|
        mix_vct(out_data.to_vct(chn), beg, @out_snd, chn, false)
      end
      if rev_mx
        @reverb_channels.times do |chn|
          mix_vct(rev_data.to_vct(chn), beg, @rev_snd, chn, false)
        end
      end
    end
  end
end

class CLM_Instrument
  def run_fullmix(start, dur, in_chans, sr,
                  inloc, file, mx, rev_mx, revframe, envs)
    beg = seconds2samples(start)
    samps = seconds2samples(dur)
    unless sr
      mus_mix(@ws_output, file, beg, samps, inloc, mx, envs)
      if rev_mx
        mus_mix(@ws_reverb, file, beg, samps, inloc, rev_mx, false)
      end
    else
      inframe = make_frame(in_chans)
      outframe = make_frame(@channels)
      srcs = make_array(in_chans) do
        make_src(:srate, sr)
      end
      each_sample(start, dur) do |i|
        if envs
          in_chans.times do |chn|
            @channels.times do |ochn|
              if envs[chn] and env?(envs[chn][ochn])
                mixer_set!(mx, chn, ochn, env(envs[chn][ochn]))
              end
            end
          end
        end
        in_chans.times do |chn|
          frame_set!(inframe, chn,
                     src(srcs[chn], 0.0,
                         lambda do |dir| readin(file[chn]) end))
        end
        frame2file(@ws_output, i, frame2frame(inframe, mx, outframe))
        if rev_mx
          frame2file(@ws_reverb, i, frame2frame(inframe, rev_mx, revframe))
        end
      end
    end
  end
end

def fullmix_test(start = 0.0, dur = 1.0)
  $now = start
  fullmix("pistol.snd", $now, dur)
  $now += dur + 0.2
  fullmix("oboe.snd", $now, dur, 0,
          [[0.1, make_env([0, 0, 1, 1], :duration, dur, :scaler, 0.5)]])
  $now += dur + 0.2
end

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

# ;;; calculate a random spread around a center of 0
def random_spread(spread)
  spread.nonzero? ? (random(spread) - spread / 2.0) : 0.0
end

# ;;; create a constant envelope if argument is a number
def envelope_or_number(val)
  assert_type((number?(val) or array?(val) or vct?(val)),
              val, 0, "a number, an array or a vct")
  case val
  when Numeric
    [0, Float(val), 1, Float(val)]
  when Vct
    val.to_a
  when Array
    val
  end
end

# ;;; create a vct from an envelope
def make_gr_env(env, length = 512)
  length_1 = (length - 1).to_f
  make_vct!(length) do |i|
    envelope_interp(i / length_1, env)
  end
end

# ;;; Grain envelopes
def raised_cosine(*args)
  duty_cycle, length = nil
  optkey(args, binding,
         [:duty_cycle, 100],
         [:length, 128])
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
  assert_type(File.exists?(file), file, 3, "an existing file")
  input_channel = nil
  grains, amp_envelope, grain_envelope, grain_envelope_end = nil
  grain_envelope_transition, grain_envelope_array_size, grain_duration = nil
  grain_duration_spread, grain_duration_limit, srate, srate_spread = nil
  srate_linear, srate_base, srate_error, grain_start, grain_start_spread = nil
  grain_start_in_seconds, grain_density, grain_density_spread = nil
  reverb_amount, reverse, where_to, where_bins, grain_distance = nil
  grain_distance_spread, grain_degree, grain_degree_spread = nil
  optkey(args, binding,
         [:input_channel, 0],
         [:grains, 0],
         [:amp_envelope, [0, 0, 0.3, 1, 0.7, 1, 1, 0]],
         [:grain_envelope, [0, 0, 0.3, 1, 0.7, 1, 1, 0]],
         [:grain_envelope_end, false],
         [:grain_envelope_transition, [0, 0, 1, 1]],
         [:grain_envelope_array_size, 512],
         [:grain_duration, 0.1],
         [:grain_duration_spread, 0.0],
         [:grain_duration_limit, 0.002],
         [:srate, 0.0],
         [:srate_spread, 0.0],
         [:srate_linear, false],
         [:srate_base, 2.0 ** (1.0 / 12)],
         [:srate_error, 0.01],
         [:grain_start, [0, 0, 1, 1]],
         [:grain_start_spread, 0.0],
         [:grain_start_in_seconds, false],
         [:grain_density, 10.0],
         [:grain_density_spread, 0.0],
         [:reverb_amount, 0.01],
         [:reverse, false],
         [:where_to, 0],
         [:where_bins, []],
         [:grain_distance, 1.0],
         [:grain_distance_spread, 0.0],
         [:grain_degree, 45.0],
         [:grain_degree_spread, 0.0])
  beg, fin = times2samples(start, dur)
  in_file_channels = ws_channels(file)
  in_file_sr       = ws_srate(file)
  in_file_dur      = ws_duration(file)
  rd = make_ws_reader(file,
                      :channel, [input_channel, in_file_channels - 1].min,
                      :vct?, true)
  in_file_reader   = make_src(:input, lambda do |dir| ws_readin(rd) end,
                              :srate, 1.0)
  set_mus_increment(in_file_reader, -1) if reverse
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
  sr_spread_env = make_env(:envelope, envelope_or_number(srate_spread),
                           :duration, dur)
  amp_env = make_env(:envelope, amp_envelope, :scaler, amp, :duration, dur)
  gr_dur = make_env(:envelope, envelope_or_number(grain_duration),
                    :duration, dur)
  gr_dur_spread = make_env(:envelope, envelope_or_number(grain_duration_spread),
                           :duration, dur)
  gr_start_scaler = (grain_start_in_seconds ? 1.0 : in_file_dur)
  gr_start = make_env(:envelope, envelope_or_number(grain_start),
                      :duration, dur)
  gr_start_spread = make_env(:envelope, envelope_or_number(grain_start_spread),
                             :duration, dur)
  gr_dens_env = make_env(:envelope, envelope_or_number(grain_density),
                         :duration, dur)
  gr_dens_spread_env = make_env(:envelope,
                                envelope_or_number(grain_density_spread),
                                :duration, dur)
  if vct?(grain_envelope)
    ge = grain_envelope
  else
    ge = make_gr_env(grain_envelope, grain_envelope_array_size)
  end
  gr_env = make_table_lookup(:frequency, 1.0, :initial_phase, 0, :wave, ge)
  if grain_envelope_end
    if vct?(grain_envelope_end)
      ge = grain_envelope_end
    else
      ge = make_gr_env(grain_envelope_end, grain_envelope_array_size)
    end
  else
    ge = make_vct(512)
  end
  gr_env_end = make_table_lookup(:frequency, 1.0, :initial_phase, 0, :wave, ge)
  gr_int_env = make_env(:envelope,
                        envelope_or_number(grain_envelope_transition),
                        :duration, dur)
  gr_dist = make_env(:envelope, envelope_or_number(grain_distance),
                     :duration, dur)
  gr_dist_spread = make_env(:envelope,
                            envelope_or_number(grain_distance_spread),
                            :duration, dur)
  gr_degree = make_env(:envelope, envelope_or_number(grain_degree),
                       :duration, dur)
  gr_degree_spread = make_env(:envelope,
                              envelope_or_number(grain_degree_spread),
                              :duration, dur)
  gr_start_sample = beg
  gr_samples = 0
  gr_offset = 1
  gr_dens = 0.0
  gr_dens_spread = 0.0
  grain_counter = 0
  samples = 0
  first_grain = true
  case grain_duration
  when Numeric
    dur += grain_duration
  when Array
    dur += grain_duration.last
  end
  run_instrument(start, dur, :degree, 45.0) do
    if gr_offset < gr_samples
      gr_offset += 1
      (if grain_envelope_end
         gr_where = env(gr_int_env)
         (1 - gr_where) * table_lookup(gr_env) +
           gr_where * table_lookup(gr_env_end)
       else
         table_lookup(gr_env)
       end) * env(amp_env) * src(in_file_reader)
    else
      if first_grain
        first_grain = false
        gr_start_sample = beg
      else
        gr_start_sample += seconds2samples(1.0 / (gr_dens + gr_dens_spread))
        if (gr_start_sample > fin) or
          (grains.nonzero? and (grain_counter >= grains))
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
      gr_duration = [grain_duration_limit,
        env(gr_dur) + random_spread(env(gr_dur_spread))].max
      gr_samples = seconds2samples(gr_duration)
      gr_srate = if srate_linear
                   env(sr_env) + random_spread(env(sr_spread_env))
                 else
                   env(sr_env) * srate_base ** random_spread(env(sr_spread_env))
                 end
      set_mus_increment(in_file_reader, gr_srate)
      in_samples = (gr_samples / (1.0 / srate_ratio)).round
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
      if where.nonzero? and where_bins.length > 0
        (where_bins.length - 1).times do |chn|
          locsig_set!(@locsig, chn,
                      (where.between?(where_bins[chn], where_bins[chn + 1]) ?
                       1.0 :
                       0.0))
        end
      else
        if where_to == Grani_to_grain_allchans
          @channels.times do |chn|
            locsig_set!(@locsig, chn, 1.0)
          end
        else
          set_mus_location(gr_dist, gr_from_beg)
          set_mus_location(gr_dist_spread, gr_from_beg)
          set_mus_location(gr_degree, gr_from_beg)
          set_mus_location(gr_degree_spread, gr_from_beg)
          deg = env(gr_degree) + random_spread(env(gr_degree_spread))
          dist = env(gr_dist) + random_spread(env(gr_dist_spread))
          dist_scl = 1.0 / [dist, 1.0].max
          if sample2file?(@ws_reverb)
            locsig_reverb_set!(@locsig, 0,
                               reverb_amount * (1.0 / sqrt([dist, 1.0].max)))
          end
          if @channels == 1
            locsig_set!(@locsig, 0, dist_scl)
          else
            if @channels == 2
              frac = [90.0, [0.0, deg].max].min / 90.0
              locsig_set!(@locsig, 0, dist_scl * (1.0 - frac))
              locsig_set!(@locsig, 1, dist_scl * frac)
            else
              if @channels > 2
                locsig_set!(@locsig, 0, if deg.between?(0, 90)
                                          dist_scl * ((90.0 - deg) / 90.0)
                                        else
                                          if deg.between?(270, 360)
                                            dist_scl * ((deg - 270.0) / 90.0)
                                          else
                                            0.0
                                          end
                                        end)
                locsig_set!(@locsig, 1, if deg.between?(90, 180)
                                          dist_scl * (180.0 - deg) / 90.0
                                        else
                                          if deg.between?(0, 90)
                                            dist_scl * (deg / 90.0)
                                          else
                                            0.0
                                          end
                                        end)
                locsig_set!(@locsig, 2, if deg.between?(180, 270)
                                          dist_scl * (270.0 - deg) / 90.0
                                        else
                                          if deg.between?(90, 180)
                                            dist_scl * (deg - 90.0) / 90.0
                                          else
                                            0.0
                                          end
                                        end)
                if @channels > 3
                  locsig_set!(@locsig, 3, if deg.between?(270, 360)
                                            dist_scl * (360.0 - deg) / 90.0
                                          else
                                            if deg.between?(180, 270)
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
      set_ws_location(rd, in_start)
      0.0
    end
  end
  close_ws_reader(rd)
end

def grani_test(start = 0.0, dur = 1.0)
  grani(start, dur, 5.0, "oboe.snd", :grain_envelope, raised_cosine())
  $now = start + dur + 0.2
end

# BES-FM
def bes_fm(start, dur, freq, amp, ratio, index)
  car_ph = mod_ph = 0.0
  car_incr = hz2radians(freq)
  mod_incr = ratio.to_f * car_incr
  ampenv = make_env(:envelope, [0, 0, 25, 1, 75, 1, 100, 0],
                    :scaler, amp,
                    :duration, dur)
  run_instrument(start, dur) do
    out_val = env(ampenv) * bes_j1(car_ph)
    car_ph = car_ph + car_incr + index.to_f * bes_j1(mod_ph)
    mod_ph += mod_incr
    out_val
  end
end

def bes_fm_test(start = 0.0, dur = 1.0)
  bes_fm(start, dur, 440, 10, 1, 4)
  $now = start + dur + 0.2
end

# SSB_FM
class Ssb_fm < Musgen
  def initialize(freq)
    super()
    @frequency = freq
    @osc1 = make_oscil(freq, 0)
    @osc2 = make_oscil(freq, HALF_PI)
    @osc3 = make_oscil(0, 0)
    @osc4 = make_oscil(0, HALF_PI)
    @hilbert = make_hilbert_transform(40)
    @delay = make_delay(40)
  end

  def inspect
    format("%s.new(%s)", self.class, @frequency)
  end

  def to_s
    format("#<%s freq: %s>", self.class, @frequency)
  end

  def run_func(val1 = 0.0, val2 = 0.0)
    ssb_fm(val1)
  end
  
  def ssb_fm(modsig)
    am0 = oscil(@osc1)
    am1 = oscil(@osc2)
    car0 = oscil(@osc3, hilbert_transform(@hilbert, modsig))
    car1 = oscil(@osc4, delay(@delay, modsig))
    am0 * car0 + am1 * car1
  end
end

def make_ssb_fm(freq = 440.0)
  Ssb_fm.new(freq)
end

def ssb_fm?(obj)
  obj.kind_of?(Ssb_fm)
end

def ssb_fm(gen, modsig = 0.0)
  gen.ssb_fm(modsig)
end

# FM2
class Fm2 < Musgen
  def initialize(f1, f2, f3, f4, p1, p2, p3, p4)
    super()
    @osc1 = make_oscil(f1, p1)
    @osc2 = make_oscil(f2, p2)
    @osc3 = make_oscil(f3, p3)
    @osc4 = make_oscil(f4, p4)
  end

  def inspect
    format("%s.new(%s, %s, %s, %s, %s, %s, %s, %s)",
           self.class, @f1, @f2, @f3, @f4, @p1, @p2, @p3, @p4)
  end

  def to_s
    format("#<%s %1.3f, %1.3f, %1.3f, %1.3f, %1.3f, %1.3f, %1.3f, %1.3f>",
           self.class, @f1, @f2, @f3, @f4, @p1, @p2, @p3, @p4)
  end

  def run_func(val1 = 0.0, val2 = 0.0)
    fm2(val1)
  end
  
  def fm2(index)
    (oscil(@osc1, index * oscil(@osc2)) +
     oscil(@osc3, index * oscil(@osc4))) * 0.25
  end
end

# make_fm2(1000, 100, 1000, 100, 0, 0, HALF_PI, HALF_PI)
# make_fm2(1000, 100, 1000, 100, 0, 0, 0, HALF_PI)
def make_fm2(f1, f2, f3, f4, p1, p2, p3, p4)
  Fm2.new(f1, f2, f3, f4, p1, p2, p3, p4)
end

def fm2?(obj)
  obj.kind_of?(Fm2)
end

def fm2(gen, index = 0.0)
  gen.fm2(index)
end

def clm_ins_test(start = 0.0, dur = 1.0)
  $now = start
  violin_test($now, dur)
  fm_violin_test($now, dur)
  pluck_test($now, dur)
  vox_test($now, dur)
  fofins_test($now, dur)
  fm_trumpet_test($now, dur)
  pqw_vox_test($now, dur)
  flute_test($now, dur)
  fm_bell_test($now, dur)
  fm_insect_test($now, dur)
  fm_drum_test($now, dur)
  gong_test($now, dur)
  attract_test($now, dur)
  pqw_test($now, dur)
  tubebell_test($now, dur)
  wurley_test($now, dur)
  rhodey_test($now, dur)
  hammondoid_test($now, dur)
  metal_test($now, dur)
  drone_canter_test($now, dur)
  reson_test($now, dur)
  cellon_test($now, dur)
  gran_synth_test($now, dur)
  touch_tone_test($now, dur)
  spectra_test($now, dur)
  two_tab_test($now, dur)
  lbj_piano_test($now, dur)
  resflt_test($now, dur)
  scratch_test($now, dur)
  pins_test($now, dur)
  zc_test($now, dur)
  zn_test($now, dur)
  za_test($now, dur)
  clm_expsrc_test($now, dur)
  exp_snd_test($now, dur)
  expfil_test($now, dur)
  graph_eq_test($now, dur)
  anoi_test($now, dur)
  fullmix_test($now, dur)
  grani_test($now, dur)
  bes_fm_test($now, dur)
end

# clm-ins.rb ends here
