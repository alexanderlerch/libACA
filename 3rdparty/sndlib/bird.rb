# bird songs
# translated (semi-automatically) from a Sambox note list to bird.clm, then bird.scm, then bird.rb

$out_file = false
$out_data = false
$with_editable_mixes = false

def normalize_partials(lst1)
  lst = lst1.flatten # i.e. copy -- there doesn't seem to be a copy method for arrays?
  sum = 0.0
  len = (lst.length) / 2
  0.upto(len - 1) do |i|
    sum = sum + lst[(i * 2) + 1]
  end
  0.upto(len - 1) do |i|
    lst[(i * 2) + 1] = lst[(i * 2) + 1] / sum
  end
  lst
end

def bigbird(start, dur, frequency, freqskew, amplitude, freq_envelope, amp_envelope, partials)
  gls_env = make_env(freq_envelope, hz2radians(freqskew), dur)
  os = make_oscil(frequency)
  coeffs = partials2polynomial(normalize_partials(partials))
  amp_env = make_env(amp_envelope, amplitude, dur)	
  beg = (srate() * start).round
  len = (srate() * dur).round
  local_data  = make_vct len
  local_data.map! do
    env(amp_env) *
    polynomial(coeffs,
	       oscil(os, env(gls_env)))
  end
  vct_add!($out_data, local_data, beg)
end

def bird(start, dur, frequency, freqskew, amplitude, freq_envelope, amp_envelope)
  gls_env = make_env(freq_envelope, hz2radians(freqskew), dur)
  os = make_oscil(frequency)
  amp_env = make_env(amp_envelope, amplitude, dur)
  beg = (srate() * start).round
  len = (srate() * dur).round
  local_data  = make_vct len
  local_data.map! do
    env(amp_env) * oscil(os, env(gls_env))
  end
  vct_add!($out_data, local_data, beg)
end

def one_bird(beg, maxdur, birdname, &func)
  $out_data = make_vct((srate() * maxdur).round)
  func.call()
  mix_vct($out_data, (beg*srate()).round, $out_file, 0, $with_editable_mixes)
  birdname
end

$main_amp = [0.00, 0.00, 0.25, 1.00, 0.60, 0.70, 0.75, 1.00, 1.00, 0.0]
$bird_tap = [0.00, 0.00, 0.01, 1.00, 0.99, 1.00, 1.00, 0.0]
$bird_amp = [0.00, 0.00, 0.25, 1.00, 0.75, 1.00, 1.00, 0.0]

def orchard_oriole(beg)
  oriup = [0.00, 0.00, 1.00, 1.0]
  oridwn = [0.00, 1.00, 1.00, 0.0]
  oriupdwna = [0.00, 0.00, 0.60, 1.00, 1.00, 0.60]
  oriupdwnb = [0.00, 0.50, 0.30, 1.00, 1.00, 0.0]
  oribiga = [0.00, 0.90, 0.15, 1.00, 0.40, 0.30, 0.60, 0.60, 0.85, 0.00, 1.00, 0.0]
  orimid = [0.00, 1.00, 0.05, 0.50, 0.10, 1.00, 0.25, 0.00, 0.85, 0.50, 1.00, 0.0]
  oridwnup = [0.00, 0.30, 0.25, 0.00, 1.00, 1.0]
  oriamp = [0.00, 0.00, 0.10, 1.00, 1.00, 0.0]
  
  one_bird(beg, 3.0, "orchard_oriole") do
   bird(0.38, 0.03, 3700, 100, 0.05, oridwn, $main_amp)
   bird(0.41, 0.05, 2500, 1000, 0.1, oriup, $main_amp)
   bigbird(0.5, 0.1, 2000, 800, 0.2, oriupdwna, $main_amp, [1, 1, 2, 0.02, 3, 0.05])
   bird(0.65, 0.03, 3900, 1200, 0.1, oridwn, $main_amp)
   bigbird(0.7, 0.21, 2000, 1200, 0.15, oribiga, $main_amp, [1, 1, 2, 0.05])
   bird(1.0, 0.05, 4200, 1000, 0.1, oridwn, $main_amp)
   bigbird(1.1, 0.1, 2000, 1000, 0.25, orimid, $main_amp, [1, 1, 2, 0.05])
   bigbird(1.3, 0.1, 2000, 1000, 0.25, orimid, $main_amp, [1, 1, 2, 0.05])
   bird(1.48, 0.1, 2300, 3200, 0.1, oriupdwnb, oriamp)
   bird(1.65, 0.03, 1800, 300, 0.05, oriup, $main_amp)
   bird(1.7, 0.03, 2200, 100, 0.04, oridwn, $main_amp)
   bird(1.8, 0.07, 2500, 2000, 0.15, oriupdwnb, oriamp)
   bigbird(1.92, 0.2, 2400, 1200, 0.25, oridwnup, $main_amp, [1, 1, 2, 0.04])
   bird(2.2, 0.02, 2200, 3000, 0.04, oriup, $main_amp)
   bird(2.28, 0.02, 2200, 3000, 0.04, oriup, $main_amp)
   bigbird(2.4, 0.17, 2000, 1000, 0.2, oriupdwna, oriamp, [1, 1, 2, 0.04])
  end
end


def cassins_kingbird(beg)
  kingfirst = [0.00, 0.30, 0.45, 1.00, 0.90, 0.10, 1.00, 0.0]
  kingsecond = [0.00, 0.00, 0.02, 0.50, 0.04, 0.00, 0.06, 0.55, 0.08, 0.05, 0.10, 0.60, 0.12, 0.05, 0.14, 0.65, 0.16, 0.10, 0.18, 0.70, 0.20, 0.10, 0.22, 0.75, 0.24, 0.15, 0.26, 0.80, 0.28, 0.20, 0.30, 0.85, 0.32, 0.25, 0.34, 0.90, 0.36, 0.30, 0.38, 0.95, 0.40, 0.40, 0.42, 1.00, 0.44, 0.50, 0.46, 1.00, 0.48, 0.45, 0.50, 1.00, 0.52, 0.50, 0.54, 1.00, 0.56, 0.40, 0.58, 0.95, 0.60, 0.40, 0.62, 0.90, 0.64, 0.40, 0.66, 0.85, 0.68, 0.35, 0.70, 0.80, 0.72, 0.30, 0.74, 0.75, 0.76, 0.25, 0.78, 0.70, 0.80, 0.20, 0.82, 0.65, 0.84, 0.10, 0.86, 0.60, 0.88, 0.00, 0.90, 0.55, 0.92, 0.00, 0.94, 0.50, 0.96, 0.00, 1.00, 0.40]
  
  one_bird(beg, 3.0, "cassins_kingbird") do
   bigbird(0.03, 0.04, 1700, 1200, 0.15, kingfirst, $main_amp, [1, 1, 2, 0.5, 3, 0, 4, 0.2])
   bigbird(0.12, 0.18, 1700, 900, 0.25, kingsecond, $main_amp, [1, 1, 2, 0.01, 3, 0, 4, 0.1])
  end
end


def chipping_sparrow(beg)
  chip_up = [0.00, 0.80, 0.15, 1.00, 0.75, 0.30, 1.00, 0.0]
  
  one_bird(beg, 1.1, "chipping_sparrow") do
   bird(0, 0.05, 4000, 2400, 0.2, chip_up, $main_amp)
   bird(0.06, 0.05, 4000, 2400, 0.2, chip_up, $main_amp)
   bird(0.12, 0.05, 4000, 2400, 0.2, chip_up, $main_amp)
   bird(0.18, 0.05, 4000, 2400, 0.2, chip_up, $main_amp)
   bird(0.24, 0.05, 4000, 2400, 0.2, chip_up, $main_amp)
   bird(0.30, 0.05, 4000, 2400, 0.2, chip_up, $main_amp)
   bird(0.36, 0.05, 4000, 2400, 0.2, chip_up, $main_amp)
   bird(0.42, 0.05, 4000, 2400, 0.2, chip_up, $main_amp)
   bird(0.48, 0.05, 4000, 2400, 0.2, chip_up, $main_amp)
   bird(0.54, 0.05, 4000, 2400, 0.2, chip_up, $main_amp)
   bird(0.60, 0.05, 4000, 2400, 0.2, chip_up, $main_amp)
   bird(0.66, 0.05, 4000, 2400, 0.2, chip_up, $main_amp)
   bird(0.72, 0.05, 4000, 2400, 0.2, chip_up, $main_amp)
   bird(0.78, 0.05, 4000, 2400, 0.2, chip_up, $main_amp)
   bird(0.84, 0.05, 4000, 2400, 0.2, chip_up, $main_amp)
   bird(0.90, 0.05, 4000, 2400, 0.2, chip_up, $main_amp)
   bird(0.96, 0.05, 4000, 2400, 0.2, chip_up, $main_amp)
  end
end


def bobwhite(beg)
  bobup1 = [0.00, 0.00, 0.40, 1.00, 1.00, 1.0]
  bobup2 = [0.00, 0.00, 0.65, 0.50, 1.00, 1.0]
  
  one_bird(beg, 2.0, "bobwhite") do
   bigbird(0.4, 0.2, 1800, 200, 0.1, bobup1, $main_amp, [1, 1, 2, 0.02])
   bigbird(1, 0.20, 1800, 1200, 0.2, bobup2, $main_amp, [1, 1, 2, 0.02])
  end
end


def western_meadowlark(beg)
  no_skw = [0.00, 0.00, 1.00, 0.0]
  down_skw = [0.00, 1.00, 0.40, 0.40, 1.00, 0.0]
  fas_down = [0.00, 1.00, 1.00, 0.0]
  
  one_bird(beg, 3.0, "western_meadowlark") do
   bigbird(0.800, 0.1, 2010.000, 0.000, 0.100, no_skw, $main_amp, [1, 1, 2, 0.04])
   bigbird(1.100, 0.15, 3000.000, 100.000, 0.110, down_skw, $main_amp, [1, 1, 2, 0.04])
   bigbird(1.300, 0.25, 2000.000, 150.000, 0.200, down_skw, $main_amp, [1, 1, 2, 0.04])
   bigbird(1.650, 0.15, 3010.000, 250.000, 0.110, down_skw, $main_amp, [1, 1, 2, 0.04])
   bigbird(1.850, 0.10, 2200.000, 150.000, 0.110, down_skw, $main_amp, [1, 1, 2, 0.04])
   bigbird(2.000, 0.10, 3200.000, 1400.000, 0.110, fas_down, $main_amp, [1, 1, 2, 0.04])
   bigbird(2.200, 0.05, 2000.000, 200.000, 0.110, fas_down, $main_amp, [1, 1, 2, 0.04])
   bigbird(2.300, 0.10, 1600.000, 0.000, 0.110, fas_down, $main_amp, [1, 1, 2, 0.04])
  end
end


def scissor_tailed_flycatcher(beg)
  scissor = [0.00, 0.00, 0.40, 1.00, 0.60, 1.00, 1.00, 0.0]
  one_bird(beg, 1.0, "scissor_tailed_flycatcher") do
   bigbird(0, 0.05, 1800, 1800, 0.2, scissor, $main_amp, [1, 0.5, 2, 1, 3, 0.5, 4, 0.1, 5, 0.01])
  end
end


def great_horned_owl(beg)
  owlup = [0.00, 0.00, 0.30, 1.00, 1.00, 1.0]
  owldown = [0.00, 1.00, 1.00, 0.0]
  
  one_bird(beg, 3.0, "great_horned_owl") do
   bigbird(0.3, 0.1, 300, 0, 0.1, $main_amp, $main_amp, [1, 1, 3, 0.02, 7, 0.01])
   bigbird(0.6, 0.4, 293, 6, 0.1, owldown, $main_amp, [1, 1, 3, 0.02, 7, 0.01])
   bigbird(1.75, 0.35, 293, 7, 0.1, owlup, $main_amp, [1, 1, 3, 0.02, 7, 0.01])
   bigbird(2.5, 0.2, 300, 0, 0.1, owlup, $main_amp, [1, 1, 3, 0.02, 7, 0.01])
  end
end


def black_throated_gray_warbler(beg)
  grayone = [0.00, 0.50, 0.02, 0.60, 0.04, 0.45, 0.06, 0.62, 0.08, 0.40, 0.10, 0.65, 0.12, 0.35, 0.14, 0.70, 0.18, 0.30, 0.20, 0.70, 0.22, 0.30, 0.24, 0.70, 0.25, 0.20, 0.30, 0.80, 0.35, 0.10, 0.40, 0.90, 0.45, 0.00, 0.50, 1.00, 0.55, 0.00, 0.60, 1.00, 0.65, 0.00, 0.70, 1.00, 0.75, 0.00, 0.80, 1.00, 0.85, 0.00, 0.90, 1.00, 0.95, 0.00, 1.00, 0.50]
  graytwo = [0.00, 0.00, 0.01, 0.40, 0.02, 0.00, 0.03, 0.40, 0.04, 0.00, 0.05, 0.40, 0.06, 0.00, 0.07, 0.40, 0.08, 0.00, 0.09, 0.40, 0.10, 0.00, 0.25, 0.80, 0.40, 0.30, 0.55, 1.00, 0.70, 0.00, 0.85, 0.80, 1.00, 0.40]
  graythree = [0.00, 1.00, 0.01, 0.60, 0.02, 1.00, 0.03, 0.60, 0.04, 1.00, 0.05, 0.60, 0.06, 1.00, 0.07, 0.60, 0.08, 1.00, 0.09, 0.60, 0.10, 1.00, 0.11, 0.60, 0.12, 1.00, 0.13, 0.60, 0.14, 1.00, 0.15, 0.60, 0.16, 1.00, 0.17, 0.60, 0.18, 1.00, 0.19, 0.60, 0.20, 1.00, 0.21, 0.55, 0.22, 1.00, 0.23, 0.50, 0.24, 1.00, 0.25, 0.50, 0.26, 1.00, 0.27, 0.50, 0.28, 1.00, 0.29, 0.50, 0.30, 1.00, 0.31, 0.50, 0.32, 1.00, 0.33, 0.50, 0.34, 1.00, 0.35, 0.50, 0.36, 1.00, 0.37, 0.50, 0.38, 1.00, 0.39, 0.50, 0.40, 1.00, 0.41, 0.50, 0.42, 1.00, 0.43, 0.50, 0.44, 1.00, 0.45, 0.50, 0.46, 1.00, 0.47, 0.50, 0.48, 1.00, 0.49, 0.50, 0.50, 1.00, 0.51, 0.50, 0.52, 1.00, 0.53, 0.50, 0.54, 1.00, 0.55, 0.50, 0.56, 1.00, 0.57, 0.50, 0.58, 1.00, 0.59, 0.50, 0.60, 1.00, 1.00, 0.0]
  grayfour = [0.00, 0.00, 1.00, 1.0]
  
  one_bird(beg, 2.0, "black_throated_gray_warbler") do
   bird(0, 0.12, 3700, 600, 0.05, grayone, $main_amp)
   bird(0.18, 0.08, 3000, 800, 0.07, graytwo, $main_amp)
   bird(0.28, 0.12, 3700, 600, 0.12, grayone, $main_amp)
   bird(0.44, 0.08, 3000, 800, 0.15, graytwo, $main_amp)
   bird(0.54, 0.12, 3700, 600, 0.20, grayone, $main_amp)
   bird(0.72, 0.08, 3000, 800, 0.25, graytwo, $main_amp)
   bird(0.82, 0.12, 3700, 600, 0.25, grayone, $main_amp)
   bird(0.96, 0.2, 3000, 2000, 0.2, graythree, $main_amp)
   bird(1.2, 0.02, 4500, 500, 0.05, grayfour, $main_amp)
   bird(1.25, 0.02, 4200, 800, 0.05, grayfour, $main_amp)
   bird(1.3, 0.02, 4000, 900, 0.05, grayfour, $main_amp)
  end
end


def yellow_warbler(beg)
  yellow_up = [0.00, 0.00, 0.60, 1.00, 1.00, 0.50]
  yellow_swirl = [0.00, 1.00, 0.05, 1.00, 0.60, 0.00, 0.80, 0.30, 1.00, 0.10]
  yellow_down = [0.00, 1.00, 1.00, 0.0]
  yellow_last = [0.00, 0.00, 0.30, 0.20, 0.80, 0.70, 1.00, 1.0]
  swirl_amp = [0.00, 0.00, 0.90, 1.00, 1.00, 0.0]
  
  one_bird(beg, 2.0, "yellow_warbler") do
   bird(0, 0.05, 5600, 400, 0.05, yellow_up, $main_amp)
   bird(0.23, 0.12, 5000, 1500, 0.15, yellow_swirl, swirl_amp)
   bird(0.45, 0.13, 5000, 1700, 0.17, yellow_swirl, swirl_amp)
   bird(0.62, 0.16, 5000, 2000, 0.20, yellow_swirl, swirl_amp)
   bird(0.85, 0.15, 5000, 2000, 0.20, yellow_swirl, swirl_amp)
   bird(1.05, 0.075, 3700, 1000, 0.20, yellow_down, $main_amp)
   bird(1.15, 0.075, 3700, 800, 0.15, yellow_down, $main_amp)
   bird(1.25, 0.075, 3700, 800, 0.15, yellow_down, $main_amp)
   bird(1.4, 0.2, 3700, 2000, 0.2, yellow_last, swirl_amp)
  end
end


def black_necked_stilt(beg)
  upamp = [0.00, 0.00, 0.90, 1.00, 1.00, 0.0]
  rampup = [0.00, 0.00, 0.50, 1.00, 1.00, 0.20]
  
  one_bird(beg, 1.0, "black_necked_stilt") do
   bigbird(0, 0.1, 900, 100, 0.2, rampup, upamp, [1, 0.5, 2, 1, 3, 0.75, 4, 0.5, 5, 0.1])
   bigbird(0.30, 0.1, 900, 200, 0.2, rampup, upamp, [1, 0.5, 2, 1, 3, 0.75, 4, 0.5, 5, 0.1])
   bigbird(0.60, 0.1, 900, 250, 0.2, rampup, upamp, [1, 0.5, 2, 1, 3, 0.75, 4, 0.5, 5, 0.1])
  end
end



def chestnut_sided_warbler(beg)
  ycurve = [0.00, 1.00, 0.30, 0.50, 0.60, 1.00, 0.80, 0.20, 1.00, 0.0]
  vcurve = [0.00, 0.20, 0.50, 1.00, 1.00, 0.0]
  wcurve = [0.00, 0.50, 0.15, 0.00, 0.45, 0.10, 0.60, 1.00, 0.70, 0.90, 1.00, 0.90]
  upcurve = [0.00, 0.00, 0.95, 1.00, 1.00, 1.0]
  downcurve = [0.00, 1.00, 0.25, 0.30, 0.60, 0.15, 1.00, 0.0]
  louder = [0.00, 0.00, 0.90, 1.00, 1.00, 0.0]
  wamp = [0.00, 0.00, 0.10, 1.00, 0.40, 0.10, 0.50, 0.90, 0.60, 0.10, 0.70, 1.00, 1.00, 0.0]
  
  one_bird(beg, 2.0, "chestnut_sided_warbler") do
   bigbird(0.1, 0.1, 4050, 1200, 0.05, ycurve, $main_amp, [1, 1, 2, 0.1])
   bigbird(0.25, 0.03, 3900, 300, 0.075, vcurve, $main_amp, [1, 1, 2, 0.1])
   bigbird(0.3, 0.1, 4050, 1200, 0.15, ycurve, louder, [1, 1, 2, 0.1])
   bigbird(0.42, 0.03, 3800, 500, 0.1, vcurve, $main_amp, [1, 1, 2, 0.1])
   bigbird(0.5, 0.1, 4000, 1200, 0.2, ycurve, $bird_tap, [1, 1, 2, 0.1])
   bigbird(0.65, 0.03, 3800, 500, 0.15, vcurve, $main_amp, [1, 1, 2, 0.1])
   bigbird(0.72, 0.1, 4000, 1200, 0.2, ycurve, $bird_tap, [1, 1, 2, 0.1])
   bigbird(0.85, 0.03, 3800, 500, 0.15, vcurve, $main_amp, [1, 1, 2, 0.1])
   bigbird(0.91, 0.1, 4000, 1200, 0.2, ycurve, $bird_tap, [1, 1, 2, 0.1])
   bigbird(1.05, 0.12, 3800, 2200, 0.15, wcurve, wamp, [1, 1, 2, 0.1])
   bigbird(1.20, 0.12, 3800, 2200, 0.15, wcurve, wamp, [1, 1, 2, 0.1])
   bigbird(1.35, 0.12, 2500, 2200, 0.25, upcurve, louder, [1, 1, 2, 0.1])
   bigbird(1.50, 0.12, 2500, 4000, 0.15, downcurve, $main_amp, [1, 1, 2, 0.1])
  end
end


def grasshopper_sparrow(beg)
  grassone = [0.00, 0.50, 0.02, 0.80, 0.04, 0.30, 0.06, 0.80, 0.07, 0.10, 0.08, 0.90, 0.10, 0.00, 0.11, 0.90, 0.12, 0.00, 0.13, 0.90, 0.14, 0.10, 0.15, 1.00, 0.16, 0.10, 0.17, 1.00, 0.18, 0.10, 0.19, 1.00, 0.20, 0.10, 0.21, 1.00, 0.22, 0.10, 0.23, 1.00, 0.24, 0.10, 0.25, 1.00, 0.26, 0.10, 0.27, 1.00, 0.28, 0.10, 0.29, 1.00, 0.30, 0.10, 0.31, 1.00, 0.32, 0.10, 0.33, 1.00, 0.34, 0.10, 0.35, 1.00, 0.36, 0.10, 0.37, 1.00, 0.38, 0.10, 0.39, 1.00, 0.40, 0.10, 0.41, 1.00, 0.42, 0.10, 0.43, 1.00, 0.44, 0.10, 0.45, 1.00, 0.46, 0.10, 0.47, 1.00, 0.48, 0.10, 0.49, 1.00, 0.50, 0.10, 0.51, 1.00, 0.52, 0.10, 0.53, 1.00, 0.54, 0.10, 0.55, 1.00, 0.56, 0.10, 0.57, 1.00, 0.58, 0.10, 0.59, 1.00, 0.60, 0.10, 0.61, 1.00, 0.62, 0.10, 0.63, 1.00, 0.64, 0.10, 0.65, 1.00, 0.66, 0.10, 0.67, 1.00, 0.68, 0.10, 0.69, 1.00, 0.70, 0.10, 0.71, 1.00, 0.72, 0.10, 0.73, 1.00, 0.74, 0.10, 0.75, 1.00, 0.76, 0.10, 0.77, 1.00, 0.78, 0.10, 0.79, 1.00, 0.80, 0.10, 0.81, 1.00, 0.82, 0.10, 0.83, 1.00, 0.84, 0.10, 0.85, 1.00, 0.86, 0.10, 0.87, 1.00, 0.88, 0.10, 0.89, 1.00, 0.90, 0.10, 0.91, 1.00, 0.92, 0.10, 0.93, 1.00, 0.94, 0.10, 0.95, 1.00, 0.96, 0.10, 0.97, 1.00, 0.98, 0.10, 1.00, 1.0]
  grasstwo = [0.00, 0.00, 0.10, 1.00, 0.20, 0.00, 0.30, 1.00, 0.40, 0.00, 0.50, 1.00, 0.60, 0.00, 0.70, 1.00, 0.80, 0.00, 0.90, 1.00, 1.00, 0.0]
  
  one_bird(beg, 3.0, "grasshopper_sparrow") do
   bird(0.49, 0.01, 8000, 100, 0.1, grasstwo, $main_amp)
   bird(0.60, 0.01, 5700, 300, 0.1, grasstwo, $main_amp)
   bird(0.92, 0.01, 3900, 100, 0.1, grasstwo, $main_amp)
   bird(1.00, 1.4, 6000, 2500, 0.2, grassone, $main_amp)
  end
end


def swamp_sparrow(beg)
  swamp_up = [0.00, 0.00, 0.60, 0.70, 1.00, 1.0]
  swamp_down = [0.00, 1.00, 0.50, 0.50, 0.60, 0.60, 1.00, 0.0]
  
  one_bird(beg, 2.0, "swamp_sparrow") do
   bird(0, 0.02, 3900, 200, 0.3, swamp_up, $main_amp)
   bird(0.035, 0.035, 3200, 3000, 0.1, swamp_down, $main_amp)
   bird(0.08, 0.025, 3700, 0, 0.1, $main_amp, $main_amp)

   bird(0.1, 0.02, 3900, 200, 0.3, swamp_up, $main_amp)
   bird(0.135, 0.035, 3200, 3000, 0.1, swamp_down, $main_amp)
   bird(0.18, 0.025, 3700, 0, 0.1, $main_amp, $main_amp)

   bird(0.2, 0.02, 3900, 200, 0.3, swamp_up, $main_amp)
   bird(0.235, 0.035, 3200, 3000, 0.1, swamp_down, $main_amp)
   bird(0.28, 0.025, 3700, 0, 0.1, $main_amp, $main_amp)

   bird(0.3, 0.02, 3900, 200, 0.3, swamp_up, $main_amp)
   bird(0.335, 0.035, 3200, 3000, 0.1, swamp_down, $main_amp)
   bird(0.38, 0.025, 3700, 0, 0.1, $main_amp, $main_amp)

   bird(0.4, 0.02, 3900, 200, 0.3, swamp_up, $main_amp)
   bird(0.435, 0.035, 3200, 3000, 0.1, swamp_down, $main_amp)
   bird(0.48, 0.025, 3700, 0, 0.1, $main_amp, $main_amp)

   bird(0.5, 0.02, 3900, 200, 0.3, swamp_up, $main_amp)
   bird(0.535, 0.035, 3200, 3000, 0.1, swamp_down, $main_amp)
   bird(0.58, 0.025, 3700, 0, 0.1, $main_amp, $main_amp)

   bird(0.6, 0.02, 3900, 200, 0.3, swamp_up, $main_amp)
   bird(0.635, 0.035, 3200, 3000, 0.1, swamp_down, $main_amp)
   bird(0.68, 0.025, 3700, 0, 0.1, $main_amp, $main_amp)

   bird(0.7, 0.02, 3900, 200, 0.3, swamp_up, $main_amp)
   bird(0.735, 0.035, 3200, 3000, 0.1, swamp_down, $main_amp)
   bird(0.78, 0.025, 3700, 0, 0.1, $main_amp, $main_amp)

   bird(0.8, 0.02, 3900, 200, 0.3, swamp_up, $main_amp)
   bird(0.835, 0.035, 3200, 3000, 0.1, swamp_down, $main_amp)
   bird(0.88, 0.025, 3700, 0, 0.1, $main_amp, $main_amp)

   bird(0.9, 0.02, 3900, 200, 0.3, swamp_up, $main_amp)
   bird(0.935, 0.035, 3200, 3000, 0.1, swamp_down, $main_amp)
   bird(0.98, 0.025, 3700, 0, 0.1, $main_amp, $main_amp)
  end
end


def golden_crowned_sparrow(beg)
  goldone = [0.00, 1.00, 0.25, 0.20, 1.00, 0.0]
  goldtwo = [0.00, 0.90, 0.05, 1.00, 0.10, 0.40, 1.00, 0.0]
  goldtrill = [0.00, 0.50, 0.10, 0.00, 0.20, 1.00, 0.30, 0.00, 0.40, 1.00, 0.50, 0.00, 0.60, 1.00, 0.70, 0.00, 0.80, 1.00, 0.90, 0.00, 1.00, 0.50]
  
  one_bird(beg, 3.0, "golden_crowned_sparrow") do
   bird(0.6, 0.5, 4300, 1000, 0.15, goldone, $main_amp)
   bird(1.3, 0.45, 3300, 200, 0.15, goldone, $main_amp)
   bird(1.75, 0.4, 3800, 100, 0.15, goldtwo, $main_amp)
   bird(2.2, 0.3, 3800, 100, 0.1, goldtrill, $main_amp)
  end
end


def indigo_bunting(beg)
  buntdwn = [0.00, 1.00, 1.00, 0.0]
  buntv = [0.00, 0.00, 0.50, 1.00, 1.00, 0.0]
  bunty = [0.00, 1.00, 0.50, 0.00, 1.00, 0.90]
  buntn = [0.00, 0.80, 0.30, 1.00, 0.70, 0.20, 1.00, 0.0]
  buntx = [0.00, 1.00, 0.10, 0.50, 0.25, 0.90, 1.00, 0.0]
  buntup = [0.00, 0.00, 1.00, 1.0]
  
  one_bird(beg, 3.0, "indigo_bunting") do
   bird(0.4, 0.08, 3000, 700, 0.25, buntdwn, $main_amp)
   bird(0.52, 0.02, 6200, 1000, 0.05, buntdwn, $main_amp)
   bird(0.55, 0.15, 3500, 2300, 0.1, buntv, $main_amp)
   bird(0.74, 0.02, 6200, 1800, 0.05, buntx, $main_amp)
   bird(0.80, 0.15, 3400, 2300, 0.1, buntv, $main_amp)
   bird(1.00, 0.1, 3400, 800, 0.2, buntv, $main_amp)
   bird(1.13, 0.03, 4100, 2000, 0.05, buntdwn, $main_amp)
   bird(1.25, 0.08, 3400, 800, 0.2, buntv, $main_amp)
   bird(1.40, 0.03, 4100, 2000, 0.05, buntdwn, $main_amp)
   bird(1.5, 0.07, 3700, 300, 0.1, buntdwn, $main_amp)
   bird(1.6, 0.1, 4100, 2200, 0.15, bunty, $main_amp)
   bird(1.72, 0.05, 3700, 300, 0.1, buntdwn, $main_amp)
   bird(1.81, 0.1, 4100, 2200, 0.15, bunty, $main_amp)
   bird(1.94, 0.07, 5200, 1800, 0.2, buntn, $main_amp)
   bird(2.05, 0.08, 3000, 1500, 0.15, buntup, $main_amp)
   bird(2.20, 0.07, 5200, 1800, 0.2, buntn, $main_amp)
   bird(2.33, 0.08, 3000, 1500, 0.15, buntup, $main_amp)
   bird(2.43, 0.07, 5200, 1800, 0.1, buntn, $main_amp)
   bird(2.51, 0.08, 3000, 1500, 0.10, buntup, $main_amp)
  end
end


def hooded_warbler(beg)
  hoodup = [0.00, 0.00, 1.00, 1.0]
  hooddown = [0.00, 1.00, 1.00, 0.0]
  
  one_bird(beg, 3.0, "hooded_warbler") do
   bird(0.6, 0.03, 3900, 1600, 0.05, hooddown, $main_amp)
   bird(0.64, 0.03, 3900, 1700, 0.05, hooddown, $main_amp)
   bird(0.8, 0.03, 3900, 2000, 0.10, hooddown, $main_amp)
   bird(0.84, 0.03, 3900, 2000, 0.10, hooddown, $main_amp)
   bird(0.93, 0.03, 3900, 2100, 0.15, hooddown, $main_amp)
   bird(0.97, 0.03, 3900, 2100, 0.15, hooddown, $main_amp)
   bird(1.05, 0.03, 3900, 2100, 0.05, hooddown, $main_amp)
   bird(1.09, 0.03, 3900, 2100, 0.2, hooddown, $main_amp)
   bird(1.17, 0.03, 3900, 2100, 0.2, hooddown, $main_amp)
   bird(1.21, 0.03, 3900, 2100, 0.2, hooddown, $main_amp)
   bird(1.39, 0.03, 3900, 2100, 0.2, hooddown, $main_amp)
   bird(1.43, 0.03, 3900, 2100, 0.2, hooddown, $main_amp)
   bird(1.51, 0.03, 3900, 2100, 0.2, hooddown, $main_amp)
   bird(1.55, 0.03, 3900, 2100, 0.2, hooddown, $main_amp)
   bird(1.63, 0.03, 3900, 2100, 0.2, hooddown, $main_amp)
   bird(1.67, 0.03, 3900, 2100, 0.2, hooddown, $main_amp)
   bird(1.75, 0.03, 3900, 2100, 0.2, hooddown, $main_amp)
   bird(1.80, 0.03, 3900, 2100, 0.2, hooddown, $main_amp)

   bird(1.90, 0.04, 3000, 1000, 0.15, hoodup, $main_amp)
   bird(1.98, 0.04, 3000, 1000, 0.15, hoodup, $main_amp)
   bird(2.05, 0.04, 3000, 1000, 0.15, hoodup, $main_amp)
   bird(2.13, 0.04, 3000, 1000, 0.15, hoodup, $main_amp)
   bird(2.21, 0.04, 3000, 1000, 0.15, hoodup, $main_amp)
   bird(2.29, 0.04, 3000, 1000, 0.15, hoodup, $main_amp)
   bird(2.37, 0.04, 3000, 1000, 0.15, hoodup, $main_amp)
   bird(2.45, 0.04, 3000, 1000, 0.15, hoodup, $main_amp)
  end
end



def american_widgeon(beg)
  widgeon = [0.00, 0.00, 0.50, 1.00, 1.00, 0.0]
  
  one_bird(beg, 1.0, "american_widgeon") do
   bigbird(0.3, 0.07, 1900, 300, 0.15, widgeon, widgeon, [1, 1, 2, 0.02])
   bigbird(0.4, 0.11, 1700, 1400, 0.25, widgeon, widgeon, [1, 0.7, 2, 1, 3, 0.02])
   bigbird(0.55, 0.07, 1900, 300, 0.15, widgeon, widgeon, [1, 1, 2, 0.02])
  end
end


def louisiana_waterthrush(beg)
  water_one = [0.00, 0.80, 0.35, 0.40, 0.45, 0.90, 0.50, 1.00, 0.75, 1.00, 1.00, 0.10]
  water_two = [0.00, 1.00, 0.40, 0.00, 0.60, 0.10, 1.00, 0.80]
  water_three = [0.00, 1.00, 0.95, 0.00, 1.00, 0.0]
  water_four = [0.00, 0.00, 1.00, 1.0]
  water_five = [0.00, 1.00, 1.00, 0.0]
  water_amp = [0.00, 0.00, 0.35, 1.00, 0.50, 0.20, 0.90, 1.00, 1.00, 0.0]
  water_damp = [0.00, 0.00, 0.90, 1.00, 1.00, 0.0]
  
  one_bird(beg, 2.0, "louisiana_waterthrush") do
   bird(0, 0.17, 4100, 2000, 0.2, water_one, water_amp)
   bird(0.32, 0.18, 4050, 2050, 0.3, water_one, water_amp)
   bird(0.64, 0.20, 4000, 1900, 0.25, water_one, water_amp)
   bird(0.9, 0.2, 3900, 2000, 0.3, water_two, $bird_tap)
   bird(1.25, 0.12, 3000, 3000, 0.25, water_three, water_damp)
   bird(1.4, 0.1, 2700, 1500, 0.2, water_four, water_damp)
   bird(1.58, 0.02, 5200, 1000, 0.1, water_five, $main_amp)
   bird(1.65, 0.02, 5200, 1000, 0.1, water_five, $main_amp)
   bird(1.7, 0.035, 3200, 1000, 0.1, water_four, water_damp)
  end
end


def robin(beg)
  r_one = [0.00, 0.10, 0.08, 0.70, 0.30, 0.00, 0.35, 1.00, 0.40, 0.30, 1.00, 0.30]
  r_two = [0.00, 0.00, 0.10, 1.00, 0.20, 0.70, 0.35, 0.70, 0.65, 0.30, 0.70, 0.50, 0.80, 0.00, 0.90, 0.20, 1.00, 0.0]
  r_three = [0.00, 0.20, 0.25, 1.00, 0.60, 0.70, 0.90, 0.00, 1.00, 0.10]
  r_four = [0.00, 1.00, 1.00, 0.0]
  r_five = [0.00, 0.50, 0.10, 0.00, 0.20, 1.00, 0.30, 0.00, 0.40, 1.00, 0.50, 0.00, 0.60, 1.00, 0.70, 0.50, 1.00, 0.20]
  r_six = [0.00, 0.00, 0.12, 0.70, 0.30, 0.00, 0.70, 1.00, 1.00, 0.50]
  
  one_bird(beg, 3.0, "robin") do
   bigbird(0.45, 0.06, 2000, 800, 0.15, r_six, $main_amp, [1, 1, 2, 0.1])
   bigbird(0.56, 0.10, 2000, 900, 0.15, r_one, $main_amp, [1, 1, 2, 0.1])
   bigbird(1.04, 0.24, 2000, 2000, 0.25, r_two, $main_amp, [1, 1, 2, 0.1])
   bigbird(1.63, 0.13, 1900, 1600, 0.20, r_three, $main_amp, [1, 1, 2, 0.1])
   bigbird(1.80, 0.11, 2200, 1200, 0.25, r_four, $main_amp, [1, 1, 2, 0.1])
   bigbird(2.31, 0.21, 1950, 2000, 0.15, r_five, $main_amp, [1, 1, 2, 0.1])
  end
end


def solitary_vireo(beg) 
  bigskew = [0.00, 0.20, 0.03, 0.30, 0.06, 0.10, 0.10, 0.50, 0.13, 0.40, 0.16, 0.80, 0.19, 0.50, 0.22, 0.90, 0.25, 0.60, 0.28, 1.00, 0.31, 0.60, 0.34, 1.00, 0.37, 0.50, 0.41, 0.90, 0.45, 0.40, 0.49, 0.80, 0.51, 0.40, 0.54, 0.75, 0.57, 0.35, 0.60, 0.70, 0.63, 0.30, 0.66, 0.60, 0.69, 0.25, 0.72, 0.50, 0.75, 0.20, 0.78, 0.30, 0.82, 0.10, 0.85, 0.30, 0.88, 0.05, 0.91, 0.30, 0.94, 0.00, 0.95, 0.30, 0.99, 0.00, 1.00, 0.10]
  one_bird(beg, 1.0, "solitary_vireo") do
   bird(0, 0.4, 1800, 1200, 0.2, bigskew, $main_amp)
  end
end


def pigeon_hawk(beg)
  hupdown = [0.00, 0.00, 0.30, 1.00, 0.70, 1.00, 1.00, 0.0]
  
  one_bird(beg, 2.0, "pigeon_hawk") do
   bigbird(0, 0.1, 1900, 200, 0.2, hupdown, $main_amp, [1, 0.7, 2, 1])
   bigbird(0.12, 0.01, 2050, 0, 0.1, $main_amp, $main_amp, [1, 0.5, 2, 1])
   bigbird(0.13, 0.1, 1900, 200, 0.2, hupdown, $main_amp, [1, 0.7, 2, 1])
   bigbird(0.25, 0.01, 2050, 0, 0.1, $main_amp, $main_amp, [1, 0.5, 2, 1])
   bigbird(0.26, 0.1, 1900, 200, 0.2, hupdown, $main_amp, [1, 0.7, 2, 1])
   bigbird(0.38, 0.01, 2050, 0, 0.1, $main_amp, $main_amp, [1, 0.5, 2, 1])
   bigbird(0.39, 0.1, 1900, 200, 0.2, hupdown, $main_amp, [1, 0.7, 2, 1])
   bigbird(0.51, 0.01, 2050, 0, 0.1, $main_amp, $main_amp, [1, 0.5, 2, 1])
   bigbird(0.52, 0.1, 1900, 200, 0.2, hupdown, $main_amp, [1, 0.7, 2, 1])
   bigbird(0.64, 0.01, 2050, 0, 0.1, $main_amp, $main_amp, [1, 0.5, 2, 1])
   bigbird(0.65, 0.1, 1900, 200, 0.2, hupdown, $main_amp, [1, 0.7, 2, 1])
   bigbird(0.77, 0.01, 2050, 0, 0.1, $main_amp, $main_amp, [1, 0.5, 2, 1])
   bigbird(0.78, 0.1, 1900, 200, 0.2, hupdown, $main_amp, [1, 0.7, 2, 1])
   bigbird(0.90, 0.01, 2050, 0, 0.1, $main_amp, $main_amp, [1, 0.5, 2, 1])
   bigbird(0.91, 0.1, 1900, 200, 0.2, hupdown, $main_amp, [1, 0.7, 2, 1])
   bigbird(1.03, 0.01, 2050, 0, 0.1, $main_amp, $main_amp, [1, 0.5, 2, 1])
   bigbird(1.04, 0.1, 1900, 200, 0.2, hupdown, $main_amp, [1, 0.7, 2, 1])
   bigbird(1.16, 0.01, 2050, 0, 0.1, $main_amp, $main_amp, [1, 0.5, 2, 1])
   bigbird(1.17, 0.1, 1900, 200, 0.2, hupdown, $main_amp, [1, 0.7, 2, 1])
   bigbird(1.29, 0.01, 2050, 0, 0.1, $main_amp, $main_amp, [1, 0.5, 2, 1])
   bigbird(1.30, 0.1, 1900, 200, 0.2, hupdown, $main_amp, [1, 0.7, 2, 1])
   bigbird(1.42, 0.01, 2050, 0, 0.1, $main_amp, $main_amp, [1, 0.5, 2, 1])
   bigbird(1.43, 0.1, 1900, 200, 0.2, hupdown, $main_amp, [1, 0.7, 2, 1])
   bigbird(1.55, 0.01, 2050, 0, 0.1, $main_amp, $main_amp, [1, 0.5, 2, 1])
   bigbird(1.56, 0.1, 1900, 200, 0.2, hupdown, $main_amp, [1, 0.7, 2, 1])
   bigbird(1.68, 0.01, 2050, 0, 0.1, $main_amp, $main_amp, [1, 0.5, 2, 1])
   bigbird(1.69, 0.1, 1900, 200, 0.2, hupdown, $main_amp, [1, 0.7, 2, 1])
   bigbird(1.81, 0.01, 2050, 0, 0.1, $main_amp, $main_amp, [1, 0.5, 2, 1])
   bigbird(1.82, 0.1, 1900, 200, 0.2, hupdown, $main_amp, [1, 0.7, 2, 1])
  end
end


def cerulean_warbler(beg)
  w_down = [0.00, 1.00, 1.00, 0.0]
  trill = [0.00, 0.80, 0.10, 1.00, 0.25, 0.50, 0.40, 1.00, 0.55, 0.50, 0.70, 1.00, 1.00, 0.0]
  w_up = [0.00, 0.00, 1.00, 1.0]
  
  one_bird(beg, 2.0, "cerulean_warbler") do
   bird(0.27, 0.05, 3000, 1000, 0.05, w_down, $main_amp)
   bird(0.33, 0.05, 3000, 800, 0.075, w_up, $main_amp)
   bird(0.41, 0.01, 3200, 700, 0.07, w_down, $main_amp)
   bird(0.42, 0.01, 3200, 700, 0.08, w_down, $main_amp)
   bird(0.43, 0.06, 3200, 700, 0.09, w_down, $main_amp)
   bird(0.51, 0.06, 3200, 500, 0.1, w_up, $main_amp)
   bird(0.6, 0.10, 3000, 1200, 0.2, trill, $main_amp)
   bird(0.72, 0.05, 3000, 800, 0.2, w_up, $main_amp)
   bird(0.8, 0.10, 3000, 1200, 0.2, trill, $main_amp)
   bird(0.92, 0.05, 3000, 800, 0.2, w_up, $main_amp)
   bird(1.00, 0.01, 3900, 600, 0.1, w_up, $main_amp)
   bird(1.01, 0.01, 3910, 800, 0.1, w_up, $main_amp)
   bird(1.02, 0.01, 3940, 500, 0.1, w_up, $main_amp)
   bird(1.03, 0.01, 4000, 500, 0.1, w_up, $main_amp)
   bird(1.04, 0.01, 3900, 1000, 0.1, w_up, $main_amp)
   bird(1.05, 0.01, 3900, 1000, 0.1, w_up, $main_amp)
   bird(1.06, 0.01, 3900, 1000, 0.1, w_up, $main_amp)
   bird(1.07, 0.01, 3900, 1000, 0.1, w_up, $main_amp)
   bird(1.08, 0.01, 3900, 1000, 0.1, w_up, $main_amp)
   bird(1.09, 0.01, 3900, 1000, 0.1, w_up, $main_amp)
   bird(1.10, 0.01, 3900, 1000, 0.1, w_up, $main_amp)
   bird(1.11, 0.01, 3900, 1000, 0.1, w_up, $main_amp)
   bird(1.12, 0.01, 3900, 1000, 0.1, w_up, $main_amp)
   bird(1.13, 0.01, 3900, 1000, 0.1, w_up, $main_amp)
   bird(1.14, 0.01, 3900, 1000, 0.1, w_up, $main_amp)
   bird(1.15, 0.01, 3900, 1000, 0.1, w_up, $main_amp)
   bird(1.16, 0.01, 3900, 1000, 0.1, w_up, $main_amp)
   bird(1.17, 0.01, 3900, 1000, 0.1, w_up, $main_amp)
   bird(1.18, 0.01, 3900, 1000, 0.1, w_up, $main_amp)
   bird(1.19, 0.01, 3900, 1000, 0.1, w_up, $main_amp)
   bird(1.20, 0.01, 3900, 1000, 0.1, w_up, $main_amp)
   bird(1.21, 0.01, 3900, 1000, 0.1, w_up, $main_amp)
   bird(1.22, 0.01, 3900, 1000, 0.1, w_up, $main_amp)
   bird(1.23, 0.01, 3900, 1200, 0.1, w_up, $main_amp)
   bird(1.24, 0.01, 3900, 1200, 0.1, w_up, $main_amp)
   bird(1.25, 0.01, 3900, 1200, 0.1, w_up, $main_amp)
   bird(1.26, 0.01, 3900, 1200, 0.1, w_up, $main_amp)
   bird(1.27, 0.01, 3900, 1400, 0.1, w_up, $main_amp)
   bird(1.28, 0.01, 3900, 1400, 0.1, w_up, $main_amp)
   bird(1.29, 0.01, 3900, 1400, 0.1, w_up, $main_amp)
   bird(1.30, 0.01, 3900, 1400, 0.1, w_up, $main_amp)
  end
end


def nashville_warbler(beg)
  nash_blip = [0.00, 0.60, 0.35, 1.00, 1.00, 0.0]
  nash_down = [0.00, 0.90, 0.05, 1.00, 0.10, 0.90, 0.65, 0.50, 1.00, 0.0]
  nash_up = [0.00, 0.00, 0.15, 0.20, 0.25, 0.05, 0.90, 0.95, 1.00, 1.0]
  nash_amp = [0.00, 0.00, 0.80, 1.00, 1.00, 0.0]
  
  one_bird(beg, 2.0, "nashville_warbler") do
   bird(0.15, 0.025, 3900, 300, 0.3, nash_blip, $main_amp)
   bird(0.24, 0.16, 4200, 3800, 0.15, nash_down, nash_amp)
   bird(0.42, 0.025, 3900, 300, 0.3, nash_blip, $main_amp)
   bird(0.55, 0.14, 4300, 3700, 0.15, nash_down, nash_amp)
   bird(0.75, 0.03, 3950, 350, 0.3, nash_blip, $main_amp)
   bird(0.81, 0.17, 4200, 3900, 0.175, nash_down, $main_amp)
   bird(1.0, 0.02, 3800, 400, 0.25, nash_blip, $main_amp)
   bird(1.11, 0.14, 4200, 3800, 0.165, nash_down, nash_amp)
   bird(1.3, 0.03, 3750, 300, 0.2, nash_blip, $main_amp)
   bird(1.4, 0.11, 4200, 3700, 0.1, nash_down, $main_amp)
   bird(1.57, 0.1, 3800, 2200, 0.1, nash_up, $main_amp)
   bird(1.7, 0.1, 3800, 2150, 0.125, nash_up, $main_amp)
   bird(1.85, 0.075, 3900, 1800, 0.1, nash_up, nash_amp)
  end
end


def eastern_phoebe(beg)
  phoebe_one = [0.00, 0.00, 0.30, 0.30, 0.35, 0.50, 0.55, 0.40, 0.70, 0.80, 0.75, 0.70, 0.80, 1.00, 0.95, 0.90, 1.00, 0.0]
  phoebe_two = [0.00, 0.00, 0.50, 1.00, 1.00, 0.0]
  phoebe_three = [0.00, 0.00, 0.10, 0.40, 0.80, 1.00, 1.00, 0.10]
  phoebe_four = [0.00, 1.00, 0.50, 0.70, 1.00, 0.0]
  phoebe_amp = [0.00, 0.00, 0.10, 1.00, 1.00, 0.0]
  
  one_bird(beg, 1.0, "eastern_phoebe") do
   bird(0, 0.225, 3000, 1300, 0.3, phoebe_one, $main_amp)
   bird(0.35, 0.12, 3000, 500, 0.1, phoebe_two, phoebe_amp)
   bird(0.4, 0.10, 3000, 1500, 0.2, phoebe_three, phoebe_amp)
   bird(0.55, 0.05, 3000, 1400, 0.2, phoebe_four, phoebe_amp)
  end
end


def painted_bunting(beg)
  b_one = [0.00, 0.00, 1.00, 1.0]
  b_two = [0.00, 0.00, 0.90, 1.00, 1.00, 0.0]
  b_three = [0.00, 1.00, 1.00, 0.0]
  b_four = [0.00, 0.00, 0.50, 1.00, 1.00, 0.0]
  b_five = [0.00, 0.70, 0.15, 0.00, 0.40, 1.00, 0.80, 1.00, 1.00, 0.50]
  b_six = [0.00, 0.00, 0.10, 0.50, 0.15, 0.00, 0.40, 1.00, 0.90, 1.00, 1.00, 0.0]
  b_seven = [0.00, 1.00, 0.25, 0.40, 0.75, 0.50, 1.00, 0.0]
  b_eight = [0.00, 0.30, 0.40, 0.40, 0.50, 1.00, 0.60, 0.20, 1.00, 0.0]
  b_nine = [0.00, 0.00, 0.05, 1.00, 0.30, 1.00, 0.50, 0.30, 0.90, 1.00, 1.00, 0.0]
  b_ten = [0.00, 0.40, 0.25, 0.00, 0.35, 1.00, 0.50, 0.00, 0.65, 1.00, 0.75, 0.00, 0.85, 1.00, 1.00, 0.0]
  b_eleven = [0.00, 1.00, 1.00, 0.0]
  b_twelve = [0.00, 0.00, 0.50, 1.00, 1.00, 0.50]
  b_thirteen = [0.00, 0.00, 0.05, 1.00, 0.30, 0.20, 0.60, 0.20, 0.90, 1.00, 1.00, 0.0]
  b_fourteen = [0.00, 0.30, 0.30, 1.00, 0.60, 0.30, 1.00, 0.0]
  b_fifteen = [0.00, 0.00, 0.10, 0.50, 0.50, 0.50, 0.90, 1.00, 1.00, 0.0]
  
  one_bird(beg, 2.0, "painted_bunting") do
   bird(0.05, 0.10, 3100, 900, 0.05, b_one, b_two)
   bird(0.21, 0.07, 4100, 700, 0.15, b_three, $main_amp)
   bird(0.36, 0.12, 3700, 1000, 0.20, b_four, $main_amp)
   bird(0.52, 0.08, 2300, 1600, 0.15, b_five, b_six)
   bird(0.68, 0.1, 4000, 1000, 0.25, b_one, $bird_tap)
   bird(0.8, 0.12, 2300, 1700, 0.2, b_seven, $main_amp)
   bird(0.96, 0.15, 3800, 2200, 0.3, b_eight, b_nine)
   bird(1.18, 0.1, 2300, 1600, 0.15, b_ten, $main_amp)
   bird(1.3, 0.02, 3200, 1000, 0.1, b_eleven, $main_amp)
   bird(1.33, 0.02, 3200, 1000, 0.1, b_eleven, $main_amp)
   bird(1.36, 0.02, 3200, 1000, 0.1, b_eleven, $main_amp)
   bird(1.40, 0.03, 4000, 2000, 0.12, b_twelve, b_thirteen)
   bird(1.47, 0.1, 2300, 1700, 0.2, b_fourteen, b_fifteen)
  end
end


def western_flycatcher(beg)
  f_one = [0.00, 0.00, 0.10, 1.00, 0.20, 0.40, 0.95, 0.10, 1.00, 0.0]
  a_one = [0.00, 0.00, 0.10, 0.20, 0.20, 0.10, 0.30, 1.00, 0.90, 1.00, 1.00, 0.0]
  f_two = [0.00, 0.50, 0.25, 1.00, 0.50, 0.00, 0.60, 0.00, 0.95, 0.30, 1.00, 0.60]
  a_two = [0.00, 0.00, 0.10, 1.00, 0.20, 1.00, 0.50, 0.10, 0.60, 0.10, 0.90, 1.00, 1.00, 0.0]
  
  one_bird(beg, 1.0, "western_flycatcher") do
   bigbird(0, 0.2, 2000, 2200, 0.2, f_one, a_one, [1, 1, 2, 0.02, 3, 0.1, 4, 0.01])
   bigbird(0.3, 0.2, 2000, 1100, 0.2, f_two, a_two, [1, 1, 2, 0.02, 3, 0.1, 4, 0.01])
  end
end


def bachmans_sparrow(beg)
  sopening = [0.00, 1.00, 0.10, 0.50, 0.90, 0.50, 1.00, 0.0]
  sup = [0.00, 0.10, 0.35, 0.00, 1.00, 1.0]
  sdwn = [0.00, 1.00, 0.40, 0.50, 1.00, 0.0]
  supn = [0.00, 0.00, 1.00, 1.0]
  slast = [0.00, 1.00, 0.25, 0.00, 0.75, 0.40, 1.00, 0.50]
  
  one_bird(beg, 5.0, "bachmans_sparrow") do
   bird(0, 0.51, 4900, 200, 0.3, sopening, $main_amp)
   bird(0.52, 0.015, 3800, 200, 0.1, sup, $main_amp)
   bird(0.52, 0.015, 3750, 250, 0.1, sup, $main_amp)
   bird(0.54, 0.015, 3600, 300, 0.1, sup, $main_amp)
   bird(0.56, 0.015, 3500, 250, 0.1, sup, $main_amp)
   bird(0.58, 0.015, 3400, 200, 0.1, sup, $main_amp)
   bird(0.60, 0.015, 3200, 200, 0.1, sup, $main_amp)
   bird(0.62, 0.015, 3800, 100, 0.1, sup, $main_amp)

   bird(0.65, 0.07, 3000, 750, 0.2, sup, $main_amp)
   bird(0.73, 0.03, 5000, 1000, 0.1, sdwn, $main_amp)
   bird(0.80, 0.07, 3000, 750, 0.2, sup, $main_amp)
   bird(0.88, 0.03, 5000, 1000, 0.1, sdwn, $main_amp)
   bird(0.95, 0.07, 3000, 750, 0.2, sup, $main_amp)
   bird(1.03, 0.03, 5000, 1000, 0.1, sdwn, $main_amp)
   bird(1.10, 0.07, 3000, 750, 0.2, sup, $main_amp)
   bird(1.18, 0.03, 5000, 1000, 0.1, sdwn, $main_amp)
   bird(1.25, 0.07, 3000, 750, 0.2, sup, $main_amp)
   bird(1.33, 0.03, 5000, 1000, 0.1, sdwn, $main_amp)
   bird(1.40, 0.07, 3000, 750, 0.2, sup, $main_amp)
   bird(1.48, 0.03, 5000, 1000, 0.1, sdwn, $main_amp)
   bird(1.55, 0.07, 3000, 750, 0.2, sup, $main_amp)
   bird(1.63, 0.03, 5000, 1000, 0.1, sdwn, $main_amp)

   bird(2.8, 0.06, 4000, 1700, 0.1, supn, $main_amp)
   bird(2.87, 0.01, 5200, 0, 0.2, supn, $main_amp)
   bird(2.9, 0.06, 4000, 1700, 0.1, supn, $main_amp)
   bird(2.97, 0.01, 5200, 0, 0.2, supn, $main_amp)
   bird(3.0, 0.06, 4000, 1700, 0.1, supn, $main_amp)
   bird(3.07, 0.01, 5200, 0, 0.2, supn, $main_amp)
   bird(3.1, 0.06, 4000, 1700, 0.1, supn, $main_amp)
   bird(3.17, 0.01, 5200, 0, 0.2, supn, $main_amp)
   bird(3.2, 0.06, 4000, 1700, 0.1, supn, $main_amp)
   bird(3.27, 0.01, 5200, 0, 0.2, supn, $main_amp)

   bird(3.4, 0.15, 3000, 1000, 0.2, slast, $main_amp)
   bird(3.6, 0.15, 3000, 1000, 0.2, slast, $main_amp)
   bird(3.8, 0.15, 3000, 1000, 0.2, slast, $main_amp)
   bird(4.0, 0.15, 3000, 1000, 0.2, slast, $main_amp)
   bird(4.2, 0.15, 3000, 1000, 0.2, slast, $main_amp)
   bird(4.4, 0.15, 3000, 1000, 0.2, slast, $main_amp)
  end
end


def cedar_waxwing(beg)
  cedar = [0.00, 0.00, 0.25, 0.70, 0.70, 1.00, 0.90, 1.00, 1.00, 0.20]
  cedamp = [0.00, 0.00, 0.20, 1.00, 0.40, 1.00, 1.00, 0.0]

  one_bird(beg, 1.0, "cedar_waxwing") do
   bird(0, 0.50, 6000, 800, 0.2, cedar, cedamp)
  end
end


def bairds_sparrow(beg)
  bairdend = [0.00, 0.00, 0.25, 1.00, 0.50, 0.00, 0.75, 1.00, 1.00, 0.0]
  bairdstart = [0.00, 0.50, 0.05, 1.00, 0.10, 0.00, 0.15, 1.00, 0.20, 0.00, 0.25, 1.00, 0.30, 0.00, 0.35, 1.00, 0.40, 0.00, 0.45, 1.00, 0.50, 0.00, 0.55, 1.00, 0.60, 0.00, 0.65, 1.00, 0.70, 0.00, 0.75, 1.00, 0.80, 0.00, 0.85, 1.00, 0.90, 0.00, 0.95, 1.00, 1.00, 0.0]
  
  one_bird(beg, 2.0, "bairds_sparrow") do
   bird(0, 0.09, 6500, 1500, 0.2, bairdstart, $main_amp)
   bird(0.22, 0.01, 5900, 100, 0.2, bairdend, $main_amp)
   bird(0.25, 0.09, 6000, 1000, 0.2, bairdstart, $main_amp)
   bird(0.45, 0.01, 4200, 100, 0.2, bairdend, $main_amp)
   bird(0.50, 0.08, 4200, 600, 0.2, bairdstart, $main_amp)
   bird(0.59, 0.01, 4400, 100, 0.2, bairdend, $main_amp)
   bird(0.60, 0.01, 4400, 100, 0.2, bairdend, $main_amp)
   bird(0.68, 0.07, 5400, 700, 0.2, bairdstart, $main_amp)

   bird(0.75, 0.01, 4200, 100, 0.2, bairdend, $main_amp)
   bird(0.79, 0.01, 4400, 100, 0.2, bairdend, $main_amp)
   bird(0.83, 0.01, 4200, 100, 0.19, bairdend, $main_amp)
   bird(0.87, 0.01, 4400, 100, 0.19, bairdend, $main_amp)
   bird(0.91, 0.01, 4200, 100, 0.18, bairdend, $main_amp)
   bird(0.95, 0.01, 4400, 100, 0.18, bairdend, $main_amp)
   bird(0.99, 0.01, 4200, 100, 0.17, bairdend, $main_amp)
   bird(1.03, 0.01, 4400, 100, 0.17, bairdend, $main_amp)
   bird(1.07, 0.01, 4200, 100, 0.16, bairdend, $main_amp)
   bird(1.11, 0.01, 4400, 100, 0.16, bairdend, $main_amp)
   bird(1.15, 0.01, 4200, 100, 0.15, bairdend, $main_amp)
   bird(1.19, 0.01, 4400, 100, 0.15, bairdend, $main_amp)
   bird(1.23, 0.01, 4200, 100, 0.14, bairdend, $main_amp)
   bird(1.27, 0.01, 4400, 100, 0.14, bairdend, $main_amp)
   bird(1.31, 0.01, 4200, 100, 0.13, bairdend, $main_amp)
   bird(1.35, 0.01, 4400, 100, 0.13, bairdend, $main_amp)
   bird(1.39, 0.01, 4200, 100, 0.12, bairdend, $main_amp)
   bird(1.43, 0.01, 4400, 100, 0.12, bairdend, $main_amp)
   bird(1.47, 0.01, 4200, 100, 0.11, bairdend, $main_amp)
   bird(1.51, 0.01, 4400, 100, 0.11, bairdend, $main_amp)
   bird(1.55, 0.01, 4200, 100, 0.10, bairdend, $main_amp)
   bird(1.59, 0.01, 4400, 100, 0.10, bairdend, $main_amp)
   bird(1.63, 0.01, 4200, 100, 0.09, bairdend, $main_amp)
   bird(1.67, 0.01, 4400, 100, 0.09, bairdend, $main_amp)
   bird(1.71, 0.01, 4200, 100, 0.08, bairdend, $main_amp)
   bird(1.75, 0.01, 4400, 100, 0.08, bairdend, $main_amp)
   bird(1.79, 0.01, 4200, 100, 0.07, bairdend, $main_amp)
   bird(1.83, 0.01, 4400, 100, 0.07, bairdend, $main_amp)
   bird(1.87, 0.01, 4200, 100, 0.06, bairdend, $main_amp)
   bird(1.92, 0.01, 4400, 100, 0.06, bairdend, $main_amp)
   bird(1.97, 0.01, 4200, 100, 0.05, bairdend, $main_amp)
  end
end


def kentucky_warbler(beg)
  kenstart = [0.00, 0.30, 0.50, 1.00, 1.00, 0.0]
  kendwn = [0.00, 0.90, 0.10, 1.00, 1.00, 0.0]
  kenup = [0.00, 0.00, 1.00, 1.0]
  kentrill = [0.00, 1.00, 0.25, 0.00, 0.50, 0.00, 0.75, 1.00, 1.00, 0.0]
  
  one_bird(beg, 3.0, "kentucky_warbler") do
   bigbird(0.6, 0.02, 3800, 200, 0.05, kenstart, $main_amp, [1, 1, 2, 0.03])
   bigbird(0.65, 0.03, 4300, 200, 0.15, kenup, $main_amp, [1, 1, 2, 0.1])
   bigbird(0.73, 0.02, 3200, 100, 0.1, kendwn, $main_amp, [1, 1, 2, 0.1])

   bigbird(0.75, 0.05, 3000, 800, 0.15, kenstart, $main_amp, [1, 1, 2, 0.01])
   bigbird(0.82, 0.06, 3100, 1200, 0.1, kendwn, $main_amp, [1, 1, 2, 0.01])
   bigbird(0.90, 0.06, 3200, 1200, 0.1, kendwn, $main_amp, [1, 1, 2, 0.01])
   bigbird(0.98, 0.05, 4600, 100, 0.2, kentrill, $main_amp, [1, 1, 2, 0.1])

   bigbird(1.10, 0.05, 2900, 800, 0.15, kenstart, $main_amp, [1, 1, 2, 0.01])
   bigbird(1.17, 0.06, 3000, 1200, 0.1, kendwn, $main_amp, [1, 1, 2, 0.01])
   bigbird(1.25, 0.06, 3100, 1200, 0.1, kendwn, $main_amp, [1, 1, 2, 0.01])
   bigbird(1.33, 0.05, 4600, 100, 0.2, kentrill, $main_amp, [1, 1, 2, 0.1])

   bigbird(1.43, 0.05, 2800, 800, 0.15, kenstart, $main_amp, [1, 1, 2, 0.01])
   bigbird(1.50, 0.05, 2700, 1200, 0.1, kendwn, $main_amp, [1, 1, 2, 0.01])
   bigbird(1.57, 0.06, 2800, 1200, 0.1, kendwn, $main_amp, [1, 1, 2, 0.01])
   bigbird(1.64, 0.05, 4600, 100, 0.2, kentrill, $main_amp, [1, 1, 2, 0.1])

   bigbird(1.75, 0.05, 2700, 800, 0.15, kenstart, $main_amp, [1, 1, 2, 0.01])
   bigbird(1.81, 0.05, 2600, 1200, 0.1, kendwn, $main_amp, [1, 1, 2, 0.01])
   bigbird(1.88, 0.06, 2600, 1200, 0.1, kendwn, $main_amp, [1, 1, 2, 0.01])
   bigbird(1.97, 0.05, 4600, 100, 0.2, kentrill, $main_amp, [1, 1, 2, 0.1])

   bigbird(2.05, 0.05, 2700, 800, 0.15, kenstart, $main_amp, [1, 1, 2, 0.01])
   bigbird(2.12, 0.06, 2600, 1200, 0.1, kendwn, $main_amp, [1, 1, 2, 0.01])
   bigbird(2.20, 0.05, 4600, 100, 0.2, kentrill, $main_amp, [1, 1, 2, 0.1])

   bigbird(2.30, 0.05, 2800, 800, 0.15, kenstart, $main_amp, [1, 1, 2, 0.01])
   bigbird(2.37, 0.06, 2700, 1200, 0.1, kendwn, $main_amp, [1, 1, 2, 0.01])
   bigbird(2.45, 0.05, 4700, 100, 0.25, kentrill, $main_amp, [1, 1, 2, 0.1])
  end
end


def rufous_sided_towhee(beg)
  towhee_one = [0.00, 0.10, 0.02, 0.05, 0.04, 0.15, 0.06, 0.05, 0.08, 0.20, 0.10, 0.04, 0.12, 0.25, 0.14, 0.03, 0.16, 0.30, 0.18, 0.02, 0.20, 0.35, 0.22, 0.01, 0.24, 0.40, 0.26, 0.00, 0.28, 0.45, 0.30, 0.00, 0.32, 0.50, 0.34, 0.00, 0.36, 0.50, 0.80, 1.00, 1.00, 0.0]
  towhee_two = [0.00, 0.00, 1.00, 1.0]
  towhee_three = [0.00, 1.00, 1.00, 0.0]
  
  one_bird(beg, 2.0, "rufous_sided_towhee") do
   bigbird(0.25, 0.13, 1400, 1100, 0.2, towhee_one, $main_amp, [1, 0.03, 2, 1, 3, 0.03])
   bigbird(0.45, 0.13, 1400, 1100, 0.2, towhee_one, $main_amp, [1, 0.03, 2, 1, 3, 0.03])
   bigbird(0.60, 0.13, 1400, 1100, 0.2, towhee_one, $main_amp, [1, 0.03, 2, 1, 3, 0.03])
   bigbird(0.75, 0.10, 1400, 1100, 0.2, towhee_one, $main_amp, [1, 0.03, 2, 1, 3, 0.03])

   bird(0.88, 0.01, 5100, 2000, 0.1, towhee_two, $main_amp)
   bird(0.895, 0.01, 5100, 1600, 0.1, towhee_two, $main_amp)
   bird(0.91, 0.01, 5100, 1000, 0.1, towhee_two, $main_amp)
   bird(0.93, 0.01, 3000, 1200, 0.1, towhee_three, $main_amp)

   bird(0.945, 0.01, 5100, 2000, 0.09, towhee_two, $main_amp)
   bird(0.96, 0.01, 5100, 1600, 0.09, towhee_two, $main_amp)
   bird(0.975, 0.01, 5100, 1000, 0.09, towhee_two, $main_amp)
   bird(0.995, 0.01, 3000, 1200, 0.09, towhee_three, $main_amp)

   bird(1.01, 0.01, 5100, 2000, 0.1, towhee_two, $main_amp)
   bird(1.025, 0.01, 5100, 1600, 0.1, towhee_two, $main_amp)
   bird(1.04, 0.01, 5100, 1000, 0.1, towhee_two, $main_amp)
   bird(1.06, 0.01, 3000, 1200, 0.1, towhee_three, $main_amp)

   bird(1.075, 0.01, 5100, 2000, 0.09, towhee_two, $main_amp)
   bird(1.09, 0.01, 5100, 1600, 0.09, towhee_two, $main_amp)
   bird(1.105, 0.01, 5100, 1000, 0.09, towhee_two, $main_amp)
   bird(1.125, 0.01, 3000, 1200, 0.09, towhee_three, $main_amp)

   bird(1.14, 0.01, 5100, 2000, 0.08, towhee_two, $main_amp)
   bird(1.155, 0.01, 5100, 1600, 0.08, towhee_two, $main_amp)
   bird(1.17, 0.01, 5100, 1000, 0.08, towhee_two, $main_amp)
   bird(1.19, 0.01, 3000, 1200, 0.08, towhee_three, $main_amp)

   bird(1.205, 0.01, 5100, 2000, 0.08, towhee_two, $main_amp)
   bird(1.220, 0.01, 5100, 1600, 0.08, towhee_two, $main_amp)
   bird(1.235, 0.01, 5100, 1000, 0.08, towhee_two, $main_amp)
   bird(1.255, 0.01, 3000, 1200, 0.08, towhee_three, $main_amp)

   bird(1.27, 0.01, 5100, 2000, 0.07, towhee_two, $main_amp)
   bird(1.285, 0.01, 5100, 1600, 0.07, towhee_two, $main_amp)
   bird(1.30, 0.01, 5100, 1000, 0.07, towhee_two, $main_amp)
   bird(1.32, 0.01, 3000, 1200, 0.07, towhee_three, $main_amp)

   bird(1.335, 0.01, 5100, 2000, 0.06, towhee_two, $main_amp)
   bird(1.350, 0.01, 5100, 1600, 0.06, towhee_two, $main_amp)
   bird(1.365, 0.01, 5100, 1000, 0.06, towhee_two, $main_amp)
   bird(1.385, 0.01, 3000, 1200, 0.06, towhee_three, $main_amp)

   bird(1.400, 0.01, 5100, 2000, 0.05, towhee_two, $main_amp)
   bird(1.415, 0.01, 5100, 1600, 0.05, towhee_two, $main_amp)
   bird(1.430, 0.01, 5100, 1000, 0.05, towhee_two, $main_amp)
   bird(1.45, 0.01, 3000, 1200, 0.05, towhee_three, $main_amp)

   bird(1.465, 0.01, 5100, 2000, 0.03, towhee_two, $main_amp)
   bird(1.480, 0.01, 5100, 1600, 0.03, towhee_two, $main_amp)
   bird(1.495, 0.01, 5100, 1000, 0.03, towhee_two, $main_amp)
   bird(1.515, 0.01, 3000, 1200, 0.03, towhee_three, $main_amp)
  end
end


def prothonotary_warbler(beg)
  pro_one = [0.00, 0.10, 0.20, 0.00, 1.00, 1.0]
  pro_two = [0.00, 0.00, 1.00, 1.0]
  pro_amp = [0.00, 0.00, 0.20, 1.00, 0.40, 0.50, 1.00, 0.0]
  
  one_bird(beg, 3.0, "prothonotary_warbler") do
   bird(0.76, 0.08, 3000, 3000, 0.05, pro_one, pro_amp)
   bird(0.85, 0.05, 4000, 2500, 0.06, pro_two, $bird_amp)

   bird(1.02, 0.09, 3000, 3000, 0.10, pro_one, pro_amp)
   bird(1.12, 0.05, 4000, 2500, 0.10, pro_two, $bird_amp)

   bird(1.26, 0.08, 3000, 3000, 0.15, pro_one, pro_amp)
   bird(1.35, 0.05, 4000, 2500, 0.16, pro_two, $bird_amp)

   bird(1.54, 0.08, 3000, 3000, 0.20, pro_one, pro_amp)
   bird(1.63, 0.05, 4000, 2500, 0.19, pro_two, $bird_amp)

   bird(1.80, 0.08, 3000, 3000, 0.20, pro_one, pro_amp)
   bird(1.89, 0.05, 4000, 2500, 0.16, pro_two, $bird_amp)

   bird(2.03, 0.08, 3000, 3000, 0.15, pro_one, pro_amp)
   bird(2.12, 0.05, 4000, 2500, 0.10, pro_two, $bird_amp)

   bird(2.30, 0.08, 3000, 3000, 0.10, pro_one, pro_amp)
   bird(2.39, 0.05, 4000, 2500, 0.06, pro_two, $bird_amp)
  end
end


def audubons_warbler(beg)
  w_up = [0.00, 0.00, 1.00, 1.0]
  w_down = [0.00, 1.00, 1.00, 0.0]
  w_end = [0.00, 0.00, 0.15, 1.00, 0.45, 0.90, 0.50, 0.00, 0.55, 1.00, 0.90, 0.90, 1.00, 0.10]
  w_updown = [0.00, 0.10, 0.50, 1.00, 1.00, 0.0]
  
  one_bird(beg, 3.0, "audubons_warbler") do
   bird(0.75, 0.04, 2400, 200, 0.05, w_down, $bird_amp)
   bird(0.83, 0.03, 3200, 200, 0.1, w_up, $bird_amp)
   bird(0.90, 0.04, 2500, 300, 0.15, w_up, $bird_amp)
   bird(0.97, 0.04, 2300, 600, 0.15, w_down, $bird_amp)
   bird(1.02, 0.03, 3500, 400, 0.20, w_up, $bird_amp)
   bird(1.06, 0.04, 2300, 1200, 0.10, w_up, $bird_amp)
   bird(1.13, 0.05, 2300, 1200, 0.15, w_down, $bird_amp)
   bird(1.22, 0.02, 3200, 800, 0.25, w_up, $bird_amp)
   bird(1.25, 0.08, 2400, 600, 0.20, w_updown, $bird_amp)
   bird(1.35, 0.02, 2200, 400, 0.10, w_up, $bird_amp)
   bird(1.38, 0.07, 2400, 1400, 0.15, w_down, $bird_amp)
   bird(1.47, 0.03, 3000, 800, 0.20, w_up, $bird_amp)
   bird(1.50, 0.03, 2500, 400, 0.10, w_updown, $bird_amp)
   bird(1.55, 0.01, 2300, 100, 0.05, w_up, $bird_amp)
   bird(1.56, 0.06, 2200, 1400, 0.15, w_down, $bird_amp)
   bird(1.65, 0.03, 3100, 800, 0.10, w_up, $bird_amp)
   bird(1.70, 0.07, 2800, 800, 0.15, w_updown, $bird_amp)
   bird(1.79, 0.06, 2400, 1000, 0.10, w_down, $bird_amp)
   bird(1.86, 0.14, 3100, 900, 0.25, w_end, $bird_amp)
   bird(2.02, 0.12, 3200, 800, 0.20, w_end, $bird_amp)
  end
end


def lark_bunting(beg)
  b_down = [0.00, 1.00, 1.00, 0.0]
  b_up = [0.00, 0.00, 1.00, 1.0]
  b_trill_one = [0.00, 0.00, 0.06, 0.80, 0.12, 0.00, 0.18, 0.85, 0.24, 0.05, 0.36, 0.90, 0.42, 0.10, 0.48, 0.95, 0.54, 0.20, 0.60, 1.00, 0.66, 0.20, 0.72, 1.00, 0.78, 0.20, 0.84, 1.00, 0.90, 0.20, 1.00, 1.0]
  b_trill_two = [0.00, 0.00, 0.05, 0.80, 0.10, 0.00, 0.15, 0.85, 0.20, 0.00, 0.25, 0.90, 0.30, 0.00, 0.35, 0.95, 0.40, 0.00, 0.45, 1.00, 0.50, 0.00, 0.55, 1.00, 0.60, 0.00, 0.65, 1.00, 0.70, 0.00, 0.75, 1.00, 0.80, 0.00, 0.85, 1.00, 0.90, 0.00, 0.95, 1.00, 1.00, 0.0]
  
  one_bird(beg, 3.0, "lark_bunting") do
   bird(0.1, 0.03, 1800, 100, 0.1, b_up, $bird_amp)
   bird(0.2, 0.12, 3700, 400, 0.2, b_up, $bird_amp)

   bird(0.4, 0.03, 4100, 500, 0.15, b_down, $bird_amp)
   bird(0.45, 0.05, 2000, 400, 0.20, b_down, $bird_amp)
   bird(0.51, 0.03, 1800, 100, 0.1, b_up, $bird_amp)

   bird(0.6, 0.03, 4100, 500, 0.15, b_down, $bird_amp)
   bird(0.65, 0.05, 2000, 400, 0.20, b_down, $bird_amp)
   bird(0.71, 0.03, 1800, 100, 0.1, b_up, $bird_amp)

   bird(0.8, 0.03, 4100, 500, 0.15, b_down, $bird_amp)
   bird(0.85, 0.05, 2000, 400, 0.20, b_down, $bird_amp)
   bird(0.91, 0.03, 1800, 100, 0.1, b_up, $bird_amp)

   bird(1.0, 0.03, 4100, 500, 0.15, b_down, $bird_amp)
   bird(1.05, 0.05, 2000, 400, 0.20, b_down, $bird_amp)
   bird(1.11, 0.03, 1800, 100, 0.1, b_up, $bird_amp)

   bird(1.2, 0.03, 4100, 500, 0.15, b_down, $bird_amp)
   bird(1.25, 0.05, 2000, 400, 0.20, b_down, $bird_amp)
   bird(1.31, 0.03, 1800, 100, 0.1, b_up, $bird_amp)

   bird(1.4, 0.03, 4100, 500, 0.15, b_down, $bird_amp)
   bird(1.45, 0.05, 2000, 400, 0.20, b_down, $bird_amp)
   bird(1.51, 0.03, 1800, 100, 0.1, b_up, $bird_amp)

   bird(1.6, 0.03, 4100, 500, 0.15, b_down, $bird_amp)
   bird(1.65, 0.05, 2000, 400, 0.20, b_down, $bird_amp)
   bird(1.71, 0.03, 1800, 100, 0.1, b_up, $bird_amp)

   bird(1.77, 0.23, 6000, 600, 0.15, b_trill_one, $bird_amp)
   bird(2.005, 0.28, 6000, 600, 0.15, b_trill_two, $bird_amp)
  end
end


def eastern_bluebird(beg)
  blue_one = [0.00, 0.00, 1.00, 1.0]
  blue_two = [0.00, 1.00, 1.00, 0.0]
  blue_three = [0.00, 0.60, 0.10, 1.00, 0.20, 0.00, 0.25, 1.00, 0.30, 0.00, 0.35, 1.00, 0.40, 0.00, 0.45, 1.00, 0.50, 0.00, 0.75, 1.00, 1.00, 0.0]
  blue_four = [0.00, 0.00, 0.50, 1.00, 1.00, 0.0]
  blue_five = [0.00, 0.50, 0.10, 1.00, 0.20, 0.00, 0.35, 1.00, 0.50, 0.00, 0.65, 1.00, 0.80, 0.00, 0.95, 1.00, 1.00, 0.50]
  
  one_bird(beg, 3.0, "eastern_bluebird") do
   bird(0.75, 0.02, 2000, 1600, 0.1, blue_one, $bird_amp)
   bird(0.80, 0.02, 2000, 1600, 0.1, blue_one, $bird_amp)
   bird(0.86, 0.02, 2000, 1600, 0.1, blue_one, $bird_amp)
   bird(1.00, 0.13, 2000, 1400, 0.2, blue_two, $bird_amp)
   bird(1.20, 0.24, 2000, 800, 0.2, blue_three, $bird_amp)
   bird(1.68, 0.03, 2200, 400, 0.1, blue_one, $bird_amp)
   bird(1.72, 0.10, 1950, 100, 0.15, blue_four, $bird_amp)
   bird(1.96, 0.15, 2000, 600, 0.20, blue_five, $bird_amp)
  end
end


def chuck_wills_widow(beg)
  wid_down = [0.00, 1.00, 1.00, 0.0]
  wid_one = [0.00, 0.00, 0.10, 0.10, 0.25, 1.00, 0.50, 0.30, 0.80, 0.70, 1.00, 0.0]
  wid_two = [0.00, 0.20, 0.30, 1.00, 0.50, 0.30, 0.60, 0.70, 0.90, 0.10, 1.00, 0.0]
  
  one_bird(beg, 1.0, "chuck_wills_widow") do
   bird(0.05, 0.03, 1000, 800, 0.1, wid_down, $bird_amp)
   bird(0.32, 0.20, 1000, 1000, 0.2, wid_one, $bird_amp)
   bird(0.56, 0.29, 900, 1100, 0.2, wid_two, $bird_amp)
  end
end


def blue_gray_gnatcatcher(beg)	
  gskw1 = [0.00, 0.00, 0.15, 1.00, 0.75, 0.80, 0.90, 1.00, 1.00, 0.70]
  gskw2 = [0.00, 0.00, 0.25, 1.00, 0.75, 0.70, 1.00, 0.0]
  
  one_bird(beg, 3.0, "blue_gray_gnatcatcher") do
   bigbird(0.5, 0.20, 4000, 1000, 0.2, gskw1, $bird_amp, [1, 0.4, 2, 1, 3, 0.1])
   bigbird(0.8, 0.13, 4000, 800, 0.2, gskw2, $bird_amp, [1, 0.4, 2, 1, 3, 0.2])

   bigbird(1.4, 0.25, 4000, 800, 0.2, gskw2, $bird_amp, [1, 0.4, 2, 1, 3, 0.3])
   bigbird(1.80, 0.17, 4000, 900, 0.2, gskw1, $bird_amp, [1, 0.4, 2, 1, 3, 0.3])
   bigbird(2.00, 0.17, 4000, 700, 0.2, gskw1, $bird_amp, [1, 0.4, 2, 1, 3, 0.3])
   bigbird(2.20, 0.17, 4000, 800, 0.2, gskw2, $bird_amp, [1, 0.4, 2, 1, 3, 0.3])
  end
end


def black_throated_sparrow(beg)
  black_up = [0.00, 0.00, 1.00, 1.0]
  black_down = [0.00, 1.00, 1.00, 0.0]
  black_down_amp = [0.00, 0.00, 0.75, 1.00, 1.00, 0.0]
  black_trill = [0.00, 0.00, 0.03, 0.70, 0.06, 0.00, 0.09, 0.75, 0.12, 0.00, 0.15, 0.80, 0.18, 0.05, 0.21, 0.85, 0.24, 0.10, 0.27, 0.90, 0.30, 0.10, 0.33, 1.00, 0.36, 0.10, 0.39, 1.00, 0.42, 0.10, 0.45, 1.00, 0.48, 0.10, 0.51, 1.00, 0.54, 0.10, 0.57, 1.00, 0.60, 0.10, 0.63, 1.00, 0.66, 0.10, 0.69, 1.00, 0.72, 0.10, 0.75, 1.00, 0.78, 0.10, 0.81, 1.00, 0.84, 0.10, 0.87, 1.00, 0.90, 0.00, 0.93, 0.95, 0.96, 0.00, 1.00, 0.90]
  black_up_down = [0.00, 0.00, 0.50, 1.00, 1.00, 0.20]
  black_amp = [0.00, 0.00, 0.50, 1.00, 1.00, 0.0]
  
  one_bird(beg, 3.0, "black_throated_sparrow") do
   bird(0.8, 0.02, 2200, 1000, 0.1, black_down, $bird_amp)
   bird(0.83, 0.01, 3000, 200, 0.05, black_up, $bird_amp)
   bird(0.96, 0.02, 5800, 500, 0.05, black_up, $bird_amp)
   bird(1.00, 0.02, 4000, 200, 0.05, black_up, $bird_amp)
   bird(1.04, 0.10, 2100, 1700, 0.15, black_down, black_down_amp)
   bird(1.15, 0.05, 5700, 400, 0.25, black_up, $bird_amp)
   bird(1.25, 0.25, 2000, 900, 0.2, black_trill, $bird_amp)
   bird(1.52, 0.05, 5600, 400, 0.15, black_up_down, $bird_amp)

   bird(1.6, 0.04, 3900, 1100, 0.15, black_up, $bird_amp)
   bird(1.66, 0.01, 1900, 100, 0.10, black_up, black_amp)

   bird(1.69, 0.01, 3600, 300, 0.10, black_up, black_amp)
   bird(1.71, 0.03, 3900, 1000, 0.15, black_up, black_amp)
   bird(1.74, 0.02, 5000, 100, 0.20, black_up, black_amp)
   bird(1.76, 0.01, 1900, 100, 0.10, black_up, black_amp)

   bird(1.78, 0.01, 3600, 300, 0.10, black_up, black_amp)
   bird(1.80, 0.03, 3900, 1000, 0.15, black_up, black_amp)
   bird(1.83, 0.02, 5000, 100, 0.20, black_up, black_amp)
   bird(1.85, 0.01, 1900, 100, 0.10, black_up, black_amp)

   bird(1.87, 0.01, 3600, 300, 0.10, black_up, black_amp)
   bird(1.89, 0.03, 3900, 1000, 0.15, black_up, black_amp)
   bird(1.92, 0.02, 5000, 100, 0.20, black_up, black_amp)
   bird(1.94, 0.01, 1900, 100, 0.10, black_up, black_amp)

   bird(1.96, 0.01, 3600, 300, 0.10, black_up, black_amp)
   bird(1.98, 0.03, 3900, 1000, 0.15, black_up, black_amp)
   bird(2.01, 0.02, 5000, 100, 0.20, black_up, black_amp)
   bird(2.03, 0.01, 1900, 100, 0.10, black_up, black_amp)

   bird(2.05, 0.01, 3600, 300, 0.10, black_up, black_amp)
   bird(2.07, 0.03, 3900, 1000, 0.15, black_up, black_amp)
   bird(2.10, 0.02, 5000, 100, 0.20, black_up, black_amp)
   bird(2.13, 0.01, 1900, 100, 0.10, black_up, black_amp)

   bird(2.16, 0.03, 3800, 300, 0.1, black_up, $bird_amp)
  end
end



def black_chinned_sparrow(beg)
  chin_up = [0.00, 0.00, 1.00, 1.0]
  chin_up2 = [0.00, 0.00, 0.30, 0.20, 1.00, 1.0]
  
  one_bird(beg, 3.0, "black_chinned_sparrow") do
   bird(0.6, 0.2, 4200, 100, 0.1, chin_up, $bird_amp)
   bird(1.0, 0.09, 3800, 2000, 0.1, chin_up2, $bird_amp)
   bird(1.25, 0.08, 3900, 1700, 0.12, chin_up2, $bird_amp)
   bird(1.40, 0.08, 3600, 2300, 0.13, chin_up, $bird_amp)
   bird(1.50, 0.11, 3100, 2800, 0.14, chin_up, $bird_amp)
   bird(1.65, 0.07, 2900, 2700, 0.15, chin_up, $bird_amp)
   bird(1.74, 0.07, 2900, 2700, 0.15, chin_up, $bird_amp)
   bird(1.82, 0.07, 3000, 2300, 0.13, chin_up, $bird_amp)
   bird(1.89, 0.07, 3200, 2000, 0.10, chin_up, $bird_amp)
   bird(1.97, 0.05, 3200, 1500, 0.10, chin_up, $bird_amp)
   bird(2.04, 0.04, 3400, 1000, 0.07, chin_up, $bird_amp)
   bird(2.10, 0.03, 3600, 700, 0.05, chin_up, $bird_amp)
   bird(2.15, 0.03, 3800, 300, 0.05, chin_up, $bird_amp)
   bird(2.19, 0.02, 3900, 100, 0.03, chin_up, $bird_amp)
   bird(2.22, 0.01, 3900, 100, 0.01, chin_up, $bird_amp)
   bird(2.24, 0.01, 3900, 100, 0.01, chin_up, $bird_amp)
  end
end


def various_gull_cries_from_end_of_colony_5(beg)
  gullstart = [0, 0, 10, 1, 20, 0.5000, 40, 0.6000, 60, 0.5000, 100, 0]
  gullmiddle = [0, 0, 10, 1, 30, 0.5000, 80, 0.5000, 100, 0]
  gullend = [0, 0, 5, 1, 10, 0.5000, 90, 0.4000, 100, 0]
  unknown = [1, 0.1, 2, 1, 3, 0.1, 4, 0.01, 5, 0.09, 6, 0.01, 7, 0.01]

  one_bird(beg, 10.0, "gulls") do
   bigbird(0.250, 0.80, 1180, 1180, 0.08, gullend, $bird_amp, unknown)
   bigbird(1.500, 0.90, 1180, 1180, 0.07, gullend, $bird_amp, unknown)
   bigbird(2.750, 1.00, 1050, 1050, 0.08, gullend, $bird_amp, unknown)
   bigbird(4.800, 0.05, 1180, 1180, 0.06, gullstart, $bird_amp, unknown)
   bigbird(4.950, 0.10, 1180, 1180, 0.08, gullstart, $bird_amp, unknown)
   bigbird(5.150, 0.10, 1180, 1180, 0.09, gullstart, $bird_amp, unknown)
   bigbird(5.350, 0.10, 1180, 1180, 0.1, gullmiddle, $bird_amp, unknown)
   bigbird(5.450, 0.40, 1050, 1050, 0.1, gullend, $bird_amp, unknown)
   bigbird(6.250, 0.80, 1050, 1050, 0.1, gullend, $bird_amp, unknown)
   bigbird(7.450, 1.80, 1050, 1050, 0.1, gullend, $bird_amp, unknown)
  end
end


def make_birds
#  atime = Time.new
  $out_file = new_sound("test.snd")
  set_squelch_update(true, $out_file, 0)
  orchard_oriole(0)
  cassins_kingbird(3)
  chipping_sparrow(6)
  bobwhite(9)
  western_meadowlark(12)
  scissor_tailed_flycatcher(15)
  great_horned_owl(18)
  black_throated_gray_warbler(21)
  yellow_warbler(24)
  black_necked_stilt(27)
  chestnut_sided_warbler(30)
  grasshopper_sparrow(33)
  swamp_sparrow(36)
  golden_crowned_sparrow(39)
  indigo_bunting(42)
  hooded_warbler(45)
  american_widgeon(48)
  louisiana_waterthrush(51)
  robin(54)
  solitary_vireo(57)
  pigeon_hawk(61)
  cerulean_warbler(64)
  nashville_warbler(67)
  eastern_phoebe(70)
  painted_bunting(73)
  western_flycatcher(76)
  bachmans_sparrow(79)
  cedar_waxwing(82)
  bairds_sparrow(85)
  kentucky_warbler(88)
  rufous_sided_towhee(91)
  prothonotary_warbler(94)
  audubons_warbler(97)
  lark_bunting(100)
  eastern_bluebird(103)
  chuck_wills_widow(106)
  blue_gray_gnatcatcher(109)
  black_throated_sparrow(112)
  black_chinned_sparrow(115)
  set_squelch_update(false, $out_file, 0)
  various_gull_cries_from_end_of_colony_5(118)
  $out_data = false
#  btime = Time.new
#  $stderr.write sprintf("time: %f\n", btime - atime)
end

