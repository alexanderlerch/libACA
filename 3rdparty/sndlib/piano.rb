# piano.rb -- translation of piano.scm -*- snd-ruby -*-
# ;;; CLM piano.ins (Scott Van Duyne) translated to Snd/Scheme

# Ruby Translator: Michael Scholz <mi-scholz@users.sourceforge.net>
# Created: Thu Mar 06 03:58:02 CET 2003
# Changed: Mon Nov 22 13:25:33 CET 2010

module Piano
  require "ws"
  require "env"
  include Math

  Number_of_stiffness_allpasses = 8
  Longitudinal_mode_cutoff_keynum = 29
  Longitudinal_mode_stiffness_coefficient = -0.5
  Golden_mean = 0.618
  Loop_gain_env_t60 = 0.05
  Loop_gain_default = 0.9999
  Nstrings = 3

  # ;;keyNum indexed parameter tables
  # ;;these should all be &key variable defaults for p instrument
  Default_loudPole_table = [36, 0.8, 60, 0.85, 84, 0.7, 96, 0.6, 108, 0.5]
  Default_softPole_table = [36, 0.93, 60, 0.9, 84, 0.9, 96, 0.8, 108, 0.8]
  Default_loudGain_table  = [21.0, 0.7, 36.0, 0.7, 48.0, 0.7, 60.0, 0.65, 72.0, 0.65, 84.0,
                             0.65, 87.006, 0.681, 88.07, 0.444, 90.653, 0.606, 95.515, 0.731,
                             99.77, 0.775, 101.897, 0.794, 104.024, 0.8, 105.695, 0.806]
  Default_softGain_table = [21, 0.25, 108, 0.25]
  Default_strikePosition_table = [21.0, 0.14, 23.884, 0.139, 36.0, 0.128, 56.756, 0.129,
                                  57.765, 0.13, 59.0, 0.13, 60.0, 0.128, 61.0, 0.128, 62.0,
                                  0.129, 66.128, 0.129, 69.0, 0.128, 72.0, 0.128, 73.0,
                                  0.128, 79.0, 0.128, 80.0, 0.128, 96.0, 0.128, 99.0, 0.128]
  Default_detuning2_table = [22.017, -0.09, 23.744, -0.09, 36.0, -0.08, 48.055, -0.113,
                             60.0, -0.135, 67.264, -0.16, 72.0, -0.2, 84.054, -0.301,
                             96.148, -0.383, 108, -0.383]
  Default_detuning3_table = [21.435, 0.027, 23.317, 0.043, 36.0, 0.03, 48.0, 0.03, 60.0,
                             0.03, 72.0, 0.02, 83.984, 0.034, 96.0, 0.034, 99.766, 0.034]
  Default_stiffnessCoefficient_table = [21.0, -0.92, 24.0, -0.9, 36.0, -0.7, 48.0, -0.25,
                                        60.0, -0.1, 75.179, -0.04, 82.986, -0.04, 92.24,
                                        -0.04, 96.0, -0.04, 99.0, 0.2, 108.0, 0.5]
  Default_singleStringDecayRate_table = [21.678, -2.895, 24.0, -3.0, 36.0, -4.641, 41.953, -5.867,
                                         48.173, -7.113, 53.818, -8.016, 59.693, -8.875, 66.605,
                                         -9.434, 73.056, -10.035, 78.931, -10.293, 84.0, -12.185]
  Default_singleStringZero_table = [21.0, -0.3, 24.466, -0.117, 28.763, -0.047, 36.0, -0.03, 48.0,
                                    -0.02, 60.0, -0.01, 72.0, -0.01, 84.0, -0.01, 96.0, -0.01]
  Default_singleStringPole_table = [21.0, 0.0, 24.466, 0.0, 28.763, 0.0, 36.0, 0.0, 108, 0.0]
  Default_releaseLoopGain_table = [21.643, 0.739, 24.0, 0.8, 36.0, 0.88, 48.0, 0.91, 60.0, 0.94,
                                   72.0, 0.965, 84.0, 0.987, 88.99, 0.987, 89.0, 1.0, 108.0, 1.0]
  Default_dryTapFiltCoeft60_table = [36, 0.35, 60, 0.25, 108, 0.15]
  Default_dryTapFiltCoefTarget_table = [36, -0.8, 60, -0.5, 84, -0.4, 108, -0.1]
  Default_dryTapFiltCoefCurrent_table = [0, 0, 200, 0]
  Default_dryTapAmpt60_table = [36, 0.55, 60, 0.5, 108, 0.45]
  Default_sustainPedalLevel_table = [21.0, 0.25, 24.0, 0.25, 36.0, 0.2, 48.0, 0.125, 60.0,
                                     0.075, 72.0, 0.05, 84.0, 0.03, 96.0, 0.01, 99.0, 0.01]
  Default_pedalResonancePole_table = [20.841, 0.534, 21.794, 0.518, 33.222, 0.386, 45.127,
                                      0.148, 55.445, -0.065, 69.255, -0.409, 82.905, -0.729,
                                      95.763, -0.869, 106.398, -0.861]
  Default_pedalEnvelopet60_table = [21.0, 7.5, 108.0, 7.5]
  Default_soundboardCutofft60_table = [21.0, 0.25, 108.0, 0.25]
  Default_dryPedalResonanceFactor_table = [21.0, 0.5, 108.0, 0.5]
  Default_unaCordaGain_table = [21, 1.0, 24, 0.4, 29, 0.1, 29.1, 0.95, 108, 0.95]

  def p(start, *args)
    duration = get_args(args, :duration, 1.0)
    keyNum = get_args(args, :keyNum, 60.0)
    strike_velocity = get_args(args, :strike_velocity, 0.5)
    pedal_down = get_args(args, :pedal_down, false)
    release_time_margin = get_args(args, :release_time_margin, 0.75)
    amp = get_args(args, :amp, 0.5)
    detuningFactor = get_args(args, :detuningFactor, 1.0)
    detuningFactor_table = get_args(args, :detuningFactor_table, [])
    stiffnessFactor = get_args(args, :stiffnessFactor, 1.0)
    stiffnessFactor_table = get_args(args, :stiffnessFactor_table, [])
    pedalPresenceFactor = get_args(args, :pedalPresenceFactor, 0.3)
    longitudinalMode = get_args(args, :longitudinalMode, 10.5)
    strikePositionInvFac = get_args(args, :strikePositionInvFac, -0.9)
    singleStringDecayRateFactor = get_args(args, :singleStringDecayRateFactor, 1.0)
    loudPole = get_args(args, :loudPole, nil)
    loudPole_table = get_args(args, :loudPole_table, Default_loudPole_table)
    softPole = get_args(args, :softPole, nil)
    softPole_table = get_args(args, :softPole_table, Default_softPole_table)
    loudGain = get_args(args, :loudGain, nil)
    loudGain_table = get_args(args, :loudGain_table, Default_loudGain_table)
    softGain = get_args(args, :softGain, nil)
    softGain_table = get_args(args, :softGain_table, Default_softGain_table)
    strikePosition = get_args(args, :strikePosition, nil)
    strikePosition_table = get_args(args, :strikePosition_table, Default_strikePosition_table)
    detuning2 = get_args(args, :detuning2, nil)
    detuning2_table = get_args(args, :detuning2_table, Default_detuning2_table)
    detuning3 = get_args(args, :detuning3, nil)
    detuning3_table = get_args(args, :detuning3_table, Default_detuning3_table)
    stiffnessCoefficient = get_args(args, :stiffnessCoefficient, nil)
    stiffnessCoefficient_table = get_args(args, :stiffnessCoefficient_table,
                                          Default_stiffnessCoefficient_table)
    singleStringDecayRate = get_args(args, :singleStringDecayRate, nil)
    singleStringDecayRate_table = get_args(args, :singleStringDecayRate_table,
                                           Default_singleStringDecayRate_table)
    singleStringZero = get_args(args, :singleStringZero, nil)
    singleStringZero_table = get_args(args, :singleStringZero_table,
                                      Default_singleStringZero_table)
    singleStringPole = get_args(args, :singleStringPole, nil)
    singleStringPole_table = get_args(args, :singleStringPole_table,
                                      Default_singleStringPole_table)
    releaseLoopGain = get_args(args, :releaseLoopGain, nil)
    releaseLoopGain_table = get_args(args, :releaseLoopGain_table, Default_releaseLoopGain_table)
    dryTapFiltCoeft60 = get_args(args, :dryTapFiltCoeft60, nil)
    dryTapFiltCoeft60_table = get_args(args, :dryTapFiltCoeft60_table,
                                       Default_dryTapFiltCoeft60_table)
    dryTapFiltCoefTarget = get_args(args, :dryTapFiltCoefTarget, nil)
    dryTapFiltCoefTarget_table = get_args(args, :dryTapFiltCoefTarget_table,
                                          Default_dryTapFiltCoefTarget_table)
    dryTapFiltCoefCurrent = get_args(args, :dryTapFiltCoefCurrent, nil)
    dryTapFiltCoefCurrent_table = get_args(args, :dryTapFiltCoefCurrent_table,
                                           Default_dryTapFiltCoefCurrent_table)
    dryTapAmpt60 = get_args(args, :dryTapAmpt60, nil)
    dryTapAmpt60_table = get_args(args, :dryTapAmpt60_table, Default_dryTapAmpt60_table)
    sustainPedalLevel = get_args(args, :sustainPedalLevel, nil)
    sustainPedalLevel_table = get_args(args, :sustainPedalLevel_table,
                                       Default_sustainPedalLevel_table)
    pedalResonancePole = get_args(args, :pedalResonancePole, nil)
    pedalResonancePole_table = get_args(args, :pedalResonancePole_table,
                                        Default_pedalResonancePole_table)
    pedalEnvelopet60 = get_args(args, :pedalEnvelopet60, nil)
    pedalEnvelopet60_table = get_args(args, :pedalEnvelopet60_table,
                                      Default_pedalEnvelopet60_table)
    soundboardCutofft60 = get_args(args, :soundboardCutofft60, nil)
    soundboardCutofft60_table = get_args(args, :soundboardCutofft60_table,
                                         Default_soundboardCutofft60_table)
    dryPedalResonanceFactor = get_args(args, :dryPedalResonanceFactor, nil)
    dryPedalResonanceFactor_table = get_args(args, :dryPedalResonanceFactor_table,
                                             Default_dryPedalResonanceFactor_table)
    unaCordaGain = get_args(args, :unaCordaGain, nil)
    unaCordaGain_table = get_args(args, :unaCordaGain_table, Default_unaCordaGain_table)
    
    dur = seconds2samples(duration)
    freq = 440.0 * (2.0 ** ((keyNum - 69.0) / 12.0))
    wT = (TWO_PI * freq) / mus_srate
    
    # ;;look_up parameters in tables (or else use the override value)
    loudPole = (loudPole or envelope_interp(keyNum, loudPole_table))
    softPole = (softPole or envelope_interp(keyNum, softPole_table))
    loudGain = (loudGain or envelope_interp(keyNum, loudGain_table))
    softGain = (softGain or envelope_interp(keyNum, softGain_table))
    strikePosition = (strikePosition or envelope_interp(keyNum, strikePosition_table))
    detuning2 = (detuning2 or envelope_interp(keyNum, detuning2_table))
    detuning3 = (detuning3 or envelope_interp(keyNum, detuning3_table))
    stiffnessCoefficient = (stiffnessCoefficient or
                            envelope_interp(keyNum, stiffnessCoefficient_table))
    singleStringDecayRate = (singleStringDecayRate or
                             envelope_interp(keyNum, singleStringDecayRate_table))
    singleStringDecayRate = singleStringDecayRateFactor * singleStringDecayRate
    singleStringZero = (singleStringZero or envelope_interp(keyNum, singleStringZero_table))
    singleStringPole = (singleStringPole or envelope_interp(keyNum, singleStringPole_table))
    releaseLoopGain = (releaseLoopGain or envelope_interp(keyNum, releaseLoopGain_table))
    dryTapFiltCoeft60 = (dryTapFiltCoeft60 or
                         envelope_interp(keyNum, dryTapFiltCoeft60_table))
    dryTapFiltCoefTarget = (dryTapFiltCoefTarget or
                            envelope_interp(keyNum, dryTapFiltCoefTarget_table))
    dryTapFiltCoefCurrent = (dryTapFiltCoefCurrent or
                             envelope_interp(keyNum, dryTapFiltCoefCurrent_table))
    dryTapAmpt60 = (dryTapAmpt60 or envelope_interp(keyNum, dryTapAmpt60_table))
    sustainPedalLevel = (sustainPedalLevel or envelope_interp(keyNum, sustainPedalLevel_table))
    pedalResonancePole = (pedalResonancePole or envelope_interp(keyNum, pedalResonancePole_table))
    pedalEnvelopet60 = (pedalEnvelopet60 or envelope_interp(keyNum, pedalEnvelopet60_table))
    soundboardCutofft60 = (soundboardCutofft60 or
                           envelope_interp(keyNum, soundboardCutofft60_table))
    dryPedalResonanceFactor = (dryPedalResonanceFactor or
                               envelope_interp(keyNum, dryPedalResonanceFactor_table))
    unaCordaGain = (unaCordaGain or envelope_interp(keyNum, unaCordaGain_table))
    detuningFactor = if detuningFactor_table.empty?
                       envelope_interp(keyNum, detuningFactor_table)
                     else
                       detuningFactor
                     end
    stiffnessFactor = if stiffnessFactor_table.empty?
                        envelope_interp(keyNum, stiffnessFactor_table)
                      else
                        stiffnessFactor
                      end

    # ;;initialize soundboard impulse response elements
    dryTap_one_pole_one_zero_pair = make_one_pole_one_zero(1.0, 0.0, 0.0)
    dryTap0 = dryTap_one_pole_one_zero_pair[0]
    dryTap1 = dryTap_one_pole_one_zero_pair[1]
	 
    dryTap_coef_expseg = make_expseg(dryTapFiltCoefCurrent, dryTapFiltCoefTarget)
    drycoefrate = in_t60(dryTapFiltCoeft60)
    dryTap_one_pole_swept = make_one_pole_swept()
    dryTap_amp_expseg = make_expseg(1.0, 0.0)
    dryamprate = in_t60(dryTapAmpt60)
	 
    # ;;initialize open_string resonance elements		
    wetTap_one_pole_one_zero_pair =
    make_one_pole_one_zero(1.0 - signum(pedalResonancePole) * pedalResonancePole,
                           0.0, -pedalResonancePole)
    wetTap0 = wetTap_one_pole_one_zero_pair[0]
    wetTap1 = wetTap_one_pole_one_zero_pair[1]
	 
    wetTap_coef_expseg = make_expseg(0.0, -0.5)
    wetcoefrate = in_t60(pedalEnvelopet60)
    wetTap_one_pole_swept = make_one_pole_swept()
    wetTap_amp_expseg = make_expseg(sustainPedalLevel * pedalPresenceFactor *
                                                       (pedal_down ? 1.0 : dryPedalResonanceFactor),
                                    0.0)
    wetamprate = in_t60(pedalEnvelopet60)
    sb_cutoff_rate = in_t60(soundboardCutofft60)
	 
    # ;;initialize velocity_dependent piano hammer filter elements
    hammerPole = softPole + (loudPole - softPole) * strike_velocity
    hammerGain = softGain + (loudGain - softGain) * strike_velocity
    hammer_one_pole = Array.new(4)
	 
    # ;;strike position comb filter delay length
    agraffe_len = (mus_srate * strikePosition) / freq

    0.upto(3) do |i|
      hammer_one_pole[i] = make_one_pole(1.0 * (1.0 - hammerPole), -hammerPole)
    end

    vals = apfloor(agraffe_len, wT)
    dlen1 = vals[0]
    apcoef1 = vals[1]
    agraffe_delay1 = make_delay0(dlen1)
    agraffe_tuning_ap1 = make_one_pole_allpass(apcoef1)
	     
    # ;;compute coefficients for and initialize the coupling filter
    # ;;taking L=g(1 - bz^-1)/(1-b), and computing Hb = -(1-L)/(2-L)
    attenuationPerPeriod = 10.0 ** (singleStringDecayRate / freq / 20.0)
    g = attenuationPerPeriod    # ;;DC gain
    b = singleStringZero
    a = singleStringPole
    ctemp = 1 + -b + g + -(a * g) + Nstrings * (1 + -b + -g + a * g)

    cfb0 = (2 * (-1 + b + g + -(a * g))) / ctemp
    cfb1 = (2 * (a + -(a * b) + -(b * g) + a * b * g)) / ctemp
    cfa1 = (-a + a * b + -(b * g) + a * b * g + Nstrings * (-a + a * b + b * g + -(a * b * g))) /
           ctemp
    couplingFilter_pair = make_one_pole_one_zero(cfb0, cfb1, cfa1)
    cou0 = couplingFilter_pair[0]
    cou1 = couplingFilter_pair[1]
	     
    # ;;determine string tunings (and longitudinal modes, if present)
    freq1 = if keyNum <= Longitudinal_mode_cutoff_keynum
              freq * longitudinalMode
            else
              freq
            end
    freq2 = freq + detuning2 * detuningFactor
    freq3 = freq + detuning3 * detuningFactor

    # ;;scale stiffness coefficients, if desired
    stiffnessCoefficient = if stiffnessFactor > 1.0
                             stiffnessCoefficient - ((stiffnessCoefficient + 1) *
                                                     (stiffnessFactor - 1))
                           else
                             stiffnessCoefficient * stiffnessFactor
                           end
    stiffnessCoefficientL = if keyNum <= Longitudinal_mode_cutoff_keynum
                              Longitudinal_mode_stiffness_coefficient
                            else
                              stiffnessCoefficient
                            end
    # ;;initialize the coupled_string elements
    vals1 = tune_piano(freq1, stiffnessCoefficientL, Number_of_stiffness_allpasses,
                       cfb0, cfb1, cfa1)
    delayLength1 = vals1[0]
    tuningCoefficient1 = vals1[1]
    vals2 = tune_piano(freq2, stiffnessCoefficient, Number_of_stiffness_allpasses,
                       cfb0, cfb1, cfa1)
    delayLength2 = vals2[0]
    tuningCoefficient2 = vals2[1]
    vals3 = tune_piano(freq3, stiffnessCoefficient, Number_of_stiffness_allpasses,
                       cfb0, cfb1, cfa1)
    delayLength3 = vals3[0]
    tuningCoefficient3 = vals3[1]
    string1_delay = make_delay0(delayLength1 - 1)
    string1_tuning_ap = make_one_pole_allpass(tuningCoefficient1)
    string1_stiffness_ap = Array.new(8)
    string2_delay = make_delay0(delayLength2 - 1)
    string2_tuning_ap = make_one_pole_allpass(tuningCoefficient2)
    string2_stiffness_ap = Array.new(8)
    string3_delay = make_delay0(delayLength3 - 1)
    string3_tuning_ap = make_one_pole_allpass(tuningCoefficient3)
    string3_stiffness_ap = Array.new(8)
    # ;;initialize loop_gain envelope
    loop_gain_expseg = make_expseg(Loop_gain_default, releaseLoopGain)
    looprate = in_t60(Loop_gain_env_t60)
    adelOut = 0.0
    loop_gain = Loop_gain_default
    is_release_time = false
    string1_junction_input = 0.0
    string2_junction_input = 0.0
    string3_junction_input = 0.0
    couplingFilter_output = 0.0
    sampCount = 0
    noi = make_noise()

    0.upto(7) do |i|
      string1_stiffness_ap[i] = make_one_pole_allpass(stiffnessCoefficientL)
    end
    0.upto(7) do |i|
      string2_stiffness_ap[i] = make_one_pole_allpass(stiffnessCoefficient)
    end
    0.upto(7) do |i|
      string3_stiffness_ap[i] = make_one_pole_allpass(stiffnessCoefficient)
    end

    run_instrument(start, duration + release_time_margin) do
      if is_release_time
        loop_gain = loop_gain_expseg.call(looprate)
      elsif sampCount == dur
        is_release_time = true
        dryamprate = sb_cutoff_rate
        wetamprate = sb_cutoff_rate
      end
      dryTap = (dryTap_amp_expseg.call(dryamprate) * \
                dryTap_one_pole_swept.call(one_pole_one_zero(dryTap0, dryTap1, noi.call(amp)),
                                           dryTap_coef_expseg.call(drycoefrate)))
      
      openStrings = (wetTap_amp_expseg.call(wetamprate) * \
                     wetTap_one_pole_swept.call(one_pole_one_zero(wetTap0, wetTap1, noi.call(amp)),
                                                wetTap_coef_expseg.call(wetcoefrate)))
      adelIn = dryTap + openStrings
      0.upto(3) do |i| adelIn = one_pole(hammer_one_pole[i], adelIn) end
      combedExcitationSignal = hammerGain * (adelOut + adelIn * strikePositionInvFac)
      adelOut = agraffe_tuning_ap1.call(delay0(agraffe_delay1, adelIn))
      string1_junction_input += couplingFilter_output
      0.upto(7) do |i|
        string1_junction_input = string1_stiffness_ap[i].call(string1_junction_input)
      end
      string1_junction_input = (unaCordaGain * combedExcitationSignal + \
                                loop_gain * delay0(string1_delay,
                                                   string1_tuning_ap.call(string1_junction_input)))
      string2_junction_input += couplingFilter_output
      0.upto(7) do |i|
        string2_junction_input = string2_stiffness_ap[i].call(string2_junction_input)
      end
      string2_junction_input = (combedExcitationSignal + \
                                loop_gain * delay0(string2_delay,
                                                   string2_tuning_ap.call(string2_junction_input)))
      string3_junction_input += couplingFilter_output
      0.upto(7) do |i|
        string3_junction_input = string3_stiffness_ap[i].call(string3_junction_input)
      end
      string3_junction_input = (combedExcitationSignal + \
                                loop_gain * delay0(string3_delay,
                                                   string3_tuning_ap.call(string3_junction_input)))
      couplingFilter_input = string1_junction_input + string2_junction_input +
                    string3_junction_input
      couplingFilter_output = one_pole_one_zero(cou0, cou1, couplingFilter_input)
      sampCount += 1
      couplingFilter_input
    end
  end

  # ;;; converts t60 values to suitable :rate values for expseg
  def in_t60(t60)
    1.0 - (0.001 ** (1.0 / t60 / mus_srate))
  end

  # ;;; expseg (like musickit asymp)
  def make_expseg(cv = 0.0, tv = 0.0)
    lambda do |r|
      old_cv = cv
      cv = cv + (tv - cv) * r
      old_cv     # ; (bil) this is slightly different (getting clicks)
    end            
  end

  # ;;; signal controlled one-pole lowpass filter
  def make_one_pole_swept
    y1 = 0.0
    lambda do |input, coef|
      y1 = (coef + 1) * input - coef * y1
    end
  end

  # ;;; one-pole allpass filter
  def make_one_pole_allpass(coeff)
    coef = coeff
    x1 = 0.0
    y1 = 0.0
    lambda do |input|
      y1 = (coef * (input - y1)) + x1
      x1 = input
      y1
    end
  end

  def one_pole_one_zero(f0, f1, input)
    one_zero(f0, one_pole(f1, input))
  end

  def make_one_pole_one_zero(a0, a1, b1)
    [make_one_zero(a0, a1), make_one_pole(1.0, b1)]
  end

  # ;;; very special noise generator
  def make_noise
    noise_seed = 16383
    lambda do |amp|
      noise_seed = (noise_seed * 1103515245 + 12345) & 0xffffffff
      # ;; (bil) added the logand -- otherwise we get an overflow somewhere
      amp * (((noise_seed / 65536).round % 65536) * 0.0000305185 - 1.0)
    end
  end

  # ;;; delay line unit generator with length 0 capabilities...
  def make_delay0(len)
    len > 0 ? make_delay(len) : false
  end

  def delay0(f, input)
    f ? delay(f, input) : input
  end

  def ap_phase(a1, wT)
    atan2((a1 * a1 - 1.0) * sin(wT), 2.0 * a1 + (a1 * a1 + 1.0) * cos(wT))
  end

  def opoz_phase(b0, b1, a1, wT)
    s = sin(wT)
    c = cos(wT)
    atan2(a1 * s * (b0 + b1 * c) - b1 * s * (1 + a1 * c),
          (b0 + b1 * c) * (1 + a1 * c) + b1 * s * a1 * s)
  end

  def get_allpass_coef(samp_frac, wT)
    ta = tan(-(samp_frac * wT))
    c = cos(wT)
    s = sin(wT)
    (-ta + signum(ta) * sqrt((ta * ta + 1) * s * s)) / (c * ta - s)
  end

  def signum(x)
    if x == 0.0
      0
    elsif x < 0.0
      -1
    else
      1
    end
  end

  def apfloor(len, wT)
    len_int = len.floor.round
    len_frac = len - len_int
    if len_frac < Golden_mean
      len_int -= 1
      len_frac += 1.0
    end
    if len_frac < Golden_mean and len_int > 0
      len_int -= 1
      len_frac += 1.0
    end
    [len_int, get_allpass_coef(len_frac, wT)]
  end

  def tune_piano(frequency, stiffnessCoefficient, numAllpasses, b0, b1, a1)
    wT = (frequency * TWO_PI) / mus_srate
    len = (TWO_PI + (numAllpasses * ap_phase(stiffnessCoefficient, wT)) +
           opoz_phase(1 + 3 * b0, a1 + 3 * b1, a1, wT)) / wT
    apfloor(len, wT)
  end
end

include Piano

=begin
with_sound(:clm, false, :channels, 1) do
  7.times do |i|
    p(i * 0.5,
      :duration, 0.5,
      :keyNum, 24 + 12.0 * i,
      :strike_velocity, 0.5,
      :amp, 0.4,
      :dryPedalResonanceFactor, 0.25)
  end
end

with_sound(:clm, false, :channels, 1) do
  7.times do |i|
    p(i * 0.5,
      :duration, 0.5,
      :keyNum, 24 + 12.0 * i,
      :strike_velocity, 0.5,
      :amp, 0.4,
      :dryPedalResonanceFactor, 0.25,
      :detuningFactor_table, [24, 5, 36, 7.0, 48, 7.5, 60, 12.0, 72, 20,
                              84, 30, 96, 100, 108, 300],
      :stiffnessFactor_table, [21, 1.5, 24, 1.5, 36, 1.5, 48, 1.5, 60, 1.4,
                               72, 1.3, 84, 1.2, 96, 1.0, 108, 1.0])
  end
end

with_sound(:clm, false, :channels, 1) do
  7.times do |i|
    p(i * 0.5,
      :duration, 0.5,
      :keyNum, 24 + 12.0 * i,
      :strike_velocity, 0.5,
      :amp, 0.4,
      :dryPedalResonanceFactor, 0.25,
      :singleStringDecayRate_table, [21, -5, 24.0, -5.0, 36.0, -5.4, 41.953, -5.867, 48.173,
                                     -7.113, 53.818, -8.016, 59.693, -8.875, 66.605, -9.434,
                                     73.056, -10.035, 78.931, -10.293, 84.000, -12.185],
      :singleStringPole_table, [21, 0.8, 24, 0.7, 36.0, 0.6, 48, 0.5, 60,
                                0.3, 84, 0.1, 96, 0.03, 108, 0.03],
      :stiffnessCoefficient_table, [21.0, -0.92, 24.0, -0.9, 36.0, -0.7, 48.0, -0.250, 60.0,
                                    -0.1, 75.179, -0.040, 82.986, -0.040, 92.240, 0.3, 96.0,
                                    0.5, 99.0, 0.7, 108.0, 0.7])
  end
end

with_sound(:clm, false, :channels, 1) do
  p(0,
    :duration, 5,
    :keyNum, 24 + 12.0 * 5,
    :strike_velocity, 0.5,
    :amp, 0.4,
    :dryPedalResonanceFactor, 0.25,
    :singleStringDecayRateFactor, 1 / 10.0)
end
=end

# piano.rb ends here
