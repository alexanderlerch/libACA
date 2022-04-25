# prc95.rb -- Translation of prc95.scm/prc-toolkit95.lisp to Snd/Ruby
# Perry Cook's Physical Modelling Toolkit

# Translator/Author: Michael Scholz <mi-scholz@users.sourceforge.net>
# Changed: Mon Nov 22 13:28:27 CET 2010

require "ws"

module PRC
  def play_all(dur = 1)
    with_sound(:clm, false, :play, 1, :statistics, true, :output, "cook.snd") do
      beg = 0
      plucky(beg, dur, 440, 0.2, 1.0)
      beg += dur + 0.2
      bow(beg, dur, 440, 0.2, 1.0)
      beg += dur + 0.2
      brass(beg, dur, 440, 0.2, 1.0)
      beg += dur + 0.2
      clarinet(beg, dur, 440, 0.2, 1.0)
      beg += dur + 0.2
      flute(beg, dur, 440, 0.2, 1.0)
    end
  end
  
  def make_reed(*args)
    offset, slope = nil
    optkey(args, binding,
           [:offset, 0.6],
           [:slope, -0.8])
    lambda do |samp| [1.0, offset + slope * samp].min end
  end
  
  def reedtable(r, sample)
    r.call(sample)
  end

  def make_bowtable(*args)
    offset, slope = nil
    optkey(args, binding,
           [:offset, 0.0],
           [:slope, 1.0])
    lambda do |samp| [0.0, 1.0 - (slope * (samp + offset)).abs].max end
  end

  def bowtable(b, sample)
    b.call(sample)
  end

  def jettable(sample)
    [-1.0, [1.0, sample * (sample * sample - 1.0)].min].max
  end

  def make_onezero(*args)
    gain, zerocoeff = nil
    optkey(args, binding,
           [:gain, 0.5],
           [:zerocoeff, 1.0])
    make_one_zero(gain, gain * zerocoeff)
  end

  def make_onep(*args)
    polecoeff = optkey(args, [:polecoeff, 0.9])
    make_one_pole(1.0 - polecoeff, -polecoeff)
  end

  def set_pole(p, val)
    set_mus_b1(p, -val)
    set_mus_a0(p, 1.0 - val)
  end

  def set_gain(p, val)
    set_mus_a0(p, mus_a0(p) * val)
  end

  def lip_set_freq(b, freq)
    set_mus_frequency(b, freq)
  end

  def lip(b, mouthsample, boresample)
    temp = formant(b, mouthsample - boresample)
    temp = [1.0, temp * temp].min
    temp * mouthsample + ((1.0 - temp) * boresample)
  end

  def make_dc_block
    input = output = 0.0
    lambda do |samp|
      output = samp + (0.99 * output - input)
      input = samp
      output
    end
  end

  def dc_block(b, sample)
    b.call(sample)
  end

  def make_delaya(len, lag)
    lastin = output = 0.0
    input = make_delay(len)
    outpointer = 2.0 - lag
    outpointer += len while outpointer <= 0.0
    outpoint = outpointer.floor
    alpha = outpointer - outpoint
    coeff = (1.0 - alpha) / (1.0 + alpha)
    outpoint = -outpoint
    lambda do |samp|
      delay(input, samp)
      temp = tap(input, outpoint)
      output = -coeff * output + lastin + temp * coeff
      lastin = temp
      output
    end
  end

  def delaya(d, sample)
    d.call(sample)
  end

  def make_delayl(len, lag)
    input = make_delay(len)
    outpointer = 1 - lag
    outpointer += len while outpointer <= 0.0
    outpoint = outpointer.floor
    alpha = outpointer - outpoint
    omalpha = 1.0 - alpha
    outpoint = -outpoint
    lambda do |samp|
      delay(input, samp)
      tap(input, outpoint - 1) * omalpha + tap(input, outpoint) * alpha
    end
  end

  def delayl(d, sample)
    d.call(sample)
  end

  # sample instruments

  def plucky(start, dur, freq, amp, maxa)
    len = (mus_srate / 100.0).floor + 1
    delayline = make_delaya(len, mus_srate / freq.to_f - 0.5)
    filter = make_onezero()
    dout = 0.0
    len.times do |i| dout = delaya(delayline, 0.99 * dout + maxa * (1.0 - random(2.0))) end
    run_instrument(start, dur) do
      dout = delaya(delayline, one_zero(filter, dout))
      amp * dout
    end
  end

  def bowstr(start, dur, freq, amp, maxa)
    len = (mus_srate / 100.0).floor + 1
    ratio = 0.8317
    temp = mus_srate / freq.to_f - 4.0
    neckdelay = make_delayl(len, temp * ratio)
    bridgedelay = make_delayl((len / 2.0).floor, temp * (1.0 - ratio))
    bowtab = make_bowtable(:slope, 3.0)
    filt = make_onep()
    rate = 0.001
    bowing = true
    bowvelocity = rate
    maxvelocity = maxa
    attackrate = rate
    durlen = seconds2samples(dur)
    ctr = 0
    release = (0.8 * durlen).floor
    bridgeout = 0.0
    neckout = 0.0
    set_pole(filt, 0.6)
    set_gain(filt, 0.3)
    run_instrument(start, dur) do
      bridgerefl = nutrefl = veldiff = stringvel = bowtemp = 0.0
      if bowing
        unless maxvelocity == bowvelocity
          if bowvelocity < maxvelocity
            bowvelocity += attackrate
          else
            bowvelocity -= attackrate
          end
        end
      else
        if bowvelocity > 0.0
          bowvelocity -= attackrate
        end
      end
      bowtemp = 0.3 * bowvelocity
      filt_output = one_pole(filt, bridgeout)
      bridgerefl = -filt_output
      nutrefl = -neckout
      stringvel = bridgerefl + nutrefl
      veldiff = bowtemp - stringvel
      veldiff = veldiff * bowtable(bowtab, veldiff)
      neckout = delayl(neckdelay, bridgerefl + veldiff)
      bridgeout = delayl(bridgedelay, nutrefl + veldiff)
      result = amp * 10.0 * filt_output
      if ctr == release
        bowing = false
        attackrate = 0.0005
      end
      ctr += 1
      result
    end
  end

  def brass(start, dur, freq, amp, maxa)
    len = (mus_srate / 100.0).floor + 1
    delayline = make_delaya(len, 1.0 + mus_srate / freq.to_f)
    lipfilter = make_formant()
    dcblocker = make_dc_block()
    blowing = true
    rate = 0.001
    breathpressure = 0.0
    maxpressure = maxa
    attackrate = rate
    durlen = seconds2samples(dur)
    release = (0.8 * durlen).floor
    ctr = 0
    dout = 0.0
    lip_set_freq(lipfilter, freq)
    run_instrument(start, dur) do
      if blowing
        unless maxpressure == breathpressure
          if breathpressure < maxpressure
            breathpressure += attackrate
          else
            breathpressure -= attackrate
          end
        end
      else
        if breathpressure > 0.0
          breathpressure -= attackrate
        end
      end
      dout = delaya(delayline,
                    dc_block(dcblocker, lip(lipfilter, 0.3 * breathpressure, 0.9 * dout)))
      result = amp * dout
      if ctr == release
        blowing = false
        attackrate = 0.0005
      end
      ctr += 1
      result
    end
  end

  def clarinet(start, dur, freq, amp, maxa)
    len = (mus_srate / 100.0).floor + 1
    delayline = make_delayl(len, 0.5 * (mus_srate / freq.to_f) - 1.0)
    rtable = make_reed(:offset, 0.7, :slope, -0.3)
    filter = make_onezero()
    blowing = true
    breathpressure = 0.0
    rate = 0.001
    maxpressure = maxa
    attackrate = rate
    durlen = seconds2samples(dur)
    release = (0.8 * durlen).floor
    ctr = 0
    dout = 0.0
    run_instrument(start, dur) do
      pressurediff = 0.0
      if blowing
        unless maxpressure == breathpressure
          if breathpressure < maxpressure
            breathpressure += attackrate
          else
            breathpressure -= attackrate
          end
        end
      else
        if breathpressure > 0.0
          breathpressure -= attackrate
        end
      end
      pressurediff = one_zero(filter, -0.95 * dout) - breathpressure
      dout = delayl(delayline,
                    breathpressure + pressurediff * reedtable(rtable, pressurediff))
      result = amp * dout
      if ctr == release
        blowing = false
        attackrate = 0.0005
      end
      ctr += 1
      result
    end
  end

  def flute(start, dur, freq, amp, maxa)
    len = (mus_srate / 100.0).floor + 1
    ratio = 0.8
    temp = mus_srate / freq.to_f - 0.5
    jetdelay = make_delayl((len / 2.0).floor, temp * (1.0 - ratio))
    boredelay = make_delayl(len, ratio * temp)
    filter = make_onep()
    dcblocker = make_dc_block()
    jetrefl = 0.6
    sinphase = 0.0
    blowing = true
    breathpressure = 0.0
    rate = 0.0005
    maxpressure = maxa
    attackrate = rate
    durlen = seconds2samples(dur)
    release = (0.8 * durlen).floor
    ctr = 0
    dout = 0.0
    set_pole(filter, 0.8)
    set_gain(filter, -1.0)
    run_instrument(start, dur) do
      randpressure = 0.1 * breathpressure * random(1.0)
      temp = 0.0
      pressurediff = 0.0
      sinphase += 0.0007
      sinphase -= 6.28 if sinphase > 6.28
      randpressure = randpressure + 0.05 * breathpressure * sin(sinphase)
      if blowing
        unless maxpressure == breathpressure
          if breathpressure < maxpressure
            breathpressure += attackrate
          else
            breathpressure -= attackrate
          end
        end
      else
        if breathpressure > 0.0
          breathpressure -= attackrate
        end
      end
      temp = dc_block(dcblocker, one_pole(filter, dout))
      pressurediff = jettable(delayl(jetdelay,
                                     breathpressure + (randpressure - jetrefl * temp)))
      dout = delayl(boredelay, pressurediff)
      result = 0.3 * amp * dout
      if ctr == release
        blowing = false
        attackrate = 0.0005
      end
      ctr += 1
      result
    end
  end
end

include PRC

# prc95.rb ends here
