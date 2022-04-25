# pvoc.rb -- pvoc.scm -> pvoc.rb

# Translator: Michael Scholz <mi-scholz@users.sourceforge.net>
# Created: Sat Mar 27 00:19:51 CET 2004
# Changed: Sat Feb 19 17:21:21 CET 2011

# Comment:
# 
# versions of the Moore-Klingbeil-Trevisani-Edwards phase-vocoder
#
# class Pvocoder
#   initialize(fftsize, overlap, interp, analyze, edit, synthesize)
#   inspect
#   pvocoder(input)
#
#  make_pvocoder(fftsize, overlap, interp, analyze, edit, synthesize)
#  pvocoder(pv, input)
#
#  test_pv_1
#  test_pv_2(freq)
#  test_pv_3(time)
#  test_pv_4(gate)
#  
#  pvoc(*rest)
#  
# Code:

require "ws"

class Pvocoder
  def initialize(fftsize, overlap, interp, analyze, edit, synthesize)
    @output     = interp
    @interp     = interp
    @hop        = overlap
    @filptr     = 0
    @N          = fftsize
    @window     = vct_scale!(make_fft_window(Hamming_window, fftsize), 2.0 / (0.54 * fftsize))
    @D          = fftsize / overlap
    @in_data    = nil
    @ampinc     = make_vct(fftsize)
    @freqs      = make_vct(fftsize)
    @amps       = make_vct(fftsize / 2)
    @phaseinc   = make_vct(fftsize / 2)
    @phases     = make_vct(fftsize / 2)
    @lastphase  = make_vct(fftsize / 2)
    @analyze    = analyze
    @edit       = edit
    @synthesize = synthesize
  end

  def inspect
    format("#<%s outctr: %d, interp: %d, filptr: %d, N: %d, D: %d, in_data: %s>",
           self.class, @output, @interp, @filptr, @N, @D, @in_data.inspect)
  end
  
  def pvocoder(input)
    if @output >= @interp
      if @analyze
        @analyze.call(self, input)
      else
        vct_fill!(@freqs, 0.0)
        @output = 0
        if (not vct?(@in_data))
          @in_data = make_vct!(@N) do input.call end
        else
          vct_move!(@in_data, 0, @D)
          ((@N - @D)...@N).each do |i| @in_data[i] = input.call end
        end
        buf = @filptr % @N
        if buf.zero?
          vct_fill!(@ampinc, 0.0)
          vct_add!(@ampinc, @in_data)
          vct_multiply!(@ampinc, @window)
        else
          @N.times do |k|
            @ampinc[buf] = @window[k] * @in_data[k]
            buf += 1
            if buf >= @N
              buf = 0
            end
          end
        end
        @filptr += @D
        mus_fft(@ampinc, @freqs, @N, 1)
        rectangular2polar(@ampinc, @freqs)
      end
      if @edit
        @edit.call(self)
      else
        pscl = 1.0 / @D
        kscl = TWO_PI / @N
        (@N / 2).times do |k|
          phasediff = @freqs[k] - @lastphase[k]
          @lastphase[k] = @freqs[k]
          while phasediff > PI
            phasediff -= TWO_PI
          end
          while phasediff < -TWO_PI
            phasediff += TWO_PI
          end
          @freqs[k] = pscl * phasediff + k * kscl
        end
      end
      scl = 1.0 / @interp
      vct_subtract!(@ampinc, @amps)
      vct_subtract!(@freqs, @phaseinc)
      vct_scale!(@ampinc, scl)
      vct_scale!(@freqs, scl)
    end
    @output += 1
    if @synthesize
      @synthesize.call
    else
      vct_add!(@amps, @ampinc)
      vct_add!(@phaseinc, @freqs)
      vct_add!(@phases, @phaseinc)
      sine_bank(@amps, @phases)
    end
  end
end

add_help(:make_pvocoder,
         "make_pvocoder(fftsize, overlap, interp, [analyze=false, [edit=false, [synthesize=false]]])
makes a new (Ruby-based, not CLM) phase-vocoder generator")
def make_pvocoder(fftsize = 512,
                  overlap = 4,
                  interp = 128,
                  analyze = false,
                  edit = false,
                  synthesize = false)
  Pvocoder.new(fftsize, overlap, interp, analyze, edit, synthesize)
end

add_help(:pvocoder,
         "pvocoder(pv, input) is the phase-vocoder generator associated with make_pvocoder")
def pvocoder(pv, input)
  pv.pvocoder(input)
end

=begin
let(open_sound("oboe.snd"),
    make_pvocoder(256, 4, 64),
    make_sampler(0)) do |ind, pv, rd|
  map_channel(lambda do |y| pvocoder(pv, rd) end)
  play(ind, :wait, true)
  save_sound_as("pvoc.snd", ind)
  revert_sound(ind)
  close_sound(ind)
  open_sound("pvoc.snd")
end
=end

def test_pv_1
  pv = make_phase_vocoder(false, 512, 4, 128, 1.0, false, false, false)
  rd = make_sampler(0)
  map_channel(lambda do |y| phase_vocoder(pv, lambda do |dir| next_sample(rd) end) end)
  free_sampler(rd)
end

def test_pv_2(freq)
  pv = make_phase_vocoder(false, 512, 4, 128, freq, false, false, false)
  rd = make_sampler(0)
  map_channel(lambda do |y| phase_vocoder(pv, lambda do |dir| next_sample(rd) end) end)
  free_sampler(rd)
end

def test_pv_3(time)
  pv = make_phase_vocoder(false, 512, 4, (time * 128.0).floor, 1.0, false, false, false)
  rd = make_sampler(0)
  len = (time * frames).floor
  data = make_vct!(len) do phase_vocoder(pv, lambda do |dir| next_sample(rd) end) end
  free_sampler(rd)
  vct2channel(data, 0, len)
end

def test_pv_4(gate)
  pv = make_phase_vocoder(false,
                          512, 4, 128, 1.0,
                          false,
                          lambda { |v|
                            phase_vocoder_amp_increments(v).map! do |val|
                              if val < gate
                                0.0
                              else
                                val
                              end
                              true
                            end
                          }, false)
  rd = make_sampler(0)
  map_channel(lambda do |y| phase_vocoder(pv, lambda do |dir| next_sample(rd) end) end)
  free_sampler(rd)
end

# another version of the phase vocoder

add_help(:pvoc,
         "(pvoc, *rest) [fftsize, overlap, time, pitch, gate, hoffset, snd, chn] \
applies the phase vocoder algorithm to the current sound (i.e. fft analysis, \
oscil bank resynthesis). 'pitch' specifies the pitch transposition ratio, 'time' - \
specifies the time dilation ratio, 'gate' specifies a resynthesis gate in dB (partials \
with amplitudes lower than the gate value will not be synthesized), \
'hoffset is a pitch offset in Hz.")
def pvoc(*rest)
  fftsize, overlap, time, pitch, gate, hoffset, snd, chn = nil
  optkey(rest, binding,
         [:fftsize, 512],
         [:overlap, 4],
         [:time, 1.0],
         [:pitch, 1.0],
         [:gate, 0.0],
         [:hoffset, 0.0],
         [:snd, false],
         [:chn, false])
  len = frames(snd, chn)
  filptr = 0
  sr = srate(snd)
  fftsize2 = fftsize / 2
  d = fftsize / overlap
  interp = d * time
  syngate = gate.zero? ? 0.0 : (10 ** (-gate.abs / 20.0))
  poffset = hz2radians(hoffset)
  window = make_fft_window(Hamming_window, fftsize)
  fdr = make_vct(fftsize)
  fdi = make_vct(fftsize)
  lastphase = make_vct(fftsize2)
  lastamp = make_vct(fftsize2)
  lastfreq = make_vct(fftsize2)
  ampinc = make_vct(fftsize2)
  freqinc = make_vct(fftsize2)
  fundamental = TWO_PI / fftsize
  output = interp
  resynth_oscils = make_array(fftsize2) do make_oscil(:frequency, 0) end
  outlen = (time * len).floor
  in_data = channel2vct(0, fftsize * 2, snd, chn)
  in_data_beg = 0
  vct_scale!(window, 2.0 / (0.54 * fftsize))
  out_data = make_vct([len, outlen].max)
  out_data.length.times do |i|
    if output >= interp
      output = 0
      buffix = filptr % fftsize
      vct_fill!(lastamp, 0.0)
      vct_fill!(lastfreq, 0.0)
      vct_add!(lastamp, fdr)
      vct_add!(lastfreq, fdi)
      fftsize.times do |k|
        fdr[buffix] = window[k] * in_data[filptr - in_data_beg]
        filptr += 1
        buffix += 1
        if buffix >= fftsize
          buffix = 0
        end
      end
      filptr -= fftsize - d
      if filptr > in_data_beg + fftsize
        in_data_beg = filptr
        in_data = channel2vct(in_data_beg, fftsize * 2, snd, chn)
      end
      vct_fill!(fdi, 0.0)
      mus_fft(fdr, fdi, fftsize, 1)
      fftsize2.times do |k|
        a = fdr[k]
        b = fdi[k]
        mag = sqrt(a * a + b * b)
        phase = 0
        phasediff = 0
        fdr[k] = mag
        if mag > 0
          phase = -atan2(b, a)
          phasediff = phase - lastphase[k]
          lastphase[k] = phase
          while phasediff > PI
            phasediff -= TWO_PI
          end
          while phasediff < -PI
            phasediff += TWO_PI
          end
        end
        fdi[k] = pitch * ((phasediff * sr) / (d * sr) + k * fundamental + poffset)
        if fdr[k] < syngate
          fdr[k] = 0.0
        end
        ampinc[k] = (fdr[k] - lastamp[k]) / interp
        freqinc[k] = (fdi[k] - lastfreq[k]) / interp
      end
    end
    output += 1
    vct_add!(lastamp, ampinc)
    vct_add!(lastfreq, freqinc)
    out_data[i] = oscil_bank(lastamp, resynth_oscils, lastfreq)
  end
  vct2channel(out_data, 0, out_data.length)
end

# pvoc.rb ends here
