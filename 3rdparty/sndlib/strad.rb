# strad.rb -- Translation CLM --> Snd-Ruby
#
# Bowed string physical model with stiffness.  CLM version adapted
# from the Matlab and C versions courtesy of JOS and Stefania Serafin
# from code revised on 7/14/01
#
# CLM version by Juan Reyes
#
# Translator/Author: Michael Scholz <mi-scholz@users.sourceforge.net>
# Created: Sun Mar 16 02:46:52 CET 2003
# Changed: Wed Nov 17 23:56:56 CET 2010

require "ws"

class DCBlock
  def initialize(input = 0.0, output = 0.0)
    @dc_input = input
    @dc_output = output
  end

  def dcblock(sample)
    @dc_output = sample + (0.99 * @dc_output - @dc_input)
    @dc_input = sample
    @dc_output
  end
  
  def inspect
    format("#<%s: input: %1.3f, output: %1.3f>", self.class, @dc_input, @dc_output)
  end
end

add_help(:bow, "bow(start = 0, dur = 1, freq = 440, amp = 0.5, *args)
        :bufsize 2205         @srate 22050
        :fb      0.2          bow force (0.0..1.0)
        :vb      0.05         bow velocity (0.0..0.8)
        :bp      0.08         bow position (0.0 bridge, 0.5 middle of string, 1.0 nut)
        :inharm  0.1          inharmonicity (0.0 harmonic, 1.0 not harmonic)
        :ampenv  [0, 0, 20, 1, 48, 1, 92, 0, 100, 0]
        :degree  45
        :dist    0.0025
        :reverb  0.005")
def bow(start = 0, dur = 1, freq = 440, amp = 0.5, *args)
  bufsize, fb, vb, bp, inharm, ampenv, degree, dist, reverb = nil
  optkey(args, binding,
         [:bufsize, 2205],
         [:fb, 0.2],
         [:vb, 0.05],
         [:bp, 0.08],
         [:inharm, 0.1],
         [:ampenv, [0, 0, 20, 1, 48, 1, 92, 0, 100, 0]],
         [:degree, 45],
         [:dist, 0.0025],
         [:reverb, 0.005])
  len = seconds2samples(dur)
  ampf     = make_env(:envelope, ampenv, :scaler, amp, :length, len)
  dcblocker  = DCBlock.new
  vinut      = make_vct(bufsize)
  vinbridge  = make_vct(bufsize)
  vinutt     = make_vct(bufsize)
  vinbridget = make_vct(bufsize)
  vib = vin = vibt = vint = 0.0
  mus = 0.8                                       # friction coeff
  twavespeedfactor = 5.2
  posl = posr = poslt = posrt = indexl = indexr = indexlt = indexrt = 0
  indexl_1 = indexr_1 = indexlt_1 = indexrt_1 = indexl_2 = indexr_2 = indexlt_2 = indexrt_2 = 0
  updl = updr = updlt = updrt = 0
  # biquad coefficients from jos follow
  b0b = 0.859210
  b1b = -0.704922
  b2b = 0.022502
  a1b = -0.943639
  a2b = 0.120665
  b0n = 7.0580050e-001
  b1n = -5.3168461e-001
  b2n = 1.4579750e-002
  a1n = -9.9142489e-001
  a2n = 1.8012052e-001
  # bridge side, torsional waves
  b0bt = 9.9157155e-001
  b1bt = -8.2342890e-001
  b2bt = 8.8441749e-002
  a1bt = -8.3628218e-001
  a2bt = 9.2866585e-002
  # finger side, torsional waves
  b0nt = 4.3721359e-001
  b1nt = -2.7034968e-001
  b2nt = -5.7147560e-002
  a1nt = -1.2158343e+000
  a2nt = 3.2555068e-001
  # initializations for the filters
  xm1bt = xm2bt = xm1nt = xm2nt = ym1bt = ym2bt = ym1nt = ym2nt = xm1b = xm2b = ym1b = ym2b = 0.0
  xm1n = xm2n = ym1n = ym2n = ynb = ynbt = ynn = ynnt = ya1nb = ynba1 = y1nb = 0.0
  # friedlander friction inits
  aa = bb1 = cc1 = delta1 = bb2 = cc2 = delta2 = v = v1 = v2 = rhs = lhs = vtemp = f = 0.0
  string_impedance = 0.55
  string_impedancet = 1.8
  stick = 0
  zslope = 1.0 / ((1.0 / (2.0 * string_impedance)) + (1.0 / (2.0 * string_impedancet)))
  xnn = xnb = xnnt = xnbt = 0.0
  alphar = alphal = alphart = alphalt = 0
  l = (mus_srate() / freq) - 2
  lt = l / twavespeedfactor
  del_right = l * bp
  del_left = l * (1 - bp)
  del_leftt = lt * (1 - bp)
  del_rightt = lt * bp
  samp_rperiod = del_right.floor
  samp_lperiod = del_left.floor
  samp_lperiodt = del_leftt.floor
  samp_rperiodt = del_rightt.floor
  samp_rperiod = [samp_rperiod, 0].max
  samp_rperiod = [samp_rperiod, bufsize - 1].min
  samp_lperiod = [samp_lperiod, 0].max
  samp_lperiod = [samp_lperiod, bufsize - 1].min
  alphar = (del_right - samp_rperiod).to_f
  alphal = (del_left - samp_lperiod).to_f
  samp_rperiodt = [samp_rperiodt, 0].max
  samp_rperiodt = [samp_rperiodt, bufsize - 1].min
  samp_lperiodt = [samp_lperiodt, 0].max
  samp_lperiodt = [samp_lperiodt, bufsize - 1].min
  alphart = (del_rightt - samp_rperiodt).to_f
  alphalt = (del_leftt - samp_lperiodt).to_f
  posr = (len + posr) % bufsize
  posl = (len + posl) % bufsize
  posrt = (len + posrt) % bufsize
  poslt = (len + poslt) % bufsize
  bowfilt = lambda do |inharmon|
    # initialize coefficients
    ynb = ((b0b * vib + b1b * xm1b + b2b * xm2b) - a1b * ym1b) - a2b * ym2b
    xm2b, xm1b = xm1b, vib
    ym2b, ym1b = ym1b, ynb
    ynn = ((b0n * vin + b1n * xm1n + b2n * xm2n) - a1n * ym1n) - a2n * ym2n
    xm2n, xm1n = xm1n, vin
    ym2n, ym1n = ym1n, ynn
    # filters for torsional waves
    ynbt = ((b0bt * vibt + b1bt * xm1bt + b2bt * xm2bt) - a1bt * ym1bt) - a2bt * ym2bt
    xm2bt, xm1bt = xm1bt, vibt
    ym2bt, ym1bt = ym1bt, ynbt
    ynnt = ((b0nt * vint + b1nt * xm1nt + b2nt * xm2nt) - a1nt * ym1nt) - a2nt * ym2nt
    xm2nt, xm1nt = xm1nt, vint
    ym2nt, ym1nt = ym1nt, ynnt
    inharmon = [inharmon, 0.00001].max
    inharmon = [inharmon, 0.9999].min
    ya1nb = y1nb = -(inharmon * ynb) + ynba1 + inharmon * ya1nb
    ynba1 = ynb
    y1nb = -y1nb
    ynn = -ynn
    ynbt = -ynbt
  end
  friedlander = lambda do |vh|
    aa = zslope
    bb1 = ((0.2 * zslope + 0.3 * fb) - zslope * vb) - zslope * vh
    cc1 = ((0.06 * fb + zslope * vh * vb) - 0.2 * zslope * vh) - 0.3 * vb * fb
    delta1 = bb1 * bb1 - 4 * aa * cc1
    bb2 = ((-0.2 * zslope - 0.3 * fb) - zslope * vb) - zslope * vh
    cc2 = (((0.06 * fb + zslope * vh * vb) + 0.2 * zslope * vh) + 0.3 * vb * fb) + 0.1 * fb
    delta2 = bb2 * bb2 - 4 * aa * cc2
    if vb.zero? or fb.zero?
      v = vh
    else
      if vh == vb
        v, stick = vb, 1
      else
        if vh > vb
          lhs, rhs = 0.0, 1.0
        else
          rhs, lhs = 0.0, 1.0
        end
        if rhs == 1.0
          if delta1 < 0
            v, stick = vb, 1
          else
            if stick == 1
              vtemp = vb
              f = 2.0 * zslope * (vtemp - vh)
              if f >= (-(mus * fb))
                v = vtemp
              else
                v1 = (-bb1 + sqrt(delta1)) / (2.0 * aa)
                v2 = (-bb1 - sqrt(delta1)) / (2.0 * aa)
                v = [v1, v2].min
                stick = 0
              end
            else
              v1 = (-bb1 + sqrt(delta1)) / (2.0 * aa)
              v2 = (-bb1 - sqrt(delta1)) / (2.0 * aa)
              v = [v1, v2].min
              stick = 0
            end
          end
        elsif lhs == 1.0
          if delta2 < 0
            v, stick = vb, 1
          else
            if stick == 1
              vtemp = vb
              f = zslope * (vtemp - vh)
              if f <= (mus * fb) and f > 0
                v = vtemp
              else
                v1 = (-bb2 - sqrt(delta2)) / (2.0 * aa)
                v2 = (-bb2 + sqrt(delta2)) / (2.0 * aa)
                vtemp = [v1, v2].min
                stick = 0
                if vtemp > vb
                  v, stick = vb, 1
                else
                  v = vtemp
                  f = zslope * (v - vh)
                end
              end
            else
              v1 = (-bb2 - sqrt(delta2)) / (2.0 * aa)
              v2 = (-bb2 + sqrt(delta2)) / (2.0 * aa)
              v = [v1, v2].min
              stick = 0
            end
          end
          if v > vb
            v, stick = vb, 1
          end
        end
      end
      f = zslope * (v - vh)
      xnn = y1nb + (f / (2.0 * string_impedance))
      xnb = ynn + (f / (2.0 * string_impedance))
    end
  end
  i = 0
  run_instrument(start, dur, :distance, dist, :reverb_amount, reverb, :degree, degree) do
    indexl = ((i + posl + bufsize) - samp_lperiod) % bufsize
    indexr = ((i + posr + bufsize) - samp_rperiod) % bufsize
    indexlt = ((i + poslt + bufsize) - samp_lperiodt) % bufsize
    indexrt = ((i + posrt + bufsize) - samp_rperiodt) % bufsize
    indexl_1 = (((i + posl + bufsize) - samp_lperiod) - 1) % bufsize
    indexr_1 = (((i + posr + bufsize) - samp_rperiod) - 1) % bufsize
    indexlt_1 = (((i + poslt + bufsize) - samp_lperiodt) - 1) % bufsize
    indexrt_1 = (((i + posrt + bufsize) - samp_rperiodt) - 1) % bufsize
    indexl_2 = (((i + posl + bufsize) - samp_lperiod) - 2) % bufsize
    indexr_2 = (((i + posr + bufsize) - samp_rperiod) - 2) % bufsize
    indexlt_2 = (((i + poslt + bufsize) - samp_lperiodt) - 2) % bufsize
    indexrt_2 = (((i + posrt + bufsize) - samp_rperiodt) - 2) % bufsize
    # fractional delay lines
    vib = ((vinbridge[indexl_2] * (alphal - 1) * (alphal - 2)) / 2.0) +
          (vinbridge[indexl_1] * -alphal * (alphal - 2)) +
                                ((vinbridge[indexl] * alphal * (alphal - 1)) / 2.0)
    vin = ((vinut[indexr_2] * (alphar - 1) * (alphar - 2)) / 2.0) +
          (vinut[indexr_1] * -alphar * (alphar - 2)) +
                            ((vinut[indexr] * alphar * (alphar - 1)) / 2.0)
    vibt = ((vinbridget[indexlt_2] * (alphalt - 1) * (alphalt - 2)) / 2.0) +
           (vinbridget[indexlt_1] * -alphalt * (alphalt - 2)) +
                                   ((vinbridget[indexlt] * alphalt * (alphalt - 1)) / 2.0)
    vint = ((vinutt[indexrt_2] * (alphart - 1) * (alphart - 2)) / 2.0) +
           (vinutt[indexrt_1] * -alphart * (alphart - 2)) +
                               ((vinutt[indexrt] * alphart * (alphart - 1)) / 2.0)
    # biquad filters
    bowfilt.call(inharm)
    vh1 = ynn + y1nb + ynnt + ynbt
    # now solve set of simultaneous equations for v and f
    friedlander.call(vh1)
    f = zslope * (v - vh1)
    xnnt = ynbt + (f / (2.0 * string_impedancet))
    xnbt = ynnt + (f / (2.0 * string_impedancet))
    updl = (i + posl + bufsize) % bufsize
    updr = (i + posr + bufsize) % bufsize
    updlt = (i + poslt + bufsize) % bufsize
    updrt = (i + posrt + bufsize) % bufsize
    vinbridge[updl] = xnb
    vinut[updr] = xnn
    vinbridget[updlt] = xnbt
    vinutt[updrt] = xnnt
    out_val = env(ampf) * dcblocker.dcblock(xnb)
    lhs = rhs = 0.0
    i += 1
    out_val
  end
end

=begin
with_sound(:channels, 2) do bow(0, 3, 400, 0.5, :vb, 0.15, :fb, 0.1, :inharm, 0.25) end
with_sound(:channels, 2) do bow(0, 2, 440, 0.5, :fb, 0.25) end
with_sound(:channels, 2) do bow(0, 4, 600, 0.8) end

with_sound(:channels, 2) do bow(0, 6, 147, 2, :fb, 0.035, :vb, 0.1) end
with_sound(:channels, 2) do bow(0, 3, 1100, 0.5, :vb, 0.45, :fb, 0.9, :inharm, 0.3) end
with_sound(:channels, 2) do bow(0, 3, 1500, 0.5, :vb, 0.25, :fb, 0.9, :inharm, 0.3) end

with_sound(:channels, 2) do bow(0, 3, 1525, 0.5, :vb, 0.25, :fb, 0.9, :inharm, 0.3) end

with_sound(:channels, 2, :reverb, :jc_reverb_rb) do bow(0, 1, 400, 0.5, :reverb, 0.0051) end

with_sound(:channels, 2, :reverb, :jc_reverb_rb) do
   bow(0, 3, 366, 0.5, :degree, 0)
   bow(0, 3, 422, 0.5, :degree, 90)
   bow(4, 6, 147, 2, :fb, 0.035, :vb, 0.1, :reverb, 0.051)
end
=end

# strad.rb ends here
