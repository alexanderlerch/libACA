# analog-filter.rb -- analog-filter.scm --> analog-filter.rb -*- snd-ruby -*-

# Translator/Author: Michael Scholz <mi-scholz@users.sourceforge.net>
# Created: Tue Aug 01 22:58:31 CEST 2006
# Changed: Wed Nov 17 22:07:58 CET 2010

# Commentary:
#
# ;;; various even order analog filters, based primarily on Anders Johansson's (GPL'd) code
#
# module Analog_filter
#  make_butterworth_lowpass(n, fc)
#  make_butterworth_highpass(n, fc)
#  make_butterworth_bandpass(n, fl, fh)
#  make_butterworth_bandstop(n, fl, fh)
#
#  make_chebyshev_lowpass(n, fc, ripple = 1.0)
#  make_chebyshev_highpass(n, fc, ripple = 1.0)
#  make_chebyshev_bandpass(n, fl, fh, ripple = 1.0)
#  make_chebyshev_bandstop(n, fl, fh, ripple = 1.0)
#
#  make_inverse_chebyshev_lowpass(n, fc, loss_dB = 60.0)
#  make_inverse_chebyshev_highpass(n, fc, loss_dB = 60.0)
#  make_inverse_chebyshev_bandpass(n, fl, fh, loss_dB = 60.0)
#  make_inverse_chebyshev_bandstop(n, fl, fh, loss_dB = 60.0)
#
#  make_bessel_lowpass(n, fc)
#  make_bessel_highpass(n, fc)
#  make_bessel_bandpass(n, fl, fh)
#  make_bessel_bandstop(n, fl, fh)
#
#  make_elliptic_lowpass(n, fc, ripple = 1.0, loss_dB = 60.0)
#  make_elliptic_highpass(n, fc, ripple = 1.0, loss_dB = 60.0)
#  make_elliptic_bandpass(n, fl, fh, ripple = 1.0, loss_dB = 60.0)
#  make_elliptic_bandstop(n, fl, fh, ripple = 1.0, loss_dB = 60.0)
#

# Code:

require "clm"
require "dsp"

module Analog_filter
  def analog2digital(n, num, den, fz)
    g = 1.0
    wc = tan(PI * fz)
    wcc = wc * wc
    c = Vct.new(2 * n)
    j = 0
    k = 0
    0.step(n - 1, 2) do |i|
      nt0 = num[j + 0] / wcc
      nt1 = num[j + 1] / wc
      nt2 = num[j + 2]
      dt0 = den[j + 0] / wcc
      dt1 = den[j + 1] / wc
      dt2 = den[j + 2]
      kd = dt0 + dt1 + dt2
      kn = nt0 + nt1 + nt2
      c[k + 0] = (2.0 * dt2 - 2.0 * dt0) / kd
      c[k + 1] = (dt0 + (-dt1) + dt2) / kd
      c[k + 2] = (2.0 * nt2 - 2.0 * nt0) / kn
      c[k + 3] = (nt0 + (-nt1) + nt2) / kn
      g *= (kn / kd)
      j += 3
      k += 4
    end
    a = []
    b = []
    k = 0
    0.step(n - 1, 2) do |i|
      a.unshift(vct(c[k + 3], c[k + 2], c[k + 3]))
      b.unshift(vct(1.0, c[k], c[k + 1]))
      k += 4
    end
    [cascade2canonical(a).scale!(g), cascade2canonical(b)]
  end

  def prototype2highpass(n, num, den)
    g = 1.0
    numt = Vct.new(num.length)
    dent = Vct.new(den.length)
    i = 0
    0.step(n - 1, 2) do |k|
      g *= (num[i + 2] / den[i + 2])
      numt[i + 0] = 1.0
      numt[i + 1] = num[i + 1] / num[i + 2]
      numt[i + 2] = num[i + 0] / num[i + 2]
      dent[i + 0] = 1.0
      dent[i + 1] = den[i + 1] / den[i + 2]
      dent[i + 2] = den[i + 0] / den[i + 2]
      i += 3
    end
    numt[0] = g
    [numt, dent]
  end

  #
  # === BUTTERWORTH ===
  #
  def butterworth_prototype(n)
    len = (n * 3) / 2
    num = Vct.new(len)
    den = Vct.new(len)
    n2 = 2.0 * n
    j = 0
    1.step(n - 1, 2) do |w|
      num[j + 0] = 0.0
      num[j + 1] = 0.0
      num[j + 2] = 1.0
      den[j + 0] = 1.0
      den[j + 1] = 2.0 * cos((w * PI) / n2)
      den[j + 2] = 1.0
      j += 3
    end
    [num, den]
  end

  # n = order, fc = cutoff freq (srate = 1.0)
  add_help(:make_butterworth_lowpass,
           "make_butterworth_lowpass(n, fc): \
returns a lowpass Buttterworth filter; N = order, \
FC = cutoff freq (srate = 1.0): make_butterworth_lowpass(8, 0.1)")
  def make_butterworth_lowpass(n, fc)
    if n.odd? then n += 1 end
    proto = butterworth_prototype(n)
    coeffs = analog2digital(n, proto[0], proto[1], fc)
    make_filter(:xcoeffs, coeffs[0], :ycoeffs, coeffs[1])
  end

  add_help(:make_butterworth_highpass,
           "make_butterworth_highpass(n, fc): \
returns a highpass Buttterworth filter; N = order, \
FC = cutoff freq (srate = 1.0): make_butterworth_highpass(8, 0.1)")
  def make_butterworth_highpass(n, fc)
    if n.odd? then n += 1 end
    proto = butterworth_prototype(n)
    hproto = prototype2highpass(n, proto[0], proto[1])
    coeffs = analog2digital(n, hproto[0], hproto[1], fc)
    make_filter(:xcoeffs, coeffs[0], :ycoeffs, coeffs[1])
  end

  add_help(:make_butterworth_bandpass,
           "make_butterworth_bandpass(n, fl, fh): \
returns a bandpass Buttterworth filter; N = order, \
FL and FH are (1.0-based) edge freqs: make_butterworth_bandpass(4, 0.1, 0.2)")
  def make_butterworth_bandpass(n, fl, fh)
    lp = make_butterworth_lowpass(n, fh)
    hp = make_butterworth_highpass(n, fl)
    lambda do |y| filter(lp, filter(hp, y)) end
  end

  add_help(:make_butterworth_bandstop,
           "make_butterworth_bandstop(n, fl, fh): \
returns a bandstop Buttterworth filter; N = order, \
FL and FH are (1.0-based) edge freqs: make_butterworth_bandstop(4, 0.1, 0.2)")
  def make_butterworth_bandstop(n, fl, fh)
    lp = make_butterworth_lowpass(n, fl)
    hp = make_butterworth_highpass(n, fh)
    lambda do |y| filter(lp, y) + filter(hp, y) end
  end

  #
  # === CHEBYSHEV ===
  #

  # ripple in dB (positive)
  def chebyshev_prototype(n, ripple = 1.0)
    e = sqrt((10.0 ** (0.1 * ripple)) - 1.0)
    v0 = asinh(1.0 / e) / n.to_f
    len = (n * 3) / 2
    n2 = 2.0 * n
    sinhv0 = sinh(v0)
    coshv0 = cosh(v0)
    num = Vct.new(len)
    den = Vct.new(len)
    j = 0
    1.step(n - 1, 2) do |l|
      lpi = l * PI
      u = -(sinhv0 * sin(lpi / n2))
      w = coshv0 * cos(lpi / n2)
      num[j + 0] = 0.0
      num[j + 1] = 0.0
      num[j + 2] = 1.0
      den[j + 0] = 1.0
      den[j + 1] = -2.0 * u
      den[j + 2] = u * u + w * w
      j += 3
    end
    num[2] = (2.0 ** (2 - n)) / (3.2 ** (log(ripple) / log(10.0)))
    [num, den]
  end

  # n = order, fc = cutoff freq (srate = 1.0)
  add_help(:make_chebyshev_lowpass,
           "make_chebyshev_lowpass(n, fc, ripple=1.0): \
returns a lowpass Chebyshev filter; N = order, \
FC = cutoff freq (srate = 1.0): make_chebyshev_lowpass(8, 0.1)")
  def make_chebyshev_lowpass(n, fc, ripple = 1.0)
    if n.odd? then n += 1 end
    proto = chebyshev_prototype(n, ripple)
    coeffs = analog2digital(n, proto[0], proto[1], fc)
    make_filter(:xcoeffs, coeffs[0], :ycoeffs, coeffs[1])
  end

  add_help(:make_chebyshev_highpass,
           "make_chebyshev_highpass(n, fc, ripple=1.0): \
returns a highpass Chebyshev filter; N = order, \
FC = cutoff freq (srate = 1.0): make_chebyshev_highpass(8, 0.1, 0.01)")
  def make_chebyshev_highpass(n, fc, ripple = 1.0)
    if n.odd? then n += 1 end
    proto = chebyshev_prototype(n, ripple)
    hproto = prototype2highpass(n, proto[0], proto[1])
    coeffs = analog2digital(n, hproto[0], hproto[1], fc)
    make_filter(:xcoeffs, coeffs[0], :ycoeffs, coeffs[1])
  end

  add_help(:make_chebyshev_bandpass,
           "make_chebyshev_bandpass(n, fl, fh, ripple=1.0): \
returns a bandpass Chebyshev filter; N = order, \
FL and FH = edge freq (srate = 1.0): make_chebyshev_highpass(8, 0.1, 0.01)")
  def make_chebyshev_bandpass(n, fl, fh, ripple = 1.0)
    lp = make_chebyshev_lowpass(n, fh, ripple)
    hp = make_chebyshev_highpass(n, fl, ripple)
    lambda do |y| filter(lp, filter(hp, y)) end
  end

  add_help(:make_chebyshev_bandstop,
           "make_chebyshev_bandstop(n, fl, fh, ripple=1.0): \
returns a bandstop Chebyshev filter; N = order, \
FL and FH = edge freqs (srate = 1.0): make_chebyshev_bandstop(8, 0.1, 0.4, 0.01)")
  def make_chebyshev_bandstop(n, fl, fh, ripple = 1.0)
    lp = make_chebyshev_lowpass(n, fl, ripple)
    hp = make_chebyshev_highpass(n, fh, ripple)
    lambda do |y| filter(lp, y) + filter(hp, y) end
  end

  #
  # === INVERSE CHEBYSHEV ===
  #

  def inverse_chebyshev_prototype(n, loss_dB = 60.0)
    e = sqrt(1.0 / (10.0 ** (0.1 * loss_dB) - 1.0))
    v0 = asinh(1.0 / e) / n.to_f
    len = (n * 3) / 2
    n2 = 2.0 * n
    num = Vct.new(len)
    den = Vct.new(len)
    pl = 0.0
    j = 0
    1.0.step(n - 1, 2.0) do |l|
      lpi = l * PI
      u = -(sinh(v0) * sin(lpi / n2))
      w = cosh(v0) * cos(lpi / n2)
      t = 1.0 / sin(((l + pl) * PI) / n2)
      num[j + 0] = 1.0
      num[j + 1] = 0.0
      num[j + 2] = t * t
      den[j + 0] = 1.0
      den[j + 1] = (-2.0 * u) / (u * u + w * w)
      den[j + 2] = 1.0 / (u * u + w * w)
      j += 3
    end
    [num, den, 1.122 ** -loss_dB]
  end

  # n = order, fc = cutoff freq (srate = 1.0)
  add_help(:make_inverse_chebyshev_lowpass,
           "make_inverse_chebyshev_lowpass(n, fc, loss_dB=60.0): \
returns a lowpass inverse-Chebyshev filter; N = order, \
FC = cutoff freq (srate = 1.0): make_inverse_chebyshev_lowpass(10, 0.4, 120)")
  def make_inverse_chebyshev_lowpass(n, fc, loss_dB = 60.0)
    if n.odd? then n += 1 end
    proto = inverse_chebyshev_prototype(n, loss_dB)
    coeffs = analog2digital(n, proto[0], proto[1], fc)
    make_filter(:xcoeffs, coeffs[0].scale!(proto[2]), :ycoeffs, coeffs[1])
  end

  add_help(:make_inverse_chebyshev_highpass,
           "make_inverse_chebyshev_highpass(n, fc, loss_dB=60.0): \
returns a highpass inverse-Chebyshev filter; N = order, \
FC = cutoff freq (srate = 1.0): make_inverse_chebyshev_highpass(10, 0.1, 120)")
  def make_inverse_chebyshev_highpass(n, fc, loss_dB = 60.0)
    if n.odd? then n += 1 end
    proto = inverse_chebyshev_prototype(n, loss_dB)
    hproto = prototype2highpass(n, proto[0], proto[1])
    coeffs = analog2digital(n, hproto[0], hproto[1], fc)
    make_filter(:xcoeffs, coeffs[0].scale!(proto[2]), :ycoeffs, coeffs[1])
  end

  add_help(:make_inverse_chebyshev_bandpass,
           "make_inverse_chebyshev_bandpass(n, fl, fh, loss_dB=60.0): \
returns a bandpass inverse-Chebyshev filter; N = order, \
FL and FH are edge freqs (srate = 1.0): make_inverse_chebyshev_bandpass(8, 0.1, 0.4)")
  def make_inverse_chebyshev_bandpass(n, fl, fh, loss_dB = 60.0)
    lp = make_inverse_chebyshev_lowpass(n, fh, loss_dB)
    hp = make_inverse_chebyshev_highpass(n, fl, loss_dB)
    lambda do |y| filter(lp, filter(hp, y)) end
  end

  add_help(:make_inverse_chebyshev_bandstop,
           "make_inverse_chebyshev_bandstop(n, fl, fh, loss_dB=60.0): \
returns a bandstop inverse-Chebyshev filter; N = order, \
FL and FH are edge freqs (srate = 1.0): make_inverse_chebyshev_bandstop(8, 0.1, 0.4, 90)")
  def make_inverse_chebyshev_bandstop(n, fl, fh, loss_dB = 60.0)
    lp = make_inverse_chebyshev_lowpass(n, fl, loss_dB)
    hp = make_inverse_chebyshev_highpass(n, fh, loss_dB)
    lambda do |y| filter(lp, y) + filter(hp, y) end
  end

  if provided? :gsl
    # requires with-gsl
    if defined? gsl_roots
      # gsl_roots isn't defined for ruby in snd-xen.c
      #
      # === BESSEL(-Thompson) ===
      #
      def fact(n)
        x = 1
        2.upto(n) do |i| x *= i end
        x
      end

      def bessel_i(n)
        Vct.new(n + 1) do |i| fact(2 * n - i) / ((2 ** (n - i)) * fact(i) * fact(n - i)) end
      end
      
      def bessel_prototype(n)
        len = (n * 3) / 2
        num = Vct.new(len)
        den = Vct.new(len)
        b2 = bessel_i(n)
        p = gsl_roots(b2.to_a)
        p.map! do |x| x / (b2[0] ** (1.0 / n)) end
        j = 0
        0.step(n - 1, 2) do |i|
          num[j + 0] = 0.0
          num[j + 0] = 0.0
          num[j + 2] = 1.0
          den[j + 0] = 1.0
          den[j + 0] = -2.0 * p[i].real
          den[j + 2] = (p[i] * p[i + 1]).real
          j += 3
        end
        [num, den]
      end
      
      add_help(:make_bessel_lowpass,
               "make_bessel_lowpass(n, fc): \
returns a lowpass Bessel filter; N = order, \
FC = cutoff freq (srate = 1.0): make_bessel_lowpass(4, 0.1)")
      def make_bessel_lowpass(n, fc)
        if n.odd? then n += 1 end
        proto = bessel_prototype(n)
        coeffs = analog2digital(n, proto[0], proto[1], fc)
        make_filter(:xcoeffs, coeffs[0], :ycoeffs, coeffs[1])
      end

      add_help(:make_bessel_highpass,
               "make_bessel_highpass(n, fc): \
returns a highpass Bessel filter; N = order, \
FC = cutoff freq (srate = 1.0): make_bessel_highpass(8, 0.1)")
      def make_bessel_highpass(n, fc)
        if n.odd? then n += 1 end
        proto = bessel_prototype(n)
        hproto = prototype2highpass(n, proto[0], proto[1])
        coeffs = analog2digital(n, hproto[0], hproto[1], fc)
        make_filter(:xcoeffs, coeffs[0], :ycoeffs, coeffs[1])
      end

      add_help(:make_bessel_bandpass,
               "make_bessel_bandpass(n, fl, fh): \
returns a bandpass Bessel filter; N = order, \
FL and FH are edge freqs (srate = 1.0): make_bessel_bandpass(4, 0.1, 0.2)")
      def make_bessel_bandpass(n, fl, fh)
        lp = make_bessel_lowpass(n, fh)
        hp = make_bessel_highpass(n, fl)
        lambda do |y| filter(lp, filter(hp, y)) end
      end

      add_help(:make_bessel_bandstop,
               "make_bessel_bandstop(n, fl, fh): \
returns a bandstop Bessel filter; N = order, \
FL and FH are edge freqs (srate = 1.0): make_bessel_bandstop(8, 0.1, 0.2)")
      def make_bessel_bandstop(n, fl, fh, ripple = 1.0, loss_dB = 60.0)
        lp = make_bessel_lowpass(n, fl)
        hp = make_bessel_highpass(n, fh)
        lambda do |y| filter(lp, y) + filter(hp, y) end
      end
    end
    #
    # === ELLIPTIC ===
    #

    def minimize_function(f, xmin, xmax, arg1 = nil, arg2 = nil)
      fx = snd_func(f, xmin, arg1, arg2)
      n = 20
      x = Vct.new(n)
      n.times do |i|
        step = (xmax - xmin) / (n - 1.0)
        s = xmin
        (n - 1).times do |j|
          x[j] = s
          s += step
        end
        x[n - 1] = xmax
        n.times do |j|
          ft = snd_func(f, x[j], arg1, arg2)
          if ft < fx
            fx = ft
            xmax = (j < (n - 1)) ? x[j + 1] : x[n - 1]
            xmin = j > 0 ? x[j - 1] : x[0]
          end
        end
      end
      (xmax + xmin) / 2.0
    end

    def findm(m, arg1, arg2)
      (gsl_ellipk(m) / gsl_ellipk(1.0 - m) - arg1).abs
    end

    def findv(u, arg1, arg2)
      vals = gsl_ellipj(u, arg1)
      (arg2 - vals[0] / vals[1]).abs
    end
    
    def elliptic_prototype(n, ripple = 1.0, loss_dB = 60.0)
      e = sqrt((10.0 ** (0.1 * ripple)) - 1.0)
      k1 = e / sqrt((10.0 ** (0.1 * loss_dB)) - 1.0)
      k1p = sqrt(1.0 - k1 * k1)
      kr = m = k = 0.0
      len = (n * 3) / 2
      num = Vct.new(len)
      den = Vct.new(len)
      g = 1.0
      eps = 0.0000001
      if (1.0 - k1p * k1p).abs > eps
        kr = n.to_f * (gsl_ellipk(k1 * k1) / gsl_ellipk(k1p * k1p))
      end
      m = minimize_function(:findm, 0.001, 0.999, kr)
      k = gsl_ellipk(m)
      cv = Vct.new((0.5 * 3 * (n + 1)).floor)
      j = 0
      0.step(n - 1, 2) do |i|
        vals = gsl_ellipj(((i + 1) * k) / n.to_f, m)
        sn, cn, dn = vals[0..2]
        cv[j + 0] = sn
        cv[j + 1] = cn
        cv[j + 2] = dn
        z = Complex(0.0, -1.0) / (sqrt(m) * sn)
        pz = (z * make_rectangular(z.real, -z.imag)).real
        g /= pz
        num[j + 0] = 1.0
        num[j + 1] = -2.0 * z.real
        num[j + 2] = pz
        j += 3
      end
      optarg0 = k1p * k1p
      optarg1 = 1.0 / e
      minf = minimize_function(:findv, 0.0, 1.0 / e, optarg0, optarg1)
      v0 = (k * minf) / (n.to_f * gsl_ellipk(k * k1))
      vals = gsl_ellipj(v0, 1.0 - m)
      sn, cn, dn = vals[0..2]
      j = 0
      0.step(n - 1, 2) do |i|
        p = -(cv[j + 1] * cv[j + 2] * sn * cn + (Complex(0.0, 1.0) * cv[j + 0] * dn)) /
          (1.0 - (cv[j + 2] * sn * cv[j + 2] * sn))
        pp = (p * make_rectangular(p.real, -p.imag)).real
        g *= pp
        den[j + 0] = 1.0
        den[j + 1] = -2.0 * p.real
        den[j + 2] = pp
        j += 3
      end
      g = (g / sqrt(1.0 + e * e)).abs
      [num, den, g]
    end
    
    # n = order, fc = cutoff freq (srate = 1.0)
    add_help(:make_elliptic_lowpass,
             "make_elliptic_lowpass(n, fc, ripple=1.0, loss_dB=60.0): \
returns a lowpass elliptic filter; N = order, \
FC = cutoff freq (srate = 1.0): make_elliptic_lowpass(8, 0.25, 0.01, 90)")
    def make_elliptic_lowpass(n, fc, ripple = 1.0, loss_dB = 60.0)
      if n.odd? then n += 1 end
      proto = elliptic_prototype(n, ripple, loss_dB)
      coeffs = analog2digital(n, proto[0], proto[1], fc)
      make_filter(:xcoeffs, coeffs[0].scale!(proto[2]), :ycoeffs, coeffs[1])
    end

    add_help(:make_elliptic_highpass,
             "make_elliptic_highpass(n, fc, ripple=1.0, loss_dB=60.0): \
returns a highpass elliptic filter; N = order, \
FC = cutoff freq (srate = 1.0): make_elliptic_highpass(8, 0.25, 0.01, 90)")
    def make_elliptic_highpass(n, fc, ripple = 1.0, loss_dB = 60.0)
      if n.odd? then n += 1 end
      proto = elliptic_prototype(n, ripple, loss_dB)
      hproto = prototype2highpass(n, proto[0], proto[1])
      coeffs = analog2digital(n, hproto[0], hproto[1], fc)
      make_filter(:xcoeffs, coeffs[0].scale!(proto[2]), :ycoeffs, coeffs[1])
    end

    add_help(:make_elliptic_bandpass,
             "make_elliptic_bandpass(n, fl, fh, ripple=1.0, loss_dB=60.0): \
returns a bandpass elliptic filter; N = order, \
FL and FH are edge freqs (srate = 1.0): make_elliptic_bandpass(6, 0.1, 0.2, 0.1, 90)")
    def make_elliptic_bandpass(n, fl, fh, ripple = 1.0, loss_dB = 60.0)
      lp = make_elliptic_lowpass(n, fh, ripple, loss_dB)
      hp = make_elliptic_highpass(n, fl, ripple, loss_dB)
      lambda do |y| filter(lp, filter(hp, y)) end
    end

    add_help(:make_elliptic_bandstop,
             "make_elliptic_bandstop(n, fl, fh, ripple=1.0, loss_dB=60.0): \
returns a bandstop elliptic filter; N = order, \
FL and FH are edge freqs (srate = 1.0): make_elliptic_bandstop(6, 0.1, 0.2, 0.1, 90)")
    def make_elliptic_bandstop(n, fl, fh, ripple = 1.0, loss_dB = 60.0)
      lp = make_elliptic_lowpass(n, fl, ripple, loss_dB)
      hp = make_elliptic_highpass(n, fh, ripple, loss_dB)
      lambda do |y| filter(lp, y) + filter(hp, y) end
    end
  end
end

include Analog_filter

# analog-filter.rb ends here
