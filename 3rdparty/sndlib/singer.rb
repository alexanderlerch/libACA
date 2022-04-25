# singer.rb -- singer.ins -> singer.scm -> singer.rb -*- snd-ruby -*-

# Translator: Michael Scholz <mi-scholz@users.sourceforge.net>
# Created: Sat Apr 23 13:07:53 CEST 2005
# Changed: Mon Nov 22 13:36:42 CET 2010

# Commentary:
#
# Perry Cook's physical model of the vocal tract as described in:
#
# Cook, Perry R. "Synthesis of the Singing Voice Using a Physically
# Parameterized Model of the Human Vocal Tract"
# Published in the Proceedings of the International Computer Music Conference, Ohio 1989
# and as Stanford University Department of Music Technical Report Stan-M-57, August 1989.
#
# -- "Identification of Control Parameters in an Articulatory Vocal
#    Tract Model, with Applications to the Synthesis of Singing,"
#    Ph.D. Thesis, Stanford University Department of Music Technical
#    Report
#
# -- "SPASM, a Real-time Vocal Tract Physical Model Controller; and
#    Singer, the Companion Software Synthesis System", Computer Music
#    Journal, vol 17 no 1 Spring 1993.
#
# This code is a translation of Perry Cook's singer implementation originally in C.
# Apparently all Perry's data is aimed at srate=22050.
#
# Code:

def singer(start, amp, data)
  # data is a list of lists very similar to the sequence of synthesize
  # calls in Perry's original implementation.
  # Each imbedded list has the form: dur shape glot pitch glotamp
  # noiseamps vibramt.
  # See below for examples.
  setup = data.first
  durs = data.map do |val| val[0] end
  dur = 0.0
  durs.each do |val| dur += val end
  dur -= samples2seconds(1)
  beg = start
  begs = [start] + durs.map do |val| beg += val end
  beg_samps = begs.map do |val| seconds2samples(val) end
  change_times = (beg_samps + [beg_samps.last]).to_vct
  shps = data.map do |val| val[1] end
  glts = data.map do |val| val[2] end
  pfun = [0.0, 0.8 * setup[3]]
  data.zip(begs[1..-1]) do |dat, b| pfun.push(b - start, Float(dat[3])) end
  gfun = [0.0, 0.0]
  data.zip(begs[1..-1]) do |dat, b| gfun.push(b - start, Float(dat[4])) end
  nfun = [0.0, Float(setup[5])]
  data.zip(begs[1..-1]) do |dat, b| nfun.push(b - start, Float(dat[5])) end
  vfun = [0.0, Float(setup[6])]
  data.zip(begs[1..-1]) do |dat, b| vfun.push(b - start, Float(dat[6])) end
  noiseamps = Vct.new(data.length) do |i| Float(data[i][5]) end
  frq_env = make_env(:envelope, pfun, :duration, dur)
  vib_env = make_env(:envelope, vfun, :duration, dur)
  vib_osc = make_oscil(:frequency, 6.0)
  glot_env = make_env(:envelope, gfun, :duration, dur)
  noise_env = make_env(:envelope, nfun, :duration, dur)
  ran_vib = make_rand_interp(:frequency, 10, :amplitude, 0.02)
  # 
  tractlength = 9                              # length of vocal tract
  shape_data = Vct.new(shps.length * (tractlength + 8))
  glot_datai = Vct.new(2 * glts.length)
  glot_datar = Vct.new(2 * glts.length)
  shps.each_with_index do |shp, i|
    shp[1..-1].each_with_index do |val, j| shape_data[j + i] = val end
  end
  ii = 0
  glts.each do |glt|
    glot_datai[ii] = 0.0
    glot_datai[ii + 1] = glt[0]
    glot_datar[ii] = glt[1]
    glot_datar[ii + 1] = glt[2]
    ii += 2
  end
  table_size = 1000                               # size of glottis wave-table
  noseposition = 3
  noselength = 6
  nose_ring_time = 1000                           # naso pharynx response decay time
  one_over_two_pi = 1.0 / TWO_PI
  two_pi_over_table_size = TWO_PI / table_size
  table_size_over_sampling_rate = table_size / mus_srate
  dpole = 0.998
  dgain = 1.0 - dpole
  tong_hump_pole = 0.998
  tong_hump_gain = 1.0 - tong_hump_pole
  tong_tip_pole = 0.998
  tong_tip_gain = 1.0 - tong_tip_pole
  glot_table = Vct.new(table_size + 1)
  glot_table2 = Vct.new(table_size + 1)
  gn_table = Vct.new(table_size + 1)
  gn_gain = 0.0
  gn_out = 0.0
  gn_del = Vct.new(4)
  gn_coeffs = Vct.new(4)
  sines = Vct.new(200)
  cosines = Vct.new(200)
  table_increment = 0.0
  table_location = 0.0
  glot_refl_gain = 0.7
  pitch = 400.0
  vibr_amt = 0.0
  last_lip_in = 0.0                               # for lip reflection/transmission filter
  last_lip_out = 0.0
  last_lip_refl = 0.0
  lip_refl_gain = -0.45
  noise_gain = 0.0                                # for vocal tract noise generator
  noise_input = 0.0
  noise_output = 0.0
  noise_c = Vct.new(4)                            # net coefficients on delayed outputs
  noise_pos = 0
  fnoiseamp = 0.0
  inz1 = 0.0
  inz2 = 0.0
  outz = Vct.new(4)                               # delayed versions of input and output
  # nasal tract acoustic tube structure
  nose_coeffs = vct(0.0, -0.29, -0.22, 0.0, 0.24, 0.3571)
  nose1 = Vct.new(noselength)
  nose2 = Vct.new(noselength)
  velum_pos = 0.0
  alpha = Vct.new(4)
  nose_last_minus_refl = 0.0
  nose_last_plus_refl = 0.0
  nose_last_output = 0.0
  nose_filt = 0.0
  nose_filt1 = 0.0
  time_nose_closed = 1000  # this is a hack used to determine if we
                           # need to calculate the nasal acoustics
  # vocal tract acoustic tube structure
  radii = Vct.new(tractlength + 8)
  # the radii array contains the vocal tract section radii
  # (tractlength-1 of them), then glottal reflection gain then lip
  # reflection gain, then noise position, then noise gain, then noise
  # pole angle, then noise pole radius, then noise pole angle2, then
  # noise pole radius2, then velum opening radius
  8.times do |i| radii[i] = 1.0 end
  radii[8] = 0.7
  radii[9] = -0.5
  coeffs = Vct.new(tractlength)
  dline1 = Vct.new(tractlength)
  dline2 = Vct.new(tractlength)
  # throat radiation low-pass filter
  lt = Vct.new(2)
  ltcoeff = 0.9995
  ltgain = 0.05                                   # a low order iir filter
  lip_radius = 0.0
  s_glot = 0.0
  s_glot_mix = 0.0
  s_noise = 0.0
  last_tract_plus = 0.0
  initial_noise_position = 0.0
  formant_shift = 1.0
  target_radii = Vct.new(tractlength + 8)
  8.times do |i| target_radii[i] = 1.0 end
  target_radii[8] = 0.7
  target_radii[9] = -0.5
  radii_poles = Vct.new(tractlength + 8, dpole)
  radii_poles[2] = tong_hump_pole
  radii_poles[3] = tong_hump_pole
  radii_poles[4] = tong_hump_pole
  radii_poles[5] = tong_tip_pole
  radii_pole_gains = Vct.new(tractlength + 8, dgain)
  radii_pole_gains[2] = tong_hump_gain
  radii_pole_gains[3] = tong_hump_gain
  radii_pole_gains[4] = tong_hump_gain
  radii_pole_gains[5] = tong_tip_gain
  change_radii = 0
  glotsamp = 0.0
  delta = 0.0
  temp_arr = Vct.new(tractlength + 1)
  new_glot = 1
  first_glot = 1
  new_tract = 1
  first_tract = 1
  offset = -1
  next_offset = seconds2samples(start)
  last_sfd = -1
  last_gfd = -1
  run_instrument(start, dur) do |i|
    if i == next_offset
      # time to check for new tract shapes, glottal pulse shapes etc.
      offset += 1
      fnoiseamp = noiseamps[offset]
      if last_sfd == -1
        last_sfd = 0
      else
        new_sfd = last_sfd + tractlength + 8
        kk = new_sfd
        last_sfd.upto(new_sfd - 1) do |j|
          if (shape_data[j] - shape_data[kk]).abs > 0.001 then new_tract = 1 end
          kk += 1
        end
        last_sfd = new_sfd
      end
      if last_gfd == -1
        last_gfd = 0
      else
        last_gfd += 2
      end
      next_offset = change_times[offset + 1].to_i
    end
    if new_tract.nonzero?
      jj = last_sfd - 1
      target_radii.map! do |val| shape_data[jj += 1] end
      if first_tract == 1
        radii.map_with_index! do |val, j| target_radii[j] end
      end
      change_radii = 0
      initial_noise_position = radii[tractlength + 1]
      target_radii.zip(radii) do |t, r|
        if (t - r).abs > 0.001 then change_radii = 1 end
      end
    end
    if first_tract == 1 or change_radii.nonzero?
      if new_tract.zero?
        radii.map_with_index! do |val, j|
          val * radii_poles[j] + target_radii[j] * radii_pole_gains[j]
        end
      end
      # set tract shape
      temp_arr[0] = 1.0
      1.upto(temp_arr.length - 1) do |j|
        temp_arr[j] = radii[j - 1] * radii[j - 1]
        if temp_arr[j].zero? then temp_arr[j] = 1e-10 end
      end
      1.upto(tractlength - 1) do |j|
        coeffs[j] = (temp_arr[j - 1] - temp_arr[j]) / (temp_arr[j - 1] + temp_arr[j])
      end
      glot_refl_gain = radii[tractlength - 1]
      lip_refl_gain = radii[tractlength]
      noise_pos = radii[tractlength + 1].to_i
      noise_gain = radii[tractlength + 2]
      # fricative noise generator (set noise angle and radius)
      noise_angle = hz2radians(radii[tractlength + 3])
      noise_radius = radii[tractlength + 4]
      noise_a = -2.0 * cos(noise_angle / formant_shift) * noise_radius
      noise_b = noise_radius * noise_radius
      noise_angle2 = hz2radians(radii[tractlength + 5])
      noise_radius2 = radii[tractlength + 6]
      noise_a2 = -2.0 * cos(noise_angle2 / formant_shift) * noise_radius2
      noise_b2 = noise_radius2 * noise_radius2
      noise_c[0] = noise_a + noise_a2
      noise_c[1] = noise_b + noise_b2 + noise_a * noise_a2
      noise_c[2] = noise_a2 * noise_b + noise_b2 * noise_a
      noise_c[3] = noise_b2 * noise_b
      lip_radius = radii[tractlength - 2]
      velum_pos = radii[tractlength + 7]
      leftradius = radii[noseposition - 2]
      velumradius = velum_pos
      rightradius = radii[noseposition - 1]
      # nasal tract (set nasal shape)
      temp = [rightradius - velumradius, 0.0].max
      alpha[1] = leftradius * leftradius
      alpha[2] = temp * temp
      alpha[3] = velumradius * velumradius
      temp1 = 2.0 / (alpha[1] + alpha[2] + alpha[3])
      alpha[1] *= temp1
      alpha[2] *= temp1
      alpha[3] *= temp1
    end
    if new_tract.nonzero?
      new_tract = 0
      first_tract = 0
      if s_noise < 1.0 or fnoiseamp < 0.0001
        target_radii[tractlength + 1] = initial_noise_position
      end
    end
    if new_glot.nonzero?
      if first_glot.zero?
        glot_table2.map_with_index! do |val, j| glot_table[j] end
      end
      harms = glot_datai[last_gfd + 1].to_i
      a = glot_datar[last_gfd]
      b = glot_datar[last_gfd + 1]
      a2 = TWO_PI * a
      b2 = TWO_PI * b
      sines.fill(0.0)
      cosines.fill(0.0)
      if a != b
        temp = one_over_two_pi / (b - a)
        temp1 = 1.0 - cos(a2)
        sines[1] = (cos(a2) + (sin(a2) - sin(b2)) * temp) * temp1 * one_over_two_pi
        cosines[1] = (-sin(a2) + (cos(a2) - cos(b2)) * temp) * temp1 * one_over_two_pi
      end
      sines[1] = sines[1] + (0.75 + -cos(a2) + cos(2.0 * a2) * 0.25) * one_over_two_pi
      cosines[1] = cosines[1] + (sin(a2) - sin(2.0 * a2) * 0.25) * one_over_two_pi - a * 0.5
      ka1 = a2
      ka2 = 2 * a2
      ka3 = 3 * a2
      2.upto(harms) do |k|
        if b != a
          temp = one_over_two_pi / ((b - a) * k)
          sines[k] = (cos(ka2) + (sin(ka2) - sin(k * b2)) * temp) * (temp1 / k)
          cosines[k] = (-sin(ka2) + (cos(ka2) - cos(k * b2)) * temp) * (temp1 / k)
        end
        sines[k] = sines[k] + ((1.0 - cos(ka2)) / k) + \
          ((cos(ka1) - 1.0) * 0.5) / (k - 1) + \
          ((cos(ka3) - 1.0) * 0.5) / (k + 1)
        sines[k] *= one_over_two_pi
        cosines[k] = cosines[k] + sin(ka2) / k - (sin(ka1) * 0.5) / (k - 1) - (sin(ka3) * 0.5) / (k + 1)
        cosines[k] *= one_over_two_pi
        ka1 += a2
        ka2 += a2
        ka3 += a2
      end
      glot_table.fill(0.0)
      x = 0.0
      glot_table.length.times do |j|
        1.upto(harms) do |k|
          glot_table[j] = glot_table[j] + cosines[k] * cos(k * x) + sines[k] * sin(k * x)
        end
        x += two_pi_over_table_size
      end
      s_glot_mix = 1.0
      delta = 1.0 / (next_offset - i)
      if first_glot.nonzero?
        glot_table2.map_with_index! do |val, j| glot_table[j] end
        first_glot = 0
      end
      new_glot = 0
    end
    s_glot_mix -= delta
    s_glot = env(glot_env)
    s_noise = env(noise_env)
    pitch = env(frq_env)
    vibr_amt = env(vib_env)
    table_increment = pitch *
      (1.0 + vibr_amt * oscil(vib_osc) + rand_interp(ran_vib)) *
      table_size_over_sampling_rate
    last_lip_out = last_lip_in + last_tract_plus
    last_lip_refl = (last_lip_in + last_tract_plus) * lip_refl_gain
    last_lip_in = last_tract_plus
    # next glot tick
    glotsamp = dline2[1] * glot_refl_gain
    if table_increment.nonzero?
      table_location += table_increment
      if table_location >= table_size then table_location -= table_size end
      int_loc = table_location.floor
      table1 = glot_table[int_loc]
      table2 = glot_table2[int_loc]
      glotsamp = glotsamp + s_glot * (table1 + s_glot_mix * (table2 - table1))
      # glot noise tick
      if gn_table[int_loc].nonzero? and gn_gain.nonzero?
        gn_out = gn_gain * s_glot * (1.0 - random(2.0)) -
          gn_coeffs[3] * gn_del[3] -
          gn_coeffs[2] * gn_del[2] -
          gn_coeffs[1] * gn_del[1] -
          gn_coeffs[0] * gn_del[0]
        3.downto(1) do |j| gn_del[j] = gn_del[j - 1] end
        gn_del[0] = gn_out
      end
      glotsamp = glotsamp + gn_out * gn_table[int_loc]
    end
    # next tract tick
    lt[0] = dline1[2] + dline2[2]
    dline2[1] = dline2[2] + coeffs[1] * (glotsamp - dline2[2])
    temp = glotsamp + (dline2[1] - dline2[2])
    2.upto(noseposition - 1) do |j|
      dline2[j] = dline2[j + 1] + coeffs[j] * (dline1[j - 1] - dline2[j + 1])
      dline1[j - 1], temp = temp, dline1[j - 1] + (dline2[j] - dline2[j + 1])
    end
    jj = noseposition
    # next nasal tick
    plussamp = dline1[jj - 1]
    minussamp = dline2[jj + 1]
    nose_reftemp = 0.0
    if velum_pos.zero? and time_nose_closed >= nose_ring_time
      # nasal tick
      nose_reftemp = alpha[1] * plussamp + alpha[2] * minussamp + alpha[3] * nose2[1]
      nose_last_minus_refl = nose_reftemp - plussamp
      nose_last_plus_refl = nose_reftemp - minussamp
    else
      if velum_pos.nonzero?
        time_nose_closed = 0
      else
        time_nose_closed += 1
      end
      nose_reftemp = alpha[1] * plussamp + alpha[2] * minussamp + alpha[3] * nose2[1]
      plus_in = velum_pos * (nose_reftemp - nose2[1])
      nose_last_minus_refl = nose_reftemp - plussamp
      nose_last_plus_refl = nose_reftemp - minussamp
      nose_reftemp = nose_coeffs[1] * (plus_in - nose2[2])
      nose2[1] = nose2[2] + nose_reftemp
      nose_temp = plus_in + nose_reftemp
      2.upto(noselength - 2) do |j|
        nose_reftemp = nose_coeffs[j] * (nose1[j - 1] - nose2[j + 1])
        nose2[j] = nose2[j + 1] + nose_reftemp
        nose1[j - 1], nose_temp = nose_temp, nose1[j - 1] + nose_reftemp
      end
      nose_reftemp = nose_coeffs[noselength - 1] * (nose1[noselength - 2] - nose_last_output * 0.25)
      nose2[noselength - 1] = nose_last_output * 0.25 + nose_reftemp
      nose1[noselength - 1] = nose1[noselength - 2] + nose_reftemp
      nose1[noselength - 2] = nose_temp
      nose_filt1, nose_filt = nose_filt, nose1[noselength - 1]
      nose_last_output = (nose_filt + nose_filt1) * 0.5
    end
    dline2[jj] = nose_last_minus_refl
    dline1[jj - 1], temp = temp, nose_last_plus_refl
    (noseposition + 1).upto(tractlength - 2) do |j|
      dline2[j] = dline2[j + 1] + coeffs[j] * (dline1[j - 1] - dline2[j + 1])
      dline1[j - 1], temp = temp, dline1[j - 1] + (dline2[j] - dline2[j + 1])
    end
    dline2[tractlength - 1] = last_lip_refl +
      coeffs[tractlength - 1] * (dline1[tractlength - 2] - last_lip_refl)
    dline1[tractlength - 1] = dline1[tractlength - 2] + (dline2[tractlength - 1] - last_lip_refl)
    dline1[tractlength - 2] = temp
    if noise_gain.nonzero?
      noise_input = 1.0 - random(2.0)             # a guess
      3.downto(1) do |j|
        outz[j] = outz[j - 1]
      end
      outz[0] = noise_output
      noise_output = noise_input - inz2
      4.times do |j|
        noise_output = noise_output - noise_c[j] * outz[j]
      end
      inz2, inz1 = inz1, noise_input
      dline1[noise_pos] = dline1[noise_pos] + noise_output * noise_gain * s_noise
    end
    last_tract_plus = dline1[tractlength - 1] * lip_radius
    lt[1] = ltgain * (lt[0] + ltcoeff * lt[1])
    amp * (last_lip_out + nose_last_output + lt[1])
  end
end

Test_glt = [10, 0.65, 0.65]
Loud_glt = [13, 0.6, 0.6]
Soft_glt = [13, 0.65, 0.73]
Wide4_glt = [18, 0.534, 0.56]
Wide5_glt = [10, 0.65, 0.65]
Greekdefault_glt = [20, 0.65, 0.672472]
Lowbass_glt = [99, 0.5, 0.17737593]

Aa_shp = [8, 0.63110816, 0.94615144, 1.0756062, 0.9254686, 0.9928594, 0.98307705,
  1.4507878, 0.95167005, 0.9, -0.4, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
Hh2_shp = [8, 0.928177, 0.61326, 0.39779, 0.530387, 0.679558, 0.961326, 1.44199,
  1.09392, 0.7, -0.203125, 1.0, 0.0, 554.1667, 0.8, 2000.0, 0.772222, 0.0]
Dhh_shp = [8, 0.828729, 1.45856, 0.9882353, 0.662983, 0.9352941, 1.2529411, 0.40588236,
  1.1740758, 0.7, -0.140625, 7.0, 0.023333002, 3039.613, 0.691692, 1264.1677, 0.404788, 0.0]
Aah_shp = [8, 0.8214024, 0.7839217, 1.0981537, 0.9937591, 0.817757, 1.1907763, 1.3149668,
  1.0705689, 0.7, -0.4, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
Hhh_shp = [8, 0.928177, 0.61326, 0.39779, 0.530387, 0.679558, 0.961326, 1.44199, 1.09392,
  0.7, -0.203125, 1.0, 0.046296295, 554.1667, 0.8, 2000.0, 0.7722222, 0.0]
Ohh_shp = [8, 1.02762, 0.696133, 0.39779, 0.513812, 0.6371682, 1.4070797, 1.80663, 0.5044248,
  0.7, -0.2, 1.0, 0.0, 1000.0, 0.0, 0.0, 0.0, 0.0]
Ah_shp = [8, 0.7162393, 0.6389201, 0.8881412, 0.6060006, 1.293248, 1.4140776, 1.8503952,
  0.8622935, 0.9, -0.4, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
Oo_shp = [8, 0.46043858, 1.0865723, 0.33916336, 0.88724023, 0.9989101, 1.224445, 0.39867023,
  0.506609, 0.9, -0.4, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
Ahh_shp = [8, 0.928177, 0.779006, 0.629834, 0.629834, 1.02762, 1.65746, 1.62431, 0.944751,
  0.7, -0.45, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
Eem_shp = [8, 0.928177, 1.37569, 1.37569, 0.679558, 0.629834, 0.24817872, 0.56896555, 0.662983,
  0.7, -0.403125, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.09677419]
Hoo_shp = [8, 1.32597, 1.29282, 0.39779, 0.530387, 1.32597, 1.34254, 1.78182, 0.46408796,
  0.7, -0.4, 1.0, 0.031045755, 2215.7856, 0.82698005, 1026.6984, 0.96960765, 0.0]
Ooo_shp = [8, 1.32597, 1.29282, 0.39779, 0.530387, 1.32597, 1.34254, 1.78182, 0.464088,
  0.7, -0.4, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
Ee_shp = [8, 1.02, 1.637, 1.67, 1.558, 0.952, 0.501, 0.681, 0.675, 0.9, -0.4, 1.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
Ih_shp = [8, 0.72092783, 1.2719809, 1.3881364, 0.6532612, 0.7501422, 0.65654784, 0.8194081,
  0.6556785, 0.9, -0.4, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
Ee2_shp = [8, 0.9180887, 1.3481673, 1.3433423, 0.74573994, 0.593326, 0.5647744, 0.6692766,
  0.7419633, 0.7, -0.405254, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
Ihh_shp = [8, 0.7906788, 1.272475, 1.4089537, 0.68072784, 0.62673146, 0.7479623, 0.7506758,
  0.7054355, 0.9, -0.4, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
Open_shp = [8, 3.0, 3.0, 3.0, 3.0, 3.0, 3.0, 3.0, 3.0, 0.7, -0.45,
  1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0]
Thh_shp = [8, 0.828729, 1.45856, 0.9882353, 0.662983, 0.9352941, 1.2529411, 0.40588236,
  1.1740758, 0.7, -0.140625, 7.0, 0.101764, 3039.613, 0.691692, 1264.1677, 0.404788, 0.0]
Aw_shp = [8, 1.0525645, 0.643587, 0.935229, 0.4901642, 1.0743295, 1.1822895, 1.4161918,
  0.82537806, 0.9, -0.4, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
Eee_shp = [8, 0.928177, 1.37569, 1.37569, 0.679558, 0.629834, 0.646409, 0.56896555, 0.662983,
  0.7, -0.403125, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
Ttp_shp = [8, 0.928177, 0.779006, 0.629834, 0.629834, 1.02762, 0.18584079, 1.62431, 0.944751,
  0.7, -0.45, 6.0, 0.388889, 10514.583, 0.854335, 1315.2043, 0.280428, 0.0]
Aww_shp = [8, 1.02762, 0.696133, 0.563536, 0.513812, 0.977901, 1.37569, 1.80663, 0.712707,
  0.7, -0.2, 1.0, 0.0, 1000.0, 0.0, 0.0, 0.0, 0.0]
Eee2_shp = [8, 0.928177, 1.37569, 1.37569, 0.679558, 0.629834, 0.646409, 0.5117647, 0.662983,
  0.7, -0.203125, 7.3688526, 0.0, 5214.53, 0.975806, 0.0, 0.0, 0.0]
Jjj_shp = [8, 0.928177, 0.779006, 0.629834, 0.629834, 1.02762, 0.1592921, 1.1464338, 0.944751,
  0.7, -0.45, 6.0, 0.098039, 2315.7278, 0.7089554, 3066.7, 0.7983351, 0.0]
Ttt_shp = [8, 0.928177, 0.779006, 0.629834, 0.629834, 1.02762, 0.0, 1.62431, 0.944751,
  0.7, -0.45, 6.0, 0.388889, 10514.583, 0.854335, 1315.2043, 0.280428, 0.0]
Bb2_shp = [8, 1.0, 1.0, 0.46902645, 0.5486725, 0.65486723, 1.079646, 1.3982301, 0.0,
  0.7, -0.2, 8.0, 0.03, 500.0, 0.98, 0.0, 0.0, 0.0]
Eh_shp = [8, 0.7866194, 1.1630946, 1.2335452, 0.93186677, 0.94121367, 0.7586716, 1.3509308,
  0.8279036, 0.9, -0.4, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
Kkp_shp = [8, 0.8214024, 0.7839217, 1.0981537, 0.1592921, 1.061947, 1.1907763, 1.3149668,
  1.0705689, 0.7, -0.4, 4.0, 0.4, 2000.0, 0.93, 0.0, 0.0, 0.0]
Pipe1_shp = [8, 1.0, 1.0, 1.0, 0.7, 0.7, 0.7, 0.7, 0.7, 0.0, 0.0, 1.0, 0.0, 100.0,
  0.0, 0.0, 0.0, 0.0]
Tzz_shp = [8, 0.828729, 1.45856, 0.9882353, 0.662983, 0.9352941, 1.2529411, 0.40588236,
  1.1740758, 0.7, -0.140625, 7.0, 0.101764, 3039.613, 0.691692, 1264.1677, 0.404788, 0.0]
Bbb_shp = [8, 1.0, 1.0, 0.46902645, 0.5486725, 0.65486723, 1.079646, 1.3982301, 0.0,
  0.7, -0.2, 8.0, 0.03, 500.0, 0.98, 0.0, 0.0, 0.0]
Ehh_shp = [8, 0.682, 1.554, 1.581, 1.367, 1.315, 1.579, 0.843, 1.476, 0.7, -0.24507,
  1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
Kk2_shp = [8, 0.82140243, 0.7839217, 1.0981537, 0.0, 1.061947, 1.1907763, 1.3149668,
  1.0705689, 0.7, -0.4, 5.0, 0.01, 2000.0, 0.93, 0.0, 0.0, 0.0]
PpP_shp = [8, 1.0, 1.0, 0.3362832, 0.49557513, 0.7079646, 1.2389379, 1.1327434, 0.29203534,
  0.7, -0.2, 8.0, 0.040740736, 0.0, 0.89649165, 2082.2144, 0.8713607, 0.0]
Uhh_shp = [8, 0.928177, 0.61326, 0.39779, 0.530387, 0.679558, 0.961326, 1.44199, 1.09392,
  0.7, -0.203125, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
Big_shp = [8, 3.0, 3.0, 3.0, 3.0, 3.0, 3.0, 3.0, 3.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0]
Euu_shp = [8, 0.9285748, 1.3756071, 1.3747121, 0.6794088, 0.60398144, 0.43471563,
  0.8356653, 0.7158814, 0.7, -0.403122, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
Kkk_shp = [8, 0.8214024, 0.7839217, 1.0981537, 0.0, 1.061947, 1.1907763, 1.3149668, 1.0705689,
  0.7, -0.4, 4.0, 0.09444445, 2000.0, 0.93, 0.0, 0.0, 0.0]
Ppp_shp = [8, 1.0, 1.0, 0.3362832, 0.49557513, 0.7079646, 1.2389379, 1.1327434, 0.0,
  0.7, -0.2, 8.0, 0.05, 500.0, 0.98, 0.0, 0.0, 0.0]
Uu_shp = [8, 0.45291674, 1.0539645, 0.39576897, 0.8116293, 1.0510263, 1.1789232, 0.47529656,
  0.62563825, 0.9, -0.4, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
Fff_shp = [8, 0.93787295, 0.70496833, 0.8969878, 0.60815966, 0.9375178, 0.7412625, 1.1285298,
  0.2665695, 0.7, -0.202603, 8.0, 0.10341219, 8236.909, 0.945306, 79.28094, 0.498648, 0.0]
Ll2_shp = [8, 0.928177, 0.779006, 0.71772796, 0.807417, 1.02762, 1.65746, 0.36206907,
  0.86510503, 0.7, -0.258055, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.20806663]
Uuu_shp = [8, 0.55, 0.943094, 1.035, 0.434071, 1.14681, 1.487, 0.555, 0.656, 0.9, -0.4,
  1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
Lll_shp = [8, 0.928177, 0.779006, 0.7330638, 0.8156748, 1.02762, 1.65746, 0.3620689, 0.944751,
  0.7, -0.103125, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.21774194]
Rolledr_shp = [8, 0.3365169, 0.9244819, 1.0542682, 0.4485168, 1.0597233, 0.054845095,
  0.66896766, 0.8336522, 0.9, -0.4, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
Vvv_shp = [8, 0.9400966, 0.6775904, 0.88759726, 0.59890866, 0.9485658, 0.737778, 1.1542239,
  0.23893797, 0.7, -0.2, 8.0, 0.5, 8500.0, 0.95, 0.0, 0.5, 0.0]
Rolledrc_shp = [8, 0.928177, 0.779006, 0.629834, 0.629834, 1.02762, 0.0, 1.62431, 0.944751,
  0.7, -0.45, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
Mmm_shp = [8, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.7, -0.2,
  1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.503268]
Rolledro_shp = [8, 0.928177, 0.779006, 0.629834, 0.629834, 1.02762, 0.42477876, 1.62431,
  0.944751, 0.7, -0.45, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
Breath_shp = [8, 0.928177, 0.779006, 0.629834, 0.629834, 1.02762, 1.65746, 1.62431, 0.944751,
  0.7, -0.45, 1.0, 0.018518519, 2588.6013, 0.90612125, 812.6343, 0.9814815, 0.0]
Moo_shp = [8, 1.32597, 1.29282, 0.39779, 0.530387, 1.32597, 1.34254, 1.78182, 0.0,
  0.7, -0.4, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.30645162]
Rr2_shp = [8, 0.3365169, 0.9244819, 1.0542682, 0.4485168, 1.0597233, 0.71856207,
  0.66896766, 0.7274576, 0.9, -0.4, 1.0, 0.0, 0.0, 0.0, 0.0, 32.000004, 0.0]
Chh_shp = [8, 0.928177, 0.779006, 0.629834, 0.629834, 1.02762, 0.1592921, 1.1464338,
  0.944751, 0.7, -0.45, 6.0, 0.098039, 2315.7278, 0.7089554, 3066.7, 0.7983351, 0.0]
Gg2_shp = [8, 0.8214024, 0.4122405, 0.40788835, 0.0, 0.8495575, 0.7129002, 0.7308959,
  0.7785335, 0.7, -0.4, 4.0, 0.05, 2000.0, 0.9, 0.0, 0.0, 0.0]
Nng_shp = [8, 1.0, 1.0, 1.0333333, 0.0, 1.0, 0.99999994, 0.9568965, 1.3189656,
  0.7, -0.2, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0]
Rrr_shp = [8, 0.3365169, 0.9244819, 1.0542682, 0.4485168, 1.0597233, 0.71856207,
  0.66896766, 0.7274576, 0.9, -0.4, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
Wsp_shp = [8, 0.928177, 0.779006, 0.629834, 0.629834, 1.02762, 1.65746, 1.62431,
  0.944751, 0.7, -0.45, 1.0, 0.018518519, 0.0, 0.97, 0.0, 0.0, 0.0]
Ggg_shp = [8, 0.8214024, 0.7839217, 1.0981537, 0.0, 0.8495575, 0.7129002, 0.7308959,
  0.7785335, 0.7, -0.4, 4.0, 0.05, 2000.0, 0.9, 0.0, 0.0, 0.0]
Nnn_shp = [8, 1.0, 1.0, 1.0, 1.4579439, 1.0, 0.0, 0.9568965, 1.3189656,
  0.7, -0.2, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.503268]
Sh2_shp = [8, 0.828729, 1.45856, 0.9882353, 0.662983, 0.9352941, 1.2529411, 0.40588236,
  0.9882353, 0.7, -0.140625, 7.0, 0.0, 2451.5984, 0.928097, 2957.0518, 0.883636, 0.0]
Xx2_shp = [8, 0.928177, 1.37569, 1.37569, 0.8495575, 0.3451327, 0.646409, 0.56896555, 0.662983,
  0.7, -0.403125, 5.0, 0.022222, 2102.0833, 0.805556, 1735.4166, 0.759259, 0.0]
Dd2_shp = [8, 0.928177, 0.779006, 0.629834, 0.629834, 1.02762, 0.0, 0.72165513, 0.5996184,
  0.7, -0.45, 6.0, 0.02, 4851.6665, 0.953704, 2500.0, 0.966296, 0.0]
Ggg1_shp = [8, 0.8214024, 0.7839217, 1.0981537, 0.18584079, 1.061947, 1.1907763,
  1.3149668, 1.0705689, 0.7, -0.4, 4.0, 0.4, 2000.0, 0.9, 0.0, 0.0, 0.0]
Noisahh_shp = [8, 0.928177, 0.779006, 0.629834, 0.629834, 1.02762, 1.65746, 1.62431, 0.944751,
  0.7, -0.45, 1.0, 0.005, 0.0, 0.787037, 3777.0835, 0.759259, 0.0]
Shh_shp = [8, 0.828729, 1.45856, 0.9882353, 0.662983, 0.9352941, 1.2529411, 0.40588236,
  0.9882353, 0.7, -0.140625, 7.0, 0.023333, 2451.5984, 0.9280972, 2957.0518, 0.88363576, 0.0]
Xxx_shp = [8, 0.928177, 1.37569, 1.37569, 0.3451327, 0.6371682, 0.646409, 0.56896555, 0.662983,
  0.7, -0.403125, 4.0, 0.022222219, 2102.0833, 0.8055556, 612.5, 0.7592593, 0.0]
Ddd_shp = [8, 0.928177, 0.779006, 0.629834, 0.629834, 1.02762, 0.0, 0.72165513, 0.5996184,
  0.7, -0.45, 6.0, 0.02, 4851.6665, 0.953704, 2500.0, 0.966296, 0.0]
Gxx_shp = [8, 0.928177, 1.37569, 1.37569, 0.3451327, 0.6371682, 0.646409, 0.56896555, 0.662983,
  0.7, -0.403125, 4.0, 0.022222, 2102.0833, 0.805556, 612.5, 0.759259, 0.0]
None_shp = [8, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
Sss_shp = [8, 0.928177, 1.3588235, 1.3588235, 0.679558, 0.61764705, 0.63529414, 0.31764707,
  0.65294117, 0.7, -0.103125, 7.0, 0.105292, 1500.0, 0.916452, 4943.75, 0.97222227, 0.0]
Zzz_shp = [8, 0.928177, 1.3588235, 1.3588235, 0.679558, 0.61764705, 0.63529414, 0.31764707,
  0.65294117, 0.7, -0.103125, 7.0, 0.016, 1500.0, 0.9257112, 4943.75, 0.925926, 0.0]

=begin
with_sound do
  singer(0, 0.1, [
           [0.4, Ehh_shp, Test_glt, 523.0, 0.8, 0.0, 0.01],
           [0.6, Oo_shp, Test_glt, 523.0, 0.7, 0.1, 0.01]])
end
=end

=begin
with_sound do
  singer(0.2, 0.1,[
           [0.05, Ehh_shp, Test_glt, 523.0, 0.8, 0.0, 0.01],
           [0.15, Ehh_shp, Test_glt, 523.0, 0.8, 0.0, 0.01],
           [0.05, Kkk_shp, Test_glt, 523.0, 0.0, 0.0, 0.01],
           [0.05, Kkk_shp, Test_glt, 523.0, 0.0, 0.0, 0.01],
           [0.02, Kkp_shp, Test_glt, 523.0, 0.0, 1.0, 0.01],
           [0.08, Kkp_shp, Test_glt, 523.0, 0.0, 0.2, 0.01],
           [0.05, Ooo_shp, Test_glt, 523.0, 0.8, 0.0, 0.01],
           [0.15, Ooo_shp, Test_glt, 523.0, 0.8, 0.0, 0.01],
           [0.05, Eee_shp, Test_glt, 523.0, 0.8, 0.0, 0.01],
           [0.15, Eee_shp, Test_glt, 523.0, 0.8, 0.0, 0.01],
           [0.05, Ehh_shp, Test_glt, 523.0, 0.8, 0.0, 0.01],
           [0.15, Ehh_shp, Test_glt, 523.0, 0.8, 0.0, 0.01],
           [0.05, Mmm_shp, Test_glt, 523.0, 0.8, 0.0, 0.01],
           [0.15, Mmm_shp, Test_glt, 523.0, 0.8, 0.0, 0.01],
           [0.10, Mmm_shp, Test_glt, 523.0, 0.0, 0.0, 0.01]])
end
=end

# singer.rb ends here
