# maraca.rb -- maraca.ins -> maraca.scm -> maraca.rb

# Translator: Michael Scholz <mi-scholz@users.sourceforge.net>
# Created: Fri Apr 22 23:29:22 CEST 2005
# Changed: Sat Feb 19 18:27:34 CET 2011

# Commentary:
#
# Perry Cook's maraca from CMJ vol 21 no 3 (Fall 97) p 44 translated
# from CLM's maraca.ins
#
# Code:

def maraca(start, dur,
           amp = 0.1,
           sound_decay = 0.95,
           system_decay = 0.999,
           probability = 0.0625,
           shell_freq = 3200.0,
           shell_reso = 0.96)
  temp = 0.0
  shake_energy = 0.0
  snd_level = 0.0
  input = 0.0
  output = Vct.new(2)
  coeffs = Vct.new(2)
  num_beans = 64
  j = 0
  sndamp = amp / 16384.0
  srate4 = (mus_srate / 4.0).floor
  gain = ((log(num_beans) / log(4)) * 40.0) / num_beans
  coeffs[0] = -2.0 * shell_reso * cos(hz2radians(shell_freq))
  coeffs[1] = shell_reso * shell_reso
  run_instrument(start, dur) do
    if temp < TWO_PI
      temp += hz2radians(20)
      shake_energy += 1.0 - cos(temp)
    end
    if j == srate4
      temp = 0.0
      j = 0
    end
    j += 1
    shake_energy *= system_decay
    if random(1.0) < probability
      snd_level = snd_level + gain * shake_energy
    end
    input = snd_level * (random(2.0) - 1.0)
    snd_level *= sound_decay
    input = input - output[0] * coeffs[0] -  output[1] * coeffs[1]
    output[1], output[0] = output[0], input
    sndamp * (output[0] - output[1])
  end
end
# maraca: vct2channel(maraca(0, 5, 0.5))
# cabasa: vct2channel(maraca(0, 5, 0.5, 0.95, 0.997, 0.5, 3000.0, 0.7))

def big_maraca(start, dur,
               amp = 0.1,
               sound_decay = 0.95,
               system_decay = 0.999,
               probability = 0.0625,
               shell_freqs = [3200.0],
               shell_resos = [0.96],
               randiff = 0.01,
               with_filters = true)
  temp = 0.0
  temp1 = 0.0
  resn = shell_freqs.length
  shake_energy = 0.0
  snd_level = 0.0
  input = 0.0
  sum = 0.0
  last_sum = 0.0
  last_diff = 0.0
  diff = 0.0
  output = Vct.new(resn * 2)
  coeffs = Vct.new(resn * 2)
  basesf = Vct.new(resn)
  num_beans = 64
  j = 0
  sndamp = amp / 16384.0
  srate4 = (mus_srate / 4.0).floor
  gain = ((log(num_beans) / log(4)) * 40.0) / num_beans
  resn.times do |i|
    basesf[i] = coeffs[i * 2] = -2.0 * shell_resos[i] * cos(hz2radians(shell_freqs[i]))
    coeffs[1 + i * 2] = shell_resos[i] * shell_resos[i]
  end
  run_instrument(start, dur) do
    if temp < TWO_PI
      temp += hz2radians(20)
      shake_energy += 1.0 - cos(temp)
    end
    if j == srate4
      temp = 0.0
      j = 0
    end
    j += 1
    shake_energy *= system_decay
    if random(1.0) < probability
      snd_level = snd_level + gain * shake_energy
      basesf.each_with_index do |val, i|
        coeffs[i * 2] = val + (random(2.0 * randiff) - randiff)
      end
    end
    input = snd_level * (random(2.0) - 1.0)
    snd_level *= sound_decay
    temp1 = input
    last_sum = sum
    sum = 0.0
    resn.times do |i|
      input = temp1
      input = input - output[i * 2] * coeffs[i * 2] -  output[i * 2 + 1] * coeffs[i * 2 + 1]
      output[i * 2 + 1], output[i * 2] = output[i * 2], input
      sum += input
    end
    if with_filters
      last_diff, diff = diff, sum - last_sum
      temp1 = last_diff + diff
    else
      temp1 = sum
    end
    sndamp * temp1
  end
end
# tambourine: big_maraca(0, 1, 0.25, 0.95, 0.9985, 0.03125,
#                        [2300, 5600, 8100], [0.96, 0.995, 0.995], 0.01)
# sleighbells: big_maraca(0, 2, 0.5, 0.97, 0.9994, 0.03125,
#                        [2500, 5300, 6500, 8300, 9800], [0.999, 0.999, 0.999, 0.999, 0.999])
# sekere: big_maraca(0, 2, 0.5, 0.96, 0.999, .0625, [5500], [0.6])
# windchimes: big_maraca(0, 2, 0.5, 0.99995, 0.95, 0.001,
#                        [2200, 2800, 3400], [0.995, 0.995, 0.995], 0.01, false)

# maraca.rb ends here
