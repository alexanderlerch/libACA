#!/usr/bin/env ruby
# agn.rb -- Bill Schottstaedt's agn.cl 
#     (see clm-2/clm-example.clm and clm-2/bess5.cl)

# Translator/Author: Michael Scholz <mi-scholz@users.sourceforge.net>
# Created: Sat May 24 20:35:03 CEST 2003
# Changed: Sat Jul 28 00:37:44 CEST 2012

# Type do_agn
# or start the script in a shell.

$clm_c_version = true

require "sndlib"
if $clm_c_version
  require "sndins"
else
  require "v"		# fm_violin_rb, jc_reverb_rb
  require "clm-ins"	# nrev_rb
  class Instrument
    alias fm_violin fm_violin_rb
    alias jc_reverb jc_reverb_rb
    alias nrev nrev_rb 
  end
end
require "clm"
require "ws"
require "env"

$clm_play	= true
$clm_statistics	= true
$clm_verbose	= true
$clm_srate	= 44100
$clm_channels	= 2
$clm_reverb	= :jc_reverb
$clm_reverb_data = [:volume, 0.8]
$clm_reverb_channels = 2
$clm_delete_reverb = true

class Agn
  include Math
  include Env
  Limit = 256
  Time  = 60
  
  def initialize
    mode = [0, 0, 2, 4, 11, 11, 5, 6, 7, 0, 0, 0, 0]
    @octs = make_array(Limit) do |i| (4 + 2 * rbell(random(1.0))).floor end
    @pits = make_array(Limit) do |i| mode[(12 * random(1.0)).floor] end
    @rhys = make_array(Limit) do |i| (4 + 6 * random(1.0)).floor end
    @amps = make_array(Limit) do |i| (1 + 8 * rbell(random(1.0))).floor end
  end
  
  def tune(x)
    [1.0, 256.0 / 243, 9.0 / 8, 32.0 / 27, 81.0 / 64,
     4.0 / 3, 1024.0 / 729, 3.0 / 2, 128.0 / 81, 27.0 / 16,
     16.0 / 9, 243.0 / 128, 2.0].at(x % 12) * 2 ** x.divmod(12).first
  end

  def rbell(x)
    envelope_interp(x * 100, [0, 0, 10, 0.25, 90, 1.0, 100, 1.0])
  end

  def agn(file)
    File.open(file, "w") do |f|
      f << "# from agn.cl (see clm-2/clm-example.clm and clm-2/bess5.cl)\n"
      f << "#\n"
      f << make_default_comment() << "\n\n"
      wins = [[0, 0, 40, 0.1, 60, 0.2, 75, 0.4, 82, 1, 90, 1, 100, 0],
              [0, 0, 60, 0.1, 80, 0.2, 90, 0.4, 95, 1, 100, 0],
              [0, 0, 10, 1, 16, 0, 32, 0.1, 50, 1, 56, 0, 60, 0, 90, 0.3,100,0],
              [0, 0, 30, 1, 56, 0, 60, 0, 90, 0.3, 100, 0],
              [0, 0, 50, 1, 80, 0.3, 100, 0],
              [0, 0, 40, 0.1, 60, 0.2, 75, 0.4, 82, 1, 90, 1, 100, 0],
              [0, 0, 40, 0.1, 60, 0.2, 75, 0.4, 82, 1, 90, 1, 100, 0],
              [0, 0, 10, 1, 32, 0.1, 50, 1, 90, 0.3, 100, 0],
              [0, 0, 60, 0.1, 80, 0.3, 95, 1, 100, 0],
              [0, 0, 80, 0.1, 90, 1, 100, 0]]
      (1..3).each do |i|
        cellbeg, cellsiz, cellctr = 0, 4, 0
        whichway, base, mi, winnum, mytempo = 1, i, i - 1, 0, 0.2
        nextbeg = revamt = ranamt = beg = dur = freq = ampl = ind = 0.0
        while beg < Time and cellctr < Limit
          beg += nextbeg
          nextbeg = dur = [0.25,
	    mytempo * (0.9 + 0.2 * random(1.0)) * @rhys[cellctr]].max
          freq = (16.352 / 2 ** mi) * tune(@pits[cellctr]) * 2 ** @octs[cellctr]
          dur += dur if freq < 100
          ampl = [0.003, @amps[cellctr] * (1.0 / (60 * base))].max
          ind = random(1.0) * 2 * base
          revamt = base * 0.1
          winnum = (10 * (beg - beg.floor)).floor
          ranamt = 0.00001 * (logn(freq, 2.0) - 4) ** 4
          f << format("\
fm_violin(%f, %f, %f, %f, :fm_index, %f,
  :amp_env, %s,
  :reverb_amount, %f, :noise_amount, %f)\n",
            beg, dur, freq, ampl, ind,
	    wins[winnum].inspect, revamt, ranamt)
          cellctr += 1
          if cellctr > (cellsiz + cellbeg)
            cellbeg += 1
            if random(1.0) > 0.5
              cellsiz += whichway
            end
            if cellsiz > 16 and random(1.0) > 0.99
              whichway = -2
              if cellsiz > 12 and random(1.0) > 0.999
                whichway = -1
                if cellsiz < 4
                  whichway = 1
                end
              end
            end
            cellbeg += 3
            cellctr = cellbeg
          end
        end
      end
      f << "\n# " + file + " ends here\n"
    end
    file
  end
end

def do_agn(file = "agn.rbm")
  sndfile = File.basename(file, ".*") + ".snd"
  message("Writing %s", file.inspect)
  Agn.new.agn(file)
  clm_load(file, :clm, true, :output, sndfile)
end

unless provided?(:snd)
  do_agn((ARGV[0] or "agn.rbm"))
end

# agn.rb ends here
