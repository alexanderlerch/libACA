#!/usr/bin/env ruby
# bess1.rb -- some examples from clm/rt.lisp and clm/bess5.cl

# Copyright (C) 2002--2009 Michael Scholz

# Author: Michael Scholz <mi-scholz@users.sourceforge.net>
# Created: Sun Sep 15 19:11:12 CEST 2002
# Changed: Tue Sep 29 02:05:49 CEST 2009

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

# Commentary:

# Requires sndlib.so and libxm.so!
#
# This file provides simple mono real time output to DAC.  Tempo,
# frequency, amplitude, and FM index can be controlled via sliders.
# The music algorithms are taken from clm/rt.lisp and clm/bess5.cl.
# 
# Bess.new.start -- starts a Motif widget with two DAC tests.
#
# Bess.new.start(:srate,       $clm_srate       # 22050
#                :bufsize,     $clm_rt_bufsize  # 128
#                :data_format, $clm_data_format # Mus_lshort
#                :which,       :agn             # :agn or :vct_test
#                :play,        false)

# Code:

def warn(*args)
  str = format(*args) << ($! ? ": #{$!}" : "") << "\n"
  str << (($@ and $DEBUG) ? "\n[#{$@.join("\n")}]" : "")
  $stdout.print str
  $! = nil
end

def die(*args)
  warn(*args)
  exit 1
end

def rbm_require(lib)
  puts "loading #{lib.inspect}" if $VERBOSE
  require lib.to_s
rescue ScriptError
  die "\aScriptError"
end

rbm_require "sndlib"
$output      = nil         # holds fd from mus_audio_open_output()
$clm_srate       = 22050
$clm_data_format = Mus_lshort
$clm_rt_bufsize  = 128

module Bess_utils
  def rbm_random(n)
    mus_random(n).abs
  end

  def get_args(args, key, val)
    if(key == :help and (args == key or args.member?(key) or args.assoc(key)))
      val = true
    elsif(args.member?(key))
      x = args[args.index(key) + 1]
      val = ((x == nil) ? val : x)
    elsif(args.assoc(key))
      val = (args.assoc(key)[1] rescue val)
    end
    val
  end

  def seconds2samples(sec)
    sr = (mus_srate() rescue $clm_srate)
    (sec * sr).round
  end

  def envelope_interp(*args)
    x = args[0]
    env = args[1]
    base = args[2]
    if (not env) or env.empty?
      0.0
    elsif x <= env[0] or env[2..-1].empty?
      env[1]
    elsif env[2] > x
      if env[1] == env[3] or (base and base == 0.0)
        env[1]
      elsif (not base) or base == 1.0
        env[1] + (x - env[0]) * ((env[3] - env[1]) / (env[2] - env[0]))
      else
        env[1] + ((env[3] - env[1]) / (base - 1.0)) *
                                     ((base ** ((x - env[0]) / (env[2] - env[0]))) - 1.0)
      end
    else
      envelope_interp(x, env[2..-1])
    end
  end

  include Math

  # simple violin, see snd/fm.html
  def make_rt_violin(dur = 1.0, freq = 440.0, amp = 0.3, *args)
    fm_index = get_args(args, :fm_index, 1.0)
    amp_env  = get_args(args, :amp_env, [0, 0, 25, 1, 75, 1, 100, 0])
    frq_scl = hz2radians(freq)
    maxdev = frq_scl * fm_index
    index1 = maxdev * (5.0 / log(freq))
    index2 = maxdev * 3.0 * ((8.5 - log(freq)) / (3.0 + freq / 1000.0))
    index3 = maxdev * (4.0 / sqrt(freq))
    carrier = make_oscil(:frequency, freq)
    fmosc1 = make_oscil(:frequency, freq)
    fmosc2 = make_oscil(:frequency, freq * 3.0)
    fmosc3 = make_oscil(:frequency, freq * 4.0)
    ampf = make_env(:envelope, amp_env, :scaler, amp, :duration, dur)
    indf1 = make_env(:envelope, [0, 1, 25, 0.4, 75, 0.6, 100, 0], :scaler, index1, :duration, dur)
    indf2 = make_env(:envelope, [0, 1, 25, 0.4, 75, 0.6, 100, 0], :scaler, index2, :duration, dur)
    indf3 = make_env(:envelope, [0, 1, 25, 0.4, 75, 0.6, 100, 0], :scaler, index3, :duration, dur)
    pervib = make_triangle_wave(:frequency, 0.5, :amplitude, 0.0025 *  frq_scl)
    ranvib = make_rand_interp(:frequency, 16.0, :amplitude, 0.005 * frq_scl)
    lambda do | |
      vib = triangle_wave(pervib) + rand_interp(ranvib)
      env(ampf) * oscil(carrier,
                        vib + env(indf1) * oscil(fmosc1, vib) +
                             env(indf2) * oscil(fmosc2, 3.0 * vib) +
                             env(indf3) * oscil(fmosc3, 4.0 * vib))
    end
  end
end

# class Agn is a simplified translation of clm/bess5.cl and
# clm/clm-example.lisp.
class Agn
  include Bess_utils
  
  def initialize
    @tempo = 0.25
    @amp = 1.0
    @freq = 1.0
    @index = 1.0
    @play = false
    @lim = 256
    @time = 60
    @octs = Array.new(@lim + 1) do |i| (4 + 2 * rbell(rbm_random(1.0))).floor end
    @rhys = Array.new(@lim + 1) do |i| (4 + 6 * rbm_random(1.0)).floor end
    @amps = Array.new(@lim + 1) do |i| (1 + 8 * rbell(rbm_random(1.0))).floor end
    @pits = Array.new(@lim + 1) do |i|
      [0, 0, 2, 4, 11, 11, 5, 6, 7, 9, 2, 0, 0].at((12 * rbm_random(1.0)).floor)
    end
    @begs = Array.new(@lim + 1) do |i|
      if rbm_random(1.0) < 0.9
        (4 + 2 * rbm_random(1.0)).floor
      else
        (6 * rbm_random(4.0)).floor
      end
    end
  end

  # called by XtAppAddWorkProc
  def rt_send2dac(func)
    if @play
      mus_audio_write($output, vct2sound_data(vct_map!(make_vct($clm_rt_bufsize), func.call),
                                                  make_sound_data(1, $clm_rt_bufsize), 0),
                      $clm_rt_bufsize)
      false
    else
      mus_audio_close($output)
      $output = nil
      true
    end
  end
  
  # see clm/rt.lisp
  def make_vct_test(*args)
    srate       = get_args(args, :srate, $clm_srate)
    bufsize     = get_args(args, :bufsize, $clm_rt_bufsize)
    data_format = get_args(args, :data_format, $clm_data_format)
    $clm_srate = set_mus_srate(srate).to_i
    $clm_rt_bufsize = bufsize
    $output = mus_audio_open_output(Mus_audio_default, srate, 1, data_format, bufsize * 2)
    mode = [0, 12, 2, 4, 14, 4, 5, 5, 0, 7, 7, 11, 11]
    pits = Array.new(@lim + 1) do rbm_random(12.0).floor end
    begs = Array.new(@lim + 1) do 1 + rbm_random(3.0).floor end
    cellbeg, cellsiz, cellctr = 0, 6, 0
    func = nil
    len = dur = 0
    lambda do | |
      if len > 1
        len -= 1
      else
        dur = @tempo * begs[cellctr + 1]
        cellctr += 1
        if cellctr > (cellsiz + cellbeg)
          cellbeg += 1 if rbm_random(1.0) > 0.5
          cellsiz += 1 if rbm_random(1.0) > 0.5
          cellctr = cellbeg
        end
        func = make_rt_violin(dur, @freq * 16.351 * 16 * 2 ** (mode[pits[cellctr]] / 12.0),
                              @amp * 0.3, :fm_index, @index)
        len = (seconds2samples(dur) / bufsize).ceil
      end
      func
    end
  end
  
  def tune(x)
    [1.0, 256.0 / 243, 9.0 / 8, 32.0 / 27, 81.0 / 64,
     4.0 / 3, 1024.0 / 729, 3.0 / 2, 128.0 / 81, 27.0 / 16,
     16.0 / 9, 243.0 / 128, 2.0].at(x % 12) * 2 ** x.divmod(12).first
  end

  def rbell(x)
    envelope_interp(x * 100, [0, 0, 10, 0.25, 90, 1.0, 100, 1.0])
  end

  # see clm/bess5.cl
  def make_agn(*args)
    srate       = get_args(args, :srate, $clm_srate)
    bufsize     = get_args(args, :bufsize, $clm_rt_bufsize)
    data_format = get_args(args, :data_format, $clm_data_format)
    $clm_srate = set_mus_srate(srate).to_i
    $clm_rt_bufsize = bufsize
    $output = mus_audio_open_output(Mus_audio_default, srate, 1, data_format, bufsize * 2)
    die("can't open DAC (%s)", $output.inspect) if $output < 0
    wins = [[0, 0, 40, 0.1, 60, 0.2, 75, 0.4, 82, 1, 90, 1, 100, 0],
            [0, 0, 60, 0.1, 80, 0.2, 90, 0.4, 95, 1, 100, 0],
            [0, 0, 10, 1, 16, 0, 32, 0.1, 50, 1, 56, 0, 60, 0, 90, 0.3, 100, 0],
            [0, 0, 30, 1, 56, 0, 60, 0, 90, 0.3, 100, 0],
            [0, 0, 50, 1, 80, 0.3, 100, 0],
            [0, 0, 40, 0.1, 60, 0.2, 75, 0.4, 82, 1, 90, 1, 100, 0],
            [0, 0, 40, 0.1, 60, 0.2, 75, 0.4, 82, 1, 90, 1, 100, 0],
            [0, 0, 10, 1, 32, 0.1, 50, 1, 90, 0.3, 100, 0],
            [0, 0, 60, 0.1, 80, 0.3, 95, 1, 100, 0],
            [0, 0, 80, 0.1, 90, 1, 100, 0]]
    cellbeg, cellsiz, cellctr, whichway = 0, 4, 0, 1
    nextbeg = beg = 0.0
    func = nil
    len = dur = 0
    lambda do | |
      if len > 1
        len -= 1
      else
        beg += nextbeg
        nextbeg += [0.025, @tempo * (0.95 + rbm_random(0.1)) * @begs[cellctr]].max
        dur = [0.025, @tempo * (0.85 + rbm_random(0.1)) * @rhys[cellctr]].max
        freq = @freq * 16.351 * tune(@pits[cellctr]) * 2 ** @octs[cellctr]
        dur += dur if freq < 100
        ampl = @amp * 10 * [0.003, @amps[cellctr] * 0.01].max
        ind = @index * rbm_random(1.0) * 3.0
        cellctr += 1
        if cellctr > (cellsiz + cellbeg)
          cellbeg += 1
          if rbm_random(1.0) > 0.5
            cellsiz += whichway
          end
          if cellsiz > 10 and rbm_random(1.0) > 0.99
            whichway = -2
            if cellsiz > 6 and rbm_random(1.0) > 0.999
              whichway = -1
              if cellsiz < 4
                whichway = 1
              end
            end
          end
          nextbeg += rbm_random(1.0)
          cellctr = cellbeg
        end
        func = make_rt_violin(dur, freq, ampl, :fm_index, ind,
                              :amp_env, wins[(10 * (beg - beg.floor)).floor])
        len = (seconds2samples(dur) / bufsize).ceil
      end
      func
    end
  end
end

class Bess < Agn
  rbm_require "libxm"
  
  def initialize
    super
    @sliderback = "lightsteelblue"
    @background = "lightsteelblue1"
    @which = @proc = nil
    @shell_app = @form = nil
    @tl = @ts = @fl = @fs = @al = @as = @il = @is = nil
    1.upto(15) do |i|
      trap(i) do |sig|
        puts "\nSignal #{sig} received.  Process #{$$} canceled."
        RXtRemoveWorkProc(@proc) if @proc
        exit 0
      end
    end
  end
  
  def get_color(color)
    col = RXColor()
    dpy = RXtDisplay(@shell_app[0])
    cmap = RDefaultColormap(dpy, RDefaultScreen(dpy))
    warn("Can't allocate #{color.inspect}!") if RXAllocNamedColor(dpy, cmap, color, col, col).zero?
    Rpixel(col)
  end

  def set_label(wid, *args)
    RXtVaSetValues(wid, [RXmNlabelString, RXmStringCreate(format(*args), RXmFONTLIST_DEFAULT_TAG)])
  end

  def make_label(wid, name)
    RXtCreateManagedWidget(name, RxmLabelWidgetClass, @form,
                           [RXmNleftAttachment, RXmATTACH_FORM,
                            RXmNbottomAttachment, RXmATTACH_NONE,
                            RXmNtopAttachment, RXmATTACH_WIDGET,
                            RXmNtopWidget, wid,
                            RXmNrightAttachment, RXmATTACH_NONE,
                            RXmNalignment, RXmALIGNMENT_END,
                            RXmNrecomputeSize, false,
                            RXmNbackground, get_color(@background)])
  end
  
  def make_scale_label(wid)
    RXtCreateManagedWidget("label", RxmLabelWidgetClass, @form,
                           [RXmNleftAttachment, RXmATTACH_WIDGET,
                            RXmNleftWidget, wid,
                            RXmNbottomAttachment, RXmATTACH_NONE,
                            RXmNtopAttachment, RXmATTACH_OPPOSITE_WIDGET,
                            RXmNtopWidget, wid,
                            RXmNrightAttachment, RXmATTACH_NONE,
                            RXmNbackground, get_color(@background)])
  end
  
  def make_scale(wid)
    RXtCreateManagedWidget("scale", RxmScaleWidgetClass, @form,
                           [RXmNleftAttachment, RXmATTACH_WIDGET,
                            RXmNleftWidget, wid,
                            RXmNbottomAttachment, RXmATTACH_NONE,
                            RXmNtopAttachment, RXmATTACH_OPPOSITE_WIDGET,
                            RXmNtopWidget, wid,
                            RXmNrightAttachment, RXmATTACH_FORM,
                            RXmNshowValue, false,
                            RXmNorientation, RXmHORIZONTAL,
                            RXmNheight, 20,
                            RXmNbackground, get_color(@sliderback)])
  end

  # return label and scale widget
  def make_scales(wid, name, val, callback)
    label = make_scale_label(make_label(wid, name))
    scale = make_scale(label)
    set_label(label, val.kind_of?(Integer) ? "%4d" : "%4.3f", val)
    RXtAddCallback(scale, RXmNdragCallback, callback, label)
    RXtAddCallback(scale, RXmNvalueChangedCallback, callback ,label)
    [label, scale]
  end

  def do_play(*args)
    if @play
      case @which
      when :agn
        func = make_agn(*args)
      when :vct_test
        func = make_vct_test(*args)
      else
        func = make_agn(*args)
      end
      @proc = RXtAppAddWorkProc(@shell_app[1], lambda do |c| rt_send2dac(func) end)
    else
      RXtRemoveWorkProc(@proc) if @proc
    end
  end

  def set_defaults(parent)
    @tempo = 0.25
    @amp = 1.0
    @freq = 1.0
    @index = 1.0
    low_tempo = 0.05
    high_tempo = 0.5
    low_freq = 0.1
    high_freq = 4.0
    high_index = 2.0
    set_label(@tl, "%4.3f", @tempo)
    RXmScaleSetValue(@ts, (100 * (@tempo - low_tempo) / (high_tempo - low_tempo)).round)
    set_label(@fl, "%4.3f", @freq)
    RXmScaleSetValue(@fs, (100 * (@freq - low_freq) / (high_freq - low_freq)).round)
    set_label(@al, "%4.3f", @amp)
    RXmScaleSetValue(@as, (100 * @amp).round)
    set_label(@il, "%4.3f", @index)
    RXmScaleSetValue(@is, (100 * (@index / high_index)).round)
  end
  
  def start(*args)
    @play  = get_args(args, :play, false)
    @which = get_args(args, :which, :agn)
    # rest args are going to make_vct_test() or make_agn()
    cargs = [$0] + $*
    @shell_app = RXtVaOpenApplication("FM", cargs.length, cargs, RapplicationShellWidgetClass,
                                      [RXmNallowShellResize, true, RXmNtitle, "FM forever!"])
    RXtAddEventHandler(@shell_app[0], 0, true,
                       lambda do |w, c, i, f| R_XEditResCheckMessages(w, c, i, f) end)
    @form = RXtCreateManagedWidget("form", RxmFormWidgetClass, @shell_app[0],
                                   [RXmNresizePolicy, RXmRESIZE_GROW,
                                    RXmNbackground, get_color(@background)])
    play = RXtCreateManagedWidget("play", RxmToggleButtonWidgetClass, @form,
                                  [RXmNtopAttachment, RXmATTACH_FORM,
                                   RXmNleftAttachment, RXmATTACH_FORM,
                                   RXmNrightAttachment, RXmATTACH_NONE,
                                   RXmNbottomAttachment, RXmATTACH_NONE,
                                   RXmNbackground, get_color(@background)])
    radio = RXmCreateRadioBox(@form, "radio",
                              [RXmNorientation, RXmHORIZONTAL,
                               RXmNtopAttachment, RXmATTACH_FORM,
                               RXmNleftAttachment, RXmATTACH_WIDGET,
                               RXmNleftWidget, play,
                               RXmNrightAttachment, RXmATTACH_NONE,
                               RXmNbottomAttachment, RXmATTACH_NONE,
                               RXmNbackground, get_color(@background)])
    p_agn = RXtCreateManagedWidget("agn", RxmToggleButtonWidgetClass, radio,
                                   [RXmNtopAttachment, RXmATTACH_FORM,
                                    RXmNleftAttachment, RXmATTACH_FORM,
                                    RXmNrightAttachment, RXmATTACH_NONE,
                                    RXmNbottomAttachment, RXmATTACH_NONE,
                                    RXmNbackground, get_color(@background)])
    p_test = RXtCreateManagedWidget("test", RxmToggleButtonWidgetClass, radio,
                                    [RXmNtopAttachment, RXmATTACH_FORM,
                                     RXmNleftAttachment, RXmATTACH_WIDGET,
                                     RXmNleftWidget, p_agn,
                                     RXmNrightAttachment, RXmATTACH_NONE,
                                     RXmNbottomAttachment, RXmATTACH_NONE,
                                     RXmNbackground, get_color(@background)])
    quit = RXtCreateManagedWidget(" quit ", RxmPushButtonWidgetClass, @form,
                                  [RXmNtopAttachment, RXmATTACH_FORM,
                                   RXmNleftAttachment, RXmATTACH_WIDGET,
                                   RXmNleftWidget, radio,
                                   RXmNrightAttachment, RXmATTACH_FORM,
                                   RXmNbottomAttachment, RXmATTACH_NONE,
                                   RXmNbackground, get_color(@background)])
    sep = RXtCreateManagedWidget("sep", RxmSeparatorWidgetClass, @form,
                           [RXmNleftAttachment, RXmATTACH_FORM,
                            RXmNbottomAttachment, RXmATTACH_NONE,
                            RXmNtopAttachment, RXmATTACH_WIDGET,
                            RXmNtopWidget, radio,
                            RXmNrightAttachment, RXmATTACH_FORM,
                            RXmNheight, 4,
                            RXmNorientation, RXmHORIZONTAL])
    RXmToggleButtonSetState(play, @play, true)
    RXtAddCallback(play, RXmNvalueChangedCallback,
                   lambda do |w, c, i|
                     @play = Rset(i)
                     set_defaults(sep) if @play
                     do_play(*args)
                   end)
    RXmToggleButtonSetState(p_agn, @which == :agn, true)
    RXtAddCallback(p_agn, RXmNvalueChangedCallback,
                   lambda do |w, c, i|
                     @which = c if Rset(i)
                     @play = false
                     RXmToggleButtonSetState(play, @play, true)
                   end, :agn)
    RXmToggleButtonSetState(p_test, @which == :vct_test, true)
    RXtAddCallback(p_test, RXmNvalueChangedCallback,
                   lambda do |w, c, i|
                     @which = c if Rset(i)
                     @play = false
                     RXmToggleButtonSetState(play, @play, true)
                   end, :vct_test)
    RXtAddCallback(quit, RXmNactivateCallback,
                   lambda do |w, c, i|
                     RXtRemoveWorkProc(@proc) if @proc
                     exit 0
                   end)
    low_tempo = 0.05
    high_tempo = 0.5
    low_freq = 0.1
    high_freq = 4.0
    high_index = 2.0
    @tl, @ts = make_scales(sep, " tempo:", @tempo,
                           lambda do |w, c, i|
                             @tempo = low_tempo + Rvalue(i) * (high_tempo - low_tempo) * 0.01
                             set_label(c, "%4.3f", @tempo)
                           end)
    RXmScaleSetValue(@ts, (100 * (@tempo - low_tempo) / (high_tempo - low_tempo)).round)
    @fl, @fs = make_scales(@ts, "  freq:", @freq,
                           lambda do |w, c, i|
                             @freq = low_freq + Rvalue(i) * ((high_freq - low_freq) * 0.01)
                             set_label(c, "%4.3f", @freq)
                           end)
    RXmScaleSetValue(@fs, (100 * (@freq - low_freq) / (high_freq - low_freq)).round)
    @al, @as = make_scales(@fs, "   amp:", @amp,
                           lambda do |w, c, i|
                             @amp = Rvalue(i) * 0.01
                             set_label(c, "%4.3f", @amp)
                           end)
    RXmScaleSetValue(@as, (100 * @amp).round)
    @il, @is = make_scales(@as, " index:", @index,
                           lambda do |w, c, i|
                             @index = Rvalue(i) * high_index * 0.01
                             set_label(c, "%4.3f", @index)
                           end)
    RXmScaleSetValue(@is, (100 * (@index / high_index)).round)
    do_play(*args)
    RXtManageChild(radio)
    RXtRealizeWidget(@shell_app[0])
    RXtAppMainLoop(@shell_app[1])
  end
end

begin
  # Bess.new.start(:srate, $clm_srate,
  #                :bufsize, $clm_rt_bufsize,
  #                :data_format, $clm_data_format,
  #                :which, :agn,
  #                :play, false)
  Bess.new.start
end

# bess1.rb ends here
