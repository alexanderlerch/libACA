#!/usr/local/bin/ruby -wd
# bess -- Translation of Bill Schottstaedt's bess.scm to Ruby.

# Copyright (c) 2002--2009 Michael Scholz <mi-scholz@users.sourceforge.net>
# All rights reserved.
# 
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
# 1. Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in the
#    documentation and/or other materials provided with the distribution.
# 
# THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
# ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
# OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
# HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
# OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
# SUCH DAMAGE.

begin require 'rubygems' rescue LoadError end

file = File.basename __FILE__
banner = "This is #{file.upcase} v1.9, (C) 2002--2009 Michael Scholz"

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
  require lib.to_s
rescue ScriptError
  die "\aScriptError"
end

1.upto(15) do |i|
  trap(i) do |sig| die("\nSignal #{sig} received.  Process #{$$} canceled.") end
end

class Bess
  def initialize(banner, file)
    @bufsize = 256
    @srate = 22050
    @chans = 1
    @play = 0.0
    @freq = 220.0
    @fm_index1 = 1.0
    @fm_ratio1 = 1
    @amp = 0.5
    @sliderback = "lightsteelblue"
    @background = "lightsteelblue1"
    @low_freq = 40.0
    @high_freq = 2000.0
    @high_index1 = 3.0
    @high_ratio = 10
    get_options(banner, file)
    rbm_require(:libxm)
    rbm_require(:sndlib)
    set_mus_srate(@srate)
  end

  def get_options(banner, file)
    vers = false
    help = false
    vers_msg = "#{banner}
#{file.capitalize} comes with ABSOLUTELY NO WARRANTY.
You may redistribute copies of #{file.capitalize}
under the terms of the GNU General Public License.
For more information about these matters, see the file named COPYING."
    help_msg = "#{banner}
#{file.capitalize} is a Ruby script working with sndlib.so and
libxm.so which must be in the Ruby library path, for example in
/usr/local/lib/ruby/site_ruby, or the environment variable $RUBYLIB
must be set correctly. It opens the DAC, creates some scale widgets,
and starts two CLM oscillators doing frequency modulation in
semi-real-time. This is a translation of bess.scm of Bill
Schottstaedt\'s Snd sound editor.

Default values shown in brackets.

Usage: #{file} [ options ] [ -- X options ]

   -p, --play               play immediately (#{@play.nonzero? ? "yes" : "no"})

   -f, --frequency NUMBER   frequency between #{@low_freq} and #{@high_freq} (#{@freq})
   -i, --index1 NUMBER      fm_index1 between 0 and #{@high_index1} (#{@fm_index1})
   -r, --ratio NUMBER       ratio between 0 and #{@high_ratio} (#{@fm_ratio1})
   -a, --amplitude NUMBER   amplitude between 0 and 1 (#{@amp})

   -B, --bufsize NUMBER     buffer size (#{@bufsize})
   -S, --srate NUMBER       sampling rate (#{@srate})
   -C, --channels NUMBER    number of channels (#{@chans})

   -b  --background COLOR   background color (#{@background})
   -s, --sliderback COLOR   slider background color (#{@sliderback})

   -V, --version            display version information and exit
   -h, --help               display this help message and exit

Example: #{file} -pf1000 -r3 -b ivory1 -s ivory3"
    rbm_require "getoptlong"
    GetoptLong.new(["--play", "-p", GetoptLong::NO_ARGUMENT],
                   ["--frequency", "-f", GetoptLong::REQUIRED_ARGUMENT],
                   ["--index1", "-i", GetoptLong::REQUIRED_ARGUMENT],
                   ["--ratio", "-r", GetoptLong::REQUIRED_ARGUMENT],
                   ["--amplitude", "-a", GetoptLong::REQUIRED_ARGUMENT],
                   ["--bufsize", "-B", GetoptLong::REQUIRED_ARGUMENT],
                   ["--srate", "-S", GetoptLong::REQUIRED_ARGUMENT],
                   ["--channels", "-C", GetoptLong::REQUIRED_ARGUMENT],
                   ["--background", "-b", GetoptLong::REQUIRED_ARGUMENT],
                   ["--sliderback", "-s", GetoptLong::REQUIRED_ARGUMENT],
                   ["--version", "-V", GetoptLong::NO_ARGUMENT],
                   ["--help", "-h", GetoptLong::NO_ARGUMENT]).each do |name, arg|

      case name
      when "--play"
        @play = 1.0
      when "--frequency"
        @freq = arg.to_f.abs
        @freq = @freq < @low_freq ? @low_freq : @freq > @high_freq ? @high_freq : @freq
      when "--index1"
        ind = arg.to_f.abs
        @fm_index1 = ind > @high_index1 ? @high_index1 : ind
      when "--ratio"
        rat = arg.to_i.abs
        @fm_ratio1 = rat > @high_ratio ? @high_ratio : rat
      when "--amplitude"
        amp = arg.to_f.abs
        @amp = amp > 1 ? 1 : amp
      when "--bufsize"
        @bufsize = arg.to_i
      when "--srate"
        @srate = arg.to_i
      when "--channels"
        @chans = arg.to_i
      when "--sliderback"
        @sliderback = arg
      when "--background"
        @background = arg
      when "--version"
        vers = true
      when "--help"
        help = true
      end
    end
    die help_msg if help
    die vers_msg if vers
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
                          RXmNwidth, 80,          #114,
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
                            RXmNbackground, get_color(@sliderback)])
  end
  
  def make_scales(wid, name, val, callback)
    label = make_scale_label(make_label(wid, name))
    scale = make_scale(label)
    set_label(label, val.kind_of?(Integer) ? "%8d" : "%8.3f", val)
    RXtAddCallback(scale, RXmNdragCallback, callback, label)
    RXtAddCallback(scale, RXmNvalueChangedCallback, callback ,label)
    scale
  end
  
  def start_dac(&body)
    args = [$0] + $*
    @shell_app = RXtVaOpenApplication("FM", args.length, args, RapplicationShellWidgetClass,
                                      [RXmNallowShellResize, true, RXmNtitle, "FM forever!"])
    RXtAddEventHandler(@shell_app[0], 0, true,
                       lambda do |w, c, i, f| R_XEditResCheckMessages(w, c, i, f) end)
    @form = RXtCreateManagedWidget("form", RxmFormWidgetClass, @shell_app[0],
                                  [RXmNresizePolicy, RXmRESIZE_GROW,
                                   RXmNbackground, get_color(@background)])
    play_button = RXtCreateManagedWidget("play", RxmToggleButtonWidgetClass, @form,
                                         [RXmNtopAttachment, RXmATTACH_FORM,
                                          RXmNleftAttachment, RXmATTACH_FORM,
                                          RXmNrightAttachment, RXmATTACH_NONE,
                                          RXmNbottomAttachment, RXmATTACH_NONE,
                                          RXmNbackground, get_color(@background)])
    RXmToggleButtonSetState(play_button, @play.nonzero? ? true : false, false)
    RXtAddCallback(play_button, RXmNvalueChangedCallback,
                   lambda do |w, c, i| @play = Rset(i) ? 1.0 : 0.0 end)
    quit_button = RXtCreateManagedWidget(" quit ", RxmPushButtonWidgetClass, @form,
                                         [RXmNtopAttachment, RXmATTACH_FORM,
                                          RXmNleftAttachment, RXmATTACH_NONE,
                                          RXmNrightAttachment, RXmATTACH_FORM,
                                          RXmNbottomAttachment, RXmATTACH_NONE,
                                          RXmNbackground, get_color(@background)])
    RXtAddCallback(quit_button, RXmNactivateCallback, lambda do |w, c, i| exit(0) end)
    wid = make_scales(play_button, "  carrier:", @freq,
                      lambda do |w, c, i|
                        @freq = @low_freq + Rvalue(i) * ((@high_freq - @low_freq) / 100.0)
                        set_label(c, "%8.3f", @freq)
                      end)
    RXmScaleSetValue(wid, (100 * (@freq - @low_freq) / (@high_freq - @low_freq)).round)
    wid = make_scales(wid, " amplitude:", @amp,
                      lambda do |w, c, i|
                        @amp = Rvalue(i) / 100.0
                        set_label(c, "%8.3f", @amp)
                      end)
    RXmScaleSetValue(wid, (100 * @amp).round)
    wid = make_scales(wid, "fm index 1:", @fm_index1,
                      lambda do |w, c, i|
                        @fm_index1 = Rvalue(i) * (@high_index1 / 100.0)
                        set_label(c, "%8.3f", @fm_index1)
                      end)
    RXmScaleSetValue(wid, (100 * @fm_index1 / @high_index1).round)
    wid = make_scales(wid, "c/m ratio 1:", @fm_ratio1,
                      lambda do |w, c, i|
                        @fm_ratio1 = (Rvalue(i) * (@high_ratio / 100.0)).round
                        set_label(c, "%8d", @fm_ratio1)
                      end)
    RXmScaleSetValue(wid, (@fm_ratio1 * 100 / @high_ratio).round)
    if defined? @fm_index2
      wid = make_scales(wid, "fm index 2:", @fm_index2,
                        lambda do |w, c, i|
                          @fm_index2 = Rvalue(i) * (@high_index2 / 100.0)
                          set_label(c, "%8.3f", @fm_index2)
                        end)
      RXmScaleSetValue(wid, (100 * @fm_index2 / @high_index2).round)
      wid = make_scales(wid, "c/m ratio 2:", @fm_ratio2,
                        lambda do |w, c, i|
                          @fm_ratio2 = (Rvalue(i) * (@high_ratio / 100.0)).round
                          set_label(c, "%8d", @fm_ratio2)
                        end)
      RXmScaleSetValue(wid, (@fm_ratio2 * 100 / @high_ratio).round)
    end
    if defined? @fm_index3
      wid = make_scales(wid, "fm index 3:", @fm_index2,
                        lambda do |w, c, i|
                          @fm_index2 = Rvalue(i) * (@high_index3 / 100.0)
                          set_label(c, "%8.3f", @fm_index2)
                        end)
      RXmScaleSetValue(wid, (100 * @fm_index3 / @high_index3).round)
      wid = make_scales(wid, "c/m ratio 3:", @fm_ratio3,
                        lambda do |w, c, i|
                          @fm_ratio3 = (Rvalue(i) * (@high_ratio / 100.0)).round
                          set_label(c, "%8d", @fm_ratio3)
                        end)
      RXmScaleSetValue(wid, (@fm_ratio3 * 100 / @high_ratio).round)
    end
    proc = nil
    data = make_sound_data(@chans, @bufsize)
    port = mus_audio_open_output(0, @srate, @chans, Mus_lshort, @bufsize * 2)
    die("Can't open DAC!") if port < 0
    RXmAddWMProtocolCallback(@shell_app[0],
                             RXmInternAtom(RXtDisplay(@shell_app[0]), "WM_DELETE_WINDOW", false),
                             lambda do |w, c, i|
                               RXtRemoveWorkProc(proc)
                               mus_audio_close(port)
                             end, false)
    proc = RXtAppAddWorkProc(@shell_app[1], lambda do |dummy|
                               @bufsize.times do |i|
                                 @chans.times do |c|
                                   sound_data_set!(data, c, i, body.call)
                                 end
                               end
                               mus_audio_write(port, data, @bufsize)
                               false
                             end)
    RXtRealizeWidget(@shell_app[0])
    RXtAppMainLoop(@shell_app[1])
  rescue
    die("start_dac() { ... }")
  end
end

# test functions

def bess(banner, file, &body)
  b = Bess.new(banner, file)
  b.make_ffm()
  b.start_dac() do b.instance_eval(&body) end
rescue
  die("bess(banner, file, osf, mdf) { ... }")
end

class Bess
  def make_fm
    @osc = make_oscil(0.0)
    @mod = make_oscil(0.0)
  end
  
  def fm
    @amp * @play * oscil(@osc, in_hz(@freq) + @fm_index1 * oscil(@mod, in_hz(@fm_ratio1 * @freq)))
  end

  def make_ffm
    @osc = make_oscil(0.0)
    @md1 = make_oscil(0.0)
    @md2 = make_oscil(0.0)
    @md3 = make_oscil(0.0)
    @fm_index1 = 1.0
    @fm_index2 = 0.0
    @fm_index3 = 0.0
    @fm_ratio1 = 1
    @fm_ratio2 = 1
    @fm_ratio3 = 1
    @high_index2 = 3.0
    @high_index2 = 1.0
    @high_index3 = 0.25
    @amp = 0.5
  end

  def ffm_rb
    @amp * @play * oscil(@osc, in_hz(@freq) + @fm_index1 * oscil(@md1, in_hz(@fm_ratio1 * @freq)) +
                                            @fm_index2 * oscil(@md2, in_hz(@fm_ratio2 * @freq)) +
                                            @fm_index3 * oscil(@md3, in_hz(@fm_ratio3 * @freq)))
  end

  def ffm
    ffm_c(@amp, @play, @freq, @fm_index1, @fm_index2, @fm_index3,
          @fm_ratio1, @fm_ratio2, @fm_ratio3, @osc, @md1, @md2, @md3)
  end

  rbm_require 'inline'
  include Inline
  inline do |ffm_c|
    fft_c.c "
#include <sndlib.h>
#include <clm.h>
  
typedef struct {
    mus_any *gen;
    VALUE *vcts;
    int nvcts;
    void *input_ptree;
} mus_xen;
  
double
fft_c(double amp,
      double play,
      double freq,
      double fm_index1,
      double fm_index2,
      double fm_index3,
      double fm_ratio1,
      double fm_ratio2,
      double fm_ratio3,
      mus_any *osc,
      mus_any *md1,
      mus_any *md2,
      mus_any *md3)
{
      return (amp * play * mus_oscil(osc, mus_hz2radians(freq) +
              fm_index1 * mus_oscil(md1, mus_hz2radians(fm_ratio1 * freq), 0.0) +
              fm_index2 * mus_oscil(md2, mus_hz2radians(fm_ratio2 * freq), 0.0) +
              fm_index3 * mus_oscil(md3, mus_hz2radians(fm_ratio3 * freq), 0.0), 0.0));
}"
  end
end

begin
  bess(banner, file) do ffm_rb() end
end

# bess.rb ends here
