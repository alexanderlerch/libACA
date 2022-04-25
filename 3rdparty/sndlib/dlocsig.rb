# dlocsig.rb -- CLM -> Snd/Ruby translation of dlocsig.lisp

# Translator/Author: Michael Scholz <mi-scholz@users.sourceforge.net>
# Copyright (c) 2003-2012 Michael Scholz <mi-scholz@users.sourceforge.net>
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

# Original Copyright of Fernando Lopez Lezcano:

# ;;; Copyright (c) 92, 93, 94, 98, 99, 2000, 2001 Fernando Lopez Lezcano. 
# ;;; All rights reserved.
# ;;; Use and copying of this software and preparation of derivative works
# ;;; based upon this software are permitted and may be copied as long as 
# ;;; no fees or compensation are charged for use, copying, or accessing
# ;;; this software and all copies of this software include this copyright
# ;;; notice. Suggestions, comments and bug reports are welcome. Please 
# ;;; address email to: nando@ccrma.stanford.edu
# ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

# ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
# ;;; Dynamic multichannel three-dimentional signal locator
# ;;; (wow that sound good! :-)
# ;;;
# ;;; by Fernando Lopez Lezcano
# ;;;    CCRMA, Stanford University
# ;;;    nando@ccrma.stanford.edu
# ;;;
# ;;; Thanks to Juan Pampin for help in the initial coding of the new version
# ;;; and for prodding me to finish it. To Joseph L. Anderson and Marcelo Perticone
# ;;; for insights into the Ambisonics coding and decoding process. 
# ;;; http://www.york.ac.uk/inst/mustech/3d_audio/ambison.htm for more details...

# Commentary:

# Tested with Snd 7.10, Motif 2.2.2, Gtk+ 2.2.1, Ruby 1.6.6, 1.6.8 and 1.9.0.
#
# The code is a translation of the Lisp code of Fernando Lopez Lezcano
# found in clm-2/dlocsig of the CLM distribution.  An extensive
# documentation of the purpose and usage of it can be found in
# clm-2/dlocsig/dlocsig.html.
#
# Note: dlocsig.rb handles not more rev_channels than out_channels;
# B_format_ambisonics handles only 4 out_channels and 0, 1, or 4
# rev_channels.
#
# The simple example
#
# [[-10, 10], [0, 5], [10, 10]].to_path.snd_plot
#
# draws trajectory, velocity, doppler curve, and the acceleration in
# Snd's lisp-graph.  If you have gnuplot installed, the example
#
# [[-10, 10], [0, 5], [10, 10]].to_path.pplot
#
# draws all four curves in one gnuplot window.

#   DL.make_path
#   DL.make_polar_path
#   DL.make_closed_path
#   and to_path take the following options
#
#   Open_bezier_path.new(path, *args)
#                        :d3,                  true
#                        :polar,               false
#                        :error,               0.01
#                        :curvature,           nil     
#                        :initial_direction,   [0.0, 0.0, 0.0]
#                        :final_direction,     [0.0, 0.0, 0.0]
#
# Closed_bezier_path.new(path, *args)
#                        :d3,                  true
#                        :polar,               false
#                        :error,               0.01
#                        :curvature,           nil
#
#   DL.make_literal_path
#   DL.make_literal_polar_path take the following options
#
#       Literal_path.new(path, *args)
#                        :d3,                  true
#                        :polar,               false
#
#   DL.make_spiral_path takes these options
#
#        Spiral_path.new :start_angle,         0
#                        :turns,               2
#
# The make_locsig-replacement make_dlocsig takes these arguments:
#
#         DL.make_dlocsig(startime, dur, *args)
#                        :path,                nil
#                        :scaler,              1.0
#                        :reverb_amount,       0.05
#                        :rbm_output,          $output
#                        :rbm_reverb,          $reverb
#                        :output_power,        1.5
#                        :reverb_power,        0.5
#                        :render_using,        :amplitude_panning
#                                           or :b_format_ambisonics
#                                           or :decoded_ambisonics
#
# Sample instruments (sinewave() and move() below) show how to replace
# the usual make_locsig() and locsig() by DL.make_dlocsig() and
# DL.dlocsig().

# Example functions at the end of the file:

# class Instrument
#   sinewave(start, dur, freq, amp, path, amp_env, *dlocsig_args)
#   move(start, file, path, *dlocsig_args)
#   move_sound(path, *dlocsig_args) do ... end
#
# class With_sound
#   run_dlocsig(start, dur, *dlocsig_args) do |samp| ... end

# Classes and Modules:

# module Inject
#   inject(n)
#   sum(initial)
#   product(initial)
#
# class Array
#   to_trias
#   to_path(*args)
#
# class Sndplot
#   initialize(chns)
#   snd
#   open
#   close
#
# class Gnuplot
#   initialize
#   open
#   close
#   command(*args)
#   reset
#   set_autoscale
#   set_x_range(range)
#   set_y_range(range)
#   set_z_range(range)
#   set_grid
#   set_surface
#   set_parametric
#   set_ticslevel(level)
#   set_title(title)
#   set_label(label)
#   set_margins(margin)
#   set_border(border)
#   start_multiplot
#   end_multiplot
#   size(xorigin, yorigin, xsize, ysize)
#   data(data, *args)
#   plot_2d_curve(curve, *args)
#   plot_2d_curves(curves, *args)
#   plot_3d_curve(curve, *args)
#  
# module DL
#   class Dlocsig < Dlocs
#     initialize(start, dur, *args)
#     one_turn
#     one_turn=(val)
#     speed_of_sound
#     speed_of_sound=(val)
#     run_beg
#     run_end
#     angles_in_degree
#     angles_in_radians
#     angles_in_turns
#     distance_in_meters
#     distance_in_feet
#
#   class Path
#     initialize(path, *args)
#     path_x
#     path_y
#     path_z
#     path_time
#     scale_path(scaling)
#     translate_path(translation)
#     rotate_path(rotation, *args)
#
#     path_trajectory
#     path_2d_trajectory
#     path_velocity
#     path_doppler
#     path_acceleration
#
#     plot_open
#     plot_close
#     cmd(*args)
#     plot_trajectory(*args)
#     plot_velocity(reset)
#     plot_doppler(reset)
#     plot_acceleration(reset)
#     pplot(normalize)
#
#     snd_open(chns)
#     snd_close
#     snd_trajectory(chn, label)
#     snd_velocity(chn, label)
#     snd_doppler(chn, label)
#     snd_acceleration(chn, label)
#     snd_plot
#
#   DL.make_dlocsig(start, dur, *args)
#   DL.dlocsig(dl, loc, input)
#  
#   DL.make_path(path, *args)
#   DL.make_polar_path(path, *args)
#   DL.make_closed_path(path, *args)
#   DL.make_literal_path(path, *args)
#   DL.make_literal_polar_path(path, *args)
#   DL.make_spiral_path(*args)
#
# class Dlocsig_menu
#   initialize(label, snd_p)
#   post_dialog

# Code:

require "ws"
require "matrix"
include Math

provided?(:snd_motif) and (not provided?(:xm)) and require("libxm.so")
provided?(:snd_gtk)   and (not provided?(:xg)) and require("libxg.so")

class DlocsigError < StandardError
end

Ruby_exceptions[:dlocsig_error] = DlocsigError

def dl_error(*msg)
  Snd.raise(:dlocsig_error, (msg.empty? ? "" : format(*msg)))
end

# module Inject, see Thomas, David, Hunt, Andrew: Programming Ruby --
# The Pragmatic Programmer's Guide, 2001 Addison-Wesley, page 102n

module Inject
  def inject(n)
    each do |x| n = yield(n, x) end
    n
  end
  
  def sum(initial = 0)
    inject(initial) do |n, v| n + v end
  end
  
  def product(initial = 1)
    inject(initial) do |n, v| n * v end
  end
end unless defined? Inject

# used by plotting curves
# to_trias: [0, 1, 2, 3, 4, 5] --> [[0, 1, 2], [3, 4, 5]]
# to_path:  [[-10, 10], [0, 5], [10, 10]].to_path <=> DL.make_path([[-10, 10], [0, 5], [10, 10]])
# uses the same options as DL.make_path()

class Array
  include Inject

  def to_trias
    ary = []
    unless self.length.divmod(3).last.nonzero?
      0.step(self.length - 2, 3) do |i|
        ary.push([self[i], self[i + 1], self[i + 2]])
      end
    end
    ary
  end

  def to_path(*args)
    DL.make_path(self, *args)
  end
end

class Sndplot
  def initialize(chns = 1)
    @chns = chns
    @snd = open
  end
  attr_reader :snd

  def inspect
    format("#<%s: snd: %d, chns: %d>", self.class, @snd, @chns)
  end
  
  def open
    if snds = sounds()
      snds.each do |s| set_sound_property(:selected, false, s) end
      set_sound_property(:selected, true, selected_sound)
    end
    if @snd = snds.detect do |s| channels(s) >= @chns end
      set_sound_property(:dlocsig_created, false, @snd)
      select_sound(@snd)
    else
      @snd = new_sound(snd_tempnam, default_output_header_type, default_output_data_format,
                       default_output_srate, @chns)
      set_sound_property(:dlocsig_created, true, @snd)
    end
    channels(@snd).times do |chn|
      set_channel_property(:time_graph, time_graph?(@snd, chn), @snd, chn)
      set_channel_property(:transform_graph, transform_graph?(@snd, chn), @snd, chn)
      set_channel_property(:lisp_graph, lisp_graph?(@snd, chn), @snd, chn)
      set_time_graph?(false, @snd, chn)
      set_transform_graph?(false, @snd, chn)
      set_lisp_graph?(true, @snd, chn)
    end
    $exit_hook.add_hook!("dlocsig-hook") do | |
      close
      false
    end
    @snd
  end

  def close
    if snds = sounds()
      snds.each do |snd|
        set_sound_property(:selected, false, snd)
        unless sound_property(:dlocsig_created, snd).nil?
          if sound_property(:dlocsig_created, snd)
            close_sound_extend(snd)
          else
            channels(snd).times do |chn|
              set_time_graph?(channel_property(:time_graph, snd, chn), snd, chn)
              set_transform_graph?(channel_property(:transform_graph, snd, chn), snd, chn)
              set_lisp_graph?(channel_property(:lisp_graph, snd, chn), snd, chn)
            end
          end
        end
      end
    end
    $exit_hook.remove_hook!("dlocsig-hook")
    self
  end
end

class Gnuplot
  @@plot_stream = nil
  
  def initialize
    if (not @@plot_stream) or @@plot_stream.closed?
      open
    end
  end

  def inspect
    format("#<%s: plot_stream: %s>", self.class, @@plot_stream.inspect)
  end
  
  def open
    if (gnuplot = `which gnuplot`).empty?
      dl_error("gnuplot not found?")
    else
      @@plot_stream = IO.popen(gnuplot, "w")
    end
  end

  def close
    unless @@plot_stream.closed?
      @@plot_stream.puts("quit")
      @@plot_stream.close
      @@plot_stream = nil
    end
  end

  def command(*args)
    open if @@plot_stream.closed?
    @@plot_stream.printf(*args)
    format(*args).chomp
  rescue
    Snd.warning("%s#%s", self.class, get_func_name)
  end

  def reset
    command "reset\n"
  end

  def set_autoscale
    command "set autoscale\n"
  end

  def set_x_range(range = [])
    command("set xrange [%f:%f]\n", range[0], range[1]) if range.length == 2
  end

  def set_y_range(range = [])
    command("set yrange [%f:%f]\n", range[0], range[1]) if range.length == 2
  end

  def set_z_range(range = [])
    command("set zrange [%f:%f]\n", range[0], range[1]) if range.length == 2
  end

  def set_grid
    command "set grid xtics; set grid ytics; set grid ztics\n"
  end

  def set_surface
    command "set surface\n"
  end

  def set_parametric
    command "set parametric\n"
  end

  def set_ticslevel(level = 0)
    command("set ticslevel %.2f\n", level)
  end

  def set_title(title = "")
    command("set title \"%s\"\n", title) unless title.empty?
  end

  def set_label(label = "")
    command("set label \"%s\"\n", label) unless label.empty?
  end

  def set_margins(margin = 1)
    command("set tmargin %f\n", margin)
    command("set lmargin %f\n", margin)
    command("set rmargin %f\n", margin)
    command("set bmargin %f\n", margin)
  end

  def set_border(border = nil)
    command("set border %d\n", border.to_i) if border
  end

  def start_multiplot
    command "set multiplot\n"
  end

  def end_multiplot
    command "set nomultiplot\n"
  end

  def size(xorigin, yorigin, xsize, ysize)
    command("set origin %f,%f\n", xorigin.to_f, yorigin.to_f)
    command("set size %f,%f\n", xsize.to_f, ysize.to_f)
  end

  def data(data, *args)
    style, label = nil
    optkey(args, binding,
           [:style, "linespoints"],
           [:label, ""])
    command("plot '-' %s %s\n", label.empty? ? "" : "title \"#{label}\"",
                                             style.empty? ? "" : "with #{style}")
    data.each_with_index do |y, x| command("%f %f\n", x, y) end
    command "e\n"
  end

  def plot_2d_curve(curve, *args)
    style, label = nil
    optkey(args, binding,
           [:style, "linespoints"],
           [:label, ""])
    set_grid()
    command("plot '-' %s %s\n", label.empty? ? "" : "title \"#{label}\"",
                                             style.empty? ? "" : "with #{style}")
    curve.each_pair do |x, y| command("%.8f %.8f\n", x, y) end
    command "e\n"
  end

  def plot_2d_curves(curves, *args)
    styles, labels = nil
    optkey(args, binding,
           [:styles, "linespoints"],
           [:labels, ""])
    set_grid()
    styles = curves.map do |i| styles end unless array?(styles)
    labels = curves.map do |i| labels end unless array?(labels)
    command "plot"
    curves.each_with_index do |x, i|
      style = styles[i]
      label = labels[i]
      command " '-' "
      command(" title \"%s\"", label) if label or (not label.empty?)
      command(" with %s", style) if style or (not style.empty?)
      command(", ") if i != (curves.length - 1)
    end
    command "\n"
    curves.each do |curve|
      curve.each_pair do |x, y| command("%.8f %.8f\n", x, y) end
      command "e\n"
    end
  end

  def plot_3d_curve(curve, *args)
    style, label, zstyle, xrot, zrot, scale, zscale = nil
    optkey(args, binding,
           [:style, "linespoints"],
           [:label, ""],
           [:zstyle, "impulses"],
           :xrot,
           :zrot,
           :scale,
           :zscale)
    set_border(127 + 256 + 512)
    set_grid()
    set_surface()
    set_parametric()
    set_ticslevel(0)
    if xrot or zrot or scale or zscale
      command("set view %s,%s,%s,%s\n", xrot, zrot, scale, zscale)
    end
    command "splot '-'"
    command(" title \"%s\"", label) unless label.empty?
    command(" with %s 1", style) unless style.empty?
    command(", '-' notitle with %s 1", zstyle) unless zstyle.empty?
    command "\n"
    curve.to_trias.each do |x, y, z| command("%.8f %.8f %.8f\n", x, y, z) end
    command "e\n"
    if zstyle
      curve.to_trias.each do |x, y, z| command("%.8f %.8f %.8f\n", x, y, z) end
      command "e\n"
    end
  end
end

module DL
  Path_maxcoeff = 8
  Point707 = cos(TWO_PI / 8.0)

  Amplitude_panning   = 1
  B_format_ambisonics = 2
  Decoded_ambisonics  = 3
  def which_render(val)
    case val
    when :amplitude_panning, Amplitude_panning
      Amplitude_panning
    when :b_format_ambisonics, B_format_ambisonics
      B_format_ambisonics
    when :decoded_ambisonics, Decoded_ambisonics
      Decoded_ambisonics
    else
      Amplitude_panning
    end
  end

  def cis(r)
    Complex(cos(r), sin(r))
  end

  def distance(x, y, z)
    sqrt(x * x + y * y + z * z)
  end

  def nearest_point(x0, y0, z0, x1, y1, z1, px, py, pz)
    if same?(x0, y0, z0, px, py, pz)
      [x0, y0, z0]
    elsif same?(x1, y1, z1, px, py, pz)
      [x1, y1, z1]
    elsif same?(x0, y0, z0, x1, y1, z1)
      [x0, y0, z0]
    else
      xm0 = x1 - x0
      ym0 = y1 - y0
      zm0 = z1 - z0
      xm1 = px - x0
      ym1 = py - y0
      zm1 = pz - z0
      d0 = distance(xm0, ym0, zm0)
      d1 = distance(xm1, ym1, zm1)
      p = d1 * ((xm0 * xm1 + ym0 * ym1 + zm0 * zm1) / (d0 * d1))
      ratio = p / d0
      [x0 + xm0 * ratio, y0 + ym0 * ratio, z0 + zm0 * ratio]
    end
  end

  def same?(a0, b0, c0, a1, b1, c1)
    a0 == a1 and b0 == b1 and c0 == c1
  end
  
  def rotation_matrix(x, y, z, angle)
    mag = distance(x, y, z)
    dx = x / mag
    dy = y / mag
    dz = z / mag
    ri = Matrix.I(3)
    ra = Matrix.rows([[0.0, dz, -dy], [-dz, 0.0, dx], [dy, -dx, 0.0]])
    raa = ra * ra
    sn = sin(-angle)
    omcs = 1 - cos(-angle)
    raa = raa.map do |xx| omcs * xx end
    ra = ra.map do |xx| sn * xx end
    (ri + ra + raa)
  end

  class Dlocsig_base
    include DL

    def initialize
      @one_turn = 360.0
      @speed_of_sound = 344.0
    end
    attr_accessor :one_turn, :speed_of_sound

    def angles_in_degree
      @one_turn = 360.0
    end

    def angles_in_radians
      @one_turn = TWO_PI
    end

    def angles_in_turns
      @one_turn = 1.0
    end

    def distances_in_meters
      @speed_of_sound = 344.0
    end

    def distances_in_feet
      @speed_of_sound = 1128.0
    end
  end
  
  class Speaker_config < Dlocsig_base
    Groups = Struct.new("Groups", :size, :vertices, :speakers, :matrix)

    def initialize
      super
      @number = nil
      @coords = nil
      @groups = nil
    end
    
    protected
    def set_speakers(channels, d3)
      if channels.between?(1, 8)
        d3 = false if channels < 4
        arrange_speakers(unless d3
                           case channels
                           when 1
                             [[0]]
                           when 2
                             [[-60, 60]]
                           when 3
                             [[-45, 45, 180]]
                           when 4
                             [[-45, 45, 135, 225]]
                           when 5
                             [[-45, 0, 45, 135, -135]]
                           when 6
                             [[-60, 0, 60, 120, 180, 240]]
                           when 7
                             [[-45, 0, 45, 100, 140, -140, -100]]
                           when 8
                             [[-22.5, 22.5, 67.5, 112.5, 157.5, 202.5, 247.5, 292.5]]
                           end
                         else
                           case channels
                           when 4
                             [[[-60, 0], [60, 0], [180, 0], [0, 90]],
                              [[0, 1, 3], [1, 2, 3], [2, 0, 3], [0, 1, 2]]]
                           when 5
                             [[[-45, 0], [45, 0], [135, 0], [-135, 0], [0, 90]],
                              [[0, 1, 4], [1, 2, 4], [2, 3, 4], [3, 0, 4], [0, 1, 2], [2, 3, 0]]]
                           when 6
                             [[[-45, 0], [45, 0], [135, 0], [-135, 0], [-90, 60], [90, 60]],
                              [[0, 1, 4], [1, 4, 5], [1, 2, 5], [2, 3, 5],
                               [3, 4, 5], [3, 0, 4], [0, 1, 2], [2, 3, 0]]]
                           when 7
                             [[[-45, 0], [45, 0], [135, 0], [-135, 0],
                               [-60, 60], [60, 60], [180, 60]],
                              [[0, 1, 4], [1, 4, 5], [1, 2, 5], [2, 6, 5], [2, 3, 6],
                               [3, 4, 6], [3, 0, 4], [4, 5, 6], [0, 1, 2], [2, 3, 0]]]
                           when 8
                             [[[-45, 10], [45, -10], [135, -10], [225, -10],
                               [-45, 45], [45, 45], [135, 45], [225, 45]],
                              [[0, 4, 5], [0, 5, 1], [5, 1, 2], [2, 6, 5], [6, 7, 2], [2, 3, 7],
                               [3, 7, 4], [3, 0, 4], [4, 7, 6], [6, 5, 4], [0, 1, 2], [2, 3, 0]]]
                           end
                         end)
      else
        dl_error(channels, "only 1 to 8 channels possible")
      end
    end
    
    private
    def arrange_speakers(args)
      speakers = args.shift
      groups = args.shift
      @number = speakers.length
      @coords = speakers.map do |s|
        a = (array?(s) ? s[0] : s).to_f
        e = (array?(s) ? s[1] : 0).to_f
        evec = cis((e / @one_turn) * TWO_PI)
        dxy = evec.real
        avec = cis((a / @one_turn) * TWO_PI)
        x = (dxy * avec.imag)
        y = (dxy * avec.real)
        z = evec.imag
        mag = distance(x, y, z)
        [x / mag, y / mag, z / mag]
      end
      unless groups
        if @number == 1
          groups = [[0]]
        else
          groups = make_array(@number) do |i| [i, i + 1] end
          groups[-1][-1] = 0
        end
      end
      @groups = groups.map do |group|
        size = group.length
        vertices = group.map do |vertice| @coords[vertice] end
        matrix = case size
                 when 3
                   if (m = Matrix[vertices[0], vertices[1], vertices[2]]).regular?
                     m.inverse.to_a
                   else
                     nil
                   end
                 when 2
                   if (m = Matrix[vertices[0][0, 2], vertices[1][0, 2]]).regular?
                     m.inverse.to_a
                   else
                     nil
                   end
                 else
                   nil
                 end
        Groups.new(size, vertices, group, matrix)
      end
    end
  end
  
  class Dlocsig < Speaker_config
    def initialize
      super
      @render_using = Amplitude_panning
      @output_power = 1.5
      @reverb_power = 0.5
      @rbm_output = nil
      @rbm_reverb = nil
      @out_channels = 4
      @rev_channels = 1
      @clm = true
      @delay = []
      @prev_time = @prev_dist = @prev_group = @prev_x = @prev_y = @prev_z = false
      @first_dist = @last_dist = 0.0
      @min_dist = @max_dist = 0.0
      @start = nil
      @end = nil
      @output_gains = nil
      @reverb_gains = nil
      @path = nil
      @run_beg = @run_end = nil
    end
    attr_reader :run_beg, :run_end, :out_channels, :rev_channels

    def inspect
      format("#<%s: channels: %d, reverb: %s>", self.class, @out_channels, @rev_channels.inspect)
    end

    def each
      (@run_beg...@run_end).each do |i| yield(i) end
    end
    alias run each

    # general clm version
    # dl.dlocsig(loc, val)
    def dlocsig(loc, input)
      if loc < @start
        delay(@path, (loc >= @end ? 0.0 : input), 0.0)
        @out_channels.times do |chn| out_any(loc, 0.0, chn, @rbm_output) end
      else
        sample = delay(@path, (loc >= @end ? 0.0 : input), env(@delays))
        @out_channels.times do |chn|
          out_any(loc, sample * env(@output_gains[chn]), chn, @rbm_output)
        end
        @rev_channels.times do |chn|
          out_any(loc, sample * env(@reverb_gains[chn]), chn, @rbm_reverb)
        end
      end
    end

    # dl.ws_dlocsig do |loc| ...; val; end
    # @clm == true    @rbm_output/@rbm_reverb: sample2files
    # @clm == false   @rbm_output/@rbm_reverb: sound index numbers
    # With_sound#run_dlocsig below uses this method
    def ws_dlocsig
      len = @run_end - @run_beg
      out_data = make_vct(len)
      len.times do |i|
        loc = i + @run_beg
        input = yield(loc)
        if loc < @start
          delay(@path, (loc >= @end ? 0.0 : input), 0.0)
        else
          out_data[i] = delay(@path, (loc >= @end ? 0.0 : input), env(@delays))
        end
      end
      @out_channels.times do |chn|
        if @clm
          (@run_beg...@run_end).each do |i|
            out_any(i, out_data[i - @run_beg] * env(@output_gains[chn]), chn, @rbm_output)
          end
        else
          out = vct_multiply!(vct_copy(out_data),
                              Vct.new(len) do |x| env(@output_gains[chn]) end)
          mix_vct(out, @run_beg, @rbm_output, chn, false)
        end
      end
      @rev_channels.times do |chn|
        if @clm
          (@run_beg...@run_end).each do |i|
            out_any(i, out_data[i - @run_beg] * env(@reverb_gains[chn]), chn, @rbm_reverb)
          end
        else
          out = vct_multiply!(vct_copy(out_data),
                              Vct.new(len) do |x| env(@reverb_gains[chn]) end)
          mix_vct(out, @run_beg, @rbm_reverb, chn, false)
        end
      end
    end
    
    # :amplitude_panning
    # :b_format_ambisonics
    # :decoded_ambisonics
    def make_dlocsig(startime, dur, *args)
      path, scaler, reverb_amount, output_power, reverb_power, render_using = nil
      rbm_output, rbm_reverb, out_channels, rev_channels, clm = nil
      optkey(args, binding,
             :path,
             [:scaler, 1.0],
             [:reverb_amount, 0.05],
             [:output_power, 1.5],
             [:reverb_power, 0.5],
             [:render_using, Amplitude_panning],
             [:rbm_output, $output],
             [:rbm_reverb, $reverb],
             [:out_channels, 4],
             [:rev_channels, 1],
             [:clm, true])
      @output_power = output_power
      @reverb_power = reverb_power
      @render_using = which_render(render_using)
      @rbm_output   = rbm_output
      @rbm_reverb   = rbm_reverb
      @out_channels = out_channels
      @rev_channels = rev_channels
      @clm          = clm
      if @render_using == B_format_ambisonics and @out_channels != 4
        dl_error("B_format_ambisonics requires 4 output channels")
      end
      if @render_using == B_format_ambisonics and
          @rev_channels.nonzero? and
          (@rev_channels != 1 and @rev_channels != 4)
        dl_error("B_format_ambisonics accepts only 0, 1 or 4 rev_channels")
      end
      if @rev_channels > @out_channels
        dl_error("more rev_channels than out_channels")
      end
      if @render_using == B_format_ambisonics
        scaler *= 0.8
      end
      unless path.kind_of?(Path)
        if array?(path) and !path.empty?
          path = make_path(path)
        else
          dl_error(path, "sorry, need a path")
        end
      end
      xpoints = path.path_x
      ypoints = path.path_y
      zpoints = path.path_z
      tpoints = path.path_time
      @channel_gains = make_array(@out_channels) do [] end
      @channel_rev_gains = make_array(@rev_channels) do [] end
      self.set_speakers(@out_channels, (not zpoints.detect do |x| x.nonzero? end.nil?))
      @speed_limit = (@speed_of_sound * (tpoints[-1] - tpoints[0])) / dur
      if xpoints.length == 1
        walk_all_rooms(xpoints[0], ypoints[0], zpoints[0], tpoints[0])
      else
        xb = yb = zb = tb = 0.0
        (tpoints.length - 1).times do |i|
          xa, xb = xpoints[i, 2]
          ya, yb = ypoints[i, 2]
          za, zb = zpoints[i, 2]
          ta, tb = tpoints[i, 2]
          minimum_segment_length(xa, ya, za, ta, xb, yb, zb, tb)
        end
        walk_all_rooms(xb, yb, zb, tb)
      end
      @start = dist2samples(@first_dist - @min_dist)
      @end = seconds2samples(startime + dur)
      min_delay = dist2samples(@min_dist)
      @run_beg = seconds2samples(startime)
      @run_end = @end + dist2samples(@last_dist) - min_delay
      real_dur = dur + dist2seconds(@last_dist - @first_dist)
      min_dist_unity = [@min_dist, 1.0].max
      unity_gain = scaler * min_dist_unity ** @output_power
      @output_gains = make_array(@number) do |i|
        make_env(:envelope, @channel_gains[i], :scaler, unity_gain, :duration, real_dur)
      end
      if @rev_channels.nonzero?
        unity_rev_gain = reverb_amount * scaler * min_dist_unity ** @reverb_power
        @reverb_gains = make_array(@rev_channels) do |i|
          make_env(:envelope, @channel_rev_gains[i], :scaler, unity_rev_gain, :duration, real_dur)
        end
      end
      @delays = make_env(:envelope, @delay, :offset, -min_delay, :duration, real_dur)
      @path = make_delay(:size, 1, :max_size, [1, dist2samples(@max_dist)].max)
      self
    end

    private
    def dist2samples(d)
      (d * (mus_srate() / @speed_of_sound)).round
    end

    def dist2seconds(d)
      d / @speed_of_sound.to_f
    end

    def transition_point_3(vert_a, vert_b, xa, ya, za, xb, yb, zb)
      line_b = vct(xa, ya, za)
      line_m = tr3_sub(vct(xb, yb, zb), line_b)
      normal = tr3_cross(vert_a, vert_b)
      if (denominator = tr3_dot(normal, line_m)).abs <= 0.000001
        false
      else
        vct2list(tr3_add(line_b, tr3_scale(line_m, -tr3_dot(normal, line_b) / denominator)))
      end
    end

    def tr3_cross(v1, v2)
      vct(v1[1] * v2[2] - v1[2] * v2[1],
          v1[2] * v2[0] - v1[0] * v2[2],
          v1[0] * v2[1] - v1[1] * v2[0])
    end

    def tr3_dot(v1, v2)
      dot_product(v1, v2)
    end

    def tr3_sub(v1, v2)
      vct_subtract!(vct_copy(v1), v2)
    end

    def tr3_add(v1, v2)
      vct_add!(vct_copy(v1), v2)
    end

    def tr3_scale(v1, c)
      vct_scale!(vct_copy(v1), c)
    end

    def transition_point_2(vert, xa, ya, xb, yb)
      ax = vert[0]
      bx = xa - xb
      ay = vert[1]
      by = ya - yb
      cx = -xa
      cy = -ya
      d = by * cx - bx * cy
      f = ay * bx - ax * by
      if f.zero?
        false
      else
        [(d * ax) / f, (d * ay) / f]
      end
    end

    def calculate_gains(x, y, z, group)
      zero_coord = 1e-10
      zero_gain = 1e-10
      size = group.size
      if mat = group.matrix
        if x.abs < zero_coord and y.abs < zero_coord and z.abs < zero_coord
          [true, [1.0, 1.0, 1.0]]
        else
          case size
          when 3
            gain_a = mat[0][0] * x + mat[0][1] * y + mat[0][2] * z
            gain_b = mat[1][0] * x + mat[1][1] * y + mat[1][2] * z
            gain_c = mat[2][0] * x + mat[2][1] * y + mat[2][2] * z
            mag = distance(gain_a, gain_b, gain_c)
            if gain_a.abs < zero_gain then gain_a = 0.0 end
            if gain_b.abs < zero_gain then gain_b = 0.0 end
            if gain_c.abs < zero_gain then gain_c = 0.0 end
            [(gain_a >= 0 and gain_b >= 0 and gain_c >= 0),
              [gain_a / mag, gain_b / mag, gain_c / mag]]
          when 2
            gain_a = mat[0][0] * x + mat[0][1] * y
            gain_b = mat[1][0] * x + mat[1][1] * y
            mag = distance(gain_a, gain_b, 0.0)
            if gain_a.abs < zero_gain then gain_a = 0.0 end
            if gain_b.abs < zero_gain then gain_b = 0.0 end
            [(gain_a >= 0 and gain_b >= 0), [gain_a / mag, gain_b / mag]]
          when 1
            [true, [1.0]]
          end
        end
      else
        [true, [1.0, 1.0, 1.0]]
      end
    end

    def find_group(x, y, z)
      grp = gns = false
      @groups.detect do |group|
        inside, gains = calculate_gains(x, y, z, group)
        if inside
          grp, gns = group, gains
          true
        end
      end
      [grp, gns]
    end

    def push_zero_gains(time)
      @out_channels.times do |i| @channel_gains[i].push(time, 0.0) end
      @rev_channels.times do |i| @channel_rev_gains[i].push(time, 0.0) end
    end

    def push_gains(group, gains, dist, time)
      outputs = make_vct(@out_channels)
      revputs = if @rev_channels > 0
                  make_vct(@rev_channels)
                else
                  false
                end
      if dist >= 1.0
        att = 1.0 / dist ** @output_power
        ratt = 1.0 / dist ** @reverb_power
      else
        att = 1.0 - dist ** (1.0 / @output_power)
        ratt = 1.0 - dist ** (1.0 / @reverb_power)
      end
      if dist >= 1.0
        group.speakers.each_with_index do |speaker, i|
          gain = gains[i]
          outputs[speaker] = gain * att
          if @rev_channels > 1
            revputs[speaker] = gain * ratt
          end
        end
      else
        @number.times do |speaker|
          if found = group.speakers.index(speaker)
            gain = gains[found]
            outputs[speaker] = gain + (1.0 - gain) * att
            if @rev_channels > 1
              revputs[speaker] = gain + (1.0 - gain) * ratt
            end
          else
            outputs[speaker] = att
            if @rev_channels > 1
              revputs[speaker] = ratt
            end
          end
        end
      end
      vct2list(outputs).each_with_index do |val, i| @channel_gains[i].push(time, val) end
      if @rev_channels == 1
        @channel_rev_gains[0].push(time, ratt)
      elsif @rev_channels > 1
        vct2list(revputs).each_with_index do |val, i| @channel_rev_gains[i].push(time, val) end
      end
    end
    
    def amplitude_panning(x, y, z, dist, time)
      if @prev_group
        if time != @prev_time and ((dist - @prev_dist) / (time - @prev_time)) > @speed_limit
          Snd.display("%s#%s: supersonic radial movement", self.class, get_func_name)
        end
        inside, gains = calculate_gains(x, y, z, @prev_group)
        if inside
          push_gains(@prev_group, gains, dist, time)
          @prev_x, @prev_y, @prev_z = x, y, z
        else
          group, gains = find_group(x, y, z)
          if group
            edge = group.vertices & @prev_group.vertices
            if edge.length == 2
              if pint = transition_point_3(edge[0], edge[1], x, y, z, @prev_x, @prev_y, @prev_z)
                xi, yi, zi = pint
                di = distance(xi, yi, zi)
                ti = @prev_time +
                     (distance(xi - @prev_x, yi - @prev_y, zi - @prev_z) / \
                      distance(x - @prev_x, y - @prev_y, z - @prev_z)) * (time - @prev_time)
                if ti < @prev_time
                  inside, gains = calculate_gains(xi, yi, zi, @prev_group)
                  if inside
                    push_gains(@prev_group, gains, di, ti)
                  else
                    inside, gains = calculate_gains(xi, yi, zi, group)
                    if inside
                      push_gains(group, gains, di, ti)
                    else
                      dl_error("outside of both adjacent groups")
                    end
                  end
                else
                  if $DEBUG
                    Snd.warning("%s#%s: current time <= previous time", self.class, get_func_name)
                  end
                end
              end
            elsif edge.length == 1 and group.size == 2
              if pint = transition_point_2(edge[0], x, y, @prev_x, @prev_y)
                xi, yi = pint
                di = distance(xi, yi, 0.0)
                ti = @prev_time +
                     (distance(xi - @prev_x, yi - @prev_y, 0.0) / \
                      distance(x - @prev_x, y - @prev_y, 0.0)) * (time - @prev_time)
                if ti < @prev_time
                  inside, gains = calculate_gains(xi, yi, 0.0, @prev_group)
                  if inside
                    push_gains(@prev_group, gains, di, ti)
                    inside, gains = calculate_gains(xi, yi, 0.0, group)
                    if inside
                      push_gains(group, gains, di, ti)
                    else
                      dl_error("outside of both adjacent groups")
                    end
                  end
                else
                  if $DEBUG
                    Snd.warning("%s#%s: current time <= previous time", self.class, get_func_name)
                  end
                end
              end
            elsif edge.length == 1
              Snd.display("%s#%s: only one point in common", self.class, get_func_name)
            elsif edge.length.zero?
              Snd.display("%s#%s: with no common points", self.class, get_func_name)
            end
            push_gains(group, gains, dist, time)
            @prev_group, @prev_x, @prev_y, @prev_z = group, x, y, z
          else
            push_zero_gains(time)
            @prev_group = false
          end
        end
      else
        group, gains = find_group(x, y, z)
        if group
          push_gains(group, gains, dist, time)
          @prev_group, @prev_x, @prev_y, @prev_z = group, x, y, z
        else
          push_zero_gains(time)
          @prev_group = false
        end
      end
      @prev_time = time
      @prev_dist = dist
    end

    def b_format_ambisonics(x, y, z, dist, time)
      if dist > 1.0
        att = (1.0 / dist) ** @output_power
        @channel_gains[0].push(time, Point707 * att)
        @channel_gains[1].push(time, (y / dist) * att)
        @channel_gains[2].push(time, (-x / dist) * att)
        @channel_gains[3].push(time, (z / dist) * att)
        if @rev_channels == 1
          @channel_rev_gains[0].push(time, 1.0 / (dist ** @reverb_power))
        elsif @rev_channels == 4
          ratt = (1.0 / dist) ** @reverb_power
          @channel_rev_gains[0].push(time, Point707 * ratt)
          @channel_rev_gains[1].push(time, (y / dist) * ratt)
          @channel_rev_gains[2].push(time, (-x / dist) * ratt)
          @channel_rev_gains[3].push(time, (z / dist) * ratt)
        end
      elsif dist.zero?
        @channel_gains[0].push(time, 1.0)
        (1..3).each do |i| @channel_gains[i].push(time, 0.0) end
        if @rev_channels >= 1
          @channel_rev_gains[0].push(time, 1.0)
        end
        if @rev_channels == 4
          (1..3).each do |i| @channel_rev_gains[i].push(time, 0.0) end
        end
      else
        att = dist ** (1.0 / @output_power)
        @channel_gains[0].push(time, 1.0 - (1.0 - Point707) * dist ** @output_power)
        @channel_gains[1].push(time, (y / dist) * att)
        @channel_gains[2].push(time, (-x / dist) * att)
        @channel_gains[3].push(time, (z / dist) * att)
        if @rev_channels == 1
          @channel_rev_gains[0].push(time, 1.0 - dist ** (1.0 / @reverb_power))
        elsif @rev_channels == 4
          ratt = dist ** (1.0 / @reverb_power)
          @channel_rev_gains[0].push(time, 1.0 - (1.0 - Point707) * dist ** @reverb_power)
          @channel_rev_gains[1].push(time, (y / dist) * ratt)
          @channel_rev_gains[2].push(time, (-x / dist) * ratt)
          @channel_rev_gains[3].push(time, (z / dist) * ratt)
        end
      end
    end

    def decoded_ambisonics(x, y, z, dist, time)
      if dist > 1.0
        att = (1.0 / dist) ** @output_power
        attw = Point707 * Point707 * att
        attx = att * (x / dist)
        atty = att * (y / dist)
        attz = att * (z / dist)
        @coords.each_with_index do |s, i|
          @channel_gains[i].push(time, Point707 * (attw + attx * s[0] + atty * s[1] + attz * s[2]))
        end
        if @rev_channels == 1
          @channel_rev_gains[0].push(time, 1.0 / (dist ** @reverb_power))
        elsif @rev_channels == 4
          ratt = (1.0 / dist) ** @reverb_power
          rattw = Point707 * Point707 * ratt
          rattx = ratt * (x / dist)
          ratty = ratt * (y / dist)
          rattz = ratt * (z / dist)
          @rev_channels.times do |i|
            s = @coords[i]
            @channel_rev_gains[i].push(time, Point707 * \
                                       (rattw + rattx * s[0] + ratty * s[1] + rattz * s[2]))
          end
        end
      elsif dist.zero?
        att = Point707 * Point707
        @coords.each_index do |i| @channel_gains[i].push(time, att) end
        if @rev_channels == 1
          @channel_rev_gains[0].push(time, 1.0)
        else
          @rev_channels.times do |i| @channel_rev_gains[i].push(time, att) end
        end
      else
        att = dist ** (1.0 / @output_power)
        attw = Point707 * (1.0 - (1.0 - Point707) * dist ** @output_power)
        attx = att * (x / dist)
        atty = att * (y / dist)
        attz = att * (z / dist)
        @coords.each_with_index do |s, i|
          @channel_gains[i].push(time, Point707 * (attw + attx * s[0] + atty * s[1] + attz * s[2]))
        end
        if @rev_channels == 1
          @channel_rev_gains[0].push(time, 1.0 - dist ** (1.0 / @reverb_power))
        elsif @rev_channels == 4
          ratt = dist ** (1.0 / @reverb_power)
          rattw = Point707 * (1.0 - (1.0 - Point707) * dist ** @reverb_power)
          rattx = ratt * (x / dist)
          ratty = ratt * (y / dist)
          rattz = ratt * (z / dist)
          @rev_channels.times do |i|
            s = @coords[i]
            @channel_rev_gains[i].push(time, Point707 * \
                                       (rattw + rattx * s[0] + ratty * s[1] + rattz * s[2]))
          end
        end
      end
    end

    def walk_all_rooms(x, y, z, time)
      dist = distance(x, y, z)
      if @first_dist.zero?
        @first_dist = dist
      end
      @last_dist = dist
      if @min_dist.zero? or dist < @min_dist
        @min_dist = dist
      end
      if @max_dist.zero? or dist > @max_dist
        @max_dist = dist
      end
      @delay.push(time, dist2samples(dist))
      case @render_using
      when Amplitude_panning
        amplitude_panning(x, y, z, dist, time)
      when B_format_ambisonics
        b_format_ambisonics(x, y, z, dist, time)
      when Decoded_ambisonics
        decoded_ambisonics(x, y, z, dist, time)
      end
    end

    def change_direction(xa, ya, za, ta, xb, yb, zb, tb)
      walk_all_rooms(xa, ya, za, ta)
      if xa != xb or ya != yb or za != zb or ta != tb
        xi, yi, zi = nearest_point(xa, ya, za, xb, yb, zb, 0.0, 0.0, 0.0)
        if (((xa < xb) ? (xa <= xi and xi <= xb) : (xb <= xi and xi <= xa)) and
            ((ya < yb) ? (ya <= yi and yi <= yb) : (yb <= yi and yi <= ya)) and
            ((za < zb) ? (za <= zi and zi <= zb) : (zb <= zi and zi <= za)))
          walk_all_rooms(xi, yi, zi,
                         tb + (ta - tb) * (distance(xb - xi, yb - yi, zb - zi) / \
                                           distance(xb - xa, yb - ya, zb - za)))
        end
      end
    end

    def intersects_inside_radius(xa, ya, za, ta, xb, yb, zb, tb)
      mag = distance(xb - xa, yb - ya, zb - za)
      vx = (xb - xa) / mag
      vy = (yb - ya) / mag
      vz = (zb - za) / mag
      bsq = xa * vx + ya * vy + za * vz
      disc = bsq * bsq - ((xa * xa + ya * ya + za * za) - 1.0)
      if disc >= 0.0
        root = sqrt(disc)
        rin = -bsq - root
        rout = -bsq + root
        xi = xo = nil
        if rin > 0 and rin < mag
          xi = xa + vx * rin
          yi = ya + vy * rin
          zi = za + vz * rin
          ti = tb + (ta - tb) *
               (distance(xb - xi, yb - yi, zb - zi) / distance(xb - xa, yb - ya, zb - za))
        end
        if rout > 0 and rout.abs < mag
          xo = xa + vx * rout
          yo = ya + vy * rout
          zo = za + vz * rout
          to = tb + (ta - tb) *
               (distance(xb - xo, yb - yo, zb - zo) / distance(xb - xa, yb - ya, zb - za))
        end
        if xi
          change_direction(xa, ya, za, ta, xi, yi, zi, ti)
          if xo
            change_direction(xi, yi, zi, ti, xo, yo, zo, to)
            change_direction(xo, yo, zo, to, xb, yb, zb, tb)
          else
            change_direction(xi, yi, zi, ti, xb, yb, zb, tb)
          end
        else
          if xo
            change_direction(xa, ya, za, ta, xo, yo, zo, to)
            change_direction(xo, yo, zo, to, xb, yb, zb, tb)
          else
            change_direction(xa, ya, za, ta, xb, yb, zb, tb)
          end
        end
      else
        change_direction(xa, ya, za, ta, xb, yb, zb, tb)
      end
    end

    def minimum_segment_length(xa, ya, za, ta, xb, yb, zb, tb)
      if distance(xb - xa, yb - ya, zb - za) < 1.0
        intersects_inside_radius(xa, ya, za, ta, xb, yb, zb, tb)
      else
        xi = (xa + xb) * 0.5
        yi = (ya + yb) * 0.5
        zi = (za + zb) * 0.5
        ti = tb + (ta - tb) *
             (distance(xb - xi, yb - yi, zb - zi) / distance(xb - xa, yb - ya, zb - za))
        minimum_segment_length(xa, ya, za, ta, xi, yi, zi, ti)
        minimum_segment_length(xi, yi, zi, ti, xb, yb, zb, tb)
      end
    end
  end

  class Path < Dlocsig_base
    def initialize
      super
      @rx = @ry = @rz = @rv = @rt = @tx = @ty = @tz = @tt = nil
      @gnuplot = @sndplot = nil
    end

    def path_x
      (@tx or (@rx or (render_path(); @rx)))
    end

    def path_y
      (@ty or (@ry or (render_path(); @ry)))
    end

    def path_z
      (@tz or (@rz or (render_path(); @rz)))
    end

    def path_time
      (@tt or (@rt or (render_path(); @rt)))
    end

    def scale_path(scaling)
      assert_type((number?(scaling) or array?(scaling)), 0, scaling, "a number or an array")
      if number?(scaling) then scaling = [scaling, scaling, scaling] end
      transform_path(:scaling, scaling)
    end

    def translate_path(translation)
      assert_type((number?(translation) or array?(translation)),
                  0, translation, "a number or an array")
      if number?(translation) then translation = [translation, translation, translation] end
      transform_path(:translation, translation)
    end

    def rotate_path(rotation, *args)
      assert_type(number?(rotation), 0, rotation, "a number")
      rotation_center, rotation_axis = nil
      optkey(args, binding,
             :rotation_center,
             [:rotation_axis, [0.0, 0.0, 1.0]])
      transform_path(:rotation, rotation,
                     :rotation_center, rotation_center,
                     :rotation_axis, rotation_axis)
    end
    
    def path_trajectory
      path_x.map_with_index do |d, i| [d, path_y[i], path_z[i]] end.flatten
    end

    def path_2d_trajectory
      path_x.map_with_index do |d, i| [d, path_y[i]] end.flatten
    end

    # if velocity is zero, ti == tf
    Secure_distance = 0.0001
    def path_velocity
      xp, yp, zp, tp = path_x, path_y, path_z, path_time
      (0...(tp.length - 1)).map do |i|
        xi, xf = xp[i, 2]
        yi, yf = yp[i, 2]
        zi, zf = zp[i, 2]
        ti, tf = tp[i, 2]
        if tf == ti
          tf += Secure_distance
        end
        [(ti + tf) / 2.0, distance(xf - xi, yf - yi, zf - zi) / (tf - ti)]
      end.flatten
    end
    
    def path_doppler
      xp, yp, zp, tp = path_x, path_y, path_z, path_time
      (0...(tp.length - 1)).map do |i|
        xi, xf = xp[i, 2]
        yi, yf = yp[i, 2]
        zi, zf = zp[i, 2]
        ti, tf = tp[i, 2]
        if tf == ti
          tf += Secure_distance
        end
        [(tf + ti) / 2.0, -((distance(xf, yf, zf) - distance(xi, yi, zi)) / (tf - ti))]
      end.flatten
    end

    def path_acceleration
      v = path_velocity()
      result = []
      0.step(v.length - 3, 2) do |i|
        ti, vi, tf, vf = v[i, 4]
        if tf == ti
          tf += Secure_distance
        end
        am = (vf - vi) / (tf - ti)
        result << ti << am << tf << am
      end
      result
    end

    # Gnuplot
    def plot_open
      if @gnuplot.kind_of?(Gnuplot)
        @gnuplot
      else
        @gnuplot = Gnuplot.new
      end
    end
    
    def plot_close
      @gnuplot.close if @gnuplot.kind_of?(Gnuplot)
    end

    def cmd(*args)
      @gnuplot = plot_open
      @gnuplot.command(format(*args) << "\n")
    end
    
    def plot_trajectory(*args)
      @gnuplot = plot_open
      label, reset = nil
      optkey(args, binding,
             [:label, "trajectory"],
             [:reset, true])
      @gnuplot.reset() if reset
      @gnuplot.set_autoscale()
      if path_z.detect do |z| z.nonzero? end
        @gnuplot.plot_3d_curve(path_trajectory(), :label, label, *args)
      else
        @gnuplot.plot_2d_curve(path_2d_trajectory(), :label, label, *args)
      end
    end

    def plot_velocity(reset = true)
      @gnuplot = plot_open
      @gnuplot.reset() if reset
      @gnuplot.set_autoscale()
      @gnuplot.plot_2d_curve(path_velocity(), :label, "velocity", :style, "steps")
    end

    def plot_doppler(reset = true)
      @gnuplot = plot_open
      @gnuplot.reset() if reset
      @gnuplot.set_autoscale()
      @gnuplot.plot_2d_curve(path_doppler(), :label, "doppler", :style, "steps")
    end

    def plot_acceleration(reset = true)
      @gnuplot = plot_open
      @gnuplot.reset() if reset
      @gnuplot.set_autoscale()
      @gnuplot.plot_2d_curve(path_acceleration(), :label, "acceleration", :style, "steps")
    end

    def pplot(normalize = true)
      @gnuplot = plot_open
      norm = lambda do |env, nrm|
        unless nrm
          env
        else
          mx = env.each_pair do |x, y| y end.max
          if mx.zero?
            env
          else
            env.each_pair do |x, y| [x, y / mx.to_f] end.flatten
          end
        end
      end
      @gnuplot.reset()
      @gnuplot.size(0, 0, 1, 1)
      @gnuplot.start_multiplot()
      @gnuplot.size(0.0, 0.333, 1.0, 0.667)
      plot_trajectory(:reset, false)
      @gnuplot.size(0.0, 0.0, 1.0, 0.333)
      @gnuplot.plot_2d_curves([norm.call(path_velocity(), normalize),
                               norm.call(path_acceleration(), normalize),
                               norm.call(path_doppler(), normalize)],
                              :labels, ["velocity", "acceleration", "doppler"],
                              :styles, ["steps", "steps", "steps"])
      @gnuplot.end_multiplot()
    end

    # Sndplot
    def snd_open(chns = 1)
      if @sndplot.kind_of?(Sndplot)
        @sndplot
      else
        @sndplot = Sndplot.new(chns)
      end
    end

    def snd_close
      @sndplot.close if @sndplot.kind_of?(Sndplot)
    end
    
    def snd_trajectory(chn = 0, label = "trajectory")
      @sndplot = snd_open
      graph(path_2d_trajectory(), label, false, false, false, false, @sndplot.snd, chn)
      @sndplot
    end
    
    def snd_velocity(chn = 0, label = "velocity")
      @sndplot = snd_open
      graph(path_velocity(), label, false, false, false, false, @sndplot.snd, chn)
      @sndplot
    end
    
    def snd_doppler(chn = 0, label = "doppler")
      @sndplot = snd_open
      graph(path_doppler(), label, false, false, false, false, @sndplot.snd, chn)
      @sndplot
    end
    
    def snd_acceleration(chn = 0, label = "acceleration")
      @sndplot = snd_open
      graph(path_acceleration(), label, false, false, false, false, @sndplot.snd, chn)
      @sndplot
    end
    
    def snd_plot
      @sndplot = snd_open(4)
      snd_trajectory(0)
      snd_velocity(1)
      snd_acceleration(2)
      snd_doppler(3)
      @sndplot
    end

    private
    def transform_path(*args)
      scaling, translation, rotation, rotation_center, rotation_axis = nil
      optkey(args, binding,
             :scaling,
             :translation,
             :rotation,
             :rotation_center,
             [:rotation_axis, [0.0, 0.0, 1.0]])
      render_path() if @rx.nil?
      if scaling or translation or rotation
        rotation = TWO_PI * (rotation / @one_turn) if rotation
        if rotation_axis and (rotation_axis.length != 3)
          dl_error(rotation_axis, "rotation axis has to have all three coordinates")
        end
        matrix = if rotation
                   rotation_matrix(rotation_axis[0], rotation_axis[1], rotation_axis[2], rotation)
                 end
        xc = path_x()
        yc = path_y()
        zc = path_z()
        if rotation_center and (rotation_center.length != 3)
          dl_error(rotation_center, "rotation center has to have all three coordinates")
        end
        xtr = []
        ytr = []
        ztr = []
        xc.each_with_index do |x, i|
          y = yc[i]
          z = zc[i]
          xw, yw, zw = x, y, z
          if rotation_center and rotation
            xw -= rotation_center[0]
            yw -= rotation_center[1]
            zw -= rotation_center[2]
          end
          if rotation
            mc = [xw, yw, zw]
            xv, yv, zv = matrix.column_vectors
            xr = xv.to_a.map_with_index do |xx, ii| xx * mc[ii] end.to_a.sum
            yr = yv.to_a.map_with_index do |xx, ii| xx * mc[ii] end.to_a.sum
            zr = zv.to_a.map_with_index do |xx, ii| xx * mc[ii] end.to_a.sum
            xw, yw, zw = xr, yr, zr
          end
          if rotation_center and rotation
            xw += rotation_center[0]
            yw += rotation_center[1]
            zw += rotation_center[2]
          end
          if scaling
            xw *= scaling[0]
            yw *= scaling[1] if scaling[1]
            zw *= scaling[2] if scaling[2]
          end
          if translation
            xw += translation[0]
            yw += translation[1] if translation[1]
            zw += translation[2] if translation[2]
          end
          xtr << xw
          ytr << yw
          ztr << zw
        end
        @tx, @ty, @tz = xtr, ytr, ztr
      else
        @tt = @rt.dup
        @tx = @rx.dup
        @ty = @ry.dup
        @tz = @rz.dup
      end
    end

    def reset_transformation
      @tt = @tx = @ty = @tz = nil
    end

    def reset_rendering
      @rt = @rv = @rx = @ry = @rz = nil
      reset_transformation()
    end

    def parse_cartesian_coordinates(points, d3)
      if array?(points[0])
        x = points.map do |p| p[0] end
        y = points.map do |p| p[1] end
        z = points.map do |p| d3 ? (p[2] or 0.0) : 0.0 end
        v = points.map do |p| d3 ? p[3] : p[2] end
        [x, y, z, v]
      else
        if d3
          x = []
          y = []
          z = []
          0.step(points.length - 3, 3) do |i|
            x += [points[i]]
            y += [points[i + 1]]
            z += [points[i + 2]]
          end
          [x, y, z, x.map do |i| nil end]
        else
          x = []
          y = []
          0.step(points.length - 2, 2) do |i|
            x += [points[i]]
            y += [points[i + 1]]
          end
          [x, y, x.map do |i| 0.0 end, x.map do |i| nil end]
        end
      end
    end

    def parse_polar_coordinates(points, d3)
      if array?(points[0])
        x = []
        y = []
        z = []
        v = []
        points.each do |p|
          d = p[0]
          a = p[1]
          e = (d3 ? (p[2] or 0.0) : 0.0)
          evec = cis((e / @one_turn) * TWO_PI)
          dxy = d * evec.real
          avec = cis((a / @one_turn) * TWO_PI)
          z << (d * evec.imag)
          x << (dxy * avec.imag)
          y << (dxy * avec.real)
          v << (d3 ? p[3] : p[2])
        end
        [x, y, z, v]
      else
        if d3
          x = []
          y = []
          z = []
          0.step(points.length - 1, 3) do |i|
            d, a, e = points[i, 3]
            evec = cis((e / @one_turn) * TWO_PI)
            dxy = (d * evec.real)
            avec = cis((a / @one_turn) * TWO_PI)
            z << (d * evec.imag)
            x << (dxy * avec.imag)
            y << (dxy * avec.real)
          end
          [x, y, z, x.map do |i| nil end]
        else
          x = []
          y = []
          0.step(points.length - 1, 2) do |i|
            d, a = points[i, 2]
            avec = cis((a / @one_turn) * TWO_PI)
            x << (d * avec.imag)
            y << (d * avec.real)
          end
          [x, y, x.map do |i| 0.0 end, x.map do |i| nil end]
        end
      end
    end
  end

  class Bezier_path < Path
    def initialize(path, *args)
      @path = path
      if (not @path) or (array?(@path) and @path.empty?)
        dl_error("can't define a path with no points in it")
      end
      super()
      d3, polar, error, curvature = nil
      optkey(args, binding,
             [:d3, true],
             [:polar, false],
             [:error, 0.01],
             :curvature)
      @d3        = d3
      @polar     = polar
      @error     = error
      @curvature = curvature
      @x = @y = @z = @v = @bx = @by = @bz = nil
      # for ac() and a()
      @path_ak_even = nil
      @path_ak_odd = nil
      @path_gtab = nil
      @path_ftab = nil
    end

    private
    def parse_path
      if @polar
        @x, @y, @z, @v = parse_polar_coordinates(@path, @d3)
      else
        @x, @y, @z, @v = parse_cartesian_coordinates(@path, @d3)
      end
      if @v[0] and @v.min < 0.1
        if @v.min < 0.0
          Snd.warning("%s#%s: velocities must be all positive, corrected",
                      self.class, get_func_name)
        end
        @v.map! do |x| [0.1, x].max end
      end
      @bx = @by = @bz = nil
      reset_rendering()
    end

    def fit_path
      parse_path() if @x.nil?
    end

    def bezier_point(u, c)
      u1 = 1.0 - u
      cr = make_array(3) do |i| make_array(3) do |j| u1 * c[i][j] + u * c[i][j + 1] end end
      1.downto(0) do |i|
        0.upto(i) do |j|
          3.times do |k| cr[k][j] = u1 * cr[k][j] + u * cr[k][j + 1] end
        end
      end
      [cr[0][0], cr[1][0], cr[2][0]]
    end

    def berny(xl, yl, zl, xh, yh, zh, ul, u, uh, c)
      x, y, z = bezier_point(u, c)
      xn, yn, zn = nearest_point(xl, yl, zl, xh, yh, zh, x, y, z)
      if distance(xn - x, yn - y, zn - z) > @error
        xi, yi, zi = berny(xl, yl, zl, x, y, z, ul, (ul + u) / 2.0, u, c)
        xj, yj, zj = berny(x, y, z, xh, yh, zh, u, (u + uh) / 2.0, uh, c)
        [xi + [x] + xj, yi + [y] + yj, zi + [z] + zj]
      else
        [[], [], []]
      end
    end
    
    def render_path
      fit_path() if @bx.nil?
      rx = []
      ry = []
      rz = []
      rv = []
      if (not @v[0]) or @v[0].zero?
        @v[0] = 1.0
        @v[-1] = 1.0
      end
      if @x.length == 1
        @rx = @x
        @ry = @y
        @rz = @z
        @rt = [0.0]
        return
      end
      xf_bz = yf_bz = zf_bz = vf_bz = 0.0
      (@v.length - 1).times do |i|
        x_bz = @bx[i]
        y_bz = @by[i]
        z_bz = @bz[i]
        vi_bz, vf_bz = @v[i, 2]
        xi_bz = x_bz[0]
        xf_bz = x_bz[-1]
        yi_bz = y_bz[0]
        yf_bz = y_bz[-1]
        zi_bz = z_bz[0]
        zf_bz = z_bz[-1]
        xs, ys, zs = berny(xi_bz, yi_bz, zi_bz, xf_bz, yf_bz, zf_bz, 0, 0.5, 1, [x_bz, y_bz, z_bz])
        rx += [xi_bz] + xs
        ry += [yi_bz] + ys
        rz += [zi_bz] + zs
        rv += [vi_bz] + xs.map do nil end
      end
      rx << xf_bz
      ry << yf_bz
      rz << zf_bz
      rv << vf_bz
      xseg = [rx[0]]
      yseg = [ry[0]]
      zseg = [rz[0]]
      vi = rv[0]
      ti = 0.0
      times = [ti]
      (1...rx.length).each do |i|
        x = rx[i]
        y = ry[i]
        z = rz[i]
        v = rv[i]
        xseg << x
        yseg << y
        zseg << z
        if v
          sofar = 0.0
          dseg = (0...xseg.length - 1).map do |j|
            xsi, xsf = xseg[j, 2]
            ysi, ysf = yseg[j, 2]
            zsi, zsf = zseg[j, 2]
            sofar += distance(xsf - xsi, ysf - ysi, zsf - zsi)
          end
          df = dseg[-1]
          vf = v
          aa = ((vf - vi) * (vf + vi)) / (df * 4.0)
          tseg = dseg.map do |d|
            ti + (if vi and vi.nonzero? and vf == vi
                    d / vi
                  elsif aa.nonzero?
                    ((vi * vi + 4.0 * aa * d) ** 0.5 - vi) / (2.0 * aa)
                  else
                    0.0
                  end)
          end
          times += tseg
          xseg = [x]
          yseg = [y]
          zseg = [z]
          vi = v
          ti = tseg[-1]
        end
      end
      @rx = rx
      @ry = ry
      @rz = rz
      tf = times[-1]
      @rt = times.map do |tii| tii / tf end
      reset_transformation()
    end

    # called in Closed_bezier_path#calculate_fit
    def a(k, n)
      if ([Path_maxcoeff * 2.0 + 1, n].min).odd?
        make_a_odd() unless @path_ak_odd
        @path_ak_odd[(n - 3) / 2][k - 1]
      else
        make_a_even() unless @path_ak_even
        @path_ak_even[(n - 4) / 2][k - 1]
      end
    end
    
    # called in Open_bezier_path#calculate_fit
    def ac(k, n)
      n = [n, Path_maxcoeff].min
      make_a_even() unless @path_ak_even
      @path_ak_even[n - 2][k - 1]
    end

    def make_a_even
      g = lambda do |m|
        @path_gtab = make_array(Path_maxcoeff) unless @path_gtab
        @path_gtab[0] = 1.0
        @path_gtab[1] = -4.0
        (2...Path_maxcoeff).each do |i|
          @path_gtab[i] = -4.0 * @path_gtab[i - 1] - @path_gtab[i - 2]
        end
        @path_gtab[m]
      end
      @path_ak_even = make_array(Path_maxcoeff - 1)
      (1...Path_maxcoeff).each do |m|
        @path_ak_even[m - 1] = make_array(m)
        (1..m).each do |k|
          @path_ak_even[m - 1][k - 1] = (-g.call(m - k) / g.call(m)).to_f
        end
      end
    end

    def make_a_odd
      f = lambda do |m|
        @path_ftab = make_array(Path_maxcoeff) unless @path_ftab
        @path_ftab[0] = 1.0
        @path_ftab[1] = -3.0
        (2...Path_maxcoeff).each do |i|
          @path_ftab[i] = -4.0 * @path_ftab[i - 1] - @path_ftab[i - 2]
        end
        @path_ftab[m]
      end
      @path_ak_odd = make_array(Path_maxcoeff - 1)
      (1...Path_maxcoeff).each do |m|
        @path_ak_odd[m - 1] = make_array(m)
        (1..m).each do |k|
          @path_ak_odd[m - 1][k - 1] = (-f.call(m - k) / f.call(m)).to_f
        end
      end
    end
  end

  class Open_bezier_path < Bezier_path
    def initialize(path, *args)
      super
      initial_direction, final_direction = nil
      optkey(args, binding,
             [:initial_direction, [0.0, 0.0, 0.0]],
             [:final_direction, [0.0, 0.0, 0.0]])
      @initial_direction = initial_direction
      @final_direction   = final_direction
    end

    private
    def calculate_fit
      n = @x.length - 1
      m = n - 1
      p = [@x, @y, @z]
      d = make_array(3) do make_array(n + 1, 0.0) end
      d = Matrix[d[0], d[1], d[2]].to_a
      ref = lambda do |z, j, i|
        if i > n
          z[j][i - n]
        elsif i < 0
          z[j][i + n]
        elsif i == n
          z[j][n] - d[j][n]
        elsif i == 0
          z[j][0] + d[j][0]
        else
          z[j][i]
        end
      end
      d[0][0] = (@initial_direction[0] or 0.0)
      d[1][0] = (@initial_direction[1] or 0.0)
      d[2][0] = (@initial_direction[2] or 0.0)
      d[0][n] = (@final_direction[0] or 0.0)
      d[1][n] = (@final_direction[1] or 0.0)
      d[2][n] = (@final_direction[2] or 0.0)
      (1...n).each do |i|
        (1..[Path_maxcoeff - 1, m].min).each do |j|
          3.times do |k|
            d[k][i] = d[k][i] + ac(j, n) * (ref.call(p, k, i + j) - ref.call(p, k, i - j))
          end
        end
      end
      [n, p, d]
    end
    
    def fit_path
      parse_path() if @x.nil?
      case points = @x.length
      when 1
        @bx = @by = @bz = nil
      when 2
        x1, x2 = @x[0, 2]
        y1, y2 = @y[0, 2]
        z1, z2 = @z[0, 2]
        @bx = [[x1, x1, x2, x2]]
        @by = [[y1, y1, y2, y2]]
        @bz = [[z1, z1, z2, z2]]
      else
        n, p, d = calculate_fit()
        c = @curvature
        cs = make_array(n)
        if c.kind_of?(NilClass) or (array?(c) and c.empty?)
          n.times do |i| cs[i] = [1.0, 1.0] end
        elsif number?(c)
          n.times do |i| cs[i] = [c, c] end
        elsif array?(c) and c.length == n
          c.each_with_index do |ci, i|
            cs[i] = if array?(ci)
                      if ci.length != 2
                        dl_error(ci, "curvature sublist must have two elements")
                      else
                        ci
                      end
                    else
                      [ci, ci]
                    end
          end
        else
          dl_error(c, "bad curvature argument to path, need #{n} elements")
        end
        @bx = (0...n).map do |i|
          [p[0][i], p[0][i] + d[0][i] * cs[i][0], p[0][i + 1] - d[0][i + 1] * cs[i][1], p[0][i + 1]]
        end
        @by = (0...n).map do |i|
          [p[1][i], p[1][i] + d[1][i] * cs[i][0], p[1][i + 1] - d[1][i + 1] * cs[i][1], p[1][i + 1]]
        end
        @bz = (0...n).map do |i|
          [p[2][i], p[2][i] + d[2][i] * cs[i][0], p[2][i + 1] - d[2][i + 1] * cs[i][1], p[2][i + 1]]
        end
      end
      reset_rendering()
    end
  end

  class Closed_bezier_path < Bezier_path
    def initialize(path, *args)
      super
    end

    private
    def calculate_fit
      n = @x.length - 1
      m = (n - (n.odd? ? 3 : 4)) / 2
      p = [@x, @y, @z]
      d = make_array(3) do make_array(n, 0.0) end
      ref = lambda do |z, j, i|
        if i > (n - 1)
          z[j][i - n]
        elsif i < 0
          z[j][i + n]
        else
          z[j][i]
        end
      end
      n.times do |i|
        (1..m).each do |j|
          3.times do |k|
            d[k][i] = d[k][i] + a(j, n) * (ref.call(p, k, i + j) - ref.call(p, k, i - j))
          end
        end
      end
      if @curvature
        n.times do |i|
          curve = @curvature[i]
          d[0][i] *= curve
          d[1][i] *= curve
          d[2][i] *= curve
        end
      end
      [n - 1, p, d]
    end
    
    def fit_path
      parse_path() if @x.nil?
      if @x.length > 4
        n, p, d = calculate_fit()
        xc = (0...n).map do |i|
          [p[0][i], p[0][i] + d[0][i], p[0][i + 1] - d[0][i + 1], p[0][i + 1]]
        end
        yc = (0...n).map do |i|
          [p[1][i], p[1][i] + d[1][i], p[1][i + 1] - d[1][i + 1], p[1][i + 1]]
        end
        zc = (0...n).map do |i|
          [p[2][i], p[2][i] + d[2][i], p[2][i + 1] - d[2][i + 1], p[2][i + 1]]
        end
        @bx = xc + [[p[0][n], p[0][n] + d[0][n], p[0][0] - d[0][0], p[0][0]]]
        @by = yc + [[p[1][n], p[1][n] + d[1][n], p[1][0] - d[1][0], p[1][0]]]
        @bz = zc + [[p[2][n], p[2][n] + d[2][n], p[2][0] - d[2][0], p[2][0]]]
      else
        xc = []
        yc = []
        zc = []
        (@x.length - 1).times do |i|
          x1, x2 = @x[i, 2]
          y1, y2 = @y[i, 2]
          z1, z2 = @z[i, 2]
          xc << [x1, x1, x2, x2]
          yc << [y1, y1, y2, y2]
          zc << [z1, z1, z2, z2]
        end
        @bx = xc
        @by = yc
        @bz = zc
      end
      reset_rendering()
    end
  end

  class Literal_path < Path
    def initialize(path, *args)
      @path  = path
      if (not @path) or (array?(@path) and @path.empty?)
        dl_error("can't define a path with no points in it")
      end
      super()
      d3, polar = nil
      optkey(args, binding,
             [:d3, true],
             [:polar, false])
      @d3    = d3
      @polar = polar
    end

    private
    def render_path
      if @polar
        @rx, @ry, @rz, @rv = parse_polar_coordinates(@path, @d3)
      else
        @rx, @ry, @rz, @rv = parse_cartesian_coordinates(@path, @d3)
      end
      if (not @rv[0]) or @rv[0].zero?
        @rv[0] = 1.0
        @rv[-1] = 1.0
      end
      if @rx.length == 1
        @rt = [0.0]
        return
      end
      rx = @rx
      ry = @ry
      rz = @rz
      rv = @rv
      xseg = [rx[0]]
      yseg = [ry[0]]
      zseg = [rz[0]]
      vi = rv[0]
      ti = 0.0
      times = [ti]
      (1...rx.length).each do |i|
        x = rx[i]
        y = ry[i]
        z = rz[i]
        v = rv[i]
        xseg << x
        yseg << y
        zseg << z
        if v
          sofar = 0.0
          dseg = (0...xseg.length - 1).map do |j|
            xsi, xsf = xseg[j, 2]
            ysi, ysf = yseg[j, 2]
            zsi, zsf = zseg[j, 2]
            sofar += distance(xsf - xsi, ysf - ysi, zsf - zsi)
          end
          df = dseg[-1]
          vf = v
          aa = ((vf - vi) * (vf + vi)) / (df * 4.0)
          tseg = dseg.map do |d|
            ti + (if vi and vi.nonzero? and vf == vi
                    d / vi
                  elsif aa.nonzero?
                    ((vi * vi + 4.0 * aa * d) ** 0.5 - vi) / (2.0 * aa)
                  else
                    0.0
                  end)
          end
          times += tseg
          xseg = [x]
          yseg = [y]
          zseg = [z]
          vi = v
          ti = tseg[-1]
        end
      end
      tf = times[-1]
      @rt = times.map do |tii| tii / tf end 
      reset_transformation()
    end
  end
  
  class Spiral_path < Literal_path
    def initialize(*args)
      # to fool Literal_path.new
      super([nil])
      start_angle, turns = nil
      optkey(args, binding,
             [:start_angle, 0],
             [:turns, 2])
      @start_angle = start_angle
      @turns       = turns
    end

    private
    def render_path
      start = (@start_angle / @one_turn.to_f) * TWO_PI
      total = (@turns.zero? ? TWO_PI : (@turns * TWO_PI))
      step_angle = @one_turn / 100.0
      steps = (total / ((step_angle / @one_turn.to_f) * TWO_PI)).abs
      step = total / (steps.ceil * (step_angle < 0 ? -1 : 1))
      x = []
      y = []
      z = []
      (total / step).round.abs.times do
        xy = cis(start)
        x << (10.0 * xy.imag)
        y << (10.0 * xy.real)
        z << 0.0
        start += step
      end
      sofar = 0.0
      dp = (0...x.length - 1).map do |i|
        xi, xf = x[i, 2]
        yi, yf = y[i, 2]
        zi, zf = z[i, 2]
        sofar += distance(xf - xi, yf - yi, zf - zi)
      end
      td = 0.0
      times = (0...dp.length - 1).map do |i|
        di, df = dp[i, 2]
        td = td + (df - di) / 4.0
      end
      @rx = x
      @ry = y
      @rz = z
      tf = times[-1]
      @rt = times.map do |ti| ti / tf end 
      reset_transformation()
    end
  end

  module_function
  def make_dlocsig(start, dur, *args)
    dl = Dlocsig.new
    dl.make_dlocsig(start, dur, *args)
    dl
  end

  def dlocsig(dl, dloc, input)
    dl.dlocsig(dloc, input)
  end

  def make_path(path, *args)
    Open_bezier_path.new(path, *args)
  end
  
  def make_polar_path(path, *args)
    Open_bezier_path.new(path, :polar, true, *args)
  end
  
  def make_closed_path(path, *args)
    d3 = nil
    optkey(args, binding, [:d3, true])
    len = d3 ? 3 : 2
    unless path[0][0, len] == path[-1][0, len]
      path += [path[0]]
    end
    Closed_bezier_path.new(path, *args)
  end

  def make_literal_path(path, *args)
    Literal_path.new(path, *args)
  end
  
  def make_literal_polar_path(path, *args)
    Literal_path.new(path, :polar, true, *args)
  end

  def make_spiral_path(*args)
    Spiral_path.new(*args)
  end
end

# example functions (see clm-2/dlocsig/move-sound.ins and
# clm-2/dlocsig/dlocsig.html)

class Instrument
  def sinewave(start, dur, freq, amp, path, amp_env = [0, 1, 1, 1], *dlocsig_args)
    os = make_oscil(:frequency, freq)
    en = make_env(:envelope, amp_env, :scaler, amp, :duration, dur)
    run_dlocsig(start, dur, :path, path, *dlocsig_args) do
      env(en) * oscil(os)
    end
  end

  def move(start, file, path, *dlocsig_args)
    dl_error(path, "need a path") unless path.kind_of?(DL::Path)
    dur = ws_duration(file)
    chns = ws_channels(file)
    rds = make_array(chns) do |chn| make_ws_reader(file, :start, start, :channel, chn) end
    run_dlocsig(start, dur, :path, path, *dlocsig_args) do
      rds.map do |rd| ws_readin(rd) end.sum / chns
    end
  end

  add_help(:move_sound,
           "move_sound(path, *args) do ... end
 sound_let-args:
     :channels,    1  # channels to move
 start time in output file:
     :startime,    0
 rest args: make_dlocsig")
  def move_sound(path, *args, &body)
    chns        = get_shift_args(args, :channels, 1)
    start       = get_shift_args(args, :startime, 0)
    sound_let([:channels, chns, body]) do |to_move|
      if @verbose
        Snd.display("%s: moving sound on %d channel%s", get_func_name, chns, (chns > 1 ? "s" : ""))
      end
      move(0, to_move, path, *args)
      rbm_mix(to_move, :output_frame, seconds2samples(start))
    end
  end
end

class With_sound
  def run_dlocsig(start, dur, *dlocsig_args, &body)
    with_sound_info(get_func_name(2), start, dur)
    dl = DL.make_dlocsig(start, dur,
                         :clm, @clm,
                         :rbm_output, @ws_output,
                         :rbm_reverb, @ws_reverb,
                         :out_channels, @channels,
                         :rev_channels, @reverb_channels,
                         *dlocsig_args)
    dl.ws_dlocsig(&body)
  end
end

# Dlocsig menu
#
# require 'snd-xm'
#
# make_snd_menu("Dlocsig") do
#   cascade("Dlocsig (Snd)") do
#     entry(Dlocsig_bezier, "Bezier path (Snd)", true)
#     entry(Dlocsig_spiral, "Spiral path (Snd)", true)
#   end
#   cascade("Dlocsig (CLM)") do
#     entry(Dlocsig_bezier, "Bezier path (CLM)", false)
#     entry(Dlocsig_spiral, "Spiral path (CLM)", false)
#   end
# end

if provided? :snd_motif or provided? :snd_gtk
  class Dlocsig_menu
    require "snd-xm"
    include Snd_XM
    require "xm-enved"
    include DL

    def initialize(label, snd_p)
      @label = label
      @snd_p = snd_p
      @dialog = nil
      @out_chans = 4
      @rev_chans = 1
      @path = nil
      @sliders = []
      @init_out_chans = 4
      @output_power = @init_power = 1.5
      @reverb_power = @init_rev_power = 0.5
      @render_using = Amplitude_panning
    end

    private
    def with_sound_target(*comment_args)
      if @render_using == B_format_ambisonics
        set_scale_value(@sliders[0].scale, @out_chans = 4)
      end
      comment_string = if string?($clm_comment) and !$clm_comment.empty?
                         format("%s; %s", $clm_comment, format(*comment_args))
                       else
                         format(*comment_args)
                       end
      snd_path, snd_name = File.split(file_name(selected_sound))
      snd_name = snd_name.split("moved-").last
      snd_to_move = format("%s/%s", snd_path, snd_name)
      snd_moved = format("%s/moved-%s", snd_path, snd_name)
      path = @path
      output_power = @output_power
      reverb_power = @reverb_power
      render_using = @render_using
      f = with_sound(:clm, (not @snd_p),
                     :output, snd_moved,
                     :channels, @out_chans,
                     :reverb_channels, @rev_chans,
                     :comment, comment_string,
                     :info, (not $clm_notehook),
                     :statistics, true,
                     :play, true) do
        move(0,
             snd_to_move,
             path,
             :output_power, output_power,
             :reverb_power, reverb_power,
             :render_using, render_using)
      end
      Snd.display(f.output.inspect)
    rescue
      Snd.warning("%s#%s: %s", self.class, get_func_name, comment_string)
    end

    def add_with_sound_sliders(parent = @dialog.parent)
      @sliders << @dialog.add_slider("output channels",
                               2, @init_out_chans, 8, 1, :linear, parent) do |w, c, i|
        @out_chans =   get_scale_value(w, i).round
      end
      @sliders <<  @dialog.add_slider("output power",
                                0, @init_power, 10, 100, :linear, parent) do |w, c, i|
        @output_power = get_scale_value(w, i, 100.0)
      end
      @sliders << @dialog.add_slider("reverb power",
                                     0, @init_rev_power, 10, 100, :linear, parent) do |w, c, i|
        @reverb_power = get_scale_value(w, i, 100.0)
      end
    end

    def reset_with_sound_sliders(reverb_p = true)
      set_scale_value(@sliders[0].scale, @out_chans = @init_out_chans)
      @output_power = @init_power
      set_scale_value(@sliders[1].scale, @output_power, 100.0)
      @reverb_power = @init_rev_power
      set_scale_value(@sliders[2].scale, @reverb_power, 100.0)
    end
    
    def add_with_sound_targets
      @dialog.add_target([["amplitude panning", :amplitude, true],
                          ["b format ambisonics", :b_format, false],
                          ["decoded ambisonics", :decoded, false]]) do |val|
        @render_using = case val
                        when :amplitude
                          Amplitude_panning
                        when :b_format
                          set_scale_value(@sliders[0].scale, @out_chans = 4)
                          B_format_ambisonics
                        when :decoded
                          Decoded_ambisonics
                        end
      end
      @dialog.add_target([["no reverb", :no_reverb, false],
                          ["1 rev chan", :one_rev_chan, true],
                          ["4 rev chans", :four_rev_chans, false]]) do |val|
        case val
        when :no_reverb
          @rev_chans = 0
        when :one_rev_chan
          @rev_chans = 1
        when :four_rev_chans
          @rev_chans = 4
        end
      end
    end

    def set_xm_enveds_hooks(*enveds)
      enveds.each do |e|
        e.before_enved_hook.reset_hook! # to prevent running $enved_hook
        e.before_enved_hook.add_hook!("dlocsig-hook") do |pos, x, y, reason|
          if reason == Enved_move_point
            if e.in_range?(x)
              old_x = e.point(pos).first
              e.stretch!(old_x, x)
              e.point(pos, :y, y)
            else
              false
            end
          else
            false
          end
        end
        e.after_enved_hook.add_hook!("dlocsig-hook") do |pos, reason| show_values end
      end
    end

  # comment string
  def dlocsig_strings
    dlstr = ["", :amplitude_panning, :b_format_ambisonics, :decoded_ambisonics]
    format("%s, output_power: %1.2f, reverb_power: %1.2f",
           dlstr[@render_using],
           @output_power,
           @reverb_power)
  end
    
    def help_cb
      help_dialog(@label,
                  "\
The current sound will be moved through the chosen path.  You can set \
the reverberator via the global with-sound-variable $clm_reverb \
(#{$clm_reverb.inspect}).  If you want four reverb channels, you \
may try freeverb from freeverb.rb.

reverb    reverb-channels output-channels source

jc_reverb        1               4        examp.rb
jl_reverb        1               2        clm-ins.rb
nrev             1               4        clm-ins.rb
freeverb         4               > 4      freeverb.rb

Amplitude-panning: generates amplitude panning between adjacent speakers.

B-format-ambisonics: generates a four channel first order b-format encoded soundfile.

Decoded-ambisonics: the ambisonics encoded information is decoded to the number of selected speakers.

Note: reverb on spiral path generates noise if turns is less than 2.6

For detailed information see clm-2/dlocsig.html.",
                  ["{Libxm}: graphics module",
                   "{Ruby}: extension language",
                   "{Motif}: Motif extensions via Libxm",
                   "{dlocsig}: Fernando Lopez Lezcano's multichannel locator"])
    end
  end
  
  class Dlocsig_bezier < Dlocsig_menu
    require "xm-enved"
    
    def initialize(label, snd_p = false)
      super
      @target = :with_sound
      @which_path = :open_bezier_path
      @snd_path = [[-10.0, 10.0, 0.0, 1.0], [0.0, 5.0, 1.0, 1.0], [10.0, 10.0, 0.0, 1.0]]
      @trajectory = nil
      @z_value = nil
      @velocity = nil
      @label_list = []
    end

    def inspect
      str = @snd_path.inspect
      new_str = ""
      [str.length, 20].min.times do |i| new_str << str[i] end
      new_str << "..." if str.length > 20
      format("%s (%s)", @label, new_str)
    end
    
    def post_dialog
      unless @dialog.kind_of?(Dialog) and widget?(@dialog.dialog)
        init_traj = [0, 1, 0.5, 0.5, 1, 1]
        init_z_traj = [0, 0, 0.5, 0.1, 1, 0]
        init_vel = [0, 0.5, 1, 0.5]
        @dialog = make_dialog(@label,
                              :help_cb, lambda do |w, c, i|
                                help_cb()
                              end, :clear_cb, lambda do |w, c, i|
                                create_path
                                @path.pplot
                              end, :reset_cb, lambda do |w, c, i|
                                reset_with_sound_sliders
                                @trajectory.envelope = init_traj
                                @z_value.envelope = init_z_traj
                                @velocity.envelope = init_vel
                                show_values
                              end) do |w, c, i|
          create_path
          with_sound_target("%s: %s, path: %s", @which_path, dlocsig_strings, @snd_path.inspect)
        end
        if provided? :xm
          frame_args = [RXmNshadowThickness, 4,
                        RXmNshadowType, RXmSHADOW_ETCHED_OUT,
                        RXmNbackground, basic_color,
                        RXmNheight, 170,
                        RXmNwidth, 400]
          pane = RXtCreateManagedWidget("pane", RxmPanedWindowWidgetClass, @dialog.parent,
                                        [RXmNsashHeight, 1, RXmNsashWidth, 1,
                                         RXmNorientation, RXmHORIZONTAL,
                                         RXmNbackground, basic_color])
          xepane = RXtCreateManagedWidget("xepane", RxmPanedWindowWidgetClass, pane,
                                          [RXmNsashHeight, 1, RXmNsashWidth, 1,
                                           RXmNorientation, RXmVERTICAL,
                                           RXmNbackground, basic_color])
          trfr = RXtCreateManagedWidget("trfr", RxmFrameWidgetClass, xepane, frame_args)
          zfr = RXtCreateManagedWidget("zfr", RxmFrameWidgetClass, xepane, frame_args)
          vefr = RXtCreateManagedWidget("vefr", RxmFrameWidgetClass, xepane, frame_args)
          vepane = RXtCreateManagedWidget("vpane", RxmPanedWindowWidgetClass, pane,
                                          [RXmNsashHeight, 1, RXmNsashWidth, 1,
                                           RXmNseparatorOn, true,
                                           RXmNorientation, RXmVERTICAL,
                                           RXmNbackground, basic_color])
          add_with_sound_sliders(vepane)
          rc = RXtCreateManagedWidget("form", RxmRowColumnWidgetClass, vepane,
                                      [RXmNorientation, RXmVERTICAL,
                                       RXmNalignment, RXmALIGNMENT_CENTER])
          @label_list = make_array(8) do |i|
            RXtCreateManagedWidget("W" * 30, RxmLabelWidgetClass, rc, [])
          end
          add_with_sound_targets
          @dialog.add_target([["open bezier", :open_bezier_path, true],
                              ["closed bezier", :closed_bezier_path, false],
                              ["literal", :literal_path, false]]) do |val|
            @which_path = val
            create_path
          end
          activate_dialog(@dialog.dialog)
          @trajectory = make_xenved("x, y", trfr,
                                    :envelope, init_traj,
                                    :axis_bounds, [0.0, 1.0, 0.0, 1.0],
                                    :axis_label, [-10.0, 10.0, 0.0, 10.0])
          @z_value = make_xenved("z", zfr,
                                 :envelope, init_z_traj,
                                 :axis_bounds, [0.0, 1.0, 0.0, 1.0],
                                 :axis_label, [-10.0, 10.0, 0.0, 10.0])
          @velocity = make_xenved("velocity v", vefr,
                                  :envelope, init_vel,
                                  :axis_bounds, [0.0, 1.0, 0.05, 1.0],
                                  :axis_label, [-10.0, 10.0, 0.0, 2.0])
        else
          pane = Rgtk_hbox_new(false, 0)
          Rgtk_box_pack_start(RGTK_BOX(@dialog.parent), pane, false, false, 4)
          Rgtk_widget_show(pane)
          xepane = Rgtk_vbox_new(true, 0)
          Rgtk_box_pack_start(RGTK_BOX(pane), xepane, true, true, 4)
          Rgtk_widget_show(xepane)
          activate_dialog(@dialog.dialog)
          @trajectory = make_xenved("x, y", xepane,
                                    :envelope, init_traj,
                                    :axis_bounds, [0.0, 1.0, 0.0, 1.0],
                                    :axis_label, [-10.0, 10.0, 0.0, 10.0])
          @z_value = make_xenved("z", xepane,
                                 :envelope, init_z_traj,
                                 :axis_bounds, [0.0, 1.0, 0.0, 1.0],
                                 :axis_label, [-10.0, 10.0, 0.0, 10.0])
          @velocity = make_xenved("velocity v", xepane,
                                  :envelope, init_vel,
                                  :axis_bounds, [0.0, 1.0, 0.05, 1.0],
                                  :axis_label, [-10.0, 10.0, 0.0, 2.0])
          vepane = Rgtk_vbox_new(false, 0)
          Rgtk_box_pack_start(RGTK_BOX(pane), vepane, false, false, 4)
          Rgtk_widget_show(vepane)
          add_with_sound_sliders(vepane)
          @label_list = make_array(8) do |i|
            lab = Rgtk_label_new("W" * 30)
            Rgtk_box_pack_start(RGTK_BOX(vepane), lab, false, false, 4)
            Rgtk_widget_show(lab)
            lab
          end
          add_with_sound_targets
          @dialog.add_target([["open bezier", :open_bezier_path, true],
                              ["closed bezier", :closed_bezier_path, false],
                              ["literal", :literal_path, false]]) do |val|
            @which_path = val
            create_path
          end
        end
        set_xm_enveds_hooks(@trajectory, @z_value, @velocity)
        @dialog.clear_string("Gnuplot")
        @dialog.doit_string((@snd_p ? "With_Snd" : "With_Sound"))
        show_values
      else
        activate_dialog(@dialog.dialog)
      end
    end

    private
    def create_path
      test_path
      @path = case @which_path
              when :open_bezier_path
                make_path(@snd_path)
              when :closed_bezier_path
                make_closed_path(@snd_path)
              when :literal_path
                make_literal_path(@snd_path)
              end
    end
    
    def test_path
      if @snd_path.length == 2
        Snd.display("%s#%s: path has only two points, one added", self.class, get_func_name)
        @snd_path.insert(1, [0.0, @snd_path[0][1] - 0.1, 0.0, @snd_path[0][3] + 0.1])
      end
      unless @snd_path.detect do |pnt| pnt[1].nonzero? end
        Snd.display("%s#%s: y-values are all zero, changed to mid-point 0.1",
                    self.class, get_func_name)
        @snd_path[@snd_path.length / 2][1] = 0.1
      end
    end
    
    def points_to_path
      @snd_path = []
      @trajectory.each do |x, y|
        z = @z_value.interp(x)
        vel = @velocity.interp(x)
        @snd_path.push([x * 20.0 - 10.0, y * 10.0, z * 10.0, vel * 2.0])
      end
    end
    
    def show_values
      points_to_path
      @label_list.each_with_index do |w, i|
        if i.zero?
          change_label(w, format("%6s %6s %6s %6s", "x", "y", "z", "v"))
        else
          x, y, z, v = @snd_path[i - 1]
          if x
            change_label(w, format("%s %s %s %s",
                                   to_f_str(x), to_f_str(y), to_f_str(z), to_f_str(v)))
          else
            change_label(w, "")
          end
        end
      end
    end

    def to_f_str(val)
      "%6s" % ("% 3.1f" % val)
    end
  end

  class Dlocsig_spiral < Dlocsig_menu
    def initialize(label, snd_p = false)
      super
      @start = 0
      @turns = 3.0
    end

    def inspect
      format("%s (%d %1.1f)", @label, @start, @turns)
    end
    
    def post_dialog
      unless @dialog.kind_of?(Dialog) and widget?(@dialog.dialog)
        sliders = []
        init_start = 0
        init_turns = 3.0
        @dialog = make_dialog(@label,
                              :help_cb, lambda do |w, c, i|
                                help_cb
                              end, :clear_cb, lambda do |w, c, i|
                                make_spiral_path(:start_angle, @start, :turns, @turns).pplot
                              end, :reset_cb, lambda do |w, c, i|
                                reset_with_sound_sliders
                                set_scale_value(sliders[0].scale, @start = init_start)
                                @turns = init_turns
                                set_scale_value(sliders[1].scale, init_turns, 10.0)
                              end) do |w, c, i|
          @path = make_spiral_path(:start_angle, @start, :turns, @turns)
          with_sound_target("spiral_path: %s, start: %d, turns: %1.1f",
                            dlocsig_strings, @start, @turns)
        end
        add_with_sound_sliders(if provided? :xg
                                 @dialog.dialog
                               else
                                 @dialog.parent
                               end)
        sliders << @dialog.add_slider("start angle", 0, init_start, 360) do |w, c, i|
          @start = get_scale_value(w, i)
        end
        # turns below 2.6 together with reverb create noise
        sliders << @dialog.add_slider("turns", 2.6, init_turns, 10.0, 10) do |w, c, i|
          @turns = get_scale_value(w, i, 10.0)
        end
        add_with_sound_targets
        @dialog.clear_string("Gnuplot")
        @dialog.doit_string((@snd_p ? "With_Snd" : "With_Sound"))
      end
      activate_dialog(@dialog.dialog)
    end
  end

  unless defined? $__private_dlocsig_menu__ and $__private_dlocsig_menu__
    snd_main = make_snd_menu("Dlocsig") do
      cascade("Dlocsig (Snd)") do
        entry(Dlocsig_bezier, "Bezier path (Snd)", true)
        entry(Dlocsig_spiral, "Spiral path (Snd)", true)
      end
      cascade("Dlocsig (CLM)") do
        entry(Dlocsig_bezier, "Bezier path (CLM)", false)
        entry(Dlocsig_spiral, "Spiral path (CLM)", false)
      end
    end

    if provided? :xm
      set_label_sensitive(menu_widgets[Top_menu_bar], "Dlocsig", ((sounds() or []).length > 1))
    else
      set_sensitive(snd_main.menu, ((sounds() or []).length > 1))
    end
    
    unless $open_hook.member?("dlocsig-menu-hook")
      $open_hook.add_hook!("dlocsig-menu-hook") do |snd|
        if provided? :xm
          set_label_sensitive(menu_widgets[Top_menu_bar], "Dlocsig", true)
        else
          set_sensitive(snd_main.menu, true)
        end
        false
      end
      
      $close_hook.add_hook!("dlocsig-menu-hook") do |snd|
        if provided? :xm
          set_label_sensitive(menu_widgets[Top_menu_bar], "Dlocsig", ((sounds() or []).length > 1))
        else
          set_sensitive(snd_main.menu, ((sounds() or []).length > 1))
        end
        false
      end
    end
  end
end

# JC_REVERB (examp.rb) default options
#
# :low_pass, false
# :volume,   1.0
# :amp_env,  false
# :delay1,   0.013
# :delay2,   0.011
# :delay3,   0.015
# :delay4,   0.017
# :double,   false
#
# $clm_reverb = :jc_reverb_rb
# $clm_reverb_data = [:low_pass, false, :volume, 1.0, :amp_env, false,
#                    :delay1, 0.013, :delay2, 0.011, :delay3, 0.015, :delay4, 0.017]

# require "clm-ins"
#
# JL_REVERB has no options
#
# $clm_reverb = :jl_reverb
# $clm_reverb_data = []

# NREV default options
# 
# :reverb_factor, 1.09
# :lp_coeff,      0.7
# :volume,        1.0
#
# $clm_reverb = :nrev_rb
# $clm_reverb_data = [:volume, 1.0, :lp_coeff, 0.7]

# INTERN or N_REV default options (only with_snd)
#
# :amount,   0.1
# :filter,   0.5
# :feedback, 1.09
#
# $clm_reverb = :intern
# $clm_reverb_data = [:amount, 0.1, :filter, 0.5, :feedback, 1.09]

# require "freeverb"
# 
# FREEVERB default options
#
# :room_decay,        0.5,
# :damping,           0.5,
# :global,            0.3,
# :predelay,          0.03,
# :output_gain,       1.0,
# :output_mixer,      nil,
# :scale_room_decay,  0.28,
# :offset_room_decay, 0.7,
# :combtuning,        [1116, 1188, 1277, 1356, 1422, 1491, 1557, 1617],
# :allpasstuning,     [556, 441, 341, 225],
# :scale_damping,     0.4,
# :stereo_spread,     23,
#
# $clm_reverb = :freeverb
# $clm_reverb_data = [:room_decay, 0.5, :damping, 0.5, :global, 0.3, :predelay, 0.03,
#                     :output_gain, 1.0, :output_mixer, nil, :scale_room_decay, 0.7,
#                     :scale_damping, 0.4, :stereo_spread, 23]

=begin
# (snd-ruby-mode)
# Examples:

(with_sound(:channels, 4, :output, "rdloc04.snd") do
   sinewave(0, 1, 440, 0.5, [[-10, 10], [0, 5], [10, 10]].to_path)
 end)

(with_sound(:channels, 4, :output, "rdlocspiral.snd") do
   move(0, "/usr/gnu/sound/SFiles/bell.snd", DL.make_spiral_path(:start_angle, 180, :turns, 3.5))
 end)

([[-10, 10, 0, 0], [0, 5, 0, 1], [10, 10, 0, 0]].to_path(:error, 0.01).plot_velocity)

(with_sound(:channels, 4, :output, "rdlocmove.snd") do
   move_sound(DL.make_path([[-10, 10], [0.1, 0.1], [10, -10]])) do
     fm_violin_rb(0, 1, 440, 0.1)
     fm_violin_rb(0.3, 2, 1020, 0.05)
   end
 end)
=end

# dlocsig.rb ends here
