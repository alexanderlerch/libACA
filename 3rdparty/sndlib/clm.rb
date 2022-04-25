# clm.rb -- Ruby extension

# Author: Michael Scholz <mi-scholz@users.sourceforge.net>
# Created: Wed Oct 14 23:02:57 CEST 2009
# Changed: Thu Jun 13 15:39:27 CEST 2013

# Commentary:
#
# Ruby extensions:
# 
# array?(obj)   alias list?(obj)
# hash?(obj)
# string?(obj)
# regexp?(obj)
# symbol?(obj)
# number?(obj)
# integer?(obj)
# float?(obj)
# rational?(obj)
# complex?(obj)
# boolean?(obj)
# proc?(obj)
# thunk?(obj)
# method?(obj)
# func?(obj)
# mus?(obj)
# get_func_name(n)
# assert_type(condition, obj, pos, msg)
# identity(arg)
# ignore(*rest)
# with_silence(exception) do |old_verbose, old_debug| ... end
# 
# provided?(feature)
# provide(feature)
# features(all)
#
# Backward compatibility aliases and constants (from sndXX.scm)
# 
# enum(*names)
#
# class Object
#  null?
#  function?(obj)
#  snd_func(name, *rest, &body)
#  set_snd_func(name, val, *rest, &body)
#  snd_apropos(str_or_sym)
#
# NilClass(arg)
# Fixnum(arg)
#
# class NilClass
#  each
#  apply(func, *rest, &body)
#  empty?
#  zero?
#  nonzero?
#  to_vct
#  to_vector
#  to_poly
#  +(other)
#  -(other)
#  *(other)
#  
# backward compatibility methods:
#  String#to_sym, Symbol#to_sym
#  make_array(len, init, &body)
#  Array#insert
#  Float#step
#  Range#step
#  Enumerable#each_index
#  Enumerable#zip
#
# class Array
#  to_pairs
#  each_pair do |x, y| ... end
#  to_string(len)
#  first=(val)
#  last=(val)
#  pick
#  rand
#  rand!
#  add(other)
#  add!(other)
#  subtract(other)
#  subtract!(other)
#  multiply(other)
#  multiply!(other)
#  offset(scl)
#  offset!(scl)
#  scale(scl)
#  scale!(scl)
#  to_vector
#  car
#  car=
#  cadr
#  cadr=
#  caddr
#  caddr=
#  cadddr
#  cadddr=
#  caddddr
#  caddddr=
#  cdr
#  step(n)
#  apply(func, *rest, &body)
#
# class Vec < Array
#  Vec[]
#  initialize(len, init, &body)
#  inspect
#  to_s
#  to_vector
#  +(other)
#  -(other)
#  *(other)
#
# Vec(obj)
# make_vector(len, init, &body)
# vector?(obj)
# vector(*args)
#
# class String
#  to_vector
#  to_vct
#
# Vct(obj)
# make_vct!(len, init) do |i| ... end
#
# class Vct
#  Vct[]
#  name
#  to_sound_data(sd, chn)
#  to_vct
#  to_vector
#  apply(func, *rest, &body)
#  +(other)   handles self.offset (Numeric) and self.add (Array, Vec, Vct)
#  -(other)   handles self.offset (Numeric) and self.subtract (Array, Vec, Vct)
#  *(other)   handles self.scale (Numeric) and self.multiply (Array, Vec, Vct)
#  step(n)
#  [](idx, size)
#
# class Fixnum
#  +(other)   handles other.offset on Vct, Array, and Vec
#  *(other)   handles other.scale on Vct, Array, and Vec
#
# class Float
#  +(other)   handles other.offset on Vct, Array, and Vec
#  *(other)   handles other.scale on Vct, Array, and Vec
#
# SoundData(ary)          can be used to reread evaled output from sound_data2string
# sound_data2string(sd)   produces a string which can be evaled and reread with SoundData
#
# class SoundData
#  name
#  to_vct(chn)
#  to_a
#  length
#  scale!(scl)
#  fill!(val)
#  each(chn)
#  each_with_index(chn)
#  map(chn)
#  map!(chn)
#
# mus_a0(gen)
# set_mus_a0(gen, val)
# mus_a1(gen)
# set_mus_a1(gen, val)
# mus_a2(gen)
# set_mus_a2(gen, val)
# mus_b1(gen)
# set_mus_b1(gen, val)
# mus_b2(gen)
# set_mus_b2(gen, val)
#
# class Mus
#  run(arg1, arg2)
#  apply(*rest)
#  inspect
#  close
#  xcoeff=(index, val)
#  ycoeff=(index, val)
#  a0  a0=(val)
#  a1  a1=(val)
#  a2  a2=(val)
#  b1  b1=(val)
#  b2  b2=(val)
#
# class Musgen   base class for generators written in Ruby
#  initialize
#  inspect
#  to_s
#  run(val1, val2)
#  apply(*rest)
#  eql?(other)
#  reset
#
# class Numeric
#  positive?
#  negative?
#
# class Integer
#  even?
#  odd?
#  prime?
#
# module Enumerable
#  map_with_index do |x, i| ... end
#  map_with_index! do |x, i| ... end
#  clm_cycle
#  clm_cycle=(val)
#
# as_one_edit_rb(*origin, &body)
# map_channel_rb(beg, dur, snd, chn, edpos, edname, &body)
# map_chan_rb(beg, dur, edpos, snd, chn, &body)
# 
# module Info
#  description=(text)
#  description
#
# class Proc
#  to_method(name, klass)
#  to_str
#  to_body
#  source
#  source=
#
# make_proc2method(name, prc)
# make_proc_with_setter(name, getter, setter)
# make_proc_with_source(string, bind)
# proc_source(prc)   set_proc_source(prc, val)
#
# Multi-line input to the Snd listener and Emacs/inf-snd.el
# 
# $emacs_eval_hook.call(line)
# run_emacs_eval_hook(line)
# 
# class Snd_eval
#  Snd_eval.count_level(line)
#
# class Snd_prompt
#  initialize(level)
#  inspect
#  update(level)
#  reset
#  
# start_emacs_eval(file)
# start_listener_eval(file)
# stop_emacs_eval
# stop_listener_eval
#
# Debugging resp. inspecting local variables
# 
# debug_properties(name)      set_debug_properties(name, val)
# debug_property(key, name)   set_debug_property(key, val, name)
# debug_binding(name)         set_debug_binding(bind, name)
# display_all_variables(name)
# each_variables(bind, &body)
#
# let(*rest) do |*rest| ... end
#
# Utilities:
#
# close_sound_extend(snd)
# times2samples(start, dur)
# random(n)
# logn(r, b)
# car(v), cadr(v), caddr(v), cdr(v)
# warning(*args), die(*args), error(*args)
# clm_message(*args), message(*args), debug(*args), debug_trace(*args)
#
# class Snd
#  Snd.add_sound_path(path)
#  Snd.open_from_path(fname)
#  Snd.find_from_path(fname)
#  Snd.fullname(fname)
#  Snd.load_path
#  Snd.message(*args)
#  Snd.display(*args)
#  Snd.warning(*args)
#  Snd.die(*args)
#  Snd.error(*args)
#  Snd.debug(*args)
#  Snd.debug_trace(*args)
#  Snd.sounds
#  Snd.regions
#  Snd.marks(snd, chn)
#  Snd.snd(snd)
#  Snd.chn(chn)
#  Snd.catch(tag, retval)
#  Snd.throw(tag, *rest)
#  Snd.raise(tag, *rest)
#
# snd_catch(tag, retval)
# snd_throw(tag, *rest)
# snd_raise(tag, *rest)
#
# gloop(*args) do |args| ... end
# get_args(args, key, default)
# get_shift_args(args, key, default)
# get_class_or_key(args, klass, key, default)
# optkey(args, *rest)
# load_init_file(file)
#
# edit_list_proc_counter
# set_edit_list_proc_counter

def make_polar(r, theta)
  Complex(cos(theta) * r, sin(theta) * r)
end

def make_rectangular(re, im = 1.0)
  Complex(re, im)
end

def array?(obj)
  obj.kind_of?(Array)
end
alias list? array?

def hash?(obj)
  obj.kind_of?(Hash)
end

def string?(obj)
  obj.kind_of?(String)
end

def regexp?(obj)
  obj.kind_of?(Regexp)
end

def symbol?(obj)
  obj.kind_of?(Symbol)
end

def number?(obj)
  obj.kind_of?(Numeric)
end

def integer?(obj)
  obj.kind_of?(Fixnum)
end

def float?(obj)
  obj.kind_of?(Float)
end

def rational?(obj)
  obj.kind_of?(Rational)
end

def complex?(obj)
  obj.kind_of?(Complex)
end

def boolean?(obj)
  obj.kind_of?(TrueClass) or obj.kind_of?(FalseClass)
end

def proc?(obj)
  obj.kind_of?(Proc)
end

def thunk?(obj)
  obj.kind_of?(Proc) and obj.arity.zero?
end

def method?(obj)
  obj.kind_of?(Method)
end

def func?(obj)
  obj.kind_of?(String) or obj.kind_of?(Symbol)
end

def mus?(obj)
  obj.kind_of?(Mus)
end

def binding?(obj)
  obj.kind_of?(Binding)
end

def get_func_name(n = 1)
  if ca = caller(n)[0].scan(/^.*:in `(.*)'/).first
    ca.first
  else
    "top_level"
  end
end

def assert_type(condition, obj, pos, msg)
  condition or Kernel.raise(TypeError,
                            format("%s: wrong type arg %d, %s, wanted %s",
                                   get_func_name(2), pos, obj.inspect, msg))
end

def identity(arg)
  arg
end

def ignore(*rest)
  nil
end

unless defined? $LOADED_FEATURES
  alias $LOADED_FEATURES $"
end

def provided?(feature)
  $LOADED_FEATURES.map do |f| File.basename(f) end.member?(feature.to_s.tr("_", "-"))
end

def provide(feature)
  $LOADED_FEATURES.push(feature.to_s)
end

def features(all = nil)
  if all
    $LOADED_FEATURES.map do |f| File.basename(f) end
  else
    $LOADED_FEATURES.map do |f|
      next if f.include?("/") or f.include?(".")
      f
    end.compact
  end
end

# with_silence(exception) do |old_verbose, old_debug| ... end
# 
# subpress debug messages (mostly on older Ruby versions)
# 
# with_silence do $global_var ||= value end
# with_silence(LoadError) do require("nonexistent.file") end
def with_silence(exception = StandardError)
  old_verbose = $VERBOSE
  old_debug   = $DEBUG
  $VERBOSE = false
  $DEBUG   = false
  ret = if block_given?
          begin
            yield(old_verbose, old_debug)
          rescue exception
            false
          end
        else
          false
        end
  $VERBOSE = old_verbose
  $DEBUG   = old_debug
  ret
end

include Math

TWO_PI  = PI * 2.0 unless defined? TWO_PI
HALF_PI = PI * 0.5 unless defined? HALF_PI

# ruby19 moved complex.rb and rational.rb to C
# (See ruby/Changelog Sun Mar 16 08:51:41 2008.)
# FIXME
# Thu Nov 18 01:26:45 CET 2010
# with ruby19 and C Complex
# val < 1.0 ==> Math::DomainError: Numerical argument is out of domain - "acosh"
#unless defined? Complex
with_silence do
  # lib/complex.rb is deprecated
  require "complex"
end
unless defined? Rational
  with_silence do
    # warning: method redefined; discarding old numerator|denominator|gcd|lcm
    require "rational"
  end
end

# ruby19 moved Continuation to 'continuation'!
# (See ruby/ChangeLog Tue Jan 20 16:17:12 2009.)
unless defined? Kernel.callcc
  require "continuation"
end

unless provided?(:sndlib)
  with_silence do
    # warning: method redefined; discarding old rand
    require "sndlib"
  end
end

#
# Backward compatibility aliases and constants (from sndXX.scm)
# 
# alias new old
if provided? :snd
  alias save_options                    save_state
  alias delete_samples_with_origin      delete_samples
  alias default_output_type             default_output_header_type
  alias default_output_format           default_output_data_format
  alias mus_audio_set_oss_buffers       mus_oss_set_buffers
  unless defined? mus_file_data_clipped
    alias mus_file_data_clipped         mus_clipping
    alias set_mus_file_data_clipped     set_mus_clipping
  end
  alias mus_data_clipped                mus_clipping
  alias set_mus_data_clipped            set_mus_clipping
  alias dac_is_running                  playing
  # backwards compatibility for snd 8
  alias make_ppolar                     make_two_pole
  alias make_zpolar                     make_two_zero
  alias make_average                    make_moving_average
  alias average                         moving_average
  alias average?                        moving_average?
  # *windowed_maxamp -> dsp.rb
  def samples2sound_data(beg = 0,
                         num = false,
                         snd = false,
                         chn = false,
                         obj = false,
                         pos = false,
                         sd_chan = 0)
    len = (num or frames(snd, chn))
    gen = (obj or make_sound_data(1, len))
    vct2sound_data(channel2vct(beg, len, snd, chn, pos), gen, sd_chan)
  end

  def open_sound_file(*args)
    mus_sound_open_output(get_args(args, :file, (little_endian ? "test.wav" : "test.snd")),
                          get_args(args, :srate, 44100),
                          get_args(args, :channels, 1),
                          (little_endian ? Mus_lfloat : Mus_bfloat),
                          get_args(args, :header_type, (little_endian ? Mus_riff : Mus_next)),
                          get_args(args, :comment, ""))
  end

  alias close_sound_file mus_sound_close_output

  def vct2sound_file(fd, v, samps)
    mus_sound_write(fd, 0, samps - 1, 1, vct2sound_data(v))
  end

  # snd10.scm
  def make_sum_of_sines(*args)
    sines, frequency, initial_phase = nil
    optkey(args, binding,
           [:sines, 1],
           [:frequency, 0.0],
           [:initial_phase, 0.0])
    gen = make_nsin(:frequency, frequency, :n, sines)
    gen.phase = initial_phase
    gen
  end
  alias sum_of_sines  nsin
  alias sum_of_sines? nsin?

  def make_sum_of_cosines(*args)
    cosines, frequency, initial_phase = nil
    optkey(args, binding,
           [:cosines, 1],
           [:frequency, 0.0],
           [:initial_phase, 0.0])
    gen = make_ncos(:frequency, frequency, :n, cosines)
    gen.phase = initial_phase
    gen
  end
  alias sum_of_cosines  ncos
  alias sum_of_cosines? ncos?

  def make_sine_summation(*args)
    frequency, initial_phase, n, a, ratio = nil
    optkey(args, binding,
           [:frequency, 0.0],
           [:initial_phase, 0.0],
           [:n, 1],
           [:a, 0.5],
           [:ratio, 1.0])
    gen = make_nrxysin(:frequency, frequency, :ratio, ratio, :n, n, :r, a)
    gen.phase = initial_phase
    gen
  end
  alias sine_summation  nrxysin
  alias sine_summation? nrxysin?

  # snd13.scm
  def clm_print(fmt, *args)
    snd_print(format(fmt, *args))
  end unless defined? clm_print
end

# enum("foo", :bar, "FOO_BAR")
# produces three constants
# Foo     == 0
# Bar     == 1
# FOO_BAR == 2
def enum(*names)
  cap_alpha = ?A.kind_of?(String) ? ?A.sum : ?A
  lit_alpha = ?a.kind_of?(String) ? ?a.sum : ?a
  letter_diff = cap_alpha - lit_alpha
  names.flatten.map_with_index do |name, i|
    const_name = name.to_s
    if const_name[0].between?(?a, ?z)
      const_name[0] += letter_diff
    end
    Object.const_set(const_name, i)
    const_name
  end
end

class Object
  def null?
    self.nil? or
      (self.respond_to?(:zero?) and self.zero?) or
      (self.respond_to?(:empty?) and self.empty?) or
      (self.respond_to?(:length) and self.length.zero?)
  end

  def function?(obj)
    func?(obj) and Snd.catch(:all, false) do self.method(obj) end.first
  rescue
    false
  end
  
  # Float(nil) ==> 0.0 like Integer(nil) ==> 0
  alias old_Float Float
  def new_Float(numb)
    if numb.kind_of?(NilClass)
      0.0
    else
      old_Float(numb)
    end
  end
  alias Float new_Float

  def snd_func(name, *rest, &body)
    send(name.to_s, *rest, &body)
  end
  
  def set_snd_func(name, val, *rest, &body)
    send(format("set_%s", name.to_s), val, *rest, &body)
  end

# snd_apropos(str_or_sym)
# if `str_or_sym' is a symbol, returns snd_help result,
# if `str_or_sym' is a string or regexp it looks in
#   self.public_methods,
#   self.protected_methods,
#   self.private_methods,
#   Object.constants, and
#   Kernel.global_variables and returns an array of strings or nil.
# 
#     [].snd_apropos(/^apply/)     ==> ["apply", "apply_controls"]
# vct(0).snd_apropos("subseq")     ==> ["subseq", "vct_subseq"]
#        snd_apropos(/^mus_sound/) ==> ["mus_sound_...", ...]
  def snd_apropos(str_or_sym)
    case str_or_sym
    when Symbol
      snd_help(str_or_sym)
    when String, Regexp
      res = []
      [self.public_methods,
        self.protected_methods,
        self.private_methods,
        Object.constants,
        Kernel.global_variables].each do |m| res += m.grep(/#{str_or_sym}/) end
      res
    else
      nil
    end
  end
end

def NilClass(arg)
  nil
end

alias Fixnum Integer

class NilClass
  def each
    nil
  end

  def apply(func, *rest, &body)
    nil
  end
  
  def empty?
    true
  end

  # Integer(nil) ==> 0
  def zero?
    true
  end
  
  def nonzero?
    false
  end

  def to_vct
    vector2vct([])
  end

  def to_vector
    vector()
  end

  def to_poly
    poly()
  end

  def +(other)
    other
  end

  def -(other)
    other
  end

  def *(other)
    snd_func(other.class.name, nil)
  end
end

# If $DEBUG = true, on older Ruby versions warnings occur about
# missing NilClass#to_str and Symbol#to_str
if $DEBUG and RUBY_VERSION < "1.8.0"
  class Object
    def method_missing(id, *args)
      if id == :to_str
        self.class.class_eval do define_method(id, lambda do | | self.to_s end) end
        id.id2name
      else
        Kernel.raise(NameError,
                     format("[version %s] undefined method `%s'", RUBY_VERSION, id.id2name))
      end
    end
  end
end
  
class String
  def to_sym
    self.intern
  end unless defined? "a".to_sym
end

class Symbol
  def to_sym
    self
  end unless defined? :a.to_sym
end

alias object_id __id__ unless defined? object_id

# Provides descriptions of instances of classes, see nb.rb,
# xm-enved.rb, etc.
#
# m = lambda do |*args| puts args end
# m.info = "my description"
# puts m.info
module Info
  def description=(val)
    @description = val.to_s
  end
  alias info= description=
  
  def description
    if defined?(@description) and string?(@description) and (not @description.empty?)
      @description
    else
      "no description available"
    end
  end
  alias info description
end

unless defined? snd_help
  alias snd_help get_help
end

$array_print_length = 10

def print_length
  $array_print_length
end unless defined? print_length

def set_print_length(val)
  $array_print_length = val
end unless defined? set_print_length

module Enumerable
  def map_with_index
    i = -1
    self.map do |x| yield(x, i += 1) end
  end

  def map_with_index!
    i = -1
    self.map! do |x| yield(x, i += 1) end
  end
  
  def clm_cycle
    unless defined? @clm_cycle_index then @clm_cycle_index = 0 end
    val = self[@clm_cycle_index % self.length]
    @clm_cycle_index += 1
    if @clm_cycle_index == self.length then @clm_cycle_index = 0 end
    val
  end

  def clm_cycle=(val)
    unless defined? @clm_cycle_index then @clm_cycle_index = 0 end
    self[@clm_cycle_index % self.length] = val
    @clm_cycle_index += 1
    if @clm_cycle_index == self.length then @clm_cycle_index = 0 end
    val
  end
  attr_accessor :clm_cycle_index

  # backward compatibility methods
  def each_index
    self.each_with_index do |val, i| yield(i) end
  end unless vct(0).respond_to?(:each_index)
  
  # Enumerable#zip, new in ruby core since 19-Nov-2002.
  # a = [4, 5, 6]
  # b = [7, 8, 9]
  # [1, 2, 3].zip(a, b) --> [[1, 4, 7], [2, 5, 8], [3, 6, 9]]
  # [1, 2].zip(a, b)    --> [[1, 4, 7], [2, 5, 8]]
  # a.zip([1, 2],[8])   --> [[4, 1, 8], [5, 2, nil], [6, nil, nil]]
  def clm_zip(*objs)
    args = objs.map do |obj| obj.to_a end
    res = self.to_a
    res.each_with_index do |val, i|
      ary = [val]
      args.each do |obj| ary.push(obj[i]) end
      if block_given?
        yield(*ary)
      else
        res[i] = ary
      end
    end
    res
  end
  alias zip clm_zip unless [].respond_to?(:zip)
end

# Older Ruby versions lack Array.new(10) do |i| ... end
# make_array
# make_array(10)
# make_array(10, 1.0)
# make_array(10) do |i| ... end
def make_array(len = 0, init = nil)
  if len >= 0
    if block_given?
      Array.new(len, init).map_with_index do |x, i| yield(i) end
    else
      Array.new(len, init)
    end
  else
    Kernel.raise(TypeError, format("array length < 0 (%s)?", len.inspect))
  end
end

class Array
  def insert(pos, *args)
    unless args.empty?
      if pos < 0
        pos = self.length - (pos.abs - 1)
      end
      tmp = self.dup
      self[pos, args.length] = args
      self[pos + args.length..-1] = tmp[pos..-1]
    end
    self
  end unless defined? [].insert

  # [0.0, 0.0, 0.5, 0.2, 1.0, 1.0].to_pairs --> [[0.0, 0.0], [0.5, 0.2], [1.0, 1.0]]
  def to_pairs
    ary = []
    self.step(2) do |a, b| ary.push([a, b]) end
    ary
  end

  # [0.0, 0.0, 0.5, 0.2, 1.0, 1.0].each_pair do |x, y| print x, " ", y, "\n" end
  # --> 0.0 0.0
  #     0.5 0.2
  #     1.0 1.0
  def each_pair
    ary = []
    self.step(2) do |a, b| ary.push(yield(a, b)) end
    ary
  end

  # prints flat float array more prettily
  def to_string(len = print_length)
    ary = self.flatten
    str = "["
    ary.each_with_index do |val, i|
      if i < len
        str = str + "%1.3f, " % val.to_f
      else
        break
      end
    end
    if ary.length > len
      str += "..."
    else
      str.chop!.chop!
    end
    str += "]"
  end

  alias old_to_s to_s
  alias to_s inspect

  def first=(val)
    self[0] = val
  end

  def last=(val)
    self[-1] = val
  end

  # ary.pick       ==> random value
  # ary.pick(3)    ==> [x, y, z]
  # ary.pick(true) ==> whole ary randomized
  def array_pick(n = 1)
    n = self.length if n == true
    if n == 1
      self[kernel_rand(self.length)]
    else
      (0...n).map do |i| self[kernel_rand(self.length)] end
    end
  end
  alias pick array_pick

  def array_rand
    tmp = self.dup
    tmp.each_index do |i|
      r = kernel_rand(tmp.length)
      tmp[r], tmp[i] = tmp[i], tmp[r]
    end
    tmp
  end
  alias rand array_rand
  
  def array_rand!
    self.each_index do |i|
      r = kernel_rand(self.length)
      self[r], self[i] = self[i], self[r]
    end
    self
  end
  alias rand! array_rand!
  
  def add(other)
    new_ary = self.dup
    [self.length, other.length].min.times do |i| new_ary[i] += other[i] end
    new_ary
  end

  def add!(other)
    [self.length, other.length].min.times do |i| self[i] += other[i] end
    self
  end

  def subtract(other)
    new_ary = self.dup
    [self.length, other.length].min.times do |i| new_ary[i] -= other[i] end
    new_ary
  end

  def subtract!(other)
    [self.length, other.length].min.times do |i| self[i] -= other[i] end
    self
  end
  
  def multiply(other)
    new_ary = self.dup
    [self.length, other.length].min.times do |i| new_ary[i] *= other[i] end
    new_ary
  end

  def multiply!(other)
    [self.length, other.length].min.times do |i| self[i] *= other[i] end
    self
  end

  def offset(scl)
    scl = Float(scl)
    self.class.new(self.length) do |i| self[i] + scl end
  end

  def offset!(scl)
    scl = Float(scl)
    self.map! do |val| val += scl end
  end

  def scale(scl)
    scl = Float(scl)
    self.class.new(self.length) do |i| self[i] * scl end
  end

  def scale!(scl)
    scl = Float(scl)
    self.map! do |val| val *= scl end
  end

  def to_vector
    Vec.new(self.length) do |i| Float(self[i]) end
  end
  
  def car
    self[0]
  end
  
  def car=(val)
    self[0] = val
  end

  def cadr
    self[1]
  end

  def cadr=(val)
    self[1] = val
  end

  def caddr
    self[2]
  end

  def caddr=(val)
    self[2] = val
  end

  def cadddr
    self[3]
  end
  def cadddr=(val)
    self[3] = val
  end

  def caddddr
    self[4]
  end

  def caddddr=(val)
    self[4] = val
  end

  def cdr
    self[1..-1]
  end

  def step(n = 1)
    0.step(self.length - n, n) do |i| yield(*self[i, n]) end
    self
  end

  add_help(:apply,
           "Array#apply([:func,] *rest, &body)
applies function or procedure with possible rest args \
to each element of Array or subclasses of Array.
                                  [0, 1, 2].apply(\"a: %d\\n\") do |fmt, a| printf(fmt, a) end
                                  [0, 1, 2].apply(:printf, \"a: %d\\n\")
both produce
a: 0
a: 1
a: 2
                               [1, 2, 3, 4].apply(:+)      # ==> 10
                 %w(snd sndplay with_sound).apply(:length) # ==> [3, 7, 10] 
          [[1, 2, 3, 4], [1, 2, 3], [1, 2]].apply(:max)    # ==> [4, 3, 2]
[vct(0.1, 0.2, 0.3), vct(-0.1, -0.2, -0.3)].apply(:peak)   # ==> [0.3, 0.3]
                                     sounds.apply(:map) do |s| puts s end
                                     sounds.apply(:close_sound)")
  def apply(func, *rest, &body)
    if block_given? and (not symbol?(func))
      rest.unshift(func)
      self.map do |item| body.call(*rest + [item]) end
    else
      case func
      when Proc, Method
        self.map do |item| func.call(*rest + [item]) end
      when Symbol, String
        meths = self.methods
        if body and (meths.member?(func.to_s) or meths.member?(func.to_sym))
          # map, each, ...
          self.send(func, *rest, &body)
        else
          receiver = self.compact.first
          meths = receiver.methods
          if receiver and (meths.member?(func.to_s) or meths.member?(func.to_sym))
            # methods
            case func.to_sym
            when :+, :-, :*
              res = receiver
              self[1..-1].compact.map do |item| res = res.send(func, *rest + [item]) end
              res
            else
              len = rest.length + ((array?(receiver) and receiver.length) or 1)
              if receiver.method(func).arity.abs == len
                # remove_file (String(WS) in ws.rb)
                self.map do |item| send(func, *rest + [item]) end
              else
                # length, max, min, ...
                self.map do |item| item.send(func, *rest) end
              end
            end
          else
            # functions
            self.map do |item| send(func, *rest + [item]) end
          end
        end
      end
    end
  end

  # original operands +, -, and * can now handle nil and numberic (offset, multiply)
  # 
  # [].+(ary)      concatenate arrays
  # [].+(number)   [].add(number)
  unless defined? [].ary_plus
    alias old_ary_plus +
    def ary_plus(other)
      case other
      when Numeric
        self.offset(other)
      when NilClass
        self
      else
        self.old_ary_plus(other)
      end
    end
    alias + ary_plus
  end

  # [].-(ary)     intersection
  # [1, 2, 3, 4] - [2, 3] ==> [1, 4]
  # [] - number   [].offset
  unless defined? [].ary_minus
    alias old_ary_minus -
    def ary_minus(other)
      case other
      when Numeric
        self.offset(-other)
      when NilClass
        self
      else
        self.old_ary_minus(other)
      end
    end
    alias - ary_minus
  end
  
  # [].*(n)   repetition or [].join(n)
  # [5] * 3              ==> [5, 5, 5]
  # ["foo", "bar"] * "-" ==> "foo-bar"
  unless defined? [].ary_times
    alias old_ary_times *
    def ary_times(other)
      case other
      when NilClass
        nil.to_a
      else
        self.old_ary_times(other)
      end
    end
    alias * ary_times
  end
end

# name Vector is in use (lib/ruby/1.9/matrix.rb)
class Vec < Array
  def self.[](*ary)
    self.new(ary.length) do |i| ary[i] end
  end
  
  def initialize(len, init = 0.0, &body)
    @name = "vector"
    if len >= 0
      if block_given?
        super(len, &body)
      else
        super(len, init)
      end
    else
      Kernel.raise(TypeError, format("array length < 0 (%s)?", len.inspect))
    end
    if test = self.detect do |x| (not number?(x)) end
      Kernel.raise(TypeError, format("only numeric elements (%s)?", test.inspect))
    end
  end

  def inspect
    str = "%s(" % @name
    self.each do |val| str = str + "%s, " % val end
    if self.length > 0 then str.chop!.chop! end
    str += ")"
    str
  end

  def to_s
    if self.length > 0
      vals = ":"
      self.map do |val| vals = vals + " %s" % val end
    else
      vals = ""
    end
    format("#<%s[%d]%s>", self.class, self.length, vals)
  end
  
  def +(other)
    case other
    when Numeric
      self.offset(other).to_vector
    when Array, Vec, Vct
      self.add(other.to_vector)
    when NilClass
      self
    end
  end
  
  def -(other)
    case other
    when Numeric
      self.offset(-other).to_vector
    when Array, Vec, Vct
      self.subtract(other.to_vector)
    when NilClass
      self
    end
  end

  def *(other)
    case other
    when Numeric
      self.scale(other).to_vector
    when Array, Vec, Vct
      self.multiply(other.to_vector)
    when NilClass
      nil.to_vector
    end
  end
end

def Vec(obj)
  if obj.nil? then obj = [] end
  assert_type(obj.respond_to?(:to_vector), obj, 0,
              "an object containing method 'to_vector' (Vct, String, Array and subclasses)")
  obj.to_vector
end

def make_vector(len, init = 0.0, &body)
  Vec.new(len, init, &body)
end

def vector?(obj)
  obj.kind_of?(Vec)
end

def vector(*args)
  args.to_vector
end

class String
  def to_vector
    if self.scan(/^vector\([-+,.)\d\s]+/).null?
      nil
    else
      eval(self)
    end
  end
  
  def to_vct
    if self.scan(/^vct\([-+,.)\d\s]+/).null?
      nil
    else
      eval(self)
    end
  end
end

def Vct(obj)
  if obj.nil? then obj = [] end
  assert_type(obj.respond_to?(:to_vct), obj, 0,
              "an object containing method 'to_vct' (Vct, String, Array and subclasses)")
  obj.to_vct
end

def make_vct!(len, init = 0.0, &body)
  if block_given?
    Vct.new(len, &body)
  else
    Vct.new(len, init)
  end
end

class Vct
  def self.[](*ary)
    self.new(ary.length) do |i| ary[i] end
  end

  def name
    self.class.to_s.downcase
  end
  
  def to_sound_data(sd = nil, chn = 0)
    if sound_data?(sd)
      vct2sound_data(self, sd, chn)
    else
      vct2sound_data(self)
    end
  end

  def to_vct(chn = 0)           # CHN for compatibility with sound-data
    self
  end

  def to_vector
    Vec.new(self.length) do |i| self[i] end
  end

  def apply(*rest, &body)
    self.to_a.apply(*rest, &body)
  end
  
  def +(other)
    case other
    when Numeric
      self.offset(other)
    when Array, Vec, Vct
      self.add(other.to_vct)
    when NilClass
      self
    end
  end

  def -(other)
    case other
    when Numeric
      self.offset(-other)
    when Array, Vec, Vct
      self.subtract(other.to_vct)
    when NilClass
      self
    end
  end

  def *(other)
    case other
    when Numeric
      self.scale(other)
    when Array, Vec, Vct
      self.multiply(other.to_vct)
    when NilClass
      nil.to_vct
    end
  end

  def step(n = 1, &body)
    self.to_a.step(n, &body)
  end

  # v = vct(0, 1, 2, 3, 4)
  # v[2..4]  ==> vct(2.000, 3.000, 4.000)
  # v[2...4] ==> vct(2.000, 3.000)
  # v[3, 4]  ==> vct(3.000, 4.000)
  # v[-1]    ==> 4.0
  def vct_ref_extend(idx, size = nil)
    case idx
    when Fixnum
      if idx < 0 then idx += self.length end
      if idx < 0 then Snd.raise(:out_of_range, "index < 0", idx) end
      if integer?(size)
        size += idx - 1
        if size >= self.length then size = self.length - 1 end
        if size.between?(0, self.length - 1) and size >= idx
          self.subseq(idx, size)
        else
          nil.to_vct # i.e. false
        end
      else
        vct_ref(self, idx)
      end
    when Range
      beg = idx.first
      len = idx.last
      if beg < 0 then beg += self.length end
      if len < 0 then len += self.length end
      if len >= self.length then len = self.length - 1 end
      # exclude_end?: (1..2)  ==> false
      #               (1...2) ==> true
      if idx.exclude_end? then len -= 1 end
      if beg.between?(0, self.length - 1) and len >= beg
        self.subseq(beg, len)
      else
        nil.to_vct # i.e. false
      end
    end
  end
  # alias [] vct_ref_extend

  # This is required since Ruby 1.9.
  alias zip clm_zip if [].respond_to?(:zip)
end

class Fixnum
  # no reloading (load "clm.rb")
  unless defined? 0.new_int_plus
    alias int_plus +
    def new_int_plus(other)
      case other
      when Vct, Array, Vec
        other.offset(Float(self))
      when NilClass
        self
      else
        self.int_plus(other)
      end
    end
    alias + new_int_plus
  end

  unless defined? 0.new_int_times
    alias int_times *
    def new_int_times(other)
      case other
      when Vct, Array, Vec
        other.scale(self)
      when NilClass
        0
      else
        self.int_times(other)
      end
    end
    alias * new_int_times
  end
end

class Float
  # no reloading (load "clm.rb")
  unless defined? 0.0.new_float_plus
    alias float_plus +
    def new_float_plus(other)
      case other
      when Vct, Array, Vec
        other.offset(self)
      when NilClass
        self
      else
        self.float_plus(other)
      end
    end
    alias + new_float_plus
  end

  unless defined? 0.0.new_float_times
    alias float_times *
    def new_float_times(other)
      case other
      when Vct, Array, Vec
        other.scale(self)
      when NilClass
        0.0
      else
        self.float_times(other)
      end
    end
    alias * new_float_times
  end
  
  unless defined? 0.0.imag
    def imag
      0.0
    end
    alias image imag
  end
end

def SoundData(ary)
  assert_type((array?(ary) and vct?(ary.first)), ary, 0, "an array of vcts")
  sd = SoundData.new(ary.length, ary.first.length)
  ary.each_with_index do |v, chn| vct2sound_data(v, sd, chn) end
  sd
end

def sound_data2string(sd)
  sd.to_a.to_s
end

def sound_data2vector(sd)
  make_array(sd.chans) do |chn|
    sound_data2vct(sd, chn).to_a
  end
end

class SoundData
  def name
    "sound-data"
  end
  
  def to_vct(chn = 0)
    sound_data2vct(self, chn)
  end
  
  # returns an array of sd.chans vcts
  def to_a
    sound_data2vector(self)
  end

  alias sd_length length
  def length
    self.size / self.chans
  end

  def fill!(val)
    sound_data_fill!(self, val)
  end
  
  alias sd_each each
  def each(chn = nil)
    if chn
      self.length.times do |i| yield(self[chn, i]) end
    else
      self.sd_each do |val| yield(val) end
    end
  end

  def each_with_index(chn = nil)
    if chn
      self.length.times do |i| yield(self[chn, i], i) end
    else
      self.length.times do |i|
        self.chans.times do |j| yield(self[j, i], i) end
      end
    end
  end
  
  def map(chn = nil)
    sd = nil
    if chn
      sd = self.dup
      self.each_with_index(chn) do |val, i| sd[chn, i] = yield(val) end
    else
      sd = SoundData.new(self.chans, self.length)
      self.chans.times do |j|
        self.each_with_index(j) do |val, i| sd[j, i] = yield(val) end
      end
    end
    sd
  end

  def map!(chn = nil)
    if chn
      self.each_with_index(chn) do |val, i| self[chn, i] = yield(val) end
    else
      self.chans.times do |j|
        self.each_with_index(j) do |val, i| self[j, i] = yield(val) end
      end
    end
    self
  end
end

def mus_a0(gen)
  mus_xcoeff(gen, 0)
end

def set_mus_a0(gen, val)
  set_mus_xcoeff(gen, 0, val)
end

def mus_a1(gen)
  mus_xcoeff(gen, 1)
end

def set_mus_a1(gen, val)
  set_mus_xcoeff(gen, 1, val)
end

def mus_a2(gen)
  mus_xcoeff(gen, 2)
end

def set_mus_a2(gen, val)
  set_mus_xcoeff(gen, 2, val)
end

def mus_b1(gen)
  mus_ycoeff(gen, 1)
end

def set_mus_b1(gen, val)
  set_mus_ycoeff(gen, 1, val)
end

def mus_b2(gen)
  mus_ycoeff(gen, 2)
end

def set_mus_b2(gen, val)
  set_mus_ycoeff(gen, 2, val)
end

class Mus
  # clm_gen.call(a1, a2) requires 2 arguments but clm_gen.run([a1, [a2]])
  # 0, 1 or 2.
  # 
  # clm_gen.run([arg1, [arg2]])
  def run(arg1 = 0.0, arg2 = 0.0)
    mus_run(self, arg1, arg2)
  end

  def apply(*rest)
    mus_apply(self, *rest)
  end

  alias mus_inspect inspect
  def inspect
    "#<" + mus_describe(self) + ">"
  end

  def close
    mus_close(self)
  end
  
  # gen.xcoeff = 0, 0.4
  # set_mus_xcoeff(gen, index, val)
  def xcoeff=(args)
    set_mus_xcoeff(self, *args.flatten[0, 2])
  end
  
  # gen.ycoeff = 0, 0.4
  # set_mus_ycoeff(gen, index, val)
  def ycoeff=(args)
    set_mus_ycoeff(self, *args.flatten[0, 2])
  end
  
  def a0
    mus_xcoeff(self, 0)
  end

  def a0=(val)
    set_mus_xcoeff(self, 0, val)
  end
  
  def a1
    mus_xcoeff(self, 1)
  end

  def a1=(val)
    set_mus_xcoeff(self, 1, val)
  end
  
  def a2
    mus_xcoeff(self, 2)
  end

  def a2=(val)
    set_mus_xcoeff(self, 2, val)
  end
  
  def b1
    mus_ycoeff(self, 1)
  end

  def b1=(val)
    set_mus_ycoeff(self, 1, val)
  end
  
  def b2
    mus_ycoeff(self, 2)
  end

  def b2=(val)
    set_mus_ycoeff(self, 2, val)
  end
end

# base class for generators written in Ruby
class Musgen
  def initialize
    @frequency = $clm_default_frequency
    @phase = 0.0
    @scaler = 1.0
    @length = 0
    @data = nil
    @increment = 0
    @interp_type = -1
    @file_name = ""
  end
  attr_accessor :frequency
  attr_accessor :phase
  attr_accessor :scaler
  attr_accessor :increment
  attr_reader   :length
  attr_reader   :data
  attr_reader   :interp_type
  attr_reader   :file_name
  
  def inspect
    format("%s.new()", self.class)
  end

  def to_s
    format("#<%s>", self.class)
  end

  def run(val1 = 0.0, val2 = 0.0)
    self.run_func(val1, val2)
  end
  alias call run

  def apply(*rest)
    self.run_func(*rest)
  end

  def eql?(other)
    self == other
  end

  def reset
    @frequency = $clm_default_frequency
    @phase = 0.0
    @scaler = 1.0
    @increment = 0
    self
  end
end

class Numeric
  def positive?
    self > 0
  end

  def negative?
    self < 0
  end
end

class Integer
  def even?
    self.modulo(2) == 0
  end unless defined? 1.even?

  def odd?
    self.modulo(2) != 0
  end unless defined? 1.odd?
  
  def prime?
    (self == 2) or
    (self.odd? and 3.step(sqrt(self), 2) do |i| return false if self.modulo(i) == 0 end)
  end
end

class Float
  # step accepts floats as arguments (still implemented in newer versions)
  def step(upto, step)
    counter = self
    while counter < upto
      yield(counter)
      counter += step
    end
    counter
  end unless 1.1.respond_to?(:step)
end

class Range
  def step(n = 1, &body)
    self.to_a.step(n, &body)
  end unless defined? Range.new(0, 1).step
end

def as_one_edit_rb(*origin, &body)
  # ruby compatibility:
  # ruby pre 1.9: lambda do end.arity != lambda do | | end.arity
  # ruby     1.9: they are even (0)
  as_one_edit(lambda do | | body.call end, origin.empty? ? "" : format(*origin))
end

def map_channel_rb(beg = 0, dur = false,
                   snd = false, chn = false, edpos = false, edname = false, &func)
  map_channel(func, beg, dur, snd, chn, edpos, edname) 
end

add_help(:map_chan_rb,
         "map_chan(func,[start=0,[end=false,[edname=false,[snd=false,[chn=false,[edpos=false]]]]]])\
  map_chan applies func to samples in the specified channel.\
  It is the old (\"irregular\") version of map_channel.")
def map_chan_rb(beg = 0, dur = false, ednam = false, snd = false, chn = false, edpos = false, &func)
  map_chan(func, beg, dur, ednam, snd, chn, edpos) 
end

class Proc
  include Info
  alias run call

  add_help(:to_method,
           "Proc#to_method(name, [klass=Object])  \
converts a Proc to a Method 'name' in the given class, default Object.  \
'name' can be a string or a symbol.

m = lambda do |*args| p args end
m.to_method(:func)
func(1, 2, 3) ==> [1, 2, 3]

lambda do |x| p x end.to_method(:foo);  foo(\"text1\") ==> \"text1\"
lambda do |x| p x end.to_method(\"bar\"); bar(\"text2\") ==> \"text2\"")
  def to_method(name, klass = Object)
    name = case name
           when String
             name.intern
           when Symbol
             name
           end
    body = self
    klass.class_eval do define_method(name, body) end
  end

  # Important:
  # The following works only with newer ruby versions (I assume >=
  # 1.8.x).  Proc#inspect must return #<Proc:0x80c96a0@xxx:x> to
  # locate the source file of the procedure, not only #<Proc:0x80c96a0>!

  # Functions to_str and to_body try to search the procedure source
  # code in a file determined by to_s.  It is only a simple scanner
  # which doesn't look for the whole Ruby syntax. ;-)
  # 
  # It doesn't work if no source file exists, i.e, if the code is
  # eval'ed by the Snd listener (or in Emacs).  You must load the file
  # instead.
  # 
  # with_sound(:notehook, lambda do |name| snd_print(name) if name =~ /viol/ end) do
  #   fm_violin(0, 1, 440, 0.3)
  # end
  # 
  # $clm_notehook = lambda do |name| clm_print(name) if name =~ /viol/ end
  # 
  # with_sound do
  #   fm_violin(0, 1, 440, 0.3)
  # end
  # 
  # with_sound(:save_body, true) do
  #  ...
  # end

  # returns something like 'lambda do ... end'
  def to_str
    if body = self.source
      return body
    end
    file, line = self.to_s.sub(/>/, "").split(/@/).last.split(/:/)
    if file[0] == ?( and file[-1] == ?)
      if $VERBOSE
        warning("%s#%s: no file found for procedure %s", self.class, get_func_name, self.inspect)
      end
      body = ""
    elsif (not File.exists?(file))
      if $VERBOSE
        warning("%s#%s: Sorry, you need a higher ruby version to use Proc#to_str.
It works only with newer ruby versions (I assume >= 1.8.x).
Proc#inspect must return #<Proc:0x01234567@xxx:x> not only %s!",
             self.class, get_func_name, self.inspect)
      end
      body = ""
    else
      lineno = line.to_i
      body = ""
      blck = i = 0
      first_line = true
      File.foreach(file) do |ln|
        i += 1
        next if i < lineno
        body << ln
        if first_line
          if (ln.scan(/\s*do\b|\{/).length - ln.scan(/\s*end\b|\}/).length).zero? and
              (ln.scan(/\(/).length - ln.scan(/\)/).length).zero?
            break
          else
            first_line = false
            blck = 1
            next
          end
        end
        next if /\s*\S+\s*(if|unless|while|until)+/ =~ ln
        break if (blck += Snd_eval.count_level(ln)).zero?
        break if blck.negative?
      end
    end
    unless self.source then self.source = body end
    body
  end

  # returns the inner body without 'lambda do end'
  def to_body
    if (body = self.to_str).null?
      ""
    elsif body.split(/\n/).length == 1
      body.chomp.sub(/^(?:\s*\w+(?:\(.*\))??\s*(?:do\s+|\{\s*))(.*)\s*(?:end|\})$/, '\1').strip
    else
      body.split(/\n/)[1..-2].join("\n")
    end
  end

  # property set in g_edit_list_to_function (snd-edits.c)
  def source
    property(self.object_id, :proc_source)
  end

  def source=(val)
    set_property(self.object_id, :proc_source, val)
  end
end

def make_proc2method(name, prc)
  prc.to_method(name)
end

# produces two new functions: NAME and SET_NAME
# val = 10
# make_proc_with_setter(:foo, lambda { puts val }, lambda { |a| val = a })
# foo ==> 10
# set_foo(12)
# foo ==> 12
def make_proc_with_setter(name, getter, setter)
  make_proc2method(name, getter)
  make_proc2method(format("set_%s", name).intern, setter)
end

# prc = make_proc_with_source(%(lambda do |a, b, c| puts a, b, c end))
# prc.call(1, 2, 3)
# prc.source ==> "lambda do |a, b, c| puts a, b, c end"
# 
# With the second argument BIND one can use local variables known in
# the current (or other) environment in the proc body:
# 
# os = make_oscil(:frequency, 330)
# prc = make_proc_with_source(%(lambda do | | 10.times do |i| p os.run end end), binding)
# puts prc.source   ==> lambda do | | 10.times do |i| p os.run end end
# prc.call          ==> ..., 0.748837699712728
# puts
# prc.call          ==> ..., 0.97679449812022
def make_proc_with_source(string, bind = binding)
  if proc?(prc = (res = Snd.catch(:all) do eval(string, bind) end).first)
    prc.source = string
    prc
  else
    Snd.raise(:runtime_error, res, prc, string)
  end
end

make_proc_with_setter(:proc_source,
                      lambda do |prc| prc.source end,
                      lambda do |prc, val| prc.source = val end)

# Multi-line input to the Snd listener and Emacs/inf-snd.el.
# A simple parser collects multi-line input, e.g.
# 
# with_sound do
#   fm_violin(0.0, 0.1, 330, 0.1)
#   fm_violin(0.1, 0.1, 660, 0.1)
# end
# 
# and evals it.
#
# ~/.snd
# set_listener_prompt("snd> ")   # optional
# start_listener_eval            # installs read-hook for snd-listener input
# start_emacs_eval               # installs emacs-eval-hook

make_hook("$emacs_eval_hook", 1, "\
emacs_eval_hook(line):  called each time inf-snd.el sends a line to the Snd process.  \
The hook functions may do their best to deal with multi-line input; \
they can collect multi-line input and eval it by itself.  \
One example is install_eval_hooks(file, retval, input, hook, &reset_cursor) in clm.rb.")

# inf-snd.el calls this function each time a line was sent to the
# emacs buffer.
def run_emacs_eval_hook(line)
  if $emacs_eval_hook.empty?
    # without emacs-eval-hook only single line eval
    file = "(emacs-eval-hook)"
    set_snd_input(:emacs)
    begin
      Snd.display(eval(line, TOPLEVEL_BINDING, file, 1).inspect)
    rescue Interrupt, ScriptError, NameError, StandardError
      Snd.display(verbose_message_string(true, "# ", file))
    end
    set_snd_input(:snd)
    nil
  else
    $emacs_eval_hook.call(line)
  end
end

class Snd_eval
  class << Snd_eval
    Open_token = %w(class module def do { while until if unless case begin for)
    Close_token = %w(end })

    def count_level(line)
      eval_level = 0
      # skip strings and symbols which may contain reserved words
      line.gsub(/(:\w+|".+")/, "").split(/\b/).each do |s|
        case s
        when *Open_token
          eval_level += 1
        when *Close_token
          eval_level -= 1
        end
      end
      eval_level
    end
  end
end

class Snd_prompt
  # level number inserted into original prompt
  # ">"     --> "(0)>"
  # "snd> " --> "snd(0)> "
  def initialize(level)
    @listener_prompt = listener_prompt
    @base_prompt = listener_prompt.split(/(\(\d+\))?(>)?\s*$/).car.to_s
    @rest_prompt = listener_prompt.scan(/>\s*$/).car.to_s
    update(level)
  end

  def inspect
    format("#<%s %s(0)%s>", self.class, @base_prompt, @rest_prompt)
  end

  def update(level)
    set_listener_prompt(format("%s(%d)%s", @base_prompt, level, @rest_prompt))
  end

  def reset
    set_listener_prompt(@listener_prompt)
  end
end

def install_eval_hooks(file, retval, input, hook, &reset_cursor)
  eval_level = 0
  eval_line = ""
  prompt = Snd_prompt.new(eval_level)
  reset_cursor.nil? or reset_cursor.call
  $exit_hook.add_hook!(file) do | | prompt.reset end
  hook.add_hook!(file) do |line|
    eval_line = eval_line + line + "\n"
    eval_level += Snd_eval.count_level(line)
    if eval_level.negative?
      eval_level = 0
      eval_line = ""
    end
    if eval_level.zero?
      set_snd_input(input)
      begin
        Snd.display(eval(eval_line, TOPLEVEL_BINDING, file, 1).inspect)
      rescue Interrupt, ScriptError, NameError, StandardError 
        Snd.display(verbose_message_string(true, "# ", file))
      ensure
        eval_line = ""
      end
    end
    prompt.update(eval_level)
    reset_cursor.nil? or reset_cursor.call
    retval
  end
end

# installs the emacs-eval-hook
def start_emacs_eval(name = "(emacs)")
  install_eval_hooks(name, nil, :emacs, $emacs_eval_hook) do
    $stdout.print(listener_prompt)
    $stdout.flush
  end
end

# installs the read-hook
def start_listener_eval(name = "(snd)")
  set_show_listener(true)
  install_eval_hooks(name, true, :snd, $read_hook)
end

def stop_emacs_eval(name = "(emacs)")
  $emacs_eval_hook.remove_hook!(name)
  $exit_hook.run_hook_by_name(name)
  $exit_hook.remove_hook!(name)
end

def stop_listener_eval(name = "(snd)")
  $read_hook.remove_hook!(name)
  $exit_hook.run_hook_by_name(name)
  $exit_hook.remove_hook!(name)
  reset_listener_cursor
  clm_print("\n%s", listener_prompt)
end

# Debugging resp. inspecting local variables

make_proc_with_setter(:debug_properties,
                      lambda do |name| property(name, :debug_property) end,
                      lambda do |name, val| set_property(name, :debug_property, val) end)

make_proc_with_setter(:debug_property,
                      lambda do |key, name|
                        hash?(h = debug_properties(name)) and h[key]
                      end,
                      lambda do |key, val, name|
                        unless hash?(h = debug_properties(name)) and h.store(key, [val] + h[key])
                          unless array?(a = property(:debug, :names)) and a.push(name)
                            set_property(:debug, :names, [name])
                          end
                          set_debug_properties(name, {key => [val]})
                        end
                      end)

make_proc_with_setter(:debug_binding,
                      lambda do |name|
                        debug_property(:binding, name)
                      end,
                      lambda do |bind, *name|
                        set_debug_property(:binding, bind, (name[0] or get_func_name(3)))
                      end)

# Shows all local variables of last call of functions prepared with
# set_debug_binding(binding)
# 
# def function1
#   [...]
#   set_debug_binding(binding)
# end
# def function2
#   [...]
#   set_debug_binding(binding)
# end
# [...]
# function1
# function2
# [...]
# 
# display_all_variables
def display_all_variables(name = nil)
  if name
    [name]
  else
    (property(:debug, :names) or [])
  end.each do |nm|
    debug_binding(nm).each do |bind|
      Snd.message("=== %s ===", nm)
      Snd.message()
      each_variables(bind) do |var, val|
        Snd.message("%s = %s", var, val.inspect)
      end
      Snd.message()
    end
  end
end

# each_variables provides all local variable names and their values in
# the given proc context
# 
# def function
#   [...]
#   each_variables do |k, v|
#     Snd.display("%s = %s", k, v)
#   end
# end
def each_variables(bind = binding, &prc)
  eval("local_variables", bind).each do |name|
    name = name.to_s
    prc.call(name, eval(name, bind))
  end
end

# let(8, :foo, "bar") do |a, b, c|
#   printf("a: %d, b: %s, c: %s\n", a, b, c)
# end
#
# Simulates a save local variable environment and restores old
# variables to their original values.
def let(*args, &prc)
  locals = Hash.new
  bind = prc.binding
  each_variables(bind) do |var, val|
    locals[var] = val
  end
  prc.call(*args)
rescue Interrupt, ScriptError, NameError, StandardError
  Kernel.raise
ensure
  @locals = locals
  locals.each_key do |name|
    eval("#{name} = @locals[#{name.inspect}]", bind)
  end
  remove_instance_variable("@locals")
end

# for irb (rgb.rb)
def make_color(r, g, b)
  [:Pixel, 0]
end unless defined? make_color

def doc(*rest)
  # dummy for old Kernel.doc
end

##
## Utilities
##

if provided? :snd_nogui
  alias close_sound_extend close_sound
else
  def close_sound_extend(snd)
    # 5 == Notebook
    if main_widgets[5]
      idx = Snd.sounds.index(snd)
      if idx.nil? then idx = 0 end
      close_sound(snd)
      snds = sounds() and set_selected_sound(snds[idx < snds.length ? idx : -1])
    else
      close_sound(snd)
    end
  end
end

add_help(:times2samples,
         "times2samples(start, dur) \
START and DUR are in seconds; returns array [beg, end] in samples.")
def times2samples(start, dur)
  beg = seconds2samples(start)
  [beg, beg + seconds2samples(dur)]
end

def random(val)
  if val.zero?
    val
  else
    case val
    when Fixnum
      kernel_rand(val)
    when Float
      val.negative? ? -mus_random(val).abs : mus_random(val).abs
    end
  end
end

def logn(r, b = 10)
  if r <= 0 then Snd.raise(:ruby_error, r, "r must be > 0") end
  if b <= 0 or b == 1 then Snd.raise(:ruby_error, b, "b must be > 0 and != 1") end
  log(r) / log(b)
end

def car(v)
  v[0]
end

def cadr(v)
  v[1]
end

def caddr(v)
  v[2]
end

def cdr(v)
  v[1..-1]
end

def verbose_message_string(stack_p, remark, *args)
  fmt_remark = format("\n%s", remark)
  str = if args.null?
          ""
        elsif args.length == 1
          String(args.car)
        else
          format(*args)
        end
  str = if str.split(/\n/).length > 1
          str.split(/\n/).join(fmt_remark)
        else
          format("%s%s", remark, str)
        end
  if $!
    str += format("[%s] %s (%s)", rb_error_to_mus_tag.inspect, snd_error_to_message, $!.class)
    if stack_p then str += format("\n%s%s", remark, $!.backtrace.join(fmt_remark)) end
  else
    if stack_p and caller(2) then str += format("\n%s%s", remark, caller(2).join(fmt_remark)) end
  end
  str
end

def warning(*args)
  str = "Warning: " << verbose_message_string($VERBOSE, nil, *args)
  if provided? :snd
    snd_warning(str)
    nil
  else
    clm_message(str)
  end
end

def die(*args)
  message(verbose_message_string(true, nil, *args))
  exit(1) unless provided? :snd
end

def error(*args)
  Snd.raise(:runtime_error, verbose_message_string(true, nil, *args))
end

make_proc_with_setter(:snd_input,
                      lambda { property(:snd_input, :snd_listener) },
                      lambda { |val| set_property(:snd_input, :snd_listener, val) })

# like clm_print(fmt, *args)

def clm_message(*args)
  msg = if args.null?
          ""
        elsif args.length == 1
          String(args.car)
        else
          format(*args)
        end
  if provided? :snd
    if provided? :snd_nogui
      clm_print("%s\n", msg)
    else
      clm_print("\n%s", msg)
    end
    nil
  else
    $stdout.print(msg, "\n")
  end
end

# like clm_print(*args), in emacs it prepends msg with a comment sign

def message(*args)
  clm_message(verbose_message_string(false, "# ", *args))
end

# debug(var1, var2) --> #<DEBUG: ClassName: value1, ClassName: value2>

def debug(*args)
  fmt = ""
  args.each do |arg|
    fmt += format("%s: %s", arg.class, arg.inspect)
    fmt += ", "
  end
  message("#<DEBUG: %s>", fmt.chomp(", "))
end

def debug_trace(*args)
  debug(*args)
  clm_message(verbose_message_string(true, "# "))
end

if provided?(:snd) then set_snd_input(:snd) end

class Snd
  class << Snd
    Snd_path = Array.new

    if provided? :snd_motif
      def add_sound_path(path)
        Snd_path.push(path)
        add_directory_to_view_files_list(path)
      end
      
      def open_from_path(fname)
        snd_file = Snd.fullname(fname)
        find_sound(snd_file) or open_sound(snd_file)
      end
      
      def find_from_path(fname)
        find_sound(Snd.fullname(fname))
      end
    else
      def add_sound_path(path)
        Snd_path.push(path)
      end
    end

    def fullname(fname)
      if File.exists?(fname)
        fname
      else
        f = File.basename(fname)
        Snd_path.each do |path|
          if File.exists?(path + "/" + f)
            return path + "/" + f
          end
        end
        Snd.raise(:no_such_file, fname)
      end
    end
    
    def load_path
      Snd_path
    end

    def message(*args)
      clm_message(verbose_message_string(false, "# ", *args))
    end

    def display(*args)
      msg = if args.null?
              ""
            elsif args.length == 1
              String(args.car)
            else
              format(*args)
            end
      if snd_input == :snd
        if provided? :snd_nogui
          clm_print("%s\n", msg)
        else
          clm_print("\n%s", msg)
        end
        nil
      else
        $stdout.print(msg, "\n")
      end
    end
    
    def warning(*args)
      if provided? :snd
        snd_warning(verbose_message_string($VERBOSE, nil, *args))
        nil
      else
        args[0] = "Warning: " + String(args[0])
        Snd.display(verbose_message_string($VERBOSE, "# ", *args))
      end
    end

    def die(*args)
      Snd.display(verbose_message_string(true, nil, *args))
      exit(1) unless provided? :snd
    end

    def error(*args)
      Snd.raise(:runtime_error, verbose_message_string(true, nil, *args))
    end

    def debug(*args)
      if args.null?
        Snd.message("#<DEBUG>")
      elsif args.length == 1
        Snd.message("#<DEBUG: %s>", String(args.car))
      else
        Snd.message("#<DEBUG: %s>", format(*args))
      end
    end

    def debug_trace(*args)
      Snd.debug(*args)
      Snd.display(verbose_message_string(true, "# "))
    end

    def sounds
      (Kernel.sounds or []).reverse
    end

    def regions
      (Kernel.regions or []).reverse
    end
    
    def marks(snd = false, chn = false)
      (Kernel.marks(snd, chn) or [])
    end

    def snd(sn = false)
      sn or selected_sound or Snd.sounds.car
    end

    def chn(ch = false)
      ch or selected_channel or 0
    end

    def catch(tag = :all, retval = :undefined)
      old_debug = $DEBUG
      $DEBUG = false
      val = Kernel.catch(tag) do yield end
      # catch/throw part
      # [:snd_throw, tag, get_func_name(2), *rest]
      if array?(val) and val.car == :snd_throw
        if retval != :undefined
          if proc?(retval)
            retval.call(val.cdr)
          else
            [retval]
          end
        else
          val.cdr
        end
      else
        [val]
      end
      # ruby1.9/ChangeLog
      # Thu Feb  2 16:01:24 2006  Yukihiro Matsumoto  <matz@ruby-lang.org>
      # * error.c (Init_Exception): change NameError to direct subclass of
      #   Exception so that default rescue do not handle it silently.
    rescue Interrupt, ScriptError, NameError, StandardError
      mus_tag = rb_error_to_mus_tag
      # raise part
      if (tag == mus_tag) or (tag == :all)
        if retval != :undefined
          if proc?(retval)
            retval.call(mus_tag, snd_error_to_message)
          else
            [retval]
          end
        else
          [mus_tag, snd_error_to_message]
        end
      else
        Kernel.raise
      end
    ensure
      $DEBUG = old_debug
    end

    def throw(tag, *rest)
      Kernel.throw(tag, [:snd_throw, tag, get_func_name(2), *rest])
    end

    def raise(tag, *rest)
      msg = format("%s in %s:", tag, get_func_name(2))
      rest.each do |s| msg += format(" %s,", s) end
      msg.chomp!(",")
      exception = case tag
                  when :out_of_range
                    RangeError
                  when :wrong_type_arg
                    TypeError
                  when *Snd_error_tags
                    StandardError
                  else
                    Ruby_exceptions[tag] or RuntimeError
                  end
      Kernel.raise(exception, msg, caller(1))
    end
  end
end

# almost all StandardError
Snd_error_tags = [# clm2xen.c
                  :mus_error,
                  :no_such_method,
                  :wrong_type_arg, # TypeError
                  # snd-0.h
                  :no_such_envelope,
                  :no_such_sample,
                  :no_such_edit,
                  :cannot_save,
                  :cant_update_file,
                  # snd-chn.c
                  :cant_open_file,
                  # snd-dac.c
                  :bad_format,
                  :no_such_player,
                  :arg_error,
                  # snd-draw.c
                  :no_such_widget,
                  :no_such_graphics_context,
                  :no_such_axis,
                  :bad_length,
                  # snd-edits.c
                  :no_such_direction,
                  :no_such_region,
                  :no_such_auto_delete_choice,
                  # snd-env.c
                  :env_error,
                  # snd-error.c
                  :snd_error,
                  # snd-gxcolormaps.c
                  :no_such_colormap,
                  :colormap_error,
                  # snd-key.c
                  :no_such_key,
                  # snd-ladspa.c
                  :no_such_plugin,
                  :plugin_error,
                  # snd-marks.c
                  :no_such_mark,
                  # snd-menu.c
                  :no_such_menu,
                  # snd-mix.c
                  :no_such_mix,
                  # snd-print.c
                  :cannot_print,
                  # snd-region.c
                  :io_error,
                  # run.c
                  :wrong_number_of_args,
                  :cannot_parse,
                  # snd-snd.c
                  :no_such_sound,
                  :not_a_sound_file,
                  :cannot_apply_controls,
                  :bad_size,
                  :snd_internal_error,
                  # snd-xen.c
                  :no_active_selection,
                  :bad_arity,
                  # snd-xmain.c
                  :xt_error,
                  # snd-xchn.c
                  :no_such_color,
                  # snd.c
                  :snd_top_level,
                  :gsl_error,
                  # sndlib2xen.h
                  :out_of_range,
                  :no_such_channel,
                  :no_such_file,
                  :bad_type,
                  :no_data,
                  :bad_header,
                  # xm.c
                  :no_such_resource]

def rb_error_to_mus_tag
  # to_s and string error-names intentional here
  # otherwise e.g. NameError goes to case StandardError!
  case $!.class.to_s
  when "StandardError"
    $!.message.split(/[: ]/).first.downcase.intern
  when "RangeError"
    :out_of_range
  when "TypeError"
    :wrong_type_arg
  when "ArgumentError"
    :wrong_number_of_args
  else
    # converts ruby exceptions to symbols: NoMethodError --> :no_method_error
    $!.class.to_s.gsub(/([A-Z])/) do |c|
      "_" + c.tr("A-Z", "a-z")
    end[1..-1].intern
  end
end

def snd_error_to_message
  $!.message.split(/\n/).first.sub(/^.*: /, "")
end

add_help(:snd_catch,
         "snd_catch([tag=:all, [retval=:undefined]])  \
catchs snd_throw and exceptions and \
returns body's last value wrapped in an array if all goes well.  \
If a snd_throw tag meets snd_catch's, returns an array with the tag name, \
the function name from where was thrown and optional arguments given to snd_throw.  \
If an exception was risen and the exception name meets tag name, \
returns an array with tag name and the exception message, otherwise reraises exception.  \
If retval is given and tag matches exception or snd_throw tag, returns retval.  \
If retval is a procedure, calls retval with tag name and message.

res = snd_catch do 10 + 2 end
puts res ==> [12]

res = Snd.catch(:no_such_file) do
  open_sound(\"unknown-file.snd\")
end
puts res ==> [:no_such_file,
             \"open_sound: no_such_file: Unknown_file.snd No such file or directory\"]

res = Snd.catch(:finish) do
  10.times do |i|
    if i == 8 then snd_throw(:finish, i) end
  end
end
puts res ==> [:finish, \"top_level\", 8]

res = Snd.catch(:all, lambda do |tag, msg| Snd.display([tag, msg]) end) do
  set_listener_prompt(17)
end
==> [:wrong_type_arg, \"set_listener-prompt: wrong type arg 0, 17, wanted a string\"]
puts res ==> nil

The lambda function handles the error in the last case.")
def snd_catch(tag = :all, retval = :undefined, &body)
  Snd.catch(tag, retval, &body)
end

add_help(:snd_throw,
         "snd_throw(tag, *rest)  \
jumps to the corresponding snd_catch('tag') and returns an array \
with tag, function name and possible *rest strings or values.")
def snd_throw(tag, *rest)
  Snd.throw(tag, *rest)
end

class Break < StandardError
end

Ruby_exceptions = {
  :script_error          => ScriptError,
  :load_error            => LoadError,
  :name_error            => NameError,
  :not_implemented_error => NotImplementedError,
  :syntax_error          => SyntaxError,
  :interrupt             => Interrupt,
  :system_exit           => SystemExit,
  :standard_error        => StandardError,
  :arg_error             => ArgumentError,
  :float_domain_error    => FloatDomainError,
  :index_error           => IndexError,
  :io_error              => IOError,
  :eof_error             => EOFError,
  :local_jump_error      => LocalJumpError,
  :no_memory_error       => NoMemoryError,
  :range_error           => RangeError,
  :regexp_error          => RegexpError,
  :runtime_error         => RuntimeError,
  :security_error        => SecurityError,
  :system_call_error     => SystemCallError,
  :system_stack_error    => SystemStackError,
  :thread_error          => ThreadError,
  :type_error            => TypeError,
  :zero_division_error   => ZeroDivisionError,
  :break                 => Break}

add_help(:snd_raise,
         "snd_raise(tag, *rest)  \
raises an exception 'tag' with an error message \
containing function name, tag and possible *rest strings or values.  \
'tag' is a symbol, \
a Ruby exception looks like :local_jump_error instead of LocalJumpError, \
a Snd error tag looks like :no_such_sound.")
def snd_raise(tag, *rest)
  Snd.raise(tag, *rest)
end

def srate
  mus_srate
end unless defined? srate

# general purpose loop

add_help(:gloop,
         "gloop(*args) { |args| ... }
 :step   = 1
 :before = nil (thunk)
 :after  = nil (thunk)

args[0]: Range    (each)
         Hash(s)  (each)
         Array(s) (each_with_index) [args.last == Fixnum --> step]
         Fixnum   (times)
         Fixnum   [args[1] == :step --> step]

A general purpose loop, handling Range, Hash, Array, Vec, Vct, Fixnum,
with optional step.  Returns the result of body as array like map.

Examples:
  Range
    gloop(0..3) do |i| puts i end
  Hash               (loops over all Hashs consecutively)
    gloop({1 => :a, 2 => :b}, {11 => :aa => 22 => :bb}) do |k, v|
      print('key: ', k, ' value: ', v)
      puts
    end
  Array, Vec, Vct
    gloop([0, 1]) do |x, i|
      print(i, ': ', x)
      puts end
  Arrays with step   (mixes all Arrays)
    gloop([0, 1, 2, 3], [:a, :b, :c, :d], [55, 66, 77, 88, 99], 2) do |x, i|
      print(i, ': ', x.inspect)
      puts
    end
  Numeric (like Integer#times)
    gloop(3) do |i| puts i end
  Numeric with step (like Integer#step)
    gloop(6, 2) do |i| puts i end
  a simple body call
    gloop do puts 'empty' end")
def gloop(*args, &body)
  step   = get_shift_args(args, :step, 1)
  before = get_shift_args(args, :before)
  after  = get_shift_args(args, :after)
  do_extra = lambda do |thunk| thunk?(thunk) ? thunk.call : snd_func(thunk) end
  result = []
  case args[0]
  when Range
    args[0].step(step) do |i|
      do_extra.call(before) if before
      result << body.call(i)
      do_extra.call(after) if after
    end
  when Array, Vec, Vct
    lmax = args.map do |x| x.length end.max
    0.step(lmax - 1, step.round) do |i|
      do_extra.call(before) if before
      result << body.call(*args.map do |x| x[i] end << i)
      do_extra.call(after) if after
    end
  when Hash
    args.each do |x| x.each do |k, v|
        do_extra.call(before) if before
        result << body.call(k, v)
        do_extra.call(after) if after
      end
    end
  when Numeric
    0.step(args[0], number?(args[1]) ? args[1] : step) do |i|
      do_extra.call(before) if before
      result << body.call(i)
      do_extra.call(after) if after
    end
  else
    do_extra.call(before) if before
    result << body.call
    do_extra.call(after) if after
  end
  result
end

# get_args(args, key, default = nil)
#
# returns value, whether DEFAULT or value of KEY found in ARGS

def get_args(args, key, default = nil)
  if args.member?(key)
    arg = args[args.index(key) + 1]
    default = arg.nil? ? default : arg
  end
  default
end

def get_shift_args(args, key, default = nil)
  default = get_args(args, key, default)
  if args.member?(key)
    i = args.index(key)
    2.times do args.delete_at(i) end
  end
  default
end

# var = get_class_or_key(args, Klass, :key, default = nil)

def get_class_or_key(args, klass, key, default = nil)
  if (not symbol?(args.first)) and args.first.kind_of?(klass)
    args.shift
  else
    get_shift_args(args, key, default)
  end
end

# var1, var2, var3, var4 = optkey(args, [:key, default],
#                                       [:number, 1],
#                                       [Array, :list, [0, 1, 2, 3]],
#                                       :var_w/o_default_value)
#
# Key-default pairs must be included in brackets while keys alone can
# be included in brackets or not, see last key
# ":var_w/o_default_value" above.  If no default value is specified,
# nil is used.

def optkey(args, *rest)
  args_1 = args.dup
  bind = binding?(rest.car) ? rest.shift : nil
  @locals = nil
  vals = rest.map do |keys|
    val = if array?(keys)
            case keys.length
            when 1
              name = keys.car.to_s
              get_class_or_key(args_1, Object, keys.car, nil)
            when 2
              name = keys.car.to_s
              get_class_or_key(args_1, keys.cadr.class, *keys)
            when 3
              name = keys.cadr.to_s
              get_class_or_key(args_1, *keys)
            else
              assert_type(keys.length.between?(1, 3), keys, 1,
                          "an array of one to three elements [class, :key, default]")
            end
          else
            name = keys.to_s
            get_class_or_key(args_1, Object, keys, nil)
          end
    @locals = val
    eval("#{name} = @locals", bind)
    val
  end
  remove_instance_variable("@locals")
  if vals.length == 1
    vals.first
  else
    vals
  end
end

add_help(:load_init_file,
         "load_init_file(file) \
Returns false if file doesn't exist, otherwise loads it. \
File may reside in current working dir or in $HOME dir.")
def load_init_file(file)
  if File.exists?(file)
    Snd.catch do load(file) end
  elsif File.exists?(f = ENV["HOME"] + "/" + file)
    Snd.catch do load(f) end
  else
    false
  end
end

let(-1) do |count|
  # see rotate_phase(func, snd, chn) in dsp.rb
  # it's necessary to produce a uniq method name
  make_proc_with_setter(:edit_list_proc_counter, lambda { count }, lambda { count += 1 })
end

# clm.rb ends here
