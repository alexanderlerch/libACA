\ clm-ins.fs -- clm-ins.scm|rb -> clm-ins.fs -*- snd-forth -*-

\ Translator/Author: Michael Scholz <mi-scholz@users.sourceforge.net>
\ Created: Fri Feb 03 10:36:51 CET 2006
\ Changed: Thu Jun 25 19:41:26 CEST 2009

\ Commentary:
\
\ jc-reverb    ( keyword-args -- )
\ violin       ( start dur freq amp keyword-args -- )
\ fm-violin    ( start dur freq amp keyword-args -- )
\ 
\ clm-ins.scm|rb instruments
\ 
\ pluck        ( start dur freq amp :optional weighting lossfact -- )
\ vox          ( start dur freq amp ampfun freqfun freqscl voxfun index :optional vibscl -- )
\ fofins       ( start dur freq amp vib f0 a0 f1 a1 f2 a2 :optional ae ve -- )
\ fm-trumpet   ( start dur keyword-args -- )
\ pqw-vox      ( start dur freq spacing-freq amp ampfun freqfun freqscl ... -- )
\ stereo-flute ( start dur freq flow keyword-args -- )
\ fm-bell      ( start dur freq amp :optional amp-env index-env index -- )
\ fm-insect    ( start dur freq amp amp-env ... -- )
\ fm-drum      ( start dur freq amp index :optional high degr dist rev-amt -- )
\ gong         ( start dur freq amp -- )
\ attract      ( start dur amp c -- )
\ pqw          ( start dur sfreq cfreq amp ampfun indexfun parts -- )
\ tubebell     ( start dur freq amp :optional base --)
\ wurley       ( start dur freq amp -- )
\ rhodey       ( start dur freq amp :optional base -- )
\ hammondoid   ( start dur freq amp -- )
\ metal        ( start dur freq amp -- )
\ drone        ( start dur freq amp ampfun synth ampat ampdc rvibamt rvibfreq -- )
\ canter       ( start dur pitch amp ampfun ranfun skewfun ... -- )
\ nrev         ( keyword-args -- )
\ reson        ( start dur pitch amp indxfun skewfun ... -- )
\ cellon       ( start dur pitch0 amp ampfun betafun ... -- )
\ jl-reverb    ( keyword-args -- )
\ gran-synth   ( start dur freq grain-dur interval amp -- )
\ touch-tone   ( numbers keyword-args -- )
\ spectra      ( start dur freq amp :optional parts ampenv vibamp vibfrq degr dist rev-amt -- )
\ two-tab      ( start dur freq amp :optional par1 par2 aenv ienv vamp vfrq degr dist rev -- )
\ lbj-piano    ( start dur freq amp -- )
\ resflt       ( start dur keyword-args -- )
\ scratch-ins  ( start file src-ratio turntable -- )
\ pins         ( file start dur keyword-args -- )
\ zc 	       ( start dur freq amp len1 len2 feedback -- )
\ zn 	       ( start dur freq amp len1 len2 feedforward -- )
\ za 	       ( start dur freq amp len1 len2 fb ffw -- )
\ clm-expsrc   ( start dur in-file exp-ratio src-ratio amp :optional rev start-in-file -- )
\ exp-snd      ( file start dur amp :optional exp-amt ramp seglen sr hop ampenv -- )
\ expfil       ( start dur hopsecs rampsecs steadysecs file1 file2 -- )
\ graph-eq     ( file start dur keyword-args -- )
\ anoi         ( fname start dur :optional fftsize amp-scaler R -- )
\ fullmix      ( in-file :optional start dur inbeg matrix srate reverb-amount -- )
\ bes-fm       ( start dur freq amp ratio index -- )

require clm
require env

\ Prevent name clash with possibly loaded sndins.so.
\ sndins.so instruments can be called with fm-violin-ins etc.
[defined] fm-violin [if] <'> fm-violin alias fm-violin-ins [then]
[defined] jc-reverb [if] <'> jc-reverb alias jc-reverb-ins [then]
[defined] nrev      [if] <'> nrev      alias nrev-ins      [then]

\ General input function for src, granulate etc.
: readin-cb ( gen -- proc; dir self -- r )
  1 proc-create swap ,
 does> ( dir self -- r )
  nip @ ( gen ) readin
;

: reverb-dur ( rev -- dur ) mus-length samples->seconds *clm-decay-time* f+ ;

\ clm/jcrev.ins
instrument: jc-reverb-fs <{ :key
     volume 1.0
     delay1 0.013
     delay2 0.011
     delay3 0.015
     delay4 0.017
     low-pass #f
     doubled #f
     amp-env #f -- }>
  doc" The Chowning reverb.\n\
0 1 440 0.2 <'> fm-violin :reverb <'> jc-reverb with-sound\n\
0 1 440 0.2 <'> fm-violin\n\
  :reverb-data #( :low-pass #t ) :reverb <'> jc-reverb :channels 2 with-sound"
  *output* mus-channels { chans }
  *reverb* mus-channels { rev-chans }
  *reverb* reverb-dur { dur }
  *verbose* if get-func-name rev-chans chans reverb-info then
  :feedback -0.7 :feedforward 0.7 :size 1051 make-all-pass { allpass1 }
  :feedback -0.7 :feedforward 0.7 :size  337 make-all-pass { allpass2 }
  :feedback -0.7 :feedforward 0.7 :size  113 make-all-pass { allpass3 }
  :scaler 0.742 :size 4799 make-comb { comb1 }
  :scaler 0.733 :size 4999 make-comb { comb2 }
  :scaler 0.715 :size 5399 make-comb { comb3 }
  :scaler 0.697 :size 5801 make-comb { comb4 }
  chans 1 > { chan2 }
  chans 4 = { chan4 }
                               :size delay1 seconds->samples make-delay              { outdel1 }
  chan2                     if :size delay2 seconds->samples make-delay else #f then { outdel2 }
  doubled chan4 ||          if :size delay3 seconds->samples make-delay else #f then { outdel3 }
  chan4 doubled chan2 && || if :size delay4 seconds->samples make-delay else #f then { outdel4 }
  amp-env if :envelope amp-env :scaler volume :duration dur make-env else #f then { env-a }
  doubled chan4 && if $" jc-reverb is not set up for doubled reverb in quad" _ error then
  0.0 0.0 { comb-sum comb-sum-1 }
  0.0 dur run
    0.0 rev-chans 0 ?do j i *reverb* in-any f+ loop { in-val }
    allpass3  allpass2  allpass1 in-val 0.0 all-pass  0.0 all-pass  0.0 all-pass { allpass-sum }
    comb-sum-1 { comb-sum-2 }
    comb-sum   to comb-sum-1
    comb1 allpass-sum 0.0 comb
    comb2 allpass-sum 0.0 comb f+
    comb3 allpass-sum 0.0 comb f+
    comb4 allpass-sum 0.0 comb f+ to comb-sum
    low-pass if
      comb-sum comb-sum-2 f+ 0.25 f* comb-sum-1 f2/ f+
    else
      comb-sum
    then { all-sums }
    outdel1 all-sums 0.0 delay { del-a }
    doubled if outdel3 all-sums 0.0 delay del-a f+ to del-a then
    env-a ?dup-if env to volume then
    i del-a volume f* *output* outa drop
    chan2 if
      outdel2 all-sums 0.0 delay { del-b }
      doubled if outdel4 all-sums 0.0 delay del-b f+ to del-b then
      i del-b volume f* *output* outb drop
    then
    chan4 if
      i outdel3 all-sums 0.0 delay volume f* *output* outc drop
      i outdel4 all-sums 0.0 delay volume f* *output* outd drop
    then
  loop
;instrument
<'> jc-reverb-fs alias jc-reverb

\ snd/fm.html
instrument: violin <{ start dur freq amp
     :key
     fm-index      1.0
     amp-env       #( 0 0 25 1 75 1 100 0 )
     index-env     #( 0 1 25 0.4 75 0.6 100 0 )
     degree        0.0
     distance      1.0
     reverb-amount 0.01 -- }>
  doc" Violin example from snd/fm.html.\n\
0 3 440 0.5 :fm-index 0.5 <'> violin with-sound"
  freq hz->radians { frq-scl }
  frq-scl fm-index f* { maxdev }
  5.0 freq flog f/ maxdev f* { index1 }
  8.5 freq flog f- 3.0 freq 1000.0 f/ f+ f/ maxdev 3.0 f* f* { index2 }
  4.0 freq fsqrt f/ maxdev f* { index3 }
  :frequency freq 	 make-oscil { carrier }
  :frequency freq 	 make-oscil { fmosc1 }
  :frequency freq 3.0 f* make-oscil { fmosc2 }
  :frequency freq 4.0 f* make-oscil { fmosc3 }
  :envelope amp-env   :scaler amp    :duration dur make-env { ampf }
  :envelope index-env :scaler index1 :duration dur make-env { indf1 }
  :envelope index-env :scaler index2 :duration dur make-env { indf2 }
  :envelope index-env :scaler index3 :duration dur make-env { indf3 }
  :frequency  5.0 :amplitude 0.0025 frq-scl f* make-triangle-wave { pervib }
  :frequency 16.0 :amplitude 0.005  frq-scl f* make-rand-interp   { ranvib }
  start dur #{ :degree degree :distance distance :reverb-amount reverb-amount } run-instrument
    pervib 0.0 triangle-wave ranvib 0.0 rand-interp f+ { vib }
    carrier
    vib
    fmosc1     vib    0.0 oscil  indf1 env f* f+
    fmosc2 3.0 vib f* 0.0 oscil  indf2 env f* f+
    fmosc3 4.0 vib f* 0.0 oscil  indf3 env f* f+
    0.0 oscil  ampf env f*
  end-run
;instrument

: violin-test <{ :optional start 0.0 dur 1.0 -- }>
  start now!
  now@ dur 440 0.5 violin
  dur 0.2 f+ step
;

\ === FM-Violin (clm/v.ins, snd/v.scm|rb) ===
instrument: fm-violin-fs <{ start dur freq amp
     :key
     fm-index                   1.0
     amp-env                    #( 0 0 25 1 75 1 100 0 )
     periodic-vibrato-rate      5.0
     periodic-vibrato-amplitude 0.0025
     random-vibrato-rate        16.0
     random-vibrato-amplitude   0.005
     noise-freq                 1000.0
     noise-amount               0.0
     ind-noise-freq             10.0
     ind-noise-amount           0.0
     amp-noise-freq             20.0
     amp-noise-amount           0.0
     gliss-env                  #( 0 0 100 0 )
     glissando-amount           0.0
     fm1-env                    #( 0 1 25 0.4 75 0.6 100 0 )
     fm2-env                    #( 0 1 25 0.4 75 0.6 100 0 )
     fm3-env                    #( 0 1 25 0.4 75 0.6 100 0 )
     fm1-rat                    1.0
     fm2-rat                    3.0
     fm3-rat                    4.0
     fm1-index                  #f
     fm2-index                  #f
     fm3-index                  #f
     base                       1.0
     degree                     0.0
     distance                   1.0
     reverb-amount              0.01
     index-type                 'violin -- }>
  doc" FM-Violin from clm/v.ins|snd/v.scm|rb.\n\
0 3 440 0.5 :fm-index 0.5 <'> fm-violin with-sound"
  freq fabs 1.0 f<= if
    $" freq = %s? reset to 440.0" _ #( freq ) string-format warning
    440.0 to freq
  then
  freq hz->radians          { frq-scl }
  fm-index f0<>             { modulate }
  frq-scl fm-index f*       { maxdev }
  index-type 'violin equal? { vln }
  freq flog                  { logfreq }
  freq fsqrt                { sqrtfreq }
  fm1-index unless maxdev vln if 5.0 else 7.5 then logfreq f/ f* pi fmin to fm1-index then
  fm2-index unless
    maxdev 3.0 f* vln if 8.5 logfreq f- 3.0 freq 0.001 f* f+ f/ else 15.0 sqrtfreq f/ then
    f* pi fmin to fm2-index
  then
  fm3-index unless maxdev vln if 4.0 else 8.0 then sqrtfreq f/ f* pi fmin to fm3-index then
  noise-amount             f0=
  fm1-env fm2-env       equal? &&
  fm1-env fm3-env       equal? &&
  fm1-rat fm1-rat floor f- f0= &&
  fm2-rat fm1-rat floor f- f0= &&
  fm2-rat fm2-rat floor f- f0= &&
  fm3-rat fm1-rat floor f- f0= &&
  fm3-rat fm3-rat floor f- f0= && { easy-case }
  easy-case modulate && 1.0 && fm1-index || { norm }
  :frequency freq make-oscil { carrier }
  :envelope amp-env :scaler amp :duration dur :base base make-env { ampf }
  #f #f #f { fmosc1 fmosc2 fmosc3 }
  #f #f #f { indf1 indf2 indf3 }
  modulate if
    easy-case if
      :frequency freq fm1-rat f*
      :coeffs
      #( fm1-rat f>s                  fm1-index
	 fm2-rat fm1-rat f/ fround->s fm2-index
	 fm3-rat fm1-rat f/ fround->s fm3-index ) 1 partials->polynomial make-polyshape
    else
      :frequency freq fm1-rat f* make-oscil
    then to fmosc1
    easy-case unless
      :frequency freq fm2-rat f* make-oscil to fmosc2
      :frequency freq fm3-rat f* make-oscil to fmosc3
      :envelope fm1-env :scaler norm      :duration dur make-env to indf1
      :envelope fm2-env :scaler fm2-index :duration dur make-env to indf2
      :envelope fm3-env :scaler fm3-index :duration dur make-env to indf3
    then
  then
  :envelope gliss-env :scaler glissando-amount frq-scl f* :duration dur make-env { frqf }
  :frequency periodic-vibrato-rate
  :amplitude periodic-vibrato-amplitude frq-scl f* make-triangle-wave { pervib }
  :frequency random-vibrato-rate
  :amplitude random-vibrato-amplitude frq-scl f* make-rand-interp { ranvib }
  #f #f #f { fm-noi ind-noi amp-noi }
  noise-amount f0<> if
    :frequency noise-freq :amplitude noise-amount pi f* make-rand to fm-noi
  then
  ind-noise-freq f0<> ind-noise-amount f0<> && if
    :frequency ind-noise-freq :amplitude ind-noise-amount make-rand-interp to ind-noi
  then
  amp-noise-freq f0<> amp-noise-amount f0<> && if
    :frequency amp-noise-freq :amplitude amp-noise-amount make-rand-interp to amp-noi
  then
  0.0 0.0 1.0 1.0 { vib fuzz ind-fuzz amp-fuzz }
  modulate if
    easy-case if
      start dur #{ :degree degree :distance distance :reverb-amount reverb-amount } run-instrument
	fm-noi if fm-noi 0.0 rand to fuzz then
	frqf env  pervib 0.0 triangle-wave f+  ranvib 0.0 rand-interp f+ to vib
	ind-noi if ind-noi 0.0 rand-interp 1.0 f+ to ind-fuzz then
	amp-noi if amp-noi 0.0 rand-interp 1.0 f+ to amp-fuzz then
	carrier  fmosc1 1.0 vib polyshape  ind-fuzz f* vib f+  0.0  oscil ampf env f*  amp-fuzz f*
      end-run
    else
      start dur #{ :degree degree :distance distance :reverb-amount reverb-amount } run-instrument
	fm-noi if fm-noi 0.0 rand to fuzz then
	frqf env  pervib 0.0 triangle-wave f+  ranvib 0.0 rand-interp f+ to vib
	ind-noi if ind-noi 0.0 rand-interp 1.0 f+ to ind-fuzz then
	amp-noi if amp-noi 0.0 rand-interp 1.0 f+ to amp-fuzz then
	carrier ( gen )
	fmosc1 fm1-rat vib f* fuzz f+ 0.0 oscil  indf1 env f*
	fmosc2 fm2-rat vib f* fuzz f+ 0.0 oscil  indf2 env f* f+
	fmosc3 fm3-rat vib f* fuzz f+ 0.0 oscil  indf3 env f* f+ ind-fuzz f* vib f+ ( fm )
	0.0 ( pm ) oscil ampf env f*  amp-fuzz f*
      end-run
    then
  else
    start dur #{ :degree degree :distance distance :reverb-amount reverb-amount } run-instrument
      fm-noi if fm-noi 0.0 rand to fuzz then
      frqf env  pervib 0.0 triangle-wave f+  ranvib 0.0 rand-interp f+ to vib
      ind-noi if ind-noi 0.0 rand-interp 1.0 f+ to ind-fuzz then
      amp-noi if amp-noi 0.0 rand-interp 1.0 f+ to amp-fuzz then
      carrier vib 0.0 oscil  ampf env f*  amp-fuzz f*
    end-run
  then
;
<'> fm-violin-fs alias fm-violin

: fm-violin-test <{ :optional start 0.0 dur 1.0 -- }>
  start now!
  now@ dur 440 0.5 fm-violin
  dur 0.2 f+ step
;

\ === CLM-INS.(RB|SCM) ===
\ (with original comments from clm-ins.scm)

hide
: get-optimum-c { s o p -- t c }
  o 1/f s o fsin f* 1.0 s f- s o fcos f* f+ fatan2 f* { pa }
  p pa f- f>s { tmp_int } tmp_int unless 1 to tmp_int then
  p pa f- tmp_int f- { pc }
  begin pc 0.1 f< while tmp_int 1 - to tmp_int pc 1e f+ to pc repeat
  tmp_int
  o fsin o pc f* fsin f- o o pc f* f+ fsin f/
;
: tune-it { f s1 -- s c t }
  mus-srate f f/ { p }
  s1 f0= if 0.5 else s1 then { s }
  f hz->radians { o }
  s o p get-optimum-c { t1 c1 }
  1.0 s f- o p get-optimum-c { t2 c2 }
  s 0.5 f<> c1 fabs c2 fabs f< && if 1.0 s f- c1 t1 else s c2 t2 then
;
set-current

\ PLUCK
\
\ The Karplus-Strong algorithm as extended by David Jaffe and Julius
\ Smith -- see Jaffe and Smith, "Extensions of the Karplus-Strong
\ Plucked-String Algorithm" CMJ vol 7 no 2 Summer 1983, reprinted in
\ "The Music Machine".  translated from CLM's pluck.ins
instrument: pluck <{ start dur freq amp
     :optional
     weighting 0.5
     lossfact  0.9 -- }>
  doc" Implements the Jaffe-Smith plucked string physical model.  \
WEIGHTING is the ratio of the once-delayed to the twice-delayed samples.  \
It defaults to 0.5 = shortest decay.  \
Anything other than 0.5 = longer decay.  \
Must be between 0 and less than 1.0.  \
LOSSFACT can be used to shorten decays.  \
Most useful values are between 0.8 and 1.0.\n\
  0 1 330 0.3 0.95 0.95 <'> pluck with-sound"
  freq weighting tune-it { wt0 c dlen }
  lossfact f0= if 1.0 else 1.0 lossfact fmin then { lf }
  wt0 f0= if 0.5 else 1.0 wt0 fmin then { wt }
  lf 1.0 wt f- f* lf wt f* make-one-zero { allp }
  c 1.0 make-one-zero { feedb }
  dlen 0.0 make-vct map 1.0 2.0 mus-random f- end-map { tab }
  start dur #{ :degree 90.0 random } run-instrument
    tab cycle-ref { val }
    tab i dlen mod 1.0 c f- feedb allp val one-zero one-zero f* vct-set! drop
    amp val f*
  end-run
;instrument
previous

: pluck-test <{ :optional start 0.0 dur 1.0 -- }>
  start now!
  now@ dur 330 0.3 0.95 0.95 pluck
  dur 0.2 f+ step
;

\ formant center frequencies for a male speaker (vox and pqw-vox)
#{ :I:   #( 390.0 1990.0 2550.0 )
   :UH:  #( 520.0 1190.0 2390.0 )
   :U:   #( 440.0 1020.0 2240.0 )
   :W:   #( 300.0  610.0 2200.0 )
   :Y:   #( 300.0 2200.0 3065.0 )
   :L:   #( 300.0 1300.0 3000.0 )
   :D:   #( 300.0 1700.0 2600.0 )
   :N:   #( 280.0 1700.0 2600.0 )
   :T:   #( 200.0 1700.0 2600.0 )
   :TH:  #( 200.0 1400.0 2200.0 )
   :V:   #( 175.0 1100.0 2400.0 )
   :ZH:  #( 175.0 1800.0 2000.0 )
   :E:   #( 530.0 1840.0 2480.0 )
   :A:   #( 730.0 1090.0 2440.0 )
   :OO:  #( 300.0  870.0 2240.0 )
   :LL:  #( 380.0  880.0 2575.0 )
   :EE:  #( 260.0 3500.0 3800.0 )
   :I2:  #( 350.0 2300.0 3340.0 )
   :G:   #( 250.0 1350.0 2000.0 )
   :NG:  #( 280.0 2300.0 2750.0 )
   :K:   #( 350.0 1350.0 2000.0 )
   :S:   #( 200.0 1300.0 2500.0 )
   :THE: #( 200.0 1600.0 2200.0 )
   :ZZ:  #( 900.0 2400.0 3800.0 )
   :AE:  #( 660.0 1720.0 2410.0 )
   :OW:  #( 570.0  840.0 2410.0 )
   :ER:  #( 490.0 1350.0 1690.0 )
   :R:   #( 420.0 1300.0 1600.0 )
   :LH:  #( 280.0 1450.0 1600.0 )
   :B:   #( 200.0  800.0 1750.0 )
   :M:   #( 280.0  900.0 2200.0 )
   :P:   #( 300.0  800.0 1750.0 )
   :F:   #( 175.0  900.0 4400.0 )
   :SH:  #( 200.0 1800.0 2000.0 )
   :Z:   #( 200.0 1300.0 2500.0 )
   :VV:  #( 565.0 1045.0 2400.0 ) } value clm-ins-formants

\ MLBVOI
\ 
\ translation from MUS10 of Marc LeBrun's waveshaping voice instrument
\ (using FM here) this version translated (and simplified slightly)
\ from CLM's mlbvoi.ins
instrument: vox <{ start dur freq amp ampfun freqfun freqscl voxfun index
     :optional
     vibscl 0.1 -- }>
  voxfun length { size }
  size make-array { f1 }
  size make-array { f2 }
  size make-array { f3 }
  size 1- 0 ?do
    clm-ins-formants voxfun i 1+ object-ref hash-ref { phon }
    voxfun i object-ref { n }
    f1 i n array-set!
    phon 0 array-ref f1 i 1+ rot array-set!
    f2 i n array-set!
    phon 1 array-ref f2 i 1+ rot array-set!
    f3 i n array-set!
    phon 2 array-ref f3 i 1+ rot array-set!
  2 +loop
  :frequency 0.0 make-oscil { car-os }
  6 make-array map :frequency 0.0 make-oscil end-map { ofs }
  :envelope ampfun :scaler amp :duration dur make-env { ampf }
  :envelope f1 :duration dur make-env { frmf1 }
  :envelope f2 :duration dur make-env { frmf2 }
  :envelope f3 :duration dur make-env { frmf3 }
  :envelope freqfun :duration dur :scaler freqscl freq f* :offset freq make-env { freqf }
  :frequency  6.0 :amplitude freq vibscl f* make-triangle-wave { per-vib }
  :frequency 20.0 :amplitude freq   0.01 f* make-rand-interp   { ran-vib }
  6 0.0 make-vct { freqs }
  6 0.0 make-vct { amps }
  start dur #{ :degree 90.0 random } run-instrument
    freqf env per-vib 0.0 triangle-wave f+ ran-vib 0.0 rand-interp f+ { frq }
    frmf1 env    { frm }
    frm frq f/   { frm0 }
    frm0 floor dup f>s { frm-fint frm-int }
    frm-int 2 mod unless
      freqs 0 frm-fint frq f* hz->radians        vct-set! drop
      freqs 1 frm-fint 1.0 f+ frq f* hz->radians vct-set! drop
      amps  1 frm0 frm-fint f-                   vct-set! drop
      amps  0 1.0 amps 1 vct-ref f-              vct-set! drop
    else
      freqs 1 frm-fint frq f* hz->radians        vct-set! drop
      freqs 0 frm-fint 1.0 f+ frq f* hz->radians vct-set! drop
      amps  0 frm0 frm-fint f-                   vct-set! drop
      amps  1 1.0 amps 0 vct-ref f-              vct-set! drop
    then
    frmf2 env    to frm
    frm frq f/   to frm0
    frm0 floor   to frm-fint
    frm-fint f>s to frm-int
    frm-int 2 mod unless
      freqs 2 frm-fint frq f* hz->radians        vct-set! drop
      freqs 3 frm-fint 1.0 f+ frq f* hz->radians vct-set! drop
      amps  3 frm0 frm-fint f-                   vct-set! drop
      amps  2 1.0 amps 3 vct-ref f-              vct-set! drop
    else
      freqs 3 frm-fint frq f* hz->radians        vct-set! drop
      freqs 2 frm-fint 1.0 f+ frq f* hz->radians vct-set! drop
      amps  2 frm0 frm-fint f-                   vct-set! drop
      amps  3 1.0 amps 2 vct-ref f-              vct-set! drop
    then
    frmf3 env    to frm
    frm frq f/   to frm0
    frm0 floor   to frm-fint
    frm-fint f>s to frm-int
    frm-int 2 mod unless
      freqs 4 frm-fint frq f* hz->radians        vct-set! drop
      freqs 5 frm-fint 1.0 f+ frq f* hz->radians vct-set! drop
      amps  5 frm0 frm-fint f-                   vct-set! drop
      amps  4 1.0 amps 5 vct-ref f-              vct-set! drop
    else
      freqs 5 frm-fint frq f* hz->radians        vct-set! drop
      freqs 4 frm-fint 1.0 f+ frq f* hz->radians vct-set! drop
      amps  4 frm0 frm-fint f-                   vct-set! drop
      amps  5 1.0 amps 4 vct-ref f-              vct-set! drop
    then
    car-os frq hz->radians 0.0 oscil index f* { caros }
    ofs 0 array-ref caros 0.2 f* freqs 0 vct-ref f+ 0.0 oscil amps 0 vct-ref f*
    ofs 1 array-ref caros 0.2 f* freqs 1 vct-ref f+ 0.0 oscil amps 1 vct-ref f* f+ 0.80 f*
    ofs 2 array-ref caros 0.5 f* freqs 2 vct-ref f+ 0.0 oscil amps 2 vct-ref f*
    ofs 3 array-ref caros 0.5 f* freqs 3 vct-ref f+ 0.0 oscil amps 3 vct-ref f* f+ 0.15 f* f+
    ofs 4 array-ref caros        freqs 4 vct-ref f+ 0.0 oscil amps 4 vct-ref f*
    ofs 5 array-ref caros        freqs 5 vct-ref f+ 0.0 oscil amps 5 vct-ref f* f+ 0.05 f* f+
    ampf env f*
  end-run
;instrument

: vox-test <{ :optional start 0.0 dur 1.0 -- }>
  start now!
  #( 0 0 25 1 75 1 100 0 )  { amp-env }
  #( 0 0 5 0.5 10 0 100 1 ) { frq-env }
  #( 0 :E: 25 :AE: 35 :ER: 65 :ER: 75 :I: 100 :UH: ) { examp1 }
  #( 0 :I: 5 :OW: 10 :I: 50 :AE: 100 :OO: )          { examp2 }

  now@ dur 170 0.4 amp-env frq-env 0.1 examp1 0.05 0.1 vox
  dur 0.2 f+ step
  now@ dur 300 0.4 amp-env frq-env 0.1 examp2 0.02 0.1 vox
  dur 0.2 f+ step
  now@ 5.0 600 0.4 amp-env frq-env 0.1 examp2 0.01 0.1 vox
  5.0 0.2 f+ step
;

\ FOF example
\
\ snd/sndclm.html, section wave-train
instrument: fofins <{ start dur freq amp vib f0 a0 f1 a1 f2 a2
     :optional
     ae #( 0 0 25 1 75 1 100 0 )
     ve #( 0 1 100 1 ) -- }>
  doc" produces FOF synthesis.\n\
0 1 270 0.2 0.001 730 0.6 1090 0.3 2440 0.1 <'> fofins with-sound"
  :envelope ae :scaler amp :duration dur make-env { ampf }
  :frequency 6.0 make-oscil { vibr }
  :envelope ve :scaler vib :duration dur make-env { vibenv }
  f0 hz->radians { frq0 }
  f1 hz->radians { frq1 }
  f2 hz->radians { frq2 }
  mus-srate 22050.0 f= if 100 else 200 then { foflen }
  two-pi foflen f/ { win-freq }
  foflen 0.0 make-vct map
    a0 i frq0 f* fsin f*
    a1 i frq1 f* fsin f* f+
    a2 i frq2 f* fsin f* f+ f2/
    1.0 i win-freq f* fcos f- f*
  end-map { foftab }
  :frequency freq :wave foftab make-wave-train { wt0 }
  start dur #{ :degree 90.0 random } run-instrument
    ampf env  wt0  vibenv env vibr 0.0 0.0 oscil f*  wave-train  f*
  end-run
;instrument

: fofins-test <{ :optional start 0.0 dur 1.0 -- }>
  start now!
  now@ dur 270 0.2 0.001 730 0.6 1090 0.3 2440 0.1 fofins
  dur 0.2 f+ step
;

\ FM TRUMPET
\
\ Dexter Morrill's FM-trumpet: from CMJ feb 77 p51
instrument: fm-trumpet <{ start dur
     :key
     frq1     250
     frq2     1500
     amp1     0.5
     amp2     0.1
     ampatt1  0.03
     ampdec1  0.35
     ampatt2  0.03
     ampdec2  0.3
     modfrq1  250
     modind11 0
     modind12 2.66
     modfrq2  250
     modind21 0
     modind22 1.8
     rvibamp  0.007
     rvibfrq  125
     vibamp   0.007
     vibfrq   7
     vibatt   0.6
     vibdec   0.2
     frqskw   0.03
     frqatt   0.06
     ampenv1  #( 0 0 25 1 75 0.9 100 0 )
     ampenv2  #( 0 0 25 1 75 0.9 100 0 )
     indenv1  #( 0 0 25 1 75 0.9 100 0 )
     indenv2  #( 0 0 25 1 75 0.9 100 0 ) -- }>
  doc" 0 2 <'> fm-trumpet with-sound"
  :envelope
  #( 0 1  25 0.1  75 0  100 0 )
  25
  100 vibatt dur f/ f* 45.0 fmin
  75
  100 1 vibdec dur f/ f- f* 55.0 fmax stretch-envelope
  :scaler vibamp :duration dur                          make-env { per-vib-f }
  :frequency rvibfrq :amplitude rvibamp                 make-rand-interp { ran-vib }
  :frequency vibfrq                                     make-oscil { per-vib }
  75 100 1 0.01 dur f/ f- f* fmax { dec-01 }
  :envelope
  #( 0 0  25 1  75 1  100 0 )
  25 25 100 frqatt dur f/ f* fmin
  75 dec-01 stretch-envelope
  :scaler frqskw :duration dur                          make-env { frq-f }
  25 100 ampatt1 dur f/ f* fmin { ampattpt1 }
  75 100 1 ampdec1 dur f/ f- f* fmax { ampdecpt1 }
  25 100 ampatt2 dur f/ f* fmin { ampattpt2 }
  75 100 1 ampdec2 dur f/ f- f* fmax { ampdecpt2 }
  :envelope indenv1 25 ampattpt1 75 dec-01 stretch-envelope
  :scaler modfrq1 modind12 modind11 f- f* :duration dur make-env { mod1-f }
  :frequency 0.0                                        make-oscil { mod1 }
  :frequency 0.0                                        make-oscil { car1 }
  :envelope ampenv1 25 ampattpt1 75 ampdecpt1 stretch-envelope
  :scaler amp1 :duration dur make-env { car1-f }
  :envelope indenv2 25 ampattpt2 75 dec-01 stretch-envelope
  :scaler modfrq2 modind22 modind21 f- f* :duration dur make-env { mod2-f }
  :frequency 0.0                                        make-oscil { mod2 }
  :frequency 0.0                                        make-oscil { car2 }
  :envelope ampenv2 25 ampattpt2 75 ampdecpt2 stretch-envelope
  :scaler amp2 :duration dur                            make-env { car2-f }
  start dur #{ :degree 90.0 random } run-instrument
    ran-vib 0.0 rand-interp 1.0 f+
    1.0 per-vib-f env per-vib 0.0 0.0 oscil f* f+ f*
    1.0 frq-f env f+ f* hz->radians { frq-change }
    car1-f env
    car1 mod1 modfrq1 frq-change f* 0.0 oscil mod1-f env f* frq1 f+ frq-change f* 0.0 oscil f*
    car2-f env
    car2 mod2 modfrq2 frq-change f* 0.0 oscil mod2-f env f* frq2 f+ frq-change f* 0.0 oscil f* f+
  end-run
;instrument

: fm-trumpet-test <{ :optional start 0.0 dur 1.0 -- }>
  start now!
  now@ dur fm-trumpet
  dur 0.2 f+ step
;

struct
  cell% field sin-evens
  cell% field cos-evens
  cell% field sin-odds
  cell% field cos-odds
  cell% field frmfs
  cell% field sin-coeffs
  cell% field cos-coeffs
  cell% field amps
end-struct pqw-vox%

\ PQWVOX
\
\ translation of CLM pqwvox.ins (itself translated from MUS10 of MLB's
\ waveshaping voice instrument (using phase quadrature waveshaping))
instrument: pqw-vox <{ start dur
     freq spacing-freq
     amp ampfun
     freqfun freqscl
     phonemes
     formant-amps formant-shapes -- }>
  :frequency 0.0 make-oscil { car-sin }
  :frequency 0.0 :initial-phase half-pi make-oscil { car-cos }
  :envelope ampfun :scaler amp :duration dur make-env { ampf }
  :envelope freqfun :scaler freqscl freq f* :duration dur :offset freq make-env { freqf }
  :frequency 6.0 :amplitude freq 0.1 f* make-triangle-wave { per-vib }
  :frequency 20.0 :amplitude freq 0.05 f* make-rand-interp { ran-vib }
  phonemes length { plen }
  plen make-array { phone1 }
  plen make-array { phone2 }
  plen make-array { phone3 }
  plen 1- 0 ?do
    phonemes i object-ref { ph }
    phone1 i ph array-set!
    phone2 i ph array-set!
    phone3 i ph array-set!
    clm-ins-formants phonemes i 1+ object-ref hash-ref { ary }
    phone1 i 1+ ary 0 object-ref array-set!
    phone2 i 1+ ary 1 object-ref array-set!
    phone3 i 1+ ary 2 object-ref array-set!
  2 +loop
  phone1 phone2 phone3 3 >array { phones }
  nil { pv }
  formant-amps map
    pqw-vox% %alloc to pv
    :frequency 0.0 make-oscil pv sin-evens !
    :frequency 0.0 make-oscil pv sin-odds !
    :frequency 0.0 :initial-phase half-pi make-oscil pv cos-evens !
    :frequency 0.0 :initial-phase half-pi make-oscil pv cos-odds !
    formant-shapes i object-ref normalize-partials { shape }
    shape mus-chebyshev-first-kind  partials->polynomial pv cos-coeffs !
    shape mus-chebyshev-second-kind partials->polynomial pv sin-coeffs !
    :envelope phones i array-ref :duration dur make-env pv frmfs !
    formant-amps i object-ref pv amps !
    pv
  end-map { values }
  4 0.0 make-vct { vals }
  spacing-freq freq f/ { frq-ratio }
  start dur #{ :degree 90.0 random } run-instrument
    freqf env per-vib 0.0 triangle-wave f+ ran-vib 0.0 rand-interp f+ { frq }
    frq frq-ratio f* hz->radians { frqscl }
    car-sin frqscl 0.0 oscil { carsin }
    car-cos frqscl 0.0 oscil { carcos }
    0.0 ( sum )
    values each to pv
      pv frmfs @ env frq f/ { frm0 }
      frm0 floor            { frm-fint }
      frm-fint f>s 2 mod unless
	vals 0 frm-fint frq f* hz->radians       vct-set! drop ( even-freq )
	vals 1 frm-fint 1e f+ frq f* hz->radians vct-set! drop ( odd-freq )
	vals 3 frm0 frm-fint f-                  vct-set! drop ( odd-amp )
	vals 2 1.0 vals 3 vct-ref f-             vct-set! drop ( even-amp )
      else
	vals 1 frm-fint frq f* hz->radians       vct-set! drop ( odd-freq )
	vals 0 frm-fint 1e f+ frq f* hz->radians vct-set! drop ( even-freq )
	vals 2 frm0 frm-fint f-                  vct-set! drop ( even-amp )
	vals 3 1.0 vals 2 vct-ref f-             vct-set! drop ( odd-amp )
      then
      pv cos-coeffs @ carcos polynomial { fax }
      pv sin-coeffs @ carcos polynomial carsin f* { yfax }
      pv sin-evens @ vals 0 vct-ref 0.0 oscil yfax f*
      pv cos-evens @ vals 0 vct-ref 0.0 oscil fax f* f- vals 2 vct-ref f*
      pv sin-odds @  vals 1 vct-ref 0.0 oscil yfax f*
      pv cos-odds @  vals 1 vct-ref 0.0 oscil fax f* f- vals 3 vct-ref f* f+ pv amps @ f* f+
    end-each
    ampf env f*
  end-run
  values each ( pv ) free throw end-each
;instrument

: pqw-vox-test <{ :optional start 0.0 dur 1.0 -- }>
  start now!
  #( 0 0 50 1 100 0 ) { ampfun }
  #( 0 0 100 0 ) { freqfun }
  #( 0 0 100 1 ) { freqramp }
  #( #( 1 1 2 0.5 ) #( 1 0.5 2 0.5 3 1 ) #( 1 1 4 0.5 ) ) { shapes1 }
  #( #( 1 1 2 0.5 ) #( 1 1 2 0.5 3 0.2 4 0.1 ) #( 1 1 3 0.1 4 0.5 ) ) { shapes2 }
  #( #( 1 1 2 0.5 ) #( 1 1 4 0.1 ) #( 1 1 2 0.1 4 0.05 ) ) { shapes3 }
  #( #( 1 1 2 0.5 3 0.1 4 0.01 ) #( 1 1 4 0.1 ) #( 1 1 2 0.1 4 0.05 ) ) { shapes4 }
   #( 0.8 0.15 0.05 ) { amps }

  now@ dur 300 300 0.5 ampfun freqfun  0.00 #( 0 :L:  100 :L: ) #( 0.33 0.33 0.33 ) shapes1 pqw-vox
  dur 0.2 f+ step
  now@ dur 200 200 0.5 ampfun freqramp 0.10 #( 0 :UH: 100 :ER: ) amps shapes2 pqw-vox
  dur 0.2 f+ step
  now@ dur 100 314 0.5 ampfun freqramp 0.10 #( 0 :UH: 100 :ER: ) amps shapes2 pqw-vox
  dur 0.2 f+ step
  now@ dur 200 314 0.5 ampfun freqramp 0.01 #( 0 :UH: 100 :ER: ) amps shapes3 pqw-vox
  dur 0.2 f+ step
  now@ dur 100 414 0.5 ampfun freqramp 0.01 #( 0 :OW: 50  :E: 100 :ER: ) amps shapes4 pqw-vox
  dur 0.2 f+ step
;

\ STEREO-FLUTE
instrument: stereo-flute <{ start dur freq flow
     :key
     flow-envelope   #( 0 1 100 1 )
     decay           0.01
     noise           0.0356
     embouchure-size 0.5
     fbk-scl1        0.5
     fbk-scl2        0.55
     out-scl         1.0
     a0              0.7
     b1              -0.3
     vib-rate        5.0
     vib-amount      0.03
     ran-rate        5.0
     ran-amount      0.03 -- }>
 doc" A physical model of a flute.\n\
0 1 440 0.55 :flow-envelope #( 0 0 1 1 2 1 3 0 ) <'> stereo-flute with-sound"
  :envelope flow-envelope :scaler flow :duration dur decay f- make-env { flowf }
  :frequency vib-rate                            make-oscil       { p-vib }
  :frequency ran-rate                          	 make-rand-interp { ran-vib }
  :frequency mus-srate f2/ :amplitude 1.0      	 make-rand        { breath }
  mus-srate freq f/ fround->s { periodic-samples }
  embouchure-size periodic-samples f* fround->s         make-delay       { emb }
  periodic-samples                  	         make-delay       { bore }
  a0 b1                                        	 make-one-pole    { rlf }
  0.0 0.0 0.0 0.0     { emb-sig delay-sig out-sig prev-out-sig }
  0.0 0.0 0.0 0.0 0.0 { cur-exit cur-diff cur-flow dc-blocked prev-dc-blocked }
  start dur #{ :degree 90.0 random } run-instrument
    bore  out-sig  0.0 delay to delay-sig
    emb   cur-diff 0.0 delay to emb-sig
    p-vib 0.0      0.0 oscil       vib-amount f*
    ran-vib        0.0 rand-interp ran-amount f* f+
                                    flowf env    f+ to cur-flow
    breath 0.0 rand cur-flow f* noise f* cur-flow  f+  fbk-scl1 delay-sig f*  f+ to cur-diff
    emb-sig  emb-sig emb-sig f* emb-sig f*  f- to cur-exit
    rlf  fbk-scl2 delay-sig f*  cur-exit f+  one-pole to out-sig
    \ ;; NB the DC blocker is not in the cicuit. It is applied to the out-sig 
    \ ;; but the result is not fed back into the system.
    out-sig prev-out-sig f-  0.995 prev-dc-blocked f*  f+  to dc-blocked
    out-sig to prev-out-sig
    dc-blocked to prev-dc-blocked
    out-scl dc-blocked f*
  end-run
;instrument

: flute-test <{ :optional start 0.0 dur 1.0 -- }>
  start now!
  now@ dur 440 0.55 :flow-envelope #( 0 0 1 1 2 1 3 0 ) stereo-flute
  dur 0.2 f+ step
;

\ FM-BELL
instrument: fm-bell <{ start dur freq amp
     :optional
     amp-env   #( 0 0 0.1 1 10 0.6 25 0.3 50 0.15 90 0.1 100 0 )
     index-env #( 0 1 2 1.1 25 0.75 75 0.5 100 0.2 )
     index 1.0 -- }>
  freq 32.0 f* hz->radians                 { fm-ind1 }
  8.0 freq 50.0 f/ f- 4.0 f* hz->radians   { fm-ind2 }
  1.4 freq 250.0 f/ f- 0.705 f* fm-ind2 f* { fm-ind3 }
  20.0 freq 20.0 f/ f- 32.0 f* hz->radians { fm-ind4 }
  :frequency freq f2*     			  make-oscil { mod1 }
  :frequency freq 1.41 f* 			  make-oscil { mod2 }
  :frequency freq 2.82 f* 			  make-oscil { mod3 }
  :frequency freq 2.4 f*  			  make-oscil { mod4 }
  :frequency freq         			  make-oscil { car1 }
  :frequency freq         			  make-oscil { car2 }
  :frequency freq 2.4 f*  			  make-oscil { car3 }
  :envelope amp-env   :scaler amp   :duration dur make-env   { ampf }
  :envelope index-env :scaler index :duration dur make-env   { indf }
  0.0 { fmenv }
  start dur #{ :degree 90.0 random } run-instrument
    indf env to fmenv
    car1  fmenv fm-ind1 f* mod1 0.0 0.0 oscil f*  0.0 oscil
    car2  fmenv fm-ind2 mod2 0.0 0.0 oscil f*
                fm-ind3 mod3 0.0 0.0 oscil f* f+ f*  0.0 oscil 0.15 f* f+
    car3  fmenv fm-ind4 f* mod4 0.0 0.0 oscil f*  0.0 oscil 0.15 f* f+
    ampf env f*
  end-run
;instrument

: fm-bell-test <{ :optional start 0.0 dur 1.0 -- }>
  start now!
  now@ dur 440.0 0.5 fm-bell
  dur 0.2 f+ step
;

\ FM-INSECT
\ clm/insect.ins
instrument: fm-insect <{ start dur
     freq amp amp-env
     mod-freq mod-skew mod-freq-env
     mod-index mod-index-env
     fm-index fm-ratio -- }>
  :frequency freq                                                     make-oscil { carrier }
  :frequency mod-freq                                                 make-oscil { fm1-osc }
  :frequency fm-ratio freq f*                                         make-oscil { fm2-osc }
  :envelope amp-env       :scaler amp                   :duration dur make-env   { ampf }
  :envelope mod-index-env :scaler mod-index hz->radians :duration dur make-env   { indf }
  :envelope mod-freq-env  :scaler mod-skew hz->radians  :duration dur make-env   { modfrqf }
  fm-index fm-ratio f* freq f* hz->radians { fm2-amp }
  start dur #{ :degree 90.0 random } run-instrument
    fm1-osc modfrqf env             0.0 oscil  indf env f* { garble-in }
    fm2-osc garble-in               0.0 oscil  fm2-amp  f* { garble-out }
    carrier garble-out garble-in f+ 0.0 oscil  ampf env f*
  end-run
;instrument    

: fm-insect-test <{ :optional start 0.0 dur 1.0 -- }>
  start now!
  #( 0 0 40 1 95 1 100 0.5 )    { locust }
  #( 0 1 25 0.7 75 0.78 100 1 ) { bug-hi }
  #( 0 0 25 1 75 0.7 100 0 )    { amp }

  now@ 0.000 f+ 1.699 4142.627 0.015 amp 60 -16.707 locust 500.866 bug-hi 0.346 0.5 fm-insect
  now@ 0.195 f+ 0.233 4126.284 0.030 amp 60 -12.142 locust 649.490 bug-hi 0.407 0.5 fm-insect
  now@ 0.217 f+ 2.057 3930.258 0.045 amp 60  -3.011 locust 562.087 bug-hi 0.591 0.5 fm-insect
  now@ 2.100 f+ 1.500  900.627 0.060 amp 40 -16.707 locust 300.866 bug-hi 0.346 0.5 fm-insect
  now@ 3.000 f+ 1.500  900.627 0.060 amp 40 -16.707 locust 300.866 bug-hi 0.046 0.5 fm-insect
  now@ 3.450 f+ 1.500  900.627 0.090 amp 40 -16.707 locust 300.866 bug-hi 0.006 0.5 fm-insect
  now@ 3.950 f+ 1.500  900.627 0.120 amp 40 -10.707 locust 300.866 bug-hi 0.346 0.5 fm-insect
  now@ 4.300 f+ 1.500  900.627 0.090 amp 40 -20.707 locust 300.866 bug-hi 0.246 0.5 fm-insect
  6.0 step
;

\ FM-DRUM
\
\ Jan Mattox's fm drum:
instrument: fm-drum <{ start dur freq amp index
     :optional
     high    #f
     degr    0.0
     dist    1.0
     rev-amt 0.01 -- }>
  high if 3.414 8.525 else 1.414 3.515 then { casrat fmrat }
  :envelope #( 0 0 25 0 75 1 100 1 )
  :scaler   high if 66.0 hz->radians else 0 then :duration dur make-env { glsf }
  #( 0 0 3 0.05 5 0.2 7 0.8 8 0.95 10 1.0 12 0.95 20 0.3 30 0.1 100 0 ) { ampfun }
  100 high if 0.01 else 0.015 then f* dur f/ { atdrpt }
  :envelope ampfun 10 atdrpt 15 atdrpt 1 f+ 100 100 dur 0.2 f- dur f/ f* f- fmax stretch-envelope
  :scaler   amp :duration dur                                  make-env { ampf }
  #( 0 0 5 0.014 10 0.033 15 0.061 20 0.099
     25 0.153 30 0.228 35 0.332 40 0.477 45 0.681
     50 0.964 55 0.681 60 0.478 65 0.332 70 0.228
     75 0.153 80 0.099 85 0.061 90 0.033 95 0.0141 100 0 ) { indxfun }
  100 100 dur 0.1 f- dur f/ f* f- { indxpt }
  indxfun 50 atdrpt 65 indxpt stretch-envelope { divindxf }
  :envelope divindxf :duration dur
  :scaler   index fmrat freq f* f* hz->radians pi fmin         make-env { indxf }
  :envelope divindxf :duration dur
  :scaler   index casrat freq f* f* hz->radians pi fmin        make-env { mindxf }
  :envelope ampfun 10 atdrpt 90 atdrpt 1 f+ 100 100 dur 0.05 f- dur f/ f* f- fmax stretch-envelope
  :duration dur
  :scaler   7000 hz->radians pi fmin                           make-env { devf }
  :frequency 7000 :amplitude 1                                 make-rand { rn }
  :frequency freq make-oscil { car }
  :frequency freq fmrat  f*                                    make-oscil { fmosc }
  :frequency freq casrat f*                                    make-oscil { cc }
  0.0 { gls }
  start dur #{ :degree degr :distance dist :reverb-amount rev-amt } run-instrument
    glsf env to gls
    cc  devf env  rn 0.0 rand f*  gls casrat f* f+  0.0 oscil
    mindxf env f*  gls fmrat f* f+       fmosc swap 0.0 oscil
    indxf env f* gls f+                  car swap   0.0 oscil ampf env f*
  end-run
;instrument

: fm-drum-test <{ :optional start 0.0 dur 1.0 -- }>
  start now!
  now@ dur 55 0.3 5    fm-drum
  dur 0.2 f+ step
  now@ dur 66 0.3 4 #t fm-drum
  dur 0.2 f+ step
;

\ FM-GONG
\
\ Paul Weineke's gong.
instrument: gong <{ start dur freq amp
     :key
     degree        0.0
     distance      1.0
     reverb-amount 0.005 -- }>
  0.01 1.160 freq f* f* hz->radians { indx01 }
  0.30 1.160 freq f* f* hz->radians { indx11 }
  0.01 3.140 freq f* f* hz->radians { indx02 }
  0.38 3.140 freq f* f* hz->radians { indx12 }
  0.01 1.005 freq f* f* hz->radians { indx03 }
  0.50 1.005 freq f* f* hz->radians { indx13 }
  5 { atpt }
  100 0.002 dur f/ f* { atdur }
  #( 0 0 3 1 15 0.5 27 0.25 50 0.1 100 0 ) { expf }
  #( 0 0 15 0.3 30 1.0 75 0.5 100 0 ) { rise }
  #( 0 0 75 1.0 98 1.0 100 0 ) { fmup }
  #( 0 0 2 1.0 100 0 ) { fmdwn }
  :envelope expf atpt atdur 0 0 stretch-envelope :scaler amp :duration dur make-env { ampfun }
  :envelope fmup  :scaler indx11 indx01 f- :duration dur :offset indx01    make-env { indxfun1 }
  :envelope fmdwn :scaler indx12 indx02 f- :duration dur :offset indx02    make-env { indxfun2 }
  :envelope rise  :scaler indx13 indx03 f- :duration dur :offset indx03    make-env { indxfun3 }
  :frequency freq make-oscil { car }
  :frequency freq 1.160 f* make-oscil { mod1 }
  :frequency freq 3.140 f* make-oscil { mod2 }
  :frequency freq 1.005 f* make-oscil { mod3 }
  start dur #{ :degree degree :distance distance :reverb-amount reverb-amount } run-instrument
    car
    mod3 0.0 0.0 oscil indxfun3 env f*
    mod2 0.0 0.0 oscil indxfun2 env f* f+
    mod1 0.0 0.0 oscil indxfun1 env f* f+
    0.0 oscil ampfun env f*
  end-run
;instrument

: gong-test <{ :optional start 0.0 dur 1.0 -- }>
  start now!
  now@ dur 261.61 0.6 gong
  dur 0.2 f+ step
;

\ ATTRACT
\
\ by James McCartney, from CMJ vol 21 no 3 p 6
instrument: attract <{ start dur amp c -- }>
  0.2 0.2 { a b }
  0.04 { dt }
  amp f2/ c f/ { scale }
  -1.0 { x }
  0.0 0.0 0.0 { x1 y z }
  start dur #{ :degree 90.0 random } run-instrument
    x  y z f+  dt f* f- to x1
    a y f*  x f+  dt f* +to y
    x z f*  b f+  c z f*  f-  dt f* +to z
    x1 to x
    scale x f*
  end-run
;instrument

: attract-test <{ :optional start 0.0 dur 1.0 -- }>
  start now!
  now@ dur 0.5 2.0 attract
  dur 0.2 f+ step
;

\ PQW
\
\ phase-quadrature waveshaping used to create asymmetric (i.e. single
\ side-band) spectra.  The basic idea here is a variant of sin x sin y
\ - cos x cos y = cos (x + y)
\ 
\ clm/pqw.ins
instrument: pqw <{ start dur sfreq cfreq amp ampfun indexfun parts
     :key
     degree        0.0
     distance      1.0
     reverb-amount 0.005 -- }>
  parts normalize-partials { nparts }
  :frequency sfreq :initial-phase half-pi make-oscil { sp-cos }
  :frequency sfreq make-oscil { sp-sin }
  :frequency cfreq :initial-phase half-pi make-oscil { c-cos }
  :frequency cfreq make-oscil { c-sin }
  nparts mus-chebyshev-second-kind partials->polynomial { sin-coeffs }
  nparts mus-chebyshev-first-kind  partials->polynomial { cos-coeffs }
  :envelope ampfun :scaler amp :duration dur make-env { amp-env }
  :envelope indexfun :duration dur make-env { ind-env }
  0.0 0.0 0.0 0.0 { vib ax fax yfax }
  cfreq sfreq f/ { r }
  :frequency 5.0  :amplitude 0.005 sfreq f* hz->radians make-triangle-wave { tr }
  :frequency 12.0 :amplitude 0.005 sfreq f* hz->radians make-rand-interp { rn }
  start dur #{ :degree degree :distance distance :reverb-amount reverb-amount  } run-instrument
    tr 0.0 triangle-wave rn 0.0 rand-interp f+ to vib
    1.0 ind-env env fmin  sp-cos vib 0.0 oscil f* to ax
    cos-coeffs ax polynomial to fax
    sp-sin vib 0.0 oscil  sin-coeffs ax polynomial f* to yfax
    c-sin vib r f* 0.0 oscil yfax f*
    c-cos vib r f* 0.0 oscil fax f* f- amp-env env f*
  end-run
;instrument

: pqw-test <{ :optional start 0.0 dur 1.0 -- }>
  start now!
  now@ dur 200 1000 0.2 #( 0 0 25 1 100 0 ) #( 0 1 100 0 ) #( 2 0.1 3 0.3 6 0.5 ) pqw
  dur 0.2 f+ step
;

\ taken from Perry Cook's stkv1.tar.Z (Synthesis Toolkit), but I was
\ in a bit of a hurry and may not have made slavishly accurate
\ translations.  Please let me (bil@ccrma.stanford.edu) know of any
\ serious (non-envelope) errors.
\
\ from Perry Cook's TubeBell.cpp
instrument: tubebell <{ start dur freq amp :optional base 32.0 -- }>
  :frequency freq 0.995 f*          make-oscil { osc0 }
  :frequency freq 0.995 1.414 f* f* make-oscil { osc1 }
  :frequency freq 1.005 f*          make-oscil { osc2 }
  :frequency freq 1.414 f*          make-oscil { osc3 }
  :envelope #( 0 0 0.005 1 dur 0.006 fmax 0 ) :base base     :duration dur make-env { ampenv1 }
  :envelope #( 0 0 0.001 1 dur 0.002 fmax 0 ) :base base f2* :duration dur make-env { ampenv2 }
  :frequency 2.0 make-oscil { ampmod }
  amp f2/ { g0 }
  g0 0.707 f* { g1 }
  start dur #{ :degree 90.0 random } run-instrument
    ampmod 0.0 0.0 oscil 0.007 f* 0.993 f+
    osc0  osc1 0.0 0.0 oscil 0.203 f*  0.0 oscil ampenv1 env g1 f* f*
    osc2  osc3 0.0 0.0 oscil 0.144 f*  0.0 oscil ampenv2 env g0 f* f*  f+  f*
  end-run
;instrument

: tubebell-test <{ :optional start 0.0 dur 1.0 -- }>
  start now!
  now@ dur 440 0.2 32 tubebell
  dur 0.2 f+ step
;

\ from Perry Cook's Wurley.cpp
instrument: wurley <{ start dur freq amp -- }>
  :frequency freq      make-oscil { osc0 }
  :frequency freq 4 f* make-oscil { osc1 }
  :frequency 510       make-oscil { osc2 }
  :frequency 510       make-oscil { osc3 }
  :frequency 8         make-oscil { ampmod }
  :envelope #( 0 0 1 1 9 1 10 0 )         :duration dur make-env { ampenv }
  :envelope #( 0 0 0.001 1 0.15 0 dur 0.16 fmax 0 ) :duration dur make-env { indenv }
  :envelope #( 0 0 0.001 1 0.25 0 dur 0.26 fmax 0 ) :duration dur make-env { resenv }
  amp f2/ { g0 }
  g0 0.307 f* { g1 }
  start dur #{ :degree 90.0 random } run-instrument
    ampenv env
    ampmod 0.0 0.0 oscil 0.007 f* 1.0 f+  f*
    osc0  osc1 0.0 0.0 oscil               0.307 f*  0.0 oscil g0 f*
    osc2  osc3 0.0 0.0 oscil indenv env f* 0.117 f*  0.0 oscil g1 f* resenv env f*  f+  f*
  end-run
;instrument

: wurley-test <{ :optional start 0.0 dur 1.0 -- }>
  start now!
  now@ dur 440 0.2 wurley
  dur 0.2 f+ step
;

\ from Perry Cook's Rhodey.cpp
instrument: rhodey <{ start dur freq amp :optional base 0.5 -- }>
  :frequency freq make-oscil { osc0 }
  :frequency freq make-oscil { osc1 }
  :frequency freq make-oscil { osc2 }
  :frequency freq make-oscil { osc3 }
  :envelope #( 0 0 0.005 1 dur 0.006 fmax 0 )  :base base        :duration dur make-env { ampenv1 }
  :envelope #( 0 0 0.001 1 dur 0.002 fmax 0 )  :base base 1.5 f* :duration dur make-env { ampenv2 }
  :envelope #( 0 0 0.001 1 0.25 0 ) :base base 4 f*   :duration dur make-env { ampenv3 }
  amp f2/ { g0 }
  start dur #{ :degree 90.0 random } run-instrument
    osc0  osc1 0.0 0.0 oscil 0.535 f*  0.0 oscil ampenv1 env f* g0 f*
    osc2  osc3 0.0 0.0 oscil 0.109 f* ampenv3 env f*  0.0 oscil ampenv2 env f* g0 f* f+
  end-run
;instrument

: rhodey-test <{ :optional start 0.0 dur 1.0 -- }>
  start now!
  now@ dur 440 0.2 0.5 rhodey
  dur 0.2 f+ step
;

\ from Perry Cook's BeeThree.cpp
instrument: hammondoid <{ start dur freq amp -- }>
  :frequency freq 0.999 f* make-oscil { osc0 }
  :frequency freq 1.997 f* make-oscil { osc1 }
  :frequency freq 3.006 f* make-oscil { osc2 }
  :frequency freq 6.009 f* make-oscil { osc3 }
  :envelope #( 0 0 0.005 1 dur 0.006 fmax 0.008 f- 1 dur 0 ) :duration dur make-env { ampenv1 }
  :envelope #( 0 0 0.005 1 dur 0.006 fmax 0 )                :duration dur make-env { ampenv2 }
  amp f2/ { g0 }
  start dur #{ :degree 90.0 random } run-instrument
    osc0 0.0 0.0 oscil 0.1875 amp f* f*
    osc1 0.0 0.0 oscil 0.1875 amp f* f* f+
    osc2 0.0 0.0 oscil g0 f* f+ ampenv1 env f*
    osc3 0.0 0.0 oscil 0.375 amp f* f* ampenv2 env f* f+
  end-run
;instrument

: hammondoid-test <{ :optional start 0.0 dur 1.0 -- }>
  start now!
  now@ dur 440 0.2 hammondoid
  dur 0.2 f+ step
;

\ from Perry Cook's HeavyMtl.cpp
instrument: metal <{ start dur freq amp -- }>
  :frequency freq                 make-oscil { osc0 }
  :frequency freq 4 f* 0.999 f*   make-oscil { osc1 }
  :frequency freq 3 f* 1.001 f*   make-oscil { osc2 }
  :frequency freq 0.5 f* 1.002 f* make-oscil { osc3 }
  :envelope #( 0 0 0.001 1 dur 0.002 fmax 0.002 f- 1 dur 0 ) :duration dur make-env { ampenv0 }
  :envelope #( 0 0 0.001 1 dur 0.002 fmax 0.011 f- 1 dur 0 ) :duration dur make-env { ampenv1 }
  :envelope #( 0 0 0.010 1 dur 0.020 fmax 0.015 f- 1 dur 0 ) :duration dur make-env { ampenv2 }
  :envelope #( 0 0 0.030 1 dur 0.040 fmax 0.040 f- 1 dur 0 ) :duration dur make-env { ampenv3 }
  start dur #{ :degree 90.0 random } run-instrument
    osc0
    osc1  osc2 0.0 0.0 oscil ampenv2 env f* 0.574 f*  0.0 oscil ampenv1 env f* 0.202 f*
    osc3 0.0 0.0 oscil ampenv3 env f* 0.116 f* f+  ( osc0 ) 0.0 oscil ampenv0 env f* 0.615 amp f* f*
  end-run
;instrument

: metal-test <{ :optional start 0.0 dur 1.0 -- }>
  start now!
  now@ dur 440 0.2 metal
  dur 0.2 f+ step
;

\ DRONE
instrument: drone <{ start dur freq amp ampfun synth ampat ampdc rvibamt rvibfreq -- }>
  :frequency freq :wave  synth #f #f partials->wave  make-table-lookup { s }
  :envelope ampfun 25.0 100.0 ampat dur f/ f* 75.0 100.0 100.0 ampdc dur f/ f* f- stretch-envelope
  :scaler amp 0.25 f*
  :duration dur make-env { ampenv }
  :frequency rvibfreq :amplitude rvibamt freq f* hz->radians make-rand { ranvib }
  start dur #{ :degree 90.0 random } run-instrument
    s  ranvib 0.0 rand fabs  table-lookup ampenv env f*
  end-run
;instrument

\ CANTER
instrument: canter <{ start dur pitch amp
     ampfun ranfun skewfun skewpc ranpc ranfreq indexfun atdr dcdr
     ampfun1 indfun1 fmtfun1
     ampfun2 indfun2 fmtfun2
     ampfun3 indfun3 fmtfun3
     ampfun4 indfun4 fmtfun4 -- }>
  pitch 400.0 f/ flog 910.0 400.0 f/ flog f/ 100.0 f* floor { k }
  100.0 atdr dur f/ f*                               { atpt }
  100.0 100.0 dcdr dur f/ f* f-                      { dcpt }
  k fmtfun1 1.0 envelope-interp                      { lfmt1 }
  0.5 lfmt1 pitch f/ f+ floor                        { harm1 }
  k indfun1 1.0 envelope-interp pitch f* hz->radians { dev11 }
  dev11 f2/                                          { dev01 }
  k ampfun1 1.0 envelope-interp amp f* 1.0 harm1 lfmt1 pitch f/ f- fabs f- f* { lamp1 }
  k fmtfun2 1.0 envelope-interp                      { lfmt2 }
  0.5 lfmt2 pitch f/ f+ floor                        { harm2 }
  k indfun2 1.0 envelope-interp pitch f* hz->radians { dev12 }
  dev12 f2/                                          { dev02 }
  k ampfun2 1.0 envelope-interp amp f* 1.0 harm2 lfmt2 pitch f/ f- fabs f- f* { lamp2 }
  k fmtfun3 1.0 envelope-interp                      { lfmt3 }
  0.5 lfmt3 pitch f/ f+ floor                        { harm3 }
  k indfun3 1.0 envelope-interp pitch f* hz->radians { dev13 }
  dev13 f2/                                          { dev03 }
  k ampfun3 1.0 envelope-interp amp f* 1.0 harm3 lfmt3 pitch f/ f- fabs f- f* { lamp3 }
  k fmtfun4 1.0 envelope-interp                      { lfmt4 }
  0.5 lfmt4 pitch f/ f+ floor                        { harm4 }
  k indfun4 1.0 envelope-interp pitch f* hz->radians { dev14 }
  dev14 f2/                                          { dev04 }
  k ampfun4 1.0 envelope-interp amp f* 1.0 harm4 lfmt4 pitch f/ f- fabs f- f* { lamp4 }
  :envelope ampfun 25.0 atpt 75.0 dcpt stretch-envelope
  :duration dur make-env                             { tampfun }
  :envelope skewfun 25.0 atpt 75.0 dcpt stretch-envelope
  :duration dur
  :scaler pitch skewpc f* hz->radians make-env       { tskwfun }
  :envelope ranfun 25.0 atpt 75.0 dcpt stretch-envelope
  :duration dur make-env                             { tranfun }
  :envelope indexfun 25.0 atpt 75.0 dcpt stretch-envelope
  :duration dur make-env                             { tidxfun }
  :frequency pitch make-oscil                        { modgen }
  :frequency pitch harm1 f* make-oscil   	     { gen1 }
  :frequency pitch harm2 f* make-oscil   	     { gen2 }
  :frequency pitch harm3 f* make-oscil   	     { gen3 }
  :frequency pitch harm4 f* make-oscil   	     { gen4 }
  :frequency ranfreq :amplitude ranpc pitch f* hz->radians make-rand { ranvib }
  start dur #{ :degree 90.0 random } run-instrument
    tskwfun env tranfun env ranvib 0.0 rand f* f+ { frqval }
    modgen frqval 0.0 oscil { modval }
    tampfun env { ampval }
    tidxfun env { indval }
    gen1  dev01  indval dev11 f*  f+  modval f*  frqval f+  harm1 f* 0.0 oscil lamp1 ampval f* f*
    gen2  dev02  indval dev12 f*  f+  modval f*  frqval f+  harm2 f* 0.0 oscil lamp3 ampval f* f* f+
    gen3  dev03  indval dev13 f*  f+  modval f*  frqval f+  harm3 f* 0.0 oscil lamp3 ampval f* f* f+
    gen4  dev04  indval dev14 f*  f+  modval f*  frqval f+  harm4 f* 0.0 oscil lamp4 ampval f* f* f+
  end-run
;instrument

: drone/canter-test <{ :optional start 0.0 dur 1.0 -- }>
  start now!
  #( 0 1200 100 1000 )     { fmt1 }
  #( 0 2250 100 1800 )	   { fmt2 }
  #( 0 4500 100 4500 )	   { fmt3 }
  #( 0 6750 100 8100 )	   { fmt4 }
  #( 0 0.67 100 0.70 )	   { amp1 }
  #( 0 0.95 100 0.95 )	   { amp2 }
  #( 0 0.28 100 0.33 )	   { amp3 }
  #( 0 0.14 100 0.15 )	   { amp4 }
  #( 0 0.75 100 0.65 )	   { ind1 }
  #( 0 0.75 100 0.75 )	   { ind2 }
  #( 0 1 100 1 )	   { ind3 }
  #( 0 1 100 1 )	   { ind4 }
  #( 0 0 100 0 )	   { skwf }
  #( 0 0 25 1 75 1 100 0 ) { ampf }
  #( 0 0.5 100 0.5 )	   { ranf }
  #( 0 1 100 1 )           { index }
  #( 0 0 5 1 95 1 100 0 )  { solid }
  #( 0.5 0.06 1 0.62 1.5 0.07 2 0.6 2.5 0.08 3 0.56 4 0.24 5 0.98 6 0.53
     7 0.16 8 0.33 9 0.62 10 0.12 12 0.14 14 0.86 16 0.12 23 0.14 24 0.17 ) { bassdr2 }
  #( 0.3 0.04 1 0.81 2 0.27 3 0.2 4 0.21 5 0.18 6 0.35 7 0.03
     8 0.07 9 0.02 10 0.025 11 0.035 ) { tenordr }

  now@ 4 115.0 0.125 solid bassdr2 0.1 0.5 0.01 10 drone
  now@ 4 229.0 0.125 solid tenordr 0.1 0.5 0.01 11 drone
  now@ 4 229.5 0.125 solid tenordr 0.1 0.5 0.01 09 drone
  now@ 2.100 918.000 0.175 ampf ranf skwf 0.050 0.01 10 index 0.005 0.005
  amp1 ind1 fmt1 amp2 ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4 canter
  now@ 2.100 f+ 0.300 688.500 0.175 ampf ranf skwf 0.050 0.01 10 index 0.005 0.005
  amp1 ind1 fmt1 amp2 ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4 canter
  now@ 2.400 f+ 0.040 826.200 0.175 ampf ranf skwf 0.050 0.01 10 index 0.005 0.005
  amp1 ind1 fmt1 amp2 ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4 canter
  now@ 2.440 f+ 0.560 459.000 0.175 ampf ranf skwf 0.050 0.01 10 index 0.005 0.005
  amp1 ind1 fmt1 amp2 ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4 canter
  now@ 3.000 f+ 0.040 408.000 0.175 ampf ranf skwf 0.050 0.01 10 index 0.005 0.005
  amp1 ind1 fmt1 amp2 ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4 canter
  now@ 3.040 f+ 0.040 619.650 0.175 ampf ranf skwf 0.050 0.01 10 index 0.005 0.005
  amp1 ind1 fmt1 amp2 ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4 canter
  now@ 3.080 f+ 0.040 408.000 0.175 ampf ranf skwf 0.050 0.01 10 index 0.005 0.005
  amp1 ind1 fmt1 amp2 ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4 canter
  now@ 3.120 f+ 0.040 688.500 0.175 ampf ranf skwf 0.050 0.01 10 index 0.005 0.005
  amp1 ind1 fmt1 amp2 ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4 canter
  now@ 3.160 f+ 0.290 459.000 0.175 ampf ranf skwf 0.050 0.01 10 index 0.005 0.005
  amp1 ind1 fmt1 amp2 ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4 canter
  now@ 3.450 f+ 0.150 516.375 0.175 ampf ranf skwf 0.050 0.01 10 index 0.005 0.005
  amp1 ind1 fmt1 amp2 ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4 canter
  now@ 3.600 f+ 0.040 826.200 0.175 ampf ranf skwf 0.050 0.01 10 index 0.005 0.005
  amp1 ind1 fmt1 amp2 ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4 canter
  now@ 3.640 f+ 0.040 573.750 0.175 ampf ranf skwf 0.050 0.01 10 index 0.005 0.005
  amp1 ind1 fmt1 amp2 ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4 canter
  now@ 3.680 f+ 0.040 619.650 0.175 ampf ranf skwf 0.050 0.01 10 index 0.005 0.005
  amp1 ind1 fmt1 amp2 ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4 canter
  now@ 3.720 f+ 0.180 573.750 0.175 ampf ranf skwf 0.050 0.01 10 index 0.005 0.005
  amp1 ind1 fmt1 amp2 ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4 canter
  now@ 3.900 f+ 0.040 688.500 0.175 ampf ranf skwf 0.050 0.01 10 index 0.005 0.005
  amp1 ind1 fmt1 amp2 ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4 canter
  now@ 3.940 f+ 0.260 459.000 0.175 ampf ranf skwf 0.050 0.01 10 index 0.005 0.005
  amp1 ind1 fmt1 amp2 ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4 canter
  4.4 step
;

\ NREV (the most popular Samson box reverb)
\
\ REVERB-FACTOR controls the length of the decay -- it should not
\ exceed (/ 1.0 .823), LP-COEFF controls the strength of the low pass
\ filter inserted in the feedback loop, VOLUME can be used to boost the
\ reverb output.
\
\ clm/nrev.ins
instrument: nrev-fs <{ :key
     reverb-factor 1.09
     lp-coeff      0.7
     lp-out-coeff  0.85
     output-scale  1.0
     volume        1.0
     amp-env       #( 0 1 1 1 ) -- }>
  doc" NREV (the most popular Samson box reverb).\n\
<'> fm-violin-test :reverb <'> nrev with-sound"
  *output* mus-channels { chans }
  *reverb* mus-channels { rev-chans }
  *reverb* reverb-dur { dur }
  *verbose* if get-func-name rev-chans chans reverb-info then
  mus-srate 25641.0 f/ { sr }
  #( 1433 1601 1867 2053 2251 2399 347 113 37 59 43 37 29 19 ) map
    sr *key* f* f>s dup 2 mod unless 1+ then ( val )
    begin ( val ) dup prime? false? while 2 + repeat ( val )
  end-map { dly-len }
  :scaler 0.822 reverb-factor f*  :size dly-len  0 array-ref make-comb { comb0 }
  :scaler 0.802 reverb-factor f*  :size dly-len  1 array-ref make-comb { comb1 }
  :scaler 0.773 reverb-factor f*  :size dly-len  2 array-ref make-comb { comb2 }
  :scaler 0.753 reverb-factor f*  :size dly-len  3 array-ref make-comb { comb3 }
  :scaler 0.753 reverb-factor f*  :size dly-len  4 array-ref make-comb { comb4 }
  :scaler 0.733 reverb-factor f*  :size dly-len  5 array-ref make-comb { comb5 }
  :feedback -0.7 :feedforward 0.7 :size dly-len  6 array-ref make-all-pass { allp0 }
  :feedback -0.7 :feedforward 0.7 :size dly-len  7 array-ref make-all-pass { allp1 }
  :feedback -0.7 :feedforward 0.7 :size dly-len  8 array-ref make-all-pass { allp2 }
  :feedback -0.7 :feedforward 0.7 :size dly-len  9 array-ref make-all-pass { allp3 }
  :feedback -0.7 :feedforward 0.7 :size dly-len 10 array-ref make-all-pass { allp4 }
  :feedback -0.7 :feedforward 0.7 :size dly-len 11 array-ref make-all-pass { allp5 }
  :feedback -0.7 :feedforward 0.7 :size dly-len 12 array-ref make-all-pass { allp6 }
  :feedback -0.7 :feedforward 0.7 :size dly-len 13 array-ref make-all-pass { allp7 }
  lp-coeff lp-coeff 1.0 f- make-one-pole { low }
  lp-out-coeff lp-coeff 1.0 f- make-one-pole { low-a }
  lp-out-coeff lp-coeff 1.0 f- make-one-pole { low-b }
  lp-out-coeff lp-coeff 1.0 f- make-one-pole { low-c }
  lp-out-coeff lp-coeff 1.0 f- make-one-pole { low-d }
  :envelope amp-env :scaler output-scale :duration dur make-env { ampf }
  0.0 dur run
    0.0 ( rev ) rev-chans 0 ?do j i *reverb* in-any f+ loop volume f* ampf env f* { rev }
    0.0 ( outrev )
    comb0 rev 0.0 comb f+
    comb1 rev 0.0 comb f+
    comb2 rev 0.0 comb f+
    comb3 rev 0.0 comb f+
    comb4 rev 0.0 comb f+
    comb5 rev 0.0 comb f+ { outrev }
    allp2  allp1  allp0 outrev 0.0 all-pass  0.0 all-pass  0.0 all-pass to outrev
    allp3  low outrev one-pole  0.0 all-pass to outrev
    low-a  allp4 outrev 0.0 all-pass  one-pole 	output-scale f* { sample-a }
    low-b  allp5 outrev 0.0 all-pass  one-pole 	output-scale f* { sample-b }
    low-c  allp6 outrev 0.0 all-pass  one-pole 	output-scale f* { sample-c }
    low-d  allp7 outrev 0.0 all-pass  one-pole 	output-scale f* { sample-d }
    chans 2 = if
      i sample-a sample-d f+ f2/ *output* outa drop
    else
      i sample-a *output* outa drop
    then
    chans 2 = chans 4 = || if
      chans 2 = if
	i sample-b sample-c f+ f2/ *output* outb drop
      else
	i sample-b *output* outb drop
      then
    then
    chans 4 = if
      i sample-c *output* outc drop
      i sample-d *output* outd drop
    then
  loop
;instrument
<'> nrev-fs alias nrev

struct
  cell% field carriers
  cell% field ampfs
  cell% field indfs
  cell% field c-rats
end-struct reson%

\ RESON
instrument: reson <{ start dur pitch amp
     indxfun skewfun pcskew
     skewat skewdc
     vibfreq vibpc
     ranvibfreq ranvibpc data -- }>
  :frequency pitch make-oscil { mod }
  :envelope skewfun 25 100 skewat dur f/ f* 75 100 100 skewdc dur f/ f* f- stretch-envelope
  :scaler pcskew pitch f* hz->radians
  :duration dur                                                  make-env { frqf }
  :frequency vibfreq :amplitude vibpc pitch f* hz->radians       make-triangle-wave { pervib }
  :frequency ranvibfreq :amplitude ranvibpc pitch f* hz->radians make-rand-interp { ranvib }
  0.0 data each ( lst-val ) 2 object-ref f+ end-each { totalamp }
  nil { rs }
  data object->array map! *key* { frmdat }
    reson% %alloc to rs
    frmdat 0 object-ref { ampf }
    frmdat 1 object-ref { freq }
    frmdat 2 object-ref { rfamp }
    frmdat 3 object-ref dur f/ 100.0 f* { ampat }
    100.0  frmdat 4 object-ref dur f/ 100.0 f*  f- { ampdc }
    frmdat 5 object-ref freq f* hz->radians { dev0 }
    frmdat 6 object-ref freq f* hz->radians { dev1 }
    frmdat 7 object-ref dur f/ 100.0 f* { indxat }
    100.0  frmdat 8 object-ref dur f/ 100.0 f*  f- { indxdc }
    freq pitch f/ fround->s { harm }
    1.0  harm freq pitch f/ f- fabs  f- { rsamp }
    pitch harm f* { cfq }
    ampat f0= if 25.0 to ampat then
    ampdc f0= if 75.0 to ampdc then
    indxat f0= if 25.0 to indxat then
    indxdc f0= if 75.0 to indxdc then
    :envelope indxfun 25 indxat 75 indxdc stretch-envelope
    :scaler dev1 dev0 f-
    :offset dev0
    :duration dur          make-env rs indfs !
    :envelope ampf 25 ampat 75 ampdc stretch-envelope
    :scaler rsamp amp  rfamp totalamp f/  f* f*
    :duration dur          make-env rs ampfs !
    harm rs c-rats !
    :frequency cfq         make-oscil rs carriers !
    rs
  end-map { values }
  start dur #{ :degree 90.0 random } run-instrument
    pervib 0.0 triangle-wave ranvib 0.0 rand-interp f+ frqf env f+ { vib }
    mod vib 0.0 oscil { modsig }
    0.0
    values each { rs }
      rs ampfs @ env rs carriers @  vib rs c-rats @ f* rs indfs @ env modsig f* f+  0.0 oscil f* f+
    end-each
  end-run
  values each ( rs ) free throw end-each
;instrument

: reson-test <{ :optional start 0.0 dur 1.0 -- }>
  start now!
  #( #( #( 0 0 100 1 ) 1200 0.5 0.1 0.1 0 1.0 0.1 0.1 )
     #( #( 0 1 100 0 ) 2400 0.5 0.1 0.1 0 1.0 0.1 0.1 ) ) { data }

  now@ dur 440 0.5 #( 0 0 100 1 ) #( 0 0 100 1 ) 0.1 0.1 0.1 5 0.01 5 0.01 data reson
  dur 0.2 f+ step
;

\ STK's feedback-fm instrument named CelloN in Sambox-land
instrument: cellon <{ start dur pitch0 amp ampfun
     betafun beta0 beta1 betaat betadc ampat ampdc
     pitch1 glissfun glissat glissdc
     pvibfreq pvibpc pvibfun pvibat pvibdc
     rvibfreq rvibpc rvibfun -- }>
  pitch1 f0= if pitch0 else pitch1 then { pit1 }
  :frequency pitch0                   make-oscil         { carr }
  0.5 -0.5                            make-one-zero      { low }
  :frequency pitch0                   make-oscil         { fmosc }
  :frequency pvibfreq :amplitude 1.0  make-triangle-wave { pvib }
  :frequency rvibfreq :amplitude 1.0  make-rand-interp   { rvib }
  ampat f0> if 100 ampat dur f/ f* else 25 then          { ampap }
  ampdc f0> if 100 1 ampdc dur f/ f- f* else 75 then     { ampdp }
  glissat f0> if 100 glissat dur f/ f* else 25 then      { glsap }
  glissdc f0> if 100 1 glissdc dur f/ f- f* else 75 then { glsdp }
  betaat f0> if 100 betaat dur f/ f* else 25 then        { betap }
  betadc f0> if 100 1 betadc dur f/ f- f* else 75 then   { betdp }
  pvibat f0> if 100 pvibat dur f/ f* else 25 then        { pvbap }
  pvibdc f0> if 100 1 pvibdc dur f/ f- f* else 75 then   { pvbdp }
  :envelope pvibfun 25 pvbap 75 pvbdp stretch-envelope
  :scaler pvibpc pitch0 f* hz->radians
  :duration dur                                   make-env { pvibenv }
  :envelope rvibfun
  :scaler rvibpc pitch0 f* hz->radians
  :duration dur                                   make-env { rvibenv }
  :envelope glissfun 25 glsap 75 glsdp stretch-envelope
  :scaler pit1 pitch0 f- hz->radians
  :duration dur                                   make-env { glisenv }
  :envelope ampfun 25 ampap 75 ampdp stretch-envelope
  :scaler amp
  :duration dur                                   make-env { amplenv }
  :envelope betafun 25 betap 75 betdp stretch-envelope
  :scaler beta1 beta0 f-
  :offset beta0
  :duration dur                                   make-env { betaenv }
  0.0 { fm }
  start dur #{ :degree 90.0 random } run-instrument
    pvibenv env pvib 0.0 triangle-wave f*
    rvibenv env rvib 0.0 rand-interp f* f+
    glisenv env f+ { vib }
    low  betaenv env  fmosc  vib fm f+  0.0 oscil  f*  one-zero to fm
    amplenv env  carr  vib fm f+ 0.0 oscil  f*
  end-run
;instrument

: cellon-test <{ :optional start 0.0 dur 1.0 -- }>
  start now!
  
  now@ dur 220 0.5
  #( 0 0 25 1 75 1 100 0 )		\ ampfun
  #( 0 0 25 1 75 1 100 0 )		\ betafun
  0.75 1 0 0 0 0 220
  #( 0 0 25 1 75 1 100 0 )		\ glissfun
  0 0 0 0
  #( 0 0 100 0 )			\ pvibfun
  0 0 0 0
  #( 0 0 100 0 )			\ rvibfun
  cellon
  dur 0.2 f+ step
;

\ JL-REVERB
instrument: jl-reverb <{ :key -- }>
  *output* mus-channels { chans }
  *reverb* mus-channels { rev-chans }
  *reverb* reverb-dur { dur }
  *verbose* if get-func-name rev-chans chans reverb-info then
  :feedback -0.7 :feedforward 0.7 :size 2111 make-all-pass { allpass1 }
  :feedback -0.7 :feedforward 0.7 :size  673 make-all-pass { allpass2 }
  :feedback -0.7 :feedforward 0.7 :size  223 make-all-pass { allpass3 }
  :scaler 0.742 :size  9601 make-comb { comb1 }
  :scaler 0.733 :size 10007 make-comb { comb2 }
  :scaler 0.715 :size 10799 make-comb { comb3 }
  :scaler 0.697 :size 11597 make-comb { comb4 }
  :size 0.013 seconds->samples make-delay { outdel1 }
  chans 1 > if :size 0.011 seconds->samples make-delay else #f then { outdel2 }
  chans 2 > if :size 0.015 seconds->samples make-delay else #f then { outdel3 }
  chans 3 > if :size 0.017 seconds->samples make-delay else #f then { outdel4 }
  0.0 { allpass-sum }
  0.0 { all-sums }
  0.0 dur run
    0.0 rev-chans 0 ?do j i *reverb* in-any f+ loop { in-val }
    allpass3  allpass2  allpass1 in-val  0.0 all-pass 0.0 all-pass 0.0 all-pass to allpass-sum
    comb1 allpass-sum 0.0 comb
    comb2 allpass-sum 0.0 comb f+
    comb3 allpass-sum 0.0 comb f+
    comb4 allpass-sum 0.0 comb f+ to all-sums
    i  outdel1 all-sums 0.0 delay  *output*  outa drop
    outdel2 if i  outdel2 all-sums 0.0 delay  *output* 	outb drop then
    outdel3 if i  outdel3 all-sums 0.0 delay  *output* 	outc drop then
    outdel4 if i  outdel4 all-sums 0.0 delay  *output* 	outd drop then
  loop
;instrument

\ GRAN-SYNTH
instrument: gran-synth <{ start dur freq grain-dur interval amp -- }>
  :envelope #( 0 0 25 1 75 1 100 0 ) :duration grain-dur make-env { grain-env }
  :frequency freq                                        make-oscil { carrier }
  grain-dur interval fmax mus-srate f* fceil f>s { grain-size }
  :frequency interval 1/f :size grain-size               make-wave-train { grains }
  grains mus-data map! grain-env env  carrier 0.0 0.0 oscil  f* end-map drop
  start dur #{ :degree 90.0 random } run-instrument
    grains 0.0 wave-train  amp  f*
  end-run
;instrument

: gran-synth-test <{ :optional start 0.0 dur 1.0 -- }>
  start now!
  now@ dur 100 0.0189 0.02 0.4 gran-synth
  dur 0.2 f+ step
;

\ TOUCH-TONE
\ 
\ clm/ugex.ins
instrument: touch-tone <{ numbers :key start 0.0 -- }>
  doc" (see clm/ugex.ins) NUMBERS is an array with phone numbers.\n\
#( 8 5 7 7 5 8 ) <'> touch-tone with-sound"
  #( 0  697  697  697  770  770  770  852  852  852  941  941  941 ) { tt1 }
  #( 0 1209 1336 1477 1209 1336 1477 1209 1336 1477 1209 1336 1477 ) { tt2 }
  numbers each ( numb ) dup 0= if drop 11 then { idx }
    :frequency tt1 idx array-ref make-oscil { frq1 }
    :frequency tt2 idx array-ref make-oscil { frq2 }
    i 0.3 f* start f+  0.2  #{ :degree 90.0 random } run-instrument
      frq1 0.0 0.0 oscil  frq2 0.0 0.0 oscil  f+  0.25 f*
    end-run
  end-each
;instrument

: touch-tone-test <{ :optional start 0.0 dur 1.0 -- }>
  start now!
  #( 8 5 7 7 5 8 ) :start now@ touch-tone
  dur 6 ( numbers ) f* 0.2 f+ step
;

\ SPECTRA
instrument: spectra <{ start dur freq amp
     :optional
     parts   #( 1 1 2 0.5 )
     ampenv  #( 0 0 50 1 100 0 )
     vibamp  0.005
     vibfrq  5.0
     degr    0.0
     dist    1.0
     rev-amt 0.005 -- }>
  :frequency freq :wave parts #f #f partials->wave  make-table-lookup { s }
  :envelope ampenv :scaler amp :duration dur        make-env { ampf }
  freq hz->radians vibamp f* { vamp }
  :frequency vibfrq :amplitude vamp                 make-triangle-wave { pervib }
  :frequency vibfrq 1.0 f+ :amplitude vamp           make-rand-interp { ranvib }
  start dur #{ :degree degr :distance dist :reverb-amount rev-amt } run-instrument
    s  pervib 0.0 triangle-wave ranvib 0.0 rand-interp f+  table-lookup  ampf env  f*
  end-run
;instrument

: spectra-test <{ :optional start 0.0 dur 1.0 -- }>
  start now!
  #( 1.00 0.1132 2.00 0.0252 3.00 0.0292 4.01 0.0136 5.03 0.0045
     6.06 0.0022 7.11 0.0101 8.17 0.0004 9.23 0.0010 10.33 0.0012
     11.44 0.0013 12.58 0.0011 13.75 0.0002 14.93 0.0005 16.14 0.0002 ) { p-a4 }
  now@ dur 440 2.0 p-a4 #( 0 0 1 1 5 0.9 12 0.5 25 0.25 100 0 ) spectra
  dur 0.2 f+ step
;

\ TWO-TAB
\
\ interpolate between two waveforms (this could be extended to
\ implement all the various wavetable-based synthesis techniques).
instrument: two-tab <{ start dur freq amp
     :optional
     part1     #( 1.0 1.0 2.0 0.5 )
     part2     #( 1.0 0.0 3.0 1.0 ) 
     ampenv    #( 0 0 50 1 100 0 )
     interpenv #( 0 1 100 0 )
     vibamp    0.005
     vibfrq    5.0
     degr      0.0
     dist      1.0
     rev-amt   0.005 -- }>
  :frequency freq :wave part1 #f #f partials->wave make-table-lookup { s1 }
  :frequency freq :wave part2 #f #f partials->wave make-table-lookup { s2 }
  :envelope ampenv :scaler amp :duration dur       make-env { ampf }
  :envelope interpenv :duration dur                make-env { interpf }
  freq hz->radians vibamp f* { vamp }
  :frequency vibfrq :amplitude vamp                make-triangle-wave { pervib }
  :frequency vibfrq 1.0 f+ :amplitude vamp         make-rand-interp { ranvib }
  start dur #{ :degree degr :distance dist :reverb-amount rev-amt } run-instrument
    pervib 0.0 triangle-wave  ranvib 0.0 rand-interp  f+ { vib }
    interpf env { intrp }
    s1 vib table-lookup intrp f* s2 vib table-lookup 1.0 intrp f- f* f+ ampf env f*
  end-run
;instrument

: two-tab-test <{ :optional start 0.0 dur 1.0 -- }>
  start now!
  now@ dur 440 0.5 two-tab
  dur 0.2 f+ step
;

\ LBJ-PIANO
#( #( 1.97 0.0326 2.99 0.0086 3.95 0.0163 4.97 0.0178 5.98 0.0177
      6.95 0.0315 8.02 0.0001 8.94 0.0076  9.96 0.0134 10.99 0.0284
      11.98 0.0229 13.02 0.0229 13.89 0.0010 15.06 0.0090 16.00 0.0003
      17.08 0.0078 18.16 0.0064 19.18 0.0129 20.21 0.0085 21.27 0.0225
      22.32 0.0061 23.41 0.0102 24.48 0.0005 25.56 0.0016 26.64 0.0018
      27.70 0.0113 28.80 0.0111 29.91 0.0158 31.06 0.0093 32.17 0.0017
      33.32 0.0002 34.42 0.0018 35.59 0.0027 36.74 0.0055 37.90 0.0037
      39.06 0.0064 40.25 0.0033 41.47 0.0014 42.53 0.0004 43.89 0.0010
      45.12 0.0039 46.33 0.0039 47.64 0.0009 48.88 0.0016 50.13 0.0006
      51.37 0.0010 52.70 0.0002 54.00 0.0004 55.30 0.0008 56.60 0.0025
      57.96 0.0010 59.30 0.0012 60.67 0.0011 61.99 0.0003 62.86 0.0001
      64.36 0.0005 64.86 0.0001 66.26 0.0004 67.70 0.0006 68.94 0.0002
      70.10 0.0001 70.58 0.0002 72.01 0.0007 73.53 0.0006 75.00 0.0002
      77.03 0.0005 78.00 0.0002 79.57 0.0006 81.16 0.0005 82.70 0.0005
      84.22 0.0003 85.41 0.0002 87.46 0.0001 90.30 0.0001 94.02 0.0001
      95.26 0.0002 109.39 0.0003 )
   #( 1.98 0.0194 2.99 0.0210 3.97 0.0276 4.96 0.0297 5.96 0.0158
      6.99 0.0207 8.01 0.0009 9.00 0.0101 10.00 0.0297 11.01 0.0289
      12.02 0.0211 13.04 0.0127 14.07 0.0061 15.08 0.0174 16.13 0.0009
      17.12 0.0093 18.16 0.0117 19.21 0.0122 20.29 0.0108 21.30 0.0077
      22.38 0.0132 23.46 0.0073 24.14 0.0002 25.58 0.0026 26.69 0.0035
      27.77 0.0053 28.88 0.0024 30.08 0.0027 31.13 0.0075 32.24 0.0027
      33.36 0.0004 34.42 0.0004 35.64 0.0019 36.78 0.0037 38.10 0.0009
      39.11 0.0027 40.32 0.0010 41.51 0.0013 42.66 0.0019 43.87 0.0007
      45.13 0.0017 46.35 0.0019 47.65 0.0021 48.89 0.0014 50.18 0.0023
      51.42 0.0015 52.73 0.0002 54.00 0.0005 55.34 0.0006 56.60 0.0010
      57.96 0.0016 58.86 0.0005 59.30 0.0004 60.75 0.0005 62.22 0.0003
      63.55 0.0005 64.82 0.0003 66.24 0.0003 67.63 0.0011 69.09 0.0007
      70.52 0.0004 72.00 0.0005 73.50 0.0008 74.95 0.0003 77.13 0.0013
      78.02 0.0002 79.48 0.0004 82.59 0.0004 84.10 0.0003 )
   #( 2.00 0.0313 2.99 0.0109 4.00 0.0215 5.00 0.0242 5.98 0.0355
      7.01 0.0132 8.01 0.0009 9.01 0.0071 10.00 0.0258 11.03 0.0221
      12.02 0.0056 13.06 0.0196 14.05 0.0160 15.11 0.0107 16.11 0.0003
      17.14 0.0111 18.21 0.0085 19.23 0.0010 20.28 0.0048 21.31 0.0128
      22.36 0.0051 23.41 0.0041 24.05 0.0006 25.54 0.0019 26.62 0.0028
      27.72 0.0034 28.82 0.0062 29.89 0.0039 30.98 0.0058 32.08 0.0011
      33.21 0.0002 34.37 0.0008 35.46 0.0018 36.62 0.0036 37.77 0.0018
      38.92 0.0042 40.07 0.0037 41.23 0.0011 42.67 0.0003 43.65 0.0018
      44.68 0.0025 45.99 0.0044 47.21 0.0051 48.40 0.0044 49.67 0.0005
      50.88 0.0019 52.15 0.0003 53.42 0.0008 54.69 0.0010 55.98 0.0005
      57.26 0.0013 58.53 0.0027 59.83 0.0011 61.21 0.0027 62.54 0.0003
      63.78 0.0003 65.20 0.0001 66.60 0.0006 67.98 0.0008 69.37 0.0019
      70.73 0.0007 72.14 0.0004 73.62 0.0002 74.40 0.0003 76.52 0.0006
      77.97 0.0002 79.49 0.0004 80.77 0.0003 81.00 0.0001 82.47 0.0005
      83.97 0.0001 87.27 0.0002 )
   #( 2.00 0.0257 2.99 0.0142 3.97 0.0202 4.95 0.0148 5.95 0.0420
      6.95 0.0037 7.94 0.0004 8.94 0.0172 9.95 0.0191 10.96 0.0115
      11.97 0.0059 12.98 0.0140 14.00 0.0178 15.03 0.0121 16.09 0.0002
      17.07 0.0066 18.08 0.0033 19.15 0.0022 20.18 0.0057 21.22 0.0077
      22.29 0.0037 23.33 0.0066 24.97 0.0002 25.49 0.0019 26.55 0.0042
      27.61 0.0043 28.73 0.0038 29.81 0.0084 30.91 0.0040 32.03 0.0025
      33.14 0.0005 34.26 0.0003 35.38 0.0019 36.56 0.0037 37.68 0.0049
      38.86 0.0036 40.11 0.0011 41.28 0.0008 42.50 0.0004 43.60 0.0002
      44.74 0.0022 45.99 0.0050 47.20 0.0009 48.40 0.0036 49.68 0.0004
      50.92 0.0009 52.17 0.0005 53.46 0.0007 54.76 0.0006 56.06 0.0005
      57.34 0.0011 58.67 0.0005 59.95 0.0015 61.37 0.0008 62.72 0.0004
      65.42 0.0009 66.96 0.0003 68.18 0.0003 69.78 0.0003 71.21 0.0004
      72.45 0.0002 74.22 0.0003 75.44 0.0001 76.53 0.0003 78.31 0.0004
      79.83 0.0003 80.16 0.0001 81.33 0.0003 82.44 0.0001 83.17 0.0002
      84.81 0.0003 85.97 0.0003 89.08 0.0001 90.70 0.0002 92.30 0.0002
      95.59 0.0002 97.22 0.0003 98.86 0.0001 108.37 0.0001 125.54 0.0001 )
   #( 1.99 0.0650 3.03 0.0040 4.03 0.0059 5.02 0.0090 5.97 0.0227
      6.98 0.0050 8.04 0.0020 9.00 0.0082 9.96 0.0078 11.01 0.0056
      12.01 0.0095 13.02 0.0050 14.04 0.0093 15.08 0.0064 16.14 0.0017
      17.06 0.0020 18.10 0.0025 19.14 0.0023 20.18 0.0015 21.24 0.0032
      22.29 0.0029 23.32 0.0014 24.37 0.0005 25.43 0.0030 26.50 0.0022
      27.60 0.0027 28.64 0.0024 29.76 0.0035 30.81 0.0136 31.96 0.0025
      33.02 0.0003 34.13 0.0005 35.25 0.0007 36.40 0.0014 37.51 0.0020
      38.64 0.0012 39.80 0.0019 40.97 0.0004 42.09 0.0003 43.24 0.0003
      44.48 0.0002 45.65 0.0024 46.86 0.0005 48.07 0.0013 49.27 0.0008
      50.49 0.0006 52.95 0.0001 54.23 0.0005 55.45 0.0004 56.73 0.0001
      58.03 0.0003 59.29 0.0002 60.59 0.0003 62.04 0.0002 65.89 0.0002
      67.23 0.0002 68.61 0.0002 69.97 0.0004 71.36 0.0005 85.42 0.0001 )
   #( 1.98 0.0256 2.96 0.0158 3.95 0.0310 4.94 0.0411 5.95 0.0238
      6.94 0.0152 7.93 0.0011 8.95 0.0185 9.92 0.0166 10.93 0.0306
      11.94 0.0258 12.96 0.0202 13.97 0.0403 14.95 0.0228 15.93 0.0005
      17.01 0.0072 18.02 0.0034 19.06 0.0028 20.08 0.0124 21.13 0.0137
      22.16 0.0102 23.19 0.0058 23.90 0.0013 25.30 0.0039 26.36 0.0039
      27.41 0.0025 28.47 0.0071 29.64 0.0031 30.60 0.0027 31.71 0.0021
      32.84 0.0003 33.82 0.0002 35.07 0.0019 36.09 0.0054 37.20 0.0038
      38.33 0.0024 39.47 0.0055 40.55 0.0016 41.77 0.0006 42.95 0.0002
      43.27 0.0018 44.03 0.0006 45.25 0.0019 46.36 0.0033 47.50 0.0024
      48.87 0.0012 50.03 0.0016 51.09 0.0004 53.52 0.0017 54.74 0.0012
      56.17 0.0003 57.40 0.0011 58.42 0.0020 59.70 0.0007 61.29 0.0008
      62.56 0.0003 63.48 0.0002 64.83 0.0002 66.12 0.0012 67.46 0.0017
      68.81 0.0003 69.13 0.0003 70.53 0.0002 71.84 0.0001 73.28 0.0002
      75.52 0.0010 76.96 0.0005 77.93 0.0003 78.32 0.0003 79.73 0.0003
      81.69 0.0002 82.52 0.0001 84.01 0.0001 84.61 0.0002 86.88 0.0001
      88.36 0.0002 89.85 0.0002 91.35 0.0003 92.86 0.0002 93.40 0.0001
      105.28 0.0002 106.22 0.0002 107.45 0.0001 108.70 0.0003 122.08 0.0002 )
   #( 1.97 0.0264 2.97 0.0211 3.98 0.0234 4.98 0.0307 5.96 0.0085
      6.94 0.0140 7.93 0.0005 8.96 0.0112 9.96 0.0209 10.98 0.0194
      11.98 0.0154 12.99 0.0274 13.99 0.0127 15.01 0.0101 15.99 0.0002
      17.04 0.0011 18.08 0.0032 19.14 0.0028 20.12 0.0054 21.20 0.0053
      22.13 0.0028 23.22 0.0030 24.32 0.0006 25.24 0.0004 26.43 0.0028
      27.53 0.0048 28.52 0.0039 29.54 0.0047 30.73 0.0044 31.82 0.0007
      32.94 0.0008 34.04 0.0012 35.13 0.0018 36.29 0.0007 37.35 0.0075
      38.51 0.0045 39.66 0.0014 40.90 0.0004 41.90 0.0002 43.08 0.0002
      44.24 0.0017 45.36 0.0013 46.68 0.0020 47.79 0.0015 48.98 0.0010
      50.21 0.0012 51.34 0.0001 53.82 0.0003 55.09 0.0004 56.23 0.0005
      57.53 0.0004 58.79 0.0005 59.30 0.0002 60.03 0.0002 61.40 0.0003
      62.84 0.0001 66.64 0.0001 67.97 0.0001 69.33 0.0001 70.68 0.0001
      73.57 0.0002 75.76 0.0002 76.45 0.0001 79.27 0.0001 80.44 0.0002
      81.87 0.0002 )
   #( 2.00 0.0311 2.99 0.0086 3.99 0.0266 4.97 0.0123 5.98 0.0235
      6.97 0.0161 7.97 0.0008 8.96 0.0088 9.96 0.0621 10.99 0.0080
      11.99 0.0034 12.99 0.0300 14.03 0.0228 15.04 0.0105 16.03 0.0004
      17.06 0.0036 18.09 0.0094 18.95 0.0009 20.17 0.0071 21.21 0.0161
      22.25 0.0106 23.28 0.0104 24.33 0.0008 25.38 0.0030 26.46 0.0035
      27.50 0.0026 28.59 0.0028 29.66 0.0128 30.75 0.0139 31.81 0.0038
      32.93 0.0006 34.04 0.0004 35.16 0.0005 36.25 0.0023 37.35 0.0012
      38.46 0.0021 39.59 0.0035 40.71 0.0006 41.86 0.0007 42.42 0.0001
      43.46 0.0003 44.17 0.0032 45.29 0.0013 46.57 0.0004 47.72 0.0011
      48.79 0.0005 50.11 0.0005 51.29 0.0003 52.47 0.0002 53.68 0.0004
      55.02 0.0005 56.18 0.0003 57.41 0.0003 58.75 0.0007 59.33 0.0009
      60.00 0.0004 61.34 0.0001 64.97 0.0003 65.20 0.0002 66.48 0.0002
      67.83 0.0002 68.90 0.0003 70.25 0.0003 71.59 0.0002 73.68 0.0001
      75.92 0.0001 77.08 0.0002 78.45 0.0002 81.56 0.0002 82.99 0.0001
      88.39 0.0001 )
   #( 0.97 0.0059 1.98 0.0212 2.99 0.0153 3.99 0.0227 4.96 0.0215
      5.97 0.0153 6.98 0.0085 7.98 0.0007 8.97 0.0179 9.98 0.0512
      10.98 0.0322 12.00 0.0098 13.02 0.0186 14.00 0.0099 15.05 0.0109
      15.88 0.0011 17.07 0.0076 18.11 0.0071 19.12 0.0045 20.16 0.0038
      21.23 0.0213 22.27 0.0332 23.34 0.0082 24.34 0.0014 25.42 0.0024
      26.47 0.0012 27.54 0.0014 28.60 0.0024 29.72 0.0026 30.10 0.0008
      31.91 0.0021 32.13 0.0011 33.02 0.0007 34.09 0.0014 35.17 0.0007
      36.27 0.0024 37.39 0.0029 38.58 0.0014 39.65 0.0017 40.95 0.0012
      41.97 0.0004 42.43 0.0002 43.49 0.0001 44.31 0.0012 45.42 0.0031
      46.62 0.0017 47.82 0.0013 49.14 0.0013 50.18 0.0010 51.54 0.0003
      53.90 0.0006 55.06 0.0010 56.31 0.0003 57.63 0.0001 59.02 0.0003
      60.09 0.0004 60.35 0.0004 61.62 0.0009 63.97 0.0001 65.19 0.0001
      65.54 0.0002 66.92 0.0002 67.94 0.0002 69.17 0.0003 69.60 0.0004
      70.88 0.0002 72.24 0.0002 76.12 0.0001 78.94 0.0001 81.75 0.0001
      82.06 0.0001 83.53 0.0001 90.29 0.0002 91.75 0.0001 92.09 0.0002
      93.28 0.0001 97.07 0.0001 )
   #( 1.98 0.0159 2.98 0.1008 3.98 0.0365 4.98 0.0133 5.97 0.0101
      6.97 0.0115 7.97 0.0007 8.99 0.0349 10.01 0.0342 11.01 0.0236
      12.00 0.0041 13.02 0.0114 14.05 0.0137 15.06 0.0100 16.05 0.0007
      17.04 0.0009 18.12 0.0077 19.15 0.0023 20.12 0.0017 21.24 0.0113
      22.26 0.0126 23.30 0.0093 24.36 0.0007 25.43 0.0007 26.47 0.0009
      27.55 0.0013 28.59 0.0025 29.61 0.0010 30.77 0.0021 31.86 0.0023
      32.96 0.0003 34.03 0.0007 35.06 0.0005 36.20 0.0006 37.34 0.0006
      38.36 0.0009 39.60 0.0016 40.69 0.0005 41.77 0.0002 42.92 0.0002
      44.02 0.0003 45.24 0.0006 46.33 0.0004 47.50 0.0007 48.71 0.0007
      49.87 0.0002 51.27 0.0002 53.42 0.0003 55.88 0.0003 57.10 0.0004
      58.34 0.0002 59.86 0.0003 61.13 0.0003 67.18 0.0001 68.50 0.0001
      71.17 0.0001 83.91 0.0001 90.55 0.0001 )
   #( 0.98 0.0099 2.00 0.0181 2.99 0.0353 3.98 0.0285 4.97 0.0514
      5.96 0.0402 6.96 0.0015 7.98 0.0012 8.98 0.0175 9.98 0.0264
      10.98 0.0392 11.98 0.0236 13.00 0.0153 14.04 0.0049 15.00 0.0089
      16.01 0.0001 17.03 0.0106 18.03 0.0028 19.05 0.0024 20.08 0.0040
      21.11 0.0103 22.12 0.0104 23.20 0.0017 24.19 0.0008 25.20 0.0007
      26.24 0.0011 27.36 0.0009 27.97 0.0030 29.40 0.0044 30.37 0.0019
      31.59 0.0017 32.65 0.0008 33.59 0.0005 34.79 0.0009 35.75 0.0027
      36.88 0.0035 37.93 0.0039 39.00 0.0031 40.08 0.0025 41.16 0.0010
      43.25 0.0004 44.52 0.0012 45.62 0.0023 45.85 0.0012 47.00 0.0006
      47.87 0.0008 48.99 0.0003 50.48 0.0003 51.62 0.0001 52.43 0.0001
      53.56 0.0002 54.76 0.0002 56.04 0.0002 56.68 0.0006 57.10 0.0003
      58.28 0.0005 59.47 0.0003 59.96 0.0002 60.67 0.0001 63.08 0.0002
      64.29 0.0002 66.72 0.0001 67.97 0.0001 68.65 0.0001 70.43 0.0001
      79.38 0.0001 80.39 0.0001 82.39 0.0001 )
   #( 1.00 0.0765 1.99 0.0151 2.99 0.0500 3.99 0.0197 5.00 0.0260
      6.00 0.0145 6.98 0.0128 7.97 0.0004 8.98 0.0158 9.99 0.0265
      11.02 0.0290 12.02 0.0053 13.03 0.0242 14.03 0.0103 15.06 0.0054
      16.04 0.0006 17.08 0.0008 18.10 0.0058 19.16 0.0011 20.16 0.0055
      21.18 0.0040 22.20 0.0019 23.22 0.0014 24.05 0.0005 25.31 0.0019
      26.38 0.0018 27.44 0.0022 28.45 0.0024 29.57 0.0073 30.58 0.0032
      31.66 0.0071 32.73 0.0015 33.85 0.0005 34.96 0.0003 36.00 0.0020
      37.11 0.0018 38.18 0.0055 39.23 0.0006 40.33 0.0004 41.52 0.0003
      43.41 0.0028 45.05 0.0003 45.99 0.0002 47.07 0.0003 48.52 0.0002
      49.48 0.0003 50.63 0.0003 51.81 0.0002 54.05 0.0002 55.24 0.0001
      56.62 0.0001 57.81 0.0004 59.16 0.0013 60.23 0.0003 66.44 0.0001
      68.99 0.0004 75.49 0.0001 87.56 0.0004 )
   #( 0.98 0.0629 1.99 0.0232 2.98 0.0217 4.00 0.0396 4.98 0.0171
      5.97 0.0098 6.99 0.0167 7.99 0.0003 8.98 0.0192 9.98 0.0266
      10.99 0.0256 12.01 0.0061 13.02 0.0135 14.02 0.0062 15.05 0.0158
      16.06 0.0018 17.08 0.0101 18.09 0.0053 19.11 0.0074 20.13 0.0020
      21.17 0.0052 22.22 0.0077 23.24 0.0035 24.00 0.0009 25.32 0.0016
      26.40 0.0022 27.43 0.0005 28.55 0.0026 29.60 0.0026 30.65 0.0010
      31.67 0.0019 32.77 0.0008 33.81 0.0003 34.91 0.0003 36.01 0.0005
      37.11 0.0010 38.20 0.0014 39.29 0.0039 40.43 0.0012 41.50 0.0006
      43.38 0.0017 43.75 0.0002 44.94 0.0005 46.13 0.0002 47.11 0.0003
      48.28 0.0005 48.42 0.0005 49.44 0.0003 50.76 0.0004 51.93 0.0002
      54.15 0.0003 55.31 0.0005 55.50 0.0003 56.98 0.0003 57.90 0.0004
      60.33 0.0002 61.39 0.0001 61.59 0.0001 65.09 0.0002 66.34 0.0001
      68.85 0.0001 70.42 0.0002 71.72 0.0001 73.05 0.0003 79.65 0.0001
      85.28 0.0002 93.52 0.0001 )
   #( 1.02 0.0185 1.99 0.0525 2.98 0.0613 3.99 0.0415 4.98 0.0109
      5.97 0.0248 6.99 0.0102 7.98 0.0005 8.98 0.0124 9.99 0.0103
      10.99 0.0124 12.00 0.0016 13.01 0.0029 14.03 0.0211 15.04 0.0128
      16.07 0.0021 17.09 0.0009 18.09 0.0043 19.14 0.0022 20.13 0.0016
      21.20 0.0045 22.21 0.0088 23.26 0.0046 24.29 0.0013 25.35 0.0009
      26.39 0.0028 27.49 0.0009 28.51 0.0006 29.58 0.0012 30.70 0.0010
      31.74 0.0019 32.75 0.0002 33.85 0.0001 34.95 0.0005 36.02 0.0003
      37.16 0.0009 38.25 0.0018 39.35 0.0008 40.54 0.0004 41.61 0.0002
      43.40 0.0004 43.74 0.0003 45.05 0.0001 46.11 0.0003 47.40 0.0002
      48.36 0.0004 49.55 0.0004 50.72 0.0002 52.00 0.0001 55.58 0.0002
      57.02 0.0001 57.98 0.0002 59.13 0.0003 61.56 0.0001 66.56 0.0001
      87.65 0.0002 )
   #( 1.00 0.0473 1.99 0.0506 2.99 0.0982 3.99 0.0654 5.00 0.0196
      5.99 0.0094 6.99 0.0118 7.93 0.0001 8.99 0.0057 10.01 0.0285
      11.01 0.0142 12.03 0.0032 13.03 0.0056 14.06 0.0064 15.06 0.0059
      16.11 0.0005 17.09 0.0033 18.14 0.0027 19.15 0.0014 20.17 0.0010
      21.21 0.0059 22.26 0.0043 23.31 0.0031 24.31 0.0018 25.33 0.0009
      26.41 0.0005 27.47 0.0015 28.53 0.0015 29.58 0.0041 30.65 0.0025
      31.73 0.0011 32.83 0.0010 34.98 0.0003 36.07 0.0009 37.23 0.0001
      38.26 0.0020 39.41 0.0014 40.53 0.0005 41.40 0.0003 42.80 0.0002
      43.48 0.0028 43.93 0.0001 45.03 0.0003 46.18 0.0007 47.41 0.0001
      48.57 0.0002 49.67 0.0001 50.83 0.0002 54.39 0.0001 55.58 0.0002
      57.97 0.0005 58.11 0.0002 59.21 0.0001 60.42 0.0002 61.66 0.0001 )
   #( 1.00 0.0503 2.00 0.0963 2.99 0.1304 3.99 0.0218 4.98 0.0041
      5.98 0.0292 6.98 0.0482 7.99 0.0005 8.99 0.0280 10.00 0.0237
      11.00 0.0152 12.02 0.0036 12.95 0.0022 14.06 0.0111 15.07 0.0196
      16.08 0.0016 17.11 0.0044 18.13 0.0073 19.17 0.0055 20.19 0.0028
      21.20 0.0012 22.27 0.0068 23.30 0.0036 24.35 0.0012 25.35 0.0002
      26.46 0.0005 27.47 0.0005 28.59 0.0009 29.65 0.0021 30.70 0.0020
      31.78 0.0012 32.89 0.0010 35.06 0.0005 36.16 0.0008 37.27 0.0010
      38.36 0.0010 39.47 0.0014 40.58 0.0004 41.43 0.0007 41.82 0.0003
      43.48 0.0008 44.53 0.0001 45.25 0.0003 46.43 0.0002 47.46 0.0002
      48.76 0.0005 49.95 0.0004 50.96 0.0002 51.12 0.0002 52.33 0.0001
      54.75 0.0001 55.75 0.0002 56.90 0.0002 58.17 0.0002 59.40 0.0004
      60.62 0.0002 65.65 0.0001 66.91 0.0002 69.91 0.0001 71.25 0.0002 )
   #( 1.00 0.1243 1.98 0.1611 3.00 0.0698 3.98 0.0390 5.00 0.0138
      5.99 0.0154 7.01 0.0287 8.01 0.0014 9.01 0.0049 10.00 0.0144
      11.01 0.0055 12.05 0.0052 13.01 0.0011 14.05 0.0118 15.07 0.0154
      16.12 0.0028 17.14 0.0061 18.25 0.0007 19.22 0.0020 20.24 0.0011
      21.27 0.0029 22.30 0.0046 23.34 0.0049 24.35 0.0004 25.45 0.0003
      26.47 0.0007 27.59 0.0008 28.16 0.0009 29.12 0.0002 29.81 0.0006
      30.81 0.0009 31.95 0.0004 33.00 0.0011 34.12 0.0005 35.18 0.0003
      36.30 0.0008 37.38 0.0003 38.55 0.0003 39.64 0.0006 40.77 0.0007
      41.52 0.0006 41.89 0.0006 43.04 0.0011 43.60 0.0009 44.31 0.0002
      45.68 0.0002 46.56 0.0003 47.60 0.0001 48.83 0.0006 50.01 0.0003
      51.27 0.0003 56.04 0.0005 57.21 0.0003 58.56 0.0004 59.83 0.0003
      61.05 0.0001 62.20 0.0001 67.37 0.0002 76.53 0.0001 )
   #( 0.99 0.0222 1.99 0.0678 2.99 0.0683 4.00 0.0191 5.00 0.0119
      6.01 0.0232 6.98 0.0336 7.99 0.0082 9.01 0.0201 10.01 0.0189
      11.01 0.0041 12.01 0.0053 13.05 0.0154 14.04 0.0159 15.06 0.0092
      16.11 0.0038 17.12 0.0014 18.15 0.0091 19.16 0.0006 20.30 0.0012
      21.25 0.0061 22.28 0.0099 23.34 0.0028 24.38 0.0012 25.43 0.0016
      26.49 0.0048 27.55 0.0025 28.62 0.0015 29.71 0.0032 30.78 0.0077
      31.88 0.0011 32.97 0.0007 34.08 0.0006 35.16 0.0008 36.28 0.0004
      37.41 0.0006 38.54 0.0005 39.62 0.0002 40.80 0.0003 41.93 0.0001
      43.06 0.0002 44.21 0.0003 45.38 0.0002 46.54 0.0007 47.78 0.0003
      48.95 0.0004 50.10 0.0003 51.37 0.0002 53.79 0.0003 56.20 0.0001
      58.71 0.0002 66.47 0.0003 )
   #( 1.01 0.0241 1.99 0.1011 2.98 0.0938 3.98 0.0081 4.99 0.0062
      5.99 0.0291 6.99 0.0676 7.59 0.0004 8.98 0.0127 9.99 0.0112
      10.99 0.0142 12.00 0.0029 13.02 0.0071 14.02 0.0184 15.03 0.0064
      16.07 0.0010 17.09 0.0011 18.11 0.0010 19.15 0.0060 20.19 0.0019
      21.24 0.0025 22.29 0.0013 23.31 0.0050 25.41 0.0030 26.50 0.0018
      27.53 0.0006 28.63 0.0012 29.66 0.0013 30.77 0.0020 31.84 0.0006
      34.04 0.0001 35.14 0.0001 36.32 0.0004 37.41 0.0007 38.53 0.0007
      39.67 0.0009 40.85 0.0003 45.49 0.0002 46.65 0.0001 47.81 0.0004
      49.01 0.0002 53.91 0.0002 55.14 0.0002 57.69 0.0002 )
   #( 1.00 0.0326 2.00 0.1066 2.99 0.1015 4.00 0.0210 4.97 0.0170
      5.99 0.0813 6.98 0.0820 7.96 0.0011 8.99 0.0248 10.03 0.0107
      11.01 0.0126 12.01 0.0027 13.01 0.0233 14.04 0.0151 15.05 0.0071
      16.04 0.0002 17.10 0.0061 18.12 0.0059 19.15 0.0087 20.23 0.0005
      21.25 0.0040 22.30 0.0032 23.35 0.0004 24.40 0.0001 25.45 0.0030
      26.54 0.0022 27.60 0.0003 28.70 0.0009 29.80 0.0029 30.85 0.0006
      31.97 0.0006 34.19 0.0004 35.30 0.0003 36.43 0.0007 37.56 0.0005
      38.68 0.0019 39.88 0.0013 41.00 0.0003 43.35 0.0003 44.51 0.0002
      45.68 0.0006 46.93 0.0010 48.11 0.0006 49.29 0.0003 55.58 0.0002 )
   #( 0.98 0.0113 1.99 0.0967 3.00 0.0719 3.98 0.0345 4.98 0.0121
      6.00 0.0621 7.00 0.0137 7.98 0.0006 9.01 0.0314 10.01 0.0171
      11.02 0.0060 12.03 0.0024 13.05 0.0077 14.07 0.0040 15.12 0.0032
      16.13 0.0004 17.15 0.0011 18.20 0.0028 19.18 0.0003 20.26 0.0003
      21.31 0.0025 22.35 0.0021 23.39 0.0005 25.55 0.0002 26.62 0.0014
      27.70 0.0003 28.78 0.0005 29.90 0.0030 31.01 0.0011 32.12 0.0005
      34.31 0.0001 35.50 0.0002 36.62 0.0002 37.76 0.0005 38.85 0.0002
      40.09 0.0004 43.60 0.0001 44.73 0.0002 46.02 0.0002 47.25 0.0004
      48.44 0.0004 )
   #( 0.99 0.0156 1.98 0.0846 2.98 0.0178 3.98 0.0367 4.98 0.0448
      5.98 0.0113 6.99 0.0189 8.00 0.0011 9.01 0.0247 10.02 0.0089
      11.01 0.0184 12.03 0.0105 13.00 0.0039 14.07 0.0116 15.09 0.0078
      16.13 0.0008 17.14 0.0064 18.19 0.0029 19.22 0.0028 20.25 0.0017
      21.32 0.0043 22.37 0.0055 23.42 0.0034 24.48 0.0004 25.54 0.0002
      26.61 0.0017 27.70 0.0011 28.80 0.0002 29.89 0.0019 30.97 0.0028
      32.09 0.0007 34.30 0.0002 35.44 0.0003 36.55 0.0001 37.69 0.0004
      38.93 0.0002 40.05 0.0005 41.20 0.0005 42.37 0.0002 43.54 0.0003
      44.73 0.0001 45.95 0.0002 47.16 0.0001 48.43 0.0005 49.65 0.0004
      55.90 0.0002 59.81 0.0004 )
   #( 1.01 0.0280 2.00 0.0708 2.99 0.0182 3.99 0.0248 4.98 0.0245
      5.98 0.0279 6.98 0.0437 7.99 0.0065 8.99 0.0299 10.00 0.0073
      10.99 0.0011 12.03 0.0122 13.03 0.0028 14.08 0.0044 15.11 0.0097
      16.15 0.0010 17.17 0.0025 18.19 0.0017 19.24 0.0008 20.28 0.0040
      21.32 0.0024 22.38 0.0008 23.46 0.0032 24.52 0.0010 25.59 0.0008
      26.68 0.0009 27.76 0.0012 28.88 0.0003 29.95 0.0005 31.05 0.0017
      32.14 0.0002 33.29 0.0003 37.88 0.0002 39.03 0.0002 40.19 0.0004
      41.37 0.0003 43.74 0.0002 46.20 0.0001 48.68 0.0001 49.93 0.0001
      51.19 0.0002 )
   #( 1.00 0.0225 1.99 0.0921 2.98 0.0933 3.99 0.0365 4.99 0.0100
      5.98 0.0213 6.98 0.0049 7.98 0.0041 8.98 0.0090 9.99 0.0068
      11.01 0.0040 12.03 0.0086 13.02 0.0015 14.04 0.0071 15.09 0.0082
      16.14 0.0011 17.15 0.0014 18.18 0.0010 19.26 0.0013 20.26 0.0005
      21.33 0.0006 22.36 0.0011 23.46 0.0016 24.52 0.0004 25.59 0.0002
      26.70 0.0006 27.78 0.0007 28.87 0.0002 30.03 0.0008 31.14 0.0010
      32.24 0.0006 33.37 0.0002 35.67 0.0003 37.99 0.0004 39.17 0.0004
      40.35 0.0005 41.53 0.0001 46.42 0.0001 )
   #( 1.00 0.0465 1.99 0.0976 2.98 0.0678 4.00 0.0727 4.99 0.0305
      5.98 0.0210 6.98 0.0227 8.00 0.0085 9.01 0.0183 10.02 0.0258
      11.05 0.0003 12.06 0.0061 13.05 0.0021 14.10 0.0089 15.12 0.0077
      16.16 0.0016 17.21 0.0061 18.23 0.0011 19.29 0.0031 20.36 0.0031
      21.41 0.0007 22.48 0.0013 23.55 0.0020 24.64 0.0004 25.74 0.0005
      26.81 0.0006 27.95 0.0006 29.03 0.0001 30.22 0.0010 31.30 0.0004
      32.48 0.0001 33.60 0.0002 38.30 0.0003 )
   #( 1.00 0.0674 1.99 0.0841 2.98 0.0920 3.99 0.0328 4.99 0.0368
      5.98 0.0206 6.99 0.0246 8.01 0.0048 9.01 0.0218 10.03 0.0155
      11.05 0.0048 12.06 0.0077 13.00 0.0020 14.10 0.0083 15.15 0.0084
      16.18 0.0015 17.22 0.0039 18.27 0.0032 19.34 0.0026 20.40 0.0012
      21.47 0.0009 22.54 0.0008 23.62 0.0016 24.71 0.0005 25.82 0.0004
      26.91 0.0002 28.03 0.0008 29.17 0.0002 30.32 0.0028 31.45 0.0004
      32.61 0.0005 33.77 0.0001 36.14 0.0003 37.32 0.0002 38.54 0.0005
      39.75 0.0002 42.23 0.0002 48.65 0.0001 )
   #( 1.01 0.0423 1.99 0.0240 2.98 0.0517 4.00 0.0493 5.00 0.0324
      6.00 0.0094 6.99 0.0449 7.99 0.0050 9.00 0.0197 10.03 0.0132
      11.03 0.0009 12.07 0.0017 13.08 0.0023 14.12 0.0094 15.16 0.0071
      16.21 0.0020 17.25 0.0005 18.30 0.0027 19.04 0.0004 20.43 0.0022
      21.51 0.0002 22.59 0.0006 23.72 0.0018 24.80 0.0002 25.88 0.0002
      27.03 0.0002 28.09 0.0006 29.31 0.0002 30.46 0.0004 31.61 0.0007
      32.78 0.0005 33.95 0.0001 36.34 0.0002 37.56 0.0001 38.80 0.0001
      40.02 0.0001 44.14 0.0001 )
   #( 1.00 0.0669 1.99 0.0909 2.99 0.0410 3.98 0.0292 4.98 0.0259
      5.98 0.0148 6.98 0.0319 7.99 0.0076 9.01 0.0056 10.02 0.0206
      11.04 0.0032 12.05 0.0085 13.08 0.0040 14.12 0.0037 15.16 0.0030
      16.20 0.0013 17.24 0.0021 18.30 0.0010 19.36 0.0015 20.44 0.0013
      21.50 0.0009 22.60 0.0015 23.69 0.0014 24.80 0.0006 25.87 0.0002
      27.02 0.0006 28.12 0.0002 29.28 0.0003 30.43 0.0002 31.59 0.0007
      32.79 0.0001 35.14 0.0001 37.57 0.0001 40.03 0.0002 41.28 0.0004
      44.10 0.0001 )
   #( 0.99 0.0421 1.99 0.1541 2.98 0.0596 3.98 0.0309 4.98 0.0301
      5.99 0.0103 7.00 0.0240 8.01 0.0073 9.01 0.0222 10.04 0.0140
      11.05 0.0033 12.08 0.0045 13.13 0.0009 14.13 0.0015 15.21 0.0026
      16.24 0.0003 17.30 0.0004 18.35 0.0010 19.39 0.0003 20.50 0.0015
      21.57 0.0003 22.68 0.0011 23.80 0.0005 24.90 0.0008 26.02 0.0002
      27.16 0.0001 28.30 0.0006 29.48 0.0002 31.81 0.0005 33.00 0.0003
      34.21 0.0001 37.89 0.0001 )
   #( 0.99 0.0389 2.00 0.2095 3.00 0.0835 3.99 0.0289 5.00 0.0578
      5.99 0.0363 7.01 0.0387 8.01 0.0056 9.04 0.0173 10.05 0.0175
      11.08 0.0053 12.10 0.0056 13.15 0.0064 14.19 0.0036 15.22 0.0019
      16.29 0.0010 17.36 0.0017 18.43 0.0018 19.51 0.0004 20.60 0.0011
      21.70 0.0003 22.82 0.0003 23.95 0.0001 25.05 0.0004 26.17 0.0001
      28.50 0.0003 29.68 0.0001 32.07 0.0003 33.28 0.0004 34.52 0.0001 )
   #( 1.00 0.1238 1.99 0.2270 3.00 0.0102 3.99 0.0181 4.98 0.0415
      6.00 0.0165 7.01 0.0314 8.02 0.0148 9.04 0.0203 10.05 0.0088
      11.07 0.0062 12.11 0.0070 13.14 0.0054 14.19 0.0028 15.24 0.0044
      16.30 0.0029 17.38 0.0009 18.45 0.0026 19.56 0.0003 20.65 0.0025
      21.74 0.0014 22.87 0.0013 23.99 0.0007 25.15 0.0002 27.46 0.0004
      28.39 0.0006 28.65 0.0004 29.85 0.0001 31.05 0.0002 32.27 0.0003
      33.52 0.0002 34.76 0.0003 )
   #( 1.00 0.1054 2.00 0.2598 2.99 0.0369 3.98 0.0523 4.99 0.0020
      5.99 0.0051 7.00 0.0268 8.01 0.0027 9.04 0.0029 10.05 0.0081
      11.08 0.0047 12.12 0.0051 13.16 0.0091 14.19 0.0015 15.27 0.0030
      16.34 0.0017 17.42 0.0006 18.51 0.0003 19.61 0.0007 20.72 0.0003
      21.84 0.0001 22.99 0.0010 24.13 0.0001 28.44 0.0001 30.09 0.0001 )
   #( 0.99 0.0919 2.00 0.0418 2.99 0.0498 3.99 0.0135 4.99 0.0026
      6.00 0.0155 7.01 0.0340 8.02 0.0033 9.04 0.0218 10.08 0.0084
      11.11 0.0057 12.15 0.0051 13.21 0.0043 14.25 0.0015 15.31 0.0023
      16.40 0.0008 17.48 0.0004 18.59 0.0016 19.71 0.0010 20.84 0.0018
      21.98 0.0002 23.11 0.0013 24.26 0.0003 26.67 0.0002 29.12 0.0002
      30.37 0.0002 31.62 0.0003 32.92 0.0001 )
   #( 0.99 0.1174 1.99 0.1126 2.99 0.0370 3.99 0.0159 5.01 0.0472
      6.01 0.0091 7.03 0.0211 8.05 0.0015 9.07 0.0098 10.11 0.0038
      11.15 0.0042 12.20 0.0018 13.24 0.0041 14.32 0.0033 15.41 0.0052
      16.49 0.0001 17.61 0.0004 18.71 0.0004 19.84 0.0004 20.99 0.0002
      22.14 0.0006 23.31 0.0006 24.50 0.0004 25.70 0.0002 28.09 0.0002
      28.66 0.0002 32.00 0.0001 )
   #( 1.00 0.1085 2.00 0.1400 2.99 0.0173 3.99 0.0229 5.00 0.0272
      6.02 0.0077 7.03 0.0069 8.04 0.0017 9.08 0.0045 10.10 0.0030
      11.15 0.0040 12.20 0.0007 13.25 0.0019 14.32 0.0008 15.42 0.0024
      16.50 0.0002 17.59 0.0005 18.71 0.0003 19.83 0.0002 20.98 0.0005
      23.29 0.0008 )
   #( 1.00 0.0985 2.00 0.1440 2.99 0.0364 3.99 0.0425 5.00 0.0190
      6.01 0.0089 7.03 0.0278 8.04 0.0006 9.07 0.0083 10.10 0.0021
      11.14 0.0050 12.18 0.0005 13.26 0.0036 14.33 0.0005 15.41 0.0026
      17.62 0.0004 18.75 0.0004 19.89 0.0003 21.04 0.0012 22.21 0.0002
      23.38 0.0004 27.04 0.0001 )
   #( 0.99 0.1273 2.00 0.1311 2.99 0.0120 4.00 0.0099 5.00 0.0235
      6.02 0.0068 7.03 0.0162 8.06 0.0009 9.08 0.0083 10.12 0.0014
      11.17 0.0050 12.24 0.0010 13.29 0.0013 14.39 0.0022 15.48 0.0011
      16.59 0.0002 17.70 0.0003 18.84 0.0010 20.00 0.0003 21.17 0.0003
      23.56 0.0004 28.79 0.0003 )
   #( 1.00 0.1018 2.00 0.1486 3.00 0.0165 4.00 0.0186 5.01 0.0194
      6.02 0.0045 7.04 0.0083 8.06 0.0012 9.10 0.0066 10.15 0.0009
      11.19 0.0008 12.26 0.0011 13.34 0.0028 14.45 0.0006 15.53 0.0009
      16.66 0.0002 17.79 0.0006 18.94 0.0005 20.11 0.0003 21.29 0.0005
      22.49 0.0003 23.73 0.0005 26.22 0.0001 27.52 0.0001 28.88 0.0002 )
   #( 1.00 0.1889 1.99 0.1822 3.00 0.0363 4.00 0.0047 5.01 0.0202
      6.03 0.0053 7.05 0.0114 8.01 0.0002 9.13 0.0048 10.17 0.0010
      11.23 0.0033 12.30 0.0010 13.38 0.0006 14.50 0.0002 15.62 0.0010
      20.27 0.0001 21.47 0.0001 )
   #( 1.00 0.0522 1.99 0.0763 2.99 0.0404 4.00 0.0139 5.01 0.0185
      6.01 0.0021 7.06 0.0045 8.09 0.0002 9.11 0.0003 10.17 0.0006
      11.25 0.0004 12.32 0.0005 13.40 0.0003 14.53 0.0003 15.65 0.0007
      16.80 0.0001 17.95 0.0002 19.14 0.0006 20.34 0.0002 21.56 0.0003 )
   #( 0.99 0.1821 1.99 0.0773 3.00 0.0125 4.01 0.0065 5.01 0.0202
      6.03 0.0071 7.05 0.0090 8.08 0.0006 9.13 0.0008 10.18 0.0013
      11.25 0.0010 12.33 0.0012 13.42 0.0006 14.54 0.0005 15.65 0.0004
      17.97 0.0002 19.15 0.0001 )
   #( 1.00 0.1868 2.00 0.0951 3.00 0.0147 4.01 0.0134 5.02 0.0184
      6.04 0.0132 7.06 0.0011 8.11 0.0008 9.15 0.0010 10.22 0.0012
      11.30 0.0011 12.40 0.0003 13.11 0.0004 13.49 0.0002 14.62 0.0003
      15.77 0.0001 )
   #( 1.00 0.1933 2.00 0.0714 3.00 0.0373 4.00 0.0108 5.02 0.0094
      6.02 0.0010 7.07 0.0022 8.11 0.0002 9.16 0.0065 10.23 0.0015
      11.31 0.0023 12.40 0.0003 13.53 0.0014 14.66 0.0002 15.81 0.0011
      18.20 0.0002 19.41 0.0001 )
   #( 0.99 0.2113 1.99 0.0877 3.00 0.0492 4.01 0.0094 5.02 0.0144
      6.04 0.0103 7.07 0.0117 8.12 0.0006 9.19 0.0019 10.25 0.0007
      11.35 0.0017 12.45 0.0010 13.58 0.0003 14.74 0.0003 15.91 0.0003
      19.57 0.0002 )
   #( 0.99 0.2455 1.99 0.0161 3.00 0.0215 4.01 0.0036 5.03 0.0049
      6.04 0.0012 7.09 0.0036 8.14 0.0011 9.21 0.0009 10.30 0.0001
      11.40 0.0012 12.50 0.0001 13.66 0.0005 14.84 0.0001 )
   #( 1.00 0.1132 2.00 0.0252 3.00 0.0292 4.01 0.0136 5.03 0.0045
      6.06 0.0022 7.11 0.0101 8.17 0.0004 9.23 0.0010 10.33 0.0012
      11.44 0.0013 12.58 0.0011 13.75 0.0002 14.93 0.0005 16.14 0.0002 )
   #( 1.00 0.1655 2.00 0.0445 3.00 0.0120 4.00 0.0038 5.02 0.0015
      6.07 0.0038 7.11 0.0003 8.19 0.0002 9.25 0.0010 10.36 0.0011
      11.48 0.0005 12.63 0.0002 13.79 0.0003 16.24 0.0002 )
   #( 0.99 0.3637 1.99 0.0259 3.01 0.0038 4.01 0.0057 5.03 0.0040
      6.07 0.0067 7.12 0.0014 8.19 0.0004 9.27 0.0003 10.38 0.0002
      12.67 0.0001 )
   #( 1.00 0.1193 2.00 0.0230 3.00 0.0104 4.01 0.0084 5.04 0.0047
      6.08 0.0035 7.13 0.0041 8.20 0.0002 9.29 0.0005 10.40 0.0005
      11.53 0.0003 12.70 0.0002 13.91 0.0002 )
   #( 1.00 0.0752 2.00 0.0497 3.00 0.0074 4.02 0.0076 5.05 0.0053
      6.09 0.0043 7.15 0.0024 8.22 0.0001 9.32 0.0006 10.45 0.0002
      11.58 0.0001 12.78 0.0001 15.22 0.0001 )
   #( 1.00 0.2388 2.00 0.0629 3.01 0.0159 4.04 0.0063 5.07 0.0051
      6.12 0.0045 7.19 0.0026 8.29 0.0015 9.43 0.0001 11.75 0.0002 )
   #( 1.00 0.1919 2.01 0.0116 3.01 0.0031 4.03 0.0090 5.07 0.0061
      6.13 0.0036 7.19 0.0013 8.30 0.0016 9.13 0.0001 10.59 0.0002
      11.78 0.0002 )
   #( 1.00 0.1296 2.00 0.0135 3.01 0.0041 4.04 0.0045 5.09 0.0028
      6.14 0.0046 7.23 0.0007 8.32 0.0007 9.50 0.0001 )
   #( 1.00 0.0692 2.00 0.0209 3.02 0.0025 4.05 0.0030 5.09 0.0047
      6.17 0.0022 7.25 0.0015 8.36 0.0015 9.53 0.0010 10.69 0.0001
      13.40 0.0001 )
   #( 1.00 0.1715 2.00 0.0142 3.01 0.0024 4.03 0.0015 5.07 0.0017
      6.13 0.0018 7.22 0.0009 8.33 0.0014 9.51 0.0007 10.69 0.0002 )
   #( 1.00 0.1555 2.01 0.0148 3.02 0.0007 4.06 0.0006 5.10 0.0005
      6.16 0.0008 7.26 0.0009 8.39 0.0008 9.58 0.0002 )
   #( 1.00 0.1357 2.00 0.0116 3.02 0.0026 4.04 0.0009 5.09 0.0004
      6.17 0.0005 7.27 0.0002 8.40 0.0001 )
   #( 1.00 0.2185 2.01 0.0087 3.03 0.0018 4.06 0.0025 5.11 0.0020
      6.20 0.0012 7.32 0.0005 8.46 0.0001 9.66 0.0003 )
   #( 1.00 0.2735 2.00 0.0038 3.02 0.0008 4.06 0.0012 5.12 0.0008
      6.22 0.0011 7.35 0.0003 8.50 0.0002 )
   #( 1.00 0.1441 1.99 0.0062 3.01 0.0023 4.05 0.0011 5.11 0.0012
      6.20 0.0003 7.33 0.0004 8.50 0.0001 )
   #( 1.00 0.0726 2.01 0.0293 3.03 0.0022 5.14 0.0005 6.26 0.0011
      7.41 0.0002 8.63 0.0002 )
   #( 1.00 0.0516 2.00 0.0104 3.02 0.0029 5.15 0.0002 6.27 0.0001 )
   #( 1.00 0.0329 2.00 0.0033 3.03 0.0013 4.10 0.0005 5.19 0.0004
      6.32 0.0002 )
   #( 1.00 0.0179 1.99 0.0012 3.04 0.0005 4.10 0.0017 5.20 0.0005
      6.35 0.0001 )
   #( 1.00 0.0334 2.01 0.0033 3.04 0.0011 4.13 0.0003 5.22 0.0003 )
   #( 0.99 0.0161 2.01 0.0100 3.04 0.0020 4.13 0.0003 )
   #( 1.00 0.0475 1.99 0.0045 3.03 0.0035 4.12 0.0011 )
   #( 1.00 0.0593 2.00 0.0014 4.17 0.0002 )
   #( 1.00 0.0249 2.01 0.0016 )
   #( 1.00 0.0242 2.00 0.0038 4.19 0.0002 )
   #( 1.00 0.0170 2.02 0.0030 )
   #( 1.00 0.0381 2.00 0.0017 3.09 0.0002 )
   #( 1.00 0.0141 2.03 0.0005 3.11 0.0003 4.26 0.0001 )
   #( 1.00 0.0122 2.03 0.0024 )
   #( 1.00 0.0107 2.07 0.0007 3.12 0.0004 )
   #( 1.00 0.0250 2.02 0.0026 3.15 0.0002 )
   #( 1.01 0.0092 )
   #( 1.01 0.0102 2.09 0.0005 )
   #( 1.00 0.0080 2.00 0.0005 3.19 0.0001 )
   #( 1.01 0.0298 2.01 0.0005 ) ) constant piano-spectra

0.04 value *clm-piano-attack-duration*
0.2  value *clm-piano-realease-duration*
-10  value *clm-db-drop-per-second*

\ This thing sounds pretty good down low, below middle c or so.
\ Unfortunately, there are some tens of partials down there and we're
\ using exponential envelopes.  You're going to wait for a long long
\ time just to hear a single low note.  The high notes sound pretty
\ rotten--they just don't sparkle; I have a feeling that this is due
\ to the low amplitude of the original data, and the lack of
\ mechanical noise.
\
\ The only thing you can do to alter the sound of a piano note is to
\ set the pfreq parameter.  Pfreq is used to look up the partials.  By
\ default, it's set to the requested frequency.  Setting it to a
\ neighboring freq is useful when you're repeating notes.  Note that
\ there's no nyquist detection; a high freq with a low pfreq, will
\ give you fold over (hmmm...maybe I can get those high notes to
\ sparkle after all).
instrument: lbj-piano <{ start dur freq amp
     :key
     degree        45.0
     distance      1.0
     reverb-amount 0.0 -- }>
  piano-spectra  12.0 freq 32.703 f/ flog 2.0 flog f/ f* f>s  array-ref normalize-partials { parts }
  dur *clm-piano-attack-duration* *clm-piano-realease-duration* f+ f+ to dur
  dur *clm-piano-realease-duration* f- { env1dur }
  env1dur mus-srate f* fround->s { env1samples }
  #( 0.0
     0.0
     *clm-piano-attack-duration* 100.0 f* env1dur f/ 4.0 f/
     1.0
     *clm-piano-attack-duration* 100.0 f* env1dur f/
     1.0
     100.0
     *clm-db-drop-per-second* env1dur f* db->linear ) { ampfun1 }
  :envelope ampfun1 :scaler amp :duration env1dur :base 10000.0 make-env { ampenv1 }
  :envelope #( 0 1 100 0 )
  :scaler   amp ampfun1 -1 array-ref f*
  :duration env1dur
  :base     1.0       make-env { ampenv2 }
  parts length 2/ 0.0 make-vct { alist }
  parts length 2/ make-array map!
    alist i  parts i 2* 1+ array-ref  vct-set! drop
    :frequency  parts i 2* array-ref  freq f* make-oscil
  end-map { oscils }
  start dur #{ :degree degree :distance distance :reverb-amount reverb-amount } run-instrument
    0.0 ( sum ) oscils each ( os ) 0.0 0.0 oscil alist i vct-ref f* f+ end-each ( sum )
    i env1samples > if ampenv2 else ampenv1 then env ( sum ) f*
  end-run
;instrument

: lbj-piano-test <{ :optional start 0.0 dur 1.0 -- }>
  start now!
  now@ dur 440 0.5 lbj-piano
  dur 0.24 f+ 0.2 f+ step
;

\ RESFLT
\ clm/resflt.ins
instrument: resflt <{ start dur
     :key
     driver     #f
     ranfreq    10000.0
     noiamp     0.01
     noifun     #( 0 0 50 1 100 0 )
     cosamp     0.1
     cosfreq1   200.0
     cosfreq0   230.0
     cosnum     10
     ampcosfun  #( 0 0 50 1 100 0 )
     freqcosfun #( 0 0 100 1 )
     freq1      550.0
     r1         0.995
     g1         0.1
     freq2      1000.0
     r2         0.995
     g2         0.1
     freq3      2000.0
     r3         0.995
     g3         0.1
     degree     0.0
     distance   1.0 -- }>
  doc" 0 1 <'> resflt with-sound"
  :radius r1 :frequency freq1 make-two-pole { f1 }
  :radius r2 :frequency freq2 make-two-pole { f2 }
  :radius r3 :frequency freq3 make-two-pole { f3 }
  nil nil nil { frqf ampf gen }
  driver if
    :envelope noifun :scaler noiamp :duration dur                                make-env to ampf
    :frequency ranfreq                                                           make-rand to gen
  else
    :envelope freqcosfun  :scaler cosfreq1 cosfreq0 f- hz->radians :duration dur make-env to frqf
    :envelope ampcosfun   :scaler cosamp :duration dur                           make-env to ampf
    :frequency cosfreq0 :cosines cosnum                             make-sum-of-cosines to gen
  then
  start dur #{ :degree degree :distance distance } run-instrument
    gen driver if 0.0 ( rand ) else frqf env ( sum-of-cosines ) then 0.0 mus-run ampf env f*
    { input }
    f1 input g1 f* two-pole
    f2 input g2 f* two-pole f+
    f3 input g3 f* two-pole f+
  end-run
;instrument

: resflt-test <{ :optional start 0.0 dur 1.0 -- }>
  start now!
  now@ dur :driver #f resflt
  dur 0.2 f+ step
  now@ dur :driver #t resflt
  dur 0.2 f+ step
;

hide
: scratch-input-cb { rd samp -- proc ; dir self -- r }
  1 proc-create samp , rd ,
 does> { dir self -- r }
  self @ { samp }
  self cell+ @ { rd }
  rd samp 0 file->sample		\ (file->sample rd samp 0)
  dir self +!				\ samp += dir
;
set-current

\ SCRATCH-INS
instrument: scratch-ins <{ start file src-ratio turntable -- }>
  file find-file to file
  file false? if 'file-not-found $" %s: cannot find %S" #( get-func-name file ) fth-raise then
  file mus-sound-duration { dur }
  file make-readin { f }
  turntable 0 object-ref seconds->samples { cur-samp }
  turntable 1 object-ref seconds->samples { turn-samp }
  :input f cur-samp scratch-input-cb :srate src-ratio make-src { rd }
  src-ratio f0> { forwards }
  forwards turn-samp cur-samp < && if rd src-ratio fnegate set-mus-increment drop then
  1 { turn-i }
  0 { turning }
  0.0 0.0 { last-val1 last-val2 }
  start dur #{ :degree 90.0 random } run-instrument
    turn-i turntable length >= if leave then
    rd 0.0 src { val }
    turning unless
      forwards cur-samp turn-samp >= && if
	1
      else
	forwards 0= cur-samp turn-samp <= &&
	if
	  -1
	else
	  turning
	then
      then to turning
    else
      last-val2 last-val1 f<= last-val1 val f>= &&
      last-val2 last-val1 f>= last-val1 val f<= && || if
	turn-i 1+ to turn-i
	turn-i turntable length < if
	  turntable turn-i object-ref seconds->samples to turn-samp
	  forwards negate to forwards
	  rd  rd mus-increment fnegate  set-mus-increment drop
	then
	0 to turning
      then
    then
    last-val1 to last-val2
    val to last-val1
    val
  end-run
  f mus-close drop
;instrument
previous

: scratch-test <{ :optional start 0.0 dur 1.0 -- }>
  start now!
  start "fyow.snd" dur 1.5 fmin #( 0 0.5 0.25 1 ) scratch-ins
  "fyow.snd" find-file mus-sound-duration 0.2 f+ step
;

\ PINS
\
\ spectral modeling (SMS)
instrument: pins <{ start dur file amp
     :key
     transposition 1.0
     time-scaler   1.0
     fftsize       256
     highest-bin   128
     max-peaks     16
     attack        #f -- }>
  doc" start dur \"fyow.snd\" 1.0 :time-scaler 2.0 pins"
  file find-file to file
  file false? if 'file-not-found $" %s: cannot find %S" #( get-func-name file ) fth-raise then
  file mus-sound-duration { fdur }
  dur time-scaler f/      { sdur }
  sdur fdur f> if
    'forth-error
    $" %s is %.3f seconds long, but we'll need %.3f seconds of data for this note"
    #( file fdur sdur ) fth-raise
  then
  file make-readin                                 { fil }
  fftsize make-vct                                 { fdr }
  fftsize make-vct                                 { fdi }
  blackman2-window fftsize 0.0 0.0 make-fft-window { win }
  fftsize make-vct                                 { fftamps }
  max-peaks 2*                                     { max-oscils }
  max-oscils make-vct                              { current-peak-freqs }
  max-oscils make-vct                              { last-peak-freqs }
  max-oscils make-vct                              { current-peak-amps }
  max-oscils make-vct                              { last-peak-amps }
  max-peaks  make-vct                              { peak-amps }
  max-peaks  make-vct                              { peak-freqs }
  max-oscils make-array map! :frequency 0.0 make-oscil end-map { resynth-oscils }
  max-oscils make-vct                              { ampls }
  max-oscils make-vct                              { rates }
  max-oscils make-vct                              { freqs }
  max-oscils make-vct                              { sweeps }
  fftsize 4.0 f/ fround->s                         { hop }
  time-scaler hop f* fround->s                     { outhop }
  outhop 1/f                                       { ifreq }
  ifreq hz->radians                                { ihifreq }
  mus-srate fftsize f/                             { fft-mag }
  max-oscils                                       { cur-oscils }
  attack if attack else 0 then                     { ramped }
  attack                                           { splice-attack }
  attack if attack else 1 then                     { attack-size }
  0.0                                              { ramp-ind }
  attack-size make-vct                             { ramped-attack }
  outhop                                           { trigger }
  win fftsize 0.42323 f* 1/f vct-scale! drop
  0 { filptr }
  start dur #{ :degree 90.0 random } run-instrument
    splice-attack if
      attack-size 1/f { ramp }
      fil filptr 0 file->sample amp f* ( outval )
      filptr 1+ to filptr
      filptr attack-size > if
	1 { mult }
	ramped-attack map!
	  fil filptr i + 0 file->sample mult f*
	  mult ramp f- to mult
	end-map drop
	#f to splice-attack
      then
      ( outval )
    else
      trigger outhop >= if
	0 { peaks }
	0 to trigger
	fdr map! fil filptr i + 0 file->sample win i vct-ref f* end-map drop
	filptr fdr vct-length + to filptr
	fdi 0.0 vct-fill! drop
	filptr fftsize hop - - to filptr
	fdr fdi fftsize 1 mus-fft drop
	highest-bin 0 ?do
	  fftamps i  fdr i vct-ref dup f* fdi i vct-ref dup f* f+ fsqrt f2*  vct-set! drop
	loop
	current-peak-freqs each { fv }
	  current-peak-amps i vct-ref { av }
	  last-peak-freqs i fv vct-set! drop
	  last-peak-amps  i av vct-set! drop
	  current-peak-amps i 0.0 vct-set! drop
	end-each
	peak-amps 0.0 vct-fill! drop
	fftamps 0 vct-ref { ra }
	0.0 0.0 { la ca }
	highest-bin 0 ?do
	  ca to la
	  ra to ca
	  fftamps i vct-ref to ra
	  ca 0.001 f>
	  ca ra f> &&
	  ca la f> && if
	    la flog10 ra flog10 f- f2/  la flog10 -2.0 ca flog10 f* f+  ra flog10 f+  f/ { offset }
	    10.0  ca flog10  0.25  la flog10 ra flog10 f-  f* offset f*  f-  f** { amp-1 }
	    fft-mag  i offset -1.0 f+ f+  f* { freq }
	    peaks max-peaks = if
	      0 { minp }
	      peak-amps 0 vct-ref { minpeak }
	      max-peaks 1 ?do
		peak-amps i vct-ref minpeak f< if
		  i to minp
		  peak-amps i vct-ref to minpeak
		then
	      loop
	      amp-1 minpeak f> if
		peak-freqs minp freq vct-set! drop
		peak-amps minp amp-1 vct-set! drop
	      then
	    else
	      peak-freqs peaks freq vct-set! drop
	      peak-amps peaks amp-1 vct-set! drop
	      peaks 1+ to peaks
	    then
	  then
	loop
	peaks 0 ?do
	  0 { maxp }
	  peak-amps 0 vct-ref ( maxpk )
	  max-peaks 1 ?do
	    peak-amps i vct-ref over f> if
	      i to maxp
	      drop ( maxpk )
	      peak-amps i vct-ref ( maxpk )
	    then
	  loop
	  ( maxpk ) f0> if
	    -1 { closestp }
	    10 { closestamp }
	    peak-freqs maxp vct-ref { cur-freq }
	    cur-freq 1/f { icf }
	    max-peaks 0 ?do
	      last-peak-amps i vct-ref f0> if
		icf last-peak-freqs i vct-ref cur-freq f- fabs f* { closeness }
		closeness closestamp f< if
		  closeness to closestamp
		  i to closestp
		then
	      then
	    loop
	    closestamp 0.1 f< if
	      current-peak-amps closestp  peak-amps maxp vct-ref  vct-set! drop
	      peak-amps maxp 0.0 vct-set! drop
	      current-peak-freqs closestp cur-freq vct-set! drop
	    then
	  then
	loop
	max-peaks 0 ?do
	  peak-amps i vct-ref f0> if
	    -1 { new-place }
	    max-oscils 0 ?do
	      last-peak-amps i vct-ref f0= current-peak-amps i vct-ref f0= && if
		i to new-place
		leave
	      then
	    loop
	    current-peak-amps new-place   peak-amps  i vct-ref  vct-set! drop
	    peak-amps i 0.0 vct-set! drop
	    current-peak-freqs new-place  peak-freqs i vct-ref  vct-set! drop
	    last-peak-freqs new-place     peak-freqs i vct-ref  vct-set! drop
	    resynth-oscils new-place array-ref ( gen )
	    transposition peak-freqs i vct-ref f* ( val )
	    set-mus-frequency drop
	  then
	loop
	0 to cur-oscils
	max-oscils 0 ?do
	  rates i  current-peak-amps i vct-ref last-peak-amps i vct-ref f- ifreq f*  vct-set! drop
	  current-peak-amps i vct-ref f0<> last-peak-amps i vct-ref f0<> || if
	    i to cur-oscils
	  then
	  sweeps i
	  current-peak-freqs i vct-ref last-peak-freqs i vct-ref f- transposition f* ihifreq f*
	  vct-set! drop
	loop
	cur-oscils 1+ to cur-oscils
      then
      trigger 1+ to trigger
      ramped 0= if
	0.0 ( sum )
      else
	ramped-attack ramp-ind vct-ref ( sum )
	ramp-ind 1+ to ramp-ind
	ramp-ind ramped = if 0 to ramp-ind then
      then ( sum )
      cur-oscils 0 ?do
	ampls i vct-ref f0<> rates i vct-ref f0<> || if
	  resynth-oscils i array-ref  freqs i vct-ref  0.0 oscil
	  ampls i vct-ref f* f+ ( sum += ... )
	  ampls i  rates  i vct-ref  object-set+!
	  freqs i  sweeps i vct-ref  object-set+!
	then
      loop
      amp ( sum ) f*
    then
  end-run
;instrument

: pins-test <{ :optional start 0.0 dur 1.0 -- }>
  start now!
  now@ dur "fyow.snd" 1.0 :time-scaler 2.0 pins
  dur 0.2 f+ step
;

\ ZC
instrument: zc <{ start dur freq amp len1 len2 feedback -- }>
  :frequency freq make-pulse-train { s }
  :size len1 :scaler feedback :max-size len1 len2 max 1+ make-comb { d0 }
  :envelope #( 0 0 1 1 ) :scaler len2 len1 f-  :duration dur make-env { zenv }
  start dur #{ :degree 90.0 random } run-instrument
    d0  s 0.0 pulse-train amp f*  zenv env  comb
  end-run
;instrument

: zc-test <{ :optional start 0.0 dur 1.0 -- }>
  start now!
  now@ dur 100 0.4 20 100 0.95 zc
  dur 0.2 f+ step
  now@ dur 100 0.4 100 20 0.95 zc
  dur 0.2 f+ step
;

\ ZN
\
\ notches are spaced at srate/len, feedforward sets depth thereof so
\ sweep of len from 20 to 100 sweeps the notches down from 1000 Hz to
\ ca 200 Hz so we hear our downward glissando beneath the pulses.
instrument: zn <{ start dur freq amp len1 len2 feedforward -- }>
  :frequency freq make-pulse-train { s }
  :size len1 :scaler feedforward :max-size len1 len2 max 1+ make-notch { d0 }
  :envelope #( 0 0 1 1 ) :scaler len2 len1 f- :duration dur make-env { zenv }
  start dur #{ :degree 90.0 random } run-instrument
    d0  s 0.0 pulse-train amp f*  zenv env  notch
  end-run
;instrument

: zn-test <{ :optional start 0.0 dur 1.0 -- }>
  start now!
  now@ dur 100 0.5 20 100 0.95 zn
  dur 0.2 f+ step
  now@ dur 100 0.5 100 20 0.95 zn
  dur 0.2 f+ step
;

\ ZA
instrument: za <{ start dur freq amp len1 len2 fb ffw -- }>
  :frequency freq make-pulse-train { s }
  :size len1 :feedback fb :feedforward ffw :max-size len1 len2 max 1+ make-all-pass { d0 }
  :envelope #( 0 0 1 1 ) :scaler len2 len1 f- :duration dur make-env { zenv }
  start dur #{ :degree 90.0 random } run-instrument
    d0  s 0.0 pulse-train amp f*  zenv env  all-pass
  end-run
;instrument

: za-test <{ :optional start 0.0 dur 1.0 -- }>
  start now!
  now@ dur 100 0.3 20 100 0.95 0.95 za
  dur 0.2 f+ step
  now@ dur 100 0.3 100 20 0.95 0.95 za
  dur 0.2 f+ step
;

hide
: clm-src-cb { gen -- proc; dir self -- r }
  1 proc-create gen ,
 does> ( dir self -- r )
  nip @ ( gen ) #f #f granulate
;
set-current

\ CLM-EXPSRC
instrument: clm-expsrc <{ start dur in-file exp-ratio src-ratio amp
     :optional
     rev           #f
     start-in-file 0 -- }>
  in-file find-file to in-file
  in-file false? if 'file-not-found $" %s: cannot find %S" #( get-func-name in-file ) fth-raise then
  start-in-file in-file mus-sound-srate f* fround->s { stf }
  :file in-file :channel 0 :start stf make-readin { fdA }
  :input fdA readin-cb :expansion exp-ratio make-granulate { exA }
  in-file mus-sound-chans 2 = *output* mus-channels 2 = && { two-chans }
  two-chans if :file in-file :channel 1 :start stf make-readin else #f then { fdB }
  :input fdB readin-cb :expansion exp-ratio make-granulate { exB }
  :input exA clm-src-cb :srate src-ratio make-src { srcA }
  two-chans if :input exB clm-src-cb :srate src-ratio make-src else #f then { srcB }
  *reverb* rev && { revit }
  revit if two-chans if rev f2/ else rev then else 0.0 then { rev-amp }
  start dur run
    srcA 0.0 src  amp f* { valA }
    two-chans if srcB 0.0 src  amp f* else 0.0 then { valB }
    i valA 0 *output* out-any drop
    two-chans if i valB 1 *output* out-any drop then
    revit if i valA valB f+ rev-amp f* 0 *reverb* out-any drop then
  loop
;instrument
previous

: clm-expsrc-test <{ :optional start 0.0 dur 1.0 -- }>
  start now!
  now@ dur "oboe.snd" 2.0 1.0 1.0 clm-expsrc
  dur 0.2 f+ step
;

\ EXP-SND
instrument: exp-snd <{ file start dur amp
     :optional
     exp-amt 1.0
     ramp    0.4
     seglen  0.15
     sr      1.0
     hop     0.05
     ampenv  #f -- }>
  doc" ;; granulate with envelopes on the expansion amount, segment envelope shape,\n\
;; segment length, hop length, and input file resampling rate\n\
\"fyow.snd\" 0 3 1 #( 0 1 1 3 ) 0.4 0.15 #( 0 2 1 0.5 ) 0.05 <'> exp-snd with-sound\n\
\"oboe.snd\" 0 3 1 #( 0 1 1 3 ) 0.4 0.15 #( 0 2 1 0.5 ) 0.2  <'> exp-snd with-sound"
  file find-file to file
  file false? if 'file-not-found $" %s: cannot find %S" #( get-func-name file ) fth-raise then
  file 0 make-readin { f0 }
  :envelope exp-amt array? if exp-amt else #( 0 exp-amt 1 exp-amt ) then
  :duration dur make-env { expenv }
  :envelope seglen array? if seglen else #( 0 seglen 1 seglen ) then
  :duration dur make-env { lenenv }
  seglen if
    seglen array? if seglen max-envelope else seglen then
  else
    0.15
  then { max-seg-len }
  seglen if
    seglen array? if seglen 1 array-ref else seglen then
  else
    0.15
  then { initial-seg-len }
  max-seg-len 0.15 f> if 0.6 0.15 f* max-seg-len f/ else 0.6 then { scaler-amp }
  :envelope sr array? if sr else #( 0 sr 1 sr ) then
  :duration dur make-env { srenv }
  ramp array? if ramp else #( 0 ramp 1 ramp ) then { rampdata }
  :envelope rampdata :duration dur make-env { rampenv }
  ramp if
    ramp array? if ramp 1 array-ref else ramp then
  else
    0.4
  then { initial-ramp-time }
  :envelope hop array? if hop else #( 0 hop 1 hop ) then
  :duration dur make-env { hopenv }
  hop if
    hop array? if hop max-envelope else hop then
  else
    0.05
  then { max-out-hop }
  hop if
    hop array? if hop 1 array-ref else hop then
  else
    0.05
  then { initial-out-hop }
  exp-amt if
    exp-amt array? if exp-amt min-envelope else exp-amt then
  else
    1.0
  then { min-exp-amt }
  exp-amt if
    exp-amt array? if exp-amt 1 array-ref else exp-amt then
  else
    1.0
  then { initial-exp-amt }
  max-out-hop min-exp-amt f/ { max-in-hop }
  :envelope ampenv #( 0 0 0.5 1 1 0 ) || :scaler amp :duration dur make-env { ampe }
  :input f0 readin-cb
  :expansion initial-exp-amt
  :max-size max-out-hop max-in-hop fmax max-seg-len f+ mus-srate f* fceil f>s
  :ramp initial-ramp-time
  :hop initial-out-hop
  :length initial-seg-len
  :scaler scaler-amp make-granulate { ex-a }
  ampe env { vol }
  ex-a granulate vol f* { val-a0 }
  ex-a granulate vol f* { val-a1 }
  rampdata min-envelope f0<= rampdata max-envelope 0.5 f>= || if
    'forth-error
    $" ramp argument to expand must always be between 0.0 and 0.5, %.3f -- %.3f"
    #( rampdata min-envelope rampdata max-envelope ) fth-raise
  then
  0.0 0.0 { ex-samp next-samp }
  0.0 0.0 0.0 0.0 0.0 { expa segl resa rmpl hp }
  0 0 { sl rl }
  start dur #{ :degree 90.0 random } run-instrument
    expenv  env to expa
    lenenv  env to segl
    srenv   env to resa
    rampenv env to rmpl
    hopenv  env to hp
    segl mus-srate f* floor dup f>s to sl
    ( fsl ) rmpl   f* floor     f>s to rl
    ampe env to vol
    ex-a sl   set-mus-length drop
    ex-a rl   set-mus-ramp drop
    ex-a hp   set-mus-frequency drop
    ex-a expa set-mus-increment drop
    resa +to next-samp
    next-samp ex-samp 1.0 f+ f> if
      next-samp ex-samp f- fround->s 0 ?do
	val-a1                to val-a0
	ex-a granulate vol f* to val-a1
	1.0                  +to ex-samp
      loop
    then
    next-samp ex-samp f= if
      val-a0
    else
      next-samp ex-samp f-  val-a1 val-a0 f-  f*  val-a0 f+
    then
  end-run
;instrument

: exp-snd-test <{ :optional start 0.0 dur 1.0 -- }>
  start now!
  "fyow.snd" now@ dur 1.0 #( 0 1 1 3 ) 0.4 0.15 #( 0 2 1 0.5 ) 0.05 exp-snd
  dur 0.2 f+ step
  "oboe.snd" now@ dur 1.0 #( 0 1 1 3 ) 0.4 0.15 #( 0 2 1 0.5 ) 0.2  exp-snd
  dur 0.2 f+ step
;

struct
  cell% field exp-rampval
  cell% field exp-rampinc
  cell% field exp-loc
  cell% field exp-segctr
  cell% field exp-whichseg
  cell% field exp-ramplen
  cell% field exp-steadylen
  cell% field exp-trigger
end-struct grn%

\ EXPFIL
instrument: expfil <{ start dur hopsecs rampsecs steadysecs file1 file2 -- }>
  rampsecs seconds->samples { ramplen }
  grn% %alloc { grn1 }
  grn% %alloc { grn2 }
  0.0 0.0                         grn1 exp-rampval !   grn2 exp-rampval !
  ramplen 1/f dup                 grn1 exp-rampinc !   grn2 exp-rampinc !
  0 0                             grn1 exp-loc !       grn2 exp-loc !
  0 0                             grn1 exp-segctr !    grn2 exp-segctr !
  0 0                             grn1 exp-whichseg !  grn2 exp-whichseg !
  ramplen dup                     grn1 exp-ramplen !   grn2 exp-ramplen !
  steadysecs seconds->samples dup grn1 exp-steadylen ! grn2 exp-steadylen !
  0 0                             grn1 exp-trigger !   grn2 exp-trigger !
  hopsecs seconds->samples { hop }
  start seconds->samples   { out1 }
  hop out1 +               { out2 }
  file1 find-file to file1
  file1 false? if 'file-not-found $" %s: cannot find %S" #( get-func-name file1 ) fth-raise then
  file1 0 make-readin { fil1 }
  file2 find-file to file2
  file2 false? if 'file-not-found $" %s: cannot find %S" #( get-func-name file2 ) fth-raise then
  file2 0 make-readin { fil2 }
  0.0 { inval }
  start dur #{ :degree 90.0 random } run-instrument
    0.0 ( val )
    i out1 = if
      fil1 grn1 exp-loc @ 0 file->sample to inval
      1 grn1 exp-loc +!
      grn1 exp-whichseg @ case
	0 of
	  grn1 exp-rampval @ inval f* to inval
	  grn1 exp-rampinc @ grn1 exp-rampval +!
	  1 grn1 exp-segctr +!
	  grn1 exp-segctr @ grn1 exp-ramplen @ = if
	    0 grn1 exp-segctr !
	    1 grn1 exp-whichseg +!
	  then
	endof
	1 of
	  1 grn1 exp-segctr +!
	  grn1 exp-segctr @ grn1 exp-steadylen @ = if
	    0 grn1 exp-segctr !
	    1 grn1 exp-whichseg +!
	  then
	endof
	grn1 exp-rampval @ inval f* to inval
	1 grn1 exp-segctr +!
	grn1 exp-rampinc @ fnegate grn1 exp-rampval +!
	grn1 exp-segctr @ grn1 exp-ramplen @ = if
	  0 grn1 exp-segctr !
	  1 grn1 exp-trigger !
	  0 grn1 exp-whichseg !
	  0.0 grn1 exp-rampval !
	then
      endcase
      inval f+ ( val )
      1 +to out1
      grn1 exp-trigger @ 1 = if
	0 grn1 exp-trigger !
	hop +to out1
      then
    then
    i out2 = if
      fil2 grn2 exp-loc @ 0 file->sample { inval }
      1 grn2 exp-loc +!
      grn2 exp-whichseg @ case
	0 of
	  grn2 exp-rampval @ inval f* to inval
	  grn2 exp-rampinc @ grn2 exp-rampval +!
	  1 grn2 exp-segctr +!
	  grn2 exp-segctr @ grn2 exp-ramplen @ = if
	    0 grn2 exp-segctr !
	    1 grn2 exp-whichseg +!
	  then
	endof
	1 of
	  1 grn2 exp-segctr +!
	  grn2 exp-segctr @ grn2 exp-steadylen @ = if
	    0 grn2 exp-segctr !
	    1 grn2 exp-whichseg +!
	  then
	endof
	grn2 exp-rampval @ inval f* to inval
	1 grn2 exp-segctr +!
	grn2 exp-rampinc @ fnegate grn2 exp-rampval +!
	grn2 exp-segctr @ grn2 exp-ramplen @ = if
	  0 grn2 exp-segctr !
	  1 grn2 exp-trigger !
	  0 grn2 exp-whichseg !
	  0.0 grn2 exp-rampval !
	then
      endcase
      inval f+ ( val )
      1 +to out2
      grn2 exp-trigger @ 1 = if
	0 grn2 exp-trigger !
	hop +to out2
      then
    then
  end-run
  grn1 free throw
  grn2 free throw
;instrument

: expfil-test <{ :optional start 0.0 dur 1.0 -- }>
  start now!
  now@ dur 0.2 0.01 0.1 "oboe.snd" "fyow.snd" expfil
  dur 0.2 f+ step
;

\ GRAPH-EQ
\
\ From: Marco Trevisani <marco@ccrma.Stanford.EDU>
\ 
\ This should work like a Graphic Equalizer....
\ Very easy to use. Just some note:
\ 
\ "amp" & "amp-env" apply an enveloppe to the final result of the
\ filtering.  
\ 
\ "dur" as ""standard"" in my instruments, when dur = 0 it will take the length of the
\ sndfile input, otherwise the duration in seconds.
\ 
\ "gain-freq-list" is a list of gains and frequencies to
\ filter --in this order gain and frequencies--. There is no limit to
\ the size of the list. Gain can be a number or an
\ envelope. Unfortunatelly in this version they cant alternate, one
\ should chose, all envelopes or all numbers i.e.: 
\ case 1 -> #( .1 440.0 .3 1500.0 .2 330.0 ...etc) or 
\ case 2 -> #((0 .1 1 .5) 440.0 (0 1 1 .01) 1500 (0 .3 1 .5) 330.0 ...etc) 
\ #( .1 440.0 (0 1 1 .01) 1500 ..etc) <<< again, this is not allowed ..
\ 
\ "offset-gain" This apply to all the gains if case 1. It adds or
\ subtracts an offset to all the gains in the list. This number can be positive or
\ negative. In case the result is a negative number --let's say offset =
\ -.4 and, like in case 1, the first gain is .1, the result would be
\ -.3 -- the instrument will pass a gain equal to 0.  
\ 
\ "filt-gain-scale" & "filt-gain-base" will apply to the elements of the
\ envelopes if we are in case 2, gains are envelopes.
instrument: graph-eq <{ file start dur
     :key
     file-start      0.0
     amplitude       1.0
     amp-env         #( 0 1 0.8 1 1 0 )
     amp-base        1.0
     offset-gain     0.0
     gain-freq-list  #( #( 0 1 1 0 ) 440 #( 0 0 1 1 ) 660 )
     filt-gain-scale 1.0
     filt-gain-base  1.0
     a1              0.99 -- }>
  doc" \"oboe.snd\" 0 2 graph-eq"
  file find-file to file
  file false? if 'file-not-found $" %s: cannot find %S" #( get-func-name file ) fth-raise then
  :file file :start file mus-sound-srate file-start f* fround->s make-readin { rd }
  :envelope amp-env :scaler amplitude :duration dur :base amp-base make-env { ampf }
  gain-freq-list length 2/ { len }
  len make-array { gainl }
  len make-array { freql }
  0 { idx }
  gain-freq-list length 1- 0 ?do
    gainl idx  gain-freq-list i    array-ref array-set!
    freql idx  gain-freq-list i 1+ array-ref array-set!
    1 +to idx
  2 +loop
  gainl 0 array-ref array? dup { if-list-in-gain } if
    len make-array map!
      :envelope gainl i array-ref
      :scaler   filt-gain-scale
      :duration dur
      :base     filt-gain-base make-env
    end-map
  else
    #f
  then { env-size }
  freql map :frequency *key* :radius a1 make-formant end-map { frm-size }
  len 1.0 make-vct { gains }
  gainl each { gval }
    freql i array-ref { fval }
    if-list-in-gain if
      :envelope gval
      :scaler filt-gain-scale
      :duration dur
      :base filt-gain-base make-env
      env-size i rot ( en ) array-set!
      frm-size i :frequency fval :radius a1 make-formant array-set!
    else
      frm-size i :frequency fval :radius a1 make-formant array-set!
      gains i
      offset-gain gval f+ f0< if
	0.0
      else
	offset-gain gval f+
      then vct-set! drop
    then
  end-each
  1.0 a1 f- { 1-a1 }
  start dur #{ :degree 90.0 random } run-instrument
    rd readin { inval }
    0.0 ( outval )
    env-size each { en }
      if-list-in-gain if
	gains i  en env  1-a1 f* vct-set! drop
      then
      gains i vct-ref
      frm-size i array-ref ( fmt ) inval 0.0 formant  f*  f+ ( outval )
    end-each
    ampf env f* ( outval )
  end-run
  rd mus-close drop
;instrument

: graph-eq-test <{ :optional start 0.0 dur 1.0 -- }>
  start now!
  "oboe.snd" now@ dur :amplitude 50.0 graph-eq
  dur 0.2 f+ step
;

\ ANOI
\ 
\ a kind of noise reduction -- on-going average spectrum is squelched
\ to some extent obviously aimed at intermittent signal in background
\ noise
\ this is based on Perry Cook's Scrubber.m
\ 
\ clm/anoi.ins
instrument: anoi <{ fname start dur :optional fftsize 128 amp-scaler 1.0 R two-pi -- }>
  fftsize 2/ { freq-inc }
  fftsize  0.0 make-vct { fdr }
  fftsize  0.0 make-vct { fdi }
  freq-inc 1.0 make-vct { spectr }
  freq-inc 1.0 make-vct { scales }
  freq-inc 0.0 make-vct { diffs }
  blackman2-window fftsize 0.0 0.0 make-fft-window { win }
  0.0 { amp }
  amp-scaler 4.0 f* mus-srate f/ { incr }
  fname find-file to fname
  fname false? if 'file-not-found $" %s: cannot find %S" #( get-func-name fname ) fth-raise then
  fname make-file->sample { fil }
  1.0 R fftsize f/ f- { radius }
  mus-srate fftsize f/ { bin }
  freq-inc make-array map! :radius radius :frequency i bin f* make-formant end-map { fs }
  start seconds->samples { beg }
  start dur #{ :degree 90.0 random } run-instrument
    fil i beg - 0 file->sample { inval }
    fdr inval cycle-set!
    \ fdr i fftsize mod inval vct-set! drop
    amp amp-scaler f< if incr +to amp then
    \ i beg - fftsize mod unless
    fdr cycle-start@ 0= if
      fdr fdi win 1 spectrum drop
      diffs map
	spectr i vct-ref 0.9 f*  fdr i vct-ref 0.1 f*  f+ { x }
	spectr i x vct-set! drop
	x fdr i vct-ref f>= if
	  scales i vct-ref fftsize fnegate f/
	else
	  fdr i vct-ref dup spectr i vct-ref f- swap f/ scales i vct-ref f- fftsize f/
	then
      end-map drop
    then
    0.0 ( outval )
    fs each { fmt }
      scales i vct-ref { curscl }
      fmt inval 0.0 formant curscl f* f+ ( outval += ... )
      scales i  diffs i vct-ref curscl f+  vct-set! drop
    end-each
    ( outval ) amp f*
  end-run
  fil mus-close drop
;instrument

: anoi-test <{ :optional start 0.0 dur 1.0 -- }>
  start now!
  "fyow.snd" now@ dur 128 2.0 anoi
  dur 0.2 f+ step
;

\ Date: Fri, 25 Sep 1998 09:56:41 +0300
\ From: Matti Koskinen <mjkoskin@sci.fi>
\ To: linux-audio-dev@ginette.musique.umontreal.ca
\ Subject: [linux-audio-dev] Announce: alpha version of denoising
\ [...]
\ 	I wrote a simple denoiser called anoi after it's parent
\ 	clm-instrument anoi.ins.
\ 
\ 	anoi tries to remove white noise like tape hiss from wav-
\ 	files. Removing of noise succeeds ok, but depending of the
\ 	original sound, some distortion can be audible.
\ 
\ 	If someone is interested, http://www.sci.fi/~mjkoskin
\ 	contains tarred and gzipped file.
\ 
\ 	Now only monophonic wav-files can be denoised, but adding
\ 	others isn't too difficult. 
\ 
\ -matti
\ mjkoskin@sci.fi

\ FULLMIX
instrument: fullmix <{ in-file
     :optional
     start      0.0
     dur        #f
     inbeg      0.0
     matrix     #f
     sr         #f
     rev-amount #f -- }>
  doc" \"pistol.snd\" 0 1 fullmix\n\
:envelope #( 0 0 1 1 ) :duration dur :scaler 0.5 make-env value en
\"oboe.snd\" 0 2 0 #( #( 0.8 en ) ) 2.0 <'> fullmix with-sound"
  in-file find-file to in-file
  in-file false? if 'file-not-found $" %s: cannot find %S" #( get-func-name in-file ) fth-raise then
  dur unless in-file mus-sound-duration inbeg f- sr if sr fabs else 1.0 then f/ to dur then
  in-file mus-sound-chans { in-chans }
  inbeg in-file mus-sound-srate f* fround->s { inloc }
  *output* mus-channels { out-chans }
  matrix if
    in-chans out-chans max make-mixer
  else
    in-chans out-chans max 1.0 make-scalar-mixer
  then { mx }
  *reverb* rev-amount f0> && if
    in-chans make-mixer { rmx }
    in-chans 0 ?do rmx i 0 rev-amount mixer-set! drop loop
    rmx
  else
    #f
  then { rev-mx }
  #f { envs }
  matrix if
    matrix object-length 0> if
      in-chans 0 ?do
	matrix i object-ref { inlist }
	out-chans 0 ?do
	  inlist i object-ref { outn }
	  outn if
	    outn number? if
	      mx j ( inp ) i ( outp ) outn mixer-set! drop
	    else
	      outn env? outn array? || if
		envs unless
		  in-chans make-array map! out-chans make-array end-map to envs
		then
		envs j ( inp ) array-ref  i ( outp ) 
		outn env? if outn else :envelope outn :duration dur make-env then
		array-set!
	      else
		$" %s: unknown element in matrix: %S" #( get-func-name outn ) string-format warning
	      then
	    then
	  then
	loop
      loop
    else
      in-chans 0 ?do i out-chans < if mx i i matrix mixer-set! drop then loop
    then
  then
  sr unless
    \ ws-info ( start dur local-vars -- start dur )
    \ 
    \ This is normally done in RUN or RUN-INSTRUMENT, but here
    \ we haven't one of them.
    \ 
    start dur local-variables ws-info ( start dur )
    ( start ) seconds->samples { st }
    ( dur ) seconds->samples { samps }
    *output* in-file undef make-file->frame st samps inloc mx envs mus-mix drop
    rev-mx if *reverb* 1 make-frame st samps inloc rev-mx #f mus-mix drop then
  else
    in-chans make-frame { inframe }
    out-chans make-frame { outframe }
    in-chans make-array map!
      :file in-file :channel i :start inloc make-readin { rd }
      :input rd readin-cb :srate sr make-src
    end-map { srcs }
    envs if
      start dur run
	envs each
	  each { en } env? if mx j ( inp ) i ( outp ) en env mixer-set! drop then end-each
	end-each
	in-chans 0 ?do inframe i srcs i array-ref 0.0 src frame-set! drop loop
	*output* i inframe mx outframe frame->frame frame->file drop
	rev-mx if *reverb* i inframe rev-mx outframe frame->frame frame->file drop then
      loop
    else
      start dur run
	in-chans 0 ?do inframe i srcs i array-ref 0.0 src frame-set! drop loop
	*output* i inframe mx outframe frame->frame frame->file drop
	rev-mx if *reverb* i inframe rev-mx outframe frame->frame frame->file drop then
      loop
    then
  then
;instrument

: fullmix-test <{ :optional start 0.0 dur 1.0 -- }>
  .stack
  start now!
  :envelope #( 0 0 1 1 ) :duration dur :scaler 0.5 make-env { en }  
  "pistol.snd" now@ dur fullmix
  dur 0.2 f+ step
  "oboe.snd"   now@ dur 0 #( #( 0.1 en ) ) fullmix
  dur 0.2 f+ step
;

'snd provided? [if]
  \ ;;; bes-fm -- can also use bes-j0 here as in earlier versions
  instrument: bes-fm <{ start dur freq amp ratio index -- }>
    0.0 0.0 { car-ph mod-ph }
    freq hz->radians { car-incr }
    ratio car-incr f* { mod-incr }
    :envelope #( 0 0 25 1 75 1 100 0 ) :scaler amp :duration dur make-env { ampenv }
    start dur #{ :degree 90.0 random } run-instrument
      ampenv env car-ph bes-j1 f* ( result )
      mod-ph bes-j1 index f* car-incr f+ +to car-ph
      mod-incr +to mod-ph
    end-run
  ;instrument

  : bes-fm-test <{ :optional start 0.0 dur 1.0 -- }>
    start now!
    now@ dur 440.0 10.0 1.0 4.0 bes-fm
    dur 0.2 f+ step
  ;

  include dsp
[else]
  : bes-fm-test <{ :optional start 0.0 dur 1.0 -- }>
  ;

  \ --- Hilbert transform

  : make-hilbert-transform ( len -- gen )
    doc" Makes a Hilbert transform filter."
    { len }
    len 2* 1+ { arrlen }
    arrlen 0.0 make-vct { arr }
    len even? if len else len 1+ then { lim }
    lim len negate ?do
      i len + { kk }
      i pi f* { denom }
      1.0  denom fcos  f- { num }
      num f0<> i 0<> || if
	arr kk  num denom f/  denom len f/ fcos 0.46 f* 0.54 f+  f*  vct-set! drop
      then
    loop
    arrlen arr make-fir-filter
  ;
  <'> fir-filter alias hilbert-transform
[then]

\ SSB-FM
\ ;;; this might be better named "quasi-ssb-fm" -- cancellations are not perfect
struct
  cell% field sbfm-am0
  cell% field sbfm-am1
  cell% field sbfm-car0
  cell% field sbfm-car1
  cell% field sbfm-mod0
  cell% field sbfm-mod1
end-struct sbfm%

: make-ssb-fm ( freq -- ssb )
  { freq }
  sbfm% %alloc { sbfm }
  freq 0.0     make-oscil   sbfm sbfm-am0 !
  freq half-pi make-oscil   sbfm sbfm-am1 !
  0.0 0.0      make-oscil   sbfm sbfm-car0 !
  0.0 half-pi  make-oscil   sbfm sbfm-car1 !
  40 make-hilbert-transform sbfm sbfm-mod0 !
  40 make-delay             sbfm sbfm-mod1 !
  sbfm
;
: ssb-fm ( gen modsig -- val )
  { gen modsig }
  gen sbfm-am0  @ 0.0 0.0 oscil
  gen sbfm-car0 @ gen sbfm-mod0 @ modsig hilbert-transform 0.0 oscil f*
  gen sbfm-am1  @ 0.0 0.0 oscil
  gen sbfm-car1 @ gen sbfm-mod1 @ modsig 0.0 delay 0.0 oscil f* f+
;

\ ;;; if all we want are asymmetric fm-generated spectra, we can just
\ ;;; add 2 fm oscil pairs:

struct
  cell% field fm2-os1
  cell% field fm2-os2
  cell% field fm2-os3
  cell% field fm2-os4
end-struct fm2%

: make-fm2 ( f1 f2 f3 f4 p1 p2 p3 p4 -- )
  { f1 f2 f3 f4 p1 p2 p3 p4 }
  fm2% %alloc { fm2 }
  f1 p1 make-oscil fm2 fm2-os1 !
  f2 p2 make-oscil fm2 fm2-os2 !
  f3 p3 make-oscil fm2 fm2-os3 !
  f4 p4 make-oscil fm2 fm2-os4 !
  fm2
;
: fm2 ( gen index -- val )
  { gen index }
  gen fm2-os1 @  gen fm2-os2 @ 0.0 0.0 oscil index f*  0.0 oscil
  gen fm2-os3 @  gen fm2-os4 @ 0.0 0.0 oscil index f*  0.0 oscil  f+ 0.25 f*
;

\ ;;; rms gain balance
\ ;;; This is a translation of the rmsgain code provided by Fabio Furlanete.

: make-rmsgain <{ :optional hp 10.0 -- gen }>
  doc" makes an RMS gain generator."
  2.0  two-pi mus-srate f/ hp f* fcos  f- { b }
  b  b b f* 1.0 f- fsqrt  f- { c2 }
  1.0 c2 f- { c1 }
  #()      :rmsg-c1   c1  array-assoc-set!
  ( rmsg ) :rmsg-c2   c2  array-assoc-set!
  ( rmsg ) :rmsg-q    0.0 array-assoc-set!
  ( rmsg ) :rmsg-r    0.0 array-assoc-set!
  ( rmsg ) :rmsg-avg  0.0 array-assoc-set!
  ( rmsg ) :rmsg-avgc 0   array-assoc-set!
  ( rmsg )
;
: rmsgain-rms ( gen sig -- val )
  doc" runs an RMS gain generator."
  { gen sig }
  gen :rmsg-c1 array-assoc-ref  sig f*  sig f*
  gen :rmsg-c2 array-assoc-ref  gen :rmsg-q array-assoc-ref  f*  f+
  dup gen :rmsg-q rot array-assoc-set! drop ( val ) fsqrt
;
: rmsgain-gain ( gen sig rmsval -- val )
  doc" returns the current RMS gain."
  { gen sig rmsval }
  gen :rmsg-c1 array-assoc-ref  sig f*  sig f*
  gen :rmsg-c2 array-assoc-ref  gen :rmsg-r array-assoc-ref  f*  f+
  dup ( val val ) gen :rmsg-r rot array-assoc-set! drop
  ( val ) f0= if rmsval else rmsval  gen :rmsg-r array-assoc-ref fsqrt  f/ then { this-gain }
  gen     :rmsg-avg array-assoc-ref this-gain f+ gen :rmsg-avg  rot array-assoc-set!
  ( gen ) :rmsg-avgc array-assoc-ref 1+          gen :rmsg-avgc rot array-assoc-set! drop
  sig this-gain f*
;
: rmsgain-balance ( gen sig comp -- val )
  doc" scales a signal based on a RMS gain."
  { gen sig comp }
  gen sig  gen comp rmsgain-rms  rmsgain-gain
;
: rmsgain-gain-avg ( gen -- val )
  doc" is part of the RMS gain stuff."
  { gen }
  gen :rmsg-avg array-assoc-ref  gen :rmsg-avgc array-assoc-ref f/
;
: rmsgain-balance-avg ( gen -- val )
  doc" is part of the RM gain stuff."
  :rmsg-avg array-assoc-ref
;

: clm-ins-test <{ :optional start 0.0 dur 1.0 }>
  start now!
  now@ dur       violin-test
  now@ dur    fm-violin-test
  now@ dur        pluck-test
  now@ dur          vox-test
  now@ dur       fofins-test
  now@ dur   fm-trumpet-test
  now@ dur      pqw-vox-test
  now@ dur        flute-test
  now@ dur      fm-bell-test
  now@ dur    fm-insect-test
  now@ dur      fm-drum-test
  now@ dur         gong-test
  now@ dur      attract-test
  now@ dur          pqw-test
  now@ dur     tubebell-test
  now@ dur       wurley-test
  now@ dur       rhodey-test
  now@ dur   hammondoid-test
  now@ dur        metal-test
  now@ dur drone/canter-test
  now@ dur        reson-test
  now@ dur       cellon-test
  now@ dur   gran-synth-test
  now@ dur   touch-tone-test
  now@ dur      spectra-test
  now@ dur      two-tab-test
  now@ dur    lbj-piano-test
  now@ dur       resflt-test
  now@ dur      scratch-test
  now@ dur         pins-test
  now@ dur           zc-test
  now@ dur           zn-test
  now@ dur           za-test
  now@ dur   clm-expsrc-test
  now@ dur      exp-snd-test
  now@ dur       expfil-test
  now@ dur     graph-eq-test
  now@ dur         anoi-test
  now@ dur      fullmix-test
  now@ dur       bes-fm-test
;

\ clm-ins.fs ends here
