\ clm.fs -- clm related base words, with-sound and friends -*- snd-forth -*-

\ Author: Michael Scholz <mi-scholz@users.sourceforge.net>
\ Created: Mon Mar 15 19:25:58 CET 2004
\ Changed: Fri Jun 26 23:04:01 CEST 2009

\ Commentary:
\
\ clm-print            ( fmt args -- )
\ clm-message          ( fmt args -- )
\ 
\ now@   	       ( -- secs )
\ now!   	       ( secs -- )
\ step                 ( secs -- )
\ tempo@ 	       ( -- secs )
\ tempo! 	       ( secs -- )
\ interval->hertz      ( n -- r )
\ keynum->hertz        ( n -- r )
\ hertz->keynum        ( n -- r )
\ bpm->seconds         ( bpm -- secs )
\ rhythm->seconds      ( rhy -- secs )
\ 
\ tempnam              ( -- name )
\ fth-tempnam          ( -- name )
\ make-default-comment ( -- str )
\ times->samples       ( start dur -- len beg )
\ normalize-partials   ( parts1 -- parts2 )
\ 
\ ws-local-variables   ( -- )
\ ws-interrupt?        ( -- )
\ ws-info              ( start dur -- )
\ run                  ( start dur -- )
\ run-instrument       ( start dur args -- )
\ end-run              ( sample -- )
\ reverb-info          ( caller in-chans out-chans -- )
\ instrument:          ( -- )
\ ;instrument          ( -- )
\ event:               ( -- )
\ ;event               ( -- )
\
\ find-file            ( file -- fname|#f )
\ snd-info             ( output :key reverb-file-name scaled? timer -- )
\ play-sound           ( input :key verbose dac-size audio-format -- )
\ record-sound         ( output keyword-args -- )
\
\ clm-mix              ( infile :key output output-frame frames input-frame scaler -- )
\ with-sound           ( body-xt keyword-args -- ws )
\ clm-load             ( fname keyword-args -- ws )
\ with-current-sound   ( :key offset scaled-to scaled-by -- )
\ scaled-to            ( body-xt scl -- )
\ scaled-by            ( body-xt scl -- )
\ with-offset          ( body-xt secs -- )
\ with-mix             ( body-str args fname beg -- )
\ sound-let            ( ws-xt-lst body-xt -- )

$" fth 26-Jun-2009" value *clm-version*

\ defined in snd/snd-xen.c
[ifundef] snd-print   : snd-print   ( str -- str )  dup .string ;             [then]
[ifundef] clm-print   : clm-print   ( fmt args -- ) format snd-print drop ;   [then]
[ifundef] clm-message : clm-message ( fmt args -- str ) ." \ " fth-print cr ; [then]

[ifundef] flog10
  <'> flog  alias flog10
  <'> fln   alias flog
  <'> flnp1 alias flogp1
[then]

dl-load sndlib Init_sndlib

'snd provided? [unless]
  <'> noop alias main-widgets
  <'> noop alias sounds
  <'> noop alias set-selected-sound
  <'> noop alias sound?
  <'> noop alias open-sound
  <'> noop alias find-sound
  <'> noop alias update-sound
  <'> noop alias save-sound
  <'> noop alias close-sound
  <'> noop alias close-sound-extend
  <'> noop alias channels
  <'> noop alias play
  <'> noop alias play-and-wait
  <'> noop alias maxamp
  <'> noop alias frames
  <'> noop alias scale-channel
  <'> noop alias snd-tempnam
  <'> noop alias snd-version
  : c-g? ( -- f ) #f ;
[then]

\ Also defined in examp.fs.
[ifundef] close-sound-extend
  \ 5 == notebook widget
  : close-sound-extend <{ snd -- }>
    main-widgets 5 array-ref false? unless
      0 { idx }
      sounds empty? unless sounds snd array-index to idx then
      snd close-sound drop
      sounds empty? unless
	sounds length 1 = if
	  sounds 0 array-ref
	else 
	  idx sounds length < if
	    sounds idx array-ref
	  else
	    sounds -1 array-ref
	  then
	then set-selected-sound drop
      then
  else
    snd close-sound drop
  then
;
[then]

\ === Notelist ===
hide
0.00 value *clm-current-time*
60.0 value *clm-tempo*
0.25 value *clm-beat*
set-current
: now@   ( -- secs ) *clm-current-time* ;
: now!   ( secs -- ) to *clm-current-time* ;
: step   ( secs -- ) now@ f+ now! ;
: tempo@ ( -- secs ) *clm-tempo* ;
: tempo! ( secs -- ) to *clm-tempo* ;
previous

\ --- Pitches ---
6.875 constant lowest-freq

: interval->hertz ( n -- r ) { n } 2.0 12.0 n 3.0 f+ f+ 12.0 f/ f** lowest-freq f* ;
: keynum->hertz   ( n -- r ) { n } 2.0 n 3.0 f+ 12.0 f/ f** lowest-freq f* ;
: hertz->keynum   ( r -- n ) lowest-freq f/ 2.0 flogn 12.0 f* 3.0 f- f>s ;

hide
: pitch ( interval octave "name" --; self -- freq )
  { interval octave }
  2.0 octave 1.0 f+ 12.0 f* interval 3.0 f+ f+ 12.0 f/ f** lowest-freq f*
  create ,
 does> ( self -- freq )
  @
;
set-current
 0 0 pitch |C0    1 0 pitch |Cs0    1 0 pitch |Df0
 2 0 pitch |D0    3 0 pitch |Ds0    3 0 pitch |Ef0
 4 0 pitch |E0    4 0 pitch |Ff0    5 0 pitch |Es0
 5 0 pitch |F0    6 0 pitch |Fs0    6 0 pitch |Gf0
 7 0 pitch |G0    8 0 pitch |Gs0    8 0 pitch |Af0
 9 0 pitch |A0   10 0 pitch |As0   10 0 pitch |Bf0
11 0 pitch |B0   11 0 pitch |Cf0   12 0 pitch |Bs0

 0 1 pitch |C1    1 1 pitch |Cs1    1 1 pitch |Df1
 2 1 pitch |D1    3 1 pitch |Ds1    3 1 pitch |Ef1
 4 1 pitch |E1    4 1 pitch |Ff1    5 1 pitch |Es1
 5 1 pitch |F1    6 1 pitch |Fs1    6 1 pitch |Gf1
 7 1 pitch |G1    8 1 pitch |Gs1    8 1 pitch |Af1
 9 1 pitch |A1   10 1 pitch |As1   10 1 pitch |Bf1
11 1 pitch |B1   11 1 pitch |Cf1   12 1 pitch |Bs1

 0 2 pitch |C2    1 2 pitch |Cs2    1 2 pitch |Df2
 2 2 pitch |D2    3 2 pitch |Ds2    3 2 pitch |Ef2
 4 2 pitch |E2    4 2 pitch |Ff2    5 2 pitch |Es2
 5 2 pitch |F2    6 2 pitch |Fs2    6 2 pitch |Gf2
 7 2 pitch |G2    8 2 pitch |Gs2    8 2 pitch |Af2
 9 2 pitch |A2   10 2 pitch |As2   10 2 pitch |Bf2
11 2 pitch |B2   11 2 pitch |Cf2   12 2 pitch |Bs2

 0 3 pitch |C3    1 3 pitch |Cs3    1 3 pitch |Df3
 2 3 pitch |D3    3 3 pitch |Ds3    3 3 pitch |Ef3
 4 3 pitch |E3    4 3 pitch |Ff3    5 3 pitch |Es3
 5 3 pitch |F3    6 3 pitch |Fs3    6 3 pitch |Gf3
 7 3 pitch |G3    8 3 pitch |Gs3    8 3 pitch |Af3
 9 3 pitch |A3   10 3 pitch |As3   10 3 pitch |Bf3
11 3 pitch |B3   11 3 pitch |Cf3   12 3 pitch |Bs3

 0 4 pitch |C4    1 4 pitch |Cs4    1 4 pitch |Df4
 2 4 pitch |D4    3 4 pitch |Ds4    3 4 pitch |Ef4
 4 4 pitch |E4    4 4 pitch |Ff4    5 4 pitch |Es4
 5 4 pitch |F4    6 4 pitch |Fs4    6 4 pitch |Gf4
 7 4 pitch |G4    8 4 pitch |Gs4    8 4 pitch |Af4
 9 4 pitch |A4   10 4 pitch |As4   10 4 pitch |Bf4
11 4 pitch |B4   11 4 pitch |Cf4   12 4 pitch |Bs4

 0 5 pitch |C5    1 5 pitch |Cs5    1 5 pitch |Df5
 2 5 pitch |D5    3 5 pitch |Ds5    3 5 pitch |Ef5
 4 5 pitch |E5    4 5 pitch |Ff5    5 5 pitch |Es5
 5 5 pitch |F5    6 5 pitch |Fs5    6 5 pitch |Gf5
 7 5 pitch |G5    8 5 pitch |Gs5    8 5 pitch |Af5
 9 5 pitch |A5   10 5 pitch |As5   10 5 pitch |Bf5
11 5 pitch |B5   11 5 pitch |Cf5   12 5 pitch |Bs5

 0 6 pitch |C6    1 6 pitch |Cs6    1 6 pitch |Df6
 2 6 pitch |D6    3 6 pitch |Ds6    3 6 pitch |Ef6
 4 6 pitch |E6    4 6 pitch |Ff6    5 6 pitch |Es6
 5 6 pitch |F6    6 6 pitch |Fs6    6 6 pitch |Gf6
 7 6 pitch |G6    8 6 pitch |Gs6    8 6 pitch |Af6
 9 6 pitch |A6   10 6 pitch |As6   10 6 pitch |Bf6
11 6 pitch |B6   11 6 pitch |Cf6   12 6 pitch |Bs6

 0 7 pitch |C7    1 7 pitch |Cs7    1 7 pitch |Df7
 2 7 pitch |D7    3 7 pitch |Ds7    3 7 pitch |Ef7
 4 7 pitch |E7    4 7 pitch |Ff7    5 7 pitch |Es7
 5 7 pitch |F7    6 7 pitch |Fs7    6 7 pitch |Gf7
 7 7 pitch |G7    8 7 pitch |Gs7    8 7 pitch |Af7
 9 7 pitch |A7   10 7 pitch |As7   10 7 pitch |Bf7
11 7 pitch |B7   11 7 pitch |Cf7   12 7 pitch |Bs7

 0 8 pitch |C8    1 8 pitch |Cs8    1 8 pitch |Df8
 2 8 pitch |D8    3 8 pitch |Ds8    3 8 pitch |Ef8
 4 8 pitch |E8    4 8 pitch |Ff8    5 8 pitch |Es8
 5 8 pitch |F8    6 8 pitch |Fs8    6 8 pitch |Gf8
 7 8 pitch |G8    8 8 pitch |Gs8    8 8 pitch |Af8
 9 8 pitch |A8   10 8 pitch |As8   10 8 pitch |Bf8
11 8 pitch |B8   11 8 pitch |Cf8   12 8 pitch |Bs8
previous

\ --- Note length ---
: bpm->seconds    ( bpm -- secs ) 60.0 swap f/ ;
: rhythm->seconds ( rhy -- secs ) 4.0 tempo@ bpm->seconds f* f* ;

: notelength ( scale "name" --; self -- r )
  rhythm->seconds create ,
 does> ( self -- r )
  @
;

 1.0     notelength |W			\ whole
 2.0 1/f notelength |H			\ half
 4.0 1/f notelength |Q			\ quarter
 8.0 1/f notelength |A			\ eighth
16.0 1/f notelength |S			\ sixteenth
32.0 1/f notelength |T			\ thirty-second
 1.0      2.0 1/f f+ notelength |W.
 2.0 1/f  4.0 1/f f+ notelength |H.
 4.0 1/f  8.0 1/f f+ notelength |Q.
 8.0 1/f 16.0 1/f f+ notelength |A.
16.0 1/f 32.0 1/f f+ notelength |S.

\ === Global User Variables (settable in ~/.snd_forth or ~/.fthrc) ===
#f 	      value *output*
#f 	      value *reverb*
#f 	      value *locsig*
mus-lshort    value *clm-audio-format*
#f            value *clm-comment*
1.0           value *clm-decay-time*
#f  	      value *clm-delete-reverb*
"test.snd"    value *clm-file-name*
#f 	      value *clm-notehook*
#f  	      value *clm-play*
#f            value *clm-player*           
#f 	      value *clm-reverb*
1     	      value *clm-reverb-channels*
#()           value *clm-reverb-data*
"test.reverb" value *clm-reverb-file-name*
#f  	      value *clm-statistics*
#f	      value *clm-verbose*
#f            value *clm-debug*
#()           value *clm-search-list* \ array of sound directories
#()           value *clm-instruments* \ array of arrays #( ins-name start dur local-vars )

'snd provided? [unless]
  1                 constant default-output-chans
  44100             constant default-output-srate
  mus-next          constant default-output-header-type
  mus-lfloat        constant default-output-data-format
  mus-audio-default constant audio-output-device
  512               constant dac-size
  0.0               constant clm-default-frequency
[then]

default-output-chans       value *clm-channels*
default-output-srate       value *clm-srate*
locsig-type                value *clm-locsig-type*
default-output-header-type value *clm-header-type*
default-output-data-format value *clm-data-format*
audio-output-device        value *clm-output-device*
dac-size                   value *clm-rt-bufsize*
mus-file-buffer-size       value *clm-file-buffer-size*
mus-clipping               value *clm-clipped*
mus-array-print-length     value *clm-array-print-length*
clm-table-size             value *clm-table-size*
clm-default-frequency      value *clm-default-frequency*

\ internal global variables
*clm-channels* value *channels*
*clm-verbose*  value *verbose*
*clm-notehook* value *notehook*

hide
user *fth-file-number*
set-current
: tempnam ( -- name )
  doc" Looks for environment variables TMP, TEMP, or TMPDIR, otherwise \
uses /tmp as temporary path and produces something like:\n\
/tmp/fth-12345-1.snd\n\
/tmp/fth-12345-2.snd\n\
/tmp/fth-12345-3.snd\n\
[...]"
  1 *fth-file-number* +!
  "%s/fth-%d-%d.snd"
  environ "TMP" array-assoc-ref ?dup-if
    1 object-ref
  else
    environ "TEMP" array-assoc-ref ?dup-if
      1 object-ref
    else
      environ "TMPDIR" array-assoc-ref ?dup-if
	1 object-ref
      else
	"/tmp"
      then
    then
  then ( tmp ) getpid *fth-file-number* @  3 >array string-format
;
previous

: fth-tempnam ( -- fname )
  'snd provided? if
    snd-tempnam
  else
    tempnam
  then
;

: make-default-comment ( -- str )
  $" Written %s by %s at %s using clm (%s)" _
  #( $" %a %d-%b-%y %H:%M %Z" current-time strftime
     getlogin
     gethostname
     *clm-version* ) string-format
;

: times->samples ( start dur -- limit begin )
  { start dur }
  start seconds->samples { beg }
  dur   seconds->samples { len }
  beg len b+ beg
;

: normalize-partials ( parts1 -- parts2 )
  { parts1 }
  0.0 ( sum ) parts1 object-length 1 ?do parts1 i object-ref fabs f+ 2 +loop
  dup f0= if
    $" all parts have 0.0 amplitude: %s" #( parts1 ) string-format warning
    ( sum ) drop parts1
  else
    ( sum ) 1/f { scl }
    parts1 map i 2 mod if *key* scl f* else *key* then end-map ( parts2 )
  then
;

\ === With-Sound Run-Instrument ===

$" with-sound error"     create-exception with-sound-error
$" with-sound interrupt" create-exception with-sound-interrupt
#() value *ws-args*			\ array for recursive with-sound calls 
#f value *clm-current-instrument*	\ current instrument set in INSTRUMENT:

: ws-local-variables ( -- )
  *clm-instruments* empty? if
    $" *clm-instruments* is empty" #() clm-message
  else
    nil { vals }
    "" #() clm-message
    *clm-instruments* each to vals
      $" === %s [%.3f-%.3f] ===" #( vals 0 array-ref vals 1 array-ref vals 2 array-ref ) clm-message
      vals 3 array-ref each ( var ) $" %s = %s" swap clm-message end-each
      "" #() clm-message
    end-each
  then
;
: ws-interrupt? ( -- )
  c-g? if
    'with-sound-interrupt #( "interrupted" ) fth-throw
  then
;
: ws-info ( start dur vars -- start dur )
  { start dur vars }
  *clm-instruments* #( *clm-current-instrument* start dur vars ) array-push to *clm-instruments*
  *notehook* dup xt? swap proc? || if
    *clm-current-instrument* start dur *notehook* dup proc? if proc->xt then execute stack-reset
  then
  ws-interrupt?
  start dur
;

hide
: (run)            ( start dur vars      -- limit begin ) ws-info times->samples ;
: (run-instrument) ( start dur args vars -- limit begin )
  { start dur args vars }
  args hash? unless #{} to args then
  :degree    args :degree   hash-ref         0.0 ||
  :distance  args :distance hash-ref         1.0 ||
  :reverb    args :reverb   hash-ref        0.05 ||
  :channels  args :channels hash-ref  *channels* ||
  :output    args :output   hash-ref    *output* ||
  :revout    args :revout   hash-ref    *reverb* ||
  :type      args :type     hash-ref locsig-type || make-locsig to *locsig*
  \ we set channel 3/4 if any to 0.5 * channel 1/2
  *output* mus-output? if
    *output* mus-channels 2 > if
      *locsig* 2  *locsig* 0 locsig-ref f2/  locsig-set! drop
      *output* mus-channels 3 > if
	*locsig* 3  *locsig* 1 locsig-ref f2/  locsig-set! drop
      then
    then
  then
  *reverb* mus-output? if
    *reverb* mus-channels 2 > if
      *locsig* 2  *locsig* 0 locsig-reverb-ref f2/  locsig-reverb-set! drop
      *reverb* mus-channels 3 > if
	*locsig* 3  *locsig* 1 locsig-reverb-ref f2/  locsig-reverb-set! drop
      then
    then
  then
  start dur vars (run)
;
: (end-run) { val idx -- } *locsig* idx val locsig drop ;
set-current
\ RUN/LOOP is only a simple replacement of
\ start dur TIMES->SAMPLES ?DO ... LOOP
\
\ RUN-INSTRUMENT/END-RUN requires at least an opened *output*
\ generator (file->sample), optional an opened *reverb* generator.  It
\ uses LOCSIG to set samples in output file.  At the end of the loop a
\ sample value must remain on top of stack!
\
\ instrument: foo
\   0 0.1 nil run-instrument 0.2 end-run
\ ;instrument
\ <'> foo with-sound
\
\ fills a sound file of length 0.1 seconds with 2205 samples (srate
\ 22050) with 0.2.
\ 
\ 0.0 1.0                   RUN            ... LOOP
\ 0.0 1.0 #{ :degree 45.0 } RUN-INSTRUMENT ... END-RUN
: run            ( start dur -- )
  postpone local-variables postpone (run)            postpone ?do
; immediate compile-only
: run-instrument ( start dur locsig-args -- )
  postpone local-variables postpone (run-instrument) postpone ?do
; immediate compile-only
: end-run        ( sample -- )
  postpone r@ postpone (end-run) postpone loop
; immediate compile-only
previous

: reverb-info ( caller in-chans out-chans -- )
  { caller in-chans out-chans }
  $" %s on %d in and %d out channels" #( caller in-chans out-chans ) clm-message
;

\ === Helper functions for instruments ===
hide
: ins-info   ( ins-name -- ) to *clm-current-instrument* ;
: event-info ( ev-name -- )  *clm-verbose* if #() clm-message else drop then ;
set-current
: instrument: ( -- )
  >in @ parse-word $>string { ins-name } >in !
  :
  ins-name postpone literal <'> ins-info   compile,
;
: event: ( -- )
  >in @ parse-word $>string { ev-name }  >in !
  :
  ev-name  postpone literal <'> event-info compile,
;
: ;instrument ( -- ) postpone ; ; immediate
<'> ;instrument alias ;event immediate
previous

\ === Playing and Recording Sound Files ===
: find-file ( file -- fname|#f )
  doc" Returns the possibly full path name of FILE if FILE exists or \
if FILE was found in *CLM-SEARCH-LIST*, otherwise returns #f."
  { file }
  file file-exists? if
    file
  else
    #f { fname }
    file string? *clm-search-list* array? && if
      *clm-search-list* each ( dir )
	"/" $+ file $+ dup file-exists? if to fname leave else drop then
      end-each
    then
    fname
  then
;

hide
: .maxamps ( fname name sr scl? -- )
  { fname name sr scl? }
  fname file-exists? if
    fname mus-sound-maxamp { vals }
    vals length 0 ?do
      $" %6s %c: %.3f (near %.3f secs)%s"
      #( name
	 [char] A i 2/ +
	 vals i 1+ array-ref
	 vals i    array-ref sr f/
	 scl? if $"  (before scaling)" else "" then ) clm-message
    2 +loop
  then
;
: .timer ( obj -- )
  { obj }
  $" %*s: %.3f  (utime %.3f, stime %.3f)"
  #( 8 $" real" obj real-time@ obj user-time@ obj system-time@ ) clm-message
;
: .timer-ratio ( srate frames obj -- )
  { sr frms obj }
  frms 0> if
    sr frms f/ { m }
    $" %*s: %.2f  (uratio %.2f)"
    #( 8 $" ratio" obj real-time@ m f* obj user-time@ m f* ) clm-message
  else
    $" %*s: no ratio" #( 8 $" ratio" ) clm-message
  then
;
set-current
: snd-info <{ output :key reverb-file-name #f scaled? #f timer #f -- }>
  output mus-sound-duration { dur }
  output mus-sound-frames   { frames }
  output mus-sound-chans    { channels }
  output mus-sound-srate    { srate }
  $" filename: %S"            #( output )             clm-message
  $"    chans: %d, srate: %d" #( channels srate f>s ) clm-message
  $"   format: %s [%s]"
  #( output mus-sound-data-format mus-data-format-name
     output mus-sound-header-type mus-header-type-name ) clm-message
  $"   length: %.3f  (%d frames)" #( dur frames ) clm-message
  timer timer? if
    timer .timer
    srate frames timer .timer-ratio
  then
  output $" maxamp" srate scaled? .maxamps
  reverb-file-name ?dup-if $" revamp" srate #f .maxamps then
  output mus-sound-comment { comm }
  comm empty? unless $"  comment: %S" #( comm ) clm-message then
;
previous

\ === Playing and Recording one or two Channel Sounds ===
: play-sound <{ input
     :key
     verbose      *clm-verbose*
     dac-size     *clm-rt-bufsize*
     audio-format *clm-audio-format* -- }>
  doc" Plays sound file INPUT.\n\
\"bell.snd\" :verbose #t play-sound"
  input find-file to input
  input false? if 'no-such-file #( get-func-name input ) fth-throw then
  input mus-sound-frames { frames }
  input mus-sound-srate  { srate }
  input mus-sound-chans  { chans }
  chans 2 > if
    $" %s: we can only handle 2 chans, not %d" _ #( get-func-name chans ) string-format warning
    2 to chans
  then
  verbose if input snd-info then
  dac-size frames min { bufsize }
  bufsize 0> if
    chans bufsize make-sound-data { data }
    input mus-sound-open-input { snd-fd }
    snd-fd 0< if 'forth-error #( get-func-name $" cannot open %s" _ input ) fth-throw then
    mus-audio-default srate chans 2 min audio-format bufsize mus-audio-open-output { dac-fd }
    dac-fd 0< if 'forth-error #( get-func-name $" cannot open dac" _ ) fth-throw then
    frames 0 ?do
      i bufsize + frames > if frames i - to bufsize then
      snd-fd 0 bufsize 1- chans data mus-sound-read drop
      dac-fd data bufsize mus-audio-write drop
    bufsize +loop
    snd-fd mus-sound-close-input drop
    dac-fd mus-audio-close drop
  else
    $" nothing to play for %S (%d frames)" #( input bufsize ) string-format warning
  then
;
: record-sound ( output keyword-args -- )
  <{ output :key
     duration      10.0
     verbose       *clm-verbose*
     output-device *clm-output-device*
     dac-size      *clm-rt-bufsize*
     srate         *clm-srate*
     channels      *clm-channels*
     audio-format  *clm-audio-format*
     data-format   *clm-data-format*
     header-type   *clm-header-type*
     comment       *clm-comment*     -- }>
  doc" Records from dac output device to the specified OUTPUT file."
  duration seconds->samples   	{ frames }
  dac-size frames min 	        { bufsize }
  channels 2 min      	        { chans }
  mus-srate                	{ old-srate }
  srate set-mus-srate drop
  comment empty? if $" written %s by %s" _ #( date get-func-name ) string-format to comment then
  chans bufsize make-sound-data { data }
  chans 0.25 make-vct           { vals }
  vals each drop mus-audio-mixer mus-audio-reclev i vals mus-audio-mixer-write drop end-each
  vals 0.75 vct-fill! drop
  vals each drop output-device  mus-audio-amp i vals mus-audio-mixer-write drop end-each
  output srate chans data-format header-type comment mus-sound-open-output { snd-fd }
  snd-fd 0< if 'forth-error #( get-func-name $" cannot open %S" _ output ) fth-throw then
  output-device srate chans audio-format bufsize mus-audio-open-input      { dac-fd }
  dac-fd 0< if 'forth-error #( get-func-name $" cannot open dac" _ ) fth-throw then
  verbose if
    $" filename: %s"                #( output )          clm-message
    $"   device: %d"                #( output-device )   clm-message
    $"    chans: %d, srate: %d"     #( chans srate )     clm-message
    $" r format: %s [Dac]" #( audio-format mus-data-format-name ) clm-message
    $" w format: %s [%s]"
    #( data-format mus-data-format-name header-type mus-header-type-name ) clm-message
    $"   length: %.3f  (%d frames)" #( duration frames ) clm-message
    $"  comment: %S"                #( comment )         clm-message
  then
  frames 0 ?do
    i bufsize + frames > if frames i - to bufsize then
    dac-fd data bufsize mus-audio-read drop
    snd-fd 0 bufsize 1- chans data mus-sound-write drop
  bufsize +loop
  dac-fd mus-audio-close drop
  snd-fd frames chans * data-format mus-bytes-per-sample * mus-sound-close-output drop
  old-srate set-mus-srate drop
;
: clm-mix <{ infile :key output #f output-frame 0 frames #f input-frame 0 scaler 1.0 -- }>
  doc" Mixes files in with-sound's *output* generator.\n\
\"oboe.snd\" clm-mix\n\
Mixes oboe.snd in *output* at *output*'s location 0 from oboe.snd's location 0 on.  \
The whole oboe.snd file will be mixed in because :frames is not specified."
  0  { chans }
  #f { mx }
  *output* mus-output? { outgen }
  output unless
    outgen if
      *output* mus-channels  to chans
      *output* mus-file-name to output
    else
      'with-sound-error $" %s: *output* gen or :output required" #( get-func-name ) fth-raise
    then
  then
  infile find-file to infile
  infile false? if 'file-not-found $" %s: cannot find %S" #( get-func-name infile ) fth-raise then
  frames infile mus-sound-frames || dup unless drop undef then to frames
  outgen if *output* mus-close drop then
  chans       0>
  scaler     f0<> &&
  scaler 1.0 f<>  && if
    save-stack { s }
    chans  chans dup * 0 ?do scaler loop make-mixer to mx
    s restore-stack
  then
  output        ( outfile )
  infile        ( infile )
  output-frame  ( outloc )
  frames        ( frames )
  input-frame   ( inloc )
  mx            ( mixer )
  #f            ( envs )    mus-mix drop
  outgen if output continue-sample->file to *output* then
;

hide
: ws-get-snd ( ws -- snd )
  { ws }
  ws :output array-assoc-ref find-file { fname }
  fname 0 find-sound dup sound? if ( snd ) save-sound then drop
  fname open-sound ( snd )
;
: ws-scaled-to ( ws -- )
  { ws }
  ws :scaled-to array-assoc-ref { scale }
  'snd provided? if
    ws ws-get-snd { snd }
    0.0  snd #t #f maxamp each fmax end-each { mx }
    mx f0<> if
      scale mx f/ to scale
      snd #f #f frames { len }
      ws :channels array-assoc-ref 0 ?do scale 0 len snd i ( chn ) #f scale-channel drop loop
    then
    snd save-sound drop
  else
    ws :output array-assoc-ref mus-sound-maxamp { smax }
    0.0 smax length 1 ?do smax i array-ref fabs fmax 2 +loop { mx }
    mx f0<> if
      ws :output array-assoc-ref :scaler scale mx f/ clm-mix
    then
  then
;
: ws-scaled-by ( ws -- )
  { ws }
  ws :scaled-by array-assoc-ref { scale }
  'snd provided? if
    ws ws-get-snd { snd }
    snd #f #f frames { len }
    ws :channels array-assoc-ref 0 ?do scale 0 len snd i ( chn ) #f scale-channel drop loop
    snd save-sound drop
  else
    ws :output array-assoc-ref :scaler scale clm-mix
  then
;
: ws-before-output ( ws -- )
  { ws }
  ws     :old-table-size         clm-table-size         	array-assoc-set!
  ( ws ) :old-file-buffer-size   mus-file-buffer-size   	array-assoc-set!
  ( ws ) :old-array-print-length mus-array-print-length 	array-assoc-set!
  ( ws ) :old-clipping           mus-clipping           	array-assoc-set!
  ( ws ) :old-srate       	 mus-srate        	    	array-assoc-set!
  ( ws ) :old-locsig-type 	 locsig-type      	    	array-assoc-set!
  ( ws ) :old-*output*    	 *output*         	    	array-assoc-set!
  ( ws ) :old-*reverb*    	 *reverb*         	    	array-assoc-set!
  ( ws ) :old-verbose     	 *verbose*        	    	array-assoc-set! 
  ( ws ) :old-debug       	 *clm-debug*      	    	array-assoc-set!
  ( ws ) :old-channels    	 *channels*       	    	array-assoc-set!
  ( ws ) :old-notehook    	 *notehook*       	    	array-assoc-set!
  ( ws ) :old-decay-time  	 *clm-decay-time* 	    	array-assoc-set! to ws
  ws :verbose                array-assoc-ref  		    	to *verbose*
  ws :debug                  array-assoc-ref  		    	to *clm-debug*
  ws :channels               array-assoc-ref  		    	to *channels*
  ws :notehook               array-assoc-ref  		    	to *notehook*
  ws :decay-time             array-assoc-ref  		    	to *clm-decay-time*
  *clm-table-size*           set-clm-table-size         drop
  *clm-file-buffer-size*     set-mus-file-buffer-size   drop
  *clm-array-print-length*   set-mus-array-print-length drop
  *clm-clipped* boolean? if *clm-clipped* else #f then set-mus-clipping drop
  ws :srate                  array-assoc-ref  		    	set-mus-srate   drop
  ws :locsig-type            array-assoc-ref  		    	set-locsig-type drop
;
: ws-after-output ( ws -- ws )
  { ws }
  ws :old-table-size         array-assoc-ref set-clm-table-size         drop
  ws :old-file-buffer-size   array-assoc-ref set-mus-file-buffer-size   drop
  ws :old-array-print-length array-assoc-ref set-mus-array-print-length drop
  ws :old-clipping           array-assoc-ref set-mus-clipping           drop
  ws :old-srate       	     array-assoc-ref set-mus-srate              drop
  ws :old-locsig-type 	     array-assoc-ref set-locsig-type            drop
  ws :old-*output*    	     array-assoc-ref 				 to *output*
  ws :old-*reverb*    	     array-assoc-ref 				 to *reverb*
  ws :old-verbose     	     array-assoc-ref 				 to *verbose*
  ws :old-debug       	     array-assoc-ref 				 to *clm-debug*
  ws :old-channels    	     array-assoc-ref 				 to *channels*
  ws :old-notehook    	     array-assoc-ref 				 to *notehook*
  ws :old-decay-time  	     array-assoc-ref 				 to *clm-decay-time*
  *ws-args* array-pop
;
: ws-statistics ( ws -- )
  { ws }
  ws :output array-assoc-ref
  :reverb-file-name ws :reverb-file-name  array-assoc-ref
  :scaled?          ws :scaled-to array-assoc-ref ws :scaled-by array-assoc-ref ||
  :timer            ws :timer             array-assoc-ref
  snd-info
;
\ player can be one of xt, proc, string, or #f.
\
\       xt: output player execute
\     proc: player #( output ) run-proc
\   string: "player output" system
\ else snd: output play-and-wait
\      clm: output play-sound
\ 
\ A player may look like this:
\
\ : play-3-times ( output -- )
\   { output }
\   3 0 ?do output play-and-wait drop loop
\ ;
\ <'> play-3-times to *clm-player*
: ws-play-it ( ws -- )
  { ws }
  ws :output array-assoc-ref { output }
  ws :player array-assoc-ref { player }
  player proc? if
    player #( output ) run-proc drop
  else
    player string? if
      $" %s %s" #( player output ) string-format file-shell drop
    else
      'snd provided? if
	output find-file play-and-wait drop
      else
	output :verbose #f play-sound
      then
    then
  then
;
: set-args ( key def ws -- )
  { key def ws }
  key def get-optkey ws key rot array-assoc-set! to ws
;
set-current
: with-sound-default-args ( keyword-args -- ws )
  #() to *clm-instruments*
  #() { ws }
  *ws-args* ws array-push to *ws-args*
  :play              *clm-play*             ws set-args
  :statistics        *clm-statistics*       ws set-args
  :verbose           *clm-verbose*          ws set-args
  :debug             *clm-debug*            ws set-args
  :continue-old-file #f                     ws set-args
  :output            *clm-file-name*        ws set-args
  :channels          *clm-channels*         ws set-args
  :srate             *clm-srate*            ws set-args
  :locsig-type       *clm-locsig-type*      ws set-args
  :header-type       *clm-header-type*      ws set-args
  :data-format       *clm-data-format*      ws set-args
  :comment           *clm-comment*          ws set-args
  :notehook          *clm-notehook*         ws set-args
  :scaled-to         #f                     ws set-args
  :scaled-by         #f                     ws set-args
  :delete-reverb     *clm-delete-reverb*    ws set-args
  :reverb            *clm-reverb*           ws set-args
  :reverb-data       *clm-reverb-data*      ws set-args
  :reverb-channels   *clm-reverb-channels*  ws set-args
  :reverb-file-name  *clm-reverb-file-name* ws set-args
  :player            *clm-player*           ws set-args
  :decay-time        *clm-decay-time*       ws set-args
  ws
;  
: with-sound-args ( keyword-args -- ws )
  #() { ws }
  *ws-args* -1 array-ref { ws1 }
  *ws-args* ws array-push to *ws-args*
  :play              #f                        	    ws set-args
  :player            #f                        	    ws set-args
  :statistics        #f                        	    ws set-args
  :continue-old-file #f               	            ws set-args
  :verbose           ws1 :verbose     	   array-assoc-ref ws set-args
  :debug             ws1 :debug       	   array-assoc-ref ws set-args
  :output            ws1 :output      	   array-assoc-ref ws set-args
  :channels          ws1 :channels    	   array-assoc-ref ws set-args
  :srate             ws1 :srate       	   array-assoc-ref ws set-args
  :locsig-type       ws1 :locsig-type 	   array-assoc-ref ws set-args
  :header-type       ws1 :header-type 	   array-assoc-ref ws set-args
  :data-format       ws1 :data-format 	   array-assoc-ref ws set-args
  :comment $" with-sound level %d" #( *ws-args* length ) string-format ws set-args
  :notehook          ws1 :notehook         array-assoc-ref ws set-args
  :scaled-to         ws1 :scaled-to        array-assoc-ref ws set-args
  :scaled-by         ws1 :scaled-by        array-assoc-ref ws set-args
  :delete-reverb     ws1 :delete-reverb    array-assoc-ref ws set-args
  :reverb            ws1 :reverb           array-assoc-ref ws set-args
  :reverb-data       ws1 :reverb-data      array-assoc-ref ws set-args
  :reverb-channels   ws1 :reverb-channels  array-assoc-ref ws set-args
  :reverb-file-name  ws1 :reverb-file-name array-assoc-ref ws set-args
  :decay-time        ws1 :decay-time       array-assoc-ref ws set-args
  ws
;
: with-sound-main ( body-xt ws -- ws )
  { body-xt ws }
  body-xt xt? body-xt proc? || body-xt 1 $" a proc or xt"         assert-type
  ws array?                    ws      2 $" an associative array" assert-type
  ws ws-before-output
  ws :reverb array-assoc-ref { reverb-xt }
  reverb-xt if
    reverb-xt xt? reverb-xt proc? || reverb-xt 3 $" a proc or xt" assert-type
    #t
  else
    #f
  then { rev? }
  ws :output            array-assoc-ref { output }
  ws :reverb-file-name  array-assoc-ref { revput }
  ws :continue-old-file array-assoc-ref { cont? }
  cont? if
    output continue-sample->file
  else
    output file-delete
    output
    ws :channels    array-assoc-ref
    ws :data-format array-assoc-ref
    ws :header-type array-assoc-ref
    ws :comment array-assoc-ref dup empty? if drop make-default-comment then
    make-sample->file
  then to *output*
  *output* sample->file? unless
    'with-sound-error #( get-func-name $" cannot open sample->file" _ ) fth-throw
  then
  cont? if
    output mus-sound-srate set-mus-srate drop
    'snd provided? if output 0 find-sound dup sound? if close-sound-extend else drop then then
  then
  rev? if
    cont? if
      revput continue-sample->file
    else
      revput file-delete
      revput
      ws :reverb-channels array-assoc-ref
      ws :data-format     array-assoc-ref
      ws :header-type     array-assoc-ref
      $" with-sound temporary reverb file" make-sample->file
    then to *reverb*
    *reverb* sample->file? unless
      'with-sound-error #( get-func-name $" cannot open reverb sample->file" _ ) fth-throw
    then
  then
  ws :timer make-timer array-assoc-set! to ws
  \ compute ws body
  *clm-debug* if
    \ EXECUTE provides probably a more precise backtrace than FTH-CATCH.
    body-xt dup proc? if proc->xt then execute
  else
    body-xt 'with-sound-interrupt #t fth-catch if
      stack-reset
      *output* mus-close drop
      *reverb* if *reverb* mus-close drop then
      $" body-xt interrupted by C-g" #() clm-message
      ws ws-after-output ( ws )
      exit
    then
  then
  reverb-xt if
    *reverb* mus-close drop
    ws :reverb-file-name array-assoc-ref undef make-file->sample to *reverb*
    *reverb* file->sample? unless
      'with-sound-error #( get-func-name $" cannot open file->sample" _ ) fth-throw
    then
    \ compute ws reverb
    *clm-debug* if
      \ push reverb arguments on stack
      ws :reverb-data array-assoc-ref each end-each reverb-xt dup proc? if proc->xt then execute
    else
      reverb-xt 'with-sound-interrupt #t fth-catch if
	stack-reset
	*output* mus-close drop
	*reverb* mus-close drop
	$" reverb-xt interrupted by C-g" #() clm-message
	ws ws-after-output ( ws )
	exit
      then
    then
    *reverb* mus-close drop
  then
  *output* mus-close drop
  ws :timer array-assoc-ref stop-timer
  ws ws-get-snd drop
  ws :statistics    array-assoc-ref              if ws ws-statistics then
  ws :delete-reverb array-assoc-ref reverb-xt && if ws :reverb-file-name array-assoc-ref file-delete then
  ws :scaled-to     array-assoc-ref   	     	  if ws ws-scaled-to  then
  ws :scaled-by     array-assoc-ref   	     	  if ws ws-scaled-by  then
  ws :play          array-assoc-ref   	     	  if ws ws-play-it    then
  ws ws-after-output ( ws )
;
previous

\ Usage: <'> resflt-test with-sound drop
\        <'> resflt-test :play #f :channels 2 with-sound .g
\        lambda: resflt-test ; :output "resflt.snd" with-sound drop
: with-sound ( body-xt keyword-args -- ws )
  doc" \\ keywords and default values:\n\
:play              *clm-play*             (#f)\n\
:statistics        *clm-statistics*       (#f)\n\
:verbose           *clm-verbose*          (#f)\n\
:debug             *clm-debug*            (#f)\n\
:continue-old-file                        (#f)\n\
:output            *clm-file-name*        (\"test.snd\")\n\
:channels          *clm-channels*         (1)\n\
:srate             *clm-srate*            (44100)\n\
:locsig-type       *clm-locsig-type*      (mus-interp-linear)\n\
:header-type       *clm-header-type*      (mus-next)\n\
:data-format       *clm-data-format*      (mus-lfloat)\n\
:comment           *clm-comment*          (#f)\n\
:notehook          *clm-notehook*         (#f)\n\
:scaled-to                                (#f)\n\  
:scaled-by                                (#f)\n\  
:delete-reverb     *clm-delete-reverb*    (#f)\n\
:reverb            *clm-reverb*           (#f)\n\
:reverb-data       *clm-reverb-data*      (#())\n\
:reverb-channels   *clm-reverb-channels*  (1)\n\
:reverb-file-name  *clm-reverb-file-name* (\"test.reverb\")\n\
:player            *clm-player*           (#f)\n\
:decay-time        *clm-decay-time*       (1.0)\n\
Executes BODY-XT, a proc object or an xt, and returns an assoc array with with-sound arguments.\n\
<'> resflt-test with-sound .g cr\n\
<'> resflt-test :play #t :channels 2 :srate 44100 with-sound drop"
  *ws-args* empty? if
    with-sound-default-args
  else
    with-sound-args
  then ( ws )
  with-sound-main ( ws )
;
: clm-load ( fname keyword-args -- ws )
  doc" Loads and evals the CLM instrument call file FNAME.  \
See with-sound for a full keyword list.\n\
\"test.fsm\" :play #t :player \"sndplay\" clm-load drop"
  *ws-args* empty? if
    with-sound-default-args
  else
    with-sound-args
  then
  { fname ws }
  fname file-exists? if
    ws :verbose array-assoc-ref if $" loading %S" _ #( fname ) clm-message then
    fname <'> file-eval ws with-sound-main ( ws )
  else
    'no-such-file $" %s: %S not found" #( get-func-name fname ) fth-raise
  then
;

: with-current-sound <{ body-xt :key offset 0.0 scaled-to #f scaled-by #f -- }>
  doc" Must be called within with-sound body.  \
Takes all arguments from current with-sound except :output, :scaled-to, :scaled-by and :comment."
  *output* mus-output? false? if
    'with-sound-error $" %s can only be called within with-sound" #( get-func-name ) fth-raise
  then
  with-sound-args { ws }
  fth-tempnam { output }
  ws     :output    output    array-assoc-set!
  ( ws ) :scaled-to scaled-to array-assoc-set!
  ( ws ) :scaled-by scaled-by array-assoc-set! to ws
  body-xt ws with-sound-main drop
  output :output-frame offset seconds->samples clm-mix
  output file-delete
;
: scaled-to <{ body-xt scl -- }>
  doc" Must be called within with-sound body.  \
Scales BODY-XT's resulting sound file to SCL.\n\
lambda: ( -- )\n\
  0.0 0.1 660.0 0.5 fm-violin\n\
  0.5 0.1 550.0 0.1 <'> fm-violin 0.8 scaled-to ( scaled to 0.8 )\n\
; with-sound"
  body-xt :scaled-to scl with-current-sound
;
: scaled-by <{ body-xt scl -- }>
  doc" Must be called within with-sound body.  \
Scales BODY-XT's resulting sound file by SCL.\n\
lambda: ( -- )\n\
  0.0 0.1 660.0 0.5 fm-violin\n\
  0.5 0.1 550.0 0.1 <'> fm-violin 2.0 scaled-by ( scaled to 0.2 )\n\
; with-sound"
  body-xt :scaled-by scl with-current-sound
;
: with-offset <{ body-xt sec -- }>
  doc" Must be called within with-sound body.  \
Mixes BODY-XT's resulting sound file into main sound file at SEC seconds.\n\
lambda: ( -- )\n\
  0.0 0.1 660.0 0.5 fm-violin\n\
  0.5 0.1 550.0 0.1 <'> fm-violin 1.0 with-offset ( its actual begin time is 1.5 )\n\
; with-sound"
  body-xt :offset sec with-current-sound
;

: with-mix <{ body-str args fname start -- }>
  doc" BODY-STR is a string with with-sound commands, \
ARGS is an array of with-sound arguments, \
FNAME is the temporary mix file name without extension, \
and START is the begin time for mix in.\n\
lambda: ( -- )\n\
  0.0 0.1 440 0.1 fm-violin\n\
  \"\n\
  0.0 0.1 550 0.1 fm-violin\n\
  0.1 0.1 660 0.1 fm-violin\n\
  \" #() \"sec1\" 0.5 with-mix\n\
  \"\n\
  0.0 0.1  880 0.1 :reverb-amount 0.2 fm-violin\n\
  0.1 0.1 1320 0.1 :reverb-amount 0.2 fm-violin\n\
  \" #( :reverb <'> jc-reverb ) \"sec2\" 1.0 with-mix\n\
  2.0 0.1 220 0.1 fm-violin\n\
  ; with-sound drop"
  body-str string? body-str 1 $" a string" assert-type
  args     array?  args     2 $" an array" assert-type
  fname    string? fname    3 $" a string" assert-type
  start    number? start    4 $" a number" assert-type
  *output* mus-output? false? if
    'with-sound-error $" %s can only be called within with-sound" #( get-func-name ) fth-raise
  then
  fname ".snd" $+ { snd-file }
  fname ".fsm" $+ { mix-file }
  snd-file file-exists? if
    snd-file file-mtime
  else
    #f
  then { snd-time }
  mix-file file-exists? if
    mix-file readlines "" array-join
  else
    ""
  then ( old-body ) body-str string= if
    mix-file file-mtime
  else
    mix-file #( body-str ) writelines
    #f
  then { mix-time }
  snd-time false?
  mix-time false? ||
  snd-time mix-time b< || if
    mix-file args each end-each :output snd-file clm-load drop
  then
  snd-file :output-frame start seconds->samples clm-mix
;

: sound-let ( ws-xt-lst body-xt -- )
  doc" Requires an array of arrays WS-XT-LST with with-sound args and xts, and a BODY-XT.  \
The BODY-XT must take WS-XT-LST length arguments which are tempfile names.  \
with-sound will be feed with ws-args und ws-xts from WS-XT-LST.  \
:output is set to tempnam which will be on stack before executing BODY-XT.  \
These temporary files will be deleted after execution of BODY-XT.\n\
#( #( #( :reverb <'> jc-reverb ) 0.0 1 220 0.2 <'> fm-violin )\n\
   #( #()                      0.5 1 440 0.3 <'> fm-violin ) ) ( the ws-xt-lst )\n\
lambda: { tmp1 tmp2 }\n\
  tmp1 :output tmp2 clm-mix\n\
  tmp1 clm-mix\n\
; ( the body-xt ) <'> sound-let with-sound drop"
  { ws-xt-lst body-xt }
  ws-xt-lst array?             ws-xt-lst 1 $" an array"       assert-type
  body-xt xt? body-xt proc? ||   body-xt 2 $" a proc or xt" assert-type
  *output* mus-output? false? if
    'with-sound-error $" %s can only be called within with-sound" #( get-func-name ) fth-raise
  then
  ws-xt-lst map
    *key* 0 array-ref ( args ) each end-each with-sound-args
    ( ws ) :output fth-tempnam array-assoc-set! { ws }
    *key* 1 array-ref ( xt ) each end-each ws with-sound-main :output array-assoc-ref ( outfile )
  end-map { outfiles }
  body-xt xt? if
    outfiles each end-each body-xt execute
  else
    body-xt outfiles run-proc drop
  then
  outfiles each file-delete end-each
;

\ === example instruments, more in clm-ins.fs ===

instrument: simp ( start dur freq amp -- )
  { start dur freq amp }
  :frequency freq make-oscil { os }
  :envelope #( 0e 0e 25e 1e 75e 1e 100e 0e ) :duration dur :scaler amp make-env { en }
  start dur run
    i  os 0.0 0.0 oscil en env f*  *output* outa drop
  loop
;instrument

: run-test ( -- ) 0.0 1.0 330.0 0.5 simp ;

: input-fn ( gen -- proc; dir self -- r )
  1 proc-create swap ,
 does> ( dir self -- r )
  nip @ readin
;

instrument: src-simp ( start dur amp sr sr-env fname -- )
  { start dur amp sr sr-env fname }
  :file fname find-file make-readin { f }
  :input f input-fn :srate sr make-src { sc }
  :envelope sr-env :duration dur make-env { en }
  start dur run
    i  sc en env #f src amp f*  *output* outa drop
  loop
  f mus-close drop
;instrument

instrument: conv-simp ( start dur filt fname amp -- )
  { start dur filt fname amp }
  :file fname find-file make-readin { f }
  filt string? if
    8192 0.0 make-vct { v }
    filt find-file 0 0 v length v file->array
  else
    filt
  then { data }
  :input f input-fn :filter data make-convolve { cv }
  start dur run
    i cv #f convolve  amp f*  *output* outa drop
  loop
  f mus-close drop
;instrument

\ <'> src-test with-sound drop
event: src-test ( -- )
  0.0 1.0 1.0 0.2 #( 0e 0e 50e 1e 100e 0e ) $" oboe.snd" src-simp
;event

\ <'> conv1-test with-sound drop
event: conv1-test ( -- )
  0.0 1.0 vct( 0.5 0.2 0.1 0.05 0e 0e 0e 0e ) $" fyow.snd" 1.0 conv-simp
;event

\ <'> conc2-test with-sound drop
event: conv2-test ( -- )
  0.0 1.0 $" pistol.snd" $" fyow.snd" 0.2 conv-simp
;event

\ <'> inst-test with-sound drop
event: inst-test ( -- )
  0.0 1.0 1.0 0.2 #( 0 0 50 1 100 0 ) $" oboe.snd" src-simp
  1.2 1.0 vct( 0.5 0.2 0.1 0.05 0 0 0 0 ) $" fyow.snd" 1.0 conv-simp
  2.4 1.0 $" pistol.snd" $" fyow.snd" 0.2 conv-simp
;event

\ waveshape removed from clm.c
#f 'snd provided? && [if]
  instrument: arpeggio <{ start dur freq amp :key ampenv #( 0 0 0.5 1 1 0 ) offset 1.0 -- }>
    start dur times->samples { end beg }
    12 make-array map!
      :frequency freq offset i 6 - 0.03 f* f* f+
      :partials #( 1 1  5 0.7  6 0.7  7 0.7  8 0.7  9 0.7  10 0.7 ) make-waveshape
    end-map { waveshbank }
    :envelope ampenv :scaler amp 0.25 f* :length end make-env { amp-env }
    end 0.0 make-vct map!
      0.0 ( wvsum ) waveshbank each ( wv ) 1.0 0.0 waveshape f+ end-each ( wvsum ) amp-env env f*
    end-map ( output )
    #f channels 0 ?do ( output ) beg end #f i #f undef vct->channel ( output ) loop ( output ) drop
  ;instrument

  event: arpeggio-test ( -- )
    :file "arpeggio.snd"
    :header-type mus-next
    :data-format mus-lfloat
    :channels 2
    :srate 22050 new-sound { snd }
    0 10 65 0.5 arpeggio
    snd save-sound drop
    0 snd play drop
    snd close-sound drop
  ;event
[then]

\ clm.fs ends here
