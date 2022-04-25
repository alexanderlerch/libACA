#! /usr/bin/env fth
\ agn.fth -- Bill Schottstaedt's agn.cl
\     (see clm-2/clm-example.clm and clm-2/bess5.cl)

\ Translator/Author: Michael Scholz <mi-scholz@users.sourceforge.net>
\ Created: Wed Dec 15 23:30:43 CET 2004
\ Changed: Sat Jul 28 00:00:24 CEST 2012

\ Type do-agn
\ or start the script in a shell.

#t value *clm-c-version*

dl-load sndlib Init_sndlib
*clm-c-version* [if]
  dl-load sndins Init_sndins
[else]
  require clm-ins
[then]
require clm
require env

*argc* 2 > [if]
  *argv* 2 array-ref
[else]
  "agn.fsm"
[then] value agn-test-file
60.0 value agn-time

#t		to *clm-play*
#t		to *clm-statistics*
#t		to *clm-verbose*
44100		to *clm-srate*
2		to *clm-channels*
<'> jc-reverb	to *clm-reverb*
'( :volume 0.8 ) to *clm-reverb-data*
2		to *clm-reverb-channels*
#t		to *clm-delete-reverb*

: rbell ( x -- r ) 100 f* '( 0 0 10 0.25 90 1 100 1 ) 1.0 envelope-interp ;
: tune ( x -- r )
  { x }
  #( 1 256/243 9/8 32/27 81/64 4/3 1024/729 3/2 128/81 27/16 16/9 243/128 2 )
  x 12.0 fmod f>s array-ref
  2.0 x 12.0 f/ floor f**
  f*
;

#( 0 0 2 4 11 11 5 6 7 9 2 0 0 ) constant agn-mode
256 constant agn-lim

#f value agn-octs
#f value agn-pits
#f value agn-rhys
#f value agn-amps
#f value agn-begs

: agn-init ( -- )
  agn-lim make-array map!
    1.0 random rbell f2* 4.0 f+ floor
  end-map to agn-octs
  agn-lim make-array map!
    agn-mode 1.0 random 12.0 f* floor f>s array-ref
  end-map to agn-pits
  agn-lim make-array map!
    1.0 random 6.0 f* 4.0 f+
  end-map to agn-rhys
  agn-lim make-array map!
    1.0 random rbell 8.0 f* 1.0 f+
  end-map to agn-amps
  agn-lim make-array map!
    1.0 random 0.9 f< if 1.0 random f2* 4.0 f+ else 4.0 random 6.0 f* then
  end-map to agn-begs
;

: agn ( fname -- )
  ( fname ) io-open-write { io }
  io "\\ from agn.cl (see clm-2/clm-example.clm and clm-2/bess5.cl)\n" io-write
  io "\\\n" io-write
  io "%s\n" '( make-default-comment ) io-write-format
  #( '( 0 0 40 0.1 60 0.2 75 0.4 82 1 90 1 100 0 )
     '( 0 0 60 0.1 80 0.2 90 0.4 95 1 100 0 )
     '( 0 0 10 1 16 0 32 0.1 50 1 56 0 60 0 90 0.3 100 0 )
     '( 0 0 30 1 56 0 60 0 90 0.3 100 0 )
     '( 0 0 50 1 80 0.3 100 0 )
     '( 0 0 40 0.1 60 0.2 75 0.4 82 1 90 1 100 0 )
     '( 0 0 40 0.1 60 0.2 75 0.4 82 1 90 1 100 0 )
     '( 0 0 10 1 32 0.1 50 1 90 0.3 100 0 )
     '( 0 0 60 0.1 80 0.3 95 1 100 0 )
     '( 0 0 80 0.1 90 1 100 0 ) ) { wins }
  agn-init
  4 1 do
    0 4 0 { cellbeg cellsiz cellctr }
    1 i s>f i 1- s>f 0.2 { whichway base mi mytempo }
    0.0 0.0 { nextbeg beg }
    begin
      beg agn-time f< cellctr agn-lim < and
    while
	beg nextbeg f+ to beg
	0.25 mytempo 1.0 random 0.2 f* 0.9 f+ f* agn-rhys 
	  cellctr array-ref f* fmax to nextbeg
	  16.352 2.0 mi f** f/ agn-pits cellctr array-ref tune f*
	  2.0 agn-octs cellctr array-ref f** f* { freq }
	freq 100.0 f< if nextbeg f2* else nextbeg then { dur }
	0.003 agn-amps cellctr array-ref 60.0 base f* 1/f f* fmax { amp }
	1.0 random 2.0 f* base f* { ind }
	base 0.1 f* { revamt }
	10.0 beg beg floor f- f* floor f>s { winnum }
	0.00001 freq 2.0 flogn 4.0 f- 4.0 f** f* { ranamt }
	io
	"
%f %f %f %f :fm-index %f
  :amp-env %S
  :reverb-amount %f :noise-amount %f fm-violin"
	  '( beg dur freq amp ind wins winnum array-ref revamt ranamt )
	  io-write-format
	cellctr 1+ to cellctr
	cellctr cellsiz cellbeg + > if
	  cellbeg 1+ to cellbeg
	  1.0 random 0.5 f> if cellsiz whichway + to cellsiz then
	  cellsiz 16 > 1.0 random 0.99 f> and if
	    -2 to whichway
	  else
	    cellsiz 12 > 1.0 random 0.999 f> and if
	      -1 to whichway
	    else
	      cellsiz 4 < if
		1 to whichway
	      then
	    then
	  then
	  cellbeg 3 + to cellbeg
	  cellbeg to cellctr
	then
    repeat
  loop
  io "\n\n\\ %s ends here\n" '( agn-test-file ) io-write-format
  io io-close
;

: do-agn ( -- )
  agn-test-file undef file-basename ".snd" $+ { sndfile }
  "\\ writing \"%s\"\n" '( agn-test-file ) fth-print
  agn-test-file agn
  :output sndfile agn-test-file clm-load
;

'snd provided? [unless] do-agn [then]

\ agn.fth ends here
