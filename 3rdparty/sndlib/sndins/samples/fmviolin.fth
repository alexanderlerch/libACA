#! /usr/bin/env fth
\ fmviolin.fth -- CLM fmviolin.clm

\ Translator/Author: Michael Scholz <mi-scholz@users.sourceforge.net>
\ Created: Mon Dec 06 17:23:22 CET 2004
\ Changed: Sat Jul 28 03:13:10 CEST 2012

\ Commentary:

\ A translation of Bill Schottstaedt's clm/fmviolin.clm from Lisp
\ into Fth.

\ short-example
\ long-example

\ Code:

#t value *clm-c-version*

dl-load sndlib Init_sndlib
*clm-c-version* [if]
  dl-load sndins Init_sndins
[else]
  require clm-ins
  <'> noop alias freeverb
[then]
require clm

"test-ins-f.snd" to *clm-file-name*
#t	to *clm-play*
#t	to *clm-statistics*
#t	to *clm-verbose*
44100	to *clm-srate*
2	to *clm-channels*
2	to *clm-reverb-channels*
#t	to *clm-delete-reverb*

1.0	value fmv-fm-index                   
'( 0 0 25 1 75 1 100 0 ) value fmv-amp-env                    
5.0	value fmv-periodic-vibrato-rate      
0.0025	value fmv-periodic-vibrato-amp
16.0	value fmv-random-vibrato-rate        
0.005	value fmv-random-vibrato-amp
1000.0	value fmv-noise-freq                 
0.0	value fmv-noise-amount               
10.0	value fmv-ind-noise-freq             
0.0	value fmv-ind-noise-amount           
20.0	value fmv-amp-noise-freq             
0.0	value fmv-amp-noise-amount           
'( 0 0 100 0 ) value fmv-gliss-env                  
0.0	value fmv-glissando-amount           
'( 0 1 25 0.4 75 0.6 100 0 ) value fmv-fm1-env                    
'( 0 1 25 0.4 75 0.6 100 0 ) value fmv-fm2-env                    
'( 0 1 25 0.4 75 0.6 100 0 ) value fmv-fm3-env                    
1.0	value fmv-fm1-rat                    
3.0	value fmv-fm2-rat                    
4.0	value fmv-fm3-rat                    
#f	value fmv-fm1-index                  
#f	value fmv-fm2-index                  
#f	value fmv-fm3-index                  
0.0	value fmv-base                       
0.01	value fmv-reverb-amount              
0.0	value fmv-degree                     
1.0	value fmv-distance                   
'violin	value fmv-index-type                 

: restore-fm-violin-defaults ( -- )
  1.0                          to fmv-fm-index                   
  '( 0 0 25 1 75 1 100 0 )     to fmv-amp-env                    
  5.0			       to fmv-periodic-vibrato-rate      
  0.0025		       to fmv-periodic-vibrato-amp
  16.0		               to fmv-random-vibrato-rate        
  0.005			       to fmv-random-vibrato-amp
  1000.0		       to fmv-noise-freq                 
  0.0			       to fmv-noise-amount               
  10.0			       to fmv-ind-noise-freq             
  0.0			       to fmv-ind-noise-amount           
  20.0			       to fmv-amp-noise-freq             
  0.0			       to fmv-amp-noise-amount           
  '( 0 0 100 0 )	       to fmv-gliss-env                  
  0.0			       to fmv-glissando-amount           
  '( 0 1 25 0.4 75 0.6 100 0 ) to fmv-fm1-env                    
  '( 0 1 25 0.4 75 0.6 100 0 ) to fmv-fm2-env                    
  '( 0 1 25 0.4 75 0.6 100 0 ) to fmv-fm3-env                    
  1.0			       to fmv-fm1-rat                    
  3.0			       to fmv-fm2-rat                    
  4.0			       to fmv-fm3-rat                    
  #f			       to fmv-fm1-index                  
  #f			       to fmv-fm2-index                  
  #f			       to fmv-fm3-index                  
  0.0			       to fmv-base                       
  0.0			       to fmv-degree                     
  1.0			       to fmv-distance                   
  0.01			       to fmv-reverb-amount              
  'violin		       to fmv-index-type                 
;

: old-fm-violin ( start dur freq amp keyword-args -- )
  :fm-index         fmv-fm-index              get-args { fm-index }
  :amp-env          fmv-amp-env               get-args { amp-env }
  :periodic-vibrato-rate      fmv-periodic-vibrato-rate get-args { pvrate }
  :periodic-vibrato-amplitude fmv-periodic-vibrato-amp  get-args { pvamp }
  :random-vibrato-rate        fmv-random-vibrato-rate   get-args { rvrate }
  :random-vibrato-amplitude   fmv-random-vibrato-amp    get-args { rvamp }
  :noise-freq       fmv-noise-freq            get-args { noise-freq }
  :noise-amount     fmv-noise-amount          get-args { noise-amount }
  :ind-noise-freq   fmv-ind-noise-freq        get-args { ind-noise-freq }
  :ind-noise-amount fmv-ind-noise-amount      get-args { ind-noise-amount }
  :amp-noise-freq   fmv-amp-noise-freq        get-args { amp-noise-freq }
  :amp-noise-amount fmv-amp-noise-amount      get-args { amp-noise-amount }
  :gliss-env        fmv-gliss-env             get-args { gliss-env }
  :glissando-amount fmv-glissando-amount      get-args { gliss-amount }
  :fm1-env          fmv-fm1-env               get-args { fm1-env }
  :fm2-env          fmv-fm2-env               get-args { fm2-env }
  :fm3-env          fmv-fm3-env               get-args { fm3-env }
  :fm1-rat          fmv-fm1-rat               get-args { fm1-rat }
  :fm2-rat          fmv-fm2-rat               get-args { fm2-rat }
  :fm3-rat          fmv-fm3-rat               get-args { fm3-rat }
  :fm1-index        fmv-fm1-index             get-args { fm1-index }
  :fm2-index        fmv-fm2-index             get-args { fm2-index }
  :fm3-index        fmv-fm3-index             get-args { fm3-index }
  :base             fmv-base                  get-args { base }
  :degree           fmv-degree                get-args { degree }
  :distance         fmv-distance              get-args { distance }
  :reverb-amount    fmv-reverb-amount         get-args { revamount }
  :index-type       fmv-index-type            get-args { index-type }
  { start dur freq amp }
  start dur freq amp 0.125 f*
  :fm-index                   fm-index
  :amp-env                    amp-env
  :periodic-vibrato-rate      pvrate
  :periodic-vibrato-amplitude pvamp
  :random-vibrato-rate        rvrate
  :random-vibrato-amplitude   rvamp
  :noise-freq                 noise-freq
  :noise-amount               noise-amount
  :ind-noise-freq             ind-noise-freq
  :ind-noise-amount           ind-noise-amount
  :amp-noise-freq             amp-noise-freq
  :amp-noise-amount           amp-noise-amount
  :gliss-env                  gliss-env
  :glissando-amount           gliss-amount
  :fm1-env                    fm1-env
  :fm2-env                    fm2-env
  :fm3-env                    fm3-env
  :fm1-rat                    fm1-rat
  :fm2-rat                    fm2-rat
  :fm3-rat                    fm3-rat
  :fm1-index                  fm1-index
  :fm2-index                  fm2-index
  :fm3-index                  fm3-index
  :base                       base
  :degree                     degree
  :distance                   distance
  :reverb-amount              revamount
  :index-type                 index-type   fm-violin
;

: vln-one-sin ( start dur freq amp keyword-args -- )
  :degree       0 get-args drop
  :noise-amount 0 get-args drop
  :degree 90.0 random :noise-amount 0.0 old-fm-violin
;

: vln-one-sin-ran ( start dur freq amp keyword-args -- )
  :degree 0 get-args drop
  :degree 90.0 random old-fm-violin
;

: vln-one-sin-exp ( start dur freq amp keyword-args -- )
  :base 0 get-args drop
  :base 0.03125 vln-one-sin
;

: cel-one-sum ( start dur freq amp keyword-args -- )
  :degree     0 get-args drop
  :index-type 0 get-args drop
  :degree 90.0 random :index-type 'cello old-fm-violin
;

: violin-new ( start dur freq amp keyword-args -- )
  :fm-index      1.0                  get-args { fm-index }
  :amp-env       '( 0 0 221 1 240 0 ) get-args { amp-env }
  :reverb-amount 0.2                  get-args { rev-amt }
  { start dur freq amp }
  6 0 do
    1.0 random 0.05 f* i f* start f+	\ start
    1.0 random 0.10 f* i f* dur   f+	\ dur
    freq 400.0 f> if
      1.0 random 0.02 f* 0.99 f+
    else
      1.0 random freq 200.0 f> if 0.01 f* 0.995 f+ else 0.002 f* 0.999 f+ then
    then freq f*			\ freq
    amp					\ amp
    :fm-index      fm-index 1.0 random 0.75 f+ f*
    :amp-env       amp-env
    :reverb-amount rev-amt fm-violin
  loop
;

event: fth-short-example ( -- )
  '( 0 0 221 1 240 0 )             { amp-env }
  '( 0 -1 5 0.25 10 0 100 0.1 ) to fmv-gliss-env
  12.0  			to fmv-random-vibrato-rate
  0.01  			to fmv-random-vibrato-amp
  0.01  			to fmv-glissando-amount
  0.002 			to fmv-noise-amount
  0 8.53 993.323        0.03 :fm-index 0.75 :reverb-amount 0.2 :amp-env amp-env
    violin-new
  5 4.53 5/6 993.323 f* 0.02 :fm-index 0.55 :reverb-amount 0.2 :amp-env amp-env
    violin-new
;event

0  value *counter*
#f value *timer*

: test-info { ctime -- }
  *counter* 1+ to *counter*
  *timer* stop-timer
  "\\ %02d: score %3d   utime %7.3f\n" '( *counter* ctime *timer* utime@ )
    fth-print
;

event: fth-long-example ( -- )
  '( 0 0 1 1 99 1 100 0 )                        { tap }
  '( 0 0 0.5 1 5 1 10 0.5 15 0.25 35 0.1 100 0 ) { metalamp }
  '( 0 0 75 0.1 90 0.3 97 0.6 100 1 )            { whoosh }
  '( 0 0 50 1 100 0 )                            { mamp }
  '( 0 0 65 1 100 0 )                            { n-amp }
  0 { beg }
  0 to *counter*
  make-timer to *timer*

  beg test-info

  \ 1, score 0
  restore-fm-violin-defaults
  0 to fmv-glissando-amount
  0.1 to fmv-reverb-amount
  metalamp to fmv-amp-env
  6.718 to fmv-fm1-rat
  4.414 to fmv-fm2-rat
  5.141 to fmv-fm3-rat
  beg 0.0000 f+ 1.6 164.5868 0.16   :fm-index 2.1087 vln-one-sin
  beg 0.0003 f+ 4   164.5868 0.2600 :fm-index 1.5488 vln-one-sin
  beg 0.0005 f+ 1.2 125.9513 0.2600 :fm-index 2.2999 vln-one-sin
  beg 0.0005 f+ 2.8 125.9513 0.16   :fm-index 1.6818 vln-one-sin
  beg 0.0013 f+ 4    24.4994 0.3    :fm-index 2.4557 vln-one-sin
  beg 0.0033 f+ 3    24.4994 0.3    :fm-index 1.9387 vln-one-sin
  beg 0.0035 f+ 2.8  24.4994 0.2600 :fm-index 2.3828 vln-one-sin
  beg 0.0040 f+ 0.8  24.4994 0.16   :fm-index 1.7348 vln-one-sin
  beg 0.0043 f+ 4    24.4994 0.3    :fm-index 2.0886 vln-one-sin

  beg 6 + dup to beg test-info

  \ 2, score 6
  2.718 to fmv-fm1-rat
  4.414 to fmv-fm2-rat
  3.141 to fmv-fm3-rat
  beg 0.0003 f+ 1.2  88.8854 0.16   :fm-index 2.0711 vln-one-sin
  beg 0.0003 f+ 4    88.8854 0.2600 :fm-index 2.0225 vln-one-sin
  beg 0.0005 f+ 1.2 102.7186 0.2600 :fm-index 1.9300 vln-one-sin
  beg 0.0010 f+ 1.2  32.7025 0.3600 :fm-index 1.9269 vln-one-sin
  beg 0.0015 f+ 2.8  32.7025 0.2600 :fm-index 2.2153 vln-one-sin
  beg 0.0023 f+ 2    32.7025 0.2600 :fm-index 2.1968 vln-one-sin
  beg 0.0023 f+ 4    32.7025 0.3600 :fm-index 1.6091 vln-one-sin
  beg 0.0025 f+ 2    32.7025 0.2600 :fm-index 2.1766 vln-one-sin
  beg 0.0035 f+ 1.2  32.7025 0.16   :fm-index 1.5157 vln-one-sin
  beg 0.0040 f+ 0.8  32.7025 0.16   :fm-index 1.8092 vln-one-sin
  beg 0.0043 f+ 2    32.7025 0.2600 :fm-index 1.6198 vln-one-sin

  beg 6 + dup to beg test-info

  \ 3, score 12
  0.2   to fmv-reverb-amount
  5.141 to fmv-fm3-rat
  beg 0.0003 f+ 1.2 177.7708 0.3  :fm-index 1.9631 vln-one-sin
  beg 0.0003 f+ 4   177.7708 0.3  :fm-index 1.9647 vln-one-sin
  beg 0.0005 f+ 2.8 336.2471 0.16 :fm-index 2.3977 vln-one-sin
  beg 0.0008 f+ 1.2 336.2471 0.25 :fm-index 2.4103 vln-one-sin
  beg 0.0010 f+ 2    65.4050 0.3  :fm-index 1.8419 vln-one-sin
  beg 0.0033 f+ 2    65.4050 0.3  :fm-index 2.4540 vln-one-sin
  beg 0.0043 f+ 4    65.4050 0.16 :fm-index 2.2909 vln-one-sin

  beg 6 + dup to beg test-info

  \ 4, score 18
  0.1 to fmv-reverb-amount
  beg 0.0003 f+ 1.2 11.1107 0.3  :fm-index 1.8715 vln-one-sin
  beg 0.0003 f+ 4   11.1107 0.3  :fm-index 2.4590 vln-one-sin
  beg 0.0005 f+ 2.8 21.0154 0.16 :fm-index 2.3802 vln-one-sin
  beg 0.0008 f+ 1.2 21.0154 0.25 :fm-index 1.7564 vln-one-sin
  beg 0.0010 f+ 2    4.0878 0.3  :fm-index 2.2529 vln-one-sin
  beg 0.0033 f+ 2    4.0878 0.3  :fm-index 1.9693 vln-one-sin
  beg 0.0043 f+ 4    4.0878 0.16 :fm-index 2.2534 vln-one-sin

  beg 6 + dup to beg test-info

  \ 5, score 24
  restore-fm-violin-defaults
  0.1 to fmv-noise-amount
  beg 0.0000 f+ 5.4 116.5400 0.25 :fm-index 2.2822 :reverb-amount 0.0280
    :amp-env '( 0 0 0.0556 1 4.0937 0.6 9.1414 0.3 24.2845 0.1 100.0 0 )
    vln-one-sin-ran
  beg 0.0100 f+ 5.4  43.6538 0.25 :fm-index 2.0867 :reverb-amount 0.0202
    :amp-env '( 0 0 0.0556 1 4.0937 0.6 9.1414 0.3 24.2845 0.1 100.0 0 )
    vln-one-sin-ran
  beg 0.0200 f+ 5.4 130.8100 0.25 :fm-index 1.9652 :reverb-amount 0.0270
    :amp-env '( 0 0 0.0556 1 4.0937 0.6 9.1414 0.3 24.2845 0.1 100.0 0 )
    vln-one-sin-ran
  beg 0.0250 f+ 5.4  87.3075 0.25 :fm-index 2.1524 :reverb-amount 0.0260
    :amp-env '( 0 0 0.0556 1 4.0937 0.6 9.1414 0.3 24.2845 0.1 100.0 0 )
    vln-one-sin-ran
  beg 0.0300 f+ 4.5 261.6200 0.15 :fm-index 2.1384 :reverb-amount 0.0242
    :amp-env '( 0 0 0.0667 1 4.1044 0.6 9.1515 0.3 24.2929 0.1 100 0 )
    vln-one-sin-ran
  beg 0.0300 f+ 4.5 174.6150 0.15 :fm-index 2.1425 :reverb-amount 0.0265
    :amp-env '( 0 0 0.0667 1 4.1044 0.6 9.1515 0.3 24.2929 0.1 100 0 )
    vln-one-sin-ran
  beg 0.0300 f+ 4.5 130.8100 0.15 :fm-index 1.9805 :reverb-amount 0.0201
    :amp-env '( 0 0 0.0667 1 4.1044 0.6 9.1515 0.3 24.2929 0.1 100 0 ) 
    vln-one-sin-ran
  beg 0.0350 f+ 4.5  43.6538 0.15 :fm-index 2.4876 :reverb-amount 0.0329
    :amp-env '( 0 0 0.0667 1 4.1044 0.6 9.1515 0.3 24.2929 0.1 100 0 )
    vln-one-sin-ran
  beg 0.0400 f+ 3.6 220      0.15 :fm-index 1.8282 :reverb-amount 0.0244
    :amp-env '( 0 0 0.0833 1 4.1204 0.6 9.1667 0.3 24.3056 0.1 100.0 0 )
    :noise-amount 0.15 vln-one-sin-ran
  beg 0.0400 f+ 3.6 174.6150 0.15 :fm-index 2.3479 :reverb-amount 0.0200
    :amp-env '( 0 0 0.0833 1 4.1204 0.6 9.1667 0.3 24.3056 0.1 100.0 0 )
    :noise-amount 0.15 vln-one-sin-ran
  beg 0.0400 f+ 3.6 523.2400 0.15 :fm-index 1.6424 :reverb-amount 0.0286
    :amp-env '( 0 0 0.0833 1 4.1204 0.6 9.1667 0.3 24.3056 0.1 100.0 0 )
    :noise-amount 0.15 vln-one-sin-ran
  beg 0.0450 f+ 3.6 349.2300 0.15 :fm-index 1.6449 :reverb-amount 0.0333
    :amp-env '( 0 0 0.0833 1 4.1204 0.6 9.1667 0.3 24.3056 0.1 100.0 0 )
    :noise-amount 0.15 vln-one-sin-ran
  beg 0.0500 f+ 6  699.4600 0.15 :fm-index 1.5570 :reverb-amount 0.13 
    :amp-env tap vln-one-sin-ran
  beg 0.0500 f+ 6 1397.9200 0.15 :fm-index 1.5131 :reverb-amount 0.13 
    :amp-env tap vln-one-sin-ran
  beg 0.0500 f+ 6  783.9800 0.15 :fm-index 2.2031 :reverb-amount 0.13 
    :amp-env tap vln-one-sin-ran
  beg 0.0500 f+ 6 1046.4800 0.15 :fm-index 2.2724 :reverb-amount 0.13 
    :amp-env tap vln-one-sin-ran
  beg 0.0600 f+ 9   21.8269 0.15 :fm-index 2.1048 :reverb-amount 0.1 
    :amp-env tap vln-one-sin-ran
  beg 0.0600 f+ 8   87.3075 0.15 :fm-index 1.8854 :reverb-amount 0.1 
    :amp-env tap vln-one-sin-ran
  beg 0.0600 f+ 7   65.4050 0.15 :fm-index 1.6781 :reverb-amount 0.1 
    :amp-env tap vln-one-sin-ran
  beg 0.0600 f+ 8   43.6538 0.15 :fm-index 1.7862 :reverb-amount 0.1 
    :amp-env tap vln-one-sin-ran
  beg 0.0700 f+ 6  175.6150 0.15 :fm-index 2.2656 :reverb-amount 0.1 
    :amp-env tap vln-one-sin-ran
  beg 0.0700 f+ 6  350.2300 0.15 :fm-index 2.4241 :reverb-amount 0.1 
    :amp-env tap vln-one-sin-ran
  beg 0.0700 f+ 6  131.8100 0.15 :fm-index 2.4294 :reverb-amount 0.1 
    :amp-env tap vln-one-sin-ran
  beg 0.0700 f+ 6   32.7025 0.15 :fm-index 1.5779 :reverb-amount 0.1 
    :amp-env tap vln-one-sin-ran
  beg 0.0800 f+ 6  196.9950 0.15 :fm-index 1.8511 :reverb-amount 0.1 
    :amp-env tap vln-one-sin-ran
  beg 0.0800 f+ 6 1047.4800 0.15 :fm-index 2.2148 :reverb-amount 0.1 
    :amp-env tap vln-one-sin-ran
  beg 0.0800 f+ 6  831.6200 0.15 :fm-index 1.9913 :reverb-amount 0.1 
    :amp-env tap vln-one-sin-ran
  beg 0.2700 f+ 6  784.9800 0.16 :fm-index 2.0693 :reverb-amount 0.1 
    :amp-env tap vln-one-sin-ran
  beg 0.2700 f+ 6   64.4050 0.16 :fm-index 1.6920 :reverb-amount 0.1 
    :amp-env tap vln-one-sin-ran
  beg 0.2700 f+ 6  208.6550 0.16 :fm-index 2.2597 :reverb-amount 0.1 
    :amp-env tap vln-one-sin-ran
  beg 0.2700 f+ 6   43.6538 0.16 :fm-index 2.2522 :reverb-amount 0.1 
    :amp-env tap vln-one-sin-ran

  beg 12 + dup to beg test-info

  \ 6, score 36
  restore-fm-violin-defaults
  beg 0.0000 f+ 1.8 349.2300 0.16 :fm-index 2.1541 :reverb-amount 0.0225
    :amp-env '( 0 0 0.1667 1 4.2003 0.6 9.2424 0.3 24.3687 0.1 100 0 )
    :noise-amount 0.0500 vln-one-sin-ran
  beg 0.0100 f+ 2.7 146.8300 0.16 :fm-index 2.3335 :reverb-amount 0.0274
    :amp-env '( 0 0 0.1111 1 4.1470 0.6 9.1919 0.3 24.3266 0.1 100.0 0 )
    :noise-amount 0.0500 vln-one-sin-ran
  beg 0.0200 f+ 1.8 880      0.16 :fm-index 2.1910 :reverb-amount 0.0279
    :amp-env '( 0 0 0.1667 1 4.2003 0.6 9.2424 0.3 24.3687 0.1 100 0 )
    :noise-amount 0.0500 vln-one-sin-ran
  beg 0.0250 f+ 3.6  73.4150 0.16 :fm-index 2.1410 :reverb-amount 0.0223
    :amp-env '( 0 0 0.0833 1 4.1204 0.6 9.1667 0.3 24.3056 0.1 100.0 0 )
    :noise-amount 0.0500 vln-one-sin-ran
  beg 0.0300 f+ 2.7  87.3075 0.16 :fm-index 1.8491 :reverb-amount 0.0217
    :amp-env '( 0 0 0.1111 1 4.1470 0.6 9.1919 0.3 24.3266 0.1 100.0 0 )
    :noise-amount 0.0010 vln-one-sin-ran
  beg 0.0300 f+ 2.7  75.5662 0.16 :fm-index 1.9191 :reverb-amount 0.0204
    :amp-env '( 0 0 0.1111 1 4.1470 0.6 9.1919 0.3 24.3266 0.1 100.0 0 )
    :noise-amount 0.0010 vln-one-sin-ran
  beg 0.0400 f+ 3.6  52.3432 0.16 :fm-index 1.6090 :reverb-amount 0.0296
    :amp-env '( 0 0 0.0833 1 4.1204 0.6 9.1667 0.3 24.3056 0.1 100.0 0 )
    :noise-amount 0.0010 vln-one-sin-ran
  beg 0.0450 f+ 1.8  73.4150 0.16 :fm-index 2.2201 :reverb-amount 0.0221
    :amp-env '( 0 0 0.1667 1 4.2003 0.6 9.2424 0.3 24.3687 0.1 100 0 )
    :noise-amount 0.0010 vln-one-sin-ran
  beg 0.0500 f+ 4   116.5400 0.06 :fm-index 2.0230 :reverb-amount 0.1
    :amp-env tap :noise-amount 0.0010 vln-one-sin-ran
  beg 0.0500 f+ 4    97.9975 0.06 :fm-index 1.7284 :reverb-amount 0.1
    :amp-env tap :noise-amount 0.0010 vln-one-sin-ran
  beg 0.0600 f+ 4    36.7075 0.06 :fm-index 1.6845 :reverb-amount 0.1
    :amp-env tap :noise-amount 0.0010 vln-one-sin-ran
  beg 0.0650 f+ 4    97.9975 0.06 :fm-index 2.4616 :reverb-amount 0.1
    :amp-env tap :noise-amount 0.0010 vln-one-sin-ran

  beg 7 + dup to beg test-info

  \ 7, score 43
  beg 0.0000 f+ 1.8 261.6200 0.16 :fm-index 2.2576 :reverb-amount 0.0286
    :amp-env '( 0 0 0.1667 1 4.2003 0.6 9.2424 0.3 24.3687 0.1 100 0 )
    :noise-amount 0.0100 vln-one-sin-ran
  beg 0.0100 f+ 2.7 130.8100 0.16 :fm-index 2.1530 :reverb-amount 0.0330
    :amp-env '( 0 0 0.1111 1 4.1470 0.6 9.1919 0.3 24.3266 0.1 100.0 0 )
    :noise-amount 0.0100 vln-one-sin-ran
  beg 0.0200 f+ 1.8 523.2400 0.16 :fm-index 2.0608 :reverb-amount 0.0235
    :amp-env '( 0 0 0.1667 1 4.2003 0.6 9.2424 0.3 24.3687 0.1 100 0 )
    :noise-amount 0.0100 vln-one-sin-ran
  beg 0.0250 f+ 3.6  65.4050 0.16 :fm-index 2.2203 :reverb-amount 0.0234
    :amp-env '( 0 0 0.0833 1 4.1204 0.6 9.1667 0.3 24.3056 0.1 100.0 0 )
    :noise-amount 0.0100 vln-one-sin-ran
  beg 0.0300 f+ 2.7  65.4050 0.16 :fm-index 1.7089 :reverb-amount 0.0208
    :amp-env '( 0 0 0.1111 1 4.1470 0.6 9.1919 0.3 24.3266 0.1 100.0 0 )
    :noise-amount 0.0010 vln-one-sin-ran
  beg 0.0300 f+ 2.7 130.8100 0.16 :fm-index 2.2948 :reverb-amount 0.0269
    :amp-env '( 0 0 0.1111 1 4.1470 0.6 9.1919 0.3 24.3266 0.1 100.0 0 )
    :noise-amount 0.0010 vln-one-sin-ran
  beg 0.0400 f+ 3.6  32.7025 0.16 :fm-index 1.7677 :reverb-amount 0.0288
    :amp-env '( 0 0 0.0833 1 4.1204 0.6 9.1667 0.3 24.3056 0.1 100.0 0 )
    :noise-amount 0.0010 vln-one-sin-ran
  beg 0.0450 f+ 1.8  32.7025 0.16 :fm-index 1.9030 :reverb-amount 0.0209
    :amp-env '( 0 0 0.1667 1 4.2003 0.6 9.2424 0.3 24.3687 0.1 100 0 )
    :noise-amount 0.0010 vln-one-sin-ran
  beg 0.0500 f+ 4    65.4050 0.06 :fm-index 2.2757 :reverb-amount 0.1
    :amp-env tap :noise-amount 0.0010 vln-one-sin-ran
  beg 0.0500 f+ 4    65.4050 0.06 :fm-index 2.2435 :reverb-amount 0.1
    :amp-env tap :noise-amount 0.0010 vln-one-sin-ran
  beg 0.0600 f+ 4    32.7025 0.06 :fm-index 1.9619 :reverb-amount 0.1
    :amp-env tap :noise-amount 0.0010 vln-one-sin-ran
  beg 0.0650 f+ 4    65.4050 0.06 :fm-index 2.0207 :reverb-amount 0.1
    :amp-env tap :noise-amount 0.0010 vln-one-sin-ran

  beg 6 + dup to beg test-info

  \ 8, score 49
  beg 0.0100 f+ 0.9  3135.9200 0.16 :fm-index 2.1204 :reverb-amount 0.0024
    :amp-env '( 0 0 0.3333 1 4.3603 0.6 9.3939 0.3 24.4950 0.1 100.0 0 )
    :noise-amount 0.0100 vln-one-sin-ran
  beg 0.0100 f+ 0.45 1567.96   0.16 :fm-index 2.0691 :reverb-amount 0.0025
    :amp-env '( 0 0 0.6667 1 4.6801 0.6 9.6970 0.3 24.7475 0.1 100 0 )
    :noise-amount 0.0100 vln-one-sin-ran
  beg 0.0200 f+ 0.9  6271.8400 0.16 :fm-index 2.2081 :reverb-amount 0.0022
    :amp-env '( 0 0 0.3333 1 4.3603 0.6 9.3939 0.3 24.4950 0.1 100.0 0 )
    :noise-amount 0.0100 vln-one-sin-ran
  beg 0.0250 f+ 0.9   783.9800 0.16 :fm-index 1.8719 :reverb-amount 0.0022
    :amp-env '( 0 0 0.3333 1 4.3603 0.6 9.3939 0.3 24.4950 0.1 100.0 0 )
    :noise-amount 0.0100 vln-one-sin-ran
  beg 0.0300 f+ 0.27  783.9800 0.16 :fm-index 1.9705 :reverb-amount 0.0020
    :amp-env '( 0 0 1.1111 1 5.1066 0.6 10.1010 0.3 25.0842 0.1 100.0 0 )
    :noise-amount 0.0100 vln-one-sin-ran
  beg 0.0300 f+ 0.63 1567.96   0.16 :fm-index 1.6778 :reverb-amount 0.0021
    :amp-env '( 0 0 0.4762 1 4.4974 0.6 9.5238 0.3 24.6032 0.1 100.0 0 )
    :noise-amount 0.0100 vln-one-sin-ran
  beg 0.0400 f+ 0.9   391.9900 0.16 :fm-index 1.9558 :reverb-amount 0.0023
    :amp-env '( 0 0 0.3333 1 4.3603 0.6 9.3939 0.3 24.4950 0.1 100.0 0 )
    :noise-amount 0.0100 vln-one-sin-ran
  beg 0.0450 f+ 0.45  195.9950 0.16 :fm-index 2.1344 :reverb-amount 0.0027
    :amp-env '( 0 0 0.6667 1 4.6801 0.6 9.6970 0.3 24.7475 0.1 100 0 )
    :noise-amount 0.0100 vln-one-sin-ran
  beg 0.050 f+ 2  783.980 0.16 :reverb-amount 0.01 :amp-env tap 
    :noise-amount 0.009 vln-one-sin-ran
  beg 0.050 f+ 1 1567.960 0.16 :reverb-amount 0.01 :amp-env tap 
    :noise-amount 0.009 vln-one-sin-ran
  beg 0.060 f+ 2  391.990 0.16 :reverb-amount 0.01 :amp-env tap 
    :noise-amount 0.009 vln-one-sin-ran
  beg 0.065 f+ 1  783.980 0.16 :reverb-amount 0.01 :amp-env tap 
    :noise-amount 0.009 vln-one-sin-ran
  beg 0.070 f+ 2  195.995 0.16 :reverb-amount 0.01 :amp-env tap 
    :noise-amount 0.004 vln-one-sin-ran
  beg 0.070 f+ 1 1567.960 0.16 :reverb-amount 0.01 :amp-env tap 
    :noise-amount 0.004 vln-one-sin-ran
  beg 0.080 f+ 1  784.980 0.16 :reverb-amount 0.01 :amp-env tap 
    :noise-amount 0.004 vln-one-sin-ran
  beg 0.085 f+ 2  391.990 0.16 :reverb-amount 0.01 :amp-env tap 
    :noise-amount 0.004 vln-one-sin-ran

  beg 6 + dup to beg test-info

  \ 9, score 55
  beg 0.0100 f+ 0.9  97.9975 0.1 :fm-index 2.0885 :reverb-amount 0.0031
    :amp-env '( 0 0 0.3333 1 4.3603 0.6 9.3939 0.3 24.4950 0.1 100.0 0 )
    :noise-amount 0.0100 vln-one-sin-ran
  beg 0.0100 f+ 1.8  48.9988 0.1 :fm-index 2.2269 :reverb-amount 0.0026
    :amp-env '( 0 0 0.1667 1 4.2003 0.6 9.2424 0.3 24.3687 0.1 100 0 )
    :noise-amount 0.0100 vln-one-sin-ran
  beg 0.0200 f+ 0.9 195.9950 0.1 :fm-index 2.0305 :reverb-amount 0.0032
    :amp-env '( 0 0 0.3333 1 4.3603 0.6 9.3939 0.3 24.4950 0.1 100.0 0 )
    :noise-amount 0.0100 vln-one-sin-ran
  beg 0.0250 f+ 0.9  24.4994 0.1 :fm-index 2.4934 :reverb-amount 0.0025
    :amp-env '( 0 0 0.3333 1 4.3603 0.6 9.3939 0.3 24.4950 0.1 100.0 0 )
    :noise-amount 0.0100 vln-one-sin-ran
  beg 0.0300 f+ 1.8  97.9975 0.1 :fm-index 2.4039 :reverb-amount 0.0023
    :amp-env '( 0 0 0.1667 1 4.2003 0.6 9.2424 0.3 24.3687 0.1 100 0 )
    :noise-amount 0.0400 vln-one-sin-ran
  beg 0.0300 f+ 0.9 195.9950 0.1 :fm-index 1.5159 :reverb-amount 0.0021
    :amp-env '( 0 0 0.3333 1 4.3603 0.6 9.3939 0.3 24.4950 0.1 100.0 0 )
    :noise-amount 0.0400 vln-one-sin-ran
  beg 0.0300 f+ 0.9 392.9900 0.1 :fm-index 2.2122 :reverb-amount 0.0028
    :amp-env '( 0 0 0.3333 1 4.3603 0.6 9.3939 0.3 24.4950 0.1 100.0 0 )
    :noise-amount 0.0400 vln-one-sin-ran
  beg 0.0300 f+ 1.8 784.9800 0.1 :fm-index 2.1574 :reverb-amount 0.0020
    :amp-env '( 0 0 0.1667 1 4.2003 0.6 9.2424 0.3 24.3687 0.1 100 0 )
    :noise-amount 0.0400 vln-one-sin-ran
  beg 0.0300 f+ 2.7  24.4994 0.1 :fm-index 2.1963 :reverb-amount 0.0031
    :amp-env '( 0 0 0.1111 1 4.1470 0.6 9.1919 0.3 24.3266 0.1 100.0 0 )
    :noise-amount 0.0100 vln-one-sin-ran
  beg 0.0300 f+ 1.8  48.9988 0.1 :fm-index 1.9761 :reverb-amount 0.0032
    :amp-env '( 0 0 0.1667 1 4.2003 0.6 9.2424 0.3 24.3687 0.1 100 0 )
    :noise-amount 0.0100 vln-one-sin-ran
  beg 0.0400 f+ 2.7  12.2497 0.1 :fm-index 1.5088 :reverb-amount 0.0021
    :amp-env '( 0 0 0.1111 1 4.1470 0.6 9.1919 0.3 24.3266 0.1 100.0 0 )
    :noise-amount 0.0100 vln-one-sin-ran
  beg 0.0450 f+ 1.8   6.1248 0.1 :fm-index 1.7384 :reverb-amount 0.0021
    :amp-env '( 0 0 0.1667 1 4.2003 0.6 9.2424 0.3 24.3687 0.1 100 0 )
    :noise-amount 0.0100 vln-one-sin-ran
  beg 0.050 f+ 2 24.4994 0.1 :reverb-amount 0.1 :amp-env tap 
    :noise-amount 0.004 vln-one-sin-ran
  beg 0.050 f+ 1 48.9988 0.1 :reverb-amount 0.1 :amp-env tap 
    :noise-amount 0.004 vln-one-sin-ran
  beg 0.060 f+ 2 12.2497 0.1 :reverb-amount 0.1 :amp-env tap 
    :noise-amount 0.004 vln-one-sin-ran
  beg 0.065 f+ 1 24.4994 0.1 :reverb-amount 0.1 :amp-env tap 
    :noise-amount 0.004 vln-one-sin-ran
  beg 0.070 f+ 2  6.1248 0.1 :fm-index 1.2474 :reverb-amount 0.1 :amp-env tap
    :noise-amount 0.004 vln-one-sin-ran
  beg 0.070 f+ 1 48.9988 0.1 :fm-index 0.7526 :reverb-amount 0.1 :amp-env tap
    :noise-amount 0.004 vln-one-sin-ran
  beg 0.080 f+ 1 25.4994 0.1 :fm-index 1.1080 :reverb-amount 0.1 :amp-env tap
    :noise-amount 0.004 vln-one-sin-ran
  beg 0.085 f+ 2 12.2497 0.1 :fm-index 1.0859 :reverb-amount 0.1 :amp-env tap
    :noise-amount 0.004 vln-one-sin-ran
  beg 0.090 f+ 4 97.9975 0.1 :fm-index 2.4788 :reverb-amount 0.1 :amp-env tap
    :noise-amount 0.004 vln-one-sin-ran
  beg 0.090 f+ 3 48.9988 0.1 :fm-index 1.8980 :reverb-amount 0.1 :amp-env tap
    :noise-amount 0.004 vln-one-sin-ran
  beg 0.090 f+ 3 25.4994 0.1 :fm-index 2.1151 :reverb-amount 0.1 :amp-env tap
    :noise-amount 0.004 vln-one-sin-ran
  beg 0.095 f+ 5 12.2497 0.1 :fm-index 2.3224 :reverb-amount 0.1 :amp-env tap
    :noise-amount 0.004 vln-one-sin-ran

  beg 6 + dup to beg test-info

  \ 10, score 61
  beg 0.2100 f+ 0.9 123.4725 0.1 :reverb-amount 0.0031
    :amp-env '( 0 0 0.3333 1 4.3603 0.6 9.3939 0.3 24.4950 0.1 100.0 0 )
    :noise-amount 0.0100 vln-one-sin-ran
  beg 0.2100 f+ 1.8  61.7363 0.1 :reverb-amount 0.0023
    :amp-env '( 0 0 0.1667 1 4.2003 0.6 9.2424 0.3 24.3687 0.1 100 0 )
    :noise-amount 0.0100 vln-one-sin-ran
  beg 0.2200 f+ 0.9 246.9450 0.1 :reverb-amount 0.0023
    :amp-env '( 0 0 0.3333 1 4.3603 0.6 9.3939 0.3 24.4950 0.1 100.0 0 )
    :noise-amount 0.0100 vln-one-sin-ran
  beg 0.2250 f+ 0.9  30.8681 0.1 :reverb-amount 0.0026
    :amp-env '( 0 0 0.3333 1 4.3603 0.6 9.3939 0.3 24.4950 0.1 100.0 0 )
    :noise-amount 0.0100 vln-one-sin-ran
  beg 0.2300 f+ 1.8 123.4725 0.1 :reverb-amount 0.0027
    :amp-env '( 0 0 0.1667 1 4.2003 0.6 9.2424 0.3 24.3687 0.1 100 0 )
    :noise-amount 0.0400 vln-one-sin-ran
  beg 0.2300 f+ 0.9 246.9450 0.1 :reverb-amount 0.0026
    :amp-env '( 0 0 0.3333 1 4.3603 0.6 9.3939 0.3 24.4950 0.1 100.0 0 )
    :noise-amount 0.0400 vln-one-sin-ran
  beg 0.2300 f+ 0.9 494.8900 0.1 :reverb-amount 0.0020
    :amp-env '( 0 0 0.3333 1 4.3603 0.6 9.3939 0.3 24.4950 0.1 100.0 0 )
    :noise-amount 0.0400 vln-one-sin-ran
  beg 0.2300 f+ 1.8 988.7800 0.1 :reverb-amount 0.0025
    :amp-env '( 0 0 0.1667 1 4.2003 0.6 9.2424 0.3 24.3687 0.1 100 0 )
    :noise-amount 0.0400 vln-one-sin-ran
  beg 0.2300 f+ 2.7  30.8681 0.1 :reverb-amount 0.0028
    :amp-env '( 0 0 0.1111 1 4.1470 0.6 9.1919 0.3 24.3266 0.1 100.0 0 )
    :noise-amount 0.0100 vln-one-sin-ran
  beg 0.2300 f+ 1.8  61.7363 0.1 :reverb-amount 0.0023
    :amp-env '( 0 0 0.1667 1 4.2003 0.6 9.2424 0.3 24.3687 0.1 100 0 )
    :noise-amount 0.0100 vln-one-sin-ran
  beg 0.2400 f+ 2.7  15.4341 0.1 :reverb-amount 0.0030
    :amp-env '( 0 0 0.1111 1 4.1470 0.6 9.1919 0.3 24.3266 0.1 100.0 0 )
    :noise-amount 0.0100 vln-one-sin-ran
  beg 0.2450 f+ 1.8  20.5788 0.1 :reverb-amount 0.0023
    :amp-env '( 0 0 0.1667 1 4.2003 0.6 9.2424 0.3 24.3687 0.1 100 0 )
    :noise-amount 0.0100 vln-one-sin-ran
  beg 0.250 f+ 2 30.8681 0.1 :reverb-amount 0.1 :amp-env tap 
    :noise-amount 0.009 vln-one-sin-ran
  beg 0.250 f+ 1 61.7363 0.1 :reverb-amount 0.1 :amp-env tap 
    :noise-amount 0.009 vln-one-sin-ran
  beg 0.260 f+ 2 15.4341 0.1 :reverb-amount 0.1 :amp-env tap 
    :noise-amount 0.009 vln-one-sin-ran
  beg 0.265 f+ 1 30.8681 0.1 :reverb-amount 0.1 :amp-env tap 
    :noise-amount 0.009 vln-one-sin-ran
  beg 0.271 f+ 2 30.8681 0.1 :reverb-amount 0.1 :amp-env tap 
    :noise-amount 0.004 vln-one-sin-ran
  beg 0.271 f+ 1 61.7363 0.1 :reverb-amount 0.1 :amp-env tap 
    :noise-amount 0.004 vln-one-sin-ran
  beg 0.281 f+ 1 31.8681 0.1 :reverb-amount 0.1 :amp-env tap 
    :noise-amount 0.004 vln-one-sin-ran
  beg 0.286 f+ 2 15.4341 0.1 :reverb-amount 0.1 :amp-env tap 
    :noise-amount 0.004 vln-one-sin-ran

  beg 8 + dup to beg test-info

  \ 11, score 69
  '( 0 0 1 1 100 0 ) { yup }
  beg 0.0100 f+ 0.9  3135.9200 0.16 :fm-index 1.7299 :reverb-amount 0.0026
    :amp-env '( 0 0 0.3333 1 4.3603 0.6 9.3939 0.3 24.4950 0.1 100.0 0 )
    :noise-amount 0.0100 vln-one-sin-ran
  beg 0.0100 f+ 0.45 1464.6987 0.16 :fm-index 1.9173 :reverb-amount 0.0027
    :amp-env '( 0 0 0.6667 1 4.6801 0.6 9.6970 0.3 24.7475 0.1 100 0 )
    :noise-amount 0.0100 vln-one-sin-ran
  beg 0.0200 f+ 0.9  6714.0048 0.16 :fm-index 2.4604 :reverb-amount 0.0032
    :amp-env '( 0 0 0.3333 1 4.3603 0.6 9.3939 0.3 24.4950 0.1 100.0 0 )
    :noise-amount 0.0100 vln-one-sin-ran
  beg 0.0250 f+ 0.9   684.1190 0.16 :fm-index 1.9969 :reverb-amount 0.0021
    :amp-env '( 0 0 0.3333 1 4.3603 0.6 9.3939 0.3 24.4950 0.1 100.0 0 )
    :noise-amount 0.0100 vln-one-sin-ran
  beg 0.0300 f+ 0.27  684.1190 0.16 :fm-index 2.0022 :reverb-amount 0.0026
    :amp-env '( 0 0 1.1111 1 5.1066 0.6 10.1010 0.3 25.0842 0.1 100.0 0 )
    :noise-amount 0.0100 vln-one-sin-ran
  beg 0.0300 f+ 0.63 1464.6987 0.16 :fm-index 2.1058 :reverb-amount 0.0027
    :amp-env '( 0 0 0.4762 1 4.4974 0.6 9.5238 0.3 24.6032 0.1 100.0 0 )
    :noise-amount 0.0100 vln-one-sin-ran
  beg 0.0400 f+ 0.9   319.5325 0.16 :fm-index 2.2293 :reverb-amount 0.0029
    :amp-env '( 0 0 0.3333 1 4.3603 0.6 9.3939 0.3 24.4950 0.1 100.0 0 )
    :noise-amount 0.0100 vln-one-sin-ran
  beg 0.0450 f+ 0.45  149.2445 0.16 :fm-index 1.5780 :reverb-amount 0.0025
    :amp-env '( 0 0 0.6667 1 4.6801 0.6 9.6970 0.3 24.7475 0.1 100 0 )
    :noise-amount 0.0100 vln-one-sin-ran
  beg 0.050 f+ 1  684.1190 0.16 :reverb-amount 0.01 :amp-env yup 
    :noise-amount 0.009 vln-one-sin-ran
  beg 0.050 f+ 1 1464.6987 0.16 :reverb-amount 0.01 :amp-env yup 
    :noise-amount 0.009 vln-one-sin-ran
  beg 0.060 f+ 1  319.5325 0.16 :reverb-amount 0.01 :amp-env yup 
    :noise-amount 0.009 vln-one-sin-ran
  beg 0.065 f+ 1  684.1190 0.16 :reverb-amount 0.01 :amp-env yup 
    :noise-amount 0.009 vln-one-sin-ran
  beg 0.070 f+ 1  149.2445 0.16 :reverb-amount 0.01 :amp-env yup 
    :noise-amount 0.004 vln-one-sin-ran
  beg 0.070 f+ 1 1464.6987 0.16 :reverb-amount 0.01 :amp-env yup 
    :noise-amount 0.004 vln-one-sin-ran
  beg 0.080 f+ 1  561.6022 0.16 :reverb-amount 0.01 :amp-env yup 
    :noise-amount 0.004 vln-one-sin-ran
  beg 0.085 f+ 1  319.5325 0.16 :reverb-amount 0.01 :amp-env yup 
    :noise-amount 0.004 vln-one-sin-ran

  beg 3 + dup to beg test-info

  \ 12, score 72
  beg 0.0100 f+ 0.9 3135.9200 0.16 :fm-index 1.6329 :reverb-amount 0.0031
    :amp-env '( 0 0 0.3333 1 4.3603 0.6 9.3939 0.3 24.4950 0.1 100.0 0 )
    :noise-amount 0.0100 vln-one-sin-ran
  beg 0.0100 f+ 0.45 1810.5774 0.16 :fm-index 1.8298 :reverb-amount 0.0031
    :amp-env '( 0 0 0.6667 1 4.6801 0.6 9.6970 0.3 24.7475 0.1 100 0 )
    :noise-amount 0.0100 vln-one-sin-ran
  beg 0.0200 f+ 0.9 5431.4135 0.16 :fm-index 2.1640 :reverb-amount 0.0022
    :amp-env '( 0 0 0.3333 1 4.3603 0.6 9.3939 0.3 24.4950 0.1 100.0 0 )
    :noise-amount 0.0100 vln-one-sin-ran
  beg 0.0250 f+ 0.9 1045.3680 0.16 :fm-index 1.6971 :reverb-amount 0.0032
    :amp-env '( 0 0 0.3333 1 4.3603 0.6 9.3939 0.3 24.4950 0.1 100.0 0 )
    :noise-amount 0.0100 vln-one-sin-ran
  beg 0.0300 f+ 0.27 1045.3680 0.16 :fm-index 2.4855 :reverb-amount 0.0028
    :amp-env '( 0 0 1.1111 1 5.1066 0.6 10.1010 0.3 25.0842 0.1 100.0 0 )
    :noise-amount 0.0100 vln-one-sin-ran
  beg 0.0300 f+ 0.63 1810.5774 0.16 :fm-index 2.1604 :reverb-amount 0.0020
    :amp-env '( 0 0 0.4762 1 4.4974 0.6 9.5238 0.3 24.6032 0.1 100.0 0 )
    :noise-amount 0.0100 vln-one-sin-ran
  beg 0.0400 f+ 0.9 603.5612 0.16 :fm-index 2.4204 :reverb-amount 0.0031
    :amp-env '( 0 0 0.3333 1 4.3603 0.6 9.3939 0.3 24.4950 0.1 100.0 0 )
    :noise-amount 0.0100 vln-one-sin-ran
  beg 0.0450 f+ 0.4500 348.4765 0.16 :fm-index 2.3918 :reverb-amount 0.0026
    :amp-env '( 0 0 0.6667 1 4.6801 0.6 9.6970 0.3 24.7475 0.1 100 0 )
    :noise-amount 0.0100 vln-one-sin-ran
  beg 0.0460 f+ 0.9 201.1989 0.16 :fm-index 1.5205 :reverb-amount 0.0024
    :amp-env '( 0 0 0.3333 1 4.3603 0.6 9.3939 0.3 24.4950 0.1 100.0 0 )
    :noise-amount 0.0100 vln-one-sin-ran
  beg 0.0460 f+ 0.9 116.1656 0.16 :fm-index 2.3049 :reverb-amount 0.0028
    :amp-env '( 0 0 0.3333 1 4.3603 0.6 9.3939 0.3 24.4950 0.1 100.0 0 )
    :noise-amount 0.0100 vln-one-sin-ran
  beg 0.0500 f+ 0.9 3135.9200 0.16 :fm-index 2.4363 :reverb-amount 0.0021
    :amp-env '( 0 0 0.3333 1 4.3603 0.6 9.3939 0.3 24.4950 0.1 100.0 0 )
    :noise-amount 0.0100 vln-one-sin-ran
  beg 0.0500 f+ 0.45 1464.6987 0.16 :fm-index 2.3865 :reverb-amount 0.0027
    :amp-env '( 0 0 0.6667 1 4.6801 0.6 9.6970 0.3 24.7475 0.1 100 0 )
    :noise-amount 0.0100 vln-one-sin-ran
  beg 0.0600 f+ 0.9 6714.0048 0.16 :fm-index 1.7354 :reverb-amount 0.0021
    :amp-env '( 0 0 0.3333 1 4.3603 0.6 9.3939 0.3 24.4950 0.1 100.0 0 )
    :noise-amount 0.0100 vln-one-sin-ran
  beg 0.0650 f+ 0.9 684.1190 0.16 :fm-index 1.8282 :reverb-amount 0.0025
    :amp-env '( 0 0 0.3333 1 4.3603 0.6 9.3939 0.3 24.4950 0.1 100.0 0 )
    :noise-amount 0.0100 vln-one-sin-ran
  beg 0.0700 f+ 0.2700 684.1190 0.16 :fm-index 2.3923 :reverb-amount 0.0025
    :amp-env '( 0 0 1.1111 1 5.1066 0.6 10.1010 0.3 25.0842 0.1 100.0 0 )
    :noise-amount 0.0100 vln-one-sin-ran
  beg 0.0700 f+ 0.63 1464.6987 0.16 :fm-index 2.2789 :reverb-amount 0.0028
    :amp-env '( 0 0 0.4762 1 4.4974 0.6 9.5238 0.3 24.6032 0.1 100.0 0 )
    :noise-amount 0.0100 vln-one-sin-ran
  beg 0.0800 f+ 0.9 319.5325 0.16 :fm-index 1.5438 :reverb-amount 0.0027
    :amp-env '( 0 0 0.3333 1 4.3603 0.6 9.3939 0.3 24.4950 0.1 100.0 0 )
    :noise-amount 0.0100 vln-one-sin-ran
  beg 0.0850 f+ 0.4500 149.2445 0.16 :fm-index 2.4210 :reverb-amount 0.0028
    :amp-env '( 0 0 0.6667 1 4.6801 0.6 9.6970 0.3 24.7475 0.1 100 0 )
    :noise-amount 0.0100 vln-one-sin-ran
  beg 0.0860 f+ 0.9 69.7078 0.16 :fm-index 2.0288 :reverb-amount 0.0029
    :amp-env '( 0 0 0.3333 1 4.3603 0.6 9.3939 0.3 24.4950 0.1 100.0 0 )
    :noise-amount 0.0100 vln-one-sin-ran
  beg 0.0860 f+ 0.9 32.5585 0.16 :fm-index 1.8254 :reverb-amount 0.0028
    :amp-env '( 0 0 0.3333 1 4.3603 0.6 9.3939 0.3 24.4950 0.1 100.0 0 )
    :noise-amount 0.0100 vln-one-sin-ran

  beg 3 + dup to beg test-info

  \ 13, score 75
  0    to fmv-reverb-amount
  0.01 to fmv-noise-amount
  beg 0.0500 f+ 0.9  3135.9200 0.16 :fm-index 1.7334
    :amp-env '( 0 0 0.3333 1 4.3603 0.6 9.3939 0.3 24.4950 0.1 100.0 0 )
    vln-one-sin-ran
  beg 0.0500 f+ 0.45 1810.5774 0.16 :fm-index 2.3629
    :amp-env '( 0 0 0.6667 1 4.6801 0.6 9.6970 0.3 24.7475 0.1 100 0 )
    vln-one-sin-ran
  beg 0.0600 f+ 0.9  5431.4135 0.16 :fm-index 2.2744
    :amp-env '( 0 0 0.3333 1 4.3603 0.6 9.3939 0.3 24.4950 0.1 100.0 0 )
    vln-one-sin-ran
  beg 0.0650 f+ 0.9  1045.3680 0.16 :fm-index 1.8722
    :amp-env '( 0 0 0.3333 1 4.3603 0.6 9.3939 0.3 24.4950 0.1 100.0 0 )
    vln-one-sin-ran
  beg 0.1100 f+ 0.27 1045.3680 0.16 :fm-index 2.3139
    :amp-env '( 0 0 1.1111 1 5.1066 0.6 10.101 0.3 25.0842 0.1 100.0 0 )
    vln-one-sin-ran
  beg 0.1100 f+ 0.63 1810.5774 0.16 :fm-index 1.6216
    :amp-env '( 0 0 0.4762 1 4.4974 0.6 9.5238 0.3 24.6032 0.1 100.0 0 )
    vln-one-sin-ran
  beg 0.1200 f+ 0.9   603.5612 0.16 :fm-index 1.5308
    :amp-env '( 0 0 0.3333 1 4.3603 0.6 9.3939 0.3 24.4950 0.1 100.0 0 )
    vln-one-sin-ran
  beg 0.1650 f+ 0.45  348.4765 0.16 :fm-index 2.0346
    :amp-env '( 0 0 0.6667 1 4.6801 0.6 9.6970 0.3 24.7475 0.1 100 0 )
    vln-one-sin-ran
  beg 0.1660 f+ 0.9   201.1989 0.16 :fm-index 1.8176
    :amp-env '( 0 0 0.3333 1 4.3603 0.6 9.3939 0.3 24.4950 0.1 100.0 0 )
    vln-one-sin-ran
  beg 0.1660 f+ 0.9   116.1656 0.16 :fm-index 1.7145
    :amp-env '( 0 0 0.3333 1 4.3603 0.6 9.3939 0.3 24.4950 0.1 100.0 0 )
    vln-one-sin-ran
  beg 0.1700 f+ 0.9  3135.9200 0.16 :fm-index 2.4459
    :amp-env '( 0 0 0.3333 1 4.3603 0.6 9.3939 0.3 24.4950 0.1 100.0 0 )
    vln-one-sin-ran
  beg 0.1700 f+ 0.45 1464.6987 0.16 :fm-index 2.4644
    :amp-env '( 0 0 0.6667 1 4.6801 0.6 9.6970 0.3 24.7475 0.1 100 0 )
    vln-one-sin-ran
  beg 0.1800 f+ 0.9  6714.0048 0.16 :fm-index 1.9985
    :amp-env '( 0 0 0.3333 1 4.3603 0.6 9.3939 0.3 24.4950 0.1 100.0 0 )
    vln-one-sin-ran
  beg 0.1850 f+ 0.9   684.1190 0.16 :fm-index 2.4542
    :amp-env '( 0 0 0.3333 1 4.3603 0.6 9.3939 0.3 24.4950 0.1 100.0 0 )
    vln-one-sin-ran
  beg 0.2900 f+ 0.27  684.1190 0.16 :fm-index 2.3391
    :amp-env '( 0 0 1.1111 1 5.1066 0.6 10.101 0.3 25.0842 0.1 100.0 0 )
    vln-one-sin-ran
  beg 0.2900 f+ 0.63 1464.6987 0.16 :fm-index 1.5138
    :amp-env '( 0 0 0.4762 1 4.4974 0.6 9.5238 0.3 24.6032 0.1 100.0 0 )
    vln-one-sin-ran
  beg 0.3    f+ 0.9   319.5325 0.16 :fm-index 1.5440
    :amp-env '( 0 0 0.3333 1 4.3603 0.6 9.3939 0.3 24.4950 0.1 100.0 0 )
    vln-one-sin-ran
  beg 0.3050 f+ 0.45  149.2445 0.16 :fm-index 2.2283
    :amp-env '( 0 0 0.6667 1 4.6801 0.6 9.6970 0.3 24.7475 0.1 100 0 )
    vln-one-sin-ran
  beg 0.3060 f+ 0.9    69.7078 0.16 :fm-index 1.9498
    :amp-env '( 0 0 0.3333 1 4.3603 0.6 9.3939 0.3 24.4950 0.1 100.0 0 )
    vln-one-sin-ran
  beg 0.3060 f+ 0.9    32.5585 0.16 :fm-index 2.2943
    :amp-env '( 0 0 0.3333 1 4.3603 0.6 9.3939 0.3 24.4950 0.1 100.0 0 )
    vln-one-sin-ran

  beg 3 + dup to beg test-info

  \ 14, score 78
  restore-fm-violin-defaults
  metalamp to fmv-amp-env
  0     to fmv-glissando-amount
  0.01  to fmv-reverb-amount
  2.718 to fmv-fm1-rat
  1.414 to fmv-fm2-rat
  3.141 to fmv-fm3-rat
  beg 0.2600 f+ 1.2 355.5416 0.16 :fm-index 2.0375 vln-one-sin
  beg 0.2600 f+ 1.5 354.8319 0.16 :fm-index 1.8744 vln-one-sin
  beg 0.2603 f+ 0.9 356.2527 0.16 :fm-index 1.8743 vln-one-sin
  beg 0.2605 f+ 0.9 409.2356 0.16 :fm-index 2.0808 vln-one-sin
  beg 0.2605 f+ 2.1 410.0541 0.16 :fm-index 1.9219 vln-one-sin
  beg 0.2608 f+ 1.2 130.8100 0.16 :fm-index 1.5746 vln-one-sin
  beg 0.2613 f+ 3   130.2883 0.16 :fm-index 2.3771 vln-one-sin
  beg 0.2615 f+ 0.9 130.2883 0.16 :fm-index 1.7765 vln-one-sin
  beg 0.2615 f+ 2.1 130.5489 0.26 :fm-index 1.6485 vln-one-sin
  beg 0.2625 f+ 2   130.8100 0.16 :fm-index 2.1416 vln-one-sin
  beg 0.2633 f+ 2   130.5488 0.16 :fm-index 2.0883 vln-one-sin

  beg 4 + dup to beg test-info

  \ 15, score 82
  '( 0 0 0.5 1 5 1 10 0.5 15 0.25 35 0.1 100 0 ) to fmv-amp-env
  0.0001 to fmv-noise-amount
  0.01   to fmv-reverb-amount
  beg 0.2605 f+ 0.8  523.2400 0.16 :fm-index 2.3056 vln-one-sin-ran
  beg 0.2605 f+ 1    247.1611 0.16 :fm-index 1.6308 vln-one-sin-ran
  beg 0.2610 f+ 0.6 1107.6991 0.16 :fm-index 1.9364 vln-one-sin-ran
  beg 0.2613 f+ 2    116.7506 0.16 :fm-index 2.3740 vln-one-sin-ran
  beg 0.2615 f+ 0.6  116.7506 0.16 :fm-index 1.8374 vln-one-sin-ran
  beg 0.2615 f+ 1.4  247.1611 0.16 :fm-index 1.7250 vln-one-sin-ran
  beg 0.2620 f+ 0.6   55.1491 0.16 :fm-index 1.5495 vln-one-sin-ran
  beg 0.2623 f+ 1     26.0506 0.16 :fm-index 1.7235 vln-one-sin-ran
  beg 0.2623 f+ 2     12.3054 0.16 :fm-index 1.8818 vln-one-sin-ran
  beg 0.2623 f+ 2      5.8127 0.16 :fm-index 1.9537 vln-one-sin-ran
  beg 0.2625 f+ 2    523.2400 0.16 :fm-index 2.1593 vln-one-sin-ran
  beg 0.2625 f+ 1    256.2390 0.16 :fm-index 1.9851 vln-one-sin-ran
  beg 0.2630 f+ 2   1068.4561 0.16 :fm-index 1.8015 vln-one-sin-ran
  beg 0.2633 f+ 2    125.4843 0.16 :fm-index 1.6161 vln-one-sin-ran
  beg 0.2635 f+ 0.6  125.4843 0.16 :fm-index 2.2767 vln-one-sin-ran
  beg 0.2635 f+ 1.4  256.2390 0.16 :fm-index 2.0835 vln-one-sin-ran
  beg 0.2640 f+ 0.4   61.4517 0.16 :fm-index 1.5310 vln-one-sin-ran
  beg 0.2643 f+ 1     30.0939 0.16 :fm-index 1.5803 vln-one-sin-ran
  beg 0.2643 f+ 2     14.7374 0.16 :fm-index 1.9586 vln-one-sin-ran
  beg 0.2643 f+ 2      7.2172 0.16 :fm-index 1.7270 vln-one-sin-ran
  beg 0.2645 f+ 6     28.4710 0.16 :fm-index 1.5983 vln-one-sin-ran
  beg 0.2648 f+ 10    25.6239 0.16 :fm-index 1.7285 vln-one-sin-ran
  beg 0.2648 f+ 8     21.3532 0.16 :fm-index 1.7955 vln-one-sin-ran
  beg 0.2648 f+ 8     17.0826 0.16 :fm-index 2.0866 vln-one-sin-ran

  beg 4 + dup to beg test-info

  \ 16, score 86
  0.001 to fmv-reverb-amount
  0.004 to fmv-noise-amount
  3.141 to fmv-fm1-rat
  1.414 to fmv-fm2-rat
  2.718 to fmv-fm3-rat
  beg 0.2600 f+ 1.6 1643.4968 0.16 :fm-index 2.1104 vln-one-sin-ran
  beg 0.2600 f+ 2   1643.4968 0.16 :fm-index 1.5191 vln-one-sin-ran
  beg 0.2603 f+ 1.2 1643.4968 0.16 :fm-index 2.0478 vln-one-sin-ran
  beg 0.2603 f+ 4   1643.4968 0.16 :fm-index 2.0473 vln-one-sin-ran
  beg 0.2605 f+ 1.2 1422.1663 0.16 :fm-index 1.9845 vln-one-sin-ran
  beg 0.2605 f+ 2.8 1422.1663 0.16 :fm-index 2.0429 vln-one-sin-ran
  beg 0.2605 f+ 1.2 1422.1663 0.16 :fm-index 1.6184 vln-one-sin-ran
  beg 0.2608 f+ 1.6  523.2400 0.16 :fm-index 2.3908 vln-one-sin-ran
  beg 0.2608 f+ 2    523.2400 0.16 :fm-index 1.6733 vln-one-sin-ran
  beg 0.2610 f+ 1.2  523.2400 0.16 :fm-index 2.0431 vln-one-sin-ran
  beg 0.2610 f+ 4    523.2400 0.16 :fm-index 1.7430 vln-one-sin-ran
  beg 0.2613 f+ 1.2  523.2400 0.16 :fm-index 2.2030 vln-one-sin-ran
  beg 0.2613 f+ 2.8  523.2400 0.16 :fm-index 2.0149 vln-one-sin-ran
  beg 0.2615 f+ 1.2  523.2400 0.16 :fm-index 2.2310 vln-one-sin-ran
  beg 0.2615 f+ 2    523.2400 0.16 :fm-index 2.1625 vln-one-sin-ran
  beg 0.2618 f+ 4    523.2400 0.16 :fm-index 2.0000 vln-one-sin-ran
  beg 0.2618 f+ 4    523.2400 0.16 :fm-index 2.2034 vln-one-sin-ran
  beg 0.2620 f+ 3    523.2400 0.16 :fm-index 2.0186 vln-one-sin-ran
  beg 0.2620 f+ 1.5  523.2400 0.16 :fm-index 2.1373 vln-one-sin-ran
  beg 0.2623 f+ 3    523.2400 0.16 :fm-index 1.9046 vln-one-sin-ran
  beg 0.2623 f+ 3    523.2400 0.16 :fm-index 2.1834 vln-one-sin-ran
  beg 0.2625 f+ 1.2  523.2400 0.16 :fm-index 1.8266 vln-one-sin-ran
  beg 0.2625 f+ 2.8  523.2400 0.16 :fm-index 1.5937 vln-one-sin-ran
  beg 0.2628 f+ 0.8  523.2400 0.16 :fm-index 1.9762 vln-one-sin-ran
  beg 0.2628 f+ 2    523.2400 0.16 :fm-index 1.8954 vln-one-sin-ran
  beg 0.2630 f+ 4    523.2400 0.16 :fm-index 2.3302 vln-one-sin-ran
  beg 0.2630 f+ 4    523.2400 0.16 :fm-index 2.4949 vln-one-sin-ran

  beg 4 + dup to beg test-info

  \ 17, score 90
  3.414 to fmv-fm1-rat
  1.414 to fmv-fm2-rat
  2.718 to fmv-fm3-rat
  beg 0.2600 f+ 1.6 821.7484 0.16 :fm-index 2.4793 vln-one-sin-ran
  beg 0.2600 f+ 2   821.7484 0.16 :fm-index 2.4789 vln-one-sin-ran
  beg 0.2603 f+ 1.2 821.7484 0.16 :fm-index 2.0827 vln-one-sin-ran
  beg 0.2603 f+ 4   821.7484 0.16 :fm-index 2.4769 vln-one-sin-ran
  beg 0.2605 f+ 1.2 711.0832 0.16 :fm-index 2.4094 vln-one-sin-ran
  beg 0.2605 f+ 2.8 711.0832 0.16 :fm-index 2.4031 vln-one-sin-ran
  beg 0.2605 f+ 1.2 711.0832 0.16 :fm-index 2.1428 vln-one-sin-ran
  beg 0.2608 f+ 1.6 261.6200 0.16 :fm-index 2.3129 vln-one-sin-ran
  beg 0.2608 f+ 2   261.6200 0.16 :fm-index 2.3488 vln-one-sin-ran
  beg 0.2610 f+ 1.2 261.6200 0.16 :fm-index 2.1466 vln-one-sin-ran
  beg 0.2610 f+ 4   261.6200 0.16 :fm-index 1.6938 vln-one-sin-ran
  beg 0.2613 f+ 1.2 261.6200 0.16 :fm-index 2.1287 vln-one-sin-ran
  beg 0.2613 f+ 2.8 261.6200 0.16 :fm-index 2.1917 vln-one-sin-ran
  beg 0.2615 f+ 1.2 261.6200 0.16 :fm-index 2.3583 vln-one-sin-ran
  beg 0.2615 f+ 2   261.6200 0.16 :fm-index 1.8368 vln-one-sin-ran
  beg 0.2618 f+ 4   261.6200 0.16 :fm-index 1.5107 vln-one-sin-ran
  beg 0.2618 f+ 4   261.6200 0.16 :fm-index 1.6218 vln-one-sin-ran
  beg 0.2620 f+ 3   261.6200 0.16 :fm-index 1.9041 vln-one-sin-ran
  beg 0.2620 f+ 1.5 261.6200 0.16 :fm-index 1.5748 vln-one-sin-ran
  beg 0.2623 f+ 3   261.6200 0.16 :fm-index 1.9339 vln-one-sin-ran
  beg 0.2623 f+ 3   261.6200 0.16 :fm-index 2.0489 vln-one-sin-ran
  beg 0.2625 f+ 1.2 261.6200 0.16 :fm-index 2.0888 vln-one-sin-ran
  beg 0.2625 f+ 2.8 261.6200 0.16 :fm-index 1.7306 vln-one-sin-ran
  beg 0.2628 f+ 0.8 261.6200 0.16 :fm-index 2.3257 vln-one-sin-ran
  beg 0.2628 f+ 2   261.6200 0.16 :fm-index 2.4755 vln-one-sin-ran
  beg 0.2630 f+ 4   261.6200 0.16 :fm-index 1.9459 vln-one-sin-ran
  beg 0.2630 f+ 4   261.6200 0.16 :fm-index 1.5782 vln-one-sin-ran
  
  beg 4 + dup to beg test-info

  \ 18, score 94
  3.414 to fmv-fm1-rat
  1.414 to fmv-fm2-rat
  2.718 to fmv-fm3-rat
  beg 0.2600 f+ 1.6 3286.9937 0.16 :fm-index 1.6655 vln-one-sin-ran
  beg 0.2600 f+ 2   3286.9937 0.16 :fm-index 1.9356 vln-one-sin-ran
  beg 0.2603 f+ 1.2 3286.9937 0.16 :fm-index 1.5665 vln-one-sin-ran
  beg 0.2603 f+ 4   3286.9937 0.16 :fm-index 1.6701 vln-one-sin-ran
  beg 0.2605 f+ 1.2 2844.3326 0.16 :fm-index 2.3273 vln-one-sin-ran
  beg 0.2605 f+ 2.8 2844.3326 0.16 :fm-index 1.5520 vln-one-sin-ran
  beg 0.2605 f+ 1.2 2844.3326 0.16 :fm-index 2.4104 vln-one-sin-ran
  beg 0.2608 f+ 1.6 1046.4800 0.16 :fm-index 2.1075 vln-one-sin-ran
  beg 0.2608 f+ 2   1046.4800 0.16 :fm-index 1.7004 vln-one-sin-ran
  beg 0.2610 f+ 1.2 1046.4800 0.16 :fm-index 1.6502 vln-one-sin-ran
  beg 0.2610 f+ 4   1046.4800 0.16 :fm-index 2.4591 vln-one-sin-ran
  beg 0.2613 f+ 1.2 1046.4800 0.16 :fm-index 2.1491 vln-one-sin-ran
  beg 0.2613 f+ 2.8 1046.4800 0.16 :fm-index 2.1594 vln-one-sin-ran
  beg 0.2615 f+ 1.2 1046.4800 0.16 :fm-index 2.4783 vln-one-sin-ran
  beg 0.2615 f+ 2   1046.4800 0.16 :fm-index 2.2080 vln-one-sin-ran
  beg 0.2618 f+ 4   1046.4800 0.16 :fm-index 1.5844 vln-one-sin-ran
  beg 0.2618 f+ 4   1046.4800 0.16 :fm-index 1.5440 vln-one-sin-ran
  beg 0.2620 f+ 3   1046.4800 0.16 :fm-index 1.9857 vln-one-sin-ran
  beg 0.2620 f+ 1.5 1046.4800 0.16 :fm-index 1.5165 vln-one-sin-ran
  beg 0.2623 f+ 3   1046.4800 0.16 :fm-index 1.8309 vln-one-sin-ran
  beg 0.2623 f+ 3   1046.4800 0.16 :fm-index 2.1236 vln-one-sin-ran
  beg 0.2625 f+ 1.2 1046.4800 0.1  :fm-index 2.4074 vln-one-sin-ran
  beg 0.2625 f+ 2.8 1046.4800 0.1  :fm-index 1.6315 vln-one-sin-ran
  beg 0.2628 f+ 0.8 1046.4800 0.1  :fm-index 1.8061 vln-one-sin-ran
  beg 0.2628 f+ 2   1046.4800 0.1  :fm-index 2.3664 vln-one-sin-ran
  beg 0.2630 f+ 4   1046.4800 0.1  :fm-index 2.2490 vln-one-sin-ran
  beg 0.2630 f+ 4   1046.4800 0.1  :fm-index 2.4081 vln-one-sin-ran
  
  beg 4 + dup to beg test-info

  \ 19, score 98
  0.01 to fmv-reverb-amount
  beg 0.2600 f+ 1.6 1643.4968 0.16 :fm-index 1.9284 vln-one-sin-ran
  beg 0.2600 f+ 2   1643.4968 0.16 :fm-index 2.2171 vln-one-sin-ran
  beg 0.2603 f+ 1.2 1643.4968 0.16 :fm-index 2.2272 vln-one-sin-ran
  beg 0.2603 f+ 4   1643.4968 0.16 :fm-index 1.5677 vln-one-sin-ran
  beg 0.2605 f+ 1.2 1422.1663 0.16 :fm-index 2.0476 vln-one-sin-ran
  beg 0.2605 f+ 2.8 1422.1663 0.16 :fm-index 2.3289 vln-one-sin-ran
  beg 0.2605 f+ 1.2 1422.1663 0.16 :fm-index 2.0269 vln-one-sin-ran
  beg 0.2608 f+ 1.6  523.2400 0.16 :fm-index 1.7767 vln-one-sin-ran
  beg 0.2608 f+ 2    523.2400 0.16 :fm-index 1.8117 vln-one-sin-ran
  beg 0.2610 f+ 1.2  523.2400 0.16 :fm-index 1.5694 vln-one-sin-ran
  beg 0.2610 f+ 4    523.2400 0.16 :fm-index 1.6869 vln-one-sin-ran
  beg 0.2613 f+ 1.2  523.2400 0.16 :fm-index 1.9340 vln-one-sin-ran
  beg 0.2613 f+ 2.8  523.2400 0.16 :fm-index 2.3986 vln-one-sin-ran
  beg 0.2615 f+ 1.2  523.2400 0.16 :fm-index 2.4593 vln-one-sin-ran
  beg 0.2615 f+ 2    523.2400 0.16 :fm-index 2.3430 vln-one-sin-ran
  beg 0.2618 f+ 4    523.2400 0.16 :fm-index 2.2650 vln-one-sin-ran
  beg 0.2618 f+ 4    523.2400 0.16 :fm-index 2.3015 vln-one-sin-ran
  beg 0.2620 f+ 3    523.2400 0.16 :fm-index 1.9909 vln-one-sin-ran
  beg 0.2620 f+ 1.5  523.2400 0.16 :fm-index 2.3916 vln-one-sin-ran
  beg 0.2623 f+ 3    523.2400 0.16 :fm-index 2.0401 vln-one-sin-ran
  beg 0.2623 f+ 3    523.2400 0.16 :fm-index 1.8484 vln-one-sin-ran
  beg 0.2625 f+ 1.2  523.2400 0.16 :fm-index 2.3138 vln-one-sin-ran
  beg 0.2625 f+ 2.8  523.2400 0.16 :fm-index 1.6295 vln-one-sin-ran
  beg 0.2628 f+ 0.8  523.2400 0.16 :fm-index 2.2344 vln-one-sin-ran
  beg 0.2628 f+ 2    523.2400 0.16 :fm-index 1.8423 vln-one-sin-ran
  beg 0.2630 f+ 4    523.2400 0.16 :fm-index 2.2086 vln-one-sin-ran
  beg 0.2630 f+ 4    523.2400 0.16 :fm-index 2.3130 vln-one-sin-ran
  
  beg 4 + dup to beg test-info

  \ 20, score 102
  0.0001 to fmv-noise-amount
  0.01   to fmv-reverb-amount
  2.718  to fmv-fm1-rat
  1.141  to fmv-fm2-rat
  3.141  to fmv-fm3-rat
  beg 0.2605 f+ 0.8 523.2400 0.16 :fm-index 2.0123 vln-one-sin-ran
  beg 0.2605 f+ 1   493.8728 0.16 :fm-index 2.1176 vln-one-sin-ran
  beg 0.2610 f+ 0.6 554.3535 0.16 :fm-index 1.9163 vln-one-sin-ran
  beg 0.2613 f+ 2   466.1539 0.16 :fm-index 1.5048 vln-one-sin-ran
  beg 0.2615 f+ 0.6 466.1539 0.16 :fm-index 1.5242 vln-one-sin-ran
  beg 0.2615 f+ 1.4 493.8728 0.16 :fm-index 1.9509 vln-one-sin-ran
  beg 0.2620 f+ 0.6 439.9907 0.16 :fm-index 2.2131 vln-one-sin-ran
  beg 0.2623 f+ 1   415.2959 0.16 :fm-index 1.7326 vln-one-sin-ran
  beg 0.2623 f+ 2   391.9871 0.16 :fm-index 1.9936 vln-one-sin-ran
  beg 0.2623 f+ 2   369.9866 0.16 :fm-index 2.1103 vln-one-sin-ran
  beg 0.2625 f+ 2   523.2400 0.16 :fm-index 1.6206 vln-one-sin-ran
  beg 0.2625 f+ 1   522.7173 0.16 :fm-index 1.8598 vln-one-sin-ran
  beg 0.2630 f+ 2   523.7632 0.16 :fm-index 1.8015 vln-one-sin-ran
  beg 0.2633 f+ 2   522.1951 0.16 :fm-index 2.3575 vln-one-sin-ran
  beg 0.2635 f+ 0.6 522.1951 0.16 :fm-index 1.5010 vln-one-sin-ran
  beg 0.2635 f+ 1.4 522.7173 0.16 :fm-index 2.4075 vln-one-sin-ran
  beg 0.2640 f+ 0.4 521.6734 0.16 :fm-index 2.0721 vln-one-sin-ran
  beg 0.2643 f+ 1   521.1523 0.16 :fm-index 2.0433 vln-one-sin-ran
  beg 0.2643 f+ 2   520.6316 0.16 :fm-index 1.9788 vln-one-sin-ran
  beg 0.2643 f+ 2   520.1115 0.16 :fm-index 1.6770 vln-one-sin-ran
  
  beg 8 + dup to beg test-info

  \ 21, score 110
  0.004 to fmv-noise-amount
  beg 0.2600 f+ 0.8 1046.4800 0.16 :fm-index 1.5610 vln-one-sin-ran
  beg 0.2600 f+ 1   1044.3912 0.16 :fm-index 2.3514 vln-one-sin-ran
  beg 0.2603 f+ 0.6 1048.5730 0.16 :fm-index 1.9958 vln-one-sin-ran
  beg 0.2603 f+ 2   1042.3066 0.16 :fm-index 1.9654 vln-one-sin-ran
  beg 0.2605 f+ 0.6 1042.3066 0.16 :fm-index 1.5285 vln-one-sin-ran
  beg 0.2605 f+ 1.4 1044.3912 0.16 :fm-index 1.8881 vln-one-sin-ran
  beg 0.2608 f+ 0.6 1040.2262 0.16 :fm-index 1.8682 vln-one-sin-ran
  beg 0.2608 f+ 0.8  523.2400 0.16 :fm-index 1.8296 vln-one-sin-ran
  beg 0.2610 f+ 1    522.1956 0.16 :fm-index 2.1899 vln-one-sin-ran
  beg 0.2610 f+ 0.6  524.2865 0.16 :fm-index 1.9614 vln-one-sin-ran
  beg 0.2613 f+ 2    521.1533 0.16 :fm-index 1.7483 vln-one-sin-ran
  beg 0.2615 f+ 0.6  521.1533 0.16 :fm-index 1.8717 vln-one-sin-ran
  beg 0.2615 f+ 1.4  522.1956 0.16 :fm-index 1.5619 vln-one-sin-ran
  beg 0.2620 f+ 0.6  520.1131 0.16 :fm-index 2.4331 vln-one-sin-ran
  beg 0.2623 f+ 1    519.0749 0.16 :fm-index 2.4153 vln-one-sin-ran
  beg 0.2623 f+ 2    518.0388 0.16 :fm-index 1.5477 vln-one-sin-ran
  beg 0.2623 f+ 2    517.0048 0.16 :fm-index 1.9956 vln-one-sin-ran
  beg 0.2625 f+ 2    523.2400 0.16 :fm-index 1.8111 vln-one-sin-ran
  beg 0.2625 f+ 1    522.7173 0.16 :fm-index 2.4820 vln-one-sin-ran
  beg 0.2630 f+ 2    523.7632 0.16 :fm-index 1.5744 vln-one-sin-ran
  beg 0.2633 f+ 2    522.1951 0.16 :fm-index 1.9950 vln-one-sin-ran
  beg 0.2635 f+ 0.6  522.1951 0.16 :fm-index 1.9792 vln-one-sin-ran
  beg 0.2635 f+ 1.4  522.7173 0.16 :fm-index 1.7415 vln-one-sin-ran
  beg 0.2640 f+ 0.4  521.6734 0.16 :fm-index 2.0884 vln-one-sin-ran
  beg 0.2643 f+ 1    521.1523 0.16 :fm-index 2.3605 vln-one-sin-ran
  beg 0.2643 f+ 2    520.6316 0.16 :fm-index 1.7817 vln-one-sin-ran
  beg 0.2643 f+ 2    520.1115 0.16 :fm-index 2.0283 vln-one-sin-ran
  
  beg 4 + dup to beg test-info

  \ 22, score 114
  0.1   to fmv-reverb-amount
  0     to fmv-glissando-amount
  2.718 to fmv-fm1-rat
  1.414 to fmv-fm2-rat
  3.141 to fmv-fm3-rat
  beg 0.2600 f+ 1.6 177.7708 0.16 :fm-index 1.6447 vln-one-sin-ran
  beg 0.2600 f+ 2   177.7708 0.16 :fm-index 2.4875 vln-one-sin-ran
  beg 0.2603 f+ 1.2 177.7708 0.16 :fm-index 1.6126 vln-one-sin-ran
  beg 0.2603 f+ 4   177.7708 0.16 :fm-index 2.3122 vln-one-sin-ran
  beg 0.2605 f+ 1.2 205.4371 0.16 :fm-index 2.4116 vln-one-sin-ran
  beg 0.2605 f+ 2.8 205.4371 0.16 :fm-index 1.5337 vln-one-sin-ran
  beg 0.2608 f+ 1.2 205.4371 0.16 :fm-index 2.0307 vln-one-sin-ran
  beg 0.2608 f+ 1.6  65.4050 0.16 :fm-index 2.2341 vln-one-sin-ran
  beg 0.2610 f+ 2    65.4050 0.16 :fm-index 2.4683 vln-one-sin-ran
  beg 0.2610 f+ 1.2  65.4050 0.16 :fm-index 2.0643 vln-one-sin-ran
  beg 0.2613 f+ 4    65.4050 0.16 :fm-index 2.1925 vln-one-sin-ran
  beg 0.2615 f+ 1.2  65.4050 0.16 :fm-index 2.1325 vln-one-sin-ran
  beg 0.2615 f+ 2.8  65.4050 0.16 :fm-index 1.5847 vln-one-sin-ran
  beg 0.2620 f+ 1.2  65.4050 0.16 :fm-index 1.8781 vln-one-sin-ran
  beg 0.2623 f+ 2    65.4050 0.16 :fm-index 2.0283 vln-one-sin-ran
  beg 0.2623 f+ 4    65.4050 0.16 :fm-index 2.4739 vln-one-sin-ran
  beg 0.2623 f+ 4    65.4050 0.16 :fm-index 2.2333 vln-one-sin-ran
  beg 0.2625 f+ 2    65.4050 0.16 :fm-index 2.2194 vln-one-sin-ran
  beg 0.2625 f+ 1    65.4050 0.16 :fm-index 2.4491 vln-one-sin-ran
  beg 0.2630 f+ 2    65.4050 0.16 :fm-index 1.5672 vln-one-sin-ran
  beg 0.2633 f+ 2    65.4050 0.16 :fm-index 2.3254 vln-one-sin-ran
  beg 0.2635 f+ 1.2  65.4050 0.16 :fm-index 1.8302 vln-one-sin-ran
  beg 0.2635 f+ 2.8  65.4050 0.16 :fm-index 1.9201 vln-one-sin-ran
  beg 0.2640 f+ 0.8  65.4050 0.16 :fm-index 1.9164 vln-one-sin-ran
  beg 0.2643 f+ 2    65.4050 0.16 :fm-index 1.9483 vln-one-sin-ran
  beg 0.2643 f+ 4    65.4050 0.16 :fm-index 2.4247 vln-one-sin-ran
  beg 0.2643 f+ 4    65.4050 0.16 :fm-index 2.0419 vln-one-sin-ran
  
  beg 4 + dup to beg test-info

  \ 23, score 118
  2.718 to fmv-fm1-rat
  4.414 to fmv-fm2-rat
  3.141 to fmv-fm3-rat
  beg 0.2600 f+ 1.6  88.8854 0.16 :fm-index 2.2832 vln-one-sin-ran
  beg 0.2600 f+ 2    88.8854 0.16 :fm-index 1.6588 vln-one-sin-ran
  beg 0.2603 f+ 1.2  88.8854 0.16 :fm-index 2.2392 vln-one-sin-ran
  beg 0.2603 f+ 4    88.8854 0.16 :fm-index 1.7354 vln-one-sin-ran
  beg 0.2605 f+ 1.2 102.7186 0.16 :fm-index 1.6692 vln-one-sin-ran
  beg 0.2605 f+ 2.8 102.7186 0.16 :fm-index 2.1518 vln-one-sin-ran
  beg 0.2608 f+ 1.2 102.7186 0.16 :fm-index 2.2439 vln-one-sin-ran
  beg 0.2608 f+ 1.6  32.7025 0.16 :fm-index 2.1665 vln-one-sin-ran
  beg 0.2610 f+ 2    32.7025 0.16 :fm-index 1.7947 vln-one-sin-ran
  beg 0.2610 f+ 1.2  32.7025 0.16 :fm-index 2.0740 vln-one-sin-ran
  beg 0.2613 f+ 4    32.7025 0.16 :fm-index 1.9705 vln-one-sin-ran
  beg 0.2615 f+ 1.2  32.7025 0.16 :fm-index 1.9447 vln-one-sin-ran
  beg 0.2615 f+ 2.8  32.7025 0.16 :fm-index 2.4918 vln-one-sin-ran
  beg 0.2620 f+ 1.2  32.7025 0.16 :fm-index 1.6275 vln-one-sin-ran
  beg 0.2623 f+ 2    32.7025 0.16 :fm-index 2.2355 vln-one-sin-ran
  beg 0.2623 f+ 4    32.7025 0.16 :fm-index 2.0084 vln-one-sin-ran
  beg 0.2623 f+ 4    32.7025 0.16 :fm-index 1.8964 vln-one-sin-ran
  beg 0.2625 f+ 2    32.7025 0.16 :fm-index 2.3937 vln-one-sin-ran
  beg 0.2625 f+ 1    32.7025 0.16 :fm-index 1.8634 vln-one-sin-ran
  beg 0.2630 f+ 2    32.7025 0.16 :fm-index 1.5217 vln-one-sin-ran
  beg 0.2633 f+ 2    32.7025 0.16 :fm-index 1.9275 vln-one-sin-ran
  beg 0.2635 f+ 1.2  32.7025 0.16 :fm-index 2.4413 vln-one-sin-ran
  beg 0.2635 f+ 2.8  32.7025 0.16 :fm-index 2.3242 vln-one-sin-ran
  beg 0.2640 f+ 0.8  32.7025 0.16 :fm-index 2.3267 vln-one-sin-ran
  beg 0.2643 f+ 2    32.7025 0.16 :fm-index 1.7004 vln-one-sin-ran
  beg 0.2643 f+ 4    32.7025 0.16 :fm-index 1.8785 vln-one-sin-ran
  beg 0.2643 f+ 4    32.7025 0.16 :fm-index 2.4573 vln-one-sin-ran
  
  beg 4 + dup to beg test-info

  \ 24, score 122
  2.718 to fmv-fm1-rat
  4.414 to fmv-fm2-rat
  5.141 to fmv-fm3-rat
  beg 0.2600 f+ 1.6 22.2213 0.16 :fm-index 1.6232 vln-one-sin-ran
  beg 0.2600 f+ 2   22.2213 0.16 :fm-index 1.5982 vln-one-sin-ran
  beg 0.2603 f+ 1.2 22.2213 0.16 :fm-index 2.1585 vln-one-sin-ran
  beg 0.2603 f+ 4   22.2213 0.16 :fm-index 2.2207 vln-one-sin-ran
  beg 0.2605 f+ 1.2 42.0309 0.16 :fm-index 1.5294 vln-one-sin-ran
  beg 0.2605 f+ 2.8 42.0309 0.16 :fm-index 1.9544 vln-one-sin-ran
  beg 0.2608 f+ 1.2 42.0309 0.16 :fm-index 2.4016 vln-one-sin-ran
  beg 0.2608 f+ 1.6  8.1756 0.16 :fm-index 1.5267 vln-one-sin-ran
  beg 0.2610 f+ 2    8.1756 0.16 :fm-index 2.4190 vln-one-sin-ran
  beg 0.2610 f+ 1.2  8.1756 0.16 :fm-index 2.2757 vln-one-sin-ran
  beg 0.2613 f+ 4    8.1756 0.16 :fm-index 2.3607 vln-one-sin-ran
  beg 0.2615 f+ 1.2  8.1756 0.16 :fm-index 1.8698 vln-one-sin-ran
  beg 0.2615 f+ 2.8  8.1756 0.16 :fm-index 2.3753 vln-one-sin-ran
  beg 0.2620 f+ 1.2  8.1756 0.16 :fm-index 2.3392 vln-one-sin-ran
  beg 0.2623 f+ 2    8.1756 0.16 :fm-index 1.5088 vln-one-sin-ran
  beg 0.2623 f+ 4    8.1756 0.16 :fm-index 2.2084 vln-one-sin-ran
  beg 0.2623 f+ 4    8.1756 0.16 :fm-index 1.9512 vln-one-sin-ran
  beg 0.2625 f+ 2    8.1756 0.16 :fm-index 2.0399 vln-one-sin-ran
  beg 0.2625 f+ 1    8.1756 0.16 :fm-index 1.7053 vln-one-sin-ran
  beg 0.2630 f+ 2    8.1756 0.16 :fm-index 2.3204 vln-one-sin-ran
  beg 0.2633 f+ 2    8.1756 0.16 :fm-index 1.6336 vln-one-sin-ran
  beg 0.2635 f+ 1.2  8.1756 0.16 :fm-index 1.9483 vln-one-sin-ran
  beg 0.2635 f+ 2.8  8.1756 0.16 :fm-index 2.3255 vln-one-sin-ran
  beg 0.2640 f+ 0.8  8.1756 0.16 :fm-index 1.7331 vln-one-sin-ran
  beg 0.2643 f+ 2    8.1756 0.16 :fm-index 1.9318 vln-one-sin-ran
  beg 0.2643 f+ 4    8.1756 0.16 :fm-index 1.6908 vln-one-sin-ran
  beg 0.2643 f+ 4    8.1756 0.16 :fm-index 2.4103 vln-one-sin-ran
  
  beg 4 + dup to beg test-info

  \ 25, score 126
  beg 0.2600 f+ 1.6 11.1107 0.16 :fm-index 1.6371 vln-one-sin-ran
  beg 0.2600 f+ 2   11.1107 0.16 :fm-index 1.8971 vln-one-sin-ran
  beg 0.2603 f+ 1.2 11.1107 0.16 :fm-index 1.9065 vln-one-sin-ran
  beg 0.2603 f+ 4   11.1107 0.16 :fm-index 2.2143 vln-one-sin-ran
  beg 0.2605 f+ 1.2 21.0154 0.16 :fm-index 1.8011 vln-one-sin-ran
  beg 0.2605 f+ 2.8 21.0154 0.16 :fm-index 2.1950 vln-one-sin-ran
  beg 0.2608 f+ 1.2 21.0154 0.16 :fm-index 2.3563 vln-one-sin-ran
  beg 0.2608 f+ 1.6  4.0878 0.16 :fm-index 2.3181 vln-one-sin-ran
  beg 0.2610 f+ 2    4.0878 0.16 :fm-index 2.0776 vln-one-sin-ran
  beg 0.2610 f+ 1.2  4.0878 0.16 :fm-index 1.8336 vln-one-sin-ran
  beg 0.2613 f+ 4    4.0878 0.16 :fm-index 1.5019 vln-one-sin-ran
  beg 0.2615 f+ 1.2  4.0878 0.16 :fm-index 2.2368 vln-one-sin-ran
  beg 0.2615 f+ 2.8  4.0878 0.16 :fm-index 1.7462 vln-one-sin-ran
  beg 0.2620 f+ 1.2  4.0878 0.16 :fm-index 1.9604 vln-one-sin-ran
  beg 0.2623 f+ 2    4.0878 0.16 :fm-index 2.2361 vln-one-sin-ran
  beg 0.2623 f+ 4    4.0878 0.16 :fm-index 1.9972 vln-one-sin-ran
  beg 0.2623 f+ 4    4.0878 0.16 :fm-index 2.4870 vln-one-sin-ran
  beg 0.2625 f+ 2    4.0878 0.16 :fm-index 2.0762 vln-one-sin-ran
  beg 0.2625 f+ 1    4.0878 0.16 :fm-index 2.2973 vln-one-sin-ran
  beg 0.2630 f+ 2    4.0878 0.16 :fm-index 2.2350 vln-one-sin-ran
  beg 0.2633 f+ 2    4.0878 0.16 :fm-index 2.1613 vln-one-sin-ran
  beg 0.2635 f+ 1.2  4.0878 0.16 :fm-index 2.0640 vln-one-sin-ran
  beg 0.2635 f+ 2.8  4.0878 0.16 :fm-index 2.1738 vln-one-sin-ran
  beg 0.2640 f+ 0.8  4.0878 0.16 :fm-index 1.5188 vln-one-sin-ran
  beg 0.2643 f+ 2    4.0878 0.16 :fm-index 1.8766 vln-one-sin-ran
  beg 0.2643 f+ 4    4.0878 0.16 :fm-index 2.3083 vln-one-sin-ran
  beg 0.2643 f+ 4    4.0878 0.16 :fm-index 2.2215 vln-one-sin-ran
  
  beg 4 + dup to beg test-info

  \ 26, score 130
  beg 0.2600 f+ 1.6  66.5893 0.16 :fm-index 1.7041 vln-one-sin-ran
  beg 0.2600 f+ 2    66.4564 0.16 :fm-index 2.0296 vln-one-sin-ran
  beg 0.2603 f+ 1.2  66.7225 0.16 :fm-index 1.8321 vln-one-sin-ran
  beg 0.2603 f+ 4    66.3237 0.16 :fm-index 2.1550 vln-one-sin-ran
  beg 0.2605 f+ 1.2 125.4490 0.16 :fm-index 2.1806 vln-one-sin-ran
  beg 0.2605 f+ 2.8 125.6999 0.16 :fm-index 2.3570 vln-one-sin-ran
  beg 0.2608 f+ 1.2 125.1986 0.16 :fm-index 1.9861 vln-one-sin-ran
  beg 0.2608 f+ 1.6  24.4994 0.16 :fm-index 1.6412 vln-one-sin-ran
  beg 0.2610 f+ 2    24.4505 0.16 :fm-index 1.9770 vln-one-sin-ran
  beg 0.2610 f+ 1.2  24.5484 0.16 :fm-index 2.0103 vln-one-sin-ran
  beg 0.2613 f+ 4    24.4017 0.16 :fm-index 2.0663 vln-one-sin-ran
  beg 0.2615 f+ 1.2  24.4017 0.16 :fm-index 2.1521 vln-one-sin-ran
  beg 0.2615 f+ 2.8  24.4505 0.16 :fm-index 2.4453 vln-one-sin-ran
  beg 0.2620 f+ 1.2  24.3530 0.16 :fm-index 2.0930 vln-one-sin-ran
  beg 0.2623 f+ 2    24.6960 0.16 :fm-index 2.3423 vln-one-sin-ran
  beg 0.2623 f+ 4    24.7454 0.16 :fm-index 2.0856 vln-one-sin-ran
  beg 0.2623 f+ 4    24.7948 0.16 :fm-index 1.9570 vln-one-sin-ran
  beg 0.2625 f+ 2    24.4994 0.16 :fm-index 2.4642 vln-one-sin-ran
  beg 0.2625 f+ 1    24.4749 0.16 :fm-index 1.9901 vln-one-sin-ran
  beg 0.2630 f+ 2    24.5239 0.16 :fm-index 1.9972 vln-one-sin-ran
  beg 0.2633 f+ 2    24.4505 0.16 :fm-index 1.9148 vln-one-sin-ran
  beg 0.2635 f+ 1.2  24.4505 0.16 :fm-index 1.9017 vln-one-sin-ran
  beg 0.2635 f+ 2.8  24.4749 0.16 :fm-index 2.4958 vln-one-sin-ran
  beg 0.2640 f+ 0.8  24.4260 0.16 :fm-index 2.2518 vln-one-sin-ran
  beg 0.2643 f+ 2    24.5975 0.16 :fm-index 2.1120 vln-one-sin-ran
  beg 0.2643 f+ 4    24.6221 0.16 :fm-index 2.3154 vln-one-sin-ran
  beg 0.2643 f+ 4    24.6467 0.16 :fm-index 1.9240 vln-one-sin-ran
  
  beg 4 + dup to beg test-info

  \ 27, score 134
  6.718 to fmv-fm1-rat
  4.414 to fmv-fm2-rat
  5.141 to fmv-fm3-rat
  beg 0.2600 f+ 1.6 164.5868 0.16 :fm-index 1.9587 vln-one-sin-ran
  beg 0.2600 f+ 2   164.5868 0.16 :fm-index 1.5071 vln-one-sin-ran
  beg 0.2603 f+ 1.2 164.5868 0.16 :fm-index 1.7690 vln-one-sin-ran
  beg 0.2603 f+ 4   164.5868 0.16 :fm-index 1.7686 vln-one-sin-ran
  beg 0.2605 f+ 1.2 125.9513 0.16 :fm-index 1.5702 vln-one-sin-ran
  beg 0.2605 f+ 2.8 125.9513 0.16 :fm-index 2.1962 vln-one-sin-ran
  beg 0.2608 f+ 1.2 125.9513 0.16 :fm-index 1.7701 vln-one-sin-ran
  beg 0.2608 f+ 1.6  24.4994 0.16 :fm-index 2.1665 vln-one-sin-ran
  beg 0.2610 f+ 2    24.4994 0.16 :fm-index 1.9345 vln-one-sin-ran
  beg 0.2610 f+ 1.2  24.4994 0.16 :fm-index 2.2037 vln-one-sin-ran
  beg 0.2613 f+ 4    24.4994 0.16 :fm-index 1.6826 vln-one-sin-ran
  beg 0.2615 f+ 1.2  24.4994 0.16 :fm-index 1.5410 vln-one-sin-ran
  beg 0.2615 f+ 2.8  24.4994 0.16 :fm-index 1.8293 vln-one-sin-ran
  beg 0.2620 f+ 1.2  24.4994 0.16 :fm-index 2.1468 vln-one-sin-ran
  beg 0.2623 f+ 2    24.4994 0.16 :fm-index 2.0758 vln-one-sin-ran
  beg 0.2623 f+ 4    24.4994 0.16 :fm-index 2.4138 vln-one-sin-ran
  beg 0.2623 f+ 4    24.4994 0.16 :fm-index 1.8479 vln-one-sin-ran
  beg 0.2625 f+ 3    24.4994 0.16 :fm-index 2.4639 vln-one-sin-ran
  beg 0.2625 f+ 1.5  24.4994 0.16 :fm-index 2.3995 vln-one-sin-ran
  beg 0.2630 f+ 3    24.4994 0.16 :fm-index 1.8609 vln-one-sin-ran
  beg 0.2633 f+ 3    24.4994 0.16 :fm-index 2.4506 vln-one-sin-ran
  beg 0.2635 f+ 1.2  24.4994 0.16 :fm-index 2.1577 vln-one-sin-ran
  beg 0.2635 f+ 2.8  24.4994 0.16 :fm-index 1.6663 vln-one-sin-ran
  beg 0.2640 f+ 0.8  24.4994 0.16 :fm-index 2.1166 vln-one-sin-ran
  beg 0.2643 f+ 2    24.4994 0.16 :fm-index 1.9362 vln-one-sin-ran
  beg 0.2643 f+ 4    24.4994 0.16 :fm-index 2.2052 vln-one-sin-ran
  beg 0.2643 f+ 4    24.4994 0.16 :fm-index 2.0102 vln-one-sin-ran
  
  beg 4 + dup to beg test-info

  \ 28, score 138
  restore-fm-violin-defaults
  whoosh   to fmv-gliss-env
  metalamp to fmv-amp-env
  0.8   to fmv-glissando-amount
  0.01  to fmv-reverb-amount
  2.718 to fmv-fm1-rat
  1.141 to fmv-fm2-rat
  3.141 to fmv-fm3-rat
  beg 0.2600 f+ 0.4 1046.4800 0.16 :fm-index 2.3870 vln-one-sin
  beg 0.2600 f+ 0.5 1044.3912 0.16 :fm-index 2.4309 vln-one-sin
  beg 0.2603 f+ 0.3 1048.5730 0.16 :fm-index 2.15   vln-one-sin
  beg 0.2603 f+ 0.5 1042.3066 0.16 :fm-index 1.7211 vln-one-sin
  beg 0.2610 f+ 0.3  524.2865 0.16 :fm-index 2.1751 vln-one-sin
  beg 0.2620 f+ 0.3  520.1131 0.16 :fm-index 1.5433 vln-one-sin
  beg 0.2623 f+ 0.4  517.0048 0.16 :fm-index 2.4335 vln-one-sin
  beg 0.2625 f+ 0.4  523.2400 0.16 :fm-index 2.2778 vln-one-sin
  beg 0.2635 f+ 0.3  522.1951 0.16 :fm-index 1.9441 vln-one-sin
  beg 0.2643 f+ 0.4  520.6316 0.16 :fm-index 2.4656 vln-one-sin
  
  beg 4 + dup to beg test-info

  \ 29, score 142
  restore-fm-violin-defaults
  beg 0.1200 f+ 0.4 2092.9600 0.16 :fm-index 3 :reverb-amount 0
    :amp-env metalamp :fm2-rat 1.1410 
    :fm3-rat 3.1410 :fm1-rat 2.7180 vln-one-sin
  beg 0.1200 f+ 0.5 2088.7820 0.16 :fm-index 3 :reverb-amount 0
    :amp-env metalamp :fm2-rat 1.1410 :fm3-rat 3.1410 
    :fm1-rat 2.7180 vln-one-sin
  beg 0.1200 f+ 0.3 2097.1460 0.16 :fm-index 3 :reverb-amount 0
    :amp-env metalamp :fm2-rat 1.1410 :fm3-rat 3.1410 
    :fm1-rat 2.7180 vln-one-sin
  beg 0.1200 f+ 0.5 2084.6130 0.16 :fm-index 3 :reverb-amount 0
    :amp-env metalamp :fm2-rat 1.1410 :fm3-rat 3.1410 
    :fm1-rat 2.7180 vln-one-sin
  beg 0.1210 f+ 0.3 1048.5730 0.16 :fm-index 3 :reverb-amount 0
    :amp-env metalamp :fm2-rat 1.1410 :fm3-rat 3.1410 
    :fm1-rat 2.7180 vln-one-sin
  beg 0.1220 f+ 0.3 1040.2260 0.16 :fm-index 3 :reverb-amount 0
    :amp-env metalamp :fm2-rat 1.1410 :fm3-rat 3.1410 
    :fm1-rat 2.7180 vln-one-sin
  beg 0.1220 f+ 0.5 1034.0100 0.16 :fm-index 3 :reverb-amount 0
    :amp-env metalamp :fm2-rat 1.1410 :fm3-rat 3.1410 
    :fm1-rat 2.7180 vln-one-sin
  beg 0.1230 f+ 0.5 1046.4800 0.16 :fm-index 3 :reverb-amount 0
    :amp-env metalamp :fm2-rat 1.1410 :fm3-rat 3.1410 
    :fm1-rat 2.7180 vln-one-sin
  beg 0.1240 f+ 0.3 1044.3900 0.16 :fm-index 3 :reverb-amount 0
    :amp-env metalamp :fm2-rat 1.1410 :fm3-rat 3.1410 
    :fm1-rat 2.7180 vln-one-sin
  beg 0.1240 f+ 0.5 1041.2630 0.16 :fm-index 3 :reverb-amount 0
    :amp-env metalamp :fm2-rat 1.1410 :fm3-rat 3.1410 
    :fm1-rat 2.7180 vln-one-sin

  beg 2 + dup to beg test-info

  \ 30, score 144
  '( 0 0 20 0.5000 40 0.1 60 0.2 80 1 100 0 ) { z1amp }
  '( 0 0 20 1 60 0.1 75 0.3 100 0 )	      { z2amp }
  2.718 to fmv-fm1-rat
  4.414 to fmv-fm2-rat
  5.141 to fmv-fm3-rat
  beg 0.4880 f+ 1.1770  416.6072 0.0110 :fm-index 1.1140 :reverb-amount 0.1
    :amp-env z1amp :noise-amount 0.0050 vln-one-sin-ran
  beg 0.5050 f+ 2.4900  859.5863 0.0083 :fm-index 0.5890 :reverb-amount 0.1
    :amp-env z2amp :noise-amount 0.0050 vln-one-sin-ran
  beg 1 + to beg
  beg 0.0590 f+ 1.0550 1758.0816 0.0053 :fm-index 1.8640 :reverb-amount 0.1
    :amp-env z2amp :noise-amount 0.0050 vln-one-sin-ran
  beg 0.0930 f+ 1.8580  229.0566 0.0110 :fm-index 1.9690 :reverb-amount 0.1
    :amp-env z2amp :noise-amount 0.0050 vln-one-sin-ran
  beg 0.3490 f+ 3.3680  479.1994 0.0083 :fm-index 1.9970 :reverb-amount 0.1
    :amp-env z1amp :noise-amount 0.0050 vln-one-sin-ran
  beg 0.5010 f+ 3.0680  411.8241 0.0110 :fm-index 1.5390 :reverb-amount 0.1
    :amp-env z2amp :noise-amount 0.0050 vln-one-sin-ran
  beg 0.5200 f+ 2.8290  984.8456 0.0053 :fm-index 0.0560 :reverb-amount 0.1
    :amp-env z1amp :noise-amount 0.0050 vln-one-sin-ran
  beg 0.6100 f+ 0.7040 1767.7444 0.0053 :fm-index 1.2620 :reverb-amount 0.1
    :amp-env z2amp :noise-amount 0.0050 vln-one-sin-ran
  beg 0.8480 f+ 3.0510  859.7203 0.0083 :fm-index 1.6080 :reverb-amount 0.1
    :amp-env z2amp :noise-amount 0.0050 vln-one-sin-ran
  beg 1 + to beg
  beg 0.4880 f+ 3.2350  231.9431 0.0110 :fm-index 0.9690 :reverb-amount 0.1
    :amp-env z1amp :noise-amount 0.0050 vln-one-sin-ran
  beg 0.5610 f+ 3.2810  475.2009 0.0083 :fm-index 0.3740 :reverb-amount 0.1
    :amp-env z2amp :noise-amount 0.0050 vln-one-sin-ran
  beg 0.7970 f+ 2.8400  988.8375 0.0053 :fm-index 0.4200 :reverb-amount 0.1
    :amp-env z2amp :noise-amount 0.0050 vln-one-sin-ran
  beg 1 + to beg
  beg 0.0620 f+ 1.0210  411.7247 0.0110 :fm-index 0.1370 :reverb-amount 0.1
    :amp-env z2amp :noise-amount 0.0050 vln-one-sin-ran
  beg 0.2130 f+ 1.1610  848.5959 0.0083 :fm-index 1.3120 :reverb-amount 0.1
    :amp-env z2amp :noise-amount 0.0050 vln-one-sin-ran
  beg 0.4410 f+ 2.6160  390.0600 0.0110 :fm-index 1.9030 :reverb-amount 0.1
    :amp-env z1amp :noise-amount 0.0050 vln-one-sin-ran
  beg 0.4490 f+ 0.7000  802.3538 0.0083 :fm-index 1.5940 :reverb-amount 0.1
    :amp-env z2amp :noise-amount 0.0050 vln-one-sin-ran
  beg 0.5270 f+ 2.5080 1773.9366 0.0053 :fm-index 1.8030 :reverb-amount 0.1
    :amp-env z1amp :noise-amount 0.0050 vln-one-sin-ran
  beg 0.7820 f+ 2.7990  232.4344 0.0110 :fm-index 0.0590 :reverb-amount 0.1
    :amp-env z2amp :noise-amount 0.0050 vln-one-sin-ran
  beg 0.7830 f+ 2.7660 1650.1434 0.0053 :fm-index 0.4400 :reverb-amount 0.1
    :amp-env z2amp :noise-amount 0.0050 vln-one-sin-ran
  beg 0.7890 f+ 3.1560  475.7231 0.0083 :fm-index 0.7370 :reverb-amount 0.1
    :amp-env z2amp :noise-amount 0.0050 vln-one-sin-ran
  beg 1 + to beg
  beg 0.1540 f+ 2.1290  976.0237 0.0053 :fm-index 1.2690 :reverb-amount 0.1
    :amp-env z2amp :noise-amount 0.0050 vln-one-sin-ran
  beg 0.4890 f+ 3.3650  390.0525 0.0110 :fm-index 1.4580 :reverb-amount 0.1
    :amp-env z2amp :noise-amount 0.0050 vln-one-sin-ran
  beg 0.7450 f+ 1.5070 1665.9722 0.0053 :fm-index 1.9330 :reverb-amount 0.1
    :amp-env z2amp :noise-amount 0.0050 vln-one-sin-ran
  beg 0.8320 f+ 1.4430  798.1238 0.0083 :fm-index 0.8560 :reverb-amount 0.1
    :amp-env z1amp :noise-amount 0.0050 vln-one-sin-ran
  beg 0.9440 f+ 3.1560  229.0528 0.0110 :fm-index 1.8300 :reverb-amount 0.1
    :amp-env z2amp :noise-amount 0.0050 vln-one-sin-ran
  beg 1 + to beg
  beg 0.3930 f+ 1.1100  473.7225 0.0083 :fm-index 1.6260 :reverb-amount 0.1
    :amp-env z1amp :noise-amount 0.0050 vln-one-sin-ran
  beg 0.6970 f+ 1.6170  988.7953 0.0053 :fm-index 0.4230 :reverb-amount 0.1
    :amp-env z2amp :noise-amount 0.0050 vln-one-sin-ran
  beg 1 + to beg
  beg 0.0620 f+ 1.3190  390.9769 0.0110 :fm-index 0.4100 :reverb-amount 0.1
    :amp-env z2amp :noise-amount 0.0050 vln-one-sin-ran
  beg 0.0840 f+ 3.3660  804.6413 0.0083 :fm-index 1.8760 :reverb-amount 0.1
    :amp-env z2amp :noise-amount 0.0050 vln-one-sin-ran
  beg 0.1740 f+ 2.7210  418.6819 0.0110 :fm-index 0.0910 :reverb-amount 0.1
    :amp-env z2amp :noise-amount 0.0050 vln-one-sin-ran
  beg 0.5700 f+ 3.4460  845.4019 0.0077 :fm-index 0.7660 :reverb-amount 0.1
    :amp-env z2amp :noise-amount 0.0050 vln-one-sin-ran
  beg 0.6440 f+ 1.1790 1656.5756 0.0049 :fm-index 0.2960 :reverb-amount 0.1
    :amp-env z1amp :noise-amount 0.0050 vln-one-sin-ran
  beg 0.6600 f+ 2.8520 1758.9788 0.0049 :fm-index 0.4520 :reverb-amount 0.1
    :amp-env z2amp :noise-amount 0.0050 vln-one-sin-ran
  beg 0.8270 f+ 1.8840  387.0009 0.0099 :fm-index 1.3010 :reverb-amount 0.1
    :amp-env z1amp :noise-amount 0.0050 vln-one-sin-ran
  beg 0.8870 f+ 3.4040  796.7213 0.0077 :fm-index 1.1820 :reverb-amount 0.1
    :amp-env z2amp :noise-amount 0.0050 vln-one-sin-ran
  beg 0.9640 f+ 3.3230  416.3916 0.0099 :fm-index 0.6290 :reverb-amount 0.1
    :amp-env z2amp :noise-amount 0.0050 vln-one-sin-ran
  beg 1 + to beg
  beg 0.1320 f+ 1.7050 1637.2303 0.0049 :fm-index 1.0570 :reverb-amount 0.1
    :amp-env z1amp :noise-amount 0.0050 vln-one-sin-ran
  beg 0.15   f+ 3.1250 1762.4906 0.0049 :fm-index 1.3170 :reverb-amount 0.1
    :amp-env z2amp :noise-amount 0.0050 vln-one-sin-ran
  beg 0.3860 f+ 2.9670  852.0487 0.0077 :fm-index 1.4790 :reverb-amount 0.1
    :amp-env z1amp :noise-amount 0.0050 vln-one-sin-ran
  beg 0.6670 f+ 0.6780  413.7094 0.0099 :fm-index 0.9470 :reverb-amount 0.1
    :amp-env z2amp :noise-amount 0.0050 vln-one-sin-ran
  beg 0.8780 f+ 2.7490 1749.7509 0.0049 :fm-index 0.5040 :reverb-amount 0.1
    :amp-env z1amp :noise-amount 0.0050 vln-one-sin-ran
  beg 0.9730 f+ 0.5990  848.1253 0.0077 :fm-index 1.9380 :reverb-amount 0.1
    :amp-env z1amp :noise-amount 0.0050 vln-one-sin-ran
  beg 1 + to beg
  beg 0.0880 f+ 3.3360  229.9144 0.0099 :fm-index 1.3930 :reverb-amount 0.1
    :amp-env z2amp :noise-amount 0.0050 vln-one-sin-ran
  beg 0.1170 f+ 1.1300  984.0816 0.0049 :fm-index 0.3560 :reverb-amount 0.1
    :amp-env z2amp :noise-amount 0.0050 vln-one-sin-ran
  beg 0.4640 f+ 1.7330  478.7184 0.0077 :fm-index 0.2840 :reverb-amount 0.1
    :amp-env z2amp :noise-amount 0.0050 vln-one-sin-ran
  beg 0.5760 f+ 0.5680  413.4253 0.0099 :fm-index 1.5020 :reverb-amount 0.1
    :amp-env z2amp :noise-amount 0.0050 vln-one-sin-ran
  beg 0.8200 f+ 1.2150  230.9588 0.0099 :fm-index 1.0990 :reverb-amount 0.1
    :amp-env z1amp :noise-amount 0.0050 vln-one-sin-ran
  beg 0.8320 f+ 3.4590  473.8903 0.0077 :fm-index 0.7680 :reverb-amount 0.1
    :amp-env z2amp :noise-amount 0.0050 vln-one-sin-ran
  beg 0.8320 f+ 0.7260  857.2875 0.0077 :fm-index 0.7520 :reverb-amount 0.1
    :amp-env z2amp :noise-amount 0.0050 vln-one-sin-ran

  beg 4 + dup to beg test-info

  \ 31, score 156
  '( 0 1 20 0 100 0 ) { indfunc }
  '( 0 1 90 1 100 0 ) { indfunc2 }
  '( 0 1 6 1 10 0.5 20 0.3630 30 0.27
     40 0.2 50 0.12 60 0.08 70 0.04 100 0 ) { ampfunc }
  '( 0 0 1 1 3 1 10 0.5 30 0.2 60 0.05 100 0 ) { ampfunc1 }
  restore-fm-violin-defaults
  beg 0.2600 f+ 0.0500 80 0.8 :fm-index 5 :reverb-amount 0
    :amp-env ampfunc1 :fm1-env indfunc2 vln-one-sin
  beg 1 + to beg
  beg 0.2610 f+ 0.2 80 0.8 :fm-index 4 :reverb-amount 0
    :amp-env ampfunc :fm1-env indfunc :fm2-rat 0.6875 vln-one-sin
  beg 1 + to beg
  beg 0.2600 f+ 0.05 80 0.8 :fm-index 5 :reverb-amount 0
    :amp-env ampfunc1 :fm1-env indfunc2 vln-one-sin
  beg 0.2620 f+ 0.2  80 0.8 :fm-index 5 :reverb-amount 0
    :amp-env ampfunc :fm1-env indfunc :fm2-rat 0.6875 vln-one-sin
  beg 1 + to beg
  beg 0.2600 f+ 0.0500 80 0.8 :fm-index 6 :reverb-amount 0
    :amp-env ampfunc1 :fm1-env indfunc2  vln-one-sin
  beg 0.2630 f+ 0.2 80 0.8 :fm-index 6 :reverb-amount 0
    :amp-env ampfunc :fm1-env indfunc :fm2-rat 0.6875 vln-one-sin
  beg 1 + to beg
  beg 0.2600 f+ 0.0500 80 0.3 :fm-index 4 :reverb-amount 0
    :amp-env ampfunc1 :fm1-env indfunc2 vln-one-sin 
  beg 0.2620 f+ 0.1 160 0.3 :fm-index 4 :reverb-amount 0
    :amp-env ampfunc :fm1-env indfunc :fm2-rat 0.6875 vln-one-sin
  beg 0.2620 f+ 0.2500 80 0.8 :fm-index 4 :reverb-amount 0
    :amp-env ampfunc :fm1-env indfunc :fm2-rat 0.6875 vln-one-sin
  beg 1 + to beg
  beg 0.2600 f+ 0.0500 80 0.5000 :fm-index 4 :reverb-amount 0
    :amp-env ampfunc1 :fm1-env indfunc2 vln-one-sin
  beg 0.2610 f+ 0.1 210 0.3 :fm-index 4 :reverb-amount 0
    :amp-env ampfunc :fm1-env indfunc :fm2-rat 0.6875 vln-one-sin
  beg 0.2620 f+ 0.2 80 0.1 :fm-index 4 :reverb-amount 0
    :amp-env ampfunc :fm1-env indfunc :fm2-rat 0.6875 vln-one-sin
  beg 0.2630 f+ 0.2500 320 0.1 :fm-index 2 :reverb-amount 0
    :amp-env ampfunc :fm1-env indfunc :fm2-rat 0.6875 vln-one-sin
  beg 1 + to beg
  beg 0.2600 f+ 0.0500 80 0.8 :fm-index 4 :reverb-amount 0
    :amp-env ampfunc1 :fm1-env indfunc2 vln-one-sin 
  beg 0.2610 f+ 0.1 210 0.1 :fm-index 2 :reverb-amount 0
    :amp-env ampfunc :fm1-env indfunc :fm2-rat 0.6875 vln-one-sin
  beg 0.2620 f+ 0.2 80 0.2 :fm-index 4 :reverb-amount 0
    :amp-env ampfunc :fm1-env indfunc :fm2-rat 0.6875 vln-one-sin
  beg 0.2630 f+ 0.2500 320 0.3 :reverb-amount 0
    :amp-env ampfunc :fm1-env indfunc :fm2-rat 0.6875  vln-one-sin
  beg 1 + to beg
  beg 0.2600 f+ 0.0500 80 0.8 :fm-index 2 :reverb-amount 0
    :amp-env ampfunc1 :fm1-env indfunc2 vln-one-sin
  beg 0.2610 f+ 0.1 210 0.1 :fm-index 2 :reverb-amount 0
    :amp-env ampfunc :fm1-env indfunc :fm2-rat 0.6875 vln-one-sin
  beg 0.2620 f+ 0.2 80 0.2 :fm-index 2 :reverb-amount 0
    :amp-env ampfunc :fm1-env indfunc :fm2-rat 0.6875 vln-one-sin
  beg 0.2630 f+ 0.2500 320 0.3 :reverb-amount 0
    :amp-env ampfunc :fm1-env indfunc :fm2-rat 0.6875 vln-one-sin

  beg 1 + dup to beg test-info

  \ 32, score 164
  0     to fmv-glissando-amount
  0.004 to fmv-noise-amount
  3.141 to fmv-fm1-rat
  4.414 to fmv-fm2-rat
  2.718 to fmv-fm3-rat
  beg 0.2600 f+ 4 3286.9937 0.16 :fm-index 2.2165 :reverb-amount 0.01
    :amp-env n-amp :fm1-env n-amp vln-one-sin-ran
  beg 0.2603 f+ 4 1046.4800 0.16 :fm-index 2.3234 :reverb-amount 0.01
    :amp-env n-amp :fm1-env n-amp vln-one-sin-ran
  beg 0.2605 f+ 4 2844.3326 0.16 :fm-index 2.4790 :reverb-amount 0.1
    :amp-env n-amp :fm1-env n-amp vln-one-sin-ran
  beg 0.2608 f+ 4  821.7484 0.1  :fm-index 1.8667 :reverb-amount 0.01
    :amp-env n-amp :fm1-env n-amp vln-one-sin-ran
  beg 0.2610 f+ 4  261.6200 0.1  :fm-index 1.8523 :reverb-amount 0.01
    :amp-env n-amp :fm1-env n-amp vln-one-sin-ran
  beg 0.2613 f+ 4  711.0832 0.1  :fm-index 2.2300 :reverb-amount 0.1
    :amp-env n-amp :fm1-env n-amp vln-one-sin-ran
  beg 0.2615 f+ 4  205.4371 0.06 :fm-index 1.5187 :reverb-amount 0.01
    :amp-env n-amp :fm1-env n-amp vln-one-sin-ran
  beg 0.2618 f+ 4   65.4050 0.06 :fm-index 2.4074 :reverb-amount 0.01
    :amp-env n-amp :fm1-env n-amp vln-one-sin-ran
  beg 0.2620 f+ 4  177.7708 0.06 :fm-index 2.4481 :reverb-amount 0.1
    :amp-env n-amp :fm1-env n-amp vln-one-sin-ran
  beg 0.2623 f+ 4   51.3593 0.01 :fm-index 2.3069 :reverb-amount 0.01
    :amp-env n-amp :fm1-env n-amp vln-one-sin-ran
  beg 0.2625 f+ 4   16.3513 0.01 :fm-index 2.1008 :reverb-amount 0.01
    :amp-env n-amp :fm1-env n-amp vln-one-sin-ran
  beg 0.2628 f+ 4   44.4427 0.01 :fm-index 2.4860 :reverb-amount 0.1
    :amp-env n-amp :fm1-env n-amp vln-one-sin-ran

  beg 8 + dup to beg test-info

  \ 33, score 172
  restore-fm-violin-defaults
  beg 0.2603 f+ 1.2  88.8854 0.1  :fm-index 2.3144 :reverb-amount 0.2
    :amp-env mamp :fm2-rat 4.4140 :fm3-rat 5.1410 :fm1-rat 2.7180 vln-one-sin
  beg 0.2603 f+ 4    88.8854 0.1  :fm-index 2.1690 :reverb-amount 0.2
    :amp-env mamp :fm2-rat 4.4140 :fm3-rat 5.1410 :fm1-rat 2.7180 vln-one-sin
  beg 0.2605 f+ 2.8 168.1236 0.05 :fm-index 2.1850 :reverb-amount 0.2
    :amp-env mamp :fm2-rat 4.4140 :fm3-rat 5.1410 :fm1-rat 2.7180 vln-one-sin
  beg 0.2608 f+ 1.2 168.1236 0.08 :fm-index 1.7743 :reverb-amount 0.2
    :amp-env mamp :fm2-rat 4.4140 :fm3-rat 5.1410 :fm1-rat 2.7180 vln-one-sin
  beg 0.2610 f+ 2    32.7025 0.1  :fm-index 2.4925 :reverb-amount 0.2
    :amp-env mamp :fm2-rat 4.4140 :fm3-rat 5.1410 :fm1-rat 2.7180 vln-one-sin
  beg 0.2633 f+ 2    32.7025 0.1  :fm-index 2.1325 :reverb-amount 0.2
    :amp-env mamp :fm2-rat 4.4140 :fm3-rat 5.1410 :fm1-rat 2.7180 vln-one-sin
  beg 0.2643 f+ 4    32.7025 0.05 :fm-index 1.7578 :reverb-amount 0.2
    :amp-env mamp :fm2-rat 4.4140 :fm3-rat 5.1410 :fm1-rat 2.7180 vln-one-sin

  beg 8 + dup to beg test-info

  \ 34, score 180
  beg 0.2600 f+ 6.6830  244.8160 0.0060 :fm-index 2   :reverb-amount 0.2
    :noise-amount 0.0040 vln-one-sin-ran
  beg 0.2600 f+ 5.5170  495.4040 0.0060 :fm-index 2   :reverb-amount 0.2
    :noise-amount 0.0040 vln-one-sin-ran
  beg 0.2600 f+ 7.5350  980.6190 0.0020 :fm-index 2   :reverb-amount 0.2
    :noise-amount 0.0040 vln-one-sin-ran
  beg 0.2600 f+ 7.1990 1965.4290 0.0020 :fm-index 0.8 :reverb-amount 0.2
    :noise-amount 0.0040 vln-one-sin-ran
  beg 0.2600 f+ 4.0790 3835.3170 0.0020 :fm-index 0.8 :reverb-amount 0.2
    :noise-amount 0.0040 vln-one-sin-ran
  beg 0.5170 f+ 4.7400 1320.9670 0.0020 :fm-index 0.8 :reverb-amount 0.2
    :noise-amount 0.0040 vln-one-sin-ran
  beg 0.7040 f+ 7.2080  655.5670 0.0040 :fm-index 2   :reverb-amount 0.2
    :noise-amount 0.0040 vln-one-sin-ran

  beg 9 + dup to beg test-info

  \ 35, score 189
  '( 0 0 15 1 100 0 ) { updown }
  0   to fmv-glissando-amount
  0.9 to fmv-reverb-amount
  beg 0.5450 f+ 6.4650  366.3330 0.0320 :fm-index 1.0480
    :amp-env '( 0 0 1.5468 1 2.0882 0.7 2.3202 1 98.4532 0.7500 100 0 )
    vln-one-sin-exp
  beg 0.5950 f+ 8.4340 1172.5830 0.0180 :fm-index 1.1350
    :amp-env '( 0 0 1.1857 1.0 1.6007 0.7 1.7785 1 98.8143 0.5556 100 0 )
    vln-one-sin-exp
  beg 0.7650 f+ 1.6210  369.9940 0.0170 :fm-index 0.0960
    :amp-env '( 0 0 6.1690 1.0 8.3282 0.7 9.2535 1 93.8310 0.5294 100 0 )
    vln-one-sin-exp
  beg 0.8820 f+ 3.0640  246.9420 0.0170 :fm-index 0.0020
    :amp-env '( 0 0 3.2637 1 4.4060 0.7 4.8956 1.0 96.7363 0.5294 100 0 )
    vln-one-sin-exp
  beg 0.9250 f+ 3.1170  123.4710 0.0380 :fm-index 0.2330
    :amp-env '( 0 0 3.2082 1 4.3311 0.7 4.8123 1 96.7918 0.7895 100 0 )
    vln-one-sin-exp
  beg 0.9810 f+ 3.5670  123.4710 0.0420 :fm-index 0.2330
    :amp-env '( 0 0 2.8035 1 3.7847 0.7 4.2052 1.0 97.1965 0.8095 100 0 )
    vln-one-sin-exp
  beg 1 + to beg
  beg 0.1280 f+ 1.0450  246.9420 0.0170 :fm-index 1.2050
    :amp-env '( 0 0 9.5694 1 12.9187 0.7 14.3541 1 90.4306 0.5294 100 0 )
    vln-one-sin-exp
  beg 0.2550 f+ 3.3870  374.1370 0.0170 :fm-index 0.1800
    :amp-env '( 0 0 2.9525 1.0 3.9858 0.7 4.4287 1 97.0475 0.5294 100 0 )
    vln-one-sin-exp
  beg 0.2990 f+ 8.3050 1576.9120 0.0200 :fm-index 0.2990
    :amp-env '( 0 0 1.2041 1 1.6255 0.7 1.8061 1 98.7959 0.6 100 0 )
    vln-one-sin-exp
  beg 0.3300 f+ 4.4630  246.9420 0.0170 :fm-index 0.0020
    :amp-env '( 0 0 2.2406 1 3.0249 0.7 3.3610 1.0 97.7594 0.5294 100 0 )
    vln-one-sin-exp
  beg 0.6600 f+ 8.9940 1576.9120 0.0200 :fm-index 0.2990
    :amp-env '( 0 0 1.1119 1 1.5010 0.7 1.6678 1 98.8881 0.6 100 0 )
    vln-one-sin-exp
  beg 0.9060 f+ 8.8360 1172.5830 0.0180 :fm-index 1.1350
    :amp-env '( 0 0 1.1317 1 1.5278 0.7 1.6976 1 98.8683 0.5556 100 0 )
    vln-one-sin-exp
  beg 1 + to beg
  beg 0.1510 f+ 4.9320 374.1370 0.0170 :fm-index 0.1800
    :amp-env '( 0 0 2.0276 1 2.7372 0.7 3.0414 1 97.9724 0.5294 100 0 )
    vln-one-sin-exp
  beg 0.2720 f+ 2.3250 369.9940 0.0170 :fm-index 1.1030
    :amp-env '( 0 0 4.3011 1 5.8065 0.7 6.4516 1 95.6989 0.5294 100 0 )
    vln-one-sin-exp
  beg 1 + to beg
  beg 0.6960 f+ 3.5540 366.3330 0.0310 :fm-index 1.0480
    :amp-env '( 0 0 2.8137 1 3.7985 0.7 4.2206 1 97.1863 0.7419 100 0 )
    vln-one-sin-exp
  beg 1 + to beg
  beg 0.7240 f+ 0.6040 246.9420 0.0170 :fm-index 1.2050
    :amp-env '( 0 0 16.5563 1 22.351 0.7 24.8344 1 83.4437 0.5294 100 0 )
    vln-one-sin-exp
  beg 0.9420 f+ 2.5010 123.4710 0.0330 :fm-index 0.2330
    :amp-env '( 0 0 3.9984 1 5.3978 0.7 5.9976 1 96.0016 0.7576 100 0 )
    vln-one-sin-exp
  beg 1 + to beg
  beg 0.0340 f+ 2.3860 246.9420 0.0170 :fm-index 0.0020
    :amp-env '( 0 0 4.1911 1 5.6580 0.7 6.2867 1 95.8089 0.5294 100 0 )
    vln-one-sin-exp
  beg 0.3850 f+ 1.4510 369.9940 0.0170 :fm-index 1.1030
    :amp-env '( 0 0 6.8918 1 9.3039 0.7 10.3377 1 93.1082 0.5294 100 0 )
    vln-one-sin-exp
  beg 0.5670 f+ 2.6550 374.1370 0.0170 :fm-index 0.1800
    :amp-env '( 0 0 3.7665 1 5.0847 0.7 5.6497 1 96.2335 0.5294 100 0 )
    vln-one-sin-exp
  beg 0.9830 f+ 2.9860 123.4710 0.0380 :fm-index 0.2330
    :amp-env '( 0 0 3.3490 1 4.5211 0.7 5.0234 1 96.6510 0.7895 100 0 )
    vln-one-sin-exp
  beg 1 + to beg
  beg 0.4910 f+ 0.6110 123.9770 0.0170 :fm-index 0.7550
    :amp-env '( 0 0 16.3666 1 22.0949 0.7 24.55 1 83.6334 0.5294 100 0 )
    vln-one-sin-exp
  beg 0.7570 f+ 1.4440 123.4710 0.0170 :fm-index 0.0020
    :amp-env '( 0 0 6.9252 1 9.3490 0.7 10.3878 1 93.0748 0.5294 100 0 )
    vln-one-sin-exp
  beg 0.7750 f+ 0.5370  92.4435 0.0330 :fm-index 0.9200
    :amp-env '( 0 0 18.622 1 25.1397 0.7 27.9330 1 81.3780 0.7576 100 0 )
    vln-one-sin-exp
  beg 0.7750 f+ 10.537  92.4435 0.0130 :fm-index 0.9200
    :amp-env '( 0 0 0.9490 1 1.2812 0.7 1.4236 1 99.0510 0.3846 100 0 )
    vln-one-sin-exp
  beg 0.9380 f+ 0.6520 122.2995 0.0170 :fm-index 1.8380
    :amp-env '( 0 0 15.3374 1 20.706 0.7 23.0061 1 84.6626 0.5294 100 0 )
    vln-one-sin-exp
  beg 1 + to beg 
  beg 0.2350 f+ 3.7250 586.2915 0.0180 :fm-index 1.1350
    :amp-env '( 0 0 2.6846 1 3.6242 0.7 4.0268 1 97.3154 0.5556 100 0 )
    vln-one-sin-exp
  beg 0.2560 f+ 2.8900 183.1665 0.0260 :fm-index 1.0480
    :amp-env '( 0 0 3.4602 1 4.6713 0.7 5.1903 1 96.5398 0.6923 100 0 )
    vln-one-sin-exp
  beg 0.2710 f+ 1.6210 187.0685 0.0170 :fm-index 0.1800
    :amp-env '( 0 0 6.169 1.0 8.3282 0.7 9.2535 1 93.8310 0.5294 100 0 )
    vln-one-sin-exp
  beg 0.2920 f+ 2.0160 183.1665 0.0290 :fm-index 1.0480
    :amp-env '( 0 0 4.9603 1 6.6964 0.7 7.4405 1 95.0397 0.7241 100 0 )
    vln-one-sin-exp
  beg 0.2920 f+ 12.016 183.1665 0.0290 :fm-index 1.0480
    :amp-env '( 0 0 0.832 1 1.1235 0.7 1.248 1.0 99.1678 0.7241 100 0 )
    vln-one-sin-exp
  beg 0.3300 f+ 0.7300 184.9970 0.0170 :fm-index 0.0960
    :amp-env '( 0 0 13.699 1 18.4932 0.7 20.548 1.0 86.3014 0.529 100 0 )
    vln-one-sin-exp
  beg 0.3570 f+ 1.9600 183.1665 0.0280 :fm-index 1.0480
    :amp-env '( 0 0 5.1020 1.0 6.8878 0.7 7.6531 1 94.8980 0.7143 100 0 )
    vln-one-sin-exp
  beg 0.3820 f+ 2.2450  61.7355 0.0330 :fm-index 0.2330
    :amp-env '( 0 0 4.4543 1 6.0134 0.7 6.6815 1 95.5457 0.7576 100 0 )
    vln-one-sin-exp
  beg 0.3820 f+ 12.2450 61.7355 0.0330 :fm-index 0.2330
    :amp-env '( 0 0 0.8167 1 1.1025 0.7 1.2250 1 99.1833 0.7576 100 0 )
    vln-one-sin-exp
  beg 0.5410 f+ 3.0130 246.5050 0.0360 :fm-index 1.1350
    :amp-env '( 0 0 3.3190 1.0 4.4806 0.7 4.9784 1 96.6810 0.7778 100 0 )
    vln-one-sin-exp
  beg 0.5570 f+ 2.322 1251.5960 0.0400 :fm-index 0.2990
    :amp-env '( 0 0 4.3066 1 5.8140 0.7 6.4599 1 95.6934 0.8 100 0 )
    vln-one-sin-exp
  beg 0.5570 f+ 18.322 1251.5960 0.020 :fm-index 0.2990
    :amp-env '( 0 0 0.5458 1.000 0.7368 0.7 0.8187 1 99.4542 0.6 100 0 )
    vln-one-sin-exp
  beg 1 + to beg
  beg 0.1060 f+ 1.9900 183.1665 0.0230 :fm-index 1.0480
    :amp-env '( 0 0 5.0251 1.0 6.7839 0.7 7.5377 1 94.9749 0.6522 100 0 )
    vln-one-sin-exp
  beg 0.2570 f+ 1.9180  61.7355 0.0330 :fm-index 0.2330
    :amp-env '( 0 0 5.2138 1 7.0386 0.7 7.8206 1 94.7862 0.7576 100 0 )
    vln-one-sin-exp
  beg 0.6370 f+ 1.3090 183.1665 0.0310 :fm-index 1.0480
    :amp-env '( 0 0 7.6394 1 10.3132 0.7 11.4591 1 92.3606 0.7419 100 0 )
    vln-one-sin-exp
  beg 1 + to beg 
  beg 0.0330 f+ 1.1590 183.1665 0.0250 :fm-index 1.0480
    :amp-env '( 0 0 8.6281 1 11.6480 0.7 12.9422 1 91.3719 0.6800 100 0 )
    vln-one-sin-exp
  beg 0.0980 f+ 1.2400  30.8675 0.0330 :fm-index 0.2330
    :amp-env '( 0 0 8.0645 1 10.8871 0.7 12.0968 1 91.9355 0.7576 100 0 )
    vln-one-sin-exp
  beg 0.0980 f+ 11.2400 30.8675 0.0130 :fm-index 0.2330
    :amp-env '( 0 0 0.8897 1 1.2011 0.7 1.3345 1 99.1103 0.3846 100 0 )
    vln-one-sin-exp
  beg 0.1260 f+ 0.2600 123.4710 0.0170 :fm-index 1.2050
    :amp-env '( 0 0 38.462 1 51.9231 0.7 57.6923 1 61.5385 0.5294 100 0 )
    vln-one-sin-exp
  beg 0.1260 f+ 10.26  123.4710 0.0170 :fm-index 1.2050
    :amp-env '( 0 0 0.9747 1 1.3158 0.7 1.4620 1 99.0253 0.5294 100 0 )
    vln-one-sin-exp
  beg 0.0600 f+ 13.877 3951.1200 0.009               :amp-env updown vln-one-sin
  beg 0.2600 f+ 14.877  123.4725 0.017 :fm-index 1.5 :amp-env updown vln-one-sin
  beg 0.2600 f+ 13.877   61.7363 0.017 :fm-index 1.5 :amp-env updown vln-one-sin
  beg 0.2600 f+ 12.877   30.8681 0.017 :fm-index 1.5 :amp-env updown vln-one-sin
  beg 0.2600 f+ 11.877   15.4341 0.017 :fm-index 1.5 :amp-env updown vln-one-sin

  beg 19 + dup to beg test-info

  \ 36, score 217
  restore-fm-violin-defaults
  beg 0.2620 f+ 0.3906 440 0.4500 :fm-index 1.2 :reverb-amount 0.0013
    :amp-env '( 0 0 0.7680 1 4.7774 0.6 9.7891 0.3 24.8243 0.1 100 0 )
    cel-one-sum
  beg 0.2640 f+ 0.5220 220 0.4500 :fm-index 1.2 :reverb-amount 0.0012
    :amp-env '( 0 0 0.5747 1.0 4.5919 0.6 9.6134 0.3 24.6778 0.1 100 0 )
    cel-one-sum
  beg 0.2660 f+ 1.5660 880 0.4500 :fm-index 1.2 :reverb-amount 0.0014
    :amp-env '( 0 0 0.1916 1.0 4.2242 0.6 9.2651 0.3 24.3876 0.1 100 0 )
    cel-one-sum
  beg 0.2680 f+ 1.5660 110 0.4500 :fm-index 1.2 :reverb-amount 0.0013
    :amp-env '( 0 0 0.1916 1.0 4.2242 0.6 9.2651 0.3 24.3876 0.1 100 0 ) 
    cel-one-sum

  beg 3 + dup to beg test-info

  \ 37, score 220
  beg 0.8600 f+ 0.9    733.3330 0.1875 :fm-index 0.2 :distance 1.0 
    :reverb-amount 0.0012
    :amp-env '( 0 0 0.3333 1 4.3603 0.6 9.3939 0.3 24.4950 0.1 100 0 )
    vln-one-sin
  beg 0.8600 f+ 0.225  550      0.1875 :fm-index 0.2 :distance 1.0 
    :reverb-amount 0.0015
    :amp-env '( 0 0 1.3333 1 5.3199 0.6 10.3030 0.3 25.2525 0.1 100 0 )
    vln-one-sin
  beg 0.8600 f+ 0.45   586.6670 0.3750 :fm-index 0.2 :distance 1.0 
    :reverb-amount 0.0013
    :amp-env '( 0 0 0.6667 1 4.6801 0.6 9.6970 0.3 24.7475 0.1 100 0 )  
    vln-one-sin
  beg 0.9020 f+ 0.9    733.3330 0.1875 :fm-index 0.4 :distance 1.0 
    :reverb-amount 0.0013
    :amp-env '( 0 0 0.3333 1 4.3603 0.6 9.3939 0.3 24.4950 0.1 100 0 )  
    vln-one-sin
  beg 0.9020 f+ 0.225  550      0.1875 :fm-index 0.4 :distance 1.0 
    :reverb-amount 0.0010
    :amp-env '( 0 0 1.3333 1 5.3199 0.6 10.3030 0.3 25.2525 0.1 100 0 ) 
    vln-one-sin
  beg 0.9020 f+ 0.45   586.6670 0.3750 :fm-index 0.4 :distance 1.0 
    :reverb-amount 0.0015
    :amp-env '( 0 0 0.6667 1 4.6801 0.6 9.6970 0.3 24.7475 0.1 100 0 )  
    vln-one-sin
  beg 0.9430 f+ 0.9    366.6670 0.1875 :fm-index 0.6 :distance 1.0 
    :reverb-amount 0.0016
    :amp-env '( 0 0 0.3333 1 4.3603 0.6 9.3939 0.3 24.4950 0.1 100 0 )  
    vln-one-sin
  beg 0.9430 f+ 0.225  275      0.1875 :fm-index 0.6 :distance 1.0 
    :reverb-amount 0.0015
    :amp-env '( 0 0 1.3333 1 5.3199 0.6 10.3030 0.3 25.2525 0.1 100 0 ) 
    vln-one-sin
  beg 0.9430 f+ 0.45   293.3340 0.3750 :fm-index 0.6 :distance 1.0 
    :reverb-amount 0.0015
    :amp-env '( 0 0 0.6667 1 4.6801 0.6 9.6970 0.3 24.7475 0.1 100 0 )  
    vln-one-sin
  beg 0.9850 f+ 0.9    733.3330 0.1875 :fm-index 0.8 :distance 1.0 
    :reverb-amount 0.0010
    :amp-env '( 0 0 0.3333 1 4.3603 0.6 9.3939 0.3 24.4950 0.1 100 0 )  
    vln-one-sin
  beg 0.9850 f+ 0.225  550      0.1875 :fm-index 0.8 :distance 1.0 
    :reverb-amount 0.0013
    :amp-env '( 0 0 1.3333 1 5.3199 0.6 10.3030 0.3 25.2525 0.1 100 0 ) 
    vln-one-sin
;event

: long-example  ( -- )
  <'> fth-long-example
  *clm-c-version* if	\ we have only a C version of freeverb
    :reverb <'> freeverb :reverb-data '( :room-decay 0.8 )
  else
    :reverb <'> nrev :reverb-data '( :lp-coeff 0.6 )
  then with-sound
;

: short-example ( -- )
  <'> fth-short-example
  :reverb <'> nrev :reverb-data '( :lp-coeff 0.6 ) with-sound
;

'snd provided? [unless]
  *argc* 2 > [if]
    *argv* array-pop drop
    long-example
  [else]
    short-example
  [then]
[then]

\ fmviolin.fth ends here
