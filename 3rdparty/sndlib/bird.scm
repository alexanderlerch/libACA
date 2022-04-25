;;; bird songs -- (load "bird.scm") then (make-birds)
;;;
;;; some of the bird names come from a 1966 bird guide
;;;
;;; see animals.scm for later versions of some of these songs
;;;
;;; 3-Nov-08:  changed some of the function names to avoid collisions with animals.scm (prepended "b-")
;;; 13-Dec-12: minor optimizations

(provide 'snd-bird.scm)

;;; translated (semi-automatically) from a Sambox note list to bird.clm, then bird.scm

(if (provided? 'snd)
    (if (not (provided? 'snd-ws.scm)) (load "ws.scm"))
    (if (not (provided? 'sndlib-ws.scm)) (load "sndlib-ws.scm")))

(definstrument (bigbird start dur frequency freqskew amplitude freq-envelope amp-envelope partials)
  "(bigbird start dur frequency freqskew amplitude freq-envelope amp-envelope partials)"
  (let ((gls-env (make-env freq-envelope (hz->radians freqskew) dur))
	(os (make-polywave frequency :partials (normalize-partials partials)))
	(amp-env (make-env amp-envelope amplitude dur))
	(beg (seconds->samples start))
	(end (seconds->samples (+ start dur))))
    (do ((i beg (+ i 1)))
	((= i end))
      (outa i (* (env amp-env)
		 (polywave os (env gls-env)))))))

(definstrument (bird start dur frequency freqskew amplitude freq-envelope amp-envelope)
  "(bird start dur frequency freqskew amplitude freq-envelope amp-envelope)"
  (let ((gls-env (make-env freq-envelope (hz->radians freqskew) dur))
	(os (make-oscil frequency))
	(amp-env (make-env amp-envelope amplitude dur))
	(end (seconds->samples (+ start dur)))
	(beg (seconds->samples start)))
    (do ((i beg (+ i 1)))
	((= i end))
      (outa i (* (env amp-env)
		 (oscil os (env gls-env)))))))
  
(define main-amp '(.00 .00 .25 1.00 .60 .70 .75 1.00 1.00 .0))
(define bird-tap '(.00 .00 .01 1.00 .99 1.00 1.00 .0))
(define bird-amp '(.00 .00 .25 1.00 .75 1.00 1.00 .0))

(define (b-orchard-oriole beg)
  "(orchard-oriole beg) produces an orchard oriole call at time 'beg'"
  (let ((oriup '(.00 .00 1.00 1.0))
	(oridwn '(.00 1.00 1.00 .0))
	(oriupdwna '(.00 .00 .60 1.00 1.00 .60 ))
	(oriupdwnb '(.00 .50 .30 1.00 1.00 .0))
	(oribiga '(.00 .90 .15 1.00 .40 .30 .60 .60 .85 .00 1.00 .0))
	(orimid '(.00 1.00 .05 .50 .10 1.00 .25 .00 .85 .50 1.00 .0))
	(oridwnup '(.00 .30 .25 .00 1.00 1.0))
	(oriamp '(.00 .00 .10 1.00 1.00 .0)))
    (set! beg (- beg .38))
    (bird (+ beg .38) .03 3700 100 .05 oridwn main-amp)
    (bird (+ beg .41) .05 2500 1000 .1 oriup main-amp)
    (bigbird (+ beg .5) .1 2000 800 .2 oriupdwna main-amp '(1 1 2 .02 3 .05))
    (bird (+ beg .65) .03 3900 1200 .1 oridwn main-amp)
    (bigbird (+ beg .7) .21 2000 1200 .15 oribiga main-amp '(1 1 2 .05))
    (bird (+ beg 1.0) .05 4200 1000 .1 oridwn main-amp)
    (bigbird (+ beg 1.1) .1 2000 1000 .25 orimid main-amp '(1 1 2 .05))
    (bigbird (+ beg 1.3) .1 2000 1000 .25 orimid main-amp '(1 1 2 .05))
    (bird (+ beg 1.48) .1 2300 3200 .1 oriupdwnb oriamp)
    (bird (+ beg 1.65) .03 1800 300 .05 oriup main-amp)
    (bird (+ beg 1.7) .03 2200 100 .04 oridwn main-amp)
    (bird (+ beg 1.8) .07 2500 2000 .15 oriupdwnb oriamp)
    (bigbird (+ beg 1.92) .2 2400 1200 .25 oridwnup main-amp '(1 1 2 .04))
    (bird (+ beg 2.2) .02 2200 3000 .04 oriup main-amp)
    (bird (+ beg 2.28) .02 2200 3000 .04 oriup main-amp)
    (bigbird (+ beg 2.4) .17 2000 1000 .2 oriupdwna oriamp '(1 1 2 .04))))


(define (b-cassins-kingbird beg)
  "(cassins-kingbird beg) produces a cassins kingbird call at time 'beg'"
  (let ((kingfirst '(.00 .30 .45 1.00 .90 .10 1.00 .0))
	(kingsecond '(.00 .00 .02 .50 .04 .00 .06 .55 .08 .05 .10 .60 .12 .05 .14 .65 .16 .10 .18 .70 .20 .10 .22 .75 .24 .15 .26 .80 .28 .20 .30 .85 .32 .25 .34 .90 .36 .30 .38 .95 .40 .40 .42 1.00 .44 .50 .46 1.00 .48 .45 .50 1.00 .52 .50 .54 1.00 .56 .40 .58 .95 .60 .40 .62 .90 .64 .40 .66 .85 .68 .35 .70 .80 .72 .30 .74 .75 .76 .25 .78 .70 .80 .20 .82 .65 .84 .10 .86 .60 .88 .00 .90 .55 .92 .00 .94 .50 .96 .00 1.00 .40 )))
    (set! beg (- beg .03))
    (bigbird (+ beg .03) .04 1700 1200 .15 kingfirst main-amp '(1 1 2 .5 3 0 4 .2))
    (bigbird (+ beg .12) .18 1700 900 .25 kingsecond main-amp '(1 1 2 .01 3 0 4 .1))))


(define (b-chipping-sparrow beg)
  "(chipping-sparrow beg) produces a chipping sparrow call at time 'beg'"
  (let ((chip-up '(.00 .80 .15 1.00 .75 .30 1.00 .0)))
    (bird beg .05 4000 2400 .2 chip-up main-amp)
    (bird (+ beg .06) .05 4000 2400 .2 chip-up main-amp)
    (bird (+ beg .12) .05 4000 2400 .2 chip-up main-amp)
    (bird (+ beg .18) .05 4000 2400 .2 chip-up main-amp)
    (bird (+ beg .24) .05 4000 2400 .2 chip-up main-amp)
    (bird (+ beg .30) .05 4000 2400 .2 chip-up main-amp)
    (bird (+ beg .36) .05 4000 2400 .2 chip-up main-amp)
    (bird (+ beg .42) .05 4000 2400 .2 chip-up main-amp)
    (bird (+ beg .48) .05 4000 2400 .2 chip-up main-amp)
    (bird (+ beg .54) .05 4000 2400 .2 chip-up main-amp)
    (bird (+ beg .60) .05 4000 2400 .2 chip-up main-amp)
    (bird (+ beg .66) .05 4000 2400 .2 chip-up main-amp)
    (bird (+ beg .72) .05 4000 2400 .2 chip-up main-amp)
    (bird (+ beg .78) .05 4000 2400 .2 chip-up main-amp)
    (bird (+ beg .84) .05 4000 2400 .2 chip-up main-amp)
    (bird (+ beg .90) .05 4000 2400 .2 chip-up main-amp)
    (bird (+ beg .96) .05 4000 2400 .2 chip-up main-amp)))


(define (b-bobwhite beg)
  "(bobwhite beg) produces a bobwhite call at time 'beg'"
  (let ((bobup1 '(.00 .00 .40 1.00 1.00 1.0))
	(bobup2 '(.00 .00 .65 .50 1.00 1.0)))
    (set! beg (- beg .4))
    (bigbird (+ beg .4) .2 1800 200 .1 bobup1 main-amp '(1 1 2 .02))
    (bigbird (+ beg 1) .20 1800 1200 .2 bobup2 main-amp '(1 1 2 .02))))


(define (b-western-meadowlark beg)
  "(western-meadowlark beg) produces a western meadowlark call at time 'beg'"
  (let ((no-skw '(.00 .00 1.00 .0))
	(down-skw '(.00 1.00 .40 .40 1.00 .0))
	(fas-down '(.00 1.00 1.00 .0)))
    (set! beg (- beg .8))
    (bigbird (+ beg .800) .1 2010.000 0.000 .100 no-skw main-amp '(1 1 2 .04))
    (bigbird (+ beg 1.100) .15 3000.000 100.000 .110 down-skw main-amp '(1 1 2 .04))
    (bigbird (+ beg 1.300) .25 2000.000 150.000 .200 down-skw main-amp '(1 1 2 .04))
    (bigbird (+ beg 1.650) .15 3010.000 250.000 .110 down-skw main-amp '(1 1 2 .04))
    (bigbird (+ beg 1.850) .10 2200.000 150.000 .110 down-skw main-amp '(1 1 2 .04))
    (bigbird (+ beg 2.000) .10 3200.000 1400.000 .110 fas-down main-amp '(1 1 2 .04))
    (bigbird (+ beg 2.200) .05 2000.000 200.000 .110 fas-down main-amp '(1 1 2 .04))
    (bigbird (+ beg 2.300) .10 1600.000 0.000 .110 fas-down main-amp '(1 1 2 .04))))


(define (b-scissor-tailed-flycatcher beg)
  "(scissor-tailed-flycatcher beg) produces a scissor-tailed flycatcher call at time 'beg'"
  (let ((scissor '(.00 .00 .40 1.00 .60 1.00 1.00 .0)))
    (bigbird beg .05 1800 1800 .2 scissor main-amp '(1 .5 2 1 3 .5 4 .1 5 .01))))


(define (b-great-horned-owl beg)
  "(great-horned-owl beg) produces a great horned owl call at time 'beg'"
  (let ((owlup '(.00 .00 .30 1.00 1.00 1.0))
	(owldown '(.00 1.00 1.00 .0)))
    (set! beg (- beg .3))
    (bigbird (+ beg .3) .1 300 0 .1 main-amp main-amp '(1 1 3 .02 7 .01))
    (bigbird (+ beg .6) .4 293 6 .1 owldown main-amp '(1 1 3 .02 7 .01))
    (bigbird (+ beg 1.75) .35 293 7 .1 owlup main-amp '(1 1 3 .02 7 .01))
    (bigbird (+ beg 2.5) .2 300 0 .1 owlup main-amp '(1 1 3 .02 7 .01))))


(define (b-black-throated-gray-warbler beg)
  "(black-throated-gray-warbler beg) produces a black throated gray warbler call at time 'beg'"
  (let ((grayone '(.00 .50 .02 .60 .04 .45 .06 .62 .08 .40 .10 .65 .12 .35 .14 .70 .18 .30 .20 .70 .22 .30 .24 .70 .25 .20 .30 .80 .35 .10 .40 .90 .45 .00 .50 1.00 .55 .00 .60 1.00 .65 .00 .70 1.00 .75 .00 .80 1.00 .85 .00 .90 1.00 .95 .00 1.00 .50 ))
	(graytwo '(.00 .00 .01 .40 .02 .00 .03 .40 .04 .00 .05 .40 .06 .00 .07 .40 .08 .00 .09 .40 .10 .00 .25 .80 .40 .30 .55 1.00 .70 .00 .85 .80 1.00 .40 ))
	(graythree '(.00 1.00 .01 .60 .02 1.00 .03 .60 .04 1.00 .05 .60 .06 1.00 .07 .60 .08 1.00 .09 .60 .10 1.00 .11 .60 .12 1.00 .13 .60 .14 1.00 .15 .60 .16 1.00 .17 .60 .18 1.00 .19 .60 .20 1.00 .21 .55 .22 1.00 .23 .50 .24 1.00 .25 .50 .26 1.00 .27 .50 .28 1.00 .29 .50 .30 1.00 .31 .50 .32 1.00 .33 .50 .34 1.00 .35 .50 .36 1.00 .37 .50 .38 1.00 .39 .50 .40 1.00 .41 .50 .42 1.00 .43 .50 .44 1.00 .45 .50 .46 1.00 .47 .50 .48 1.00 .49 .50 .50 1.00 .51 .50 .52 1.00 .53 .50 .54 1.00 .55 .50 .56 1.00 .57 .50 .58 1.00 .59 .50 .60 1.00 1.00 .0))
	(grayfour '(.00 .00 1.00 1.0)))
    (bird beg .12 3700 600 .05 grayone main-amp)
    (bird (+ beg .18) .08 3000 800 .07 graytwo main-amp)
    (bird (+ beg .28) .12 3700 600 .12 grayone main-amp)
    (bird (+ beg .44) .08 3000 800 .15 graytwo main-amp)
    (bird (+ beg .54) .12 3700 600 .20 grayone main-amp)
    (bird (+ beg .72) .08 3000 800 .25 graytwo main-amp)
    (bird (+ beg .82) .12 3700 600 .25 grayone main-amp)
    (bird (+ beg .96) .2 3000 2000 .2 graythree main-amp)
    (bird (+ beg 1.2) .02 4500 500 .05 grayfour main-amp)
    (bird (+ beg 1.25) .02 4200 800 .05 grayfour main-amp)
    (bird (+ beg 1.3) .02 4000 900 .05 grayfour main-amp)))


(define (b-yellow-warbler beg)
  "(yellow-warbler beg) produces a yellow warbler call at time 'beg'"
  (let ((yellow-up '(.00 .00 .60 1.00 1.00 .50 ))
	(yellow-swirl '(.00 1.00 .05 1.00 .60 .00 .80 .30 1.00 .10 ))
	(yellow-down '(.00 1.00 1.00 .0))
	(yellow-last '(.00 .00 .30 .20 .80 .70 1.00 1.0))
	(swirl-amp '(.00 .00 .90 1.00 1.00 .0)))
    (bird beg .05 5600 400 .05 yellow-up main-amp)
    (bird (+ beg .23) .12 5000 1500 .15 yellow-swirl swirl-amp)
    (bird (+ beg .45) .13 5000 1700 .17 yellow-swirl swirl-amp)
    (bird (+ beg .62) .16 5000 2000 .20 yellow-swirl swirl-amp)
    (bird (+ beg .85) .15 5000 2000 .20 yellow-swirl swirl-amp)
    (bird (+ beg 1.05) .075 3700 1000 .20 yellow-down main-amp)
    (bird (+ beg 1.15) .075 3700 800 .15 yellow-down main-amp)
    (bird (+ beg 1.25) .075 3700 800 .15 yellow-down main-amp)
    (bird (+ beg 1.4)  .2   3700 2000 .2 yellow-last swirl-amp)))


(define (b-black-necked-stilt beg)
  "(black-necked-stilt beg) produces a black necked stilt call at time 'beg'"
  (let (
	;;	have to guess about upper partials (cut off by spectrograph)
	;;	"birds" book has piping sound coming back down whereas "songs
	;;	of western birds" just shows it going up.
	;;
	(upamp '(.00 .00 .90 1.00 1.00 .0))
	(rampup '(.00 .00 .50 1.00 1.00 .20 )))
    (bigbird beg .1 900 100 .2 rampup upamp '( 1 .5  2 1 3 .75 4 .5  5 .1))
    (bigbird (+ beg .30) .1 900 200 .2 rampup upamp '( 1 .5  2 1 3 .75 4 .5  5 .1))
    (bigbird (+ beg .60) .1 900 250 .2 rampup upamp '( 1 .5  2 1 3 .75 4 .5  5 .1))))


(define (b-chestnut-sided-warbler beg)
  "(chestnut-sided-warbler beg) produces a chestnut sided warbler call at time 'beg'"
  (let ((ycurve '(.00 1.00 .30 .50 .60 1.00 .80 .20 1.00 .0))
	(vcurve '(.00 .20 .50 1.00 1.00 .0))
	(wcurve '(.00 .50 .15 .00 .45 .10 .60 1.00 .70 .90 1.00 .90 ))
	(upcurve '(.00 .00 .95 1.00 1.00 1.0))
	(downcurve '(.00 1.00 .25 .30 .60 .15 1.00 .0))
	(louder '(.00 .00 .90 1.00 1.00 .0))
	(wamp '(.00 .00 .10 1.00 .40 .10 .50 .90 .60 .10 .70 1.00 1.00 .0)))
    (set! beg (- beg .1))
    (bigbird (+ beg .1) .1 4050 1200 .05 ycurve main-amp '(1 1 2 .1))
    (bigbird (+ beg .25) .03 3900 300 .075 vcurve main-amp '(1 1 2 .1))
    (bigbird (+ beg .3) .1 4050 1200 .15 ycurve louder '(1 1 2 .1))
    (bigbird (+ beg .42) .03 3800 500 .1 vcurve main-amp '(1 1 2 .1))
    (bigbird (+ beg .5) .1 4000 1200 .2 ycurve bird-tap '(1 1 2 .1))
    (bigbird (+ beg .65) .03 3800 500 .15 vcurve main-amp '(1 1 2 .1))
    (bigbird (+ beg .72) .1 4000 1200 .2 ycurve bird-tap '(1 1 2 .1))
    (bigbird (+ beg .85) .03 3800 500 .15 vcurve main-amp '(1 1 2 .1))
    (bigbird (+ beg .91) .1 4000 1200 .2 ycurve bird-tap '(1 1 2 .1))
    (bigbird (+ beg 1.05) .12 3800 2200 .15 wcurve wamp '(1 1 2 .1))
    (bigbird (+ beg 1.20) .12 3800 2200 .15 wcurve wamp '(1 1 2 .1))
    (bigbird (+ beg 1.35) .12 2500 2200 .25 upcurve louder '(1 1 2 .1))
    (bigbird (+ beg 1.50) .12 2500 4000 .15 downcurve main-amp '(1 1 2 .1))))


(define (b-grasshopper-sparrow beg)
  "(grasshopper-sparrow beg) produces a grasshopper sparrow call at time 'beg'"
  (let ((grassone '(.00 .50 .02 .80 .04 .30 .06 .80 .07 .10 .08 .90 .10 .00 .11 .90 .12 .00 .13 .90 .14 .10 .15 1.00 .16 .10 .17 1.00 .18 .10 .19 1.00 .20 .10 .21 1.00 .22 .10 .23 1.00 .24 .10 .25 1.00 .26 .10 .27 1.00 .28 .10 .29 1.00 .30 .10 .31 1.00 .32 .10 .33 1.00 .34 .10 .35 1.00 .36 .10 .37 1.00 .38 .10 .39 1.00 .40 .10 .41 1.00 .42 .10 .43 1.00 .44 .10 .45 1.00 .46 .10 .47 1.00 .48 .10 .49 1.00 .50 .10 .51 1.00 .52 .10 .53 1.00 .54 .10 .55 1.00 .56 .10 .57 1.00 .58 .10 .59 1.00 .60 .10 .61 1.00 .62 .10 .63 1.00 .64 .10 .65 1.00 .66 .10 .67 1.00 .68 .10 .69 1.00 .70 .10 .71 1.00 .72 .10 .73 1.00 .74 .10 .75 1.00 .76 .10 .77 1.00 .78 .10 .79 1.00 .80 .10 .81 1.00 .82 .10 .83 1.00 .84 .10 .85 1.00 .86 .10 .87 1.00 .88 .10 .89 1.00 .90 .10 .91 1.00 .92 .10 .93 1.00 .94 .10 .95 1.00 .96 .10 .97 1.00 .98 .10 1.00 1.0))
	(grasstwo '(.00 .00 .10 1.00 .20 .00 .30 1.00 .40 .00 .50 1.00 .60 .00 .70 1.00 .80 .00 .90 1.00 1.00 .0)))
    (set! beg (- beg .49))
    (bird (+ beg .49) .01 8000 100 .1 grasstwo main-amp)
    (bird (+ beg .60) .01 5700 300 .1 grasstwo main-amp)
    (bird (+ beg .92) .01 3900 100 .1 grasstwo main-amp)
    (bird (+ beg 1.00) 1.4 6000 2500 .2 grassone main-amp)))


(define (b-swamp-sparrow beg)
  "(swamp-sparrow  beg) produces a swamp sparrow call at time 'beg'"
  (let ((swamp-up '(.00 .00 .60 .70 1.00 1.0))
	(swamp-down '(.00 1.00 .50 .50 .60 .60 1.00 .0)))
    (bird beg .02 3900 200 .3 swamp-up main-amp)
    (bird (+ beg .035) .035 3200 3000 .1 swamp-down main-amp)
    (bird (+ beg .08) .025 3700 0 .1 main-amp main-amp)
    
    (bird (+ beg .1) .02 3900 200 .3 swamp-up main-amp)
    (bird (+ beg .135) .035 3200 3000 .1 swamp-down main-amp)
    (bird (+ beg .18) .025 3700 0 .1 main-amp main-amp)
    
    (bird (+ beg .2) .02 3900 200 .3 swamp-up main-amp)
    (bird (+ beg .235) .035 3200 3000 .1 swamp-down main-amp)
    (bird (+ beg .28) .025 3700 0 .1 main-amp main-amp)
    
    (bird (+ beg .3) .02 3900 200 .3 swamp-up main-amp)
    (bird (+ beg .335) .035 3200 3000 .1 swamp-down main-amp)
    (bird (+ beg .38) .025 3700 0 .1 main-amp main-amp)
    
    (bird (+ beg .4) .02 3900 200 .3 swamp-up main-amp)
    (bird (+ beg .435) .035 3200 3000 .1 swamp-down main-amp)
    (bird (+ beg .48) .025 3700 0 .1 main-amp main-amp)
    
    (bird (+ beg .5) .02 3900 200 .3 swamp-up main-amp)
    (bird (+ beg .535) .035 3200 3000 .1 swamp-down main-amp)
    (bird (+ beg .58) .025 3700 0 .1 main-amp main-amp)
    
    (bird (+ beg .6) .02 3900 200 .3 swamp-up main-amp)
    (bird (+ beg .635) .035 3200 3000 .1 swamp-down main-amp)
    (bird (+ beg .68) .025 3700 0 .1 main-amp main-amp)
    
    (bird (+ beg .7) .02 3900 200 .3 swamp-up main-amp)
    (bird (+ beg .735) .035 3200 3000 .1 swamp-down main-amp)
    (bird (+ beg .78) .025 3700 0 .1 main-amp main-amp)
    
    (bird (+ beg .8) .02 3900 200 .3 swamp-up main-amp)
    (bird (+ beg .835) .035 3200 3000 .1 swamp-down main-amp)
    (bird (+ beg .88) .025 3700 0 .1 main-amp main-amp)
    
    (bird (+ beg .9) .02 3900 200 .3 swamp-up main-amp)
    (bird (+ beg .935) .035 3200 3000 .1 swamp-down main-amp)
    (bird (+ beg .98) .025 3700 0 .1 main-amp main-amp)))


(define (b-golden-crowned-sparrow beg)
  "(golden-crowned-sparrow beg) produces a golden crowned sparrow call at time 'beg'"
  (let (
	;;	these have as different song around here.
	(goldone '(.00 1.00 .25 .20 1.00 .0))
	(goldtwo '(.00 .90 .05 1.00 .10 .40 1.00 .0))
	(goldtrill '(.00 .50 .10 .00 .20 1.00 .30 .00 .40 1.00 .50 .00 .60 1.00 .70 .00 .80 1.00 .90 .00 1.00 .50 )))
    (set! beg (- beg .6))
    (bird (+ beg .6) .5 4300 1000 .15 goldone main-amp)
    (bird (+ beg 1.3) .45 3300 200 .15 goldone main-amp)
    (bird (+ beg 1.75) .4 3800 100 .15 goldtwo main-amp)
    (bird (+ beg 2.2) .3 3800 100 .1 goldtrill main-amp)))


(define (b-indigo-bunting beg)
  "(indigo-bunting beg) produces a indigo bunting call at time 'beg'"
  (let ((buntdwn '(.00 1.00 1.00 .0))
	(buntv '(.00 .00 .50 1.00 1.00 .0))
	(bunty '(.00 1.00 .50 .00 1.00 .90 ))
	(buntn '(.00 .80 .30 1.00 .70 .20 1.00 .0))
	(buntx '(.00 1.00 .10 .50 .25 .90 1.00 .0))
	(buntup '(.00 .00 1.00 1.0)))
    (set! beg (- beg .4))
    (bird (+ beg .4) .08 3000 700 .25 buntdwn main-amp)
    (bird (+ beg .52) .02 6200 1000 .05 buntdwn main-amp)
    (bird (+ beg .55)  .15 3500 2300 .1 buntv main-amp)
    (bird (+ beg .74) .02 6200 1800 .05 buntx main-amp)
    (bird (+ beg .80) .15 3400 2300 .1 buntv main-amp)
    (bird (+ beg 1.00) .1 3400 800 .2 buntv main-amp)
    (bird (+ beg 1.13) .03 4100 2000 .05 buntdwn main-amp)
    (bird (+ beg 1.25) .08 3400 800 .2 buntv main-amp)
    (bird (+ beg 1.40) .03 4100 2000 .05 buntdwn main-amp)
    (bird (+ beg 1.5) .07 3700 300 .1 buntdwn main-amp)
    (bird (+ beg 1.6) .1  4100 2200 .15 bunty main-amp)
    (bird (+ beg 1.72) .05 3700 300 .1 buntdwn main-amp)
    (bird (+ beg 1.81) .1  4100 2200 .15 bunty main-amp)
    (bird (+ beg 1.94) .07 5200 1800 .2 buntn main-amp)
    (bird (+ beg 2.05) .08 3000 1500 .15 buntup main-amp)
    (bird (+ beg 2.20) .07 5200 1800 .2 buntn main-amp)
    (bird (+ beg 2.33) .08 3000 1500 .15 buntup main-amp)
    (bird (+ beg 2.43) .07 5200 1800 .1 buntn main-amp)
    (bird (+ beg 2.51) .08 3000 1500 .10 buntup main-amp)))


(define (b-hooded-warbler beg)
  "(hooded-warbler beg) produces a hooded warbler call at time 'beg'"
  (let ((hoodup '(.00 .00 1.00 1.0))
	(hooddown '(.00 1.00 1.00 .0)))
    (set! beg (- beg .6))
    (bird (+ beg .6) .03 3900 1600 .05 hooddown main-amp)
    (bird (+ beg .64) .03 3900 1700 .05 hooddown main-amp)
    (bird (+ beg .8) .03 3900 2000 .10 hooddown main-amp)
    (bird (+ beg .84) .03 3900 2000 .10 hooddown main-amp)
    (bird (+ beg .93) .03 3900 2100 .15 hooddown main-amp)
    (bird (+ beg .97) .03 3900 2100 .15 hooddown main-amp)
    (bird (+ beg 1.05) .03 3900 2100 .05 hooddown main-amp)
    (bird (+ beg 1.09) .03 3900 2100 .2 hooddown main-amp)
    (bird (+ beg 1.17) .03 3900 2100 .2 hooddown main-amp)
    (bird (+ beg 1.21) .03 3900 2100 .2 hooddown main-amp)
    (bird (+ beg 1.39) .03 3900 2100 .2 hooddown main-amp)
    (bird (+ beg 1.43) .03 3900 2100 .2 hooddown main-amp)
    (bird (+ beg 1.51) .03 3900 2100 .2 hooddown main-amp)
    (bird (+ beg 1.55) .03 3900 2100 .2 hooddown main-amp)
    (bird (+ beg 1.63) .03 3900 2100 .2 hooddown main-amp)
    (bird (+ beg 1.67) .03 3900 2100 .2 hooddown main-amp)
    (bird (+ beg 1.75) .03 3900 2100 .2 hooddown main-amp)
    (bird (+ beg 1.80) .03 3900 2100 .2 hooddown main-amp)
    
    (bird (+ beg 1.90) .04 3000 1000 .15 hoodup main-amp)
    (bird (+ beg 1.98) .04 3000 1000 .15 hoodup main-amp)
    (bird (+ beg 2.05) .04 3000 1000 .15 hoodup main-amp)
    (bird (+ beg 2.13) .04 3000 1000 .15 hoodup main-amp)
    (bird (+ beg 2.21) .04 3000 1000 .15 hoodup main-amp)
    (bird (+ beg 2.29) .04 3000 1000 .15 hoodup main-amp)
    (bird (+ beg 2.37) .04 3000 1000 .15 hoodup main-amp)
    (bird (+ beg 2.45) .04 3000 1000 .15 hoodup main-amp)))


(define (b-american-widgeon beg)
  "(american-widgeon beg) produces an american widgeon call at time 'beg'"
  (let ((widgeon '(.00 .00 .50 1.00 1.00 .0)))
    (set! beg (- beg .3))
    (bigbird (+ beg .3) .07 1900 300 .15 widgeon widgeon '(1 1 2 .02))
    (bigbird (+ beg .4) .11 1700 1400 .25 widgeon widgeon '(1 .7 2 1 3 .02))
    (bigbird (+ beg .55) .07 1900 300 .15 widgeon widgeon '(1 1 2 .02))))


(define (b-louisiana-waterthrush beg)
  "(louisiana-waterthrush beg) produces a louisiana waterthrush call at time 'beg'"
  (let ((water-one '(.00 .80 .35 .40 .45 .90 .50 1.00 .75 1.00 1.00 .10 ))
	(water-two '(.00 1.00 .40 .00 .60 .10 1.00 .80 ))
	(water-three '(.00 1.00 .95 .00 1.00 .0))
	(water-four '(.00 .00 1.00 1.0))
	(water-five '(.00 1.00 1.00 .0))
	(water-amp '(.00 .00 .35 1.00 .50 .20 .90 1.00 1.00 .0))
	(water-damp '(.00 .00 .90 1.00 1.00 .0)))
    (bird beg .17 4100 2000 .2 water-one water-amp)
    (bird (+ beg .32) .18 4050 2050 .3 water-one water-amp)
    (bird (+ beg .64) .20 4000 1900 .25 water-one water-amp)
    (bird (+ beg .9) .2 3900 2000 .3 water-two bird-tap)
    (bird (+ beg 1.25) .12 3000 3000 .25 water-three water-damp)
    (bird (+ beg 1.4) .1 2700 1500 .2 water-four water-damp)
    (bird (+ beg 1.58) .02 5200 1000 .1 water-five main-amp)
    (bird (+ beg 1.65) .02 5200 1000 .1 water-five main-amp)
    (bird (+ beg 1.7) .035 3200 1000 .1 water-four water-damp)))


(define (b-robin beg)
  "(robin beg) produces a robin call at time 'beg'"
  (let ((r-one '(.00 .10 .08 .70 .30 .00 .35 1.00 .40 .30 1.00 .30 ))
	(r-two '(.00 .00 .10 1.00 .20 .70 .35 .70 .65 .30 .70 .50 .80 .00 .90 .20 1.00 .0))
	(r-three '(.00 .20 .25 1.00 .60 .70 .90 .00 1.00 .10 ))
	(r-four '(.00 1.00 1.00 .0))
	(r-five '(.00 .50 .10 .00 .20 1.00 .30 .00 .40 1.00 .50 .00 .60 1.00 .70 .50 1.00 .20 ))
	(r-six '(.00 .00 .12 .70 .30 .00 .70 1.00 1.00 .50 )))
    (set! beg (- beg .45))
    (bigbird (+ beg .45) .06 2000 800 .15 r-six main-amp '(1 1 2 .1))
    (bigbird (+ beg .56) .10 2000 900 .15 r-one main-amp '(1 1 2 .1))
    (bigbird (+ beg 1.04) .24 2000 2000 .25 r-two main-amp '(1 1 2 .1))
    (bigbird (+ beg 1.63) .13 1900 1600 .20 r-three main-amp '(1 1 2 .1))
    (bigbird (+ beg 1.80) .11 2200 1200 .25 r-four main-amp '(1 1 2 .1))
    (bigbird (+ beg 2.31) .21 1950 2000 .15 r-five main-amp '(1 1 2 .1))))


(define (b-solitary-vireo beg)
  "(solitary-vireo beg) produces a solitary vireo call at time 'beg'"
  (let ((bigskew '(.00 .20 .03 .30 .06 .10 .10 .50 .13 .40 .16 .80 .19 .50 .22 .90 .25 .60 .28 1.00 .31 .60 .34 1.00 .37 .50 .41 .90 .45 .40 .49 .80 .51 .40 .54 .75 .57 .35 .60 .70 .63 .30 .66 .60 .69 .25 .72 .50 .75 .20 .78 .30 .82 .10 .85 .30 .88 .05 .91 .30 .94 .00 .95 .30 .99 .00 1.00 .10 )))
    (bird beg .4 1800 1200 .2 bigskew main-amp)))


(define (b-pigeon-hawk beg)
  "(pigeon-hawk beg) produces a pigeon hawk (merlin) call at time 'beg'"
  (let ((hupdown '(.00 .00 .30 1.00 .70 1.00 1.00 .0)))
    (bigbird beg .1 1900 200 .2 hupdown main-amp '(1 .7 2 1))
    (bigbird (+ beg .12) .01 2050 0 .1 main-amp main-amp '(1 .5 2 1))
    (bigbird (+ beg .13) .1 1900 200 .2 hupdown main-amp '(1 .7 2 1))
    (bigbird (+ beg .25) .01 2050 0 .1 main-amp main-amp '(1 .5 2 1))
    (bigbird (+ beg .26) .1 1900 200 .2 hupdown main-amp '(1 .7 2 1))
    (bigbird (+ beg .38) .01 2050 0 .1 main-amp main-amp '(1 .5 2 1))
    (bigbird (+ beg .39) .1 1900 200 .2 hupdown main-amp '(1 .7 2 1))
    (bigbird (+ beg .51) .01 2050 0 .1 main-amp main-amp '(1 .5 2 1))
    (bigbird (+ beg .52) .1 1900 200 .2 hupdown main-amp '(1 .7 2 1))
    (bigbird (+ beg .64) .01 2050 0 .1 main-amp main-amp '(1 .5 2 1))
    (bigbird (+ beg .65) .1 1900 200 .2 hupdown main-amp '(1 .7 2 1))
    (bigbird (+ beg .77) .01 2050 0 .1 main-amp main-amp '(1 .5 2 1))
    (bigbird (+ beg .78) .1 1900 200 .2 hupdown main-amp '(1 .7 2 1))
    (bigbird (+ beg .90) .01 2050 0 .1 main-amp main-amp '(1 .5 2 1))
    (bigbird (+ beg .91) .1 1900 200 .2 hupdown main-amp '(1 .7 2 1))
    (bigbird (+ beg 1.03) .01 2050 0 .1 main-amp main-amp '(1 .5 2 1))
    (bigbird (+ beg 1.04) .1 1900 200 .2 hupdown main-amp '(1 .7 2 1))
    (bigbird (+ beg 1.16) .01 2050 0 .1 main-amp main-amp '(1 .5 2 1))
    (bigbird (+ beg 1.17) .1 1900 200 .2 hupdown main-amp '(1 .7 2 1))
    (bigbird (+ beg 1.29) .01 2050 0 .1 main-amp main-amp '(1 .5 2 1))
    (bigbird (+ beg 1.30) .1 1900 200 .2 hupdown main-amp '(1 .7 2 1))
    (bigbird (+ beg 1.42) .01 2050 0 .1 main-amp main-amp '(1 .5 2 1))
    (bigbird (+ beg 1.43) .1 1900 200 .2 hupdown main-amp '(1 .7 2 1))
    (bigbird (+ beg 1.55) .01 2050 0 .1 main-amp main-amp '(1 .5 2 1))
    (bigbird (+ beg 1.56) .1 1900 200 .2 hupdown main-amp '(1 .7 2 1))
    (bigbird (+ beg 1.68) .01 2050 0 .1 main-amp main-amp '(1 .5 2 1))
    (bigbird (+ beg 1.69) .1 1900 200 .2 hupdown main-amp '(1 .7 2 1))
    (bigbird (+ beg 1.81) .01 2050 0 .1 main-amp main-amp '(1 .5 2 1))
    (bigbird (+ beg 1.82) .1 1900 200 .2 hupdown main-amp '(1 .7 2 1))))


(define (b-cerulean-warbler beg)
  "(cerulean-warbler beg) produces a cerulean warbler call at time 'beg'"
  (let ((w-down '(.00 1.00 1.00 .0))
	(trill '(.00 .80 .10 1.00 .25 .50 .40 1.00 .55 .50 .70 1.00 1.00 .0))
	(w-up '(.00 .00 1.00 1.0)))
    (set! beg (- beg .27))
    (bird (+ beg .27) .05 3000 1000 .05 w-down main-amp)
    (bird (+ beg .33) .05 3000 800 .075 w-up main-amp)
    (bird (+ beg .41) .01 3200 700 .07 w-down main-amp)
    (bird (+ beg .42) .01 3200 700 .08 w-down main-amp)
    (bird (+ beg .43) .06 3200 700 .09 w-down main-amp)
    (bird (+ beg .51) .06 3200 500 .1 w-up main-amp)
    (bird (+ beg .6) .10 3000 1200 .2 trill main-amp)
    (bird (+ beg .72) .05 3000 800 .2 w-up main-amp)
    (bird (+ beg .8) .10 3000 1200 .2 trill main-amp)
    (bird (+ beg .92) .05 3000 800 .2 w-up main-amp)
    (bird (+ beg 1.00) .01 3900 600 .1 w-up main-amp)
    (bird (+ beg 1.01) .01 3910 800 .1 w-up main-amp)
    (bird (+ beg 1.02) .01 3940 500 .1 w-up main-amp)
    (bird (+ beg 1.03) .01 4000 500 .1 w-up main-amp)
    (bird (+ beg 1.04) .01 3900 1000 .1 w-up main-amp)
    (bird (+ beg 1.05) .01 3900 1000 .1 w-up main-amp)
    (bird (+ beg 1.06) .01 3900 1000 .1 w-up main-amp)
    (bird (+ beg 1.07) .01 3900 1000 .1 w-up main-amp)
    (bird (+ beg 1.08) .01 3900 1000 .1 w-up main-amp)
    (bird (+ beg 1.09) .01 3900 1000 .1 w-up main-amp)
    (bird (+ beg 1.10) .01 3900 1000 .1 w-up main-amp)
    (bird (+ beg 1.11) .01 3900 1000 .1 w-up main-amp)
    (bird (+ beg 1.12) .01 3900 1000 .1 w-up main-amp)
    (bird (+ beg 1.13) .01 3900 1000 .1 w-up main-amp)
    (bird (+ beg 1.14) .01 3900 1000 .1 w-up main-amp)
    (bird (+ beg 1.15) .01 3900 1000 .1 w-up main-amp)
    (bird (+ beg 1.16) .01 3900 1000 .1 w-up main-amp)
    (bird (+ beg 1.17) .01 3900 1000 .1 w-up main-amp)
    (bird (+ beg 1.18) .01 3900 1000 .1 w-up main-amp)
    (bird (+ beg 1.19) .01 3900 1000 .1 w-up main-amp)
    (bird (+ beg 1.20) .01 3900 1000 .1 w-up main-amp)
    (bird (+ beg 1.21) .01 3900 1000 .1 w-up main-amp)
    (bird (+ beg 1.22) .01 3900 1000 .1 w-up main-amp)
    (bird (+ beg 1.23) .01 3900 1200 .1 w-up main-amp)
    (bird (+ beg 1.24) .01 3900 1200 .1 w-up main-amp)
    (bird (+ beg 1.25) .01 3900 1200 .1 w-up main-amp)
    (bird (+ beg 1.26) .01 3900 1200 .1 w-up main-amp)
    (bird (+ beg 1.27) .01 3900 1400 .1 w-up main-amp)
    (bird (+ beg 1.28) .01 3900 1400 .1 w-up main-amp)
    (bird (+ beg 1.29) .01 3900 1400 .1 w-up main-amp)
    (bird (+ beg 1.30) .01 3900 1400 .1 w-up main-amp)))


(define (b-nashville-warbler beg)
  "(nashville-warbler beg) produces a nashville warbler call at time 'beg'"
  (let ((nash-blip '(.00 .60 .35 1.00 1.00 .0))
	(nash-down '(.00 .90 .05 1.00 .10 .90 .65 .50 1.00 .0))
	(nash-up '(.00 .00 .15 .20 .25 .05 .90 .95 1.00 1.0))
	(nash-amp '(.00 .00 .80 1.00 1.00 .0)))
    (set! beg (- beg .15))
    (bird (+ beg .15) .025 3900 300 .3 nash-blip main-amp)
    (bird (+ beg .24) .16 4200 3800 .15 nash-down nash-amp)
    (bird (+ beg .42) .025 3900 300 .3 nash-blip main-amp)
    (bird (+ beg .55) .14 4300 3700 .15 nash-down nash-amp)
    (bird (+ beg .75) .03 3950 350 .3 nash-blip main-amp)
    (bird (+ beg .81) .17 4200 3900 .175 nash-down main-amp)
    (bird (+ beg 1.0) .02 3800 400 .25 nash-blip main-amp)
    (bird (+ beg 1.11) .14 4200 3800 .165 nash-down nash-amp)
    (bird (+ beg 1.3) .03 3750 300 .2 nash-blip main-amp)
    (bird (+ beg 1.4) .11 4200 3700 .1 nash-down main-amp)
    (bird (+ beg 1.57) .1 3800 2200 .1 nash-up main-amp)
    (bird (+ beg 1.7) .1 3800 2150 .125 nash-up main-amp)
    (bird (+ beg 1.85) .075 3900 1800 .1 nash-up nash-amp)))


(define (b-eastern-phoebe beg)
  "(eastern-phoebe beg) produces an eastern-phoebe call at time 'beg'"
  (let ((phoebe-one '(.00 .00 .30 .30 .35 .50 .55 .40 .70 .80 .75 .70 .80 1.00 .95 .90 1.00 .0))
	(phoebe-two '(.00 .00 .50 1.00 1.00 .0))
	(phoebe-three '(.00 .00 .10 .40 .80 1.00 1.00 .10 ))
	(phoebe-four '(.00 1.00 .50 .70 1.00 .0))
	(phoebe-amp '(.00 .00 .10 1.00 1.00 .0)))
    (bird beg .225 3000 1300 .3 phoebe-one main-amp)
    (bird (+ beg .35) .12 3000 500 .1 phoebe-two phoebe-amp)
    (bird (+ beg .4) .10 3000 1500 .2 phoebe-three phoebe-amp)
    (bird (+ beg .55) .05 3000 1400 .2 phoebe-four phoebe-amp)))


(define (b-painted-bunting beg)
  "(painted-bunting beg) produces a painted bunting call at time 'beg'"
  (let ((b-one '(.00 .00 1.00 1.0))
	(b-two '(.00 .00 .90 1.00 1.00 .0))
	(b-three '(.00 1.00 1.00 .0))
	(b-four '(.00 .00 .50 1.00 1.00 .0))
	(b-five '(.00 .70 .15 .00 .40 1.00 .80 1.00 1.00 .50 ))
	(b-six '(.00 .00 .10 .50 .15 .00 .40 1.00 .90 1.00 1.00 .0))
	(b-seven '(.00 1.00 .25 .40 .75 .50 1.00 .0))
	(b-eight '(.00 .30 .40 .40 .50 1.00 .60 .20 1.00 .0))
	(b-nine '(.00 .00 .05 1.00 .30 1.00 .50 .30 .90 1.00 1.00 .0))
	(b-ten '(.00 .40 .25 .00 .35 1.00 .50 .00 .65 1.00 .75 .00 .85 1.00 1.00 .0))
	(b-eleven '(.00 1.00 1.00 .0))
	(b-twelve '(.00 .00 .50 1.00 1.00 .50 ))
	(b-thirteen '(.00 .00 .05 1.00 .30 .20 .60 .20 .90 1.00 1.00 .0))
	(b-fourteen '(.00 .30 .30 1.00 .60 .30 1.00 .0))
	(b-fifteen '(.00 .00 .10 .50 .50 .50 .90 1.00 1.00 .0)))
    (set! beg (- beg .05))
    (bird (+ beg .05) .10 3100 900 .05 b-one b-two)
    (bird (+ beg .21) .07 4100 700 .15 b-three main-amp)
    (bird (+ beg .36) .12 3700 1000 .20 b-four main-amp)
    (bird (+ beg .52) .08 2300 1600 .15 b-five b-six)
    (bird (+ beg .68) .1 4000 1000 .25 b-one bird-tap)
    (bird (+ beg .8) .12 2300 1700 .2 b-seven main-amp)
    (bird (+ beg .96) .15 3800 2200 .3 b-eight b-nine)
    (bird (+ beg 1.18) .1 2300 1600 .15 b-ten main-amp)
    (bird (+ beg 1.3) .02 3200 1000 .1 b-eleven main-amp)
    (bird (+ beg 1.33) .02 3200 1000 .1 b-eleven main-amp)
    (bird (+ beg 1.36) .02 3200 1000 .1 b-eleven main-amp)
    (bird (+ beg 1.40) .03 4000 2000 .12 b-twelve b-thirteen)
    (bird (+ beg 1.47) .1 2300 1700 .2 b-fourteen b-fifteen)))


(define (b-western-flycatcher beg)
  "(western-flycatcher beg) produces a western flycatcher call at time 'beg'"
  (let ((f-one '(.00 .00 .10 1.00 .20 .40 .95 .10 1.00 .0))
	(a-one '(.00 .00 .10 .20 .20 .10 .30 1.00 .90 1.00 1.00 .0))
	(f-two '(.00 .50 .25 1.00 .50 .00 .60 .00 .95 .30 1.00 .60 ))
	(a-two '(.00 .00 .10 1.00 .20 1.00 .50 .10 .60 .10 .90 1.00 1.00 .0)))
    (bigbird beg .2 2000 2200 .2 f-one a-one '(1 1 2 .02 3 .1 4 .01))
    (bigbird (+ beg .3) .2 2000 1100 .2 f-two a-two '(1 1 2 .02 3 .1 4 .01))))


(define (b-bachmans-sparrow beg)
  "(bachmans-sparrow beg) produces a bachmans sparrow call at time 'beg'"
  (let ((sopening '(.00 1.00 .10 .50 .90 .50 1.00 .0))
	(sup '(.00 .10 .35 .00 1.00 1.0))
	(sdwn '(.00 1.00 .40 .50 1.00 .0))
	(supn '(.00 .00 1.00 1.0))
	(slast '(.00 1.00 .25 .00 .75 .40 1.00 .50 )))
    (bird beg .51 4900 200 .3 sopening main-amp)
    (bird (+ beg .52) .015 3800 200 .1 sup main-amp)
    (bird (+ beg .52) .015 3750 250 .1 sup main-amp)
    (bird (+ beg .54) .015 3600 300 .1 sup main-amp)
    (bird (+ beg .56) .015 3500 250 .1 sup main-amp)
    (bird (+ beg .58) .015 3400 200 .1 sup main-amp)
    (bird (+ beg .60) .015 3200 200 .1 sup main-amp)
    (bird (+ beg .62) .015 3800 100 .1 sup main-amp)
    
    (bird (+ beg .65) .07 3000 750 .2 sup main-amp)
    (bird (+ beg .73) .03 5000 1000 .1 sdwn main-amp)
    (bird (+ beg .80) .07 3000 750 .2 sup main-amp)
    (bird (+ beg .88) .03 5000 1000 .1 sdwn main-amp)
    (bird (+ beg .95) .07 3000 750 .2 sup main-amp)
    (bird (+ beg 1.03) .03 5000 1000 .1 sdwn main-amp)
    (bird (+ beg 1.10) .07 3000 750 .2 sup main-amp)
    (bird (+ beg 1.18) .03 5000 1000 .1 sdwn main-amp)
    (bird (+ beg 1.25) .07 3000 750 .2 sup main-amp)
    (bird (+ beg 1.33) .03 5000 1000 .1 sdwn main-amp)
    (bird (+ beg 1.40) .07 3000 750 .2 sup main-amp)
    (bird (+ beg 1.48) .03 5000 1000 .1 sdwn main-amp)
    (bird (+ beg 1.55) .07 3000 750 .2 sup main-amp)
    (bird (+ beg 1.63) .03 5000 1000 .1 sdwn main-amp)
    
    (bird (+ beg 2.8) .06 4000 1700 .1 supn main-amp)
    (bird (+ beg 2.87) .01 5200 0 .2 supn main-amp)
    (bird (+ beg 2.9) .06 4000 1700 .1 supn main-amp)
    (bird (+ beg 2.97) .01 5200 0 .2 supn main-amp)
    (bird (+ beg 3.0) .06 4000 1700 .1 supn main-amp)
    (bird (+ beg 3.07) .01 5200 0 .2 supn main-amp)
    (bird (+ beg 3.1) .06 4000 1700 .1 supn main-amp)
    (bird (+ beg 3.17) .01 5200 0 .2 supn main-amp)
    (bird (+ beg 3.2) .06 4000 1700 .1 supn main-amp)
    (bird (+ beg 3.27) .01 5200 0 .2 supn main-amp)
    
    (bird (+ beg 3.4) .15 3000 1000 .2 slast main-amp)
    (bird (+ beg 3.6) .15 3000 1000 .2 slast main-amp)
    (bird (+ beg 3.8) .15 3000 1000 .2 slast main-amp)
    (bird (+ beg 4.0) .15 3000 1000 .2 slast main-amp)
    (bird (+ beg 4.2) .15 3000 1000 .2 slast main-amp)
    (bird (+ beg 4.4) .15 3000 1000 .2 slast main-amp)))


(define (b-cedar-waxwing beg)
  "(cedar-waxwing beg) produces a cedar waxwing call at time 'beg'"
  (let ((cedar '(.00 .00 .25 .70 .70 1.00 .90 1.00 1.00 .20 ))
	(cedamp '(.00 .00 .20 1.00 .40 1.00 1.00 .0)))
    (bird beg .50 6000 800 .2 cedar cedamp)))


(define (b-bairds-sparrow beg)
  "(bairds-sparrow beg) produces a bairds sparrow call at time 'beg'"
  (let ((bairdend '(.00 .00 .25 1.00 .50 .00 .75 1.00 1.00 .0))
	(bairdstart '(.00 .50 .05 1.00 .10 .00 .15 1.00 .20 .00 .25 1.00 .30 .00 .35 1.00 .40 .00 .45 1.00 .50 .00 .55 1.00 .60 .00 .65 1.00 .70 .00 .75 1.00 .80 .00 .85 1.00 .90 .00 .95 1.00 1.00 .0)))
    (bird beg .09 6500 1500 .2 bairdstart main-amp)
    (bird (+ beg .22) .01 5900 100 .2 bairdend main-amp)
    (bird (+ beg .25) .09 6000 1000 .2 bairdstart main-amp)
    (bird (+ beg .45) .01 4200 100 .2 bairdend main-amp)
    (bird (+ beg .50) .08 4200 600 .2 bairdstart main-amp)
    (bird (+ beg .59) .01 4400 100 .2 bairdend main-amp)
    (bird (+ beg .60) .01 4400 100 .2 bairdend main-amp)
    (bird (+ beg .68) .07 5400 700 .2 bairdstart main-amp)
    (bird (+ beg .75) .01 4200 100 .2 bairdend main-amp)
    (bird (+ beg .79) .01 4400 100 .2 bairdend main-amp)
    (bird (+ beg .83) .01 4200 100 .19 bairdend main-amp)
    (bird (+ beg .87) .01 4400 100 .19 bairdend main-amp)
    (bird (+ beg .91) .01 4200 100 .18 bairdend main-amp)
    (bird (+ beg .95) .01 4400 100 .18 bairdend main-amp)
    (bird (+ beg .99) .01 4200 100 .17 bairdend main-amp)
    (bird (+ beg 1.03) .01 4400 100 .17 bairdend main-amp)
    (bird (+ beg 1.07) .01 4200 100 .16 bairdend main-amp)
    (bird (+ beg 1.11) .01 4400 100 .16 bairdend main-amp)
    (bird (+ beg 1.15) .01 4200 100 .15 bairdend main-amp)
    (bird (+ beg 1.19) .01 4400 100 .15 bairdend main-amp)
    (bird (+ beg 1.23) .01 4200 100 .14 bairdend main-amp)
    (bird (+ beg 1.27) .01 4400 100 .14 bairdend main-amp)
    (bird (+ beg 1.31) .01 4200 100 .13 bairdend main-amp)
    (bird (+ beg 1.35) .01 4400 100 .13 bairdend main-amp)
    (bird (+ beg 1.39) .01 4200 100 .12 bairdend main-amp)
    (bird (+ beg 1.43) .01 4400 100 .12 bairdend main-amp)
    (bird (+ beg 1.47) .01 4200 100 .11 bairdend main-amp)
    (bird (+ beg 1.51) .01 4400 100 .11 bairdend main-amp)
    (bird (+ beg 1.55) .01 4200 100 .10 bairdend main-amp)
    (bird (+ beg 1.59) .01 4400 100 .10 bairdend main-amp)
    (bird (+ beg 1.63) .01 4200 100 .09 bairdend main-amp)
    (bird (+ beg 1.67) .01 4400 100 .09 bairdend main-amp)
    (bird (+ beg 1.71) .01 4200 100 .08 bairdend main-amp)
    (bird (+ beg 1.75) .01 4400 100 .08 bairdend main-amp)
    (bird (+ beg 1.79) .01 4200 100 .07 bairdend main-amp)
    (bird (+ beg 1.83) .01 4400 100 .07 bairdend main-amp)
    (bird (+ beg 1.87) .01 4200 100 .06 bairdend main-amp)
    (bird (+ beg 1.92) .01 4400 100 .06 bairdend main-amp)
    (bird (+ beg 1.97) .01 4200 100 .05 bairdend main-amp)))


(define (b-kentucky-warbler beg)
  "(kentucky-warbler beg) produces a kentucky warbler call at time 'beg'"
  (let ((kenstart '(.00 .30 .50 1.00 1.00 .0))
	(kendwn '(.00 .90 .10 1.00 1.00 .0))
	(kenup '(.00 .00 1.00 1.0))
	(kentrill '(.00 1.00 .25 .00 .50 .00 .75 1.00 1.00 .0)))
    (set! beg (- beg .6))
    (bigbird (+ beg .6) .02 3800 200 .05 kenstart main-amp '(1 1 2 .03))
    (bigbird (+ beg .65) .03 4300 200 .15 kenup main-amp '(1 1 2 .1))
    (bigbird (+ beg .73) .02 3200 100 .1 kendwn main-amp '(1 1 2 .1))
    
    (bigbird (+ beg .75) .05 3000 800 .15 kenstart main-amp '(1 1 2 .01))
    (bigbird (+ beg .82) .06 3100 1200 .1 kendwn main-amp '(1 1 2 .01))
    (bigbird (+ beg .90) .06 3200 1200 .1 kendwn main-amp '(1 1 2 .01))
    (bigbird (+ beg .98) .05 4600 100 .2 kentrill main-amp '(1 1 2 .1))
    
    (bigbird (+ beg 1.10) .05 2900 800 .15 kenstart main-amp '(1 1 2 .01))
    (bigbird (+ beg 1.17) .06 3000 1200 .1 kendwn main-amp '(1 1 2 .01))
    (bigbird (+ beg 1.25) .06 3100 1200 .1 kendwn main-amp '(1 1 2 .01))
    (bigbird (+ beg 1.33) .05 4600 100 .2 kentrill main-amp '(1 1 2 .1))
    
    (bigbird (+ beg 1.43) .05 2800 800 .15 kenstart main-amp '(1 1 2 .01))
    (bigbird (+ beg 1.50) .05 2700 1200 .1 kendwn main-amp '(1 1 2 .01))
    (bigbird (+ beg 1.57) .06 2800 1200 .1 kendwn main-amp '(1 1 2 .01))
    (bigbird (+ beg 1.64) .05 4600 100 .2 kentrill main-amp '(1 1 2 .1))
    
    (bigbird (+ beg 1.75) .05 2700 800 .15 kenstart main-amp '(1 1 2 .01))
    (bigbird (+ beg 1.81) .05 2600 1200 .1 kendwn main-amp '(1 1 2 .01))
    (bigbird (+ beg 1.88) .06 2600 1200 .1 kendwn main-amp '(1 1 2 .01))
    (bigbird (+ beg 1.97) .05 4600 100 .2 kentrill main-amp '(1 1 2 .1))
    
    (bigbird (+ beg 2.05) .05 2700 800 .15 kenstart main-amp '(1 1 2 .01))
    (bigbird (+ beg 2.12) .06 2600 1200 .1 kendwn main-amp '(1 1 2 .01))
    (bigbird (+ beg 2.20) .05 4600 100 .2 kentrill main-amp '(1 1 2 .1))
    
    (bigbird (+ beg 2.30) .05 2800 800 .15 kenstart main-amp '(1 1 2 .01))
    (bigbird (+ beg 2.37) .06 2700 1200 .1 kendwn main-amp '(1 1 2 .01))
    (bigbird (+ beg 2.45) .05 4700 100 .25 kentrill main-amp '(1 1 2 .1))))


(define (b-rufous-sided-towhee beg)
  "(rufous-sided-towhee beg) produces a rufous sided towhee call at time 'beg'"
  (let ((towhee-one '(.00 .10 .02 .05 .04 .15 .06 .05 .08 .20 .10 .04 .12 .25 .14 .03 .16 .30 .18 .02 .20 .35 .22 .01 .24 .40 .26 .00 .28 .45 .30 .00 .32 .50 .34 .00 .36 .50 .80 1.00 1.00 .0))
	(towhee-two '(.00 .00 1.00 1.0))
	(towhee-three '(.00 1.00 1.00 .0)))
    (set! beg (- beg .25))
    (bigbird (+ beg .25) .13 1400 1100 .2 towhee-one main-amp '(1 .03 2 1 3 .03))
    (bigbird (+ beg .45) .13 1400 1100 .2 towhee-one main-amp '(1 .03 2 1 3 .03))
    (bigbird (+ beg .60) .13 1400 1100 .2 towhee-one main-amp '(1 .03 2 1 3 .03))
    (bigbird (+ beg .75) .10 1400 1100 .2 towhee-one main-amp '(1 .03 2 1 3 .03))
    
    (bird (+ beg .88) .01 5100 2000 .1 towhee-two main-amp)
    (bird (+ beg .895) .01 5100 1600 .1 towhee-two main-amp)
    (bird (+ beg .91) .01 5100 1000 .1 towhee-two main-amp)
    (bird (+ beg .93) .01 3000 1200 .1 towhee-three main-amp)
    
    (bird (+ beg .945) .01 5100 2000 .09 towhee-two main-amp)
    (bird (+ beg .96) .01 5100 1600 .09 towhee-two main-amp)
    (bird (+ beg .975) .01 5100 1000 .09 towhee-two main-amp)
    (bird (+ beg .995) .01 3000 1200 .09 towhee-three main-amp)
    
    (bird (+ beg 1.01) .01 5100 2000 .1 towhee-two main-amp)
    (bird (+ beg 1.025) .01 5100 1600 .1 towhee-two main-amp)
    (bird (+ beg 1.04) .01 5100 1000 .1 towhee-two main-amp)
    (bird (+ beg 1.06) .01 3000 1200 .1 towhee-three main-amp)
    
    (bird (+ beg 1.075) .01 5100 2000 .09 towhee-two main-amp)
    (bird (+ beg 1.09) .01 5100 1600 .09 towhee-two main-amp)
    (bird (+ beg 1.105) .01 5100 1000 .09 towhee-two main-amp)
    (bird (+ beg 1.125) .01 3000 1200 .09 towhee-three main-amp)
    
    (bird (+ beg 1.14) .01 5100 2000 .08 towhee-two main-amp)
    (bird (+ beg 1.155) .01 5100 1600 .08 towhee-two main-amp)
    (bird (+ beg 1.17) .01 5100 1000 .08 towhee-two main-amp)
    (bird (+ beg 1.19) .01 3000 1200 .08 towhee-three main-amp)
    
    (bird (+ beg 1.205) .01 5100 2000 .08 towhee-two main-amp)
    (bird (+ beg 1.220) .01 5100 1600 .08 towhee-two main-amp)
    (bird (+ beg 1.235) .01 5100 1000 .08 towhee-two main-amp)
    (bird (+ beg 1.255) .01 3000 1200 .08 towhee-three main-amp)
    
    (bird (+ beg 1.27) .01 5100 2000 .07 towhee-two main-amp)
    (bird (+ beg 1.285) .01 5100 1600 .07 towhee-two main-amp)
    (bird (+ beg 1.30) .01 5100 1000 .07 towhee-two main-amp)
    (bird (+ beg 1.32) .01 3000 1200 .07 towhee-three main-amp)
    
    (bird (+ beg 1.335) .01 5100 2000 .06 towhee-two main-amp)
    (bird (+ beg 1.350) .01 5100 1600 .06 towhee-two main-amp)
    (bird (+ beg 1.365) .01 5100 1000 .06 towhee-two main-amp)
    (bird (+ beg 1.385) .01 3000 1200 .06 towhee-three main-amp)
    
    (bird (+ beg 1.400) .01 5100 2000 .05 towhee-two main-amp)
    (bird (+ beg 1.415) .01 5100 1600 .05 towhee-two main-amp)
    (bird (+ beg 1.430) .01 5100 1000 .05 towhee-two main-amp)
    (bird (+ beg 1.45) .01 3000 1200 .05 towhee-three main-amp)
    
    (bird (+ beg 1.465) .01 5100 2000 .03 towhee-two main-amp)
    (bird (+ beg 1.480) .01 5100 1600 .03 towhee-two main-amp)
    (bird (+ beg 1.495) .01 5100 1000 .03 towhee-two main-amp)
    (bird (+ beg 1.515) .01 3000 1200 .03 towhee-three main-amp)))


(define (b-prothonotary-warbler beg)
  "(prothonotary-warbler beg) produces a prothonotary warbler call at time 'beg'"
  (let ((pro-one '(.00 .10 .20 .00 1.00 1.0))
	(pro-two '(.00 .00 1.00 1.0))
	(pro-amp '(.00 .00 .20 1.00 .40 .50 1.00 .0)))
    (set! beg (- beg .76))
    (bird (+ beg .76) .08 3000 3000 .05 pro-one pro-amp)
    (bird (+ beg .85) .05 4000 2500 .06 pro-two bird-amp)
    
    (bird (+ beg 1.02) .09 3000 3000 .10 pro-one pro-amp)
    (bird (+ beg 1.12) .05 4000 2500 .10 pro-two bird-amp)
    
    (bird (+ beg 1.26) .08 3000 3000 .15 pro-one pro-amp)
    (bird (+ beg 1.35) .05 4000 2500 .16 pro-two bird-amp)
    
    (bird (+ beg 1.54) .08 3000 3000 .20 pro-one pro-amp)
    (bird (+ beg 1.63) .05 4000 2500 .19 pro-two bird-amp)
    
    (bird (+ beg 1.80) .08 3000 3000 .20 pro-one pro-amp)
    (bird (+ beg 1.89) .05 4000 2500 .16 pro-two bird-amp)
    
    (bird (+ beg 2.03) .08 3000 3000 .15 pro-one pro-amp)
    (bird (+ beg 2.12) .05 4000 2500 .10 pro-two bird-amp)
    
    (bird (+ beg 2.30) .08 3000 3000 .10 pro-one pro-amp)
    (bird (+ beg 2.39) .05 4000 2500 .06 pro-two bird-amp)))


(define (b-audubons-warbler beg)
  "(audubons-warbler  beg) produces an audubons warbler (yellow-rumped warbler) call at time 'beg'"
  (let (
	;;	(yellow-rumped say the revisionists))
	(w-up '(.00 .00 1.00 1.0))
	(w-down '(.00 1.00 1.00 .0))
	(w-end '(.00 .00 .15 1.00 .45 .90 .50 .00 .55 1.00 .90 .90 1.00 .10 ))
	(w-updown '(.00 .10 .50 1.00 1.00 .0)))
    (set! beg (- beg .75))
    (bird (+ beg .75) .04 2400 200 .05 w-down bird-amp)
    (bird (+ beg .83) .03 3200 200 .1 w-up bird-amp)
    (bird (+ beg .90) .04 2500 300 .15 w-up bird-amp)
    (bird (+ beg .97) .04 2300 600 .15 w-down bird-amp)
    (bird (+ beg 1.02) .03 3500 400 .20 w-up bird-amp)
    (bird (+ beg 1.06) .04 2300 1200 .10 w-up bird-amp)
    (bird (+ beg 1.13) .05 2300 1200 .15 w-down bird-amp)
    (bird (+ beg 1.22) .02 3200 800 .25 w-up bird-amp)
    (bird (+ beg 1.25) .08 2400 600 .20 w-updown bird-amp)
    (bird (+ beg 1.35) .02 2200 400 .10 w-up bird-amp)
    (bird (+ beg 1.38) .07 2400 1400 .15 w-down bird-amp)
    (bird (+ beg 1.47) .03 3000 800 .20 w-up bird-amp)
    (bird (+ beg 1.50) .03 2500 400 .10 w-updown bird-amp)
    (bird (+ beg 1.55) .01 2300 100 .05 w-up bird-amp)
    (bird (+ beg 1.56) .06 2200 1400 .15 w-down bird-amp)
    (bird (+ beg 1.65) .03 3100 800 .10 w-up bird-amp)
    (bird (+ beg 1.70) .07 2800 800 .15 w-updown bird-amp)
    (bird (+ beg 1.79) .06 2400 1000 .10 w-down bird-amp)
    (bird (+ beg 1.86) .14 3100 900 .25 w-end bird-amp)
    (bird (+ beg 2.02) .12 3200 800 .20 w-end bird-amp)))


(define (b-lark-bunting beg)
  "(lark-bunting beg) produces a lark bunting call at time 'beg'"
  (let ((b-down '(.00 1.00 1.00 .0))
	(b-up '(.00 .00 1.00 1.0))
	(b-trill-one '(.00 .00 .06 .80 .12 .00 .18 .85 .24 .05 .36 .90 .42 .10 .48 .95 .54 .20 .60 1.00 .66 .20 .72 1.00 .78 .20 .84 1.00 .90 .20 1.00 1.0))
	(b-trill-two '(.00 .00 .05 .80 .10 .00 .15 .85 .20 .00 .25 .90 .30 .00 .35 .95 .40 .00 .45 1.00 .50 .00 .55 1.00 .60 .00 .65 1.00 .70 .00 .75 1.00 .80 .00 .85 1.00 .90 .00 .95 1.00 1.00 .0)))
    (set! beg (- beg .1))
    (bird (+ beg .1) .03 1800 100 .1 b-up bird-amp)
    (bird (+ beg .2) .12 3700 400 .2 b-up bird-amp)
    
    (bird (+ beg .4) .03 4100 500 .15 b-down bird-amp)
    (bird (+ beg .45) .05 2000 400 .20 b-down bird-amp)
    (bird (+ beg .51) .03 1800 100 .1 b-up bird-amp)
    
    (bird (+ beg .6) .03 4100 500 .15 b-down bird-amp)
    (bird (+ beg .65) .05 2000 400 .20 b-down bird-amp)
    (bird (+ beg .71) .03 1800 100 .1 b-up bird-amp)
    
    (bird (+ beg .8) .03 4100 500 .15 b-down bird-amp)
    (bird (+ beg .85) .05 2000 400 .20 b-down bird-amp)
    (bird (+ beg .91) .03 1800 100 .1 b-up bird-amp)
    
    (bird (+ beg 1.0) .03 4100 500 .15 b-down bird-amp)
    (bird (+ beg 1.05) .05 2000 400 .20 b-down bird-amp)
    (bird (+ beg 1.11) .03 1800 100 .1 b-up bird-amp)
    
    (bird (+ beg 1.2) .03 4100 500 .15 b-down bird-amp)
    (bird (+ beg 1.25) .05 2000 400 .20 b-down bird-amp)
    (bird (+ beg 1.31) .03 1800 100 .1 b-up bird-amp)
    
    (bird (+ beg 1.4) .03 4100 500 .15 b-down bird-amp)
    (bird (+ beg 1.45) .05 2000 400 .20 b-down bird-amp)
    (bird (+ beg 1.51) .03 1800 100 .1 b-up bird-amp)
    
    (bird (+ beg 1.6) .03 4100 500 .15 b-down bird-amp)
    (bird (+ beg 1.65) .05 2000 400 .20 b-down bird-amp)
    (bird (+ beg 1.71) .03 1800 100 .1 b-up bird-amp)
    
    (bird (+ beg 1.77) .23 6000 600 .15 b-trill-one bird-amp)
    (bird (+ beg 2.005) .28 6000 600 .15 b-trill-two bird-amp)))


(define (b-eastern-bluebird beg)
  "(eastern-bluebird beg) produces an eastern bluebird call at time 'beg'"
  (let ((blue-one '(.00 .00 1.00 1.0))
	(blue-two '(.00 1.00 1.00 .0))
	(blue-three '(.00 .60 .10 1.00 .20 .00 .25 1.00 .30 .00 .35 1.00 .40 .00 .45 1.00 .50 .00 .75 1.00 1.00 .0))
	(blue-four '(.00 .00 .50 1.00 1.00 .0))
	(blue-five '(.00 .50 .10 1.00 .20 .00 .35 1.00 .50 .00 .65 1.00 .80 .00 .95 1.00 1.00 .50 )))
    (set! beg (- beg .75))
    (bird (+ beg .75) .02 2000 1600 .1 blue-one bird-amp)
    (bird (+ beg .80) .02 2000 1600 .1 blue-one bird-amp)
    (bird (+ beg .86) .02 2000 1600 .1 blue-one bird-amp)
    (bird (+ beg 1.00) .13 2000 1400 .2 blue-two bird-amp)
    (bird (+ beg 1.20) .24 2000 800 .2 blue-three bird-amp)
    (bird (+ beg 1.68) .03 2200 400 .1 blue-one bird-amp)
    (bird (+ beg 1.72) .10 1950 100 .15 blue-four bird-amp)
    (bird (+ beg 1.96) .15 2000 600 .20 blue-five bird-amp)))


(define (b-chuck-wills-widow beg)
  "(chuck-wills-widow beg) produces a chuck wills widow call at time 'beg'"
  (let ((wid-down '(.00 1.00 1.00 .0))
	(wid-one '(.00 .00 .10 .10 .25 1.00 .50 .30 .80 .70 1.00 .0))
	(wid-two '(.00 .20 .30 1.00 .50 .30 .60 .70 .90 .10 1.00 .0)))
    (set! beg (- beg .05))
    (bird (+ beg .05) .03 1000 800 .1 wid-down bird-amp)
    (bird (+ beg .32) .20 1000 1000 .2 wid-one bird-amp)
    (bird (+ beg .56) .29 900 1100 .2 wid-two bird-amp)))


(define (b-blue-gray-gnatcatcher beg)
  "(blue-gray-gnatcatcher beg) produces a blue gray gnatcatcher call at time 'beg'"
  (let ((gskw1 '(.00 .00 .15 1.00 .75 .80 .90 1.00 1.00 .70 ))
	(gskw2 '(.00 .00 .25 1.00 .75 .70 1.00 .0)))
    (set! beg (- beg .5))
    (bigbird (+ beg .5) .20 4000 1000 .2 gskw1 bird-amp '(1 .4 2 1 3 .1))
    (bigbird (+ beg .8) .13 4000 800 .2 gskw2 bird-amp '(1 .4 2 1 3 .2))
    
    (bigbird (+ beg 1.4) .25 4000 800 .2 gskw2 bird-amp '(1 .4 2 1 3 .3))
    (bigbird (+ beg 1.80) .17 4000 900 .2 gskw1 bird-amp '(1 .4 2 1 3 .3))
    (bigbird (+ beg 2.00) .17 4000 700 .2 gskw1 bird-amp '(1 .4 2 1 3 .3))
    (bigbird (+ beg 2.20) .17 4000 800 .2 gskw2 bird-amp '(1 .4 2 1 3 .3))))


(define (b-black-throated-sparrow beg)
  "(black-throated-sparrow beg) produces a black throated sparrow call at time 'beg'"
  (let ((black-up '(.00 .00 1.00 1.0))
	(black-down '(.00 1.00 1.00 .0))
	(black-down-amp '(.00 .00 .75 1.00 1.00 .0))
	(black-trill '(.00 .00 .03 .70 .06 .00 .09 .75 .12 .00 .15 .80 .18 .05 .21 .85 .24 .10 .27 .90 .30 .10 .33 1.00 .36 .10 .39 1.00 .42 .10 .45 1.00 .48 .10 .51 1.00 .54 .10 .57 1.00 .60 .10 .63 1.00 .66 .10 .69 1.00 .72 .10 .75 1.00 .78 .10 .81 1.00 .84 .10 .87 1.00 .90 .00 .93 .95 .96 .00 1.00 .90 ))
	(black-up-down '(.00 .00 .50 1.00 1.00 .20 ))
	(black-amp '(.00 .00 .50 1.00 1.00 .0)))
    (set! beg (- beg .8))
    (bird (+ beg .8) .02 2200 1000 .1 black-down bird-amp)
    (bird (+ beg .83) .01 3000 200 .05 black-up bird-amp)
    (bird (+ beg .96) .02 5800 500 .05 black-up bird-amp)
    (bird (+ beg 1.00) .02 4000 200 .05 black-up bird-amp)
    (bird (+ beg 1.04) .10 2100 1700 .15 black-down black-down-amp)
    (bird (+ beg 1.15) .05 5700 400 .25 black-up bird-amp)
    (bird (+ beg 1.25) .25 2000 900 .2 black-trill bird-amp)
    (bird (+ beg 1.52) .05 5600 400 .15 black-up-down bird-amp)
    
    (bird (+ beg 1.6) .04 3900 1100 .15 black-up bird-amp)
    (bird (+ beg 1.66) .01 1900 100 .10 black-up black-amp)
    
    (bird (+ beg 1.69) .01 3600 300 .10 black-up black-amp)
    (bird (+ beg 1.71) .03 3900 1000 .15 black-up black-amp)
    (bird (+ beg 1.74) .02 5000 100 .20 black-up black-amp)
    (bird (+ beg 1.76) .01 1900 100 .10 black-up black-amp)
    
    (bird (+ beg 1.78) .01 3600 300 .10 black-up black-amp)
    (bird (+ beg 1.80) .03 3900 1000 .15 black-up black-amp)
    (bird (+ beg 1.83) .02 5000 100 .20 black-up black-amp)
    (bird (+ beg 1.85) .01 1900 100 .10 black-up black-amp)
    
    (bird (+ beg 1.87) .01 3600 300 .10 black-up black-amp)
    (bird (+ beg 1.89) .03 3900 1000 .15 black-up black-amp)
    (bird (+ beg 1.92) .02 5000 100 .20 black-up black-amp)
    (bird (+ beg 1.94) .01 1900 100 .10 black-up black-amp)
    
    (bird (+ beg 1.96) .01 3600 300 .10 black-up black-amp)
    (bird (+ beg 1.98) .03 3900 1000 .15 black-up black-amp)
    (bird (+ beg 2.01) .02 5000 100 .20 black-up black-amp)
    (bird (+ beg 2.03) .01 1900 100 .10 black-up black-amp)
    
    (bird (+ beg 2.05) .01 3600 300 .10 black-up black-amp)
    (bird (+ beg 2.07) .03 3900 1000 .15 black-up black-amp)
    (bird (+ beg 2.10) .02 5000 100 .20 black-up black-amp)
    (bird (+ beg 2.13) .01 1900 100 .10 black-up black-amp)
    
    (bird (+ beg 2.16) .03 3800 300 .1 black-up bird-amp)))


(define (b-black-chinned-sparrow beg)
  "(black-chinned-sparrow beg) produces a black chinned sparrow call at time 'beg'"
  (let ((chin-up '(.00 .00 1.00 1.0))
	(chin-up2 '(.00 .00 .30 .20 1.00 1.0)))
    (set! beg (- beg .6))
    (bird (+ beg .6) .2 4200 100 .1 chin-up bird-amp)
    (bird (+ beg 1.0) .09 3800 2000 .1 chin-up2 bird-amp)
    (bird (+ beg 1.25) .08 3900 1700 .12 chin-up2 bird-amp)
    (bird (+ beg 1.40) .08 3600 2300 .13 chin-up bird-amp)
    (bird (+ beg 1.50) .11 3100 2800 .14 chin-up bird-amp)
    (bird (+ beg 1.65) .07 2900 2700 .15 chin-up bird-amp)
    (bird (+ beg 1.74) .07 2900 2700 .15 chin-up bird-amp)
    (bird (+ beg 1.82) .07 3000 2300 .13 chin-up bird-amp)
    (bird (+ beg 1.89) .07 3200 2000 .10 chin-up bird-amp)
    (bird (+ beg 1.97) .05 3200 1500 .10 chin-up bird-amp)
    (bird (+ beg 2.04) .04 3400 1000 .07 chin-up bird-amp)
    (bird (+ beg 2.10) .03 3600 700 .05 chin-up bird-amp)
    (bird (+ beg 2.15) .03 3800 300 .05 chin-up bird-amp)
    (bird (+ beg 2.19) .02 3900 100 .03 chin-up bird-amp)
    (bird (+ beg 2.22) .01 3900 100 .01 chin-up bird-amp)
    (bird (+ beg 2.24) .01 3900 100 .01 chin-up bird-amp)))


(define (various-gull-cries-from-end-of-colony-5 beg)
  "(various-gull-cries-from-end-of-colony-5 beg) produces a various gull cries at time 'beg'"
  (let ((gullstart '(0 0 10 1 20 .5000 40 .6000 60 .5000 100 0 ))
	(gullmiddle '(0 0 10 1 30 .5000 80 .5000 100 0 ))
	(gullend '(0 0 5 1 10 .5000 90 .4000 100 0 )))
    (set! beg (- beg .25))
    (bigbird (+ beg .250) .80  1180  1180  .08 gullend  bird-amp
	     '(1  .1  2  1  3  .1  4  .01  5
		  .09  6  .01  7  .01))
    (bigbird (+ beg 1.500) .90  1180  1180  .07  gullend  bird-amp
	     '(1  .1  2  1  3  .1  4  .01  5
		  .09  6  .01  7  .01))
    (bigbird (+ beg 2.750) 1.00  1050  1050  .08  gullend  bird-amp
	     '(1  .1  2  1  3  .1  4  .01  5
		  .09  6  .01  7  .01))
    (bigbird (+ beg 4.800) .05  1180 1180  .06  gullstart  bird-amp
	     '(1  .1  2  1  3  .1  4  .01  5
		  .09  6  .01  7  .01))
    (bigbird (+ beg 4.950) .10  1180 1180  .08  gullstart  bird-amp
	     '(1  .1  2  1  3  .1  4  .01  5
		  .09  6  .01  7  .01))
    (bigbird (+ beg 5.150) .10  1180 1180  .09  gullstart  bird-amp
	     '(1  .1  2  1  3  .1  4  .01  5
		  .09  6  .01  7  .01))
    (bigbird (+ beg 5.350) .10  1180 1180  .1  gullmiddle  bird-amp
	     '(1  .1  2  1  3  .1  4  .01  5
		  .09  6  .01  7  .01))
    (bigbird (+ beg 5.450) .40  1050  1050  .1  gullend  bird-amp
	     '(1  .1  2  1  3  .1  4  .01  5
		  .09  6  .01  7  .01))
    (bigbird (+ beg 6.250) .80  1050  1050  .1  gullend  bird-amp
	     '(1  .1  2  1  3  .1  4  .01  5
		  .09  6  .01  7  .01))
    (bigbird (+ beg 7.450) 1.80  1050  1050  .1 gullend  bird-amp
	     '(1  .1  2  1  3  .1  4  .01  5
		  .09  6  .01  7  .01))))


(define (make-birds)
  "(make-birds) calls all the birds in bird.scm"
  (with-sound (:clipped #f)
    (b-orchard-oriole 0)
    (b-cassins-kingbird 3)
    (b-chipping-sparrow 6)
    (b-bobwhite 9)
    (b-western-meadowlark 12)
    (b-scissor-tailed-flycatcher 15)
    (b-great-horned-owl 18)
    (b-black-throated-gray-warbler 21)
    (b-yellow-warbler 24)
    (b-black-necked-stilt 27)
    (b-chestnut-sided-warbler 30)
    (b-grasshopper-sparrow 33)
    (b-swamp-sparrow 36)
    (b-golden-crowned-sparrow 39)
    (b-indigo-bunting 42)
    (b-hooded-warbler 45)
    (b-american-widgeon 48)
    (b-louisiana-waterthrush 51)
    (b-robin 54)
    (b-solitary-vireo 57)
    (b-pigeon-hawk 61)
    (b-cerulean-warbler 64)
    (b-nashville-warbler 67)
    (b-eastern-phoebe 70)
    (b-painted-bunting 73)
    (b-western-flycatcher 76)
    (b-bachmans-sparrow 79)
    (b-cedar-waxwing 82)
    (b-bairds-sparrow 85)
    (b-kentucky-warbler 88)
    (b-rufous-sided-towhee 91)
    (b-prothonotary-warbler 94)
    (b-audubons-warbler 97)
    (b-lark-bunting 100)
    (b-eastern-bluebird 103)
    (b-chuck-wills-widow 106)
    (b-blue-gray-gnatcatcher 109)
    (b-black-throated-sparrow 112)
    (b-black-chinned-sparrow 115)
    (various-gull-cries-from-end-of-colony-5 118)))

