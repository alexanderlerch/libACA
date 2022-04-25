;;; Examples of the fm violin (see v.ins) 
;;; This file semi-automatically translated from a sambox note list ca 1983.
;;;
;; Translator/Author: Michael Scholz <mi-scholz@users.sourceforge.net>
;; Created: Tue Jun 24 19:05:06 CEST 2003
;; Changed: Sat Jul 28 04:16:19 CEST 2012

;; Type (short-example)
;; or   (long-example)

(define *clm-c-version* #t)

(if (not (provided? 'sndlib))
    (let ((hsndlib (dlopen "libsndlib.so")))
      (if (string? hsndlib)
	  (snd-error (format #f "script needs the sndlib module: ~A" hsndlib))
	  (dlinit hsndlib "Init_sndlib"))))
(if *clm-c-version*
  (if (not (provided? 'sndins))
      (let ((hsndins (dlopen "libsndins.so")))
        (if (string? hsndins)
	    (snd-error (format #f "script needs the sndins module: ~A" hsndins))
	    (dlinit hsndins "Init_sndins"))))
  (begin
    (load "v.scm")
    (load "freeverb.scm")))

(if (not (provided? 'snd-ws.scm)) (load "ws.scm"))

(define *clm-file-name* "test-ins-s.snd")
(define *clm-play* #t)
(define *clm-statistics* #t)
(define *clm-verbose* #t)
(define *clm-srate* 44100)
(define *clm-channels* 2)
(define *clm-reverb-channels* 2)
(define *clm-delete-reverb* #t)

(define (main args)
  (if (> (length args) 1)
      (short-example)
      (long-example)))

;;show progress of long example
(define show #t)
(define start-time 0)
(define counter 0)

(define (snd-msg frm . args)
  (snd-print (apply format (append (list #f frm) args))))

(define (current-score-time time)
  (set! counter (+ counter 1))
  (if show
      (snd-msg ";; ~2D: score ~3D utime ~7,3F~%"
	       counter
	       (- time 24)
	       (/ (- (get-internal-real-time) start-time) 100.0))))

(definstrument (violin-new beg dur freq amp fm1-rat fm2-rat fm3-rat fm-index 
                 reverb-amount amp-env)
  (do ((i 0 (1+ i)))
      ((= 5 i))
    (fm-violin (+ beg (* i .05 (random 1.0)))
	       (+ dur (* i .1 (random 1.0)))
	       (if (> freq 400) (* freq (+ .99 (* .02 (random 1.0))))
		   (if (> freq 200) (* freq (+ .995 (* .01 (random 1.0))))
		       (* freq (+ .999 (* .002 (random 1.0))))))
	       amp
	       :reverb-amount (or reverb-amount .2)
	       :fm-index (* fm-index (+ .75 (random 1.0)))
	       :amp-env amp-env)))

(define (short-example)
  (with-sound (:reverb nrev :reverb-data '(:lp-coeff 0.6))
	      (violin-new 0 8.53 993.323 .03 :fm-index .75
                :reverb-amount .20 :amp-env (list 0 0 221.00 1 240.00 0))
	      (violin-new 5 4.53 (* 5/6 993.323) .02 :fm-index .55
		:reverb-amount .20 :amp-env (list 0 0 221.00 1 240.00 0))))

(define pna '(0 0 1 1 10 .6000 25 .3 100 0))
(define ind2 '(0 1 25 .4 75 .6000 100 0))
(define high_att_ind '(0 1 25 .2000 75 .4 100 0))
(define no_att_ind '(0 .6000 75 .6000 100 0))
(define no_dec_ind '(0 1 25 .4 75 .6000 100 .6000))
(define no_att_or_dec_ind '(0 .6000 100 .6000))
(define amp '(0 0 25 1 60 .7000 75 1 100 0))
(define ramp '(0 0 100 1))
(define fast_up '(0 0 25 1 100 1))
(define slow_up '(0 0 25 0 100 1))
(define skwfrq '(0 -1 5 .2500 10 0 100 .1))
(define circle '(0 90 0 90 0 90 0 90))
(define oldpizzf '(0 0 1 1 5 .6000 10 .3 25 .1 100 0))
(define newpizzf '(0 0 1 1 5 .6000 10 .3 25 .1 99 .0200 100 0))
(define pizzf '(0 0 1 1 5 .6000 10 .3 25 .1 100 0))
(define legatof '(0 0 30 1 90 1 100 0))
(define marcatof '(0 0 3 1 10 .8000 95 1 100 0))
(define one '(0 1 100 1))
(define mod_up '(0 0 25 0 75 1 100 1))
(define mod_down '(0 1 25 1 75 0 100 0))
(define one_to_zero '(0 1 75 1 100 0))
(define zero_to_one '(0 0 75 0 100 1))
(define down_flat '(0 1 25 0 75 .0500 100 0))
(define down_down '(0 1 25 0 75 .0500 100 -1))
(define down_up '(0 1 25 0 75 .0500 100 1))
(define flat_down '(0 -.1000 10 .1 25 0 75 .0500 100 -1))
(define flat_up '(0 -.1000 10 .1 25 0 75 0 100 1))
(define up_flat '(0 -1 25 .0500 75 0 100 0))
(define up_up '(0 -1 25 .0500 75 0 100 1))
(define up_down '(0 -1 25 .0500 75 0 100 -1))
(define swellf '(0 0 25 .8000 50 1 75 .8000 100 0))
(define fpf '(0 0 25 1 50 .3 75 .3 100 0))
(define indswell '(1 1 25 .4 75 1 100 0))
(define pyr '(0 1 25 .1 95 .1 100 0))
(define fbell '(0 1 2 1.1000 25 .7500 75 .5 100 .2000))
(define lowbell '(0 1 5 1.2500 25 .8000 75 .5 100 .2000))
(define abell '(0 0 .1 1 10 .6000 25 .3 50 .1500 90 .1 100 0))
(define dwnup '(0 1 10 .4 20 1 35 .3 45 .8000 60 .2000 80 .6000 100 0))
(define up50down '(0 0 50 1 100 0))
(define slowupfastdown '(0 0 25 1 97 1 100 0))
(define slowup '(0 0 50 .1 95 1 100 0))
(define indtoone '(0 1 25 .4 100 .6500))
(define tap '(0 0 1 1 99 1 100 0))
(define metalamp '(0 0 .5 1 5 1 10 .5 15 .2500 35 .1 100 0))
(define whoosh '(0 0 75 .1 90 .3 97 .6000 100 1))
(define mamp '(0 0 50 1 100 0))
(define n_amp '(0 0 65 1 100 0))
(define updown '(0 0 15 1 100 0))
(define indfunc '(0 1 20 0 100 0))
(define indfunc2 '(0 1 90 1 100 0))
(define ampfunc '(0 1 6 1 10 .5 20 .363 30 .27 40 .2 50 .12
                  60 .08 70 .04 100 0))
(define ampfunc1 '(0 0 1 1 3 1 10 .5 30 .2000 60 .0500 100 0))
(define z1amp '(0 0 20 .5 40 .1 60 .2000 80 1 100 0))
(define z2amp '(0 0 20 1 60 .1 75 .3 100 0))
(define newf '(0 0 .5 1 5 1 10 .5 15 .2500 35 .1 100 0))
(define yup '(0 0 1 1 100 0))

(define fm-violin-random-vibrato-rate 12.0)
(define fm-violin-random-vibrato-amplitude .01)
(define fm-violin-gliss-env skwfrq)
(define fm-violin-glissando-amount .01)
(define fm-violin-noise-amount .002)
(define fm-violin-fm-index 1.0)
(define fm-violin-amp-env '(0 0  25 1  75 1  100 0))
(define fm-violin-periodic-vibrato-rate 5.0)
(define fm-violin-random-vibrato-rate 16.0)
(define fm-violin-periodic-vibrato-amplitude 0.0025)
(define fm-violin-random-vibrato-amplitude 0.005)
(define fm-violin-noise-amount 0.0)
(define fm-violin-noise-freq 1000.0)
(define fm-violin-ind-noise-amount 0.0)
(define fm-violin-ind-noise-freq 10.0)
(define fm-violin-amp-noise-amount 0.0)
(define fm-violin-amp-noise-freq 20.0)
(define fm-violin-gliss-env '(0 0  100 0))
(define fm-violin-glissando-amount 0.0)
(define fm-violin-fm1-env '(0 1  25 .4  75 .6  100 0))
(define fm-violin-fm2-env fm-violin-fm1-env)
(define fm-violin-fm3-env fm-violin-fm1-env)
(define fm-violin-fm1-rat 1.0)
(define fm-violin-fm2-rat 3.0)
(define fm-violin-fm3-rat 4.0)
(define fm-violin-base 0.0)
(define fm-violin-reverb-amount 0.01)
(define fm-violin-index-type 1)
(define fm-violin-index1 #f)
(define fm-violin-index2 #f)
(define fm-violin-index3 #f)

(define (restore-fm-violin-defaults)
  (set! fm-violin-fm-index 1.0)
  (set! fm-violin-amp-env '(0 0  25 1  75 1  100 0))
  (set! fm-violin-periodic-vibrato-rate 5.0)
  (set! fm-violin-random-vibrato-rate 16.0)
  (set! fm-violin-periodic-vibrato-amplitude 0.0025)
  (set! fm-violin-random-vibrato-amplitude 0.005)
  (set! fm-violin-noise-amount 0.0)
  (set! fm-violin-noise-freq 1000.0)
  (set! fm-violin-ind-noise-amount 0.0)
  (set! fm-violin-ind-noise-freq 10.0)
  (set! fm-violin-gliss-env '(0 0  100 0))
  (set! fm-violin-glissando-amount 0.0)
  (set! fm-violin-fm1-env '(0 1  25 .4  75 .6  100 0))
  (set! fm-violin-fm2-env fm-violin-fm1-env)
  (set! fm-violin-fm3-env fm-violin-fm1-env)
  (set! fm-violin-fm1-rat 1.0)
  (set! fm-violin-fm2-rat 3.0)
  (set! fm-violin-fm3-rat 4.0)
  (set! fm-violin-base 0.0)
  (set! fm-violin-reverb-amount 0.01)
  (set! fm-violin-index-type 1)
  (set! fm-violin-index1 #f)
  (set! fm-violin-index2 #f)
  (set! fm-violin-index3 #f))

(definstrument (old-fm-violin startime dur frequency amplitude
  (fm-index                   fm-violin-fm-index)
  (amp-env                    fm-violin-amp-env)
  (periodic-vibrato-rate      fm-violin-periodic-vibrato-rate)
  (random-vibrato-rate        fm-violin-random-vibrato-rate)
  (periodic-vibrato-amplitude fm-violin-periodic-vibrato-amplitude)
  (random-vibrato-amplitude   fm-violin-random-vibrato-amplitude)
  (noise-amount               fm-violin-noise-amount)
  (noise-freq                 fm-violin-noise-freq)
  (ind-noise-freq             fm-violin-ind-noise-freq)
  (ind-noise-amount           fm-violin-ind-noise-amount)
  (amp-noise-freq             fm-violin-amp-noise-freq)
  (amp-noise-amount           fm-violin-amp-noise-amount)
  (gliss-env                  fm-violin-gliss-env)
  (glissando-amount           fm-violin-glissando-amount)
  (fm1-env                    fm-violin-fm1-env)
  (fm2-env                    fm-violin-fm2-env)
  (fm3-env                    fm-violin-fm3-env)
  (fm1-rat                    fm-violin-fm1-rat)
  (fm2-rat                    fm-violin-fm2-rat)
  (fm3-rat                    fm-violin-fm3-rat)
  (fm1-index                  fm-violin-index1)
  (fm2-index                  fm-violin-index2)
  (fm3-index                  fm-violin-index3)
  (degree                     0.0)
  (distance                   1.0)
  (reverb-amount              fm-violin-reverb-amount)
  (base                       fm-violin-base)
  (index-type                 fm-violin-index-type))
  (if *clm-c-version*
    (fm-violin startime dur frequency amplitude
      :fm-index fm-index
      :amp-env amp-env
      :periodic-vibrato-rate periodic-vibrato-rate
      :random-vibrato-rate random-vibrato-rate
      :periodic-vibrato-amplitude periodic-vibrato-amplitude
      :random-vibrato-amplitude random-vibrato-amplitude
      :noise-amount noise-amount
      :noise-freq noise-freq
      :ind-noise-freq ind-noise-freq
      :ind-noise-amount ind-noise-amount
      :amp-noise-freq amp-noise-freq
      :amp-noise-amount amp-noise-amount
      :gliss-env gliss-env
      :glissando-amount glissando-amount
      :fm1-env fm1-env
      :fm2-env fm2-env
      :fm3-env fm3-env
      :fm1-rat fm1-rat
      :fm2-rat fm2-rat
      :fm3-rat fm3-rat
      :fm1-index fm1-index
      :fm2-index fm2-index
      :fm3-index fm3-index
      :degree degree
      :distance distance
      :reverb-amount reverb-amount
      :base base
      :index-type index-type)
    (fm-violin startime dur frequency amplitude
      :fm-index fm-index
      :amp-env amp-env
      :periodic-vibrato-rate periodic-vibrato-rate
      :random-vibrato-rate random-vibrato-rate
      :periodic-vibrato-amplitude periodic-vibrato-amplitude
      :random-vibrato-amplitude random-vibrato-amplitude
      :noise-amount noise-amount
      :noise-freq noise-freq
      :ind-noise-freq ind-noise-freq
      :ind-noise-amount ind-noise-amount
      :amp-noise-freq amp-noise-freq
      :amp-noise-amount amp-noise-amount
      :gliss-env gliss-env
      :glissando-amount glissando-amount
      :fm1-env fm1-env
      :fm2-env fm2-env
      :fm3-env fm3-env
      :fm1-rat fm1-rat
      :fm2-rat fm2-rat
      :fm3-rat fm3-rat
      :fm1-index fm1-index
      :fm2-index fm2-index
      :fm3-index fm3-index
      :degree degree
      :distance distance
      :reverb-amount reverb-amount
      :base base)))

(define fullbeg 24.0)

(define-macro (vln_one_sin start-time duration frequency amplitude . rest)
  `(old-fm-violin ,(- start-time fullbeg) 
		  ,duration 
		  ,frequency 
		  ,(* amplitude .125) 
		  :degree (random 90.0) 
		  :noise-amount 0.0
		  ,@rest))

(define-macro (vln_one_sin_ran start-time duration frequency amplitude . rest)
  `(old-fm-violin ,(- start-time fullbeg) 
		  ,duration 
		  ,frequency 
		  ,(* amplitude .125) 
		  :degree (random 90.0) 
		  ,@rest))

(define-macro (vln_one_sin_exp start dur freq amp . rest) 
  `(vln_one_sin ,start ,dur ,freq ,amp :base 0.03125 ,@rest))

(define-macro (violin start dur freq amp . rest) 
  `(vln_one_sin ,start ,dur ,freq ,amp ,@rest))

(define-macro (cel_one_sum start-time duration frequency amplitude . rest)
  `(old-fm-violin ,(- start-time fullbeg) 
		  ,duration 
		  ,frequency 
		  ,(* amplitude .125) 
		  :degree (random 90.0) 
		  :index-type 0
		  ,@rest))

(define (long-example)
  (set! start-time (get-internal-real-time))
  (with-sound (:reverb freeverb :reverb-data '(:room-decay 0.8))
	      (set! fm-violin-glissando-amount 0.0)
	      (set! fm-violin-reverb-amount .1)
	      (set! fm-violin-amp-env metalamp)
	      (set! fm-violin-fm1-rat 6.718)
	      (set! fm-violin-fm2-rat 4.414)
	      (set! fm-violin-fm3-rat 5.141)

	      (current-score-time 24)
	      (vln_one_sin 24 1.6000 164.5868 .1600 :fm-index 2.1087)
	      (vln_one_sin 24.0003 4 164.5868 .2600 :fm-index 1.5488)
	      (vln_one_sin 24.0005 1.2000 125.9513 .2600 :fm-index 2.2999)
	      (vln_one_sin 24.0005 2.8000 125.9513 .1600 :fm-index 1.6818)
	      (vln_one_sin 24.0013 4 24.4994 .3 :fm-index 2.4557)
	      (vln_one_sin 24.0033 3 24.4994 .3 :fm-index 1.9387)
	      (vln_one_sin 24.0035 2.8000 24.4994 .2600 :fm-index 2.3828)
	      (vln_one_sin 24.0040 .8000 24.4994 .1600 :fm-index 1.7348)
	      (vln_one_sin 24.0043 4 24.4994 .3 :fm-index 2.0886)

	      (set! fm-violin-fm1-rat 2.718)
	      (set! fm-violin-fm2-rat 4.414)
	      (set! fm-violin-fm3-rat 3.141)

	      (current-score-time 30)
	      (vln_one_sin 30.0003 1.2000 88.8854 .1600 :fm-index 2.0711)
	      (vln_one_sin 30.0003 4 88.8854 .2600 :fm-index 2.0225)
	      (vln_one_sin 30.0005 1.2000 102.7186 .2600 :fm-index 1.9300)
	      (vln_one_sin 30.0010 1.2000 32.7025 .3600 :fm-index 1.9269)
	      (vln_one_sin 30.0015 2.8000 32.7025 .2600 :fm-index 2.2153)
	      (vln_one_sin 30.0023 2 32.7025 .2600 :fm-index 2.1968)
	      (vln_one_sin 30.0023 4 32.7025 .3600 :fm-index 1.6091)
	      (vln_one_sin 30.0025 2 32.7025 .2600 :fm-index 2.1766)
	      (vln_one_sin 30.0035 1.2000 32.7025 .1600 :fm-index 1.5157)
	      (vln_one_sin 30.0040 .8000 32.7025 .1600 :fm-index 1.8092)
	      (vln_one_sin 30.0043 2 32.7025 .2600 :fm-index 1.6198)

	      (set! fm-violin-reverb-amount .2)
	      (set! fm-violin-fm3-rat 5.141)	

	      (current-score-time 36)
	      (vln_one_sin 36.0003 1.2000 177.7708 .3 :fm-index 1.9631)
	      (vln_one_sin 36.0003 4 177.7708 .3 :fm-index 1.9647)
	      (vln_one_sin 36.0005 2.8000 336.2471 .1600 :fm-index 2.3977)
	      (vln_one_sin 36.0008 1.2000 336.2471 .2500 :fm-index 2.4103)
	      (vln_one_sin 36.0010 2 65.4050 .3 :fm-index 1.8419)
	      (vln_one_sin 36.0033 2 65.4050 .3 :fm-index 2.4540)
	      (vln_one_sin 36.0043 4 65.4050 .1600 :fm-index 2.2909)

	      (set! fm-violin-reverb-amount .1)

	      (current-score-time 42)
	      (vln_one_sin 42.0003 1.2000 11.1107 .3 :fm-index 1.8715)
	      (vln_one_sin 42.0003 4 11.1107 .3 :fm-index 2.4590)
	      (vln_one_sin 42.0005 2.8000 21.0154 .1600 :fm-index 2.3802)
	      (vln_one_sin 42.0008 1.2000 21.0154 .2500 :fm-index 1.7564)
	      (vln_one_sin 42.0010 2 4.0878 .3 :fm-index 2.2529)
	      (vln_one_sin 42.0033 2 4.0878 .3 :fm-index 1.9693)
	      (vln_one_sin 42.0043 4 4.0878 .1600 :fm-index 2.2534)

	      (restore-fm-violin-defaults)
	      (set! fm-violin-noise-amount .1)

	      (current-score-time 48)
	      (vln_one_sin_ran 48 5.4000 116.5400 .2500
                :fm-index 2.2822 :reverb-amount .0280
                :amp-env 
		'(0 0 .0556 1 4.0937 .6000 9.1414 .3 24.2845 .1 100.0 0))
	      (vln_one_sin_ran 48.0100 5.4000 43.6538 .2500
		:fm-index 2.0867 :reverb-amount .0202
		:amp-env 
		'(0 0 .0556 1 4.0937 .6000 9.1414 .3 24.2845 .1 100.0 0))
	      (vln_one_sin_ran 48.0200 5.4000 130.8100 .2500
		:fm-index 1.9652 :reverb-amount .0270
		:amp-env 
		'(0 0 .0556 1 4.0937 .6000 9.1414 .3 24.2845 .1 100.0 0))
	      (vln_one_sin_ran 48.0250 5.4000 87.3075 .2500
		:fm-index 2.1524 :reverb-amount .0260
		:amp-env 
		'(0 0 .0556 1 4.0937 .6000 9.1414 .3 24.2845 .1 100.0 0))
	      (vln_one_sin_ran 48.0300 4.5000 261.6200 .1500
		:fm-index 2.1384 :reverb-amount .0242
		:amp-env 
		'(0 0 .0667 1 4.1044 .6000 9.1515 .3 24.2929 .1 100 0))
	      (vln_one_sin_ran 48.0300 4.5000 174.6150 .1500
		:fm-index 2.1425 :reverb-amount .0265
		:amp-env 
		'(0 0 .0667 1 4.1044 .6000 9.1515 .3 24.2929 .1 100 0))
	      (vln_one_sin_ran 48.0300 4.5000 130.8100 .1500
		:fm-index 1.9805 :reverb-amount .0201
		:amp-env 
		'(0 0 .0667 1 4.1044 .6000 9.1515 .3 24.2929 .1 100 0))
	      (vln_one_sin_ran 48.0350 4.5000 43.6538 .1500
		:fm-index 2.4876 :reverb-amount .0329
		:amp-env 
		'(0 0 .0667 1 4.1044 .6000 9.1515 .3 24.2929 .1 100 0))
	      (vln_one_sin_ran 48.0400 3.6000 220 .1500
		:fm-index 1.8282 :reverb-amount .0244
		:amp-env 
		'(0 0 .0833 1 4.1204 .6000 9.1667 .3 24.3056 .1 100.0 0)
		:noise-amount .1500)
	      (vln_one_sin_ran 48.0400 3.6000 174.6150 .1500
		:fm-index 2.3479 :reverb-amount .0200
		:amp-env 
		'(0 0 .0833 1 4.1204 .6000 9.1667 .3 24.3056 .1 100.0 0)
		:noise-amount .1500)
	      (vln_one_sin_ran 48.0400 3.6000 523.2400 .1500
		:fm-index 1.6424 :reverb-amount .0286
		:amp-env 
		'(0 0 .0833 1 4.1204 .6000 9.1667 .3 24.3056 .1 100.0 0)
		:noise-amount .1500)
	      (vln_one_sin_ran 48.0450 3.6000 349.2300 .1500
		:fm-index 1.6449 :reverb-amount .0333
		:amp-env 
		'(0 0 .0833 1 4.1204 .6000 9.1667 .3 24.3056 .1 100.0 0)
		:noise-amount .1500)
	      (vln_one_sin_ran 48.0500 6 699.4600 .1500
		:fm-index 1.5570 :reverb-amount .1300 :amp-env tap)
	      (vln_one_sin_ran 48.0500 6 1397.9200 .1500
		:fm-index 1.5131 :reverb-amount .1300 :amp-env tap)
	      (vln_one_sin_ran 48.0500 6 783.9800 .1500
		:fm-index 2.2031 :reverb-amount .1300 :amp-env tap)
	      (vln_one_sin_ran 48.0500 6 1046.4800 .1500
		:fm-index 2.2724 :reverb-amount .1300 :amp-env tap)
	      (vln_one_sin_ran 48.0600 9 21.8269 .1500
		:fm-index 2.1048 :reverb-amount .1 :amp-env tap)
	      (vln_one_sin_ran 48.0600 8 87.3075 .1500
		:fm-index 1.8854 :reverb-amount .1 :amp-env tap)
	      (vln_one_sin_ran 48.0600 7 65.4050 .1500
		:fm-index 1.6781 :reverb-amount .1 :amp-env tap)
	      (vln_one_sin_ran 48.0600 8 43.6538 .1500
		:fm-index 1.7862 :reverb-amount .1 :amp-env tap)
	      (vln_one_sin_ran 48.0700 6 175.6150 .1500
		:fm-index 2.2656 :reverb-amount .1 :amp-env tap)
	      (vln_one_sin_ran 48.0700 6 350.2300 .1500
		:fm-index 2.4241 :reverb-amount .1 :amp-env tap)
	      (vln_one_sin_ran 48.0700 6 131.8100 .1500
		:fm-index 2.4294 :reverb-amount .1 :amp-env tap)
	      (vln_one_sin_ran 48.0700 6 32.7025 .1500
		:fm-index 1.5779 :reverb-amount .1 :amp-env tap)
	      (vln_one_sin_ran 48.0800 6 196.9950 .1500
		:fm-index 1.8511 :reverb-amount .1 :amp-env tap)
	      (vln_one_sin_ran 48.0800 6 1047.4800 .1500
		:fm-index 2.2148 :reverb-amount .1 :amp-env tap)
	      (vln_one_sin_ran 48.0800 6 831.6200 .1500
		:fm-index 1.9913 :reverb-amount .1 :amp-env tap)
	      (vln_one_sin_ran 48.0800 6 2793.8400 .1500
		:fm-index 2.2607 :reverb-amount .1 :amp-env tap)
	      (vln_one_sin_ran 48.2700 6 784.9800 .1600
		:fm-index 2.0693 :reverb-amount .1 :amp-env tap)
	      (vln_one_sin_ran 48.2700 6 64.4050 .1600
		:fm-index 1.6920 :reverb-amount .1 :amp-env tap)
	      (vln_one_sin_ran 48.2700 6 208.6550 .1600
		:fm-index 2.2597 :reverb-amount .1 :amp-env tap)
					; from lizard
	      (vln_one_sin_ran 48.2700 6 43.6538 .1600
		:fm-index 2.2522 :reverb-amount .1 :amp-env tap)

	      (current-score-time 60)
	      (restore-fm-violin-defaults)
	      (vln_one_sin_ran 60 1.8000 349.2300 .1600
		:fm-index 2.1541 :reverb-amount .0225
		:amp-env 
		'(0 0 .1667 1 4.2003 .6000 9.2424 .3 24.3687 .1 100 0)
		:noise-amount .0500)
	      (vln_one_sin_ran 60.0100 2.7000 146.8300 .1600
		:fm-index 2.3335 :reverb-amount .0274
		:amp-env 
		'(0 0 .1111 1 4.1470 .6000 9.1919 .3 24.3266 .1 100.0 0)
		:noise-amount .0500)
	      (vln_one_sin_ran 60.0200 1.8000 880 .1600
		:fm-index 2.1910 :reverb-amount .0279
		:amp-env
		'(0 0 .1667 1 4.2003 .6000 9.2424 .3 24.3687 .1 100 0)
		:noise-amount .0500)
	      (vln_one_sin_ran 60.0250 3.6000 73.4150 .1600
		:fm-index 2.1410 :reverb-amount .0223
		:amp-env 
		'(0 0 .0833 1 4.1204 .6000 9.1667 .3 24.3056 .1 100.0 0)
		:noise-amount .0500)
	      (vln_one_sin_ran 60.0300 2.7000 87.3075 .1600
		:fm-index 1.8491 :reverb-amount .0217
		:amp-env 
		'(0 0 .1111 1 4.1470 .6000 9.1919 .3 24.3266 .1 100.0 0)
		:noise-amount .0010)
	      (vln_one_sin_ran 60.0300 2.7000 75.5662 .1600
		:fm-index 1.9191 :reverb-amount .0204
		:amp-env 
		'(0 0 .1111 1 4.1470 .6000 9.1919 .3 24.3266 .1 100.0 0)
		:noise-amount .0010)
	      (vln_one_sin_ran 60.0400 3.6000 52.3432 .1600
		:fm-index 1.6090 :reverb-amount .0296
		:amp-env 
		'(0 0 .0833 1 4.1204 .6000 9.1667 .3 24.3056 .1 100.0 0)
		:noise-amount .0010)
	      (vln_one_sin_ran 60.0450 1.8000 73.4150 .1600
		:fm-index 2.2201 :reverb-amount .0221
		:amp-env 
		'(0 0 .1667 1 4.2003 .6000 9.2424 .3 24.3687 .1 100 0)
		:noise-amount .0010)
	      (vln_one_sin_ran 60.0500 4 116.5400 .0600
		:fm-index 2.0230 :reverb-amount .1
		:amp-env tap :noise-amount .0010)
	      (vln_one_sin_ran 60.0500 4 97.9975 .0600
		:fm-index 1.7284 :reverb-amount .1
		:amp-env tap :noise-amount .0010)
	      (vln_one_sin_ran 60.0600 4 36.7075 .0600
		:fm-index 1.6845 :reverb-amount .1
		:amp-env tap :noise-amount .0010)
	      (vln_one_sin_ran 60.0650 4 97.9975 .0600
		:fm-index 2.4616 :reverb-amount .1
		:amp-env tap :noise-amount .0010)

	      (current-score-time 67)
	      (vln_one_sin_ran 67 1.8000 261.6200 .1600
		:fm-index 2.2576 :reverb-amount .0286
		:amp-env 
		'(0 0 .1667 1 4.2003 .6000 9.2424 .3 24.3687 .1 100 0)
		:noise-amount .0100)
	      (vln_one_sin_ran 67.0100 2.7000 130.8100 .1600
		:fm-index 2.1530 :reverb-amount .0330
		:amp-env 
		'(0 0 .1111 1 4.1470 .6000 9.1919 .3 24.3266 .1 100.0 0)
		:noise-amount .0100)
	      (vln_one_sin_ran 67.0200 1.8000 523.2400 .1600
		:fm-index 2.0608 :reverb-amount .0235
		:amp-env 
		'(0 0 .1667 1 4.2003 .6000 9.2424 .3 24.3687 .1 100 0)
		:noise-amount .0100)
	      (vln_one_sin_ran 67.0250 3.6000 65.4050 .1600
		:fm-index 2.2203 :reverb-amount .0234
		:amp-env 
		'(0 0 .0833 1 4.1204 .6000 9.1667 .3 24.3056 .1 100.0 0)
		:noise-amount .0100)
	      (vln_one_sin_ran 67.0300 2.7000 65.4050 .1600
		:fm-index 1.7089 :reverb-amount .0208
		:amp-env 
		'(0 0 .1111 1 4.1470 .6000 9.1919 .3 24.3266 .1 100.0 0)
		:noise-amount .0010)
	      (vln_one_sin_ran 67.0300 2.7000 130.8100 .1600
		:fm-index 2.2948 :reverb-amount .0269
		:amp-env 
		'(0 0 .1111 1 4.1470 .6000 9.1919 .3 24.3266 .1 100.0 0)
		:noise-amount .0010)
	      (vln_one_sin_ran 67.0400 3.6000 32.7025 .1600
		:fm-index 1.7677 :reverb-amount .0288
		:amp-env 
		'(0 0 .0833 1 4.1204 .6000 9.1667 .3 24.3056 .1 100.0 0)
		:noise-amount .0010)
	      (vln_one_sin_ran 67.0450 1.8000 32.7025 .1600
		:fm-index 1.9030 :reverb-amount .0209
		:amp-env 
		'(0 0 .1667 1 4.2003 .6000 9.2424 .3 24.3687 .1 100 0)
		:noise-amount .0010)
	      (vln_one_sin_ran 67.0500 4 65.4050 .0600
		:fm-index 2.2757 :reverb-amount .1
		:amp-env tap :noise-amount .0010)
	      (vln_one_sin_ran 67.0500 4 65.4050 .0600
		:fm-index 2.2435 :reverb-amount .1
		:amp-env tap :noise-amount .0010)
	      (vln_one_sin_ran 67.0600 4 32.7025 .0600
		:fm-index 1.9619 :reverb-amount .1
		:amp-env tap :noise-amount .0010)
	      (vln_one_sin_ran 67.0650 4 65.4050 .0600
		:fm-index 2.0207 :reverb-amount .1
		:amp-env tap :noise-amount .0010)

	      (current-score-time 73)
	      (vln_one_sin_ran 73.0100 .9000 3135.9200 .1600
		:fm-index 2.1204 :reverb-amount .0024
		:amp-env 
		'(0 0 .3333 1 4.3603 .6000 9.3939 .3 24.4950 .1 100.0 0)
		:noise-amount .0100)
	      (vln_one_sin_ran 73.0100 .4500 1567.9600 .1600
		:fm-index 2.0691 :reverb-amount .0025
		:amp-env 
		'(0 0 .6667 1 4.6801 .6000 9.6970 .3 24.7475 .1 100 0)
		:noise-amount .0100)
	      (vln_one_sin_ran 73.0200 .9000 6271.8400 .1600
		:fm-index 2.2081 :reverb-amount .0022
		:amp-env 
		'(0 0 .3333 1 4.3603 .6000 9.3939 .3 24.4950 .1 100.0 0)
		:noise-amount .0100)
	      (vln_one_sin_ran 73.0250 .9000 783.9800 .1600
		:fm-index 1.8719 :reverb-amount .0022
		:amp-env 
		'(0 0 .3333 1 4.3603 .6000 9.3939 .3 24.4950 .1 100.0 0)
		:noise-amount .0100)
	      (vln_one_sin_ran 73.0300 .2700 783.9800 .1600
		:fm-index 1.9705 :reverb-amount .0020
		:amp-env 
		'(0 0 1.1111 1 5.1066 .6000 10.1010 .3 25.0842 .1 100.0 0)
		:noise-amount .0100)
	      (vln_one_sin_ran 73.0300 .6300 1567.9600 .1600
		:fm-index 1.6778 :reverb-amount .0021
		:amp-env 
		'(0 0 .4762 1 4.4974 .6000 9.5238 .3 24.6032 .1 100.0 0)
		:noise-amount .0100)
	      (vln_one_sin_ran 73.0400 .9000 391.9900 .1600
		:fm-index 1.9558 :reverb-amount .0023
		:amp-env 
		'(0 0 .3333 1 4.3603 .6000 9.3939 .3 24.4950 .1 100.0 0)
		:noise-amount .0100)
	      (vln_one_sin_ran 73.0450 .4500 195.9950 .1600
		:fm-index 2.1344 :reverb-amount .0027
		:amp-env 
		'(0 0 .6667 1 4.6801 .6000 9.6970 .3 24.7475 .1 100 0)
		:noise-amount .0100)
	      (vln_one_sin_ran 73.0500 2 783.9800 .1600
		:reverb-amount .0100 :amp-env tap :noise-amount .0090)
	      (vln_one_sin_ran 73.0500 1 1567.9600 .1600
		:reverb-amount .0100 :amp-env tap :noise-amount .0090)
	      (vln_one_sin_ran 73.0600 2 391.9900 .1600
		:reverb-amount .0100 :amp-env tap :noise-amount .0090)
	      (vln_one_sin_ran 73.0650 1 783.9800 .1600
		:reverb-amount .0100 :amp-env tap :noise-amount .0090)
	      (vln_one_sin_ran 73.0700 2 195.9950 .1600
		:reverb-amount .0100 :amp-env tap :noise-amount .0040)
	      (vln_one_sin_ran 73.0700 1 1567.9600 .1600
		:reverb-amount .0100 :amp-env tap :noise-amount .0040)
	      (vln_one_sin_ran 73.0800 1 784.9800 .1600
		:reverb-amount .0100 :amp-env tap :noise-amount .0040)
	      (vln_one_sin_ran 73.0850 2 391.9900 .1600
		:reverb-amount .0100 :amp-env tap :noise-amount .0040)

	      (current-score-time 79)
	      (vln_one_sin_ran 79.0100 .9000 97.9975 .1
		:fm-index 2.0885 :reverb-amount .0031
		:amp-env 
		'(0 0 .3333 1 4.3603 .6000 9.3939 .3 24.4950 .1 100.0 0)
		:noise-amount .0100)
	      (vln_one_sin_ran 79.0100 1.8000 48.9988 .1
		:fm-index 2.2269 :reverb-amount .0026
		:amp-env 
		'(0 0 .1667 1 4.2003 .6000 9.2424 .3 24.3687 .1 100 0)
		:noise-amount .0100)
	      (vln_one_sin_ran 79.0200 .9000 195.9950 .1
		:fm-index 2.0305 :reverb-amount .0032
		:amp-env 
		'(0 0 .3333 1 4.3603 .6000 9.3939 .3 24.4950 .1 100.0 0)
		:noise-amount .0100)
	      (vln_one_sin_ran 79.0250 .9000 24.4994 .1
		:fm-index 2.4934 :reverb-amount .0025
		:amp-env 
		'(0 0 .3333 1 4.3603 .6000 9.3939 .3 24.4950 .1 100.0 0)
		:noise-amount .0100)
	      (vln_one_sin_ran 79.0300 1.8000 97.9975 .1
		:fm-index 2.4039 :reverb-amount .0023
		:amp-env 
		'(0 0 .1667 1 4.2003 .6000 9.2424 .3 24.3687 .1 100 0)
		:noise-amount .0400)
	      (vln_one_sin_ran 79.0300 .9000 195.9950 .1
		:fm-index 1.5159 :reverb-amount .0021
		:amp-env 
		'(0 0 .3333 1 4.3603 .6000 9.3939 .3 24.4950 .1 100.0 0)
		:noise-amount .0400)
	      (vln_one_sin_ran 79.0300 .9000 392.9900 .1
		:fm-index 2.2122 :reverb-amount .0028
		:amp-env 
		'(0 0 .3333 1 4.3603 .6000 9.3939 .3 24.4950 .1 100.0 0)
		:noise-amount .0400)
	      (vln_one_sin_ran 79.0300 1.8000 784.9800 .1
		:fm-index 2.1574 :reverb-amount .0020
		:amp-env 
		'(0 0 .1667 1 4.2003 .6000 9.2424 .3 24.3687 .1 100 0)
		:noise-amount .0400)
	      (vln_one_sin_ran 79.0300 2.7000 24.4994 .1
		:fm-index 2.1963 :reverb-amount .0031
		:amp-env 
		'(0 0 .1111 1 4.1470 .6000 9.1919 .3 24.3266 .1 100.0 0)
		:noise-amount .0100)
	      (vln_one_sin_ran 79.0300 1.8000 48.9988 .1
		:fm-index 1.9761 :reverb-amount .0032
		:amp-env 
		'(0 0 .1667 1 4.2003 .6000 9.2424 .3 24.3687 .1 100 0)
		:noise-amount .0100)
	      (vln_one_sin_ran 79.0400 2.7000 12.2497 .1
		:fm-index 1.5088 :reverb-amount .0021
		:amp-env 
		'(0 0 .1111 1 4.1470 .6000 9.1919 .3 24.3266 .1 100.0 0)
		:noise-amount .0100)
	      (vln_one_sin_ran 79.0450 1.8000 6.1248 .1
		:fm-index 1.7384 :reverb-amount .0021
		:amp-env 
		'(0 0 .1667 1 4.2003 .6000 9.2424 .3 24.3687 .1 100 0)
		:noise-amount .0100)
	      (vln_one_sin_ran 79.0500 2 24.4994 .1
		:reverb-amount .1 :amp-env tap :noise-amount .0040)
	      (vln_one_sin_ran 79.0500 1 48.9988 .1
		:reverb-amount .1 :amp-env tap :noise-amount .0040)
	      (vln_one_sin_ran 79.0600 2 12.2497 .1
		:reverb-amount .1 :amp-env tap :noise-amount .0040)
	      (vln_one_sin_ran 79.0650 1 24.4994 .1
		:reverb-amount .1 :amp-env tap :noise-amount .0040)
	      (vln_one_sin_ran 79.0700 2 6.1248 .1
		:fm-index 1.2474 :reverb-amount .1 :amp-env tap
		:noise-amount .0040)
	      (vln_one_sin_ran 79.0700 1 48.9988 .1
		:fm-index .7526 :reverb-amount .1 :amp-env tap
		:noise-amount .0040)
	      (vln_one_sin_ran 79.0800 1 25.4994 .1
		:fm-index 1.1080 :reverb-amount .1 :amp-env tap
		:noise-amount .0040)
	      (vln_one_sin_ran 79.0850 2 12.2497 .1
		:fm-index 1.0859 :reverb-amount .1 :amp-env tap
		:noise-amount .0040)
	      (vln_one_sin_ran 79.0900 4 97.9975 .1
		:fm-index 2.4788 :reverb-amount .1 :amp-env tap
		:noise-amount .0040)
	      (vln_one_sin_ran 79.0900 3 48.9988 .1
		:fm-index 1.8980 :reverb-amount .1 :amp-env tap
		:noise-amount .0040)
	      (vln_one_sin_ran 79.0900 3 25.4994 .1
		:fm-index 2.1151 :reverb-amount .1 :amp-env tap
		:noise-amount .0040)
	      (vln_one_sin_ran 79.0950 5 12.2497 .1
		:fm-index 2.3224 :reverb-amount .1 :amp-env tap
		:noise-amount .0040)

	      (current-score-time 85)
	      (vln_one_sin_ran 85.2100 .9000 123.4725 .1
		:reverb-amount .0031
		:amp-env 
		'(0 0 .3333 1 4.3603 .6000 9.3939 .3 24.4950 .1 100.0 0)
		:noise-amount .0100)
	      (vln_one_sin_ran 85.2100 1.8000 61.7363 .1
		:reverb-amount .0023
		:amp-env 
		'(0 0 .1667 1 4.2003 .6000 9.2424 .3 24.3687 .1 100 0)
		:noise-amount .0100)
	      (vln_one_sin_ran 85.2200 .9000 246.9450 .1
		:reverb-amount .0023
		:amp-env 
		'(0 0 .3333 1 4.3603 .6000 9.3939 .3 24.4950 .1 100.0 0)
		:noise-amount .0100)
	      (vln_one_sin_ran 85.2250 .9000 30.8681 .1
		:reverb-amount .0026
		:amp-env 
		'(0 0 .3333 1 4.3603 .6000 9.3939 .3 24.4950 .1 100.0 0)
		:noise-amount .0100)
	      (vln_one_sin_ran 85.2300 1.8000 123.4725 .1
		:reverb-amount .0027
		:amp-env 
		'(0 0 .1667 1 4.2003 .6000 9.2424 .3 24.3687 .1 100 0)
		:noise-amount .0400)
	      (vln_one_sin_ran 85.2300 .9000 246.9450 .1
		:reverb-amount .0026
		:amp-env 
		'(0 0 .3333 1 4.3603 .6000 9.3939 .3 24.4950 .1 100.0 0)
		:noise-amount .0400)
	      (vln_one_sin_ran 85.2300 .9000 494.8900 .1
		:reverb-amount .0020
		:amp-env 
		'(0 0 .3333 1 4.3603 .6000 9.3939 .3 24.4950 .1 100.0 0)
		:noise-amount .0400)
	      (vln_one_sin_ran 85.2300 1.8000 988.7800 .1
		:reverb-amount .0025
		:amp-env 
		'(0 0 .1667 1 4.2003 .6000 9.2424 .3 24.3687 .1 100 0)
		:noise-amount .0400)
	      (vln_one_sin_ran 85.2300 2.7000 30.8681 .1
		:reverb-amount .0028
		:amp-env 
		'(0 0 .1111 1 4.1470 .6000 9.1919 .3 24.3266 .1 100.0 0)
		:noise-amount .0100)
	      (vln_one_sin_ran 85.2300 1.8000 61.7363 .1
		:reverb-amount .0023
		:amp-env 
		'(0 0 .1667 1 4.2003 .6000 9.2424 .3 24.3687 .1 100 0)
		:noise-amount .0100)
	      (vln_one_sin_ran 85.2400 2.7000 15.4341 .1
		:reverb-amount .0030
		:amp-env 
		'(0 0 .1111 1 4.1470 .6000 9.1919 .3 24.3266 .1 100.0 0)
		:noise-amount .0100)
	      (vln_one_sin_ran 85.2450 1.8000 20.5788 .1
		:reverb-amount .0023
		:amp-env 
		'(0 0 .1667 1 4.2003 .6000 9.2424 .3 24.3687 .1 100 0)
		:noise-amount .0100)
	      (vln_one_sin_ran 85.2500 2 30.8681 .1
		:reverb-amount .1 :amp-env tap :noise-amount .0090)
	      (vln_one_sin_ran 85.2500 1 61.7363 .1
		:reverb-amount .1 :amp-env tap :noise-amount .0090)
	      (vln_one_sin_ran 85.2600 2 15.4341 .1
		:reverb-amount .1 :amp-env tap :noise-amount .0090)
	      (vln_one_sin_ran 85.2650 1 30.8681 .1
		:reverb-amount .1 :amp-env tap :noise-amount .0090)
	      (vln_one_sin_ran 85.2710 2 30.8681 .1
		:reverb-amount .1 :amp-env tap :noise-amount .0040)
	      (vln_one_sin_ran 85.2710 1 61.7363 .1
		:reverb-amount .1 :amp-env tap :noise-amount .0040)
	      (vln_one_sin_ran 85.2810 1 31.8681 .1
		:reverb-amount .1 :amp-env tap :noise-amount .0040)
	      (vln_one_sin_ran 85.2860 2 15.4341 .1
		:reverb-amount .1 :amp-env tap :noise-amount .0040)


	      (current-score-time 93)
	      (vln_one_sin_ran 93.0100 .9000 3135.9200 .1600
		:fm-index 1.7299 :reverb-amount .0026
		:amp-env 
		'(0 0 .3333 1 4.3603 .6000 9.3939 .3 24.4950 .1 100.0 0)
		:noise-amount .0100)
	      (vln_one_sin_ran 93.0100 .4500 1464.6987 .1600
		:fm-index 1.9173 :reverb-amount .0027
		:amp-env 
		'(0 0 .6667 1 4.6801 .6000 9.6970 .3 24.7475 .1 100 0)
		:noise-amount .0100)
	      (vln_one_sin_ran 93.0200 .9000 6714.0048 .1600
		:fm-index 2.4604 :reverb-amount .0032
		:amp-env 
		'(0 0 .3333 1 4.3603 .6000 9.3939 .3 24.4950 .1 100.0 0)
		:noise-amount .0100)
	      (vln_one_sin_ran 93.0250 .9000 684.1190 .1600
		:fm-index 1.9969 :reverb-amount .0021
		:amp-env 
		'(0 0 .3333 1 4.3603 .6000 9.3939 .3 24.4950 .1 100.0 0)
		:noise-amount .0100)
	      (vln_one_sin_ran 93.0300 .2700 684.1190 .1600
		:fm-index 2.0022 :reverb-amount .0026
		:amp-env 
		'(0 0 1.1111 1 5.1066 .6000 10.1010 .3 25.0842 .1 100.0 0)
		:noise-amount .0100)
	      (vln_one_sin_ran 93.0300 .6300 1464.6987 .1600
		:fm-index 2.1058 :reverb-amount .0027
		:amp-env 
		'(0 0 .4762 1 4.4974 .6000 9.5238 .3 24.6032 .1 100.0 0)
		:noise-amount .0100)
	      (vln_one_sin_ran 93.0400 .9000 319.5325 .1600
		:fm-index 2.2293 :reverb-amount .0029
		:amp-env 
		'(0 0 .3333 1 4.3603 .6000 9.3939 .3 24.4950 .1 100.0 0)
		:noise-amount .0100)
	      (vln_one_sin_ran 93.0450 .4500 149.2445 .1600
		:fm-index 1.5780 :reverb-amount .0025
		:amp-env 
		'(0 0 .6667 1 4.6801 .6000 9.6970 .3 24.7475 .1 100 0)
		:noise-amount .0100)
	      (vln_one_sin_ran 93.0500 1 684.1190 .1600
		:reverb-amount .0100 :amp-env yup :noise-amount .0090)
	      (vln_one_sin_ran 93.0500 1 1464.6987 .1600
		:reverb-amount .0100 :amp-env yup :noise-amount .0090)
	      (vln_one_sin_ran 93.0600 1 319.5325 .1600
		:reverb-amount .0100 :amp-env yup :noise-amount .0090)
	      (vln_one_sin_ran 93.0650 1 684.1190 .1600
		:reverb-amount .0100 :amp-env yup :noise-amount .0090)
	      (vln_one_sin_ran 93.0700 1 149.2445 .1600
		:reverb-amount .0100 :amp-env yup :noise-amount .0040)
	      (vln_one_sin_ran 93.0700 1 1464.6987 .1600
		:reverb-amount .0100 :amp-env yup :noise-amount .0040)
	      (vln_one_sin_ran 93.0800 1 561.6022 .160
		:reverb-amount .0100 :amp-env yup :noise-amount .0040)
	      (vln_one_sin_ran 93.0850 1 319.5325 .1600
		:reverb-amount .0100 :amp-env yup :noise-amount .0040)

	      (current-score-time 96)
	      (vln_one_sin_ran 96.0100 .9000 3135.9200 .1600
		:fm-index 1.6329 :reverb-amount .0031
		:amp-env 
		'(0 0 .3333 1 4.3603 .6000 9.3939 .3 24.4950 .1 100.0 0)
		:noise-amount .0100)
	      (vln_one_sin_ran 96.0100 .4500 1810.5774 .1600
		:fm-index 1.8298 :reverb-amount .0031
		:amp-env 
		'(0 0 .6667 1 4.6801 .6000 9.6970 .3 24.7475 .1 100 0)
		:noise-amount .0100)
	      (vln_one_sin_ran 96.0200 .9000 5431.4135 .1600
		:fm-index 2.1640 :reverb-amount .0022
		:amp-env 
		'(0 0 .3333 1 4.3603 .6000 9.3939 .3 24.4950 .1 100.0 0)
		:noise-amount .0100)
	      (vln_one_sin_ran 96.0250 .9000 1045.3680 .1600
		:fm-index 1.6971 :reverb-amount .0032
		:amp-env 
		'(0 0 .3333 1 4.3603 .6000 9.3939 .3 24.4950 .1 100.0 0)
		:noise-amount .0100)
	      (vln_one_sin_ran 96.0300 .2700 1045.3680 .1600
		:fm-index 2.4855 :reverb-amount .0028
		:amp-env 
		'(0 0 1.1111 1 5.1066 .6000 10.1010 .3 25.0842 .1 100.0 0)
		:noise-amount .0100)
	      (vln_one_sin_ran 96.0300 .6300 1810.5774 .1600
		:fm-index 2.1604 :reverb-amount .0020
		:amp-env 
		'(0 0 .4762 1 4.4974 .6000 9.5238 .3 24.6032 .1 100.0 0)
		:noise-amount .0100)
	      (vln_one_sin_ran 96.0400 .9000 603.5612 .1600
		:fm-index 2.4204 :reverb-amount .0031
		:amp-env 
		'(0 0 .3333 1 4.3603 .6000 9.3939 .3 24.4950 .1 100.0 0)
		:noise-amount .0100)
	      (vln_one_sin_ran 96.0450 .4500 348.4765 .1600
		:fm-index 2.3918 :reverb-amount .0026
		:amp-env 
		'(0 0 .6667 1 4.6801 .6000 9.6970 .3 24.7475 .1 100 0)
		:noise-amount .0100)
	      (vln_one_sin_ran 96.0460 .9000 201.1989 .1600
		:fm-index 1.5205 :reverb-amount .0024
		:amp-env 
		'(0 0 .3333 1 4.3603 .6000 9.3939 .3 24.4950 .1 100.0 0)
		:noise-amount .0100)
	      (vln_one_sin_ran 96.0460 .9000 116.1656 .1600
		:fm-index 2.3049 :reverb-amount .0028
		:amp-env 
		'(0 0 .3333 1 4.3603 .6000 9.3939 .3 24.4950 .1 100.0 0)
		:noise-amount .0100)
	      (vln_one_sin_ran 96.0500 .9000 3135.9200 .1600
		:fm-index 2.4363 :reverb-amount .0021
		:amp-env 
		'(0 0 .3333 1 4.3603 .6000 9.3939 .3 24.4950 .1 100.0 0)
		:noise-amount .0100)
	      (vln_one_sin_ran 96.0500 .4500 1464.6987 .1600
		:fm-index 2.3865 :reverb-amount .0027
		:amp-env 
		'(0 0 .6667 1 4.6801 .6000 9.6970 .3 24.7475 .1 100 0)
		:noise-amount .0100)
	      (vln_one_sin_ran 96.0600 .9000 6714.0048 .1600
		:fm-index 1.7354 :reverb-amount .0021
		:amp-env
		'(0 0 .3333 1 4.3603 .6000 9.3939 .3 24.4950 .1 100.0 0)
		:noise-amount .0100)
	      (vln_one_sin_ran 96.0650 .9000 684.1190 .1600
		:fm-index 1.8282 :reverb-amount .0025
		:amp-env
		'(0 0 .3333 1 4.3603 .6000 9.3939 .3 24.4950 .1 100.0 0)
		:noise-amount .0100)
	      (vln_one_sin_ran 96.0700 .2700 684.1190 .1600
		:fm-index 2.3923 :reverb-amount .0025
		:amp-env
		'(0 0 1.1111 1 5.1066 .6000 10.1010 .3 25.0842 .1 100.0 0)
		:noise-amount .0100)
	      (vln_one_sin_ran 96.0700 .6300 1464.6987 .1600
		:fm-index 2.2789 :reverb-amount .0028
		:amp-env
		'(0 0 .4762 1 4.4974 .6000 9.5238 .3 24.6032 .1 100.0 0)
		:noise-amount .0100)
	      (vln_one_sin_ran 96.0800 .9000 319.5325 .1600
		:fm-index 1.5438 :reverb-amount .0027
		:amp-env
		'(0 0 .3333 1 4.3603 .6000 9.3939 .3 24.4950 .1 100.0 0)
		:noise-amount .0100)
	      (vln_one_sin_ran 96.0850 .4500 149.2445 .1600
		:fm-index 2.4210 :reverb-amount .0028
		:amp-env
		'(0 0 .6667 1 4.6801 .6000 9.6970 .3 24.7475 .1 100 0)
		:noise-amount .0100)
	      (vln_one_sin_ran 96.0860 .9000 69.7078 .1600
		:fm-index 2.0288 :reverb-amount .0029
		:amp-env
		'(0 0 .3333 1 4.3603 .6000 9.3939 .3 24.4950 .1 100.0 0)
		:noise-amount .0100)
	      (vln_one_sin_ran 96.0860 .9000 32.5585 .1600
		:fm-index 1.8254 :reverb-amount .0028
		:amp-env
		'(0 0 .3333 1 4.3603 .6000 9.3939 .3 24.4950 .1 100.0 0)
		:noise-amount .0100)

	      (set! fm-violin-reverb-amount 0.0)
	      (set! fm-violin-noise-amount 0.01)
	      (current-score-time 99)
	      (vln_one_sin_ran 99.0500 .9000 3135.9200 .1600 :fm-index 1.7334
		:amp-env
		'(0 0 .3333 1 4.3603 .6000 9.3939 .3 24.4950 .1 100.0 0))
	      (vln_one_sin_ran 99.0500 .4500 1810.5774 .1600 :fm-index 2.3629
		:amp-env
		'(0 0 .6667 1 4.6801 .6000 9.6970 .3 24.7475 .1 100 0))
	      (vln_one_sin_ran 99.0600 .9000 5431.4135 .1600 :fm-index 2.2744
		:amp-env
		'(0 0 .3333 1 4.3603 .6000 9.3939 .3 24.4950 .1 100.0 0))
	      (vln_one_sin_ran 99.0650 .9000 1045.3680 .1600 :fm-index 1.8722
		:amp-env
		'(0 0 .3333 1 4.3603 .6000 9.3939 .3 24.4950 .1 100.0 0))
	      (vln_one_sin_ran 99.1100 .2700 1045.3680 .1600 :fm-index 2.3139
		:amp-env
		'(0 0 1.1111 1 5.1066 .6000 10.1010 .3 25.0842 .1 100.0 0))
	      (vln_one_sin_ran 99.1100 .6300 1810.5774 .1600 :fm-index 1.6216
		:amp-env
		'(0 0 .4762 1 4.4974 .6000 9.5238 .3 24.6032 .1 100.0 0))
	      (vln_one_sin_ran 99.1200 .9000 603.5612 .1600 :fm-index 1.5308
		:amp-env
		'(0 0 .3333 1 4.3603 .6000 9.3939 .3 24.4950 .1 100.0 0))
	      (vln_one_sin_ran 99.1650 .4500 348.4765 .1600 :fm-index 2.0346
		:amp-env
		'(0 0 .6667 1 4.6801 .6000 9.6970 .3 24.7475 .1 100 0))
	      (vln_one_sin_ran 99.1660 .9000 201.1989 .1600 :fm-index 1.8176
		:amp-env
		'(0 0 .3333 1 4.3603 .6000 9.3939 .3 24.4950 .1 100.0 0))
	      (vln_one_sin_ran 99.1660 .9000 116.1656 .1600 :fm-index 1.7145
		:amp-env
		'(0 0 .3333 1 4.3603 .6000 9.3939 .3 24.4950 .1 100.0 0))
	      (vln_one_sin_ran 99.1700 .9000 3135.9200 .1600 :fm-index 2.4459
		:amp-env
		'(0 0 .3333 1 4.3603 .6000 9.3939 .3 24.4950 .1 100.0 0))
	      (vln_one_sin_ran 99.1700 .4500 1464.6987 .1600 :fm-index 2.4644
		:amp-env
		'(0 0 .6667 1 4.6801 .6000 9.6970 .3 24.7475 .1 100 0))
	      (vln_one_sin_ran 99.1800 .9000 6714.0048 .1600 :fm-index 1.9985
		:amp-env
		'(0 0 .3333 1 4.3603 .6000 9.3939 .3 24.4950 .1 100.0 0))
	      (vln_one_sin_ran 99.1850 .9000 684.1190 .1600 :fm-index 2.4542
		:amp-env
		'(0 0 .3333 1 4.3603 .6000 9.3939 .3 24.4950 .1 100.0 0))
	      (vln_one_sin_ran 99.2900 .2700 684.1190 .1600 :fm-index 2.3391
		:amp-env
		'(0 0 1.1111 1 5.1066 .6000 10.1010 .3 25.0842 .1 100.0 0))
	      (vln_one_sin_ran 99.2900 .6300 1464.6987 .1600 :fm-index 1.5138
		:amp-env
		'(0 0 .4762 1 4.4974 .6000 9.5238 .3 24.6032 .1 100.0 0))
	      (vln_one_sin_ran 99.3000 .9000 319.5325 .1600 :fm-index 1.5440
		:amp-env
		'(0 0 .3333 1 4.3603 .6000 9.3939 .3 24.4950 .1 100.0 0))
	      (vln_one_sin_ran 99.3050 .4500 149.2445 .1600 :fm-index 2.2283
		:amp-env
		'(0 0 .6667 1 4.6801 .6000 9.6970 .3 24.7475 .1 100 0))
	      (vln_one_sin_ran 99.3060 .9000 69.7078 .1600 :fm-index 1.9498
		:amp-env
		'(0 0 .3333 1 4.3603 .6000 9.3939 .3 24.4950 .1 100.0 0))
	      (vln_one_sin_ran 99.3060 .9000 32.5585 .1600 :fm-index 2.2943
		:amp-env
		'(0 0 .3333 1 4.3603 .6000 9.3939 .3 24.4950 .1 100.0 0))

	      (restore-fm-violin-defaults)
	      (set! fm-violin-amp-env metalamp)
	      (set! fm-violin-glissando-amount 0.0)
	      (set! fm-violin-reverb-amount 0.01)
	      (set! fm-violin-fm1-rat 2.718)
	      (set! fm-violin-fm2-rat 1.414)
	      (set! fm-violin-fm3-rat 3.141)

	      (current-score-time 102)
	      (vln_one_sin 102.2600 1.2000 355.5416 .1600 :fm-index 2.0375)
	      (vln_one_sin 102.2600 1.5000 354.8319 .1600 :fm-index 1.8744)
	      (vln_one_sin 102.2603 .9000 356.2527 .1600 :fm-index 1.8743)
	      (vln_one_sin 102.2605 .9000 409.2356 .1600 :fm-index 2.0808)
	      (vln_one_sin 102.2605 2.1000 410.0541 .1600 :fm-index 1.9219)
	      (vln_one_sin 102.2608 1.2000 130.8100 .1600 :fm-index 1.5746)
	      (vln_one_sin 102.2613 3 130.2883 .1600 :fm-index 2.3771)
	      (vln_one_sin 102.2615 .9000 130.2883 .1600 :fm-index 1.7765)
	      (vln_one_sin 102.2615 2.1000 130.5489 .2600 :fm-index 1.6485)
	      (vln_one_sin 102.2625 2 130.8100 .1600 :fm-index 2.1416)
	      (vln_one_sin 102.2633 2 130.5488 .1600 :fm-index 2.0883)

	      (set! fm-violin-noise-amount .0001)
	      (set! fm-violin-amp-env newf)
	      (set! fm-violin-reverb-amount .01)

	      (current-score-time 106)
	      (vln_one_sin_ran 106.2605 .8000 523.2400 .1600 :fm-index 2.3056)
	      (vln_one_sin_ran 106.2605 1 247.1611 .1600 :fm-index 1.6308)
	      (vln_one_sin_ran 106.2610 .6000 1107.6991 .1600 :fm-index 1.9364)
	      (vln_one_sin_ran 106.2613 2 116.7506 .1600 :fm-index 2.3740)
	      (vln_one_sin_ran 106.2615 .6000 116.7506 .1600 :fm-index 1.8374)
	      (vln_one_sin_ran 106.2615 1.4000 247.1611 .1600 :fm-index 1.7250)
	      (vln_one_sin_ran 106.2620 .6000 55.1491 .1600 :fm-index 1.5495)
	      (vln_one_sin_ran 106.2623 1 26.0506 .1600 :fm-index 1.7235)
	      (vln_one_sin_ran 106.2623 2 12.3054 .1600 :fm-index 1.8818)
	      (vln_one_sin_ran 106.2623 2 5.8127 .1600 :fm-index 1.9537)
	      (vln_one_sin_ran 106.2625 2 523.2400 .1600 :fm-index 2.1593)
	      (vln_one_sin_ran 106.2625 1 256.2390 .1600 :fm-index 1.9851)
	      (vln_one_sin_ran 106.2630 2 1068.4561 .1600 :fm-index 1.8015)
	      (vln_one_sin_ran 106.2633 2 125.4843 .1600 :fm-index 1.6161)
	      (vln_one_sin_ran 106.2635 .6000 125.4843 .1600 :fm-index 2.2767)
	      (vln_one_sin_ran 106.2635 1.4000 256.2390 .1600 :fm-index 2.0835)
	      (vln_one_sin_ran 106.2640 .4 61.4517 .1600 :fm-index 1.5310)
	      (vln_one_sin_ran 106.2643 1 30.0939 .1600 :fm-index 1.5803)
	      (vln_one_sin_ran 106.2643 2 14.7374 .1600 :fm-index 1.9586)
	      (vln_one_sin_ran 106.2643 2 7.2172 .1600 :fm-index 1.7270)
	      (vln_one_sin_ran 106.2645 6 28.4710 .1600 :fm-index 1.5983)
	      (vln_one_sin_ran 106.2648 10 25.6239 .1600 :fm-index 1.7285)
	      (vln_one_sin_ran 106.2648 8 21.3532 .1600 :fm-index 1.7955)
	      (vln_one_sin_ran 106.2648 8 17.0826 .1600 :fm-index 2.0866)

	      (set! fm-violin-reverb-amount .001)
	      (set! fm-violin-noise-amount .004)
	      (set! fm-violin-fm1-rat 3.141)
	      (set! fm-violin-fm2-rat 1.414)
	      (set! fm-violin-fm3-rat 2.718)

	      (current-score-time 110)
	      (vln_one_sin_ran 110.2600 1.6000 1643.4968 .1600 :fm-index 2.1104)
	      (vln_one_sin_ran 110.2600 2 1643.4968 .1600 :fm-index 1.5191)
	      (vln_one_sin_ran 110.2603 1.2000 1643.4968 .1600 :fm-index 2.0478)
	      (vln_one_sin_ran 110.2603 4 1643.4968 .1600 :fm-index 2.0473)
	      (vln_one_sin_ran 110.2605 1.2000 1422.1663 .1600 :fm-index 1.9845)
	      (vln_one_sin_ran 110.2605 2.8000 1422.1663 .1600 :fm-index 2.0429)
	      (vln_one_sin_ran 110.2605 1.2000 1422.1663 .1600 :fm-index 1.6184)
	      (vln_one_sin_ran 110.2608 1.6000 523.2400 .1600 :fm-index 2.3908)
	      (vln_one_sin_ran 110.2608 2 523.2400 .1600 :fm-index 1.6733)
	      (vln_one_sin_ran 110.2610 1.2000 523.2400 .1600 :fm-index 2.0431)
	      (vln_one_sin_ran 110.2610 4 523.2400 .1600 :fm-index 1.7430)
	      (vln_one_sin_ran 110.2613 1.2000 523.2400 .1600 :fm-index 2.2030)
	      (vln_one_sin_ran 110.2613 2.8000 523.2400 .1600 :fm-index 2.0149)
	      (vln_one_sin_ran 110.2615 1.2000 523.2400 .1600 :fm-index 2.2310)
	      (vln_one_sin_ran 110.2615 2 523.2400 .1600 :fm-index 2.1625)
	      (vln_one_sin_ran 110.2618 4 523.2400 .1600 :fm-index 2.0000)
	      (vln_one_sin_ran 110.2618 4 523.2400 .1600 :fm-index 2.2034)
	      (vln_one_sin_ran 110.2620 3 523.2400 .1600 :fm-index 2.0186)
	      (vln_one_sin_ran 110.2620 1.5000 523.2400 .1600 :fm-index 2.1373)
	      (vln_one_sin_ran 110.2623 3 523.2400 .1600 :fm-index 1.9046)
	      (vln_one_sin_ran 110.2623 3 523.2400 .1600 :fm-index 2.1834)
	      (vln_one_sin_ran 110.2625 1.2000 523.2400 .1600 :fm-index 1.8266)
	      (vln_one_sin_ran 110.2625 2.8000 523.2400 .1600 :fm-index 1.5937)
	      (vln_one_sin_ran 110.2628 .8000 523.2400 .1600 :fm-index 1.9762)
	      (vln_one_sin_ran 110.2628 2 523.2400 .1600 :fm-index 1.8954)
	      (vln_one_sin_ran 110.2630 4 523.2400 .1600 :fm-index 2.3302)
	      (vln_one_sin_ran 110.2630 4 523.2400 .1600 :fm-index 2.4949)

	      (set! fm-violin-fm1-rat 3.414)
	      (set! fm-violin-fm2-rat 1.414)
	      (set! fm-violin-fm3-rat 2.718)
	      (current-score-time 114)
	      (vln_one_sin_ran 114.2600 1.6000 821.7484 .1600 :fm-index 2.4793)
	      (vln_one_sin_ran 114.2600 2 821.7484 .1600 :fm-index 2.4789)
	      (vln_one_sin_ran 114.2603 1.2000 821.7484 .1600 :fm-index 2.0827)
	      (vln_one_sin_ran 114.2603 4 821.7484 .1600 :fm-index 2.4769)
	      (vln_one_sin_ran 114.2605 1.2000 711.0832 .1600 :fm-index 2.4094)
	      (vln_one_sin_ran 114.2605 2.8000 711.0832 .1600 :fm-index 2.4031)
	      (vln_one_sin_ran 114.2605 1.2000 711.0832 .1600 :fm-index 2.1428)
	      (vln_one_sin_ran 114.2608 1.6000 261.6200 .1600 :fm-index 2.3129)
	      (vln_one_sin_ran 114.2608 2 261.6200 .1600 :fm-index 2.3488)
	      (vln_one_sin_ran 114.2610 1.2000 261.6200 .1600 :fm-index 2.1466)
	      (vln_one_sin_ran 114.2610 4 261.6200 .1600 :fm-index 1.6938)
	      (vln_one_sin_ran 114.2613 1.2000 261.6200 .1600 :fm-index 2.1287)
	      (vln_one_sin_ran 114.2613 2.8000 261.6200 .1600 :fm-index 2.1917)
	      (vln_one_sin_ran 114.2615 1.2000 261.6200 .1600 :fm-index 2.3583)
	      (vln_one_sin_ran 114.2615 2 261.6200 .1600 :fm-index 1.8368)
	      (vln_one_sin_ran 114.2618 4 261.6200 .1600 :fm-index 1.5107)
	      (vln_one_sin_ran 114.2618 4 261.6200 .1600 :fm-index 1.6218)
	      (vln_one_sin_ran 114.2620 3 261.6200 .1600 :fm-index 1.9041)
	      (vln_one_sin_ran 114.2620 1.5000 261.6200 .1600 :fm-index 1.5748)
	      (vln_one_sin_ran 114.2623 3 261.6200 .1600 :fm-index 1.9339)
	      (vln_one_sin_ran 114.2623 3 261.6200 .1600 :fm-index 2.0489)
	      (vln_one_sin_ran 114.2625 1.2000 261.6200 .1600 :fm-index 2.0888)
	      (vln_one_sin_ran 114.2625 2.8000 261.6200 .1600 :fm-index 1.7306)
	      (vln_one_sin_ran 114.2628 .8000 261.6200 .1600 :fm-index 2.3257)
	      (vln_one_sin_ran 114.2628 2 261.6200 .1600 :fm-index 2.4755)
	      (vln_one_sin_ran 114.2630 4 261.6200 .1600 :fm-index 1.9459)
	      (vln_one_sin_ran 114.2630 4 261.6200 .1600 :fm-index 1.5782)

	      (set! fm-violin-fm1-rat 3.414)
	      (set! fm-violin-fm2-rat 1.414)
	      (set! fm-violin-fm3-rat 2.718)
	      (current-score-time 118)
	      (vln_one_sin_ran 118.2600 1.6000 3286.9937 .1600 :fm-index 1.6655)
	      (vln_one_sin_ran 118.2600 2 3286.9937 .1600 :fm-index 1.9356)
	      (vln_one_sin_ran 118.2603 1.2000 3286.9937 .1600 :fm-index 1.5665)
	      (vln_one_sin_ran 118.2603 4 3286.9937 .1600 :fm-index 1.6701)
	      (vln_one_sin_ran 118.2605 1.2000 2844.3326 .1600 :fm-index 2.3273)
	      (vln_one_sin_ran 118.2605 2.8000 2844.3326 .1600 :fm-index 1.5520)
	      (vln_one_sin_ran 118.2605 1.2000 2844.3326 .1600 :fm-index 2.4104)
	      (vln_one_sin_ran 118.2608 1.6000 1046.4800 .1600 :fm-index 2.1075)
	      (vln_one_sin_ran 118.2608 2 1046.4800 .1600 :fm-index 1.7004)
	      (vln_one_sin_ran 118.2610 1.2000 1046.4800 .1600 :fm-index 1.6502)
	      (vln_one_sin_ran 118.2610 4 1046.4800 .1600 :fm-index 2.4591)
	      (vln_one_sin_ran 118.2613 1.2000 1046.4800 .1600 :fm-index 2.1491)
	      (vln_one_sin_ran 118.2613 2.8000 1046.4800 .1600 :fm-index 2.1594)
	      (vln_one_sin_ran 118.2615 1.2000 1046.4800 .1600 :fm-index 2.4783)
	      (vln_one_sin_ran 118.2615 2 1046.4800 .1600 :fm-index 2.2080)
	      (vln_one_sin_ran 118.2618 4 1046.4800 .1600 :fm-index 1.5844)
	      (vln_one_sin_ran 118.2618 4 1046.4800 .1600 :fm-index 1.5440)
	      (vln_one_sin_ran 118.2620 3 1046.4800 .1600 :fm-index 1.9857)
	      (vln_one_sin_ran 118.2620 1.5000 1046.4800 .1600 :fm-index 1.5165)
	      (vln_one_sin_ran 118.2623 3 1046.4800 .1600 :fm-index 1.8309)
	      (vln_one_sin_ran 118.2623 3 1046.4800 .1600 :fm-index 2.1236)
	      (vln_one_sin_ran 118.2625 1.2000 1046.4800 .1 :fm-index 2.4074)
	      (vln_one_sin_ran 118.2625 2.8000 1046.4800 .1 :fm-index 1.6315)
	      (vln_one_sin_ran 118.2628 .8000 1046.4800 .1 :fm-index 1.8061)
	      (vln_one_sin_ran 118.2628 2 1046.4800 .1 :fm-index 2.3664)
	      (vln_one_sin_ran 118.2630 4 1046.4800 .1 :fm-index 2.2490)
	      (vln_one_sin_ran 118.2630 4 1046.4800 .1 :fm-index 2.4081)

	      (set! fm-violin-reverb-amount .01)

	      (current-score-time 122)
	      (vln_one_sin_ran 122.2600 1.6000 1643.4968 .1600 :fm-index 1.9284)
	      (vln_one_sin_ran 122.2600 2 1643.4968 .1600 :fm-index 2.2171)
	      (vln_one_sin_ran 122.2603 1.2000 1643.4968 .1600 :fm-index 2.2272)
	      (vln_one_sin_ran 122.2603 4 1643.4968 .1600 :fm-index 1.5677)
	      (vln_one_sin_ran 122.2605 1.2000 1422.1663 .1600 :fm-index 2.0476)
	      (vln_one_sin_ran 122.2605 2.8000 1422.1663 .1600 :fm-index 2.3289)
	      (vln_one_sin_ran 122.2605 1.2000 1422.1663 .1600 :fm-index 2.0269)
	      (vln_one_sin_ran 122.2608 1.6000 523.2400 .1600 :fm-index 1.7767)
	      (vln_one_sin_ran 122.2608 2 523.2400 .1600 :fm-index 1.8117)
	      (vln_one_sin_ran 122.2610 1.2000 523.2400 .1600 :fm-index 1.5694)
	      (vln_one_sin_ran 122.2610 4 523.2400 .1600 :fm-index 1.6869)
	      (vln_one_sin_ran 122.2613 1.2000 523.2400 .1600 :fm-index 1.9340)
	      (vln_one_sin_ran 122.2613 2.8000 523.2400 .1600 :fm-index 2.3986)
	      (vln_one_sin_ran 122.2615 1.2000 523.2400 .1600 :fm-index 2.4593)
	      (vln_one_sin_ran 122.2615 2 523.2400 .1600 :fm-index 2.3430)
	      (vln_one_sin_ran 122.2618 4 523.2400 .1600 :fm-index 2.2650)
	      (vln_one_sin_ran 122.2618 4 523.2400 .1600 :fm-index 2.3015)
	      (vln_one_sin_ran 122.2620 3 523.2400 .1600 :fm-index 1.9909)
	      (vln_one_sin_ran 122.2620 1.5000 523.2400 .1600 :fm-index 2.3916)
	      (vln_one_sin_ran 122.2623 3 523.2400 .1600 :fm-index 2.0401)
	      (vln_one_sin_ran 122.2623 3 523.2400 .1600 :fm-index 1.8484)
	      (vln_one_sin_ran 122.2625 1.2000 523.2400 .1600 :fm-index 2.3138)
	      (vln_one_sin_ran 122.2625 2.8000 523.2400 .1600 :fm-index 1.6295)
	      (vln_one_sin_ran 122.2628 .8000 523.2400 .1600 :fm-index 2.2344)
	      (vln_one_sin_ran 122.2628 2 523.2400 .1600 :fm-index 1.8423)
	      (vln_one_sin_ran 122.2630 4 523.2400 .1600 :fm-index 2.2086)
	      (vln_one_sin_ran 122.2630 4 523.2400 .1600 :fm-index 2.3130)

	      (set! fm-violin-noise-amount .0001)
	      (set! fm-violin-fm1-rat 2.718)
	      (set! fm-violin-fm2-rat 1.141)
	      (set! fm-violin-fm3-rat 3.141)
	      (set! fm-violin-reverb-amount .01)

	      (current-score-time 126)
	      (vln_one_sin_ran 126.2605 .8000 523.2400 .1600 :fm-index 2.0123)
	      (vln_one_sin_ran 126.2605 1 493.8728 .1600 :fm-index 2.1176)
	      (vln_one_sin_ran 126.2610 .6000 554.3535 .1600 :fm-index 1.9163)
	      (vln_one_sin_ran 126.2613 2 466.1539 .1600 :fm-index 1.5048)
	      (vln_one_sin_ran 126.2615 .6000 466.1539 .1600 :fm-index 1.5242)
	      (vln_one_sin_ran 126.2615 1.4000 493.8728 .1600 :fm-index 1.9509)
	      (vln_one_sin_ran 126.2620 .6000 439.9907 .1600 :fm-index 2.2131)
	      (vln_one_sin_ran 126.2623 1 415.2959 .1600 :fm-index 1.7326)
	      (vln_one_sin_ran 126.2623 2 391.9871 .1600 :fm-index 1.9936)
	      (vln_one_sin_ran 126.2623 2 369.9866 .1600 :fm-index 2.1103)
	      (vln_one_sin_ran 126.2625 2 523.2400 .1600 :fm-index 1.6206)
	      (vln_one_sin_ran 126.2625 1 522.7173 .1600 :fm-index 1.8598)
	      (vln_one_sin_ran 126.2630 2 523.7632 .1600 :fm-index 1.8015)
	      (vln_one_sin_ran 126.2633 2 522.1951 .1600 :fm-index 2.3575)
	      (vln_one_sin_ran 126.2635 .6000 522.1951 .1600 :fm-index 1.5010)
	      (vln_one_sin_ran 126.2635 1.4000 522.7173 .1600 :fm-index 2.4075)
	      (vln_one_sin_ran 126.2640 .4 521.6734 .1600 :fm-index 2.0721)
	      (vln_one_sin_ran 126.2643 1 521.1523 .1600 :fm-index 2.0433)
	      (vln_one_sin_ran 126.2643 2 520.6316 .1600 :fm-index 1.9788)
	      (vln_one_sin_ran 126.2643 2 520.1115 .1600 :fm-index 1.6770)

	      (set! fm-violin-noise-amount .004)

	      (current-score-time 134)
	      (vln_one_sin_ran 134.2600 .8000 1046.4800 .1600 :fm-index 1.5610)
	      (vln_one_sin_ran 134.2600 1 1044.3912 .1600 :fm-index 2.3514)
	      (vln_one_sin_ran 134.2603 .6000 1048.5730 .1600 :fm-index 1.9958)
	      (vln_one_sin_ran 134.2603 2 1042.3066 .1600 :fm-index 1.9654)
	      (vln_one_sin_ran 134.2605 .6000 1042.3066 .1600 :fm-index 1.5285)
	      (vln_one_sin_ran 134.2605 1.4000 1044.3912 .1600 :fm-index 1.8881)
	      (vln_one_sin_ran 134.2608 .6000 1040.2262 .1600 :fm-index 1.8682)
	      (vln_one_sin_ran 134.2608 .8000 523.2400 .1600 :fm-index 1.8296)
	      (vln_one_sin_ran 134.2610 1 522.1956 .1600 :fm-index 2.1899)
	      (vln_one_sin_ran 134.2610 .6000 524.2865 .1600 :fm-index 1.9614)
	      (vln_one_sin_ran 134.2613 2 521.1533 .1600 :fm-index 1.7483)
	      (vln_one_sin_ran 134.2615 .6000 521.1533 .1600 :fm-index 1.8717)
	      (vln_one_sin_ran 134.2615 1.4000 522.1956 .1600 :fm-index 1.5619)
	      (vln_one_sin_ran 134.2620 .6000 520.1131 .1600 :fm-index 2.4331)
	      (vln_one_sin_ran 134.2623 1 519.0749 .1600 :fm-index 2.4153)
	      (vln_one_sin_ran 134.2623 2 518.0388 .1600 :fm-index 1.5477)
	      (vln_one_sin_ran 134.2623 2 517.0048 .1600 :fm-index 1.9956)
	      (vln_one_sin_ran 134.2625 2 523.2400 .1600 :fm-index 1.8111)
	      (vln_one_sin_ran 134.2625 1 522.7173 .1600 :fm-index 2.4820)
	      (vln_one_sin_ran 134.2630 2 523.7632 .1600 :fm-index 1.5744)
	      (vln_one_sin_ran 134.2633 2 522.1951 .1600 :fm-index 1.9950)
	      (vln_one_sin_ran 134.2635 .6000 522.1951 .1600 :fm-index 1.9792)
	      (vln_one_sin_ran 134.2635 1.4000 522.7173 .1600 :fm-index 1.7415)
	      (vln_one_sin_ran 134.2640 .4 521.6734 .1600 :fm-index 2.0884)
	      (vln_one_sin_ran 134.2643 1 521.1523 .1600 :fm-index 2.3605)
	      (vln_one_sin_ran 134.2643 2 520.6316 .1600 :fm-index 1.7817)
	      (vln_one_sin_ran 134.2643 2 520.1115 .1600 :fm-index 2.0283)

	      (set! fm-violin-reverb-amount .1)
	      (set! fm-violin-fm1-rat 2.718)
	      (set! fm-violin-fm2-rat 1.414)
	      (set! fm-violin-fm3-rat 3.141)
	      (set! fm-violin-glissando-amount 0.0)

	      (current-score-time 138)
	      (vln_one_sin_ran 138.2600 1.6000 177.7708 .1600 :fm-index 1.6447)
	      (vln_one_sin_ran 138.2600 2 177.7708 .1600 :fm-index 2.4875)
	      (vln_one_sin_ran 138.2603 1.2000 177.7708 .1600 :fm-index 1.6126)
	      (vln_one_sin_ran 138.2603 4 177.7708 .1600 :fm-index 2.3122)
	      (vln_one_sin_ran 138.2605 1.2000 205.4371 .1600 :fm-index 2.4116)
	      (vln_one_sin_ran 138.2605 2.8000 205.4371 .1600 :fm-index 1.5337)
	      (vln_one_sin_ran 138.2608 1.2000 205.4371 .1600 :fm-index 2.0307)
	      (vln_one_sin_ran 138.2608 1.6000 65.4050 .1600 :fm-index 2.2341)
	      (vln_one_sin_ran 138.2610 2 65.4050 .1600 :fm-index 2.4683)
	      (vln_one_sin_ran 138.2610 1.2000 65.4050 .1600 :fm-index 2.0643)
	      (vln_one_sin_ran 138.2613 4 65.4050 .1600 :fm-index 2.1925)
	      (vln_one_sin_ran 138.2615 1.2000 65.4050 .1600 :fm-index 2.1325)
	      (vln_one_sin_ran 138.2615 2.8000 65.4050 .1600 :fm-index 1.5847)
	      (vln_one_sin_ran 138.2620 1.2000 65.4050 .1600 :fm-index 1.8781)
	      (vln_one_sin_ran 138.2623 2 65.4050 .1600 :fm-index 2.0283)
	      (vln_one_sin_ran 138.2623 4 65.4050 .1600 :fm-index 2.4739)
	      (vln_one_sin_ran 138.2623 4 65.4050 .1600 :fm-index 2.2333)
	      (vln_one_sin_ran 138.2625 2 65.4050 .1600 :fm-index 2.2194)
	      (vln_one_sin_ran 138.2625 1 65.4050 .1600 :fm-index 2.4491)
	      (vln_one_sin_ran 138.2630 2 65.4050 .1600 :fm-index 1.5672)
	      (vln_one_sin_ran 138.2633 2 65.4050 .1600 :fm-index 2.3254)
	      (vln_one_sin_ran 138.2635 1.2000 65.4050 .1600 :fm-index 1.8302)
	      (vln_one_sin_ran 138.2635 2.8000 65.4050 .1600 :fm-index 1.9201)
	      (vln_one_sin_ran 138.2640 .8000 65.4050 .1600 :fm-index 1.9164)
	      (vln_one_sin_ran 138.2643 2 65.4050 .1600 :fm-index 1.9483)
	      (vln_one_sin_ran 138.2643 4 65.4050 .1600 :fm-index 2.4247)
	      (vln_one_sin_ran 138.2643 4 65.4050 .1600 :fm-index 2.0419)

	      (set! fm-violin-fm1-rat 2.718)
	      (set! fm-violin-fm2-rat 4.414)
	      (set! fm-violin-fm3-rat 3.141)

	      (current-score-time 142)
	      (vln_one_sin_ran 142.2600 1.6000 88.8854 .1600 :fm-index 2.2832)
	      (vln_one_sin_ran 142.2600 2 88.8854 .1600 :fm-index 1.6588)
	      (vln_one_sin_ran 142.2603 1.2000 88.8854 .1600 :fm-index 2.2392)
	      (vln_one_sin_ran 142.2603 4 88.8854 .1600 :fm-index 1.7354)
	      (vln_one_sin_ran 142.2605 1.2000 102.7186 .1600 :fm-index 1.6692)
	      (vln_one_sin_ran 142.2605 2.8000 102.7186 .1600 :fm-index 2.1518)
	      (vln_one_sin_ran 142.2608 1.2000 102.7186 .1600 :fm-index 2.2439)
	      (vln_one_sin_ran 142.2608 1.6000 32.7025 .1600 :fm-index 2.1665)
	      (vln_one_sin_ran 142.2610 2 32.7025 .1600 :fm-index 1.7947)
	      (vln_one_sin_ran 142.2610 1.2000 32.7025 .1600 :fm-index 2.0740)
	      (vln_one_sin_ran 142.2613 4 32.7025 .1600 :fm-index 1.9705)
	      (vln_one_sin_ran 142.2615 1.2000 32.7025 .1600 :fm-index 1.9447)
	      (vln_one_sin_ran 142.2615 2.8000 32.7025 .1600 :fm-index 2.4918)
	      (vln_one_sin_ran 142.2620 1.2000 32.7025 .1600 :fm-index 1.6275)
	      (vln_one_sin_ran 142.2623 2 32.7025 .1600 :fm-index 2.2355)
	      (vln_one_sin_ran 142.2623 4 32.7025 .1600 :fm-index 2.0084)
	      (vln_one_sin_ran 142.2623 4 32.7025 .1600 :fm-index 1.8964)
	      (vln_one_sin_ran 142.2625 2 32.7025 .1600 :fm-index 2.3937)
	      (vln_one_sin_ran 142.2625 1 32.7025 .1600 :fm-index 1.8634)
	      (vln_one_sin_ran 142.2630 2 32.7025 .1600 :fm-index 1.5217)
	      (vln_one_sin_ran 142.2633 2 32.7025 .1600 :fm-index 1.9275)
	      (vln_one_sin_ran 142.2635 1.2000 32.7025 .1600 :fm-index 2.4413)
	      (vln_one_sin_ran 142.2635 2.8000 32.7025 .1600 :fm-index 2.3242)
	      (vln_one_sin_ran 142.2640 .8000 32.7025 .1600 :fm-index 2.3267)
	      (vln_one_sin_ran 142.2643 2 32.7025 .1600 :fm-index 1.7004)
	      (vln_one_sin_ran 142.2643 4 32.7025 .1600 :fm-index 1.8785)
	      (vln_one_sin_ran 142.2643 4 32.7025 .1600 :fm-index 2.4573)

	      (set! fm-violin-fm1-rat 2.718)
	      (set! fm-violin-fm2-rat 4.414)
	      (set! fm-violin-fm3-rat 5.141)

	      (current-score-time 146)
	      (vln_one_sin_ran 146.2600 1.6000 22.2213 .1600 :fm-index 1.6232)
	      (vln_one_sin_ran 146.2600 2 22.2213 .1600 :fm-index 1.5982)
	      (vln_one_sin_ran 146.2603 1.2000 22.2213 .1600 :fm-index 2.1585)
	      (vln_one_sin_ran 146.2603 4 22.2213 .1600 :fm-index 2.2207)
	      (vln_one_sin_ran 146.2605 1.2000 42.0309 .1600 :fm-index 1.5294)
	      (vln_one_sin_ran 146.2605 2.8000 42.0309 .1600 :fm-index 1.9544)
	      (vln_one_sin_ran 146.2608 1.2000 42.0309 .1600 :fm-index 2.4016)
	      (vln_one_sin_ran 146.2608 1.6000 8.1756 .1600 :fm-index 1.5267)
	      (vln_one_sin_ran 146.2610 2 8.1756 .1600 :fm-index 2.4190)
	      (vln_one_sin_ran 146.2610 1.2000 8.1756 .1600 :fm-index 2.2757)
	      (vln_one_sin_ran 146.2613 4 8.1756 .1600 :fm-index 2.3607)
	      (vln_one_sin_ran 146.2615 1.2000 8.1756 .1600 :fm-index 1.8698)
	      (vln_one_sin_ran 146.2615 2.8000 8.1756 .1600 :fm-index 2.3753)
	      (vln_one_sin_ran 146.2620 1.2000 8.1756 .1600 :fm-index 2.3392)
	      (vln_one_sin_ran 146.2623 2 8.1756 .1600 :fm-index 1.5088)
	      (vln_one_sin_ran 146.2623 4 8.1756 .1600 :fm-index 2.2084)
	      (vln_one_sin_ran 146.2623 4 8.1756 .1600 :fm-index 1.9512)
	      (vln_one_sin_ran 146.2625 2 8.1756 .1600 :fm-index 2.0399)
	      (vln_one_sin_ran 146.2625 1 8.1756 .1600 :fm-index 1.7053)
	      (vln_one_sin_ran 146.2630 2 8.1756 .1600 :fm-index 2.3204)
	      (vln_one_sin_ran 146.2633 2 8.1756 .1600 :fm-index 1.6336)
	      (vln_one_sin_ran 146.2635 1.2000 8.1756 .1600 :fm-index 1.9483)
	      (vln_one_sin_ran 146.2635 2.8000 8.1756 .1600 :fm-index 2.3255)
	      (vln_one_sin_ran 146.2640 .8000 8.1756 .1600 :fm-index 1.7331)
	      (vln_one_sin_ran 146.2643 2 8.1756 .1600 :fm-index 1.9318)
	      (vln_one_sin_ran 146.2643 4 8.1756 .1600 :fm-index 1.6908)
	      (vln_one_sin_ran 146.2643 4 8.1756 .1600 :fm-index 2.4103)

	      (current-score-time 150)
	      (vln_one_sin_ran 150.2600 1.6000 11.1107 .1600 :fm-index 1.6371)
	      (vln_one_sin_ran 150.2600 2 11.1107 .1600 :fm-index 1.8971)
	      (vln_one_sin_ran 150.2603 1.2000 11.1107 .1600 :fm-index 1.9065)
	      (vln_one_sin_ran 150.2603 4 11.1107 .1600 :fm-index 2.2143)
	      (vln_one_sin_ran 150.2605 1.2000 21.0154 .1600 :fm-index 1.8011)
	      (vln_one_sin_ran 150.2605 2.8000 21.0154 .1600 :fm-index 2.1950)
	      (vln_one_sin_ran 150.2608 1.2000 21.0154 .1600 :fm-index 2.3563)
	      (vln_one_sin_ran 150.2608 1.6000 4.0878 .1600 :fm-index 2.3181)
	      (vln_one_sin_ran 150.2610 2 4.0878 .1600 :fm-index 2.0776)
	      (vln_one_sin_ran 150.2610 1.2000 4.0878 .1600 :fm-index 1.8336)
	      (vln_one_sin_ran 150.2613 4 4.0878 .1600 :fm-index 1.5019)
	      (vln_one_sin_ran 150.2615 1.2000 4.0878 .1600 :fm-index 2.2368)
	      (vln_one_sin_ran 150.2615 2.8000 4.0878 .1600 :fm-index 1.7462)
	      (vln_one_sin_ran 150.2620 1.2000 4.0878 .1600 :fm-index 1.9604)
	      (vln_one_sin_ran 150.2623 2 4.0878 .1600 :fm-index 2.2361)
	      (vln_one_sin_ran 150.2623 4 4.0878 .1600 :fm-index 1.9972)
	      (vln_one_sin_ran 150.2623 4 4.0878 .1600 :fm-index 2.4870)
	      (vln_one_sin_ran 150.2625 2 4.0878 .1600 :fm-index 2.0762)
	      (vln_one_sin_ran 150.2625 1 4.0878 .1600 :fm-index 2.2973)
	      (vln_one_sin_ran 150.2630 2 4.0878 .1600 :fm-index 2.2350)
	      (vln_one_sin_ran 150.2633 2 4.0878 .1600 :fm-index 2.1613)
	      (vln_one_sin_ran 150.2635 1.2000 4.0878 .1600 :fm-index 2.0640)
	      (vln_one_sin_ran 150.2635 2.8000 4.0878 .1600 :fm-index 2.1738)
	      (vln_one_sin_ran 150.2640 .8000 4.0878 .1600 :fm-index 1.5188)
	      (vln_one_sin_ran 150.2643 2 4.0878 .1600 :fm-index 1.8766)
	      (vln_one_sin_ran 150.2643 4 4.0878 .1600 :fm-index 2.3083)
	      (vln_one_sin_ran 150.2643 4 4.0878 .1600 :fm-index 2.2215)

	      (current-score-time 154)
	      (vln_one_sin_ran 154.2600 1.6000 66.5893 .1600 :fm-index 1.7041)
	      (vln_one_sin_ran 154.2600 2 66.4564 .1600 :fm-index 2.0296)
	      (vln_one_sin_ran 154.2603 1.2000 66.7225 .1600 :fm-index 1.8321)
	      (vln_one_sin_ran 154.2603 4 66.3237 .1600 :fm-index 2.1550)
	      (vln_one_sin_ran 154.2605 1.2000 125.4490 .1600 :fm-index 2.1806)
	      (vln_one_sin_ran 154.2605 2.8000 125.6999 .1600 :fm-index 2.3570)
	      (vln_one_sin_ran 154.2608 1.2000 125.1986 .1600 :fm-index 1.9861)
	      (vln_one_sin_ran 154.2608 1.6000 24.4994 .1600 :fm-index 1.6412)
	      (vln_one_sin_ran 154.2610 2 24.4505 .1600 :fm-index 1.9770)
	      (vln_one_sin_ran 154.2610 1.2000 24.5484 .1600 :fm-index 2.0103)
	      (vln_one_sin_ran 154.2613 4 24.4017 .1600 :fm-index 2.0663)
	      (vln_one_sin_ran 154.2615 1.2000 24.4017 .1600 :fm-index 2.1521)
	      (vln_one_sin_ran 154.2615 2.8000 24.4505 .1600 :fm-index 2.4453)
	      (vln_one_sin_ran 154.2620 1.2000 24.3530 .1600 :fm-index 2.0930)
	      (vln_one_sin_ran 154.2623 2 24.6960 .1600 :fm-index 2.3423)
	      (vln_one_sin_ran 154.2623 4 24.7454 .1600 :fm-index 2.0856)
	      (vln_one_sin_ran 154.2623 4 24.7948 .1600 :fm-index 1.9570)
	      (vln_one_sin_ran 154.2625 2 24.4994 .1600 :fm-index 2.4642)
	      (vln_one_sin_ran 154.2625 1 24.4749 .1600 :fm-index 1.9901)
	      (vln_one_sin_ran 154.2630 2 24.5239 .1600 :fm-index 1.9972)
	      (vln_one_sin_ran 154.2633 2 24.4505 .1600 :fm-index 1.9148)
	      (vln_one_sin_ran 154.2635 1.2000 24.4505 .1600 :fm-index 1.9017)
	      (vln_one_sin_ran 154.2635 2.8000 24.4749 .1600 :fm-index 2.4958)
	      (vln_one_sin_ran 154.2640 .8000 24.4260 .1600 :fm-index 2.2518)
	      (vln_one_sin_ran 154.2643 2 24.5975 .1600 :fm-index 2.1120)
	      (vln_one_sin_ran 154.2643 4 24.6221 .1600 :fm-index 2.3154)
	      (vln_one_sin_ran 154.2643 4 24.6467 .1600 :fm-index 1.9240)

	      (set! fm-violin-fm1-rat 6.718)
	      (set! fm-violin-fm2-rat 4.414)
	      (set! fm-violin-fm3-rat 5.141)

	      (current-score-time 158)
	      (vln_one_sin_ran 158.2600 1.6000 164.5868 .1600 :fm-index 1.9587)
	      (vln_one_sin_ran 158.2600 2 164.5868 .1600 :fm-index 1.5071)
	      (vln_one_sin_ran 158.2603 1.2000 164.5868 .1600 :fm-index 1.7690)
	      (vln_one_sin_ran 158.2603 4 164.5868 .1600 :fm-index 1.7686)
	      (vln_one_sin_ran 158.2605 1.2000 125.9513 .1600 :fm-index 1.5702)
	      (vln_one_sin_ran 158.2605 2.8000 125.9513 .1600 :fm-index 2.1962)
	      (vln_one_sin_ran 158.2608 1.2000 125.9513 .1600 :fm-index 1.7701)
	      (vln_one_sin_ran 158.2608 1.6000 24.4994 .1600 :fm-index 2.1665)
	      (vln_one_sin_ran 158.2610 2 24.4994 .1600 :fm-index 1.9345)
	      (vln_one_sin_ran 158.2610 1.2000 24.4994 .1600 :fm-index 2.2037)
	      (vln_one_sin_ran 158.2613 4 24.4994 .1600 :fm-index 1.6826)
	      (vln_one_sin_ran 158.2615 1.2000 24.4994 .1600 :fm-index 1.5410)
	      (vln_one_sin_ran 158.2615 2.8000 24.4994 .1600 :fm-index 1.8293)
	      (vln_one_sin_ran 158.2620 1.2000 24.4994 .1600 :fm-index 2.1468)
	      (vln_one_sin_ran 158.2623 2 24.4994 .1600 :fm-index 2.0758)
	      (vln_one_sin_ran 158.2623 4 24.4994 .1600 :fm-index 2.4138)
	      (vln_one_sin_ran 158.2623 4 24.4994 .1600 :fm-index 1.8479)
	      (vln_one_sin_ran 158.2625 3 24.4994 .1600 :fm-index 2.4639)
	      (vln_one_sin_ran 158.2625 1.5000 24.4994 .1600 :fm-index 2.3995)
	      (vln_one_sin_ran 158.2630 3 24.4994 .1600 :fm-index 1.8609)
	      (vln_one_sin_ran 158.2633 3 24.4994 .1600 :fm-index 2.4506)
	      (vln_one_sin_ran 158.2635 1.2000 24.4994 .1600 :fm-index 2.1577)
	      (vln_one_sin_ran 158.2635 2.8000 24.4994 .1600 :fm-index 1.6663)
	      (vln_one_sin_ran 158.2640 .8000 24.4994 .1600 :fm-index 2.1166)
	      (vln_one_sin_ran 158.2643 2 24.4994 .1600 :fm-index 1.9362)
	      (vln_one_sin_ran 158.2643 4 24.4994 .1600 :fm-index 2.2052)
	      (vln_one_sin_ran 158.2643 4 24.4994 .1600 :fm-index 2.0102)

	      (restore-fm-violin-defaults)
	      (set! fm-violin-glissando-amount .8)
	      (set! fm-violin-reverb-amount .01)
	      (set! fm-violin-gliss-env whoosh)
	      (set! fm-violin-amp-env metalamp)
	      (set! fm-violin-fm1-rat 2.718)
	      (set! fm-violin-fm2-rat 1.141)
	      (set! fm-violin-fm3-rat 3.141)

	      (current-score-time 162)
	      (vln_one_sin 162.2600 .4 1046.4800 .1600 :fm-index 2.3870)
	      (vln_one_sin 162.2600 .5 1044.3912 .1600 :fm-index 2.4309)
	      (vln_one_sin 162.2603 .3 1048.5730 .1600 :fm-index 2.1500)
	      (vln_one_sin 162.2603 .5 1042.3066 .1600 :fm-index 1.7211)
	      (vln_one_sin 162.2610 .3 524.2865 .1600 :fm-index 2.1751)
	      (vln_one_sin 162.2620 .3 520.1131 .1600 :fm-index 1.5433)
	      (vln_one_sin 162.2623 .4 517.0048 .1600 :fm-index 2.4335)
	      (vln_one_sin 162.2625 .4 523.2400 .1600 :fm-index 2.2778)
	      (vln_one_sin 162.2635 .3 522.1951 .1600 :fm-index 1.9441)
	      (vln_one_sin 162.2643 .4 520.6316 .1600 :fm-index 2.4656)

	      (restore-fm-violin-defaults)
	      (current-score-time 166)
	      (vln_one_sin 166.1200 .4 2092.9600 .1600 :fm-index 3
		:reverb-amount 0 :amp-env metalamp 
		:fm2-rat 1.1410 :fm3-rat 3.1410 :fm1-rat 2.7180)
	      (vln_one_sin 166.1200 .5 2088.7820 .1600 :fm-index 3 
		:reverb-amount 0 :amp-env metalamp 
		:fm2-rat 1.1410 :fm3-rat 3.1410 :fm1-rat 2.7180)
	      (vln_one_sin 166.1200 .3 2097.1460 .1600 :fm-index 3 
		:reverb-amount 0 :amp-env metalamp 
		:fm2-rat 1.1410 :fm3-rat 3.1410 :fm1-rat 2.7180)
	      (vln_one_sin 166.1200 .5 2084.6130 .1600 :fm-index 3 
		:reverb-amount 0 :amp-env metalamp 
		:fm2-rat 1.1410 :fm3-rat 3.1410 :fm1-rat 2.7180)
	      (vln_one_sin 166.1210 .3 1048.5730 .1600 :fm-index 3 
		:reverb-amount 0 :amp-env metalamp 
		:fm2-rat 1.1410 :fm3-rat 3.1410 :fm1-rat 2.7180)
	      (vln_one_sin 166.1220 .3 1040.2260 .1600 :fm-index 3 
		:reverb-amount 0 :amp-env metalamp 
		:fm2-rat 1.1410 :fm3-rat 3.1410 :fm1-rat 2.7180)
	      (vln_one_sin 166.1220 .5 1034.0100 .1600 :fm-index 3 
		:reverb-amount 0 :amp-env metalamp 
		:fm2-rat 1.1410 :fm3-rat 3.1410 :fm1-rat 2.7180)
	      (vln_one_sin 166.1230 .5 1046.4800 .1600 :fm-index 3 
		:reverb-amount 0 :amp-env metalamp 
		:fm2-rat 1.1410 :fm3-rat 3.1410 :fm1-rat 2.7180)
	      (vln_one_sin 166.1240 .3 1044.3900 .1600 :fm-index 3 
		:reverb-amount 0 :amp-env metalamp 
		:fm2-rat 1.1410 :fm3-rat 3.1410 :fm1-rat 2.7180)

	      (set! fm-violin-fm1-rat 2.718)
	      (set! fm-violin-fm2-rat 4.414)
	      (set! fm-violin-fm3-rat 5.141)

	      (vln_one_sin 166.1240 .5 1041.2630 .1600 :fm-index 3 
		:reverb-amount 0 :amp-env metalamp 
		:fm2-rat 1.1410 :fm3-rat 3.1410 :fm1-rat 2.7180)
	      (current-score-time 168)
	      (vln_one_sin_ran 168.4880 1.1770 416.6072 .0110 :fm-index 1.1140
		:reverb-amount .1 :amp-env z1amp :noise-amount .0050)
	      (vln_one_sin_ran 168.5050 2.4900 859.5863 .0083 :fm-index .5890
		:reverb-amount .1 :amp-env z2amp :noise-amount .0050)
	      (vln_one_sin_ran 169.0590 1.0550 1758.0816 .0053 :fm-index 1.8640
		:reverb-amount .1 :amp-env z2amp :noise-amount .0050)
	      (vln_one_sin_ran 169.0930 1.8580 229.0566 .0110 :fm-index 1.9690
		:reverb-amount .1 :amp-env z2amp :noise-amount .0050)
	      (vln_one_sin_ran 169.3490 3.3680 479.1994 .0083 :fm-index 1.9970
		:reverb-amount .1 :amp-env z1amp :noise-amount .0050)
	      (vln_one_sin_ran 169.5010 3.0680 411.8241 .0110 :fm-index 1.5390
		:reverb-amount .1 :amp-env z2amp :noise-amount .0050)
	      (vln_one_sin_ran 169.5200 2.8290 984.8456 .0053 :fm-index .0560
		:reverb-amount .1 :amp-env z1amp :noise-amount .0050)
	      (vln_one_sin_ran 169.6100 .7040 1767.7444 .0053 :fm-index 1.2620
		:reverb-amount .1 :amp-env z2amp :noise-amount .0050)
	      (vln_one_sin_ran 169.8480 3.0510 859.7203 .0083 :fm-index 1.6080
		:reverb-amount .1 :amp-env z2amp :noise-amount .0050)
	      (vln_one_sin_ran 170.4880 3.2350 231.9431 .0110 :fm-index .9690
		:reverb-amount .1 :amp-env z1amp :noise-amount .0050)
	      (vln_one_sin_ran 170.5610 3.2810 475.2009 .0083 :fm-index .3740
		:reverb-amount .1 :amp-env z2amp :noise-amount .0050)
	      (vln_one_sin_ran 170.7970 2.8400 988.8375 .0053 :fm-index .4200
		:reverb-amount .1 :amp-env z2amp :noise-amount .0050)
	      (vln_one_sin_ran 171.0620 1.0210 411.7247 .0110 :fm-index .1370
		:reverb-amount .1 :amp-env z2amp :noise-amount .0050)
	      (vln_one_sin_ran 171.2130 1.1610 848.5959 .0083 :fm-index 1.3120
		:reverb-amount .1 :amp-env z2amp :noise-amount .0050)
	      (vln_one_sin_ran 171.4410 2.6160 390.0600 .0110 :fm-index 1.9030
		:reverb-amount .1 :amp-env z1amp :noise-amount .0050)
	      (vln_one_sin_ran 171.4490 .7000 802.3538 .0083 :fm-index 1.5940
		:reverb-amount .1 :amp-env z2amp :noise-amount .0050)
	      (vln_one_sin_ran 171.5270 2.5080 1773.9366 .0053 :fm-index 1.8030
		:reverb-amount .1 :amp-env z1amp :noise-amount .0050)
	      (vln_one_sin_ran 171.7820 2.7990 232.4344 .0110 :fm-index .0590
		:reverb-amount .1 :amp-env z2amp :noise-amount .0050)
	      (vln_one_sin_ran 171.7830 2.7660 1650.1434 .0053 :fm-index .4400
		:reverb-amount .1 :amp-env z2amp :noise-amount .0050)
	      (vln_one_sin_ran 171.7890 3.1560 475.7231 .0083 :fm-index .7370
		:reverb-amount .1 :amp-env z2amp :noise-amount .0050)
	      (vln_one_sin_ran 172.1540 2.1290 976.0237 .0053 :fm-index 1.2690
		:reverb-amount .1 :amp-env z2amp :noise-amount .0050)
	      (vln_one_sin_ran 172.4890 3.3650 390.0525 .0110 :fm-index 1.4580
		:reverb-amount .1 :amp-env z2amp :noise-amount .0050)
	      (vln_one_sin_ran 172.7450 1.5070 1665.9722 .0053 :fm-index 1.9330
		:reverb-amount .1 :amp-env z2amp :noise-amount .0050)
	      (vln_one_sin_ran 172.8320 1.4430 798.1238 .0083 :fm-index .8560
		:reverb-amount .1 :amp-env z1amp :noise-amount .0050)
	      (vln_one_sin_ran 172.9440 3.1560 229.0528 .0110 :fm-index 1.8300
		:reverb-amount .1 :amp-env z2amp :noise-amount .0050)
	      (vln_one_sin_ran 173.3930 1.1100 473.7225 .0083 :fm-index 1.6260
		:reverb-amount .1 :amp-env z1amp :noise-amount .0050)
	      (vln_one_sin_ran 173.6970 1.6170 988.7953 .0053 :fm-index .4230
		:reverb-amount .1 :amp-env z2amp :noise-amount .0050)
	      (vln_one_sin_ran 174.0620 1.3190 390.9769 .0110 :fm-index .4100
		:reverb-amount .1 :amp-env z2amp :noise-amount .0050)
	      (vln_one_sin_ran 174.0840 3.3660 804.6413 .0083 :fm-index 1.8760
		:reverb-amount .1 :amp-env z2amp :noise-amount .0050)
	      (vln_one_sin_ran 174.1740 2.7210 418.6819 .0110 :fm-index .0910
		:reverb-amount .1 :amp-env z2amp :noise-amount .0050)
	      (vln_one_sin_ran 174.5700 3.4460 845.4019 .0077 :fm-index .7660
		:reverb-amount .1 :amp-env z2amp :noise-amount .0050)
	      (vln_one_sin_ran 174.6440 1.1790 1656.5756 .0049 :fm-index .2960
		:reverb-amount .1 :amp-env z1amp :noise-amount .0050)
	      (vln_one_sin_ran 174.6600 2.8520 1758.9788 .0049 :fm-index .4520
		:reverb-amount .1 :amp-env z2amp :noise-amount .0050)
	      (vln_one_sin_ran 174.8270 1.8840 387.0009 .0099 :fm-index 1.3010
		:reverb-amount .1 :amp-env z1amp :noise-amount .0050)
	      (vln_one_sin_ran 174.8870 3.4040 796.7213 .0077 :fm-index 1.1820
		:reverb-amount .1 :amp-env z2amp :noise-amount .0050)
	      (vln_one_sin_ran 174.9640 3.3230 416.3916 .0099 :fm-index .6290
		:reverb-amount .1 :amp-env z2amp :noise-amount .0050)
	      (vln_one_sin_ran 175.1320 1.7050 1637.2303 .0049 :fm-index 1.0570
		:reverb-amount .1 :amp-env z1amp :noise-amount .0050)
	      (vln_one_sin_ran 175.1500 3.1250 1762.4906 .0049 :fm-index 1.3170
		:reverb-amount .1 :amp-env z2amp :noise-amount .0050)
	      (vln_one_sin_ran 175.3860 2.9670 852.0487 .0077 :fm-index 1.4790
		:reverb-amount .1 :amp-env z1amp :noise-amount .0050)
	      (vln_one_sin_ran 175.6670 .6780 413.7094 .0099 :fm-index .9470
		:reverb-amount .1 :amp-env z2amp :noise-amount .0050)
	      (vln_one_sin_ran 175.8780 2.7490 1749.7509 .0049 :fm-index .5040
		:reverb-amount .1 :amp-env z1amp :noise-amount .0050)
	      (vln_one_sin_ran 175.9730 .5990 848.1253 .0077 :fm-index 1.9380
		:reverb-amount .1 :amp-env z1amp :noise-amount .0050)
	      (vln_one_sin_ran 176.0880 3.3360 229.9144 .0099 :fm-index 1.3930
		:reverb-amount .1 :amp-env z2amp :noise-amount .0050)
	      (vln_one_sin_ran 176.1170 1.1300 984.0816 .0049 :fm-index .3560
		:reverb-amount .1 :amp-env z2amp :noise-amount .0050)
	      (vln_one_sin_ran 176.4640 1.7330 478.7184 .0077 :fm-index .2840
		:reverb-amount .1 :amp-env z2amp :noise-amount .0050)
	      (vln_one_sin_ran 176.5760 .5680 413.4253 .0099 :fm-index 1.5020
		:reverb-amount .1 :amp-env z2amp :noise-amount .0050)
	      (vln_one_sin_ran 176.8200 1.2150 230.9588 .0099 :fm-index 1.0990
		:reverb-amount .1 :amp-env z1amp :noise-amount .0050)
	      (vln_one_sin_ran 176.8320 3.4590 473.8903 .0077 :fm-index .7680
		:reverb-amount .1 :amp-env z2amp :noise-amount .0050)
	      (vln_one_sin_ran 176.8320 .7260 857.2875 .0077 :fm-index .7520
		:reverb-amount .1 :amp-env z2amp :noise-amount .0050)

	      (restore-fm-violin-defaults)

	      (current-score-time 180)
	      (violin 180.2600 .0500 80 .8000 :fm-index 5 :reverb-amount 0
		:amp-env ampfunc1 :fm1-env indfunc2)
	      (violin 181.2610 .2000 80 .8000 :fm-index 4 :reverb-amount 0
		:amp-env ampfunc :fm1-env indfunc :fm2-rat .6875)
	      (violin 182.2600 .0500 80 .8000 :fm-index 5 :reverb-amount 0
		:amp-env ampfunc1 :fm1-env indfunc2)
	      (violin 182.2620 .2000 80 .8000 :fm-index 5 :reverb-amount 0
		:amp-env ampfunc :fm1-env indfunc :fm2-rat .6875)
	      (violin 183.2600 .0500 80 .8000 :fm-index 6 :reverb-amount 0
		:amp-env ampfunc1 :fm1-env indfunc2)
	      (violin 183.2630 .2000 80 .8000 :fm-index 6 :reverb-amount 0
		:amp-env ampfunc :fm1-env indfunc :fm2-rat .6875)
	      (violin 184.2600 .0500 80 .3 :fm-index 4 :reverb-amount 0
		:amp-env ampfunc1 :fm1-env indfunc2)
	      (violin 184.2620 .1 160 .3 :fm-index 4 :reverb-amount 0
		:amp-env ampfunc :fm1-env indfunc :fm2-rat .6875)
	      (violin 184.2620 .2500 80 .8000 :fm-index 4 :reverb-amount 0
		:amp-env ampfunc :fm1-env indfunc :fm2-rat .6875)
	      (violin 185.2600 .0500 80 .5 :fm-index 4 :reverb-amount 0
		:amp-env ampfunc1 :fm1-env indfunc2)
	      (violin 185.2610 .1 210 .3 :fm-index 4 :reverb-amount 0
		:amp-env ampfunc :fm1-env indfunc :fm2-rat .6875)
	      (violin 185.2620 .2000 80 .1 :fm-index 4 :reverb-amount 0
		:amp-env ampfunc :fm1-env indfunc :fm2-rat .6875)
	      (violin 185.2630 .2500 320 .1 :fm-index 2 :reverb-amount 0
		:amp-env ampfunc :fm1-env indfunc :fm2-rat .6875)
	      (violin 186.2600 .0500 80 .8000 :fm-index 4 :reverb-amount 0
		:amp-env ampfunc1 :fm1-env indfunc2)
	      (violin 186.2610 .1 210 .1 :fm-index 2 :reverb-amount 0
		:amp-env ampfunc :fm1-env indfunc :fm2-rat .6875)
	      (violin 186.2620 .2000 80 .2000 :fm-index 4 :reverb-amount 0
		:amp-env ampfunc :fm1-env indfunc :fm2-rat .6875)
	      (violin 186.2630 .2500 320 .3 :reverb-amount 0 :amp-env ampfunc
		:fm1-env indfunc :fm2-rat .6875)
	      (violin 187.2600 .0500 80 .8000 :fm-index 2 :reverb-amount 0
		:amp-env ampfunc1 :fm1-env indfunc2)
	      (violin 187.2610 .1 210 .1 :fm-index 2 :reverb-amount 0
		:amp-env ampfunc :fm1-env indfunc :fm2-rat .6875)
	      (violin 187.2620 .2000 80 .2000 :fm-index 2 :reverb-amount 0
		:amp-env ampfunc :fm1-env indfunc :fm2-rat .6875)
	      (violin 187.2630 .2500 320 .3 :reverb-amount 0 :amp-env ampfunc
		:fm1-env indfunc :fm2-rat .6875)

	      (set! fm-violin-glissando-amount 0.0)
	      (set! fm-violin-noise-amount 0.004)
	      (set! fm-violin-fm1-rat 3.141)
	      (set! fm-violin-fm2-rat 4.414)
	      (set! fm-violin-fm3-rat 2.718)

	      (current-score-time 188)
	      (vln_one_sin_ran 188.2600 4 3286.9937 .1600 :fm-index 2.2165
		:reverb-amount .0100 :amp-env n_amp :fm1-env n_amp)
	      (vln_one_sin_ran 188.2603 4 1046.4800 .1600 :fm-index 2.3234
		:reverb-amount .0100 :amp-env n_amp :fm1-env n_amp)
	      (vln_one_sin_ran 188.2605 4 2844.3326 .1600 :fm-index 2.4790
		:reverb-amount .1 :amp-env n_amp :fm1-env n_amp)
	      (vln_one_sin_ran 188.2608 4 821.7484 .1 :fm-index 1.8667
		:reverb-amount .0100 :amp-env n_amp :fm1-env n_amp)
	      (vln_one_sin_ran 188.2610 4 261.6200 .1 :fm-index 1.8523
		:reverb-amount .0100 :amp-env n_amp :fm1-env n_amp)
	      (vln_one_sin_ran 188.2613 4 711.0832 .1 :fm-index 2.2300
		:reverb-amount .1 :amp-env n_amp :fm1-env n_amp)
	      (vln_one_sin_ran 188.2615 4 205.4371 .0600 :fm-index 1.5187
		:reverb-amount .0100 :amp-env n_amp :fm1-env n_amp)
	      (vln_one_sin_ran 188.2618 4 65.4050 .0600 :fm-index 2.4074
		:reverb-amount .0100 :amp-env n_amp :fm1-env n_amp)
	      (vln_one_sin_ran 188.2620 4 177.7708 .0600 :fm-index 2.4481
		:reverb-amount .1 :amp-env n_amp :fm1-env n_amp)
	      (vln_one_sin_ran 188.2623 4 51.3593 .0100 :fm-index 2.3069
		:reverb-amount .0100 :amp-env n_amp :fm1-env n_amp)
	      (vln_one_sin_ran 188.2625 4 16.3513 .0100 :fm-index 2.1008
		:reverb-amount .0100 :amp-env n_amp :fm1-env n_amp)
	      (vln_one_sin_ran 188.2628 4 44.4427 .0100 :fm-index 2.4860
		:reverb-amount .1 :amp-env n_amp :fm1-env n_amp)

	      (restore-fm-violin-defaults)
	      (current-score-time 196)
	      (vln_one_sin 196.2603 1.2000 88.8854 .1 :fm-index 2.3144 
		:reverb-amount .2000 :amp-env mamp 
		:fm2-rat 4.4140 :fm3-rat 5.1410 :fm1-rat 2.7180)
	      (vln_one_sin 196.2603 4 88.8854 .1 :fm-index 2.1690 
		:reverb-amount .2000 :amp-env mamp 
		:fm2-rat 4.4140 :fm3-rat 5.1410 :fm1-rat 2.7180)
	      (vln_one_sin 196.2605 2.8000 168.1236 .0500 :fm-index 2.1850 
		:reverb-amount .2000 :amp-env mamp 
		:fm2-rat 4.4140 :fm3-rat 5.1410 :fm1-rat 2.7180)
	      (vln_one_sin 196.2608 1.2000 168.1236 .0800 :fm-index 1.7743 
		:reverb-amount .2000 :amp-env mamp 
		:fm2-rat 4.4140 :fm3-rat 5.1410 :fm1-rat 2.7180)
	      (vln_one_sin 196.2610 2 32.7025 .1 :fm-index 2.4925 
		:reverb-amount .2000 :amp-env mamp 
		:fm2-rat 4.4140 :fm3-rat 5.1410 :fm1-rat 2.7180)
	      (vln_one_sin 196.2633 2 32.7025 .1 :fm-index 2.1325 
		:reverb-amount .2000 :amp-env mamp 
		:fm2-rat 4.4140 :fm3-rat 5.1410 :fm1-rat 2.7180)
	      (vln_one_sin 196.2643 4 32.7025 .0500 :fm-index 1.7578 
		:reverb-amount .2000 :amp-env mamp 
		:fm2-rat 4.4140 :fm3-rat 5.1410 :fm1-rat 2.7180)

	      (current-score-time 204)
	      (vln_one_sin_ran 204.2600 6.6830 244.8160 .0060 :fm-index 2
		:reverb-amount .2000 :noise-amount .0040)
	      (vln_one_sin_ran 204.2600 5.5170 495.4040 .0060 :fm-index 2
		:reverb-amount .2000 :noise-amount .0040)
	      (vln_one_sin_ran 204.2600 7.5350 980.6190 .0020 :fm-index 2
		:reverb-amount .2000 :noise-amount .0040)
	      (vln_one_sin_ran 204.2600 7.1990 1965.4290 .0020 :fm-index .8000
		:reverb-amount .2000 :noise-amount .0040)
	      (vln_one_sin_ran 204.2600 4.0790 3835.3170 .0020 :fm-index .8000
		:reverb-amount .2000 :noise-amount .0040)
	      (vln_one_sin_ran 204.5170 4.7400 1320.9670 .0020 :fm-index .8000
		:reverb-amount .2000 :noise-amount .0040)
	      (vln_one_sin_ran 204.7040 7.2080 655.5670 .0040 :fm-index 2
		:reverb-amount .2000 :noise-amount .0040)
	      (vln_one_sin_ran 205.0490 6.6530 169.4230 .0040 :fm-index 2
		:reverb-amount .2000 :noise-amount .0040)

	      (set! fm-violin-glissando-amount 0.0)
	      (set! fm-violin-reverb-amount .9)

	      (current-score-time 213)
	      (vln_one_sin_exp 213.5450 6.4650 366.3330 .0320 :fm-index 1.0480
		:amp-env
		'(0 0 1.5468 1 2.0882 .7000 2.3202 1 98.4532 .7500 100.0 0))
	      (vln_one_sin_exp 213.5950 8.4340 1172.5830 .0180 :fm-index 1.1350
		:amp-env
		'(0 0 1.1857 1.0 1.6007 .7000 1.7785 1 98.8143 .5556 100.0 0))
	      (vln_one_sin_exp 213.7650 1.6210 369.9940 .0170 :fm-index .0960
		:amp-env
		'(0 0 6.1690 1.0 8.3282 .7000 9.2535 1.0 93.8310 .5294 100.0 0))
	      (vln_one_sin_exp 213.8820 3.0640 246.9420 .0170 :fm-index .0020
		:amp-env
		'(0 0 3.2637 1 4.4060 .7000 4.8956 1.0 96.7363 .5294 100.0 0))
	      (vln_one_sin_exp 213.9250 3.1170 123.4710 .0380 :fm-index .2330
		:amp-env
		'(0 0 3.2082 1 4.3311 .7000 4.8123 1 96.7918 .7895 100.0 0))
	      (vln_one_sin_exp 213.9810 3.5670 123.4710 .0420 :fm-index .2330
		:amp-env
		'(0 0 2.8035 1 3.7847 .7 4.2052 1.0 97.1965 .8095 100.0 0))
	      (vln_one_sin_exp 214.1280 1.0450 246.9420 .0170 :fm-index 1.2050
		:amp-env
		'(0 0 9.5694 1 12.9187 .7000 14.3541 1 90.4306 .5294 100.0 0))
	      (vln_one_sin_exp 214.2550 3.3870 374.1370 .0170 :fm-index .1800
		:amp-env
		'(0 0 2.9525 1.0 3.9858 .7000 4.4287 1.0 97.0475 .5294 100.0 0))
	      (vln_one_sin_exp 214.2990 8.3050 1576.9120 .0200 :fm-index .2990
		:amp-env
		'(0 0 1.2041 1 1.6255 .7000 1.8061 1 98.7959 .6000 100.0 0))
	      (vln_one_sin_exp 214.3300 4.4630 246.9420 .0170 :fm-index .0020
		:amp-env
		'(0 0 2.2406 1 3.0249 .7 3.3610 1.0 97.7594 .5294 100.0 0))
	      (vln_one_sin_exp 214.6600 8.9940 1576.9120 .0200 :fm-index .2990
		:amp-env
		'(0 0 1.1119 1 1.5010 .7000 1.6678 1 98.8881 .6000 100.0 0))
	      (vln_one_sin_exp 214.9060 8.8360 1172.5830 .0180 :fm-index 1.1350
		:amp-env
		'(0 0 1.1317 1 1.5278 .7000 1.6976 1 98.8683 .5556 100.0 0))
	      (vln_one_sin_exp 215.1510 4.9320 374.1370 .0170 :fm-index .1800
		:amp-env
		'(0 0 2.0276 1 2.7372 .7000 3.0414 1 97.9724 .5294 100.0 0))
	      (vln_one_sin_exp 215.2720 2.3250 369.9940 .0170 :fm-index 1.1030
		:amp-env
		'(0 0 4.3011 1 5.8065 .7000 6.4516 1 95.6989 .5294 100.0 0))
	      (vln_one_sin_exp 216.6960 3.5540 366.3330 .0310 :fm-index 1.0480
		:amp-env
		'(0 0 2.8137 1 3.7985 .7000 4.2206 1 97.1863 .7419 100.0 0))
	      (vln_one_sin_exp 217.7240 .6040 246.9420 .0170 :fm-index 1.2050
		:amp-env
		'(0 0 16.5563 1 22.3510 .7000 24.8344 1 83.4437 .5294 100 0))
	      (vln_one_sin_exp 217.9420 2.5010 123.4710 .0330 :fm-index .2330
		:amp-env
		'(0 0 3.9984 1 5.3978 .7000 5.9976 1 96.0016 .7576 100.0 0))
	      (vln_one_sin_exp 218.0340 2.3860 246.9420 .0170 :fm-index .0020
		:amp-env
		'(0 0 4.1911 1 5.6580 .7000 6.2867 1 95.8089 .5294 100.0 0))
	      (vln_one_sin_exp 218.3850 1.4510 369.9940 .0170 :fm-index 1.1030
		:amp-env
		'(0 0 6.8918 1 9.3039 .7000 10.3377 1 93.1082 .5294 100.0 0))
	      (vln_one_sin_exp 218.5670 2.6550 374.1370 .0170 :fm-index .1800
		:amp-env
		'(0 0 3.7665 1 5.0847 .7000 5.6497 1 96.2335 .5294 100.0 0))
	      (vln_one_sin_exp 218.9830 2.9860 123.4710 .0380 :fm-index .2330
		:amp-env
		'(0 0 3.3490 1 4.5211 .7000 5.0234 1 96.6510 .7895 100.0 0))
	      (vln_one_sin_exp 219.4910 .6110 123.9770 .0170 :fm-index .7550
		:amp-env
		'(0 0 16.3666 1 22.0949 .7000 24.5499 1 83.6334 .5294 100.0 0))
	      (vln_one_sin_exp 219.7570 1.4440 123.4710 .0170 :fm-index .0020
		:amp-env
		'(0 0 6.9252 1 9.3490 .7000 10.3878 1 93.0748 .5294 100.0 0))
	      (vln_one_sin_exp 219.7750 .5370 92.4435 .0330 :fm-index .9200
		:amp-env
		'(0 0 18.6220 1 25.1397 .7000 27.9330 1 81.3780 .7576 100.0 0))
	      (vln_one_sin_exp 219.7750 10.5370 92.4435 .0130 :fm-index .9200
		:amp-env
		'(0 0 .9490 1 1.2812 .7000 1.4236 1 99.0510 .3846 100.0 0))
	      (vln_one_sin_exp 219.9380 .6520 122.2995 .0170 :fm-index 1.8380
		:amp-env
		'(0 0 15.3374 1 20.7055 .7 23.0061 1 84.6626 .5294 100.0 0))
	      (vln_one_sin_exp 220.2350 3.7250 586.2915 .0180 :fm-index 1.1350
		:amp-env 
		'(0 0 2.6846 1 3.6242 .7 4.0268 1 97.3154 .5556 100.0 0))
	      (vln_one_sin_exp 220.2560 2.8900 183.1665 .0260 :fm-index 1.0480
		:amp-env
		'(0 0 3.4602 1 4.6713 .7000 5.1903 1 96.5398 .6923 100.0 0))
	      (vln_one_sin_exp 220.2710 1.6210 187.0685 .0170 :fm-index .1800
		:amp-env
		'(0 0 6.1690 1.0 8.3282 .7 9.2535 1.0 93.8310 .5294 100.0 0))
	      (vln_one_sin_exp 220.2920 2.0160 183.1665 .0290 :fm-index 1.0480
		:amp-env
		'(0 0 4.9603 1 6.6964 .7000 7.4405 1 95.0397 .7241 100.0 0))
	      (vln_one_sin_exp 220.2920 12.0160 183.1665 .0290 :fm-index 1.0480
		:amp-env
		'(0 0 .8322 1 1.1235 .7000 1.2483 1.0000 99.1678 .7241 100.0 0))
	      (vln_one_sin_exp 220.3300 .7300 184.9970 .0170 :fm-index .0960
		:amp-env
		'(0 0 13.6986 1 18.4932 .7 20.5479 1.0 86.3014 .5294 100 0))
	      (vln_one_sin_exp 220.3570 1.9600 183.1665 .0280 :fm-index 1.0480
		:amp-env
		'(0 0 5.1020 1.0 6.8878 .7 7.6531 1.0 94.8980 .7143 100.0 0))
	      (vln_one_sin_exp 220.3820 2.2450 61.7355 .0330 :fm-index .2330
		:amp-env
		'(0 0 4.4543 1 6.0134 .7000 6.6815 1 95.5457 .7576 100.0 0))
	      (vln_one_sin_exp 220.3820 12.2450 61.7355 .0330 :fm-index .2330
		:amp-env
		'(0 0 .8167 1 1.1025 .7000 1.2250 1 99.1833 .7576 100.0 0))
	      (vln_one_sin_exp 220.5410 3.0130 246.5050 .0360 :fm-index 1.1350
		:amp-env
		'(0 0 3.3190 1.0 4.4806 .7 4.9784 1.0 96.6810 .7778 100.0 0))
	      (vln_one_sin_exp 220.5570 2.3220 1251.5960 .0400 :fm-index .2990
		:amp-env
		'(0 0 4.3066 1 5.8140 .7000 6.4599 1 95.6934 .8000 100.0 0))
	      (vln_one_sin_exp 220.5570 18.3220 1251.5960 .0200 :fm-index .2990
		:amp-env
		'(0 0 .5458 1.0000 .7368 .7000 .8187 1 99.4542 .6000 100.0 0))
	      (vln_one_sin 220.5600 13.8770 3951.1200 .0060 :fm-index .5 
		:amp-env updown)
	      (vln_one_sin 220.7600 17.8770 493.8900 .0170 :fm-index .5280 
		:amp-env updown)
	      (vln_one_sin_exp 221.1060 1.9900 183.1665 .0230 :fm-index 1.0480
		:amp-env
		'(0 0 5.0251 1.0 6.7839 .7 7.5377 1 94.9749 .6522 100.0 0))
	      (vln_one_sin 221.1600 13.8770 1975.5600 .0110 :fm-index 1.2000 
		:amp-env updown)
	      (vln_one_sin_exp 221.2570 1.9180 61.7355 .0330 :fm-index .2330
		:amp-env
		'(0 0 5.2138 1 7.0386 .7000 7.8206 1 94.7862 .7576 100.0 0))
	      (vln_one_sin 221.2600 15.8770 246.9450 .0170 :fm-index .5280 
		:amp-env updown)
	      (vln_one_sin_exp 221.6370 1.3090 183.1665 .0310 :fm-index 1.0480
		:amp-env
		'(0 0 7.6394 1 10.3132 .7000 11.4591 1 92.3606 .7419 100.0 0))
	      (vln_one_sin 221.8600 16.8770 987.7800 .0130 :fm-index 1.5000 
		:amp-env updown)
	      (vln_one_sin_exp 222.0330 1.1590 183.1665 .0250 :fm-index 1.0480
		:amp-env 
		'(0 0 8.6281 1 11.648 .7 12.9422 1.0 91.3719 .68 100.0 0))
	      (vln_one_sin 222.0600 13.8770 3951.1200 .0090 :amp-env updown)
	      (vln_one_sin_exp 222.0980 1.2400 30.8675 .0330 :fm-index .2330
		:amp-env
		'(0 0 8.0645 1 10.8871 .7000 12.0968 1 91.9355 .7576 100.0 0))
	      (vln_one_sin_exp 222.0980 11.2400 30.8675 .0130 :fm-index .2330
		:amp-env
		'(0 0 .8897 1 1.2011 .7000 1.3345 1 99.1103 .3846 100.0 0))
	      (vln_one_sin_exp 222.1260 .2600 123.4710 .0170 :fm-index 1.2050
		:amp-env
		'(0 0 38.4615 1 51.9231 .7000 57.6923 1 61.5385 .5294 100.0 0))
	      (vln_one_sin_exp 222.1260 10.2600 123.4710 .0170 :fm-index 1.2050
		:amp-env
		'(0 0 .9747 1 1.3158 .7000 1.4620 1 99.0253 .5294 100.0 0))
	      (vln_one_sin 222.2600 14.8770 123.4725 .0170 :fm-index 1.5000 
		:amp-env updown)
	      (vln_one_sin 222.2600 13.8770 61.7363 .0170 :fm-index 1.5000 
		:amp-env updown)
	      (vln_one_sin 222.2600 12.8770 30.8681 .0170 :fm-index 1.5000 
		:amp-env updown)
	      (vln_one_sin 222.2600 11.8770 15.4341 .0170 :fm-index 1.5000 
		:amp-env updown)

	      (restore-fm-violin-defaults)
	      (current-score-time 241)
	      (cel_one_sum 241.2620 .3906 440 .4500 :fm-index 1.2000 
		:reverb-amount .0013
		:amp-env 
		'(0 0 .7680 1 4.7774 .6000 9.7891 .3 24.8243 .1 100 0))
	      (cel_one_sum 241.2640 .5220 220 .4500 :fm-index 1.2000 
		:reverb-amount .0012
		:amp-env
		'(0 0 .5747 1.0000 4.5919 .6000 9.6134 .3 24.6778 .1 100 0))
	      (cel_one_sum 241.2660 1.5660 880 .4500 :fm-index 1.2000 
		:reverb-amount .0014
		:amp-env
		'(0 0 .1916 1.0000 4.2242 .6000 9.2651 .3 24.3876 .1 100.0 0))
	      (cel_one_sum 241.2680 1.5660 110 .4500 :fm-index 1.2000 
		:reverb-amount .0013
		:amp-env
		'(0 0 .1916 1.0000 4.2242 .6000 9.2651 .3 24.3876 .1 100.0 0))
	      (current-score-time 244)
	      (vln_one_sin 244.8600 .9000 733.3330 .1875
		:fm-index .2000 :distance 1 :reverb-amount .0012
		:amp-env
		'(0 0 .3333 1 4.3603 .6000 9.3939 .3 24.4950 .1 100.0 0))
	      (vln_one_sin 244.8600 .2250 550 .1875
		:fm-index .2000 :distance 1 :reverb-amount .0015
		:amp-env
		'(0 0 1.3333 1 5.3199 .6000 10.3030 .3 25.2525 .1 100 0))
	      (vln_one_sin 244.8600 .4500 586.6670 .3750
		:fm-index .2000 :distance 1 :reverb-amount .0013
		:amp-env 
		'(0 0 .6667 1 4.6801 .6000 9.6970 .3 24.7475 .1 100 0))
	      (vln_one_sin 244.9020 .9000 733.3330 .1875
		:fm-index .4 :distance 1 :reverb-amount .0013
		:amp-env
		'(0 0 .3333 1 4.3603 .6000 9.3939 .3 24.4950 .1 100.0 0))
	      (vln_one_sin 244.9020 .2250 550 .1875
		:fm-index .4 :distance 1 :reverb-amount .0010
		:amp-env
		'(0 0 1.3333 1 5.3199 .6000 10.3030 .3 25.2525 .1 100 0))
	      (vln_one_sin 244.9020 .4500 586.6670 .3750
		:fm-index .4 :distance 1 :reverb-amount .0015
		:amp-env 
		'(0 0 .6667 1 4.6801 .6000 9.6970 .3 24.7475 .1 100 0))
	      (vln_one_sin 244.9430 .9000 366.6670 .1875
		:fm-index .6000 :distance 1 :reverb-amount .0016
		:amp-env
		'(0 0 .3333 1 4.3603 .6000 9.3939 .3 24.4950 .1 100.0 0))
	      (vln_one_sin 244.9430 .2250 275 .1875
		:fm-index .6000 :distance 1 :reverb-amount .0015
		:amp-env
		'(0 0 1.3333 1 5.3199 .6000 10.3030 .3 25.2525 .1 100 0))
	      (vln_one_sin 244.9430 .4500 293.3340 .3750
		:fm-index .6000 :distance 1 :reverb-amount .0015
		:amp-env 
		'(0 0 .6667 1 4.6801 .6000 9.6970 .3 24.7475 .1 100 0))
	      (vln_one_sin 244.9850 .9000 733.3330 .1875
		:fm-index .8000 :distance 1 :reverb-amount .0010
		:amp-env
		'(0 0 .3333 1 4.3603 .6000 9.3939 .3 24.4950 .1 100.0 0))
	      (vln_one_sin 244.9850 .2250 550 .1875
		:fm-index .8000 :distance 1 :reverb-amount .0013
		:amp-env
		'(0 0 1.3333 1 5.3199 .6000 10.3030 .3 25.2525 .1 100 0))))

;; fmviolin.scm ends here
