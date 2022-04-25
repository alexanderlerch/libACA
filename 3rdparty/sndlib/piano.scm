;;; CLM piano.ins (Scott Van Duyne) translated to Snd/Scheme

(provide 'snd-piano.scm)
(if (provided? 'snd)
    (if (not (provided? 'snd-ws.scm)) (load "ws.scm"))
    (if (not (provided? 'sndlib-ws.scm)) (load "sndlib-ws.scm")))

(if (not (provided? 'snd-env.scm)) (load "env.scm"))

;;; see generators.scm for the old scheme versions of one-pole-all-pass, pnoise, one-pole-swept, and expseg

(define number-of-stiffness-allpasses 8)
(define longitudinal-mode-cutoff-keynum 29)
(define longitudinal-mode-stiffness-coefficient -.5)
(define golden-mean .618)
(define loop-gain-env-t60 .05)
(define loop-gain-default .9999)
(define nstrings 3)
(define two-pi (* 2 pi))

;;keyNum indexed parameter tables
;;these should all be &key variable defaults for p instrument
(define default-loudPole-table '(36 .8 60 .85 84 .7 96 .6 108 .5))
(define default-softPole-table '(36 .93 60 .9 84 .9 96 .8 108 .8))
(define default-loudGain-table  '(21.000 0.700 36.000 0.700 48.000 0.700 60.000 0.650 72.000 0.650
					 84.000 0.650 87.006 0.681 88.070 0.444 90.653 0.606 95.515 0.731
					 99.770 0.775 101.897 0.794 104.024 0.800 105.695 0.806))
(define default-softGain-table '(21 .25 108 .25))
(define default-strikePosition-table '(21.000 0.140 23.884 0.139 36.000 0.128 56.756 0.129 57.765 0.130
					      59.000 0.130 60.000 0.128 61.000 0.128 62.000 0.129 66.128 0.129
					      69.000 0.128 72.000 0.128 73.000 0.128 79.000 0.128 80.000 0.128
					      96.000 0.128 99.000 0.128))

(define default-detuning2-table '(22.017 -0.090 23.744 -0.090 36.000 -0.080 48.055 -0.113 60.000 -0.135
					 67.264 -0.160 72.000 -0.200 84.054 -0.301 96.148 -0.383 108 -0.383))
(define default-detuning3-table '(21.435 0.027 23.317 0.043 36.000 0.030 48.000 0.030 60.000 0.030 72.000
					 0.020 83.984 0.034 96.000 0.034 99.766 0.034))
(define default-stiffnessCoefficient-table '(21.000 -0.920 24.000 -0.900 36.000 -0.700 48.000 -0.250 60.000
						    -0.100 75.179 -0.040 82.986 -0.040 92.240 -0.040 96.000 -0.040
						    99.000 .2 108.000 .5))

(define default-singleStringDecayRate-table '(21.678 -2.895 24.000 -3.000 36.000 -4.641 41.953 -5.867 48.173
						     -7.113 53.818 -8.016 59.693 -8.875 66.605 -9.434 73.056 -10.035
						     78.931 -10.293 84.000 -12.185))
(define default-singleStringZero-table '(21.000 -0.300 24.466 -0.117 28.763 -0.047 36.000 -0.030 48.000 -0.020
						60.000 -0.010 72.000 -0.010 84.000 -0.010 96.000 -0.010))
(define default-singleStringPole-table '(21.000 0 24.466 0 28.763 0 36.000 0 108 0))
(define default-releaseLoopGain-table '(21.643 0.739 24.000 0.800 36.000 0.880 48.000 0.910 60.000 0.940
					       72.000 0.965 84.000 0.987 88.99 .987 89.0 1.0 108 1.0))

(define default-DryTapFiltCoeft60-table '(36 .35 60 .25 108 .15))
(define default-DryTapFiltCoefTarget-table '(36 -.8 60 -.5 84 -.4 108 -.1))
(define default-DryTapFiltCoefCurrent-table '(0 0 200 0))
(define default-DryTapAmpt60-table '(36 .55 60 .5 108 .45))
(define default-sustainPedalLevel-table '(21.000 0.250 24.000 0.250 36.000 0.200 48.000 0.125 60.000 0.075
						 72.000 0.050 84.000 0.030 96.000 0.010 99.000 0.010))
(define default-pedalResonancePole-table '(20.841 0.534 21.794 0.518 33.222 0.386 45.127 0.148 55.445 -0.065
						  69.255 -0.409 82.905 -0.729 95.763 -0.869 106.398 -0.861))
(define default-pedalEnvelopet60-table '(21.0 7.5 108.0 7.5))
(define default-soundboardCutofft60-table '(21.0 .25 108.0 .25))
(define default-DryPedalResonanceFactor-table '(21.0 .5 108.0 .5))
(define default-unaCordaGain-table '(21 1.0  24 .4 29 .1 29.1 .95 108 .95))


(definstrument (p start (duration 1.0)
		  (keyNum 60.0)                    ; middleC=60: can use fractional part to detune
		  (strike-velocity 0.5)            ; corresponding normalized velocities (range: 0.0--1.0)
		  (pedal-down #f)                  ; set to #t for sustain pedal down...pedal-down-times not yet implemented
		  (release-time-margin 0.75)       ; extra compute time allowed beyond duration
		  (amp .5)                         ; amp scale of noise inputs...
		  
		  ;;slider controls
		  (detuningFactor 1.0)
		  (detuningFactor-table ())
		  (stiffnessFactor 1.0)
		  (stiffnessFactor-table ())
		  (pedalPresenceFactor .3)
		  (longitudinalMode 10.5)
		  (StrikePositionInvFac -0.9)
		  (singleStringDecayRateFactor 1.0)
		  
		  ;; parameter tables indexed by keyNum
		  ;;   you can override the loudPole-table by directly setting :loudPole to a value
		  loudPole
		  (loudPole-table default-loudPole-table)
		  softPole
		  (softPole-table default-softPole-table)
		  loudGain
		  (loudGain-table default-loudGain-table)
		  softGain
		  (softGain-table default-softGain-table)
		  
		  strikePosition (strikePosition-table default-strikePosition-table)
		  detuning2
		  (detuning2-table default-detuning2-table)
		  detuning3
		  (detuning3-table default-detuning3-table)
		  stiffnessCoefficient
		  (stiffnessCoefficient-table default-stiffnessCoefficient-table)
		  singleStringDecayRate
		  (singleStringDecayRate-table default-singleStringDecayRate-table)
		  singleStringZero
		  (singleStringZero-table default-singleStringZero-table)
		  singleStringPole
		  (singleStringPole-table default-singleStringPole-table)
		  
		  releaseLoopGain
		  (releaseLoopGain-table default-releaseLoopGain-table)
		  DryTapFiltCoeft60
		  (DryTapFiltCoeft60-table default-DryTapFiltCoeft60-table)
		  DryTapFiltCoefTarget
		  (DryTapFiltCoefTarget-table default-DryTapFiltCoefTarget-table)
		  DryTapFiltCoefCurrent
		  (DryTapFiltCoefCurrent-table default-DryTapFiltCoefCurrent-table)
		  DryTapAmpt60
		  (DryTapAmpt60-table default-DryTapAmpt60-table)
		  sustainPedalLevel
		  (sustainPedalLevel-table default-sustainPedalLevel-table)
		  pedalResonancePole
		  (pedalResonancePole-table default-pedalResonancePole-table)
		  pedalEnvelopet60
		  (pedalEnvelopet60-table default-pedalEnvelopet60-table)
		  soundboardCutofft60
		  (soundboardCutofft60-table default-soundboardCutofft60-table)
		  DryPedalResonanceFactor
		  (DryPedalResonanceFactor-table default-DryPedalResonanceFactor-table)
		  unaCordaGain
		  (unaCordaGain-table default-unaCordaGain-table))
  
  ;; converts t60 values to suitable :rate values for expseg
  (define (In-t60 t60) (- 1.0 (expt 0.001 (/ 1.0 t60 (mus-srate)))))
  
  (define (make-one-pole-one-zero a0 a1 b1)
    (list (make-one-zero a0 a1)
	  (make-one-pole 1.0 b1)))
  
  (define (apPhase a1 wT)
    (atan (* (- (* a1 a1) 1.0) 
	     (sin wT))
	  (+ (* 2.0 a1) 
	     (* (+ (* a1 a1) 1.0) 
		(cos wT)))))
  
  (define (opozPhase b0 b1 a1 wT)
    (let ((s (sin wT))
	  (c (cos wT)))
      (atan (- (* a1 s (+ b0 (* b1 c))) 
	       (* b1 s (+ 1 (* a1 c))))
	    (+ (* (+ b0 (* b1 c)) 
		  (+ 1 (* a1 c))) 
	       (* b1 s a1 s)))))
  
  (define (signum n)
    ;; in CL this returns 1.0 if n is float
    (if (positive? n) 1
	(if (zero? n) 0
	    -1)))

  (define (get-allpass-coef samp-frac wT)
    (let ((ta (tan (- (* samp-frac wT))))
	  (c (cos wT))
	  (s (sin wT)))
      (/ (+ (- ta) 
	    (* (signum ta)
	       (sqrt (* (+ 1 (* ta ta)) 
			(* s s))))) ; is this correct? it's in the original
	 (- (* c ta) s))))
  
  (define (apfloor len wT)
    (let* ((len-int (floor len))
	   (len-frac (- len len-int)))
      (if (< len-frac golden-mean)
	  (begin
	    (set! len-int (- len-int 1))
	    (set! len-frac (+ len-frac 1.0))))
      (if (and (< len-frac golden-mean)
	       (> len-int 0))
	  (begin
	    (set! len-int (- len-int 1))
	    (set! len-frac (+ len-frac 1.0))))
      (list len-int (get-allpass-coef len-frac wT))))
  
  (define (tune-piano frequency stiffnessCoefficient numAllpasses b0 b1 a1)
    (let* ((wT (/ (* frequency two-pi) (mus-srate)))
	   (len (/ (+ two-pi
		      (* numAllpasses
			 (apPhase stiffnessCoefficient wT))
		      (opozPhase (+ 1 (* 3 b0)) (+ a1 (* 3 b1)) a1 wT))
		   wT)))
      (apfloor len wT)))
  
  (let (;;look-up parameters in tables (or else use the override value)
	(loudPole (or loudPole (envelope-interp keyNum loudPole-table)))
	(softPole (or softPole (envelope-interp keyNum softPole-table)))
	(loudGain (or loudGain (envelope-interp keyNum loudGain-table)))
	(softGain (or softGain (envelope-interp keyNum softGain-table)))
	
	(strikePosition (or strikePosition (envelope-interp keyNum strikePosition-table)))
	(detuning2 (or detuning2 (envelope-interp keyNum detuning2-table)))
	(detuning3 (or detuning3 (envelope-interp keyNum detuning3-table)))
	(stiffnessCoefficient (or stiffnessCoefficient (envelope-interp keyNum stiffnessCoefficient-table)))
	(singleStringDecayRate-1 (or singleStringDecayRate (envelope-interp keyNum singleStringDecayRate-table)))
	
	(singleStringZero (or singleStringZero (envelope-interp keyNum singleStringZero-table)))
	(singleStringPole (or singleStringPole (envelope-interp keyNum singleStringPole-table)))
	
	(releaseLoopGain (or releaseLoopGain (envelope-interp keyNum releaseLoopGain-table)))
	(DryTapFiltCoeft60 (or DryTapFiltCoeft60 (envelope-interp keyNum DryTapFiltCoeft60-table)))
	(DryTapFiltCoefTarget (or DryTapFiltCoefTarget (envelope-interp keyNum DryTapFiltCoefTarget-table)))
	(DryTapFiltCoefCurrent (or DryTapFiltCoefCurrent (envelope-interp keyNum DryTapFiltCoefCurrent-table)))
	(DryTapAmpt60 (or DryTapAmpt60 (envelope-interp keyNum DryTapAmpt60-table)))
	(sustainPedalLevel (or sustainPedalLevel (envelope-interp keyNum sustainPedalLevel-table)))
	(pedalResonancePole (or pedalResonancePole (envelope-interp keyNum pedalResonancePole-table)))
	(pedalEnvelopet60 (or pedalEnvelopet60 (envelope-interp keyNum pedalEnvelopet60-table)))
	(soundboardCutofft60 (or soundboardCutofft60 (envelope-interp keyNum soundboardCutofft60-table)))
	(DryPedalResonanceFactor (or DryPedalResonanceFactor (envelope-interp keyNum DryPedalResonanceFactor-table)))
	(unaCordaGain (or unaCordaGain (envelope-interp keyNum unaCordaGain-table)))
	(detuningFactor (if (null? detuningFactor-table) (envelope-interp keyNum detuningFactor-table) detuningFactor))
	(stiffnessFactor (if (null? stiffnessFactor-table) (envelope-interp keyNum stiffnessFactor-table) stiffnessFactor))
	
	(dryTap-one-pole-one-zero-pair (make-one-pole-one-zero 1.0 0.0 0.0))
	(dryTap-one-pole-swept 0.0)
	(wetTap-one-pole-swept 0.0)
	
	(beg (seconds->samples start))
	(dur (seconds->samples duration))
	(freq (* 440.0 (expt 2.0 (/ (- keyNum 69.0) 12.0)))))
    
    (let((end (+ beg dur (seconds->samples release-time-margin)))
	 (release-time (+ beg dur))
	 (wT (/ (* two-pi freq) (mus-srate)))
	 ;;strike position comb filter delay length
	 (agraffe-len (/ (* (mus-srate) strikePosition) freq))
	 (singleStringDecayRate (* singleStringDecayRateFactor singleStringDecayRate-1)))
      
      (let (;;initialize soundboard impulse response elements
	    ;;initialize open-string resonance elements		
	    (wetTap-one-pole-one-zero-pair (make-one-pole-one-zero (- 1.0 (* (signum pedalResonancePole) pedalResonancePole)) 0.0 (- pedalResonancePole)))

	    (sb-cutoff-rate (In-t60 soundboardCutofft60))
	    
	    ;;initialize velocity-dependent piano hammer filter elements
	    (hammerPole (+ softPole (* (- loudPole softPole) strike-velocity)))
	    (hammerGain (+ softGain (* (- loudGain softGain) strike-velocity)))
	    
	    (vals (apfloor agraffe-len wT))
	    (attenuationPerPeriod (expt 10.0 (/ singleStringDecayRate freq 20.0))))

	(let ((dlen1 (car vals))
	      (apcoef1 (cadr vals))
	      
	      ;;compute coefficients for and initialize the coupling filter
	      ;;  taking L=g(1 - bz^-1)/(1-b), and computing Hb = -(1-L)/(2-L)
	      
	      (g attenuationPerPeriod)  ;;DC gain
	      (b singleStringZero)
	      (a singleStringPole)
	      
	      ;;determine string tunings (and longitudinal modes, if present)
	      (freq1 (if (<= keyNum longitudinal-mode-cutoff-keynum) (* freq longitudinalMode) freq))
	      (freq2 (+ freq (* detuning2 detuningFactor)))
	      (freq3 (+ freq (* detuning3 detuningFactor)))
	      
	      ;;scale stiffness coefficients, if desired
	      (stiffnessCoefficient (if (> stiffnessFactor 1.0)
					(- stiffnessCoefficient
					   (* (+ 1 stiffnessCoefficient)
					      (- stiffnessFactor 1)))
					(* stiffnessCoefficient stiffnessFactor))))
	  
	  (let ((ctemp (+ 1 (- b) g (- (* a g))
			  (* nstrings (+ 1 (- b) (- g) (* a g)))))
		(stiffnessCoefficientL (if (<= keyNum longitudinal-mode-cutoff-keynum)
					   longitudinal-mode-stiffness-coefficient
					   stiffnessCoefficient)))
		
	    (let ((cfb0 (/ (* 2 (+ -1 b g (- (* a g)))) ctemp))
		  (cfb1 (/ (* 2 (+ a (- (* a b)) (- (* b g)) (* a b g))) ctemp))
		  (cfa1 (/ (+ (- a) (* a b) (- (* b g)) (* a b g)
			      (* nstrings (+ (- a) (* a b) (* b g) (- (* a b g)))))
			   ctemp))
		  
		  (agraffe-delay1 (make-delay dlen1))
		  (agraffe-tuning-ap1 (make-one-pole-all-pass 1 apcoef1)))
	      
	      (let ((couplingFilter-pair (make-one-pole-one-zero cfb0 cfb1 cfa1))
		    ;;initialize the coupled-string elements
		    (vals1 (tune-piano freq1 stiffnessCoefficientL number-of-stiffness-allpasses cfb0 cfb1 cfa1))
		    (vals2 (tune-piano freq2 stiffnessCoefficient number-of-stiffness-allpasses cfb0 cfb1 cfa1))
		    (vals3 (tune-piano freq3 stiffnessCoefficient number-of-stiffness-allpasses cfb0 cfb1 cfa1)))

		(let ((delayLength1 (car vals1))
		      (tuningCoefficient1 (cadr vals1))
		      
		      (delayLength2 (car vals2))
		      (tuningCoefficient2 (cadr vals2))
		      
		      (delayLength3 (car vals3))
		      (tuningCoefficient3 (cadr vals3)))

		  (let ((dryTap0 (car dryTap-one-pole-one-zero-pair))
			(dryTap1 (cadr dryTap-one-pole-one-zero-pair))
			
			(wetTap0 (car wetTap-one-pole-one-zero-pair))
			(wetTap1 (cadr wetTap-one-pole-one-zero-pair))
			
			(op1 (make-one-pole (- 1.0 hammerPole) (- hammerPole)))
			(op2 (make-one-pole (- 1.0 hammerPole) (- hammerPole)))
			(op3 (make-one-pole (- 1.0 hammerPole) (- hammerPole)))
			(op4 (make-one-pole (- 1.0 hammerPole) (- hammerPole)))
			
			(cou0 (car couplingFilter-pair))
			(cou1 (cadr couplingFilter-pair))
			
			(string1-delay (make-delay (- delayLength1 1)))
			(string1-tuning-ap (make-one-pole-all-pass 1 tuningCoefficient1))
			(string1-stiffness-ap (make-one-pole-all-pass 8 stiffnessCoefficientL))
			
			(string2-delay (make-delay (- delayLength2 1)))
			(string2-tuning-ap (make-one-pole-all-pass 1 tuningCoefficient2))
			(string2-stiffness-ap (make-one-pole-all-pass 8 stiffnessCoefficient))
			
			(string3-delay (make-delay (- delayLength3 1)))
			(string3-tuning-ap (make-one-pole-all-pass 1 tuningCoefficient3))
			(string3-stiffness-ap (make-one-pole-all-pass 8 stiffnessCoefficient))
			
			;;initialize loop-gain envelope
			(loop-gain loop-gain-default) 
			(loop-gain-ry (* releaseLoopGain (In-t60 loop-gain-env-t60)))
			(loop-gain-rx (- 1.0 (In-t60 loop-gain-env-t60)))

			(dry-coef DryTapFiltCoefCurrent)
			(dry-coef-ry (* DryTapFiltCoefTarget (In-t60 DryTapFiltCoeft60)))
			(dry-coef-rx (- 1.0 (In-t60 DryTapFiltCoeft60)))

			(wet-coef 0.0)
			(wet-coef-ry (* -0.5 (In-t60 pedalEnvelopet60)))
			(wet-coef-rx (- 1.0 (In-t60 pedalEnvelopet60)))

			(dryTap 0.0)
			(dryTap-x 1.0)
			(dryTap-rx (- 1.0 (In-t60 DryTapAmpt60)))

			(openStrings 0.0)
			(wetTap-x (* sustainPedalLevel pedalPresenceFactor (if pedal-down 1.0 DryPedalResonanceFactor)))
			(wetTap-rx (- 1.0 (In-t60 pedalEnvelopet60)))

			(combedExcitationSignal 0.0)
			(adelOut 0.0)
			(adelIn 0.0)
			(totalTap 0.0)
			(is-release-time #f)
			(string1-junction-input 0.0)
			(string2-junction-input 0.0)
			(string3-junction-input 0.0)
			(couplingFilter-input 0.0)
			(couplingFilter-output 0.0)
			(temp1 0.0)
			(noi 0.0)
			(pn-gen 16383)
			)

		    (do ((i beg (+ i 1)))
			((= i end))
		      
		      (if is-release-time
			  (set! loop-gain (+ loop-gain-ry (* loop-gain-rx loop-gain)))
			  (if (= i release-time)
			      (begin
				(set! is-release-time #t)
				(set! dryTap-rx (- 1.0 sb-cutoff-rate))
				(set! wetTap-rx dryTap-rx))))
		      
		      (set! pn-gen (logand (+ (* pn-gen 1103515245) 12345) #xffffffff))
		      (set! noi (* amp (- (* pn-gen 4.6566128730774e-10) 1.0)))

		      (set! temp1 (one-zero dryTap0 (one-pole dryTap1 noi)))
		      (set! dry-coef (+ dry-coef-ry (* dry-coef-rx dry-coef)))
		      (set! dryTap-one-pole-swept (- (* (+ 1.0 dry-coef) temp1) (* dry-coef dryTap-one-pole-swept)))
		      (set! dryTap-x (* dryTap-x dryTap-rx))
		      (set! dryTap (* dryTap-x dryTap-one-pole-swept))
						      
		      (set! pn-gen (logand (+ (* pn-gen 1103515245) 12345) #xffffffff))
		      (set! noi (* amp (- (* pn-gen 4.6566128730774e-10) 1.0)))

		      (set! temp1 (one-zero wetTap0 (one-pole wetTap1 noi)))
		      (set! wet-coef (+ wet-coef-ry (* wet-coef-rx wet-coef)))
		      (set! wetTap-one-pole-swept (- (* (+ 1.0 wet-coef) temp1) (* wet-coef wetTap-one-pole-swept)))
		      (set! wetTap-x (* wetTap-x wetTap-rx))
		      (set! openStrings (* wetTap-x wetTap-one-pole-swept))
							   
		      (set! totalTap (+ dryTap openStrings))
		      
		      (set! adelIn (one-pole op1 (one-pole op2 (one-pole op3 (one-pole op4 totalTap)))))
		      (set! combedExcitationSignal (* hammerGain (+ adelOut (* adelIn StrikePositionInvFac))))
		      (set! noi (delay agraffe-delay1 adelIn))
		      (set! adelOut (one-pole-all-pass agraffe-tuning-ap1 noi))
		      
		      (set! string1-junction-input (+ string1-junction-input couplingFilter-output))
		      (set! string1-junction-input (one-pole-all-pass string1-stiffness-ap string1-junction-input))
		      (set! string1-junction-input (+ (* unaCordaGain combedExcitationSignal)
						      (* loop-gain
							 (delay string1-delay
								(one-pole-all-pass string1-tuning-ap string1-junction-input)))))
		      
		      (set! string2-junction-input (+ string2-junction-input couplingFilter-output))
		      (set! string2-junction-input (one-pole-all-pass string2-stiffness-ap string2-junction-input))
		      (set! string2-junction-input (+ combedExcitationSignal
						      (* loop-gain 
							 (delay string2-delay
								(one-pole-all-pass string2-tuning-ap string2-junction-input)))))
		      
		      (set! string3-junction-input (+ string3-junction-input couplingFilter-output))
		      (set! string3-junction-input (one-pole-all-pass string3-stiffness-ap string3-junction-input))
		      (set! string3-junction-input (+ combedExcitationSignal
						      (* loop-gain
							 (delay string3-delay
								(one-pole-all-pass string3-tuning-ap string3-junction-input)))))
		      
		      (set! couplingFilter-input (+ string1-junction-input string2-junction-input string3-junction-input))
		      (set! couplingFilter-output (one-zero cou0 (one-pole cou1 couplingFilter-input)))
		      
		      (outa i couplingFilter-input))))))))))))

#|

(with-sound ()
	    (do ((i 0 (+ i 1))) ((= i 8))
	      (p
	       (* i .5)
	       :duration .5
	       :keyNum (+ 24 (* 12 i))
	       :strike-velocity .5
					;0 to 1, 0 is softest played note, 1 is loud note
	       :amp .4
					;overall volume level
	       :DryPedalResonanceFactor .25
					;0 no open string resonance
					;1.0 is about full resonance of dampers raised
					;can be greater than 1.0
	       )))


(with-sound ()
	    (do ((i 0 (+ i 1))) ((= i 8))
	      (p
	       (* i .5)
	       :duration .5
	       :keyNum (+ 24 (* 12 i))
	       :strike-velocity .5
					;0 to 1, 0 is softest played note, 1 is loud note
	       :amp .4
					;overall volume level
	       :DryPedalResonanceFactor .25
					;0 no open string resonance
					;1.0 is about full resonance of dampers raised
					;can be greater than 1.0
	       
	       ;;modification to do detunedness
	       :detuningFactor-table '(24 5 36 7.0 48 7.5 60 12.0 72 20
					  84 30 96 100 108 300)
					;scales the above detuning values
					;  so 1.0 is nominal detuning
					;  0.0 is exactly in tune (no two stage decay...)
					;  > 1.0 is out of tune...
	       
	       ;;modification to do stiffness
	       :stiffnessFactor-table '(21 1.5 24 1.5 36 1.5 48 1.5 60 1.4
					   72 1.3 84 1.2 96 1.0 108 1.0)
					;0.0 to 1.0 is less stiff, 1.0 to 2.0 is more stiff...
	       )))


(with-sound ()
	    (do ((i 0 (+ i 1))) ((= i 8))
	      (p
	       (* i .5)
	       :duration .5
	       :keyNum (+ 24 (* 12 i))
	       :strike-velocity .5
					;0 to 1, 0 is softest played note, 1 is loud note
	       :amp .4
					;overall volume level
	       :DryPedalResonanceFactor .25
					;0 no open string resonance
					;1.0 is about full resonance of dampers raised
					;can be greater than 1.0
	       
	       ;;modifications to do damped sounds
	       :singleStringDecayRate-table '(21 -5 24.000 -5.000 36.000 -5.4
						 41.953 -5.867 48.173 -7.113 53.818 -8.016
						 59.693 -8.875 66.605 -9.434 73.056 -10.035
						 78.931 -10.293 84.000 -12.185)
	       :singleStringPole-table '(21 .8 24 0.7  36.000 .6 48 .5 60 .3
					    84 .1 96 .03 108 .03)
	       :stiffnessCoefficient-table '(21.000 -0.920 24.000 -0.900 36.000 -0.700
						    48.000 -0.250 60.000 -0.100 75.179 -0.040
						    82.986 -0.040 92.240 .3 96.000 .5
						    99.000 .7 108.000 .7)
					;these are the actual allpass coefficients modified here
					;to allow dampedness at high freqs
	       )))

(let ((i 5))
  (with-sound ()
	      (p
	       0
	       :duration 5
	       :keyNum (+ 24 (* 12 i))
	       :strike-velocity .5
					;0 to 1, 0 is softest played note, 1 is loud note
	       :amp .4
					;overall volume level
	       :DryPedalResonanceFactor .25
					;0 no open string resonance
					;1.0 is about full resonance of dampers raised
					;can be greater than 1.0
	       
	       ;;modification for long duration notes
	       :singleStringDecayRateFactor 1/10
					;scales attenuation rate (1/2 means twice as long duration)
	       )))
|#

