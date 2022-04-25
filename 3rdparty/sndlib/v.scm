(provide 'snd-v.scm)

(if (provided? 'snd)
    (if (not (provided? 'snd-ws.scm)) (load "ws.scm"))
    (if (not (provided? 'sndlib-ws.scm)) (load "sndlib-ws.scm")))

(define default-index-env '(0 1  25 .4  75 .6  100 0))
(define default-amp-env '(0 0  25 1  75 1  100 0))

(definstrument (fm-violin startime dur frequency amplitude
	    (fm-index 1.0)
	    amp-env 
	    (periodic-vibrato-rate 5.0) 
	    (random-vibrato-rate 16.0)
	    (periodic-vibrato-amplitude 0.0025) 
	    (random-vibrato-amplitude 0.005)
	    (noise-amount 0.0) 
	    (noise-freq 1000.0)
	    (ind-noise-freq 10.0) 
	    (ind-noise-amount 0.0)
	    (amp-noise-freq 20.0) 
	    (amp-noise-amount 0.0)
	    (gliss-env '(0 0  100 0))
	    (glissando-amount 0.0) 
	    fm1-env 
	    fm2-env
	    fm3-env
	    (fm1-rat 1.0) 
	    (fm2-rat 3.0)	 
	    (fm3-rat 4.0)                    
	    fm1-index
	    fm2-index
	    fm3-index
	    degree
	    (distance 1.0)
	    (reverb-amount 0.01)
	    (base 1.0))

    "(fm-violin startime dur frequency amplitude
   (fm-index 1.0) (amp-env '(0 0  25 1  75 1  100 0)) 
   (periodic-vibrato-rate 5.0) (random-vibrato-rate 16.0) 
   (periodic-vibrato-amplitude 0.0025) (random-vibrato-amplitude 0.005) 
   (noise-amount 0.0) (noise-freq 1000.0) (ind-noise-freq 10.0) 
   (ind-noise-amount 0.0) (amp-noise-freq 20.0) 
   (amp-noise-amount 0.0) (gliss-env '(0 0  100 0)) 
   (glissando-amount 0.0) (fm1-env '(0 1  25 .4  75 .6  100 0)) 
   (fm2-env '(0 1  25 .4  75 .6  100 0)) (fm3-rat 4.0) 
   (fm3-env '(0 1  25 .4  75 .6  100 0)) (fm1-rat 1.0) 
   (fm2-rat 3.0) (fm1-index #f) (fm2-index #f) 
   (fm3-index #f) (degree #f) (distance 1.0) 
   (reverb-amount 0.01) (base 1.0)) 

  (with-sound () (fm-violin 0 1 440 .1))"

    (let ((beg (seconds->samples startime))
	  (end (seconds->samples (+ startime dur)))
	  (frq-scl (hz->radians frequency))
	  (logfreq (log frequency))
	  (sqrtfreq (sqrt frequency))
	  (maxdev (* (hz->radians frequency) fm-index)))
      (let ((index1 (or fm1-index (min pi (* maxdev (/ 5.0 logfreq)))))
	    (index2 (or fm2-index (min pi (* maxdev 3.0 (/ (- 8.5 logfreq) (+ 3.0 (* frequency .001)))))))
	    (index3 (or fm3-index (min pi (* maxdev (/ 4.0 sqrtfreq)))))
	    (easy-case (and (zero? noise-amount)
			    (equal? fm1-env fm2-env)
			    (equal? fm1-env fm3-env)
			    (= fm1-rat (floor fm1-rat))
			    (= fm2-rat (floor fm2-rat))
			    (= fm3-rat (floor fm3-rat))
			    (integer? (rationalize (/ fm2-rat fm1-rat))) ; might be 2=2 but 1=3 or whatever
			    (integer? (rationalize (/ fm3-rat fm1-rat))))))
	(let ((norm (if easy-case 1.0 index1)))
	  (let ((fmosc1  (if easy-case 
			     (make-polywave (* fm1-rat frequency) 
					    (list (floor fm1-rat) index1
						  (floor (/ fm2-rat fm1-rat)) index2
						  (floor (/ fm3-rat fm1-rat)) index3)
					    mus-chebyshev-second-kind)
			     (make-oscil (* fm1-rat frequency))))
		(indf1 (make-env (or fm1-env default-index-env) norm :duration dur))
		(indf2 (or easy-case (make-env (or fm2-env default-index-env) index2 :duration dur)))
		(indf3 (or easy-case (make-env (or fm3-env default-index-env) index3 :duration dur)))
		(frqf (make-env gliss-env (* glissando-amount frq-scl) :duration dur))
		(pervib (make-triangle-wave periodic-vibrato-rate (* periodic-vibrato-amplitude frq-scl)))
		(ranvib (make-rand-interp random-vibrato-rate (* random-vibrato-amplitude frq-scl)))
		(fm-noi (if (not (zero? noise-amount))
			    (make-rand noise-freq (* pi noise-amount))
			    #f))
		(ind-noi (if (and (not (zero? ind-noise-amount))
				  (not (zero? ind-noise-freq)))
			     (make-rand-interp ind-noise-freq ind-noise-amount)
			     #f))
		(amp-noi (if (and (not (zero? amp-noise-amount))
				  (not (zero? amp-noise-freq)))
			     (make-rand-interp amp-noise-freq amp-noise-amount)
			     #f))
		(carrier (make-oscil frequency))
		(fmosc2 (if (not easy-case) (make-oscil (* fm2-rat frequency))))
		(fmosc3 (if (not easy-case) (make-oscil (* fm3-rat frequency))))
		(ampf (make-env (or amp-env default-amp-env) :scaler amplitude :base base :duration dur))
		(locs (make-locsig (or degree (random 90.0)) distance reverb-amount)))
	  
	    (if (or (not easy-case) 
		    ind-noi 
		    amp-noi
		    fm-noi)
		(let ((fuzz 0.0)
		      (vib 0.0)
		      (anoi 1.0)
		      (inoi 1.0))
		  (if easy-case ; no fm-noi here
		      (do ((i beg (+ i 1)))
			  ((= i end))
			(if amp-noi 
			    (set! anoi (* (env ampf) (+ 1.0 (rand-interp amp-noi))))
			    (set! anoi (env ampf)))
			(if ind-noi 
			    (set! inoi (+ 1.0 (rand-interp ind-noi))))
			(set! vib (+ (env frqf) (triangle-wave pervib) (rand-interp ranvib)))
			(locsig locs i (* anoi (oscil carrier (+ vib (* inoi (env indf1) (polywave fmosc1 vib)))))))
		      
		      (if (or ind-noi amp-noi fm-noi)
			  (if (not (or ind-noi amp-noi))
			      (do ((i beg (+ i 1)))
				  ((= i end))
				(let ((fuzz (rand fm-noi))
				      (vib (+ (env frqf) (triangle-wave pervib) (rand-interp ranvib))))
				  (locsig locs i (* (env ampf) 
						    (oscil carrier 
							   (+ vib
							      (* (env indf1) (oscil fmosc1 (+ (* fm1-rat vib) fuzz)))
							      (* (env indf2) (oscil fmosc2 (+ (* fm2-rat vib) fuzz)))
							      (* (env indf3) (oscil fmosc3 (+ (* fm3-rat vib) fuzz)))))))))

			      (do ((i beg (+ i 1)))
				  ((= i end))
				(if fm-noi (set! fuzz (rand fm-noi)))
				(if amp-noi 
				    (set! anoi (* (env ampf) (+ 1.0 (rand-interp amp-noi))))
				    (set! anoi (env ampf)))
				(if ind-noi (set! inoi (+ 1.0 (rand-interp ind-noi))))
				(set! vib (+ (env frqf) (triangle-wave pervib) (rand-interp ranvib)))
				(locsig locs i (* anoi (oscil carrier 
							      (+ vib
								 (* inoi
								    (+ (* (env indf1) (oscil fmosc1 (+ (* fm1-rat vib) fuzz)))
								       (* (env indf2) (oscil fmosc2 (+ (* fm2-rat vib) fuzz)))
								       (* (env indf3) (oscil fmosc3 (+ (* fm3-rat vib) fuzz)))))))))))

			  (do ((i beg (+ i 1)))
			      ((= i end))
			    (let ((vib (+ (env frqf) (triangle-wave pervib) (rand-interp ranvib))))
			      (locsig locs i (* (env ampf)
						(oscil carrier (+ vib
								  (* (env indf1) (oscil fmosc1 (* fm1-rat vib)))
								  (* (env indf2) (oscil fmosc2 (* fm2-rat vib)))
								  (* (env indf3) (oscil fmosc3 (* fm3-rat vib))))))))))))
		(if (= (mus-scaler frqf) 0.0)
		    (do ((i beg (+ i 1)))
			((= i end))
		      (let ((vib (+ (triangle-wave pervib) (rand-interp ranvib))))
			(locsig locs i (* (env ampf) 
					  (oscil carrier (+ vib (* (env indf1) 
								   (polywave fmosc1 vib))))))))
		    (do ((i beg (+ i 1)))
			((= i end))
		      (let ((vib (+ (env frqf) (triangle-wave pervib) (rand-interp ranvib))))
			(locsig locs i (* (env ampf) 
					  (oscil carrier (+ vib (* (env indf1) 
								   (polywave fmosc1 vib)))))))))))))))


;; (with-sound (:statistics #t) (fm-violin 0 10 440 .1 :fm-index 2.0))
;; (with-sound (:statistics #t) (fm-violin 0 10 440 .1 :noise-amount .01))
;; (with-sound (:statistics #t) (fm-violin 0 10 440 .1 :ind-noise-amount .01))
;; (with-sound (:statistics #t) (fm-violin 0 10 440 .1 :fm1-rat 1.002))

