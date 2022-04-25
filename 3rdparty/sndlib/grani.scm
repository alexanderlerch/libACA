;;; *************************
;;;    ENVELOPES (env.scm)
;;; *************************

;;;=============================================================================
;;; Exponential envelopes
;;;=============================================================================

;;; Approximate an exponential envelope with a given base and error bound
;;; by Fernando Lopez-Lezcano (nando@ccrma.stanford.edu)
;;;
;;; base:
;;;   step size of the exponential envelope
;;; error:
;;;   error band of the approximation
;;; scaler:
;;;   scaling factor for the y coordinates
;;; offset:
;;;   offset for the y coordinates
;;; cutoff:
;;;   lowest value of the exponentially rendered envelope, values lower than
;;;   this cutoff value will be approximated as cero.
;;; out-scaler
;;;   scaler for the converted values

(provide 'snd-grani.scm)

(if (provided? 'snd)
    (if (not (provided? 'snd-ws.scm)) (load "ws.scm"))
    (if (not (provided? 'sndlib-ws.scm)) (load "sndlib-ws.scm")))
(if (not (provided? 'snd-env.scm)) (load "env.scm"))

(define grani-default-base (expt 2 1/12))

(define* (exp-envelope env1
		       (base grani-default-base)
		       (error 0.01)
		       (scaler 1)
		       (offset 0)
		       (cutoff #f)
		       (out-scaler 1))
  (let* ((base (exact->inexact base))
	 (error (exact->inexact error))
	 (scaler (exact->inexact scaler))
	 (offset (exact->inexact offset))
	 (out-scaler (exact->inexact out-scaler))
	 (ycutoff (and cutoff (expt base (+ offset (* cutoff scaler)))))
	 (result ()))
    ;; linear interpolation
    (letrec ((interpolate (lambda (xl yl xh yh xi) (+ yl (* (- xi xl) (/ (- yh yl) (- xh xl))))))
	     ;;
	     ;; recursively render one segment
	     ;;   xl,xh   = x coordinates of segment ends
	     ;;   yl,yh   = y coordinates of segment ends
	     ;;   yle,yhe = exponential values of y coords of segment ends
	     ;;   error   = linear domain error bound for rendering
	     ;;
	     (exp-seg (lambda (xl yle xh yhe yl yh error)
			(let* ((xint (/ (+ xl xh) 2.0))
			       (yint (interpolate xl yl xh yh xint))
			       (yinte (interpolate xl yle xh yhe xint))
			       (yexp (expt base yint))
			       (yerr (- (expt base (+ yint error)) yexp)))
			  ;; is the linear approximation accurate enough?
			  ;; are we still over the cutoff limit?
			  (if (and (> (abs (- yexp yinte)) yerr)
				   (if ycutoff (> yinte ycutoff) #t))
			      ;; no --> add a breakpoint and recurse right and left
			      (call-with-values
				  (lambda () (exp-seg xl yle xint yexp yl yint error))
				(lambda (xi yi)
				  (call-with-values
				      (lambda () (exp-seg xint yexp xh yhe yint yh error))
				    (lambda (xj yj)
				      (values (append xi (list xint) xj)
					      (append yi (list yexp) yj))))))
			      ;; yes --> don't need to add nu'ting to the envelope
			      (values () ()))))))
      ;; loop for each segment in the envelope
      (let segs ((en env1))
	(let* ((x (car en))
	       (y (exact->inexact (cadr en)))
	       (nx (caddr en))
	       (ny (exact->inexact (cadddr en)))
	       (yscl (+ offset (* y scaler)))
	       (nyscl (+ offset (* ny scaler)))
	       (xy (list x (if (or (not ycutoff)
				   (>= (expt base yscl) ycutoff))
			       (* out-scaler (expt base yscl))
			       0.0))))
	  (set! result (append result xy))
	  (call-with-values
	      (lambda () (exp-seg x (expt base yscl) nx (expt base nyscl) yscl nyscl error))
	    (lambda (xs ys)
	      (if (not (null? ys))
		  (let ((ys-scaled (map (lambda (y) (* y out-scaler)) ys)))
		    (let vals ((xx xs) 
			       (yy ys-scaled))
		      (let ((x (car xx))
			    (y (car yy)))
			(set! result (append result (list x y)))
			(if (> (length xx) 1)
			    (vals (cdr xx) (cdr yy)))))))))
	  (if (<= (length en) 4)
	      (append result (list nx (if (or (not ycutoff)
					      (>= (expt base nyscl) ycutoff))
					  (* out-scaler (expt base nyscl))
					  0.0)))
	      (segs (cddr en))))))))

;;; Amplitude envelope in dBs
;;;
;;; The db scale is defined as:
;;;    value(db)=(* 20 (log10 (/ vin vref)))
;;;  where:
;;;    vref=1.0 reference value = digital clipping

(define* (db-envelope envelope (cutoff -70) (error 0.01))
  (exp-envelope envelope
		:base 10
		:scaler 1/20
		:offset 0
		:cutoff cutoff
		:error error))

(define* (make-db-env envelope
		      (scaler 1)
		      (offset 0)
		      (base 1)
		      (duration 0)
		      (end 0)
		      (cutoff -70)
		      (error 0.01))
  (make-env (db-envelope envelope cutoff error)
	    :scaler scaler :offset offset
	    :base base :duration duration :length (+ 1 end)))

;;; Pitch envelopes (y units are semitone and octave intervals)

(define* (semitones-envelope envelope (around 1.0) (error 0.01))
  (exp-envelope envelope
		:error error
		:base (expt 2 1/12)
		:cutoff #f
		:scaler 1
		:offset 0
		:out-scaler around))

(define* (make-semitones-env envelope
			     (around 1.0)
			     (scaler 1.0)
			     (offset 0.0)
			     (base 1)
			     (duration 0)
			     (end 0)
			     (error 0.01))
  (make-env (semitones-envelope envelope around error)
	    :scaler scaler :offset offset
	    :base base :duration duration :length (+ 1 end)))

(define* (octaves-envelope envelope (around 1.0) (error 0.01))
  (exp-envelope envelope
		:error error
		:base 2
		:cutoff #f
		:scaler 1
		:offset 0
		:out-scaler around))

(define* (make-octaves-env envelope
			   (around 1.0)
			   (scaler 1.0)
			   (offset 0.0)
			   (base 1)
			   (duration 0)
			   (end 0)
			   (error 0.01))
  (make-env (octaves-envelope envelope around error)
	    :scaler scaler :offset offset
	    :base base :duration duration :length (+ 1 end)))


;;; *************************
;;;    GRANI (clm-ins.scm)
;;; *************************

;;; grani: a granular synthesis instrument
;;;   by Fernando Lopez-Lezcano
;;;   http://ccrma.stanford.edu/~nando/clm/grani/
;;;
;;;   Original grani.ins instrument written for the 220a Course by
;;;   Fernando Lopez-Lezcano & Juan Pampin, November 6 1996
;;;
;;; Mar 21 1997: working with hop and grain-dur envelopes
;;; Mar 22 1997: working with src envelope (grain wise) & src spread
;;; Jan 26 1998: started work on new version
;;; Nov  7 1998: input soundfile duration calculation wrong
;;; Nov 10 1998: bug in in-samples (thanks to Kristopher D. Giesing for this one)
;;; Dec 20 1998: added standard locsig code
;;; Feb 19 1999: added "nil" as default value of where to avoid warning (by bill)
;;; Jan 10 2000: added input-channel to select which channel of the input file 
;;;              to process.
;;;              added grain-start-in-seconds to be able to specify input file
;;;              locations in seconds for the grain-start envelope
;;; May 06 2002: fixed array passing of where-bins in clisp (reported by Charles
;;;              Nichols and jennifer l doering
;;; Mar 27 2003: added option for sending grains to all channels (requested by
;;;              Oded Ben-Tal)
;;; Jun 17 2006: made some changes for the run macro (Bill)
;;; Jul 14 2007: removed :start args (Bill)
;;;-----------------------------------------------------------------------------
;;; Auxiliary functions

;;; calculate a random spread around a center of 0

(define-macro (random-spread spread)
  `(if (not (zero? ,spread))
       (- (random ,spread)
	  (/ ,spread 2.0))
       0.0))

;;; convert a time in seconds to a number of samples

(define-macro (to-samples time srate)
  `(floor (* ,time ,srate)))

;;; create a constant envelope if argument is a number

(define (envelope-or-number in)
  (if (number? in)
      (list 0 in 1 in)
      in))

;;; create a vct from an envelope

(define* (make-gr-env env1 (len 512))
  (let ((env-vct (make-vct len))
	(length-1 (exact->inexact (- len 1))))
    (do ((i 0 (+ 1 i)))
	((= i len) env-vct)
      (set! (env-vct i) (envelope-interp (/ i length-1) env1)))))

;;;-----------------------------------------------------------------------------
;;; Grain envelopes

(define* (raised-cosine	(duty-cycle 100)
			(len 128))
  (let* ((v (make-vct len))
	 (active (* len duty-cycle 0.01))
	 (incr (/ pi (- active 1)))
	 (start (/ (- len active) 2))
	 (end (/ (+ len active) 2))
	 (s 0))
    (do ((i 0 (+ 1 i)))
	((= i len) v)
      (if (and (>= i start) (< i end))
	  (let ((sine (sin (* s incr))))
	    (set! s (+ 1 s))
	    (set! (v i) (* sine sine)))))))

;;;=============================================================================
;;; Granular synthesis instrument
;;;=============================================================================

;;; input-channel:
;;;   from which channel in the input file are samples read
;;; amp-envelope:
;;;   amplitude envelope for the note
;;; grain-envelope:
;;; grain-envelope-end:
;;;   envelopes for each individual grain. The envelope applied in the result
;;;   of interpolating both envelopes. The interpolation is controlled by
;;;   grain-envelope-trasition. If "grain-envelope-end" is nil interpolation
;;;   is turned off and only grain-envelope is applied to the grains. 
;;; grain-envelope-trasition:
;;;   an enveloper that controls the interpolation between the two grain envelopes
;;;   0 -> selects "grain-envelope"
;;;   1 -> selects "grain-envelope-end"
;;; grain-envelope-array-size
;;;   size of the array passed to make-table-lookup
;;; grain-duration:
;;;   envelope that controls grain duration in seconds
;;; srate-linear:
;;;   #t -> sample rate envelope is linear
;;;   #f -> sample rate envelope is exponential
;;; srate:
;;;   envelope that controls sample rate conversion. The envelope is an
;;;   exponential envelope, the base and error bound of the conversion
;;;   are controlled by "srate-base" and "srate-error".
;;; srate-spread:
;;;   random spread of sample rate conversion around "srate"
;;; srate-base:
;;;   base for the exponential conversion
;;;   for example: base = (expt 2 (/ 12)) creates a semitone envelope
;;; srate-error:
;;;   error bound for the exponential conversion. 
;;; grain-start:
;;;   envelope that determines the starting point of the current grain in
;;;   the input file. "y"->0 starts the grain at the beginning of the input
;;;   file. "y"->1 starts the grain at the end of the input file.
;;; grain-start-spread:
;;;   random spread around the value of "grain-start"
;;; grain-start-in-seconds:
;;;   #f -> grain-start y envelope expressed in percent of the duration of the input file
;;;   #t -> grain-start y envelope expressed in seconds
;;; grain-density:
;;;   envelope that controls the number of grains per second generated in the output file
;;; grain-density-spread:
;;;   envelope that controls a random variation of density

(define grani-to-locsig 0.0)
(define grani-to-grain-duration 1)
(define grani-to-grain-start 2)
(define grani-to-grain-sample-rate 3)
(define grani-to-grain-random 4)
(define grani-to-grain-allchans 5)

(definstrument (grani start-time duration amplitude file
		      (input-channel 0)
		      (grains 0)
		      (amp-envelope '(0 0 0.3 1 0.7 1 1 0))
		      (grain-envelope '(0 0 0.3 1 0.7 1 1 0)) 
		      (grain-envelope-end #f)
		      (grain-envelope-transition '(0 0 1 1)) 
		      (grain-envelope-array-size 512)
		      (grain-duration 0.1)
		      (grain-duration-spread 0.0)
		      (grain-duration-limit 0.002)
		      (srate 0.0)
		      (srate-spread 0.0)
		      (srate-linear #f)
		      (srate-base grani-default-base)
		      (srate-error 0.01)
		      (grain-start '(0 0 1 1)) 
		      (grain-start-spread 0.0)
		      (grain-start-in-seconds #f)
		      (grain-density 10.0)
		      (grain-density-spread 0.0)
		      (reverb-amount 0.01)
		      (reverse #f)
		      (where-to 0)
		      (where-bins #f) ; a vct, not a list
		      (grain-distance 1.0)
		      (grain-distance-spread 0.0)
		      (grain-degree 45.0)
		      (grain-degree-spread 0.0)
		      (verbose #t))
  (let ((ts (times->samples start-time duration))
	 (in-file-channels (channels file))
	 (in-file-sr (exact->inexact (mus-sound-srate file))))

    (let ((beg (car ts))
	  (end (cadr ts))
	  (in-file-dur  (/ (frames file) in-file-sr))
	  (out-chans (channels *output*))
	  (gr-samples 0)
	  
	  ;; ratio between input and output sampling rates
	  (srate-ratio (/ in-file-sr (mus-srate)))
	  ;; sample rate converter for input samples
	  (rd (make-readin :file file :channel (min input-channel (- in-file-channels 1)))))
      
      (let ((last-in-sample (floor (* in-file-dur in-file-sr)))
	    
	    (in-file-reader (make-src :input rd :srate 1.0))
	    ;; sample rate conversion envelope
	    (sr-env (make-env (if srate-linear
				  (envelope-or-number srate)
				  (exp-envelope (envelope-or-number srate) 
						:base srate-base 
						:error srate-error))
			      :scaler srate-ratio
			      :duration duration))
	    ;; sample rate conversion random spread
	    (sr-spread-env (make-env (envelope-or-number srate-spread)
				     :duration duration))			 
	    ;; amplitude envelope for the note
	    (amp-env (make-env amp-envelope
			       :scaler amplitude
			       :duration duration))
	    ;; grain duration envelope
	    (gr-dur (make-env (envelope-or-number grain-duration)
			      :duration duration))
	    (gr-dur-spread (make-env (envelope-or-number grain-duration-spread)
				     :duration duration))
	    ;; position in the input file where the grain starts
	    (gr-start-scaler (if (not grain-start-in-seconds) in-file-dur 1.0))
	    (gr-start (make-env (envelope-or-number grain-start)
				:duration duration))
	    ;; random variation in the position in the input file
	    (gr-start-spread (make-env (envelope-or-number grain-start-spread)
				       :duration duration))			  
	    ;; density envelope in grains per second
	    (gr-dens-env (make-env (envelope-or-number grain-density)
				   :duration duration))
	    ;; density spread envelope in grains per second
	    (gr-dens-spread-env (make-env (envelope-or-number grain-density-spread)
					  :duration duration))
	    ;; grain envelope
	    (gr-env (make-table-lookup :frequency 1.0
				       :initial-phase 0.0
				       :wave (if (vct? grain-envelope)
						 grain-envelope
						 (make-gr-env grain-envelope 
							      grain-envelope-array-size))))
	    ;; grain envelope
	    (gr-env-end (make-table-lookup :frequency 1.0
					   :initial-phase 0.0
					   :wave (if grain-envelope-end
						     (if (vct? grain-envelope-end)
							 grain-envelope-end
							 (make-gr-env grain-envelope-end 
								      grain-envelope-array-size))
						     (make-vct 512))))
	    ;; envelope for transition between grain envelopes
	    (gr-int-env (make-env (envelope-or-number grain-envelope-transition) :duration duration))
	    (gr-int-env-1 (make-env (envelope-or-number grain-envelope-transition) :duration duration :offset 1.0 :scaler -1.0))
	    (interp-gr-envs grain-envelope-end)
	    ;; envelope for distance of grains (for using in locsig)
	    (gr-dist (make-env (envelope-or-number grain-distance)
			       :duration duration))
	    (gr-dist-spread (make-env (envelope-or-number grain-distance-spread)
				      :duration duration))
	    ;; envelopes for angular location and spread of grain in the stereo field
	    (gr-degree (make-env (envelope-or-number grain-degree)
				 :duration duration))
	    (gr-degree-spread (make-env (envelope-or-number grain-degree-spread)
					:duration duration))
	    ;; signal locator in the stereo image
	    (loc (make-locsig :degree 45.0
			      :distance 1.0
			      :channels out-chans))
	    
	    (in-samples 0)
	    (gr-start-sample beg)
	    (gr-from-beg 0)
	    (in-start 0)
	    (in-start-value 0.0)
	    (gr-duration 0.0)
	    (gr-dens 0.0)
	    (gr-dens-spread 0.0)
	    (gr-srate 0.0)
	    (grain-counter 0)
	    (first-grain #t)
	    (val1 0.0)
	    (val2 0.0)
	    (where 0.0)
	    (happy #t)
	    (where-bins-len (if (vct? where-bins) (length where-bins) 0)))
	(if (<= where-bins-len 1)
	    (set! where-bins #f))

	(if reverse (set! (mus-increment in-file-reader) -1.0))
	(do () 
	    ((not happy))
	  ;;
	  ;; start of a new grain
	  ;;
	  (if first-grain
	      ;; first grain always starts at 0
	      (begin
		(set! first-grain #f)
		(set! gr-start-sample beg))
	      (begin
		;; start grain in output file using
		;; increments from previous grain
		(set! gr-start-sample (+ gr-start-sample
					 (floor 
					  (* (/ (+ gr-dens gr-dens-spread)) (mus-srate)))))
		;; finish if start of grain falls outside of note
		;; bounds or number of grains exceeded
		(if (or (> gr-start-sample end)
			(and (not (zero? grains))
			     (>= grain-counter grains)))
		    (set! happy #f))))
	  (if happy
	      (begin
		;; back to the beginning of the grain
		;(set! gr-offset 0)
		;; start of grain in samples from beginning of note
		(set! gr-from-beg (floor (- gr-start-sample beg)))
		;; reset out-time dependent envelopes to current time
		(set! (mus-location amp-env) gr-from-beg)
		(set! (mus-location gr-dur) gr-from-beg)
		(set! (mus-location gr-dur-spread) gr-from-beg)
		(set! (mus-location sr-env) gr-from-beg)
		(set! (mus-location sr-spread-env) gr-from-beg)
		(set! (mus-location gr-start) gr-from-beg)
		(set! (mus-location gr-start-spread) gr-from-beg)
		(set! (mus-location gr-dens-env) gr-from-beg)
		(set! (mus-location gr-dens-spread-env) gr-from-beg)
		;; start of grain in input file
		(set! in-start-value (+ (* (env gr-start) gr-start-scaler)
					(mus-random (* 0.5 (env gr-start-spread)
						       gr-start-scaler))))
		(set! in-start (floor (* in-start-value in-file-sr)))
		;; duration in seconds of the grain
		(set! gr-duration (max grain-duration-limit
				       (+ (env gr-dur)
					  (mus-random (* 0.5 (env gr-dur-spread))))))
		;; number of samples in the grain
		(set! gr-samples (floor (* gr-duration (mus-srate))))
		;; new sample rate for grain
		(set! gr-srate (if srate-linear
				   (+ (env sr-env)
				      (mus-random (* 0.5 (env sr-spread-env))))
				   (* (env sr-env)
				      (expt srate-base
					    (mus-random (* 0.5 (env sr-spread-env)))))))
		;; set new sampling rate conversion factor
		(set! (mus-increment in-file-reader) gr-srate)
		;; number of samples in input
		(set! in-samples (floor (* gr-samples srate-ratio)))
		
		;; check for out of bounds condition in in-file pointers
		(if (> (+ in-start in-samples) last-in-sample)
		    (set! in-start (- last-in-sample in-samples))
		    (if (< in-start 0)
			(set! in-start 0)))
		;; reset position of input file reader
		(set! (mus-location rd) in-start)
		
		;; restart grain envelopes
		(set! (mus-phase gr-env) 0.0)
		(set! (mus-phase gr-env-end) 0.0)
		;; reset grain envelope durations
		(set! (mus-frequency gr-env) (/ gr-duration))
		(set! (mus-frequency gr-env-end) (/ gr-duration))
		;;
		;; move position in output file for next grain
		;;
		(set! gr-dens (env gr-dens-env))
		;; increment spread in output file for next grain
		(set! gr-dens-spread (mus-random (* 0.5 (env gr-dens-spread-env))))
		(set! grain-counter (+ grain-counter 1))
		(set! where (cond (;; use duration of grains as delimiter
				   (= where-to grani-to-grain-duration)
				   gr-duration)
				  (;; use start in input file as delimiter
				   (= where-to grani-to-grain-start)
				   in-start-value)
				  (;; use sampling rate as delimiter
				   (= where-to grani-to-grain-sample-rate)
				   gr-srate)
				  (;; use a random number as delimiter
				   (= where-to grani-to-grain-random)
				   (random 1.0))
				  (else grani-to-locsig)))
		(if (and where-bins
			 (not (zero? where)))
		    ;; set output scalers according to criteria
		    (do ((chn 0 (+ chn 1)))
			((or (= chn out-chans)
			     (= chn where-bins-len)))
		      (locsig-set! loc chn (if (< (where-bins chn)
						  where
						  (where-bins (+ chn 1)))
					       1.0
					       0.0)))
		    ;; if not "where" see if the user wants to send to all channels
		    (if (= where-to grani-to-grain-allchans)
			;; send the grain to all channels
			(do ((chn 0 (+ chn 1)))
			    ((= chn out-chans))
			  (locsig-set! loc chn 1.0))
			;; "where" is zero or unknown: use normal n-channel locsig, 
			;; only understands mono reverb and 1, 2 or 4 channel output
			(begin
			  (set! (mus-location gr-dist) gr-from-beg)
			  (set! (mus-location gr-dist-spread) gr-from-beg)
			  (set! (mus-location gr-degree) gr-from-beg)
			  (set! (mus-location gr-degree-spread) gr-from-beg)
			  ;; set locsig parameters, for now only understands stereo
			  (move-locsig loc
				       (+ (env gr-degree)
					  (mus-random (* 0.5 (env gr-degree-spread))))
				       (+ (env gr-dist)
					  (mus-random (* 0.5 (env gr-dist-spread))))))))

		(let ((grend (+ gr-start-sample gr-samples)))
		  (if interp-gr-envs
		      (do ((gr-offset gr-start-sample (+ gr-offset 1)))
			  ((= gr-offset grend))
			(locsig loc gr-offset (* (env amp-env) 
						 (src in-file-reader)
						 (+ (* (env gr-int-env) (table-lookup gr-env-end))
						    (* (env gr-int-env-1) (table-lookup gr-env))))))

		      (do ((gr-offset gr-start-sample (+ gr-offset 1)))
			  ((= gr-offset grend))
			(locsig loc gr-offset (* (env amp-env) 
						 (table-lookup gr-env)
						 (src in-file-reader)))))))))))))


;; (with-sound (:channels 2 :reverb jc-reverb :reverb-channels 1) (let ((file "oboe.snd")) (grani 0 2 5 file :grain-envelope (raised-cosine))))
;; (with-sound (:channels 2) (let ((file "oboe.snd")) (grani 0 2 5 file :grain-envelope (raised-cosine))))

(define (test-grani)
  (with-sound (:channels 2 :reverb jc-reverb :reverb-channels 1 :statistics #t)
    (grani 0 1 .5 "oboe.snd" :grain-envelope '(0 0 0.2 0.2 0.5 1 0.8 0.2 1 0))
    (grani 0 4 1 "oboe.snd")
    (grani 0 4 1 "oboe.snd" :grains 10)
    (grani 0 4 1 "oboe.snd" 
	   :grain-start 0.11 
	   :amp-envelope '(0 1 1 1) :grain-density 8
	   :grain-envelope '(0 0 0.2 0.2 0.5 1 0.8 0.2 1 0)
	   :grain-envelope-end '(0 0 0.01 1 0.99 1 1 0)
	   :grain-envelope-transition '(0 0 0.4 1 0.8 0 1 0))
    (grani 0 3 1 "oboe.snd" 
	   :grain-start 0.1 
	   :amp-envelope '(0 1 1 1) :grain-density 20
	   :grain-duration '(0 0.003 0.2 0.01 1 0.3))
    (grani 0 3 1 "oboe.snd" 
	   :grain-start 0.1 
	   :amp-envelope '(0 1 1 1) :grain-density 20
	   :grain-duration '(0 0.003 0.2 0.01 1 0.3)
	   :grain-duration-limit 0.02)
    (grani 0 2 1 "oboe.snd" 
	   :amp-envelope '(0 1 1 1) :grain-density 40
	   :grain-start '(0 0.1 0.3 0.1 1 0.6))
    (grani 0 2 1 "oboe.snd" 
	   :amp-envelope '(0 1 1 1) :grain-density 40
	   :grain-start '(0 0.1 0.3 0.1 1 0.6)
	   :grain-start-spread 0.01)
    (grani 0 2.6 1 "oboe.snd" 
	   :grain-start 0.1 :grain-start-spread 0.01
	   :amp-envelope '(0 1 1 1) :grain-density 40
	   :srate '(0 0 0.2 0 0.6 5 1 5))
    (grani 0 2.6 1 "oboe.snd" 
	   :grain-start 0.1 :grain-start-spread 0.01
	   :amp-envelope '(0 1 1 1) :grain-density 40
	   :srate-base 2
	   :srate '(0 0 0.2 0 0.6 -1 1 -1))
    (grani 0 2.6 1 "oboe.snd" 
	   :grain-start 0.1 :grain-start-spread 0.01
	   :amp-envelope '(0 1 1 1) :grain-density 40
	   :srate-linear #t
	   :srate (list 0 1 0.2 1 0.6 (expt 2 (/ 5 12)) 1 (expt 2 (/ 5 12))))
    (grani 0 2 1 "oboe.snd" 
	   :grain-start 0.1 :grain-start-spread 0.01
	   :amp-envelope '(0 1 1 1) :grain-density 40
	   :grain-duration '(0 0.02 1 0.1) 
	   :grain-duration-spread '(0 0 0.5 0.1 1 0)
	   :where-to grani-to-grain-duration ; from grani.scm
	   :where-bins (vct 0 0.05 1))
    (grani 0 2 1 "oboe.snd" 
	   :grain-start 0.1 :grain-start-spread 0.01
	   :amp-envelope '(0 1 1 1) :grain-density 40
	   :grain-degree '(0 0 1 90)
	   :grain-degree-spread 10)))