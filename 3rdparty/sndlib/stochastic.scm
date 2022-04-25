;; CLM implementation of Xenakis' Dynamic Stochastic Synthesis as heard in
;; his GENDY3, S.709, Legende d'Eer, etc.
;; 12/17/03
;; revised 01/22/06
;; Bill Sack wsack@buffalo.edu

;; revised slightly to accommodate the run macro, Bill 13-Jun-06

(if (provided? 'snd)
    (if (not (provided? 'snd-ws.scm)) (load "ws.scm"))
    (if (not (provided? 'sndlib-ws.scm)) (load "sndlib-ws.scm")))
(if (not (provided? 'snd-env.scm)) (load "env.scm"))

(definstrument 
  (stochastic start dur
	      (amp .9) (bits 16) (xmin 1) (xmax 20) (xwig 0) (xstep 1) (ywig 0) (xfb 0)
	      (init-array '((10 0) (10 1) (10 0) (10 -.7) (10 0) (10 .5) 
			    (10 0) (10 -.3) (10 0) (10 .2) (10 0) (10 -.1))))
  ;;some explanation of the parameters:
  ;;amp - scales overall amplitude
  ;;bits - the resolution of the wave's amplitude dimension
  ;;xmin - minimum number of samples between time breakpoints. must be equal to or greater than 1
  ;;xmax - maximum number of samples between time breakpoints.
  ;;xwig - amplitude applied to random walk function in time dimension
  ;;xstep - quantization of freedom in time dimension, in samples. minimum of 1
  ;;ywig - amplitude applied to random walk function in amplitude dimension, in 
  ;;       percent of overall possible amplitude
  ;;xfb - an attempt at an FIR low-pass filter - the old (n + (x * n-1)) trick,
  ;;      not really that useful
  ;;init-array - initial x and y breakpoints for wave. x values must be 
  ;;             integers and 1 or greater, y values between -1.0 and 1.0
  (let* ((y 0.0) (dx 0) (prev-dx 0) (dy 0.0)
	 (j 0.0) (m 0) (dt 0) (output 0.0) 
	 (oldy 0.0) (xdev 0) (ydev 0)
	 (beg (seconds->samples start))
	 (end (+ beg (seconds->samples dur)))
	 (d-click (make-env (list 0 1 (- end 100) 1 end 0) :duration dur))
	 (b (expt 2 (- bits 1))); because we use signed ints - see (- b) below
	 ;;make vct to hold x,y breakpoints
	 (xy-array (make-vct (* (length init-array) 2)))
	 (xy-array-l (floor (length xy-array)))
	 )
    ;;fill xy-array with values from init-array
    (do ((iy 0 (+ iy 2));;index for reading values from init-array (a 2-dimensional list)
	 (jy 0 (+ jy 1)));;index for writing to xy-array (a 1-dimensional vct)
	((= iy xy-array-l) xy-array)
      (set! (xy-array iy) ((init-array jy) 0))
      (set! (xy-array (+ iy 1))
	    ;;convert signed float y values into signed integers 
	    (floor (* b
		      ((init-array jy) 1)))))

     (do ((i beg (+ i 1)))
	 ((= i end))
       (if (= dx dt);;when current sample is a breakpoint
	   (begin
	     (set! dx (floor (xy-array (modulo m xy-array-l))))
	     (set! y (xy-array (+ (modulo m xy-array-l) 1)))
	     (set! prev-dx (floor (xy-array (modulo (- m 2) xy-array-l))))
	     (set! dy (- y oldy))
	     (set! oldy y)
	     ;;straight uniform distribution for y
	     (set! ydev (round (mus-random (* .01 b ywig))))
	     ;;gaussian distribution for x
	     (set! xdev 
		   (* xstep (round 
			     (* xwig 
				(sqrt (* -2.0 (log (- 1 (random 1.0))))) ; ??
				(cos (random 6.283185307179586))))))
	     (set! (xy-array (modulo m xy-array-l))
		       ;;mirror stuff for x
		       (cond ((or  (< (round xmax) (+ dx xdev))
				   (> (round xmin)(+ dx xdev)))
			      (max (min ;;this mirror is attentuated
				    (round (+ (* xfb prev-dx) (* (- 1  xfb) (+ dx (- xdev)))))
				    (round xmax))
				   (round xmin)))
			     (else (round (+ (* xfb prev-dx)
					     (* (- 1  xfb) (+ dx xdev)))))))
	     (set! (xy-array (+ (modulo m xy-array-l) 1))
		       ;;mirror stuff for y 
		       (cond ((or (< b (+ y ydev)) (> (- b) (+ y ydev)))
			      (max (min (+ y (- ydev)) b) (- b)))
			     (else (+ y ydev))))
	     (set! m (+ m 2))
	     (set! dt 0)))
       (set! dt (+ dt 1))
       (set! j (+ j (/ dy dx)));linear interpolation
       (set! output (/ j b));normalization -1 to 1
       (outa i (* amp output (env d-click))))))
  
;(with-sound (:statistics #t)(stochastic 0 10 :xwig .25 :ywig 10.0))
  