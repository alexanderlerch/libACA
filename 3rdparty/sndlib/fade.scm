;;; cross fade instruments
;;;
;;; cross-fade sweeps up, down, or from mid-spectrum outwards,
;;; dissolve-fade chooses randomly -- like a graphical dissolve
;;; neither is exactly spectacular, but they work -- use similar sounds if possible (speech is problematic)
;;;
;;; translated from fade.ins

(definstrument (cross-fade beg dur amp file1 file2 ramp-beg ramp-dur ramp-type bank-dur fs fwidth)
  ;; ramp-type 0=sweep up, 1=sweep down, 2=split from middle

  (if (> (+ (max bank-dur ramp-beg) ramp-dur bank-dur) dur)
      (begin
	(set! ramp-beg (* 0.25 dur))
	(set! ramp-dur (* dur 0.49))
	(set! bank-dur (* dur 0.24))))
	
  (let ((fil1 (make-sampler 0 file1))
	(fil2 (make-sampler 0 file2))
	(start (seconds->samples beg))
	(ramp-samps (seconds->samples ramp-dur))
	(bank-samps (seconds->samples bank-dur))
	(fs1 (make-vector fs)))

    (let ((bin (/ (mus-srate) (* 2 fs)))
	  (radius (- 1.0 (/ fwidth (* 2 fs)))))
      (do ((k 0 (+ k 1)))
	  ((= k fs))
	(set! (fs1 k) (make-formant (* k bin) radius))))
    (set! fs1 (make-formant-bank fs1))
    
    (let ((end (+ start (seconds->samples dur)))
	  (bank-incr (/ 1.0 bank-samps))
	  (ramp-incr (/ 1.0 ramp-samps))
	  (ramp-start (+ start (seconds->samples ramp-beg))))
      (let ((bank1-start (- ramp-start bank-samps))
	    (ramp-end (+ ramp-start ramp-samps))
	    (bank2-start (+ ramp-start ramp-samps)))
	
	(do ((i start (+ i 1)))
	    ((= i bank1-start))
	  ;; in first section -- just mix in file1
	  (outa i (* amp (read-sample fil1))))
	
	(let ((bank2-end (+ bank2-start bank-samps))
	      (ramp 0.0)
	      (outval 0.0)
	      (inputs (make-vct fs 0.0))
	      (ifs (/ 1.0 fs))
	      (mid 0))
	  
	  (do ((i bank1-start (+ i 1))
	       (bank1 0.0 (+ bank1 bank-incr)))
	      ((= i ramp-start))
	    ;; in bank1 section -- fire up the resonators
	    (let ((inval (read-sample fil1)))
	      (set! outval (formant-bank fs1 inval))
	      (outa i (* amp (+ (* bank1 outval) (* (- 1.0 bank1) inval))))))
	  
	  ;; in the ramp
	  (case ramp-type
	    ((0)
	     (do ((i ramp-start (+ i 1)))
		 ((= i ramp-end))
	       (let ((inval1 (read-sample fil1))
		     (inval2 (read-sample fil2)))
		 ;; now the choice of spectral fade -- we should end with all bank1 0.0 and all bank2 1.0
		 (set! ramp (+ ramp ramp-incr))
		 
		 ;; low freqs go first
		 (if (>= ramp 0.5)
		     (begin
		       (set! mid (floor (* (- (* 2.0 ramp) 1.0) fs)))
		       (vct-fill! inputs inval2)
		       (do ((k mid (+ k 1))
			    (ks 1.0 (- ks ifs)))
			   ((>= k fs))
			 (vct-set! inputs k (+ (* ks inval2) (* (- 1.0 ks) inval1)))))
		     (begin
		       (set! mid (min fs (floor (* 2.0 ramp fs))))
		       (vct-fill! inputs inval1)
		       (do ((k 0 (+ k 1))
			    (ks (* 2.0 ramp) (- ks ifs)))
			   ((= k mid))
			 (vct-set! inputs k (+ (* ks inval2) (* (- 1.0 ks) inval1))))))
		 (outa i (* amp (formant-bank fs1 inputs))))))
	    
	    ((1)
	     (do ((i ramp-start (+ i 1)))
		 ((= i ramp-end))
	       (let ((inval1 (read-sample fil1))
		     (inval2 (read-sample fil2)))
		 (set! ramp (+ ramp ramp-incr))
		 
		 ;; high freqs go first
		 (if (>= ramp 0.5)
		     (let ((r2 (- (* 2.0 ramp) 1.0)))
		       (set! mid (min fs (ceiling (* (- 1.0 r2) fs))))
		       (vct-fill! inputs inval2)
		       (do ((k 0 (+ k 1))
			    (ks r2 (+ ks ifs)))
			   ((= k mid))
			 (vct-set! inputs k (+ (* ks inval2) (* (- 1.0 ks) inval1)))))
		     (begin
		       (set! mid (ceiling (* (- 1.0 (* 2.0 ramp)) fs)))
		       (vct-fill! inputs inval1)
		       (do ((k mid (+ k 1))
			    (ks 0.0 (+ ks ifs)))
			   ((>= k fs))
			 (vct-set! inputs k (+ (* ks inval2) (* (- 1.0 ks) inval1))))))
		 (outa i (* amp (formant-bank fs1 inputs))))))
	    
	    (else
	     (let ((half-fs (/ fs 2)))
	       (do ((i ramp-start (+ i 1)))
		   ((= i ramp-end))
		 (let ((inval1 (read-sample fil1))
		       (inval2 (read-sample fil2)))
		   ;; now the choice of spectral fade -- we should end with all bank1 0.0 and all bank2 1.0
		   (set! ramp (+ ramp ramp-incr))
		   ;; sweep from midpoint out
		   (vct-fill! inputs inval1)
		   (set! mid (min half-fs (floor (* fs ramp))))
		   (do ((k (- half-fs mid) (+ k 1))
			(hk (+ half-fs mid -1) (- hk 1))
			(ks (max 0.0 (- (* 2.0 ramp) 1.0)) (+ ks ifs)))
		       ((= k half-fs))
		     (let ((rfs (min 1.0 ks)))
		       (set! (inputs k) (+ (* rfs inval2) (* (- 1.0 rfs) inval1)))
		       (set! (inputs hk) (inputs k))))
		   (outa i (* amp (formant-bank fs1 inputs))))))))
	  
	  (do ((i ramp-end (+ i 1))
	       (bank2 1.0 (- bank2 bank-incr)))
	      ((= i bank2-end))
	    ;; in bank2 section -- ramp out resonators
	    (let ((inval (read-sample fil2)))
	      (set! outval (formant-bank fs1 inval))
	      (outa i (* amp (+ (* bank2 outval) (* (- 1.0 bank2) inval))))))
	  
	  (do ((i bank2-end (+ i 1)))
	      ((= i end))
	    ;; in last section -- just mix file2
	    (outa i (* amp (read-sample fil2))))
	  )))))



;;; (vct->channel (with-sound (:output (make-vct 22050)) (cross-fade 0 .1 1 0 1 .01 .01 0 .1 256 2)))
;;; (vct->channel (with-sound (:output (make-vct 44100)) (cross-fade 0 2 1.0 "oboe.snd" "trumpet.snd" 0.5 1.0 0 .1 256 2)))
;;; (with-sound (:statistics #t) (cross-fade 0 2 1.0 "oboe.snd" "trumpet.snd" 0.5 1.0 0 .1 256 2))
;;; (with-sound () (cross-fade 0 2 1.0 "oboe.snd" "trumpet.snd" 0.5 1.0 0 .1 256 2))
;;; these fades seem more successful to me when done relatively quickly (the opposite of the dissolve below
;;; which is best if done as slowly as possible).  I like the sweep up best -- a sort of "evaporation" effect.


(definstrument (dissolve-fade beg dur amp file1 file2 fsize r lo hi)
  (let ((fil1 (make-sampler 0 file1))
	(fil2 (make-sampler 0 file2))
	(start (seconds->samples beg))
	(freq-inc (floor (/ fsize 2)))
	(ramp-inc (/ 1.0 1024.0)))
    (let ((end (+ start (seconds->samples dur)))
	  (spectr (make-vector freq-inc #f))
	  (trigger (floor (/ (* dur (mus-srate)) freq-inc)))
	  (fs (make-vector freq-inc #f))
	  (amps (make-vct freq-inc amp))
	  (ctr 0)
	  (inputs (make-vct freq-inc 0.0))
	  (ramps (make-vector freq-inc -1))
	  (in2s (make-vector freq-inc #f))
	  (in2-ctr 0)
	  (ramp-ctr 0))
    
      (if (not (number? hi)) (set! hi freq-inc))
      (let ((bin (floor (/ (mus-srate) fsize)))
	    (radius (- 1.0 (/ r fsize)))) 
	(do ((k lo (+ k 1)))
	    ((= k hi))
	  (set! (fs k) (make-formant (* k bin) radius))))
      (set! fs (make-formant-bank fs amps)) ; wrap it up...

      (do ((i start (+ i 1)))
	  ((= i end))

	;; once a ramp is set in motion, it takes care of itself -- we need only choose which to trigger
	(set! ctr (+ ctr 1))
	(if (> ctr trigger)
	    (let ((next (floor (random freq-inc))))
	      ;; find next randomly chosen resonator to flip
	      (if (not (spectr next))
		  (set! (spectr next) (- 1.0 ramp-inc))
		  (call-with-exit
		   (lambda (bbreak)
		     (do ((j next (+ j 1))
			  (k next (- k 1)))
			 (#t)
		       (if (and (< j freq-inc) 
				(not (spectr j)))
			   (begin 
			     (set! (spectr j) (- 1.0 ramp-inc))
			     (set! next j)
			     (bbreak)))
		       (if (and (>= k 0) 
				(not (spectr k)))
			   (begin 
			     (set! (spectr k) (- 1.0 ramp-inc))
			     (set! next k)
			     (bbreak)))))))
	      (set! (ramps ramp-ctr) next)
	      (set! ramp-ctr (+ ramp-ctr 1))
	      (set! ctr 0)))
	
	(let ((inval1 (read-sample fil1))
	      (inval2 (read-sample fil2)))
	  (vct-fill! inputs inval1)

	  (do ((k 0 (+ k 1)))
	      ((= k in2-ctr))
	    (vct-set! inputs (vector-ref in2s k) inval2))

	  (if (> ramp-ctr 0)
	      (let ((rk 0)
		    (sp 0.0)
		    (fixup-ramps #f))
		(do ((k 0 (+ k 1)))
		    ((= k ramp-ctr))
		  (set! rk (ramps k))
		  (set! sp (vector-ref spectr rk)) 
		  (set! (inputs k) (+ (* sp inval1) (* (- 1.0 sp) inval2)))
		  (set! sp (- sp ramp-inc))
		  (if (> sp 0.0)
		      (set! (spectr rk) sp)
		      (begin
			(set! (in2s in2-ctr) rk)
			(set! in2-ctr (+ in2-ctr 1))
			(set! fixup-ramps #t)
			(set! (ramps k) -1))))
		(if fixup-ramps
		    (let ((j 0))
		      (do ((k 0 (+ k 1)))
			  ((= k ramp-ctr))
			(if (>= (ramps k) 0)
			    (begin
			      (set! (ramps j) (ramps k))
			      (set! j (+ j 1)))))
		      (set! ramp-ctr j)))))

	  (outa i (formant-bank fs inputs)))))))


;;; (with-sound (:statistics #t) (dissolve-fade 0 1 1.0 "oboe.snd" "trumpet.snd" 256 2 0 128))
;;; (vct->channel (with-sound (:output (make-vct 44100)) (dissolve-fade 0 2 1 0 1 4096 2 2 #f)))
;;;
;;; another neat effect here is to simply let the random changes float along with no
;;; direction -- if the hit is 1.0 send it toward 0.0 and vice versa -- strange
;;; pitches emerge from noises etc



#|
;;; make it easy to see and hear:

(with-sound (:output "p1.snd") 
  (let ((g (make-ncos 200 100)))
    (do ((i 0 (+ i 1)))
	((= i 100000))
      (outa i (ncos g)))))

(with-sound (:output "p2.snd") 
  (let ((g (make-ncos 123 100)))
    (do ((i 0 (+ i 1)))
	((= i 100000))
      (outa i (ncos g)))))

(with-sound (:statistics #t) 
  (cross-fade 0 2 1.0 "p1.snd" "p2.snd" 0.5 1.0 0 .1 256 2))

(with-sound (:statistics #t) 
  (dissolve-fade 0 2 1.0 "p1.snd" "p2.snd" 256 2 0 128))
|#
