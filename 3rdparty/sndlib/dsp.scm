;;; a DSP-related grabbag

(provide 'snd-dsp.scm)
(if (provided? 'snd)
    (if (not (provided? 'snd-ws.scm)) (load "ws.scm"))
    (if (not (provided? 'sndlib-ws.scm)) (load "sndlib-ws.scm")))
;; (if (not (provided? 'snd-generators.scm)) (load "generators.scm"))
(if (not (provided? 'snd-env.scm)) (load "env.scm"))

(define (binomial n k)
  "(binomial n k) computes the binomial coefficient C(N,K)"
  (let ((mn (min k (- n k))))
    (if (< mn 0)
	0
	(if (= mn 0)
	    1
	    (let* ((mx (max k (- n k)))
		   (cnk (+ 1 mx)))
	      (do ((i 2 (+ 1 i)))
		  ((> i mn) cnk)
		(set! cnk (/ (* cnk (+ mx i)) i))))))))

(define (log10 a) 
  "(log10 a) returns the log base 10 of 'a'"
  (log a 10))

;;; src-duration (see src-channel in extsnd.html)

(define (src-duration e)
  "(src-duration envelope) returns the new duration of a sound after using 'envelope' for time-varying sampling-rate conversion"
  (let* ((len (length e))
	 (ex0 (e 0))
	 (ex1 (e (- len 2)))
	 (all-x (- ex1 ex0))
	 (dur 0.0))
    (do ((i 0 (+ i 2)))
	((>= i (- len 2)) dur)
      (let* ((x0 (e i))
	     (x1 (e (+ i 2)))
	     (y0 (e (+ i 1))) ; 1/x x points
	     (y1 (e (+ i 3)))
	     (area (if (< (abs (- y0 y1)) .0001)
		       (/ (- x1 x0) (* y0 all-x))
		       (* (/ (- (log y1) (log y0)) 
			     (- y1 y0)) 
			  (/ (- x1 x0) all-x)))))
	(set! dur (+ dur (abs area)))))))

;;; :(src-duration '(0 1 1 2))
;;; 0.69314718055995
;;; :(src-duration #(0 1 1 2))
;;; 0.69314718055995

(define (src-fit-envelope e target-dur)
  (scale-envelope e (/ (src-duration e) target-dur))) ; scale-envelope is in env.scm


;;; -------- Dolph-Chebyshev window
;;; 
;;; formula taken from Richard Lyons, "Understanding DSP"
;;; see clm.c for C version (using either GSL's or GCC's complex trig functions)

(define (dolph N gamma)
  "(dolph n gamma) produces a Dolph-Chebyshev FFT data window of 'n' points using 'gamma' as the window parameter."
  (let* ((alpha (cosh (/ (acosh (expt 10.0 gamma)) N)))
	 (den (/ 1.0 (cosh (* N (acosh alpha)))))
	 (freq (/ pi N))
	 (rl (make-vct N))
	 (im (make-vct N)))
    (do ((i 0 (+ i 1))
	 (phase 0.0 (+ phase freq)))
	((= i N))
      (let ((val (* den (cos (* N (acos (* alpha (cos phase))))))))
	(set! (rl i) (real-part val))
	(set! (im i) (imag-part val)))) ;this is always essentially 0.0
    (fft rl im -1)            ;direction could also be 1
    (let ((pk (vct-peak rl)))
      (vct-scale! rl (/ 1.0 pk)))
    (do ((i 0 (+ i 1))
	 (j (/ N 2)))
	((= i N))
      (set! (im i) (rl j))
      (set! j (+ j 1))
      (if (= j N) (set! j 0)))
    im))


;;; this version taken from Julius Smith's "Spectral Audio..." with three changes
;;;   it does the DFT by hand, and is independent of anything from Snd (fft, vcts etc)

(define (dolph-1 N gamma)
  "(dolph-1 n gamma) produces a Dolph-Chebyshev FFT data window of 'n' points using 'gamma' as the window parameter."
  (let* ((alpha (cosh (/ (acosh (expt 10.0 gamma)) N)))
	 (den (/ 1.0 (cosh (* N (acosh alpha)))))
	 (freq (/ pi N))
	 (vals (make-vector N))
	 (w (make-vector N))
	 (pk 0.0)
	 (mult -1))
    (do ((i 0 (+ i 1))
	 (phase (* -0.5 pi) (+ phase freq)))
	((= i N))
      (set! (vals i) (* mult den (cos (* N (acos (* alpha (cos phase)))))))
      (set! mult (- mult)))
    ;; now take the DFT
    (do ((i 0 (+ i 1)))
	((= i N))
      (let ((sum 0.0))
	(do ((k 0 (+ k 1)))
	    ((= k N))
	  (set! sum (+ sum (* (vals k) (exp (/ (* 2.0 0+1.0i pi k i) N))))))
	(set! (w i) (magnitude sum))
	(if (> (w i) pk) (set! pk (w i)))))
    ;; scale to 1.0 (it's usually pretty close already, that is pk is close to 1.0)
    (do ((i 0 (+ i 1)))
	((= i N))
      (set! (w i) (/ (w i) pk)))
    w))


;;; -------- move sound down by n (a power of 2)

(define* (down-oct n snd chn)
  "(down-n n) moves a sound down by power of 2 n"
  ;; I think this is "stretch" in DSP jargon -- to interpolate in the time domain we're squeezing the frequency domain
  ;;  the power-of-2 limitation is based on the underlying fft function's insistence on power-of-2 data sizes
  ;;  see stretch-sound-via-dft below for a general version
  (let* ((len (frames snd chn))
	 (pow2 (ceiling (log len 2)))
	 (fftlen (floor (expt 2 pow2)))
	 (fftlen2 (/ fftlen 2))
	 (fft-1 (- (* n fftlen) 1))
	 (fftscale (/ 1.0 fftlen))
	 (rl1 (channel->vct 0 fftlen snd chn))
	 (im1 (make-vct fftlen)))
    (fft rl1 im1 1)
    (vct-scale! rl1 fftscale)
    (vct-scale! im1 fftscale)
    (let ((rl2 (make-vct (* n fftlen)))
	  (im2 (make-vct (* n fftlen))))
      (set! (rl2 0) (rl1 0))
      (set! (im2 0) (im1 0))
      (do ((i 1 (+ i 1)) ; lower half
	   (k (- fftlen 1) (- k 1))
	   (j fft-1 (- j 1)))
	  ((= i fftlen2))
	(set! (rl2 i) (rl1 i))
	(set! (rl2 j) (rl1 k))
	(set! (im2 i) (im1 i))
	(set! (im2 j) (im1 k)))
      (fft rl2 im2 -1)
      (vct->channel rl2 0 (* n len) snd chn #f (format #f "down-oct ~A" n)))))

(define* (stretch-sound-via-dft factor snd chn)
  "(stretch-sound-via-dft factor snd chn) makes the given channel longer ('factor' should be > 1.0) by \
squeezing in the frequency domain, then using the inverse DFT to get the time domain result."
  ;; this is very slow! factor>1.0
  (let* ((n (frames snd chn))
	 (n2 (floor (/ n 2.0)))
	 (out-n (round (* n factor)))
	 (in-data (channel->vct 0 n snd chn))
	 (out-data (make-vct out-n))
	 (fr (make-vector out-n 0.0))
	 (freq (/ (* 2 pi) n)))
    (do ((i 0 (+ i 1)))
	((= i n))
      ;; DFT + split
      (if (< i n2)
	  (set! (fr i) (edot-product (* freq 0.0-1.0i i) in-data))
	  (set! (fr (+ i (- out-n n 1))) (edot-product (* freq 0.0-1.0i i) in-data))))
    (set! freq (/ (* 2 pi) out-n))
    (do ((i 0 (+ i 1)))
	((= i out-n))
      ;; inverse DFT
      (set! (out-data i) (real-part (/ (edot-product (* freq 0.0+1.0i i) fr) n))))
    (vct->channel out-data 0 out-n snd chn #f (format #f "stretch-sound-via-dft ~A" factor))))



;;; -------- compute-uniform-circular-string
;;;
;;; this is a simplification of the underlying table-filling routine for "scanned synthesis".
;;; To watch the wave, open some sound (so Snd has some place to put the graph), turn off
;;; the time domain display (to give our graph all the window -- to do this in a much more
;;; elegant manner, see snd-motif.scm under scanned-synthesis).

(define compute-uniform-circular-string
  (lambda (size x0 x1 x2 mass xspring damp)
    (define circle-ref 
      (lambda (v i)
	(if (< i 0)
	    (v (+ size i))
	    (if (>= i size)
		(v (- i size))
		(v i)))))
    (let* ((dm (/ damp mass))
	   (km (/ xspring mass))
	   (denom (+ 1.0 dm))
	   (p1 (/ (+ 2.0 (- dm (* 2.0 km))) denom))
	   (p2 (/ km denom))
	   (p3 (/ -1.0 denom)))
      (do ((i 0 (+ i 1)))
	  ((= i size))
	(set! (x0 i) (+ (* p1 (x1 i))
			(* p2 (+ (circle-ref x1 (- i 1)) (circle-ref x1 (+ i 1))))
			(* p3 (x2 i)))))
      (vct-fill! x2 0.0)
      (vct-add! x2 x1)
      (vct-fill! x1 0.0)
      (vct-add! x1 x0))))

(define compute-string
  ;; this is the more general form
  (lambda (size x0 x1 x2 masses xsprings esprings damps haptics)
    (define circle-ref 
      (lambda (v i)
	(if (< i 0)
	    (v (+ size i))
	    (if (>= i size)
		(v (- i size))
		(v i)))))
    (do ((i 0 (+ i 1)))
	((= i size))
      (let* ((dm (/ (damps i) (masses i)))
	     (km (/ (xsprings i) (masses i)))
	     (cm (/ (esprings i) (masses i)))
	     (denom (+ 1.0 dm cm))
	     (p1 (/ (+ 2.0 (- dm (* 2.0 km))) denom))
	     (p2 (/ km denom))
	     (p3 (/ -1.0 denom))
	     (p4 (/ (haptics i) (* (masses i) denom))))
	(set! (x0 i) (+ (* p1 (x1 i))
			(* p2 (+ (circle-ref x1 (- i 1)) (circle-ref x1 (+ i 1))))
			(* p3 (x2 i))
			p4))))
    (do ((i 0 (+ i 1)))
	((= i size))
      (set! (x2 i) (x1 i))
      (set! (x1 i) (x0 i)))))


;;; -------- "frequency division" -- an effect from sed_sed@my-dejanews.com

(define* (freqdiv n snd chn)
  "(freqdiv n snd chn) repeats each nth sample n times (clobbering the intermediate samples): (freqdiv 8)"
  (let* ((data (channel->vct 0 #f snd chn))
	 (len (length data)))
    (if (> n 1)
	(do ((i 0 (+ i n)))
	    ((>= i len)
	     (vct->channel data 0 len snd chn))
	  (let ((val (data i))
		(stop (min len (+ i n))))
	    (do ((k (+ i 1) (+ k 1)))
		((= k stop))
	      (vct-set! data k val)))))))


;;; -------- "adaptive saturation" -- an effect from sed_sed@my-dejanews.com
;;;
;;; a more extreme effect is "saturation":
;;;   (map-channel (lambda (val) (if (< (abs val) .1) val (if (>= val 0.0) 0.25 -0.25))))

(define* (adsat size (beg 0) dur snd chn)
  "(adsat size beg dur snd chn) is an 'adaptive saturation' sound effect"
  (let* ((len (if (number? dur) dur (- (frames snd chn) beg)))
	 (data (make-vct (* size (ceiling (/ len size))))))
    (let ((reader (make-sampler beg snd chn))
	  (mn 0.0)
	  (mx 0.0)
	  (vals (make-vct size)))
      (do ((i 0 (+ i size)))
	  ((>= i len))
	(do ((k 0 (+ k 1)))
	    ((= k size))
	  (vct-set! vals k (next-sample reader)))
	(set! mn (vct-min vals))
	(set! mx (vct-max vals))
	(do ((k 0 (+ k 1)))
	    ((= k size))
	  (if (negative? (vct-ref vals k))
	      (vct-set! data (+ i k) mn)
	      (vct-set! data (+ i k) mx))))
      (vct->channel data beg len snd chn current-edit-position (format #f "adsat ~A ~A ~A" size beg dur)))))


;;; -------- spike
;;;
;;; makes sound more spikey -- sometimes a nice effect

(define* (spike snd chn)
  "(spike snd chn) multiplies successive samples together to make a sound more spikey"
  (let* ((len (frames snd chn))
	 (data (make-vct len))
	 (amp (maxamp snd chn))) ; keep resultant peak at maxamp
    (let ((reader (make-sampler 0 snd chn))
	  (x1 0.0)
	  (x2 0.0)
	  (amp1 (/ 1.0 (* amp amp))))
      (do ((i 0 (+ i 1)))
	  ((= i len))
	(let ((x0 (next-sample reader)))
	  (vct-set! data i (* x0 x1 x2))
	  (set! x2 x1)
	  (set! x1 (abs x0))))
      (vct->channel (vct-scale! data amp1) 0 len snd chn current-edit-position "spike"))))

;;; the more successive samples we include in the product, the more we
;;;   limit the output to pulses placed at (just after) wave peaks


;;; -------- easily-fooled autocorrelation-based pitch tracker 

(define* (spot-freq s0 snd chn)
  "(spot-freq samp snd chn) tries to determine the current pitch: (spot-freq (left-sample))"
  (let* ((pow2 (ceiling (log (/ (srate snd) 20.0) 2)))
	 (fftlen (floor (expt 2 pow2)))
	 (data (autocorrelate (channel->vct s0 fftlen snd chn)))
	 (cor-peak (vct-peak data)))
    (if (= cor-peak 0.0)
	0.0
	(call-with-exit
	 (lambda (return)
	   (do ((i 1 (+ i 1)))
	       ((= i (- fftlen 2)) 0)
	     (if (and (< (data i) (data (+ i 1)))
		      (> (data (+ i 1)) (data (+ i 2))))
		 (begin
		   (let* ((logla (log10 (/ (+ cor-peak (data i)) (* 2 cor-peak))))
			  (logca (log10 (/ (+ cor-peak (data (+ i 1))) (* 2 cor-peak))))
			  (logra (log10 (/ (+ cor-peak (data (+ i 2))) (* 2 cor-peak))))
			  (offset (/ (* 0.5 (- logla logra))
				     (+ logla logra (* -2.0 logca)))))
		     (return (/ (srate snd)
				(* 2 (+ i 1 offset)))))))))))))

;(hook-push graph-hook 
;	   (lambda (hook)
;	     (status-report (format #f "~A" (spot-freq (left-sample))))))



;;; -------- chorus (doesn't always work and needs speedup)
(define chorus-size 5)
(define chorus-time .05)
(define chorus-amount 20.0)
(define chorus-speed 10.0)

(define (chorus)
  "(chorus) tries to produce the chorus sound effect"
  (define (make-flanger)
    (let* ((ri (make-rand-interp :frequency chorus-speed :amplitude chorus-amount))
	   (len (floor (random (* 3.0 chorus-time (srate)))))
	   (gen (make-delay len :max-size (+ len chorus-amount 1))))
      (list gen ri)))
  (define (flanger dly inval)
    (+ inval 
       (delay (car dly)
	      inval
	      (rand-interp (cadr dly)))))
  (let ((dlys (make-vector chorus-size)))
    (do ((i 0 (+ i 1)))
	((= i chorus-size))
      (set! (dlys i) (make-flanger)))
    (lambda (inval)
      (do ((sum 0.0)
	   (i 0 (+ i 1)))
	  ((= i chorus-size)
	   (* .25 sum))
	(set! sum (+ sum (flanger (dlys i) inval)))))))


;;; -------- chordalize (comb filters to make a chord using chordalize-amount and chordalize-base)
(define chordalize-amount .95)
(define chordalize-base 100)
(define chordalize-chord '(1 3/4 5/4))

(define (chordalize)
  "(chordalize) uses harmonically-related comb-filters to bring out a chord in a sound"
  ;; chord is a list of members of chord such as '(1 5/4 3/2)
  (let ((combs (make-comb-bank (apply vector (map (lambda (interval)
						    (make-comb chordalize-amount (floor (* chordalize-base interval))))
						  chordalize-chord))))
	(scaler (/ 0.5 (length chordalize-chord)))) ; just a guess -- maybe this should rescale to old maxamp
    (lambda (x)
      (* scaler (comb-bank combs x)))))


;;; -------- zero-phase, rotate-phase
;;; fft games (from the "phazor" package of Scott McNab)

(define* (zero-phase snd chn)
  "(zero-phase snd chn) calls fft, sets all phases to 0, and un-ffts"
  (let* ((len (frames snd chn))
	 (pow2 (ceiling (log len 2)))
	 (fftlen (floor (expt 2 pow2)))
	 (fftscale (/ 1.0 fftlen))
	 (rl (channel->vct 0 fftlen snd chn))
	 (old-pk (vct-peak rl))
	 (im (make-vct fftlen)))
    (if (> old-pk 0.0)
	(begin
	  (fft rl im 1)
	  (rectangular->magnitudes rl im)
	  (vct-scale! rl fftscale)
	  (vct-scale! im 0.0)
	  (fft rl im -1)
	  (let ((pk (vct-peak rl)))
	    (vct->channel (vct-scale! rl (/ old-pk pk)) 0 len snd chn #f "zero-phase"))))))

(define* (rotate-phase func snd chn)
  "(rotate-phase func snd chn) calls fft, applies func to each phase, then un-ffts"
  (let* ((len (frames snd chn))
	 (pow2 (ceiling (log len 2)))
	 (fftlen (floor (expt 2 pow2)))
	 (fftlen2 (floor (/ fftlen 2)))
	 (fftscale (/ 1.0 fftlen))
	 (rl (channel->vct 0 fftlen snd chn))
	 (old-pk (vct-peak rl))
	 (im (make-vct fftlen)))
    (if (> old-pk 0.0)
	(begin
	  (fft rl im 1)
	  (rectangular->magnitudes rl im)
	  (vct-scale! rl fftscale)
	  (set! (im 0) 0.0)
	  (do ((i 1 (+ i 1))
	       (j (- fftlen 1) (- j 1)))
	      ((= i fftlen2))
	    ;; rotate the fft vector by func, keeping imaginary part complex conjgate of real
	    (set! (im i) (func (im i)))
	    (set! (im j) (- (im i))))
	  (polar->rectangular rl im)
	  (fft rl im -1)
	  (let ((pk (vct-peak rl)))
	    (vct->channel (vct-scale! rl (/ old-pk pk)) 0 len snd chn #f 
			  (format #f "rotate-phase ~A" (procedure-source func))))))))

;(rotate-phase (lambda (x) 0.0)) is the same as (zero-phase)
;(rotate-phase (lambda (x) (random pi))) randomizes phases
;(rotate-phase (lambda (x) x)) returns original
;(rotate-phase (lambda (x) (- x))) reverses original (might want to write fftlen samps here)
;(rotate-phase (lambda (x) (* x 2))) reverb-effect (best with voice)
;(rotate-phase (lambda (x) (* x 12)) "bruise blood" effect


(define (signum n)
  ;; in CL this returns 1.0 if n is float
  (if (positive? n) 1
      (if (zero? n) 0
	  -1)))


;;; -------- brighten-slightly

(define* (brighten-slightly amount snd chn)
  "(brighten-slightly amount snd chn) is a form of contrast-enhancement ('amount' between ca .1 and 1)"
  (let* ((mx (maxamp))
	 (brt (* 2 pi amount))
	 (len (frames snd chn))
	 (data (make-vct len))
	 (reader (make-sampler 0 snd chn)))
    (do ((i 0 (+ i 1)))
	((= i len))
      (vct-set! data i (sin (* (next-sample reader) brt))))
    (vct->channel (vct-scale! data (/ mx (vct-peak data))) 0 len snd chn current-edit-position (format #f "brighten-slightly ~A" amount))))

(define (brighten-slightly-1 coeffs)
  "(brighten-slightly-1 coeffs) is a form of contrast-enhancement: (brighten-slightly-1 '(1 .5 3 1))"
  (let ((pcoeffs (partials->polynomial coeffs))
	(mx (maxamp))
	(len (frames)))
    (let ((data (make-vct len))
	  (reader (make-sampler 0)))
      (do ((i 0 (+ i 1)))
	  ((= i len))
	(vct-set! data i (polynomial pcoeffs (next-sample reader))))
      (vct->channel (vct-scale! data (/ mx (vct-peak data)))))))
       



;;; -------- FIR filters

;;; Snd's (very simple) spectrum->coefficients procedure is:

(define (spectrum->coeffs order spectr)
  "(spectrum->coeffs order spectr) returns FIR filter coefficients given the filter order and desired spectral envelope (a vct)"
  (let* ((coeffs (make-vct order))
	 (n order)
	 (m (floor (/ (+ n 1) 2)))
	 (am (* 0.5 (+ n 1)))
	 (q (/ (* pi 2.0) n)))
    (if (not (vct? spectr))
	(error "spectrum->coeffs spectrum argument should be a vct"))
     (do ((j 0 (+ j 1))
	  (jj (- n 1) (- jj 1)))
	 ((= j m) coeffs)
       (let ((xt (* 0.5 (spectr 0))))
	 (do ((i 1 (+ i 1)))
	     ((= i m))
	   (set! xt (+ xt (* (spectr i) (cos (* q i (- am j 1)))))))
	 (let ((coeff (* 2.0 (/ xt n))))
	   (set! (coeffs j) coeff)
	   (set! (coeffs jj) coeff))))))

;; (filter-channel et al reflect around the midpoint, so to match exactly you need to take
;;   the env passed and flip it backwards for the back portion -- that is to say, this function
;;   needs a wrapper to make it act like anyone would expect)


(define (fltit-1 order spectr)
  "(fltit-1 order spectrum) creates an FIR filter from spectrum and order and returns a closure that calls it: 
(map-channel (fltit-1 10 (vct 0 1.0 0 0 0 0 0 0 1.0 0)))"
  (let ((flt (make-fir-filter order (spectrum->coeffs order spectr))))
    (lambda (x)
      (fir-filter flt x))))

;(map-channel (fltit-1 10 (vct 0 1.0 0 0 0 0 0 0 1.0 0)))
;
;(let ((notched-spectr (make-vct 40)))
;  (set! (notched-spectr 2) 1.0)  
;  (set! (notched-spectr 37) 1.0)
;  (map-channel (fltit-1 40 notched-spectr)))
;

;;; -------- Hilbert transform

(define* (make-hilbert-transform (len 30))
  "(make-hilbert-transform (len 30)) makes a Hilbert transform filter"
  (let* ((arrlen (+ 1 (* 2 len)))
	 (arr (make-vct arrlen))
	 (lim (if (even? len) len (+ 1 len))))
     (do ((i (- len) (+ i 1)))
	 ((= i lim))
       (let ((k (+ i len))
	     (denom (* pi i))
	     (num (- 1.0 (cos (* pi i)))))
	 (if (or (= num 0.0) 
		 (= i 0))
	     (set! (arr k) 0.0)
	     ;; this is the "ideal" -- rectangular window -- version:
	     ;; (set! (arr k) (/ num denom))
	     ;; this is the Hamming window version:
	     (set! (arr k) (* (/ num denom) 
			      (+ .54 (* .46 (cos (/ (* i pi) len)))))) ; window
	     )))
    (make-fir-filter arrlen arr)))

(define hilbert-transform fir-filter)

#|
  (let ((h (make-hilbert-transform 15)))
    (map-channel (lambda (y)
		   (hilbert-transform h y))))

;;; this comes from R Lyons:
(define* (sound->amp-env snd chn)
  (let ((hlb (make-hilbert-transform 40))
	(d (make-delay 40)))
    (map-channel
     (lambda (y)
       (let ((hy (hilbert-transform hlb y))
	     (dy (delay d y)))
	 (sqrt (+ (* hy hy) (* dy dy)))))
     0 #f snd chn #f "sound->amp-env")))

(define* (hilbert-transform-via-fft snd chn)
  ;; same as FIR version but use FFT and change phases by hand
  (let* ((size (frames snd chn))
	 (len (expt 2 (ceiling (log size 2.0))))
	 (rl (make-vct len))
	 (im (make-vct len))
	 (rd (make-sampler 0 snd chn))
	 (pi2 (* 0.5 pi)))
    (do ((i 0 (+ i 1)))
	((= i size))
      (set! (rl i) (rd)))
    (mus-fft rl im len)
    (do ((i 0 (+ i 1)))
	((= i len))
      (let* ((c (make-rectangular (rl i) (im i)))
	     (ph (angle c))
	     (mag (magnitude c)))
	(if (< i (/ len 2))
	    (set! ph (+ ph pi2))
	    (set! ph (- ph pi2)))
	(set! c (make-polar mag ph))
	(set! (rl i) (real-part c))
	(set! (im i) (imag-part c))))
    (mus-fft rl im len -1)
    (vct-scale! rl (/ 1.0 len))
    (vct->channel rl 0 len snd chn #f "hilbert-transform-via-fft")))
|#

;;; -------- highpass filter 

(define* (make-highpass fc (len 30))
  "(make-highpass fc (len 30)) makes an FIR highpass filter"
  (let* ((arrlen (+ 1 (* 2 len)))
	 (arr (make-vct arrlen)))
    (do ((i (- len) (+ i 1)))
	((= i len))
      (let ((k (+ i len))
	    (denom (* pi i))
	    (num (- (sin (* fc i)))))
	(if (= i 0)
	    (set! (arr k) (- 1.0 (/ fc pi)))
	    (set! (arr k) (* (/ num denom) 
			     (+ .54 (* .46 (cos (/ (* i pi) len)))))))))
    (make-fir-filter arrlen arr)))

(define highpass fir-filter)

#|
  (let ((hp (make-highpass (* .1 pi))))
    (map-channel (lambda (y)
		   (highpass hp y))))
|#


;;; -------- lowpass filter

(define* (make-lowpass fc (len 30))
  "(make-lowpass fc (len 30)) makes an FIR lowpass filter"
  (let* ((arrlen (+ 1 (* 2 len)))
	 (arr (make-vct arrlen)))
    (do ((i (- len) (+ i 1)))
	((= i len))
      (let ((k (+ i len))
	    (denom (* pi i))
	    (num (sin (* fc i))))
	(if (= i 0)
	    (set! (arr k) (/ fc pi))
	    (set! (arr k) (* (/ num denom) 
			     (+ .54 (* .46 (cos (/ (* i pi) len)))))))))
    (make-fir-filter arrlen arr)))

(define lowpass fir-filter)

#|
  (let ((hp (make-lowpass (* .2 pi))))
    (map-channel (lambda (y)
		   (lowpass hp y))))
|#

;;; -------- bandpass filter

(define* (make-bandpass flo fhi (len 30))
  "(make-bandpass flo fhi (len 30)) makes an FIR bandpass filter"
  (let* ((arrlen (+ 1 (* 2 len)))
	 (arr (make-vct arrlen)))
    (do ((i (- len) (+ i 1)))
	((= i len))
      (let ((k (+ i len))
	    (denom (* pi i))
	    (num (- (sin (* fhi i)) (sin (* flo i)))))
	(if (= i 0)
	    (set! (arr k) (/ (- fhi flo) pi))
	    (set! (arr k) (* (/ num denom) 
			     (+ .54 (* .46 (cos (/ (* i pi) len)))))))))
    (make-fir-filter arrlen arr)))

(define bandpass fir-filter)

#|
  (let ((hp (make-bandpass (* .1 pi) (* .2 pi))))
    (map-channel (lambda (y)
		   (bandpass hp y))))

;; for more bands, you can add the coeffs:

(define* (make-bandpass-2 flo1 fhi1 flo2 fhi2 (len 30))
  (let* ((f1 (make-bandpass flo1 fhi1 len))
	 (f2 (make-bandpass flo2 fhi2 len)))
    (vct-add! (mus-xcoeffs f1) (mus-xcoeffs f2))
    f1))

(let ((ind (new-sound "test.snd")))
  (map-channel (lambda (y) (mus-random 1.0)) 0 10000)
  (let ((f2 (make-bandpass-2 (* .12 pi) (* .15 pi) (* .22 pi) (* .25 pi) 100)))
    (map-channel (lambda (y) (fir-filter f2 y)))
    ))

|#

;;; -------- bandstop filter

(define* (make-bandstop flo fhi (len 30))
  "(make-bandstop flo fhi (len 30)) makes an FIR bandstop (notch) filter"
  (let* ((arrlen (+ 1 (* 2 len)))
	 (arr (make-vct arrlen)))
    (do ((i (- len) (+ i 1)))
	((= i len))
      (let ((k (+ i len))
	    (denom (* pi i))
	    (num (- (sin (* flo i)) (sin (* fhi i)))))
	(if (= i 0)
	    (set! (arr k) (- 1.0 (/ (- fhi flo) pi)))
	    (set! (arr k) (* (/ num denom) 
			     (+ .54 (* .46 (cos (/ (* i pi) len)))))))))
    (make-fir-filter arrlen arr)))

(define bandstop fir-filter)

#|
  (let ((hp (make-bandstop (* .1 pi) (* .3 pi))))
    (map-channel (lambda (y)
		   (bandstop hp y))))
|#

;;; -------- differentiator

(define* (make-differentiator (len 30))
  "(make-differentiator (len 30)) makes an FIR differentiator (highpass) filter"
  (let* ((arrlen (+ 1 (* 2 len)))
	 (arr (make-vct arrlen)))
    (do ((i (- len) (+ i 1)))
	((= i len))
      (let ((k (+ i len)))
	(if (= i 0)
	    (set! (arr k) 0.0)
	    (set! (arr k) (* (- (/ (cos (* pi i)) i) (/ (sin (* pi i)) (* pi i i))) 
			     (+ .54 (* .46 (cos (/ (* i pi) len)))))))))
    (make-fir-filter arrlen arr)))

(define differentiator fir-filter)

#|
  (let ((hp (make-differentiator)))
    (map-channel (lambda (y)
		   (differentiator hp y))))
|#


;;; -------- IIR filters
;;; see analog-filter.scm for the usual suspects

;;; -------- Butterworth filters (see also further below -- make-butter-lp et al)
;;;
;; translated from CLM butterworth.cl:
;;
;;   Sam Heisz, January 1998
;;   inspired by some unit generators written for Csound by Paris Smaragdis
;;   who based his work on formulas from 
;;   Charles Dodge, Computer music: synthesis, composition, and performance.

(define butter filter)

(define (make-butter-high-pass fq)
  "(make-butter-high-pass freq) makes a Butterworth filter with high pass cutoff at 'freq'"
  ;; this is the same as iir-low-pass-2 below with 'din' set to (sqrt 2.0) -- similarly with the others
  (let* ((r (tan (/ (* pi fq) (srate))))
	 (r2 (* r r))
	 (c1 (/ 1.0 (+ 1.0 (* r (sqrt 2.0)) r2)))
	 (c2  (* -2.0 c1))
	 (c3 c1)
	 (c4 (* 2.0 (- r2 1.0) c1))
	 (c5 (* (+ (- 1.0 (* r (sqrt 2.0))) r2) c1)))
    (make-filter 3
		 (vct c1 c2 c3)
		 (vct 0.0 c4 c5))))

(define (make-butter-low-pass fq)
  "(make-butter-low-pass freq) makes a Butterworth filter with low pass cutoff at 'freq'.  The result 
can be used directly: (filter-sound (make-butter-low-pass 500.0)), or via the 'butter' generator"
  (let* ((r (/ 1.0 (tan (/ (* pi fq) (srate)))))
	 (r2 (* r r))
	 (c1 (/ 1.0 (+ 1.0 (* r (sqrt 2.0)) r2)))
	 (c2 (* 2.0 c1))
	 (c3 c1)
	 (c4 (* 2.0 (- 1.0 r2) c1))
	 (c5  (* (+ (- 1.0 (* r (sqrt 2.0))) r2) c1)))
    (make-filter 3
		 (vct c1 c2 c3)
		 (vct 0.0 c4 c5))))

(define (make-butter-band-pass fq bw)
  "(make-butter-band-pass freq band) makes a bandpass Butterworth filter with low edge at 'freq' and width 'band'"
  (let* ((d (* 2.0 (cos (/ (* 2.0 pi fq) (srate)))))
	 (c (/ 1.0 (tan (/ (* pi bw) (srate)))))
	 (c1 (/ 1.0 (+ 1.0 c)))
	 (c2 0.0)
	 (c3 (- c1))
	 (c4 (* (- c) d c1))
	 (c5 (* (- c 1.0) c1)))
    (make-filter 3
		 (vct c1 c2 c3)
		 (vct 0.0 c4 c5))))

(define (make-butter-band-reject fq bw)
  "(make-butter-band-reject freq band) makes a band-reject Butterworth filter with low edge at 'freq' and width 'band'"
  (let* ((d  (* 2.0 (cos (/ (* 2.0 pi fq) (srate)))))
	 (c (tan (/ (* pi bw) (srate))))
	 (c1 (/ 1.0 (+ 1.0 c)))
	 (c2 (* (- d) c1))
	 (c3 c1)
	 (c4 c2)
	 (c5 (* (- 1.0 c) c1)))
    (make-filter 3
		 (vct c1 c2 c3)
		 (vct 0.0 c4 c5))))

;;; simplest use is (filter-sound (make-butter-low-pass 500.0))
;;; see also effects.scm


;;; from "DSP Filter Cookbook" by Lane et al, Prompt Pubs, 2001
;;; 
;;; use with the filter generator
;;;   (define gen (make-iir-high-pass-2 1000))
;;;   (filter gen 1.0)
;;;   etc

(define (make-biquad a0 a1 a2 b1 b2)
  "(make-biquad a0 a1 a2 b1 b2) returns a biquad filter (use with the CLM filter gen)"
  (make-filter 3 
	       (vct a0 a1 a2) 
	       (vct 0.0 b1 b2)))

(define* (make-iir-low-pass-2 fc din) ; din=(sqrt 2.0) for example (suggested range 0.2.. 10)
  (let* ((theta (/ (* 2 pi fc) (mus-srate)))
	 (d (or din (sqrt 2.0)))
	 (beta (* 0.5 (/ (- 1.0 (* (/ d 2) (sin theta)))
			 (+ 1.0 (* (/ d 2) (sin theta))))))
	 (gamma (* (+ 0.5 beta) (cos theta)))
	 (alpha (* 0.5 (+ 0.5 beta (- gamma)))))
    (make-filter 3 
		 (vct alpha (* 2.0 alpha) alpha)
		 (vct 0.0 (* -2.0 gamma) (* 2.0 beta)))))

(define* (make-iir-high-pass-2 fc din)
  (let* ((theta (/ (* 2 pi fc) (mus-srate)))
	 (d (or din (sqrt 2.0)))
	 (beta (* 0.5 (/ (- 1.0 (* (/ d 2) (sin theta)))
			 (+ 1.0 (* (/ d 2) (sin theta))))))
	 (gamma (* (+ 0.5 beta) (cos theta)))
	 (alpha (* 0.5 (+ 0.5 beta gamma))))
    (make-filter 3
		 (vct alpha (* -2.0 alpha) alpha)
		 (vct 0.0 (* -2.0 gamma) (* 2.0 beta)))))

(define (make-iir-band-pass-2 f1 f2)
  (let* ((theta (/ (* 2 pi (sqrt (* f1 f2))) (mus-srate)))
	 (Q (/ (sqrt (* f1 f2)) (- f2 f1)))
	 (t2 (tan (/ theta (* 2 Q))))
	 (beta (* 0.5 (/ (- 1.0 t2)
			 (+ 1.0 t2))))
	 (gamma (* (+ 0.5 beta) (cos theta)))
	 (alpha (- 0.5 beta)))
    (make-filter 3
		 (vct alpha 0.0 (- alpha))
		 (vct 0.0 (* -2.0 gamma) (* 2.0 beta)))))

(define (make-iir-band-stop-2 f1 f2)
  (let* ((theta (/ (* 2 pi (sqrt (* f1 f2))) (mus-srate)))
	 (Q (/ (sqrt (* f1 f2)) (- f2 f1)))
	 (t2 (tan (/ theta (* 2 Q))))
	 (beta (* 0.5 (/ (- 1.0 t2)
			 (+ 1.0 t2))))
	 (gamma (* (+ 0.5 beta) (cos theta)))
	 (alpha (+ 0.5 beta)))
    (make-filter 3
		 (vct alpha (* -2.0 gamma) alpha)
		 (vct 0.0 (* -2.0 gamma) (* 2.0 beta)))))

(define* (make-eliminate-hum (hum-freq 60.0) (hum-harmonics 5) (bandwidth 10))
  (let ((gen (make-vector hum-harmonics)))
    (do ((i 0 (+ i 1)))
	((= i hum-harmonics))
      (let ((center (* (+ i 1.0) hum-freq))
	    (b2 (* 0.5 bandwidth)))
	(set! (gen i) (make-iir-band-stop-2 (- center b2) (+ center b2)))))
    gen))

(define (eliminate-hum gen x0)
  (let ((val x0))
    (do ((i 0 (+ i 1)))
	((= i (length gen)))
      (set! val (filter (vector-ref gen i) val))) ; "cascade" n filters
    val))

;;; (let ((hummer (make-eliminate-hum))) (map-channel (lambda (x) (eliminate-hum hummer x))))

(define (make-peaking-2 f1 f2 m)
  ;; bandpass, m is gain at center of peak
  ;; use map-channel with this one (not clm-channel or filter)
  (let* ((theta (/ (* 2 pi (sqrt (* f1 f2))) (mus-srate)))
	 (Q (/ (sqrt (* f1 f2)) (- f2 f1)))
	 (t2 (* (/ 4.0 (+ m 1)) (tan (/ theta (* 2 Q)))))
	 (beta (* 0.5 (/ (- 1.0 t2)
			 (+ 1.0 t2))))
	 (gamma (* (+ 0.5 beta) (cos theta)))
	 (alpha (- 0.5 beta))
	 (flt (make-filter 3
			   (vct alpha 0.0 (- alpha))
			   (vct 0.0 (* -2.0 gamma) (* 2.0 beta))))
	 (m1 (- m 1.0)))
    (lambda (x) (+ x (* m1 (filter flt x))))))


(define (cascade->canonical A)
  "(cascade->canonical A) converts a list of cascade coeffs (vcts with 3 entries) to canonical form"
  ;; from Orfanidis "Introduction to Signal Processing"

  (define (conv M h L x y)
    ;; x * h -> y
    (do ((n 0 (+ n 1)))
	((= n (+ L M)))
      (let ((sum 0.0)
	    (start (max 0 (- n (+ L 1))))
	    (end (min n M)))
	(do ((m start (+ m 1)))
	    ((> m end))
	  (set! sum (+ sum (* (h m) (x (- n m))))))
	(set! (y n) sum))))

  (let* ((K (length A))
	 (d (make-vct (+ 1 (* 2 K))))
	 (a1 (make-vct (+ 1 (* 2 K)))))
    (set! (a1 0) 1.0)
    (do ((i 0 (+ i 1)))
	((= i K))
      (conv 2 (A i) (+ 1 (* 2 i)) a1 d)
      (let ((end (+ 3 (* 2 i))))
	(do ((j 0 (+ j 1)))
	    ((= j end))
	  (vct-set! a1 j (vct-ref d j)))))
    a1))


(define (make-butter-lp M fc)
  "(make-butter-lp M fc) returns a butterworth low-pass filter; its order is 'M' * 2, 'fc' is the cutoff frequency in Hz"
  (let* ((xcoeffs ())
	 (ycoeffs ())
	 (theta (/ (* 2 pi fc) (mus-srate)))
	 (st (sin theta))
	 (ct (cos theta)))
    (do ((k 1 (+ k 1)))
	((> k M))
      (let* ((d (* 2 (sin (/ (* pi (- (* 2 k) 1)) (* 4 M)))))
	     (beta (* 0.5 (/ (- 1.0 (* 0.5 d st))
			     (+ 1.0 (* 0.5 d st)))))
	     (gamma (* ct (+ 0.5 beta)))
	     (alpha (* 0.25 (+ 0.5 beta (- gamma)))))
	(set! xcoeffs (cons (vct (* 2 alpha) (* 4 alpha) (* 2 alpha)) xcoeffs))
	(set! ycoeffs (cons (vct 1.0 (* -2.0 gamma) (* 2.0 beta)) ycoeffs))))
    (make-filter (+ 1 (* 2 M))
		 (cascade->canonical xcoeffs)
		 (cascade->canonical ycoeffs))))
	 
(define (make-butter-hp M fc)
  "(make-butter-hp M fc) returns a butterworth high-pass filter; its order is 'M' * 2, 'fc' is the cutoff frequency in Hz"
  (let* ((xcoeffs ())
	 (ycoeffs ())
	 (theta (/ (* 2 pi fc) (mus-srate)))
	 (st (sin theta))
	 (ct (cos theta)))
    (do ((k 1 (+ k 1)))
	((> k M))
      (let* ((d (* 2 (sin (/ (* pi (- (* 2 k) 1)) (* 4 M)))))
	     (beta (* 0.5 (/ (- 1.0 (* 0.5 d st))
			     (+ 1.0 (* 0.5 d st)))))
	     (gamma (* ct (+ 0.5 beta)))
	     (alpha (* 0.25 (+ 0.5 beta gamma))))
	(set! xcoeffs (cons (vct (* 2 alpha) (* -4 alpha) (* 2 alpha)) xcoeffs))
	(set! ycoeffs (cons (vct 1.0 (* -2.0 gamma) (* 2.0 beta)) ycoeffs))))
    (make-filter (+ 1 (* 2 M))
		 (cascade->canonical xcoeffs)
		 (cascade->canonical ycoeffs))))
	 
(define (make-butter-bp M f1 f2)
  "(make-butter-bp M f1 f2) returns a butterworth band-pass filter; its order is 'M' * 2, 'f1' and 'f2' are the band edge frequencies in Hz"
  (let* ((xcoeffs ())
	 (ycoeffs ())
	 (f0 (sqrt (* f1 f2)))
	 (Q (/ f0 (- f2 f1)))
	 (theta0 (/ (* 2 pi f0) (mus-srate)))
	 (de (/ (* 2 (tan (/ theta0 (* 2 Q)))) (sin theta0)))
	 (de2 (/ de 2))
	 (tn0 (tan (* theta0 0.5))))
    (do ((i 1 (+ i 1))
	 (k 1)
	 (j 1))
	((> i M))
      (let* ((Dk (* 2 (sin (/ (* pi (- (* 2 k) 1)) (* 2 M)))))
	     (Ak (/ (+ 1 (* de2 de2)) (* Dk de2)))
	     (dk1 (sqrt (/ (* de Dk)
			   (+ Ak (sqrt (- (* Ak Ak) 1))))))
	     (Bk (* de2 (/ Dk dk1)))
	     (Wk (real-part (+ Bk (sqrt (- (* Bk Bk) 1.0))))) ; fp inaccuracies causing tiny (presumably bogus) imaginary part here
	     (thetajk (if (= j 1)
			  (* 2 (atan tn0 Wk))
			  (* 2 (atan (* tn0 Wk)))))
	     (betajk (* 0.5 (/ (- 1.0 (* 0.5 dk1 (sin thetajk)))
			       (+ 1.0 (* 0.5 dk1 (sin thetajk))))))
	     (gammajk (* (+ 0.5 betajk) (cos thetajk)))
	     (wk2 (/ (- Wk (/ 1.0 Wk)) dk1))
	     (alphajk (* 0.5 (- 0.5 betajk) (sqrt (+ 1.0 (* wk2 wk2))))))
	(set! xcoeffs (cons (vct (* 2 alphajk) 0.0 (* -2 alphajk)) xcoeffs))
	(set! ycoeffs (cons (vct 1.0 (* -2.0 gammajk) (* 2.0 betajk)) ycoeffs))
	(if (= j 1)
	    (set! j 2)
	    (begin
	      (set! k (+ k 1))
	      (set! j 1)))))
    (make-filter (+ 1 (* 2 M))
		 (cascade->canonical xcoeffs)
		 (cascade->canonical ycoeffs))))
	 
(define (make-butter-bs M f1 f2)
  "(make-butter-bs M f1 f2) returns a butterworth band-stop filter; its order is 'M' * 2, 'f1' and 'f2' are the band edge frequencies in Hz"
  (let* ((xcoeffs ())
	 (ycoeffs ())
	 (f0 (sqrt (* f1 f2)))
	 (Q (/ f0 (- f2 f1)))
	 (theta0 (/ (* 2 pi f0) (mus-srate)))
	 (de (/ (* 2 (tan (/ theta0 (* 2 Q)))) (sin theta0)))
	 (de2 (/ de 2))
	 (ct (cos theta0))
	 (tn0 (tan (* theta0 0.5))))
    (do ((i 1 (+ i 1))
	 (k 1)
	 (j 1))
	((> i M))
      (let* ((Dk (* 2 (sin (/ (* pi (- (* 2 k) 1)) (* 2 M)))))
	     (Ak (/ (+ 1 (* de2 de2)) (* Dk de2)))
	     (dk1 (sqrt (/ (* de Dk)
			   (+ Ak (sqrt (- (* Ak Ak) 1))))))
	     (Bk (* de2 (/ Dk dk1)))
	     (Wk (real-part (+ Bk (sqrt (- (* Bk Bk) 1.0)))))
	     (thetajk (if (= j 1)
			  (* 2 (atan tn0 Wk))
			  (* 2 (atan (* tn0 Wk)))))
	     (betajk (* 0.5 (/ (- 1.0 (* 0.5 dk1 (sin thetajk)))
			       (+ 1.0 (* 0.5 dk1 (sin thetajk))))))
	     (gammajk (* (+ 0.5 betajk) (cos thetajk)))
	     (alphajk (* 0.5 (+ 0.5 betajk) (/ (- 1.0 (cos thetajk)) (- 1.0 ct)))))
	(set! xcoeffs (cons (vct (* 2 alphajk) (* -4 ct alphajk) (* 2 alphajk)) xcoeffs))
	(set! ycoeffs (cons (vct 1.0 (* -2.0 gammajk) (* 2.0 betajk)) ycoeffs))
	(if (= j 1)
	    (set! j 2)
	    (begin
	      (set! k (+ k 1))
	      (set! j 1)))))
    (make-filter (+ 1 (* 2 M))
		 (cascade->canonical xcoeffs)
		 (cascade->canonical ycoeffs))))
	 

;;; -------- notch filters

(define* (make-notch-frequency-response cur-srate freqs (notch-width 2))
  (let ((freq-response (list 1.0 0.0)))
    (for-each
     (lambda (i)
      (set! freq-response (cons (/ (* 2 (- i notch-width)) cur-srate) freq-response)) ; left upper y hz
      (set! freq-response (cons 1.0 freq-response)) ; left upper y resp
      (set! freq-response (cons (/ (* 2 (- i (/ notch-width 2))) cur-srate) freq-response)) ; left bottom y hz
      (set! freq-response (cons 0.0 freq-response)) ; left bottom y resp
      (set! freq-response (cons (/ (* 2 (+ i (/ notch-width 2))) cur-srate) freq-response)) ; right bottom y hz
      (set! freq-response (cons 0.0 freq-response)) ; right bottom y resp
      (set! freq-response (cons (/ (* 2 (+ i notch-width)) cur-srate) freq-response)) ; right upper y hz
      (set! freq-response (cons 1.0 freq-response))) ; right upper y resp
     freqs)
    (set! freq-response (cons 1.0 freq-response))
    (set! freq-response (cons 1.0 freq-response)) 
    (reverse freq-response)))

(define* (notch-channel freqs (filter-order #f) beg dur snd chn edpos (truncate #t) (notch-width 2))
  "(notch-channel freqs (filter-order #f) beg dur snd chn edpos (truncate #t) (notch-width 2)) -> notch filter removing freqs"
  (filter-channel (make-notch-frequency-response (* 1.0 (srate snd)) freqs notch-width)
		  (or filter-order (min (frames snd chn) (expt 2 (floor (log (/ (srate snd) notch-width) 2)))))
		  beg dur snd chn edpos truncate
		  (format #f "notch-channel '~A ~A ~A ~A" freqs filter-order beg dur)))

(define* (notch-sound freqs filter-order snd chn (notch-width 2))
  "(notch-sound freqs filter-order snd chn (notch-width 2)) -> notch filter removing freqs"
  (filter-sound (make-notch-frequency-response (* 1.0 (srate snd)) freqs notch-width)
		(or filter-order (min (frames snd chn) (expt 2 (floor (log (/ (srate snd) notch-width) 2)))))
		snd chn #f
		(format #f "notch-channel '~A ~A 0 #f" freqs filter-order)))

(define* (notch-selection freqs filter-order (notch-width 2))
  "(notch-selection freqs filter-order (notch-width 2)) -> notch filter removing freqs"
  (if (selection?)
      (filter-selection (make-notch-frequency-response (* 1.0 (selection-srate)) freqs notch-width)
			(or filter-order (min (selection-frames) (expt 2 (floor (log (/ (selection-srate) notch-width) 2))))))))
;; apparently I'm using powers of 2 here so that mus_make_fir_coeffs, called from get_filter_coeffs, can use an fft
;;   the others use the fft internally for the fir filter, but not filter-selection


;;; -------- fractional Fourier Transform, z transform
;;;
;;; translated from the fxt package of Joerg Arndt

(define (fractional-fourier-transform fr fi n v)
  "(fractional-fourier-transform real imaginary n angle) performs a fractional Fourier transform on data; if angle=1.0, you get a normal Fourier transform"
  ;; this is the slow (dft) form
  ;; v=1 -> normal fourier transform
  (let ((hr (make-vct n))
	(hi (make-vct n))
	(ph0 (/ (* v 2 pi) n)))
     (do ((w 0 (+ 1 w)))
	 ((= w n))
       (let ((sr 0.0)
	     (si 0.0))
	 (do ((k 0 (+ k 1)))
	     ((= k n))
	   (let* ((phase (* ph0 k w))
		  (c (cos phase))
		  (s (sin phase))
		  (x (fr k))
		  (y (fi k))
		  (r (- (* x c) (* y s)))
		  (i (+ (* y c) (* x s))))
	     (set! sr (+ sr r))
	     (set! si (+ si i))))
	 (set! (hr w) sr)
	 (set! (hi w) si)))
    (list hr hi)))

(define (z-transform f n z)
  ;; using vector to allow complex sums (z=e^2*pi*i/n -> fourier transform)
  ;;   (z-transform data n (exp (make-rectangular 0.0 (* (/ 2.0 n) pi))))
  "(z-transform data n z) performs a Z transform on data; if z=e^2*pi*j/n you get a Fourier transform; complex results in returned vector"
  (let ((res (make-vector n)))
    (do ((w 0 (+ 1 w)))
	((= w n))
      (let ((sum 0.0)
	    (t 1.0)
	    (m (expt z w)))
	;; -w?? there seems to be confusion here -- slowzt.cc in the fxt package uses +w
	(do ((k 0 (+ k 1)))
	    ((= k n))
	  (set! sum (+ sum (* (f k) t)))
	  (set! t (* t m)))
	(set! (res w) sum)))
    res))



;;; -------- slow Hartley transform 

(define (dht data) 
  "(dht data) returns the Hartley transform of 'data'."
  ;; taken from Perry Cook's SignalProcessor.m (the slow version of the Hartley transform)
  (let* ((len (length data)) 
	 (arr (make-vct len))
	 (w (/ (* 2.0 pi) len)))
     (do ((i 0 (+ i 1)))
	 ((= i len))
       (do ((j 0 (+ j 1)))
	   ((= j len))
	 (set! (arr i) (+ (arr i) 
			  (* (data j) 
			     (+ (cos (* i j w)) 
				(sin (* i j w))))))))
    arr))

(define* (find-sine freq beg dur snd)
  "(find-sine freq beg dur snd) returns the amplitude and initial-phase (for sin) at freq"
  (let ((incr (/ (* freq 2 pi) (srate snd)))
	(sw 0.0)
	(cw 0.0)
	(reader (make-sampler beg snd)))
     (do ((i 0 (+ i 1))) ; this could also use edot-product
	 ((= i dur))
       (let ((samp (next-sample reader)))
	 (set! sw (+ sw (* samp (sin (* i incr)))))
	 (set! cw (+ cw (* samp (cos (* i incr)))))))
    (list (* 2 (/ (sqrt (+ (* sw sw) (* cw cw))) dur))
	  (atan cw sw))))

;;; this is a faster version of find-sine using the "Goertzel algorithm" taken from R Lyons "Understanding DSP" p 529
;;; it returns the same result as find-sine above if you take (* 2 (/ (goertzel...) dur)) -- see snd-test.scm examples

(define* (goertzel freq (beg 0) dur snd)
  "(goertzel freq beg dur snd) returns the amplitude of the 'freq' spectral component"
  (let* ((sr (srate snd))
	 (rfreq (/ (* 2.0 pi freq) sr))
	 (cs (* 2.0 (cos rfreq))))
    (let ((reader (make-sampler beg snd 0))
	  (len (- (if (number? dur) dur (- (frames snd 0) beg)) 2))
	  (flt (make-two-pole 1.0 (- cs) 1.0)))
      (do ((i 0 (+ i 1)))
	  ((= i len))
	(two-pole flt (next-sample reader)))
      (let ((y1 (two-pole flt (next-sample reader)))
	    (y0 (two-pole flt (next-sample reader))))
	(magnitude (- y0 (* y1 (exp (make-rectangular 0.0 (- rfreq))))))))))

#|
;; old version:
    (let ((y2 0.0)
	  (y1 0.0)
	  (y0 0.0)
	  (reader (make-sampler beg snd 0))
	  (len (if (number? dur) dur (- (frames snd 0) beg))))
      (do ((i 0 (+ i 1)))
	  ((= i len))
	(set! y2 y1)
	(set! y1 y0)
	(set! y0 (+ (- (* y1 cs) y2) (next-sample reader))))
|#


(define (make-spencer-filter)
  "(make-spencer-filter) is a version of make-fir-filter; it returns one of the standard smoothing filters from \
the era when computers were human beings"
  (make-fir-filter 15 (apply vct (map (lambda (n) (/ n 320.0)) (list -3 -6 -5 3 21 46 67 74 67 46 21 3 -5 -6 -3)))))


;;; -------- any-random
;;;
;;; arbitrary random number distributions via the "rejection method"

(define* (any-random amount e)
  (if (= amount 0.0)
      0.0
      (if (not e)
	  (random amount)
	  (letrec ((next-random 
		    (lambda ()
		      (let* ((len (length e))
			     (x (random (* 1.0 (e (- len 2)))))
			     (y (random 1.0)))
			(if (<= y (envelope-interp x e))
			    x
			    (next-random))))))
	    (next-random)))))

(define (gaussian-distribution s)
  (let ((e ())
	(den (* 2.0 s s)))
    (do ((i 0 (+ i 1))
	 (x 0.0 (+ x .05))
	 (y -4.0 (+ y .4)))
	((= i 21))
      (set! e (cons x e))
      (set! e (cons (exp (- (/ (* y y) den))) e)))
    (reverse e)))

(define (pareto-distribution a)
  (let ((e ())
	(scl (/ (expt 1.0 (+ a 1.0)) a)))
    (do ((i 0 (+ i 1))
	 (x 0.0 (+ x .05))
	 (y 1.0 (+ y .2)))
	((= i 21))
      (set! e (cons x e))
      (set! e (cons (* scl (/ a (expt y (+ a 1.0)))) e)))
    (reverse e)))

;(map-channel (lambda (y) (any-random 1.0 '(0 1 1 1)))) ; uniform distribution
;(map-channel (lambda (y) (any-random 1.0 '(0 0 0.95 0.1 1 1)))) ; mostly toward 1.0
;(let ((g (gaussian-distribution 1.0))) (map-channel (lambda (y) (any-random 1.0 g))))
;(let ((g (pareto-distribution 1.0))) (map-channel (lambda (y) (any-random 1.0 g))))

;;; this is the inverse integration function used by CLM to turn a distribution function into a weighting function

(define* (inverse-integrate dist (data-size 512) (e-size 50))
  (let* ((e ())
	 (sum (cadr dist))
	 (first-sum sum)
	 (data (make-vct data-size))
	 (x0 (car dist))
	 (x1 (dist (- (length dist) 2)))
	 (xincr (/ (- x1 x0) e-size)))
    (do ((i 0 (+ i 1))
	 (x x0 (+ x xincr)))
	((> i e-size))
      (set! e (cons sum e))
      (set! e (cons x e))
      (set! sum (+ sum (envelope-interp x dist))))
    (let ((incr (/ (- (cadr e) first-sum) (- data-size 1))))
      (set! e (reverse e))
      (do ((i 0 (+ i 1))
	   (x first-sum (+ x incr)))
	  ((= i data-size))
	(set! (data i) (envelope-interp x e)))
      data)))

(define (gaussian-envelope s)
  (let ((e ())
	(den (* 2.0 s s)))
    (do ((i 0 (+ i 1))
	 (x -1.0 (+ x .1))
	 (y -4.0 (+ y .4)))
	((= i 21))
      (set! e (cons x e))
      (set! e (cons (exp (- (/ (* y y) den))) e)))
    (reverse e)))

;;; (make-rand :envelope (gaussian-envelope 1.0))



;;; ---------------- Julius Smith stuff ----------------
;;;
;;; these are from "Mathematics of the DFT", W3K Pubs

(define* (channel-mean snd chn)            ; <f, 1> / n
  "(channel-mean snd chn) returns the average of the samples in the given channel: <f,1>/n"
  (let ((sum 0.0)
	(N (frames snd chn))
	(reader (make-sampler 0 snd chn)))
    (do ((i 0 (+ i 1)))
	((= i N))
      (set! sum (+ sum (next-sample reader))))
    (/ sum N)))

(define* (channel-total-energy snd chn)    ; <f, f>
  "(channel-total-energy snd chn) returns the sum of the squares of all the samples in the given channel: <f,f>"
  (let ((sum 0.0)
	(N (frames snd chn))
	(reader (make-sampler 0 snd chn)))
    (do ((i 0 (+ i 1)))
	((= i N))
      (let ((y (next-sample reader)))
	(set! sum (+ sum (* y y)))))
    sum))

(define* (channel-average-power snd chn)   ; <f, f> / n
  "(channel-average-power snd chn) returns the average power in the given channel: <f,f>/n"
  (/ (channel-total-energy snd chn) (frames snd chn)))

(define* (channel-rms snd chn)             ; sqrt(<f, f> / n)
  "(channel-rms snd chn) returns the RMS value of the samples in the given channel: sqrt(<f,f>/n)"
  (sqrt (channel-average-power snd chn)))

(define* (channel-variance snd chn) ; "sample-variance" might be better, <f, f> - (<f, 1> / n) ^ 2 with quibbles
  "(channel-variance snd chn) returns the sample variance in the given channel: <f,f>-((<f,1>/ n)^2"
  (let* ((N (frames snd chn))
	 (mu (* (/ N (- N 1)) (channel-mean snd chn))) ; avoid bias sez JOS
	 (P (channel-total-energy snd chn)))
    (- P (* mu mu))))

(define* (channel-norm snd chn)            ; sqrt(<f, f>)
  "(channel-norm snd chn) returns the norm of the samples in the given channel: sqrt(<f,f>)"
  (sqrt (channel-total-energy snd chn)))

(define* (channel-lp p snd chn)
  "(channel-lp p snd chn) returns the Lp norm of the samples in the given channel"
  (let ((sum 0.0)
	(N (frames snd chn))
	(reader (make-sampler 0 snd chn)))
    (do ((i 0 (+ i 1)))
	((= i N))
      (set! sum (+ sum (expt (abs (next-sample reader)) p))))
    (expt sum (/ 1.0 p))))

(define* (channel-lp-inf snd chn)
  "(channel-lp-inf snd chn) returns the maxamp in the given channel (the name is just math jargon for maxamp)"
  (let ((mx 0.0)
	(N (frames snd chn))
	(reader (make-sampler 0 snd chn)))
    (do ((i 0 (+ i 1)))
	((= i N))
      (set! mx (max mx (abs (next-sample reader)))))
    mx))

(define (channel2-inner-product s1 c1 s2 c2)         ; <f, g>
  "(channel2-inner-product s1 c1 s2 c2) returns the inner-product of the two channels: <f,g>"
  (let ((N (frames s1 c1))
	(sum 0.0)
	(r1 (make-sampler 0 s1 c1))
	(r2 (make-sampler 0 s2 c2)))
     (do ((i 0 (+ i 1)))
	 ((= i N))
       (set! sum (+ sum (* (r1) (r2)))))
    sum))

(define (channel2-angle s1 c1 s2 c2)                 ; acos(<f, g> / (sqrt(<f, f>) * sqrt(<g, g>)))
  "(channel2-angle s1 c1 s2 c2) treats the two channels as vectors, returning the 'angle' between them: acos(<f,g>/(sqrt(<f,f>)*sqrt(<g,g>)))"
  (let ((inprod (channel2-inner-product s1 c1 s2 c2))
	(norm1 (channel-norm s1 c1))
	(norm2 (channel-norm s2 c2)))
    (acos (/ inprod (* norm1 norm2)))))

(define (channel2-orthogonal? s1 c1 s2 c2)           ; <f, g> == 0
  "(channel2-orthogonal? s1 c1 s2 c2) returns #t if the two channels' inner-product is 0: <f,g>==0"
  (= (channel2-inner-product s1 c1 s2 c2) 0.0))

(define (channel2-coefficient-of-projection s1 c1 s2 c2) ; s1,c1 = x, s2,c2 = y, <f, g> / <f, f>
  "(channel2-coefficient-of-projection s1 c1 s2 c2) returns <f,g>/<f,f>"
  (/ (channel2-inner-product s1 c1 s2 c2)
     (channel-total-energy s1 c1)))

;;; -------- end of JOS stuff --------

#|
(define* (channel-distance-1 (s1 0) (c1 0) (s2 1) (c2 0))    ; sqrt(<f - g, f - g>)
  "(channel-distance s1 c1 s2 c2) returns the euclidean distance between the two channels: sqrt(<f-g,f-g>)"
  (let ((r1 (make-sampler 0 s1 c1))
	(r2 (make-sampler 0 s2 c2))
	(sum 0.0)
	(N (min (frames s1 c1) (frames s2 c2)))
	(diff 0.0))
     (do ((i 0 (+ i 1)))
	 ((= i N))
       (set! diff (- (r1) (r2)))
       (set! sum (+ sum (* diff diff))))
    (sqrt sum)))
|#
(define* (channel-distance (s1 0) (c1 0) (s2 1) (c2 0))    ; sqrt(<f - g, f - g>)
  "(channel-distance s1 c1 s2 c2) returns the euclidean distance between the two channels: sqrt(<f-g,f-g>)"
  (let ((r1 (make-sampler 0 s1 c1))
	(r2 (make-sampler 0 s2 c2))
	(N (min (frames s1 c1) (frames s2 c2)))
	(data1 #f)
	(data2 #f))
    (set! data1 (make-vct N))
    (set! data2 (make-vct N))
    (do ((i 0 (+ i 1)))
	((= i N))
      (vct-set! data1 i (next-sample r1)))
    (do ((i 0 (+ i 1)))
	((= i N))
      (vct-set! data2 i (next-sample r2)))
    (vct-subtract! data1 data2)
    (sqrt (dot-product data1 data1))))


(define (periodogram N)
  "(periodogram N) displays an 'N' point Bartlett periodogram of the samples in the current channel"
  (let* ((len (frames))
	 (average-data (make-vct N))
	 (rd (make-sampler 0))
	 (N2 (* 2 N))
	 (rl (make-vct N2))
	 (im (make-vct N2)))
     (do ((i 0 (+ i N)))
	 ((>= i len))
       (vct-scale! rl 0.0)
       (vct-scale! im 0.0)
       (do ((k 0 (+ k 1)))
	   ((= k N))
	 (set! (rl k) (rd)))
       (mus-fft rl im)
       (do ((k 0 (+ k 1)))
	   ((= k N))
	 (set! (average-data k) (+ (average-data k) 
				   (* (rl k) (rl k)) 
				   (* (im k) (im k))))))
     ;; perhaps faster -- use vct-add! and vct-multiply!
     ;; or add snd-spectrum results
    (graph (vct-scale! average-data (/ 1.0 (ceiling (/ len N)))))))


;;; -------- ssb-am friends

(define* (shift-channel-pitch freq (order 40) (beg 0) dur snd chn edpos)
  "(shift-channel-pitch freq (order 40) (beg 0) dur snd chn edpos) uses the ssb-am CLM generator to \
shift the given channel in pitch without changing its length.  The higher 'order', the better usually."
  ;; higher order = better cancellation
  (let ((gen (make-ssb-am freq order)))
    (map-channel (lambda (y) 
		   (ssb-am gen y)) 
		 beg dur snd chn edpos 
		 (format #f "shift-channel-pitch ~A ~A ~A ~A" freq order beg dur))))

(define (hz->2pi freq)
  "(hz->2pi freq) is like hz->radians but uses the current sound's srate, not mus-srate"
  (/ (* 2 pi freq) (srate))) 

(define* (ssb-bank old-freq new-freq pairs (order 40) (bw 50.0) (beg 0) dur snd chn edpos)
  (let ((ssbs (make-vector pairs))
	(bands (make-vector pairs))
	(factor (/ (- new-freq old-freq) old-freq)))
    (do ((i 1 (+ i 1)))
	((> i pairs))
      (let ((aff (* i old-freq))
	    (bwf (* bw (+ 1.0 (/ i (* 2 pairs))))))
	(set! (ssbs (- i 1)) (make-ssb-am (* i factor old-freq)))
	(set! (bands (- i 1)) (make-bandpass (hz->2pi (- aff bwf)) 
					     (hz->2pi (+ aff bwf)) 
					     order))))
    (let ((data (channel->vct beg dur snd chn edpos)))
      (let ((len (length data))
	    (mx (vct-peak data)))
	(let ((summer (make-vct len 0.0))
	      (adder (make-vct len 0.0)))
	  (do ((i 0 (+ i 1)))
	      ((= i pairs))
	    (let ((gen (vector-ref ssbs i))
		  (filt (vector-ref bands i)))
	      (do ((k 0 (+ k 1)))
		  ((= k len))
		(vct-set! adder k (ssb-am gen (bandpass filt (vct-ref data k)))))
	      (vct-add! summer adder)
	      (fill! adder 0.0)))
	  (vct-scale! summer (/ mx (vct-peak summer)))
	  (vct->channel summer beg len snd chn current-edit-position
			(format #f "ssb-bank ~A ~A ~A ~A ~A ~A ~A" old-freq new-freq pairs order bw beg dur)))))))

;;; (let ((ind (open-sound "oboe.snd"))) (ssb-bank 550.0 660.0 10))


(define* (ssb-bank-env old-freq new-freq freq-env pairs (order 40) (bw 50.0) (beg 0) dur snd chn edpos)
  ;; this version adds a frequency envelope
  ;; (ssb-bank-env 557 880 '(0 0 1 100.0) 7)
  (let ((ssbs (make-vector pairs))
	(bands (make-vector pairs))
	(factor (/ (- new-freq old-freq) old-freq))
	(frenvs (make-vector pairs)))
    (do ((i 1 (+ i 1)))
	((> i pairs))
      (let ((aff (* i old-freq))
	    (bwf (* bw (+ 1.0 (/ i (* 2 pairs))))))
	(set! (ssbs (- i 1)) (make-ssb-am (* i factor old-freq)))
	(set! (bands (- i 1)) (make-bandpass (hz->2pi (- aff bwf)) 
					     (hz->2pi (+ aff bwf)) 
					     order))
	(set! (frenvs (- i 1)) (make-env freq-env 
					 :scaler (hz->radians i) 
					 :length (frames)))))
    (let ((data (channel->vct beg dur snd chn edpos)))
      (let ((len (length data))
	    (mx (vct-peak data)))
	(let ((summer (make-vct len 0.0))
	      (adder (make-vct len 0.0)))
	  (do ((i 0 (+ i 1)))
	      ((= i pairs))
	    (let ((gen (vector-ref ssbs i))
		  (filt (vector-ref bands i))
		  (e (vector-ref frenvs i)))
	      (do ((k 0 (+ k 1)))
		  ((= k len))
		(vct-set! adder k (ssb-am gen (bandpass filt (vct-ref data k)) (env e))))
	      (vct-add! summer adder)
	      (fill! adder 0.0)))
	  (vct-scale! summer (/ mx (vct-peak summer)))
	  (vct->channel summer beg len snd chn current-edit-position
			(format #f "ssb-bank-env ~A ~A '~A ~A ~A ~A ~A ~A" old-freq new-freq freq-env pairs order bw beg dur)))))))

;;; (let ((ind (open-sound "oboe.snd"))) (ssb-bank-env 550 600 '(0 1 1 2) 10))
#|
;;; a "bump function" (Stein and Shakarchi)
(define (bumpy)
  (let* ((x 0.0) 
	 (xi (/ 1.0 (frames)))
	 (start 0)
	 (end 1)
	 (scl (exp (/ 4.0 (- end start))))) ; normalize it
    (map-channel (lambda (y) 
		   (let ((val (if (or (<= x start) ; don't divide by zero
				      (>= x end))
				  0.0
				  (* (exp (/ -1.0 (- x start))) 
				     (exp (/ -1.0 (- end x)))))))
		     (set! x (+ x xi))
		     (* scl val))))))
|#


;;; vct|channel|spectral-polynomial

(define (vct-polynomial v coeffs)
  ;; Horner's rule applied to entire vct
  (let* ((v-len (length v))
	 (num-coeffs (length coeffs))
	 (new-v (make-vct v-len (coeffs (- num-coeffs 1)))))
    (do ((i (- num-coeffs 2) (- i 1)))
	((< i 0))
      (vct-offset! (vct-multiply! new-v v) (coeffs i)))
    new-v))

(define* (channel-polynomial coeffs snd chn)
  (let ((len (frames snd chn)))
    (vct->channel 
     (vct-polynomial 
      (channel->vct 0 len snd chn) 
      coeffs) 
     0 len snd chn #f (format #f "channel-polynomial ~A" (vct->string coeffs)))))

;;; (channel-polynomial (vct 0.0 .5)) = x*.5
;;; (channel-polynomial (vct 0.0 1.0 1.0 1.0)) = x*x*x + x*x + x

;;; convolution -> * in freq

(define* (spectral-polynomial coeffs snd chn)
  (let* ((len (frames snd chn))
	 (sound (channel->vct 0 len snd chn))
	 (num-coeffs (length coeffs))
	 (fft-len (if (< num-coeffs 2) 
		      len 
		      (expt 2 (ceiling (log (* (- num-coeffs 1) len) 2)))))
	 (rl1 (make-vct fft-len 0.0))
	 (rl2 (make-vct fft-len 0.0))
	 (new-sound (make-vct fft-len)))
    (if (> (coeffs 0) 0.0)
	(let ((dither (coeffs 0)))
	  (do ((i 0 (+ i 1)))
	      ((= i fft-len))
	    (set! (new-sound i) (mus-random dither)))))
    (if (> num-coeffs 1)
	(begin
	  (vct-add! new-sound (vct-scale! (vct-copy sound) (coeffs 1)))
	  (if (> num-coeffs 2)
	      (let ((peak (maxamp snd chn)))
		(vct-add! (vct-scale! rl1 0.0) sound)
		(do ((i 2 (+ i 1)))
		    ((= i num-coeffs))
		  (convolution rl1 (vct-add! (vct-scale! rl2 0.0) sound) fft-len)
		  (let ((pk (vct-peak rl1)))
		    (vct-add! new-sound (vct-scale! (vct-copy rl1) (/ (* (coeffs i) peak) pk)))))
		(let ((pk (vct-peak new-sound)))
		  (vct-scale! new-sound (/ peak pk)))))))
    (vct->channel new-sound 0 (max len (* len (- num-coeffs 1))) snd chn #f (format #f "spectral-polynomial ~A" (vct->string coeffs)))))


;;; ----------------
;;; SCENTROID
;;;
;;; by Bret Battey
;;; Version 1.0 July 13, 2002
;;; translated to Snd/Scheme Bill S 19-Jan-05
;;;
;;; Returns the continuous spectral centroid envelope of a sound.
;;; The spectral centroid is the "center of gravity" of the spectrum, and it
;;; has a rough correlation to our sense of "brightness" of a sound. 
;;;
;;; [Beauchamp, J., "Synthesis by spectral amplitude and 'brightness' matching
;;; analyzed musical sounds". Journal of Audio Engineering Society 30(6), 396-406]
;;;
;;; The formula used is:
;;;    C = [SUM<n=1toj>F(n)A(n)] / [SUM<n=1toj>A(n)]
;;;    Where j is the number of bins in the analysis, 
;;;    F(n) is the frequency of a given bin,
;;;    A(n) is the magnitude of the given bin.
;;;
;;; If a pitch envelope for the analyzed sound is available, the results
;;; of SCENTROID can be used with the function NORMALIZE-CENTROID, below, 
;;; to provide a "normalized spectral centroid". 
;;;
;;; DB-FLOOR -- Frames below this decibel level (0 dB = max) will be discarded
;;; and returned with spectral centroid = 0
;;;
;;; RFREQ -- Rendering frequency. Number of  measurements per second.
;;;
;;; FFTSIZE -- FFT window size. Must be a power of 2. 4096 is recommended.

(define* (scentroid file (beg 0.0) dur (db-floor -40.0) (rfreq 100.0) (fftsize 4096))
  "(scentroid file (beg 0.0) dur (db-floor -40.0) (rfreq 100.0) (fftsize 4096)) returns the spectral centroid envelope of a sound; 'rfreq' is \
the rendering frequency, the number of measurements per second; 'db-floor' is the level below which data will be ignored"
  (let* ((fsr (srate file))
	 (incrsamps (floor (/ fsr rfreq)))
	 (start (floor (* beg fsr)))
	 (end (+ start (if dur (floor (* dur fsr)) (- (frames file) beg))))
	 (fdr (make-vct fftsize))
	 (fdi (make-vct fftsize))
	 (scl (make-vct (/ fftsize 2)))
	 (ones (make-vct (/ fftsize 2) 1.0))
	 (windows (+ 1 (floor (/ (- end start) incrsamps))))
	 (results (make-vct windows))
	 (fft2 (floor (/ fftsize 2)))
	 (binwidth (* 1.0 (/ fsr fftsize)))
	 (rd (make-readin file)))
    (do ((k 0 (+ k 1)))
	((= k fft2))
      (vct-set! scl k (* k binwidth)))
    (do ((i start (+ i incrsamps))
	 (loc 0 (+ 1 loc)))
	((>= i end))
      (set! (mus-location rd) i)
      (do ((j 0 (+ j 1)))
	  ((= j fftsize))
	(vct-set! fdr j (readin rd)))
      (if (>= (linear->db (sqrt (/ (dot-product fdr fdr) fftsize))) db-floor)
	  (begin
	    (clear-array fdi)
	    (mus-fft fdr fdi fftsize)
	    (rectangular->magnitudes fdr fdi)
	    (set! (results loc) (/ (dot-product scl fdr fft2) 
				   (dot-product ones fdr fft2))))))
    results))
	     

;;; ----------------
;;;
;;; invert-filter inverts an FIR filter
;;;
;;; say we previously filtered a sound via (filter-channel (vct .5 .25 .125))
;;;   and we want to undo it without using (undo):
;;;   (filter-channel (invert-filter (vct .5 .25 .125)))
;;;
;;; there are a million gotchas here.  The primary one is that the inverse filter
;;;   can "explode" -- the coefficients can grow without bound.  For example, any
;;;   filter returned by spectrum->coeffs above will be a problem (it always returns
;;;   a "linear phase" filter).  Could this be used to remove reverb?

(define (invert-filter fcoeffs)
  "(invert-filter coeffs) tries to return an inverse filter to undo the effect of the FIR filter coeffs."
  (let* ((flen (length fcoeffs))
	 (coeffs (make-vct (+ 32 flen))) ; add room for coeffs to die away
	 (order (length coeffs)))
    (do ((i 0 (+ i 1)))
	((= i flen))
      (set! (coeffs i) (fcoeffs i)))
    (let ((nfilt (make-vct order)))
      (set! (nfilt 0) (/ 1.0 (coeffs 0)))
      (do ((i 1 (+ i 1)))
	  ((= i order))
	(let ((sum 0.0))
	  (do ((j 0 (+ j 1))
	       (k i (- k 1)))
	      ((= j i))
	    (set! sum (+ sum (* (nfilt j) (coeffs k)))))
	  (set! (nfilt i) (/ sum (- (coeffs 0))))))
      nfilt)))


;;; ----------------
;;;
;;; Volterra filter
;;;
;;; one of the standard non-linear filters
;;; this version is taken from Monson Hayes "Statistical DSP and Modeling"
;;;   it is a slight specialization of the form mentioned by J O Smith and others

(define (make-volterra-filter acoeffs bcoeffs)
  "(make-volterra-filter acoeffs bcoeffs) returns a list for use with volterra-filter, producing one of the standard non-linear filters"
  (list acoeffs 
	bcoeffs 
	(make-vct (max (length acoeffs) (length bcoeffs)))))

(define (volterra-filter flt x)
  "(volterra-filter flt x) takes 'flt', a list returned by make-volterra-filter, and an input 'x', and returns the (non-linear filtered) result"
  (let* ((as (car flt))
	 (bs (cadr flt))
	 (xs (caddr flt))
	 (xlen (length xs))
	 (x1len (length as))
	 (x2len (length bs))
	 (sum 0.0))
    (vct-move! xs (- xlen 1) (- xlen 2) #t)
    (set! (xs 0) x)
    (set! sum (dot-product as xs x1len))
    (do ((i 0 (+ i 1)))
	((= i x2len))
      (do ((j i (+ j 1)))
	  ((= j x2len))
	(set! sum (+ sum (* (bs j) (xs i) (xs j))))))
    sum))

;;; (define flt (make-volterra-filter (vct .5 .1) (vct .3 .2 .1)))
;;; (map-channel (lambda (x) (volterra-filter flt x)))



;;; ----------------
;;;
;;; harmonicizer (each harmonic is split into a set of harmonics via Chebyshev polynomials)
;;;   obviously very similar to ssb-bank above, but splits harmonics individually, rather than pitch-shifting them

(define* (harmonicizer freq coeffs pairs (order 40) (bw 50.0) (beg 0) dur snd chn edpos)
  "(harmonicizer freq coeffs pairs (order 40) (bw 50.0) (beg 0) dur snd chn edpos) splits out each harmonic \
and replaces it with the spectrum given in coeffs"
  (let ((bands (make-vector pairs))
	(pcoeffs (partials->polynomial coeffs))
	(avgs (make-vector pairs))
	(peaks (make-vector pairs))
	(flt (make-filter 2 (vct 1 -1) (vct 0 -0.9)))
	(old-mx (maxamp))
	(startup 40)
	(len (- (or dur (frames snd chn edpos)) beg)))
    (let ((adder (make-vct len))
	  (summer (make-vct len))
	  (indata (channel->vct beg len snd chn edpos)))
      
      (do ((i 0 (+ i 1)))
	  ((= i pairs))
	(let ((aff (* (+ i 1) freq))
	      (bwf (* bw (+ 1.0 (/ (+ i 1) (* 2 pairs))))))
	  (set! (peaks i) (make-moving-max 128))
	  (set! (avgs i) (make-moving-average 128))
	  (set! (bands i) (make-bandpass (hz->2pi (- aff bwf)) 
					 (hz->2pi (+ aff bwf)) 
					 order))))
      ;; ignore startup
      (do ((k 0 (+ k 1)))
	  ((= k startup))
	(let ((sum 0.0))
	  (do ((i 0 (+ i 1)))
	      ((= i pairs))
	    (let* ((sig (bandpass (vector-ref bands i) (vct-ref indata k)))
		   (mx (moving-max (vector-ref peaks i) sig)))
	      (set! sum (+ sum (* mx (polynomial pcoeffs (* sig (moving-average (vector-ref avgs i) (/ (max mx 0.01))))))))))
	  (filter flt sum)))

      (do ((pair 0 (+ pair 1)))
	  ((= pair pairs))
	(let ((bp (vector-ref bands pair))
	      (pk (vector-ref peaks pair))
	      (avg (vector-ref avgs pair)))
	  (fill! adder 0.0)
	  (do ((k startup (+ k 1)))
	      ((= k len))
	    (let* ((sig (bandpass bp (vct-ref indata k)))
		   (mx (moving-max pk sig)))
	      (vct-set! adder k (* mx (polynomial pcoeffs (* sig (moving-average avg (/ (max mx 0.01)))))))))
	  (vct-add! summer adder)))

      (do ((k startup (+ k 1)))
	  ((= k len))
	(vct-set! summer k (filter flt (vct-ref summer k))))
      
      (let ((nmx (vct-peak summer)))
	(if (> nmx 0.0)
	    (vct-scale! summer (/ old-mx nmx))))
      (vct->channel summer beg len snd chn))))

;;; (harmonicizer 550.0 (list 1 .5 2 .3 3 .2) 10)



;;; ----------------
;;;
;;; linear sampling rate conversion

(define* (linear-src-channel sr snd chn)
  "(linear-src-channel sr snd chn) performs sampling rate conversion using linear interpolation."
  (let* ((rd (make-sampler 0 snd chn))
	 (last (rd))
	 (next (rd))
	 (intrp 0.0)
	 (tempfile 
	  (with-sound (:output (snd-tempnam) :srate (srate snd) :to-snd #f)
	     (do ((samp 0 (+ samp 1)))
		 ((sampler-at-end? rd))
	       (outa samp
		     (let ((pos intrp))
		       (if (>= pos 1.0)
			   (let ((num (floor pos)))
			     (do ((i 0 (+ i 1)))
				 ((= i num))
			       (set! last next)
			       (set! next (read-sample rd)))
			     (set! pos (- pos num))))
		       (set! intrp (+ pos sr))
		       (+ last (* pos (- next last))))
		     ))))
	 (len (frames tempfile)))
    (set-samples 0 (- len 1) tempfile snd chn #t "linear-src" 0 #f #t)
    ;; first #t=truncate to new length, #f=at current edpos, #t=auto delete temp file
    ))


;;; -------- spectrum displayed in various frequency scales

(define display-bark-fft
  ;; click in lisp-graph to change the tick placement choice

  (let ((snd-color-1 (lambda args
		       (if (defined? 'snd-color)
			   (apply snd-color args)
			   #f)))) ; no-gui case I guess
		       
    (let ((bark-fft-size 0)
	  (bark-tick-function 0)
	  (color1 (snd-color-1 8))  ; selected-data-color
	  (color2 (snd-color-1 2))  ; red
	  (color3 (snd-color-1 4))) ; blue
      
      (define (bark f) 
	(let ((f2 (/ f 7500))) 
	  (+ (* 13.5 (atan (* .00076 f))) (* 3.5 (atan (* f2 f2))))))
      
      (define (mel f) 
	(* 1127 (log (+ 1.0 (/ f 700.0)))))
      
      (define (erb f) 
	(+ 43.0 (* 11.17 (log (/ (+ f 312) (+ f 14675))))))
      
      (define (display-bark-fft-1 hook)
	(let* ((snd (hook 'snd))
	       (chn (hook 'chn))
	       (ls (left-sample snd chn))
	       (rs (right-sample snd chn))
	       (fftlen (floor (expt 2 (ceiling (log (+ 1 (- rs ls)) 2))))))
	  (if (> fftlen 0)
	      (let ((data (channel->vct ls fftlen snd chn))
		    (normalized (not (= (transform-normalization snd chn) dont-normalize)))
		    (linear #t))                               ; can't currently show lisp graph in dB 
		;; snd-axis make_axes: WITH_LOG_Y_AXIS, but LINEAR currently in snd-chn.c 3250
		(if (vct? data)
		    (let ((fftdata (snd-spectrum data              ; returns fftlen / 2 data points
						 (fft-window snd chn) fftlen linear 
						 (fft-window-beta snd chn) #f normalized)))
		      (if (vct? fftdata)
			  (let* ((sr (srate snd))
				 (mx (vct-peak fftdata))
				 (data-len (length fftdata))
				 
				 ;; bark settings
				 (bark-low (floor (bark 20.0)))
				 (bark-high (ceiling (bark (* 0.5 sr))))
				 (bark-frqscl (/ data-len (- bark-high bark-low)))
				 (bark-data (make-vct data-len))
				 
				 ;; mel settings
				 (mel-low (floor (mel 20.0)))
				 (mel-high (ceiling (mel (* 0.5 sr))))
				 (mel-frqscl (/ data-len (- mel-high mel-low)))
				 (mel-data (make-vct data-len))
				 
				 ;; erb settings
				 (erb-low (floor (erb 20.0)))
				 (erb-high (ceiling (erb (* 0.5 sr))))
				 (erb-frqscl (/ data-len (- erb-high erb-low)))
				 (erb-data (make-vct data-len)))
			    
			    (set! bark-fft-size fftlen)
			    
			     (do ((i 0 (+ i 1)))
				 ((= i data-len))
			       (let* ((val (fftdata i))
				      (frq (* sr (/ i fftlen)))
				      (bark-bin (round (* bark-frqscl (- (bark frq) bark-low))))
				      (mel-bin (round (* mel-frqscl (- (mel frq) mel-low))))
				      (erb-bin (round (* erb-frqscl (- (erb frq) erb-low)))))
				 (if (and (>= bark-bin 0)
					  (< bark-bin data-len))
				     (set! (bark-data bark-bin) (+ val (bark-data bark-bin))))
				 (if (and (>= mel-bin 0)
					  (< mel-bin data-len))
				     (set! (mel-data mel-bin) (+ val (mel-data mel-bin))))
				 (if (and (>= erb-bin 0)
					  (< erb-bin data-len))
				     (set! (erb-data erb-bin) (+ val (erb-data erb-bin))))))
			     
			     (if normalized
				 (let ((bmx (vct-peak bark-data))
				       (mmx (vct-peak mel-data))
				       (emx (vct-peak erb-data)))
				   (if (> (abs (- mx bmx)) .01)
				       (vct-scale! bark-data (/ mx bmx)))
				   (if (> (abs (- mx mmx)) .01)
				       (vct-scale! mel-data (/ mx mmx)))
				   (if (> (abs (- mx emx)) .01)
				       (vct-scale! erb-data (/ mx emx)))))
			    
			    (graph (list bark-data mel-data erb-data) 
				   "ignored" 
				   20.0 (* 0.5 sr) 
				   0.0 (if normalized 1.0 (* data-len (y-zoom-slider snd chn)))
				   snd chn 
				   #f show-bare-x-axis)))))))
	  
	  (list color1 color2 color3))) ; tell lisp graph display what colors to use
      
      (define (make-bark-labels hook)
	;; at this point the x axis has no markings, but there is room for labels and ticks
	(let* ((snd (hook 'snd))
	       (chn (hook 'chn))
	       (old-foreground-color (foreground-color snd chn copy-context)))
	  
	  (let* ((axinfo (axis-info snd chn lisp-graph))
		 (axis-x0 (axinfo 10))
		 (axis-x1 (axinfo 12))
		 (axis-y0 (axinfo 13))
		 (axis-y1 (axinfo 11))
		 (label-height 15)
		 (char-width 8)
		 (sr2 (* 0.5 (srate snd)))
		 (minor-tick-len 6)
		 (major-tick-len 12)
		 (tick-y0 axis-y1)
		 (minor-y0 (+ axis-y1 minor-tick-len))
		 (major-y0 (+ axis-y1 major-tick-len))
		 (bark-label-font (snd-font 3))
		 (bark-numbers-font (snd-font 2))
		 (label-pos (+ axis-x0 (* .45 (- axis-x1 axis-x0))))
		 (cr (make-cairo (car (channel-widgets snd chn)))))
	    
	    (define (scale-position scale f)
	      (let ((b20 (scale 20.0)))
		(round (+ axis-x0 
			  (/ (* (- axis-x1 axis-x0) (- (scale f) b20)) 
			     (- (scale sr2) b20))))))
	    
	    (define (bark-position f) (scale-position bark f))
	    (define (mel-position f) (scale-position mel f))
	    (define (erb-position f) (scale-position erb f))
	    
	    (define (draw-bark-ticks bark-function)
	      (if bark-numbers-font (set! (current-font snd chn copy-context) bark-numbers-font))
	      
	      (draw-line axis-x0 tick-y0 axis-x0 major-y0 snd chn copy-context cr)
	      (let ((i1000 (scale-position bark-function 1000.0))
		    (i10000 (scale-position bark-function 10000.0)))
		
		(draw-line i1000 tick-y0 i1000 major-y0 snd chn copy-context cr)
		(draw-line i10000 tick-y0 i10000 major-y0 snd chn copy-context cr)
		
		(draw-string "20" axis-x0 major-y0 snd chn copy-context cr)
		(draw-string "1000" (- i1000 (* 3 4)) major-y0 snd chn copy-context cr)
		(draw-string "10000" (- i10000 (* 6 4)) major-y0 snd chn copy-context cr)
		
		(draw-string (format #f "fft size: ~D" bark-fft-size) (+ axis-x0 10) axis-y0 snd chn copy-context cr)
		
		(do ((i 100 (+ i 100)))
		    ((= i 1000))
		  (let ((i100 (scale-position bark-function i)))
		    (draw-line i100 tick-y0 i100 minor-y0 snd chn copy-context cr)))
		
		(do ((i 2000 (+ i 1000)))
		    ((= i 10000))
		  (let ((i1000 (scale-position bark-function i)))
		    (draw-line i1000 tick-y0 i1000 minor-y0 snd chn copy-context cr)))))
	    
	    ;; bark label/ticks
	    (set! (foreground-color snd chn copy-context) color1)
	    (if (= bark-tick-function 0) (draw-bark-ticks bark-position))
	    (if bark-label-font (set! (current-font snd chn copy-context) bark-label-font))
	    (draw-string "bark," label-pos (+ axis-y1 label-height) snd chn copy-context cr)
	    
	    ;; mel label/ticks
	    (set! (foreground-color snd chn copy-context) color2)
	    (if (= bark-tick-function 1) (draw-bark-ticks mel-position))
	    (if bark-label-font (set! (current-font snd chn copy-context) bark-label-font))
	    (draw-string "mel," (+ (* char-width 6) label-pos) (+ axis-y1 label-height) snd chn copy-context cr)
	    
	    ;; erb label/ticks
	    (set! (foreground-color snd chn copy-context) color3)
	    (if (= bark-tick-function 2) (draw-bark-ticks erb-position))
	    (if bark-label-font (set! (current-font snd chn copy-context) bark-label-font))
	    (draw-string "erb" (+ (* char-width (+ 6 5)) label-pos) (+ axis-y1 label-height) snd chn copy-context cr)
	    (free-cairo cr))
	  
	  (set! (foreground-color snd chn copy-context) old-foreground-color)))
      
      ;; mouse click = move to next scale's ticks
      (define (choose-bark-ticks hook)
	(if (= (hook 'axis) lisp-graph)
	    (begin
	      (set! bark-tick-function (+ bark-tick-function 1))
	      (if (> bark-tick-function 2)
		  (set! bark-tick-function 0))
	      (update-lisp-graph (hook 'snd) (hook 'chn)))))
      
      ;; user's view of display-bark-fft function
      (lambda* (off col1 col2 col3)
	       (if col1 (set! color1 col1))
	       (if col2 (set! color2 col2))
	       (if col3 (set! color3 col3))
	       (if (not off)
		   (begin
		     (hook-push lisp-graph-hook display-bark-fft-1)
		     (hook-push after-lisp-graph-hook make-bark-labels)
		     (hook-push mouse-click-hook choose-bark-ticks)
		     (for-each (lambda (snd)
				 (do ((c 0 (+ 1 c)))
				     ((= c (channels snd)))
				   (update-lisp-graph snd c)))
			       (sounds)))
		   (begin
		     (hook-remove lisp-graph-hook display-bark-fft-1)
		     (hook-remove after-lisp-graph-hook make-bark-labels)
		     (hook-remove mouse-click-hook choose-bark-ticks)
		     (for-each (lambda (snd)
				 (do ((c 0 (+ 1 c)))
				     ((= c (channels snd)))
				   (set! (lisp-graph? snd c) #f)))
			       (sounds))))))))

(define (undisplay-bark-fft) (display-bark-fft #t))



;;; -------- lpc-coeffs, lpc-predict

(define (lpc-coeffs data n m)
  ;; translated and changed to use 0-based arrays from memcof of NRinC
  
  "(lpc-coeffs data n m) returns 'm' LPC coeffients (in a vector) given 'n' data points in the vct 'data'"
  
  (let ((d (make-vector m 0.0))
	(wk1 (make-vector n 0.0))
	(wk2 (make-vector n 0.0))
	(wkm (make-vector n 0.0)))
    (set! (wk1 0) (data 0))
    (set! (wk2 (- n 2)) (data (- n 1)))
    (do ((j 1 (+ j 1)))
	((= j (- n 1)))
      (set! (wk1 j) (data j))
      (set! (wk2 (- j 1)) (data j)))
    (do ((k 0 (+ k 1)))
	((= k m) d)
      (let ((num 0.0)
	    (denom 0.0)
	    (end (- n k 1)))
	(do ((j 0 (+ j 1)))
	    ((= j end))
	  (let ((x1 (vector-ref wk1 j))
		(x2 (vector-ref wk2 j)))
	    (set! num (+ num (* x1 x2)))
	    (set! denom (+ denom (* x1 x1) (* x2 x2)))))
	(if (not (= denom 0.0))
	    (set! (d k) (/ (* 2.0 num) denom)))
	(do ((i 0 (+ i 1)))
	    ((= i k)) ; 1st time is skipped presumably
	  (set! (d i) (- (wkm i) (* (d k) (wkm (- k i 1))))))
	(if (< k (- m 1))
	    (let ((end (- n k 2)))
	      (do ((i 0 (+ i 1)))
		  ((= i (+ k 1)))
		(vector-set! wkm i (vector-ref d i)))
	      (do ((j 0 (+ j 1)))
		  ((= j end))
		(set! (wk1 j) (- (wk1 j) (* (wkm k) (wk2 j))))
		(set! (wk2 j) (- (wk2 (+ j 1)) (* (wkm k) (wk1 (+ j 1))))))))))))

(define* (lpc-predict data n coeffs m nf clipped)
  ;; translated and changed to use 0-based arrays from predic of NRinC
  ;; incoming coeffs are assumed to be in a vector (from lpc-coeffs)

  "(lpc-predict data n coeffs m nf clipped) takes the output of lpc-coeffs ('coeffs', a vector) and the length thereof ('m'), \
'n' data points of 'data' (a vct), and produces 'nf' new data points (in a vct) as its prediction. If 'clipped' is #t, the new data \
is assumed to be outside -1.0 to 1.0."

  (let ((future (make-vct nf 0.0))
	(reg (make-vct m 0.0)))
     (do ((i 0 (+ i 1))
	  (j (- n 1) (- j 1)))
	 ((= i m))
       (set! (reg i) (data j)))
     (do ((j 0 (+ j 1)))
	 ((= j nf) future)
       (let ((sum 0.0))
	 (do ((k 0 (+ k 1)))
	     ((= k m))
	   (set! sum (+ sum (* (coeffs k) (reg k)))))
	 (do ((k (- m 1) (- k 1)))
	     ((= k 0))
	   (set! (reg k) (reg (- k 1))))
	 
	 ;; added this block
	 (if clipped
	     (if (> sum 0.0)
		 (if (< sum 1.0)
		     (set! sum 1.0))
		 (if (> sum -1.0)
		     (set! sum -1.0))))
	 
	 (set! (reg 0) sum)
	 (set! (future j) sum)))))


;;; -------- unclip-channel

(define* (unclip-channel snd chn)
  "(unclip-channel snd chn) looks for clipped portions and tries to reconstruct the original using LPC"
  (let ((clips 0)                              ; number of clipped portions * 2
	(unclipped-max 0.0))

    ;; count clipped portions
    (let ((in-clip #f)
	  (len (frames snd chn))
	  (reader (make-sampler 0 snd chn)))
      (do ((i 0 (+ i 1)))
	  ((= i len))
	(let ((absy (abs (next-sample reader))))
	  (if (> absy .9999)                    ; this sample is clipped
	      (if (not in-clip)
		  (set! in-clip #t))
	      (begin                            ; not clipped
		(set! unclipped-max (max unclipped-max absy))
		(if in-clip
		    (begin
		      (set! in-clip #f)
		      (set! clips (+ clips 2)))))))))

    (if (> clips 0)                             ; we did find clipped portions
	(let ((clip-data (make-vector clips 0))  ; clipped portion begin and end points
	      (clip-beg 0)
	      (in-clip #f)
	      (cur-clip 0)
	      (samp 0)
	      (len (frames snd chn))
	      (reader (make-sampler 0 snd chn)))
	  (do ((i 0 (+ i 1)))
	      ((= i len))
	    (let ((absy (abs (next-sample reader))))
	      (if (> absy .9999)
		  (if (not in-clip)
		      (begin
			(set! in-clip #t)
			(set! clip-beg samp)))
		  (begin                            ; not clipped
		    (if in-clip                     ; if we were in a clipped portion
			(begin                      ;   save the bounds in clip-data
			  (set! in-clip #f)
			  (set! (clip-data cur-clip) clip-beg)
			  (set! (clip-data (+ 1 cur-clip)) (- samp 1))
			  (set! cur-clip (+ cur-clip 2))))))
	      (set! samp (+ samp 1))))
	  
	  ;; try to restore clipped portions
	  
	  (let ((min-data-len 32)
		(max-len 0))
	    (as-one-edit
	     (lambda ()
	       (do ((clip 0 (+ clip 2)))               ;   so go through all...
		   ((>= clip clips))
		 (let* ((clip-beg (clip-data clip))  ; clip-beg to clip-end inclusive are clipped
			(clip-end (clip-data (+ 1 clip)))
			(clip-len (+ 1 (- clip-end clip-beg)))
			(data-len (max min-data-len (* clip-len 4))))
		   
		   (if (> clip-len max-len) 
		       (set! max-len clip-len))
		   
		   (let ((forward-data-len data-len)
			 (backward-data-len data-len)
			 (previous-end (if (= clip 0) 0 (clip-data (- clip 1))))
			 (next-beg (if (< clip (- clips 3)) (clip-data (+ clip 2)) (frames snd chn))))
		     
		     (if (< (- clip-beg data-len) previous-end)  ; current beg - data collides with previous
			 (begin
			   ;; (format #t ";[~A] collision at ~A -> [~A : ~A]" clip previous-end clip-beg clip-end)
			   (set! forward-data-len (max 4 (- clip-beg previous-end)))))
		     
		     (if (> (+ clip-end data-len) next-beg)    ; current end + data collides with next
			 (begin
			   ;; (format #t ";[~A] collision at [~A : ~A] -> ~A" clip clip-beg clip-end next-beg)
			   (set! backward-data-len (max 4 (- next-beg clip-end)))))
		     
		     (let ((forward-predict-len (min (max clip-len (floor (/ forward-data-len 2))) forward-data-len))
			   (backward-predict-len (min (max clip-len (floor (/ backward-data-len 2))) backward-data-len)))
		       
		       ;; use LPC to reconstruct going both forwards and backwards
		       
		       (let* ((data (channel->vct (- clip-beg forward-data-len) forward-data-len snd chn))
			      (future (lpc-predict 
				       data forward-data-len 
				       (lpc-coeffs data forward-data-len forward-predict-len)
				       forward-predict-len
				       clip-len #f))
			      
			      (rdata (vct-reverse! (channel->vct (+ 1 clip-end) backward-data-len snd chn)))
			      (past (lpc-predict 
				     rdata backward-data-len 
				     (lpc-coeffs rdata backward-data-len backward-predict-len)
				     backward-predict-len
				     clip-len #f))
			      
			      (new-data (make-vct clip-len 0.0)))
			 
			 (if (> clip-len 1)
			     (do ((i 0 (+ i 1))
				  (j (- clip-len 1) (- j 1)))
				 ((= i clip-len))
			       (let ((sn (* 0.5 (+ 1.0 (cos (* pi (/ i (- clip-len 1))))))))
				 (set! (new-data i) (+ (* sn 
							  (future i))
						       (* (- 1.0 sn) 
							  (past j))))))
			     
			     ;; todo perhaps move this mix dependent on data-lens?
			     ;; todo perhaps special case for 2 samps (what if both 1.0 for example?)
			     ;; todo perhaps if multichannel and channels are correlated and one is not clipped -- use
			     ;;   its data to help reconstruct clipped case?
			     
			     (set! (new-data 0) (if (> (future 0) 0.0)
						    (max (future 0) (past 0))
						    (min (future 0) (past 0)))))
			 
			 ;; write reconstruction
			 (vct->channel new-data clip-beg clip-len snd chn))))))))
	    
	    (if (> unclipped-max .95) (set! unclipped-max .999))
	    (scale-channel (/ unclipped-max (maxamp snd chn)) 0 (frames snd chn) snd chn)
	    (list 'max unclipped-max 'clips (/ clips 2) 'max-len max-len)))
	  
	'no-clips)))

(define* (unclip-sound snd)
  "(unclip-sound snd) applies unclip-channel to each channel of 'snd'."
  (let ((index (or snd (selected-sound) (car (sounds)))))
    (if (not (sound? index))
	(error 'no-such-sound (list "unclip-sound" snd))
	(let ((chns (channels index)))
	  (do ((chn 0 (+ 1 chn)))
	      ((= chn chns))
	    (unclip-channel index chn))))))


(define* (kalman-filter-channel (Q 1.0e-5))
  ;; translated from http://www.scipy.org/Cookbook/KalmanFiltering by Andrew Straw (but "R" here is a signal)
  (let ((size (frames))
	(mx (maxamp))
	(data (channel->vct 0))
	(xhat 0.0)
	(P 1.0) ; any non-zero value ok here
	(R 0.01) ; first guess
	(Pminus 0.0)
	(frm (make-formant :radius (- 1.0 (/ 2000.0 (srate))) :frequency 1000))
	(del (make-moving-average 256))
	(K 0.0))

     (do ((k 1 (+ k 1)))
	 ((= k size))
       (let ((datum (data k))
	     (xhatminus xhat))
	 
	 (let* ((res (formant frm datum))
		(avg (moving-average del (abs res))))
	   (set! R (/ .000001 (+ avg .001))))
	 ;; K now goes between say .5 if avg large to almost 0 if avg 0 (R is inverse essentially)
	 ;;   so filter lp effect increases as apparent true signal decreases
	 ;;   "truth" here is based on vocal resonances
	 
	 (set! (data k) xhatminus) ; filter output
	 
	 (set! Pminus (+ P Q))
	 (set! K (/ Pminus (+ Pminus R)))
	 (set! xhat (+ xhatminus
		       (* K (- datum xhatminus))))
	 (set! P (* (- 1.0 K) Pminus))))
    
     (vct-scale! data (/ mx (vct-peak data)))
     (vct->channel data)))


;;; -------- Savitzky-Golay filter coefficients (FIR filter -- returns vct of coeffs centered at vct midpoint)
;;;
;;; based on Numerical Recipes in C p 652
;;; needs mixer-solve in mixer.scm

(define* (make-savitzky-golay-filter size (order 2)) ;assuming symmetric filter (left = right)
  (if (even? size) 
      (set! size (+ size 1)))
  (let ((n (/ (- size 1) 2))
	(a (make-mixer (+ 1 order))))
    (do ((i 0 (+ i 1)))
	((> i (* order 2)))
      (let ((sum (if (= i 0) 1.0 0.0)))
	(do ((k 1 (+ k 1)))
	    ((> k n))
	  (set! sum (+ sum (expt k i) (expt (- k) i))))
	(let ((m i))
	  (if (> i (- (* 2 order) i))
	      (set! m (- (* 2 order) i)))
	  (do ((k (- m) (+ k 2)))
	      ((> k m))
	    (set! (a (/ (+ i k) 2) (/ (- i k) 2)) sum)))))
    (let ((b (mixer-solve a (let ((f (make-frame (+ order 1))))
			      (frame-set! f 0 1.0) ; set others instead for derivative
			      f)))
	  (result (make-vct size)))
      (do ((k (- n) (+ k 1))
	   (i 0 (+ i 1)))
	  ((> k n))
	(let ((sum (b 0))
	      (fac 1.0))
	  (do ((m 1 (+ 1 m))) 
	      ((> m order)) 
	    (set! fac (* fac k))
	    (set! sum (+ sum (* (b m) fac))))
	  (set! (result i) sum)))
      (make-fir-filter :order size :xcoeffs result))))

(define savitzky-golay-filter fir-filter)

#|
;; NRinC examples (2nd ed, p651)
:(make-savitzky-golay-filter 5 2)
#<fir-filter: order: 5, xs: [-0.086 0.343 0.486 0.343 -0.086]>
:(make-savitzky-golay-filter 11 2)
#<fir-filter: order: 11, xs: [-0.084 0.021 0.103 0.161 0.196 0.207 0.196 0.161...(0: -0.084, 5: 0.207)]>
:(make-savitzky-golay-filter 11 4)
#<fir-filter: order: 11, xs: [0.042 -0.105 -0.023 0.140 0.280 0.333 0.280 0.140...(1: -0.105, 5: 0.333)]>
:(make-savitzky-golay-filter 25 2)
#<fir-filter: order: 25, xs: [-0.049 -0.027 -0.006 0.012 0.028 0.043 0.055 0.066...(0: -0.049, 12: 0.090)]>
|#


;;; -------- hard and soft clipping
;;;
;;; from Julius Smith's http://ccrma.stanford.edu/~jos/pasp/Cubic_Soft_Clipper.html

(define (hard-clipped x)
  (max -1.0 (min 1.0 x)))

(define (soft-clipped x)
  (max -0.6667 (min 0.6667 (- x (* 0.3333 x x x)))))



;;; -------- parallel FM spectrum calculator

;(fm-parallel-component 200 2000.0 (list 2000.0 200.0) (list 0.5 1.0) () () #t)

(define (fm-parallel-component freq-we-want wc wms inds ns bs using-sine)
  "(fm-parallel-component freq carrier modfreqs indices () () with-sines) returns the amplitude of \"freq\" in \
the multi-modulator FM case described by the list of modulator frequencies and indices"
  (if (not (null? wms))
      (let* ((sum 0.0)
	     (index (car inds))
	     (mx (ceiling (* 7 index)))
	     (wm (car wms)))
	(do ((k (- mx) (+ k 1)))
	    ((>= k mx) sum)
	  (set! sum (+ sum (fm-parallel-component freq-we-want (+ wc (* k wm)) (cdr wms) (cdr inds) 
					      (append ns (list k)) (append bs (list index)) 
					      using-sine)))))
      (if (< (abs (- freq-we-want (abs wc))) .1)
	  (let ((bmult 1.0))
	    (for-each
	     (lambda (n index)
	       (set! bmult (* bmult (bes-jn n index))))
	     ns bs)
	    (if (and using-sine (< wc 0.0)) (set! bmult (- bmult)))
	    ;(format #t ";add ~A from ~A ~A" bmult ns bs)
	    bmult)
	  0.0)))


;;; this returns the component in FM with complex index (using-sine ignored for now)

(define (fm-complex-component freq-we-want wc wm a b interp using-sine)
  (let ((sum 0.0)
	(mxa (ceiling (* 7 a)))
	(mxb (ceiling (* 7 b))))
    (do ((k (- mxa) (+ k 1)))
	((>= k mxa))
      (do ((j (- mxb) (+ j 1)))
	  ((>= j mxb))
	(if (< (abs (- freq-we-want (+ wc (* k wm) (* j wm)))) 0.1)
	    (let ((curJI (* (bes-jn k a)
			    (bes-in (abs j) b)
			    (expt 0.0+1.0i j))))
	      (set! sum (+ sum curJI))
	      (if (> (magnitude curJI) 0.001)
		  (format #t ";fm-complex-component add ~A from J~D(~A) = ~A and I~D(~A) = ~A"
			       curJI 
			       k a (bes-jn k a)
			       j b (bes-in (abs j) b)))))))
    (list sum
	  (+ (* (- 1.0 interp) (real-part sum))
	     (* interp (imag-part sum))))))

;(fm-complex-component 1200 1000 100 1.0 3.0 0.0 #f)


(define (fm-cascade-component freq-we-want wc wm1 a wm2 b)
  (let ((sum 0.0)
	(mxa (ceiling (* 7 a)))
	(mxb (ceiling (* 7 b))))
    (do ((k (- mxa) (+ k 1)))
	((>= k mxa))
      (do ((j (- mxb) (+ j 1)))
	  ((>= j mxb))
	(if (< (abs (- freq-we-want (+ wc (* k wm1) (* j wm2)))) 0.1)
	    (let ((curJJ (* (bes-jn k a)
			    (bes-jn j (* k b)))))
	      (set! sum (+ sum curJJ))
	      (if (> (magnitude curJJ) 0.001)
		  (format #t ";fm-cascade-component add ~A from J~D(~A) = ~A and I~D(~A) = ~A"
			       curJJ 
			       k a (bes-jn k a)
			       j b (bes-jn j (* k b))))))))
    sum))

;(fm-cascade-component 2000 2000 500 1.5 50 1.0)



;;; waveshaping harmonic amplitude at a given index

(define (cheby-hka k a coeffs) ; (coeff 0 = DC)
  (let ((sum 0.0)
	(n (length coeffs)))
    (do ((j 0 (+ j 1)))
	((= j n))
      (let ((dsum 0.0)
	    (p (+ k (* 2 j))))
	(do ((i 0 (+ i 1)))
	    ((>= (+ p (* 2 i)) n))
	  (set! dsum (+ dsum (* (expt -1 i)
				(coeffs (+ p (* 2 i)))
				(+ (binomial (+ p i) i)
				   (binomial (+ p i -1) (- i 1)))))))
	(set! sum (+ sum (* dsum 
			    (expt a p)
			    (binomial p j))))))
    sum))

#|
(with-sound ()
  (let ((gen (make-polyshape 1000.0 :partials (list 1 .5  2 .25  3 .125  4 .125))))
    (do ((i 0 (+ i 1)))
	((= i 88200))
      (outa i (* .5 (polyshape gen 0.25))))))

(cheby-hka 1 0.25 (vct 0 .5 .25 .125 .125))
|#


;;; find not-so-spikey amps for waveshaping

(define* (flatten-partials any-partials (tries 32))

  (define (cos-fft-to-max n cur-amps)
    (let* ((size 1024)
	   (fft-rl (make-vct size))
	   (fft-im (make-vct size)))
      (do ((i 0 (+ i 1))
	   (bin 2 (+ bin 2)))
	  ((= i n))
	(set! (fft-rl bin) (cur-amps i)))
      (vct-peak (mus-fft fft-rl fft-im size -1))))

  (let* ((partials (if (list? any-partials)
		       (list->vct any-partials)
		       any-partials))
	 (len (length partials))
	 (topk 0)
	 (DC 0.0)
	 (original-sum (let ((sum 0.0))
			 (do ((i 0 (+ i 2)))
			     ((>= i len) sum)
			   (let ((hnum (partials i))
				 (amp (partials (+ i 1))))
			     (if (= hnum 0)
				 (set! DC amp)
				 (begin
				   (set! topk (max topk hnum))
				   (set! sum (+ sum amp))))))))
	 (min-sum original-sum)
	 (original-partials (let ((v (make-vct topk)))
			      (do ((i 0 (+ i 2)))
				  ((>= i len) v)
				(let ((hnum (partials i)))
				  (if (not (= hnum 0))
				      (set! (v (- hnum 1)) (partials (+ i 1))))))))
	 (min-partials (vct-copy original-partials)))

    (if (<= topk (log tries 2))
	(set! tries (floor (expt 2 (- topk 1)))))

    (do ((try 0 (+ 1 try)))
	((= try tries))
      (let ((new-partials (vct-copy original-partials)))
	(do ((k 0 (+ k 1)))
	    ((= k topk))
	  (if (> (random 1.0) 0.5)
	      (set! (new-partials k) (- (new-partials k)))))
	(let ((new-sum (cos-fft-to-max topk new-partials)))
	  (if (< new-sum min-sum)
	      (begin
		(set! min-partials (vct-copy new-partials))
		(set! min-sum new-sum))))))

    (let ((new-amps (vct-scale! min-partials (/ original-sum min-sum)))
	  (new-partials (vct-copy partials)))
      (do ((i 0 (+ i 2)))
	  ((>= i len))
	(let ((hnum (new-partials i)))
	  (if (= hnum 0)
	      (set! (new-partials (+ i 1)) DC)
	      (set! (new-partials (+ i 1)) (new-amps (- hnum 1))))))
      new-partials)))

      
#|
(with-sound (:clipped #f :statistics #t :channels 2)
  (let* ((amps (normalize-partials (list 1 .25 2 .5 3 .25)))
	 (gen1 (make-polywave 400.0 amps))
	 (gen2 (make-polywave 400.0 (flatten-partials amps))))
    (do ((i 0 (+ i 1)))
	((= i 44100))
      (outa i (polywave gen1))
      (outb i (polywave gen2)))))
|#



#|
;;; these are standard FFTs for s7

(define* (fft! rl im n (dir 1))

  (if (not im)
      (let ((clear (copy rl)))
	(fill! clear 0.0)
	(set! im clear)))

  (if (not n)
      (set! n (length rl)))

  (do ((i 0 (+ i 1))
       (j 0))
      ((= i n))
    (if (> j i)
	(let ((tempr (rl j))
	      (tempi (im j)))
	  (set! (rl j) (rl i))
	  (set! (im j) (im i))
	  (set! (rl i) tempr)
	  (set! (im i) tempi)))
    (let ((m (/ n 2)))
      (do () 
	  ((or (< m 2) (< j m)))
	(set! j (- j m))
	(set! m (/ m 2)))
      (set! j (+ j m))))
  
  (let ((ipow (floor (log n 2)))
	(prev 1))
    (do ((lg 0 (+ lg 1))
	 (mmax 2 (* mmax 2))
	 (pow (/ n 2) (/ pow 2))
	 (theta (* pi dir) (* theta 0.5)))
	((= lg ipow))
      (let ((wpr (cos theta))
	    (wpi (sin theta))
	    (wr 1.0)
	    (wi 0.0))
	(do ((ii 0 (+ ii 1)))
	    ((= ii prev))
	  (do ((jj 0 (+ jj 1))
	       (i ii (+ i mmax))
	       (j (+ ii prev) (+ j mmax)))
	      ((>= jj pow))
	    (let ((tempr (- (* wr (rl j)) (* wi (im j))))
		  (tempi (+ (* wr (im j)) (* wi (rl j)))))
	      (set! (rl j) (- (rl i) tempr))
	      (set! (im j) (- (im i) tempi))
	      (set! (rl i) (+ (rl i) tempr))
	      (set! (im i) (+ (im i) tempi))))
	  (let ((wtemp wr))
	    (set! wr (- (* wr wpr) (* wi wpi)))
	    (set! wi (+ (* wi wpr) (* wtemp wpi)))))
	(set! prev mmax))))
  rl)


(define* (cfft! data n (dir 1))

  (if (not n) (set! n (length data)))

  (do ((i 0 (+ i 1))
       (j 0))
      ((= i n))
    (if (> j i)
	(let ((temp (data j)))
	  (set! (data j) (data i))
	  (set! (data i) temp)))
    (let ((m (/ n 2)))
      (do () 
	  ((or (< m 2) (< j m)))
	(set! j (- j m))
	(set! m (/ m 2)))
      (set! j (+ j m))))
  
  (let ((ipow (floor (log n 2)))
	(prev 1))
    (do ((lg 0 (+ lg 1))
	 (mmax 2 (* mmax 2))
	 (pow (/ n 2) (/ pow 2))
	 (theta (make-rectangular 0.0 (* pi dir)) (* theta 0.5)))
	((= lg ipow))
      (let ((wpc (exp theta))
	    (wc 1.0))
	(do ((ii 0 (+ ii 1)))
	    ((= ii prev))
	  (do ((jj 0 (+ jj 1))
	       (i ii (+ i mmax))
	       (j (+ ii prev) (+ j mmax)))
	      ((>= jj pow))
	    (let ((tc (* wc (data j))))
	      (set! (data j) (- (data i) tc))
	      (set! (data i) (+ (data i) tc))))
	  (set! wc (* wc wpc)))
	(set! prev mmax))))

  data)


;;; > (cfft! (list 0.0 1+i 0.0 0.0))
;;; (1+1i -1+1i -1-1i 1-1i)
;;; 
;;; > (cfft! (vector 0.0 1+i 0.0 0.0))
;;; #(1+1i -1+1i -1-1i 1-1i)
;;; 
;;; ;; check against built-in FFT
;;; > (let ((rl (vct 0.0 1.0 0.0 0.0)) 
;;;         (im (vct 0.0 1.0 0.0 0.0))) 
;;;     (mus-fft rl im) 
;;;     (map make-rectangular (vct->list rl) (vct->list im)))
;;; (1+1i -1+1i -1-1i 1-1i)

|#
