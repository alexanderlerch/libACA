(provide 'snd-generators.scm)
(if (provided? 'snd)
    (if (not (provided? 'snd-ws.scm)) (load "ws.scm"))
    (if (not (provided? 'sndlib-ws.scm)) (load "sndlib-ws.scm")))

;;; it is dangerous to use a method within a generator's definition of that method
;;;   if the gen is used as the environment in with-environment, the embedded call
;;;   becomes a recursive call on that method.  You either need to check the type
;;;   of the method argument, or use #_method to override the name lookup, or use
;;;   the explicit call style: (((gen 'embedded-gen) 'shared-method) ...)


(define nearly-zero 1.0e-12) ; 1.0e-14 in clm.c, but that is trouble here (noddcos)

;;; --------------------------------------------------------------------------------

;;; nssb (see nxycos) -- wouldn't a more consistent name be nxycos? but it already exists -- perhaps delete nssb?

(defgenerator (nssb 
	       :make-wrapper (lambda (g)
			       (set! (g 'frequency) (hz->radians (g 'frequency)))
			       g))
  (frequency *clm-default-frequency*) (ratio 1.0) (n 1) (angle 0.0) fm)


(define* (nssb gen (fm 0.0))
;;  "(make-nssb frequency (ratio 1.0) (n 1)) creates an nssb generator, similar to nxysin.\n\
;;   (nssb gen (fm 0.0)) returns n sinusoids from frequency spaced by frequency * ratio."
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let* ((cx angle)
	   (mx (* cx ratio))
	   (den (sin (* 0.5 mx))))
      (set! angle (+ angle fm frequency))
      (if (< (abs den) nearly-zero)
	  -1.0
	  (/ (- (* (sin cx) 
		   (sin (* mx (/ (+ n 1) 2)))
		   (sin (/ (* n mx) 2)))
		(* (cos cx) 
		   0.5 (+ den (sin (* mx (+ n 0.5))))))
	     (* (+ n 1) den))))))
  
#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-nssb 1000.0 0.1 3)))
     (do ((i 0 (+ i 1)))
	 ((= i 10000))
       (outa i (nssb gen)))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-nssb 1000.0 0.1 3))
	(vib (make-oscil 5.0))
	(ampf (make-env '(0 0 1 1 2 1 3 0) :length 20000 :scaler 1.0)))
     (do ((i 0 (+ i 1)))
	 ((= i 20000))
       (outa i (* (env ampf) 
		  (nssb gen (* (hz->radians 100.0) 
			       (oscil vib))))))))
|#



;;; --------------------------------------------------------------------------------

;;; G&R 1st col rows 1&2

(define (find-nxysin-max n ratio)
  
  (define (ns x n) 
    (let* ((a2 (/ x 2))
	   (den (sin a2)))
      (if (= den 0.0)
	  0.0
	  (/ (* (sin (* n a2)) (sin (* (+ 1 n) a2))) den))))
  
  (define (nodds x n) 
    (let ((den (sin x))
	  (num (sin (* n x))))
      (if (= den 0.0)
	  0.0
	  (/ (* num num) den))))
  
  (define (find-mid-max n lo hi)
    (let ((mid (/ (+ lo hi) 2)))
      (let ((ylo (ns lo n))
	    (yhi (ns hi n)))
	(if (< (abs (- ylo yhi)) nearly-zero) ; was e-100 but that hangs if not using doubles
	    (ns mid n)
	    (if (> ylo yhi)
		(find-mid-max n lo mid)
		(find-mid-max n mid hi))))))
  
  (define (find-nodds-mid-max n lo hi)
    (let ((mid (/ (+ lo hi) 2)))
      (let ((ylo (nodds lo n))
	    (yhi (nodds hi n)))
	(if (< (abs (- ylo yhi)) nearly-zero)
	    (nodds mid n)
	    (if (> ylo yhi)
		(find-nodds-mid-max n lo mid)
		(find-nodds-mid-max n mid hi))))))
  
  (if (= ratio 1)
      (find-mid-max n 0.0 (/ pi (+ n .5)))
      (if (= ratio 2)
	  (find-nodds-mid-max n 0.0 (/ pi (+ (* 2 n) 0.5)))
	  n)))


(defgenerator (nxysin
	       :make-wrapper (lambda (g)
			       (set! (g 'frequency) (hz->radians (g 'frequency)))
			       (set! (g 'norm) (/ 1.0 (find-nxysin-max (g 'n) (g 'ratio))))
			       g))
  (frequency *clm-default-frequency*) (ratio 1.0) (n 1) (angle 0.0) fm
  (norm 1.0))


(define* (nxysin gen (fm 0.0))
;;  "(make-nxysin frequency (ratio 1.0) (n 1)) creates an nxysin generator.\n\
;;   (nxysin gen (fm 0.0)) returns n sines from frequency spaced by frequency * ratio."
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let* ((x angle)
	   (y (* x ratio))
	   (den (sin (* y 0.5))))
      (set! angle (+ angle fm frequency))
      (if (< (abs den) nearly-zero)
	  0.0
	  (/ (* (sin (+ x (* 0.5 (- n 1) y)))
		(sin (* 0.5 n y))
		norm)
	     den)))))

;;; if x (angle) is constant (an initial-phase offset for a sum of sines,
;;;  the peak amp is nsin-max(n) + abs(sin(initial-phase))*(1 - nsin-max(n))
;;;  that is, it varys sinusoidally from a sum-of-sines .7245 to a sum-of-cosines 1
;;; since we're treating "x" as the carrier (it's not a constant phase offset in this case)
;;;  the output varies as x does, so we have a maxamp of n? There are special cases
;;;  for low n and low integer ratio:

;;;  ratio (4):    (40):   (400):
;;;    1: 3.23      29.34       290.1
;;;    2: 2.9404    28.97       289.7
;;;    3: 3.85      38.6        346.8
;;; 1.123: n
;;;   .5: 3.55      30.0        290

;;; a ratio of 1 gives a sum of equal amplitude sines, so we could use nsin-max?
;;;            2                                odd harmonics -- use noddsin?
;;; else use n (not so great for ratio: 3, but not way off)
;;; worst case right now is probably ratio .5

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-nxysin 300 1/3 3)))
     (do ((i 0 (+ i 1)))
	 ((= i 20000))
       (outa i (nxysin gen)))))

;;; here's the varying initial-phase case:

(with-sound (:clipped #f)
   (let ((x 0.0)
	 (ix (/ pi 1000))
	 (n 100))
     (do ((i 0 (+ i 1)))
	 ((= i 1000))
       (let ((pk 0.0)
	     (phi x)
	     (y 0.0)
	     (iy (/ (* 2 pi) 10000)))
	 (set! x (+ x ix))
	 (do ((k 0 (+ k 1)))
	     ((= k 10000))
	   ;; x = phi
	   (let ((den (sin (/ y 2))))
	     (if (not (= den 0.0))
		 (let ((sum (abs (/ (* (sin (+ phi (* y (/ (- n 1) 2)))) (sin (/ (* n y) 2))) den))))
		   (if (> sum pk)
		       (set! pk sum)))))
	   (set! y (+ y iy)))
	 (outa i pk)))))
|#


(defgenerator (nxycos
	       :make-wrapper (lambda (g)
			       (set! (g 'frequency) (hz->radians (g 'frequency)))
			       g))
  (frequency *clm-default-frequency*) (ratio 1.0) (n 1) (angle 0.0) fm)


(define* (nxycos gen (fm 0.0))
;;  "(make-nxycos frequency (ratio 1.0) (n 1)) creates an nxycos generator.\n\
;;   (nxycos gen (fm 0.0)) returns n cosines from frequency spaced by frequency * ratio."
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let* ((x angle)
	   (y (* x ratio))
	   (den (sin (* y 0.5))))
      (set! angle (+ angle fm frequency))
      (if (< (abs den) nearly-zero)
	  1.0
	  (/ (* (cos (+ x (* 0.5 (- n 1) y)))
		(sin (* 0.5 n y)))
	     (* n den)))))) ; n=normalization

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-nxycos 300 1/3 3)))
     (do ((i 0 (+ i 1)))
	 ((= i 20000))
       (outa i (* .5 (nxycos gen))))))
|#



;;; --------------------------------------------------------------------------------
;;;
;;; G&R 1st col rows 3 4

(defgenerator (nxy1cos
	       :make-wrapper (lambda (g)
			       (set! (g 'frequency) (hz->radians (g 'frequency)))
			       g))
  (frequency *clm-default-frequency*) (ratio 1.0) (n 1) (angle 0.0) fm)


(define* (nxy1cos gen (fm 0.0))
;;  "(make-nxy1cos frequency (ratio 1.0) (n 1)) creates an nxy1cos generator.\n\
;;   (nxy1cos gen (fm 0.0)) returns 2n cosines from frequency spaced by frequency * ratio with every other cosine multiplied by -1."
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let* ((x angle)
	   (y (* x ratio))
	   (den (cos (* y 0.5))))
      (set! angle (+ angle fm frequency))
      (if (< (abs den) nearly-zero)
	  -1.0
	  (max -1.0
	       (min 1.0
		    (/ (* (sin (* n y))
			  (sin (+ x (* (- n 0.5) y))))
		       (* 2 n den))))))))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-nxy1cos 300 1/3 3)))
    (do ((i 0 (+ i 1)))
	 ((= i 20000))
       (outa i (nxy1cos gen)))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-nxy1cos 300 1/3 3))
	(gen1 (make-nxycos 300 1/3 6)))
     (do ((i 0 (+ i 1)))
	 ((= i 20000))
       (outa i (* 0.4 (+ (nxycos gen1 0.0) (nxy1cos gen)))))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-nxy1cos (radians->hz (* .01 pi)) 1.0 3)))
       (do ((i 0 (+ i 1)))
	   ((= i 20000))
	 (outa i (nxy1cos gen)))))
|#


(defgenerator (nxy1sin
	       :make-wrapper (lambda (g)
			       (set! (g 'frequency) (hz->radians (g 'frequency)))
			       g))
  (frequency *clm-default-frequency*) (ratio 1.0) (n 1) (angle 0.0) fm)


(define* (nxy1sin gen (fm 0.0))
;;  "(make-nxy1sin frequency (ratio 1.0) (n 1)) creates an nxy1sin generator.\n\
;;   (nxy1sin gen (fm 0.0)) returns n sines from frequency spaced by frequency * ratio with every other sine multiplied by -1."
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let* ((x angle)
	   (y (* x ratio))
	   (den (cos (* y 0.5))))
      (set! angle (+ angle fm frequency))
      (/ (* (sin (+ x (* 0.5 (- n 1) (+ y pi))))
	    (sin (* 0.5 n (+ y pi))))
	 (* n den)))))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-nxy1sin 300 1/3 3)))
     (do ((i 0 (+ i 1)))
	 ((= i 20000))
       (outa i (nxy1sin gen)))))
|#

;;;   we can get the sinusoidally varying maxamp by using e.g. (make-nxy1sin 1 1000 3)
;;;   the peak starts at ca .72 and goes to 1 etc
;;; the peak is just offset from pi (either way)



;;; --------------------------------------------------------------------------------

;;; n odd sinusoids: noddsin, noddcos, noddssb

;;; sndclm.html (G&R) 1st col 5th row (sum of odd sines)

(define (find-noddsin-max n)
  
  (define (nodds x n) 
    (let ((den (sin x))
	  (num (sin (* n x))))
      (if (= den 0.0)
	  0.0
	  (/ (* num num) den))))
  
  (define (find-mid-max n lo hi)
    (let ((mid (/ (+ lo hi) 2)))
      (let ((ylo (nodds lo n))
	    (yhi (nodds hi n)))
	(if (< (abs (- ylo yhi)) 1e-9)
	    (nodds mid n)
	    (if (> ylo yhi)
		(find-mid-max n lo mid)
		(find-mid-max n mid hi))))))
  
  (find-mid-max n 0.0 (/ pi (+ (* 2 n) 0.5))))


(defgenerator (noddsin 
	       :make-wrapper (lambda (g)
			       (if (< (g 'n) 1) (set! (g 'n) 1))
			       (set! (g 'frequency) (hz->radians (g 'frequency)))
			       (set! (g 'norm) (/ 1.0 (find-noddsin-max (g 'n)))) ; these could be precomputed
			       g))
  (frequency *clm-default-frequency*) (n 1) (angle 0.0) (norm 1.0) fm)


(define* (noddsin gen (fm 0.0))
;;  "(make-noddsin frequency (n 1)) creates an noddsin generator.\n\
;;   (noddsin gen (fm 0.0)) returns n odd-numbered sines spaced by frequency."
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let ((snx (sin (* n angle)))
	  (den (sin angle)))
      (set! angle (+ angle fm frequency))
      (if (< (abs den) nearly-zero)
	  0.0
	  (/ (* norm snx snx) den)))))

;;; max is at about: 3*pi/(8*n) -- essentially half of the nsin peak
;;; and we end up with the same max amp as nsin!!
;;; :(/ (* 8 (sin (* pi 3/8)) (sin (* pi 3/8))) (* 3 pi))
;;; 7.245186202974229185687564326622851596478E-1


#|
;;; clarinety
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-noddsin 300 :n 3))
	(ampf (make-env '(0 0 1 1 2 1 3 0) :length 40000 :scaler .5)))
      (do ((i 0 (+ i 1)))
	  ((= i 40000))
	(outa i (* (env ampf) (noddsin gen))))))
|#



(defgenerator (noddcos
	       :make-wrapper (lambda (g)
			       (set! (g 'frequency) (hz->radians (g 'frequency)))
			       g))
  (frequency *clm-default-frequency*) (n 1) (angle 0.0) fm)


(define* (noddcos gen (fm 0.0))
;;  "(make-noddcos frequency (n 1)) creates an noddcos generator.\n\
;;   (noddcos gen (fm 0.0)) returns n odd-numbered cosines spaced by frequency."
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let ((cx angle)
	  (den (* 2 n (sin angle)))) ; "n" here is normalization
      (set! angle (+ angle fm frequency))
      (if (< (abs den) nearly-zero)
	  (let ((fang (modulo (abs cx) (* 2 pi))))
	    ;; hopefully this almost never happens...
	    (if (or (< fang 0.001)
		    (< (abs (- fang (* 2 pi))) 0.001))
		1.0
		-1.0))
	  (/ (sin (* 2 n cx)) den)))))

;;; (Gradshteyn and Ryzhik 1.342)

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-noddcos 100 :n 10)))
      (do ((i 0 (+ i 1)))
	  ((= i 10000))
	(outa i (* .5 (noddcos gen))))))
|#



(defgenerator (noddssb
	       :make-wrapper (lambda (g)
			       (set! (g 'frequency) (hz->radians (g 'frequency)))
			       g))
  (frequency *clm-default-frequency*) (ratio 1.0) (n 1) (angle 0.0) fm)


(define* (noddssb gen (fm 0.0))
;;  "(make-noddssb frequency (ratio 1.0) (n 1)) creates an noddssb generator.\n\
;;   (noddssb gen (fm 0.0)) returns n sinusoids from frequency spaced by 2 * ratio * frequency."
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let* ((cx angle)
	   (mx (* cx ratio))
	   (x (- cx mx))
	   (sinnx (sin (* n mx)))
	   (den (* n (sin mx)))) ; "n" is normalization
      (set! angle (+ angle fm frequency))
      (if (< (abs den) nearly-zero)
	  (if (< (modulo (abs mx) (* 2 pi)) .1)
	      -1.0
	      1.0)
	  (- (* (sin x)
		(/ (* sinnx sinnx) den))
	     (* (cos x)
		(/ (sin (* 2 n mx))
		   (* 2 den))))))))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-noddssb 1000.0 0.1 5)))
    (do ((i 0 (+ i 1)))
	((= i 10000))
      (outa i (* .5 (noddssb gen))))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-noddssb 1000.0 0.1 5))
	(vib (make-oscil 5.0)))
    (do ((i 0 (+ i 1)))
	((= i 10000))
      (outa i (* .5 (noddssb gen (* (hz->radians 100.0) (oscil vib))))))))
|#




;;; --------------------------------------------------------------------------------
;;;
;;; various kernels: ncos2 = ncos squared (Fejer), ncos4 = ncos2 squared (Jackson), npcos = Poussin kernel

(defgenerator (ncos2
	       :make-wrapper (lambda (g)
			       (set! (g 'frequency) (hz->radians (g 'frequency)))
			       g))
  (frequency *clm-default-frequency*) (n 1) (angle 0.0) fm)


(define* (ncos2 gen (fm 0.0))
;;  "(make-ncos2 frequency (n 1)) creates an ncos2 (Fejer kernel) generator.\n\
;;   (ncos2 gen (fm 0.0)) returns n sinusoids spaced by frequency scaled by (n-k)/(n+1)"
  
  ;; from "Trigonometric Series" Zygmund p88 with changes suggested by Katznelson "Introduction to Harmonic Analysis" p12, and
  ;;   scaling by an extra factor of 1/n+1 to make sure we always peak at 1.0 (I assume callers in this context are interested 
  ;;   in the pulse-train aspect and want easily predictable peak amp).  Harmonics go as (n-i)/n+1.
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let* ((x angle)
	   (den (sin (* 0.5 x))))
      (set! angle (+ angle fm frequency))
      (if (< (abs den) nearly-zero)
	  1.0
	  (let ((val (/ (sin (* 0.5 (+ n 1) x)) 
			(* (+ n 1) den))))
	    (* val val))))))

;;; can't use two oscils here because the angles have to line up perfectly

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-ncos2 100.0 :n 10)))
    (do ((i 0 (+ i 1)))
	((= i 20000))
      (outa i (* .5 (ncos2 gen))))))
|#



(define make-ncos4 make-ncos2)

;; Katznelson p16

(define* (ncos4 gen (fm 0.0))
;;  "(make-ncos4 frequency (n 1)) creates an ncos4 (Jackson kernel) generator.\n\
;;   (ncos4 gen (fm 0.0)) returns n sinusoids spaced by frequency scaled by ((n-k)/(n+1))^2"
  
  (let ((val (ncos2 gen fm)))
    (* val val))) ; we already normalized this to 1.0

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-ncos4 100.0 :n 10)))
    (do ((i 0 (+ i 1)))
	((= i 20000))
      (outa i (* .5 (ncos4 gen))))))
|#



(defgenerator (npcos
	       :make-wrapper (lambda (g)
			       (set! (g 'frequency) (hz->radians (g 'frequency)))
			       g))
  (frequency *clm-default-frequency*) (n 1) (angle 0.0) fm)


(define* (npcos gen (fm 0.0))
;;  "(make-npcos frequency (n 1)) creates an npcos (Poussin kernel) generator.\n\
;;   (npcos gen (fm 0.0)) returns n*2+1 sinusoids spaced by frequency with amplitudes in a sort of tent shape."
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let* ((den (sin (* 0.5 angle)))
	   (result (if (< (abs den) nearly-zero)
		       1.0
		       (let* ((n1 (+ n 1))
			      (result1 
			       (let ((val (/ (sin (* 0.5 n1 angle)) 
					     (* n1 den))))
				 (* val val)))
			      (p2n2 (+ (* 2 n) 2))
			      (result2 
			       (let ((val (/ (sin (* 0.5 p2n2 angle)) 
					     (* p2n2 den))))
				 (* val val))))
			 (- (* 2 result2) result1)))))
      (set! angle (+ angle fm frequency))
      result)))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-npcos 100.0 :n 10)))
    (do ((i 0 (+ i 1)))
	((= i 20000))
      (outa i (* .5 (npcos gen))))))
|#


#|

;;; ncos5 and nsin5 are minor variants of nsin and ncos -- the last component is at half amplitude

(defgenerator (ncos5
	       :make-wrapper (lambda (g)
			       (set! (g 'frequency) (hz->radians (g 'frequency)))
			       g))
  (frequency *clm-default-frequency*) (n 1) (angle 0.0) fm)


(define* (ncos5 gen (fm 0.0))
;;  "(make-ncos5 frequency (n 1)) creates an ncos5 generator.\n\
;;   (ncos5 gen (fm 0.0)) returns n cosines spaced by frequency. All are equal amplitude except the first and last at half amp."
  
  ;; from "Chebyshev Polynomials", Mason and Handscomb, p87
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let* ((x angle)
	   (den (tan (* 0.5 x))))
      (set! angle (+ angle fm frequency))
      (if (< (abs den) nearly-zero)
	  1.0
	  (/ (- (/ (sin (* n x))
		   (* 2 den))
		0.5)
	     (- n 0.5))))))


(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-ncos5 100.0 :n 10)))
    (do ((i 0 (+ i 1)))
	((= i 20000))
      (outa i (* .5 (ncos5 gen))))))


(define (find-nsin5-max n)
  
  (define (ns x n)
    (let* ((den (tan (* 0.5 x))))
      (if (< (abs den) nearly-zero)
	  0.0
	  (/ (- 1.0 (cos (* n x)))
	     den))))
  
  (define (find-mid-max n lo hi)
    (let ((mid (/ (+ lo hi) 2)))
      (let ((ylo (ns lo n))
	    (yhi (ns hi n)))
	(if (< (abs (- ylo yhi)) 1e-9)
	    (ns mid n)
	    (if (> ylo yhi)
		(find-mid-max n lo mid)
		(find-mid-max n mid hi))))))
  
  (find-mid-max n 0.0 (/ pi (+ n .5))))


(defgenerator (nsin5
	       :make-wrapper (lambda (g)
			       (set! (g 'frequency) (hz->radians (g 'frequency)))
			       (set! (g 'n) (max 2 (g 'n)))
			       (set! (g 'norm) (find-nsin5-max (g 'n)))
			       g))
  (frequency *clm-default-frequency*) (n 2) (angle 0.0) (norm 1.0) fm)


(define* (nsin5 gen (fm 0.0))
;;  "(make-nsin5 frequency (n 1)) creates an nsin5 generator.\n\
;;   (nsin5 gen (fm 0.0)) returns n sines spaced by frequency. All are equal amplitude except last at half amp."
  
  ;; from "Chebyshev Polynomials", Mason and Handscomb, p100
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let* ((x angle)
	   (den (tan (* 0.5 x))))
      (set! angle (+ angle fm frequency))
      (if (< (abs den) nearly-zero)
	  0.0
	  (/ (- 1.0 (cos (* n x)))
	     (* den norm))))))


(define (find-nsin-max n)
  
  (define (ns x n) 
    (let* ((a2 (/ x 2))
	   (den (sin a2)))
      (if (= den 0.0)
	  0.0
	  (/ (* (sin (* n a2)) (sin (* (+ 1 n) a2))) den))))
  
  (define (find-mid-max n lo hi)
    (let ((mid (/ (+ lo hi) 2)))
      (let ((ylo (ns lo n))
	    (yhi (ns hi n)))
	(if (< (abs (- ylo yhi)) 1e-14)
	    (ns mid n) ; rationalize (/ mid pi) for location
	    (if (> ylo yhi)
		(find-mid-max n lo mid)
		(find-mid-max n mid hi))))))
  
  (find-mid-max n 0.0 (/ pi (+ n .5))))


(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-nsin5 100.0 :n 10)))
    (do ((i 0 (+ i 1)))
	((= i 20000))
      (outa i (nsin5 gen)))))

(let ((norms (list 1.0 0.0)))
  (do ((i 2 (+ i 1)))
      ((= i 40))
    (let* ((res (with-sound (:clipped #f)
 	          (let ((gen (make-nsin5 100.0 :n i)))
		    (do ((i 0 (+ i 1)))
			((= i 20000))
		      (outa i (nsin5 gen))))))
	   (snd (find-sound res)))
      (format #t ";~D: ~A" i (maxamp snd 0))
      (set! norms (cons (maxamp snd 0) norms))))
  (reverse norms))

;;; from the same book p 110 is atan(x)/x, if x=cos we get:

(with-sound (:clipped #f :statistics #t)
  (let* ((x 0.0)
	 (freq (hz->radians 100.0)))
    (do ((i 0 (+ i 1)))
	((= i 20000))
      (outa i (/ (- (/ (atan (cos x))
		       (cos x))
		    (* 0.5 1.76275))
		 -0.1187))
      (set! x (+ x freq)))))

(let ((sum 0.0))
  (do ((s 1 (+ s 2)))
      ((>= s 100))
    (set! sum (+ sum (* 4 (/ (expt (- (sqrt 2.0) 1.0) (+ (* 2 s) 1))
			     (+ (* 2 s) 1))))))
  sum) ; ~ 0.096

;;; the evens cancel, each of the odds gets through once
|#




(define generator-max-r 0.999999)
(define generator-min-r -0.999999)
(define (generator-clamp-r r)
  (min generator-max-r (max generator-min-r r)))


;;; --------------------------------------------------------------------------------
;;;
;;; n sinusoids scaled by r: nrsin, nrcos, nrssb

(defgenerator (nrsin
	       :make-wrapper (lambda (g)
			       (set! (g 'r) (generator-clamp-r (g 'r)))
			       (set! (g 'gen) (make-nrxysin (g 'frequency) 1.0 (g 'n) (g 'r)))
			       g)
	       :methods (list
			 (cons 'mus-frequency
			       (make-procedure-with-setter
				(lambda (g) (mus-frequency (g 'gen)))
				(lambda (g val) (set! (mus-frequency (g 'gen)) val))))
			 (cons 'mus-scaler
			       (make-procedure-with-setter
				(lambda (g) (mus-scaler (g 'gen)))
				(lambda (g val) (set! (mus-scaler (g 'gen)) val))))))
  (frequency *clm-default-frequency*) (n 1) (r 0.5) 
  (gen #f))


(define* (nrsin gen (fm 0.0))
;;  "(make-nrsin frequency (n 1) (r 0.5)) creates an nrsin generator.\n\
;;   (nrsin gen (fm 0.0)) returns n sines spaced by frequency with amplitudes scaled by r^k."
  
  (nrxysin (gen 'gen) fm))



(defgenerator (nrcos
	       :make-wrapper (lambda (g)
			       (set! (g 'frequency) (hz->radians (g 'frequency)))
			       (set! (g 'n) (+ 1 (g 'n)))
			       (set! (g 'r) (generator-clamp-r (g 'r)))
			       (set! (g 'rr) (* (g 'r) (g 'r)))
			       (set! (g 'r1) (+ 1.0 (g 'rr)))
			       (set! (g 'e1) (expt (g 'r) (g 'n)))
			       (set! (g 'e2) (expt (g 'r) (+ (g 'n) 1)))
			       (set! (g 'norm) (- (/ (- (expt (abs (g 'r)) (g 'n)) 1) (- (abs (g 'r)) 1)) 1.0)) ; n+1??
			       (set! (g 'trouble) (or (= (g 'n) 1) (< (abs (g 'r)) nearly-zero)))
			       g)
	       :methods (list
			 (cons 'mus-order
			       (make-procedure-with-setter
				(lambda (g) (- (g 'n) 1))
				(lambda (g val) 
				  (set! (g 'n) (+ 1 val))
				  (set! (g 'e1) (expt (g 'r) (g 'n)))
				  (set! (g 'e2) (expt (g 'r) (+ (g 'n) 1)))
				  (set! (g 'norm) (- (/ (- (expt (abs (g 'r)) (g 'n)) 1) (- (abs (g 'r)) 1)) 1.0))
				  (set! (g 'trouble) (or (= (g 'n) 1) (< (abs (g 'r)) nearly-zero)))
				  val)))
			 (cons 'mus-frequency
			       (make-procedure-with-setter
				(lambda (g) (radians->hz (g 'frequency)))
				(lambda (g val) (set! (g 'frequency) (hz->radians val)))))
			 (cons 'mus-scaler
			       (make-procedure-with-setter
				(lambda (g) 
				  (g 'r))
				(lambda (g val)
				  (set! (g 'r) (min 0.999999 (max -0.999999 val)))
				  (with-environment g
				    (let ((absr (abs r)))
				      (set! rr (* r r))
				      (set! r1 (+ 1.0 rr))
				      (set! norm (- (/ (- (expt absr n) 1) (- absr 1)) 1.0))
				      (set! trouble (or (= n 1) 
							(< absr 1.0e-12)))))
				    val)))))
  (frequency *clm-default-frequency*) (n 1) (r 0.5) (angle 0.0) fm rr r1 e1 e2 norm trouble)


(define* (nrcos gen (fm 0.0))
;;  "(make-nrcos frequency (n 1) (r 0.5)) creates an nrcos generator.\n\
;;   (nrcos gen (fm 0.0)) returns n cosines spaced by frequency with amplitudes scaled by r^k."
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let ((x angle)
	  (rcos (* r (cos angle))))
      (set! angle (+ angle fm frequency))
      (if trouble
	  0.0
	  (/ (+ (- rcos (* e1 (cos (* n x))) rr)
		(* e2 (cos (* (- n 1) x))))
	     (* norm (+ r1 (* -2.0 rcos))))))))

;; it's faster to use polywave here and nrcos->polywave for the partials list (animals.scm) if n is not enormous

;;; formula changed to start at k=1 and n increased so we get 1 to n
;;; here is the preoptimization form:
#|
  (with-environment gen
    (let ((x angle))
      (set! angle (+ angle fm frequency))
      (if (or (= n 1)
	      (< (abs r) nearly-zero))
	  0.0
	  (let ((norm (- (/ (- (expt (abs r) n) 1) (- (abs r) 1)) 1.0))) ; n+1??
	    (/ (+ (- (* r (cos x)) 
		     (* (expt r n) (cos (* n x))) (* r r)) 
		  (* (expt r (+ n 1)) (cos (* (- n 1) x))))
	       (* norm (+ 1.0 (* -2.0 r (cos x)) (* r r))))))))
|#

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-nrcos 400.0 :n 5 :r 0.5)))
    (do ((i 0 (+ i 1)))
	((= i 10000))
      (outa i (* .5 (nrcos gen))))))

(with-sound (:clipped #f :statistics #t :play #t :scaled-to .1)
  (let ((gen (make-nrcos 1200.0 :n 3 :r 0.99))
	(mod (make-oscil 400.0)) ; multi-carrier fm
	(index 0.01))
    (do ((i 0 (+ i 1)))
	((= i 30000))
      (outa i (nrcos gen (* index (oscil mod)))))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-nrcos 2000.0 :n 3 :r 0.5))
	(mod (make-oscil 400.0)) ; multi-carrier fm
	(index 0.02))
    (do ((i 0 (+ i 1)))
	((= i 30000))
      (outa i (* .5 (nrcos gen (* index (oscil mod))))))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-nrcos 2000.0 :n 3 :r 0.5))
	(mod (make-oscil 400.0))
	(index (make-env '(0 0 1 .1) :length 30000))) ; or '(0 .4 1 0)
    (do ((i 0 (+ i 1)))
	((= i 30000))
      (outa i (* .5 (nrcos gen (* (env index) (oscil mod))))))))
|#

(definstrument (lutish beg dur freq amp)
  (let ((res1 (max 1 (round (/ 1000.0 (max 1.0 (min 1000.0 freq))))))
	(maxind (max .01 (min .3 (/ (- (log freq) 3.5) 8.0)))))
    (let ((gen (make-nrcos (* freq res1) :n (max 1 (- res1 2))))
	  (mod (make-oscil freq))
	  (start (seconds->samples beg))
	  (stop (seconds->samples (+ beg dur)))
	  (index (make-env (list 0 maxind 1 (* maxind .25) (max dur 2.0) 0.0) :duration dur))
	  (amplitude (make-env (list 0 0  .01 1  .2 1  .5 .5  1 .25  (max dur 2.0) 0.0) :duration dur :scaler amp)))
      (do ((i start (+ i 1)))
	  ((= i stop))
	(let ((ind (env index)))
	  (set! (gen 'r) ind)
	  (outa i (* (env amplitude)
		     (nrcos gen (* ind (oscil mod))))))))))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (lutish 0 1 440 .1))

(with-sound (:clipped #f :statistics #t :play #t)
  (do ((i 0 (+ i 1)))
      ((= i 10))
    (lutish (* i .1) 2 (* 100 (+ i 1)) .05)))
|#



;;; G&R 2nd col 1st and 2nd rows

(defgenerator (nrssb
	       :make-wrapper (lambda (g)
			       (set! (g 'frequency) (hz->radians (g 'frequency)))
			       (set! (g 'r) (generator-clamp-r (g 'r)))
			       (if (< (g 'r) 0.0)
				   (set! (g 'r) 0.0))
			       g))
  (frequency *clm-default-frequency*) (ratio 1.0) (n 1) (r 0.5) (angle 0.0) fm interp)


(define* (nrssb gen (fm 0.0))
;;  "(make-nrssb frequency (ratio 1.0) (n 1) (r 0.5)) creates an nrssb generator.\n\
;;   (nrssb gen (fm 0.0)) returns n sinusoids from frequency spaced by frequency * ratio with amplitudes scaled by r^k."
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let* ((cx angle)
	   (mx (* cx ratio))
	   (rn (- (expt r n)))
	   (rn1 (expt r (+ n 1)))
	   (nmx (* n mx))
	   (n1mx (* (- n 1) mx))
	   (norm (/ (- rn 1) (- r 1)))
	   (den (* norm (+ 1.0 (* -2.0 r (cos mx)) (* r r)))))
      (set! angle (+ angle fm frequency))
      (/ (- (* (sin cx)
	       (+ (* r (sin mx))
		  (* rn (sin nmx))
		  (* rn1 (sin n1mx))))
	    (* (cos cx)
	       (+ 1.0
		  (* -1.0 r (cos mx))
		  (* rn (cos nmx))
		  (* rn1 (cos n1mx)))))
	 den))))


(define (nrssb-interp gen fm interp)
;;  "(make-nrssb frequency (ratio 1.0) (n 1) (r 0.5)) creates an nrssb generator for use with nrssb-interp.\n\
;;   (nrssb-interp gen fm interp) returns n sinusoids from frequency spaced by frequency * ratio with amplitudes scaled by r^k.\
;;   The 'interp' argument determines whether the sidebands are above (1.0) or below (-1.0) frequency."
  
  (environment-set! gen 'fm fm)
  (environment-set! gen 'interp interp)
  (with-environment gen
    (let* ((cx angle)
	   (mx (* cx ratio))
	   (rn (- (expt r n)))
	   (rn1 (expt r (+ n 1)))
	   (nmx (* n mx))
	   (n1mx (* (- n 1) mx))
	   (norm (/ (- rn 1) (- r 1)))
	   (den (* norm (+ 1.0 (* -2.0 r (cos mx)) (* r r)))))
      (set! angle (+ angle fm frequency))
      (/ (- (* interp 
	       (sin cx)
	       (+ (* r (sin mx))
		  (* rn (sin nmx))
		  (* rn1 (sin n1mx))))
	    (* (cos cx)
	       (+ 1.0
		  (* -1.0 r (cos mx))
		  (* rn (cos nmx))
		  (* rn1 (cos n1mx)))))
	 den))))



#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-nrssb 1000 0.1 5 0.5)))
    (do ((i 0 (+ i 1)))
	((= i 10000))
      (outa i (nrssb gen)))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-nrssb 1000 0.1 5 0.5))
	(vib (make-oscil 5)))
    (do ((i 0 (+ i 1)))
	((= i 10000))
      (outa i (nrssb gen (* (hz->radians 100) (oscil vib)))))))
|#

(definstrument (oboish beg dur freq amp aenv)
  (let ((res1 (max 1 (round (/ 1400.0 (max 1.0 (min 1400.0 freq))))))
	 (mod1 (make-oscil 5.0))
	 (res2 (max 1 (round (/ 2400.0 (max 1.0 (min 2400.0 freq))))))
	 (gen3 (make-oscil freq))
	 (start (seconds->samples beg))
	 (amplitude (make-env aenv :duration dur :base 4 :scaler amp))
	 (skenv (make-env (list 0.0 0.0 1 1 2.0 (mus-random 1.0) 3.0 0.0 (max 4.0 (* dur 20.0)) 0.0) 
			  :duration dur :scaler (hz->radians (random (* freq .05)))))
	 (relamp (+ .85 (random .1)))
	 (avib (make-rand-interp 5 .2))
	 (hfreq (hz->radians freq))
	 (h3freq (hz->radians (* .003 freq)))
	 (scl (/ 0.05 amp)))
    (let ((gen (make-nrssb (* freq res1) (/ res1) :n res1 :r 0.75))
	  (gen2 (make-oscil (* freq res2)))
	  (stop (+ start (seconds->samples dur))))
      (do ((i start (+ i 1)))
	  ((= i stop))
	(let* ((vol (* (+ .8 (rand-interp avib)) 
		       (env amplitude)))
	       (vib (+ (* h3freq (oscil mod1))
		       (env skenv)))
	       (vola (* scl vol))
	       (result (* vol
			  (+ (* (- relamp vola) 
				(nrssb-interp gen (* res1 vib) -1.0))
			     (* (+ (- 1.0 relamp) vola) 
				(oscil gen2 (+ (* vib res2)
					       (* hfreq (oscil gen3 vib)))))))))
	  (outa i result)
	  (if *reverb* (outa i (* .01 result) *reverb*)))))))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (oboish 0 1 300 .1 '(0 0 1 1 2 0)))

(with-sound (:clipped #f :statistics #t :play #t)
  (do ((i 0 (+ i 1)))
      ((= i 10))
    (oboish (* i .3) .4 (+ 100 (* 50 i)) .05 '(0 0 1 1 2 1 3 0))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((rats (vector 1 256/243 9/8 32/27 81/64 4/3 1024/729 3/2 128/81 27/16 16/9 243/128 2))
	(mode (vector 0 0 2 4 11 11 5 6 7 9 2 12 0)))
    (do ((i 0 (+ i 1)))
	((= i 20))
      (oboish (/ (random 32) 8) 
	      (/ (+ 3 (random 8)) 8)
	      (* 16.351 16 (rats (mode (random 12))))
	      (+ .25 (random .25))
	      (let* ((pt1 (random 1.0))
		     (pt2 (+ pt1 (random 1.0)))
		     (pt3 (+ pt2 (random 1.0))))
		(list 0 0 pt1 1 pt2 .5 pt3 0))))))

;;; .85 .15 (* 2 freq) 300, 2400 + 0.5*vib
|#




;;; --------------------------------------------------------------------------------
;;;
;;; n sinusoids scaled by k: nkssb


;;; G&R 1st col ksinkx cases

(defgenerator (nkssb
	       :make-wrapper (lambda (g)
			       (set! (g 'frequency) (hz->radians (g 'frequency)))
			       (set! (g 'n) (+ 1 (g 'n))) ; sum goes 1 to n-1
			       g)
	       :methods (list
			 (cons 'mus-order
			       (make-procedure-with-setter
				(lambda (g) (- (g 'n) 1))
				(lambda (g val) (set! (g 'n) (+ 1 val)) val)))))
  (frequency *clm-default-frequency*) (ratio 1.0) (n 1) (angle 0.0) fm interp)


(define* (nkssb gen (fm 0.0))
;;  "(make-nkssb frequency (ratio 1.0) (n 1)) creates an nkssb generator.\n\
;;   (nkssb gen (fm 0.0)) returns n sinusoids from frequency spaced by frequency * ratio with amplitude k."
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let* ((x (* angle ratio))
	   (cxx (- angle x))
	   (sx2 (sin (* 0.5 x)))
	   (sx22 (* 2 sx2))
	   (sxsx (* 4 sx2 sx2))
	   (nx (* n x))
	   (nx2 (* 0.5 (- (* 2 n) 1) x)))
      (set! angle (+ angle fm frequency))
      (if (< (abs sx2) 1.0e-8)
	  -1.0
	  (let ((s1 (- (/ (sin nx) sxsx)
		       (/ (* n (cos nx2)) sx22)))
		(c1 (- (/ (* n (sin nx2)) sx22)
		       (/ (- 1.0 (cos nx)) sxsx))))
	    (/ (- (* (sin cxx) s1)
		  (* (cos cxx) c1))
	       (* 0.5 n (- n 1)))))))) ; normalization, nominal n is off by 1


(define (nkssb-interp gen fm interp)
;;  "  (make-nkssb-interp frequency (ratio 1.0) (n 1)) creates an nkssb generator for nkssb-interp.\n\
;;   (nkssb-interp gen fm interp) returns n sinusoids from frequency spaced by frequency * ratio with amplitude k.\
;;   The 'interp' argument determines whether the sidebands are above (1.0) or below (-1.0) frequency."
  
  (environment-set! gen 'fm fm)
  (environment-set! gen 'interp interp)
  (with-environment gen
    (let ((x (* angle ratio)))
      (let ((cxx (- angle x))
	    (sx2 (sin (* 0.5 x))))
	(let ((sx22 (* 2 sx2))
	      (sxsx (* 4 sx2 sx2))
	      (nx (* n x))
	      (nx2 (* 0.5 (- (* 2 n) 1) x)))
	  (set! angle (+ angle fm frequency))
	  (if (< (abs sx2) 1.0e-8)
	      1.0
	      (let ((s1 (- (/ (sin nx) sxsx)
			   (/ (* n (cos nx2)) sx22)))
		    (c1 (- (/ (* n (sin nx2)) sx22)
			   (/ (- 1.0 (cos nx)) sxsx))))
		(/ (- (* (cos cxx) c1)
		      (* interp (sin cxx) s1))
		   (* 0.5 n (- n 1)))))))))) ; normalization, nominal n is off by 1, peak seems to be solid right through the interpolation

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-nkssb 1000.0 0.1 5)))
    (do ((i 0 (+ i 1)))
	((= i 10000))
      (outa i (nkssb gen)))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-nkssb 1000.0 0.1 5))
	(vib (make-oscil 5.0))
	(vibamp (hz->radians 50.0)))
    (do ((i 0 (+ i 1)))
	((= i 30000))
      (outa i (nkssb gen (* vibamp (oscil vib)))))))
|#

(definstrument (nkssber beg dur freq mfreq n vibfreq amp)
  (let ((start (seconds->samples beg))
	 (stop (seconds->samples (+ beg dur)))
	 (gen (make-nkssb freq (/ mfreq freq) n))
	 (move (make-env '(0 1 1 -1) :duration dur))
	 (vib (make-oscil vibfreq))
	 (vibamp (hz->radians (* (/ freq mfreq) 5.0)))
	 (ampf (make-env '(0 0 1 1 5 1 6 0) :scaler amp :duration dur)))
    (do ((i start (+ i 1)))
	((= i stop))
      (outa i (* (env ampf)
		 (nkssb-interp gen 
			       (* vibamp (oscil vib))
			       (env move))) ; interp env
	    ))))

#|
(with-sound (:play #t)
  (nkssber 0 1 1000 100 5 5 0.5)
  (nkssber 1 2 600 100 4 1 0.5)
  (nkssber 3 2 1000 540 3 3 0.5)
  (nkssber 5 4 300 120 2 0.25 0.5)
  (nkssber 9 1 30 4 40 0.5 0.5)
  (nkssber 10 1 20 6 80 0.5 0.5))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-nkssb 1000.0 0.1 5))
	(move (make-env '(0 1 1 -1) :length 30000))
	(vib (make-oscil 5.0))
	(vibamp (hz->radians 50.0)))
    (do ((i 0 (+ i 1)))
	((= i 30000))
      (outa i (* 0.5 (nkssb-interp gen 
				   (* vibamp (oscil vib))
				   (env move))) ; interp env
	    ))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-nkssb 600.0 1/6 4))
	(vib (make-oscil 1.0))
	(vibamp (hz->radians 30.0)))
    (do ((i 0 (+ i 1)))
	((= i 100000)) 
      (let ((intrp (oscil vib)))
	(outa i (* 0.5 (nkssb-interp gen 
				     (* vibamp intrp)
				     intrp)))))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-nkssb 1000.0 (/ 540 1000) 3))
	(vib (make-oscil 3.0)) ; 0.3  or 125 + 0.25 and 2 -> circling sound
	(vibamp (hz->radians (* (/ 1000 540) 5.0))))
    (do ((i 0 (+ i 1)))
	((= i 100000)) 
      (let ((intrp (oscil vib)))
	(outa i (* 0.5 (nkssb-interp gen 
				     (* vibamp intrp)
				     intrp)))))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-nkssb 300.0 (/ 120 300) 2))
	(vib (make-oscil 0.25))
	(vibamp (hz->radians (* (/ 300 120) 5.0))))
    (do ((i 0 (+ i 1)))
	((= i 300000)) 
      (let ((intrp (oscil vib)))
	(outa i (* 0.5 (nkssb-interp gen 
				     (* vibamp intrp)
				     intrp)))))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-nkssb 30.0 (/ 4 30) 40))
	(vib (make-oscil 0.5))
	(vibamp (hz->radians (* (/ 30 4) 5.0))))
    (do ((i 0 (+ i 1)))
	((= i 300000)) 
      (let ((intrp (oscil vib)))
	(outa i (* 0.5 (nkssb-interp gen 
				     (* vibamp intrp)
				     intrp)))))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-nkssb 20.0 (/ 6 20) 80)) ; 120 8 80 (100), 6 400
	
	(vib (make-oscil 0.5))
	(vibamp (hz->radians (* (/ 20 6) 5.0))))
    (do ((i 0 (+ i 1)))
	((= i 300000))
      (let ((intrp (oscil vib)))
	(outa i (* 0.5 (nkssb-interp gen 
				     (* vibamp intrp)
				     intrp)))))))
|#


;;; --------------------------------------------------------------------------------

;;; n cos scaled by sin(k*pi/(n+1))/sin(pi/(n+1))
;;; "Biased Trigonometric Polynomials", Montgomery and Vorhauer
;;; American Math Monthly vol 114 no 9 Nov 2007

(defgenerator (nsincos
	       :make-wrapper (lambda (g)
			       (let ((n (g 'n)))
				 (set! (g 'frequency) (hz->radians (g 'frequency)))
				 (set! (g 'n2) (/ (+ n 1) 2))
				 (set! (g 'cosn) (cos (/ pi (+ n 1))))
				 (do ((k 1 (+ k 1)))
				     ((> k n))
				   (set! (g 'norm) (+ (g 'norm) 
						      (/ (sin (/ (* k pi) (+ n 1))) 
							 (sin (/ pi (+ n 1)))))))
				 g)))
  (frequency *clm-default-frequency*) (n 1) 
  (angle 0.0) (n2 1.0) (cosn 1.0) (norm 0.0) fm)


(define* (nsincos gen (fm 0.0))
;;  "(make-nsincos frequency (n 1)) creates an nsincos generator.\n\
;;   (nsincos gen (fm 0.0)) returns n cosines spaced by frequency with amplitude sin(k*pi/(n+1))/sin(pi/(n+1))"
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let* ((x angle)
	   (num (cos (* n2 x))))
      (set! angle (+ angle fm frequency))
      (/ (* num num)
	 (* norm (- (cos x) cosn))))))

#|
(with-sound (:clipped #f :statistics #t :play #f)
  (let ((gen (make-nsincos 100.0 3)))
    (do ((i 0 (+ i 1)))
	((= i 20000))
      (outa i (nsincos gen)))))
|#




;;; --------------------------------------------------------------------------------
;;;
;;; Ramanujan, "On certain Arithmetical Functions"

(defgenerator (n1cos 
	       :make-wrapper (lambda (g)
			       (set! (g 'frequency) (hz->radians (g 'frequency)))
			       g))
  (frequency *clm-default-frequency*) (n 1) (angle 0.0) fm)

(define* (n1cos gen (fm 0.0))
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let* ((x angle)
	   (tn (tan (* 0.5 x))))
      (set! angle (+ angle fm frequency))
      (if (< (abs tn) 1.0e-6)
	  1.0
	  (/ (- 1.0 (cos (* n x)))
	     (* tn tn
		n n 2)))))) ; normalization -- this still has the very large DC term

#|
(with-sound (:clipped #f)
  (let ((gen (make-n1cos 100.0 10)))
    (do ((i 0 (+ i 1)))
	((= i 44100))
      (outa i (n1cos gen)))))
|#



#|
;;; --------------------------------------------------------------------------------

;;; not sure the next two are interesting -- 2 more kernels

;;; Dimitrov and Merlo

(defgenerator (npos1cos
	       :make-wrapper (lambda (g)
			       (set! (g 'frequency) (hz->radians (g 'frequency)))
			       g))
  (frequency *clm-default-frequency*) (n 1) (angle 0.0) fm)


(define* (npos1cos gen (fm 0.0))
;;  "(make-npos1cos frequency (n 1)) creates an npos1cos generator.\n\
;;   (npos1cos gen (fm 0.0)) returns n cosines spaced by frequency."
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let* ((x angle)
	   (num (- (* (+ n 2) (sin (/ (* n x) 2)))
		   (* n (sin (/ (* (+ n 2) x) 2)))))
	   (sx (sin (/ x 2)))
	   (den (* 4 n (+ n 1) (+ n 2) sx sx sx sx)))
      (set! angle (+ angle fm frequency))
      (if (< (abs den) nearly-zero)
	  0.0
	  (/ (* 3 num num)
	     den)))))

;;; needs normalization and no DC.   side amps seem close


(with-sound (:clipped #f :statistics #t :play #f)
  (let ((gen (make-npos1cos 100.0 3)))
    (do ((i 0 (+ i 1)))
	((= i 20000))
      (outa i (npos1cos gen)))))


(defgenerator (npos3cos
	       :make-wrapper (lambda (g)
			       (set! (g 'frequency) (hz->radians (g 'frequency)))
			       g))
  (frequency *clm-default-frequency*) (n 1) (angle 0.0) fm)


(define* (npos3cos gen (fm 0.0))
;;  "(make-npos3cos frequency (n 1)) creates an npos3cos generator.\n\
;;   (npos3cos gen (fm 0.0)) returns n cosines spaced by frequency."
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let* ((x angle)
	   (sx (sin (/ x 2)))
	   (den (* (+ (* 4 n) 2) sx sx)))
      (set! angle (+ angle fm frequency))
      (if (< (abs den) nearly-zero)
	  (* 1.0 n)
	  (/ (- 2 (cos (* n x)) (cos (* (+ n 1) x)))
	     den)))))

;;; needs normalization and no DC, peak at den=0 not right.   side amps seem close

(with-sound (:clipped #f :statistics #t :play #f)
  (let ((gen (make-npos3cos 100.0 3)))
    (do ((i 0 (+ i 1)))
	((= i 20000))
      (outa i (npos3cos gen)))))
|#




;;; --------------------------------------------------------------------------------
;;;
;;; inf sinusoids scaled by r: rcos, rssb

(defgenerator (rcos
	       :make-wrapper (lambda (g)
			       (set! (g 'osc) (make-oscil (g 'frequency) (* 0.5 pi)))
			       (set! (g 'r) (generator-clamp-r (g 'r)))
			       (set! (g 'rr) (* (g 'r) (g 'r)))
			       (let ((absr (abs (g 'r))))
				 (set! (g 'norm) (/ (- 1.0 absr) (* 2.0 absr)))
				 (set! (g 'trouble) (< absr nearly-zero)))
			       g)
	       :methods (list
			 (cons 'mus-frequency
			       (make-procedure-with-setter
				(lambda (g) (mus-frequency (g 'osc)))
				(lambda (g val) (set! (mus-frequency (g 'osc)) val))))

			 (cons 'mus-scaler
			       (make-procedure-with-setter
				(lambda (g) (g 'r))
				(lambda (g val) 
				  (set! (g 'r) (generator-clamp-r val))
				  (set! (g 'rr) (* (g 'r) (g 'r)))
				  (let ((absr (abs (g 'r))))
				    (set! (g 'norm) (/ (- 1.0 absr) (* 2.0 absr)))
				    (set! (g 'trouble) (< absr nearly-zero))))))
				  
			 (cons 'mus-phase
			       (make-procedure-with-setter
				(lambda (g) (mus-phase (g 'osc)))
				(lambda (g val) (set! (mus-phase (g 'osc)) val))))))
  (frequency *clm-default-frequency*) (r 0.5) fm
  (osc #f) rr norm trouble)

(define* (rcos gen (fm 0.0))
;;  "(make-rcos frequency (r 0.5)) creates an rcos generator.\n\
;;   (rcos gen (fm 0.0)) returns many cosines spaced by frequency with amplitude r^k."
  
  ;; from Andrews, Askey, Roy "Special Functions" 5.1.16, p243. r^k cos sum
  ;; a variant of the G&R 2nd col 4th row
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (if trouble
	0.0                       ; 1.0 from the formula, but we're subtracting out DC
	(* (- (/ (- 1.0 rr)
		 (- (+ 1.0 rr)
		    (* 2.0 r (oscil osc fm))))
	      1.0)
	     norm))))


#|
  (with-environment gen
    (let ((absr (abs r))
	  (rr (* r r)))
      (if (< absr nearly-zero)
	  0.0                       ; 1.0 from the formula, but we're subtracting out DC
	  (* (- (/ (- 1.0 rr)
		   (- (+ 1.0 rr)
		      (* 2.0 r (oscil osc fm))))
		1.0)
	     (/ (- 1.0 absr) (* 2.0 absr))))))) ; normalization
|#

#|
;;; G&R form:
(define* (rcos gen (fm 0.0))
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let* ((absr (abs r))
	   (rcosx (* r (oscil osc fm))))
      (* (- (/ (- 1.0 rcosx)
	       (+ 1.0 
		  (* r r)
		  (* -2.0 rcosx)))
	    1.0)
	 (/ (- 1.0 absr) absr))))) ; normalization
|#

;;; if r>0 we get the spike at multiples of 2pi, since the k*pi case is flipping -1 1 -1 etc
;;; if r<0, we get the spike at multiples of (2k-1)pi since the r sign now counteracts the cos k*pi sign
;;;  so the peak amp is the same in the two cases, so the normalization has to use abs(r)!
;;;  but in the k*pi case we tend to miss k*pi (whereas we never miss 0 since we start there),
;;;  so the actual maxamp may be less than 1.0




#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-rcos 100.0 :r 0.5)))
    (do ((i 0 (+ i 1)))
	((= i 20000))
      (outa i (rcos gen)))))
|#

;; this uses rkoddssb below

(definstrument (stringy beg dur freq amp)
  (let ((n (floor (/ (mus-srate) (* 3 freq)))))
    (let ((start (seconds->samples beg))
	  (stop (seconds->samples (+ beg dur)))
	  (r (expt .001 (/ n))))
      (let ((carrier (make-rcos freq (* .5 r)))
	    (clang (make-rkoddssb (* freq 2) (/ 1.618 2) r))
	    (ampf (make-env '(0 0 1 1 2 .5 4 .25 10 0) :scaler amp :duration dur))
	    (clangf (make-env (list 0 0 .1 1 .2 .1 .3 0) :scaler (* amp .5) :duration .1))
	    (rf (make-env (list 0 1 1 0) :scaler (* 0.5 r) :duration dur))
	    (crf (make-env (list 0 1 1 0) :scaler r :duration .1)))
	(do ((i start (+ i 1)))
	    ((= i stop))
	  (set! (mus-scaler clang) (env crf))
	  (set! (carrier 'r) (env rf))
	  (outa i (+ (* (env clangf)
			(rkoddssb clang 0.0))
		     (* (env ampf)
			(rcos carrier 0.0)))))))))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (stringy 0 1 1000 .5))

(with-sound (:clipped #f :statistics #t :play #t)
  (do ((i 0 (+ i 1)))
      ((= i 10))
    (stringy (* i .3) .3 (+ 200 (* 100 i)) .5)))
|#



(defgenerator (rssb 
	       :make-wrapper (lambda (g)
			       (set! (g 'frequency) (hz->radians (g 'frequency)))
			       (set! (g 'r) (generator-clamp-r (g 'r)))
			       g)
	       
	       :methods (list
			 (cons 'mus-scaler
			       (make-procedure-with-setter
				(lambda (g) (g 'r))
				(lambda (g val) (set! (g 'r) (generator-clamp-r val)))))))
  (frequency *clm-default-frequency*) (ratio 1.0) (r 0.5) (angle 0.0) fm interp)


(define* (rssb gen (fm 0.0))
;;  "(make-rssb frequency (ratio 1.0) (r 0.5)) creates an rssb generator.\n\
;;   (rssb gen (fm 0.0)) returns many cosines from frequency spaced by frequency * ratio with amplitude r^k."
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let* ((angle1 angle)
	   (angle2 (* angle1 ratio))
	   (carsin (sin angle1))
	   (canrcos (cos angle1))
	   (den (+ 1.0 (* r r) (* -2.0 r (cos angle2))))
	   (sumsin (* r (sin angle2)))
	   (sumcos (- 1.0 (* r (cos angle2)))))
      (set! angle (+ angle1 fm frequency))
      (/ (- (* carsin sumsin)
	    (* canrcos sumcos))
	 (* 2 den)))))


(define (rssb-interp gen fm interp)
;;  "(make-rssb frequency (ratio 1.0) (r 0.5)) creates an rssb generator for rssb-interp.\n\
;;   (rssb-interp gen fm interp) returns many cosines from frequency spaced by frequency * ratio with amplitude r^k.\
;;  The 'interp' argument determines whether the sidebands are above (1.0) or below (-1.0) frequency."
  
  (environment-set! gen 'fm fm)
  (environment-set! gen 'interp interp)
  (with-environment gen
    (let* ((angle1 angle)
	   (angle2 (* angle1 ratio))
	   (carsin (sin angle1))
	   (canrcos (cos angle1))
	   (den (+ 1.0 (* r r) (* -2.0 r (cos angle2))))
	   (sumsin (* r (sin angle2)))
	   (sumcos (- 1.0 (* r (cos angle2)))))
      (set! angle (+ angle1 fm frequency))
      (/ (- (* carsin sumsin)
	    (* interp canrcos sumcos))
	 (* 2 den)))))



(definstrument (bump beg dur freq amp f0 f1 f2)
  (let ((start (seconds->samples beg))
	(stop (seconds->samples (+ beg dur)))
	(res0 (round (/ f0 freq)))
	(res1 (round (/ f1 freq)))
	(res2 (round (/ f2 freq))))
    (let ((gen1 (make-rssb (* res0 freq) (/ res0) .4))
	  (gen2 (make-rssb (* res1 freq) (/ res1) .5))
	  (gen3 (make-rssb (* res2 freq) (/ res2) .6))
	  (ampf (make-env '(0 0 .1 1 2 .5 3 .1 4 1 5 .4 6 .1 80 0) :scaler amp :base 32 :duration dur)) ; or 50 at end
	  ;;           or '(0 0 .1 1 2 .5 3 .1 4 .3 5 .1 40 0)
	  (pervib (make-triangle-wave 5.0 (hz->radians 3.0)))
	  (ranvib (make-rand-interp 12.0 (hz->radians 2.0))))
      (do ((i start (+ i 1)))
	  ((= i stop))
	(let ((vib (+ (rand-interp ranvib)
		      (triangle-wave pervib))))
	  (outa i (* (env ampf)
		     (+ (* .85 (rssb-interp gen1 (* res0 vib) -1))
			(* .1 (rssb-interp gen2 (* res1 vib) 0))
			(* .05 (rssb-interp gen3 (* res2 vib) 1))))))))))

#|
(with-sound (:play #t)
  (do ((k 0 (+ k 1))) 
      ((= k 10))
    (bump (* 0.4 k) 1 (* 16.3 (expt 2.0 (+ 3 (/ k 12)))) .5 520 1190 2390))
  (do ((k 0 (+ k 1))) 
      ((= k 10))
    (let* ((freq (* 16.3 (expt 2.0 (+ 3 (/ k 12)))))
	   (scl (sqrt (/ freq 120))))
      (bump (+ 4 (* 0.4 k)) 1 freq  .5 (* scl 520) (* scl 1190) (* scl 2390)))))

(with-sound (:clipped #f :statistics #t :play #t) 
  (do ((k 0 (+ k 1))) 
      ((= k 10))
    (let* ((freq (* 16.3 (expt 2.0 (+ 3 (/ k 12))))) ; if oct=5 (and env end at 100), sort of hammered string effect
	   (f0 520) ; "uh"
	   (f1 1190)
	   (f2 2390)
	   ;; "ah" is good: 730 1090 2440
	   ;; it might be smoother to scale the formant freqs by (sqrt (/ freq 120)) or even (expt (/ freq 120) 0.3)
	   (res0 (round (/ f0 freq)))
	   (res1 (round (/ f1 freq)))
	   (res2 (round (/ f2 freq)))
	   (gen1 (make-rssb (* res0 freq) (/ res0) .4))
	   (gen2 (make-rssb (* res1 freq) (/ res1) .5))
	   (gen3 (make-rssb (* res2 freq) (/ res2) .6))
	   (ampf (make-env '(0 0 .1 1 2 .5 3 .1 4 1 5 .4 6 .1 80 0) :scaler .5 :base 32 :length 60000)) ; or 50 at end
	   ;;           or '(0 0 .1 1 2 .5 3 .1 4 .3 5 .1 40 0)
	   (pervib (make-triangle-wave 5.0 (hz->radians 3.0)))
	   (ranvib (make-rand-interp 12.0 (hz->radians 2.0))))
      (do ((i 0 (+ i 1)))
	  ((= i 60000))
	(let ((vib (+ (rand-interp ranvib)
		      (triangle-wave pervib))))
	  (outa (+ i (* k 30000)) (* (env ampf)
				     (+ (* .85 (rssb-interp gen1 (* res0 vib) -1))
					(* .1 (rssb-interp gen2 (* res1 vib) 0))
					(* .05 (rssb-interp gen3 (* res2 vib) 1))))))))))

(with-sound (:clipped #f :statistics #t :play #t) 
  (do ((k 0 (+ k 1))) 
      ((= k 10))
    (let* ((freq (* 16.3 (expt 2.0 (+ 3 (/ k 12))))) ; froggy if oct=1 or 2 and "ah" (env end at 10 = cycling) ("er" is good too at oct=2)
	   (scl (sqrt (/ freq 120)))
	   (f0 (* scl 520)) ; "uh"
	   (f1 (* scl 1190))
	   (f2 (* scl 2390))
	   ;; "ah" is good: 730 1090 2440
	   (res0 (floor (/ f0 freq)))
	   (res1 (floor (/ f1 freq)))
	   (res2 (floor (/ f2 freq)))
	   (gen1 (make-rk!ssb (* res0 freq) (/ res0) 2.4))
	   (gen2 (make-rssb (* res1 freq) (/ res1) .5))
	   (gen3 (make-rssb (* res2 freq) (/ res2) .6))
	   (ampf (make-env '(0 0 .1 1 2 .5 3 .1 4 .3 5 .4 6 .1 40 0) :scaler .5 :base 32 :length 60000)) ; or 50 at end
	   ;;           or '(0 0 .1 1 2 .5 3 .1 4 .3 5 .1 40 0)
	   (pervib (make-triangle-wave 5.0 (hz->radians 3.0)))
	   (ranvib (make-rand-interp 12.0 (hz->radians 2.0))))
      (do ((i 0 (+ i 1)))
	  ((= i 60000))
	(let ((vib (+ (rand-interp ranvib)
		      (triangle-wave pervib))))
	  (outa (+ i (* k 30000)) (* (env ampf)
				     (+ (* .85 (rk!ssb gen1 (* res0 vib)))
					(* .1 (rssb-interp gen2 (* res1 vib) 0))
					(* .05 (rssb-interp gen3 (* res2 vib) 1))))))))))

(with-sound (:clipped #f :statistics #t :play #t) 
  (do ((k 0 (+ k 1))) 
      ((= k 10))
    (let* ((freq (* 16.3 (expt 2.0 (+ 3 (/ k 12)))))
	   (scl (sqrt (/ freq 120)))
	   (f0 (* scl 490)) ; "uh"
	   (f1 (* scl 1350))
	   (f2 (* scl 2440))
	   ;; "ah" is good: 730 1090 2440
	   (res0 (floor (/ f0 freq)))
	   (res1 (floor (/ f1 freq)))
	   (res2 (floor (/ f2 freq)))
	   (gen1 (make-rk!ssb (* res0 freq) (/ res0) 2))
	   (gen2 (make-rk!ssb (* res1 freq) (/ res1) 3))
	   (gen3 (make-rk!ssb (* res2 freq) (/ res2) 3))
	   (ampf (make-env '(0 0 .1 1 2 .5 3 .1 4 .3 5 .4 6 .1 40 0) :scaler .5 :base 32 :length 30000))
	   (pervib (make-triangle-wave 5.0 (hz->radians 3.0)))
	   (ranvib (make-rand-interp 12.0 (hz->radians 2.0))))
      (do ((i 0 (+ i 1)))
	  ((= i 30000))
	(let ((vib (+ (rand-interp ranvib)
		      (triangle-wave pervib))))
	  (outa (+ i (* k 30000)) (* (env ampf)
				     (+ (* .85 (rk!ssb gen1 (* res0 vib)))
					(* .1 (rk!ssb gen2 (* res1 vib)))
					(* .05 (rk!ssb gen3 (* res2 vib)))))))))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-rssb 2000.0 (/ 103.0 2000) 0.5)))
    (do ((i 0 (+ i 1)))
	((= i 10000))
      (outa i (rssb gen)))))
|#


;;; --------------------------------------------------------------------------------
;;;
;;; rxysin
;;;
;;; similar to rssb: (JO 1st)

(defgenerator (rxysin
	       :make-wrapper (lambda (g)
			       (set! (g 'frequency) (hz->radians (g 'frequency)))
			       (set! (g 'r) (generator-clamp-r (g 'r)))
			       g)
	       :methods (list
			 (cons 'mus-scaler
			       (make-procedure-with-setter
				(lambda (g) (g 'r))
				(lambda (g val) (set! (g 'r) (generator-clamp-r val)))))))
  
  (frequency *clm-default-frequency*) (ratio 1.0) (r 0.5) (angle 0.0) fm)


(define* (rxysin gen (fm 0.0))
;;  "(make-rxysin frequency (ratio 1.0) (r 0.5)) creates an rxysin generator (similar to rssb).\n\
;;   (rxysin gen (fm 0.0)) returns many sines from frequency spaced by frequency * ratio with amplitude r^k."
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let* ((x angle)
	   (y (* x ratio)))
      (set! angle (+ angle fm frequency))
      (/ (- (sin x)
	    (* r (sin (- x y))))
	 (+ 1.0 
	    (* -2.0 r (cos y))
	    (* r r))))))

#|
(with-sound (:clipped #f :statistics #t :play #t :scaled-to .5)
  (let ((gen (make-rxysin 1000 0.1 0.5)))
    (do ((i 0 (+ i 1)))
	((= i 10000))
      (outa i (rxysin gen)))))
|#


(defgenerator (rxycos
	       :make-wrapper (lambda (g)
			       (set! (g 'frequency) (hz->radians (g 'frequency)))
			       (set! (g 'r) (generator-clamp-r (g 'r)))
			       (set! (g 'rr) (* (g 'r) (g 'r)))
			       (set! (g 'norm) (- 1.0 (abs (g 'r)))) ; abs for negative r
			       g)
	       :methods (list
			 (cons 'mus-scaler
			       (make-procedure-with-setter
				(lambda (g) (g 'r))
				(lambda (g val) 
				  (set! (g 'r) (generator-clamp-r val))
				  (set! (g 'rr) (* (g 'r) (g 'r)))
				  (set! (g 'norm) (- 1.0 (abs (g 'r)))))))))
  (frequency *clm-default-frequency*) (ratio 1.0) (r 0.5) (angle 0.0) fm norm rr)


(define* (rxycos gen (fm 0.0))
;;  "(make-rxycos frequency (ratio 1.0) (r 0.5)) creates an rxycos generator.\n\
;;   (rxycos gen (fm 0.0)) returns many cosines from frequency spaced by frequency * ratio with amplitude r^k."
;; I really need to figure out how to keep these dic strings out of the evaluator.
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let* ((x angle)
	   (y (* x ratio)))
      (set! angle (+ angle fm frequency))
      (* (/ (- (cos x)
	       (* r (cos (- x y))))
	    (+ 1.0 rr
	       (* -2.0 r (cos y))))
	 norm))))

#|
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-rxycos 1000 0.1 0.5)))
    (do ((i 0 (+ i 1)))
	((= i 10000))
      (outa i (rxycos gen)))))
|#


(define* (clamp-rxycos-r gen (fm 0.0))
  ;; in this case we need to track ratio, as well as r, since the
  ;;   highest frequency goes as x+ky (y=ratio*x); we want the value of k when
  ;;   we reach srate/3, then solve for the corresponding r.
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let* ((x (radians->hz (+ frequency fm))) ; undo the earlier hz->radians
	   (y (* x ratio))
	   (topk (/ 1.0 (floor (/ (- (/ (mus-srate) 3) x) y))))
	   (maxr (expt cutoff topk)))
      (if (>= r 0.0)
	  (min r maxr)
	  (max r (- maxr))))))

(defgenerator (safe-rxycos
	       :make-wrapper (lambda (g)
			       (set! (g 'frequency) (hz->radians (g 'frequency)))
			       (set! (g 'r) (clamp-rxycos-r g 0.0))
			       g)
	       
	       :methods (list
			 (cons 'mus-scaler
			       (make-procedure-with-setter
				(lambda (g) (g 'r))
				(lambda (g val)
				  (set! (g 'r) val)
				  (set! (g 'r) (clamp-rxycos-r g 0.0)))))
			 (cons 'mus-frequency
			       (make-procedure-with-setter
				(lambda (g) (radians->hz (g 'frequency)))
				(lambda (g val)
				  (set! (g 'frequency) (hz->radians val))
				  (set! (g 'r) (clamp-rxycos-r g 0.0))
				  val)))
			 (cons 'mus-offset ; ratio accessor in defgenerator
			       (make-procedure-with-setter
				(lambda (g) (g 'ratio))
				(lambda (g val)
				  (set! (g 'ratio) val)
				  (set! (g 'r) (clamp-rxycos-r g 0.0))
				  val)))))
  
  (frequency *clm-default-frequency*) (ratio 1.0) (r 0.5) (angle 0.0) (cutoff 0.001) fm)


(define* (safe-rxycos gen (fm 0.0))
;;  "(make-safe-rxycos frequency (ratio 1.0) (r 0.5)) creates a safe-rxycos generator.\n\
;;   (safe-rxycos gen (fm 0.0)) returns many cosines from frequency spaced by frequency * ratio with amplitude r^k where 'r' is restricted to a safe value."
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let (;(gen (current-environment)) ; !
	   (x angle)
	   (y (* angle ratio)))
      (set! angle (+ angle fm frequency))
      (if (not (= fm 0.0))
	  (set! r (clamp-rxycos-r (current-environment) fm)))
      (* (/ (- (cos x)
	       (* r (cos (- x y))))
	    (+ 1.0 
	       (* -2.0 r (cos y))
	       (* r r)))
	 (- 1.0 (abs r)))))) ; norm, abs for negative r

#|
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-safe-rxycos 1000 0.1 0.99)))
    (do ((i 0 (+ i 1)))
	((= i 10000))
      (outa i (safe-rxycos gen)))))
|#




;;; --------------------------------------------------------------------------------

;;; inf cosines scaled by e^-r (special case of rcos): ercos, erssb

;;; sndclm.html G&R 2nd col last row (with normalization)

(defgenerator (ercos
	       :make-wrapper (lambda (g)
			       (set! (g 'osc) (make-oscil (g 'frequency)))
			       (if (<= (g 'r) 0.0) (set! (g 'r) 0.00001))
			       (set! (g 'cosh-t) (cosh (g 'r)))
			       (let ((exp-t (exp (- (g 'r)))))
				 (set! (g 'offset) (/ (- 1.0 exp-t) (* 2.0 exp-t)))
				 (set! (g 'scaler) (* (sinh (g 'r)) (g 'offset))))
			       g)
	       :methods (list
			 (cons 'mus-frequency
			       (make-procedure-with-setter
				(lambda (g) (mus-frequency (g 'osc)))
				(lambda (g val) (set! (mus-frequency (g 'osc)) val))))
			 (cons 'mus-phase
			       (make-procedure-with-setter
				(lambda (g) (mus-phase (g 'osc)))
				(lambda (g val) (set! (mus-phase (g 'osc)) val))))))
  
  (frequency *clm-default-frequency*) (r 1.0) fm
  (osc #f) scaler offset cosh-t)


(define* (ercos gen (fm 0.0))
;;  "(make-ercos frequency (r 0.5)) creates an ercos generator (a special case of rcos).\n\
;;   (ercos gen (fm 0.0)) returns many cosines from frequency with amplitude e^(-kr)."
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (- (/ scaler 
	  (- cosh-t (oscil osc fm)))
       offset)))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-ercos 100 :r 1.0)))
    (do ((i 0 (+ i 1)))
	((= i 10000))
      (outa i (ercos gen)))))
|#

(definstrument (ercoser beg dur freq amp r)
  (let ((start (seconds->samples beg))
	 (stop (seconds->samples (+ beg dur)))
	 (gen (make-ercos freq :r r))
	 (t-env (make-env '(0 .1 1 2) :duration dur)))
    (do ((i start (+ i 1)))
	((= i stop))
      (let ((r (env t-env)))
	(set! (gen 'r) r)
	(set! (gen 'cosh-t) (cosh r))
	(let ((exp-t (exp (- r))))
	  (set! (gen 'offset) (/ (- 1.0 exp-t) (* 2.0 exp-t)))
	  (set! (gen 'scaler) (* (sinh r) (gen 'offset))))
      (outa i (* amp (ercos gen)))))))

#|
;; change "t" during note -- smoothly changing sum-of-cosines spectra (damped "lute-stop" effect)
(with-sound (:play #t)
  (ercoser 0 1 100 .5 0.1))
|#


(defgenerator (erssb
	       :make-wrapper (lambda (g)
			       (set! (g 'frequency) (hz->radians (g 'frequency)))
			       g))
  (frequency *clm-default-frequency*) (ratio 1.0) (r 0.5) (angle 0.0) fm)


(define* (erssb gen (fm 0.0))
;;  "(make-erssb frequency (ratio 1.0) (r 0.5)) creates an erssb generator (a special case of rssb).\n\
;;   (erssb gen (fm 0.0)) returns many sinusoids from frequency spaced by frequency * ratio with amplitude e^(-kr)."
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let* ((cx angle)
	   (mx (* cx ratio))
	   (cxx (- cx mx))
	   (ccmx (- (cosh r) (cos mx))))
      (set! angle (+ angle fm frequency))
      (if (< (abs ccmx) nearly-zero)
	  1.0
	  (/ (- (* (cos cxx)
		   (- (/ (sinh r) ccmx)
		      1.0))
		(* (sin cxx)
		   (/ (sin mx) ccmx)))
	     (* 2.0 (- (/ 1.0 (- 1.0 (exp (- r)))) 1.0))))))) ; normalization

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-erssb 1000.0 0.1 1.0)))
    (do ((i 0 (+ i 1)))
	((= i 20000))
      (outa i (erssb gen)))))
|#




#|
;;; --------------------------------------------------------------------------------
;;; removed 8-May-08 -- not useful or different from (for example) rk!cos

;;; inf sinusoids scaled by r^2: r2cos, r2sin, r2ssb

;;; Jolley 2nd col 2nd row (1st row is cos tweak of this)

(defgenerator (r2sin
	       :make-wrapper (lambda (g)
			       (set! (g 'frequency) (hz->radians (g 'frequency)))
			       (if (>= (* (g 'r) (g 'r)) 1.0)
				   (set! (g 'r) 0.9999999))
			       g))
  (frequency *clm-default-frequency*) (r 0.5) (angle 0.0) fm)


(define* (r2sin gen (fm 0.0))
;;  "(make-r2sin frequency (r 0.5)) creates an r2sin generator.\n\
;;   (r2sin gen (fm 0.0)) returns many even-numbered sines from frequency with amplitude r^(2k)/(2k)!."
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let* ((x angle))
      (set! angle (+ angle fm frequency))
      (* (sinh (* r (cos x)))
	 (sin (* r (sin x)))))))


;;; even harmonics, but we can't push the upper partials past the (2k)! range, so not very flexible

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-r2sin 100.0 :r 0.5)))
    (do ((i 0 (+ i 1)))
	((= i 20000))
      (outa i (r2sin gen)))))



(defgenerator (r2cos
	       :make-wrapper (lambda (g)
			       (set! (g 'frequency) (hz->radians (g 'frequency)))
			       (if (>= (* (g 'r) (g 'r)) 1.0)
				   (set! (g 'r) 0.9999999))
			       g))
  (frequency *clm-default-frequency*) (r 0.5) (angle 0.0) fm)


(define* (r2cos gen (fm 0.0))
;;  "(make-r2cos frequency (r 0.5)) creates an r2cos generator.\n\
;;   (r2cos gen (fm 0.0)) returns many even-numbered cosines from frequency with amplitude r^(2k)/(2k)!."
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let* ((x angle))
      (set! angle (+ angle fm frequency))
      (/ (- (* (cosh (* r (cos x)))
	       (cos (* r (sin x))))
	    1.0)                   ; omit DC
	 (- (cosh r) 1.0)))))      ; normalize

;;; odd harmonics, but we can't push the upper partials past the (2k)! range, so not very flexible

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-r2cos 100.0 :r 0.5)))
    (do ((i 0 (+ i 1)))
	((= i 20000))
      (outa i (r2cos gen)))))



(defgenerator (r2ssb
	       :make-wrapper (lambda (g)
			       (set! (g 'frequency) (hz->radians (g 'frequency)))
			       g))
  (frequency *clm-default-frequency*) (ratio 1.0) (r 0.5) (angle 0.0) fm)


(define* (r2ssb gen (fm 0.0))
;;  "(make-r2ssb frequency (ratio 1.0) (r 0.5)) creates an r2ssb generator.\n\
;;   (r2ssb gen (fm 0.0)) returns many even-numbered sinusoids from frequency spaced by frequency * ratio, if that makes any sense, with amplitude r^(2k)/(2k)!."
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let* ((cx angle)
	   (mx (* cx ratio))
	   (a r)
	   (asinx (* a (sin mx)))
	   (acosx (* a (cos mx))))
      (set! angle (+ angle fm frequency))
      (/ (- (* (cos cx)
	       (cosh acosx)
	       (cos asinx))
	    (* (sin cx)
	       (sinh acosx)
	       (sin asinx)))
	 (cosh a))))) ; normalization


(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-r2ssb 1000.0 0.1 0.5)))
    (do ((i 0 (+ i 1)))
	((= i 20000))
      (outa i (r2ssb gen)))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-r2ssb 1000.0 0.1 0.5))
	(vib (make-oscil 5)))
    (do ((i 0 (+ i 1)))
	((= i 20000))
      (outa i (r2ssb gen (* (hz->radians 100.0) (oscil vib)))))))
|#




;;; --------------------------------------------------------------------------------
;;;
;;; inf odd cosines scaled by e^-r: eoddcos

;;; Jolley 1st col 2nd row
;;;   heads toward a square wave as "r" -> 0.0 (odd harmonics, 1/k amp)

;;; this is the cos side of rkoddssb with r=e^-a

(defgenerator (eoddcos 
	       :make-wrapper (lambda (g)
			       (set! (g 'osc) (make-oscil (g 'frequency) (* 0.5 pi)))
			       g)
	       :methods (list
			 (cons 'mus-frequency 
			       (make-procedure-with-setter
				(lambda (g) (mus-frequency (g 'osc)))
				(lambda (g val) (set! (mus-frequency (g 'osc)) val))))
			 (cons 'mus-phase 
			       (make-procedure-with-setter
				(lambda (g) (mus-phase (g 'osc)))
				(lambda (g val) (set! (mus-phase (g 'osc)) val))))))
  (frequency *clm-default-frequency*) (r 1.0) fm
  (osc #f))


(define* (eoddcos gen (fm 0.0))
;;  "(make-eoddcos frequency (r 0.5)) creates an eoddcos generator.\n\
;;   (eoddcos gen (fm 0.0)) returns many cosines from spaced by frequency with amplitude e^(-r)."
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let* ((a r)
	   (sinha (sinh a)))
      (if (zero? sinha)
	  0.0 ; just a guess
	  (/ (atan (oscil osc fm) sinha)
	     (atan 1.0 sinha)))))) ; normalization

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-eoddcos 400.0 :r 1.0)))
    (do ((i 0 (+ i 1)))
	((= i 10000))
      (outa i (eoddcos gen)))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-eoddcos 400.0 :r 0.0))
	(a-env (make-env '(0 0 1 1) :length 10000)))
    (do ((i 0 (+ i 1)))
	((= i 10000))
      (set! (gen 'r) (env a-env))
      (outa i (eoddcos gen)))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen1 (make-eoddcos 400.0 :r 0.0))
	(gen2 (make-oscil 400.0))
	(a-env (make-env '(0 0 1 1) :length 10000)))
    (do ((i 0 (+ i 1)))
	((= i 10000))
      (set! (gen 'r1) (env a-env))
      (outa i (* .5 (eoddcos gen1 (* .1 (oscil gen2))))))))
|#


#|
;;; --------------------------------------------------------------------------------
;;; removed 6-May-08

;;; inf odd cosines scaled by complicated mess: koddcos

;;; Jolley 1st col 5th row 

(define make-koddcos make-oscil)

(define* (koddcos gen (fm 0.0))
;;  "(make-koddcos frequency) creates a koddcos generator.\n\
;;   (koddcos gen (fm 0.0)) returns many cosines from spaced by frequency with amplitude too messy to write down, and the output looks wrong anyway."
  
  (let ((arg (* 2.0 (oscil gen fm))))
    (if (>= arg 0.0)
	(/ (acos (- 1.0 arg)) pi)
	(/ (acos (+ 1.0 arg)) (- pi)))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-koddcos 400.0)))
    (do ((i 0 (+ i 1)))
	((= i 10000))
      (outa i (* .3 (koddcos gen))))))

;;; as printed in J, this is not usable -- 1-2sin can be 3 so acos will be complex -- looks like we're missing: x < pi
;;; we get odd harmonics but wrong amps
|#



;;; --------------------------------------------------------------------------------

;;; inf cosines scaled by r^k/k: rkcos, rksin, rkssb

;;; G&R 2nd col 6th row, also J 536
;;; r^k/k -- this sums to ln(1/(1-x)) if x<1 (J 118)

(defgenerator (rkcos 
	       :make-wrapper (lambda (g)
			       (set! (g 'osc) (make-oscil (g 'frequency) (* 0.5 pi)))
			       (set! (g 'r) (generator-clamp-r (g 'r))) ; or clip at 0.0?
			       g)
	       :methods (list
			 (cons 'mus-frequency
			       (make-procedure-with-setter
				(lambda (g) (mus-frequency (g 'osc)))
				(lambda (g val) (set! (mus-frequency (g 'osc)) val))))
			 (cons 'mus-scaler
			       (make-procedure-with-setter
				(lambda (g) (g 'r))
				(lambda (g val) (set! (g 'r) (generator-clamp-r val)))))
			 (cons 'mus-phase
			       (make-procedure-with-setter
				(lambda (g) (mus-phase (g 'osc)))
				(lambda (g val) (set! (mus-phase (g 'osc)) val))))))
  (frequency *clm-default-frequency*) (r 0.5) fm
  (osc #f))

;;; not very flexible, and very similar to others in the r^k mold


(define* (rkcos gen (fm 0.0))
;;  "(make-rkcos frequency (r 0.5)) creates an rkcos generator.\n\
;;   (rkcos gen (fm 0.0)) returns many cosines from spaced by frequency with amplitude (r^k)/k."
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let ((cs (oscil osc fm)))
      (/ (* 0.5 (log (+ 1.0 (* -2.0 r cs) (* r r))))
	 (log (- 1.0 (abs r))))))) ; norm

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-rkcos 440.0 :r 0.5)))
    (do ((i 0 (+ i 1)))
	((= i 10000))
      (outa i (rkcos gen)))))
|#


(defgenerator (rksin
	       :make-wrapper (lambda (g)
			       (set! (g 'frequency) (hz->radians (g 'frequency)))
			       g)
	       :methods (list
			 (cons 'mus-scaler
			       (make-procedure-with-setter
				(lambda (g) (g 'r))
				(lambda (g val) (set! (g 'r) (generator-clamp-r val)))))))
  (frequency *clm-default-frequency*) (r 0.5) (angle 0.0) fm)

;;; normalization based on 0 of derivative of atan arg (for max) at cos x = r,
;;;   so we get a maxamp here of (atan (/ (* r (sin (acos r))) (- 1.0 (* r r))))

(define* (rksin gen (fm 0.0))
;;  "(make-rksin frequency (r 0.5)) creates an rksin generator.\n\
;;   (rksin gen (fm 0.0)) returns many sines from spaced by frequency with amplitude (r^k)/k."
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let ((x angle))
      (set! angle (+ angle fm frequency))
      (/ (atan (* r (sin x))
	       (- 1.0 (* r (cos x))))
	 (atan (* r (sin (acos r)))  ; normalization
	       (- 1.0 (* r r)))))))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-rksin 100.0 :r 0.5)))
    (do ((i 0 (+ i 1)))
	((= i 10000))
      (outa i (rksin gen)))))
|#



(defgenerator (rkssb
	       :make-wrapper (lambda (g)
			       (set! (g 'frequency) (hz->radians (g 'frequency)))
			       g)
	       :methods (list
			 (cons 'mus-scaler
			       (make-procedure-with-setter
				(lambda (g) (g 'r))
				(lambda (g val) (set! (g 'r) (generator-clamp-r val)))))))
  (frequency *clm-default-frequency*) (ratio 1.0) (r 0.5) (angle 0.0) fm)


(define* (rkssb gen (fm 0.0))
;;  "(make-rkssb frequency (ratio 1.0) (r 0.5)) creates an rkssb generator.\n\
;;   (rkssb gen (fm 0.0)) returns many sinusoids from frequency from spaced by frequency * ratio with amplitude (r^k)/k."
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let* ((cx angle)
	   (mx (* cx ratio))
	   (cxx (* (- 1.0 ratio) cx))
	   (rcosmx (* r (cos mx))))
      (set! angle (+ angle fm frequency))
      (/ (- (* (cos cxx)
	       -0.5 (log (+ 1.0 (* -2.0 rcosmx) (* r r))))
	    (* (sin cxx)
	       (atan (* r (sin mx))
		     (- 1.0 rcosmx))))
	 (- (log (- 1.0 (abs r)))))))) ; normalization

#|
(with-sound (:clipped #f :statistics #t :play #t :scaled-to .5)
  (let ((gen (make-rkssb 1000.0 0.5 :r 0.75)) ; (make-rkssb 300.0 3.0 :r 0.5)
	(ampf (make-env '(0 0 1 1 2 1 3 0) :length 20000)))
    (do ((i 0 (+ i 1)))
	((= i 20000))
      (outa i (* (env ampf) 
		 (rkssb gen))))))
|#



;;; --------------------------------------------------------------------------------

;;; inf cosines scaled by r^k/k!: rk!cos, rk!ssb

;;; G&R 2nd col 3rd from last (simplified)

(defgenerator (rk!cos
	       :make-wrapper (lambda (g)
			       (set! (g 'frequency) (hz->radians (g 'frequency)))
			       (set! (g 'norm) (/ 1.0 (- (exp (abs r)) 1.0)))
			       g)
	       :methods (list
			 (cons 'mus-phase 
			       (make-procedure-with-setter
				(lambda (g) (g 'angle))
				(lambda (g val) (set! (g 'angle) val))))))
  (frequency *clm-default-frequency*) (r 0.5) (angle 0.0) fm norm)


(define* (rk!cos gen (fm 0.0))
;;  "(make-rk!cos frequency (r 0.5)) creates an rk!cos generator.\n\
;;   (rk!cos gen (fm 0.0)) returns many cosines spaced by frequency with amplitude (r^k)/k!."
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let ((x angle))
      (set! angle (+ angle fm frequency))
      (* (- (* (exp (* r (cos x)))
	       (cos (* r (sin x))))
	    1.0) ; omit DC
	 norm))))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-rk!cos 440.0 :r 0.5)))
    (do ((i 0 (+ i 1)))
	((= i 10000))
      (outa i (* .5 (rk!cos gen))))))
|#

;;; the k! denominator dominates, so r * ratio = formant center approximately; (n!)^(1/n) 
;;;   so freq=100, r=30, the center of the spectrum is around 3kHz:

#|
(with-sound (:clipped #f :statistics #t :play #t :scaled-to .5)
  (let ((gen (make-rk!cos 100.0 :r 40.0)) 
	(r 40.0) 
	(incr (/ -40.0 100000)))
    (do ((i 0 (+ i 1)))
	((= i 100000)) 
      (set! (gen 'r) r) 
      (set! r (+ r incr))
      (outa i (rk!cos gen)))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-rk!cos 300.0 :r 10.0)) 
	(ampf (make-env '(0 0 .1 1 .2 1 3 .5 5 .25 10 0) :scaler .5 :length 10000))
	(r 10.0) 
	(incr (/ -10.0 10000)))
    (do ((i 0 (+ i 1)))
	((= i 10000)) 
      (set! (gen 'r) r) 
      (set! r (+ r incr))
      (outa i (* (env ampf) (rk!cos gen))))))

(with-sound (:clipped #f :statistics #t :play #t :scaled-to .5)
  (let ((gen (make-rk!cos 1000.0 :r 8.0)) 
	(frqf (make-env '(0 1 1 0) :base 32 :scaler (hz->radians 1000) :length 10000)))
    (do ((i 0 (+ i 1)))
	((= i 10000)) 
      (outa i (rk!cos gen (env frqf))))))

(with-sound (:clipped #f :statistics #t :play #t :scaled-to .5)
  (let ((gen (make-rk!cos 3000.0 :r 1.0)) (ampf (make-env '(0 0 1 1 10 1 11 0) :length 10000))
	(frqf (make-env '(0 1 1 0 2 .25 3 0) :base 3 :scaler (hz->radians 2000) :length 10000)))
    (do ((i 0 (+ i 1)))
	((= i 10000)) 
      (outa i (* (env ampf) (rk!cos gen (env frqf)))))))

(with-sound (:play #t :scaled-to .5)
  (do ((k 0 (+ k 1)))
      ((= k 6))
    (let ((gen (make-rk!cos 3000.0 :r 0.6)) (ampf (make-env '(0 0 1 1 2 1 3 0) :length 3000))
	  (frqf (make-env '(0 0 1 1) :base .1 :scaler (hz->radians 2000) :length 3000))) ; '(0 .5  1 1 2 0 3 0) '(0 1 1 0 2 1 6 -1)
      (do ((i 0 (+ i 1)))
	  ((= i 3000)) 
	(outa (+ i (* k 4000)) 
	      (* (env ampf) 
		 (rk!cos gen (env frqf))))))))

(with-sound (:clipped #f :statistics #t :play #t :scaled-to .5)
  (do ((k 0 (+ k 1)))
      ((= k 6))
    (let ((gen (make-rk!cos 1000.0 :r 1.0)) (ampf (make-env '(0 0 1 1 2 1 3 0) :length 3000))
	  (frqf (make-env '(0 .9 1 1 2 -1) :base .1 :scaler (hz->radians 500) :length 3000)))
      (do ((i 0 (+ i 1)))
	  ((= i 3000)) 
	(outa (+ i (* k 10000)) (* (env ampf) (rk!cos gen (env frqf))))))))

(with-sound (:clipped #f :statistics #t :play #t :scaled-to .5)
  (do ((k 0 (+ k 1)))
      ((= k 6))
    (let ((gen (make-rk!cos 500.0 :r 1.5)) (ampf (make-env '(0 0 1 1 2 1 3 0) :length 3000))
	  (frqf (make-env '(0 1 1 1 2 -1) :base .5 :scaler (hz->radians 400) :length 3000)))
      (do ((i 0 (+ i 1)))
	  ((= i 3000)) 
	(outa (+ i (* k 10000)) (* (env ampf) (rk!cos gen (env frqf))))))))
|#


(defgenerator (rk!ssb
	       :make-wrapper (lambda (g)
			       (set! (g 'frequency) (hz->radians (g 'frequency)))
			       g))
  (frequency *clm-default-frequency*) (ratio 1.0) (r 1.0) (angle 0.0) fm)


(define* (rk!ssb gen (fm 0.0))
;;  "(make-rk!ssb frequency (ratio 1.0) (r 0.5)) creates an rk!ssb generator.\n\
;;   (rk!ssb gen (fm 0.0)) returns many sinusoids from frequency spaced by frequency * ratio with amplitude (r^k)/k!."
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let* ((cx angle)
	   (mx (* cx ratio))
	   (ercosmx (exp (* r (cos mx))))
	   (rsinmx (* r (sin mx))))
      (set! angle (+ angle fm frequency))
      (/ (- (* (cos cx) ercosmx (cos rsinmx))
	    (* (sin cx) ercosmx (sin rsinmx)))
	 (exp (abs r)))))) ; normalization (keeping DC term here to get "carrier")

#|
(with-sound (:clipped #f :statistics #t :play #t :scaled-to .5)
  (let ((gen (make-rk!ssb 1000.0 0.1 :r 0.5)) ; (make-rk!ssb 200.0 3.0 :r 2)
	(ampf (make-env '(0 0 1 1 2 1 3 0) :length 20000)))
    (do ((i 0 (+ i 1)))
	((= i 20000))
      (outa i (* (env ampf) (rk!ssb gen))))))

					; (make-rk!ssb 0.0 120.0 :r 15) gives a widely separated wave-train of pulses
					;   so (make-rk!ssb 0.0 40.0 :r 70) is insecty (:r 100)
					;      (make-rk!ssb 0.0 10.0 :r 100) -- some bird? (make-rk!ssb 0.0 15.0 :r 300)
					;      (make-rk!ssb 1000.0 25.0 :r 10) (make-rk!ssb 3000.0 25.0 :r 100) -- another bird (5000)
|#

(definstrument (bouncy beg dur freq amp (bounce-freq 5) (bounce-amp 20))
  (let ((gen (make-rk!ssb (* freq 4) 1/4 :r 1.0)) 
	 (gen1 (make-oscil bounce-freq)) 
	 (bouncef (make-env '(0 1 1 0) :base 32 :scaler bounce-amp :duration 1.0))
	 (rf (make-env (list 0 0 1 1 (max 2.0 dur) 0) :base 32 :scaler 3 :duration dur))
	 (ampf (make-env (list 0 0 .01 1 .03 1 1 .15 (max 2 dur) 0.0) :base 32 :scaler amp :duration dur))
	 (start (seconds->samples beg))
	 (stop (seconds->samples (+ beg dur))))
    (do ((i start (+ i 1)))
	((= i stop))
      (set! (gen 'r) (+ (abs (* (env bouncef) (oscil gen1)))
			(env rf)))
      (outa i (* (env ampf)
		 (rk!ssb gen))))))

#|
(with-sound (:statistics #t :play #t :clipped #f)
  (bouncy 0 2 300 .5 5 10))

(with-sound (:statistics #t :play #t :clipped #f)
  (bouncy 0 2 200 .5 3 2))
|#
			  


#|
;;; --------------------------------------------------------------------------------
;;; rxyk!cos
;;; moved to clm.c 18-Apr-13)

(defgenerator (rxyk!sin
	       :make-wrapper (lambda (g)
			       (set! (g 'frequency) (hz->radians (g 'frequency)))
			       g))
  (frequency *clm-default-frequency*) (ratio 1.0) (r 0.5) (angle 0.0) fm)


(define* (rxyk!sin gen (fm 0.0))
;;  "(make-rxyk!sin frequency (ratio 1.0) (r 0.5)) creates an rxyk!sin generator.\n\
;;   (rxyk!sin gen (fm 0.0)) returns many sines from frequency spaced by frequency * ratio with amplitude r^k/k!."
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let* ((x angle)
	   (y (* x ratio)))
      (set! angle (+ angle fm frequency))
      (/ (* (exp (* r (cos y)))
	    (sin (+ x (* r (sin y))))) ; was cos by mistake (18-Apr-13)
	 (exp (abs r))))))


(with-sound (:clipped #f :statistics #t :play #t :scaled-to .5)
  (let ((gen (make-rxyk!sin 1000 0.1 0.5)))
    (do ((i 0 (+ i 1)))
	((= i 10000))
      (outa i (rxyk!sin gen)))))



(defgenerator (rxyk!cos
	       :make-wrapper (lambda (g)
			       (set! (g 'frequency) (hz->radians (g 'frequency)))
			       (set! (g 'ar) (/ 1.0 (exp (abs (g 'r)))))
			       g))
  (frequency *clm-default-frequency*) (ratio 1.0) (r 0.5) (angle 0.0) fm ar)


(define* (rxyk!cos gen (fm 0.0))
;;  "(make-rxyk!cos frequency (ratio 1.0) (r 0.5)) creates an rxyk!cos generator.\n\
;;   (rxyk!cos gen (fm 0.0)) returns many cosines from frequency spaced by frequency * ratio with amplitude r^k/k!."
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let* ((x angle)
	   (y (* x ratio)))
      (set! angle (+ angle fm frequency))
      (* (exp (* r (cos y)))
	 (cos (+ x (* r (sin y))))
	 ar))))


(with-sound (:clipped #f :statistics #t :play #t :scaled-to .5)
  (let ((gen (make-rxyk!cos 1000 0.1 0.5)))
    (do ((i 0 (+ i 1)))
	((= i 10000))
      (outa i (rxyk!cos gen)))))
|#


(definstrument (brassy beg dur freq amp ampf freqf gliss)
  (let ((pitch-time .05)
	(amp-time .1))
    (let ((gen (make-rxyk!cos freq :r 0.0))
	  (start (seconds->samples beg))
	  (end (seconds->samples (+ beg dur)))
	  (amp-env (make-env ampf :duration dur :scaler amp))
	  (pitch-env (make-env freqf :scaler (/ gliss freq) :duration dur))
	  (slant (make-moving-average (seconds->samples pitch-time)))
	  (vib (make-oscil 5))
	  (vib-index (hz->radians 4.0)))
      (set! (mus-increment slant) (* (hz->radians freq) (mus-increment slant)))
      (do ((i start (+ i 1)))
	  ((= i end))
	(let* ((harmfrq (env pitch-env))
	       (harmonic (floor harmfrq))
	       (dist (abs (- harmfrq harmonic))))
	  (set! (mus-scaler gen) (* 20.0
				    (if (< dist amp-time)
					dist
					(if (> dist 0.9)
					    (- 1.0 dist)
					    amp-time))))
	  (outa i (* (env amp-env)
		     (rxyk!cos gen (+ (moving-average slant harmonic)
				      (* vib-index (oscil vib)))))))))))
#|
(with-sound (:statistics #t :play #t)
  (brassy 0 4 50 .05 '(0 0 1 1 10 1 11 0) '(0 1 1 0) 1000))
|#




;;; --------------------------------------------------------------------------------

;;; inf cosines scaled by complicated mess: r2k!cos

;;; from Askey "Ramanujan and Hypergeometric Series" in Berndt and Rankin "Ramanujan: Essays and Surveys" p283
;;;
;;; this gives a sum of cosines of decreasing amp where the "k" parameter determines
;;;   the "index" (in FM nomenclature) -- higher k = more cosines

(defgenerator (r2k!cos
	       :make-wrapper (lambda (g)
			       (set! (g 'osc) (make-oscil (g 'frequency)))
			       g)
	       :methods (list
			 (cons 'mus-frequency
			       (make-procedure-with-setter
				(lambda (g) (mus-frequency (g 'osc)))
				(lambda (g val) (set! (mus-frequency (g 'osc)) val))))
			 (cons 'mus-phase
			       (make-procedure-with-setter
				(lambda (g) (mus-phase (g 'osc)))
				(lambda (g val) (set! (mus-phase (g 'osc)) val))))))
  (frequency *clm-default-frequency*) (r 0.5) (k 0.0) fm
  (osc #f))


(define* (r2k!cos gen (fm 0.0))
;;  "(make-2rk!cos frequency (r 0.5) (k 0.0)) creates an r2k!cos generator.\n\
;;   (r2k!cos gen (fm 0.0)) returns many cosines spaced by frequency with amplitude too messy to write down."
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let ((rr1 (+ 1.0 (* r r)))
	  (r2 (* 2 (abs r)))) ; abs for negative r
      (* (expt (- rr1
		  (* r2 (oscil osc fm)))
	       (- k))
	 (expt (- rr1 r2) k))))) ; amplitude normalization

;;; there is still noticable DC offset if r != 0.5 -- could precompute it and subtract (and there's lots of DC anyway)

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-r2k!cos 440.0 :r 0.5 :k 3.0)))
    (do ((i 0 (+ i 1)))
	((= i 10000))
      (outa i (r2k!cos gen)))))

(with-sound (:clipped #f :statistics #t :play #t :scaled-to .5)
  (let ((gen (make-r2k!cos 440.0 :r 0.5 :k 3.0)) 
	(indf (make-env '(0 1 1 0 10 0) :length 80000 :scaler 10.0 :offset 1)))
    (do ((i 0 (+ i 1)))
	((= i 80000)) 
      (set! (gen 'k) (env indf))
      (outa i (r2k!cos gen)))))
|#

(definstrument (pianoy beg dur freq amp)
  (let ((gen (make-r2k!cos freq :r 0.5 :k 3.0)) 
	 (ampf (make-env (list 0 0 .01 1 .03 1 1 .15 (max 2 dur) 0.0) :base 32 :scaler amp :duration dur))
	 (start (seconds->samples beg))
	 (stop (seconds->samples (+ beg dur))))
    (do ((i start (+ i 1)))
	((= i stop))
      (outa i (* (env ampf)
		 (r2k!cos gen))))))

#|
(with-sound (:statistics #t :play #t :clipped #f)
  (pianoy 0 3 100 .5))
;;; this can be combined with bouncy-like changes to get an evolving sound
|#

(definstrument (pianoy1 beg dur freq amp (bounce-freq 5) (bounce-amp 20))
  (let ((gen (make-r2k!cos freq :r 0.5 :k 3.0)) 
	 (gen1 (make-oscil bounce-freq)) 
	 (bouncef (make-env '(0 1 1 0) :base 32 :scaler bounce-amp :duration 1.0))
	 (rf (make-env (list 0 0 1 1 (max 2.0 dur) 0) :base 32 :scaler .1 :offset .25 :duration dur))
	 (ampf (make-env (list 0 0 .01 1 .03 1 1 .15 (max 2 dur) 0.0) :base 32 :scaler amp :duration dur))
	 (start (seconds->samples beg))
	 (stop (seconds->samples (+ beg dur))))
    (do ((i start (+ i 1)))
	((= i stop))
      (set! (gen 'r) (+ (abs (* (env bouncef) (oscil gen1))) 
			(env rf)))
      (outa i (* (env ampf)
		 (r2k!cos gen))))))

#|
(with-sound (:statistics #t :play #t :clipped #f)
  (pianoy1 0 4 200 .5 1 .1))
|#

(definstrument (pianoy2 beg dur freq amp)
  (let ((gen (make-r2k!cos freq :r 0.5 :k 3.0)) 
	 (ampf (make-env (list 0 0 .01 1 .03 1 1 .15 (max 2 dur) 0.0) :base 32 :scaler amp :duration dur))
	 (knock (make-fmssb 10.0 20.0 :index 1.0))
	 (kmpf (make-env '(0 0 1 1 3 1 100 0) :base 3 :scaler .05 :length 30000))
	 (indf (make-env '(0 1 1 0) :length 30000 :base 3 :scaler 10))
	 (start (seconds->samples beg))
	 (stop (seconds->samples (+ beg dur))))
    (do ((i start (+ i 1)))
	((= i stop))
      (set! (knock 'index) (env indf))
      (outa i (+ (* (env ampf)
		    (r2k!cos gen))
		 (* (env kmpf) 
		    (fmssb knock 0.0)))))))

#|
(with-sound (:clipped #f :statistics #t :play #t) 
  (pianoy2 0 1 100 .5))
|#


;;; --------------------------------------------------------------------------------

;;; inf sines scaled by 1/2^k: k2sin

;;; Jolley 1st col 1st row

;;; not flexible -- very similar to several others

(defgenerator (k2sin
	       :make-wrapper (lambda (g)
			       (set! (g 'frequency) (hz->radians (g 'frequency)))
			       g))
  (frequency *clm-default-frequency*) (angle 0.0) fm)


(define* (k2sin gen (fm 0.0))
;;  "(make-k2sin frequency) creates a k2sin generator.\n\
;;   (k2sin gen (fm 0.0)) returns many sines spaced by frequency with amplitude 1/(2^k)."
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let ((x angle))
      (set! angle (+ angle fm frequency))
      (/ (* 3.0 (sin x)) ; 3 rather than 4 for normalization
	 (- 5.0 (* 4.0 (cos x)))))))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-k2sin 440.0)))
    (do ((i 0 (+ i 1)))
	((= i 10000))
      (outa i (k2sin gen)))))
|#



;;; using the 2nd Sansone formula, we get the sum of cos case by using a=-5b/4 or 3/(4cosx-5)

(defgenerator (k2cos
	       :make-wrapper (lambda (g)
			       (set! (g 'frequency) (hz->radians (g 'frequency)))
			       g))
  (frequency *clm-default-frequency*) (angle 0.0) fm)


(define* (k2cos gen (fm 0.0))
;;  "(make-k2cos frequency) creates a k2cos generator.\n\
;;   (k2cos gen (fm 0.0)) returns many cosines spaced by frequency with amplitude 1/(2^k)."
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let ((x angle))
      (set! angle (+ angle fm frequency))
      (* 0.5 (- (/ 3.0
		   (- 5.0 (* 4.0 (cos x))))
		1.0)))))

#|
(with-sound (:clipped #f :statistics #t :play #t :scaled-to .5)
  (let ((gen (make-k2cos 440.0)))
    (do ((i 0 (+ i 1)))
	((= i 10000))
      (outa i (k2cos gen)))))
|#



(defgenerator (k2ssb
	       :make-wrapper (lambda (g)
			       (set! (g 'frequency) (hz->radians (g 'frequency)))
			       g))
  (frequency *clm-default-frequency*) (ratio 1.0) (angle 0.0) fm)


(define* (k2ssb gen (fm 0.0))
;;  "(make-k2ssb frequency (ratio 1.0)) creates a k2ssb generator.\n\
;;   (k2ssb gen (fm 0.0)) returns many sinusoids from frequency spaced by frequency * ratio with amplitude 1/(2^k)."
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let* ((cx angle)
	   (mx (* cx ratio)))
      (set! angle (+ angle fm frequency))
      (/ (- (* 3 (cos cx))
	    (* (sin cx) 4.0 (sin mx)))
	 (* 3.0 (- 5.0 (* 4.0 (cos mx))))))))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-k2ssb 1000.0 0.1)))
    (do ((i 0 (+ i 1)))
	((= i 10000))
      (outa i (* .5 (k2ssb gen))))))
|#



;;; --------------------------------------------------------------------------------


;;; this was inspired by Andrews, Askey, Roy "Special Functions" p396, but there's an error somewhere...
;;;   it produces sum r^k sin(2k-1)x
;;;   (not normalized)

(defgenerator (dblsum
	       :make-wrapper (lambda (g)
			       (set! (g 'frequency) (hz->radians (* 2 (g 'frequency))))
			       g)
	       :methods (list
			 (cons 'mus-frequency
			       (make-procedure-with-setter
				(lambda (g) (radians->hz (* 0.5 (g 'frequency))))
				(lambda (g val) (set! (g 'frequency) (hz->radians (* 2 val))) val)))))
  (frequency *clm-default-frequency*) (r 0.5) (angle 0.0) fm)


(define* (dblsum gen (fm 0.0))
;;  "(make-dblsum frequency (r 0.5)) creates a dblsum generator.\n\
;;   (dblsum gen (fm 0.0)) returns many sines from frequency spaced by frequency * (2k -1) with amplitude r^k (this is buggy)."
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let ((x angle))
      (set! angle (+ angle fm frequency))
      (/ (* (+ 1 r) (sin (* 0.5 x)))
	 (* (- 1 r) (+ 1.0 (* -2.0 r (cos x)) (* r r)))))))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-dblsum 100 0.5)))
    (do ((i 0 (+ i 1)))
	((= i 10000))
      (outa i (* .25 (dblsum gen))))))
|#




;;; --------------------------------------------------------------------------------

;;; inf odd sinusoids scaled by r^odd-k/odd-k: rkoddssb

;;;  G&R 2nd col rows 7&8 (odd r^k/k) 

(defgenerator (rkoddssb
	       :make-wrapper (lambda (g)
			       (set! (g 'frequency) (hz->radians (g 'frequency)))
			       (set! (g 'r) (generator-clamp-r (g 'r)))
			       g)
	       
	       :methods (list
			 (cons 'mus-scaler
			       (make-procedure-with-setter
				(lambda (g) (g 'r))
				(lambda (g val) (set! (g 'r) (generator-clamp-r val)))))))
  (frequency *clm-default-frequency*) (ratio 1.0) (r 0.5) (angle 0.0) fm)


(define* (rkoddssb gen (fm 0.0))
;;  "(make-rkoddssb frequency (ratio 1.0) (r 0.5)) creates an rkoddssb generator.\n\
;;   (rkoddssb gen (fm 0.0)) returns many sinusoids from frequency spaced by frequency * 2 * ratio with amplitude (r^(2k-1))/(2k-1)."
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let* ((cx angle)
	   (mx (* cx ratio))
	   (cxx (- cx mx)))
      (set! angle (+ angle fm frequency))
      (/ (- (* (cos cxx)
	       0.5
	       (log (/ (+ 1.0 (* 2.0 r (cos mx)) (* r r))
		       (+ 1.0 (* -2.0 r (cos mx)) (* r r)))))
	    (* (sin cxx)
	       (atan (* 2.0 r (sin mx))
		     (- 1.0 (* r r)))))
	 (- (log (+ 1 r))    ; normalization (r^k/k for odd k)
	    (log (- 1 r)))))))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-rkoddssb 1000.0 0.1 0.5)))
    (do ((i 0 (+ i 1)))
	((= i 10000))
      (outa i (* .5 (rkoddssb gen))))))
|#

(definstrument (glassy beg dur freq amp)
  (let* ((n (floor (/ (mus-srate) (* 3 freq))))
	 (r (expt .001 (/ n))))
    (let ((start (seconds->samples beg))
	  (stop (seconds->samples (+ beg dur)))
	  (clang (make-rkoddssb (* freq 2) (/ 1.618 2) r))
	  (clangf (make-env (list 0 0 .01 1 .1 1 .2 .4 (max .3 dur) 0) :scaler amp :duration dur))
	  (crf (make-env (list 0 1 1 0) :scaler r :duration dur)))
      (do ((i start (+ i 1)))
	  ((= i stop))
	(set! (clang 'r) (env crf))
	(outa i (* (env clangf)
		   (rkoddssb clang 0.0)))))))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (glassy 0 .1 1000 .5))

(with-sound (:clipped #f :statistics #t :play #t)
  (do ((i 0 (+ i 1)))
      ((= i 10))
    (glassy (* i .3) .1 (+ 400 (* 100 i)) .5)))

(with-sound (:statistics #t :play #t :scaled-to .5)
  (let ((gen (make-rkoddssb 5000.0 0.1 0.95))
	(ampf (make-env '(0 0 9 1 10 0) :base 32 :length 10000))
	(noi (make-rand 10000 .1)))
    (do ((i 0 (+ i 1)))
	((= i 10000))
      (outa i (* (env ampf) (sin (rkoddssb gen (rand noi))))))))
|#



;;; --------------------------------------------------------------------------------

;;; inf sinusoids scaled by kr^k: krksin

;;; Zygmund 1st
;;;   this looks interesting, but how to normalize?  sum of sines is bad enough, kr^k -> r/(1-r)^2 if x^2<1 (since n=inf)
;;;   for low n, we could use the Tn roots stuff (clm.c)
;;;   the formula must be assuming r<1.0 -- if greater than 1 it's acting like r2k! above

(defgenerator (krksin
	       :make-wrapper (lambda (g)
			       (set! (g 'frequency) (hz->radians (g 'frequency)))
			       g))
  (frequency *clm-default-frequency*) (r 0.5) (angle 0.0) fm)


(define* (krksin gen (fm 0.0))
;;  "(make-krksin frequency (r 0.5)) creates a krksin generator.\n\
;;   (krksin gen (fm 0.0)) returns many sines spaced by frequency with amplitude kr^k."
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let* ((x angle)
	   (r1 (- 1.0 r))
	   (r2 (* r r))
	   (r3 (if (> r .9) r1 1.0)) ; not right yet...
	   (den (+ 1.0 (* -2.0 r (cos x)) r2)))
      (set! angle (+ angle fm frequency))
      (/ (* r1 r1 r3 (sin x))
	 (* den den)))))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-krksin 440.0 0.5)))
    (do ((i 0 (+ i 1)))
	((= i 10000))
      (outa i (krksin gen)))))

(with-sound (:clipped #f :statistics #t :scaled-to .5 :play #t)
  (let ((gen (make-krksin 6.0 0.965))) ; 60 .6 also
    (do ((i 0 (+ i 1)))
	((= i 100000))
      (outa i (krksin gen)))))

(do ((i 0 (+ i 1)))
    ((= i 10))
  (let ((mx (maxamp (with-sound (:clipped #f :output (make-vct 10000))
		      (let ((gen (make-krksin 20.0 (* i 0.1))))
			(do ((i 0 (+ i 1)))
			    ((= i 10000))
			  (outa i (krksin gen))))))))
    (format #t ";~A: ~A" (* 0.1 i) mx)))

;;; relation between 1/(1-x)^2 and peak amp:
(with-sound (:clipped #f)
  (do ((i 0 (+ i 1))
       (r 0.0 (+ r .01)))
      ((= i 100))
    (let ((val (/ 1.0 (expt (- 1 r) 2))))
      (let ((pk 0.0))
	(let ((gen (make-krksin 1.0 r)))
	  (do ((k 0 (+ k 1)))
	      ((= k 100000))
	    (let ((x (abs (krksin gen))))
	      (if (> x pk) (set! pk x)))))
	(outa i (/ pk val))))))

;;; r 0: 1.0 (sin(x) in this case)
;;; else min den is (1-2r+r^2) so peak should be around (/ (expt (+ 1 (* - 2 r) (* r r)) 2))
;;;   but at that point sin(x)->0 as x

|#



#|
;;; --------------------------------------------------------------------------------

;;; absolute value of oscil: abssin

;;; Zygmund 2nd -- not actually very useful, but shows cos 2nx of abs

(defgenerator (abssin
	       :make-wrapper (lambda (g)
			       (set! (g 'osc) (make-oscil (g 'frequency)))
			       g)	       
	       :methods (list
			 (cons 'mus-frequency
			       (make-procedure-with-setter
				(lambda (g) (mus-frequency (g 'osc)))
				(lambda (g val) (set! (mus-frequency (g 'osc)) val))))
			 (cons 'mus-phase
			       (make-procedure-with-setter
				(lambda (g) (mus-phase (g 'osc)))
				(lambda (g val) (set! (mus-phase (g 'osc)) val))))))
  (frequency *clm-default-frequency*) fm
  (osc #f))


(define* (abssin gen (fm 0.0))
;;  "(make-abssin frequency) creates an abssin generator.\n\
;;   (abssin gen (fm 0.0)) returns (abs oscil)."
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (/ (- (abs (oscil osc fm))
	  (/ 2.0 pi))
       (/ 2.0 pi))))  ; original went from 0 to 1.0, subtract 2/pi, and we get peak at -2/pi

;; DC: sin^2 x = 1/2 - cos 2x, 
;;   so every term in the sum adds 1/(2(4k^2-1)) -> 1/4 (J 397 or 373)
;;   so DC is 2/pi = 0.6366

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-abssin 440.0)))
    (do ((i 0 (+ i 1)))
	((= i 10000))
      (outa i (abssin gen)))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((vib (make-abssin 100.0)) ; spacing will be 200, if FM you get index-proportional amount as constant offset
	(gen (make-oscil 1000.0))
	(ampf (make-env '(0 0 1 1 2 1 3 0) :scaler .5 :length 20000)))
    (do ((i 0 (+ i 1)))
	((= i 10000))
      (outa i 
	    (* (env ampf)
	       (oscil gen 0.0 (* 3 (abssin vib 0.0))))))))

;;; pitch is 2*freq, 200 1, 400 .203, 600 .087, 800 .049, 1000 .031, 1200 .021
;;;                      1      .2        .086      .048       .030       .021 -- (/ 3.0 (- (* 4 (* 6 6)) 1))
|#



;;; --------------------------------------------------------------------------------

;;; inf cosines, scaled by (-a+sqrt(a^2-b^2))^n/b^n: abcos

;;; from Sansone, p182, assumptions: a not 0, b not 0, b/a real, abs(b/a)<1 (b less than a)

(defgenerator (abcos
	       :make-wrapper (lambda (g)
			       (set! (g 'frequency) (hz->radians (g 'frequency)))
			       g))
  (frequency *clm-default-frequency*) (a 0.5) (b 0.25) (angle 0.0) fm)


(define* (abcos gen (fm 0.0))
;;  "(make-abcos frequency (a 0.5) (b 0.25)) creates an abcos generator.\n\
;;   (abcos gen (fm 0.0)) returns many cosines spaced by frequency with amplitude (-a+sqrt(a^2-b^2))^k/b^k."
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let ((x angle)
	   (norm (/ 0.5 (- (/ 1.0 
			      (- 1.0 (/ (abs (- (sqrt (- (* a a) (* b b))) 
						a)) 
					b))) 
			   1.0)))) ;; i.e. 1/(1-r) -1 because we start at k=1, r=the complicated a/b business
      (set! angle (+ angle fm frequency))
      (* norm (- (/ (sqrt (- (* a a) (* b b)))
		    (+ a (* b (cos x))))
		 1.0)))))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-abcos 100.0 0.5 0.25)))
    (do ((i 0 (+ i 1)))
	((= i 10000))
      (outa i (abcos gen)))))
|#



(defgenerator (absin
	       :make-wrapper (lambda (g)
			       (set! (g 'frequency) (hz->radians (g 'frequency)))
			       g))
  (frequency *clm-default-frequency*) (a 0.5) (b 0.25) (angle 0.0) fm)


(define* (absin gen (fm 0.0))
;;  "(make-absin frequency (a 0.5) (b 0.25)) creates an absin generator.\n\
;;   (absin gen (fm 0.0)) returns many sines spaced by frequency with amplitude (-a+sqrt(a^2-b^2))^k/b^k."
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let ((x angle))
      (set! angle (+ angle fm frequency))
      (/ (* (sin x) 
	    (sqrt (- (* a a) (* b b))))
	 (+ a (* b (cos x)))))))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-absin 100.0 0.5 0.25)))
    (do ((i 0 (+ i 1)))
	((= i 10000))
      (outa i (* .5 (absin gen))))))
|#




;;; --------------------------------------------------------------------------------

;;; inf cosines scaled by 1/(r^2+k^2): r2k2cos

;;; J 2nd col 3rd row

(defgenerator (r2k2cos
	       :make-wrapper (lambda (g)
			       (set! (g 'frequency) (hz->radians (g 'frequency)))
			       g))
  (frequency *clm-default-frequency*) (r 1.0) (angle 0.0) fm)


(define (r2k2cos-norm a)
  ;; J 124
  (- (* (/ pi (* 2 a))
	(/ (cosh (* pi a))
	   (sinh (* pi a))))
     (/ 1.0
	(* 2 a a))))

(define* (r2k2cos gen (fm 0.0))
;;  "(make-r2k2cos frequency (r 1.0)) creates an r2k2cos generator.\n\
;;   (r2k2cos gen (fm 0.0)) returns many cosines spaced by frequency with amplitude 1/(r^2+k^2)."
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let ((x angle)
	  (a r))
      (if (> x (* 2 pi))
	  (set! x (modulo x (* 2 pi))))
      (set! angle (+ x fm frequency))
      (/ (- (* pi (/ (cosh (* a (- pi x)))
		     (sinh (* a pi))))
	    (/ a))
	 (* 2 a (r2k2cos-norm a))))))


#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-r2k2cos 100.0 1.0))) ; 400 .25 -- this isn't very flexible
    (do ((i 0 (+ i 1)))
	((= i 10000))
      (outa i (* .5 (r2k2cos gen))))))
|#




;;; --------------------------------------------------------------------------------

;;; coskx/k = -ln(2sin(x/2)) or 1/2ln(1/(2-2cosx))
;;; sinkx/k = (pi-x)/2 both 0..2pi
;;; similarly -1^k : x/2 and ln(2cos(x/2)) (p44..46)
;;; 2k-1: pi/x and 1/2ln cot (x/2) 0..2pi and 0..pi
;;; but all of these are unbounded, and discontinuous

;;; --------------------------------------------------------------------------------

#|
;;;  from Stilson/Smith apparently -- was named "Discrete Summation Formula" which doesn't convey anything to me
;;;    Alexander Kritov suggests time-varying "a" is good (this is a translation of his code)

(defgenerator (blsaw
	       :make-wrapper (lambda (g)
			       (set! (g 'frequency) (hz->radians (g 'frequency)))
			       g))
  (frequency *clm-default-frequency*) (n 1) (r 0.5) (angle 0.0) fm)


(define* (blsaw gen (fm 0.0))
;;  "(make-blsaw frequency (n 1) (r 0.5)) creates a blsaw generator.\n\
;;   (blsaw gen (fm 0.0)) returns a band-limited sawtooth wave."
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let* ((a r)
	   (N n)
	   (x angle)
	   (incr frequency)
	   (den (+ 1.0 (* -2.0 a (cos x)) (* a a))))
      (set! angle (+ angle fm incr))
      (if (< (abs den) nearly-zero)
	  0.0
	  (let* ((s1 (* (expt a (- N 1.0)) (sin (+ (* (- N 1.0) x) incr))))
		 (s2 (* (expt a N) (sin (+ (* N x) incr))))
		 (s3 (* a (sin (+ x incr)))))
	    (/ (+ (sin incr) 
		  (- s3) 
		  (- s2) 
		  s1) 
	       den))))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-blsaw 440.0 :r 0.5 :n 3)))
    (do ((i 0 (+ i 1)))
	((= i 10000))
      (outa i (blsaw gen)))))
|#




;;; --------------------------------------------------------------------------------

;;; asymmetric fm gens

(defgenerator (asyfm
	       :make-wrapper (lambda (g)
			       (set! (g 'frequency) (hz->radians (g 'frequency)))
			       g))
  (frequency *clm-default-frequency*) (ratio 1.0) (r 1.0) (index 1.0) (phase 0.0) fm)


(define* (asyfm-J gen (fm 0.0))
;;  "(asyfm-J gen fm) is the same as the CLM asymmetric-fm generator (index=1.0), set r != 1.0 to get the asymmetric spectra"
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let* ((r1 (/ r))
	   (one (if (or (> r 1.0) 
			(and (< r 0.0)
			     (> r -1.0)))
		    -1.0 1.0))
	   (modphase (* ratio phase))
	   (result (* (exp (* 0.5 index (- r r1) (+ one (cos modphase))))
		      (cos (+ phase (* 0.5 index (+ r r1) (sin modphase))))))) ; use cos, not sin, to get predictable amp
      (set! phase (+ phase fm frequency))
      result)))

#|
(with-sound (:clipped #f :statistics #t :play #t) 
  (let ((gen (make-asyfm 2000.0 :ratio .1))) 
    (do ((i 0 (+ i 1)))
	((= i 10000))
      (outa i (* .5 (asyfm-J gen))))))

(with-sound (:clipped #f :statistics #t :play #t) 
  (let ((gen (make-asyfm 2000.0 :ratio .1 :index 1))
	(r-env (make-env '(0 -4 1 -1) :length 20000)))
    (do ((i 0 (+ i 1)))
	((= i 20000))
      (set! (gen 'r) (env r-env))
      (outa i (asyfm-J gen)))))

(define (val index r)
  (let ((sum 0.0))
    (do ((i -20 (+ i 1)))
	((= i 21))
      (set! sum (+ sum (* (expt r i) (bes-jn i index)))))
    (let ((norm (exp (* 0.5 index (- r (/ r))))))
      (list sum norm))))

(for-each
 (lambda (index)
   (for-each
    (lambda (r)
      (let ((peak (maxamp (with-sound (:clipped #f :output (make-vct 1000))
			    (let ((gen (make-asymmetric-fm 2000.0 :ratio .1 :r r)))
			      (do ((i 0 (+ i 1)))
				  ((= i 1000))
				(outa i (asymmetric-fm gen index))))))))
	(if (> (abs (- peak 1.0)) .1)
	    (format #t ";asymmetric-fm peak: ~A, index: ~A, r: ~A" peak index r))))
    (list -10.0 -1.5 -0.5 0.5 1.0 1.5 10.0)))
 (list 1.0 3.0 10.0))
|#

(define* (asyfm-I gen (fm 0.0))
;;  "(asyfm-I gen fm) is the I0 case of the asymmetric-fm generator"
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let* ((r1 (/ r))
	   (modphase (* ratio phase))
	   (result (* (exp (* 0.5 index (+ r r1) (- (cos modphase) 1.0)))
		      (cos (+ phase (* 0.5 index (- r r1) (sin modphase)))))))
      (set! phase (+ phase fm frequency))
      result)))

#|
(with-sound (:clipped #f :statistics #t :play #t) 
  (let ((gen (make-asyfm 2000.0 :ratio .1))) 
    (do ((i 0 (+ i 1)))
	((= i 10000))
      (outa i (* .5 (asyfm-I gen))))))
|#




;;; --------------------------------------------------------------------------------

;;; bess (returns bes-jn, like oscil returns sin) normalized to peak at 1.0
;;;   frequency here is the frequency in Hz of the damped sinusoid part of the bessel function

(define bessel-peaks (vector 1.000 0.582 0.487 0.435 0.400 0.375 0.355 0.338 0.325 0.313 0.303 0.294 0.286 0.279 0.273 0.267 0.262 0.257 0.252 0.248))

(defgenerator (bess
	       :make-wrapper (lambda (g)
			       (set! (g 'frequency) (hz->radians (g 'frequency)))
			       (if (>= (g 'n) (length bessel-peaks)) 
				   (set! (g 'norm) (/ 0.67 (expt (g 'n) 1/3)))
				   ;; this formula comes from V P Krainov, "Selected Mathetical Methods in Theoretical Physics"
				   (set! (g 'norm) (bessel-peaks (g 'n))))
			       g))
  (frequency *clm-default-frequency*) (n 0) (angle 0.0) (norm 1.0) fm)

(define* (bess gen (fm 0.0))
;;  "(make-bess frequency (n 0)) creates a bessel function (Jn) generator.\n\
;;   (bess gen (fm 0.0)) returns Jn."
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let ((result (/ (bes-jn n angle) norm)))
      (set! angle (+ angle frequency fm))
      result)))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-bess 100.0 :n 0)))
    (do ((i 0 (+ i 1)))
	((= i 1000))
      (outa i (bess gen)))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen1 (make-bess 400.0 :n 1))
	(gen2 (make-bess 400.0 :n 1))
	(vol (make-env '(0 0 1 1 9 1 10 0) :scaler 2.0 :length 20000)))
    (do ((i 0 (+ i 1)))
	((= i 20000))
      (outa i (bess gen1 (* (env vol) (bess gen2 0.0)))))))

;;; max amps:
(do ((i 1 (+ i 1)))
    ((= i 100))
  (let ((mx 0.0))
    (do ((k 0.0 (+ k .001)))
	((> k 200))
      (let ((val (bes-jn i k)))
	(if (> (abs val) mx)
	    (set! mx (abs val)))))
    (format #t ";~A" (+ mx .001))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen1 (make-bess 400.0 :n 1))
	(gen2 (make-oscil 400.0))
	(vol (make-env '(0 1 1 0) :scaler 1.0 :length 20000)))
    (do ((i 0 (+ i 1)))
	((= i 20000))
      (outa i (bess gen1 (* (env vol) (oscil gen2 0.0)))))))

;;; also gen2 800, env scl 0.2
|#




;;; --------------------------------------------------------------------------------

;;; Watson "Bessel Functions" p358 127 128 (J0(k sqrt(r^2+a^2- 2ar cos x)) = sum em Jm(ka)Jm(kr) cos mx
;;;   em here is "Neumann's factor" (p22) = 1 if m=0, 2 otherwise

(defgenerator (jjcos
	       :make-wrapper (lambda (g)
			       (set! (g 'frequency) (hz->radians (g 'frequency)))
			       g))
  (frequency *clm-default-frequency*) (r 0.5) (a 1.0) (k 1.0) (angle 0.0) fm)


(define* (jjcos gen (fm 0.0))
;;  "(make-jjcos frequency (r 0.5) (a 1.0) (k 1)) creates a jjcos generator.\n\
;;   (jjcos gen (fm 0.0)) returns a sum of cosines scaled by a product of Bessel functions."
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let* ((x angle)
	   (dc (* (bes-j0 (* k a)) (bes-j0 (* k r))))
	   (norm (- (bes-j0 (* k (sqrt (+ (* a a) (* r r) (* -2 a r))))) dc)))
      
      ;; this norm only works if the a/r/k values all small enough that the initial J0 bump dominates
      ;;   if they're large (k=10 for example), later maxes come into play.
      ;; we need a formula for a sum of JJ's
      ;;
      ;; the resultant spectra are similar to FM (we can get sharper bumps, or low-passed bumps, etc)
      
      (set! angle (+ angle fm frequency))
      (/ (- (bes-j0 (* k (sqrt (+ (* r r) 
				  (* a a)
				  (* a (* -2.0 r (cos x)))))))
	    dc)             ; get rid of DC component
	 norm))))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-jjcos 100.0 :a 1.0 :r 1.0 :k 1)))
    (do ((i 0 (+ i 1)))
	((= i 10000))
      (outa i (* .5 (jjcos gen))))))

;;; example:
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-jjcos 100.0 :a 2.0 :r 1.0 :k 1)))
    (do ((i 0 (+ i 1)))
	((= i 20000))
      (outa i (jjcos gen)))))

:(* (bes-jn 1 1) (bes-jn 1 2))
0.253788089467046
:(* (bes-jn 2 1) (bes-jn 2 2))
0.0405418594904987
:(* (bes-jn 3 1) (bes-jn 3 2))
0.00252256243314325
:(* (bes-jn 4 1) (bes-jn 4 2))
8.41951242883886e-5
which matches perfectly

set k=10
:(* (bes-jn 1 10) (bes-jn 1 20))
0.00290541944296873
:(* (bes-jn 2 10) (bes-jn 2 20))
-0.0408277687368493
:(* (bes-jn 3 10) (bes-jn 3 20))
-0.00577380202685643
:(* (bes-jn 4 10) (bes-jn 4 20))
-0.0286956880041051
:(* (bes-jn 5 10) (bes-jn 5 20))
-0.0353830269096024
:(* (bes-jn 6 10) (bes-jn 6 20))
7.96480491715688e-4
:(* (bes-jn 7 10) (bes-jn 7 20))
-0.0399227881572529
:(* (bes-jn 8 10) (bes-jn 8 20))
-0.0234795438775677
:(* (bes-jn 9 10) (bes-jn 9 20))
0.0365188087949483
:(* (bes-jn 10 10) (bes-jn 10 20))
0.0386925399194178
:(* (bes-jn 11 10) (bes-jn 11 20))
0.00755397504265978
:(* (bes-jn 12 10) (bes-jn 12 20))
-0.00754046620160803
:(* (bes-jn 13 10) (bes-jn 13 20))
-0.00591450759566936
:(* (bes-jn 14 10) (bes-jn 14 20))
-0.00175050411436045
:(* (bes-jn 15 10) (bes-jn 15 20))
-3.66078549147997e-6

which again matches

(define* (jjsin gen (fm 0.0))
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let ((x angle))
      (set! angle (+ angle fm frequency))
      (* (sin x)
	 (bes-j0 (* k (sqrt (+ (* r r) 
			       (* a a)
			       (* a (* -2.0 r (cos x)))))))))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-jjcos 100.0 :a 1.0 :r 1.0 :k 1)))
    (do ((i 0 (+ i 1)))
	((= i 10000))
      (outa i (jjsin gen)))))

(define* (jjesin gen (fm 0.0))
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let ((x angle))
      (set! angle (+ angle fm frequency))
      (* (exp (* r (- (cos x) 1.0))) ; -1 for norm , but there's huge DC offset
	 (bes-j0 (* r (sin x)))))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-jjcos 100.0 :a 1.0 :r 1.0 :k 1)))
    (do ((i 0 (+ i 1)))
	((= i 10000))
      (outa i (jjesin gen)))))

|#



;;; --------------------------------------------------------------------------------

;;; check J0(zsinx) formula 
;;; main difference from FM: index is divided by 2, J value is squared, else just like cos(sin)

(defgenerator (j0evencos
	       :make-wrapper (lambda (g)
			       (set! (g 'frequency) (hz->radians (g 'frequency)))
			       g))
  (frequency *clm-default-frequency*) (index 1.0) (angle 0.0) fm)


(define* (j0evencos gen (fm 0.0))
;;  "(make-j0evencos frequency (index 1.0)) creates a j0evencos generator.\n\
;;   (j0evencos gen (fm 0.0)) returns a sum of cosines scaled Jk^2(index/2)."
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let* ((x angle)
	   (z index)
	   (j0 (bes-j0 (* 0.5 z)))
	   (dc (* j0 j0)))
      (set! angle (+ angle fm frequency))
      (if (= dc 1.0)
	  1.0
	  (/ (- (bes-j0 (* z (sin x)))
		dc)        ; get rid of DC component
	     (- 1.0 dc)))))) ; normalize

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-j0evencos 100.0 1.0)))
    (do ((i 0 (+ i 1)))
	((= i 30000))
      (outa i (* .5 (j0evencos gen))))))

index 10 (so 10/2 is the bes-jn arg):

(let ((base (* (bes-jn 4 5.0) (bes-jn 4 5.0)))) ; max (fft norms -> 1.0)
  (do ((i 1 (+ i 1)))
      ((= i 11))
    (format #t ";~A: ~A ~A" i (* (bes-jn i 5.0) (bes-jn i 5.0)) (/ (* (bes-jn i 5.0) (bes-jn i 5.0)) base))))
					;1: 0.107308091385168 0.701072497819036
					;2: 0.00216831005396058 0.0141661502497507
					;3: 0.133101826831083 0.86958987897572
					;4: 0.153062759870046 1.0
					;5: 0.0681943848279407 0.445532178342005
					;6: 0.0171737701015899 0.112200839160164
					;7: 0.00284904116112987 0.0186135488707298
					;8: 3.38752000110201e-4 0.00221315753353599
					;9: 3.04735259399795e-5 1.99091705688911e-4
					;10: 2.15444461145164e-6 1.4075563600714e-5

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-j0evencos 100.0 0.0)) 
	(indf (make-env '(0 0 1 20) :length 30000)))
    (do ((i 0 (+ i 1)))
	((= i 30000)) 
      (set! (gen 'index) (env indf))
      (outa i (* 0.5 (j0evencos gen))))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-j0evencos 100.0 0.0)) 
	(indf (make-env '(0 0 1 20) :length 30000))
	(carrier (make-oscil 2000.0)))
    (do ((i 0 (+ i 1)))
	((= i 30000)) 
      (set! (gen 'index) (env indf))
      (outa i (* 0.5 (oscil carrier) (j0evencos gen))))))

;;; why no "carrier"?  I subtracted DC out above -- to make this look right, I need to use the bes(sin) without any fixup.

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-j0evencos 100.0 0.0)) 
	(indf (make-env '(0 20 1 0) :length 30000))
	(carrier (make-oscil 2000.0)))
    (do ((i 0 (+ i 1)))
	((= i 30000)) 
      (set! (gen 'index) (env indf))
      (outa i (* 0.5 (j0evencos gen (oscil carrier)))))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-j0evencos 100.0 0.0))                 ; also 20 800, 20 200 (less index mvt), or 200 50 
	(indf (make-env '(0 10 1 0) :length 30000))
	(carrier (make-oscil 2000.0)))
    (do ((i 0 (+ i 1)))
	((= i 30000)) 
      (set! (gen 'index) (env indf))
      (outa i (* 0.5 (j0evencos gen (* .1 (oscil carrier))))))))

(define (j0even beg dur freq amp mc-ratio index)
  (let* ((gen (make-j0evencos (* mc-ratio freq) 0.0)) 
	 (indf (make-env '(0 10 1 0) :duration dur))
	 (carrier (make-oscil freq))
	 (start (seconds->samples beg))
	 (end (+ start (seconds->samples dur))))
    (do ((i start (+ i 1)))
	((= i end))
      (set! (gen 'index) (env indf))
      (outa i (* 0.5 (j0evencos gen (* index (oscil carrier))))))))

(with-sound (:clipped #f :statistics #t :play #t)
  (do ((i 0 (+ i 1)))
      ((= i 10))
    (j0even i 1.0 2000.0 0.5 (+ .1 (* .05 i)) 0.1)))

(define* (jfm beg dur freq amp mc-ratio index (index-env '(0 1 1 1 2 0)))
  (let* ((start (seconds->samples beg))
         (end (+ start (seconds->samples dur)))
         (md (make-j0evencos (* freq mc-ratio)))
	 (cr (make-oscil 2000))
	 (vib (make-oscil 5))
	 (vibamp (hz->radians (* freq .01)))
         (ampf (make-env '(0 0 1 1 20 1 21 0) :scaler amp :duration dur)) 
         (indf (make-env index-env :scaler index :duration dur)))
    (do ((i start (+ i 1)))
	((= i end))
      (let ((vb (* vibamp (oscil vib))))
	(set! (md 'index) (env indf))
	(outa i (* (env ampf)
		   (oscil cr vb)
		   (j0evencos md (* vb mc-ratio))))))))

(with-sound (:output "test1.snd" :play #t) (jfm 0 3.0 400.0 0.5 .5 4.0 '(0 1  1 2  2 .5)))
|#


;;; --------------------------------------------------------------------------------

(defgenerator (j2cos
	       :make-wrapper (lambda (g)
			       (set! (g 'frequency) (hz->radians (g 'frequency)))
			       (if (< (g 'n) 1) (set! (g 'n) 1))
			       g))
  (frequency *clm-default-frequency*) (r 0.5) (n 1) (angle 0.0) fm)


(define* (j2cos gen (fm 0.0))
;;  "(make-j2cos frequency (r 0.5) (n 1)) creates a j2cos generator.\n\
;;   (j2cos gen (fm 0.0)) returns a sum of cosines scaled in a very complicated way."
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let* ((x angle)
	   (rsinx2 (* 2.0 r (sin (* 0.5 x)))))
      (set! angle (+ angle fm frequency))
      (if (< (abs rsinx2) nearly-zero)
	  1.0
	  (/ (bes-jn n rsinx2)
	     rsinx2)))))

;;; this goes berserk if n=0, needs normalization, dc omission, doc/test
;;; if n=1, sample 0 = 1, the rest are in the .5 range!
;;; maybe j2cos isn't all that useful...

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-j2cos 100.0 :r 1.0 :n 0)))
    (do ((i 0 (+ i 1)))
	((= i 10000))
      (outa i (* .5 (j2cos gen))))))
|#




;;; --------------------------------------------------------------------------------

(defgenerator (jpcos
	       :make-wrapper (lambda (g)
			       (set! (g 'frequency) (hz->radians (g 'frequency)))
			       (if (= (g 'r) (g 'a))
				   (begin
				     (snd-warning (format #f ";jpcos r and a can't be equal (~A)" (g 'r)))
				     (set! (g 'r) (+ (g 'a) .01))))
			       g))
  (frequency *clm-default-frequency*) (r 0.5) (a 0.0) (k 1.0) (angle 0.0) fm)


(define* (jpcos gen (fm 0.0))
;;  "(make-jpcos frequency (r 0.5) (a 0.0) (k 1)) creates a jpcos generator.\n\
;;   (jpcos gen (fm 0.0)) returns a sum of cosines scaled in a very complicated way."
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let* ((x angle)
	   ;; (dc (/ (* (sin (* k a)) (sin (* k r))) (* k a r)))
	   ;; from P0(x)=1, J[1/2](x)=sqrt(2/(pi x))sin(x), omitting original 1/pi
	   ;;   G&R 914 (8.464), 974 (8.912), but it's missing some remaining (small) component
	   ;; also omitting the original divide by (* pi (sqrt arg)) -- it's just an amplitude scaler
	   ;;   and in this context, we get -1..1 peak amps from the sin anyway.
	   (arg (+ (* r r) 
		   (* a a)
		   (* a (* -2.0 r (cos x))))))
      (set! angle (+ angle fm frequency))
      (if (< (abs arg) nearly-zero) ; r = a, darn it! This will produce a spike, but at least it's not a NaN
	  1.0
	  (sin (* k (sqrt arg)))))))

#|
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-jpcos 100.0 :a 1.0 :r 0.5 :k 1)))
    (do ((i 0 (+ i 1)))
	((= i 210000))
      (outa i (jpcos gen)))))

(with-sound (:clipped #f :statistics #t)
  (let* ((gen (make-jpcos 400.0 :a 1.0 :r 0.5 :k 10))
	 (dur 1.0)
	 (samps (seconds->samples dur))
	 (ampf (make-env '(0 0 1 1 10 1 11 0) :duration dur :scaler 0.5))
	 (indf (make-env '(0 0 1 1) :duration dur :scaler 1.0)))
    (do ((i 0 (+ i 1)))
	((= i samps))
      (set! (gen 'r) (env indf))
      (outa i (* (env ampf)
		 (jpcos gen))))))

;;; -.725, 1/.275
(with-sound (:clipped #f :scaled-to .5) 
  (let* ((gen (make-oscil 100.0))) 
    (do ((i 0 (+ i 1))) 
	((= i 44100)) 
      (outa i (sqrt (+ 1.0 (oscil gen)))))))

(with-sound (:clipped #f :scaled-to .5) 
  (let* ((gen (make-oscil 100.0))
	 (indf (make-env '(0 .1 1 .9) :length 44100)))
    (do ((i 0 (+ i 1))) 
	((= i 44100)) 
      (let ((ind (env indf)))
	(outa i (sqrt (+ (* 1.0 1.0) (* ind ind) (* -2 1.0 ind (oscil gen)))))))))

;;; rkcos r=.4 or so (.6?), so rkcos+indf is mostly equivalent? (k=scaler in both)

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-rkcos 440.0 :r 0.6)) 
	(gen1 (make-oscil 440.0)) 
	(indf (make-env '(0 .1 1 .8) :length 50000)))
    (do ((i 0 (+ i 1)))
	((= i 50000)) 
      (set! (gen 'r) (env indf))
      (outa i (oscil gen1 (* (gen 'r) (rkcos gen)))))))
|#



;;; --------------------------------------------------------------------------------

(defgenerator (jncos :make-wrapper (lambda (g)
				     (set! (g 'frequency) (hz->radians (g 'frequency)))
				     g))
  (frequency *clm-default-frequency*) (r 0.5) (a 1.0) (n 0) (angle 0.0) fm)


(define* (jncos gen (fm 0.0))
;;  "(make-jncos frequency (r 0.5) (a 1.0) (n 0)) creates a jncos generator.\n\
;;   (jncos gen (fm 0.0)) returns a sum of cosines scaled in a very complicated way."
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let ((arg (sqrt (+ (* r r) 
			(* a a)
			(* a (* -2.0 r (cos angle)))))))
      (set! angle (+ angle fm frequency))
      (if (< arg nearly-zero)
	  1.0
	  (/ (bes-jn n arg)
	     (expt arg n))))))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-jncos 100.0 :a 0.5 :r 1.0 :n 0)))
    (do ((i 0 (+ i 1)))
	((= i 41000))
      (outa i (jncos gen)))))
|#



;;; --------------------------------------------------------------------------------

;;; use J0(cos)+J1(cos) to get full spectrum

(defgenerator (j0j1cos
	       :make-wrapper (lambda (g)
			       (set! (g 'frequency) (hz->radians (g 'frequency)))
			       g))
  (frequency *clm-default-frequency*) (index 1.0) (angle 0.0) fm)


(define* (j0j1cos gen (fm 0.0))
;;  "(make-j0j1cos frequency (index 1.0)) creates a j0j1cos generator.\n\
;;   (j0j1cos gen (fm 0.0)) returns a sum of cosines scaled in a very complicated way."
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let* ((x angle)
	   (z index)
	   (j0 (bes-j0 (* 0.5 z)))
	   (dc (* j0 j0))
	   (arg (* z (cos x))))
      (set! angle (+ angle fm frequency))
      (/ (- (+ (bes-j0 arg)
	       (bes-j1 arg))
	    dc)        ; get rid of DC component
	 1.215))))      ; not the best...

					; need to normalize j0j1cos -- min depends on index, so peak depends on max and min and dc
					;       (max (- 1.2154 dc)
					;	    (- -0.5530 dc)

#|
(let ((mx 0.0) (x 0.0) (saved-x 0.0))
  (do ((i 0 (+ i 1)))
      ((= i 1000))
    (let ((val (+ (bes-j0 x) (bes-j1 x))))
      (if (> (abs val) mx)
	  (begin
	    (set! mx (abs val))
	    (set! saved-x x)))
      (set! x (+ x .001))))
  (list mx saved-x))

(1.21533317877749 0.825000000000001)
(1.21533318495717 0.824863000002882)
(1.21533318495718 0.824863061409846)

(-0.552933995255066 4.57000000000269)
(-0.552933995483144 4.56997100028488)

(do ((i 0 (+ i 1)))
    ((= i 10))
  (let ((pk (maxamp 
	     (with-sound (:output (make-vct 10000))
  	       (let ((gen (make-j0j1cos 100.0 i)))
		 (do ((i 0 (+ i 1)))
		     ((= i 10000))
		   (outa i (j0j1cos gen))))))))
    (format #t ";~A: ~A" i pk)))
					;0: 0.0
					;1: 0.555559098720551
					;2: 0.938335597515106
					;3: 0.953315675258636
					;4: 1.16509592533112
					;5: 1.21275520324707
					;6: 1.14727067947388
					;7: 1.07083106040955
					;8: 1.05760526657104
					;9: 1.11238932609558
					;10: 1.1824289560318
					;11: 1.21528387069702
					;12: 1.19094204902649
					;13: 1.14720714092255
					;14: 1.12512302398682

|#

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-j0j1cos 100.0 1.0)))
    (do ((i 0 (+ i 1)))
	((= i 30000))
      (outa i (j0j1cos gen)))))
|#



;;; --------------------------------------------------------------------------------

(defgenerator (jycos
	       :make-wrapper (lambda (g)
			       (set! (g 'frequency) (hz->radians (g 'frequency)))
			       (set! (g 'r) (max .0001 (g 'r))) ; 0->inf in bes-y0
			       (let ((a (g 'a)) ; "c"
				     (r (g 'r))); "b"
				 (if (<= r a)
				     (format #t ";jycos a: ~A must be < r: ~A" a r))
				 (if (<= (+ (* a a) (* r r)) (* 2 a r))
				     (format #t ";jycos a: ~A, r: ~A will cause bes-y0 to return -inf!" a r)))
			       g))
  (frequency *clm-default-frequency*) (r 1.0) (a 0.5) ; "b" and "c" in the docs
  (angle 0.0) fm)


(define* (jycos gen (fm 0.0))
;;  "(make-jycos frequency (r 1.0) (a 0.5)) creates a jycos generator.\n\
;;   (jycos gen (fm 0.0)) returns a sum of cosines scaled by Yn(r)*Jn(r)."
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let* ((x angle)
	   (b r)
	   (c a)
	   (b2c2 (+ (* b b) (* c c)))
	   (dc (* (bes-y0 b) (bes-j0 c)))
	   (norm (abs (- (bes-y0 (sqrt (+ b2c2 (* -2 b c)))) dc))))
      (set! angle (+ angle fm frequency))
      (/ (- (bes-y0 (sqrt (+ b2c2 (* -2.0 b c (cos x)))))
	    dc)
	 norm))))

;;; oops -- bes-y0(0) is -inf!
;;; norm only works for "reasonable" a and r

#|
(with-sound (:clipped #f :statistics #t :play #f)
  (let ((gen (make-jycos 100.0 1.5 1.0))
	(af (make-env '(0 0 1 1) :length 30000))
	(rf (make-env '(0 3 1 3) :length 30000))
	(ampf (make-env '(0 0 1 1 10 1 11 0) :scaler 0.5 :length 30000)))
    (do ((i 0 (+ i 1)))
	((= i 30000))
      (set! (gen 'a) (env af))
      (set! (gen 'r) (env rf))
      (outa i (* (env ampf)
		 (jycos gen))))))

:(* (bes-yn 1 1.5) (bes-jn 1 1.0))
-0.181436652807559
:(* (bes-yn 2 1.5) (bes-jn 2 1.0))
-0.107112311628537
:(* (bes-yn 3 1.5) (bes-jn 3 1.0))
-0.0405654243875417

:(/ .107 .181)
0.591160220994475  [0.600]
:(/ .040 .181)
0.220994475138122  [0.228]
|#



;;; --------------------------------------------------------------------------------

#|
(defgenerator (jcos
	       :make-wrapper (lambda (g)
			       (set! (g 'frequency) (hz->radians (g 'frequency)))
			       g))
  (frequency *clm-default-frequency*) (n 0) (r 1.0) (a 0.5) ; "b" and "c" in the docs
  (angle 0.0) fm)


(define* (jcos gen (fm 0.0))
;;  "(make-jcos frequency (n 0) (r 1.0) (a 0.5)) creates a jcos generator.\n\
;;   (jcos gen (fm 0.0)) returns a sum of cosines scaled in some complex manner."
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let* ((x angle)
	   (b r)
	   (c a)
	   (dc (* (bes-j0 b) (bes-j0 c))))
      (set! angle (+ angle fm frequency))
      (- (bes-jn n (* (+ n 1) (sqrt (+ (* b b) (* c c) (* -2.0 b c (cos x))))))
	 dc))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-jcos 100.0 0 1.0 1.0)))
    (do ((i 0 (+ i 1)))
	((= i 30000))
      (outa i (jcos gen)))))
|#



;;; --------------------------------------------------------------------------------

#|
(defgenerator (sin2n
	       :make-wrapper (lambda (g)
			       (set! (g 'frequency) (hz->radians (g 'frequency)))
			       g))
  (frequency *clm-default-frequency*) (n 1) (r 1.0) (angle 0.0) fm)


(define* (sin2n gen (fm 0.0))
;;  "(make-sin2n frequency (n 0) (r 1.0)) creates a sin2n generator.\n\
;;   (sin2n gen (fm 0.0)) returns (r*sin)^(2n)"
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let* ((x angle))
      (set! angle (+ angle fm frequency))
      (expt (* r (sin x)) (* 2 n)))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-sin2n 100.0 2 1.0)))
    (do ((i 0 (+ i 1)))
	((= i 30000))
      (outa i (sin2n gen)))))
|#



;;; --------------------------------------------------------------------------------

#|
;;; do we need modulo 2*pi for the angles? (it is not used in clm.c)

:(let ((ph 0.0)) (do ((i 0 (+ i 1))) ((= i 22050)) (set! ph (+ ph (hz->radians 100.0)))) ph)
628.31850751536

:(let ((ph (* 2 pi 1000000))) (do ((i 0 (+ i 1))) ((= i 22050)) (set! ph (+ ph (hz->radians 100.0)))) (- ph (* 2 pi 1000000)))
628.318502381444

:(let ((ph (* 2 pi 1000000000))) (do ((i 0 (+ i 1))) ((= i 22050)) (set! ph (+ ph (hz->radians 100.0)))) (- ph (* 2 pi 1000000000)))
628.311109542847

:(let ((ph (* 2 pi 1000000000000))) (do ((i 0 (+ i 1))) ((= i 22050)) (set! ph (+ ph (hz->radians 100.0)))) (- ph (* 2 pi 1000000000000)))
624.462890625

;; similar results from running oscil with 0.0 initial-phase, and 2*pi*1000000000, or running one
;;   oscil for 3 hours at 6000 Hz -- the sinusoid is clean even around an angle of a billion -- worst 
;;   case increment is pi, so we get (say) a billion samples before we may notice a sag => ca. 8 hours.  
;;   I think that's a long enough tone...  (In clm.c and here, the phase and increment are both doubles;
;;   53 bits of mantissa, billion=30, so we still have about 23 bits, which actually matches results above).
|#


;;; --------------------------------------------------------------------------------

;;; blackman as a waveform -- all the other fft windows could be implemented
;;;   perhaps most useful as an amplitude envelope

#|
(defgenerator (blackman
	       :make-wrapper (lambda (g)
			       (let ((n (g 'n)))
				 (set! n (min (max n 1) 10))
				 (set! (g 'frequency) (hz->radians (g 'frequency)))
				 (case n
				   ((1) (set! (g 'coeffs) (vct 0.54 -0.46)))
				   ((2) (set! (g 'coeffs) (vct 0.34401 -0.49755 0.15844)))
				   ((3) (set! (g 'coeffs) (vct 0.21747 -0.45325 0.28256 -0.04672)))
				   ((4) (set! (g 'coeffs) (vct 0.084037 -0.29145 0.375696 -0.20762 0.041194)))
				   ((5) (set! (g 'coeffs) (vct 0.097167 -0.3088448 0.3626224 -0.1889530 0.04020952 -0.0022008)))
				   ((6) (set! (g 'coeffs) (vct 0.063964353 -0.239938736 0.3501594961 -0.247740954 0.0854382589
								  -0.012320203 0.0004377882)))
				   ((7) (set! (g 'coeffs) (vct 0.04210723 -0.18207621 0.3177137375 -0.284437984 0.1367622316
								  -0.033403806 0.0034167722 -0.000081965)))
				   ((8) (set! (g 'coeffs) (vct 0.027614462 -0.135382235 0.2752871215 -0.298843294 0.1853193194
								  -0.064888448 0.0117641902 -0.000885987 0.0000148711)))
				   ((9) (set! (g 'coeffs) (vct 0.01799071953 -0.098795950 0.2298837751 -0.294112951 0.2243389785
								  -0.103248745 0.0275674108 -0.003839580	0.0002189716 -0.000002630)))
				   ((10) (set! (g 'coeffs) (vct 0.0118717384 -0.071953468 0.1878870875 -0.275808066 0.2489042133 
								   -0.141729787 0.0502002984 -0.010458985 0.0011361511 -0.000049617
								   0.0000004343))))
				 g))
	       :methods (list
			 (cons 'mus-reset
			       (lambda (g)
				 (set! (g 'angle) 0.0)))))
  (frequency *clm-default-frequency*) (n 4) (coeffs #f) (angle 0.0) fm)


(define* (blackman gen (fm 0.0))
;;  "(make-blackman frequency (n 4)) creates a blackman generator.\n\
;;   (blackman gen (fm 0.0)) returns the nth Blackman-Harris fft data window as a periodic waveform. (n <= 10)"
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let ((x angle))
      (set! angle (+ angle fm frequency))
      (polynomial coeffs (cos x)))))
|#

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((black4 (make-blackman 440.0)))
    (do ((i 0 (+ i 1)))
	((= i 20000))
      (outa i (blackman black4 0.0)))))
|#

;;; but that is the same as polyshape/polywave!

(define blackman polywave)
(define blackman? polywave?)

(define* (make-blackman (frequency 440.0) (n 4))
  (make-polywave frequency 
		 (case n
		   ;; this data is from clm.c
		   ((0) (list 0 0))
		   ((1) (list 0 0.54 1 -0.46))
		   ((2) (list 0 0.42323 1 -0.49755 2 0.078279))
		   ((3) (list 0 0.35875 1 0.48829 2 0.14128 3 -0.01168))
		   ((4) (list 0 0.287333 1 -0.44716 2 0.20844 3 -0.05190 4 0.005149))
		   ((5) (list 0 .293557 1 -.451935 2 .201416 3 -.047926 4 .00502619 5 -.000137555))
		   ((6) (list 0 .2712203 1 -.4334446 2 .2180041 3 -.0657853 4 .010761867 5 -.0007700127 6 .00001368088))
		   ((7) (list 0 .2533176 1 -.4163269 2 .2288396 3 -.08157508 4 .017735924 5 -.0020967027 
			      6 .00010677413 7 -.0000012807))
		   ((8) (list 0 .2384331 1 -.4005545 2 .2358242 3 -.09527918 4 .025373955 5 -.0041524329 
			      6 .00036856041 7 -.00001384355 8 .0000001161808))
		   ((9) (list 0 .2257345 1 -.3860122 2 .2401294 3 -.1070542 4 .03325916 5 -.00687337 
			      6 .0008751673 7 -.0000600859 8 .000001710716 9 -.00000001027272))
		   ((10) (list 0 .2151527 1 -.3731348 2 .2424243 3 -.1166907 4 .04077422 5 -.01000904 
			       6 .0016398069 7 -.0001651660 8 .000008884663 9 -.000000193817 10 .00000000084824)))))




;;; --------------------------------------------------------------------------------


;;; we can add the sin(cos) and sin(sin) cases, using -index in the latter to get 
;;;   asymmetric fm since Jn(-B) = (-1)^n Jn(B)
;;;
;;; the same trick would work in the other two cases -- gapped spectra

(defgenerator (fmssb
	       :make-wrapper (lambda (g)
			       (set! (g 'frequency) (hz->radians (g 'frequency)))
			       g))
  (frequency *clm-default-frequency*) (ratio 1.0) (index 1.0) (angle 0.0) fm)


(define* (fmssb gen (fm 0.0))
;;  "(make-fmssb frequency (ratio 1.0) (index 1.0)) creates an fmssb generator.\n\
;;   (fmssb gen (fm 0.0)) returns single-sideband FM."
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let* ((cx angle)
	   (mx (* cx ratio)))
      (set! angle (+ angle fm frequency))
      (- (* (cos cx)
	    (sin (* index (cos mx))))
	 (* (sin cx)
	    (sin (* index (sin mx)))))))) ; use -index for the other side

;;; FM with complex index
(define* (fpmc beg dur freq amp mc-ratio fm-index interp)
  (let* ((start (seconds->samples beg))
         (end (+ start (seconds->samples dur)))
         (cr 0.0)
	 (cr-frequency (hz->radians freq))
	 (md-frequency (hz->radians (* freq mc-ratio)))
	 (md 0.0))
    (do ((i start (+ i 1)))
	((= i end))
      (let ((val (sin (+ cr (* fm-index (sin md))))))
        (outa i (* amp (+ (* (- 1.0 interp) (real-part val))
                          (* interp (imag-part val)))))
        (set! cr (+ cr cr-frequency))
        (set! md (+ md md-frequency))))))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-fmssb 1000.0 0.1 :index 8.0)))  ; 1 3 7 11 ... -- interesting effect
    (do ((i 0 (+ i 1)))
	((= i 10000))
      (outa i (* .3 (fmssb gen))))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-fmssb 1000.0 0.1 :index 8.0)) 
	(ampf (make-env '(0 0 1 1 100 0) :base 32 :scaler .3 :length 30000))
	(indf (make-env '(0 1 1 0) :length 30000 :scaler 8)))
    (do ((i 0 (+ i 1)))
	((= i 30000)) 
      (set! (gen 'index) (env indf))
      (outa i (* (env ampf) (fmssb gen))))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-fmssb 1000.0 0.05 :index 1.0)) 
	(ampf (make-env '(0 0 1 1 100 0) :base 32 :scaler .3 :length 30000))
	(indf (make-env '(0 1 1 0) :length 30000 :base 32 :scaler 10)))
    (do ((i 0 (+ i 1)))
	((= i 30000)) 
      (set! (gen 'index) (env indf))
      (outa i (* (env ampf) (fmssb gen))))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-fmssb 100.0 5.4 :index 1.0)) ; also 100 700
	(ampf (make-env '(0 0 1 1 100 0) :base 32 :scaler .3 :length 30000)) ; also 0 0 1 1 3 1 100 0...
	;; '(0 0 1 .75 2 1 3 .95 4 .5 10 0) -> bowed effect, '(0 0 1 .75 2 1 3 .125 4 .25 5 1 6 .8 20 0)
	;; '(0 0 1 .75 2 1 3 .1 4 .7 5 1 6 .8 100 0) -> clickier attack (300 too)
	(indf (make-env '(0 1 1 0) :length 30000 :base 32 :scaler 10)))
    ;; '(0 0 1 1 3 0)
    (do ((i 0 (+ i 1)))
	((= i 30000)) 
      (set! (gen 'index) (env indf))
      (outa i (* (env ampf) (fmssb gen))))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-fmssb 10.0 2.0 :index 1.0)) 
	(ampf (make-env '(0 0 1 1 3 1 100 0) :base 32 :scaler .3 :length 30000))
	(indf (make-env '(0 1 1 0) :length 30000 :base 32 :scaler 10)))
    (do ((i 0 (+ i 1)))
	((= i 30000)) 
      (set! (gen 'index) (env indf))
      (outa i (* (env ampf) (fmssb gen))))))

(with-sound (:statistics #t :scaled-to .5 :play #t)
  (let ((gen1 (make-fmssb 500 1))
	(gen2 (make-fmssb 1000 .2))
	(ampf (make-env '(0 0 1 1 100 0) :base 32 :length 30000))
	(indf (make-env '(0 1 1 1 10 0) :scaler 5.0 :base 32 :length 30000)))
    (do ((i 0 (+ i 1)))
	((= i 30000))
      (let ((ind (env indf)))
	(set! (gen1 'index) ind)
	(set! (gen2 'index) ind)
	(outa i (* (env ampf)
		   (+ (fmssb gen1 0.0)
		      (fmssb gen2 0.0))))))))

;;; imaginary machines (also imaginary beasts)
|#

(definstrument (machine1 beg dur cfreq mfreq amp index gliss)
  (let ((gen (make-fmssb cfreq (/ mfreq cfreq) :index 1.0))
	 (start (seconds->samples beg))
	 (stop (seconds->samples (+ beg dur)))
	 (ampf (make-env '(0 0 1 .75 2 1 3 .1 4 .7 5 1 6 .8 100 0) :base 32 :scaler amp :duration dur))
	 (indf (make-env '(0 0 1 1 3 0) :duration dur :base 32 :scaler index))
	 (frqf (make-env (if (> gliss 0.0) '(0 0 1 1) '(0 1 1 0)) :duration dur :scaler (hz->radians (* (/ cfreq mfreq) (abs gliss))))))
    (do ((i start (+ i 1)))
	((= i stop)) 
      (set! (gen 'index) (env indf))
      (outa i (* (env ampf) (fmssb gen (env frqf)))))))
#|

(with-sound (:statistics #t :play #t)
  (do ((i 0.0 (+ i .5)))
      ((>= i 2.0))
    (machine1 i .3 100 540 0.5 3.0 0.0)
    (machine1 i .1 100 1200 .5 10.0 200.0)
    (machine1 i .3 100 50 .75 10.0 0.0)
    (machine1 (+ i .1) .1 100 1200 .5 20.0 1200.0)
    (machine1 (+ i .3) .1 100 1200 .5 20.0 1200.0)
    (machine1 (+ i .3) .1 100 200 .5 10.0 200.0)
    (machine1 (+ i .36) .1 100 200 .5 10.0 200.0)
    (machine1 (+ i .4) .1 400 300 .5 10.0 -900.0)
    (machine1 (+ i .4) .21 100 50 .75 10.0 1000.0)
    ))

(with-sound (:statistics #t :play #t)
  (do ((i 0.0 (+ i .2)))
      ((>= i 2.0))
    (machine1 i .3 100 540 0.5 4.0 0.0)
    (machine1 (+ i .1) .3 200 540 0.5 3.0 0.0))
  (do ((i 0.0 (+ i .6)))
      ((>= i 2.0))
    (machine1 i .3 1000 540 0.5 6.0 0.0)
    (machine1 (+ i .1) .1 2000 540 0.5 1.0 0.0)
    ))

(with-sound (:statistics #t :play #t :scaled-to .5)
  (let ((gen (make-rkoddssb 1000.0 2.0 0.875))
	(noi (make-rand 15000 .02))
	(gen1 (make-rkoddssb 100.0 0.1 0.9))
	(ampf (make-env '(0 0 1 1 11 1 12 0) :duration 11.0 :scaler .5))
	(frqf (make-env '(0 0 1 1 2 0 10 0 11 1 12 0 20 0) :duration 11.0 :scaler (hz->radians 10.0))))
    (do ((i 0 (+ i 1)))
	((= i (* 12 44100)))
      (outa i (* (env ampf) 
		 (+ (rkoddssb gen1 (env frqf))
		    (* .2 (sin (rkoddssb gen (rand noi)))))))))
  
  (do ((i 0.0 (+ i 2)))
      ((>= i 10.0))
    (machine1 i 3 100 700 0.5 4.0 0.0)
    (machine1 (+ i 1) 3 200 700 0.5 3.0 0.0))
  (do ((i 0.0 (+ i 6)))
      ((>= i 10.0))
    (machine1 i 3 1000 540 0.5 6.0 0.0)
    (machine1 (+ i 1) 1 2000 540 0.5 1.0 0.0)
    ))

(with-sound (:statistics #t :play #t)
  (do ((i 0.0 (+ i .2)))
      ((>= i 2.0))
    (machine1 i .3 1200 540 0.5 40.0 0.0)
    (machine1 (+ i .1) .3 2400 540 0.5 3.0 0.0))
  (do ((i 0.0 (+ i .6)))
      ((>= i 2.0))
    (machine1 i .3 1000 540 0.5 6.0 0.0)
    (machine1 (+ i .1) .1 2000 540 0.5 10.0 100.0)
    ))

;;; same as above but up octave
(with-sound (:statistics #t :play #t)
  (do ((i 0.0 (+ i .1)))
      ((>= i 2.0))
    (machine1 i .15 2400 1080 0.25 40.0 0.0)
    (machine1 (+ i .05) .2 4800 1080 0.5 3.0 0.0))
  (do ((i 0.0 (+ i .3)))
      ((>= i 2.0))
    (machine1 i .15 2000 1080 0.5 6.0 0.0)
    (machine1 (+ i .05) .1 4000 1080 0.5 10.0 100.0)
    ))
|#

(define (fm-cancellation beg dur frequency ratio amp index)
  (let* ((cx 0.0)
	 (mx 0.0)
	 (car-frequency (hz->radians frequency))
	 (mod-frequency (hz->radians ratio))
	 (start (seconds->samples beg))
	 (stop (+ start (seconds->samples dur))))
    (do ((i start (+ i 1)))
	((= i stop))
      (outa i (* amp (- (* (cos cx)
			   (sin (* index (cos mx))))
			(* (sin cx)
			   (sin (* index (sin mx))))))
	    ;; use -index for reflection
	    )
      (set! cx (+ cx car-frequency))
      (set! mx (+ mx mod-frequency)))))

					;(with-sound () (fm-cancellation 0 1 1000.0 100.0 0.3 9.0))




;;; --------------------------------------------------------------------------------

;;; k3sin

(defgenerator (k3sin
	       :make-wrapper (lambda (g)
			       (set! (g 'frequency) (hz->radians (g 'frequency)))
			       (set! (g 'coeffs) (vct 0.0
						      (/ (* pi pi) 6.0)
						      (/ pi -4.0)
						      (/ 12.0)))
			       (set! (g 'two-pi) (* 2.0 pi))
			       g)
	       :methods (list
			 (cons 'mus-reset
			       (lambda (g)
				 (set! (g 'frequency) 0.0)
				 (set! (g 'angle) 0.0)))))
  (frequency *clm-default-frequency*) (angle 0.0) (coeffs #f) fm two-pi)


(define* (k3sin gen (fm 0.0))
;;  "(make-k3sin frequency) creates a k3sin generator.\n\
;;   (k3sin gen (fm 0.0)) returns a sum of sines scaled by k^3."
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let ((x angle))
      (if (not (<= 0.0 x two-pi))
	  (set! x (modulo x two-pi)))
      (set! angle (+ x fm frequency))
      (polynomial coeffs x))))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-k3sin 100.0)))
    (do ((i 0 (+ i 1)))
	((= i 30000))
      (outa i (k3sin gen)))))
|#



;;; --------------------------------------------------------------------------------

;;; I(z) case A&S

(defgenerator (izcos
	       :make-wrapper (lambda (g)
			       (set! (g 'frequency) (hz->radians (g 'frequency)))
			       (set! (g 'dc) (bes-i0 (g 'r)))
			       (set! (g 'norm) (- (exp (g 'r)) (g 'dc)))
			       g)
	       :methods (list
			 (cons 'mus-scaler
			       (make-procedure-with-setter
				(lambda (g) (g 'r))
			       (lambda (g val)
				 (set! (g 'r) val)
				 (set! (g 'dc) (bes-i0 val))
				 (set! (g 'norm) (- (exp val) (g 'dc)))
				 val)))))
  (frequency *clm-default-frequency*) (r 1.0) (angle 0.0)
  (dc 0.0) (norm 1.0) fm)


(define* (izcos gen (fm 0.0))
;;  "(make-izcos frequency (r 1.0)) creates an izcos generator.\n\
;;   (izcos gen (fm 0.0)) returns a sum of sines scaled by In(r)."
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let ((x angle))
      (set! angle (+ angle fm frequency))
      (if (< (abs norm) nearly-zero)
	  1.0
	  (/ (- (exp (* r (cos x)))
		dc)
	     norm)))))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-izcos 100.0 1.0)))
    (do ((i 0 (+ i 1)))
	((= i 30000))
      (outa i (* .5 (izcos gen))))))

(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-izcos 100.0 1.0))
	(indf (make-env '(0 0 1 3) :length 30000)))
    (do ((i 0 (+ i 1)))
	((= i 30000))
      (set! (mus-scaler gen) (env indf))
      (outa i (izcos gen)))))

|#



;;; --------------------------------------------------------------------------------

(definstrument (organish beg dur freq amp fm-index amp-env)
  ;; this has an organ-style chiff (better than fm index sweep)
  (let ((start (seconds->samples beg))
	(carriers (make-vector 3 #f))
	(fmoscs (make-vector 3 #f))
	(ampfs (make-vector 3 #f))
	(pervib (make-triangle-wave 5 (hz->radians (* freq .003))))
	(ranvib (make-rand-interp 6 (hz->radians (* freq .002))))
	(resc (make-nrssb 340.0 1.0 5 .5))
	(resf (make-env (list 0 0 .05 1  .1 0 dur 0) :scaler (* amp .05) :duration dur)))
    (let ((stop (+ start (seconds->samples dur))))
      (do ((i 0 (+ i 1)))
	  ((= i 3))
	(let* ((frq (* freq (expt 2 i)))
	       (index1 (hz->radians (* fm-index frq (/ 5.0 (log frq)))))
	       (index2 (hz->radians (* fm-index frq 3.0 (/ (- 8.5 (log frq)) (+ 3.0 (* frq .001))))))
	       (index3 (hz->radians (* fm-index frq (/ 4.0 (sqrt frq))))))
	  (set! (carriers i) (make-oscil frq))
	  (set! (fmoscs i) (make-polywave frq
					  :partials (list 1 index1
							  3 index2
							  4 index3)))))
      
      (set! (ampfs 0) (make-env (or amp-env '(0 0 1 1 2 1 3 0)) :scaler amp :duration dur))
      (set! (ampfs 1) (make-env (list 0 0  .04 1  .075 0 dur 0) :scaler (* amp .0125) :duration dur))
      (set! (ampfs 2) (make-env (list 0 0  .02 1  .05 0 dur 0) :scaler (* amp .025) :duration dur))
      
      ;; also good:
      ;;    (set! (ampfs 1) (make-env (list 0 0  .02 1  .05 0  (- dur .1) 0  (- dur .05) 1 dur 0) :scaler (* amp .025) :duration dur))
      ;;    (set! (ampfs 2) (make-env (list 0 0  .01 1 .025 0  (- dur .15) 0 (- dur .1) 1 dur 0) :scaler (* amp .05) :duration dur))
      
      (do ((i start (+ i 1)))
	  ((= i stop))
	(let ((vib (+ (triangle-wave pervib) (rand-interp ranvib)))
	      (sum (* (env resf)
		      (nrssb resc 0.0))))
	  (do ((k 0 (+ k 1))
	       (n 1 (* n 2)))
	      ((= k 3))
	    (set! sum (+ sum (* (env (ampfs k))
				(oscil (carriers k)
				       (+ (* n vib)
					  (polywave (fmoscs k) (* n vib))))))))
	  (outa i sum))))))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (do ((i 0 (+ i 1)))
      ((= i 10))
    (organish (* i .3) .4 (+ 100 (* 50 i)) .5 1.0 #f)))

(with-sound (:clipped #f :statistics #t :play #t)
  (do ((i 0 (+ i 1)))
      ((= i 10))
    (organish (* i .3) .4 (+ 100 (* 50 i)) .5 1.0 '(0 0 1 1 2 .5 3 .25 4 .125 10 0))))
|#



;;; --------------------------------------------------------------------------------

(defgenerator (adjustable-square-wave 
	       :make-wrapper 
	       (lambda (g)
		 (set! (g 'p1) (make-pulse-train 
				(g 'frequency) 
				(g 'amplitude)))
		 (set! (g 'p2) (make-pulse-train 
				(g 'frequency) 
				(- (g 'amplitude))
				(* 2.0 pi (- 1.0 (g 'duty-factor)))))
		 g)
	       
	       :methods (list
			 (cons 'mus-frequency
			       (make-procedure-with-setter
				(lambda (g) (mus-frequency (g 'p1)))
				(lambda (g val) (set! (mus-frequency (g 'p1)) val))))
			 (cons 'mus-phase
			       (make-procedure-with-setter
				(lambda (g) (mus-phase (g 'p1)))
				(lambda (g val) (set! (mus-phase (g 'p1)) val))))
			 (cons 'mus-scaler
			       (make-procedure-with-setter
				(lambda (g) (g 'duty-factor))
				(lambda (g val)
				  (set! (g 'duty-factor) val)
				  (set! (mus-phase (g 'p2)) (* 2.0 pi (- 1.0 (g 'duty-factor))))
				  val)))))
  
  (frequency *clm-default-frequency*) (duty-factor 0.5) (amplitude 1.0)
  (sum 0.0) (p1 #f) (p2 #f) fm)


(define* (adjustable-square-wave gen (fm 0.0))
;;  "(make-adjustable-square-wave frequency (duty-factor 0.5) (amplitude 1.0)) creates an adjustable-square-wave generator.\n\
;;   (adjustable-square-wave gen (fm 0.0)) returns a square-wave where the duty-factor sets the ratio of pulse duration to pulse period."
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (set! sum (+ sum
		 (pulse-train p1 fm)
		 (pulse-train p2 fm)))))

#|
(with-sound ()
  (let ((gen (make-adjustable-square-wave 100 .2 .5)))
    (do ((i 0 (+ i 1)))
	((= i 22050))
      (outa i (adjustable-square-wave gen)))))
|#


(defgenerator (adjustable-triangle-wave 
	       :make-wrapper 
	       (lambda (g)
		 (let ((df (g 'duty-factor)))
		   (set! (g 'gen) (make-triangle-wave (g 'frequency)))
		   (set! (g 'top) (- 1.0 df))
		   (if (not (= df 0.0))
		       (set! (g 'scl) (/ (g 'amplitude) df)))
		   g))
	       
	       :methods (list
			 (cons 'mus-frequency
			       (make-procedure-with-setter
				(lambda (g) (mus-frequency (g 'gen)))
				(lambda (g val) (set! (mus-frequency (g 'gen)) val))))
			 (cons 'mus-phase
			       (make-procedure-with-setter
				(lambda (g) (mus-phase (g 'gen)))
				(lambda (g val) (set! (mus-phase (g 'gen)) val))))
			 (cons 'mus-scaler
			       (make-procedure-with-setter
				(lambda (g) (g 'duty-factor))
				(lambda (g val)
				  (set! (g 'duty-factor) val)
				  (set! (g 'top) (- 1.0 val))
				  (if (not (= val 0.0))
				      (set! (g 'scl) (/ (g 'amplitude) val)))
				  val)))))
  
  (frequency *clm-default-frequency*) (duty-factor 0.5) (amplitude 1.0) 
  (gen #f) (top 0.0) (scl 0.0) fm)


(define* (adjustable-triangle-wave gen (fm 0.0))
;;  "(make-adjustable-triangle-wave frequency (duty-factor 0.5) (amplitude 1.0)) creates an adjustable-triangle-wave generator.\n\
;;   (adjustable-triangle-wave gen (fm 0.0)) returns a triangle-wave where the duty-factor sets the ratio of pulse duration to pulse period."
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let ((val (triangle-wave gen fm)))
      (* scl (- val (max (- top) (min top val)))))))

#|
(with-sound ()
  (let ((gen (make-adjustable-triangle-wave 100 .2 .5)))
    (do ((i 0 (+ i 1)))
	((= i 22050))
      (outa i (adjustable-triangle-wave gen)))))
|#


(defgenerator (adjustable-sawtooth-wave 
	       :make-wrapper 
	       (lambda (g)
		 (let ((df (g 'duty-factor)))
		   (set! (g 'gen) (make-sawtooth-wave (g 'frequency)))
		   (set! (g 'top) (- 1.0 df))
		   (if (not (= df 0.0))
		       (set! (g 'scl) (/ (g 'amplitude) df)))
		   g))
	       
	       :methods (list
			 (cons 'mus-frequency
			       (make-procedure-with-setter
				(lambda (g) (mus-frequency (g 'gen)))
				(lambda (g val) (set! (mus-frequency (g 'gen)) val))))
			 (cons 'mus-phase
			       (make-procedure-with-setter
				(lambda (g) (mus-phase (g 'gen)))
				(lambda (g val) (set! (mus-phase (g 'gen)) val))))
			 (cons 'mus-scaler
			       (make-procedure-with-setter
				(lambda (g) (g 'duty-factor))
				(lambda (g val)
				  (set! (g 'duty-factor) val)
				  (set! (g 'top) (- 1.0 val))
				  (if (not (= val 0.0))
				      (set! (g 'scl) (/ (g 'amplitude) val)))
				  val)))))
  
  (frequency *clm-default-frequency*) (duty-factor 0.5) (amplitude 1.0) 
  (gen #f) (top 0.0) (scl 0.0) fm)


(define* (adjustable-sawtooth-wave gen (fm 0.0))
;;  "(make-adjustable-sawtooth-wave frequency (duty-factor 0.5) (amplitude 1.0)) creates an adjustable-sawtooth-wave generator.\n\
;;   (adjustable-sawtooth-wave gen (fm 0.0)) returns a sawtooth-wave where the duty-factor sets the ratio of pulse duration to pulse period."
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let ((val (sawtooth-wave gen fm)))
      (* scl (- val (max (- top) (min top val)))))))

#|
(with-sound ()
  (let ((gen (make-adjustable-sawtooth-wave 100 .2 .5)))
    (do ((i 0 (+ i 1)))
	((= i 22050))
      (outa i (adjustable-sawtooth-wave gen)))))
|#


;;; and just for laughs... (almost anything would fit in this hack)
(defgenerator (adjustable-oscil 
	       :make-wrapper (lambda (g)
			       (let ((df (g 'duty-factor)))
				 (set! (g 'gen) (make-oscil (g 'frequency)))
				 (set! (g 'top) (- 1.0 df))
				 (if (not (= df 0.0))
				     (set! (g 'scl) (/ df)))
				 g))
	       
	       :methods (list
			 (cons 'mus-frequency
			       (make-procedure-with-setter
				(lambda (g) (mus-frequency (g 'gen)))
				(lambda (g val) (set! (mus-frequency (g 'gen)) val))))
			 (cons 'mus-phase
			       (make-procedure-with-setter
				(lambda (g) (mus-phase (g 'gen)))
				(lambda (g val) (set! (mus-phase (g 'gen)) val))))
			 (cons 'mus-scaler
			       (make-procedure-with-setter
				(lambda (g) (g 'duty-factor))
				(lambda (g val)
				  (set! (g 'duty-factor) val)
				  (set! (g 'top) (- 1.0 val))
				  (if (not (= val 0.0))
				      (set! (g 'scl) (/ val)))
				  val)))))
  
  (frequency *clm-default-frequency*) (duty-factor 0.5)
  (gen #f) (top 0.0) (scl 0.0) fm)


(define* (adjustable-oscil gen (fm 0.0))
;;  "(make-adjustable-oscil frequency (duty-factor 0.5)) creates an adjustable-oscil generator.\n\
;;   (adjustable-oscil gen (fm 0.0)) returns a sinusoid where the duty-factor sets the ratio of pulse duration to pulse period."
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let ((val (oscil gen fm)))
      (* scl (- val (max (- top) (min top val)))))))

#|
(with-sound ()
  (let ((gen (make-adjustable-oscil 100 .2)))
    (do ((i 0 (+ i 1)))
	((= i 22050))
      (outa i (adjustable-oscil gen)))))
|#




;;;--------------------------------------------------------------------------------

(define* (make-table-lookup-with-env frequency pulse-env size)
  (let* ((len (or size (clm-table-size)))
	 (ve (make-vct len))
	 (e (make-env pulse-env :length len)))
    (do ((i 0 (+ i 1)))
	((= i len))
      (vct-set! ve i (env e)))
    (make-table-lookup frequency 0.0 ve len)))

(define* (make-wave-train-with-env frequency pulse-env size)
  (let* ((len (or size (clm-table-size)))
	 (ve (make-vct len))
	 (e (make-env pulse-env :length len)))
    (do ((i 0 (+ i 1)))
	((= i len))
      (vct-set! ve i (env e)))
    (make-wave-train frequency 0.0 ve len)))



;;; --------------------------------------------------------------------------------

(defgenerator (round-interp 
	       :make-wrapper (lambda (g)
			       (set! (g 'rnd) (make-rand-interp (g 'frequency) (g 'amplitude)))
			       (set! (g 'flt) (make-moving-average (g 'n)))
			       g)
	       :methods (list
			 (cons 'mus-frequency
			       (make-procedure-with-setter
				(lambda (g) (mus-frequency (g 'rnd)))
				(lambda (g val) (set! (mus-frequency (g 'rnd)) val))))
			 (cons 'mus-phase
			       (make-procedure-with-setter
				(lambda (g) (mus-phase (g 'rnd)))
				(lambda (g val) (set! (mus-phase (g 'rnd)) val))))))
  
  (frequency *clm-default-frequency*) (n 1) (amplitude 1.0)
  (rnd #f) (flt #f) fm)


(define* (round-interp gen (fm 0.0))
;;  "(make-round-interp frequency (n 1) (amplitude 1.0)) creates a round-interp generator.\n\
;;   (round-interp gen (fm 0.0)) returns a rand-interp sequence low-pass filtered by a moving-average generator of length n."
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (moving-average flt (rand-interp rnd fm))))

#|
(with-sound (:channels 5)
  (let ((gen0 (make-round-interp 100 1))
	(gen1 (make-round-interp 100 10))
	(gen2 (make-round-interp 100 100))
	(gen3 (make-round-interp 100 1000))
	(gen4 (make-round-interp 100 10000)))
    (do ((i 0 (+ i 1)))
	((= i 100000))
      (out-any i (round-interp gen0 0.0) 0)
      (out-any i (round-interp gen1 0.0) 1)
      (out-any i (round-interp gen2 0.0) 2)
      (out-any i (round-interp gen3 0.0) 3)
      (out-any i (round-interp gen4 0.0) 4))))
|#



;;; --------------------------------------------------------------------------------
;;;
;;; env-any functions

(define (sine-env e)
  (env-any e (lambda (y)
	       (* 0.5 (+ 1.0 (sin (+ (* -0.5 pi) 
				     (* pi y))))))))

(define (square-env e)
  (env-any e (lambda (y)
	       (* y y))))

(define (blackman4-env e)
  (env-any e
	   (lambda (y)
	     (let ((cx (cos (* pi y))))
	       (+ 0.084037 (* cx (+ -.29145 (* cx (+ .375696 (* cx (+ -.20762 (* cx .041194))))))))))))

(define (multi-expt-env e expts)
  (env-any e (lambda (y)
	       (let ((b (expts (modulo (channels e) (length expts)))))
		 (/ (- (expt b y) 1.0) (- b 1.0))))))



;;; --------------------------------------------------------------------------------
;;;
;;; pm with any generator that has mus-phase and mus-run:

(define (run-with-fm-and-pm gen fm pm)
  (set! (mus-phase gen) (+ (mus-phase gen) pm))
  (let ((result (mus-run gen fm 0.0)))
    (set! (mus-phase gen) (- (mus-phase gen) pm))
    result))

#|
(let ((gen1 (make-oscil 440.0))
      (gen2 (make-oscil 440.0)))
  (do ((i 0 (+ i 1)))
      ((= i 1000))
    (let* ((pm (mus-random 1.0))
	   (val1 (oscil gen1 0.0 pm))
	   (val2 (run-with-fm-and-pm gen2 0.0 pm)))
      (if (fneq val1 val2)
	  (format #t ";run-with-fm-and-pm: ~A ~A" val1 val2)))))
|#



;;; --------------------------------------------------------------------------------

;;; cos^n J 121

(defgenerator (nchoosekcos
	       :make-wrapper (lambda (g)
			       (set! (g 'frequency) (hz->radians (g 'frequency)))
			       g))
  (frequency *clm-default-frequency*) (ratio 1.0) (n 1) (angle 0.0) fm)

(define* (nchoosekcos gen (fm 0.0))
;;  "(make-nchoosekcos frequency (ratio 1.0) (n 1)) creates an nchoosekcos generator.\n\
;;   (nchoosekcos gen (fm 0.0)) returns a sum of cosines scaled by the binomial coeffcients."
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let* ((x angle)
	   (y (* x ratio)))
      (set! angle (+ angle fm frequency))
      (* (cos x)
	 (expt (cos y) n)))))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-nchoosekcos 2000.0 0.05 10)))
    (do ((i 0 (+ i 1)))
	((= i 30000))
      (outa i (* .5 (nchoosekcos gen))))))
|#



;;; --------------------------------------------------------------------------------
;;;
;;; sinc-train

(defgenerator (sinc-train 
	       :make-wrapper (lambda (g)
			       (if (<= (g 'n) 0)
				   (begin
				     (set! (g 'original-n) 1)
				     (set! (g 'n) 3))
				   (begin
				     (set! (g 'original-n) (g 'n))
				     (set! (g 'n) (+ 1 (* 2 (g 'n)))))) ; mimic ncos
			       (set! (g 'original-frequency) (g 'frequency))
			       (set! (g 'frequency) (* 0.5 (g 'n) (hz->radians (g 'frequency))))
			       g)
	       
	       :methods (list
			 (cons 'mus-order
			       (make-procedure-with-setter
				(lambda (g) (g 'original-n))
				(lambda (g val)
				  (if (<= val 0)
				      (begin
					(set! (g 'original-n) 1)
					(set! (g 'n) 3))
				      (begin
					(set! (g 'original-n) val)
					(set! (g 'n) (+ 1 (* 2 val)))))
				  (set! (g 'frequency) (* 0.5 (g 'n) (hz->radians (g 'original-frequency))))
				  (g 'original-n))))
			 (cons 'mus-frequency
			       (make-procedure-with-setter
				(lambda (g) (g 'original-frequency))
				(lambda (g val)
				  (set! (g 'original-frequency) val)
				  (set! (g 'frequency) (* 0.5 (g 'n) (hz->radians val)))
				  val)))))
  
  (frequency *clm-default-frequency*) (n 1) (angle 0.0)
  (original-n 1) (original-frequency 0.0) fm)


(define* (sinc-train gen (fm 0.0))
;;  "(make-sinc-train frequency (n 1)) creates a sinc-train generator with n components.\n\
;;   (sinc-train gen (fm 0.0)) returns a sinc-train"
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let* ((x angle)
	   (max-angle (* pi 0.5 n))
	   (new-angle (+ x fm frequency))
	   (DC (/ 1.0 n))
	   (norm (/ n (- n 1))))
      (if (> new-angle max-angle)
	  (set! new-angle (- new-angle (* pi n))))
      (set! angle new-angle)
      (if (< (abs x) nearly-zero)
	  1.0
	  (* norm (- (/ (sin x) x) DC))))))

#|
(with-sound (:clipped #f :statistics #t)
  (let* ((g (make-sinc-train 100.0 40)))
    (do ((i 0 (+ i 1)))
	((= i 44100))
      (outa i (* .5 (sinc-train g 0.0))))))
|#



;;; --------------------------------------------------------------------------------
;;;
;;; pink-noise (based on rand-bank idea of Orfanidis)

#|
(defgenerator (pink-noise
	       :make-wrapper (lambda (g)
			       (if (<= (g 'n) 0) (set! (g 'n) 1))
			       (let ((n (g 'n)))
				 (set! (g 'rands) (make-vector n))
				 (do ((i 0 (+ i 1)))
				     ((= i n))
				   (set! ((g 'rands) i) (make-rand :frequency (/ (mus-srate) (expt 2 i))))
				   (set! (mus-phase ((g 'rands) i)) (random pi))))
			       g))
  (n 1) (rands #f))


(define (pink-noise gen)
;;  "(make-pink-noise (n 1)) creates a pink-noise generator with n octaves of rand (12 is recommended).\n\
;;  (pink-noise gen) returns the next random value in the 1/f stream produced by gen."
  
  (with-environment gen
    (/ (rand-bank rands) (* 2.5 (sqrt n))))) ; this normalization is not quite right
|#

(define* (make-pink-noise (n 1))
  (let ((v (make-vct (* n 2)))
	(amp (/ (* 2.5 (sqrt n)))))
    (set! (v 0) amp)
    (do ((i 2 (+ i 2)))
	((= i (* 2 n)))
      (set! (v i) (mus-random amp))
      (set! (v (+ i 1)) (random 1.0)))
    v))

(define pink-noise? vct?)

#|
(define (pink-noise v)
  (let ((amp (v 0))
	(sum 0.0)
	(p 0.0)
	(len (vct-length v)))
    (do ((i 2 (+ i 2))
	 (x 0.5 (* x 0.5)))
	((= i len) 
	 (+ sum (mus-random amp)))
      (set! sum (+ sum (v i)))
      (set! p (- (v (+ i 1)) x))
      (if (negative? p)
	  (begin
	    (set! (v (+ i 1)) (+ p 1.0))
	    (set! (v i) (mus-random amp)))
	  (set! (v (+ i 1)) p)))))
|#      

#|
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-pink-noise 12)))
    (do ((i 0 (+ i 1)))
	((= i 44100))
      (outa i (pink-noise gen)))))

(with-sound (:statistics #t) (let ((gen (make-pink-noise 12))) (do ((i 0 (+ i 1))) ((= i 441000)) (outa i (pink-noise gen)))))
|#




;;; --------------------------------------------------------------------------------
;;;
;;; brown-noise


(defgenerator (brown-noise
	       :make-wrapper (lambda (g)
			       (set! (g 'gr) (make-rand (g 'frequency) (g 'amplitude)))
			       g))
  (frequency *clm-default-frequency*) (amplitude 1.0) fm gr (sum 0.0) (prev 0.0))


(define* (brown-noise gen (fm 0.0))
;;  "(make-brown-noise frequency (amplitude 1.0)) returns a generator that produces brownian noise.\n\
;;  (brown-noise gen (fm 0.0)) returns the next brownian noise sample."
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let ((val (rand gr fm)))
    (if (not (= val prev))
	(begin
	  (set! prev val)
	  (set! sum (+ sum val))))
    sum)))

#|
;; this is slightly faster, but ugly
(define* (make-brown-noise (frequency 440.0) (amplitude 1.0))
  (vector 0.0 0.0 (make-rand frequency amplitude)))

(define (brown-noise? g)
  (and (vector? g)
       (= (length g) 3)
       (rand? (g 2))))

(define* (brown-noise g (fm 0.0))
  (let ((val (rand (vector-ref g 2) fm)))
    (if (not (= val (vector-ref g 1)))
	(begin
	  (vector-set! g 1 val)
	  (vector-set! g 0 (+ (vector-ref g 0) val))))
    (vector-ref g 0)))

(with-sound (:clipped #f :statistics #t)
  (let* ((gen (make-brown-noise 1000)))
    (do ((i 0 (+ i 1)))
	((= i 44100))
      (outa i (* .01 (brown-noise gen))))))
|#




;;; --------------------------------------------------------------------------------
;;;
;;; green-noise

(defgenerator (green-noise
	       :make-wrapper (lambda (g)
			       (set! (g 'gr) (make-rand (g 'frequency) (g 'amplitude)))
			       (set! (g 'sum) (* 0.5 (+ (g 'low) (g 'high))))
			       g))
  (frequency *clm-default-frequency*) (amplitude 1.0) (low -1.0) (high 1.0)
  fm gr (sum 0.0) (prev 0.0))


(define* (green-noise gen (fm 0.0))
;;  "(make-green-noise frequency (amplitude 1.0) (low -1.0) (high 1.0)) returns a new green-noise (bounded brownian noise) generator.\n\
;;   (green-noise gen (fm 0.0)) returns the next sample in a sequence of bounded brownian noise samples."
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let ((val (rand gr fm)))
    (if (not (= val prev))
	(begin
	  (set! prev val)
	  (set! sum (+ sum val))
	  (if (not (<= low sum high))
	      (set! sum (- sum (* 2 val))))))
    sum)))


#|
(define* (make-green-noise (frequency 440.0) (amplitude 1.0) (low -1.0) (high 1.0))
  (vector 0.0 0.0 low high (make-rand frequency amplitude)))

(define (green-noise? g)
  (and (vector? g)
       (= (length g) 5)
       (rand? (g 4))))

(define* (green-noise g (fm 0.0))
  (let ((val (rand (g 4) fm)))
    (if (not (= val (g 1)))
	(begin
	  (set! (g 1) val)
	  (set! (g 0) (+ (g 0) val))
	  (if (not (<= (g 2) (g 0) (g 3)))
	      (set! (g 0) (- (g 0) (* 2.0 val))))))
    (g 0)))

(with-sound (:clipped #f :statistics #t)
  (let* ((gen (make-green-noise 1000)))
    (do ((i 0 (+ i 1)))
	((= i 44100))
      (outa i (green-noise gen)))))
|#




;;; --------------------------------------------------------------------------------
;;;
;;; green-noise-interp

(defgenerator (green-noise-interp
	       :make-wrapper (lambda (g)
			       (set! (g 'sum) (* 0.5 (+ (g 'low) (g 'high))))
			       (set! (g 'dv) (/ 1.0 (ceiling (/ (mus-srate) (max 1.0 (g 'frequency))))))
			       (set! (g 'frequency) (hz->radians (g 'frequency)))
			       (set! (g 'incr) (* (mus-random (g 'amplitude)) dv))
			       (set! (g 'two-pi) (* 2.0 pi))
			       g))
  (frequency *clm-default-frequency*) (amplitude 1.0) (low -1.0) (high 1.0)
  (angle 0.0) (sum 0.0) (incr 0.0) fm two-pi dv)


(define* (green-noise-interp gen (fm 0.0))
;;  "(make-green-noise-interp frequency (amplitude 1.0) (low -1.0) (high 1.0)) returns a new interpolating green noise (bounded brownian noise) generator.\n\
;;   (green-noise-interp gen (fm 0.0)) returns the next sample in a sequence of interpolated bounded brownian noise samples."
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (if (not (<= 0.0 angle two-pi))
	(let ((val (mus-random amplitude)))
	  (set! angle (modulo angle two-pi))
	  (if (< angle 0.0) (set! angle (+ angle two-pi)))
	  (if (not (<= low (+ sum val) high))
	      (set! val (min (- high sum)
			     (max (- low sum)
				  (- val))))) ; at least bounce
	  (set! incr (* dv val))))
    (set! angle (+ angle fm frequency))
    (set! sum (+ sum incr))))

#|
(with-sound (:clipped #f :statistics #t)
  (let* ((gen (make-green-noise-interp 1000)))
    (do ((i 0 (+ i 1)))
	((= i 44100))
      (outa i (green-noise-interp gen)))))


(definstrument (green1 beg end freq amp lo hi)
  (let ((grn (make-green-noise :frequency freq :amplitude amp :high hi :low lo)))
    (do ((i beg (+ i 1)))
	((= i end))
      (outa i (green-noise grn 0.0)))))

(definstrument (green2 beg end freq amp lo hi)
  (let ((grn (make-green-noise-interp :frequency freq :amplitude amp :high hi :low lo)))
    (do ((i beg (+ i 1)))
	((= i end))
      (outa i (green-noise-interp grn 0.0)))))

(with-sound () (green1 0 10000 1000 0.1 -0.5 0.5) (green2 10000 20000 1000 0.1 -0.5 0.5))

(definstrument (green3 start dur freq amp amp-env noise-freq noise-width noise-max-step)
  ;; brownian noise on amp env
  (let ((grn (make-green-noise-interp :frequency noise-freq :amplitude noise-max-step :high (* 0.5 noise-width) :low (* -0.5 noise-width)))
	 (osc (make-oscil freq))
	 (e (make-env amp-env :scaler amp :duration dur))
	 (beg (seconds->samples start))
	 (end (seconds->samples (+ start dur))))
    (do ((i beg (+ i 1)))
	((= i end))
      (outa i (* (env e) 
		 (+ 1.0 (green-noise-interp grn 0.0))
		 (oscil osc))))))

(with-sound () (green3 0 2.0 440 .5 '(0 0 1 1 2 1 3 0) 100 .2 .02))


(definstrument (green4 start dur freq amp freq-env gliss noise-freq noise-width noise-max-step)
  ;; same but on freq env
  (let ((grn (make-green-noise-interp :frequency noise-freq 
				      :amplitude (hz->radians noise-max-step)
				      :high (hz->radians (* 0.5 noise-width))
				      :low (hz->radians (* -0.5 noise-width))))
	 (osc (make-oscil freq))
	 (e (make-env freq-env :scaler (hz->radians gliss) :duration dur))
	 (beg (seconds->samples start))
	 (end (seconds->samples (+ start dur))))
    (do ((i beg (+ i 1)))
	((= i end))
      (outa i (* amp (oscil osc (+ (env e) (green-noise-interp grn 0.0))))))))

(with-sound (:statistics #t) (green4 0 2.0 440 .5 '(0 0 1 1 2 1 3 0) 440 100 100 10))

|#



#|
;;; --------------------------------------------------------------------------------
;;;
;;; moving-max
;;;
;;; this is now a built-in generator (4-Mar-13)

(defgenerator (moving-max
	       :make-wrapper (lambda (g)
			       (let ((dly (make-delay (g 'n))))
				 (set! (g 'dly) dly)
				 (set! (mus-scaler dly) 0.0)
				 g))
	       :methods (list
			 (cons 'mus-data
			       (lambda (g) (mus-data (g 'dly))))
			 (cons 'mus-scaler
			       (make-procedure-with-setter
				(lambda (g) (mus-scaler (g 'dly)))
				(lambda (g val) (set! (mus-scaler (g 'dly)) val))))))
  (n 128) (dly #f) fm)


(define (moving-max gen fm)
;;  "(make-moving-max (n 128) returns a moving-max generator.\n\
;;  (moving-max gen input) returns the maxamp over the last n inputs."
  
  (let* ((absy (abs fm))
	 (dly (gen 'dly))
	 (mx (delay dly absy)))
    (if (>= absy (mus-scaler dly))
	(set! (mus-scaler dly) absy)
	(if (>= mx (mus-scaler dly))
	    (set! (mus-scaler dly) (vct-peak (mus-data dly)))))
    (mus-scaler dly)))
|#



;;; --------------------------------------------------------------------------------
;;;
;;; moving-sum

(defgenerator (moving-sum
	       :make-wrapper (lambda (g)
			       (let ((dly (make-moving-average (g 'n))))
				 (set! (g 'gen) dly)
				 (set! (mus-increment dly) 1.0) ; this is 1/n by default
				 g)))
  (n 128) (gen #f))


(define (moving-sum gen y)
  
  "  (make-moving-sum (n 128) returns a moving-sum generator.\n\
  (moving-sum gen input) returns the sum of the absolute values in a moving window over the last n inputs."
  
  (moving-average (gen 'gen) (abs y)))




;;; --------------------------------------------------------------------------------
;;;
;;; moving-variance
;;;
;;; this taken from the dsp bboard -- untested!

(defgenerator (moving-variance
	       :make-wrapper (lambda (g)
			       (let ((g1 (make-moving-average (g 'n)))
				     (g2 (make-moving-average (g 'n))))
				 (set! (g 'gen1) g1)
				 (set! (mus-increment g1) 1.0)
				 (set! (g 'gen2) g2)
				 (set! (mus-increment g2) 1.0)
				 g)))
  (n 128) (gen1 #f) (gen2 #f) y)


(define (moving-variance gen y)
  (environment-set! gen 'y y)
  (with-environment gen
    (let ((x1 (moving-average gen1 y))
	  (x2 (moving-average gen2 (* y y))))
      (/ (- (* n x2)
	    (* x1 x1))
	 (* n (- n 1))))))


#|
(with-sound (:clipped #f)
  (let ((gen (make-moving-variance 128))) 
    (do ((i 0 (+ i 1))) 
	((= i 10000)) 
      (outa i (moving-variance gen (random 1.0))))))
|#


;;; similarly (moving-inner-product x y) -> (moving-sum (* x y)), 
;;;           (moving-distance x y) -> (sqrt (moving-sum (* (- x y) (- x y))))



;;; --------------------------------------------------------------------------------
;;;
;;; moving-rms

(defgenerator (moving-rms
	       :make-wrapper (lambda (g)
			       (set! (g 'gen) (make-moving-average (g 'n)))
			       g))
  (n 128) (gen #f) y)


(define (moving-rms gen y)
;;  "(make-moving-rms (n 128) returns a moving-rms generator.\n\
;;  (moving-rms gen input) returns the rms of the values in a window over the last n inputs."

  (environment-set! gen 'y y)
  (with-environment gen
    (sqrt (max 0.0 ;; this is tricky -- due to floating point inaccuracy, we can get negative output
	       ;;   from moving-rms even if all the inputs are positive!  The sqrt then returns
	       ;;   a complex number and all hell breaks loose
	       (moving-average gen (* y y))))))




;;; --------------------------------------------------------------------------------
;;;
;;; moving-length

(defgenerator (moving-length
	       :make-wrapper (lambda (g)
			       (let ((dly (make-moving-average (g 'n))))
				 (set! (g 'gen) dly)
				 (set! (mus-increment dly) 1.0)
				 g)))
  (n 128) (gen #f) y)


(define (moving-length gen y)
;;  "(make-moving-length (n 128) returns a moving-length generator.\n\
;;  (moving-length gen input) returns the length of the values in a window over the last few inputs."

  (environment-set! gen 'y y)
  (with-environment gen
    (sqrt (max 0.0 (moving-average gen (* y y))))))


#|
(let ((ml (make-moving-length 128))
      (rd (make-readin "oboe.snd")))
  (with-sound () 
    (do ((i 0 (+ i 1)))
	((= i 50828))
      (outa i (moving-length ml (readin rd))))))
|#

#|
;; perhaps also use moving-rms gen to avoid amplifying noise-sections (or even squlech them)
(define* (agc (ramp-speed .001) (window-size 512))
  (let ((maxer (make-moving-max window-size))
	(mult 1.0))
    (map-channel
     (lambda (y)
       (let* ((curmax (moving-max maxer y))
	      (diff (- 0.5 (* mult curmax)))
	      (this-incr (* diff ramp-speed)))
	 (set! mult (+ mult this-incr))
	 (* y mult))))))

;;; moving-mean = average
|#



;;; --------------------------------------------------------------------------------
;;;
;;; weighted-moving-average
;;;
;;; arithmetic (1/n) weights

(defgenerator (weighted-moving-average
	       :make-wrapper (lambda (g)
			       (let ((dly (make-moving-average (g 'n))))
				 (set! (mus-increment dly) 1.0)
				 (set! (g 'dly) dly)
				 g)))
  (n 128) (dly #f) (num 0.0) (sum 0.0) y)


(define (weighted-moving-average gen y)
  
  "  (make-weighted-moving-average (n 128) returns a weighted-moving-average generator.\n\
  (weighted-moving-average gen y) returns the sum of the last n inputs weighted by 1/n"
  
  (environment-set! gen 'y y)
  (with-environment gen
    (let* ((n (mus-order dly))
	   (den (/ (* (+ 1 n) n) 2)))
      (set! num (- (+ num (* n y)) sum))
      (set! sum (moving-average dly y))
      (/ num den))))



;;; --------------------------------------------------------------------------------
;;;
;;; exponentially-weighted-moving-average
;;;
;;; geometric (r^n) weights

(defgenerator (exponentially-weighted-moving-average
	       :make-wrapper (lambda (g)
			       (let* ((n (g 'n))
				      (flt (make-one-pole (/ 1.0 n) (/ (- n) (+ 1.0 n)))))
				 (set! (g 'gen) flt)
				 g)))
  (n 128) (gen #f))


(define (exponentially-weighted-moving-average gen y)
  "  (make-exponentially-weighted-moving-average (n 128) returns an exponentially-weighted-moving-average generator.\n\
  (exponentially-weighted-moving-average gen y) returns the sum of the last n inputs weighted by (-n/(n+1))^k"
  (one-pole (gen 'gen) y))




;;; --------------------------------------------------------------------------------
;;;
;;; polyoid -- Tn + Un to get arbitrary initial-phases

(defgenerator (polyoid
	       :make-wrapper (lambda (g)
			       (let* ((lst (g 'partial-amps-and-phases))
				      (len (length lst))
				      (topk (let ((n 0))
					      (do ((i 0 (+ i 3)))
						  ((>= i len))
						(set! n (max n (floor (lst i)))))
					      n))
				      (sin-amps (make-vct (+ topk 1) 0.0))
				      (cos-amps (make-vct (+ topk 1) 0.0)))
				 (do ((j 0 (+ j 3)))
				     ((>= j len))
				   (let ((n (floor (lst j)))
					 (amp (lst (+ j 1)))
					 (phase (lst (+ j 2))))
				     (if (> n 0)                                   ; constant only applies to cos side
					 (set! (sin-amps n) (* amp (cos phase))))
				     (set! (cos-amps n) (* amp (sin phase)))))
				 (set! (g 'tn) cos-amps)
				 (set! (g 'un) sin-amps)
				 (set! (g 'frequency) (hz->radians (g 'frequency)))
				 g))
	       
	       :methods (list
			 (cons 'mus-data
			       (lambda (g) (g 'tn)))
			 (cons 'mus-xcoeffs
			       (lambda (g) (g 'tn)))
			 (cons 'mus-ycoeffs
			       (lambda (g) (g 'un)))
			 (cons 'mus-xcoeff
			       (make-procedure-with-setter
				(lambda (g ind) ((g 'tn) ind))
				(lambda (g ind val) (vct-set! (g 'tn) ind val))))
			 (cons 'mus-ycoeff
			       (make-procedure-with-setter
				(lambda (g ind) ((g 'un) ind))
				(lambda (g ind val) (vct-set! (g 'un) ind val))))))
  
  (frequency *clm-default-frequency*) (partial-amps-and-phases #f) (angle 0.0)
  (tn #f) (un #f) fm)


(define* (polyoid gen (fm 0.0))
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let ((x angle))
      (set! angle (+ angle fm frequency))
      (mus-chebyshev-tu-sum x tn un))))


(define (polyoid-env gen fm amps phases)
  ;; amps and phases are the envelopes, one for each harmonic, setting the sample-wise amp and phase
  (let* ((original-data (polyoid-partial-amps-and-phases gen))
	 (data-len (length original-data))
	 (amps-len (length amps))
	 (tn (polyoid-tn gen))
	 (un (polyoid-un gen)))
    (do ((i 0 (+ i 3))
	 (j 0 (+ j 1)))
	((or (= j amps-len)
	     (= i data-len)))
      (let ((hn (floor (original-data i)))
	    (amp (env (amps j)))
	    (phase (env (phases j))))
	(set! (tn hn) (* amp (sin phase)))
	(set! (un hn) (* amp (cos phase)))))
    (polyoid gen fm)))

#|
(with-sound (:clipped #f)
  (let ((samps 44100)
	(gen (make-polyoid 100.0 (vector 1 1 0.0))))
    (do ((i 0 (+ i 1)))
	((= i samps))
      (outa i (polyoid gen)))))

(with-sound (:clipped #f)
  (let ((samps 44100)
	(gen (make-polywave 100.0 '(1 1) mus-chebyshev-second-kind))
	(gen1 (make-oscil 100.0)))
    (set! (mus-phase gen) (* 0.5 pi))
    (do ((i 0 (+ i 1)))
	((= i samps))
      (outa i (* (oscil gen1) (polywave gen))))))

(with-sound (:clipped #f :statistics #t)
  (let ((samps 44100)
	(gen (make-polyoid 100.0 (vector 1 0.5 0.0 51 0.25 0.0 64 .25 (/ pi 2)))))
    (do ((i 0 (+ i 1)))
	((= i samps))
      (outa i (polyoid gen)))))

(define (test-polyoid n)
  (let* ((res (with-sound (:channels 2 :clipped #f)
		(let ((freqs (make-vct n))
		      (phases (make-vct n))           ; for oscil-bank
		      (cur-phases (make-vct (* 3 n))) ; for polyoid
		      (amp (/ 1.0 n)))
		  (do ((i 0 (+ i 1))
		       (j 0 (+ j 3)))
		      ((= i n))
		    (set! (cur-phases j) (+ i 1))
		    (set! (cur-phases (+ j 1)) (/ 1.0 n))
		    (set! (cur-phases (+ j 2)) (random (* 2 pi)))
		    
		    (set! (freqs i) (hz->radians (+ i 1.0)))
		    (set! (phases i) (cur-phases (+ j 2))))
		  
		  (let ((gen (make-polyoid 1.0 cur-phases))
			(obank (make-oscil-bank freqs phases)))
		    (do ((i 0 (+ i 1)))
			((= i 88200))
		      (outa i (* amp (oscil-bank obank))))
		    (do ((i 0 (+ i 1)))
			((= i 88200))
		      (outb i (polyoid gen 0.0)))))))
	 (snd (find-sound res)))
    (channel-distance snd 0 snd 1)))

;;; 0 diff up to 4096 so far (unopt and opt) -- 1.0e-12 at 4096, opt is more than 20 times as fast


(with-sound (:clipped #f :channels 2 :statistics #t)
  (let* ((samps 44100)
	 (gen1 (make-polyoid 100.0 (vector 1 0.5 0.0  3 0.25 0.0  4 .25 0.0)))
	 (gen2 (make-polyoid 100.0 (vector 1 0.5 0.0  3 0.25 0.0  4 .25 0.0)))
	 (amps1 (vector (make-env '(0 0 1 1 2 0) :end samps :scaler 0.5)
			(make-env '(0 1 1 0 2 1) :end samps :scaler 0.25)
			(make-env '(0 1 1 0) :end samps :scaler 0.25)))
	 (phases1 (vector (make-env '(0 0 1 1) :end samps :scaler (/ pi 2))
			  (make-env '(0 0 1 1) :end samps :scaler (/ pi 2))
			  (make-env '(0 1 1 0) :end samps :scaler (/ pi 2))))
	 (amps2 (vector (make-env '(0 0 1 1 2 0) :end samps :scaler 0.5)
			(make-env '(0 1 1 0 2 1) :end samps :scaler 0.25)
			(make-env '(0 1 1 0) :end samps :scaler 0.25)))
	 (phases2 (vector (make-env '(0 0 1 0) :end samps)
			  (make-env '(0 0 1 0) :end samps)
			  (make-env '(0 0 1 0) :end samps))))
    (do ((i 0 (+ i 1)))
	((= i samps))
      (outa i (polyoid-env gen1 0.0 amps1 phases1))
      (outb i (polyoid-env gen2 0.0 amps2 phases2)))))


(with-sound (:clipped #f :channels 2 :channels 3 :statistics #t)
  (let* ((samps 44100)
	 (gen1 (make-polyoid 100.0 (vector 1 1 0 2 1 0 3 1 0)))
	 (gen2 (make-polyoid 100.0 (vector 1 1 0 2 1 0 3 1 0)))
	 (gen3 (make-polyoid 100.0 (vector 1 1 (/ pi 2) 2 1 (/ pi 2) 3 1 (/ pi 2))))
	 (amps1 (vector (make-env '(0 1 1 1) :end samps) (make-env '(0 1 1 1) :end samps) (make-env '(0 1 1 1) :end samps)))
	 (amps2 (vector (make-env '(0 1 1 1) :end samps) (make-env '(0 1 1 1) :end samps) (make-env '(0 1 1 1) :end samps)))
	 (amps3 (vector (make-env '(0 1 1 1) :end samps) (make-env '(0 1 1 1) :end samps) (make-env '(0 1 1 1) :end samps)))
	 (phases1 (vector (make-env '(0 0 1 0) :end samps) (make-env '(0 0 1 0) :end samps) (make-env '(0 0 1 0) :end samps)))
	 (phases2 (vector (make-env '(0 0 .1 0 .9 1 1 1) :end samps :scaler (/ pi 2))
			  (make-env '(0 0 .1 0 .9 1 1 1) :end samps :scaler (/ pi 2))
			  (make-env '(0 0 .1 0 .9 1 1 1) :end samps :scaler (/ pi 2))))
	 (phases3 (vector (make-env '(0 1 1 1) :end samps :scaler (/ pi 2)) 
			  (make-env '(0 1 1 1) :end samps :scaler (/ pi 2)) 
			  (make-env '(0 1 1 1) :end samps :scaler (/ pi 2)))))
    (do ((i 0 (+ i 1)))
	((= i samps))
      (outa i (* .1 (polyoid-env gen1 0.0 amps1 phases1)))
      (outb i (* .1 (polyoid-env gen2 0.0 amps2 phases2)))
      (outc i (* .1 (polyoid-env gen3 0.0 amps3 phases3))))))

|#



;;; --------------------------------------------------------------------------------
;;;
;;; noid -- sum of n sinusoids at arbitrary (default=random) initial phases
;;;
;;;   for max peak (all cos), set phases arg to (make-vector n (/ pi 2))
;;;   for min peak, use one of the sets in peak-phases.scm (multiplied through by pi)
;;;
;;; since initial phases are 0 or pi in peak-phases.scm if n>20, this code could be optimized

(define* (make-noid (frequency 0.0) (n 1) (phases #f) (choice 'all))
  (make-polyoid frequency
		(let ((amps (make-vector (* 3 n) 0.0)))
		  (do ((i 1 (+ i 1))
		       (j 0 (+ j 3)))
		      ((> i n))
		    
		    (case choice
		      ((all)   (set! (amps j) i))
		      ((odd)   (set! (amps j) (- (* 2 i) 1)))
		      ((prime) (set! (amps j) (some-primes (- i 1)))) ; defined below up to 1024th or so -- probably should use low-primes.scm
		      ((even)  (set! (amps j) (max 1 (* 2 (- i 1))))))
		    
		    (set! (amps (+ j 1)) (/ 1.0 n))
		    
		    (if (vector? phases)
			(set! (amps (+ j 2)) (phases (- i 1)))
			(if (not phases)
			    (set! (amps (+ j 2)) (random (* 2 pi)))
			    (if (eq? phases 'max-peak)
				(set! (amps (+ j 2)) (/ pi 2))
				;; else min-peak, handled below
				))))
		  
		  (if (eq? phases 'min-peak)
		      (let ((vector-find-if (lambda (func vect)
					      (let ((len (length vect))
						    (result #f))
						(do ((i 0 (+ i 1)))
						    ((or (= i len)
							 result)
						     result)
						  (set! result (func (vect i))))))))
			
			(if (not (defined? 'noid-min-peak-phases))
			    (load "peak-phases.scm"))
			
			(let ((min-dat (vector-find-if 
					(lambda (val)
					  (and val
					       (vector? val)
					       (= (val 0) n)
					       (let* ((a-val (val 1))
						      (a-len (length val))
						      (a-data (list a-val (val 2))))
						 (do ((k 2 (+ k 1)))
						     ((= k a-len))
						   (if (and (number? (val k))
							    (< (val k) a-val))
						       (begin
							 (set! a-val (val k))
							 (set! a-data (list a-val (val (+ k 1)))))))
						 a-data)))
					(case choice
					  ((all) noid-min-peak-phases)
					  ((odd) nodd-min-peak-phases)
					  ((prime) primoid-min-peak-phases)
					  ((even) neven-min-peak-phases)))))
			  (if min-dat
			      (let (;(norm (car min-dat))
				    (rats (cadr min-dat)))
				(do ((i 1 (+ i 1))
				     (j 0 (+ j 3)))
				    ((> i n))
				  (set! (amps (+ j 1)) (/ 1.0 n)) ;(/ 0.999 norm)) -- can't decide about this -- I guess it should be consistent with the #f case
				  (set! (amps (+ j 2)) (* pi (rats (- i 1))))))))))
		  
		  amps)))

(define noid polyoid)
(define noid? polyoid?)


(define some-primes (vector 1
			    2    3    5    7   11   13   17   19   23   29   31   37   41   43   47   53   59   61 
			    67   71   73   79   83   89   97  101  103  107  109  113  127  131  137  139  149  151 
			    157  163  167  173  179  181  191  193  197  199  211  223  227  229  233  239  241  251 
			    257  263  269  271  277  281  283  293  307  311  313  317  331  337  347  349  353  359 
			    367  373  379  383  389  397  401  409  419  421  431  433  439  443  449  457  461  463 
			    467  479  487  491  499  503  509  521  523  541  547  557  563  569  571  577  587  593 
			    599  601  607  613  617  619  631  641  643  647  653  659  661  673  677  683  691  701 
			    709  719  727  733  739  743  751  757  761  769  773  787  797  809  811  821  823  827 
			    829  839  853  857  859  863  877  881  883  887  907  911  919  929  937  941  947  953 
			    967  971  977  983  991  997 1009 1013 1019 1021 1031 1033 1039 1049 1051 1061 1063 1069 
			    1087 1091 1093 1097 1103 1109 1117 1123 1129 1151 1153 1163 1171 1181 1187 1193 1201 1213 
			    1217 1223 1229 1231 1237 1249 1259 1277 1279 1283 1289 1291 1297 1301 1303 1307 1319 1321 
			    1327 1361 1367 1373 1381 1399 1409 1423 1427 1429 1433 1439 1447 1451 1453 1459 1471 1481 
			    1483 1487 1489 1493 1499 1511 1523 1531 1543 1549 1553 1559 1567 1571 1579 1583 1597 1601 
			    1607 1609 1613 1619 1621 1627 1637 1657 1663 1667 1669 1693 1697 1699 1709 1721 1723 1733 
			    1741 1747 1753 1759 1777 1783 1787 1789 1801 1811 1823 1831 1847 1861 1867 1871 1873 1877 
			    1879 1889 1901 1907 1913 1931 1933 1949 1951 1973 1979 1987 1993 1997 1999 2003 2011 2017 
			    2027 2029 2039 2053 2063 2069 2081 2083 2087 2089 2099 2111 2113 2129 2131 2137 2141 2143 
			    2153 2161 2179 2203 2207 2213 2221 2237 2239 2243 2251 2267 2269 2273 2281 2287 2293 2297 
			    2309 2311 2333 2339 2341 2347 2351 2357 2371 2377 2381 2383 2389 2393 2399 2411 2417 2423 
			    2437 2441 2447 2459 2467 2473 2477 2503 2521 2531 2539 2543 2549 2551 2557 2579 2591 2593 
			    2609 2617 2621 2633 2647 2657 2659 2663 2671 2677 2683 2687 2689 2693 2699 2707 2711 2713 
			    2719 2729 2731 2741 2749 2753 2767 2777 2789 2791 2797 2801 2803 2819 2833 2837 2843 2851 
			    2857 2861 2879 2887 2897 2903 2909 2917 2927 2939 2953 2957 2963 2969 2971 2999 3001 3011 
			    3019 3023 3037 3041 3049 3061 3067 3079 3083 3089 3109 3119 3121 3137 3163 3167 3169 3181 
			    3187 3191 3203 3209 3217 3221 3229 3251 3253 3257 3259 3271 3299 3301 3307 3313 3319 3323 
			    3329 3331 3343 3347 3359 3361 3371 3373 3389 3391 3407 3413 3433 3449 3457 3461 3463 3467 
			    3469 3491 3499 3511 3517 3527 3529 3533 3539 3541 3547 3557 3559 3571 3581 3583 3593 3607 
			    3613 3617 3623 3631 3637 3643 3659 3671 3673 3677 3691 3697 3701 3709 3719 3727 3733 3739 
			    3761 3767 3769 3779 3793 3797 3803 3821 3823 3833 3847 3851 3853 3863 3877 3881 3889 3907 
			    3911 3917 3919 3923 3929 3931 3943 3947 3967 3989 4001 4003 4007 4013 4019 4021 4027 4049 
			    4051 4057 4073 4079 4091 4093 4099 4111 4127 4129 4133 4139 4153 4157 4159 4177 4201 4211 
			    4217 4219 4229 4231 4241 4243 4253 4259 4261 4271 4273 4283 4289 4297 4327 4337 4339 4349 
			    4357 4363 4373 4391 4397 4409 4421 4423 4441 4447 4451 4457 4463 4481 4483 4493 4507 4513 
			    4517 4519 4523 4547 4549 4561 4567 4583 4591 4597 4603 4621 4637 4639 4643 4649 4651 4657 
			    4663 4673 4679 4691 4703 4721 4723 4729 4733 4751 4759 4783 4787 4789 4793 4799 4801 4813 
			    4817 4831 4861 4871 4877 4889 4903 4909 4919 4931 4933 4937 4943 4951 4957 4967 4969 4973 
			    4987 4993 4999 5003 5009 5011 5021 5023 5039 5051 5059 5077 5081 5087 5099 5101 5107 5113 
			    5119 5147 5153 5167 5171 5179 5189 5197 5209 5227 5231 5233 5237 5261 5273 5279 5281 5297 
			    5303 5309 5323 5333 5347 5351 5381 5387 5393 5399 5407 5413 5417 5419 5431 5437 5441 5443 
			    5449 5471 5477 5479 5483 5501 5503 5507 5519 5521 5527 5531 5557 5563 5569 5573 5581 5591 
			    5623 5639 5641 5647 5651 5653 5657 5659 5669 5683 5689 5693 5701 5711 5717 5737 5741 5743 
			    5749 5779 5783 5791 5801 5807 5813 5821 5827 5839 5843 5849 5851 5857 5861 5867 5869 5879 
			    5881 5897 5903 5923 5927 5939 5953 5981 5987 6007 6011 6029 6037 6043 6047 6053 6067 6073 
			    6079 6089 6091 6101 6113 6121 6131 6133 6143 6151 6163 6173 6197 6199 6203 6211 6217 6221 
			    6229 6247 6257 6263 6269 6271 6277 6287 6299 6301 6311 6317 6323 6329 6337 6343 6353 6359 
			    6361 6367 6373 6379 6389 6397 6421 6427 6449 6451 6469 6473 6481 6491 6521 6529 6547 6551 
			    6553 6563 6569 6571 6577 6581 6599 6607 6619 6637 6653 6659 6661 6673 6679 6689 6691 6701 
			    6703 6709 6719 6733 6737 6761 6763 6779 6781 6791 6793 6803 6823 6827 6829 6833 6841 6857 
			    6863 6869 6871 6883 6899 6907 6911 6917 6947 6949 6959 6961 6967 6971 6977 6983 6991 6997 
			    7001 7013 7019 7027 7039 7043 7057 7069 7079 7103 7109 7121 7127 7129 7151 7159 7177 7187 
			    7193 7207 7211 7213 7219 7229 7237 7243 7247 7253 7283 7297 7307 7309 7321 7331 7333 7349 
			    7351 7369 7393 7411 7417 7433 7451 7457 7459 7477 7481 7487 7489 7499 7507 7517 7523 7529 
			    7537 7541 7547 7549 7559 7561 7573 7577 7583 7589 7591 7603 7607 7621 7639 7643 7649 7669 
			    7673 7681 7687 7691 7699 7703 7717 7723 7727 7741 7753 7757 7759 7789 7793 7817 7823 7829 
			    7841 7853 7867 7873 7877 7879 7883 7901 7907 7919 7927 7933 7937 7949 7951 7963 7993 8009 
			    8011 8017 8039 8053 8059 8069 8081 8087 8089 8093 8101 8111 8117 8123 8147 8161 8167 8171))


#|
(with-sound (:clipped #f)
  (let ((samps 44100)
	(gen (make-noid 100.0 3)))
    (do ((i 0 (+ i 1)))
	((= i samps))
      (outa i (noid gen)))))

(with-sound (:clipped #f :channels 2)
  (let* ((samps 44100)
	 (n 10)
	 (gen (make-noid 1.0 n 'min-peak))
	 (gen2 (make-oscil n ((polyoid-partial-amps-and-phases gen) (- (length (polyoid-partial-amps-and-phases gen)) 1)))))
    (do ((i 0 (+ i 1)))
	((= i samps))
      (outa i (noid gen))
      (outb i (oscil gen2)))))

(with-sound (:clipped #f)
  (let ((samps 44100)
	(gen (make-noid 100.0 10 'min-peak)))
    (do ((i 0 (+ i 1)))
	((= i samps))
      (outa i (noid gen)))))

(with-sound (:clipped #f :statistics #t)
  (let ((samps 44100)
	(gen (make-noid 10.0 1024 'min-peak)))
    (do ((i 0 (+ i 1)))
	((= i samps))
      (outa i (noid gen)))))

(with-sound (:clipped #f :channels 4)
  (let ((samps 44100)
	(gen1 (make-noid 100.0 32 'max-peak))
	(gen2 (make-noid 100.0 32 (make-vector 32 0.0)))
	(gen3 (make-noid 100.0 32))
	(gen4 (make-noid 100.0 32 'min-peak)))
    (do ((i 0 (+ i 1)))
	((= i samps))
      (outa i (noid gen1 0.0))
      (outb i (noid gen2 0.0))
      (outc i (noid gen3 0.0))
      (outd i (noid gen4 0.0)))))


(do ((i 0 (+ i 1)))
    ((= i 4))
  (with-sound (:clipped #f :output (string-append "test-noid-" (number->string i) ".snd"))
    (let ((samps 44100)
	  (gen (make-noid 100.0 32 (if (= i 0) 'max-peak
				       (if (= i 1) (make-vector 32 0.0)
					   (if (= i 2) #f
					       'min-peak))))))
      (do ((i 0 (+ i 1)))
	  ((= i samps))
	(outa i (noid gen))))))

(define (knoid n)
  (with-sound (:channels 4 :statistics #t) 
    (let ((samps 100000)
	  (gen1 (make-noid 10.0 n 'min-peak 'all))
	  (gen2 (make-noid 10.0 n 'min-peak 'odd))
	  (gen3 (make-noid 10.0 n 'min-peak 'even))
	  (gen4 (make-noid 10.0 n 'min-peak 'prime)))
      (do ((i 0 (+ i 1)))
	  ((= i samps))
	(outa i (* 0.5 (noid gen1 0.0)))
	(outb i (* 0.5 (noid gen2 0.0)))
	(outc i (* 0.5 (noid gen3 0.0)))
	(outd i (* 0.5 (noid gen4 0.0)))))))

(with-sound (:clipped #f)
  (let ((samps 44100)
	(gen (make-noid 100.0 19 (apply vector (map (lambda (n) (* pi n)) (list 0 1 0 0 1 1 1 1 1 1 0 0 1 1 1 0 1 0 1) )))))
    (do ((i 0 (+ i 1))) 
	((= i samps))
      (outa i (noid gen)))))
|#

#|
;;; --------------------------------------------------------------------------------
;;;
;;; roid -- sum of n sinusoids at arbitrary (default=random) initial phases and amp r^n

(define* (make-roid (frequency 0.0) (n 1) (r 1.0) (phases #f))
  (make-polyoid frequency
		(let ((amps (make-vector (* 3 n) 0.0))
		      (rn (/ 1.0 n)))
		  (do ((i 1 (+ i 1))
		       (j 0 (+ j 3)))
		      ((> i n))
		    (set! (amps j) i)
		    (set! (amps (+ j 1)) rn)
		    (set! rn (* rn r))
		    (if (vector? phases)
			(set! (amps (+ j 2)) (phases (- i 1)))
			(if (not phases)
			    (set! (amps (+ j 2)) (random (* 2 pi)))
			    (if (eq? phases 'max-peak)
				(set! (amps (+ j 2)) (/ pi 2))
				;; else min-peak, handled separately
				))))
		  
		  (if (eq? phases 'min-peak)
		      (let ((vector-find-if (lambda (func vect)
					      (let ((len (length vect))
						    (result #f))
						(do ((i 0 (+ i 1)))
						    ((or (= i len)
							 result)
						     result)
						  (set! result (func (vect i))))))))
			
			(if (not (defined? 'roid-min-peak-phases))
			    (load "peak-phases.scm"))
			
			(let ((min-dat (vector-find-if 
					(lambda (val)
					  (and val
					       (vector? val)
					       (= (val 0) n)
					       (let* ((a-val (val 1))
						      (a-len (length val))
						      (a-data (list a-val (val 2))))
						 (do ((k 2 (+ k 1)))
						     ((= k a-len))
						   (if (and (number? (val k))
							    (< (val k) a-val))
						       (begin
							 (set! a-val (val k))
							 (set! a-data (list a-val (val (+ k 1)))))))
						 a-data)))
					roid-min-peak-phases)))
			  (if min-dat
			      (let* ((norm (car min-dat))
				     (rats (cadr min-dat))
				     (rn (/ 0.999 norm)))
				(do ((i 1 (+ i 1))
				     (j 0 (+ j 3)))
				    ((> i n))
				  (set! (amps (+ j 1)) rn)
				  (set! rn (* rn r))
				  (set! (amps (+ j 2)) (* pi (rats (- i 1))))))))))
		  
		  amps)))

(define roid polyoid)
(define roid? polyoid?)
|#

#|
(with-sound (:clipped #f)
  (let ((samps 44100)
	(gen (make-roid 100.0 6 0.5 'min-peak)))
    (do ((i 0 (+ i 1)))
	((= i samps))
      (outa i (roid gen)))))
|#



;;; ---------------- old waveshape generator ----------------

(define waveshape? polyshape?)
(define waveshape polyshape)

(define* (make-waveshape (frequency *clm-default-frequency*) 
			 (partials '(1 1)) 
			 wave 
			 (size *clm-table-size*))
  (if (not wave)
      (make-polyshape frequency :partials partials)
      (make-polyshape frequency :coeffs wave)))

(define* (partials->waveshape partials 
			      (size *clm-table-size*))
  (partials->polynomial partials))




;;; ---------------- tanh(sin(x)) ----------------

(defgenerator (tanhsin
	       :make-wrapper (lambda (g)
			       (set! (g 'osc) (make-oscil (g 'frequency) (g 'initial-phase)))
			       (set! (g 'frequency) (hz->radians (g 'frequency))) ; so that mus-frequency works at least read side
			       g))
  (frequency *clm-default-frequency*) (r 1.0) (initial-phase 0.0)
  (osc #f) fm)


(define* (tanhsin gen (fm 0.0))
;;  "(make-tanhsin (frequency 0.0) (r 1.0) (initial-phase 0.0) returns a tanhsin generator.\n\
;;  (tanhsin gen (fm 0.0)) produces tanh(r*sin) which approaches a square wave as r increases."

  (environment-set! gen 'fm fm)
  (with-environment gen
  (tanh (* r (oscil osc fm)))))



;;; ---------------- moving-fft ----------------

(defgenerator (moving-fft
	       :make-wrapper (lambda (g)
			       (let ((n (g 'n)))
				 (set! (g 'rl) (make-vct n))
				 (set! (g 'im) (make-vct n))
				 (set! (g 'data) (make-vct n))
				 (set! (g 'window) (make-fft-window hamming-window n))
				 (vct-scale! (g 'window) (/ 2.0 (* 0.54 n)))
				 (set! (g 'outctr) (+ n 1)) ; first time fill flag
				 g))
	       :methods (list
			 (cons 'mus-data (lambda (g) (g 'data)))
			 (cons 'mus-xcoeffs (lambda (g) (g 'rl)))
			 (cons 'mus-ycoeffs (lambda (g) (g 'im)))
			 (cons 'mus-run (lambda (g arg1 arg2) (moving-fft g)))))
  (input #f) (n 512) (hop 128) (outctr 0)
  (rl #f) (im #f) (data #f) 
  (window #f))


(define (moving-fft gen)
;;  "(make-moving-fft reader (size 512) (hop 128)) returns a moving-fft generator. \n\
;; (moving-fft gen) produces an FFT (polar form) of 'size' samples every 'hop' samples, \n\
;; taking input from the readin generator 'reader'.  The magnitudes are available as mus-xcoeffs, \n\
;; the phases as mus-ycoeffs, and the current input data as mus-data."
  
  (with-environment gen
    (let ((new-data #f))
      (if (>= outctr hop)
	  (let ((fft-window window))
	    (if (> outctr n) ; must be first time through -- fill data array
		(begin
		  (do ((i 0 (+ i 1)))
		      ((= i n))
		    (vct-set! data i (readin input))))
		(let ((mid (- n hop)))
		  (vct-move! data 0 hop)
		  (do ((i mid (+ i 1)))
		      ((= i n))
		    (vct-set! data i (readin input)))))
	    (set! outctr 0)
	    (set! new-data #t)
	    (vct-fill! im 0.0)
	    (vct-subseq data 0 n rl)
	    (vct-multiply! rl fft-window)
	    (mus-fft rl im n 1)
	    (rectangular->polar rl im)))
      (set! outctr (+ outctr 1))
      new-data)))


#|
(let* ((snd (new-sound))
       (rd (make-readin "oboe.snd"))
       (ft (make-moving-fft rd))
       (data (make-vct 256)))
  (set! (lisp-graph?) #t)
  (do ((i 0 (+ i 1)))
      ((= i 10000))
    (if (moving-fft ft)
	(begin
	  (vct-subseq (mus-xcoeffs ft) 0 255 data)
	  (graph data "fft" 0.0 11025.0 0.0 0.1 snd 0 #t))))
  (close-sound snd))
|#



;;; ---------------- moving spectrum ----------------

(defgenerator (moving-spectrum
	       :make-wrapper (lambda (g)
			       (let ((n (g 'n)))
				 (set! (g 'amps) (make-vct n))
				 (set! (g 'phases) (make-vct n))
				 (set! (g 'amp-incs) (make-vct n))
				 (set! (g 'freqs) (make-vct n))
				 (set! (g 'freq-incs) (make-vct n))
				 (set! (g 'new-freq-incs) (make-vct n))
				 (set! (g 'data) (make-vct n))
				 (set! (g 'fft-window) (make-fft-window hamming-window n))
				 (vct-scale! (g 'fft-window) (/ 2.0 (* 0.54 n)))
				 (set! (g 'outctr) (+ n 1)) ; first time fill flag
				 g)))
  (input #f) (n 512) (hop 128) 
  (outctr 0)
  (amps #f) (phases #f) 
  (amp-incs #f) (freqs #f) (freq-incs #f) (new-freq-incs #f) 
  (fft-window #f)
  (data #f) (dataloc 0))

(define (moving-spectrum gen)
  (with-environment gen
    (let ((n2 (/ n 2)))
      (if (>= outctr hop)
	  (begin
	    (if (> outctr n) ; must be first time through -- fill data array
		(begin
		  (do ((i 0 (+ i 1)))
		      ((= i n))
		    (vct-set! data i (readin input))))
		(begin
		  (vct-move! data 0 hop)
		  (do ((i (- n hop) (+ i 1)))
		      ((= i n))
		    (vct-set! data i (readin input)))))
	    
	    (set! outctr 0) ; -1??
	    (set! dataloc (modulo dataloc n))
	    
	    (vct-fill! new-freq-incs 0.0)
	    (do ((i 0 (+ i 1))
		 (j dataloc (+ j 1)))
		((= i n))
	      (if (= j n) (set! j 0))
	      (set! (amp-incs j) (* (fft-window i)
				    (data i))))
	    (set! dataloc (+ dataloc hop))
	    
	    (mus-fft amp-incs new-freq-incs n 1)
	    (rectangular->polar amp-incs new-freq-incs)
	    
	    (let ((scl (/ 1.0 hop))
		  (kscl (/ (* 2 pi) n)))
	      (vct-subtract! amp-incs amps)
	      (vct-scale! amp-incs scl)
	      
	      (do ((i 0 (+ i 1))
		   (ks 0.0 (+ ks kscl)))
		  ((= i n2))
		(let ((diff (modulo (- (new-freq-incs i) (freq-incs i)) (* 2 pi))))
		  (set! (freq-incs i) (new-freq-incs i))
		  (if (> diff pi) (set! diff (- diff (* 2 pi))))
		  (if (< diff (- pi)) (set! diff (+ diff (* 2 pi))))
		  (set! (new-freq-incs i) (+ (* diff scl) ks))))
	      
	      (vct-subtract! new-freq-incs freqs)
	      (vct-scale! new-freq-incs scl))))
      
      (set! outctr (+ outctr 1))
      
      (vct-add! amps amp-incs)
      (vct-add! freqs new-freq-incs)
      (vct-add! phases freqs))))


(define (test-sv)
  ;; sv-amps = pv-amps (but len is diff)
  ;; sv-phases = pv-phases
  ;; sv-freqs = pv-phase-increments
  
  (let ((pv (make-phase-vocoder (make-readin "oboe.snd") ))
	(sv (make-moving-spectrum (make-readin "oboe.snd"))))
    (do ((k 0 (+ k 1)))
	((= k 20))
      (do ((i 0 (+ i 1))) 
	  ((= i 2000)) 
	(phase-vocoder pv) 
	(moving-spectrum sv))
      (do ((i 0 (+ i 1)))
	  ((= i 256))
	(if (fneq ((sv 'amps) i) ((phase-vocoder-amps pv) i))
	    (format #t ";~D amps: ~A ~A" i ((sv 'amps) i) ((phase-vocoder-amps pv) i)))
	(if (fneq ((sv 'freqs) i) ((phase-vocoder-phase-increments pv) i))
	    (format #t ";~D freqs: ~A ~A" i ((sv 'freqs) i) ((phase-vocoder-phase-increments pv) i)))))))

#|
(define* (sine-bank amps phases size)
  (let ((len (or size (length amps)))
	(sum 0.0))
    (do ((i 0 (+ i 1)))
	((= i len))
      (set! sum (+ sum (* (amps i)
			  (sin (phases i))))))
    sum))

(with-sound (:channels 2)
  (let* ((gen (make-moving-spectrum (make-readin "oboe.snd")))
	 (pv (make-phase-vocoder (make-readin "oboe.snd")))
	 (samps (frames "oboe.snd")))
    (do ((i 0 (+ i 1)))
	((= i samps))
      (moving-spectrum gen)
      (outa i (sine-bank (gen 'amps) (gen 'phases) 256)) ; size = n/2 as in pv
      (outb i (phase-vocoder pv)))))

					; :(channel-distance 0 0 0 1)
					; 7.902601100022e-9
|#


;;; moving spectrum returns freqs in radians, and does not try to find the interpolated peak,
;;;   so we need another version that returns current freq/amp pairs that can be used directly in oscil
;;;   This is the main portion of the "pins" instrument (also find-pitch in examp.scm)




;;; ---------------- moving scentroid ----------------

(defgenerator (moving-scentroid
	       :make-wrapper (lambda (g)
			       (let ((n (g 'size)))
				 (set! (g 'rl) (make-vct n))
				 (set! (g 'im) (make-vct n))
				 (set! (g 'dly) (make-delay n))
				 (set! (g 'rms) (make-moving-rms n))
				 (set! (g 'hop) (floor (/ (mus-srate) (g 'rfreq))))
				 (set! (g 'binwidth) (/ (mus-srate) n))
				 g)))
  (dbfloor -40.0) (rfreq 100.0) 
  (size 4096) (hop 1024) (outctr 0)
  (curval 0.0) (binwidth 1.0)
  (rl #f) (im #f) 
  (dly #f) (rms #f) x)

(define* (moving-scentroid gen (x 0.0))
  (environment-set! gen 'x x)
  (with-environment gen
    
    (let ((rms (moving-rms rms x)))
      (if (>= outctr hop)
	  (begin
	    (set! outctr 0)	    
	    (if (< (linear->db rms) dbfloor)
		(set! curval 0.0)
		(let* ((data (mus-data dly))
		       (n size)
		       (fft2 (/ n 2))
		       (numsum 0.0)
		       (densum 0.0))
		  (clear-array im)
		  (vct-subseq data 0 (- n 1) rl)
		  (mus-fft rl im n 1)          ; we can use the delay line contents un-reordered because phases are ignored here
		  (rectangular->magnitudes rl im)
		  (do ((k 0 (+ k 1)))
		      ((= k fft2))
		    (set! numsum (+ numsum (* k (rl k))))
		    (set! densum (+ densum (rl k))))
		  (set! curval (/ (* binwidth numsum) densum)))))))
    (delay dly x)       ; our "sliding window" on the input data
    (set! outctr (+ outctr 1))
    curval))

#|
(let* ((snd (open-sound "oboe.snd"))
       (cur-srate (srate snd))
       (old-srate (mus-srate)))
  (set! (mus-srate) cur-srate)
  
  (let ((scn (make-moving-scentroid -40.0 100.0 128))
	(vals (scentroid "oboe.snd" 0.0 1.1 -40.0 100.0 128))
	(k 0))
    
    (let ((data (channel->vct 0 22050 snd 0)))
      (close-sound snd)
      (do ((i 0 (+ i 1)))
	  ((= i (scn 'size)))
	(moving-scentroid scn (data i)))
      (set! (scn 'outctr) (scn 'hop))
      
      (do ((i (scn 'size) (+ i 1))
	   (j 0 (+ j 1)))
	  ((= i 22050))
	(let ((val (moving-scentroid scn (data i))))
	  (if (= (modulo j (scn 'hop)) 0)
	      (begin
		(format #t "[~A ~A]~%" val (vals k))
		(set! k (+ k 1)))))))
    (set! (mus-srate) old-srate)))
|#



;;; ---------------- moving-autocorrelation ----------------

(defgenerator (moving-autocorrelation
	       :make-wrapper (lambda (g)
			       (let ((n (g 'n)))
				 (set! (g 'rl) (make-vct n))
				 (set! (g 'im) (make-vct n))
				 (set! (g 'data) (make-vct n))
				 (set! (g 'outctr) (+ n 1)) ; first time fill flag
				 g))
	       :methods (list
			 (cons 'mus-run (lambda (g arg1 arg2) (moving-autocorrelation g)))
			 (cons 'mus-data (lambda (g) (g 'rl)))))
  (input #f) (n 512) (hop 128) (outctr 0)
  (rl #f) (im #f) (data #f))


(define (moving-autocorrelation gen)
;;  "(make-moving-autocorrelation reader (size 512) (hop 128)) returns a moving-autocorrelation generator. \n\
;; (moving-autocorrelation gen) produces the autocorrelation of 'size' samples every 'hop' samples, \n\
;; taking input from the readin generator 'reader'.  The output data is available via mus-data."
  
  (with-environment gen
    (let ((new-data #f))
      (if (>= outctr hop)
	  (begin
	    (if (> outctr n) ; must be first time through -- fill data array
		(begin
		  (do ((i 0 (+ i 1)))
		      ((= i n))
		    (vct-set! data i (readin input))))
		(begin
		  (vct-move! data 0 hop)
		  (do ((i (- n hop) (+ i 1)))
		      ((= i n))
		    (vct-set! data i (readin input)))))
	    (set! outctr 0)
	    (set! new-data #t)
	    (clear-array im)
	    (vct-subseq data 0 (- n 1) rl)
	    (autocorrelate rl)))
      (set! outctr (+ outctr 1))
      new-data)))




;;; ---------------- moving-pitch ----------------

(defgenerator (moving-pitch
	       :make-wrapper (lambda (g)
			       (set! (g 'ac) (make-moving-autocorrelation
					      (g 'input)
					      (g 'n)
					      (g 'hop)))
			       g)
	       :methods (list
			 (cons 'mus-run (lambda (g arg1 arg2) (moving-pitch g)))))
  
  (input #f) (n 512) (hop 128)
  (ac #f) (val 0.0))


(define (moving-pitch gen)
  (with-environment gen
    (if (moving-autocorrelation ac)
	(let* ((data (mus-data ac))
	       (peak 0.0)
	       (peak-loc 0)
	       (len (length data)))
	  (do ((i 8 (+ i 1))) ; assume we're not in the top few octaves
	      ((= i len))
	    (let ((apk (abs (data i))))
	      (if (> apk peak)
		  (begin
		    (set! peak apk)
		    (set! peak-loc i)))))
	  (if (or (= peak 0.0)
		  (= peak-loc 0))
	      (set! val 0.0)
	      (let* ((la (data (- peak-loc 1)))
		     (ra (data (+ peak-loc 1)))
		     (logla (log (/ (max la .0000001) peak) 10))
		     (logra (log (/ (max ra .0000001) peak) 10)))
		(set! val
		      (/ (mus-srate)
			 (+ peak-loc (/ (* 0.5 (- logla logra))
					(+ logla logra)))))))))
    val))

#|
(let* ((rd (make-readin "oboe.snd"))
       (cur-srate (srate "oboe.snd"))
       (old-srate (mus-srate)))
  (set! (mus-srate) cur-srate)
  (let* ((scn (make-moving-pitch rd))
	 (last-pitch 0.0)
	 (pitch 0.0))
    (do ((i 0 (+ i 1)))
	((= i 22050))
      (set! last-pitch pitch)
      (set! pitch (moving-pitch scn))
      (if (not (= last-pitch pitch))
	  (format #t "~A: ~A~%" (* 1.0 (/ i cur-srate)) pitch))))
  (set! (mus-srate) old-srate))
|#



#|
(define (abel k)
  ;; sum i from 1 to k (-1)^(i + 1) * (sin i) / i
  (with-sound (:clipped #f :statistics #t) 
    (let ((harmonics (make-vct (* 2 k))))
      (do ((i 1 (+ i 1))
	   (j 0 (+ j 2))
	   (n -1 (- n))) 
	  ((= i k)) 
	(set! (harmonics j) i)
	(set! (harmonics (+ j 1)) (/ n i)))
      (let ((gen (make-polywave 100.0 :partials (normalize-partials harmonics))))
	(do ((i 0 (+ i 1)))
	    ((= i 100000))
	  (outa i (polywave gen)))))))

(define* (adds num freq e amp v (type mus-chebyshev-first-kind))
  (with-sound (:clipped #f :statistics #t :play #t) 
    (let ((harmonics (make-vct (* 2 num)))
	  (freqe (make-env e :length num)))
      (do ((i 1 (+ i 1))
	   (j 0 (+ j 2)))
	  ((= i num)) 
	(set! (harmonics j) i)
	(set! (harmonics (+ j 1)) (env freqe)))
      (let ((gen (make-polywave freq :partials (normalize-partials harmonics) :type type))
	    (vib (make-oscil 5)))
	(do ((i 0 (+ i 1)))
	    ((= i 100000))
	  (outa i (* amp (polywave gen (* (hz->radians v) (oscil vib))))))))))


					;(adds 200 20 '(0 0 10 1 12 0 20 0 24 .2 35 0 46 0 57 .1 68 0) .5 2)
					;(adds 300 15 '(0 0 10 1 12 0 20 0 24 .2 35 0 46 0 57 .1 68 0) .5 3)

|#


#|
(defgenerator (circler
	       :make-wrapper (lambda (g)
			       (set! (g 'frequency) (hz->radians (g 'frequency)))
			       g))
  (frequency *clm-default-frequency*) (angle 0.0) fm)

(define* (circler gen (fm 0.0))
;;  "(make-circler (frequency 0.0) returns a circler generator.\n\
;;   (circler gen (fm 0.0)) produces a waveform made up of half circles"
  
  (environment-set! gen 'fm fm)
  (with-environment gen
    (let* ((x (modulo angle (* 2 pi)))
	   (xx (/ (* 4 x) (* 2 pi)))
	   (y (if (< xx 2)
		  (sqrt (- 1 (* (- 1 xx) (- 1 xx))))
		  (- (sqrt (- 1 (* (- 3 xx) (- 3 xx))))))))
      (set! angle (+ x fm frequency))
      y)))

(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-circler 10.0)))
    (do ((i 0 (+ i 1)))
	((= i 20000))
      (outa i (circler gen)))))

;;; odd harmonics: 1, .18 .081 .048 .033 .024, .019
|#


#|
;; "extremal trigonometric polynomials"

(define (extremal-trig N freq)
  (with-sound ()
    (let ((tan-scl (tan (/ pi (* 2 (+ N 1)))))
	  (incr (hz->radians freq)))
      (do ((k 1 (+ k 1)))
	  ((= k N))
	(let ((cos-coeff (* tan-scl (sin (/ (* k pi) (+ N 1)))))
	      (kincr (* k incr)))
	  (do ((i 0 (+ i 1))
	       (x 0.0 (+ x kincr)))
	      ((= i 40000))
	    (outa i (* cos-coeff (cos x)))))))))
|#



;;; ---------------- flocsig (flanged locsig) ----------------

(defgenerator (flocsig 
	       ;; assume stereo out/rev 
	       :make-wrapper (lambda (g)
			       (set! (g 'maxd) (ceiling (g 'amplitude))) ; was amplitude?
			       (set! (g 'out1) (make-vct (g 'maxd)))
			       (set! (g 'out2) (make-vct (g 'maxd)))
			       (set! (g 'ri) (make-rand-interp 
					      :frequency (g 'frequency) 
					      :amplitude (- (g 'amplitude) 1.0)))
			       (if (not (g 'offset))
				   (set! (g 'offset) (mus-random (* 0.3 (g 'amplitude)))))
			       g))
  (reverb-amount 0.0) (frequency 1.0) (amplitude 2.0) offset
  (maxd 0)
  (out1 #f) (out2 #f) (outloc 0)
  (ri #f) samp input)


(define (flocsig gen samp input)
  ;; signal position and per-channel-delay depends on rand-interp
  (environment-set! gen 'samp samp)
  (environment-set! gen 'input input)
  (with-environment gen
    (let* ((rpos (rand-interp ri))
	   (pos (min (max (+ rpos offset) (- amplitude)) amplitude))
	   (amp1 (if (<= pos -1.0) 1.0
		     (if (>= pos 1.0) 0.0
			 (/ (sqrt (- 1.0 pos)) (sqrt 2.0)))))
	   (amp2 (if (<= pos -1.0) 0.0
		     (if (>= pos 1.0) 1.0
			 (/ (sqrt (+ 1.0 pos)) (sqrt 2.0)))))
	   (dly1 (abs (min 0.0 pos)))
	   (frac1 (- dly1 (floor dly1)))
	   (dly2 (max 0.0 pos))
	   (frac2 (- dly2 (floor dly2)))
	   (loc outloc)
	   (size maxd)
	   (loc10 (modulo (+ loc (floor dly1)) size))
	   (loc11 (modulo (+ loc 1 (floor dly1)) size))
	   (loc20 (modulo (+ loc (floor dly2)) size))
	   (loc21 (modulo (+ loc 1 (floor dly2)) size))
	   (rev reverb-amount))
      
      (set! (out1 loc10) (+ (out1 loc10) (* amp1 input (- 1.0 frac1))))
      (set! (out1 loc11) (+ (out1 loc11) (* amp1 input frac1)))
      (set! (out2 loc20) (+ (out2 loc20) (* amp2 input (- 1.0 frac2))))
      (set! (out2 loc21) (+ (out2 loc21) (* amp2 input frac2)))
      
      (let ((val1 (out1 loc))
	    (val2 (out2 loc)))
	(set! (out1 loc) 0.0)
	(set! (out2 loc) 0.0)
	(set! loc (+ loc 1))
	(if (= loc size) (set! loc 0))
	(outa samp val1)
	(outb samp val2)
	(if (> rev 0.0)
	    (begin
	      (outa samp (* rev val1) *reverb*)
	      (outb samp (* rev val2) *reverb*)))
	(set! outloc loc)))))





;;; --------------------------------------------------------------------------------
;;; old version of one-pole-all-pass
#|
(defgenerator one-pole-allpass coeff input x1 y1)

(define (one-pole-allpass gen input)
  (environment-set! gen 'input input)
  (with-environment gen
    (set! y1 (+ x1 (* coeff (- input y1))))
    (set! x1 input)
    y1))

(defgenerator one-pole-allpass-bank coeff input x1 y1 x2 y2 x3 y3 x4 y4 x5 y5 x6 y6 x7 y7 x8 y8) 

(define (one-pole-allpass-bank gen input)
  (environment-set! gen 'input input)
  (with-environment gen
    (set! y1 (+ x1 (* coeff (- input y1))))
    (set! x1 input)

    (set! y2 (+ x2 (* coeff (- y1 y2))))
    (set! x2 y1)

    (set! y3 (+ x3 (* coeff (- y2 y3))))
    (set! x3 y2)

    (set! y4 (+ x4 (* coeff (- y3 y4))))
    (set! x4 y3)

    (set! y5 (+ x5 (* coeff (- y4 y5))))
    (set! x5 y4)

    (set! y6 (+ x6 (* coeff (- y5 y6))))
    (set! x6 y5)

    (set! y7 (+ x7 (* coeff (- y6 y7))))
    (set! x7 y6)

    (set! y8 (+ x8 (* coeff (- y7 y8))))
    (set! x8 y7)
    y8))


(defgenerator expseg currentValue targetValue r)

(define (expseg gen r)
  (environment-set! gen 'r r)
  (with-environment gen
    (set! currentValue (+ (* r targetValue) (* (- 1.0 r) currentValue)))))
    ;(set! currentValue (+ currentValue (* r (- targetValue currentValue))))))
    ;; (bil) this is slightly different (getting clicks)


(define (make-one-pole-swept)
  (vector 0.0))

(define (one-pole-swept gen input coef)
  ;; signal controlled one-pole lowpass filter
  (set! (gen 0) (- (* (+ 1.0 coef) input) (* coef (gen 0)))))

(define (make-pnoise)
  (vector 16383))

(define (pnoise gen x)
  ;; very special noise generator
  (set! (gen 0) (logand (floor (+ (* (gen 0) 1103515245) 12345)) #xffffffff))
  ;; (bil) added the logand -- otherwise we get an overflow somewhere
  (* x (- (* (modulo (floor (/ (gen 0) 65536.0)) 65536) 0.0000305185) 1.0)))
  ;; this looks nutty to me -- was it originally running in 32 bits?


(define pn-gen 16383)
(define (pnoise x)
  ;; very special noise generator
  (set! pn-gen (logand (+ (* pn-gen 1103515245) 12345) #xffffffff))
  ;; (bil) added the logand -- otherwise we get an overflow somewhere, also removed floor
  (* x (- (* pn-gen 4.6566128730774e-10) 1.0)))

|#
                              



;;; --------------------------------------------------------------------------------

(define (calling-all-generators)
  ;; for snd-test
  (with-sound (:play #f)
    (lutish 0 1 440 .1)
    (oboish 1 1 300 .1 '(0 0 1 1 2 0))
    (nkssber 2 1 1000 100 5 5 0.5)
    (stringy 3 1 1000 .5)
    (ercoser 4 1 100 .5 0.1)
    (bouncy 5 2 300 .5 5 10)
    (pianoy 6 3 100 .5)
    (pianoy1 7 4 200 .5 1 .1)
    (pianoy2 8 1 100 .5)
    (glassy 9 .1 1000 .5)
    (machine1 10 .3 100 540 0.5 3.0 0.0)
    (organish 11 .4 100 .5 1.0 #f)
    (brassy 12 4 50 .5 '(0 0 1 1 10 1 11 0) '(0 1 1 0) 1000)))


