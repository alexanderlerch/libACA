;;; agn.scm -- Bill Schottstaedt's agn.cl
;;;    (see clm-2/clm-example.clm and clm-2/bess5.cl)

;; Translator/Author: Michael Scholz <mi-scholz@users.sourceforge.net>
;; Created: Tue Jun 24 19:05:06 CEST 2003
;; Changed: Sat Jul 28 00:33:36 CEST 2012

;; Try (do-agn)

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
  (load "v.scm"))

(if (not (provided? 'snd-ws.scm)) (load "ws.scm"))
(if (not (provided? 'snd-env.scm)) (load "env.scm"))

(define *clm-play* #t)
(define *clm-statistics* #t)
(define *clm-verbose* #t)
(define *clm-srate* 44100)
(define *clm-channels* 2)
(define *clm-reverb* jc-reverb)
(define *clm-reverb-data* '(:volume 0.8))
(define *clm-reverb-channels* 2)
(define *clm-delete-reverb* #t)

(define (snd-msg frm . args)
  (snd-print (apply format (append (list #f frm) args))))

(define (main args)
  (do-agn (if (= 2 (length args)) (cadr args) "agn.clm")))

(define* (do-agn (file "agn.clm"))
  (let ((sndfile (format #f "~A.snd"  "agn")))
    (snd-msg ";; Writing ~S~%" file)
    (agn file)
    (with-sound (:output sndfile)
		(snd-msg ";; Loading ~S~%" file)
		(load file))))

(define lim 256)
(define time 60)
(define mode (list->vector '(0 0 2 4 11 11 5 6 7 0 0 0 0)))
(define rats (list->vector '(1.0 256/243 9/8 32/27 81/64 4/3 1024/729
                             3/2 128/81 27/16 16/9 243/128 2.0)))

(define bell '(0 0 10 0.25 90 1.0 100 1.0))

(define octs (make-vector (1+ lim)))
(define pits (make-vector (1+ lim)))
(define rhys (make-vector (1+ lim)))
(define amps (make-vector (1+ lim)))

(define (tune x)
  (let* ((pit (modulo x 12))
	 (oct (inexact->exact (floor (/ x 12))))
	 (base (vector-ref rats pit)))
    (* base (expt 2 oct))))

(define (rbell x)
  (envelope-interp (* x 100) bell))

(define* (glog r b)
  (if (<= r 0) (error "r must be > 0"))
  (if (and b (<= b 0)) (error "b must be > 0"))
  (if b (/ (log r) (log b)) (log r)))

(define (agn file)
  (let ((wins (list->vector '((0 0 40 0.1 60 0.2 75 0.4 82 1 90 1 100 0)
			      (0 0 60 0.1 80 0.2 90 0.4 95 1 100 0)
			      (0 0 10 1 16 0 32 0.1 50 1 56 0 60 0 90 0.3 100 0)
			      (0 0 30 1 56 0 60 0 90 0.3 100 0)
			      (0 0 50 1 80 0.3 100 0)
			      (0 0 40 0.1 60 0.2 75 0.4 82 1 90 1 100 0)
			      (0 0 40 0.1 60 0.2 75 0.4 82 1 90 1 100 0)
			      (0 0 10 1 32 0.1 50 1 90 0.3 100 0)
			      (0 0 60 0.1 80 0.3 95 1 100 0)
			      (0 0 80 0.1 90 1 100 0)))))
    (do ((i 0 (1+ i)))
	((= i (+ lim 1)))
      (vector-set! octs i
	(inexact->exact (floor (+ 4 (* 2 (rbell (random 1.0)))))))
      (vector-set! pits i
	(vector-ref mode (inexact->exact (floor (* 12 (random 1.0))))))
      (vector-set! rhys i
	(inexact->exact (floor (+ 4 (* 6 (random 1.0))))))
      (vector-set! amps i
	(inexact->exact (floor (+ 1 (* 8 (rbell (random 1.0))))))))
    (call-with-output-file file
      (lambda (out-port)
	(format out-port
	  ";; from agn.cl (see clm-2/clm-example.clm and clm-2/bess5.cl)~%")
	(do ((i 1 (1+ i)))
	    ((> i 3))
	  (let ((cellbeg 0)
		(cellsiz 4)
		(cellctr 0)
		(whichway 1)
		(base i)
		(mi (- i 1))
		(winnum 0)
		(mytempo 0.2)
		(nextbeg 0.0)
		(revamt 0.0)
		(ranamt 0.0)
		(beg 0.0)
		(dur 0.0)
		(freq 0.0)
		(ampl 0.0)
		(ind 0.0))
	    (while (and (< beg time) (< cellctr lim))
		   (set! beg (+ beg nextbeg))
		   (set! nextbeg (max 0.25
		     (* mytempo (+ 0.9 (* 0.2 (random 0.1)))
		     (vector-ref rhys cellctr))))
		   (set! freq (* (/ 16.352 (expt 2 mi))
		     (tune (vector-ref pits cellctr))
		     (expt 2 (vector-ref octs cellctr))))
		   (set! dur nextbeg)
		   (if (< freq 100) (set! dur (+ dur dur)))
		   (set! ampl (max 0.003
		     (* (vector-ref amps cellctr) (/ (* 60 base)))))
		   (set! ind (* (random 1.0) 2 base))
		   (set! cellctr (1+ cellctr))
		   (set! revamt (* base 0.1))
		   (set! winnum (inexact->exact
		     (floor (* 10 (- beg (floor beg))))))
		   (set! ranamt (* 0.00001 (expt (- (glog freq 2.0) 4) 4)))
		   (format out-port
		     "
(fm-violin ~F ~F ~F ~F :fm-index ~F
  :amp-env '~S
  :reverb-amount ~F :noise-amount ~F)"
		     beg dur freq ampl ind 
		     (vector-ref wins winnum) revamt ranamt)
		   (set! cellctr (1+ cellctr))
		   (if (> cellctr (+ cellsiz cellbeg))
		       (begin
			 (set! cellbeg (1+ cellbeg))
			 (if (> (random 1.0) 0.5)
			     (set! cellsiz (+ cellsiz whichway)))
			 (if (and (> cellsiz 16) (> (random 1.0) 0.99))
			     (begin
			       (set! whichway -2)
			       (if (and (> cellsiz 12) (> (random 1.0) 0.999))
				   (begin
				     (set! whichway -1)
				     (if (< cellsiz 4)
					 (set! whichway 1))))))
			 (set! cellbeg (+ cellbeg 3))
			 (set! cellctr cellbeg))))))
	(format out-port "~%~%;; ~A ends here~%" file))))
  file)

;; agn.scm ends here
