;;; maxf.scm -- CLM -> Snd/Scheme translation of maxf.ins

;; Translator/Author: Michael Scholz <scholz-micha@gmx.de>
;; Last: Tue Mar 25 04:32:23 CET 2003
;; Version: $Revision: 1.2 $

;; array -> vector functions added by Bill S, 18-Apr-11
;; defgenerator changes (Bill 25-Jul-12)

;; It follows the original header by Juan Reyes.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  maxf.ins
;;  This is Max Mathews (mvm) new filter (2002)
;;  High-Q, 2-Integrator, filter with
;;  Two Poles, and one Zero at the Origin
;;
;; It synthesizes equal-tempered frequencies
;; integer & just scales out of a wide-band input
;; signal.
;; Based on  Max's code (filter.cpp)
;;
;;  This heuristic might be called Modal Synthesis.
;;  But as well it can also be  additive synthesis in
;;  which a resonator is  initialized to generate the
;;  exponentially decaying sinusoids at the desired
;;  phase.
;;
;;   This implementation written by Juan Reyes with dsp 
;;   assistance from JOS.
;;   This  version Oct-30, 2002
;;
;;   Change gain(att) of input file if clipping
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'snd-maxf.scm)
(if (provided? 'snd)
    (if (not (provided? 'snd-ws.scm)) (load "ws.scm"))
    (if (not (provided? 'sndlib-ws.scm)) (load "sndlib-ws.scm")))
;;; (if (not (provided? 'snd-jcrev.scm)) (load "jcrev.scm"))

(define *locsig-type* mus-interp-sinusoidal)

(define (snd-msg frm . args)
  (let ((str (apply format (append (list #f frm) args))))
    (if (getenv "EMACS")
	(display str)
	(snd-print str))))

(defgenerator mvm sample pp1 pp2 pp3 yy1 yy2 zz1 zz2 out)

(define (mvmfilt b sample0)
  (environment-set! b 'sample sample0)
  (with-environment b
    (set! yy2 (- (+ (* pp1 yy1)
		    (* pp2 zz1))
		 (* pp3 sample)))
    (set! zz2 (- zz1 (* pp2 yy2)))
    (set! zz1 zz2)
    (set! yy1 yy2)
    (set! out yy1)))

(define pi2s (/ (* 2.0 pi) (mus-srate)))
(define i2s (/ 2.0 (mus-srate)))
(define tper (/ 1.0 (mus-srate)))

(define (set-coeffs b famp ffreq fdecay)
  (let ((centerfreq (* ffreq pi2s)))
    (let ((maxdecay (/ (* 2.0 tper) (* centerfreq centerfreq)))
	  (mindecay (/ tper centerfreq)))
      ;; Conditions for JOS constraints
      ;; maxdecay: Filter may be unstable
      ;; mindecay: Filter may not oscillate
      (if (>= fdecay maxdecay)
	  (set! fdecay maxdecay)
	  (if (<= fdecay mindecay)
	      (set! fdecay mindecay)))
      (set! (b 'pp1) (- 1.0 (/ i2s fdecay)))
      (set! (b 'pp2) (* ffreq pi2s))
      (set! (b 'pp3) (* (b 'pp2) famp)))))

(define (make-array initial-value dim1 dim2) ; I'm guessing ...
  (make-vector (list dim1 dim2) initial-value))

(define (array-set! arr val i1 i2)
  (set! (arr i1 i2) val))

(define array-ref vector-ref)

(define* (maxfilter file beg
		    (att 1.0)
		    (numf 1)
		    (freqfactor 1.0)
		    (amplitude 1.0)
		    (amp-env '(0 1 100 1))
		    (degree (random 90.0))
		    (distance 1.0)
		    (reverb-amount 0.2))
  "(maxfilter file beg (att 1.0) (numf 1) (freqfactor 1.0)
            (amplitude 1.0) (amp-env '(0 1 100 1))
            (degree (random 90.0)) (distance 1.0) (reverb-amount 0.2))

This is Max Mathews (mvm) new filter (2002) High-Q, 2-Integrator,
filter with Two Poles, and one Zero at the Origin

It synthesizes equal-tempered frequencies integer & just scales
out of a wide-band input signal.
Based on Max's code (filter.cpp)

This heuristic might be called Modal Synthesis.  But as well it
can also be additive synthesis in which a resonator is
initialized to generate the exponentially decaying sinusoids at
the desired phase.

   (att 1)   in-file attenuation
   (numf  1)   1 filter
   (numf  4)   4 filters
   (numf  9)   9 filters
   (numf 12)  12 filters
   (numf 13)  13 filters"
  (let ((beg (floor (* beg (mus-srate))))
	(dur (mus-sound-frames file)))
    (let ((formfil (make-mvm))
	  (end (+ beg dur))
	  (rdA (make-readin :file file :channel 0))
	  (ampf (make-env :envelope amp-env :scaler amplitude :length dur))
	  (state-0 (make-array 0.0  1 3))
	  (state-1 (make-array 0.0 12 3))
	  (state-2 (make-array 0.0  9 3))
	  (state-3 (make-array 0.0 13 3))
	  (state-4 (make-array 0.0  4 3))
	  (state-5 (make-array 0.0  2 3))
	  (loc (make-locsig :degree degree 
			    :distance distance
			    :reverb reverb-amount
			    :type *locsig-type*)))
      (cond ((= numf 1)
	     (snd-msg ";;;; State 0 (default): One filter~%")
	     (array-set! state-0 7.54e-002 0 0)
	     (array-set! state-0 (* 2000 freqfactor) 0 1)
	     (array-set! state-0 2.0 0 2))
	    ;;
	    ((= numf 2)
	     (snd-msg ";;;; State 5: Two filters~%")
	     (array-set! state-5 7.54e-003 0 0)
	     (array-set! state-5 (* 200.0 freqfactor) 0 1)
	     (array-set! state-5 4.0 0 2)
	     ;;
	     (array-set! state-5 7.54e-004 1 0)
	     (array-set! state-5 (* 800.0 freqfactor) 1 1)
	     (array-set! state-5 1.0 1 2))
	    ;;
	    ((= numf 4)
	     (snd-msg ";;;; State 4: Four filters~%")
	     (array-set! state-4 7.54e-002 0 0)
	     (array-set! state-4 (* 1000.0 freqfactor) 0 1)
	     (array-set! state-4 0.5 0 2)
	     ;;
	     (array-set! state-4 3.225e-002 1 0)
	     (array-set! state-4 (* 400.0 freqfactor) 1 1)
	     (array-set! state-4 3.0 1 2)
	     ;;
	     (array-set! state-4 1.14e-002 2 0)
	     (array-set! state-4 (* 800.0 freqfactor) 2 1)
	     (array-set! state-4 2.8 2 2)
	     ;;
	     (array-set! state-4 7.54e-002 3 0)
	     (array-set! state-4 (* 1600.0 freqfactor) 3 1)
	     (array-set! state-4 1.0 3 2))
	    ;;
	    ((= numf 9)
	     (snd-msg ";;;; State 2: Streached overtone string  9 filters~%")
	     (array-set! state-2 1.07e-002 0 0)
	     (array-set! state-2 100.0 0 1)
	     (array-set! state-2 2.5 0 2)
	     ;;
	     (array-set! state-2 1.07e-002 1 0)
	     (array-set! state-2 202.0 1 1)
	     (array-set! state-2 0.75 1 2)
	     ;;
	     (array-set! state-2 1.07e-002 2 0)
	     (array-set! state-2 305.0 2 1)
	     (array-set! state-2 0.5 2 2)
	     ;;
	     (array-set! state-2 7.077e-003 3 0)
	     (array-set! state-2 408.0 3 1)
	     (array-set! state-2 0.4 3 2)
	     ;;
	     (array-set! state-2 1.07e-002 4 0)
	     (array-set! state-2 501.0 4 1)
	     (array-set! state-2 0.3 4 2)
	     ;;
	     (array-set! state-2 1.07e-002 5 0)
	     (array-set! state-2 612.0 5 1)
	     (array-set! state-2 0.25 5 2)
	     ;;
	     (array-set! state-2 1.07e-003 6 0)
	     (array-set! state-2 715.0 6 1)
	     (array-set! state-2 0.25 6 2)
	     ;;
	     (array-set! state-2 1.07e-002 7 0)
	     (array-set! state-2 817.0 7 1)
	     (array-set! state-2 0.2 7 2)
	     ;;
	     (array-set! state-2 1.07e-002 8 0)
	     (array-set! state-2 920.0 8 1)
	     (array-set! state-2 0.18 8 2))
	    ;;
	    ((= numf 12)
	     (snd-msg ";;;; State 1: Risset bell long  12 filters~%")
	     (array-set! state-1 5.025e-002 0 0)
	     (array-set! state-1 224.0 0 1)
	     (array-set! state-1 3.7 0 2)
	     ;;
	     (array-set! state-1 5.025e-002 1 0)
	     (array-set! state-1 225.0 1 1)
	     (array-set! state-1 3.3 1 2)
	     ;;
	     (array-set! state-1 5.025e-002 2 0)
	     (array-set! state-1 368.0 2 1)
	     (array-set! state-1 2.8 2 2)
	     ;;
	     (array-set! state-1 5.025e-002 3 0)
	     (array-set! state-1 369.0 3 1)
	     (array-set! state-1 2.4 3 2)
	     ;;
	     (array-set! state-1 1.047e-002 4 0)
	     (array-set! state-1 476.0 4 1)
	     (array-set! state-1 1.9 4 2)
	     ;;
	     (array-set! state-1 5.025e-002 5 0)
	     (array-set! state-1 680.0 5 1)
	     (array-set! state-1 1.7 5 2)
	     ;;
	     (array-set! state-1 5.025e-002 6 0)
	     (array-set! state-1 800.0 6 1)
	     (array-set! state-1 1.5 6 2)
	     ;;
	     (array-set! state-1 4.05e-002 7 0)
	     (array-set! state-1 1096.0 7 1)
	     (array-set! state-1 1.1 7 2)
	     ;;
	     (array-set! state-1 4.05e-002 8 0)
	     (array-set! state-1 1099.0 8 1)
	     (array-set! state-1 0.9 8 2)
	     ;;
	     (array-set! state-1 4.05e-002 9 0)
	     (array-set! state-1 1200.0 9 1)
	     (array-set! state-1 0.6 9 2)
	     ;;
	     (array-set! state-1 3.78e-002 10 0)
	     (array-set! state-1 1504.0 10 1)
	     (array-set! state-1 0.4 10 2)
	     ;;
	     (array-set! state-1 4.05e-002 11 0)
	     (array-set! state-1 1628.0 11 1)
	     (array-set! state-1 0.3 11 2))
	    ;;
	    ((= numf 13)
	     (snd-msg ";;;; State 3: Open major chord with repeated octave  12 filters~%")
	     (array-set! state-3 5.025e-002 0 0)
	     (array-set! state-3 100.0 0 1)
	     (array-set! state-3 2.0 0 2)
	     ;;
	     (array-set! state-3 5.025e-002 1 0)
	     (array-set! state-3 251.0 1 1)
	     (array-set! state-3 2.0 1 2)
	     ;;
	     (array-set! state-3 5.025e-002 2 0)
	     (array-set! state-3 299.0 2 1)
	     (array-set! state-3 2.0 2 2)
	     ;;
	     (array-set! state-3 5.025e-002 3 0)
	     (array-set! state-3 401.0 3 1)
	     (array-set! state-3 2.0 3 2)
	     ;;
	     (array-set! state-3 5.025e-002 4 0)
	     (array-set! state-3 199.0 4 1)
	     (array-set! state-3 2.0 4 2)
	     ;;
	     (array-set! state-3 5.025e-002 5 0)
	     (array-set! state-3 501.0 5 1)
	     (array-set! state-3 2.0 5 2)
	     ;;
	     (array-set! state-3 5.025e-002 6 0)
	     (array-set! state-3 599.0 6 1)
	     (array-set! state-3 2.0 6 2)
	     ;;
	     (array-set! state-3 5.025e-002 7 0)
	     (array-set! state-3 801.0 7 1)
	     (array-set! state-3 2.0 7 2)
	     ;;
	     (array-set! state-3 5.025e-002 8 0)
	     (array-set! state-3 201.0 8 1)
	     (array-set! state-3 2.0 8 2)
	     ;;
	     (array-set! state-3 5.025e-002 9 0)
	     (array-set! state-3 749.0 9 1)
	     (array-set! state-3 2.0 9 2)
	     ;;
	     (array-set! state-3 5.025e-002 10 0)
	     (array-set! state-3 900.0 10 1)
	     (array-set! state-3 2.0 10 2)
	     ;;
	     (array-set! state-3 5.025e-004 11 0)
	     (array-set! state-3 1205.0 11 1)
	     (array-set! state-3 2.0 11 2)
	     ;;
	     (array-set! state-3 5.025e-004 12 0)
	     (array-set! state-3 1205.0 12 1)
	     (array-set! state-3 2.0 12 2))
	    (t
	     (snd-msg "Please leave default or enter [1] [2] [4] [9] [12] [13]~%")
	     (set! numf 1)))

      (let ((run-state (case numf
			 ((1) state-0)
			 ((2) state-5)
			 ((4) state-4)
			 ((9) state-2)
			 ((12) state-1)
			 ((13) state-3))))

	(do ((i beg (+ 1 i)))
	    ((= i end))
	  (let ((outvalA (* att (readin rdA)))
		(add-fl 0.0))
	    (do ((j 0 (+ 1 j)))
		((= j numf))
	      (set-coeffs formfil (array-ref run-state j 0) (array-ref run-state j 1) (array-ref run-state j 2))
	      (set! add-fl (+ add-fl (mvmfilt formfil outvalA))))
	    (locsig loc i (* (env ampf) add-fl))))))))

;; (let* ((ifile "dog.snd")
;;        (ofile "gmax_dog.snd")
;;        (snd (find-sound ofile))
;;        (number-ary '(1 2 4 9 12 13)))
;;   (if snd
;;       (close-sound snd))
;;   (with-sound (:play 1 :statistics #t :channels 4 :output ofile :reverb jc-reverb
;; 			   :comment
;; 			   (format #f "maxfilter test, filters ~S, source ~A" number-ary ifile))
;; 	      (do ((i 0 (+ 1 i))
;; 		   (nary number-ary (cdr nary)))
;; 		  ((null? nary))
;; 		(maxfilter ifile i :numf (car nary) :degree (random 3454)))))

;; (with-sound () (maxfilter "dog.snd" 0))
;; (with-sound (:srate 44100) (maxfilter "dog.snd" 0 :numf 12))
;; (with-sound (:srate 44100) (maxfilter "dog.snd" 0 :numf 13 :att 0.75))
;; (with-sound (:srate 44100) (maxfilter "dog.snd" 0 :numf 2 :att 0.25 :freqfactor 0.5))

;; maxf.scm ends here
