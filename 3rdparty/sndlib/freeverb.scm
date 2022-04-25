;;; freeverb.scm -- CLM -> Snd/Scheme translation of freeverb.ins

;; Translator/Author: Michael Scholz <scholz-micha@gmx.de>
;; Last: Thu Apr 24 01:32:15 CEST 2003
;; Version: $Revision: 1.2 $

;;; Original notes of Fernando Lopez-Lezcano

;; Freeverb - Free, studio-quality reverb SOURCE CODE in the public domain
;;
;; Written by Jezar at Dreampoint, June 2000
;; http://www.dreampoint.co.uk
;;
;; Translated into clm-2 by Fernando Lopez-Lezcano <nando@ccrma.stanford.edu>
;; Version 1.0 for clm-2 released in January 2001
;; http://ccrma.stanford.edu/~nando/clm/freeverb/
;;
;; Changes to the original code by Jezar (by Fernando Lopez-Lezcano):
;; - the clm version can now work with a mono input or an n-channel input
;;   stream (in the latter case the number of channels of the input and output
;;   streams must match.
;; - the "wet" parameter has been eliminated as it does not apply to the model
;;   that clm uses to generate reverberation
;; - the "width" parameter name has been changed to :global. It now controls the
;;   coefficients of an NxN matrix that specifies how the output of the reverbs
;;   is mixed into the output stream.
;; - predelays for the input channels have been added.
;; - damping can be controlled individually for each channel.

;; For more information see clm-2/freeverb/index.html [MS]

;;; changed to accommodate run and mono output, bill 11-Jun-06
;;;            use the filtered-comb gen, bill 29-Jun-06
;;; optimized slightly, bill 17-Sep-12

;;; Code:

(provide 'snd-freeverb.scm)
(if (provided? 'snd)
    (if (not (provided? 'snd-ws.scm)) (load "ws.scm"))
    (if (not (provided? 'sndlib-ws.scm)) (load "sndlib-ws.scm")))

(definstrument (freeverb
		   (room-decay 0.5)
		   (damping 0.5)
		   (global 0.3)
		   (predelay 0.03)
		   (output-gain 1.0)
		   (output-mixer #f)
		   (scale-room-decay 0.28)
		   (offset-room-decay 0.7)
		   (combtuning '(1116 1188 1277 1356 1422 1491 1557 1617))
		   (allpasstuning '(556 441 341 225))
		   (scale-damping 0.4)
		   (stereo-spread 23)
		   (verbose #f))
  (let ((startime 0.0)
	(dur (+ 1.0 (mus-sound-duration (mus-file-name *reverb*))))
	 (out-chans (channels *output*))
	 (in-chans (channels *reverb*))
	 (srate-scale (/ *clm-srate* 44100.0))
	 (room-decay-val (+ (* room-decay scale-room-decay) offset-room-decay))
	 (numcombs (length combtuning))
	 (numallpasses (length allpasstuning)))
    (let ((beg (seconds->samples startime))
	  (end (cadr (times->samples startime dur)))
	  (out-mix (if (mixer? output-mixer) output-mixer
		       (make-mixer out-chans 0.0)))
	  (out-buf (make-frame out-chans 0.0))
	  (f-out (make-frame out-chans 0.0))
	  (f-in (make-frame in-chans 0.0))
	  (predelays (make-vector in-chans))
	  (fcombs (make-vector (* out-chans numcombs)))
	  (allpasses (make-vector (* out-chans numallpasses)))
	  (local-gain (if (= out-chans 1)
			  global
			  (+ (/ (- 1.0 global) (- 1 (/ 1.0 out-chans)))
			     (/ 1.0 out-chans))))
	  (global-gain 0.0))
      (set! global-gain (if (= out-chans 1)
			    local-gain
			    (/ (- out-chans (* local-gain out-chans))
			       (- (* out-chans out-chans) out-chans))))
      (if verbose
	  (format #t ";;; freeverb: ~d input channels, ~d output channels~%" in-chans out-chans))
      (if (and (> in-chans 1)
	       (not (= in-chans out-chans)))
	  (error "input must be mono or input channels must equal output channels"))
      (if (not (mixer? output-mixer))
	  (if (vector? output-mixer)
	      (do ((i 0 (+ i 1)))
		  ((= i out-chans))
		(do ((j 0 (+ j 1)))
		    ((= j out-chans))
		  (set! (out-mix i j) (vector-ref output-mixer (+ (* i out-chans) j)))))
	      (do ((i 0 (+ i 1)))
		  ((= i out-chans))
		(do ((j 0 (+ j 1)))
		    ((= j out-chans))
		  (set! (out-mix i j)
			(/ (* output-gain (if (= i j)
					      local-gain
					      global-gain))
			   out-chans))))))
      (do ((c 0 (+ 1 c)))
	  ((= c in-chans))
	(vector-set! predelays
		     c
		     (make-delay :size (round (* *clm-srate*
						 (if (vector? predelay)
						     (vector-ref predelay c)
						     (if (list? predelay)
							 (list-ref predelay c)
							 predelay)))))))
      (do ((c 0 (+ 1 c)))
	  ((= c out-chans))
	(do ((i 0 (+ i 1)))
	    ((= i numcombs))
	  (let* ((tuning (list-ref combtuning i))
		 (len (floor (* srate-scale tuning)))
		 (dmp (* scale-damping
			 (if (vector? damping)
			     (vector-ref damping i)
			     (if (list? damping)
				 (list-ref damping i)
				 damping)))))
	    (if (odd? c)
		(set! len (+ len (floor (* srate-scale stereo-spread)))))
	    (vector-set! fcombs (+ (* c numcombs) i)
			 (make-filtered-comb :size len 
					     :scaler room-decay-val 
					     :filter (make-one-zero :a0 (- 1.0 dmp) :a1 dmp))))))
      (do ((c 0 (+ 1 c)))
	  ((= c out-chans))
	(do ((i 0 (+ i 1)))
	    ((= i numallpasses))
	  (let* ((tuning (list-ref allpasstuning i))
		 (len (floor (* srate-scale tuning))))
	    (if (odd? c)
		(set! len (+ len (floor (* srate-scale stereo-spread)))))
	    (vector-set! allpasses (+ (* c numallpasses) i)
			 (make-all-pass :size len :feedforward -1 :feedback 0.5)))))

      (if (and (= out-chans 1)
	       (= in-chans 1))
	  
	  (let ((amp (out-mix 0 0))
		(pdelay (vector-ref predelays 0)))
	    (set! allpasses (make-all-pass-bank allpasses))
	    (set! fcombs (make-filtered-comb-bank fcombs))

	    (do ((i beg (+ i 1)))
		((= i end))
	      (outa i (* amp (all-pass-bank allpasses
					    (filtered-comb-bank fcombs
								(delay pdelay
								       (ina i *reverb*))))))))
	  
	  (let ((allp-c (make-vector out-chans))
		(fcmb-c (make-vector out-chans)))
	    (do ((c 0 (+ c 1)))
		((= c out-chans))
	      (set! (allp-c c) (make-vector numallpasses))
	      (set! (fcmb-c c) (make-vector numcombs)))
	    (do ((c 0 (+ c 1)))
		((= c out-chans))
	      (do ((j 0 (+ j 1)))
		  ((= j numcombs))
		(set! ((fcmb-c c) j) (fcombs (+ j (* c numcombs)))))
	      (do ((j 0 (+ j 1)))
		  ((= j numallpasses))
		(set! ((allp-c c) j) (allpasses (+ j (* c numallpasses)))))
	      (set! (allp-c c) (make-all-pass-bank (allp-c c)))
	      (set! (fcmb-c c) (make-filtered-comb-bank (fcmb-c c))))
	    
	    (do ((i beg (+ i 1)))
		((= i end))
	      (file->frame *reverb* i f-in)
	      (if (> in-chans 1)
		  (do ((c 0 (+ c 1)))
		      ((= c out-chans))
		    (frame-set! f-out c 
				(filtered-comb-bank (vector-ref fcmb-c c)
						    (delay (vector-ref predelays c) 
							   (frame-ref f-in c)))))
		  
		  (let ((val (delay (vector-ref predelays 0) (frame-ref f-in 0))))
		    (do ((c 0 (+ c 1)))
			((= c out-chans))
		      (frame-set! f-out c 
				  (filtered-comb-bank (vector-ref fcmb-c c) val)))))
	      
	      (do ((c 0 (+ 1 c)))
		  ((= c out-chans))
		(frame-set! f-out c (all-pass-bank (vector-ref allp-c c) (frame-ref f-out c))))
	      
	      (frame->file *output* i (frame->frame f-out out-mix out-buf))))))))

;; freeverb.scm ends here

;;; (with-sound (:statistics #t :reverb freeverb :reverb-data '(:output-gain 3.0)) (outa 0 .5 *reverb*))
;;; (with-sound (:channels 2 :reverb-channels 2 :statistics #t :reverb freeverb :reverb-data '(:output-gain 3.0)) (outa 0 .5 *reverb*) (outb 0 .1 *reverb*))


