;;; Perry Cook's maraca from CMJ vol 21 no 3 (Fall 97) p 44
;;;   translated from CLM's maraca.ins

(provide 'snd-maraca.scm)
(if (provided? 'snd)
    (if (not (provided? 'snd-ws.scm)) (load "ws.scm"))
    (if (not (provided? 'sndlib-ws.scm)) (load "sndlib-ws.scm")))

(define two-pi (* 2 pi))

(definstrument (maraca beg dur (amp .1) 
		 (sound-decay 0.95) 
		 (system-decay 0.999) 
		 (probability .0625)
		 (shell-freq 3200.0)
		 (shell-reso 0.96))
  (let ((num-beans 64))
    (let ((st (seconds->samples beg))
	  (nd (seconds->samples (+ beg dur)))
	  (temp 0.0)
	  (shake-energy 0.0)
	  (snd-level 0.0)
	  (input 0.0)
	  (stop 0)
	  (h20 (hz->radians 20.0))
	  (sndamp (/ amp 16384.0))
	  (srate4 (floor (/ (mus-srate) 4)))
	  (gain (/ (* (log num-beans 4.0) 40) num-beans))
	  (tz (make-two-pole 1.0 (* -2.0 shell-reso (cos (hz->radians shell-freq))) (* shell-reso shell-reso))) 
	  (oz (make-one-zero 1.0 -1.0))
	  ;; gourd resonance filter
	  )
      (do ((i st (+ i srate4)))
	  ((>= i nd))
	(set! temp 0.0)
	(set! stop (min nd (+ i srate4)))
	(do ((k i (+ k 1)))
	    ((= k stop))
	  (if (< temp two-pi)
	      (begin
		;; shake over 50msec and add shake energy
		(set! temp (+ temp h20))
		(set! shake-energy (+ shake-energy (- 1.0 (cos temp))))))
	  (set! shake-energy (* shake-energy system-decay))
	  ;; if collision, add energy
	  (if (< (random 1.0) probability)
	      (set! snd-level (+ snd-level (* gain shake-energy))))
	  ;; actual sound is random
	  (set! input (mus-random snd-level))
	  ;; compute exponential sound decay
	  (set! snd-level (* snd-level sound-decay))
	  ;; gourd resonance filter calc
	  (outa k (* sndamp (one-zero oz (two-pole tz input)))))))))

;;; maraca: (with-sound (:statistics #t :play #t) (maraca 0 5 .5))
;;; cabasa: (with-sound (:statistics #t :play #t) (maraca 0 5 .5 0.95 0.997 0.5 3000.0 0.7))

(definstrument (big-maraca beg dur (amp .1) 
			   (sound-decay 0.95) 
			   (system-decay 0.999) 
			   (probability .0625)
			   (shell-freqs '(3200.0))
			   (shell-resos '(0.96))
			   (randiff .01)
			   (with-filters #t))
  ;; like maraca, but takes a list of resonances and includes low-pass filter (or no filter)	
  (let ((num-beans 64)
	(resn (length shell-freqs)))
    (let ((st (seconds->samples beg))
	  (nd (seconds->samples (+ beg dur)))
	  (temp 0.0)
	  (shake-energy 0.0)
	  (snd-level 0.0)
	  (input 0.0)
	  (sum 0.0)
	  (last-sum 0.0)
	  (tzs (make-vector resn))
	  (h20 (hz->radians 20.0))
	  (stop 0)
	  (sndamp (/ amp (* 16384.0 resn)))
	  (srate4 (floor (/ (mus-srate) 4)))
	  (gain (/ (* (log num-beans 4) 40) num-beans))
	  (oz (make-one-zero (/ amp (* resn 16384.0)) (/ amp (* resn 16384.0)))))

      ;; we need to fixup Perry's frequency dithering amount since we're going through our mus-frequency method
      (set! randiff (radians->hz randiff))

      ;; gourd resonance filters
      (do ((i 0 (+ i 1)))
	  ((= i resn))
	(vector-set! tzs i (make-two-pole 1.0 
					  (* -2.0 (shell-resos i) (cos (hz->radians (shell-freqs i))))
					  (* (shell-resos i) (shell-resos i)))))
      
      (do ((i st (+ i srate4)))
	  ((>= i nd))
	(set! temp 0.0)
	(set! stop (min nd (+ i srate4)))
	(do ((k i (+ k 1)))
	    ((= k stop))

	  (if (< temp two-pi)
	      (begin
		;; shake over 50msec and add shake energy
		(set! temp (+ temp h20))
		(set! shake-energy (+ shake-energy (- 1.0 (cos temp))))))

	  (set! shake-energy (* shake-energy system-decay))
	  ;; if collision, add energy
	  (if (< (random 1.0) probability)
	      (begin
		(set! snd-level (+ snd-level (* gain shake-energy)))
		;; randomize res freqs a bit
		(do ((j 0 (+ j 1)))
		    ((= j resn))
		  (set! (mus-frequency (vector-ref tzs j)) (+ (shell-freqs j) (mus-random randiff))))))

	  ;; actual sound is random
	  (set! input (mus-random snd-level))
	  ;; compute exponential sound decay
	  (set! snd-level (* snd-level sound-decay))

	  ;; gourd resonance filter calcs
	  (set! last-sum sum)
	  (set! sum 0.0)
	  (do ((j 0 (+ j 1)))
	      ((= j resn))
	    (set! sum (+ sum (two-pole (vector-ref tzs j) input))))

	  (if with-filters
	      (outa k (one-zero oz (- sum last-sum)))
	      (outa k (* sndamp sum))))))))
	  
;;; tambourine: (with-sound (:play #t :statistics #t) (big-maraca 0 1 .25 0.95 0.9985 .03125 '(2300 5600 8100) '(0.96 0.995 0.995) .01))
;;; sleighbells: (with-sound (:play #t :statistics #t) (big-maraca 0 2 .15 0.97 0.9994 0.03125 '(2500 5300 6500 8300 9800) '(0.999 0.999 0.999 0.999 0.999)))
;;; sekere: (with-sound (:play #t :statistics #t) (big-maraca 0 2 .5 0.96 0.999 .0625 '(5500) '(0.6)))
;;; windchimes: (with-sound (:play #t :statistics #t) (big-maraca 0 2 .5 0.99995 0.95 .001 '(2200 2800 3400) '(0.995 0.995 0.995) .01 #f))

