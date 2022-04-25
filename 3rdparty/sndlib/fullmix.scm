(provide 'snd-fullmix.scm)

(if (provided? 'snd)
    (if (not (provided? 'snd-ws.scm)) (load "ws.scm"))
    (if (not (provided? 'sndlib-ws.scm)) (load "sndlib-ws.scm")))


(definstrument (fullmix in-file beg outdur inbeg matrix srate reverb-amount)
  ;; "matrix" can be a simple amplitude or a list of lists
  ;;     each inner list represents one input channel's amps into one output channel
  ;;     each element of the list can be a number, a list (turned into an env) or an env
  ;;
  ;; "srate" can be a negative number (read in reverse), or an envelope.
  (let ((st (seconds->samples (or beg 0.0)))
	(dur (or outdur
		 (/ (- (mus-sound-duration in-file) (or inbeg 0.0))
		    (or (and srate (number? srate) (abs srate)) 1.0))))
	(in-chans (channels in-file))
	(out-chans (channels *output*))
	(reversed (or (and (number? srate) (negative? srate))
		      (and (list? srate) (negative? (cadr srate)))))
	
	(inloc (floor (* (or inbeg 0.0) (mus-sound-srate in-file)))))
    
    (let ((samps (seconds->samples dur))
	  (mx (if matrix
		  (make-mixer (max in-chans out-chans))
		  (make-scalar-mixer (max in-chans out-chans) 1.0)))
	  (rev-mx (if (and *reverb* reverb-amount (> reverb-amount 0.0))
		      (let ((rmx (make-mixer in-chans)))
			(do ((i 0 (+ i 1)))
			    ((= i in-chans))
			  (mixer-set! rmx i 0 reverb-amount)) ; 0->assume 1 chan reverb stream, I think
			rmx)
		      #f))
	  
	  (file (if (or (not srate) 
			(and (number? srate) 
			     (= srate 1.0)))
		    (make-file->frame in-file)
		    (let ((vect (make-vector in-chans #f)))
		      (do ((i 0 (+ i 1)))
			  ((= i in-chans))
			(vector-set! vect i (make-readin in-file i inloc :direction (if reversed -1 1))))
		      vect)))
	  (envs #f)
	  (srcenv (if (list? srate)
		      (make-env srate :duration dur :scaler (if reversed -1.0 1.0))
		      #f)))
      
      (if matrix
	  (begin
	    (if (list? matrix) ; matrix is list of scalers, envelopes (lists), or env gens
		(do ((inp 0 (+ inp 1))
		     (off 0 (+ off out-chans)))
		    ((= inp in-chans))
		  (let ((inlist (list-ref matrix inp)))
		    (do ((outp 0 (+ outp 1)))
			((= outp out-chans))
		      (let ((outn (list-ref inlist outp)))
			(if outn
			    (if (number? outn)
				(mixer-set! mx inp outp outn)
				(if (or (env? outn)
					(list? outn))
				    (begin
				      (if (not envs)
					  (set! envs (make-vector (* in-chans out-chans) #f)))
				      (if (env? outn)
					  (vector-set! envs (+ off outp) outn)
					  (vector-set! envs (+ off outp) (make-env outn :duration dur))))
				    (format #t "unknown element in matrix: ~A" outn))))))))
		(do ((inp 0 (+ inp 1))) ; matrix is a number in this case (a global scaler)
		    ((= inp in-chans))
		  (if (< inp out-chans)
		      (mixer-set! mx inp inp matrix))))))
      
      (if (or (not srate)
	      (and (number? srate)
		   (= srate 1.0)))
	  (let ((mxe (if envs
			 (let ((v (make-vector in-chans)))
			   (do ((i 0 (+ i 1))
				(off 0 (+ off out-chans)))
			       ((= i in-chans))
			     (let ((vo (make-vector out-chans #f)))
			       (vector-set! v i vo)
			       (do ((j 0 (+ j 1)))
				   ((= j out-chans))
				 (vector-set! vo j (vector-ref envs (+ off j))))))
			   v)
			 envs)))
	    ;; -------- no src
	    (mus-mix *output* file st samps inloc mx mxe)
	    (if rev-mx
		(mus-mix *reverb* file st samps inloc rev-mx #f)))
	  
	  (let ((srcs (make-vector in-chans #f)))
	    (do ((inp 0 (+ inp 1)))
		((= inp in-chans))
	      (vector-set! srcs inp (make-src :input (vector-ref file inp) :srate (if (number? srate) (abs srate) 0.0))))
	    (mus-mix-with-envs file st samps mx rev-mx envs srcs srcenv *output* *reverb*)
	    )))))

#|
(with-sound (:channels 2 :statistics #t)
  (fullmix "pistol.snd")
  (fullmix "2.snd" .5 1)
  (fullmix "2.snd" 1.5 1 0 #f 2.0)
  (fullmix "oboe.snd" 1 2 0 (list (list .1 (make-env '(0 0 1 1) :duration 2 :scaler .5))))
  (fullmix "pistol.snd" 2 1 0 #f .5)
  (fullmix "2.snd" 0 2 0 (list (list .1 .2) (list .3 .4)) 2.0)
  (fullmix "oboe.snd" 3 2 0 (list (list .1 (make-env '(0 0 1 1) :duration 2 :scaler .5))) .25)
  (let ((e0->0 (make-env '(0 0 1 1) :duration 2))
	(e0->1 (make-env '(0 1 1 0) :duration 2))
	(e1->0 (make-env '(0 1 1 0) :duration 2))
	(e1->1 (make-env '(0 0 1 1) :duration 2)))
    (fullmix "2.snd" 4 2 0 (list (list e0->0 e0->1) (list e1->0 e1->1))))
  (let ((e0->0 (make-env '(0 0 1 1) :duration 2))
	(e0->1 (make-env '(0 1 1 0) :duration 2))
	(e1->0 (make-env '(0 1 1 0) :duration 2))
	(e1->1 (make-env '(0 0 1 1) :duration 2)))
    (fullmix "2.snd" 6 2 0 (list (list e0->0 e0->1) (list e1->0 e1->1)) 2.0)))

(with-sound (:channels 2 :statistics #t)
  (fullmix "2.snd" 0 2 0 (list (list .1 .2) (list .3 .4)) 2.0))

(with-sound () (fullmix "pistol.snd" 0 2 2 #f -1.0))

(with-sound (:channels 2)
  (let ((e0->0 (make-env '(0 0 1 1) :duration 2))
	(e0->1 (make-env '(0 1 1 0) :duration 2))
	(e1->0 (make-env '(0 1 1 0) :duration 2))
	(e1->1 (make-env '(0 0 1 1) :duration 2)))
    (fullmix "2.snd" 6 2 0 (list (list e0->0 e0->1) (list e1->0 e1->1))) 2.0))

  
(with-sound () (fullmix "pistol.snd"))
(with-sound () (fullmix "pistol.snd" 1))
(with-sound () (fullmix "pistol.snd" 1 1))
(with-sound () (fullmix "pistol.snd" 0 1 1))
(with-sound (:statistics #t) (fullmix "pistol.snd" 0 1 0 2.0))
(with-sound (:statistics #t :channels 2) (fullmix "pistol.snd" 0 1 0 2.0))
(with-sound (:statistics #t :channels 2) (fullmix "pistol.snd" 0 1 0 (list (list 0.1 0.7))))
(with-sound (:statistics #t :channels 2) (fullmix "pistol.snd" 0 1 0 (list (list 0.1 (list 0 0 1 1)))))

(with-sound (:channels 2 :output "one-2.snd") (do ((i 0 (+ i 1))) ((= i 10000)) (outa i 0.5) (outb i -0.5)))
(with-sound (:channels 4 :output "one-4.snd") (do ((i 0 (+ i 1))) ((= i 10000)) (outa i 0.5) (outb i -0.5) (outc i 0.1) (outd i -0.1)))

(with-sound (:statistics #t :channels 2) (fullmix "one-2.snd" 0 .2 0 '((1.0 0.5) (0.5 1.0))))
(with-sound (:statistics #t :channels 2) (fullmix "one-2.snd" 0 .2 0 (list (list 0.1 (list 0 0 1 1)) (list (list 0 1 1  0) .5))))
(with-sound (:statistics #t :channels 2) 
  (let ((e0->0 (make-env '(0 0 1 1) :end 10000))
	(e0->1 (make-env '(0 1 1 0) :end 10000))
	(e1->0 (make-env '(0 1 1 0) :end 10000))
	(e1->1 (make-env '(0 0 1 1) :end 10000)))
    (fullmix "one-2.snd" 0 .2 0 (list (list e0->0 e0->1) (list e1->0 e1->1)))))


(with-sound (:statistics #t :channels 2 :reverb jc-reverb) 
  (let ((e0->0 (make-env '(0 0 1 1) :end 10000))
	(e0->1 (make-env '(0 1 1 0) :end 10000))
	(e1->0 (make-env '(0 1 1 0) :end 10000))
	(e1->1 (make-env '(0 0 1 1) :end 10000)))
    (fullmix "one-2.snd" 0 .2 0 (list (list e0->0 e0->1) (list e1->0 e1->1)) #f .1)))

(with-sound () (fullmix "oboe.snd" 0 2 0 #f '(0 0.5 1 1 2 .1)))
|#
