;;; bess1.scm -- some examples from clm-2/rt.lisp and clm-2/bess5.cl

;; Author: Michael Scholz <scholz-micha@gmx.de>
;; Created: Thu May 29 04:14:35 CEST 2003
;; Last: Sun Jun 15 03:50:21 CEST 2003
;; changed slightly 14-Jun-06 Bill to match bess.scm, fix pitch problem in make-oscil.
;;   then again 18-Dec-09 to use s7 rather than Guile
;; changed vct-map! to use a loop instead (Bill 4-July-12)

(if (not (provided? 'snd-motif)) (error "bess1.scm needs motif"))

;;; Commentary:

;; This file provides simple mono real time output to DAC.  Tempo,
;; frequency, amplitude, and FM index can be controlled via sliders.
;; The music algorithms are taken from clm-2/rt.lisp and
;; clm-2/bess5.cl.

;; (main) calls (rt-motif) which starts a Motif widget with two DAC
;; tests.
;;
;; (rt-motif :srate       *clm-srate*        ;; 22050
;;           :bufsize     *clm-rt-bufsize*   ;; 128
;;           :data-format *clm-data-format*) ;; mus-lshort

;;; Code:

(define *clm-srate* 22050)
(define *clm-data-format* mus-lfloat)
(define *clm-rt-bufsize* 1024)
(define *output* #f)			;holds fd from (mus-audio-open-output)

(define ctempo 0.25)
(define camp 1.0)
(define cfreq 1.0)
(define cindex 1.0)
(define cplay #f)
  
(define sliderback "lightsteelblue")
(define background "lightsteelblue1")

;(define (seconds->samples secs) (round (* secs (mus-srate))))

;; called by XtAppAddWorkProc
(define (rt-send->dac func)
  (if cplay
      (let ((data (make-vct *clm-rt-bufsize*)))
	(do ((i 0 (+ i 1)))
	    ((= i *clm-rt-bufsize*))
	  (set! (data i) (func)))
	(mus-audio-write *output* (vct->sound-data data (make-sound-data 1 *clm-rt-bufsize*) 0) *clm-rt-bufsize*)
	#f)
      (begin
	(mus-audio-close *output*)
	#t)))

(define* (make-rt-violin dur freq amp
			       (fm-index 1.0)
			       (amp-env '(0 0 25 1 75 1 100 0)))
  "(make-rt-violin dur freq amp (fm-index 1.0) (amp-env '(0 0 25 1 75 1
100 0))) real time simple violin (see snd-14/fm.html)"
  (let* ((frq-scl (hz->radians freq))
         (maxdev (* frq-scl fm-index))
         (index1 (* maxdev (/ 5.0 (log freq))))
         (index2 (* maxdev 3.0 (/ (- 8.5 (log freq)) (+ 3.0 (/ freq 1000)))))
         (index3 (* maxdev (/ 4.0 (sqrt freq))))
         (carrier (make-oscil :frequency freq))
         (fmosc1 (make-oscil :frequency freq))
         (fmosc2 (make-oscil :frequency (* 3 freq)))
         (fmosc3 (make-oscil :frequency (* 4 freq)))
         (ampf  (make-env :envelope amp-env :scaler amp :duration dur))
         (indf1 (make-env :envelope '(0 1 25 0.4 75 0.6 100 0) :scaler index1 :duration dur))
         (indf2 (make-env :envelope '(0 1 25 0.4 75 0.6 100 0) :scaler index2 :duration dur))
         (indf3 (make-env :envelope '(0 1 25 0.4 75 0.6 100 0) :scaler index3 :duration dur))
         (pervib (make-triangle-wave :frequency 5 :amplitude (* 0.0025 frq-scl)))
         (ranvib (make-rand-interp :frequency 16 :amplitude (* 0.005 frq-scl))))
    (lambda ()
      (let ((vib (+ (triangle-wave pervib) (rand-interp ranvib))))
	(* (env ampf)
	   (oscil carrier
		  (+ vib 
		     (* (env indf1) (oscil fmosc1 vib))
		     (* (env indf2) (oscil fmosc2 (* 3.0 vib)))
		     (* (env indf3) (oscil fmosc3 (* 4.0 vib))))))))))

(define lim 256)

;; from clm-2/rt.lisp
(define* (make-vct-test (srate *clm-srate*)
			(bufsize *clm-rt-bufsize*)
			(data-format *clm-data-format*))
  (let ((vmode (vector 0 12 2 4 14 4 5 5 0 7 7 11 11))
	(vpits (make-vector (+ 1 lim) 0))
	(vbegs (make-vector (+ 1 lim) 0))
	(cellbeg 0)
	(cellsiz 6)
	(cellctr 0)
	(func #f)
	(len 0)
	(dur 0.0))
    (do ((i 0 (+ 1 i)))
	((= i lim))
      (set! (vpits i) (floor (random 12.0)))
      (set! (vbegs i) (+ 1 (floor (random 3.0)))))
    (set! *clm-srate* srate)
    (set! *clm-rt-bufsize* bufsize)
    (set! (mus-srate) srate)
    (set! *output* (mus-audio-open-output mus-audio-default srate 1 data-format (* bufsize 2)))
    (lambda ()
      (if (> len 1)
	  (set! len (- len 1))
	  (begin
	    (set! dur (* ctempo (vbegs (+ cellctr 1))))
	    (set! cellctr (+ cellctr 1))
	    (if (> cellctr (+ cellsiz cellbeg))
		(begin
		  (if (> (random 1.0) 0.5) (set! cellbeg (+ 1 cellbeg)))
		  (if (> (random 1.0) 0.5) (set! cellsiz (+ 1 cellsiz)))
		  (set! cellctr cellbeg)))

	    (format #t "dur: ~A, freq: ~A, amp: ~A, index: ~A~%"
		    dur
		    (let ((freq (* cfreq 16.351 16
				   (expt 2 (/ (vmode (vpits cellctr))
					      12.0)))))
		      (if (< (* 8 freq) *clm-srate*)
			  freq
			  (/ freq 4)))
		    (* camp 0.3) cindex)

	    (set! func (make-rt-violin dur
				       (let ((freq (* cfreq 16.351 16
						      (expt 2 (/ (vmode (vpits cellctr))
								 12.0)))))
					 (if (< (* 8 freq) *clm-srate*)
					     freq
					     (/ freq 4)))
				       (* camp 0.3) :fm-index cindex))
	    (set! len (ceiling (/ (seconds->samples dur) bufsize)))))
      func)))

;; from clm-2/bess5.cl and clm-2/clm-example.lisp
(define time 60)
(define mode (vector 0 0 2 4 11 11 5 6 7 9 2 0 0))
(define rats (vector 1.0 256/243 9/8 32/27 81/64 4/3 1024/729 3/2 128/81 27/16 16/9 243/128 2.0))

(define bell '(0 0 10 0.25 90 1.0 100 1.0))

(define pits (make-vector (+ 1 lim) 0))
(define octs (make-vector (+ 1 lim) 0))
(define rhys (make-vector (+ 1 lim) 0))
(define begs (make-vector (+ 1 lim) 0))
(define amps (make-vector (+ 1 lim) 0))

(define (tune x)
  (let* ((pit (modulo x 12))
	 (oct (floor (/ x 12)))
	 (base (rats pit)))
    (* base (expt 2 oct))))

(define (rbell x)
  (envelope-interp (* x 100) bell))

(define* (make-agn (srate *clm-srate*)
		   (bufsize *clm-rt-bufsize*)
		   (data-format *clm-data-format*))
  (let ((wins (vector '(0 0 40 0.1 60 0.2 75 0.4 82 1 90 1 100 0)
		      '(0 0 60 0.1 80 0.2 90 0.4 95 1 100 0)
		      '(0 0 10 1 16 0 32 0.1 50 1 56 0 60 0 90 0.3 100 0)
		      '(0 0 30 1 56 0 60 0 90 0.3 100 0)
		      '(0 0 50 1 80 0.3 100 0)
		      '(0 0 40 0.1 60 0.2 75 0.4 82 1 90 1 100 0)
		      '(0 0 40 0.1 60 0.2 75 0.4 82 1 90 1 100 0)
		      '(0 0 10 1 32 0.1 50 1 90 0.3 100 0)
		      '(0 0 60 0.1 80 0.3 95 1 100 0)
		      '(0 0 80 0.1 90 1 100 0)))
	(nextbeg 0.0)
	(beg 0.0)
	(dur 0.0)
	(freq 0.0)
	(ampl 0.0)
	(ind 0.0)
	(cellctr 0)
	(cellsiz 4)
	(cellbeg 0)
	(whichway 1)
	(func #f)
	(len 0))
    (do ((i 0 (+ 1 i)))
	((= i lim))
      (set! (octs i) (floor (+ 4 (* 2 (rbell (random 1.0))))))
      (set! (pits i) (mode (floor (random 12.0))))
      (set! (rhys i) (floor (+ 4 (random 6.0))))
      (set! (begs i) (if (< (random 1.0) 0.9) 
		      (floor (+ 4 (random 2.0)))
		      (floor (random 24.0))))
      (set! (amps i) (floor (+ 1 (* 8 (rbell (random 1.0)))))) i)
    (set! *clm-srate* srate)
    (set! *clm-rt-bufsize* bufsize)
    (set! (mus-srate) srate)
    (set! *output* (mus-audio-open-output mus-audio-default srate 1 data-format (* bufsize 2)))
    (lambda ()
      (if (> len 1)
	  (set! len (- len 1))
	  (begin
	    (set! beg (+ beg nextbeg))
	    (set! nextbeg (+ nextbeg (max 0.025 (* ctempo (+ 0.95 (random 0.1)) (begs cellctr)))))
	    (set! dur (max 0.025 (* ctempo (+ 0.85 (random 0.1)) (rhys cellctr))))
	    (set! freq (* cfreq 16.351 (tune (pits cellctr)) (expt 2 (octs cellctr))))
	    (set! ampl (* camp 10 (max 0.003 (* (amps cellctr) 0.01))))
	    (set! ind (* cindex (random 3.0)))
	    (set! cellctr (+ cellctr 1))
	    (if (> cellctr (+ cellsiz cellbeg))
		(begin
		  (set! cellbeg (+ 1 cellbeg))
		  (if (> (random 1.0) 0.5) (set! cellsiz (+ cellsiz whichway)))
		  (if (and (> cellsiz 10) (> (random 1.0) 0.99))
		      (set! whichway -2)
		      (if (and (> cellsiz 6) (> (random 1.0) 0.999))
			  (set! whichway -1)
			  (if (< cellsiz 4)
			      (set! whichway 1))))
		  (set! nextbeg (+ nextbeg (random 1.0)))
		  (set! cellctr cellbeg)))
	    (set! func (make-rt-violin dur freq ampl
				       :fm-index ind
				       :amp-env (wins (floor (* 10 (- beg (floor beg)))))))
	    (set! len (ceiling (/ (seconds->samples dur) bufsize)))))
      func)))

#|
;; from env.scm
(define* (envelope-interp :rest args)
  (let ((x (car args))
	(env (cadr args))
	(base (if (null? (cddr args)) #f (caddr args))))
    (cond ((null? env) 0.0)
	  ((or (<= x (car env))
	       (null? (cddr env)))
	   (cadr env))
	  ((> (caddr env) x)
	   (if (or (= (cadr env) (cadddr env))
		   (and base (= base 0.0)))
	       (cadr env)
	       (if (or (not base) (= base 1.0))
		   (+ (cadr env)
		      (* (- x (car env))
			 (/ (- (cadddr env) (cadr env))
			    (- (caddr env) (car env)))))
		   (+ (cadr env)
		      (* (/ (- (cadddr env) (cadr env))
			    (- base 1.0))
			 (- (expt base (/ (- x (car env))
					  (- (caddr env) (car env))))
			    1.0))))))
	  (else (envelope-interp x (cddr env))))))
|#

(define* (rt-motif :rest args)
  (let* ((shell-app (XtVaOpenApplication 
		     "FM" 0 () applicationShellWidgetClass
		     (list XmNallowShellResize #t)))
	 (app (cadr shell-app))
	 (shell (car shell-app))
	 (dpy (XtDisplay shell))
	 (screen (DefaultScreenOfDisplay dpy))
	 (cmap (DefaultColormap dpy (DefaultScreen dpy)))
	 (black (BlackPixelOfScreen screen)))

    (define (get-color color)
      (let ((col (XColor)))
	(if (= (XAllocNamedColor dpy cmap color col col) 0)
	    (error (format #f "can't allocate ~A" color))
	    (.pixel col))))

    (define (set-flabel label value)
      (let ((s1 (XmStringCreate (format #f "~5,3F" value) XmFONTLIST_DEFAULT_TAG)))
	(XtVaSetValues label (list XmNlabelString s1))
	(XmStringFree s1)))

    (XtSetValues shell (list XmNtitle "FM Forever!"))
    (let* ((light-blue (get-color sliderback))
	   (form (XtCreateManagedWidget "form" xmFormWidgetClass shell 
					(list XmNbackground (get-color background)
					      XmNforeground black
					      XmNresizePolicy XmRESIZE_GROW)))
	   ;; play
	   (play-button (XtCreateManagedWidget "play" xmToggleButtonWidgetClass form
					       (list XmNleftAttachment   XmATTACH_FORM
						     XmNbottomAttachment XmATTACH_NONE
						     XmNtopAttachment    XmATTACH_FORM
						     XmNrightAttachment  XmATTACH_NONE
						     XmNbackground       (get-color background))))
	   ;; radio
	   (radio (XmCreateRadioBox form "radio"
				    (list XmNorientation XmHORIZONTAL
					  XmNleftAttachment   XmATTACH_WIDGET
					  XmNleftWidget       play-button
					  XmNbottomAttachment XmATTACH_NONE
					  XmNtopAttachment    XmATTACH_FORM
					  XmNrightAttachment  XmATTACH_NONE
					  XmNbackground       (get-color background))))
	   ;; play agn
	   (agn-button (XtCreateManagedWidget "agn" xmToggleButtonWidgetClass radio
					      (list XmNleftAttachment   XmATTACH_FORM
						    XmNbottomAttachment XmATTACH_NONE
						    XmNtopAttachment    XmATTACH_FORM
						    XmNrightAttachment  XmATTACH_NONE
						    XmNbackground       (get-color background))))
	   ;; play test
	   (test-button (XtCreateManagedWidget "test" xmToggleButtonWidgetClass radio
					       (list XmNleftAttachment   XmATTACH_WIDGET
						     XmNleftWidget       agn-button
						     XmNbottomAttachment XmATTACH_NONE
						     XmNtopAttachment    XmATTACH_FORM
						     XmNrightAttachment  XmATTACH_NONE
						     XmNbackground       (get-color background))))
	   ;; quit
	   (quit-button (XtCreateManagedWidget " quit " xmPushButtonWidgetClass form
					       (list XmNleftAttachment   XmATTACH_WIDGET
						     XmNleftWidget       radio
						     XmNbottomAttachment XmATTACH_NONE
						     XmNtopAttachment    XmATTACH_FORM
						     XmNrightAttachment  XmATTACH_FORM
						     XmNbackground       (get-color background))))
	   (sep (XtCreateManagedWidget "sep" xmSeparatorWidgetClass form
				       (list XmNleftAttachment   XmATTACH_FORM
					     XmNbottomAttachment XmATTACH_NONE
					     XmNtopAttachment    XmATTACH_WIDGET
					     XmNtopWidget        radio
					     XmNrightAttachment  XmATTACH_FORM
					     XmNheight 4
					     XmNorientation XmHORIZONTAL)))
	   ;; tempo
	   (tempo (XtCreateManagedWidget " tempo:" xmLabelWidgetClass form
					 (list XmNleftAttachment   XmATTACH_FORM
					       XmNbottomAttachment XmATTACH_NONE
					       XmNtopAttachment    XmATTACH_WIDGET
					       XmNtopWidget        sep
					       XmNrightAttachment  XmATTACH_NONE
					       XmNrecomputeSize    #f
					       XmNbackground       (get-color background))))
	   (tempo-label (XtCreateManagedWidget "label" xmLabelWidgetClass form
					       (list XmNleftAttachment   XmATTACH_WIDGET
						     XmNleftWidget       tempo
						     XmNbottomAttachment XmATTACH_NONE
						     XmNtopAttachment    XmATTACH_OPPOSITE_WIDGET
						     XmNtopWidget        tempo
						     XmNrightAttachment  XmATTACH_NONE
						     XmNbackground       (get-color background))))
	   (tempo-scale (XtCreateManagedWidget "tempo" xmScaleWidgetClass form
					       (list XmNleftAttachment   XmATTACH_WIDGET
						     XmNleftWidget       tempo-label
						     XmNbottomAttachment XmATTACH_NONE
						     XmNtopAttachment    XmATTACH_OPPOSITE_WIDGET
						     XmNtopWidget        tempo-label
						     XmNrightAttachment  XmATTACH_FORM
						     XmNshowValue        #f
						     XmNorientation      XmHORIZONTAL
						     XmNheight           20
						     XmNbackground       light-blue)))
	   ;; freq
	   (freq (XtCreateManagedWidget "  freq:" xmLabelWidgetClass form
					(list XmNleftAttachment   XmATTACH_FORM
					      XmNbottomAttachment XmATTACH_NONE
					      XmNtopAttachment    XmATTACH_WIDGET
					      XmNtopWidget        tempo
					      XmNrightAttachment  XmATTACH_NONE
					      XmNrecomputeSize    #f
					      XmNbackground       (get-color background))))
	   (freq-label (XtCreateManagedWidget "label" xmLabelWidgetClass form
					      (list XmNleftAttachment   XmATTACH_WIDGET
						    XmNleftWidget       freq
						    XmNbottomAttachment XmATTACH_NONE
						    XmNtopAttachment    XmATTACH_OPPOSITE_WIDGET
						    XmNtopWidget        freq
						    XmNrightAttachment  XmATTACH_NONE
						    XmNbackground       (get-color background))))
	   (freq-scale (XtCreateManagedWidget "freq" xmScaleWidgetClass form
					      (list XmNleftAttachment   XmATTACH_WIDGET
						    XmNleftWidget       freq-label
						    XmNbottomAttachment XmATTACH_NONE
						    XmNtopAttachment    XmATTACH_OPPOSITE_WIDGET
						    XmNtopWidget        freq-label
						    XmNrightAttachment  XmATTACH_FORM
						    XmNshowValue        #f
						    XmNorientation      XmHORIZONTAL
						    XmNheight           20
						    XmNbackground       light-blue)))
	   ;; amp
	   (amp (XtCreateManagedWidget "   amp:" xmLabelWidgetClass form
				       (list XmNleftAttachment   XmATTACH_FORM
					     XmNbottomAttachment XmATTACH_NONE
					     XmNtopAttachment    XmATTACH_WIDGET
					     XmNtopWidget        freq
					     XmNrightAttachment  XmATTACH_NONE
					     XmNrecomputeSize    #f
					     XmNbackground       (get-color background))))
	   (amp-label (XtCreateManagedWidget "label" xmLabelWidgetClass form
					     (list XmNleftAttachment   XmATTACH_WIDGET
						   XmNleftWidget       amp
						   XmNbottomAttachment XmATTACH_NONE
						   XmNtopAttachment    XmATTACH_OPPOSITE_WIDGET
						   XmNtopWidget        amp
						   XmNrightAttachment  XmATTACH_NONE
						   XmNbackground       (get-color background))))
	   (amp-scale (XtCreateManagedWidget "amp" xmScaleWidgetClass form
					     (list XmNleftAttachment   XmATTACH_WIDGET
						   XmNleftWidget       amp-label
						   XmNbottomAttachment XmATTACH_NONE
						   XmNtopAttachment    XmATTACH_OPPOSITE_WIDGET
						   XmNtopWidget        amp-label
						   XmNrightAttachment  XmATTACH_FORM
						   XmNshowValue        #f
						   XmNorientation      XmHORIZONTAL
						   XmNheight           20
						   XmNbackground       light-blue)))
	   ;; index
	   (index (XtCreateManagedWidget " index:" xmLabelWidgetClass form
					 (list XmNleftAttachment   XmATTACH_FORM
					       XmNbottomAttachment XmATTACH_NONE
					       XmNtopAttachment    XmATTACH_WIDGET
					       XmNtopWidget        amp
					       XmNrightAttachment  XmATTACH_NONE
					       XmNrecomputeSize    #f
					       XmNbackground       (get-color background))))
	   (index-label (XtCreateManagedWidget "label" xmLabelWidgetClass form
					       (list XmNleftAttachment   XmATTACH_WIDGET
						     XmNleftWidget       index
						     XmNbottomAttachment XmATTACH_NONE
						     XmNtopAttachment    XmATTACH_OPPOSITE_WIDGET
						     XmNtopWidget        index
						     XmNrightAttachment  XmATTACH_NONE
						     XmNbackground       (get-color background))))
	   (index-scale (XtCreateManagedWidget "index" xmScaleWidgetClass form
					       (list XmNleftAttachment   XmATTACH_WIDGET
						     XmNleftWidget       index-label
						     XmNbottomAttachment XmATTACH_NONE
						     XmNtopAttachment    XmATTACH_OPPOSITE_WIDGET
						     XmNtopWidget        index-label
						     XmNrightAttachment  XmATTACH_FORM
						     XmNshowValue        #f
						     XmNorientation      XmHORIZONTAL
						     XmNheight           20
						     XmNbackground       light-blue)))
	   (low-tempo 0.05)
	   (high-tempo 0.5)
	   (high-amp 1.0)
	   (low-freq 0.1)
	   (high-freq 4.0)
	   (high-index 2.0)
	   (which-play 0)
	   (proc #f)
	   (func #f))

      (define (tempo-callback w c i)
	(set! ctempo (+ low-tempo (* (.value i) (/ (- high-tempo low-tempo) 100.0))))
	(set-flabel tempo-label ctempo))

      (define (amp-callback w c i)
	(set! camp (* (.value i) (/ high-amp 100.0)))
	(set-flabel amp-label camp))

      (define (freq-callback w c i)
	(set! cfreq (+ low-freq (* (.value i) (/ (- high-freq low-freq) 100.0))))
	(set-flabel freq-label cfreq))

      (define (index-callback w c i)
	(set! cindex (* (.value i) (/ high-index 100.0)))
	(set-flabel index-label cindex))

      (define (set-defaults)
	(set! ctempo 0.25)
	(set! camp 1.0)
	(set! cfreq 1.0)
	(set! cindex 1.0)
	(set-flabel tempo-label ctempo)
	(set-flabel amp-label camp)
	(set-flabel freq-label cfreq)
	(set-flabel index-label cindex)
	(XmScaleSetValue tempo-scale (floor (* 100 (/ (- ctempo low-tempo) (- high-tempo low-tempo)))))
	(XmScaleSetValue freq-scale (floor (* 100 (/ (- cfreq low-freq) (- high-freq low-freq)))))
	(XmScaleSetValue amp-scale (floor (* 100 camp)))
	(XmScaleSetValue index-scale (floor (* 100 (/ cindex high-index)))))

      (XtManageChild radio)
      ;; add scale-change (drag and value-changed) callbacks
      (XtAddCallback tempo-scale XmNdragCallback tempo-callback)
      (XtAddCallback tempo-scale XmNvalueChangedCallback tempo-callback)

      (XtAddCallback amp-scale XmNdragCallback amp-callback)
      (XtAddCallback amp-scale XmNvalueChangedCallback amp-callback)

      (XtAddCallback freq-scale XmNdragCallback freq-callback)
      (XtAddCallback freq-scale XmNvalueChangedCallback freq-callback)

      (XtAddCallback index-scale XmNdragCallback index-callback)
      (XtAddCallback index-scale XmNvalueChangedCallback index-callback)

      (XtAddCallback agn-button XmNvalueChangedCallback
		     (lambda (w c i)
		       (if (.set i)
			   (set! which-play 0))
		       (set! cplay #f)
		       (XmToggleButtonSetState play-button cplay #f)))

      (XmToggleButtonSetState agn-button #t #f)
      (XtAddCallback test-button XmNvalueChangedCallback
		     (lambda (w c i)
		       (if (.set i)
			   (set! which-play 1))
		       (set! cplay #f)
		       (XmToggleButtonSetState play-button cplay #f)))
		     
      (XtAddCallback quit-button XmNactivateCallback
		     (lambda (w c i)
		       (set! cplay #f)
		       (if proc (XtRemoveWorkProc proc))
		       (exit 0)))
      
      (XtAddCallback play-button XmNvalueChangedCallback
		     (lambda (w c i)
		       (set! cplay (.set i))
		       (if cplay
			   (begin
			     (set-defaults)
			     (if (= which-play 0)
				 (set! func (apply make-agn (or args ())))
				 (set! func (apply make-vct-test (or args ()))))
			     (set! proc (XtAppAddWorkProc app (lambda (c) (rt-send->dac func)))))
			   (if proc (XtRemoveWorkProc proc)))))
      (XmToggleButtonSetState play-button cplay #f)
      (set-defaults)
      (XtRealizeWidget shell))
    (XtAppMainLoop app)))

(rt-motif)

;; bess1.scm ends here
