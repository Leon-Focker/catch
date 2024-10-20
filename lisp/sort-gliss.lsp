;; * sort-gliss.lsp

(in-package :ly)

;;; This is just some scripts for generating sequences of "chords"/harmonics.
;;; These functions are not designed to be used in another context, even though
;;; they could be easily modified to do so. However, this file and its functions
;;; are purely written to use in my piece "catch a break" for flute and viola.
;;; Change this, if you want to try it yourself:
(defparameter *target-dir* "/E/catch/")

(in-scale :quarter-tone)

;; ** the material

;;; all possible notes with breaks for the flute:
(defvar +gliss+
  '((d4   ((d5 cqs5))   ((a5 a5) (bqs5 af5)))
    (ds4  ((ds5 cqs5))  ((aqs5 af5)))
    (e4   ((e5 dqf5))   ((bf5 bf5)))
    (f4   ((f5 dqs5))   ((c6 bf5)))
    (fs4  ((fs5 ef5))   ((cs6 bqf5)))
    (c4   ((g5 e5))     ((cs6 af5)))
    (d4   ((gs5 gs5))   ((d6 cs6)))
    (a4   ((e6 cqs6))   ((g6 fqf6)))
    (f4   ((fqs6 ds6))  ((g6 g6)))
    (b4   ((fs6 eqf6))  ((af6 g6) (a5 g5)))
    (cs5  ((gs6 fs6))   ((bf6 a6) (b5 aqs5)))
    (a4   ((a6 gqs6))   ((bqs6 bf6) (ef6 dqf6) (aqf5 aqf5)))
    (bf4  ((bf6 aqs6))  ((cqs7 b6) (cs6 bf5)))
    (bqs4 ((bqs6 aqs6)) ((cqs7 bqs6)))
    (b4   ((cqs7 bf6))  ((e6 e6) (aqf5 aqf5)))
    (dqf5 ((dqf7 b6))   ((eqs6 eqf6)))
    (d5   ((dqs7 cs7)   (fs6 fs6)) ((eqf6 eqf6)))))

;; ** classes and parsing

(defclass note ()
  ((id :accessor id :initarg :id :initform nil)
   (root :accessor root :initarg :root :initform nil)
   (root-f :accessor root-f :initarg :root-f :initform nil)
   (times-used :accessor times-used :type integer :initform 0)))

(defclass flute-break-gliss (note)
  ((before :accessor before :initarg :before :initform nil)
   (after :accessor after :initarg :after :initform nil)
   (before-f :accessor before-f :initarg :before-f :initform nil)
   (after-f :accessor after-f :initarg :after-f :initform nil)))

(defclass viola-harmonic (note)
  ((saite :type integer :accessor saite :initarg :saite :initform 4)
   (sounding :accessor sounding :initarg :sounding :initform nil)
   (sounding-f :accessor sounding-f :initarg :sounding-f :initform nil)
   (interval :accessor interval :initarg :interval :type integer
	     :initform nil)))

;; *** printing 
(defmethod print-object :after ((nt note) stream)
  #+nil(format stream "~&~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ~%"))

(defmethod print-object ((bg flute-break-gliss) stream)
  (format stream "~&FLUTE-BREAK-GLISS ~&id: ~a, ~&root: ~a, ~&before-break: ~a, ~&after-break: ~a"
          (id bg)
	  (root bg)
	  (before bg)
	  (after bg)))

(defmethod print-object ((vh viola-harmonic) stream)
  (format stream "<VIOLA-HARMONIC ~a>" (id vh)))

;; *** any-matchp
;;; check if root is the same or if the top and bottom frequency of each
;;; gliss can be found within a gliss of the other option.
(defmethod any-matchp ((bg1 flute-break-gliss) (bg2 flute-break-gliss))
  (let* ((r1 (root-f bg1))
	 (r2 (root-f bg2))
	 (intervals1 (append (before-f bg1) (after-f bg1)))
	 (intervals2 (append (before-f bg2) (after-f bg2))))
    (or (equal (freq-to-note r1 'chromatic-scale)
	       (freq-to-note r2 'chromatic-scale))
	(loop for interval in intervals1
	      thereis (loop for freq in (flatten intervals2)
			    thereis (<= (first interval)
					freq
					(second interval))))
        (loop for interval in intervals2
	      thereis (loop for freq in (flatten intervals1)
			    thereis (<= (first interval)
					freq
					(second interval)))))))

;; *** matchp
;;; only check if the root note or any of the starting and ending notes
;;; of the breaks match - also depends on the scale (quarter or chromatic).
(defmethod matchp ((bg1 flute-break-gliss) (bg2 flute-break-gliss)
			    &optional (scale 'quarter-tone))
  (let* ((r1 (root-f bg1))
	 (r2 (root-f bg2))
	 (break1 (append (loop for b in (before-f bg1) collect (first b))
			 (loop for a in (after-f bg1) collect (second a))))
	 (break2 (append (loop for b in (before-f bg2) collect (first b))
			 (loop for a in (after-f bg2) collect (second a)))))
    ;; predicate for checking, if two notes are the same, depending on the scale:
    (flet ((samenotep (a b)
	     (equal (freq-to-note a scale)
		    (freq-to-note b scale))))
      (or (samenotep r1 r2)
	  (loop for note in break1
		thereis (find note break2 :test #'samenotep))))))

(defmethod matchp ((vh1 viola-harmonic) (vh2 viola-harmonic)
		   &optional (scale 'chromatic-scale))
  (let* ((r1 (root-f vh1))
	 (r2 (root-f vh2))
	 (s1 (sounding-f vh1))
	 (s2 (sounding-f vh2)))
    (flet ((samenotep (a b)
	     (equal (freq-to-note a scale)
		    (freq-to-note b scale))))
      (or (samenotep r1 r2)
	  (samenotep s1 s2)))))

(defmethod matchp ((vh1 viola-harmonic) (bg2 flute-break-gliss)
		   &optional (scale 'chromatic-scale))
  (let* ((r1 (root-f vh1))
	 (r2 (root-f bg2))
	 (s1 (sounding-f vh1))
	 (intervals2 (append (before-f bg2) (after-f bg2))))
    (or (equal (freq-to-note r1 scale)
	       (freq-to-note r2 scale))
	(equal (freq-to-note s1 scale)
	       (freq-to-note r2 scale))
        (loop for interval in intervals2
	      thereis (or (<= (first interval)
			      r1
			      (second interval))
			  (<= (first interval)
			      s1
			      (second interval)))))))

(defmethod matchp ((bg1 flute-break-gliss) (vh2 viola-harmonic)
		   &optional (scale 'chromatic-scale))
  (matchp vh2 bg1 scale))

;; *** parse-to-event

(defmethod parse-to-event ((bg flute-break-gliss) &optional (start-time 1))
  (make-event
   (make-chord (flatten (append `(,(root bg)) (before bg) (after bg))))
   'q
   :start-time start-time))

(defmethod parse-to-event ((vh viola-harmonic) &optional (start-time 1))
  (make-event
   (make-chord `(,(root vh)
		 ,(midi-to-note (+ (interval vh) (note-to-midi (root vh))))
		 ,(sounding vh)))
   'q
   :start-time start-time))

;; *** MAKE
(defun make-flute-break-gliss (id root before after)
  (make-instance 'flute-break-gliss
		 :id id
		 :root root
		 :before before
		 :after after
		 :root-f (note-to-freq root)
		 :before-f (loop for option in before
				 collect
				 (sort (mapcar #'note-to-freq option) #'<))
		 :after-f (loop for option in after
				 collect
				 (sort (mapcar #'note-to-freq option) #'<))))

(defun get-interval-from-harmonic (root sounding)
  (let* ((diff (- (note-to-midi sounding) (note-to-midi root))))
    (cond ((= diff 19) 7)
	  ((= diff 24) 5)
	  ((= diff 28) 4)
	  ((= diff 31) 3)
	  (t (warn "weird interval: ~a ~a" root sounding)))))

(defun make-viola-harmonic (id root sounding saite)
  (make-instance 'viola-harmonic
		 :id (or id (format nil "~a-~a-~a" saite root sounding))
		 :root root
		 :sounding sounding
		 :saite saite
		 :root-f (note-to-freq root)
		 :sounding-f (note-to-freq sounding)
		 :interval (get-interval-from-harmonic root sounding)))

;; *** find-with-id
(defun find-with-id (id ls)
  (find (string id) ls :test #'(lambda (x y) (equal x (id y)))))

;; *** *gliss*
(defparameter *gliss*
  (loop for note in +gliss+ and i from 0
	collect
	(make-flute-break-gliss i (first note) (second note) (third note))))

;; *** viola

;;; all possible artificial viola harmonics:
(defun get-all-artificial-harmonics ()
  (loop for froms in '((f3 c4 g4 d5)(cs3 gs3 ds4 as4)(cs3 gs3 ds4 as4)(cs3 gs3 ds4 as4))
	for tos in '((b4 fs4 cs5 gs5)(c4 g4 d5 a5)(c4 g4 d5 a5)(c4 g4 d5 a5))
	for interval in '(19 24 28 31)
	append
	(loop for saite in '(4 3 2 1)
	      and from in froms
	      and to in tos
	      append (loop for note from (note-to-midi from) to (note-to-midi to)
			   collect (make-viola-harmonic nil
							(midi-to-note note)
							(midi-to-note (+ interval note))
							saite)))))

(defparameter *all-vla-harmonics* (get-all-artificial-harmonics))

;; *** notate
(defun notate (lst-of-harmonics file &optional (instrument 'viola))
  (unless (listp lst-of-harmonics) (error "not a list: ~a" lst-of-harmonics))
  (unless (listp (car lst-of-harmonics))
    (setf lst-of-harmonics (list lst-of-harmonics)))
  (let* ((events (loop for i in (flatten lst-of-harmonics)
		       collect (parse-to-event i)))
	 (letters (loop for i in lst-of-harmonics
			with n = 2
			collect n
			do (setf n (+ n (length i)))))
	 (len (length events))
	 (sc (make-slippery-chicken
              '+harmonics-to-notation+
              :ensemble `(((ins (,instrument :midi-channel 1))))
              :set-palette '((1 ((c4))))
              :set-map (loop for i from 1 to len collect `(,i (1)))
              :rthm-seq-palette '((1 ((((1 4) q)))))
	      :rehearsal-letters letters
              :rthm-seq-map (loop for i from 1 to len collect `(,i ((ins (1))))))))
    (map-over-events sc 0 nil 'ins
		     #'(lambda (e) (setf (pitch-or-chord e)
				    (pitch-or-chord (pop events)))))
    (write-xml sc :file file)))

;; ** compose

;; *** flute

;; trying to equalize the use of each break:
(defun compose-flute (first len lst-of-gliss)
  "Find a sequence of notes from lst-of-gliss for the desired length and 
   starting with 'first. For each pair of consecutive notes, #'matchp or 
   any-matchp should be true.
   Use both, matchp and any-matchp, to be more flexible and also
   have a chance to use notes, we would not get to with only matchp.
   Still we try to use matchp as often as possible."
  (labels ((match (x y)
	     (and (not (equal x y))
		  (matchp x y)))
	   (any-match (x y)
	     (and (not (equal x y))
		  (any-matchp x y)))
	   (sorter (x y) (< (times-used x) (times-used y)))
	   (options (pred)
	     (loop for gliss in lst-of-gliss
		   when (funcall pred first gliss)
		     collect gliss)))
    (loop for i from 0 below len
	  for options1 = (funcall #'options #'match)
	  for options2 = (funcall #'options #'any-match)
	  for options = (cond ((not options2) lst-of-gliss)
			      ((not options1) options2)
			      ((<= (loop for i in options1
					 minimize (times-used i))
				   (+ 1 (loop for i in options2
					 minimize (times-used i))))
			       options1)
			      (t options2))
	  for next = (first (sort options #'sorter))
	  collect first into result
	  do (setf first next)
	     (incf (times-used next))
	  finally (loop for i in lst-of-gliss do (setf (times-used i) 0))
		  (return result))))

(defparameter *flute*
  (loop for i in (compose-flute (first *gliss*) 30 *gliss*) collect i))

(defparameter *flute-id*
  (loop for i in (compose-flute (first *gliss*) 30 *gliss*)
	collect (id i)))

(notate *flute* (format nil "~aflute-gliss.xml" *target-dir*) 'flute)

;; *** viola

(defun match-streak (ls)
  (loop for k in ls and i from 0
	with options = *all-vla-harmonics*
	with last = '()
	when options
	  do (setf last (reverse options)
		   options '())
	     (loop for n in last
		   when (matchp k n) do (push n options))
	unless options do (return (values last i))
	  finally (return (values last (1+ i)))))

(defun get-timeline-track (start seq &optional get-objects)
  (let* ((streak (multiple-value-list (match-streak (subseq seq start)))))
    (if (car streak)
	(append (loop repeat start collect nil)
		(loop repeat (second streak)
		      collect (if get-objects (first streak) (length (first streak))))
		(loop repeat (- (length seq) (second streak) start) collect nil))
	(loop repeat (length seq) collect nil))))

(defun combine-timeline-tracks (tracks)
  (loop for i from 0 below (apply #'min (mapcar #'length tracks))
	collect (remove-duplicates
		 (loop for track in tracks
		       when (nth i track)
			 append (nth i track)))))

(defun print-timeline (ls &optional (stream t))
  (format stream "~{~a~%~}" ls))

(defun sort-harmonics (harmonics)
  (sort (sort harmonics #'(lambda (x y) (< (note-to-midi (root x))
				      (note-to-midi (root y)))))
	#'(lambda (x y) (> (saite x) (saite y)))))

#+nil(defun reduce-by-string (lst-of-optns string-env)
  (let* ((len (length lst-of-optns)))
    (loop for i from 0 and opt in lst-of-optns
	  for prog = (/ i (1- len))
	  for strings = (list (round (interpolate prog string-env)))
	  collect (loop for n in opt when (find (saite n) strings) collect n))))

#+nil(defun compose-viola (first len lst-of-hrm)
  "start with one string and during the duration 
(1/3length, 3/5length, 4/5length) start using other strings as well."
  )

(loop for i from 0 below (length *gliss*)
      collect (get-timeline-track i *gliss*))

(loop for i from 0 below (length *gliss*)
      collect (remove-duplicates
	       (loop for n in (match-streak (subseq *gliss* i))
		     collect (saite n))))

;; for the actual flute sequence:

(loop for i from 0 below (length *flute*)
      collect (get-timeline-track i *flute*))

(loop for i from 0 below (length *flute*)
      collect (remove-duplicates
	       (loop for n in (match-streak (subseq *flute* i))
		     collect (saite n))))

(defparameter *viola-options*
  (combine-timeline-tracks
   (loop for i from 0 below (1- (length *flute*))
	 collect (get-timeline-track i *flute* t))))

;; to XML
(notate
 (loop for bar in *viola-options*
       collect (sort-harmonics bar))
       (format nil "~aviola.xml" *target-dir*))

;; NOTE: this is only harmonics for viola, not gliss!

;; EOF sort-gliss.lsp
