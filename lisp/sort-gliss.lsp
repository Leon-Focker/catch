;; * sort-gliss.lsp

(in-package :ly)

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

(defclass gliss-break ()
  ((id :accessor id :initarg :id :initform nil)
   (root :accessor root :initarg :root :initform nil)
   (before :accessor before :initarg :before :initform nil)
   (after :accessor after :initarg :after :initform nil)
   (root-f :accessor root-f :initarg :root-f :initform nil)
   (before-f :accessor before-f :initarg :before-f :initform nil)
   (after-f :accessor after-f :initarg :after-f :initform nil)
   (times-used :accessor times-used :initform 0)))

;; *** printing 
(defmethod print-object :after ((gb gliss-break) stream)
  (format stream "~&~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ~%"))

(defmethod print-object ((gb gliss-break) stream)
  (format stream "~&GLISS-BREAK ~&id: ~a, ~&root: ~a, ~&before-break: ~a, ~&after-break: ~a"
          (id gb)
	  (root gb)
	  (before gb)
	  (after gb)))

;; *** any-matchp
;;; check if root is the same or if the top and bottom frequency of each
;;; gliss can be found within a gliss of the other option.
(defmethod any-matchp ((gb1 gliss-break) (gb2 gliss-break))
  (let* ((r1 (root-f gb1))
	 (r2 (root-f gb2))
	 (intervals1 (append (before-f gb1) (after-f gb1)))
	 (intervals2 (append (before-f gb2) (after-f gb2))))
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
(defmethod matchp ((gb1 gliss-break) (gb2 gliss-break)
			    &optional (scale 'quarter-tone))
  (let* ((r1 (root-f gb1))
	 (r2 (root-f gb2))
	 (break1 (append (loop for b in (before-f gb1) collect (first b))
			 (loop for a in (after-f gb1) collect (second a))))
	 (break2 (append (loop for b in (before-f gb2) collect (first b))
			 (loop for a in (after-f gb2) collect (second a)))))
    ;; predicate for checking, if two notes are the same, depending on the scale:
    (flet ((samenotep (a b)
	     (equal (freq-to-note a scale)
		    (freq-to-note b scale))))
      (or (samenotep r1 r2)
	  (loop for note in break1
		thereis (find note break2 :test #'samenotep))))))

(defun make-gliss-break (id root before after)
  (make-instance 'gliss-break
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

;; *** *gliss*
(defparameter *gliss*
  (loop for note in +gliss+ and i from 0
	collect
	(make-gliss-break i (first note) (second note) (third note))))

;; ** compose

;; using the same predicate all the time:
#+nil(defun compose-gliss (first len lst-of-gliss &optional (pred #'matchp))
  (labels ((match (x y)
	     (and (not (equal x y))
		  (funcall pred x y)))
	   (sorter (x y) (< (times-used x) (times-used y)))
	   (options ()
	     (loop for gliss in lst-of-gliss
		   when (funcall #'match first gliss)
		     collect gliss)))
    (loop for i from 0 below len
	  for next = (first (sort (funcall #'options) #'sorter))
	  collect first into result
	  do (setf first next)
	     (incf (times-used next))
	  finally (loop for i in lst-of-gliss do (setf (times-used i) 0))
		  (return result))))

;; trying to equalize the use of each break:
(defun compose-gliss (first len lst-of-gliss)
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

(defparameter *seq1*
  (loop for i in (compose-gliss (first *gliss*) 30 *gliss*)
	collect (id i)))

;; EOF sort-gliss.lsp
