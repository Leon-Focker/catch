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
   (after-f :accessor after-f :initarg :after-f :initform nil)))

;; *** printing 
(defmethod print-object :after ((gb gliss-break) stream)
  (format stream "~&~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~%"))

(defmethod print-object ((gb gliss-break) stream)
  (format stream "~&GLISS-BREAK ~&id: ~a, ~&root: ~a, ~&before-break: ~a, ~
                     ~&after-break: ~a"
          (id gb)
	  (root gb)
	  (before gb)
	  (after gb)))

;; *** check-for-any-match
;;; check if root is the same or if the top and bottom frequency of each
;;; gliss can be found within a gliss of the other option.
(defmethod check-for-any-match ((gb1 gliss-break) (gb2 gliss-break))
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

;; *** check-for-match
;;; only check if the root note or any of the starting and ending notes
;;; of the breaks match - also depends on the scale (quarter or chromatic).
(defmethod check-for-match ((gb1 gliss-break) (gb2 gliss-break)
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

()

;; EOF sort-gliss.lsp
