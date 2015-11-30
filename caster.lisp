(load "angle.lisp")

(declaim (optimize (speed 3) (safety 0)))

(defconstant *screen-x* 320)
(defconstant *screen-y* 200)
(defconstant *tile-size* 32)
(defconstant *fov* (deg 60.0d0))
(defconstant *projection-distance* (/ (/ (/ *screen-x* 2.0d0)
					 (tan (/ (get-rad *fov*) 2.0d0)))
				      *tile-size*))
(defconstant *column-angle* (div-angle *fov* *screen-x*))

(defparameter *map*
  #((1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
    (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
    (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
    (1 0 0 0 1 1 1 1 1 0 0 0 0 0 0 1)
    (1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 1)
    (1 0 0 0 1 1 1 1 0 0 0 0 0 0 0 1)
    (1 0 0 0 0 0 0 1 0 0 0 1 1 1 0 1)
    (1 0 0 0 1 1 1 1 0 0 0 0 0 1 0 1)
    (1 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1)
    (1 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1)
    (1 0 0 0 0 1 1 1 1 1 1 0 0 0 0 1)
    (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
    (1 0 0 0 0 1 1 1 1 1 1 0 0 0 0 1)
    (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
    (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
    (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)))


(defun map-get (x y)
  (declare (type integer x y))
  (elt (elt *map* y) x))

(defun do-iterations (x y ax ay dx dy type)
  (declare (type double-float x y ax ay dx dy))
  (loop for i from 0 upto 19 do
       (let ((ix (+ ax (* i dx)))
	     (iy (+ ay (* i dy))))
	 (progn
	   (if (or (< ix 0) (< iy 0) (>= iy (length *map*)) (>= ix (length (elt *map* 0))))
	       (return-from do-iterations nil)
	       (let ((tilex (floor ix))
		     (tiley (floor iy)))
		 (cond ((eq type 'horizontal)
			(if (= (map-get tilex tiley) 0)
			    (setf tiley (- tiley 1))
			    nil))
		       ((eq type 'vertical)
			(if (and (> tilex 0) (not (eq (map-get (- tilex 1) tiley) 0)))
			    (setf tilex (- tilex 1))
			    nil))
		       (t (error "Can't get here")))
		 (if (not (eq (map-get tilex tiley) 0))
		     (return-from do-iterations (list :x ix :y iy
						      :tilex tilex :tiley tiley
						      :distance (sqrt (+ (expt (- x ix) 2)
									 (expt (- y iy) 2)))))
		     nil)))))))




(defun get-horizontal-intersection (angle x y)
  (let ((dx 0.0d0) (dy 0.0d0) (ax 0.0d0) (ay 0.0d0) (q (quadrant angle)))
    (cond ((eq q 'first)
	   (progn
	     (setf ay (ffloor y))
	     (setf ax (+ x
			 (/ (- y ay)
			    (tan (get-rad angle)))))
	     (setf dx (/ 1.0d0 (tan (get-rad angle))))
	     (setf dy -1.0d0)))
	  ((eq q 'first-second)
	   (progn
	     (setf ax x)
	     (setf ay (ffloor y))
	     (setf dx 0.0d0)
	     (setf dy -1.0d0)))
	  ((eq q 'second)
	   (progn
	     (setf ay (ffloor y))
	     (setf ax (+ x
			 (/ (- y ay)
			    (tan (get-rad angle)))))
	     (setf dx (/ 1.0d0 (tan (get-rad angle))))
	     (setf dy -1.0d0)))
	  ((eq q 'third)
	   (progn
	     (setf ay (fceiling y))
	     (setf ax (+ x
			 (/ (- y ay)
			    (tan (get-rad angle)))))
	     (setf dx (/ -1.0d0 (tan (get-rad angle))))
	     (setf dy 1.0d0)))
	  ((eq q 'third-fourth)
	   (progn
	     (setf ax x)
	     (setf ay (fceiling y))
	     (setf dx 0.0d0)
	     (setf dy 1.0d0)))
	  ((eq q 'fourth)
	   (progn
	     (setf ay (fceiling y))
	     (setf ax (+ x
			 (/ (- y ay)
			    (tan (get-rad angle)))))
	     (setf dx (/ -1.0d0 (tan (get-rad angle))))
	     (setf dy 1.0d0)))
	  (t (return-from get-horizontal-intersection nil)))
    (do-iterations x y ax ay dx dy 'horizontal)))


(defun get-vertical-intersection (angle x y)
  (let ((dx 0.0d0) (dy 0.0d0) (ax 0.0d0) (ay 0.0d0) (q (quadrant angle)))
    (cond ((eq q 'first)
	   (progn
	     (setf ax (fceiling x))
	     (setf ay (- y
			 (* (- ax x)
			    (tan (get-rad angle)))))
	     (setf dx 1.0d0)
	     (setf dy (- (tan (get-rad angle))))))
	  ((eq q 'second)
	   (progn
	     (setf ax (ffloor x))
	     (setf ay (- y
			 (* (- ax x)
			    (tan (get-rad angle)))))
	     (setf dx -1.0d0)
	     (setf dy (tan (get-rad angle)))))
	  ((eq q 'second-third)
	   (progn
	     (setf ax (ffloor x))
	     (setf ay y)
	     (setf dx -1.0d0)
	     (setf dy 0.0d0)))
	  ((eq q 'third)
	   (progn
	     (setf ax (ffloor x))
	     (setf ay (- y
			 (* (- ax x)
			    (tan (get-rad angle)))))
	     (setf dx -1.0d0)
	     (setf dy (tan (get-rad angle)))))
	  ((eq q 'fourth)
	   (progn
	     (setf ax (fceiling y))
	     (setf ay (- y
			 (* (- ax x)
			    (tan (get-rad angle)))))
	     (setf dx 1.0d0)
	     (setf dy (- (tan (get-rad angle))))))
	  ((eq q 'fourth-first)
	   (progn
	     (setf ax (fceiling x))
	     (setf ay y)
	     (setf dx 1.0d0)
	     (setf dy 0.0d0)))
	  (t (return-from get-vertical-intersection nil)))
    (do-iterations x y ax ay dx dy 'vertical)))

(defun cast-ray (angle x y)
  (declare (type double-float x y))
  (let ((hi (get-horizontal-intersection angle x y))
	(vi (get-vertical-intersection angle x y)))
    (cond ((and (not (null hi)) (not (null vi)))
	       (if (<= (getf hi :distance) (getf vi :distance))
		   (return-from cast-ray hi)
		   (return-from cast-ray vi)))
	  ((not (null hi)) (return-from cast-ray hi))
	  ((not (null vi)) (return-from cast-ray vi))
	  (t (error "Can't get here")))))


(defun do-raycast (angle x y)
  (declare (type double-float x y))
  (let ((col (add-angle angle (div-angle *fov* 2.0d0))))
    (loop for i from 0 upto (- *screen-x* 1) do
	 (let ((intersection (cast-ray col x y)))
	   (if (not (null intersection))
	       (let* ((normalized-distance (* (getf intersection :distance)
					      (cos (get-rad (sub-angle angle col)))))
		      (pheight (/ *projection-distance* normalized-distance)))
		 ;(format t "~{~a~^, ~} ~$~%" intersection (get-deg col))
		 ;(format t "~{~a~^, ~} ~$~%" (list normalized-distance pheight) (get-deg angle))
		 (setf col (sub-angle col *column-angle*))))))))

(defun main ()
       (do-raycast (deg 220.0d0) 10.0d0 4.0d0))

