(defun deg-to-rad (a)
  (* a (/ PI 180)))

(defun rad-to-deg (a)
  (* a (/ 180 PI)))

(defclass angle ()
  ((angle
    :initarg :angle
    :initform (error "Angle should be specified"))))

(defmethod initialize-instance :after ((angle angle) &key)
  (setf (slot-value angle 'angle) (cap-rad (get-rad angle))))

(defun new-angle (a)
  (make-instance 'angle :angle a))

(defun deg (a)
  (new-angle (deg-to-rad a)))

(defun rad (a)
  (new-angle a))

(defun get-rad (angle)
  (slot-value angle 'angle))

(defun get-deg (angle)
  (rad-to-deg (slot-value angle 'angle)))

(defun cap-rad (a)
  (let ((cap (mod a (* 2 PI))))
    (return-from cap-rad cap)))

(defun cap-deg (a)
  (rad-to-deg (cap-rad (deg-to-rad a))))

(defun quadrant (angle)
  (let ((a (get-deg angle)))
    (cond ((and (> a 0) (< a 90)) 'first)
	  ((= a 90) 'first-second)
	  ((and (> a 90) (< a 180) 'second))
	  ((= a 180) 'second-third)
	  ((and (> a 180) (< a 270)) 'third)
	  ((= a 270) 'third-fourth)
	  ((and (> a 270) (< a 360)) 'fourth)
	  ((or (= a 0) (= a 360)) 'fourth-first) ; Normally a is capped between 0-359.99..., but check for 360 because of rounding
	  (t (error "Can't get here")))))
    

(defun add-angle (angle other)
  (rad (+ (slot-value angle 'angle) (slot-value other 'angle))))

(defun sub-angle (angle other)
  (rad (- (slot-value angle 'angle) (slot-value other 'angle))))

(defun mul-angle (angle m)
  (rad (* (get-rad angle) m)))

(defun div-angle (angle m)
  (rad (/ (get-rad angle) m)))
