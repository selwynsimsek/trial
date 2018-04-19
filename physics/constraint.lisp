#|
This file is a part of trial
(c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:org.shirakumo.fraf.trial.physics)


(defgeneric relax (constraint delta &optional preserve-impulse)
  (:documentation "Relaxes the constraint towards its resting position."))

(defclass distance-constraint ()
  ((point-a :initarg :point-a :reader point-a)
   (point-b :initarg :point-b :reader point-b)
   (tear :initarg :tear :accessor tear)
   (target :initform NIL :accessor target)
   (stiffness :initarg :stiffness :accessor stiffness))
  (:default-initargs :point-a (error "POINT-A required.")
                     :point-b (error "POINT-B required.")
                     :tear NIL
                     :stiffness 2.1))

(defmethod initialize-instance :after ((constraint distance-constraint) &key target)
  (when (v= (location (point-a constraint)) (location (point-b constraint)))
    (error "Point locations are the same!"))
  (setf (target constraint) (or target (vlength (v- (location (point-b constraint))
                                                    (location (point-a constraint)))))))

(defmethod relax ((constraint distance-constraint) delta &optional preserve-impulse)
  (declare (ignore delta preserve-impulse))
  (let* ((point-a (point-a constraint))
         (point-b (point-b constraint))
         (loc-a (location point-a))
         (loc-b (location point-b))
         (normal (v- loc-b loc-a))
         (distance (vlength normal))
         (diff (- distance (target constraint)))
         (total-mass (+ (mass point-a) (mass point-b)))
         (mass-a (if (< 0.01 total-mass) ;; tiny masses are worthless
                     (/ (mass point-a) total-mass)
                     1/2))
         (mass-b (if (< 0.01 total-mass)
                     (/ (mass point-b) total-mass)
                     1/2)))
    (when (and (< 0.01 (abs diff)) (< 0.01 (abs distance))) ;; ignore tiny sub-pixels
      (let* ((move (v* normal (/ diff (* distance (stiffness constraint)))))
             (move-a (v* move mass-a))
             (move-b (v* move mass-b -1)))
        (nv+ (location point-a) move-a)
        (nv+ (old-location point-a) move-a)
        (nv+ (location point-b) move-b)
        (nv+ (old-location point-b) move-b)))))

(defclass pin-constraint ()
  ((point :initarg :point :reader point)
   (location :initarg :location :accessor location))
  (:default-initargs :point (error "POINT required.")))

(defmethod relax ((constraint pin-constraint) delta &optional preserve-impulse)
  (declare (ignore delta preserve-impulse))
  (let ((point (point constraint))
        (pin-to (location constraint)))
    (setf (location point) pin-to
          (old-location point) pin-to)))

(defclass angle-constraint ()
  ((point-a :initarg :point-a :accessor point-a)
   (point-b :initarg :point-b :accessor point-b)
   (point-c :initarg :point-c :accessor point-c)
   (stiffness :initarg :stiffness :accessor stiffness)
   (angle :initform NIL :accessor angle))
  (:default-initargs :point-a (error "POINT-A required.")
                     :point-b (error "POINT-B required.")
                     :point-c (error "POINT-C required.")
                     :stiffness 1.0))

(defmethod initialize-instance :after ((constraint angle-constraint) &key angle)
  (setf (angle constraint) (or angle (vangle (location (point-a constraint))
                                             (location (point-b constraint))
                                             (location (point-c constraint))))))


(defmethod relax ((constraint angle-constraint) delta &optional preserve-impulse)
  (declare (ignore preserve-impulse))
  (let* ((point-a (location (point-a constraint)))
         (point-b (location (point-b constraint)))
         (point-c (location (point-c constraint)))
         (angle (vangle point-a point-b point-c))
         (diff (- angle (angle constraint))))
    (cond
      ((<= diff (- PI))
       (incf diff (* 2 PI)))
      ((<= PI diff)
       (incf diff (* -2 PI))))
    (unless (< 0.01 (abs diff)) ;; Ignore tiny sub-pixels
      (let ((diff (* diff delta (stiffness constraint))))
        (setf (location point-a) (vrot (location point-a)
                                       (location point-b)
                                       diff)
              (location point-c) (vrot (location point-c)
                                       (location point-b)
                                       (- diff))
              (location point-b) (vrot (location point-b)
                                       (location point-a)
                                       diff)
              (location point-b) (vrot (location point-b)
                                       (location point-c)
                                       (- diff)))))))

(defclass frame-constraint ()
  ((point :initarg :point :accessor point)
   (min-point :initarg :min :accessor min-point)
   (max-point :initarg :max :accessor max-point))
  (:default-initargs :point (error "POINT required.")
                     :min NIL
                     :max NIL))

(defmethod relax ((constraint frame-constraint) delta &optional preserve-impulse)
  (declare (ignore delta))
  (let* ((point (point constraint))
         (loc (location point))
         (min (ensure-vector-type (or (min-point constraint) loc)
                                  (type-of loc)
                                  loc))
         (max (ensure-vector-type (or (max-point constraint) loc)
                                  (type-of loc)
                                  loc))
         (move-to 
           (etypecase loc
             (vec2 (vec2 (min (vx max) (max (vx min) (vx loc)))
                         (min (vy max) (max (vy min) (vy loc)))))
             (vec3 (vec3 (min (vx max) (max (vx min) (vx loc)))
                         (min (vy max) (max (vy min) (vy loc)))
                         (min (vz max) (max (vz min) (vz loc)))))
             (vec4 (vec4 (min (vx max) (max (vx min) (vx loc)))
                         (min (vy max) (max (vy min) (vy loc)))
                         (min (vz max) (max (vz min) (vz loc)))
                         (min (vw max) (max (vw min) (vw loc))))))))
    (when (v/= move-to loc)
      (if preserve-impulse
          (let ((velocity (v* (v- (old-location point) loc) 0.5))) ;; 0.5 for damping
            (setf (location point) move-to
                  (old-location point) (v- (location point) velocity)))
          (setf (location point) move-to)))))

(defun ensure-constraint (type)
  (etypecase type
    (keyword (ecase type
               (:distance 'distance-constraint)
               (:pin 'pin-constraint)
               (:angle 'angle-constraint)
               (:frame 'frame-constraint)))
    (symbol (ecase type
              (distance 'distance-constraint)
              (pin 'pin-constraint)
              (angle 'angle-constraint)
              (frame 'frame-constraint)
              (T type)))))