#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defun check-integer-size (thing size &optional unsigned)
  (declare (type (unsigned-byte 8) size))
  (declare (optimize speed))
  (if unsigned
      (unless (<= 0 thing (expt 2 size))
        (error "~a does not fit within [0,2^~a]." thing size))
      (let ((size (1- size)))
        (unless (<= (- (expt 2 size)) thing (1- (expt 2 size)))
          (error "~a does not fit within [-2^~a,2^~:*~a-1]." thing size)))))

(define-constant-fold-function cl-type->gl-type (type)
  (cond ((eql type 'fixnum) :int)
        ((subtypep type '(signed-byte 8)) :char)
        ((subtypep type '(unsigned-byte 32)) :uint)
        ((subtypep type '(signed-byte 32)) :int)
        ((subtypep type '(unsigned-byte 64)) :ulong)
        ((subtypep type '(signed-byte 64)) :long)
        ((subtypep type 'single-float) :float)
        ((subtypep type 'double-float) :double)
        ((eql type 'vec2) :vec2)
        ((eql type 'vec3) :vec3)
        ((eql type 'vec4) :vec4)
        ((eql type 'mat2) :mat2)
        ((eql type 'mat3) :mat3)
        ((eql type 'mat4) :mat4)
        (T (error "Don't know how to convert ~s to a GL type." type))))

(define-constant-fold-function gl-type->cl-type (type)
  (ecase type
    ((:boolean :ubyte :unsigned-byte :unsigned-char) '(unsigned-byte 8))
    ((:byte :char) '(signed-byte 8))
    ((:ushort :unsigned-short) '(unsigned-byte 16))
    (:short '(signed-byte 16))
    ((:uint :unsigend-int) '(unsigned-byte 32))
    ((:int :fixed :sizei :enum :bitfield) '(signed-byte 32))
    ((:uint64 :ulong :unsigned-long) '(unsigned-byte 64))
    ((:int64 :long) '(signed-byte 64))
    ((:half :half-float) 'short-float)
    ((:float :clampf) 'single-float)
    ((:double :clampd) 'double-float)
    (:vec2 'vec2)
    (:vec3 'vec3)
    (:vec4 'vec4)
    (:mat2 'mat2)
    (:mat3 'mat3)
    (:mat4 'mat4)))

(defun gl-coerce (thing type)
  (declare (optimize speed))
  (ecase type
    ((:double :double-float)
     (float thing 0.0d0))
    ((:float :single-float)
     (float thing 0.0f0))
    ((:int)
     #-elide-coercion-size-checks
     (check-integer-size thing 32)
     (values (round thing)))
    ((:uint :unsigned-int)
     #-elide-coercion-size-checks
     (check-integer-size thing 32 T)
     (values (round thing)))
    ((:char :byte)
     #-elide-coercion-size-checks
     (check-integer-size thing 8)
     (values (round thing)))
    ((:uchar :unsigned-char :unsigned-byte)
     #-elide-coercion-size-checks
     (check-integer-size thing 8 T)
     (values (round thing)))))

(define-compiler-macro gl-coerce (&whole whole &environment env thing type)
  (if (constantp type env)
      `(funcall (load-time-value
                 (ecase ,type
                   ((:double :double-float)
                    (lambda (thing) (float thing 0.0d0)))
                   ((:float :single-float)
                    (lambda (thing) (float thing 0.0f0)))
                   ((:int)
                    (lambda (thing)
                      #-elide-coercion-size-checks
                      (check-integer-size thing 32)
                      (values (round thing))))
                   ((:uint :unsigned-int)
                    (lambda (thing)
                      #-elide-coercion-size-checks
                      (check-integer-size thing 32 T)
                      (values (round thing))))
                   ((:char :byte)
                    (lambda (thing)
                      #-elide-coercion-size-checks
                      (check-integer-size thing 8)
                      (values (round thing))))
                   ((:uchar :unsigned-char :unsigned-byte)
                    (lambda (thing)
                      #-elide-coercion-size-checks
                      (check-integer-size thing 8 T)
                      (values (round thing))))))
                ,thing)
      whole))

(define-constant-fold-function gl-type-size (type)
  (ecase type
    (:boolean 1)
    ((:ubyte :unsigned-byte :byte :char) 1)
    ((:ushort :unsigned-short :short) 2)
    ((:uint :unsigned-int :int) 4)
    (:fixed 4)
    ((:ulong :unsigned-long :uint64 :int64) 8)
    (:sizei 4)
    (:enum 4)
    ((:intptr :sizeiptr :sync) #+x86 4 #+x86-64 8)
    (:bitfield 4)
    ((:half :half-float) 2)
    ((:float :clampf) 4)
    ((:double :clampd) 8)
    (:vec2 (* 2 4))
    (:vec3 (* 3 4))
    (:vec4 (* 4 4))
    (:mat2 (* 2 2 4))
    (:mat3 (* 3 3 4))
    (:mat4 (* 4 44))))

(defgeneric base-alignment (type standard))
(defgeneric base-allocation (type standard base))

(defmethod base-alignment ((type (eql :int)) (standard (eql :std140))) 4)
(defmethod base-alignment ((type (eql :bool)) (standard (eql :std140))) 4)
(defmethod base-alignment ((type (eql :float)) (standard (eql :std140))) 4)
(defmethod base-alignment ((type (eql :vec2)) (standard (eql :std140))) 8)
(defmethod base-alignment ((type (eql :vec3)) (standard (eql :std140))) 16)
(defmethod base-alignment ((type (eql :vec4)) (standard (eql :std140))) 16)
(defmethod base-alignment ((type (eql :mat2)) (standard (eql :std140))) (base-alignment :vec4 standard))
(defmethod base-alignment ((type (eql :mat3)) (standard (eql :std140))) (base-alignment :vec4 standard))
(defmethod base-alignment ((type (eql :mat4)) (standard (eql :std140))) (base-alignment :vec4 standard))
(defmethod base-alignment ((type cons) (standard (eql :std140))) (base-alignment :vec4 standard))

(defmethod base-allocation ((type (eql :int)) (standard (eql :std140)) base) 4)
(defmethod base-allocation ((type (eql :bool)) (standard (eql :std140)) base) 4)
(defmethod base-allocation ((type (eql :float)) (standard (eql :std140)) base) 4)
(defmethod base-allocation ((type (eql :vec2)) (standard (eql :std140)) base) 8)
(defmethod base-allocation ((type (eql :vec3)) (standard (eql :std140)) base) 16)
(defmethod base-allocation ((type (eql :vec4)) (standard (eql :std140)) base) 16)
(defmethod base-allocation ((type (eql :mat2)) (standard (eql :std140)) base) (base-allocation '(:array :vec4 2) standard base))
(defmethod base-allocation ((type (eql :mat3)) (standard (eql :std140)) base) (base-allocation '(:array :vec4 3) standard base))
(defmethod base-allocation ((type (eql :mat4)) (standard (eql :std140)) base) (base-allocation '(:array :vec4 4) standard base))
(defmethod base-allocation ((type cons) (standard (eql :std140)) base)
  (ecase (first type)
    (:struct (base-allocation (second type) standard base))
    (:array (destructuring-bind (identifier type count) type
              (declare (ignore identifier))
              (if (listp type)
                  (* count (base-allocation type base))
                  (round-to
                   (base-alignment :vec4 standard)
                   (* count (round-to (base-alignment :vec4 standard)
                                      (base-alignment type standard)))))))))

(defun gl-memref-std140 (ptr type)
  (flet ((%ref (i)
           (cffi:mem-aref ptr :float i)))
    (declare (inline %ref))
    (ecase type
      (:int (cffi:mem-ref ptr :int))
      (:uint (cffi:mem-ref ptr :uint))
      (:float (cffi:mem-ref ptr :float))
      (:double (cffi:mem-ref ptr :double))
      (:vec2 (vec2 (%ref 0) (%ref 1)))
      (:vec3 (vec3 (%ref 0) (%ref 1) (%ref 2)))
      (:vec4 (vec4 (%ref 0) (%ref 1) (%ref 2) (%ref 3)))
      (:mat2 (mat2 (%ref 0) (%ref 1)
                   (%ref 4) (%ref 5)))
      (:mat3 (mat3 (%ref 0) (%ref 1) (%ref 2)
                   (%ref 4) (%ref 5) (%ref 6)
                   (%ref 8) (%ref 9) (%ref 10)))
      ;; FIXME: could copy raw array with memcpy.
      (:mat4 (mat4 (%ref 0) (%ref 1) (%ref 2) (%ref 3)
                   (%ref 4) (%ref 5) (%ref 6) (%ref 7)
                   (%ref 8) (%ref 9) (%ref 10) (%ref 11))))))

(defun gl-memref (ptr type &key (layout :std140))
  (ecase layout
    (:std140 (gl-memref-std140 ptr type))
    ;; TODO: (:std430)
    ))

(defun (setf gl-memref-std140) (value ptr type)
  (flet (((setf %ref) (value i)
           (setf (cffi:mem-aref ptr :float i) value)))
    (declare (inline (setf %ref)))
    (ecase type
      (:int
       (setf (cffi:mem-ref ptr :int) value))
      (:uint
       (setf (cffi:mem-ref ptr :uint) value))
      (:float
       (setf (cffi:mem-ref ptr :float) value))
      (:double
       (setf (cffi:mem-ref ptr :double) value))
      (:vec2
       (setf (%ref 0) (vx2 value))
       (setf (%ref 1) (vy2 value)))
      (:vec3
       (setf (%ref 0) (vx3 value))
       (setf (%ref 1) (vy3 value))
       (setf (%ref 2) (vz3 value)))
      (:vec4
       (setf (%ref 0) (vx4 value))
       (setf (%ref 1) (vy4 value))
       (setf (%ref 2) (vz4 value))
       (setf (%ref 3) (vw4 value)))
      (:mat2
       (loop with idx = #(0 1 4 5)
             for i from 0 below 4
             do (setf (%ref (aref idx i)) (miref2 value i))))
      (:mat3
       (loop with idx = #(0 1 2 4 5 6 8 9 10)
             for i from 0 below 9
             do (setf (%ref (aref idx i)) (miref3 value i))))
      (:mat4
       ;; FIXME: could copy raw array with memcpy.
       (loop for i from 0 below 16
             do (setf (%ref i) (miref4 value i))))))
  value)

(defun (setf gl-memref) (value ptr type &key (layout :std140))
  (ecase layout
    (:std140 (setf (gl-memref-std140 ptr type) value))
    ;; TODO: (:std430)
    ))
