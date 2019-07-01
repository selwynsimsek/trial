#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defvar *gl-structs* (make-hash-table :test 'eq))

(defclass gl-declaration ()
  ((name :initarg :name :accessor name)
   (gl-name :initarg :gl-name :accessor gl-name)
   (gl-type :initarg :type :accessor gl-type)
   (qualifiers :initarg :qualifiers :accessor qualifiers)
   (layout :initarg :layout :accessor layout))
  (:default-initargs
   :name (error "NAME required.")
   :type (error "TYPE required.")
   :qualifiers ()
   :layout NIL))

(defmethod initialize-instance :after ((gl-declaration gl-declaration) &key name gl-name)
  (unless gl-name
    (setf (gl-name gl-declaration) (cffi:translate-underscore-separated-name name))))

(defmethod print-object ((gl-declaration gl-declaration) stream)
  (print-unreadable-object (gl-declaration stream :type T)
    (format stream "~s" (name gl-declaration))))

(defmethod compute-dependant-types ((gl-declaration gl-declaration))
  (labels ((compute (type)
             (when (listp type)
               (ecase (first type)
                 (:struct
                  (list (second type)))
                 (:array
                  (compute (second type)))))))
    (compute (gl-type gl-declaration))))

(defmethod gl-source ((gl-declaration gl-declaration))
  `(glsl-toolkit:struct-declarator
    (glsl-toolkit:type-qualifier
     ,@(when (layout gl-declaration)
         `((glsl-toolkit:layout-qualifier
            ,@(loop for id in (enlist (layout gl-declaration))
                    collect `(glsl-toolkit:layout-qualifier-id ,@(enlist id))))))
     ,@(qualifiers gl-declaration))
    (glsl-toolkit:type-specifier
     ,@(labels ((translate-type (type)
                  (etypecase type
                    (cons
                     (ecase (first type)
                       (:struct (list (gl-type (gl-struct (second type)))))
                       (:array (append (translate-type (second type))
                                       `((glsl-toolkit:array-specifier ,(third type)))))))
                    (symbol (list type)))))
         (translate-type (gl-type gl-declaration))))
    ,(gl-name gl-declaration)))

(defclass gl-struct ()
  ((name :initarg :name :accessor name)
   (gl-type :initarg :type :accessor gl-type)
   (fields :initarg :fields :accessor fields))
  (:default-initargs
   :name (error "NAME required.")
   :gl-type NIL
   :fields (error "FIELDS required.")))

(defmethod initialize-instance :after ((gl-struct gl-struct) &key name gl-type)
  (unless gl-type
    (setf (gl-type gl-struct) (cffi:translate-camelcase-name name :upper-initial-p T))))

(defmethod print-object ((gl-struct gl-struct) stream)
  (print-unreadable-object (gl-struct stream :type T)
    (format stream "~s" (name gl-struct))))

(defmethod gl-source ((gl-struct gl-struct))
  `(glsl-toolkit:struct-declaration
    ,(gl-type gl-struct)
    ,@(mapcar #'gl-source (fields gl-struct))))

(defun gl-struct (name &optional (errorp T))
  (or (gethash name *gl-structs*)
      (when errorp (error "No gl-struct named ~s is defined." name))))

(defun (setf gl-struct) (struct name)
  (check-type struct gl-struct)
  (setf (gethash name *gl-structs*) struct))

(defun remove-gl-struct (name)
  (remhash name *gl-structs*))

(defmethod compute-dependant-types ((gl-struct gl-struct))
  (mapcan #'compute-dependant-types (fields gl-struct)))

(defun translate-gl-struct-field-info (fields)
  (loop for field in fields
        collect (destructuring-bind (name type &key gl-name qualifiers layout count) field
                  `(make-instance 'gl-declaration :name ',name
                                                  :type ,(if count
                                                             `(list :array ',type ,count)
                                                             `',type)
                                                  :gl-name ',gl-name
                                                  :qualifiers ',qualifiers
                                                  :layout ,layout))))

(defmacro define-gl-struct (name/options &body fields)
  (destructuring-bind (name &rest initargs &key (type 'gl-struct))
      (enlist name/options)
    (let ((initargs (copy-list initargs)))
      (remf initargs :type)
      `(setf (gl-struct ',name)
             (make-instance ',type
                            :name ',name
                            :fields (list ,@(translate-gl-struct-field-info fields))
                            ,@initargs)))))

(defmethod compute-offsets ((struct gl-struct) (layout (eql :std140)))
  ;; TODO: this
  (error "Not implemented"))

(defmethod compute-offsets ((struct gl-struct) (layout (eql :std430)))
  ;; TODO: this
  (error "Not implemented"))
