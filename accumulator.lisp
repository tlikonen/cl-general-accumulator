;;; GENERAL-ACCUMULATOR
;;
;; A general-purpose, extensible value accumulator library for the
;; Common Lisp language.
;;
;; Author: Teemu Likonen <tlikonen@iki.fi>
;;
;; License: Public domain
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

(defpackage #:general-accumulator
  (:nicknames #:genacc)
  (:use #:cl)
  (:export #:initialize #:accumulate #:value #:with-accumulator))

(in-package :general-accumulator)


;;; Classes

(defgeneric value (accumulator)
  (:documentation "Return the accumulated value of ACCUMULATOR object."))

(defclass accumulator ()
  ((value :initarg :value :reader value)))

(defclass vector-accumulator (accumulator) nil)
(defclass special-vector-accumulator (vector-accumulator) nil)

(defclass list-accumulator (accumulator)
  ((last-cons :initarg :last-cons)))

(defclass reduce-accumulator (accumulator)
  ((function :initarg :function)))


;;; INITIALIZE

(defgeneric initialize (object &key &allow-other-keys)
  (:documentation
   "Return an accumulator object which is used to keep the information
of an accumulation process.

OBJECT argument can anything and its primary purpose is a method
dispatching: different classes of OBJECTs establish different kind of
accumulators. Methods can use OBJECT's value too, as well as any keyword
arguments passed to the generic function.

Methods should return an object, usually an instance of some class. That
object can later be used with generic functions ACCUMULATE and VALUE."))

(defmethod initialize ((object vector) &key)
  (assert (array-has-fill-pointer-p object) nil
          "The vector must have a fill pointer.")
  (make-instance 'vector-accumulator :value object))

(defmethod initialize ((object (eql :vector)) &key)
  (make-instance 'vector-accumulator
                 :value (make-array 0 :element-type t
                                    :adjustable t :fill-pointer 0)))

(defmethod initialize ((object (eql :string)) &key)
  (make-instance 'special-vector-accumulator
                 :value (make-array 0 :element-type 'character
                                    :adjustable t :fill-pointer 0)))

(defmethod initialize ((object (eql :bit-vector)) &key)
  (make-instance 'special-vector-accumulator
                 :value (make-array 0 :element-type 'bit
                                    :adjustable t :fill-pointer 0)))

(defmethod initialize ((object list) &key)
  (make-instance 'list-accumulator :value object :last-cons (last object)))

(defmethod initialize ((object (eql :list)) &key)
  (make-instance 'list-accumulator :value nil :last-cons nil))

(defmethod initialize ((function function) &key)
  (make-instance 'reduce-accumulator :function function))


;;; ACCUMULATE

(defgeneric accumulate (accumulator object)
  (:documentation
   "Accumulate OBJECT to ACCUMULATOR instance. Methods of this generic
function should specialize at least on the first argument (ACCUMULATOR)
and they should accumulate the second argument (OBJECT) to the
accumulator object."))

(defmethod accumulate :around ((acc accumulator) item)
  (call-next-method)
  item)

(defmethod accumulate ((acc vector-accumulator) item)
  (vector-push-extend item (value acc)))

(defmethod accumulate ((acc special-vector-accumulator) (item vector))
  (loop :for i :across item
        :do (vector-push-extend i (value acc))))

(defmethod accumulate ((acc special-vector-accumulator) (item cons))
  (dolist (i item)
    (vector-push-extend i (value acc))))

(defmethod accumulate ((acc list-accumulator) item)
  (let ((new-cons (cons item nil)))
    (with-slots (value last-cons) acc
      (if (and value last-cons)
          (setf (cdr last-cons) new-cons)
          (setf value new-cons))
      (setf last-cons new-cons))))

(defmethod accumulate ((acc reduce-accumulator) item)
  (with-slots (function value) acc
    (setf value (if (slot-boundp acc 'value)
                    (funcall function value item)
                    item))))


;;; The macro

(defmacro with-accumulator ((name object &rest keyword-arguments) &body body)
  "Create a local function NAME for handling an accumulation of type
OBJECT. Execute BODY forms and return the value of the last form.

This macro uses generic functions to handle the accumulation. There are
some built-in methods defined for common use-cases (see below) but user
can add more methods and therefore any kind of accumulation is possible.

First a new accumulator object is created with the generic function
INITIALIZE. The OBJECT argument (evaluated) and optional
KEYWORD-ARGUMENTS (evaluated) are passed to INITIALIZE and it should
return an accumulator object that stores the state of the accumulation.

Then a local function NAME is created for simple accumulation. The
function can optionally take one argument which is an object to be
accumulated. The generic function ACCUMULATE is used to handle the
accumulation. The return value of the local function comes from the
generic function ACCUMULATE. The built-in accumulators return the input
argument.

If the local function is called without arguments then the generic
function VALUE is called. It should return the currently accumulated
value.


Built-in accumulators
---------------------

OBJECT argument is used to define the type of accumulation process.
There are several built-in types:

:LIST

    Creates a list collector. Each accumulated object is collected to a
    list. Example:

        GENACC> (with-accumulator (collect :list)
                  (collect 1) (collect 2) (collect 3)
                  (collect))
        (1 2 3)

    The collecting is done destructively. The applicable ACCUMULATE
    method maintains a pointer to the last cons cell of the list and
    each time modifies its CDR value to point to a new cons cell.

[a list]

    If OBJECT is of type LIST then new elements are collected at the
    end. Example:

        GENACC> (with-accumulator (collect (list 1 2 3))
                  (collect 4) (collect 5)
                  (collect))
        (1 2 3 4 5)

    This is a destructive operation. The CDR value of the last cons cell
    of the original list is modified and linked to a new cons cell.

:VECTOR

    Creates a general vector collector. It creates an adjustable vector
    with a fill pointer 0 and element type T. New elements are pushed to
    that vector with CL:VECTOR-PUSH-EXTEND function. Example:

        GENACC> (with-accumulator (collect :vector)
                  (collect \"first\") (collect \"second\")
                  (collect))
        #(\"first\" \"second\")

:STRING

    This is similar to :VECTOR but the element type is CHARACTER. The
    underlying ACCUMULATE methods can take a single CHARACTER or a
    sequence of CHARACTERs as the argument. Example:

        GENACC> (with-accumulator (collect :string)
                  (collect #\\a)
                  (collect \"bcd\")
                  (collect #(#\\e #\\f))
                  (collect '(#\\g #\\h #\\i))
                  (collect))
        \"abcdefghi\"

:BIT-VECTOR

    This is similar to :STRING but the element type is BIT. The argument
    for the accumulator function can a BIT or a sequence of BITs.

[a vector]

    If OBJECT is of type VECTOR which satisfies the test
    ARRAY-HAS-FILL-POINTER-P then that vector is appended starting from
    its current fill pointer.

        GENACC> (with-accumulator
                    (collect (make-array 2 :fill-pointer 2 :adjustable t
                                         :initial-contents (vector 1 2)))
                  (collect 3)
                  (collect 4)
                  (collect))
        #(1 2 3 4)

    Note that if the vector is not adjustable then the accumulator may
    reach vector's limits and CL:VECTOR-PUSH-EXTEND signals an error.

[a function]

    If OBJECT is of type FUNCTION then the accumulator behaves like the
    CL:REDUCE function: all accumulated objects are combined into one by
    calling the given reducer function. Examples:

        GENACC> (with-accumulator (summing #'+)
                  (summing 5) (summing 7) (summing 11)
                  (summing))
        23

        GENACC> (with-accumulator (nc #'nconc)
                  (nc (list 1 2 3))
                  (nc (list 4 5 6))
                  (nc (list 7 8 9))
                  (nc))
        (1 2 3 4 5 6 7 8 9)

        GENACC> (with-accumulator (early-char (lambda (a b)
                                                (if (char< a b) a b)))
                  (early-char #\\o)
                  (early-char #\\b)
                  (early-char #\\s)
                  (early-char))
        #\\b

    Note that a single reduce operation for a sequence is probably
    faster with just CL:REDUCE function than with WITH-ACCUMULATOR
    macro. Therefore, the macro could be used only for collecting values
    into a sequence and CL:REDUCE would be used for the actual reduce
    operation. On the other hand, an advantage of doing all the reducing
    work with WITH-ACCUMULATOR is that the macro does it one step at the
    time. Intermediate results of the reducing are always available.


Adding a custom accumulator
---------------------------

The whole accumulation process is handled by three generic functions:
INITIALIZE, ACCUMULATE and VALUE. Writing new methods for those
functions allow adding any kind of accumulators. The following example
adds an accumulator which calculates the arithmetic mean of accumulated
numbers.

First we define a class whose instances will keep the state of the
accumulator. In this case we need to store the sum and the count of
accumulated numbers so we create slots for them.

    (defclass mean-accumulator ()
      ((sum :initform 0)
       (count :initform 0)))

Then we add a method for initializing an instance of the class. The
generic function INITIALIZE is used for that. It is called with the
OBJECT argument of WITH-ACCUMULATOR macro and with optional
KEYWORD-ARGUMENTS. In this example we use an EQL specializer for
symbol :MEAN. We don't use any keyword arguments so there's just empty
&key at the end of the lambda list.

    (defmethod genacc:initialize ((type (eql :mean)) &key)
      (make-instance 'mean-accumulator))

Now we create a method for generic function ACCUMULATE. The function is
called with two arguments: (1) the accumulator object created by
INITIALIZE and (2) the object that is meant to be accumulated. This
method specializes on our MEAN-ACCUMULATOR class as well as on number
class. The number is added to the previous value and the count is
increased by one.

    (defmethod genacc:accumulate ((object mean-accumulator)
                                  (number number))
      (with-slots (sum count) object
        (incf sum number)
        (incf count 1)))

For returning the accumulated mean value we create a method for the
generic function VALUE. This method, too, must specialize on the
MEAN-ACCUMULATOR class. We get the current accumulated mean value by
dividing the value of SUM slot with the value of COUNT slot.

    (defmethod genacc:value ((object mean-accumulator))
      (with-slots (sum count) object
        (/ sum count)))

Now the custom accumulator is ready and it can be used with the
WITH-ACCUMULATOR macro. Example:

    GENACC> (with-accumulator (mean :mean)
              (loop repeat 10 do (mean (random 1000)))
              (format t \"The mean so far: ~A~%\" (mean))
              (loop repeat 10 do (mean (random 1000)))
              (format t \"The final mean:  ~A~%\" (mean)))
    The mean so far: 2512/5
    The final mean:  2704/5
    NIL
"

  (let ((accumulator (gensym "ACCUMULATOR")))
    `(let ((,accumulator (initialize ,object ,@keyword-arguments)))
       (flet ((,name (&optional (item nil item-supplied-p))
                (if item-supplied-p
                    (accumulate ,accumulator item)
                    (value ,accumulator))))
         ,@body))))
