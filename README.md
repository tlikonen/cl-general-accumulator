General accumulator
===================

**A general-purpose, extensible value accumulator library for Common Lisp**


Author and license
------------------

Author:  Teemu Likonen <<tlikonen@iki.fi>>

License: Public domain

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.


Introduction
------------

General accumulator is a general-purpose, extensible value accumulator
library for the Common Lisp language. Its main interface is
`with-accumulator` macro which sets an environment for easy
accumulation. The library provides several built-in accumulators which
should cover the most common use-cases but any kind of accumulators can
be added because the accumulator back-end is implemented through generic
functions.

    (with-accumulator (NAME OBJECT &key KEYWORD-ARGUMENTS ...)
      BODY ...)

The `with-accumulator` macro creates an accumulation environment in
which the local function _name_ handles the accumulation. Accumulator's
type is defined by the _object_ argument. Then all _body_ forms are
executed normally and the return value of the last form is returned.

The local function _name_ can optionally take one argument which is an
object to be accumulated. If the function is called without arguments it
returns the currently accumulated value. The accumulation process is
handled by generic functions `initialize`, `accumulate` and `value`.


Built-in accumulators
---------------------

There are several built-in accumulator types. Possible values for the
_object_ argument of `with-accumulator` macro:


### :LIST

Creates a list collector. Each accumulated object is collected to a
list. Example:

    GENACC> (with-accumulator (collect :list)
              (collect 1) (collect 2) (collect 3)
              (collect))
    (1 2 3)

The collecting is done destructively. The applicable `accumulate` method
maintains a pointer to the last cons cell of the list and each time
modifies its _cdr_ value to point to a new cons cell.


### [List]

If the _object_ argument is of type _list_ then new elements are
collected at the end. Example:

    GENACC> (with-accumulator (collect (list 1 2 3))
              (collect 4) (collect 5)
              (collect))
    (1 2 3 4 5)

This is a destructive operation. The _cdr_ value of the last cons cell
of the original list is modified and linked to a new cons cell.


### :VECTOR

Creates a general vector collector. It creates an adjustable vector with
a fill pointer 0 and element type _t_. New elements are pushed to that
vector with `cl:vector-push-extend` function. Example:

    GENACC> (with-accumulator (collect :vector)
              (collect "first") (collect "second")
              (collect))
    #("first" "second")


### :STRING

This is similar to `:VECTOR` but the element type is _character_. The
underlying `accumulate` methods can take a single _character_ or a
sequence of characters as the argument. Example:

    GENACC> (with-accumulator (collect :string)
              (collect #\a)
              (collect "bcd")
              (collect #(#\e #\f))
              (collect '(#\g #\h #\i))
              (collect))
    "abcdefghi"


### :BIT-VECTOR

This is similar to `:STRING` but the element type is _bit_. The argument
for the accumulator function can a _bit_ or a sequence of bits.


### [Vector]

If _object_ is of type _vector_ which satisfies the test
`cl:array-has-fill-pointer-p` then that vector is appended, starting
from its current fill pointer.

    GENACC> (with-accumulator
                (collect (make-array 2 :fill-pointer 2 :adjustable t
                                     :initial-contents (vector 1 2)))
              (collect 3)
              (collect 4)
              (collect))
    #(1 2 3 4)

Note that if the vector is not adjustable then the accumulator may reach
vector's limits and `cl:vector-push-extend` signals an error.


### [Function]

If _object_ is of type _function_ then the accumulator behaves like the
`cl:reduce` function: all accumulated objects are combined into one by
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
              (early-char #\o)
              (early-char #\b)
              (early-char #\s)
              (early-char))
    #\b


Adding a custom accumulator
---------------------------

The whole accumulation process is handled by three generic functions:
`initialize`, `accumulate` and `value`. Writing new methods for those
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
generic function `initialize` is used for that. It is called with the
_object_ argument of `with-accumulator` macro and with optional
_keyword-arguments_. In this example we use an _EQL_ specializer for
symbol `:MEAN`. We don't use any keyword arguments so there's just empty
_&key_ at the end of the lambda list.

    (defmethod genacc:initialize ((type (eql :mean)) &key)
      (make-instance 'mean-accumulator))

Now we create a method for generic function `accumulate`. The function
is called with two arguments:

 1. the accumulator object created by `initialize`
 2. the object that is meant to be accumulated.

This method specializes on our `mean-accumulator` class as well as on
the _number_ class. The new number is added to the previous value and
the count is increased by one.

    (defmethod genacc:accumulate ((object mean-accumulator)
                                  (number number))
      (with-slots (sum count) object
        (incf sum number)
        (incf count 1)))

For returning the accumulated mean value we create a method for the
generic function `value`. This method, too, must specialize on the
`mean-accumulator` class. We get the current accumulated mean value by
dividing the value of _sum_ slot with the value of _count_ slot.

    (defmethod genacc:value ((object mean-accumulator))
      (with-slots (sum count) object
        (/ sum count)))

Now the custom accumulator is ready and it can be used with the
`with-accumulator` macro. Example:

    GENACC> (with-accumulator (mean :mean)
              (loop repeat 10 do (mean (random 1000)))
              (format t "The mean so far: ~A~%" (mean))
              (loop repeat 10 do (mean (random 1000)))
              (format t "The final mean:  ~A~%" (mean)))
    The mean so far: 2512/5
    The final mean:  2704/5
    NIL


The source code
---------------

GitHub repository: <https://github.com/tlikonen/cl-general-accumulator>