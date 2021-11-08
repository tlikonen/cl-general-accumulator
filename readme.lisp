(defparameter *head*
  "~
General accumulator
===================

**A general-purpose, extensible value accumulator library for Common
Lisp**


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

For more information see the documentation of `with-accumulator` in the
next section.


License and Source Code
-----------------------

Author: Teemu Likonen <<tlikonen@iki.fi>>

OpenPGP key: [6965F03973F0D4CA22B9410F0F2CAE0E07608462][PGP]

License: [Creative Commons CC0][CC0] (public domain dedication)

The source code repository:
<https://github.com/tlikonen/cl-general-accumulator>

[PGP]: http://www.iki.fi/tlikonen/pgp-key.asc
[CC0]: https://creativecommons.org/publicdomain/zero/1.0/legalcode


The Programming Interface
-------------------------

")

(load "accumulator.lisp")
(load "print-doc.lisp")
(format t *head*)
(print-doc:print-doc "GENERAL-ACCUMULATOR")
