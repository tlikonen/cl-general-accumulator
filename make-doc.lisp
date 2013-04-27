#-sbcl
(error "This is only for SBCL.")

(require :sb-posix)
(require :sb-introspect)

(flet ((probe-load (path &optional (default (user-homedir-pathname)))
         (let ((path (merge-pathnames path default)))
           (when (probe-file path) (load path))))
       (funcallstr (string &rest args)
         (apply (read-from-string string) args)))
  (or (probe-load #p"quicklisp/setup.lisp")
      (probe-load #p".quicklisp/setup.lisp")
      (let ((url "http://beta.quicklisp.org/quicklisp.lisp")
            (init (nth-value 1 (funcallstr "sb-posix:mkstemp"
                                           "/tmp/quicklisp-XXXXXX"))))
        (unwind-protect
             (progn
               (sb-ext:run-program "wget" (list "-O" init "--" url)
                                   :search t :output t)
               (when (probe-load init)
                 (funcallstr "quicklisp-quickstart:install")))
          (delete-file init)))))

(defun symbol-doc-type (symbol)
  (let (docs)
    (flet ((doc (symbol type key)
             (push (list symbol key (documentation symbol type)) docs)))
      (cond ((ignore-errors (macro-function symbol))
             (doc symbol 'function :macro))
            ((ignore-errors (symbol-function symbol))
             (doc symbol 'function :function)))
      (when (ignore-errors (symbol-value symbol))
        (doc symbol 'variable :variable))
      (cond ((subtypep symbol 'condition)
             (doc symbol 'type :condition))
            ((ignore-errors (find-class symbol))
             (doc symbol 'type :class))))
    docs))

(defparameter *head*
  "~
General accumulator
===================

**A general-purpose, extensible value accumulator library for Common
Lisp**


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

For more information see the documentation of `with-accumulator` in the
next section.


Interface (API)
---------------

")

(defparameter *tail*
  "~
The source code
---------------

GitHub repository: <https://github.com/tlikonen/cl-general-accumulator>
")


(defun print-doc (package &key (stream *standard-output*) (prefix "### "))
  (format stream *head*)
  (loop :with *package* := (find-package package)
        :with *print-right-margin* := 72
        :with symbols := (sort (loop :for symbol
                                     :being :each :external-symbol :in package
                                     :collect symbol)
                               #'string-lessp :key #'symbol-name)

        :for (symbol type doc) :in (mapcan #'symbol-doc-type symbols)
        :for name := (string-downcase (symbol-name symbol))
        :if doc
        :do
        (format stream "~A" prefix)
        (case type
          (:function
           (format stream "Function: `~A`" name)
           (let ((ll (sb-introspect:function-lambda-list symbol)))
             (when ll
               (format stream "~%~%The lambda list:~%~%     ~(~S~)" ll))))
          (:macro
           (format stream "Macro: `~A`" name)
           (let ((ll (sb-introspect:function-lambda-list symbol)))
             (when ll
               (format stream "~%~%The lambda list:~%~%     ~(~S~)" ll))))
          (:variable (format stream "Variable: `~A`" name))
          (:condition (format stream "Condition: `~A`" name))
          (:class (format stream "Class: `~A`" name)))
        (format stream "~%~%~A~%~%~%" doc))
  (format stream *tail*))


(pushnew (make-pathname :directory (pathname-directory *load-pathname*))
         asdf:*central-registry*)

(with-output-to-string (*standard-output*)
  (ql:quickload "general-accumulator"))

(print-doc "GENACC")