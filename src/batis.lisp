(in-package :cl-user)
(defpackage batis
  (:use :cl
        :cl-annot)
  (:import-from :batis.sqlparser
                :parse))
(in-package :batis)

(cl-syntax:use-syntax :annot)

(defvar *sql* (make-hash-table))

@export
(defmacro defsql (sql-name args &key sql-body &allow-other-keys)
  "define sql name and its args"
  `(progn
     (setf (gethash ',sql-name *sql*)
           (lambda (&key ,@args &allow-other-keys)
             ,sql-body))))

@export
(defmacro sql-condition (test-form sql-body)
 "apply sql-body when test-form is true

Example:
  (sql-condition (not (null product_name))
                 \" and product_name like :product_name)"
  `(if (funcall (lambda () ,test-form))
                  ,sql-body
                  ""))

@export
(defannotation select (sql-form def-form)
    (:arity 2)
  (when (not (and (listp def-form)
                 (eq (car def-form) 'defsql)))
    (error "@select needs defsql"))
  (let ((sqlbuf (gensym))
        (sql (gensym)))
    (setf sqlbuf (with-output-to-string (s)
                   (loop repeat (length sql-form) do (write-string "~A" s))))
    (setf sql `(format NIL ,sqlbuf ,@sql-form))

    (append def-form `(:sql-body ,sql))))
