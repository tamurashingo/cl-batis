(in-package :cl-user)
(defpackage batis.macro
  (:use :cl
        :cl-annot))
(in-package :batis.macro)

(cl-syntax:use-syntax :annot)

@export
(defmacro defsql (sql-name args &key sql-body sql-type &allow-other-keys)
  "define sql name and its args"
  `(defparameter
       ,sql-name
     (lambda (&key ,@args &allow-other-keys)
       (declare (ignore ,@args))
       ,sql-body)))

@export
(defmacro sql-condition (test-form sql-body)
  "apply sql-body when test-form is true

Example:
  (sql-condition (not (null product_name))
                 \" and product_name like :product_name \")"
  `(if (funcall (lambda () ,test-form))
       ,sql-body
       ""))

@export
(defannotation select (sql-form def-form)
    (:arity 2)
  "define SELECT SQL

Example:
  @select (\"select * from product where valid_flag = '1' \"
           (sql-condition (not (null product_name))
                          \" and product_name like :product_name \"))
  (defsql fetch-product (product_name))"
  (when (not (and (listp def-form)
                  (eq (car def-form) 'defsql)))
    (error "@select needs defsql"))
  (let ((sqlbuf (gensym))
        (sql (gensym)))
    (setf sqlbuf (with-output-to-string (s)
                   (loop repeat (length sql-form) do (write-string "~A" s))))
    (setf sql `(format NIL ,sqlbuf ,@sql-form))

    (append def-form `(:sql-body ,sql :sql-type `select))))


@export
(defannotation update (sql-form def-form)
    (:arity 2)
  "define UPDATE SQL

Example:
  @select (\"update product set product_price = :product_price where product_id = :product_id\")
  (defsql update_price (product_price product_id))"
  (when (not (and (listp def-form)
                  (eq (car def-form) 'defsql)))
    (error "@update needs defsql"))
  (let ((sqlbuf (gensym))
        (sql (gensym)))
    (setf sqlbuf (with-output-to-string (s)
                   (loop repeat (length sql-form) do (write-string "~A" s))))
    (setf sql `(format NIL ,sqlbuf ,@sql-form))

    (append def-form `(:sql-body ,sql :sql-type `update))))

