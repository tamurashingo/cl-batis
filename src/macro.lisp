(in-package :cl-user)
(defpackage batis.macro
  (:use :cl
        :cl-annot))
(in-package :batis.macro)

(cl-syntax:use-syntax :annot)

@export
(defclass <batis-sql> ()
  ((gen-sql-fn :type function
               :initarg :gen-sql-fn
               :initform (error "missing gen-sql-fn"))
   (sql-type :type keyword
             :initarg :sql-type)))

@export
(defmacro defsql (sql-name args &key sql-body sql-type &allow-other-keys)
  "define sql name and its args"
  (declare (ignore sql-type))
  `(defparameter
       ,sql-name
     (make-instance '<batis-sql> :gen-sql-fn (lambda (&key ,@args &allow-other-keys)
                                               (declare (ignore ,@args))
                                               ,sql-body)
                                 :sql-type ,sql-type)))

@export
(defmacro sql-cond (test-form sql-body)
  "apply sql-body when test-form is true

Example:
  (sql-cond (not (null product_name))
            \" and product_name like :product_name \")"
  `(if (funcall (lambda () ,test-form))
       ,sql-body
       NIL))

(defun trim-first-and-or (condition)
  (ppcre:regex-replace "^\\s*([Aa][Nn][Dd]|[Oo][Rr])\\s" condition " "))

@export
(defun sql-where (&rest conditions)
  "insert `WHERE` if there is any condition.
Furthermore, if that content begins with `AND` or `OR`, strip it off."
  (let ((conds (remove-if #'null conditions)))
    (if (null conds)
        ""
        (progn
          (setf (car conds) (trim-first-and-or (car conds)))
          (format NIL " where ~{ ~A~}" conds)))))

(defun trim-last-comma (column)
  (ppcre:regex-replace "\\,\\s*$" column " "))

@export
(defun sql-set (&rest columns)
  "insert `SET`.
Furthermore, if that content ends with `,`, strip it off."
  (let* ((cols (remove-if #'null columns))
         (lst (1- (length cols))))
    (if (null cols)
        ""
        (progn
          (setf (elt cols lst) (trim-last-comma (elt cols lst)))
          (format NIL " set ~{~A~}" cols)))))

@export
(defannotation select (sql-form def-form)
    (:arity 2)
  "define SELECT SQL

Example:
  @select (\"select * from product where valid_flag = '1' \"
           (sql-cond (not (null product_name))
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

    (append def-form `(:sql-body ,sql :sql-type :select))))


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

    (append def-form `(:sql-body ,sql :sql-type :update))))

