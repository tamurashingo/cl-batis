(in-package :cl-user)
(defpackage batis.sql
  (:use :cl
        :cl-annot))
(in-package :batis.sql)

(cl-syntax:use-syntax :annot)

@export
(defvar *SQL* (make-hash-table)
  "key: sql-name
value:
  sql-body: lambda expression that returns SQL string
  sql-type: 'select or 'update")

(defmacro prepare (sql-name)
  `(getf (gethash ,sql-name *SQL*) :sql))
