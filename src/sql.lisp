(in-package :cl-user)
(defpackage batis.sql
  (:use :cl
        :cl-annot)
  (:import-from :batis.datasource
                :<sql-session>
                :<sql-session-dbi>
                :connection
                :proxy
                :<sql-session-dbi-cp>)
  (:import-from :batis.sqlparser
                :parse))
(in-package :batis.sql)

(cl-syntax:use-syntax :annot)

@export
(defmethod select-one ((session <sql-session>) sql-name &rest params &key &allow-other-keys)
  (car (apply #'select-list session sql-name params)))

@export
(defmethod select-list ((session <sql-session>) sql-name &rest params &key &allow-other-keys)
  (multiple-value-bind (sql params) (gen-sql-params sql-name params)
    (sql-execute session sql params)))

@export
(defmethod update-one ((session <sql-session>) sql-name &rest params &key &allow-other-keys)
  (multiple-value-bind (sql params) (gen-sql-params sql-name params)
    (sql-execute session sql params)))

(defmethod sql-execute ((session <sql-session-dbi>) sql params)
  ""
  (let* ((conn (connection session))
         (query (dbi:prepare conn sql))
         (result (apply #'dbi:execute query params)))
    (dbi:fetch-all result)))

(defmethod sql-execute ((session <sql-session-dbi-cp>) sql params)
  ""
  (let* ((conn (proxy session))
         (query (dbi-cp:prepare conn sql))
         (result (apply #'dbi-cp:execute query params)))
    (dbi-cp:fetch-all result)))


(defun gen-sql-params (sql-name params)
  "generate parameterized SQL and its parameters"
  (let* ((sql (apply sql-name params))
         (parsed-sql (parse sql))
         (params (create-params (getf parsed-sql :args) params)))
    (values (getf parsed-sql :sql) params)))

(defun create-params (named-params named-value)
  "create params to use execute method.
named-params: array of parameter names
named-value: property list of argment values

  (create-params '(NAME PRICE PRICE) '(:NAME \"name\" :PRICE 100))
  -> (\"name\" 100 100)"
  (loop for key in named-params
        collect (let ((key (intern (symbol-name key) :KEYWORD)))
                  (getf named-value key))))
