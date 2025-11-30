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
                :parse)
  (:import-from :batis.macro
                :<batis-sql>))
(in-package :batis.sql)

(cl-syntax:use-syntax :annot)


@export
(defmethod select-one ((session <sql-session>) sql-name &rest params &key &allow-other-keys)
  (warn "SELECT-ONE is deprecated and will be removed in a future version.")
  (car (apply #'select-list session sql-name params)))

@export
(defmethod select-list ((session <sql-session>) sql-name &rest params &key &allow-other-keys)
  (warn "SELECT-LIST is deprecated and will be removed in a future version.")
  (multiple-value-bind (sql params) (gen-sql-and-params sql-name params)
    (sql-execute session sql params)))

@export
(defmethod update-one ((session <sql-session>) sql-name &rest params &key &allow-other-keys)
  (warn "UPDATE-ONE is deprecated and will be removed in a future version.")
  (multiple-value-bind (sql params) (gen-sql-and-params sql-name params)
    (sql-execute session sql params)))

(defmethod sql-execute ((session <sql-session-dbi>) sql params)
  ""
  (let* ((conn (connection session))
         (query (dbi:prepare conn sql))
         (result (dbi:execute query params)))
    (dbi:fetch-all result)))

(defmethod sql-execute ((session <sql-session-dbi-cp>) sql params)
  ""
  (let* ((conn (proxy session))
         (query (dbi-cp:prepare conn sql))
         (result (dbi-cp:execute query params)))
    (dbi-cp:fetch-all result)))


@export
(defmethod gen-sql-and-params ((sql-name <batis-sql>) params)
  "generate parameterized SQL and its parameters"
  (let* ((sql-fn (slot-value sql-name 'batis.macro::gen-sql-fn))
         (sql (apply sql-fn params))
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
