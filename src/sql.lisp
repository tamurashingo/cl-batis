(in-package :cl-user)
(defpackage batis.sql
  (:use :cl
        :cl-annot)
  (:import-from :batis.datasource
                :<sql-session>
                :connection)
  (:import-from :batis.sqlparser
                :parse))
(in-package :batis.sql)

(cl-syntax:use-syntax :annot)

@export
(defmethod select-one ((session <sql-session>) sql-name &rest params &key &allow-other-keys)
  (car (apply #'select-list session sql-name params)))

@export
(defmethod select-list ((session <sql-session>) sql-name &rest params &key &allow-other-keys)
  (let* ((sql (apply sql-name params))
         (parsed-sql (parse sql))
         (params (create-params (getf parsed-sql :args) params)))
    (select-list-dbi (connection session) (getf parsed-sql :sql) params)))


(defmethod select-list-dbi ((conn dbi.driver::<dbi-connection>) sql params)
  (let* ((query (dbi:prepare conn sql))
         (result (apply #'dbi:execute query params)))
    (dbi:fetch-all result)))


@export
(defmethod update-one ((session <sql-session>) sql-name &rest params &key &allow-other-keys)
  (let* ((sql (apply sql-name params))
         (parsed-sql (parse sql))
         (params (create-params (getf parsed-sql :args) params)))
    (let* ((query (dbi:prepare (connection session) (getf parsed-sql :sql)))
           (result (apply #'dbi:execute query params)))
      (dbi:fetch-all result))))


(defun create-params (named-params named-value)
  "create params to use execute method.
named-params: array of parameter names
named-value: property list of argment values

  (create-params '(NAME PRICE PRICE) '(:NAME \"name\" :PRICE 100))
  -> (\"name\" 100 100)"
  (loop for key in named-params
        collect (let ((key (intern (symbol-name key) :KEYWORD)))
                  (getf named-value key))))
