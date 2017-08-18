(in-package :cl-user)
(defpackage batis-test.datasource
  (:use :cl
        :batis.datasource
        :prove))
(in-package :batis-test.datasource)

(plan NIL)


(defparameter *conn-dbi* (dbi:connect :mysql
                                      :database-name "batis"
                                      :username "nobody"
                                      :password "nobody"))

(defparameter *conn-pool* (dbi-cp:make-dbi-connection-pool :mysql
                                                           :database-name "batis"
                                                           :username "nobody"
                                                           :password "nobody"))


;; CL-DBI
(defparameter *session* (create-sql-session *conn-dbi*))
(is-type *session* 'batis.datasource::<sql-session>
         "class of <sql-session>")
(is-type *session* 'batis.datasource::<sql-session-dbi>
         "class of <sql-session-dbi")
(is-type (batis.datasource::connection *session*)
         'dbi.driver::<dbi-connection>
         "have a connection")
(close-sql-session *session*)


;; CL-DBI-Connection-Pool
(defparameter *session* (create-sql-session *conn-pool*))
(is-type *session* 'batis.datasource::<sql-session>
         "class of <sql-session>")
(is-type *session* 'batis.datasource::<sql-session-dbi-cp>
         "class of <sql-session-dbi-cp>")
(is-type (batis.datasource::connection *session*)
         'dbi.driver::<dbi-connection>
         "have a connection")
(is-type (batis.datasource::proxy *session*)
         'dbi-cp.proxy::<dbi-connection-proxy>
         "have a connection proxy")
(close-sql-session *session*)


;; direct
(defparameter *session* (create-sql-session :mysql
                                            :database-name "batis"
                                            :username "nobody"
                                            :password "nobody"))
(is-type *session* 'batis.datasource::<sql-session>
         "class of <sql-session>")
(is-type *session* 'batis.datasource::<sql-session-dbi>
         "class of <sql-session-dbi>")
(is-type (batis.datasource::connection *session*)
         'dbi.driver::<dbi-connection>
         "have a connection")
(close-sql-session *session*)


(dbi-cp:shutdown *conn-pool*)

(finalize)
