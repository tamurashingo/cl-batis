(in-package :cl-user)
(defpackage batis.datasource
  (:use :cl
        :cl-annot
        :annot.class))
(in-package :batis.datasource)

(cl-syntax:use-syntax :annot)

@export
@export-accessors
(defclass <sql-session> ()
  ((connection :type dbi.driver::<dbi-connection>
               :initarg :connection
               :accessor connection
               :initform NIL)))

(defclass <sql-session-dbi> (<sql-session>)
  ())

(defclass <sql-session-dbi-cp> (<sql-session>)
  ((proxy :type dbi-cp.proxy::<dbi-connection-proxy>
          :initarg :proxy
          :accessor proxy
          :initform NIL)))


@export
(defmethod create-sql-session ((conn dbi.driver::<dbi-connection>) &key &allow-other-keys)
  (make-instance '<sql-session-dbi>
                 :connection conn))

@export
(defmethod create-sql-session ((connection-pool dbi-cp.connectionpool:<dbi-connection-pool>) &key &allo-other-keys)
  (let* ((connection-proxy (dbi-cp:get-connection connection-pool))
         (conn (dbi-cp.proxy:dbi-connection connection-proxy)))
    (make-instance '<sql-session-dbi-cp>
                   :connection conn
                   :proxy connection-proxy)))

@export
(defmethod create-sql-session (driver-name &rest params &key database-name &allow-other-keys)
  (make-instance '<sql-session-dbi>
                 :connection (apply #'dbi:connect driver-name params)))


@export
(defmethod close-sql-session ((session <sql-session-dbi>))
  (dbi:disconnect (connection session)))

@export
(defmethod close-sql-session ((session <sql-session-dbi-cp>))
  (dbi-cp:disconnect (proxy session)))

