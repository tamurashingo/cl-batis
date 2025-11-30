(in-package :cl-user)
(defpackage batis.datasource
  (:use :cl
        :cl-annot
        :annot.class))
(in-package :batis.datasource)

(cl-syntax:use-syntax :annot)

(defclass <sql-session> ()
  ((connection :type dbi.driver::<dbi-connection>
               :initarg :connection
               :accessor connection
               :initform (error "missing initarg")))
  (:documentation "DEPRECATED: This class is deprecated and will be removed in a future version."))

(defclass <sql-session-dbi> (<sql-session>)
  ())

(defclass <sql-session-dbi-cp> (<sql-session>)
  ((proxy :type dbi-cp.proxy::<dbi-connection-proxy>
          :initarg :proxy
          :accessor proxy
          :initform (error "missing initarg"))))


@export
(defmethod create-sql-session ((conn dbi.driver::<dbi-connection>) &key &allow-other-keys)
  (warn "CREATE-SQL-SESSION is deprecated and will be removed in a future version.")
  (prog1
      (make-instance '<sql-session-dbi>
                     :connection conn)))


@export
(defmethod create-sql-session ((connection-pool dbi-cp.connectionpool:<dbi-connection-pool>) &key &allow-other-keys)
  (warn "CREATE-SQL-SESSION is deprecated and will be removed in a future version.")
  (let* ((connection-proxy (dbi-cp:get-connection connection-pool))
         (conn (dbi-cp.proxy:dbi-connection connection-proxy)))
    (prog1
        (make-instance '<sql-session-dbi-cp>
                       :connection conn
                       :proxy connection-proxy))))


@export
(defmethod create-sql-session (driver-name &rest params &key database-name &allow-other-keys)
  (declare (ignore database-name))
  (warn "CREATE-SQL-SESSION is deprecated and will be removed in a future version.")
  (let ((conn (apply #'dbi:connect driver-name params)))
    (prog1
        (make-instance '<sql-session-dbi>
                       :connection conn))))

@export
(defmacro with-transaction (session &body body)
  (warn "WITH-TRANSACTION is deprecated and will be removed in a future version.")
  (let ((conn-var (gensym "CONN-VAR")))
    `(let ((,conn-var (batis.datasource::connection ,session)))
       (dbi:with-transaction ,conn-var
         ,@body))))

@export
(defmethod commit ((session <sql-session-dbi>))
  (warn "COMMIT is deprecated and will be removed in a future version.")
  (dbi:commit (connection session)))

@export
(defmethod commit ((session <sql-session-dbi-cp>))
  (warn "COMMIT is deprecated and will be removed in a future version.")
  (dbi-cp:commit (proxy session)))

@export
(defmethod rollback ((session <sql-session-dbi>))
  (warn "ROLLBACK is deprecated and will be removed in a future version.")
  (dbi:rollback (connection session)))

@export
(defmethod rollback ((session <sql-session-dbi-cp>))
  (warn "ROLLBACK is deprecated and will be removed in a future version.")
  (dbi-cp:rollback (proxy session)))


@export
(defmethod close-sql-session ((session <sql-session-dbi>))
  (warn "CLOSE-SQL-SESSION is deprecated and will be removed in a future version.")
  (dbi:disconnect (connection session)))

@export
(defmethod close-sql-session ((session <sql-session-dbi-cp>))
  (warn "CLOSE-SQL-SESSION is deprecated and will be removed in a future version.")
  (dbi-cp:disconnect (proxy session)))


