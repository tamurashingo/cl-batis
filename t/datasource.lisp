(in-package :cl-user)
(defpackage batis-test.datasource
  (:use :cl
        :batis.datasource
        :rove))
(in-package :batis-test.datasource)


(setup
  (defparameter *conn-dbi* (dbi:connect :mysql
                                        :database-name "test"
                                        :username "root"
                                        :password "password"
                                        :host "mysql-test"
                                        :port 3306))

  (defparameter *conn-pool* (dbi-cp:make-dbi-connection-pool :mysql
                                                             :database-name "test"
                                                             :username "root"
                                                             :password "password"
                                                             :host "mysql-test"
                                                             :port 3306)))

(teardown
  (dbi-cp:shutdown *conn-pool*))


(deftest create-session-from-dbi
  (let ((session (create-sql-session *conn-dbi*)))
    (unwind-protect
      (progn (ok (typep session 'batis.datasource::<sql-session>))
             (ok (typep session 'batis.datasource::<sql-session-dbi>))
             (ok (typep (batis.datasource::connection session) 'dbi.driver::<dbi-connection>)))
      (close-sql-session session))))


(deftest create-session-from-connection-pool
  (let ((session (create-sql-session *conn-pool*)))
    (unwind-protect
      (progn (ok (typep session 'batis.datasource::<sql-session>))
             (ok (typep session 'batis.datasource::<sql-session-dbi-cp>))
             (ok (typep (batis.datasource::connection session) 'dbi.driver::<dbi-connection>))
             (ok (typep (batis.datasource::proxy session) 'dbi-cp.proxy::<dbi-connection-proxy>)))
      (close-sql-session session))))


(deftest create-session-direct
  (let ((session (create-sql-session :mysql
                                     :database-name "test"
                                     :username "root"
                                     :password "password"
                                     :host "mysql-test"
                                     :port 3306)))
    (unwind-protect
      (progn (ok (typep session 'batis.datasource::<sql-session>))
             (ok (typep session 'batis.datasource::<sql-session-dbi>))
             (ok (typep (batis.datasource::connection session) 'dbi.driver::<dbi-connection>)))
      (close-sql-session session))))

