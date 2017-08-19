(in-package :cl-user)
(defpackage batis-test.transaction
  (:use :cl
        :batis.macro
        :batis.datasource
        :batis.sql
        :batis.dbi
        :prove))
(in-package :batis-test.transaction)

(cl-syntax:use-syntax :annot)


;; register product information
@update ("insert into product (id, name, price) values (:id, :name, :price)")
(defsql register-product (id name price))

;; update product information
@update ("update
            product"
         (sql-set
            " price = :price ")
         (sql-where
            " id = :id "))
(defsql update-product-price (id price))


;; search product
@select ("select name, price from product" (sql-where " id = :id "))
(defsql search-product (id))



(defun test-all (session1 session2)
  (progn
    (do-sql session1 "drop table if exists product")
    (do-sql session1 "create table product (id integer primary key, name varchar(20) not null, price integer not null)")

    ;; register product on session1
    (update-one session1 register-product :id 1 :name "Land of Lisp" :price 4104)
    (commit session1)

    ;; search product on session2
    (is (select-one session2 search-product :id 1)
        '(:|name| "Land of Lisp" :|price| 4104)
        "commit on session1 and updated on session2")

    ;; update product on session2
    (update-one session2 update-product-price :id 1 :price 3500))

    ;; updated on session2
    (is (select-one session2 search-product :id 1)
        '(:|name| "Land of Lisp" :|price| 3500)
        "no commit on session2")
    ;; not updated on session1
    (is (select-one session1 search-product :id 1)
        '(:|name| "Land of Lisp" :|price| 4104)
        "not updated on session1")

    ;; rollback
    (rollback session2)
    
    ;; rollbacked
    (is (select-one session1 search-product :id 1)
        '(:|name| "Land of Lisp" :|price| 4104)
        "rollbacked session1")
    (is (select-one session2 search-product :id 1)
        '(:|name| "Land of Lisp" :|price| 4104)
        "rollbacked session2")

    )


;; CL-DBI
(defparameter *conn-dbi-mysql* (dbi:connect :mysql
                                            :database-name "batis"
                                            :username "nobody"
                                            :password "nobody"))
(defparameter *conn-dbi-postgres* (dbi:connect :postgres
                                               :database-name "batis"
                                               :username "nobody"
                                               :password "nobody"))

;; CL-DBI-Connection-Pool
(defparameter *conn-dbi-cp-mysql* (dbi-cp:make-dbi-connection-pool :mysql
                                                                   :database-name "batis"
                                                                   :username "nobody"
                                                                   :password "nobody"))
(defparameter *conn-dbi-cp-postgres* (dbi-cp:make-dbi-connection-pool :postgres
                                                                      :database-name "batis"
                                                                      :username "nobody"
                                                                      :password "nobody"))

(defparameter *session1-mysql* (create-sql-session *conn-dbi-mysql*))
(defparameter *session2-mysql* (create-sql-session *conn-dbi-cp-mysql*))

(defparameter *session1-postgres* (create-sql-session *conn-dbi-cp-postgres*))
(defparameter *session2-postgres* (create-sql-session *conn-dbi-postgres*))


(plan NIL)

(test-all *session1-mysql* *session2-mysql*)
(test-all *session1-postgres* *session2-postgres*)

(close-sql-session *session1-mysql*)
(close-sql-session *session2-mysql*)
(close-sql-session *session1-postgres*)
(close-sql-session *session2-postgres*)

(dbi-cp:shutdown *conn-dbi-cp-mysql*)
(dbi-cp:shutdown *conn-dbi-cp-postgres*)

(finalize)
