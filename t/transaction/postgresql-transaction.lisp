(in-package :cl-user)
(defpackage batis-test.transaction.postgresql
  (:use :cl
        :batis
        :rove))
(in-package :batis-test.transaction.postgresql)

(cl-syntax:use-syntax :annot)

(defparameter *session-postgresql1* nil)
(defparameter *session-postgresql2* nil)

(setup
  (setf *session-postgresql1* (batis.datasource:create-sql-session :postgres
                                                                   :database-name "test"
                                                                   :username "batis"
                                                                   :password "password"
                                                                   :host "postgresql-test"
                                                                   :port 5432))
  (setf *session-postgresql2* (batis.datasource:create-sql-session :postgres
                                                                   :database-name "test"
                                                                   :username "batis"
                                                                   :password "password"
                                                                   :host "postgresql-test"
                                                                   :port 5432))

  (do-sql *session-postgresql1* "create table book (id integer primary key, name varchar(20) not null, price integer not null)"))

(teardown
  (batis.datasource:close-sql-session *session-postgresql2*)
  (batis.datasource:close-sql-session *session-postgresql1*))

;;; --------------------------------------------------------------------------------
;;; define sql
;;; --------------------------------------------------------------------------------

;; register book information
@update ("insert into book (id, name, price) values (:id, :name, :price)")
(defsql register-book (id name price))

;; update book information
@update ("update
            book"
         (sql-set
            " price = :price ")
         (sql-where
            " id = :id "))
(defsql update-book-price (id price))

;; search book
@select ("select name, price from book" (sql-where " id = :id "))
(defsql search-book (id))


;;; --------------------------------------------------------------------------------
;;; test
;;; --------------------------------------------------------------------------------

(deftest transction-test-postgresql
  (testing "commit"
    (with-transaction *session-postgresql2*
      ;; register book on session2
      (update-one *session-postgresql2* register-book :id 1 :name "Land of Lisp" :price 4104)
      (commit *session-postgresql2*)

      (ok (equal (select-one *session-postgresql2* search-book :id 1)
                 '(:|name| "Land of Lisp" :|price| 4104))))
  
    (ok (equal (select-one *session-postgresql1* search-book :id 1)
               '(:|name| "Land of Lisp" :|price| 4104))))

  (testing "rollback"
    (with-transaction *session-postgresql2*
      (update-one *session-postgresql2* update-book-price :id 1 :price 3500)

      ;; updated on session2
      (ok (equal (select-one *session-postgresql2* search-book :id 1)
                '(:|name| "Land of Lisp" :|price| 3500)))

      ;; not updated on session1
      (ok (equal (select-one *session-postgresql1* search-book :id 1)
                '(:|name| "Land of Lisp" :|price| 4104)))

      ;; rollback on session2
      (rollback *session-postgresql2*))

      ;; rollbacked
    (ok (equal (select-one *session-postgresql1* search-book :id 1)
              '(:|name| "Land of Lisp" :|price| 4104)))
    (ok (equal (select-one *session-postgresql2* search-book :id 1)
              '(:|name| "Land of Lisp" :|price| 4104)))))
