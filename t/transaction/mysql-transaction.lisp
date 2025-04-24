(in-package :cl-user)
(defpackage batis-test.transaction.mysql
  (:use :cl
        :batis
        :rove))
(in-package :batis-test.transaction.mysql)

(cl-syntax:use-syntax :annot)

(defparameter *session-mysql1* nil)
(defparameter *session-mysql2* nil)

(setup
  (setf *session-mysql1* (batis.datasource:create-sql-session :mysql
                                                              :database-name "test"
                                                              :username "root"
                                                              :password "password"
                                                              :host "mysql-test"
                                                              :port 3306))
  (setf *session-mysql2* (batis.datasource:create-sql-session :mysql
                                                              :database-name "test"
                                                              :username "root"
                                                              :password "password"
                                                              :host "mysql-test"
                                                              :port 3306))

  (do-sql *session-mysql1* "create table book (id integer primary key, name varchar(20) not null, price integer not null)"))

(teardown
  (batis.datasource:close-sql-session *session-mysql2*)
  (batis.datasource:close-sql-session *session-mysql1*))


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

(deftest transaction-test-mysql
  (testing "commit"
    (with-transaction *session-mysql2*
      ;; register book on session2
      (update-one *session-mysql2* register-book :id 1 :name "Land of Lisp" :price 4104)
      (commit *session-mysql2*)

      (ok (equal (select-one *session-mysql2* search-book :id 1)
                '(:|name| "Land of Lisp" :|price| 4104))))
    
    (ok (equal (select-one *session-mysql1* search-book :id 1)
              '(:|name| "Land of Lisp" :|price| 4104))))

  (testing "rollback"
    (with-transaction *session-mysql2*
      (update-one *session-mysql2* update-book-price :id 1 :price 3500)

      ;; updated on session2
      (ok (equal (select-one *session-mysql2* search-book :id 1)
                '(:|name| "Land of Lisp" :|price| 3500)))

      ;; not updated on session1
      (ok (equal (select-one *session-mysql1* search-book :id 1)
                '(:|name| "Land of Lisp" :|price| 4104)))

      ;; rollback on session2
      (rollback *session-mysql2*))

      ;; rollbacked
    (ok (equal (select-one *session-mysql1* search-book :id 1)
              '(:|name| "Land of Lisp" :|price| 4104)))
    (ok (equal (select-one *session-mysql2* search-book :id 1)
              '(:|name| "Land of Lisp" :|price| 4104)))))

