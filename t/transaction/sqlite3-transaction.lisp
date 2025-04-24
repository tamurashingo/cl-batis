(in-package :cl-user)
(defpackage batis-test.transaction.sqlite3
  (:use :cl
        :batis
        :rove))
(in-package :batis-test.transaction.sqlite3)

(cl-syntax:use-syntax :annot)

(defparameter *session-sqlite31* nil)
(defparameter *session-sqlite32* nil)

(setup
  (setf *session-sqlite31* (batis.datasource:create-sql-session :sqlite3
                                                                :database-name "/app/volumes/batis_test.sqlite3"))
  (setf *session-sqlite32* (batis.datasource:create-sql-session :sqlite3
                                                                :database-name "/app/volumes/batis_test.sqlite3"))

  (do-sql *session-sqlite31* "create table book (id integer primary key, name varchar(20) not null, price integer not null)"))

(teardown
  (batis.datasource:close-sql-session *session-sqlite32*)
  (batis.datasource:close-sql-session *session-sqlite31*))


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

(deftest transaction-test-sqlite3
  (testing "commit"
    (with-transaction *session-sqlite32*
      ;; register book on session2
      (update-one *session-sqlite32* register-book :id 1 :name "Land of Lisp" :price 4104)
      (commit *session-sqlite32*)

      (ok (equal (select-one *session-sqlite32* search-book :id 1)
                '(:|name| "Land of Lisp" :|price| 4104))))
    
    (ok (equal (select-one *session-sqlite31* search-book :id 1)
              '(:|name| "Land of Lisp" :|price| 4104))))

  (testing "rollback"
    (with-transaction *session-sqlite32*
      (update-one *session-sqlite32* update-book-price :id 1 :price 3500)

      ;; updated on session2
      (ok (equal (select-one *session-sqlite32* search-book :id 1)
                '(:|name| "Land of Lisp" :|price| 3500)))

      ;; not updated on session1
      (ok (equal (select-one *session-sqlite31* search-book :id 1)
                '(:|name| "Land of Lisp" :|price| 4104)))

      ;; rollback on session2
      (rollback *session-sqlite32*))

      ;; rollbacked
    (ok (equal (select-one *session-sqlite31* search-book :id 1)
              '(:|name| "Land of Lisp" :|price| 4104)))
    (ok (equal (select-one *session-sqlite32* search-book :id 1)
              '(:|name| "Land of Lisp" :|price| 4104)))))

