(in-package :cl-user)
(defpackage batis-test.sql.mysql
  (:use :cl
        :batis.macro
        :batis.sql
        :batis.dbi
        :rove))
(in-package :batis-test.sql.mysql)

(cl-syntax:use-syntax :annot)

(defparameter *session-mysql* nil)


(setup
  (setf *session-mysql* (batis.datasource:create-sql-session :mysql
                                                             :database-name "test"
                                                             :username "root"
                                                             :password "password"
                                                             :host "mysql-test"
                                                             :port 3306))
  (do-sql *session-mysql* "drop table if exists product")
  (do-sql *session-mysql* "create table product (id integer primary key, name varchar(20) not null, price integer not null)"))


(teardown
  (batis.datasource:close-sql-session *session-mysql*))


;;; --------------------------------------------------------------------------------
;;; define sql
;;; --------------------------------------------------------------------------------

;;; ----------------------------------------
;;; register product information
;;; ----------------------------------------
@update ("insert into product (id, name, price) values (:id, :name, :price)")
(defsql register-product (id name price))

;;; ----------------------------------------
;;; update product information
;;; ----------------------------------------
@update ("update
            product "
         (sql-set
          (sql-cond (not (null name))
                    " name = :name, ")
          (sql-cond (not (null price))
                    " price = :price "))
         (sql-where
          " id = :id "))
(defsql update-product (id name price))

;;; ----------------------------------------
;;; search product
;;; ----------------------------------------
@select ("select name, price from product where id = :id")
(defsql search-product (id))

;;; ----------------------------------------
;;; show all product
;;; ----------------------------------------
@select ("select id, name, price from product order by id")
(defsql show-all-product ())

;;; ----------------------------------------
;;; search product with conditions
;;; ----------------------------------------
@select ("select id, name, price from product"
         (sql-where
          (sql-cond (not (null name))
                    "     name = :name ")
          (sql-cond (not (null price_low))
                    " and price >= :price_low ")
          (sql-cond (not (null price_high))
                    " and price <= :price_high "))
         " order by id ")
(defsql filter-product (name price_low price_high))



;;; --------------------------------------------------------------------------------
;;; test
;;; --------------------------------------------------------------------------------

(deftest mysql-insert-record
  (update-one *session-mysql* register-product :id 1 :name "NES" :price 14800)
  (update-one *session-mysql* register-product :id 2 :name "SNES" :price 25000)
  (update-one *session-mysql* register-product :id 3 :name "MEGA DRIVE" :price 21000)
  (update-one *session-mysql* register-product :id 4 :name "PC Engine" :price 24800))

(deftest mysql-select-one
  (ok (equal (select-one *session-mysql* search-product :id 1)
             '(:|name| "NES" :|price| 14800))))

(deftest select-list
  (ok (equal (select-list *session-mysql* show-all-product)
             '((:|id| 1 :|name| "NES" :|price| 14800)
               (:|id| 2 :|name| "SNES" :|price| 25000)
               (:|id| 3 :|name| "MEGA DRIVE" :|price| 21000)
               (:|id| 4 :|name| "PC Engine" :|price| 24800)))))

(deftest mysql-filter
  (ok (equal (select-one *session-mysql* filter-product)
             '(:|id| 1 :|name| "NES" :|price| 14800)))

  (ok (equal (select-list *session-mysql* filter-product)
             '((:|id| 1 :|name| "NES" :|price| 14800)
               (:|id| 2 :|name| "SNES" :|price| 25000)
               (:|id| 3 :|name| "MEGA DRIVE" :|price| 21000)
               (:|id| 4 :|name| "PC Engine" :|price| 24800))))

  (ok (equal (select-list *session-mysql* filter-product :price_high 20000)
             '((:|id| 1 :|name| "NES" :|price| 14800))))

  (ok (equal (select-list *session-mysql* filter-product :price_low 20000 :price_high 22000)
             '((:|id| 3 :|name| "MEGA DRIVE" :|price| 21000))))

  (ok (equal (select-list *session-mysql* filter-product :name "MEGA DRIVE" :price_low 20000 :price_high 25000)
             '((:|id| 3 :|name| "MEGA DRIVE" :|price| 21000)))))


(deftest mysql-update
  (update-one *session-mysql* update-product :id 1 :name "Family Computer")
  (ok (equal (select-one *session-mysql* search-product :id 1)
             '(:|name| "Family Computer" :|price| 14800)))

  (update-one *session-mysql* update-product :id 2 :name "Super Famicom" :price 20000)
  (ok (equal (select-one *session-mysql* search-product :id 2)
             '(:|name| "Super Famicom" :|price| 20000)))

  (update-one *session-mysql* update-product :id 3 :price 12000)
  (ok (equal (select-one *session-mysql* search-product :id 3)
             '(:|name| "MEGA DRIVE" :|price| 12000))))

