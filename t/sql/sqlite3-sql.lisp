(in-package :cl-user)
(defpackage batis-test.sql.sqlite3
  (:use :cl
        :batis.macro
        :batis.sql
        :batis.dbi
        :rove))
(in-package :batis-test.sql.sqlite3)

(cl-syntax:use-syntax :annot)

(defparameter *session-sqlite3* nil)

(setup
  (setf *session-sqlite3* (batis.datasource:create-sql-session :sqlite3
                                                               :database-name "/app/volumes/batis_test.sqlite3"))
  (do-sql *session-sqlite3* "drop table if exists product")
  (do-sql *session-sqlite3* "create table product (id integer primary key, name varchar(20) not null, price integer not null)"))


(teardown
  (batis.datasource:close-sql-session *session-sqlite3*))
      

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

(deftest sqlite3-insert-record
  (update-one *session-sqlite3* register-product :id 1 :name "NES" :price 14800)
  (update-one *session-sqlite3* register-product :id 2 :name "SNES" :price 25000)
  (update-one *session-sqlite3* register-product :id 3 :name "MEGA DRIVE" :price 21000)
  (update-one *session-sqlite3* register-product :id 4 :name "PC Engine" :price 24800))

(deftest sqlite3-select-one
  (ok (equal (select-one *session-sqlite3* search-product :id 1)
             '(:|name| "NES" :|price| 14800))))

(deftest sqlite3-select-list
  (ok (equal (select-list *session-sqlite3* show-all-product)
             '((:|id| 1 :|name| "NES" :|price| 14800)
               (:|id| 2 :|name| "SNES" :|price| 25000)
               (:|id| 3 :|name| "MEGA DRIVE" :|price| 21000)
               (:|id| 4 :|name| "PC Engine" :|price| 24800)))))

(deftest sqlite3-filter
  (ok (equal (select-one *session-sqlite3* filter-product)
             '(:|id| 1 :|name| "NES" :|price| 14800)))

  (ok (equal (select-list *session-sqlite3* filter-product)
             '((:|id| 1 :|name| "NES" :|price| 14800)
               (:|id| 2 :|name| "SNES" :|price| 25000)
               (:|id| 3 :|name| "MEGA DRIVE" :|price| 21000)
               (:|id| 4 :|name| "PC Engine" :|price| 24800))))

  (ok (equal (select-list *session-sqlite3* filter-product :price_high 20000)
             '((:|id| 1 :|name| "NES" :|price| 14800))))

  (ok (equal (select-list *session-sqlite3* filter-product :price_low 20000 :price_high 22000)
             '((:|id| 3 :|name| "MEGA DRIVE" :|price| 21000))))
    
  (ok (equal (select-list *session-sqlite3* filter-product :name "MEGA DRIVE" :price_low 20000 :price_high 25000)
             '((:|id| 3 :|name| "MEGA DRIVE" :|price| 21000)))))


(deftest sqlite3-update
  (update-one *session-sqlite3* update-product :id 1 :name "Family Computer")
  (ok (equal (select-one *session-sqlite3* search-product :id 1)
             '(:|name| "Family Computer" :|price| 14800)))

  (update-one *session-sqlite3* update-product :id 2 :name "Super Famicom" :price 20000)
  (ok (equal (select-one *session-sqlite3* search-product :id 2)
             '(:|name| "Super Famicom" :|price| 20000)))

  (update-one *session-sqlite3* update-product :id 3 :price 12000)
  (ok (equal (select-one *session-sqlite3* search-product :id 3)
             '(:|name| "MEGA DRIVE" :|price| 12000))))



