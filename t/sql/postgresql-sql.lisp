(in-package :cl-user)
(defpackage batis-test.sql.postgresql
  (:use :cl
        :batis.macro
        :batis.sql
        :batis.dbi
        :rove))
(in-package :batis-test.sql.postgresql)

(cl-syntax:use-syntax :annot)

(defparameter *session-postgresql* nil)

(setup
  (setf *session-postgresql* (batis.datasource:create-sql-session :postgres
                                                                  :database-name "test"
                                                                  :username "batis"
                                                                  :password "password"
                                                                  :host "postgresql-test"
                                                                  :port 5432))
  (do-sql *session-postgresql* "drop table if exists product")
  (do-sql *session-postgresql* "create table product (id integer primary key, name varchar(20) not null, price integer not null)"))


(teardown
  (batis.datasource:close-sql-session *session-postgresql*))
      

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

(deftest postgresql-insert-record
  (update-one *session-postgresql* register-product :id 1 :name "NES" :price 14800)
  (update-one *session-postgresql* register-product :id 2 :name "SNES" :price 25000)
  (update-one *session-postgresql* register-product :id 3 :name "MEGA DRIVE" :price 21000)
  (update-one *session-postgresql* register-product :id 4 :name "PC Engine" :price 24800))

(deftest postgresql-select-one
  (ok (equal (select-one *session-postgresql* search-product :id 1)
             '(:|name| "NES" :|price| 14800))))

(deftest postgresql-select-list
  (ok (equal (select-list *session-postgresql* show-all-product)
             '((:|id| 1 :|name| "NES" :|price| 14800)
               (:|id| 2 :|name| "SNES" :|price| 25000)
               (:|id| 3 :|name| "MEGA DRIVE" :|price| 21000)
               (:|id| 4 :|name| "PC Engine" :|price| 24800)))))

(deftest postgresql-filter
  (ok (equal (select-one *session-postgresql* filter-product)
             '(:|id| 1 :|name| "NES" :|price| 14800)))

  (ok (equal (select-list *session-postgresql* filter-product)
             '((:|id| 1 :|name| "NES" :|price| 14800)
               (:|id| 2 :|name| "SNES" :|price| 25000)
               (:|id| 3 :|name| "MEGA DRIVE" :|price| 21000)
               (:|id| 4 :|name| "PC Engine" :|price| 24800))))

  (ok (equal (select-list *session-postgresql* filter-product :price_high 20000)
             '((:|id| 1 :|name| "NES" :|price| 14800))))

  (ok (equal (select-list *session-postgresql* filter-product :price_low 20000 :price_high 22000)
             '((:|id| 3 :|name| "MEGA DRIVE" :|price| 21000))))
    
  (ok (equal (select-list *session-postgresql* filter-product :name "MEGA DRIVE" :price_low 20000 :price_high 25000)
             '((:|id| 3 :|name| "MEGA DRIVE" :|price| 21000)))))


(deftest postgresql-update
  (update-one *session-postgresql* update-product :id 1 :name "Family Computer")
  (ok (equal (select-one *session-postgresql* search-product :id 1)
             '(:|name| "Family Computer" :|price| 14800)))

  (update-one *session-postgresql* update-product :id 2 :name "Super Famicom" :price 20000)
  (ok (equal (select-one *session-postgresql* search-product :id 2)
             '(:|name| "Super Famicom" :|price| 20000)))

  (update-one *session-postgresql* update-product :id 3 :price 12000)
  (ok (equal (select-one *session-postgresql* search-product :id 3)
             '(:|name| "MEGA DRIVE" :|price| 12000))))



