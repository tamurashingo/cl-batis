(in-package :cl-user)
(defpackage batis-test.sql
  (:use :cl
        :batis.macro
        :batis.sql
        :batis.dbi
        :prove))
(in-package :batis-test.sql)

(cl-syntax:use-syntax :annot)


;; register product information
@update ("insert into product (id, name, price) values (:id, :name, :price)")
(defsql register-product (id name price))

;; update product information
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
          

;; search product
@select ("select name, price from product where id = :id")
(defsql search-product (id))

;; show all product
@select ("select id, name, price from product order by id")
(defsql show-all-product ())

;; search product with conditions
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



;; ----------------------------------------
;; create table
;; ----------------------------------------
(defun reset-table (session)
  (do-sql session "drop table if exists product")
  (do-sql session "create table product (id integer primary key, name varchar(20) not null, price integer not null)"))


;; ----------------------------------------
;; UPDATE-ONE
;; ----------------------------------------
;; SQLite3 does not return row count, so verify that the error does not occur.
(defun test-update (session)
  (update-one session register-product :id 1 :name "NES" :price 14800)
  (update-one session register-product :id 2 :name "SNES" :price 25000)
  (update-one session register-product :id 3 :name "MEGA DRIVE" :price 21000)
  (update-one session register-product :id 4 :name "PC Engine" :price 24800))

;; ----------------------------------------
;; SELECT-ONE
;; ----------------------------------------
(defun test-select-one (session)
  (is (select-one session search-product :id 1)
      '(:|name| "NES" :|price| 14800)))


;; ----------------------------------------
;; SELECT-LIST
;; ----------------------------------------
(defun test-select-list (session)
  (is (select-list session show-all-product)
      '((:|id| 1 :|name| "NES" :|price| 14800)
        (:|id| 2 :|name| "SNES" :|price| 25000)
        (:|id| 3 :|name| "MEGA DRIVE" :|price| 21000)
        (:|id| 4 :|name| "PC Engine" :|price| 24800))))


;; ----------------------------------------
;; FILTER(SELECT)
;; ----------------------------------------
(defun test-select-filter (session)
  (is (select-one session filter-product)
      '(:|id| 1 :|name| "NES" :|price| 14800))

  (is (select-list session filter-product)
      '((:|id| 1 :|name| "NES" :|price| 14800)
        (:|id| 2 :|name| "SNES" :|price| 25000)
        (:|id| 3 :|name| "MEGA DRIVE" :|price| 21000)
        (:|id| 4 :|name| "PC Engine" :|price| 24800)))
    
  (is (select-list session filter-product :name "NES")
      '((:|id| 1 :|name| "NES" :|price| 14800)))

  (is (select-list session filter-product :price_low 20000)
      '((:|id| 2 :|name| "SNES" :|price| 25000)
        (:|id| 3 :|name| "MEGA DRIVE" :|price| 21000)
        (:|id| 4 :|name| "PC Engine" :|price| 24800)))

  (is (select-list session filter-product :price_high 20000)
      '((:|id| 1 :|name| "NES" :|price| 14800)))

  (is (select-list session filter-product :price_low 20000 :price_high 22000)
      '((:|id| 3 :|name| "MEGA DRIVE" :|price| 21000)))

  (is (select-list session filter-product :name "MEGA DRIVE" :price_low 20000 :price_high 25000)
      '((:|id| 3 :|name| "MEGA DRIVE" :|price| 21000))))

;; ----------------------------------------
;; FILTER(UPDATE)
;; ----------------------------------------
(defun test-update-filter (session)
  (update-one session update-product :id 1 :name "Family Computer")
  (is (select-one session search-product :id 1)
      '(:|name| "Family Computer" :|price| 14800))

  (update-one session update-product :id 2 :name "Super Famicom" :price 20000)
  (is (select-one session search-product :id 2)
      '(:|name| "Super Famicom" :|price| 20000))

  (update-one session update-product :id 3 :price 12000)
  (is (select-one session search-product :id 3)
      '(:|name| "MEGA DRIVE" :|price| 12000)))


(plan nil)

;; CL-DBI
(defparameter *conn-dbi* (dbi:connect :sqlite3 :database-name ":memory:"))
(defparameter *session-dbi* (batis.datasource:create-sql-session *conn-dbi*))

;; CL-DBI-Connection-Pool
(defparameter *conn-dbi-cp* (dbi-cp:make-dbi-connection-pool :mysql
                                                             :database-name "batis"
                                                             :username "nobody"
                                                             :password "nobody"
                                                             :initial-size 2
                                                             :max-size 3))
(defparameter *session-dbi-cp* (batis.datasource:create-sql-session *conn-dbi-cp*))

;; Direct
(defparameter *session-direct* (batis.datasource:create-sql-session :postgres
                                                                    :database-name "batis"
                                                                    :username "nobody"
                                                                    :password "nobody"))

(defun test-all (session)
  (progn
    (reset-table session)
    (test-update session)
    (test-select-one session)
    (test-select-list session)
    (test-select-filter session)
    (test-update-filter session)))

(test-all *session-dbi*)
(test-all *session-dbi-cp*)
(test-all *session-direct*)

(batis.datasource:close-sql-session *session-dbi*)
(batis.datasource:close-sql-session *session-direct*)

(dbi-cp:shutdown *conn-dbi-cp*)

(finalize)

