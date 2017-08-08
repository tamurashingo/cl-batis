(in-package :cl-user)
(defpackage batis-test.sql
  (:use :cl
        :batis.macro
        :batis.sql
        :prove))
(in-package :batis-test.sql)

(cl-syntax:use-syntax :annot)


;; register product information
@update ("insert into product (id, name, price) values (:id, :name, :price)")
(defsql register-product (id name price))

;; search product
@select ("select name, price from product where id = :id")
(defsql search-product (id))

;; show all product
@select ("select id, name, price from product order by id")
(defsql show-all-product ())

;; search product with conditions
@select ("select id, name, price from product where 1 = 1"
         (sql-condition (not (null name))
                        " and name = :name ")
         (sql-condition (not (null price_low))
                        " and price >= :price_low ")
         (sql-condition (not (null price_high))
                        " and price <= :price_high ")
         " order by id ")
(defsql filter-product (name price_low price_high))


(defparameter *conn* (dbi:connect :mysql
                                  :database-name "batis"
                                  :username "nobody"
                                  :password "nobody"))
(defparameter *session* (batis.datasource:create-sql-session *conn*))


;; ----------------------------------------
;; create table
;; ----------------------------------------
(dbi:do-sql *conn* "drop table if exists product")
(dbi:do-sql *conn* "create table product (id integer primary key, name varchar(20) not null, price integer not null)")


(plan 13)

;; ----------------------------------------
;; UPDATE-ONE
;; ----------------------------------------
(is (update-one *session* register-product :id 1 :name "NES" :price 14800)
    '(1)
    "register NES information")
(is (update-one *session* register-product :id 2 :name "SNES" :price 25000)
    '(1)
    "register SNES information")
(is (update-one *session* register-product :id 3 :name "MEGA DRIVE" :price 21000)
    '(1)
    "register MEGA DRIVE information (Sega Genesis)")
(is (update-one *session* register-product :id 4 :name "PC Engine" :price 24800)
    '(1)
    "register PC Engine information (TurboGrafx-16)")


;; ----------------------------------------
;; SELECT-ONE
;; ----------------------------------------
(is (select-one *session* search-product :id 1)
    '(:|name| "NES" :|price| 14800))


;; ----------------------------------------
;; SELECT-LIST
;; ----------------------------------------
(is (select-list *session* show-all-product)
    '((:|id| 1 :|name| "NES" :|price| 14800)
      (:|id| 2 :|name| "SNES" :|price| 25000)
      (:|id| 3 :|name| "MEGA DRIVE" :|price| 21000)
      (:|id| 4 :|name| "PC Engine" :|price| 24800)))


;; ----------------------------------------
;; FILTER
;; ----------------------------------------
(is (select-one *session* filter-product)
    '(:|id| 1 :|name| "NES" :|price| 14800))

(is (select-list *session* filter-product)
    '((:|id| 1 :|name| "NES" :|price| 14800)
      (:|id| 2 :|name| "SNES" :|price| 25000)
      (:|id| 3 :|name| "MEGA DRIVE" :|price| 21000)
      (:|id| 4 :|name| "PC Engine" :|price| 24800)))
    
(is (select-list *session* filter-product :name "NES")
    '((:|id| 1 :|name| "NES" :|price| 14800)))

(is (select-list *session* filter-product :price_low 20000)
    '((:|id| 2 :|name| "SNES" :|price| 25000)
      (:|id| 3 :|name| "MEGA DRIVE" :|price| 21000)
      (:|id| 4 :|name| "PC Engine" :|price| 24800)))

(is (select-list *session* filter-product :price_high 20000)
    '((:|id| 1 :|name| "NES" :|price| 14800)))

(is (select-list *session* filter-product :price_low 20000 :price_high 22000)
    '((:|id| 3 :|name| "MEGA DRIVE" :|price| 21000)))

(is (select-list *session* filter-product :name "MEGA DRIVE" :price_low 20000 :price_high 25000)
    '((:|id| 3 :|name| "MEGA DRIVE" :|price| 21000)))


(batis.datasource:close-sql-session *session*)

(finalize)
