# Cl-Batis - SQL Mapping Framework for Common Lisp

[![Build Status](https://travis-ci.org/tamurashingo/cl-batis.svg?branch=master)](https://travis-ci.org/tamurashingo/cl-batis)


## Usage

### create session

```common-lisp
;; with CL-DBI connection
(defparameter *conn-dbi* (dbi:connect :mysql
                                      :database-name "batis"
                                      :username "nobody"
                                      :password "nobody"))
(defparameter *session* (create-sql-session *conn-dbi*))


;; with CL-DBI-Connection-Pool
(defparameter *conn-pool* (dbi-cp:make-dbi-connection-pool :mysql
                                                           :database-name "batis"
                                                           :username "nobody"
                                                           :password "nobody"))
(defparameter *session* (create-sql-session *conn-pool*))

;; direct
(defparameter *session* (create-sql-session :mysql
                                            :database-name "batis"
                                            :username "nobody"
                                            :password "nobody"))

```

### how to do DDL

Cl-Batis does not support DDL.
If you want to use DDL, use `do-sql`.

```common-lisp
(do-sql session "truncate table product")
```

### Define SQL

There are two type of methods.

- `update`
- `select`

when use `(cl-syntax:use-syntax :annot)`, `@update` and `@select` can be used.

#### update

```common-lisp
@update ("insert into product (id, name, price) values (:id, :name, :price)")
(defsql register-product (id name price))

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
```

#### select

```common-lisp
@select ("select name, price from product where id = :id")
(defsql search-product (id))

@select ("select id, name, price from product"
         (sql-where
           (sql-cond (not (null name))
                     " and name = :name ")
           (sql-cond (not (null price_low))
                     " and price >= :price_low ")
           (sql-cond (not (null price_high))
                     " and price <= :price_high "))
         " order by id ")
(defsql filter-product (name price_low price_high))
```

#### where, set

```common-lisp
@select
("select * from product where "
 (sql-cond (not (null price))
           " price = :price")
 (sql-cond (not (null valid_flag))
           " and valid_flag = :valid_flag"))
(defsql search-by-price (price valid_flag))
```

In dynamic condition, if `sql-cond` returns nothing, you would end up with SQL that looked like this:

```SQL
select * from product where
```

This would fail.
And, if only the second condition was met, you would end up with SQL that looked like this:

```SQL
select * from product where
and valid_flag = '1'
```

This would also fail.


So, `cl-batis` provides `SQL-WHERE` function.

```common-lisp
@select
("select * from product"
 (sql-where
   (sql-cond (not (null price))
    " price = :price")
   (sql-cond (not (null valid_flag))
    " and valid_flag = :valid_flag ")))
(defsql search-by-product (price valid_flag))
```

The `SQL-WHERE` knows to only insert `WHERE` if there is any condition.
Furthermore, if that content begins with `AND` or `OR`, strip it off.


```common-lisp
@update
("update product"
 (sql-set
  (sql-cond (not (null price))
            " price = :price, ")
  (sql-cond (not (null name))
            " name = :name "))
 (sql-where
  " id = :id "))
(defsql update-product-info (id price name))
```

There is a similar solution for dynamic update statements called `SQL-SET`.
The `SQL-SET` knows to strip last comma off.



### Execute

#### update

```common-lisp
(update-one *session* register-product :id 1 :name "NES" :price 14800)
```

#### select

```common-lisp
(select-one *session* search-product :id 1)
  -> (:|name| "NES" :|price| 14800))
```

```common-lisp
(select-list *session* filter-product :price_low 20000)
  ->((:|id| 2 :|name| "SNES" :|price| 25000)
     (:|id| 3 :|name| "MEGA DRIVE" :|price| 21000)
     (:|id| 4 :|name| "PC Engine" :|price| 24800)))
```

### transaction

```common-lisp
(commit *session*)

(rollback *session*)
```

### release session

```common-lisp
(close-sql-session *session*)
```

## Databases

* SQLite3
* PostgreSQL
* MySQL

## Example

```common-lisp
;;;
;;; create session
;;;
CL-USER> (defparameter session
           (create-sql-session :mysql
                               :database-name "scdata"
                               :username "root"
                               :password "password"))
SESSION

;;;
;;; create table
;;;
CL-USER> (do-sql session "create table product (id integer primary key, name varchar(20) not null, price integer not null)")
; No value

;;;
;;; define sql
;;;
CL-USER> (select (" select * from product where id = :id ")
                 (defsql select-product (id)))
SELECT-PRODUCT
CL-USER> (select (" select name, price from product "
                  (sql-where
                   (sql-cond (not (null name))
                             " and name = :name ")
                   (sql-cond (not (null price_low))
                             " and price >= :price_low ")
                   (sql-cond (not (null price_high))
                             " and price <= :price_high "))
                  " order by id ")
                 (defsql select-product-by-name-or-price (name price_low price_high)))
; in:
;      SELECT (" select name, price from product "
;          (SQL-WHERE (SQL-COND (NOT (NULL NAME)) " and name = :name ")
;                     (SQL-COND (NOT (NULL PRICE_LOW))
;                               " and price >= :price_low ")
;                     (SQL-COND (NOT (NULL PRICE_HIGH))
;                               " and price <= :price_high "))
;          " order by id ")
;     (NULL NAME)
; --> IF
; ==>
;   NAME
;
; caught STYLE-WARNING:
;   reading an ignored variable: NAME

;     (NULL PRICE_LOW)
; --> IF
; ==>
;   PRICE_LOW
;
; caught STYLE-WARNING:
;   reading an ignored variable: PRICE_LOW

;     (NULL PRICE_HIGH)
; --> IF
; ==>
;   PRICE_HIGH
;
; caught STYLE-WARNING:
;   reading an ignored variable: PRICE_HIGH
;
; compilation unit finished
;   caught 3 STYLE-WARNING conditions
SELECT-PRODUCT-BY-NAME-OR-PRICE
CL-USER> (update ("insert into product (id, name, price) values (:id, :name, :price)")
                 (defsql register-product (id name price)))
REGISTER-PRODUCT

;;;
;;; insert
;;;
CL-USER> (update-one session register-product :id 1 :name "NES" :price 14800)
(1)
CL-USER> (update-one session register-product :id 2 :name "SNES" :price 25000)
(1)
CL-USER> (update-one session register-product :id 3 :name "MEGA DRIVE" :price 21000)
(1)
CL-USER> (update-one session register-product :id 4 :name "PC Engine" :price 24800)
(1)

;;;
;;; select one record
;;;
CL-USER> (select-one session select-product :id 1)
(:|id| 1 :|name| "NES" :|price| 14800)

;;;
;;; select some records
;;;
CL-USER> (select-list session select-product-by-name-or-price)
((:|name| "NES" :|price| 14800) (:|name| "SNES" :|price| 25000)
 (:|name| "MEGA DRIVE" :|price| 21000) (:|name| "PC Engine" :|price| 24800))
CL-USER> (select-list session select-product-by-name-or-price :price_low 20000)
((:|name| "SNES" :|price| 25000) (:|name| "MEGA DRIVE" :|price| 21000)
 (:|name| "PC Engine" :|price| 24800))
CL-USER> (select-list session select-product-by-name-or-price :price_low 20000 :price_high 22000)
((:|name| "MEGA DRIVE" :|price| 21000))
CL-USER> (select-list session select-product-by-name-or-price :name "PC Engine")
((:|name| "PC Engine" :|price| 24800))
```

## Installation

This library is available on Quicklisp.

```commonlisp
(ql:quickload :cl-batis)
```

## Author

* tamura shingo (tamura.shingo@gmail.com)

## Copyright

Copyright (c) 2017 tamura shingo (tamura.shingo@gmail.com)

## License

Licensed under the MIT License.
